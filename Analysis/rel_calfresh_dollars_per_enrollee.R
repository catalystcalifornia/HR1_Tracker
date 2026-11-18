library(RPostgres)
library(tidyverse)
library(readxl)
library(janitor)
library(tigris)
options(tigris_use_cache = TRUE)

# Connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("eqb_hr1_tracker")
con_shared <-connect_to_db("rda_shared_data")

########### Read in raw CalFresh data ----
# Data from https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard
filepath <- "W:/Project/ECI/HR 1 Tracker/Data/CalFresh/Master data PUBLIC ACCESSIBLE.xlsx"
calfresh_raw <- read_excel(filepath, sheet="Monthly", skip=1, guess_max = Inf) %>%
  clean_names()

# Filter for CalFresh data June 2025/August 26
calfresh_25_26 <- calfresh_raw %>%
  select(county, month, calendar_year, cal_fresh_persons, total_issuances) %>%
  filter(calendar_year %in% c(2025,2026)) %>% # most recent data is August 2026
  filter((calendar_year == 2025 & month == "June") |
           (calendar_year == 2026 & month == "August"))

########### Dollars per enrollee ----
# The Consumer Price Index's food at home prices rose 2.7% from June 2025 to June 2026
# https://www.bls.gov/opub/ted/2026/consumer-prices-up-3-5-percent-over-the-year-ended-june-2026.htm
food_inflation <- 0.027 

# Data on dollars per enrollee in CalFresh and percent change from June 2025 to 2026
dollars_per_enrollee <- calfresh_25_26 %>%
  mutate(calfresh_dollars_per_enrollee = total_issuances / cal_fresh_persons) %>%
  pivot_wider(
    id_cols = county,
    names_from = calendar_year,
    values_from = calfresh_dollars_per_enrollee,
    names_glue = "calfresh_dollars_per_enrollee_{calendar_year}") %>%
  # The "real" 2025 dollars is adjusted for inflation 
  mutate(calfresh_dollars_per_enrollee_2025_real = calfresh_dollars_per_enrollee_2025 * (1+food_inflation)) %>%
  # The "nominal" percent change does NOT account for inflation
  mutate(calfresh_dollars_per_enrollee_pct_change_nominal = 
           (calfresh_dollars_per_enrollee_2026 / calfresh_dollars_per_enrollee_2025 - 1) * 100,
        calfresh_dollars_per_enrollee_pct_change = 
            (calfresh_dollars_per_enrollee_2026 / calfresh_dollars_per_enrollee_2025_real - 1) * 100) %>%
  select(county, calfresh_dollars_per_enrollee_2025, calfresh_dollars_per_enrollee_2025_real, everything())

# Pulling CA county geoids, names
ca_counties <- dbGetQuery(con_shared, "SELECT statefp, countyfp, name AS county_name FROM geographies_ca.cb_2023_06_county_500k")
counties <- ca_counties %>%
  mutate(county_geoid = paste0(statefp, countyfp)) %>%
  select(county_geoid, county_name)

# Joining CalFresh data with county data
dollars_per_enrollee_county <- counties %>%
  left_join(dollars_per_enrollee, by = c("county_name" = "county")) %>%
  mutate(geolevel = "county", .after = county_name) %>%
  arrange(calfresh_dollars_per_enrollee_pct_change)

# When NOT adjusted for inflation
summary(dollars_per_enrollee_county$calfresh_dollars_per_enrollee_pct_change_nominal)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -5.000555 -1.117084  0.003739 -0.157235  0.759826  3.516716

# When adjusted for inflation, the percent change in $/enrollee is MORE negative
summary(dollars_per_enrollee_county$calfresh_dollars_per_enrollee_pct_change) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -7.4981 -3.7167 -2.6254 -2.7821 -1.8892  0.7952 

sd(dollars_per_enrollee_county$calfresh_dollars_per_enrollee_pct_change)
# std dev = 1.501287

final_df <- dollars_per_enrollee_county %>%
  select(county_geoid:geolevel, 
         calfresh_dollars_per_enrollee_2025, 
         calfresh_dollars_per_enrollee_2025_real,
         calfresh_dollars_per_enrollee_2026,
         calfresh_dollars_per_enrollee_pct_change)

########### Push to Postgres ----
# Write table with metadata
table_name <- "rel_calfresh_dollars_per_enrollee_county_jan27"
schema <- "dashboard"
indicator <- "A table with the June 2025 and August 2026 percent change in CalFresh 
dollars per enrollee by county, using the total number of CalFresh persons (not households). 
Note that the percent change is adjusted for inflation, specifically using the 
Consumer Price Index food at home index. For reference, the Consumer Price 
Index food at home prices rose 2.7% from June 2025 to June 2026.

Statewide, the percent change is -2.73 and across counties, the standard deviation 
is 1.50.

Summary statistics for the percent change across counties are
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-7.4981 -3.7167 -2.6254 -2.7821 -1.8892  0.7952 

Data from California Dept. of Social Services https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard 
and Consumer Price Index data from https://www.bls.gov/opub/ted/2026/consumer-prices-up-3-5-percent-over-the-year-ended-june-2026.htm"
source <- "Script: W:/Project/ECI/HR 1 Tracker/Github/AV/HR1_Tracker/Analysis/rel_calfresh_dollars_per_enrollee.R "
qa_filepath <- "W:/Project/ECI/HR 1 Tracker/Documentation/QA_rel_calfresh_dollars_per_enrollee.docx"

dbWriteTable(con, Id(schema, table_name), final_df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(final_df) # Get column names
column_comments <- c(
  "Geoid of county", 
  "Name of county",
  "County level table",
  "Dollars per enrollee during June 2025",
  "Dollars per enrollee during June 2025, using real dollars adjusted for inflation",
  "Dollars per enrollee during August 2026",
  "Percent change of dollars per enrollee in CalFresh from June 2025 to August 2026, adjusted for inflation"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)
