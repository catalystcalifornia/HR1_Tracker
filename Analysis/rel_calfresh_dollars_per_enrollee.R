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

# Filter for CalFresh data June 2025/26
calfresh_june_25_26 <- calfresh_raw %>%
  filter(calendar_year %in% c(2025,2026), month %in% c("June"))

########### Dollars per enrollee ----
# The Consumer Price Index's food at home prices rose 2.7% from June 2025 to June 2026
# https://www.bls.gov/opub/ted/2026/consumer-prices-up-3-5-percent-over-the-year-ended-june-2026.htm
food_inflation <- 0.027 

# Data on dollars per enrollee in CalFresh and percent change from June 2025 to 2026
dollars_per_enrollee <- calfresh_june_25_26 %>%
  select(county, calendar_year, cal_fresh_persons, total_issuances) %>%
  mutate(calfresh_dollars_per_enrollee = total_issuances / cal_fresh_persons) %>%
  pivot_wider(
    id_cols = county,
    names_from = calendar_year,
    values_from = calfresh_dollars_per_enrollee,
    names_glue = "calfresh_dollars_per_enrollee_{calendar_year}_06") %>%
  # The "real" 2025 dollars is adjusted for inflation 
  mutate(calfresh_dollars_per_enrollee_2025_06_real = calfresh_dollars_per_enrollee_2025_06 * (1+food_inflation)) %>%
  # The "nominal" percent change does NOT account for inflation
  mutate(calfresh_dollars_per_enrollee_pct_change_nominal = 
           (calfresh_dollars_per_enrollee_2026_06 / calfresh_dollars_per_enrollee_2025_06 - 1) * 100,
        calfresh_dollars_per_enrollee_pct_change = 
            (calfresh_dollars_per_enrollee_2026_06 / calfresh_dollars_per_enrollee_2025_06_real - 1) * 100) %>%
  select(county, calfresh_dollars_per_enrollee_2025_06, calfresh_dollars_per_enrollee_2025_06_real, everything())

# When NOT adjusted for inflation, dollars per enrollee increases from June 2025 to June 2026.
summary(dollars_per_enrollee$calfresh_dollars_per_enrollee_pct_change_nominal)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -6.4601 -0.3142  0.1797  0.1799  0.8819  3.0983 

# When adjusted for inflation, dollars per enrollee decreases from June 2025 to June 2026.
summary(dollars_per_enrollee$calfresh_dollars_per_enrollee_pct_change) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -8.9193 -2.9349 -2.4540 -2.4539 -1.7703  0.3878 

# Pulling CA county geoids, names
ca_counties <- dbGetQuery(con_shared, "SELECT statefp, countyfp, name AS county_name FROM geographies_ca.cb_2023_06_county_500k")
counties <- ca_counties %>%
  mutate(county_geoid = paste0(statefp, countyfp)) %>%
  select(county_geoid, county_name)

# Joining CalFresh data with county data
dollars_per_enrollee_county <- counties %>%
  left_join(dollars_per_enrollee, by = c("county_name" = "county")) %>%
  mutate(geolevel = "county", .after = county_name) 

final_df <- dollars_per_enrollee_county %>%
  select(county_geoid:geolevel, 
         calfresh_dollars_per_enrollee_2025_06, 
         calfresh_dollars_per_enrollee_2025_06_real,
         calfresh_dollars_per_enrollee_2026_06,
         calfresh_dollars_per_enrollee_pct_change)

########### Push to Postgres ----
# Write table with metadata
table_name <- "rel_calfresh_dollars_per_enrollee_county_jan27"
schema <- "dashboard"
indicator <- "A table with the June 2025 and June 2026 percent change in CalFresh 
dollars per enrollee by county, using the total number of CalFresh persons (not households). 
Essentially, dollars per enrollee is a measure of how much each CalFresh enrollee
receives per month on average. Note that the percent change is adjusted for inflation, 
specifically using the Consumer Price Index food at home index. For reference, the Consumer Price 
Index food at home prices rose 2.7% from June 2025 to June 2026. Data from 
California Dept. of Social Services https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard 
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
  "Dollars per enrollee during June 2026",
  "Percent change of dollars per enrollee in CalFresh from June 2025 to June 2026, adjusted for inflation"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)
