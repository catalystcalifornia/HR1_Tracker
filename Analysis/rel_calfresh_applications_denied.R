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

calfresh_25_26 <- calfresh_raw %>%
  select(county, month, calendar_year, applications_denied, applications_received,
         denial_reason_ineligible, applications_received) %>%
  filter(calendar_year %in% c(2025,2026)) %>% # most recent data is August 2026
  filter((calendar_year == 2025 & month == "June") |
           (calendar_year == 2026 & month == "August"))

# Pulling CA county geoids, names
ca_counties <- dbGetQuery(con_shared, "SELECT statefp, countyfp, name AS county_name FROM geographies_ca.cb_2023_06_county_500k")
counties <- ca_counties %>%
  mutate(county_geoid = paste0(statefp, countyfp)) %>%
  select(county_geoid, county_name)

## Analyze Number of Applications Received and Denied, June 2025/Aug 2026 ----

### Rate of applications denied out of all applications received ----
applications_denied_rate <- calfresh_25_26 %>%
  select(county, calendar_year, applications_denied, applications_received) %>%
  mutate(denial_rate = applications_denied / applications_received * 100) %>%
  pivot_wider(
    id_cols = county,
    names_from = calendar_year,
    values_from = c(applications_denied, applications_received, denial_rate),
    names_glue = "calfresh_{.value}_{calendar_year}") %>%
  mutate(calfresh_denial_rate_pct_change = (calfresh_denial_rate_2026 / calfresh_denial_rate_2025 - 1) * 100)
summary(applications_denied_rate$calfresh_denial_rate_pct_change) 
# Generally, the rate of applications denied has decreased.
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -100.000  -26.803  -16.481  -18.733   -9.287   51.123        2 

# std dev = 24.15914
sd(applications_denied_rate$calfresh_denial_rate_pct_change, na.rm = TRUE) 

# Joining CalFresh data with county data
rel_applications_denied_rate<- counties %>%
  left_join(applications_denied_rate, by = c("county_name" = "county")) %>%
  mutate(geolevel = "county", .after = county_name) %>%
  arrange(calfresh_denial_rate_pct_change)

########### Push to Postgres ----
schema <- "dashboard"
source <- "Script: W:/Project/ECI/HR 1 Tracker/Github/AV/HR1_Tracker/Analysis/rel_calfresh_applications_denied.R "
qa_filepath <- "W:/Project/ECI/HR 1 Tracker/Documentation/QA_rel_calfresh_applications_denied.docx"

# Write table with metadata
final_df <- rel_applications_denied_rate
table_name <- "rel_calfresh_applications_denied_county_jan27"
indicator <- "A table with the June 2025 and August 2026 CalFresh application denial 
rates by county and the percent change in denial rate. 

Summary statistics for the percent change are
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     
-100.000  -26.803  -16.481  -18.733   -9.287   51.123        

Data from California Dept. of Social Services https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard"

dbWriteTable(con, Id(schema, table_name), final_df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(final_df) # Get column names
column_comments <- c(
  "Geoid of county", 
  "Name of county",
  "County level table",
  "Number of CalFresh applications denied during June 2025 (numerator)",
  "Number of CalFresh applications denied during August 2026",
  "Number of CalFresh applications received during June 2025 (denominator)",
  "Number of CalFresh applications received during August 2026",
  "Rate of application denial during June 2025",
  "Rate of application denial during August 2026",
  "Percent change in application denial rate from June 2025 to August 2026"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

### Rate of applications denied due to ineligibility out of all applications denied ----
applications_denied_reason_ineligible_rate <- calfresh_25_26 %>%
  select(county, calendar_year, applications_denied, denial_reason_ineligible) %>%
  mutate(ineligible_denial_rate = if_else(
          (applications_denied==0 | denial_reason_ineligible==0), 0, 
           denial_reason_ineligible / applications_denied * 100)) %>%
  pivot_wider(
    id_cols = county,
    names_from = calendar_year,
    values_from = c(denial_reason_ineligible, applications_denied, ineligible_denial_rate),
    names_glue = "calfresh_{.value}_{calendar_year}") %>%
  mutate(calfresh_ineligible_denial_rate_pct_change = case_when(
          (calfresh_ineligible_denial_rate_2025==0 & calfresh_ineligible_denial_rate_2026==0) ~ 0, 
          (calfresh_ineligible_denial_rate_2025==0 & calfresh_ineligible_denial_rate_2026>0) ~ NA,
          TRUE ~ (calfresh_ineligible_denial_rate_2026 / calfresh_ineligible_denial_rate_2025 - 1) * 100))
# Generally, the rate of applications denied due to ineligibility has increased.
# Statewide, the percent change is 5.844583.
summary(applications_denied_reason_ineligible_rate$calfresh_ineligible_denial_rate_pct_change)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -100.00  -24.62    0.00   14.55   21.03  629.45       3 

# std dev = 102.636
sd(applications_denied_reason_ineligible_rate$calfresh_ineligible_denial_rate_pct_change, na.rm=TRUE)

# Joining CalFresh data with county data
rel_applications_denied_reason_ineligible_rate <- counties %>%
  left_join(applications_denied_reason_ineligible_rate, by = c("county_name" = "county")) %>%
  mutate(geolevel = "county", .after = county_name) %>%
  arrange(calfresh_ineligible_denial_rate_pct_change)

########### Push to Postgres ----

# Write table with metadata
final_df <- rel_applications_denied_reason_ineligible_rate
table_name <- "rel_calfresh_apps_denied_ineligible_county_jan27"
indicator <- "A table with the June 2025 and August 2026 CalFresh ineligibility application denial 
rates by county and the percent change in denial rate. Statewide, the percent change is 5.844583.

Summary statistics for the percent change are
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
-100.00  -24.62    0.00   14.55   21.03  629.45 

Data from California Dept. of Social Services https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard"

dbWriteTable(con, Id(schema, table_name), final_df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(final_df) # Get column names
column_comments <- c(
  "Geoid of county", 
  "Name of county",
  "County level table",
  "Number of CalFresh applications denied due to ineligibility during June 2025 (numerator)",
  "Number of CalFresh applications denied due to ineligibility during August 2026",
  "Number of CalFresh applications denied during June 2025 (denominator)",
  "Number of CalFresh applications denied during August 2026",
  "Rate of ineligibility application denial during June 2025",
  "Rate of ineligibility application denial during June August",
  "Percent change in ineligibility application denial rate from June 2025 to August 2026"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

### Total applications received ----
applications_received <- calfresh_25_26 %>%
  select(county, calendar_year, applications_received) %>%
  pivot_wider(
    names_from = calendar_year,
    values_from = applications_received,
    names_glue = "calfresh_applications_received_{calendar_year}") %>%
  mutate(calfresh_applications_received_pct_change = 
           (calfresh_applications_received_2026 / calfresh_applications_received_2025 - 1) * 100)
summary(applications_received$calfresh_applications_received_pct_change)
# Generally, the number of applications received has increased.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -36.364  -4.198   1.199   8.810   7.874 192.456       2 

# std dev = 32.88689
sd(applications_received$calfresh_applications_received_pct_change, na.rm=TRUE)

# Joining CalFresh data with county data
rel_applications_received <- counties %>%
  left_join(applications_received, by = c("county_name" = "county")) %>%
  mutate(geolevel = "county", .after = county_name) %>%
  arrange(calfresh_applications_received_pct_change)

########### Push to Postgres ----

# Write table with metadata
final_df <- rel_applications_received
table_name <- "rel_calfresh_applications_received_county_jan27"
indicator <- "A table with the June 2025 and August 2026 number of CalFresh applications received 
by county and the percent change in the numbers of applications received. Statewide, 
the percent change has increased by 5.9762521.

Summary statistics for the percent change are
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
-36.364  -4.198   1.199   8.810   7.874 192.456  

Data from California Dept. of Social Services https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard"

dbWriteTable(con, Id(schema, table_name), final_df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(final_df) # Get column names
column_comments <- c(
  "Geoid of county", 
  "Name of county",
  "County level table",
  "Number of CalFresh applications received during June 2025",
  "Number of CalFresh applications received during August 2026",
  "Percent change in number of applications received from June 2025 to August 2026"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)
