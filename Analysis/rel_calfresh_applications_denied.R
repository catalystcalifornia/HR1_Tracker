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

# Filter for CalFresh data Aug 2025/26
calfresh_aug_25_26 <- calfresh_raw %>%
  filter(calendar_year %in% c(2025,2026), month %in% c("August"))

# Filter for CalFresh data June 2025/26
calfresh_june_25_26 <- calfresh_raw %>%
  filter(calendar_year %in% c(2025,2026), month %in% c("June"))

# Pulling CA county geoids, names
ca_counties <- dbGetQuery(con_shared, "SELECT statefp, countyfp, name AS county_name FROM geographies_ca.cb_2023_06_county_500k")
counties <- ca_counties %>%
  mutate(county_geoid = paste0(statefp, countyfp)) %>%
  select(county_geoid, county_name)

## Analyze Number of Applications Received and Denied, June 2025-26 ----

### Rate of applications denied out of all applications received ----
applications_denied_rate_june <- calfresh_june_25_26 %>%
  select(county, calendar_year, applications_denied, applications_received) %>%
  mutate(denial_rate = applications_denied / applications_received * 100) %>%
  pivot_wider(
    id_cols = county,
    names_from = calendar_year,
    values_from = c(applications_denied, applications_received, denial_rate),
    names_glue = "calfresh_{.value}_{calendar_year}_06") %>%
  mutate(calfresh_denial_rate_pct_change = (calfresh_denial_rate_2026_06 / calfresh_denial_rate_2025_06 - 1) * 100)
summary(applications_denied_rate_june$calfresh_denial_rate_pct_change) 
# Generally, the rate of applications denied has decreased.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -47.386 -18.684 -12.434 -11.042  -3.623  60.840       2 

# std dev = 18.52217
sd(applications_denied_rate_june$calfresh_denial_rate_pct_change, na.rm = TRUE) 

# Joining CalFresh data with county data
rel_applications_denied_rate_june <- counties %>%
  left_join(applications_denied_rate_june, by = c("county_name" = "county")) %>%
  mutate(geolevel = "county", .after = county_name) %>%
  arrange(desc(calfresh_denial_rate_pct_change))

########### Push to Postgres ----
schema <- "dashboard"
source <- "Script: W:/Project/ECI/HR 1 Tracker/Github/AV/HR1_Tracker/Analysis/rel_calfresh_applications_denied.R "
qa_filepath <- "W:/Project/ECI/HR 1 Tracker/Documentation/QA_rel_calfresh_applications_denied.docx"

# Write table with metadata
final_df <- rel_applications_denied_rate_june
table_name <- "rel_calfresh_applications_denied_county_jan27"
indicator <- "A table with the June 2025 and June 2026 CalFresh application denial 
rates by county and the percent change in denial rate. Data from California Dept. of
Social Services https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard"

dbWriteTable(con, Id(schema, table_name), final_df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(final_df) # Get column names
column_comments <- c(
  "Geoid of county", 
  "Name of county",
  "County level table",
  "Number of CalFresh applications denied during June 2025 (numerator)",
  "Number of CalFresh applications denied during June 2026",
  "Number of CalFresh applications received during June 2025 (denominator)",
  "Number of CalFresh applications received during June 2026",
  "Rate of application denial during June 2025",
  "Rate of application denial during June 2026",
  "Percent change in application denial rate from June 2025 to June 2026"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

### Rate of applications denied due to ineligibility out of all applications denied ----
applications_denied_reason_ineligible_rate_june <- calfresh_june_25_26 %>%
  select(county, calendar_year, applications_denied, denial_reason_ineligible) %>%
  mutate(ineligible_denial_rate = if_else(applications_denied==0, NA, denial_reason_ineligible / applications_denied * 100)) %>%
  pivot_wider(
    id_cols = county,
    names_from = calendar_year,
    values_from = c(denial_reason_ineligible, applications_denied, ineligible_denial_rate),
    names_glue = "calfresh_{.value}_{calendar_year}_06") %>%
  mutate(calfresh_ineligible_denial_rate_pct_change = if_else(calfresh_ineligible_denial_rate_2025_06==0, calfresh_ineligible_denial_rate_2026_06, (calfresh_ineligible_denial_rate_2026_06 / calfresh_ineligible_denial_rate_2025_06 - 1) * 100))
# Generally, the rate of applications denied due to ineligibility has decreased.
summary(applications_denied_reason_ineligible_rate_june$calfresh_ineligible_denial_rate_pct_change)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -100.000  -34.865   -7.553  -12.800    2.427  121.069        2 

# std dev = 47.06017
sd(applications_denied_reason_ineligible_rate_june$calfresh_ineligible_denial_rate_pct_change, na.rm=TRUE)

# Joining CalFresh data with county data
rel_applications_denied_reason_ineligible_rate_june <- counties %>%
  left_join(applications_denied_reason_ineligible_rate_june, by = c("county_name" = "county")) %>%
  mutate(geolevel = "county", .after = county_name) %>%
  arrange(desc(calfresh_ineligible_denial_rate_pct_change))

########### Push to Postgres ----

# Write table with metadata
final_df <- rel_applications_denied_reason_ineligible_rate_june
table_name <- "rel_calfresh_apps_denied_ineligible_county_jan27"
indicator <- "A table with the June 2025 and June 2026 CalFresh ineligibility application denial 
rates by county and the percent change in denial rate. Data from California Dept. of
Social Services https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard"

dbWriteTable(con, Id(schema, table_name), final_df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(final_df) # Get column names
column_comments <- c(
  "Geoid of county", 
  "Name of county",
  "County level table",
  "Number of CalFresh applications denied due to ineligibility during June 2025 (numerator)",
  "Number of CalFresh applications denied due to ineligibility during June 2026",
  "Number of CalFresh applications denied during June 2025 (denominator)",
  "Number of CalFresh applications denied during June 2026",
  "Rate of ineligibility application denial during June 2025",
  "Rate of ineligibility application denial during June 2026",
  "Percent change in ineligibility application denial rate from June 2025 to June 2026"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

### Total applications received ----
applications_received_june <- calfresh_june_25_26 %>%
  select(county, calendar_year, applications_received) %>%
  pivot_wider(
    names_from = calendar_year,
    values_from = applications_received,
    names_glue = "calfresh_applications_received_{calendar_year}_06") %>%
  mutate(calfresh_applications_received_pct_change = (calfresh_applications_received_2026_06 / calfresh_applications_received_2025_06 - 1) * 100)
summary(applications_received_june$calfresh_applications_received_pct_change)
# Generally, the number of applications received has decreased.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -32.184 -10.309  -6.739  -6.534  -2.385  19.703       2 
# std dev = 8.347697
sd(applications_received_june$calfresh_applications_received_pct_change, na.rm=TRUE)

# Joining CalFresh data with county data
rel_applications_received_june <- counties %>%
  left_join(applications_received_june, by = c("county_name" = "county")) %>%
  mutate(geolevel = "county", .after = county_name) %>%
  arrange(desc(calfresh_applications_received_pct_change))

########### Push to Postgres ----

# Write table with metadata
final_df <- rel_applications_received_june
table_name <- "rel_calfresh_applications_received_county_jan27"
indicator <- "A table with the June 2025 and June 2026 number of CalFresh applications received 
by county and the percent change in the numbers of applications received. Data from California Dept. of
Social Services https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard"

dbWriteTable(con, Id(schema, table_name), final_df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(final_df) # Get column names
column_comments <- c(
  "Geoid of county", 
  "Name of county",
  "County level table",
  "Number of CalFresh applications received during June 2025",
  "Number of CalFresh applications received during June 2026",
  "Percent change in number of applications received from June 2025 to June 2026"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)


## Analyze Number of Applications Received and Denied, Aug 2025-26 ----

### Rate of applications denied out of all applications received ----
# Generally, the rate of applications denied has decreased.
applications_denied_rate_aug <- calfresh_aug_25_26 %>%
  select(county, calendar_year, applications_denied, applications_received) %>%
  mutate(denial_rate = applications_denied / applications_received * 100) %>%
  pivot_wider(
    id_cols = county,
    names_from = calendar_year,
    values_from = c(applications_denied, applications_received, denial_rate),
    names_glue = "cal_fresh_{.value}_{calendar_year}_08") %>%
  mutate(calfresh_denial_rate_pct_change = (cal_fresh_denial_rate_2026_08 / cal_fresh_denial_rate_2025_08 - 1) * 100)

# Generally, rate of applications denied has ...
summary(applications_denied_rate_aug$calfresh_denial_rate_pct_change) 
### Aug 2025-26
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -54.7061  -7.4635   0.4311  -0.4184   8.7351  55.7971        1 

### June 2025-26
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -47.386 -18.684 -12.434 -11.042  -3.623  60.840       2 

### Rate of applications denied due to ineligibility out of all applications denied ----
# Generally, the rate of applications denied due to ineligibility has ...
applications_denied_reason_ineligible_rate_aug <- calfresh_aug_25_26 %>%
  select(county, calendar_year, applications_denied, denial_reason_ineligible) %>%
  mutate(ineligible_denial_rate = if_else(applications_denied==0, NA, denial_reason_ineligible / applications_denied * 100)) %>%
  pivot_wider(
    id_cols = county,
    names_from = calendar_year,
    values_from = ineligible_denial_rate,
    names_glue = "cal_fresh_ineligible_denial_rate_{calendar_year}_08") %>%
  mutate(calfresh_ineligible_denial_rate_pct_change = if_else(cal_fresh_ineligible_denial_rate_2025_08==0, cal_fresh_ineligible_denial_rate_2026_08, (cal_fresh_ineligible_denial_rate_2026_08 / cal_fresh_ineligible_denial_rate_2025_08 - 1) * 100))

# Generally, rate of applications denied due to ineligibility has ...
summary(applications_denied_reason_ineligible_rate_aug$calfresh_ineligible_denial_rate_pct_change)
### Aug 2025/26
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# -100.0000  -54.2402  -36.5834  -26.4643   -0.5937  376.0274         1 

### June 2025/26
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -100.000  -34.865   -7.553  -12.800    2.427  121.069        2 

### Total applications received ----
# Generally, the number of applications received has ...
applications_received_aug <- calfresh_aug_25_26 %>%
  select(county, calendar_year, applications_received) %>%
  pivot_wider(
    names_from = calendar_year,
    values_from = applications_received,
    names_glue = "calfresh_applications_received_{calendar_year}_08") %>%
  mutate(calfresh_applications_received_pct_change = (calfresh_applications_received_2026_08 / calfresh_applications_received_2025_08 - 1) * 100)
# Generally, the number of applications received has decreased.
summary(applications_received_aug$calfresh_applications_received_pct_change)
### Aug 2025/26
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -12.5000  -0.1714   4.8840  13.6544  10.9481 300.0000 

### June 2025/26
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -32.184 -10.309  -6.739  -6.534  -2.385  19.703       2 