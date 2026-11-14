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

# Filter for CalFresh data 2025/26
calfresh_25_26 <- calfresh_raw %>%
  filter(calendar_year %in% c(2025,2026)) %>%
  select(county, month, calendar_year, denial_reason_ineligible, applications_denied, applications_received)

# Filter for CalFresh data Aug 2025/26
calfresh_aug_25_26 <- calfresh_raw %>%
  filter(calendar_year %in% c(2025,2026), month %in% c("August"))

# Filter for CalFresh data June 2025/26
calfresh_june_25_26 <- calfresh_raw %>%
  filter(calendar_year %in% c(2025,2026), month %in% c("June"))

# Create dataframe of data columns for easier referencing
calfresh_columns <- as.data.frame(names(calfresh_june_25_26))

## Analyze Number of Applications Received and Denied, June 2025-26 ----

### Rate of applications denied out of all applications received ----
# Generally, the rate of applications denied has decreased.
applications_denied_rate_june <- calfresh_june_25_26 %>%
  select(county, calendar_year, applications_denied, applications_received) %>%
  mutate(denial_rate = applications_denied / applications_received * 100) %>%
  pivot_wider(
    id_cols = county,
    names_from = calendar_year,
    values_from = denial_rate,
    names_glue = "cal_fresh_denial_rate_{calendar_year}_06") %>%
  mutate(calfresh_denial_rate_pct_change = (cal_fresh_denial_rate_2026_06 / cal_fresh_denial_rate_2025_06 - 1) * 100)

# Generally, rate of applications denied has decreased.
summary(applications_denied_rate_june$calfresh_denial_rate_pct_change) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -47.386 -18.684 -12.434 -11.042  -3.623  60.840       2 

### Rate of applications denied due to ineligibility out of all applications denied ----
# Generally, the rate of applications denied due to ineligibility has decreased.
applications_denied_reason_ineligible_rate_june <- calfresh_june_25_26 %>%
  select(county, calendar_year, applications_denied, denial_reason_ineligible) %>%
  mutate(ineligible_denial_rate = if_else(applications_denied==0, NA, denial_reason_ineligible / applications_denied * 100)) %>%
  pivot_wider(
    id_cols = county,
    names_from = calendar_year,
    values_from = ineligible_denial_rate,
    names_glue = "cal_fresh_ineligible_denial_rate_{calendar_year}_06") %>%
  mutate(calfresh_ineligible_denial_rate_pct_change = if_else(cal_fresh_ineligible_denial_rate_2025_06==0, cal_fresh_ineligible_denial_rate_2026_06, (cal_fresh_ineligible_denial_rate_2026_06 / cal_fresh_ineligible_denial_rate_2025_06 - 1) * 100))
summary(applications_denied_reason_ineligible_rate_june$calfresh_ineligible_denial_rate_pct_change)
# Generally, rate of applications denied due to ineligibility has decreased.
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -100.000  -34.865   -7.553  -12.800    2.427  121.069        2 

### Total applications received ----
# Generally, the number of applications received has decreased.
applications_received_june <- calfresh_june_25_26 %>%
  select(county, calendar_year, applications_received) %>%
  pivot_wider(
    names_from = calendar_year,
    values_from = applications_received,
    names_glue = "calfresh_applications_received_{calendar_year}_06") %>%
  mutate(calfresh_applications_received_pct_change = (calfresh_applications_received_2026_06 / calfresh_applications_received_2025_06 - 1) * 100)
# Generally, the number of applications received has decreased.
summary(applications_received_june$calfresh_applications_received_pct_change)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -32.184 -10.309  -6.739  -6.534  -2.385  19.703       2 


## Analyze Number of Applications Received and Denied, Aug 2025-26 ----

### Rate of applications denied out of all applications received ----
# Generally, the rate of applications denied has decreased.
applications_denied_rate_aug <- calfresh_aug_25_26 %>%
  select(county, calendar_year, applications_denied, applications_received) %>%
  mutate(denial_rate = applications_denied / applications_received * 100) %>%
  pivot_wider(
    id_cols = county,
    names_from = calendar_year,
    values_from = denial_rate,
    names_glue = "cal_fresh_denial_rate_{calendar_year}_08") %>%
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