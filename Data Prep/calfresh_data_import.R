library(RPostgres)
library(tidyverse)
library(readxl)
library(janitor)

# Connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("eqb_hr1_tracker")

########### Read in raw CalFresh data and push to Postgres ----
# Data from https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard
filepath <- "W:/Project/ECI/HR 1 Tracker/Data/CalFresh/Master data PUBLIC ACCESSIBLE.xlsx"
calfresh_raw <- read_excel(filepath, sheet="Monthly", skip=1, guess_max = Inf) %>%
  clean_names()

# Push to rda_shared_data
con_shared <- connect_to_db("rda_shared_data")
table_name <- "cdss_calfresh_2014_26"
schema <- "health"
indicator <- "A table with CalFresh data from the CA Dept of Social Services (CDSS)
including data by state and county on total enrollment, dollar amount of issuances given
to enrollees, demographic breakdown of new applications and caseloads, and timeliness 
of issuances distributed. Demographics include race, gender, age, and language."
source <- "Script: W:/Project/ECI/HR 1 Tracker/Github/AV/HR1_Tracker/Data Prep/calfresh_data_import.R"
qa_filepath <- "QA doc: W:/Project/ECI/HR 1 Tracker/Documentation/QA_calfresh_data_import.docx"

dbWriteTable(con_shared, Id(schema, table_name), calfresh_raw, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(calfresh_raw) # Get column names
column_comments <- c("")

add_table_comments(con_shared, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)


# Filter for CalFresh data 2025/26
calfresh_25_26 <- calfresh_raw %>%
  filter(calendar_year %in% c(2025,2026))

# Create dataframe of data columns for easier referencing
calfresh_25_26_columns <- as.data.frame(names(calfresh_25_26))

# Filter for CalFresh data June 2025/26
calfresh_june_25_26 <- calfresh_raw %>%
  filter(calendar_year %in% c(2025,2026) & month %in% c("June"))

########### Total Enrollment ----
enrollment <- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         cal_fresh_persons
  ) 
  
########### Issuances Per Enrollee ----
# Monthly dollar amount of CalFresh benefits issued per enrollee
# https://dpss.lacounty.gov/en/food/calfresh.html

issuances <- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         cal_fresh_persons, 
         total_issuances
  ) 

# QA: Total issuances is nearly always less than total EBT dollars. I think EBT dollars
# refers to how much is in recipients' EBT debit cards.
# qa <- calfresh_25_26 %>%
#   select(county, date, month, calendar_year, 
#          cal_fresh_persons, 
#          total_issuances, federal_issuances, state_issuances,
#          ebt_fsp_dollars, ebt_snb_dollars, ebt_tnb_dollars
#   ) %>%
#   mutate(total_ebt_dollars = rowSums(across(c(ebt_fsp_dollars, ebt_snb_dollars, ebt_tnb_dollars)), na.rm=TRUE),
#          less_than = if_else(total_issuances <= total_ebt_dollars, TRUE, FALSE),
#          average_difference = total_ebt_dollars - total_issuances)
# table(qa$less_than)
# summary(qa$average_difference)

########### Total Applications Denied, # of Denials due to Ineligibility ----
applications_denied <- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         applications_denied, denial_reason_ineligible
  ) 

# QA: Summing across all the columns for denial reasons does NOT add up to the total 
# number of applications denied. The sum is always less than the total.
# qa <- applications_denied %>%
#   mutate(applications_denied_check = rowSums(across(contains("denial_reason"))), na.rm=TRUE) %>%
#   select(applications_denied, applications_denied_check) %>%
#   mutate(less_than = if_else(applications_denied_check <= applications_denied, TRUE, FALSE))
# table(qa$less_than)

########### New Applications by Race and Age ----
new_applications_age <- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         contains("new_applications_age")
  )

new_applications_race <- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         new_applications_american_indian_alaskan_native:new_applications_race_ethnicity_total
  )

########### Caseload by Race and Age ----
caseload_age <- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         contains("caseload_age")
  ) 

caseload_race <- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         contains("caseload_race_ethnicity_")
  )
########### Immigrant Impact ----
# California Food Assistance Program (CFAP) is a state-funded food program for 
# certain noncitizens who are ineligible to CalFresh only because of their 
# immigration status.  Benefits received through CFAP are the same as CalFresh;
# the only difference is that CalFresh is federally funded and CFAP is state-funded.  
# https://dpss.lacounty.gov/en/food/calfresh/california-food-assistance-program--cfap--information-page.html
cfap_immigrant <- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         all_cfap_persons
  ) %>%
  filter(county == "Statewide") # Only has state-level data

new_applications_immigrant <- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         contains("new_applications_language")
  )

caseload_immigrant<- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         contains("caseload_language")
  )

########### County Burden ----
# Federal regulations require that all households 
# entitled to expedited service (ES) be issued
# CalFresh benefits within seven days after the initial application date; 
# California law requires ES benefits be issued within three days. Households 
# not entitled to ES, but eligible for CalFresh benefits, must be
# issued benefits within 30 days after the date of initial application
# https://www.cdss.ca.gov/lettersnotices/entres/getinfo/acin/2012/I-23_12.pdf
# ES eligibility https://dpss.lacounty.gov/en/food/calfresh.html

services_timeliness <- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         timeliness_30_day_percentage, timeliness_expedited_services_percentage_monthly
  ) 

# The Active Error Rate measures the accuracy of the benefit amount issued to the 
# participant of the program at the county level. It is calculated by adding the 
# total amount of all over-and-under issuances of benefits divided by the total 
# amount of benefits issued for that sample month.
# Negative Error Rate is the number of cases either terminated in error, 
# denied in error, or suspended in error divided by the number of completed cases.
# Negative Error Rate Cases Completed is the number of cases completed and used 
# for the purposes of determining the negative error rate. The
# denominator from the negative error rate.
# https://www.cdss.ca.gov/Portals/9/DSSDB/Dashboards/CalFresh/Definitions_Sources.pdf?ver=2017-05-25-073509-020
application_errors <- calfresh_25_26 %>%
  select(county, date, month, calendar_year, 
         active_error_rate, 
         negative_error_rate, negative_error_rate_on_completed_cases
  ) 

