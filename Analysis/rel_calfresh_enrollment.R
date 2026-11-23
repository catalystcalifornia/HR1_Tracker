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

# Filter for CalFresh data June 2025/August 2026 (Sept 2026 is all NA values)
calfresh_25_26 <- calfresh_raw %>%
  select(county, month, calendar_year, cal_fresh_persons) %>%
  filter(calendar_year %in% c(2025,2026)) %>% # most recent data is August 2026
  filter((calendar_year == 2025 & month == "June") |
         (calendar_year == 2026 & month == "August"))

# Preset time periods as variables so it is easy to update in the future:
before<-"2025_06"
after<-"2026_08"

# JZ QA: Create a combined month_year 
calfresh_25_26<-calfresh_25_26%>%
  mutate(month_year=ifelse(calendar_year == 2025 & month == "June", before,
                           ifelse(calendar_year == 2026 & month == "August", after, "Did not recode"
                                  )))
# Check that worked
table(calfresh_25_26$month_year) # good


########### Total Enrollment ----
# Data on number of persons in CalFresh and percent change from June 2025 to August 2026
enrollment <- calfresh_25_26 %>%
  select(county, month_year, cal_fresh_persons) %>%
  pivot_wider(
    names_from = month_year,
    values_from = cal_fresh_persons,
    names_prefix = "calfresh_enrollment_",
    names_glue = "calfresh_enrollment_{month_year}") %>%
  mutate(calfresh_enrollment_pct_change = (calfresh_enrollment_2026_08 / calfresh_enrollment_2025_06 - 1)  * 100)

# See if this looks good: yes! The reason why we want to keep the columns with a year_month combined instead of just 2025 and 2026, 
# is because we will have to update data later so having clear notation of what year/month we are looking at
# will make things easier for us in the future.

# QA: Looking at data on number of households, and caseloads by age + race. Note that
# the households is always much greater than the number of caseloads. I do not understand
# why so I don't trust the age+race columns.

qa <- calfresh_raw %>%
  filter((calendar_year == 2025 & month == "June") |
           (calendar_year == 2026 & month == "August")) %>%
  select(county, date, month, calendar_year, 
         cal_fresh_persons, cal_fresh_households,
         contains("caseload_age_total"),
         contains("caseload_race_ethnicity_total"))
# QA: Is the number of persons always greater than the number of households? Yes.
table(qa$cal_fresh_persons > qa$cal_fresh_households, useNA='always')
# QA: Is the number of caseloads by age and race equal to each other? Yes. 
# There are 7 rows with NAs for the number of caseloads by age and race 
table(qa$caseload_age_total == qa$caseload_race_ethnicity_total, useNA='always')
# QA: Is the number of households always greater than the number of caseloads? Yes.
table(qa$cal_fresh_households > qa$caseload_race_ethnicity_total, useNA='always')


# JZ QA notes: 
# # Agree not super sure what the main difference is between persons and caseload. 
# I think that a 'caseload' is a different unit than an individual person enrollee but how/why I don't really understand
# Can explore more in a different script

calfresh_raw %>%
  filter((calendar_year == 2025 & month == "June") |
           (calendar_year == 2026 & month == "August")) %>%
  select(
    county, calendar_year, cal_fresh_persons,caseload_age_total, caseload_race_ethnicity_total) %>%
  group_by(county, calendar_year)%>%
  mutate(
    age_diff = caseload_age_total - cal_fresh_persons,
    race_diff = caseload_race_ethnicity_total - cal_fresh_persons) %>%
  View()


# Pulling CA county geoids, names
ca_counties <- dbGetQuery(con_shared, "SELECT statefp, countyfp, name AS county_name FROM geographies_ca.cb_2023_06_county_500k")
counties <- ca_counties %>%
  mutate(county_geoid = paste0(statefp, countyfp)) %>%
  select(county_geoid, county_name)

# Joining CalFresh data with county data
enrollment_county <- counties %>%
  left_join(enrollment, by = c("county_name" = "county")) %>%
  mutate(geolevel = "county", .after = county_name) %>%
  arrange(calfresh_enrollment_pct_change)
summary(enrollment_county$calfresh_enrollment_pct_change)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -11.865  -7.856  -5.715  -5.783  -4.058   2.31

sd(enrollment_county$calfresh_enrollment_pct_change)
# std dev = 2.916159

final_df <- enrollment_county
########### Push to Postgres ----

# Write table with metadata
table_name <- "rel_calfresh_totalenrollment_county_jan27"
schema <- "dashboard"
indicator <- "A table of the percent change in CalFresh enrollment from June 2025
to August 2026 by county, using the total number of CalFresh persons (not households). 
Statewide, the percent change is -7.50 and across counties, the standard
deviation is 2.91.

Summary statistics for the percent change across counties are  
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -11.865  -7.856  -5.715  -5.783  -4.058   2.31
 
Data from California Dept. of Social Services https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard"
source <- "Script: W:/Project/ECI/HR 1 Tracker/Github/AV/HR1_Tracker/Analysis/rel_calfresh_enrollment.R "
qa_filepath <- "W:/Project/ECI/HR 1 Tracker/Documentation/QA_rel_calfresh_enrollment.docx"

dbWriteTable(con, Id(schema, table_name), final_df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(final_df) # Get column names
column_comments <- c(
  "Geoid of county", 
  "Name of county",
  "County level table",
  "Number of persons enrolled in CalFresh during June 2025",
  "Number of persons enrolled in CalFresh during August 2026",
  "Percent change of persons enrolled in CalFresh from June 2025 to August 2026"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)