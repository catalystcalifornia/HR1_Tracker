library(RPostgres)
library(tidyverse)
library(readxl)
library(janitor)
library(tigris)
options(tigris_use_cache = TRUE)

# Connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("eqb_hr1_tracker")

########### Read in raw CalFresh data ----
# Data from https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard
filepath <- "W:/Project/ECI/HR 1 Tracker/Data/CalFresh/Master data PUBLIC ACCESSIBLE.xlsx"
calfresh_raw <- read_excel(filepath, sheet="Monthly", skip=1, guess_max = Inf) %>%
  clean_names()

# Filter for CalFresh data June 2025/26
calfresh_june_25_26 <- calfresh_raw %>%
  filter(calendar_year %in% c(2025,2026), month %in% c("June"))

########### Total Enrollment ----
# Data on number of persons in CalFresh and percent change from June 2025 to 2026
enrollment <- calfresh_june_25_26 %>%
  select(county, calendar_year, cal_fresh_persons) %>%
  pivot_wider(
    names_from = calendar_year,
    values_from = cal_fresh_persons,
    names_prefix = "calfresh_persons_",
    names_glue = "calfresh_persons_{calendar_year}_06") %>%
  mutate(calfresh_persons_pct_change = (calfresh_persons_2026_06 - calfresh_persons_2025_06) / calfresh_persons_2025_06 * 100)

# QA: Looking at data on number of households, and caseloads by age + race. Note that
# the households is always much greater than the number of caseloads. I do not understand
# why so I don't trust the age+race columns.
qa <- calfresh_june_25_26 %>%
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

# Pulling in 2024 county geometries
ca_counties <- counties(state = "CA", cb = TRUE, progress_bar = FALSE) %>%
  select(county_geoid = GEOID, county_name = NAME, geom = geometry) %>%
  st_transform(4326)
st_crs(ca_counties)

# Joining CalFresh data with county data
enrollment_county <- ca_counties %>%
  left_join(enrollment, by = c("county_name" = "county")) %>%
  mutate(geolevel = "county", .after = county_name) 

final_df <- enrollment_county
########### Push to Postgres ----
# Write table with metadata
table_name <- "rel_calfresh_totalenrollment_county_jan27"
schema <- "dashboard"
indicator <- "A table with the June 2025 and June 2026 CalFresh enrollment rates by county, using the
total number of CalFresh persons (not households). Includes the percent change
in enrollment from June 2025 to 2026. Data from California Dept. of
Social Services https://www.cdss.ca.gov/inforesources/data-portal/research-and-data/calfresh-data-dashboard"
source <- "Script: W:/Project/ECI/HR 1 Tracker/Github/AV/HR1_Tracker/Analysis/rel_calfresh_enrollment.R "
qa_filepath <- "W:/Project/ECI/HR 1 Tracker/Documentation/QA_rel_calfresh_enrollment.docx"

export_shpfile(con=con, df=final_df, schema=schema, table_name=table_name, geometry_column = "geom")

# Comment on table and columns
column_names <- colnames(final_df) # Get column names
column_comments <- c(
  "Geoid of county", 
  "Name of county",
  "County level table",
  "Number of persons enrolled in CalFresh during June 2025",
  "Number of persons enrolled in CalFresh during June 2026",
  "Percent change of persons enrolled in CalFresh from June 2025 to June 2026",
  "County geometry in CRS 4326"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)