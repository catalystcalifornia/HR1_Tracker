# Set up workspace-----------------------

# Connect to postgres
library(dplyr)
library(RPostgres)
library(tidyr)
library(tidycensus)
library(readxl)
library(stringr)
library(sf)
library(tigris)
library(rpostgis)
library(purrr)
library(readr)
library(ggplot2)
library(openxlsx)
library(mapview)
options(scipen = 999)
# Connect to postgres

source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("eqb_hr1_tracker")
con_shared<-connect_to_db("rda_shared_data")
drv <- dbDriver("PostgreSQL")

# Read in tables from postgres:
### I think the demographic cuts of most interest are: total enrollment, by age/race, aid category, and language spoken

medical_aid_raw<-dbGetQuery(con_shared, "SELECT * FROM health.cchs_medical_eligibles_aid_cat_county_2010_26")
medical_race_raw<-dbGetQuery(con_shared, "SELECT * FROM health.cchs_medical_eligibles_race_age_county_2010_26")
medical_lang_raw<-dbGetQuery(con_shared, "SELECT * FROM health.cchs_medical_eligibles_language_county_2010_26")

medical_exp_0_19_raw<-dbGetQuery(con_shared, "SELECT * FROM health.cchs_medical_expansion_age0_19_2016_26")
medical_exp_19_50plus_raw<-dbGetQuery(con_shared, "SELECT * FROM health.cchs_medical_expansion_age19_50plus_2020_26")

# Establish before/after times for percent change calcs---------------------------
# Set your percent time change START and END times here: this way if we want to make changes we only have to reset it once here
before<-"2025-06" # June 2025
after<-"2026-10" # October 2026

# Read in california counties using tigris so I can join to my relational table: 
ca_counties <- counties(
  state = "CA",
  year = 2026,
  cb = FALSE
)

# Create a relational table for total medi-cal population-----------------------------

# Create a relational table with Medi-Cal total enrollment for each year/month of interest as defined in our 'before' and 'after' time breaks
# This can be helpful for each subsequent analysis when we are calculating rates of enrollment out of total Medi-Cal enrollment

medical_tot<-medical_lang_raw%>%
  group_by(county, month_of_eligibility)%>%
  filter(month_of_eligibility %in% c(after, before)) %>%
  summarise(total=sum(total_eligibles, na.rm=TRUE))%>%
  pivot_wider(
    id_cols = county,
    names_from = c(month_of_eligibility),
    values_from = total,
    names_glue = "total_{month_of_eligibility}",
    values_fill = 0)%>%
  rename_with(~ gsub("-", "_", .x))%>%   # clean up column names
  filter(county!="STATEWIDE") # remove 'STATEWIDE' 

# Data exploration: Does not need full QA------------------------

# I want to understand if the sub categories within each of these tables adds to a 'total' universe of people enrolled in MediCal

# To do this lets take a single county / time period and add the subcategories fore two tables and see if the totals are the same

total_race_lac <- medical_race_raw%>%
  filter(county=="Los Angeles" & month_of_eligibility %in% c(before, after))%>%
  summarise(total=sum(total_eligibles, na.rm=TRUE)) # 7752765

total_aid_lac <- medical_aid_raw%>%
  filter(county=="Los Angeles" & month_of_eligibility %in% c(before, after))%>%
  summarise(total=sum(total_eligibles, na.rm=TRUE)) # same

total_lang_lac <- medical_lang_raw%>%
  filter(county=="Los Angeles" & month_of_eligibility %in% c(before, after))%>%
  summarise(total=sum(total_eligibles, na.rm=TRUE)) # same

# Time bench marks from EQB: 
# Before HR 1 benchmark: June 2025
# ‘After’ HR 1 benchmark: Most recent data:  October 2026

# Lets see if those ranges of months/dates exist in the data

table(medical_race_raw$month_of_eligibility) # yes I see data for 2025 and 2026 June/July. But for some reason ONLY October 2025 is missing...
table(medical_lang_raw$month_of_eligibility) # missing here too
table(medical_aid_raw$month_of_eligibility) # missing here too

# Methodology decision (also written in QA doc): Because Medi-Cal enrollment in general does not to be very seasonal, based on enrollment mapped quarterly from 2012-2026 here: https://www.chcf.org/medi-cal-enrollment-tracker/?__cf_chl_tk=m6KSwdY9YPwajvB51aKhJNNg93R3CSKvRXpLu7SgEMA-1794865612-1.0.1.1-qzn.Um1gXbwHmwfqqAykoa0nSQQ4pC6tO33moTcbVQk
# I feel OK with moving up the 'later' comparison date so that the 'before' and 'after' months are not the same. 
# This way we are measuring more 'after' HR 1

# Analysis: Total Medi-Cal enrollment -----------------------

# Lets start with June 2025 as the base and compare to---> OCTOBER 2026 

total_enrollment_oct26<-medical_race_raw%>%
  filter(month_of_eligibility==after | month_of_eligibility==before)%>%
  group_by(month_of_eligibility, county)%>%
  summarise(total=sum(total_eligibles, na.rm=TRUE))%>%
  ungroup()%>%
  pivot_wider(names_from=month_of_eligibility, values_from=total)%>%
  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  rename_with(~ paste0("medical_enrollment_", .x), .cols = 2:3) %>%group_by(county)%>%
  mutate(medical_enrollment_pct_change=(medical_enrollment_2026_10-medical_enrollment_2025_06)/medical_enrollment_2025_06*100)%>%
  left_join(ca_counties%>%select(GEOID, NAME), by=c("county"="NAME"))%>%
  mutate(geolevel="county")%>%
  rename("county_name"="county",
         "county_geoid"="GEOID")%>%
  select(county_geoid, county_name, geolevel, everything()) %>%
select(-geometry)%>% # Remove geom column
 filter(county_name!="STATEWIDE")%>% # remove row for statewide 
ungroup()

# check total number of unique counties remaining in my table
length(unique(total_enrollment_oct26$county_name)) # 58 -- good

# check resulting table is a df -- good
class(total_enrollment_oct26)

# export this as a xlsx file: for EQB project team sharing no need to QA this 
 # write.xlsx(total_enrollment_oct26, file = "W:/Project/ECI/HR 1 Tracker/Data Exploration/medical_enrollment_june25_oct26.xlsx", overwrite = TRUE)

# quick visual: June 2025 --> October 2026: NO NEED to QA this

# ggplot(total_enrollment_oct26,
#        aes(x = county_name, y = medical_enrollment_pct_change)) +
#   geom_col(fill = "steelblue") +
#   geom_hline(yintercept = 0, color = "black") +
#   labs(
#     title = "Percent Change in Medi-Cal Enrollment by County",
#     subtitle = "June 2025 to October 2026",
#     x = "County",
#     y = "Percent Change (%)"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1)
#   )
# 

###### Push this to postgres as a relational table ######

df<-total_enrollment_oct26 # assign as 'df'

# make sure no trailing spaces anywhere

names(df) <- gsub(" ", "", names(df))

df <- df %>%
  mutate(across(where(is.character), ~ replace(.x, .x == " ", "")))

# set column types

charvect <- rep("numeric", ncol(df))
charvect <- replace(charvect, c(1, 2, 3), c("varchar"))
names(charvect) <- names(df)

# set table info

table_name <- "rel_medical_enrollment_county_jan27"
schema <- "dashboard"
indicator <- "Total monthly enrollment of Medi-Cal eligibles (total Medi-Cal universe) expansion program in June 2025, October 2026, and the percent change in enrollment between June 2025 and October 2026.
 Original data source: https:\\data.chhs.ca.gov\\dataset\\medi-cal-certified-eligibles-with-demographics-by-month"
source <- "R script:W:\\Project\\ECI\\HR 1 Tracker\\Github\\JZ\\HR1_Tracker\\Analysis\\rel_medical_totenrollment_county_jan27.R"
qa_filepath <- "W:\\Project\\ECI\\HEI\\Documentation\\QA_rel_medical_enrollment.docx"
table_comment <- paste0(indicator, source)

# push to postgres

dbWriteTable(con,     
             DBI::Id(schema = schema, table = table_name),
             df,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

column_names <- colnames(df) # Get column names

column_comments <- c(
  'county geoid',
  'county name',
  'geolevel',
  'Total medi-cal enrollment by county for June 2025',
  'Total medi-cal enrollment by county for October 2026',
  'Percent change in medi-cal enrollment by county from June 2025 to October 2026')

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)


# Analysis:  Medi-Cal enrollment by aid category -----------------------

# To account for the fact that enrollment by aid changes is also driven by changes in overall Medi-Cal enrollment
# It makes sense to compare the rate of enrollment by aid across our before and after years
# as opposed to just TOTAL enrollment in each aid group by years. 

# See what the most recent data is
medical_aid_raw%>%
  group_by(month_of_eligibility)%>%
  summarise(count=n())%>%
  View() # Up to OCT 2026

# Lets stick with June 2025 as the base and compare to---> OCT 2026 

# Methodology 1: Just comparing % change in total enrollment in each aid group 

aid_enrollment_oct26<- medical_aid_raw %>%
  filter(month_of_eligibility %in% c(after, before)) %>%
  group_by(month_of_eligibility, county, aid_category) %>%
  summarise(
    count = sum(total_eligibles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(month_of_eligibility, county) %>%
  mutate(total = sum(count)) %>%
  ungroup()%>%
  pivot_wider(names_from=month_of_eligibility, values_from=c("count","total"))%>%
  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  group_by(county, aid_category)%>%
  mutate(june25_oct26_pct_change=(count_2026_10-count_2025_06)/count_2025_06*100)

# Methodology 2: Compare rate of aid enrollment across our before and after years

aid_enrollment_oct26_qa<- medical_aid_raw %>%
  filter(month_of_eligibility %in% c(after, before)) %>%
  group_by(month_of_eligibility, county, aid_category) %>%
  summarise(
    count = sum(total_eligibles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(month_of_eligibility, county) %>%
  mutate(total = sum(count),
         rate=count/total*100) %>%
ungroup()%>%
  pivot_wider(names_from=month_of_eligibility, values_from=c("count","total", "rate"))%>%
  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  group_by(county, aid_category)%>%
  mutate(diff=(rate_2026_10-rate_2025_06))

# Join Methodology 1 + Methodology 2 and make side by side comparisons

join<-aid_enrollment_oct26%>%
  left_join(aid_enrollment_oct26_qa)

# I agree the raw enrollment percent change is NOT the best calc, but the diff in rates is better.

# Assign the QA version with methodology 2 as the main analysis df

aid_enrollment_oct26<-aid_enrollment_oct26_qa

# export this as a xlsx file:
# write.xlsx(aid_enrollment_oct26,
#            file = "W:/Project/ECI/HR 1 Tracker/Data Exploration/medical_aid_june25_oct26.xlsx", overwrite = TRUE)

# how many unique counties:

length(unique(aid_enrollment_oct26$county)) # 60 when it should be 58

aid_enrollment_oct26%>%
  group_by(county)%>%
  summarise(count=n())%>%
  View() ## the two are: Suppressed Cell Total, STATEWIDE

# remove the 2 non county rows
aid_enrollment_oct26<-aid_enrollment_oct26%>%
  filter(!grepl("Suppressed Cell Total|STATEWIDE", county))

# check:
length(unique(aid_enrollment_oct26$county)) # good now 58 counties

# Continue making this into a rel table: recode aid_category values using case_when for better formatted columns when we pivot wider

aid_enrollment_oct26<- aid_enrollment_oct26 %>%
  mutate(
    aid_category_re = case_when(
      aid_category == "ACA Expansion Adult Age 19 to 64" ~ "aca_exp_adult",
      aid_category == "Adoption/Foster Care" ~ "adopt_foster",
      aid_category == "CHIP" ~ "chip",
      aid_category == "LTC" ~ "ltc",
      aid_category == "Other" ~ "other",
      aid_category == "Parent/Caretaker Relative & Child" ~ "parent_caretaker",
      aid_category == "Seniors and Persons with Disabilities" ~ "senior_disabled",
      TRUE ~ "Did not get recoded"
    )
  )

# check that worked --- none good

aid_enrollment_oct26 %>%
  filter(aid_category_re == "Did not get recoded") %>%
  count(aid_category)

table(aid_enrollment_oct26$aid_category) # good 
table(aid_enrollment_oct26$aid_category_re) # good -

# pivot table wider by aid category

aid_enrollment_oct26<-aid_enrollment_oct26%>%
  pivot_wider(
  id_cols = county,
  names_from = aid_category_re,
  values_from = c(
    count_2025_06,
    count_2026_10,
    total_2025_06,
    total_2026_10,
    rate_2025_06,
    rate_2026_10,
    diff
  ),
  names_glue = "{aid_category_re}_{.value}"
)

# Check the calculation worked: values match, looks good!

aid_enrollment_oct26%>%
  filter(county=="San Diego")%>%
  mutate(chip_qa=(chip_rate_2026_10-chip_rate_2025_06))%>%
  select(chip_qa, chip_diff)%>%
  View()

# Now add in county geoids and clean up column names to match workflow

aid_enrollment_oct26<-aid_enrollment_oct26%>%
  left_join(ca_counties%>%select(GEOID, NAME, geometry), by=c("county"="NAME"))%>%
  mutate(geolevel="county")%>%
  rename("county_name"="county",
         "county_geoid"="GEOID")%>%
  select(county_geoid, county_name, geolevel, everything()) %>%
  mutate(across(where(is.numeric),
                ~ replace(.x, is.infinite(.x) | is.nan(.x), NA_real_)))%>% # replace all Inf and NaN with NULL
  select(-geometry)%>%
  ungroup()

# check class of final df -- good
class(aid_enrollment_oct26)


###### Push Medi-Cal Aid table to postgres ######

df<-aid_enrollment_oct26 # assign as 'df'

# make sure no trailing spaces anywhere

names(df) <- gsub(" ", "", names(df))

df <- df %>%
  mutate(across(where(is.character), ~ replace(.x, .x == " ", "")))

# set column types

charvect <- rep("numeric", ncol(df))
charvect <- replace(charvect, c(1, 2, 3), c("varchar"))
names(charvect) <- names(df)

# set table info

table_name <- "rel_medical_enrollment_aid_county_jan27_qa"
schema <- "dashboard"
indicator <- "Total and rate of enrollment in Medi-Cal by aid programs in June 2025, October 2026, and the difference in those enrollment rates
between June 2025 and October 2026. For instance, aid program enrollment rates are calculated by taking (aid enrollment / total enrollment in Medi-Cal) for June 2025 and October 2026.
The rate diff calcs are then made by subtracting the rates from each other: (aid enrollment rate Oct 2026)-(aid enrollment rate in June 2025)
 Original data source: https:\\data.chhs.ca.gov\\dataset\\medi-cal-certified-eligibles-with-demographics-by-month"
source <- "R script:W:\\Project\\ECI\\HR 1 Tracker\\Github\\JZ\\HR1_Tracker\\Analysis\\rel_medical_totenrollment_county_jan27.R"
qa_filepath <- "W:\\Project\\ECI\\HEI\\Documentation\\QA_rel_medical_enrollment.docx"
table_comment <- paste0(indicator, source)

# push to postgres

dbWriteTable( con,
              DBI::Id(schema = schema, table = table_name),
              df,
              overwrite = TRUE, row.names = FALSE,
              field.types = charvect)

# add meta data programmatically

# Get column names
column_names <- colnames(df)

# Automatically generate column comments
column_comments <- case_when(
  
  # Geographic identifiers
  column_names == "county_geoid" ~ "County GEOID",
  column_names == "county_name" ~ "County name",
  column_names == "geolevel" ~ "Geographic level",
  
  # Aid category enrollment counts
  str_detect(column_names, "_count_") ~ paste0(
    str_remove(column_names, "_count_.*") %>% str_replace_all("_", " "),
    " enrollment in ",
    case_when(
      str_detect(column_names, "2025_06$") ~ "June 2025",
      str_detect(column_names, "2026_10$") ~ "October 2026",
      TRUE ~ "unknown date"
    )
  ),
  
  # Total Medi-Cal enrollment
  str_detect(column_names, "_total_") ~ paste0(
    "Total Medi-Cal enrollment in ",
    case_when(
      str_detect(column_names, "2025_06$") ~ "June 2025",
      str_detect(column_names, "2026_10$") ~ "October 2026",
      TRUE ~ "unknown date"
    )
  ),
  
  # Aid category enrollment rates
  str_detect(column_names, "_rate_") ~ paste0(
    str_remove(column_names, "_rate_.*") %>% str_replace_all("_", " "),
    " rate of enrollment out of total Medi-Cal enrollment in ",
    case_when(
      str_detect(column_names, "2025_06$") ~ "June 2025",
      str_detect(column_names, "2026_10$") ~ "October 2026",
      TRUE ~ "unknown date"
    )
  ),
  
  # Percentage point differences between June 2025 and October 2026
  str_detect(column_names, "_june25_oct26_diff$") ~ paste0(
    str_remove(column_names, "_june25_oct26_diff$") %>%
      str_replace_all("_", " "),
    " percentage point change in share of total Medi-Cal enrollment from June 2025 to October 2026"
  ),
  
  # Catch any unmatched columns
  TRUE ~ "Column description not defined"
)

# check this worked

# Confirm number of comments matches number of columns
length(column_comments) == ncol(df)

# Review column names alongside generated comments
data.frame(
  column_name = column_names,
  column_comment = column_comments
) %>%
  View()

# Check whether any columns were missed
column_comments[column_comments == "Column description not defined"]

# column_names <- colnames(df) # Get column names
# 
# column_comments <- column_comments <- c(
#   'county geoid',
#   'county name',
#   'geolevel',
#   'Total medi-cal enrollment in aca expansion adult aid category by county for October 2026',
#   'Total medi-cal enrollment in the adopted/foster aid category by county for October 2026',
#   'Total medi-cal enrollment in the CHIP aid category by county for October 2026',
#   'Total medi-cal enrollment in the LTC aid category by county for October 2026',
#   'Total medi-cal enrollment in the OTHER aid category by county for October 2026',
#   'Total medi-cal enrollment in the parent or caretaker with child aid category by county for October 2026',
#   'Total medi-cal enrollment in the senior disabled aid category by county for October 2026',
#   
#   'Total medi-cal enrollment in aca expansion adult aid category by county for June 2025',
#   'Total medi-cal enrollment in the adopted/foster aid category by county for June 2025',
#   'Total medi-cal enrollment in the CHIP aid category by county for June 2025',
#   'Total medi-cal enrollment in the LTC aid category by county for June 2025',
#   'Total medi-cal enrollment in the OTHER aid category by county for June 2025',
#   'Total medi-cal enrollment in the parent or caretaker with child aid category by county for June 2025',
#   'Total medi-cal enrollment in the senior disabled aid category by county for June 2025',
#   
#   'Percent change in medi-cal enrollment from June 2025 to october 2026 in the aca expansion adult aid category by county',
#   'Percent change in medi-cal enrollment from June 2025 to october 2026 in the adopted/foster aid category by county for June 2025',
#   'Percent change in medi-cal enrollment from June 2025 to october 2026 in the CHIP aid category by county',
#   'Percent change in medi-cal enrollment from June 2025 to october 2026 in the LTC aid category by county',
#   'Percent change in medi-cal enrollment from June 2025 to october 2026t in the OTHER aid category by county',
#   'Percent change in medi-cal enrollment from June 2025 to october 2026 in the parent or caretaker with child aid category by county5',
#   'Percent change in medi-cal enrollment from June 2025 to october 2026 in the senior disabled aid category by county'
#   
# 
# )

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

# Analysis: Medi-Cal Enrollment by Language Spoken------------------------------------

# Check what dates are available
medical_lang_raw%>%
  group_by(month_of_eligibility)%>%
  summarise(count=n())%>%
  View() # October 2026

# Check how many unique counties are in the data
length(unique(medical_lang_raw$county)) # 60 instead of 58

medical_lang_raw%>%
  group_by(county)%>%
  summarise(count=n())%>%
  View() ## the two are: Invalid County Code, STATEWIDE

# See what the 'Invalid County Code' looks like in the raw data
medical_lang_raw%>%
  filter(county=="Invalid County Code")%>%View() # No data and not for most years anyway so OK to filter out

# remove the 2 non county rows
medical_lang_re<-medical_lang_raw%>%
  filter(!grepl("Invalid County Code|STATEWIDE", county))

# check:
length(unique(medical_lang_re$county)) # good now 58 counties

# Clean up langauge column for when we eventually pivot table wider

medical_lang_re<- medical_lang_re %>%
  filter(month_of_eligibility %in% c(after, before)) %>%
  mutate(language_re=language %>%
           str_to_lower() %>%
           str_replace_all("[/ ]", "_") %>%
           str_remove_all("-"))

# Pivot table wider

lang_enrollment_oct26<-medical_lang_re %>%
  filter(month_of_eligibility %in% c(after, before)) %>%
  pivot_wider(
    id_cols = county,
    names_from = c(language_re, month_of_eligibility),
    values_from = total_eligibles,
    names_glue = "{language_re}_{month_of_eligibility}",
    values_fill = 0)%>%
  rename_with(~ gsub("-", "_", .x))   # clean up column names

# skim through column names, I need to make sure the language options in June 2025 and October 2026 match

cols_2025 <- names(lang_enrollment_oct26) %>%
  stringr::str_subset("_2025_06$") # pull out 2025 column names

expected_2026 <- sub("2025_06$", "2026_10", cols_2025) # use 2025 column names but replace with 2026 suffix

# find out what is missing between 2025 and 2026 
setdiff(expected_2026, names(lang_enrollment_oct26)) # unknown_2026_10 is missing in actual 2026 data 2025. In 2026 it is:missing_unknown_2025_06

# lang_enrollment_oct26%>%
#   select(missing_unknown_2026_10) # has
# 
# lang_enrollment_oct26%>%
#   select(unknown_2026_10) # does not have 
# 
# lang_enrollment_oct26%>%
#   select(missing_unknown_2025_06) %>%
#   View() # has 
# 
# lang_enrollment_oct26%>%
#   select(unknown_2025_06) %>%
#   View() # has both unknown AND missing/unknown for 2025

sort(names(lang_enrollment_oct26)) 
# hindi_2026_10
# "missing_unknown_2025_06" "missing_unknown_2026_10"
# punjabi_2026_10
# unknown_2025_06

# pull out column names for easier self QAing --if you filter for 'unknown' here you see the problem
colnames<-as.data.frame(colnames(lang_enrollment_oct26))

# We should just have one 'missing' language that is a combination of missing/unknown+unknown

lang_enrollment_oct26_qa<-lang_enrollment_oct26%>%
  group_by(county)%>%
  mutate(unknown_2025_06=sum(unknown_2025_06, missing_unknown_2025_06, na.rm = TRUE))%>% # make the 2025 unknown = unknown + unknown/missing
  select(-missing_unknown_2025_06)%>% # now remove the unknown/missing for 2025
  rename("unknown_2026_10"="missing_unknown_2026_10") # rename 2026 to just unknown

# check resulting names in qa version

sort(names(lang_enrollment_oct26_qa))
qa_colnames<-as.data.frame(colnames(lang_enrollment_oct26_qa)) # only "unknown" remaining for 2025 and 2026 - good!

# assign qa version as main analysis table
lang_enrollment_oct26<-lang_enrollment_oct26_qa

# now see if there is still differences

cols_2025 <- names(lang_enrollment_oct26) %>%
  stringr::str_subset("_2025_06$") # pull out 2025 column names

expected_2026 <- sub("2025_06$", "2026_10", cols_2025) # use 2025 column names but replace with 2026 suffix

# find out what is missing between 2025 and 2026 
setdiff(expected_2026, names(lang_enrollment_oct26)) # NONE! good!

# Now check the reverse: languages in 2026 that are not in 2025

cols_2026 <- names(lang_enrollment_oct26) %>%
  stringr::str_subset("_2026_10$")

expected_2025 <- sub("2026_10$", "2025_06", cols_2026)

setdiff(expected_2025, names(lang_enrollment_oct26)) # "hindi_2025_06"   "punjabi_2025_06" these are NOT in 2025

# lang_enrollment_oct26%>%
#   select(punjabi_2025_06) # none for 2025
# 
# lang_enrollment_oct26%>%
#   select(punjabi_2026_10) # has in 2026
# 
# lang_enrollment_oct26%>%
#   select(hindi_2025_06) # none
# 
# lang_enrollment_oct26%>%
#   select(hindi_2026_10) # has in 2026

# Unfortunately will not be able to calculate percent change for Hindi / Punjabi from 2025 to 2026, will have to exclude entirely for now

lang_enrollment_oct26<-lang_enrollment_oct26%>%
  select(-c("punjabi_2026_10", "hindi_2026_10"))

# now see if there are differences

cols_2026 <- names(lang_enrollment_oct26) %>%
  stringr::str_subset("_2026_10$")

expected_2025 <- sub("2026_10$", "2025_06", cols_2026)

setdiff(expected_2025, names(lang_enrollment_oct26)) # no more ! 

# Methodology option 1: Percent change in total enrollees by language spoken 

# proceed with calcs:

lang_enrollment_oct26_opt1<-lang_enrollment_oct26%>%
  group_by(county)%>%
  mutate(
    across(
      ends_with("_2025_06"),
      ~ {
        col_2026 <- sub("2025_06$", "2026_10", cur_column())
        
        if_else(
          .x == 0,
          NA_real_,
          (get(col_2026) - .x) / .x * 100
        )
      },
      .names = "{sub('_2025_06$', '', .col)}_pct_change"
    )
  )

# Check the calculation worked: values match, looks good!

lang_enrollment_oct26_opt1%>%
  filter(county=="Los Angeles")%>%
  mutate(korean_qa=(korean_2026_10-korean_2025_06)/korean_2025_06*100)%>%
  select(korean_qa, korean_pct_change)%>%
  View()

# Method option 2: Calculate percent medi-cal enrollment by language spoken out of total Medi-Cal enrollment
# Then compare the difference in percents across our 'before' and 'after' year.
# This would account  for enrollment changes to Medi-Cal in general

# create a sub df for just medi-cal totals by county to use as denominator 
## JZ NOTE: If this passes QA, I also cpoied this code in line 84 because I realized for workflow purposes
# We should just have a medical total df that can be joined to any subsequent medi-cal anlaysis instead of recalculating for every Medi-Cal analysis

medical_tot<-medical_lang_raw%>%
  group_by(county, month_of_eligibility)%>%
  filter(month_of_eligibility %in% c(after, before)) %>%
  summarise(total=sum(total_eligibles, na.rm=TRUE))%>%
  pivot_wider(
    id_cols = county,
    names_from = c(month_of_eligibility),
    values_from = total,
    names_glue = "total_{month_of_eligibility}",
    values_fill = 0)%>%
  rename_with(~ gsub("-", "_", .x))%>%   # clean up column names
filter(county!="STATEWIDE")

# Join this to my language df

lang_enrollment_oct26_opt2<-lang_enrollment_oct26%>%
  left_join(medical_tot)

# check what column names are
colnames_opt2<-as.data.frame(colnames(lang_enrollment_oct26_opt2))

# now calculate rate of enrollment by language spoken now that I have total medi-cal enrollment for my denominator

step1 <- lang_enrollment_oct26_opt2 %>%
  
  # Calculate enrollment rates for each language
  mutate(
    across(
      matches("_(2025_06|2026_10)$") & !starts_with("total_") & !contains("_rate_"),
      ~ .x / get(paste0(
        "total_",
        str_extract(cur_column(), "2025_06|2026_10")
      )) * 100,
      .names = "{str_replace(.col, '_(2025_06|2026_10)$', paste0('_rate_', str_extract(.col, '2025_06|2026_10')))}"
    )
  ) 


# see if that worked so far ----so far so good

step1%>%
  filter(county=='Alameda')%>%
  mutate(qa26=english_2026_10/total_2026_10*100,
         qa25=spanish_2025_06/total_2025_06*100)%>%
  select(qa26, english_rate_2026_10, qa25, spanish_rate_2025_06)%>%
  View()
  
# Now that the rates look good,  calculate percentage point differences
step2<-step1%>%
  mutate(
    across(
      ends_with("_rate_2025_06"),
      ~ get(str_replace(cur_column(), "_rate_2025_06$", "_rate_2026_10")) - .x,
      .names = "{str_remove(.col, '_rate_2025_06$')}_diff"
    )
  )

# check that worked --- yes looks good

step2%>%
  filter(county=='Alameda')%>%
  mutate(qa=english_rate_2026_10-english_rate_2025_06)%>%
  select(qa, english_diff)%>%
  View()

# assign back to option 2 df

lang_enrollment_oct26_opt2<-step2

# Now I want to compare option 1 and option 2 methodologies

join<-lang_enrollment_oct26_opt1%>%
  left_join(lang_enrollment_oct26_opt2)%>%
  select(county,ends_with("change"), ends_with("diff"))%>%
  select(county, all_of(sort(setdiff(names(.), "county"))))

# Push this out to excel so we have it to look at later
# write.xlsx(join, file = "W:/Project/ECI/HR 1 Tracker/Data Exploration/medical_enrollment_langspoken_june25_oct26.xlsx", overwrite = FALSE)

# I do think it makes more sense to move forward with option 2 for more accuracy 

lang_enrollment_oct26<-lang_enrollment_oct26_opt2

# Now add in county geoids and clean up column names to match workflow

lang_enrollment_oct26<-lang_enrollment_oct26%>%
  left_join(ca_counties%>%select(GEOID, NAME, geometry), by=c("county"="NAME"))%>%
  mutate(geolevel="county")%>%
  rename("county_name"="county",
         "county_geoid"="GEOID")%>%
  select(county_geoid, county_name, geolevel, everything()) %>%
  mutate(across(where(is.numeric),
                ~ replace(.x, is.infinite(.x) | is.nan(.x), NA_real_)))%>% # replace all Inf and NaN with NULL
  select(-geometry)%>%
  ungroup()

# check class of final df -- good
class(lang_enrollment_oct26)

###### Push Medi-Cal language spoken table to postgres ######

df<-lang_enrollment_oct26 # assign as 'df'

# make sure no trailing spaces anywhere

names(df) <- gsub(" ", "", names(df))

df <- df %>%
  mutate(across(where(is.character), ~ replace(.x, .x == " ", "")))

# set column types

charvect <- rep("numeric", ncol(df))
charvect <- replace(charvect, c(1, 2, 3), c("varchar"))
names(charvect) <- names(df)

# set table info

table_name <- "rel_medical_enrollment_lang_county_jan27_qa"
schema <- "dashboard"
indicator <- "Total enrollment in the Medi-Cal by language spoken in June 2025, October 2026, and the percent change in enrollment between June 2025 and October 2026.
 Original data source: https:\\data.chhs.ca.gov\\dataset\\medi-cal-certified-eligibles-with-demographics-by-month"
source <- "R script:W:\\Project\\ECI\\HR 1 Tracker\\Github\\JZ\\HR1_Tracker\\Analysis\\rel_medical_totenrollment_county_jan27.R"
qa_filepath <- "W:\\Project\\ECI\\HEI\\Documentation\\QA_rel_medical_enrollment.docx"
table_comment <- paste0(indicator, source)

# push to postgres

dbWriteTable( con,
              DBI::Id(schema = schema, table = table_name),
              df,
              overwrite = TRUE, row.names = FALSE,
              field.types = charvect)

# add meta data programmatically because there are too many columns to manually type out

column_names <- colnames(df)

# Extract language from each column name
language <- column_names %>%
  str_remove("(_rate_2026_10|_rate_2025_06|_2026_10|_2025_06|_june25_oct26_diff)$") %>%
  str_replace_all("_", " ")

# Generate column comments based on column name patterns
column_comments <- case_when(
  
  str_detect(column_names, "_rate_2026_10$") ~
    paste0("Rate of Medi-Cal enrollees whose spoken language is ",
           language, " in October 2026 out of all Medi-Cal enrollees"),
  
  str_detect(column_names, "_rate_2025_06$") ~
    paste0("Rate of Medi-Cal enrollees whose spoken language is ",
           language, " in June 2025 out of all Medi-Cal enrollees"),
  
  str_detect(column_names, "_2026_10$") ~
    paste0("Count of Medi-Cal enrollees whose spoken language is ",
           language, " in October 2026"),
  
  str_detect(column_names, "_2025_06$") ~
    paste0("Count of Medi-Cal enrollees whose spoken language is ",
           language, " in June 2025"),
  
  str_detect(column_names, "_june25_oct26_diff$") ~
    paste0("Percentage point difference in the rate of Medi-Cal enrollees ",
           "whose spoken language is ", language,
           " between October 2026 and June 2025 (October 2026 minus June 2025)"),
  
  column_names == "county_geoid" ~ "County geographic identifier",
  column_names == "county_name" ~ "County name",
  column_names == "geolevel" ~ "Geographic level",
  column_names == "county" ~ "County name",
  
  TRUE ~ NA_character_
)

# Assign column names to the vector
names(column_comments) <- column_names

# check this worked --- looks good
data.frame(
  column_name = column_names,
  column_comment = column_comments
) %>%
  View()


# add comments 
add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

# COMMENTING OUT OLD COMMENT CODE FOR NOW
# # Get column names
# cols <- names(df)
# 
# # Generate column comments
# column_comments <- sapply(cols, function(col) {
#   
#   # First 3 columns
#   if (col == "county_geoid") return("county geoid")
#   if (col == "county_name") return("county name")
#   if (col == "geolevel") return("geolevel")
#   
#   # Extract language name by removing suffix
#   language <- col %>%
#     stringr::str_remove("_2026_10$|_2025_06$|_pct_change$") %>%
#     stringr::str_replace_all("_", " ")
#   
#   # Generate description based on suffix
#   if (grepl("_2026_10$", col)) {
#     paste0(
#       "Total Medi-Cal enrollment for ", language,
#       " language by county for October 2026"
#     )
#     
#   } else if (grepl("_2025_06$", col)) {
#     paste0(
#       "Total Medi-Cal enrollment for ", language,
#       " language by county for June 2025"
#     )
#     
#   } else if (grepl("_pct_change$", col)) {
#     paste0(
#       "Percent change in Medi-Cal enrollment from June 2025 to October 2026 for ",
#       language, " language by county"
#     )
#     
#   } else {
#     NA_character_
#   }
# })
# 
# # Remove names from vector
# column_comments <- unname(column_comments)
# 
# 
# # check this worked:
# 
# # 93 - good
# length(column_comments)
# 
# # View column names alongside generated comments: looks good!!!
# data.frame(
#   column_name = names(lang_enrollment_oct26),
#   column_comment = column_comments
# ) %>%
#   View()

# Analysis: Medi-Cal Expansion population: Immigrant enrollment in Medi-Cal -------------------

# This is a DIFFERENT data source than the Medi-Cal total enrollment analyses prior

# We are analyzing specifically the Medi-Cal expansion population to get at immigrants, documented+undocumented, with Medi-Cal

# Check how recent the data is in this raw dataset
medical_exp_0_19_raw%>%
  group_by(eligibility_date)%>%
  summarise(count=n())%>%
  View() # Up to JULY 2026

medical_exp_19_50plus_raw%>%
  group_by(reporting_period)%>%
  summarise(count=n())%>%
  filter(grepl("2026", reporting_period))%>%
  View() # Up to JULY 2026

# Establish before/after times for percent change calcs for Medi-Cal expansion---------------------------

before<- "2025-06" # SAME as all other medi-cal analyses thus far
after_exp<- "2026-07" # JULY because that is the most recent data for this dataset

# Check how many unique counties are in this dataset and if California state-wide is included

length(unique(medical_exp_0_19_raw$county)) # 59 (should be 58 counties)
length(unique(medical_exp_19_50plus_raw$county))  # 60 (should be 58 counties)

# explore what the extra county is that is in the medi-cal expansion tables but not in the california counties table

outlier<-medical_exp_0_19_raw %>%
  distinct(county) %>%
  anti_join(ca_counties %>% st_drop_geometry(),
            by = c("county" = "NAME")) # San Diego is the extra county? But I see it in both?

# the issue is there are values of 'San Diego ' with an extra space so lets clean those up
medical_exp_0_19_raw<-medical_exp_0_19_raw %>%
  mutate(county=ifelse(county %in% "San Diego ", "San Diego", county))

# now check unique county names
length(unique(medical_exp_0_19_raw$county)) # 58 -good

# now check what the extra county names are in the 19+ table

medical_exp_19_50plus_raw %>%
  distinct(county) %>%
  anti_join(ca_counties %>% st_drop_geometry(),
            by = c("county" = "NAME")) # This has 'Statewide' and 'Statewide total' which explains why there are 60 unique county names instead of 58

# Start with analysis for age 0-19

unique(medical_exp_0_19_raw$eligibility_date)

expansion_youth_july26<- medical_exp_0_19_raw %>%
  filter(eligibility_date %in% c(after_exp, before)) %>%
  group_by(eligibility_date, county) %>%
  summarise(
    total = sum(number_of_beneficiaries, na.rm = TRUE),
    .groups = "drop"
  ) 

# check how many NAs there are

sum(is.na(expansion_youth_july26$total)) # 0 that is good!

# Originally EQB asked for JUne 2025 --> June 2026 but I think it is OK to move the 'later' date to July 2026 and just create a consistent methodology where our 'end' date is always the most recent data available

# check how many unique  counties there are: 
length(unique(expansion_youth_july26$county)) # 57 out of 58 one is dropped

# Check what is being dropped -- Alpine 

medical_exp_0_19_raw %>%
  filter(county %in% setdiff(
    unique(medical_exp_0_19_raw$county),
    unique(expansion_youth_july26$county)
  )) %>%
  count(county, eligibility_date)

# revise code so Alpine is not dropped even if it doesn't have data for both our year/month of interest

expansion_youth_july26 <- medical_exp_0_19_raw %>%
  filter(eligibility_date %in% c(after_exp, before)) %>%
  group_by(eligibility_date, county) %>%
  summarise(
    total = sum(number_of_beneficiaries, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    county = unique(medical_exp_0_19_raw$county),
    eligibility_date = c(before, after_exp)
  )
# check that worked
length(unique(expansion_youth_july26$county)) # 58 -- good

# Proceed with calcs

expansion_youth_july26<-expansion_youth_july26%>%
  rename("month_of_eligibility"="eligibility_date")%>% # make this match other medi-cal dataset 
  pivot_wider(names_from=month_of_eligibility, values_from=total)%>%
  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  rename_with(~ paste0("expansion_youth_", .x), .cols = 2:3) %>%
  group_by(county)%>%
  mutate(expansion_youth_pct_change=(expansion_youth_2026_07-expansion_youth_2025_06)/expansion_youth_2025_06*100)%>%
  left_join(ca_counties%>%select(GEOID, NAME, geometry), by=c("county"="NAME"))%>%
  mutate(geolevel="county")%>%
  rename("county_name"="county",
         "county_geoid"="GEOID")%>%
  select(county_geoid, county_name, geolevel, everything()) %>%
  mutate(across(where(is.numeric),
                ~ replace(.x, is.infinite(.x) | is.nan(.x), NA_real_)))%>% # replace all Inf and NaN with NULL
  select(-geometry)%>% # Remove geom
  ungroup()

# one last check 
length(unique(expansion_youth_july26$county_name)) # 58 -- good


# now analyze separately for the 19+ expansion population

# Calcs using June 2026 as end date

qa <- medical_exp_19_50plus_raw %>%
  filter(grepl("July 2026|June 2025",reporting_period)) # some rows have a different format for the reporting_period column

expansion_adult_july26<- medical_exp_19_50plus_raw %>%
  filter(reporting_period %in% c(after_exp, before)) %>% 
  rename("month_of_eligibility"="reporting_period")%>% # make this match other medi-cal dataset
   group_by(month_of_eligibility, county) %>%
  summarise(
    total = sum(number_of_eligible_individuals, na.rm = TRUE), # aggregate all the 19+ age groups into one adult group
    .groups = "drop"
  ) %>%
  complete(
    county = unique(medical_exp_0_19_raw$county),
    month_of_eligibility = c(before, after_exp)
  )

# apply qa fix:

expansion_adult_july26_qa<- medical_exp_19_50plus_raw %>%
  mutate(reporting_period=ifelse(reporting_period %in% "July 2026", after_exp,
                                 ifelse(reporting_period %in% "June 2025", before, reporting_period)))%>% # fix the diff formatted reporting_period values
  filter(reporting_period %in% c(after_exp, before)) %>% 
  rename("month_of_eligibility"="reporting_period")%>% # make this match other medi-cal dataset
  group_by(month_of_eligibility, county) %>%
  summarise(
    total = sum(number_of_eligible_individuals, na.rm = TRUE), # aggregate all the 19+ age groups into one adult group
    .groups = "drop"
  ) %>%
  complete(
    county = unique(medical_exp_0_19_raw$county),
    month_of_eligibility = c(before, after_exp)
  )

# Check QA fix worked: 
expansion_adult_july26_qa%>%
  filter(county %in% qa$county)%>%
  View() # I will focus on Calaveras as my test county


check<-medical_exp_19_50plus_raw%>%
  filter(county=="Calaveras")%>%
  filter(reporting_period %in% c(after_exp, before, "June 2025", "July 2026"))

# Caleveras total in our before period should be 209 (19+118+72) which is what I see in expansion_adult_july26_qa 

# Comparing expansion_adult_july26_qa values to expansion_adult_july26 I also see these counties increased in total eligibles
# Looks good

# Reassign expansion_adult_july26_qa as main analysis df

expansion_adult_july26<-expansion_adult_july26_qa

# how many NAs are there:
sum(is.na(expansion_adult_july26_qa$total)) # 0!

# quick check we aggregated all the age groups to get a combined total for age 19

check<-medical_exp_19_50plus_raw%>%
  filter(county=='Alameda')%>%
  filter(reporting_period %in% c(after_exp, "July 2026")) %>%
  summarise(total=sum(number_of_eligible_individuals)) #41920 for July 2026 including both date formats

expansion_adult_july26%>%
  filter(county=="Alameda")%>%
  View() #41920 for July 2026 -- good

# continue with calcs and pivoting table wider

expansion_adult_july26<-expansion_adult_july26%>%
  pivot_wider(names_from=month_of_eligibility, values_from=total)%>%
  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  rename_with(~ paste0("expansion_adult_", .x), .cols = 2:3) %>%
  group_by(county)%>%
  mutate(expansion_enrollment_pct_change=(expansion_adult_2026_07-expansion_adult_2025_06)/expansion_adult_2025_06*100)%>%
  left_join(ca_counties%>%select(GEOID, NAME, geometry), by=c("county"="NAME"))%>%
  mutate(geolevel="county")%>%
  rename("county_name"="county",
         "county_geoid"="GEOID")%>%
  select(county_geoid, county_name, geolevel, everything()) %>%
  mutate(across(where(is.numeric),
                ~ replace(.x, is.infinite(.x) | is.nan(.x), NA_real_)))%>% # replace all Inf and NaN with NULL
  select(-geometry)%>% # I want to remove the spatial column
  filter(!grepl("Statewide|Statewide Total", county_name))%>% # remove row for statewide 
  ungroup()


# check number of unique counties 
length(unique(expansion_adult_july26$county_name)) # 58 good

# check NAs
sum(is.na(expansion_adult_july26$expansion_enrollment_pct_change)) # 2 --looking at it I see its because Sierra and Alpine counties had 0 enrollment for both June 2025 and July 2026 -thats OK

# Join tables together to calculate for TOTAL expansion population not disaggregated by age groups

qa <- expansion_total_july26<-expansion_youth_july26%>%
  left_join(expansion_adult_july26)%>%
  select(-ends_with(".y")) %>% # clean up the duplicate columns from the join
  rename_with(~ str_remove(.x, "\\.x$"), ends_with(".x")) %>%
  filter(if_any(everything(), is.na))
         
expansion_total_july26<-expansion_youth_july26%>%
  left_join(expansion_adult_july26)%>%
  select(-ends_with(".y")) %>% # clean up the duplicate columns from the join
  rename_with(~ str_remove(.x, "\\.x$"), ends_with(".x"))%>% # clean up column names
  
  # calculate total enrollment and percent change
  group_by(county_geoid)%>%
  mutate(expansion_total_2025_06=expansion_youth_2025_06+expansion_adult_2025_06,
         expansion_total_2026_07=expansion_youth_2026_07+expansion_adult_2026_07,
         expansion_total_pct_change=(expansion_total_2026_07-expansion_total_2025_06)/expansion_total_2025_06*100)%>%
  mutate(across(where(is.numeric), ~ replace(.x, is.infinite(.x) | is.nan(.x), NA_real_)))%>% # replace all Inf and NaN with NULL
  ungroup()

# check class-- good
class(expansion_total_july26)

# check final unique county count
length(unique(expansion_total_july26$county_name)) # 58 good!

# check NAs
sum(is.na(expansion_total_july26$expansion_enrollment_pct_change)) # 2 good

###### Push Medi-cal expansion enrollment table to postgres ######

df<-expansion_total_july26 # assign as 'df'

# make sure no trailing spaces anywhere

names(df) <- gsub(" ", "", names(df))

df <- df %>%
  mutate(across(where(is.character), ~ replace(.x, .x == " ", "")))

# set column types

charvect <- rep("numeric", ncol(df))
charvect <- replace(charvect, c(1, 2, 3), c("varchar"))
names(charvect) <- names(df)

# set table info

table_name <- "rel_medical_enrollment_expansion_county_jan27_qa"
schema <- "dashboard"
indicator <- "Total enrollment in the Medi-Cal expansion program in June 2025, July 2026, and the percent change in enrollment between June 2025 and July 2026.
Data is cut by ages 0-19, age 19-plus, and for the entire medi-cal expansion population which aggregates the enrollment totals
for age 0-10 and age 19+ Original data source: https:\\data.chhs.ca.gov/dataset/medi-cal-adult-expansion"
source <- "R script:W:\\Project\\ECI\\HR 1 Tracker\\Github\\JZ\\HR1_Tracker\\Analysis\\rel_medical_totenrollment_county_jan27.R"
qa_filepath <- "W:\\Project\\ECI\\HEI\\Documentation\\QA_rel_medical_enrollment.docx"
table_comment <- paste0(indicator, source)

# push to postgres

dbWriteTable( con,
              DBI::Id(schema = schema, table = table_name),
              df,
              overwrite = TRUE, row.names = FALSE,
              field.types = charvect)

# add meta data

column_names <- colnames(df) # Get column names

column_comments <- column_comments <- c(
  'county geoid',
  'county name',
  'geolevel',
  'Total medi-cal expansion enrollment by county for June 2025 for age 0-19',
  'Total medi-cal expansion enrollment by county for July 2026 for age 0-19',
  'Percent change in medi-cal expansion enrollment by county from June 2025 to July 2026 for age 0-19',
  'Total medi-cal expansion enrollment by county for June 2025 for adults 19 plus',
  'Total medi-cal expansion enrollment by county for July 2026 for adults 19 plus',
  'Percent change in medi-cal expansion enrollment by county from June 2025 to July 2026 for adults 19 plus',
  'Total medi-cal expansion enrollment by county for June 2025 for entire medi-cal expansion population',
  'Total medi-cal expansion enrollment by county for July 2026 for entire medi-cal expansion population',
  'Percent change in medi-cal expansion enrollment by county from June 2025 to July 2026 for entire medi-cal expansion population'
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

# Analysis: Medi-Cal enrollment by race category ----------------------

# Create separate relational table for medi-cal monthly enrollment and
# percentage point difference between June 2025 and October 2026 by race
before_col<-gsub("-", "_", before) # June 2025
after_col<-gsub("-", "_", after) # October 2026

race_enrollment_oct26<- medical_race_raw %>%
  filter(month_of_eligibility %in% c(after, before)) %>%
  group_by(month_of_eligibility, county, ethnicity) %>%
  summarise(
    count = sum(total_eligibles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(month_of_eligibility, county) %>%
  mutate(total = sum(count, na.rm = TRUE),
         rate = count/total*100) %>%
  ungroup()%>%
  # Deciding that these ethnicity categories are NOT inclusive of Latinx
  mutate(ethnicity_re = case_when(
    ethnicity == "AI/AN"        ~ "nh_aian",
    ethnicity == "ASIAN"        ~ "nh_asian",
    ethnicity == "BLACK"        ~ "nh_black",
    ethnicity == "HISPANIC"     ~ "latinx",
    ethnicity == "NOT REPORTED" ~ "unreported",
    ethnicity == "WHITE"        ~ "nh_white",
    TRUE                        ~ "statewide"))%>%
# pivot table wide
  pivot_wider(
    id_cols = county,
    names_from = c(ethnicity_re, month_of_eligibility),
    values_from = rate,
    names_glue = "{ethnicity_re}_rate_{month_of_eligibility}",
    values_fill = 0)%>%

  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  
  group_by(county)%>%
  mutate(
    across(
      ends_with(before_col),
      ~ {
          col_2026 <- sub(before_col, after_col, cur_column())
          (get(col_2026) - .x) 
          
      },
      .names = "{sub(paste0('_', before_col, '$'), '', .col)}_diff")
    )%>%
  ungroup()
  

# Check the calculation worked: values match, looks good!

check<-race_enrollment_oct26%>%
  filter(county=="San Diego")%>%
  mutate(latinx_qa= latinx_rate_2026_10-latinx_rate_2025_06)%>%
  select(latinx_qa, latinx_rate_diff)

rm(check)


# left join county_geoid values and clean up columns to match naming conventions

race_enrollment_oct26<-race_enrollment_oct26%>%
  left_join(ca_counties%>%select(GEOID, NAME, geometry), by=c("county"="NAME"))%>%
  mutate(geolevel="county")%>%
  rename("county_name"="county",
         "county_geoid"="GEOID")%>%
  select(county_geoid, county_name, geolevel, everything())%>%
  select(-geometry, -starts_with("statewide"))%>% # Remove county geoms
  ungroup()

# check resulting table is a  df -- good
class(race_enrollment_oct26)


###### Push Medi-cal race enrollment table to postgres ######

df<-race_enrollment_june25 # assign as 'df'

# make sure no trailing spaces anywhere

names(df) <- gsub(" ", "", names(df))

df <- df %>%
  mutate(across(where(is.character), ~ replace(.x, .x == " ", "")))

# set column types

charvect <- rep("numeric", ncol(df))
charvect <- replace(charvect, c(1, 2, 3), c("varchar"))
names(charvect) <- names(df)

# set table info

table_name <- "rel_medical_enrollment_race_county_jan27"
schema <- "dashboard"
indicator <- "Enrollment rates in the Medi-Cal program by race in June 2025,
October 2026, and the percentage point difference in enrollment rates between October 2026 and June 2025.
Original data source: https://data.chhs.ca.gov/dataset/medi-cal-certified-eligibles-with-demographics-by-month"
source <- "R script:W:\\Project\\ECI\\HR 1 Tracker\\Github\\JZ\\HR1_Tracker\\Analysis\\rel_medical_totenrollment_county_jan27.R"
qa_filepath <- "W:\\Project\\ECI\\HR 1 Tracker\\Documentation\\QA_rel_medical_enrollment.docx"
table_comment <- paste0(indicator, source)

# push to postgres

dbWriteTable( con,
              DBI::Id(schema = schema, table = table_name),
              df,
              overwrite = TRUE, row.names = FALSE,
              field.types = charvect)

# add meta data

column_names <- colnames(df) # Get column names

column_comments <- c(
  'county geoid',
  'county name',
  'geolevel',
  'Enrollment rate by county for June 2025 for AIAN',
  'Enrollment rate by county for June 2025 for Asian',
  'Enrollment rate by county for June 2025 for Black',
  'Enrollment rate by county for June 2025 for Latinx',
  'Enrollment rate by county for June 2025 for unreported race/ethnicity',
  'Enrollment rate by county for June 2025 for White',
  'Enrollment rate by county for October 2026 for AIAN',
  'Enrollment rate by county for October 2026 for Asian',
  'Enrollment rate by county for October 2026 for Black',
  'Enrollment rate by county for October 2026 for Latinx',
  'Enrollment rate by county for October 2026 for unreported race/ethnicity',
  'Enrollment rate by county for October 2026 for White',
  'Difference in enrollment rate for AIAN between October 2026 and June 2025',
  'Difference in enrollment rate for Asian between October 2026 and June 2025',
  'Difference in enrollment rate for Black between October 2026 and June 2025',
  'Difference in enrollment rate for Latinx between October 2026 and June 2025',
  'Difference in enrollment rate for unreported race/ethnicity between October 2026 and June 2025',
  'Difference in enrollment rate for White between October 2026 and June 2025'
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)
