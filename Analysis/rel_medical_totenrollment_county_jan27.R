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

# Read in california counties using tigris so I can join to my relational table: 

ca_counties <- counties(
  state = "CA",
  year = 2026,
  cb = FALSE
)

# Data exploration: Does not need full QA------------------------

# I want to understand if the sub categories within each of these tables adds to a 'total' universe of people enrolled in MediCal

# To do this lets take a single county / time period and add the subcategories fore two tables and see if the totals are the same
total_race_lac<-medical_race_raw%>%
  filter(county=="Los Angeles" & month_of_eligibility=="2026-10")%>%
  summarise(total=sum(total_eligibles)) # 3622919

total_aid_lac<-medical_aid_raw%>%
  filter(county=="Los Angeles" & month_of_eligibility=="2026-10")%>%
  summarise(total=sum(total_eligibles)) # 3622919 --  the same

total_lang_lac<-medical_lang_raw%>%
  filter(county=="Los Angeles" & month_of_eligibility=="2026-10")%>%
  summarise(total=sum(total_eligibles)) # 3622919 --  the same

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

# Establish before/after times for percent change calcs---------------------------

# Set your percent time change START and END times here: this way if we want to make changes we only have to reset it once here

before<-"2025-06" # June 2025
after<-"2026-10" # October 2026


# Analysis: Total Medi-Cal enrollment -----------------------

# Lets start with June 2025 as the base and compare to---> OCTOBER 2026 

total_enrollment_oct26_qa<-medical_race_raw%>%
  filter(month_of_eligibility==after | month_of_eligibility==before)%>%
  group_by(month_of_eligibility, county)%>%
  summarise(total=sum(total_eligibles))%>%
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

# 16 NAs because they don't have June 2025 OR October 2026 enrollment figures -seem like smaller areas so this is OK
sum(is.na(total_enrollment_oct26$medical_enrollment_pct_change))

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


# Analyis:  Medi-Cal enrollment by aid category -----------------------

# See what the most recent data is
medical_aid_raw%>%
  group_by(month_of_eligibility)%>%
  summarise(count=n())%>%
  View() # Up to OCT 2026

# Lets stick with June 2025 as the base and compare to---> OCT 2026 

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



# how many NAs
sum(is.na(aid_enrollment_oct26$june25_oct26_pct_change)) # only 4 -Alpine, Mono for small AID groups makes sense

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

# Continue making this into a rel table:

# recode aid_category values to be more concise and remove the STATEWIDE and SUPPRESSED CELL rows so only counties remain
medical_aid_re<-medical_aid_raw%>%
  filter(!grepl("Suppressed Cell Total|STATEWIDE", county))%>%
  mutate(aid_category_re=ifelse(aid_category %in% "ACA Expansion Adult Age 19 to 64", "aca_exp_adult",
                                ifelse(aid_category %in% "Adoption/Foster Care", "adopt_foster",
                                       ifelse(aid_category %in% "CHIP", "chip",
                                              ifelse(aid_category %in% "LTC", "ltc",
                                                     ifelse(aid_category %in% "Other", "other",
                                                            ifelse(aid_category %in% "Parent/Caretaker Relative & Child", "parent_caretaker",
                                                                   ifelse(aid_category %in% "Seniors and Persons with Disabilities", "senior_disabled",
                                                                          "Did not get recoded"))))))))

# check that worked
table(medical_aid_re$aid_category) # good 
table(medical_aid_re$aid_category_re) # good -

# pivot table wide

aid_enrollment_oct26<-medical_aid_re %>%
  filter(month_of_eligibility %in% c(after, before)) %>%
  pivot_wider(
    id_cols = county,
    names_from = c(aid_category_re, month_of_eligibility),
    values_from = total_eligibles,
    names_glue = "{aid_category_re}_{month_of_eligibility}",
    values_fill = 0)%>%
  rename_with(~ gsub("-", "_", .x))   # clean up column names

# proceed with calcs

aid_enrollment_oct26<-aid_enrollment_oct26%>%
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

aid_enrollment_oct26%>%
  filter(county=="San Diego")%>%
  mutate(chip_qa=(chip_2026_10-chip_2025_06)/chip_2025_06*100)%>%
  select(chip_qa, chip_pct_change)%>%
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

table_name <- "rel_medical_enrollment_aid_county_jan27"
schema <- "dashboard"
indicator <- "Total enrollment in the Medi-Cal by aid program in June 2025, October 2026, and the percent change in enrollment between June 2025 and October 2026.
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

# add meta data

column_names <- colnames(df) # Get column names

column_comments <- column_comments <- c(
  'county geoid',
  'county name',
  'geolevel',
  'Total medi-cal enrollment in aca expansion adult aid category by county for October 2026',
  'Total medi-cal enrollment in the adopted/foster aid category by county for October 2026',
  'Total medi-cal enrollment in the CHIP aid category by county for October 2026',
  'Total medi-cal enrollment in the LTC aid category by county for October 2026',
  'Total medi-cal enrollment in the OTHER aid category by county for October 2026',
  'Total medi-cal enrollment in the parent or caretaker with child aid category by county for October 2026',
  'Total medi-cal enrollment in the senior disabled aid category by county for October 2026',
  
  'Total medi-cal enrollment in aca expansion adult aid category by county for June 2025',
  'Total medi-cal enrollment in the adopted/foster aid category by county for June 2025',
  'Total medi-cal enrollment in the CHIP aid category by county for June 2025',
  'Total medi-cal enrollment in the LTC aid category by county for June 2025',
  'Total medi-cal enrollment in the OTHER aid category by county for June 2025',
  'Total medi-cal enrollment in the parent or caretaker with child aid category by county for June 2025',
  'Total medi-cal enrollment in the senior disabled aid category by county for June 2025',
  
  'Percent change in medi-cal enrollment from June 2025 to october 2026 in the aca expansion adult aid category by county',
  'Percent change in medi-cal enrollment from June 2025 to october 2026 in the adopted/foster aid category by county for June 2025',
  'Percent change in medi-cal enrollment from June 2025 to october 2026 in the CHIP aid category by county',
  'Percent change in medi-cal enrollment from June 2025 to october 2026 in the LTC aid category by county',
  'Percent change in medi-cal enrollment from June 2025 to october 2026t in the OTHER aid category by county',
  'Percent change in medi-cal enrollment from June 2025 to october 2026 in the parent or caretaker with child aid category by county5',
  'Percent change in medi-cal enrollment from June 2025 to october 2026 in the senior disabled aid category by county'
  

)

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

lang_enrollment_oct26%>%
  select(missing_unknown_2026_10) # has

lang_enrollment_oct26%>%
  select(unknown_2026_10) # does not have 

lang_enrollment_oct26%>%
  select(missing_unknown_2025_06) # has 

lang_enrollment_oct26%>%
  select(unknown_2025_06) # has both unknown AND missing/unknown for 2025

# We should just have one 'missing' language that is a combination of missing/unknown+unknown

lang_enrollment_oct26<-lang_enrollment_oct26%>%
  group_by(county)%>%
  mutate(unknown_2025_06=unknown_2025_06+missing_unknown_2025_06)%>% # make the 2025 unknown = unknown + unknown/missing
  select(-missing_unknown_2025_06)%>% # now remove the unknown/missing for 2025
  rename("unknown_2026_10"="missing_unknown_2026_10") # rename 2026 to just unknown

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

lang_enrollment_oct26%>%
  select(punjabi_2025_06) # none for 2025

lang_enrollment_oct26%>%
  select(punjabi_2026_10) # has in 2026

lang_enrollment_oct26%>%
  select(hindi_2025_06) # none

lang_enrollment_oct26%>%
  select(hindi_2026_10) # has in 2026

# Unfortunately will not be able to calculate percent change for Hindi / Punjabi from 2025 to 2026, will have to exclude entirely for now

lang_enrollment_oct26<-lang_enrollment_oct26%>%
  select(-c("punjabi_2026_10", "hindi_2026_10"))

# now see if there are differences

cols_2026 <- names(lang_enrollment_oct26) %>%
  stringr::str_subset("_2026_10$")

expected_2025 <- sub("2026_10$", "2025_06", cols_2026)

setdiff(expected_2025, names(lang_enrollment_oct26)) # no more ! 

# proceed with calcs:

lang_enrollment_oct26<-lang_enrollment_oct26%>%
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

lang_enrollment_oct26%>%
  filter(county=="Los Angeles")%>%
  mutate(korean_qa=(korean_2026_10-korean_2025_06)/korean_2025_06*100)%>%
  select(korean_qa, korean_pct_change)%>%
  View()


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

table_name <- "rel_medical_enrollment_lang_county_jan27"
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

# Get column names
cols <- names(df)

# Generate column comments
column_comments <- sapply(cols, function(col) {
  
  # First 3 columns
  if (col == "county_geoid") return("county geoid")
  if (col == "county_name") return("county name")
  if (col == "geolevel") return("geolevel")
  
  # Extract language name by removing suffix
  language <- col %>%
    stringr::str_remove("_2026_10$|_2025_06$|_pct_change$") %>%
    stringr::str_replace_all("_", " ")
  
  # Generate description based on suffix
  if (grepl("_2026_10$", col)) {
    paste0(
      "Total Medi-Cal enrollment for ", language,
      " language by county for October 2026"
    )
    
  } else if (grepl("_2025_06$", col)) {
    paste0(
      "Total Medi-Cal enrollment for ", language,
      " language by county for June 2025"
    )
    
  } else if (grepl("_pct_change$", col)) {
    paste0(
      "Percent change in Medi-Cal enrollment from June 2025 to October 2026 for ",
      language, " language by county"
    )
    
  } else {
    NA_character_
  }
})

# Remove names from vector
column_comments <- unname(column_comments)


# check this worked:

# 93 - good
length(column_comments)

# View column names alongside generated comments: looks good!!!
data.frame(
  column_name = names(lang_enrollment_oct26),
  column_comment = column_comments
) %>%
  View()

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

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

# how many NAs are there:
sum(is.na(expansion_adult_july26$total)) # only 2

# quick check we aggregated all the age groups to get a combined total for age 19

check<-medical_exp_19_50plus_raw%>%
  filter(county=='Alameda')%>%
  filter(reporting_period %in% c(after_exp)) %>%
  summarise(total=sum(number_of_eligible_individuals)) #17031 for July 2026

expansion_adult_july26%>%
  filter(county=="Alameda")%>%
  View() #17031 for July 2026 -- good

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
sum(is.na(expansion_adult_july26$expansion_enrollment_pct_change)) # from 2 to 3? --looking at it I see its because Sierra and Trinity counties had 0 enrollment for both June 2025 and July 2026 -thats OK

# Join tables together to calculate for TOTAL expansion population not disaggregated by age groups

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
sum(is.na(expansion_total_july26$expansion_enrollment_pct_change)) # 3 good

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

table_name <- "rel_medical_enrollment_expansion_county_jan27"
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

# RACE ANALYSIS: HANDING OFF TO ALICIA: NOT READY FOR QA YET 11/18/26----------------------

# Create separate relational table for medi-cal monthly enrollment and percent change between June 2025 and June 2026 by race

race_enrollment_june25<- medical_race_raw %>%
  filter(month_of_eligibility %in% c("2026-06", before)) %>%
  group_by(month_of_eligibility, county, ethnicity) %>%
  summarise(
    count = sum(total_eligibles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(month_of_eligibility, county) %>%
  mutate(total = sum(count)) %>%
  ungroup()%>%
  # NOTE I am still researching whether or not these ethnicity categories are not inclusive of Latinx. No definitive answer yet. Reached out to DHCS and can come back and adjust labels when I get more information
  
  mutate(ethnicity_re=ifelse(ethnicity %in% "AI/AN", "nh_aian",
                                    ifelse(ethnicity %in% "ASIAN", "nh_asian",
                                           ifelse(ethnicity %in% "BLACK", "nh_black",
                                                  ifelse(ethnicity %in% "HISPANIC", "latinx",
                                                         ifelse(ethnicity %in% "NOT REPORTED", "unreported",
                                                                ifelse(ethnicity %in% "WHITE", "nh_white", 
                                                                       "statewide")))))))%>%
# pivot table wide
  pivot_wider(
    id_cols = county,
    names_from = c(ethnicity_re, month_of_eligibility),
    values_from = count,
    names_glue = "{ethnicity_re}_{month_of_eligibility}",
    values_fill = 0)%>%

  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  
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

check<-race_enrollment_june25%>%
  filter(county=="San Diego")%>%
  mutate(latinx_qa=(latinx_2026_10-latinx_2025_06)/latinx_2025_06*100)%>%
  select(latinx_qa, latinx_pct_change)

rm(check)


# left join county_geoid values and clean up columns to match naming conventions

race_enrollment_june25<-race_enrollment_june25%>%
  left_join(ca_counties%>%select(GEOID, NAME, geometry), by=c("county"="NAME"))%>%
  mutate(geolevel="county")%>%
  rename("county_name"="county",
         "county_geoid"="GEOID")%>%
  select(county_geoid, county_name, geolevel, everything())%>%
  select(-geometry)%>% # Remove county geoms
  ungroup()

# check resulting table is a  df -- good
class(race_enrollment_june25)
