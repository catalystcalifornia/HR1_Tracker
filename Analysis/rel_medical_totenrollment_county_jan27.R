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

# Data exploration------------------------

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
# Before HR 1 benchmark: June 2025, July 2025, October 2025
# ‘After’ HR 1 benchmark: June 2026, July 2026, October 2026

# Lets see if those ranges of months/dates exist in the data


table(medical_race_raw$month_of_eligibility) # yes I see data for 2025 and 2026 June/July. But for some reason ONLY October 2025 is missing...

table(medical_lang_raw$month_of_eligibility) # missing here too
table(medical_aid_raw$month_of_eligibility) # missing here too


###### Total Medi-Cal enrollment ######

# Lets start with June 2025 as the base and compare to---> June 2026 

total_enrollment_june25<-medical_race_raw%>%
  filter(month_of_eligibility=="2026-06" | month_of_eligibility=="2025-06")%>%
  group_by(month_of_eligibility, county)%>%
  summarise(total=sum(total_eligibles))%>%
  ungroup()%>%
  pivot_wider(names_from=month_of_eligibility, values_from=total)%>%
  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  rename_with(~ paste0("medical_enrollment_", .x), .cols = 2:3) %>%group_by(county)%>%
  mutate(medical_enrollment_pct_change=(medical_enrollment_2026_06-medical_enrollment_2025_06)/medical_enrollment_2025_06*100)%>%
  left_join(ca_counties%>%select(GEOID, NAME, geometry), by=c("county"="NAME"))%>%
  mutate(geolevel="county")%>%
  rename("county_name"="county",
         "county_geoid"="GEOID")%>%
  select(county_geoid, county_name, geolevel, everything()) %>%
  st_as_sf(sf_column_name = "geometry") # I want to preserve the spatial column

# check resulting table is a spatial df -- good
class(total_enrollment_june25)

# view as map
mapview(total_enrollment_june25) # good

# export this as a xlsx file:
write.xlsx(total_enrollment_june25,
file = "W:/Project/ECI/HR 1 Tracker/Data Exploration/medical_enrollment_june25_june26.xlsx", overwrite = TRUE)


# quick visual: June 2025 --> June 2026

ggplot(total_enrollment_june25,
       aes(x = county, y = june25_june26_pct_change)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "Percent Change in Medi-Cal Enrollment by County",
    subtitle = "June 2025 to June 2026",
    x = "County",
    y = "Percent Change (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )


######  Medi-Cal enrollment by race  ######


race_enrollment_june25<- medical_race_raw %>%
  filter(month_of_eligibility %in% c("2026-06", "2025-06")) %>%
  group_by(month_of_eligibility, county, ethnicity) %>%
  summarise(
    count = sum(total_eligibles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(month_of_eligibility, county) %>%
  mutate(total = sum(count)) %>%
  ungroup()%>%
  # NOTE I am still researching whether or not these ethnicity categories are not inclusive of Latinx. No definitive answer yet. 
  
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
        col_2026 <- sub("2025_06$", "2026_06", cur_column())
        
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
  mutate(latinx_qa=(latinx_2026_06-latinx_2025_06)/latinx_2025_06*100)%>%
  select(latinx_qa, latinx_pct_change)

rm(check)


# left join county geoms to get county_geoid values and clean up columns to match naming conventions

race_enrollment_june25<-race_enrollment_june25%>%
  left_join(ca_counties%>%select(GEOID, NAME, geometry), by=c("county"="NAME"))%>%
  mutate(geolevel="county")%>%
  rename("county_name"="county",
         "county_geoid"="GEOID")%>%
  select(county_geoid, county_name, geolevel, everything())%>%
  st_as_sf(sf_column_name = "geometry") # I want to preserve the spatial column

# check resulting table is a spatial df -- good
class(race_enrollment_june25)

# view as map
mapview(race_enrollment_june25) # good


##### Language Spoken Medi-Cal enrollment ######




###### IMMIGRANT enrollment in Medi-Cal ######

# We are analyzing specifically the Medi-Cal expansion population to get at immigrants, documented+undocumented, with Medi-Cal

# focus just on analzying expansion for age 0-19

expansion_youth_june25<- medical_exp_0_19_raw %>%
  filter(eligibility_date %in% c("2026-06", "2025-06")) %>%
  group_by(eligibility_date, county) %>%
  summarise(
    total = sum(number_of_beneficiaries, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename("month_of_eligibility"="eligibility_date")%>% # make this match other medi-cal dataset 
  pivot_wider(names_from=month_of_eligibility, values_from=total)%>%
  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  rename_with(~ paste0("expansion_youth_", .x), .cols = 2:3) %>%
    group_by(county)%>%
  mutate(expansion_youth_pct_change=(expansion_youth_2026_06-expansion_youth_2025_06)/expansion_youth_2025_06*100)%>%
  left_join(ca_counties%>%select(GEOID, NAME, geometry), by=c("county"="NAME"))%>%
  mutate(geolevel="county")%>%
  rename("county_name"="county",
         "county_geoid"="GEOID")%>%
  select(county_geoid, county_name, geolevel, everything()) %>%
  mutate(across(where(is.numeric),
                ~ replace(.x, is.infinite(.x) | is.nan(.x), NA_real_)))%>% # replace all Inf and NaN with NULL
  st_as_sf(sf_column_name = "geometry") # I want to preserve the spatial column


# now analyze separately for the 19+ expansion population

expansion_adult_june25<- medical_exp_19_50plus_raw %>%
  filter(reporting_period %in% c("2026-06", "2025-06")) %>%
  group_by(reporting_period, county) %>%
  summarise(
    total = sum(number_of_eligible_individuals, na.rm = TRUE),
    .groups = "drop"
  ) 

# quick check we aggregated all the age groups to get a combined total for age 19

check<-medical_exp_19_50plus_raw%>%
  filter(county=='Alameda')%>%
  filter(reporting_period %in% c("2026-06")) %>%
  summarise(total=sum(number_of_eligible_individuals)) #17471 for June 2026

expansion_adult_june25%>%
  filter(county=="Alameda")%>%
  View() #17471 for June 2026 -- good

# continue with calcs and pivoting table wider

expansion_adult_june25<-expansion_adult_june25%>%
  rename("month_of_eligibility"="reporting_period")%>% # make this match other medi-cal dataset 
  pivot_wider(names_from=month_of_eligibility, values_from=total)%>%
  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  rename_with(~ paste0("expansion_adult_", .x), .cols = 2:3) %>%
  group_by(county)%>%
  mutate(expansion_enrollment_pct_change=(expansion_adult_2026_06-expansion_adult_2025_06)/expansion_adult_2025_06*100)%>%
  left_join(ca_counties%>%select(GEOID, NAME, geometry), by=c("county"="NAME"))%>%
  mutate(geolevel="county")%>%
  rename("county_name"="county",
         "county_geoid"="GEOID")%>%
  select(county_geoid, county_name, geolevel, everything()) %>%
  mutate(across(where(is.numeric),
                ~ replace(.x, is.infinite(.x) | is.nan(.x), NA_real_)))%>% # replace all Inf and NaN with NULL
  st_as_sf(sf_column_name = "geometry") # I want to preserve the spatial column

# Join tables together to calculate for TOTAL expansion population not disagrgegated by age groups

expansion_total_june25<-expansion_youth_june25%>%
  left_join(expansion_adult_june25 %>% st_drop_geometry(),
            by = "county_geoid")%>%
  select(-ends_with(".y")) %>% # clean up the duplicate columns from the join
  rename_with(~ str_remove(.x, "\\.x$"), ends_with(".x"))%>% # clean up column names

  # calculate total enrollment and percent change
  group_by(county_geoid)%>%
  mutate(expansion_total_2025_06=expansion_youth_2025_06+expansion_adult_2025_06,
         expansion_total_2026_06=expansion_youth_2026_06+expansion_adult_2026_06,
         expansion_total_pct_change=(expansion_total_2025_06+expansion_total_2026_06)/expansion_total_2025_06*100)%>%
relocate(geometry, .after = last_col())%>%
  st_as_sf(sf_column_name = "geometry")

# check class-- good
class(expansion_total_june25)

mapview(expansion_total_june25)

###### Push medi-cal expansion enrollment table to postgres ######

df<-expansion_total_june25 # assign as 'df'

# make sure this is still a spatial df -- good
class(df)

# make sure no trailing spaces anywhere

names(df) <- gsub(" ", "", names(df))

df <- df %>%
  mutate(across(where(is.character), ~ replace(.x, .x == " ", "")))

# set column types

charvect <- rep("numeric", ncol(df) - 1)
charvect <- replace(charvect, c(1, 2, 3), c("varchar"))
names(charvect) <- names(df)[names(df) != "geometry"]

sf::st_geometry(df) <- "geometry"

# set table info

table_name <- "rel_medical_enrollment_expansion_county_jan27"
schema <- "dashboard"
indicator <- "Total enrollment in the Medi-Cal expansion program in June 2025, June 2026, and the percent change in enrollment between June 2025 and June 2026.
Data is cut by ages 0-19, age 19-plus, and for the entire medi-cal expansion population which aggregates the enrollment totals
for age 0-10 and age 19+ Original data source: https://data.chhs.ca.gov/dataset/medi-cal-adult-expansion"
source <- "R script:W:\\Project\\ECI\\HR 1 Tracker\\Github\\JZ\\HR1_Tracker\\Analysis\\rel_medical_totenrollment_county_jan27.R"
qa_filepath <- "W:\\Project\\ECI\\HEI\\Documentation\\QA_rel_medical.docx"
table_comment <- paste0(indicator, source)

# push to postgres

# dbWriteTable(con,  table_name, df,
#              overwrite = TRUE, row.names = FALSE,
#              field.types = charvect)

# Push sf dataframe to PostGIS
st_write(
  df,
  dsn = con,
  layer = Id(schema = schema, table =table_name),
  delete_layer = TRUE
)

# add meta data

column_names <- colnames(df) # Get column names

column_comments <- c(
  'indicator ID number',
  'tract',
  'Total renter occupied housing units (denominator)',
  'Total renter occupied housing units MOE',
  'Total renter occupied housing units that are rent burdened. This is defined as spending 50% or more of total income on rent',
  'Total renter occupied housing units that are rent burdened MOE',
  'Total renter occupied housing units that are rent burdened CV',
  'Percentage of renter occupied units that are rent burdened out of total renter occupied units by tract',
  'MOE of the percentage of renter occupied units that are rent burdened',
  'CV of the percentage of renter occupied units that are rent burdened',
  'Percent rank of the estimate rentburden_rate')

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)
  
###### Combine medi-cal total enrollment and by race tables into one wide table ######

medical_enrollment<-total_enrollment_june25%>%
  left_join(
    race_enrollment_june25 %>% st_drop_geometry(),
    by = "county_geoid" ) %>%
  relocate(geometry, .after = last_col())%>%
select(-ends_with(".y")) %>% # clean up the duplicate columns from the join
  rename_with(~ str_remove(.x, "\\.x$"), ends_with(".x")) # clean up column names

# check class-- good
class(medical_enrollment)

#map
mapview(medical_enrollment)


# Analysis tables (not for dashboard)?-------------------------------------

######  Medi-Cal enrollment by aid category ######

# I feel like this one is probably more for the grants but won't be on the dashboard. OK to keep in long format?

# Lets stick with June 2025 as the base and compare to---> June 2026 

aid_enrollment_june25<- medical_aid_raw %>%
  filter(month_of_eligibility %in% c("2026-06", "2025-06")) %>%
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
  mutate(june25_june26_pct_change=(count_2026_06-count_2025_06)/count_2025_06*100)


# export this as a xlsx file:
write.xlsx(aid_enrollment_june25,
           file = "W:/Project/ECI/HR 1 Tracker/Data Exploration/medical_aid_june25_june26.xlsx", overwrite = TRUE)

