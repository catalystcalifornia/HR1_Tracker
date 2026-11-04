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
  select(county_name, geolevel, medical_enrollment_2025_06,medical_enrollment_2026_06, medical_enrollment_pct_change, geometry)

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

# Repeat for July 2025 ---> July 2026: very similar to June 2025-June 2026 so we could go with either

# Lets start with June 2025 as the base and compare to---> June 2026 

total_enrollment_july25<-medical_race_raw%>%
  filter(month_of_eligibility=="2026-07" | month_of_eligibility=="2025-07")%>%
  group_by(month_of_eligibility, county)%>%
  summarise(total=sum(total_eligibles))%>%
  ungroup()%>%
  pivot_wider(names_from=month_of_eligibility, values_from=total)%>%
  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  rename_with(~ paste0("medical_enrollment_", .x), .cols = 2:3) %>%
  group_by(county)%>%
  mutate(medical_enrollment_pct_change=(medical_enrollment_2026_07-medical_enrollment_2025_07)/medical_enrollment_2025_07*100
  )


# quick visual: July 2025 --> July 2026

ggplot(total_enrollment_june25,
       aes(x = county, y = medical_enrollment_pct_change)) +
  geom_col(fill = "darkblue") +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "Percent Change in Medi-Cal Enrollment by County",
    subtitle = "July 2025 to July 2026",
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
  mutate(ethnicity_re=ifelse(ethnicity %in% "AI/AN", "aian",
                                    ifelse(ethnicity %in% "ASIAN", "asian",
                                           ifelse(ethnicity %in% "BLACK", "black",
                                                  ifelse(ethnicity %in% "HISPANIC", "latinx",
                                                         ifelse(ethnicity %in% "NOT REPORTED", "unreported",
                                                                ifelse(ethnicity %in% "WHITE", "white", 
                                                                       "NULL")))))))%>%

  pivot_wider(names_from=month_of_eligibility, values_from=c("count","total"))%>%
  rename_with(~ gsub("-", "_", .x)) %>%  # clean up column names
  group_by(county, aid_category)%>%
  mutate(june25_june26_pct_change=(count_2026_06-count_2025_06)/count_2025_06*100
  )



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

