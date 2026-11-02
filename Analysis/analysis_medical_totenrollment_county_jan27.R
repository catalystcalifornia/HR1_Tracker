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


