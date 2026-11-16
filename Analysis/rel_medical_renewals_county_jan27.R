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

# Read in data originally from: https://data.chhs.ca.gov/dataset/medi-cal-annual-renewals-by-county
### Saved in the W: W:\Project\ECI\HR 1 Tracker\Data\MediCal\medi-cal-annual-renewals-by-county-e3p5lpof

renewals_raw<-read.csv("W:\\Project\\ECI\\HR 1 Tracker\\Data\\MediCal\\medi-cal-annual-renewals-by-county-e3p5lpof\\medi-cal-annual-renewals-by-county.csv")
renewals_dict<-read.csv("W:\\Project\\ECI\\HR 1 Tracker\\Data\\MediCal\\medi-cal-annual-renewals-by-county-e3p5lpof\\medi-cal-annual-renewals-by-county-data-dictionary.csv")

# clean up column names
renewals_raw<-renewals_raw%>%  
  rename_with(~ str_to_lower(.x) %>%
  str_replace_all("\\.", "_"))


# Read in california counties using tigris so I can get county geoids later 

ca_counties <- counties(
  state = "CA",
  year = 2026,
  cb = FALSE
)

# Explore data ------------------------------------------

##### What is the date ranges this data has ######

renewals_raw%>%
 group_by(year)%>%
  summarise(count=n()) # 2018 - 2026 


renewals_raw%>%
  group_by(renewal_due_month)%>%
  summarise(count=n())%>%
  View()

# Only goes up to April 2026

# Methodology exploration--------------------------------------

# QUESTION: MAGI + NON MAGI OR JUST MAGI ?

# Explore different methodologies we want to do with renewals

####### Variables of interest based on arguments we care about: ######

#ADMINISTRATIVE BURDEN: 

###Renewals Due / Renewals Completed / Renewals Completed by Due Date ---- wondering if it  makes more sense to combine these as a ratio
###Discontinued - Failure to Respond


#NEW ELIGIBILITY REQUIREMENTS:

###Continued Medi-Cal Eligibility


###### Option 1: Percent change April 2025 - April 2026 ######

# I want to get a sense of what the different renewal variables are looking like in terms of how they changed from April 2025 - April 2026 
## 

renewals_april<-renewals_raw%>%
  filter(renewal_due_month=="2026-04" | renewal_due_month=="2025-04") # filter only for April 25 and 26 

# Analyze percent change for: Continued medi-cal Eligibility

renewals_cont_medical<-renewals_april%>%
  select(renewal_due_month, county, eligibility_financial_method, continued_medi_cal_eligibility)%>%
  mutate(continued_medi_cal_eligibility=as.numeric(continued_medi_cal_eligibility))%>%
  group_by(renewal_due_month, county)%>%
  summarise(
    continued_medi_cal_eligibility =   sum(continued_medi_cal_eligibility, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(eligibility_financial_method = "COMBINED"))%>% # calculate combined for MAGI + NonMAGI
  ungroup()


######### Su

###### Option 2: Renewals due vs. completed | completed by due date ratio ######

# Wondering if this gets a bit at administrative burden by comparing what the usual ratio of renewals due to renewals actually completed (or completed by deadline) is 
# We can compare that ratio from April 2025 to April 2025

