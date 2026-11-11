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

# clean up column names
renewals_raw<-renewals_raw%>%  
  rename_with(~ str_to_lower(.x) %>%
  str_replace_all("\\.", "_"))


# Read in california counties using tigris so I can join to my relational table later: 

ca_counties <- counties(
  state = "CA",
  year = 2026,
  cb = FALSE
)
