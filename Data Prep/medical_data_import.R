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
con2<-connect_to_db("rda_shared_data")
drv <- dbDriver("PostgreSQL")


# Read in Medi-Cal monthly total enrollment data-------------------------------

#### Medi-Cal Certified Eligibles Data by Month with Demographics ####

# URL: https://data.chhs.ca.gov/dataset/medi-cal-certified-eligibles-with-demographics-by-month

# W Folder containing the extracted Excel files

folder <- "W:\\Project\\ECI\\HR 1 Tracker\\Data\\MediCal\\medi-cal-certified-eligibles-with-demographics-by-month-g232qi98"

# Find all csv files in the folder
files <- list.files(
  path = folder,
  pattern = "\\.csv?$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Read each CSV into a separate dataframe within a list
data_list <- set_names(
  map(files, read_csv),
  tools::file_path_sans_ext(basename(files)) %>%
    str_replace_all("-", "_")) # clean up the csv flie names by replacing the dashes - with _ to make filenames more R friendly

# Check names of imported csv files --all the csv files got read in- good
names(data_list)
# 
# [1] "archived_by_aid_category_certified_eligibles_data_dictionary"             
# [2] "archived_by_aid_category_certified_eligibles"                             
# [3] "by_age_group_and_sex_certified_eligibles_data_dictionary"                 
# [4] "by_age_group_and_sex_certified_eligibles"                                 
# [5] "by_aid_category_certified_eligibles_data_dictionary"                      
# [6] "by_aid_category_certified_eligibles"                                      
# [7] "by_delivery_system_and_plan_certified_eligibles_data_dictionary"          
# [8] "by_delivery_system_and_plan_certified_eligibles"                          
# [9] "by_medicare_dual_status_and_age_group_certified_eligibles_data_dictionary"
# [10] "by_medicare_dual_status_and_age_group_certified_eligibles"                
# [11] "by_medicare_dual_status_certified_eligibles_data_dictionary"              
# [12] "by_medicare_dual_status_certified_eligibles"                              
# [13] "by_primary_language_certified_eligibles_data_dictionary"                  
# [14] "by_primary_language_certified_eligibles"                                  
# [15] "by_race_ethnicity_and_age_group_certified_eligibles_data_dictionary"      
# [16] "by_race_ethnicity_and_age_group_certified_eligibles"                      
# [17] "reference_medi_cal_aid_codes_and_aid_code_groups_data_dictionary"         
# [18] "reference_medi_cal_aid_codes_and_aid_code_groups"       

# Lets pull out initial tables of interest as dfs for my R environment

# Aid category: 

aid_category_df<-data_list[["by_aid_category_certified_eligibles"]]
aid_category_dict<-data_list[["by_aid_category_certified_eligibles_data_dictionary"]]


# Race and age

race_age_df<-data_list[["by_age_group_and_sex_certified_eligibles"]]
race_age_dict<-data_list[["by_age_group_and_sex_certified_eligibles_data_dictionary"]]
