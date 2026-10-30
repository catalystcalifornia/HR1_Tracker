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


# Medi-Cal monthly total enrollment data-------------------------------

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
# [1] "archived_by_aid_category_certified_eligibles_data_dictionary"             --OK to excluse since it is archived
# [2] "archived_by_aid_category_certified_eligibles"                             --OK to excluse since it is archived

# [3] "by_age_group_and_sex_certified_eligibles_data_dictionary"                 
# [4] "by_age_group_and_sex_certified_eligibles"                                 

# [5] "by_aid_category_certified_eligibles_data_dictionary"                      
# [6] "by_aid_category_certified_eligibles"                                      ---of interest

# [7] "by_delivery_system_and_plan_certified_eligibles_data_dictionary"          
# [8] "by_delivery_system_and_plan_certified_eligibles"                          

# [9] "by_medicare_dual_status_and_age_group_certified_eligibles_data_dictionary" 
# [10] "by_medicare_dual_status_and_age_group_certified_eligibles"                

# [11] "by_medicare_dual_status_certified_eligibles_data_dictionary"              
# [12] "by_medicare_dual_status_certified_eligibles"                              

# [13] "by_primary_language_certified_eligibles_data_dictionary"                ---of interest  
# [14] "by_primary_language_certified_eligibles"                                --of interest  

# [15] "by_race_ethnicity_and_age_group_certified_eligibles_data_dictionary"   ---of interest   
# [16] "by_race_ethnicity_and_age_group_certified_eligibles"                    ---of interest  

# [17] "reference_medi_cal_aid_codes_and_aid_code_groups_data_dictionary"       
# [18] "reference_medi_cal_aid_codes_and_aid_code_groups"       

# Lets pull out all the tables as dfs for my R environment and push all of them to rda_shared_data even if only some are of most interest for the HR 1 tracker

# Age Sex

age_sex_df<-data_list[["by_age_group_and_sex_certified_eligibles"]]
age_sex_dict<-data_list[["by_aid_category_certified_eligibles_data_dictionary"]]

# Aid category: 

aid_category_df<-data_list[["by_aid_category_certified_eligibles"]]
aid_category_dict<-data_list[["by_aid_category_certified_eligibles_data_dictionary"]]

# Delivery system

delivery_df<-data_list[["by_delivery_system_and_plan_certified_eligibles"]]
delivery_dict<-data_list[["by_delivery_system_and_plan_certified_eligibles_data_dictionary"]]

# Dual status and age

dual_age_df<-data_list[["by_medicare_dual_status_and_age_group_certified_eligibles"]]
dual_age_dict<-data_list[["by_medicare_dual_status_and_age_group_certified_eligibles_data_dictionary"]]

# Dual status 

dual_df<-data_list[["by_medicare_dual_status_certified_eligibles"]]
dual_dict<-data_list[["by_medicare_dual_status_certified_eligibles_data_dictionary"]]


# Race and age

race_age_df<-data_list[["by_race_ethnicity_and_age_group_certified_eligibles"]]
race_age_dict<-data_list[["by_race_ethnicity_and_age_group_certified_eligibles_data_dictionary"]]

# Language spoken

lang_df<-data_list[["by_primary_language_certified_eligibles"]]
lang_dict<-data_list[["by_primary_language_certified_eligibles_data_dictionary"]]

# aid code reference files --I don't really see these codes used in any of the dataframes
aid_ref<-data_list[["reference_medi_cal_aid_codes_and_aid_code_groups"]]
aid_ref_dict<-data_list[["reference_medi_cal_aid_codes_and_aid_code_groups_data_dictionary"]]


# Clean up all the column names across each of these dfs

# Find all dataframe names ending in _df
df_names <- ls(pattern = "_df$")

for (df_name in df_names) {
  
  df <- get(df_name)
  
  names(df) <- names(df) %>%
    gsub("-", "_", x = .) %>%
    gsub(" ", "_", x = .) %>%
    gsub("\\.", "_", x = .) %>%
    tolower()
  
  assign(df_name, df)
}

###### Push each of these tables to rda_shared_data.health in postgres ######

# Rename all tables to how I want them to be named in postgres

cchs_medical_eligibles_age_sex_county_2010_26<-age_sex_df
cchs_medical_eligibles_aid_cat_county_2010_26<-aid_category_df
cchs_medical_eligibles_delivery_system_county_2010_26<-delivery_df
cchs_medical_eligibles_dual_coverage_age_county_2010_26<-dual_age_df
cchs_medical_eligibles_dual_coverage_county_2010_26<-dual_df
cchs_medical_eligibles_race_age_county_2010_26<-race_age_df
cchs_medical_eligibles_language_county_2010_26<-lang_df

# Use loop to push all data frames to postgres at once

df_names <- ls(pattern = "^cchs_")

# check that worked
print(df_names) # good

# Set PostgreSQL schema
schema <- "health"

# Set table comment info:
source<-"https://data.chhs.ca.gov/dataset/medi-cal-certified-eligibles-with-demographics-by-month"
QA<-"W:\\Project\\ECI\\HR 1 Tracker\\Documentation\\QA_medical_data_import.docx"

# Loop through each dataframe
for (df_name in df_names) {
  
  # Get dataframe
  df <- get(df_name)
  
  # Use dataframe name as PostgreSQL table name
  table_name <- df_name
  
  # make sure no trailing spaces anywhere
  
  names(df) <- gsub(" ", "", names(df))
  
  df[df == " "] <- ""
  
  
  # Push dataframe to PostgreSQL
  dbWriteTable(
    con_shared,
    DBI::Id(schema = schema, table = table_name),
    df,
    overwrite = TRUE,
    row.names = FALSE
  )
  
  message("Uploaded: ", schema, ".", table_name)
  
  # Add table comments
  table_comment <- paste0(
    "QA doc: ",QA," | ",
    table_name,
    " | Source: ",source)
  
  
  # Add PostgreSQL table comment
  sql <- paste0(
    "COMMENT ON TABLE ",
    as.character(dbQuoteIdentifier(con_shared, DBI::Id(schema = schema, table = table_name))),
    " IS ",
    as.character(dbQuoteString(con_shared, table_comment))
  )
  
  dbExecute(con_shared, sql)
  
}


#### Medi-Cal SB 75 age 0-19 + 19-65 enrollment: documented + undocumented folks ####

# read in youth 0-19 table: only 1 csv file
# URL: https://data.chhs.ca.gov/dataset/sb-75-full-scope-medi-cal-for-all-children-enrollment 

youth_raw<-read.csv("W:\\Project\\ECI\\HR 1 Tracker\\Data\\MediCal\\sb-75-full-scope-medi-cal-for-all-children-enrollment-urcxncax\\sb-75-full-scope-medi-cal-for-all-children-enrollment.csv")

# Clean column names
colnames(youth_raw) <- colnames(youth_raw) %>%
  str_to_lower() %>%
  str_replace_all("[. ]+", "_")

youth_df<-youth_raw%>%
  mutate(age_group="age0_19")%>% # add column for age group designation
mutate(number_of_beneficiaries=as.numeric(number_of_beneficiaries))

# side by side compare youth_raw to youth_df make sure they still match after some cleanup, looks good. 

###### Push Youth expansion table (age 0-19) to postgres ######

# make sure no trailing spaces anywhere

names(youth_df) <- gsub(" ", "", names(youth_df))

youth_df[youth_df == " "] <- ""

# set column types

charvect = rep("varchar", ncol(youth_df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(3), c("numeric"))

# add df colnames to the character vector

names(charvect) <- colnames(youth_df)

table_name <- "cchs_medical_expansion_age0_19_2016_26"
schema <- "health"
indicator <- "This dataset includes the monthly count of individuals under age 19 receiving full scope Medi-Cal benefits as authorized by California Welfare and Institutions Code 14007.8. The count reflects the total number eligible during the month. California provides full scope Medi-Cal benefits to all children under the age of 19 regardless of immigration status. This program is referred to as SB 75 and was implemented May 16, 2016 pursuant to SB 75 (Chapter 18, Statutes of 2015), Section 14007.8 and added to the Welfare and Institutions Code and amended by SB 4 (Chapter 709, Statutes of 2015)."
source <- "R script: W:\\Project\\ECI\\HR 1 Tracker\\Github\\JZ\\HR1_Tracker\\Data Prep\\medical_data_import.R"
qa_filepath <- "W:\\Project\\ECI\\HR 1 Tracker\\Documentation\\QA_medical_data_import.docx"
table_comment <- paste0(indicator, source)

# push to postgres

dbWriteTable(con_shared,  
             DBI::Id(schema = schema, table = table_name),
             youth_df,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

column_names <- colnames(youth_df) # Get column names

column_comments <- c(
  'eligibility date',
  'county',
  'total number of beneficiaries',
  'annotation code',
  'annotation code description',
  'age group')

add_table_comments(con_shared, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)


# read in the adult files: multiple csv files
# URL: https://data.chhs.ca.gov/dataset/medi-cal-adult-expansion 

folder <- "W:\\Project\\ECI\\HR 1 Tracker\\Data\\MediCal\\medi-cal-adult-expansion-ptrvpz8j"

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
    str_replace_all("-", "_")%>% # clean up the csv flie names by replacing the dashes - with _ to make filenames more R friendly
    { if_else(
      str_detect(., "_dictionary"),
      .,
      str_c(., "_df"))}) # add a _df to the end of all the item names to make it easier to do group data frame clean up actions

# Check names of imported csv files --all the csv files got read in- good
names(data_list)

# For each of these list items I want to create a column called 'age_group' where I populate it with the corresponding age the data table has data for

data_list <- imap(data_list, ~ .x %>%
                    mutate(
                      age_group = case_when(
                        str_starts(.y, "young_ae_19_25") ~ "19_25",
                        str_starts(.y, "age_26_49_ae_population") ~ "26_49",
                        str_starts(.y, "older_ae_50_and_over") ~ "50_over",
                        TRUE ~ NA_character_
                      )
                    )
)


# add all these list items as dataframes in my R environment

list2env(data_list, envir = .GlobalEnv)

# continue cleaning up all of these table names and column names 

# Get all dataframe names ending in _df
df_names <- ls(pattern = "_df$")

for (df_name in df_names) {
  df <- get(df_name)
  
  # Clean column names
  colnames(df) <- colnames(df) %>%
    str_to_lower() %>%
    str_replace_all("[. ]+", "_")
  
  # Remove _and_ and _by_ to shorten table names
  new_name <- df_name %>%
    str_replace_all("_(and|by)_", "_")
  
  # Save dataframe with updated name
  assign(new_name, df, envir = .GlobalEnv)
  
  # Remove original if name changed
  if (new_name != df_name) {
    rm(list = df_name, envir = .GlobalEnv)
  }
}

# skimming my R environment table names look better now

# Combine all the eligible totals only tables into one combined table with all age groups

age19_50_plus_total<-rbind(young_ae_19_25_population_county_df, age_26_49_ae_population_county_df,
                           older_ae_50_over_population_county_df)


# make sure it looks good -yes
View(age19_50_plus_total)

# rename tables for uploading to postgres:

cchs_medical_expansion_age19_50plus_2020_26<-age19_50_plus_total

# separate tables for the age26-49 pop with different demographic cuts

cchs_medical_expansion_age26_49_race_2020_26<-age_26_49_ae_population_ethnicity_county_df
cchs_medical_expansion_age26_49_spokenlang_2020_26<-age_26_49_ae_population_spoken_language_county_df
cchs_medical_expansion_age26_49_writtenlang_2020_26<-age_26_49_ae_population_written_language_county_df

###### Push all 4  Medi-Cal 19-50+ age expansion tables to postgres ######

# For the adult expansion tables: Use loop to push all data frames to postgres at once

df_names <- ls(pattern = "^cchs_medical_expansion")

# check that worked
print(df_names) # good

# Set PostgreSQL schema
schema <- "health"

# Set table comment info:
source<-"https://data.chhs.ca.gov/dataset/medi-cal-adult-expansion"
QA<-"W:\\Project\\ECI\\HR 1 Tracker\\Documentation\\QA_medical_data_import.docx"
description<-"Age 19-25 Young Adult Expansion Population: The datasets include the monthly count of individuals ages 19 through 25, by county, receiving full scope Medi-Cal benefits. The counts reflect the total number of eligible individuals enrolled during the month. California provides full scope Medi-Cal benefits to young adults ages 19 through 25, regardless of immigration status. Lawfully present individuals ages 21 through 25, who are not in a pregnancy aid code, are included in this count. Individuals in the dataset are eligible for federal financial participation for their emergency and pregnancy-related services. The Young Adult Expansion program was implemented January 1, 2020, pursuant to Senate Bill 104 (Chapter 67, Statutes of 2019), California Welfare and Institutions Code Section 14007.8. Age 26-49 Adult Expansion Population: The datasets include the monthly count of individuals 26-49 years of age, by county, receiving full scope Medi-Cal benefits. The counts reflect the total number of eligible individuals enrolled during the month. California provides full scope Medi-Cal benefits to adults 26-49 years of age, regardless of immigration status. Lawfully present individuals 26-49 years of age, who are not in a pregnancy aid code, are included in this count. Individuals in the dataset are eligible for federal financial participation for their emergency and pregnancy-related services. The Age 26-49 Adult Expansion program was implemented January 1, 2024, pursuant to Senate Bill (SB) 184 (Chapter 47, Statutes of 2022), California Welfare and Institutions Code Section 14007.8. Age 50 and over Older Adult Expansion Population: The datasets include the monthly count of individuals 50 years of age or older, by county, receiving full scope Medi-Cal benefits. The counts reflect the total number of eligible individuals enrolled during the month. California provides full scope Medi-Cal benefits to adults 50 years of age or older, regardless of immigration status. Lawfully present individuals 50 years of age or older, who are not in a pregnancy aid code, are included in this count. Individuals in the dataset are eligible for federal financial participation for their emergency and pregnancy-related services. The Older Adult Expansion program was implemented May 1, 2022, pursuant to Assembly Bill (AB) 133 (Chapter 143, Statutes of 2021), California Welfare and Institutions Code Section 14007.8."
  

# Loop through each dataframe
for (df_name in df_names) {
  
  # Get dataframe
  df <- get(df_name)
  
  # Use dataframe name as PostgreSQL table name
  table_name <- df_name
  
  # make sure no trailing spaces anywhere
  
  names(df) <- gsub(" ", "", names(df))
  
  df[df == " "] <- ""
  
  
  # Push dataframe to PostgreSQL
  dbWriteTable(
    con_shared,
    DBI::Id(schema = schema, table = table_name),
    df,
    overwrite = TRUE,
    row.names = FALSE
  )
  
  message("Uploaded: ", schema, ".", table_name)
  
  # Add table comments
  table_comment <- paste0(
    "QA doc: ",QA," | ",
    table_name,
    " | Source: ",source," ",description)
  
  
  # Add PostgreSQL table comment
  sql <- paste0(
    "COMMENT ON TABLE ",
    as.character(dbQuoteIdentifier(con_shared, DBI::Id(schema = schema, table = table_name))),
    " IS ",
    as.character(dbQuoteString(con_shared, table_comment))
  )
  
  dbExecute(con_shared, sql)
  
}


