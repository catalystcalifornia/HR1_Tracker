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
library(lubridate)

# Connect to postgres

source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("eqb_hr1_tracker")
con_shared<-connect_to_db("rda_shared_data")
drv <- dbDriver("PostgreSQL")

# Read in tables from postgres:

medical_race_raw<-dbGetQuery(con_shared, "SELECT * FROM health.cchs_medical_eligibles_race_age_county_2010_26")

# get medi-cal total enrollment not by race

medical_enrollment<-medical_race_raw%>%
  group_by(month_of_eligibility, county)%>%
  summarise(total=sum(total_eligibles))%>%
  ungroup()

# identify counties that have NA values
medical_enrollment %>%
  mutate(month = ym(month_of_eligibility)) %>%
  filter(month < as.Date("2025-07-01")) %>%
  group_by(county) %>%
  summarise(
    n_observations = n(),
    n_valid = sum(!is.na(total)),
    .groups = "drop"
  ) %>%
  arrange(n_valid)

# create model that skips counties with insufficient amount of total enrollment numb ers

df <- medical_enrollment %>%
  mutate(month = ym(month_of_eligibility)) %>%
  arrange(county, month) %>%
  rename(enrollment = total) %>%
  group_by(county) %>%
  mutate(time = row_number()) %>%
  group_modify(~ {
    
    pre_data <- .x %>%
      filter(
        month < as.Date("2025-07-01"),
        !is.na(enrollment),
        !is.na(time)
      )
    
    if (nrow(pre_data) < 2) {
      return(.x %>%
               mutate(
                 projected_enrollment = NA_real_,
                 enrollment_gap = NA_real_,
                 enrollment_gap_pct = NA_real_
               ))
    }
    
    pre_model <- lm(enrollment ~ time, data = pre_data)
    
    .x %>%
      mutate(
        projected_enrollment = as.numeric(
          predict(pre_model, newdata = .x)
        ),
        enrollment_gap = enrollment - projected_enrollment,
        enrollment_gap_pct =
          100 * enrollment_gap / projected_enrollment
      )
  }) %>%
  ungroup()




summary(pre_model)
