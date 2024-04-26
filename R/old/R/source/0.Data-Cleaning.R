library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)
library(ggplot2)
library(patchwork)
library(REDCapR)
library(Hmisc)
library(readxl)

# Download Data from API
uri <- 'https://redcap.ictr.wisc.edu/api/'
load('data/RedCap/token.RData')
currentDate <- Sys.Date()
data <- redcap_read_oneshot(redcap_uri = uri, token = token, export_survey_fields = TRUE)$data

# Label the RedCap Raw Enrolled Data
source('R/source/Functions.R')
source('R/source/0.RedCap_labels.R')

# Process the raw data
enrolled_ids <- data |> 
  filter(redcap_event_name %in% c('questionnaires_arm_4', 'questionnaires_arm_5', 'questionnaires_arm_6')) %>%
  filter(str_detect(record_id, '^MAXED')) |> 
  pull(record_id) |> 
  unique()
  
redcap_raw_enrolled <- data %>%
  filter(record_id %in% enrolled_ids) %>%
  mutate(study_visit = recode(redcap_event_name, 
                              'questionnaires_arm_3' = 'Intake',
                              'questionnaires_arm_4' = 'Day_A',
                              'questionnaires_arm_5' = 'Day_B',
                              'questionnaires_arm_6' = 'Day_C',
                              'questionnaires_arm_7' = 'Parents')) %>%
  select(record_id, study_visit, everything()) %>%
  recalculate_groups()

# Save RedCap Raw Enrolled Data
save(redcap_raw_enrolled, file = 'data/RedCap/redcap_raw_enrolled.RData')

# Scorekeep and create RedCap Wide and long files for Survey Data
source('R/source/0.Scorekeep.R')

# Clean Exercise Session Data
source('R/source/0.Exercise-Session-Cleaning.R')

# Clean BISS data (Exercise and Rest Sessions)
source('R/source/0.BISS-Cleaning.R')

# Clean Affect data (Exercise and Rest Sessions)
source('R/source/0.Affect-Cleaning.R')
