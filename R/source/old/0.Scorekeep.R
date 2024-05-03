# 0. Load packages
library(readxl)
library(haven)
library(cgwtools)
library(scorekeeper)
library(purrr)
library(tibble)
library(dplyr)
library(REDCapR)
library(anytime)
library(labelled)
library(stringr)
library(Hmisc)

# 1. Download Data from API
uri <- 'https://redcap.ictr.wisc.edu/api/'
load('data/RedCap/token.RData')
currentDate <- Sys.Date()

# Writes a .csv file of the captured data from redcap

df <- redcap_read_oneshot(redcap_uri = uri, token = token)$data  |> 
  filter (redcap_event_name != 'consent_arm_2')  |> 
  filter (redcap_event_name != 'online_screening_arm_1') |> 
  filter (str_detect(record_id, 'M'))

rawfilename = paste('data/RedCap/MAXED_redcap_raw.', currentDate, '.csv', sep = "")
write.csv(df, file = rawfilename)

# 2. Scorekeep

# Load Individual Scoresheets
filenames <- list.files('scoresheets_clean/') # MAKE SURE THIS IS THE CORRECT DIRECTORY NAME
filenames <- paste('scoresheets_clean', filenames, sep = '/' )
ldf <- lapply(filenames, read_xlsx)

# List of Measure Names from the Scoresheets
measures <- list.files('scoresheets_clean/') 
measures <- gsub('.xlsx*', '', measures)

# Names the scoresheets
names(ldf) <- measures

# Cleans and saves cleaned data for each measure 

x <- vector(mode = 'list', length = (length(measures)))
names(x) <- measures

tibble_func_1 <- function(x) {
  y = as_tibble(x)
  return (y) }

cleaned_data <- purrr::map(x, tibble_func_1)

for (i in 1:length(measures)) {
  cleaned <- scorekeep(df, ldf[[i]])
  cleaned_last <- cleaned[[max(ldf[[i]]$step)]]
  cleaned_data[[i]] <-cleaned_last
}

## 3.2. Weight Suppression Variables

WT_VARS <- df %>% select('record_id', 'redcap_event_name', starts_with(c('edhistory', 'dcf', 'bmi'))) %>% 
  mutate (dcf_high_wt = as.numeric(dcf_height)) %>% 
  mutate (wt_suppress_high_current_obj = dcf_high_wt - bmi_weight_kg*2.205) %>% 
  mutate (wt_suppress_high_settle_sr = dcf_high_wt - as.numeric(dcf_weight_settle_weight)) %>% 
  mutate (wt_suppress_high_low  = dcf_high_wt - as.numeric(dcf_weight_low)) %>% 
  select (record_id, dcf_high_wt, starts_with(c('bmi', 'wt_suppress', 'dcf_weight_settle'))) %>% 
  filter (!is.na(dcf_high_wt)) %>% 
  rename (id = 'record_id')
  

# 4. Long Dataset

cleaned_data <- cleaned_data %>% 
  append(list(WT_VARS)) 

names(cleaned_data)[[length(cleaned_data)]] <- 'WT_VARS'


# 5. Clear environment 
rm(list = setdiff(ls(), 'cleaned_data'))

long_data <- cleaned_data[[1]]

j = 2
while (j <= length(cleaned_data)) {
  long_data <- full_join(long_data, cleaned_data[[j]])
  j = j+1
}


long_data <- long_data %>% 
  mutate(timepoint = recode(timepoint, questionnaires_arm_3 = 'Intake', questionnaires_arm_4= 'Day_A', `questionnaires_arm_5` = 'Day_B', `questionnaires_arm_6` = 'Day_C', `questionnaires_arm_7` = 'Parents')) 



# 5. Wide Dataset
cols <- colnames(long_data)
cols <- cols[-c(1,2)]

wide_data <- long_data %>% 
  tidyr::pivot_wider(names_from = timepoint, 
                     values_from = cols)


# Datasets

MAXED_redcap_rdf <- cleaned_data 
MAXED_redcap_long <- long_data
MAXED_redcap_wide <- wide_data


# Move old files
filenames <- as.list(list.files('data/RedCap/'))
currentDate <- Sys.Date()

old_rdf <- filenames[grepl('MAXED_redcap_rdf[^|]*$', filenames)][[1]]
old_long <- filenames[grepl('MAXED_redcap_long[^|]*$', filenames)][[1]]
old_wide <- filenames[grepl('MAXED_redcap_wide[^|]*$', filenames)][[1]]
old_raw <- filenames[grepl('MAXED_redcap_raw[^|]*$', filenames)][[1]]


file.copy (from = paste0('data/RedCap/', old_long), to = paste0('data/RedCap/old/',old_long))
file.remove (from = paste0('data/RedCap/', old_long))


file.copy (from = paste0('data/RedCap/', old_wide), to = paste0('data/RedCap/old/' ,old_wide))
file.remove (from = paste0('data/RedCap/', old_wide))

file.copy (from = paste0('data/RedCap/', old_raw), to = paste0('data/RedCap/old/',old_raw))
file.remove (from = paste0('data/RedCap/', old_raw))


file.copy (from = paste0('data/RedCap/', old_rdf), to = paste0('data/RedCap/old/', old_rdf))
file.remove (from = paste0('data/RedCap/', old_rdf))

# Save!
rdffilename <-  paste('data/RedCap/MAXED_redcap_rdf.', currentDate, '.RData', sep = "")
save(MAXED_redcap_rdf, file = rdffilename)
longfilename <- paste('data/RedCap/MAXED_redcap_long.', currentDate, '.RData', sep = "")
save(MAXED_redcap_long, file = longfilename)
widefilename <- paste('data/RedCap/MAXED_redcap_wide.', currentDate, '.RData', sep = "")
save(MAXED_redcap_wide, file = widefilename)

rm(list = ls())

