source('R/source/Functions.R')

# Exercise Session Info
load('data/RedCap/redcap_raw_enrolled.RData')
filenames <- as.list(list.files('data/RedCap/'))
longfile <- filenames[grepl('MAXED_redcap_long[^|]*$', filenames)][[1]]
load(paste0('data/RedCap/', longfile))

groups <- recalculate_groups(redcap_raw_enrolled)
groups$id = as.character((groups$record_id))
groups <- select(groups, id, group, group_factor) |> 
  filter(!is.na(group)) 

# Clean Rest Session Data
rest_data <- redcap_raw_enrolled %>%
  select(record_id, study_visit, starts_with('rest')) %>%
  rename(id = 'record_id') %>%
  filter(study_visit == 'Day_A') %>%
  mutate(across(starts_with('rest'), ~ as.numeric(.)))  |> 
  pivot_longer(
    cols = starts_with('rest'), # Selects all columns that start with 'rest'
    names_pattern = "rest_(.*)_(\\d+)", # Captures the measure and the time point
    names_to = c("variable", "time"), # Creates two new columns: 'variable' and 'time'
    values_to = "value" # The values in the pivoted columns
  ) |> 
  filter(!is.na(value)) %>%
  separate(variable, into = c("construct", "variable"), sep = "_", remove = FALSE) 

rest_data <- rest_data |> 
  filter(construct == 'hr') |> 
  mutate(variable = 'Heart Rate',
         condition = 'Rest', 
         day = 'a',
         id = as.character(id), 
         time = as.numeric(time)) |> 
  select(-construct)

ex_data <-  redcap_raw_enrolled |> 
  select(record_id, starts_with('ex_hr'), starts_with('ex_hr_0'), starts_with('ex_watts'), starts_with('ex_dis')) |> 
  rename(id = 'record_id')
# Clean Exercise Session Data
ex_data$id = as.character(ex_data$id)

enrolled_ids <- redcap_raw_enrolled %>%
  select(record_id) %>%
  distinct()

cet <- MAXED_redcap_long |> 
  filter(id %in% enrolled_ids$record_id,
         timepoint == 'Day_C') |> 
  select(id, cet_total_weighted_sum, cet_rigid_subscale, cet_wtcontrol_subscale, cet_enjoy_subscale, cet_mood_subscale, cet_avoid_subscale, cet_clinical)

ex_demos <- redcap_raw_enrolled |> 
  select(record_id, studya_age, studya_max_hr) |> 
  filter(!is.na(studya_age)) |> 
  rename(id = ('record_id')) |> 
  mutate(id = as.character(id))

# Pull out the a and b datasets
ex_data_a <- ex_data %>%
  select(id, !ends_with('_b')) %>%
  filter(rowSums(is.na(across(-id))) != (ncol(.) - 1))
names(ex_data_a)[-1] <- paste0(names(ex_data_a)[-1], "_a")

ex_data_b <- process_ex_data(ex_data, '_b')
ex_data <- full_join(ex_data_a, ex_data_b)

# Pivot longer and further processing
ex_data_1 <- ex_data %>%
  mutate(across(-id, as.numeric)) %>%
  pivot_longer(cols = -c(id),
               names_pattern = "(.*_)([0-9]+)_(.)", 
               names_to = c("variable", "time", "day"),
               values_to = "value") %>%
  mutate(variable = str_remove(variable, "_$")) %>%
  mutate(time = as.numeric(time),
         variable = str_replace(variable, "^(ex)_?", ""),
         variable = recode(variable, 'hr' = 'Heart Rate', 'watts' = 'Watts', 'dis' = 'Distance', 'calm' = 'Calm', 'borg' = 'Percieved Exertion'),
         condition = recode(day, 'a' = 'Self-Paced', 'b' = 'Prescribed'),
         study_visit = recode(day, 'a' = 'Day_A', 'b' = 'Day_B')) %>%
  full_join(rest_data) |> 
  full_join(cet) %>%
  full_join(groups) %>%
  full_join(ex_demos) 


# Save Exercise Paramter Data
save(ex_data, file = 'data/Exercise_Params/Exercise_Session_Data.RData')
