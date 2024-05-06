library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)
library(lubridate)


load('data/RedCap/redcap_raw_enrolled.RData')
source('R/source/Functions.R')
pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', "MAXED_1011", "MAXED_1012")

redcap_raw_enrolled <- redcap_raw_enrolled |> 
  filter(!record_id %in% pilot_ids)

biss <-  redcap_raw_enrolled |> 
  select(record_id, study_visit, starts_with('ex_phys'), starts_with ('ex_shap'), starts_with('ex_wt'), starts_with('ex_attract'), starts_with('ex_looks'), starts_with ('ex_avg'), starts_with('rest_bis')) |> 
  mutate(across(-c(record_id, study_visit), as.numeric))  # Convert all except 'record_id' to numeric

groups <- recalculate_groups(redcap_raw_enrolled)
groups$id = as.character((groups$record_id))
groups <- select(groups, id, group, group_factor) |> 
  filter(!is.na(group)) 

biss_b <- biss %>%
  select(record_id, ends_with('_b')) %>%
  filter(rowSums(is.na(select(., -record_id))) != (ncol(.) - 1))

biss_a_ex <- biss |> 
  select(record_id, !ends_with('_b') & !starts_with('rest') & !contains('study_visit')) %>%
  filter(rowSums(is.na(select(., -record_id))) != (ncol(.) - 1))  

names(biss_a_ex)[-1] <- paste0(names(biss_a_ex)[-1], "_a")

biss_ex <- full_join(biss_a_ex, biss_b)

biss_long_ex <- biss_ex %>%
  pivot_longer(
    cols =  -record_id,
    names_pattern = "(.*_)([0-9]+)_(.)", 
    names_to = c("variable", "time", "day"),
    values_to = "value"
  ) %>%
  mutate(variable = str_remove(variable, "_$")) |> 
  separate(variable, into = c("condition", "variable"), sep = "_", remove = FALSE) |> 
  mutate(condition = recode(day, "a" = "Self-Paced", "b" = "Prescribed"))

biss_rest_a <- biss |> 
  select(record_id, starts_with('rest') & !ends_with("_c")) |> 
  filter(rowSums(is.na(select(cur_data(), -record_id))) != (ncol(select(cur_data(), -record_id)))) |>
  pivot_longer(
    cols = -record_id,
    names_pattern = "(.*_)([0-9]+)", 
    names_to = c("variable", "time"),
    values_to = "value"
  ) |> 
  mutate(variable = str_remove(variable, "_$")) |>  # Removes the trailing underscore
  separate(variable, into = c("condition", "construct", "variable"), sep = "_", remove = FALSE) |> 
  mutate(condition = 'Rest', 
         day = 'a') |>
         select(-construct) 

biss_rest_c <- biss |> 
    select(record_id, starts_with('rest') & ends_with("_c"))%>%
    filter(rowSums(is.na(select(., -record_id))) != (ncol(.) - 1)) |> 
    pivot_longer(
      cols = -record_id,
      names_pattern = "(.*_)([0-9]+)(_.*)", 
      names_to = c("variable", "time", "day"),
      values_to = "value"
    ) |> 
  mutate(variable = str_remove(variable, "_$")) |> 
  separate(variable, into = c("condition", "construct", "variable"), sep = "_", remove = FALSE) |> 
  mutate(condition = 'Rest', 
         day = 'c') |>
  select(-construct) 

  # Removes the trailing underscore
  
BISS_rest <- full_join(biss_rest_a, biss_rest_c) 

BISS <- full_join(BISS_rest, biss_long_ex)

# Add pre values for rest condition - have to get these from the 'nvs_pre' variables
BISS_Rest_0 <- redcap_raw_enrolled |> 
  select(record_id, study_visit, starts_with('nvs_biss')) |> 
  select(record_id, study_visit, contains('pre') & !contains('factor')) |> 
  # convert biss variables to numeric
  mutate(across(-c(record_id,study_visit), as.numeric)) |>
  pivot_longer(
    cols = -c(record_id, study_visit),
    names_pattern = "(.*_)([0-9]+)(_.*)",
    names_to = c("construct", "var_number", "time"),
    values_to = "value"
  ) |> 
  filter(!is.na(value)) |>
  select(-c(construct, time)) |> 
  mutate(variable = recode(var_number, '1' = 'phys', '2' = 'shap', '3' = 'wt', '4' = 'attract', '5' = 'looks', '6' = 'avg')) |> 
  select(-c(var_number)) |> 
  mutate(condition = 'Rest', 
         time = '0', 
         day = recode(study_visit, 'Day_A' = 'a', 'Day_C' = 'c')) |> 
  select(-c(study_visit))

BISS <- BISS %>%
  #reverse score the attractiveness and shape questions
  mutate(value = case_when(
    variable %in% c("attract", "shap", "avg") & value == 1 ~ 9,
    variable %in% c("attract", "shap", "avg") & value == 2 ~ 8,
    variable %in% c("attract", "shap", "avg") & value == 3 ~ 7,
    variable %in% c("attract", "shap", "avg") & value == 4 ~ 6,
    variable %in% c("attract", "shap", "avg") & value == 6 ~ 4,
    variable %in% c("attract", "shap", "avg") & value == 7 ~ 3,
    variable %in% c("attract", "shap", "avg") & value == 8 ~ 2,
    variable %in% c("attract", "shap", "avg") & value == 9 ~ 1,
    TRUE ~ value
  ))

BISS <- full_join(BISS, BISS_Rest_0) 

# Reverse code specific variables

BISS <- BISS %>%
  rename(id = 'record_id') |> 
  mutate(id = as.character(id)) 

# join groups
BISS <- left_join(BISS, groups)
BISS$time <- as.numeric(as.character(BISS$time))
#  Recode variable names
BISS <- BISS %>%
  mutate(variable = recode(variable, 'phys' = 'Appearance', 'shap' = 'Shape', 'wt' = 'Weight', 'attract' = 'Phys Attract', 'looks' = 'Looks', 'avg' = 'Avg Person'),
         day = recode(day, 'a'= 'Day_A', 'b' = 'Day_B', c = 'Day_C')) 
# Add average values
averages <- BISS %>%
  group_by(id, time, day, condition, group, group_factor) %>%
  summarise(value = mean(value, na.rm = TRUE),
            condition = first(condition),
            group = first(group),
            group_factor = first(group_factor)) %>%
  mutate(variable = 'Average') %>%
  ungroup()

# Append to the original data
BISS <- bind_rows(BISS, averages) %>%
  arrange(id, time, variable)

# change self-paced and prescribed to exercise
BISS <-BISS |> 
  mutate(condition = recode(condition, 'Self-Paced' = 'Exercise', 'Prescribed' = 'Exercise')) |> 
  mutate (task = case_when(condition == 'Exercise' & day == 'Day_A' ~ "Self-Paced", 
                           condition == 'Exercise' & day == 'Day_B' ~ "Prescribed", 
                           condition == 'Rest' & day == 'Day_A' ~ "Prescribed",
                           condition == 'Rest' & day == 'Day_C' ~ "Self-Paced") 
          ) |> 
  rename('study_visit' = day)

load('data/RedCap/redcap_raw_enrolled.RData')


bmi <- redcap_raw_enrolled |> 
  select(record_id, bmi_at_intake) |> 
  filter(!is.na(bmi_at_intake)) |> 
  filter(bmi_at_intake >0) |> 
  rename(id = 'record_id') |> 
  rename(bmi = 'bmi_at_intake')

bmi$id = as.character(bmi$id)

age <- redcap_raw_enrolled |> 
  mutate(age = as.numeric(as_date(dcf_date) - as_date(dcf_age))/364.25) |> 
  select(record_id, age) |> 
  filter(!is.na(age)) |>
  filter(age > 14)  |> 
  rename(id = 'record_id')

age$id = as.character(age$id)


BISS <- left_join(BISS, bmi) 
BISS <- left_join(BISS, age)

save(BISS, file = 'data/BISS/biss_data.RData')
