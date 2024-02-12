library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)
library(ggplot2)
library(patchwork)
library(lubridate)


load('data/RedCap/redcap_raw_enrolled.RData')
source('R/source/Functions.R')
pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', "MAXED_1011", "MAXED_1012")
redcap_raw_enrolled <- redcap_raw_enrolled |> 
  filter(!record_id %in% pilot_ids)
groups <- recalculate_groups(redcap_raw_enrolled)
groups$id = (groups$record_id)
groups <- select(groups, id, group, group_factor) |> 
  filter(!is.na(group))

affect <- redcap_raw_enrolled |> 
  select(record_id, starts_with ('rest_affect'), starts_with('ex_borg'), starts_with('ex_enth'), starts_with('ex_crum'), starts_with('ex_fatig'), starts_with('ex_calm'))

# Pull out the a,b, c datasets
affect_b <- affect %>%
  select(record_id, ends_with('_b')) %>%
  filter(rowSums(is.na(select(., -record_id))) != (ncol(.) - 1))
affect_a <- affect |> 
  select(record_id, !ends_with('_b') & !ends_with('_c'))%>%
  filter(rowSums(is.na(select(., -record_id))) != (ncol(.) - 1))
names(affect_a)[-1] <- paste0(names(affect_a)[-1], "_a")
affect_c <- affect |> 
  select(record_id, ends_with('_c')) %>%
  filter(rowSums(is.na(select(., -record_id))) != (ncol(.) - 1))
affect_ab <- full_join(affect_a, affect_b)
affect <- full_join(affect_ab,affect_c)

# Convert labelled columns to factors
affect_numeric <- affect %>%
  mutate(across(-record_id, ~as.numeric(.)))  # Convert all except 'record_id' to numeric

affect_long <- affect_numeric %>%
  pivot_longer(
    cols = -record_id, 
    names_pattern = "(.*_)([0-9]+)_(.)", 
    names_to = c("variable", "time", "day"),
    values_to = "value"
  ) %>%
  mutate(variable = str_remove(variable, "_$"))  # Removes the trailing underscore

affect <- affect_long %>%
  mutate(
    condition = str_extract(variable, "^(rest|ex)"),  # Extract prefix
    variable = str_replace(variable, "^(rest|ex)_?", "")  # Remove prefix and optional underscore
  ) |> 
  mutate(
    variable = str_replace(variable, "^(affect)_?", "")
  ) |> 
  mutate(variable = recode(variable, 'crum' = 'Crummy', 'enth' = 'Enthusiastic', 'fatig' = 'Fatigued', 'calm' = 'Calm', 'borg' = 'Percieved Exertion'),
         condition = recode(condition, 'ex' = 'Exercise', 'rest' = 'Rest')) |> 
  mutate(study_visit = recode(day, 'a' = 'Day_A', 'b' = 'Day_B', 'c' = 'Day_C')) |> 
  select(-c(day)) 


Affect_Rest_0 <- redcap_raw_enrolled |> 
  select(record_id, study_visit, starts_with('nvs_paas')) |> 
  select(record_id, study_visit, contains('pre') & !contains('factor')) |>  
  # convert  to numeric 
  mutate(across(-c(record_id,study_visit), as.numeric)) |>
  pivot_longer(
    cols = -c(record_id, study_visit),
    names_pattern = "(.*_)([0-9]+)(_.*)",
    names_to = c("construct", "var_number", "time"),
    values_to = "value"
  )  |> 
  filter(!is.na(value)) |> 
  select(-c(construct, time)) |> 
  mutate(variable = recode(var_number, '1' = 'Enthusiastic', '2' = 'Crummy', '3' = 'Fatigued', '4' = 'Calm')) |> 
  select(-c(var_number)) |> 
  mutate(condition = 'Rest', 
         time = '0')

Affect <- full_join(affect, Affect_Rest_0) |> 
  mutate(id = record_id) 

         
Affect<- left_join(Affect, groups) |> 
  select(-c(record_id)) |>
  mutate(task = case_when(study_visit == 'Day_A' & condition == "Exercise" ~ 'SelfPaced',
                         study_visit == 'Day_B' & condition == "Exercise" ~ 'Prescribed', 
                         study_visit == 'Day_C' & condition == "Rest" ~ 'SelfPaced', 
                         study_visit == 'Day_A' & condition == "Rest" ~ 'Prescribed')) |> 
  mutate(time = as.numeric(time))

bmi <- redcap_raw_enrolled |> 
  select(record_id, bmi_at_intake) |> 
  filter(!is.na(bmi_at_intake)) |> 
  filter(bmi_at_intake >0) |> 
  rename(id = 'record_id') |> 
  rename(bmi = 'bmi_at_intake')

age <- redcap_raw_enrolled |> 
  mutate(age = as.numeric(as_date(dcf_date) - as_date(dcf_age))/364.25) |> 
  select(record_id, age) |> 
  filter(!is.na(age)) |>
  filter(age > 14)  |> 
  rename(id = 'record_id')
  

Affect <- left_join(Affect, bmi) 
Affect <- left_join(Affect, age)

# Make Rest the reference condition
factor_order <- c('Rest', 'Exercise')
Affect$condition <- factor(Affect$condition, levels = factor_order)

# Make Control the reference group
factor_order <- c('Control', 'ED')
Affect$group_factor <- factor(Affect$group_factor, levels = factor_order)

save(Affect, file = 'data/Affect/MAXED_Affect.RData')
