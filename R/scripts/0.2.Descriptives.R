source("R/source/0.Packages.R")
library(tibble)
library(stringr)
library(purrr)
# Load Exercise response summary data
load("data/Exercise_Response/Exercise_Response_Summary_Data.RData")
# load redcap data (Exercise self-report; weight)
load("data/RedCap/MAXED_redcap_wide.2024-05-03.RData")
# load redcap raw enrolled to pull resting HR
load("data/RedCap/redcap_raw_enrolled.RData")
# load self-paced exercise parameter data
load("data/Exercise_Params/Exercise_Session_Data.RData")
# Create some variables for the exercise session data
sample_ids <- unique(exercise_response_data$id)
# load actigraph data
load("data/Actigraph/Actigraph_Mini.RData")

# Compute MaxHR, Distance, MaxWatts for each participant within each session
Watts_SP <- ex_data |> 
  filter(variable == 'Watts', 
         condition == 'Self-Paced') |> 
  group_by(id) |> 
  mutate(watts_max = max(value, na.rm = TRUE))  |> 
  select(id, watts_max) |> 
  distinct() |> 
  # remove negative values
  filter(watts_max > 0)

HR_SP <- ex_data |> 
  filter(variable == 'Heart Rate', 
         condition == 'Self-Paced') |> 
  group_by(id) |> 
  mutate(max_pct_hr = max(value/studya_max_hr *100)) |> 
  filter(time > 5) |> 
  mutate(avg_pct_hr = mean(value/studya_max_hr *100)) |> 
  select(id, avg_pct_hr, max_pct_hr) |> 
  distinct()

rest_hr <- redcap_raw_enrolled |> 
  select(record_id, starts_with('rest_hr')) |> 
  select (-starts_with('rest_hr_c')) |> 
  # remove rows where all rest_hr values are NA
  filter (!is.na (rest_hr_5)) |> 
  # create average resting heart rate which averages across all rest_hr values
  mutate(rest_hr = rowMeans(across(starts_with('rest_hr')), na.rm = TRUE)) |> 
  select(record_id, rest_hr) |> 
  # rename record id to id and make it a character
  rename(id = record_id) |>
  mutate(id = as.character(id))

# add restint heart rate to ex_data
karvonen_hr_SP <- ex_data |> 
  left_join(rest_hr, by = "id") |> 
  filter(variable == 'Heart Rate', 
         condition == 'Self-Paced') |> 
  mutate (karvonven_hr_pct = (as.numeric(value)- as.numeric(rest_hr))*100
          /(as.numeric(studya_max_hr) - as.numeric(rest_hr))) |> 
  group_by(id) |> 
  mutate(karvonen_max_intense = max(karvonven_hr_pct, na.rm = TRUE)) |> 
  distinct(id, karvonen_max_intense) |> 
  # remove rows wtih -Inf
  filter(karvonen_max_intense > 0)

Distance_SP <- ex_data |> 
  filter(variable == 'Distance', 
         condition == 'Self-Paced') |> 
  group_by(id) |> 
  mutate(distance = max(value)) |> 
  select(id, distance) |> 
  distinct()

# Combine the variables
SP_exercise_data <- Watts_SP |> 
  full_join(HR_SP, by = "id") |>
  full_join(Distance_SP, by = "id") |> 
  full_join(karvonen_hr_SP, by = "id")

# Selection of variables from SR data
SP_vars <- c( 'avg_pct_hr', 'max_pct_hr', 'karvonen_max_intense', 'distance')

# Selection of variables from MAXED RedCap data
ED_vars <- c('EDE_global_Intake', 'EDE_restraint_mean_Intake', 'EDE_eating_concern_mean_Intake', 'EDE_weight_concern_mean_Intake', 'EDE_shape_concern_mean_Intake')

Exercise_vars <- c('cet_total_Day_C', 'cet_avoid_raw_Day_C','cet_wtcontrol_raw_Day_C', 'cet_mood_raw_Day_C', 'cet_enjoy_raw_complete_Day_C','cet_rigid_raw_complete_Day_C',
                   'eds_sum_Day_C', 
                   'fomb_ANR_mean_Day_C', 
                   'fomb_APR_mean_Day_C', 
                   'fomb_SPR_mean_Day_C', 
                   'fomb_SNR_mean_Day_C', 
                   'muscularity_sum_Day_C')
Weight_vars <- c('bmi_at_intake_Intake', 
                 'wt_suppress_high_current_Intake')
Actigraph_vars <- c('MVPA_bouted', 'LPA_bouted')


# Filter out pilot ids
corr_vars <- MAXED_redcap_wide |>
  filter(id %in% sample_ids) |>
  select(id, all_of(ED_vars), all_of(Exercise_vars), all_of(Weight_vars)) |> 
  # recode negative weight suppression values to 0 
  mutate(wt_suppress_high_current_Intake = ifelse(wt_suppress_high_current_Intake < 0, 0, wt_suppress_high_current_Intake))

# select actigraph data
actigraph_data <- Actigraph|>
  # make id variable by selecting 'MAXED_####' *just the first part of the ID
  mutate(id = str_extract(ID, "^[^\\s]+")) |> 
  filter(id %in% sample_ids) |>
  # select only the id and the variables of interest
  select(id, all_of(Actigraph_vars))

# add actigraph data to corr_vars
corr_vars <- corr_vars |> 
  full_join(actigraph_data, by = "id")

# Combine with exercise response data and 
cor_vars2 <- exercise_response_data |>
  full_join(corr_vars, by = "id") |> 
  full_join(SP_exercise_data, by = "id")

# code averages of EDE global 
cor_vars2 <- cor_vars2 |> 
  mutate('EDE_global_Intake' = EDE_global_Intake/4)

#Create age variable based on birthdate and intake date 
age_vars <- redcap_raw_enrolled |>
  select(record_id, dcf_age,dcf_date,study_visit) |>
  filter(study_visit == 'Intake') |>
  rename(id = record_id) |>
  mutate('Age'= floor(as.numeric(difftime(dcf_date, dcf_age, units = "days") / 365.25))) |> 
  mutate(id = as.character(id)) |> 
  select(id, Age)


#Add age variable to cor_vars2
cor_vars2 <- cor_vars2 |> 
  left_join(age_vars, by = "id")

#select variables for preaim table

preaim_table <- cor_vars2 |> 
  select(id,group,LPA_bouted,MVPA_bouted, distance,karvonen_max_intense, max_pct_hr,avg_pct_hr, muscularity_sum_Day_C, fomb_ANR_mean_Day_C, fomb_APR_mean_Day_C, fomb_SPR_mean_Day_C, fomb_SNR_mean_Day_C,eds_sum_Day_C, cet_total_Day_C, cet_avoid_raw_Day_C, cet_wtcontrol_raw_Day_C, cet_mood_raw_Day_C, cet_enjoy_raw_complete_Day_C, cet_rigid_raw_complete_Day_C, EDE_global_Intake, EDE_restraint_mean_Intake,  EDE_eating_concern_mean_Intake, EDE_weight_concern_mean_Intake, EDE_shape_concern_mean_Intake, bmi_at_intake_Intake, wt_suppress_high_current_Intake)

preaim_table <- preaim_table |> 
  mutate(group = case_when(group == 1 ~ 'ED', 
                           group == 0 ~ 'HC')) 
#do t-test for each variable in preaim_table
conduct_t_test <- function(data, var_name) {
  t_test_result <- t.test(data[[var_name]] ~ data$group)
  return(data.frame(
    Variable = var_name,
    T_value = t_test_result$statistic,
    P_value = t_test_result$p.value
  ))
}

# Loop through each variable in the dataset
results <- lapply(names(preaim_table)[!(names(preaim_table) %in% c("id", "group"))], function(var_name) {
  # Call the conduct_t_test function for each variable
  t_test_result <- conduct_t_test(preaim_table, var_name)
  # Return the variable name and t-test result
  return(t_test_result)
})

# Combine the results into a single dataframe
results_df <- do.call(rbind, results)

# round t value and p value to two decimal places
results_df <- results_df |>
  mutate(T_value = sprintf("%.2f", T_value))
# renaming variables 
results_df$Variable[results_df$Variable== 'EDE_global_Intake'] <- 'EDE Global'
results_df$Variable[results_df$Variable== 'EDE_restraint_mean_Intake'] <- 'EDE Restraint Subscale'
results_df$Variable[results_df$Variable== 'EDE_eating_concern_mean_Intake'] <- 'EDE Eating Concern Subscale'
results_df$Variable[results_df$Variable== 'EDE_weight_concern_mean_Intake'] <- 'EDE Weight Concern Subscale'
results_df$Variable[results_df$Variable== 'EDE_shape_concern_mean_Intake'] <- 'EDE Shape Concern Subscale'
results_df$Variable[results_df$Variable== 'cet_total_Day_C'] <- 'CET'
results_df$Variable[results_df$Variable== 'cet_avoid_raw_Day_C'] <- 'CET Avoidance'
results_df$Variable[results_df$Variable== 'cet_wtcontrol_raw_Day_C'] <- 'CET Weight Control'
results_df$Variable[results_df$Variable== 'cet_mood_raw_Day_C'] <- 'CET Mood'
results_df$Variable[results_df$Variable== 'cet_enjoy_raw_complete_Day_C'] <- 'CET Enjoyment'
results_df$Variable[results_df$Variable== 'cet_rigid_raw_complete_Day_C'] <- 'CET Rigid'
results_df$Variable[results_df$Variable== 'eds_sum_Day_C'] <- 'EDS'
results_df$Variable[results_df$Variable== 'fomb_ANR_mean_Day_C'] <- 'FAMB ANR'
results_df$Variable[results_df$Variable== 'fomb_APR_mean_Day_C'] <- 'FAMB APR'
results_df$Variable[results_df$Variable== 'fomb_SPR_mean_Day_C'] <- 'FAMB SPR'
results_df$Variable[results_df$Variable== 'fomb_SNR_mean_Day_C'] <- 'FAMB SNR'
results_df$Variable[results_df$Variable== 'muscularity_sum_Day_C'] <- 'DFM'
results_df$Variable[results_df$Variable== 'bmi_at_intake_Intake'] <- 'BMI'
results_df$Variable[results_df$Variable== 'wt_suppress_high_current_Intake'] <- 'Weight Suppression'
results_df$Variable[results_df$Variable== 'MVPA_bouted'] <- 'MVPA'
results_df$Variable[results_df$Variable== 'LPA_bouted'] <- 'LPA'
results_df$Variable[results_df$Variable== 'avg_pct_hr'] <- 'Average % HR'
results_df$Variable[results_df$Variable== 'max_pct_hr'] <- 'Max % HR'
results_df$Variable[results_df$Variable== 'karvonen_max_intense'] <- 'Karvonen Max Intensity'
results_df$Variable[results_df$Variable== 'distance'] <- 'Distance'


# calculate cohen's d for each variable
cohen_d <- function(data, var_name) {
  cohens_d_result <- effsize::cohen.d(data[[var_name]] ~ data$group)
  return(data.frame(
    Variable = var_name,
    Cohen_d = cohens_d_result$estimate
  ))
}

# Loop through each variable in the dataset
results_cohen_d <- lapply(names(preaim_table)[!(names(preaim_table) %in% c("id", "group"))], function(var_name) {
  # Call the cohen_d function for each variable
  cohen_d <- cohen_d(preaim_table, var_name)
  # Return the variable name and cohen_d result
  return(cohen_d)
})

results_d <- do.call (rbind, results_cohen_d)
#renaming variables
results_d$Variable[results_d$Variable== 'EDE_global_Intake'] <- 'EDE Global'
results_d$Variable[results_d$Variable== 'EDE_restraint_mean_Intake'] <- 'EDE Restraint Subscale'
results_d$Variable[results_d$Variable== 'EDE_eating_concern_mean_Intake'] <- 'EDE Eating Concern Subscale'
results_d$Variable[results_d$Variable== 'EDE_weight_concern_mean_Intake'] <- 'EDE Weight Concern Subscale'
results_d$Variable[results_d$Variable== 'EDE_shape_concern_mean_Intake'] <- 'EDE Shape Concern Subscale'
results_d$Variable[results_d$Variable== 'cet_total_Day_C'] <- 'CET'
results_d$Variable[results_d$Variable== 'cet_avoid_raw_Day_C'] <- 'CET Avoidance'
results_d$Variable[results_d$Variable== 'cet_wtcontrol_raw_Day_C'] <- 'CET Weight Control'
results_d$Variable[results_d$Variable== 'cet_mood_raw_Day_C'] <- 'CET Mood'
results_d$Variable[results_d$Variable== 'cet_enjoy_raw_complete_Day_C'] <- 'CET Enjoyment'
results_d$Variable[results_d$Variable== 'cet_rigid_raw_complete_Day_C'] <- 'CET Rigid'
results_d$Variable[results_d$Variable== 'eds_sum_Day_C'] <- 'EDS'
results_d$Variable[results_d$Variable== 'fomb_ANR_mean_Day_C'] <- 'FAMB ANR'
results_d$Variable[results_d$Variable== 'fomb_APR_mean_Day_C'] <- 'FAMB APR'
results_d$Variable[results_d$Variable== 'fomb_SPR_mean_Day_C'] <- 'FAMB SPR'
results_d$Variable[results_d$Variable== 'fomb_SNR_mean_Day_C'] <- 'FAMB SNR'
results_d$Variable[results_d$Variable== 'muscularity_sum_Day_C'] <- 'DFM'
results_d$Variable[results_d$Variable== 'bmi_at_intake_Intake'] <- 'BMI'
results_d$Variable[results_d$Variable== 'wt_suppress_high_current_Intake'] <- 'Weight Suppression'
results_d$Variable[results_d$Variable== 'MVPA_bouted'] <- 'MVPA'
results_d$Variable[results_d$Variable== 'LPA_bouted'] <- 'LPA'
results_d$Variable[results_d$Variable== 'avg_pct_hr'] <- 'Average % HR'
results_d$Variable[results_d$Variable== 'max_pct_hr'] <- 'Max % HR'
results_d$Variable[results_d$Variable== 'karvonen_max_intense'] <- 'Karvonen Max Intensity'
results_d$Variable[results_d$Variable== 'distance'] <- 'Distance'

#calculate averages and standard deviations for each group
preaim_table <- preaim_table |>
  select(-id) |>
  drop_na() |>
  group_by(group) |>
  summarise(across(everything(), list(avg = mean, sd = sd)))

preaim_table <- preaim_table |>
  rename( 'EDE Global (Mean)' = 'EDE_global_Intake_avg',
          'EDE Global (SD)' = 'EDE_global_Intake_sd',
          'EDE Restraint Subscale (Mean)' = 'EDE_restraint_mean_Intake_avg',
          'EDE Restraint Subscale (SD)' = 'EDE_restraint_mean_Intake_sd',
          'EDE Eating Concern Subscale (Mean)' = 'EDE_eating_concern_mean_Intake_avg',
          'EDE Eating Concern Subscale (SD)' = 'EDE_eating_concern_mean_Intake_sd',
          'EDE Weight Concern Subscale (Mean)' = 'EDE_weight_concern_mean_Intake_avg',
          'EDE Weight Concern Subscale (SD)' = 'EDE_weight_concern_mean_Intake_sd',
          'EDE Shape Concern Subscale (Mean)' = 'EDE_shape_concern_mean_Intake_avg',
          'EDE Shape Concern Subscale (SD)' = 'EDE_shape_concern_mean_Intake_sd',
          'CET (Mean)' = 'cet_total_Day_C_avg',
          'CET (SD)' = 'cet_total_Day_C_sd',
          'CET Avoidance (Mean)' = 'cet_avoid_raw_Day_C_avg',
          'CET Avoidance (SD)' = 'cet_avoid_raw_Day_C_sd',
          'CET Weight Control (Mean)' = 'cet_wtcontrol_raw_Day_C_avg',
          'CET Weight Control (SD)' = 'cet_wtcontrol_raw_Day_C_sd',
          'CET Mood (Mean)' = 'cet_mood_raw_Day_C_avg',
          'CET Mood (SD)' = 'cet_mood_raw_Day_C_sd',
          'CET Enjoyment (Mean)' = 'cet_enjoy_raw_complete_Day_C_avg',
          'CET Enjoyment (SD)' = 'cet_enjoy_raw_complete_Day_C_sd',
          'CET Rigid (Mean)' = 'cet_rigid_raw_complete_Day_C_avg',
          'CET Rigid (SD)' = 'cet_rigid_raw_complete_Day_C_sd',
          'EDS (Mean)' = 'eds_sum_Day_C_avg',
          'EDS (SD)' = 'eds_sum_Day_C_sd',
          'FAMB ANR (Mean)' = 'fomb_ANR_mean_Day_C_avg',
          'FAMB ANR (SD)' = 'fomb_ANR_mean_Day_C_sd',
          'FAMB APR (Mean)' = 'fomb_APR_mean_Day_C_avg',
          'FAMB APR (SD)' = 'fomb_APR_mean_Day_C_sd',
          'FAMB SPR (Mean)' = 'fomb_SPR_mean_Day_C_avg',
          'FAMB SPR (SD)' = 'fomb_SPR_mean_Day_C_sd',
          'FAMB SNR (Mean)' = 'fomb_SNR_mean_Day_C_avg',
          'FAMB SNR (SD)' = 'fomb_SNR_mean_Day_C_sd',
          'DFM (Mean)' = 'muscularity_sum_Day_C_avg',
          'DFM (SD)' = 'muscularity_sum_Day_C_sd',
          'BMI (Mean)' = 'bmi_at_intake_Intake_avg',
          'BMI (SD)' = 'bmi_at_intake_Intake_sd',
          'Weight Suppression (Mean)' = 'wt_suppress_high_current_Intake_avg',
          'Weight Suppression (SD)' = 'wt_suppress_high_current_Intake_sd',
          'MVPA (Mean)' = 'MVPA_bouted_avg',
          'MVPA (SD)' = 'MVPA_bouted_sd',
          'LPA (Mean)' = 'LPA_bouted_avg',
          'LPA (SD)' = 'LPA_bouted_sd',
          'Average % HR (Mean)' = 'avg_pct_hr_avg',
          'Average % HR (SD)' = 'avg_pct_hr_sd',
          'Max % HR (Mean)' = 'max_pct_hr_avg',
          'Max % HR (SD)' = 'max_pct_hr_sd',
          'Karvonen Max Intensity (Mean)' = 'karvonen_max_intense_avg',
          'Karvonen Max Intensity (SD)' = 'karvonen_max_intense_sd',
          'Distance (Mean)' = 'distance_avg',
          'Distance (SD)' = 'distance_sd')

#pivot table wide to long with group values as columns and the columns as rows
preaim_table <- preaim_table %>%
  pivot_longer(cols = -group, names_to = "variable", 
               values_to = "value") %>%
  pivot_wider(names_from = group, values_from = value) |> 
  # round 'ED' and 'HC' columns to 2 decimal points
  mutate(across(starts_with("ED"), ~round(., 2)),
         across(starts_with("HC"), ~round(., 2))) 

preaim_table <- preaim_table %>%
  mutate(Measure = case_when(str_detect(variable, "Mean") ~ "Mean",
                             str_detect(variable, "SD") ~ "SD")) |> 
  # get rid of (Mean) and (SD) in variable name
  mutate(variable = str_remove(variable, "\\(Mean\\)"),
         variable = str_remove(variable, "\\(SD\\)")) |> 
  pivot_wider(
    names_from = Measure,
    values_from = c(HC, ED),
    names_glue = "{.value}_{Measure}"
  ) %>%
  select(variable,starts_with("ED"), starts_with("HC")) |> 
  # use sprintf to make double columns 2 decimal points
  mutate(across(starts_with("ED"), ~sprintf("%.2f", .)),
         across(starts_with("HC"), ~sprintf("%.2f", .))) |> 
  # combine Mean and SD Columns to Mean (SD) format for each group
  unite("ED", ED_Mean, ED_SD, sep = " (", remove = FALSE) %>%
  mutate(ED = str_replace(ED, "\\s*\\(\\s*$", "")) %>%
  mutate(ED = paste0(ED, ")")) |> 
  unite("HC", HC_Mean, HC_SD, sep = " (", remove = FALSE) %>%
  mutate(HC = str_replace(HC, "\\s*\\(\\s*$", "")) %>%
  mutate(HC = paste0(HC, ")")) |> 
  # remove _Mean and _SD columns
  select(-ends_with("Mean"), -ends_with("SD")) |> 
  # rename columns
  rename( "ED Mean (SD)" = ED,
          "HC Mean (SD)" = HC) |>
  mutate(variable = str_trim(variable))


preaim_table <- preaim_table |> 
  rename('Variable' = 'variable') 

# join t test table and other preaim_table
preaim_table <-  preaim_table |> 
  full_join(results_df, by = "Variable") 

#join cohens d to preaim_tabe
preaim_table <- preaim_table |> 
  full_join(results_d, by= "Variable")
preaim_table <- preaim_table |> 
  mutate(Cohen_d = round(Cohen_d, 2)) |> 
  rename ('Cohen\'s d' = Cohen_d) |> 
  rename (p = P_value) |>
  #italicize t value
  rename('t value' = T_value)  




#Create age variable based on birthdate and intake date 
age_vars_2 <- redcap_raw_enrolled |>
  select(record_id, group, dcf_age,dcf_date,study_visit) |>
  filter(study_visit == 'Intake') |>
  rename(id = record_id) |>
  mutate('Age'= floor(as.numeric(difftime(dcf_date, dcf_age, units = "days") / 365.25))) |> 
  mutate(id = as.character(id)) |> 
  select(id, Age, group) |> 
  # recode group to be ED (1) and HC (0)
  mutate(group = case_when(group == 1 ~ 'ED', 
                           group == 0 ~ 'HC'))

# Loop through each variable in the dataset
  # Call the conduct_t_test function for each variable
  age_t <- conduct_t_test(age_vars_2, "Age")
  age_d <- cohen_d(age_vars_2, "Age")
  
# join age_t and age_d
  age_t <- age_t |> full_join(age_d, by = "Variable")

  
  age_mean_sd <- age_vars_2 |>
    select(group, Age) |>
    drop_na() |>
    filter(is.finite(Age)) |>
    mutate(Age = as.numeric(Age)) |>
    group_by(group) |>
    summarise(mean = mean(Age), 
              sd = sd(Age))
  # pull mean and SD together and make it M (SD)
  age_mean_sd <- age_mean_sd |>
    mutate(mean_sd = paste0(round(mean, 2), " (", round(sd, 2), ")")) |>
    select(-mean, -sd) |>
    mutate(Variable = "Age")  
    # pivot to wide format
    
    age_mean_sd_wide <- age_mean_sd |>
    select(group, mean_sd)  |> 
    pivot_wider(names_from = group, values_from = `mean_sd`, names_prefix = "", names_sep = " ") |>
    rename(`ED Mean (SD)` = `ED`, `HC Mean (SD)` = `HC`) |> 
    mutate(Variable = "Age")
  
    # join age_mean_sd_wide with age_t
    age_t <- age_t |>
    full_join(age_mean_sd_wide, by = "Variable") |> 
      mutate(Cohen_d = round(Cohen_d, 2)) |> 
      rename ('Cohen\'s d' = Cohen_d) |> 
      rename (p = P_value) |>
      #italicize t value
      mutate(T_value = sprintf("%.2f", T_value)) |> 
      rename('t value' = T_value) |> 
      select("Variable", "ED Mean (SD)", "HC Mean (SD)", "t value", "p", "Cohen's d")

# bind age_t to preaim_table
  preaim_table <- preaim_table |>
    bind_rows(age_t) |> 
    # rename ED Mean (SD) to ED and HC Mean (SD) to HC
    rename('ED' = 'ED Mean (SD)', 'HC' = 'HC Mean (SD)') 
 
  # Summarize the data
  demo_data <- redcap_raw_enrolled |>
    filter(record_id %in% sample_ids) |>
    select(record_id, study_visit, group, dcf_research_sexual_identity, dcf_research_gender, 
           dcf_race___1, dcf_race___2, dcf_race___3, dcf_race___4, dcf_race___5, 
           dcf_race___6, dcf_race___8, dcf_race___9, dcf_race___10, dcf_race___11) |>
    filter(study_visit == 'Intake') |>
    mutate(group = case_when(group == 1 ~ 'ED', group == 0 ~ 'HC')) |> 
    group_by(group) |>
    summarise(
      Asexual = sum(dcf_research_sexual_identity == 4, na.rm = TRUE),
      Bisexual_Pansexual = sum(dcf_research_sexual_identity == 2, na.rm = TRUE),
      Lesbian_Gay = sum(dcf_research_sexual_identity == 1, na.rm = TRUE),
      Heterosexual = sum(dcf_research_sexual_identity == 3, na.rm = TRUE),
      Sex_Orientation_NR = sum(is.na(dcf_research_sexual_identity)), 
      Woman = sum(dcf_research_gender == 1, na.rm = TRUE),
      Nonbinary = sum(dcf_research_gender == 3, na.rm = TRUE),
      Unsure = sum(dcf_research_gender == 4, na.rm = TRUE),
      Gender_NR = sum(is.na(dcf_research_gender)),
      White = sum(dcf_race___1 == 1, na.rm = TRUE),
      Black_African_American = sum(dcf_race___2 == 1, na.rm = TRUE),
      Asian = sum(dcf_race___3 == 1, na.rm = TRUE),
      Hispanic = sum(dcf_race___5 == 1, na.rm = TRUE),
      Middle_Eastern = sum(dcf_race___9 == 1, na.rm = TRUE),
      South_Asian = sum(dcf_race___10 == 1, na.rm = TRUE),
      Southeast_Asian = sum(dcf_race___11 == 1, na.rm = TRUE)
    ) |>
    pivot_longer(
      cols = -group,
      names_to = "Variable",
      values_to = "N"
    ) |>
    pivot_wider(
      names_from = group,
      values_from = N,
      names_prefix = "",
      names_sep = " "
    ) |> 
    # make ED and HC character variables
    mutate(ED = as.character(as.integer(ED)), HC = as.character(as.integer(HC))) 
  
  
  # join demo_data with preaim_table by Variable, ED, and HC
  demo_vars <- demo_data |>
    full_join(preaim_table, by = c("Variable", "ED", "HC"))
  
# recode Variable to take out underscores and replace with spaces
  demo_vars$Variable <- gsub("_", " ", demo_vars$Variable)
  demo_vars$Variable <- gsub("Lesbian Gay", "Lesbian/Gay", demo_vars$Variable)
  demo_vars$Variable <- gsub("Bisexual Pansexual", "Bisexual/Pansexual", demo_vars$Variable)
  demo_vars$Variable <- gsub("Black African American", "Black/African American", demo_vars$Variable)
  demo_vars$Variable <- gsub("CET Enjoyment", "CET Lack of Enjoyment", demo_vars$Variable)
  demo_vars$Variable <- gsub("EDE Restraint Subscale", "EDE Restraint", demo_vars$Variable)
  demo_vars$Variable <- gsub("EDE Eating Concern Subscale", "EDE Eating Concern", demo_vars$Variable)
  demo_vars$Variable <- gsub("EDE Shape Concern Subscale", "EDE Shape Concern", demo_vars$Variable)
  demo_vars$Variable <- gsub("EDE Weight Concern Subscale", "EDE Weight Concern", demo_vars$Variable)
  demo_vars$Variable <- gsub("Sex Orientation NR", "Sex Orientation Not Reported", demo_vars$Variable)
  demo_vars$Variable <- gsub("Gender NR","Gender Not Reported",  demo_vars$Variable)
  
  
demo_vars$Variable <- factor(demo_vars$Variable, levels = c("Asexual", "Bisexual/Pansexual", "Lesbian/Gay", "Heterosexual", "Sex Orientation Not Reported", "Woman", "Nonbinary", "Unsure", "Gender Not Reported", "White", "Black/African American", "Asian", "Hispanic", "Middle Eastern", "South Asian", "Southeast Asian", "Age", "BMI", "Weight Suppression", "EDE Global", "EDE Restraint", "EDE Eating Concern", "EDE Shape Concern", "EDE Weight Concern", "LPA", "MVPA", "Distance", "Karvonen Max Intensity", "Max % HR", "Average % HR", "DFM", "FAMB ANR", "FAMB APR", "FAMB SPR", "FAMB SNR", "EDS", "CET", "CET Avoidance", "CET Weight Control", "CET Mood", "CET Lack of Enjoyment", "CET Rigid"))

# sort descending by Variable
  demo_vars_1 <- demo_vars |>
    arrange(Variable)
  
  
  save(demo_vars_1, file = "results/demo_vars.RData")
  
