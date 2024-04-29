library(haven)
library(embarktools)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)
# Load Exercise response summary data
load("data/Exercise_Response_Summary_Data.RData")
# load redcap data (Exercise self-report; weight)
load("data/RedCap/MAXED_redcap_wide.2024-04-21.RData")
# load self-paced exercise parameter data
load("data/Exercise_Params/Exercise_Session_Data.RData")
# Create some variables for the exercise session data
sample_ids <- unique(exercise_response_data$id)
# load actigraph data
load("data/Actigraph/Actigraph_Mini.RData")

# Compute MaxHR, Distance, MaxWatts for each participant within each session
Watts_SP <- ex_data_1 |> 
  filter(variable == 'Watts', 
         condition == 'Self-Paced') |> 
  group_by(id) |> 
  mutate(watts_max = max(value, na.rm = TRUE))  |> 
  select(id, watts_max) |> 
  distinct() |> 
  # remove negative values
  filter(watts_max > 0)

HR_SP <- ex_data_1 |> 
  filter(variable == 'Heart Rate', 
         condition == 'Self-Paced') |> 
  group_by(id) |> 
  mutate(max_pct_hr = max(value/studya_max_hr *100)) |> 
  filter(time > 5) |> 
  mutate(avg_pct_hr = mean(value/studya_max_hr *100)) |> 
  select(id, avg_pct_hr, max_pct_hr) |> 
  distinct()

Distance_SP <- ex_data_1 |> 
  filter(variable == 'Distance', 
         condition == 'Self-Paced') |> 
  group_by(id) |> 
  mutate(distance = max(value)) |> 
  select(id, distance) |> 
  distinct()

# Combine the variables
SP_exercise_data <- Watts_SP |> 
  full_join(HR_SP, by = "id") |>
  full_join(Distance_SP, by = "id")

# Selection of variables from SR data
SP_vars <- c('avg_pct_hr', 'max_pct_hr', 'distance')

# Selection of variables from MAXED RedCap data

Exercise_vars <- c('cet_total_Day_C', 
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
  select(id, all_of(Exercise_vars), all_of(Weight_vars)) |> 
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


key_biomarker_variables <- c('Leptin_ResidChange', 
                             'BDNF_ResidChange', 
                             'Cortisol_ResidChange')

key_BISS_variables <- c('Average_P', 
                        'Average_SP',
                        'Weight_P', 
                        'Weight_SP',
                        'Shape_P',
                        'Shape_SP')

key_affect_variables <- c('Calm_P', 
                          'Calm_SP',
                          'Enthusiastic_P',
                          'Enthusiastic_SP',
                          'Fatigued_P',
                          'Fatigued_SP',
                          'Crummy_P',
                          'Crummy_SP',
                          'Percieved_Exertion_SP',
                          'Percieved_Exertion_P'
)

# filter data for ED group
cor_vars2_ED <- cor_vars2 |>
  filter(group == "1") |> 
  # make all columns (except for ID) numeric
  mutate(across(-id, as.numeric))
  
# make a correlation matrix, excluding ID and removing pairwise NA values -- also add standard deviation
exercise_response_cors_ED <- cor(cor_vars2_ED |>
                             select(-id), use = "pairwise.complete.obs")

# pivot correlation matrix to long Format
ED_cors_long <- exercise_response_cors_ED |> 
  as.data.frame() |>
  rownames_to_column(var = "variable1") |>
  pivot_longer(cols = -variable1, names_to = "variable2", values_to = "correlation") |> 
  filter(correlation != 1)

# only include if var1 in key_affect, key_biomarker, or key_BISS and var2 in Exercise_vars or Weight_vars
ED_cors_long <- ED_cors_long |>
  filter((variable1 %in% key_affect_variables | variable1 %in% key_biomarker_variables | variable1 %in% key_BISS_variables ) & 
         (variable2 %in% Exercise_vars | variable2 %in% Weight_vars | variable2 %in% SP_vars | variable2 %in% Actigraph_vars))
# remove _Day_C from variable names
ED_cors_long$variable2 <- gsub("_Day_C", "", ED_cors_long$variable2)
# remove _Intake from variable names
ED_cors_long$variable2 <- gsub("_Intake", "", ED_cors_long$variable2)
# remove _high_current_obj from variable names
ED_cors_long$variable2 <- gsub("_high_current", "", ED_cors_long$variable2)


# filter data for HC group
cor_vars2_HC <- cor_vars2 |>
  filter(group == "0") |> 
  # make all columns (except for ID) numeric
  mutate(across(-id, as.numeric))

# make a correlation matrix, excluding ID and removing pairwise NA values
exercise_response_cors_HC <- cor(cor_vars2_HC |>
                             select(-id), use = "pairwise.complete.obs")

# pivot correlation matrix to long Format
HC_cors_long <- exercise_response_cors_HC |> 
  as.data.frame() |>
  rownames_to_column(var = "variable1") |>
  pivot_longer(cols = -variable1, names_to = "variable2", values_to = "correlation") |> 
  filter(correlation != 1)

# only include if var1 in key_affect, key_biomarker, or key_BISS and var2 in Exercise_vars or Weight_vars
HC_cors_long <- HC_cors_long |>
  filter((variable1 %in% key_affect_variables | variable1 %in% key_biomarker_variables | variable1 %in% key_BISS_variables) & 
         (variable2 %in% Exercise_vars | variable2 %in% Weight_vars | variable2 %in% SP_vars | variable2 %in% Actigraph_vars))
# remove _Day_C from variable names
HC_cors_long$variable2 <- gsub("_Day_C", "", HC_cors_long$variable2)
# remove _Intake from variable names
HC_cors_long$variable2 <- gsub("_Intake", "", HC_cors_long$variable2)
# remove _high_current_obj from variable names
HC_cors_long$variable2 <- gsub("_high_current", "", HC_cors_long$variable2)
#add group column
ED_cors_long$group <- "ED"
HC_cors_long$group <- "Control"

# bind the two dataframes together
cors_long <- rbind(ED_cors_long, HC_cors_long)

save(cors_long, file = "data/cors_long.RData")
