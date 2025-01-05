
load('data/Exercise_Response/Exercise_Response_Summary_Data.RData')

# select group == 1 for separate dataset
ex_response_ED <- exercise_response_data |>
  filter(group == 1) |> 
  # make all columns (except for ID) numeric
  mutate(across(-id, as.numeric)) |> 
  select(-id, -group, -group_factor) |> 
  # remove variable with name _Pre 
  select(-contains("_Pre"))  |> 
  # remove variable with name _Post
  select(-contains("_Post"))  |>
  # remove variable with name ANCOVA
  select(-contains("ANCOVA"))  

# select group == 1 for separate dataset
ex_response_control <- exercise_response_data |>
  filter(group == 0) |> 
  # make all columns (except for ID) numeric
  mutate(across(-id, as.numeric)) |> 
  select(-id, -group, -group_factor) |> 
  # remove variable with name _Pre 
  select(-contains("_Pre"))  |> 
  # remove variable with name _Post
  select(-contains("_Post"))  |>
  # remove variable with name ANCOVA
  select(-contains("ANCOVA"))  


# Function to apply cor.test to each pair and extract estimates and CIs
get_cor_ci <- function(df) {
  combn(names(df), 2, function(x) {
    test <- cor.test(df[[x[1]]], df[[x[2]]], use = "pairwise.complete.obs")
    data.frame(
      var1 = x[1],
      var2 = x[2],
      estimate = test$estimate,
      ci_lower = test$conf.int[1],
      ci_upper = test$conf.int[2],
      p_value = test$p.value
    )
  }, simplify = FALSE) %>%
    bind_rows()
}

# Apply the function to your selected dataframe
cor_ci_results_ed <- get_cor_ci(ex_response_ED)

# save results
save(cor_ci_results_ed, file = "results/Corr_CI_Results_ed.RData")

# Apply the function to control data
cor_ci_results_control <- get_cor_ci(ex_response_control)

# save results
save(cor_ci_results_control, file = "results/Corr_CI_Results_control.RData")

# combine results
# add group column
cor_ci_results_ed$group <- "ED"
cor_ci_results_control$group <- "Control"

# combine results
cor_ci_results <- rbind(cor_ci_results_ed, cor_ci_results_control)

# select key variables
key_biomarker_variables <- c('Leptin_ResidChange', 
                             'BDNF_ResidChange', 
                             'Cortisol_ResidChange', 
                             'AEA_ResidChange',
                             'AG_ResidChange')
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

key_variables <- c(key_biomarker_variables, key_BISS_variables, key_affect_variables)

cor_ci_results <- cor_ci_results |>
  filter(var1 %in% key_variables & var2 %in% key_variables)

save(cor_ci_results, file = "results/Corr_CI_Results.RData")

