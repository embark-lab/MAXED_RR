library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(ggplot2)
library(embarktools)
library(car)

load('data/Assays/Assay_results.RData')

Assay_results.1 <- Assay_results %>%
  select(-c('Estradiol')) |> 
  unique() |> 
  # First, pivot wider to make separate columns for Pre and Post for each variable
  pivot_wider(names_from = Time, values_from = c(BDNF, Leptin, Cortisol)) |> 
  # Calculate change scores
  mutate(BDNF_RawChange = BDNF_Post - BDNF_Pre,
         Leptin_RawChange = Leptin_Post - Leptin_Pre, 
         Cortisol_RawChange = Cortisol_Post - Cortisol_Pre) 

conditions <- c('Exercise', 'Rest')
biomarkers <- c('BDNF', 'Leptin', 'Cortisol')
models <- list()

for (condition in conditions) {
  models[[condition]] <- list() # Initialize a sublist for each condition
  
  for (biomarker in biomarkers) {
    # Correct approach to create formulas
    ancova_formula <- reformulate(c(paste0(biomarker,"_Pre"), "group_factor"), response = paste0(biomarker,"_Post"))
    residual_change_formula <- reformulate(paste0(biomarker,"_Pre"), response = paste0(biomarker,"_Post"))
    
    filtered_data <- filter(Assay_results.1, Condition == condition)
    
    # Fit the models
    ancova_model <- lm(ancova_formula, data = filtered_data)
    residual_change_model <- lm(residual_change_formula, data = filtered_data)
    
    # Store models in the list
    models[[condition]][[biomarker]] <- list(ancova = ancova_model, residual_change = residual_change_model)
  }
}


# Assuming 'models' is structured as models[condition][biomarker][model_type]
# and 'conditions' and 'biomarkers' are your vectors of conditions and biomarkers

# Step 1: Iterate over each level of nesting to calculate predictions
predictions_list <- map(conditions, ~{
  condition <- .x
  condition_models <- models[[condition]]
  
  map(biomarkers, ~{
    biomarker <- .x
    biomarker_models <- condition_models[[biomarker]]
    
    filtered_data <- filter(Assay_results.1, Condition == condition)
    
    # Calculate predictions for both models within the biomarker and condition
    ancova_predictions <- predict(biomarker_models[['ancova']], newdata = filtered_data)
    residual_change_predictions <- predict(biomarker_models[['residual_change']], newdata = filtered_data)
    
    # Return a data frame with the necessary identifying information and predictions
    data.frame(ID = filtered_data$ID, 
               Condition = condition, 
               Biomarker = biomarker,
               AncovaPred = ancova_predictions, 
               PrePred = residual_change_predictions)
  }) |> bind_rows()
}) |> bind_rows()

# Pivot biomarker predictions to wide format
predictions_list <- predictions_list |> 
  pivot_wider(values_from = c(AncovaPred, PrePred), names_from = c(Biomarker))
# put biomarker names first to match the original dataset 
predictions_list <- predictions_list |> 
  rename(BDNF_AncovaPred = AncovaPred_BDNF, 
         Leptin_AncovaPred = AncovaPred_Leptin,
         BDNF_PrePred = PrePred_BDNF,
         Leptin_PrePred = PrePred_Leptin,
         Cortisol_AncovaPred = AncovaPred_Cortisol,
         Cortisol_PrePred = PrePred_Cortisol)

# Step 2: Merge this predictions dataframe back into your original dataset

Assay_results.2 <- left_join(Assay_results.1, predictions_list, by = c("ID", "Condition"))
  # Add the predicted values and residuals to the original dataset
  Assay_results.2$BDNF_ANCOVAresid <- with(Assay_results.2, BDNF_Post - BDNF_AncovaPred)
  Assay_results.2$Leptin_ANCOVAresid <- with(Assay_results.2, Leptin_Post - Leptin_AncovaPred)
  Assay_results.2$BDNF_ResidChange <- with(Assay_results.2, BDNF_Post - BDNF_PrePred)
  Assay_results.2$Leptin_ResidChange <- with(Assay_results.2, Leptin_Post - Leptin_PrePred)
  Assay_results.2$Cortisol_ANCOVAresid <- with(Assay_results.2, Cortisol_Post - Cortisol_AncovaPred)
  Assay_results.2$Cortisol_ResidChange <- with(Assay_results.2, Cortisol_Post - Cortisol_PrePred)
  
  Biomarker_residuals <- Assay_results.2 
  
  save (Biomarker_residuals, file = 'data/Assays/Biomarker_residuals.RData')
  
  # Calculate whether group predicts residual change scores
  ResidChange.BDNF_model1_ex <- lm(BDNF_ResidChange ~ group_factor, data = Assay_results.2 |> filter(Condition == 'Exercise'))
  ResidChange.Leptin_model1_ex <- lm(Leptin_ResidChange ~ group_factor, data = Assay_results.2 |> filter(Condition == 'Exercise'))
  ResidChange.BDNF_model1_rest <- lm(BDNF_ResidChange ~ group_factor, data = Assay_results.2 |> filter(Condition == 'Rest'))
  ResidChange.Leptin_model1_rest <- lm(Leptin_ResidChange ~ group_factor, data = Assay_results.2 |> filter(Condition == 'Rest'))
  ResidChange.Cortisol_model1_ex <- lm(Cortisol_ResidChange ~ group_factor, data = Assay_results.2 |> filter(Condition == 'Exercise'))
  ResidChange.Cortisol_model1_rest <- lm(Cortisol_ResidChange ~ group_factor, data = Assay_results.2 |> filter(Condition == 'Rest'))


Assay_results.3 <- Assay_results.2 |> 
  pivot_longer(cols = c(starts_with("BDNF"), starts_with("Leptin"), starts_with("Cortisol")),
             names_to = "Measure_Type", values_to = "Value") |> 
  separate(Measure_Type, into = c("Assay", "Outcome"), sep = "_") |> 
  filter(!is.na(Value))

Assays <- unique(Assay_results.3$Assay)
Conditions <- c('Exercise', 'Rest')
Groups <- c('Control', 'ED')
Outcomes <- c('Pre', 'Post', 'RawChange', 'ResidChange')

# Initialize a list to store combined results
combined_results <- list()

for (condition in Conditions) {
  for (assay in Assays) {
    for (outcome in Outcomes) {
    # Subset the data for the current condition and group
    current_df <- Assay_results.3 |> 
      filter(Condition == condition, Assay == assay, Outcome == outcome)
    # calculate t-test for each assay, comparing independent samples of control vs. ED
    t <- t.test(Value ~ group_factor, data = current_df)
    tidy_t <- broom::tidy(t)
    # add to combined results
    combined_results[[paste(assay, condition, outcome, sep = "_")]] <- tidy_t
    }
  }
}

# Combine all results into a single data frame
combined_results_df <- do.call(rbind, combined_results)
# save the results 

# Make rownames a column
combined_results_df <- combined_results_df |> rownames_to_column(var = "Params")
# Split the Params column into separate columns
biomarker_ttest_results <- combined_results_df |> separate(Params, into = c("Assay", "Condition", "Outcome"), sep = "_")
 
# rename the columns
biomarker_ttest_results <- biomarker_ttest_results |> 
  rename(Estimate = estimate, LowerCI = conf.low, UpperCI = conf.high,
         Control_Mean = estimate1, 
         ED_Mean = estimate2)

save(biomarker_ttest_results, file = "results/combined_assay_ttests.RData")

supplement_ttable <- biomarker_ttest_results |> 
  select(Assay, Outcome, Condition, ED_Mean, Control_Mean, statistic, parameter,  p.value) |> 
  mutate(t = round(statistic, 2),
         df = round(parameter, 2),
         p.value = round(p.value, 3), 
         Control_Mean = round(Control_Mean, 2),
         ED_Mean = round(ED_Mean, 2))
# drop the statistic and parameter columns
supplement_ttable <- supplement_ttable |> select(-statistic, -parameter)
# put p.value column at the end
supplement_ttable <- supplement_ttable |> select(-p.value) |> bind_cols(p.value = supplement_ttable$p.value)

# save the table
save(supplement_ttable, file = "results/supplement_ttable.RData")

Exercise_results <- biomarker_ttest_results |> filter(str_detect(Condition, "Exercise"))
# Make Outcome a Factor variable for plotting
Exercise_results$Outcome <- factor(Exercise_results$Outcome, levels = c("Pre", "Post", "RawChange", "ResidChange"))


# Clean up columns -- this just switches the sign of the estimate and CI columns so that differences represent ED > Control
Exercise_results <- Exercise_results |> 
  select(Assay, Outcome, Estimate, LowerCI, UpperCI, p.value) |> 
  mutate(Estimate = round(Estimate, 2)*-1,
         p.value = round(p.value, 3), 
         LowerCI.1 = round(UpperCI, 2)*-1,
         UpperCI.1 = round(LowerCI, 2)*-1) |> 
  # add column for unit of measure - make it pg/ml for BDNF and Leptin, and ng/mL for Cortisol
  mutate(Unit = case_when(
    Assay == "BDNF" ~ "pg/ml",
    Assay == "Leptin" ~ "pg/ml",
    Assay == "Cortisol" ~ "ng/mL"
  ))

# Make a ggplot that is a forest plot of the results
Biomarker_ForestPlot <- ggplot(Exercise_results, aes(x = Estimate, y = factor(Assay), color = Outcome)) +
  geom_point(position = position_dodge(width = 0.25), size = 3) +
  geom_errorbarh(aes(xmin = LowerCI.1, xmax = UpperCI.1), height = 0.2, position = position_dodge(width = 0.25), size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Unit, scales = "free", ncol = 1) +
  labs(title = "ED vs. Control Group Difference Scores in \n Biomarker Measurements by Assay during Exercise",
       x = "Group Difference (ED-Control)",
       y = "Assay") +
  embarktools::embark_theme_a +
  scale_color_manual(values = embark_palette(6)[c(1:4)])

#save the plot data
save(Biomarker_ForestPlot, file = 'figs/4.Biomarkers/Exercise_Biomarker_ForestPlot.RData')

ggsave("figs/4.Biomarkers/Exercise_Biomarker_ForestPlot.png", width = 8, height = 7)

# Within ED Pre-Post Biomarker Dependendent T-Tests

rm(list = ls())
load('data/Assays/Assay_results.RData')

Assay_results_ED <- Assay_results |> 
  filter(group_factor == 'ED') |> 
  select(-group_factor, -group, -Estradiol)

Assay_results_ED

# compute pre-post dependent t-tests for each assay and condition
Assays <- c('BDNF', 'Leptin', 'Cortisol')
Conditions <- c('Exercise', 'Rest')

# Pivot the data to long format
Assay_results_ED_long <- Assay_results_ED |> 
  pivot_longer(cols = Assays, names_to = "Assay", values_to = "Value") |> 
  filter(!is.na(Value))

# Compute the dependent t-tests for each assay and condition
combined_results <- list()

for (condition in Conditions) {
  for (assay in Assays) {
    # Subset the data for the current condition and group
    current_df <- Assay_results_ED_long |> 
      filter(Condition == condition, Assay == assay)
    long_df <- current_df |> pivot_wider(names_from = 'Time', values_from = 'Value')
    # calculate t-test for each assay, comparing paired samples of pre vs. post
    t <- t.test(long_df$Post, long_df$Pre, paired = TRUE)
    tidy_t <- broom::tidy(t)
    # add to combined results
    combined_results[[paste(assay, condition, sep = "_")]] <- tidy_t
  }
}

# Combine all results into a single data frame
ed_biomarkers <- do.call(rbind, combined_results)

# Make rownames a column
ed_biomarkers <- ed_biomarkers |> rownames_to_column(var = "Params")
# Split the Params column into separate columns
ed_biomarkers <- ed_biomarkers |> separate (Params, into = c("Assay", "Condition"), sep = "_")

# rename the columns
ed_biomarkers <- ed_biomarkers |> 
  rename(Estimate = estimate, LowerCI = conf.low, UpperCI = conf.high, 
         t = statistic, df = parameter, p.value = p.value)
# remove method and alternative columns
ed_biomarkers <- ed_biomarkers |> select(-method, -alternative)
# round the values to 2 decimal places
ed_biomarkers <- ed_biomarkers |> 
  mutate(Estimate = round(Estimate, 2),
         LowerCI = round(LowerCI, 2),
         UpperCI = round(UpperCI, 2),
         t = round(t, 2),
         df = round(df, 2),
         p.value = round(p.value, 3)) |> 
  mutate(Unit = case_when(
    Assay == "BDNF" ~ "pg/ml",
    Assay == "Leptin" ~ "pg/ml",
    Assay == "Cortisol" ~ "ng/mL"
  ))
# Save the results
save(ed_biomarkers, file = "results/ed_biomarkers.RData")

# add forest plot
ggplot(ed_biomarkers, aes(x = Estimate, y = Assay, color = Condition)) +
  geom_point(position = position_dodge(width = 0.25), size = 3) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, position = position_dodge(width = 0.25), size = 1.5) +
  facet_wrap(~Unit, scales = "free", ncol = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Pre-Post Biomarker Differences by Assay \n within the ED Group",
       x = "Mean Difference (Post-Pre pg/mL)",
       y = "Assay") +
  embarktools::embark_theme_a +
  scale_color_manual(values = embark_palette(6)[c(2,1)])

ggsave("figs/4.Biomarkers/ED_Biomarker_ForestPlot.png", width = 8, height = 7)


# Evaluate Homogeneity of Variance for Assays across ED and Control Groups at Pre and Post
Assay_results_long <- Assay_results |> 
  select(-group, -Estradiol) |> 
  pivot_longer(cols = c(BDNF, Leptin, Cortisol), names_to = "Assay", values_to = "Value") |> 
  filter(!is.na(Value))

# Compute Levene's test for homogeneity of variance for each assay, condition, and Time
Conditions <- c('Exercise', 'Rest')
Times <- c('Pre', 'Post')

hov_results <- list()

for (condition in Conditions) {
  for (time in Times) {
    for (assay in Assays) {
      # Subset the data for the current condition and group
      current_df <- Assay_results_long |> 
        filter(Condition == condition, Assay == assay, Time == time)
      #calculate summary statistics for each assay, condition, and time
      summary_stats <- current_df |> 
        group_by(group_factor) |> 
        summarise(variance = var(Value), sd = sd(Value), n = n())
      # Pivot the summary statistics to wide format
      summary_stats <- summary_stats |> pivot_wider(names_from = 'group_factor', values_from = c('variance', 'sd', 'n'))
      # calculate Levene's test for each assay, comparing variance of ED and Control groups
      hov <- leveneTest(Value ~ group_factor, data = current_df)
      tidy_hov <- broom::tidy(hov)
      # combine summary statistics and Levene's test results
      tidy_hov <- cbind(summary_stats, tidy_hov)
      # add to combined results
      hov_results[[paste(assay, condition, time, sep = "_")]] <- tidy_hov
    }
  }
}

# Combine all results into a single data frame
hov_results <- do.call(rbind, hov_results)

# make Rownames a column
hov_results <- hov_results |> rownames_to_column(var = "Params")
# Split the Params column into separate columns
hov_results <- hov_results |> separate (Params, into = c("Assay", "Condition", "Time"), sep = "_")

# rename the columns
hov_results <- hov_results |> 
  rename(`Control Var` = variance_Control, `ED Var` = variance_ED, 
         `Control SD` = sd_Control, `ED SD` = sd_ED, 
         `Control N` = n_Control, `ED N` = n_ED, 
         `Test Statistic` = statistic)
# round the values to 2 decimal places
hov_results <- hov_results |> 
  mutate(`Control Var` = round(`Control Var`, 2),
         `ED Var` = round(`ED Var`, 2),
         `Control SD` = round(`Control SD`, 2),
         `ED SD` = round(`ED SD`, 2),
         `Control N` = round(`Control N`, 2),
         `ED N` = round(`ED N`, 2),
         `Test Statistic` = round(`Test Statistic`, 2), 
         p.value = round(p.value, 3))

# remove df column
hov_results <- hov_results |> select(-df)

# remove variance columns
hov_results_biomarkers <- hov_results |> 
  select(-`Control Var`, -`ED Var`) |> 
  filter(Condition == 'Exercise') |> 
  select(-Condition)

# Save the results
save(hov_results_biomarkers, file = "results/hov_results_biomarkers.RData")
