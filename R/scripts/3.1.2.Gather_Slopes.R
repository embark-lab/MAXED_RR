library(dplyr)
library(embarktools)
library(tidyr)
library(stringr)
library(tibble)

load("Data/Affect/Affect_Slope_Estimates.RData")
load("Data/BISS/BISS_Slope_Estimates.RData")
load("Data/Assays/Biomarker_residuals.RData")

# Pivot affect and BISS data

variables <- unique(affect_slope_estimates$variable)

# recode Self-Paced and Prescribed to be 'SP" and 'P'
affect_slope_estimates$task <- affect_slope_estimates$task |>
  str_replace("SelfPaced", "SP") |>
  str_replace("Prescribed", "P")

affect_slope_estimates <- affect_slope_estimates |> 
  pivot_wider(names_from = variable, values_from = slope) |> 
  rename(id = id, task = task)

# Pivot wider again for task
affect_slope_estimates <- affect_slope_estimates |> 
  pivot_wider(names_from = task, values_from = variables) |> 
  rename_with(~str_replace(., " ", "_"))


variables <- unique(biss_slope_estimates$variable)

biss_slope_estimates$task <- biss_slope_estimates$task |>
  str_replace("Self-Paced", "SP") |>
  str_replace("Prescribed", "P")

biss_slope_estimates <- biss_slope_estimates |> 
  pivot_wider(names_from = variable, values_from = slope) |> 
  rename(id = id, task = task)

biss_slope_estimates <- biss_slope_estimates |>
  pivot_wider(names_from = task, values_from = variables) |> 
  rename_with(~str_replace(., " ", "_"))

# Fileter exercise condition from biomarker residuals
Biomarker_residuals <- Biomarker_residuals |>
  filter(Condition == "Exercise") |>
  select(-Condition, -group, -group_factor) |> 
  # rename ID to id
  rename(id = ID)

# Import groups data
load("Data/RedCap/groups.RData")

# Merge all data by id
exercise_response_data <- full_join(affect_slope_estimates, biss_slope_estimates, by = "id") |>
  full_join(Biomarker_residuals, by = "id") |>
  full_join(groups, by = "id")

# remove Pilot ids
Pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', 'MAXED_1011', 'MAXED_1012')

exercise_response_data <- exercise_response_data |>
  filter(!id %in% Pilot_ids)

#save exercise response data
save(exercise_response_data, file = "Data/Exercise_Response_Summary_Data.RData")

# Make all columns (except for ID) numeric
exercise_response_data <- exercise_response_data |>
  mutate(across(-id, as.numeric))

#make a correlation matrix, excluding ID and removing pairwise NA values
exercise_response_cors <- cor(exercise_response_data |>
                            select(-id), use = "pairwise.complete.obs")

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

# combine a list of all key vars
key_variables <- c(key_biomarker_variables, key_BISS_variables, key_affect_variables, 'group')

# save exercise response data
save(exercise_response_data, file = "data/Exercise_Response/Exercise_Response_Summary_Data.RData")

exercise_response_cors_control <- cor(exercise_response_data |>
                                   filter(group == "0") |>
                                   select(-id), use = "pairwise.complete.obs")

#pivot correlation matrix to long Format
correlation_matrix_long_control <- exercise_response_cors_control |>
  as.data.frame() |>
  rownames_to_column(var = "variable1") |>
  pivot_longer(cols = -variable1, names_to = "variable2", values_to = "correlation") |> 
  filter(correlation != 1)
# filter out variables not in key variables
correlation_matrix_long_control <- correlation_matrix_long_control |>
  filter(variable1 %in% key_variables & variable2 %in% key_variables)


correlation_matrix_long_control$variable1 <- factor(correlation_matrix_long_control$variable1, levels = c(key_affect_variables, key_biomarker_variables, key_BISS_variables))
correlation_matrix_long_control$variable2 <- factor(correlation_matrix_long_control$variable2, levels = c(key_affect_variables, key_biomarker_variables, key_BISS_variables))


data_lower_triangle <- correlation_matrix_long_control %>%
  filter(as.numeric(variable2) < as.numeric(variable1)) # You'll replace 'comparison' with actual comparison logic

# Make upper triangle the ED group only
exercise_response_cors_ed <- cor(exercise_response_data |>
                            filter(group == "1") |>
                            select(-id), use = "pairwise.complete.obs")


# long correlation matrix for ed group
correlation_matrix_long_ed <- exercise_response_cors_ed |>
  as.data.frame() |>
  rownames_to_column(var = "variable1") |>
  pivot_longer(cols = -variable1, names_to = "variable2", values_to = "correlation") |> 
  filter(correlation != 1)
# filter out variables not in key variables
correlation_matrix_long_ed <- correlation_matrix_long_ed |>
  filter(variable1 %in% key_variables & variable2 %in% key_variables)

correlation_matrix_long_ed$variable1 <- factor(correlation_matrix_long_ed$variable1, levels = c(key_affect_variables, key_biomarker_variables, key_BISS_variables))
correlation_matrix_long_ed$variable2 <- factor(correlation_matrix_long_ed$variable2, levels = c(key_affect_variables, key_biomarker_variables, key_BISS_variables))

data_lower_triangle_ed <- correlation_matrix_long_ed %>%
  filter(as.numeric(variable2) < as.numeric(variable1))

data_lower_triangle_ed <- data_lower_triangle_ed %>%
  mutate(dataset = "ED")

data_lower_triangle <- data_lower_triangle %>%
  mutate(dataset = "Control")

# Combine the datasets
combined_correlation_data <- bind_rows(data_lower_triangle, data_lower_triangle_ed)

ggplot(combined_correlation_data, aes(x = variable1, y = variable2, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = embark_colors[1], high = embark_colors[2], mid = "white", midpoint = 0) +
  theme_minimal() +
  facet_grid(dataset ~ .) +
  labs(title = "Correlation Matrix of Exercise Response Data \n across Control and ED groups", 
       caption = "Note: Correlation matrix of key variables in exercise response data. P = Prescribed, SP = Self Paced; 
       Variables represent Slopes (Body Image and Affect) and residualized change scores (Biomarkers).") +
 embark_theme_a +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 10), 
        legend.position = "right",
        plot.caption = element_text(size = 9), 
        legend.title = element_blank(),
        # bold the facet labels
        strip.text = element_text(face = "bold")) +
# make gridlines light grey
theme(panel.grid.major = element_line(colour = "grey80"),
      panel.grid.minor = element_blank())

# save the plot
ggsave("figs/5.correlations/Ex_response_cormat.png", width = 10, height = 10, dpi = 300)

# calculate average r2 for each group
combined_correlation_data |>
  group_by(dataset) |>
  summarize(mean_correlation = mean(sqrt(correlation^2), na.rm = TRUE))

# save combined correlation data as .RData
save(combined_correlation_data, file = "results/ex_response_correlation_data.RData")


# calculate average r2 for each group within key variable categories
corr_data2 <- combined_correlation_data |> 
  # add variable indicating which category each variable belongs to
  mutate(category1 = case_when(
    variable1 %in% key_biomarker_variables ~ "Biomarkers",
    variable1 %in% key_BISS_variables ~ "BISS",
    variable1 %in% key_affect_variables ~ "Affect"
  ), 
  category2 = case_when(
    variable2 %in% key_biomarker_variables ~ "Biomarkers",
    variable2 %in% key_BISS_variables ~ "BISS",
    variable2 %in% key_affect_variables ~ "Affect"
  )) 

# filter vars when category 1 and 2 are the same
corr_data2 <- corr_data2 |>
  filter(category1 == category2)

