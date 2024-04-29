# Load necessary libraries
library(dplyr)
library(car)
library(broom)
library(tidyr)
source('R/source/0.clean_variance.R')

load('data/Affect/affect_data_list.RData')
tasks <- c('Prescribed', 'SelfPaced')

# clean the data list
combined_results <- clean_variance(affect_data_list, tasks)

# Combine into one single dataframe
Affect_descriptives <- clean_variance_results(combined_results)


# Save the results
save(affect_descriptives, file = "Results/affect_descriptives.RData")
