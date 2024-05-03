# Load necessary libraries
source('R/source/0.Packages.R')
source('R/source/2.Clean_Variance.R')

load('data/Affect/MAXED_Affect.RData')
tasks <- c('Prescribed', 'SelfPaced')
vars <- unique(Affect$variable)

# Prepare the data list
affect_data_list <- prep_df_list(Affect, vars)

# clean the data list
combined_results <- clean_variance(affect_data_list, tasks)

# Combine into one single dataframe
affect_descriptives <- clean_variance_results(combined_results)


# Save the results
save(affect_descriptives, file = "Results/affect_descriptives.RData")
