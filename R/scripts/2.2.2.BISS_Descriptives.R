# Load necessary libraries
source('R/source/0.Packages.R')
source('R/source/2.clean_variance.R')

load('data/BISS/biss_data.RData')
tasks <- c('Prescribed', 'Self-Paced')
vars <- unique(BISS$variable)

# Prepare the data list
df_list <- prep_df_list(BISS, vars)

# clean the data list
combined_results <- clean_variance(df_list, tasks)

# Combine into one single dataframe
BISS_descriptives <- clean_variance_results(combined_results)

# Save the results
save(BISS_descriptives, file = "Results/BISS_descriptives.RData")
