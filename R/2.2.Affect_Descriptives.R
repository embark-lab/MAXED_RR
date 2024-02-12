# Load necessary libraries
library(dplyr)
library(car)
library(broom)

load('data/Affect/affect_plot_data.RData')

# Define the tasks
tasks <- c('Prescribed', 'SelfPaced')

# Initialize a list to store combined results
combined_results <- list()

# Loop over the affect variables in df_list
for (var_name in names(df_list)) {
  # Extract the dataframe for the current affect variable
  current_df <- df_list[[var_name]]
  
  # Loop over each task
  for (task_name in tasks) {
    # Subset the data for the current task
    task_data <- filter(current_df, task == task_name)
    
    # Process each variable
    for (variable in c("var_change_30", "var_change", "var_change_min")) {
      # Initialize a dataframe to store stats
      stats <- data.frame(group_factor = unique(task_data$group_factor))
      
      # Calculate mean and sd for each group
      stats$mean <- sapply(stats$group_factor, function(g) mean(task_data[[variable]][task_data$group_factor == g], na.rm = TRUE))
      stats$sd <- sapply(stats$group_factor, function(g) sd(task_data[[variable]][task_data$group_factor == g], na.rm = TRUE))
      
      # Perform Levene's Test for homogeneity of variances
      levene_result <- leveneTest(reformulate("group_factor", response = variable), 
                                  data = task_data, 
                                  center = median)
      
      # Tidy the Levene's test result
      tidy_levene <- broom::tidy(levene_result)
      
      # Combine stats and Levene's test result
      combined_result <- cbind(stats, tidy_levene)
      
      # Add variable, task, and affect variable columns for identification
      combined_result$variable_name <- variable
      combined_result$affect_variable <- var_name
      combined_result$task <- task_name
      
      # Store the combined result
      combined_results[[paste(var_name, task_name, variable, sep = "_")]] <- combined_result
    }
  }
}

# Process each combination of variable, task, and affect variable
for (combination in names(combined_results)) {
  # Add a row identifier
  combined_results[[combination]]$row_id <- paste(combined_results[[combination]]$affect_variable, 
                                                  combined_results[[combination]]$task, 
                                                  combined_results[[combination]]$variable_name, 
                                                  sep = "_")
  
  # Pivot wider to separate columns for each group and statistic
  combined_results[[combination]] <- combined_results[[combination]] %>% 
    pivot_wider(names_from = group_factor, 
                values_from = c(mean, sd),
                names_sep = "_")
}

# Combine all results into one dataframe
final_combined_results <- bind_rows(combined_results, .id = "ID")

# Drop the ID column
final_combined_results <- final_combined_results %>% select(-ID, -df)

# Round all numeric values to 2 decimal places
final_combined_results <- final_combined_results %>% mutate_if(is.numeric, round, 2)

# Rename columns as needed (example: renaming 'statistic' to 'F (Levene's)')
final_combined_results <- final_combined_results %>% 
  rename(`F (Levene's)` = statistic, 
         df = df.residual)

# Reorder the columns (if needed)
final_combined_results <- final_combined_results %>% 
  select(variable_name, affect_variable, task, mean_ED, mean_Control, sd_ED, sd_Control, `F (Levene's)`, df, p.value)
# rename columns to be a little cleaner
final_combined_results <- final_combined_results %>% 
  rename(`Mean (ED)` = mean_ED, 
         `Mean (Control)` = mean_Control, 
         `SD (ED)` = sd_ED, 
         `SD (Control)` = sd_Control, 
          Variable = affect_variable, 
         `Change Indicator` = variable_name)


# Rename 'final_combined_results' to 'affect_descriptives'
affect_descriptives <- final_combined_results
# Recode the 'Change Indicator' column using base R
affect_descriptives$`Change Indicator` <- with(affect_descriptives, ifelse(`Change Indicator` == "var_change_30", "30 min vs. BL",
                                                                           ifelse(`Change Indicator` == "var_change", "Max - BL",
                                                                                  ifelse(`Change Indicator` == "var_change_min", "BL - Min",
                                                                                         `Change Indicator`))))

# Save the results
save(affect_descriptives, file = "Results/affect_descriptives.RData")
