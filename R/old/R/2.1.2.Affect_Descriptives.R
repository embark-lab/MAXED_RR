# Load necessary libraries
library(dplyr)
library(car)
library(broom)
library(tidyr)

load('data/Affect/affect_plot_data.RData')

# for each data frame in df_list, select the id, group_factor, task, and variance columns and store the result in a new data frame

affect_variance <- list()
for (i in 1:length(df_list)) {
  affect_variance[[i]] <- df_list[[i]] %>% 
    select(id, group_factor, task, variance)
  names(affect_variance)[i] <- names(df_list)[i]
}


# Define the tasks
tasks <- c('Prescribed', 'SelfPaced')

# Initialize a list to store combined results
combined_results <- list()

# Loop over the affect variables in df_list
for (var_name in names(affect_variance)) {
  # Extract the dataframe for the current affect variable
  current_df <- affect_variance[[var_name]]
  
  # Loop over each task
  for (task_name in tasks) {
    # Subset the data for the current task
    task_data <- filter(current_df, task == task_name)
    
    # Process each variable
    for (variable in c("variance")) {
      # Initialize a dataframe to store stats
      stats <- data.frame(group_factor = unique(task_data$group_factor))
      
      # Calculate mean and sd for each group
      stats$mean <- sapply(stats$group_factor, function(g) mean(task_data[[variable]][task_data$group_factor == g], na.rm = TRUE))
      stats$sd <- sapply(stats$group_factor, function(g) sd(task_data[[variable]][task_data$group_factor == g], na.rm = TRUE))
      
      
      t_test_result <- t.test(reformulate("group_factor", response = variable), 
                             data = task_data)
      tidy_t_test <- broom::tidy(t_test_result)
      combined_result <- cbind(stats, tidy_t_test)
      
      # Perform Levene's Test for homogeneity of variances
    #  levene_result <- leveneTest(reformulate("group_factor", response = variable), 
                            #      data = task_data, 
                            #      center = median)
      
      # Tidy the Levene's test result
   #   tidy_levene <- broom::tidy(levene_result)
      
      # Combine stats and Levene's test result
    #  combined_result <- cbind(stats, tidy_levene)
      
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
final_combined_results <- final_combined_results %>% select(-ID)

# Round all numeric values to 2 decimal places
final_combined_results <- final_combined_results %>% mutate_if(is.numeric, round, 2)

# Rename columns 
final_combined_results <- final_combined_results %>% 
  rename(t = statistic, 
         df = parameter)

# Reorder the columns (if needed)
final_combined_results <- final_combined_results %>% 
  select(variable_name, affect_variable, task, mean_ED, mean_Control, t, df, p.value)
# rename columns to be a little cleaner
final_combined_results <- final_combined_results %>% 
  rename(`Avg Within Session Variance (ED)` = mean_ED, 
         `Avg Within Session Variance (Control)` = mean_Control, 
          Variable = affect_variable, 
         `Change Indicator` = variable_name)


# Rename 'final_combined_results' to 'affect_descriptives'
affect_descriptives <- final_combined_results
# Recode the 'Change Indicator' column 
affect_descriptives$`Change Indicator` <- with(affect_descriptives, ifelse(`Change Indicator` == "variance", "Variance"))

# Save the results
save(affect_descriptives, file = "Results/affect_descriptives.RData")
