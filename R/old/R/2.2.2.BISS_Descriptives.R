# Load necessary libraries
library(dplyr)
library(car)
library(broom)

load('data/BISS/biss_data.RData')

tasks <- c('Prescribed', 'Self-Paced')

combined_results <- list()

vars <- unique(BISS$variable)

# Initialize an empty list to store the data frames
df_list <- list()

for (var in vars) {
  # For each unique variable, filter, group, mutate and then store the resultant data frame in the list
  df_temp <- BISS %>%
    filter(variable == var) %>%
    group_by(id, task, condition) %>%
    mutate(var_30 =  ifelse(time == 30, value, NA_real_),
           max_var = max(value, na.rm = TRUE),
           min_var = min(value, na.rm = TRUE),
           bl_var = ifelse(time == 0, value, NA_real_)) %>%
    mutate(bl_var = first(na.omit(bl_var)), 
           var_30 = first(na.omit(var_30))) %>% 
    mutate(var_change = max_var - bl_var) |> 
    mutate(var_change_30 = var_30 - bl_var) |> 
    mutate(var_change_min = bl_var - min_var) |>
    mutate(variance = var(value, na.rm = TRUE)) %>%
    select(id, group_factor, task, max_var, bl_var, var_change, var_change_30, var_change_min, variance) |> 
    ungroup() %>%
    distinct() |> 
    filter(!is.na(var_change_30))
  
  # Append the data frame to the list
  df_list[[var]] <- df_temp
}

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
      combined_result$biss_variable <- var_name
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
  select(variable_name, biss_variable, task, mean_ED, mean_Control, sd_ED, sd_Control, `F (Levene's)`, df, p.value)
# rename columns to be a little cleaner
final_combined_results <- final_combined_results %>% 
  rename(`Mean (ED)` = mean_ED, 
         `Mean (Control)` = mean_Control, 
         `SD (ED)` = sd_ED, 
         `SD (Control)` = sd_Control, 
         Variable = biss_variable, 
         `Change Indicator` = variable_name)


# Rename 'final_combined_results' to 'affect_descriptives'
BISS_descriptives <- final_combined_results
# Recode the 'Change Indicator' column using base R
BISS_descriptives$`Change Indicator` <- with(BISS_descriptives, ifelse(`Change Indicator` == "var_change_30", "30 min vs. BL",
                                                                           ifelse(`Change Indicator` == "var_change", "Max - BL",
                                                                                  ifelse(`Change Indicator` == "var_change_min", "BL - Min",
                                                                                         `Change Indicator`))))

# Save the results
save(BISS_descriptives, file = "Results/BISS_descriptives.RData")
