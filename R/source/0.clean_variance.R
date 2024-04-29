library(dplyr)

prep_df_list <- function(df, vars) {
  # Initialize an empty list to store the results
  df_list <- list()
  
  for (var in vars) {
    # Filter for each unique variable and apply transformations
    df_temp <- df %>%
      filter(variable == var) %>%
      group_by(id, task, condition) %>%
      mutate(
        var_30 = ifelse(time == 30, value, NA_real_), # Capture value at time = 30
        max_var = max(value, na.rm = TRUE), # Maximum value
        min_var = min(value, na.rm = TRUE), # Minimum value
        bl_var = ifelse(time == 0, value, NA_real_) # Baseline value at time = 0
      ) %>%
      mutate(
        bl_var = first(na.omit(bl_var)),  # Ensure only the first non-NA baseline value is used
        var_30 = first(na.omit(var_30))  # Ensure only the first non-NA value at time 30 is used
      ) %>%
      mutate(
        var_change = max_var - bl_var,  # Change from baseline to max
        var_change_30 = var_30 - bl_var,  # Change from baseline to value at time 30
        var_change_min = bl_var - min_var,  # Change from baseline to minimum value
        variance = var(value, na.rm = TRUE)  # Variance of values
      ) %>%
      ungroup() %>%
      select(id, group_factor, task, max_var, bl_var, var_change, var_change_30, var_change_min, variance) %>%
      distinct() %>%
      filter(!is.na(var_change_30))  # Ensure there's a valid change at time 30
    
    # Append the processed dataframe to the list
    df_list[[var]] <- df_temp
  }
  
  return(df_list)  # Return the list of dataframes
}

clean_variance <- function(df_list, tasks) {
  combined_results <- list()
  # Initialize an empty list to store the results
  # Loop over the affect variables in df_list
  for (var_name in names(df_list)) {
    # Extract the dataframe for the current affect variable
    current_df <- df_list[[var_name]]
    
    # Loop over each task
    for (task_name in tasks) {
      # Subset the data for the current task
      task_data <- filter(current_df, task == task_name)
      
      # Process each variable
      for (variable in c("variance", "var_change_30", "var_change", "var_change_min")) {
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
        combined_result$change_indicator <- variable
        combined_result$dv <- var_name
        combined_result$task <- task_name
        
        # add cohen's d
        cohen_d_formula <- as.formula(paste(variable, "~ group_factor"))
        cohen_d_result <- cohen.d(formula = cohen_d_formula, data = task_data)
        combined_result$cohen_d <- cohen_d_result$estimate*-1      
        
        # Store the combined result
        combined_results[[paste(var_name, task_name, variable, sep = "_")]] <- combined_result
      }
    }
  }
  return(combined_results)
}

clean_variance_results <- function(combined_results) {
for (combination in names(combined_results)) {
  # Add a row identifier
  combined_results[[combination]]$row_id <- paste(combined_results[[combination]]$dv, 
                                                  combined_results[[combination]]$task, 
                                                  combined_results[[combination]]$change_indicator, 
                                                  sep = "_")
  
  # Pivot wider to separate columns for each group and statistic
  combined_results[[combination]] <- combined_results[[combination]] %>% 
    pivot_wider(names_from = group_factor, 
                values_from = c(mean, sd),
                names_sep = "_")
}

# Combine all results into one dataframe
cleaned.1 <- bind_rows(combined_results, .id = "ID") %>% 
  select(-ID, -df, -row_id) %>% 
  mutate_if(is.numeric, round, 2)

# Rename columns 
cleaned_variance_df <- cleaned.1 %>% 
  rename(`F (Levene's)` = statistic, 
         df = df.residual) %>% 
  select(dv, change_indicator, task, mean_ED, mean_Control, sd_ED, sd_Control, cohen_d, `F (Levene's)`, df, p.value) %>% 
  rename(`Mean (ED)` = mean_ED, 
         `Mean (Control)` = mean_Control, 
         `SD (ED)` = sd_ED,
         `SD (Control)` = sd_Control,
          Variable = dv, 
         `Change Indicator` = change_indicator, 
         `Cohen's d` = cohen_d, 
         `Levenes p-value` = p.value) 

cleaned_variance_df$`Change Indicator` <- with(cleaned_variance_df, ifelse(`Change Indicator` == "var_change_30", "30 min vs. BL",
                                                                         ifelse(`Change Indicator` == "var_change", "Max - BL",
                                                                                ifelse(`Change Indicator` == "var_change_min", "BL - Min",
                                                                                      `Change Indicator`))))
return(cleaned_variance_df)
}

