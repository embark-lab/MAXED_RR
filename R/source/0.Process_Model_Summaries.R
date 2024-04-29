
process_model_summaries <- function(models_list) {
  # Create a list to hold the model summaries
  model_summaries <- list()
  # Loop through the variables and tasks
  for (var in Variables) {
    for (t in Tasks) {
      # Extract the model for the current variable and task
      if (!is.null(models_list[[var]]) && !is.null(models_list[[var]][[t]])) {
        model <- models_list[[var]][[t]]
        
        # Get the summary of the model
        summary_table <- summary(model)
        
        # Store the summary in the list with a meaningful name
        model_summaries[[paste(var, t, sep = "_")]] <- summary_table
      }
    }
  }  
  
  combined_coefs <- data.frame()
  for (model_name in names(model_summaries)) {
    current_coefs <- as.data.frame( coef(summary(model_summaries[[model_name]])) )
    current_coefs <- mutate(current_coefs, model = model_name)
    combined_coefs <- bind_rows(combined_coefs, current_coefs)
  }
  
  # Remove '...' and any numbers from the row names
  rownames(combined_coefs) <- gsub("\\.\\.\\.", "", rownames(combined_coefs))
  # Break apart the model column into the affect variable and task
  combined_coefs <- separate(combined_coefs, model, c("variable", "task"), sep = "_")
  # Make rownames a column called "term"
  combined_coefs <- mutate(combined_coefs, term = rownames(combined_coefs))
  rownames(combined_coefs) <- NULL  # Remove the rownames
  # Take numbers out of the term column
  combined_coefs <- mutate(combined_coefs, term = gsub("[0-9]", "", term))
  # Change p-values to non-scientific notation
  combined_coefs$`Pr(>|t|)` <- format(combined_coefs$`Pr(>|t|)`, scientific = FALSE)
  combined_coefs <- mutate(combined_coefs, term = dplyr::recode(term,
                                                                              `(Intercept)` = "Intercept",
                                                                              age = "Age",
                                                                              bmi = "BMI",
                                                                              conditionExercise = "Condition",
                                                                              time = "Time",
                                                                              group_factorED = "Group",
                                                                              `time:conditionExercise` = "Time x Condition",
                                                                              `time:group_factorED` = "Time x Group",
                                                                              `time:conditionExercise:group_factorED` = "Time x Condition x Group",
                                                                              `bmi:conditionExercise` = "BMI x Condition",
                                                                              `group_factorED:conditionExercise` = "Group x Condition",
                                                                              `group_factorED:time` = "Group x Time",
                                                                              `group_factorED:time:conditionExercise` = "Group x Time x Condition",
                                                                              
                                                                              .default = term  # This preserves original values where no recode is defined
  ))
  combined_coefs<- combined_coefs |> 
    select(c("variable", "task", "term", "Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")) 
  # Get the name of the input object
  input_name <- deparse(substitute(models_list))
  # Create the new object name by appending "_summary"
  new_object_name <- paste0(input_name, "_summary")
  # Assign the modified dataframe to the new object name in the global environment
  assign(new_object_name, combined_coefs, envir = .GlobalEnv)
}
