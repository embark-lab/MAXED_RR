# Description: This script runs the multilevel models for the affect data.

# Load packages
library(tidyr)
library(dplyr)
library(lme4)
library(emmeans)
library(lmerTest)
library(cgwtools)


load("data/Affect/MAXED_Affect.RData")

Affect_Variables <- c('Crummy', 'Calm', 'Enthusiastic', 'Fatigued')
Tasks <- c('Prescribed', 'SelfPaced')

# Make an empty list to hold the models
affect_models <- list()
affect_emmeans <- list()
affect_models_ed <- list()
affect_emmeans_ed <- list()
affect_hov <- list()

# Loop through the variables
for (t in Tasks) {
for (var in Affect_Variables) {
  df <- Affect %>%
    filter(task == t & variable == var)
  print(head(df))
  # Overall:
  
# run lmm
affect_models[[var]][[t]] <-  lmer(value ~ 1 + age + group_factor + time + bmi*condition + time*condition + time*group_factor*condition + (1+time|condition:id), data = df, control = lmerControl(optimizer = "bobyqa")) 

print(summary(affect_models[[var]][[t]]))

# run emmeans
affect_emmeans[[var]][[t]] <- emmeans(affect_models[[var]][[t]], ~ time * condition * group_factor, at = list(time = seq(0, 30, by = 5)), data = df)
}
}
  
 # Within ED:

ed_df <- Affect |> 
  filter(group_factor == 'ED')

# Loop through the variables
for (t in Tasks) {
  for (var in Affect_Variables) {
    df <- ed_df %>%
      filter(variable == var) |> 
      filter(task == t) 
        
    # run lmm
    affect_models_ed[[var]][[t]] <-  lmer(value ~ 1 + age +  time + bmi*condition + time*condition + (1+time|condition:id), data = df) 
    print(summary(affect_models_ed[[var]][[t]]))
    # run emmeans
    affect_emmeans_ed[[var]][[t]] <- emmeans(affect_models_ed[[var]][[t]], ~ time*condition, at = list(time = seq(0, 30, by = 5)), data = df)
  }
}
  
# Heterogeneity of Variance
  # Loop through the variables
for (t in Tasks) {
  for (var in Affect_Variables) {
    df <- Affect %>%
      filter(variable == var) |> 
      filter(task == t) 
    
    # run lmm
    affect_hov[[var]][[t]] <-  lmer(value ~ (1+group_factor*time|condition:id), data = df) 
    }
}

save(affect_models, affect_emmeans, affect_models_ed, affect_emmeans_ed, affect_hov, file = "results/affect_models.RData")


process_model_summaries <- function(models_list) {
  # Create a list to hold the model summaries
  model_summaries <- list()
  # Loop through the variables and tasks
  for (var in Affect_Variables) {
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
  # Recode the term column
  combined_coefs <- mutate(combined_coefs, term = recode(term, 
                                                         "(Intercept)" = "Intercept",
                                                         "age" = "Age",
                                                         "bmi" = "BMI",
                                                         "conditionExercise" = "Condition",
                                                         "time" = "Time",
                                                         "group_factorED" = "Group",
                                                         "time:conditionExercise" = "Time x Condition",
                                                         "time:group_factorED" = "Time x Group",
                                                         "time:conditionExercise:group_factorED" = "Time x Condition x Group", 
                                                         "bmi:conditionExercise" = "BMI x Condition",
                                                         "group_factorED:conditionExercise" = "Group x Condition",
                                                         "group_factorED:time" = "Group x Time",
                                                         "group_factorED:time:conditionExercise" = "Group x Time x Condition"))
  
  # Change p-values to non-scientific notation
  combined_coefs$`Pr(>|t|)` <- format(combined_coefs$`Pr(>|t|)`, scientific = FALSE)
  combined_coefs<- combined_coefs |> 
    select(c("variable", "task", "term", "Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")) 
  # Get the name of the input object
  input_name <- deparse(substitute(models_list))
  # Create the new object name by appending "_summary"
  new_object_name <- paste0(input_name, "_summary")
  # Assign the modified dataframe to the new object name in the global environment
  assign(new_object_name, combined_coefs, envir = .GlobalEnv)
  # Save
  resave(list = new_object_name, file = paste0("results/affect_models.RData"))
  
}

process_model_summaries(affect_models)
process_model_summaries(affect_models_ed)
process_model_summaries(affect_hov)
