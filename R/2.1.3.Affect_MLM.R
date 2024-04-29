# Description: This script runs the multilevel models for the affect data.

# Load packages
library(tidyr)
library(dplyr)
library(lme4)
library(emmeans)
library(lmerTest)
library(cgwtools)


load("data/Affect/MAXED_Affect.RData")
source('R/source/0.Process_Model_Summaries.R')

Variables <- c('Crummy', 'Calm', 'Enthusiastic', 'Fatigued')
Tasks <- c('Prescribed', 'SelfPaced')


# standardize bmi
Affect$bmi <- (Affect$bmi - mean(Affect$bmi, na.rm = TRUE)) / sd(Affect$bmi, na.rm = TRUE)
# standardize age
Affect$age <- (Affect$age - mean(Affect$age, na.rm = TRUE)) / sd(Affect$age, na.rm = TRUE)

# recode condition so that exercise is the reference group
Affect$condition <- relevel(Affect$condition, ref = "Rest")

# Make an empty list to hold the models
affect_models <- list()
affect_emmeans <- list()
affect_models_ed <- list()
affect_emmeans_ed <- list()

# Loop through the variables
for (t in Tasks) {
for (var in Variables) {
  df <- Affect %>%
    filter(task == t & variable == var)
  print(head(df))
  # Overall:
  
# run lmm
affect_models[[var]][[t]] <-  lmer(value ~ 1 + age + group_factor + time + bmi*condition + time*condition + time*group_factor*condition + 
                                     (1+time|condition:id), data = df, 
                                   control = lmerControl(optimizer = "bobyqa")) 

print(summary(affect_models[[var]][[t]]))

# run emmeans
affect_emmeans[[var]][[t]] <- emmeans(affect_models[[var]][[t]], ~ time * condition * group_factor, 
                                      at = list(time = seq(0, 30, by = 5)), data = df)
}
}
  
 # Within ED:

ed_df <- Affect |> 
  filter(group_factor == 'ED')

# Loop through the variables
for (t in Tasks) {
  for (var in Variables) {
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
  


save(affect_models, affect_emmeans, affect_models_ed, affect_emmeans_ed, file = "results/affect_models.RData")


process_model_summaries(affect_models)
# save
resave(affect_models_summary, file = "results/affect_models.RData")

process_model_summaries(affect_models_ed)
resave(affect_models_ed_summary, file = "results/affect_models.RData")
