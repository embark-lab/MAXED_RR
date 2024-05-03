# Description: This script runs the multilevel models for the BISS data.

source("R/source/0.Packages.R")
source("R/source/2.Process_Model_Summaries.R")
load("data/BISS/biss_data.RData")

Variables <- (unique(BISS$variable))
Tasks <- unique(BISS$task)
# make condition a factor, with 'Rest' as the reference level
BISS$condition <- factor(BISS$condition, levels = c('Rest', 'Exercise'))
# Standardize bmi
BISS$bmi <- scale(BISS$bmi)
# Standardize age
BISS$age <- scale(BISS$age)

# Make an empty list to hold the models
biss_models <- list()
biss_emmeans <- list()
biss_models_ed <- list()
biss_emmeans_ed <- list()

# Loop through the variables
for (t in Tasks) {
for (var in Variables) {
  df <- BISS %>%
    filter(task == t & variable == var)
  print(head(df))
  # Overall:
  
# run lmm
biss_models[[var]][[t]] <-  lmer(value ~ 1 + age + group_factor + time + bmi*condition + time*condition + time*group_factor*condition + 
                                   (1+time|condition:id), 
                                 data = df, 
                                 control = lmerControl(optimizer = "bobyqa")) 

print(summary(biss_models[[var]][[t]]))

# run emmeans
biss_emmeans[[var]][[t]] <- emmeans(biss_models[[var]][[t]], ~ time * condition * group_factor, 
                                    at = list(time = seq(0, 30, by = 5)), 
                                    data = df)
}
}
  
 # Within ED:

ed_df <- BISS |> 
  filter(group_factor == 'ED')

# Loop through the variables
for (t in Tasks) {
  for (var in Variables) {
    df <- ed_df %>%
      filter(variable == var) |> 
      filter(task == t) 
        
    # run lmm
    biss_models_ed[[var]][[t]] <-  lmer(value ~ 1 + age +  time + bmi*condition + time*condition + 
                                          (1+time|condition:id), 
                                        data = df,
                                        control = lmerControl(optimizer = "bobyqa")) 
    print(summary(biss_models_ed[[var]][[t]]))
    # run emmeans
    biss_emmeans_ed[[var]][[t]] <- emmeans(biss_models_ed[[var]][[t]], ~ time*condition, at = list(time = seq(0, 30, by = 5)), data = df)
  }
}
  


save(biss_models, biss_emmeans, biss_models_ed, biss_emmeans_ed, file = "results/biss_models.RData")

process_model_summaries(biss_models)
# resave
resave(biss_models_summary, file = "results/biss_models.RData")


# Note - Prescribed Weight Model in ED condition has singular fit -- due to those with low intercepts having no variance in time
# one fix is to remove those with low intercepts
# problem_data <- ed_df %>%
#   filter(variable == 'Weight') |> 
#   filter(task == 'Prescribed') |> 
#   filter(!(time == 0 & value == 1))
# 
# problem_model <- lmer(value ~ 1 + time*condition + bmi*condition + (1+time|condition:id), 
#                       data = problem_data,
#                       control = lmerControl(optimizer = "bobyqa"))
# summary(problem_model)
# 
# # replace biss_models_ed$Weight$Prescribed with problem_model
# biss_models_ed$Weight$Prescribed <- problem_model
process_model_summaries(biss_models_ed)
resave(biss_models_ed_summary, file = "results/biss_models.RData")
