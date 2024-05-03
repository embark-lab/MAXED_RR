# Description: This script runs the multilevel models for the affect data.

source('R/source/0.Packages.R')
load("data/Affect/MAXED_Affect.RData")
Affect_Variables <- c('Crummy', 'Calm', 'Enthusiastic', 'Fatigued')
Tasks <- c('Prescribed', 'SelfPaced')

# plot raw data
  ggplot(data = Affect |> filter (task == 'Prescribed' & condition == 'Exercise'), 
                                  aes(x = time, y = value, color = group_factor, group = group_factor)) +
  facet_wrap(~variable, scales = 'free_y') +
  theme_minimal() +
  labs(title = 'Raw Affect Data', x = 'Time', y = 'Value') + 
  stat_summary(fun.y = "mean", geom="line", size = 2, position=position_dodge(0.95)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", width = 0.1, position=position_dodge(0.90)) +
  embarktools::embark_theme_a + 
  scale_color_manual(values = embarktools::embark_palette(6)) 
  # put caps on the error bars
  
  # plot raw data
  ggplot(data = Affect |> filter (task == 'SelfPaced' & condition == 'Exercise'), 
         aes(x = time, y = value, color = group_factor, group = group_factor)) +
    facet_wrap(~variable, scales = 'free_y') +
    theme_minimal() +
    labs(title = 'Raw Affect Data', x = 'Time', y = 'Value') + 
    stat_summary(fun.y = "mean", geom="line", size = 2, position=position_dodge(0.95)) +
    stat_summary(fun.data = "mean_cl_boot", geom="errorbar", width = 0.1, position=position_dodge(0.90)) +
    embarktools::embark_theme_a + 
    scale_color_manual(values = embarktools::embark_palette(6)) 
  # put caps on the error bars

  Affect_clean <- na.omit(Affect)
  # make separate data for each task
  Affect_clean_prescribed <- Affect_clean |> filter(task == 'Prescribed' & condition == 'Exercise')
  Affect_clean_selfpaced <- Affect_clean |> filter(task == 'SelfPaced' & condition == 'Exercise')
  
  # join the data in a list
  task_data <- list(Prescribed = Affect_clean_prescribed, SelfPaced = Affect_clean_selfpaced)
  
 anovas <- list() 
variance_df <- data.frame()
for (t in Tasks) {
    for (var in Affect_Variables) {
 m0 <- lme(fixed = value ~ 1 + age + group_factor + time + time*group_factor,
           random = list(id = pdSymm(form = ~ 1)),
           data = task_data[[t]] |> filter(variable == var), 
           method = 'REML')
 # Checking heterogeneity of time effect between groups
 m1 <- lme(fixed = value ~ 1 + age + group_factor + time + time*group_factor,  
           random = list(id = pdDiag(form = ~ group_factor)),
           data = task_data[[t]]  |> filter(variable == var),
           method = 'REML')
  m2 <-  lme(fixed = value ~ 1 + age + group_factor + time + time*group_factor,  
             random = list(id = pdDiag(form = ~ group_factor)),
             weights = varIdent(form = ~ 1 | group_factor),
             data = task_data[[t]]  |> filter(variable == var),
             method = 'REML')
  
  Control_between_var <- as.numeric(VarCorr(m2)[1])
  ED_between_var <- Control_between_var + as.numeric(VarCorr(m2)[2])
  Control_within_var <- as.numeric( (summary(m2)$sigma*coef(m2$modelStruct$varStruct, uncons=FALSE))^2 )
  ED_within_var <- (summary(m2)$sigma*1.0000)^2
  # put the results in a data frame
  variances <- data.frame(
    Task = t,
    Variable = var,
    Model = "Between + Within",
    Between_Variance_Control = Control_between_var,
    Between_Variance_ED = ED_between_var,
    Within_Variance_Control = Control_within_var, 
    Within_Variance_ED = ED_within_var
  )
  variance_df <- rbind(variance_df, variances)
  
  
anovas[[t]][[var]][['between']] <- anova(m0, m1)
anovas[[t]][[var]][['between + within']] <- anova(m1, m2)

    }
  }
 
 # Initialize an empty data frame to store final results
 anovas_df <- data.frame()
 
 # Your list of tasks and variables might be extracted from the names of the anovas list
 # tasks <- names(anovas)
 # variables <- names(anovas[[tasks[1]]]) # Adjust based on actual structure
 
 for (task in names(anovas)) {
   for (variable in names(anovas[[task]])) {
     for (comparison in c("between", "between + within")) {
       # Access the ANOVA result
       anova_result <- anovas[[task]][[variable]][[comparison]]
       
       # Create a temporary data frame with the ANOVA result and new variables
       temp_df <- cbind(
         data.frame(task = task, variable = variable, comparison = comparison),
         anova_result
       )
       # Row-bind this temporary data frame to the final results data frame
       anovas_df <- rbind(anovas_df, temp_df)
     }
   }
 }
 


 # remove the 'call column'
anovas_df <- anovas_df |> 
   select(-call)
# recode if comparison = 'between' and model = '1', model = 'null'
anovas_df <- anovas_df |> 
  mutate(Model = case_when(
    comparison == 'between' & Model == '1' ~ 'Null',
    comparison == 'between' & Model == '2' ~ 'Between',
    comparison == 'between + within' & Model == '1' ~ 'Between',
    comparison == 'between + within' & Model == '2' ~ 'Between + Within'))
# remove rows with remainder 3 after dividing by 4
anovas_df <- anovas_df |> 
  filter(row_number() %% 4 != 3)

# recode 'test
anovas_df <- anovas_df |> 
  mutate(Test = case_when(
    Model == 'Between' ~ 'Null vs Between',
    Model == 'Between + Within' ~ 'Between vs Full'))
# remove comparison column
anovas_df <- anovas_df |> 
  select(-comparison)

# rename to affect_hov_results
affect_hov_results <- anovas_df |> 
  rename(
    Task = task,
    Variable = variable)

# add the variance_df
affect_hov_results <- affect_hov_results |> 
  left_join(variance_df, by = c('Task', 'Variable', 'Model'))

# remove rownames
rownames(affect_hov_results) <- NULL

# save the damn hov results
save(affect_hov_results, file = "results/affect_hov_results.RData")


