
# Loop through the variables
for (t in Tasks) {
  for (var in Affect_Variables) {
    df <- Affect %>%
      filter(variable == var) |> 
      filter(task == t) |> 
      mutate(time = as.numeric(time))
    
    # run lmm
    affect_hov_full[[var]][[t]] <-  lmer(value ~1 + age + group_factor + time +
                                           bmi*condition + time*condition + time*group_factor*condition +  
                                           (1+group_factor*time|condition:id), data = df) 
    
    # save model warnings
    model_warnings[['hov_full']][[var]][[t]] <- capture.output(print(summary(affect_hov_full[[var]][[t]])))
    
    affect_hov_time_only[[var]][[t]] <-  lmer(value ~1 + age + group_factor + time + bmi*condition + time*condition + time*group_factor*condition +  (1+time|condition:id), data = df)
    model_warnings[['time_only']][[var]][[t]] <- capture.output(print(summary(affect_hov_time_only[[var]][[t]])))
    
    affect_hov_null[[var]][[t]] <-  lmer(value ~1 + age + group_factor + time + bmi*condition + time*condition + time*group_factor*condition +  (1|condition:id), data = df)
    model_warnings[['null']][[var]][[t]] <- capture.output(print(summary(affect_hov_null[[var]][[t]])))
    
    # compare models
    anova_1 <- anova(affect_hov_null[[var]][[t]], affect_hov_time_only[[var]][[t]])
    anova_2 <- anova(affect_hov_time_only[[var]][[t]], affect_hov_full[[var]][[t]])
    anova_results[[var]][[t]] <- list(anova_1, anova_2)
  }
}

