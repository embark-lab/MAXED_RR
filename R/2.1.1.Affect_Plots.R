# Data
load(file = 'data/Affect/MAXED_Affect.RData')

# Functions and Packages
source(file = 'R/source/0.Packages.R')
source(file = 'R/source/0.themes_and_settings.R')
source(file = 'R/source/Functions.R')
source(file = 'R/source/2.D_Plots.R')
source(file = 'R/source/2.RawChange_Plots.R')

affect_plot_ex_df <- Affect %>% 
  filter(task %in% c('Prescribed', 'SelfPaced') 
         & variable != 'Percieved Exertion' 
         & condition == 'Exercise' 
         & !id %in% pilot_ids) |> 
  # recode 'SelfPaced' to 'Self-Paced'
  mutate(task = recode(task, 'SelfPaced' = 'Self-Paced')) 
affect_plot_ex_df$variable <- factor(affect_plot_ex_df$variable, levels = c("Enthusiastic", "Calm", "Crummy", "Fatigued"))


affect_ribbon_plot <- ribbon_plot(
  data = affect_plot_ex_df,
  time_var = "time",
  value_var = "value",
  group_var = "group_factor",
  task_var = "task",
  facet_var = "variable", 
  plot_title = "Affect Over Time During Exercise")


# save the ggplot object
save(affect_ribbon_plot, file = 'figs/2.affect/affect_plot_ex.RData')
# Save the plot
ggsave(affect_ribbon_plot, file = 'figs/2.affect/affect_plot_ex.png',height = 6, width = 8, 
       units = "in", bg = 'transparent')

# Filter data not in pilot_ids and where variable is not 'Percieved Exertion'
affect <- Affect %>%
  filter(!id %in% pilot_ids) %>%
  filter(variable != 'Percieved Exertion') |> 
  mutate(task = recode(task, 'SelfPaced' = 'Self-Paced')) 

Affect_plot_data <- process_dplot_data(df = affect, 
                   vars = unique(affect$variable),
                   file_path = 'data/Affect/Affect_data_list.RData')

load(file = 'data/Affect/Affect_data_list.RData')

# Initialize list to hold data frames for each variable
annotation_df <- list()
for (var in vars) {
  d_p_e <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'Prescribed'), var_change ~ group_factor)
  d_p_e_30 <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'Prescribed'), var_change_30 ~ group_factor)
  d_sp_e <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'Self-Paced'), var_change ~ group_factor)
  d_sp_e_30 <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'Self-Paced'), var_change_30 ~ group_factor)
  d_p_r <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Rest', task == 'Prescribed'), var_change ~ group_factor)
  d_p_r_30 <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Rest', task == 'Prescribed'), var_change_30 ~ group_factor)
  d_sp_r <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Rest', task == 'Self-Paced'), var_change ~ group_factor)
  d_sp_r_30 <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Rest', task == 'Self-Paced'), var_change_30 ~ group_factor)
  d_var_p_e <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'Prescribed'), variance ~ group_factor)
  d_var_sp_e <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'Self-Paced'), variance ~ group_factor)
  
  df_temp <- data.frame(
    variable = var,
    max_change_controlvED_d = c(d_p_e$estimate*-1, d_sp_e$estimate*-1, d_p_r$estimate*-1, d_sp_r$estimate*-1),
    change_30_controlvED_d = c(d_p_e_30$estimate*-1, d_sp_e_30$estimate*-1, d_p_r_30$estimate*-1, d_sp_r_30$estimate*-1),
    variance_controlvED_d = c(d_var_p_e$estimate*-1, d_var_sp_e$estimate*-1, NA, NA),
    task = c('Prescribed', 'Self-Paced', 'Prescribed', 'Self-Paced'),
    condition = c('Exercise', 'Exercise', 'Rest', 'Rest')
  )
  
  annotation_df[[var]] <- df_temp
}

annotation_df <- bind_rows(annotation_df)

Affect_plot_data <- left_join(Affect_plot_data, annotation_df, by = c("variable", 'condition', 'task'))

Affect_plot_data$variable <- factor(Affect_plot_data$variable, 
                                    levels = c("Enthusiastic", "Calm", "Crummy", "Fatigued"))
save(Affect_plot_data, file = "data/Affect/affect_plot_data.RData")

exercise_data <- Affect_plot_data |> 
  filter(condition == 'Exercise')

affect_d_plot_exercise_30 <-  d_plot(df = exercise_data, task_var = "task", dv = "var_change_30", 
                                           group_var = "group_factor", cohen_d_var = "change_30_controlvED_d", 
                                           title = "Affect Changes During Exercise (30 min vs BL) Across Groups")
# Save ggplot object
save(affect_d_plot_exercise_30, file = "figs/2.affect/affect_d_plot_exercise_30.RData")
# Save .png file
save_plot("figs/2.affect/affect_change_plot.png", affect_d_plot_exercise_30, base_height=10, base_width=15)

affect_d_plot_exercise_variance <-  d_plot(df = exercise_data, task_var = "task", dv = "variance", 
                                                  group_var = "group_factor", cohen_d_var = "variance_controlvED_d", 
                                                  title = "Within Subject Affective Variance During Exercise Across Groups")

# save ggplot object
save(affect_d_plot_exercise_variance, file = "figs/2.affect/affect_variance_plot.RData")
save_plot("figs/2.affect/affect_variance_plot.png", affect_d_plot_exercise_variance, base_height=10, base_width=15)


