
# Load BISS Data
load(file = 'data/BISS/biss_data.RData')

# Functions and Packages
source(file = 'R/source/0.Packages.R')
source(file = 'R/source/0.themes_and_settings.R')
source(file = 'R/source/Functions.R')
source(file = 'R/source/2.D_Plots.R')
source(file = 'R/source/2.RawChange_Plots.R')

# Plot settings for session
BISS <- BISS %>%
  filter(!id %in% pilot_ids)
vars <- unique(BISS$variable)
BISS$variable <- factor(BISS$variable, levels = c("Phys Attract", "Appearance", "Looks", "Shape", 'Weight', 'Avg Person', "Average" ))

biss_ribbon_plot <- ribbon_plot(
  data = BISS,
  time_var = "time",
  value_var = "value",
  group_var = "group_factor",
  task_var = "task",
  facet_var = "variable",
  plot_title = "Body Image Over Time During Exercise")

# save plot as png
ggsave(biss_ribbon_plot, filename = "figs/3.body_image/biss_plot_exercise.png", width = 10, height = 8)
# save ggplot object
saveRDS(biss_ribbon_plot, file = "results/biss_ribbon_plot.RDS")

# process data for d_plot

BISS_Plot_Data <- process_dplot_data(df = BISS, 
                                        vars = unique(BISS$variable),
                                        file_path = 'data/BISS/BISS_Data_List.RData')
# load BISS data List
load(file = 'data/BISS/BISS_Data_List.RData')
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

# Bind all the dataframes in the list into one
annotation_df <- bind_rows(annotation_df)
BISS_Plot_Data <- left_join(BISS_Plot_Data, annotation_df, by = c("variable", 'condition', 'task'))
BISS_Plot_Data$variable <- factor(BISS_Plot_Data$variable, levels = c("Phys Attract", "Appearance", "Looks", "Shape", 'Weight', 'Avg Person', 'Average'))

exercise_data <- BISS_Plot_Data|> 
  filter(condition == 'Exercise')

BISS_d_plot_exercise_30 <-  d_plot(df = exercise_data, task_var = "task", dv = "var_change_30", 
                                     group_var = "group_factor", cohen_d_var = "change_30_controlvED_d", 
                                     title = "Body Image Changes During Exercise (30 min vs BL) Across Groups")
# Save ggplot object
save(BISS_d_plot_exercise_30, file = "figs/3.body_image/biss_change_30_exercise.RData")
# Save .png file
save_plot("figs/3.body_image/biss_change_30_exercise.png", BISS_d_plot_exercise_30, base_height=10, base_width=15)

BISS_d_plot_exercise_variance <-  d_plot(df = exercise_data, task_var = "task", dv = "variance", 
                                           group_var = "group_factor", cohen_d_var = "variance_controlvED_d", 
                                           title = "Within Subject Body Image Variance During Exercise Across Groups")

# save ggplot object
save(BISS_d_plot_exercise_variance, file = "figs/3.body_image/biss_change_variance.RData")
save_plot("figs/3.body_image/biss_change_variance.png", 
          BISS_d_plot_exercise_variance, base_height=10, base_width=15)


