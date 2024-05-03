# Initialize an empty list to store the data frames
df_list <- list()

for (var in vars) {
  # For each unique variable, filter, group, mutate and then store the resultant data frame in the list
  df_temp <- affect %>%
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

affect_data_list <- df_list
save(affect_data_list, file = "data/Affect/affect_data_list.RData")


all_data <- bind_rows(df_list, .id = "variable")
all_data$variable <- factor(all_data$variable, levels = c("Enthusiastic", "Calm", "Crummy", "Fatigued"))
all_data <- all_data |> 
  filter(!is.na(bl_var))

annotation_df <- list()

for (var in vars) {
  d_p_e <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'Prescribed'), var_change ~ group_factor)
  d_p_e_30 <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'Prescribed'), var_change_30 ~ group_factor)
  d_sp_e <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'SelfPaced'), var_change ~ group_factor)
  d_sp_e_30 <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'SelfPaced'), var_change_30 ~ group_factor)
  d_p_r <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Rest', task == 'Prescribed'), var_change ~ group_factor)
  d_p_r_30 <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Rest', task == 'Prescribed'), var_change_30 ~ group_factor)
  d_sp_r <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Rest', task == 'SelfPaced'), var_change ~ group_factor)
  d_sp_r_30 <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Rest', task == 'SelfPaced'), var_change_30 ~ group_factor)
  d_var_p_e <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'Prescribed'), variance ~ group_factor)
  d_var_sp_e <- effsize::cohen.d(data = df_list[[var]] %>% filter(condition == 'Exercise', task == 'SelfPaced'), variance ~ group_factor)
  
  
  
  df_temp <- data.frame(
    variable = var,
    max_change_controlvED_d = c(d_p_e$estimate*-1, d_sp_e$estimate*-1, d_p_r$estimate*-1, d_sp_r$estimate*-1),
    change_30_controlvED_d = c(d_p_e_30$estimate*-1, d_sp_e_30$estimate*-1, d_p_r_30$estimate*-1, d_sp_r_30$estimate*-1),
    variance_controlvED_d = c(d_var_p_e$estimate*-1, d_var_sp_e$estimate*-1, NA, NA),
    task = c('Prescribed', 'SelfPaced', 'Prescribed', 'SelfPaced'),
    condition = c('Exercise', 'Exercise', 'Rest', 'Rest')
  )
  
  annotation_df[[var]] <- df_temp
}

annotation_df <- bind_rows(annotation_df)
all_data <- left_join(all_data, annotation_df, by = c("variable", 'condition', 'task'))
all_data$variable <- factor(all_data$variable, levels = c("Enthusiastic", "Calm", "Crummy", "Fatigued"))
Affect_plot_data <- all_data
save(Affect_plot_data, file = "data/Affect/affect_plot_data.RData")
