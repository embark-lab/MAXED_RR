library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(cowplot)
library(grid)
library(gridSVG)
library(effsize)
library(embarktools)
library(cgwtools)

load(file = 'data/Affect/MAXED_Affect.RData')
source(file = 'R/source/0.themes_and_settings.R')
source(file = 'R/source/Functions.R')

affect_plot_ex_df <- Affect %>% 
  filter(task %in% c('Prescribed', 'SelfPaced') & variable != 'Percieved Exertion' & condition == 'Exercise')
custom_linetypes <- c("SelfPaced" = "dotted", "Prescribed" = "dashed")
affect_plot_ex_df$variable <- factor(affect_plot_ex_df$variable, levels = c("Enthusiastic", "Calm", "Crummy", "Fatigued"))

# Plot for negative emotions
affect_plot_ex <- ggplot(affect_plot_ex_df, 
                      aes(x = time, y = value, group = interaction(group_factor, task), 
                          color = group_factor, linetype = task)) +
  geom_smooth(aes(fill = group_factor), size = 1, alpha = 0.2) +
  facet_wrap(~variable) +
  labs(color = 'Group',
       linetype = 'Ex Condition') +
  theme_minimal() +
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.text = element_text(size = 16, family = "Avenir", color = '#1A4F66'),
    strip.background = element_rect(fill = 'grey90'),
    strip.text = element_text(color = "#1A4F66", face = 'bold', family = "Avenir"),
    legend.position = 'top'
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  # add title
  labs(title = "Affect Over Time During Exercise", 
       x = 'Time (mins)', 
       y = '') +
  # center title and make it bold
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = "Avenir", face = "bold", color ="#1A4F66")) 
 
ggsave(affect_plot_ex, file = 'figs/2.affect/affect_plot_ex.png',height = 6, width = 8, 
       units = "in", bg = 'transparent')

# Boxplots


# Filter data not in pilot_ids and where variable is not 'Percieved Exertion'
affect <- Affect %>%
  filter(!id %in% pilot_ids) %>%
  filter(variable != 'Percieved Exertion')

vars <- unique(affect$variable)

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


exercise_data <- all_data |> 
  filter(condition == 'Exercise')

affect_d_plot_exercise_30<- ggplot(exercise_data, aes(x = task, y = var_change_30, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Task', values = embarktools::embark_palette()) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = '', x = '') +
  guides(alpha = FALSE) +
  ylim(-3,5) +
  theme(legend.position = 'none') +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  geom_text(aes(
    label = ifelse(!is.na(change_30_controlvED_d), paste("d =", round(change_30_controlvED_d, 2)), NA), 
    x= task, y = 3.5),
    vjust = -1, size = 8, fontface = 'bold', family = 'Avenir', 
    color = ifelse(subset(exercise_data, !is.na(change_30_controlvED_d))$change_30_controlvED_d > 0, "#fc6d46", "#1A4F66"),
    inherit.aes = FALSE)# Adjust ncol to your preference

affect_d_plot_exercise_30 <- affect_d_plot_exercise_30 + 
  labs(title = "Affect Changes During Exercise (30 min vs BL) Across Groups") +
  embarktools::embark_theme_a +
  # make strip text bigger and add color
  theme(strip.text = element_text(color = "#1A4F66", 
                                  size = 20, 
                                  family = 'Avenir')) + 
  # Make the title bigger
  theme(plot.title = element_text(size = 24, family = 'Avenir')) +
  # make the strip background grey
  theme(strip.background = element_rect(fill = "grey90")) +
  # make legend text just a little bigger
  theme(legend.text = element_text(size = 20, family = 'Avenir')) +
  # add horizontal line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 

save_plot("figs/2.affect/affect_change_plot.png", affect_d_plot_exercise_30, base_height=10, base_width=15)

affect_d_plot_exercise_max<- ggplot(exercise_data, aes(x = task, y = var_change, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Task', values = embarktools::embark_palette()) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = '', x = '') +
  guides(alpha = FALSE) +
  ylim(-3,5) +
  theme(legend.position = 'none') +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  geom_text(aes(
    label = ifelse(!is.na(max_change_controlvED_d), paste("d =", round(max_change_controlvED_d, 2)), NA), 
    x= task, y = 3.5),
    vjust = -1, size = 10, fontface = 'bold', family = 'Avenir', 
    color = ifelse(subset(exercise_data, !is.na(max_change_controlvED_d))$max_change_controlvED_d > 0, "#fc6d46", "#1A4F66"),
    inherit.aes = FALSE)# Adjust ncol to your preference

affect_d_plot_exercise_max <- affect_d_plot_exercise_max + 
  labs(title = "Affect Changes During Exercise (Max vs BL) Across Groups") +
  embarktools::embark_theme_a +
  # make strip text bigger and add color
  theme(strip.text = element_text(color = "#1A4F66", 
                                  size = 12, 
                                  family = 'Avenir'), 
        element_rect(fill = "white", color = "black"))


affect_d_plot_exercise_variance <- ggplot(exercise_data, aes(x = task, y = variance, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Task', values = embarktools::embark_palette()) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = '', x = '') +
  guides(alpha = FALSE) +
  theme(strip.text = element_text(color = "#1A4F66")) +
  embarktools::embark_theme_a +
    ylim(0,1.5)+
    facet_wrap(~ variable, scales = "free", ncol = 2) +
    geom_text(aes(
      label = ifelse(!is.na(variance_controlvED_d), paste("d =", round(variance_controlvED_d, 2)), NA), 
      x= task, y = 1.2),
      vjust = -1, size = 8, fontface = 'bold', family = 'Avenir', 
      color = ifelse(subset(exercise_data, !is.na(variance_controlvED_d))$variance_controlvED_d > 0, "#fc6d46", "#1A4F66"),
      inherit.aes = FALSE) +
  labs(title = "Affect Variance During Exercise Across Groups") +
  # make strip text bigger and add color
  theme(strip.text = element_text(color = "#1A4F66", 
                                  size = 20, 
                                  family = 'Avenir')) + 
  # Make the title bigger
  theme(plot.title = element_text(size = 24, family = 'Avenir')) +
  # make the strip background grey
  theme(strip.background = element_rect(fill = "grey90")) +
  # make legend text just a little bigger
  theme(legend.text = element_text(size = 20, family = 'Avenir')) 



save_plot("figs/2.affect/affect_variance_plot.png", affect_d_plot_exercise_variance, base_height=10, base_width=15)

Affect_plot_data <- all_data

save(Affect_plot_data, file = "data/Affect/affect_plot_data.RData")

