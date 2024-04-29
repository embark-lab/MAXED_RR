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

# Load BISS Data
load(file = 'data/BISS/biss_data.RData')

# Plot settings for session
source(file = 'R/source/0.themes_and_settings.R')
custom_colors <- c("#fc6d46","#1a4e66")
BISS <- BISS %>%
  filter(!id %in% pilot_ids)
vars <- unique(BISS$variable)
BISS$variable <- factor(BISS$variable, levels = c("Phys Attract", "Appearance", "Looks", "Shape", 'Weight', 'Avg Person', "Average" ))

theme_no_x_text <- function() {
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
}


create_biss_plot <- function(data, condition_1, plot_title) {
  # Filter data based on the specified condition
  filtered_data <- data |> filter(condition == condition_1)
  # Creating the plot
  biss_plot <- ggplot(filtered_data, aes(x = time, y = value, group = interaction(group_factor, task), color = group_factor, linetype = task)) +
    geom_smooth(linewidth = 1, aes(fill = group_factor), alpha = 0.2) +
    facet_wrap(~variable) +
    scale_color_manual(name = 'Group', values = custom_colors) +
    scale_fill_manual(name = 'Group', values = custom_colors) +
    scale_linetype_manual(values = custom_linetypes, 
                          guide = guide_legend(override.aes = list(color = "black"))) +
    labs(x = 'Time', 
         y = "Body Image Valence (1-9)", 
         linetype = 'Task',
         title = plot_title,
         caption = "Note: Responses rated every 5 minutes on 9-point Likert scale from 1 (extremely dissatisfied/negative) to \n 9 (extremely satisfied/positive)") +
    theme_minimal() +
    theme(text = element_text(size = 16, family = "Avenir"),
          axis.title = element_text(size = 14, family = "Avenir"),
          axis.text = element_text(size = 14, family = "Avenir"),
          strip.background = element_rect(fill = 'darkgrey'),
          strip.text = element_text(color = 'white', face = 'bold', family = "Avenir"),
          legend.position = 'top',
          legend.title = element_text(face = "bold", family = "Avenir"),
          plot.title = element_text(hjust = 0.5, family = "Avenir", face = "bold"),
          plot.caption = element_text(hjust = 0, size = 14, family = "Avenir"),
          plot.title.position = "plot") +
    theme(panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
          plot.background = element_rect(fill = 'transparent', colour = 'transparent'))
  
  return(biss_plot)
}

biss_plot_exercise <- create_biss_plot(BISS, 'Exercise', 'Body Image States Scale Scores Over Time During Exercise')
ggsave(file = 'figs/3.body_image/biss_plot_exercise.png', height = 10, width =10)

biss_plot_rest <- create_biss_plot(BISS, 'Rest', 'Body Image States Scale Scores Over Time During Rest')
ggsave(file = 'figs/3.body_image/biss_plot_rest.png')



# Initialize an empty list to store the data frames
df_list <- list()

for (var in vars) {
  # For each unique variable, filter, group, mutate and then store the resultant data frame in the list
  df_temp <- BISS %>%
    filter(variable == var) %>%
    group_by(id, task, condition) %>%
    mutate(var_30 =  ifelse(time == 30, value, NA_real_),
           max_var = max(value, na.rm = TRUE),
           bl_var = ifelse(time == 0, value, NA_real_)) %>%
    mutate(bl_var = first(na.omit(bl_var)), 
           var_30 = first(na.omit(var_30))) %>% 
    mutate(var_change = max_var - bl_var) |> 
    mutate(var_change_30 = var_30 - bl_var) |> 
    mutate(var_change_min = bl_var - min(value, na.rm = TRUE)) |>
    mutate(variance = var(value, na.rm = TRUE)) %>%
    select(id, group_factor, task, max_var, bl_var, var_change, var_change_30, var_change_min, variance) |> 
    ungroup() %>%
    distinct() |> 
    filter(!is.na(var_change_30))
  
  # Append the data frame to the list
  df_list[[var]] <- df_temp
}

all_data <- bind_rows(df_list, .id = "variable")
all_data$variable <- factor(all_data$variable, levels = c("Phys Attract", "Appearance", "Looks", "Shape", 'Weight', "Avg Person", 'Average'))

# Remove rows with NA values
all_data <- all_data |> 
  filter(!is.na(bl_var))

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
all_data <- left_join(all_data, annotation_df, by = c("variable", 'condition', 'task'))
all_data$variable <- factor(all_data$variable, levels = c("Phys Attract", "Appearance", "Looks", "Shape", 'Weight', 'Avg Person', 'Average'))

exercise_data <- all_data |> 
  filter(condition == 'Exercise')

biss_d_plot_exercise_30<- ggplot(exercise_data, aes(x = task, y = var_change_30, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Task', values = embarktools::embark_palette()) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = '', x = '') +
  guides(alpha = FALSE) +
  ylim(-3,5) +
  theme(legend.position = 'none') +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  geom_text(aes(
    label = ifelse(!is.na(change_30_controlvED_d), paste("d =", round(change_30_controlvED_d, 2)), NA), 
    x= task, y = 3.2),
    vjust = -1, size = 5, fontface = 'bold', family = 'Avenir', 
    color = ifelse(subset(exercise_data, !is.na(change_30_controlvED_d))$change_30_controlvED_d < 0, "#fc6d46", "#1A4F66"),
    inherit.aes = FALSE)# Adjust ncol to your preference

biss_d_plot_exercise_30 <- biss_d_plot_exercise_30 + 
  labs(title = "BISS Changes During Exercise (30min vs BL) Across Groups") +
  embarktools::embark_theme_a + 
  # add grey to the strip background
  theme(strip.background = element_rect(fill = "grey90")) + 
  theme(strip.text = element_text(color = "#1A4F66"))

biss_d_plot_exercise_30
ggsave(biss_d_plot_exercise_30, file = 'figs/3.body_image/biss_change_30_exercise.png', height = 10, width = 10)

biss_d_plot_exercise_var <- ggplot(exercise_data, aes(x = task, y = variance, fill = group_factor, alpha = 0.2)) +
  geom_point()  +
  geom_boxplot() +
  scale_color_manual(name = 'Task', values = embarktools::embark_palette()) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = '', x = '') +
  guides(alpha = FALSE) +
  ylim(0,3) +
  theme(legend.position = 'none') +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  geom_text(aes(
    label = ifelse(!is.na(variance_controlvED_d), paste("d =", round(variance_controlvED_d, 2)), NA), 
    x= task, y = 2),
    vjust = -1, size = 5, fontface = 'bold', family = 'Avenir', 
    color = ifelse(subset(exercise_data, !is.na(variance_controlvED_d))$variance_controlvED_d < 0, "#fc6d46", "#1A4F66"),
    inherit.aes = FALSE)# Adjust ncol to your preference

biss_d_plot_exercise_var <- biss_d_plot_exercise_var + 
  labs(title = "BISS Variance During Exercise Across Groups") +
  embarktools::embark_theme_a + 
  # add grey to the strip background
  theme(strip.background = element_rect(fill = "grey90")) + 
  theme(strip.text = element_text(color = "#1A4F66"))

biss_d_plot_exercise_var
ggsave(biss_d_plot_exercise_var, file = 'figs/3.body_image/biss_variance_exercise.png', height = 10, width = 10)
