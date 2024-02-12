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

load(file = 'data/Affect/MAXED_Affect.RData')

# Settings and Themes

transparent_theme <- theme(
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA),
  strip.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.box.background = element_rect(fill = "transparent", colour = NA)
)

theme_1 <- 
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 18, family = "Avenir", face = "bold"),
    legend.position = "top",
    plot.background = element_rect(fill = 'transparent', colour = "#1a4e66", size = 3))


custom_colors <- c("ED" ="#fc6d46", "Control" =  "#1a4e66")

pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', "MAXED_1011", "MAXED_1012")

negative_emotions <- c("Crummy", "Fatigued")
positive_emotions <- c("Calm", "Enthusiastic")

positive_fill <-"#c2b824"  # Color for negative emotions
negative_fill <- "darkgrey"  # Color for positive emotions

combine_plots <- function(plot_neg, plot_pos, title_text) {
  caption_text <- "Note: Responses rated every 5 minutes on 5-point Likert scale from \n 0 (Do Not Feel) to 4 (Feel Very Strongly)"
  wrapped_caption <- str_wrap(caption_text, width = 80)  
  
  combined_plot <- plot_neg + plot_pos + plot_layout(ncol = 1) +
    plot_annotation(title = title_text,
                    caption = wrapped_caption,
                    theme = theme(text = element_text(size = 18, family = 'Garet', face = "bold", color = '#1A4F66'))) &
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0, size = 14)) +
    theme(panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
          plot.background = element_rect(fill = 'transparent', colour = 'transparent'))  # added this line
  
  return(combined_plot)
}


affect_plot_ex_df <- Affect %>% 
  filter(task %in% c('Prescribed', 'SelfPaced') & variable != 'Percieved Exertion' & condition == 'Exercise')

custom_linetypes <- c("SelfPaced" = "dotted", "Prescribed" = "dashed")

# Plot for negative emotions
plot_neg_ex <- ggplot(affect_plot_ex_df %>% filter(variable %in% negative_emotions), 
                      aes(x = time, y = value, group = interaction(group_factor, task), color = group_factor, linetype = task)) +
  geom_smooth(aes(fill = group_factor), size = 2, alpha = 0.2) +
  facet_wrap(~variable) +
  labs(color = 'Group',
       linetype = 'Ex Condition') +
  theme_minimal() +
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_blank(),
    axis.text = element_text(size = 16, family = "Avenir"),
    strip.background = element_rect(fill = negative_fill),
    strip.text = element_text(color = 'white', face = 'bold', family = "Avenir"),
    legend.position = "top",
    legend.title = element_text(face = "bold", family = "Avenir")) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) 

# Plot for positive emotions
plot_pos_ex <- ggplot(affect_plot_ex_df %>% filter(variable %in% positive_emotions), 
                      aes(x = time, y = value, group = interaction(group_factor, task), color = group_factor, linetype = task)) +
  geom_smooth(aes(fill = group_factor), size = 2, alpha = 0.2) +
  facet_wrap(~variable) +
  labs(x = "Time (mins)",
       linetype = 'Ex Condition', 
       y = "") +   # Set y-axis title
  theme_minimal() +  
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_blank(),
    axis.text = element_text(size = 16, family = "Avenir"),
    strip.background = element_rect(fill = positive_fill),
    strip.text = element_text(color = 'white', face = 'bold', family = "Avenir"),
    legend.position = 'none'
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) 


# Combine the plots with patchwork
affect_plot_ex <- combine_plots(plot_neg_ex, plot_pos_ex, "Affect Over Time During Exercise")

ggsave(affect_plot_ex, file = 'figs/2.affect/affect_plot_ex.png',height = 6, width = 8, units = "in", bg = 'transparent')

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
    filter(variable == var, 
           study_visit != 'c') %>%
    group_by(id, task) %>%
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

# Initialize a list to store the plots
plot_list <- list()

for (var in vars) {
  # Compute Cohen's d for each variable
  d_p <- cohen.d(data = df_list[[var]] %>% filter(task == 'Prescribed'), var_change_30 ~ group_factor)
  d_sp <- cohen.d(data = df_list[[var]] %>% filter(task == 'SelfPaced'), var_change_30 ~ group_factor)
  
  # Create a plot for each variable
  bp <- ggplot(df_list[[var]], aes(x = task, y = var_change_30, color = group_factor, fill = group_factor, alpha = 0.2)) +
    geom_point() +
    geom_boxplot() +
    scale_color_manual(name = 'Group', values = custom_colors) +
    scale_fill_manual(name = "Group", values = custom_colors) +
    labs(
      y = '', 
      title = var, # Using the variable name as the title
      x = ''
    ) +
    theme_1  +
    guides(alpha = FALSE) +
    ylim(-3,6) +
    geom_text(aes(
      label = ifelse(!is.na(d_p$estimate), paste("d =", round(d_p$estimate*-1, 2)), NA), 
      x= task, y = 3.5),
      vjust = -1, size = 5, fontface = 'bold', family = 'Avenir', 
      color = ifelse(!is.na(d_p$estimate) < 0, "#fc6d46", "#1A4F66"),
      inherit.aes = FALSE)# Adjust ncol to your preference

  # Append the plot to the list
  plot_list[[var]] <- bp
}


bp <- ggplot(df_list$Enthusiastic, aes(x = task, y = var_change_30, color = group_factor, fill = group_factor)) +
  geom_point(alpha = 0.2) +
  geom_boxplot(alpha = 0.2) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  theme_1  

legend_grob <- get_legend(bp)
library(grid)
# Set all backgrounds in the legend grob to transparent
legend_grob$grobs <- lapply(legend_grob$grobs, function(x) {
  if("rect" %in% class(x)) {
    x$gp$fill <- "transparent"
  }
  return(x)
})


# Modify each plot in your plot_list:
plot_list<- lapply(plot_list, function(p) p + transparent_theme)

# Create a combined title
title_plot <- ggdraw() + 
  draw_label("Affective Changes during Exercsie (30m vs. BL)", fontface='bold', size=18)

# Create a plot for the legend
legend_plot <- get_legend(
  ggplot(df_list$Enthusiastic, aes(x = task, y = var_change_30, color = group_factor, fill = group_factor)) +
    geom_point(alpha = 0.2) +
    geom_boxplot(alpha = 0.2) +
    scale_color_manual(name = 'Group', values = custom_colors) +
    scale_fill_manual(name = "Group", values = custom_colors) +
    theme_1
)
library(cowplot)

# 2x2 grid of plots
plots_grid <- plot_grid(
  plot_list[[vars[2]]], plot_list[[vars[3]]],
  plot_list[[vars[4]]], plot_list[[vars[1]]],
  ncol = 2
)

# Now, combine the title, legend, and 2x2 grid
affect_change_plot <- plot_grid(
  title_plot, 
  legend_plot, 
  plots_grid, 
  ncol = 1,
  rel_heights = c(0.1, 0.05, 1)  # Adjust these values as needed for better alignment
) 

# View the combined plot
affect_change_plot

save_plot("figs/2.affect/affect_change_plot.png", affect_change_plot, base_height=10, base_width=15)


plot_list_variance <- list()


for (var in vars) {
  # Compute Cohen's d for each variable
  d_p <- cohen.d(data = df_list[[var]] %>% filter(task == 'Prescribed'), variance ~ group_factor)
  d_sp <- cohen.d(data = df_list[[var]] %>% filter(task == 'SelfPaced'), variance ~ group_factor)
  
  # Create a plot for each variable
  bp <- ggplot(df_list[[var]], aes(x = task, y = variance, color = group_factor, fill = group_factor, alpha = 0.2)) +
    geom_point() +
    geom_boxplot() +
    scale_color_manual(name = 'Group', values = custom_colors) +
    scale_fill_manual(name = "Group", values = custom_colors) +
    labs(
      y = '', 
      title = var, # Using the variable name as the title
      x = ''
    ) +
    guides(alpha = FALSE) +
    ylim(0,2)+
    # Annotating Cohen's d val
    annotate("text", x = 1, y = 1.5, 
             label = paste("d =", round(d_p$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
    annotate("text", x = 2, y = 1.5, 
             label = paste("d =", round(d_sp$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
    embark_theme_a + 
    theme(legend.position = 'none')
    
  
  # Append the plot to the list
  plot_list_variance[[var]] <- bp
}

# 2x2 grid of plots
plots_grid_variance <- plot_grid(
  plot_list_variance[[vars[2]]], plot_list_variance[[vars[3]]],
  plot_list_variance[[vars[4]]], plot_list_variance[[vars[1]]],
  ncol = 2
)

# Create a combined title
title_plot_variance <- ggdraw() + 
  draw_label("Variance in Affect over 30 minutes of Exercise Across Groups", fontface='bold', size=18)

# Now, combine the title, legend, and 2x2 grid
affect_change_plot_variance <- plot_grid(
  title_plot_variance, 
  legend_plot, 
  plots_grid_variance, 
  ncol = 1,
  rel_heights = c(0.1, 0.05, 1)  # Adjust these values as needed for better alignment
) 

save_plot("figs/2.affect/affect_variance_plot.png", affect_change_plot_variance, base_height=10, base_width=15)

save(df_list, file = "data/Affect/affect_plot_data.RData")

