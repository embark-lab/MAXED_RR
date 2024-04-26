library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)
library(ggplot2)
library(patchwork)



affect_plot_a_df <- affect %>% 
  filter(day == 'Self-Paced' & variable != 'Percieved Exertion' & condition == 'Exercise')

negative_emotions <- c("Crummy", "Fatigued")
positive_emotions <- c("Calm", "Enthusiastic")

positive_fill <-"#c2b824"  # Color for negative emotions
negative_fill <- "darkgrey"  # Color for positive emotions

custom_colors <- c("ED" = "#1a4e66", "Control" = "#fc6d46")


# Plot for negative emotions
plot_neg_a <- ggplot(affect_plot_a_df %>% filter(variable %in% negative_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor, fill = group_factor)) +
  geom_smooth(method = "lm", size = 2, linetype = 'dashed', alpha = 0.2) +
  facet_wrap(~variable) +
  labs(color = 'Group') +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "Avenir"), 
        axis.title.x = element_blank(),  
        axis.title.y = element_blank(),  
        axis.text.x = element_blank(),   
        axis.text.y = element_text(size = 16),
        strip.background = element_rect(fill = negative_fill),
        strip.text = element_text(color = 'white', face = 'bold'),
        legend.position = 'top',
        legend.title = element_text(face = "italic")) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  ylim(-0.5, 3)

# Plot for positive emotions
plot_pos_a <- ggplot(affect_plot_a_df %>% filter(variable %in% positive_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor, fill = group_factor)) +
  geom_smooth(method = "lm", size = 2, linetype = 'dashed', alpha = 0.2) +
  facet_wrap(~variable) +
  labs(x = "Time (mins)") +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "Avenir"), 
        axis.title  = element_text(size = 14), 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16),
        strip.background = element_rect(fill = positive_fill),
        strip.text = element_text(color = 'white', face = 'bold'),
        legend.position = 'none') +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  ylim(-0.5,3)


# Combine the plots with patchwork

# Wrap the caption
caption_text <- "Note: Responses rated every 5 minutes on 5-point Likert scale from \n 0 (Do Not Feel) to 4 (Feel Very Strongly)"
wrapped_caption_a <- str_wrap(caption_text, width = 100)  

affect_plot_a <- combine_plots(plot_neg_a, plot_pos_a, "Affect Over Time During Self-Paced Exercise")

affect_plot_a

ggsave(file = 'figs/2.affect/Affect_DayA.png')

affect_plot_b_df <- affect %>% 
  filter(day == 'Prescribed' & variable != 'Percieved Exertion' & condition == 'Exercise')

# Styling adjustments
# Plot for negative emotions
plot_neg_b <- ggplot(affect_plot_b_df %>% filter(variable %in% negative_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor, fill = group_factor)) +
  geom_smooth(method = "lm", size = 2, linetype = 'dashed', alpha = 0.2) +
  facet_wrap(~variable) +
  labs(color = 'Group') +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "Avenir"), 
        axis.title.x = element_blank(),  
        axis.title.y = element_blank(),  
        axis.text.x = element_blank(),   
        axis.text.y = element_text(size = 16),
        strip.background = element_rect(fill = negative_fill),
        strip.text = element_text(color = 'white', face = 'bold'),
        legend.position = 'top',
        legend.title = element_text(face = "bold")) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  ylim(-0.5, 3)


# Plot for positive emotions
plot_pos_b <- ggplot(affect_plot_b_df %>% filter(variable %in% positive_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor, fill = group_factor)) +
  geom_smooth(method = "lm", size = 2, linetype = 'dashed', alpha = 0.2) +
  facet_wrap(~variable) +
  labs(x = "Time (mins)") +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "Avenir"), 
        axis.title  = element_text(size = 14), 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16),
        strip.background = element_rect(fill = positive_fill),
        strip.text = element_text(color = 'white', face = 'bold'),
        legend.position = 'none') +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  ylim(-0.5,3)

# Combine the plots with patchwork
affect_plot_b <- combine_plots(plot_neg_b, plot_pos_b, "Affect Over Time During Prescribed Exercise")

print(affect_plot_b)

# Combine the plots with patchwork
affect_plot_b <- combine_plots(plot_neg_b, plot_pos_b, "Affect Over Time During Prescribed Exercise")

print(affect_plot_b)


ggsave(file = 'figs/2.affect/Affect_DayB.png')



affect_plot_c_df <- affect %>% 
  filter(day == 'Self-Paced', variable != 'Percieved Exertion' & condition == 'Rest')

# Plot for negative emotions
plot_neg_c <- ggplot(affect_plot_c_df %>% filter(variable %in% negative_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor)) +
  geom_smooth(method = "lm", size = 2) +
  facet_wrap(~variable) +
  labs(color = 'Group') +
  theme_minimal() +
  theme(text = element_text(size = 18), 
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.title.y = element_blank(),  # Hide y-axis title for this plot
        axis.text.x = element_blank(),   # Hide x-axis text/ticks
        strip.background = element_rect(fill = negative_fill),
        strip.text = element_text(color = 'white', face = 'bold')) +
  scale_color_manual(values = custom_colors)+
  ylim(-0.5, 3)

# Plot for positive emotions
plot_pos_c <- ggplot(affect_plot_c_df %>% filter(variable %in% positive_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor)) +
  geom_smooth(method = "lm", size = 2) +
  facet_wrap(~variable) +
  labs(x = "Time (mins)") +   # Set y-axis title
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.title  = element_text(size = 12), 
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = positive_fill),
        strip.text = element_text(color = 'white', face = 'bold'),
        legend.position = 'none') +
  scale_color_manual(values = custom_colors) +
  ylim(-0.5,3)

# Combine the plots with patchwork
affect_plot_c <- combine_plots(plot_neg_c, plot_pos_c, "Affect Over Time During Rest")

ggsave(affect_plot_c, file = 'figs/2.affect/Rest_Affect.png')

affect_plot_ex_df <- affect %>% 
  filter(day %in% c('Prescribed', 'Self-Paced') & variable != 'Percieved Exertion' & condition == 'Exercise')


custom_linetypes <- c("Self-Paced" = "dotted", "Prescribed" = "dashed")

# Plot for negative emotions
plot_neg_ex <- ggplot(affect_plot_ex_df %>% filter(variable %in% negative_emotions), 
                      aes(x = time, y = value, group = interaction(group_factor, day), color = group_factor, linetype = day)) +
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
                      aes(x = time, y = value, group = interaction(group_factor, day), color = group_factor, linetype = day)) +
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

ggsave(plot_neg_ex, file = 'figs/2.affect/neg_plot.png')


# Combine the plots with patchwork
affect_plot_ex <- combine_plots(plot_neg_ex, plot_pos_ex, "Affect Over Time During Exercise")


ggsave(affect_plot_ex, file = 'figs/2.affect/affect_plot_ex.png',height = 6, width = 8, units = "in", bg = 'transparent')

library(gridExtra)
library(cowplot)
library(ggplot2)
library(grid)
library(gridSVG)

theme_1 <- 
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 18, family = "Avenir", face = "bold"),
    legend.position = "top",
    plot.background = element_rect(fill = 'transparent', colour = "#1a4e66", size = 3))


load(file = 'data/affect_data/affect_data.RData')
custom_colors <- c("ED" = "#1a4e66", "Control" = "#fc6d46")


pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', "MAXED_1011", "MAXED_1012")

# Filter data not in pilot_ids and where variable is not 'Percieved Exertion'
affect <- affect %>%
  filter(!record_id %in% pilot_ids) %>%
  filter(variable != 'Percieved Exertion')

vars <- unique(affect$variable)

# Initialize an empty list to store the data frames
df_list <- list()

for (var in vars) {
  # For each unique variable, filter, group, mutate and then store the resultant data frame in the list
  df_temp <- affect %>%
    filter(variable == var, 
           day != 'c') %>%
    group_by(record_id, day) %>%
    mutate(var_30 =  ifelse(time == 30, value, NA_real_),
           max_var = max(value, na.rm = TRUE),
           bl_var = ifelse(time == 0, value, NA_real_)) %>%
    mutate(bl_var = first(na.omit(bl_var)), 
           var_30 = first(na.omit(var_30))) %>% 
    mutate(var_change = max_var - bl_var) |> 
    mutate(var_change_30 = var_30 - bl_var) |> 
    select(record_id, group_factor, day, max_var, bl_var, var_change, var_change_30) |> 
    ungroup() %>%
    distinct()
  
  # Append the data frame to the list
  df_list[[var]] <- df_temp
}


# Initialize a list to store the plots
plot_list <- list()

for (var in vars) {
  # Compute Cohen's d for each variable
  d_p <- cohen.d(data = df_list[[var]] %>% filter(day == 'Prescribed'), var_change_30 ~ group_factor)
  d_sp <- cohen.d(data = df_list[[var]] %>% filter(day == 'Self-Paced'), var_change_30 ~ group_factor)
  
  # Create a plot for each variable
  bp <- ggplot(df_list[[var]], aes(x = day, y = var_change_30, color = group_factor, fill = group_factor, alpha = 0.2)) +
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
    # Annotating Cohen's d val
    annotate("text", x = 1, y = 3, 
             label = paste("d =", round(d_p$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
    annotate("text", x = 2, y = 3, 
             label = paste("d =", round(d_sp$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
    theme(legend.position = 'none')
  
  # Append the plot to the list
  plot_list[[var]] <- bp
}
library(gridExtra)
library(cowplot)

bp <- ggplot(df_list$Enthusiastic, aes(x = day, y = var_change_30, color = group_factor, fill = group_factor)) +
  geom_point(alpha = 0.2) +
  geom_boxplot(alpha = 0.2) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  theme_1  

bp

legend_grob <- get_legend(bp)
library(grid)
# Set all backgrounds in the legend grob to transparent
legend_grob$grobs <- lapply(legend_grob$grobs, function(x) {
  if("rect" %in% class(x)) {
    x$gp$fill <- "transparent"
  }
  return(x)
})



transparent_theme <- theme(
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA),
  strip.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.box.background = element_rect(fill = "transparent", colour = NA)
)

# Modify each plot in your plot_list:
plot_list<- lapply(plot_list, function(p) p + transparent_theme)

# Create a combined title
title_plot <- ggdraw() + 
  draw_label("Affective Changes during Exercsie (30m vs. BL)", fontface='bold', size=18)

# Create a plot for the legend
legend_plot <- get_legend(
  ggplot(df_list$Enthusiastic, aes(x = day, y = var_change_30, color = group_factor, fill = group_factor)) +
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





