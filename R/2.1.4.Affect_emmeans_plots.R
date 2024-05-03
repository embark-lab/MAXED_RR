
source("R/source/0.Packages.R")
# Plot emmeans for affect

# Load the data
load("results/affect_models.RData")

emmeans <- as.data.frame(affect_emmeans)

# Pivot Longer
emmeans <- emmeans |> 
  pivot_longer(
    cols = everything(), # Pivot all columns
    names_to = c("variable", "task", ".value"), # Extract variable, task, and rest as value
    names_pattern = "([^.]*)\\.([^.]*)\\.(.*)" # Define pattern for splitting column names
  ) 

emmeans$variable <- factor(emmeans$variable, levels = c("Crummy", "Fatigued", "Calm", "Enthusiastic"))


emmeans_prescribed_plot <- ggplot(emmeans |> filter(task == 'Prescribed'), aes(x = time, y = emmean, color = group_factor)) +
  geom_point() +
  geom_line(aes(linetype = as.factor(condition))) +
  facet_wrap(~variable) +
  theme_bw() +
  labs(x = "Time (minutes)", 
       y = "Estimated Marginal Means", 
       title = "Estimated Marginal Means of Affect Measures During \n Prescribed Exercise",
       linetype = "Condition",  # Rename linetype legend
       color = "Group") +  
  scale_color_manual(name = 'Group', values = embark_palette()) +
  scale_linetype_manual(values = c("Rest" = "dashed", "Exercise" = "solid")) +
  embark_theme_a

# save the plot
save(emmeans_prescribed_plot, file = "figs/2.affect/affect_emmeans_prescribed.RData")
ggsave("figs/2.affect/affect_emmeans_prescribed.png", width = 10, height = 8)


emmeans_selfpaced_plot <- ggplot(emmeans |> filter(task == 'SelfPaced'), aes(x = time, y = emmean, color = group_factor)) +
  geom_point() +
  geom_line(aes(linetype = condition)) +
  facet_wrap(~variable) +
  theme_bw() +
  labs(x = "Time (minutes)", 
       y = "Estimated Marginal Means", 
       title = "Estimated Marginal Means of Affect Measures During \n Self-Paced Exercise",
       linetype = "Condition",  # Rename linetype legend
       color = "Group") +  scale_color_manual(name = 'Group', values = embark_palette()) +
  scale_linetype_manual(values = c("Rest" = "dashed", "Exercise" = "solid")) +
  embark_theme_a

# save the plot
save(emmeans_selfpaced_plot, file = "figs/2.affect/affect_emmeans_selfpaced.RData")

# Save the plots
ggsave("figs/2.affect/affect_emmeans_selfpaced.png", width = 10, height = 8)

