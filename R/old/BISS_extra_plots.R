prescribed_data <- all_data |> 
  filter(task == 'Prescribed')

biss_d_plot_prescribed <- ggplot(prescribed_data, aes(x = condition, y = var_change, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'condition', values = embarktools::embark_palette()) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = '', x = '') +
  guides(alpha = FALSE) +
  ylim(0,6) +
  # If you still want to annotate the plots, this becomes a bit trickier with facet_wrap.
  # You might need to find another way or adjust based on the specific needs and look of the final plots.
  theme(legend.position = 'none') +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  geom_text(aes(
    label = ifelse(!is.na(max_change_controlvED_d), paste("d =", round(max_change_controlvED_d, 2)), NA), 
    x= condition, y = 4.5),
    vjust = -1, size = 5, fontface = 'bold', family = 'Avenir', 
    color = ifelse(subset(prescribed_data, !is.na(max_change_controlvED_d))$max_change_controlvED_d < 0, "#fc6d46", "#1A4F66"),
    inherit.aes = FALSE)# Adjust ncol to your preference

biss_d_plot_prescribed <- biss_d_plot_prescribed + 
  labs(title = "BISS Changes During Prescribed Exercise and Rest across Groups") + 
  embarktools::embark_theme_a 

biss_d_plot_prescribed

ggsave(biss_d_plot_prescribed, file = 'figs/3.body_image/biss_change_prescribed.png')


exercise_data <- all_data |> 
  filter(condition == 'Exercise')
biss_d_plot_exercise<- ggplot(exercise_data, aes(x = task, y = var_change, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Task', values = embarktools::embark_palette()) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = '', x = '') +
  guides(alpha = FALSE) +
  ylim(0,6) +
  # If you still want to annotate the plots, this becomes a bit trickier with facet_wrap.
  # You might need to find another way or adjust based on the specific needs and look of the final plots.
  theme(legend.position = 'none') +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  geom_text(aes(
    label = ifelse(!is.na(max_change_controlvED_d), paste("d =", round(max_change_controlvED_d, 2)), NA), 
    x= task, y = 4.5),
    vjust = -1, size = 5, fontface = 'bold', family = 'Avenir', 
    color = ifelse(subset(exercise_data, !is.na(max_change_controlvED_d))$max_change_controlvED_d < 0, "#fc6d46", "#1A4F66"),
    inherit.aes = FALSE)# Adjust ncol to your preference

biss_d_plot_exercise <- biss_d_plot_exercise + 
  labs(title = "BISS Changes During Exercise (Max vs BL) Across Groups") +
  embarktools::embark_theme_a 

biss_d_plot_exercise

ggsave(biss_d_plot_exercise, file = 'figs/3.body_image/biss_change_max_exercise.png', height = 10, width = 10)



selfpaced_data <- all_data |> 
  filter(task == 'Self-Paced')
biss_d_plot_selfpaced <- ggplot(selfpaced_data, aes(x = condition, y = var_change, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'condition', values = embarktools::embark_palette()) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = '', x = '') +
  guides(alpha = FALSE) +
  ylim(0,6) +
  # If you still want to annotate the plots, this becomes a bit trickier with facet_wrap.
  # You might need to find another way or adjust based on the specific needs and look of the final plots.
  theme(legend.position = 'none') +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  geom_text(aes(
    label = ifelse(!is.na(max_change_controlvED_d), paste("d =", round(max_change_controlvED_d, 2)), NA), 
    x= condition, y = 4.5),
    vjust = -1, size = 5, fontface = 'bold', family = 'Avenir', 
    color = ifelse(subset(selfpaced_data, !is.na(max_change_controlvED_d))$max_change_controlvED_d < 0, "#fc6d46", "#1A4F66"),
    inherit.aes = FALSE)# Adjust ncol to your preference

biss_d_plot_selfpaced <- biss_d_plot_selfpaced + 
  labs(title = "BISS Changes During Self-Paced Exercise and Rest across Groups") +
  embarktools::embark_theme_a 

biss_d_plot_selfpaced

ggsave(biss_d_plot_selfpaced, file = 'figs/3.body_image/biss_change_selfpaced.png')

