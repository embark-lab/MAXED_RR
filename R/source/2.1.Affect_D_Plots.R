affect_d_plot <- function (df, task_var, dv, group_var, cohen_d_var, title) {
  # Convert strings to symbols for tidy evaluation
  task_var_sym <- rlang::sym(task_var)
  dv_sym <- rlang::sym(dv)
  group_var_sym <- rlang::sym(group_var)
  cohen_d_var_sym <- rlang::sym(cohen_d_var)
  
  # Prepare the filtered data for geom_text outside the ggplot call to avoid '.' confusion
  text_data <- df[!is.na(df[[cohen_d_var]]), ]
  
  # Building the plot using tidy evaluated variables
  plot <- ggplot(df, aes(x = !!task_var_sym, y = !!dv_sym, fill = !!group_var_sym, alpha = 0.2)) +
    geom_point() +
    geom_boxplot() +
    scale_color_manual(name = 'Task', values = embarktools::embark_palette()) +
    scale_fill_manual(name = "Group", values = custom_colors) +
    labs(y = '', x = '') +
    guides(alpha = FALSE) +
    ylim(-3, 5) +
    theme(legend.position = 'none') +
    facet_wrap(~ variable, scales = "free", ncol = 2) +
    geom_text(data = text_data,
              aes(label = paste("d =", round(!!cohen_d_var_sym, 2)),
                  x = 1, y = 3.5),
              vjust = -1, size = 8, fontface = 'bold', family = 'Avenir',
              color = ifelse(text_data[[cohen_d_var]] > 0, "#fc6d46", "#1A4F66"),
              inherit.aes = FALSE) +
    embarktools::embark_theme_a +
    theme(strip.text = element_text(color = "#1A4F66", size = 20, family = 'Avenir'), 
          plot.title = element_text(size = 24, family = 'Avenir'),
          strip.background = element_rect(fill = "grey90"), 
          legend.text = element_text(size = 20, family = 'Avenir')) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    labs(title = title)
  
  return(plot)
}

# Example call to the function
affect_d_plot(df = exercise_data, task_var = "task", dv = "var_change_30", 
              group_var = "group_factor", cohen_d_var = "change_30_controlvED_d", 
              title = "Affect Changes During Exercise (30 min vs BL) Across Groups")
