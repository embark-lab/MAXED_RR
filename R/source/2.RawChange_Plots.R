ribbon_plot <- function(data, time_var, value_var, group_var, task_var, plot_title,
                               xlab = "Time (mins)", ylab = "", 
                               group_colors = c('ED' = "#fc6d46", 'Control' = "#1a4e66"),
                               linetypes = c("Self-Paced" = "dotted", "Prescribed" = "dashed"),
                               facet_var = NULL, legend_title = "Ex Condition") {
  # Set up the plot with correct aesthetic mappings
  p <- ggplot(data, aes_string(x = time_var, y = value_var, 
                               group = paste0("interaction(", group_var, ",", task_var, ")"),
                               color = group_var, fill = group_var, linetype = task_var)) +
    geom_smooth(size = 1, alpha = 0.2) +  # Apply transparency to the fill
    labs(color = 'Group', linetype = legend_title) +
    theme_minimal() +
    scale_linetype_manual(values = linetypes) +
    scale_color_manual(values = group_colors) +
    scale_fill_manual(values = group_colors) +  # Consistent color mapping for fill
    guides(
      linetype = guide_legend(override.aes = list(color = "black")),
      fill = guide_none()# Ensure line types are shown in black
    ) +
    labs(title = plot_title, x = xlab, y = ylab) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, family = "Avenir", face = "bold", color ="#1A4F66"),
      text = element_text(size = 18, family = "Avenir"),
      axis.text = element_text(size = 16, family = "Avenir", color = '#1A4F66'),
      strip.background = element_rect(fill = 'grey90'),
      strip.text = element_text(color = "#1A4F66", face = 'bold', family = "Avenir"),
      legend.position = 'top'
    )
  
  # Add facet_wrap if facet_var is provided
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)))
  }
  
  return(p)
}



# ribbon_plot(
#   data = affect_plot_ex_df,
#   time_var = "time",
#   value_var = "value",
#   group_var = "group_factor",
#   task_var = "task",
#   facet_var = "variable"
# )
# 
