
process_dplot_data <- function(df, vars, file_path) {
  # Ensure data is being passed correctly
  print(head(data))
  
  df_list <- list()
  
  # Function to process each variable
  process_variable <- function() {
    library(dplyr)
    print(paste("Processing variable:", var))  # Debugging print
    dplyr::filter(df, variable == var) %>% 
      dplyr::group_by(id, task, condition) %>%
      dplyr::mutate(
        var_30 = ifelse(time == 30, value, NA_real_),
        max_var = max(value, na.rm = TRUE),
        min_var = min(value, na.rm = TRUE),
        bl_var = ifelse(time == 0, value, NA_real_),
        bl_var = dplyr::first(na.omit(bl_var)),
        var_30 = dplyr::first(na.omit(var_30)),
        var_change = max_var - bl_var,
        var_change_30 = var_30 - bl_var,
        var_change_min = bl_var - min_var,
        variance = var(value, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(id, condition, group_factor, task, max_var, bl_var, var_change, var_change_30, var_change_min, variance) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(var_change_30), !is.na(bl_var))
  }
  
  for (var in vars) {
    df_list[[var]] <- process_variable()
  }
  
  names(df_list) <- vars
  save(df_list, file = paste0(file_path))
  
  all_data <- dplyr::bind_rows(df_list, .id = "variable")
  all_data <- dplyr::filter(all_data, !is.na(bl_var))
  
  return(all_data)
}


d_plot <- function (df, task_var, dv, group_var, cohen_d_var, title) {
  # Convert strings to symbols for tidy evaluation
  task_var_sym <- rlang::sym(task_var)
  dv_sym <- rlang::sym(dv)
  group_var_sym <- rlang::sym(group_var)
  cohen_d_var_sym <- rlang::sym(cohen_d_var)
  ymin <- min(df[[dv_sym]], na.rm = TRUE)
  ymax <- max(df[[dv_sym]], na.rm = TRUE)
  n_vars <- length(unique(df$variable))
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
    ylim(ymin, ymax) +
    theme(legend.position = 'none') +
    facet_wrap(~ variable, scales = "free", ncol = ceiling(sqrt(n_vars))) +
    geom_text(data = text_data,
              aes(label = paste("d =", round(!!cohen_d_var_sym, 2)),
                  x = !!task_var_sym, y = (ymax*.7)),
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

# # Example call to the function
#  d_plot(df = exercise_data, task_var = "task", dv = "var_change_30",
#                group_var = "group_factor", cohen_d_var = "change_30_controlvED_d",
#                title = "Affect Changes During Exercise (30 min vs BL) Across Groups")
