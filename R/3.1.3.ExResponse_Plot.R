library(dplyr)
library(ggplot2)

load('data/Exercise_Response/Exercise_Response_Summary_Data.RData')

load('results/Corr_CI_Results_control.RData')
load('results/Corr_CI_Results_ed.RData')

cor_ci_results_ed$group <- 'ED'
cor_ci_results_control$group <- 'Control'

cor_ci_results <- rbind(cor_ci_results_ed, cor_ci_results_control)

key_biomarker_variables <- c('Leptin_ResidChange', 
                             'BDNF_ResidChange', 
                             'Cortisol_ResidChange')

key_BISS_variables <- c('Average_P', 
                        'Average_SP',
                        'Weight_P', 
                        'Weight_SP',
                        'Shape_P',
                        'Shape_SP')

key_affect_variables <- c('Calm_P', 
                          'Calm_SP',
                          'Enthusiastic_P',
                          'Enthusiastic_SP',
                          'Fatigued_P',
                          'Fatigued_SP',
                          'Crummy_P',
                          'Crummy_SP',
                          'Percieved_Exertion_SP',
                          'Percieved_Exertion_P'
)

# combine a list of all key vars
key_variables <- c(key_biomarker_variables, key_BISS_variables, key_affect_variables)


cor_ci_results.1 <- cor_ci_results |>
  filter(var1 %in% key_variables & var2 %in% key_variables)


cor_ci_results.1$var1 <- factor(cor_ci_results.1$var1, levels = c(key_variables))
cor_ci_results.1$var2 <- factor(cor_ci_results.1$var2, levels = c(key_variables))

# Create a reversed copy
cor_ci_results_reversed <- cor_ci_results.1 %>%
  rename(var1_old = var1, var2_old = var2) %>%
  rename(var1 = var2_old, var2 = var1_old) 

# Combine original and reversed data, adding group back
cor_ci_results_full <- rbind(cor_ci_results.1, cor_ci_results_reversed)

# Assign upper and lower triangles
data_upper_triangle <- cor_ci_results_full %>%
  filter(group == 'Control' & as.numeric(var1) < as.numeric(var2))

data_lower_triangle <- cor_ci_results_full %>%
  filter(group == 'ED' & as.numeric(var1) > as.numeric(var2))

# Combine the two triangles
cor_ci_results_full <- rbind(data_lower_triangle, data_upper_triangle)

# add NA for the diagonal
# first add rows for the diagonal -- use each key variable with itself
diagonal_data <- data.frame(var1 = key_variables, var2 = key_variables, estimate = NA, ci_lower = NA, ci_upper = NA, p_value = NA, group = NA)

# add the diagonal data to the full data
cor_ci_results_full <- rbind(cor_ci_results_full, diagonal_data)


library(ggplot2)
library(dplyr)

# Assuming cor_ci_results_full already loaded and prepared

# Define a function to determine significance labels
get_significance_label <- function(p_value) {
  if (is.na(p_value)) {
    return("")
  } else if 
  (p_value < 0.05) {
    return("*")
  } else {
    return()
  }
}

# Apply the function to create a new column for significance labels
cor_ci_results_full <- cor_ci_results_full %>%
  mutate(significance = sapply(p_value, get_significance_label))

# save the full corr table for display
save(cor_ci_results_full, file = 'results/Exercise_Response_Correlation_Matrix.RData')

# Plotting
Exercise_response_corrplot <- ggplot(cor_ci_results_full, aes(x = as.factor(var1), y = as.factor(var2), fill = estimate)) +
  geom_tile(color = "white") +  # Tiles for correlation coefficients
  geom_text(aes(label = significance), color = 'purple', size = 12, vjust = 0.8) +  # Add significance labels
  scale_fill_gradient2(low = embark_colors[1], high = embark_colors[2], mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Matrix of Exercise Response Data \n across Control and ED groups",
       caption = "Note: Correlation matrix of key variables in exercise response data. P = Prescribed, SP = Self-Paced; 
       Variables represent Slopes (Body Image and Affect) and residualized change scores (Biomarkers).
       ED group presented below the diaginol; Control is above
       * = significant at p < 0.5") +
  embark_theme_a +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.position = "right",
        plot.caption = element_text(size = 9),
        legend.title = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank()) 

ggsave("figs/5.correlations/Exercise_Response_Correlation_Matrix.png", width = 10, height = 10, units = "in", dpi = 300)

# save the ggplot object itself
save(Exercise_response_corrplot, file = 'figs/5.correlations/Exercise_Response_Corrplot.RData')

