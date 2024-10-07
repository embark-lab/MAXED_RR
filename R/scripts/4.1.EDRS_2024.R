source('R/source/0.Packages.R')
load('results/exercise_corrs_cleaned.RData')

# Remove + Affect, - Affect, Exertion, and Biomarker variables

neg_affect_variables <- c('Fatigued',
                          'Crummy')

pos_affect_variables <- c('Calm', 
                          'Enthusiastic')

key_biomarker_variables <- c('Leptin', 
                             'BDNF', 
                             'Cortisol')

exertion <- c('Percieved_Exertion')


corrs_cleaned <- cors_long[!cors_long$variable1 %in% c('=== EXERTION ===', exertion, '=== BIOMARKERS ===', key_biomarker_variables) ,]

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
corrs_cleaned <- corrs_cleaned %>%
  mutate(significance = sapply(p_value, get_significance_label))

# recode 'Average' and 'Weight' to be 'Avg' and 'Wt' 
corrs_cleaned$variable1 <- dplyr::recode(corrs_cleaned$variable1, 'Average' = 'Avg', 'Weight' = 'Wt')
# recode karvonen_max_intense, max_pct_hr, and max_hr to be 'Max Intense', 'Max % HR', and 'Max HR'
corrs_cleaned$variable2 <- dplyr::recode(corrs_cleaned$variable2, 'karvonen_max_intense' = 'Max Intense', 'max_pct_hr' = 'Max % HR', 'avg_pct_hr' = 'Avg HR', 'distance' = 'Distance')

p <- ggplot(corrs_cleaned, aes(x = variable1, y = variable2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = significance), color = 'purple', size = 12, vjust = 0.8) +  # Add significance labels
  scale_fill_gradient2(low = embark_colors[1], high = embark_colors[2], mid = "white", midpoint = 0) +
  theme_minimal() +
  facet_grid(vars(group), vars(Condition)) +
  labs(title = "Correlation Matrix of Exercise Response Data with
      Key Variables of Interest across Control and ED groups", 
       caption = "Note: LPA - light physical activity; MVPA - moderate-to-vigorous physcial activity; DFM - drive for muscularity; 
       FAMB - Functional assessment of maladaptive behavior - exercise; EDS - exercise dependence scale; 
       CET - compulsive exercise test;  * p < 0.05.",
       x = 'Exercise Response') +
  embark_theme_a +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text (size = 16),
        legend.position = "right",
        plot.caption = element_text(size = 9), 
        legend.title = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.grid.major = element_line(colour = "grey70"),
        panel.grid.minor = element_blank(), 
        axis.text.y.left = element_text(size = 12), 
        axis.text.y.right = element_blank(),
        axis.ticks.y.left = element_line(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18))  +
  # put x axis labels on 45 degree angle
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p

# save as 10 high x 5 wide
ggsave("figs/5.correlations/EDRS_2024.png", p, height = 10, width = 8, units = "in", dpi = 300)


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
                          'Crummy_SP'
)

# combine a list of all key vars
key_variables <- c(key_BISS_variables, key_affect_variables)


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

# Plotting
Exercise_response_corrplot <- ggplot(cor_ci_results_full, aes(x = as.factor(var1), y = as.factor(var2), fill = estimate)) +
  geom_tile(color = "white") +  # Tiles for correlation coefficients
  geom_text(aes(label = significance), color = 'purple', size = 12, vjust = 0.8) +  # Add significance labels
  scale_fill_gradient2(low = embark_colors[1], high = embark_colors[2], mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Matrix of Exercise Response Data \n across Control and ED groups",
       caption = "Note: Correlation matrix of key variables in exercise response data. P = Prescribed, SP = Self-Paced; 
       Variables represent slopes of Body Image (Average; Shape; Weight) and Affect variables.
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

ggsave("figs/5.correlations/EDRS_Exercise_Response_Correlation_Matrix.png", width = 10, height = 10, units = "in", dpi = 300)

# save the ggplot object itself
save(Exercise_response_corrplot, file = 'figs/5.correlations/EDRS_Exercise_Response_Corrplot.RData')




