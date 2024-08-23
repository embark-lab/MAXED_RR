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


corrs_cleaned <- cors_long[!cors_long$variable1 %in% c("=== + AFFECT ===", pos_affect_variables, '=== - AFFECT ===', neg_affect_variables, '=== EXERTION ===', exertion, '=== BIOMARKERS ===', key_biomarker_variables, "=== BISS === ") ,]

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
  labs(title = "Correlation Matrix of Body Image Response Data with
      Key Variables of Interest across Control and ED groups", 
      caption = "Note: Correlation matrix of BISS exercise Response with key variables \n
        * p < 0.05.",
       x = 'BISS Exercise Response') +
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
        plot.title = element_text(size = 18)) 
p

# save as 10 high x 5 wide
ggsave("figs/5.correlations/3.3.4.Corr_Plot_Mini.png", p, height = 10, width = 8, units = "in", dpi = 300)
