
# Inserting dummy separators 
full_corr_data_withseps <- combined_correlation_data %>%
  # First separator
  rbind(data.frame(variable1 = "Separator1", variable2 = unique(combined_correlation_data$variable2), correlation = NA)) %>%
  rbind(data.frame(variable1 = unique(combined_correlation_data$variable1), variable2 = "Separator1", correlation = NA)) %>%
  # Second separator
  rbind(data.frame(variable1 = "Separator2", variable2 = unique(combined_corerelation_data$variable2), correlation = NA)) %>%
  rbind(data.frame(variable1 = unique(combined_correlation_data$variable1), variable2 = "Separator2", correlation = NA))


# Convert back to factors with the desired level ordering including the separator
full_corr_data_withseps$variable1 <- factor(full_corr_data_withseps$variable1, levels = c(key_affect_variables, "Separator1", key_biomarker_variables, "Separator2", key_BISS_variables))
full_corr_data_withseps$variable2 <- factor(full_corr_data_withseps$variable2, levels = c(key_affect_variables, "Separator1", key_biomarker_variables, "Separator2", key_BISS_variables))

# remove rows where variable1 and variable2 are both NA
full_corr_data_withseps <- full_corr_data_withseps |>
  filter(!(is.na(variable1) | is.na(variable2)))

# Plotting the lower triangle of the correlation matrix
ggplot(full_corr_data_withseps, aes(x = variable1, y = variable2, fill = correlation)) +
  geom_tile(color = "lightgrey", size = 0.1) +  # Adjust 'color' and 'size' as needed
  scale_fill_gradient2(low = embarktools::embark_colors[1], high = embarktools::embark_colors[2], 
                       mid = "white", midpoint = 0, na.value = "grey") +  # 'na.value' matches background
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix of Exercise Response Data") + 
  # remove 'Separator' labels
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.title = element_blank()) +
  scale_x_discrete(labels = function(x) ifelse(x %in% c("Separator1", "Separator2"), "", x)) +
  scale_y_discrete(labels = function(x) ifelse(x %in% c("Separator1", "Separator2"), "", x)) 


