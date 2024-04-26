load('data/cors_long.RData')
# make variables factors

SP_vars <- c('avg_pct_hr', 'max_pct_hr', 'distance')

# Selection of variables from MAXED RedCap data

Exercise_vars <- c('cet_total_Day_C', 
                   'eds_sum_Day_C', 
                   'fomb_ANR_mean_Day_C', 
                   'fomb_APR_mean_Day_C', 
                   'fomb_SPR_mean_Day_C', 
                   'fomb_SNR_mean_Day_C', 
                   'muscularity_sum_Day_C')
Weight_vars <- c('bmi_at_intake_Intake', 
                 'wt_suppress_high_current_Intake')

Actigraph_vars <- c('MVPA_bouted', 'LPA_bouted')

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

cors_long$variable1 <- factor(cors_long$variable1, levels = c(key_affect_variables, key_biomarker_variables, key_BISS_variables))


# Inserting dummy rows for separators and labels

# Define a helper function
add_separator_rows <- function(data, unique_based_on, new_variable_values, new_variable_name, groups) {
  # Determine the variable names for dynamic assignment
  target_var_name <- ifelse(unique_based_on == "variable1", "variable2", "variable1")
  
  for (group in groups) {
    for (new_value in new_variable_values) {
      # Create a new row based on the specified unique variable
      unique_values <- unique(data[[unique_based_on]])
      new_rows <- data.frame(
        setNames(object = list(unique_values), nm = unique_based_on),
        setNames(object = list(rep(new_value, length(unique_values))), nm = target_var_name),
        correlation = NA,
        group = group
      )
      # Append the new rows to the original data frame
      data <- rbind(data, new_rows)
    }
  }
  return(data)
}

# Adding "=== WEIGHT ===" and "=== EXERCISE ===" rows for 'variable2' based on unique values of 'variable1'


cors_long <- add_separator_rows(cors_long, "variable2", c("=== + AFFECT ===_P", 
                                                          "=== - AFFECT ===_P",
                                                          "=== BISS ===_P ", 
                                                          "=== EXERTION ===_P", 
                                                          '=== BIOMARKERS ===_P', 
                                                          "=== + AFFECT ===_SP", 
                                                          "=== - AFFECT ===_SP",
                                                          "=== BISS ===_SP ", 
                                                          "=== EXERTION ===_SP", 
                                                          '=== BIOMARKERS ===_SP'), "variable1", c("ED", "Control"))

cors_long <- add_separator_rows(cors_long, "variable1", c("=== WEIGHT ===", 
                                                          "=== EX FUNCTIONS ===", 
                                                          "=== SP EXERCISE ===", 
                                                          "=== ACTIGRAPH ==="), 
                                "variable2", 
                                c("ED", "Control"))

# Now, order or arrange your dataframe so the separators are in the correct position

# rename variables
cors_long$variable2 <- gsub("fomb_SNR_mean", "FAMB SNR", cors_long$variable2)
cors_long$variable2 <- gsub("fomb_SPR_mean", "FAMB SPR", cors_long$variable2)
cors_long$variable2 <- gsub("fomb_ANR_mean", "FAMB ANR", cors_long$variable2)
cors_long$variable2 <- gsub("fomb_APR_mean", "FAMB APR", cors_long$variable2)
cors_long$variable2 <- gsub("eds_sum", "EDS", cors_long$variable2)
cors_long$variable2 <- gsub("cet_total", "CET", cors_long$variable2)
cors_long$variable2 <- gsub("bmi_at_intake", "BMI", cors_long$variable2)
cors_long$variable2 <- gsub("wt_suppress", "Wt Suppress", cors_long$variable2)
cors_long$variable2 <- gsub("muscularity_sum", "DFM", cors_long$variable2)
cors_long$variable2 <- gsub("MVPA_bouted", "MVPA", cors_long$variable2)
cors_long$variable2 <- gsub("LPA_bouted", "LPA", cors_long$variable2)


# Add _p to the end of Leptin_ResidChange and BDNF_ResidChange
cors_long$variable1 <- gsub("Leptin_ResidChange", "Leptin_P", cors_long$variable1)
cors_long$variable1 <- gsub("BDNF_ResidChange", "BDNF_P", cors_long$variable1)
cors_long$variable1 <- gsub("Cortisol_ResidChange", "Cortisol_P", cors_long$variable1)


# Separate _P and _SP from variable names into a column called 'Condition'
cors_long$Condition <- ifelse(grepl("_P", cors_long$variable1), "Prescribed", "SelfPaced")
cors_long$variable1 <- gsub("_P", "", cors_long$variable1)
cors_long$variable1 <- gsub("_SP", "", cors_long$variable1)


Exercise_vars <- c('CET', 
                   'EDS', 
                   'FAMB ANR',
                   'FAMB APR',
                   'FAMB SPR',
                   'FAMB SNR', 
                   'DFM')

Weight_vars <- c('BMI', 
                 'Wt Suppres')

Activity_vars <- c('MVPA', 
                   'LPA')


# Ensure 'variable2' is a factor and set the levels including separators
cors_long$variable2 <- factor(cors_long$variable2, levels = c(Weight_vars,"=== WEIGHT ===", Exercise_vars, "=== EX FUNCTIONS ===", SP_vars, "=== SP EXERCISE ===", Activity_vars, "=== ACTIGRAPH ==="))


key_BISS_variables <- c('Average', 
                        'Weight', 
                        'Shape')

neg_affect_variables <- c('Fatigued',
                          'Crummy')

pos_affect_variables <- c('Calm', 
                          'Enthusiastic')

key_biomarker_variables <- c('Leptin', 
                             'BDNF', 
                             'Cortisol')

exertion <- c('Percieved_Exertion')

cors_long$variable1 <- factor(cors_long$variable1, levels = c("=== + AFFECT ===", pos_affect_variables, '=== - AFFECT ===', neg_affect_variables, '=== EXERTION ===', exertion, '=== BIOMARKERS ===', key_biomarker_variables, "=== BISS === ", key_BISS_variables))

# at rows indicating that correlations in SelfPaced condition for ED and Control on all Variable2 variables with key biomarker variables are not available
cors_long <- rbind(cors_long, data.frame(variable1 = 'Leptin', variable2 = unique(cors_long$variable2), correlation = NA, group = "ED", Condition = "SelfPaced"))
cors_long <- rbind(cors_long, data.frame(variable1 = 'BDNF', variable2 = unique(cors_long$variable2), correlation = NA, group = "ED", Condition = "SelfPaced"))
cors_long <- rbind(cors_long, data.frame(variable1 = 'Leptin', variable2 = unique(cors_long$variable2), correlation = NA, group = "Control", Condition = "SelfPaced"))
cors_long <- rbind(cors_long, data.frame(variable1 = 'BDNF', variable2 = unique(cors_long$variable2), correlation = NA, group = "Control", Condition = "SelfPaced"))
cors_long <- rbind(cors_long, data.frame(variable1 = 'Cortisol', variable2 = unique(cors_long$variable2), correlation = NA, group = "ED", Condition = "SelfPaced"))
cors_long <- rbind(cors_long, data.frame(variable1 = 'Cortisol', variable2 = unique(cors_long$variable2), correlation = NA, group = "Control", Condition = "SelfPaced"))


p <- ggplot(cors_long, aes(x = variable1, y = variable2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = embark_colors[1], high = embark_colors[2], mid = "white", midpoint = 0) +
  theme_minimal() +
  facet_grid(vars(group), vars(Condition)) +
  labs(title = "Correlation Matrix of Exercise Response Data with
      Key Variables of Interest across Control and ED groups", 
       caption = "Note: Correlation matrix of key variables in exercise response data. \n
       Variables represent Slopes (Body Image and Affect) and residualized change scores (Biomarkers).") +
  embark_theme_a +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 6), 
        legend.position = "right",
        plot.caption = element_text(size = 9), 
        legend.title = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.grid.major = element_line(colour = "grey70"),
        panel.grid.minor = element_blank()) + 
  theme(axis.text.y.left = element_text(size = 6), 
        axis.text.y.right = element_blank(),
        axis.ticks.y.left = element_line(),
        axis.ticks.y.right = element_blank())

p

# make a second graph that is the same, but suppress correlations < .2
p2 <- ggplot(cors_long |> filter(correlation^2 > 0.05 | is.na(correlation)), aes(x = variable1, y = variable2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = embark_colors[1], high = embark_colors[2], mid = "white", midpoint = 0) +
  theme_minimal() +
  facet_grid(vars(group), vars(Condition)) +
  labs(title = "Correlation Matrix of Exercise Response Data with
      Key Variables of Interest across Control and ED groups", 
       caption = "Note: Correlation matrix of key variables in exercise response data. \n
       Variables represent Slopes (Body Image and Affect) and residualized change scores (Biomarkers).") +
  embark_theme_a +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 6), 
        legend.position = "right",
        plot.caption = element_text(size = 9), 
        legend.title = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.grid.major = element_line(colour = "grey70"),
        panel.grid.minor = element_blank()) + 
  theme(axis.text.y.left = element_text(size = 6), 
        axis.text.y.right = element_blank(),
        axis.ticks.y.left = element_line(),
        axis.ticks.y.right = element_blank())


p2

ggsave("figs/5.correlations/exercise_response_correlation_matrix_separated.png", width = 10, height = 6, dpi = 300)
