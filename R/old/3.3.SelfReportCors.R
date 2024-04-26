library(haven)

load("data/Exercise_Response_Summary_Data.RData")
load("data/RedCap/MAXED_redcap_long.2024-01-12.RData")

sample_ids <- unique(exercise_response_data$id)

# Selection of variables from MAXED RedCap data

Exercise_vars <- c('cet_clinical_Day_C', 'cet_total_Day_C', 'eds_sum_Day_C')
Weight_vars <- c('bmi_at_intake_Intake', 'wt_suppress_high_current_obj_Intake')

# Filter out pilot ids
corr_vars <- MAXED_redcap_wide |>
  filter(id %in% sample_ids) |>
  select(id, all_of(Exercise_vars), all_of(Weight_vars))

# Combine with exercise response data
cor_vars2 <- exercise_response_data |>
  full_join(corr_vars, by = "id")


key_biomarker_variables <- c('Leptin_ResidChange', 
                             'BDNF_ResidChange')

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

# filter data for ED group
cor_vars2_ED <- cor_vars2 |>
  filter(group == "1") |> 
  # make all columns (except for ID) numeric
  mutate(across(-id, as.numeric))
  
# make a correlation matrix, excluding ID and removing pairwise NA values
exercise_response_cors_ED <- cor(cor_vars2_ED |>
                             select(-id), use = "pairwise.complete.obs")

# pivot correlation matrix to long Format
ED_cors_long <- exercise_response_cors_ED |> 
  as.data.frame() |>
  rownames_to_column(var = "variable1") |>
  pivot_longer(cols = -variable1, names_to = "variable2", values_to = "correlation") |> 
  filter(correlation != 1)

# only include if var1 in key_affect, key_biomarker, or key_BISS and var2 in Exercise_vars or Weight_vars
ED_cors_long <- ED_cors_long |>
  filter((variable1 %in% key_affect_variables | variable1 %in% key_biomarker_variables | variable1 %in% key_BISS_variables) & 
         (variable2 %in% Exercise_vars | variable2 %in% Weight_vars))
# remove _Day_C from variable names
ED_cors_long$variable2 <- gsub("_Day_C", "", ED_cors_long$variable2)
# remove _Intake from variable names
ED_cors_long$variable2 <- gsub("_Intake", "", ED_cors_long$variable2)
# remove _high_current_obj from variable names
ED_cors_long$variable2 <- gsub("_high_current_obj", "", ED_cors_long$variable2)


# filter data for HC group
cor_vars2_HC <- cor_vars2 |>
  filter(group == "0") |> 
  # make all columns (except for ID) numeric
  mutate(across(-id, as.numeric))

# make a correlation matrix, excluding ID and removing pairwise NA values
exercise_response_cors_HC <- cor(cor_vars2_HC |>
                             select(-id), use = "pairwise.complete.obs")

# pivot correlation matrix to long Format
HC_cors_long <- exercise_response_cors_HC |> 
  as.data.frame() |>
  rownames_to_column(var = "variable1") |>
  pivot_longer(cols = -variable1, names_to = "variable2", values_to = "correlation") |> 
  filter(correlation != 1)

# only include if var1 in key_affect, key_biomarker, or key_BISS and var2 in Exercise_vars or Weight_vars
HC_cors_long <- HC_cors_long |>
  filter((variable1 %in% key_affect_variables | variable1 %in% key_biomarker_variables | variable1 %in% key_BISS_variables) & 
         (variable2 %in% Exercise_vars | variable2 %in% Weight_vars))
# remove _Day_C from variable names
HC_cors_long$variable2 <- gsub("_Day_C", "", HC_cors_long$variable2)
# remove _Intake from variable names
HC_cors_long$variable2 <- gsub("_Intake", "", HC_cors_long$variable2)
# remove _high_current_obj from variable names
HC_cors_long$variable2 <- gsub("_high_current_obj", "", HC_cors_long$variable2)
#add group column
ED_cors_long$group <- "ED"
HC_cors_long$group <- "Control"

# bind the two dataframes together
cors_long <- rbind(ED_cors_long, HC_cors_long)

# make variables factors
cors_long$variable1 <- factor(cors_long$variable1, levels = c(key_affect_variables, key_biomarker_variables, key_BISS_variables))


ggplot(cors_long, aes(x = variable1, y = variable2, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = embark_colors[1], high = embark_colors[2], mid = "white", midpoint = 0) +
  theme_minimal() +
  facet_grid(group ~ .) +
  labs(title = "Correlation Matrix of Exercise Response Data with 
       Key Variables of Interest across Control and ED groups", 
       caption = "Note: Correlation matrix of key variables in exercise response data. P = Prescribed, SP = Self Paced; 
       Variables represent Slopes (Body Image and Affect) and residualized change scores (Biomarkers).") +
  embark_theme_a +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 10), 
        legend.position = "right",
        plot.caption = element_text(size = 9), 
        legend.title = element_blank(),
        # bold the facet labels
        strip.text = element_text(face = "bold")) +
  # make gridlines light grey
  theme(panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank())

#save plot
ggsave("figs/5.correlations/exercise_response_correlation_matrix.png", width = 10, height = 6, dpi = 300)

