library(dplyr)
library(ggplot2)
library(tidyr)
library(embarktools)
library(effsize)
load('data/Assays/Assay_results.RData')
Assay_results <- Assay_results |> 
  filter(str_starts(Sample_Number, "S"))
# Make a graph comparing pre and post treatment levels of the biomarkers across group and visit

# Make Assay results longer for ggplot by pivoting BDNF and Leptin to 'Assay'
Assay_results_long <- Assay_results %>%
  pivot_longer(cols = c(BDNF, Leptin, Cortisol), names_to = 'Assay', values_to = 'Value') %>%
  mutate(Assay = factor(Assay, levels = c('BDNF', 'Leptin', 'Cortisol'))) |> 
  mutate(Time = factor(Time, levels = c('Pre', 'Post'))) 

# Fun little ggplot to compare pre and post treatment levels of the biomarkers across group and visit, individual level daat 
Group_Assay_Results <- Assay_results_long %>%
  ggplot(aes(x = Time, y = Value, color = group_factor, linetype = Condition)) +
  geom_point() +
  geom_line(aes(group = interaction(group_factor, Condition, ID))) +
  facet_wrap(~Assay, scales = 'free_y', labeller = labeller( 
    Assay = c("Cortisol" = "Cortisol (ng/ml)", 
              "Leptin" = "Leptin (pg/ml)", 
              "BDNF" = "BDNF (pg/ml)")
  )) +
  theme_minimal() +
  embark_theme_a +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.position = 'right', 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12)) +
  labs(title = 'Circulating Biomarkers Pre- and Post- \n Exercise: Individual-level Data',
       x = "",
       y = "") +
  # put some major gridlines in light grey 
  theme(panel.grid.major = element_line(colour = "grey", size = 0.5)) +
  # make vertical gridlines white
  scale_color_manual(name = 'Group', values = embark_palette())

# save the plot data
save(Group_Assay_Results, file = 'figs/4.Biomarkers/Group_Assay_Results_Plot.RData')
ggsave('figs/4.Biomarkers/Assay_results.png', width = 10, height = 6, dpi = 300)

assay_summary_data <- Assay_results_long %>%
  group_by(Assay, Time, group_factor, Condition) |> 
  summarize(mean_value = mean(Value, na.rm = TRUE),
            median_value = median(Value, na.rm = TRUE),
            se = sd(Value, na.rm = TRUE) / sqrt(n()), 
            lower_mean = mean_value - se, 
            upper_mean = mean_value + se)

save(assay_summary_data, file = 'data/Assays/assay_summary_data.RData')

Mean_assay_plot <- ggplot(assay_summary_data, 
                          aes(x = Time, group = interaction(group_factor, Condition))) +
  geom_point(aes(y = mean_value, color = group_factor), shape = 19) +   # Solid circles for mean
  # label the mean points with the actual number
 # geom_point(aes(y = median_value, color = group_factor), shape = 1) +  # Hollow circles for median
  geom_line(aes(y = mean_value, color = group_factor, linetype = Condition)) +  # Lines connecting means
  geom_errorbar(aes(ymin = lower_mean, ymax = upper_mean, color = group_factor), width = 0.2) +  # Error bars for means
  facet_wrap(~Assay, scales = 'free_y', labeller = labeller( 
    Assay = c("Cortisol" = "Cortisol (ng/ml)", 
              "Leptin" = "Leptin (pg/ml)", 
              "BDNF" = "BDNF (pg/ml)")
  )) +
  labs(color = "Group", linetype = "Condition")  +
  embarktools::embark_theme_a +
  scale_color_manual(values = embark_palette()) +
  theme(legend.position = 'right', 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # make y axis test smaller
  theme(axis.text.y = element_text(size = 8)) +
  theme(axis.text.x = element_text (size = 12)) +
  labs(title = 'Circulating Biomarkers Pre- and Post- \n Exercise: Means by Group and Condition', 
       y = '',
       x = element_blank()) 

# calculate pre-post cohen's d values within assay, group, and condition
annotate <- Assay_results_long %>%
  group_by(Assay, group_factor, Condition) %>%
  filter(!is.na(Value), !is.na(Time)) %>%
  mutate(cohens_d = effsize::cohen.d(Value[Time == 'Post'], Value[Time == 'Pre'])$estimate) |> 
  select(Assay, group_factor, Condition, cohens_d) |>
  distinct()
  
annotate.1 <- left_join(annotate, assay_summary_data, by = c('Assay', 'group_factor', 'Condition'))

# add mean pre-post value to the cohens d
annotate.2 <- annotate.1|>
  group_by(Assay, group_factor, Condition) |>
  mutate(midpoint = mean(mean_value)) |> 
# select only midpoint and cohens d
  select(Assay, group_factor, Condition, midpoint, cohens_d) |>
  distinct() |> 
  filter (Condition == 'Exercise') 

annotate.3 <- annotate.2 |>
  group_by(Assay) |> 
  # identify whether control or ED midpoint is greater
  mutate(position_factor = ifelse(midpoint[group_factor == 'Control'] > midpoint[group_factor == 'ED'], 'Control', 'ED')) |>
# adjust the y position 
  mutate(y_pos = case_when(position_factor == group_factor ~ midpoint*1.05,
                         position_factor != group_factor ~ midpoint*0.95)) 

# Annotate Mean Assay Plot with Cohen's d values
Mean_assay_plot <- Mean_assay_plot + 
  geom_text(data = annotate.3, aes(label = paste('Ex d =', round(cohens_d, 2)), x = 1.55, y = y_pos), 
            color = ifelse(annotate.3$group_factor == 'Control', embark_palette()[1], embark_palette()[2]),
            size = 3, hjust = 0, vjust = 0)

# save plot data
save(Mean_assay_plot, file = 'figs/4.Biomarkers/Assay_summary_results_plot.RData')
ggsave('figs/4.Biomarkers/Assay_summary_results.png', width = 10, height = 6, dpi = 300)



