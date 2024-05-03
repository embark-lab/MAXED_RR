
library(embarktools)
library(haven)
library(tidyr)
library(ggplot2)
# Plot emmeans for biss

# Load the data
load("results/biss_models.RData")

emmeans <- as.data.frame(biss_emmeans)
# Replace Self.Paced with Self-Paced in column names
colnames(emmeans) <- gsub("Self.Paced", "Self-Paced", colnames(emmeans))
# Replace Avg.Person with Avg Person in column names
colnames(emmeans) <- gsub("Avg.Person", "AvgPerson", colnames(emmeans))
# Replace Phs.Attract with PhsAttract in column names
colnames(emmeans) <- gsub("Phys.Attract", "PhsAttract", colnames(emmeans))

# Pivot Longer
emmeans <- emmeans |> 
  pivot_longer(
    cols = everything(), # Pivot all columns
    names_to = c("variable", "task", ".value"), # Extract variable, task, and rest as value
    names_pattern = "([^.]*)\\.([^.]*)\\.(.*)" # Define pattern for splitting column names
  ) 

emmeans$variable <- factor(emmeans$variable, levels = c("Appearance", "AvgPerson", "Looks", "PhsAttract", "Shape", "Weight", "Average"))


BISS_emmeans_prescribed <- ggplot(emmeans |> filter(task == 'Prescribed'), aes(x = time, y = emmean, color = group_factor)) +
  geom_point() +
  geom_line(aes(linetype = as.factor(condition))) +
  facet_wrap(~variable) +
  theme_bw() +
  labs(x = "Time (minutes)", 
       y = "Estimated Marginal Means", 
       title = "Estimated Marginal Means of BISS Measures During \n Prescribed Exercise",
       linetype = "Condition",  # Rename linetype legend
       color = "Group") +  
  scale_color_manual(name = 'Group', values = embark_palette()) +
  scale_linetype_manual(values = c("Rest" = "dashed", "Exercise" = "solid")) +
  embark_theme_a

# Save the plot data
save(BISS_emmeans_prescribed, file = "figs/3.body_image/BISS_emmeans_prescribed.RData")

ggsave("figs/3.body_image/biss_emmeans_prescribed.png", width = 10, height = 8)


BISS_emmeans_selfpaced <- ggplot(emmeans |> filter(task == 'Self-Paced'), aes(x = time, y = emmean, color = group_factor)) +
  geom_point() +
  geom_line(aes(linetype = condition)) +
  facet_wrap(~variable) +
  theme_bw() +
  labs(x = "Time (minutes)", 
       y = "Estimated Marginal Means", 
       title = "Estimated Marginal Means of BISS Measures During \n Self-Paced Exercise",
       linetype = "Condition",  # Rename linetype legend
       color = "Group") +  scale_color_manual(name = 'Group', values = embark_palette()) +
  scale_linetype_manual(values = c("Rest" = "dashed", "Exercise" = "solid")) +
  embark_theme_a

# Save the plot data
save(BISS_emmeans_selfpaced, file = "figs/3.body_image/BISS_emmeans_selfpaced.RData")

# Save the plots
ggsave("figs/3.body_image/biss_emmeans_selfpaced.png", width = 10, height = 8)

