library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)
library(ggplot2)
library(patchwork)
library(haven)
library(ggthemes)
library(cowplot)

load('data/RedCap/redcap_raw_enrolled.RData')
load('data/Survey_Data/MAXED_redcap_long.2023-09-04.RData')
load('data/RedCap/redcap_raw.RData')


custom_linetypes <- c("Self-Paced" = "dotted", "Prescribed" = "dashed")

# graph of HR over time
custom_colors <- c("#fc6d46","#1a4e66")

ex_data_hr <- ex_data |>  
  filter(variable == 'Heart Rate') |> 
  mutate(`Percent Max HR` = value/studya_max_hr_a*100)

ex_data_hr

hr_plot <- ggplot(ex_data_hr, 
       aes(x = time, y = `Percent Max HR`, 
           group = interaction(group_factor, day), 
           color = group_factor, 
           linetype = day)) +
  geom_smooth(aes(fill = group_factor), size = 2.5, alpha = 0.2) +
  labs(x = "Time (mins)",
       y = "% Max HR",
       linetype = 'Ex Condition',
       color = 'Group',
       title = 'Heart Rate Over Exercise Session') +
  theme_minimal(base_family = "Avenir") + 
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title  = element_text(size = 18, family = "Avenir", face = "bold"), 
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 20, family = "Avenir", face = "bold"),
    legend.position = "top",
    strip.background = element_rect(fill = 'grey80', color = "grey50"),
    strip.text = element_text(color = 'grey20', face = 'bold', size = 14, family = "Avenir"),
    panel.grid.minor = element_line(size = 0.5, linetype = 'dashed', color = "grey90"),
    legend.title = element_text(face = "bold")
  ) +
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors)

hr_plot

ggsave(hr_plot, file = 'figs/4.ex_params//hr_plot.png')

# Distance

distance_plot <- ggplot(ex_data |> filter(variable == 'Distance'), 
       aes(x = time, y = value, 
           group = interaction(group_factor, day), 
           color = group_factor, 
           linetype = day)) +
  geom_smooth(aes(fill = group_factor), size = 2.5, alpha = 0.2) +
  labs(x = "Time (mins)",
       y = 'Distance',
       linetype = 'Ex Condition',
       color = 'Group',
       title = 'Distance Covered During Exercise Session') +
  theme_minimal(base_family = "Avenir") + 
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 20, family = "Avenir", face = "bold"),
    legend.position = "top",
    strip.background = element_rect(fill = 'grey80', color = "grey50"),
    strip.text = element_text(color = 'grey20', face = 'bold', size = 14),
    panel.grid.minor = element_line(size = 0.5, linetype = 'dashed', color = "grey90"),
    legend.title = element_text(face = "bold")
  ) +
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors)

distance_plot

ggsave(distance_plot, file = 'figs/4.ex_params//distance_plot.png')

# Watts

watts_plot <- ggplot(ex_data |> filter(variable == 'Watts'), 
                        aes(x = time, y = value, 
                            group = interaction(group_factor, day), 
                            color = group_factor, 
                            linetype = day)) +
  geom_smooth(aes(fill = group_factor), size = 2.5, alpha = 0.2) +
  labs(x = "Time (mins)",
       y = 'Watts',
       linetype = 'Ex Condition',
       color = 'Group',
       title = 'Bike Resistance (Watts) During Exercise Session') +
  theme_minimal(base_family = "Avenir") + 
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 20, family = "Avenir", face = "bold"),
    legend.position = "top",
    strip.background = element_rect(fill = 'grey80', color = "grey50"),
    strip.text = element_text(color = 'grey20', face = 'bold', size = 14),
    panel.grid.minor = element_line(size = 0.5, linetype = 'dashed', color = "grey90"),
    legend.title = element_text(face = "bold")
  ) +
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors)

watts_plot

ggsave(watts_plot, file = 'figs/4.ex_params//watts_plot.png')

hr_plot_nolegend <- hr_plot + theme(legend.position = 'none', axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title = '% Max Heart Rate')
distance_plot_nolegend <- distance_plot + theme(legend.position = 'none', axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title = 'Distance (miles)')
watts_plot_nolegend <- watts_plot + theme(legend.position = 'none', axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title = 'Effort (watts)')


legend<- get_legend(hr_plot)

# Combine the plots without individual titles
combined_plots <- hr_plot_nolegend / distance_plot_nolegend / watts_plot_nolegend 

# Lay out the plots
ex_params_plot <- combined_plots + plot_layout(ncol = 3) 

# Create an annotation for the title
plot_annotation <- plot_annotation(
  title = 'Exercise Parameters Over Exercise Sessions',
  caption = 'Time (mins)',
  theme = theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.caption = element_text(size = 20, hjust = 0.5, face = "bold"))
  )
  
ex_params_plot <- ex_params_plot + plot_annotation 

# Create a ggplot object for the title
title_plot <- ggplot() + 
  theme_void() +
  labs(title = 'Exercise Parameters Over Exercise Sessions') +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = margin(t = 0, b = 0, l = 0, r = 0, "pt"))

x_label_plot <- ggplot() + 
  theme_void() +
  labs(caption = 'Time (mins)') +
  theme(plot.caption = element_text(size = 20, hjust = 0.5)) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = margin(t = 0, b = 0, l = 0, r = 0, "pt"))
x_label_plot <- x_label_plot + 
  theme(
    plot.caption = element_text(face = "bold", size = 20, hjust = 0.5)
  )

# Final assembly
ex_params_plot <- (title_plot / 
                 legend / 
                 ex_params_plot / 
                 x_label_plot) + 
  plot_layout(heights = c(0.02, 0.05, 1, 0.02))


ex_params_plot <- ex_params_plot &   # Left alignment of caption
  theme(panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
        plot.background = element_rect(fill = 'transparent', colour = 'transparent')) 

ggsave(ex_params_plot, file = 'figs/4.ex_params/ex_params_plot.png')


theme_1 <- 
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 18, family = "Avenir", face = "bold"),
    legend.position = "top",
    plot.background = element_rect(fill = 'transparent', colour = "#1a4e66", size = 3))

load('data/Exercise_Params/Exercise_Session_Data.RData')
custom_colors <- c("ED" = "#1a4e66", "Control" = "#fc6d46")


pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', "MAXED_1011", "MAXED_1012")

ex_data <- ex_data |> 
  filter(!id %in% pilot_ids)

HR_data_1 <- ex_data |> 
  filter(variable == 'Heart Rate', 
         day == 'Self-Paced') |> 
  group_by(id, day) |> 
  mutate(max_pct_hr = max(value/studya_max_hr_a *100)) |> 
  filter(time > 5) |> 
  mutate(avg_pct_hr = mean(value/studya_max_hr_a *100)) |> 
  select(id, day, group, group_factor, avg_pct_hr, max_pct_hr) |> 
  distinct()

library(effsize)
d_hr <- cohen.d(data = HR_data_1, max_pct_hr ~ group_factor)


hr_plot <- ggplot(HR_data_1, aes(x = factor(1),y = max_pct_hr, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = 'HR (% Max)', 
       title = 'Mean HR',
       x = '') +
  theme_1 + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  guides(alpha = FALSE) +
  stat_summary(
    fun.y=median,
    geom="text",
    aes(label=sprintf("%.0f", ..y..)),
    vjust=-0.5,
    size=5,
    colour = 'black',
    position = position_dodge(width = 0.75)
  ) +
  # Annotating Cohen's d val
  annotate("text", x = 1, y = 85, 
           label = paste("d =", round(d_hr$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
  ylim(40,90)



watts_data_1 <- ex_data |> 
  filter(variable == 'Watts', 
         day == 'Self-Paced') |> 
  distinct() |> 
  group_by(id) |> 
  mutate(watts_max = max(value, na.rm = TRUE))  |> 
  select(id, day, group, group_factor, watts_max, cet_total_weighted_sum) |> 
  distinct() |> 
  filter(!is.na(cet_total_weighted_sum)) 

library(effsize)
d_watts <- cohen.d(data = watts_data_1, watts_max ~ group_factor)


w_plot <- ggplot(watts_data_1, aes(x = factor(1),y = watts_max, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = 'Watts', 
       title = 'Max Effort',
       x = '') +
  theme_1 + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  guides(alpha = FALSE) +
  stat_summary(
    fun.y=median,
    geom="text",
    aes(label=sprintf("%.0f", ..y..)),
    vjust=-0.5,
    size=5,
    colour = 'black',
    position = position_dodge(width = 0.75)
  ) +
  # Annotating Cohen's d val
  annotate("text", x = 1, y = 120, 
           label = paste("d =", round(d_watts$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
  ylim(0, 130)




distance_data_1 <- ex_data |> 
  filter(variable == 'Distance', 
         day == 'Self-Paced', 
         !is.na(value)) |> 
  distinct() |> 
  group_by(id) |> 
  mutate(distance_total = max(value, na.rm = TRUE))  |> 
  select(id, day, group, group_factor, distance_total, cet_total_weighted_sum) |>
  distinct() |> 
  filter(!is.na(cet_total_weighted_sum)) 

d_distance <- cohen.d(data = distance_data_1, distance_total ~ group_factor)


d_plot <- ggplot(distance_data_1, aes(x = factor(1),y = distance_total, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = 'Miles', 
       title = 'Distance',
       x = '') +
  theme_1 + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  guides(alpha = FALSE) +
  stat_summary(
    fun.y=median,
    geom="text",
    aes(label=sprintf("%.0f", ..y..)),
    vjust=-0.5,
    size=5,
    colour = 'black',
    position = position_dodge(width = 0.75)
  ) +
  # Annotating Cohen's d val
  annotate("text", x = 1, y = 9, 
           label = paste("d =", round(d_distance$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
  ylim(2, 10)

hr_plot_1 <- hr_plot + theme(legend.position = 'none', axis.title.x = element_blank()) +
  labs(title = 'Mean HR')
d_plot_1 <- d_plot + theme(legend.position = 'none', axis.title.x = element_blank()) +
  labs(title = 'Distance')
w_plot_1 <- w_plot + theme(legend.position = 'none', axis.title.x = element_blank()) +
  labs(title = 'Max Effort')

legend<- get_legend(hr_plot)


ex_plot_1 <- hr_plot_1 /d_plot_1 / w_plot_1 +
  plot_layout(ncol = 3)

ex_plot_1


# Create a ggplot object for the title
title_plot <- ggplot() + 
  theme_void() +
  labs(title = 'Exercise Parameters During Self-Paced Exercise') +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = margin(t = 0, b = 0, l = 0, r = 0, "pt"))



# Final assembly
ex_plot_1 <- (title_plot / 
                legend / 
                ex_plot_1) + 
  plot_layout(heights = c(0.02, 0.05, 1))

ex_plot_1

ex_plot_1 <- ex_plot_1 &   # Left alignment of caption
  theme(panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
        plot.background = element_rect(fill = 'transparent', colour = 'transparent')) 

ggsave(ex_plot_1, file = 'figs/4.ex_params/ex_params_boxplots.png')



