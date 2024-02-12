library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)


# Define custom line types
custom_linetypes <- c("Self-Paced" = "dotted", "Prescribed" = "dashed")

custom_colors <- c("#fc6d46","#1a4e66")
biss$variable <- factor(biss$variable, levels = c("Phys Attract", "Appearance", "Looks", "Shape", 'Weight', 'Average'))

# Define custom line types

biss_plot <- ggplot(biss, aes(x = time, y = value, group = interaction(group_factor, day), color = group_factor, linetype = day)) +
  geom_smooth(size = 1, aes(fill = group_factor), alpha = 0.2) +
  facet_wrap(~variable) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = 'Time', 
       y = "Body Image Valence (1-9)", 
       linetype = 'Ex Condition') +
  theme_minimal() +
  theme(text = element_text(size = 16, family = "Avenir"),
        axis.title = element_text(size = 14, family = "Avenir"),
        axis.text = element_text(size = 14, family = "Avenir"),
        strip.background = element_rect(fill = 'darkgrey'),
        strip.text = element_text(color = 'white', face = 'bold', family = "Avenir"),
        legend.position = 'top',
        legend.title = element_text(face = "bold", family = "Avenir"),
        plot.title = element_text(hjust = 0.5, family = "Avenir")
  )

caption_text <- "Note: Responses rated every 5 minutes on 9-point Likert scale from \n 1 (extremely dissatisfied/negative) to 9 (extremely satisfied/positive)"
wrapped_caption <- str_wrap(caption_text, width = 100)  

biss_plot <- biss_plot +
  plot_annotation(title = "Body Image States Scale Scores Over Time During Exercise",
                  caption = wrapped_caption,
                  theme = theme(text = element_text(size = 14, family = "Avenir")) &
                    theme(plot.title.position = "plot",   # Position of title within plot area
                          plot.title = element_text(hjust = 0.5, family = "Avenir", face = "bold"),
                          plot.caption = element_text(hjust = 0, size = 14, family = "Avenir")) &   # Left alignment of caption
 theme(panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
  plot.background = element_rect(fill = 'transparent', colour = 'transparent')) )

biss_plot


ggsave(file = 'figs/3.body_image/biss_plot.png')
