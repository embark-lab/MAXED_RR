library(ggplot2)
transparent_theme <- theme(
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA),
  strip.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.box.background = element_rect(fill = "transparent", colour = NA)
)

theme_1 <- 
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 18, family = "Avenir", face = "bold"),
    legend.position = "top",
    plot.background = element_rect(fill = 'transparent', colour = "#1a4e66", size = 3))


custom_colors <- c("ED" ="#fc6d46", "Control" =  "#1a4e66")
custom_colors_2 <- c("Control" ="#fc6d46", "ED" =  "#1a4e66")


custom_linetypes <- c("Self-Paced" = "dotted", "Prescribed" = "dashed")
pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', "MAXED_1011", "MAXED_1012")

negative_emotions <- c("Crummy", "Fatigued")
positive_emotions <- c("Calm", "Enthusiastic")

positive_fill <-"#c2b824"  # Color for negative emotions
negative_fill <- "darkgrey"  # Color for positive emotions