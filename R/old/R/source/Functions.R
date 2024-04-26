# Define function to recalculate group assignments
# This function takes a dataframe, extracts and recodes the 'participant_assignment' field,
# then computes and adds the 'group' and 'group_factor' fields.
recalculate_groups <- 
  function(df) {
  df %>% 
    mutate(participant_assignment = zap_labels(participant_assignment) %>% as.character()) %>% 
    mutate(group = recode(participant_assignment, "1"="0", "2"="1", "3"="1", "4"="1")) %>% 
    mutate(group_factor = factor(group, levels = c("0", "1"), labels = c('Control', "ED")))
}

# Define function to process exercise data based on a suffix ('_a' or '_b')
# This function filters out rows with all NAs except for the 'id' field.
process_ex_data <- function(ex_data, suffix) {
  ex_data %>%
    select(id, ends_with(suffix)) %>%
    filter(rowSums(is.na(select(., -id))) != (ncol(.) - 1))
}

# combine plots for affect loess plots
combine_plots <- function(plot_neg, plot_pos, title_text) {
  caption_text <- "Note: Responses rated every 5 minutes on 5-point Likert scale from \n 0 (Do Not Feel) to 4 (Feel Very Strongly)"
  wrapped_caption <- str_wrap(caption_text, width = 80)  
  
  combined_plot <- plot_neg + plot_pos + plot_layout(ncol = 1) +
    plot_annotation(title = title_text,
                    caption = wrapped_caption,
                    theme = theme(text = element_text(size = 18, family = 'Garet', face = "bold", color = '#1A4F66'))) &
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0, size = 14)) +
    theme(panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
          plot.background = element_rect(fill = 'transparent', colour = 'transparent'))  # added this line
  
  return(combined_plot)
}