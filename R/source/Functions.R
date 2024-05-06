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

