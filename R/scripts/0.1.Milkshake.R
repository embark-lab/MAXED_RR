library(dplyr)
library(tidyr)
library(effsize)
library(stringr)

# Load the data
load('data/RedCap/redcap_raw_enrolled.Rdata')
load('data/RedCap/groups.RData')

day_c_data <- redcap_raw_enrolled %>%
  filter(study_visit == "Day_C") %>%
  select(record_id, contains('pre') & contains ('nvs'), contains('mid') & contains('nvs')) |> 
  rename(id = record_id) |> 
  # make id a character
  mutate(id = as.character(id)) |> 
  # chnage 'nvs_pre' to 'milkshake_pre' and 'nvs_mid' to 'milkshake_post' in variable names
  rename_with(~str_replace(., "pre", "milkshake_pre")) |>
  rename_with(~str_replace(., "mid", "milkshake_post")) |> 
  # remove the 'nvs' from the variable names
  rename_with(~str_replace(., "nvs_", ""))

day_a_data <- redcap_raw_enrolled %>%
  filter(study_visit == "Day_A") %>%
  select(record_id, contains('pre') & contains ('pvs'), contains ('post') & contains ('nvs')) |> 
  rename(id = record_id) |> 
  # make id a character
  mutate(id = as.character(id)) |> 
  # select if contains nvs and post OR pvs AND pre
  rename_with(~str_replace(., "pre", "milkshake_post"), contains("pre")) %>%
  rename_with(~str_replace(., "post", "milkshake_pre"), !contains("milkshake_post")) |> 
# remove 'nvs' and 'pvs' from the variable names
  rename_with(~str_replace(., "nvs_", "")) |>
  rename_with(~str_replace(., "pvs_", ""))

# Merge and recode participant_assignment
merged_c_data <- day_c_data %>%
  left_join(groups, by = "id")

merged_a_data <- day_a_data %>%
  left_join(groups, by = "id")

# Composite Score Calculation Function
composite_scores <- function(data, vars) {
  rowSums(data[vars], na.rm = TRUE)
}

# Add composite scores to the dataset
score_vars <- list(
  biss_sum_milkshake_pre = c("biss_1_milkshake_pre", "biss_2_milkshake_pre", "biss_3_milkshake_pre", "biss_4_milkshake_pre", "biss_5_milkshake_pre", "biss_6_milkshake_pre"),
  biss_sum_milkshake_post = c("biss_1_milkshake_post", "biss_2_milkshake_post", "biss_3_milkshake_post", "biss_4_milkshake_post", "biss_5_milkshake_post", "biss_6_milkshake_post"))

for (var in names(score_vars)) {
  merged_a_data <- merged_a_data %>%
    mutate(!!var := composite_scores(., score_vars[[var]]))
}

for (var in names(score_vars)) {
  merged_c_data <- merged_c_data %>%
    mutate(!!var := composite_scores(., score_vars[[var]]))
}


# string replace test vars with their true names in the dataset
merged_a_data <- merged_a_data |> 
  rename_with(~str_replace(., "paas_1", "Enthusiasm"), contains("paas_1")) |>
  rename_with(~str_replace(., "paas_2", "Crumminess"), contains("paas_2")) |>
  rename_with(~str_replace(., "paas_3", "Fatigue"), contains("paas_3")) |>
  rename_with(~str_replace(., "paas_4", "Calm"), contains("paas_4")) |>
  rename_with(~str_replace(., "biss_sum", "BISS"), contains("biss_sum")) |>
  rename_with(~str_replace(., "fcq_3_binge", "Binge"), contains("fcq_3_binge")) |>
  rename_with(~str_replace(., "fcq_3_restrict", "Restrict"), contains("fcq_3_restrict")) |>
  rename_with(~str_replace(., "fcq_3_fast", "Fast"), contains("fcq_3_fast")) |>
  rename_with(~str_replace(., "fcq_3_vomit", "Vomit"), contains("fcq_3_vomit")) |>
  rename_with(~str_replace(., "fcq_3_exercise", "Exercise"), contains("fcq_3_exercise"))

# same with merged_c_data
merged_c_data <- merged_c_data |> 
  rename_with(~str_replace(., "paas_1", "Enthusiasm"), contains("paas_1")) |>
  rename_with(~str_replace(., "paas_2", "Crumminess"), contains("paas_2")) |>
  rename_with(~str_replace(., "paas_3", "Fatigue"), contains("paas_3")) |>
  rename_with(~str_replace(., "paas_4", "Calm"), contains("paas_4")) |>
  rename_with(~str_replace(., "biss_sum", "BISS"), contains("biss_sum")) |>
  rename_with(~str_replace(., "fcq_3_binge", "Binge"), contains("fcq_3_binge")) |>
  rename_with(~str_replace(., "fcq_3_restrict", "Restrict"), contains("fcq_3_restrict")) |>
  rename_with(~str_replace(., "fcq_3_fast", "Fast"), contains("fcq_3_fast")) |>
  rename_with(~str_replace(., "fcq_3_vomit", "Vomit"), contains("fcq_3_vomit")) |>
  rename_with(~str_replace(., "fcq_3_exercise", "Exercise"), contains("fcq_3_exercise"))


# make a list of variables to test 
test_vars <- c("Enthusiasm", "Crumminess", "Fatigue", "Calm", 
               "BISS", 
               "Binge", "Restrict", "Fast", "Vomit", "Exercise")

# Define the perform_tests function
perform_tests <- function(df, var) {
  # Extract the pre and post vectors
  pre <- df[[paste0(var, "_milkshake_pre")]]
  post <- df[[paste0(var, "_milkshake_post")]] 
  # Perform paired t-test, omitting NA pairs
  t_test <- t.test(pre, post, paired = TRUE, na.action = na.omit) 
  # Calculate Cohen's d effect size
  cohen_d <- effsize::cohen.d(pre, post, paired = TRUE, na.rm = TRUE)$estimate
  # Return t-test statistic, p-value, and Cohen's d
  list(pre_mean = mean(pre, na.rm = T), post_mean = mean(post, na.rm = T), t_value = t_test$statistic, p_value = t_test$p.value, cohen_d = cohen_d)
}

results_a <- purrr::map_dfr(test_vars, function(var) {
  ed_group <- merged_a_data %>% filter(group == 1)
  hc_group <- merged_a_data %>% filter(group == 0)
  ed_test <- perform_tests(ed_group, var)
  hc_test <- perform_tests(hc_group, var)
  
  data.frame(
    Variable = var,
    Group = c("ED", "HC"),
    pre_milkshake_mean = c(ed_test$pre_mean, hc_test$pre_mean),
    post_milkshake_mean = c(ed_test$post_mean, hc_test$post_mean),
    t_value = c(ed_test$t_value, hc_test$t_value),
    p_value = c(ed_test$p_value, hc_test$p_value),
    cohen_d = c(ed_test$cohen_d, hc_test$cohen_d)
  )
}, .id = NULL)


milkshake_results_a <- results_a %>%
  mutate(
    Pre_Milkshake = sprintf("%.2f", pre_milkshake_mean),
    Post_Milkshake = sprintf("%.2f", post_milkshake_mean),
    `t-value` = sprintf("%.2f", -as.numeric(t_value)),
    `p-value` = sprintf("%.3f", p_value),
    `Cohen's d Effect Size` = sprintf("%.2f", -as.numeric(cohen_d))
  ) %>%
  select(Variable, Group, Pre_Milkshake, Post_Milkshake, `t-value`, `p-value`, `Cohen's d Effect Size`)

results_c <- purrr::map_dfr(test_vars, function(var) {
  ed_group <- merged_c_data %>% filter(group == 1)
  hc_group <- merged_c_data %>% filter(group == 0)
  ed_test <- perform_tests(ed_group, var)
  hc_test <- perform_tests(hc_group, var)
  
  data.frame(
    Variable = var,
    Group = c("ED", "HC"),
    pre_milkshake_mean = c(ed_test$pre_mean, hc_test$pre_mean),
    post_milkshake_mean = c(ed_test$post_mean, hc_test$post_mean),
    t_value = c(ed_test$t_value, hc_test$t_value),
    p_value = c(ed_test$p_value, hc_test$p_value),
    cohen_d = c(ed_test$cohen_d, hc_test$cohen_d)
  )
}, .id = NULL)


# Save the final table
save(milkshake_results_a, file = "results/milkshake_table.RData")
