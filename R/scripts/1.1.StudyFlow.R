#load wd and pull data
load("data/RedCap/MAXED_redcap_long.2024-05-03.RData")
# save maxed_screening_data
load("data/RedCap/MAXED_screening_data.RData")

#Screening
total_screenings <- nrow(maxed_screening_data)
ineligible_screens <- sum(maxed_screening_data$screen_eligible == 0, na.rm = TRUE)
ineligible_reasons <- table(maxed_screening_data$screen_pp_ineligible)
# put names to ineligible reasons
names(ineligible_reasons) <- c("Age", "Significant Binge Eatig", "Unable to Attend Study Visits", "Medical Exclusion", 
                               "Uncomfortable with Blood Draw", "Other")
#Intake
intake_total <- sum(MAXED_redcap_long$timepoint == "Intake")
pp_status <- table(MAXED_redcap_long$pp_status)

intake_dx <- table(MAXED_redcap_long$participant_assignment)
names(intake_dx) <- c("Control", "Subthreshold ED", "ED in partial Remission", "Active ED", "Ineligible")

#Reducing grouping to 1(control) and 2(ED)
MAXED_redcap_long$group <- case_when(
  MAXED_redcap_long$participant_assignment %in% c(1) ~ "Control",
  MAXED_redcap_long$participant_assignment %in% c(2:4) ~ "ED"
)

#Ineligibles at intake
intake_ineligible <- pp_status['6']
intake_ineligible_reasons <- table(MAXED_redcap_long$intake_ineligible)
names(intake_ineligible_reasons) <- c("BED", "ARFID", "Bipolar 1", "Other")


#Completed intake but did not enroll
intake_not_enroll <- pp_status['8']
intake_drop <- table(MAXED_redcap_long$drop_after_intake)
names(intake_drop) <- c("No Reason", "Time Constraint", "Unforseen Circumstances")


dropout_intake_data <- MAXED_redcap_long[MAXED_redcap_long$pp_status == 8, ]
#grouping of dropouts
dropout_table <- table(dropout_intake_data$participant_assignment)
names(dropout_table) <- c("Control", "Subthreshold ED", "ED in partial Remission", "Active ED")


#Study Visits
pilot_pp_etc <- c("MAXED_1001", "MAXED_1003", "MAXED_1010", "MAXED_1011", "MAXED_1012", "1011", "MAXED-1015", "MAXED 1022", "MAXED-1024", "MAXED-1056")
maxed_visit_data <- subset(MAXED_redcap_long, !(id %in% pilot_pp_etc))
#Day A
day_a_attendance <- sum(maxed_visit_data$timepoint == "Day_A")
day_b_attendance <- sum(maxed_visit_data$timepoint == "Day_B")
day_c_attendance <- sum(maxed_visit_data$timepoint == "Day_C")

#completed study visits
completed_visits <- MAXED_redcap_long[MAXED_redcap_long$timepoint %in% c("Day_A", "Day_B", "Day_C"), ]
completed_visits <- subset(completed_visits, !(id %in% pilot_pp_etc))
total_completed_visits <- sum(table(completed_visits$id) == 3)

#make attendence table that is day_a_attendance, day_b_attendance, day_c_attendance, total_completed_visits
attendance_table <- c(day_a_attendance, day_b_attendance, day_c_attendance, total_completed_visits)
# name the columns
names(attendance_table) <- c("Day A", "Day B", "Day C", "Total Completed Visits")
# save in Aim_1_Results.RData
# make dataframe with milk_drank_percent_a, milk_drank_percent_c and id
milk_data <- maxed_visit_data %>%
  select(milk_drank_percent_a, milk_drank_percent_c, id)

# make milkdrank_a data with just a and id
milk_data_a <- milk_data %>%
  select(milk_drank_percent_a, id)

# filter out rows where milk_drank_percent_a is NA
milk_data_a <- milk_data_a[!is.na(milk_data_a$milk_drank_percent_a),]
# make column for incomplete (0) vs complete (1) milkshake task which splits at 80%
milk_data_a$complete <- ifelse(milk_data_a$milk_drank_percent_a >= 80, 1, 0)
# get % of participants who completed the task
milk_complete_a <- sum(milk_data_a$complete) / nrow(milk_data_a) * 100

# do the same thing for c
milk_data_c <- milk_data %>%
  select(milk_drank_percent_c, id)
milk_data_c <- milk_data_c[!is.na(milk_data_c$milk_drank_percent_c),]
milk_data_c$complete <- ifelse(milk_data_c$milk_drank_percent_c >= 80, 1, 0)
milk_complete_c <- sum(milk_data_c$complete) / nrow(milk_data_c) * 100

save(total_screenings, 
     ineligible_screens, 
     ineligible_reasons, 
     intake_total, 
     intake_dx, 
     intake_ineligible_reasons, 
     intake_drop, 
     dropout_table, 
     attendance_table, 
     milk_complete_a, 
     milk_complete_c, 
     file = "results/Aim_1_Results.RData")

