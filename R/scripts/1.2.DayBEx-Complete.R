load("data/Exercise_Params/Exercise_Session_Data.RData")
# filter only watts data on day b and only first 5 columns
watts_data <- ex_data |>
  filter(day == "b" & variable == "Watts") |>
  select(1:5) 
# count the number of rows for each id
watts_data <- watts_data |>
  group_by(id) |>
  summarise(nrows = n()) |> 
  mutate(Complete = ifelse(nrows == 7, 1, 0))
# calculate the percentage that completed the task (nrows ==7 )
day_b_complete <- watts_data |>
  summarise(Complete = sum(Complete), Total = n(), Percentage = Complete/Total*100)
day_b_ex_complete <- day_b_complete$Percentage 

# Save in Aim_1_Results.RData
resave(day_b_ex_complete, file = "results/Aim_1_Results.RData")
