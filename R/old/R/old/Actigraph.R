library(haven)
library(embarktools)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

# Load Exercise response summary data
Actigraph <- read.csv("data/Actigraph/GGIR_output/output_RAW/results/part2_summary.csv")
Actigraph |> Actigraph
 #compute valid wear days by adding weekday and weekend wear days
Actigraph.1 <- Actigraph |> 
  mutate(valid_wear_days = N.valid.WKdays + N.valid.WEdays) |> 
  select(ID, starts_with("AD"), valid_wear_days)
  