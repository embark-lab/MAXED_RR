library(haven)
library(embarktools)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

# Load Exercise response summary data
Actigraph <- read.csv("data/Actigraph/GGIR_output/output_RAW/results/part5_personsummary_MM_L44.8M100.6V428.8_T5A5.csv")

# Create a new column for all MVPA minutes in bouts > 1min
Actigraph.1 <- Actigraph %>%
  mutate(MVPA_bouted = dur_day_MVPA_bts_1_5_min_pla + dur_day_MVPA_bts_5_10_min_pla + dur_day_MVPA_bts_10_min_pla, 
         LPA_bouted = dur_day_LIG_bts_1_5_min_pla + dur_day_LIG_bts_5_10_min_pla + dur_day_LIG_bts_10_min_pla) %>%
  filter(Nvaliddays >= 3) %>%
  select(ID, MVPA_bouted, LPA_bouted, Nvaliddays, dur_day_LIG_unbt_min_pla, dur_day_MOD_unbt_min_pla, dur_day_VIG_unbt_min_pla)

Actigraph <- Actigraph.1

# save the data
save(Actigraph, file = "data/Actigraph/Actigraph_Mini.RData")
