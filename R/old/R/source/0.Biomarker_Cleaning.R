library(rio)
library(stringr)
library(dplyr)
Assays <- import_list('data/Assays/20230725_FINAL_Embark_MSD_17139.xlsx')

Assay_results <- Assays$Results |> 
  select(`Sample #`, Sample, `MAXED ID`, `Date of Collection`, `Volume (100uL for leptin, 20uL for BDNF)`, `BDNF (pg/mL)`, `Leptin (pg/mL)`)

Assay_results <- Assay_results |> filter (
  `Volume (100uL for leptin, 20uL for BDNF)` == '120uL'
)

# remove rows with both assays missing
Assay_results <- Assay_results |> filter (
  !is.na(`BDNF (pg/mL)`) | !is.na(`Leptin (pg/mL)`)
)

# use stringr to split Sample column into 'ID', 'Sample Number', 'Day', 'Time'
Assay_results <- Assay_results |> 
  mutate(
    # ID = MAXED_####, Sample Number = S#, Day = Day#, Time = Pre/Post
    ID = str_extract(Sample, 'MAXED_10[0-9][0-9]'),
    Sample_Number = str_extract(Sample, 'S[0-9]'),
    Day = str_extract(Sample, 'Day[A/B]'),
    Time = str_extract(Sample, 'Pre|Post')
  )

Assay_results <- Assay_results |>
  select(ID, Sample_Number, Day, Time, `BDNF (pg/mL)`, `Leptin (pg/mL)`)

Assay_results$Condition <- recode(Assay_results$Day, 'DayA' = 'Rest', 'DayB' = 'Exercise')

# load group data to merge with Assay_results
load('data/RedCap/groups.RData')
groups$ID = as.character(groups$id)
Assay_results <- left_join(Assay_results, groups, by = 'ID')

# Clean columns
Assay_results <- Assay_results |> 
  select(ID, group, group_factor, Sample_Number, Time, Condition, `BDNF (pg/mL)`, `Leptin (pg/mL)`) |> 
  rename(BDNF = `BDNF (pg/mL)`, Leptin = `Leptin (pg/mL)`) |> 
  mutate(across(c(BDNF, Leptin), ~ as.numeric(.)))

save(Assay_results, file = 'data/Assays/Assay_results.RData')

