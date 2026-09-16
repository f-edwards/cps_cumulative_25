# create first event annual counts by race/ethn for removal and TPR -------
# by state ----------------------------------------------------------------
# updated 6/2/25 ---------------------------------------------------------

rm(list = ls()); gc()
library(data.table)
library(tidyverse)
library(mice)

#### grab imputed data
afcars_path<-"~/Projects/ndacan_processing/imputations/afcars/"
afcars_files<-paste(afcars_path,
                    list.files(afcars_path),
                    sep = "")

afcars_imps <- afcars_files |> 
  map(read_rds)

# make long frames --------------------------------------------------------

afcars_long <- afcars_imps |> 
  map(mice::complete, action = "long") |> 
  bind_rows() |> 
  filter(age<18)

# make first removal counts -----------------------------------------------

afcars_rem <- afcars_long |>  
  filter(totalrem==1, entered==1) |> 
  group_by(.imp, state, year, age, race_ethn) |> 
  summarise(first_entry = n())

# make first full tpr counts -----------------------------------------------

afcars_tpr <- afcars_long  |>  
  filter(istpr==1) |>  
  group_by(.imp) |> 
  distinct(stfcid, .keep_all = T)

afcars_tpr <- afcars_tpr |> 
  group_by(.imp, state, year, age, race_ethn) |> 
  summarise(first_tpr = n())

# join --------------------------------------------------------------------
### make full table
temp <- expand_grid(.imp = unique(afcars_long$.imp),
                    state = unique(afcars_long$state),
                    year = unique(afcars_long$year),
                    age = unique(afcars_long$age),
                    race_ethn = unique(afcars_long$race_ethn)) 

out_st <- temp |> 
  left_join(afcars_rem)  |>  
  left_join(afcars_tpr) |> 
  mutate(first_tpr = ifelse(is.na(first_tpr), 0, first_tpr),
         first_entry = ifelse(is.na(first_entry), 0, first_entry))

out_nat <- temp |> 
  left_join(afcars_rem)  |>  
  left_join(afcars_tpr) |> 
  mutate(first_tpr = ifelse(is.na(first_tpr), 0, first_tpr),
         first_entry = ifelse(is.na(first_entry), 0, first_entry)) |> 
  group_by(.imp, year, age, race_ethn) |> 
  summarize(first_entry = sum(first_entry),
            first_tpr = sum(first_tpr))



# output ------------------------------------------------------------------
write_csv(out_st, "./data/afcars_first_event_state.csv")
write_csv(out_nat, "./data/afcars_first_event_national.csv")

