# create first event annual counts by race/ethn for removal and TPR -------
# by state ----------------------------------------------------------------
# updated 6/2/25 ---------------------------------------------------------

rm(list = ls()); gc()
library(tidyverse)
library(tidycensus)
library(mice)

#### grab imputed data
afcars_path<-"~/Projects/ndacan_processing/imputations/afcars"
afcars_files<-list.files(afcars_path,
                         full.names = T)

afcars_ab <- afcars_files[1:6]
afcars_reg <- afcars_files[7:14]
afcars_files<-c(afcars_reg, afcars_ab)

afcars_imps <- afcars_files |> 
  map(read_rds)

# make long frames --------------------------------------------------------

afcars_long <- afcars_imps |> 
  map(mice::complete, action = "long") |> 
  bind_rows()

# recode white to nh white alone
afcars_long <- afcars_long |> 
  mutate(white_nh = 
           white == 1 & 
           (hisorgin == 0 & amiakn == 0 &
              asian == 0 & blkafram == 0 & 
              hawaiipi == 0))

# make first removal counts -----------------------------------------------

afcars_rem <- afcars_long |>  
  filter(totalrem==1, entered==1) |> 
  group_by(.imp, state, year, age) |> 
  summarise(
    first_entry_total = n(),
    first_entry_amiakn = sum(amiakn == 1),
    first_entry_asian = sum(asian == 1),
    first_entry_blkafram = sum(blkafram == 1),
    first_entry_hawaiipi = sum(hawaiipi == 1),
    first_entry_white = sum(white_nh == 1),
    first_entry_his = sum(hisorgin == 1))

# make first full tpr counts -----------------------------------------------
afcars_tpr <- afcars_long  |>  
  filter(istpr==1) |>  
  group_by(.imp) |> 
  distinct(stfcid, .keep_all = T)

afcars_tpr <- afcars_tpr |> 
  group_by(.imp, state, year, age) |> 
  summarise(
    tpr_total = n(),
    tpr_amiakn = sum(amiakn == 1),
    tpr_asian = sum(asian == 1),
    tpr_blkafram = sum(blkafram == 1),
    tpr_hawaiipi = sum(hawaiipi == 1),
    tpr_white = sum(white_nh == 1),
    tpr_his = sum(hisorgin == 1))

# pivot -------------------------------------------------------------------
afcars_rem<- afcars_rem |> 
  pivot_longer(cols = first_entry_total:first_entry_his,
               names_to = "race_ethn",
               values_to = "first_entry",
               names_prefix = "first_entry_")

afcars_tpr<- afcars_tpr |> 
  pivot_longer(cols = tpr_total:tpr_his,
               names_to = "race_ethn",
               values_to = "first_tpr",
               names_prefix = "tpr_")

# join --------------------------------------------------------------------
### make full table
temp <- expand_grid(.imp = 1:5,
                    state = unique(afcars_long$state),
                    year = 2016:2023,
                    age = 0:17,
                    race_ethn = c("amiakn", "asian", "blkafram",
                                  "hawaiipi", "white", "his", "total")) 

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

xwalk <- fips_codes |> 
  select(state, state_code) |> 
  distinct() |> 
  mutate(state_code = as.numeric(state_code))
#### start here to modify race classification schema
out_st <- out_st |> 
  rename(state_code = state) |> 
  mutate(state_code = as.numeric(state_code)) |> 
  left_join(xwalk) 

# output ------------------------------------------------------------------
write_csv(out_st, "./data/afcars_first_event_state.csv")
write_csv(out_nat, "./data/afcars_first_event_national.csv")

