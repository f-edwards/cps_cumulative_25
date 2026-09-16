#### wrangle imputed ncands into geographic unit time-series for 
#### total and first events
#### last edit 5/27/25

library(tidyverse)
library(data.table)
library(mice)

first_inv_index<-read_csv("./data/first_inv.csv")
first_victim_index<-read_csv("./data/first_sub.csv")

### grab filenames and format
files<-list.files("~/Projects/ndacan_processing/imputations/ncands_SAP_imputed",
                  full.names = T)

### batch process with mice::complete

nat_out<-list()
state_out<-list()

state_frame <- expand_grid(
  .imp = 1:5,
  staterr = unique(first_inv_index$staterr),
  subyr = 2010:2023,
  age = 0:17,
  race_ethn = c("tot", "ai", "as", "nh", 
                "bl", "wh_nh", "his"))

nat_frame <- expand_grid(
  .imp = 1:5,
  subyr = 2010:2023,
  age = 0:17,
  race_ethn = c("tot", "ai", "as", "nh",
                "bl", "wh_nh", "his"))

for(i in 1:length(files)){
  print(i)
  # read imputed, subset to only first_inv, first_sub from index
  imps<-readRDS(files[i])
  temp<-mice::complete(imps, action = "long", include = F)
  # create nh white
  temp <- temp |> 
    mutate(
      chracwh_nh = case_when(
        chracwh == 1 & cethn == 2 ~ 1,
        T ~ 2))
  
  first_inv<- temp |> 
    semi_join(first_inv_index) |> 
    group_by(staterr, .imp) |> 
    distinct(chid, .keep_all = T)
  
  first_victim<- temp |>
    semi_join(first_victim_index) |> 
    group_by(staterr, .imp) |> 
    distinct(chid, .keep_all = T)
  
  rm(imps); rm(temp); gc()
  #### aggregate temp to nat and state
  ### handle alone or combined
  nat_inv <- first_inv |> 
    group_by(.imp, age, subyr) |> 
    summarize(first_inv_tot = n(),
              first_inv_ai = sum(chracai == 1),
              first_inv_as = sum(chracas == 1),
              first_inv_nh = sum(chracnh == 1),
              first_inv_bl = sum(chracbl == 1),
              first_inv_wh_nh = sum(chracwh_nh == 1),
              first_inv_his = sum(cethn == 1)) |> 
    pivot_longer(cols = first_inv_tot:first_inv_his,
                 names_to = "race_ethn", 
                 values_to = "first_inv") |> 
    mutate(race_ethn = str_sub(race_ethn,
                               11, -1))
  
  nat_victim <- first_victim |> 
    group_by(.imp, age, subyr) |> 
    summarize(first_vic_tot = n(),
              first_vic_ai = sum(chracai == 1),
              first_vic_as = sum(chracas == 1),
              first_vic_nh = sum(chracnh == 1),
              first_vic_bl = sum(chracbl == 1),
              first_vic_wh_nh = sum(chracwh_nh == 1),
              first_vic_his = sum(cethn == 1))|> 
    pivot_longer(cols = first_vic_tot:first_vic_his,
                 names_to = "race_ethn", 
                 values_to = "first_sub") |> 
    mutate(race_ethn = str_sub(race_ethn,
                               11, -1))
  
  nat <- nat_inv |> 
    left_join(nat_victim)
  
  state_inv <- first_inv |> 
    group_by(.imp, staterr, age, subyr) |> 
    summarize(first_inv_tot = n(),
              first_inv_ai = sum(chracai == 1),
              first_inv_as = sum(chracas == 1),
              first_inv_nh = sum(chracnh == 1),
              first_inv_bl = sum(chracbl == 1),
              first_inv_wh_nh = sum(chracwh_nh == 1),
              first_inv_his = sum(cethn == 1)) |> 
    pivot_longer(cols = first_inv_tot:first_inv_his,
                 names_to = "race_ethn", 
                 values_to = "first_inv") |> 
    mutate(race_ethn = str_sub(race_ethn,
                               11, -1))
  
  state_victim <- first_victim |> 
    group_by(.imp, staterr, age, subyr) |> 
    summarize(first_vic_tot = n(),
              first_vic_ai = sum(chracai == 1),
              first_vic_as = sum(chracas == 1),
              first_vic_nh = sum(chracnh ==1),
              first_vic_bl = sum(chracbl == 1),
              first_vic_wh_nh = sum(chracwh_nh == 1),
              first_vic_his = sum(cethn == 1))|> 
    pivot_longer(cols = first_vic_tot:first_vic_his,
                 names_to = "race_ethn", 
                 values_to = "first_sub") |> 
    mutate(race_ethn = str_sub(race_ethn,
                               11, -1))
  
  state <- state_inv |> 
    left_join(state_victim)
  
  nat_out[[i]] <- nat
  state_out[[i]] <- state
  
}

# each imputation is random sample of year
# index files ensure only first events
# sum over by year, imp, age, race
# for totals
nat_merge <- nat_out |> 
  bind_rows() |> 
  group_by(.imp, age, subyr, race_ethn) |> 
  summarize(first_inv = sum(first_inv),
            first_sub = sum(first_sub))

nat_merge <- nat_frame |> 
  left_join(nat_merge) |> 
  mutate(first_inv = ifelse(is.na(first_inv), 0, first_inv),
         first_sub = ifelse(is.na(first_sub), 0, first_sub))


state_merge <- state_out |> 
  bind_rows() |> 
  group_by(.imp, age, subyr, staterr, race_ethn) |> 
  summarize(first_inv = sum(first_inv),
            first_sub = sum(first_sub))

state_merge <- state_frame |> 
  left_join(state_merge) |> 
  mutate(first_inv = ifelse(is.na(first_inv), 0, first_inv),
         first_sub = ifelse(is.na(first_sub), 0, first_sub))

write_csv(nat_merge, "./data/ncands_first_event_national.csv")
write_csv(state_merge, "./data/ncands_first_event_state.csv")