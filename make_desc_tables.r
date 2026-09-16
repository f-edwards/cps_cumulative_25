#### TITLE 
#### project: 
#### Author: Frank Edwards
#### Email:  frank.edwards@rutgers.edu
#### repo: 
#
# log: 
#
#----------------------------------------

library(tidyverse)

dat <- read_csv("./vis/tables_pep_nat.csv") |> 
  mutate(race_ethn = case_when(
    race_ethn == "Haw/PI" ~ "NHPI",
    T ~ race_ethn
  )) |> 
  mutate(race_ethn = factor(
    race_ethn,
    levels = c("Total",
               "AIAN",
               "Asian",
               "Black",
               "Hispanic",
               "NHPI",
               "White"))) |> 
  mutate(
    q_mn = round(q_mn, 4),
    q_se = round(q_mn, 4),
    c_mn = round(c_mn, 4),
    c_se = round(c_mn, 4)) |> 
  select(year, varname, race_ethn, age, 
         q_mn, q_se, c_mn, c_se) |> 
  write_csv("./vis/lt_rounded.csv")

t2016 <- dat |> 
  filter(year==2016,
         age == 17) |> 
  rename(c_mn2016 = c_mn) |> 
  select(varname, race_ethn, age, c_mn2016)

t2023 <- dat |> 
  filter(year==2023,
         age == 17) |> 
  rename(c_mn2023 = c_mn) |> 
  select(varname, race_ethn, age, c_mn2023)

delta <- t2023 |> 
  left_join(t2016) |> 
  mutate(delta = (c_mn2023 - c_mn2016),
         pct_change_delta =  delta / c_mn2016,
         prop_baseline = c_mn2023/c_mn2016) |> 
  write_csv("./vis/delta.csv")

afcars_nat <- read_csv("./data/afcars_first_event_national.csv")
ncands_nat <- read_csv("./data/ncands_first_event_national.csv")

ncands_nat |> 
  group_by(subyr, race_ethn, .imp) |> 
  summarize(first_inv = sum(first_inv),
            first_sub = sum(first_sub)) |> 
  summarize(first_inv_min = min(first_inv),
            first_inv_max = max(first_inv),
            first_inv_mean = mean(first_inv),
            first_sub_min = min(first_sub),
            first_sub_max = max(first_sub),
            first_sub_mean = mean(first_sub)) |> 
  filter(subyr>=2016) |> 
  write_csv("./vis/ncands_desc.csv")

afcars_nat |> 
  group_by(year, race_ethn, .imp) |> 
  summarize(first_entry = sum(first_entry),
            first_tpr = sum(first_tpr)) |> 
  summarize(first_entry_min = min(first_entry),
            first_entry_max = max(first_entry),
            first_entry_mean = mean(first_entry),
            first_tpr_min = min(first_tpr),
            first_tpr_max = max(first_tpr),
            first_tpr_mean = mean(first_tpr)) |> 
  filter(year>=2016) |> 
  write_csv("./vis/afcars_desc.csv")


