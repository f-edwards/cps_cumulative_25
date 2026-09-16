#### make_all.R
#### project: cps_cumulative_25
#### Author: Frank Edwards
#### Email:  frank.edwards@rutgers.edu
# readme: takes NCANDS, AFCARS, SEER, produces needed tables for analysis
# log: created 5/30/25
#

# libraries -------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
# first event indices -----------------------------------------------------
# if ./data/first_sub.csv and first_inv.csv not created
# source("make_ncands_first.r")


# convenience functions ---------------------------------------------------

recode_race <- function(x){
  x <- x |> 
    mutate(race_ethn = 
             case_when(
               race_ethn %in% c("tot", "total") ~ "Total",
               race_ethn %in% c("ai", "amiakn")  ~ "AIAN",
               race_ethn %in% c("asian", "as") ~ "Asian",
               race_ethn %in% c("blkafram", "blk", "bl") ~ "Black",
               race_ethn %in% c("hawaiipi", "nh") ~ "Haw/PI",
               race_ethn %in% c("his") ~ "Hispanic",
               race_ethn %in% c("white", "wh_nh", "wh", "White") ~ "White"
             ))
  return(x)
}

# pre-process data for time series-----------------------------------------
# generates csv files for life table estimation
# make_afcars_first handles this, NCANDS requires chid rptid linkage 
# for dedup
# source("process_imputed_ncands_first_event.r")
# source("process_imputed_afcars_first_event.r")
# source("process_pop.r")

# read processed data -----------------------------------------------------
afcars_nat <- read_csv("./data/afcars_first_event_national.csv")
ncands_nat <- read_csv("./data/ncands_first_event_national.csv")

pop_pep_nat <- read_csv("./data/pop_pep_nat.csv")
pop_seer_nat <- read_csv("./data/pop_seer_nat.csv",
                         col_types = "nncn")
### pull white from seer, pull total from seer
pop_seer_nat <- pop_seer_nat |> 
  bind_rows(
  pop_seer_nat |> 
  group_by(year, age) |> 
  summarize(pop = sum(pop)) |> 
  mutate(race_ethn = "tot")) |> 
  filter(race_ethn %in% c("tot", "White"))

pop_nat <- pop_pep_nat |> 
  filter(race_ethn != "wh") |> 
  bind_rows(pop_seer_nat) |> 
  arrange(year, age, race_ethn) |> 
  filter(year>=2010) |> 
  write_csv("./vis/pop_tab.csv")

# harmonize and join ------------------------------------------------------
dat_nat <- afcars_nat |> 
  recode_race() |> 
  left_join(pop_nat |> 
              recode_race() |> 
              mutate(year = year + 1)) |> # use lagged pop because missing 2023 pop data, remove when obtained
  left_join(ncands_nat |> 
              recode_race() |> 
              rename(year = subyr))
  # add ncands after postprocess
 # use lagged pop because missing 2023 pop data, remove when obtained
# add ncands after postprocess


# estimate life tables ----------------------------------------------------
source("make_tables.r")


# generate visuals --------------------------------------------------------
source("make_vis.r")

