library(tidyverse)

pop<-read_delim("~/Projects/data/censusdata_race5.csv",
                col_names = c(
                  "year", "state", "county", "race5",
                  "hisp", "age", "pop"))

### create race variable for alone or combined
# harmonize with ncands counts
# Race5
# 7 = White, alone or in combination
# 8 = Black, alone or in combination
# 9 = American Indian/Alaska Native, alone or in combination
# 10 = Asian, alone or in combination
# 11 = Pacific Islander/Native Hawaiian, alone or in combination
# 
# Hisp
# 1 = no
# 2 = yes


pop <- pop |> 
  mutate(race_ethn = 
           case_when(
             race5 == 7 & (hisp ==  1) ~ "wh",
             race5 == 8 ~ "blk",
             race5 == 9 ~ "ai",
             race5 == 10 ~ "as",
             race5 == 11 ~ "nh",
             hisp == 2 ~ "his"
           ))

pop_st <- pop |> 
  filter(age<=18) |> 
  group_by(year, state, age, race_ethn) |> 
  summarise(pop = sum(pop)) |> 
  ungroup() |> 
  filter(year>=2000)

pop_nat <- pop |> 
  filter(age<=18) |> 
  group_by(year, age, race_ethn) |> 
  summarise(pop = sum(pop)) |> 
  filter(year>=2000)

write_csv(pop_st, "./data/pop_pep_st.csv")
write_csv(pop_nat, "./data/pop_pep_nat.csv")
