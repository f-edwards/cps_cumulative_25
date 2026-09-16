library(tidyverse)

pop<-read_fwf("~/Projects/data/us.1990_2023.singleages.through89.90plus.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")))

pop<-pop|>
  mutate(pop = as.integer(pop))|>
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AIAN", 
             race==4 ~ "API",
             hisp==1 ~ "Latinx")) 

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

write_csv(pop_st, "./data/pop_seer_st.csv")
write_csv(pop_nat, "./data/pop_seer_nat.csv")
