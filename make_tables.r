#### make_tables.r
#### project: cps_cumulative_25
#### Author: Frank Edwards
#### Email:  frank.edwards@rutgers.edu
#### repo: 
#
# log: make period life tables 
#
#----------------------------------------
source("lifetable.r")


# make long by outcome ----------------------------------------------------
tab_nat <- dat_nat |> 
  pivot_longer(
    cols = starts_with("first_"),
    names_to = "varname",
    values_to = "var")
# parameters for national tables ------------------------------------------
vars<-unique(tab_nat$varname)
race<-unique(tab_nat$race_ethn)
years<-unique(tab_nat$year)
imps <- unique(tab_nat$.imp)
tables_out<-list()
# loop over parameters, estimate national tables --------------------------
counter<-0
for(h in 1:length(vars)){
  for(i in imps){
    for(j in 1:length(years)){
      for(k in 1:length(race)){
        counter<-counter + 1
        print(counter)

        temp<-tab_nat |>
          filter(.imp == i,
                 varname == vars[h],
                 year == years[j],
                 race_ethn == race[k])

        tables_out[[counter]]<-make_life_table(temp)
      }
    }
  }
}

tables <- bind_rows(tables_out)

tables <- tables |> 
  mutate(q_se = sqrt(q * (1-q)/pop),
         c_se = sqrt(c * (1-c)/pop))
#### combine across imps
tables_within<-tables %>%
  group_by(year, race_ethn, age, varname) %>%
  summarise(q_mn = mean(q),
            c_mn = mean(c),
            q_v_within = mean(q_se^2),
            c_v_within = mean(c_se^2))

tables_between<-tables %>%
  left_join(tables_within) %>%
  group_by(year, race_ethn, age, varname) %>%
  summarise(
    q_v_between = mean((q - q_mn)^2),
    c_v_between = mean((c - c_mn)^2))

tables_comb<-tables_within %>%
  left_join(tables_between) %>%
  mutate(c_se = sqrt(c_v_within + (1 + 1/5)*c_v_between),
         q_se = sqrt(q_v_within + (1 + 1/5)*q_v_between)) %>%
  select(year, varname, race_ethn, age, q_mn, q_se, c_mn, c_se)

tables_comb<-tables_comb %>%
  mutate(
    q_max = q_mn + 2 * q_se,
    q_min = q_mn - 2 * q_se,
    c_max = c_mn + 2 * c_se,
    c_min = c_mn - 2 * c_se) %>%
  mutate(c_max = ifelse(c_max>1, 1, c_max),
         c_min = ifelse(c_min<0, 0, c_min),
         q_max = ifelse(q_max>1, 1, q_max),
         q_min = ifelse(q_min<0, 0, q_min))

write_csv(tables_comb, "./vis/tables_pep_nat.csv")
