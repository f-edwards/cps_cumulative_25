#### make_vis.r
#### project: 
#### Author: Frank Edwards
#### Email:  frank.edwards@rutgers.edu
#### repo: 
#
# log: 
#
#----------------------------------------

# min notes
# For the estimates, do you have the overall estimates (i.e., for all children), for both the cumulative risks by age 18 and age-specific risks 0-17 by outcome and year? I don't see them in the output or in the figures. If so, could you send them along? 
# 
# I think most of this will already be on your list, but just in case, these are obvious things plus some thoughts/suggestions that are more substantial that we can discuss (in italics):
# For Cumulative Risk by 18 Plot:
# Re-order the panels so that from top left corner clockwise, they go: first_inv, first_sub, first_entry, first_tpr 
# Spell out outcome headers for each panel (Investigation, Confirmed maltreatment, Foster Care, Parental Rights Termination) - hopefully these truncated versions fit? The figure title will note that these are firsts 
# Switch the gradation so that most recent year is darkest and also so that legend reads down (2016 at top to 2022 at bottom)
# Capitalize "year" in horizontal axis
# Spell out "AIAN" and "API" in column headers
# Take up more lateral/horizontal space with the plots and add labels for each year (we have space)?
# Add whiskers for confidence intervals?
# Add all-children estimate (as 1st column of panels)


# For Age-Specific Plot:
# Spell out outcomes in row headers (Investigation, Confirmed maltreatment, Foster Care, Parental Rights Termination) - hopefully these truncated versions fit? The figure title will note that these are firsts 
# Re-order the rows so that the order of outcomes is: first_inv, first_sub, first_entry, first_tpr (so just bump first_entry down to third row)
# Capitalize "year" in the legend
# Switch the gradation so that most recent year is darkest and also so that legend reads down (2016 at top to 2022 at bottom)
# Capitalize "age" in horizontal axis
# Spell out "AIAN" and "API" in column headers
# Take up more lateral/horizontal space with the plots and add more age labels on horizontal axis (we have space)
# Add whiskers for confidence intervals?
# Add all-children estimates (as additional line)



library(tidyverse)

theme_set(theme_bw())

# pull lifetable data -----------------------------------------------------

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
               "White")))

dat <- dat |> 
  mutate(varname = case_when(
    varname == "first_inv" ~ "Investigation",
    varname == "first_sub" ~ "Confirmed maltreatment",
    varname == "first_entry" ~ "Foster care",
    varname == "first_tpr" ~ "Termination"
  )) |> 
  mutate(varname = 
           factor(varname,
                  levels = c(
                    "Investigation",
                    "Confirmed maltreatment",
                    "Foster care",
                    "Termination")))

# ggplot(dat,
#        aes(x = age,
#            y = q_mn * 100,
#            ymin = q_min * 100,
#            ymax = q_max * 100,
#            color = race_ethn,
#            fill = race_ethn,
#            group = race_ethn)) + 
#   geom_line() + 
#   geom_linerange() + 
#   facet_grid(varname~year,
#              scales = "free") + 
#   labs(x = "Age",
#        y = "Percent of population") +
#   theme(legend.position = "bottom")
# 
# ggsave("./vis/fig1.pdf")

ggplot(dat |> 
         filter(age == 17,
                varname %in% c("Investigation", 
                               "Confirmed maltreatment")),
       aes(x = year,
           color = race_ethn,
           fill = race_ethn,
           y = c_mn * 100,
           ymax = c_max * 100,
           ymin = c_min * 100)) + 
  geom_ribbon(color = NA,
              alpha = 0.5) + 
  geom_line() + 
  coord_cartesian(ylim = c(0, 60)) + 
  facet_wrap(~varname) +
  labs(x = "", y = "Cumulative risk",
       fill = "", color = "") + 
  theme(legend.position = "bottom") 

ggsave("./vis/fig1.png", width = 8, height = 5)

ggplot(dat |> 
         filter(age == 17,
                varname %in% c("Foster care", 
                               "Termination")),
       aes(x = year,
           color = race_ethn,
           fill = race_ethn,
           y = c_mn * 100,
           ymax = c_max * 100,
           ymin = c_min * 100)) + 
  geom_ribbon(color = NA,
              alpha = 0.5) + 
  geom_line() + 
  facet_wrap(~varname) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(x = "", y = "Cumulative risk",
       fill = "", color = "") + 
  theme(legend.position = "bottom") 

ggsave("./vis/fig2.png", width = 8, height = 5)
