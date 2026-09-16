rm(list=ls())
gc()
library(tidyverse)
library(lubridate)

ncands_files<-list.files("~/Projects/ndacan_data/ncands",
                         full.names = T)
bs
## run all years

id_out<-list()
for(i in 1:length(ncands_files)){
  t<-read_tsv(ncands_files[i])
  
  t<-t |> 
    rename_with(tolower) |> 
    select(subyr, staterr, chid, rptid, rptdt, chprior, rptvictim)
  
  t<-t |> 
    filter(chprior != 1 | is.na(chprior))
  
  id_out[[i]] <- t
}

# ### deduplicate
# all_data <- bind_rows(id_out)
# ### test if it's unique within states or across states
# all_data |> 
#   mutate(stID = paste(staterr, chid, sep="")) |> 
#   summarize(across = length(unique(chid)),
#             within = length(unique(stID)))

# identical. IDS are unique across states for this period

first_inv <- bind_rows(id_out) |> 
  mutate(rptdt = ymd(rptdt)) |> 
  arrange(rptdt) |> 
  group_by(staterr) |> 
  distinct(chid, .keep_all = T)|> 
  select(subyr, staterr, chid, rptid)

first_sub <- bind_rows(id_out) |> 
  mutate(rptdt = ymd(rptdt)) |> 
  arrange(rptdt) |> 
  group_by(staterr) |> 
  filter(rptvictim == 1) |> 
  distinct(chid, .keep_all = T) |> 
  select(subyr, staterr, chid, rptid)

# confirm
# first_inv[duplicated(first_inv$chid),]
# all_data[duplicated(all_data$chid),]
# looks fine

## output
write_csv(first_inv, "./data/first_inv.csv")
write_csv(first_sub, "./data/first_sub.csv")
