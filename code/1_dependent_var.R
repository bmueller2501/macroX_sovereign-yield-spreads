
library(pacman)
p_load(readxl)
p_load(dplyr)
p_load(tidyverse)
p_load(countrycode)
p_load(data.table)


#load data
dat <- readxl::read_xls("./input/spreads.xls")
c <- colnames(dat[,2:ncol(dat)])

#edit
dat <- dat %>% 
  mutate(year = as.integer(substr(as.character(dat$date), 1, 4))) %>% 
  select(-date)

dat <- gather(data = dat, key = "iso3", value = "spr", c) %>%
  mutate(iso3 = countrycode(iso3, "iso2c", "iso3c")) %>%
  as.data.table

dat <- dat %>% filter(is.na(year) == F); summary(dat) #some days are missing

dat <- dat[, .(spr = mean(spr, na.rm = T)), by = .(year, iso3)]

dep <- dat
rm(dat)
