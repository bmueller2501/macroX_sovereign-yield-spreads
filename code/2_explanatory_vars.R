
##-------------------------------------------------------------------------------------------------
## load data on explanatory variables

vars <- getSheetNames("./input/full_data.xlsx")

dat_list <- list()
for (i0 in 1:length(vars)){
  tmp <- openxlsx::read.xlsx("./input/full_data.xlsx", 
                                       sheet = i0, colNames = TRUE,
                                       na.strings = "NA")
  
  # store current sheet into full_data_list
  dat_list[[i0]] <- melt(tmp, id = "year", variable = "iso3")
  # adjust file types
  dat_list[[i0]]$year <- as.integer(dat_list[[i0]]$year)
  dat_list[[i0]]$iso3 <- as.character(dat_list[[i0]]$iso3)
  dat_list[[i0]][[paste0(vars[i0])]] <- as.numeric(dat_list[[i0]]$value)
  dat_list[[i0]] <- dat_list[[i0]] %>% select(-value)
}
rm(tmp, i0)

dat <- Reduce(merge, dat_list); rm(dat_list)
dat$iso3 <- countrycode(dat$iso3, "country.name", "iso3c")

# filter for countries that joined the EMU in 1999
dat <- dat %>% filter(iso3 %in% j99) %>% as.data.table


##-------------------------------------------------------------------------------------------------
## do some variable corrections

# calculate trade balance relative to GDP
dat$tb <- dat$tb/dat$gdp

# calculate openness relative to GDP
dat$openness <- dat$openness/dat$gdp

# calculate total outstanding debt relative to EMU total outstanding debt
dat <- data.table(dat)
dat[, debt_ea := debt/sum(debt), by = year]

# # calculate terms of trade growth for tot_oecd
# dat <- dat %>% 
#   group_by(iso3) %>% 
#   # 0 for now until we have data for 1998
#   mutate(tot_g = tot_oecd - lag(tot_oecd))
# use terms of trade instead of tot growth

# PSPP
# comment out line 1 and line 2 if you want it in absolute terms
# line 1 only if you want it relative to GDP
# line 2 only if you want it as dummy variable
dat$pspp <- (dat$pspp/1000)/dat$gdp #(1) relative to GDP
# dat$pspp <- ifelse(dat$year >= 2015,1,0) #(2) dummy


##-------------------------------------------------------------------------------------------------
## calculate values relative to germany

dat_notrel <- dat

deu <- subset(dat, iso3 == "DEU")
vars <- names(dat)[!names(dat) %in% c("year", "iso3",
                                      "i_us", "i_us_ey",
                                      "baa", "baa_ey",
                                      "bbb", "bbb_ey",
                                      "emu")]

for(v in vars) {
  for(y in unique(dat$year)) {
    dat[[paste0(v)]][dat$year == y] <- 
      dat[[paste0(v)]][dat$year == y]-deu[[paste0(v)]][deu$year == y]
  }
}

rm(deu)


##-------------------------------------------------------------------------------------------------
## create data sets of explanatory variables: end-of-year, yearly average lagged

# Maltritz variables, end-of-year
exp_ey <- subset(dat, year >= 1999) %>%
  select(year, iso3, 
         bb, debt_ratio, i_avg, g, inf, inf_var, gcf, tb, 
         openness, tot_oecd, debt, debt_ea, i_us_ey, bbb_ey) %>%
  rename(i_us = "i_us_ey", bbb = "bbb_ey") # rename that it is same to exp_lag

# Maltritz variables, average lagged
exp_lag <- subset(dat, year >= 1999) %>%
  select(year, iso3, 
         bb, debt_ratio, i_avg, g, inf, inf_var, gcf, tb, 
         openness, tot_oecd, debt, debt_ea, i_us, bbb) %>%
  mutate(year = year + 1) %>%
  filter(year < max(year))



# extended variables, end-of-year
exp_ext_ey <- subset(dat, year >= 1999) %>%
  select(year, iso3, 
         bb, debt_ratio, i_avg, g, inf, inf_var, gcf, tb, 
         openness, tot_oecd, debt, debt_ea, i_us_ey, bbb_ey,
         pensions, reer, pspp) %>% # exclude cds
  rename(i_us = "i_us_ey", bbb = "bbb_ey")

# extended variables, average lagged
exp_ext_lag <- subset(dat, year >= 1999) %>%
  select(year, iso3, 
         bb, debt_ratio, i_avg, g, inf, inf_var, gcf, tb, 
         openness, tot_oecd, debt, debt_ea, i_us_ey, bbb_ey,
         pensions, reer, pspp) %>%
  mutate(year = year + 1) %>%
  filter(year < max(year))



# not relative to germany, will not present that, robustness check in Maltritz
exp_ey_notrel <- subset(dat_notrel, year >= 1999) %>%
  select(year, iso3, 
         bb, debt_ratio, i_avg, g, inf, inf_var, gcf, tb, 
         openness, tot_oecd, debt, debt_ea, i_us_ey, bbb_ey) %>%
  rename(i_us = "i_us_ey", bbb = "bbb_ey")


rm(dat, dat_notrel)


