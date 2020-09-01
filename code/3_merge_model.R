
p_load(BMS)

dat <- merge(dep, exp, by = c("year", "iso3"))
summary(dat)

#clean data, remove NA
dat <- na.omit(dat)

#model, first try with BMS pack

moddat <- as.matrix(dat %>% 
                      #filter(year %in% c(2015:2019)) %>% 
                      select(spr, var_names) %>% 
                      select(-gdp))

mod.BMS <- bms(moddat, burn = 2000, iter = 10000, mprior = "random", mcmc = "bd"); summary(mod.BMS)
