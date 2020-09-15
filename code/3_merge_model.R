
dat <- merge(dep, exp, by = c("year", "iso3"))
summary(dat)

#clean data, remove NA
dat <- na.omit(dat)
dat_rel <- dat

#calc vaules relative to germany
deu <- subset(dat_rel, iso3 == "DEU")
for(v in vars) {
  for(y in unique(dat_rel$year)) {
    # v <- vars[1]; y <- 1999 #for testing
    dat_rel[[paste0(v)]][dat_rel$year == y] <- 
      dat_rel[[paste0(v)]][dat_rel$year == y]-deu[[paste0(v)]][deu$year == y]
  }
}
rm(deu)


#subset countries joined 1999
j99 <- c("AUT", "BEL", "DEU", "ESP", "FIN", "FRA", "ITA", "NLD", "PRT", "IRL", "LUX")


#model, first try with BMS pack and data relative to germany
moddat <- dat_rel %>% 
  filter(year %in% c(1999:2007), iso3 != "DEU") %>% 
  filter(iso3 %in% j99) %>%
  select(spr, vars) %>%
  select(-c(gdp, i_us, baa, pspp))

moddat <- as.matrix(moddat)

mod.BMS <- bms(moddat, burn = 2000, iter = 10000, mprior = "random", mcmc = "bd"); summary(mod.BMS)
