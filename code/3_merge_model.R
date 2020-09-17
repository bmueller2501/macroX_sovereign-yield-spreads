# merge the data into one data.table
dat <- merge(dep, exp, by = c("year", "iso3"))
summary(dat)

# remove cds for now since we don't have full coverage
dat <- dat %>% select(-cds)
vars <- vars[vars != "cds"] 

# clean data, remove NA
dat <- na.omit(dat)
dat_rel <- dat

# calc values relative to Germany
deu <- subset(dat_rel, iso3 == "DEU")
for(v in vars) {
  for(y in unique(dat_rel$year)) {
    dat_rel[[paste0(v)]][dat_rel$year == y] <- 
      dat_rel[[paste0(v)]][dat_rel$year == y]-deu[[paste0(v)]][deu$year == y]
  }
}
rm(deu)

# model, first try with BMS pack and data relative to Germany

setup_model <- function(data_rel){
  return(dat_rel %>% 
           filter(iso3 != "DEU") %>% 
           select(spr, vars) %>%
           select(-c(pspp,
                     i_us,
                     baa,
                     gdp)))
}

moddat <- dat_rel %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model()

moddat <- as.matrix(moddat)

mod.BMS <- bms(moddat, burn = 100000, iter = 200000, g = "BRIC", mprior = "uniform", mcmc = "bd"); summary(mod.BMS)

coef(mod.BMS, exact = TRUE) # exact=TRUE: This means that the marginal likelihoods are used for BMA