# merge the data into one data.table
dat <- merge(dep, exp, by = c("year", "iso3")); summary(dat)
dat_lagged <- merge(dep, exp_lagged, by = c("year", "iso3")); summary(dat_lagged)


# remove cds, pensions and pension_index for now since we don't have full coverage
dat <- dat %>% select(-c(cds, pensions, pensions_index))
dat_lagged <- dat_lagged %>% select(-c(cds, pensions, pensions_index))
vars <- vars[!(vars %in% c("cds", "pensions", "pensions_index"))] 

# clean data, remove NA
dat <- na.omit(dat)
dat_rel <- dat
dat_lagged <- na.omit(dat_lagged)
dat_lagged_rel <- dat_lagged

# calc values relative to Germany
deu <- subset(dat_rel, iso3 == "DEU")
deu_lagged <- subset(dat_lagged_rel, iso3 == "DEU")
for(v in vars) {
  if(v %in% c("i_us", "baa")){ next }
  
  for(y in unique(dat_rel$year)) {
    dat_rel[[paste0(v)]][dat_rel$year == y] <- 
      dat_rel[[paste0(v)]][dat_rel$year == y]-deu[[paste0(v)]][deu$year == y]
    
    if(y %in% unique(deu_lagged$year)){
      dat_lagged_rel[[paste0(v)]][dat_lagged_rel$year == y] <- 
        dat_lagged_rel[[paste0(v)]][dat_lagged_rel$year == y]-deu_lagged[[paste0(v)]][deu_lagged$year == y]
    }
  }
}
rm(deu, deu_lagged)

# model, first try with BMS pack and data relative to Germany

setup_model <- function(data){
  return(data %>% 
           filter(iso3 != "DEU") %>% 
           select(spr, vars) %>%
           select(-c(pspp,
                     reer,
                     gdp,
                     tot_oecd,
                     tot_imf,
                     emu)))
}

moddat <- dat_rel %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model()

moddat <- as.matrix(moddat)

mod.BMS <- bms(moddat, burn = 1000, iter = 20000, g = "BRIC", mprior = "uniform", mcmc = "bd")

coef(mod.BMS, exact = TRUE) # exact=TRUE: This means that the marginal likelihoods are used for BMA
