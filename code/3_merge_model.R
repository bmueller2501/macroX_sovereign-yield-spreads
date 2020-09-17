# merge the data into one data.table
dat <- merge(dep, exp, by = c("year", "iso3"))
dat_lagged <- merge(dep, exp_lagged, by = c("year", "iso3"))

# remove cds for now since we don't have full coverage
dat <- dat %>% select(-cds)
dat_lagged <- dat_lagged %>% select(-cds)
vars <- vars[vars != "cds"] 

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
                     tot_eiu,
                     tot_imf,
                     emu)))
}
run_model <- function(data){
  moddat <- as.matrix(data)
  
  return(bms(moddat, burn = 1000, iter = 20000, g = "BRIC", mprior = "uniform", mcmc = "bd"))
}

moddat <- dat_rel %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model()

mod.BMS <- run_model(moddat)

coef(mod.BMS, exact = TRUE) # exact=TRUE: This means that the marginal likelihoods are used for BMA
