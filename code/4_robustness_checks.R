
##-------------------------------------------------------------------------------------------------
## replication of model results in paper, talbe 2

# baseline (1999-2009) "in times of crisis"
mod.BMS.baseline <- dat_ey %>% 
  filter(year %in% c(1999:2009)) %>% 
  setup_model() %>% run_model()

model_results_as_table(mod.BMS.baseline, F)

# lagged (1999-2009)
mod.BMS.lagged <- dat_lag %>% 
  filter(year %in% c(1999:2009)) %>% 
  setup_model() %>% run_model()

model_results_as_table(mod.BMS.lagged, F)

# pre-crisis (1999-2007)
mod.BMS.precrisis <- dat_ey %>% 
  filter(year %in% c(1999:2007)) %>% 
  setup_model() %>% run_model()

model_results_as_table(mod.BMS.precrisis, F)


##-------------------------------------------------------------------------------------------------
## actual robustness checks, table a3 in appendix

# unmodified variables (not relative to Germany)
mod.BMS.unmodified <- dat_ey_notrel %>%
  filter(year %in% c(1999:2009)) %>%
  setup_model() %>% run_model()

model_results_as_table(mod.BMS.unmodified, F)

# check if robust to model prior (e.g. change to "random" model prior)
moddat <- dat_ey %>%
  filter(year %in% c(1999:2019)) %>%
  setup_model()

mod.BMS.g.random <- bms(as.matrix(moddat), burn = 1000, iter = 10000, g = "BRIC", mprior = "random", mcmc = "bd")
model_results_as_table(mod.BMS.g.random, F)







