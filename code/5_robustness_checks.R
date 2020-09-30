##-------------------------------------------------------------------------------------------------
## replication of model results from Maltritz, table 2

# baseline (1999-2009) "in times of crisis"
moddat.ey %>% 
  filter(year %in% c(1999:2009)) %>% 
  setup_model() %>% 
  run_model() %>%
  model_results_as_table()

# lagged (1999-2009)
moddat.lag %>% 
  filter(year %in% c(1999:2009)) %>% 
  setup_model() %>% 
  run_model() %>%
  model_results_as_table()

# pre-crisis (1999-2007)
moddat.ey %>% 
  filter(year %in% c(1999:2007)) %>% 
  setup_model() %>% 
  run_model() %>%
  model_results_as_table()

##-------------------------------------------------------------------------------------------------
## actual robustness checks, table a3 in appendix

# # unmodified variables (not relative to Germany)
# mod.BMS.unmodified <- moddat_notrel.ey %>%
#   filter(year %in% c(1999:2009)) %>%
#   setup_model() %>% run_model()
# model_results_as_table(mod.BMS.unmodified, F)

# check if robust to model prior (e.g. change to "random" model prior)
tmp <- moddat.ey %>%
  filter(year %in% c(1999:2019)) %>%
  setup_model() %>% as.matrix

bms(tmp, burn = 1000, iter = 10000, g = "BRIC", mprior = "random", mcmc = "bd") %>% 
  model_results_as_table(); rm(tmp)

