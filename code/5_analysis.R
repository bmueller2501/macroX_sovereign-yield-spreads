
##-------------------------------------------------------------------------------------------------
## analysing our main model, whole period: 1999-2019

mod.BMS <- dat_ey %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS, F)

# coef(mod.BMS, exact = TRUE) # exact=TRUE: This means that the marginal likelihoods are used for BMA

# looks good
plotConv(mod.BMS)
cor(pmp.bma(mod.BMS))

# inclusion and signs of coef of the best 500 models
image(mod.BMS[1:500])

plotModelsize(mod.BMS) # plot the prior and posterior model size distribution (see result)

beta.draws.bma(mod.BMS[1:5]) # produces the posterior expected coefficient values for the best 5 models (see result)


##-------------------------------------------------------------------------------------------------
## potential outputs

#1999-2009, Maltritz variables, end-of-year
mod.BMS.ey_9909 <- dat_ey %>% 
  filter(year %in% c(1999:2009)) %>% 
  setup_model() %>% run_model()
print(xtable(model_results_as_table(mod.BMS.ey_9909, F)), include.rownames=FALSE)

#1999-2007, Maltritz variables, end-of-year
mod.BMS.ey_9907 <- dat_ey %>% 
  filter(year %in% c(1999:2007)) %>% 
  setup_model() %>% run_model()
print(xtable(model_results_as_table(mod.BMS.ey_9907, F)), include.rownames=FALSE)

#1999-2019, Maltritz variables, end-of-year
mod.BMS.base_9919 <- dat_ey %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS.base_9919, F)
print(xtable(model_results_as_table(mod.BMS.base_9919)), include.rownames=FALSE)

#2008-2019, Maltritz variables, end-of-year
mod.BMS.base_0819 <- dat_ey %>% 
  filter(year %in% c(2008:2019)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS.base_0819, F)
print(xtable(model_results_as_table(mod.BMS.base_0819)), include.rownames=FALSE)

#2010-2019, Maltritz variables, end-of-year
mod.BMS.base_1019 <- dat_ey %>% 
  filter(year %in% c(2010:2019)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS.base_1019, F)
print(xtable(model_results_as_table(mod.BMS.base_1019)), include.rownames=FALSE)

#1999-2019, extended variables, end-of-year
mod.BMS.ext_9919 <- dat_ey_ext %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS.ext_9919, T)
print(xtable(model_results_as_table(mod.BMS.ext_9919)), include.rownames=FALSE)

#2008-2019, extended variables, end-of-year
mod.BMS.ext_0819 <- dat_ey_ext %>% 
  filter(year %in% c(2008:2019)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS.ext_0819, T)
print(xtable(model_results_as_table(mod.BMS.ext_0819)), include.rownames=FALSE)

#2014-2019, extended variables, end-of-year
mod.BMS.ext_1419 <- dat_ey_ext %>% 
  filter(year %in% c(2014:2019)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS.ext_1419, T)
print(xtable(model_results_as_table(mod.BMS.ext_1419)), include.rownames=FALSE)

#1999-2007, extended variables, end-of-year
mod.BMS.ext_9907 <- dat_ey_ext %>% 
  filter(year %in% c(1999:2007)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS.ext_9907, T)
print(xtable(model_results_as_table(mod.BMS.ext_9907)), include.rownames=FALSE)


##-------------------------------------------------------------------------------------------------
## potential outputs

set.seed(1)
#1999-2007, Maltritz variables, end-of-year
mod.BMS.ey_9907 <- dat_ey %>% 
  filter(year %in% c(1999:2007)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS.ey_9907, F)
print(xtable(model_results_as_table(mod.BMS.ey_9907, F) %>% select(PI, DI)), include.rownames=FALSE)

set.seed(2)
#2008-2019, Maltritz variables, end-of-year
mod.BMS.ey_0819 <- dat_ey %>% 
  filter(year %in% c(2008:2019)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS.ey_0819, F)
print(xtable(model_results_as_table(mod.BMS.ey_0819, F) %>% select(PI, DI)), include.rownames=FALSE)

set.seed(3)
#1999-2019, Maltritz variables, end-of-year
mod.BMS.ey_9919 <- dat_ey %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS.ey_9919, F)
print(xtable(model_results_as_table(mod.BMS.ey_9919, F) %>% select(PI, DI)), include.rownames=FALSE)


