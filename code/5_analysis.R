
##-------------------------------------------------------------------------------------------------
## analysing our main model, 1999-2019, Maltritz variables, end-of-year

mod.BMS <- moddat.ey %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model() %>% run_model()
model_results_as_table(mod.BMS)

# coef(mod.BMS, exact = TRUE) # exact=TRUE: This means that the marginal likelihoods are used for BMA

# looks good
plotConv(mod.BMS)
cor(pmp.bma(mod.BMS))

# inclusion and signs of coef of the best 500 models
image(mod.BMS[1:500])

# plot the prior and posterior model size distribution (see result)
plotModelsize(mod.BMS) 

# produces the posterior expected coefficient values for the best 5 models (see result)
beta.draws.bma(mod.BMS[1:5]) 


##-------------------------------------------------------------------------------------------------
## potential outputs for the paper

#1999-2007, Maltritz variables, end-of-year
moddat.ey %>% 
  filter(year %in% c(1999:2007)) %>% 
  setup_model() %>% 
  run_model() %>%
  model_results_as_table()

#1999-2009, Maltritz variables, end-of-year
moddat.ey %>% 
  filter(year %in% c(1999:2009)) %>% 
  setup_model() %>% 
  run_model() %>%
  model_results_as_table()

#1999-2019, Maltritz variables, end-of-year
moddat.ey %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model() %>% 
  run_model() %>%
  model_results_as_table()

#2008-2019, Maltritz variables, end-of-year
moddat.ey %>% 
  filter(year %in% c(2008:2019)) %>% 
  setup_model() %>% 
  run_model() %>%
  model_results_as_table()


#1999-2019, extended variables, end-of-year
moddat_ext.ey %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model() %>% 
  run_model() %>%
  model_results_as_table()

#2008-2019, extended variables, end-of-year
moddat_ext.ey %>% 
  filter(year %in% c(2008:2019)) %>% 
  setup_model() %>% 
  run_model() %>%
  model_results_as_table()

#2014-2019, extended variables, end-of-year
moddat_ext.ey %>% 
  filter(year %in% c(2014:2019)) %>% 
  setup_model() %>% 
  run_model() %>%
  model_results_as_table()


#1999-2019, Maltritz variables, average lagged
moddat.lag %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model() %>% 
  run_model() %>%
  model_results_as_table()


##-------------------------------------------------------------------------------------------------
## actual outputs for the paper

## Table 1

#1999-2007, Maltritz variables, end-of-year
set.seed(1)
mod.BMS.ey_9907 <- moddat.ey %>% 
  filter(year %in% c(1999:2007)) %>% 
  setup_model() %>% run_model()
out1 <- model_results_as_table(mod.BMS.ey_9907)

#2008-2019, Maltritz variables, end-of-year
set.seed(2)
mod.BMS.ey_0819 <- moddat.ey %>% 
  filter(year %in% c(2008:2019)) %>% 
  setup_model() %>% run_model()
out2 <- model_results_as_table(mod.BMS.ey_0819) %>% select(PI, DI)

#1999-2019, Maltritz variables, end-of-year
set.seed(3)
mod.BMS.ey_9919 <- moddat.ey %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model() %>% run_model()
out3 <- model_results_as_table(mod.BMS.ey_9919) %>% select(PI, DI)

out <- cbind(out1, matrix(nrow = 14, ncol = 1), out2, matrix(nrow = 14, ncol = 1), out3)
print(xtable(out), include.rownames=FALSE)



## Table 2

#2008-2019, extended variables, end-of-year
set.seed(1)
mod.BMS.ext.ey_0819 <- moddat_ext.ey %>% 
  filter(year %in% c(2008:2019)) %>% 
  setup_model() %>% run_model()
out1 <- model_results_as_table(mod.BMS.ext.ey_0819)

#1999-2019, extended variables, end-of-year
set.seed(2)
mod.BMS.ext.ey_9919 <- moddat_ext.ey %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model() %>% run_model()
out2 <- model_results_as_table(mod.BMS.ext.ey_9919) %>% select(PI, DI)

#1999-2019, extended variables, average lagged
set.seed(3)
mod.BMS.ext.lag_9919 <- moddat_ext.lag %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model() %>% run_model()
out3 <- model_results_as_table(mod.BMS.ext.lag_9919) %>% select(PI, DI)

out <- cbind(out1, matrix(nrow = 17, ncol = 1), out2, matrix(nrow = 17, ncol = 1), out3)
print(xtable(out), include.rownames=FALSE)





