# robustness checks

# we have dat_rel and dat_lagged_rel

## 1. replication steps of the paper to check similarities/differences
## 2. actual robustness checks

## 1. replication steps of the paper to check similarities/differences

### 1.1 replication of BMAs in table 2

#### 1.1.1 baseline (1999-2009)
moddat <- dat_rel %>% 
  filter(year %in% c(1999:2009)) %>% 
  setup_model()

mod.BMS.baseline <- run_model(moddat)
model_results_as_table(mod.BMS.baseline)

#### 1.1.2 lagged (1999-2009)
moddat <- dat_lagged_rel %>% 
  filter(year %in% c(1999:2009)) %>% 
  setup_model()

mod.BMS.lagged <- run_model(moddat)
model_results_as_table(mod.BMS.lagged)

#### 1.1.3 pre-crisis (1999-2007)
moddat <- dat_rel %>% 
  filter(year %in% c(1999:2007)) %>% 
  setup_model()

mod.BMS.precrisis <- run_model(moddat)
model_results_as_table(mod.BMS.precrisis)

## 2. actual robustness checks

### 2.1 unmodified variables (not relative to Germany)
moddat <- dat %>% 
  filter(year %in% c(1999:2009)) %>% 
  setup_model()

mod.BMS.unmodified <- run_model(moddat)
model_results_as_table(mod.BMS.unmodified)

### 2.2 check if robust to model prior (e.g. change to "random" model prior)
moddat <- dat_rel %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model()
moddat <- as.matrix(moddat)

mod.BMS.g.random <- bms(moddat, burn = 1000, iter = 10000, g = "BRIC", mprior = "random", mcmc = "bd")
model_results_as_table(mod.BMS.g.random)