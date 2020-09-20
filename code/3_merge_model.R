# merge the data into one data.table
dat <- merge(dep_last, exp, by = c("year", "iso3")); summary(dat)
dat_lagged <- merge(dep_last, exp_lagged, by = c("year", "iso3")); summary(dat_lagged)

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

#############################
# FUNCTIONS -----------------

setup_model <- function(data){
  return(data %>% 
           filter(iso3 != "DEU") %>% 
           fastDummies::dummy_cols(select_columns = c("iso3")) %>%
           select(spr, vars, starts_with("iso3_")) %>% # iso3 for country dummy
           select(-iso3_AUT) %>% # exclude one of the country dummies to avoid perfect collinearity
           select(-c(pspp,
                     reer,
                     gdp,
                     tot_oecd,
                     tot_imf,
                     emu)))
}
run_model <- function(data){
  moddat <- as.matrix(data)
  
  return(bms(moddat, burn = 5000, iter = 30000, g = "BRIC", mprior = "uniform", mcmc = "rev.jump"))
}
variables.bma <- data.frame(
  No = 1:14,
  code = c("bb", "debt_ratio", "i_avg", "g", "inf", "inf_var", "gcf", "tb", "openness",
           "tot_g", "debt", "debt_ea", "i_us", "baa"),
  Variable = c(
    # Country specific yield spread drivers
    "Budget balance to GDP",
    "Total government debt to GDP",
    "Average interest rate",
    "GDP growth",
    "Inflation",
    "Inflation variation",
    "Capital formation",
    "Trade balance",
    "Openness",
    "Terms of Trade growth",
    # Liquidity indicators
    "Total government debt",
    "Total government debt to total government debt of all EMU countries",
    # Global conditions
    "Global capital costs: US interest rate",
    "Market sentiment: BBB rated US corporate bond spread to US treasuries")
)
model_results_as_table <- function(model){
  cf <- as.data.frame(coef(model, exact = TRUE))
  cf$code <- row.names(cf)
  
  cf <- cf %>% 
    filter(!grepl("iso3", code)) %>% 
    # join variables.bma to get naming and ordering right
    left_join(variables.bma, by = c("code")) %>% 
    # prepare for output
    mutate(
      PI = round(PIP*100, 1),
      DI = if_else(Cond.Pos.Sign == 0.5, "+/-",
                   if_else(Cond.Pos.Sign > 0.5, "+", "-"))
    ) %>% 
    arrange(No) %>% 
    select(No, Variable, PI, DI)
  
  return(cf)
}

moddat <- dat_rel %>% 
  filter(year %in% c(1999:2019)) %>% 
  setup_model()

mod.BMS <- run_model(moddat)

model_results_as_table(mod.BMS)

# coef(mod.BMS, exact = TRUE) # exact=TRUE: This means that the marginal likelihoods are used for BMA
