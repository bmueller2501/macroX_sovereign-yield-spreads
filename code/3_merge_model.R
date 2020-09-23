
##-------------------------------------------------------------------------------------------------
## merge the dep and exp into model data sets and omit NAs

dat_base <- merge(dep, exp, by = c("year", "iso3")) %>% na.omit
dat_ey <- merge(dep_ey, exp_ey, by = c("year", "iso3")) %>% na.omit
dat_lag <- merge(dep, exp_lag, by = c("year", "iso3")) %>% na.omit

dat_ey_notrel <- merge(dep_ey, exp_ey_notrel, by = c("year", "iso3")) %>% na.omit

#we have 13 NAs in the spread varaible from LUX in the early years


##-------------------------------------------------------------------------------------------------
## functions for setup and run

setup_model <- function(data){
  data %>% 
    filter(iso3 != "DEU") %>% 
    fastDummies::dummy_cols(select_columns = c("iso3")) %>%
    select(spr, vars, starts_with("iso3_")) %>% # iso3 for country dummy
    select(-iso3_AUT) # exclude one of the country dummies to avoid perfect collinearity
}

run_model <- function(data){
  bms(as.matrix(data), burn = 5000, iter = 30000, g = "BRIC", mprior = "uniform", mcmc = "rev.jump")
}


##-------------------------------------------------------------------------------------------------
## model results as table, same as table 2 in paper

variables.bma <- data.frame(
  No = 1:14,
  code = c("bb", "debt_ratio", "i_avg", "g", "inf", "inf_var", "gcf", "tb", "openness",
           "tot_g", "debt", "debt_ea", "i_us", "bbb"),
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



