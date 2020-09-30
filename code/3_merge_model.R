##-------------------------------------------------------------------------------------------------
## merge the dep and exp into model data sets and omit NAs

moddat.ey <- merge(dep_ey, exp_ey, by = c("year", "iso3")) %>% na.omit
moddat.lag <- merge(dep, exp_lag, by = c("year", "iso3")) %>% na.omit

moddat_ext.ey <- merge(dep_ey, exp_ext_ey, by = c("year", "iso3")) %>% na.omit
moddat_ext.lag <- merge(dep, exp_ext_lag, by = c("year", "iso3")) %>% na.omit

# moddat_notrel.ey <- merge(dep_ey, exp_ey_notrel, by = c("year", "iso3")) %>% na.omit


rm(dep, dep_ey, exp_ey, exp_lag, exp_ext_ey, exp_ext_lag, exp_ey_notrel)


##-------------------------------------------------------------------------------------------------
## functions for setup and run

setup_model <- function(data){
  vars <- names(data)[!names(data) %in% c("year", "iso3")]
  data %>% 
    filter(iso3 != "DEU") %>% 
    fastDummies::dummy_cols(select_columns = c("iso3")) %>%
    select(spr, vars, starts_with("iso3_")) %>% # iso3 for country dummy
    select(-iso3_AUT) # exclude one of the country dummies to avoid perfect collinearity
}

run_model <- function(data){
  bms(as.matrix(data), burn = 5000, iter = 20000, g = "BRIC", mprior = "uniform", mcmc = "rev.jump")
}


##-------------------------------------------------------------------------------------------------
## model results as table, similar to table 2 in paper

# Maltritz variables
variables.bma <- data.frame(
  No = 1:14,
  code = c("bb", "debt_ratio", "i_avg", "g", "inf", "inf_var", "gcf", "tb", "openness",
           "tot_oecd", "debt", "debt_ea", "i_us", "bbb"),
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
    "Terms of Trade",
    # Liquidity indicators
    "Total government debt",
    "Total government debt to total government debt of all EMU countries",
    # Global conditions
    "Global capital costs: US interest rate",
    "Market sentiment: BBB rated US corporate bond spread to US treasuries")
)


# extended variables
variables_ext.bma <- data.frame(
  No = 1:17,
  code = c("bb", "debt_ratio", "i_avg", "g", "inf", "inf_var", "gcf", "tb", "openness",
           "tot_oecd", "debt", "debt_ea", "i_us", "bbb", "pensions", "reer","pspp"),
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
    "Terms of Trade",
    # Liquidity indicators
    "Total government debt",
    "Total government debt to total government debt of all EMU countries",
    # Global conditions
    "Global capital costs: US interest rate",
    "Market sentiment: BBB rated US corporate bond spread to US treasuries",
    # Variable extension
    "Pension expenditure",
    "Real effective exchange rate",
    "Public Sector Purchase Programme"
    )
)


model_results_as_table <- function(model){
  cf <- as.data.frame(coef(model, exact = TRUE))
  cf$code <- row.names(cf)
  cf <- cf %>% filter(!grepl("iso3", code))
  
  if(nrow(cf) > 14) {cf <- cf %>% left_join(variables_ext.bma, by = c("code"))} else{
    cf <- cf %>% left_join(variables.bma, by = c("code"))
  }
  
  cf <- cf %>%
    mutate(
      PI = round(PIP*100, 1),
      DI = if_else(Cond.Pos.Sign == 0.5, "+/-",
                   if_else(Cond.Pos.Sign > 0.5, "+", "-"))
    ) %>% 
    arrange(No) %>% 
    select(No, Variable, PI, DI)
  
  return(cf)
}

