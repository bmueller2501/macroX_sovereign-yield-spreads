# This file loads all required libraries and run all scripts

rm(list=ls(all=TRUE))

require(pacman)
pacman::p_load(readxl)
pacman::p_load(dplyr)
pacman::p_load(tidyverse)
pacman::p_load(countrycode)
pacman::p_load(data.table)
pacman::p_load(openxlsx)
pacman::p_load(reshape2)
pacman::p_load(countrycode)
pacman::p_load(BMS)
pacman::p_load(fastDummies)

# set settings for this run
joined_1999 <- TRUE # defines whether to only consider the subset of countries that joined the EMU 1999
j99 <- c("AUT", "BEL", "DEU", "ESP", "FIN", "FRA", "ITA", "NLD", "PRT", "IRL", "LUX")

# setwd("C:/Users/Viktor/Dropbox/GitHub/macroX_sovereign-yield-spreads/")
# setwd("C:/Users/bmuel/Desktop/GitHub/macroX_sovereign-yield-spreads")

source(paste("./code/1_dependent_var.R", sep =""))
source(paste("./code/2_explanatory_vars.R", sep =""))
source(paste("./code/3_merge_model.R", sep =""))
source(paste("./code/4_robustness_checks.R", sep =""))
