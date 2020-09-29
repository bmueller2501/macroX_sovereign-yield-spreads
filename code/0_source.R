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
pacman::p_load(xtable)

# set settings for this run
j99 <- TRUE # defines whether to only consider the subset of countries that joined the EMU 1999
cj99 <- c("AUT", "BEL", "DEU", "ESP", "FIN", "FRA", "ITA", "NLD", "PRT", "IRL", "LUX")

# setwd("C:/Users/Viktor/Dropbox/GitHub/macroX_sovereign-yield-spreads/")
# setwd("C:/Users/bmuel/Desktop/GitHub/macroX_sovereign-yield-spreads")

source(paste("./code/1_dependent_var.R", sep =""))
source(paste("./code/2_explanatory_vars.R", sep =""))
source(paste("./code/3_merge_model.R", sep =""))

# run the following scripts step-by-step
# source(paste("./code/4_robustness_checks.R", sep =""))
# source(paste("./code/5_analysis.R", sep =""))
# source(paste("./code/6_ssvs.R", sep =""))
