rm(list=ls(all=TRUE))

require(pacman)
pacman::p_load(readxl)
pacman::p_load(dplyr)
pacman::p_load(tidyverse)
pacman::p_load(countrycode)
pacman::p_load(data.table)
pacman::p_load(openxlsx)
pacman::p_load(reshape2)
pacman::p_load(dplyr)
pacman::p_load(countrycode)
pacman::p_load(data.table)

setwd("C:/Users/Viktor/Dropbox/GitHub/macroX_sovereign-yield-spreads/")

source(paste("./code/1_dependent_var.R", sep =""))
source(paste("./code/2_explanatory_vars.R", sep =""))
source(paste("./code/3_merge_model.R", sep =""))
