
rm(list=ls(all=TRUE))

# libraries
require(pacman)
p_load(readxl)
p_load(dplyr)
p_load(tidyverse)
p_load(countrycode)
p_load(data.table)
p_load(ggplot2)
p_load(cowplot)
p_load(gridGraphics)
p_load(openxlsx)
p_load(BMS)
p_load(reshape2)
p_load(fastDummies)
p_load(xtable)


# only subset of countries that joined the EMU 1999
j99 <- c("AUT", "BEL", "DEU", "ESP", "FIN", "FRA", "ITA", "NLD", "PRT", "IRL", "LUX")

# run scripts
source(paste("./code/1_dependent_var.R", sep =""))
source(paste("./code/2_explanatory_vars.R", sep =""))
source(paste("./code/3_merge_model.R", sep =""))

# run the following scripts step-by-step
# source(paste("./code/4_robustness_checks.R", sep =""))
# source(paste("./code/5_analysis.R", sep =""))

