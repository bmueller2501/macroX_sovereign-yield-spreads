rm(list=ls(all=TRUE))

require(pacman)
pacman::p_load(openxlsx)
pacman::p_load(reshape2)
pacman::p_load(dplyr)
pacman::p_load(countrycode)

setwd("C:/Users/Viktor/Dropbox/GitHub/macroX_sovereign-yield-spreads")
var_names <- getSheetNames("./input/full_data.xlsx")
N <- length(var_names) 	#Number of sheets in the *.xlsx-file

full_data_list <-list()

for (i0 in 1:N){
  data <- openxlsx::read.xlsx("./input/full_data.xlsx", 
                                       sheet = i0, colNames = TRUE,
                                       na.strings = "NA")
  
  full_data_list[[i0]] <- melt(data, id = "year", variable = "iso3")
  full_data_list[[i0]]$year <- as.integer(full_data_list[[i0]]$year)
  full_data_list[[i0]]$iso3 <- as.character(full_data_list[[i0]]$iso3)
  full_data_list[[i0]][[paste0(var_names[i0])]] <- as.numeric(full_data_list[[i0]]$value)
  full_data_list[[i0]] <- full_data_list[[i0]] %>% select(-value)
}

full_data <- Reduce(merge, full_data_list)
full_data$iso3 <- countrycode(full_data$iso3, "country.name", "iso3c")