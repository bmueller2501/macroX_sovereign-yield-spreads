# load data on explanatory variables

vars <- getSheetNames("./input/full_data.xlsx")

full_data_list <- list()
for (i0 in 1:length(vars)){
  data <- openxlsx::read.xlsx("./input/full_data.xlsx", 
                                       sheet = i0, colNames = TRUE,
                                       na.strings = "NA")
  
  # store current sheet into full_data_list
  full_data_list[[i0]] <- melt(data, id = "year", variable = "iso3")
  # adjust file types
  full_data_list[[i0]]$year <- as.integer(full_data_list[[i0]]$year)
  full_data_list[[i0]]$iso3 <- as.character(full_data_list[[i0]]$iso3)
  full_data_list[[i0]][[paste0(vars[i0])]] <- as.numeric(full_data_list[[i0]]$value)
  full_data_list[[i0]] <- full_data_list[[i0]] %>% select(-value)
}
rm(data, i0)

full_data <- Reduce(merge, full_data_list); rm(full_data_list)
full_data$iso3 <- countrycode(full_data$iso3, "country.name", "iso3c")

# filter for countries that joined the EMU in 1999 in case setting is active
if(joined_1999){
  full_data <- full_data %>% filter(iso3 %in% j99)
}

#####################
# var corrections
#####################

# calculate trade balance relative to GDP
full_data$tb <- full_data$tb/full_data$gdp

# calculate total outstanding debt relative to EMU total outstanding debt
full_data <- data.table(full_data)
full_data[, debt_ea := debt/sum(debt), by = year]



# PSPP
# comment out line 1 and line 2 if you want it in absolute terms
# line 1 only if you want it relative to GDP
# line 2 only if you want it as dummy variable
full_data$pspp <- (full_data$pspp/1000)/full_data$gdp #(1) relative to GDP
# full_data$pspp <- ifelse(full_data$year >= 2015,1,0) #(2) dummy

exp <- data.table(full_data); rm(full_data)

exp_lagged <- exp
exp_lagged$year <- exp$year + 1
exp_lagged <- exp_lagged[exp_lagged$year < max(exp_lagged$year)]

vars <- names(exp)[!names(exp) %in% c("year", "iso3")]
