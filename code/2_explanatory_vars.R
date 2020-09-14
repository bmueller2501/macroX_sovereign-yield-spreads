
vars <- getSheetNames("./input/full_data.xlsx")

full_data_list <- list()
for (i0 in 1:length(vars)){
  data <- openxlsx::read.xlsx("./input/full_data.xlsx", 
                                       sheet = i0, colNames = TRUE,
                                       na.strings = "NA")
  
  full_data_list[[i0]] <- melt(data, id = "year", variable = "iso3")
  full_data_list[[i0]]$year <- as.integer(full_data_list[[i0]]$year)
  full_data_list[[i0]]$iso3 <- as.character(full_data_list[[i0]]$iso3)
  full_data_list[[i0]][[paste0(vars[i0])]] <- as.numeric(full_data_list[[i0]]$value)
  full_data_list[[i0]] <- full_data_list[[i0]] %>% select(-value)
}
rm(data)

full_data <- Reduce(merge, full_data_list)
full_data$iso3 <- countrycode(full_data$iso3, "country.name", "iso3c")

#var correction
full_data$tb <- full_data$tb/full_data$gdp

full_data <- data.table(full_data)
full_data[, debt_ea := debt/sum(debt), by = year]


exp <- data.table(full_data)
vars <- names(exp)[!names(exp) %in% c("year", "iso3")]

rm(i0, full_data, full_data_list)
