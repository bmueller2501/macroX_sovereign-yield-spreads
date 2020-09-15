
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

#####################
# var corrections
#####################

# calculate trade balance relative to GDP
full_data$tb <- full_data$tb/full_data$gdp

# calculate total outstanding debt relative to EMU total outstanding debt
# @ Benjamin: vielleicht kannst du einen dummy machen wo wir in 0_source festlegen,
# ob wir das 11 länder oder das 19 länder setup wollen weil je nach dem sum(debt)
# anders ausfällt. wäre sinnbefreit die schulden eines landes in relation zu einem aggregat
# zu setzen das wir gar nicht betrachten. also entweder debt/sum(debt der 11 oder 19 länder)
full_data <- data.table(full_data)
full_data[, debt_ea := debt/sum(debt), by = year]


# PSPP
# comment line 1 and line 2 out if you want it in absolute terms
# line 1 only if you want it relative to GDP
# line 2 only if you want it as dummy variable
full_data$pspp <- (full_data$pspp/1000)/full_data$gdp #(1)
#full_data$pspp <- ifelse(full_data$year >= 2015,1,0) #(2)

exp <- data.table(full_data)
vars <- names(exp)[!names(exp) %in% c("year", "iso3")]

# rm(i0, full_data, full_data_list)
