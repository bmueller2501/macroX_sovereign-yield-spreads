# load data for the dependent variable (spreads)

# load data
dat <- readxl::read_xls("./input/spreads.xls")
c <- colnames(dat[,2:ncol(dat)])

# get year from date
dat <- dat %>% 
  mutate(year = as.integer(substr(as.character(dat$date), 1, 4))) %>% 
  select(-date)

# adjust table to get: year, iso3, spread
dat <- gather(data = dat, key = "iso3", value = "spr", c) %>%
  mutate(iso3 = countrycode(iso3, "iso2c", "iso3c")) %>%
  as.data.table

# fix code for Ireland
dat$iso3[dat$iso3 == "IRN"] <- "IRL"

# filter out entries with missing dates
dat <- dat %>% filter(!is.na(year)); summary(dat)

# get the average spread for each country for each year
dat <- dat[, .(spr = mean(spr, na.rm = T)), by = .(year, iso3)]

dep <- data.table(dat)
rm(dat)

# filter for countries that joined the EMU in 1999 in case setting is active
if(joined_1999){
  dep <- dep %>% filter(iso3 %in% j99)
}
