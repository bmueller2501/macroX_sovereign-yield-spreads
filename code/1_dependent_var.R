##-------------------------------------------------------------------------------------------------
## load data for the dependent variable (spreads) and create average year and end-of-year
## data set for spreads

# load data from input folder
dat <- readxl::read_xls("./input/spreads.xls")
c <- colnames(dat[,2:ncol(dat)])

# get year from date
dat <- dat %>% 
  mutate(year = as.integer(substr(as.character(dat$date), 1, 4)))

# adjust table to get: year, iso3, spread
dat <- gather(data = dat, key = "iso3", value = "spr", c) %>%
  mutate(iso3 = countrycode(iso3, "iso2c", "iso3c"))

# fix iso3 code for Ireland
dat$iso3[dat$iso3 == "IRN"] <- "IRL"

# filter out entries with missing dates
dat <- dat %>% filter(!is.na(year)); summary(dat)

# get the end-of-year value
dep_ey <- dat %>%
  group_by(iso3, year) %>% 
  filter(date == max(date)) %>% 
  select(-date) %>% 
  rename(spr_ey = "spr") %>%
  ungroup()

# get the average spread for each country for each year
dat <- data.table(dat)
dep <- dat[, .(spr = mean(spr, na.rm = T)), by = .(year, iso3)]

# filter for countries that joined the EMU in 1999
dep <- dep %>% filter(iso3 %in% j99)
dep_ey <- dep_ey %>% filter(iso3 %in% j99)

rm(dat)


##-------------------------------------------------------------------------------------------------
## plot yearly average spreads and difference between average and end-of-year values

options(scipen=100)

plotdat <- merge(dep, dep_ey) %>%
  filter(year <= 2019, iso3 != "LUX") %>% #only 1999-2019 and without LUX
  mutate(diff = spr-spr_ey)
dep_ey <- dep_ey %>% rename(spr = "spr_ey")

y <- seq(1999, 2019, by = 2)

# yearly average spreads
time.spr_avg <- ggplot(plotdat) +
  geom_line(aes(x=year, y=spr, color=iso3), size = 0.6) +
  scale_x_continuous(labels = as.character(y), breaks = y) +
  labs(x='Year', y="Basis Points",
       color = "Countries (ISO3)"); time.spr_avg
# ggsave("output/time.spr_avg.png", width=8, height=5)


# difference between average spread of the year and spread at the end of the year
time.spr_diff <- ggplot(plotdat) +
  geom_line(aes(x=year, y=diff, color=iso3), size = 0.6) +
  scale_x_continuous(labels = as.character(y), breaks = y) +
  labs(x='Year', y="Basis Points",
       color = "Countries (ISO3)")
# ("output/time.spr_diff.png", width=8, height=5)

plot_grid(time.spr_avg, time.spr_diff, align='h',labels='AUTO')
ggsave("output/time.spr.png", width=15, height=5)


rm(plotdat, time.spr_avg, time.spr_diff)

