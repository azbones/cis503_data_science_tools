# example data science workflow

# GETTING THE DATA

library(readr)
library(tidyverse)

setwd("~/Documents/My_Documents/ASU/cis 503/cis503_data_science_tools/")
azgsod <- read_csv("./data/azgsod.zip")
View(azgsod)

# This is the query to pull back the data from Google's BigQuery (note it is using standard SQL dialect)
#
# SELECT b.name, b.country, b.state, b.call, b.lat, b.lon, b.elev, b.begin, b.end, a.wban, 
#        a.stn, a.year, a.mo, a.da, a.temp, a.count_temp, a.dewp, a.count_dewp, a.stp, a.count_stp, 
#        a.visib, a.count_visib, a.wdsp, a.count_wdsp, a.gust, a.max, a.flag_max, a.min, a.flag_min, 
#        a.prcp, a.flag_prcp, a.sndp, a.fog, a.rain_drizzle, a.snow_ice_pellets, a.hail, a.thunder, 
#        a.tornado_funnel_cloud
# FROM `bigquery-public-data.noaa_gsod.gsod*` a
# JOIN `bigquery-public-data.noaa_gsod.stations` b ON a.stn=b.usaf AND a.wban=b.wban
# WHERE state LIKE "AZ"


# CHECK THE INTEGRITY OF THE DATA
# metadata and schema for data- https://www7.ncdc.noaa.gov/CDO/GSOD_DESC.txt
# metadata and scheme for stations- ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt 

# did the import work correctly?
lapply(azgsod, typeof)
?lapply

# does the data seem correct?
lapply(azgsod, range)

# CLEANING THE DATA
# remove old school convention of setting missing data to nines
# azgsod$dewp[azgsod$dewp == 9999.9] <- NA # boolean mask

azgsod <- read_csv("./data/azgsod.zip", na = c('9999.9','999.9'))


# EXPLORE THE DATA 

# let's count the number of observations by station

azgsod %>%
  count(name) 

# add print to add more lines to output

# now let's arrange the output by the count in descending order using 
arrange(desc())

# look at observations by year

azgsod %>%
  count(year) %>%
  print(n=100) 

# RESEARCH QUESTION- has the temperature changed over the duration of the dataset?

# Let's start to visualize the data by looking at temp by year
azgsod %>%
    ggplot(aes(year, temp)) + geom_point()

# What kind of data type is year?


# Let's add some datetime types in.
azgsod %>%
mutate(yrmoda = ISOdatetime(.$year, .$mo, .$da, 0, 0, 0)) %>%
  ggplot(aes(yrmoda, temp)) + geom_line()

# Let's focus the time period to look at the pattern


# Instead of looking at all the datapoints, let's group

azgsod %>% 
  group_by(year) %>% 
  summarise(mean_temp =mean(temp)) %>% 
  ggplot(aes(x=year, y=mean_temp)) + geom_point()

# Let's look at the number of data points by station

azgsod %>%
  group_by(name) %>%
  summarise(count_temp = n()) %>%
  ggplot(aes(count_temp)) + geom_histogram()
 
# focus


azgsod %>% 
  filter(name == 'DAVIS-MONTHAN AFB AIRPORT') %>%
  count(year) %>%
  print(n=100)

azgsod %>% 
  filter(name == 'DAVIS-MONTHAN AFB AIRPORT') -> davis

davis %>%
  select(year, mo, da, temp) %>%
  mutate(yrmoda = ISOdatetime(.$year, .$mo, .$da, 0, 0, 0)) %>%
  ggplot(aes(yrmoda, temp)) + geom_point()

davis %>%
  # filter(year == 1980) %>%  # TODO: take out in order to add back in on the fly
  select(year, mo, da, temp) %>%
  mutate(yrmoda = ISOdatetime(.$year, .$mo, .$da, 0, 0, 0)) %>%
  ggplot(aes(yrmoda, temp)) + geom_line()



davis %>%
  group_by(year, mo) %>%
  summarise(mean= mean(temp)) -> davis_monthly

# plot some mean temps

davis %>%
  group_by(year) %>%
  summarise(mean_mo_temp = max(temp)) %>%
  ggplot(aes(year, mean_mo_temp)) + geom_point() +stat_smooth()

davis %>%
  filter(mo == 7) %>%
  group_by(year,mo) %>%
  summarise(mean_mo_temp = max(temp)) %>%
  mutate(yrmoda = ISOdatetime(.$year, .$mo, .$da, 0, 0, 0)) %>%
  ggplot(aes(year, mean_mo_temp)) + geom_point() +stat_smooth()

# create a simple linear model

library(modelr)

davis_mod <- lm(mean ~ factor(mo), data= davis_monthly)

summary(davis_mod)

# plot model predictions




davis_monthly %>% 
  add_predictions(davis_mod) %>%
  ggplot(aes(year + mo/12, pred)) + geom_line()

# plot residuals

davis_monthly %>% 
  add_residuals(davis_mod) %>%
  ggplot(aes(year + mo/12, resid)) + geom_point()




davis$year + davis$mo


library(maps)

# fun with mapping

# create a base plot to draw on
p <- ggplot() +
  coord_fixed() +
  xlab("") +
  ylab("")

# get the state data to draw state map
us_map <- map_data("state")
az_map <- subset(us_map, us_map$region=="arizona")

# create the base az map 
base_az_map <- p + geom_polygon(data=az_map, aes(x=long, y=lat, group=group), 
                                     colour="light blue", fill="light blue")

# filter for just the unique stations lat and longs
azgsod %>%
  select(name, lat, lon) %>%
  unique() -> az_station_locs

# add the station locations to the base plot and base Arizona map
base_az_map +
  geom_point(data=az_station_locs, aes(x=lon, y=lat), color="dark blue", size=3, alpha=.2)





azgsod %>% 
  group_by(mo) %>% 
  summarise(count=n(), mean_temp =mean(temp))


azgsod %>% group_by(year) %>% summarise(count=n(), mean_temp =mean(temp)) %>% ggplot(aes(x=year, y=mean_temp)) +geom_line()
