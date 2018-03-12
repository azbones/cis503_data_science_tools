# example data science workflow
# print out the cheatsheet to help you- https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

# NOTE: THERE ARE INTENTIONAL OMMISSIONS IN THE CODE BELOW. YOU WILL HAVE TO FIX THE CODE TO MAKE IT WORK.

# Make sure your R environment has the required libraries. We will need "readr", "tidyverse", "maps", and "modelr".
# You can install packages using the RStudio UI or with this command: install.packages("package_name")

# Next, let's get the data

break # prevents running the whole page

library(readr) # This library lets us read CSVs simply
library(tidyverse) # This library loads a variety of tools we need

setwd("directory") # Set your home path here or use the "Files" UI in RStudio
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

# now let's arrange the output by the count in descending order from above. Can you figure out how to do this with the code below using the desc function?

arrange(desc())

# look at observations by year

azgsod %>%
  count(year) %>%
  print(n=100) 

# RESEARCH QUESTION- has the temperature changed over the duration of the dataset?

# Let's start to visualize the data by looking at temp by year. This will take some time given the dataset is large.
azgsod %>%
  ggplot(aes(x=year, y=temp)) + 
  geom_point()

# A better, faster way to plot large datasets is to use geom_boxplot().

azgsod %>%
  ggplot(aes(x=year, y=temp)) +
  geom_boxplot()

# Note that the boxplot didn't plot each year which is what I originally intended. To fix this, use as.factor() to change the year variable to a "factor" datatype.


# What kind of data type is year?


# Let's add some datetime types in.
azgsod %>%
mutate(yrmoda = ISOdatetime(.$year, .$mo, .$da, 0, 0, 0)) %>%
  ggplot(aes(yrmoda, temp)) + 
  geom_line()

# Let's focus the time period to look at the pattern
# Instead of looking at all the datapoints, let's group

azgsod %>% 
  group_by(year) %>% 
  summarise(mean_temp =mean(temp)) %>% 
  ggplot(aes(x=year, y=mean_temp)) + 
  geom_point()

# Let's look at the number of data points by station

azgsod %>%
  group_by(name) %>%
  summarise(count_temp = n()) %>%
  ggplot(aes(count_temp)) + 
  geom_histogram()

# Let's look at the top stations

azgsod %>%
  group_by(name) %>%
  summarise(count_temp = n()) %>%
  arrange(desc(count_temp))

# Now, let's focus the analysis and look at just a single station 'DAVIS-MONTHAN AFB AIRPORT'

# Add the filter parameter below for our target station

azgsod %>% 
  filter(name == ?) %>% # what does the red x to the left of this line number mean?
  count(year) %>%
  print(n=100) # why is there a red x to the left of this line number?

# What does this output tell us about the data?


# Let's add our datetime var and assign our focused data to its own dataframe

azgsod %>% 
  filter(name == ?) %>%
  mutate(yrmoda = ISOdatetime(.$year, .$mo, .$da, 0, 0, 0)) -> davis

# read the code above to describe each action we took on the data

# Now, let's plot it

davis %>%
  select(?, temp) %>%
  ggplot(aes(yrmoda, temp)) + 
  geom_point()

# How can we change the plot above to see what the pattern looks like in a given year.

# Let's look at mean monthly temp data

davis %>%
  group_by(year, mo) %>%
  summarise(mean= ?) -> davis_monthly

# plot some max temps

davis %>%
  group_by(year) %>%
  summarise(mean_max_temp = ?) %>%
  ggplot(aes(year, mean_max_temp)) + 
  geom_point() 

# Add a trendline to the plot above using stat_smooth()

# Now, let's just look at some specific months like July

davis %>%
  ? %>%
  group_by(year,mo) %>%
  summarise(mean_max_temp = max(temp)) %>%
  ggplot(aes(year, mean_max_temp)) + 
  geom_point() + 
  stat_smooth()

# Let's create a simple linear model

library(modelr) # this is a simple modeling library

davis_mod <- lm(mean ~ as.factor(mo), data= davis_monthly)

summary(davis_mod)

# plot model predictions

davis_monthly %>% 
  modelr::add_predictions(davis_mod) %>%
  ggplot(aes(year + mo/12, pred)) + 
  geom_line()

# plot residuals

davis_monthly %>% 
  modelr::add_residuals(davis_mod) %>%
  ggplot(aes(year + mo/12, resid)) + 
  geom_point()

# Now let's use an additive seasonal decomposition to look at the trend minus the seasonality

ts_davis_mo = ts(davis_monthly$mean, frequency = 12)
decompose_davis_mo = decompose(ts_davis_mo, "additive")

?ts 
?decompose # what time period is the code above using to remove seasonality?

plot(as.ts(decompose_davis_mo$trend))
plot(as.ts(decompose_davis_mo$seasonal))
plot(as.ts(decompose_davis_mo$random))
plot(decompose_davis_mo)

# As a final example, let's look at how to bring maps into our plots

library(maps) # the maps library to plot geo data

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

# What does this plot show you?
# How might you want to add to it to better understand the data?

