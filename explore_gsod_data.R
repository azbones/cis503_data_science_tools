# exploring the gsod data for Arizona

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


library(readr)
azgsod <- read_csv("~/your path here/cis503_data_science_tools/data/azgsod.zip")