library(rgdal)
library(rgeos)
library(leaflet)
library(noncensus)
library(shiny)
library(ggplot2)
library(ggmap)
library(plyr)

setwd(getwd())

forecast.df <-data.frame(station = integer(), Density = double())
data("zip_codes")
ca.zips <- zip_codes[zip_codes$state == 'CA',]
stations <- read.csv('selected_stations.csv')

#Buildup Station Forecast Summry
for (i in list.files()) 
{if (grepl('forecast',i)) {curr_csv = read.csv(i)} 
  if (grepl('forecast',i)) {forecast.df <- rbind(forecast.df,data.frame(station = substr(i,1,6), Density = sum(curr_csv$yhat)))}}

#Function to Determine Nearest Station
nearest_station <- function(data_station, location) {
  num_station = dim(stations)[1]
  station_list = vector(mode="list", length = num_station)
  names(station_list) <- data_station$USAF
  for (id in names(station_list)) {
    station_list[id] = distVincentySphere(location, c(data_station[data_station$USAF == id, 'longitude'], 
                                                      data_station[data_station$USAF == id, 'latitude'])) 
  }
  nearest = names(which.min(station_list)) 
  c(toString(data_station[data_station$USAF == nearest, 'CZ']), nearest)
}

#Find Nearest Station to Zip
nearest.station <-data.frame(station = integer())
for (row in 1:nrow(ca.zips)) {
  nearest.station <- rbind(nearest.station,data.frame(station = nearest_station(stations,c(ca.zips[row, "longitude"], ca.zips[row, "latitude"]))[2]))}

nearest.station <- merge(nearest.station,forecast.df, by="station")
zip_codes <- cbind(ca.zips$zip,ca.zips$city, nearest.station)
zip_codes <-rename(zip_codes, c("ca.zips$zip"="GEOID10", "ca.zips$city"="City"))


#Buildup Polygons
downloaddir<-getwd()
dat<-readOGR(downloaddir, "cb_2016_us_zcta510_500k")
subdat<-dat[dat$GEOID10 %in% zip_codes$GEOID10,]

# ----- Transform to EPSG 4326 - WGS84 (required)
#subdat<-spTransform(subdat, CRS("+init=epsg:4326"))
proj4string(subdat) <- CRS("+init=epsg:4326")

# ----- save the data slot
subdat_data<-subdat@data[,c("GEOID10", "ALAND10")]

# ----- simplification yields a SpatialPolygons class
subdat<-gSimplify(subdat,tol=0.01, topologyPreserve=TRUE)

# ----- to write to geojson we need a SpatialPolygonsDataFrame
subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)

#-----merge in density data
subdat <- merge(x = subdat, y = zip_codes, by = "GEOID10", all.x = TRUE)

saveRDS(subdat, "subdat.rds")

head(subdat)
