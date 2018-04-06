library(noncensus)
library(ggmap)

forecast.df <-data.frame(station = integer(), forecast = double())

setwd(getwd())

for (i in list.files()) 
{if (grepl('forecast',i)) {curr_csv = read.csv(i)} 
  if (grepl('forecast',i)) {forecast.df <- rbind(forecast.df,data.frame(station = substr(i,1,6), forecast = sum(curr_csv$yhat)))}}


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