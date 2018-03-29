library(rgdal)
library(rgeos)
library(leaflet)
library(noncensus)
library(shiny)
library(ggplot2)
library(ggmap)


data("zip_codes")
zip_codes[zip_codes$state == 'CA',]


downloaddir<-getwd()
#setwd('C:/Users/eyang/Desktop/Shiny_PC')
dat<-readOGR(downloaddir, "cb_2016_us_zcta510_500k")

#Remove this when we can just use non_census
zip_codes = read.csv('ca_zips.csv')

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