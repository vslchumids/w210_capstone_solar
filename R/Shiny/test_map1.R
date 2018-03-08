library(shiny)
library(leaflet)
library(ggmap)
library(geosphere)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Load Station Data
stations <- read.csv('CA_weather_stations.csv')
stations <- stations[c("USAF", "STATION", 'latitude', 'longitude')]

# Find the nearest station
nearest_station <- function(data_station, location) {
  num_station = dim(stations)[1]
  station_list = vector(mode="list", length = num_station)
  names(station_list) <- data_station$USAF
  for (id in names(station_list)) {
    station_list[id] = distVincentySphere(location, c(data_station[data_station$USAF == id, 'longitude'], 
                                                      data_station[data_station$USAF == id, 'latitude'])) 
  }
  nearest = names(which.min(station_list)) 
  c(toString(data_station[data_station$USAF == nearest, 'STATION']), nearest)
}


ui <- fluidPage(
  textInput(inputId = "Address", label = "Enter an Address", value = "1712 briarbush ct, San Jose, CA 95131"),
  p(),
  actionButton("go", "update"),
  p(),
  leafletOutput("mymap"),
  p(),
  verbatimTextOutput("value")
)

server <- function(input, output) {
  points <- eventReactive(input$go, {
    geocode(input$Address, output='latlon', source = "dsk")
  })
  output$value = renderPrint(nearest_station(stations, points())) 
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui = ui, server = server)