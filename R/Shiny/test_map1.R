library(shiny)
library(leaflet)
library(ggmap)
library(geosphere)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Load Station Data
stations <- read.csv('selected_stations.csv')
stations <- stations[c("USAF", "STATION", 'latitude', 'longitude', 'Region')]

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
  c(toString(data_station[data_station$USAF == nearest, 'Region']), nearest)
}

# Collect forecast data
forecast_data <- function(station_id) {
  # Read table
  station_data = paste(station_id, "_forecasttable.csv", sep = '')
  df = read.csv(station_data) 
  
  # Forecast period and getting data of this period
  forecast_period = dim(df)[1] - 365
  forecast_df = df[-(1:forecast_period), c('yhat_lower', 'yhat', 'yhat_upper')]
  forecast_df 
}


ui <- fluidPage(
  textInput(inputId = "Address", label = "Enter an Address", value = "University of California Berkeley, Berkeley, CA"),
  p(),
  actionButton("go", "Evaluate"),
  p(),
  leafletOutput("mymap"),
  p(),
  verbatimTextOutput("value"),
  p(),
  plotOutput("ts")
)

server <- function(input, output) {
  points <- eventReactive(input$go, {
    geocode(input$Address, output='latlon', source = "dsk")
  })
  output$ts = renderPlot({
    ts.plot(forecast_data(nearest_station(stations, points())[2]), 
                                 main = 'Annual Solar Iradiance Estimation',
                                 xlab = 'Day of a Year', 
                                 ylab = 'GHI',
                                 type = 'l',
                                 lwd = 5,
                                 col = 'dark red'
                                  ) 
  })
  output$value = renderText({
    nearest_station(stations, points())[1] 
  })
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui = ui, server = server)