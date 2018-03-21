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
  leafletOutput("mymap", height = 800, width = 1200),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, 
                draggable = FALSE, top = 30, left = "auto",  right = 'auto', bottom = "auto", 
                width = 400, height = "auto",
                h5("Consumption Inputs", align = 'center'),
                column(12, sliderInput(inputId='empl_slide', label = '# Employees', value = 25, min=10, max = 100),
                sliderInput(inputId='sqft_slide', label = 'Square feet', value = 5000, min=250, max = 10000),
                sliderInput(inputId='hrs_slide', label = 'Weekly Operation Hours', value = 40, min=1, max = 168)),
                column(12, offset = 0.5,checkboxGroupInput(inputId='consump_check', label = 'Check All That Apply', 
                                                           choices = c('Open 24' = 'Open 24', 
                                                                       'Electric Heat' = 'Electric Heat', 
                                                                       'Electric Cool' = 'Electric Cool',  
                                                                       'Electic Manufacture' = 'Electric Manufacture'))),
                h5("Expected Payback Period", align = 'center'),
                column(12, offset = 0.5,numericInput(inputId = 'payback', label = 'Years', 
                                                     value = 5, min = 5, max = 30)),
                h5("Please Enter Your Location", align = 'center'),
                textInput(inputId = "Address", label = NULL, value = "Your Address"),
                actionButton("go", "Evaluate")
                ),
  p(),
  htmlOutput("value"),
  p(),
  plotOutput("ts")
)

server <- function(input, output) {
  employee_count <- reactive({ input$empl_slide 
  })
  office_size <- reactive({ input$sqft_slide 
  })
  hours <- reactive({ input$hrs_slide 
  })
  consumption <- reactive({ input$consump_check 
  })
  payback <- reactive({ input$payback 
  })
  
  points <- eventReactive(input$go, {
    geocode(input$Address, output='latlon', source = "dsk")
  })
  output$ts = renderPlot(width = 800, height = 400,
                         { ts.plot(forecast_data(nearest_station(stations, points())[2]), 
                                 main = 'Solar Potential Index Estimation',
                                 xlab = 'Day of a Year', 
                                 ylab = 'SPI',
                                 type = 'l',
                                 lwd = 5,
                                 col = c('dark red', 'blue', 'green'),
                                 lty = c(1:3)) 
                           legend("topright", 
                                  c("Worst", "Average", "Best"), 
                                  col = c('dark red', 'black', 'green'),
                                  lty = c(1:3)) 
  })
  output$value = renderUI({
    str1 <- paste("Climate Region: ", nearest_station(stations, points())[1])
    str2 <- "Business Attributes"
    str3 <- paste("Employee Size: ", employee_count())
    str4 <- paste("Office Size: ", office_size())
    str5 <- paste("Weekly Business Hours: ", hours())
    str6 <- paste("Comsumption: ", consumption())
    str7 <- paste("Payback Period: ", payback())
    HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = '<br/>'))
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