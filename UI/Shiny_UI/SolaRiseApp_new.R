#Try New Features
library(rgdal)
library(rgeos)
library(leaflet)
library(noncensus)
library(shiny)
library(ggplot2)
library(ggmap)
library(geosphere)
library(data.table)
library(xts)
library(shinythemes)

# ----------------------------
# Loading and selecting Data
# ----------------------------

# Load Station Data
stations <- read.csv('selected_stations.csv')
stations <- stations[c("USAF", "STATION", 'latitude', 'longitude', 'CZ')]

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
  c(toString(data_station[data_station$USAF == nearest, 'CZ']), nearest)
}

# Collect forecast data
# Calculate monthly avg
monthly_avg <- function(station_id) {
  # Read table
  station_data = paste(station_id, "_forecasttable.csv", sep = '')
  df = read.csv(station_data) 
  # Forecast period and getting data of this period
  forecast_period = dim(df)[1] - 365
  forecast_df = df[-(1:forecast_period), c('ds', 'yhat_lower', 'yhat', 'yhat_upper')]
  forecast_df$ds = as.Date(forecast_df$ds)
  forecast_xts = as.xts(data.table(forecast_df))
  monthly <- apply.monthly(forecast_xts, colMeans)
  monthly
}

# Calculate weekly avg
weekly_avg <- function(station_id) {
  # Read table
  station_data = paste(station_id, "_forecasttable.csv", sep = '')
  df = read.csv(station_data) 
  
  # Forecast period and getting data of this period
  forecast_period = dim(df)[1] - 365
  forecast_df = df[-(1:forecast_period), c('ds', 'yhat_lower', 'yhat', 'yhat_upper')]
  forecast_df$ds = as.Date(forecast_df$ds)
  forecast_xts = as.xts(data.table(forecast_df))
  weekly <- apply.weekly(forecast_xts, colMeans)
  weekly
}


#------------
# UI Section
#------------

ui <- fluidPage("",
  #theme = 'agency.css',
  theme = shinytheme("cerulean"),
  titlePanel(title = div("SolaRise Business Solar Estimator", 
             img(src='power_solar.png', height = 50, width = 75))),  
  navbarPage(title = '', 
             id="nav",
             #theme = 'agency.css',
             tabPanel("About Us",
                      fluidRow(
                        column(4, img(src='solar_pic1.jpg', height = 200, width = 300, 
                                      top = 20, bottom = 20, align = "right")), 
                        column(8, h3("Who are we?", align = 'left'),
                         p("The cost of solar installation and adoption declined significant in the past few years.
                            Does solar adoption the right decision for your business? Solarise has the answer.
                            SolaRise is an solar evaluator for small to midsize businesses based on data science and 
                            machine learning algorithm. Our intellegent engine gives custom and intuitive guidence 
                            to business owners on solar adoption."))),
                      fluidRow(column(12, div(style = "height:100px;"))),
                      fluidRow(
                        column(4, img(src='business1.jpg', height = 200, width = 300, 
                                      top = 20, bottom = 20, align = "right")), 
                        column(8, h3("Why Solarise?", align = 'left'),
                               p("Unlike existing solar energy evaluators which only provide estimation for residentual users
                                  based on the location and roof top size. SolaRise is designed for small to medium size business.
                                  It not only uses location, weather data, and roof size for solar potential estimation and projection
                                  but also considers business types, operation hours, energy usage pattern, and  business growth. 
                                  The tool is designed with a typical decision making process in mind and intuitive to business onwers"))),
                      fluidRow(column(12, div(style = "height:100px;"))),
                      fluidRow(
                        column(4, img(src='howto.jpg', height=200, width = 300,  
                                      top = 20, bottom = 20, align = "right")),
                        column(8, h3("How to use?", align = 'left'),
                               br("Go to", strong('Tell us Your Business'), "fill in the input form about your business, press report."),
                               br("Go to", strong('Detailed Report => Solar Energy Project'), "for solar potential index annual projection"),
                               br("Go to", strong('Detailed Report => Business Report'), "for detailed business related ")
                               )),
                      fluidRow(column(12, div(style = "height:100px;")))
                      ),
             tabPanel("Tell us Your Business", 
                
                      #Input Panel
                      column(4, 
                        wellPanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, left = "auto", right = "auto", bottom = "auto",
                                      width = "auto", height = "auto",
                                      
                                  h4("Enter Your Location", align = 'center'),
                                  textInput(inputId = "Address", label = NULL, width = '100%',
                                                value = "e.g. 301 Old San Francisco Rd, Sunnyvale, CA 94086"),
                                  
                                  h4("Consumption Inputs", align = 'center'),
                                  selectInput("biz_select", "Select Business Type:", 
                                                                     c("Office" = "ofc", 
                                                                       "Warehouse" = "war", 
                                                                       "Restuarant" = "rest",
                                                                       "Laboratory" = "lab")),
                                 sliderInput(inputId='empl_slide', label = '# Employees', value = 25, min=10, max = 100),
                                 sliderInput(inputId='sqft_slide', label = 'Square feet', value = 5000, min=250, max = 10000),
                                 sliderInput(inputId='hrs_slide', label = 'Weekly Operation Hours', value = 40, min=1, max = 168),
                                  
                                 checkboxGroupInput(inputId='consump_check', label = 'Check All That Apply', 
                                                                             choices = c('Open 24' = 'Open 24',
                                                                                         'Electric Heat' = 'Electric Heat',
                                                                                         'Electric Cool' = 'Electric Cool',
                                                                                         'Electic Manufacture' = 'Electric Manufacture')),
                                  h4("Expected Payback Period", align = 'center'),
                                  numericInput(inputId = 'payback', label = 'Years', value = 5, min = 5, max = 30),
                                  actionButton("go", "Go SolarRise"))),
                      column(8, leafletOutput("map", height = 850))
                      ),
             navbarMenu("Detailed Report",
               tabPanel("Solar Energy Projection",
                        h2("Solar Energy Annual Trend", align = 'center'),
                        fluidRow(
                          column(12, 
                                 plotOutput("SPI_monthly", height = 400))),  
                        fluidRow(column(12, div(style = "height:100px;"))),
                        fluidRow(
                          column(12, 
                                 plotOutput("SPI_weekly", height = 400))
                          )
                        ),
      
               tabPanel("Business Report",
                        h2("Personalized Reporting", align = 'center'),
                        fluidRow(
                          column(12, htmlOutput("BusinessAttri"))
                          ),
                        fluidRow(column(12, div(style = "height:100px;"))),
                        fluidRow(
                          column(12, img(src='SolarROI.jpg', height = 400))
                          )
                        )
               ),
             tabPanel("Our Team") 
  )
)

server <- function(input,output, session){
  
  # downloaddir<-getwd()
  # #setwd('C:/Users/eyang/Desktop/Shiny_PC')
  # zip_codes = read.csv('ca_zips.csv') 
  # dat<-readOGR(downloaddir, "cb_2016_us_zcta510_500k") 
  # 
  # subdat<-dat[dat$GEOID10 %in% zip_codes$GEOID10,]
  # 
  # # ----- Transform to EPSG 4326 - WGS84 (required)
  # #subdat<-spTransform(subdat, CRS("+init=epsg:4326"))
  # proj4string(subdat) <- CRS("+init=epsg:4326")
  # 
  # # ----- save the data slot
  # subdat_data<-subdat@data[,c("GEOID10", "ALAND10")]
  # 
  # # ----- simplification yields a SpatialPolygons class
  # subdat<-gSimplify(subdat,tol=0.01, topologyPreserve=TRUE)
  # 
  # # ----- to write to geojson we need a SpatialPolygonsDataFrame
  # subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
  # 
  # #-----merge in density data
  # subdat <- merge(x = subdat, y = zip_codes, by = "GEOID10", all.x = TRUE)
  subdat <- readRDS("subdat.rds")
  
  labels <- sprintf(
    "<strong>%s",
    subdat$GEOID10
  ) %>% lapply(htmltools::HTML)
  
  
  #------Setup Density Buckets
  bins <- c(0.001, .01, .02, .05, .1, .2, .5, 1, Inf)
  pal <- colorBin("Blues", domain = subdat$Density, bins = bins)
  
  #----Setup Temp Datasets
  df <- data.frame(estimate=rep(c("consumption", "generation"), each=5),
                   year=rep(c(2018,2019,2020,2021,2022),2),
                   kw=c(30,30,35,35,40,30,30,40,30,35),
                   savings=c(2,3,5,-2,5,0,0,0,0,0))

  #-----Map Search
  points <- eventReactive(input$go, {geocode(input$Address, output='latlon', source = "dsk")})
  
  #-------Map Visualization
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data= if (input$go==0) {subdat} else {subdat[subdat$GEOID10 == as.numeric(as.character(revgeocode(c(points()$lon,points()$lat), output = 'more')$postal_code)),]}, 
                  fillColor =if (input$go==0) {~pal(subdat$Density)} else {'blue'},
                  fillOpacity =if (input$go==0) {0.45} else {0.00009},
                  weight = .5, 
                  highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>% 
#<<<<<<< HEAD
      setView(if(input$go==0) {lng = -119.4179} else {points()},if(input$go==0) {lat = 36.7783} else {points()}, if(input$go==0) {zoom = 6} else {zoom=14}) %>% 
#=======
      setView(if(input$go==0) {lng = -119.4179} else {points()},if(input$go==0) {lat = 36.7783} else {points()}, if(input$go==0) {zoom = 6} else {zoom=13}) %>% 
#>>>>>>> 9f665df4fb6f42934239666abfc3992c9f2afbff
      addMarkers(if(input$go==0) {lng = 116.3636} else {points()[,1]},if(input$go==0) {lat = 39.91} else {points()[,2]}) %>% 
      #addCircleMarkers(if(input$go==0) {lng = 116.3636} else {points()[,1]},if(input$go==0) {lat = 39.91} else {points()[,2]}, color = 'red',
                       #if(input$go==0) {opacity = 0} else {opacity = 1}, radius = 50)
      leaflet::addLegend(pal=pal,value = subdat$Density, opacity = 0.7, title = NULL, position = "bottomleft")
      
  })
  
  #-----Consumption Graphs
  
  output$SPI_monthly = renderPlot({
    #par(bg = '#2FA4E7')
    barplot(monthly_avg(nearest_station(stations, points())[2]), 
            names.arg = c(months(index(monthly_avg(nearest_station(stations, points())[2])))), 
            main = 'Monthly Average SPI Est',
            ylab = 'Monthly Avg SPI',
            xlab = 'Month', 
            cex.main = 0.8,
            cex.lab = 0.8,
            cex.axis = 0.8,
            ylim = c(0.0, 1.05),
            beside = TRUE,
            col = c('firebrick', 'darkorange', 'darkolivegreen2'))
    legend("topright", c("Worst", "Average", "Best"), fill = c('darkolivegreen2', 'darkorange', 'firebrick'))
  })
  output$SPI_weekly = renderPlot({
    #par(bg = '#2FA4E7')
    barplot(weekly_avg(nearest_station(stations, points())[2]), 
            names.arg = c(week(index(weekly_avg(nearest_station(stations, points())[2])))), 
            main = 'Weekly Average SPI Est',
            ylab = 'Weekly Avg SPI',
            xlab = 'Week of the Year', 
            cex.main = 0.8,
            cex.lab = 0.8,
            cex.axis = 0.8,
            ylim = c(0.0, 1.05),
            beside = TRUE,
            col = c('firebrick', 'darkorange', 'darkolivegreen2'))
    legend("topright", c("Worst", "Average", "Best"), fill = c('darkolivegreen2', 'firebrick', 'darkorange'))
  })
  
  output$linetemp <- renderPlot({ggplot(data=df, aes(x=year, y=kw, group=estimate)) +
      geom_line(aes(color=estimate))+
      geom_point(aes(color=estimate))+ 
      theme(legend.position="bottom") +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))})
  
  output$bartemp <- renderPlot({ggplot(data=df, aes(x=year, y=savings)) +
      geom_bar(stat="identity", width=0.5, fill="steelblue")})
  
#------Business Attribute
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
  
  output$BusinessAttri = renderUI({
    str1 <- paste("Climate Zone: ", nearest_station(stations, points())[1])
    str2 <- "Business Attributes"
    str3 <- paste("Employee Size: ", employee_count())
    str4 <- paste("Office Size: ", office_size())
    str5 <- paste("Weekly Business Hours: ", hours())
    str6 <- paste("Comsumption: ", consumption())
    str7 <- paste("Payback Period: ", payback())
    HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = '<br/>'))
  })

  }


shinyApp(ui=ui, server=server)