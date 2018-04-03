#Try New Features
library(rgdal)
library(rgeos)
library(leaflet)
library(noncensus)
library(shiny)
library(ggplot2)
library(ggmap)


ui <- #fluidPage(
  
  
  navbarPage("SolaRise Optimizer", id="nav",
             tabPanel("Map Overview",
                              
             leafletOutput("map", height = 1000),
                
                #Input Panel
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 330, height = "auto",
                              
                              h4("Location Search", align = 'center'),
                              #column(12,textInput(inputId = 'zip_search', label = NULL, value = 'Enter Zip')),
                              column(12, textInput(inputId = "Address", label = NULL, value = "Your Address"),
                              actionButton("go", "Search")),
                
                              
                              h4("Consumption Inputs", align = 'center'),
                              column(12, selectInput("biz_select", "Select Business Type:",c("Office" = "ofc",
                                                                                             "Warehouse" = "war",
                                                                                             "Restuarant" = "rest",
                                                                                             "Laboratory" = "lab")),
                                     sliderInput(inputId='empl_slide', label = '# Employees', value = 25, min=10, max = 100),
                                     #br(),
                                     sliderInput(inputId='sqft_slide', label = 'Square feet', value = 5000, min=250, max = 10000),
                                     sliderInput(inputId='hrs_slide', label = 'Weekly Operation Hours', value = 40, min=1, max = 168)),
                              
                              column(12, offset = 0.5,checkboxGroupInput(inputId='consump_check', label = 'Check All That Apply', 
                                                                         choices = c('Open 24' = 'Open 24',
                                                                                     'Electric Heat' = 'Electric Heat',
                                                                                     'Electric Cool' = 'Electric Cool',
                                                                                     'Electic Manufacture' = 'Electric Manufacture'))),
                              h4("Expected Payback Period", align = 'center'),
                              column(12, offset = 0.5,numericInput(inputId = 'payback', label = 'Years', value = 5, min = 5, max = 30))),
                
                
                #Output Panel
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = FALSE, top = "auto", left = 20, right = "auto", bottom = 60,
                              width = 330, height = "auto",
                              plotOutput("linetemp", height = 250),
                              plotOutput("bartemp", height = 250))
             ),
             
             
             tabPanel("Detailed Report",
                      h2("Personalized Reporting", align = 'center')
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
                  weight = if (input$go==0) {0.45} else {0.10}, 
                  highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>% 
      setView(if(input$go==0) {lng = -119.4179} else {points()},if(input$go==0) {lat = 36.7783} else {points()}, if(input$go==0) {zoom = 6} else {zoom=13}) %>% 
      addMarkers(if(input$go==0) {lng = 116.3636} else {points()[,1]},if(input$go==0) {lat = 39.91} else {points()[,2]})
      #addCircleMarkers(if(input$go==0) {lng = 116.3636} else {points()[,1]},if(input$go==0) {lat = 39.91} else {points()[,2]}, color = 'red',
                       #if(input$go==0) {opacity = 0} else {opacity = 1}, radius = 50)
      
  })
  
  #-----Consumption Graphs
  output$linetemp <- renderPlot({ggplot(data=df, aes(x=year, y=kw, group=estimate)) +
      geom_line(aes(color=estimate))+
      geom_point(aes(color=estimate))+ 
      theme(legend.position="bottom") +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))})
  
  output$bartemp <- renderPlot({ggplot(data=df, aes(x=year, y=savings)) +
      geom_bar(stat="identity", width=0.5, fill="steelblue")})
  
  
  }



shinyApp(ui=ui, server=server)