#Try New Features
library(rgdal)
library(rgeos)
library(leaflet)
library(noncensus)
library(shiny)


# ui <- fluidPage(mainPanel(leafletOutput("map", height = 800)), 
#                 hr(),
#                 
#                 column(3, wellPanel(sliderInput(inputId='empl_slide', label = '# Employees', 
#                             value = 25, min=10, max = 100), br(),
#                 sliderInput(inputId='sqft_slide', label = 'Squarefeet', 
#                             value = 1000, min=250, max = 10000))),
#                 
#                 column(2, offset = 1, sliderInput(inputId='empl', label = '# Employees', 
#                                       value = 25, min=10, max = 100), br(),
#                        sliderInput(inputId='sqft', label = 'Squarefeet', 
#                                    value = 1000, min=250, max = 10000))
#                 )

ui <- fluidPage(leafletOutput("map", height = 1000),
                
                #Input Panel
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 330, height = "auto",
                
                h4("Consumption Inputs", align = 'center'),
                column(12,sliderInput(inputId='empl_slide', label = '# Employees', value = 25, min=10, max = 100),
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
                              draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 60,
                              width = 330, height = "auto",
                              plotOutput("histtemp", height = 250))
                
                )


server <- function(input,output, session){

downloaddir<-getwd()
setwd('C:/Users/eyang/Desktop/shiny')
zip_codes = read.csv('ca_zips.csv') 
dat<-readOGR(downloaddir, "cb_2016_us_zcta510_500k") 

subdat<-dat[dat$GEOID10 %in% zip_codes$GEOID10,]

# ----- Transform to EPSG 4326 - WGS84 (required)
subdat<-spTransform(subdat, CRS("+init=epsg:4326"))

# ----- save the data slot
subdat_data<-subdat@data[,c("GEOID10", "ALAND10")]

# ----- simplification yields a SpatialPolygons class
subdat<-gSimplify(subdat,tol=0.01, topologyPreserve=TRUE)

# ----- to write to geojson we need a SpatialPolygonsDataFrame
subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)

#-----merge in density data
subdat <- merge(x = subdat, y = zip_codes, by = "GEOID10", all.x = TRUE)

labels <- sprintf(
  "<strong>%s",
  subdat$GEOID10
) %>% lapply(htmltools::HTML)


#------Setup Density Buckets
bins <- c(0.001, .01, .02, .05, .1, .2, .5, 1, Inf)
pal <- colorBin("YlOrRd", domain = subdat$Density, bins = bins)

#----Setup Temp Datasets
x <- rnorm(60)

#-------Map Visualization
output$map <- renderLeaflet({
leaflet() %>%
  addTiles() %>%
  addPolygons(data=subdat, 
              fillColor = ~pal(subdat$Density),
              fillOpacity = 0.45,
              weight = .5, 
              highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))

})

#-----Consumption Graphs
output$histtemp <- renderPlot({hist(x,
     main = "Energy Generation",
     xlab = "Years",
     col = '#00DD00',
     border = 'white')})
  }

shinyApp(ui=ui, server=server)