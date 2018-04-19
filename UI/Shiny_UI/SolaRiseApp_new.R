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
library(xml2)
library(rvest)
library(stringr)
library(rjson)
library(reshape)
library(FinancialMath)

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

# generate plot data
plot_data <- function(station_id, plot_type) {
  if (plot_type == 'Monthly Average') {
    plot_data = monthly_avg(station_id)
  } else if (plot_type == 'Weekly Average') {
    plot_data = weekly_avg(station_id)
  } else {
    print("Invalid selection")
  }
  plot_data
}

# generate plot label
plot_label <- function(station_id, plot_type) {
  if (plot_type == 'Monthly Average') {
    plot_label = months(index(monthly_avg(station_id)))
  } else if (plot_type == 'Weekly Average') {
    plot_label = week(index(weekly_avg(station_id)))
  } else {
    print("Invalid selection")
  }
  plot_label
}

# HTTP Interface
add_eq <- function(x) {
  paste("&", x, "=Y", sep = "")
}

process_params <- function(params) {
  params_l = strsplit(params, ", ")
  params_y = unlist(lapply(params_l, add_eq))
  result = paste(params_y, collapse='')
  result
}

collect_params <- function(station,
                           biz_type, 
                           employee_count, 
                           roof_size,
                           office_size, 
                           weekday_start,
                           weekday_hours,
                           weekend_start,
                           weekend_hours,
                           consumption1,
                           consumption2 ) {
  params_final = paste("http://solarise.7bsuxv7th2.us-west-2.elasticbeanstalk.com/solarise?",
                       "station=", toString(station),
                       "&biz=", toString(biz_type),
                       "&wkrs=", toString(employee_count),
                       "&sqft=", toString(office_size),
                       "&roof=", toString(roof_size),                 
                       "&wd_st=", toString(weekday_start),
                       "&wd_hrs=",toString(weekday_hours),
                       "&we_st=", toString(weekend_start),
                       "&we_hrs=",toString(weekend_hours),
                       process_params(toString(consumption1)),
                       process_params(toString(consumption2)), 
                       sep = "")
  params_final
}

# read_http <- function(http) {
#   pg = read_html(http)
#   body_text = pg %>% html_nodes("body") %>% html_text()
#   json_data = fromJSON(body_text)
#   json_data
# }
break_even_df <- function(json_data) {
  start = seq(1, 120, by = 12)
  end = seq(12, 120, by = 12)
  saving = numeric()
  maintain = numeric()
  for (year in 1:10){
    saving[year] = sum(json_data$saving[start[year]:end[year]])
    maintain[year] = -1 * (sum(json_data$cleaning_cost[start[year]:end[year]]) + sum(json_data$inspection_cost[start[year]:end[year]]))
  }
  cost = json_data$init_cost + json_data$incentive
  cost_dep = cost * c(0.2, 0.32, 0.192, 0.1152, 0.1152, 0.0576, 0.0, 0.0, 0.0, 0.0)
  total_cost = cost_dep + maintain
  profit = saving + total_cost
  # Cumsum
  saving_cs = cumsum(saving)
  cost_cs = cumsum(total_cost)
  profit_cs = cumsum(profit)
  
  breakeven = setNames(data.frame(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    saving_cs, cost_cs, profit_cs),
    c('Year', 'Accum_Saving', 'Accum_Cost', 'Accum_Profit')
  )
  breakeven 
}

cash_flow_df <- function(json_data) {
  start = seq(1, 120, by = 12)
  end = seq(12, 120, by = 12)
  saving = numeric()
  maintain = numeric()
  cost = json_data$init_cost + json_data$incentive
  for (year in 1:10){
    saving[year] = sum(json_data$saving[start[year]:end[year]])
    maintain[year] = -1 * (sum(json_data$cleaning_cost[start[year]:end[year]]) + sum(json_data$inspection_cost[start[year]:end[year]]))
  }
  cost = c(cost, rep(0, 10))
  saving = c(0, saving)
  maintain = c(0, maintain)
  cashflow = saving + cost + maintain
  cash_flow = setNames(data.frame(
    c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    saving, cost, maintain, cashflow),
    c('Year', 'Saving', 'Fixed_Cost', 'Maintenance_Cost', 'Cashflow')
  )
  cash_flow 
}

consum_http <- function(json_data) {
  sum(json_data$cons)/10.0
}

gen_http <- function(json_data) {
  sum(json_data$gen)/10.0
}

opt_http <- function(json_data) {
  sum(json_data$rec_cap)/120
}

breakeven_plot <- function(break_even) {
  melted_breakeven = setNames(melt(break_even, id = 'Year'), c('Year', 'Labels', 'value'))
  melted_breakeven
}

be_table <- function(breakeven) {
  breakeven_T = setNames(cbind(c("Accum_Saving", "Accum_Depreciation", "Accum_Profit"), transpose(breakeven[2:4])), 
                         c("Depreciation", "Year 1", "Year 2", "Year 3", "Year 4", 
                           "Year 5", "Year 6", "Year 7", "Year 8", "Year 9", "Year 10"))
  breakeven_T
} 

pnl_table <- function(cashflow) {
  cashflow_T = setNames(cbind(c("Saving", "Fixed_Cost", "Maintenance_Cost", "Cashflow"), transpose(cashflow[2:5])), 
                         c("Profit_&_Loss", "Year 0", "Year 1", "Year 2", "Year 3", "Year 4", 
                           "Year 5", "Year 6", "Year 7", "Year 8", "Year 9", "Year 10"))
  cashflow_T
} 

roi_cal <- function(be, i) {
  npv_rev = NPV(i = i, cf0 = be$Saving[1], cf = be$Saving[2:11], time = be$Year[2:11])
  npv_fixed_cost = NPV(i = i, cf0 = be$Fixed_Cost[1], cf = be$Fixed_Cost[2:11], time = be$Year[2:11])
  npv_maintain_cost = NPV(i = i, cf0 = be$Maintenance_Cost[1], cf = be$Maintenance_Cost[2:11], time = be$Year[2:11])
  npv_cost = npv_fixed_cost + npv_maintain_cost
  npv = npv_rev + npv_cost
  roi = abs(npv_rev / npv_cost)
  finance = setNames(data.frame(c('NPV Revenue', 'NPV Cost', 'NPV Cashflow', 'ROI'), c(npv_rev, npv_cost, npv, roi)),
                     c("Net Present Values", ""))
  finance
}

roi <- function(be, i) {
  npv_rev = NPV(i = i, cf0 = be$Saving[1], cf = be$Saving[2:11], time = be$Year[2:11])
  npv_fixed_cost = NPV(i = i, cf0 = be$Fixed_Cost[1], cf = be$Fixed_Cost[2:11], time = be$Year[2:11])
  npv_maintain_cost = NPV(i = i, cf0 = be$Maintenance_Cost[1], cf = be$Maintenance_Cost[2:11], time = be$Year[2:11])
  npv_cost = npv_fixed_cost + npv_maintain_cost
  roi = abs(npv_rev / npv_cost)
  roi
}

decision <- function(cf, be, i, roi, expect) {
  be_year_first = be$Year[be$Accum_Profit >= 0][1]
  be_year_second = be$Year[be$Accum_Profit >= 0][2]
  if (be_year_first == 1) {
    if (be$Accum_Profit[2] > 0) {
      be_year = be_year_first
    } else {
      be_year = be_year_second
    }
  } else { 
    be_year = be_year_first
  } 
    
  roi_to_compare = roi(cf, i)
  if ((be_year <= expect) && (roi_to_compare >= roi)) {
    text = paste("YES, Solar Panel Installation Meets Your Investment Objectives")
  } else {
    text = paste("NO, Solar Panel Install Does Not Meet Your Investment Objectives")
  }
}

#------------
# UI Section
#------------

ui <- fluidPage("",
  #theme = 'agency.css',
  theme = shinytheme("yeti"),
  titlePanel(windowTitle = "SolaRise", title = div("SolaRise Business Optimizer", 
             img(src='power_solar.png', height = 50, width = 75))),  
  navbarPage(title = '', 
             id="nav",
             #theme = 'agency.css',
             tabPanel(title = "About",
                      fluidRow(column(12,img(src='AboutPrimary.png', height = "auto", width = "100%", 
                                       top = 20, bottom = 20, align = "center"))), 
                      fluidRow(column(12, h2("What You Need to Know", align = 'center'))),

                      fluidRow(
                        column(2, img(src='sun.png', height = "auto", width = "70%", 
                                      top = 20, bottom = 20, align = "right")), 
                        column(10, h3("What's My Energy Generation Potential?", align = 'left'),
                               p("Based on your location we utilize machine learing and forecasting methods to predict the future potential
                                 for energy generation in your specific area."))),

                      fluidRow(column(12, div(style = "height:40px;"))),
                      fluidRow(
                        column(2, img(src='consump.png', height = "auto", width = "70%", 
                                      top = 20, bottom = 20, align = "right")), 
                        column(10, h3("How Do My Usage Patterns Impact Savings?", align = 'left'),
                               p("Provide us the specifics for your business and we estimate your current and future energy 
                                 consumption needs."))),                      
                      fluidRow(column(12, div(style = "height:40px;"))),
                      fluidRow(
                        column(2, img(src='savings.png', height = "auto", width = "70%", 
                                      top = 20, bottom = 20, align = "right")), 
                        column(10, h3("How Much Do I Need to Spend?", align = 'left'),
                               p("We optimize a system.  Estimate costs and determine any local rebates available to you."))),                        
                      
                      fluidRow(column(12, div(style = "height:40px;"))),
                      fluidRow(
                        column(12, img(src='whySolarise.png', height = "auto", width = "100%", 
                                       top = 20, bottom = 20, align = "center"))),
                      fluidRow(column(12, div(style = "height:10px;")))
                               ),
             tabPanel("Our Methodology",
                      fluidRow(
                        column(2),
                        column(8, align="center", 
                               h2("Data Science for Business Decision", align = 'center'),
                               p(),
                               p("Solarise's product philosophy focuses on using data science and machine learning for better 
                                 business decision making. User inputs combined with data from external sources are fed into mutliple
                                 stages of machine learning models to forecast solar potential, energy consumption, and business
                                 scenarios. The final model produces an optimized muti-year cach flow analysis for the solar panel investment. 
                                 Users can futher adjust key finacial parameters before making a final decision", align = 'left'),
                               p(),
                               img(src='ds_decision.png', height = "auto", width = "80%",  top = 20, bottom = 20), 
                               h4("Design Methodology", align="center")),
                        column(2)),
                      fluidRow(column(12, div(style = "height:50px;"))),
                      fluidRow(
                        column(2),
                        column(8, align="center",  
                               h2("Data Sources", align = 'center'), 
                               fluidRow(
                                 column(1, img(src='weather_data.png', height = "auto", width = "100%", 
                                        top = 20, bottom = 0, align = "left")), 
                                 column(7, align = 'left',
                                        h3("Weather Data Sources", align = "left"),
                                        a("NOAA's National Solar Radiation Data Base ",href= 'https://www1.ncdc.noaa.gov/', align = "left"),
                                        #a("https://www1.ncdc.noaa.gov/", align = "left"),
                                        fluidRow(column(7, div(style = "height:10px;"))),
                                        a("California Energy Commission",href="http://www.energy.ca.gov/maps/renewable/building_climate_zones.html", align = "left"))),
                                        #a("http://www.energy.ca.gov/maps/renewable/building_climate_zones.html", align = "left"))),
                               fluidRow(column(12, div(style = "height:30px;"))),
                               fluidRow(
                                 column(1, img(src='consumption_data.png', height = "auto", width = "80%", 
                                        top = 20, bottom = 0, align = "left")), 
                                 column(7, align = 'left',
                                        h3("Consumption Data Source", align = "left"),
                                        a("US Energy Information Administration",href= 'https://www.eia.gov/consumption/commercial/', align = "left"))), 
                                        

                               fluidRow(column(12, div(style = "height:30px;"))),
                               fluidRow(
                                 column(1, img(src='cash.png', height = "auto", width = "100%", 
                                               top = 20, bottom = 0, align = "left")), 
                                 column(7, align = 'left',
                                        h3("Incentives Data Source", align = "left"),
                                        a("Database of State Incentives for Reneawbles & Efficiency",href= 'http://www.dsireusa.org/', align = "left")))                              
                               
                               ),
                        column(2)),
                      fluidRow(column(12, div(style = "height:50px;"))),
                      fluidRow(
                        column(2),
                        column(8, align="center", 
                               h2("Solar Terminology and Depreciation", align = 'center'),
                               p(),
                               h4("Solar Potential Index (SPI)", align = 'left'),
                               tags$li("Global Horizontal Irradiance = (diffused horizontal irradiance) + (direct normal rrradiance) * cos(θ)", align="left"),
                               tags$li("SPI = average(global horizontal irradiance) / Max(global horizontal irradiance)", align = 'left'),
                               tags$li("Unit: a ratio between 0 and 1", align = 'left'),
                               tags$li("θ: Zenith angle of the Sun", align = 'left'),
                               img(src='zenith.jpg', height = "auto", width = "40%",  top = 20, bottom = 20), 
                               p(),
                               tags$li("Solar Generation (kilowatt) = SPI * installed capacity", align = 'left'),
                               h4("Solar System Cost Depreciation", align="left"),
                               tags$li("Commercial solar energy system qualify for accelerated depreciation under IRS MACRS schedule", align = 'left'),
                               tags$li("MACRS is a way for businesses to achieve a partial tax break", align = 'left'),
                               tags$li("The MACRS cost depreciation rate is defined as", align = 'left'),
                               img(src='solar_dep.png', height = "auto", width = "20%",  top = 20, bottom = 20) 
                               ),
                        
                        column(2)),
                      fluidRow(column(12, div(style = "height:50px;"))),
                      fluidRow(
                        column(2),
                        column(8, align="center",  
                               h2("Model Interaction", align = 'center'),
                               p(),
                               p("There are a number models that interact with each other in our design. Here are some details:", align = "left"),
                               h3("Solar Generation:", align="left"),
                               p("We utilize California weather station data to build a clustering model. The model outputs a list of climate regions 
                                 for the state. Within each climate region, we identify the centroid and build a time series forecast for solar irradiance. 
                                 Using location data we are able to associate any California address with a specific climate region and it's respective 
                                 future irradiance potential.", align = 'left'),
                               h3("Energy Consumption:", align="left"),
                               p("We use national commercial building energy consumption data to build an OLS regression model. Based on 
                                 user input in business type, size, building features, and operation hours, we can predict a consumption
                                 pattern for the business", align="left"),
                               h3("Optimization:", align="left"),
                               p("Lastly, we feed the solar generation potential, expected consumption patterns, and other constrains to 
                                 a linear progarmming model for optimization. This final model produces an ideal cachflow
                                 based on the given information", align="left"),
                               p(), 
                               img(src='models_inter.png', height = "auto", width = "90%", top = 20, bottom = 20), 
                               h4("Model Interaction", align="center")),
                        column(2)),
                      fluidRow(column(12, div(style = "height:50px;"))),
                      fluidRow(
                        column(2),
                        column(8, align="center",  
                               h2("Core Optimization Model", align = 'center'), 
                               p("Our optimization model utilizes the GLPK (GNU Linear Programming Kit) solver to determine the most optimal solar system size 
                                 based on locations and specific characteristics of the businesses as described by our users, such as number of employees, 
                                 building size, operation hours, etc.  These inputs drive the results of our Solar Irradiance and Energy Consumption models,
                                 which get fed into the optimization model.  Our optimization algorithm then maximizes net energy savings by considering amount 
                                 of solar energy generation, consumption patterns, installation cost, maintenance costs, etc.  An ideal cashflow and a final 
                                 recommendation on solar adoption will be produced based on results of the model.", align="left"),
                               img(src='optimization.png', height = "auto", width = "80%", top = 20, bottom = 20)), 
                        column(2))
                      ), 
             tabPanel("Your Business", 
                
                      #Input Panel
                      column(4, 
                        wellPanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, left = "auto", right = "auto", bottom = "auto",
                                      width = "auto", height = "auto",
                                      
                                  h4("Enter Your Location", align = 'center'),
                                  textInput(inputId = "Address", label = NULL, width = '100%',
                                                value = "e.g. 301 Old San Francisco Rd, Sunnyvale, CA 94086"),
                                  actionButton("go", "Find Address"),
                                  HTML('<hr style="color: grey;">'),
                                  
                                  h4("Consumption Inputs", align = 'center'),
                                  selectInput("biz_select", "Select Business Type:", 
                                                                     c("Office" = "ofc", 
                                                                       "Warehouse" = "war", 
                                                                       "Restuarant" = "rest",
                                                                       "Laboratory" = "lab",
                                                                       "Convenience Store" = "conv",
                                                                       "Religious Worship" = "reli", 
                                                                       "Outpatient Health" = "out_health",
                                                                       "Inpatient Health" = "in_health",
                                                                       "Education" = "edu",
                                                                       "Nursing Home" = "nurse",
                                                                       "Lodging" = "lodge",
                                                                       "Strip Mall" = "strip",
                                                                       "Enclosed Mall" = "enc_mall",
                                                                       "Retail Other" = "ret_oth",
                                                                       "Service" = "service",
                                                                       "Other" = "oth"
                                                                       )),
                                 column(12,sliderInput(inputId='roof_slide', label = 'Roof Size (Sq ft)', value = 1500, min=250, max = 10000)),
                                 column(6,sliderInput(inputId='empl_slide', label = '# Employees', value = 15, min=10, max = 100)),
                                 column(6,sliderInput(inputId='sqft_slide', label = 'Building Size (Sq ft)', value = 2000, min=250, max = 10000)),
                                 column(6,sliderInput(inputId='weekday_start', label = 'Weekday Start Hour', value = 8, min=0, max = 23)),
                                 column(6,sliderInput(inputId='weekday_total', label = 'Weekday Daily Hours', value = 10, min=0, max = 24)),
                                 column(6,sliderInput(inputId='weekend_start', label = 'Weekend Start Hour', value = 10, min=0, max = 23)),
                                 column(6,sliderInput(inputId='weekend_total', label = 'Weekend Daily Hours', value = 12, min=0, max = 24)),                                  
                                 column(6,checkboxGroupInput(inputId='consump_check', label = 'Check All That Apply', 
                                                             selected = 'elec_Cool', 
                                                             choices = c('Open 24' = 'open_24', 
                                                                         'Electric Heat' = 'elec_Heat', 
                                                                         'Electric Cool' = 'elec_Cool'))),
                                 column(6,checkboxGroupInput(inputId='consump_check_2', label = NULL, 
                                                             selected = "refridg",
                                                             choices = c('Open Weekend' = 'open_wkend',
                                                                         'Electric Cook' = 'elec_cook',
                                                                         'Refridgeration' = 'refridg',
                                                                         'Electic Manufacture' = 'ele_mfg'))),
                                  actionButton("detail", "Go to Report")
                                 )),
                      column(8, leafletOutput("map", height = 900),
                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = FALSE, top = "auto", left = "auto", right = 50, bottom = 40,
                                           width = 275, height = "auto",
                                           plotOutput("spi.hist", height = 275)))
                      ),
             tabPanel(title = "Solarise Report",
                      column(4, 
                        wellPanel(style = "position:fixed;", id = "controls", class = "panel panel-default", fixed = TRUE, 
                                      draggable = FALSE, left = "auto", right = "auto", bottom = "auto", top="auto",
                                      width = 300, height = "auto",
                                  h4("Select Solar Potential Plot", align = 'center'),
                                  selectInput("spi_plot", label="select plot", c("Monthly Average" = "Monthly Average", "Weekly Average" = "Weekly Average")),
                                  h4("Discount Rate (%)", align = 'center'),
                                  sliderInput(inputId='discount_slide', label = "%", value = 8, step=0.5, min=4, max = 12),
                                  h4("Target ROI", align = 'center'),
                                  sliderInput(inputId='roi_slide', label = "ROI", value = 2, step = 0.1, min=1, max = 5),
                                  h4("Target Payback Period", align = 'center'),
                                  sliderInput(inputId='payback_slide', label = 'Years', value = 5, min=2, max = 10)
                        )),
                      column(8,
                        h3("Personalized Report", align = 'center'),
                        fluidRow(column(12, div(style = "height:50px;"))),
                        h4("Solar Potential Forecast", align = 'left'),
                        fluidRow(
                          column(12, plotOutput("SPI", height = 400))),  
                        fluidRow(column(12, div(style = "height:50px;"))),
                        h4("Business Summary", align = 'left'),
                        h3(textOutput("sys_optimal"), align = 'left'),
                        #h5(textOutput("gen_total"), align = 'left'),
                        fluidRow(column(12, div(style = "height:20px;"))),
                        fluidRow(
                          column(4, tableOutput("BusinessAttri")),
                          column(4, plotOutput("kw", height =250))
                          #column(12, textOutput("HTTP"))
                          ),
                        fluidRow(column(12, div(style = "height:50px;"))),
                        h4("ROI Analysis", align = 'left'),
                        fluidRow(
                          column(12, plotOutput("ROI", height = 400))
                          ),
                        fluidRow(
                          h5("Breakeven Analysis Table", align = 'left'),
                          column(12, tableOutput("be"))
                        ),
                        fluidRow(column(12, div(style = "height:50px;"))),
                        fluidRow(
                          h5("Cashflow Analysis Table", align = 'left'),
                          column(12, tableOutput("pnl")),
                          fluidRow(column(12, div(style = "height:50px;"))),
                          h5("ROI Analysis Table", align = 'left'),
                          column(12, tableOutput("roi"))
                        ),
                        fluidRow(column(12, div(style = "height:50px;"))),
                        fluidRow(column(2, imageOutput("decision_image")),
                                 column(10, h2(textOutput("decision_message"))))
                        )
               ),
             tabPanel(title = "Next Steps",
                        column(2),
                        column(8, align="center",  
                               h3("Solar Storage", align = 'center'),
                               img(src='solar_storage.jpg', height = "auto", width = "70%", top = 20, bottom = 20), 
                               p(),
                               p("Solar energy storage is a natural extention of the Solarise project. Currently, the Solarise model   
                                  only considers solar generation during the day and energy consumption during the business operation hours.
                                  The benefit and flexibility of solar energy can be further realized with the addition of efficient storage devices.",
                                 align = 'left'),
                               fluidRow(column(12, div(style = "height:50px;"))),
                               h3("Solar Installation Incentive", align="Center"),
                               img(src='solar_incentive.jpg', height = "auto", width = "80%", top = 20, bottom = 20), 
                               p("In our current model implementation, we considered a number of solar panel installation incentives. However,
                                  the data we can currently access are static. The energy polices and the solar incentives are changing frequently 
                                  based on market demand and the political enviroment. Future enhancdments would incoorperate newly released solar incetive 
                                  programs and more accurately estimate the financial impact.", align="left"),
                               fluidRow(column(12, div(style = "height:50px;"))),
                               h3("Continuous Learning of User Consumption Behavior", align="Center"),
                               img(src='cont_learning.png', height = "auto", width = "70%", top = 20, bottom = 20), 
                               p("The published business consumption data is very limited in data size and feature richness. Therefore, the consumption regression
                                 model we built can be improved. As more and more user use the SolaRise tool, we will store, retrain, and 
                                 further improve the consumption and optimization models", align="left")),
                        column(2)),
             tabPanel(title = "Our Team",
                      fluidRow(column(12, div(style = "height:100px;"))),
                      fluidRow(
                        column(4, align='center',
                               tags$img(src='eric.jpeg', height = "250", width = "auto", class = "img-fluid rounded_circle",
                                      top = 20, bottom = 20, align = "center"),  
                               h4("Eric Yang"),
                               a(
                                 icon(name = "linkedin", class="fa-3x", lib = "font-awesome"), 
                                 href= 'https://www.linkedin.com/in/eric-yang-5a10646/')),
                        
                        column(4, align='center',
                               img(src='qian.jpg', height = "250", width = "auto", 
                                      top = 20, bottom = 20, align = "center"),  
                               h4("Qian Yu"),
                               a(
                                 icon(name = "linkedin", class="fa-3x", lib = "font-awesome"), 
                                 href= 'https://www.linkedin.com/in/qyupublic/')),
                        column(4, align='center',
                               img(src='vchu.png', height = "250", width = "auto", 
                                      top = 20, bottom = 20, align = "center"),  
                               h4("Vincent Chu"),
                               a(
                                 icon(name = "linkedin", class="fa-3x", lib = "font-awesome"), 
                                 href= 'https://www.linkedin.com/in/vincent-chu-9b23311/'))
                      )
                     )
  )
)

server <- function(input,output, session){
  
  subdat <- readRDS("subdat.rds")
  
  labels <- sprintf(
    "<strong>%s</strong><br/>Zip: %g<br/>SPI: %g",
    subdat$City, subdat$GEOID10, round(subdat$Density, 2)
  ) %>% lapply(htmltools::HTML)
  
  #------Setup Density Buckets
  bins <- c(0.40, 0.43, 0.46, 0.48, 0.51, 0.54, 0.57, 0.59, 0.62)
  pal <- colorBin("RdYlGn", domain = subdat$Density, bins = bins)

  #------Business Attribute
  biz_type <- reactive({ input$biz_select
  })
  employee_count <- reactive({ input$empl_slide 
  })
  office_size <- reactive({ input$sqft_slide 
  })
  roof_size <- reactive({ input$roof_slide 
  })
  weekday_start <- reactive({ input$weekday_start 
  })
  weekday_hours <- reactive({ input$weekday_total 
  })
  weekend_start <- reactive({ input$weekend_start 
  })
  weekend_hours <- reactive({ input$weekend_total 
  })
  consumption1 <- reactive({ input$consump_check 
  })
  consumption2 <- reactive({ input$consump_check_2 
  })
  discount <- reactive({ input$discount_slide 
  })
  payback <- reactive({ input$payback_slide 
  })
  
  
  #-----Map Search
  points <- eventReactive(input$go, {geocode(input$Address, output='latlon', source = "dsk")})
  http <- eventReactive(input$detail, {
    collect_params(nearest_station(stations, points())[2],
                   biz_type(), 
                   employee_count(), 
                   roof_size(), 
                   office_size(), 
                   weekday_start(),
                   weekday_hours(),
                   weekend_start(),
                   weekend_hours(),
                   consumption1(),
                   consumption2()) 
  })
  
  read_http <-eventReactive(input$detail,{    
    pg = read_html(http())
    body_text = pg %>% html_nodes("body") %>% html_text()
    json_data = fromJSON(body_text)
    json_data})
  
  breakeven_P <- eventReactive(input$detail, {break_even_df(read_http())})
  breakeven_T <- eventReactive(input$detail, {break_even_df(read_http())})
  cashflow_T <- eventReactive(input$detail, {cash_flow_df(read_http())})
  roi_T <- eventReactive(input$detail, {cash_flow_df(read_http())})
  decision_T <- eventReactive(input$detail, {break_even_df(read_http())})
  
  
  #rev.zip <- eventReactive(input$go, {revgeocode(as.numeric(geocode(input$Address)),output = 'more')$postal_code})
  
  
  rev.zip <- eventReactive(input$go, {as.numeric(as.character(revgeocode(c(geocode(input$Address, output='latlon', source = "dsk")$lon,
                                                                           geocode(input$Address, output='latlon', source = "dsk")$lat),
                                                                         output = 'more')$postal_code))})
  
  zip.spi <- eventReactive(input$go, {subdat[subdat$GEOID10 == as.numeric(as.character(revgeocode(c(geocode(input$Address, output='latlon', source = "dsk")$lon,
                                                                                                         geocode(input$Address, output='latlon', source = "dsk")$lat),
                                                                                                       output = 'more')$postal_code)),]$Density})
  
  observeEvent(input$detail, {
    updateNavbarPage(session = session, inputId='nav', selected = 'Solarise Report')
  })
  
  #-------Map Visualization
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(label = if (input$go==0) {labels} else {NULL},
                  data= if (input$go==0) {subdat} else {subdat[subdat$GEOID10 == rev.zip(),]}, 
                  fillColor =if (input$go==0) {~pal(subdat$Density)} else {~pal(subdat[subdat$GEOID10 == rev.zip(),]$Density)},
                  fillOpacity =if (input$go==0) {0.45} else {0.00045},
                  weight = .5, 
                  highlightOptions = highlightOptions(color = "green", weight = 2,bringToFront = TRUE),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>%
#<<<<<<< HEAD
      setView(if(input$go==0) {lng = -116.4179} else {points()},if(input$go==0) {lat = 36.7783} else {points()}, if(input$go==0) {zoom = 6} else {zoom=11}) %>% 
      addMarkers(if(input$go==0) {lng = 116.3636} else {points()[,1]},if(input$go==0) {lat = 39.91} else {points()[,2]}) %>% 
      leaflet::addLegend(pal=pal,value = subdat$Density, opacity = 0.7, title = "Solar Potential Index", position = "topright")
      
  })
  
  #-----Consumption Graphs
  
  output$SPI = renderPlot({
    #par(bg = '#2FA4E7')
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Creating Solar Potential Plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 8
      
      for (i in 1:n) {
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("step", i))
        
        Sys.sleep(0.1)
      }
    })
    barplot(plot_data(nearest_station(stations, points())[2], input$spi_plot), 
            names.arg = plot_label(nearest_station(stations, points())[2], input$spi_plot), 
            main = input$spi_plot,
            ylab = 'Solar Potential Index',
            xlab = if (input$spi_plot == 'Monthly Average') "Month" else "Week",
            cex.main = 1.5,
            cex.lab = 0.8,
            cex.axis = 0.8,
            ylim = c(0.0, 1.05),
            beside = TRUE,
            col = c("dodgerblue", "blue", "navy"))
    legend("topright", c("Worst","Average","Best"), fill = c("dodgerblue", "blue", "navy"))
  })
  
  output$be = renderTable({ 
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Building P&L Table', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("step", i))
        
        Sys.sleep(0.01)
      }
    })
    be_table(breakeven_T()) })
  
  output$pnl = renderTable({ 
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Building P&L Table', value = 0, {
      # Number of times we'll go through the loop
      n <- 8
      
      for (i in 1:n) {
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("step", i))
        
        Sys.sleep(0.1)
      }
    })
    pnl_table(cashflow_T()) })
  
  output$roi = renderTable({
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Creating ROI Table', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("step", i))
        
        Sys.sleep(0.005)
      }
    })
    roi_cal(roi_T(), input$discount_slide * 0.01) })
  
  output$ROI = renderPlot({
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Creating Breakeven Plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("step", i))
        
        Sys.sleep(0.005)
      }
    })
    ggplot(breakeven_plot(breakeven_P()), aes(x=Year, y = value, color = Labels)) + 
      geom_line(size = 1.5) + 
      ylab('Dollar ($)') + 
      xlab('Year') + ggtitle("Solar Installation Breakeven") +
      geom_hline(yintercept = 0) + 
      scale_color_manual(values=c("darkolivegreen2", "firebrick", "royalblue")) + 
      theme_linedraw() + 
      theme(plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12))
  })
  
  output$spi.hist <- renderPlot({
    if (input$go==0)  {
      qplot(subdat$Density, geom="histogram", 
           binwidth = 0.0275, xlab = 'Solar Potential Index',
           ylab = 'Zipcode Count', main = 'SPI Histogram', 
           fill=I("lightsteelblue1")) + 
      theme_light() + 
      theme(text=element_text(size=10,  family="Arial"))} 
    else {
      qplot(subdat$Density, geom="histogram", 
            binwidth = 0.0275, xlab = 'Solar Potential Index', 
            ylab = 'Zipcode Count', main = 'SPI Histogram', 
            fill=I("lightsteelblue1")) + 
        theme_light()+ 
        theme(text=element_text(size=10,  family="Arial")) +
        geom_vline(xintercept = zip.spi(), linetype="dashed", color = "red", size=1.5)}}) 
                                        
  output$BusinessAttri = renderTable({
    setNames( 
      data.frame(  
        c("Climate Zone", "Employee Size", "Office Size (Sq Ft)", "Roof Size (Sq Ft)", "weekday_hours", "weekend_hours", "Consumption", ""),  
        c(nearest_station(stations, points())[1], employee_count(), office_size(), roof_size(), weekday_hours(), 
          weekend_hours(), toString(consumption1()), toString(consumption2()))
        ),
      c("Business Information", "Values")
      )
  })
  
  output$decision_message = renderText({ 
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Recommandation...', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("step", i))
        
        Sys.sleep(0.005)
      }
    })
    decision(cashflow_T(), decision_T(), input$discount_slide * 0.01, input$roi_slide, input$payback_slide)
  })

   output$decision_image = renderImage({
     list(src = if (str_detect(decision(cashflow_T(), decision_T(), input$discount_slide * 0.01, input$roi_slide, input$payback_slide), 'YES')) {
       'check.jpg'} else {'xbox.jpg'}, contentType = 'image/jpeg', width = 100, height = 100)}, deleteFile = FALSE)
   
   #output$consump_total = renderText({paste('Annual Consumption Estimate: ', round(consum_http(read_http())), ' kW')})
   output$sys_optimal = renderText({paste('Your Optimal System Size is ', opt_http(read_http()), ' KW')})
   
   output$kw = renderPlot({
     x <- data.frame(kilowatts = c(round(consum_http(read_http())),round(gen_http(read_http()))), type = c('consumption', 'generation'))
     
     ggplot(data=x, aes(x=type, y=kilowatts)) +
       geom_bar(stat="identity", fill= c("green3", "darkgreen"))+
       geom_text(aes(label=kilowatts), vjust=1.6, color="white",
                 position = position_dodge(0.9), size=3.5) + ggtitle("Average Yearly Energy Profile") + theme_minimal()
     
     # barplot(c(gen_http(read_http()),consum_http(read_http())),
     #         names.arg = c('Generation', 'Consumption'), main = 'Average Yearly Energy Profile', col=c("green","red"), xlab = 'kilowatts')
   })
   
   }



shinyApp(ui=ui, server=server)