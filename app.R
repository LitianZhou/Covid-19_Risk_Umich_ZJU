library(shiny)
library(readr)
library(magrittr)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(tigris)
library(DT)
library(readxl)
COVID19_by_Neighborhood <- read.csv("data/COVID19_by_Neighborhood.csv")

zipcode_daily <- read_csv("data/zipcode_daily_with_1_11_future_day_case_count.csv")

# some new datasets, might be helpful
# when I show the .xlsx dataset, there are something wrong with the date, so I change the xlsx file into .csv file
#zipcode_daily_income <- read_excel("data/zipcode_daily_cases&social-distance&population&income.xlsx")
zipcode_daily_income <- read_csv("data/zipcode_daily_cases&social-distance&population&income.csv") 
places_totals <- read_csv("data/latimes-place-totals.csv")
risk_scores_table <- read_csv("data/risk_score.csv")


#data cleaning
#get combined table for the three trend plot variables 
infect_rate_by_zip <- zipcode_daily_income %>% transmute(value = new_confirmed_cases/population * 100000, date, ZIP, type = "Infection Rate")
risk_scores <- risk_scores_table %>%  transmute(value = value * 1000, date, ZIP, type = "Risk Score")
mobility_indices <- zipcode_daily_income %>% transmute(ZIP, date, type = "Mobility Index", value = (median_home_dwell_time - median_non_home_dwell_time) * distance_traveled_from_home / 600000) 
joined_df <- rbind(infect_rate_by_zip, risk_scores)
trend <- rbind(joined_df, mobility_indices) 
#convert the date column to date object and arrange by date order
trend$date <- ymd(trend$date)
arrange(trend, date)

# char_zips <- zctas(cb = TRUE, starts_with = c("90","91","92"))
# saveRDS(char_zips, "char_zips.rds")
# Our reference
# 1. London map:https://www.doorda.com/covid-19-data-free-download/#1589463968147-d5954274-124c
# 2. SuperZip map:https://shiny.rstudio.com/gallery/superzip-example.html

ui <- fluidPage(
    titlePanel("Covid-19 Risk Umich + ZJU"),
    tabsetPanel(
        tabPanel("Interactive Map",
                 textOutput("select_stat"),
                 leafletOutput("map",width = "100%", height = 700),
                 
                 # to-dos: see London map
                 # 1. adjust the text position to the middle(align to the middle)
                 # 2. add a risk scale at the top
                 # 3. let user able to wrap up the filter ()
                 # 4. change neighborhood input to zipcode input
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = "120", left = "70", 
                               right = "auto", bottom = "auto",
                               width = "330", height = "500",
                               
                               h3("filter"),

                               selectInput("statistic",
                                           "You are interested in...",
                                           c("exposure risk", "public mobility", "death rate","infectious rate"),
                                           selected = "exposure risk"),
                               selectInput("neighborhood",
                                           "locate at ...",
                                           unique(COVID19_by_Neighborhood$COMTY_NAME),
                                           multiple = T),
                               plotly::plotlyOutput("hist", height = 200)
                 )
        ),
        tabPanel("Map by Statistics",
                 HTML("<button type='button' class='btn btn-danger' data-toggle='collapse' data-target='#demo'>Date Selector</button>"),
                 leafletOutput("map_stat",width = "100%", height = 700),
                 div(id = "demo", class = "collapse in", 
                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   draggable = TRUE, top = "120", left = "70", 
                                   right = "auto", bottom = "auto",
                                   width = "330", height = "500",
                         sliderInput(inputId = "dateInput", 
                                     label = "Dates:",
                                     min = as.Date("2020-03-15","%Y-%m-%d"),
                                     max = as.Date("2020-05-27","%Y-%m-%d"),
                                     value = as.Date("2020-04-29"), timeFormat="%Y-%m-%d", 
                                     step = 1,
                                     animate = animationOptions(interval = 1800)),
                         selectInput("stat_mode",
                                     "You are interested in...",
                                     c( "Infection Rate","Mobility Index", "Risk Score"),
                                     selected = "Infection Rate"),
                         selectInput("zip_search",
                                     "locate at ...",
                                     unique(risk_scores_table$ZIP),
                                     multiple = F),
                         textOutput("stat_output")
                     )
                 )
        ),
        tabPanel("Trend Plot",
                 sidebarPanel(
                   h3("Zip Code Selector"),
                   selectInput("zipID",
                               "locate at ...",
                               unique(risk_scores_table$ZIP),
                               multiple = F),
                   HTML("<button type='button' class='btn btn-danger' data-toggle='collapse' data-target='#explain'>Statistics Details</button>"),
                   div(id = "explain", class = "collapse", 
                       h5("Infection rate: "),
                       "daily new confirmed cases /population * 100000 (number of daily new confirmed cases per 100,000 population at the county level)",
                       h5("Risk score: "),
                       " LSTM model outcome * 1000",
                       h5("Mobility index: "),
                       "(median_home_dwell_time - median_non_home_dwell_time) * distance_traveled_from_home / 600,000",
                   )
                 ),
                 mainPanel(
                   plotly::plotlyOutput("trend_plot_by_county", height = 800)
                 )
        ),
        # a new kind of tab to show
        tabPanel("Raw data table(new)",
                 # controls to select a data set and spcify the number of observations to view
                 sidebarPanel(
                   selectInput("dataset","Choose a dataset:",
                               choices = c("zipcode_daily","place_totals", "trend_data", "zip_test")
                               ),
                   numericInput("obs", "Number of observations to view:", 10)
                 ),
                 mainPanel(
                   # summary of the dataset
                   # verbatimTextOutput("summary"),
                   tableOutput("view")
                 )
        ),
        tabPanel("Raw data table",
                 DT::dataTableOutput("rawData")
        )
    )
)

server <- function(input, output) {
    options(tigris_use_cache = TRUE)
    output$select_stat = renderText(paste("So you want to know",input$statistic))
    
    # the map is made from here:
    # to-dos: 
# 1.show the selected neighborhood AND zipcode only by input$zipcode and input$neighborhood
# 2.show a pop-up when the mouse hover over a zipcode, 
#     with information: zipcode, risk score, cummulative cases, population.
# 3. add search location bar on the right of zoom -/+ buttons. See London map (need leaflet API functionality)
# 4. add search zipcode bar on the right of 3., the map will zoom in to the user-typed in input$zip
    
    output$map = renderLeaflet({
      char_zips <- readRDS("char_zips.rds")
      
      # join zip boundaries and case count
      char_zips <- geo_join(char_zips, 
                            data.frame("zipcode"=char_zips$GEOID10,"RiskScore" = c(1:652)), 
                            by_sp = "GEOID10", 
                            by_df = "zipcode",
                            how = "left")
      pal <- colorNumeric(
        palette = "Reds",
        domain = c(1:700))
      
      labels <- paste0(
        "Zip Code: ",
        char_zips$GEOID10, "<br/>",
        "Case Count: ",
        char_zips$RiskScore) %>%
        lapply(htmltools::HTML)
      
      char_zips %>% 
        leaflet %>% 
        addProviderTiles("CartoDB.Voyager") %>% 
        setView(lng = -118.19, lat=34.05, zoom = 9) %>%
        addPolygons(fillColor = ~pal(RiskScore),
                    weight = 2,
                    opacity =0.15,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.5,
                    highlight = highlightOptions(weight = 2,
                                                 color = "#667",
                                                 dashArray = "",
                                                 fillOpacity = 0.7,
                                                 bringToFront = TRUE),label = labels) %>%
        addLegend(pal = pal, 
                  values = ~RiskScore, 
                  opacity = 0.7, 
                  title = htmltools::HTML("Case Count <br> 
                                    by Zip Code"),
                  position = "bottomright")
      
    })
    
    output$hist = renderPlotly({
        ggplotly(
            ggplot2::ggplot(data = COVID19_by_Neighborhood)+
                geom_histogram(aes(x=cases), binwidth=30) +
                theme_classic()
        )
    })
    char_zips <- readRDS("char_zips.rds")

    # get trend data at a specific date
    trend_date <- trend %>% filter(date == as.Date("2020-05-01")) %>% select(-date) %>% filter(type=="Risk Score")
    trend_date$value <- trunc(trend_date$value)

    # join zip boundaries and trend_date
    char_zips <- geo_join(char_zips, 
                          trend_date, 
                          by_sp = "GEOID10", 
                          by_df = "ZIP",
                          how = "left")
    
    # new raw_data table
    places_totals2 <- places_totals %>% select(date:new_confirmed_cases)
    # Return the requested dataset
    datasetInput <- reactive({
      switch(input$dataset,
             "zipcode_daily_income" = zipcode_daily_income,
             "place_totals" = places_totals2,
             "trend_data" = trend,
             "zip_test" = trend_date
             )
    })
    # Generate a summary of the dataset
    output$summary <- renderPrint({
      dataset <- datasetInput()
    })
    
    # Show the first "n" observations
    output$view <- renderTable({
      head(datasetInput(), n = input$obs)
    })
    
    
    
    # 2.show a pop-up when the mouse hover over a zipcode, 
    # with information: zipcode, risk score, cummulative cases, population.
    # there are some problems with the popup
    showZipcodePopup <- function(zipcode, lat, lng){
      selectedZip <- zipcode_daily[which((zipcode_daily$ZIP == zipcode)&&(zipcode_daily$date == 2020/5/15)),]
      content <- as.character(tagList(
        tags$h4("Confirmed cases:", as.integer(selectedZip$confirmed_cases)),
        tags$strong(HTML(sprintf("%s",
                                 selectedZip$ZIP))), tags$br(),
        sprintf("Population of this zipcode: %s", selectedZip$population)
      ))
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
    }
    
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if(is.null(event))
        return()
      print(event)
      # zipcode <- XY_zipcode[which((XY_zipcode$lat == event$lat)&(XY_zipcode$lng = event$lng)),]
      # zipcode <- zipcode$zipcode
      # print(zipcode)
      isolate({
        showZipcodePopup(event$id, event$lat, event$lng)
      })
    })
  
    #trend plot: return trend plot by zipcode entered
    output$trend_plot_by_county = renderPlotly({
      trend_input <- trend %>% filter(ZIP == input$zipID) 
      ggplotly(
          ggplot(data = trend_input, aes(x=date, y=value, color=type)) +
          geom_line(linetype = a) +
          geom_smooth(se = FALSE, linetype = "dashed") + 
          theme_classic()
      )

    })
    
    #stat map: render map based on date and statstic selected
    output$map_stat = renderLeaflet({
      char_zips <- readRDS("char_zips.rds")

      #specify date and type of stat(i.e. Infection Rate, Mobility Index, Risk Score)
      #round the values to integer to faciliate coloring
      stat_type <- input$stat_mode
      trend_date <- trend %>% filter(date == input$dateInput) %>% select(-date) %>% filter(type == stat_type)
      
      #get domain and palette, and process data based on stat choice
      if (stat_type == "Infection Rate"){
        trend_date <- trend_date %>% mutate(value_integer = trunc(log1p(value)))
        domain_in = c(0:7)
        palette_in = "Reds"
      } else if (stat_type == "Mobility Index"){
        trend_date <- trend_date %>% mutate(value_integer = trunc(value))
        
        domain_in = c(-110:100)
        rc1 <- colorRampPalette(colors = c("green", "white"), space = "Lab", bias = 1)(110)
        rc2 <- colorRampPalette(colors = c("white", "red"), space = "Lab", bias = 1)(100)
        palette_in = c(rc1, rc2)
      } else {
        trend_date <- trend_date %>% mutate(value_integer = trunc(value))
        domain_in = c(-12:200)
        rc1 <- colorRampPalette(colors = c("green", "white"), space = "Lab", bias = 1)(12)
        rc2 <- colorRampPalette(colors = c("white", "red"), space = "Lab", bias = 1)(200)
        palette_in = c(rc1, rc2)
        
      }
      
      
      char_zips <- geo_join(char_zips, 
                            trend_date, 
                            by_sp = "GEOID10", 
                            by_df = "ZIP",
                            how = "inner")
      pal <- colorNumeric(
        palette = palette_in,
        domain = domain_in
      )

      labels <- paste0(
        "Zip Code: ",
        char_zips$GEOID10, "<br/>",
        stat_type,
        ": ",
        char_zips$value) %>%
        lapply(htmltools::HTML)

      char_zips %>%
        leaflet %>%
        addProviderTiles("CartoDB.Voyager") %>%
        setView(lng = -118.24, lat=34.05, zoom = 10) %>%
        addPolygons(fillColor = ~pal(value_integer),
                    weight = 2,
                    opacity =0.15,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.5,
                    highlight = highlightOptions(weight = 2,
                                                 color = "#667",
                                                 dashArray = "",
                                                 fillOpacity = 0.7,
                                                 bringToFront = TRUE),label = labels) %>%

        addLegend(pal = pal,
                  values = ~value_integer,
                  opacity = 0.7,
                  labFormat = labelFormat(
                      transform = function(x) {if (stat_type == "Infection Rate"){exp(x) + 1} else {x}}
                  ),
                  title = htmltools::HTML(paste0(stat_type, "<br> \n by Zip Code")),
                  position = "bottomright")

    })
    
    #stat map: display stat info based on date, stat type and county selection
    output$stat_output = renderText({
      trend_filtered <- trend %>% filter(date == input$dateInput, type == input$stat_mode, ZIP == input$zip_search) %>% select(value)
      paste("On date ", input$dateInput," the ", input$stat_mode, " at ZIP ", input$zip_search, " is ", trend_filtered)
      # if (trend_filtered == numeric(0)){
      #   paste("Known.")
      # } else {
      #   paste(trend_filtered)
      # }
            })
    
    #raw data table: test purposes
    output$rawData = DT::renderDataTable({
      trend_infect <- trend %>% filter(type == "Infection Rate") %>% mutate(value_log = trunc(log1p(value)))
      trend_mobility <- trend %>% filter(type == "Mobility Index")
      
      # trend_date_pos <- trend_mobility %>% filter(value >= 0) %>% mutate(value_integer = trunc(log1p(value)))
      # trend_date_neg <- trend_mobility %>% filter(value < 0) %>% mutate(value_integer =  -1 * trunc(log1p(abs(value))))
      # trend_mobility <- rbind(trend_date_pos, trend_date_neg)
      trend_filtered <- trend %>% filter(date == as.Date("2020-05-01"), type == "Infection Rate", ZIP == "90001")
      
      trend_risk <- trend %>% filter(type == "Risk Score") %>% mutate(value_log = log1p(value))
      trend_filtered
    })

}

shinyApp(ui = ui, server = server)
