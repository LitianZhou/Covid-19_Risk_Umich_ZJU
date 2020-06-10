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
library(tidyr)

# some new datasets, might be helpful
# when I show the .xlsx dataset, there are something wrong with the date, so I change the xlsx file into .csv file
# zipcode_daily_income <- read_excel("data/zipcode_daily_cases&social-distance&population&income.xlsx")
zipcode_daily_income <- read_csv("data/zipcode_daily_cases&social-distance&population&income.csv") 
risk_scores_table <- read_csv("risk_score/daily_predict_4_day_avg_risk_new.csv")


#data cleaning
#get combined table for the three trend plot variables 
infect_rate_by_zip <- zipcode_daily_income %>% transmute(value = confirmed_cases/population * 10000, date, ZIP, type = "Infection Rate")

risk_scores <- risk_scores_table %>%  separate(`date_start - date_end`, c("date","discard"), sep = ' - ') %>% transmute(value = `cases/population * 10000`, date, ZIP, type = "Risk Score")
mobility_indices <- zipcode_daily_income %>% transmute(ZIP, date, type = "Mobility Index", value = (median_home_dwell_time - median_non_home_dwell_time) * distance_traveled_from_home / 600000) 
joined_df <- rbind(infect_rate_by_zip, risk_scores)
trend <- rbind(joined_df, mobility_indices)
#trend <- rbind(trend, risk_scores_0_100)

#convert the date column to date object and arrange by date order
trend$date <- ymd(trend$date)
arrange(trend, date)

#round all values to 3 decimal digits
trend$value <- round(trend$value, digits = 3)

#create result data table
infect_result <- infect_rate_by_zip %>% select(-type) %>% rename(Infection_Rate = value)
risk_result <- risk_scores %>% select(-type) %>% rename(Risk_Score = value)
mobility_result <- mobility_indices %>% select(-type) %>% rename(Mobility_Index = value)
temp <- merge(infect_result, mobility_result, all=TRUE)
result_data_table <- merge(temp, risk_result, all=TRUE)


# char_zips <- zctas(cb = TRUE, starts_with = c("90","91","92"))
# saveRDS(char_zips, "char_zips.rds")
# Our reference
# 1. London map:https://www.doorda.com/covid-19-data-free-download/#1589463968147-d5954274-124c
# 2. SuperZip map:https://shiny.rstudio.com/gallery/superzip-example.html

ui <- fluidPage(
    includeCSS("styles.css"),
    titlePanel(
      h1("COVID-19 Risk Scoring in Los Angeles County")
      ),
    tabsetPanel(
        tabPanel("Map by Statistics",
                 HTML("<button type='button' class='btn-danger' data-toggle='collapse' data-target='#demo'>Selector</button>"),
                 leafletOutput("map_stat",width = "100%", height = 700),
                 div(id = "demo", class = "collapse", 
                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   draggable = TRUE, top = "170", left = "100", 

                                   right = "auto", bottom = "auto",
                                   width = "330", height = "400",
                         sliderInput(inputId = "dateInput", 
                                     label = "Select Date: ",
                                     min = as.Date("2020-03-16","%Y-%m-%d"),
                                     max = as.Date("2020-05-27","%Y-%m-%d"),
                                     value = as.Date("2020-04-29"), timeFormat="%Y-%m-%d", 
                                     step = 1,
                                     animate = animationOptions(interval =500)),
                         selectInput("stat_mode",
                                     "Select Statistics",
                                     c( "Infection Rate","Mobility Index", "Risk Score"),
                                     selected = "Infection Rate"),
                         selectInput("zip_search",
                                     "Select Zip Location",
                                     unique(risk_scores_table$ZIP),
                                     multiple = F),
                         h5("Search Result: "),
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
                 ),
                 mainPanel(
                   plotly::plotlyOutput("trend_plot_by_county", height = 800)
                 )
        ),
        # a new kind of tab to show
        tabPanel("Raw Data Table",
                 # controls to select a data set and spcify the number of observations to view
                 sidebarPanel(
                   selectInput("dataset","Choose a dataset:",
                               choices = c("Data Table Statistics", "Risk Score Prediction")
                               ),
                   numericInput("obs", "Number of observations to view:", 10)
                 ),
                 mainPanel(
                   tableOutput("view")
                 )
        ),
        tabPanel("Result Data Table",
                 sidebarPanel(
                 numericInput("num", "Number of observations to view:", 10),
                 HTML("<button type='button' class='btn btn-danger' data-toggle='collapse' data-target='#explain'>Statistics Details</button>"),
                 div(id = "explain", class = "collapse", 
                     h5("Infection Rate: "),
                     "total confirmed cases /population * 10,000 (number of total confirmed cases per 10,000 population at the zip code level updated daily)",
                     h5("Risk Score: "),
                     " LSTM model outcome: prediction of average number of new confirmed cases per 10,000 population in a four days window (starting at the specified date) based on data of new confirmed cases in a six days window preceding the specified date",
                     h5("Mobility Index: "),
                     "(median_home_dwell_time - median_non_home_dwell_time) * distance_traveled_from_home / 600,000"
                 )
                 ),
                 mainPanel(
                   tableOutput("result_data")
                 )
                 
                 )
    )
)

server <- function(input, output) {
    options(tigris_use_cache = TRUE)

    # new raw_data table
    #places_totals2 <- places_totals %>% select(date:new_confirmed_cases)
    # Return the requested dataset
    datasetInput <- reactive({
      switch(input$dataset,
             "Data Table Statistics" = zipcode_daily_income,
             "Risk Score Prediction" = risk_scores_table
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
    
    
    output$result_data <- renderTable({
      head(result_data_table, n = input$num)
    })
    
    
    #trend plot: return trend plot by zipcode entered
    output$trend_plot_by_county = renderPlotly({
      trend_input <- trend %>% filter(ZIP == input$zipID)
      ggplotly(
          ggplot2::ggplot(data = trend_input, aes(x=date, y=value, color=type)) +
          geom_line() +
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
      } else { #Risk Score
        trend_date <- trend_date %>% mutate(value_integer = trunc(value))
        domain_in = c(-20:7)
        rc1 <- colorRampPalette(colors = c("green", "white"), space = "Lab", bias = 1)(20)
        rc2 <- colorRampPalette(colors = c("white", "red"), space = "Lab", bias = 1)(7)
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
                  #values = ~(min(value_integer):max(value_integer)),
                  values = ~(value_integer),
                  opacity = 0.7,
                  labFormat = labelFormat(
                      transform = function(x) {if (stat_type == "Infection Rate"){exp(x) + 1} else {x}}
                  ),
                  title = htmltools::HTML(paste0(stat_type, "<br> \n by Zip Code")),
                  position = "bottomleft")

    })
    
    #stat map: display stat info based on date, stat type and county selection
    output$stat_output = renderText({
      trend_filtered <- trend %>% filter(date == input$dateInput, type == input$stat_mode, ZIP == input$zip_search) %>% select(value)
      paste("On date ", input$dateInput," the ", input$stat_mode, " at ZIP ", input$zip_search, " is ", trend_filtered)
      })

}

shinyApp(ui = ui, server = server)
