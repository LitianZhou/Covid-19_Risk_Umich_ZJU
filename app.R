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
zipcode_daily <- read.csv("data/zipcode_daily_with_1_11_future_day_case_count.csv", header=T)

# some new datasets, might be helpful
zipcode_daily_income <- read_csv("data/zipcode_daily_cases&social-distance&population&income.xlsx")
places_totals <- read_csv("data/latimes-place-totals.csv")

# char_zips <- zctas(cb = TRUE, starts_with = c("90","91","92"))
# saveRDS(char_zips, "char_zips.rds")
# Our reference
# 1. London map:https://www.doorda.com/covid-19-data-free-download/#1589463968147-d5954274-124c
# 2. SuperZip map:https://shiny.rstudio.com/gallery/superzip-example.html

ui <- fluidPage(
    titlePanel("Covid-19 Risk Umich + ZJU"),
    tabsetPanel(
        tabPanel("Interactive map",
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
        tabPanel("Raw data table",
                 DT::dataTableOutput("rawData")
        ),
        # a new kind of tab to show
        tabPanel("Raw data table(new)",
                 # controls to select a data set and spcify the number of observations to view
                 sidebarPanel(
                   selectInput("dataset","Choose a dataset:",
                               choices = c("zipcode_daily","place_totals")),
                   numericInput("obs", "Number of observations to view:", 10)
                 ),
                 mainPanel(
                   # summary of the dataset
                   # verbatimTextOutput("summary"),
                   tableOutput("view")
                 )
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
        char_zips %>%
            filter(GEOID10 %in% zipcode_daily$ZIP)
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
    
    # choose columns to display
    zipcode_daily2 <- zipcode_daily %>% select(ZIP:places)
    
    output$rawData = DT::renderDataTable({
        zipcode_daily2
    })
    
    
    
    # new raw_data table
    places_totals2 <- places_totals %>% select(date:new_confirmed_cases)
    # Return the requested dataset
    datasetInput <- reactive({
      switch(input$dataset,
             "zipcode_daily" = zipcode_daily_income,
             "place_totals" = places_totals2,
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
    
}

shinyApp(ui = ui, server = server)
