if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  here, htmltools, leaflet, lubridate, scales, shiny, shinydashboard, sp, tidyverse)

fxns_r <- here("scripts/shiny/spatial-page/page_functions.R")
stopifnot(file.exists(fxns_r))
source(fxns_r)

## --------------
## SPATIAL PAGE
ui <- navbarPage(
  "CalCOFI", id="nav",
  tabPanel(
    "Interactive map",
    div(
      class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("style.css"),
        includeScript("gomap.js")),
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map1", width="100%", height="100%"),
      
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 700, bottom = "auto",
        width = 330, height = "auto",
        
        h2("Inputs"),
        numericInput(
          'yr', # selection gets stored as `input$yr`
          'Year', 
          min = min(year(bottle$date), na.rm = T),
          max = max(year(bottle$date), na.rm = T),
          value = median(year(bottle$date), na.rm = T),
          step = 1),
        numericInput(
          'qr', # selection gets stored as `input$qr`
          'Quarter',
          min = 1,
          max = 4,
          step = 1,
          value = 1),
        selectInput(
          'lin',
          'Line ID',
          lines,),
      ),
      column(
        4,
        absolutePanel(
          id = "controls",class = "panel panel-default",
          fixed = TRUE,
          draggable = FALSE, top = 50, left = "auto", right = 0, bottom = "auto",
          width = 600, height = 1000000, 
          h2('Plots'),
          tabsetPanel(
            tabPanel(
              title = 'Depth profiles',
              width = "100%",
              height = "100%",
              status = 'primary',
              solidHeader = T,
              plotOutput("profile")),
            tabPanel(
              title = "Station Line Profiles",
              width = "100%",
              height = "200%",
              plotOutput('stationline')),
            textOutput("quarter"),
          ),
        )),
      tags$div(
        id="cite",
        'Data compiled for ', tags$em('CalCOFI'), ' by Us')),
    ),
  
  tabPanel(
    "Temporal Tab",
    div(
      class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("style.css"),
        includeScript("gomap.js")),
      leafletOutput("map2", width="100%", height="100%"),
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 700, bottom = "auto",
        width = 330, height = "auto",
        
        h2("Inputs"),
        sliderInput(
          "animation", label = "Time Range", 
          min = min(year(bottle$date), na.rm = T), 
          max = max(year(bottle$date), na.rm = T), 
          value = c(2011,2013),
          step = 0.25,
          sep = "",
        ),
        verbatimTextOutput("range"),
        numericInput(
          'station', # selection gets stored as `input$yr`
          'Station ID', 
          min = 0,
          max = 500,
          value = 110.0,
        ),
        selectInput(
          'lin',
          'Line ID',
          lines,
        ),
        h4("Select Parameters"),
        checkboxInput('oxy', 'Oxygen', value = FALSE, width = NULL),
        checkboxInput('temp', 'Temperature', value = FALSE, width = NULL),
        checkboxInput('ph', 'pH', value = FALSE, width = NULL),
        
      ),
      column(
        4,
        absolutePanel(
          id = "controls",class = "panel panel-default",
          fixed = TRUE,
          draggable = FALSE, top = 50, left = "auto", right = 0, bottom = "auto",
          width = 600, height = 1000000, 
          h2('Plots'),
          tabsetPanel(
            tabPanel(
              title = 'Time Series Plots',
              width = "100%",
              height = "100%",
              status = 'primary',
              solidHeader = T,
            ),
            tabPanel(
              title = "Depth Average Plots",
              width = "100%",
              height = "100%",),),
        )
      ),
    ),
  ),
)


## ------------------
## OUTPUTS
server <- function(input, output, session) {
  
  ## MAP PANEL
  # user retrieve data by year
  map_data <- reactive({get_map_data(input$yr, input$qr)})
  
  # base map layer (will show default year 2000)
  output$map1 <- renderLeaflet({make_basemap()})
  output$map2 <- renderLeaflet({make_basemap()})
  
  # plot user's selection
  observe({
    input$nav
    
    tab1 <- leafletProxy('map1') %>%
      update_basemap(map_data())
    tab2 <- leafletProxy('map2') %>%
      update_basemap(map_data())
  }
  )
  
  
  ## PROFILE PANEL
  # user retrieve data by year/quarter
  profile_plot <- reactive({
    make_profile(input$yr, input$qr)
  })
  station_line_plot <- reactive({
    make_station_line(input$yr, input$lin)
  })
  
  # plot depth profiles
  output$profile <- renderPlot({profile_plot()})
  output$stationline <- renderPlot({station_line_plot()})
  output$range <- renderPrint({input$animation })
  #output$quarter <- renderText({input$qr})
  
} 

## ---------------
## DEPLOY

shinyApp(ui,server)