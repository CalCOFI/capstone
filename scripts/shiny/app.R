if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  here, htmltools, leaflet, lubridate, scales, shiny, shinydashboard, sp, tidyverse)

fxns_r <- here("scripts/shiny/spatial-page/page_functions.R")
bottle_rda <- here("data/processed/bottle.RData")
stopifnot(file.exists(fxns_r))
source(fxns_r)
load(bottle_rda)

# UI ----
#* Spatial tab ----
ui <- navbarPage(
  "CalCOFI", id="nav",
  tabPanel(
    "Home",
    h1("CalCOFI App"),
  ),
  tabPanel(
    "Spatial Trends",
    div(
      class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("style.css"),
        includeScript("gomap.js"),),
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
          'Transect (Line ID)',
          lines,),
        selectInput(
          'sta',
          'Station ID',
          stations,),
        h4("Select Parameters"),
        radioButtons('param', 'Parameters', 
                     choices = 
                       c("Oxygen" = "oxy",
                       "Temperature" = "temp",
                       "Salinity" = "sal",
                       "Chlorophyll" = "chlorophyll"),
                     selected = "oxy",),
      ),
      
      column(
        4,
        absolutePanel(
          id = "controls",class = "panel panel-default",
          fixed = TRUE,
          draggable = FALSE, top = 50, left = "auto", right = 0, bottom = "auto",
          width = 600, height = 10000, 
          h2('Profiles'),
          tabsetPanel(
            tabPanel(
              title = 'Depth profiles',
              width = "100%",
              height = "100%",
              status = 'primary',
              solidHeader = T,
              plotOutput("profile", width = "100%", height = "800px",),
              downloadButton(outputId = "prof_down", label = "Download the plot")),
            tabPanel(
              title = "Transect Profile",
              width = "100%",
              height = "200%",
              plotOutput('stationline', width = "100%", height = "800px",),
              downloadButton(outputId = "sta_down", label = "Download the plot")),
          ),
        )),
      tags$div(
        id="cite",
        'Data compiled for ', tags$em('CalCOFI'), ' by Us')),
    actionButton("show", label = NULL, icon = icon("info"))
  ),
#* temporal tab ----  
  tabPanel(
    "Temporal Trends",
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
        dateRangeInput(
          "animation",
          "Date Range",
          start = "1990-06-14",
          end = "2010-01-26",
          min = as.Date(as.Date(min((bottle$date), na.rm = T), "%Y-%m-%d"), "%Y-%m-%d"),
          max = as.Date(as.Date(max((bottle$date), na.rm = T), "%Y-%m-%d"), "%Y-%m-%d"),
          format = "yyyy-mm-dd",
          startview = "month",
          weekstart = 0,
          language = "en",
          separator = " to ",
          width = NULL,
          autoclose = TRUE
        ),
        sliderInput(
          "times",
          "Press Play to Animate",
          min = as.Date(as.Date(min((bottle$date), na.rm = T), "%Y-%m-%d"), "%Y-%m-%d"),
          max = as.Date(as.Date(max((bottle$date), na.rm = T), "%Y-%m-%d"), "%Y-%m-%d"),
          value = as.Date(as.Date(min((bottle$date), na.rm = T), "%Y-%m-%d"), "%Y-%m-%d"),
          animate = animationOptions(interval = 400, loop = TRUE),
          step = 30,
          timeFormat = "%b %Y",
        ),
        selectInput(
          'qr2',
          'Quarter',
          1:4,
          selected = 1),
        numericInput(
          'yr2', # selection gets stored as `input$yr`
          'Year', 
          min = min(year(bottle$date), na.rm = T),
          max = max(year(bottle$date), na.rm = T),
          value = 1992,
          step = 1),
        selectInput(
          'lin2',
          'Line ID',
          lines,),
        selectInput(
          'sta2',
          'Station ID',
          stations,),
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

# SERVER ----
server <- function(input, output, session) {
  
  # map ----
  # user retrieve data by year
  map_data <- reactive({get_map_data(input$yr, input$qr)})
  map_data2 <- reactive({get_map_data(input$yr2, input$qr2)})
  
  # base map layer (will show default year 2000)
  output$map1 <- renderLeaflet({make_basemap()})
  output$map2 <- renderLeaflet({make_basemap()})
  
  # * update_basemap() ----
  # plot user's selection
  observe({
    input$nav
    
    tab1 <- leafletProxy('map1') %>%
      update_basemap(map_data())
    tab2 <- leafletProxy('map2') %>%
      update_basemap(map_data2())
  })
  
  # * map1_marker_click ----
  # When marker is clicked, update Station ID selector
  observe({
    event <- input$map1_marker_click
    if (is.null(event))
      return()
    updateSelectInput(session, "sta", selected=strsplit(event$id," ")[[1]][[2]])
  })
  observe({
    event <- input$map1_marker_click
    if (is.null(event))
      return()
    updateSelectInput(session, "lin", selected=strsplit(event$id," ")[[1]][[1]])
  })
  observe({
    event <- input$map2_marker_click
    if (is.null(event))
      return()
    updateSelectInput(session, "sta2", selected=strsplit(event$id," ")[[1]][[2]])
  })
  observe({
    event <- input$map2_marker_click
    if (is.null(event))
      return()
    updateSelectInput(session, "lin2", selected=strsplit(event$id," ")[[1]][[1]])
  })
  observe({
   val <- input$animation
   if (is.null(val))
     return()
   updateSliderInput(session, "times", value = as.Date(val[1], "%Y-%m-%d"),
                     min = as.Date(val[1],"%Y-%m-%d"), max = as.Date(val[2], "%Y-%m-%d"), 
                     timeFormat = "%Y-%m-%d")
  })
  observe({
    quarter_val <- tibble(
      date = format(input$times, "%Y-%m-%d") %>% as.Date()) %>% 
      mutate(quarter = lubridate::quarter(date))
    updateSelectInput(session, "qr2", selected = as.character(quarter_val))
  })
  observe({
    year_val <- format(input$times, "%Y")
    updateNumericInput(session, "yr2", value = year_val)
  })
  
  ## PROFILE PANEL
  # user retrieve data by year/quarter
  profile_plot <- reactive({
    make_profile(input$yr, input$lin)
  })
  
  station_line_plot <- reactive({
    if(input$param == "oxy"){
      make_station_line(input$yr, input$lin)
    }else{
      if(input$param == "temp"){
        make_station_line_temp(input$yr, input$lin)
      }else{
        if(input$param == "sal"){
          make_station_line_salinity(input$yr, input$lin)
        }
        else{
          make_station_line_chlor(input$yr, input$lin)
        }
      }
    }
  })
  #---- Intro to CalCOFI modal
  output[["image"]] <- renderImage({
    list(src = "ims/calcofi_header.png",
         alt = "This is alternate text",
         width = "100%",
         height = "75px")}, deleteFile = FALSE)
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "How to use this app",
      imageOutput("image", height = "100px"),
      HTML(
      "This app is a data visualization tool that explores the following
      properties of the seawater off the coast of California: dissolved oxygen concentration, temperature, salinity, and chlorophyll. <br>
      The visualizations use data collected by the California Cooperative Fisheries and Oceanic Investigations (CalCOFI). 
      Data is collected by collecting seawater at numerous discrete depths (between 0 - 500+ meters) at" ),
      tags$a("sampling station", href = "https://calcofi.org/sampling-info/station-positions/"),
      HTML("locations that are arranged along parallel “station lines” extending perpendicular off the coast. CalCOFI samples these stations 
      quarterly — in Winter, Spring, Summer, and Fall of each year — and has been doing so for over 50 years."),
      #size = "l",
      easyClose = FALSE,
    ),
    )
  })
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$sta_down <- downloadHandler(
    filename =  function() {
      paste("station-line", input$yr, input$qr, input$sta, input$lin, sep="_")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
    png(file) # open the png device
      if(input$param == "oxy"){
        make_station_line(input$yr, input$lin)
      }else{
        if(input$param == "temp"){
          make_station_line_temp(input$yr, input$lin)
        }else{
          if(input$param == "sal"){
            make_station_line_salinity(input$yr, input$lin)
          }
          else{
            make_station_line_chlor(input$yr, input$lin)
          }
        }
      }
    dev.off()  # turn the device off
      
    })
  output$prof_down <- downloadHandler(
    filename =  function() {
      paste("profile-plot", input$yr, input$qr, input$sta, input$lin, sep="_")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file){
      png(file) # open the png device
      make_profile(input$yr, input$lin)
      dev.off()  # turn the device off
      
    })
  
  # plot depth profiles
  output$profile <- renderPlot({profile_plot()})
  output$stationline <- renderPlot({station_line_plot()})
  
} 

## ---------------
## DEPLOY

shinyApp(ui,server)