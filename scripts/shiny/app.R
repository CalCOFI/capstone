library(shiny)
library(shinydashboard)
library(leaflet)
library(sp)
library(htmltools)
library(tidyverse)
library(scales)
library(lubridate)
load('spatial-page/page_functions.RData') # for deployment
# load('scripts/shiny/spatial-page/page_functions.RData') # for development

## --------------
## SPATIAL PAGE
ui <- navbarPage("CalCOFI", id="nav",
              tabPanel("Interactive map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("style.css"),
                          includeScript("gomap.js")
                        ),
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map", width="100%", height="100%"),
                        
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 700, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Inputs"),
                                      numericInput('yr', # selection gets stored as `input$yr`
                                                   'Year', 
                                                   min = min(year(bottle$date)),
                                                   max = max(year(bottle$date)),
                                                   value = median(year(bottle$date)),
                                                   step = 1),
                                      numericInput('qr', # selection gets stored as `input$qr`
                                                   'Quarter',
                                                   min = 1,
                                                   max = 4,
                                                   step = 1,
                                                   value = 1),
                                      selectInput('lin',
                                                  'Line ID',
                                                  lines,
                                      ),

                        ),
                        column(4,
                               absolutePanel(id = "controls",class = "panel panel-default",
                                             fixed = TRUE,
                                             draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                             width = 500, height = "auto", 
                                             h2('Plots'),
                                             plotOutput("profile"),
                                             plotOutput('stationline')
                               )
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('CalCOFI'), ' by Us'
                        )
                    ),
           ),
           
           tabPanel("Something else",
                    fluidRow(h2("Something else"))
           ),
           conditionalPanel("false", icon("crosshair")),
)
# # user input panel for spatial tab
# spatial_sidebar <- sidebarPanel(
#     numericInput('yr', # selection gets stored as `input$yr`
#                  'Year', 
#                  min = min(year(bottle$date)),
#                  max = max(year(bottle$date)),
#                  value = median(year(bottle$date)),
#                  step = 1),
#     numericInput('qr', # selection gets stored as `input$qr`
#                  'Quarter',
#                  min = 1,
#                  max = 4,
#                  step = 1,
#                  value = 1),
#     width = 12)
# 
# map_comments <- 'Note: point size shows location variability across all sampling events, but not to scale.'
# 
# # define layout for body of spatial tab
# spatial_tab <- tabItem(tabName = 'spatial',
#                        fluidRow(
#                            # user selections
#                            box(spatial_sidebar,
#                                title = 'Select time',
#                                status = 'info',
#                                solidHeader = T,
#                                # collapsible = T,
#                                width = 2),
#                            # map
#                            box(map_comments,
#                                leafletOutput('map', 'width=100%', 'height=100%'),
#                                title = 'Sampling locations',
#                                status = 'primary',
#                                solidHeader = T,),
#                            # depth profile
#                            tabBox(
#                                tabPanel("Profile",
#                                plotOutput('profile'),
#                                title = 'Depth profiles',
#                                status = 'primary',
#                                solidHeader = T,
#                                # collapsible = T,
#                                width = 4),
#                                tabPanel("Intro",
#                                         solidHeader = T,
#                                         # collapsible = T,
#                                         width = 4),
#                                width = 4)
#                        )
# )

## ------------------------
## SOME OTHER PAGE
# 
# blank_tab <- tabItem(tabName = 'somethingelse',
#                      h2('Something completely different')) 
# 
# ## ------------------------
# ## USER INTERFACE LAYOUT
# 
# # menu to navigate tabs (note tabName must match tabItems)
# sidebar <- sidebarPanel(sidebarMenu(
#     menuItem('Spatial variation', tabName = 'spatial'),
#     menuItem('Another tab', tabName = 'somethingelse')),
#     width = 1)
# 
# # body for each tab
# body <- mainPanel(
#     tabItems(
#         spatial_tab,
#         blank_tab
#     )
# )
# 
# # define user interface
# ui <- fluidPage(
#     titlePanel("Draft"),
#     sidebar,
#     body,
# )


## ------------------
## OUTPUTS
server <- function(input, output, session) {
    
    ## MAP PANEL
    # user retrieve data by year
    map_data <- reactive({get_map_data(input$yr, input$qr)})
    
    # base map layer (will show default year 2000)
    output$map <- renderLeaflet({make_basemap()})
    
    # plot user's selection
    observe({
        leafletProxy('map') %>%
            update_basemap(map_data())
    })
    
    ## PROFILE PANEL
    # user retrieve data by year/quarter
    profile_plot <- reactive({
        make_profile(input$yr, input$qr)
    })
    profile_plot <- reactive({
      make_station_line(input$yr, input$lin)
    })
    
    # plot depth profiles
    output$profile <- renderPlot({profile_plot()})
    output$stationline <- renderPlot({station_line_plot})
    
} 

## ---------------
## DEPLOY

shinyApp(ui,server)