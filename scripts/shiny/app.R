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

# user input panel for spatial tab
spatial_sidebar <- sidebarPanel(
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
    width = 12)

map_comments <- 'Note: point size shows location variability across all sampling events, but not to scale.'

# define layout for body of spatial tab
spatial_tab <- tabItem(tabName = 'spatial',
                       fluidRow(
                           # user selections
                           box(spatial_sidebar,
                               title = 'Select time',
                               status = 'info',
                               solidHeader = T,
                               collapsible = T,
                               width = 3),
                           # map
                           box(map_comments,
                               leafletOutput('map'),
                               title = 'Sampling locations',
                               status = 'primary',
                               solidHeader = T,
                               collapsible = T,
                               width = 4),
                           # depth profile
                           box(plotOutput('profile'),
                               title = 'Depth profiles',
                               status = 'primary',
                               solidHeader = T,
                               collapsible = T,
                               width = 5)
                       )
)

## ------------------------
## SOME OTHER PAGE

blank_tab <- tabItem(tabName = 'somethingelse',
                     h2('Something completely different')) 

## ------------------------
## USER INTERFACE LAYOUT

# menu to navigate tabs (note tabName must match tabItems)
sidebar <- dashboardSidebar(sidebarMenu(
    menuItem('Spatial variation', tabName = 'spatial'),
    menuItem('Another tab', tabName = 'somethingelse'))
)

# body for each tab
body <- dashboardBody(
    tabItems(
        spatial_tab,
        blank_tab
    )
)

# define user interface
ui <- dashboardPage(
    dashboardHeader(title = 'Draft'),
    sidebar,
    body
)


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
    
    # plot depth profiles
    output$profile <- renderPlot({profile_plot()})
    
} 

## ---------------
## DEPLOY

shinyApp(ui,server)