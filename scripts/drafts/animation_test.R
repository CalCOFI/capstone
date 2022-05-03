library(shiny)
ui <- fluidPage(
  # Application title
  titlePanel("Grad.desc demo"),
  # Sidebar with a slider input for the number of frames
  sidebarLayout(
    sidebarPanel(
      sliderInput('myslider', 
                  'Steps', 
                  min=1, 
                  max=35, # count all frames(pictures) 
                  value=c(1,5), 
                  animate=animationOptions(interval = 200,loop=T)
      ),
      verbatimTextOutput("range"),
    ),  
    # Show ui
    mainPanel(
      uiOutput("ui")
    )
  )
)
server <- function(input, output, session) {
  imgurl <- reactive({
    i=input$myslider
    return(paste0("./images/grad.desc",i,".png")) #the path of pictures
  })
  output$range <- renderPrint({input$myslider})
  output$ui <- renderUI({
    tags$div(
      tags$img(src = imgurl())
    )
  })
}

shinyApp(ui,server)