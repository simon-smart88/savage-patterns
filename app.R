library(shiny)
library(htmltools)
library(glue)
library(colourpicker)
library(shinyjqui)
source("gradientInput.R")
source("ring_module.R")

ui <- fluidPage(
  #title = "Savage patterns",
  titlePanel("Savage patterns"),
        sidebarPanel(width = c(4,8),
            selectInput("module", "Pattern", choices = c("Ring" = "ring", "Square" = "square")),
            conditionalPanel("input.module == 'ring'", ring_module_ui("ring_module")),
            downloadButton("download")
        ),
        mainPanel(
            uiOutput("svgout")
            )
)

server <- function(input, output, session){

  patterns <- reactiveValues()

  ring_module_server("ring_module", patterns)

  #send to UI
  output$svgout <- renderUI({
    patterns[[input$module]]
  })

  output$download <- downloadHandler(
    filename = function(){
      "your.svg"
    },
    content = function(file){

      write(as.character(patterns[[input$module]]), file)
    }
  )

}



shinyApp(ui, server)
