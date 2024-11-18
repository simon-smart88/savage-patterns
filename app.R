library(shiny)
library(htmltools)
library(glue)
library(colourpicker)
library(shinyjqui)
library(shinyjs)
source("ring_module.R")
source("square_module.R")
source("line_module.R")

jsCode <- "shinyjs.svgprint = function(){
document.getElementsByTagName('svg');
}"

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsCode, functions = c("svgprint")),
  #title = "Savage patterns",
  titlePanel("Savage patterns"),
        sidebarPanel(width = c(4,8),
            selectInput("module", "Pattern", choices = c("Square" = "square", "Ring" = "ring", "Line" = "line"), selected = "line"),
            conditionalPanel("input.module == 'ring'", ring_module_ui("ring_module")),
            conditionalPanel("input.module == 'square'", square_module_ui("square_module")),
            conditionalPanel("input.module == 'line'", line_module_ui("line_module")),
            actionButton("download", "Download"),
            div(downloadButton("download_h"), style = "visibility: hidden")
        ),
        mainPanel(
            uiOutput("svgout"),
            )
)

server <- function(input, output, session){

  patterns <- reactiveValues()

  ring_module_server("ring_module", patterns)
  square_module_server("square_module", patterns)
  line_module_server("line_module", patterns)
  
  #send to UI
  output$svgout <- renderUI({
    patterns[[input$module]]
  })

  observeEvent(input$download, {
    runjs("
      var elements = document.getElementsByTagName('svg')[0];
      var svgHTML = new XMLSerializer().serializeToString(elements);
      Shiny.setInputValue('svg', svgHTML, {priority: 'event', raw: true});
      document.getElementById('download_h').click();
    ")
  })


  output$download_h <- downloadHandler(
    filename = function(){
      "your_test3.svg"
    },
    content = function(file){
      req(input$svg)
      write(input$svg, file)
    }
  )

}



shinyApp(ui, server)

