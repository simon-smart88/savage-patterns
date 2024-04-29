library(shiny)
library(htmltools)
library(glue)
library(colourpicker)
library(shinyjqui)
library(shinyjs)
source("gradientInput.R")
source("ring_module.R")
source("square_module.R")

jsCode <- "shinyjs.svgprint = function(){
document.getElementsByTagName('svg');
}"


ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsCode, functions = c("svgprint")),
  #title = "Savage patterns",
  titlePanel("Savage patterns"),
        sidebarPanel(width = c(4,8),
            selectInput("module", "Pattern", choices = c("Square" = "square", "Ring" = "ring"), selected = "ring"),
            conditionalPanel("input.module == 'ring'", ring_module_ui("ring_module")),
            conditionalPanel("input.module == 'square'", square_module_ui("square_module")),
            downloadButton("download"),
            actionButton("get_html", "Get html")
        ),
        mainPanel(
            uiOutput("svgout"),
            )
)

server <- function(input, output, session){

  patterns <- reactiveValues()

  ring_module_server("ring_module", patterns)
  square_module_server("square_module", patterns)

  #send to UI
  output$svgout <- renderUI({
    patterns[[input$module]]
  })


  observeEvent(input$get_html, {
    runjs("
      var elements = document.getElementsByTagName('svg')[0];
      var svgHTML = new XMLSerializer().serializeToString(elements);
      Shiny.setInputValue('svg', svgHTML, {priority: 'event', raw: true});
    ")
  })

  output$download <- downloadHandler(
    filename = function(){
      "your_test.svg"
    },
    content = function(file){
      #write(as.character(patterns[[input$module]]), file)
      write(input$svg, file)
    }
  )

}



shinyApp(ui, server)

