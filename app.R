library(shiny)
library(htmltools)
library(glue)
library(colourpicker)
library(shinyjqui)
library(shinyjs)
source("ring_module.R")
source("square_module.R")
source("line_module.R")

modules <- c("square", "ring", "line")
names(modules) <- tools::toTitleCase(modules)

modules
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Savage patterns"),
        sidebarPanel(width = c(4,8),
            selectInput("module", "Pattern", choices = modules, selected = modules[length(modules)]),
            do.call(tagList, lapply(modules, function(module) {
              conditionalPanel(
                condition = sprintf("input.module == '%s'", module),
                get(paste0(module, "_module_ui"))(paste0(module, "_module"))
              )
            })),
            actionButton("download", "Download"),
            div(downloadButton("download_h"), style = "visibility: hidden")
        ),
        mainPanel(
            uiOutput("svgout"),
            )
)

server <- function(input, output, session){
  
  patterns <- reactiveValues()

  # lapply(modules, function(module) {
  #   do.call(get(paste0(module, "_module_server")), args = list(id = module, patterns = patterns))
  # })
  #   
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

