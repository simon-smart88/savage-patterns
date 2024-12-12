library(shiny)
library(bslib)
library(shinyWidgets)
library(htmltools)
library(glue)
library(colourpicker)
library(shinyjqui)
library(shinyjs)
source("ring_module.R")
source("square_module.R")
source("line_module.R")

modules <- c("square", "ring", "line")
names(modules) <- paste0("<img src='", modules, "_icon.svg'>")

ui <- page_sidebar(
  shinyjs::useShinyjs(),
  title = "Savage patterns",
  sidebar = sidebar(
    shinyWidgets::radioGroupButtons("module", "Pattern", choices = modules, justified = TRUE, size = "lg"),
    do.call(tagList, lapply(modules, function(module) {
      conditionalPanel(
        condition = glue("input.module == '{module}'"),
        get(glue("{module}_module_ui"))(glue("{module}_module"))
      )})
    ),
    actionButton("download", "Download", icon = icon("download"), width = "100%", style = "font-size: 1.5rem;"),
    div(downloadButton("download_h"), style = "visibility: hidden"),
    width = "35%",
  ),
  uiOutput("svgout")
)

server <- function(input, output, session){

  patterns <- reactiveValues()

  # lapply(modules, function(module) {
  #   do.call(get(paste0(module, "_module_server")), args = list(id = module, patterns = patterns))
  # })

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

