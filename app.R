library(shiny)
library(bslib)
library(shinyWidgets)
library(htmltools)
library(glue)
library(shinyjs)

modules <- gsub("_module.R", "", list.files("R/", "*module*"))
names(modules) <- paste0("<img src='", modules, "_icon.svg'>")

ui <- page_navbar(
  nav_panel("Create",
    shinyjs::useShinyjs(),
    layout_sidebar(
      sidebar = sidebar(

        # buttons for selecting module
        radioGroupButtons("module", "Choose a pattern",
                          choices = modules, justified = TRUE,
                          size = "lg", status = "info"),

        # call all the module UI inside conditional panels
        do.call(tagList, lapply(modules, function(module) {
          conditionalPanel(
            condition = glue("input.module == '{module}'"),
            get(glue("{module}_module_ui"))(glue("{module}_module"))
          )})
        ),
        actionButton("download", "Download", icon = icon("download"), width = "100%", style = "font-size: 1.5rem;"),
        div(downloadButton("download_h"), style = "visibility: hidden"),
        width = "400px",
      ),
      div(style = "overflow:hidden", uiOutput("svgout"), height="90vh", width = "100%")
    )),
  nav_panel("About",
    layout_columns(
      col_widths = breakpoints(
        sm = c(12),
        md = c(-2, 8, -2),
        lg = c(-3, 6, -3)
      ),
      about_module_ui("about"),
    )
  ),
  theme = bs_theme(version = 5, "simplex",
                   primary = "#e4401b",
                   info = "#eae5e5"),
  title = "Savage patterns"
)

server <- function(input, output, session){

  patterns <- reactiveValues()

  # lapply(modules, function(module) {
  #   do.call(get(paste0(module, "_module_server")), args = list(id = module, patterns = patterns))
  # })

  ring_module_server("ring_module", patterns, reactive(input$module))
  square_module_server("square_module", patterns, reactive(input$module))
  line_module_server("line_module", patterns, reactive(input$module))

  # send to UI
  output$svgout <- renderUI({
    patterns[[input$module]]
  })

  # store the current pattern and then click the actual download button
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
      glue("{input$module}_{substr(Sys.time(), 1, 19)}.svg")
    },
    content = function(file){
      req(input$svg)
      write(input$svg, file)
    }
  )

}

shinyApp(ui, server)

