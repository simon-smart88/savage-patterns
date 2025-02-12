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
            get(glue("{module}_module_ui"))(module)
          )})
        ),
        actionButton("download", "Download", icon = icon("download"), width = "100%", style = "font-size: 1.5rem;"),
        div(downloadButton("download_h"), style = "visibility: hidden"),
        width = "400px",
      ),
      div(class = "svg_container", uiOutput("svgout"))
    )),

  nav_panel("Gallery",
    layout_columns(
      col_widths = breakpoints(
        sm = c(12),
        md = c(-2, 8, -2),
        lg = c(-3, 6, -3)
      ),
      gallery_module_ui("gallery"),
    )
  ),
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
  title = "Savage patterns",
  tags$head(tags$link(href = "styles.css", rel = "stylesheet"))
)

server <- function(input, output, session){

  patterns <- reactiveValues()

  lapply(modules, function(module) {
    do.call(get(paste0(module, "_module_server")),
            args = list(module, patterns, reactive(input$module)))
  })

  gallery_module_server("gallery")

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

