gallery_module_ui <- function(id){
  ns = NS(id)
  tagList(
    h3(style = "text-align: center;", "Here you can find some examples of patterns I've created"),
    fluidRow(
      column(width = 6, actionButton(ns("previous"), "Previous", width = "100%")),
      column(width = 6, actionButton(ns("next_one"), "Next", width = "100%")),
    ),
    div(class = "svg_container", uiOutput(ns("gallery")))
  )
}

gallery_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    files <- sample(list.files("www/gallery/"))

    index <- reactiveVal(1)

    observeEvent(input$previous, {
      if (index() > 1){
        index(index() - 1)
      } else {
        index(length(files))
      }
    })

    observeEvent(input$next_one, {
      if (index() > length(files)){
        index(1)
      } else {
        index(index() + 1)
      }
    })

    output$gallery <- renderUI({
      tags$img(src = paste0("gallery/", files[index()]))})
    })

}
