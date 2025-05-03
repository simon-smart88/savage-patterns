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

    files <- sample(c("square_2025-02-05 20_24_22.svg", "ring_2025-02-05 20_09_42.svg",
                      "square_2025-02-05 20_21_02.svg", "ring_2025-02-05 20_11_14.svg",
                      "line_2025-02-05 20_45_09.svg", "square_2025-02-05 20_23_24.svg",
                      "square_2025-02-05 20_27_13.svg", "square_2025-02-05 20_34_07.svg",
                      "line_2025-02-05 20_45_36.svg", "line_2025-02-05 20_48_27.svg",
                      "square_2025-02-05 20_19_41.svg", "line_2025-02-05 20_39_50.svg",
                      "ring_2025-02-05 20_05_02.svg", "line_2025-02-05 20_44_31.svg",
                      "line_2025-02-11 21_46_48.svg", "ring_2025-02-05 20_13_34.svg",
                      "line_2025-02-05 20_37_19.svg", "line_2025-02-11 21_43_55.svg",
                      "square_2025-02-05 20_32_54.svg", "line_2025-02-05 20_38_18.svg",
                      "square_2025-02-05 20_30_27.svg", "line_2025-02-11 21_45_06.svg",
                      "line_2025-02-05 20_50_03.svg"))

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
      tags$img(src = paste0("https://raw.githubusercontent.com/simon-smart88/savage-patterns/main/images/gallery/", files[index()]))})
    })

}
