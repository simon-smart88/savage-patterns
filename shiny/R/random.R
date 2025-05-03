random_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "title", tags$label("Randomise")),
    actionButton(ns("random"), "Everything", icon = icon("random"), width = "100%", style = "font-size: 1.5rem;"),
    layout_columns(
      actionButton(ns("random_pattern"), "Pattern", width = "100%"),
      actionButton(ns("random_animation"), "Animation", width = "100%"),
      actionButton(ns("random_colour"), "Colour", width = "100%")
    ),
    div(class = "title", tags$label("Adjust")),
  )
}

random_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    list(
      all = reactive(input$random),
      pattern = reactive(input$random_pattern),
      animation = reactive(input$random_animation),
      colour = reactive(input$random_colour)
    )
  })
}
