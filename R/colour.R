colour_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("hue"),
                span("Hue",
                     tooltip(icon("info-circle"), "Choose a colour palette to use")),
                c("Random" = "random", "Red" = "red", "Orange" = "orange",
                  "Yellow" = "yellow", "Green" = "green", "Blue" =  "blue",
                  "Purple" = "purple", "Pink" = "pink", "Monochrome" = "monochrome")),
    selectInput(ns("lumin"),
                span("Luminosity",
                     tooltip(icon("info-circle"), "Choose how bright the colour palette is")),
                c("Random" = "random", "Light" = "light",
                  "Bright" = "bright", "Dark" = "dark")),
    uiOutput(ns("colour_picker"))
    )

}

colour_server <- function(id, invalidate_color) {
  moduleServer(id, function(input, output, session) {

    init_cols <- reactive({
      invalidate_color()
      randomcoloR::randomColor(count = 3, hue = input$hue, luminosity = input$lumin)
    })

    output$colour_picker <- renderUI({
      tagList(
        tags$label(
          span("Pick colours",
               tooltip(icon("info-circle"), "Manually select the colours that the pattern cycles through"))
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          colourpicker::colourInput(session$ns("colour_1"), "", value = init_cols()[1], showColour = "background", closeOnClick = TRUE),
          colourpicker::colourInput(session$ns("colour_2"), "", value = init_cols()[2], showColour = "background", closeOnClick = TRUE),
          colourpicker::colourInput(session$ns("colour_3"), "", value = init_cols()[3], showColour = "background", closeOnClick = TRUE)))
    })

    list(
      colour_1 = reactive(input$colour_1),
      colour_2 = reactive(input$colour_2),
      colour_3 = reactive(input$colour_3)
    )
  })
}
