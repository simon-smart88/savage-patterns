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
    layout_columns(
      col_widths = c(4, 4, 4),
      colourpicker::colourInput(ns("colour_1"), "", showColour = "background", closeOnClick = TRUE),
      colourpicker::colourInput(ns("colour_2"), "", showColour = "background", closeOnClick = TRUE),
      colourpicker::colourInput(ns("colour_3"), "", showColour = "background", closeOnClick = TRUE))
    )
}

colour_server <- function(id, invalidate_color) {
  moduleServer(id, function(input, output, session) {

    observe({
      invalidate_color()
      random <- randomcoloR::randomColor(count = 3, hue = input$hue, luminosity = input$lumin)
      colourpicker::updateColourInput(session, "colour_1", value = random[1])
      colourpicker::updateColourInput(session, "colour_2", value = random[2])
      colourpicker::updateColourInput(session, "colour_3", value = random[3])
    })

    list(
      colour_1 = reactive(input$colour_1),
      colour_2 = reactive(input$colour_2),
      colour_3 = reactive(input$colour_3)
    )
  })
}
