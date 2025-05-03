library(colourpicker)

random_color <- function(count = 1,
                         hue = c(" ", "random", "red", "orange", "yellow",
                                 "green", "blue", "purple", "pink", "monochrome"),
                         luminosity = c(" ", "random", "light", "bright", "dark")) {

  # Argument matching
  hue <- match.arg(hue)
  luminosity <- match.arg(luminosity)

  # Handle defaults/empty strings
  if (hue == " ") hue <- "random"
  if (luminosity == " ") luminosity <- "random"

  # Generate requested number of colors
  colors <- replicate(count, {
    # Handle random hue selection
    current_hue <- if (hue == "random") {
      sample(c("red", "orange", "yellow", "green", "blue", "purple", "pink", "monochrome"), 1)
    } else {
      hue
    }

    # Handle random luminosity selection
    current_luminosity <- if (luminosity == "random") {
      sample(c("light", "bright", "dark"), 1)
    } else {
      luminosity
    }

    # Generate the actual color
    generate_single_color(current_hue, current_luminosity)
  })

  return(colors)
}

# Helper function to generate a single color
generate_single_color <- function(hue, luminosity) {
  if (hue == "monochrome") {
    s <- 0
    h <- 0
  } else {
    s <- runif(1, 0.5, 1)
    h <- switch(hue,
                "red" = 0,
                "orange" = 30,
                "yellow" = 60,
                "green" = 120,
                "blue" = 240,
                "purple" = 270,
                "pink" = 330,
                runif(1, 0, 360)) # for "random" or invalid hue
  }

  l <- switch(luminosity,
              "bright" = runif(1, 0.5, 0.8),
              "light" = runif(1, 0.7, 0.9),
              "dark" = runif(1, 0.1, 0.4),
              runif(1, 0.1, 0.9)) # fallback for invalid luminosity

  rgb_col <- hcl(h, s * 100, l * 100)
  return(rgb_col)
}

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
      colourInput(ns("colour_1"), "", showColour = "background", closeOnClick = TRUE),
      colourInput(ns("colour_2"), "", showColour = "background", closeOnClick = TRUE),
      div(class = "custom-left-picker",
        colourInput(ns("colour_3"), "", showColour = "background", closeOnClick = TRUE)
      )
    )
  )
}

colour_server <- function(id, invalidate_color) {
  moduleServer(id, function(input, output, session) {

    observe({
      invalidate_color()
      random <- random_color(count = 3, hue = input$hue, luminosity = input$lumin)
      updateColourInput(session, "colour_1", value = random[1])
      updateColourInput(session, "colour_2", value = random[2])
      updateColourInput(session, "colour_3", value = random[3])
    })

    list(
      colour_1 = reactive(input$colour_1),
      colour_2 = reactive(input$colour_2),
      colour_3 = reactive(input$colour_3)
    )
  })
}
