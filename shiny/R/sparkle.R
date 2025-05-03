library(shiny)
library(bslib)

sparkle_module_ui <- function(id){
  ns <- NS(id)
  tagList(
      layout_sidebar(
        sidebar = sidebar(
        markdown("This is a work in progress, inspired by a <a href= http://www.timhead.net/art/art2000/81sce.htm target='_blank'>Tim Head piece</a> that was created by
                 <a href= https://glowinthedark.co.uk/ target='_blank'>Paul Harter</a>. It uses Javascript to draw randomly-coloured squares on an HTML canvas. You can
                 change the probability of new pixels RGB values and make those changes oscillate."),
        accordion(
          multiple = FALSE,
          open = c("Pattern"),
          accordion_panel("Pattern",
            sliderInput(ns("canvas_size"), "Pixels per side", value = 100, min = 100, max = 2000, step = 5),
            sliderInput(ns("pixel_percent"), "Percentage of pixels to update", value = 1, min = 0.1, max = 100, step= 0.1)
          ),
          accordion_panel("Red",
            sliderInput(ns("r_mean"), "Red mean", value = 128, min = 1, max = 256, step = 1),
            sliderInput(ns("r_amplitude"), "Red amplitude", value = 1, min = 0, max = 128, step = 1),
            sliderInput(ns("r_frequency"), "Red frequency", value = 0.1, min = 0.01, max = 1, step = 0.01),
          ),
          accordion_panel("Green",
            sliderInput(ns("g_mean"), "Green mean", value = 128, min = 1, max = 256, step = 1),
            sliderInput(ns("g_amplitude"), "Green amplitude", value = 1, min = 0, max = 128, step = 1),
            sliderInput(ns("g_frequency"), "Green frequency", value = 0.1, min = 0.01, max = 1, step = 0.01),
          ),
          accordion_panel("Blue",
            sliderInput(ns("b_mean"), "Blue mean", value = 128, min = 1, max = 256, step = 1),
            sliderInput(ns("b_amplitude"), "Blue amplitude", value = 1, min = 0, max = 128, step = 1),
            sliderInput(ns("b_frequency"), "Blue frequency", value = 0.1, min = 0.01, max = 1, step = 0.01),
          ),
        ),
      sliderInput(ns("alpha"), "Transparency", value = 128, min = 1, max = 256, step = 1),
      plotOutput(ns("sine_wave_plot")),
      width = "400px"

      # have a prob of being affected by colour rules
      # make rules to oscillate the colour ranges (and others)
      # https://www.bbc.co.uk/opensource/projects/project/peaks-js

    ),
    tags$div(
      class = "canvas_parent",
        tags$div(
          class = "canvas_container",
          tags$canvas(
            id = "myCanvas",
            width = 100,
            height = 100,
            style = "width: 100%; height: 100%; image-rendering: pixelated;"
          )
        ),
      tags$script(src = "sparkle.js")
      )
    )
  )
}

sparkle_module_server <- function(id, patterns, module){
  moduleServer(id, function(input, output, session) {

  output$sine_wave_plot <- renderPlot({
    time <- seq(0, 20, length.out = 1000)

    r_sine <- input$r_amplitude * sin(2 * pi * input$r_frequency * time) + 128 # input$r_mean
    g_sine <- input$g_amplitude * sin(2 * pi * input$g_frequency * time) + 128 # input$g_mean
    b_sine <- input$b_amplitude * sin(2 * pi * input$b_frequency * time) + 128 # input$b_mean

    plot(time, r_sine, type = "l", col = "red", lwd = 2,
         xlab = "Time (seconds)", ylab = "Amplitude",
         main = "RGB waveforms",
         ylim = c(0, 256))
    lines(time, g_sine, col = "green", lwd = 2)
    lines(time, b_sine, col = "blue", lwd = 2)
  })

  }
)}



