ring_module_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
    random_ui(ns("ring")),
    accordion(
      multiple = FALSE,
      open = c("Pattern"),
      accordion_panel("Pattern",
        sliderInput(ns("reps"),
                    span("Number of circles",
                         tooltip(icon("info-circle"), "The number of circles on each side of the pattern")),
                    value = 25, step = 2, min = 7, max = 45, ticks = FALSE),
        sliderInput(ns("radius"),
                    span("Circle size",
                         tooltip(icon("info-circle"), "The size of each circle")),
                    value = 30, step = 1, min = 10, max = 50, ticks = FALSE),
        sliderInput(ns("bulge"),
                    span("Circle size bulge",
                         tooltip(icon("info-circle"), "Determines how much bigger the circles in the centre are compared to the outside")),
                    value = 1, step = 0.1, min = 0.1, max = 100, ticks = FALSE),
        sliderInput(ns("space"),
                    span("Distance between circles",
                         tooltip(icon("info-circle"), "The distance between each circle")),
                    value = 10, step = 1, min = 1, max = 30, ticks = FALSE)
      ),
      accordion_panel("Animation",
        sliderInput(ns("breath"),
                    span("Change in size",
                         tooltip(icon("info-circle"), "How much the circles change in size during the animation")),
                    value = 20, step = 1, min = 10, max = 30, ticks = FALSE),
        sliderInput(ns("speed"),
                    span("Animation duration",
                         tooltip(icon("info-circle"), "How long in seconds the animation takes to go through one cycle")),
                    value = 30, step = 1, min = 5, max = 60, ticks = FALSE)
      ),
      accordion_panel("Colour",
        sliderInput(ns("stroke"),
                    span("Line thickness",
                         tooltip(icon("info-circle"), "Adjust the thickness of the lines")),
                    value = 0.5, step = 0.05, min = 0.1, max = 1, ticks = FALSE),
        uiOutput(ns("colour_picker")),
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
                      "Bright" = "bright", "Dark" = "dark"))
      )
    )
  )
}

ring_module_server <- function(id, patterns){
  moduleServer(id, function(input, output, session) {

    invalidate_trigger <- reactiveVal(0)

    init_cols <- reactive({
      invalidate_trigger()
      randomcoloR::randomColor(count = 3, hue = input$hue, luminosity = input$lumin)
    })

    output$colour_picker <- renderUI({
      n_cols <- 3
      ids <- session$ns(paste0("colour_", 1:n_cols))
      tagList(
        tags$label(
          span("Pick colours",
               tooltip(icon("info-circle"), "Manually select the colours that the pattern cycles through"))
        ),
        lapply(seq_along(ids), function(i) {colourpicker::colourInput(
          ids[i], "", value = init_cols()[i],
          showColour = "background", closeOnClick = TRUE)}))
    })

    random <- random_server("ring")

    observeEvent(random$all(), {
      updateSliderInput(session, "radius", value = 9 + sample.int(41, size = 1))
      updateSliderInput(session, "bulge", value = sample.int(100, size = 1))
      updateSliderInput(session, "reps", value = 1 + sample.int(22, size = 1) * 2)
      updateSliderInput(session, "space", value = sample.int(30, size = 1))
      updateSliderInput(session, "breath", value = 9 + sample.int(21, size = 1))
      updateSliderInput(session, "speed", value = 4 + sample.int(56, size = 1))
      updateSliderInput(session, "stroke", value = sample.int(10, size = 1)/10)
      invalidate_trigger(invalidate_trigger() + 1)
    })

    observeEvent(random$pattern(), {
      updateSliderInput(session, "radius", value = 9 + sample.int(41, size = 1))
      updateSliderInput(session, "bulge", value = sample.int(100, size = 1))
      updateSliderInput(session, "reps", value = 1 + sample.int(22, size = 1) * 2)
      updateSliderInput(session, "space", value = sample.int(30, size = 1))
    })

    observeEvent(random$animation(), {
      updateSliderInput(session, "breath", value = 9 + sample.int(21, size = 1))
      updateSliderInput(session, "speed", value = 4 + sample.int(56, size = 1))
    })

    observeEvent(random$colour(), {
      updateSliderInput(session, "stroke", value = sample.int(10, size = 1)/10)
      invalidate_trigger(invalidate_trigger() + 1)
    })

    observe({
      req(length(input_colours()) > 0)
      colours <- input_colours()
      shinyjs::runjs(glue("
      document.getElementById('pattern').style.setProperty('--colour_1', '{input$colour_1}');
      document.getElementById('pattern').style.setProperty('--colour_2', '{input$colour_2}');
      document.getElementById('pattern').style.setProperty('--colour_3', '{input$colour_3}');"))
    })

    observe({
      shinyjs::runjs(glue("
      document.getElementById('pattern').style.setProperty('--stroke', '{input$stroke}');"))
    })

    input_colours <- reactive({
      n_cols <- 3
      ids <- paste0("colour_", 1:n_cols)
      c(lapply(seq_along(ids), function(i) {input[[ids[i]]]}))
    })

    # function to generate svg circle
    svg_circle <- function(x, space, speed, radius, breath, stroke, reps, bulge){
      breath <- breath/100
      radius_bulge <- (x[3]/reps*2) * (bulge/10)
      tags$circle(
        class = glue("circle_{x[3]}"),
        cx = radius+(x[1]*space),
        cy = radius+(x[2]*space),
        # animate the radius
        tags$animate(
          attributeName = "r",
          values = glue("{(radius*(1-breath))+radius_bulge};
                          {(radius*(1+breath))+radius_bulge};
                          {(radius*(1-breath))+radius_bulge}"),
           dur = glue("{speed}s"),
           repeatCount = "indefinite"),
      )
    }

    css_colour_vars <- function(variable, colour){
      glue("{variable}: {colour} ;")
    }

    # generates css of colour sequence
    css_colour_keys <- function(frame, variable){
      glue("{frame}% {{stroke: var({variable})}}")
    }

    # generates delays in start of css animation to create gradient
    css_delay <- function(x, speed, reps){
      glue(".circle_{x[1]} {{ animation-delay: {-(speed/reps)*x[1]}s;}}")
    }

    # generate the pattern
    svg_pattern <- reactive({
      req(length(input_colours) > 0)

      # create a matrix of sequences
      reps <- input$reps
      low_half <- (reps-1)/2
      high_half <- (reps+1)/2
      element_mat <- matrix(c(
        # column and row indices
        rep(seq(1:reps),reps), # 123,123,123
        rep(seq(1:reps),each=reps), # 111,222,333
        # bulge with higher values at the centre of the matrix
        rep(c(0:low_half, (low_half-1):0), reps) + # 01210
          c(rep(seq(1:high_half),each=reps), rev(rep(seq(low_half:1), each=reps)))), #111,222,111
        nrow = reps^2, byrow = F)

      # apply the svg_circle function to the matrix
      elements <- apply(
        element_mat, 1, svg_circle,
        space = input$space,
        speed = input$speed,
        radius = input$radius,
        breath = input$breath,
        reps = input$reps,
        bulge = input$bulge)

      # view port to crop borders
      centre <- input$radius+(element_mat[ceiling(input$reps/2),1]*input$space)
      edge <- centre - (input$radius+(element_mat[3,1]*input$space))
      top_corner <- centre - edge
      bottom_corner <- (centre + edge) - top_corner

      elements_sub <- element_mat[unique(element_mat[,3]),]
      css_delay_result <- paste(apply(elements_sub, 1, css_delay, speed = input$speed, reps = input$reps), collapse= ' ')

      # colours <- gradient$result()$col
      colours <- isolate(input_colours())
      n_cols <- length(colours)
      col_vars <- glue("--colour_{1:n_cols}")

      css_colour_var_result <- paste(css_colour_vars(col_vars, colours), collapse = ' ')

      frames <- c(seq(0, 50, length.out = n_cols), seq(50, 100, length.out = n_cols)[2:n_cols])
      colour_var_seq <- c(col_vars, rev(col_vars[1:n_cols - 1]))
      css_colour_keys_result <- paste(css_colour_keys(frames, colour_var_seq), collapse = ' ')

      # create the final svg
      tagList(
        tags$svg(
          xmlns = "http://www.w3.org/2000/svg",
         `xmlns:xlink`="http://www.w3.org/1999/xlink",
         version="1.1",
         viewBox = glue("{top_corner} {top_corner} {bottom_corner} {bottom_corner}"),
         height = "100%",
         id = "pattern",
         tags$style(paste0("
          :root{",
            css_colour_var_result
            ,"
             --stroke: 0.5px
          }

          circle {animation: col 30s linear infinite;
                  fill: none;
                  stroke-width: var(--stroke)}

          @keyframes col {",
            css_colour_keys_result,"
          }
         ")),
         tags$style(css_delay_result),

         elements))
    })
    observe(patterns$ring <- svg_pattern())

  }
)}
