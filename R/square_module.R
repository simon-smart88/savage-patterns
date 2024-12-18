square_module_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
    random_ui(ns("square")),
    accordion(
      multiple = FALSE,
      open = c("Pattern"),
      accordion_panel("Pattern",
        sliderInput(ns("reps"),
                    span("Number of squares",
                         tooltip(icon("info-circle"), "The number of squares on each side of the pattern")),
                    value = 19, step = 2, min = 7, max = 31, ticks = FALSE),
        sliderInput(ns("bulge"),
                    span("Bulge",
                         tooltip(icon("info-circle"), "How much the pattern bulges - negative values make the corners bulge, positive values make the centre bulge")),
                    value = 5, step = 0.5, min = -10, max = 10, ticks = FALSE),
        sliderInput(ns("internal"),
                    span("Internal square size",
                         tooltip(icon("info-circle"), "The size of the squares inside the squares")),
                    value = c(10, 50), step = 1, min = 10, max = 90, ticks = FALSE),
        shinyWidgets::materialSwitch(ns("switch"),
                                     span("Activate checkerboard",
                                          tooltip(icon("info-circle"), "If this is toggled, the colours of the squares are swapped round in adjacent squares creating a checkerboard effect")),
                                     value = FALSE, status = "success"),
      ),
      accordion_panel("Animation",
        #sliderInput(ns("lag_offset"), "Maximum internal", value = 20, step = 1, min = 10, max = 30),
        sliderInput(ns("speed"),
                    span("Animation duration",
                         tooltip(icon("info-circle"), "How long in seconds the animation takes to go through one cycle")),
                    value = 30, step = 1, min = 5, max = 60, ticks = FALSE),
        sliderInput(ns("colour_speed"),
                    span("Colour change duration",
                         tooltip(icon("info-circle"), "How long in seconds the colour animation takes to go through one cycle")),
                    value = 30, step = 1, min = 5, max = 60, ticks = FALSE),
      ),
      accordion_panel("Colour",
        sliderInput(ns("colour_dif"),
                    span("Inner and outer colour difference",
                         tooltip(icon("info-circle"), "How contrasting the colour difference is between the inner and outer squares")),
                    value = 50, step = 1, min = 5, max = 95, ticks = FALSE),
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

square_module_server <- function(id, patterns){
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

    random <- random_server("square")

    observeEvent(random$all(), {
      updateSliderInput(session, "bulge", value = sample.int(20, size = 1) - 10)
      updateSliderInput(session, "reps", value = 1 + sample.int(15, size = 1) * 2)
      low_internal <- 10 + sample.int(30, size = 1)
      high_internal <- low_internal + sample.int(30, size = 1)
      updateSliderInput(session, "internal", value = c(low_internal, high_internal))
      updateSliderInput(session, "speed", value = 4 + sample.int(56, size = 1))
      updateSliderInput(session, "colour_speed", value = 4 + sample.int(56, size = 1))
      updateSliderInput(session, "dif", value = 5 + sample.int(90, size = 1))
      invalidate_trigger(invalidate_trigger() + 1)
    })

    observeEvent(random$pattern(), {
      updateSliderInput(session, "bulge", value = sample.int(20, size = 1) - 10)
      updateSliderInput(session, "reps", value = 1 + sample.int(15, size = 1) * 2)
      low_internal <- 10 + sample.int(30, size = 1)
      high_internal <- low_internal + sample.int(30, size = 1)
      updateSliderInput(session, "internal", value = c(low_internal, high_internal))
    })

    observeEvent(random$animation(), {
      updateSliderInput(session, "speed", value = 4 + sample.int(56, size = 1))
      updateSliderInput(session, "colour_speed", value = 4 + sample.int(56, size = 1))
      updateSliderInput(session, "dif", value = 5 + sample.int(90, size = 1))
    })

    observeEvent(random$colour(), {
      invalidate_trigger(invalidate_trigger() + 1)
    })

    # function to generate svg circle
    svg_rect <- function(matrix, reps, bulge, internal, speed){

      # + 0.5 to avoid white lines
      width <- round(matrix[7], 3) + 0.5
      x <- round(matrix[8], 3)
      height <- round(matrix[9], 3) + 0.5
      y <- round(matrix[10], 3)

      center_x <- x + (width * 0.5)
      center_y <- y + (height * 0.5)

      # Initial width and height
      initial_width <- round(width * (1 - (internal[1] / 100)), 3)
      initial_height <- round(height * (1 - (internal[1] / 100)), 3)

      # Final width and height (increased by the factor)
      final_width <- round(width * (1 - (internal[2] / 100)), 3)
      final_height <- round(height * (1 - (internal[2] / 100)), 3)

      # Calculate x and y to keep the rectangle centered
      initial_x <- round(center_x - (initial_width / 2), 3)
      initial_y <- round(center_y - (initial_height / 2), 3)
      final_x <- round(center_x - (final_width / 2), 3)
      final_y <- round(center_y - (final_height / 2), 3)

      tags$g(
        tags$rect(
          class = glue("rect_{matrix[4]}_out"),
          x = x,
          y =  y,
          width = width,
          height = height),
        tags$rect(
          class = glue("rect_{matrix[4]}_in"),
          tags$animate(
            attributeName = "x",
            values = glue("{initial_x}; {final_x}; {initial_x}"),
            dur = glue("{speed}s"),
            repeatCount = "indefinite"),
          tags$animate(
            attributeName = "y",
            values = glue("{initial_y}; {final_y}; {initial_y}"),
            dur = glue("{speed}s"),
            repeatCount = "indefinite"),
          tags$animate(
            attributeName = "width",
            values = glue("{initial_width}; {final_width}; {initial_width}"),
            dur = glue("{speed}s"),
            repeatCount = "indefinite"),
          tags$animate(
            attributeName = "height",
            values = glue("{initial_height}; {final_height}; {initial_height}"),
            dur = glue("{speed}s"),
            repeatCount = "indefinite")
          )
        )

    }

    observe({
      shinyjs::runjs(glue("
      document.getElementById('pattern').style.setProperty('--colour_speed', '{input$colour_speed}s');"))
    })

    observe({
      req(length(input_colours()) > 0)
      colours <- input_colours()
      shinyjs::runjs(glue("
      document.getElementById('pattern').style.setProperty('--colour_1', '{input$colour_1}');
      document.getElementById('pattern').style.setProperty('--colour_2', '{input$colour_2}');
      document.getElementById('pattern').style.setProperty('--colour_3', '{input$colour_3}');"))
    })


    input_colours <- reactive({
      n_cols <- 3
      ids <- paste0("colour_", 1:n_cols)
      c(lapply(seq_along(ids), function(i) {input[[ids[i]]]}))
    })

    css_colours <- reactive({
      n_cols <- 3
      ids <- paste0("colour_", 1:n_cols)
      steps <- seq(0, 100, length.out = (2*n_cols)-1)
      a <- paste(lapply(seq_along(ids), function(i) {glue::glue("{steps[i]}% {{fill: input[['{ids[i]}']] }}")}), collapse= ' ')
      a
    })

    css_colour_vars <- function(variable, colour){
      glue("{variable}: {colour} ;")
    }

    # generates css of colour sequence
    css_colour_keys <- function(frame, variable){
      glue("{frame}% {{fill: var({variable})}}")
    }

    # generates delays in start of css animation to create gradient
    # reversed for inner and outer rectangles
    css_delay <- function(matrix, speed, reps, colour_dif){
      glue(".rect_{matrix[1] + 1}_out {{animation-delay: {-(speed/reps)*matrix[4]}s;}}
            .rect_{matrix[1] + 1}_in {{animation-delay: {(-((speed/reps)*matrix[4])) - (speed * {colour_dif / 100})}s;}}")
    }

    # generate the pattern
    svg_pattern <- reactive({
      req(length(input_colours) > 0)
      #view port to crop borders
      top_corner <- 0
      bottom_corner <- 1000

      # create a matrix of sequences
      reps <- input$reps
      low_half <- (reps-1)/2
      high_half <- (reps+1)/2
      element_mat <- matrix(c(
        # column and row indices
        rep(seq(1:reps),reps) - 1, # 123,123,123
        rep(seq(1:reps),each=reps) - 1, # 111,222,333
        rep(c(1:(low_half+1), (low_half):1), reps), #12321
        # bulge with higher values at the centre of the matrix
        rep(c(0:low_half, (low_half-1):0), reps) + #01210
          c(rep(seq(1:high_half),each=reps), rev(rep(seq(low_half:1), each=reps)), #111,222,111
        # reverse bulge
        rep(c(low_half:0, (1:low_half)), reps) +  #21012
          c(rev(rep(1:high_half,each=reps)), rep(2:high_half, each=reps)), #222,111,222
        rep(c((high_half):1, (2:high_half)), reps))),
        nrow = reps^2, byrow = F)

      if (input$bulge >= 0){
        width_bulge <- element_mat[,3] ^ (input$bulge/5)
      }
      if (input$bulge < 0){
        #width_bulge <- element_mat[,6] ^ (abs(input$bulge)/5)
        pattern <- rep(c((high_half):1, (2:high_half)), reps)
        width_bulge <- pattern ^ (abs(input$bulge)/5)
      }

      if (input$switch){
        element_mat[(element_mat[,4] %% 2 == 1 ), 4] <- element_mat[(element_mat[,4] %% 2 == 1 ), 5]
      }

      total_width <- sum(width_bulge)
      width_fraction <- width_bulge / total_width
      width <- width_fraction * input$reps * 1000
      element_mat <- cbind(element_mat, width)

      x <- rep(c(0, cumsum(width[1:(reps-1)])), reps)
      element_mat <- cbind(element_mat, x)

      height <- rep(width[1:reps], each = reps)
      element_mat <- cbind(element_mat, height)

      y <- rep(x[1:reps], each = reps)
      element_mat <- cbind(element_mat, y)

      # apply the svg_rect function to the matrix
      elements <- apply(element_mat, 1, svg_rect,
                        internal = input$internal,
                        speed = input$speed,
                        reps = input$reps,
                        bulge = input$bulge)

      elements_sub <- element_mat[unique(element_mat[,4]),]
      # interesting with input$speed too
      css_delay_result <- paste(
        apply(elements_sub, 1, css_delay,
              speed = input$colour_speed,
              reps = input$reps,
              colour_dif = input$colour_dif),
        collapse= ' ')

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
          tags$style(
            paste0("
              :root{",
                 css_colour_var_result
                 ,"
               --colour_speed: 30s;
              }
              rect {animation: col var(--colour_speed) linear infinite;
                    fill: none;
                    }

              @keyframes col {",css_colour_keys_result,"
              }
         ")),
         tags$style(css_delay_result),
         elements))
    })

    observe(patterns$square <- svg_pattern())

  }
  )}
