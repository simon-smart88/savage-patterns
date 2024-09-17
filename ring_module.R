ring_module_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
  sliderInput(ns("radius"), "Radius", value = 30, step = 1, min = 10, max = 50),
  sliderInput(ns("bulge"), "Radius bulge", value = 1, step = 0.1, min = 0.1, max = 100),
  sliderInput(ns("reps"), "Repetitions", value = 25, step = 2, min = 11, max = 45),
  sliderInput(ns("space"), "Space between circles", value = 10, step = 1, min = 1, max = 30),
  sliderInput(ns("breath"), "Change in radius", value = 20, step = 1, min = 10, max = 30),
  sliderInput(ns("speed"), "Animation duration", value = 30, step = 1, min = 5, max = 60),
  sliderInput(ns("stroke"), "Line thickness", value = 0.5, step = 0.05, min = 0.1, max = 1),
  uiOutput(ns("colour_picker")),
  selectInput(ns("hue"), "Hue", c("random", "red", "orange", "yellow",
                                  "green", "blue", "purple", "pink", "monochrome")),
  selectInput(ns("lumin"), "Luminosity", c("random", "light", "bright", "dark")),
  actionButton(ns("random"), "Randomise"),
  actionButton(ns("update"), "Update")
  )
}

ring_module_server <- function(id, patterns){
  moduleServer(id, function(input, output, session) {

    init_cols <- reactive({
      randomcoloR::randomColor(count = 3, hue = input$hue, luminosity = input$lumin)
    })

    output$colour_picker <- renderUI({
      n_cols <- 3
      ids <- session$ns(paste0("colour_", 1:n_cols))
      tagList(
        tags$label("Pick colours"),
        lapply(seq_along(ids), function(i) {colourpicker::colourInput(
          ids[i], "", value = init_cols()[i],
          showColour = "background", closeOnClick = TRUE)}))
    })

    observeEvent(input$random, {
      updateSliderInput(session, "radius", value = 9 + sample.int(41, size = 1))
      updateSliderInput(session, "bulge", value = sample.int(100, size = 1))
      updateSliderInput(session, "reps", value = 1 + sample.int(22, size = 1) * 2)
      updateSliderInput(session, "space", value = sample.int(30, size = 1))
      updateSliderInput(session, "breath", value = 9 + sample.int(21, size = 1))
      updateSliderInput(session, "speed", value = 4 + sample.int(56, size = 1))
      updateSliderInput(session, "stroke", value = sample.int(10, size = 1)/10)
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

    # gradient <- callModule(gradientInput, "cols", init_cols = c(10, 50, 70))
    # #create colour gradient from result
    # #sample from gradient depending on reps

    #function to generate svg circle
    svg_circle <- function(x, space, speed, radius, breath, stroke, reps, bulge){
      breath <- breath/100
      radius_bulge <- (x[3]/reps*2) * (bulge/10)
      tags$circle(class = glue("circle_{x[3]}"),
                  cx = radius+(x[1]*space),
                  cy = radius+(x[2]*space),
                  # `stroke-width` = glue("{stroke}px"),
                  # animate the radius
                  tags$animate(attributeName = "r",
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
      #view port to crop borders
      top_corner <- 2 * input$radius
      #top_corner <- input$radius+ ((input$radius*(1+input$breath))+input$bulge)
      bottom_corner <- (input$reps * input$space) - top_corner

      #create a matrix of sequences
      reps <- input$reps
      low_half <- (reps-1)/2
      high_half <- (reps+1)/2
      element_mat <- matrix(c(
        #column and row indices
        rep(seq(1:reps),reps), # 123,123,123
        rep(seq(1:reps),each=reps), # 111,222,333
        #bulge with higher values at the centre of the matrix
        rep(c(0:low_half, (low_half-1):0), reps) + #01210
          c(rep(seq(1:high_half),each=reps), rev(rep(seq(low_half:1), each=reps)))), #111,222,111
        nrow = reps^2, byrow = F)

      #apply the svg_circle function to the matrix
      elements <- apply(element_mat, 1, svg_circle,
                        space = input$space,
                        speed = input$speed,
                        radius = input$radius,
                        breath = input$breath,
                        # stroke = input$stroke,
                        reps = input$reps,
                        bulge = input$bulge)

      elements_sub <- element_mat[unique(element_mat[,3]),]
      css_delay_result <- paste(apply(elements_sub, 1, css_delay, speed = input$speed, reps = input$reps), collapse= ' ')

      #colours <- gradient$result()$col
      colours <- isolate(input_colours())
      n_cols <- length(colours)
      col_vars <- glue("--colour_{1:n_cols}")

      css_colour_var_result <- paste(css_colour_vars(col_vars, colours), collapse = ' ')

      frames <- c(seq(0, 50, length.out = n_cols), seq(50, 100, length.out = n_cols)[2:n_cols])
      colour_var_seq <- c(col_vars, rev(col_vars[1:n_cols - 1]))
      css_colour_keys_result <- paste(css_colour_keys(frames, colour_var_seq), collapse = ' ')

      #create the final svg
      tagList(tags$svg(xmlns = "http://www.w3.org/2000/svg",
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

                                @keyframes col {",css_colour_keys_result,"
                                }
                       ")),
                       tags$style(css_delay_result),

                       elements))
    })
    observe(patterns$ring <- svg_pattern())

  }
)}
