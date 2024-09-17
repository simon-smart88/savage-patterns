square_module_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
    sliderInput(ns("reps"), "Repetitions", value = 25, step = 2, min = 11, max = 45),
    sliderInput(ns("bulge"), "Radius bulge", value = 1, step = 0.1, min = 0.1, max = 10),
    sliderInput(ns("internal"), "Internal size", value = c(10, 50), step = 1, min = 10, max = 90),
    #sliderInput(ns("lag_offset"), "Maximum internal", value = 20, step = 1, min = 10, max = 30),
    sliderInput(ns("speed"), "Animation duration", value = 30, step = 1, min = 5, max = 60),
    uiOutput(ns("colour_picker")),
    actionButton(ns("add_colour"), "Add colour"),
    selectInput(ns("hue"), "Hue", c("random", "red", "orange", "yellow",
                                    "green", "blue", "purple", "pink", "monochrome")),
    selectInput(ns("lumin"), "Luminosity", c("random", "light", "bright", "dark")),
    actionButton(ns("update"), "Update")
  )
}

square_module_server <- function(id, patterns){
  moduleServer(id, function(input, output, session) {


    output$colour_picker <- renderUI({
      n_cols <- 2 + input$add_colour
      ids <- session$ns(paste0("colour_", 1:n_cols))
      init_cols <- randomcoloR::randomColor(count = n_cols, hue = input$hue, luminosity = input$lumin)
      tagList(
        tags$label("Pick colours"),
        lapply(seq_along(ids), function(i) {colourpicker::colourInput(
                                          ids[i], "", value = init_cols[i],
                                          showColour = "background", closeOnClick = TRUE)}))
    })


    #function to generate svg circle
    svg_rect <- function(matrix, reps, bulge, internal, speed){
      # for use with colours
      centre_bulge <- (matrix[4]/reps*2) * (bulge/10)


      # total_width <- sum(width_bulge)
      # width_fraction <- width_bulge / total_width
      # width <- width_fraction * 1000
      # browser()
      width <- matrix[5]
      x <- matrix[6]
      height <- matrix[7]
      y <- matrix[8]
      tags$rect(class = glue("rect_{matrix[3]}"),
                  # x = matrix[1] * width,
                  # y = matrix[2] * width,
                  # width = width + (matrix[1] * width),
                  # height = width + (matrix[2] * width),
                  x = x,
                  y =  y,
                  width = width,
                  height = height,
                  fill = "red",
                  stroke = "black",
                  `stroke-width` = 1

                  # animate the radius
                  # tags$animate(attributeName = "r",
                  #              values = glue("{(radius*(1-breath))+radius_bulge};
                  #                           {(radius*(1+breath))+radius_bulge};
                  #                           {(radius*(1-breath))+radius_bulge}"),
                  #              dur = glue("{speed}s"),
                  #              repeatCount = "indefinite"),
      )
    }

    # observeEvent(input$update, {
    #     shinyjs::runjs(sprintf("
    #         document.getElementById('%s').style.fill = '%s';", "ab", "#00FF00"))
    #   })
    observeEvent(input$update, {
      shinyjs::runjs(sprintf("
            document.getElementById('anim').setAttribute('values', '40; 50; 40');"))
    })


#
#     observeEvent(input$get_html, {
#     a <- shinyjs::runjs("document.getElementsByTagName('svg');")
#     print(a)
#     })
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

    css_colours <- reactive({
      n_cols <- 3 + input$add_colour
      ids <- paste0("colour_", 1:n_cols)
      steps <- seq(0, 100, length.out = (2*n_cols)-1)
      a <- paste(lapply(seq_along(ids), function(i) {glue::glue("{steps[i]}% {{fill: input[['{ids[i]}']] }}")}), collapse= ' ')
      #print(a)
      a
    })

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
      top_corner <- 0
      #top_corner <- input$radius+ ((input$radius*(1+input$breath))+input$bulge)
      bottom_corner <- 1000

      #create a matrix of sequences
      reps <- input$reps
      low_half <- (reps-1)/2
      high_half <- (reps+1)/2
      element_mat <- matrix(c(
        #column and row indices
        rep(seq(1:reps),reps) - 1, # 123,123,123
        rep(seq(1:reps),each=reps) - 1, # 111,222,333
        rep(c(1:(low_half+1), (low_half):1), reps), #12321
        #bulge with higher values at the centre of the matrix
        rep(c(0:low_half, (low_half-1):0), reps) + #01210
          c(rep(seq(1:high_half),each=reps), rev(rep(seq(low_half:1), each=reps)))), #111,222,111
        nrow = reps^2, byrow = F)

      width_bulge <- element_mat[,3] ^ (input$bulge/10)
      total_width <- sum(width_bulge)
      width_fraction <- width_bulge / total_width
      width <- width_fraction * input$reps * 1000
      element_mat <- cbind(element_mat, width)

      x <- rep(c(0, cumsum(width[1:(reps-1)])), reps)
      element_mat <- cbind(element_mat, x)

      height <- rep(width[1:reps], each = reps)
      element_mat <- cbind(element_mat, height)

      #y <- rep(c(0, cumsum(height[1:(reps-1)])), reps)
      y <- rep(x[1:reps], each = reps)
      element_mat <- cbind(element_mat, y)

      # browser()
      #apply the svg_circle function to the matrix
      elements <- apply(element_mat, 1, svg_rect,
                        # width = width,
                        internal = input$internal,
                        speed = input$speed,
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

    #' svg_pattern <- reactive({
    #'
    #'   tagList(tags$svg(xmlns = "http://www.w3.org/2000/svg",
    #'                    `xmlns:xlink`="http://www.w3.org/1999/xlink",
    #'                    version="1.1",
    #'                    viewBox = glue("0 0 100 100"),
    #'                    height="100%",
    #'                    tags$style(paste0("
    #'                             .clr {animation: col 3s linear infinite;}
    #'                             @keyframes col {",css_colours(),"}")),
    #'                   #  tags$style("
    #'                   # .clr {attributeName: 'width';
    #'                   #       values: '40;80;40';
    #'                   #       repeatCount: 'indefinite';}
    #'                   #  "),
    #'                    tags$rect(class="clr",
    #'                              x = 10,
    #'                              y = 10,
    #'                              height = 50,
    #'                              width = 30,
    #'                              `stroke-width` = 2,
    #'                              # fill = 'red',
    #'                              stroke='black'),
    #'                   tags$rect(class="clr",
    #'                             x = 10,
    #'                             y = 10,
    #'                             height = 50,
    #'                             width = 30,
    #'                             `stroke-width` = 2,
    #'                             # fill = 'red',
    #'                             stroke='black'),
    #'                    ))
    #' })
    observe(patterns$square <- svg_pattern())

  }
  )}
