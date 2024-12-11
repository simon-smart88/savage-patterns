line_module_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("random"), "Randomise", icon = icon("random"), width = "100%", style = "font-size: 1.5rem;"),
    accordion(
      multiple = FALSE,
      open = c("Pattern"),
      accordion_panel("Pattern",
        sliderInput(ns("outer"), "Outer", value = 9, step = 1, min = 6, max = 20),
        sliderInput(ns("inner"), "Inner", value = 3, step = 1, min = 2, max = 15),
        sliderInput(ns("inner_size"), "Inner size", value = 50, step = 1, min = 10, max = 80),
        sliderInput(ns("inner_offset"), "Inner offset", value = 50, step = 1, min = 10, max = 80),
        shinyWidgets::materialSwitch(ns("zoom"), "Zoom", value = FALSE)
      ),
      accordion_panel("Animation",
        sliderInput(ns("breath"), "Change in size", value = 20, step = 1, min = 10, max = 50),
        sliderInput(ns("speed"), "Animation duration", value = 30, step = 1, min = 5, max = 60)
      ),
      accordion_panel("Colour",
        sliderInput(ns("stroke"), "Line thickness", value = 1, step = 0.05, min = 0.1, max = 2)
      )
    )
  )
}

line_module_server <- function(id, patterns){
  moduleServer(id, function(input, output, session) {

    observeEvent(input$random, {
      updateSliderInput(session, "outer", value = 6 + sample.int(14, size = 1))
      updateSliderInput(session, "inner", value = 3+ sample.int(12, size = 1))
      updateSliderInput(session, "inner_size", value = 10 + sample.int(70, size = 1))
      updateSliderInput(session, "inner_offset", value = 10 + sample.int(70, size = 1))
      updateSliderInput(session, "breath", value = 10 + sample.int(30, size = 1))
      updateSliderInput(session, "speed", value = 5 + sample.int(55, size = 1))
      updateSliderInput(session, "stroke", value = sample.int(20, size = 1)/20)
    })

    observe({
      shinyjs::runjs(glue("
      document.getElementById('pattern').style.setProperty('--stroke', '{input$stroke / 100}');"))
    })

    svg_line <- function(x, speed){
      tags$line(x1 = x[1], x2 = x[2], y1 = x[3], y2 = x[4],
                tags$animate(attributeName = "x2",
                             values = glue("{x[2]}; {x[5]}; {x[2]}"),
                             dur = glue("{speed}s"),
                             repeatCount = "indefinite"),
                tags$animate(attributeName = "y2",
                             values = glue("{x[4]}; {x[6]}; {x[4]}"),
                             dur = glue("{speed}s"),
                             repeatCount = "indefinite"),
                )
    }


  svg_pattern <- reactive({

    outer_points <- input$outer
    inner_points <- input$inner

    # create coordinates of the nodes
    inner_offset <- (outer_points - inner_points) / 2
    outer_x <- c(seq(0, outer_points - 1), rep(c(0, outer_points - 1), outer_points - 2), seq(0, outer_points - 1))
    outer_y <- c(rep(0, outer_points), rep(seq(1, outer_points - 2), each = 2), rep(outer_points - 1, outer_points))
    inner_x <- c(seq(0, inner_points - 1), rep(c(0, inner_points - 1), inner_points - 2), seq(0, inner_points - 1)) + inner_offset
    inner_y <- c(rep(0, inner_points), rep(seq(1, inner_points - 2), each = 2), rep(inner_points - 1, inner_points)) + inner_offset

    # rescale the inner points relative to outer points
    old_inner_range <- diff(range(inner_y))
    new_inner_range <- outer_points * (input$inner_size / 100)
    inner_scale <- new_inner_range / old_inner_range
    inner_mean <- mean(inner_x)
    inner_x1 <- (inner_x - inner_mean) * inner_scale + inner_mean
    inner_y1 <- (inner_y - inner_mean) * inner_scale + inner_mean

    animation_factor <- 1 + (input$breath / 100)
    inner_x2 <- (inner_x1 - inner_mean) / animation_factor + inner_mean
    inner_y2 <- (inner_y1 - inner_mean) / animation_factor + inner_mean

    # replicate so that all nodes are joined
    outer_x_set <- rep(outer_x, length(inner_x))
    outer_y_set <- rep(outer_y, length(inner_y))
    inner_x1_set <- rep(inner_x1, each = length(outer_x))
    inner_y1_set <- rep(inner_y1, each = length(outer_y))
    inner_x2_set <- rep(inner_x2, each = length(outer_x))
    inner_y2_set <- rep(inner_y2, each = length(outer_y))

    # adjust the inner lines by the offset if they are joined to the top and bottom outer nodes
    side_top <- outer_y_set == 0 & outer_x_set < outer_points - 1 & outer_x_set > 0
    side_bottom <- outer_y_set == outer_points - 1 & outer_x_set < outer_points - 1 & outer_x_set > 0
    side_left <- outer_x_set == 0
    side_right <- outer_x_set == outer_points - 1

    point_offset <- (inner_x[2] - inner_x[1]) * (input$inner_offset / 100)
    inner_y1_set[side_top] <- inner_y1_set[side_top] - point_offset
    inner_y1_set[side_bottom] <- inner_y1_set[side_bottom] + point_offset
    inner_x1_set[side_left] <- inner_x1_set[side_left] - point_offset
    inner_x1_set[side_right] <- inner_x1_set[side_right] + point_offset

    # apply to the function
    element_mat <- matrix(c(outer_x_set, inner_x1_set, outer_y_set, inner_y1_set, inner_x2_set, inner_y2_set), ncol = 6)
    elements <- apply(element_mat, 1, svg_line,
                      speed = input$speed)

    if (input$zoom){
      top_corner <- min(inner_x1)
      bottom_corner <- max(inner_x1) - top_corner
    } else {
      top_corner <- 0
      bottom_corner <- input$outer - 1
    }

    # create the final svg
    tagList(tags$svg(xmlns = "http://www.w3.org/2000/svg",
                     `xmlns:xlink`="http://www.w3.org/1999/xlink",
                     version="1.1",
                     viewBox = glue("{top_corner} {top_corner} {bottom_corner} {bottom_corner}"),
                     height = "100%",
                     id = "pattern",
                     tags$style(paste0("
                             :root{
                               --stroke: 0.01px
                             }
                            line {stroke-width: var(--stroke);
                                  stroke: black}
                       ")),
                     elements))

  })

  observe(patterns$line <- svg_pattern())

  })
}
