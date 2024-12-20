line_module_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
    random_ui(ns("line")),
    accordion(
      id = ns("acc"),
      multiple = FALSE,
      open = c("Pattern"),
      accordion_panel("Pattern",
        sliderInput(ns("outer"),
                    span("Number of outer points",
                        tooltip(icon("info-circle"), "Determines the number of points on each edge of the square")),
                    value = 9, step = 1, min = 4, max = 20, ticks = FALSE),
        sliderInput(ns("inner"),
                    span("Number of inner points",
                        tooltip(icon("info-circle"), "Determines the number of points inside the square")),
                    value = 3, step = 1, min = 2, max = 15, ticks = FALSE),
        sliderInput(ns("inner_size"),
                    span("Size of inner points",
                        tooltip(icon("info-circle"), "Adjust the position of the inner points - lower numbers move them closer to the centre and high numbers move them closer to the edge")),
                    value = 50, step = 1, min = 10, max = 80, ticks = FALSE),
        sliderInput(ns("inner_offset"),
                    span("Inner point offset",
                        tooltip(icon("info-circle"), "Move the lines to the inner points away from each other towards the nearest outer edge")),
                    value = 50, step = 1, min = 0, max = 80, ticks = FALSE),
        shinyWidgets::materialSwitch(ns("zoom"),
                                     span("Only view inner points",
                                        tooltip(icon("info-circle"), "Toggle this to crop the pattern so that only the inner points are visible")),
                                     value = FALSE)
      ),
      accordion_panel("Animation",
        sliderInput(ns("breath"),
                    span("Change in size",
                        tooltip(icon("info-circle"), "Controls how much the inner point offset increases")),
                    value = 20, step = 1, min = 10, max = 50, ticks = FALSE),
        sliderInput(ns("speed"),
                    span("Animation duration",
                         tooltip(icon("info-circle"), "How long in seconds the animation takes to go through one cycle")),
                    value = 30, step = 1, min = 5, max = 60, ticks = FALSE)
      ),
      accordion_panel("Colour",
        sliderInput(ns("stroke"), "Line thickness", value = 1, step = 0.05, min = 0.1, max = 2, ticks = FALSE),
        sliderInput(ns("opacity"),
                    span("Opacity",
                         tooltip(icon("info-circle"), "Move the lines to the inner points away from each other towards the nearest outer edge")),
                    value = c(25,75), step = 1, min = 0, max = 100, ticks = FALSE),
        colour_ui(ns("line")),
      )
    )
  )
}

line_module_server <- function(id, patterns, module){
  moduleServer(id, function(input, output, session) {

    init <- observe({
      if (module() == "line"){
        shinyjs::runjs("document.getElementById('line_module-line-random').click();")
        init$destroy()
      }
    })

    invalidate_colour <- reactiveVal(0)
    colour <- colour_server("line", invalidate_colour)
    random <- random_server("line")

    observeEvent(random$all(), {
      updateSliderInput(session, "outer", value = 6 + sample.int(14, size = 1))
      updateSliderInput(session, "inner", value = 3+ sample.int(12, size = 1))
      updateSliderInput(session, "inner_size", value = 10 + sample.int(70, size = 1))
      updateSliderInput(session, "inner_offset", value = 10 + sample.int(70, size = 1))
      updateSliderInput(session, "breath", value = 10 + sample.int(30, size = 1))
      updateSliderInput(session, "speed", value = 5 + sample.int(55, size = 1))
      updateSliderInput(session, "stroke", value = sample.int(20, size = 1)/20)
      invalidate_colour(invalidate_colour() + 1)
    })

    observeEvent(random$pattern(), {
      updateSliderInput(session, "outer", value = 6 + sample.int(14, size = 1))
      updateSliderInput(session, "inner", value = 3+ sample.int(12, size = 1))
      updateSliderInput(session, "inner_size", value = 10 + sample.int(70, size = 1))
      updateSliderInput(session, "inner_offset", value = 10 + sample.int(70, size = 1))
      accordion_panel_open("acc", c("Pattern"))
    })

    observeEvent(random$animation(), {
      updateSliderInput(session, "breath", value = 10 + sample.int(30, size = 1))
      updateSliderInput(session, "speed", value = 5 + sample.int(55, size = 1))
      accordion_panel_open("acc", c("Animation"))
    })

    observeEvent(random$colour(), {
      updateSliderInput(session, "stroke", value = sample.int(20, size = 1)/20)
      invalidate_colour(invalidate_colour() + 1)
      accordion_panel_open("acc", c("Colour"))
    })

    observe({
      shinyjs::runjs(glue("
      document.getElementById('pattern').style.setProperty('--stroke', '{input$stroke / 100}');"))
    })

    observe({
      shinyjs::runjs(glue("
      document.getElementById('pattern').style.setProperty('--colour_1', '{colour$colour_1()}');
      document.getElementById('pattern').style.setProperty('--colour_2', '{colour$colour_2()}');
      document.getElementById('pattern').style.setProperty('--colour_3', '{colour$colour_3()}');"))
    })

    observe({
      shinyjs::runjs(glue("
      document.getElementById('pattern').style.setProperty('--opacity_start', '{input$opacity[1] / 100}');
      document.getElementById('pattern').style.setProperty('--opacity_mid', '{mean(input$opacity) / 100}');
      document.getElementById('pattern').style.setProperty('--opacity_end', '{input$opacity[2] / 100}');"))
    })

    observe({
      shinyjs::runjs(glue("
      document.getElementById('pattern').style.setProperty('--stroke', '{input$stroke / 100}');"))
    })

    svg_line <- function(x, speed){
      tags$line(
        x1 = x[1], x2 = x[2], y1 = x[3], y2 = x[4],
        stroke = glue::glue("url(#gradient-opacity{x[7]})"),
        tags$animate(
          attributeName = "x2",
          values = glue("{x[2]}; {x[5]}; {x[2]}"),
          dur = glue("{speed}s"),
          repeatCount = "indefinite"),
        tags$animate(
          attributeName = "y2",
           values = glue("{x[4]}; {x[6]}; {x[4]}"),
           dur = glue("{speed}s"),
           repeatCount = "indefinite")
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
    side_index <- (as.numeric(side_left) * 1) + (as.numeric(side_top) * 2) +
                  (as.numeric(side_right) * 3) + (as.numeric(side_bottom) * 4)

    point_offset <- (inner_x[2] - inner_x[1]) * (input$inner_offset / 100)
    inner_y1_set[side_top] <- inner_y1_set[side_top] - point_offset
    inner_y1_set[side_bottom] <- inner_y1_set[side_bottom] + point_offset
    inner_x1_set[side_left] <- inner_x1_set[side_left] - point_offset
    inner_x1_set[side_right] <- inner_x1_set[side_right] + point_offset

    # apply to the function
    element_mat <- matrix(c(outer_x_set, inner_x1_set, outer_y_set, inner_y1_set, inner_x2_set, inner_y2_set, side_index), ncol = 7)
    elements <- apply(element_mat, 1, svg_line,
                      speed = input$speed)

    if (input$zoom){
      top_corner <- min(inner_x1)
      bottom_corner <- max(inner_x1) - top_corner
    } else {
      top_corner <- 0
      bottom_corner <- input$outer - 1
    }

    css_opacity_var <- glue(" --opacity_start: isolate(input$opacity[1]/100);
                              --opacity_mid: isolate(mean(input$opacity)/100);
                              --opacity_end: isolate(input$opacity[2]/100);")

    css_colour_var <- glue("--colour_1: {isolate(colour$colour_1())};
                            --colour_2: {isolate(colour$colour_2())};
                            --colour_3: {isolate(colour$colour_3())};")


    line_gradients <- tagList(
      tags$stop(offset = "0%", `stop-color` =  "var(--colour_1)", `stop-opacity` = "var(--opacity_start)"),
      tags$stop(offset = "50%", `stop-color` =  "var(--colour_2)", `stop-opacity` = "var(--opacity_mid)"),
      tags$stop(offset = "100%", `stop-color` = "var(--colour_3)", `stop-opacity` = "var(--opacity_end)")
#
#       tags$stop(
#       offset = "0%",
#       tags$animate(
#         attributeName = "stop-color",
#         values = "var(--colour_1); var(--colour_2); var(--colour_3); var(--colour_2); var(--colour_1)",
#         dur = "30s",
#         repeatCount = "indefinite"
#       ),
#       tags$animate(
#         attributeName = "stop-opacity",
#         values = "var(--opacity_start); var(--opacity_mid); var(--opacity_end); var(--opacity_mid); var(--opacity_start)",
#         dur = "30s",
#         repeatCount = "indefinite"
#       )
#     ),
#     tags$stop(
#       offset = "100%",
#       tags$animate(
#         attributeName = "stop-color",
#         values = "var(--colour_3); var(--colour_2); var(--colour_1); var(--colour_2); var(--colour_3)",
#         dur = "30s",
#         repeatCount = "indefinite"
#       ),
#       tags$animate(
#         attributeName = "stop-opacity",
#         values = "var(--opacity_end); var(--opacity_mid); var(--opacity_start); var(--opacity_mid); var(--opacity_end)",
#         dur = "30s",
#         repeatCount = "indefinite"
#       )
#     )
    )

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
          paste0('
           :root{
             --stroke: 0.01px;',
              css_opacity_var,
              css_colour_var,'
           }
           line {
            stroke-width: var(--stroke);
            }
           ')),
           tags$defs(
             tags$linearGradient(
               id = "gradient-opacity1", x1 = "0%", y1 = "0%", x2 = "100%", y2 = "0%", line_gradients
             ),
             tags$linearGradient(
               id = "gradient-opacity2", x1 = "0%", y1 = "0%", x2 = "0%", y2 = "100%", line_gradients
             ),
             tags$linearGradient(
               id = "gradient-opacity3", x1 = "100%", y1 = "0%", x2 = "0%", y2 = "0%", line_gradients
             ),
             tags$linearGradient(
               id = "gradient-opacity4", x1 = "0%", y1 = "100%", x2 = "0%", y2 = "0%", line_gradients
           )),
         elements))

  })



  observe(patterns$line <- svg_pattern())

  })
}
