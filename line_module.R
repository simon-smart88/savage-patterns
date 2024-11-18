line_module_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
    sliderInput(ns("outer"), "Outer", value = 9, step = 1, min = 6, max = 20),
    sliderInput(ns("inner"), "Inner", value = 3, step = 1, min = 2, max = 11),
    sliderInput(ns("inner_size"), "Inner size", value = 50, step = 1, min = 10, max = 90)

  )
}

line_module_server <- function(id, patterns){
  moduleServer(id, function(input, output, session) {
  

    svg_line <- function(x){
      tags$line(x1 = x[1], x2 = x[2], y1 = x[3], y2 = x[4])
    }
      
  
  svg_pattern <- reactive({
    
    outer_points <- input$outer
    inner_points <- input$inner
    
    inner_offset <- (outer_points - inner_points) / 2
    
    outer_x <- c(seq(0, outer_points - 1), rep(c(0, outer_points - 1), outer_points - 2), seq(0, outer_points - 1))
    outer_y <- c(rep(0, outer_points), rep(seq(1, outer_points - 2), each = 2), rep(outer_points - 1, outer_points))
    
    inner_x <- c(seq(0, inner_points - 1), rep(c(0, inner_points - 1), inner_points - 2), seq(0, inner_points - 1)) + inner_offset
    inner_y <- c(rep(0, inner_points), rep(seq(1, inner_points - 2), each = 2), rep(inner_points - 1, inner_points)) + inner_offset
    
    old_inner_range <- diff(range(inner_y))
    new_inner_range <- input$outer * (input$inner_size / 100)
    
    inner_scale <- new_inner_range / old_inner_range
    inner_mean <- mean(inner_x)
    
    inner_x <- (inner_x - inner_mean) * inner_scale + inner_mean
    inner_y <- (inner_y - inner_mean) * inner_scale + inner_mean
    
    x1 <- rep(outer_x, length(inner_x))
    x2 <- rep(inner_x, each = length(outer_x))
    
    y1 <- rep(outer_y, length(inner_y))
    y2 <- rep(inner_y, each = length(outer_y))
    
    element_mat <- matrix(c(x1, x2, y1, y2), ncol = 4)
    # browser()
    elements <- apply(element_mat, 1, svg_line)
    
    top_corner <- 0
    bottom_corner <- input$outer
    
    
    #create the final svg
    tagList(tags$svg(xmlns = "http://www.w3.org/2000/svg",
                     `xmlns:xlink`="http://www.w3.org/1999/xlink",
                     version="1.1",
                     viewBox = glue("{top_corner} {top_corner} {bottom_corner} {bottom_corner}"),
                     height = "100%",
                     id = "pattern",
                     tags$style(paste0("

                                line { stroke-width: 0.01px;
                                      stroke: black}

                       ")),
                     
                     elements))
    
  })
  
  observe(patterns$line <- svg_pattern())
  
  })
}