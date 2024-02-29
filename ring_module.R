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
  tags$label("Colours"),
  gradientInputUI(ns("cols")),
  actionButton(ns("random"), "Randomise"),
  actionButton(ns("update"), "Update")
  )
}

ring_module_server <- function(id, patterns){
  moduleServer(id, function(input, output, session) {

  observeEvent(input$update, {
        tags$script(HTML(
      "circle_15.style.fill = 'skyblue';"
    ))
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

    gradient <- callModule(gradientInput, "cols", init_cols = c(10, 50, 70))
    #create colour gradient from result
    #sample from gradient depending on reps

    #function to generate svg circle
    svg_circle <- function(x, space, speed, radius, breath, stroke, colours, reps, bulge){
      breath <- breath/100
      n_cols <- length(gradient$result()$col)
      radius_bulge <- (x[3]/reps*2) * (bulge/10)
      tags$circle(id = glue("circle_{x[3]}"),
                  cx = radius+(x[1]*space),
                  cy = radius+(x[2]*space),
                  fill = "none",
                  `stroke-width` = glue("{stroke}px"),
                  # animate the radius
                  tags$animate(attributeName = "r",
                               values = glue("{(radius*(1-breath))+radius_bulge};
                                            {(radius*(1+breath))+radius_bulge};
                                            {(radius*(1-breath))+radius_bulge}"),
                               dur = glue("{speed}s"),
                               repeatCount = "indefinite"),
                  #animate the colours using a negative offset of the bulge as 'begin' to create the range in colours
                  tags$animate(attributeName = "stroke",
                               values = paste(c(gradient$result()$col,rev(gradient$result()$col[1:n_cols - 1])), collapse = ";"),
                               dur = glue("{speed}s"),
                               repeatCount = "indefinite",
                               begin = glue("{-(speed/reps)*x[3]}s"))
      )
    }

    # generate the pattern
    svg_pattern <- reactive({

      #view port to crop borders
      top_corner <- 2 * input$radius
      #top_corner <- input$radius+ ((input$radius*(1+input$breath))+input$bulge)
      bottom_corner <- (input$reps * input$space) - top_corner

      #create a matrix of sequences
      reps <- input$reps
      low_half <- (reps-1)/2
      high_half <- (reps+1)/2
      elements <- matrix(c(#column and row indices
        rep(seq(1:reps),reps), # 123,123,123
        rep(seq(1:reps),each=reps), # 111,222,333
        #produces the bulge with higher values at the centre of the matrix
        rep(c(0:low_half, (low_half-1):0), reps) + #01210
          c(rep(seq(1:high_half),each=reps), rev(rep(seq(low_half:1), each=reps)))), #111,222,111
        nrow = reps^2, byrow = F)

      #apply the svg_circle function to the matrix
      elements <- apply(elements, 1, svg_circle,
                        space = input$space,
                        speed = input$speed,
                        radius = input$radius,
                        breath = input$breath,
                        stroke = input$stroke,
                        reps = input$reps,
                        colours = gradient$result()$col,
                        bulge = input$bulge)

      #create the final svg
      tagList(tags$svg(xmlns = "http://www.w3.org/2000/svg",
                       `xmlns:xlink`="http://www.w3.org/1999/xlink",
                       version="1.1",
                       viewBox = glue("{top_corner} {top_corner} {bottom_corner} {bottom_corner}"),
                       height="100%",
                       elements))
    })
    observe(patterns$ring <- svg_pattern())

  }
)}
