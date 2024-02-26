library(shiny)
library(htmltools)
library(glue)
library(colourpicker)
library(shinyjqui)
source("gradientInput.R")

ui <- fluidPage(
  title = "Savage",
        sidebarPanel(width = c(4,8),
            sliderInput("radius", "Radius", value = 30, step = 1, min = 10, max = 50),
            sliderInput("bulge", "Radius bulge", value = 10, step = 0.1, min = 0.1, max = 100),
            sliderInput("reps", "Repetitions", value = 25, step = 2, min = 11, max = 45),
            sliderInput("space", "Space between circles", value = 10, step = 1, min = 1, max = 30),
            sliderInput("breath", "Change in radius", value = 20, step = 1, min = 10, max = 30),
            sliderInput("speed", "Animation duration", value = 30, step = 1, min = 5, max = 60),
            sliderInput("stroke", "Line thickness", value = 0.5, step = 0.05, min = 0.1, max = 1),
            tags$label("Colours"),
            gradientInputUI("cols"),
            actionButton("random", "Randomise"),
            downloadButton("download")
        ),
        mainPanel(
            uiOutput("svgout")
            )
)

server <- function(input, output, session){
  
  observeEvent(input$random, {
    updateSliderInput(session, "radius", value = runif(1, 10, 50))
    updateSliderInput(session, "bulge", value = runif(1, 0.1, 100))
    updateSliderInput(session, "reps", value = as.integer(1 + (2 * runif(1, 5, 22))))
    updateSliderInput(session, "space", value = runif(1, 1, 30))
    updateSliderInput(session, "breath", value = runif(1, 10, 30))
    updateSliderInput(session, "speed", value = runif(1, 5, 60))
    updateSliderInput(session, "stroke", value = runif(1, 0.1, 1))
  })
  

  gradient <- callModule(gradientInput, "cols", init_cols = c(10, 50, 70))
  #create colour gradient from result
  #sample from gradient depending on reps

  #function to generate svg circle
  svg_circle <- function(x, space, speed, radius, breath, stroke, colours, reps, bulge){
    breath <- breath/100
    n_cols <- length(gradient$result()$col)
    radius_bulge <- (x[3]/reps*2) * (bulge/100)
    tags$circle(id = glue("circle_{x[3]}"),
                cx = radius+(x[1]*space),
                cy = radius+(x[2]*space),
                fill = "none",
                `stroke-width` = glue("{stroke}px"),
                # animate the radius
                tags$animate(attributeName = "r",
                             values = glue("{(radius*(1-breath))*radius_bulge};
                                            {(radius*(1+breath))*radius_bulge};
                                            {(radius*(1-breath))*radius_bulge}"),
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
  # shinyjs::html()
  # shinyjs::

  # c(rep(seq(1:high_half),each=reps), rev(rep(seq(low_half:1), each=reps)))

  # generate the pattern
  svg_pattern <- reactive({
  
  #view port to crop borders  
  top_corner <- input$radius*(1+(input$breath/100)) + input$radius
  bottom_corner <- (input$radius+(input$reps * input$space)) - (top_corner + input$radius + input$space) 

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
  tagList(tags$svg(viewbox = glue("{top_corner} {top_corner} {bottom_corner} {bottom_corner}"), elements))
  })

  #send to UI
  output$svgout <- renderUI({
    svg_pattern()
  })

  output$download <- downloadHandler(
    filename = function(){
      "your.svg"
    },
    content = function(file){
      write(as.character(tags$html(tags$body(svg_pattern()))), file)
    }
  )

}



shinyApp(ui, server)
