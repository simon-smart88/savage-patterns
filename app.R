library(shiny)
library(htmltools)
library(glue)
library(colourpicker)
library(shinyjqui)
source("gradientInput.R")

ui <- fluidPage(
        sidebarPanel(
            numericInput("space", "space between", value = 10),
            numericInput("radius", "radius", value = 30),
            sliderInput("reps", "reps", value = 25, step = 2, min = 11, max = 45),
            numericInput("breath", "change in radius", value = 20),
            numericInput("speed", "animation speed", value = 30),
            numericInput("stroke", "stroke width", value = 0.5, step = 0.1),
            gradientInputUI("cols"),
            downloadButton("download")
        ),
        mainPanel(
            uiOutput("svgout")
            )
)

server <- function(input, output, session){

  gradient <- callModule(gradientInput, "cols", init_cols = c(10, 50, 70))
  #create colour gradient from result
  #sample from gradient depending on reps

  svg_circle <- function(x, space, speed, radius, breath, stroke, colours, reps){
    breath <- breath/100
    n_cols <- length(gradient$result()$col)
    tags$circle(id = glue("circle_{x[3]}"),
                cx = 30+(x[1]*space),
                cy = 30+(x[2]*space),
                fill = "none",
                `stroke-width` = glue("{stroke}px"),
                tags$animate(attributeName = "r",
                             values = glue("{radius*(1-breath)};{radius*(1+breath)};{radius*(1-breath)}"),
                             dur = glue("{speed}s"),
                             repeatCount = "indefinite"),
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

  svg_pattern <- reactive({
  top_corner <- input$radius*(1+(input$breath/100)) + 30
  bottom_corner <- (30+(input$reps * input$space)) - (top_corner)

  reps <- input$reps
  low_half <- (reps-1)/2
  high_half <- (reps+1)/2
  elements <- matrix(c(rep(seq(1:reps),reps), # 123,123,123
                       rep(seq(1:reps),each=reps), # 111,222,333
                       rep(c(0:low_half, (low_half-1):0), reps) + #01210
                         c(rep(seq(1:high_half),each=reps), rev(rep(seq(low_half:1), each=reps)))), #111,222,111
                     nrow = reps^2, byrow = F)

  # browser()

  elements <- apply(elements, 1, svg_circle,
                    space = input$space,
                    speed = input$speed,
                    radius = input$radius,
                    breath = input$breath,
                    stroke = input$stroke,
                    reps = input$reps,
                    colours = gradient$result()$col)

  print(c(top_corner,bottom_corner))

  tagList(tags$svg(viewbox = glue("{top_corner} {top_corner} {bottom_corner} {bottom_corner}"), elements))
  })


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
