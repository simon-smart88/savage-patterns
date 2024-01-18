library(shiny)
library(htmltools)
library(glue)

ui <- fluidPage(
  sidebarPanel(
  numericInput("space", "space between", value = 10),
  numericInput("radius", "radius", value = 30),
  numericInput("reps", "reps", value = 25),
  numericInput("breath", "change in radius", value = 20),
  numericInput("speed", "animation speed", value = 30),
  numericInput("stroke", "stroke width", value = 0.5, step = 0.1),
  downloadButton("download")
  ),
  mainPanel(uiOutput("svgout"))

)

server <- function(input, output, session){

  svg_circle <- function(x, space, speed, radius, breath, stroke){
    breath <- breath/100
    tags$circle(cx = 30+(x[1]*space),
                cy = 30+(x[2]*space),
                stroke = "black",
                fill = "none",
                `stroke-width` = glue("{stroke}px"),
                tags$animate(attributeName = "r",
                             values = glue("{radius*(1-breath)};{radius*(1+breath)};{radius*(1-breath)}"),
                             dur = glue("{speed}s"),
                             repeatCount = "indefinite"))
  }

  svg_pattern <- reactive({
  top_corner <- input$radius*(1+(input$breath/100)) + 30
  bottom_corner <- (30+(input$reps * input$space)) - 2*(top_corner)

  reps <- input$reps
  elements <- matrix(c(rep(seq(1:reps),reps),rep(seq(1:reps),each=reps)), nrow = reps^2, byrow = F)

  elements <- apply(elements, 1, svg_circle, space = input$space,
                    speed = input$speed,
                    radius = input$radius,
                    breath = input$breath,
                    stroke = input$stroke)

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
