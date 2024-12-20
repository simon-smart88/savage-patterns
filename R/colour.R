colour_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/@jaames/iro@5"),
      tags$style(HTML("
      .colorPicker {
        margin: 20px;
        width: 300px;
      }
      .swatch {
        width: 30px;
        height: 30px;
        display: inline-block;
        margin-right: 10px;
        cursor: pointer;
      }
    "))
    ),
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
                  "Bright" = "bright", "Dark" = "dark")),

      div(id = ns("colorPickerContainer"), class = "colorPicker"),
    )
}

colour_server <- function(id, invalidate_color) {
  moduleServer(id, function(input, output, session) {

    observe({
      runjs(paste0('
      const colorPicker = new iro.ColorPicker("#',session$ns("colorPickerContainer"),'", {
        width: 300,
        colors: [
          "',random()[1],'",
          "',random()[2],'",
          "',random()[3],'"
        ],
        handleRadius: 9,
        borderWidth: 1,
        borderColor: "#fff"
      });

      const initialColors = [
        "',random()[1],'",
        "',random()[2],'",
        "',random()[3],'"
      ];

      const selectedColors = [...initialColors];

      colorPicker.on(["mount", "color:change"], function(color){
        const hexString = color.hexString;
        const index = color.index;
        selectedColors[index] = hexString;
        Shiny.setInputValue("',session$ns("colour"),'", selectedColors);
  });
    '))
    })

    observe(print(input$colour))

    random <- reactive({
      invalidate_color()
      randomcoloR::randomColor(count = 3, hue = input$hue, luminosity = input$lumin)
    })

    list(
      colour_1 = reactive(input$colour[1]),
      colour_2 = reactive(input$colour[2]),
      colour_3 = reactive(input$colour[3])
    )
  })
}

