square_module_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
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


    css_colours <- reactive({
      n_cols <- 3 + input$add_colour
      ids <- paste0("colour_", 1:n_cols)
      steps <- seq(0, 100, length.out = (2*n_cols)-1)
      a <- paste(lapply(seq_along(ids), function(i) {glue::glue("{steps[i]}% {{fill: input[['{ids[i]}']] }}")}), collapse= ' ')
      #print(a)
      a
    })

    # generate the pattern
    svg_pattern <- reactive({

      tagList(tags$svg(xmlns = "http://www.w3.org/2000/svg",
                       `xmlns:xlink`="http://www.w3.org/1999/xlink",
                       version="1.1",
                       viewBox = glue("0 0 100 100"),
                       height="100%",
                       tags$style(paste0("
                                .clr {animation: col 3s linear infinite;}
                                @keyframes col {",css_colours(),"}")),
                      #  tags$style("
                      # .clr {attributeName: 'width';
                      #       values: '40;80;40';
                      #       repeatCount: 'indefinite';}
                      #  "),
                       tags$rect(class="clr",
                                 x = 10,
                                 y = 10,
                                 height = 50,
                                 width = 30,
                                 `stroke-width` = 2,
                                 # fill = 'red',
                                 stroke='black')
                       ))
    })
    observe(patterns$square <- svg_pattern())

  }
  )}
