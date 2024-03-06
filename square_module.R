square_module_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("update"), "Update")
  )
}

square_module_server <- function(id, patterns){
  moduleServer(id, function(input, output, session) {

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

    # generate the pattern
    svg_pattern <- reactive({

      tagList(tags$svg(xmlns = "http://www.w3.org/2000/svg",
                       `xmlns:xlink`="http://www.w3.org/1999/xlink",
                       version="1.1",
                       viewBox = glue("0 0 100 100"),
                       height="100%",
                       tags$style("
                                .clr {animation: col 3s linear infinite;}

                                @keyframes col {
                                0% {fill: #186A91}
                                25% {fill: #B4E2E1}
                                50% {fill: #ECC950}
                                75% {fill: #B4E2E1}
                                100% {fill: #186A91}
                                }
                       "),
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
