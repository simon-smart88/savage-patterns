test_module_ui <- function(id){

}


test_module_server <- function(id, patterns, module){
  moduleServer(id, function(input, output, session) {

    svg_line <- function(x, speed){
      tags$line(x1 = x[1], x2 = x[2], y1 = x[3], y2 = x[4])
    }





    })
}
