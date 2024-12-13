about_module_ui <- function(id){
tagList(
  h1("About"),
  h2("Painting"),
  h2("Photoshop"),
  h2("Illustrator"),
  h2("Flash"),
  tags$head(
    tags$script(src = "https://unpkg.com/@ruffle-rs/ruffle")
  ),
  tags$div(
    tags$object(id = "swf-container", data = "flash.swf", type = "application/x-shockwave-flash", width = "600", height = "600",
                tags$param(name="allowScriptAccess", value = "always"))
  ),

  h2("SVGs"),
  h3("Animations"),
  tags$a("https://www.petercollingridge.co.uk/"),
  h3("Hand-coded"),
  h3("Python"),
  h2("Now")
)

}
