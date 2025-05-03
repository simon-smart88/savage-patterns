about_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(
      '.caption {
        margin-top: 0px;
        font-style: italic;
        color: #555;
        text-align: right;
      }'),
      tags$script(src = "https://unpkg.com/@ruffle-rs/ruffle")
      ),
    h1("About"),
      markdown("I have been making art for over 20 years now and this app is the latest iteration of many
               different methods that I've used over the years as my knowledge of different technologies has changed.
               This is a brief summary of the methods used and then at the end a basic explanation of how this works."),
    h2("Painting"),
      markdown("Starting at school I did a lot of painting, this one is a pretty blatant imitation of
              <a href= https://www.tate.org.uk/art/artworks/riley-nataraja-t06859 target='_blank'>Bridget Riley\'s Nataraja (1993)</a>.
              The difficulty with painting is that it is slow, expensive, messy and you need an army of technicians to do it well."),
      img(src = "https://raw.githubusercontent.com/simon-smart88/savage-patterns/main/images/painting.JPG"),
      p("Untitled (2003)", class = "caption"),
    h2("Paint"),
    markdown("Putting this together, I found this forgotten piece buried on my hard drive and I presumably made it in Paint based on it being
             a bitmap. It must be one of the earliest pieces of digital art I ever made and at that point Paint was the best technology I had.
             Paint was obviously not well-suited to the kinds of patterns I wanted to create though."),
    img(src = "https://raw.githubusercontent.com/simon-smart88/savage-patterns/main/images/blue red yello.bmp"),
    p("Blue red yello (2003)", class = "caption"),
    h2("Photoshop"),
      markdown("At school I started using the Photoshop which was definitely an improvement on both physical painting and digital Painting,
                but I was still drawing each shape by hand and a pixel-based system was wholly inappropriate when I wanted sharp lines.
                This is a screenshot of part of a piece imitating pieces like
                <a href=https://www.wikiart.org/en/victor-vasarely/vega-200-1968 target='_blank'>Victor Vasarely's Vega (1968)</a>
                to demonstrate the problem."),
      img(src = "https://raw.githubusercontent.com/simon-smart88/savage-patterns/main/images/photoshop.png"),
      p("2 (2007)", class = "caption"),
    h2("Illustrator"),
      markdown("After finishing school, I found out about the vector-based Illustrator and it was a revelation! Now I could
               create huge pieces with sharp lines to my hearts content! But I was still painstakingly drawing a lot of shapes
               by hand and copy, pasting and transforming to create more complex pieces. Whilst the file sizes were smaller than
               in Photoshop, I was mostly saving as pdfs which are brilliant as you can see below. I really wanted to animate them but
               didn't know how (I actually screenshotted some to create videos that seem to have been lost)."),
    tags$div(class = "square_container",
      tags$iframe(
        src = "illustrator.pdf",
        style = "width: 100%; height: 100%; border: none;"
      )
    ),
    p("Big2lines sq4 zoom3 (2008)", class = "caption"),
    h2("Flash"),
      markdown("At some point I learnt about Flash and for the first time could make animations which definitely took things to
               the next level. I can't remember much about how I created them any more but it was still laborious and
               since then Flash has been deprecated. Shout out to <a href=https://ruffle.rs/ target='_blank'>Ruffle</a>
               for bringing this back from the dead!"),
    tags$div(class = "square_container",
      tags$object(id = "swf-container", data = "https://raw.githubusercontent.com/simon-smart88/savage-patterns/main/images/flash.swf", type = "application/x-shockwave-flash", width = "100%", height = "100%",
                  tags$param(name="allowScriptAccess", value = "always"))
    ),
      p("Oval waves of blue2 half (2008)", class = "caption"),
    h2("SVGs"),
      markdown("My web designer brother kindly pointed out to me that scalable vector graphics (.svg) would be a good format to use and
               it turned out I could export them from Illustrator. This piece still contains 80,000 lines of code to generate it through.
               Being able to see the files as code was another revelation and I started to learn about the syntax for creating SVGs
               myself rather than using Illustator."),

      img(src = "https://raw.githubusercontent.com/simon-smart88/savage-patterns/main/images/illustrator.svg"),
      p("Rotated grid (2011)", class = "caption"),

    h3("Animations"),
      markdown("Looking around the web I found some great resources made by
      <a href=https://www.petercollingridge.co.uk/ target='_blank'>Peter Collingridge</a>
      for animating SVGs using javascript. I had no idea what I was doing, but now I could copy some code into the file and make things
      spin. The possibilities were endless!"),
      tags$object(type="image/svg+xml", data="scripting.svg"),
      p("Circle grid2 col (copy) script (2012)", class = "caption"),
    h3("Hand-coded"),
      markdown("Next up I started creating SVGs from scratch in a text editor which was quite laborious but easier than drawing
               lines by hand. I also started to learn about styling SVGs using cascading style sheets (CSS) and using animations
               in SVGs directly rather than using javascript."),
      img(src = "https://raw.githubusercontent.com/simon-smart88/savage-patterns/main/images/hand_coded.svg"),
      p("Old school (2012)", class = "caption"),
    h3("Python"),
      markdown("By this point I'd been writing scripts to analyse experimental data for a few years and was beginning to use python to
               create graphs. I figured that I could speed up creating SVGs by writing python scripts. Looking back 10 years later, my
               attempts are hilariously basic as I wasn't using any libraries and not even writing the results to a file, but copying
               the output from the console."),
      uiOutput(ns("python")),
      img(src = "https://raw.githubusercontent.com/simon-smart88/savage-patterns/main/images/python.svg"),
      p("Shades of the rainbow (2014)", class = "caption"),
    h2("Now"),
      markdown("I'd had a long hiatus where I hadn't done anything new for almost 10 years until 2024, when I noticed the
               <a href='https://bsky.app/search?q=%23rtistry' target='_blank'>#Rtistry tag</a> on social media which got me thinking
               about what it might be possible to achieve. The app is built with
               <a href=https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/ target='_blank'>R shiny</a>
               which is mainly used for data-centric applications, but the tools allow creating any html elements you like, including
               svg. The basic principle is to create a matrix containing a pattern and then apply that to a function which generates
               the svg. It uses css variables that are updated using javascript to adjust the colours on-the-fly and javascript to
               capture the svg as presented to the user to download.
               <a href=https://github.com/simon-smart88/savage-patterns target='_blank'>The source code is on GitHub.</a>"),
    br(),
    br(),
    br()
  )
}

about_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    python_code <- paste(readLines("shades of the rainbow.py"), collapse = "\n")

    output$python <- renderUI({
      tags$details(
        tags$summary("Click to see the horrors for yourself"),
        tags$pre(
          tags$code(python_code)
        )
      )
    })

  })
}

