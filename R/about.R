about_module_ui <- function(id){
tagList(
  tags$head(tags$style(
    '
    .caption {
      margin-top: 0px;
      font-style: italic;
      color: #555;
      text-align: right;
    }
    '
  )),
  h1("About"),
    markdown("I have been making art for over 20 years now and this app is the latest iteration of many
             different methods that I've used over the years as my knowledge of different technologies has changed.
             This is a brief summary of the methods used and then at the end a basic explanation of how this works."),
  h2("Painting"),
    markdown("Starting at school I did a lot of painting, this one is a pretty blatant imitation of
            <a href= https://www.tate.org.uk/art/artworks/riley-nataraja-t06859 target='_blank'>Bridget Riley\'s Nataraja (1993)</a>.
            The difficulty with painting is that it is slow, expensive, messy and you need an army of technicians to do it well."),
    img(src = "painting.JPG"),
    p("Untitled (2003)", class = "caption"),
  h2("Photoshop"),
    markdown("Still at school I started using the only drawing technology I knew about to make patterns - Photoshop.
              This was definitely an improvement on painting, but I was still drawing each shape by hand and a pixel-based
              system was wholly inappropriate when I wanted sharp lines. This is a screenshot of part of a piece imitating
              pieces like <a href=https://www.wikiart.org/en/victor-vasarely/vega-200-1968 target='_blank'>Victor Vasarely's Vega (1968)</a> to demonstrate
              the problem."),
    img(src = "photoshop.png"),
    p("Untitled (2003)", class = "caption"),
  h2("Illustrator"),
    markdown("Finding out about the vector-based Illustrator was a revelation! Now I could create huge pieces with sharp
             lines to my hearts content! But I was still painstakingly drawing a lot of shapes by hand and copy and pasting
             to create more complex pieces. Whilst the file sizes were smaller than in Photoshop, this one still contains
             80,000 lines of code to generate it."),
    img(src = "illustrator.svg"),
  h2("Flash"),
    markdown("At some point I learnt about Flash and for the first time could make animations which definitely took things to
             the next level. I can't remember much about how I created them any more but it was still laborious and
             since then Flash has been deprecated. Shout out to <a href=https://ruffle.rs/ target='_blank'>Ruffle</a>
             for bringing this back from the dead!"),
  tags$head(
    tags$script(src = "https://unpkg.com/@ruffle-rs/ruffle")
  ),
    tags$object(id = "swf-container", data = "flash.swf", type = "application/x-shockwave-flash", width = "100%", height = "auto",
                tags$param(name="allowScriptAccess", value = "always")
  ),
  h2("SVGs"),
    markdown("My web designer brother kindly pointed out to me that scalable vector graphics (.svg) would be a good format to use and
             it turned out I could export them from Illustrator. Being able to see the files as code was another revelation and I
             started to learn about the syntax for creating SVGs myself rather than using Illustator."),
  h3("Animations"),
    markdown("Looking around the web I found some great resources made by
    <a href=https://www.petercollingridge.co.uk/ target='_blank'>Peter Collingridge</a>
    for animating SVGs using javascript. I had no idea what I was doing, but not I could copy some code into the file and make things
    spin. The possibities were endless!"),
    tags$object(type="image/svg+xml", data="scripting.svg"),
    p("Circle grid2 col (copy) script (2012)", class = "caption"),
  h3("Hand-coded"),
    markdown("Next up I started creating SVGs from scratch in a text editor which was quite laborious but easier than drawing
             lines by hand. I also started to learn about styling SVGs using cascading style sheets (CSS) and using animations
             in SVGs directly rather than using javascript."),
    img(src = "hand_coded.svg"),
    p("Old school (2012)", class = "caption"),
  h3("Python"),
    markdown("By this point I'd been writing scripts to analyse experimental data for a few years and was beginning to use python to
             create graphs. I figured that I could speed up creating SVGs by writing python scripts. Looking back 10 years later, my
             attempts are hilariously basic as I wasn't using any libraries and not even writing the results to a file, but copying
             the output from the console."),
    img(src = "python.svg"),
    p("Shades of the rainbow (2014)", class = "caption"),
  h2("Now")

)

}
