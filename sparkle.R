library(shiny)
library(htmltools)

# Define UI
ui <- fluidPage(
  titlePanel("Pixelated Canvas with Scaling"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("canvas_size", "Pixels per side", value = 100, min = 100, max = 1000, step = 5,
                  animate = animationOptions(interval = 100, playButton = "Play", pauseButton = "Pause" )),

      sliderInput("pixel_update_percent", "Percentage of pixels to update", value = 10, min = 0, max = 100,
                  animate = animationOptions(interval = 100, playButton = "Play", pauseButton = "Pause" ))
    ),
    mainPanel(
      tags$div(
        id = "canvas-container",
        style = "width: 1000px; height: 1000px; overflow: hidden; border: 1px solid black;", # Remove scrollbar
        tags$canvas(
          id = "myCanvas",
          width = 100,  # Intrinsic canvas width
          height = 100, # Intrinsic canvas height
          style = "width: 100%; height: 100%; image-rendering: pixelated;" # Scale to container
        )
      ),
      tags$script(HTML("
                const canvas = document.getElementById('myCanvas');
                const ctx = canvas.getContext('2d');

                let width = 100;
                let height = 100;
                let pixelUpdatePercent = 10; // Percentage of pixels to update
                let imageData = ctx.createImageData(width, height);

                // Function to fill ImageData with random colors
                function fillRandomColors(imageData) {
                    for (let i = 0; i < imageData.data.length; i += 4) {
                        imageData.data[i] = Math.floor(Math.random() * 256);
                        imageData.data[i + 1] = Math.floor(Math.random() * 256);
                        imageData.data[i + 2] = Math.floor(Math.random() * 256);
                        imageData.data[i + 3] = 255;
                    }
                }

                // Initialize ImageData with random colors
                fillRandomColors(imageData);
                ctx.putImageData(imageData, 0, 0);

                // Function to resize the canvas (intrinsic dimensions)
                function resizeCanvas(newWidth, newHeight) {
                    width = newWidth;
                    height = newHeight;
                    canvas.width = width;
                    canvas.height = height;

                    // Reinitialize ImageData with new dimensions
                    imageData = ctx.createImageData(width, height);
                    fillRandomColors(imageData);
                    ctx.putImageData(imageData, 0, 0);
                }

                // Function to update random pixels
                function updateRandomPixels() {
                    const totalPixels = width * height;
                    const pixelsToUpdate = Math.round((pixelUpdatePercent / 100) * totalPixels);

                    for (let n = 0; n < pixelsToUpdate; n++) {
                        const x = Math.floor(Math.random() * width);
                        const y = Math.floor(Math.random() * height);
                        const index = (y * width + x) * 4;

                        imageData.data[index] = Math.floor(Math.random() * 256);
                        imageData.data[index + 1] = Math.floor(Math.random() * 256);
                        imageData.data[index + 2] = Math.floor(Math.random() * 256);
                        imageData.data[index + 3] = 255;
                    }

                    ctx.putImageData(imageData, 0, 0);
                    requestAnimationFrame(updateRandomPixels);
                }

                // Start the pixel update loop
                updateRandomPixels();

                // Resize canvas instantly when sliders change
                $(document).on('input', '#canvas_size', function() {
                    const newWidth = $('#canvas_size').val();
                    const newHeight = $('#canvas_size').val();
                    resizeCanvas(newWidth, newHeight);
                });

                // Update pixel update percentage
                $(document).on('input', '#pixel_update_percent', function() {
                    pixelUpdatePercent = $('#pixel_update_percent').val();
                });
            "))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # No server-side logic needed for this example
}

# Run the app
shinyApp(ui, server)
