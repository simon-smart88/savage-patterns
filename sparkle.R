library(shiny)
library(htmltools)

# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("canvas_size", "Pixels per side", value = 100, min = 100, max = 1000, step = 5),
      sliderInput("pixel_percent", "Percentage of pixels to update", value = 1, min = 0.1, max = 100, step= 0.1),
      sliderInput("r_range", "Red range", value = 128, min = 1, max = 256, step = 1),
      sliderInput("g_range", "Green range", value = 128, min = 1, max = 256, step = 1),
      sliderInput("b_range", "Blue range", value = 128, min = 1, max = 256, step = 1),
      sliderInput("alpha", "Transparency", value = 128, min = 1, max = 256, step = 1),
      sliderInput("colour_step", "Colour speed", value = 5, min = 1, max = 25, step = 1)

      # have a prob of being affected by colour rules
      # make rules to oscillate the colour ranges (and others)

    ),
    mainPanel(
      tags$div(
        id = "canvas-container",
        style = "width: 95vh; height: 95vh; overflow: hidden; border: 1px solid black;",
        tags$canvas(
          id = "myCanvas",
          width = 100,
          height = 100,
          style = "width: 100%; height: 100%; image-rendering: pixelated;"
        )
      ),
      tags$script(HTML("
        const canvas = document.getElementById('myCanvas');
        const ctx = canvas.getContext('2d');

        let width = 100;
        let height = 100;
        let r_range = 128;
        let g_range = 128;
        let b_range = 128;
        let alpha = 128;
        let pixelUpdatePercent = 10;
        let imageData = ctx.createImageData(width, height);

        // Function to fill ImageData with random colors
        function fillRandomColors(imageData) {
            for (let i = 0; i < imageData.data.length; i += 4) {
                imageData.data[i] = Math.floor(Math.random() * r_range);
                imageData.data[i + 1] = Math.floor(Math.random() * g_range);
                imageData.data[i + 2] = Math.floor(Math.random() * b_range);
                imageData.data[i + 3] = Math.floor(Math.random() * alpha);
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

                imageData.data[index] = Math.floor(Math.random() * r_range);
                imageData.data[index + 1] = Math.floor(Math.random() * g_range);
                imageData.data[index + 2] = Math.floor(Math.random() * b_range);
                imageData.data[index + 3] = Math.floor(Math.random() * alpha);
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
        $(document).on('input', '#pixel_percent', function() {
            pixelUpdatePercent = $('#pixel_percent').val();
        });

        // Update RGB values
        $(document).on('input', '#r_range', function() {
            r_range = $('#r_range').val();
        });
        $(document).on('input', '#g_range', function() {
            g_range = $('#g_range').val();
        });
        $(document).on('input', '#b_range', function() {
            b_range = $('#b_range').val();
        });
        $(document).on('input', '#alpha', function() {
            alpha = $('#alpha').val();
        });

        // Track currently pressed keys
        const pressedKeys = new Set();

        // Add keys to the set on keydown
        $(document).on('keydown', function(e) {
            pressedKeys.add(e.key);
        });

        // Remove keys from the set on keyup
        $(document).on('keyup', function(e) {
            pressedKeys.delete(e.key);
        });

        // Periodically check for pressed keys and trigger actions
        setInterval(function() {
          ['z', 'x', 'o', 'p', 'r', 't', 'g', 'h', 'b', 'n'].forEach(key => {
            if (pressedKeys.has(key)) {
              Shiny.onInputChange(`${key}_key`, new Date().getTime());
            }
          });
        }, 50);

      "))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$z_key, {
    updateSliderInput(session, "canvas_size", value = input$canvas_size - 20)
  })
  observeEvent(input$x_key, {
    updateSliderInput(session, "canvas_size", value = input$canvas_size + 20)
  })
  observeEvent(input$o_key, {
    updateSliderInput(session, "pixel_percent", value = input$pixel_percent - 1)
  })
  observeEvent(input$p_key, {
    updateSliderInput(session, "pixel_percent", value = input$pixel_percent + 1)
  })
  observeEvent(input$r_key, {
    updateSliderInput(session, "r_range", value = input$r_range - input$colour_step)
  })
  observeEvent(input$t_key, {
    updateSliderInput(session, "r_range", value = input$r_range + input$colour_step)
  })
  observeEvent(input$g_key, {
    updateSliderInput(session, "g_range", value = input$g_range - input$colour_step)
  })
  observeEvent(input$h_key, {
    updateSliderInput(session, "g_range", value = input$g_range + input$colour_step)
  })
  observeEvent(input$b_key, {
    updateSliderInput(session, "b_range", value = input$b_range - input$colour_step)
  })
  observeEvent(input$n_key, {
    updateSliderInput(session, "b_range", value = input$b_range + input$colour_step)
  })
}

# Run the app
shinyApp(ui, server)
