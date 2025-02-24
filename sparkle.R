library(shiny)
library(bslib)

# Define UI
ui <- page_navbar(
      layout_sidebar(
        sidebar = sidebar(
        accordion(
          multiple = FALSE,
          open = c("Pattern"),
          accordion_panel("Pattern",
            sliderInput("canvas_size", "Pixels per side", value = 100, min = 100, max = 1000, step = 5),
            sliderInput("pixel_percent", "Percentage of pixels to update", value = 1, min = 0.1, max = 100, step= 0.1)
          ),
          accordion_panel("Red",
            sliderInput("r_mean", "Red mean", value = 128, min = 1, max = 256, step = 1),
            sliderInput("r_amplitude", "Red amplitude", value = 1, min = 0, max = 128, step = 1),
            sliderInput("r_frequency", "Red frequency", value = 0.1, min = 0.01, max = 1, step = 0.01),
          ),
          accordion_panel("Green",
            sliderInput("g_mean", "Green mean", value = 128, min = 1, max = 256, step = 1),
            sliderInput("g_amplitude", "Green amplitude", value = 1, min = 0, max = 128, step = 1),
            sliderInput("g_frequency", "Green frequency", value = 0.1, min = 0.01, max = 1, step = 0.01),
          ),
          accordion_panel("Blue",
            sliderInput("b_mean", "Blue mean", value = 128, min = 1, max = 256, step = 1),
            sliderInput("b_amplitude", "Blue amplitude", value = 1, min = 0, max = 128, step = 1),
            sliderInput("b_frequency", "Blue frequency", value = 0.1, min = 0.01, max = 1, step = 0.01),
          ),
        ),
      sliderInput("alpha", "Transparency", value = 128, min = 1, max = 256, step = 1),
      plotOutput("sine_wave_plot"),
      width = "400px"

      # have a prob of being affected by colour rules
      # make rules to oscillate the colour ranges (and others)
      # https://www.bbc.co.uk/opensource/projects/project/peaks-js

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

        let r_mean = 128;
        let r_amplitude = 1;
        let r_frequency = 1;

        let g_mean = 128;
        let g_amplitude = 1;
        let g_frequency = 1;

        let b_mean = 128;
        let b_amplitude = 1;
        let b_frequency = 1;

        let phase = 0;

        let alpha = 128;
        let pixelUpdatePercent = 10;
        let imageData = ctx.createImageData(width, height);

        // Function to fill ImageData with random colors
        function fillRandomColors(imageData) {
            for (let i = 0; i < imageData.data.length; i += 4) {
                imageData.data[i] = Math.floor(Math.random() * r_range);
                imageData.data[i + 1] = Math.floor(Math.random() * g_range);
                imageData.data[i + 2] = Math.floor(Math.random() * b_range);
                imageData.data[i + 3] = alpha;
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
                imageData.data[index + 3] = alpha;
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
        $(document).on('input', '#r_mean', function() {
            r_mean = $('#r_mean').val();
        });
        $(document).on('input', '#r_amplitude', function() {
            r_amplitude = $('#r_amplitude').val();
        });
        $(document).on('input', '#r_frequency', function() {
            r_frequency = $('#r_frequency').val();
        });
        $(document).on('input', '#g_mean', function() {
            g_mean = $('#g_mean').val();
        });
        $(document).on('input', '#g_amplitude', function() {
            g_amplitude = $('#g_amplitude').val();
        });
        $(document).on('input', '#g_frequency', function() {
            g_frequency = $('#g_frequency').val();
        });
        $(document).on('input', '#b_mean', function() {
            b_mean = $('#b_mean').val();
        });
        $(document).on('input', '#b_amplitude', function() {
            b_amplitude = $('#b_amplitude').val();
        });
        $(document).on('input', '#b_frequency', function() {
            b_frequency = $('#b_frequency').val();
        });
        $(document).on('input', '#alpha', function() {
            alpha = $('#alpha').val();
        });


        // Function to update r_range with a sine wave
        function sine_waves() {
            const time = Date.now() / 1000;

            const r_sine = r_amplitude * Math.sin(2 * Math.PI * r_frequency * time + phase);
            r_range = Math.round(128 + r_sine);

            const g_sine = g_amplitude * Math.sin(2 * Math.PI * g_frequency * time + phase);
            g_range = Math.round(128 + g_sine);

            const b_sine = b_amplitude * Math.sin(2 * Math.PI * b_frequency * time + phase);
            b_range = Math.round(128 + b_sine);
        }

        setInterval(function() {
            sine_waves();
        }, 100);

      "))
    )
  )
)

server <- function(input, output, session) {

  output$sine_wave_plot <- renderPlot({
    time <- seq(0, 20, length.out = 1000)

    r_sine <- input$r_amplitude * sin(2 * pi * input$r_frequency * time) + 128 # input$r_mean
    g_sine <- input$g_amplitude * sin(2 * pi * input$g_frequency * time) + 128 # input$g_mean
    b_sine <- input$b_amplitude * sin(2 * pi * input$b_frequency * time) + 128 # input$b_mean

    plot(time, r_sine, type = "l", col = "red", lwd = 2,
         xlab = "Time (seconds)", ylab = "Amplitude",
         main = "RGB waveforms",
         ylim = c(0, 256))
    lines(time, g_sine, col = "green", lwd = 2)
    lines(time, b_sine, col = "blue", lwd = 2)
  })

}

# Run the app
shinyApp(ui, server)
