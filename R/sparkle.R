library(shiny)
library(bslib)

sparkle_module_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
      layout_sidebar(
        sidebar = sidebar(
        markdown("This is a work in progress, inspired by a <a href= http://www.timhead.net/art/art2000/81sce.htm target='_blank'>Tim Head piece</a> that was created by
                 <a href= https://glowinthedark.co.uk/ target='_blank'>Paul Harter</a>. It uses Javascript to draw randomly-coloured squares on an HTML canvas."),
        accordion(
          multiple = FALSE,
          open = c("Pattern"),
          accordion_panel("Pattern",
            sliderInput(ns("canvas_size"), "Pixels per side", value = 100, min = 100, max = 2000, step = 5),
            sliderInput(ns("pixel_percent"), "Percentage of pixels to update", value = 1, min = 0.1, max = 100, step= 0.1)
          ),
          accordion_panel("Red",
            sliderInput(ns("r_mean"), "Red mean", value = 128, min = 1, max = 256, step = 1),
            sliderInput(ns("r_amplitude"), "Red amplitude", value = 1, min = 0, max = 128, step = 1),
            sliderInput(ns("r_frequency"), "Red frequency", value = 0.1, min = 0.01, max = 10, step = 0.01),
          ),
          accordion_panel("Green",
            sliderInput(ns("g_mean"), "Green mean", value = 128, min = 1, max = 256, step = 1),
            sliderInput(ns("g_amplitude"), "Green amplitude", value = 1, min = 0, max = 128, step = 1),
            sliderInput(ns("g_frequency"), "Green frequency", value = 0.1, min = 0.01, max = 1, step = 0.01),
          ),
          accordion_panel("Blue",
            sliderInput(ns("b_mean"), "Blue mean", value = 128, min = 1, max = 256, step = 1),
            sliderInput(ns("b_amplitude"), "Blue amplitude", value = 1, min = 0, max = 128, step = 1),
            sliderInput(ns("b_frequency"), "Blue frequency", value = 0.1, min = 0.01, max = 1, step = 0.01),
          ),
        ),
      sliderInput(ns("alpha"), "Transparency", value = 128, min = 1, max = 256, step = 1),
      plotOutput(ns("sine_wave_plot")),
      width = "400px"

      # have a prob of being affected by colour rules
      # make rules to oscillate the colour ranges (and others)
      # https://www.bbc.co.uk/opensource/projects/project/peaks-js

    ),
    mainPanel(
      tags$div(
        id = "canvas-container",
        style = "width: 90vh; height: 90vh; overflow: hidden; border: 1px solid black;",
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
        $(document).on('input', '#sparkle-canvas_size', function() {
            const newWidth = $('#sparkle-canvas_size').val();
            const newHeight = $('#sparkle-canvas_size').val();
            resizeCanvas(newWidth, newHeight);
        });

        // Update pixel update percentage
        $(document).on('input', '#sparkle-pixel_percent', function() {
            pixelUpdatePercent = $('#sparkle-pixel_percent').val();
        });

        // Update RGB values
        $(document).on('input', '#sparkle-r_mean', function() {
            r_mean = $('#sparkle-r_mean').val();
        });
        $(document).on('input', '#sparkle-r_amplitude', function() {
            r_amplitude = $('#sparkle-r_amplitude').val();
        });
        $(document).on('input', '#sparkle-r_frequency', function() {
            r_frequency = $('#sparkle-r_frequency').val();
        });
        $(document).on('input', '#sparkle-g_mean', function() {
            g_mean = $('#sparkle-g_mean').val();
        });
        $(document).on('input', '#sparkle-g_amplitude', function() {
            g_amplitude = $('#sparkle-g_amplitude').val();
        });
        $(document).on('input', '#sparkle-g_frequency', function() {
            g_frequency = $('#sparkle-g_frequency').val();
        });
        $(document).on('input', '#sparkle-b_mean', function() {
            b_mean = $('#sparkle-b_mean').val();
        });
        $(document).on('input', '#sparkle-b_amplitude', function() {
            b_amplitude = $('#sparkle-b_amplitude').val();
        });
        $(document).on('input', '#sparkle-b_frequency', function() {
            b_frequency = $('#sparkle-b_frequency').val();
        });
        $(document).on('input', '#sparkle-alpha', function() {
            alpha = $('#sparkle-alpha').val();
        });


// Create an audio context (suspended by default)
// let audioContext = new (window.AudioContext || window.webkitAudioContext)();

// Create oscillators for each channel
let r_oscillator, g_oscillator, b_oscillator;
let r_gainNode, g_gainNode, b_gainNode;

// Function to initialize oscillators and gain nodes
function initializeOscillators() {
  // Create oscillators
  r_oscillator = audioContext.createOscillator();
  g_oscillator = audioContext.createOscillator();
  b_oscillator = audioContext.createOscillator();

  // Create gain nodes
  r_gainNode = audioContext.createGain();
  g_gainNode = audioContext.createGain();
  b_gainNode = audioContext.createGain();

  // Set oscillator types to sine wave
  r_oscillator.type = 'sine';
  g_oscillator.type = 'sine';
  b_oscillator.type = 'sine';

  // Connect oscillators to gain nodes and gain nodes to the audio context destination (speakers)
  r_oscillator.connect(r_gainNode).connect(audioContext.destination);
  g_oscillator.connect(g_gainNode).connect(audioContext.destination);
  b_oscillator.connect(b_gainNode).connect(audioContext.destination);

  // Start the oscillators
  r_oscillator.start();
  g_oscillator.start();
  b_oscillator.start();
}

// Function to update oscillator frequencies and gains
function updateOscillators() {
  if (!r_oscillator || !g_oscillator || !b_oscillator) return; // Ensure oscillators are initialized

  // Update red channel
  r_oscillator.frequency.setValueAtTime(r_frequency, audioContext.currentTime);
  r_gainNode.gain.setValueAtTime(r_amplitude / 256, audioContext.currentTime); // Normalize amplitude to [0, 1]

  // Update green channel
  g_oscillator.frequency.setValueAtTime(g_frequency, audioContext.currentTime);
  g_gainNode.gain.setValueAtTime(g_amplitude / 256, audioContext.currentTime); // Normalize amplitude to [0, 1]

  // Update blue channel
  b_oscillator.frequency.setValueAtTime(b_frequency, audioContext.currentTime);
  b_gainNode.gain.setValueAtTime(b_amplitude / 256, audioContext.currentTime); // Normalize amplitude to [0, 1]
}

            initializeOscillators(); // Initialize oscillators after resuming


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
            updateOscillators();
        }, 16);

      "))
    )
  )
)
}

sparkle_module_server <- function(id, patterns, module){
  moduleServer(id, function(input, output, session) {

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
)}



