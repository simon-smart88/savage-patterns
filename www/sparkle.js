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

// Function to fill ImageData with random colors within current ranges
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

// Function to resize the canvas
function resizeCanvas(newWidth, newHeight) {
    width = newWidth;
    height = newHeight;
    canvas.width = width;
    canvas.height = height;
    imageData = ctx.createImageData(width, height);
    fillRandomColors(imageData);
    ctx.putImageData(imageData, 0, 0);
}

// Function to update color ranges based on sine waves
function updateColorRanges() {
    const time = Date.now() / 1000;

    // Update color ranges using sine waves
    r_range = Math.round(r_mean + r_amplitude * Math.sin(2 * Math.PI * r_frequency * time));
    g_range = Math.round(g_mean + g_amplitude * Math.sin(2 * Math.PI * g_frequency * time));
    b_range = Math.round(b_mean + b_amplitude * Math.sin(2 * Math.PI * b_frequency * time));

    // Ensure values stay within 0-255 range
    r_range = Math.max(0, Math.min(255, r_range));
    g_range = Math.max(0, Math.min(255, g_range));
    b_range = Math.max(0, Math.min(255, b_range));
}

// Function to update random pixels
function updateRandomPixels() {
    const totalPixels = width * height;
    const pixelsToUpdate = Math.round((pixelUpdatePercent / 100) * totalPixels);

    // Update color ranges first
    updateColorRanges();

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

// Event listeners for slider updates
$(document).on('input', '#sparkle-canvas_size', function() {
    const size = $(this).val();
    resizeCanvas(size, size);
});

$(document).on('input', '#sparkle-pixel_percent', function() {
    pixelUpdatePercent = $(this).val();
});

// Color parameter updates
$(document).on('input', '#sparkle-r_mean', function() {
    r_mean = parseInt($(this).val());
});
$(document).on('input', '#sparkle-r_amplitude', function() {
    r_amplitude = parseInt($(this).val());
});
$(document).on('input', '#sparkle-r_frequency', function() {
    r_frequency = parseFloat($(this).val());
});

$(document).on('input', '#sparkle-g_mean', function() {
    g_mean = parseInt($(this).val());
});
$(document).on('input', '#sparkle-g_amplitude', function() {
    g_amplitude = parseInt($(this).val());
});
$(document).on('input', '#sparkle-g_frequency', function() {
    g_frequency = parseFloat($(this).val());
});

$(document).on('input', '#sparkle-b_mean', function() {
    b_mean = parseInt($(this).val());
});
$(document).on('input', '#sparkle-b_amplitude', function() {
    b_amplitude = parseInt($(this).val());
});
$(document).on('input', '#sparkle-b_frequency', function() {
    b_frequency = parseFloat($(this).val());
});

$(document).on('input', '#sparkle-alpha', function() {
    alpha = parseInt($(this).val());
});
