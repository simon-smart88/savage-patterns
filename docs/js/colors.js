// Random colour generation - port of shiny/R/colour.R's random_color() /
// generate_single_color(), which pick a hue/luminosity band and then call
// grDevices::hcl(). This isn't part of the parity-tested pattern maths (it
// only ever chooses the *initial*, user-visible swatch colours, which are
// pass-through values everywhere else) so it doesn't need to be bit-for-bit
// identical to R's hcl() - just the same standard CIE LCh(uv) -> sRGB
// conversion, so the palette feels the same.

const HUE_ANGLES = {
  red: 0,
  orange: 30,
  yellow: 60,
  green: 120,
  blue: 240,
  purple: 270,
  pink: 330,
};

function hclToHex(h, c, l) {
  // LCh(uv) -> Luv
  const hRad = (h * Math.PI) / 180;
  const L = l;
  const u = c * Math.cos(hRad);
  const v = c * Math.sin(hRad);

  // D65 reference white
  const Xn = 95.047;
  const Yn = 100;
  const Zn = 108.883;

  let X, Y, Z;
  if (L <= 0) {
    X = Y = Z = 0;
  } else {
    const u0 = (4 * Xn) / (Xn + 15 * Yn + 3 * Zn);
    const v0 = (9 * Yn) / (Xn + 15 * Yn + 3 * Zn);
    Y = L > 8 ? Yn * Math.pow((L + 16) / 116, 3) : (Yn * L) / 903.3;
    const uPrime = u / (13 * L) + u0;
    const vPrime = v / (13 * L) + v0;
    X = (Y * 9 * uPrime) / (4 * vPrime);
    Z = (Y * (12 - 3 * uPrime - 20 * vPrime)) / (4 * vPrime);
  }

  const x = X / 100;
  const y = Y / 100;
  const z = Z / 100;

  let r = 3.2404542 * x - 1.5371385 * y - 0.4985314 * z;
  let g = -0.969266 * x + 1.8760108 * y + 0.041556 * z;
  let b = 0.0556434 * x - 0.2040259 * y + 1.0572252 * z;

  const companding = (channel) => {
    const c2 = channel <= 0.0031308 ? 12.92 * channel : 1.055 * Math.pow(channel, 1 / 2.4) - 0.055;
    return Math.max(0, Math.min(255, Math.round(c2 * 255)));
  };

  r = companding(r);
  g = companding(g);
  b = companding(b);

  const toHex = (n) => n.toString(16).padStart(2, "0");
  return `#${toHex(r)}${toHex(g)}${toHex(b)}`;
}

function randomBetween(min, max) {
  return min + Math.random() * (max - min);
}

function generateSingleColor(hue, luminosity) {
  let h, c;
  if (hue === "monochrome") {
    c = 0;
    h = 0;
  } else {
    c = randomBetween(0.5, 1) * 100;
    if (hue in HUE_ANGLES) {
      h = HUE_ANGLES[hue];
    } else {
      h = randomBetween(0, 360);
    }
  }

  let l;
  switch (luminosity) {
    case "bright":
      l = randomBetween(0.5, 0.8) * 100;
      break;
    case "light":
      l = randomBetween(0.7, 0.9) * 100;
      break;
    case "dark":
      l = randomBetween(0.1, 0.4) * 100;
      break;
    default:
      l = randomBetween(0.1, 0.9) * 100;
  }

  return hclToHex(h, c, l);
}

const HUE_CHOICES = ["red", "orange", "yellow", "green", "blue", "purple", "pink", "monochrome"];
const LUMINOSITY_CHOICES = ["light", "bright", "dark"];

function randomColors(count, hue, luminosity) {
  const colors = [];
  for (let i = 0; i < count; i++) {
    const currentHue = hue === "random" ? HUE_CHOICES[Math.floor(Math.random() * HUE_CHOICES.length)] : hue;
    const currentLuminosity =
      luminosity === "random" ? LUMINOSITY_CHOICES[Math.floor(Math.random() * LUMINOSITY_CHOICES.length)] : luminosity;
    colors.push(generateSingleColor(currentHue, currentLuminosity));
  }
  return colors;
}
