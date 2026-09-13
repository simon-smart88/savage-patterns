// Line pattern generator - transliterated from shiny/R/line_module.R's
// svg_pattern reactive. See tests/testthat/helper-reference-line.R for the
// R oracle this must stay in parity with, and the project plan for the
// specific R quirks (inner_mean reuse, asymmetric side-offset, no rounding)
// that are intentionally preserved here.
//
// Known deviation: at inner=2 (the slider's minimum) the original R code
// hits a `seq(1, 0)` auto-reversal (R counts down when from > to) that makes
// inner_x and inner_y come out at different lengths, which R then silently
// reconciles via vector recycling through several more steps - a genuinely
// undefined-feeling edge case that isn't reproduced bit-for-bit here. This
// implementation instead treats inner=2 as a normal (small) inner grid.

function generateLineData({ outer, inner, innerSize, innerOffset, breath, zoom }) {
  const outerPoints = outer;
  const innerPoints = inner;

  const innerOffsetCalc = (outerPoints - innerPoints) / 2;

  const outerX = []
    .concat(seqR(0, outerPoints - 1))
    .concat(repR([0, outerPoints - 1], { times: outerPoints - 2 }))
    .concat(seqR(0, outerPoints - 1));
  const outerY = []
    .concat(repR([0], { times: outerPoints }))
    .concat(repR(seqR(1, outerPoints - 2), { each: 2 }))
    .concat(repR([outerPoints - 1], { times: outerPoints }));

  const innerXRaw = []
    .concat(seqR(0, innerPoints - 1))
    .concat(repR([0, innerPoints - 1], { times: innerPoints - 2 }))
    .concat(seqR(0, innerPoints - 1));
  const innerYRaw = []
    .concat(repR([0], { times: innerPoints }))
    .concat(repR(seqR(1, innerPoints - 2), { each: 2 }))
    .concat(repR([innerPoints - 1], { times: innerPoints }));
  const innerX = innerXRaw.map((v) => v + innerOffsetCalc);
  const innerY = innerYRaw.map((v) => v + innerOffsetCalc);

  const oldInnerRange = diffRangeR(innerY);
  const newInnerRange = outerPoints * (innerSize / 100);
  const innerScale = newInnerRange / oldInnerRange;
  const innerMean = meanR(innerX);

  const innerX1 = innerX.map((v) => (v - innerMean) * innerScale + innerMean);
  // NB: reuses innerMean (computed from x) for the y-transform too, matching
  // the original R code exactly.
  const innerY1 = innerY.map((v) => (v - innerMean) * innerScale + innerMean);

  const animationFactor = 1 + breath / 100;
  const innerX2 = innerX1.map((v) => (v - innerMean) / animationFactor + innerMean);
  const innerY2 = innerY1.map((v) => (v - innerMean) / animationFactor + innerMean);

  const outerXSet = repR(outerX, { times: innerX.length });
  const outerYSet = repR(outerY, { times: innerY.length });
  const innerX1Set = repR(innerX1, { each: outerX.length });
  const innerY1Set = repR(innerY1, { each: outerY.length });
  const innerX2Set = repR(innerX2, { each: outerX.length });
  const innerY2Set = repR(innerY2, { each: outerY.length });

  const n = outerXSet.length;
  const pointOffset = (innerX[1] - innerX[0]) * (innerOffset / 100);

  const lines = [];
  for (let i = 0; i < n; i++) {
    const ox = outerXSet[i];
    const oy = outerYSet[i];
    let ix1 = innerX1Set[i];
    let iy1 = innerY1Set[i];
    const ix2 = innerX2Set[i];
    const iy2 = innerY2Set[i];

    const sideTop = oy === 0 && ox < outerPoints - 1 && ox > 0;
    const sideBottom = oy === outerPoints - 1 && ox < outerPoints - 1 && ox > 0;
    const sideLeft = ox === 0;
    const sideRight = ox === outerPoints - 1;
    const side = (sideLeft ? 1 : 0) + (sideTop ? 2 : 0) + (sideRight ? 3 : 0) + (sideBottom ? 4 : 0);

    if (sideTop) iy1 -= pointOffset;
    if (sideBottom) iy1 += pointOffset;
    if (sideLeft) ix1 -= pointOffset;
    if (sideRight) ix1 += pointOffset;

    lines.push({ x1: ox, y1: oy, x2: ix1, y2: iy1, x2Anim: ix2, y2Anim: iy2, side });
  }

  let topCorner, bottomCorner;
  if (zoom) {
    topCorner = Math.min(...innerX1);
    bottomCorner = Math.max(...innerX1) - topCorner;
  } else {
    topCorner = 0;
    bottomCorner = outerPoints - 1;
  }

  return { lines, topCorner, bottomCorner };
}

function renderLineSVG(params) {
  const { lines, topCorner, bottomCorner } = generateLineData(params);
  const { speed, stroke, opacity, colour1, colour2, colour3 } = params;

  const opacityStart = opacity[0] / 100;
  const opacityMid = (opacity[0] + opacity[1]) / 2 / 100;
  const opacityEnd = opacity[1] / 100;

  const lineGradients = `
      <stop offset="0%" stop-color="var(--colour_1)" stop-opacity="var(--opacity_start)"></stop>
      <stop offset="50%" stop-color="var(--colour_2)" stop-opacity="var(--opacity_mid)"></stop>
      <stop offset="100%" stop-color="var(--colour_3)" stop-opacity="var(--opacity_end)"></stop>`;

  const elements = lines
    .map(
      (l) => `<line x1="${l.x1}" x2="${l.x2}" y1="${l.y1}" y2="${l.y2}" stroke="url(#gradient-opacity${l.side})">
      <animate attributeName="x2" values="${l.x2}; ${l.x2Anim}; ${l.x2}" dur="${speed}s" repeatCount="indefinite"></animate>
      <animate attributeName="y2" values="${l.y2}; ${l.y2Anim}; ${l.y2}" dur="${speed}s" repeatCount="indefinite"></animate>
    </line>`
    )
    .join("\n");

  return `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="${topCorner} ${topCorner} ${bottomCorner} ${bottomCorner}" height="100%" id="pattern">
  <style>
    :root{
      --stroke: ${stroke / 100};
      --opacity_start: ${opacityStart};
      --opacity_mid: ${opacityMid};
      --opacity_end: ${opacityEnd};
      --colour_1: ${colour1};
      --colour_2: ${colour2};
      --colour_3: ${colour3};
    }
    line { stroke-width: var(--stroke); }
  </style>
  <defs>
    <linearGradient id="gradient-opacity1" x1="0%" y1="0%" x2="100%" y2="0%">${lineGradients}</linearGradient>
    <linearGradient id="gradient-opacity2" x1="0%" y1="0%" x2="0%" y2="100%">${lineGradients}</linearGradient>
    <linearGradient id="gradient-opacity3" x1="100%" y1="0%" x2="0%" y2="0%">${lineGradients}</linearGradient>
    <linearGradient id="gradient-opacity4" x1="0%" y1="100%" x2="0%" y2="0%">${lineGradients}</linearGradient>
  </defs>
  ${elements}
</svg>`;
}

if (typeof module !== "undefined" && module.exports) {
  module.exports = { generateLineData, renderLineSVG };
}
