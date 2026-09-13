// Ring pattern generator - transliterated from shiny/R/ring_module.R's
// svg_pattern reactive, including its `seq(low_half:1)` gotcha (R's seq()
// on a vector argument returns seq_along() of it, i.e. ascending 1:low_half,
// not the descending low_half:1 the code visually suggests) and the
// value-as-row-index lookup used to build the animation-delay CSS. See
// tests/testthat/helper-reference-ring.R for the R oracle.

function generateRingData({ reps, radius, bulge, space, breath, speed }) {
  const lowHalf = (reps - 1) / 2;
  const highHalf = (reps + 1) / 2;

  const col1 = repR(seqR(1, reps), { times: reps });
  const col2 = repR(seqR(1, reps), { each: reps });

  const bulgeBaseUnit = seqR(0, lowHalf).concat(seqR(lowHalf - 1, 0));
  const part1 = repR(bulgeBaseUnit, { times: reps });

  const part2a = repR(seqR(1, highHalf), { each: reps });
  // seq(low_half:1) in R === seq_along(low_half:1) === 1:low_half (not low_half:1).
  const part2bInner = repR(seqR(1, lowHalf), { each: reps });
  const part2b = revR(part2bInner);
  const part2 = part2a.concat(part2b);

  const col3 = part1.map((v, i) => v + part2[i]);

  const n = reps * reps;
  const rows = [];
  for (let i = 0; i < n; i++) rows.push([col1[i], col2[i], col3[i]]);

  const breathFrac = breath / 100;
  const circles = rows.map(([j, i, b]) => {
    const radiusBulge = (b / reps * 2) * (bulge / 10);
    return {
      cls: `circle_${b}`,
      cx: radius + j * space,
      cy: radius + i * space,
      rStart: radius * (1 - breathFrac) + radiusBulge,
      rEnd: radius * (1 + breathFrac) + radiusBulge,
    };
  });

  const centreIdx = Math.ceil(reps / 2); // R's 1-indexed element_mat[ceiling(reps/2), 1]
  const centre = radius + rows[centreIdx - 1][0] * space;
  const edge = centre - (radius + rows[2][0] * space); // R's element_mat[3, 1]
  const topCorner = centre - edge;
  const bottomCorner = centre + edge - topCorner;

  const uniqueCol3 = [...new Set(col3)];
  // R's element_mat[unique(element_mat[,3]),] uses the VALUES of column 3 as
  // 1-indexed ROW indices - preserved verbatim, quirk and all.
  const delayRows = uniqueCol3.map((v) => rows[v - 1]);
  const delayCss = delayRows
    .map((row) => {
      const x1 = row[0];
      return `.circle_${x1} { animation-delay: ${-(speed / reps) * x1}s;}`;
    })
    .join(" ");

  return { circles, topCorner, bottomCorner, delayCss };
}

function renderRingSVG(params) {
  const { circles, topCorner, bottomCorner, delayCss } = generateRingData(params);
  const { speed, stroke, colour1, colour2, colour3 } = params;

  const colourKeyframes = [0, 25, 50, 75, 100]
    .map((frame, i) => {
      const vars = ["--colour_1", "--colour_2", "--colour_3", "--colour_2", "--colour_1"];
      return `${frame}% {stroke: var(${vars[i]})}`;
    })
    .join(" ");

  const elements = circles
    .map(
      (c) => `<circle class="${c.cls}" cx="${c.cx}" cy="${c.cy}">
      <animate attributeName="r" values="${c.rStart}; ${c.rEnd}; ${c.rStart}" dur="${speed}s" repeatCount="indefinite"></animate>
    </circle>`
    )
    .join("\n");

  return `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="${topCorner} ${topCorner} ${bottomCorner} ${bottomCorner}" height="100%" id="pattern">
  <style>
    :root{
      --colour_1: ${colour1};
      --colour_2: ${colour2};
      --colour_3: ${colour3};
      --stroke: ${stroke};
    }
    circle {animation: col 30s linear infinite; fill: none; stroke-width: var(--stroke)}
    @keyframes col { ${colourKeyframes} }
  </style>
  <style>${delayCss}</style>
  ${elements}
</svg>`;
}

if (typeof module !== "undefined" && module.exports) {
  module.exports = { generateRingData, renderRingSVG };
}
