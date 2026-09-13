// Square pattern generator - transliterated from shiny/R/square_module.R's
// svg_pattern reactive: same seq()-on-a-vector gotcha as the ring module,
// the same "unique values used as 1-indexed row lookups" quirk for building
// the animation-delay CSS (whose selector target - the x index - doesn't
// even match the class the elements actually use - the bulge index - in the
// original code; preserved as-is), values rounded to 3dp throughout, and the
// checkerboard `switch` only relabelling the colour-animation class, never
// the geometry. See tests/testthat/helper-reference-square.R for the oracle.

function generateSquareData({ reps, bulge, internal, switchOn, speed, colourSpeed, colourDif }) {
  const topCorner = 0;
  const bottomCorner = 1000;

  const lowHalf = (reps - 1) / 2;
  const highHalf = (reps + 1) / 2;

  const col1 = repR(seqR(1, reps), { times: reps }).map((v) => v - 1);
  const col2 = repR(seqR(1, reps), { each: reps }).map((v) => v - 1);

  const col3Unit = seqR(1, lowHalf + 1).concat(seqR(lowHalf, 1));
  const col3 = repR(col3Unit, { times: reps });

  const bulgeBaseUnit = seqR(0, lowHalf).concat(seqR(lowHalf - 1, 0));
  const col4Part1 = repR(bulgeBaseUnit, { times: reps });
  const col4Part2 = repR(seqR(1, highHalf), { each: reps }).concat(
    revR(repR(seqR(1, lowHalf), { each: reps })) // seq(low_half:1) === 1:low_half
  );
  const col4 = col4Part1.map((v, i) => v + col4Part2[i]);

  const col5Unit = seqR(lowHalf, 0).concat(seqR(1, lowHalf));
  const col5Part1 = repR(col5Unit, { times: reps });
  const col5Part2 = revR(repR(seqR(1, highHalf), { each: reps })).concat(
    repR(seqR(2, highHalf), { each: reps })
  );
  const col5 = col5Part1.map((v, i) => v + col5Part2[i]);

  const col6Unit = seqR(highHalf, 1).concat(seqR(2, highHalf));
  const col6 = repR(col6Unit, { times: reps });

  const n = reps * reps;
  const widthBulgeSource = bulge >= 0 ? col3 : col6;
  const widthBulge = widthBulgeSource.map((v) => Math.pow(v, Math.abs(bulge) / 5));

  // checkerboard: swap col4 for col5 wherever (pre-switch) col4 is odd.
  const col4Switched = col4.slice();
  if (switchOn) {
    for (let i = 0; i < n; i++) {
      if (col4[i] % 2 === 1) col4Switched[i] = col5[i];
    }
  }

  const totalWidth = sumR(widthBulge);
  const width = widthBulge.map((v) => (v / totalWidth) * reps * 1000);

  const xBase = [0].concat(cumsumR(width.slice(0, reps - 1)));
  const x = repR(xBase, { times: reps });
  const height = repR(width.slice(0, reps), { each: reps });
  const y = repR(xBase, { each: reps });

  const rects = [];
  for (let i = 0; i < n; i++) {
    const w = roundR(width[i], 3) + 0.5;
    const rx = roundR(x[i], 3);
    const h = roundR(height[i], 3) + 0.5;
    const ry = roundR(y[i], 3);

    const centerX = rx + w * 0.5;
    const centerY = ry + h * 0.5;

    const initialWidth = roundR(w * (1 - internal[0] / 100), 3);
    const initialHeight = roundR(h * (1 - internal[0] / 100), 3);
    const finalWidth = roundR(w * (1 - internal[1] / 100), 3);
    const finalHeight = roundR(h * (1 - internal[1] / 100), 3);

    const initialX = roundR(centerX - initialWidth / 2, 3);
    const initialY = roundR(centerY - initialHeight / 2, 3);
    const finalX = roundR(centerX - finalWidth / 2, 3);
    const finalY = roundR(centerY - finalHeight / 2, 3);

    rects.push({
      cls: col4Switched[i],
      x: rx,
      y: ry,
      width: w,
      height: h,
      initialX,
      initialY,
      finalX,
      finalY,
      initialWidth,
      initialHeight,
      finalWidth,
      finalHeight,
    });
  }

  const rows = [];
  for (let i = 0; i < n; i++) rows.push([col1[i], col2[i], col3[i], col4Switched[i], col5[i], col6[i]]);

  const uniqueCol4 = [...new Set(col4Switched)];
  const delayRows = uniqueCol4.map((v) => rows[v - 1]); // 1-indexed row lookup, quirk preserved
  const delayCss = delayRows
    .map((row) => {
      const x1 = row[0];
      const x4 = row[3];
      return `.rect_${x1 + 1}_out {animation-delay: ${-(colourSpeed / reps) * x4}s;}
        .rect_${x1 + 1}_in {animation-delay: ${-((colourSpeed / reps) * x4) - colourSpeed * (colourDif / 100)}s;}`;
    })
    .join(" ");

  return { rects, topCorner, bottomCorner, delayCss };
}

function renderSquareSVG(params) {
  const { rects, topCorner, bottomCorner, delayCss } = generateSquareData(params);
  const { speed, colourSpeed, colour1, colour2, colour3 } = params;

  const colourKeyframes = [0, 25, 50, 75, 100]
    .map((frame, i) => {
      const vars = ["--colour_1", "--colour_2", "--colour_3", "--colour_2", "--colour_1"];
      return `${frame}% {fill: var(${vars[i]})}`;
    })
    .join(" ");

  const elements = rects
    .map(
      (r) => `<g>
      <rect class="rect_${r.cls}_out" x="${r.x}" y="${r.y}" width="${r.width}" height="${r.height}"></rect>
      <rect class="rect_${r.cls}_in">
        <animate attributeName="x" values="${r.initialX}; ${r.finalX}; ${r.initialX}" dur="${speed}s" repeatCount="indefinite"></animate>
        <animate attributeName="y" values="${r.initialY}; ${r.finalY}; ${r.initialY}" dur="${speed}s" repeatCount="indefinite"></animate>
        <animate attributeName="width" values="${r.initialWidth}; ${r.finalWidth}; ${r.initialWidth}" dur="${speed}s" repeatCount="indefinite"></animate>
        <animate attributeName="height" values="${r.initialHeight}; ${r.finalHeight}; ${r.initialHeight}" dur="${speed}s" repeatCount="indefinite"></animate>
      </rect>
    </g>`
    )
    .join("\n");

  return `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="${topCorner} ${topCorner} ${bottomCorner} ${bottomCorner}" height="100%" id="pattern">
  <style>
    :root{
      --colour_1: ${colour1};
      --colour_2: ${colour2};
      --colour_3: ${colour3};
      --colour_speed: ${colourSpeed}s;
    }
    rect {animation: col var(--colour_speed) linear infinite; fill: none;}
    @keyframes col { ${colourKeyframes} }
  </style>
  <style>${delayCss}</style>
  ${elements}
</svg>`;
}

if (typeof module !== "undefined" && module.exports) {
  module.exports = { generateSquareData, renderSquareSVG };
}
