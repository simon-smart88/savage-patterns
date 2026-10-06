// WebGL renderer for the line pattern (js/patterns/line.js's
// generateLineData() output). Replaces the SVG + SMIL rendering on screen;
// renderLineSVG() is still used for downloads in the palette colour mode.
//
// Each line becomes a quad (two triangles, six vertices) uploaded to the GPU
// once in setData(). Every vertex carries the line's start, end and animated
// end, its side, its colour coordinates, and which corner of the quad it is;
// the vertex shader does the animation (SMIL's x2 -> x2Anim -> x2 as a
// triangle wave), the thickening and the colouring. So a frame is a uniform
// update and one drawArrays() call, regardless of line count, and style
// changes (setStyle) never touch the vertex buffer. The renderer runs its own
// requestAnimationFrame loop and shuts itself down if its canvas is removed
// from the page (e.g. when another module replaces the container contents).
//
// Colour modes (style.colourMode):
//  - "gradient": matches renderLineSVG(). Each line has a 3-stop linear
//    gradient across its own bounding box (objectBoundingBox units), running
//    1 = left->right, 2 = top->bottom, 3 = right->left, 4 = bottom->top.
//    Along a straight line that bbox gradient is just the distance along the
//    line (or its reverse), so it's computed per vertex and interpolated. As
//    in SVG, lines whose bbox has zero width or height aren't drawn.
//  - "fan": the lines leaving each outer point sweep through the hue range,
//    ordered by their angle as seen from that point.
//  - "ring": every line from an outer point shares a hue, set by that point's
//    angle around the centre.
//  - "spiral": fan + ring, so each fan's rainbow starts where its point sits.
//  - "direction": hue from the line's current direction, so parallel lines
//    match and colours shift as the pattern breathes.
// Rainbow hues are generated in OKLCH so they're perceptually even, and
// opacity fades from opacity[0] at the outer point to opacity[1] at the inner.
//
// Lines thinner than one device pixel are drawn 1px wide with alpha scaled by
// their true width, approximating SVG's antialiased hairlines.

const LINE_COLOUR_MODES = { gradient: 0, fan: 1, ring: 2, spiral: 3, direction: 4 };

function createLineRenderer(container) {
  const VERT = `
    attribute vec2 aStart;
    attribute vec2 aEnd;
    attribute vec2 aEndAnim;
    attribute vec3 aCorner;   // x: 0 = start, 1 = end; y: -1/+1 = edge; z: side 1-4
    attribute vec2 aHue;      // x: position in its fan, y: outer point's angle (both 0..1)
    uniform float uF;         // animation phase, 0..1
    uniform float uTop;       // viewBox origin (zoom)
    uniform float uScale;     // device pixels per pattern unit
    uniform float uHalfWidth; // device pixels
    uniform vec2 uRes;
    uniform mediump float uMode; // LINE_COLOUR_MODES (precision must match FRAG's)
    uniform float uHueStart;  // 0..1, includes the hue cycle
    uniform float uHueRange;  // 0..1 of the colour wheel
    uniform float uLightness; // OKLCH L
    uniform float uChroma;    // OKLCH C
    varying float vT;         // gradient mode: position along the gradient
    varying float vAlong;     // 0 at the outer point, 1 at the inner point
    varying vec3 vRgb;        // rainbow modes: line colour

    const float TAU = 6.2831853;

    vec3 oklchToSrgb(float L, float C, float h) {
      float a = C * cos(h), b = C * sin(h);
      float l = L + 0.3963377774 * a + 0.2158037573 * b;
      float m = L - 0.1055613458 * a - 0.0638541728 * b;
      float s = L - 0.0894841775 * a - 1.2914855480 * b;
      l = l * l * l; m = m * m * m; s = s * s * s;
      vec3 lin = clamp(vec3(
         4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
        -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
        -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s), 0.0, 1.0);
      return mix(12.92 * lin, 1.055 * pow(lin, vec3(1.0 / 2.4)) - 0.055, step(0.0031308, lin));
    }

    void main() {
      vec2 a = aStart;
      vec2 b = mix(aEnd, aEndAnim, uF);
      vec2 d = b - a;
      float side = aCorner.z;
      vAlong = aCorner.x;
      vT = 0.0;
      vRgb = vec3(0.0);

      if (uMode == 0.0) {
        // Zero-size bbox: SVG paints nothing, so collapse off-screen.
        if (abs(d.x) < 1e-6 || abs(d.y) < 1e-6) {
          gl_Position = vec4(2.0, 2.0, 0.0, 1.0);
          return;
        }
        // Does the gradient run start->end (false) or end->start (true)?
        bool flip = side == 1.0 ? d.x < 0.0
                  : side == 2.0 ? d.y < 0.0
                  : side == 3.0 ? d.x > 0.0
                  :               d.y > 0.0;
        vT = flip ? 1.0 - aCorner.x : aCorner.x;
      } else {
        float h = uMode == 1.0 ? aHue.x
                : uMode == 2.0 ? aHue.y
                : uMode == 3.0 ? aHue.x + aHue.y
                :                atan(d.y, d.x) / TAU + 0.5;
        vRgb = oklchToSrgb(uLightness, uChroma, (uHueStart + h * uHueRange) * TAU);
      }

      vec2 pa = (a - uTop) * uScale;
      vec2 pb = (b - uTop) * uScale;
      vec2 dp = pb - pa;
      float len = length(dp);
      vec2 normal = len > 0.0 ? vec2(-dp.y, dp.x) / len : vec2(0.0);
      vec2 p = mix(pa, pb, aCorner.x) + normal * aCorner.y * uHalfWidth;
      gl_Position = vec4(p.x / uRes.x * 2.0 - 1.0, 1.0 - p.y / uRes.y * 2.0, 0.0, 1.0);
    }`;

  const FRAG = `
    precision mediump float;
    uniform float uMode;
    uniform vec4 uStop0;      // rgb + alpha, straight (not premultiplied)
    uniform vec4 uStop1;
    uniform vec4 uStop2;
    uniform float uCoverage;  // hairline alpha scale
    varying float vT;
    varying float vAlong;
    varying vec3 vRgb;
    void main() {
      vec4 c;
      if (uMode == 0.0) {
        c = vT < 0.5 ? mix(uStop0, uStop1, vT * 2.0) : mix(uStop1, uStop2, vT * 2.0 - 1.0);
      } else {
        c = vec4(vRgb, mix(uStop0.a, uStop2.a, vAlong));
      }
      float a = c.a * uCoverage;
      gl_FragColor = vec4(c.rgb * a, a);
    }`;

  // Quad corners in triangle order: (along, edge)
  const CORNERS = [[0, -1], [1, -1], [1, 1], [0, -1], [1, 1], [0, 1]];
  const ATTRIBUTES = [["aStart", 2], ["aEnd", 2], ["aEndAnim", 2], ["aCorner", 3], ["aHue", 2]];
  const FLOATS_PER_VERTEX = ATTRIBUTES.reduce((n, [, size]) => n + size, 0);
  const UNIFORMS = ["uF", "uTop", "uScale", "uHalfWidth", "uRes", "uMode", "uHueStart", "uHueRange",
    "uLightness", "uChroma", "uStop0", "uStop1", "uStop2", "uCoverage"];

  const canvas = document.createElement("canvas");
  container.appendChild(canvas);
  const gl = canvas.getContext("webgl", { antialias: true, premultipliedAlpha: true, alpha: true });
  if (!gl) throw new Error("WebGL is not available");

  const prog = link(compile(gl.VERTEX_SHADER, VERT), compile(gl.FRAGMENT_SHADER, FRAG));
  gl.useProgram(prog);
  const loc = {};
  for (const u of UNIFORMS) loc[u] = gl.getUniformLocation(prog, u);

  const buf = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, buf);
  let offset = 0;
  for (const [name, size] of ATTRIBUTES) {
    const l = gl.getAttribLocation(prog, name);
    gl.enableVertexAttribArray(l);
    gl.vertexAttribPointer(l, size, gl.FLOAT, false, FLOATS_PER_VERTEX * 4, offset * 4);
    offset += size;
  }

  gl.enable(gl.BLEND);
  gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA);

  let top = 0, span = 1, vertexCount = 0, style = null;
  let phase = 0, huePhase = 0, lastNow = null, rafId = null, snapshotRequests = [];

  function compile(type, src) {
    const s = gl.createShader(type);
    gl.shaderSource(s, src);
    gl.compileShader(s);
    if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(s));
    return s;
  }

  function link(vs, fs) {
    const p = gl.createProgram();
    gl.attachShader(p, vs);
    gl.attachShader(p, fs);
    gl.linkProgram(p);
    if (!gl.getProgramParameter(p, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(p));
    return p;
  }

  // The canvas's CSS box sets its size; match the backing store to it.
  function resize() {
    const size = Math.round(canvas.clientWidth * (window.devicePixelRatio || 1));
    if (size > 0 && canvas.width !== size) canvas.width = canvas.height = size;
  }
  const resizeObserver = new ResizeObserver(resize);
  resizeObserver.observe(canvas);

  // Per-line colour coordinates for the rainbow modes, from the static
  // (un-animated) geometry: [fan position, outer point angle], both 0..1.
  function hueCoordinates(lines) {
    const n = lines.length;
    let cx = 0, cy = 0;
    for (const l of lines) { cx += l.x1; cy += l.y1; }
    cx /= n; cy /= n;

    // Angle of each line relative to its outer point's direction to the
    // centre; lines from a boundary point span < 180deg so this never wraps.
    const fans = new Map();
    const rel = new Float32Array(n);
    lines.forEach((l, i) => {
      const toCentre = Math.atan2(cy - l.y1, cx - l.x1);
      let r = Math.atan2(l.y2 - l.y1, l.x2 - l.x1) - toCentre;
      r = Math.atan2(Math.sin(r), Math.cos(r));
      rel[i] = r;
      const key = l.x1 + "," + l.y1;
      const f = fans.get(key);
      if (f) { f.min = Math.min(f.min, r); f.max = Math.max(f.max, r); }
      else fans.set(key, { min: r, max: r, ring: (Math.atan2(l.y1 - cy, l.x1 - cx) / (2 * Math.PI) + 1.25) % 1 });
    });

    return lines.map((l, i) => {
      const f = fans.get(l.x1 + "," + l.y1);
      return [f.max > f.min ? (rel[i] - f.min) / (f.max - f.min) : 0.5, f.ring];
    });
  }

  // Geometry: output of generateLineData(). Re-uploads the vertex buffer.
  function setData({ lines, topCorner, bottomCorner }) {
    top = topCorner;
    span = bottomCorner;
    const hues = hueCoordinates(lines);
    const verts = new Float32Array(lines.length * CORNERS.length * FLOATS_PER_VERTEX);
    let k = 0;
    lines.forEach((l, i) => {
      for (const [along, edge] of CORNERS) {
        verts[k++] = l.x1; verts[k++] = l.y1;
        verts[k++] = l.x2; verts[k++] = l.y2;
        verts[k++] = l.x2Anim; verts[k++] = l.y2Anim;
        verts[k++] = along; verts[k++] = edge; verts[k++] = l.side;
        verts[k++] = hues[i][0]; verts[k++] = hues[i][1];
      }
    });
    gl.bindBuffer(gl.ARRAY_BUFFER, buf);
    gl.bufferData(gl.ARRAY_BUFFER, verts, gl.STATIC_DRAW);
    vertexCount = lines.length * CORNERS.length;
  }

  // Style, in the Create app's line control units:
  // { speed, stroke, opacity: [start, end], colour1, colour2, colour3,
  //   colourMode, hueStart (deg), hueRange (deg), hueCycle (s, 0 = off),
  //   lightness (%), chroma (% of 0.4) }. Uniforms only.
  function setStyle(s) {
    style = s;
    const rgb = (hex) => {
      const v = parseInt(hex.slice(1), 16);
      return [((v >> 16) & 255) / 255, ((v >> 8) & 255) / 255, (v & 255) / 255];
    };
    const [o0, o1] = s.opacity;
    gl.uniform4f(loc.uStop0, ...rgb(s.colour1), o0 / 100);
    gl.uniform4f(loc.uStop1, ...rgb(s.colour2), (o0 + o1) / 200);
    gl.uniform4f(loc.uStop2, ...rgb(s.colour3), o1 / 100);
    gl.uniform1f(loc.uMode, LINE_COLOUR_MODES[s.colourMode] ?? 0);
    gl.uniform1f(loc.uHueRange, s.hueRange / 360);
    gl.uniform1f(loc.uLightness, s.lightness / 100);
    gl.uniform1f(loc.uChroma, (s.chroma / 100) * 0.4);
  }

  function draw(now) {
    if (!style || !vertexCount) return;
    // Accumulate phases so changing speed doesn't jump the animation.
    const dt = lastNow === null ? 0 : (now - lastNow) / 1000;
    lastNow = now;
    phase = (phase + dt / style.speed) % 1;
    if (style.hueCycle > 0) huePhase = (huePhase + dt / style.hueCycle) % 1;
    const scale = canvas.width / span;
    const widthPx = (style.stroke / 100) * scale;

    gl.viewport(0, 0, canvas.width, canvas.height);
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);
    // SMIL's values="x2; x2Anim; x2": a 0 -> 1 -> 0 triangle wave.
    gl.uniform1f(loc.uF, 1 - Math.abs(2 * phase - 1));
    gl.uniform1f(loc.uHueStart, style.hueStart / 360 + huePhase);
    gl.uniform1f(loc.uTop, top);
    gl.uniform1f(loc.uScale, scale);
    gl.uniform1f(loc.uHalfWidth, Math.max(widthPx, 1) / 2);
    gl.uniform1f(loc.uCoverage, Math.min(widthPx, 1));
    gl.uniform2f(loc.uRes, canvas.width, canvas.height);
    gl.drawArrays(gl.TRIANGLES, 0, vertexCount);
  }

  function frame(now) {
    if (!canvas.isConnected) return destroy();
    draw(now);
    // The drawing buffer is only readable in the same task as the draw.
    snapshotRequests.forEach((resolve) => canvas.toBlob(resolve, "image/png"));
    snapshotRequests = [];
    rafId = requestAnimationFrame(frame);
  }

  // PNG of the next frame, as a Blob.
  function snapshot() {
    return new Promise((resolve) => snapshotRequests.push(resolve));
  }

  function destroy() {
    cancelAnimationFrame(rafId);
    resizeObserver.disconnect();
    gl.getExtension("WEBGL_lose_context")?.loseContext();
    canvas.remove();
  }

  resize();
  rafId = requestAnimationFrame(frame);
  return { canvas, setData, setStyle, snapshot, destroy };
}
