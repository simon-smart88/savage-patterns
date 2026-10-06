// WebGL renderer for the line pattern (js/patterns/line.js's
// generateLineData() output). Replaces the SVG + SMIL rendering on screen;
// renderLineSVG() is still used for downloads.
//
// Each line becomes a quad (two triangles, six vertices) uploaded to the GPU
// once in setData(). Every vertex carries the line's start, end and animated
// end, its side, and which corner of the quad it is; the vertex shader does
// the animation (SMIL's x2 -> x2Anim -> x2 as a triangle wave), the
// thickening and the gradient position. So a frame is a uniform update and
// one drawArrays() call, regardless of line count, and style changes
// (setStyle) never touch the vertex buffer. The renderer runs its own
// requestAnimationFrame loop and shuts itself down if its canvas is removed
// from the page (e.g. when another module replaces the container contents).
//
// Gradients match renderLineSVG(): each line has a 3-stop linear gradient
// across its own bounding box (objectBoundingBox units), running
// 1 = left->right, 2 = top->bottom, 3 = right->left, 4 = bottom->top.
// Along a straight line that bbox gradient is just the distance along the line
// (or its reverse), so it can be computed per vertex and interpolated. As in
// SVG, lines whose bbox has zero width or height aren't drawn.
//
// Lines thinner than one device pixel are drawn 1px wide with alpha scaled by
// their true width, approximating SVG's antialiased hairlines.

function createLineRenderer(container) {
  const VERT = `
    attribute vec2 aStart;
    attribute vec2 aEnd;
    attribute vec2 aEndAnim;
    attribute vec3 aCorner;   // x: 0 = start, 1 = end; y: -1/+1 = edge; z: side 1-4
    uniform float uF;         // animation phase, 0..1
    uniform float uTop;       // viewBox origin (zoom)
    uniform float uScale;     // device pixels per pattern unit
    uniform float uHalfWidth; // device pixels
    uniform vec2 uRes;
    varying float vT;         // position along the gradient, 0..1
    void main() {
      vec2 a = aStart;
      vec2 b = mix(aEnd, aEndAnim, uF);
      vec2 d = b - a;
      float side = aCorner.z;

      // Zero-size bbox: SVG paints nothing, so collapse off-screen.
      if (abs(d.x) < 1e-6 || abs(d.y) < 1e-6) {
        gl_Position = vec4(2.0, 2.0, 0.0, 1.0);
        vT = 0.0;
        return;
      }

      // Does the gradient run start->end (false) or end->start (true)?
      bool flip = side == 1.0 ? d.x < 0.0
                : side == 2.0 ? d.y < 0.0
                : side == 3.0 ? d.x > 0.0
                :               d.y > 0.0;
      vT = flip ? 1.0 - aCorner.x : aCorner.x;

      vec2 pa = (a - uTop) * uScale;
      vec2 pb = (b - uTop) * uScale;
      vec2 dp = pb - pa;
      vec2 normal = vec2(-dp.y, dp.x) / length(dp);
      vec2 p = mix(pa, pb, aCorner.x) + normal * aCorner.y * uHalfWidth;
      gl_Position = vec4(p.x / uRes.x * 2.0 - 1.0, 1.0 - p.y / uRes.y * 2.0, 0.0, 1.0);
    }`;

  const FRAG = `
    precision mediump float;
    uniform vec4 uStop0;      // rgb + alpha, straight (not premultiplied)
    uniform vec4 uStop1;
    uniform vec4 uStop2;
    uniform float uCoverage;  // hairline alpha scale
    varying float vT;
    void main() {
      vec4 c = vT < 0.5 ? mix(uStop0, uStop1, vT * 2.0) : mix(uStop1, uStop2, vT * 2.0 - 1.0);
      float a = c.a * uCoverage;
      gl_FragColor = vec4(c.rgb * a, a);
    }`;

  // Quad corners in triangle order: (along, edge)
  const CORNERS = [[0, -1], [1, -1], [1, 1], [0, -1], [1, 1], [0, 1]];
  const FLOATS_PER_VERTEX = 9;

  const canvas = document.createElement("canvas");
  container.appendChild(canvas);
  const gl = canvas.getContext("webgl", { antialias: true, premultipliedAlpha: true, alpha: true });
  if (!gl) throw new Error("WebGL is not available");

  const prog = link(compile(gl.VERTEX_SHADER, VERT), compile(gl.FRAGMENT_SHADER, FRAG));
  gl.useProgram(prog);
  const loc = {};
  for (const a of ["aStart", "aEnd", "aEndAnim", "aCorner"]) loc[a] = gl.getAttribLocation(prog, a);
  for (const u of ["uF", "uTop", "uScale", "uHalfWidth", "uRes", "uStop0", "uStop1", "uStop2", "uCoverage"]) {
    loc[u] = gl.getUniformLocation(prog, u);
  }

  const buf = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, buf);
  const stride = FLOATS_PER_VERTEX * 4;
  [["aStart", 2, 0], ["aEnd", 2, 2], ["aEndAnim", 2, 4], ["aCorner", 3, 6]].forEach(([name, size, offset]) => {
    gl.enableVertexAttribArray(loc[name]);
    gl.vertexAttribPointer(loc[name], size, gl.FLOAT, false, stride, offset * 4);
  });

  gl.enable(gl.BLEND);
  gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA);

  let top = 0, span = 1, vertexCount = 0, style = null;
  let phase = 0, lastNow = null, rafId = null;

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

  // Geometry: output of generateLineData(). Re-uploads the vertex buffer.
  function setData({ lines, topCorner, bottomCorner }) {
    top = topCorner;
    span = bottomCorner;
    const verts = new Float32Array(lines.length * CORNERS.length * FLOATS_PER_VERTEX);
    let k = 0;
    for (const l of lines) {
      for (const [along, edge] of CORNERS) {
        verts[k++] = l.x1; verts[k++] = l.y1;
        verts[k++] = l.x2; verts[k++] = l.y2;
        verts[k++] = l.x2Anim; verts[k++] = l.y2Anim;
        verts[k++] = along; verts[k++] = edge; verts[k++] = l.side;
      }
    }
    gl.bindBuffer(gl.ARRAY_BUFFER, buf);
    gl.bufferData(gl.ARRAY_BUFFER, verts, gl.STATIC_DRAW);
    vertexCount = lines.length * CORNERS.length;
  }

  // Style: { speed, stroke, opacity: [start, end], colour1, colour2, colour3 }
  // with the same units as the Create app's line controls. Uniforms only.
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
  }

  function draw(now) {
    if (!style || !vertexCount) return;
    // Accumulate phase so changing speed doesn't jump the animation.
    if (lastNow !== null) phase = (phase + (now - lastNow) / 1000 / style.speed) % 1;
    lastNow = now;
    const scale = canvas.width / span;
    const widthPx = (style.stroke / 100) * scale;

    gl.viewport(0, 0, canvas.width, canvas.height);
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);
    // SMIL's values="x2; x2Anim; x2": a 0 -> 1 -> 0 triangle wave.
    gl.uniform1f(loc.uF, 1 - Math.abs(2 * phase - 1));
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
    rafId = requestAnimationFrame(frame);
  }

  function destroy() {
    cancelAnimationFrame(rafId);
    resizeObserver.disconnect();
    gl.getExtension("WEBGL_lose_context")?.loseContext();
    canvas.remove();
  }

  resize();
  rafId = requestAnimationFrame(frame);
  return { canvas, setData, setStyle, destroy };
}
