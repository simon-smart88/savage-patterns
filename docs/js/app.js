// UI wiring for the native Savage Patterns app. The actual pattern maths
// lives in js/patterns/*.js (parity-tested against shiny/R/*_module.R via
// tests/testthat) - this file only reproduces the original Shiny app's
// interactive behaviour: module switching, sliders/accordion/randomize
// buttons, colour pickers, SVG download, and the gallery/about tabs. The line
// module is drawn on screen with WebGL (js/line-webgl.js).
(function () {
  "use strict";

  const MODULES = ["line", "ring", "square"];
  const ICONS = { line: "assets/line_icon.svg", ring: "assets/ring_icon.svg", square: "assets/square_icon.svg" };

  let currentModule = null;
  const colourWidgets = {};

  function $(id) {
    return document.getElementById(id);
  }

  function shuffle(arr) {
    const a = arr.slice();
    for (let i = a.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [a[i], a[j]] = [a[j], a[i]];
    }
    return a;
  }

  // R's sample.int(n) draws a uniform integer in [1, n].
  function sampleInt(n) {
    return 1 + Math.floor(Math.random() * n);
  }

  // ---------------- module picker ----------------
  function buildModulePicker() {
    const container = $("module-picker");
    const order = shuffle(MODULES);
    order.forEach((m) => {
      const input = document.createElement("input");
      input.type = "radio";
      input.className = "btn-check";
      input.name = "module-picker-radio";
      input.id = `module-radio-${m}`;
      input.autocomplete = "off";

      const label = document.createElement("label");
      label.className = "btn btn-outline-info";
      label.htmlFor = input.id;
      label.innerHTML = `<img src="${ICONS[m]}" alt="${m}" class="module-icon">`;

      container.appendChild(input);
      container.appendChild(label);
      input.addEventListener("change", () => selectModule(m));
    });
    return order;
  }

  function selectModule(name) {
    if (currentModule === name) return;
    currentModule = name;
    MODULES.forEach((m) => {
      $(`panel-${m}`).hidden = m !== name;
    });
    RANDOMIZE_ALL[name]();
  }

  // ---------------- generic slider value readouts ----------------
  function wireOutputs() {
    document.querySelectorAll("input[type=range]").forEach((el) => {
      const out = $(`${el.id}-out`);
      if (!out) return;
      const sync = () => {
        out.textContent = el.value;
      };
      el.addEventListener("input", sync);
      sync();
    });
  }

  // ---------------- dual-handle range widget ----------------
  function setupDualRange(lowId, highId, widgetId, outId, onChange, formatFn) {
    const low = $(lowId);
    const high = $(highId);
    const fill = $(widgetId).querySelector(".dual-range-fill");
    const out = $(outId);

    function pct(v, min, max) {
      return ((v - min) / (max - min)) * 100;
    }

    function render() {
      const min = Number(low.min);
      const max = Number(low.max);
      const lo = Number(low.value);
      const hi = Number(high.value);
      fill.style.left = pct(lo, min, max) + "%";
      fill.style.width = pct(hi, min, max) - pct(lo, min, max) + "%";
      out.textContent = formatFn ? formatFn(lo, hi) : `${lo} - ${hi}`;
    }

    low.addEventListener("input", () => {
      if (Number(low.value) > Number(high.value)) low.value = high.value;
      render();
      onChange();
    });
    high.addEventListener("input", () => {
      if (Number(high.value) < Number(low.value)) high.value = low.value;
      render();
      onChange();
    });

    render();
    dualRangeRenderers.push(render);
    return { render };
  }

  // ---------------- colour widget (hue/lumin + 3 swatches) ----------------
  function buildColourWidget(moduleName, onChange) {
    const container = $(`${moduleName}-colour`);
    container.innerHTML = `
      <div class="param">
        <label>Hue <i class="fa-solid fa-circle-info" title="Choose a colour palette to use"></i></label>
        <select class="form-select" id="${moduleName}-hue">
          <option value="random" selected>Random</option>
          <option value="red">Red</option>
          <option value="orange">Orange</option>
          <option value="yellow">Yellow</option>
          <option value="green">Green</option>
          <option value="blue">Blue</option>
          <option value="purple">Purple</option>
          <option value="pink">Pink</option>
          <option value="monochrome">Monochrome</option>
        </select>
      </div>
      <div class="param">
        <label>Luminosity <i class="fa-solid fa-circle-info" title="Choose how bright the colour palette is"></i></label>
        <select class="form-select" id="${moduleName}-lumin">
          <option value="random" selected>Random</option>
          <option value="light">Light</option>
          <option value="bright">Bright</option>
          <option value="dark">Dark</option>
        </select>
      </div>
      <div class="row g-2">
        <div class="col-4"><input type="color" class="form-control form-control-color w-100" id="${moduleName}-colour1"></div>
        <div class="col-4"><input type="color" class="form-control form-control-color w-100" id="${moduleName}-colour2"></div>
        <div class="col-4"><input type="color" class="form-control form-control-color w-100" id="${moduleName}-colour3"></div>
      </div>`;

    const hue = $(`${moduleName}-hue`);
    const lumin = $(`${moduleName}-lumin`);
    const c1 = $(`${moduleName}-colour1`);
    const c2 = $(`${moduleName}-colour2`);
    const c3 = $(`${moduleName}-colour3`);

    function randomize() {
      const [a, b, c] = randomColors(3, hue.value, lumin.value);
      c1.value = a;
      c2.value = b;
      c3.value = c;
      onChange();
    }

    hue.addEventListener("change", randomize);
    lumin.addEventListener("change", randomize);
    [c1, c2, c3].forEach((el) => el.addEventListener("input", onChange));

    colourWidgets[moduleName] = { hue, lumin, c1, c2, c3, randomize };
  }

  function getColours(moduleName) {
    const w = colourWidgets[moduleName];
    return { colour1: w.c1.value, colour2: w.c2.value, colour3: w.c3.value };
  }

  function openAccordion(moduleName, panel) {
    const el = $(`${moduleName}-acc-${panel}`);
    bootstrap.Collapse.getOrCreateInstance(el, { toggle: false }).show();
  }

  const RANDOMIZE_ALL = {};

  // ================= LINE =================
  function lineParams() {
    return {
      outer: Number($("line-outer").value),
      inner: Number($("line-inner").value),
      innerSize: Number($("line-innerSize").value),
      innerOffset: Number($("line-innerOffset").value),
      breath: Number($("line-breath").value),
      zoom: $("line-zoom").checked,
      speed: Number($("line-speed").value),
      stroke: Number($("line-stroke").value),
      opacity: [Number($("line-opacity-low").value), Number($("line-opacity-high").value)],
      ...getColours("line"),
    };
  }

  // The line pattern is drawn with WebGL (js/line-webgl.js) because the SVG
  // version slows down badly at thousands of lines. Falls back to SVG if
  // WebGL isn't available. Downloads always use renderLineSVG().
  let lineRenderer = null;

  function getLineRenderer() {
    const container = $("svg_container");
    if (lineRenderer && lineRenderer.canvas.isConnected) return lineRenderer;
    container.innerHTML = "";
    try {
      lineRenderer = createLineRenderer(container);
    } catch (e) {
      console.warn("WebGL unavailable, falling back to SVG:", e);
      lineRenderer = null;
    }
    return lineRenderer;
  }

  function updateLineStructure() {
    const p = lineParams();
    const renderer = getLineRenderer();
    if (!renderer) {
      $("svg_container").innerHTML = renderLineSVG(p);
      return;
    }
    renderer.setData(generateLineData(p));
    renderer.setStyle(p);
  }

  function updateLineStyleVars() {
    const p = lineParams();
    if (lineRenderer && lineRenderer.canvas.isConnected) {
      lineRenderer.setStyle(p);
      return;
    }
    const svg = $("pattern");
    if (!svg) return;
    svg.style.setProperty("--stroke", p.stroke / 100);
    svg.style.setProperty("--opacity_start", p.opacity[0] / 100);
    svg.style.setProperty("--opacity_mid", (p.opacity[0] + p.opacity[1]) / 2 / 100);
    svg.style.setProperty("--opacity_end", p.opacity[1] / 100);
    svg.style.setProperty("--colour_1", p.colour1);
    svg.style.setProperty("--colour_2", p.colour2);
    svg.style.setProperty("--colour_3", p.colour3);
  }

  function setupLine() {
    ["line-outer", "line-inner", "line-innerSize", "line-innerOffset", "line-breath"].forEach((id) =>
      $(id).addEventListener("input", updateLineStructure)
    );
    $("line-zoom").addEventListener("change", updateLineStructure);
    // Speed is baked into the SVG's <animate> elements but is just a
    // uniform for WebGL, so only the SVG fallback needs a rebuild.
    $("line-speed").addEventListener("input", () => (lineRenderer ? updateLineStyleVars() : updateLineStructure()));
    $("line-stroke").addEventListener("input", updateLineStyleVars);
    setupDualRange("line-opacity-low", "line-opacity-high", "line-opacity-widget", "line-opacity-out", updateLineStyleVars);
    buildColourWidget("line", updateLineStyleVars);

    function randomPattern() {
      $("line-outer").value = 6 + sampleInt(14);
      $("line-inner").value = 3 + sampleInt(12);
      $("line-innerSize").value = 10 + sampleInt(70);
      $("line-innerOffset").value = 10 + sampleInt(70);
      openAccordion("line", "pattern");
    }
    function randomAnimation() {
      $("line-breath").value = 10 + sampleInt(30);
      $("line-speed").value = 5 + sampleInt(55);
      openAccordion("line", "animation");
    }
    function randomColour() {
      $("line-stroke").value = sampleInt(20) / 20;
      colourWidgets.line.randomize();
      openAccordion("line", "colour");
    }
    function randomAll() {
      randomPattern();
      randomAnimation();
      $("line-stroke").value = sampleInt(20) / 20;
      colourWidgets.line.randomize();
      refreshOutputs();
      updateLineStructure();
    }

    $("line-random-pattern").addEventListener("click", () => {
      randomPattern();
      refreshOutputs();
      updateLineStructure();
    });
    $("line-random-animation").addEventListener("click", () => {
      randomAnimation();
      refreshOutputs();
      updateLineStructure();
    });
    $("line-random-colour").addEventListener("click", () => {
      randomColour();
      refreshOutputs();
      updateLineStyleVars();
    });
    $("line-random-all").addEventListener("click", randomAll);
    RANDOMIZE_ALL.line = randomAll;
  }

  // ================= RING =================
  function ringParams() {
    return {
      reps: Number($("ring-reps").value),
      radius: Number($("ring-radius").value),
      bulge: Number($("ring-bulge").value),
      space: Number($("ring-space").value),
      breath: Number($("ring-breath").value),
      speed: Number($("ring-speed").value),
      stroke: Number($("ring-stroke").value),
      ...getColours("ring"),
    };
  }

  function updateRingStructure() {
    $("svg_container").innerHTML = renderRingSVG(ringParams());
  }

  function updateRingStyleVars() {
    const svg = $("pattern");
    if (!svg) return;
    const p = ringParams();
    svg.style.setProperty("--stroke", p.stroke);
    svg.style.setProperty("--colour_1", p.colour1);
    svg.style.setProperty("--colour_2", p.colour2);
    svg.style.setProperty("--colour_3", p.colour3);
  }

  function setupRing() {
    ["ring-reps", "ring-radius", "ring-bulge", "ring-space", "ring-breath", "ring-speed"].forEach((id) =>
      $(id).addEventListener("input", updateRingStructure)
    );
    $("ring-stroke").addEventListener("input", updateRingStyleVars);
    buildColourWidget("ring", updateRingStyleVars);

    function randomPattern() {
      $("ring-radius").value = 9 + sampleInt(41);
      $("ring-bulge").value = sampleInt(100);
      $("ring-reps").value = 1 + sampleInt(22) * 2;
      $("ring-space").value = sampleInt(30);
      openAccordion("ring", "pattern");
    }
    function randomAnimation() {
      $("ring-breath").value = 9 + sampleInt(21);
      $("ring-speed").value = 4 + sampleInt(56);
      openAccordion("ring", "animation");
    }
    function randomColour() {
      $("ring-stroke").value = sampleInt(10) / 10;
      colourWidgets.ring.randomize();
      openAccordion("ring", "colour");
    }
    function randomAll() {
      $("ring-radius").value = 9 + sampleInt(41);
      $("ring-bulge").value = sampleInt(100);
      $("ring-reps").value = 1 + sampleInt(22) * 2;
      $("ring-space").value = 4 + sampleInt(26);
      $("ring-breath").value = 9 + sampleInt(21);
      $("ring-speed").value = 4 + sampleInt(56);
      $("ring-stroke").value = sampleInt(10) / 10;
      colourWidgets.ring.randomize();
      refreshOutputs();
      updateRingStructure();
    }

    $("ring-random-pattern").addEventListener("click", () => {
      randomPattern();
      refreshOutputs();
      updateRingStructure();
    });
    $("ring-random-animation").addEventListener("click", () => {
      randomAnimation();
      refreshOutputs();
      updateRingStructure();
    });
    $("ring-random-colour").addEventListener("click", () => {
      randomColour();
      refreshOutputs();
      updateRingStyleVars();
    });
    $("ring-random-all").addEventListener("click", randomAll);
    RANDOMIZE_ALL.ring = randomAll;
  }

  // ================= SQUARE =================
  function squareParams() {
    return {
      reps: Number($("square-reps").value),
      bulge: Number($("square-bulge").value),
      internal: [Number($("square-internal-low").value), Number($("square-internal-high").value)],
      switchOn: $("square-switch").checked,
      speed: Number($("square-speed").value),
      colourSpeed: Number($("square-colourSpeed").value),
      colourDif: Number($("square-colourDif").value),
      ...getColours("square"),
    };
  }

  function updateSquareStructure() {
    $("svg_container").innerHTML = renderSquareSVG(squareParams());
  }

  function updateSquareStyleVars() {
    const svg = $("pattern");
    if (!svg) return;
    const p = squareParams();
    svg.style.setProperty("--colour_1", p.colour1);
    svg.style.setProperty("--colour_2", p.colour2);
    svg.style.setProperty("--colour_3", p.colour3);
    svg.style.setProperty("--colour_speed", `${p.colourSpeed}s`);
  }

  function setupSquare() {
    ["square-reps", "square-bulge", "square-speed", "square-colourSpeed", "square-colourDif"].forEach((id) =>
      $(id).addEventListener("input", updateSquareStructure)
    );
    $("square-switch").addEventListener("change", updateSquareStructure);
    setupDualRange(
      "square-internal-low",
      "square-internal-high",
      "square-internal-widget",
      "square-internal-out",
      updateSquareStructure
    );
    // colour_speed also gets a redundant live-var update on top of the full
    // regen above - matches the original app (see the plan's write-up on
    // per-module regen-vs-live-var triggers), harmless either way.
    $("square-colourSpeed").addEventListener("input", updateSquareStyleVars);
    buildColourWidget("square", updateSquareStyleVars);

    // NOTE: the original app's "Randomize Everything"/"Randomize Animation"
    // handlers for square call `updateSliderInput(session, "dif", ...)` -
    // but the colour-difference slider's real id is "colourDif", not "dif".
    // That's a silent id-typo bug: those buttons never actually touch it.
    // Preserved here deliberately rather than "fixed" - colourDif is
    // intentionally left out of both randomizers below.
    function randomPattern() {
      $("square-bulge").value = sampleInt(20) - 10;
      $("square-reps").value = 1 + sampleInt(15) * 2;
      const lowInternal = 10 + sampleInt(30);
      const highInternal = lowInternal + sampleInt(30);
      $("square-internal-low").value = lowInternal;
      $("square-internal-high").value = highInternal;
      openAccordion("square", "pattern");
    }
    function randomAnimation() {
      $("square-speed").value = 4 + sampleInt(56);
      $("square-colourSpeed").value = 4 + sampleInt(56);
      openAccordion("square", "animation");
    }
    function randomColour() {
      colourWidgets.square.randomize();
      openAccordion("square", "colour");
    }
    function randomAll() {
      randomPattern();
      randomAnimation();
      colourWidgets.square.randomize();
      refreshOutputs();
      updateSquareStructure();
    }

    $("square-random-pattern").addEventListener("click", () => {
      randomPattern();
      refreshOutputs();
      updateSquareStructure();
    });
    $("square-random-animation").addEventListener("click", () => {
      randomAnimation();
      refreshOutputs();
      updateSquareStructure();
    });
    $("square-random-colour").addEventListener("click", () => {
      randomColour();
      refreshOutputs();
      updateSquareStyleVars();
    });
    $("square-random-all").addEventListener("click", randomAll);
    RANDOMIZE_ALL.square = randomAll;
  }

  // re-sync every range's <output> readout (used after a randomize sets
  // .value directly, which doesn't fire an "input" event on its own).
  const dualRangeRenderers = [];

  function refreshOutputs() {
    document.querySelectorAll("input[type=range]").forEach((el) => {
      const out = $(`${el.id}-out`);
      if (out) out.textContent = el.value;
    });
    dualRangeRenderers.forEach((r) => r());
  }

  // ---------------- download ----------------
  function setupDownload() {
    $("download-btn").addEventListener("click", () => {
      let serialized;
      if (currentModule === "line") {
        // On screen it's a WebGL canvas; the download is the equivalent SVG.
        serialized = renderLineSVG(lineParams());
      } else {
        const svg = document.querySelector("#svg_container svg");
        if (!svg) return;
        serialized = new XMLSerializer().serializeToString(svg);
      }
      const blob = new Blob([serialized], { type: "image/svg+xml" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      const now = new Date();
      const pad = (n) => String(n).padStart(2, "0");
      const stamp = `${now.getFullYear()}-${pad(now.getMonth() + 1)}-${pad(now.getDate())} ${pad(now.getHours())}_${pad(
        now.getMinutes()
      )}_${pad(now.getSeconds())}`;
      a.href = url;
      a.download = `${currentModule}_${stamp}.svg`;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    });
  }

  // ---------------- gallery ----------------
  function setupGallery() {
    const files = shuffle([
      "square_2025-02-05 20_24_22.svg", "ring_2025-02-05 20_09_42.svg",
      "square_2025-02-05 20_21_02.svg", "ring_2025-02-05 20_11_14.svg",
      "line_2025-02-05 20_45_09.svg", "square_2025-02-05 20_23_24.svg",
      "square_2025-02-05 20_27_13.svg", "square_2025-02-05 20_34_07.svg",
      "line_2025-02-05 20_45_36.svg", "line_2025-02-05 20_48_27.svg",
      "square_2025-02-05 20_19_41.svg", "line_2025-02-05 20_39_50.svg",
      "ring_2025-02-05 20_05_02.svg", "line_2025-02-05 20_44_31.svg",
      "line_2025-02-11 21_46_48.svg", "ring_2025-02-05 20_13_34.svg",
      "line_2025-02-05 20_37_19.svg", "line_2025-02-11 21_43_55.svg",
      "square_2025-02-05 20_32_54.svg", "line_2025-02-05 20_38_18.svg",
      "square_2025-02-05 20_30_27.svg", "line_2025-02-11 21_45_06.svg",
      "line_2025-02-05 20_50_03.svg",
    ]);
    let index = 0;
    const img = $("gallery-image");
    const base = "https://raw.githubusercontent.com/simon-smart88/savage-patterns/main/images/gallery/";
    function render() {
      img.src = base + encodeURIComponent(files[index]);
    }
    $("gallery-previous").addEventListener("click", () => {
      index = (index - 1 + files.length) % files.length;
      render();
    });
    $("gallery-next").addEventListener("click", () => {
      index = (index + 1) % files.length;
      render();
    });
    render();
  }

  // ---------------- about ----------------
  const PYTHON_SOURCE = String.raw`# -*- coding: utf-8 -*-
"""
Created on Tue Nov  4 00:02:01 2014

@author: simon
"""

svgo="<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\">"
svge="</svg>"

styopen="<style type=\"text/css\"><![CDATA["
styclose="]]></style>"

clas="<ellipse class=\""
clasend=">"

eopen="<ellipse class=\""
eopenend="\">"
eclose="</ellipse>"
estyle="    ellipse {stroke: black; stroke-width: 1; fill:black;	}"

cx="<animate attributeName=\"cx\" values=\""
cy="<animate attributeName=\"cy\" values=\""
rx="<animate attributeName=\"rx\" values=\""
ry="<animate attributeName=\"ry\" values=\""
sc=";"
dur="\" dur=\"20s\" repeatCount=\"indefinite\"/>"

dot=" ."
animo=" { -webkit-animation: "
animc=" 20s infinite;}"
opo="@-webkit-keyframes "
opa=" { 0% { fill-opacity: "
opb=" ; } 50% { fill-opacity: "
opc=" ; } 100% { fill-opacity: "
ope=" ; }	}"

groupo="<g id=\""
tile="tile"
cent="cent"
groupc="\">"
groupe="</g>"

useo="<use xlink:href=\"#"
usetrans="\" transform=\"rotate("
usec=")\"/>"

fillo="<animate attributeName=\"fill\" values=\"#60C;#6CF;#060;#FF6;#F60;#C00;#F39;#60C\" repeatCount=\"indefinite\"  begin=\""
fille="s\" dur=\"50s\"/>"
minus="-"

def cya(n):
    cya2=(40*n)+100
    return cya2

def cyb(n):
    cyb2=((2*n**2)+(16*n)-12)+100
    return cyb2

def cyc(n):
    cyc2=(-(2*n**2)+(64*n)+12)+100
    return cyc2

def cyd(n):
    cyd2=-(2*n**2)+(112*n)-488
    return cyd2

def cye(n):
    cye2=(2*n**2)-(32*n)+688
    return cye2

def cxa(n):
    cxa2=(40*n)+100
    return cxa2

def cxb(n):
    cxb2=((2*n**2)+(16*n)-12)+100
    return cxb2

def cxc(n):
    cxc2=(-(2*n**2)+(64*n)+12)+100
    return cxc2

def cxd(n):
    cxd2=-(2*n**2)+(112*n)-488
    return cxd2

def cxe(n):
    cxe2=(2*n**2)-(32*n)+688
    return cxe2

def rya(n):
    rya2=20
    return rya2

def ryb(n):
    ryb2=8+(2*n)
    return ryb2

def ryc(n):
    ryc2=-(2*n)+32
    return ryc2

def ryd(n):
    ryd2=-(2*n)+56
    return ryd2

def rye(n):
    rye2=(2*n)-16
    return rye2

def rxa(n):
    rxa2=20
    return rxa2

def rxb(n):
    rxb2=8+(2*n)
    return rxb2

def rxc(n):
    rxc2=-(2*n)+32
    return rxc2

def rxd(n):
    rxd2=-(2*n)+56
    return rxd2

def rxe(n):
    rxe2=(2*n)-16
    return rxe2

def opva(n):
    opva2=1-(n*0.03)
    return opva2

def opvb(n):
    opvb2=0.25+(n*0.03)
    return opvb2

def filla(n):
    filla2=0.5*n
    return filla2

"""open svg"""
print svgo

"""print css"""
print styopen

"""style for all ellipses"""
print estyle

"""style opacity animations"""
for i in range(26):
    print dot+(chr(i + ord('a')))+animo+(chr(i + ord('a')))+animc
    print opo+(chr(i + ord('a')))+opa+str(opva(i))+opb+str(opvb(i))+opc+str(opva(i))+ope

print styclose

"""print top left tile and center"""
print groupo+tile+groupc
for j in range(13): #x sets
    for i in range(13): #y sets
        print eopen+(chr(i+j + ord('a')))+eopenend
        print fillo+minus+str(filla(i+j))+fille
        print cx+str(cxa(j))+sc+str(cxb(j))+sc+str(cxa(j))+sc+str(cxc(j))+sc+str(cxa(j))+dur
        print cy+str(cya(i))+sc+str(cyb(i))+sc+str(cya(i))+sc+str(cyc(i))+sc+str(cya(i))+dur
        print rx+str(rxa(i))+sc+str(rxb(j))+sc+str(rxa(i))+sc+str(rxc(j))+sc+str(rxa(i))+dur
        print ry+str(rya(i))+sc+str(ryb(i))+sc+str(rya(i))+sc+str(ryc(i))+sc+str(rya(i))+dur
        print eclose
print groupe

"""print top right tile - use xd, xe, yb, yc"""
print groupo+cent+groupc
for j in range (13,25,1): #x = 13
    for i in range(13):
        print eopen+(chr(-j+24+i + ord('a')))+eopenend
        print fillo+minus+str(filla(-j+24+i))+fille
        print cx+str(cxa(j))+sc+str(cxd(j))+sc+str(cxa(j))+sc+str(cxe(j))+sc+str(cxa(j))+dur
        print cy+str(cya(i))+sc+str(cyb(i))+sc+str(cya(i))+sc+str(cyc(i))+sc+str(cya(i))+dur
        print rx+str(rxa(i))+sc+str(rxd(j))+sc+str(rxa(i))+sc+str(rxe(j))+sc+str(rxa(i))+dur
        print ry+str(rya(i))+sc+str(ryb(i))+sc+str(rya(i))+sc+str(ryc(i))+sc+str(rya(i))+dur
        print eclose
print groupe

"""print bottom left tile - use xb, xc, yd, ye"""
for j in range (13): #x = 13
    for i in range(13,25,1):
        print eopen+(chr(-i+24+j + ord('a')))+eopenend
        print fillo+minus+str(filla(-i+24+j))+fille
        print cx+str(cxa(j))+sc+str(cxb(j))+sc+str(cxa(j))+sc+str(cxc(j))+sc+str(cxa(j))+dur
        print cy+str(cya(i))+sc+str(cyd(i))+sc+str(cya(i))+sc+str(cye(i))+sc+str(cya(i))+dur
        print rx+str(rxa(i))+sc+str(rxb(j))+sc+str(rxa(i))+sc+str(rxc(j))+sc+str(rxa(i))+dur
        print ry+str(rya(i))+sc+str(ryd(i))+sc+str(rya(i))+sc+str(rye(i))+sc+str(rya(i))+dur
        print eclose

"""print bottom right tile - use xd, xe, yd, ye"""
for j in range (13,25,1): #x = 13
    for i in range(13,25,1):
        print eopen+(chr((-i+23+(-j+25)) + ord('a')))+eopenend
        print fillo+minus+str(filla(-i+23+(-j+25)))+fille
        print cx+str(cxa(j))+sc+str(cxd(j))+sc+str(cxa(j))+sc+str(cxe(j))+sc+str(cxa(j))+dur
        print cy+str(cya(i))+sc+str(cyd(i))+sc+str(cya(i))+sc+str(cye(i))+sc+str(cya(i))+dur
        print rx+str(rxa(i))+sc+str(rxd(j))+sc+str(rxa(i))+sc+str(rxe(j))+sc+str(rxa(i))+dur
        print ry+str(rya(i))+sc+str(ryd(i))+sc+str(rya(i))+sc+str(rye(i))+sc+str(rya(i))+dur
        print eclose

"""print tiles
print useo+tile+usetrans+str(90)+usec
print useo+cent+usetrans+str(180)+usec
"""
"""
http://sarasoueidan.com/blog/svg-transformations/
<use xlink:href="#tile" transform="rotate(90), translate(0,-1148)"/>
<use xlink:href="#tile" transform="rotate(180), translate(-1148,-1148)"/>
<use xlink:href="#tile" transform="rotate(270), translate(-1148,0)"/>
"""

print svge
`;

  function setupAbout() {
    $("python-code").textContent = PYTHON_SOURCE;
  }

  // ---------------- init ----------------
  document.addEventListener("DOMContentLoaded", () => {
    wireOutputs();
    setupDownload();
    setupGallery();
    setupAbout();
    setupLine();
    setupRing();
    setupSquare();

    const order = buildModulePicker();
    $(`module-radio-${order[0]}`).checked = true;
    selectModule(order[0]);
  });
})();
