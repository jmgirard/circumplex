// Interactive SSM curve for the introduction vignette. The reader moves
// sliders for elevation (e), amplitude (a) and displacement (d), and the plot
// redraws S(theta) = e + a * cos(theta - d).
//
// tools/precompute-vignettes.R writes this file inline into
// introduction-to-ssm-analysis.Rmd, inside a classic <script> placed after an
// empty <div id="ssm-applet">. Node loads the same file through the
// module.exports guard at the bottom, so tools/check-ssm-applet.mjs checks the
// functions the page runs, not a copy of them.
//
// Angles follow the package: degrees, with the pole labelled 360, never 0.
// So the d slider runs from 5 to 360, and the x axis labels the octants
// 45, 90, ..., 360.

(function () {
  "use strict";

  var OCTANTS = [45, 90, 135, 180, 225, 270, 315, 360];
  var DEFAULTS = { e: 0.1, a: 0.3, d: 90 };
  var PLOT = {
    width: 640, height: 320,
    left: 52, right: 20, top: 16, bottom: 40,
    ymin: -1, ymax: 1
  };

  function curve(theta, e, a, d) {
    return e + a * Math.cos((theta - d) * Math.PI / 180);
  }

  // Angles at which the curve peaks. A flat curve (a = 0) has no peak, so
  // displacement is undefined; a peak on the pole shows at both ends.
  function peaks(a, d) {
    if (a === 0) return [];
    if (d % 360 === 0) return [0, 360];
    return [d];
  }

  function xScale(theta) {
    return PLOT.left + theta / 360 * (PLOT.width - PLOT.left - PLOT.right);
  }

  function yScale(y) {
    return PLOT.top + (PLOT.ymax - y) / (PLOT.ymax - PLOT.ymin) *
      (PLOT.height - PLOT.top - PLOT.bottom);
  }

  function octantPoints(e, a, d) {
    return OCTANTS.map(function (theta) {
      return { theta: theta, y: curve(theta, e, a, d) };
    });
  }

  // One vertex per degree from 0 to 360, in pixels rounded to 0.01.
  function pathData(e, a, d) {
    var parts = [];
    for (var theta = 0; theta <= 360; theta++) {
      parts.push((theta === 0 ? "M" : "L") + xScale(theta).toFixed(2) + " " +
        yScale(curve(theta, e, a, d)).toFixed(2));
    }
    return parts.join(" ");
  }

  // Two decimals, with no "-0.00".
  function formatNumber(x) {
    var r = Math.round(x * 100) / 100;
    if (r === 0) r = 0;
    return r.toFixed(2);
  }

  function formatDisplacement(a, d) {
    return a === 0 ? "undefined" : String(Math.round(d)) + "°";
  }

  var api = {
    OCTANTS: OCTANTS, DEFAULTS: DEFAULTS, PLOT: PLOT,
    curve: curve, peaks: peaks, xScale: xScale, yScale: yScale,
    octantPoints: octantPoints, pathData: pathData,
    formatNumber: formatNumber, formatDisplacement: formatDisplacement
  };

  if (typeof module !== "undefined" && module.exports) {
    module.exports = api;
  }
  if (typeof document === "undefined") return;

  var SVG_NS = "http://www.w3.org/2000/svg";

  var CSS = [
    ".ssm-applet { --ssm-fg: #222222; --ssm-panel: #ebebeb; --ssm-grid: #ffffff;",
    "  --ssm-zero: #8c8c8c; --ssm-curve: #d40000; --ssm-mark: #222222;",
    "  --ssm-point: #ffffff; margin: 1em 0; color: var(--ssm-fg); }",
    "[data-bs-theme='dark'] .ssm-applet { --ssm-fg: #dee2e6; --ssm-panel: #2b3035;",
    "  --ssm-grid: #495057; --ssm-zero: #adb5bd; --ssm-curve: #ff6b6b;",
    "  --ssm-mark: #f8f9fa; --ssm-point: #212529; }",
    ".ssm-applet svg { display: block; width: 100%; height: auto; }",
    ".ssm-applet .ssm-axis-label { fill: var(--ssm-fg); font-size: 14px; }",
    ".ssm-applet .ssm-mark-label { fill: var(--ssm-mark); font-size: 15px;",
    "  font-style: italic; }",
    ".ssm-applet .ssm-controls { display: grid; gap: 0.4em 0.8em;",
    "  grid-template-columns: max-content 1fr 5em; align-items: center;",
    "  margin-top: 0.5em; }",
    ".ssm-applet .ssm-controls input { width: 100%; }",
    ".ssm-applet .ssm-controls output { font-variant-numeric: tabular-nums; }",
    ".ssm-applet button { margin-top: 0.5em; }"
  ].join("\n");

  function svgEl(name, attrs, parent) {
    var el = document.createElementNS(SVG_NS, name);
    for (var key in attrs) el.setAttribute(key, attrs[key]);
    if (parent) parent.appendChild(el);
    return el;
  }

  function htmlEl(name, attrs, parent, text) {
    var el = document.createElement(name);
    for (var key in attrs) el.setAttribute(key, attrs[key]);
    if (text !== undefined) el.textContent = text;
    if (parent) parent.appendChild(el);
    return el;
  }

  function build(root) {
    var style = document.createElement("style");
    style.textContent = CSS;
    document.head.appendChild(style);
    root.classList.add("ssm-applet");

    var svg = svgEl("svg", {
      viewBox: "0 0 " + PLOT.width + " " + PLOT.height,
      role: "img", "class": "ssm-plot"
    }, root);
    var defs = svgEl("defs", {}, svg);
    var head = svgEl("marker", {
      id: "ssm-arrowhead", viewBox: "0 0 10 10", refX: "9", refY: "5",
      markerWidth: "7", markerHeight: "7", orient: "auto-start-reverse"
    }, defs);
    svgEl("path", { d: "M0 0 L10 5 L0 10 Z", fill: "var(--ssm-mark)" }, head);

    var x0 = xScale(0), x1 = xScale(360);
    var y0 = yScale(PLOT.ymax), y1 = yScale(PLOT.ymin);
    svgEl("rect", {
      x: x0, y: y0, width: x1 - x0, height: y1 - y0,
      fill: "var(--ssm-panel)", "class": "ssm-panel"
    }, svg);

    [-1, -0.5, 0, 0.5, 1].forEach(function (v) {
      svgEl("line", {
        x1: x0, x2: x1, y1: yScale(v), y2: yScale(v),
        stroke: "var(--ssm-grid)", "stroke-width": 1
      }, svg);
      var t = svgEl("text", {
        x: x0 - 8, y: yScale(v) + 5, "text-anchor": "end",
        "class": "ssm-axis-label ssm-y-label"
      }, svg);
      t.textContent = v.toFixed(1);
    });
    OCTANTS.forEach(function (theta) {
      svgEl("line", {
        x1: xScale(theta), x2: xScale(theta), y1: y0, y2: y1,
        stroke: "var(--ssm-grid)", "stroke-width": 1
      }, svg);
      var t = svgEl("text", {
        x: xScale(theta), y: y1 + 22, "text-anchor": "middle",
        "class": "ssm-axis-label ssm-x-label"
      }, svg);
      t.textContent = theta + "°";
    });

    svgEl("line", {
      x1: x0, x2: x1, y1: yScale(0), y2: yScale(0),
      stroke: "var(--ssm-zero)", "stroke-width": 2.5, "class": "ssm-zero"
    }, svg);
    var elevation = svgEl("line", {
      x1: x0, x2: x1, stroke: "var(--ssm-mark)", "stroke-width": 2,
      "stroke-dasharray": "8 6", "class": "ssm-elevation"
    }, svg);
    var path = svgEl("path", {
      fill: "none", stroke: "var(--ssm-curve)", "stroke-width": 3,
      "class": "ssm-curve"
    }, svg);
    var peakGroup = svgEl("g", { "class": "ssm-peaks" }, svg);
    var pointGroup = svgEl("g", { "class": "ssm-octant-points" }, svg);
    var eLabel = svgEl("text", { "class": "ssm-mark-label ssm-e-label" }, svg);
    eLabel.textContent = "e";

    var controls = htmlEl("div", { "class": "ssm-controls" }, root);
    var sliders = {};
    var outputs = {};
    [
      { key: "e", name: "Elevation (e)", min: -0.5, max: 0.5, step: 0.05 },
      { key: "a", name: "Amplitude (a)", min: 0, max: 0.5, step: 0.05 },
      { key: "d", name: "Displacement (d)", min: 5, max: 360, step: 5 }
    ].forEach(function (s) {
      var id = "ssm-applet-" + s.key;
      htmlEl("label", { "for": id }, controls, s.name);
      sliders[s.key] = htmlEl("input", {
        type: "range", id: id, min: s.min, max: s.max, step: s.step
      }, controls);
      outputs[s.key] = htmlEl("output", { "for": id, id: id + "-value" }, controls);
    });
    var reset = htmlEl("button", { type: "button", "class": "ssm-reset" }, root,
      "Reset");

    function values() {
      return {
        e: parseFloat(sliders.e.value),
        a: parseFloat(sliders.a.value),
        d: parseFloat(sliders.d.value)
      };
    }

    function draw() {
      var v = values();
      outputs.e.textContent = formatNumber(v.e);
      outputs.a.textContent = formatNumber(v.a);
      outputs.d.textContent = formatDisplacement(v.a, v.d);
      svg.setAttribute("aria-label", "SSM cosine curve with elevation " +
        formatNumber(v.e) + ", amplitude " + formatNumber(v.a) +
        " and displacement " + formatDisplacement(v.a, v.d).replace("°",
          " degrees"));

      elevation.setAttribute("y1", yScale(v.e));
      elevation.setAttribute("y2", yScale(v.e));
      eLabel.setAttribute("x", x1 - 14);
      eLabel.setAttribute("y", yScale(v.e) - 6);
      path.setAttribute("d", pathData(v.e, v.a, v.d));

      while (peakGroup.firstChild) peakGroup.removeChild(peakGroup.firstChild);
      peaks(v.a, v.d).forEach(function (theta) {
        var px = xScale(theta);
        var top = yScale(v.e + v.a);
        svgEl("line", {
          x1: px, x2: px, y1: yScale(v.e), y2: top + 2,
          stroke: "var(--ssm-mark)", "stroke-width": 2.5,
          "stroke-dasharray": "2 4", "marker-end": "url(#ssm-arrowhead)",
          "class": "ssm-amplitude-arrow"
        }, peakGroup);
        var label = svgEl("text", {
          x: theta === 0 ? px + 8 : px - 8,
          y: (yScale(v.e) + top) / 2 + 5,
          "text-anchor": theta === 0 ? "start" : "end",
          "class": "ssm-mark-label ssm-a-label"
        }, peakGroup);
        label.textContent = "a";
        svgEl("path", {
          d: "M" + px + " " + (y1 - 1) + " l-7 -12 l14 0 Z",
          fill: "var(--ssm-mark)", "class": "ssm-displacement-marker"
        }, peakGroup);
      });

      while (pointGroup.firstChild) pointGroup.removeChild(pointGroup.firstChild);
      octantPoints(v.e, v.a, v.d).forEach(function (p) {
        svgEl("circle", {
          cx: xScale(p.theta), cy: yScale(p.y), r: 5,
          fill: "var(--ssm-point)", stroke: "var(--ssm-curve)",
          "stroke-width": 2.5, "class": "ssm-octant-point"
        }, pointGroup);
      });
    }

    function setDefaults() {
      sliders.e.value = DEFAULTS.e;
      sliders.a.value = DEFAULTS.a;
      sliders.d.value = DEFAULTS.d;
      draw();
    }

    Object.keys(sliders).forEach(function (key) {
      sliders[key].addEventListener("input", draw);
    });
    reset.addEventListener("click", setDefaults);
    setDefaults();
  }

  var root = document.getElementById("ssm-applet");
  if (root) build(root);
})();
