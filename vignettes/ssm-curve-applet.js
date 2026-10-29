// Interactive SSM curve for the introduction vignette. The reader moves
// sliders for elevation (e), amplitude (a) and displacement (d), and the plot
// redraws S(theta) = e + a * cos(theta - d).
//
// tools/precompute-vignettes.R writes this file inline into
// introduction-to-ssm-analysis.Rmd, inside a classic <script> placed after an
// empty <div id="ssm-applet">. Node loads the same file through the
// module.exports guard after the pure functions, so tools/check-ssm-applet.mjs
// checks the functions the page runs, as long as the .Rmd is re-rendered after
// each edit here (the vignette-precompute staleness check fails until it is).
//
// Angles follow the package: degrees, with the pole labelled 360, never 0.
// So the d slider runs from 5 to 360, and the x axis labels the octants
// 45, 90, ..., 360.
//
// Colors are set by CSS rules on classes, which read the theme variables; the
// SVG attributes carry the light-theme colors for a viewer without CSS
// custom properties.

(function () {
  "use strict";

  var OCTANTS = [45, 90, 135, 180, 225, 270, 315, 360];
  var DEFAULTS = { e: 0.1, a: 0.3, d: 90 };
  var SLIDERS = [
    { key: "e", name: "Elevation (e)", min: -0.5, max: 0.5, step: 0.05 },
    { key: "a", name: "Amplitude (a)", min: 0, max: 0.5, step: 0.05 },
    { key: "d", name: "Displacement (d)", min: 5, max: 360, step: 5 }
  ];
  var PLOT = {
    width: 640, height: 320,
    left: 52, right: 20, top: 16, bottom: 40,
    ymin: -1, ymax: 1
  };
  // An amplitude arrow is drawn without its HEAD-pixel head when the head
  // would reach below the elevation line; GAP keeps the head clear of the
  // octant point drawn on the peak.
  var HEAD = 10;
  var GAP = 7;

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
    OCTANTS: OCTANTS, DEFAULTS: DEFAULTS, SLIDERS: SLIDERS, PLOT: PLOT,
    curve: curve, peaks: peaks, xScale: xScale, yScale: yScale,
    octantPoints: octantPoints, pathData: pathData,
    formatNumber: formatNumber, formatDisplacement: formatDisplacement
  };

  if (typeof module !== "undefined" && module.exports) {
    module.exports = api;
  }
  if (typeof document === "undefined") return;

  var SVG_NS = "http://www.w3.org/2000/svg";
  var LIGHT = {
    fg: "#222222", panel: "#ebebeb", grid: "#ffffff", zero: "#767676",
    curve: "#d40000", mark: "#222222", point: "#ffffff"
  };

  var CSS = [
    ".ssm-applet { --ssm-fg: #222222; --ssm-panel: #ebebeb; --ssm-grid: #ffffff;",
    "  --ssm-zero: #767676; --ssm-curve: #d40000; --ssm-mark: #222222;",
    "  --ssm-point: #ffffff; margin: 1em 0; color: var(--ssm-fg); }",
    "[data-bs-theme='dark'] .ssm-applet { --ssm-fg: #dee2e6; --ssm-panel: #2b3035;",
    "  --ssm-grid: #495057; --ssm-zero: #adb5bd; --ssm-curve: #ff6b6b;",
    "  --ssm-mark: #f8f9fa; --ssm-point: #212529; }",
    ".ssm-applet svg { display: block; width: 100%; height: auto; }",
    ".ssm-applet .ssm-panel { fill: var(--ssm-panel); }",
    ".ssm-applet .ssm-grid { stroke: var(--ssm-grid); }",
    ".ssm-applet .ssm-zero { stroke: var(--ssm-zero); }",
    ".ssm-applet .ssm-curve { stroke: var(--ssm-curve); }",
    ".ssm-applet .ssm-elevation, .ssm-applet .ssm-amplitude-arrow",
    "  { stroke: var(--ssm-mark); }",
    ".ssm-applet .ssm-displacement-marker, .ssm-applet .ssm-arrowhead",
    "  { fill: var(--ssm-mark); }",
    ".ssm-applet .ssm-octant-point { fill: var(--ssm-point);",
    "  stroke: var(--ssm-curve); }",
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
      markerUnits: "userSpaceOnUse", markerWidth: HEAD, markerHeight: HEAD,
      orient: "auto"
    }, defs);
    svgEl("path", {
      d: "M0 0 L10 5 L0 10 Z", fill: LIGHT.mark, "class": "ssm-arrowhead"
    }, head);

    var x0 = xScale(0), x1 = xScale(360);
    var y0 = yScale(PLOT.ymax), y1 = yScale(PLOT.ymin);
    svgEl("rect", {
      x: x0, y: y0, width: x1 - x0, height: y1 - y0,
      fill: LIGHT.panel, "class": "ssm-panel"
    }, svg);

    [-1, -0.5, 0, 0.5, 1].forEach(function (v) {
      svgEl("line", {
        x1: x0, x2: x1, y1: yScale(v), y2: yScale(v),
        stroke: LIGHT.grid, "stroke-width": 1, "class": "ssm-grid"
      }, svg);
      var t = svgEl("text", {
        x: x0 - 8, y: yScale(v) + 5, "text-anchor": "end", fill: LIGHT.fg,
        "class": "ssm-axis-label ssm-y-label"
      }, svg);
      t.textContent = v.toFixed(1);
    });
    OCTANTS.forEach(function (theta) {
      svgEl("line", {
        x1: xScale(theta), x2: xScale(theta), y1: y0, y2: y1,
        stroke: LIGHT.grid, "stroke-width": 1, "class": "ssm-grid"
      }, svg);
      var t = svgEl("text", {
        x: xScale(theta), y: y1 + 22, "text-anchor": "middle", fill: LIGHT.fg,
        "class": "ssm-axis-label ssm-x-label"
      }, svg);
      t.textContent = theta + "°";
    });

    svgEl("line", {
      x1: x0, x2: x1, y1: yScale(0), y2: yScale(0),
      stroke: LIGHT.zero, "stroke-width": 2.5, "class": "ssm-zero"
    }, svg);
    var elevation = svgEl("line", {
      x1: x0, x2: x1, stroke: LIGHT.mark, "stroke-width": 2,
      "stroke-dasharray": "8 6", "class": "ssm-elevation"
    }, svg);
    var path = svgEl("path", {
      fill: "none", stroke: LIGHT.curve, "stroke-width": 3,
      "class": "ssm-curve"
    }, svg);
    var peakGroup = svgEl("g", { "class": "ssm-peaks" }, svg);
    var pointGroup = svgEl("g", { "class": "ssm-octant-points" }, svg);
    var eLabel = svgEl("text", {
      "text-anchor": "middle", fill: LIGHT.mark,
      "class": "ssm-mark-label ssm-e-label"
    }, svg);
    eLabel.textContent = "e";

    var controls = htmlEl("div", { "class": "ssm-controls" }, root);
    var sliders = {};
    var outputs = {};
    SLIDERS.forEach(function (s) {
      var id = "ssm-applet-" + s.key;
      htmlEl("label", { "for": id }, controls, s.name);
      sliders[s.key] = htmlEl("input", {
        type: "range", id: id, min: s.min, max: s.max, step: s.step
      }, controls);
      // The slider announces its own value text, so the readout stays quiet.
      outputs[s.key] = htmlEl("output", {
        "for": id, id: id + "-value", "aria-live": "off"
      }, controls);
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
      var text = {
        e: formatNumber(v.e),
        a: formatNumber(v.a),
        d: formatDisplacement(v.a, v.d)
      };
      var spoken = {
        e: text.e, a: text.a, d: text.d.replace("°", " degrees")
      };
      SLIDERS.forEach(function (s) {
        outputs[s.key].textContent = text[s.key];
        sliders[s.key].setAttribute("aria-valuetext", spoken[s.key]);
      });
      svg.setAttribute("aria-label", "SSM cosine curve with elevation " +
        spoken.e + ", amplitude " + spoken.a + " and displacement " + spoken.d);

      var peakAngles = peaks(v.a, v.d);
      elevation.setAttribute("y1", yScale(v.e));
      elevation.setAttribute("y2", yScale(v.e));
      // The e label goes where it is farthest from any peak's arrow.
      var labelTheta = [345, 15, 180].reduce(function (best, theta) {
        var gap = function (t) {
          return Math.min.apply(null, peakAngles.map(function (p) {
            return Math.abs(t - p);
          }).concat([360]));
        };
        return gap(theta) > gap(best) ? theta : best;
      });
      eLabel.setAttribute("x", xScale(labelTheta));
      eLabel.setAttribute("y", yScale(v.e) - 6);
      path.setAttribute("d", pathData(v.e, v.a, v.d));

      while (peakGroup.firstChild) peakGroup.removeChild(peakGroup.firstChild);
      peakAngles.forEach(function (theta) {
        var px = xScale(theta);
        var base = yScale(v.e);
        var top = yScale(v.e + v.a);
        // The head stops short of the octant point that can sit on the peak.
        var headed = base - top >= HEAD + GAP;
        var arrow = {
          x1: px, x2: px, y1: base, y2: headed ? top + GAP : top,
          stroke: LIGHT.mark, "stroke-width": 2.5, "stroke-dasharray": "2 4",
          "class": "ssm-amplitude-arrow"
        };
        if (headed) arrow["marker-end"] = "url(#ssm-arrowhead)";
        svgEl("line", arrow, peakGroup);
        var nearLeft = px - x0 < 30;
        var label = svgEl("text", {
          x: nearLeft ? px + 8 : px - 8,
          y: (base + top) / 2 + 5,
          "text-anchor": nearLeft ? "start" : "end", fill: LIGHT.mark,
          "class": "ssm-mark-label ssm-a-label"
        }, peakGroup);
        label.textContent = "a";
        svgEl("path", {
          d: "M" + px + " " + (y1 - 1) + " l-7 -12 l14 0 Z",
          fill: LIGHT.mark, "class": "ssm-displacement-marker"
        }, peakGroup);
      });

      while (pointGroup.firstChild) pointGroup.removeChild(pointGroup.firstChild);
      octantPoints(v.e, v.a, v.d).forEach(function (p) {
        svgEl("circle", {
          cx: xScale(p.theta), cy: yScale(p.y), r: 5,
          fill: LIGHT.point, stroke: LIGHT.curve,
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
