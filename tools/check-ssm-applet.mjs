// Checks the functions of vignettes/ssm-curve-applet.js, the interactive SSM
// curve in the introduction vignette, against the structural-summary curve
// S(theta) = e + a * cos(theta - d) (Gurtman & Balakrishnan, 1998, Eq. 1).
//
//   node tools/check-ssm-applet.mjs                 # the shipped applet
//   node tools/check-ssm-applet.mjs <path-to-copy>  # a planted copy
//
// It pins the slider ranges and the plot's linear axes, then covers every
// slider setting (e from -0.5 to 0.5 by 0.05, a from 0 to 0.5 by 0.05, d from
// 5 to 360 by 5) at every theta from 0 to 360 by 1, and exits 1 on the first
// rule that fails. tools/check-ssm-applet-plants.mjs shows that each rule can
// fail. Both run in .github/workflows/vignette-precompute.yaml.

import { createRequire } from "node:module";
import { resolve } from "node:path";

const require = createRequire(import.meta.url);
const file = resolve(process.argv[2] ?? "vignettes/ssm-curve-applet.js");
const applet = require(file);

function fail(message) {
  console.error(`check-ssm-applet: ${message}`);
  process.exit(1);
}

const TOL = 1e-12;
const PX_TOL = 0.01;
const es = Array.from({ length: 21 }, (_, k) => (k - 10) / 20);
const as = Array.from({ length: 11 }, (_, k) => k / 20);
const ds = Array.from({ length: 72 }, (_, k) => (k + 1) * 5);
const thetas = Array.from({ length: 361 }, (_, k) => k);
const expected = (theta, e, a, d) => e + a * Math.cos((theta - d) * Math.PI / 180);
const sameList = (x, y) =>
  Array.isArray(x) && x.length === y.length && x.every((v, i) => v === y[i]);

const SLIDERS = [
  { key: "e", min: -0.5, max: 0.5, step: 0.05 },
  { key: "a", min: 0, max: 0.5, step: 0.05 },
  { key: "d", min: 5, max: 360, step: 5 }
];
const shipped = (applet.SLIDERS ?? []).map(({ key, min, max, step }) => ({ key, min, max, step }));
if (JSON.stringify(shipped) !== JSON.stringify(SLIDERS)) {
  fail(`slider ranges are ${JSON.stringify(shipped)}, expected ${JSON.stringify(SLIDERS)}`);
}

// The axes are linear maps of the plot box, written here from PLOT's edges
// rather than taken from the applet's own scale functions.
const { PLOT } = applet;
if (PLOT.ymin !== -1 || PLOT.ymax !== 1) {
  fail(`y range is ${PLOT.ymin} to ${PLOT.ymax}, expected -1 to 1`);
}
const px = (theta) => PLOT.left + theta / 360 * (PLOT.width - PLOT.left - PLOT.right);
const py = (y) => PLOT.top + (1 - y) / 2 * (PLOT.height - PLOT.top - PLOT.bottom);
for (const theta of thetas) {
  if (!(Math.abs(applet.xScale(theta) - px(theta)) <= 1e-9)) {
    fail(`xScale(${theta}) is ${applet.xScale(theta)}, expected ${px(theta)}`);
  }
}
for (let k = -100; k <= 100; k++) {
  const y = k / 100;
  if (!(Math.abs(applet.yScale(y) - py(y)) <= 1e-9)) {
    fail(`yScale(${y}) is ${applet.yScale(y)}, expected ${py(y)}`);
  }
}

// Readouts: two decimals with no "-0.00", and d in whole degrees unless a = 0.
const twoDecimals = (k) => {
  const s = Math.abs(k * 5);
  return `${k < 0 ? "-" : ""}${Math.floor(s / 100)}.${String(s % 100).padStart(2, "0")}`;
};
for (let k = -10; k <= 10; k++) {
  if (applet.formatNumber(k / 20) !== twoDecimals(k)) {
    fail(`formatNumber(${k / 20}) is "${applet.formatNumber(k / 20)}", expected "${twoDecimals(k)}"`);
  }
}
for (const x of [-0, -1e-17, 1e-17, 0.1 + 0.2 - 0.3]) {
  if (applet.formatNumber(x) !== "0.00") {
    fail(`formatNumber(${x}) is "${applet.formatNumber(x)}", expected "0.00"`);
  }
}
for (const a of as) {
  for (const d of ds) {
    const want = a === 0 ? "undefined" : `${d}°`;
    if (applet.formatDisplacement(a, d) !== want) {
      fail(`formatDisplacement(${a}, ${d}) is "${applet.formatDisplacement(a, d)}", expected "${want}"`);
    }
  }
}

const OCTANTS = [45, 90, 135, 180, 225, 270, 315, 360];
let settings = 0;
for (const e of es) {
  for (const a of as) {
    for (const d of ds) {
      settings++;
      const at = `e = ${e}, a = ${a}, d = ${d}`;
      for (const theta of thetas) {
        const got = applet.curve(theta, e, a, d);
        if (!(Math.abs(got - expected(theta, e, a, d)) <= TOL)) {
          fail(`curve(${theta}) is ${got} at ${at}, expected ${expected(theta, e, a, d)}`);
        }
      }

      const want = a === 0 ? [] : d === 360 ? [0, 360] : [d];
      const got = applet.peaks(a, d);
      if (!sameList(got, want)) {
        fail(`peaks is ${JSON.stringify(got)} at ${at}, expected ${JSON.stringify(want)}`);
      }

      const points = applet.octantPoints(e, a, d);
      if (!sameList(points.map((p) => p.theta), OCTANTS)) {
        fail(`octant angles are ${JSON.stringify(points.map((p) => p.theta))} at ${at}`);
      }
      for (const p of points) {
        if (!(Math.abs(p.y - expected(p.theta, e, a, d)) <= TOL)) {
          fail(`octant point at ${p.theta} is ${p.y} at ${at}, off the curve`);
        }
      }

      const vertices = applet.pathData(e, a, d).trim().split(/\s*[ML]\s*/).filter(Boolean);
      if (vertices.length !== thetas.length) {
        fail(`path has ${vertices.length} vertices at ${at}, expected ${thetas.length}`);
      }
      vertices.forEach((v, i) => {
        const [x, y] = v.split(/\s+/).map(Number);
        const wx = px(i);
        const wy = py(expected(i, e, a, d));
        if (!(Math.abs(x - wx) <= PX_TOL && Math.abs(y - wy) <= PX_TOL)) {
          fail(`path vertex ${i} is (${x}, ${y}) at ${at}, expected (${wx}, ${wy})`);
        }
      });
    }
  }
}

console.log(`check-ssm-applet: ${settings} slider settings, ${thetas.length} angles each: OK`);
