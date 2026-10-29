// Checks the functions of vignettes/ssm-curve-applet.js, the interactive SSM
// curve in the introduction vignette, against the structural-summary curve
// S(theta) = e + a * cos(theta - d) (Gurtman & Balakrishnan, 1998, Eq. 1).
//
//   node tools/check-ssm-applet.mjs                 # the shipped applet
//   node tools/check-ssm-applet.mjs <path-to-copy>  # a planted copy
//
// It covers every slider setting (e from -0.5 to 0.5 by 0.05, a from 0 to 0.5
// by 0.05, d from 5 to 360 by 5) at every theta from 0 to 360 by 1, and exits
// 1 on the first rule that fails. Run by .github/workflows/vignette-precompute.yaml.

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

const { PLOT } = applet;
if (Math.abs(applet.xScale(0) - PLOT.left) > TOL) {
  fail(`xScale(0) is ${applet.xScale(0)}, not the left edge ${PLOT.left}`);
}
if (Math.abs(applet.xScale(360) - (PLOT.width - PLOT.right)) > TOL) {
  fail(`xScale(360) is ${applet.xScale(360)}, not the right edge ${PLOT.width - PLOT.right}`);
}
for (let theta = 1; theta <= 360; theta++) {
  if (!(applet.xScale(theta) > applet.xScale(theta - 1))) {
    fail(`xScale does not increase at theta = ${theta}`);
  }
}
for (let k = 1; k <= 200; k++) {
  const lo = PLOT.ymin + (k - 1) / 100;
  const hi = PLOT.ymin + k / 100;
  if (!(applet.yScale(hi) < applet.yScale(lo))) {
    fail(`yScale does not decrease between ${lo} and ${hi}`);
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
        const wx = applet.xScale(i);
        const wy = applet.yScale(expected(i, e, a, d));
        if (!(Math.abs(x - wx) <= PX_TOL && Math.abs(y - wy) <= PX_TOL)) {
          fail(`path vertex ${i} is (${x}, ${y}) at ${at}, expected (${wx}, ${wy})`);
        }
      });
    }
  }
}

console.log(`check-ssm-applet: ${settings} slider settings, ${thetas.length} angles each: OK`);
