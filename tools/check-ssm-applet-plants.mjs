// Shows that tools/check-ssm-applet.mjs can fail. Each plant writes a copy of
// vignettes/ssm-curve-applet.js with one defect to a temporary directory, runs
// the check on it, and requires exit 1 with the message of the rule that the
// defect breaks. An unchanged copy must pass. Exits 1 if any plant passes, or
// fails for another reason, or if the control fails.
//
//   node tools/check-ssm-applet-plants.mjs
//
// Run by .github/workflows/vignette-precompute.yaml after the check itself.

import { readFileSync, writeFileSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawnSync } from "node:child_process";

const source = readFileSync("vignettes/ssm-curve-applet.js", "utf8");
const dir = mkdtempSync(join(tmpdir(), "ssm-applet-plants-"));

const PLANTS = [
  ["sign of e", "return e + a * Math.cos", "return -e + a * Math.cos", "curve("],
  ["wrong a", "return e + a * Math.cos", "return e + 0.9 * a * Math.cos", "curve("],
  ["shifted d", "(theta - d) * Math.PI", "(theta - d - 5) * Math.PI", "curve("],
  ["degrees to cos", "Math.cos((theta - d) * Math.PI / 180)", "Math.cos(theta - d)", "curve("],
  ["flipped y", "(PLOT.ymax - y) /", "(y - PLOT.ymin) /", "yScale("],
  ["peak at a = 0", "if (a === 0) return [];", "if (a === 0) return [d];", "peaks is"],
  ["lone 0 peak", "return [0, 360];", "return [0];", "peaks is"],
  ["lone 360 peak", "return [0, 360];", "return [360];", "peaks is"],
  ["octant 0 for 360", "315, 360];", "315, 0];", "octant angles"],
  ["octant off curve", "y: curve(theta, e, a, d) };", "y: curve(theta, e, a, d) + 0.01 };", "off the curve"],
  ["flipped x", "PLOT.left + theta / 360", "PLOT.left + (360 - theta) / 360", "xScale("],
  ["nonlinear x", "PLOT.left + theta / 360", "PLOT.left + Math.pow(theta / 360, 1.1)", "xScale("],
  ["short path", "theta <= 360; theta++", "theta < 360; theta++", "vertices"],
  ["y range", "ymin: -1, ymax: 1", "ymin: -1.2, ymax: 1", "y range"],
  ["minus zero readout", "return r.toFixed(2);", "return x.toFixed(2);", "formatNumber("],
  ["d readout at a = 0", "a === 0 ? \"undefined\"", "false ? \"undefined\"", "formatDisplacement("],
  ["d slider from 0", "min: 5, max: 360, step: 5", "min: 0, max: 360, step: 5", "slider ranges"]
];

function run(name, text) {
  const file = join(dir, `${name.replace(/[^a-z0-9]+/gi, "-")}.js`);
  writeFileSync(file, text);
  return spawnSync(process.execPath, ["tools/check-ssm-applet.mjs", file], { encoding: "utf8" });
}

let bad = 0;
const control = run("control", source);
if (control.status !== 0) {
  console.error(`control: the unchanged applet fails the check\n${control.stderr}`);
  bad++;
}
for (const [name, from, to, message] of PLANTS) {
  if (source.split(from).length !== 2) {
    console.error(`${name}: the anchor occurs ${source.split(from).length - 1} times, expected once`);
    bad++;
    continue;
  }
  const result = run(name, source.replace(from, to));
  if (result.status !== 1 || !result.stderr.includes(message)) {
    console.error(`${name}: expected exit 1 with "${message}", got exit ${result.status}: ${result.stderr.trim()}`);
    bad++;
  } else {
    console.log(`${name}: caught (${result.stderr.trim().slice(18, 90)})`);
  }
}
rmSync(dir, { recursive: true, force: true });

if (bad > 0) {
  console.error(`check-ssm-applet-plants: ${bad} of ${PLANTS.length + 1} cases wrong`);
  process.exit(1);
}
console.log(`check-ssm-applet-plants: control passes, ${PLANTS.length} plants caught`);
