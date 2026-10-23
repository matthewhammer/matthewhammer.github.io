"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 2 of 26 of the game's script, run in order by index.html.

// ===================== HELPERS =====================

const idiv = (a, b) => Math.trunc(a / b);           // Hazel's Int division
const mod = (a, n) => a - idiv(a, n) * n;
const clamp = (n, lo = 0, hi = 100) => Math.max(lo, Math.min(hi, n));
const f1 = x => (Math.round(x * 10) / 10).toString();
const pts = ps => ps.map(([x, y]) => f1(x) + "," + f1(y)).join(" ");
// (ticket 151) a polygon grown outward by d, its corners mitred (a mitre
// held to 4 d, where a sharp corner would spike): what a stroke 2 d wide
// adds round a filled polygon, for a clip, which has no strokes
const growPoly = (ps, d) => {
  const n = ps.length;
  let a = 0;
  for (let i = 0; i < n; i++) { const [x1, y1] = ps[i], [x2, y2] = ps[(i + 1) % n]; a += x1 * y2 - x2 * y1; }
  const s = a > 0 ? 1 : -1, nrm = (p, q) => { const dx = q[0] - p[0], dy = q[1] - p[1], l = Math.hypot(dx, dy) || 1; return [s * dy / l, -s * dx / l]; };
  return ps.map((p, i) => {
    const n1 = nrm(ps[(i - 1 + n) % n], p), n2 = nrm(p, ps[(i + 1) % n]), c = 1 + n1[0] * n2[0] + n1[1] * n2[1];
    let ox = d * (n1[0] + n2[0]) / Math.max(c, 1e-6), oy = d * (n1[1] + n2[1]) / Math.max(c, 1e-6);
    const m = Math.hypot(ox, oy);
    if (m > 4 * d) { ox *= 4 * d / m; oy *= 4 * d / m; }
    return [p[0] + ox, p[1] + oy];
  });
};
function hexToRgb(h) { const n = parseInt(h.slice(1), 16); return [n >> 16, (n >> 8) & 255, n & 255]; }
function rgbToHex([r, g, b]) { return "#" + [r, g, b].map(v => Math.round(v).toString(16).padStart(2, "0")).join(""); }
// (ticket 258) a drawing marked with the procedure that drew it, for ♿ 🧩 what it's made of (madeOfHud): a group, its
// data-proc that name; nothing else changes
const drawnBy = (name, svg) => `<g data-proc="${name}">${svg}</g>`;
const mix = (h, to, t) => { const a = hexToRgb(h), b = hexToRgb(to); return rgbToHex(a.map((v, i) => v + (b[i] - v) * t)); };
const darker = h => mix(h, "#000000", 0.45);
const lighter = h => mix(h, "#ffffff", 0.45);

function line(x1, y1, x2, y2, w, color, outline) {
  const l = (ww, c) => `<line x1="${f1(x1)}" y1="${f1(y1)}" x2="${f1(x2)}" y2="${f1(y2)}" ` +
                       `stroke="${c}" stroke-width="${f1(ww)}" stroke-linecap="round"/>`;
  return (outline ? l(w + 5, outline) : "") + l(w, color);
}
// a limb through its joints (an arm: shoulder, elbow, hand): a straight
// one (two points) is a line, as ever; a bent one, a path round its joints
function limbThrough(P, w, color, outline) {
  if (P.length === 2) return line(P[0][0], P[0][1], P[1][0], P[1][1], w, color, outline);
  const d = "M" + P.map(([x, y]) => f1(x) + " " + f1(y)).join(" L");
  const l = (ww, c) => `<path d="${d}" fill="none" stroke="${c}" stroke-width="${f1(ww)}" stroke-linecap="round" stroke-linejoin="round"/>`;
  return (outline ? l(w + 5, outline) : "") + l(w, color);
}
const poly = (ps, fill, stroke, sw = 2.5) =>
  `<polygon points="${pts(ps)}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}" stroke-linejoin="round"/>`;

// A fill for a part: its color, or a pattern of it.
// A pattern's tile is 14 units drawn at `size`, its marks in `markColor`
// ("auto": a lighter shade of the cloth's own color).
function webTile(c) {
  // eight threads out from the middle to the outer ring, and four rings
  // across them, each sagging a little toward the middle between threads;
  // the webs stop short of each other
  const ends = [[0, 0], [7, 0], [14, 0], [14, 7], [14, 14], [7, 14], [0, 14], [0, 7]];
  const at = ([x, y], k) => [7 + (x - 7) * k, 7 + (y - 7) * k];
  let d = ends.map(e => `M7 7 L${at(e, 0.95).map(f1).join(" ")}`).join(" ");
  for (const f of [0.3, 0.55, 0.78, 0.95]) {
    d += " M" + at(ends[0], f).map(f1).join(" ");
    for (let i = 1; i <= 8; i++) {
      const a = ends[i - 1], b = ends[i % 8], m = at([(a[0] + b[0]) / 2, (a[1] + b[1]) / 2], f * 0.9);
      d += ` Q${f1(m[0])} ${f1(m[1])} ${at(b, f).map(f1).join(" ")}`;
    }
  }
  return `<path d="${d}" fill="none" stroke="${c}" stroke-width="0.25"/>`;
}
// One spider web, as the Hazel slide draws it: eight straight threads out
// from (cx, cy), r long, and three rings across them, straight between
// threads, at 35%, 65% and 95% of the way out.
function spiderWeb(cx, cy, r, c) {
  const at = (f, i) => [cx + f * r * Math.cos(i * Math.PI / 4), cy + f * r * Math.sin(i * Math.PI / 4)];
  let d = [0, 1, 2, 3, 4, 5, 6, 7].map(i => `M${f1(cx)} ${f1(cy)} L${at(1, i).map(f1).join(" ")}`).join(" ");
  for (const f of [0.35, 0.65, 0.95])
    d += " M" + [0, 1, 2, 3, 4, 5, 6, 7, 8].map(i => at(f, i).map(f1).join(" ")).join(" L");
  return `<path d="${d}" fill="none" stroke="${c}" stroke-width="1.2" stroke-linejoin="round" stroke-linecap="round"/>`;
}
function patternDef(id, color, kind, size = 14, markColor = "auto") {
  const lt = markColor === "auto" ? lighter(color) : markColor;
  const inner = {
    stripes: `<rect width="14" height="6" fill="${lt}"/>`,
    dots:    `<circle cx="7" cy="7" r="3" fill="${lt}"/>`,
    hearts:  `<text x="7" y="11" font-size="11" text-anchor="middle" fill="${lt}">♥</text>`,
    stars:   `<text x="7" y="11" font-size="11" text-anchor="middle" fill="${lt}">★</text>`,
    // a spider web: eight threads out from the middle, and three rings
    // across them, each sagging a little toward the middle between threads
    web:     webTile(lt),
    // (tickets 436, 437) ermine, a royal mantle's fur: white, with small black tails
    ermine: `<path d="M7 3.5 L7 9.5 M4.6 8.4 L7 10.8 L9.4 8.4" fill="none" stroke="${lt}" stroke-width="1.6" stroke-linecap="round"/>`,
    // (ticket 433) a dryad's cloak: a small leaf, its vein, sloping
    leaves: `<path d="M3 11 Q4 4 11 3 Q10 10 3 11 Z" fill="${lt}"/><path d="M3.6 10.4 L9.5 4.5" stroke="${color}" stroke-width="0.6"/>`,
    // (ticket 431) a gnome woman's skirt: five upright stripes, the first in its color, then the cover's green, yellow,
    // purple and blue
    gnomeskirt: GNOME_STRIPES.slice(1).map((c, i) => `<rect x="${f1(2.8 * (i + 1))}" width="2.8" height="14" fill="${c}"/>`).join(""),
    // (ticket 522) the prairie dress's gingham: two bands half way to the mark colour, the mark colour where they cross
    gingham: (h => `<rect x="7" width="7" height="14" fill="${h}"/><rect y="7" width="14" height="7" fill="${h}"/><rect x="7" y="7" width="7" height="7" fill="${lt}"/>`)(mix(color, lt, 0.5)),
  }[kind];
  if (!inner) return "";
  return `<pattern id="${id}" width="14" height="14" patternUnits="userSpaceOnUse" ` +
         `patternTransform="scale(${(size / 14).toFixed(3)})">` +
         `<rect width="14" height="14" fill="${color}"/>${inner}</pattern>`;
}
const fillOf = (id, color, kind) => (kind === "plain" ? color : `url(#${id})`);
