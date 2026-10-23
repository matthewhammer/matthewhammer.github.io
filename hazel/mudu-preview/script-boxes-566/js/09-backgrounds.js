"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0 AND BSD-2-Clause
// (ticket 178) Part 9 of 26 of the game's script, run in order by index.html.

// ===================== BACKGROUNDS =====================
// A scene behind the figure, in picture units: the frame runs from x0 to
// x1 and y0 to y1, and the figure stands on the ground at y = g, in the
// middle. Sizes are in k, a 700th of the frame's height, so a scene looks
// the same however big the figure's frame; its sky and ground run well
// past the frame (a blown lid can widen it). Things stand off to the
// sides, where the figure is not.
// Scenes are built from parts kept between draws, so that a slider builds
// again only what it changes (the lesson of the performance notes'
// issue 011, in small):
// - part(name, inputs, build): a pure function of the inputs it names,
//   kept under its name and those inputs ("named by its operands");
// - partByReads(name, opts, build): kept under the options it actually
//   READ while it was built, so a whole scene is built again only when
//   one of its own sliders moves, never for the figure's;
// - a thing a scene repeats (a duck, a toy, a tree house) is drawn once,
//   in <defs>, and placed with <use transform>: its size is one number
//   in that one drawing, and a count slider adds or drops one <use>;
// - a part in a group with a transform (the tree, the house, the couch)
//   keeps its own drawing when the group grows: its size slider builds
//   nothing again, it changes the transform.
// A scene's pieces are marked off in its text (SEP), and a group with a
// transform has its tags marked off too (OPEN, CLOSE): the page keeps each
// piece's element and uses it again while the piece's text is unchanged
// (mountScene, below), so a scene slider parses only the pieces it changed,
// and a size slider (a tree's, a house's, a couch's) only a group's tag.
const SEP = "\u0001", OPEN = tag => SEP + tag + SEP, CLOSE = SEP + "</g>" + SEP;
// (ticket 137: and a place seen from another, as a far view or a picture,
// has no ladders to climb: only the place itself does)
const unmarked = svg => svg.replaceAll(SEP, "").replaceAll(' data-ladder="1"', "");
// (ticket 99) A scene's objects, by depth. A scene is painted back to front.
// A thing standing in it (a tree, a table, the witch) is an object: named,
// and marked with its depth, the y where it stands on the ground (its foot,
// in the picture's own units, as the friend's feet are). The friend is
// drawn just before the first object standing nearer than their feet (a
// greater y), so they pass behind whatever stands in front of them. Flat
// things on the ground, and what is behind anywhere one can stand (the sky,
// far hills, a back wall), are not objects: they stay behind. An object
// always in front has the depth Infinity; something flying takes the depth
// of the ground under it. A scene's objects are put in order by depth when
// it is shown (sceneObjects), and in that order they are its list in the
// game, each one shown or hidden there
// (objectsOff). A name is plain words (no quotes or angle brackets), its
// own within its scene. Only an object outside any group with a transform
// (or a doorway, or a thing to drag) can have the friend stand in front of
// it; one inside another object is part of that one.
const obj = (name, z, svg) => OPEN(`<g data-obj="${name}" data-z="${Number.isFinite(z) ? f1(z) : "front"}">`) + svg + CLOSE;
// (ticket 284, Matthew: "wall and floor things become another kind of object in our system") A thing hanging on a wall or
// lying on the floor (a torch, a lever, a sign, cobwebs, moss, a sword dropped there) is an object too: named, in 🔬 Inspection's list
// with a 👁️ chip, hidden with it, lifted by 📦; but at the back (data-z "back", depth -Infinity): drawn where it is, the
// friend always in front of it, and standing in nobody's way (no collision box: a standing object needs a depth). Its kind,
// wall or floor, is its data-layer.
const wallObj = (name, svg) => OPEN(`<g data-obj="${name}" data-z="back" data-layer="wall">`) + svg + CLOSE;
const floorObj = (name, svg) => OPEN(`<g data-obj="${name}" data-z="back" data-layer="floor">`) + svg + CLOSE;
// (ticket 297, Matthew: "flowers (and all the other plants that grow \"up\" into the sky) layer correctly: one standing
// nearer than the friend's feet is drawn in front of the friend, one farther back behind") Things standing up out of the
// ground in their dozens, in rows by depth: items ({ y: where it stands, svg }) gathered into rows `band` deep, each an
// object at its row's middle, "<name> 1" the farthest, so the friend among them stands in front of the rows behind their
// feet and behind those in front. A row each, not an object each: a dozen or so groups a place (the 60 fps rule)
function depthRows(name, items, band) {
  const rows = new Map();
  for (const it of items) { const j = Math.floor(it.y / band); rows.set(j, (rows.get(j) || "") + it.svg); }
  return [...rows.keys()].sort((a, b) => a - b).map((j, i) => obj(`${name} ${i + 1}`, (j + 0.5) * band, rows.get(j))).join("");
}
// (the same round what a drawing added to its text t since it was m long: a thing drawn by statements of its own, wrapped where it is)
const wallSince = (t, m, name) => t.slice(0, m) + wallObj(name, t.slice(m)), floorSince = (t, m, name) => t.slice(0, m) + floorObj(name, t.slice(m));
// (ticket 252, Matthew: "for each tree-house trap door (defined by two houses, stacked) we get to choose the opening:
// triangle, square (current image), circle (like the pits in the castle). Same for the pits") An opening in a floor or
// a ceiling, seen in perspective: its middle (cx, cy), half its width (rx) and half its depth (ry). A floor's far edge
// is its top and the narrower; a ceiling's far edge is its bottom (ceiling). A triangle's point is at its far edge.
const OPENING_SHAPES = [["square", "■ square"], ["circle", "● circle"], ["triangle", "▲ triangle"]];
const openingShape = v => OPENING_SHAPES.some(([s]) => s === v) ? v : "square";
function openingD(shape, cx, cy, rx, ry, ceiling = false) {
  const far = ceiling ? cy + ry : cy - ry, near = ceiling ? cy - ry : cy + ry;
  if (shape === "circle") return `M${f1(cx - rx)} ${f1(cy)} A${f1(rx)} ${f1(ry)} 0 1 0 ${f1(cx + rx)} ${f1(cy)} A${f1(rx)} ${f1(ry)} 0 1 0 ${f1(cx - rx)} ${f1(cy)} Z`;
  if (shape === "triangle") return `M${f1(cx)} ${f1(far)} L${f1(cx + rx)} ${f1(near)} L${f1(cx - rx)} ${f1(near)} Z`;
  return `M${f1(cx - 0.82 * rx)} ${f1(far)} L${f1(cx + 0.82 * rx)} ${f1(far)} L${f1(cx + rx)} ${f1(near)} L${f1(cx - rx)} ${f1(near)} Z`;
}
// (a point on the floor (x, z) over a floor's opening: its share of the way out, under 1 inside)
function inOpening(shape, cx, cy, rx, ry, x, z) {
  const u = (x - cx) / rx, v = (z - cy) / ry;
  if (Math.abs(v) >= 1) return false;
  if (shape === "circle") return u * u + v * v < 1;
  if (shape === "triangle") return Math.abs(u) < (v + 1) / 2;   // (its point far, its base near)
  return Math.abs(u) < 0.82 + 0.18 * (v + 1) / 2;
}
const byDepth = list => list.slice().sort((a, b) => (a[1] - b[1]) || 0).map(([name, z, svg]) => obj(name, z, svg)).join("");
const PARTS = new Map();
function part(name, inputs, build) {
  const key = name + "|" + JSON.stringify(inputs);
  let v = PARTS.get(key);
  if (v === undefined) {
    if (PARTS.size > 400) PARTS.clear();
    PARTS.set(key, v = build());
  }
  return v;
}
const READS = new Map();
function partByReads(name, opts, build) {
  const kept = READS.get(name) || [];
  const hit = kept.find(e => Object.entries(e.reads).every(([key, v]) => opts[key] === v));
  if (hit) return hit.value;
  const reads = {};
  const value = build(new Proxy(opts, { get: (o, key) => (reads[key] = o[key]) }));
  kept.unshift({ reads, value });
  READS.set(name, kept.slice(0, 12));
  return value;
}
// A cauldron's fumes: a double helix, one green strand and one blue,
// rising from the brew to past the top of the picture, widening as it
// rises and turning round as you watch. Each strand is short segments; a
// segment swells and brightens as it turns toward you and thins and fades
// as it turns away, so the two pass in front of each other and behind.
function helixSvg(cx, bottom, top, r0, r1, k, colors = ["#7bd33a", "#4aa3ff"]) {
  const step = 13 * k, n = Math.max(2, Math.ceil((bottom - top) / step)), K = 12, dur = "2.4s";
  let t = "";
  colors.forEach((c, si) => {
    for (let j = 0; j < n; j++) {
      const u0 = j / n, u1 = (j + 1) / n, y1 = bottom - (bottom - top) * u0, y2 = bottom - (bottom - top) * u1;
      const ra = r0 + (r1 - r0) * u0, rb = r0 + (r1 - r0) * u1, w = (7 - 3 * u0) * k;
      const at = (f, jj) => jj * 0.42 + si * Math.PI - 2 * Math.PI * f / K;
      const xs = (r, jj) => [...Array(K + 1)].map((_, f) => f1(cx + r * Math.cos(at(f, jj)))).join(";");
      const ws = [...Array(K + 1)].map((_, f) => f1(w * (1 + 0.55 * Math.sin(at(f, j + 0.5))))).join(";");
      const os = [...Array(K + 1)].map((_, f) => (0.5 + 0.4 * Math.sin(at(f, j + 0.5))).toFixed(2)).join(";");
      // (ticket 118: the width and the see-through-ness, CSS properties, are
      // not SMIL: a browser restyles every SMIL animation of a CSS property
      // on every frame, and these, hundreds, held the witch's places to a
      // few frames a second. They are kept in data-helix and played as Web
      // Animations in time with the picture's clock: startHelix)
      t += `<line x1="${f1(cx + ra * Math.cos(at(0, j)))}" y1="${f1(y1)}" x2="${f1(cx + rb * Math.cos(at(0, j + 1)))}" y2="${f1(y2)}" stroke="${c}" stroke-linecap="round" stroke-width="${f1(w)}" stroke-opacity="0.8" data-helix="${ws}|${os}">` +
           `<animate attributeName="x1" values="${xs(ra, j)}" dur="${dur}" repeatCount="indefinite"/>` +
           `<animate attributeName="x2" values="${xs(rb, j + 1)}" dur="${dur}" repeatCount="indefinite"/></line>`;
    }
  });
  return t;
}
// (ticket 202, Matthew: "a simplified cauldron animation, with the same two
// fume plumes, but each drawn as a single path with a small number of points
// (and curves between), with a fat stroke color per curvy line") The fumes,
// simple: each plume one smooth path through thirteen points (Catmull-Rom, as
// cubic curves), turning as the full ones do: its shape at twelve moments of
// the 2.4 s turn, as theirs (ticket 205; six before), one SMIL animation of
// its d. (ticket 205, Matthew: closer to the full ones, "even if simpler")
// Their depth too, in part: a full plume's pieces brighten as they come round
// to the front and fade at the back (and swell and thin). Here the strand is
// painted with a band that repeats up it, bright where the strand is at the
// front: the turn's phase grows evenly with height, so one turn's worth of
// band, sliding up one turn's height every 2.4 s, stays on the front. The
// swelling too (a wide copy over it, painted so) cost 13 to 16% of the frames
// with 15 and 25 cauldrons on GitHub's computers; this, 3 to 7%. Two moving
// parts a plume, where the full ones have some hundred and twenty.
function helixCheapSvg(cx, bottom, top, r0, r1, k, colors = ["#7bd33a", "#4aa3ff"]) {
  const n = Math.max(2, Math.ceil((bottom - top) / (13 * k))), K = 12, N = 13, turn = 2 * Math.PI * (bottom - top) / (0.42 * n);
  const pathAt = (si, f) => {
    const pts = [...Array(N)].map((_, i) => { const u = i / (N - 1), a = u * n * 0.42 + si * Math.PI - 2 * Math.PI * f / K;
      return [cx + (r0 + (r1 - r0) * u) * Math.cos(a), bottom - (bottom - top) * u]; });
    let d = `M${f1(pts[0][0])} ${f1(pts[0][1])}`;
    for (let i = 0; i < N - 1; i++) {
      const p0 = pts[Math.max(0, i - 1)], p1 = pts[i], p2 = pts[i + 1], p3 = pts[Math.min(N - 1, i + 2)];
      d += ` C${f1(p1[0] + (p2[0] - p0[0]) / 6)} ${f1(p1[1] + (p2[1] - p0[1]) / 6)} ${f1(p2[0] - (p3[0] - p1[0]) / 6)} ${f1(p2[1] - (p3[1] - p1[1]) / 6)} ${f1(p2[0])} ${f1(p2[1])}`;
    }
    return d;
  };
  // (the band: at a height where the turn's phase is a, the strand is in front by sin(a): 0.9 there, 0.15 at the back, as the full ones;
  // ticket 511: seven stops, not thirteen, the same to the eye, 5 to 8% of fumes205's frames back on GitHub's slower computers)
  const band = si => [...Array(7)].map((_, i) => { const o = i / 6, front = (1 + Math.sin(2 * Math.PI * o + si * Math.PI)) / 2;
    return `<stop offset="${o.toFixed(3)}" stop-color="${colors[si]}" stop-opacity="${(0.15 + 0.75 * front).toFixed(2)}"/>`; }).join("");
  const tag = `${f1(cx)}-${Math.round(bottom)}-${Math.round(top)}-${f1(k)}`;
  return colors.map((c, si) => { const ds = [...Array(K + 1)].map((_, f) => pathAt(si, f)), id = `fume-${si}-${tag}`, g = `fume-band-${si}-${tag}`;
    return `<defs><linearGradient id="${g}" gradientUnits="userSpaceOnUse" spreadMethod="repeat" x1="0" y1="${f1(bottom)}" x2="0" y2="${f1(bottom - turn)}">${band(si)}` +
           `<animateTransform attributeName="gradientTransform" type="translate" values="0 0;0 ${f1(-turn)}" dur="2.4s" repeatCount="indefinite"/></linearGradient></defs>` +
           `<path id="${id}" d="${ds[0]}" fill="none" stroke="url(#${g})" stroke-width="${f1(5.5 * k)}" stroke-linecap="round" stroke-linejoin="round">` +
           `<animate attributeName="d" values="${ds.join(";")}" dur="2.4s" repeatCount="indefinite"/></path>`; }).join("");
}
// (ticket 118) The fumes' widths and see-through-ness as Web Animations, one
// per segment, started where the picture's clock is in its 2.4 s loop (the
// SMIL x1/x2 beside them run on that clock), so the two stay together.
// Segments kept from the last drawing keep theirs.
// (ticket 148) The fade is the segment's opacity (its stroke-opacity held at
// 1 while it plays, so it looks the same), and smooth; the width steps, 24
// times a loop. A stroke's width changing every frame, on these 240 or so
// segments, held the witch's hut to 20 to 33 frames a second in Chrome,
// standing or walking; this way, 60.
const HELIX_MS = 2400, HELIX_STEPS = 24;
function startHelix(root) {
  const pic = document.getElementById("pic"), now = pic && pic.getCurrentTime ? pic.getCurrentTime() * 1000 : 0;
  const paused = pic && pic.animationsPaused && pic.animationsPaused();
  for (const el of root.querySelectorAll("[data-helix]")) {
    if (el.__helix || !el.animate) continue;
    const [ws, os] = el.getAttribute("data-helix").split("|").map(v => v.split(";"));
    el.style.strokeOpacity = "1";
    el.__helix = [el.animate(os.map((o, i) => ({ opacity: o, offset: i / (os.length - 1) })), { duration: HELIX_MS, iterations: Infinity }),
                  el.animate(ws.map((w, i) => ({ strokeWidth: w + "px", offset: i / (ws.length - 1) })), { duration: HELIX_MS, iterations: Infinity, easing: `steps(${HELIX_STEPS})` })];
    for (const a of el.__helix) { a.currentTime = now % HELIX_MS; if (paused) a.pause(); }
  }
}
// a scene drawn inside another (a picture on a wall) keeps its ids apart
const prefixIds = (svg, p) => svg.replace(/(id="|url\(#|href="#)/g, `$1${p}`);
const tf = (x, y, s = 1, sy = s) => `transform="translate(${f1(x)} ${f1(y)}) scale(${+s.toFixed(4)} ${+sy.toFixed(4)})"`;
const use = (id, x, y, s = 1, sy = s, more = "") => `<use href="#${id}" ${tf(x, y, s, sy)}${more}/>`;
// (turning: its rays go slowly round, one ray's spacing a loop, and breathe)
// (o, the scene's options: its rays then pulse with the scene's quick LFO,
// each ray a twelfth of the way round after the last: sceneLfo)
const sunSvg = (x, y, r, turning = false, o = null) => (turning ? `<g><animateTransform attributeName="transform" type="rotate" from="0 ${f1(x)} ${f1(y)}" to="30 ${f1(x)} ${f1(y)}" dur="8s" repeatCount="indefinite"/>` : "") +
  [...Array(12)].map((_, i) => { const a = i * Math.PI / 6;
  const own = `<animate attributeName="stroke-width" values="${f1(r * 0.13)};${f1(r * 0.2)};${f1(r * 0.13)}" dur="${(2.4 + 0.3 * (i % 3)).toFixed(1)}s" repeatCount="indefinite"/>`;
  return `<line x1="${f1(x + r * 1.26 * Math.cos(a))}" y1="${f1(y + r * 1.26 * Math.sin(a))}" x2="${f1(x + r * 1.78 * Math.cos(a))}" y2="${f1(y + r * 1.78 * Math.sin(a))}" ` +
         `stroke="#ffd23f" stroke-width="${f1(r * 0.13)}" stroke-linecap="round"${turning ? `>${o ? sceneLfo(o, "fast", i / 12, "stroke-width", (v, amp) => f1(r * (0.13 + (0.04 + 0.14 * amp) * (1 + v) / 2)), own) : own}</line>` : "/>"}`; }).join("") +
  (turning ? "</g>" : "") + `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(r)}" fill="#ffd23f"/>`;
// (ticket 490, Matthew: "moon cycles. Seeing the moon accurately, during the cycle. Selecting a GPS coordinate and seeing the
// moon in the sky accurately at that GPS coordinate during the game's idea of its date (mapped to real world dates). sun
// cycles (winter, spring, summer, winter). Same idea"; "we'd want the length of day, and placement of the sun and moon to
// roughly indicate something real about those cycles, given our GPS coordinate that we choose, in the game (not from any
// real data in the device)") The sky where and when the game says: a place (one of SKY_PLACES, or 📍 its own latitude
// and longitude) and the game's date and hour (the sun's own time there, so noon is when it is highest), all settings, never
// the device's place or clock. The sun's and the moon's height and direction, and the moon's phase, worked out here (the
// usual short formulas, good to a degree or so: no network, so it works in airplane mode).
// (ticket 500) Those formulas follow SunCalc 1.9.0, https://github.com/mourner/suncalc/blob/v1.9.0/suncalc.js ,
// (c) 2011-2015 Vladimir (now Volodymyr) Agafonkin, under the BSD 2-Clause licence (its text is in NOTICE): skyNow's sun
// and moon positions and the moon's phase, and skySays's sunrise (its hour angle, at a height of -0.833 degrees), are
// SunCalc's sums, rewritten here. SunCalc's formulas come from Astronomy Answers (aa.quae.nl).
const SKY_PLACES = [["denver", "🏔️ Denver", 39.74, -104.99], ["newyork", "🗽 New York", 40.71, -74.01], ["london", "🎡 London", 51.51, -0.13],
  ["tokyo", "🗾 Tokyo", 35.68, 139.69], ["sydney", "🦘 Sydney", -33.87, 151.21], ["quito", "🌋 Quito, on the equator", -0.18, -78.47],
  ["reykjavik", "🧊 Reykjavík", 64.15, -21.94], ["tromso", "🌌 Tromsø, far north", 69.65, 18.96], ["ushuaia", "🐧 Ushuaia, far south", -54.8, -68.3],
  ["custom", "📍 my own place"]];
const skyPlaceOf = o => { const p = SKY_PLACES.find(q => q[0] === o.skyWhere && q[0] !== "custom");
  return p ? { lat: p[2], lon: p[3] } : { lat: Math.max(-89.9, Math.min(89.9, Number(o.skyLat ?? 39.74))), lon: Math.max(-180, Math.min(180, Number(o.skyLon ?? -104.99))) }; };
const skyKey = o => [o.skyWhere, o.skyLat, o.skyLon, o.skyYear, o.skyMonth, o.skyDay, o.skyHour].join("|");
let skyMemo = { key: null, now: null };
function skyNow(o) {
  const key = skyKey(o);
  if (skyMemo.key === key) return skyMemo.now;
  const { lat, lon } = skyPlaceOf(o), rad = Math.PI / 180, e = 23.4397 * rad;
  let Y = Math.round(o.skyYear ?? 2026), M = Math.round(o.skyMonth ?? 10), D = Math.min(Math.round(o.skyDay ?? 20), [31, (Y % 4 === 0 && (Y % 100 !== 0 || Y % 400 === 0)) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][M - 1] || 31);
  const hour = Math.max(0, Math.min(23.99, Number(o.skyHour ?? 12))), ut = hour - lon / 15;   // (the sun's time there, as the hour by Greenwich)
  if (M <= 2) { Y -= 1; M += 12; }
  const A = Math.floor(Y / 100), Bc = 2 - A + Math.floor(A / 4);
  const d = Math.floor(365.25 * (Y + 4716)) + Math.floor(30.6001 * (M + 1)) + D + Bc - 1524.5 + ut / 24 - 2451545;   // (days from noon, 2000-01-01, by Greenwich)
  const ra = (l, b) => Math.atan2(Math.sin(l) * Math.cos(e) - Math.tan(b) * Math.sin(e), Math.cos(l)), dec = (l, b) => Math.asin(Math.sin(b) * Math.cos(e) + Math.cos(b) * Math.sin(e) * Math.sin(l));
  const phi = lat * rad, lw = -lon * rad, sid = rad * (280.16 + 360.9856235 * d) - lw;
  const where = (a, de) => { const H = sid - a; return { alt: Math.asin(Math.sin(phi) * Math.sin(de) + Math.cos(phi) * Math.cos(de) * Math.cos(H)),
    az: Math.atan2(Math.sin(H), Math.cos(H) * Math.sin(phi) - Math.tan(de) * Math.cos(phi)) }; };   // (az from the south, toward the west)
  const Ms = rad * (357.5291 + 0.98560028 * d), Ls = Ms + rad * (1.9148 * Math.sin(Ms) + 0.02 * Math.sin(2 * Ms) + 0.0003 * Math.sin(3 * Ms)) + rad * 102.9372 + Math.PI;
  const sun = { ra: ra(Ls, 0), dec: dec(Ls, 0) };
  const Lm = rad * (218.316 + 13.176396 * d), Mm = rad * (134.963 + 13.064993 * d), Fm = rad * (93.272 + 13.22935 * d);
  const lm = Lm + rad * 6.289 * Math.sin(Mm), bm = rad * 5.128 * Math.sin(Fm), dist = 385001 - 20905 * Math.cos(Mm), moon = { ra: ra(lm, bm), dec: dec(lm, bm), dist };
  const sd = 149598000, ph = Math.acos(Math.sin(sun.dec) * Math.sin(moon.dec) + Math.cos(sun.dec) * Math.cos(moon.dec) * Math.cos(sun.ra - moon.ra));
  const inc = Math.atan2(sd * Math.sin(ph), dist - sd * Math.cos(ph)), ang = Math.atan2(Math.cos(sun.dec) * Math.sin(sun.ra - moon.ra), Math.sin(sun.dec) * Math.cos(moon.dec) - Math.cos(sun.dec) * Math.sin(moon.dec) * Math.cos(sun.ra - moon.ra));
  const now = { lat, lon, hour, sunDec: sun.dec, sun: where(sun.ra, sun.dec), moon: { ...where(moon.ra, moon.dec), frac: (1 + Math.cos(inc)) / 2, phase: 0.5 + 0.5 * inc * (ang < 0 ? -1 : 1) / Math.PI } };
  skyMemo = { key, now };
  return now;
}
// (ticket 490) the sky in words, for 🌍 where and when: the sun's height and direction and the day's length (from sunrise to
// sunset, the sun's top at the horizon: -0.83°), the moon's phase, how much of it is lit, its height and direction
function skySays(o) {
  const n = skyNow(o), deg = r => Math.round(r * 180 / Math.PI), rad = Math.PI / 180;
  const dir = az => ["north", "north-east", "east", "south-east", "south", "south-west", "west", "north-west"][Math.round(((az * 180 / Math.PI + 180) % 360 + 360) % 360 / 45) % 8];
  const phi = n.lat * rad, c = (Math.sin(-0.833 * rad) - Math.sin(phi) * Math.sin(n.sunDec)) / (Math.cos(phi) * Math.cos(n.sunDec));
  const day = c >= 1 ? "the sun does not rise today" : c <= -1 ? "the sun does not set today" : (m => `the day is ${Math.floor(m / 60)} h ${m % 60} min long`)(Math.round(2 * Math.acos(c) / rad / 15 * 60));   // (whole minutes first: never "9 h 60 min")
  const sa = deg(n.sun.alt), ma = deg(n.moon.alt), lit = Math.round(n.moon.frac * 100), p = n.moon.phase;
  const phase = lit < 3 ? "new" : lit > 97 ? "full" : (p < 0.5 ? "waxing" : "waning") + (lit < 42 ? " crescent" : lit > 58 ? " gibbous" : ", half");
  return `☀️ the sun: ${sa > 0 ? `${sa}° up, in the ${dir(n.sun.az)}` : `down, ${-sa}° below the horizon`}; ${day}. ` +
    `${["🌑", "🌒", "🌓", "🌔", "🌕", "🌖", "🌗", "🌘"][Math.round(p * 8) % 8]} the moon: ${phase}, ${lit}% lit, ${ma > 0 ? `${ma}° up, in the ${dir(n.moon.az)}` : "down"}.`;
}
// the time of day the sun makes (its height: above 6° day, below -6° night, between them dawn or dusk); a place with no
// sunrise of its own shows its sunset for dawn as well
const skyTimeOf = (o, sunrise = true) => { const a = skyNow(o).sun.alt * 180 / Math.PI, n = skyNow(o);
  return a > 6 ? "day" : a < -6 ? "night" : n.hour < 12 && sunrise ? "sunrise" : "sunset"; };
// where a body of the sky is in the picture, seen facing the equator (in the north, facing south: east on the left;
// in the south, facing north: east on the right): x across the picture's width by its direction, y from the horizon
// (h) up to the top (top) by its height; null when it is down, or behind us
// (the review after 0.0.385: facing where the noon sun is, not by the hemisphere alone: at Quito, just south of the equator,
// the sun is to the south from October to March, and facing north it was never drawn)
const skyFacesNorth = n => n.lat * Math.PI / 180 < n.sunDec;   // (latitude in degrees, the declination in radians: the review of 0.0.386)
function skyXY(c, body, h, top) {
  const south = skyMemo.now ? skyFacesNorth(skyMemo.now) : false, a = south ? ((body.az + 2 * Math.PI) % (2 * Math.PI)) - Math.PI : body.az;
  if (body.alt < -0.02 || Math.abs(a) > Math.PI / 2 * 1.08) return null;
  return [c.x0 + c.W / 2 + a / (Math.PI / 2) * (c.W / 2) * 0.92, h - Math.max(0, body.alt) / (Math.PI / 2) * (h - top)];
}
// the moon in its phase at (cx, cy), r: its dark part faint, its lit part (waxing lit on the right in the north, on the left
// in the south), the terminator an ellipse as wide as cos(2π·phase) says
function moonPhaseSvg(cx, cy, r, phase, dark, south = false) {
  const t = Math.cos(2 * Math.PI * phase), waxing = phase < 0.5, rx = Math.abs(t) * r, P = (x, y) => `${f1(x)} ${f1(y)}`;
  const outer = waxing ? `A${f1(r)} ${f1(r)} 0 0 1 ${P(cx, cy + r)}` : `A${f1(r)} ${f1(r)} 0 0 0 ${P(cx, cy + r)}`;
  const sweep = waxing ? (t > 0 ? 0 : 1) : (t > 0 ? 1 : 0);
  const lit = `M${P(cx, cy - r)} ${outer} A${f1(rx)} ${f1(r)} 0 0 ${sweep} ${P(cx, cy - r)} Z`;
  return `<g${south ? ` transform="translate(${f1(2 * cx)} 0) scale(-1 1)"` : ""}><circle cx="${f1(cx)}" cy="${f1(cy)}" r="${f1(r * 1.6)}" fill="#f4eebc" opacity="${(0.08 + 0.12 * (1 - Math.abs(t)) * (t < 0 ? 1 : 0.4)).toFixed(2)}"/>` +
    `<circle cx="${f1(cx)}" cy="${f1(cy)}" r="${f1(r)}" fill="${dark}" opacity="0.85"/><path d="${lit}" fill="#f4eebc"/></g>`;
}
// (ticket 490, part 2: "the other places' moons ... in the date's phase") a moon that is always out (the witch's woods, the
// round windows of the witch's hut and the hut's bedroom, the cat room's window): in the phase of the game's date where it
// says (🌍), at whatever time of day, at (cx, cy), r; the sky behind it dark. moonOfDateKey goes in its part's key.
const moonOfDate = o => { const n = skyNow(o); return { phase: Math.round(n.moon.phase * 100) / 100, south: skyFacesNorth(n) }; };
const moonOfDateKey = o => { const m = moonOfDate(o); return m.phase + (m.south ? "s" : "n"); };
const moonOfDateSvg = (o, cx, cy, r, dark) => { const m = moonOfDate(o); return moonPhaseSvg(cx, cy, r, m.phase, dark, m.south); };
// the sun and the moon by the clock, in a scene's sky (its horizon h, its top): the sun (low and orange near the horizon),
// the moon in its phase (faint by day); none where open says the sky is hidden there (a cliff, drawn under the night's tint)
function skyBodiesSvg(c, o, h, top, darkSky, open = null) {
  const n = skyNow(o), k = c.k, out = [], vis = p => p && (!open || open(p)) ? p : null, sp = vis(skyXY(c, n.sun, h, top)), mp = vis(skyXY(c, n.moon, h, top)), sa = n.sun.alt * 180 / Math.PI;
  if (mp) out.push(`<g opacity="${sa > 6 ? 0.55 : 1}">` + moonPhaseSvg(mp[0], mp[1], 22 * k, n.moon.phase, darkSky, skyFacesNorth(n)) + `</g>`);
  if (sp) out.push(sa > 8 ? sunSvg(sp[0], sp[1], 30 * k, true, o) : `<circle cx="${f1(sp[0])}" cy="${f1(sp[1])}" r="${f1(44 * k)}" fill="${sa > 2 ? "#ffd27a" : "#ff8c42"}" opacity="0.95"/>`);
  return out.join("");
}
// a puffy cloud, its size the drawing's own
const cloudDef = (id, puff) => `<g id="${id}"><g transform="scale(${+puff.toFixed(3)})">` +
  `<ellipse cx="0" cy="14" rx="92" ry="22" fill="#e4edf7"/>` +
  [[-62, 2, 30], [-28, -20, 38], [18, -30, 44], [58, -6, 32], [-4, 4, 36], [36, 8, 28]].map(([x, y, r]) => `<circle cx="${x}" cy="${y}" r="${r}" fill="#ffffff"/>`).join("") +
  `</g></g>`;
const cloudsSvg = (c, n, puff, seed) => part("clouds", [c.F, n, puff, seed, sceneLfoKey(c.o, "slow")], () => {
  const { x0, y0, W, H, k, R } = c;
  // (each drifting slowly back and forth, at its own pace)
  return `<defs>${cloudDef("bg-cloud", puff)}</defs>` + [...Array(n)].map((_, i) =>
    driftingCloud(x0 + W * (i + 0.2 + 0.6 * R(i, seed)) / n, y0 + H * (0.07 + 0.2 * R(i, seed + 1)), k * (0.7 + 0.5 * R(i, seed + 2)), i + seed, c.o)).join("");
});
const plank = (x, y, w, h, fill, line) => `<rect x="${f1(x)}" y="${f1(y)}" width="${f1(w)}" height="${f1(h)}" fill="${fill}" stroke="${line}" stroke-width="1.5"/>`;
const winSvg = (x, y, w, h) => `<rect x="${x - w / 2 - 9}" y="${y}" width="9" height="${h}" fill="#4a2e1a"/><rect x="${x + w / 2}" y="${y}" width="9" height="${h}" fill="#4a2e1a"/>` +
  `<rect x="${x - w / 2}" y="${y}" width="${w}" height="${h}" fill="#bfe3f5" stroke="#ffffff" stroke-width="5"/>` +
  `<path d="M${x} ${y} V${y + h} M${x - w / 2} ${y + h / 2} H${x + w / 2}" stroke="#ffffff" stroke-width="4"/>`;

// the lake's far shore (its horizon) and its near shore: where its water
// is, for its drawing and for a friend wading in it (WATER, ticket 122)
const lakeLines = ({ g, k }) => ({ horizon: g - 250 * k, shore: g - 70 * k });
// the little beach's top edge on the near shore: a curve from its left end
// to its right, bowed up in the middle
const lakeBeach = ({ W, k }, shore) => [[-W * 0.3, shore + 8 * k], [0, shore - 10 * k], [W * 0.3, shore + 8 * k]];
// the lake: sky, far wood, the lake with ducks, cattails at its edge, a
// little beach, and dirt with thin grass on the near shore
function lakeScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o, side } = c;
  const { horizon, shore } = lakeLines(c);
  const ducks = o.ducks ?? 5, dsz = (o.duckSize ?? 100) / 100, tails = o.cattails ?? 14;
  let s = SEP + part("lake-sky", [c.F, sceneLfoKey(o, "fast")], () => `<defs>${grad("bg-sky", "#58b7f0", "#dff3ff", y0, horizon)}</defs>` + all("url(#bg-sky)") +
                                       sunSvg(x0 + W * 0.14, y0 + H * 0.14, 40 * k, true, o));
  s += SEP + cloudsSvg(c, o.clouds ?? 4, (o.cloudSize ?? 100) / 100, 30);
  s += SEP + part("lake-water", c.F, () => {
    let t = `<defs>${grad("bg-lake", "#2f7fc2", "#79c6ea", horizon, shore)}</defs>` + land(horizon - 6 * k, 4, "#5c9c4c", 0.4);
    // the far shore: a wood of round trees
    // (leaf pads behind, so the wood is thick, then leafy trees and a few pines)
    // (the wood one object, standing on the far shore)
    t += `<defs>${leafyTreeDef("bg-lake-oak", 41)}${pineDef("bg-lake-pine", 43)}</defs>`;
    // (ticket 261: a pad or a tree more than 75% hidden by those in front of it, left out)
    const wood = [];
    for (let i = 0; i < 30; i++) { const j = Math.floor(R(i, 44) * 8), x = x0 - 30 * k + (W + 60 * k) * i / 29, y = horizon - 10 * k - 10 * k * R(i, 45), sx = k * 0.42 * (R(i, 46) < 0.5 ? -1 : 1);
      wood.push({ svg: padSvg(j, x, y, sx, k * 0.5, ["#2f6a32", "#356f33", "#2a5f2c"][i % 3]), shape: padOutline(j, x, y, sx, k * 0.5) }); }
    for (let i = 0; i < 24; i++) {
      const x = x0 - 20 * k + (W + 40 * k) * i / 23 + 12 * k * (R(i, 40) - 0.5), r = (22 + 18 * R(i, 41)) * k, sx = (r * 2.2 / 330) * (R(i, 48) < 0.5 ? -1 : 1);
      wood.push(R(i, 47) < 0.25 ? { svg: use("bg-lake-pine", x, horizon, r * 2.4 / 200, r * 2.4 / 200, ` color="${["#2f6e3a", "#356f33"][i % 2]}"`), shape: useOutline("bg-lake-pine", x, horizon, r * 2.4 / 200) }
                                : { svg: use("bg-lake-oak", x, horizon + 2 * k, sx, r * 2.2 / 330), shape: useOutline("bg-lake-oak", x, horizon + 2 * k, sx, r * 2.2 / 330) });
    }
    t += obj("far wood", horizon, seenOnly(wood, [[[x0 - pad, horizon], [x1 + pad, horizon], [x1 + pad, shore], [x0 - pad, shore]]]));
    t += `<rect x="${f1(x0 - pad)}" y="${f1(horizon)}" width="${f1(W + 2 * pad)}" height="${f1(shore - horizon + 14 * k)}" fill="url(#bg-lake)"/>`;
    const mSun = t.length;   // (ticket 284: the sun on the water one floor object)
    for (let i = 0; i < 18; i++) {   // the sun on the water
      const d = R(i, 43), x = x0 + W * R(i, 42), y = horizon + (shore - horizon) * (0.08 + 0.84 * d), l = (6 + 14 * d) * k;
      t += `<line x1="${f1(x - l)}" y1="${f1(y)}" x2="${f1(x + l)}" y2="${f1(y)}" stroke="#ffffff" stroke-width="${f1(2 * k)}" stroke-linecap="round" opacity="0.7"/>`;
    }
    t = floorSince(t, mSun, "sun on the water");
    const mLily = t.length;   // (ticket 284: the lily pads one object, on the water)
    for (let i = 0; i < 7; i++) {    // lily pads, a flower on some
      const d = R(i, 45), x = x0 + W * (0.05 + 0.9 * R(i, 44)), y = horizon + (shore - horizon) * (0.3 + 0.65 * d), r = (8 + 12 * d) * k;
      t += `<path d="M${P(x, y)} L${P(x + r, y - r * 0.12)} A${f1(r)} ${f1(r * 0.38)} 0 1 1 ${P(x + r, y + r * 0.12)} Z" fill="#4f9a3a" stroke="#3a7a2a" stroke-width="${f1(k)}"/>` +
           (i % 3 === 0 ? `<circle cx="${f1(x - r * 0.3)}" cy="${f1(y - r * 0.2)}" r="${f1(r * 0.3)}" fill="#f7a8c8"/>` : "");
    }
    t = floorSince(t, mLily, "lily pads");
    // the near shore: packed dirt, thin grass, and a little beach in the middle
    // (the shore's wave, 5 and 1.1, is WATER's too)
    const [ba, bq, bb] = lakeBeach(c, shore), beachTop = `M${P(...ba)} Q${P(...bq)} ${P(...bb)}`;
    t += land(shore, 5, "#b59a68", 1.1) +
         `<path d="${beachTop} L${P(W * 0.22, y1 + pad)} L${P(-W * 0.22, y1 + pad)} Z" fill="#ecd8a2"/>` +
         `<path d="${beachTop}" fill="none" stroke="#c9b27a" stroke-width="${f1(5 * k)}"/>`;
    const tufts = [];   // (ticket 284: the shore's grass in rows by depth; the beach's pebbles one floor object)
    for (let i = 0; i < 46; i++) {
      const sd = side(i), x = sd * (W * 0.27 + W * 0.25 * R(i, 46)), y = shore + 14 * k + (y1 - shore) * R(i, 47), h = (8 + 10 * R(i, 48)) * k;
      tufts.push({ y, svg: grassTuft(x, y, h * 1.2, 650 + i, "#6f9b3c", 1.6 * k) });
    }
    const mPeb = t.length;
    for (let i = 0; i < 14; i++) {   // pebbles on the beach
      const x = W * 0.36 * (R(i, 49) - 0.5), y = shore + 30 * k + (y1 - shore - 40 * k) * R(i, 50);
      t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1((3 + 4 * R(i, 51)) * k)}" ry="${f1(2.5 * k)}" fill="${["#9a9389", "#b8b0a3", "#7d776e"][i % 3]}"/>`;
    }
    t = floorSince(t, mPeb, "pebbles") + depthRows("grass", tufts, 12 * k);
    return t;
  });
  // ducks: one drawing, its size the slider's, and one <use> for each duck
  s += SEP + part("lake-ducks", [c.F, ducks, dsz, sceneLfoKey(o, "fast")], () => `<defs><g id="bg-duck"><g transform="scale(${+dsz.toFixed(3)})">` +
      `<ellipse cx="0" cy="3" rx="36" ry="5" fill="#ffffff" opacity="0.35"/>` +
      `<path d="M-30 -10 L-42 -22 L-28 -17 Z" fill="#2b2b2b"/>` +
      `<path d="M-32 -6 Q-36 -27 -12 -27 L14 -25 Q32 -22 29 -6 Q20 2 -2 2 Q-26 2 -32 -6 Z" fill="#9a958b"/>` +
      `<path d="M8 -6 Q20 -4 28 -11 Q31 -22 22 -25 Q12 -18 8 -6 Z" fill="#7a4a2a"/>` +
      `<path d="M-16 -20 Q4 -31 17 -16 Q2 -11 -16 -20 Z" fill="#6b5a48"/>` +
      `<path d="M13 -27 L27 -27 L29 -20 L12 -20 Z" fill="#1f7a3a"/><path d="M13 -25 L28 -25 L28 -22 L13 -22 Z" fill="#ffffff"/>` +
      `<circle cx="20" cy="-37" r="11" fill="#1f7a3a"/><path d="M29 -40 Q43 -38 42 -34 Q35 -32 29 -35 Z" fill="#f2b13a"/>` +
      `<circle cx="23" cy="-40" r="2.2" fill="#111"/></g></g></defs>` +
    // each paddling about on its own: out the way it faces, a pause while it
    // turns round, then back the other way, facing where it goes (never
    // swimming backwards); bobbing and rocking on the water as it goes
    [...Array(ducks)].map((_, i) => { const d = R(i, 52); return { d, i }; }).sort((a, b) => a.d - b.d).map(({ d, i }) => {
      const sd = R(i, 54) < 0.5 ? -1 : 1, z = 0.45 + 0.75 * d, x = x0 + W * (0.06 + 0.88 * R(i, 53)), y = horizon + 16 * k + (shore - horizon - 40 * k) * d;
      const go = sd * (24 + 36 * R(i, 55)) * k * z, bob = 2 * k * z, dur = (14 + 10 * R(i, 56)).toFixed(1);
      // (each duck an object, at the water line under it)
      return obj(`duck ${i + 1}`, y, `<g><animateTransform attributeName="transform" type="translate" values="0 0;${f1(go)} 0;${f1(go)} 0;0 0;0 0" keyTimes="0;0.44;0.5;0.94;1" ` +
             `calcMode="spline" keySplines="0.4 0 0.6 1;0 0 1 1;0.4 0 0.6 1;0 0 1 1" dur="${dur}s" repeatCount="indefinite"/>` +
             `<g transform="translate(${f1(x)} ${f1(y)})">` +
             `<g><animateTransform attributeName="transform" type="scale" values="1 1;-1 1" keyTimes="0;0.47" calcMode="discrete" dur="${dur}s" repeatCount="indefinite"/>` +
             `<g>${lfoTimed(o, "fast", R(i, 58), `<animateTransform attributeName="transform" type="translate" values="0 0;0 ${f1(-bob)};0 0;0 ${f1(bob)};0 0" dur="${(2.2 + 0.6 * R(i, 58)).toFixed(1)}s" repeatCount="indefinite"/>`)}` +
             `<g><animateTransform attributeName="transform" type="rotate" values="0;${(3 * sd).toFixed(1)};0;${(-2 * sd).toFixed(1)};0" dur="${(2.6 + 0.8 * R(i, 57)).toFixed(1)}s" repeatCount="indefinite"/>` +
             use("bg-duck", 0, 0, k * sd * z, k * z) + `</g></g></g></g></g>`);
    }).join(""));
  // cattails at the water's edge, either side
  // (each side's a row, one object at its middle depth)
  s += SEP + part("lake-cattails", [c.F, tails], () => `<defs><g id="bg-cattail">` +
      `<path d="M0 0 Q-14 -60 -30 -108 M2 0 Q16 -50 26 -94 M-2 0 Q-4 -70 -12 -136" fill="none" stroke="#5b8f35" stroke-width="4" stroke-linecap="round"/>` +
      `<line x1="0" y1="0" x2="2" y2="-152" stroke="#6f8f3a" stroke-width="3"/><rect x="-5" y="-132" width="13" height="44" rx="6.5" fill="#6b3f1f"/>` +
      `<line x1="1.5" y1="-132" x2="1.5" y2="-154" stroke="#7a6a3a" stroke-width="2"/></g></defs>` +
    [-1, 1].map(sd => {
      const row = [...Array(tails)].map((_, i) => side(i) !== sd ? "" :
        use("bg-cattail", side(i) * (W * 0.25 + W * 0.25 * R(i, 60)), shore + 10 * k * R(i, 61), k * (R(i, 63) < 0.5 ? -1 : 1) * (0.75 + 0.5 * R(i, 62)), k * (0.75 + 0.5 * R(i, 62)))).join("");
      return row && obj(sd < 0 ? "cattails, left" : "cattails, right", shore + 5 * k, row);
    }).join(""));
  return s;
}

// toys left on a lawn (and on a tree house's beds): one drawing each
const TOYS = ["ball", "truck", "bucket", "teddy", "wagon", "blocks", "frisbee", "rope"];
const toyDefs = `<g id="bg-toy-ball"><circle cx="0" cy="-12" r="12" fill="#e53935"/><path d="M-12 -12 Q0 -6 12 -12" fill="none" stroke="#ffffff" stroke-width="4"/><circle cx="-4" cy="-17" r="3" fill="#ffffff" opacity="0.6"/></g>` +
  `<g id="bg-toy-truck"><rect x="-26" y="-20" width="36" height="14" fill="#f2c12e" stroke="#9a7a12" stroke-width="1.5"/><path d="M10 -6 V-28 H20 L26 -18 V-6 Z" fill="#f2c12e" stroke="#9a7a12" stroke-width="1.5"/>` +
  `<path d="M13 -25 H19 L23 -18 H13 Z" fill="#bfe3f5"/><circle cx="-15" cy="-5" r="6" fill="#222"/><circle cx="16" cy="-5" r="6" fill="#222"/></g>` +
  `<g id="bg-toy-bucket"><path d="M-12 -26 H12 L9 0 H-9 Z" fill="#2e86de" stroke="#1b5fa8" stroke-width="1.5"/><path d="M-12 -26 Q0 -40 12 -26" fill="none" stroke="#1b5fa8" stroke-width="2"/>` +
  `<line x1="16" y1="-2" x2="26" y2="-30" stroke="#e53935" stroke-width="3"/><path d="M12 0 L22 2 L20 -8 Z" fill="#e53935"/></g>` +
  `<g id="bg-toy-teddy"><circle cx="-9" cy="-41" r="6" fill="#a86b3c"/><circle cx="9" cy="-41" r="6" fill="#a86b3c"/><ellipse cx="0" cy="-12" rx="14" ry="13" fill="#a86b3c"/>` +
  `<circle cx="0" cy="-33" r="11" fill="#a86b3c"/><ellipse cx="0" cy="-29" rx="5" ry="4" fill="#e3c39d"/><circle cx="-4" cy="-36" r="1.6" fill="#222"/><circle cx="4" cy="-36" r="1.6" fill="#222"/>` +
  `<ellipse cx="0" cy="-11" rx="8" ry="7" fill="#e3c39d"/></g>` +
  `<g id="bg-toy-wagon"><line x1="-30" y1="-14" x2="-50" y2="-34" stroke="#555" stroke-width="3"/><rect x="-30" y="-24" width="60" height="16" fill="#d63031" stroke="#8e1f1f" stroke-width="1.5"/>` +
  `<circle cx="-18" cy="-5" r="7" fill="#222"/><circle cx="18" cy="-5" r="7" fill="#222"/></g>` +
  `<g id="bg-toy-blocks"><rect x="-20" y="-16" width="16" height="16" fill="#e53935"/><rect x="-2" y="-16" width="16" height="16" fill="#2e86de"/><rect x="-11" y="-32" width="16" height="16" fill="#f2c12e"/></g>` +
  `<g id="bg-toy-frisbee"><ellipse cx="0" cy="-4" rx="15" ry="5" fill="#00b894" stroke="#00876c" stroke-width="1.5"/></g>` +
  `<g id="bg-toy-rope"><path d="M-26 -4 Q-10 8 0 -2 Q12 -12 26 -2" fill="none" stroke="#ff6fb5" stroke-width="3"/><rect x="-32" y="-8" width="8" height="5" rx="2" fill="#7c3aed"/><rect x="24" y="-6" width="8" height="5" rx="2" fill="#7c3aed"/></g>`;
// the tree house's stream: at u from 0 (far, on the hills) to 1 (past the
// frame's foot), its middle and half its width
const streamAt = ({ W, g, k, y1, pad }) => u => {
  const a = [W * 0.17, g - 125 * k], b = [W * 0.52, g - 40 * k], e = [W * 0.36, y1 + pad * 0.2];
  const x = (1 - u) * (1 - u) * a[0] + 2 * u * (1 - u) * b[0] + u * u * e[0], y = (1 - u) * (1 - u) * a[1] + 2 * u * (1 - u) * b[1] + u * u * e[1];
  return [x + 14 * k * Math.sin(u * 9), y, (5 + 95 * u * u + 20 * u) * k];
};
// a winding stream's points, n steps from its far end to its near one, as
// its outline is drawn (the friend wading in it reads the same: WATER)
const sampled = (along, n) => [...Array(n + 1)].map((_, i) => along(i / n));
// (ticket 42) Ripples carried down a stream: each starts where one is and in
// `dur` seconds moves on to where the next one starts, along the water and
// growing with its width (every part of a ripple, its line too, is in
// proportion to the width), so the row of them flows on and on with no jump
// when the loop begins again. along(u): the water's middle and half width at
// u; us: the ripples' places, far to near (the last only where the one before
// it ends up); draw(w): one ripple on water w wide, about the water's middle.
const flowRipples = (along, us, dur, draw) => us.slice(0, -1).map((u, i) => {
  const v = us[i + 1], K = 4, at = j => along(u + (v - u) * j / K), [x0, y0, w0] = at(0);
  const tr = [...Array(K + 1)].map((_, j) => { const [x, y] = at(j); return `${f1(x - x0)} ${f1(y - y0)}`; }).join(";");
  const sc = [...Array(K + 1)].map((_, j) => (at(j)[2] / w0).toFixed(3)).join(";");
  const anim = (type, values) => `<animateTransform attributeName="transform" type="${type}" values="${values}" dur="${dur}s" repeatCount="indefinite"/>`;
  return `<g class="flow" transform="translate(${f1(x0)} ${f1(y0)})"><g>${anim("translate", tr)}<g>${anim("scale", sc)}${draw(w0)}</g></g></g>`;
}).join("");
const inStream = (sp, x, y, margin) => {
  for (let i = 0; i <= 30; i++) { const [sx, sy, hw] = sp(i / 30); if (Math.abs(sy - y) < 12 * margin / 8 && Math.abs(sx - x) < hw + margin) return true; }
  return false;
};
// flowers: a tulip and a daisy, colored by their <use>
const FLOWER_DEFS = `<g id="bg-tulip" data-proc="tulip"><path d="M0 0 Q-2 -12 0 -22" stroke="#3f8a3a" stroke-width="2" fill="none"/><path d="M-6 -30 L-6 -20 Q0 -15 6 -20 L6 -30 L3 -25 L0 -31 L-3 -25 Z" fill="currentColor"/></g>` +
  `<g id="bg-daisy" data-proc="daisy"><path d="M0 0 V-20" stroke="#3f8a3a" stroke-width="2"/>` + [...Array(8)].map((_, i) => `<ellipse cx="0" cy="-27" rx="2.4" ry="6" fill="currentColor" transform="rotate(${i * 45} 0 -22)"/>`).join("") +
  `<circle cx="0" cy="-22" r="3" fill="#f2c12e"/></g>`;
const FLOWER_COLORS = ["#e53935", "#ffffff", "#f7a8c8", "#f2c12e", "#9b59b6", "#ff8c42"];
// (each id its own tree: a seed from its name)
const idSeed = id => [...id].reduce((a, ch) => (a * 31 + ch.charCodeAt(0)) % 9973, 7);
const oakDef = (id, s) => { const t = leafyTree(idSeed(id)); OUTLINE_OF.set(id, outlineAt(lastOutline, 0, 0, s)); return `<g id="${id}"><g transform="scale(${+s.toFixed(3)})">${t}</g></g>`; };

// a brown house of two floors, the garage on its right, a driveway and a
// stone walk, a tree each side, bushes, flowers and toys on the lawn
// the house and its garage, in the building's own units (its base's
// middle at 0, 0), for the house scene and seen from afar
function houseBuildingSvg() {
    let t = `<rect x="-220" y="-300" width="320" height="300" fill="#8a5a32"/>`;
    for (let y = -288; y < 0; y += 12) t += `<line x1="-220" y1="${y}" x2="100" y2="${y}" stroke="#74492a" stroke-width="1.5"/>`;
    t += `<rect x="-220" y="-300" width="8" height="300" fill="#6b4423"/><rect x="92" y="-300" width="8" height="300" fill="#6b4423"/>` +
         `<rect x="-226" y="-155" width="332" height="9" fill="#5e3b1f"/>` +
         `<rect x="30" y="-392" width="26" height="80" fill="#8e3b2e"/><rect x="26" y="-398" width="34" height="9" fill="#6f2c22"/>` +
         `<polygon points="-244,-296 124,-296 -60,-398" fill="#3d2b22"/>`;
    for (let y = -380; y < -296; y += 14) { const half = (y + 398) / 102 * 184; t += `<line x1="${f1(-60 - half)}" y1="${y}" x2="${f1(-60 + half)}" y2="${y}" stroke="#2c1f18" stroke-width="2"/>`; }
    t += `<circle cx="-60" cy="-338" r="15" fill="#bfe3f5" stroke="#ffffff" stroke-width="4"/>` +
         [-180, -60, 60].map(x => winSvg(x, -255, 48, 60)).join("") + [-170, 50].map(x => winSvg(x, -122, 58, 72)).join("") +
         // the front door, its little roof and its step
         `<g class="portal" data-door="" data-to="livingroom"><rect x="-86" y="-112" width="52" height="112" fill="#ffffff"/><rect x="-81" y="-107" width="42" height="107" fill="#7a1f1f"/>` +
         `<rect x="-72" y="-96" width="24" height="18" fill="#bfe3f5"/><circle cx="-46" cy="-50" r="3.5" fill="#f2c12e"/></g>` +
         `<polygon points="-100,-114 -20,-114 -60,-142" fill="#3d2b22"/><rect x="-96" y="0" width="72" height="9" fill="#9a958b"/>` +
         `<circle cx="-100" cy="-90" r="5" fill="#ffe9a8" stroke="#555" stroke-width="1.5"/>`;
    // the garage on the right
    t += `<rect x="100" y="-165" width="190" height="165" fill="#8a5a32"/>`;
    for (let y = -153; y < 0; y += 12) t += `<line x1="100" y1="${y}" x2="290" y2="${y}" stroke="#74492a" stroke-width="1.5"/>`;
    t += `<polygon points="88,-160 302,-160 195,-226" fill="#3d2b22"/><rect x="118" y="-132" width="154" height="132" fill="#f1ede4" stroke="#cfc8b8" stroke-width="2"/>` +
         [-99, -66, -33].map(y => `<line x1="118" y1="${y}" x2="272" y2="${y}" stroke="#cfc8b8" stroke-width="2"/>`).join("") +
         [0, 1, 2, 3].map(i => `<rect x="${128 + i * 37}" y="-124" width="26" height="14" fill="#bfe3f5"/>`).join("");
    // bushes along the front
    for (const [i, [x, r]] of [[-205, 26], [-160, 22], [-122, 20], [-4, 22], [36, 25], [76, 22]].entries())
      t += bushSvg(x, 0, r * 1.25, 400 + i);
    return t;
  }
// (ticket 50) the scenes' own "random", the same every time: backgroundSvg's
// R (keep the two alike), for the seats of places that scatter things
const sceneR = (i, j = 0) => { const v = Math.sin(i * 127.1 + j * 311.7) * 43758.5453; return v - Math.floor(v); };
// (ticket 50) the front step under the door: 72 of the building's units
// across (-96 to -24), its top level with the door's foot, on the building's
// base line, as houseBuildingSvg draws it; and each wagon on the lawn, a
// rider in its bed (60 across, its sides from 24 to 8 above the ground; the
// hips 18 up, a little down inside it), placed as houseScene places the toys
SEATS.house = (c, B) => { const { W, g, k, y1, pad } = c, hs = (B.houseSize ?? 100) / 100, toys = B.toys ?? 6;
  // (ticket 335: the building's base line, HOUSE_BASE, as houseScene's; 175 here till 0.0.298, while ticket 310 had moved it
  // to 150: the wagon's seat and box, and the front step, sat 25 units behind where they are drawn)
  const yb = g - HOUSE_BASE * k, bx = -40 * k, S = hs * k, X = lx => bx + lx * S;
  const seats = [{ name: "front step", x0: X(-96), x1: X(-24), y: yb, z: yb + 1, facing: ["front", "left", "right"] }];
  for (let i = 0; i < toys; i++) {
    if (TOYS[i % TOYS.length] !== "wagon") continue;
    const u = sceneR(i, 71), y = yb + 40 * k + (y1 - yb - 70 * k) * u, sc = k * (0.9 + 0.8 * u);
    let x = (i % 2 ? 1 : -1) * (W * 0.16 + W * 0.3 * sceneR(i, 70));
    const drive = X(120) + 30 * k * (y - yb) / (y1 + pad - yb);
    if (x > drive - 30 * k) x = -x;
    // (its handle is on its left in its own units, on its right when it is
    // drawn flipped: a rider faces the way it is pulled)
    const ahead = sceneR(i, 72) < 0.5 ? "right" : "left";
    seats.push({ name: i < TOYS.length ? "wagon" : `wagon ${Math.floor(i / TOYS.length) + 1}`, x0: x - 30 * sc, x1: x + 30 * sc, y: y - 18 * sc, z: y + 1,
                 facing: ["front", ahead] });
  }
  return seats; };
const HOUSE_BASE = 150;
function houseScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o, side } = c;
  const hs = (o.houseSize ?? 100) / 100, ts = (o.treeSize ?? 100) / 100, toys = o.toys ?? 6, flowers = o.flowers ?? 18;
  // (ticket 310: the house stands HOUSE_BASE above the ground line, forward of where it was (175), its lawn's far edge where
  // it always was, 205 up, so there is room to walk behind it to the back line at that edge)
  const yb = g - HOUSE_BASE * k, bx = -40 * k, S = hs * k;   // the building's base and its middle; its scale
  const X = lx => bx + lx * S;                          // a point on the building, in the scene
  let s = SEP + part("house-sky", c.F, () => `<defs>${grad("bg-sky", "#62b8ee", "#e2f4ff", y0, g - 175 * k)}</defs>` + all("url(#bg-sky)") +
                                        land(g - 205 * k, 6, "#8fc96c", 0.2) + land(g - 183 * k, 3, "#7cc25a", 0.7));
  s += SEP + cloudsSvg(c, 3, 0.8, 80);
  // the driveway from the garage door, the stone walk from the front door
  s += SEP + part("house-paths", [c.F, hs], () => {
    const gl = X(120), gr = X(270), d = X(-60);
    let t = `<polygon points="${P(gl, yb)} ${P(gr, yb)} ${P(gr + 150 * k, y1 + pad)} ${P(gl + 30 * k, y1 + pad)}" fill="#a3a09a"/>` +
            // (its joints, edge to edge: the edges spread over the whole way to y1 + pad, so at a
            // joint's height they are a share v of the way out, not u)
            [0.3, 0.6].map(u => { const y = yb + (y1 - yb) * u, v = (y - yb) / (y1 + pad - yb);
              return `<line x1="${f1(gl + 30 * k * v)}" y1="${f1(y)}" x2="${f1(gr + 150 * k * v)}" y2="${f1(y)}" stroke="#8a8780" stroke-width="${f1(2 * k)}"/>`; }).join("");
    for (let j = 0; j < 9; j++) for (let m = 0; m < 3; m++) {
      const u = (j + 0.5) / 9, cx = d + (0 - d) * u, cy = yb + 8 * k + (y1 - yb) * u, w = (50 * hs + 110 * u) * k, h = (8 + 20 * u) * k;
      const sx = cx + (m - 1) * w / 3 + w * 0.05 * (R(j * 3 + m, 90) - 0.5);
      let pts = "";
      for (let a = 0; a < 6; a++) {
        const ang = a * Math.PI / 3, rr = 0.8 + 0.25 * R(j * 18 + m * 6 + a, 91);
        pts += P(sx + Math.cos(ang) * w / 7 * rr, cy + Math.sin(ang) * h / 2 * rr) + " ";
      }
      t += `<polygon points="${pts}" fill="${["#a8a39a", "#8f8a80", "#bdb6aa"][(j + m) % 3]}" stroke="#6f6a60" stroke-width="${f1(1.2 * k)}"/>`;
    }
    return t;
  });
  // the trees beside the house: one drawing, its size the slider's
  // (each an object, where its trunk meets the lawn)
  s += SEP + part("house-trees", [c.F, hs, ts], () => `<defs>${oakDef("bg-oak", ts)}</defs>` +
    obj("tree, left", yb + 20 * k, use("bg-oak", Math.max(X(-300), x0 + 50 * k), yb + 20 * k, k)) +
    obj("tree, right", yb + 30 * k, use("bg-oak", Math.min(X(370), x1 - 50 * k), yb + 30 * k, k)));
  // flowers in the bed in front of the bushes (not the door's): two
  // drawings, colored by their <use>, in the building's group
  const bed = part("house-flowers", [flowers], () => `<defs>${FLOWER_DEFS}</defs>` +
    [...Array(flowers)].map((_, i) => use(i % 2 ? "bg-daisy" : "bg-tulip", (i % 16 < 8 ? -222 : -12) + (i % 8) * 14.5 + 4 * R(i, 95), 6 + 9 * Math.floor(i / 16), 1, 1,
      ` color="${["#e53935", "#ffffff", "#f7a8c8", "#f2c12e", "#9b59b6", "#ff8c42"][i % 6]}"`)).join(""));
  // the building, in its own units: its size is its group's transform
  // (the whole group one object, standing on its base line)
  s += obj("house", yb, OPEN(`<g id="bg-building" ${tf(bx, yb, S)}>`) + part("house-building", [], houseBuildingSvg) + SEP + bed + CLOSE);
  // toys on the lawn, either side of the walk, off the driveway
  // (each standing toy an object, nearer ones later; the frisbee and the
  // rope lie flat on the grass, so they go first, under the rest)
  s += SEP + part("house-toys", [c.F, hs, toys], () => {
    const flat = [], standing = [];
    for (let i = 0; i < toys; i++) {
      const u = R(i, 71), y = yb + 40 * k + (y1 - yb - 70 * k) * u, toy = TOYS[i % TOYS.length];
      let x = side(i) * (W * 0.16 + W * 0.3 * R(i, 70));
      const drive = X(120) + 30 * k * (y - yb) / (y1 + pad - yb);
      if (x > drive - 30 * k) x = -x;
      const t = use(`bg-toy-${toy}`, x, y, k * (0.9 + 0.8 * u) * (R(i, 72) < 0.5 ? -1 : 1), k * (0.9 + 0.8 * u));
      const name = i < TOYS.length ? toy : `${toy} ${Math.floor(i / TOYS.length) + 1}`;
      if (toy === "frisbee" || toy === "rope") flat.push(floorObj(name, t));   // (ticket 284: objects too, lying on the grass)
      else standing.push([name, y, t]);
    }
    return `<defs>${toyDefs}</defs>` + flat.join("") + byDepth(standing);
  });
  // butterflies, fluttering with the quick LFO
  // (at the depth of the ground under the nearest)
  s += obj("butterflies", g - 60 * k, part("house-butterflies", [c.F, sceneLfoKey(o, "fast"), sceneLfoKey(o, "wings")], () => butterfliesSvg(c, [[x0 + W * 0.15, g - 60 * k, 1], [x0 + W * 0.3, g - 100 * k, 0.9], [x0 + W * 0.84, g - 70 * k, 1.1]])));
  return s;
}

// a tree house: one great tree, houses on its branches, rope bridges
// between them and ladders down, in the tree's own units
const TREE_SLOTS = [[-245, -250, "fox"], [250, -290, "owl"], [-195, -420, "frog"], [210, -425, "bunny"],
                    [-345, -100, "bear"], [345, -150, "cat"]];
// Each tree house is an animal, like a costume's hood: its roof is the
// animal's head, its face above the door. One cabin drawn once (its walls
// the color of its <use>), one eye drawn once, and a head for each animal.
const HOUSE_WALLS = { fox: "#e09a5a", owl: "#b98a5a", frog: "#9cc27a", bunny: "#f2c9d6", bear: "#a8713d", cat: "#a79bbd",
                      cabin: "#c98f55" };
const houseEye = (x, y, r = 1) => `<use href="#bg-eye" ${tf(x, y, r)}/>`;
// and three houses that are only houses: a cabin (a plain gable), a fancy
// house (a steep scalloped roof, lacy trim, a finial, flower boxes) and a
// craftsman (a low wide gable, rafter tails, braces, tapered porch posts)
const scallops = (x0, x1, y, r, fill) => { let d = ""; for (let x = x0; x < x1 - 1; x += 2 * r) d += `M${x} ${y} a${r} ${r} 0 0 0 ${2 * r} 0 `; return `<path d="${d}" fill="${fill}"/>`; };
const HOUSE_STYLES = {
  cabin: () => `<polygon points="-66,-96 66,-96 0,-152" fill="#d64545" stroke="#00000055" stroke-width="2"/>` +
         `<rect x="30" y="-150" width="14" height="34" fill="#8e3b2e"/><rect x="-10" y="-130" width="20" height="18" fill="#bfe3f5" stroke="#ffffff" stroke-width="3"/>`,
  fancy: ({ fancyRoof: r, fancyTrim: t }) => `<polygon points="-70,-96 70,-96 0,-184" fill="${r}" stroke="${darker(r)}" stroke-width="2"/>` +
         [-110, -124, -138, -152, -166].map(y => { const half = (y + 184) / 88 * 70; return `<path d="${[...Array(Math.max(1, Math.floor(half / 7)))].map((_, j) => `M${f1(-half + j * 14)} ${y} a7 7 0 0 0 14 0`).join(" ")}" fill="none" stroke="${mix(r, "#ffffff", 0.2)}" stroke-width="2"/>`; }).join("") +
         scallops(-70, 70, -96, 5, t) + `<circle cx="0" cy="-132" r="13" fill="#bfe3f5" stroke="${t}" stroke-width="4"/><path d="M0 -145 V-119 M-13 -132 H13" stroke="${t}" stroke-width="2"/>` +
         `<line x1="0" y1="-184" x2="0" y2="-206" stroke="#f2c12e" stroke-width="3"/><circle cx="0" cy="-208" r="5" fill="#f2c12e"/>` +
         [-1, 1].map(sd => `<rect x="${sd * 34 - 14}" y="-54" width="28" height="7" fill="${t}"/>` +
           [-8, 0, 8].map((dx, j) => `<circle cx="${sd * 34 + dx}" cy="-56" r="3.5" fill="${["#e85d75", "#f2c12e", "#e53935"][j]}"/>`).join("")).join(""),
  craftsman: ({ craftRoof: r, craftTrim: t, craftGable: gb }) => `<polygon points="-80,-96 80,-96 0,-140" fill="${r}" stroke="${darker(r)}" stroke-width="2"/>` +
             `<polygon points="-56,-100 56,-100 0,-130" fill="${gb}"/>` + [-40, -24, -8, 8, 24, 40].map(x => `<line x1="${x}" y1="-100" x2="${x}" y2="${f1(-100 - 30 * (1 - Math.abs(x) / 56))}" stroke="${darker(gb)}" stroke-width="2"/>`).join("") +
             `<rect x="-18" y="-118" width="36" height="12" fill="#f2e6b5" stroke="${t}" stroke-width="2"/><path d="M-6 -118 V-106 M6 -118 V-106" stroke="${t}" stroke-width="2"/>` +
             [-1, 1].map(sd => [0, 1, 2].map(j => `<rect x="${f1(sd * (82 - j * 9) - 2.5)}" y="-98" width="5" height="9" fill="${darker(r)}"/>`).join("") +
               `<path d="M${sd * 52} -96 L${sd * 52} -78 L${sd * 70} -96" fill="none" stroke="${darker(r)}" stroke-width="3"/>` +
               `<polygon points="${sd * 22 - 5},-64 ${sd * 22 + 5},-64 ${sd * 22 + 7},-14 ${sd * 22 - 7},-14" fill="${t}" stroke="${darker(t)}" stroke-width="1.5"/>` +
               `<rect x="${sd * 22 - 9}" y="-14" width="18" height="14" fill="#8e8a80" stroke="#5e5a50" stroke-width="1.5"/>`).join("") +
             `<rect x="-30" y="-70" width="60" height="7" fill="${r}"/>`,
  // a cat's house: its fur, and for a tuxedo cat a white muzzle and a white
  // bib on the wall above the door
  cat: ({ catFur: f, catWhite: w, catEyes: e }) => {
    const white = w === "off" ? null : w, pale = parseInt(f.slice(1), 16) > 0xb0b0b0;
    return `<path d="M-66 -96 L-62 -140 L-54 -182 L-24 -152 L24 -152 L54 -182 L62 -140 L66 -96 Z" fill="${f}" stroke="#00000044" stroke-width="2"/>` +
      `<path d="M-52 -150 L-50 -170 L-34 -154 Z M52 -150 L50 -170 L34 -154 Z" fill="#f7a8c8"/>` +
      (white ? `<path d="M-32 -96 Q-32 -124 0 -126 Q32 -124 32 -96 Z" fill="${white}"/><path d="M-26 -100 Q0 -64 26 -100 Z" fill="${white}"/>` : "") +
      [-24, 24].map(x => `<ellipse cx="${x}" cy="-134" rx="10" ry="12" fill="${e}"/><ellipse cx="${x}" cy="-134" rx="3" ry="10" fill="#111"/>`).join("") +
      `<path d="M-6 -118 L6 -118 L0 -111 Z" fill="#ff8fb1"/>` +
      `<path d="M-10 -113 L-50 -119 M-10 -110 L-50 -107 M10 -113 L50 -119 M10 -110 L50 -107" stroke="${white || pale ? "#777777" : "#eeeeee"}" stroke-width="1.5"/>`;
  },
};
// a house's walls and roof (its head, for an animal): the colored kinds
// from the house's own colors, read only for its kind
function houseLook(kind, o, n) {
  const d = HOUSE_COLOR_DEFAULTS[kind];
  if (!d) return { walls: HOUSE_WALLS[kind], head: HOUSE_HEADS[kind] || HOUSE_STYLES[kind](), cols: [] };
  const c = Object.fromEntries(Object.entries(d).map(([k, v]) => [k, o[k + n] ?? v]));
  return { walls: c.catFur || c.fancyWalls || c.craftWalls, head: HOUSE_STYLES[kind](c), cols: Object.values(c) };
}
const HOUSE_HEADS = {
  fox: `<path d="M-66 -96 L-60 -146 L-44 -182 L-24 -150 L24 -150 L44 -182 L60 -146 L66 -96 Z" fill="#e8762c" stroke="#00000044" stroke-width="2"/>` +
       `<path d="M-52 -150 L-44 -172 L-34 -152 Z M52 -150 L44 -172 L34 -152 Z" fill="#3a2414"/>` +
       `<path d="M-62 -96 Q-30 -104 0 -122 Q30 -104 62 -96 Z" fill="#ffffff"/>` + houseEye(-24, -134, 0.9) + houseEye(24, -134, 0.9) +
       `<ellipse cx="0" cy="-120" rx="7" ry="5" fill="#1f1a17"/>`,
  owl: `<path d="M-66 -96 Q-70 -150 -44 -160 L-58 -184 L-26 -164 Q0 -170 26 -164 L58 -184 L44 -160 Q70 -150 66 -96 Z" fill="#8d6e4f" stroke="#00000044" stroke-width="2"/>` +
       `<circle cx="-24" cy="-128" r="22" fill="#e6cfa8"/><circle cx="24" cy="-128" r="22" fill="#e6cfa8"/>` +
       `<circle cx="-24" cy="-128" r="14" fill="#f2c12e"/><circle cx="24" cy="-128" r="14" fill="#f2c12e"/>` + houseEye(-24, -128, 1.1) + houseEye(24, -128, 1.1) +
       `<path d="M-8 -114 L8 -114 L0 -98 Z" fill="#e8762c"/>` +
       [-40, -20, 0, 20, 40].map(x => `<path d="M${x - 7} -104 Q${x} -96 ${x + 7} -104" fill="none" stroke="#5e4630" stroke-width="2"/>`).join(""),
  frog: `<path d="M-66 -96 Q-66 -138 0 -140 Q66 -138 66 -96 Z" fill="#4f9a3a" stroke="#00000044" stroke-width="2"/>` +
        `<circle cx="-36" cy="-142" r="21" fill="#4f9a3a"/><circle cx="36" cy="-142" r="21" fill="#4f9a3a"/>` + houseEye(-36, -144, 1.35) + houseEye(36, -144, 1.35) +
        `<path d="M-42 -112 Q0 -96 42 -112" fill="none" stroke="#2a5e22" stroke-width="4" stroke-linecap="round"/>` +
        `<circle cx="-48" cy="-110" r="7" fill="#f7a8c8" opacity="0.7"/><circle cx="48" cy="-110" r="7" fill="#f7a8c8" opacity="0.7"/>`,
  bunny: `<ellipse cx="-24" cy="-188" rx="13" ry="40" fill="#f4f4f4" stroke="#00000033" stroke-width="2"/><ellipse cx="24" cy="-188" rx="13" ry="40" fill="#f4f4f4" stroke="#00000033" stroke-width="2"/>` +
         `<ellipse cx="-24" cy="-186" rx="6" ry="30" fill="#f7a8c8"/><ellipse cx="24" cy="-186" rx="6" ry="30" fill="#f7a8c8"/>` +
         `<path d="M-66 -96 Q-68 -156 0 -160 Q68 -156 66 -96 Z" fill="#f4f4f4" stroke="#00000033" stroke-width="2"/>` + houseEye(-24, -132) + houseEye(24, -132) +
         `<path d="M-6 -120 L6 -120 L0 -113 Z" fill="#ff69b4"/><path d="M0 -113 V-108 M0 -108 Q-8 -102 -14 -106 M0 -108 Q8 -102 14 -106" fill="none" stroke="#555" stroke-width="2"/>` +
         `<rect x="-6" y="-107" width="6" height="8" fill="#ffffff" stroke="#999" stroke-width="1"/><rect x="0" y="-107" width="6" height="8" fill="#ffffff" stroke="#999" stroke-width="1"/>`,
  bear: `<circle cx="-50" cy="-152" r="17" fill="#7a4b28"/><circle cx="50" cy="-152" r="17" fill="#7a4b28"/><circle cx="-50" cy="-152" r="8" fill="#d9b38c"/><circle cx="50" cy="-152" r="8" fill="#d9b38c"/>` +
        `<path d="M-66 -96 Q-68 -162 0 -166 Q68 -162 66 -96 Z" fill="#7a4b28" stroke="#00000044" stroke-width="2"/>` + houseEye(-26, -138) + houseEye(26, -138) +
        `<ellipse cx="0" cy="-114" rx="24" ry="16" fill="#d9b38c"/><ellipse cx="0" cy="-121" rx="8" ry="6" fill="#1f1a17"/>` +
        `<path d="M0 -115 V-108 M-8 -104 Q0 -100 8 -104" fill="none" stroke="#1f1a17" stroke-width="2"/>`,
};
// the tree's other branches: from the trunk (sx, sy) out to a tip (tx, ty),
// with a fork to (fx, fy); a pad of leaves on each tip
const TREE_TIPS = [...Array(14)].map((_, j) => {
  const sd = j % 2 ? 1 : -1, r = v => { const z = Math.sin(j * 91.7 + v * 47.3) * 43758.5453; return z - Math.floor(z); };
  const sy = -190 - 28 * j, tx = sd * (130 + 340 * r(1)), ty = Math.max(-560, sy - 30 - 110 * r(2));
  return [sd * 38, sy, Math.round(tx), Math.round(ty), Math.round(tx * 0.75 + sd * 30), Math.max(-590, Math.round(ty - 50 - 40 * r(3)))];
});
// (ticket 261, Matthew: "check to see if we are drawing totally occluded objects -- remove them from the scene if they
// are more than 75% occluded (rough approx is fine)") What a drawing covers, as plain polygons in its own units (a leaf
// pad's outline; a pine's tiers and trunk; a leafy tree's trunk, limbs and pads), kept by its def's id for its <use>s
// (OUTLINE_OF); and seenOnly, which leaves out a drawing more than 75% inside the ones drawn after it (and `over`, what
// is drawn after them all), sampled over its polygons. tools/checks/occlude261.mjs measures what is left on the screen,
// and cull261.mjs what leaving them out changes, pixel by pixel.
const PAD_OUTLINE = [], OUTLINE_OF = new Map();
let CULL_HIDDEN = true;   // (off: everything drawn, as before; tools/checks/cull261.mjs compares the two)
const CULL_MOST = 0.75;   // (Matthew's: "more than 75% occluded")
const outlineAt = (polys, x, y, sx = 1, sy = sx) => polys.map(P => P.map(([a, b]) => [x + a * sx, y + b * sy]));
const useOutline = (id, x, y, s = 1, sy = s) => OUTLINE_OF.has(id) ? outlineAt(OUTLINE_OF.get(id), x, y, s, sy) : null;
// (inside by the nonzero rule, as SVG fills a polygon by default: a pine's tier crosses itself, and is filled whole)
const polyIn = (P, x, y) => { let w = 0;
  for (let i = 0, j = P.length - 1; i < P.length; j = i++) { const xi = P[i][0], yi = P[i][1], xj = P[j][0], yj = P[j][1];
    if ((yi > y) !== (yj > y) && x < (xj - xi) * (y - yi) / (yj - yi) + xi) w += yj > yi ? 1 : -1; }
  return w !== 0; };
const polyBox = P => { let a = Infinity, b = Infinity, c = -Infinity, d = -Infinity;
  for (const [x, y] of P) { if (x < a) a = x; if (x > c) c = x; if (y < b) b = y; if (y > d) d = y; } return [a, b, c, d]; };
// (items: [{ svg, shape, keep }] in drawing order, shape a list of polygons or null (kept, covering nothing); keep: never
// left out, but covering what is under it; their svgs, joined)
// (ticket 297: list, the items kept themselves, not their svgs joined)
function seenOnly(items, over = [], most = CULL_MOST, list = false) {
  if (!CULL_HIDDEN) return list ? items : items.map(it => it.svg).join("");
  const all = items.map(it => it.shape ? { svg: it.svg, polys: it.shape.map(P => ({ P, b: polyBox(P) })), src: it } : { svg: it.svg, polys: null, src: it });
  for (const it of all) if (it.polys) it.b = it.polys.reduce((a, q) => [Math.min(a[0], q.b[0]), Math.min(a[1], q.b[1]), Math.max(a[2], q.b[2]), Math.max(a[3], q.b[3])], [Infinity, Infinity, -Infinity, -Infinity]);
  const top = over.map(P => ({ P, b: polyBox(P) })), N = 6;
  const hit = (q, x, y) => x >= q.b[0] && x <= q.b[2] && y >= q.b[1] && y <= q.b[3] && polyIn(q.P, x, y);
  const kept = all.filter((it, i) => {
    if (!it.polys || items[i].keep) return true;
    const B = it.b, near = q => q.b[0] < B[2] && q.b[2] > B[0] && q.b[1] < B[3] && q.b[3] > B[1];
    const later = [...all.slice(i + 1).flatMap(o => o.polys && near(o) ? o.polys.filter(near) : []), ...top.filter(near)];
    if (!later.length) return true;
    // (sampled over each of its polygons' own boxes, about 100 points in all, those inside it counted: a pine is a
    // spike and jagged bands, which a grid over its whole box would mostly miss)
    // (kept as soon as more of it is seen than could ever be hidden enough: most drawings are plainly in view)
    let seen = 0, hid = 0; const m = Math.max(2, Math.round(Math.sqrt(N * N * 2 / it.polys.length))), sure = (1 - most) * it.polys.length * m * m;
    for (const q of it.polys) for (let a = 0; a < m; a++) for (let c = 0; c < m; c++) {
      const x = q.b[0] + (a + 0.5) * (q.b[2] - q.b[0]) / m, y = q.b[1] + (c + 0.5) * (q.b[3] - q.b[1]) / m;
      if (!polyIn(q.P, x, y)) continue;
      seen++; if (later.some(o => hit(o, x, y))) hid++; else if (seen - hid > sure) return true;
    }
    return !seen || hid <= most * seen;
  });
  return list ? kept.map(it => it.src) : kept.map(it => it.svg).join("");
}
// (ticket 261) rows of trees, far to near: each row an object ([name, z, items]), its trees ({ svg, shape }) more than
// 75% hidden by the rows in front of it and by `over` (polygons drawn after them all) left out
const treeRows = (rows, over = []) => rows.map(([name, z, items], r) =>
  obj(name, z, seenOnly(items, [...rows.slice(r + 1).flatMap(q => q[2].flatMap(it => it.shape || [])), ...over]))).join("");
// (what a scene's land(at, amp) covers for certain: below its wave's lowest crest)
const landCover = (at, amp, k, x0, x1, bottom) => [[x0, at + amp * k], [x1, at + amp * k], [x1, bottom], [x0, bottom]];
// a bonsai pad of leaves: a low cloud, a scalloped top over a flat bottom,
// shaded beneath and lit on top whatever its color (currentColor)
function padInner(j) {
  const r = v => { const z = Math.sin(j * 57.1 + v * 13.9) * 43758.5453; return z - Math.floor(z); };
  const w = 170 + 70 * r(1), h = 34 + 22 * r(2), bumps = 3 + j % 5, amp = 7 + 7 * r(3), lean = (r(4) - 0.5) * 0.6;
  const top = [], bottom = [];
  for (let i = 0; i <= 40; i++) {
    const u = i / 40, x = (u - 0.5) * w, env = Math.pow(Math.sin(Math.PI * Math.pow(u, 1 + lean)), 0.7);
    top.push([x, -h * env - amp * Math.abs(Math.sin(Math.PI * bumps * u)) * env]);
  }
  for (let i = 12; i >= 0; i--) { const u = i / 12; bottom.push([(u - 0.5) * w, 5 * Math.sin(Math.PI * u)]); }
  const pts = a => a.map(([x, y]) => `${f1(x)},${f1(y)}`).join(" ");
  const shade = top.map(([x, y]) => [x, y * 0.3]);
  const lit = top.filter((_, i) => i > 4 && i < 30).map(([x, y]) => [x * 0.7 - w * 0.06, y * 0.85 - 4]);
  PAD_OUTLINE[j] = [...top, ...bottom];   // (ticket 261)
  return `<polygon points="${pts([...top, ...bottom])}" fill="currentColor"/>` +
         `<polygon points="${pts([...shade, ...bottom])}" fill="#000000" opacity="0.2"/>` +
         `<polygon points="${pts(lit)}" fill="#ffffff" opacity="0.13"/>`;
}
const PAD_INNER = [...Array(8)].map((_, j) => padInner(j));
function padDef(id, j) { return `<g id="${id}" data-proc="padDef">${PAD_INNER[j]}</g>`; }
// a leaf pad anywhere: shape j (of 8), at (x, y), sx by sy, in a color
const padOutline = (j, x, y, sx, sy) => outlineAt([PAD_OUTLINE[((j % 8) + 8) % 8]], x, y, sx, sy);   // (ticket 261)
const padSvg = (j, x, y, sx, sy, color) => `<g data-proc="padSvg" transform="translate(${f1(x)} ${f1(y)}) scale(${sx.toFixed(3)} ${sy.toFixed(3)})" color="${color}">${PAD_INNER[((j % 8) + 8) % 8]}</g>`;
