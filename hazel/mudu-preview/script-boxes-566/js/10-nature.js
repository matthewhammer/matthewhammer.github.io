"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 10 of 26 of the game's script, run in order by index.html.

// ---- the organic things every scene grows from: trees, pines, bushes,
// grass and rocks, all in the tree house's style (limbs and leaf pads, no
// lollipops), each from its own seed, so no two are alike ----
const seeded = seed => i => { const v = Math.sin(seed * 71.3 + i * 13.7) * 43758.5453; return v - Math.floor(v); };
// a curved, tapering limb: from p0 by way of p1 to p2, w0 wide to w1
// (ticket 261: its polygon, in its own units, onto `into` when given, for an outline)
const taperSvg = (p0, p1, p2, w0, w1, fill, into) => {
  const L = [], Rr = [];
  for (let i = 0; i <= 8; i++) {
    const u = i / 8, x = (1 - u) * (1 - u) * p0[0] + 2 * u * (1 - u) * p1[0] + u * u * p2[0], y = (1 - u) * (1 - u) * p0[1] + 2 * u * (1 - u) * p1[1] + u * u * p2[1];
    const dx = 2 * (1 - u) * (p1[0] - p0[0]) + 2 * u * (p2[0] - p1[0]), dy = 2 * (1 - u) * (p1[1] - p0[1]) + 2 * u * (p2[1] - p1[1]), d = Math.hypot(dx, dy) || 1;
    const w = (w0 + (w1 - w0) * u) / 2;
    L.push(`${f1(x - dy / d * w)},${f1(y + dx / d * w)}`); Rr.push(`${f1(x + dy / d * w)},${f1(y - dx / d * w)}`);
  }
  const pts = [...L, ...Rr.reverse()];
  if (into) into.push(pts.map(q => q.split(",").map(Number)));
  return `<polygon points="${pts.join(" ")}" fill="${fill}"/>`;
};
// (ticket 261) the outline of the last tree, pine or bush drawn (leafyTree, pineTree, bushSvg), for its def's <use>s
let lastOutline = [];
// A broadleaf tree, in the old oak's units (its foot at 0, 0, about 330
// tall): a trunk flaring at the roots, limbs forking up and out, and leaf
// pads clustered at their tips, darker behind and lighter in front
function leafyTree(seed, o = {}) {
  const r = seeded(seed), H = o.h ?? 320, wide = o.wide ?? 1, bark = o.bark ?? "#6b4a2b", dk = mix(bark, "#000000", 0.35);
  const greens = o.greens ?? ["#285e2a", "#2f7d32", "#3a8a3c", "#47a047", "#58b050"];
  const limbs = [], tips = [];
  const outline = [];
  const grow = (x, y, a, len, w, lvl, id) => {
    const bend = (r(id * 7 + 1) - 0.5) * 0.6, mx = x + Math.cos(a) * len * 0.5, my = y + Math.sin(a) * len * 0.5;
    const ex = mx + Math.cos(a + bend) * len * 0.5, ey = my + Math.sin(a + bend) * len * 0.5;
    limbs.push(taperSvg([x, y], [mx, my], [ex, ey], w, w * 0.6, bark, outline));
    if (lvl === 0) { tips.push([ex, ey, len]); return; }
    const n = 2 + (r(id * 7 + 2) > 0.45 ? 1 : 0);
    for (let c = 0; c < n; c++) grow(ex, ey, a + bend + (c - (n - 1) / 2) * (0.55 + 0.3 * r(id * 7 + 3 + c)) * wide, len * (0.6 + 0.18 * r(id * 11 + c)), w * 0.6, lvl - 1, id * 3 + c + 1);
  };
  const lean = (r(1) - 0.5) * 0.18, top = [Math.sin(lean) * H * 0.45, -H * 0.44];
  let t = `<ellipse cx="0" cy="2" rx="${f1(70 * wide)}" ry="11" fill="#000000" opacity="0.15"/>`;
  // roots, flaring into the ground
  for (const e of [-1, 1]) t += `<path d="M${f1(e * 10)} -24 Q${f1(e * 16)} -4 ${f1(e * (30 + 8 * r(2 + e)))} 2 L${f1(e * 6)} 2 Z" fill="${bark}"/>`;
  t += taperSvg([0, 2], [top[0] * 0.4, -H * 0.24], top, 30, 19, bark, outline);
  for (let c = 0; c < 3; c++) grow(top[0], top[1], -Math.PI / 2 + (c - 1) * 0.7 * wide + (r(10 + c) - 0.5) * 0.3, H * (0.25 + 0.06 * r(20 + c)), 16, 1, c + 2);
  t += limbs.join("") + `<path d="M${f1(-4)} -20 Q${f1(top[0] * 0.3 - 6)} ${f1(-H * 0.22)} ${f1(top[0] - 4)} ${f1(top[1] + 10)}" fill="none" stroke="${dk}" stroke-width="3" opacity="0.5"/>`;
  // the leaves: a back layer over the crown, then a cluster at each tip
  const pads = [];
  for (let i = 0; i < 4; i++) pads.push([top[0] + (r(30 + i) - 0.5) * 140 * wide, top[1] - 60 - 70 * r(34 + i), 0.55 + 0.2 * r(38 + i), 0]);
  tips.forEach(([x, y, len], i) => { pads.push([x + (r(40 + i) - 0.5) * 30, y - 16, 0.42 + 0.16 * r(50 + i), 1]); pads.push([x + (r(60 + i) - 0.5) * 40, y + 8, 0.36 + 0.14 * r(70 + i), 2]); });
  pads.sort((a, b) => a[3] - b[3] || a[1] - b[1]);
  // (ticket 261: a pad hidden by the pads in front of it, more than 75%, left out)
  const padItems = pads.map(([x, y, sc, layer], i) => { const j = Math.floor(r(80 + i) * 8), sx = sc * (r(90 + i) < 0.5 ? -1 : 1) * wide;
    return { svg: padSvg(j, x, y, sx, sc, greens[Math.min(greens.length - 1, layer + Math.floor(r(100 + i) * 3))]), shape: padOutline(j, x, y, sx, sc) }; });
  t += seenOnly(padItems);
  lastOutline = [...outline, ...padItems.flatMap(p => p.shape)];
  return drawnBy("leafyTree", t);
}
// a tree's drawing under an id, for <use> (one drawing, many trees)
const leafyTreeDef = (id, seed, o) => { const t = leafyTree(seed, o); OUTLINE_OF.set(id, lastOutline); return `<g id="${id}">${t}</g>`; };
// A pine, in the old pine's units (its foot at 0, 0, 200 tall, 100 wide at
// the bottom): a trunk, then tiers of drooping boughs, ragged at their
// tips, darker underneath, in the color of its <use>
function pineTree(seed, o = {}) {
  const r = seeded(seed), n = o.tiers ?? 5, outline = [[[-6, -34], [6, -34], [6, 2], [-6, 2]]];
  let t = `<rect x="-6" y="-34" width="12" height="36" fill="#5e3d22"/>`;
  for (let j = 0; j < n; j++) {
    // (broad tiers, overlapping, each drooping to three soft points a side)
    const u = j / n, y = -22 - u * 158, hw = 60 * (1 - u * 0.84), th = 62 - 22 * u, tip = y - th;
    const right = [];
    for (let q = 1; q <= 3; q++) { const v = q / 3, x = hw * v;
      right.push([x - hw / 6, y - 4 - 6 * r(j * 10 + q)], [x, y + 6 + 6 * r(j * 10 + q + 5) - 4 * v]); }
    const pts = [[0, tip], ...right, ...[...right].reverse().map(([x, yy]) => [-x * (0.94 + 0.1 * r(j + 40)), yy + 2 * (r(j + 41) - 0.5)])];
    const ps = pts.map(([x, yy]) => `${f1(x)},${f1(yy)}`).join(" ");
    outline.push(pts);
    t += `<polygon points="${ps}" fill="currentColor" stroke="currentColor" stroke-width="3" stroke-linejoin="round"/>` +
         `<polygon points="${f1(-hw * 0.85)},${f1(y + 2)} ${f1(hw * 0.85)},${f1(y + 2)} ${f1(hw * 0.5)},${f1(y - th * 0.3)} ${f1(-hw * 0.5)},${f1(y - th * 0.3)}" fill="#000000" opacity="0.16"/>` +
         `<polygon points="0,${f1(tip + 4)} ${f1(-hw * 0.6)},${f1(y - th * 0.15)} ${f1(-hw * 0.18)},${f1(y - th * 0.5)}" fill="#ffffff" opacity="0.1"/>`;
  }
  lastOutline = outline;
  return drawnBy("pineTree", t);
}
const pineDef = (id, seed = 1, o) => { const t = pineTree(seed, o); OUTLINE_OF.set(id, lastOutline); return `<g id="${id}">${t}</g>`; };
// a bush: a low mound of leaf pads, darker at its foot, flowers on some
const bushSvg = (x, y, rad, seed, o = {}) => {
  const r = seeded(seed), greens = o.greens ?? ["#285e2a", "#2f7d32", "#3a8a3c", "#4a9a46", "#5aad52"], sc = rad / 95;
  let t = `<ellipse cx="${f1(x)}" cy="${f1(y + 2 * sc)}" rx="${f1(rad * 1.1)}" ry="${f1(rad * 0.22)}" fill="#000000" opacity="0.16"/>`;
  const n = 4 + Math.floor(r(1) * 3), outline = [], items = [];
  for (let i = 0; i < n; i++) {
    const u = (i + 0.5) / n, px = x + (u - 0.5) * rad * 1.3 + (r(10 + i) - 0.5) * rad * 0.3, py = y - rad * (0.15 + 0.5 * Math.sin(Math.PI * u)) * (0.7 + 0.5 * r(20 + i));
    const j = Math.floor(r(30 + i) * 8), sx = sc * (0.5 + 0.25 * r(40 + i)) * (r(50 + i) < 0.5 ? -1 : 1), sy = sc * (0.6 + 0.3 * r(60 + i));
    items.push({ svg: padSvg(j, px, py, sx, sy, greens[Math.min(greens.length - 1, Math.floor(i / n * greens.length + r(70 + i)))]), shape: padOutline(j, px, py, sx, sy) });
    outline.push(...items[i].shape);
  }
  // (ticket 261: a pad more than 75% hidden by the bush's pads in front of it, left out)
  t += seenOnly(items);
  lastOutline = outline;
  if (o.flowers) for (let i = 0; i < 6; i++) { const fx = x + (r(80 + i) - 0.5) * rad * 1.4, fy = y - rad * (0.2 + 0.6 * r(90 + i));
    t += `<circle cx="${f1(fx)}" cy="${f1(fy)}" r="${f1(rad * 0.07)}" fill="${o.flowers[i % o.flowers.length]}"/><circle cx="${f1(fx)}" cy="${f1(fy)}" r="${f1(rad * 0.025)}" fill="#fde047"/>`; }
  return drawnBy("bushSvg", t);
};
// a tuft of grass: blades from one root, each leaning its own way
const grassTuft = (x, y, h, seed, col = "#4f9a3a", wd = 2) => {
  const r = seeded(seed), lit = mix(col, "#ffffff", 0.25), n = 5 + Math.floor(r(1) * 3);
  return drawnBy("grassTuft", [...Array(n)].map((_, i) => { const lean = (i / (n - 1) - 0.5) * 1.2 + (r(10 + i) - 0.5) * 0.4, hh = h * (0.6 + 0.4 * r(20 + i));
    return `<path d="M${f1(x + (i - n / 2) * wd * 0.6)} ${f1(y)} Q${f1(x + lean * hh * 0.25)} ${f1(y - hh * 0.6)} ${f1(x + lean * hh * 0.6)} ${f1(y - hh)}" fill="none" stroke="${i % 2 ? lit : col}" stroke-width="${f1(wd)}" stroke-linecap="round"/>`; }).join(""));
};
// a rock: an irregular stone, flat where it sits, its top lit, a crack
const rockSvg = (x, y, rad, seed, tone = "#9a9488") => {
  const r = seeded(seed), lit = mix(tone, "#ffffff", 0.25), dk = mix(tone, "#000000", 0.25), pts = [], n = 10;
  for (let i = 0; i < n; i++) { const a = Math.PI + i / (n - 1) * Math.PI, rr = rad * (0.8 + 0.3 * r(i));
    pts.push([x + Math.cos(a) * rr * (1.2 + 0.2 * r(20)), y + Math.min(0, Math.sin(a)) * rr * (0.75 + 0.2 * r(i + 10))]); }
  const top = pts.slice(1, 6).map(([px, py]) => [x + (px - x) * 0.7 - rad * 0.08, y + (py - y) * 0.85 - rad * 0.05]);
  const P_ = a => a.map(([px, py]) => `${f1(px)},${f1(py)}`).join(" ");
  return `<ellipse cx="${f1(x)}" cy="${f1(y + rad * 0.06)}" rx="${f1(rad * 1.35)}" ry="${f1(rad * 0.2)}" fill="#000000" opacity="0.18"/>` +
    `<polygon points="${P_(pts)}" fill="${tone}" stroke="${dk}" stroke-width="${f1(rad * 0.04)}" stroke-linejoin="round"/>` +
    `<polygon points="${P_([[x - rad * 0.9, y - rad * 0.1], ...top, [x + rad * 0.3, y - rad * 0.3]])}" fill="${lit}" opacity="0.7"/>` +
    `<path d="M${f1(x + rad * 0.2)} ${f1(y - rad * 0.55)} l${f1(rad * 0.15)} ${f1(rad * 0.25)} l${f1(-rad * 0.05)} ${f1(rad * 0.22)}" fill="none" stroke="${dk}" stroke-width="${f1(rad * 0.04)}"/>`;
};
// (ticket 50) a rock's top, to sit on: the span across of its four topmost
// points (the 4th to 7th of its outline's 10) and their mean height, as
// rockSvg places them (keep the two alike)
const rockTop = (x, y, rad, seed) => { const r = seeded(seed), w = 1.2 + 0.2 * r(20);
  const pts = [3, 4, 5, 6].map(i => { const a = Math.PI + i / 9 * Math.PI, rr = rad * (0.8 + 0.3 * r(i));
    return [x + Math.cos(a) * rr * w, y + Math.sin(a) * rr * (0.75 + 0.2 * r(i + 10))]; });
  return { x0: pts[0][0], x1: pts[3][0], y: pts.reduce((s, p) => s + p[1], 0) / 4 }; };
// where each house's ladder goes down to: the house below it (house 1's to
// house 5, 2's to 6, 3's to 1, 4's to 2), or the ground (-1) when there is
// no house below it
const LADDER_TO = [4, 5, 0, 1, -1, -1];
// a tree house's cabin (its walls the color of its <use>) and an animal's eye
// (the cabin's drawing by itself, too: the room sign's map draws it, ticket 244)
// (ticket 331: the door apart, CABIN_DOOR, so a house outside can draw its own and swing it open as the friend goes in)
const CABIN_BODY = `<rect x="-82" y="0" width="164" height="14" fill="#8b5e34" stroke="#5e3d22" stroke-width="2"/>` +
      [-80, -66, 66, 80].map(x => `<line x1="${x}" y1="0" x2="${x}" y2="-28" stroke="#8b5e34" stroke-width="4"/>`).join("") +
      `<path d="M-82 -28 H-52 M52 -28 H82" stroke="#8b5e34" stroke-width="4"/>` +
      `<rect x="-52" y="-100" width="104" height="100" fill="currentColor"/>` +
      [-88, -76, -64, -52, -40, -28, -16].map(y => `<line x1="-52" y1="${y}" x2="52" y2="${y}" stroke="#00000030" stroke-width="1.5"/>`).join("") +
      [-1, 1].map(sd => `<rect x="${sd * 34 - 11}" y="-78" width="22" height="24" fill="#bfe3f5" stroke="#ffffff" stroke-width="3"/>`).join("") +
      "";
// (its door: shut, and swung in, the same commands, data-open, as a castle door's leaf: walkInto's and platformIn's leafOpen)
const CABIN_DOOR_D = "M-15 0 V-56 Q0 -72 15 -56 V0 Z", CABIN_DOOR_OPEN = "M-15 0 V-56 Q-12 -63 -8 -55 V-1 Z";
const CABIN_DOOR = `<path d="${CABIN_DOOR_D}" fill="#7a4b28" stroke="#4a2e1a" stroke-width="2"/><ellipse cx="9" cy="-28" rx="2.5" ry="2.5" fill="#f2c12e"/>`;
const CABIN_SVG = CABIN_BODY + CABIN_DOOR;
const CABIN_DEFS = `<g id="bg-cabin">${CABIN_SVG}</g><g id="bg-cabin-body">${CABIN_BODY}<path d="${CABIN_DOOR_D}" fill="#24150b"/></g>` +
      `<g id="bg-eye"><circle cx="0" cy="0" r="9" fill="#ffffff" stroke="#00000055" stroke-width="1.5"/><circle cx="1" cy="1" r="5" fill="#1f1a17"/><circle cx="-1.5" cy="-1.5" r="1.8" fill="#ffffff"/></g>`;
// the room inside each tree house
const HOUSE_ROOMS = ["bunks", "kitchen", "books", "lookout", "art", "music"];
const TREE_BRIDGES = [[2, 3], [0, 1], [4, 5], [0, 3], [2, 1]];   // across the tree
// (ticket 516) The crown's limbs, as the tree draws them (tree-crown), j from 0 up the trunk: each its side (sd, even
// left, odd right), the height it leaves the trunk at (sy), its tip (tx, ty) along a quadratic through (tx * 0.45, sy - 20),
// a fork to (fx, fy), its width (w). The round room at the top of the tree has the same limbs inside (crownLimbs j 0..7)
// (the scenes' own seeded numbers, backgroundSvg's R: for the map on a sign, drawn outside a scene)
const crownR = (i, j = 0) => { const v = Math.sin(i * 127.1 + j * 311.7) * 43758.5453; return v - Math.floor(v); };
const crownLimbs = R => [...Array(14)].map((_, j) => {
  const sd = j % 2 ? 1 : -1, sy = -780 - 34 * j, tx = sd * (160 + 470 * R(j, 230)), ty = sy - 80 - 220 * R(j, 231);
  return { j, sd, sy, tx, ty, fx: tx * 0.7 + sd * 60, fy: ty - 90 - 60 * R(j, 232), w: 26 - j };
});
// a crown limb and its fork, as tree-crown draws them
const crownLimbSvg = L => `<path d="M${L.sd * 20} ${L.sy} Q${f1(L.tx * 0.45)} ${f1(L.sy - 20)} ${f1(L.tx)} ${f1(L.ty)}" fill="none" stroke="#6e4a2c" stroke-width="${f1(L.w)}" stroke-linecap="round"/>` +
  `<path d="M${f1(L.tx * 0.6)} ${f1((L.sy + L.ty) / 2 - 10)} L${f1(L.fx)} ${f1(L.fy)}" fill="none" stroke="#6e4a2c" stroke-width="11" stroke-linecap="round"/>`;
// The great tree, in its own units (standing at 0, 0): its crown, branches,
// braided trunk, leaves, swing, bridges, ladders and houses, all as the
// sliders and menus say. The tree house scene draws it, and so does the
// forest clearing, small and far off: the same tree.
// (ticket 335: houses false, the tree house scene's own: its houses are objects of their own there, treeHouseParts)
function treeInner(c, houses = true) {
  const { o, R } = c;
  const n = Math.max(1, Math.min(6, o.houses ?? 4));
  // the houses' own size, apart from the tree's: they grow about where
  // they sit, and the bridges and ladders keep to their platforms
  const hz = (o.treeHouseSize ?? 100) / 100;
  const bridges = o.bridges ?? 2, ladders = o.ladders ?? 3, leaves = o.leaves ?? 60;
  let s = "";
  // the crown, high above the frame at the tree's usual size (its top is
  // about 600 of the tree's units up): seen only when the tree is small
  // (ticket 516: its limbs, crownLimbs, shared with the round room at the top of the tree, inside and out; its leaf pads a
  // part of their own, drawn after that room, over the limbs' tips where they come out of its thatch)
  s += SEP + part("tree-crown", [], () => `<polygon points="-62,-720 -26,-1290 26,-1290 62,-720" fill="#5e3d22" stroke="#3a2414" stroke-width="2"/>` +
    [-30, -8, 14].map(x => `<path d="M${x} -730 Q${x + 10} -950 ${x / 2} -1280" fill="none" stroke="#3a2414" stroke-width="3" opacity="0.5"/>`).join("") +
    crownLimbs(R).map(L => crownLimbSvg(L)).join(""));
  // more branches, up the trunk and out each side, a fork on each
  s += SEP + part("tree-branches", [], () => TREE_TIPS.map(([sx, sy, tx, ty, fx, fy]) =>
    `<path d="M${sx} ${sy} Q${f1((sx + tx) / 2)} ${f1(sy - 6)} ${tx} ${ty}" fill="none" stroke="#6e4a2c" stroke-width="13" stroke-linecap="round"/>` +
    `<path d="M${f1((sx + 2 * tx) / 3)} ${f1((sy + 2 * ty) / 3 - 4)} L${fx} ${fy}" fill="none" stroke="#6e4a2c" stroke-width="7" stroke-linecap="round"/>`).join(""));
  s += SEP + part("tree-trunk", [n], () =>
    // (ticket 241, Matthew: "there's no collision info for the outside tree house tree trunk system") the braid
    // marked, data-trunk: the tree house's box is its trunk's alone (standingHere), not its houses', bridges' or ladders'
    // (ticket 345, Matthew: "the base of the tree house trunk system is very, very curvy -- it's good, but we need more
    // fine-grained boxes there in collision info") and five bands more in its lowest third, data-base (trunkBands)
    treeLimbs(n) + treeRoots() + `<g data-trunk="" data-base="5">` + treeBraid() + `</g>`);
  // (ticket 516) the round room at the top of the tree, wrapped round the trunk where the braid ends and the crown's limbs
  // begin, the limbs coming out through its thatch; the ladder up to it from the house below (topHouse)
  s += SEP + part("tree-top", [n, hz, topHouse(o), o.windowSize ?? 100], () => treeTopSvg(o, R));
  s += SEP + part("tree-crown-pads", [leaves], () => {
    const tips = crownLimbs(R).flatMap(L => [[L.tx, L.ty], [L.fx, L.fy]]);
    tips.push([0, -1300], [-120, -1360], [120, -1350], [0, -1420]);
    // (ticket 261: a pad more than 75% hidden by the pads drawn after it, left out)
    return seenOnly([...Array(Math.round(leaves * 0.8))].map((_, i) => {
      const [x, y] = tips[i % tips.length], more = Math.floor(i / tips.length), sc = 1 + 1.1 * R(i, 233), j = Math.floor(R(i, 234) * 8);
      const px = x + more * 70 * (R(i, 235) - 0.5) * 2, py = y + 10 - more * 50, sx = sc * (R(i, 236) < 0.5 ? -1 : 1);
      return { svg: use(`bg-pad${j}`, px, py, sx, sc, ` color="${["#2a6e2e", "#2f7d32", "#3c9a40", "#266b30", "#4fb04f"][Math.floor(R(i, 237) * 5)]}"`), shape: padOutline(j, px, py, sx, sc) };
    }));
  });
  // leaves: bonsai pads, eight shapes drawn once, each used again and
  // again, grown, flipped and colored by its <use> (0 leaves: bare)
  s += SEP + part("tree-pad-shapes", [], () => `<defs>${[...Array(8)].map((_, j) => padDef(`bg-pad${j}`, j)).join("")}</defs>`);
  s += SEP + part("tree-pads", [leaves], () => {
    const tips = TREE_TIPS.flatMap(([, , tx, ty, fx, fy]) => [[tx, ty], [fx, fy]]);
    // (ticket 261: a pad more than 75% hidden by the pads drawn after it, left out)
    return seenOnly([...Array(Math.round(leaves * 0.56))].map((_, i) => {
      const [x, y] = tips[i % tips.length], more = Math.floor(i / tips.length), sc = 0.55 + 0.75 * R(i, 124), j = Math.floor(R(i, 125) * 8);
      const px = x + more * 34 * (R(i, 126) - 0.5) * 2, py = y + 6 - more * 26, sx = sc * (R(i, 127) < 0.5 ? -1 : 1);
      return { svg: use(`bg-pad${j}`, px, py, sx, sc, ` color="${["#2f7d32", "#3c9a40", "#4fb04f", "#2a6e2e", "#5aa84a"][Math.floor(R(i, 128) * 5)]}"`), shape: padOutline(j, px, py, sx, sc) };
    }));
  });
  s += SEP + part("tree-swing", [sceneLfoKey(o, "slow")], () =>
    `<path d="M30 -300 Q200 -330 430 -350" fill="none" stroke="#6e4a2c" stroke-width="16" stroke-linecap="round"/>` +
    `<path d="M300 -338 Q330 -380 350 -400" fill="none" stroke="#6e4a2c" stroke-width="7" stroke-linecap="round"/>` +
    `<g>${lfoTimed(o, "slow", 0.15, `<animateTransform attributeName="transform" type="rotate" values="-14 395 -346;14 395 -346;-14 395 -346" dur="2.6s" repeatCount="indefinite" calcMode="spline" keySplines="0.45 0 0.55 1;0.45 0 0.55 1"/>`)}` +
    `<path d="M375 -344 L372 -70 M415 -348 L418 -70" stroke="#d8c49a" stroke-width="3.5"/>` +
    `<rect data-swing-seat="" x="358" y="-72" width="74" height="12" rx="4" fill="#a0703f" stroke="#5e3d22" stroke-width="2"/></g>`);   // (ticket 240: the seat, found as it swings: swingNow)
  s += SEP + part("tree-bridges", [n, bridges, hz], () => treeBridges(n, bridges, hz));
  s += SEP + part("tree-ladders", [n, ladders, hz], () => TREE_SLOTS.slice(0, Math.min(n, ladders)).map((_, i) => {
    // (ticket 137: one down to the ground can be climbed, up to house i + 1;
    // one between houses is out of reach from the ground)
    const t = treeLadder(n, i, hz);
    return t.ground ? `<g data-ladder="1" data-house="${i + 1}">${t.svg}</g>` : t.svg;
  }).join(""));
  // the houses: one drawing, its roof the color of its <use>
  s += SEP + part("tree-cabin", [], () => `<defs>${CABIN_DEFS}</defs>`);
  if (houses) s += treeHouseParts(o, n, hz).map(h => h.svg).join("");
  return s;
}
// (each house's drawing, in the tree's units, highest first: a lower house covers the one above it; y, its platform's height)
const treeHouseParts = (o, n, hz) => treeHouses(o, n, hz).map(({ x, y, i, animal, look, sc }) => ({ i, y,
  svg: part("tree-house", [i, animal, look.cols, sc], () => `<g class="portal" data-to="treeroom" data-house="${i + 1}">` +
    use("bg-cabin-body", x, y, sc, sc, ` color="${look.walls}"`) + `<g ${tf(x, y, sc)}><g data-cabin-leaf="" data-open="${CABIN_DOOR_OPEN}">${CABIN_DOOR}</g>${look.head}</g></g>`) }));
// The tree's pieces, in its own units, for treeInner and for the map on a tree room's sign (ticket 244). The limbs out to
// the first n houses' platforms:
const treeLimbs = n => TREE_SLOTS.slice(0, n).map(([x, y]) => {
  const sd = Math.sign(x), bx0 = sd * 40, by = y + 60;
  return `<path d="M${bx0} ${by + 30} Q${f1(x * 0.5)} ${f1(by + 10)} ${f1(x - sd * 40)} ${f1(y + 14)}" fill="none" stroke="#6e4a2c" stroke-width="24" stroke-linecap="round"/>` +
         `<line x1="${sd * 30}" y1="${by + 90}" x2="${f1(x - sd * 10)}" y2="${f1(y + 16)}" stroke="#6e4a2c" stroke-width="9" stroke-linecap="round"/>`;
}).join("");
// the trunk: five trunks braided, apart at the ground, one tree above; two roots out along the ground
const treeRoots = () => `<path d="M-90 -10 Q-140 -4 -175 12 M90 -10 Q140 -4 175 12" stroke="#6e4a2c" stroke-width="16" stroke-linecap="round" fill="none"/>`;
const treeBraid = () => braidSvg(0, -780, 10, 240, 1, { strands: 5, taper: 0.5, base: "#00000026" });
// the rope bridges, the first `bridges` of TREE_BRIDGES between the n houses
const treeBridges = (n, bridges, hz) => TREE_BRIDGES.filter(([a, b]) => a < n && b < n).slice(0, bridges).map(([a, b]) => {
  const { p, m, q } = treeBridgeCurve(a, b, hz);
  let t = `<path d="M${p[0]} ${p[1]} Q${f1(m[0])} ${f1(m[1])} ${q[0]} ${q[1]}" fill="none" stroke="#8b5e34" stroke-width="10" stroke-dasharray="7 3"/>` +
          `<path d="M${p[0]} ${p[1] - 30} Q${f1(m[0])} ${f1(m[1] - 30)} ${q[0]} ${q[1] - 30}" fill="none" stroke="#d8c49a" stroke-width="3"/>`;
  for (let i = 1; i < 10; i++) {
    const u = i / 10, x = (1 - u) * (1 - u) * p[0] + 2 * u * (1 - u) * m[0] + u * u * q[0], y = (1 - u) * (1 - u) * p[1] + 2 * u * (1 - u) * m[1] + u * u * q[1];
    t += `<line x1="${f1(x)}" y1="${f1(y)}" x2="${f1(x)}" y2="${f1(y - 30)}" stroke="#d8c49a" stroke-width="2"/>`;
  }
  return t;
}).join("");
// a bridge's deck, from house a's platform's edge (p) to house b's (q), sagging between them: a quadratic through m (the
// walkways, ticket 245, follow it)
// (a house's size, as a share of the tree's: the higher ones, 3 and up, smaller; ticket 331, the friend on a bridge sized by it too)
const houseScale = (i, hz) => (i >= 2 ? 0.85 : 1) * hz;
const treeBridgeCurve = (a, b, hz) => {
  const [ax, ay] = TREE_SLOTS[a], [bx, by] = TREE_SLOTS[b], sa = Math.sign(bx - ax) || 1;
  // (a smaller house's platform is narrower)
  const edge = j => 78 * houseScale(j, hz), p = [ax + sa * edge(a), ay], q = [bx - sa * edge(b), by], sag = 30 + 0.08 * Math.hypot(q[0] - p[0], q[1] - p[1]);
  return { p, q, m: [(p[0] + q[0]) / 2, (p[1] + q[1]) / 2 + sag] };
};
// house i's ladder (0-based): down to the house below it, if there is one, or to the ground (then .ground)
const treeLadder = (n, i, hz) => {
  const [x, y] = TREE_SLOTS[i], to = LADDER_TO[i], below = to >= 0 && to < n ? TREE_SLOTS[to][1] - 4 : 0;
  const lx = x + Math.sign(x) * 52 * hz, top = y + 14 * hz;
  let t = `<line x1="${lx - 12}" y1="${top}" x2="${lx - 12}" y2="${below}" stroke="#8b5e34" stroke-width="5"/><line x1="${lx + 12}" y1="${top}" x2="${lx + 12}" y2="${below}" stroke="#8b5e34" stroke-width="5"/>`;
  for (let y2 = top + 18; y2 < below - 4; y2 += 22) t += `<line x1="${lx - 12}" y1="${y2}" x2="${lx + 12}" y2="${y2}" stroke="#a0703f" stroke-width="4"/>`;
  return { svg: t, ground: below === 0 };
};
// (ticket 516, Matthew: "New background --- round tree house interior, inspired by hut."; "Can we show a round treehouse room
// with a tree trunk in the center, and lots of limbs reaching into the roof?") The round room at the top of the tree, outside,
// in the tree's units: where the braid ends (-780) and the crown's limbs leave the trunk, the only place a room can wrap round
// the living trunk; the grass hut's shape (hutSvg's dome), the tree rooms' planks. Its floor's top (floor), the wall's
// radius (R) up to the eaves, the dome overhanging to RE, tied round the trunk at collar; the platform under it (plat).
// Inside it is the place "treetop" (treetopScene): the same limbs (crownLimbs j 0..TOP.limbs - 1), the same braid
const TOP = { floor: -660, eaves: -780, collar: -1050, R: 200, RE: 236, plat: 230, collarR: 44, limbs: 8 };
// the house below it, whose ladder goes up to it: house 4 (the lookout) when there are four houses or more, else house 2,
// else house 1 (1-based); its side of the trunk (sd: 1 right, -1 left), where the round room's one hatch is
const topHouse = o => { const n = Math.max(1, Math.min(6, o.houses ?? 4)); return n >= 4 ? 4 : n >= 2 ? 2 : 1; };
const topSide = o => Math.sign(TREE_SLOTS[topHouse(o) - 1][0]) || 1;
// the ladder up to it, from the house below's platform, at its end nearest the trunk, to an opening in the round floor
const topLadderAt = o => { const i = topHouse(o) - 1, [hx, hy] = TREE_SLOTS[i], sd = topSide(o), hz = (o.treeHouseSize ?? 100) / 100;
  return { lx: hx - sd * (78 * houseScale(i, hz) - 14), foot: hy, top: TOP.floor + 16 }; };
function topLadderSvg(o) {
  const { lx, foot, top } = topLadderAt(o);
  let t = `<line x1="${f1(lx - 12)}" y1="${foot}" x2="${f1(lx - 12)}" y2="${top}" stroke="#8b5e34" stroke-width="5"/><line x1="${f1(lx + 12)}" y1="${foot}" x2="${f1(lx + 12)}" y2="${top}" stroke="#8b5e34" stroke-width="5"/>`;
  let rungs = "";
  for (let y = foot - 18; y > top + 4; y -= 22) rungs += `M${f1(lx - 12)} ${y} H${f1(lx + 12)} `;
  return t + `<path d="${rungs.trim()}" stroke="#a0703f" stroke-width="4"/>`;
}
// where the dome is at height y: its half width (the dome hutSvg draws: a quarter ellipse from the eaves up to the collar)
const topRoofHalf = y => { const v = (TOP.eaves - 4 - y) / (TOP.eaves - 4 - TOP.collar); return v < 0 ? TOP.RE : v > 1 ? 0 : TOP.RE * Math.sqrt(1 - v * v); };
const TOP_STRAW = ["#c9c25a", "#a8a23e", "#d8d070"];
// its shell, seen from below: the wall a cylinder of upright planks (its foot and top bowing up toward us, its sides shaded;
// the two round windows the room has inside, at the same angles round the wall and the same 🪟 size, lit warm from within,
// so a spin out through one finds it here), the platform's underside over the wall's foot with beams out
// from the trunk and the ladder's opening, the dome of thatch over it all in rings of fringe, a straw collar round the trunk
function topShellSvg(o) {
  const { floor, eaves, collar, R: Ro, RE, plat, collarR } = TOP, arc = (y, b, r = Ro) => [...Array(25)].map((_, i) => { const a = Math.PI * i / 24; return [-r * Math.cos(a), y - b * Math.sin(a)]; });
  let t = `<polygon points="${ptsOf(arc(floor, 16))} ${ptsOf(arc(eaves, 12).reverse())}" fill="#b47e48" stroke="#5e3d22" stroke-width="2"/>`;
  let seams = "";
  for (let i = 1; i < 20; i++) { const a = Math.PI * i / 20, x = -Ro * Math.cos(a), sh = Math.sin(a); seams += `M${f1(x)} ${f1(floor - 16 * sh)} V${f1(eaves - 12 * sh)} `; }
  t += `<path d="${seams.trim()}" stroke="#7a4f2a" stroke-width="2" opacity="0.8"/>` +
       [-1, 1].map(s => `<polygon points="${f1(s * Ro)},${floor} ${f1(s * Ro * 0.8)},${floor - 10} ${f1(s * Ro * 0.8)},${eaves - 8} ${f1(s * Ro)},${eaves}" fill="#000000" opacity="0.16"/>`).join("");
  // (the windows: inside at th 1.31π and 1.69π; a little lower here, clear of the eaves' fringe; here at a = th - π round the front's arc)
  const ws = (o.windowSize ?? 100) / 100;
  for (const th of [1.31, 1.69]) { const a = (th - 1) * Math.PI, sa = Math.sin(a), wx = -Ro * Math.cos(a), wy = eaves + 0.55 * (floor - eaves) - 14 * sa, ry = 19 * ws, rx = ry * Math.max(0.4, sa) * 0.9;
    t += `<ellipse cx="${f1(wx)}" cy="${f1(wy)}" rx="${f1(rx + 4)}" ry="${f1(ry + 4)}" fill="#5e3d22" stroke="#3a2414" stroke-width="2"/>` +
         `<ellipse cx="${f1(wx)}" cy="${f1(wy)}" rx="${f1(rx)}" ry="${f1(ry)}" fill="#f0c066"/>` +
         `<path d="M${f1(wx - rx)} ${f1(wy)} H${f1(wx + rx)} M${f1(wx)} ${f1(wy - ry)} V${f1(wy + ry)}" stroke="#5e3d22" stroke-width="3"/>`; }
  // (the platform after the wall: seen from below, its near rim hides the wall's foot)
  let beams = "";
  for (let i = 0; i < 12; i++) { const a = Math.PI * (i + 0.5) / 12; beams += `M${f1(Math.cos(a) * 60)} ${f1(floor + 8 + Math.sin(a) * 8)} L${f1(Math.cos(a) * plat * 0.97)} ${f1(floor + 8 + Math.sin(a) * 25)} `; }
  const { lx } = topLadderAt(o);
  t += `<ellipse cx="0" cy="${floor + 8}" rx="${plat}" ry="26" fill="#5e3d22" stroke="#3a2414" stroke-width="2"/><path d="${beams.trim()}" stroke="#3a2414" stroke-width="3"/>` +
       `<path d="${openingD("square", lx, floor + 20, 22, 6)}" fill="#1f140b"/>`;
  // the eaves' underside and the dome of thatch (the grass hut's: hutSvg's curve, its rings of fringe), tied round the trunk
  const eY = eaves - 4, DH = eY - collar, kc = 0.5523;
  t += `<ellipse cx="0" cy="${eY}" rx="${RE}" ry="20" fill="#8a7030"/>` +
       `<path d="M${-RE} ${eY} C${-RE} ${f1(eY - kc * DH)} ${f1(-kc * RE)} ${collar} 0 ${collar} C${f1(kc * RE)} ${collar} ${RE} ${f1(eY - kc * DH)} ${RE} ${eY} Q0 ${eY + 36} ${-RE} ${eY} Z" fill="#c4b85a" stroke="#6e6a22" stroke-width="2"/>`;
  const straw = ["", "", ""];
  for (let jr = 1; jr <= 5; jr++) { const v = jr / 6, r = RE * Math.sqrt(1 - v * v) * 0.98, y = eY - DH * v, b = 16 * (1 - v);
    const P2 = [...Array(21)].map((_, i) => { const a = 0.08 + (Math.PI - 0.08 * 2) * i / 20; return [r * Math.cos(a), y + b * Math.sin(a)]; });
    t += `<polyline points="${ptsOf(P2)}" fill="none" stroke="#9a9434" stroke-width="3"/>`;
    P2.forEach(([x, yy], i) => { const l = 12 + ((i * 5 + jr) % 5) * 2; straw[(i + jr) % 3] += `M${f1(x)} ${f1(yy - 1)} q${(i % 3) - 1} ${f1(l / 2)} ${(i % 3) - 1.5} ${l} `; }); }
  for (let i = 0; i < 48; i++) { const x = -RE + 2 * RE * (i + 0.5) / 48, l = 12 + 9 * ((i * 7) % 5) / 4; straw[i % 3] += `M${f1(x)} ${f1(eY + 18 * Math.sqrt(1 - (x / RE) ** 2))} q${(i % 3) - 1} ${f1(l / 2)} ${(i % 3) - 1.5} ${f1(l)} `; }
  t += straw.map((d, i) => `<path d="${d.trim()}" stroke="${TOP_STRAW[i]}" stroke-width="3" fill="none" stroke-linecap="round"/>`).join("");
  // (the trunk above the dome as thick as it is inside, its strands going on up into the crown's trunk)
  t += `<polygon points="${-collarR + 2},${collar} -33,-1190 33,-1190 ${collarR - 2},${collar}" fill="#5e3d22" stroke="#3a2414" stroke-width="2"/>` +
       `<path d="M-22 ${collar} Q-30 -1120 -16 -1190 M0 ${collar} Q8 -1120 -2 -1190 M22 ${collar} Q14 -1120 18 -1190" fill="none" stroke="#3a2414" stroke-width="3" opacity="0.5"/>`;
  t += `<path d="M${-collarR - 8} ${collar + 10} Q0 ${collar + 26} ${collarR + 8} ${collar + 10} L${collarR + 2} ${collar - 8} Q0 ${collar + 4} ${-collarR - 2} ${collar - 8} Z" fill="#a8a23e" stroke="#6e6a22" stroke-width="2"/>` +
       `<path d="M${-collarR - 4} ${collar + 2} Q0 ${collar + 16} ${collarR + 4} ${collar + 2}" fill="none" stroke="#d8c48a" stroke-width="4"/>`;
  return t;
}
// where each crown limb rooted inside the dome comes out through the thatch: the limb again from the dome's edge
// (de Casteljau's split of its quadratic), over the roof, its fork from where it leaves the roof, and a straw collar
function topLimbsOutSvg(R) {
  let t = "", straw = ["", "", ""];
  for (const L of crownLimbs(R).slice(0, TOP.limbs)) {
    const p0 = [L.sd * 20, L.sy], p1 = [L.tx * 0.45, L.sy - 20], p2 = [L.tx, L.ty], q = u => [0, 1].map(a => (1 - u) * (1 - u) * p0[a] + 2 * u * (1 - u) * p1[a] + u * u * p2[a]);
    let u = 1;
    for (let s = 1; s <= 80; s++) { const [x, y] = q(s / 80); if (Math.abs(x) > topRoofHalf(y) - 4) { u = s / 80; break; } }
    // (the lowest four rest on the wall's top inside, as rafters do: out here they come out from under the eaves, curving up
    // and out to their own tips; the rest out through the thatch where their curve meets it)
    const eave = L.j < 4, u0 = u, a = eave ? [L.sd * (TOP.RE - 12 - 6 * (L.j >> 1)), TOP.eaves - 2 - 22 * (L.j >> 1)] : q(u0),
      m = eave ? [L.sd * (Math.max(Math.abs(L.tx), TOP.RE) + 30), TOP.eaves - 30] : [0, 1].map(i => p1[i] + (p2[i] - p1[i]) * u0);
    // (its bark edged dark, as inside, so the bit between the thatch and the leaves reads as a limb, not a knob)
    const d = `M${f1(a[0])} ${f1(a[1])} Q${f1(m[0])} ${f1(m[1])} ${f1(L.tx)} ${f1(L.ty)}`;
    t += `<path d="${d}" fill="none" stroke="#3a2414" stroke-width="${f1(L.w + 4)}" stroke-linecap="round"/><path d="${d}" fill="none" stroke="#6e4a2c" stroke-width="${f1(L.w)}" stroke-linecap="round"/>` +
         `<path d="${d}" fill="none" stroke="#8a6040" stroke-width="${f1(L.w * 0.25)}" stroke-linecap="round" transform="translate(0 ${f1(-L.w * 0.2)})"/>`;
    // (its fork, only when it leaves the limb outside the roof: one starting in the thatch would show as a stub; then the
    // limb goes on from its tip to the fork's end instead, so a short limb reaches up into the leaves, not ending in a knob)
    const f0 = [L.tx * 0.6, (L.sy + L.ty) / 2 - 10];
    if (Math.abs(f0[0]) <= topRoofHalf(f0[1]) + 2) t += `<path d="M${f1(L.tx)} ${f1(L.ty)} L${f1(L.fx)} ${f1(L.fy)}" fill="none" stroke="#3a2414" stroke-width="15" stroke-linecap="round"/><path d="M${f1(L.tx)} ${f1(L.ty)} L${f1(L.fx)} ${f1(L.fy)}" fill="none" stroke="#6e4a2c" stroke-width="11" stroke-linecap="round"/>`;
    else for (let s = 0; s <= 20; s++) { const v = s / 20, x = f0[0] + (L.fx - f0[0]) * v, y = f0[1] + (L.fy - f0[1]) * v;
      if (Math.abs(x) > topRoofHalf(y) + 2) { if (v < 0.9) t += `<path d="M${f1(x)} ${f1(y)} L${f1(L.fx)} ${f1(L.fy)}" fill="none" stroke="#6e4a2c" stroke-width="11" stroke-linecap="round"/>`; break; } }
    // (a collar of thatch over where it comes out, wider than the limb, so its cut end never shows: a tuft of straw wrapped
    // round it, fringe hanging over it)
    const [x, y] = eave ? [a[0] + L.sd * 4, a[1]] : q(u), [dx, dy] = eave ? [m[0] - a[0], m[1] - a[1]] : [0, 1].map(i => q(Math.min(1, u + 0.02))[i] - q(u)[i]), ang = Math.atan2(dy, dx) * 180 / Math.PI;
    t += `<ellipse cx="${f1(x - L.sd * 3)}" cy="${f1(y)}" rx="${f1(L.w * 0.6)}" ry="${f1(L.w * 1.05)}" transform="rotate(${f1(ang)} ${f1(x - L.sd * 3)} ${f1(y)})" fill="#c4b85a" stroke="#6e6a22" stroke-width="2"/>`;
    for (let i = 0; i < 7; i++) { const e = (i - 3) / 3, px = x - L.sd * 2, py = y - L.w * 0.5 + e * L.w * 0.6, l = 8 + 5 * R(L.j * 7 + i, 5172);
      straw[i % 3] += `M${f1(px)} ${f1(py)} q${f1(-L.sd * l * 0.2)} ${f1(l * 0.5)} ${f1(-L.sd * l * 0.1)} ${f1(l)} `; }
  }
  return t + straw.map((d, i) => `<path d="${d.trim()}" stroke="${TOP_STRAW[i]}" stroke-width="3" fill="none" stroke-linecap="round"/>`).join("");
}
// all of it, as the tree draws it (treeInner's part tree-top): the ladder, the shell a doorway into the room, the limbs out
const treeTopSvg = (o, R) => topLadderSvg(o) + `<g class="portal" data-to="treetop">` + topShellSvg(o) + `</g>` + topLimbsOutSvg(R);
// the n houses, each where it sits, its animal and its look, its size; the higher houses first: a lower one is
// nearer, in front of them
const treeHouses = (o, n, hz) => TREE_SLOTS.slice(0, n).map((slot, i) => [slot, i]).sort((u, v) => u[0][1] - v[0][1]).map(([[x, y, usual], i]) => {
  const chosen = o["house" + (i + 1)], animal = HOUSE_ANIMALS.some(([v]) => v === chosen) ? chosen : usual;
  return { x, y, i, animal, look: houseLook(animal, o, i + 1), sc: houseScale(i, hz) };
});
// (ticket 244) a drawing as a silhouette in one color: its every fill and stroke that color, its eyes gone
const silhouette = (svg, col) => svg.replace(/<use href="#bg-eye"[^>]*\/>/g, "").replace(/ (fill|stroke)="(?!none")[^"]*"/g, ` $1="${col}"`).replace(/ opacity="[^"]*"/g, "");
// (ticket 244, Matthew: "on that sign, let's include a graphic that serves as a map for the person in the room -- it looks
// like the treehouse, but in a few dark colors (not colorful) and maybe even reuses some of the tree house drawing logic.
// The sign indicates, with a bright white circle, which of the 6 possible houses in the map graphic is the current
// house") The tree outside, from the pieces treeInner draws it with, each piece in one dark color: the ground, the trunk
// and its limbs, the bridges and ladders, and the houses, each numbered; a white ring round house `here` (1-based). In the
// tree's units, cut to TREE_MAP_BOX: the trunk goes on up, out of it.
const TREE_MAP_BOX = [-480, -640, 960, 670];
// (ticket 516: every map draws the ladder up to the round room at the top of the tree, going up out of the box; the round
// room's own sign, here "top", has its box up round the crown (TOP_MAP_BOX), the room drawn and ringed)
const TOP_MAP_BOX = [-480, -1150, 960, 670];
function treeMapSvg(o, here, box = TREE_MAP_BOX) {
  const n = Math.max(1, Math.min(6, o.houses ?? 4)), hz = (o.treeHouseSize ?? 100) / 100;
  const bridges = o.bridges ?? 2, ladders = o.ladders ?? 3, [bx, by, bw, bh] = box, top = here === "top";
  let t = `<defs><clipPath id="bg-treemap"><rect x="${bx}" y="${by}" width="${bw}" height="${bh}"/></clipPath></defs><g clip-path="url(#bg-treemap)">` +
          (by + bh > 10 ? `<rect x="${bx}" y="10" width="${bw}" height="${by + bh - 10}" fill="#2c3318"/>` : "") +
          // (the round room's map: what is up there, the crown and the houses' tops; nothing below its box)
          (top ? silhouette(treeBraid() + `<polygon points="-62,-720 -26,-1290 26,-1290 62,-720"/>` + crownLimbs(crownR).map(crownLimbSvg).join(""), "#3b2715") + silhouette(topLadderSvg(o), "#4d3520")
               : silhouette(treeLimbs(n) + treeRoots() + treeBraid(), "#3b2715") +
                 silhouette(treeBridges(n, bridges, hz) + TREE_SLOTS.slice(0, Math.min(n, ladders)).map((_, i) => treeLadder(n, i, hz).svg).join("") + topLadderSvg(o), "#4d3520"));
  const houses = treeHouses(o, n, hz).filter(({ y, sc }) => !top || y - 110 * sc < by + bh);
  // (the numbers after all the houses: a lower house, nearer, covers the one above it, and would cover its number)
  t += houses.map(({ x, y, look, sc }) => silhouette(`<g ${tf(x, y, sc)}>${CABIN_SVG}${look.head}</g>`, "#1f140b")).join("") +
       (top ? silhouette(topShellSvg(o) + topLimbsOutSvg(crownR), "#1f140b") : "") +
       houses.map(({ x, y, i, sc }) => `<text x="${f1(x)}" y="${f1(y - 14 * sc)}" text-anchor="middle" font-size="${f1(92 * sc)}" font-weight="bold" fill="#e9d3a6" font-family="sans-serif">${i + 1}</text>`).join("");
  if (top) return t + [["#1f140b", 30], ["#ffffff", 16]].map(([col, w]) => `<ellipse data-here="top" cx="0" cy="-860" rx="270" ry="250" fill="none" stroke="${col}" stroke-width="${w}"/>`).join("") + `</g>`;
  const h = houses.find(u => u.i === here - 1);
  if (h) t += [["#1f140b", 30], ["#ffffff", 16]].map(([col, w]) => `<circle data-here="${here}" cx="${f1(h.x)}" cy="${f1(h.y - 84 * h.sc)}" r="${f1(112 * h.sc)}" fill="none" stroke="${col}" stroke-width="${w}"/>`).join("");
  return t + `</g>`;
}
// (ticket 257, Matthew: "the palm trees in the castle's green house need more segments for collision detection -- and we
// should improve the image -- it looks pretty sloppy -- let's have a palm tree segment system, with 3 or 4 variations of
// shapes, and stack them, not use ovals") A palm's trunk: n segments stacked along its curve from its foot (x, base) up
// h, leaning toward sd (lean of h across at the top, bending more as it goes up), each of one of PALM_SEGS's shapes,
// turned to the trunk's way there, narrowing as it goes up; drawn from the top down, so each segment's lip is over the
// foot of the one above it; a root flare at the ground. Its spine (data-spine: straight pieces along the curve, each
// as wide as the trunk there, unseen) is what its collision bands are measured by (standingHere), PALM_BANDS of them.
// Returns its drawing and the top of the trunk, where its crown goes. rnd(j): a seeded number in [0, 1) for segment j.
const PALM_BANDS = 8;
// (each a segment's outline: a its foot's half-width, b its top's, s its height; its foot at 0, its top at -s)
const PALM_SEGS = [
  (a, b, s) => `M${-a} 0 L${-0.92 * b} ${-0.8 * s} L${-1.14 * b} ${-s} L${1.14 * b} ${-s} L${0.92 * b} ${-0.8 * s} L${a} 0 Z`,   // a collar, its lip flared
  (a, b, s) => `M${-a} 0 Q${-1.16 * a} ${-0.5 * s} ${-b} ${-s} L${b} ${-s} Q${1.16 * a} ${-0.5 * s} ${a} 0 Z`,   // a barrel
  (a, b, s) => `M${-a} 0 L${-b} ${-s} L${-0.3 * b} ${-s} L0 ${-0.82 * s} L${0.3 * b} ${-s} L${b} ${-s} L${a} 0 Z`,   // notched, a leaf's scar
  (a, b, s) => `M${-a} 0 L${-1.06 * b} ${-0.9 * s} Q0 ${-1.14 * s} ${1.06 * b} ${-0.9 * s} L${a} 0 Z`,   // domed
].map(f => (a, b, s) => f(a, b, s).replace(/-?\d+(\.\d+)?(e-?\d+)?/g, v => f1(+v)));
function palmTrunkSvg(x, base, h, sd, k, rnd, lean = 0.18, n = 11) {
  const at = u => [x + sd * h * lean * u * u, base - h * u], turn = u => Math.atan2(sd * 2 * lean * u, 1) * 180 / Math.PI;
  const half = u => (17 - 7 * u) * k, s = h / n * 1.2;
  const flare = half(0.06);
  let t = `<path d="M${f1(x - 28 * k)} ${f1(base + 2 * k)} Q${f1(x - 15 * k)} ${f1(base - 6 * k)} ${f1(x - flare)} ${f1(base - 24 * k)} L${f1(x + flare)} ${f1(base - 24 * k)} Q${f1(x + 15 * k)} ${f1(base - 6 * k)} ${f1(x + 28 * k)} ${f1(base + 2 * k)} Z" fill="#6a4a28" stroke="#4e3519" stroke-width="${f1(1.4 * k)}"/>`;
  for (let j = n - 1; j >= 0; j--) {
    const u = j / n, [px, py] = at(u), a = half(u), b = half(Math.min(1, (j + 1.2) / n)), shape = PALM_SEGS[Math.floor(rnd(j) * PALM_SEGS.length) % PALM_SEGS.length];
    const fill = ["#8b6a3e", "#7d5d34", "#94744a"][j % 3];
    t += `<g transform="translate(${f1(px)} ${f1(py)}) rotate(${f1(turn(u + 0.5 / n))})">` +
         `<path d="${shape(a, b, s)}" fill="${fill}" stroke="#4e3519" stroke-width="${f1(1.4 * k)}" stroke-linejoin="round"/>` +
         // (its round side catching the light, and the ring where its leaf grew)
         `<path d="M${f1(-0.55 * a)} ${f1(-0.12 * s)} L${f1(-0.55 * b)} ${f1(-0.74 * s)}" stroke="#b9925e" stroke-width="${f1(2.2 * k)}" stroke-linecap="round" opacity="0.6"/>` +
         `<path d="M${f1(-0.9 * b)} ${f1(-0.78 * s)} Q0 ${f1(-0.68 * s)} ${f1(0.9 * b)} ${f1(-0.78 * s)}" fill="none" stroke="#5a3d1f" stroke-width="${f1(1.1 * k)}" opacity="0.7"/></g>`;
  }
  // (the spine: four straight pieces along the curve, unseen, each as wide as the trunk at its middle)
  t += `<g data-spine-of="palm" pointer-events="none">` + [0, 1, 2, 3].map(i => { const [ax, ay] = at(i / 4), [bx, by] = at((i + 1) / 4);
    return `<path data-spine="" data-bands="${PALM_BANDS}" d="M${f1(ax)} ${f1(ay)} L${f1(bx)} ${f1(by)}" fill="none" stroke="transparent" stroke-width="${f1(2 * half((i + 0.5) / 4))}"/>`; }).join("") + `</g>`;
  return { svg: drawnBy("palmTrunkSvg", t), top: at(1) };
}
// (ticket 50) the swing: its seat, 74 of the tree's units across (358 to
// 432), its top 72 above the ground, as treeInner's "tree-swing" hangs it
// at rest, in the tree's group (its size the tree's slider's); a sitter is
// in front of the whole tree, and swings with the seat (ticket 42). A
// big tree carries it out past the picture's right edge: only the part of
// it inside is a seat, and none once less than half of it is left.
SEATS.treehouse = (c, B) => { const ts = (B.treeSize ?? 100) / 100 * c.k, x0 = 358 * ts, x1 = Math.min(432 * ts, c.x1 - 10);
  return x1 - x0 < 37 * ts ? [] : [{ name: "swing", x0, x1, y: c.g - 72 * ts, z: c.g + 1, facing: ["front"] }]; };

// (ticket 318, Matthew: "the moss looks like slime .. we need more like 'pads', with 'fingers' that reach up, with a
// lighter, more bright color. And various styles"; "the pads should be polygons that we draw, not ovals"; "the fingers
// have wavy structure, like the witches' cauldron simple version"; "the top of each wavy strand could have a lobe,
// optionally, see pictures of real moss for guidance") Moss, after five real kinds: low pads, each a drawn polygon (a
// darker base and a lighter top on it), and fingers rising from the pads' tops, each a wavy strand drawn as the
// cauldron's simple fumes are (a smooth curve through points that sway side to side as they rise), some with a lobe at
// the tip, as real mosses carry their spore capsules on stalks. Its feet at (0, 0), its own size about w across; the
// same shape every time for a seed (seeded). Each kind's numbers:
//   w, h: the clump's width and its pads' height;   pads: how many pads, overlapping;
//   fingers, fh: how many fingers and how tall;      wave, waves: how far a finger sways, and how many times;
//   lean: how far the fingers all lean (a share of their height, to the right);   fw: a finger's thickness;
//   lobeShare, lobe: which share of the fingers carry a lobe, and its size;   side: little leaflets up each finger;
//   color, tip, lobeColor: the pads' and the fingers' green, the light tips, and the lobes.
// (Matthew: "let's have moss that can adapt to being on: floor/ground, L wall, back wall, R wall") o.on: "floor" (the
// fingers up), "left" (on a left wall: its pads against the wall, the fingers out into the room, to the right), "right"
// (the mirror), "back" (stuck on the back wall: each pad a whole ragged patch, the fingers shorter); o.share: the share of
// its kind's fingers drawn (a scene's many clumps draw fewer, to stay cheap).
const MOSS_STYLES = {
  // pincushion moss (Leucobryum): round, pale, whitish green domes, short and close
  cushion: { label: "🟢 cushion", w: 120, h: 36, pads: 3, fingers: 26, fh: 13, wave: 1.4, waves: 1, lean: 0, fw: 2.4, lobeShare: 0.2, lobe: 2.4, color: "#a6d873", tip: "#eaf8c4", lobeColor: "#f3e7a0" },
  // haircap moss (Polytrichum): tall spiky stems, star-tipped, capsules on long stalks
  haircap: { label: "⭐ haircap", w: 110, h: 22, pads: 2, fingers: 16, fh: 48, wave: 3, waves: 1.5, lean: 0, fw: 2.6, lobeShare: 0.6, lobe: 3.4, color: "#6fc74a", tip: "#ccf28e", lobeColor: "#e0a94a" },
  // sheet moss (Hypnum): a low flat mat creeping along, golden green
  carpet: { label: "🟫 carpet", w: 170, h: 14, pads: 4, fingers: 30, fh: 8, wave: 1.2, waves: 1, lean: 0.1, fw: 2, lobeShare: 0.12, lobe: 2, color: "#b0cc4e", tip: "#eef4a8", lobeColor: "#e8c660" },
  // fern moss (Thuidium): feathery fronds, a leaflet either side up each
  feather: { label: "🪶 feather", w: 130, h: 18, pads: 3, fingers: 12, fh: 34, wave: 4, waves: 1.2, lean: 0.15, fw: 2.2, side: true, lobeShare: 0.2, lobe: 2.6, color: "#8ad24f", tip: "#d8f69e", lobeColor: "#efcf6a" },
  // rock cap moss (Dicranum): windswept tufts, every finger leaning the same way
  windswept: { label: "🌬️ windswept", w: 120, h: 24, pads: 3, fingers: 18, fh: 30, wave: 2.4, waves: 1.5, lean: 0.55, fw: 2.3, lobeShare: 0.45, lobe: 2.8, color: "#82d24c", tip: "#dcf8a4", lobeColor: "#f2c25a" },
};
const MOSS_STYLE_CHOICES = Object.entries(MOSS_STYLES).map(([v, S]) => [v, S.label]);
// (the colours a placed moss can take; "none" is its kind's own)
const MOSS_COLORS = ["none", "#a6d873", "#6fc74a", "#b0cc4e", "#8ad24f", "#82d24c", "#c8e86a", "#5fc28a"];
// a smooth path through points (as helixCheapSvg's strands: each piece a cubic from its neighbours)
const smoothPath = pts => {
  let d = `M${f1(pts[0][0])} ${f1(pts[0][1])}`;
  for (let i = 0; i < pts.length - 1; i++) {
    const p0 = pts[Math.max(0, i - 1)], p1 = pts[i], p2 = pts[i + 1], p3 = pts[Math.min(pts.length - 1, i + 2)];
    d += ` C${f1(p1[0] + (p2[0] - p0[0]) / 6)} ${f1(p1[1] + (p2[1] - p0[1]) / 6)} ${f1(p2[0] - (p3[0] - p1[0]) / 6)} ${f1(p2[1] - (p3[1] - p1[1]) / 6)} ${f1(p2[0])} ${f1(p2[1])}`;
  }
  return d;
};
const MOSS_ON = [["floor", "🟫 the floor"], ["back", "🧱 the back wall"], ["left", "⬅️ a left wall"], ["right", "➡️ a right wall"]];
function mossSvg(style, seed, o = {}) {
  const S = MOSS_STYLES[style] || MOSS_STYLES.cushion, r = seeded(seed), col = o.color && o.color !== "none" ? o.color : S.color, lobes = o.lobes ?? true;
  const on = o.on || "floor", back = on === "back", nf = Math.max(3, Math.round(S.fingers * (o.share ?? 1))), fh = S.fh * (back ? 0.6 : 1);
  const base = mix(col, "#000000", 0.22), stem = mix(col, S.tip, 0.25), P = (x, y) => `${f1(x)},${f1(y)}`;
  // the pads: a dome each, its rim ragged, its bottom flat on the ground; a lighter dome on top of it, a little in from it
  const pads = [...Array(S.pads)].map((_, i) => {
    const cx = (i - (S.pads - 1) / 2) * S.w / S.pads * 0.86 + (r(i) - 0.5) * 8, pw = S.w / S.pads * (1.15 + 0.25 * r(i + 20)), ph = S.h * (0.75 + 0.35 * r(i + 40));
    const edge = [...Array(13)].map((_, j) => { const a = Math.PI - Math.PI * j / 12, n = 0.86 + 0.24 * r(i * 31 + j);
      return [cx + Math.cos(a) * pw / 2 * n, -Math.sin(a) * ph * n]; });
    return { cx, pw, ph, edge };
  });
  // (on the back wall, each pad's lower half too, shallower, so it is a patch stuck to the wall, not a dome on the ground)
  let t = (back ? pads.map(p => `<polygon points="${p.edge.map(([x, y]) => P(x, -y * 0.55)).join(" ")}" fill="${mix(base, "#000000", 0.12)}"/>`).join("") : "") +
    pads.map(p => `<polygon points="${P(p.cx - p.pw / 2, 0)} ${p.edge.map(([x, y]) => P(x, y)).join(" ")} ${P(p.cx + p.pw / 2, 0)}" fill="${base}"/>`).join("") +
    pads.map(p => `<polygon points="${p.edge.slice(2, 11).map(([x, y]) => P(p.cx + (x - p.cx) * 0.78 - 2, y * 0.84 - 1)).join(" ")}" fill="${col}"/>`).join("");
  // the fingers: each from a point on its pad's top, rising, swaying, leaning; leaflets up it (feather); a lobe at its tip
  let fingers = "", tips = "";
  for (let j = 0; j < nf; j++) {
    const p = pads[j % S.pads], a = Math.PI * (0.15 + 0.7 * r(j + 100)), bx = p.cx + Math.cos(a) * p.pw / 2 * 0.8, by = -Math.sin(a) * p.ph * 0.85;
    const hh = fh * (0.6 + 0.6 * r(j + 200)), ph = 2 * Math.PI * r(j + 300), lean = (S.lean + (r(j + 400) - 0.5) * 0.25) * hh;
    const pts = [...Array(7)].map((_, q) => { const u = q / 6; return [bx + lean * u * u + S.wave * Math.sin(u * S.waves * 2 * Math.PI + ph) * u, by - hh * u]; });
    fingers += `<path d="${smoothPath(pts)}" fill="none" stroke="${stem}" stroke-width="${f1(S.fw)}" stroke-linecap="round"/>`;
    // (its last third lighter, as a moss's tips are new growth)
    tips += `<path d="${smoothPath(pts.slice(4))}" fill="none" stroke="${S.tip}" stroke-width="${f1(S.fw * 0.8)}" stroke-linecap="round"/>`;
    if (S.side) for (const [q, sd] of [[2, -1], [3, 1], [4, -1], [5, 1]]) { const [x, y] = pts[q], l = S.fh * 0.16 * (1 - q / 8);
      fingers += `<path d="M${P(x, y)} q${f1(sd * l * 0.6)} ${f1(-l * 0.2)} ${f1(sd * l)} ${f1(-l * 0.7)}" fill="none" stroke="${stem}" stroke-width="${f1(S.fw * 0.6)}" stroke-linecap="round"/>`; }
    if (lobes && r(j + 500) < S.lobeShare) { const [x, y] = pts[6], l = S.lobe * (0.8 + 0.4 * r(j + 600)), dx = pts[6][0] - pts[5][0], dy = pts[6][1] - pts[5][1], an = Math.atan2(dx, -dy) * 180 / Math.PI;
      // (a capsule: a little drawn polygon, longer than wide, along the finger's last direction)
      tips += `<polygon points="${[[0, -l * 1.7], [l * 0.75, -l * 0.9], [l * 0.7, 0], [0, l * 0.45], [-l * 0.7, 0], [-l * 0.75, -l * 0.9]].map(([u, v]) => P(u, v)).join(" ")}" transform="translate(${f1(x)} ${f1(y)}) rotate(${f1(an)})" fill="${S.lobeColor}" stroke="${mix(S.lobeColor, "#000000", 0.3)}" stroke-width="0.6"/>`; }
  }
  const all = t + fingers + tips;
  return on === "left" ? `<g transform="rotate(90)">${all}</g>` : on === "right" ? `<g transform="rotate(-90)">${all}</g>` : all;
}

// (ticket 319, M, through Matthew: "lichen options, for decorating rocks, or castle walls, or logs, etc.") Lichen, after
// the three forms real lichens take (crustose, flat on what they grow on; foliose, leafy; fruticose, bushy or hanging),
// five kinds, all drawn shapes (polygons and strands, as ticket 318's moss), the same every time for a seed. Each kind:
//   form: "flat" (lies on the surface), "up" (stands up from it), "hang" (hangs down from it);
//   w, h: its size;  n: how many pieces (tiles, lobes, stalks, strands);  color, tip: its colour and its tips'.
// o.on as the moss's (MOSS_ON): a flat one is drawn face on on the back wall, foreshortened on the floor and on a side
// wall; one standing up is turned on a side wall as the moss is, and shorter on the back wall; one hanging always hangs.
const LICHEN_STYLES = {
  // crustose (Caloplaca, Xanthoria): a cracked crust of little tiles, orange, with darker dots of fruit on it
  crust: { label: "🟠 crust", form: "flat", w: 90, h: 52, n: 26, color: "#f08a2a", tip: "#c4561c" },
  // foliose (Parmelia): a rosette of lobed leaves from its middle, grey green, paler at the edges
  leafy: { label: "🍃 leafy rosette", form: "flat", w: 84, h: 76, n: 11, color: "#a8c49c", tip: "#dcead2" },
  // Cladonia's pixie cups: little trumpets on stalks, on a crust of scales
  cups: { label: "🏆 pixie cups", form: "up", w: 64, h: 40, n: 6, color: "#b6cc96", tip: "#8fa874" },
  // British soldiers (Cladonia cristatella): grey green stalks with bright red caps
  soldiers: { label: "💂 soldiers", form: "up", w: 60, h: 42, n: 7, color: "#aec48e", tip: "#e2342b" },
  // Usnea, old man's beard: pale green strands hanging, branching, from where it grows
  beard: { label: "🧔 beard", form: "hang", w: 54, h: 96, n: 11, color: "#c9dca0", tip: "#eef6d4" },
};
const LICHEN_STYLE_CHOICES = Object.entries(LICHEN_STYLES).map(([v, S]) => [v, S.label]);
const LICHEN_COLORS = ["none", "#f08a2a", "#f2c94c", "#a8c49c", "#c9dca0", "#ece6cf", "#7fa8a0", "#e2342b"];
// (an irregular polygon round (cx, cy), half widths rx and ry, its rim jagged by jag, from r's numbers from i0)
const blobPts = (cx, cy, rx, ry, n, r, i0, jag = 0.25) => [...Array(n)].map((_, j) => { const a = 2 * Math.PI * j / n, m = 1 - jag / 2 + jag * r(i0 + j);
  return `${f1(cx + Math.cos(a) * rx * m)},${f1(cy + Math.sin(a) * ry * m)}`; }).join(" ");
function lichenSvg(style, seed, o = {}) {
  const S = LICHEN_STYLES[style] || LICHEN_STYLES.crust, r = seeded(seed), col = o.color && o.color !== "none" ? o.color : S.color, on = o.on || (S.form === "flat" ? "back" : "floor");
  const dark = mix(col, "#000000", 0.32), pale = mix(col, "#ffffff", 0.3), P = (x, y) => `${f1(x)},${f1(y)}`;
  let t = "";
  if (style === "crust") {
    // (its patch, the cracks' colour; on it, tiles of crust, each a little polygon a shade of its own; dots of fruit)
    t += `<polygon points="${blobPts(0, -S.h / 2, S.w / 2, S.h / 2, 18, r, 0, 0.35)}" fill="${dark}"/>`;
    for (let i = 0; i < S.n; i++) { const a = 2 * Math.PI * r(i + 50), d = Math.sqrt(r(i + 80)) * 0.8, x = Math.cos(a) * S.w / 2 * d, y = -S.h / 2 + Math.sin(a) * S.h / 2 * d, s = 5 + 5 * r(i + 110);
      t += `<polygon points="${blobPts(x, y, s, s * 0.8, 6, r, i * 7 + 140, 0.4)}" fill="${mix(col, "#ffffff", 0.25 * r(i + 200))}"/>`; }
    for (let i = 0; i < 7; i++) { const a = 2 * Math.PI * r(i + 300), d = Math.sqrt(r(i + 320)) * 0.6, x = Math.cos(a) * S.w / 2 * d, y = -S.h / 2 + Math.sin(a) * S.h / 2 * d;
      t += `<polygon points="${blobPts(x, y, 3, 2.6, 7, r, i * 7 + 340, 0.2)}" fill="${S.tip}"/>`; }
  } else if (style === "leafy") {
    // (lobes round its middle, each a leaf widening out to a scalloped tip, paler at its rim; its middle darker)
    for (let i = 0; i < S.n; i++) { const a = 2 * Math.PI * (i + 0.3 * r(i)) / S.n, L = S.w / 2 * (0.65 + 0.35 * r(i + 30)), wd = L * 0.42, c = Math.cos(a), s = Math.sin(a);
      const pt = (u, v) => P(c * u - s * v, -S.h / 2 + (s * u + c * v) * S.h / S.w);
      t += `<polygon points="${[pt(0, -wd * 0.25), pt(L * 0.55, -wd * 0.55), pt(L * 0.85, -wd * 0.62), pt(L, -wd * 0.3), pt(L * 0.94, 0), pt(L, wd * 0.3), pt(L * 0.85, wd * 0.62), pt(L * 0.55, wd * 0.55), pt(0, wd * 0.25)].join(" ")}" fill="${col}" stroke="${S.tip}" stroke-width="1.6" stroke-linejoin="round"/>`; }
    t += `<polygon points="${blobPts(0, -S.h / 2, S.w * 0.14, S.h * 0.14, 9, r, 400, 0.3)}" fill="${dark}"/>`;
  } else if (style === "cups" || style === "soldiers") {
    // (a crust of little scales at its foot; stalks rising from it, a little wavy; on each a cup (a trumpet, wider at its
    // mouth, its inside darker) or a soldier's red cap (a ragged blob, some stalks forked in two))
    const fh = S.h * (on === "back" ? 0.65 : 1);
    for (let i = 0; i < 6; i++) t += `<polygon points="${blobPts((i - 2.5) * S.w / 7, -3, 7, 4, 6, r, i * 6, 0.4)}" fill="${mix(col, "#000000", 0.12 * r(i + 20))}"/>`;
    for (let i = 0; i < S.n; i++) { const bx = (i - (S.n - 1) / 2) * S.w / S.n * 0.9 + (r(i + 40) - 0.5) * 6, hh = fh * (0.55 + 0.45 * r(i + 60)), sw = r(i + 80) - 0.5;
      const pts = [...Array(5)].map((_, q) => { const u = q / 4; return [bx + sw * 6 * Math.sin(u * Math.PI), -3 - hh * u]; }), [tx, ty] = pts[4];
      t += `<path d="${smoothPath(pts)}" fill="none" stroke="${mix(col, "#000000", 0.08)}" stroke-width="${style === "soldiers" ? 3.2 : 2.6}" stroke-linecap="round"/>`;
      if (style === "cups") { const cw = 7 + 4 * r(i + 100), ch = 6 + 3 * r(i + 120);
        t += `<polygon points="${P(tx - 1.6, ty + 1)} ${P(tx + 1.6, ty + 1)} ${P(tx + cw / 2, ty - ch)} ${P(tx - cw / 2, ty - ch)}" fill="${col}" stroke="${dark}" stroke-width="0.8"/>` +
             `<polygon points="${blobPts(tx, ty - ch, cw / 2 - 0.6, 1.8, 8, r, i * 8 + 140, 0.1)}" fill="${S.tip}"/>`; }
      else { const fork = r(i + 160) < 0.4;
        for (const dx of fork ? [-3, 3] : [0]) { if (fork) t += `<path d="M${P(tx, ty)} L${P(tx + dx, ty - 5)}" stroke="${col}" stroke-width="2.4" stroke-linecap="round"/>`;
          t += `<polygon points="${blobPts(tx + dx, ty - (fork ? 7 : 3), 3.6, 3.2, 8, r, i * 9 + dx + 200, 0.35)}" fill="${S.tip}" stroke="${mix(S.tip, "#000000", 0.3)}" stroke-width="0.6"/>`; } }
    }
  } else {
    // (old man's beard: strands from where it hangs, down, swaying a little, branching, paler at their ends; hung from a
    // point above its feet, so a beard put on a log or a rock's edge hangs from it)
    const top = -S.h - 8;
    for (let i = 0; i < S.n; i++) { const bx = (i - (S.n - 1) / 2) * S.w / S.n * 0.7, L = S.h * (0.55 + 0.45 * r(i + 20)), ph = 2 * Math.PI * r(i + 40);
      const pts = [...Array(7)].map((_, q) => { const u = q / 6; return [bx * (1 + u * 0.5) + 3 * Math.sin(u * 3 * Math.PI + ph) * u, top + L * u]; });
      t += `<path d="${smoothPath(pts)}" fill="none" stroke="${col}" stroke-width="1.6" stroke-linecap="round"/>` +
           `<path d="${smoothPath(pts.slice(4))}" fill="none" stroke="${S.tip}" stroke-width="1.2" stroke-linecap="round"/>`;
      if (r(i + 60) < 0.6) { const [x, y] = pts[3], sd = r(i + 80) < 0.5 ? -1 : 1;
        t += `<path d="M${P(x, y)} q${f1(sd * 4)} ${f1(6)} ${f1(sd * 3)} ${f1(L * 0.3)}" fill="none" stroke="${pale}" stroke-width="1.1" stroke-linecap="round"/>`; } }
    t += `<polygon points="${blobPts(0, top, S.w * 0.22, 4, 8, r, 500, 0.3)}" fill="${dark}"/>`;
  }
  // (where it grows)
  if (S.form === "flat") return on === "floor" ? `<g transform="scale(1 0.42)">${t}</g>` : on === "left" || on === "right" ? `<g transform="scale(0.5 1)">${t}</g>` : t;
  if (S.form === "up") return on === "left" ? `<g transform="rotate(90)">${t}</g>` : on === "right" ? `<g transform="rotate(-90)">${t}</g>` : t;
  return t;
}
