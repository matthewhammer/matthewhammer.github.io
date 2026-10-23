"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 13 of 26 of the game's script, run in order by index.html.

// the room's frame: the back wall from x = -bw to bw, from its top to the
// floor; the vanishing point, at the eye's height, where the floor's and
// ceiling's lines meet
function roomFrame(c) {
  const { x0, y0, x1, y1, g, W, H, k } = c;
  // (the eye's height, a share of the wall: 0.55 in most rooms; the hallway's
  // is higher, so it is seen from a little above and its far end clears the friend's head)
  const floor = g - 110 * k, top = y0 + H * 0.1, bw = W * 0.34, vy = top + (floor - top) * (c.eye ?? 0.55);
  // a point (x, y) on the back wall's plane, carried out toward us to the
  // line y = to
  const out = (x, y, to) => { const t = (to - y) / (y - vy); return [x + x * t, to]; };
  return { floor, top, bw, vy, out };
}
// A room's space, for drawing in it in perspective: roomFrame's, and a
// point (X, Y) on the back wall's plane brought z toward us (z < 0; a side
// wall is X = ±bw), as drawn: at(X, Y, z); a height on the back wall, as a
// share of the wall from the floor: Yh(h). The cat room, the stable and the
// side doors place everything with it.
// onWall(sd, Y, z): a transform that lays a small drawing flat on a side wall
// (sd: -1 the left, 1 the right) at height Y and depth z: its own x runs along
// the wall toward us, its y down the wall, in units of k, foreshortened as the
// wall is (a unit toward us is z's 1/800 of the way: the side doors' scale)
function roomSpace(c) {
  const { floor, top, bw, vy, out } = roomFrame(c), wh = floor - top;
  const at = (X, Y, z = 0) => { const f = 1 / (1 + z); return [X * f, vy + (Y - vy) * f]; };
  const onWall = (sd, Y, z) => { const f = 1 / (1 + z), [x0, y0] = at(sd * bw, Y, z), q = f * f / 800;
    return `matrix(${(sd * bw * q).toFixed(4)} ${((Y - vy) * q).toFixed(4)} 0 ${(f * c.k).toFixed(4)} ${f1(x0)} ${f1(y0)})`; };
  return { floor, top, bw, vy, out, wh, at, Yh: h => floor - h * wh, onWall };
}
// a torch on the wall: a bracket, a cup, a flame that flickers, a warm glow
// (o, the scene's options: the flame then flickers with the scene's quick LFO
// and its glow breathes with the slow one, each torch its own way in)
const torchSvg = (x, y, k, i = 0, o = null) => `<circle cx="${f1(x)}" cy="${f1(y - 24 * k)}" r="${f1(70 * k)}" fill="url(#bg-glow)"${o ? `>${sceneLfo(o, "slow", (i * 0.29) % 1, "opacity", (v, amp) => (1 - (0.2 + 0.5 * amp) * (1 + v) / 2).toFixed(2), "")}</circle>` : "/>"}` +
  `<path d="M${f1(x - 4 * k)} ${f1(y + 22 * k)} L${f1(x - 2 * k)} ${f1(y)} L${f1(x + 2 * k)} ${f1(y)} L${f1(x + 4 * k)} ${f1(y + 22 * k)} Z" fill="#3a3a44"/>` +
  `<rect x="${f1(x - 12 * k)}" y="${f1(y + 18 * k)}" width="${f1(24 * k)}" height="${f1(6 * k)}" fill="#2a2a32"/>` +
  `<path d="M${f1(x - 9 * k)} ${f1(y)} L${f1(x + 9 * k)} ${f1(y)} L${f1(x + 6 * k)} ${f1(y + 8 * k)} L${f1(x - 6 * k)} ${f1(y + 8 * k)} Z" fill="#5a4632"/>` +
  // (the flame flickers by scaling about its own base, (x, y): drawn about
  // the origin and moved there, or it drifts off the torch)
  `<g transform="translate(${f1(x)} ${f1(y)})"><path d="M0 ${f1(-34 * k)} Q${f1(12 * k)} ${f1(-14 * k)} ${f1(7 * k)} ${f1(-2 * k)} Q0 ${f1(2 * k)} ${f1(-7 * k)} ${f1(-2 * k)} Q${f1(-12 * k)} ${f1(-14 * k)} 0 ${f1(-34 * k)} Z" fill="#ff9a2e">` +
  `${(own => o ? sceneLfo(o, "fast", (i * 0.41) % 1, "transform", (v, amp) => `${(1 + 0.04 * v).toFixed(3)} ${(1 - (0.05 + 0.2 * amp) * v).toFixed(3)}`, own, "scale") : own)(`<animateTransform attributeName="transform" type="scale" additive="sum" values="1 1;1.06 0.9;0.96 1.08;1 1" dur="${(0.7 + 0.13 * (i % 3)).toFixed(2)}s" repeatCount="indefinite"/>`)}</path></g>` +
  `<path d="M${f1(x)} ${f1(y - 22 * k)} Q${f1(x + 6 * k)} ${f1(y - 10 * k)} ${f1(x + 3 * k)} ${f1(y - 2 * k)} Q${f1(x)} ${f1(y)} ${f1(x - 3 * k)} ${f1(y - 2 * k)} Q${f1(x - 6 * k)} ${f1(y - 10 * k)} ${f1(x)} ${f1(y - 22 * k)} Z" fill="#ffe066"/>`;
// a cobweb in a corner (sd: which side), its threads and rings
const cobweb = (x, y, r, sd, k) => {
  let t = "";
  // (its threads fan across the same quarter as its rings: out along the wall's top, down the corner, and between)
  for (let a = 0; a <= 4; a++) { const an = a * Math.PI / 8; t += `<line x1="${f1(x)}" y1="${f1(y)}" x2="${f1(x + sd * Math.cos(an) * r)}" y2="${f1(y + Math.sin(an) * r)}" stroke="#e8e8f0" stroke-width="${f1(1 * k)}" opacity="0.55"/>`; }
  for (const f of [0.35, 0.6, 0.85]) t += `<path d="M${f1(x + sd * r * f)} ${f1(y)} Q${f1(x + sd * r * f * 0.6)} ${f1(y + r * f * 0.6)} ${f1(x)} ${f1(y + r * f)}" fill="none" stroke="#e8e8f0" stroke-width="${f1(1 * k)}" opacity="0.45"/>`;
  return t;
};
// A perspective floor's lines across it, each from side wall to side wall,
// closer together toward the back as paving is (each a step dz further back
// than the last), down to past the picture's foot
function floorRows(c, stroke, dz = 0.07) {
  const { k, y1, pad } = c, { floor, bw, vy } = roomFrame(c);
  let t = "";
  for (let j = 1; ; j++) {
    const f = 1 / (1 - dz * j), y = vy + (floor - vy) * f;
    if (!(f > 0) || y > y1 + pad) break;
    t += `<line x1="${f1(-bw * f)}" y1="${f1(y)}" x2="${f1(bw * f)}" y2="${f1(y)}" stroke="${stroke}" stroke-width="${f1(2 * k)}"/>`;
  }
  return t;
}
// The stone room: back wall, side walls, vaulted ceiling, flagstone floor,
// in its style's stone; mossy or wet where its style says
function roomShell(c, tone, opts = {}) {
  const { x0, y0, x1, y1, W, H, k, pad, R, P } = c, { floor, top, bw, vy, out } = roomFrame(c);
  const mortar = mix(tone, "#000000", 0.35), side = mix(tone, "#000000", 0.22), ceil = mix(tone, "#000000", 0.45);
  let t = `<defs><radialGradient id="bg-glow"><stop offset="0" stop-color="#ffcf6b" stop-opacity="0.55"/><stop offset="1" stop-color="#ffcf6b" stop-opacity="0"/></radialGradient>` +
          `<linearGradient id="bg-floorfade" x1="0" y1="${f1(floor)}" x2="0" y2="${f1(y1)}" gradientUnits="userSpaceOnUse"><stop offset="0" stop-color="#000000" stop-opacity="0.35"/><stop offset="1" stop-color="#000000" stop-opacity="0"/></linearGradient></defs>`;
  t += `<rect x="${f1(x0 - pad)}" y="${f1(y0 - pad)}" width="${f1(W + 2 * pad)}" height="${f1(H + 2 * pad)}" fill="${mortar}"/>`;
  // the back wall: courses of blocks, each a shade of its own, a few cracked
  const rowH = 32 * k, bwid = 58 * k;
  t += `<rect x="${f1(-bw)}" y="${f1(top)}" width="${f1(2 * bw)}" height="${f1(floor - top)}" fill="${mortar}"/>`;
  for (let r = 0, y = floor; y > top; r++, y -= rowH)
    for (let i = 0, x = -bw - (r % 2) * bwid / 2; x < bw; i++, x += bwid) {
      const xa = Math.max(x, -bw) + 2 * k, xb = Math.min(x + bwid, bw) - 2 * k, ya = Math.max(y - rowH, top) + 2 * k;
      if (xb - xa < 4 * k || y - 2 * k - ya < 1) continue;   // (and no sliver of a course under the top: a negative height, an error each drawing)
      const v = R(r * 37 + i, 900), col = mix(tone, v < 0.5 ? "#000000" : "#ffffff", 0.05 + 0.12 * Math.abs(v - 0.5));
      t += `<rect x="${f1(xa)}" y="${f1(ya)}" width="${f1(xb - xa)}" height="${f1(y - 2 * k - ya)}" rx="${f1(3 * k)}" fill="${col}"/>`;
      if (R(r * 37 + i, 901) < 0.06) t += `<path d="M${P(xa + (xb - xa) * 0.3, ya)} L${P(xa + (xb - xa) * 0.45, ya + rowH * 0.4)} L${P(xa + (xb - xa) * 0.35, y - 2 * k)}" fill="none" stroke="${mortar}" stroke-width="${f1(1.5 * k)}"/>`;
    }
  // the side walls, going off toward us, their courses in perspective
  for (const sd of [-1, 1]) {
    const [ax, ay] = out(sd * bw, top, y0 - pad), [bx, by] = out(sd * bw, floor, y1 + pad);
    t += `<polygon points="${P(sd * bw, top)} ${P(ax, ay)} ${P(sd * (W + pad), y0 - pad)} ${P(sd * (W + pad), y1 + pad)} ${P(bx, by)} ${P(sd * bw, floor)}" fill="${side}"/>`;
    for (let j = 1; j < 14; j++) {
      const y = top + (floor - top) * j / 14, [ex, ey] = out(sd * bw, y, y < vy ? y0 - pad : y1 + pad);
      t += `<line x1="${f1(sd * bw)}" y1="${f1(y)}" x2="${f1(ex)}" y2="${f1(ey)}" stroke="${mortar}" stroke-width="${f1(2 * k)}" opacity="0.6"/>`;
    }
    for (const u of [0.18, 0.4, 0.66]) {
      const x = sd * (bw + (W * 0.5 + pad - bw) * u), f = (Math.abs(x)) / bw;
      t += `<line x1="${f1(x)}" y1="${f1(vy + (top - vy) * f)}" x2="${f1(x)}" y2="${f1(vy + (floor - vy) * f)}" stroke="${mortar}" stroke-width="${f1(2 * k)}" opacity="0.4"/>`;
    }
  }
  // the vaulted ceiling: ribs springing from the walls, a boss where they meet
  t += `<polygon points="${P(-bw, top)} ${P(bw, top)} ${P(...out(bw, top, y0 - pad))} ${P(...out(-bw, top, y0 - pad))}" fill="${ceil}"/>`;
  for (const sd of [-1, 1]) t += `<path d="M${P(sd * bw, top)} Q${P(sd * bw * 0.5, top - 70 * k)} ${P(0, top - 60 * k)}" fill="none" stroke="${mix(tone, "#000000", 0.25)}" stroke-width="${f1(10 * k)}"/>` +
    `<path d="M${P(sd * bw, top)} Q${P(sd * (bw + 120 * k), y0 - 40 * k)} ${P(sd * (bw + 260 * k), y0 - pad * 0.2)}" fill="none" stroke="${mix(tone, "#000000", 0.25)}" stroke-width="${f1(12 * k)}"/>`;
  t += `<circle cx="0" cy="${f1(top - 60 * k)}" r="${f1(12 * k)}" fill="${mix(tone, "#000000", 0.15)}" stroke="${mortar}" stroke-width="${f1(3 * k)}"/>`;
  // the floor: flagstones toward the back wall, darker where the wall meets it
  const fl = opts.floor || mix(tone, "#5a4a3a", 0.35);
  t += `<polygon points="${P(-bw, floor)} ${P(bw, floor)} ${P(...out(bw, floor, y1 + pad))} ${P(...out(-bw, floor, y1 + pad))}" fill="${fl}"/>`;
  for (let i = -8; i <= 8; i++) { const [ex, ey] = out(i * bw / 8, floor, y1 + pad); t += `<line x1="${f1(i * bw / 8)}" y1="${f1(floor)}" x2="${f1(ex)}" y2="${f1(ey)}" stroke="${mix(fl, "#000000", 0.3)}" stroke-width="${f1(2 * k)}"/>`; }
  t += floorRows(c, mix(fl, "#000000", 0.3));
  for (let i = 0; i < 14; i++) {   // a flagstone here and there a shade lighter or darker
    const u = R(i, 905), x = (R(i, 906) - 0.5) * 2 * bw * (1 + u * 1.6), y = floor + (y1 - floor) * u * u + 6 * k;
    t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1((22 + 40 * u) * k)}" ry="${f1((6 + 14 * u) * k)}" fill="${R(i, 907) < 0.5 ? "#000000" : "#ffffff"}" opacity="0.06"/>`;
  }
  t += `<polygon points="${P(-bw, floor)} ${P(bw, floor)} ${P(...out(bw, floor, y1 + pad))} ${P(...out(-bw, floor, y1 + pad))}" fill="url(#bg-floorfade)"/>`;
  // moss (ticket 284: a wall object; ticket 318: clumps of pads with wavy fingers, mossSvg, the room's kinds by turns): along
  // the foot of the back wall, on the floor; here and there on the back wall, low; and on each side wall near its foot,
  // smaller farther back, its fingers out into the room
  if (opts.moss) { const { at } = roomSpace(c), kinds = ["haircap", "cushion", "windswept"], clump = (x, y, s, on, i) =>
      `<g transform="translate(${f1(x)} ${f1(y)}) scale(${(on === "floor" && R(i, 912) < 0.5 ? -s : s).toFixed(3)} ${s.toFixed(3)})">${mossSvg(kinds[i % 3], 910 + i, { on, share: 0.45 })}</g>`;
    t += wallObj("moss", [...Array(4)].map((_, i) => clump(-bw * 0.9 + 1.8 * bw * (i + 0.5) / 4 + (R(i, 910) - 0.5) * 0.15 * bw, floor + k, (0.34 + 0.14 * R(i, 911)) * k, "floor", i)).join("") +
      [-0.55, 0.62].map((u, i) => clump(u * bw, floor - (34 + 30 * R(i, 913)) * k, 0.3 * k, "back", i + 4)).join("") +
      [-1, 1].flatMap(sd => [-0.12, -0.32].map((z, j) => { const [x, y] = at(sd * bw, floor - 14 * k, z), i = 6 + (sd > 0 ? 2 : 0) + j;
        return clump(x, y, 0.3 * k / (1 + z), sd < 0 ? "left" : "right", i); })).join("")); }
  // cobwebs in the top corners (ticket 284: wall objects, each its own)
  t += wallObj("cobweb, left", cobweb(-bw, top, 70 * k, 1, k)) + wallObj("cobweb, right", cobweb(bw, top, 70 * k, -1, k));
  return t;
}
// an arched doorway in the back wall, dark beyond, a sign over it
const archDoor = (cx, floor, k, label, attrs, w = 90, h = 160) => {
  const hw = w * k / 2, tp = floor - h * k;
  return `<g class="portal" data-door="" ${attrs}><path d="M${f1(cx - hw)} ${f1(floor)} V${f1(tp + hw)} A${f1(hw)} ${f1(hw)} 0 0 1 ${f1(cx + hw)} ${f1(tp + hw)} V${f1(floor)} Z" fill="#1d1712" stroke="#4a3a2a" stroke-width="${f1(10 * k)}"/>` +
    `<path d="M${f1(cx - hw * 0.7)} ${f1(floor)} V${f1(tp + hw * 1.1)} A${f1(hw * 0.7)} ${f1(hw * 0.7)} 0 0 1 ${f1(cx + hw * 0.7)} ${f1(tp + hw * 1.1)} V${f1(floor)}" fill="#2c2219"/>` +
    `<ellipse cx="${f1(cx)}" cy="${f1(floor - 4 * k)}" rx="${f1(hw * 0.6)}" ry="${f1(6 * k)}" fill="#ffcf6b" opacity="0.18"/>` +
    plankSign(cx, tp - 26 * k, k, label) + `</g>`;
};
// a skeleton, in bone-white: hanging by its wrists from shackles (arms up),
// sitting slumped, or curled in a cage; (x, y) is its skull's top
function skeletonSvg(x, y, s, pose = "hang") {
  const B = "#efe8d6", L = "#b9b19c", w = 2.6 * s, bone = (ax, ay, bx, by, wd = w) =>
    `<line x1="${f1(ax)}" y1="${f1(ay)}" x2="${f1(bx)}" y2="${f1(by)}" stroke="${L}" stroke-width="${f1(wd + 1.6 * s)}" stroke-linecap="round"/>` +
    `<line x1="${f1(ax)}" y1="${f1(ay)}" x2="${f1(bx)}" y2="${f1(by)}" stroke="${B}" stroke-width="${f1(wd)}" stroke-linecap="round"/>`;
  const sk = y + 9 * s, neck = y + 19 * s, hip = y + 50 * s;
  let t = "";
  // the spine and ribs
  t += bone(x, neck, x, hip, 2 * s);
  for (let j = 0; j < 4; j++) { const ry = neck + 6 * s + j * 6 * s, rw = (11 - j * 1.5) * s;
    t += `<path d="M${f1(x - rw)} ${f1(ry + 3 * s)} Q${f1(x)} ${f1(ry - 4 * s)} ${f1(x + rw)} ${f1(ry + 3 * s)}" fill="none" stroke="${L}" stroke-width="${f1(3.6 * s)}" stroke-linecap="round"/>` +
         `<path d="M${f1(x - rw)} ${f1(ry + 3 * s)} Q${f1(x)} ${f1(ry - 4 * s)} ${f1(x + rw)} ${f1(ry + 3 * s)}" fill="none" stroke="${B}" stroke-width="${f1(2.2 * s)}" stroke-linecap="round"/>`; }
  t += `<path d="M${f1(x - 10 * s)} ${f1(hip - 2 * s)} Q${f1(x)} ${f1(hip + 8 * s)} ${f1(x + 10 * s)} ${f1(hip - 2 * s)} Q${f1(x)} ${f1(hip - 6 * s)} ${f1(x - 10 * s)} ${f1(hip - 2 * s)} Z" fill="${B}" stroke="${L}" stroke-width="${f1(1.2 * s)}"/>`;
  const sh = neck + 3 * s;
  if (pose === "hang") {
    t += bone(x - 3 * s, sh, x - 16 * s, sh - 14 * s) + bone(x - 16 * s, sh - 14 * s, x - 20 * s, sh - 34 * s) +
         bone(x + 3 * s, sh, x + 16 * s, sh - 14 * s) + bone(x + 16 * s, sh - 14 * s, x + 20 * s, sh - 34 * s) +
         bone(x - 6 * s, hip + 2 * s, x - 8 * s, hip + 26 * s) + bone(x - 8 * s, hip + 26 * s, x - 7 * s, hip + 50 * s) +
         bone(x + 6 * s, hip + 2 * s, x + 9 * s, hip + 25 * s) + bone(x + 9 * s, hip + 25 * s, x + 6 * s, hip + 49 * s);
  } else if (pose === "sit") {
    t += bone(x - 3 * s, sh, x - 14 * s, sh + 18 * s) + bone(x - 14 * s, sh + 18 * s, x - 10 * s, hip + 4 * s) +
         bone(x + 3 * s, sh, x + 15 * s, sh + 18 * s) + bone(x + 15 * s, sh + 18 * s, x + 12 * s, hip + 4 * s) +
         bone(x - 6 * s, hip + 2 * s, x - 26 * s, hip + 6 * s) + bone(x - 26 * s, hip + 6 * s, x - 30 * s, hip + 28 * s) +
         bone(x + 6 * s, hip + 2 * s, x + 24 * s, hip + 10 * s) + bone(x + 24 * s, hip + 10 * s, x + 30 * s, hip + 30 * s);
  } else {
    t += bone(x - 3 * s, sh, x - 12 * s, sh + 12 * s) + bone(x - 12 * s, sh + 12 * s, x + 2 * s, hip) +
         bone(x + 3 * s, sh, x + 12 * s, sh + 14 * s) + bone(x + 12 * s, sh + 14 * s, x + 6 * s, hip + 6 * s) +
         bone(x - 6 * s, hip + 2 * s, x - 18 * s, hip - 8 * s) + bone(x - 18 * s, hip - 8 * s, x - 10 * s, hip + 14 * s) +
         bone(x + 6 * s, hip + 2 * s, x + 18 * s, hip - 6 * s) + bone(x + 18 * s, hip - 6 * s, x + 12 * s, hip + 16 * s);
  }
  // the skull: its dome, its jaw, its dark eyes and nose
  t += `<path d="M${f1(x - 9 * s)} ${f1(sk + 3 * s)} Q${f1(x - 10 * s)} ${f1(y - 3 * s)} ${f1(x)} ${f1(y - 3 * s)} Q${f1(x + 10 * s)} ${f1(y - 3 * s)} ${f1(x + 9 * s)} ${f1(sk + 3 * s)} ` +
       `Q${f1(x + 8 * s)} ${f1(sk + 8 * s)} ${f1(x + 5 * s)} ${f1(sk + 9 * s)} L${f1(x - 5 * s)} ${f1(sk + 9 * s)} Q${f1(x - 8 * s)} ${f1(sk + 8 * s)} ${f1(x - 9 * s)} ${f1(sk + 3 * s)} Z" fill="${B}" stroke="${L}" stroke-width="${f1(1.3 * s)}"/>` +
       `<rect x="${f1(x - 5 * s)}" y="${f1(sk + 8 * s)}" width="${f1(10 * s)}" height="${f1(4 * s)}" rx="${f1(1.5 * s)}" fill="${B}" stroke="${L}" stroke-width="${f1(1 * s)}"/>` +
       [-1, 1].map(e => `<ellipse cx="${f1(x + e * 4 * s)}" cy="${f1(sk + 1.5 * s)}" rx="${f1(2.8 * s)}" ry="${f1(3.2 * s)}" fill="#2a2420"/>`).join("") +
       `<path d="M${f1(x)} ${f1(sk + 4.5 * s)} L${f1(x - 1.5 * s)} ${f1(sk + 7 * s)} L${f1(x + 1.5 * s)} ${f1(sk + 7 * s)} Z" fill="#2a2420"/>` +
       [-3, -1, 1, 3].map(dx => `<line x1="${f1(x + dx * s)}" y1="${f1(sk + 8.5 * s)}" x2="${f1(x + dx * s)}" y2="${f1(sk + 11.5 * s)}" stroke="${L}" stroke-width="${f1(0.8 * s)}"/>`).join("");
  return t;
}
// a chain hanging from (x, y) down `len`, its links side-on and face-on in turn
const chainSvg = (x, y, len, s) => [...Array(Math.max(1, Math.floor(len / (7 * s))))].map((_, i) => i % 2
  ? `<ellipse cx="${f1(x)}" cy="${f1(y + i * 7 * s + 3.5 * s)}" rx="${f1(1.4 * s)}" ry="${f1(4.5 * s)}" fill="#55555f"/>`
  : `<ellipse cx="${f1(x)}" cy="${f1(y + i * 7 * s + 3.5 * s)}" rx="${f1(3 * s)}" ry="${f1(4.5 * s)}" fill="none" stroke="#6a6a76" stroke-width="${f1(1.6 * s)}"/>`).join("");
// a shackle: an iron ring on a plate in the wall
const shackle = (x, y, s) => `<rect x="${f1(x - 5 * s)}" y="${f1(y - 5 * s)}" width="${f1(10 * s)}" height="${f1(10 * s)}" fill="#3a3a44"/>` +
  `<circle cx="${f1(x)}" cy="${f1(y + 8 * s)}" r="${f1(6 * s)}" fill="none" stroke="#5a5a66" stroke-width="${f1(2.6 * s)}"/>`;
// a ladder up to a hole in the ceiling, light falling through it
// (ticket 252: its opening's shape, the pit's above it)
const ladderUp = (x, floor, top, k, label, attrs, shape = "circle") => `<g class="portal" ${attrs}>` +
  `<polygon points="${f1(x - 50 * k)},${f1(top)} ${f1(x + 50 * k)},${f1(top)} ${f1(x + 120 * k)},${f1(floor + 40 * k)} ${f1(x - 120 * k)},${f1(floor + 40 * k)}" fill="#fff6c8" opacity="0.12"/>` +
  `<path d="${openingD(shape, x, top, 56 * k, 14 * k, true)}" fill="#fff3c4" opacity="0.8"/>` +
  `<g data-ladder="1" data-up="${f1(top)}">` + [-1, 1].map(e => `<line x1="${f1(x + e * 22 * k)}" y1="${f1(top)}" x2="${f1(x + e * 30 * k)}" y2="${f1(floor + 10 * k)}" stroke="#6e4a2c" stroke-width="${f1(7 * k)}"/>`).join("") +   // (ticket 252: the opening's line)
  [...Array(9)].map((_, j) => { const u = (j + 0.5) / 9, y = top + (floor + 10 * k - top) * u, hw = (22 + 8 * u) * k;
    return `<line x1="${f1(x - hw)}" y1="${f1(y)}" x2="${f1(x + hw)}" y2="${f1(y)}" stroke="#8b5e34" stroke-width="${f1(5 * k)}"/>`; }).join("") + `</g>` +
  plankSign(x, floor - 200 * k, k, label) + `</g>`;

// (ticket 284) the trap room's lever, shared with the 🎚️ lever a world designer
// places: its plate (x, y, its top left), its pivot (px, py) and the end of its
// handle (ex, ey), where the knob is; arm wraps the handle and the knob (a
// placed lever turns them as it is pulled)
// (ticket 317, Matthew: "make levels more detailed, especially the panel and add a shine to the level that mimics the
// light of the room in each instance (configurable though). Shine size and color and 'vibration level' (the torches
// vibrate and we should mimic that, by default)") Its plate an iron panel: bevelled, a rivet at each corner, the slot
// the handle swings in, a hub at the pivot; the handle with a bright edge; the knob with a ring and a gleam. And sh, its
// shine (leverShine: the place's ✨ settings): the room's light caught on the knob (moving with it) and down the plate's
// edge, as big as its size says, in its colour, flickering as the torches do (the scene's slow LFO, as torchSvg's glow;
// without one, a flicker of its own) as much as its flicker says
const leverShine = o => ({ size: Math.max(0, Number(o.leverShine ?? 100) || 0) / 100, color: o.leverShineColor || "#ffd27a", flicker: Math.max(0, Math.min(100, Number(o.leverFlicker ?? 60) || 0)) / 100, o });
function leverSvg(x, y, px, py, ex, ey, k, arm = s => s, knob = "#c0392b", sh = null) {   // (ticket 323: its knob's colour)
  const w = 26 * k, h = 40 * k, rivet = (cx, cy) => `<circle cx="${f1(cx)}" cy="${f1(cy)}" r="${f1(1.8 * k)}" fill="#9a9aa8" stroke="#24242b" stroke-width="${f1(0.6 * k)}"/>`;
  const glow = sh && sh.size > 0, f = glow ? sh.flicker : 0;
  const flick = base => !glow || f <= 0 ? "" : sceneLfo(sh.o, "slow", 0.37, "opacity", (v, amp) => (base * (1 - f * (0.3 + 0.5 * amp) * (1 + v) / 2)).toFixed(2),
    f > 0 ? `<animate attributeName="opacity" values="${[1, 1 - 0.55 * f, 1 - 0.15 * f, 1 - 0.65 * f, 1 - 0.1 * f, 1].map(m => (base * m).toFixed(2)).join(";")}" dur="1.3s" repeatCount="indefinite"/>` : "", "translate");
  return `<rect x="${f1(x)}" y="${f1(y)}" width="${f1(w)}" height="${f1(h)}" rx="${f1(2 * k)}" fill="#2e2e36" stroke="#1b1b20" stroke-width="${f1(1 * k)}"/>` +
    `<rect x="${f1(x + 2.5 * k)}" y="${f1(y + 2.5 * k)}" width="${f1(w - 5 * k)}" height="${f1(h - 5 * k)}" rx="${f1(1.5 * k)}" fill="#3c3c46"/>` +
    `<line x1="${f1(x + 2.5 * k)}" y1="${f1(y + 2.8 * k)}" x2="${f1(x + w - 2.5 * k)}" y2="${f1(y + 2.8 * k)}" stroke="#5e5e6a" stroke-width="${f1(1 * k)}"/>` +
    `<rect x="${f1(px - 2.6 * k)}" y="${f1(y + 6 * k)}" width="${f1(5.2 * k)}" height="${f1(h - 12 * k)}" rx="${f1(2.6 * k)}" fill="#15151a"/>` +
    rivet(x + 4 * k, y + 4 * k) + rivet(x + w - 4 * k, y + 4 * k) + rivet(x + 4 * k, y + h - 4 * k) + rivet(x + w - 4 * k, y + h - 4 * k) +
    (glow ? `<rect x="${f1(x + 1 * k)}" y="${f1(y + 4 * k)}" width="${f1(1.6 * k * Math.min(2, sh.size))}" height="${f1(h - 8 * k)}" fill="${sh.color}" opacity="0.55">${flick(0.55)}</rect>` : "") +
    arm(`<line x1="${f1(px)}" y1="${f1(py)}" x2="${f1(ex)}" y2="${f1(ey)}" stroke="#6a6a76" stroke-width="${f1(5 * k)}" stroke-linecap="round"/>` +
        `<line x1="${f1(px - 1.2 * k)}" y1="${f1(py)}" x2="${f1(ex - 1.2 * k)}" y2="${f1(ey)}" stroke="#a4a4b2" stroke-width="${f1(1.3 * k)}" stroke-linecap="round"/>` +
        (glow ? `<circle cx="${f1(ex)}" cy="${f1(ey)}" r="${f1(12 * k * sh.size)}" fill="${sh.color}" opacity="0.18">${flick(0.18)}</circle>` : "") +
        `<circle cx="${f1(ex)}" cy="${f1(ey)}" r="${f1(7 * k)}" fill="${knob}" stroke="${darker(knob)}" stroke-width="${f1(1.2 * k)}"/>` +
        `<ellipse cx="${f1(ex - 2.2 * k)}" cy="${f1(ey - 2.6 * k)}" rx="${f1(2.4 * k * (glow ? Math.max(0.6, sh.size) : 1))}" ry="${f1(1.5 * k * (glow ? Math.max(0.6, sh.size) : 1))}" fill="${glow ? sh.color : "#ffffff"}" opacity="0.85">${flick(0.85)}</ellipse>`) +
    `<circle cx="${f1(px)}" cy="${f1(py)}" r="${f1(4.5 * k)}" fill="#55555f" stroke="#202026" stroke-width="${f1(1 * k)}"/><circle cx="${f1(px)}" cy="${f1(py)}" r="${f1(1.6 * k)}" fill="#9a9aa8"/>`;
}
// (ticket 317) the trap room's helmet, recognizable: a knight's great helm lying on its rim, its dome with a ridge and a
// shine, the visor's eye slit, breathing holes, a band of rivets round the rim, a red plume; (cx, base) the middle of its
// rim on the floor. (ticket 435, Matthew: "we had a helmet in the dungeon -- can we follow that style, and then use the same
// routine in both places?"; "also permits us to place helmets in each of the 4 poses (L, R, toward, away)") Seen four ways:
// facing us (as the trap room has it), from behind (o.back: no front plate, slit or holes), or from the side (o.side, the way
// its front faces, -1 our left, 1 our right): a drawing of its own, the front plate edge-on at that side as a strip with the
// slit's end and the breathing holes on it, the plume falling back and the shine on the back. And worn (o.worn: no shadow on
// the floor), its plume in any colour (o.plume; false, none), or open (o.open: a window for the face instead of the front
// plate, facing us). Called as the trap room calls it, it draws as it always did.
function helmetSvg(cx, base, k, o = {}) {
  const w = 30 * k, h = 50 * k, steel = "#a3a9b4", dark = "#5a5f6a", lt = "#e2e6ee", hole = "#1d1f24", P = (x, y) => `${f1(x)} ${f1(y)}`;
  const sd = o.side || 0, pc = o.plume === undefined ? "#c0392b" : o.plume, pl = pc === "#c0392b" ? "#7a1f16" : pc ? darker(pc) : "";
  const fx = sd ? -sd : 1, X = d => cx + fx * d, front = !o.back && !o.open && !sd;
  const dome = `M${P(cx - w, base)} L${P(cx - w, base - 0.55 * h)} Q${P(cx - w, base - h)} ${P(cx, base - h)} Q${P(cx + w, base - h)} ${P(cx + w, base - 0.55 * h)} L${P(cx + w, base)} Z`;
  const win = o.open && !sd && !o.back ? ` M${P(cx - 0.6 * w, base - 0.45 * h)} A${f1(0.6 * w)} ${f1(0.37 * h)} 0 1 0 ${P(cx + 0.6 * w, base - 0.45 * h)} A${f1(0.6 * w)} ${f1(0.37 * h)} 0 1 0 ${P(cx - 0.6 * w, base - 0.45 * h)} Z` : "";
  // (the side: the plate's edge from 0.55 of the half-width out to the dome's side, its slit a notch at the front, holes on it)
  const side = !sd ? "" : `<path d="M${P(cx + sd * 0.5 * w, base - 0.7 * h)} L${P(cx + sd * w, base - 0.62 * h)} L${P(cx + sd * w, base - 6 * k)} L${P(cx + sd * 0.55 * w, base - 7 * k)} Z" fill="#959ba6" stroke="${dark}" stroke-width="${f1(1.2 * k)}"/>` +
    `<rect x="${f1(sd > 0 ? cx + 0.6 * w : cx - w)}" y="${f1(base - 0.66 * h + 2.5 * k)}" width="${f1(0.4 * w)}" height="${f1(4.5 * k)}" rx="${f1(1.5 * k)}" fill="${hole}"/>` +
    [0, 1, 2].map(r => `<circle cx="${f1(cx + sd * 0.8 * w)}" cy="${f1(base - 0.66 * h + (12 + 5 * r) * k)}" r="${f1(1.4 * k)}" fill="${hole}"/>`).join("");
  return (o.worn ? "" : `<ellipse cx="${f1(cx)}" cy="${f1(base + 2 * k)}" rx="${f1(w * 1.15)}" ry="${f1(5 * k)}" fill="#000000" opacity="0.22"/>`) +
    (pc ? `<path d="M${P(X(4 * k), base - h + 2 * k)} Q${P(X(18 * k), base - h - 16 * k)} ${P(X(34 * k), base - h - 4 * k)} Q${P(X(20 * k), base - h - 6 * k)} ${P(X(8 * k), base - h + 6 * k)} Z" fill="${pc}" stroke="${pl}" stroke-width="${f1(1 * k)}"/>` : "") +
    `<path d="${dome}${win}" fill="${steel}"${win ? ' fill-rule="evenodd"' : ""} stroke="${dark}" stroke-width="${f1(2 * k)}"/>` +
    (front ? `<path d="M${P(cx - 0.86 * w, base - 0.66 * h)} L${P(cx + 0.86 * w, base - 0.66 * h)} L${P(cx + 0.8 * w, base - 7 * k)} Q${P(cx, base - 3 * k)} ${P(cx - 0.8 * w, base - 7 * k)} Z" fill="#959ba6" stroke="${dark}" stroke-width="${f1(1.2 * k)}"/>` +
    `<rect x="${f1(cx - 0.7 * w)}" y="${f1(base - 0.66 * h + 2.5 * k)}" width="${f1(1.4 * w)}" height="${f1(4.5 * k)}" rx="${f1(2 * k)}" fill="${hole}"/>` : "") + side +
    `<line x1="${f1(cx)}" y1="${f1(base - h + 1 * k)}" x2="${f1(cx)}" y2="${f1(base - 0.66 * h)}" stroke="${dark}" stroke-width="${f1(1.8 * k)}"/>` +
    (front ? `<line x1="${f1(cx)}" y1="${f1(base - 0.66 * h + 8 * k)}" x2="${f1(cx)}" y2="${f1(base - 6 * k)}" stroke="${dark}" stroke-width="${f1(1.4 * k)}"/>` +
    [0, 1, 2].flatMap(r => [0, 1].map(cl => `<circle cx="${f1(X((6 + 5 * cl) * k))}" cy="${f1(base - 0.66 * h + (12 + 5 * r) * k)}" r="${f1(1.4 * k)}" fill="${hole}"/>`)).join("") : "") +
    `<rect x="${f1(cx - w)}" y="${f1(base - 6 * k)}" width="${f1(2 * w)}" height="${f1(6 * k)}" fill="#7d838e" stroke="${dark}" stroke-width="${f1(1 * k)}"/>` +
    [0, 1, 2, 3, 4, 5].map(i => `<circle cx="${f1(cx - w + (5 + i * (2 * w - 10 * k) / 5 / k) * k)}" cy="${f1(base - 3 * k)}" r="${f1(1.3 * k)}" fill="${lt}"/>`).join("") +
    `<path d="M${P(X(-0.7 * w), base - 0.7 * h)} Q${P(X(-0.72 * w), base - 0.95 * h)} ${P(X(-0.25 * w), base - 0.98 * h)}" fill="none" stroke="${lt}" stroke-width="${f1(3 * k)}" stroke-linecap="round" opacity="0.8"/>`;
}
// (ticket 317) the trap room's sword, lying flat on the floor from its pommel (x0, y0) to its tip (x1, y1): a blade with
// its fuller and a bright edge, a crossguard, a grip bound in leather, a round pommel
function swordSvg(x0, y0, x1, y1, k) {
  const len = Math.hypot(x1 - x0, y1 - y0), a = Math.atan2(y1 - y0, x1 - x0) * 180 / Math.PI, s = len / 100;
  return `<g transform="translate(${f1(x0)} ${f1(y0)}) rotate(${a.toFixed(1)}) scale(${s.toFixed(3)})">` +
    `<polygon points="22,-3.4 88,-2.6 100,0 88,2.6 22,3.4" fill="#c9ced6" stroke="#7d838e" stroke-width="0.7"/>` +
    `<line x1="25" y1="0" x2="80" y2="0" stroke="#9aa0aa" stroke-width="1.1"/><line x1="23" y1="-2.6" x2="88" y2="-1.9" stroke="#f1f3f7" stroke-width="0.7"/>` +
    `<rect x="19" y="-10" width="3.6" height="20" rx="1.4" fill="#b08d3a" stroke="#6e5520" stroke-width="0.6"/>` +
    `<rect x="5" y="-2.3" width="14" height="4.6" rx="1.2" fill="#6e4a2c"/>` + [7, 10, 13, 16].map(xx => `<line x1="${xx}" y1="-2.3" x2="${xx + 1.6}" y2="2.3" stroke="#4a2e1a" stroke-width="0.7"/>`).join("") +
    `<circle cx="3" cy="0" r="3.3" fill="#b08d3a" stroke="#6e5520" stroke-width="0.6"/></g>`;
}
// (ticket 284) the trap room's pit, shared with the 🕳️ trap door: its opening
// (shape sh, middle cx, cy, half widths rx, ry), dark inside, spikes glinting
// in it (R and i as the trap room's, so its two pits differ), a rim of broken
// stone; its ids from id
function pitHole(cx, cy, rx, ry, sh, k, R, i, id, spikes = true) {
  const P = (x, y) => `${f1(x)} ${f1(y)}`;
  return `<defs><radialGradient id="${id}" cx="0.5" cy="0.3" r="0.7"><stop offset="0" stop-color="#000000"/><stop offset="1" stop-color="#2a221c"/></radialGradient>` +
    `<clipPath id="${id}c"><path d="${openingD(sh, cx, cy, rx, ry)}"/></clipPath></defs>` +
    `<path d="${openingD(sh, cx, cy + 4 * k, rx + 14 * k, ry + 8 * k)}" fill="#4a4038"/>` +
    `<path d="${openingD(sh, cx, cy, rx, ry)}" fill="url(#${id})"/>` +
    (spikes ? `<g clip-path="url(#${id}c)">` + [...Array(11)].map((_, j) => {
      const x = cx - rx * 0.85 + rx * 1.7 * j / 10, b = cy + ry * 0.9, h = (26 + 10 * R(j + i * 20, 920)) * k;
      return `<polygon points="${P(x - 7 * k, b)} ${P(x, b - h)} ${P(x + 7 * k, b)}" fill="#8a8a96"/><line x1="${f1(x - 2 * k)}" y1="${f1(b - h * 0.8)}" x2="${f1(x - 4 * k)}" y2="${f1(b - 4 * k)}" stroke="#d8d8e4" stroke-width="${f1(1.5 * k)}"/>`;
    }).join("") + `</g>` : "");
}
// (ticket 305) a pit's lid, shut, as a trap door is: planks over its opening, a bar across, an iron ring (its rim of broken
// stone is pitLidSvg's); (ticket 307) its own clip, and a shade over it, deepened as it swings down into the dark (lidSwing)
function lidSvg(cx, cy, rx, ry, sh, k, id) {
  const planks = [...Array(7)].map((_, j) => `<rect x="${f1(cx - rx + 2 * rx * j / 7)}" y="${f1(cy - ry - 2 * k)}" width="${f1(2 * rx / 7 - 2 * k)}" height="${f1(2 * ry + 4 * k)}" fill="${j % 2 ? "#7a5230" : "#6e4a2c"}"/>`).join("");
  return `<defs><clipPath id="${id}l"><path d="${openingD(sh, cx, cy, rx, ry)}"/></clipPath></defs>` +
    `<g clip-path="url(#${id}l)">${planks}<rect x="${f1(cx - rx)}" y="${f1(cy - 4 * k)}" width="${f1(2 * rx)}" height="${f1(8 * k)}" fill="#4a3220"/></g>` +
    `<path d="${openingD(sh, cx, cy, rx, ry)}" fill="none" stroke="#3a2a18" stroke-width="${f1(3 * k)}"/><ellipse cx="${f1(cx)}" cy="${f1(cy)}" rx="${f1(11 * k)}" ry="${f1(5 * k)}" fill="none" stroke="#2a2a30" stroke-width="${f1(3 * k)}"/>` +
    `<path class="lid-shade" d="${openingD(sh, cx, cy, rx, ry)}" fill="#000000" opacity="0"/>`;
}
// (ticket 307, Matthew: "pit open and close work via switch -- let's animate it, like the doors opening and closing are
// now") A pit with its lid: the hole always drawn (pitHole), the lid over it, the rim of broken stone over both. Shut,
// the lid lies across the opening; open, it has swung down on its hinge, the opening's far edge, into the pit, out of
// sight (folded flat against the hinge, lidAt(y0, 0), and hidden). lidSwing turns it between the two when a lever is pulled.
const lidAt = (y0, s) => `matrix(1 0 0 ${s.toFixed(3)} 0 ${f1(y0 * (1 - s))})`;
function pitLidSvg(cx, cy, rx, ry, sh, k, R, i, id, open, spikes = true) {
  const y0 = cy - ry;
  return pitHole(cx, cy, rx, ry, sh, k, R, i, id, spikes) +
    `<g class="pit-lid" data-pit-i="${i}" data-hinge="${f1(y0)}"${open ? ` transform="${lidAt(y0, 0)}" visibility="hidden"` : ""}>` + lidSvg(cx, cy, rx, ry, sh, k, id) + `</g>` +
    pitRim(cx, cy, rx, ry, k, R, i);
}
const pitRim = (cx, cy, rx, ry, k, R, i) => [...Array(12)].map((_, j) => { const a = j / 12 * 2 * Math.PI, x = cx + Math.cos(a) * (rx + 6 * k), y = cy + Math.sin(a) * (ry + 4 * k);
  return `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1((10 + 6 * R(j, 921 + i)) * k)}" ry="${f1(6 * k)}" fill="${R(j, 922) < 0.5 ? "#6b625a" : "#5a524a"}"/>`; }).join("");
// The trap room: bare stone, two torches, a lever on the wall, and two pits
// in the floor, spikes inside, each a drop into one of the castle's dungeons
function trapRoomSvg(c, K, pits, shapes = K.pitShapes, trap = null) {
  const { x0, x1, y1, W, k, R, P } = c, { floor, top, bw, out } = roomFrame(c);
  let t = roomShell(c, ROOM_TONE.trap, { moss: "#4f6b3a" });
  t += wallObj("torch, left", torchSvg(-bw * 0.62, floor - 210 * k, k, 0, c.o)) + wallObj("torch, right", torchSvg(bw * 0.62, floor - 210 * k, k, 1, c.o));   // (ticket 284)
  // a lever on the wall, and a warning carved over the pits
  // (ticket 305) two levers, left and right, each tapped to pull it: its handle up (its pit open) or pulled down (its pit shut);
  // the left the right's mirror; each a wall object marked data-lever, its handle a .lever-arm turned by turnArm
  t += [["L", -1], ["R", 1]].map(([side, sd]) => {
    const px = sd * (bw * 0.32 + 13 * k), py = floor - 130 * k, up = trap ? !trap.levers[side] : true, a = (up ? 1 : -1) * sd * LEVER_TILT;
    return wallObj(side === "L" ? "lever, left" : "lever, right", `<g data-lever="${side}" style="cursor: pointer">` +
      leverSvg(px - 13 * k, floor - 150 * k, px, py, px, py - 44.4 * k, k, s => `<g class="lever-arm" data-pivot="${f1(px)} ${f1(py)}" transform="rotate(${a} ${f1(px)} ${f1(py)})">${s}</g>`, undefined, leverShine(c.o)) + `</g>`);
  }).join("") +
       // (ticket 323) the boulder levers, blue-knobbed, under the red ones: each down when its boulder has fallen
       (trap && trap.boulders ? [["L", -1], ["R", 1]].map(([side, sd]) => {
         const px = sd * (bw * 0.32 + 13 * k), py = floor - 64 * k, a = (trap.boulders[side] ? -1 : 1) * sd * LEVER_TILT;
         return wallObj(side === "L" ? "boulder lever, left" : "boulder lever, right", `<g data-lever="B${side}" style="cursor: pointer">` +
           leverSvg(px - 13 * k, floor - 84 * k, px, py, px, py - 44.4 * k, k, s => `<g class="lever-arm" data-pivot="${f1(px)} ${f1(py)}" transform="rotate(${a} ${f1(px)} ${f1(py)})">${s}</g>`, BOULDER_KNOB, leverShine(c.o)) + `</g>`);
       }).join("") : "") +
       wallObj("warning sign", `<rect x="${f1(-bw * 0.38)}" y="${f1(top + 40 * k)}" width="${f1(bw * 0.76)}" height="${f1(44 * k)}" rx="${f1(6 * k)}" fill="#3a3530" opacity="0.85"/>` +
       `<text x="0" y="${f1(top + 72 * k)}" text-anchor="middle" font-size="${f1(26 * k)}" font-weight="bold" fill="#f2c12e" font-family="sans-serif">⚠️ MIND THE FLOOR ⚠️</text>`);
  // the pits: dark holes in the floor, their rims of broken stone, rows of
  // spikes glinting inside
  [-1, 1].forEach((sd, i) => {
    const cx = sd * W * 0.2, cy = floor + (y1 - floor) * 0.5, rx = 110 * k, ry = 34 * k, sh = shapes[i];   // (ticket 252: its opening's shape; ticket 271: this room's)
    const id = `bg-pit${i}`;
    // (ticket 305) a pit not there is not drawn; a shut one is planks, no way down (no doorway, so no fall)
    if (trap && !trap.on[i]) return;
    const covered = !!(trap && trap.covered[i]), open = !trap || trap.open[i], to = open && !covered ? pits[i] : null;   // (ticket 323: a boulder in it, no way down)
    // (ticket 323) its boulder, there while the room has boulder levers: in the hole when fallen (a rock standing there, so a
    // solid), else up out of sight at the ceiling, where it falls from (boulderFall); data-drop: how far
    const boulder = () => { if (!trap || !trap.boulders) return "";
      const rad = rx * 0.8, base = cy + ry * 0.55, drop = base - top + 90 * k, sd = trap.seeds[i] * 7 + i;   // (ticket 328: its own seed)
      const tone = trap.tone === "surprise" ? ROCK_TONES[Math.floor(seeded(trap.seeds[i])(i + 50) * ROCK_TONES.length)] : trap.tone;
      // (ticket 338: waiting up at the ceiling, always: once landed it is a placed rock, "fallen N", no longer the room's)
      const g = `<g data-boulder="${i}" data-drop="${f1(drop)}" data-at="${f1(cx)} ${f1(base)} ${f1(rad)} ${sd} ${tone}" transform="translate(0 ${f1(-drop)})" visibility="hidden">${rockSvg(cx, base, rad, sd, tone)}</g>`;
      return g; };
    const spikes = !!(trap && trap.spikes);   // (ticket 334: off unless the room's toggle is on)
    if (!open) { t += `<g>` + pitLidSvg(cx, cy, rx, ry, sh, k, R, i, id, false, spikes) + `</g>` + boulder(); return; }   // (ticket 307: its lid across it)
    // (ticket 162: its hole, for falling in: its middle and its half widths, across and in depth)
    t += (to ? `<g class="portal" data-to="castleroom" data-room="${to}" data-pit="${f1(cx)} ${f1(cy)} ${f1(rx)} ${f1(ry)}" data-shape="${sh}">` : "<g>") +
      pitLidSvg(cx, cy, rx, ry, sh, k, R, i, id, true, spikes) +
      (to ? plankSign(cx, cy - ry - 34 * k, k, `🕳️ ${roomLabel(K.rooms[to - 1]).split(" ")[0]} ${to}`) : "") + `</g>` + boulder();
  });
  // a helmet someone dropped by the left pit, a sword by the right one
  // (the helmet an object, standing where its rim meets the floor; the pits
  // are holes in the floor, and the sword lies flat on it: neither stands,
  // so the sword is drawn first, under all that stands, the friend too)
  t += floorObj("sword", swordSvg(W * 0.31, floor + 127 * k, W * 0.42, floor + 110 * k, k)) +   // (ticket 284: a floor object; ticket 317: in detail)
       obj("helmet", floor + 70 * k, helmetSvg(-W * 0.33, floor + 70 * k, k));   // (ticket 317: a knight's helm)
  return t;
}
// a dungeon: cold stone, wet, a barred window high up with moonlight coming
// through, chains and shackles on the walls, straw, a bucket, a rat; water
// dripping. Skeletons only as many as asked for (`bones`, none at first),
// the least scary first: in the deep dungeon the skull under the candle,
// the one sitting in the cell, the one curled in the cage, the one hanging;
// in the first, the two hanging (and the bones on the floor come with them)
function dungeonSvg(c, K, deep, bones = 0) {
  const { x0, x1, y1, W, k, R, P } = c, { floor, top, bw } = roomFrame(c);
  let t = roomShell(c, deep ? ROOM_TONE.dungeon2 : ROOM_TONE.dungeon1, { moss: deep ? "#3f6b3a" : "#4a5a4a", floor: deep ? "#3a3e36" : "#45474e" });
  // the barred window, high in the back wall, and its beam of moonlight
  // (ticket 490: the moon in it the date's, and the beam as bright as the moon is lit: as it was at full, faint at new)
  const wx = deep ? bw * 0.45 : -bw * 0.5, wy = top + 60 * k, lit = (1 - Math.cos(2 * Math.PI * moonOfDate(c.o).phase)) / 2;
  t += wallObj("window", `<polygon points="${P(wx - 30 * k, wy + 30 * k)} ${P(wx + 30 * k, wy + 30 * k)} ${P(wx + 150 * k, floor + 90 * k)} ${P(wx - 20 * k, floor + 110 * k)}" fill="#cfe3ff" opacity="${(0.02 + 0.08 * lit).toFixed(3)}"/>` +
       `<rect x="${f1(wx - 32 * k)}" y="${f1(wy - 24 * k)}" width="${f1(64 * k)}" height="${f1(52 * k)}" fill="#1b2840"/>` + moonOfDateSvg(c.o, wx + 10 * k, wy - 6 * k, 10 * k, "#1b2840") +
       [...Array(5)].map((_, j) => `<line x1="${f1(wx - 26 * k + j * 13 * k)}" y1="${f1(wy - 24 * k)}" x2="${f1(wx - 26 * k + j * 13 * k)}" y2="${f1(wy + 28 * k)}" stroke="#3a3a44" stroke-width="${f1(4 * k)}"/>`).join("") +
       `<rect x="${f1(wx - 32 * k)}" y="${f1(wy - 24 * k)}" width="${f1(64 * k)}" height="${f1(52 * k)}" fill="none" stroke="#2a2a32" stroke-width="${f1(5 * k)}"/>`);   // (ticket 284: wall objects, the window with its moonbeam, the torch)
  t += wallObj("torch", torchSvg(deep ? -bw * 0.7 : bw * 0.7, floor - 200 * k, k, deep ? 2 : 1, c.o));
  // skeletons hanging by their wrists from the back wall, chains to their shackles
  // (or, with none, the chains hanging empty, an open cuff at each end)
  const hung = deep ? [[-bw * 0.15, 1, bones >= 4]] : [[-bw * 0.05, 1.1, bones >= 1], [bw * 0.3, 1, bones >= 2]];
  for (const [hi, [hx, z, shown]] of hung.entries()) {
    const s = 2.1 * k * z, hy = floor - 230 * k * z;
    t += wallObj(hung.length > 1 ? `chains ${hi + 1}` : "chains", shackle(hx - 20 * s, hy - 40 * s, s) + shackle(hx + 20 * s, hy - 40 * s, s) + chainSvg(hx - 20 * s, hy - 32 * s, 14 * s, s) + chainSvg(hx + 20 * s, hy - 32 * s, 14 * s, s) +
         (shown ? skeletonSvg(hx, hy, s, "hang") : [-1, 1].map(e => `<path d="M${f1(hx + e * 20 * s - 4 * s)} ${f1(hy - 18 * s)} a${f1(4 * s)} ${f1(4 * s)} 0 1 0 ${f1(8 * s)} 0" fill="none" stroke="#5a5a66" stroke-width="${f1(1.8 * s)}"/>`).join("")));   // (ticket 284: a wall object)
  }
  // empty shackles and chains along the walls
  for (const [si, sx] of (deep ? [-bw * 0.45, bw * 0.05] : [-bw * 0.75, bw * 0.55]).entries()) t += wallObj(`shackles ${si + 1}`, shackle(sx, floor - 150 * k, 2 * k) + chainSvg(sx, floor - 140 * k, 90 * k, 2 * k));   // (ticket 284)
  // water dripping from the ceiling into a puddle
  // (flat on the floor, and the drop no object: drawn before all that stands, the friend too)
  const dx = deep ? -bw * 0.05 : bw * 0.1;
  t += floorObj("puddle and drip", `<ellipse cx="${f1(dx)}" cy="${f1(floor + 40 * k)}" rx="${f1(50 * k)}" ry="${f1(10 * k)}" fill="#5f7fa0" opacity="0.5"/>` +
       `<ellipse cx="${f1(dx)}" cy="${f1(floor + 40 * k)}" rx="${f1(10 * k)}" ry="${f1(2 * k)}" fill="none" stroke="#cfe3ff" stroke-width="${f1(1.5 * k)}" opacity="0.7">` +
       `<animate attributeName="rx" values="${f1(4 * k)};${f1(40 * k)}" dur="1.6s" repeatCount="indefinite"/><animate attributeName="opacity" values="0.8;0" dur="1.6s" repeatCount="indefinite"/></ellipse>` +
       `<circle cx="${f1(dx)}" cy="${f1(top)}" r="${f1(3 * k)}" fill="#cfe3ff"><animate attributeName="cy" values="${f1(top)};${f1(floor + 40 * k)}" dur="1.6s" repeatCount="indefinite"/></circle>`);   // (ticket 284: the drip falls into it)
  if (deep) {
    // a cage hanging from the ceiling, a skeleton curled inside
    // (an object, as a thing hung from the ceiling is: at the floor under it,
    // here the back wall's foot, as it is drawn at the back wall's size)
    const cx = W * 0.27, cy = top + 70 * k, cw = 70 * k, ch = 120 * k;
    t += obj("cage", floor, chainSvg(cx, top - 40 * k, 110 * k, 2 * k) + (bones >= 3 ? skeletonSvg(cx, cy + 40 * k, 1.5 * k, "cage") : "") +
         `<path d="M${P(cx - cw / 2, cy + ch)} V${f1(cy + 20 * k)} Q${P(cx, cy - 20 * k)} ${P(cx + cw / 2, cy + 20 * k)} V${f1(cy + ch)} Z" fill="none" stroke="#3a3a44" stroke-width="${f1(4 * k)}"/>` +
         // (its bars, each from the dome over it down to the floor: the dome, at a share t across, is at cy + 20k - 80k t (1 - t))
         [...Array(6)].map((_, j) => { const t = j / 5; return `<line x1="${f1(cx - cw / 2 + cw * t)}" y1="${f1(cy + 20 * k - 80 * k * t * (1 - t))}" x2="${f1(cx - cw / 2 + cw * t)}" y2="${f1(cy + ch)}" stroke="#3a3a44" stroke-width="${f1(3 * k)}"/>`; }).join("") +
         `<ellipse cx="${f1(cx)}" cy="${f1(cy + ch)}" rx="${f1(cw / 2 + 4 * k)}" ry="${f1(8 * k)}" fill="#2a2a32"/>`);
    // a cell across the left: iron bars, a skeleton sitting chained inside
    // (one object, the skeleton in it part of it: standing where its bars meet the floor)
    const cl = -W * 0.5, cr = -W * 0.26;
    t += obj("cell", floor + 150 * k, (bones >= 2 ? skeletonSvg(-W * 0.38, floor + 10 * k, 2.2 * k, "sit") : "") + shackle(-W * 0.38, floor - 20 * k, 2 * k) +
         [...Array(8)].map((_, j) => `<line x1="${f1(cl + (cr - cl) * j / 7)}" y1="${f1(top + 30 * k)}" x2="${f1(cl + (cr - cl) * j / 7)}" y2="${f1(floor + 150 * k)}" stroke="#2f2f38" stroke-width="${f1(6 * k)}"/>`).join("") +
         `<line x1="${f1(cl)}" y1="${f1(floor - 60 * k)}" x2="${f1(cr)}" y2="${f1(floor - 60 * k)}" stroke="#2f2f38" stroke-width="${f1(6 * k)}"/>`);
    // glowing mushrooms by the wall, a skull with a candle on it
    // (the mushrooms one object, a row at its middle depth: they stand
    // behind the candle, and so must be sorted with it, not left between)
    let shrooms = "";
    for (let i = 0; i < 7; i++) { const x = bw * (0.1 + 0.08 * i), y = floor + (8 + 6 * R(i, 940)) * k, h = (10 + 12 * R(i, 941)) * k;
      shrooms += `<rect x="${f1(x - 1.5 * k)}" y="${f1(y - h)}" width="${f1(3 * k)}" height="${f1(h)}" fill="#cfe8c0"/><ellipse cx="${f1(x)}" cy="${f1(y - h)}" rx="${f1((6 + 4 * R(i, 942)) * k)}" ry="${f1(4 * k)}" fill="#7dff9a" opacity="0.85"/>`; }
    t += obj("mushrooms", floor + 11 * k, shrooms);
    // a candle burning on the floor: on a skull, or (with none) a stone
    // (an object, standing where its shadow lies)
    const sx = W * 0.06, sy = floor + 40 * k, s2 = 1.8 * k;
    t += obj("candle", sy + 24 * s2, `<ellipse cx="${f1(sx)}" cy="${f1(sy + 24 * s2)}" rx="${f1(14 * s2)}" ry="${f1(3 * s2)}" fill="#000000" opacity="0.3"/>` +
         (bones >= 1
           ? `<path d="M${f1(sx - 10 * s2)} ${f1(sy + 12 * s2)} Q${f1(sx - 11 * s2)} ${f1(sy)} ${f1(sx)} ${f1(sy)} Q${f1(sx + 11 * s2)} ${f1(sy)} ${f1(sx + 10 * s2)} ${f1(sy + 12 * s2)} L${f1(sx + 6 * s2)} ${f1(sy + 22 * s2)} L${f1(sx - 6 * s2)} ${f1(sy + 22 * s2)} Z" fill="#efe8d6" stroke="#b9b19c" stroke-width="${f1(1.2 * s2)}"/>` +
             [-1, 1].map(e => `<ellipse cx="${f1(sx + e * 4.5 * s2)}" cy="${f1(sy + 11 * s2)}" rx="${f1(3 * s2)}" ry="${f1(3.4 * s2)}" fill="#2a2420"/>`).join("")
           : `<path d="M${f1(sx - 12 * s2)} ${f1(sy + 23 * s2)} Q${f1(sx - 13 * s2)} ${f1(sy + 4 * s2)} ${f1(sx - 2 * s2)} ${f1(sy + 1 * s2)} Q${f1(sx + 12 * s2)} ${f1(sy + 2 * s2)} ${f1(sx + 12 * s2)} ${f1(sy + 23 * s2)} Z" fill="#6b625a" stroke="#4a4038" stroke-width="${f1(1.2 * s2)}"/>`) +
         `<rect x="${f1(sx - 3 * s2)}" y="${f1(sy - 14 * s2)}" width="${f1(6 * s2)}" height="${f1(15 * s2)}" fill="#f6efd8"/>` +
         `<path d="M${f1(sx + 3 * s2)} ${f1(sy - 14 * s2)} q${f1(2 * s2)} ${f1(5 * s2)} 0 ${f1(9 * s2)}" fill="none" stroke="#f6efd8" stroke-width="${f1(2 * s2)}"/>` +
         `<circle cx="${f1(sx)}" cy="${f1(sy - 20 * s2)}" r="${f1(16 * s2)}" fill="url(#bg-glow)"/>` +
         `<ellipse cx="${f1(sx)}" cy="${f1(sy - 19 * s2)}" rx="${f1(2.6 * s2)}" ry="${f1(5 * s2)}" fill="#ffb53b"><animate attributeName="ry" values="${f1(5 * s2)};${f1(3.6 * s2)};${f1(5.4 * s2)};${f1(5 * s2)}" dur="0.9s" repeatCount="indefinite"/></ellipse>`);
  } else {
    // straw on the floor, a bucket, a rat, scattered bones
    let straw = "";   // (ticket 284: a floor object)
    for (let i = 0; i < 30; i++) { const x = -W * 0.32 + 120 * k * R(i, 950), y = floor + (40 + 50 * R(i, 951)) * k, a = (R(i, 952) - 0.5) * 1.4, l = (18 + 14 * R(i, 953)) * k;
      straw += `<line x1="${f1(x)}" y1="${f1(y)}" x2="${f1(x + Math.cos(a) * l)}" y2="${f1(y + Math.sin(a) * l * 0.4)}" stroke="${R(i, 954) < 0.5 ? "#d9b85c" : "#b8953e"}" stroke-width="${f1(2.5 * k)}" stroke-linecap="round"/>`; }
    t += floorObj("straw", straw);
    // (the bones lie flat on the floor: drawn before what stands)
    if (bones >= 1) { let bn = ""; for (let i = 0; i < 4; i++) { const x = W * (-0.1 + 0.12 * i), y = floor + (90 + 40 * R(i, 960)) * k, a = R(i, 961) * 3;
      bn += `<line x1="${f1(x)}" y1="${f1(y)}" x2="${f1(x + Math.cos(a) * 26 * k)}" y2="${f1(y + Math.sin(a) * 8 * k)}" stroke="#efe8d6" stroke-width="${f1(5 * k)}" stroke-linecap="round"/>`; }
      t += floorObj("bones", bn); }   // (ticket 284: a floor object)
    // (the bucket and the rat objects, each standing where its bottom meets the floor)
    t += obj("bucket", floor + 110 * k, `<path d="M${P(W * 0.18, floor + 60 * k)} L${P(W * 0.17, floor + 110 * k)} L${P(W * 0.25, floor + 110 * k)} L${P(W * 0.24, floor + 60 * k)} Z" fill="#6e4a2c"/>` +
         `<ellipse cx="${f1(W * 0.21)}" cy="${f1(floor + 60 * k)}" rx="${f1(W * 0.03)}" ry="${f1(6 * k)}" fill="#4a3a2a"/>` +
         [0.75, 0.95].map(u => `<line x1="${f1(W * 0.17 + 1 * k)}" y1="${f1(floor + 60 * k + 50 * k * u)}" x2="${f1(W * 0.25 - 1 * k)}" y2="${f1(floor + 60 * k + 50 * k * u)}" stroke="#3a3a44" stroke-width="${f1(3 * k)}"/>`).join("")) +
         obj("rat", floor + 140 * k, `<path d="M${P(W * 0.05, floor + 140 * k)} q${f1(14 * k)} ${f1(-14 * k)} ${f1(30 * k)} 0 q${f1(-6 * k)} ${f1(6 * k)} ${f1(-30 * k)} 0 Z" fill="#6a6a72"/><path d="M${P(W * 0.05, floor + 140 * k)} q${f1(-20 * k)} ${f1(4 * k)} ${f1(-30 * k)} ${f1(-8 * k)}" fill="none" stroke="#8a8a92" stroke-width="${f1(2 * k)}"/>` +
         `<circle cx="${f1(W * 0.05 + 26 * k)}" cy="${f1(floor + 134 * k)}" r="${f1(1.6 * k)}" fill="#111"/>`);
  }
  return t;
}
// The hallway: the room's back wall opens onto a long corridor going off
// into the dark, lit only by its torches (the night of the fairy forest
// over it all, the torches' light on top; and no fireflies: they live
// outside). Arches cross its vault, and along its walls, by turns, a
// torch and a banner in the castle's flag colors with the owner's letter.
// A carpet in the castle's carpet color runs from us down its whole length
// to a closed door at the far end, a little light under it.
function hallwaySvg(c, K, R = {}) {
  const { x0, y0, y1, W, H, k, pad, P } = c, { floor, top, bw, vy, out } = roomFrame(c);
  const tone = ROOM_TONE.hallway, mortar = mix(tone, "#000000", 0.35), gold = "#e8b923";
  // (ticket 99: nothing stands in it, so it has no objects: its torches and
  // banners are on its walls, its stairs in the far wall, its carpet flat.
  // So the friend is always drawn after all of it, its night too. Its night
  // can't be a front object as the other rooms' dark is: the lit stairwell
  // is drawn over the night, and a front stairwell would cover the friend's
  // head, where it stands at the far end behind them)
  // (ticket 284: its carpet, banners and torches are wall and floor objects, each drawn where it was)
  let t = roomShell(c, tone, {});
  // the corridor's cross-sections, going away: f is each one's size, from
  // the back wall's (1) toward the vanishing point (0, vy)
  const N = HALL_STRETCHES, F = [...Array(N + 1)].map((_, i) => 1 / (1 + i * 0.8)), fN = F[N];
  const Y = (y, f) => vy + (y - vy) * f;
  t += `<defs><radialGradient id="hall-depth" cx="0" cy="${f1(vy)}" r="${f1(bw)}" gradientUnits="userSpaceOnUse">` +
       `<stop offset="0" stop-color="#000000" stop-opacity="0.75"/><stop offset="0.55" stop-color="#000000" stop-opacity="0.35"/><stop offset="1" stop-color="#000000" stop-opacity="0"/></radialGradient>` +
       `<linearGradient id="hall-stairlight" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#ffcf6b" stop-opacity="0.55"/><stop offset="1" stop-color="#ffcf6b" stop-opacity="0"/></linearGradient></defs>`;
  // its walls, floor and vault, from the back wall's opening to the far end
  for (const sd of [-1, 1]) {
    t += `<polygon points="${P(sd * bw, top)} ${P(sd * bw * fN, Y(top, fN))} ${P(sd * bw * fN, Y(floor, fN))} ${P(sd * bw, floor)}" fill="${mix(tone, "#000000", 0.18)}"/>`;
    for (let j = 1; j < 12; j++) { const y = top + (floor - top) * j / 12;
      t += `<line x1="${f1(sd * bw)}" y1="${f1(y)}" x2="${f1(sd * bw * fN)}" y2="${f1(Y(y, fN))}" stroke="${mortar}" stroke-width="${f1(1.6 * k)}" opacity="0.55"/>`; }
  }
  t += `<polygon points="${P(-bw, floor)} ${P(bw, floor)} ${P(bw * fN, Y(floor, fN))} ${P(-bw * fN, Y(floor, fN))}" fill="${mix(tone, "#5a4a3a", 0.4)}"/>` +
       `<polygon points="${P(-bw, top)} ${P(bw, top)} ${P(bw * fN, Y(top, fN))} ${P(-bw * fN, Y(top, fN))}" fill="${mix(tone, "#000000", 0.45)}"/>`;
  for (let i = -6; i <= 6; i++) t += `<line x1="${f1(i * bw / 6)}" y1="${f1(floor)}" x2="${f1(i * bw / 6 * fN)}" y2="${f1(Y(floor, fN))}" stroke="${mix(tone, "#000000", 0.4)}" stroke-width="${f1(1.2 * k)}" opacity="0.5"/>`;
  // at each cross-section, an arch over the vault and its pillars down the walls, and a seam in the floor
  for (let i = 1; i <= N; i++) { const f = F[i], w = bw * f, tp = Y(top, f), fl = Y(floor, f);
    t += `<path d="M${P(-w, fl)} V${f1(tp)} Q${P(0, tp - 60 * k * f)} ${P(w, tp)} V${f1(fl)}" fill="none" stroke="${mix(tone, "#000000", 0.3)}" stroke-width="${f1(Math.max(1, 10 * k * f))}"/>` +
         `<line x1="${f1(-w)}" y1="${f1(fl)}" x2="${f1(w)}" y2="${f1(fl)}" stroke="${mortar}" stroke-width="${f1(Math.max(0.6, 2 * k * f))}"/>`; }
  // the far end: its wall, and in it an archway onto the foot of a spiral
  // staircase, its steps winding up round their stone column and out of
  // sight, warm light coming down them (where it leads: castleWays)
  // (the stairwell is lit, so it is drawn over the hallway's dark, at the end)
  // When the stairs lead nowhere, there are none: the corridor ends in a
  // stone wall, its courses meeting the side walls' at the corners, and a
  // torch on it.
  const deadEnd = R.hallEnd === "none";
  let stairwell = "";
  // (lit by its torch, so, as the stairwell, it is drawn over the hallway's dark: in the night's
  // blue, warm round the torch)
  if (deadEnd) { const f = fN, w = bw * f, tp = Y(top, f), fl = Y(floor, f), kf = k * f, bwid = w / 3, ty = Y(floor - 220 * k, f);
    const stone = mix(mix(tone, "#000000", 0.3), "#0b1030", 0.6), seam = mix(stone, "#000000", 0.4);
    stairwell += `<defs><radialGradient id="hall-endlight" cx="0" cy="${f1(ty - 24 * kf)}" r="${f1(w * 1.1)}" gradientUnits="userSpaceOnUse">` +
         `<stop offset="0" stop-color="#ffcf6b" stop-opacity="0.4"/><stop offset="1" stop-color="#ffcf6b" stop-opacity="0"/></radialGradient></defs>` +
         `<rect x="${f1(-w)}" y="${f1(tp)}" width="${f1(2 * w)}" height="${f1(fl - tp)}" fill="${stone}"/>`;
    for (let j = 0; j < 12; j++) {
      const ya = Y(top + (floor - top) * j / 12, f), yb = Y(top + (floor - top) * (j + 1) / 12, f);
      if (j > 0) stairwell += `<line x1="${f1(-w)}" y1="${f1(ya)}" x2="${f1(w)}" y2="${f1(ya)}" stroke="${seam}" stroke-width="${f1(1.6 * kf)}"/>`;
      for (let x = -w + (j % 2 ? bwid / 2 : bwid); x < w - 1e-6; x += bwid)
        stairwell += `<line x1="${f1(x)}" y1="${f1(ya)}" x2="${f1(x)}" y2="${f1(yb)}" stroke="${seam}" stroke-width="${f1(1.6 * kf)}"/>`;
    }
    stairwell += `<rect x="${f1(-w)}" y="${f1(tp)}" width="${f1(2 * w)}" height="${f1(fl - tp)}" fill="url(#hall-endlight)"/>`;
  } else { const f = fN, w = bw * f, tp = Y(top, f), fl = Y(floor, f), A = hallFarArch(c), kf = k * f;
    t += `<rect x="${f1(-w)}" y="${f1(tp)}" width="${f1(2 * w)}" height="${f1(fl - tp)}" fill="${mix(tone, "#000000", 0.28)}"/>`;
    stairwell += `<path d="${A.d}" fill="#1a1410" stroke="${mix(tone, "#ffffff", 0.08)}" stroke-width="${f1(Math.max(1, 8 * kf))}"/>` +
         `<clipPath id="hall-arch"><path d="${A.d}"/></clipPath><g clip-path="url(#hall-arch)">` +
         `<rect x="${f1(A.x0)}" y="${f1(A.top)}" width="${f1(A.x1 - A.x0)}" height="${f1(A.fl - A.top)}" fill="#5a4330"/>`;
    // the steps: wedges climbing from the left round to the right, each higher and narrower, behind the column at last
    for (let j = 0; j < 9; j++) {
      const u = j / 8, y = A.fl - j * (A.fl - A.top) * 0.11, sh = (A.fl - A.top) * 0.11, xl = A.x0 + (A.x1 - A.x0) * (0.12 + 0.05 * j), xr = A.x1 - (A.x1 - A.x0) * 0.02;
      const lit = mix("#b8aca0", "#ffe2a8", 0.25 + 0.55 * u);
      stairwell += `<polygon points="${P(xl, y - sh)} ${P(xr, y - sh * (1.4 - 0.5 * u))} ${P(xr, y)} ${P(xl, y)}" fill="${mix(lit, "#000000", 0.35)}"/>` +
           `<polygon points="${P(xl, y - sh)} ${P(xl + (A.x1 - A.x0) * 0.04, y - sh * 1.35)} ${P(xr, y - sh * (1.75 - 0.5 * u))} ${P(xr, y - sh * (1.4 - 0.5 * u))}" fill="${lit}"/>`;
    }
    // the central column, and the light from above
    stairwell += `<rect x="${f1(A.x0 + (A.x1 - A.x0) * 0.04)}" y="${f1(A.top)}" width="${f1((A.x1 - A.x0) * 0.16)}" height="${f1(A.fl - A.top)}" fill="${mix(tone, "#000000", 0.15)}"/>` +
         `<rect x="${f1(A.x0)}" y="${f1(A.top)}" width="${f1(A.x1 - A.x0)}" height="${f1((A.fl - A.top) * 0.6)}" fill="url(#hall-stairlight)"/></g>`; }
  // the carpet, from us, through the room, all the way down: its color the castle's, gold at its edges, a darker band inside them
  const cw = 72 * k, edge = (u, side) => { const x = side * cw * u, [nx] = out(x, floor, y1 + pad); return [[nx, y1 + pad], [x, floor], [x * fN, Y(floor, fN)]]; };
  const strip = u => { const L = edge(u, -1), Rr = edge(u, 1); return [...L, ...Rr.reverse()].map(([x, y]) => P(x, y)).join(" "); };
  const mCarpet = t.length;
  t += `<polygon points="${strip(1)}" fill="${K.carpet}"/>` +
       `<polygon points="${strip(0.8)}" fill="none" stroke="${mix(K.carpet, "#000000", 0.35)}" stroke-width="${f1(5 * k)}"/>` +
       [-1, 1].map(sd => `<polyline points="${edge(1, sd).map(([x, y]) => P(x, y)).join(" ")}" fill="none" stroke="${gold}" stroke-width="${f1(4 * k)}"/>`).join("");
  t = floorSince(t, mCarpet, "carpet");
  // along the walls, by turns: a torch on one side and a banner on the other, in each stretch of the corridor
  const lights = [];
  for (let i = 0; i < N - 1; i++) {
    const f = 1 / (1 + HALL_LIGHT_Z[i]), w = bw * f, kk = k * f, sd = i % 2 ? 1 : -1;   // (between the pairs of doors: hallDoor)
    lights.push(wallObj(`torch ${i + 1}`, torchSvg(sd * (w - 18 * kk), Y(floor - 220 * k, f), kk, i, c.o)));
    // (the banner hangs from a pole jutting straight out of the wall across the corridor, so it
    // faces down the corridor, as drawn; the pole and its brace go back into the wall)
    const bx = -sd * (w - 34 * kk), by = Y(top + 40 * k, f), bwid = 50 * kk, bh = 180 * kk, u = 10 * kk, xw = -sd * w, xe = bx + sd * (bwid / 2 + 6 * kk);
    const mBanner = t.length;
    t += castleFlagDefs(K, u) +
         `<path d="M${P(xw, by + 26 * kk)} L${P(xw + sd * 14 * kk, by)}" stroke="#4a3220" stroke-width="${f1(Math.max(0.6, 3 * kk))}"/>` +
         `<rect x="${f1(Math.min(xw, xe))}" y="${f1(by - 5 * kk)}" width="${f1(Math.abs(xe - xw))}" height="${f1(6 * kk)}" rx="${f1(2 * kk)}" fill="#5e3d22"/>` +
         `<circle cx="${f1(xe)}" cy="${f1(by - 2 * kk)}" r="${f1(4 * kk)}" fill="${gold}"/>` +
         `<path d="M${P(bx - bwid / 2, by)} H${f1(bx + bwid / 2)} V${f1(by + bh)} L${P(bx, by + bh - 24 * kk)} L${P(bx - bwid / 2, by + bh)} Z" fill="${castleFlagFill(K, i, u)}" stroke="${gold}" stroke-width="${f1(Math.max(0.6, 3 * kk))}"/>` +
         castleLetter(K, i, bx, by + 100 * kk, 34 * kk, "serif");
    t = wallSince(t, mBanner, `banner ${i + 1}`);
  }
  // the stairwell's light, at the far end; or, at a dead end, a torch on its wall
  if (deadEnd) lights.push(wallObj("torch at the far end", torchSvg(0, Y(floor - 220 * k, fN), k * fN * 1.4, 9, c.o)));
  else { const A = hallFarArch(c); lights.push(`<circle cx="0" cy="${f1(A.top + (A.fl - A.top) * 0.15)}" r="${f1((A.x1 - A.x0) * 0.75)}" fill="url(#bg-glow)" opacity="0.6"/>`); }
  // and a torch either side where the corridor opens, near us
  for (const sd of [-1, 1]) lights.push(wallObj(sd < 0 ? "torch, left" : "torch, right", torchSvg(sd * bw * 1.12, floor - 250 * k, k * 1.1, 7 + (sd > 0 ? 1 : 0), c.o)));
  // the dark: deeper toward the far end, the night over everything, then the torches' light and flames on top
  t += `<rect x="${f1(-bw)}" y="${f1(top)}" width="${f1(2 * bw)}" height="${f1(floor - top)}" fill="url(#hall-depth)"/>` +
       `<rect x="${f1(x0 - pad)}" y="${f1(y0 - pad)}" width="${f1(W + 2 * pad)}" height="${f1(H + 2 * pad)}" fill="#0b1030" opacity="0.5"/>`;
  return t + stairwell + lights.join("");
}
// The dining room (M's idea): a long hall, drawn as the hallway is, down its
// length and from the same height, its floor of boards. Down its middle, a
// long table set for as many as "🍽️ places at table" says, 4 to 18: one at
// each end, and the rest down its two sides (eight to a side at most). A white
// cloth with a runner in the room's carpet color; at each place a chair with
// a cushion, a plate, a cup, and a knife and fork; candles burning down the
// table; candelabras of candles hanging over it on chains from the vault;
// torches along the walls; and at the far end, a great fireplace, its fire
// burning. The candle flames flicker with the quick LFO, and the torches and
// the fire glow with the slow one.
// (ticket 50) The dining room's table and chairs, where they stand, for its
// drawing (diningRoomSvg) and its seats (SEATS.castleroom) alike, so the two
// can't drift: a point in the room (x across, y up and down, z how far down
// the room, in stretches) is at(x, y, z); its places (4 to 18), one at each
// end and the rest down its sides, the left side taking the odd one; the
// table from zA (our end) to zB; the end chairs at zNear and zFar, a side's
// chair j (from our end) at zOf(sd, j); a side chair's seat from seatX(sd)[0],
// by the table, out to seatX(sd)[1], an end chair's across ±endX, at ys
function diningPlaces(c, places) {
  const { k } = c, { floor, vy } = roomFrame(c);
  const fz = z => 1 / (1 + z * 0.8), Y = (y, f) => vy + (y - vy) * f, at = (x, y, z) => { const f = fz(z); return [x * f, Y(y, f)]; };
  places = Math.max(4, Math.min(18, Math.round(places ?? 10)));
  const sides = [Math.ceil((places - 2) / 2), Math.floor((places - 2) / 2)];
  const per = Math.max(1, sides[0]), step = 0.34, zA = 0.3, zB = zA + per * step, tw = 64 * k, ytop = floor - 80 * k, ys = floor - 46 * k;
  const zOf = (sd, j) => zA + (j + 0.5 + (per - sides[sd < 0 ? 0 : 1]) / 2) * step;
  return { fz, Y, at, places, sides, per, step, zA, zB, zNear: zA - 0.2, zFar: zB + 0.2, tw, ytop, ys, zOf,
           seatX: sd => [sd * (tw - 6 * k), sd * (tw + 34 * k)], endX: 24 * k, floorAt: z => at(0, floor, z)[1] };
}
function diningRoomSvg(c, K, r = {}) {
  const { k, P, o } = c, { floor, top, bw, vy } = roomFrame(c);
  const tone = ROOM_TONE.dining, mortar = mix(tone, "#000000", 0.35), gold = "#e8b923", wood = "#7a4e2c", woodDk = "#4a2e18";
  // (a point in the room, the table and the chairs: diningPlaces)
  const D = diningPlaces(c, r.places), { fz, Y, at } = D, N = HALL_STRETCHES, fN = fz(N), pt = (x, y, z) => P(...at(x, y, z));
  const poly = (pts, fill, more = "") => `<polygon points="${pts.map(p => pt(...p)).join(" ")}" fill="${fill}"${more}/>`;
  let t = roomShell(c, tone, {});
  t += `<defs><radialGradient id="dine-depth" cx="0" cy="${f1(vy)}" r="${f1(bw)}" gradientUnits="userSpaceOnUse">` +
       `<stop offset="0" stop-color="#000000" stop-opacity="0.45"/><stop offset="0.6" stop-color="#000000" stop-opacity="0.18"/><stop offset="1" stop-color="#000000" stop-opacity="0"/></radialGradient></defs>`;
  // its walls, floor and vault, from the back wall's opening to the far end (as the hallway's), the floor of boards
  for (const sd of [-1, 1]) {
    t += poly([[sd * bw, top, 0], [sd * bw, top, N], [sd * bw, floor, N], [sd * bw, floor, 0]], mix(tone, "#000000", 0.18));
    for (let j = 1; j < 12; j++) { const y = top + (floor - top) * j / 12;
      t += `<line x1="${f1(sd * bw)}" y1="${f1(y)}" x2="${f1(sd * bw * fN)}" y2="${f1(Y(y, fN))}" stroke="${mortar}" stroke-width="${f1(1.6 * k)}" opacity="0.55"/>`; }
  }
  t += poly([[-bw, floor, 0], [bw, floor, 0], [bw, floor, N], [-bw, floor, N]], "#7a5434") + poly([[-bw, top, 0], [bw, top, 0], [bw, top, N], [-bw, top, N]], mix(tone, "#000000", 0.45));
  for (let i = -9; i <= 9; i++) t += `<line x1="${f1(i * bw / 9)}" y1="${f1(floor)}" x2="${f1(i * bw / 9 * fN)}" y2="${f1(Y(floor, fN))}" stroke="#5a3a20" stroke-width="${f1(1.2 * k)}" opacity="0.6"/>`;
  for (let i = 1; i <= N; i++) { const f = fz(i), w = bw * f, tp = Y(top, f), fl = Y(floor, f);
    t += `<path d="M${P(-w, fl)} V${f1(tp)} Q${P(0, tp - 60 * k * f)} ${P(w, tp)} V${f1(fl)}" fill="none" stroke="${mix(tone, "#000000", 0.3)}" stroke-width="${f1(Math.max(1, 10 * k * f))}"/>` +
         `<line x1="${f1(-w)}" y1="${f1(fl)}" x2="${f1(w)}" y2="${f1(fl)}" stroke="#4a2e18" stroke-width="${f1(Math.max(0.6, 2 * k * f))}"/>`; }
  // the far end: its wall, its courses meeting the side walls', and a great fireplace in it
  // (ticket 99: its fire is drawn with it, in the far wall, under all that
  // stands and under the dark; its glow is the room's "fire light", a front
  // object, as are the torches on the walls and the dark itself; a candle's
  // flame and glow go with what holds it, the table or a candelabra)
  let fireLight = "";
  { const f = fN, w = bw * f, tp = Y(top, f), fl = Y(floor, f), kf = k * f, fw = w * 0.42, fh = (fl - tp) * 0.46, mt = fl - fh;
    t += `<rect x="${f1(-w)}" y="${f1(tp)}" width="${f1(2 * w)}" height="${f1(fl - tp)}" fill="${mix(tone, "#000000", 0.22)}"/>`;
    for (let j = 1; j < 12; j++) { const y = Y(top + (floor - top) * j / 12, f);
      t += `<line x1="${f1(-w)}" y1="${f1(y)}" x2="${f1(w)}" y2="${f1(y)}" stroke="${mortar}" stroke-width="${f1(1.6 * kf)}" opacity="0.6"/>`; }
    const mFire = t.length;
    t += `<rect x="${f1(-fw - 14 * kf)}" y="${f1(mt - 4 * kf)}" width="${f1(2 * fw + 28 * kf)}" height="${f1(fl - mt + 4 * kf)}" fill="${mix(tone, "#ffffff", 0.12)}"/>` +
         `<path d="M${P(-fw, fl)} V${f1(mt + fh * 0.35)} Q${P(0, mt + fh * 0.02)} ${P(fw, mt + fh * 0.35)} V${f1(fl)} Z" fill="#1a1008"/>` +
         `<rect x="${f1(-fw - 22 * kf)}" y="${f1(mt - 16 * kf)}" width="${f1(2 * fw + 44 * kf)}" height="${f1(12 * kf)}" fill="${mix(tone, "#ffffff", 0.22)}"/>` +
         `<rect x="${f1(-fw * 0.6)}" y="${f1(fl - 8 * kf)}" width="${f1(fw * 1.2)}" height="${f1(7 * kf)}" rx="${f1(3 * kf)}" fill="${woodDk}"/>`;
    // (the fire: three flames, the middle one tallest, each flickering about its own base)
    [-0.38, 0, 0.38].forEach((u, j) => { const h = (j === 1 ? 0.62 : 0.45) * fh, bx = u * fw;
      t += `<g transform="translate(${f1(bx)} ${f1(fl - 6 * kf)})"><path d="M${f1(-fw * 0.22)} 0 Q${f1(-fw * 0.2)} ${f1(-h * 0.6)} 0 ${f1(-h)} Q${f1(fw * 0.2)} ${f1(-h * 0.6)} ${f1(fw * 0.22)} 0 Z" fill="${j === 1 ? "#ffd05a" : "#ff8a2e"}">` +
        sceneLfo(o, "fast", (j * 0.31) % 1, "transform", (v, amp) => `1 ${(1 + (0.08 + 0.18 * amp) * v).toFixed(3)}`,
          `<animateTransform attributeName="transform" type="scale" values="1 1;1 1.12;1 0.92;1 1" dur="${(0.7 + 0.13 * j).toFixed(2)}s" repeatCount="indefinite"/>`, "scale") + `</path></g>`; });
    t = wallSince(t, mFire, "fireplace");   // (ticket 284: a wall object; its glow is still the room's "fire light")
    fireLight = `<circle cx="0" cy="${f1(fl - fh * 0.4)}" r="${f1(fw * 2.2)}" fill="url(#bg-glow)">${sceneLfo(o, "slow", 0.5, "opacity", (v, amp) => (0.85 - (0.15 + 0.3 * amp) * (1 + v) / 2).toFixed(2), "")}</circle>`;
  }
  // the table, down the middle: one place at each end, the rest down its sides (the left side
  // has the odd one), its places evenly spaced
  const { sides, per, zA, zB, tw, ytop, ys, zOf } = D;
  // (a chair: its seat with a cushion in the carpet color, its back on the far side from the table;
  // at an end, its back across the room)
  const chair = (cx, z, sd, end) => {
    // (our end's chair, its back to us, is low, so the table shows over it)
    const d = end ? 0.08 : 0.07, za = z - d, zb = z + d, hi = end > 0 ? floor - 170 * k : end < 0 ? floor - 96 * k : floor - 132 * k;
    if (end) { const x = D.endX, zBack = end < 0 ? za : zb;
      return poly([[-x, ys, za], [x, ys, za], [x, ys, zb], [-x, ys, zb]], wood) + poly([[-x * 0.8, ys - 3 * k, za + 0.01], [x * 0.8, ys - 3 * k, za + 0.01], [x * 0.8, ys - 3 * k, zb - 0.01], [-x * 0.8, ys - 3 * k, zb - 0.01]], K.carpet) +
             [[-x, za], [x, za], [-x, zb], [x, zb]].map(([lx, lz]) => `<line x1="${f1(at(lx, ys, lz)[0])}" y1="${f1(at(lx, ys, lz)[1])}" x2="${f1(at(lx, floor, lz)[0])}" y2="${f1(at(lx, floor, lz)[1])}" stroke="${woodDk}" stroke-width="${f1(3 * k * fz(lz))}"/>`).join("") +
             poly([[-x, ys, zBack], [x, ys, zBack], [x, hi, zBack], [-x, hi, zBack]], woodDk, ` stroke="${wood}" stroke-width="${f1(2 * k * fz(zBack))}"`);
    }
    const [xi, xo] = D.seatX(sd);
    return [[xi, za], [xo, za], [xi, zb], [xo, zb]].map(([lx, lz]) => `<line x1="${f1(at(lx, ys, lz)[0])}" y1="${f1(at(lx, ys, lz)[1])}" x2="${f1(at(lx, floor, lz)[0])}" y2="${f1(at(lx, floor, lz)[1])}" stroke="${woodDk}" stroke-width="${f1(3 * k * fz(lz))}"/>`).join("") +
           poly([[xi, ys, za], [xo, ys, za], [xo, ys, zb], [xi, ys, zb]], wood) + poly([[xi, ys - 3 * k, za + 0.01], [xo - sd * 4 * k, ys - 3 * k, za + 0.01], [xo - sd * 4 * k, ys - 3 * k, zb - 0.01], [xi, ys - 3 * k, zb - 0.01]], K.carpet) +
           poly([[xo, ys, za], [xo, ys, zb], [xo, hi, zb], [xo, hi, za]], woodDk, ` stroke="${wood}" stroke-width="${f1(2 * k * fz(z))}"`);
  };
  // far to near: the far end's chair, the chairs down the sides, the legs, the top, what is on it, and the near end's chair
  // (ticket 99: each chair an object, standing at its middle on the floor, a
  // side's numbered from our end; the table one object, its legs, top and all
  // on it, standing at its NEAR end, zA, as it is long, running away from us)
  const { floorAt } = D;
  t += obj("chair, far end", floorAt(D.zFar), chair(0, D.zFar, 0, 1));
  for (const sd of [-1, 1]) for (let j = sides[sd < 0 ? 0 : 1] - 1; j >= 0; j--) t += obj(`chair, ${sd < 0 ? "left" : "right"} ${j + 1}`, floorAt(zOf(sd, j)), chair(0, zOf(sd, j), sd, 0));
  let table = "";
  for (const [z, sd] of [[zB - 0.06, -1], [zB - 0.06, 1], [zA + 0.06, -1], [zA + 0.06, 1]]) {
    const [x, y] = at(sd * (tw - 8 * k), ytop, z), f = fz(z);
    table += `<rect x="${f1(x - 5 * k * f)}" y="${f1(y)}" width="${f1(10 * k * f)}" height="${f1(Y(floor, f) - y)}" fill="${woodDk}"/>`;
  }
  // (the cloth over the top and hanging down at our end, the runner along it, gold at its edges)
  const rw = tw * 0.34;
  table += poly([[-tw, ytop, zA], [tw, ytop, zA], [tw, ytop, zB], [-tw, ytop, zB]], "#f3ead6") +
       poly([[-tw, ytop, zA], [tw, ytop, zA], [tw, ytop + 24 * k, zA], [-tw, ytop + 24 * k, zA]], "#e4d8bd") +
       poly([[-rw, ytop, zA], [rw, ytop, zA], [rw, ytop, zB], [-rw, ytop, zB]], K.carpet) +
       poly([[-rw, ytop, zA], [rw, ytop, zA], [rw, ytop + 34 * k, zA], [0, ytop + 42 * k, zA], [-rw, ytop + 34 * k, zA]], mix(K.carpet, "#000000", 0.12)) +
       [-1, 1].map(sd => `<line x1="${f1(at(sd * rw, ytop, zA)[0])}" y1="${f1(at(sd * rw, ytop, zA)[1])}" x2="${f1(at(sd * rw, ytop, zB)[0])}" y2="${f1(at(sd * rw, ytop, zB)[1])}" stroke="${gold}" stroke-width="${f1(1.6 * k)}"/>`).join("");
  // (a place: a plate with its rim, a gold cup beyond it, a knife and fork either side; on the table's top,
  // so flattened as it is)
  const setting = (x, z) => { const [px, py] = at(x, ytop, z), f = fz(z), ry = Math.abs(at(x, ytop, z - 0.035)[1] - at(x, ytop, z + 0.035)[1]) / 2, rx = 15 * k * f;
    const [cx, cy] = at(x * 0.82, ytop, z + 0.05);
    return `<ellipse cx="${f1(px)}" cy="${f1(py)}" rx="${f1(rx)}" ry="${f1(ry)}" fill="#ffffff" stroke="#c9bfa8" stroke-width="${f1(1.2 * k * f)}"/>` +
           `<ellipse cx="${f1(px)}" cy="${f1(py)}" rx="${f1(rx * 0.6)}" ry="${f1(ry * 0.6)}" fill="none" stroke="#d8cfb9" stroke-width="${f1(0.8 * k * f)}"/>` +
           `<line x1="${f1(px - rx * 1.35)}" y1="${f1(py - ry)}" x2="${f1(px - rx * 1.35)}" y2="${f1(py + ry)}" stroke="#b8b8c4" stroke-width="${f1(1.4 * k * f)}"/>` +
           `<line x1="${f1(px + rx * 1.35)}" y1="${f1(py - ry)}" x2="${f1(px + rx * 1.35)}" y2="${f1(py + ry)}" stroke="#b8b8c4" stroke-width="${f1(1.4 * k * f)}"/>` +
           `<path d="M${f1(cx - 4 * k * f)} ${f1(cy - 14 * k * f)} h${f1(8 * k * f)} l${f1(-2 * k * f)} ${f1(8 * k * f)} h${f1(-4 * k * f)} Z M${f1(cx - 1 * k * f)} ${f1(cy - 6 * k * f)} v${f1(5 * k * f)} M${f1(cx - 3 * k * f)} ${f1(cy)} h${f1(6 * k * f)}" fill="${gold}" stroke="${gold}" stroke-width="${f1(1.2 * k * f)}"/>`; };
  // (far to near, so nearer things sit over farther ones)
  const onTable = [[0, zB - 0.07], ...[-1, 1].flatMap(sd => [...Array(sides[sd < 0 ? 0 : 1])].map((_, j) => [sd * (tw - 20 * k), zOf(sd, j)])), [0, zA + 0.07]];
  // the candles down the runner, between the places, each in a brass stick, flickering
  const nc = Math.max(2, Math.ceil(per / 2) + 1), candles = [...Array(nc)].map((_, m) => zA + (zB - zA) * (m + 0.5) / nc);
  // (its flame and its glow go into the lists it is given: those of what holds it)
  const candle = (x, y, f, j, tall, flames, lights) => { const kk = k * f;
    flames.push(`<g transform="translate(${f1(x)} ${f1(y - tall * kk)})"><ellipse cx="0" cy="${f1(-5 * kk)}" rx="${f1(3 * kk)}" ry="${f1(6 * kk)}" fill="#ffb53b">` +
      sceneLfo(o, "fast", (j * 0.37) % 1, "ry", (v, amp) => f1(6 * kk * (1 + (0.12 + 0.25 * amp) * v)), `<animate attributeName="ry" values="${f1(6 * kk)};${f1(4.6 * kk)};${f1(6.6 * kk)};${f1(6 * kk)}" dur="${(0.7 + 0.11 * (j % 5)).toFixed(2)}s" repeatCount="indefinite"/>`) +
      `</ellipse><ellipse cx="0" cy="${f1(-3 * kk)}" rx="${f1(1.4 * kk)}" ry="${f1(3 * kk)}" fill="#fff4c2"/></g>`);
    lights.push(`<circle cx="${f1(x)}" cy="${f1(y - (tall + 6) * kk)}" r="${f1(26 * kk)}" fill="url(#bg-glow)" opacity="0.55"/>`);
    return `<rect x="${f1(x - 3 * kk)}" y="${f1(y - tall * kk)}" width="${f1(6 * kk)}" height="${f1(tall * kk)}" fill="#fbf3dc"/>`;
  };
  const tFlames = [], tLights = [];
  const things = [...onTable.map(([x, z]) => ({ z, s: setting(x, z) })), ...candles.map((z, m) => ({ z, s: (() => { const [x, y] = at(0, ytop, z), f = fz(z);
    return `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(8 * k * f)}" ry="${f1(2.6 * k * f)}" fill="#b8892c"/><rect x="${f1(x - 2 * k * f)}" y="${f1(y - 10 * k * f)}" width="${f1(4 * k * f)}" height="${f1(10 * k * f)}" fill="#c99a38"/>` +
           candle(x, y - 10 * k * f, f, m, 26, tFlames, tLights); })() }))];
  table += things.sort((a, b) => b.z - a.z).map(x => x.s).join("") + tLights.join("") + tFlames.join("");
  t += obj("table", floorAt(zA), table);
  t += obj("chair, near end", floorAt(D.zNear), chair(0, D.zNear, 0, -1));
  // the candelabras, hanging over the table on chains from the vault: a gold ring of candles each
  // (each an object, at the floor under it, numbered from our end)
  const nh = per <= 3 ? 1 : per <= 6 ? 2 : 3;
  for (let m = nh - 1; m >= 0; m--) {
    // (hung well above our eyes, so the nearer ones show higher, each clear of the next)
    const z = zA + (zB - zA) * (m + 0.5) / nh, f = fz(z), kk = k * f, [cx, cy] = at(0, top + 34 * k, z), ctop = Y(top - 30 * k, f), rx = 46 * kk, ry = 9 * kk;
    const cFlames = [], cLights = [];
    t += obj(`candelabra ${m + 1}`, floorAt(z), chainSvg(cx, ctop, cy - ctop, 2 * kk) + `<ellipse cx="${f1(cx)}" cy="${f1(cy)}" rx="${f1(rx)}" ry="${f1(ry)}" fill="none" stroke="${gold}" stroke-width="${f1(4 * kk)}"/>` +
         [...Array(6)].map((_, j) => { const a = Math.PI * 2 * j / 6 + 0.3, x = cx + Math.cos(a) * rx, y = cy + Math.sin(a) * ry; return { y, s: candle(x, y, f, 10 + m * 6 + j, 18, cFlames, cLights) }; })
           .sort((a, b) => a.y - b.y).map(x => x.s).join("") + cLights.join("") + cFlames.join(""));
  }
  // along the walls, torches, each side, in each stretch of the room (clear of the doors: as the hallway's)
  const torches = [];
  for (let i = 0; i < N - 1; i++) for (const sd of [-1, 1]) {
    const f = 1 / (1 + HALL_LIGHT_Z[i]), w = bw * f, kk = k * f;
    torches.push(torchSvg(sd * (w - 18 * kk), Y(floor - 220 * k, f), kk, i * 2 + (sd > 0 ? 1 : 0), o));
  }
  for (const sd of [-1, 1]) torches.push(torchSvg(sd * bw * 1.12, floor - 250 * k, k * 1.1, 7 + (sd > 0 ? 1 : 0), o));
  // the dark, gentler than the hallway's (the candles light it), then the fire's light and the torches on top
  // (each a front object: over the friend wherever they stand)
  t += obj("dark", Infinity, `<rect x="${f1(-bw)}" y="${f1(top)}" width="${f1(2 * bw)}" height="${f1(floor - top)}" fill="url(#dine-depth)"/>` +
       `<rect x="${f1(c.x0 - c.pad)}" y="${f1(c.y0 - c.pad)}" width="${f1(c.W + 2 * c.pad)}" height="${f1(c.H + 2 * c.pad)}" fill="#1a1030" opacity="0.28"/>`);
  return t + obj("fire light", Infinity, fireLight) + obj("torches", Infinity, torches.join(""));
}
// A castle helper: one of the castle's staff (the kitchen's two cooks, the
// smith), drawn as the healer is, in their own units, feet at (0, 0), about
// 240 tall, from `h`: skin, hair, shirt, legs (or a skirt), apron, a hat
// ("toque", "cap") or a beard, and what their hands do ("stir": a ladle
// into a pot whose middle is at (-110, -48); "chop": a knife at a table at
// their hands' height, -104; "hammer": a hammer on an anvil, striking at
// (-96, -86), tongs holding the work there). Their work keeps time with the
// scene's quick LFO (o), or their own without one. (They come into ticket
// 92's cast later, with settings of their own.)
function helperSvg(h, o, off = 0) {
  const skin = h.skin, sk = darker(skin), hair = h.hair, shirt = h.shirt, ap = h.apron;
  const limb = (pts, w, c) => `<path d="M${pts.map(p => p.join(" ")).join(" L")}" fill="none" stroke="${darker(c)}" stroke-width="${w + 3}" stroke-linecap="round" stroke-linejoin="round"/>` +
                              `<path d="M${pts.map(p => p.join(" ")).join(" L")}" fill="none" stroke="${c}" stroke-width="${w}" stroke-linecap="round" stroke-linejoin="round"/>`;
  const sleeve = h.sleeves === "rolled" ? skin : shirt;
  let t = `<ellipse cx="-16" cy="-4" rx="13" ry="6" fill="#3e2a18"/><ellipse cx="16" cy="-4" rx="13" ry="6" fill="#3e2a18"/>`;
  t += h.skirt ? `<path d="M-26 -96 Q-34 -60 -46 -10 Q0 -2 46 -10 Q34 -60 26 -96 Z" fill="${h.skirt}" stroke="${darker(h.skirt)}" stroke-width="2.5"/>`
               : `<path d="M-25 -92 L-22 -8 L-5 -8 L-1 -58 L1 -58 L5 -8 L22 -8 L25 -92 Z" fill="${h.legs}" stroke="${darker(h.legs)}" stroke-width="2.5" stroke-linejoin="round"/>`;
  t += `<path d="M-28 -150 Q-35 -122 -30 -88 L30 -88 Q35 -122 28 -150 Q0 -158 -28 -150 Z" fill="${shirt}" stroke="${darker(shirt)}" stroke-width="2.5" stroke-linejoin="round"/>`;
  // (the apron: a cook's to the knee, a smith's of leather to the shin)
  t += `<path d="M-19 -134 L19 -134 L${h.long ? 30 : 26} ${h.long ? -24 : -44} L${h.long ? -30 : -26} ${h.long ? -24 : -44} Z" fill="${ap}" stroke="${darker(ap)}" stroke-width="2"/>` +
       `<rect x="-29" y="-100" width="58" height="6" rx="3" fill="${ap}" stroke="${darker(ap)}" stroke-width="1.5"/>` +
       `<path d="M-19 -134 L-24 -150 M19 -134 L24 -150" stroke="${ap}" stroke-width="4"/>`;
  if (h.neckerchief) t += `<path d="M-12 -152 L12 -152 L0 -136 Z" fill="${h.neckerchief}" stroke="${darker(h.neckerchief)}" stroke-width="1.5"/>`;
  // the arms: the left (the picture's right) still; the right (the picture's left) at its work
  let rest = "", work = "";
  if (h.pose === "stir") {
    rest = limb([[24, -146], [38, -116]], 12, sleeve) + limb([[38, -116], [20, -99]], 9, skin) + `<circle cx="19" cy="-98" r="7" fill="${skin}" stroke="${sk}" stroke-width="1.5"/>`;
    work = lfoTimed(o, "fast", off, `<animateTransform attributeName="transform" type="rotate" values="0 -24 -146;-6 -24 -146;0 -24 -146;5 -24 -146;0 -24 -146" dur="2.4s" repeatCount="indefinite"/>`) +
      `<line x1="-58" y1="-118" x2="-110" y2="-48" stroke="#5b3a1f" stroke-width="6" stroke-linecap="round"/>` + `<ellipse cx="-110" cy="-46" rx="9" ry="5" fill="#5b3a1f"/>` +
      limb([[-24, -146], [-44, -118]], 12, sleeve) + limb([[-44, -118], [-64, -106]], 9, skin) + `<circle cx="-65" cy="-105" r="7.5" fill="${skin}" stroke="${sk}" stroke-width="1.5"/>`;
  } else if (h.pose === "chop") {
    // (a carrot held flat on the board, the knife coming down by it, once a beat)
    rest = limb([[24, -146], [42, -120]], 12, sleeve) + limb([[42, -120], [30, -106]], 9, skin) + `<circle cx="29" cy="-105" r="7" fill="${skin}" stroke="${sk}" stroke-width="1.5"/>` +
           `<path d="M24 -103 L-4 -101 L24 -99 Z" fill="#f28c28" stroke="#c96a12" stroke-width="1"/>`;
    work = lfoTimed(o, "fast", off, `<animateTransform attributeName="transform" type="translate" values="0 -10;0 0;0 -10" keyTimes="0;0.3;1" dur="0.6s" repeatCount="indefinite"/>`) +
      limb([[-24, -146], [-44, -124]], 12, sleeve) + limb([[-44, -124], [-26, -112]], 9, skin) + `<circle cx="-25" cy="-111" r="7" fill="${skin}" stroke="${sk}" stroke-width="1.5"/>` +
      `<path d="M-20 -110 L-4 -104 L-6 -100 L-24 -106 Z" fill="#d6dbe2" stroke="#8a929e" stroke-width="1"/>`;
  } else if (h.pose === "hammer") {
    // (the tongs in the left hand hold the work on the anvil; the right arm swings the hammer down onto it,
    // the hammer striking at the middle of each round, sparks then: smithyRoom)
    rest = limb([[24, -146], [30, -118]], 13, sleeve) + limb([[30, -118], [-20, -100]], 10, skin) + `<circle cx="-22" cy="-100" r="7.5" fill="${skin}" stroke="${sk}" stroke-width="1.5"/>` +
           `<path d="M-22 -100 L-84 -88 M-22 -96 L-84 -84" stroke="#2e2e36" stroke-width="3.5" stroke-linecap="round"/>` +
           `<rect x="-100" y="-90" width="22" height="7" rx="2" fill="#ff8a2e" stroke="#c2410c" stroke-width="1"/>`;
    work = lfoTimed(o, "fast", off, `<animateTransform attributeName="transform" type="rotate" values="95 -24 -146;0 -24 -146;95 -24 -146" keyTimes="0;0.5;1" calcMode="spline" keySplines="0.6 0 1 1;0 0 0.4 1" dur="0.9s" repeatCount="indefinite"/>`) +
      limb([[-24, -146], [-50, -128]], 13, sleeve) + limb([[-50, -128], [-70, -116]], 10, skin) + `<circle cx="-71" cy="-115" r="8" fill="${skin}" stroke="${sk}" stroke-width="1.5"/>` +
      `<line x1="-71" y1="-115" x2="-92" y2="-98" stroke="#6b4426" stroke-width="5" stroke-linecap="round"/>` +
      `<rect x="-104" y="-104" width="22" height="13" rx="2" transform="rotate(-38 -93 -97)" fill="#4a4a54" stroke="#2a2a30" stroke-width="1.5"/>`;
  }
  // the neck and head; the face: eyes, brows, rosy cheeks, a smile (or a smith's moustache and beard)
  t += rest + `<rect x="-7" y="-168" width="14" height="20" fill="${skin}"/><circle cx="0" cy="-190" r="30" fill="${skin}" stroke="${sk}" stroke-width="2"/>` +
       [-1, 1].map(e => `<ellipse cx="${e * 11}" cy="-193" rx="3.8" ry="4.8" fill="#2b1d14"/><circle cx="${e * 11 + 1.4}" cy="-195" r="1.4" fill="#ffffff"/>` +
         `<path d="M${e * 17} -204 Q${e * 11} ${h.beard ? -207 : -209} ${e * 5} -205" fill="none" stroke="${darker(hair)}" stroke-width="${h.beard ? 3.5 : 2.5}" stroke-linecap="round"/>` +
         `<circle cx="${e * 18}" cy="-181" r="6" fill="#f28b8b" opacity="0.4"/>`).join("") +
       `<path d="M-2 -187 Q0 -184 2 -187" fill="none" stroke="${sk}" stroke-width="1.5" stroke-linecap="round"/>`;
  t += h.beard ? `<path d="M-26 -186 Q-28 -150 0 -144 Q28 -150 26 -186 Q20 -172 8 -174 Q0 -168 -8 -174 Q-20 -172 -26 -186 Z" fill="${hair}" stroke="${darker(hair)}" stroke-width="1.5"/>` +
                 `<path d="M-14 -177 Q0 -184 14 -177 Q0 -172 -14 -177 Z" fill="${darker(hair)}"/>`
               : `<path d="M-11 -178 Q0 -166 11 -178 Q0 -172 -11 -178 Z" fill="#9b3b3b" stroke="#6b2b2b" stroke-width="1.5" stroke-linejoin="round"/>`;
  // the hair, and over it a hat: a cook's tall white toque, or a maid's white cap with its frill
  t += `<path d="M-31 -194 Q-34 -224 0 -224 Q34 -224 31 -194 Q24 -208 10 -210 Q0 -203 -10 -210 Q-24 -208 -31 -194 Z" fill="${hair}" stroke="${darker(hair)}" stroke-width="1.5"/>`;
  if (h.bun) t += `<circle cx="0" cy="-226" r="11" fill="${hair}" stroke="${darker(hair)}" stroke-width="1.5"/>`;
  if (h.hat === "toque") t += `<rect x="-24" y="-226" width="48" height="12" rx="3" fill="#ffffff" stroke="#d6d6d6" stroke-width="1.5"/>` +
    `<path d="M-24 -224 Q-40 -246 -20 -258 Q-10 -276 6 -266 Q26 -278 32 -256 Q44 -242 24 -224 Z" fill="#ffffff" stroke="#d6d6d6" stroke-width="1.5"/>`;
  if (h.hat === "cap") t += `<path d="M-34 -204 Q-38 -236 0 -238 Q38 -236 34 -204 Q30 -214 0 -216 Q-30 -214 -34 -204 Z" fill="#fbf8f0" stroke="#d9d2c0" stroke-width="1.5"/>` +
    [...Array(9)].map((_, i) => `<circle cx="${-32 + i * 8}" cy="${f1(-205 - 10 * Math.sin(Math.PI * i / 8))}" r="4" fill="#fbf8f0" stroke="#d9d2c0" stroke-width="1"/>`).join("");
  return t + `<g>${work}</g>`;
}
const KITCHEN_COOKS = [{ skin: "#e9b48c", hair: "#6b3f22", shirt: "#7d95b8", skirt: "#4f6488", apron: "#fbf8f0", hat: "cap", bun: true, pose: "stir" },
                       { skin: "#b5784e", hair: "#2e1d12", shirt: "#f4f1ea", legs: "#6b5038", apron: "#fbf8f0", hat: "toque", sleeves: "rolled", pose: "chop" }];
const SMITH = { skin: "#d39a6a", hair: "#3a2a1c", shirt: "#8a7a6a", legs: "#3e3a36", apron: "#7a4a26", long: true, beard: true, sleeves: "rolled", neckerchief: "#b83a2e", pose: "hammer" };
// The castle's kitchen: a big warm room of stone under oak beams. A great
// hearth in the back wall, its fire burning; in front of it a black cauldron
// on its tripod over its own little fire, steaming, that one cook stirs; a
// domed bread oven, its mouth aglow, loaves on its sill; shelves of crocks,
// plates and jars; strings of onions and garlic and bunches of herbs; copper
// pots and pans hanging from a rack on the beam; sacks of flour and barrels;
// and in front, a long worktable of bread, cheese, vegetables and a board,
// where the other cook chops carrots, once a beat. Its doors are in its side
// walls, as the king's room's are, and its way out a sign toward us.
// (ticket 185, v3: lifted out of the castle kitchen, unchanged) a barrel standing at (bx + dx k, by + dy),
// sc its size, two iron hoops; oakD its staves' edge
const barrelSvg = (bx, by, k, dx, dy, sc, oakD) => `<rect x="${f1(bx + dx * k - 20 * k * sc)}" y="${f1(by + dy - 60 * k * sc)}" width="${f1(40 * k * sc)}" height="${f1(60 * k * sc)}" rx="${f1(10 * k)}" fill="#8a5a32" stroke="${oakD}" stroke-width="${f1(1.5 * k)}"/>` +
      [0.2, 0.8].map(u => `<line x1="${f1(bx + dx * k - 20 * k * sc)}" y1="${f1(by - 60 * k * sc * (1 - u))}" x2="${f1(bx + dx * k + 20 * k * sc)}" y2="${f1(by - 60 * k * sc * (1 - u))}" stroke="#3a3a44" stroke-width="${f1(3 * k)}"/>`).join("");
function kitchenSvg(c, K) {
  const { y1, W, k, pad, R, P, o } = c, { at, Yh, floor, top, bw, vy } = roomSpace(c);
  const oak = "#7a5230", oakD = "#4e341d", oakL = "#a87548", copper = "#c46a32", copperD = "#8a4520";
  const pg = (pts, fill, more = "") => `<polygon points="${pts.map(p => P(...p)).join(" ")}" fill="${fill}"${more}/>`;
  const fl = mix(ROOM_TONE.kitchen, "#5a4a3a", 0.35);
  let t = roomShell(c, ROOM_TONE.kitchen, { floor: "#9a7e5c" }) +
    `<defs><radialGradient id="bg-fireglow"><stop offset="0" stop-color="#ffb347" stop-opacity="0.55"/><stop offset="1" stop-color="#ffb347" stop-opacity="0"/></radialGradient></defs>`;
  // (ticket 99: the hearth's glow is the room's "fire light", a front object,
  // over the friend wherever they stand; the oven's glow and the cauldron's
  // steam go with them, in their objects)
  let fireLight = "";
  // the beams across the ceiling, toward us
  for (const z of [0, -0.2, -0.42]) { const f = 1 / (1 + z), [xa, ya] = at(-bw, top + 26 * k, z);
    t += `<rect x="${f1(xa)}" y="${f1(ya)}" width="${f1(-2 * xa)}" height="${f1(18 * k * f)}" fill="${oak}" stroke="${oakD}" stroke-width="${f1(1.5 * k)}"/>`; }
  // the great hearth, left of the middle: a stone arch, its fire, the dark behind it, a stone mantel
  const hx = -bw * 0.5, hw = 96 * k, hh = Yh(0.46), [hl, hb] = at(hx - hw, floor), [hr] = at(hx + hw, floor);
  const mHearth = t.length;
  t += `<rect x="${f1(hl - 16 * k)}" y="${f1(hh - 30 * k)}" width="${f1(hr - hl + 32 * k)}" height="${f1(hb - hh + 30 * k)}" fill="${mix(ROOM_TONE.kitchen, "#ffffff", 0.12)}"/>` +
       `<path d="M${f1(hl)} ${f1(hb)} V${f1(hh + 30 * k)} Q${f1(hx)} ${f1(hh - 18 * k)} ${f1(hr)} ${f1(hh + 30 * k)} V${f1(hb)} Z" fill="#1e140c"/>` +
       `<rect x="${f1(hl - 26 * k)}" y="${f1(hh - 42 * k)}" width="${f1(hr - hl + 52 * k)}" height="${f1(14 * k)}" fill="${mix(ROOM_TONE.kitchen, "#ffffff", 0.22)}"/>` +
       `<rect x="${f1(hx - 50 * k)}" y="${f1(hb - 12 * k)}" width="${f1(100 * k)}" height="${f1(10 * k)}" rx="${f1(5 * k)}" fill="${oakD}"/>`;
  // (its fire burns in the wall, behind all that stands in front of it)
  [-0.5, 0, 0.5].forEach((u, j) => { const h = (j === 1 ? 70 : 52) * k, bx = hx + u * 60 * k;
    t += (`<g transform="translate(${f1(bx)} ${f1(hb - 8 * k)})"><path d="M${f1(-20 * k)} 0 Q${f1(-18 * k)} ${f1(-h * 0.6)} 0 ${f1(-h)} Q${f1(18 * k)} ${f1(-h * 0.6)} ${f1(20 * k)} 0 Z" fill="${j === 1 ? "#ffd05a" : "#ff8a2e"}">` +
      sceneLfo(o, "fast", (j * 0.31) % 1, "transform", (v, amp) => `1 ${(1 + (0.08 + 0.18 * amp) * v).toFixed(3)}`,
        `<animateTransform attributeName="transform" type="scale" values="1 1;1 1.12;1 0.92;1 1" dur="${(0.7 + 0.13 * j).toFixed(2)}s" repeatCount="indefinite"/>`, "scale") + `</path></g>`); });
  t = wallSince(t, mHearth, "hearth");   // (ticket 284: a wall object; its glow is still the room's "fire light")
  fireLight = `<circle cx="${f1(hx)}" cy="${f1(hb - 40 * k)}" r="${f1(150 * k)}" fill="url(#bg-fireglow)">${sceneLfo(o, "slow", 0.2, "opacity", (v, amp) => (0.9 - (0.15 + 0.3 * amp) * (1 + v) / 2).toFixed(2), "")}</circle>`;
  // strings of onions and garlic, and bunches of herbs, hung along the mantel
  const mStrings = t.length;
  for (let i = 0; i < 6; i++) { const x = hl - 10 * k + (hr - hl + 20 * k) * (i + 0.5) / 6, y = hh - 28 * k, kind = i % 3;
    t += `<line x1="${f1(x)}" y1="${f1(y)}" x2="${f1(x)}" y2="${f1(y + 44 * k)}" stroke="#8a6a3a" stroke-width="${f1(1.5 * k)}"/>` +
         (kind === 2 ? `<path d="M${f1(x)} ${f1(y + 40 * k)} l${f1(-9 * k)} ${f1(22 * k)} M${f1(x)} ${f1(y + 40 * k)} l0 ${f1(26 * k)} M${f1(x)} ${f1(y + 40 * k)} l${f1(9 * k)} ${f1(22 * k)}" stroke="#4f8a3a" stroke-width="${f1(4 * k)}" stroke-linecap="round"/>`
           : [0, 1, 2, 3].map(j => `<ellipse cx="${f1(x + (j % 2 ? 5 : -5) * k)}" cy="${f1(y + (14 + 9 * j) * k)}" rx="${f1(7 * k)}" ry="${f1(6 * k)}" fill="${kind ? "#f1e6cf" : "#c98a3a"}" stroke="${kind ? "#cbbd9e" : "#8a5a20"}" stroke-width="${f1(1 * k)}"/>`).join("")); }
  t = wallSince(t, mStrings, "onions, garlic and herbs");   // (ticket 284: a wall object)
  // the bread oven, right of the middle: a dome of brick, its mouth aglow, loaves on its sill
  // (an object, standing against the back wall, at its foot; its glow inside
  // its mouth, high in it, where the worktable in front leaves it to be seen)
  const ox = bw * 0.58, orad = 80 * k, [, ob] = at(0, floor);
  t += obj("oven", ob, `<rect x="${f1(ox - orad - 10 * k)}" y="${f1(ob - 70 * k)}" width="${f1(2 * orad + 20 * k)}" height="${f1(70 * k)}" fill="${mix(ROOM_TONE.kitchen, "#000000", 0.1)}"/>` +
       `<path d="M${f1(ox - orad)} ${f1(ob - 70 * k)} A${f1(orad)} ${f1(orad * 0.9)} 0 0 1 ${f1(ox + orad)} ${f1(ob - 70 * k)} Z" fill="#b5653f" stroke="#8a4528" stroke-width="${f1(2 * k)}"/>` +
       [0.3, 0.6].map(u => `<path d="M${f1(ox - orad * Math.sqrt(1 - u * u))} ${f1(ob - 70 * k - orad * 0.9 * u)} H${f1(ox + orad * Math.sqrt(1 - u * u))}" stroke="#8a4528" stroke-width="${f1(1.5 * k)}"/>`).join("") +
       `<path d="M${f1(ox - 28 * k)} ${f1(ob - 70 * k)} V${f1(ob - 96 * k)} Q${f1(ox)} ${f1(ob - 124 * k)} ${f1(ox + 28 * k)} ${f1(ob - 96 * k)} V${f1(ob - 70 * k)} Z" fill="#2a160c"/>` +
       // (the glow: wholly inside the mouth, its arch and its sides, at any size)
       `<ellipse cx="${f1(ox)}" cy="${f1(ob - 98 * k)}" rx="${f1(22 * k)}" ry="${f1(10 * k)}" fill="#ff9a3c" opacity="0.8">${sceneLfo(o, "slow", 0.6, "opacity", (v, amp) => (0.8 - (0.1 + 0.3 * amp) * (1 + v) / 2).toFixed(2), "")}</ellipse>` +
       `<rect x="${f1(ox - orad - 14 * k)}" y="${f1(ob - 74 * k)}" width="${f1(2 * orad + 28 * k)}" height="${f1(8 * k)}" fill="${oakD}"/>` +
       [-0.7, -0.45, 0.5, 0.75].map(u => `<ellipse cx="${f1(ox + u * orad)}" cy="${f1(ob - 81 * k)}" rx="${f1(13 * k)}" ry="${f1(8 * k)}" fill="#d9a05a" stroke="#a8702e" stroke-width="${f1(1.2 * k)}"/>` +
         `<path d="M${f1(ox + u * orad - 6 * k)} ${f1(ob - 84 * k)} l${f1(4 * k)} ${f1(-3 * k)} M${f1(ox + u * orad + 1 * k)} ${f1(ob - 85 * k)} l${f1(4 * k)} ${f1(-3 * k)}" stroke="#a8702e" stroke-width="${f1(1.2 * k)}"/>`).join(""));
  // shelves on the back wall between them: crocks, plates on edge, jars
  for (const [yy, kind] of [[Yh(0.72), "plates"], [Yh(0.56), "crocks"]]) { const xa = -bw * 0.12, xb = bw * 0.3, mShelf = t.length;   // (ticket 284: each shelf a wall object)
    t += `<rect x="${f1(xa)}" y="${f1(yy)}" width="${f1(xb - xa)}" height="${f1(7 * k)}" fill="${oak}" stroke="${oakD}" stroke-width="${f1(1 * k)}"/>`;
    for (let i = 0; i < 6; i++) { const x = xa + (xb - xa) * (i + 0.5) / 6;
      t += kind === "plates" ? `<ellipse cx="${f1(x)}" cy="${f1(yy - 15 * k)}" rx="${f1(13 * k)}" ry="${f1(15 * k)}" fill="#f4efe4" stroke="#5a7ab0" stroke-width="${f1(2 * k)}"/><circle cx="${f1(x)}" cy="${f1(yy - 15 * k)}" r="${f1(6 * k)}" fill="none" stroke="#5a7ab0" stroke-width="${f1(1.2 * k)}"/>`
        : `<path d="M${f1(x - 10 * k)} ${f1(yy)} Q${f1(x - 14 * k)} ${f1(yy - 18 * k)} ${f1(x - 7 * k)} ${f1(yy - 26 * k)} H${f1(x + 7 * k)} Q${f1(x + 14 * k)} ${f1(yy - 18 * k)} ${f1(x + 10 * k)} ${f1(yy)} Z" fill="${["#c98a5a", "#8aa0b8", "#d8c08a"][i % 3]}" stroke="${oakD}" stroke-width="${f1(1 * k)}"/>`; }
    t = wallSince(t, mShelf, `shelf of ${kind}`); }
  // copper pots and pans on hooks along a rack hung from the middle beam
  // (one object, hung from the ceiling: at the floor under the rack)
  { const z = -0.2, f = 1 / (1 + z), [ra, ry] = at(-bw * 0.42, top + 70 * k, z), [rb] = at(bw * 0.42, top + 70 * k, z), [, by] = at(0, top + 44 * k, z);
    let rack = [ra, rb].map(x => `<line x1="${f1(x)}" y1="${f1(by)}" x2="${f1(x)}" y2="${f1(ry)}" stroke="#3a3a44" stroke-width="${f1(2 * k)}"/>`).join("") +
         `<rect x="${f1(ra)}" y="${f1(ry)}" width="${f1(rb - ra)}" height="${f1(6 * k * f)}" fill="#3a3a44"/>`;
    for (let i = 0; i < 7; i++) { const x = ra + (rb - ra) * (i + 0.5) / 7, d = (16 + 6 * (i % 3)) * k * f, hang = (10 + 8 * (i % 2)) * k * f, yy = ry + 6 * k * f + hang;
      rack += `<line x1="${f1(x)}" y1="${f1(ry + 6 * k * f)}" x2="${f1(x)}" y2="${f1(yy)}" stroke="#3a3a44" stroke-width="${f1(1.5 * k)}"/>` +
           (i % 2 ? `<path d="M${f1(x - d)} ${f1(yy + 4 * k * f)} H${f1(x + d)} V${f1(yy + d * 1.2)} Q${f1(x)} ${f1(yy + d * 1.5)} ${f1(x - d)} ${f1(yy + d * 1.2)} Z" fill="${copper}" stroke="${copperD}" stroke-width="${f1(1.5 * k)}"/>`
                  : `<circle cx="${f1(x)}" cy="${f1(yy + d)}" r="${f1(d)}" fill="${copper}" stroke="${copperD}" stroke-width="${f1(1.5 * k)}"/><line x1="${f1(x)}" y1="${f1(yy)}" x2="${f1(x)}" y2="${f1(yy + 2 * k)}" stroke="${copperD}" stroke-width="${f1(4 * k)}"/>`) +
           `<path d="M${f1(x - d * 0.5)} ${f1(yy + d * 0.7)} q${f1(d * 0.2)} ${f1(-d * 0.3)} ${f1(d * 0.4)} 0" stroke="#f0b088" stroke-width="${f1(1.5 * k)}" fill="none" opacity="0.7"/>`; }
    t += obj("pots and pans", at(0, floor, z)[1], rack); }
  // sacks of flour and barrels by the back wall, right
  // (the barrels one object, the sack another, both standing at by)
  { const [bx, by] = at(bw * 0.92, floor, -0.02), f = 1.02;
    let barrels = "";
    for (const [dx, dy, sc] of [[-40, 0, 1], [0, 0, 1.1]]) barrels += barrelSvg(bx, by, k, dx, dy, sc, oakD);
    t += obj("barrels", by, barrels) + obj("flour sack", by, `<path d="M${f1(bx - 96 * k)} ${f1(by)} Q${f1(bx - 104 * k)} ${f1(by - 40 * k)} ${f1(bx - 84 * k)} ${f1(by - 52 * k)} Q${f1(bx - 72 * k)} ${f1(by - 58 * k)} ${f1(bx - 62 * k)} ${f1(by - 50 * k)} Q${f1(bx - 52 * k)} ${f1(by - 36 * k)} ${f1(bx - 58 * k)} ${f1(by)} Z" fill="#efe6d2" stroke="#c9b993" stroke-width="${f1(1.5 * k)}"/>` +
         `<text x="${f1(bx - 77 * k)}" y="${f1(by - 18 * k)}" font-size="${f1(11 * k)}" text-anchor="middle" fill="#8a7a5a" font-family="serif">FLOUR</text>`); }
  // the cauldron cook: in front of the hearth, stirring the cauldron on its tripod over its little fire
  // (the cauldron, its tripod, fire, pot and steam, one object, and the cook
  // another: both standing at her feet, cfy, where the tripod's legs stand
  // too; the cauldron first, so her ladle stays over the pot)
  const cz = -0.16, cf = 1 / (1 + cz), cs = 1.05 * k * cf, [cxx, cfy] = at(hx + 150 * k, floor, cz), potX = cxx - 110 * cs, potY = cfy - 46 * cs;   // (the pot's mouth, where her ladle dips)
  let cauldron = [-1, 1, 0].map(e => `<line x1="${f1(potX)}" y1="${f1(potY - 60 * cs)}" x2="${f1(potX + e * 46 * cs)}" y2="${f1(cfy + (e ? 0 : -8 * cs))}" stroke="#2a2a30" stroke-width="${f1(3 * cs)}"/>`).join("") +
       `<rect x="${f1(potX - 22 * cs)}" y="${f1(cfy - 8 * cs)}" width="${f1(44 * cs)}" height="${f1(7 * cs)}" rx="${f1(3 * cs)}" fill="${oakD}"/>`;
  cauldron += `<g transform="translate(${f1(potX)} ${f1(cfy - 6 * cs)})"><path d="M${f1(-16 * cs)} 0 Q0 ${f1(-34 * cs)} ${f1(16 * cs)} 0 Z" fill="#ff8a2e">` +
    sceneLfo(o, "fast", 0.7, "transform", (v, amp) => `1 ${(1 + (0.1 + 0.2 * amp) * v).toFixed(3)}`, `<animateTransform attributeName="transform" type="scale" values="1 1;1 1.2;1 0.9;1 1" dur="0.8s" repeatCount="indefinite"/>`, "scale") + `</path></g>`;
  // (a shallow pot, hung clear above its fire)
  cauldron += `<path d="M${f1(potX - 40 * cs)} ${f1(potY)} Q${f1(potX - 42 * cs)} ${f1(potY + 22 * cs)} ${f1(potX)} ${f1(potY + 24 * cs)} Q${f1(potX + 42 * cs)} ${f1(potY + 22 * cs)} ${f1(potX + 40 * cs)} ${f1(potY)} Z" fill="#26262c" stroke="#111114" stroke-width="${f1(2 * cs)}"/>` +
       `<ellipse cx="${f1(potX)}" cy="${f1(potY)}" rx="${f1(40 * cs)}" ry="${f1(10 * cs)}" fill="#3a3a42"/><ellipse cx="${f1(potX)}" cy="${f1(potY + 2 * cs)}" rx="${f1(34 * cs)}" ry="${f1(7 * cs)}" fill="#c98a3a"/>`;
  const stirrer = `<g transform="translate(${f1(cxx)} ${f1(cfy)}) scale(${cs.toFixed(4)})">${helperSvg(KITCHEN_COOKS[0], o, 0.1)}</g>`;
  // (its steam, rising and fading, with the slow LFO)
  cauldron += [0, 1, 2].map(j => `<path d="M${f1(potX + (j - 1) * 14 * cs)} ${f1(potY - 6 * cs)} q${f1(-10 * cs)} ${f1(-20 * cs)} 0 ${f1(-40 * cs)} q${f1(10 * cs)} ${f1(-20 * cs)} 0 ${f1(-40 * cs)}" fill="none" stroke="#ffffff" stroke-width="${f1(6 * cs)}" stroke-linecap="round" opacity="0.4">` +
    lfoTimed(o, "slow", j / 3, `<animate attributeName="opacity" values="0;0.5;0" dur="${(3 + j * 0.7).toFixed(1)}s" repeatCount="indefinite"/>`) + `</path>`).join("");
  t += obj("cauldron", cfy, cauldron) + obj("cook, stirring", cfy, stirrer);
  // the worktable, in front, right of the middle: the other cook behind it, chopping carrots on a board
  const tz0 = -0.24, tz1 = -0.4, tx0 = bw * 0.02, tx1 = bw * 0.72, tH = floor - 118 * k;
  // (the cook an object at her feet; the worktable one object, its legs, top
  // and all on it, standing at its near legs, tz1)
  const mz = -0.1, mf = 1 / (1 + mz), ms = 1.05 * k * mf, [mx, my] = at((tx0 + tx1) / 2 + 10 * k, floor, mz);
  t += obj("cook, chopping", my, `<g transform="translate(${f1(mx)} ${f1(my)}) scale(${ms.toFixed(4)})">${helperSvg(KITCHEN_COOKS[1], o, 0.5)}</g>`);
  let wt = "";
  for (const [x, z] of [[tx0 + 10 * k, tz0], [tx1 - 10 * k, tz0], [tx0 + 10 * k, tz1], [tx1 - 10 * k, tz1]]) { const [lx, ly] = at(x, tH, z), [, lb] = at(x, floor, z), f = 1 / (1 + z);
    wt += `<rect x="${f1(lx - 6 * k * f)}" y="${f1(ly)}" width="${f1(12 * k * f)}" height="${f1(lb - ly)}" fill="${oakD}"/>`; }
  wt += pg([at(tx0, tH, tz0), at(tx1, tH, tz0), at(tx1, tH, tz1), at(tx0, tH, tz1)], oakL, ` stroke="${oakD}" stroke-width="${f1(1.5 * k)}"`) +
       pg([at(tx0, tH, tz1), at(tx1, tH, tz1), at(tx1, tH + 14 * k, tz1), at(tx0, tH + 14 * k, tz1)], oak) +
       [0.25, 0.5, 0.75].map(u => { const z = tz0 + (tz1 - tz0) * u; return `<line x1="${f1(at(tx0, tH, z)[0])}" y1="${f1(at(tx0, tH, z)[1])}" x2="${f1(at(tx1, tH, z)[0])}" y2="${f1(at(tx1, tH, z)[1])}" stroke="${oak}" stroke-width="${f1(1 * k)}"/>`; }).join("");
  // what is on it: the board and carrots by the cook's hands, loaves, a round of cheese, a cabbage, a bowl of apples
  const on = (x, z) => at(x, tH, z), [bdx, bdy] = on(mx - 2 * k, tz0 - 0.03);
  wt += `<rect x="${f1(bdx - 34 * k)}" y="${f1(bdy - 6 * k)}" width="${f1(64 * k)}" height="${f1(12 * k)}" rx="${f1(3 * k)}" fill="#d8b07a" stroke="#a8804a" stroke-width="${f1(1 * k)}"/>` +
       [0, 1, 2, 3].map(i => `<circle cx="${f1(bdx - 26 * k + i * 6 * k)}" cy="${f1(bdy)}" r="${f1(2.6 * k)}" fill="#f28c28"/>`).join("");
  for (const [u, w, kind] of [[0.12, 0.3, "loaf"], [0.32, 0.65, "cheese"], [0.86, 0.35, "cabbage"], [0.88, 0.75, "apples"], [0.1, 0.72, "loaf"]]) {
    const [x, y] = on(tx0 + (tx1 - tx0) * u, tz0 + (tz1 - tz0) * w), f = 1 / (1 + tz0 + (tz1 - tz0) * w), q = k * f;
    wt += kind === "loaf" ? `<ellipse cx="${f1(x)}" cy="${f1(y - 8 * q)}" rx="${f1(22 * q)}" ry="${f1(11 * q)}" fill="#d9a05a" stroke="#a8702e" stroke-width="${f1(1.2 * q)}"/><path d="M${f1(x - 12 * q)} ${f1(y - 12 * q)} l${f1(6 * q)} ${f1(-5 * q)} M${f1(x)} ${f1(y - 13 * q)} l${f1(6 * q)} ${f1(-5 * q)}" stroke="#a8702e" stroke-width="${f1(1.4 * q)}"/>`
      : kind === "cheese" ? `<path d="M${f1(x - 20 * q)} ${f1(y)} V${f1(y - 14 * q)} A${f1(20 * q)} ${f1(6 * q)} 0 0 1 ${f1(x + 20 * q)} ${f1(y - 14 * q)} V${f1(y)} Z" fill="#f2c94c" stroke="#c9a02a" stroke-width="${f1(1.2 * q)}"/><ellipse cx="${f1(x)}" cy="${f1(y - 14 * q)}" rx="${f1(20 * q)}" ry="${f1(6 * q)}" fill="#f7d96a" stroke="#c9a02a" stroke-width="${f1(1 * q)}"/>`
      : kind === "cabbage" ? `<circle cx="${f1(x)}" cy="${f1(y - 13 * q)}" r="${f1(14 * q)}" fill="#7cb342" stroke="#4f7a2a" stroke-width="${f1(1.2 * q)}"/><path d="M${f1(x - 8 * q)} ${f1(y - 20 * q)} Q${f1(x)} ${f1(y - 8 * q)} ${f1(x + 8 * q)} ${f1(y - 20 * q)}" fill="none" stroke="#4f7a2a" stroke-width="${f1(1.2 * q)}"/>`
      : `<ellipse cx="${f1(x)}" cy="${f1(y - 6 * q)}" rx="${f1(22 * q)}" ry="${f1(8 * q)}" fill="#8a5a32"/>` + [-10, 0, 10, -5, 5].map((dx, i) => `<circle cx="${f1(x + dx * q)}" cy="${f1(y - (i < 3 ? 12 : 19) * q)}" r="${f1(6 * q)}" fill="${i % 2 ? "#e53935" : "#7cb342"}"/>`).join("");
  }
  t += obj("worktable", at(0, floor, tz1)[1], wt);
  return t + obj("fire light", Infinity, fireLight);
}
// The smithy: dark and very rustic. Rough stone, black with soot, under old
// timbers; the forge in the back wall, its coals glowing and breathing with
// the slow LFO, a hood over it to a chimney, its bellows working; the smith
// at the anvil on its stump, beating a glowing bar with his hammer, once
// round the quick LFO, the sparks flying off it as the hammer strikes; a
// quench barrel, steaming; tongs, hammers and horseshoes on the wall; a rack
// of swords and a shield; a heap of coal. The room's light is the forge's.
function smithySvg(c, K) {
  const { y1, W, k, pad, P, o } = c, { at, Yh, floor, top, bw, vy } = roomSpace(c);
  const oak = "#5a3a20", oakD = "#3a2412", iron = "#3a3a44", ironL = "#8a8f9a";
  let t = roomShell(c, ROOM_TONE.smithy, { floor: "#4a4038" }) +
    `<defs><radialGradient id="bg-forge"><stop offset="0" stop-color="#ff7a1a" stop-opacity="0.7"/><stop offset="0.5" stop-color="#ff5a0a" stop-opacity="0.25"/><stop offset="1" stop-color="#ff5a0a" stop-opacity="0"/></radialGradient>` +
    `<radialGradient id="bg-coals"><stop offset="0" stop-color="#fff0a0"/><stop offset="0.4" stop-color="#ff9a2e"/><stop offset="1" stop-color="#a0280a"/></radialGradient></defs>`;
  // soot up the walls, and old timbers across the ceiling
  t += `<rect x="${f1(-bw)}" y="${f1(top)}" width="${f1(2 * bw)}" height="${f1(floor - top)}" fill="#000000" opacity="0.25"/>`;
  for (const z of [0, -0.22, -0.46]) { const f = 1 / (1 + z), [xa, ya] = at(-bw, top + 24 * k, z);
    t += `<rect x="${f1(xa)}" y="${f1(ya)}" width="${f1(-2 * xa)}" height="${f1(20 * k * f)}" fill="${oak}" stroke="${oakD}" stroke-width="${f1(2 * k)}"/>`; }
  // the forge: a brick hearth, its coals, a hood up to the chimney, the bellows at its side
  // (one object with its hood and bellows, standing against the back wall, at
  // its foot, its coals' glow and its light (the room's) in it, over its bellows)
  const fx = -bw * 0.42, fw = 104 * k, [, fb] = at(0, floor), fy = fb - 92 * k;
  let forge = `<path d="M${f1(fx - fw * 0.8)} ${f1(fy - 50 * k)} L${f1(fx - fw * 0.3)} ${f1(top)} H${f1(fx + fw * 0.3)} L${f1(fx + fw * 0.8)} ${f1(fy - 50 * k)} Z" fill="#2a2420" stroke="#1a1612" stroke-width="${f1(2 * k)}"/>` +
       `<rect x="${f1(fx - fw)}" y="${f1(fy)}" width="${f1(2 * fw)}" height="${f1(fb - fy)}" fill="#7a3a26" stroke="#4a2014" stroke-width="${f1(2 * k)}"/>` +
       [1, 2, 3].map(r => `<line x1="${f1(fx - fw)}" y1="${f1(fy + (fb - fy) * r / 4)}" x2="${f1(fx + fw)}" y2="${f1(fy + (fb - fy) * r / 4)}" stroke="#4a2014" stroke-width="${f1(1.5 * k)}"/>`).join("") +
       `<rect x="${f1(fx - fw - 6 * k)}" y="${f1(fy - 8 * k)}" width="${f1(2 * fw + 12 * k)}" height="${f1(10 * k)}" fill="#5a2a1a"/>`;
  const forgeLight = `<ellipse cx="${f1(fx)}" cy="${f1(fy - 6 * k)}" rx="${f1(fw * 0.8)}" ry="${f1(12 * k)}" fill="url(#bg-coals)">${sceneLfo(o, "slow", 0, "opacity", (v, amp) => (1 - (0.15 + 0.3 * amp) * (1 + v) / 2).toFixed(2),
      `<animate attributeName="opacity" values="1;0.75;1" dur="3.2s" repeatCount="indefinite"/>`)}</ellipse>` +
    `<circle cx="${f1(fx)}" cy="${f1(fy - 10 * k)}" r="${f1(260 * k)}" fill="url(#bg-forge)">${sceneLfo(o, "slow", 0, "opacity", (v, amp) => (0.95 - (0.15 + 0.3 * amp) * (1 + v) / 2).toFixed(2),
      `<animate attributeName="opacity" values="0.95;0.7;0.95" dur="3.2s" repeatCount="indefinite"/>`)}</circle>`;
  // (the bellows, to the forge's left, its top board rising and falling with the slow LFO)
  { const bx = fx - fw - 54 * k, by = fy + 20 * k;
    forge += `<path d="M${f1(bx - 40 * k)} ${f1(by)} L${f1(bx + 40 * k)} ${f1(by)} L${f1(bx + 52 * k)} ${f1(by - 6 * k)} L${f1(bx + 40 * k)} ${f1(by + 10 * k)} L${f1(bx - 40 * k)} ${f1(by + 10 * k)} Z" fill="${oak}"/>` +
         `<g><path d="M${f1(bx - 40 * k)} ${f1(by)} Q${f1(bx - 46 * k)} ${f1(by - 26 * k)} ${f1(bx - 34 * k)} ${f1(by - 30 * k)} L${f1(bx + 40 * k)} ${f1(by - 4 * k)} Z" fill="#6b4a32" stroke="${oakD}" stroke-width="${f1(1.5 * k)}"/>` +
         `<line x1="${f1(bx - 34 * k)}" y1="${f1(by - 30 * k)}" x2="${f1(bx - 60 * k)}" y2="${f1(by - 70 * k)}" stroke="${oak}" stroke-width="${f1(5 * k)}" stroke-linecap="round"/>` +
         lfoTimed(o, "slow", 0.3, `<animateTransform attributeName="transform" type="rotate" values="0 ${f1(bx + 40 * k)} ${f1(by - 4 * k)};-9 ${f1(bx + 40 * k)} ${f1(by - 4 * k)};0 ${f1(bx + 40 * k)} ${f1(by - 4 * k)}" dur="3.2s" repeatCount="indefinite"/>`) + `</g>`; }
  t += obj("forge", fb, forge + forgeLight);
  // the wall's right: tongs, hammers and horseshoes on pegs; a rack of swords; a round shield
  const mTools = t.length;   // (ticket 284: the tools, the rack of swords and the shield, each a wall object)
  { const y = Yh(0.62);
    t += `<rect x="${f1(bw * 0.05)}" y="${f1(y)}" width="${f1(bw * 0.5)}" height="${f1(8 * k)}" fill="${oak}"/>`;
    for (let i = 0; i < 7; i++) { const x = bw * 0.1 + bw * 0.42 * i / 6;
      t += i % 3 === 0 ? `<path d="M${f1(x - 3 * k)} ${f1(y + 8 * k)} L${f1(x - 8 * k)} ${f1(y + 70 * k)} M${f1(x + 3 * k)} ${f1(y + 8 * k)} L${f1(x + 8 * k)} ${f1(y + 70 * k)}" stroke="${iron}" stroke-width="${f1(3 * k)}" stroke-linecap="round"/>`
        : i % 3 === 1 ? `<line x1="${f1(x)}" y1="${f1(y + 8 * k)}" x2="${f1(x)}" y2="${f1(y + 58 * k)}" stroke="#6b4426" stroke-width="${f1(4 * k)}"/><rect x="${f1(x - 10 * k)}" y="${f1(y + 54 * k)}" width="${f1(20 * k)}" height="${f1(12 * k)}" rx="${f1(2 * k)}" fill="${iron}"/>`
        : `<path d="M${f1(x - 11 * k)} ${f1(y + 40 * k)} A${f1(11 * k)} ${f1(13 * k)} 0 1 1 ${f1(x + 11 * k)} ${f1(y + 40 * k)}" fill="none" stroke="${ironL}" stroke-width="${f1(5 * k)}"/>`; } }
  t = wallSince(t, mTools, "tools on pegs");
  const mSwords = t.length;
  { const rx = bw * 0.78, ry = Yh(0.3);
    t += `<rect x="${f1(rx - 50 * k)}" y="${f1(ry)}" width="${f1(100 * k)}" height="${f1(8 * k)}" fill="${oak}"/><rect x="${f1(rx - 50 * k)}" y="${f1(ry + 90 * k)}" width="${f1(100 * k)}" height="${f1(8 * k)}" fill="${oak}"/>`;
    for (const dx of [-30, 0, 30]) t += `<path d="M${f1(rx + dx * k)} ${f1(ry - 40 * k)} L${f1(rx + dx * k - 4 * k)} ${f1(ry + 110 * k)} L${f1(rx + dx * k + 4 * k)} ${f1(ry + 110 * k)} Z" fill="#c9ced6" stroke="#6a707c" stroke-width="${f1(1 * k)}"/>` +
      `<rect x="${f1(rx + dx * k - 12 * k)}" y="${f1(ry - 44 * k)}" width="${f1(24 * k)}" height="${f1(5 * k)}" fill="#8a6a2a"/><rect x="${f1(rx + dx * k - 2.5 * k)}" y="${f1(ry - 64 * k)}" width="${f1(5 * k)}" height="${f1(20 * k)}" fill="#5a3a20"/>`;
    t = wallSince(t, mSwords, "rack of swords");
    const mShield = t.length;
    const [sx, sy] = at(bw * 0.4, Yh(0.32));
    t += `<circle cx="${f1(sx)}" cy="${f1(sy)}" r="${f1(34 * k)}" fill="${castleFlagFill(K, 0, 8 * k)}" stroke="#8a8f9a" stroke-width="${f1(5 * k)}"/><circle cx="${f1(sx)}" cy="${f1(sy)}" r="${f1(8 * k)}" fill="#c9ced6"/>`;
    t = wallSince(t, mShield, "shield"); }
  // a heap of coal by the forge (an object, at its foot)
  { const [cx, cy] = at(fx + fw + 40 * k, floor, -0.05);
    t += obj("coal heap", cy, `<path d="M${f1(cx - 50 * k)} ${f1(cy)} Q${f1(cx - 30 * k)} ${f1(cy - 40 * k)} ${f1(cx)} ${f1(cy - 44 * k)} Q${f1(cx + 34 * k)} ${f1(cy - 36 * k)} ${f1(cx + 50 * k)} ${f1(cy)} Z" fill="#1c1c22"/>` +
         [...Array(9)].map((_, i) => `<circle cx="${f1(cx - 38 * k + i * 9.5 * k)}" cy="${f1(cy - 8 * k - 22 * k * Math.sin(Math.PI * (i + 0.5) / 9))}" r="${f1(5 * k)}" fill="#2e2e36"/>`).join("")); }
  // the quench barrel, front right, steaming with the slow LFO
  // (an object, at its foot, its steam in it)
  { const z = -0.36, f = 1 / (1 + z), q = k * f, [bx, by] = at(bw * 0.5, floor, z);
    t += obj("water barrel", by, `<path d="M${f1(bx - 34 * q)} ${f1(by)} Q${f1(bx - 40 * q)} ${f1(by - 40 * q)} ${f1(bx - 34 * q)} ${f1(by - 80 * q)} H${f1(bx + 34 * q)} Q${f1(bx + 40 * q)} ${f1(by - 40 * q)} ${f1(bx + 34 * q)} ${f1(by)} Z" fill="${oak}" stroke="${oakD}" stroke-width="${f1(2 * q)}"/>` +
         [0.15, 0.85].map(u => `<line x1="${f1(bx - 37 * q)}" y1="${f1(by - 80 * q * u)}" x2="${f1(bx + 37 * q)}" y2="${f1(by - 80 * q * u)}" stroke="${iron}" stroke-width="${f1(4 * q)}"/>`).join("") +
         `<ellipse cx="${f1(bx)}" cy="${f1(by - 80 * q)}" rx="${f1(34 * q)}" ry="${f1(8 * q)}" fill="#2a4a5a"/>` +
      [0, 1].map(j => `<path d="M${f1(bx + (j ? 10 : -10) * q)} ${f1(by - 84 * q)} q${f1(-10 * q)} ${f1(-20 * q)} 0 ${f1(-40 * q)} q${f1(10 * q)} ${f1(-20 * q)} 0 ${f1(-40 * q)}" fill="none" stroke="#e8e8f0" stroke-width="${f1(6 * q)}" stroke-linecap="round" opacity="0.3">` +
      lfoTimed(o, "slow", 0.4 + j / 2, `<animate attributeName="opacity" values="0;0.4;0" dur="${(3.4 + j).toFixed(1)}s" repeatCount="indefinite"/>`) + `</path>`).join("")); }
  // the anvil on its stump, the smith beside it, striking the glowing bar on it; the sparks as the hammer lands
  // (the anvil, with its stump, an object, and the smith another, both standing
  // at ay; the anvil first, so his tongs and the bar stay over it; his sparks
  // are his, over him and the anvil, and go when he does)
  const z = -0.2, f = 1 / (1 + z), s = 1.08 * k * f, [ax, ay] = at(bw * 0.08, floor, z), hitX = ax, sx = ax + 96 * s, hitY = ay - 86 * s;   // (the smith stands so his strike, at (-96, -86) of his own, lands on the anvil)
  t += obj("anvil", ay, `<path d="M${f1(ax - 26 * s)} ${f1(ay)} L${f1(ax - 22 * s)} ${f1(ay - 50 * s)} H${f1(ax + 22 * s)} L${f1(ax + 26 * s)} ${f1(ay)} Z" fill="#6b4a32" stroke="${oakD}" stroke-width="${f1(2 * s)}"/>` +
       `<ellipse cx="${f1(ax)}" cy="${f1(ay - 50 * s)}" rx="${f1(22 * s)}" ry="${f1(5 * s)}" fill="#8a6a4a"/>` +
       `<path d="M${f1(ax - 18 * s)} ${f1(ay - 50 * s)} L${f1(ax - 12 * s)} ${f1(ay - 64 * s)} H${f1(ax + 12 * s)} L${f1(ax + 18 * s)} ${f1(ay - 50 * s)} Z" fill="#2a2a30"/>` +
       `<path d="M${f1(ax - 40 * s)} ${f1(ay - 72 * s)} Q${f1(ax - 56 * s)} ${f1(ay - 76 * s)} ${f1(ax - 64 * s)} ${f1(ay - 84 * s)} H${f1(ax + 34 * s)} V${f1(ay - 72 * s)} H${f1(ax + 12 * s)} V${f1(ay - 64 * s)} H${f1(ax - 12 * s)} V${f1(ay - 72 * s)} Z" fill="#3a3a44" stroke="#1a1a20" stroke-width="${f1(1.5 * s)}"/>`) +
       obj("smith", ay, `<g transform="translate(${f1(sx)} ${f1(ay)}) scale(${s.toFixed(4)})">${helperSvg(SMITH, o, 0)}</g>` +
  // (the sparks: a burst off the struck bar, bright at the strike, the middle of each round, gone soon after)
    `<g opacity="0">` + lfoTimed(o, "fast", 0, `<animate attributeName="opacity" values="0;0;1;0;0" keyTimes="0;0.48;0.5;0.72;1" dur="0.9s" repeatCount="indefinite"/>`) +
    [...Array(9)].map((_, i) => { const a = -Math.PI * (0.1 + 0.8 * i / 8), r1 = 10 * s, r2 = (34 + 18 * (i % 3)) * s;
      return `<line x1="${f1(hitX + Math.cos(a) * r1)}" y1="${f1(hitY + Math.sin(a) * r1)}" x2="${f1(hitX + Math.cos(a) * r2)}" y2="${f1(hitY + Math.sin(a) * r2)}" stroke="${i % 2 ? "#ffd05a" : "#ff9a2e"}" stroke-width="${f1(2.5 * s)}" stroke-linecap="round"/>`; }).join("") +
    `<circle cx="${f1(hitX)}" cy="${f1(hitY)}" r="${f1(12 * s)}" fill="#fff0a0" opacity="0.8"/></g>`);
  // the dark: the room's light is the forge's
  // (a front object: over the friend wherever they stand)
  t += obj("dark", Infinity, `<rect x="${f1(c.x0 - pad)}" y="${f1(c.y0 - pad)}" width="${f1(c.W + 2 * pad)}" height="${f1(c.H + 2 * pad)}" fill="#0a0806" opacity="0.42"/>`);
  return t;
}
// The spiral staircase (ticket 92): a round tower room, cut away at the
// front as a doll's house is, its stone wall curving round behind, and in
// the middle a stone column the steps wind up round, turn after turn, out of
// sight; the front of each turn above the first is cut away with the wall,
// so the steps can be seen climbing the back. Its doors come out onto the
// steps at four heights (its own doors, set as a hallway's: spiralDoorAt),
// arrow slits let in beams of light, torches light the way, and its way out
// is a sign toward us. A hallway's stairs lead to the castle's first one.
function spiralGeom(c) {
  const { W, k, g } = c, Yf = g - 96 * k, R = W * 0.42, r0 = W * 0.065, sq = 0.22, th0 = 0.6 * Math.PI, rise = 44 * k;
  const lvl = th => Yf - rise * (th - th0);
  // (a point at radius r and angle th, at height y: the front of the circle lower on the picture)
  const pt = (r, th, y) => [r * Math.cos(th), y + r * sq * Math.sin(th)];
  return { Yf, R, r0, sq, th0, rise, lvl, pt };
}
// (ticket 268, Matthew: "the motion plan for the spiral stairs needs work. it should follow the center of the stairs,
// not the farthest edge") a walk on the stairs, in figX/figY terms: along the steps' middle line (halfway from the column
// to the wall), turning with them from the angle nearest where it starts to the one nearest where it goes, then there
function spiralPath(x0, y0, x1, y1) {
  const G = spiralGeom(stageC()), g = SCENE.ground, rm = (G.r0 + G.R) / 2;
  const on = th => { const [x, y] = G.pt(rm, th, G.lvl(th)); return [x, y - g]; };
  const angleOf = (x, y) => { let best = G.th0, bd = Infinity; for (let th = G.th0; th <= G.th0 + 4.6 * Math.PI; th += 0.02) { const [px, py] = on(th), d = Math.hypot(px - x, py - y); if (d < bd) { bd = d; best = th; } } return best; };
  const a = angleOf(x0, y0), b = angleOf(x1, y1), pts = [], dir = Math.sign(b - a) || 1;
  for (let th = a + dir * 0.18; dir * (b - th) > 0.12; th += dir * 0.18) pts.push(on(th));
  pts.push(on(b), [x1, y1]);
  return pts;
}
// (ticket 270) up the spiral staircase, where the friend's size is reckoned from: the floor straight below their place on
// the steps (the step's depth, as the turn's front or back), in figY terms; null on the floor, below the steps' foot
function spiralFloorY(x, y) {
  const G = spiralGeom(stageC()), g = SCENE.ground, k = stageC().k;
  if (g + y > G.Yf - 20 * k) return null;
  let best = G.th0, bd = Infinity, r = (G.r0 + G.R) / 2;
  for (let th = G.th0; th <= G.th0 + 4.6 * Math.PI; th += 0.02) { const [px, py] = G.pt(r, th, G.lvl(th)), d = Math.hypot(px - x, py - (g + y)); if (d < bd) { bd = d; best = th; } }
  return G.pt(r, best, G.Yf)[1] - g;
}
// where door j (from 0) comes out onto the steps: at the wall, at its step's height, smaller the farther back
const SPIRAL_DOORS = [1.12, 1.86, 2.95, 3.72].map(u => u * Math.PI);
function spiralDoorAt(c, j) {
  const G = spiralGeom(c), th = SPIRAL_DOORS[j % 4], [x, y] = G.pt(G.R * 0.98, th, G.lvl(th));
  return { x, y, k: c.k * (0.78 + 0.14 * Math.sin(th)) };
}
function spiralSvg(c, K) {
  const { x0, y0, W, H, k, pad, P, o, R: rnd } = c, G = spiralGeom(c), { Yf, R, r0, lvl, pt } = G;
  const tone = ROOM_TONE.spiral, mortar = mix(tone, "#000000", 0.35);
  let t = `<defs><radialGradient id="bg-glow"><stop offset="0" stop-color="#ffcf6b" stop-opacity="0.55"/><stop offset="1" stop-color="#ffcf6b" stop-opacity="0"/></radialGradient>` +
    `<linearGradient id="bg-column" x1="0" y1="0" x2="1" y2="0"><stop offset="0" stop-color="${mix(tone, "#000000", 0.35)}"/><stop offset="0.45" stop-color="${mix(tone, "#ffffff", 0.12)}"/><stop offset="1" stop-color="${mix(tone, "#000000", 0.45)}"/></linearGradient>` +
    `<linearGradient id="bg-slitbeam" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#fff3c4" stop-opacity="0.4"/><stop offset="1" stop-color="#fff3c4" stop-opacity="0"/></linearGradient></defs>`;
  // the wall curving round behind: courses bowing as a round wall's do (up above our eyes, down below), joints by turns
  t += `<rect x="${f1(x0 - pad)}" y="${f1(y0 - pad)}" width="${f1(W + 2 * pad)}" height="${f1(H + 2 * pad)}" fill="${mix(tone, "#000000", 0.12)}"/>`;
  const eye = Yf - 180 * k, edge = W * 0.5 + pad;
  for (let y = Yf + 40 * k, r = 0; y > y0 - pad; y -= 30 * k, r++) {
    const bow = (y - eye) * 0.12;
    t += `<path d="M${f1(-edge)} ${f1(y + bow)} Q0 ${f1(y - bow)} ${f1(edge)} ${f1(y + bow)}" fill="none" stroke="${mortar}" stroke-width="${f1(2 * k)}" opacity="0.7"/>`;
    for (let q = -8; q <= 8; q++) { const u = (q + (r % 2) * 0.5) / 8.5, x = edge * Math.sin(u * Math.PI / 2);
      if (Math.abs(u) > 1) continue;
      const yy = y + bow - 2 * bow * (1 - (x / edge) ** 2);
      t += `<line x1="${f1(x)}" y1="${f1(yy)}" x2="${f1(x)}" y2="${f1(yy - 30 * k)}" stroke="${mortar}" stroke-width="${f1(1.6 * k)}" opacity="0.55"/>`; }
  }
  // arrow slits high in the wall, their light slanting down; torches on the wall by the steps
  let slit = 0;   // (ticket 284: each slit and its beam a wall object)
  for (const [x, y] of [[-W * 0.3, Yf - 330 * k], [W * 0.28, Yf - 230 * k], [-W * 0.05, Yf - 470 * k]]) {
    const mSlit = t.length;
    t += `<rect x="${f1(x - 6 * k)}" y="${f1(y - 40 * k)}" width="${f1(12 * k)}" height="${f1(80 * k)}" rx="${f1(5 * k)}" fill="#cfe9ff" stroke="${mortar}" stroke-width="${f1(4 * k)}"/>` +
         `<polygon points="${f1(x - 6 * k)},${f1(y - 30 * k)} ${f1(x + 6 * k)},${f1(y - 30 * k)} ${f1(x + 90 * k)},${f1(y + 170 * k)} ${f1(x + 40 * k)},${f1(y + 190 * k)}" fill="url(#bg-slitbeam)"/>`;
    t = wallSince(t, mSlit, `arrow slit ${++slit}`);
  }
  // the floor, round as the tower is (the wall's circle, at the steps' foot), and on toward us where the
  // front is cut away; its flagstones radiating from the column, to the wall behind
  const Rw = R * 1.06, fy = Yf, fl = mix(tone, "#5a4a3a", 0.4);
  t += `<ellipse cx="0" cy="${f1(fy)}" rx="${f1(Rw)}" ry="${f1(Rw * G.sq)}" fill="${fl}"/><rect x="${f1(-Rw)}" y="${f1(fy)}" width="${f1(2 * Rw)}" height="${f1(c.y1 + pad - fy)}" fill="${fl}"/>`;
  for (let i = 0; i <= 8; i++) { const a = Math.PI + Math.PI * i / 8, [ex, ey] = pt(Rw, a, fy);
    t += `<line x1="0" y1="${f1(fy)}" x2="${f1(ex)}" y2="${f1(ey)}" stroke="${mortar}" stroke-width="${f1(1.5 * k)}" opacity="0.5"/>`; }
  for (const f of [0.45, 0.75]) t += `<path d="M${f1(-Rw * f)} ${f1(fy)} A${f1(Rw * f)} ${f1(Rw * f * G.sq)} 0 0 1 ${f1(Rw * f)} ${f1(fy)}" fill="none" stroke="${mortar}" stroke-width="${f1(1.5 * k)}" opacity="0.5"/>`;
  // (the cut edge of the tower's wall, at either side of the floor, down to us)
  for (const sd of [-1, 1]) t += `<rect x="${f1(sd > 0 ? Rw : -Rw - 30 * k)}" y="${f1(fy - 6 * k)}" width="${f1(30 * k)}" height="${f1(c.y1 + pad - fy)}" fill="${mix(tone, "#000000", 0.3)}"/>`;
  // the steps: wedges from the column out to the wall, each a step higher; the front of each turn above
  // the first cut away (as the wall is); those behind the column first, then the column, then those in front
  const step = Math.PI / 10, top = y0 - pad, back = [], front = [], overhead = [];
  for (let th = G.th0; lvl(th) > top - 40 * k; th += step) {
    const a = th, b = th + step, ya = lvl(a), yb = lvl(b), sn = Math.sin((a + b) / 2);
    // (ticket 268, Matthew: "We see the back half of a screw, and the front half is missing. let's draw it all, and if we
    // have to make certain parts transparent while we are occluded by them temporarily, we can") the upper turns' front
    // steps too, over everything (an object at Infinity), each marked: seen through where it covers the friend (draw)
    const upper = sn > 0.2 && ya < Yf - 40 * k;
    const shade = mix(tone, "#ffffff", 0.18 - 0.12 * Math.min(1, (Yf - ya) / (420 * k)));
    const tread = `<polygon points="${[pt(r0, a, ya), pt(R, a, ya), pt(R, b, ya), pt(r0, b, ya)].map(p => P(...p)).join(" ")}" fill="${shade}" stroke="${mortar}" stroke-width="${f1(1.2 * k)}"/>` +
      `<polygon points="${[pt(r0, b, ya), pt(R, b, ya), pt(R, b, yb), pt(r0, b, yb)].map(p => P(...p)).join(" ")}" fill="${mix(tone, "#000000", 0.25)}" stroke="${mortar}" stroke-width="${f1(1 * k)}"/>`;
    if (upper) overhead.push({ y: ya, s: `<g data-front-step="">${tread}</g>` });
    else (sn > 0 ? front : back).push({ y: ya, s: tread, low: th < Math.PI });
  }
  // (ticket 99: the steps behind the column climb the back wall, behind
  // anywhere one stands, so they are not an object; the column is one,
  // standing where its foot is drawn; the steps in front of it at the foot
  // of the stairs (the first half turn, from the floor) are one more, a row
  // at its middle: the floor under their outer edge halfway round them. The
  // cut edges of the turns above, high at the sides, are drawn before those
  // (higher, so first in the sort) and stay as they were.)
  t += back.map(x => x.s).join("");
  // (the column, from the floor up out of sight, and the torches on it, lighting the way up: they go with it)
  const torches = [Yf - 150 * k, Yf - 330 * k, Yf - 510 * k].map((y, i) => torchSvg(r0 * (i % 2 ? 0.6 : -0.6), y, k * 0.85, 20 + i, o)).join("");
  t += obj("column", Yf + 30 * k, `<rect x="${f1(-r0)}" y="${f1(top)}" width="${f1(2 * r0)}" height="${f1(Yf + 30 * k - top)}" fill="url(#bg-column)"/>` +
       [...Array(14)].map((_, i) => `<path d="M${f1(-r0)} ${f1(Yf - i * 44 * k)} Q0 ${f1(Yf - i * 44 * k + 6 * k)} ${f1(r0)} ${f1(Yf - i * 44 * k)}" fill="none" stroke="${mortar}" stroke-width="${f1(1.5 * k)}" opacity="0.6"/>`).join("") + torches);
  const ahead = front.sort((a, b) => a.y - b.y);
  t += ahead.filter(x => !x.low).map(x => x.s).join("") + obj("steps", pt(R, (G.th0 + Math.PI) / 2, Yf)[1], ahead.filter(x => x.low).map(x => x.s).join(""));
  // (ticket 268) the front of the turns above, nearest us, the higher ones first
  if (overhead.length) t += obj("steps, in front", Infinity, overhead.sort((a, b) => a.y - b.y).map(x => x.s).join(""));
  // the dark, deeper as it rises (a front object: over the friend wherever they stand)
  t += obj("dark", Infinity, `<linearGradient id="bg-towerdark" x1="0" y1="${f1(top)}" x2="0" y2="${f1(Yf)}" gradientUnits="userSpaceOnUse"><stop offset="0" stop-color="#05040a" stop-opacity="0.55"/><stop offset="1" stop-color="#05040a" stop-opacity="0.1"/></linearGradient>` +
       `<rect x="${f1(x0 - pad)}" y="${f1(top)}" width="${f1(W + 2 * pad)}" height="${f1(Yf - top + 60 * k)}" fill="url(#bg-towerdark)"/>`);
  return t;
}
// the king's room: a red carpet to a throne on a dais, banners in the
// owner's color, crossed swords and shields, a stained-glass window, a
// chandelier, suits of armor at the sides, a chest of gold; and through its
// open door, the hallway outside, torches going off along it
// (ticket 293) The king's room's thrones, up to six, across its dais, each its own size (fitted to the back wall, the
// more of them the closer, and smaller when they would touch) and in its color theme's (A, or B: the velvet the theme's
// carpet color, a crest in its first flag color); throneLayout says where each stands and how big it is (for the
// drawing and the seats alike)
function throneLayout(c, K) {
  const { k } = c, { floor, bw } = roomFrame(c), T = K.thrones || [], n = T.length, most = Math.max(1, ...T.map(t => t.size / 100));
  const gap = n > 1 ? Math.min(125 * k * most, (1.7 * bw - 112 * k * most) / (n - 1)) : 0, fit = n > 1 ? Math.min(1, gap / (118 * k * most)) : 1;
  return T.map((t, i) => ({ ...t, x: (i - (n - 1) / 2) * gap, s: fit * t.size / 100, floor, ty: floor - 42 * k }));
}
// one throne: tall-backed, gold, its velvet and crest in its theme's colors, a crown on top; at (0, floor), at its size
function throneSvg(c, velvet, crest) {
  const { k, P } = c, { floor } = roomFrame(c), gold = "#e8b923", red = velvet, ty = floor - 42 * k;
  let t = "";
  t += `<path d="M${P(-46 * k, ty)} V${f1(ty - 150 * k)} Q${P(-46 * k, ty - 200 * k)} ${P(0, ty - 214 * k)} Q${P(46 * k, ty - 200 * k)} ${P(46 * k, ty - 150 * k)} V${f1(ty)} Z" fill="${gold}" stroke="#9a7a12" stroke-width="${f1(3 * k)}"/>` +
       `<path d="M${P(-32 * k, ty - 40 * k)} V${f1(ty - 146 * k)} Q${P(-32 * k, ty - 184 * k)} ${P(0, ty - 194 * k)} Q${P(32 * k, ty - 184 * k)} ${P(32 * k, ty - 146 * k)} V${f1(ty - 40 * k)} Z" fill="${red}"/>` +
       [...Array(6)].map((_, j) => `<circle cx="${f1(-18 * k + (j % 2) * 36 * k)}" cy="${f1(ty - 70 * k - Math.floor(j / 2) * 34 * k)}" r="${f1(2.5 * k)}" fill="${gold}"/>`).join("") +
       `<path d="M${P(-22 * k, ty - 214 * k)} L${P(-22 * k, ty - 236 * k)} L${P(-11 * k, ty - 226 * k)} L${P(0, ty - 244 * k)} L${P(11 * k, ty - 226 * k)} L${P(22 * k, ty - 236 * k)} L${P(22 * k, ty - 214 * k)} Z" fill="${gold}" stroke="#9a7a12" stroke-width="${f1(2 * k)}"/>` +
       [-11, 0, 11].map(x => `<circle cx="${f1(x * k)}" cy="${f1(ty - 230 * k + (x ? 4 * k : -6 * k))}" r="${f1(3 * k)}" fill="${["#e53935", "#3b82f6", "#22c55e"][(x / 11 + 1) | 0]}"/>`).join("") +
       `<rect x="${f1(-56 * k)}" y="${f1(ty - 40 * k)}" width="${f1(112 * k)}" height="${f1(22 * k)}" rx="${f1(6 * k)}" fill="${red}" stroke="${gold}" stroke-width="${f1(3 * k)}"/>` +
       [-1, 1].map(e => `<rect x="${f1(e * 56 * k - (e < 0 ? 0 : 14 * k))}" y="${f1(ty - 74 * k)}" width="${f1(14 * k)}" height="${f1(74 * k)}" fill="${gold}" stroke="#9a7a12" stroke-width="${f1(2 * k)}"/>` +
         `<circle cx="${f1(e * 56 * k + (e < 0 ? 7 * k : -7 * k))}" cy="${f1(ty - 78 * k)}" r="${f1(10 * k)}" fill="${gold}" stroke="#9a7a12" stroke-width="${f1(2 * k)}"/>`).join("");
  t += `<circle cx="0" cy="${f1(ty - 172 * k)}" r="${f1(12 * k)}" fill="${crest}" stroke="${gold}" stroke-width="${f1(3 * k)}"/>`;
  return t;
}
function kingThrones(c, K, wags) {
  const { k } = c, L = throneLayout(c, K), floor = roomFrame(c).floor;
  if (!L.length) return "";
  const xs = L.map(t => t.x), half = Math.max(...L.map(t => t.s)) * 95 * k;
  // the dais, as wide as its thrones, its three steps
  let dais = "";
  for (let j = 0; j < 3; j++) { const w = Math.max(...xs) - Math.min(...xs) + 2 * half - j * 30 * k, y = floor - j * 14 * k;
    dais += `<rect x="${f1(-w / 2)}" y="${f1(y - 14 * k)}" width="${f1(w)}" height="${f1(14 * k)}" fill="${mix("#8a7f72", "#ffffff", 0.1 * j)}" stroke="#5a5048" stroke-width="${f1(1.5 * k)}"/>`; }
  let t = floorObj("dais", dais);
  L.forEach((th, i) => {
    const theme = th.theme === "B" ? K.themeB : K.themeA || K, s = th.s, at = inner => `<g transform="translate(${f1(th.x)} ${f1(floor)}) scale(${s.toFixed(3)}) translate(0 ${f1(-floor)})">${inner}</g>`;
    let svg = at(throneSvg(c, theme.carpet, theme.flagA));
    // its cat, on its seat: sitting, curled up asleep, or pacing to and fro along it (the cat room's own drawings)
    if (th.cat === "cat") {
      const W = wags[i], WC = { catFur: th.fur, catWhite: th.white, catEyes: th.eyes, catNose: "#f4a0b0" }, cs = 0.85 * k * s * th.catsize / 100;
      const sx = th.x, sy = floor - 82 * k * s, cat = (inner, dy = 0) => `<g transform="translate(${f1(sx)} ${f1(sy + dy)}) scale(${cs.toFixed(3)})">${inner}</g>`;
      if (th.pose === "sleep") svg += cat(sleepingCat(WC, W, i + 10));
      else if (th.pose === "walk") { const u = Math.max(8, 40 * k * s / cs), dur = (2 * u / 26).toFixed(2), b = `-${((i * 1.3) % +dur).toFixed(2)}s`;
        svg += cat(`<g><animateTransform attributeName="transform" type="translate" values="${f1(-u)} 0;${f1(u)} 0;${f1(-u)} 0" dur="${dur}s" begin="${b}" repeatCount="indefinite"/>` +
          `<g><animateTransform attributeName="transform" type="scale" values="1 1;-1 1" keyTimes="0;0.5" calcMode="discrete" dur="${dur}s" begin="${b}" repeatCount="indefinite"/>${walkingCat(WC, W, i + 10)}</g></g>`); }
      else svg += cat(shoulderCat(4, WC, W, true), 7 * cs);
    }
    t += obj(`throne ${i + 1}`, floor, svg);
  });
  return t;
}

function kingRoomSvg(c, K) {
  const { x0, x1, y1, W, k, R, P } = c, { floor, top, bw, vy, out } = roomFrame(c);
  const flag = K.flagA, gold = "#e8b923", red = "#a3122a";
  let t = roomShell(c, ROOM_TONE.king, {}) + castleFlagDefs(K, 12 * k);
  // the stained-glass window high in the middle, its panes in colors
  const wy = top + 30 * k, ww = 90 * k, wh = 120 * k;
  const mWindow = t.length;   // (ticket 284: the window, banners, trophies, torches and carpet: wall and floor objects)
  t += `<path d="M${P(-ww / 2, wy + wh)} V${f1(wy + ww / 2)} A${f1(ww / 2)} ${f1(ww / 2)} 0 0 1 ${P(ww / 2, wy + ww / 2)} V${f1(wy + wh)} Z" fill="#2a3a5a" stroke="#3a3530" stroke-width="${f1(8 * k)}"/>` +
       [...Array(12)].map((_, j) => { const cx = -ww / 2 + ww * ((j % 3) + 0.5) / 3, cy = wy + ww / 2 + (wh - ww / 2) * (Math.floor(j / 3) + 0.5) / 4;
         return `<rect x="${f1(cx - ww / 7)}" y="${f1(cy - (wh - ww / 2) / 9)}" width="${f1(ww * 2 / 7)}" height="${f1((wh - ww / 2) * 2 / 9)}" fill="${["#e53935", "#3b82f6", "#f2c12e", "#22c55e", "#9b59b6"][(j * 7) % 5]}" opacity="0.85"/>`; }).join("") +
       `<circle cx="0" cy="${f1(wy + ww / 2)}" r="${f1(ww / 4)}" fill="${flag}" opacity="0.9"/>`;
  t = wallSince(t, mWindow, "stained-glass window");
  // banners either side, the owner's color, a gold crown and the owner's letter
  for (const [i, sd] of [-1, 1].entries()) {
    const bx = sd * bw * 0.42, by = top + 20 * k, bwid = 70 * k, bh = 220 * k;
    const mBanner = t.length;
    t += `<rect x="${f1(bx - bwid / 2 - 6 * k)}" y="${f1(by - 6 * k)}" width="${f1(bwid + 12 * k)}" height="${f1(8 * k)}" rx="${f1(3 * k)}" fill="#5e3d22"/>` +
         `<path d="M${P(bx - bwid / 2, by)} H${f1(bx + bwid / 2)} V${f1(by + bh)} L${P(bx, by + bh - 30 * k)} L${P(bx - bwid / 2, by + bh)} Z" fill="${castleFlagFill(K, i, 12 * k)}" stroke="${gold}" stroke-width="${f1(4 * k)}"/>` +
         `<path d="M${P(bx - 20 * k, by + 70 * k)} L${P(bx - 20 * k, by + 46 * k)} L${P(bx - 10 * k, by + 58 * k)} L${P(bx, by + 40 * k)} L${P(bx + 10 * k, by + 58 * k)} L${P(bx + 20 * k, by + 46 * k)} L${P(bx + 20 * k, by + 70 * k)} Z" fill="${gold}" stroke="#7a5a0a" stroke-width="${f1(2.5 * k)}" stroke-linejoin="round"/>` +
         castleLetter(K, i, bx, by + 130 * k, 44 * k, "serif");
    t = wallSince(t, mBanner, sd < 0 ? "banner, left" : "banner, right");
  }
  // crossed swords and a shield on the wall, either side of the window
  for (const sd of [-1, 1]) {
    const sx = sd * bw * 0.2, sy = top + 120 * k;
    const mTrophy = t.length;
    t += [-1, 1].map(e => `<line x1="${f1(sx - e * 34 * k)}" y1="${f1(sy - 34 * k)}" x2="${f1(sx + e * 34 * k)}" y2="${f1(sy + 34 * k)}" stroke="#c8ccd6" stroke-width="${f1(5 * k)}"/>` +
           `<line x1="${f1(sx + e * 22 * k)}" y1="${f1(sy + 18 * k)}" x2="${f1(sx + e * 38 * k)}" y2="${f1(sy + 30 * k)}" stroke="${gold}" stroke-width="${f1(6 * k)}"/>`).join("") +
         `<path d="M${P(sx - 22 * k, sy - 22 * k)} H${f1(sx + 22 * k)} V${f1(sy + 6 * k)} Q${P(sx + 22 * k, sy + 30 * k)} ${P(sx, sy + 38 * k)} Q${P(sx - 22 * k, sy + 30 * k)} ${P(sx - 22 * k, sy + 6 * k)} Z" fill="${sd < 0 ? K.flagA : K.flagB}" stroke="${gold}" stroke-width="${f1(3 * k)}"/>`;
    t = wallSince(t, mTrophy, sd < 0 ? "crossed swords, left" : "crossed swords, right");
  }
  // the torches on the back wall (drawn with the wall, before all that stands in the room, so
  // a friend standing among its things is never under them: ticket 99)
  t += wallObj("torch, left", torchSvg(-bw * 0.66, floor - 300 * k, k, 0, c.o)) + wallObj("torch, right", torchSvg(bw * 0.66, floor - 300 * k, k, 1, c.o));
  // the carpet, gold-edged, from us to the dais, in its color theme's carpet color
  const mCarpet = t.length;
  const [ca, cy] = out(-60 * k, floor, y1 + 40 * k);
  t += `<polygon points="${P(-60 * k, floor - 2 * k)} ${P(60 * k, floor - 2 * k)} ${P(-ca, cy)} ${P(ca, cy)}" fill="${K.carpet}"/>` +
       `<polygon points="${P(-60 * k, floor - 2 * k)} ${P(60 * k, floor - 2 * k)} ${P(-ca, cy)} ${P(ca, cy)}" fill="none" stroke="${gold}" stroke-width="${f1(4 * k)}"/>`;
  t = floorSince(t, mCarpet, "carpet");
  // (ticket 293: the dais and its thrones, and their cats, are a part of their own: kingThrones)
  // the chandelier: a gold ring of candles on chains, the flames flickering
  // (hung as far back as it is drawn, at the back wall's size: its depth the floor's there)
  const chy = top - 10 * k;
  t += obj("chandelier", floor, chainSvg(0, top - 80 * k, 70 * k, 2 * k) + `<ellipse cx="0" cy="${f1(chy)}" rx="${f1(80 * k)}" ry="${f1(14 * k)}" fill="none" stroke="${gold}" stroke-width="${f1(6 * k)}"/>` +
       [...Array(7)].map((_, j) => { const a = Math.PI * (j + 0.5) / 7, x = -Math.cos(a) * 80 * k, y = chy + Math.sin(a) * 14 * k;
         return `<rect x="${f1(x - 3 * k)}" y="${f1(y - 18 * k)}" width="${f1(6 * k)}" height="${f1(18 * k)}" fill="#fff6dc"/>` +
                `<ellipse cx="${f1(x)}" cy="${f1(y - 24 * k)}" rx="${f1(3.5 * k)}" ry="${f1(6 * k)}" fill="#ffb53b"${j % 2 ? "" : ` ><animate attributeName="ry" values="${f1(6 * k)};${f1(4.5 * k)};${f1(6.5 * k)};${f1(6 * k)}" dur="${(0.8 + 0.1 * j).toFixed(1)}s" repeatCount="indefinite"/></ellipse`}${j % 2 ? "/" : ""}>`; }).join(""));
  // suits of armor against the back wall, beside the banners, each holding a halberd, plumes in the
  // owner's color (the side walls are the doors': castleWays)
  for (const sd of [-1, 1]) {
    const ax = sd * bw * 0.76, ay = floor, s = 1.0 * k, st = "#aab0bc", dk = "#6a707c";
    t += obj(`armor, ${sd < 0 ? "left" : "right"}`, ay, `<line x1="${f1(ax + sd * 30 * s)}" y1="${f1(ay - 200 * s)}" x2="${f1(ax + sd * 30 * s)}" y2="${f1(ay)}" stroke="#6e4a2c" stroke-width="${f1(4 * s)}"/>` +
         `<path d="M${P(ax + sd * 30 * s, ay - 200 * s)} q${f1(sd * 22 * s)} ${f1(10 * s)} ${f1(sd * 20 * s)} ${f1(34 * s)} q${f1(-sd * 10 * s)} ${f1(-10 * s)} ${f1(-sd * 20 * s)} ${f1(-12 * s)} Z" fill="${st}" stroke="${dk}" stroke-width="${f1(1.5 * s)}"/>` +
         `<rect x="${f1(ax - 14 * s)}" y="${f1(ay - 70 * s)}" width="${f1(10 * s)}" height="${f1(70 * s)}" fill="${st}" stroke="${dk}" stroke-width="${f1(1.5 * s)}"/><rect x="${f1(ax + 4 * s)}" y="${f1(ay - 70 * s)}" width="${f1(10 * s)}" height="${f1(70 * s)}" fill="${st}" stroke="${dk}" stroke-width="${f1(1.5 * s)}"/>` +
         `<path d="M${P(ax - 24 * s, ay - 140 * s)} Q${P(ax, ay - 150 * s)} ${P(ax + 24 * s, ay - 140 * s)} L${P(ax + 18 * s, ay - 70 * s)} L${P(ax - 18 * s, ay - 70 * s)} Z" fill="${st}" stroke="${dk}" stroke-width="${f1(1.5 * s)}"/>` +
         `<line x1="${f1(ax)}" y1="${f1(ay - 144 * s)}" x2="${f1(ax)}" y2="${f1(ay - 72 * s)}" stroke="${dk}" stroke-width="${f1(1.5 * s)}"/>` +
         [-1, 1].map(e => `<rect x="${f1(ax + e * 26 * s - 6 * s)}" y="${f1(ay - 140 * s)}" width="${f1(12 * s)}" height="${f1(56 * s)}" rx="${f1(5 * s)}" fill="${st}" stroke="${dk}" stroke-width="${f1(1.5 * s)}"/>`).join("") +
         `<path d="M${P(ax - 16 * s, ay - 150 * s)} V${f1(ay - 176 * s)} Q${P(ax, ay - 194 * s)} ${P(ax + 16 * s, ay - 176 * s)} V${f1(ay - 150 * s)} Z" fill="${st}" stroke="${dk}" stroke-width="${f1(1.5 * s)}"/>` +
         `<rect x="${f1(ax - 12 * s)}" y="${f1(ay - 170 * s)}" width="${f1(24 * s)}" height="${f1(4 * s)}" fill="#2a2a32"/>` +
         `<path d="M${P(ax, ay - 188 * s)} q${f1(-6 * s)} ${f1(-26 * s)} ${f1(14 * s)} ${f1(-36 * s)} q${f1(-6 * s)} ${f1(18 * s)} ${f1(-14 * s)} ${f1(36 * s)} Z" fill="${flag}"/>`);
  }
  // a chest of gold by the dais, its lid up, coins spilling
  const cx = bw * 0.62, cyb = floor + 50 * k;
  t += obj("chest of gold", cyb, `<rect x="${f1(cx - 40 * k)}" y="${f1(cyb - 40 * k)}" width="${f1(80 * k)}" height="${f1(40 * k)}" fill="#7a4f2a" stroke="#4a2e1a" stroke-width="${f1(2 * k)}"/>` +
       `<path d="M${P(cx - 40 * k, cyb - 40 * k)} L${P(cx - 36 * k, cyb - 76 * k)} L${P(cx + 44 * k, cyb - 76 * k)} L${P(cx + 40 * k, cyb - 40 * k)} Z" fill="#8b5e34" stroke="#4a2e1a" stroke-width="${f1(2 * k)}"/>` +
       [...Array(14)].map((_, j) => `<ellipse cx="${f1(cx - 32 * k + 64 * k * R(j, 970))}" cy="${f1(cyb - 40 * k - 6 * k * R(j, 971))}" rx="${f1(6 * k)}" ry="${f1(3 * k)}" fill="${gold}" stroke="#9a7a12" stroke-width="${f1(1 * k)}"/>`).join("") +
       // (ticket 304: the coins spilled on the floor are flat, no part of its box)
       `<g class="flat">${[...Array(6)].map((_, j) => `<ellipse cx="${f1(cx - 50 * k - 30 * k * R(j, 972))}" cy="${f1(cyb + 4 * k + 10 * k * R(j, 973))}" rx="${f1(6 * k)}" ry="${f1(3 * k)}" fill="${gold}" stroke="#9a7a12" stroke-width="${f1(1 * k)}"/>`).join("")}</g>` +
       [-1, 1].map(e => `<rect x="${f1(cx - 3 * k + e * 26 * k)}" y="${f1(cyb - 40 * k)}" width="${f1(6 * k)}" height="${f1(40 * k)}" fill="#c9a227"/>`).join(""));
  return t;
}
// The greenhouse: walls and roof of glass in white iron frames, the sky and
// the castle's trees beyond; vines climbing every post and hanging from the
// ribs; glass parrots flying by the vines near the roof; palms, a banana
// plant, monstera, ferns, birds of paradise, hibiscus bushes, bromeliads,
// pots; and in the middle, a fountain, its water arching and rippling
function greenhouseSvg(c, K) {
  const { x0, x1, y0, y1, W, H, k, pad, R, P } = c, { floor, top, bw, vy, out } = roomFrame(c);
  const iron = "#f4f6f2", ironD = "#c9cfc6", leaf = ["#2f7d32", "#3c8a3e", "#4a9a46", "#5aad52", "#27693a"];
  let t = `<defs><linearGradient id="bg-gsky" x1="0" y1="${f1(y0)}" x2="0" y2="${f1(floor)}" gradientUnits="userSpaceOnUse"><stop offset="0" stop-color="#7cc7f2"/><stop offset="1" stop-color="#dff3ff"/></linearGradient>` +
    `<linearGradient id="bg-glass" x1="0" y1="0" x2="1" y2="1"><stop offset="0" stop-color="#ffffff" stop-opacity="0.75"/><stop offset="0.5" stop-color="#bff0ff" stop-opacity="0.35"/><stop offset="1" stop-color="#7fd6ff" stop-opacity="0.55"/></linearGradient>` +
    `<radialGradient id="bg-glow"><stop offset="0" stop-color="#ffcf6b" stop-opacity="0.55"/><stop offset="1" stop-color="#ffcf6b" stop-opacity="0"/></radialGradient></defs>`;
  // beyond the glass: sky, clouds, the castle's own trees
  t += `<rect x="${f1(x0 - pad)}" y="${f1(y0 - pad)}" width="${f1(W + 2 * pad)}" height="${f1(floor - y0 + pad)}" fill="url(#bg-gsky)"/>`;
  for (let i = 0; i < 4; i++) { const cx = x0 + W * (0.1 + 0.25 * i), cy = y0 + H * (0.06 + 0.06 * R(i, 980));
    t += drifting([[-30, 6, 22], [0, -6, 30], [30, 4, 22]].map(([dx, dy, r]) => `<circle cx="${f1(cx + dx * k)}" cy="${f1(cy + dy * k)}" r="${f1(r * k)}" fill="#ffffff" opacity="0.9"/>`).join(""), i, 30 * k, c.o); }
  // (the castle's trees beyond, leafy as everywhere)
  t += `<defs>${leafyTreeDef("bg-gh-tree", 97)}</defs>`;
  for (let i = 0; i < 16; i++) { const x = x0 + W * i / 15, h = (150 + 90 * R(i, 981)) * k;
    t += `<g opacity="0.85">${use("bg-gh-tree", x, floor - 20 * k, (h / 330) * (R(i, 982) < 0.5 ? -1 : 1), h / 330)}</g>`; }
  t += `<rect x="${f1(x0 - pad)}" y="${f1(floor - 30 * k)}" width="${f1(W + 2 * pad)}" height="${f1(y1 + pad - floor + 30 * k)}" fill="#7cb85a"/>`;
  // the glass: back wall panes, side walls going off toward us, the roof's
  // panes in an arch; white iron frames between them
  t += `<rect x="${f1(-bw)}" y="${f1(top)}" width="${f1(2 * bw)}" height="${f1(floor - top)}" fill="url(#bg-glass)" opacity="0.5"/>`;
  for (let i = 0; i <= 8; i++) { const x = -bw + 2 * bw * i / 8; t += `<line x1="${f1(x)}" y1="${f1(top)}" x2="${f1(x)}" y2="${f1(floor)}" stroke="${iron}" stroke-width="${f1(i % 4 ? 3 * k : 7 * k)}"/>`; }
  for (const f of [0.33, 0.66]) t += `<line x1="${f1(-bw)}" y1="${f1(top + (floor - top) * f)}" x2="${f1(bw)}" y2="${f1(top + (floor - top) * f)}" stroke="${iron}" stroke-width="${f1(3 * k)}"/>`;
  for (const sd of [-1, 1]) {
    const [ax, ay] = out(sd * bw, top, y0 - pad), [bx, by] = out(sd * bw, floor, y1 + pad);
    t += `<polygon points="${P(sd * bw, top)} ${P(ax, ay)} ${P(sd * (W + pad), y0 - pad)} ${P(sd * (W + pad), y1 + pad)} ${P(bx, by)} ${P(sd * bw, floor)}" fill="url(#bg-glass)" opacity="0.45"/>`;
    for (let j = 0; j < 5; j++) { const x = sd * (bw + (W * 0.6 + pad - bw) * j / 5), f = Math.abs(x) / bw;
      t += `<line x1="${f1(x)}" y1="${f1(vy + (top - vy) * f)}" x2="${f1(x)}" y2="${f1(vy + (floor - vy) * f)}" stroke="${iron}" stroke-width="${f1((4 + 3 * j) * k)}"/>`; }
    for (const f of [0, 0.5]) { const y = top + (floor - top) * f, [ex, ey] = out(sd * bw, y, y < vy ? y0 - pad : y1 + pad);
      t += `<line x1="${f1(sd * bw)}" y1="${f1(y)}" x2="${f1(ex)}" y2="${f1(ey)}" stroke="${iron}" stroke-width="${f1(5 * k)}"/>`; }
    // the glass's shine
    t += `<line x1="${f1(sd * bw * 1.25)}" y1="${f1(top - 20 * k)}" x2="${f1(sd * bw * 1.6)}" y2="${f1(floor - 20 * k)}" stroke="#ffffff" stroke-width="${f1(10 * k)}" opacity="0.25"/>`;
    // a stone wall up each side, the glass above it, for the doors to stand in (a castle of many
    // rooms has more archways than the back wall holds: castleWays); its courses run toward us,
    // its joints by turns, each in the wall's perspective, ending on its edges; a coping on top
    { const yw = floor - 0.7 * (floor - top), Xe = sd * (W + pad), fe = (W + pad) / bw, at = (x, y) => vy + (y - vy) * Math.abs(x) / bw;
      t += `<polygon points="${P(sd * bw, floor)} ${P(sd * bw, yw)} ${P(Xe, at(Xe, yw))} ${P(Xe, at(Xe, floor))}" fill="#c8b59a"/>`;
      const rows = 9, yr = m => floor - (floor - yw) * m / rows;
      for (let m = 1; m < rows; m++) t += `<line x1="${f1(sd * bw)}" y1="${f1(yr(m))}" x2="${f1(Xe)}" y2="${f1(at(Xe, yr(m)))}" stroke="#a08b6e" stroke-width="${f1(1.4 * k)}"/>`;
      for (let m = 0; m < rows; m++) for (let q = 1 + (m % 2) * 0.5; ; q++) { const x = sd * bw * Math.pow(1.14, q); if (Math.abs(x) >= W + pad) break;
        t += `<line x1="${f1(x)}" y1="${f1(at(x, yr(m)))}" x2="${f1(x)}" y2="${f1(at(x, yr(m + 1)))}" stroke="#a08b6e" stroke-width="${f1(1.2 * k)}"/>`; }
      t += `<line x1="${f1(sd * bw)}" y1="${f1(yw)}" x2="${f1(Xe)}" y2="${f1(at(Xe, yw))}" stroke="#e2d6c0" stroke-width="${f1(7 * k)}"/>` +
           `<line x1="${f1(sd * bw)}" y1="${f1(yw)}" x2="${f1(sd * bw)}" y2="${f1(floor)}" stroke="#a08b6e" stroke-width="${f1(2 * k)}"/>`; }
  }
  // the roof: arched ribs of iron over glass
  t += `<polygon points="${P(-bw, top)} ${P(bw, top)} ${P(...out(bw, top, y0 - pad))} ${P(...out(-bw, top, y0 - pad))}" fill="url(#bg-glass)" opacity="0.35"/>`;
  for (let j = -3; j <= 3; j++) t += `<path d="M${P(-bw - 40 * k, top + 10 * k)} Q${P(j * bw / 3, y0 - 120 * k)} ${P(bw + 40 * k, top + 10 * k)}" fill="none" stroke="${iron}" stroke-width="${f1(4 * k)}" opacity="${j ? 0.7 : 1}"/>`;
  t += `<line x1="${f1(-bw)}" y1="${f1(top)}" x2="${f1(bw)}" y2="${f1(top)}" stroke="${iron}" stroke-width="${f1(8 * k)}"/>`;
  // the floor: warm stone tiles, a mossy path round the fountain
  t += `<polygon points="${P(-bw, floor)} ${P(bw, floor)} ${P(...out(bw, floor, y1 + pad))} ${P(...out(-bw, floor, y1 + pad))}" fill="#c9b79a"/>`;
  for (let i = -6; i <= 6; i++) { const [ex, ey] = out(i * bw / 6, floor, y1 + pad); t += `<line x1="${f1(i * bw / 6)}" y1="${f1(floor)}" x2="${f1(ex)}" y2="${f1(ey)}" stroke="#a8977c" stroke-width="${f1(2 * k)}"/>`; }
  t += floorRows(c, "#a8977c", 0.09);
  const mMoss = t.length;   // (ticket 284: the moss and the petals floor objects, each vine a wall object)
  for (let i = 0; i < 26; i++) { const u = R(i, 983), x = (R(i, 984) - 0.5) * 2.6 * bw * (1 + u), y = floor + (y1 - floor) * u * u + 8 * k;
    t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1((8 + 16 * u) * k)}" ry="${f1((3 + 6 * u) * k)}" fill="#6f9a4a" opacity="0.5"/>`; }
  t = floorSince(t, mMoss, "moss");
  // petals fallen on the floor (flat: drawn with the floor, before all that stands on it, ticket 99)
  const mPetals = t.length;
  for (let i = 0; i < 20; i++) t += `<ellipse cx="${f1((R(i, 994) - 0.5) * W * 0.9)}" cy="${f1(floor + (40 + 200 * R(i, 995)) * k)}" rx="${f1(3 * k)}" ry="${f1(2 * k)}" fill="${["#f472b6", "#fb7185", "#e11d48"][i % 3]}" opacity="0.8"/>`;
  t = floorSince(t, mPetals, "fallen petals");
  // vines up every back post and along the top, leaves and flowers on them
  const vine = (x, ya, yb, seed) => {
    let d = `M${P(x, yb)}`, lv = "";
    for (let j = 1; j <= 10; j++) { const y = yb + (ya - yb) * j / 10, dx = Math.sin(j * 1.3 + seed) * 10 * k; d += ` Q${P(x + dx * 2, y + (yb - ya) / 20)} ${P(x + dx, y)}`;
      for (const e of [-1, 1]) { const lx = x + dx + e * 9 * k, ly = y + e * 4 * k, rot = e * 40 + 20 * Math.sin(j + seed);
        lv += `<ellipse cx="${f1(lx)}" cy="${f1(ly)}" rx="${f1(8 * k)}" ry="${f1(4 * k)}" transform="rotate(${f1(rot)} ${f1(lx)} ${f1(ly)})" fill="${leaf[(j + seed) % 5]}"/>`; }
      if ((j + seed) % 4 === 0) lv += `<circle cx="${f1(x + dx)}" cy="${f1(y)}" r="${f1(4 * k)}" fill="${["#f472b6", "#c084fc", "#fb7185"][(j + seed) % 3]}"/><circle cx="${f1(x + dx)}" cy="${f1(y)}" r="${f1(1.5 * k)}" fill="#fde047"/>`;
    }
    return drawnBy("vine", `<path d="${d}" fill="none" stroke="#3f6b2a" stroke-width="${f1(3 * k)}"/>` + lv);
  };
  for (let i = 0; i <= 8; i++) t += wallObj(`vine ${i + 1}`, vine(-bw + 2 * bw * i / 8, top + 10 * k, floor, i * 3));
  for (let i = 0; i < 9; i++) {   // vines hanging from the roof's edge
    const x = -bw + 2 * bw * (i + 0.5) / 9, len = (50 + 70 * R(i, 985)) * k;
    let lv = `<path d="M${P(x, top)} Q${P(x + 10 * k, top + len / 2)} ${P(x, top + len)}" fill="none" stroke="#3f6b2a" stroke-width="${f1(2 * k)}"/>`;
    for (let j = 1; j < 6; j++) { const y = top + len * j / 6, lx = x + 6 * k * Math.sin(j * 1.7); lv += `<ellipse cx="${f1(lx)}" cy="${f1(y)}" rx="${f1(7 * k)}" ry="${f1(3.5 * k)}" transform="rotate(${j % 2 ? 35 : -35} ${f1(lx)} ${f1(y)})" fill="${leaf[(i + j) % 5]}"/>`; }
    t += wallObj(`hanging vine ${i + 1}`, lv);
  }
  // glass parrots flying by the vines near the roof, each on its own slow loop
  const parrot = (x, y, s, sd, col, i) => `<g><animateTransform attributeName="transform" type="translate" values="0 0;${f1(sd * 30 * k)} ${f1(-12 * k)};${f1(sd * 60 * k)} 0;${f1(sd * 30 * k)} ${f1(10 * k)};0 0" dur="${7 + i * 2}s" repeatCount="indefinite"/>` +
    `<g transform="translate(${f1(x)} ${f1(y)}) scale(${(sd * s).toFixed(3)} ${s.toFixed(3)})" opacity="0.8">` +
    `<path d="M-30 6 L-62 22 L-58 12 L-66 8 L-34 0 Z" fill="${col}" fill-opacity="0.45" stroke="#ffffff" stroke-width="1.5"/>` +
    `<ellipse cx="0" cy="0" rx="30" ry="14" fill="${col}" fill-opacity="0.45" stroke="#ffffff" stroke-width="1.5"/>` +
    `<path d="M-8 -6 Q-2 -40 22 -34 Q10 -14 6 -2 Z" fill="${col}" fill-opacity="0.35" stroke="#ffffff" stroke-width="1.5">${lfoTimed(c.o, "fast", (x * 0.0013 + 0.5) % 1, `<animateTransform attributeName="transform" type="rotate" values="0 0 0;-18 0 0;0 0 0" dur="0.8s" repeatCount="indefinite"/>`)}</path>` +
    `<circle cx="28" cy="-8" r="11" fill="${col}" fill-opacity="0.5" stroke="#ffffff" stroke-width="1.5"/>` +
    `<path d="M37 -10 Q48 -8 42 2 Q38 -2 36 -4 Z" fill="#ffe9a8" fill-opacity="0.85" stroke="#ffffff" stroke-width="1"/>` +
    `<circle cx="31" cy="-11" r="2.5" fill="#1f2937"/><circle cx="32" cy="-12" r="1" fill="#ffffff"/>` +
    `<path d="M-20 -4 Q0 -12 20 -6" fill="none" stroke="#ffffff" stroke-width="2" opacity="0.8"/></g></g>`;
  // (ticket 99: flying by the back wall, at the depth of the floor under them there)
  t += obj("parrots", floor, parrot(-bw * 0.55, top + 60 * k, 1.1 * k, 1, "#22d3ee", 0) + parrot(bw * 0.45, top + 40 * k, 1.0 * k, -1, "#a3e635", 1) + parrot(-bw * 0.05, top + 110 * k, 0.8 * k, 1, "#f472b6", 2));
  // a palm: a curving trunk of stacked segments (palmTrunkSvg, ticket 257), fronds of leaflets arching out
  const palm = (x, base, h, sd, seed) => {
    const trunk = palmTrunkSvg(x, base, h, sd, k, j => R(seed + j, 985));
    let s2 = trunk.svg;
    const [tx, ty] = trunk.top;
    for (let f = 0; f < 7; f++) {
      const a = -Math.PI + (f + 0.5) * Math.PI / 7 + (R(seed + f, 986) - 0.5) * 0.3, len = (90 + 40 * R(seed + f, 987)) * k;
      const ex = tx + Math.cos(a) * len, ey = ty + Math.sin(a) * len * 0.6 + len * 0.35, mx = tx + Math.cos(a) * len * 0.5, my = ty + Math.sin(a) * len * 0.7 - 10 * k;
      s2 += `<path d="M${P(tx, ty)} Q${P(mx, my)} ${P(ex, ey)}" fill="none" stroke="#2f6e2a" stroke-width="${f1(3 * k)}"/>`;
      for (let j = 1; j < 9; j++) { const u = j / 9, qx = (1 - u) * (1 - u) * tx + 2 * u * (1 - u) * mx + u * u * ex, qy = (1 - u) * (1 - u) * ty + 2 * u * (1 - u) * my + u * u * ey;
        for (const e of [-1, 1]) s2 += `<line x1="${f1(qx)}" y1="${f1(qy)}" x2="${f1(qx + e * 6 * k + Math.cos(a) * 8 * k)}" y2="${f1(qy + 14 * k * (1 - u * 0.5))}" stroke="${leaf[(f + j) % 5]}" stroke-width="${f1(3.5 * k)}" stroke-linecap="round"/>`; }
    }
    return drawnBy("palm", s2 + `<circle cx="${f1(tx)}" cy="${f1(ty + 6 * k)}" r="${f1(6 * k)}" fill="#6b4a2b"/><circle cx="${f1(tx + 8 * k)}" cy="${f1(ty + 8 * k)}" r="${f1(5 * k)}" fill="#6b4a2b"/>`);
  };
  // a monstera leaf: a big heart with slits and holes
  // (its slits cut in from the leaf's own edge: drawn from past it, kept inside the leaf)
  const MONSTERA_LEAF = "M0 0 C-30 -10 -44 -50 -20 -70 C-8 -80 0 -72 0 -66 C0 -72 8 -80 20 -70 C44 -50 30 -10 0 0 Z";
  const monstera = (x, y, s, rot, col) => `<g data-proc="monstera" transform="translate(${f1(x)} ${f1(y)}) rotate(${f1(rot)}) scale(${s.toFixed(3)})">` +
    `<path d="${MONSTERA_LEAF}" fill="${col}"/>` +
    `<path d="M0 0 V-64" stroke="#1f5a22" stroke-width="2"/>` +
    `<clipPath id="bg-monstera-leaf"><path d="${MONSTERA_LEAF}"/></clipPath><g clip-path="url(#bg-monstera-leaf)">` +
    [[-1, -20], [-1, -38], [1, -26], [1, -46]].map(([e, yy]) => `<path d="M${e * 44} ${yy - 2} L${e * 8} ${yy + 6}" stroke="#cfe8c0" stroke-width="3" stroke-linecap="round" opacity="0.9"/>`).join("") + `</g>` +
    `<ellipse cx="-10" cy="-50" rx="3" ry="5" fill="#cfe8c0"/><ellipse cx="12" cy="-36" rx="3" ry="5" fill="#cfe8c0"/></g>`;
  // a fern: arching fronds
  const fern = (x, y, s, seed) => [...Array(7)].map((_, f) => { const a = -Math.PI / 2 + (f - 3) * 0.32, len = (50 + 20 * R(seed + f, 988)) * s;
    const ex = x + Math.cos(a) * len, ey = y + Math.sin(a) * len * 0.7, mx = x + Math.cos(a) * len * 0.6, my = y + Math.sin(a) * len - 10 * s;
    let fr = `<path d="M${P(x, y)} Q${P(mx, my)} ${P(ex, ey)}" fill="none" stroke="#2f7d32" stroke-width="${f1(2 * s)}"/>`;
    for (let j = 1; j < 8; j++) { const u = j / 8, qx = (1 - u) * (1 - u) * x + 2 * u * (1 - u) * mx + u * u * ex, qy = (1 - u) * (1 - u) * y + 2 * u * (1 - u) * my + u * u * ey;
      fr += `<ellipse cx="${f1(qx)}" cy="${f1(qy)}" rx="${f1(6 * s * (1 - u * 0.6))}" ry="${f1(2.2 * s)}" transform="rotate(${f1(a * 57.3 + 90)} ${f1(qx)} ${f1(qy)})" fill="${leaf[(f + j) % 5]}"/>`; }
    return drawnBy("fern", fr); }).join("");
  // a hibiscus bush: a mound of leaves, red and pink flowers with gold centers
  const hibiscus = (x, y, s, seed) => {
    let b = `<ellipse cx="${f1(x)}" cy="${f1(y - 26 * s)}" rx="${f1(54 * s)}" ry="${f1(32 * s)}" fill="#2f7d32"/>`;
    for (let i = 0; i < 18; i++) b += `<ellipse cx="${f1(x + (R(seed + i, 989) - 0.5) * 100 * s)}" cy="${f1(y - 10 * s - 50 * s * R(seed + i, 990))}" rx="${f1(10 * s)}" ry="${f1(5 * s)}" transform="rotate(${f1(R(seed + i, 991) * 180)} ${f1(x + (R(seed + i, 989) - 0.5) * 100 * s)} ${f1(y - 10 * s - 50 * s * R(seed + i, 990))})" fill="${leaf[i % 5]}"/>`;
    for (let i = 0; i < 6; i++) { const fx = x + (R(seed + i, 992) - 0.5) * 90 * s, fy = y - 14 * s - 44 * s * R(seed + i, 993), col = ["#e11d48", "#f43f5e", "#fb7185"][i % 3];
      b += [0, 1, 2, 3, 4].map(p => { const a = p * 72 * Math.PI / 180; return `<ellipse cx="${f1(fx + Math.cos(a) * 6 * s)}" cy="${f1(fy + Math.sin(a) * 6 * s)}" rx="${f1(6 * s)}" ry="${f1(4.5 * s)}" transform="rotate(${p * 72} ${f1(fx + Math.cos(a) * 6 * s)} ${f1(fy + Math.sin(a) * 6 * s)})" fill="${col}"/>`; }).join("") +
           `<circle cx="${f1(fx)}" cy="${f1(fy)}" r="${f1(2.5 * s)}" fill="#fde047"/>`; }
    return drawnBy("hibiscus", b);
  };
  // a bird of paradise: a stalk, a beak of orange and blue
  const paradise = (x, y, s) => drawnBy("paradise", `<path d="M${P(x, y)} V${f1(y - 80 * s)}" stroke="#2f6e2a" stroke-width="${f1(3 * s)}"/>` +
    `<path d="M${P(x, y - 80 * s)} L${P(x + 34 * s, y - 86 * s)} L${P(x + 6 * s, y - 74 * s)} Z" fill="#1d4ed8"/>` +
    [0, 1, 2].map(j => `<path d="M${P(x + 8 * s, y - 82 * s)} L${P(x + 14 * s + j * 6 * s, y - 112 * s + j * 6 * s)} L${P(x + 14 * s, y - 84 * s)} Z" fill="${j ? "#f97316" : "#fb923c"}"/>`).join("") +
    `<path d="M${P(x, y - 30 * s)} Q${P(x - 30 * s, y - 60 * s)} ${P(x - 40 * s, y - 40 * s)} Q${P(x - 20 * s, y - 34 * s)} ${P(x, y - 30 * s)} Z" fill="#3c8a3e"/>`);
  // a pot, terracotta, its rim lighter
  const pot = (x, y, s) => `<path d="M${P(x - 26 * s, y - 40 * s)} L${P(x - 20 * s, y)} L${P(x + 20 * s, y)} L${P(x + 26 * s, y - 40 * s)} Z" fill="#c2652f"/>` +
    `<rect x="${f1(x - 30 * s)}" y="${f1(y - 48 * s)}" width="${f1(60 * s)}" height="${f1(10 * s)}" rx="${f1(3 * s)}" fill="#d97c45"/>`;
  // the back of the room: banana plant, palms, ferns along the glass
  // (ticket 99: each plant an object at its foot; the row of ferns one, at theirs)
  t += obj("palm, left", floor + 20 * k, palm(-bw * 0.8, floor + 20 * k, 260 * k, 1, 10)) + obj("palm, right", floor + 30 * k, palm(bw * 0.86, floor + 30 * k, 290 * k, -1, 30));
  let ferns = "";
  for (let i = 0; i < 6; i++) ferns += fern(-bw + 2 * bw * (i + 0.5) / 6, floor + 6 * k, 1.1 * k, i * 5);
  t += obj("ferns", floor + 6 * k, ferns);
  t += obj("monstera, left", floor + 14 * k, monstera(-bw * 0.35, floor + 10 * k, 1.3 * k, -25, "#2f7d32") + monstera(-bw * 0.28, floor + 14 * k, 1.1 * k, 15, "#3c8a3e")) +
       obj("monstera, right", floor + 16 * k, monstera(bw * 0.4, floor + 12 * k, 1.25 * k, 20, "#27693a") + monstera(bw * 0.33, floor + 16 * k, 1.0 * k, -12, "#3c8a3e"));
  t += obj("bird of paradise, left", floor + 10 * k, paradise(-bw * 0.12, floor + 10 * k, 1.1 * k)) + obj("bird of paradise, right", floor + 8 * k, paradise(bw * 0.15, floor + 8 * k, 1.0 * k));
  // bromeliads in pots along the front, pink hearts
  for (const [x, sd] of [[-W * 0.4, 1], [W * 0.42, -1]]) {
    const y = floor + 150 * k;
    t += obj(`bromeliad, ${x < 0 ? "left" : "right"}`, y, pot(x, y, 1.3 * k) + [...Array(9)].map((_, j) => { const a = -Math.PI / 2 + (j - 4) * 0.3; return `<path d="M${P(x, y - 50 * k)} Q${P(x + Math.cos(a) * 30 * k, y - 50 * k + Math.sin(a) * 50 * k)} ${P(x + Math.cos(a) * 56 * k, y - 50 * k + Math.sin(a) * 40 * k)}" fill="none" stroke="${leaf[j % 5]}" stroke-width="${f1(7 * k)}" stroke-linecap="round"/>`; }).join("") +
         `<path d="M${P(x - 10 * k, y - 50 * k)} L${P(x, y - 90 * k)} L${P(x + 10 * k, y - 50 * k)} Z" fill="#ec4899"/><path d="M${P(x - 4 * k, y - 60 * k)} L${P(x + 14 * k, y - 82 * k)} L${P(x + 8 * k, y - 56 * k)} Z" fill="#f472b6"/>`);
  }
  t += obj("hibiscus, left", floor + 70 * k, hibiscus(-W * 0.3, floor + 70 * k, 1.3 * k, 40)) + obj("hibiscus, right", floor + 74 * k, hibiscus(W * 0.3, floor + 74 * k, 1.35 * k, 60));
  // the fountain: a wide basin of stone, a pedestal, a bowl, a fish spouting
  // at the top; water arching from the bowl's lip into the basin, rings
  // spreading on the water (one object, standing at its base, fb: ticket 99)
  const fx = 0, fb = floor + 120 * k, brx = 150 * k, bry = 36 * k;
  let ft = `<ellipse cx="${f1(fx)}" cy="${f1(fb + 10 * k)}" rx="${f1(brx + 16 * k)}" ry="${f1(bry + 10 * k)}" fill="#000000" opacity="0.15"/>` +
       `<path d="M${P(fx - brx, fb - 10 * k)} V${f1(fb + 14 * k)} Q${P(fx, fb + bry * 1.6 + 14 * k)} ${P(fx + brx, fb + 14 * k)} V${f1(fb - 10 * k)} Z" fill="#b8b2a6"/>` +
       `<ellipse cx="${f1(fx)}" cy="${f1(fb - 10 * k)}" rx="${f1(brx)}" ry="${f1(bry)}" fill="#d6d0c4" stroke="#9a948a" stroke-width="${f1(3 * k)}"/>` +
       `<ellipse cx="${f1(fx)}" cy="${f1(fb - 8 * k)}" rx="${f1(brx - 14 * k)}" ry="${f1(bry - 8 * k)}" fill="#5aaee0"/>` +
       `<ellipse cx="${f1(fx - 30 * k)}" cy="${f1(fb - 14 * k)}" rx="${f1(40 * k)}" ry="${f1(6 * k)}" fill="#ffffff" opacity="0.3"/>`;
  for (let j = 0; j < 3; j++) ft += `<ellipse cx="${f1(fx)}" cy="${f1(fb - 8 * k)}" rx="${f1(20 * k)}" ry="${f1(5 * k)}" fill="none" stroke="#e0f4ff" stroke-width="${f1(2 * k)}">` +
    `<animate attributeName="rx" values="${f1(20 * k)};${f1(brx - 20 * k)}" dur="3s" begin="${j}s" repeatCount="indefinite"/><animate attributeName="ry" values="${f1(5 * k)};${f1(bry - 12 * k)}" dur="3s" begin="${j}s" repeatCount="indefinite"/>` +
    `<animate attributeName="opacity" values="0.9;0" dur="3s" begin="${j}s" repeatCount="indefinite"/></ellipse>`;
  const bowlY = fb - 110 * k;
  ft += `<path d="M${P(fx - 16 * k, fb - 10 * k)} Q${P(fx - 8 * k, fb - 60 * k)} ${P(fx - 12 * k, bowlY + 6 * k)} H${f1(fx + 12 * k)} Q${P(fx + 8 * k, fb - 60 * k)} ${P(fx + 16 * k, fb - 10 * k)} Z" fill="#c9c3b6" stroke="#9a948a" stroke-width="${f1(2 * k)}"/>` +
       `<path d="M${P(fx - 60 * k, bowlY)} Q${P(fx, bowlY + 40 * k)} ${P(fx + 60 * k, bowlY)} Z" fill="#c9c3b6" stroke="#9a948a" stroke-width="${f1(2 * k)}"/>` +
       `<ellipse cx="${f1(fx)}" cy="${f1(bowlY)}" rx="${f1(60 * k)}" ry="${f1(12 * k)}" fill="#d6d0c4" stroke="#9a948a" stroke-width="${f1(2 * k)}"/>` +
       `<ellipse cx="${f1(fx)}" cy="${f1(bowlY + 1 * k)}" rx="${f1(50 * k)}" ry="${f1(8 * k)}" fill="#6cc0ec"/>`;
  // the water: arcs from the bowl's lip down to the basin, running
  for (const [e, dl] of [[-1, 0], [1, 0.4], [-0.5, 0.2], [0.5, 0.6]]) {
    const sx = fx + e * 56 * k, ex = fx + e * 120 * k, ey = fb - 14 * k;
    ft += `<path d="M${P(sx, bowlY + 2 * k)} Q${P(fx + e * 100 * k, bowlY - 10 * k)} ${P(ex, ey)}" fill="none" stroke="#bfe6ff" stroke-width="${f1(4 * k)}" stroke-dasharray="${f1(10 * k)} ${f1(8 * k)}" opacity="0.9">` +
         `<animate attributeName="stroke-dashoffset" values="${f1(36 * k)};0" dur="0.6s" begin="${dl}s" repeatCount="indefinite"/></path>`;
  }
  // the fish at the top, spouting a jet up that falls back into the bowl
  ft += `<path d="M${P(fx - 14 * k, bowlY - 4 * k)} Q${P(fx, bowlY - 40 * k)} ${P(fx + 14 * k, bowlY - 4 * k)} Z" fill="#e8b923" stroke="#9a7a12" stroke-width="${f1(1.5 * k)}"/>` +
       `<circle cx="${f1(fx + 5 * k)}" cy="${f1(bowlY - 22 * k)}" r="${f1(2 * k)}" fill="#1f2937"/>` +
       `<path d="M${P(fx, bowlY - 34 * k)} Q${P(fx + 6 * k, bowlY - 90 * k)} ${P(fx + 26 * k, bowlY - 4 * k)}" fill="none" stroke="#bfe6ff" stroke-width="${f1(3.5 * k)}" stroke-dasharray="${f1(8 * k)} ${f1(6 * k)}">` +
       `<animate attributeName="stroke-dashoffset" values="${f1(28 * k)};0" dur="0.5s" repeatCount="indefinite"/></path>`;
  // (ticket 441) its footprint, unseen: where the basin's wall meets the floor, as wide and deep as the basin (solidsHere)
  ft += `<ellipse data-footprint="1" cx="${f1(fx)}" cy="${f1(fb + 14 * k)}" rx="${f1(brx)}" ry="${f1(bry)}" fill="none" stroke="none"/>`;
  t += obj("fountain", fb - 10 * k, ft);   // (at its pedestal's foot: the friend's first spot, 320, stays in front of it)
  // a watering can and a bench at the sides (the petals fallen on the floor: with the moss, above)
  t += obj("watering can", floor + 190 * k, `<path d="M${P(-W * 0.2, floor + 190 * k)} h${f1(40 * k)} v${f1(-30 * k)} h${f1(-40 * k)} Z" fill="#5b8fb9"/><path d="M${P(-W * 0.2 + 40 * k, floor + 170 * k)} l${f1(30 * k)} ${f1(-20 * k)}" stroke="#5b8fb9" stroke-width="${f1(5 * k)}"/>` +
       `<path d="M${P(-W * 0.2 + 6 * k, floor + 160 * k)} q${f1(14 * k)} ${f1(-20 * k)} ${f1(28 * k)} 0" fill="none" stroke="#4a7aa0" stroke-width="${f1(4 * k)}"/>`);
  return t;
}

// the ways out of a room: the castle (out), and its other rooms; a trap
// room has its one doorway; a dungeon only its ladder up to the trap room
// the hallway's stretches, from its opening to its far end: few enough that
// the far end, the spiral stairs, stands above the friend's head
const HALL_STRETCHES = 4;
// the archway at the hallway's far end: its outline, and its box
function hallFarArch(c) {
  const { k } = c, { floor, top, bw, vy } = roomFrame(c), f = 1 / (1 + HALL_STRETCHES * 0.8), Y = y => vy + (y - vy) * f;
  const fl = Y(floor), tp = Y(top), x1 = bw * f * 0.55, x0 = -x1, r = x1, aTop = tp + (fl - tp) * 0.12;
  return { d: `M${P2(x0, fl)} V${f1(aTop + r)} A${f1(r)} ${f1(r)} 0 0 1 ${P2(x1, aTop + r)} V${f1(fl)} Z`, x0, x1, top: aTop, fl };
}
// A door in the hallway's side wall (sd: -1 left, 1 right), seen down the
// hallway in perspective, as a hand-painted adventure game draws one: its
// near and far edges at depths z0 and z1 along the wall (each at the
// cross-section 1 / (1 + z), as hallwaySvg's are), its arch rising between
// them, planks running up it, an iron ring; the hallway's dark over it, its
// sign above it facing us
// The cat room: a big, cozy wooden room for the castle's cats. Honey plank
// walls over dark wainscot, a beamed ceiling and a lantern, a stone
// fireplace with a fire (a portrait of a royal cat over its mantel), a
// window seat under the evening sky, two tall cat trees, shelves stepping up
// the side walls to a high walkway across the back, a rope bridge across the
// room, a big braided rug in the room's colors, and all a cat keeps: bowls,
// a basket, a box, yarn, a toy mouse, a feather on a string. Its cats are
// the castle's own (castleOf's cats), each where its setting puts it.
// The places in it, shared by the room's drawing and its cats': a point
// (X, Y) on the back wall's plane, brought z toward us (z < 0), as drawn
function catRoomPlaces(c) {
  const { k } = c, { floor, top, bw, vy, wh, at, Yh } = roomSpace(c);
  const FX = -0.44 * bw, WX = 0.42 * bw, TX = 0.8 * bw;
  const spot = (X, Y, z, edge, span, lift = 0) => { const f = 1 / (1 + z), [x, y] = at(X, Y, z); return { x, y: y - lift * k * f, f, z, edge, span: span * f }; };
  // the rope bridge: across the room from wall to wall, sagging in the middle
  const BZ = -0.42, bf = 1 / (1 + BZ), [bxa, bya] = at(-bw, Yh(0.7), BZ), ymid = at(0, Yh(0.66), BZ)[1], byc = 2 * ymid - bya;
  const deck = u => [bxa * (1 - 2 * u), (1 - u) * (1 - u) * bya + 2 * u * (1 - u) * byc + u * u * bya];
  const [bx, by] = deck(0.31);
  const spots = {
    fire: spot(FX - 0.12 * bw, floor, -0.17, false, 0.08 * bw), treeL: spot(-TX, Yh(0.56), -0.02, true, 0.03 * bw),
    walkway: spot(0.3 * bw, Yh(0.84), -0.035, true, 0.3 * bw), window: spot(WX - 0.1 * bw, Yh(0.24), -0.05, true, 0.1 * bw),
    treeR: spot(TX, Yh(0.56), -0.02, true, 0.03 * bw), basket: spot(-0.7 * bw, floor, -0.3, false, 0.02 * bw, 9),
    shelfL: spot(-0.965 * bw, Yh(0.76), -0.25, true, 0.02 * bw), shelfR: spot(0.965 * bw, Yh(0.76), -0.25, true, 0.02 * bw),
    mantel: spot(FX + 0.13 * bw, Yh(0.36), -0.05, true, 0.06 * bw), rug: spot(0.42 * bw, floor, -0.3, false, 0.14 * bw),
    box: spot(0.55 * bw, floor, -0.45, false, 0.02 * bw, 12), bridge: { x: bx, y: by, f: bf, z: BZ - 0.001, edge: true, span: 0.05 * bw * bf } };
  // the bridge's drawing: rope rails, planks along the deck, ropes down to it
  const bridge = () => {
    const kf = k * bf, rail = (lift, w) => { const [x0, y0] = deck(0), [x1, y1] = deck(1);
      return `<path d="M${f1(x0)} ${f1(y0 - lift)} Q0 ${f1(byc - lift)} ${f1(x1)} ${f1(y1 - lift)}" fill="none" stroke="#b08a52" stroke-width="${f1(w)}"/>`; };
    let t = rail(-3 * kf, 3 * kf);
    for (let j = 0; j <= 40; j++) { const [x, y] = deck(j / 40), [xn, yn] = deck(Math.min(1, j / 40 + 0.012)), a = Math.atan2(yn - y, xn - x) * 180 / Math.PI;
      t += `<rect x="${f1(x - 5 * kf)}" y="${f1(y - 3 * kf)}" width="${f1(10 * kf)}" height="${f1(6 * kf)}" rx="${f1(1 * kf)}" fill="${j % 3 ? "#9a6a3a" : "#8a5a30"}" stroke="#5e3d22" stroke-width="${f1(0.8 * kf)}" transform="rotate(${a.toFixed(1)} ${f1(x)} ${f1(y)})"/>`; }
    for (let j = 1; j < 20; j++) { const [x, y] = deck(j / 20);
      t += `<line x1="${f1(x)}" y1="${f1(y)}" x2="${f1(x)}" y2="${f1(y - 30 * kf)}" stroke="#c9a46a" stroke-width="${f1(1.5 * kf)}"/>`; }
    return t + rail(30 * kf, 2.5 * kf);
  };
  // the basket's and the box's near sides, drawn again over a cat in them
  const [kx, ky] = at(-0.7 * bw, floor, -0.3), kfb = k * 1 / 0.7, [ox, oy] = at(0.55 * bw, floor, -0.45), kfo = k / 0.55;
  const basketFront = () => `<path d="M${f1(kx - 46 * kfb)} ${f1(ky - 22 * kfb)} A${f1(46 * kfb)} ${f1(12 * kfb)} 0 0 0 ${f1(kx + 46 * kfb)} ${f1(ky - 22 * kfb)} ` +
      `L${f1(kx + 40 * kfb)} ${f1(ky - 2 * kfb)} A${f1(40 * kfb)} ${f1(9 * kfb)} 0 0 1 ${f1(kx - 40 * kfb)} ${f1(ky - 2 * kfb)} Z" fill="#c8995a" stroke="#7a5a2a" stroke-width="${f1(1.5 * kfb)}"/>` +
    [...Array(9)].map((_, j) => { const u = (j + 0.5) / 9 * 2 - 1, xa = kx + u * 44 * kfb, ya = ky - 22 * kfb + Math.sqrt(1 - u * u) * 12 * kfb;
      return `<line x1="${f1(xa)}" y1="${f1(ya)}" x2="${f1(kx + u * 38 * kfb)}" y2="${f1(ky - 2 * kfb + Math.sqrt(1 - u * u) * 9 * kfb)}" stroke="#7a5a2a" stroke-width="${f1(1 * kfb)}" opacity="0.6"/>`; }).join("") +
    [0.35, 0.7].map(v => `<path d="M${f1(kx - (46 - 6 * v) * kfb)} ${f1(ky - (22 - 20 * v) * kfb)} A${f1((46 - 6 * v) * kfb)} ${f1((12 - 3 * v) * kfb)} 0 0 0 ${f1(kx + (46 - 6 * v) * kfb)} ${f1(ky - (22 - 20 * v) * kfb)}" fill="none" stroke="#e0b97a" stroke-width="${f1(2 * kfb)}" stroke-dasharray="${f1(4 * kfb)} ${f1(3 * kfb)}"/>`).join("") +
    `<ellipse cx="${f1(kx)}" cy="${f1(ky - 22 * kfb)}" rx="${f1(46 * kfb)}" ry="${f1(12 * kfb)}" fill="none" stroke="#a87a3e" stroke-width="${f1(5 * kfb)}" stroke-dasharray="${f1(6 * kfb)} ${f1(2 * kfb)}"/>`;
  const boxFront = () => `<rect x="${f1(ox - 34 * kfo)}" y="${f1(oy - 40 * kfo)}" width="${f1(68 * kfo)}" height="${f1(40 * kfo)}" fill="#c9a06a" stroke="#8a6a3a" stroke-width="${f1(1.5 * kfo)}"/>` +
    `<rect x="${f1(ox - 10 * kfo)}" y="${f1(oy - 40 * kfo)}" width="${f1(20 * kfo)}" height="${f1(40 * kfo)}" fill="#d9b480" opacity="0.6"/>` +
    `<path d="M${f1(ox - 34 * kfo)} ${f1(oy - 40 * kfo)} L${f1(ox - 46 * kfo)} ${f1(oy - 30 * kfo)} L${f1(ox - 40 * kfo)} ${f1(oy - 52 * kfo)} L${f1(ox - 30 * kfo)} ${f1(oy - 46 * kfo)} Z" fill="#b8905a" stroke="#8a6a3a" stroke-width="${f1(1.2 * kfo)}"/>` +
    `<path d="M${f1(ox + 34 * kfo)} ${f1(oy - 40 * kfo)} L${f1(ox + 48 * kfo)} ${f1(oy - 32 * kfo)} L${f1(ox + 42 * kfo)} ${f1(oy - 54 * kfo)} L${f1(ox + 30 * kfo)} ${f1(oy - 46 * kfo)} Z" fill="#b8905a" stroke="#8a6a3a" stroke-width="${f1(1.2 * kfo)}"/>` +
    `<text x="${f1(ox)}" y="${f1(oy - 16 * kfo)}" text-anchor="middle" font-size="${f1(11 * kfo)}" font-family="sans-serif" fill="#6a4a22" opacity="0.7">🐟 FISH</text>`;
  return { at, Yh, FX, WX, TX, floor, top, bw, vy, wh, spots, bridge, basketFront, boxFront, BZ, kx, ky, kfb, ox, oy, kfo };
}
function catRoomSvg(c, K) {
  const { x0, y0, x1, y1, W, H, k, pad, R, P } = c, L = catRoomPlaces(c), { at, Yh, FX, WX, TX, floor, top, bw, vy, wh } = L;
  const wood = "#c68a52", woodD = "#8a5a32", woodL = "#ddb07a", beam = "#5b3a1f", wain = "#7d4b27", gold = "#e8b923";
  const pg = (pts, fill, more = "") => `<polygon points="${pts.map(p => P(...p)).join(" ")}" fill="${fill}"${more}/>`;
  const Z = -0.7;   // (as far toward us as the walls are drawn: past the picture's edges)
  let t = `<defs><radialGradient id="bg-glow"><stop offset="0" stop-color="#ffcf6b" stop-opacity="0.55"/><stop offset="1" stop-color="#ffcf6b" stop-opacity="0"/></radialGradient>` +
    `<radialGradient id="bg-fireglow"><stop offset="0" stop-color="#ffb347" stop-opacity="0.5"/><stop offset="1" stop-color="#ffb347" stop-opacity="0"/></radialGradient>` +
    `<linearGradient id="bg-catsky" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#26305c"/><stop offset="0.65" stop-color="#7a5a8c"/><stop offset="1" stop-color="#f2a65a"/></linearGradient></defs>`;
  t += `<rect x="${f1(x0 - pad)}" y="${f1(y0 - pad)}" width="${f1(W + 2 * pad)}" height="${f1(H + 2 * pad)}" fill="${beam}"/>`;
  // the back wall: planks, each a shade of its own, knots and grain; the wainscot below, its raised panels
  const pw = 30 * k, wy = Yh(0.2);
  for (let i = 0, x = -bw; x < bw - 1; i++, x += pw) {
    const xb = Math.min(bw, x + pw), col = mix(wood, R(i, 1201) < 0.5 ? "#000000" : "#ffffff", 0.03 + 0.1 * R(i, 1202));
    t += `<rect x="${f1(x)}" y="${f1(top)}" width="${f1(xb - x)}" height="${f1(wy - top)}" fill="${col}" stroke="${woodD}" stroke-width="${f1(1.2 * k)}"/>` +
         `<path d="M${P(x + pw * 0.3, top)} Q${P(x + pw * (0.15 + 0.5 * R(i, 1205)), (top + wy) / 2)} ${P(x + pw * 0.4, wy)}" fill="none" stroke="${woodD}" stroke-width="${f1(0.8 * k)}" opacity="0.3"/>`;
    if (R(i, 1203) < 0.45) { const ky = top + (wy - top) * (0.1 + 0.8 * R(i, 1204));
      t += `<ellipse cx="${f1(x + pw * 0.55)}" cy="${f1(ky)}" rx="${f1(4.5 * k)}" ry="${f1(2.6 * k)}" fill="none" stroke="${woodD}" stroke-width="${f1(1 * k)}" opacity="0.6"/>` +
           `<ellipse cx="${f1(x + pw * 0.55)}" cy="${f1(ky)}" rx="${f1(1.8 * k)}" ry="${f1(1.1 * k)}" fill="${woodD}" opacity="0.8"/>`; }
  }
  t += `<rect x="${f1(-bw)}" y="${f1(wy)}" width="${f1(2 * bw)}" height="${f1(floor - wy)}" fill="${wain}"/>`;
  for (let i = 0; i < 10; i++) { const xa = -bw + 2 * bw * i / 10 + 6 * k, w = 2 * bw / 10 - 12 * k;
    t += `<rect x="${f1(xa)}" y="${f1(wy + 10 * k)}" width="${f1(w)}" height="${f1(floor - wy - 26 * k)}" rx="${f1(2 * k)}" fill="${mix(wain, "#ffffff", 0.06)}" stroke="${mix(wain, "#000000", 0.35)}" stroke-width="${f1(1.5 * k)}"/>`; }
  t += `<rect x="${f1(-bw)}" y="${f1(wy - 5 * k)}" width="${f1(2 * bw)}" height="${f1(9 * k)}" fill="${woodL}" stroke="${woodD}" stroke-width="${f1(1 * k)}"/>` +
       `<rect x="${f1(-bw)}" y="${f1(floor - 10 * k)}" width="${f1(2 * bw)}" height="${f1(10 * k)}" fill="${beam}"/>`;
  // the side walls, going off toward us: planks in perspective, their wainscot, their rail
  for (const sd of [-1, 1]) {
    t += pg([at(sd * bw, top), at(sd * bw, top, Z), at(sd * bw, wy, Z), at(sd * bw, wy)], mix(wood, "#000000", 0.2));
    t += pg([at(sd * bw, wy), at(sd * bw, wy, Z), at(sd * bw, floor, Z), at(sd * bw, floor)], mix(wain, "#000000", 0.15));
    for (let j = 1; j <= 24; j++) { const z = -0.03 * j, [xa, ya] = at(sd * bw, top, z), [, yb] = at(sd * bw, wy, z);
      t += `<line x1="${f1(xa)}" y1="${f1(ya)}" x2="${f1(xa)}" y2="${f1(yb)}" stroke="${woodD}" stroke-width="${f1(1.2 * k / (1 + z))}" opacity="0.7"/>`; }
    for (let j = 1; j <= 8; j++) { const z = -0.09 * j, [xa, ya] = at(sd * bw, wy, z), [, yb] = at(sd * bw, floor, z);
      t += `<line x1="${f1(xa)}" y1="${f1(ya)}" x2="${f1(xa)}" y2="${f1(yb)}" stroke="${mix(wain, "#000000", 0.35)}" stroke-width="${f1(2 * k / (1 + z))}"/>`; }
    const [ra, rb] = [at(sd * bw, wy, 0), at(sd * bw, wy, Z)];
    t += `<line x1="${f1(ra[0])}" y1="${f1(ra[1])}" x2="${f1(rb[0])}" y2="${f1(rb[1])}" stroke="${woodL}" stroke-width="${f1(7 * k)}"/>`;
  }
  // the ceiling: boards toward us, beams across it, a lantern hanging from the middle one
  t += pg([at(-bw, top), at(bw, top), at(bw, top, Z), at(-bw, top, Z)], "#7a5232");
  for (let i = -8; i <= 8; i++) { const [xa, ya] = at(i * bw / 8, top), [xb, yb] = at(i * bw / 8, top, Z);
    t += `<line x1="${f1(xa)}" y1="${f1(ya)}" x2="${f1(xb)}" y2="${f1(yb)}" stroke="${beam}" stroke-width="${f1(1.5 * k)}" opacity="0.7"/>`; }
  for (const z of [0, -0.12, -0.26, -0.42, -0.6]) { const f = 1 / (1 + z), [xa, ya] = at(-bw, top, z);
    t += `<rect x="${f1(xa)}" y="${f1(ya)}" width="${f1(-2 * xa)}" height="${f1(16 * k * f)}" fill="${beam}"/>` +
         `<rect x="${f1(xa)}" y="${f1(ya + 13 * k * f)}" width="${f1(-2 * xa)}" height="${f1(3 * k * f)}" fill="#3e2714"/>`;
    for (const sd of [-1, 1]) t += `<path d="M${P(sd * bw * f, ya + 16 * k * f)} l${f1(-sd * 18 * k * f)} 0 l${f1(sd * 18 * k * f)} ${f1(26 * k * f)} Z" fill="${beam}"/>`; }
  // the floor: boards toward us, their ends staggered, darker by the wall
  t += pg([at(-bw, floor), at(bw, floor), at(bw, floor, Z), at(-bw, floor, Z)], "#a46a3a");
  for (let i = -10; i <= 10; i++) { const [xa, ya] = at(i * bw / 10, floor), [xb, yb] = at(i * bw / 10, floor, Z);
    t += `<line x1="${f1(xa)}" y1="${f1(ya)}" x2="${f1(xb)}" y2="${f1(yb)}" stroke="#7c4c26" stroke-width="${f1(1.6 * k)}"/>`;
    if (i < 10) for (let j = 0; j < 3; j++) { const z = -(0.05 + 0.22 * j + 0.15 * R(i + 20, 1210 + j)), [ax, ay] = at(i * bw / 10, floor, z), [bx] = at((i + 1) * bw / 10, floor, z);
      t += `<line x1="${f1(ax)}" y1="${f1(ay)}" x2="${f1(bx)}" y2="${f1(ay)}" stroke="#7c4c26" stroke-width="${f1(1.4 * k / (1 + z))}"/>`; } }
  t += pg([at(-bw, floor), at(bw, floor), at(bw, floor, -0.04), at(-bw, floor, -0.04)], "#000000", ` opacity="0.22"`);
  const mRug = t.length;   // (ticket 284: the rug, the fireplace, the mantel, the portrait, the window, the feather toy, the banner, the shelves and the bowls' mat, each a wall or floor object)
  // the rug: braided rings in the room's colors, from the back of the room nearly to us
  { const ra = at(0, floor, -0.08)[1], rb = at(0, floor, -0.55)[1], rcy = (ra + rb) / 2, rry = (rb - ra) / 2, rrx = 0.78 * bw * 1.4;
    const cols = [K.flagA, "#efe2c4", K.flagB, K.carpet, "#efe2c4", K.flagA, K.flagB, "#efe2c4"];
    t += `<ellipse cx="0" cy="${f1(rcy + 4 * k)}" rx="${f1(rrx)}" ry="${f1(rry)}" fill="#000000" opacity="0.2"/>`;
    cols.forEach((col, i) => { const u = 1 - i / cols.length;
      t += `<ellipse cx="0" cy="${f1(rcy)}" rx="${f1(rrx * u)}" ry="${f1(rry * u)}" fill="${col}" stroke="${mix(col, "#000000", 0.3)}" stroke-width="${f1(2.2 * k)}" stroke-dasharray="${f1(6 * k)} ${f1(3 * k)}"/>`; }); }
  t = floorSince(t, mRug, "rug");
  const mFire = t.length;
  // the fireplace: a chimney breast of round stones, standing out a little from the wall, to the ceiling
  const hw = 0.2 * bw, fz = -0.04, [cxa] = at(FX - hw, top, fz), [cxb] = at(FX + hw, top, fz), cyt = at(0, top, fz)[1], cyb = at(0, floor, fz)[1];
  t += pg([at(FX + hw, top), at(FX + hw, top, fz), at(FX + hw, floor, fz), at(FX + hw, floor)], "#4e4640");
  t += `<rect x="${f1(cxa)}" y="${f1(cyt)}" width="${f1(cxb - cxa)}" height="${f1(cyb - cyt)}" fill="#5e554e"/>`;
  for (let r = 0, y = cyb; y > cyt; r++, y -= 21 * k)
    for (let i = 0, x = cxa - (r % 2) * 14 * k; x < cxb; i++, x += (26 + 14 * R(r * 13 + i, 1220)) * k) {
      const w = (24 + 12 * R(r * 13 + i, 1221)) * k, xa = Math.max(x + 1.5 * k, cxa + 1 * k), xb = Math.min(x + w - 1.5 * k, cxb - 1 * k);
      if (xb - xa < 5 * k) continue;
      t += `<rect x="${f1(xa)}" y="${f1(Math.max(y - 20 * k, cyt))}" width="${f1(xb - xa)}" height="${f1(Math.min(19 * k, y - cyt))}" rx="${f1(7 * k)}" fill="${mix("#9a8f84", R(r * 13 + i, 1222) < 0.5 ? "#5a4a3e" : "#d8d0c4", 0.12 + 0.25 * R(r * 13 + i, 1223))}"/>`;
    }
  // its hearth, a stone slab before it, and the firebox: an arch, sooty, logs, the fire, sparks
  const ff = 1 / (1 + fz), fbw = 0.17 * bw * ff, fbh = 0.27 * wh * ff, fx = FX * ff, fy = cyb;
  t += pg([at(FX - hw - 0.02 * bw, floor, fz), at(FX + hw + 0.04 * bw, floor, fz), at(FX + hw + 0.04 * bw, floor, -0.13), at(FX - hw - 0.02 * bw, floor, -0.13)], "#8a8078") +
       pg([at(FX - hw - 0.02 * bw, floor, -0.13), at(FX + hw + 0.04 * bw, floor, -0.13), [at(FX + hw + 0.04 * bw, floor, -0.13)[0], at(0, floor, -0.13)[1] + 6 * k], [at(FX - hw - 0.02 * bw, floor, -0.13)[0], at(0, floor, -0.13)[1] + 6 * k]], "#6a6058");
  for (let i = 1; i < 6; i++) { const X = FX - hw - 0.02 * bw + (2 * hw + 0.06 * bw) * i / 6, [xa, ya] = at(X, floor, fz), [xb, yb] = at(X, floor, -0.13);
    t += `<line x1="${f1(xa)}" y1="${f1(ya)}" x2="${f1(xb)}" y2="${f1(yb)}" stroke="#5e554e" stroke-width="${f1(1.4 * k)}"/>`; }
  t += `<circle cx="${f1(fx)}" cy="${f1(fy - fbh * 0.3)}" r="${f1(200 * k)}" fill="url(#bg-fireglow)"/>`;
  t += `<path d="M${P(fx - fbw / 2 - 8 * k, fy)} V${f1(fy - fbh + fbw / 2)} A${f1(fbw / 2 + 8 * k)} ${f1(fbw / 2 + 8 * k)} 0 0 1 ${P(fx + fbw / 2 + 8 * k, fy - fbh + fbw / 2)} V${f1(fy)} Z" fill="#7a6e64"/>` +
       `<path d="M${P(fx - fbw / 2, fy)} V${f1(fy - fbh + fbw / 2)} A${f1(fbw / 2)} ${f1(fbw / 2)} 0 0 1 ${P(fx + fbw / 2, fy - fbh + fbw / 2)} V${f1(fy)} Z" fill="#1a0f0a"/>` +
       [...Array(4)].map((_, j) => `<line x1="${f1(fx - fbw * 0.4)}" y1="${f1(fy - fbh * (0.2 + 0.18 * j))}" x2="${f1(fx + fbw * 0.4)}" y2="${f1(fy - fbh * (0.2 + 0.18 * j))}" stroke="#3a1a10" stroke-width="${f1(1.2 * k)}"/>`).join("") +
       `<ellipse cx="${f1(fx)}" cy="${f1(fy - 6 * k)}" rx="${f1(fbw * 0.4)}" ry="${f1(6 * k)}" fill="#ff5a1f" opacity="0.85"><animate attributeName="opacity" values="0.85;0.6;0.9;0.85" dur="2.3s" repeatCount="indefinite"/></ellipse>`;
  for (const [sd, a] of [[-1, -14], [1, 12]]) t += `<rect x="${f1(fx - fbw * 0.36)}" y="${f1(fy - 15 * k)}" width="${f1(fbw * 0.72)}" height="${f1(10 * k)}" rx="${f1(5 * k)}" fill="#6b4226" stroke="#3a2414" stroke-width="${f1(1 * k)}" transform="rotate(${a} ${f1(fx)} ${f1(fy - 10 * k)})"/>` +
    `<ellipse cx="${f1(fx + sd * fbw * 0.33)}" cy="${f1(fy - 10 * k + sd * a * 0.9 * k * 0.3)}" rx="${f1(3 * k)}" ry="${f1(5 * k)}" fill="#c9a26b" transform="rotate(${a} ${f1(fx)} ${f1(fy - 10 * k)})"/>`;
  // (its flames: the scene's quick things, leaping with the quick LFO, each a different way into it)
  const flame = (x, y, h, w, col, dur) => `<g transform="translate(${f1(x)} ${f1(y)})"><path d="M0 ${f1(-h)} Q${f1(w)} ${f1(-h * 0.45)} ${f1(w * 0.6)} 0 Q0 ${f1(h * 0.06)} ${f1(-w * 0.6)} 0 Q${f1(-w)} ${f1(-h * 0.45)} 0 ${f1(-h)} Z" fill="${col}">` +
    sceneLfo(c.o, "fast", dur * 1.7 % 1, "transform", (v, amp) => `${(1 - 0.05 * v).toFixed(3)} ${(1 + (0.06 + 0.24 * amp) * v).toFixed(3)}`,
      `<animateTransform attributeName="transform" type="scale" additive="sum" values="1 1;1.08 0.84;0.93 1.12;1 1" dur="${dur}s" repeatCount="indefinite"/>`, "scale") + `</path></g>`;
  t += flame(fx - fbw * 0.18, fy - 12 * k, fbh * 0.55, 14 * k, "#ff7a1a", 0.9) + flame(fx + fbw * 0.16, fy - 12 * k, fbh * 0.5, 13 * k, "#ff7a1a", 1.1) +
       flame(fx, fy - 12 * k, fbh * 0.7, 18 * k, "#ff9a2e", 0.8) + flame(fx - fbw * 0.06, fy - 12 * k, fbh * 0.45, 10 * k, "#ffd166", 0.7) + flame(fx + fbw * 0.08, fy - 12 * k, fbh * 0.36, 8 * k, "#fff1a8", 0.6);
  for (let j = 0; j < 5; j++) { const sx = fx + (R(j, 1230) - 0.5) * fbw * 0.6, d = (1.4 + 0.5 * R(j, 1231)).toFixed(2);
    t += `<circle cx="${f1(sx)}" cy="${f1(fy - fbh * 0.5)}" r="${f1(1.6 * k)}" fill="#ffd166"><animate attributeName="cy" values="${f1(fy - fbh * 0.4)};${f1(fy - fbh * 0.95)}" dur="${d}s" begin="-${(R(j, 1232) * 2).toFixed(2)}s" repeatCount="indefinite"/>` +
         `<animate attributeName="opacity" values="1;0" dur="${d}s" begin="-${(R(j, 1232) * 2).toFixed(2)}s" repeatCount="indefinite"/></circle>`; }
  t = wallSince(t, mFire, "fireplace");
  const mMantel = t.length;
  let mPortrait = 0;
  // the mantel, a thick beam, two candles and a little clock on it; over it, a royal cat's portrait
  { const ma = FX - hw - 0.03 * bw, mb = FX + hw + 0.04 * bw, Ym = Yh(0.36);
    t += pg([at(ma, Ym, -0.02), at(mb, Ym, -0.02), at(mb, Ym, -0.09), at(ma, Ym, -0.09)], "#9a6a3a") +
         pg([at(ma, Ym, -0.09), at(mb, Ym, -0.09), at(mb, Ym + 14 * k, -0.09), at(ma, Ym + 14 * k, -0.09)], beam) +
         pg([at(mb, Ym, -0.02), at(mb, Ym, -0.09), at(mb, Ym + 14 * k, -0.09), at(mb, Ym + 14 * k, -0.02)], "#3e2714");
    for (const [X, i] of [[ma + 0.04 * bw, 0], [FX - 0.01 * bw, 1]]) { const [cx, cy] = at(X, Ym, -0.055);
      t += `<rect x="${f1(cx - 6 * k)}" y="${f1(cy - 3 * k)}" width="${f1(12 * k)}" height="${f1(3 * k)}" fill="${gold}"/><rect x="${f1(cx - 3 * k)}" y="${f1(cy - 22 * k)}" width="${f1(6 * k)}" height="${f1(19 * k)}" fill="#fff6dc"/>` +
           `<circle cx="${f1(cx)}" cy="${f1(cy - 28 * k)}" r="${f1(24 * k)}" fill="url(#bg-glow)"/>` +
           `<ellipse cx="${f1(cx)}" cy="${f1(cy - 27 * k)}" rx="${f1(3.2 * k)}" ry="${f1(5.5 * k)}" fill="#ffb53b"><animate attributeName="ry" values="${f1(5.5 * k)};${f1(4.2 * k)};${f1(6 * k)};${f1(5.5 * k)}" dur="${0.8 + 0.2 * i}s" repeatCount="indefinite"/></ellipse>`; }
    { const [cx, cy] = at(FX - 0.13 * bw, Ym, -0.055);
      t += `<rect x="${f1(cx - 11 * k)}" y="${f1(cy - 26 * k)}" width="${f1(22 * k)}" height="${f1(26 * k)}" rx="${f1(4 * k)}" fill="#6b4226" stroke="${gold}" stroke-width="${f1(1.5 * k)}"/>` +
           `<circle cx="${f1(cx)}" cy="${f1(cy - 15 * k)}" r="${f1(7.5 * k)}" fill="#fff6dc" stroke="${gold}" stroke-width="${f1(1.2 * k)}"/>` +
           `<line x1="${f1(cx)}" y1="${f1(cy - 15 * k)}" x2="${f1(cx)}" y2="${f1(cy - 20 * k)}" stroke="#1f1f22" stroke-width="${f1(1 * k)}"/>` +
           `<line x1="${f1(cx)}" y1="${f1(cy - 15 * k)}" x2="${f1(cx + 4 * k)}" y2="${f1(cy - 14 * k)}" stroke="#1f1f22" stroke-width="${f1(1 * k)}"><animateTransform attributeName="transform" type="rotate" values="0 ${f1(cx)} ${f1(cy - 15 * k)};360 ${f1(cx)} ${f1(cy - 15 * k)}" dur="60s" repeatCount="indefinite"/></line>`; }
    t = wallSince(t, mMantel, "mantel"); mPortrait = t.length;
    const [px, py] = at(FX, Yh(0.58), fz), pw2 = 0.28 * bw * ff, ph = 0.22 * wh * ff;
    t += `<rect x="${f1(px - pw2 / 2 - 6 * k)}" y="${f1(py - ph / 2 - 6 * k)}" width="${f1(pw2 + 12 * k)}" height="${f1(ph + 12 * k)}" fill="${gold}" stroke="#9a7a12" stroke-width="${f1(2 * k)}"/>` +
         `<rect x="${f1(px - pw2 / 2)}" y="${f1(py - ph / 2)}" width="${f1(pw2)}" height="${f1(ph)}" fill="#2f4a3a"/>` +
         `<path d="M${P(px - pw2 / 2, py + ph / 2)} Q${P(px, py + ph * 0.1)} ${P(px + pw2 / 2, py + ph / 2)} Z" fill="${K.flagA}" opacity="0.85"/>` +
         `<g transform="translate(${f1(px)} ${f1(py + ph * 0.42)}) scale(${(ph / 92).toFixed(3)})">${shoulderCat(4, { catFur: "#9ca3af", catWhite: "#ffffff", catEyes: "#e0a800", catNose: "#f4a0b0" }, false, true)}` +
         `<path d="M-14 -60 L-14 -70 L-8 -64 L-3 -73 L2 -64 L8 -70 L8 -60 Z" fill="${gold}" stroke="#9a7a12" stroke-width="1"/></g>`; }
  t = wallSince(t, mPortrait, "portrait");
  const mWindow = t.length;
  // (the window seat, drawn with the room's standing things, below: ticket 99)
  let seat = "";
  // the window: an arch onto the evening, a crescent moon, stars, far hills;
  // leaded panes; curtains tied back in the room's colors; the window seat under it
  { const [wx, wyb] = at(WX, Yh(0.3)), wyt = at(WX, Yh(0.74))[1], ww = 0.38 * bw, ar = ww / 2;
    const arch = (g) => `M${P(wx - ar + g, wyb)} V${f1(wyt + ar)} A${f1(ar - g)} ${f1(ar - g)} 0 0 1 ${P(wx + ar - g, wyt + ar)} V${f1(wyb)} Z`;
    // (all inside the window, its leads too, kept inside its arch)
    t += `<path d="${arch(-8 * k)}" fill="${beam}"/><defs><clipPath id="bg-catwin"><path d="${arch(0)}"/></clipPath></defs><g clip-path="url(#bg-catwin)"><path d="${arch(0)}" fill="url(#bg-catsky)"/>` +
         `<path d="M${P(wx - ar, wyb - 20 * k)} Q${P(wx - ar * 0.4, wyb - 46 * k)} ${P(wx, wyb - 28 * k)} Q${P(wx + ar * 0.5, wyb - 12 * k)} ${P(wx + ar, wyb - 34 * k)} V${f1(wyb)} H${f1(wx - ar)} Z" fill="#3a3a5e"/>` +
         `<path d="M${P(wx - ar, wyb - 6 * k)} Q${P(wx, wyb - 22 * k)} ${P(wx + ar, wyb - 8 * k)} V${f1(wyb)} H${f1(wx - ar)} Z" fill="#2a2a48"/>` +
         moonOfDateSvg(c.o, wx + ar * 0.45, wyt + ar * 0.75, 12 * k, "#3a3f6e") +   // (ticket 490: the date's moon, where the crescent was)
         [...Array(9)].map((_, j) => `<circle cx="${f1(wx - ar * 0.8 + ar * 1.6 * R(j, 1240))}" cy="${f1(wyt + ar * 0.3 + (wyb - wyt) * 0.45 * R(j, 1241))}" r="${f1((0.8 + 1.2 * R(j, 1242)) * k)}" fill="#ffffff">` +
           `<animate attributeName="opacity" values="1;0.25;1" dur="${(1.6 + 2 * R(j, 1243)).toFixed(1)}s" begin="-${(3 * R(j, 1244)).toFixed(1)}s" repeatCount="indefinite"/></circle>`).join("");
    for (let i = 1; i < 6; i++) t += `<line x1="${f1(wx - ar + ww * i / 6)}" y1="${f1(wyt)}" x2="${f1(wx - ar + ww * i / 6)}" y2="${f1(wyb)}" stroke="#2a2a32" stroke-width="${f1(1 * k)}" opacity="0.5"/>`;
    for (let i = 1; i < 7; i++) t += `<line x1="${f1(wx - ar)}" y1="${f1(wyt + (wyb - wyt) * i / 7)}" x2="${f1(wx + ar)}" y2="${f1(wyt + (wyb - wyt) * i / 7)}" stroke="#2a2a32" stroke-width="${f1(1 * k)}" opacity="0.5"/>`;
    t += `</g>`;
    t += `<path d="${arch(0)}" fill="none" stroke="${woodD}" stroke-width="${f1(5 * k)}"/><line x1="${f1(wx)}" y1="${f1(wyt)}" x2="${f1(wx)}" y2="${f1(wyb)}" stroke="${woodD}" stroke-width="${f1(5 * k)}"/>` +
         `<line x1="${f1(wx - ar)}" y1="${f1(wyt + ar)}" x2="${f1(wx + ar)}" y2="${f1(wyt + ar)}" stroke="${woodD}" stroke-width="${f1(5 * k)}"/>`;
    // (the curtains: a rod, and each side gathered up and tied back with a gold cord)
    t += `<rect x="${f1(wx - ar - 30 * k)}" y="${f1(wyt - 12 * k)}" width="${f1(ww + 60 * k)}" height="${f1(5 * k)}" rx="${f1(2.5 * k)}" fill="${beam}"/>` +
         [-1, 1].map(sd => `<circle cx="${f1(wx + sd * (ar + 32 * k))}" cy="${f1(wyt - 9.5 * k)}" r="${f1(5 * k)}" fill="${gold}"/>` +
           `<path d="M${P(wx + sd * (ar + 26 * k), wyt - 8 * k)} L${P(wx + sd * (ar - 22 * k), wyt - 8 * k)} Q${P(wx + sd * (ar - 4 * k), (wyt + wyb) / 2)} ${P(wx + sd * (ar + 4 * k), (wyt + wyb) / 2 + 10 * k)} ` +
           `Q${P(wx + sd * (ar - 6 * k), wyb - 10 * k)} ${P(wx + sd * (ar + 2 * k), wyb + 6 * k)} L${P(wx + sd * (ar + 30 * k), wyb + 6 * k)} Q${P(wx + sd * (ar + 22 * k), (wyt + wyb) / 2 + 20 * k)} ${P(wx + sd * (ar + 26 * k), wyt - 8 * k)} Z" fill="${K.flagA}" stroke="${mix(K.flagA, "#000000", 0.35)}" stroke-width="${f1(1.5 * k)}"/>` +
           [0.3, 0.55, 0.8].map(u => `<path d="M${P(wx + sd * (ar + (26 - 40 * u) * k), wyt - 6 * k)} Q${P(wx + sd * (ar + 6 * k), (wyt + wyb) / 2)} ${P(wx + sd * (ar + 12 * k), wyb)}" fill="none" stroke="${mix(K.flagA, "#000000", 0.3)}" stroke-width="${f1(1 * k)}" opacity="0.6"/>`).join("") +
           `<ellipse cx="${f1(wx + sd * (ar + 9 * k))}" cy="${f1((wyt + wyb) / 2 + 8 * k)}" rx="${f1(10 * k)}" ry="${f1(3 * k)}" fill="${gold}"/>`).join("");
    // (the seat: a box standing out from the wall, a cushion in the room's second color, two pillows)
    const sa = WX - 0.27 * bw, sb = WX + 0.27 * bw, Ys = Yh(0.22);
    seat += pg([at(sa, Ys, 0), at(sb, Ys, 0), at(sb, Ys, -0.09), at(sa, Ys, -0.09)], "#9a6a3a") +
         pg([at(sa, Ys, -0.09), at(sb, Ys, -0.09), at(sb, floor, -0.09), at(sa, floor, -0.09)], wood, ` stroke="${woodD}" stroke-width="${f1(1.5 * k)}"`);
    // (its three panels, side by side, each in its own third of the front)
    for (const u of [1 / 6, 0.5, 5 / 6]) { const [xa, ya] = at(sa + (sb - sa) * u - 0.075 * bw, Ys + 10 * k, -0.09), [xb, yb] = at(sa + (sb - sa) * u + 0.075 * bw, floor - 8 * k, -0.09);
      seat += `<rect x="${f1(xa)}" y="${f1(ya)}" width="${f1(xb - xa)}" height="${f1(yb - ya)}" rx="${f1(2 * k)}" fill="none" stroke="${woodD}" stroke-width="${f1(1.4 * k)}"/>`; }
    seat += pg([at(sa + 0.01 * bw, Ys - 9 * k, -0.005), at(sb - 0.01 * bw, Ys - 9 * k, -0.005), at(sb - 0.01 * bw, Ys - 4 * k, -0.085), at(sa + 0.01 * bw, Ys - 4 * k, -0.085)], K.flagB, ` stroke="${mix(K.flagB, "#000000", 0.3)}" stroke-width="${f1(1.5 * k)}"`) +
         pg([at(sa + 0.01 * bw, Ys - 4 * k, -0.085), at(sb - 0.01 * bw, Ys - 4 * k, -0.085), at(sb - 0.01 * bw, Ys + 1 * k, -0.09), at(sa + 0.01 * bw, Ys + 1 * k, -0.09)], mix(K.flagB, "#000000", 0.2));
    for (const [X, col] of [[sa + 0.06 * bw, K.flagA], [sb - 0.06 * bw, "#efe2c4"]]) { const [cx, cy] = at(X, Ys - 8 * k, -0.03);
      seat += `<rect x="${f1(cx - 14 * k)}" y="${f1(cy - 22 * k)}" width="${f1(28 * k)}" height="${f1(24 * k)}" rx="${f1(8 * k)}" fill="${col}" stroke="${mix(col, "#000000", 0.3)}" stroke-width="${f1(1.4 * k)}"/>` +
           `<circle cx="${f1(cx)}" cy="${f1(cy - 10 * k)}" r="${f1(2 * k)}" fill="${mix(col, "#000000", 0.3)}"/>`; } }
  t = wallSince(t, mWindow, "window");
  let mToy = 0;
  // the high walkway, across the back wall under the ceiling, on brackets, its edge a rail of little posts;
  // a cat flap in the wall at its end, and a feather toy dangling from it
  { const Yw = Yh(0.84), wa = -0.98 * bw, wb = 0.98 * bw;
    for (let i = 0; i <= 8; i++) { const X = wa + (wb - wa) * i / 8, [bxa, bya] = at(X, Yw + 8 * k, -0.005);
      t += `<path d="M${P(bxa - 3 * k, bya)} L${P(bxa + 3 * k, bya)} L${P(bxa + 3 * k, bya + 26 * k)} Z" fill="${beam}"/>`; }
    t += pg([at(wa, Yw + 8 * k), at(wb, Yw + 8 * k), at(wb, Yw + 8 * k, -0.07), at(wa, Yw + 8 * k, -0.07)], "#6b4527") +
         pg([at(wa, Yw, -0.07), at(wb, Yw, -0.07), at(wb, Yw + 8 * k, -0.07), at(wa, Yw + 8 * k, -0.07)], wood, ` stroke="${woodD}" stroke-width="${f1(1 * k)}"`);
    for (let i = 0; i < 12; i++) { const [mx, my] = at(wa + (wb - wa) * (i + 0.5) / 12, Yw + 4 * k, -0.07);
      t += `<path d="M${f1(mx - 2 * k)} ${f1(my)} a${f1(1.2 * k)} ${f1(1.2 * k)} 0 1 0 0.1 0 M${f1(mx + 1.2 * k)} ${f1(my - 1.5 * k)} a${f1(0.7 * k)} ${f1(0.7 * k)} 0 1 0 0.1 0" fill="${woodD}" opacity="0.5"/>`; }
    const [fx2, fy2] = at(0.88 * bw, Yw), fw = 22 * k;
    t += `<path d="M${P(fx2 - fw, fy2)} V${f1(fy2 - 30 * k)} A${f1(fw)} ${f1(fw)} 0 0 1 ${P(fx2 + fw, fy2 - 30 * k)} V${f1(fy2)} Z" fill="#2a1a10" stroke="${beam}" stroke-width="${f1(3 * k)}"/>` +
         `<path d="M${P(fx2 - fw + 3 * k, fy2 - 32 * k)} H${f1(fx2 + fw - 3 * k)} V${f1(fy2 - 2 * k)} H${f1(fx2 - fw + 3 * k)} Z" fill="${K.flagB}" opacity="0.85"><animateTransform attributeName="transform" type="skewX" values="0;3;0;-2;0" dur="5s" repeatCount="indefinite"/></path>` +
         `<rect x="${f1(fx2 - 16 * k)}" y="${f1(fy2 - 66 * k)}" width="${f1(32 * k)}" height="${f1(13 * k)}" rx="${f1(3 * k)}" fill="#a0703f" stroke="#5e3d22" stroke-width="${f1(1.2 * k)}"/>` +
         `<text x="${f1(fx2)}" y="${f1(fy2 - 56.5 * k)}" text-anchor="middle" font-size="${f1(9 * k)}" font-family="sans-serif" fill="#fff8e7">🐾 out</text>`;
    mToy = t.length;
    const [tx, ty] = at(-0.2 * bw, Yw + 8 * k, -0.07), sl = 70 * k;
    t += `<g>${sceneLfo(c.o, "slow", 0.3, "transform", (v, amp) => `${f1((5 + 12 * amp) * v)} ${f1(tx)} ${f1(ty)}`,
           `<animateTransform attributeName="transform" type="rotate" values="-8 ${f1(tx)} ${f1(ty)};9 ${f1(tx)} ${f1(ty)};-8 ${f1(tx)} ${f1(ty)}" dur="3.1s" repeatCount="indefinite"/>`)}` +
         `<line x1="${f1(tx)}" y1="${f1(ty)}" x2="${f1(tx)}" y2="${f1(ty + sl)}" stroke="#e8e0d0" stroke-width="${f1(0.8 * k)}"/>` +
         `<path d="M${P(tx, ty + sl)} q${f1(-7 * k)} ${f1(12 * k)} ${f1(-2 * k)} ${f1(26 * k)} q${f1(8 * k)} ${f1(-12 * k)} ${f1(2 * k)} ${f1(-26 * k)} Z" fill="#3fb6c8"/>` +
         `<path d="M${P(tx + 1 * k, ty + sl)} q${f1(8 * k)} ${f1(10 * k)} ${f1(3 * k)} ${f1(22 * k)} q${f1(-6 * k)} ${f1(-10 * k)} ${f1(-3 * k)} ${f1(-22 * k)} Z" fill="#f2c12e"/></g>`; }
  t = wallSince(t, mToy, "feather toy");
  const mBanner = t.length;
  // the castle's banner, its letter, hanging from the walkway in the middle
  { const [bx, by] = at(0, Yh(0.84) + 8 * k, -0.01), bwid = 0.22 * bw, bh = 0.3 * wh;
    t += castleFlagDefs(K, 10 * k) + `<path d="M${P(bx - bwid / 2, by)} H${f1(bx + bwid / 2)} V${f1(by + bh)} L${P(bx, by + bh - 24 * k)} L${P(bx - bwid / 2, by + bh)} Z" fill="${castleFlagFill(K, 0, 10 * k)}" stroke="${gold}" stroke-width="${f1(3 * k)}"/>` +
         castleLetter(K, 0, bx, by + bh * 0.48, 40 * k, "serif") +
         `<g transform="translate(${f1(bx)} ${f1(by + bh * 0.82)}) scale(${(k * 0.9).toFixed(3)})">` +
         [[-6, -6], [6, -6], [-10, 2], [10, 2]].map(([dx, dy]) => `<ellipse cx="${dx}" cy="${dy}" rx="3.2" ry="4" fill="${gold}"/>`).join("") + `<ellipse cx="0" cy="8" rx="8" ry="6.5" fill="${gold}"/></g>`; }
  t = wallSince(t, mBanner, "banner");
  // the shelves stepping up the side walls, toward us, to the walkway's ends
  const sideShelf = (sd, z0, z1, Y) => { const d = 0.07 * bw, th = 8 * k, Xw = sd * bw, Xo = sd * (bw - d);
    return pg([at(Xw, Y + th, z0), at(Xw, Y + th, z1), at(Xo, Y + th, z1), at(Xo, Y + th, z0)], "#6b4527") +
      pg([at(Xo, Y, z0), at(Xo, Y, z1), at(Xo, Y + th, z1), at(Xo, Y + th, z0)], wood, ` stroke="${woodD}" stroke-width="${f1(1 * k)}"`) +
      pg([at(Xw, Y, z1), at(Xo, Y, z1), at(Xo, Y + th, z1), at(Xw, Y + th, z1)], woodD) +
      [z0 + (z1 - z0) * 0.25, z0 + (z1 - z0) * 0.75].map(z => { const [a, b] = [at(Xw, Y + th, z), at(Xo, Y + th, z)], [cx, cy] = at(Xw, Y + th + 22 * k, z);
        return `<path d="M${P(...a)} L${P(...b)} L${P(cx, cy)} Z" fill="${beam}"/>`; }).join(""); };
  for (const sd of [-1, 1]) t += [[-0.04, -0.14, Yh(0.8)], [-0.2, -0.3, Yh(0.76)], [-0.36, -0.46, Yh(0.68)]].map(([z0, z1, Y], j) => wallObj(`shelf, ${sd < 0 ? "left" : "right"} ${j + 1}`, sideShelf(sd, z0, z1, Y))).join("");
  const mMat = t.length;
  // (the bowls' mat, flat on the floor)
  { const [bx, by] = at(0.64 * bw, floor, -0.15), s = k / 0.85;
    t += `<rect x="${f1(bx - 34 * s)}" y="${f1(by - 6 * s)}" width="${f1(68 * s)}" height="${f1(14 * s)}" rx="${f1(4 * s)}" fill="${K.flagB}" opacity="0.85"/>`; }
  t = floorSince(t, mMat, "mat for the bowls");
  // (ticket 99) all that stands in the room, after all of its walls, floor and what is on them: each
  // an object at its foot, so the cats (drawn after the room) are sorted in among them by depth and
  // never land under a wall, a shelf or the walkway
  // the lantern, hanging from the middle beam (its depth the floor's under it)
  { const [lx, ly] = at(0, top + 16 * k, -0.26), lf = 1 / 0.74, s = k * lf, by = ly + 50 * s;
    t += obj("lantern", at(0, floor, -0.26)[1], `<line x1="${f1(lx)}" y1="${f1(ly)}" x2="${f1(lx)}" y2="${f1(by - 26 * s)}" stroke="#2a2a32" stroke-width="${f1(2 * s)}"/>` +
         `<circle cx="${f1(lx)}" cy="${f1(by)}" r="${f1(60 * s)}" fill="url(#bg-glow)"/>` +
         `<path d="M${P(lx - 14 * s, by - 18 * s)} L${P(lx + 14 * s, by - 18 * s)} L${P(lx + 11 * s, by + 18 * s)} L${P(lx - 11 * s, by + 18 * s)} Z" fill="#ffe7a8" stroke="#2a2a32" stroke-width="${f1(2.5 * s)}"/>` +
         `<path d="M${P(lx - 18 * s, by - 18 * s)} L${P(lx, by - 30 * s)} L${P(lx + 18 * s, by - 18 * s)} Z" fill="#2a2a32"/>` +
         `<rect x="${f1(lx - 13 * s)}" y="${f1(by + 18 * s)}" width="${f1(26 * s)}" height="${f1(5 * s)}" fill="#2a2a32"/>` +
         `<line x1="${f1(lx)}" y1="${f1(by - 18 * s)}" x2="${f1(lx)}" y2="${f1(by + 18 * s)}" stroke="#2a2a32" stroke-width="${f1(1.5 * s)}"/>` +
         `<ellipse cx="${f1(lx)}" cy="${f1(by + 2 * s)}" rx="${f1(3 * s)}" ry="${f1(6 * s)}" fill="#ff9a2e"><animate attributeName="ry" values="${f1(6 * s)};${f1(4.5 * s)};${f1(6.5 * s)};${f1(6 * s)}" dur="0.9s" repeatCount="indefinite"/></ellipse>`); }
  // a stack of firewood at the hearth's end, toward the wall's corner
  { const [lx, ly] = at(FX - hw + 0.02 * bw, floor, -0.09); let w = "";
    for (let j = 0; j < 6; j++) { const row = j < 3 ? 0 : j < 5 ? 1 : 2, i = j - [0, 3, 5][row], x = lx + (i - (2 - row) / 2) * 12 * k + row * 0, y = ly - 6 * k - row * 10 * k;
      w += `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(5.6 * k)}" fill="#c9a26b" stroke="#6b4226" stroke-width="${f1(1.6 * k)}"/><circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(2.2 * k)}" fill="none" stroke="#9a7a4a" stroke-width="${f1(0.8 * k)}"/>`; }
    t += obj("firewood", ly, w); }
  // the window seat, with the mouse hole drawn on its front as it always was, and the mouse peeking out
  { const [mx, my] = at(0.22 * bw, floor), mr = 11 * k;
    seat += `<path d="M${P(mx - mr, my)} V${f1(my - mr)} A${f1(mr)} ${f1(mr)} 0 0 1 ${P(mx + mr, my - mr)} V${f1(my)} Z" fill="#1a0f0a"/>` +
         `<g opacity="0"><animate attributeName="opacity" values="0;0;1;1;0" keyTimes="0;0.7;0.75;0.9;1" dur="9s" repeatCount="indefinite"/>` +
         `<circle cx="${f1(mx + 2 * k)}" cy="${f1(my - 6 * k)}" r="${f1(5 * k)}" fill="#9ca3af"/><circle cx="${f1(mx - 1 * k)}" cy="${f1(my - 11 * k)}" r="${f1(3 * k)}" fill="#c9ccd4"/>` +
         `<circle cx="${f1(mx + 5 * k)}" cy="${f1(my - 11 * k)}" r="${f1(3 * k)}" fill="#c9ccd4"/><circle cx="${f1(mx + 4 * k)}" cy="${f1(my - 6 * k)}" r="${f1(0.9 * k)}" fill="#111"/>` +
         `<circle cx="${f1(mx + 7 * k)}" cy="${f1(my - 4.5 * k)}" r="${f1(1 * k)}" fill="#f4a0b0"/></g>`; }
  t += obj("window seat", at(0, floor, -0.09)[1], seat);
  // two cat trees, near the back corners: sisal posts, carpeted platforms in a
  // shade of the room's first color, a cubby at the foot, a pompom swinging, a cup bed on top
  for (const sd of [-1, 1]) {
    const X = sd * TX, z = -0.02, f = 1 / (1 + z), kf = k * f, [px, pyb] = at(X, floor, z), cpt = mix(K.flagA, "#efe2c4", 0.55), cptD = mix(cpt, "#000000", 0.25);
    let tt = "";
    const post = (x, ya, yb) => `<rect x="${f1(x - 7 * kf)}" y="${f1(ya)}" width="${f1(14 * kf)}" height="${f1(yb - ya)}" fill="#c9b48a" stroke="#8a7a5a" stroke-width="${f1(1 * kf)}"/>` +
      [...Array(Math.max(1, Math.floor((yb - ya) / (5 * kf))))].map((_, j) => `<line x1="${f1(x - 7 * kf)}" y1="${f1(ya + j * 5 * kf + 4 * kf)}" x2="${f1(x + 7 * kf)}" y2="${f1(ya + j * 5 * kf)}" stroke="#9a8a62" stroke-width="${f1(1 * kf)}"/>`).join("");
    const plat = (x, Y, rx) => { const y = at(X, Y, z)[1], up = Y > vy;
      return `<rect x="${f1(x - rx)}" y="${f1(y)}" width="${f1(2 * rx)}" height="${f1(7 * kf)}" fill="${cptD}"/>` +
        `<ellipse cx="${f1(x)}" cy="${f1(up ? y : y + 7 * kf)}" rx="${f1(rx)}" ry="${f1(7 * kf)}" fill="${up ? cpt : cptD}" stroke="${cptD}" stroke-width="${f1(1 * kf)}"/>` +
        (up ? "" : `<rect x="${f1(x - rx)}" y="${f1(y - 1 * kf)}" width="${f1(2 * rx)}" height="${f1(3 * kf)}" fill="${cpt}"/>`); };
    const y1 = at(X, Yh(0.18), z)[1], y2 = at(X, Yh(0.38), z)[1], y3 = at(X, Yh(0.56), z)[1];
    tt += `<ellipse cx="${f1(px)}" cy="${f1(pyb)}" rx="${f1(50 * kf)}" ry="${f1(8 * kf)}" fill="#000000" opacity="0.25"/>` +
         post(px + sd * 20 * kf, y2, y1) + post(px - sd * 10 * kf, y3, y1) + post(px + sd * 14 * kf, y3, y2);
    // (the cubby: a carpeted box, a round door)
    tt += `<rect x="${f1(px - 44 * kf)}" y="${f1(y1)}" width="${f1(88 * kf)}" height="${f1(pyb - y1)}" rx="${f1(5 * kf)}" fill="${cpt}" stroke="${cptD}" stroke-width="${f1(1.5 * kf)}"/>` +
         `<circle cx="${f1(px - sd * 8 * kf)}" cy="${f1((y1 + pyb) / 2 + 4 * kf)}" r="${f1(19 * kf)}" fill="#2a1a10" stroke="${cptD}" stroke-width="${f1(3 * kf)}"/>` +
         `<ellipse cx="${f1(px - sd * 6 * kf)}" cy="${f1((y1 + pyb) / 2 + 12 * kf)}" rx="${f1(4 * kf)}" ry="${f1(2.5 * kf)}" fill="#ffd166" opacity="0.8"/>` +
         `<ellipse cx="${f1(px - sd * 12 * kf)}" cy="${f1((y1 + pyb) / 2 + 12 * kf)}" rx="${f1(4 * kf)}" ry="${f1(2.5 * kf)}" fill="#ffd166" opacity="0.8"/>`;
    tt += plat(px + sd * 20 * kf, Yh(0.38), 40 * kf);
    const [hx, hy] = [px + sd * 50 * kf, y2 + 7 * kf];
    // (the pompom, and the feather on the walkway: the scene's slow things, swinging with the slow LFO)
    tt += `<g>${sceneLfo(c.o, "slow", sd < 0 ? 0.1 : 0.55, "transform", (v, amp) => `${f1((8 + 18 * amp) * v)} ${f1(hx)} ${f1(hy)}`,
           `<animateTransform attributeName="transform" type="rotate" values="-14 ${f1(hx)} ${f1(hy)};14 ${f1(hx)} ${f1(hy)};-14 ${f1(hx)} ${f1(hy)}" dur="${sd < 0 ? 2.2 : 2.7}s" repeatCount="indefinite"/>`)}` +
         `<line x1="${f1(hx)}" y1="${f1(hy)}" x2="${f1(hx)}" y2="${f1(hy + 40 * kf)}" stroke="#e8e0d0" stroke-width="${f1(1 * kf)}"/><circle cx="${f1(hx)}" cy="${f1(hy + 46 * kf)}" r="${f1(7 * kf)}" fill="${sd < 0 ? "#e5484d" : "#3b82f6"}"/>` +
         [...Array(6)].map((_, j) => `<line x1="${f1(hx)}" y1="${f1(hy + 46 * kf)}" x2="${f1(hx + Math.cos(j) * 9 * kf)}" y2="${f1(hy + 46 * kf + Math.sin(j) * 9 * kf)}" stroke="${sd < 0 ? "#e5484d" : "#3b82f6"}" stroke-width="${f1(2 * kf)}"/>`).join("") + `</g>`;
    // (the top: a round bed with a rim, and a ramp up to the first shelf on the wall)
    tt += plat(px, Yh(0.56), 38 * kf) + `<path d="M${P(px - 36 * kf, y3)} Q${P(px, y3 - 16 * kf)} ${P(px + 36 * kf, y3)}" fill="none" stroke="${cptD}" stroke-width="${f1(6 * kf)}" stroke-linecap="round"/>`;
    const [ra, rb] = [at(X + sd * 0.04 * bw, Yh(0.56), -0.03), at(sd * (bw - 0.035 * bw), Yh(0.8), -0.09)];
    tt += `<line x1="${f1(ra[0])}" y1="${f1(ra[1])}" x2="${f1(rb[0])}" y2="${f1(rb[1])}" stroke="${wood}" stroke-width="${f1(9 * k)}" stroke-linecap="round"/>` +
         [0.2, 0.4, 0.6, 0.8].map(u => `<line x1="${f1(ra[0] + (rb[0] - ra[0]) * u - 4 * k)}" y1="${f1(ra[1] + (rb[1] - ra[1]) * u)}" x2="${f1(ra[0] + (rb[0] - ra[0]) * u + 4 * k)}" y2="${f1(ra[1] + (rb[1] - ra[1]) * u)}" stroke="${woodD}" stroke-width="${f1(2.5 * k)}"/>`).join("");
    t += obj(`cat tree, ${sd < 0 ? "left" : "right"}`, pyb, tt);
  }
  // on the floor: bowls of fish and water on a mat, the basket, the box, yarn, a toy mouse
  { const [bx, by] = at(0.64 * bw, floor, -0.15), s = k / 0.85;
    t += obj("bowls", by, [[-16, "#e5484d", "fish"], [16, "#3b82f6", "water"]].map(([dx, col, what]) =>
           `<path d="M${P(bx + dx * s - 13 * s, by - 6 * s)} L${P(bx + dx * s + 13 * s, by - 6 * s)} L${P(bx + dx * s + 10 * s, by + 3 * s)} L${P(bx + dx * s - 10 * s, by + 3 * s)} Z" fill="${col}"/>` +
           `<ellipse cx="${f1(bx + dx * s)}" cy="${f1(by - 6 * s)}" rx="${f1(13 * s)}" ry="${f1(3.5 * s)}" fill="${what === "fish" ? "#a8642a" : "#8fd3ff"}"/>` +
           (what === "fish" ? [...Array(6)].map((_, j) => `<circle cx="${f1(bx + dx * s + (j - 2.5) * 3.6 * s)}" cy="${f1(by - 7 * s + (j % 2) * 1.5 * s)}" r="${f1(1.6 * s)}" fill="#7a4418"/>`).join("")
             : `<ellipse cx="${f1(bx + dx * s)}" cy="${f1(by - 6 * s)}" rx="${f1(6 * s)}" ry="${f1(1.6 * s)}" fill="none" stroke="#ffffff" stroke-width="${f1(0.8 * s)}" opacity="0.8"><animate attributeName="rx" values="${f1(2 * s)};${f1(11 * s)}" dur="2.6s" repeatCount="indefinite"/><animate attributeName="opacity" values="0.9;0" dur="2.6s" repeatCount="indefinite"/></ellipse>`)).join("")); }
  { const { kx, ky, kfb } = L;
    t += obj("basket", ky, `<ellipse cx="${f1(kx)}" cy="${f1(ky)}" rx="${f1(46 * kfb)}" ry="${f1(10 * kfb)}" fill="#000000" opacity="0.2"/>` +
         `<ellipse cx="${f1(kx)}" cy="${f1(ky - 22 * kfb)}" rx="${f1(46 * kfb)}" ry="${f1(12 * kfb)}" fill="#7a5a2a"/>` +
         `<ellipse cx="${f1(kx)}" cy="${f1(ky - 19 * kfb)}" rx="${f1(40 * kfb)}" ry="${f1(9 * kfb)}" fill="${K.flagB}"/>` + L.basketFront()); }
  { const { ox, oy, kfo } = L;
    t += obj("box", oy, `<ellipse cx="${f1(ox)}" cy="${f1(oy)}" rx="${f1(42 * kfo)}" ry="${f1(6 * kfo)}" fill="#000000" opacity="0.2"/>` +
         `<path d="M${P(ox - 34 * kfo, oy - 40 * kfo)} L${P(ox - 26 * kfo, oy - 50 * kfo)} L${P(ox + 26 * kfo, oy - 50 * kfo)} L${P(ox + 34 * kfo, oy - 40 * kfo)} Z" fill="#5a4022"/>` +
         `<path d="M${P(ox - 26 * kfo, oy - 50 * kfo)} L${P(ox - 22 * kfo, oy - 66 * kfo)} L${P(ox + 22 * kfo, oy - 66 * kfo)} L${P(ox + 26 * kfo, oy - 50 * kfo)} Z" fill="#b8905a" stroke="#8a6a3a" stroke-width="${f1(1.2 * kfo)}"/>` + L.boxFront()); }
  for (const [X, z, col, i] of [[-0.42 * bw, -0.42, "#e5484d", 0], [0.18 * bw, -0.5, "#22c55e", 1], [-0.12 * bw, -0.26, "#f2c12e", 2]]) {
    const [yx, yy] = at(X, floor, z), r = 9 * k / (1 + z);
    t += obj(`yarn, ${["red", "green", "yellow"][i]}`, yy, `<path d="M${P(yx + r * 0.6, yy - r * 0.3)} q${f1(r * 2)} ${f1(r * 0.8)} ${f1(r * 3.2)} ${f1(r * 0.2)} t${f1(r * 2.6)} ${f1(r * 0.6)}" fill="none" stroke="${col}" stroke-width="${f1(1.2 * k)}"/>` +
         `<circle cx="${f1(yx)}" cy="${f1(yy - r)}" r="${f1(r)}" fill="${col}"/>` +
         [-0.5, 0, 0.5].map(a => `<path d="M${P(yx - r * Math.cos(a + 1), yy - r - r * Math.sin(a + 1) * 0.5)} Q${P(yx, yy - r * (1.4 + a))} ${P(yx + r * Math.cos(a + 1), yy - r + r * Math.sin(a + 1) * 0.5)}" fill="none" stroke="${mix(col, "#000000", 0.3)}" stroke-width="${f1(0.9 * k)}"/>`).join(""));
  }
  { const [mx, my] = at(-0.5 * bw, floor, -0.36), s = k / 0.64;
    t += obj("toy mouse", my, `<ellipse cx="${f1(mx)}" cy="${f1(my - 5 * s)}" rx="${f1(9 * s)}" ry="${f1(5.5 * s)}" fill="#9ca3af"/><circle cx="${f1(mx + 9 * s)}" cy="${f1(my - 6 * s)}" r="${f1(4 * s)}" fill="#9ca3af"/>` +
         `<circle cx="${f1(mx + 7 * s)}" cy="${f1(my - 10 * s)}" r="${f1(2.6 * s)}" fill="#f4a0b0"/><circle cx="${f1(mx + 11.5 * s)}" cy="${f1(my - 6.5 * s)}" r="${f1(0.9 * s)}" fill="#111"/>` +
         `<path d="M${P(mx - 9 * s, my - 4 * s)} q${f1(-10 * s)} ${f1(2 * s)} ${f1(-14 * s)} ${f1(-6 * s)}" fill="none" stroke="#c9ccd4" stroke-width="${f1(1.4 * s)}"/>`); }
  return t;
}
// The cat room's cats, each at its place, drawn farthest first, the rope
// bridge among them at its own depth (so a cat on a far shelf is behind it,
// and one on it in front), the basket's and the box's near sides over a cat
// sitting in them. (ticket 99) The bridge, and each cat, an object at the
// depth of the floor under it (a cat on a perch, where the perch stands; on
// the window seat, at the seat's front, catRoomSvg's, so it sits on it)
function catRoomCats(c, K, wags) {
  const L = catRoomPlaces(c), { k } = c, items = [{ z: L.BZ, svg: obj("rope bridge", L.at(0, L.floor, L.BZ)[1], L.bridge()) }];
  K.cats.forEach((C, i) => {
    const sp = L.spots[C.spot] || L.spots.rug, s = 0.85 * k * sp.f * C.size / 100, W = wags[i];
    const WC = { catFur: C.fur, catWhite: C.white, catEyes: C.eyes, catNose: "#f4a0b0" };
    const at = (inner, dy = 0) => `<g transform="translate(${f1(sp.x)} ${f1(sp.y + dy)}) scale(${s.toFixed(3)})">${inner}</g>`;
    let svg;
    if (C.pose === "sleep") svg = at(sleepingCat(WC, W, i));
    else if (C.pose === "walk") {
      // (pacing to and fro along its place, turning round at each end, about 26 of its own units a second)
      const u = Math.max(10, sp.span / s), dur = (2 * u / 26).toFixed(2), b = `-${((i * 1.7) % +dur).toFixed(2)}s`;
      svg = at(`<g><animateTransform attributeName="transform" type="translate" values="${f1(-u)} 0;${f1(u)} 0;${f1(-u)} 0" dur="${dur}s" begin="${b}" repeatCount="indefinite"/>` +
        `<g><animateTransform attributeName="transform" type="scale" values="1 1;-1 1" keyTimes="0;0.5" calcMode="discrete" dur="${dur}s" begin="${b}" repeatCount="indefinite"/>${walkingCat(WC, W, i)}</g></g>`);
    } else svg = (sp.edge ? "" : `<ellipse cx="${f1(sp.x)}" cy="${f1(sp.y)}" rx="${f1(18 * s)}" ry="${f1(3.5 * s)}" fill="#000000" opacity="0.18"/>`) + at(shoulderCat(4, WC, W, !sp.edge), 7 * s);
    if (C.spot === "basket") svg += L.basketFront();
    if (C.spot === "box") svg += L.boxFront();
    items.push({ z: sp.z, svg: obj(`cat ${i + 1}`, L.at(0, L.floor, C.spot === "window" ? -0.09 : sp.z)[1], svg) });
  });
  return items.sort((a, b) => b.z - a.z).map(e => e.svg).join("");
}
// A horse's face, from in front, as the stable's horses look out of their stalls: its ears, its long face with its
// marking, its muzzle, its eyes at the sides, its forelock, a halter (or bridle) in `halter`. `still` (ticket 438: the
// friend's horse, facing us): no blinking, no ear flicks, no munching, no hay. (0, 0) as for horseHead.
// (ticket 497, for M) the forelock between the ears, seen from the front, in the mane's style (H.maneStyle; the stable's
// horses have none, so theirs is loose); a bow in H.bow
function forelock(H, mane, maneD) {
  const s = H.maneStyle || "loose";
  if (s === "long") return `<path d="M-10 -161 Q-21 -147 -16 -126 Q-10 -140 -2 -156 Q0 -158 2 -156 Q10 -140 16 -126 Q21 -147 10 -161 Q0 -166 -10 -161 Z" fill="${mane}" stroke="${maneD}" stroke-width="1"/>`;
  if (s === "brush") return `<path d="M-8 -158 L-7 -171 L-3 -161 L0 -174 L3 -161 L7 -171 L8 -158 Q0 -162 -8 -158 Z" fill="${mane}" stroke="${maneD}" stroke-width="1" stroke-linejoin="round"/>`;
  if (s === "buttons") return maneKnot([0, -158], 5, mane, maneD);
  if (s === "curly") return maneCurl([-6, -157], 4.5, mane, maneD) + maneCurl([6, -157], 4.5, mane, maneD) + maneCurl([0, -150], 4.5, mane, maneD);
  if (s === "bows") return maneBraid([0, -161], [0, 1], 20, mane, maneD, H.bow || "#e0559a");
  return `<path d="M-9 -160 Q-16 -146 -7 -134 Q-3 -146 0 -159 Q4 -145 10 -137 Q14 -150 9 -160 Q0 -165 -9 -160 Z" fill="${mane}"/>`;
}
function horseFace(H, i, halter, still) {
  const coat = H.coat, ln = mix(coat, "#000000", 0.42), mane = H.mane, maneD = mix(mane, "#000000", 0.3), sh = mix(coat, "#000000", 0.2);
  const light = hexToRgb(coat).reduce((a, b) => a + b, 0) > 520, muz = light ? "#b9a6a0" : mix(coat, "#2a2420", 0.4), white = "#f7f4ee";
  const blink = (3.6 + 0.9 * i).toFixed(1), ear = (3.1 + 0.8 * ((i * 3) % 4)).toFixed(1), chew = (0.55 + 0.08 * i).toFixed(2), A = x => still ? "" : x;
  const earAt = sd => `<g>${poly([[sd * 9, -155], [sd * 21, -184], [sd * 21, -152]], coat, ln, 1.5)}${poly([[sd * 12, -156], [sd * 19.5, -178], [sd * 19, -155]], mix(coat, "#e8a0a0", 0.35), mix(coat, "#e8a0a0", 0.35), 0.5)}` +
    A(`<animateTransform attributeName="transform" type="rotate" values="0 ${sd * 15} -155;0 ${sd * 15} -155;${sd * 24} ${sd * 15} -155;0 ${sd * 15} -155" keyTimes="0;0.82;0.88;1" dur="${ear}s" begin="-${(i * 1.3 + (sd > 0 ? 0.9 : 0)).toFixed(1)}s" repeatCount="indefinite"/>`) + `</g>`;
  const mark = H.mark === "star" ? `<path d="M0 -148 L4.5 -140 L0 -132 L-4.5 -140 Z" fill="${H.markColor || white}"/>`
    : H.mark === "stripe" ? `<path d="M-2.5 -148 Q0 -150 2.5 -148 L3 -76 Q0 -73 -3 -76 Z" fill="${H.markColor || white}"/>`
    : H.mark === "blaze" ? `<path d="M-8 -152 Q0 -157 8 -152 L5 -100 Q4 -86 9 -70 L-9 -70 Q-4 -86 -5 -100 Z" fill="${H.markColor || white}"/>` : "";
  // (eyes at the sides of the head, standing out a little past it, as a horse's do)
  const eye = sd => `<ellipse cx="${sd * 22}" cy="-128" rx="4.8" ry="6.2" fill="#1f1410" stroke="${ln}" stroke-width="0.8">` + A(`<animate attributeName="ry" values="6.2;6.2;0.8;6.2" keyTimes="0;0.93;0.965;1" dur="${blink}s" begin="-${(i * 0.7).toFixed(1)}s" repeatCount="indefinite"/>`) + `</ellipse>` +
    `<circle cx="${sd * 22 + 1.6}" cy="-130.5" r="1.5" fill="#ffffff"/>` +
    `<path d="M${sd * 17} -135 Q${sd * 22} -139 ${sd * 27} -134" fill="none" stroke="${ln}" stroke-width="1.1"/>`;
  const hay = !still && i % 2 ? `<g>${[[-4, 9, -5], [0, 11, 1], [4, 10, 6]].map(([x, l, dx]) => `<line x1="${x}" y1="-48" x2="${x + dx}" y2="${-48 + l}" stroke="#e3c46a" stroke-width="1.6"/>`).join("")}` +
    `<animateTransform attributeName="transform" type="rotate" values="-6 0 -48;6 0 -48;-6 0 -48" dur="${chew}s" repeatCount="indefinite"/></g>` : "";
  return earAt(-1) + earAt(1) +
    `<path d="M-16 -157 Q-25 -151 -23 -136 Q-22 -116 -15 -96 Q-12 -84 -13 -74 Q-18 -62 -13 -50 Q0 -43 13 -50 Q18 -62 13 -74 Q12 -84 15 -96 Q22 -116 23 -136 Q25 -151 16 -157 Q0 -163 -16 -157 Z" fill="${coat}" stroke="${ln}" stroke-width="1.5"/>` +
    `<path d="M-15 -112 Q-11 -98 -10 -84 M15 -112 Q11 -98 10 -84" fill="none" stroke="${sh}" stroke-width="1.4" opacity="0.6"/>` + mark +
    `<ellipse cx="0" cy="-59" rx="14.5" ry="11.5" fill="${H.mark === "blaze" ? mix(muz, "#f0d8d0", 0.5) : muz}"/>` +
    [-1, 1].map(sd => `<path d="M${sd * 5} -65 q${sd * 4} -2 ${sd * 5} 4 q${sd * -1} 3 ${sd * -4} 1" fill="#1f1410"/>`).join("") +
    hay + `<g><path d="M-6 -50 Q0 -47.5 6 -50" fill="none" stroke="#1f1410" stroke-width="1.2"/>` +
    A(`<animateTransform attributeName="transform" type="translate" values="0 0;0 1.2;0 0" dur="${chew}s" repeatCount="indefinite"/>`) + `</g>` +
    eye(-1) + eye(1) +
    forelock(H, mane, maneD) +
    // (its halter: a noseband in the castle's colors, its straps going back round the cheeks)
    [-1, 1].map(sd => `<path d="M${sd * 12.5} -82 Q${sd * 16} -98 ${sd * 18} -112" fill="none" stroke="${mix(halter, "#3a2414", 0.45)}" stroke-width="3" stroke-linecap="round"/>`).join("") +
    `<path d="M-13.5 -84 Q0 -76 13.5 -84" fill="none" stroke="${halter}" stroke-width="4.5" stroke-linecap="round"/>` +
    `<path d="M-13.5 -84 Q0 -76 13.5 -84" fill="none" stroke="${mix(halter, "#000000", 0.35)}" stroke-width="0.8" stroke-dasharray="2 2"/>` +
    `<circle cx="0" cy="-77" r="3" fill="none" stroke="#e8b923" stroke-width="1.6"/>`;
}
// A horse looking out over its stall's half door, (0, 0) where its neck meets
// the door's top: its neck and mane, its long face with its marking, a halter
// in the castle's colors. It nods with its wag (an LFO, or its own pace,
// begun its own way in), looks round now and then, flicks an ear, blinks,
// and munches a wisp of hay; each horse in its own time.
function horseHead(H, W, i, halter) {
  const coat = H.coat, ln = mix(coat, "#000000", 0.42), mane = H.mane, maneD = mix(mane, "#000000", 0.3), sh = mix(coat, "#000000", 0.2);
  const look = (6 + 1.4 * (i % 3)).toFixed(1);
  // (the neck, behind the head, and the mane down its side, swaying a little)
  const t = `<path d="M-20 8 Q-21 -40 -12 -104 L12 -104 Q21 -40 20 8 Z" fill="${coat}" stroke="${ln}" stroke-width="1.5"/>` +
    `<path d="M10 -96 Q17 -44 16 6" fill="none" stroke="${sh}" stroke-width="5" opacity="0.5"/>` +
    `<g>${[...Array(10)].map((_, j) => `<path d="M${f1(-11 - j * 0.85)} ${-104 + j * 11} q-10 4 -14 13 q8 -3 14 -6 Z" fill="${mane}" stroke="${maneD}" stroke-width="0.6"/>`).join("")}` +
    `<animateTransform attributeName="transform" type="rotate" values="0 -12 -100;-3 -12 -100;0 -12 -100" dur="${(2.4 + 0.35 * i).toFixed(2)}s" repeatCount="indefinite"/></g>`;
  const head = horseFace(H, i, halter, false);
  // (the nod: down and back up once a wag; looking round: a turn about the throat)
  return t + wagged(W, s => `<g>${wagAnim(W, "transform", (v, amp) => `0 ${f1((3 + 9 * amp) * (1 + v) / 2)}`, s, "translate")}` +
    `<g><animateTransform attributeName="transform" type="rotate" values="0 0 -70;0 0 -70;-9 0 -70;-9 0 -70;0 0 -70;0 0 -70;8 0 -70;8 0 -70;0 0 -70" dur="${look}s" begin="-${(i * 1.9).toFixed(1)}s" repeatCount="indefinite"/>${head}</g></g>`);
}
// (ticket 502, for M: the 🚜 farm yard and the 🐔 chicken coop have the stable's hens and its water trough, drawn by the
// same code, so they look the same everywhere; Matthew: "chickens -- already in stable") The stone water trough at (tx, ty),
// s its size, its water rippling (P the scene's point writer)
const troughSvg = (P, tx, ty, s) => `<ellipse cx="${f1(tx)}" cy="${f1(ty)}" rx="${f1(58 * s)}" ry="${f1(7 * s)}" fill="#000000" opacity="0.2"/>` +
  `<path d="M${P(tx - 52 * s, ty - 30 * s)} L${P(tx - 44 * s, ty - 40 * s)} L${P(tx + 44 * s, ty - 40 * s)} L${P(tx + 52 * s, ty - 30 * s)} Z" fill="#7d7468"/>` +
  `<path d="M${P(tx - 44 * s, ty - 36 * s)} L${P(tx + 44 * s, ty - 36 * s)} L${P(tx + 48 * s, ty - 31 * s)} L${P(tx - 48 * s, ty - 31 * s)} Z" fill="#6aa6c8"/>` +
  `<rect x="${f1(tx - 52 * s)}" y="${f1(ty - 30 * s)}" width="${f1(104 * s)}" height="${f1(30 * s)}" fill="#9a9082" stroke="#6a6258" stroke-width="${f1(1.2 * s)}"/>` +
  [0.25, 0.5, 0.75].map(u => `<line x1="${f1(tx - 52 * s + 104 * s * u)}" y1="${f1(ty - 30 * s)}" x2="${f1(tx - 52 * s + 104 * s * u)}" y2="${f1(ty)}" stroke="#6a6258" stroke-width="${f1(1 * s)}"/>`).join("") +
  `<ellipse cx="${f1(tx)}" cy="${f1(ty - 33.5 * s)}" rx="${f1(8 * s)}" ry="${f1(1.4 * s)}" fill="none" stroke="#ffffff" stroke-width="${f1(0.8 * s)}"><animate attributeName="rx" values="${f1(3 * s)};${f1(40 * s)}" dur="3s" repeatCount="indefinite"/><animate attributeName="opacity" values="0.8;0" dur="3s" repeatCount="indefinite"/></ellipse>`;
// A hen at (hx, hy), s her size, sd 1 facing right or -1 left; i her own pace and way into her peck, which the scene's quick
// LFO times; look 0 brown, 1 white (the stable's two), 2 black
const HEN_LOOKS = [["#a0522d", "#6b3418"], ["#f5efe2", "#c9bca2"], ["#3b3836", "#1c1a19"]];
function henSvg(o, hx, hy, s, sd, i, look = i ? 1 : 0) {
  const pk = (1.6 + 0.5 * i).toFixed(1), [fill, line] = HEN_LOOKS[look] || HEN_LOOKS[0];
  return `<g transform="translate(${f1(hx)} ${f1(hy)}) scale(${(sd * s).toFixed(3)} ${s.toFixed(3)})">` +
    `<ellipse cx="0" cy="0" rx="14" ry="2.5" fill="#000000" opacity="0.18"/>` +
    `<line x1="-3" y1="-8" x2="-4" y2="0" stroke="#e8a33a" stroke-width="1.6"/><line x1="3" y1="-8" x2="4" y2="0" stroke="#e8a33a" stroke-width="1.6"/>` +
    `<path d="M-16 -18 Q-20 -30 -12 -28 Q-6 -14 6 -14 Q14 -14 14 -22 L10 -8 Q0 -2 -10 -8 Z" fill="${fill}" stroke="${line}" stroke-width="1"/>` +
    `<path d="M-6 -16 Q0 -12 6 -15" fill="none" stroke="${line}" stroke-width="1"/>` +
    // (a peck, down and back, once round the quick LFO: the scene's quick things)
    `<g>${sceneLfo(o, "fast", i * 0.45, "transform", (v, amp) => `${f1(Math.max(0, v) * (30 + 50 * amp))} 10 -18`,
      `<animateTransform attributeName="transform" type="rotate" values="0 10 -18;0 10 -18;55 10 -18;0 10 -18;0 10 -18;55 10 -18;0 10 -18" keyTimes="0;0.4;0.47;0.54;0.75;0.82;1" dur="${pk}s" begin="-${(i * 0.7).toFixed(1)}s" repeatCount="indefinite"/>`)}` +
    `<circle cx="14" cy="-24" r="5" fill="${fill}"/><path d="M12 -29 q2 -4 4 0 q2 -4 3 1 Z" fill="#d62828"/><path d="M18 -24 l4 1 l-4 1 Z" fill="#e8a33a"/>` +
    `<path d="M15 -21 q1 3 -1 4" fill="#d62828"/><circle cx="15.5" cy="-25" r="0.9" fill="#111"/></g></g>`;
}
// The stable: the castle's stone, under timber; four stalls along the back
// wall, each a half door with its top door swung open, a horse looking out
// over it (or, empty, a blanket over the door), its name plate, a bucket; a
// hay loft over the stalls, bales stacked on it and straw hanging off its
// edge, a ladder up; a round window high up, sunbeams slanting down through
// drifting dust, swallows flying round the beams; horseshoes, saddles and
// bridles on the side walls; straw all over the floor, a water trough, a
// heap of hay with a pitchfork in it, buckets, and hens pecking about (two
// to start with; ticket 506: as many as its 🐔 setting says, up to 30).
function stablePlaces(c) {
  const { k } = c, { floor, top, bw, vy, wh, at, Yh, onWall } = roomSpace(c);
  const hb = 0.205 * bw;
  // (the stalls' middles, left to right, and the order horses take them: the outer ones first, clear of the friend)
  const bays = [-0.72, -0.24, 0.24, 0.72].map(u => u * bw), order = [0, 3, 1, 2];
  const doorTop = Yh(0.3), lintel = Yh(0.64);
  const rail = X => { const [xa, ya] = at(X - hb, doorTop, -0.01), [xb] = at(X + hb, doorTop, -0.01);
    return `<rect x="${f1(xa)}" y="${f1(ya - 2 * k)}" width="${f1(xb - xa)}" height="${f1(9 * k)}" fill="#6b4527" stroke="#3e2714" stroke-width="${f1(1 * k)}"/>`; };
  // (ticket 506: where the water trough, the hay heap and the ladder stand on the floor, for the hens to keep off them too)
  const trough = at(-0.66 * bw, floor, -0.3), hay = at(0.6 * bw, floor, -0.4), ladderX = -0.985 * bw;
  return { at, Yh, hb, bays, order, doorTop, lintel, rail, floor, top, bw, vy, wh, onWall, trough, hay, ladderX };
}
// (ticket 506) the stable's first two hens, where they always stood: [across, as a share of the back wall's half width; z;
// facing; their own pace and way into a peck (henSvg's i)]
const STABLE_HENS_FIRST = [[-0.42, -0.2, 1, 0], [0.36, -0.3, -1, 1]];
function stableSvg(c, K, hens = STABLE_HENS) {
  const { x0, x1, y0, y1, W, H, k, pad, R, P } = c, L = stablePlaces(c), { at, Yh, hb, bays, order, doorTop, lintel, floor, top, bw, vy, wh, onWall } = L;
  const oak = "#7a5230", oakD = "#4e341d", oakL = "#9c6b40", straw = "#e3c46a", strawD = "#b8963e", iron = "#2e2e36";
  const pg = (pts, fill, more = "") => `<polygon points="${pts.map(p => P(...p)).join(" ")}" fill="${fill}"${more}/>`;
  const has = new Set(order.slice(0, K.horses.length));
  let t = roomShell(c, "#b0a593", { floor: "#8a7656" }) + castleFlagDefs(K, 8 * k);
  t += `<defs><linearGradient id="bg-beam" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#fff3c4" stop-opacity="0.38"/><stop offset="1" stop-color="#fff3c4" stop-opacity="0.04"/></linearGradient></defs>`;
  // the round window high in the back wall, the sky through it; sunbeams down through the dust
  const [ox, oy] = at(0, Yh(0.88)), orr = 0.09 * bw;
  const mWindow = t.length;   // (ticket 284: the round window, the stalls, the hay loft, the lantern, the horseshoes, the saddles, the bridle and the straw, each a wall or floor object)
  t += `<circle cx="${f1(ox)}" cy="${f1(oy)}" r="${f1(orr + 9 * k)}" fill="#8f8676"/><circle cx="${f1(ox)}" cy="${f1(oy)}" r="${f1(orr)}" fill="#bfe6ff"/>` +
       `<circle cx="${f1(ox - orr * 0.3)}" cy="${f1(oy + orr * 0.2)}" r="${f1(orr * 0.35)}" fill="#ffffff" opacity="0.85"/>` +
       `<line x1="${f1(ox - orr)}" y1="${f1(oy)}" x2="${f1(ox + orr)}" y2="${f1(oy)}" stroke="${oakD}" stroke-width="${f1(4 * k)}"/><line x1="${f1(ox)}" y1="${f1(oy - orr)}" x2="${f1(ox)}" y2="${f1(oy + orr)}" stroke="${oakD}" stroke-width="${f1(4 * k)}"/>`;
  t = wallSince(t, mWindow, "round window");
  // the beams across the ceiling, under the vault
  for (const z of [0, -0.18, -0.38]) { const f = 1 / (1 + z), [xa, ya] = at(-bw, top + 30 * k, z);
    t += `<rect x="${f1(xa)}" y="${f1(ya)}" width="${f1(-2 * xa)}" height="${f1(18 * k * f)}" fill="${oak}" stroke="${oakD}" stroke-width="${f1(1.5 * k)}"/>`; }
  // the posts between the stalls, up to the loft; braces to its beam
  const loftY = Yh(0.7);
  for (const u of [-0.94, -0.48, 0, 0.48, 0.94]) { const X = u * bw, [xa, ya] = at(X - 8 * k, loftY, -0.01), [xb, yb] = at(X + 8 * k, floor, -0.01);
    t += `<rect x="${f1(xa)}" y="${f1(ya)}" width="${f1(xb - xa)}" height="${f1(yb - ya)}" fill="${oak}" stroke="${oakD}" stroke-width="${f1(1.2 * k)}"/>` +
         `<line x1="${f1(xa + 4 * k)}" y1="${f1(ya + 10 * k)}" x2="${f1(xa + 5 * k)}" y2="${f1(yb - 10 * k)}" stroke="${oakD}" stroke-width="${f1(0.8 * k)}" opacity="0.5"/>`;
    if (Math.abs(u) < 0.9) for (const sd of [-1, 1]) t += `<line x1="${f1((xa + xb) / 2)}" y1="${f1(ya + 34 * k)}" x2="${f1((xa + xb) / 2 + sd * 30 * k)}" y2="${f1(ya + 4 * k)}" stroke="${oak}" stroke-width="${f1(6 * k)}"/>`; }
  // each stall: the dark inside, a hay net, the lintel, the top door swung open, the half door
  bays.forEach((X, j) => {
    const mStall = t.length;
    const [xa, ya] = at(X - hb, lintel, 0), [xb, yb] = at(X + hb, doorTop, 0);
    t += `<rect x="${f1(xa)}" y="${f1(ya)}" width="${f1(xb - xa)}" height="${f1(yb - ya)}" fill="#24180e"/>`;
    for (let i = 1; i < 5; i++) t += `<line x1="${f1(xa + (xb - xa) * i / 5)}" y1="${f1(ya)}" x2="${f1(xa + (xb - xa) * i / 5)}" y2="${f1(yb)}" stroke="#3a2818" stroke-width="${f1(1 * k)}"/>`;
    // (a hay net on the stall's outer side, in the shadow, wisps poking out of it)
    const nx = X < 0 ? xa + 20 * k : xb - 20 * k, ny = ya + 44 * k, nr = 11 * k;
    t += `<line x1="${f1(nx)}" y1="${f1(ya)}" x2="${f1(nx)}" y2="${f1(ny - nr * 1.3)}" stroke="#5a3e22" stroke-width="${f1(1.2 * k)}"/>` +
         `<path d="M${f1(nx - nr)} ${f1(ny - nr)} Q${f1(nx)} ${f1(ny - nr * 1.6)} ${f1(nx + nr)} ${f1(ny - nr)} Q${f1(nx + nr * 1.1)} ${f1(ny + nr * 0.6)} ${f1(nx)} ${f1(ny + nr * 1.2)} Q${f1(nx - nr * 1.1)} ${f1(ny + nr * 0.6)} ${f1(nx - nr)} ${f1(ny - nr)} Z" fill="${strawD}"/>` +
         [...Array(5)].map((_, q) => `<line x1="${f1(nx - nr + q * nr / 2)}" y1="${f1(ny - nr * 1.2)}" x2="${f1(nx - nr * 0.4 + q * nr * 0.2)}" y2="${f1(ny + nr * 1.1)}" stroke="#4a3a24" stroke-width="${f1(0.7 * k)}"/>`).join("") +
         [...Array(3)].map((_, q) => `<line x1="${f1(nx - nr)}" y1="${f1(ny - nr * 0.5 + q * nr * 0.6)}" x2="${f1(nx + nr)}" y2="${f1(ny - nr * 0.5 + q * nr * 0.6)}" stroke="#4a3a24" stroke-width="${f1(0.7 * k)}"/>`).join("") +
         [[-1, 0.2], [1, -0.1], [-0.6, 0.9], [0.8, 0.7]].map(([dx, dy]) => `<line x1="${f1(nx + dx * nr)}" y1="${f1(ny + dy * nr)}" x2="${f1(nx + dx * nr * 1.5)}" y2="${f1(ny + dy * nr + 4 * k)}" stroke="${straw}" stroke-width="${f1(1 * k)}"/>`).join("");
    t += `<rect x="${f1(xa - 4 * k)}" y="${f1(ya - 10 * k)}" width="${f1(xb - xa + 8 * k)}" height="${f1(12 * k)}" fill="${oak}" stroke="${oakD}" stroke-width="${f1(1 * k)}"/>`;
    // (the top door, open toward us against the post on its outer side)
    const sd = X < 0 ? -1 : 1, hx = X + sd * hb, [p1, p2, p3, p4] = [at(hx, lintel, -0.005), at(hx, doorTop - 3 * k, -0.005), at(hx, doorTop - 3 * k, -0.075), at(hx, lintel, -0.075)];
    t += pg([p1, p2, [p2[0] - sd * 26 * k, p3[1]], [p1[0] - sd * 26 * k, p4[1]]], oakL, ` stroke="${oakD}" stroke-width="${f1(1.2 * k)}"`);
    // (the half door: planks, a Z brace, strap hinges, a latch, a name plate, a bucket)
    const [da, dt] = at(X - hb, doorTop, -0.01), [db, dfl] = at(X + hb, floor, -0.01);
    t += `<rect x="${f1(da)}" y="${f1(dt)}" width="${f1(db - da)}" height="${f1(dfl - dt)}" fill="${oakL}" stroke="${oakD}" stroke-width="${f1(1.5 * k)}"/>`;
    for (let i = 1; i < 6; i++) t += `<line x1="${f1(da + (db - da) * i / 6)}" y1="${f1(dt)}" x2="${f1(da + (db - da) * i / 6)}" y2="${f1(dfl)}" stroke="${oakD}" stroke-width="${f1(1 * k)}" opacity="0.7"/>`;
    t += `<path d="M${P(da + 6 * k, dt + 14 * k)} H${f1(db - 6 * k)} L${P(da + 6 * k, dfl - 14 * k)} H${f1(db - 6 * k)}" fill="none" stroke="${oak}" stroke-width="${f1(9 * k)}" stroke-linejoin="round"/>` +
         [dt + 18 * k, dfl - 18 * k].map(y => `<rect x="${f1(sd < 0 ? da : db - 34 * k)}" y="${f1(y - 3 * k)}" width="${f1(34 * k)}" height="${f1(6 * k)}" fill="${iron}"/>`).join("") +
         `<rect x="${f1(sd < 0 ? db - 16 * k : da + 4 * k)}" y="${f1((dt + dfl) / 2 - 3 * k)}" width="${f1(12 * k)}" height="${f1(6 * k)}" rx="${f1(2 * k)}" fill="${iron}"/>`;
    const n = order.indexOf(j) + 1;
    t += `<rect x="${f1((da + db) / 2 - 18 * k)}" y="${f1(dt + 24 * k)}" width="${f1(36 * k)}" height="${f1(16 * k)}" rx="${f1(3 * k)}" fill="#c9a227" stroke="#8a6a12" stroke-width="${f1(1 * k)}"/>` +
         `<text x="${f1((da + db) / 2)}" y="${f1(dt + 36 * k)}" text-anchor="middle" font-size="${f1(11 * k)}" font-family="sans-serif" fill="#3a2a0a">🐴 ${n}</text>`;
    if (j % 2 === 0) { const bx = sd < 0 ? db - 30 * k : da + 30 * k;
      t += `<path d="M${P(bx - 12 * k, dt + 50 * k)} L${P(bx + 12 * k, dt + 50 * k)} L${P(bx + 9 * k, dt + 76 * k)} L${P(bx - 9 * k, dt + 76 * k)} Z" fill="#9aa3ad" stroke="#5a626c" stroke-width="${f1(1 * k)}"/>` +
           `<path d="M${P(bx - 12 * k, dt + 50 * k)} Q${P(bx, dt + 34 * k)} ${P(bx + 12 * k, dt + 50 * k)}" fill="none" stroke="#5a626c" stroke-width="${f1(1.5 * k)}"/>`; }
    // (an empty stall: a blanket in the castle's colors over its door)
    if (!has.has(j)) t += `<path d="M${P(da + 8 * k, dt - 4 * k)} H${f1(db - 8 * k)} V${f1(dt + 48 * k)} Q${P((da + db) / 2, dt + 60 * k)} ${P(da + 8 * k, dt + 48 * k)} Z" fill="${castleFlagFill(K, j, 8 * k)}" stroke="${mix(K.flagA, "#000000", 0.35)}" stroke-width="${f1(1.2 * k)}"/>` +
      `<path d="M${P(da + 8 * k, dt + 40 * k)} Q${P((da + db) / 2, dt + 52 * k)} ${P(db - 8 * k, dt + 40 * k)}" fill="none" stroke="#e8b923" stroke-width="${f1(2 * k)}"/>`;
    t = wallSince(t, mStall, `stall ${n}`);
  });
  const mLoft = t.length;
  // the hay loft over the stalls: bales on it, its edge, straw hanging off it, a ladder up
  for (let i = 0; i < 9; i++) { if (i === 4) continue; const X = -0.9 * bw + 1.8 * bw * i / 8, [ba, bt] = at(X - 0.09 * bw, loftY - 34 * k, -0.04), [bb, bbm] = at(X + 0.09 * bw, loftY - 6 * k, -0.04);
    t += `<rect x="${f1(ba)}" y="${f1(bt)}" width="${f1(bb - ba)}" height="${f1(bbm - bt)}" rx="${f1(3 * k)}" fill="${i % 2 ? straw : mix(straw, "#a07a30", 0.25)}" stroke="${strawD}" stroke-width="${f1(1.2 * k)}"/>` +
         [0.3, 0.7].map(u => `<line x1="${f1(ba + (bb - ba) * u)}" y1="${f1(bt)}" x2="${f1(ba + (bb - ba) * u)}" y2="${f1(bbm)}" stroke="#7a5a2a" stroke-width="${f1(1.2 * k)}"/>`).join("") +
         [...Array(5)].map((_, j) => `<line x1="${f1(ba + (bb - ba) * R(i * 7 + j, 1301))}" y1="${f1(bt + (bbm - bt) * R(i * 7 + j, 1302))}" x2="${f1(ba + (bb - ba) * R(i * 7 + j, 1301) + 6 * k)}" y2="${f1(bt + (bbm - bt) * R(i * 7 + j, 1302) - 2 * k)}" stroke="${strawD}" stroke-width="${f1(0.8 * k)}"/>`).join(""); }
  t += pg([at(-bw, loftY, 0), at(bw, loftY, 0), at(bw, loftY, -0.12), at(-bw, loftY, -0.12)], oakD) +
       pg([at(-bw, loftY - 12 * k, -0.12), at(bw, loftY - 12 * k, -0.12), at(bw, loftY, -0.12), at(-bw, loftY, -0.12)], oak, ` stroke="${oakD}" stroke-width="${f1(1.2 * k)}"`);
  for (let i = 0; i < 46; i++) { const X = -bw + 2 * bw * (i + 0.5) / 46, [sx, sy] = at(X, loftY, -0.12), l = (6 + 16 * R(i, 1303)) * k;
    t += `<path d="M${f1(sx)} ${f1(sy)} q${f1((R(i, 1304) - 0.5) * 8 * k)} ${f1(l * 0.6)} ${f1((R(i, 1305) - 0.5) * 10 * k)} ${f1(l)}" fill="none" stroke="${i % 3 ? straw : strawD}" stroke-width="${f1(1.3 * k)}"/>`; }
  t = wallSince(t, mLoft, "hay loft");
  const mLantern = t.length;
  // a lantern on the middle post
  { const [lx, ly] = at(0, Yh(0.56), -0.02), s = k;
    t += `<circle cx="${f1(lx)}" cy="${f1(ly)}" r="${f1(55 * s)}" fill="url(#bg-glow)"/><path d="M${P(lx, ly - 24 * s)} v${f1(6 * s)}" stroke="${iron}" stroke-width="${f1(2 * s)}"/>` +
         `<path d="M${P(lx - 9 * s, ly - 14 * s)} L${P(lx + 9 * s, ly - 14 * s)} L${P(lx + 7 * s, ly + 12 * s)} L${P(lx - 7 * s, ly + 12 * s)} Z" fill="#ffe7a8" stroke="${iron}" stroke-width="${f1(2 * s)}"/>` +
         `<path d="M${P(lx - 12 * s, ly - 14 * s)} L${P(lx, ly - 22 * s)} L${P(lx + 12 * s, ly - 14 * s)} Z" fill="${iron}"/>` +
         `<ellipse cx="${f1(lx)}" cy="${f1(ly)}" rx="${f1(2.4 * s)}" ry="${f1(4.5 * s)}" fill="#ff9a2e"><animate attributeName="ry" values="${f1(4.5 * s)};${f1(3.4 * s)};${f1(5 * s)};${f1(4.5 * s)}" dur="1.1s" repeatCount="indefinite"/></ellipse>`; }
  t = wallSince(t, mLantern, "lantern");
  // the side walls, above their doors: horseshoes nailed up in a row (for luck, ends up), saddles on racks, bridles on pegs
  // (each laid flat on its wall in perspective: onWall)
  const mShoes = t.length;
  for (let j = 0; j < 6; j++)
    t += `<g transform="${onWall(-1, Yh(0.74), -0.05 - 0.055 * j)}"><path d="M-7 -8 Q-8 9 0 9 Q8 9 7 -8" fill="none" stroke="#7d8590" stroke-width="4" stroke-linecap="round"/>` +
         `<circle cx="0" cy="-12" r="1.4" fill="${iron}"/></g>`;
  t = wallSince(t, mShoes, "horseshoes");
  const mSaddles = t.length;
  for (const [z, col] of [[-0.09, "#7a3e1e"], [-0.25, "#4a2a16"]])
    t += `<g transform="${onWall(1, Yh(0.72), z)}"><rect x="-2" y="-4" width="16" height="5" fill="${oakD}"/>` +
         `<path d="M-22 0 Q-10 -14 2 -6 Q14 -16 20 -2 L14 18 L-18 18 Z" fill="${col}" stroke="#2a160a" stroke-width="1"/>` +
         `<path d="M-4 18 v14 M-9 34 q5 -6 10 0" fill="none" stroke="#9aa3ad" stroke-width="1.6"/></g>`;
  t = wallSince(t, mSaddles, "saddles");
  const mBridle = t.length;
  t += `<g transform="${onWall(1, Yh(0.78), -0.17)}"><circle cx="0" cy="0" r="2" fill="${iron}"/>` +
       `<path d="M0 0 q-12 18 -2 40 M0 0 q10 20 2 40 M-6 26 h12" fill="none" stroke="#6b3a1a" stroke-width="2"/>` +
       `<circle cx="0" cy="42" r="3" fill="none" stroke="#9aa3ad" stroke-width="1.5"/></g>`;
  t = wallSince(t, mBridle, "bridle");
  const mStraw = t.length;
  // straw all over the floor, the more toward the stalls
  for (let i = 0; i < 150; i++) { const z = -0.62 * Math.pow(R(i, 1310), 1.6), X = (R(i, 1311) - 0.5) * 2 * bw, [sx, sy] = at(X, floor, z), s = k / (1 + z), a = R(i, 1312) * Math.PI;
    t += `<line x1="${f1(sx)}" y1="${f1(sy)}" x2="${f1(sx + Math.cos(a) * 9 * s)}" y2="${f1(sy + Math.sin(a) * 2.5 * s)}" stroke="${R(i, 1313) < 0.6 ? straw : strawD}" stroke-width="${f1(1.2 * s)}" opacity="0.9"/>`; }
  t = floorSince(t, mStraw, "straw");
  // (ticket 99) all that stands in the room, after its walls, floor and what is on them: each an
  // object at its foot, so the horses (drawn after the room) are sorted in among them by depth
  // the ladder up to the loft, standing on the floor by the left wall
  { const [la, lt] = at(L.ladderX, loftY - 14 * k, -0.14), [, lb] = at(L.ladderX, floor, -0.14), lw = 24 * k;
    t += obj("ladder", lb, `<g data-ladder="1">` + [-1, 1].map(e => `<line x1="${f1(la + e * lw / 2)}" y1="${f1(lt)}" x2="${f1(la + e * lw / 2 + 4 * k)}" y2="${f1(lb)}" stroke="${oakL}" stroke-width="${f1(5 * k)}"/>`).join("") +
         [...Array(9)].map((_, j) => { const y = lt + (lb - lt) * (j + 0.5) / 9; return `<line x1="${f1(la - lw / 2)}" y1="${f1(y)}" x2="${f1(la + lw / 2 + 4 * k)}" y2="${f1(y)}" stroke="${oak}" stroke-width="${f1(3.5 * k)}"/>`; }).join("") + `</g>`); }
  // the stone water trough, its water rippling; buckets by it
  { const [tx, ty] = L.trough, s = k / 0.7;
    t += obj("water trough", ty, troughSvg(P, tx, ty, s));
    let bk = "";
    for (const dx of [64, 84]) bk += `<path d="M${P(tx + dx * s - 10 * s, ty - 24 * s)} L${P(tx + dx * s + 10 * s, ty - 24 * s)} L${P(tx + dx * s + 8 * s, ty)} L${P(tx + dx * s - 8 * s, ty)} Z" fill="#9aa3ad" stroke="#5a626c" stroke-width="${f1(1 * s)}"/>` +
      `<ellipse cx="${f1(tx + dx * s)}" cy="${f1(ty - 24 * s)}" rx="${f1(10 * s)}" ry="${f1(2.5 * s)}" fill="#5a626c"/>`;
    t += obj("buckets", ty, bk); }
  // a heap of hay with a pitchfork in it
  { const [hx, hy] = L.hay, s = k / 0.6;
    t += obj("hay heap", hy, `<ellipse cx="${f1(hx)}" cy="${f1(hy)}" rx="${f1(62 * s)}" ry="${f1(8 * s)}" fill="#000000" opacity="0.18"/>` +
         `<path d="M${P(hx - 60 * s, hy)} Q${P(hx - 50 * s, hy - 40 * s)} ${P(hx - 10 * s, hy - 52 * s)} Q${P(hx + 30 * s, hy - 56 * s)} ${P(hx + 50 * s, hy - 26 * s)} Q${P(hx + 62 * s, hy - 10 * s)} ${P(hx + 60 * s, hy)} Z" fill="${straw}" stroke="${strawD}" stroke-width="${f1(1.2 * s)}"/>` +
         [...Array(30)].map((_, j) => { const u = R(j, 1320), x = hx - 50 * s + 100 * s * u, y = hy - (4 + 44 * Math.sin(Math.PI * u) * R(j, 1321)) * s, a = (R(j, 1322) - 0.5) * 2;
           return `<line x1="${f1(x)}" y1="${f1(y)}" x2="${f1(x + 10 * s * Math.cos(a))}" y2="${f1(y - 4 * s * Math.sin(a + 1))}" stroke="${strawD}" stroke-width="${f1(1 * s)}"/>`; }).join("") +
         `<line x1="${f1(hx + 18 * s)}" y1="${f1(hy - 40 * s)}" x2="${f1(hx + 46 * s)}" y2="${f1(hy - 120 * s)}" stroke="${oakL}" stroke-width="${f1(3.5 * s)}" stroke-linecap="round"/>` +
         `<path d="M${P(hx + 10 * s, hy - 30 * s)} L${P(hx + 18 * s, hy - 40 * s)} M${P(hx + 16 * s, hy - 26 * s)} L${P(hx + 20 * s, hy - 38 * s)} M${P(hx + 22 * s, hy - 24 * s)} L${P(hx + 21 * s, hy - 37 * s)} M${P(hx + 12 * s, hy - 37 * s)} L${P(hx + 24 * s, hy - 40 * s)}" stroke="#7d8590" stroke-width="${f1(1.8 * s)}" stroke-linecap="round"/>`); }
  // two hens, pecking at the straw; (ticket 506) as many as the room's 🐔 setting says, 0 to 30: the first two where they
  // always were, as they were drawn, then the rest spread over the floor (stableMoreHens)
  for (const [u, z, sd, i] of STABLE_HENS_FIRST.slice(0, hens)) {
    const [hx, hy] = at(u * bw, floor, z), s = k / (1 + z) * 0.9;
    t += obj(i ? "hen, white" : "hen, brown", hy, henSvg(c.o, hx, hy, s, sd, i));
  }
  t += stableMoreHens(c, L, hens);
  // sunbeams from the round window down onto the floor, dust drifting in them
  // (an object at the depth where they reach the floor: ticket 99)
  { const [fx, fy] = at(0.36 * bw, floor, -0.25), [gx] = at(0.62 * bw, floor, -0.25);
    let sb = `<polygon points="${P(ox - orr * 0.7, oy)} ${P(ox + orr * 0.7, oy)} ${P(gx, fy)} ${P(fx, fy)}" fill="url(#bg-beam)"/>`;
    for (let i = 0; i < 12; i++) { const u = R(i, 1330), w = R(i, 1331), x = ox + (fx + (gx - fx) * w - ox) * u, y = oy + (fy - oy) * u, d = (5 + 5 * R(i, 1332)).toFixed(1);
      sb += `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(1.3 * k)}" fill="#fff8d8">${sceneLfo(c.o, "slow", R(i, 1333), "cy", (v, amp) => f1(y - (4 + 18 * amp) * k * (1 + v) / 2),
        `<animate attributeName="cy" values="${f1(y)};${f1(y - 14 * k)};${f1(y)}" dur="${d}s" begin="-${(R(i, 1333) * 5).toFixed(1)}s" repeatCount="indefinite"/>`)}` +
           `<animate attributeName="opacity" values="0.2;0.9;0.2" dur="${(+d * 0.7).toFixed(1)}s" begin="-${(R(i, 1334) * 4).toFixed(1)}s" repeatCount="indefinite"/></circle>`; }
    t += obj("sunbeam", fy, sb); }
  // swallows, looping round under the beams (at the back, over the floor there: ticket 99)
  let sw = "";
  for (const [i, cx, cy, rx, ry, d] of [[0, -0.3 * bw, top + 30 * k, 120 * k, 26 * k, 7], [1, 0.32 * bw, top + 48 * k, 100 * k, 22 * k, 9]]) {
    sw += `<g><animateMotion path="M${f1(cx - rx)} ${f1(cy)} a${f1(rx)} ${f1(ry)} 0 1 0 ${f1(2 * rx)} 0 a${f1(rx)} ${f1(ry)} 0 1 0 ${f1(-2 * rx)} 0" dur="${d}s" begin="-${i * 3}s" rotate="auto" repeatCount="indefinite"/>` +
         `<g transform="scale(${(k * 1.1).toFixed(3)})"><path d="M-8 0 Q0 -3 8 0 Q0 2 -8 0 Z" fill="#1f2a44"/><path d="M-8 0 L-15 -4 M-8 0 L-15 3" stroke="#1f2a44" stroke-width="1.4"/>` +
         `<path d="M0 0 Q-4 -10 -10 -12 Q-2 -6 2 0 M0 0 Q-4 10 -10 12 Q-2 6 2 0" fill="#1f2a44"><animateTransform attributeName="transform" type="scale" values="1 1;1 0.3;1 1" dur="0.35s" repeatCount="indefinite"/></path>` +
         `<circle cx="7" cy="-0.5" r="1.4" fill="#c2410c"/></g></g>`;
  }
  t += obj("swallows", floor, sw);
  return t;
}
// (ticket 506, for M; Matthew: "chickens -- already in stable 30 in stable.", then "just release and let the kids decide")
// Where the stable's hens past its first two stand, up to n in all. Each on the floor where a seeded draw puts it: of
// HEN_TRIES spots, the first well clear of every hen already there (HEN_CLEAR), else the one farthest from them, so a
// few hens stand about at random and many fill the floor evenly. How far, as a share of the two hens' room on the
// floor: an oval round each one's feet, as wide as the hen and its walk, as deep as a hen is tall (so a hen even half
// that far behind another shows its head and back over it, as in a flock). 30 fill the floor, so the farthest is not always
// clear of every other, but no two stand on one spot. Never in front of the stalls (so far forward its comb is about at
// their doors' foot), by the side walls where their doors are or on the way to them (the friend's ways out, two a
// side), on the ladder, behind or over the trough, its buckets or the hay heap, where the friend arrives (the middle,
// near us: ARRIVE's 0, 40), nor off the picture. Nearer is bigger, as the first two. Each hen's spot comes from the
// hens before it only, so one more hen leaves the others where they were. Their looks by turn (brown, white, black:
// HEN_LOOKS), each facing its own way; its peck's pace and way in from its place in a spread (henSvg's i: as a count,
// past the first few it made a peck too slow), its walk's way in its own.
const HEN_TRIES = 48, HEN_CLEAR = 1.6, HEN_SEED = 5060, HEN_LOOK_NAMES = ["brown", "white", "black"];
// (how far apart two hens' ovals are: 1 just touching, under 1 overlapping)
const henApart = (a, b) => Math.hypot((a.cx - b.cx) / (a.hw + b.hw), (a.y - b.y) / (a.hd + b.hd));
// (ticket 506: the spots kept by the room's frame and the count, all they read: the search at 30 takes ~8 ms, too much to
// do again each time a mixing-board LFO knob or a tune draws the stable again, which changes only the hens' timing (the
// 60 fps rule))
const henSpotsMemo = new Map();
function stableHenSpots(c, L, n) {
  const { k, W, g, y1 } = c, { at, floor, bw, vy } = L, f = y => (y - vy) / (floor - vy);
  const mk = [k, W, g, y1, floor, bw, vy, n].join();
  if (henSpotsMemo.has(mk)) return henSpotsMemo.get(mk);
  // (a hen's room on the floor: its feet at (x, y) at size s, as far as it walks, go)
  const room = (x, y, s, go = 0) => ({ cx: x + go / 2, y, hw: 22 * s + Math.abs(go) / 2, hd: 16 * s });
  // (what stands on the floor, as boxes on the screen: the ladder up to the loft, the trough and its buckets, the hay
  // heap; and the four doors the side walls can hold, as castleWays draws them, two a side, each from z1 - 0.12 to z1, down
  // to its foot; a hen's box, up to its comb, may not meet one)
  const [tx, ty] = L.trough, ts = k / 0.7, [hx, hy] = L.hay, hs = k / 0.6, [lx, ly] = at(L.ladderX, floor, -0.14);
  const blocks = [{ xa: lx - 34 * k, xb: lx + 34 * k, ya: -Infinity, yb: ly + 8 * k }, { xa: tx - 60 * ts, xb: tx + 96 * ts, ya: ty - 42 * ts, yb: ty + 9 * ts },
                  { xa: hx - 64 * hs, xb: hx + 64 * hs, ya: hy - 60 * hs, yb: hy + 9 * hs },
                  ...[-1, 1].flatMap(sd => [0, 1].map(j => { const z1 = -0.03 - 0.17 * j, [xa] = at(sd * bw, floor, z1), [xb, yb] = at(sd * bw, floor, z1 - 0.12);
                    return { xa: Math.min(xa, xb) - 6 * k, xb: Math.max(xa, xb) + 6 * k, ya: -Infinity, yb: yb + 6 * k }; }))];
  // (a spot on the floor no hen stands on: in front of the stalls, by a side door, where the friend arrives)
  const kept = (x, y) => { const F = f(y), X = x / F, z = 1 / F - 1;
    return z > -0.12 || (Math.abs(X) > 0.72 * bw && z > -0.4) || (Math.abs(x) < 110 * k && y > g - 20 * k); };
  const yTop = at(0, floor, -0.12)[1], yBot = y1 - 6 * k;
  const hens = STABLE_HENS_FIRST.map(([u, z]) => { const [x, y] = at(u * bw, floor, z); return room(x, y, k / (1 + z) * 0.9); }), out = [];
  for (let j = 2; j < n; j++) {
    const r = seeded(HEN_SEED + j);
    let best = null;
    for (let t = 0, found = 0; t < 600 && found < HEN_TRIES; t++) {
      const x = W * (r(4 * t) - 0.5), y = yTop + (yBot - yTop) * r(4 * t + 1), s = k * f(y) * 0.9, sd = r(4 * t + 2) < 0.5 ? -1 : 1, go = sd * (8 + 10 * r(4 * t + 3)) * s;
      const A = { xa: Math.min(x, x + go) - 22 * s, xb: Math.max(x, x + go) + 22 * s, ya: y - 33 * s, yb: y + 3 * s };
      if (kept(x, y) || kept(x + go, y) || A.xa < -W / 2 + 6 * k || A.xb > W / 2 - 6 * k || blocks.some(b => A.xa < b.xb && b.xa < A.xb && A.ya < b.yb && b.ya < A.yb)) continue;
      found++;
      const me = room(x, y, s, go), sep = Math.min(HEN_CLEAR, ...hens.map(h => henApart(me, h)));
      if (!best || sep > best.sep) best = { x, y, s, sd, go, me, sep };
      if (sep >= HEN_CLEAR) break;
    }
    if (!best) break;
    hens.push(best.me);
    out.push({ ...best, j, look: j % 3, i: ((j * 0.618034) % 1) * 2.2, off: r(4001), dur: 8 + 5 * r(4002) });
  }
  out.sort((a, b) => a.y - b.y);
  if (henSpotsMemo.size > 40) henSpotsMemo.clear();
  henSpotsMemo.set(mk, out);
  return out;
}
// The hens past the first two, each an object at its feet, back to front. Each pecks with the quick LFO (henSvg) and,
// now and then, with the slow one, walks a few steps the way it faces, turns round, pecks there, walks back and turns
// again, as the lake's ducks swim (the walk outside its spot, the turn about its feet): three animations a hen, so 30
// stay light (the 60 fps rule); held still, each stands at its spot
function stableMoreHens(c, L, n) {
  const { o } = c;
  return stableHenSpots(c, L, n).map(h => { const d = h.dur.toFixed(1), b = (h.off * h.dur).toFixed(2);
    return obj(`hen ${h.j + 1}, ${HEN_LOOK_NAMES[h.look]}`, h.y, `<g data-hen="${h.j + 1}">` +
      lfoTimed(o, "slow", h.off, `<animateTransform attributeName="transform" type="translate" values="0 0;0 0;${f1(h.go)} 0;${f1(h.go)} 0;0 0;0 0" keyTimes="0;0.3;0.44;0.8;0.94;1" ` +
        `calcMode="spline" keySplines="0 0 1 1;0.4 0 0.6 1;0 0 1 1;0.4 0 0.6 1;0 0 1 1" dur="${d}s" begin="-${b}s" repeatCount="indefinite"/>`) +
      `<g transform="translate(${f1(h.x)} ${f1(h.y)})"><g>` +
      lfoTimed(o, "slow", h.off, `<animateTransform attributeName="transform" type="scale" values="1 1;-1 1;1 1" keyTimes="0;0.44;0.94" calcMode="discrete" dur="${d}s" begin="-${b}s" repeatCount="indefinite"/>`) +
      henSvg(o, 0, 0, h.s, h.sd, h.i, h.look) + `</g></g></g>`); }).join("");
}
// The stable's horses, each looking out of its stall, the outer stalls
// first; its door's top rail drawn again over the foot of its neck. (ticket
// 99) Each horse, with that rail, an object at the foot of its stall's half
// door: all of it stands behind the door, but drawn after the room it must be
// sorted in among the room's objects, or it would be drawn over the friend
function stableHorses(c, K, wags) {
  const L = stablePlaces(c), { k } = c;
  return K.horses.map((Hh, i) => {
    const X = L.bays[L.order[i]], [hx, hy] = L.at(X, L.doorTop, -0.01), s = 0.9 * k;
    return obj(`horse ${i + 1}`, L.at(X, L.floor, -0.01)[1], `<g transform="translate(${f1(hx)} ${f1(hy)}) scale(${s.toFixed(3)})">${horseHead(Hh, wags[i], i, i % 2 ? K.flagB : K.flagA)}</g>` + L.rail(X));
  }).join("");
}
// (sk: its sign's size, when it is not the door's own: a hallway's farther doors keep signs big enough to read)
// (ticket 256, Matthew: "In the castle, it can just be a solid color indicating if the next room is bright or dim, or in
// between") how light the room through a door is, as one color, seen as the door opens: from the room's own tone, its
// walls' stone (a dungeon's dark, the greenhouse's pale), dark to lamplight; out of the castle (null), daylight
const ROOM_LIGHT = { catroom: 0.62, stable: 0.5 };   // (the rooms of no tone: how light, 0 to 1)
function roomLightBehind(kind) {
  if (kind === null) return "#cfe6f5";
  const tone = ROOM_KINDS[kind] && ROOM_KINDS[kind].tone, v = tone ? parseInt(tone.slice(1), 16) : 0;
  const lum = tone ? (0.299 * (v >> 16) + 0.587 * ((v >> 8) & 255) + 0.114 * (v & 255)) / 255 : ROOM_LIGHT[kind] ?? 0.5;
  return mix("#140d08", "#ffe3a0", clamp((lum - 0.32) / 0.48, 0, 1));
}
function hallSideDoor(c, sd, z0, z1, label, attrs, lift = 0, sk = 0, behind = "#140d08") {
  const { k } = c, S = roomSpace(c), { floor, bw } = S, h = 170 * k, rise = 46 * k;
  const at = (z, y) => S.at(sd * bw, y, z);   // (on its side wall)
  const zm = (z0 + z1) / 2, fm = 1 / (1 + zm), km = k * fm;
  sk = sk || km;
  const nb = at(z0, floor), nt = at(z0, floor - h), ft = at(z1, floor - h), fb = at(z1, floor), ap = at(zm, floor - h - rise);
  const ctl = [2 * ap[0] - (nt[0] + ft[0]) / 2, 2 * ap[1] - (nt[1] + ft[1]) / 2];
  const shape = `M${P2(...nb)} L${P2(...nt)} Q${P2(...ctl)} ${P2(...ft)} L${P2(...fb)} Z`;
  let planks = "";
  for (let j = 1; j < 4; j++) {
    const u = j / 4, z = z0 + (z1 - z0) * u, a = at(z, floor), b = at(z, floor - h - rise * Math.sin(Math.PI * u) * 0.9);
    planks += `<line x1="${f1(a[0])}" y1="${f1(a[1])}" x2="${f1(b[0])}" y2="${f1(b[1])}" stroke="#3a2a1a" stroke-width="${f1(Math.max(0.6, 1.6 * km))}"/>`;
  }
  const ring = at(z0 + (z1 - z0) * 0.78, floor - h * 0.48), band = (y) => { const a = at(z0, y), b = at(z1, y);
    return `<line x1="${f1(a[0])}" y1="${f1(a[1])}" x2="${f1(b[0])}" y2="${f1(b[1])}" stroke="#2a2a32" stroke-width="${f1(Math.max(0.8, 3 * km))}"/>`; };
  // (ticket 147: the door swung open, hinged on its far side: seen nearly edge on, a sliver there;
  // the same commands as its shape, so the browser can move one to the other)
  const zo = z1 - 0.12 * (z1 - z0), ob = at(zo, floor), ot = at(zo, floor - h - rise * Math.sin(Math.PI * 0.88) * 0.9);
  const open = `M${P2(...ob)} L${P2(...ot)} Q${P2((ot[0] + ft[0]) / 2, (ot[1] + ft[1]) / 2)} ${P2(...ft)} L${P2(...fb)} Z`;
  return `<g class="portal" data-door="" ${attrs}>` +
    `<path d="${shape}" fill="none" stroke="${mix(ROOM_TONE.hallway, "#ffffff", 0.12)}" stroke-width="${f1(10 * km)}" stroke-linejoin="round"/>` +
    `<path d="${shape}" fill="${behind}"/><path d="M${P2(...nb)} L${P2(...fb)}" stroke="#ffcf6b" stroke-opacity="0.35" stroke-width="${f1(4 * km)}"/>` +
    `<g data-leaf="" data-open="${open}">` +
    `<path d="${shape}" fill="#5a3a22" stroke="#2e1f12" stroke-width="${f1(Math.max(0.8, 3 * km))}" stroke-linejoin="round"/>` + planks +
    band(floor - h * 0.22) + band(floor - h * 0.78) +
    `<ellipse cx="${f1(ring[0])}" cy="${f1(ring[1])}" rx="${f1(3.2 * km)}" ry="${f1(6 * km)}" fill="none" stroke="#2a2a32" stroke-width="${f1(Math.max(0.8, 2.2 * km))}"/>` +
    `<path d="${shape}" fill="#0b1030" opacity="0.45"/></g>` +
    plankSign(Math.max(c.x0 + 56 * sk, Math.min(c.x1 - 56 * sk, ap[0])), ap[1] - 22 * sk - lift, sk, label) + `</g>`;
}
// A hallway's doors, and the dining room's: by turns left and right, a pair to each span of the side
// walls, packed toward us (ticket 116: spread down the whole corridor, the farther ones were too far
// away to see), each sign at least HALL_SIGN_MIN of the size it would have at the corridor's mouth;
// the torches along the walls stand in the gaps between the pairs (HALL_LIGHT_Z)
const HALL_DOOR_Z = [[0, 0.24], [0.36, 0.62], [0.82, 1.12], [1.36, 1.76]], HALL_LIGHT_Z = [0.72, 1.24, 2.18], HALL_SIGN_MIN = 0.8;
// where a side door's sign would sit (its middle), and its size: above the door's arch, kept in the picture
function hallSignAt(c, sd, z0, z1, sk) {
  const S = roomSpace(c), zm = (z0 + z1) / 2, [ax, ay] = S.at(sd * S.bw, S.floor - 216 * c.k, zm);
  return { x: Math.max(c.x0 + 56 * sk, Math.min(c.x1 - 56 * sk, ax)), y: ay - 22 * sk, w: 104 * sk, h: 36 * sk };
}
// (all of a room's doors at once: a farther door's sign, where it would sit on a nearer one's on the same
// side, goes up above it, so every sign can be read)
function hallDoors(c, doors) {
  const tops = { "-1": [], "1": [] };
  return doors.map((L, i) => {
    const sd = i % 2 ? 1 : -1, [z0, z1] = HALL_DOOR_Z[Math.floor(i / 2)], sk = c.k * Math.max(HALL_SIGN_MIN, 2 / (2 + z0 + z1));
    const A = hallSignAt(c, sd, z0, z1, sk); let y = A.y;
    for (const B of tops[sd]) if (Math.abs(A.x - B.x) < (A.w + B.w) / 2 + 4 * c.k && y + A.h / 2 > B.y - B.h / 2 - 4 * c.k) y = B.y - B.h / 2 - 4 * c.k - A.h / 2;
    tops[sd].push({ ...A, y });
    return hallSideDoor(c, sd, z0, z1, L.label, L.attrs, A.y - y, sk, L.behind);
  }).join("");
}
// Where a castle room's ways lead, for the picture (castleWays) and the
// castle designer alike: 0 is out of the castle, a number a room, null
// nowhere (or a room the castle doesn't have). Each exit has its kind, where
// it leads, and the setting that says so (key: none when it is fixed).
function wayOutOf(K, n) {
  const st = K.rooms[n - 1], S = ROOM_S_OUT.includes(ROOM_KINDS[st].exits);
  // (ticket 233: nowhere only where the way out is toward us; a room changed to an arch's or a ladder's, the usual way)
  const v = K.hall[n - 1].out === "none" && !S ? "auto" : K.hall[n - 1].out;
  if (v === "auto") { const trapAt = K.rooms.indexOf("trap") + 1;
    return ROOM_KINDS[st].exits === "ladder" && trapAt && trapAt !== n ? trapAt : n === 1 ? 0 : 1; }
  if (v === "out") return 0;
  if (v === "none") return null;   // (ticket 233: a way toward us may lead nowhere)
  const r = Number(v); return r >= 1 && r <= K.rooms.length ? r : null;
}
function roomExits(K, n) {
  const st = K.rooms[n - 1], ex = ROOM_KINDS[st].exits, H = K.hall[n - 1], id = K.id, X = [];
  const dest = d => d === "out" ? 0 : Number(d) >= 1 && Number(d) <= K.rooms.length ? Number(d) : null;
  const signed = ex === "side" || ex === "long" || ex === "spiral";
  if (ex !== "hall") X.push({ kind: { side: "sign", long: "sign", spiral: "sign", trap: "arch", ladder: "ladder", all: "arch" }[ex], to: wayOutOf(K, n), key: "castleOut" + n + id, way: true, nowhere: H.out === "none" && signed });
  // (the door behind us, at the bottom of the picture: optional, in a room with no sign there already,
  // and never in a dungeon, a pit with no way in but from above)
  if (["hall", "trap", "all"].includes(ex)) { const v = H.back; X.push({ kind: "back", to: v === "none" ? null : dest(v), key: "castleBack" + n + id, nowhere: v === "none" }); }
  // (the dining room's door in its far wall, by the fireplace: optional)
  if (ex === "long") { const v = H.far; X.push({ kind: "far", to: v === "none" ? null : dest(v), key: "castleFar" + n + id, nowhere: v === "none" }); }
  if (ex === "hall" || signed)
    H.doors.forEach((d, j) => {
      // (in a room with a sign toward us, a door that leads where the sign does is that sign)
      const to = dest(d); if (signed && to === X[0].to) return;
      X.push({ kind: "door", to, key: hallDoorKey(id, n, j + 1), j: j + 1, missing: to === null });
    });
  if (ex === "hall") { const e = H.end, to = e === "none" ? null : e === "auto" ? K.rooms.indexOf("spiral") + 1 || null : dest(e);
    X.push({ kind: "stairs", to, key: "castleHallEnd" + n + id, nowhere: to === null && (e === "none" || e === "auto") }); }
  // (a trap room's two pits drop into the castle's dungeons, the first and then the deep one: fixed)
  if (ex === "trap") { const d1 = K.rooms.indexOf("dungeon1") + 1, d2 = K.rooms.indexOf("dungeon2") + 1;
    [d1 || d2, d2 || d1].forEach((to, i) => X.push({ kind: "pit", to: to || null, key: null, i, nowhere: !to })); }
  // (the greenhouse: an arch to every other room but the dungeons, which are fallen into: fixed)
  if (ex === "all") K.rooms.forEach((s2, i) => { if (i + 1 !== n && ROOM_KINDS[s2].exits !== "ladder" && i + 1 !== X[0].to) X.push({ kind: "arch", to: i + 1, key: null }); });
  return X;
}
// the dining room's door in its far wall, to the left of the great fireplace (as far away as the
// fireplace, so small; its sign as big as the side doors' farthest, to be read)
function farWallDoor(c, label, attrs) {
  const { k } = c, { floor, top, bw, vy } = roomFrame(c), f = 1 / (1 + HALL_STRETCHES * 0.8), Y = y => vy + (y - vy) * f;
  const w = bw * f, tp = Y(top), fl = Y(floor), kf = k * f, cx = -w * 0.73, hw = w * 0.17, dt = fl - (fl - tp) * 0.58;
  return `<g class="portal" data-door="" ${attrs}><path d="M${f1(cx - hw)} ${f1(fl)} V${f1(dt + hw)} A${f1(hw)} ${f1(hw)} 0 0 1 ${f1(cx + hw)} ${f1(dt + hw)} V${f1(fl)} Z" fill="#1d1712" stroke="#4a3a2a" stroke-width="${f1(8 * kf)}"/>` +
    `<path d="M${f1(cx - hw * 0.7)} ${f1(fl)} V${f1(dt + hw * 1.1)} A${f1(hw * 0.7)} ${f1(hw * 0.7)} 0 0 1 ${f1(cx + hw * 0.7)} ${f1(dt + hw * 1.1)} V${f1(fl)}" fill="#2c2219"/>` +
    `<ellipse cx="${f1(cx)}" cy="${f1(fl - 2 * kf)}" rx="${f1(hw * 0.6)}" ry="${f1(3 * kf)}" fill="#ffcf6b" opacity="0.25"/>` +
    plankSign(cx, dt - 22 * k * HALL_SIGN_MIN, k * HALL_SIGN_MIN, label) + `</g>`;
}
// (ticket 252) the shape of the trap room's pit that falls into room n (the first pit's, if both do), else a circle
// (ticket 271: the trap room's own shapes, the first trap room's if there are more: every trap room's pits fall there)
function pitShapeOver(K, n) {
  const d1 = K.rooms.indexOf("dungeon1") + 1, d2 = K.rooms.indexOf("dungeon2") + 1, i = [d1 || d2, d2 || d1].indexOf(n), t = K.rooms.indexOf("trap") + 1;
  return (t ? K.pitShapesAt[t - 1] : K.pitShapes)[i < 0 ? 0 : i] || "circle";
}
function castleWays(c, K, n, style) {
  const { W, k } = c, { floor, bw } = roomFrame(c), S = roomSpace(c);
  const rooms = K.rooms, exits = ROOM_KINDS[style].exits, X = roomExits(K, n);
  const way = to => to === 0 ? { attrs: `data-to="castle"`, label: "🏰 out", behind: roomLightBehind(null) }
    : to ? { attrs: `data-to="castleroom" data-room="${to}"`, label: `${roomLabel(rooms[to - 1]).split(" ")[0]} ${to}`, behind: roomLightBehind(rooms[to - 1]) } : null;
  // (its way out, where it has one: castleOut; leading nowhere, it isn't there)
  const out = X[0] && X[0].way ? way(X[0].to) : null;
  // (ticket 99) All of this is drawn after the room, so all of it is objects: a door or an arch in a wall,
  // the far end's arch, the dungeon's ladder, each at the depth where it meets the floor (a side door: its
  // near edge), so the room's objects are sorted in front of it as they stand, and it is never drawn over a
  // friend nearer than it. (ticket 233) Its ways toward us are not drawn: nav arrows (castleSouthWays).
  if (exits === "trap") return out ? obj("way out", floor, archDoor(0, floor, k, out.label, out.attrs, 100, 180)) : "";
  if (exits === "ladder")
    // (ticket 252: the dungeon's ceiling hole, the shape of the pit above it that falls into it)
    return out ? obj("ladder", floor + 10 * k, ladderUp(bw * 0.82, floor, roomFrame(c).top - 30 * k, k, X[0].to === 0 ? "🪜 up and out" : `🪜 up to ${out.label}`, out.attrs, pitShapeOver(K, n))) : "";
  const links = X.filter(x => x.kind === "arch").map(x => way(x.to)).filter(Boolean);
  const doors = X.filter(x => x.kind === "door").map(x => way(x.to)).filter(Boolean);
  // (where each link leads, and each door's number, as the castle designer names it: their objects' names)
  const linkTo = X.filter(x => x.kind === "arch" && way(x.to)).map(x => x.to), doorNo = X.filter(x => x.kind === "door" && way(x.to)).map(x => x.j);
  // the hallway's: doors in its side walls, by turns left and right, two to each stretch of it
  // the king's room: its doors in its side walls, two to a side, between its back corners and the
  // picture's edges (clear of all it holds); its way out toward us, a nav arrow (ticket 233; a door set to
  // lead where that way does is that way)
  if (exits === "side" || exits === "long" || exits === "spiral") {
    // (the dining room's door in its far wall, where it leads somewhere)
    const FX = X.find(x => x.kind === "far"), FL = FX && FX.to !== null && way(FX.to), far = FL ? obj("far door", S.at(0, floor, HALL_STRETCHES * 0.8)[1], farWallDoor(c, FL.label, FL.attrs)) : "";
    // (a nearer door's sign, kept inside the picture, would sit on the farther one's: it goes above it;
    // a long room's doors run down its walls, as a hallway's do)
    // (a spiral staircase's doors come out onto its steps, each higher than the last)
    // (a long room's doors one object, all as far as its nearest one's foot, at the room's mouth)
    return (exits === "spiral" ? doors.map((L, i) => { const d = spiralDoorAt(c, i); return obj(`door ${doorNo[i]}`, d.y, archDoor(d.x, d.y, d.k, L.label, L.attrs, 70, 130)); }).join("")
      : exits === "long" ? (doors.length ? obj("doors", floor, hallDoors(c, doors)) : "")
      // (ticket 223, Matthew: "a layering issue", the throne room's side door drawn over the friend standing beside it,
      // their head and arm sticking out past its edge) a door flat on its side wall is behind anyone in the room: drawn
      // with the back wall (its foot, floor), as the long rooms' doors are, not at its near edge's depth
      : doors.map((L, i) => { const j = Math.floor(i / 2), z1 = -0.03 - 0.17 * j;
          return obj(`door ${doorNo[i]}`, floor, hallSideDoor(c, i % 2 ? 1 : -1, z1 - 0.12, z1, L.label, L.attrs, j * 46 * k, 0, L.behind)); }).join("")) + far;
  }
  if (exits === "hall") {
    // and its far end, the foot of the spiral stairs, to the room its setting says (none: only to look at)
    const end = X.find(x => x.kind === "stairs").to || 0;
    // (to this very hallway too: the friend flies up the stairs and lands back here, as through a picture of its own room)
    // (its doors one object, all as far as its nearest one's foot, at the hallway's mouth; the far end at its own floor)
    const A = hallFarArch(c), far = end ? obj("stairs", A.fl, `<g class="portal" data-door="" data-to="castleroom" data-room="${end}"><path d="${A.d}" fill="#000000" fill-opacity="0.01"/>` +
      plankSign(0, A.top - 22 * k * HALL_SIGN_MIN, k * HALL_SIGN_MIN, `${roomLabel(rooms[end - 1]).split(" ")[0]} ${end}`) + `</g>`) : "";
    return (doors.length ? obj("doors", floor, hallDoors(c, doors)) : "") + far;
  }
  // (the greenhouse: archways along its back wall, three either side of its middle; any more, as
  // with a castle of many rooms, doors in its stone side walls, in perspective, as the king's room's,
  // two to a side; and any more still, toward us, nav arrows (castleSouthWays, ticket 233))
  let t = "";
  const back = links.slice(0, 6), more = links.slice(6, 10);
  back.forEach((L, i) => {
    const sd = i % 2 ? 1 : -1, j = Math.floor(i / 2);
    // (ticket 352: 164 tall, 140 till 0.0.302, which the default friend crawled through: 1.04 of it; now 0.89, as the other rooms' doors)
    t += obj(`arch to room ${linkTo[i]}`, floor, archDoor(sd * (bw * 0.86 - j * 104 * k), floor, k, L.label, L.attrs, 76, 164));
  });
  more.forEach((L, i) => { const j = Math.floor(i / 2), z1 = -0.03 - 0.15 * j;
    // (ticket 223: on its wall, behind anyone in the room)
    t += obj(`door to room ${linkTo[6 + i]}`, floor, hallSideDoor(c, i % 2 ? 1 : -1, z1 - 0.11, z1, L.label, L.attrs, j * 46 * k, 0, L.behind)); });
  return t;
}
// (ticket 233, Matthew: "each S exit of each room that has one can be: nothing, another castle room, outside of the
// castle"; "rather than render a sign to indicate it and offer a click, we use the nav arrow UX now") A castle room's
// ways toward us, S: its way behind us (a hallway's, the trap room's, the greenhouse's: castleBack), or its way out
// where that is toward us (ROOM_S_OUT: castleOut); and a greenhouse's arches past the ten its walls hold, SW and SE
// (then S, if free; a 13-room castle of no dungeons can have a third). Each where it leads somewhere: a nav arrow over
// the picture, shown when the friend is near (insideWays, 25-world.js); leading nowhere, nothing at all.
function castleSouthWays(K, n) {
  const ex = ROOM_KINDS[K.rooms[n - 1]].exits, X = roomExits(K, n);
  const way = to => to === 0 ? { to: "castle", attrs: `data-to="castle"`, label: "🏰 out" }
    : { to: "castleroom", attrs: `data-to="castleroom" data-room="${to}"`, label: `${roomLabel(K.rooms[to - 1]).split(" ")[0]} ${to}` };
  const S = X.find(x => x.kind === "back" || (x.way && ROOM_S_OUT.includes(ex))), out = S && S.to !== null ? [{ d: "S", ...way(S.to) }] : [];
  if (ex === "all") {
    const free = ["SW", "SE", "S"].filter(d => !out.some(w => w.d === d));
    X.filter(x => x.kind === "arch" && x.to !== null).slice(10).forEach((x, i) => { if (free[i]) out.push({ d: free[i], ...way(x.to) }); });
  }
  return out;
}
// The castle as a map, for the castle designer: every way between its rooms
// (the gate into room 1, then each room's exits: roomExits), what looks wrong
// with it, and how to put each thing right. A way leading out of the castle
// leads to 0; the gate leads from 0 into room 1.
// (ticket 233: a room's way toward us, S, is a nav arrow now, no sign: the king's room's way out, as any room's way behind us)
const EXIT_ICONS = { door: "🚪", sign: "⬇️", arch: "🏛️", ladder: "🪜", stairs: "🌀", pit: "🕳️", gate: "🏰", back: "⬇️", far: "🚪" };
const EXIT_WORDS = { door: "door", sign: "way toward us", arch: "archway", ladder: "ladder", stairs: "stairs", pit: "pit", gate: "gate", back: "way toward us", far: "door in the far wall" };
function castleGraph(K) {
  const E = [{ from: 0, kind: "gate", to: 1, key: null }];
  K.rooms.forEach((_, i) => roomExits(K, i + 1).forEach(x => E.push({ from: i + 1, ...x })));
  return E;
}
const mapRoomName = (K, r) => r === 0 ? "🏰 outside" : `${roomLabel(K.rooms[r - 1]).split(" ")[0]} room ${r}`;
const mapExitName = (K, e) => e.kind === "gate" ? "🏰 castle's gate" :
  `${EXIT_ICONS[e.kind]} ${e.kind === "door" ? "door " + e.j : e.way ? "way out (its " + EXIT_WORDS[e.kind] + ")" : EXIT_WORDS[e.kind] + (e.kind === "pit" ? " " + (e.i + 1) : "")} of ${mapRoomName(K, e.from)}`;
// (the rooms that can be reached from outside, or, backward, that can get back out)
function mapReach(E, forward) {
  const seen = new Set([0]), q = [0];
  while (q.length) { const a = q.shift();
    for (const e of E) { const u = forward ? e.from : e.to, v = forward ? e.to : e.from;
      if (u === a && v !== null && v !== undefined && !seen.has(v)) { seen.add(v); q.push(v); } } }
  return seen;
}
function castleProblems(K) {
  const E = castleGraph(K), inn = mapReach(E, true), out = mapReach(E, false), P = [];
  K.rooms.forEach((_, i) => { const r = i + 1;
    if (!inn.has(r)) P.push({ kind: "unreached", room: r, text: `${mapRoomName(K, r)} can't be reached from outside.` });
    if (!out.has(r)) P.push({ kind: "stuck", room: r, text: `From ${mapRoomName(K, r)}, there's no way back out of the castle.` }); });
  for (const e of E) {
    if (e.to === null && !e.nowhere) P.push({ kind: "missing", e, text: `The ${mapExitName(K, e)} leads to a room the castle doesn't have.` });
    // (a pit is one way, by design: you fall in, and climb the ladder back up)
    else if (e.from && e.to && e.to !== e.from && e.kind !== "pit" && !E.some(f => f.from === e.to && f.to === e.from))
      P.push({ kind: "oneway", e, text: `The ${mapExitName(K, e)} goes to ${mapRoomName(K, e.to)}, but there's no way back.` });
  }
  return P;
}
// (rooms with doors in their walls, as many as four: the hallway, the king's room and its kind)
const mapDoorRoom = (K, r) => ["hall", "side", "long", "spiral"].includes(ROOM_KINDS[K.rooms[r - 1]].exits);
// a door from room a to b (0: out), in a door leading nowhere or a new one; false when a has no space
function mapAddDoor(B, K, a, b) {
  const id = K.id, val = b === 0 ? "out" : String(b), X = roomExits(K, a);
  const bad = X.find(x => x.kind === "door" && x.missing);
  if (bad) { B[bad.key] = val; return true; }
  // (the door behind us, where it's free: a hallway's, the trap room's, the greenhouse's; or the dining room's far door)
  const back = X.find(x => (x.kind === "back" || x.kind === "far") && x.nowhere);
  if (back) { B[back.key] = val; return true; }
  if (!mapDoorRoom(K, a)) return false;
  const n = K.hall[a - 1].doors.length;
  if (n >= roomDoorMax(K.rooms[a - 1])) return false;
  B["castleHallDoors" + a + id] = n + 1; B[hallDoorKey(id, a, n + 1)] = val; return true;
}
// doors two-way: when room a's way leads to room b, b gets a way back to a (a door), where it has space
function mapWayBack(B, id, a, b) {
  const K = castleOf({ ...B, castle: id });
  if (!b || b === a || roomExits(K, b).some(x => x.to === a)) return true;
  return mapAddDoor(B, K, b, a);
}
// (a door taken out of room a's walls, the ones after it moving up)
function mapCloseDoor(B, K, a, j) {
  const id = K.id, D = K.hall[a - 1].doors;
  for (let k = j; k < D.length; k++) B[hallDoorKey(id, a, k)] = String(D[k]);
  B["castleHallDoors" + a + id] = D.length - 1;
}
// how to put one thing right: what the fix says it does, and doing it (to B, the settings); null when it can't be
function mapPlan(B, id, p) {
  const K = castleOf({ ...B, castle: id }), E = castleGraph(K), inn = mapReach(E, true), name = r => mapRoomName(K, r);
  if (p.kind === "missing") { const v = p.e.kind === "door" ? (p.e.from === 1 ? "out" : "1") : ["stairs", "back", "far"].includes(p.e.kind) ? "none" : "auto";
    return { text: v === "none" ? "🔧 lead it nowhere" : v === "auto" ? "🔧 the usual way" : v === "out" ? "🔧 lead it out" : "🔧 lead it to room 1", apply: b => { b[p.e.key] = v; } }; }
  if (p.kind === "oneway") { const a = p.e.from, t = p.e.to;
    // (a door back, where the room it leads to has space; or lead it to a room that has a way back; or close it)
    if (mapAddDoor({ ...B }, K, t, a)) return { text: "🔧 add a door back", apply: b => mapAddDoor(b, castleOf({ ...b, castle: id }), t, a) };
    if (p.e.key) {
      const c = K.rooms.map((_, i) => i + 1).find(c => c !== a && c !== t && inn.has(c) && E.some(f => f.from === c && f.to === a));
      if (c) return { text: `🔧 lead it to ${name(c)} instead`, apply: b => { b[p.e.key] = String(c); } };
      if (p.e.kind === "door") return { text: "🔧 close this door", apply: b => mapCloseDoor(b, K, a, p.e.j) };
    }
    return null;
  }
  if (p.kind === "unreached") {
    // (a door to it from a room that can be reached, room 1 first, and a way back)
    const a = [1, ...ROOM_NS].find(a => a <= K.rooms.length && a !== p.room && inn.has(a) && mapAddDoor({ ...B }, K, a, p.room));
    return a ? { text: `🔧 a door to it from ${name(a)}`, apply: b => { mapAddDoor(b, castleOf({ ...b, castle: id }), a, p.room); mapWayBack(b, id, a, p.room); } } : null;
  }
  if (p.kind === "stuck") {
    const w = roomExits(K, p.room).find(x => x.way), to = p.room === 1 ? 0 : 1;
    if (w && B[w.key] !== "auto") return { text: "🔧 its way out, the usual way", apply: b => { b[w.key] = "auto"; } };
    return mapAddDoor({ ...B }, K, p.room, to) ? { text: `🔧 a door to ${name(to)}`, apply: b => mapAddDoor(b, castleOf({ ...b, castle: id }), p.room, to) } : null;
  }
  return null;
}
const mapFix = (B, id, p) => { const plan = mapPlan(B, id, p); if (plan) plan.apply(B); return !!plan; };
const mapCanFix = (B, id, p) => !!mapPlan(B, id, p);
// (ticket 50) A castle room's seats, by its style, each from the room's own
// drawing and its helpers:
// - the dining room's chairs (diningPlaces, seen from the room's height, eye
//   0.3, as castleroomScene draws it): each chair's cushion, 3 above its
//   seat, across it at its middle; a side's chairs face the table (the left
//   side's right, the right side's left), the far end's faces us (our end's
//   faces away from us, so it is no seat: there is no back view);
// - the king's room's throne (kingRoomSvg): the top of its red cushion, 40
//   above ty = floor - 42, across between its arms (at ±56, 14 wide);
// - the cat room's window seat (catRoomPlaces, and catRoomSvg's sa, sb and
//   Ys): its cushion's top halfway back (9 above Ys at the wall, 4 at the
//   front, z -0.005 to -0.085), from end to end;
// - the spiral staircase's steps (spiralGeom; each a tenth of a half turn,
//   as spiralSvg draws them): steps 2 to 5, counted up from the one on the
//   floor (0), the middle half of each from the column out to the wall, at
//   its middle; a sitter is in front of all the low steps (the steps
//   object), their legs hanging down over the lower steps; down the stairs
//   is toward us, so facing us first;
// - the stable's hay heap (stableSvg): the top of the heap, left of the
//   pitchfork in it (the heap's peak, between its two curves, at 52 up, in
//   its own units s), sunk 2 into the hay.
const ROOM_SEATS = {
  dining: (c, K, n) => {
    const { k } = c, D = diningPlaces({ ...c, eye: 0.3 }, K.places[n - 1]), seat = (name, z, xa, xb, facing) => { const f = D.fz(z);
      return { name, x0: Math.min(xa, xb) * f, x1: Math.max(xa, xb) * f, y: D.Y(D.ys - 3 * k, f), z: D.floorAt(z) + 1, facing }; };
    return [seat("chair, far end", D.zFar, -0.8 * D.endX, 0.8 * D.endX, ["front"]),
            ...[-1, 1].flatMap(sd => [...Array(D.sides[sd < 0 ? 0 : 1])].map((_, j) => { const [xi, xo] = D.seatX(sd);
              return seat(`chair, ${sd < 0 ? "left" : "right"} ${j + 1}`, D.zOf(sd, j), xi, xo - sd * 4 * k, [sd < 0 ? "right" : "left"]); }))];
  },
  // (ticket 293: each throne, where it stands and at its size)
  king: (c, K) => throneLayout(c, K).map((t, i) => ({ name: `throne ${i + 1}`, x0: t.x - 42 * c.k * t.s, x1: t.x + 42 * c.k * t.s, y: t.floor - 82 * c.k * t.s, z: t.floor + 1, facing: ["front"] })),
  catroom: c => { const { k } = c, { at, Yh, WX, bw, floor } = catRoomPlaces(c), sa = WX - 0.27 * bw, sb = WX + 0.27 * bw, Ys = Yh(0.22), z = -0.045;
    return [{ name: "window seat", x0: at(sa + 0.01 * bw, Ys, z)[0], x1: at(sb - 0.01 * bw, Ys, z)[0], y: at(0, Ys - 6.5 * k, z)[1], z: at(0, floor, -0.09)[1] + 1, facing: ["front"] }]; },
  spiral: c => { const { Yf, R, r0, th0, lvl, pt } = spiralGeom(c), step = Math.PI / 10, ra = r0 + (R - r0) * 0.25, rb = r0 + (R - r0) * 0.75;
    return [2, 3, 4, 5].map(j => { const a = th0 + j * step, th = a + step / 2, xa = pt(ra, th, 0)[0], xb = pt(rb, th, 0)[0];
      return { name: `step ${j}`, x0: Math.min(xa, xb), x1: Math.max(xa, xb), y: pt((ra + rb) / 2, th, lvl(a))[1], z: pt(R, (th0 + Math.PI) / 2, Yf)[1] + 1,
               facing: th < Math.PI ? ["front", "right", "left"] : ["front", "left", "right"] }; }); },
  stable: c => { const { k } = c, { at, floor, bw } = roomSpace(c), [hx, hy] = at(0.6 * bw, floor, -0.4), s = k / 0.6;
    return [{ name: "hay heap", x0: hx - 24 * s, x1: hx + 6 * s, y: hy - 50 * s, z: hy + 1, facing: ["front", "left", "right"] }]; },
};
SEATS.castleroom = (c, B) => { const K = castleOf(B), n = Math.max(1, Math.min(K.rooms.length, Math.round(B.castleRoom ?? 1)));
  return ROOM_SEATS[K.rooms[n - 1]]?.(c, K, n) ?? []; };
function castleroomScene(c) {
  const { o } = c;
  let K = castleOf(o);
  const n = Math.max(1, Math.min(K.rooms.length, Math.round(o.castleRoom ?? 1))), style = K.rooms[n - 1];
  // where a trap room's pits lead: the castle's dungeons (the first, then the deep one)
  const d1 = K.rooms.indexOf("dungeon1") + 1, d2 = K.rooms.indexOf("dungeon2") + 1;
  const pits = [d1 || d2, d2 || d1];
  const bones = K.bones[n - 1][style] ?? 0;
  if (style === "hallway" || style === "dining") c = { ...c, eye: 0.3 };
  // (in its own color theme)
  // (ticket 293: theme A's colors kept too, for a throne in theme A in a room in theme B)
  K = { ...K, themeA: { flagA: K.flagA, flagB: K.flagB, carpet: K.carpet } };
  if (K.roomTheme[n - 1] === "B") K = { ...K, ...K.themeB };
  const hallEnd = style === "hallway" && K.hall[n - 1].end === "none" ? "none" : "";
  const places = style === "dining" ? K.places[n - 1] : 0;
  const hens = style === "stable" ? K.hens[n - 1] : 0;   // (ticket 506: this stable's hens)
  const pitShapes = K.pitShapesAt[n - 1];   // (ticket 271: this room's own; keyed, so a chip draws it again at once)
  const trap = style === "trap" ? trapState(o, n, K.id) : null;   // (ticket 305: its pits and levers)
  const draw = () => ROOM_KINDS[style].draw(c, K, { pits, bones, hallEnd, places, hens, pitShapes, trap });
  let s = SEP + part("croom-" + style, [c.F, K.id, style === "trap" ? pits.join() + K.rooms.join() + pitShapes.join() + JSON.stringify(trap) + [o.leverShine, o.leverShineColor, o.leverFlicker].join() : bones,   // (ticket 317: and its levers' shine)
                                  K.flagA, K.flagB, K.flagStyle, OWN_EXIT_ROOMS.includes(style) ? K.carpet : "", style === "stable" ? K.horses.length : "", hallEnd, places, hens,   // (ticket 369: these were swallowed by the note above, so a change to them did not draw the room again)
                                  SCENE_LFO_THINGS["castleroom:" + style] ? sceneLfoKey(o, "fast") + sceneLfoKey(o, "slow") : "",
                                  style === "catroom" || style === "dungeon1" || style === "dungeon2" ? moonOfDateKey(o) : ""], draw);   // (ticket 490: the cat room's and the dungeons' windows' moons, the date's)
  // (the cat room's cats: a part of their own, so a cat's settings draw only the cats again)
  if (style === "catroom") { for (const C of K.cats) void o["lfo:" + C.lfo];   // (as the fairy house: sceneOpts)
    const wags = K.cats.map(catWag);
    s += SEP + part("croom-cats", [c.F, K.id, JSON.stringify(K.cats), wags.map(W => W.key).join("|")], () => catRoomCats(c, K, wags)); }
  // (ticket 293: the king's room's thrones, likewise, and their cats, each cat wagging with its own wag)
  if (style === "king") { for (const T of K.thrones) if (T.cat === "cat") void o["lfo:" + T.lfo];
    const wags = K.thrones.map((T, i) => catWag(T, i + 10));
    s += SEP + part("croom-thrones", [c.F, K.id, JSON.stringify(K.thrones), JSON.stringify(K.themeA), JSON.stringify(K.themeB), wags.map(W => W.key).join("|")], () => kingThrones(c, K, wags)); }
  // (the stable's horses, likewise, each nodding with its own wag)
  if (style === "stable") { for (const Hh of K.horses) void o["lfo:" + Hh.lfo];
    const wags = K.horses.map(catWag);
    s += SEP + part("croom-horses", [c.F, K.id, JSON.stringify(K.horses), K.flagA, K.flagB, wags.map(W => W.key).join("|")], () => stableHorses(c, K, wags)); }
  s += SEP + part("croom-ways", [c.F, K.id, n, style, K.rooms.join(), JSON.stringify(K.hall[n - 1])], () => castleWays(c, K, n, style));
  return s;
}

// Three places away from the woods: a mountain trail, a jungle waterfall,
// and a camp site. (ticket 233: their signposts, fixed links between them, are gone: the world's ways join them)
const PINE_DEF = pineDef("bg-pine", 5);
const P2 = (x, y) => `${f1(x)},${f1(y)}`;
// A snow cap on a peak, cut from the mountain's own two slopes: from the
// peak (px, py) down each slope toward its neighbors (lx, ly) and (rx, ry)
// to a depth `d`, its lower edge ragged (more at the middle), so the snow
// lies on the rock rather than floating over it
const snowCap = (px, py, lx, ly, rx, ry, d, seed, fill = "#ffffff", opacity = 0.92) => {
  const on = (qx, qy, f) => [px + (qx - px) * f, py + (qy - py) * f];
  const fl = Math.min(1, d / Math.max(1, ly - py)), fr = Math.min(1, d / Math.max(1, ry - py));
  const [ax, ay] = on(lx, ly, fl), [bx, by] = on(rx, ry, fr), pts = [[px, py], [ax, ay]];
  const rr = i => { const v = Math.sin(seed * 91.7 + i * 47.3) * 43758.5453; return v - Math.floor(v); };
  for (let i = 1; i < 7; i++) {
    const u = i / 7, x = ax + (bx - ax) * u, y = ay + (by - ay) * u, bite = (i % 2 ? -0.28 : 0.12 + 0.18 * rr(i)) * d * Math.sin(Math.PI * u);
    pts.push([x, y + bite]);
  }
  pts.push([bx, by]);
  return `<polygon points="${pts.map(([x, y]) => P2(x, y)).join(" ")}" fill="${fill}" opacity="${opacity}"/>`;
};
// Sunbeams from a far, still sun: parallel (one slope), each its own width,
// widening a little as it falls, brightest high up; each dims and brightens
// on its own as clouds go by, now and then nearly gone
const sunbeamsSvg = (id, x0, W, k, ytop, ybot, n, seed, slope = 0.42, o = null) => {
  const r = seeded(seed);
  let t = `<defs><linearGradient id="${id}" gradientUnits="userSpaceOnUse" x1="0" y1="${f1(ytop)}" x2="0" y2="${f1(ybot)}">` +
          `<stop offset="0" stop-color="#fff6bf" stop-opacity="1"/><stop offset="1" stop-color="#fff6bf" stop-opacity="0.35"/></linearGradient></defs>`;
  for (let j = 0; j < n; j++) {
    const xt = x0 - W * 0.12 + W * (0.06 + (0.98 / n) * j + 0.07 * r(j)), wt = (14 + 56 * r(j + 10)) * k, wb = wt * (1.15 + 0.25 * r(j + 20));
    const xb = xt + slope * (ybot - ytop), a = 0.14 + 0.2 * r(j + 30), dim = (0.05 + 0.25 * r(j + 40)) * a, dur = 9 + 8 * r(j + 50), begin = -dur * r(j + 60);
    t += `<polygon points="${f1(xt - wt / 2)},${f1(ytop)} ${f1(xt + wt / 2)},${f1(ytop)} ${f1(xb + wb / 2)},${f1(ybot)} ${f1(xb - wb / 2)},${f1(ybot)}" fill="url(#${id})" opacity="${a.toFixed(2)}" pointer-events="none">` +
         (o ? sceneLfo(o, "slow", r(j + 60), "opacity", v => (dim + (a - dim) * (1 + v) / 2).toFixed(3), "") : "") +
         (o && sceneLfoName(o, "slow") ? "" : `<animate attributeName="opacity" values="${a.toFixed(2)};${a.toFixed(2)};${dim.toFixed(3)};${dim.toFixed(3)};${a.toFixed(2)}" keyTimes="0;0.4;0.55;0.68;1" ` +
         `calcMode="spline" keySplines="0 0 1 1;0.4 0 0.6 1;0 0 1 1;0.4 0 0.6 1" dur="${dur.toFixed(1)}s" begin="${begin.toFixed(1)}s" repeatCount="indefinite"/>`) + `</polygon>`;
  }
  return t;
};
// a drift for any drawing of a cloud: slowly there and back, each at its own pace;
// or, given the scene's options (o), with the scene's slow LFO, each its own way in
const cloudDrift = (i, reach, o) => sceneLfo(o, "slow", (i * 0.37) % 1, "transform", (v, amp) => `${f1((reach + 12 * (i % 3)) * (0.4 + 1.8 * amp) * (1 + v) / 2)} 0`,
  `<animateTransform attributeName="transform" type="translate" values="0 0;${reach + 12 * (i % 3)} 0;0 0" dur="${24 + 7 * (i % 4)}s" repeatCount="indefinite"/>`, "translate");
const drifting = (inner, i, reach = 30, o = null) => o ? `<g>${cloudDrift(i, reach, o)}${inner}</g>` :
  `<g><animateTransform attributeName="transform" type="translate" values="0 0;${reach + 12 * (i % 3)} 0;0 0" dur="${24 + 7 * (i % 4)}s" repeatCount="indefinite"/>${inner}</g>`;
// a cloud that drifts, back and forth, slowly
const driftingCloud = (x, y, s, i, o = null) => o ? `<g>${cloudDrift(i, 30, o)}<use href="#bg-cloud" ${tf(x, y, s)}/></g>` :
  `<use href="#bg-cloud" ${tf(x, y, s)}><animateTransform attributeName="transform" type="translate" additive="sum" ` +
  `values="0 0;${30 + 12 * (i % 3)} 0;0 0" dur="${24 + 7 * (i % 4)}s" repeatCount="indefinite"/></use>`;

// The mountain trail: far snowy peaks (their height a slider: they stand
// in a group that stretches them up from the valley), a lake and a village
// far below, drifting clouds, birds circling, and the trail going on up.
// (ticket 50) three big rocks on the slope with no rock or pine standing in
// front of them (rocks left 3, right 1 and right 4, the nearest of their
// heaps; the biggest, left 1, is behind left 3): each one's top (rockTop),
// placed and sized as mountainsScene's "mt-slope" places them, and named as
// it names them
SEATS.mountains = (c, B) => { const { W, g, k } = c;
  return [4, 1, 7].map(i => { const x = (i % 2 ? 1 : -1) * (W * 0.18 + W * 0.28 * sceneR(i, 330)), y = g - 10 * k + 70 * k * sceneR(i, 331), r = (12 + 20 * sceneR(i, 332)) * k;
    return { name: `rock, ${i % 2 ? "right" : "left"} ${Math.floor(i / 2) + 1}`, ...rockTop(x, y, r, 330 + i), z: y + 1, facing: ["front", "left", "right"] }; }); };
