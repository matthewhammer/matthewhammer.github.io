"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 4 of 26 of the game's script, run in order by index.html.

// ===================== HAIR (in face units) =====================
// The Shoes and Hair hairdos, each placed by its crown on the top of the
// head (100, 30.6) and scaled from that slide's head (radius 36) to this one.

const HAIRDOS = {
  short: { crown: [55, 84], pts: [[15,124],[13,98],[23,76],[41,68],[55,67],[69,68],[87,76],[97,98],[95,124]] },
  long:  { crown: [160, 84], pts: [[116,194],[114,104],[126,76],[146,67],[160,66],[174,67],[194,76],[206,104],[204,194],[184,188],[188,124],[160,84],[132,124],[136,188]] },
  curly: { crown: [265, 84], pts: [[221,120],[215,104],[221,90],[217,78],[229,70],[235,60],[249,62],[257,54],[265,60],[273,54],[281,62],[295,60],[301,70],[313,78],[309,90],[315,104],[309,120]] },
};
// Hair length, as a fraction of each style's usual length (1 = as drawn).
const hairLen = h => (h.length ?? 100) / 100;
// Curly hair hanging down from a side, at x, from y = 120 (the cap's
// edge, in the dos' units) to y = 120 + drop: a column of curls, as a
// wavy outer edge going down and a wavy inner edge coming back up.
function curlColumn(xOut, xIn, drop) {
  const n = Math.max(1, Math.round(drop / 12)), step = drop / n, out = [], back = [];
  for (let i = 1; i <= n; i++) {
    const y = 120 + i * step, bump = i % 2 ? 6 : -2;
    out.push([xOut - Math.sign(xIn - xOut) * bump, y - step / 2], [xOut, y]);
    back.unshift([xIn, y - step / 2], [xIn + Math.sign(xIn - xOut) * (i % 2 ? 2 : -3), y]);
  }
  const tipY = 120 + drop;
  return { down: out.concat([[(xOut + xIn) / 2, tipY + 6]]), up: back };
}
function hairSvg(h) {
  const k = 80 / 36, stroke = darker(h.color), L = hairLen(h);
  const fromPts = (d, pts) => pts.map(([x, y]) => [100 + (x - d.crown[0]) * k, 30.6 + (y - d.crown[1]) * k]);
  const fromDo = d => fromPts(d, d.pts);
  switch (h.style) {
    case "short":
      return poly(fromDo(HAIRDOS.short), h.color, stroke, 4);
    case "long": {
      // what hangs below the ears grows (or shrinks) with the length
      const d = HAIRDOS.long, ear = 124;
      return poly(fromPts(d, d.pts.map(([x, y]) => [x, y > ear ? ear + (y - ear) * L : y])), h.color, stroke, 4);
    }
    case "curly": {
      // a cap of curls; longer, and curls hang down at the sides
      const d = HAIRDOS.curly, drop = Math.max(0, (L - 1) * 70);
      if (drop < 4) return poly(fromDo(d), h.color, stroke, 4);
      const left = curlColumn(215, 235, drop), right = curlColumn(315, 295, drop);
      // left column: down its inner edge, across the tip, up its outer edge
      const leftPts = [[235, 120], ...left.up.slice().reverse(), ...left.down.slice().reverse(), [215, 120]];
      const rightPts = [[315, 120], ...right.down, ...right.up, [295, 120]];
      return poly(fromPts(d, [...leftPts, ...d.pts, ...rightPts]), h.color, stroke, 4);
    }
    case "spiky": {
      const ps = [];
      for (let i = 0; i <= 14; i++) {
        const a = Math.PI * (1.05 + i * 0.9 / 14), r = i % 2 ? 84 + 44 * L : 84;
        ps.push([100 + r * Math.cos(a), 110.6 + r * Math.sin(a)]);
      }
      ps.push([100, 120]);
      return poly(ps, h.color, stroke, 4);
    }
    case "afro": {
      // a big round puff over the head, bigger all round with the length,
      // curly at its edge; at the ears it tucks in, ending there (a whole
      // circle reached down past the cheeks and framed the face like a
      // beard). Its outline: round the circle above the ear line, the
      // edge in little curls, then in under the ears, and across behind
      // the face.
      const r = 104 * L, cy = 150 - r, ear = 112, curls = Math.max(12, Math.round(2 * Math.PI * r / 24));
      const half = r * r - (ear - cy) * (ear - cy);
      if (cy + r <= ear || half <= 0) return `<circle cx="100" cy="${f1(cy)}" r="${f1(r)}" fill="${h.color}" stroke="${stroke}" stroke-width="4"/>`;
      // the arc's ends at the ear line, as angles (y down: the right end's
      // angle a0 below the middle, round over the top to the left end's)
      const a0 = Math.asin((ear - cy) / r), from = a0, to = Math.PI - a0 - 2 * Math.PI, n = Math.max(96, curls * 8), pts = [];
      for (let i = 0; i <= n; i++) {
        const a = from + (to - from) * i / n, rr = r - 5 + 6 * Math.abs(Math.sin(curls * a / 2));
        pts.push([100 + rr * Math.cos(a), cy + rr * Math.sin(a)]);
      }
      const w = Math.sqrt(half), tuck = Math.min(72, w - 4);
      const [lx, ly] = pts[pts.length - 1], [rx, ry] = pts[0];
      const d = `M${f1(rx)} ${f1(ry)} ` + pts.slice(1).map(([x, y]) => `L${f1(x)} ${f1(y)}`).join(" ") +
        ` Q${f1(lx)} ${f1(ear + 16)} ${f1(100 - tuck)} ${f1(ear + 14)} L${f1(100 + tuck)} ${f1(ear + 14)} Q${f1(rx)} ${f1(ear + 16)} ${f1(rx)} ${f1(ry)} Z`;
      return `<path d="${d}" fill="${h.color}" stroke="${stroke}" stroke-width="4" stroke-linejoin="round"/>`;
    }
    case "pigtails": {
      // the tails hang from where they're tied, longer and longer
      const ry = 34 * Math.max(0.5, L), rx = 34 * Math.min(1, 0.6 + 0.4 / Math.max(L, 0.5));
      const tail = cx => `<ellipse cx="${cx}" cy="${f1(36 + ry)}" rx="${f1(rx)}" ry="${f1(ry)}" fill="${h.color}" stroke="${stroke}" stroke-width="4"/>`;
      return tail(-8) + tail(208) + poly(fromDo(HAIRDOS.short), h.color, stroke, 4);
    }
  }
  return "";
}
// Long hair's back layer: the hair seen behind the head and shoulders, the
// outline of the long hairdo without the gap for the face, a little darker
// than the hair. Drawn behind everything (head, neck, body, arms), so it
// fills the space between the two sides.
function hairBackSvg(h) {
  if (h.style !== "long") return "";
  const d = HAIRDOS.long, k = 80 / 36, L = hairLen(h), ear = 124;
  const outline = [[116, 194], [114, 104], [126, 76], [146, 67], [160, 66], [174, 67], [194, 76], [206, 104], [204, 194]]
    .map(([x, y]) => [x, y > ear ? ear + (y - ear) * L : y])
    .map(([x, y]) => [100 + (x - d.crown[0]) * k, 30.6 + (y - d.crown[1]) * k]);
  const fill = mix(h.color, "#000000", 0.22);
  return poly(outline, fill, darker(fill), 3);
}
// The front of the hair, the bangs: the hairdo where it lies over the
// head, from the crown down to its fringe, drawn over the face, the eyes
// and the brows (the rest of the hairdo is behind the head). The fringe
// comes down `bangs` of the way from the crown to the top of the brows
// (100: its locks' tips just touch them; past 100, over the brows and the
// eyes; 0, no bangs), its edge in locks of the hairdo's own kind: pointed
// for straight hair, round for curly hair and an afro, a zigzag for spiky
// hair. Outlined where it crosses the hair, and nowhere else (not across a
// long hairdo's parting). In face units.
const BANG_LOCKS = { short: ["point", 7, 9], long: ["point", 6, 9], pigtails: ["point", 7, 9],
                     curly: ["round", 8, 8], afro: ["round", 9, 8], spiky: ["zig", 9, 14] };
// the top of the brows, as drawn (with their lift, their line's half-width)
function browTop(F) {
  const shift = faceLayout(F).eyeY - 100 - browLift(F);
  if (F.unibrow) return Math.min(78, 39 + (50 + idiv(2 * F.brow, 5)) / 2) + shift - 3.5;
  const inner = 66 + idiv(F.brow * 24, 100), top = Math.min(inner, 80) - 8;
  let m = Infinity;
  for (let i = 0; i <= 20; i++) { const t = i / 20; m = Math.min(m, (1 - t) * (1 - t) * 80 + 2 * t * (1 - t) * top + t * t * inner); }
  return m + shift - 3;
}
function bangsSvg(h, F) {
  const b = (h.bangs ?? 100) / 100, hair = BANG_LOCKS[h.style] && b > 0 ? hairSvg(h) : "";
  if (!hair) return "";
  const crown = headCrown(F), Yf = Math.min(headChin(F), crown + (browTop(F) - crown) * b);
  const [shape, n, depth] = BANG_LOCKS[h.style];
  if (Yf <= crown + depth) return "";
  // the locks, across the whole head (the head's outline clips them)
  const half = 80 * headWidth(F) * (1 + 0.12 * F.alien / 100) + 6, xL = 100 - half, w = 2 * half / n, up = Yf - depth;
  let edge = "";
  for (let j = 0; j < n; j++) {
    const x = xL + j * w;
    edge += shape === "round" ? ` Q${f1(x + w / 2)} ${f1(Yf + depth)} ${f1(x + w)} ${f1(up)}`
          : shape === "zig" ? ` L${f1(x + w / 2)} ${f1(Yf)} L${f1(x + w)} ${f1(up)}`
          : ` Q${f1(x + 0.35 * w)} ${f1(Yf - 0.2 * depth)} ${f1(x + 0.58 * w)} ${f1(Yf)} Q${f1(x + 0.72 * w)} ${f1(Yf - 0.55 * depth)} ${f1(x + w)} ${f1(up)}`;
  }
  const above = `M${f1(xL - 400)} ${f1(up)} L${f1(xL)} ${f1(up)}${edge} L${f1(xL + 2 * half + 400)} ${f1(up)} V-3000 H${f1(xL - 400)} Z`;
  // (the head's outline grown a little, so the bangs cover its line where
  // the hair goes over it, with no seam)
  const W = headWidth(F), H = headH(F);
  const head = "M" + headPoints(F).map(([x, y]) => f1(x) + " " + f1(y)).join(" L") + " Z";
  const grow = `translate(100 ${HEAD_C}) scale(${(1 + 2.5 / (80 * W)).toFixed(4)} ${(1 + 2.5 / (80 * H)).toFixed(4)}) translate(-100 ${-HEAD_C})`;
  const white = hair.replace(/fill="[^"]*"/g, 'fill="#ffffff"').replace(/stroke="[^"]*"/g, 'stroke="#ffffff"');
  const defs = `<clipPath id="bangs-head"><path d="${head}" transform="${grow}"/></clipPath><clipPath id="bangs-above"><path d="${above}"/></clipPath>` +
    `<mask id="bangs-hair" maskUnits="userSpaceOnUse" x="-3000" y="-3000" width="6000" height="6000"><g transform="${hairStretch(F)}">${white}</g></mask>`;
  const bangs = `<g clip-path="url(#bangs-head)"><g clip-path="url(#bangs-above)"><g transform="${hairStretch(F)}">${hair}</g></g>` +
    `<path d="M${f1(xL)} ${f1(up)}${edge}" fill="none" stroke="${darker(h.color)}" stroke-width="4" stroke-linejoin="round" stroke-linecap="round" mask="url(#bangs-hair)"/></g>`;
  if (F.burst <= 0) return `<defs>${defs}</defs>` + bangs;
  // a head exploding: the bangs split along its crack, the top half going
  // with the lid (as faceSvg moves it), the rest staying on the face
  const crackY = HEAD_C - 50.6 * F.faceH / 100, lift = idiv(F.burst * 7, 10), tilt = idiv(F.burst, 4);
  return `<defs>${defs}<clipPath id="bangs-lid"><rect x="-2000" y="-4000" width="4200" height="${f1(4000 + crackY)}"/></clipPath>` +
         `<clipPath id="bangs-rest"><rect x="-2000" y="${f1(crackY)}" width="4200" height="4000"/></clipPath></defs>` +
         `<g clip-path="url(#bangs-rest)">${bangs}</g>` +
         `<g transform="translate(0 ${-lift}) rotate(${-tilt} 100 ${f1(crackY)})"><g clip-path="url(#bangs-lid)">${bangs}</g></g>`;
}
// (ticket 124) Hair seen from the side, in the face's units, on the head
// as drawn (not stretched again with it). `cap`: the hair on the head, over
// its top and back down to the nape, the hairline at the forehead coming
// down as far as the bangs say, its edge in the locks of its kind (pointed,
// round, a zigzag) and its outside round, curly or spiky; long hair comes
// down in front of the ear. `back`: what hangs behind the head, over the
// body: long hair down the back, curls, the one pigtail on the far side, an
// afro's puff. `ear`: where the ear shows, under short hair or none.
function sideHair(h, F, dir) {
  const pts = headPoints(F), n = pts.length, R = 80 * headWidth(F), cx = 100, cy = HEAD_C, L = hairLen(h);
  const color = h.color, stroke = darker(h.color), style = h.style;
  const long = style === "long" || (style === "curly" && L > 1.05);
  const ear = [cx - dir * 0.12 * R, cy + 0.06 * 80 * F.faceH / 100];
  // a point of the head's outline at an angle (degrees: 0 the front, the way
  // they face; -90 the top; 180 the back), pushed out by `grow`
  const at = (deg, grow = 0) => {
    const t = (((dir > 0 ? deg : 180 - deg) % 360) + 360) % 360, f = t / 5, i = Math.floor(f) % n, j = (i + 1) % n, u = f - Math.floor(f);
    const x = pts[i][0] + (pts[j][0] - pts[i][0]) * u, y = pts[i][1] + (pts[j][1] - pts[i][1]) * u, dx = x - cx, dy = y - cy, r = Math.hypot(dx, dy) || 1;
    return [x + dx / r * grow, y + dy / r * grow];
  };
  const P = ([x, y]) => f1(x) + " " + f1(y), X = u => cx + dir * u * R, Y = v => cy + v * R;
  if (style === "none" || !BANG_LOCKS[style]) return { cap: "", back: "", ear };
  // the forehead's hairline: as low as the bangs reach, never above the
  // head's front top
  const b = Math.min(1.3, (h.bangs ?? 100) / 100), crown = headCrown(F), yf = crown + (browTop(F) - crown) * b;
  let df = -62;
  for (let d = -62; d <= 10; d += 2) { if (at(d)[1] >= yf) break; df = d; }
  // its outside: from the forehead back over the top to the nape (or, long,
  // to the back of the jaw)
  const end = long ? 130 : 160, edge = [];
  for (let d = df, i = 0; d >= end - 360; d -= 5, i++) {
    const g = style === "spiky" ? (i % 2 && d < -20 ? 8 + 30 * L : 8) : style === "curly" || style === "afro" ? (i % 2 ? 13 : 7) : 7;
    edge.push(at(d, g));
  }
  // the locks along the hairline from the top of the ear (or the temple,
  // long) up to the forehead
  const [shape, , depth] = BANG_LOCKS[style];
  const from = long ? [X(0.1), Y(-0.38)] : [X(-0.02), ear[1] - 26], to = at(df), nl = 3;
  let locks = "";
  for (let k = 1; k <= nl; k++) {
    const a = [from[0] + (to[0] - from[0]) * (k - 1) / nl, from[1] + (to[1] - from[1]) * (k - 1) / nl], c = [from[0] + (to[0] - from[0]) * k / nl, from[1] + (to[1] - from[1]) * k / nl];
    const m = [(a[0] + c[0]) / 2, (a[1] + c[1]) / 2 + depth];
    locks += shape === "round" ? ` Q${P(m)} ${P(c)}` : shape === "zig" ? ` L${P(m)} L${P(c)}` : ` Q${P([m[0] - dir * 3, m[1] - depth * 0.3])} ${P(m)} L${P(c)}`;
  }
  // the inside, from the nape round the ear (short) or down in front of it
  // (long), to the locks
  const inside = long ? [[X(-0.18), Y(0.82)], [X(0.0), Y(0.3)], from]
                      : [[ear[0] - dir * 17, ear[1] + 14], [ear[0] - dir * 15, ear[1] - 18], [ear[0] + dir * 12, ear[1] - 24], from];
  const cap = `<path d="M${P(to)} ${edge.map(p => "L" + P(p)).join(" ")} ${inside.map(p => "L" + P(p)).join(" ")}${locks} Z" fill="${color}" stroke="${stroke}" stroke-width="4" stroke-linejoin="round"/>`;
  let back = "";
  if (style === "long") {
    // down the back to its length (as the long hairdo's ends: 159 + 155 times the length)
    const bottom = 159 + 155 * L;
    back = poly([[X(0), Y(0.1)], [X(-1.05), Y(-0.35)], [X(-1.12), bottom - 24], [X(-0.85), bottom], [X(-0.5), bottom - 8], [X(-0.12), Y(0.85)]], mix(color, "#000000", 0.12), stroke, 4);
  } else if (style === "curly" && L > 1.05) {
    const drop = (L - 1) * 155;
    for (let y = Y(0.35); y < Y(0.35) + drop; y += 20) back += `<circle cx="${f1(X(-0.9) + dir * ((y / 20) % 2 ? 6 : -6))}" cy="${f1(y)}" r="16" fill="${color}" stroke="${stroke}" stroke-width="4"/>`;
  } else if (style === "pigtails") {
    const ry = 34 * Math.max(0.5, L), rx = 34 * Math.min(1, 0.6 + 0.4 / Math.max(L, 0.5));
    back = `<ellipse cx="${f1(X(-1.15))}" cy="${f1(Y(-0.3) + ry)}" rx="${f1(rx)}" ry="${f1(ry)}" fill="${color}" stroke="${stroke}" stroke-width="4"/>`;
  } else if (style === "afro") {
    const r = 104 * L;
    back = `<circle cx="${f1(X(-0.35))}" cy="${f1(150 - r)}" r="${f1(r)}" fill="${color}" stroke="${stroke}" stroke-width="4"/>`;
  }
  return { cap, back, ear: long ? null : ear };
}
// (ticket 138) Hair seen from behind, in the face's units, over the head as
// drawn: the back of the head covered from the top down to the nape, its
// lower edge in the locks of its kind and its outside round, curly or
// spiky (an afro's a big round puff); long hair hanging down the back to
// its length, long curls down it. What shows at the sides (pigtails, the
// long sides) is hairSvg's, still drawn behind the head.
function backHair(h, F) {
  const style = h.style;
  if (style === "none" || !BANG_LOCKS[style]) return "";
  const pts = headPoints(F), n = pts.length, R = 80 * headWidth(F), cx = 100, cy = HEAD_C, L = hairLen(h);
  const color = h.color, stroke = darker(color), P = ([x, y]) => f1(x) + " " + f1(y);
  // (a point of the head's outline at an angle, degrees, y down: 0 to our
  // right, -90 or 270 the top; pushed out by grow, as sideHair's)
  const at = (deg, grow = 0) => {
    const t = ((deg % 360) + 360) % 360, f = t / 5, i = Math.floor(f) % n, j = (i + 1) % n, u = f - Math.floor(f);
    const x = pts[i][0] + (pts[j][0] - pts[i][0]) * u, y = pts[i][1] + (pts[j][1] - pts[i][1]) * u, dx = x - cx, dy = y - cy, r = Math.hypot(dx, dy) || 1;
    return [x + dx / r * grow, y + dy / r * grow];
  };
  let t = "";
  if (style === "long") {
    // (down the back, as the long hairdo's ends: 159 + 155 times the length)
    const bottom = 159 + 155 * L;
    t += poly([[cx - 1.0 * R, cy - 0.15 * R], [cx - 1.1 * R, bottom - 24], [cx - 0.55 * R, bottom], [cx, bottom - 10], [cx + 0.55 * R, bottom],
               [cx + 1.1 * R, bottom - 24], [cx + 1.0 * R, cy - 0.15 * R]], color, stroke, 4);
  } else if (style === "curly" && L > 1.05) {
    const drop = (L - 1) * 155;
    for (let y = cy + 0.35 * R; y < cy + 0.35 * R + drop; y += 20)
      for (const u of [-0.6, -0.2, 0.2, 0.6]) t += `<circle cx="${f1(cx + u * R + ((y / 20) % 2 ? 6 : -6))}" cy="${f1(y)}" r="16" fill="${color}" stroke="${stroke}" stroke-width="4"/>`;
  }
  // the cap: over the top from one side to the other, down to 20 degrees
  // below level (an afro's, all round but its bottom)
  const low = style === "afro" ? 60 : 20, edge = [];
  for (let d = 180 - low, i = 0; d <= 360 + low; d += 5, i++) {
    const g = style === "spiky" ? (i % 2 && d > 200 && d < 340 ? 8 + 30 * L : 8) : style === "afro" ? (i % 2 ? 16 : 8) + 30 * L : style === "curly" ? (i % 2 ? 13 : 7) : 7;
    edge.push(at(d, g));
  }
  // the nape: locks from our right back to our left
  const [shape, , depth] = BANG_LOCKS[style], a = edge[edge.length - 1], b = edge[0], nl = 4;
  let locks = "";
  for (let k = 1; k <= nl; k++) {
    const p = [a[0] + (b[0] - a[0]) * (k - 1) / nl, a[1] + (b[1] - a[1]) * (k - 1) / nl], q = [a[0] + (b[0] - a[0]) * k / nl, a[1] + (b[1] - a[1]) * k / nl];
    const m = [(p[0] + q[0]) / 2, (p[1] + q[1]) / 2 + depth];
    locks += shape === "round" ? ` Q${P(m)} ${P(q)}` : shape === "zig" ? ` L${P(m)} L${P(q)}` : ` Q${P([m[0] + 3, m[1] - depth * 0.3])} ${P(m)} L${P(q)}`;
  }
  return t + `<path d="M${P(b)} ${edge.slice(1).map(p => "L" + P(p)).join(" ")}${locks} Z" fill="${color}" stroke="${stroke}" stroke-width="4" stroke-linejoin="round"/>`;
}
// A fairy's wreath of flowers, worn round her head on her forehead, above
// her brows: a band of vine and leaves, the front of the ring dipping a
// little toward the brows (seen from a little above), flowers along it,
// smaller toward the sides as the ring turns away. Over her hair and
// bangs. Its flowers all of one kind, or every color. In face units.
const WREATH_SETS = { mix: { name: "🌈 every color", petals: ["#f472b6", "#ffffff", "#facc15", "#60a5fa", "#a78bfa", "#fb7185"], mid: "#f59e0b" },
                      pink: { name: "🌸 pink roses", petals: ["#f472b6", "#ec4899", "#f9a8d4"], mid: "#be185d" },
                      white: { name: "🤍 daisies", petals: ["#ffffff"], mid: "#facc15" },
                      yellow: { name: "🌼 buttercups", petals: ["#facc15", "#fde047"], mid: "#ca8a04" },
                      blue: { name: "💙 forget-me-nots", petals: ["#60a5fa", "#93c5fd"], mid: "#fde047" },
                      purple: { name: "💜 violets", petals: ["#a78bfa", "#7c3aed", "#c4b5fd"], mid: "#fde047" } };
function wreathSvg(F, kind = "mix") {
  const set = WREATH_SETS[kind] || WREATH_SETS.mix, rnd = (i, s) => { const v = Math.sin(i * 12.9898 + s * 78.233) * 43758.5453; return v - Math.floor(v); };
  const crown = headCrown(F), bt = browTop(F), fr = 11;
  const cy = bt - (bt - crown) * 0.55, [xl, xr] = acrossAt(headPoints(F), cy);
  const cx = (xl + xr) / 2, rx = (xr - xl) / 2 + 6, sag = Math.max(2, Math.min(12, bt - cy - fr * 0.8));
  const at = t => [cx - rx * Math.cos(Math.PI * t), cy + sag * Math.sin(Math.PI * t)];
  // the vine
  let band = "M" + [...Array(21)].map((_, i) => at(i / 20).map(f1).join(" ")).join(" L");
  let t = `<path d="${band}" fill="none" stroke="#2f6e3a" stroke-width="8" stroke-linecap="round"/>` +
          `<path d="${band}" fill="none" stroke="#4a9a52" stroke-width="3.5" stroke-linecap="round"/>`;
  // leaves along it, pointing either way
  for (let i = 0; i < 14; i++) {
    const u = (i + 0.5) / 14, [x, y] = at(u), a = (i % 2 ? -1 : 1) * (35 + 20 * rnd(i, 1)) + (u - 0.5) * 40, k = 0.7 + 0.5 * Math.sin(Math.PI * u);
    t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(8.5 * k)}" ry="${f1(3.6 * k)}" transform="rotate(${f1(a)} ${f1(x)} ${f1(y)}) translate(${f1(6 * k)} 0)" fill="${i % 3 ? "#3f8f48" : "#5cb85c"}" stroke="#2f6e3a" stroke-width="0.8"/>`;
  }
  // flowers: five petals round a middle, smaller toward the sides
  const n = 6;
  for (let i = 0; i < n; i++) {
    const u = 0.06 + 0.88 * i / (n - 1), [x, y] = at(u), k = (0.62 + 0.38 * Math.sin(Math.PI * u)) * (0.9 + 0.2 * rnd(i, 2));
    const pc = set.petals[(i * 2 + Math.floor(rnd(i, 3) * 3)) % set.petals.length], r = fr * k, spin = rnd(i, 4) * 72;
    t += [...Array(5)].map((_, j) => { const a = (spin + j * 72) * Math.PI / 180, px = x + Math.cos(a) * r * 0.55, py = y + Math.sin(a) * r * 0.55;
      return `<ellipse cx="${f1(px)}" cy="${f1(py)}" rx="${f1(r * 0.55)}" ry="${f1(r * 0.4)}" transform="rotate(${f1(spin + j * 72)} ${f1(px)} ${f1(py)})" fill="${pc}" stroke="${darker(pc)}" stroke-width="0.8"/>`; }).join("") +
         `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(r * 0.3)}" fill="${set.mid}" stroke="${darker(set.mid)}" stroke-width="0.8"/>`;
  }
  // (a head exploding: the wreath goes off with the lid)
  if (F.burst > 0) t = `<g transform="translate(0 ${-idiv(F.burst * 7, 10)}) rotate(${-idiv(F.burst, 4)} 100 ${f1(HEAD_C - 50.6 * F.faceH / 100)})">${t}</g>`;
  return t;
}
// How far hair reaches, in face units (before the head's stretch): its
// top, its bottom and its half-width about x = 100, to size the picture.
function hairReach(h) {
  const L = hairLen(h), k = 80 / 36;
  switch (h.style) {
    case "long": return { top: 0, bottom: 30.6 + (124 - 84 + 70 * L) * k, half: 100 };
    case "curly": return { top: -40, bottom: 30.6 + (120 - 84 + Math.max(0, (L - 1) * 70) + 6) * k, half: 115 };
    case "spiky": return { top: 110.6 - 84 - 44 * L, bottom: 120, half: 84 + 44 * L };
    case "afro": return { top: 150 - 2 * 104 * L, bottom: Math.min(150, 126), half: 104 * L };
    case "pigtails": return { top: 0, bottom: 36 + 2 * 34 * Math.max(0.5, L), half: 150 };
    default: return { top: 0, bottom: 120, half: 100 };
  }
}

// The head's outline, around its center (100, 110.6): a circle of radius
// 80 stretched by face width and height, and, as `alien` goes up, wider
// across the top and narrowing to a pointed chin, like an alien's head.
const HEAD_C = 110.6;
// The head's width, as a stretch of the round head: the face width, or
// wider when the eyes need it -- big, narrow-and-slanted, or far apart
// eyes, plus a margin of 8 beside each, at the eye line. (There the head
// is the round head's width at that height, times the alien top's 12%.)
function headWidth(F) {
  const LY = faceLayout(F), a = F.alien / 100;
  const sn = Math.max(-1, Math.min(1, (LY.eyeY - HEAD_C) / (80 * (F.faceH / 100) * (1 + 0.05 * a))));
  const across = 80 * Math.sqrt(1 - sn * sn) * (1 + 0.12 * a);
  // ...and wide enough for the mouth: 4 past its corners (half its width,
  // 40 of its units, and half its line; 4, not the eyes' 8, which would
  // have widened the alien face's narrow chin at the usual mouth), where
  // the bottom half crosses
  // the corners' line -- the bottom half as tall as the mouth stretched it
  // (not as the eyes do: that would ask the head's width again)
  const low = 80 * (F.faceH / 100) * (1 + 0.25 * a) + LY.stretch;
  const sm = Math.max(0, Math.min(0.95, (LY.mouthY - HEAD_C) / low));
  const atMouth = 80 * Math.sqrt(1 - sm * sm) * (1 - 0.85 * a * sm);
  return Math.max(F.faceW / 100, (LY.eyeDX + eyeHalfW(F) + 8) / across, (42.5 * LY.mx + 4) / atMouth);
}
// How far the head's top half (side -1) or bottom half (side 1) must
// reach from its middle to hold the eyes, if it is an ellipse as wide as
// the head: the farthest, over 24 points around the eye grown by 8, of
// |py| / sqrt(1 - (px / A)^2), the half-height an ellipse A wide needs
// for the point (px, py) to be inside it.
function eyeNeed(F, side) {
  const LY = faceLayout(F), A = 80 * headWidth(F) * (1 + 0.12 * F.alien / 100);
  const w = eyeHalfW(F) + 8, h = eyeHalfH(F) + 8;
  let need = 0;
  for (let i = 0; i < 24; i++) {
    const t = i * Math.PI / 12;
    const px = LY.eyeDX + w * Math.cos(t), py = LY.eyeY - HEAD_C + h * Math.sin(t);
    if (py * side <= 0) continue;
    const u = Math.min(0.99, Math.abs(px) / A);
    need = Math.max(need, Math.abs(py) / Math.sqrt(1 - u * u));
  }
  return need;
}
// How far the head's top half grows for the eyes, as a deep mouth grows
// its bottom half (mouthDrop). Big or tall eyes push the crown up, for
// two things. The forehead: once the eyes reach 25% higher above the
// head's middle than the usual eyes would on this face (23.6 on the
// usual face), the top half grows with them, so the hair's fringe and
// the brows, which stretch with it, keep their room above the eyes. And
// the outline: the eyes stay 8 inside it all round (eyeNeed), which
// matters for eyes far apart, where the head is lower. Nothing at
// everyday sizes.
function eyeLift(F) {
  const LY = faceLayout(F), a = F.alien / 100;
  const upper = 80 * (F.faceH / 100) * (1 + 0.05 * a);
  const reach = HEAD_C - (LY.eyeY - eyeHalfH(F));
  const usual = (HEAD_C - LY.eyeY) + 13;
  const forehead = upper * reach / (usual * 1.25);
  return Math.max(0, forehead - upper, eyeNeed(F, -1) - upper);
}
// ...and how far its bottom half grows: the eyes stay 8 inside the
// outline below them too (eyeNeed). A deep mouth may want more
// (mouthDrop); the head takes the larger of the two.
function headDrop(F) {
  const lower = 80 * (F.faceH / 100) * (1 + 0.25 * F.alien / 100);
  return Math.max(mouthDrop(F), eyeNeed(F, 1) - lower);
}
// The head's height as a stretch of the round head, for what is drawn
// over it (hair, a hood): the face height, and the top half's growth.
const headH = F => F.faceH / 100 + eyeLift(F) / 80;
function headPoints(F) {
  const W = headWidth(F), H = F.faceH / 100, a = F.alien / 100, pts = [], drop = headDrop(F), lift = eyeLift(F);
  for (let i = 0; i < 72; i++) {
    const t = i * Math.PI / 36, c = Math.cos(t), sn = Math.sin(t);
    const sx = c * (1 + a * (sn < 0 ? 0.12 : -0.85 * sn));
    const sy = sn * (1 + a * (sn > 0 ? 0.25 : 0.05));
    pts.push([100 + 80 * W * sx, HEAD_C + 80 * H * sy + (sn > 0 ? drop : lift) * sn]);
  }
  return pts;
}
// Where the head's outline crosses the line y = Y: its left and right x.
function acrossAt(pts, Y) {
  const xs = [];
  pts.forEach(([x1, y1], i) => {
    const [x2, y2] = pts[(i + 1) % pts.length];
    if ((y1 - Y) * (y2 - Y) <= 0 && y1 !== y2) xs.push(x1 + (x2 - x1) * (Y - y1) / (y2 - y1));
  });
  return xs.length ? [Math.min(...xs), Math.max(...xs)] : [100, 100];
}
const headChin = F => HEAD_C + 80 * (F.faceH / 100) * (1 + 0.25 * F.alien / 100) + headDrop(F);
const headCrown = F => HEAD_C - 80 * (F.faceH / 100) * (1 + 0.05 * F.alien / 100) - eyeLift(F);
// The head's stretch, for the hair: hair drawn for the round head is
// stretched the same way about the head's center.
const hairStretch = F =>
  `translate(100 ${HEAD_C}) scale(${(headWidth(F) * (1 + 0.12 * F.alien / 100)).toFixed(3)} ` +
  `${headH(F).toFixed(3)}) translate(-100 ${-HEAD_C})`;

// What sits on top of the head (hair, or a costume's hood) when the head
// explodes: it flies off with the lid, tilting as the lid does, and higher
// than the lid, more so the bigger the explosion, as if they were coming
// apart. (Empty when the head is whole.)
function topFlies(F) {
  const b = F.burst;
  if (b <= 0) return "";
  const lift = idiv(b * 7, 10) + idiv(b * 4, 10), tilt = idiv(b, 4);
  return `translate(0 ${-lift}) rotate(${-tilt} 100 ${f1(HEAD_C - 50.6 * F.faceH / 100)})`;
}
// (with parts, its three layers apart: what is under the eyes, the eyes,
// and what is over them -- the nose, brows and mouth -- each to be made
// see-through on its own; see VISIBILITY)
// (ticket 125) An ear, in the face's units, `k` times the usual size, in the
// skin's color: its base on the head at x0, sticking out to side s (1 our
// right, -1 our left), its middle at height y. "free": a round top, the lobe
// hanging free below the head's line; "attached": the lobe running straight
// into the head; "elf": pointing up and out to a tip, the lobe free. A curl
// inside, the ear's rim.
function earSvg(kind, k, color, x0, y, s) {
  const w = 28 * k, h = 50 * k, P = (u, v) => f1(x0 + s * u * w) + " " + f1(y + v * h);
  const lobe = kind === "attached" ? `Q${P(0.95, 0.25)} ${P(0.6, 0.4)} Q${P(0.35, 0.5)} ${P(0, 0.52)}`
                                   : `Q${P(0.95, 0.25)} ${P(0.65, 0.38)} Q${P(0.55, 0.62)} ${P(0.3, 0.58)} Q${P(0.12, 0.52)} ${P(0.15, 0.36)} L${P(0, 0.3)}`;
  const top = kind === "elf" ? `M${P(0, -0.38)} Q${P(0.35, -0.55)} ${P(1.4, -1.1)} Q${P(1.1, -0.45)} ${P(1.0, -0.05)}`
                             : `M${P(0, -0.45)} Q${P(0.15, -0.62)} ${P(0.6, -0.6)} Q${P(1.05, -0.55)} ${P(1.0, -0.1)}`;
  const curl = kind === "elf" ? `M${P(0.32, -0.3)} Q${P(0.85, -0.55)} ${P(0.75, 0)} Q${P(0.7, 0.2)} ${P(0.45, 0.28)}`
                              : `M${P(0.3, -0.35)} Q${P(0.75, -0.4)} ${P(0.72, 0)} Q${P(0.7, 0.2)} ${P(0.45, 0.28)}`;
  return `<path d="${top} ${lobe} Z" fill="${color}" stroke="black" stroke-width="3" stroke-linejoin="round"/>` +
         `<path d="${curl}" fill="none" stroke="${darker(color)}" stroke-width="2.5" stroke-linecap="round"/>`;
}
// (ticket 124) in profile, the face's front edge from its middle, in the face's units
const profileEdge = F => 80 * headWidth(F) * (1 + 0.12 * F.alien / 100) - 8;
// (ticket 442, Matthew: "let's add beard to the hair sub-panel, with its own controls ... maybe useful also for the gnome
// father character, and other characters too (king, maybe the monk/druid -- having a few styles would be helpful -- maybe 4
// to start") A BEARD, in the face's units: hair.beard, one of BEARDS; hair.beardShade, a colour or "auto" (the hair's);
// hair.moustache. Short, full and long grow from the cheeks down round the mouth (left open, so the mouth and its moods show)
// to a rounded point below the chin, `long` below it; a goatee is only a tuft under the mouth to below the chin. In profile
// (dir), along the jaw from below the ear to the chin's front and down to its point; none from behind. The moustache over
// the mouth (at its front, in profile). The gnome man's and the wizard's beards are these (ticket 431, 432; set() puts one on).
const BEARDS = { none: null, short: { long: 16, flare: 0 }, full: { long: 74, flare: 10 }, long: { long: 150, flare: 12 }, goatee: { long: 24, goatee: true },
                 braided: { long: 300, flare: 8, braid: true } };
// (ticket 443, Matthew: "add new style, called braided ... let's permit the user to control the width of the braids, so they
// can be separate (like tree house tree trunk) or so fat that they don't show many or any gaps between them"; "give each
// beard style a length control") hair.beardLength, a share of each style's own length below the chin (100 its usual);
// hair.braidWidth, a share of the braid's lobes' usual width (narrow, apart with gaps; wide, no gaps at all).
const beardLong = (style, H) => BEARDS[style] ? BEARDS[style].long * Math.max(0.2, (Number(H.beardLength) || 100) / 100) : 0;
// A PLAIT, in the face's units: hanging at cx from y0 to y1, a chain of slanted lobes, left and right in turn, each upper one
// over the one below (drawn from the bottom up), narrowing a little toward the end; wk the lobes' width (1 usual); then a tie
// and a tuft. The tree house trunk's braid (braidSvg) is its cousin: strands apart, crossing as they rise.
function plaitSvg(cx, y0, y1, wk, c, ln) {
  const len = y1 - y0; if (len < 12) return "";
  // (the lobes stay long and slanted as they widen, side by side further apart: a plait, never a string of beads)
  const n = Math.max(2, Math.round(len / (18 + 6 * Math.min(2, wk)))), step = len / n;
  let s = "";
  for (let i = n - 1; i >= 0; i--) {
    const u = i / n, taper = 1 - 0.4 * u, side = i % 2 ? 1 : -1, x = cx + side * (5 + 4 * wk) * taper, y = y0 + (i + 0.5) * step;
    const rx = Math.max(2, 11 * wk * taper), ry = Math.max(step * 0.8, rx * 1.6);
    s += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(rx)}" ry="${f1(ry)}" transform="rotate(${-side * 32} ${f1(x)} ${f1(y)})" fill="${c}" stroke="${ln}" stroke-width="2"/>`;
  }
  const tw = Math.max(5, 11 * wk * 0.6);
  return s + `<path d="M${f1(cx - tw)} ${f1(y1 + 2)} Q${f1(cx - tw - 6)} ${f1(y1 + 22)} ${f1(cx - 3)} ${f1(y1 + 26)} Q${f1(cx + tw + 6)} ${f1(y1 + 22)} ${f1(cx + tw)} ${f1(y1 + 2)} Z" fill="${c}" stroke="${ln}" stroke-width="2"/>` +
    `<rect x="${f1(cx - tw - 1)}" y="${f1(y1 - 4)}" width="${f1(2 * tw + 2)}" height="7" rx="3" fill="${ln}"/>`;
}
const BEARD_WHITE = "#f4f1ea";
const beardShade = H => !H.beardShade || H.beardShade === "auto" ? H.color : H.beardShade;
function beardSvg(F, style, c, dir = 0, moustache = true, H = { beardLength: 100, braidWidth: 100 }) {
  const B = BEARDS[style]; if (!B) return "";
  const ln = darker(c), LY = faceLayout(F), pts = headPoints(F), chin = headChin(F), long = beardLong(style, H);
  // (a braided beard: a full one on the face, gathered below the chin into the plait, which hangs `long` below it)
  const cheekY = LY.eyeY + 24, [xl, xr] = acrossAt(pts, cheekY), my = LY.mouthY, tipY = chin + (B.braid ? 36 : long), mw = 24 * (F.mouthSize ?? 100) / 100;
  const plait = B.braid ? plaitSvg(dir ? 100 + dir * profileEdge(F) * 0.4 : 100, chin + 24, chin + 24 + long, Math.max(0.2, (Number(H.braidWidth) || 100) / 100), c, ln) : "";
  let out = plait;
  const st = `stroke="${ln}" stroke-width="3" stroke-linejoin="round"`;
  if (!dir) {
    if (B.goatee) out += `<path d="M${f1(100 - mw * 0.7)} ${f1(my + 22)} Q100 ${f1(my + 14)} ${f1(100 + mw * 0.7)} ${f1(my + 22)} Q${f1(100 + mw * 0.6)} ${f1(tipY - 6)} 100 ${f1(tipY)} Q${f1(100 - mw * 0.6)} ${f1(tipY - 6)} ${f1(100 - mw * 0.7)} ${f1(my + 22)} Z" fill="${c}" ${st}/>`;
    else {
      const fl = B.flare, bot = Math.max(chin - 18, tipY - 18), side = Math.min(36, 20 + B.long / 5);
      const outer = `M${f1(xl + 3)} ${f1(cheekY)} Q${f1(xl - fl)} ${f1(Math.min(chin, tipY))} ${f1(100 - side)} ${f1(bot)} Q100 ${f1(tipY + 10)} ${f1(100 + side)} ${f1(bot)} ` +
                    `Q${f1(xr + fl)} ${f1(Math.min(chin, tipY))} ${f1(xr - 3)} ${f1(cheekY)} Q${f1(xr - 14)} ${f1(my - 6)} 100 ${f1(my - 4)} Q${f1(xl + 14)} ${f1(my - 6)} ${f1(xl + 3)} ${f1(cheekY)} Z`;
      const hole = `M${f1(100 - mw - 6)} ${f1(my + 2)} Q100 ${f1(my - 8)} ${f1(100 + mw + 6)} ${f1(my + 2)} Q${f1(100 + mw + 2)} ${f1(my + 30)} 100 ${f1(my + 32)} Q${f1(100 - mw - 2)} ${f1(my + 30)} ${f1(100 - mw - 6)} ${f1(my + 2)} Z`;
      out += `<path d="${outer} ${hole}" fill="${c}" fill-rule="evenodd" ${st}/>`;
      if (tipY - chin > 40) out += [-22, 0, 22].map(dx => `<path d="M${f1(100 + dx)} ${f1(my + 44)} Q${f1(100 + dx * 1.2)} ${f1(tipY - 30)} ${f1(100 + dx * 0.8)} ${f1(tipY - 14)}" fill="none" stroke="${ln}" stroke-width="2" stroke-linecap="round" opacity="0.6"/>`).join("");
    }
    const ny = my - 4;
    if (moustache) out += `<path d="M100 ${f1(ny - 9)} Q${f1(100 - mw - 6)} ${f1(ny - 14)} ${f1(100 - mw - 16)} ${f1(ny + 8)} Q${f1(100 - mw * 0.5)} ${f1(ny + 6)} 100 ${f1(ny + 1)} ` +
                           `Q${f1(100 + mw * 0.5)} ${f1(ny + 6)} ${f1(100 + mw + 16)} ${f1(ny + 8)} Q${f1(100 + mw + 6)} ${f1(ny - 14)} 100 ${f1(ny - 9)} Z" fill="${c}" ${st}/>`;
    return out;
  }
  const edge = profileEdge(F), fx = 100 + dir * (edge - 6), bx = 100 - dir * 14;
  if (B.goatee) out += `<path d="M${f1(fx - dir * 16)} ${f1(my + 16)} Q${f1(fx + dir * 4)} ${f1(my + 14)} ${f1(fx + dir * 2)} ${f1(chin)} Q${f1(fx)} ${f1(tipY)} ${f1(fx - dir * 8)} ${f1(tipY + 2)} Q${f1(fx - dir * 20)} ${f1(chin)} ${f1(fx - dir * 16)} ${f1(my + 16)} Z" fill="${c}" ${st}/>`;
  else out += `<path d="M${f1(bx)} ${f1(cheekY)} Q${f1(100 + dir * edge * 0.6)} ${f1(my - 2)} ${f1(fx)} ${f1(my + 6)} Q${f1(fx + dir * 6)} ${f1(Math.min(chin + 10, tipY))} ${f1(100 + dir * edge * 0.55)} ${f1(tipY)} ` +
              `Q${f1(100 - dir * 4)} ${f1(tipY - 12)} ${f1(100 - dir * 26)} ${f1(Math.min(chin - 6, tipY - 10))} Q${f1(bx - dir * 8)} ${f1(cheekY + 30)} ${f1(bx)} ${f1(cheekY)} Z" fill="${c}" ${st}/>`;
  if (moustache) out += `<path d="M${f1(fx + dir * 4)} ${f1(my - 12)} Q${f1(fx - dir * 20)} ${f1(my - 14)} ${f1(fx - dir * 30)} ${f1(my + 6)} Q${f1(fx - dir * 8)} ${f1(my)} ${f1(fx + dir * 6)} ${f1(my - 2)} Z" fill="${c}" ${st}/>`;
  return out;
}
// (ticket 447: away, facing away from us: a cloth hood is drawn whole over the head instead, clothHoodAway)
function faceSvg(F, ground, parts = false, dir = 0, turn = 0, away = false) {
  const b = F.burst, k = F.candy, r = F.rays, o = F.open;
  const rayLen = r === 0 ? 0 : idiv((15 + idiv(r * 6, 10)) * b, 100);
  const shown = k * b >= 2500 ? 40 : idiv(k * b * 40, 2500);
  const lift = idiv(b * 7, 10), tilt = idiv(b, 4);
  const [upper, lower] = lips(F.smile, o);
  const LY = faceLayout(F);
  // The head, and when it explodes, split along a crack into a lid and
  // the rest: the same outline twice, clipped above and below the crack.
  const pts = headPoints(F);
  const d = "M" + pts.map(([x, y]) => f1(x) + " " + f1(y)).join(" L") + " Z";
  const crackY = HEAD_C - 50.6 * F.faceH / 100;
  const [xl, xr] = acrossAt(pts, crackY);
  const crack = CRACK.replace(/(-?[\d.]+) (-?[\d.]+)/g, (_, x, y) =>
    f1(xl + (Number(x) - 38) * (xr - xl) / 124) + " " + f1(crackY + Number(y) - 60));
  const part = (clip = "") => `<path d="${d}" fill="${F.color}" stroke="black" stroke-width="3"${clip}/>`;
  const clips = `<clipPath id="above-crack"><rect x="-2000" y="-4000" width="4200" height="${f1(4000 + crackY)}"/></clipPath>` +
                `<clipPath id="below-crack"><rect x="-2000" y="${f1(crackY)}" width="4200" height="4000"/></clipPath>`;
  // Eyes slant about their own middles, mirror images of each other:
  // tilt up and the outer corners rise, for almond alien eyes.
  const C = costumeOn(), kind = state.costume.kind;
  const hoodClips = C ? `<clipPath id="hood-brim"><rect x="-200" y="-400" width="600" height="454"/></clipPath>` +
                        `<clipPath id="hood-inside"><ellipse ${hoodEllipse(F)}/></clipPath>` : "";
  const CH = away ? null : clothHood();   // (ticket 433: a cloth hood, up)
  const under = `<defs>${clips}${hoodClips}</defs>` +
    (C ? `<g transform="${topFlies(F)}"><g transform="${hairStretch(F)}">${dir ? `<g transform="translate(${-dir * 16} 0)">${hoodBack(C, kind, F, dir)}</g>` : hoodBack(C, kind, F)}</g></g>` : "") +
    (CH ? `<g transform="${topFlies(F)}"><g transform="${hairStretch(F)}">${clothHoodBack(F, CH, dir)}</g></g>` : "") +
    sideLines(F.sides) +
    (b > 0 ? part(' clip-path="url(#below-crack)"') : part()) +
    raysUp(rayLen, 3 + idiv(r, 25), crackY) +
    (b > 0 ? `<g transform="translate(0 ${-lift}) rotate(${-tilt} 100 ${f1(crackY)})">${part(' clip-path="url(#above-crack)"')}</g>` : "") +
    (C ? `<g transform="${topFlies(F)}"><g transform="${hairStretch(F)}">${dir ? hoodSide(C, kind, F, dir) : hoodFront(C, kind, F)}</g></g>` : "") +
    (CH ? `<g transform="${topFlies(F)}"><g transform="${hairStretch(F)}">${clothHoodFront(F, CH, dir)}</g></g>` : "") +
    pieces(shown, b, crackY) +
    (b > 0 ? `<path d="${crack}" fill="none" stroke="black" stroke-width="2"/>` : "");
  const eyes = `<g transform="translate(0 ${f1(LY.eyeY - 100)})">` +
      eye(100 - LY.eyeDX, F.eyes, F.stars, F.hearts, F.xeyes, F, F.tilt) +
      eye(100 + LY.eyeDX, F.eyes, F.stars, F.hearts, F.xeyes, F, -F.tilt) + `</g>`;
  const over = noseSvg(F) +
    // the brows spread with the eyes (their path's points, so the line
    // keeps its width)
    `<path d="${(F.unibrow ? browPath(F.brow) : browPair(F.brow)).replace(/(-?[\d.]+) (-?[\d.]+)/g, (_, x, y) =>
        f1(100 + (Number(x) - 100) * LY.bx) + " " + f1(Number(y) + LY.eyeY - 100 - browLift(F)))}" ` +
      `stroke="black" fill="none" stroke-width="${F.unibrow ? 7 : 6}" stroke-linecap="round"/>` +
    // the mouth, at its place and grown with the head
    `<g transform="translate(100 ${f1(LY.mouthY)}) scale(${LY.mx.toFixed(3)} ${LY.my.toFixed(3)}) translate(-100 -150)">` +
    mouthSvg(F, o, upper, lower) +
    (dir ? "" : sickPile(F.sick, 150 + idiv(lower - 150, 2), 150 + (ground - LY.mouthY) / LY.my)) + `</g>`;
  // (ticket 131) in profile the mouth's far half is carried to the front
  // (draw), so the pile is drawn on its own: the stream from where the lips
  // meet the face's front edge, the pile whole under it, a side view of it
  const sick = !dir || F.sick <= 0 ? "" :
    `<g transform="translate(100 ${f1(LY.mouthY)}) scale(${LY.mx.toFixed(3)} ${LY.my.toFixed(3)}) translate(-100 -150)">` +
    sickPile(F.sick, 150 + idiv(lower - 150, 2), 150 + (ground - LY.mouthY) / LY.my, 100 + dir * (profileEdge(F) - 4) / LY.mx, 0.4) + `</g>`;
  // (ticket 144) turned partway: the eyes, nose, brows and mouth slid the
  // way they turn, the head's outline as it is
  const slide = t => turn ? `<g transform="translate(${turn * 14} 0)">${t}</g>` : t;
  return parts ? { under, eyes: slide(eyes), over: slide(over), sick } : under + slide(eyes) + slide(over);
}
