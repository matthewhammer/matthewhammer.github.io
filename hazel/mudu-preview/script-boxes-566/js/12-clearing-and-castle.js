"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 12 of 26 of the game's script, run in order by index.html.

function clearingScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o, side } = c;
  const near = g - 40 * k;   // the clearing's near edge, where the paths start
  let s = SEP + part("clear-sky", c.F, () => `<defs>${grad("bg-sky", "#6ec2f2", "#e8f6ff", y0, g - 150 * k)}</defs>` + all("url(#bg-sky)") +
    land(g - 230 * k, 10, "#9ccf86", 0.4) + land(g - 175 * k, 6, "#8bc56a", 1.2));
  s += SEP + cloudsSvg(c, 3, 0.8, 140);
  // the forest round the clearing: far trees all along, near ones at the sides
  // (the far row an object, at its trees' middle depth; its drawings stay
  // outside it, for the near trees use them too)
  s += SEP + part("clear-far-woods", c.F, () => {
    let t = "";
    for (let i = 0; i < 34; i++)
      t += i % 3 === 1 ? use("bg-woods-oak", x0 - 30 * k + (W + 60 * k) * i / 33, g - 218 * k + 8 * k * R(i, 310), k * (0.3 + 0.15 * R(i, 311)))
                       : use("bg-pine", x0 - 30 * k + (W + 60 * k) * i / 33, g - 218 * k + 8 * k * R(i, 310), k * (0.28 + 0.16 * R(i, 311)), k * (0.28 + 0.16 * R(i, 311)),
                             ` color="${["#2f6e3a", "#3a7d44", "#285e33"][i % 3]}"`);
    return `<defs>${oakDef("bg-woods-oak", 1)}${pineDef("bg-pine", 3)}</defs>` + obj("far woods", g - 214 * k, t);
  });
  const shown = CLEARING_THINGS.filter(T => o[T.key] ?? true);
  // the paths, from the near edge out to each thing shown
  s += SEP + part("clear-paths", [c.F, shown.map(T => T.key)], () => {
    let t = land(near, 3, "#7cc25a", 0.6) + `<path d="M${P(-40 * k, y1 + pad)} L${P(40 * k, y1 + pad)} L${P(26 * k, near)} L${P(-26 * k, near)} Z" fill="#d9c08a"/>`;
    for (const T of shown) {
      // (to where it stands on the ground: its door, its gate, its foot)
      const x = W * T.at[0], y = near - T.at[1] * k + (T.foot + 4) * T.size * k;
      t += `<path d="M${P(0, near + 4 * k)} Q${P(x * 0.5, Math.max(y, near - 10 * k))} ${P(x, y)}" fill="none" stroke="#d9c08a" stroke-width="${f1(14 * k)}" stroke-linecap="round"/>`;
    }
    const fl = [];   // (ticket 297: the flowers in rows by depth, the friend among them)
    for (let i = 0; i < 40; i++) {   // flowers in the near grass
      const d = R(i, 312), x = x0 + W * R(i, 313), y = near + 10 * k + (y1 - near) * d;
      if (Math.abs(x) < (40 + 30 * d) * k) continue;
      fl.push({ y, svg: use(i % 2 ? "bg-daisy" : "bg-tulip", x, y, k * (0.8 + 0.6 * d), k * (0.8 + 0.6 * d), ` color="${FLOWER_COLORS[i % 6]}"`) });
    }
    return `<defs>${FLOWER_DEFS}</defs>` + t + depthRows("flowers", fl, 12 * k);
  });
  // the path toward us, flat on the ground, so painted before anything standing (ticket 233: no longer a doorway of
  // its own to the path through the woods; the way toward us is the world's, a nav arrow, where it leads somewhere)
  s += SEP + part("clear-front-path", c.F, () =>
    `<path d="M${P(-40 * k, y1 + pad)} L${P(40 * k, y1 + pad)} L${P(26 * k, near)} L${P(-26 * k, near)} Z" fill="#d9c08a"/>`);
  // the things far off, each its own piece, over the paths' far ends (each
  // an object, standing where its path ends: at its foot, at its size)
  const thingName = { seeCastle: "castle", seeTree: "tree house", seeHouse: "house", seeWitch: "witch hut", seeHealer: "healer hut" };
  for (const T of shown) {
    const x = W * T.at[0], y = near - T.at[1] * k;
    const z = T.size * (T.grow ? T.grow(o) : 1) * k;
    s += SEP + obj(thingName[T.key] || T.key, y + T.foot * z,
      (T.to ? `<g class="portal" data-to="${T.to}">` : "") + `<g ${tf(x, y, z)}>${T.draw(o, c)}</g>` + (T.to ? `</g>` : ""));
  }
  // (ticket 233, Matthew: "remove all of the signs that we added initially just to link in the old, primitive way --
  // we've replaced that with the world designer UX now, and the UX it creates for the gameplay, with nav arrows, and
  // always-correct signs that are optional") no 🥾 path ⬇️ sign toward us, no 🧚 fairies ↗️ sign: the world's ways
  // are the nav arrows, and a sign with directions (🧭, a thing) says where they lead
  // near trees at the frame's edges, framing the clearing (the outer two
  // further back, the inner two nearer)
  s += SEP + part("clear-near-woods", c.F, () => byDepth([-1, 1].flatMap(sd => [[0, 0, 1.15], [1, 40, 0.8]].map(([j, dy, z]) =>
    [`tree, ${sd < 0 ? "left" : "right"}, ${j ? "inner" : "outer"}`, g + (40 + dy) * k,
     use("bg-woods-oak", sd * (W * 0.5 + 40 * k - j * 30 * k), g + (40 + dy) * k, k * z * sd, k * z)]))));
  // butterflies, fluttering with the quick LFO (at the depth of the ground
  // under the nearest)
  s += SEP + obj("butterflies", g + 30 * k, part("clearing-butterflies", [c.F, sceneLfoKey(o, "fast"), sceneLfoKey(o, "wings")], () => butterfliesSvg(c, [[x0 + W * 0.2, g + 10 * k, 1.1], [x0 + W * 0.78, g - 20 * k, 1], [x0 + W * 0.66, g + 30 * k, 0.9]])));
  return s;
}
// A castle up close: the one the clearing shows, from the same settings,
// its gate a doorway in (to its first room), a sign back to the clearing.
// (ticket 50) the crate (40 across, its top 34 above its foot) and two of
// the barrels by the road (each one's lid, 32 of its units across and 44
// up; not the far one, partly behind the near one), as castleScene's
// "castle-things" draws them
SEATS.castle = (c, B) => { const { W, g, k } = c;
  return [{ name: "crate", x0: -W * 0.3, x1: -W * 0.3 + 40 * k, y: g + 50 * k, z: g + 84 * k + 1, facing: ["front", "left", "right"] },
    ...[[W * 0.3, g + 74 * k, 1, "barrel, middle"], [W * 0.235, g + 84 * k, 0.95, "barrel, near"]].map(([bx, by, sc2, name]) =>
      ({ name, x0: bx - 16 * sc2 * k, x1: bx + 16 * sc2 * k, y: by - 44 * sc2 * k, z: by + 1, facing: ["front", "left", "right"] }))]; };
function castleScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o } = c;
  const K = castleOf(o), { foot, z } = castleAt(c, K);
  const bridgeEnd = foot + (K.moat > 0 ? 26 + 24 * K.moat : 8) * z;
  // a warm afternoon: the sky, the sun, clouds, birds far off
  // (ticket 351, Matthew: "very minor birds in top box are not animated, but should they be?") each bird's wings flap, its
  // stroke between wings up and wings down, in time with the scene's wings LFO as the butterflies' (lfoTimed: each its own
  // way into it; at its own pace with none), and it glides slowly back and forth across the sky
  let s = SEP + part("castle-sky", [c.F, sceneLfoKey(o, "fast"), sceneLfoKey(o, "wings")], () => `<defs>${grad("bg-sky", "#6fbaf0", "#fde6c2", y0, g - 160 * k)}</defs>` + all("url(#bg-sky)") +
    sunSvg(x1 - W * 0.13, y0 + H * 0.13, 30 * k, true, o) +
    wallObj("birds", [...Array(6)].map((_, i) => { const bx = x0 + W * (0.12 + 0.13 * i + 0.05 * R(i, 401)), by = y0 + H * (0.1 + 0.12 * R(i, 402)), b = (7 + 5 * R(i, 403)) * k;
      const wing = (tip, mid) => `M${P(bx - b, by + tip * b)} Q${P(bx - b * 0.4, by + mid * b)} ${P(bx, by)} Q${P(bx + b * 0.4, by + mid * b)} ${P(bx + b, by + tip * b)}`;
      const up = wing(-0.4, -0.7), down = wing(0.35, -0.15), glide = (18 + 14 * R(i, 404)) * k * (i % 2 ? -1 : 1);
      const flap = lfoTimed(o, "wings", (i * 0.23) % 1, `<animate attributeName="d" values="${up};${down};${up}" dur="${(0.8 + 0.25 * R(i, 405)).toFixed(2)}s" begin="-${(R(i, 406)).toFixed(2)}s" repeatCount="indefinite"/>`);
      return `<g><animateTransform attributeName="transform" type="translate" values="0 0;${f1(glide)} ${f1(-3 * k)};0 0" dur="${(9 + 5 * R(i, 407)).toFixed(1)}s" begin="-${(3 * R(i, 408)).toFixed(1)}s" repeatCount="indefinite" calcMode="spline" keySplines="0.45 0 0.55 1;0.45 0 0.55 1"/>` +
        `<path d="${up}" fill="none" stroke="#3a3a4a" stroke-width="${f1(2 * k)}" stroke-linecap="round">${flap}</path></g>`; }).join("")));   // (ticket 284: the birds an object)
  s += SEP + cloudsSvg(c, 4, 0.85, 170);
  // far mountains, snow on their tops, and rows of forest before them
  s += SEP + part("castle-far", c.F, () => {
    let t = "";
    for (const [row, col, snow] of [[0, "#9fb4cf", "#f4f7fb"], [1, "#86a0bf", "#e8eef6"]]) {
      // a range: peaks with saddles between them, each cap cut from its own slopes
      const base = g - (row ? 150 : 170) * k, n = 9, sp = (W + 80 * k) / n, pts = [[x0 - pad, base]];
      for (let i = 0; i <= n; i++) {
        const x = x0 - 40 * k + sp * i + 16 * k * (R(i, 412 + row) - 0.5), h = (row ? 150 : 210) * k * (0.6 + 0.5 * R(i, 410 + row));
        pts.push([x - sp * 0.5, base - h * (0.25 + 0.2 * R(i, 414 + row))], [x, base - h]);
      }
      pts.push([x1 + pad, base], [x1 + pad, y1 + pad], [x0 - pad, y1 + pad]);
      t += `<polygon points="${pts.map(([x, y]) => P2(x, y)).join(" ")}" fill="${col}"/>`;
      for (let i = 2; i < pts.length - 3; i += 2) {
        const [px, py] = pts[i], [lx, ly] = pts[i - 1], [rx, ry] = pts[i + 1];
        t += snowCap(px, py, lx, ly, rx, ry, 0.3 * (base - py), i + row * 30, snow, 1);
      }
    }
    t += land(g - 150 * k, 10, "#a9d48e", 0.4);
    // (pines and leafy trees, far ones paler; from the drawings every scene shares)
    t += `<defs>${pineDef("bg-far-pine", 9)}${leafyTreeDef("bg-far-oak", 17)}</defs>`;
    // (each row an object, standing at its trees' middle depth; ticket 261: a tree more than 75% hidden by the row in
    // front and the grass drawn after, left out)
    t += treeRows([[0, g - 140 * k, ["#4f8f5a", "#5c9c63"]], [1, g - 100 * k, ["#2f6b3a", "#3c7d45"]]].map(([row, base, cols]) => {
      const items = [];
      for (let i = 0, x = x0 - 50 * k; x < x1 + 50 * k; i++, x += (row ? 46 : 34) * k) {
        const h = (row ? 120 : 84) * k * (0.8 + 0.4 * R(i, 420 + row)), b = base + 12 * k * R(i, 422 + row), sx = (h / 330) * (R(i, 426) < 0.5 ? -1 : 1);
        items.push(R(i, 424 + row) < 0.7 ? { svg: use("bg-far-pine", x, b, h / 200, h / 200, ` color="${cols[i % 2]}"`), shape: useOutline("bg-far-pine", x, b, h / 200) }
                                         : { svg: `<g opacity="${row ? 1 : 0.85}">${use("bg-far-oak", x, b, sx, h / 330)}</g>`, shape: useOutline("bg-far-oak", x, b, sx, h / 330) });
      }
      return [row ? "trees, near row" : "trees, far row", base + 6 * k, items];
    }), [landCover(g - 80 * k, 6, k, x0 - pad, x1 + pad, y1 + pad)]);
    return t + land(g - 80 * k, 6, "#8bc56a", 1.1) + land(g - 30 * k, 3, "#7cc25a", 0.6);
  });
  // the castle, from the same settings as the clearing's (an object, its
  // walls' foot where it stands: its moat, fence and trees go with it)
  s += SEP + obj("castle", foot, part("castle-near", [c.F, K.id, K.levels, K.stone, K.trees, K.moat, K.flagA, K.flagB, K.flagStyle, K.roof], () => `<g ${tf(0, foot, z)}>${castleSvg(K)}</g>`));
  // the road up to the drawbridge, pennants on poles either side of it, the
  // ruts of carts in it, stones along its edges
  s += SEP + part("castle-road", [c.F, K.id, K.moat, K.levels, K.flagA, K.flagB, K.flagStyle, sceneLfoKey(o, "fast")], () => {
    const rw = 44 * z, bw2 = W * 0.17;
    let t = `<path d="M${P(-rw, bridgeEnd)} L${P(rw, bridgeEnd)} L${P(bw2, y1 + pad)} L${P(-bw2, y1 + pad)} Z" fill="#cdb487"/>` +
      [-1, 1].map(e => `<path d="M${P(e * rw * 0.45, bridgeEnd)} L${P(e * bw2 * 0.45, y1 + pad)}" stroke="#b39a6c" stroke-width="${f1(4 * k)}"/>`).join("");
    // (stones along its edges: at a stone's height y, the edge is v of the way from the bridge to
    // y1 + pad, where the road is bw2 wide, not u of the way)
    for (let i = 0; i < 14; i++) { const u = (i + R(i, 430)) / 14, e = i % 2 ? 1 : -1, y = bridgeEnd + (y1 - bridgeEnd) * u, v = (y - bridgeEnd) / (y1 + pad - bridgeEnd), x = e * (rw + (bw2 - rw) * v) * 1.02;
      t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1((5 + 6 * u) * k)}" ry="${f1((3 + 3 * u) * k)}" fill="${R(i, 431) < 0.5 ? "#9a9488" : "#b0aa9c"}"/>`; }
    t += castleFlagDefs(K, 6 * k);
    // (each pennant an object, standing where its pole meets the ground)
    for (const e of [-1, 1]) {
      const px = e * (rw + 30 * k), py = bridgeEnd + 30 * k, h = 120 * k;
      t += obj(`pennant, ${e < 0 ? "left" : "right"}`, py, `<rect x="${f1(px - 3 * k)}" y="${f1(py - h)}" width="${f1(6 * k)}" height="${f1(h)}" fill="#6e4a2c"/><circle cx="${f1(px)}" cy="${f1(py - h)}" r="${f1(5 * k)}" fill="#e8b923"/>` +
           `<path d="M${P(px + 3 * k, py - h + 6 * k)} q${f1(e * 30 * k)} ${f1(6 * k)} ${f1(e * 56 * k)} ${f1(14 * k)} q${f1(-e * 26 * k)} ${f1(6 * k)} ${f1(-e * 56 * k)} ${f1(16 * k)} Z" fill="${castleFlagFill(K, e < 0 ? 0 : 1, 6 * k)}">` +
           lfoTimed(o, "fast", (e + 1) / 4, `<animateTransform attributeName="transform" type="skewY" values="0;${e * 4};0;${-e * 3};0" dur="3s" repeatCount="indefinite" additive="sum"/>`) + `</path>`);
    }
    return t;
  });
  // a hay cart on the left, a well on the right, barrels and crates by the
  // road; rocks, grass, and flowers all about (each standing thing an
  // object, at the ground under its wheels, its wall, its foot)
  s += SEP + part("castle-things", c.F, () => {
    let t = `<defs>${FLOWER_DEFS}</defs>`;
    const cx = -W * 0.42, cy = g + 40 * k, s2 = 1.15 * k;
    t += obj("hay cart", cy + 4 * s2, `<ellipse cx="${f1(cx)}" cy="${f1(cy + 4 * s2)}" rx="${f1(70 * s2)}" ry="${f1(8 * s2)}" fill="#000000" opacity="0.15"/>` +
         `<path d="M${P(cx - 60 * s2, cy - 50 * s2)} Q${P(cx - 40 * s2, cy - 110 * s2)} ${P(cx, cy - 100 * s2)} Q${P(cx + 40 * s2, cy - 112 * s2)} ${P(cx + 60 * s2, cy - 50 * s2)} Z" fill="#e8c35a"/>` +
         // (its straws, kept on the heap)
         `<clipPath id="bg-hayheap"><path d="M${P(cx - 60 * s2, cy - 50 * s2)} Q${P(cx - 40 * s2, cy - 110 * s2)} ${P(cx, cy - 100 * s2)} Q${P(cx + 40 * s2, cy - 112 * s2)} ${P(cx + 60 * s2, cy - 50 * s2)} Z"/></clipPath><g clip-path="url(#bg-hayheap)">` +
         [...Array(14)].map((_, i) => `<line x1="${f1(cx - 50 * s2 + 100 * s2 * R(i, 440))}" y1="${f1(cy - 60 * s2 - 40 * s2 * R(i, 441))}" x2="${f1(cx - 50 * s2 + 100 * s2 * R(i, 440) + 10 * s2)}" y2="${f1(cy - 70 * s2 - 40 * s2 * R(i, 441))}" stroke="#c9a23e" stroke-width="${f1(2 * s2)}"/>`).join("") + `</g>` +
         `<rect x="${f1(cx - 64 * s2)}" y="${f1(cy - 52 * s2)}" width="${f1(128 * s2)}" height="${f1(30 * s2)}" fill="#8b5e34" stroke="#5e3d22" stroke-width="${f1(2 * s2)}"/>` +
         [0.33, 0.66].map(u => `<line x1="${f1(cx - 64 * s2 + 128 * s2 * u)}" y1="${f1(cy - 52 * s2)}" x2="${f1(cx - 64 * s2 + 128 * s2 * u)}" y2="${f1(cy - 22 * s2)}" stroke="#5e3d22" stroke-width="${f1(2 * s2)}"/>`).join("") +
         `<line x1="${f1(cx + 64 * s2)}" y1="${f1(cy - 30 * s2)}" x2="${f1(cx + 110 * s2)}" y2="${f1(cy - 10 * s2)}" stroke="#6e4a2c" stroke-width="${f1(5 * s2)}"/>` +
         [-1, 1].map(e => `<circle cx="${f1(cx + e * 38 * s2)}" cy="${f1(cy - 14 * s2)}" r="${f1(18 * s2)}" fill="none" stroke="#5e3d22" stroke-width="${f1(5 * s2)}"/>` +
           [0, 1, 2, 3].map(q => `<line x1="${f1(cx + e * 38 * s2)}" y1="${f1(cy - 14 * s2)}" x2="${f1(cx + e * 38 * s2 + Math.cos(q * Math.PI / 4) * 18 * s2)}" y2="${f1(cy - 14 * s2 + Math.sin(q * Math.PI / 4) * 18 * s2)}" stroke="#6e4a2c" stroke-width="${f1(2.5 * s2)}"/>` +
             `<line x1="${f1(cx + e * 38 * s2)}" y1="${f1(cy - 14 * s2)}" x2="${f1(cx + e * 38 * s2 - Math.cos(q * Math.PI / 4) * 18 * s2)}" y2="${f1(cy - 14 * s2 - Math.sin(q * Math.PI / 4) * 18 * s2)}" stroke="#6e4a2c" stroke-width="${f1(2.5 * s2)}"/>`).join("")).join(""));
    const wx = W * 0.35, wy = g + 16 * k, w2 = 1.1 * k;
    t += obj("well", wy + 4 * w2, `<ellipse cx="${f1(wx)}" cy="${f1(wy + 4 * w2)}" rx="${f1(50 * w2)}" ry="${f1(8 * w2)}" fill="#000000" opacity="0.15"/>` +
         `<path d="M${P(wx - 40 * w2, wy - 50 * w2)} V${f1(wy)} Q${P(wx, wy + 14 * w2)} ${P(wx + 40 * w2, wy)} V${f1(wy - 50 * w2)} Z" fill="#a8a29a"/>` +
         // (its stones, kept on the well)
         `<clipPath id="bg-wellwall"><path d="M${P(wx - 40 * w2, wy - 50 * w2)} V${f1(wy)} Q${P(wx, wy + 14 * w2)} ${P(wx + 40 * w2, wy)} V${f1(wy - 50 * w2)} Z"/></clipPath><g clip-path="url(#bg-wellwall)">` +
         [...Array(3)].map((_, j) => [...Array(4)].map((_, i) => `<rect x="${f1(wx - 40 * w2 + i * 20 * w2 + (j % 2) * 10 * w2)}" y="${f1(wy - 48 * w2 + j * 16 * w2)}" width="${f1(18 * w2)}" height="${f1(14 * w2)}" rx="${f1(2 * w2)}" fill="${R(i + j * 4, 450) < 0.5 ? "#b8b2a8" : "#9e988e"}"/>`).join("")).join("") + `</g>` +
         `<ellipse cx="${f1(wx)}" cy="${f1(wy - 50 * w2)}" rx="${f1(40 * w2)}" ry="${f1(10 * w2)}" fill="#3a3a44"/>` +
         [-1, 1].map(e => `<rect x="${f1(wx + e * 34 * w2 - 3 * w2)}" y="${f1(wy - 120 * w2)}" width="${f1(6 * w2)}" height="${f1(70 * w2)}" fill="#6e4a2c"/>`).join("") +
         `<polygon points="${P(wx - 54 * w2, wy - 112 * w2)} ${P(wx, wy - 150 * w2)} ${P(wx + 54 * w2, wy - 112 * w2)}" fill="#a3122a"/><line x1="${f1(wx - 34 * w2)}" y1="${f1(wy - 100 * w2)}" x2="${f1(wx + 34 * w2)}" y2="${f1(wy - 100 * w2)}" stroke="#5e3d22" stroke-width="${f1(4 * w2)}"/>` +
         `<line x1="${f1(wx)}" y1="${f1(wy - 100 * w2)}" x2="${f1(wx)}" y2="${f1(wy - 74 * w2)}" stroke="#8a8a8a" stroke-width="${f1(1.5 * w2)}"/><path d="M${P(wx - 9 * w2, wy - 74 * w2)} h${f1(18 * w2)} l${f1(-3 * w2)} ${f1(16 * w2)} h${f1(-12 * w2)} Z" fill="#8b5e34"/>`);
    for (const [bx, by, sc2, name] of [[W * 0.26, g + 66 * k, 1.1, "barrel, far"], [W * 0.3, g + 74 * k, 1, "barrel, middle"], [W * 0.235, g + 84 * k, 0.95, "barrel, near"]]) {
      const b = sc2 * k;
      t += obj(name, by, `<path d="M${P(bx - 16 * b, by)} Q${P(bx - 20 * b, by - 22 * b)} ${P(bx - 16 * b, by - 44 * b)} H${f1(bx + 16 * b)} Q${P(bx + 20 * b, by - 22 * b)} ${P(bx + 16 * b, by)} Z" fill="#8b5e34" stroke="#5e3d22" stroke-width="${f1(1.5 * b)}"/>` +
           [-34, -10].map(dy => `<path d="M${P(bx - 18 * b, by + dy * b)} Q${P(bx, by + (dy + 3) * b)} ${P(bx + 18 * b, by + dy * b)}" fill="none" stroke="#4a4a52" stroke-width="${f1(3 * b)}"/>`).join("") +
           `<ellipse cx="${f1(bx)}" cy="${f1(by - 44 * b)}" rx="${f1(16 * b)}" ry="${f1(4 * b)}" fill="#6e4a2c"/>`);
    }
    t += obj("crate", g + 84 * k, `<rect x="${f1(-W * 0.3)}" y="${f1(g + 50 * k)}" width="${f1(40 * k)}" height="${f1(34 * k)}" fill="#a0703f" stroke="#5e3d22" stroke-width="${f1(2 * k)}"/><path d="M${P(-W * 0.3, g + 50 * k)} l${f1(40 * k)} ${f1(34 * k)} M${P(-W * 0.3 + 40 * k, g + 50 * k)} l${f1(-40 * k)} ${f1(34 * k)}" stroke="#5e3d22" stroke-width="${f1(2 * k)}"/>`);
    const fl = [];   // (ticket 297: the flowers and grass in rows by depth, the friend among them)
    for (let i = 0; i < 46; i++) {   // flowers and grass in the near meadow, not on the road
      const u = R(i, 460), x = x0 + W * R(i, 461), y = g + 20 * k + (y1 - g) * u;
      if (Math.abs(x) < W * 0.07 + W * 0.12 * u + 20 * k) continue;
      fl.push({ y, svg: i % 3 === 2 ? grassTuft(x, y, (12 + 10 * u) * k, 470 + i, "#4f9a3a", 1.8 * k)
        : use(i % 2 ? "bg-daisy" : "bg-tulip", x, y, k * (0.7 + 0.6 * u), k * (0.7 + 0.6 * u), ` color="${FLOWER_COLORS[i % 6]}"`) });
    }
    t += depthRows("flowers and grass", fl, 12 * k);
    for (let i = 0; i < 6; i++) { const x = x0 + W * (0.05 + 0.18 * i), y = g + (60 + 50 * R(i, 470)) * k, rr = (10 + 8 * R(i, 471)) * k;
      if (Math.abs(x) < W * 0.2) continue;
      t += obj(`rock, ${x < 0 ? "left" : "right"}, ${Math.abs(x) > W * 0.36 ? "outer" : "inner"}`, y, rockSvg(x, y, rr * 1.2, 480 + i)); continue;
      t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(rr * 1.4)}" ry="${f1(rr)}" fill="#9a9488"/><ellipse cx="${f1(x - rr * 0.3)}" cy="${f1(y - rr * 0.3)}" rx="${f1(rr * 0.6)}" ry="${f1(rr * 0.35)}" fill="#b8b2a8"/>`; }
    return t;
  });
  // the gate, a doorway into the castle's first room, and the signs (the
  // gate's doorway, its unseen clickable arch and its sign, one object, at
  // the castle's own foot: its arch is in the castle's wall, and must stay
  // under a friend standing before the castle)
  s += SEP + obj("gate", foot, part("castle-gate", [c.F, K.levels, K.moat], () => `<g class="portal" data-door="" data-to="castleroom" data-room="1"><g ${tf(0, foot, z)}>` +
    `<path d="M-34 0 V-56 A34 34 0 0 1 34 -56 V0 Z" fill="#000000" opacity="0.01"/></g>` +
    `<rect x="${f1(W * 0.13 - 4 * k)}" y="${f1(bridgeEnd - 6 * k)}" width="${f1(8 * k)}" height="${f1(70 * k)}" fill="#6e4a2c"/>` +
    plankSign(W * 0.13, bridgeEnd - 10 * k, k, "🚪 inside") + `</g>`));
  // (ticket 233: no 🌲 clearing sign back toward us; the world's ways are the nav arrows)
  // big trees at the frame's edges, framing the castle (the outer two
  // further back, the inner two nearer)
  s += SEP + part("castle-near-trees", c.F, () => `<defs>${oakDef("bg-castle-oak", 1)}</defs>` + byDepth([-1, 1].flatMap(sd => [[0, 0, 1.2], [1, 50, 0.85]].map(([j, dy, zz]) =>
    [`big tree, ${sd < 0 ? "left" : "right"}, ${j ? "inner" : "outer"}`, g + (60 + dy) * k,
     use("bg-castle-oak", sd * (W * 0.5 + 50 * k - j * 40 * k), g + (60 + dy) * k, k * zz * sd, k * zz)]))));
  return s;
}
// Inside a castle: a stone room in perspective, its ceiling vaulted, torches
// on the walls, and the things its style keeps. Rooms: the castle's own list
// (up to ROOM_MAX), each in a style; a trap room's two pits drop into its
// dungeons, and a dungeon's ladder climbs back to the trap room.
const ROOM_TONE = Object.fromEntries(Object.entries(ROOM_KINDS).filter(([, R]) => R.tone).map(([st, R]) => [st, R.tone]));
