"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 14 of 26 of the game's script, run in order by index.html.

function mountainsScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o, side } = c;
  const peaks = (o.peakHeight ?? 100) / 100, birds = o.birds ?? 3, clouds = o.mtClouds ?? 5, lake = o.farLake ?? true;
  const valley = g - 150 * k;
  let s = SEP + part("mt-sky", c.F, () => `<defs>${grad("bg-sky", "#4f9fe0", "#e2f2ff", y0, valley)}${cloudDef("bg-cloud", 1)}</defs>` + all("url(#bg-sky)") +
    `<circle cx="${f1(x1 - W * 0.12)}" cy="${f1(y0 + H * 0.12)}" r="${f1(30 * k)}" fill="#fff6c8"/>`);
  // the high clouds, behind the peaks
  s += SEP + part("mt-clouds-high", [c.F, clouds, sceneLfoKey(o, "slow")], () => [...Array(Math.ceil(clouds / 2))].map((_, i) =>
    driftingCloud(x0 + W * (0.1 + 0.8 * R(i, 320)), y0 + H * (0.06 + 0.12 * R(i, 321)), k * (0.6 + 0.4 * R(i, 322)), i, o)).join(""));
  // the peaks, three ranges, far to near, snow on the far ones
  s += OPEN(`<g transform="translate(0 ${f1(valley)}) scale(1 ${peaks.toFixed(3)}) translate(0 ${f1(-valley)})">`) + part("mt-peaks", c.F, () => {
    let t = "";
    [[0, "#b6c6dc", 330, 9, true], [1, "#8ea3c0", 250, 7, true], [2, "#6d8296", 170, 6, false]].forEach(([r, fill, hgt, n, snow]) => {
      const base = valley + r * 12 * k, pts = [[x0 - pad, base]];
      for (let i = 0; i <= n; i++) {
        const x = x0 - 60 * k + (W + 120 * k) * i / n + 30 * k * (R(i + r * 10, 323) - 0.5);
        pts.push([x - (W / n) * 0.45, base - hgt * k * (0.35 + 0.3 * R(i + r * 10, 324))], [x, base - hgt * k * (0.75 + 0.25 * R(i + r * 10, 325))]);
      }
      pts.push([x1 + pad, base], [x1 + pad, base + 60 * k], [x0 - pad, base + 60 * k]);
      t += `<polygon points="${pts.map(([x, y]) => P2(x, y)).join(" ")}" fill="${fill}"/>`;
      if (snow) for (let i = 2; i < pts.length - 3; i += 2) {
        const [px, py] = pts[i], [lx, ly] = pts[i - 1], [rx, ry] = pts[i + 1];
        t += snowCap(px, py, lx, ly, rx, ry, 0.24 * (base - py), i + r * 20);
      }
    });
    return t;
  }) + CLOSE;
  // far below: a lake and a village by it (or not)
  if (lake) s += SEP + part("mt-lake", c.F, () => wallObj("far lake and village", `<ellipse cx="${f1(W * 0.18)}" cy="${f1(valley + 22 * k)}" rx="${f1(120 * k)}" ry="${f1(14 * k)}" fill="#5aa9e0"/>` +
    `<ellipse cx="${f1(W * 0.18)}" cy="${f1(valley + 20 * k)}" rx="${f1(90 * k)}" ry="${f1(6 * k)}" fill="#bfe3f5" opacity="0.6"/>` +
    [...Array(7)].map((_, i) => { const x = W * 0.18 - 150 * k + i * 16 * k, y = valley + 18 * k + 6 * k * R(i, 326);
      return `<rect x="${f1(x)}" y="${f1(y - 7 * k)}" width="${f1(10 * k)}" height="${f1(7 * k)}" fill="#f4efe6"/><polygon points="${P2(x - 1 * k, y - 7 * k)} ${P2(x + 5 * k, y - 12 * k)} ${P2(x + 11 * k, y - 7 * k)}" fill="${["#c0392b", "#8a5a32", "#3b5ea8"][i % 3]}"/>`; }).join("")));   // (ticket 284: the far lake and village a wall object)
  // the low clouds, in front of the peaks
  s += SEP + part("mt-clouds-low", [c.F, clouds, sceneLfoKey(o, "slow")], () => [...Array(Math.floor(clouds / 2))].map((_, i) =>
    driftingCloud(x0 + W * (0.05 + 0.9 * R(i, 327)), valley - (40 + 90 * R(i, 328)) * k, k * (0.8 + 0.5 * R(i, 329)), i + 2, o)).join(""));
  // the near slope: grass, rocks, the trail winding off uphill
  s += SEP + part("mt-slope", c.F, () => {
    let t = `<defs>${PINE_DEF}</defs>` + land(valley + 40 * k, 14, "#8fb35a", 0.8) + land(g - 30 * k, 6, "#7aa34e", 1.6);
    const L = [], Rr = [];
    for (let i = 0; i <= 20; i++) {
      const u = i / 20, y = y1 + 10 * k - (y1 - valley - 50 * k) * u, x = W * 0.05 * Math.sin(u * 5) - W * 0.25 * u * u, w = (90 - 80 * u) * k;
      L.push(P2(x - w / 2, y)); Rr.push(P2(x + w / 2, y));
    }
    t += `<polygon points="${[...L, ...Rr.reverse()].join(" ")}" fill="#c9a774"/>`;
    // (each rock an object, with the tuft of grass at its foot; each pine one)
    const sideName = i => `${side(i) < 0 ? "left" : "right"} ${Math.floor(i / 2) + 1}`;
    for (let i = 0; i < 9; i++) {
      const x = side(i) * (W * 0.18 + W * 0.28 * R(i, 330)), y = g - 10 * k + 70 * k * R(i, 331), r = (12 + 20 * R(i, 332)) * k;
      t += obj(`rock, ${sideName(i)}`, y, rockSvg(x, y, r, 330 + i, ["#8a8a8a", "#9c9c9c", "#7a7a7a"][i % 3]) + grassTuft(x + r * 1.3, y + 2 * k, 14 * k, 340 + i, "#6f9a48", 1.8 * k));
    }
    for (let i = 0; i < 8; i++) t += obj(`pine, ${sideName(i)}`, g + 30 * k * R(i, 334), use("bg-pine", side(i) * (W * 0.36 + W * 0.14 * R(i, 333)), g + 30 * k * R(i, 334), k * (0.9 + 0.5 * R(i, 335)), k * (0.9 + 0.5 * R(i, 335)),
      ` color="${["#2f6e3a", "#3a7d44", "#285e33"][i % 3]}"`));
    return t;
  });
  // (ticket 233: its old signpost, fixed links to places, is gone; the world's ways are the nav arrows, and a sign with directions, 🧭, a thing, says where they lead)
  // birds, circling high up (an object at the depth of the ground under
  // them, the valley far off: behind anywhere one stands on the slope)
  if (birds > 0) s += SEP + obj("birds", valley, part("mt-birds", [c.F, birds, sceneLfoKey(o, "fast")], () => [...Array(birds)].map((_, i) => {
    const cx = x0 + W * (0.2 + 0.6 * R(i, 336)), cy = y0 + H * (0.15 + 0.15 * R(i, 337)), rx = (50 + 40 * R(i, 338)) * k, ry = rx * 0.35;
    // (their wings beating with the quick LFO, each its own way in)
    const wingD = v => `M${f1(-12 * k)} ${f1(-5 * k * v)} Q${f1(-6 * k)} ${f1(-7 * k)} 0 0 Q${f1(6 * k)} ${f1(-7 * k)} ${f1(12 * k)} ${f1(-5 * k * v)}`;
    return `<path d="${wingD(0)}" fill="none" stroke="#333" stroke-width="${f1(2.5 * k)}">` +
           sceneLfo(o, "fast", R(i, 339), "d", v => wingD(v), `<animate attributeName="d" values="${wingD(-1)};${wingD(1)};${wingD(-1)}" dur="${(0.5 + 0.2 * R(i, 338)).toFixed(2)}s" repeatCount="indefinite"/>`) +
           `<animateMotion path="M${P(cx - rx, cy)} A${f1(rx)} ${f1(ry)} 0 1 1 ${P(cx + rx, cy)} A${f1(rx)} ${f1(ry)} 0 1 1 ${P(cx - rx, cy)}" dur="${(9 + 5 * R(i, 339)).toFixed(1)}s" repeatCount="indefinite"/></path>`;
  }).join("")));
  return s;
}

// (ticket 501, for M: "dessert -- sand dunes in distance-- placeable objects are 6 types of dessert plants (cactus,etc)";
// Matthew passing on M's ideas, the desert he means) The 🏜️ desert: an open sky, the sun high in it and a cloud or two; far
// off, what its 🏜️ setting says (red mesas to start with: farLayers, which any place can call); sand dunes rolling into the
// distance, three rows, lighter and bluer the farther they are, each dune gentle on its windward side and steep on its lee,
// its windward side in shade; and a sand floor to walk on, rippled, a lizard's tracks across it. The desert plants stand in
// it, each an object at its depth, drawn by the things' own drawings (saguaroSvg and the rest); two rocks; and a lizard on
// a flat stone doing push-ups to the beat, a tumbleweed rolling past and bouncing on the beat (the quick LFO), a hawk
// circling high up and sand blowing off the near dunes' crests (the slow one).
const DESERT_SAND = "#e6bc7a", DESERT_HAZE = "#c3cde3";
// (its horizon, the sand floor's far edge (the back line, SCENE_FAR's, just in front of it), and its dunes, each row's
// troughs and crests in turn: for their drawing and for the sand blowing off their crests, alike)
function desertGeom(c) {
  const { x0, W, g, k, R } = c, hz = g - 232 * k, back = g - 128 * k;
  const rows = [[hz + 6 * k, 26, 7, 0.6, 5010], [hz + 42 * k, 34, 5, 0.38, 5030], [back + 4 * k, 44, 4, 0.16, 5050]].map(([base, amp, n, haze, seed]) => {
    const step = (W + 120 * k) / n, xs = [...Array(n + 1)].map((_, i) => x0 - 60 * k + step * (i + (i && i < n ? 0.3 * (R(i, seed) - 0.5) : 0)));
    const ys = xs.map((_, i) => base - amp * k * 0.12 * R(i, seed + 1));
    const dunes = xs.slice(0, n).map((xa, i) => { const xb = xs[i + 1], xc = xa + (xb - xa) * (0.58 + 0.18 * R(i, seed + 2));
      return { xa, ya: ys[i], xc, top: base - amp * k * (0.55 + 0.45 * R(i, seed + 3)), xb, yb: ys[i + 1] }; });
    const fill = mix(DESERT_SAND, DESERT_HAZE, haze);
    return { base, dunes, fill, shade: mix(fill, "#6f4f7a", 0.5), lit: mix(fill, "#ffffff", 0.5) };
  });
  return { hz, back, rows };
}
function desertScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o } = c;
  const { hz, back, rows } = desertGeom(c), far = SHORE_MOUNTAINS.some(([v]) => v === o.desertFar) ? o.desertFar : "mesas";
  // (a size as the friend's is there, with 🔭 as it starts: one of the stage's units in this frame (a picture's are smaller),
  // times how near; so a plant stands beside the friend in proportion, and beside one put there as a thing)
  const u = k * 700 / (SCENE.bottom - SCENE.top), near = y => Math.max(0.1, (y - g + 140 * k) / (175 * k));
  // the sky, and the sun high in it, glowing
  let s = SEP + part("desert-sky", c.F, () => { const sx = x1 - W * 0.17, sy = y0 + H * 0.14;
    return `<defs>${grad("bg-desert-sky", "#3d86d1", "#f4e3c3", y0, hz)}<radialGradient id="bg-desert-sun"><stop offset="0" stop-color="#fff6cf" stop-opacity="0.9"/>` +
      `<stop offset="1" stop-color="#fff6cf" stop-opacity="0"/></radialGradient>${cloudDef("bg-cloud", 0.8)}</defs>` + all("url(#bg-desert-sky)") +
      `<circle cx="${f1(sx)}" cy="${f1(sy)}" r="${f1(95 * k)}" fill="url(#bg-desert-sun)"/>` + sunSvg(sx, sy, 28 * k); });
  // two small clouds, drifting (the slow LFO)
  s += SEP + part("desert-clouds", [c.F, sceneLfoKey(o, "slow")], () => [0, 1].map(i =>
    driftingCloud(x0 + W * (0.12 + 0.42 * i), y0 + H * (0.08 + 0.07 * i), k * (0.42 + 0.12 * i), i + 3, o)).join(""));
  // far off: 🏜️'s choice, on the horizon
  s += SEP + part("desert-far", [c.F, far], () => farLayers(c, hz, { mountains: far, size: 150 }));
  // the dunes, far to near: each row's sand down past the picture's foot (the rows nearer, and the floor, cover it), each
  // dune's windward side in shade (fading toward its foot), the lit edge of its crest; and, far off on the middle row, a few
  // saguaros' outlines (scenery: none is a place to go)
  const duneRow = ({ base, dunes, fill, shade, lit }, id) => {
    let d = `M${P(x0 - pad, base)} L${P(dunes[0].xa, dunes[0].ya)}`, sh = "", edge = "";
    for (const { xa, ya, xc, top, xb, yb } of dunes) {
      const w = [xa + (xc - xa) * 0.6, top], l = [xc + (xb - xc) * 0.12, yb];
      d += ` Q${P(...w)} ${P(xc, top)} Q${P(...l)} ${P(xb, yb)}`;
      sh += `M${P(xa, ya)} Q${P(...w)} ${P(xc, top)} Q${P(xc - (xc - xa) * 0.05, top + (base - top) * 0.6)} ${P(xc - (xc - xa) * 0.35, base)} L${P(xa, base)} Z `;
      // (the lit edge: the lee side's first third, a curve of its own split off the lee's)
      const q = 0.35, m1 = [xc + (l[0] - xc) * q, top + (l[1] - top) * q], e = [(1 - q) ** 2 * xc + 2 * (1 - q) * q * l[0] + q * q * xb, (1 - q) ** 2 * top + 2 * (1 - q) * q * l[1] + q * q * yb];
      edge += `M${P(xc, top)} Q${P(...m1)} ${P(...e)} `;
    }
    const top = Math.min(...dunes.map(q => q.top));
    return `<defs><linearGradient id="${id}" gradientUnits="userSpaceOnUse" x1="0" y1="${f1(top)}" x2="0" y2="${f1(base)}"><stop offset="0" stop-color="${shade}" stop-opacity="0.55"/>` +
      `<stop offset="1" stop-color="${shade}" stop-opacity="0"/></linearGradient></defs>` +
      `<path d="${d} L${P(x1 + pad, base)} L${P(x1 + pad, y1 + pad)} L${P(x0 - pad, y1 + pad)} Z" fill="${fill}"/><path d="${sh}" fill="url(#${id})"/>` +
      `<path d="${edge}" fill="none" stroke="${lit}" stroke-width="${f1(2 * k)}" stroke-linecap="round" opacity="0.8"/>`;
  };
  s += SEP + part("desert-dunes", c.F, () => {
    let cacti = "";
    rows[1].dunes.forEach(({ xa, ya, xc, top }, i) => { if (i % 2 || xc < x0 || xc > x1) return;
      const q = 0.7, wx = xa + (xc - xa) * 0.6, x = (1 - q) ** 2 * xa + 2 * (1 - q) * q * wx + q * q * xc, y = (1 - q) ** 2 * ya + 2 * (1 - q) * q * top + q * q * top + 3 * k, h = (16 + 8 * R(i, 5070)) * k;
      cacti += `M${P(x, y)} V${f1(y - h)} M${P(x, y - h * 0.42)} H${f1(x - h * 0.2)} V${f1(y - h * 0.7)} M${P(x, y - h * 0.5)} H${f1(x + h * 0.18)} V${f1(y - h * 0.8)} `; });
    return wallObj("far dunes", duneRow(rows[0], "bg-desert-dunes0") + duneRow(rows[1], "bg-desert-dunes1") +
      (cacti ? `<path d="${cacti}" fill="none" stroke="${mix("#4f8a46", DESERT_HAZE, 0.45)}" stroke-width="${f1(3.2 * k)}" stroke-linecap="round" stroke-linejoin="round"/>` : "")) +
      wallObj("near dunes", duneRow(rows[2], "bg-desert-dunes2"));
  });
  // sand blowing off the near dunes' crests, to the right on the wind (the slow LFO, each its own way in)
  s += SEP + part("desert-sand-blowing", [c.F, sceneLfoKey(o, "slow")], () => wallObj("sand blowing off the dunes",
    rows[2].dunes.filter(q => q.xc > x0 + 20 * k && q.xc < x1 - 60 * k).slice(0, 3).map(({ xc, top }, j) => {
      const L = 70 * k, lift = 6 * k;
      return `<path d="M${P(xc, top)} q${f1(L * 0.4)} ${f1(-lift)} ${f1(L)} ${f1(-lift * 0.4)} q${f1(-L * 0.5)} ${f1(lift * 0.9)} ${f1(-L)} ${f1(lift * 0.6)} Z" fill="#f8e6bf" opacity="0">` +
        lfoTimed(o, "slow", 0.2 + 0.31 * j, `<animate attributeName="opacity" values="0;0.7;0" dur="6s" repeatCount="indefinite"/>`) +
        lfoTimed(o, "slow", 0.2 + 0.31 * j, `<animateTransform attributeName="transform" type="translate" values="0 0;${f1(34 * k)} ${f1(-6 * k)}" dur="6s" repeatCount="indefinite"/>`) + `</path>`;
    }).join("")));
  // the sand floor, warmer toward us; ripples on it, smaller farther off; a lizard's tracks, from the right edge to its stone,
  // and the line its tail drew between them
  s += SEP + part("desert-floor", c.F, () => {
    let rip = "", tr = "", tail = "";
    for (let i = 0; i < 16; i++) { const y = back + 10 * k + (y1 - back) * Math.pow(R(i, 5060), 0.9), x = x0 + W * R(i, 5061), z = near(y), w = (40 + 50 * R(i, 5062)) * k * z;
      rip += `M${P(x - w, y)} q${f1(w * 0.5)} ${f1(-3 * k * z)} ${f1(w)} 0 t${f1(w)} 0 `; }
    const xa = W * 0.18 + 36 * k, ya = g + 86 * k, xb = x1 + 30 * k, yb = g + 34 * k;
    for (let i = 0; i <= 16; i++) { const q = i / 16, x = xa + (xb - xa) * q, y = ya + (yb - ya) * q + 5 * k * Math.sin(q * 9), z = near(y) * k, py = y + (i % 2 ? 5 : -5) * z;
      tr += `M${P(x, py)} l${f1(-3.4 * z)} 0 M${P(x, py)} l${f1(-2.8 * z)} ${f1(-2 * z)} M${P(x, py)} l${f1(-2.8 * z)} ${f1(2 * z)} `;
      tail += (i ? " L" : "M") + P(x + 4 * z, y); }
    return `<defs>${grad("bg-desert-floor", "#ecc98e", "#e0a85f", back, y1)}</defs>` + land(back, 5, "url(#bg-desert-floor)", 0.7) +
      floorObj("sand ripples", `<path d="${rip}" fill="none" stroke="#c99a5c" stroke-width="${f1(1.6 * k)}" stroke-linecap="round" opacity="0.55"/>`) +
      floorObj("tracks in the sand", `<path d="${tail}" fill="none" stroke="#b98a50" stroke-width="${f1(1 * k)}" opacity="0.35"/>` +
        `<path d="${tr}" fill="none" stroke="#b98a50" stroke-width="${f1(1.2 * k)}" stroke-linecap="round" opacity="0.7"/>`);
  });
  // a hawk circling high up, tilting as it turns (the slow LFO): its spot on the outer group and its loop from there, so held
  // still it stays in the sky (ticket 370's lesson); at the depth of the far ground under it
  s += SEP + part("desert-hawk", [c.F, sceneLfoKey(o, "slow")], () => { const rx = 110 * k, ry = 28 * k;
    return obj("hawk", hz, `<g transform="translate(${f1(-0.3 * W)} ${f1(y0 + H * 0.2)})"><g>` +
      lfoTimed(o, "slow", 0.15, `<animateMotion path="M0 0 A${f1(rx)} ${f1(ry)} 0 1 1 ${f1(2 * rx)} 0 A${f1(rx)} ${f1(ry)} 0 1 1 0 0" dur="16s" repeatCount="indefinite"/>`) +
      `<g transform="scale(${(1.3 * k).toFixed(3)})">${swaying(o, hawkSvg(), 0, 0, 8, 0.4, 6)}</g></g></g>`); });
  // the plants, each standing at its depth, sized for it (the Joshua tree and the saguaros as big as trees): [its name, its
  // place across (a share of the picture's width), its depth (from the ground's line), its drawing and its settings, its
  // seed, how big, mirrored]; the middle kept clear for the friend
  s += SEP + part("desert-plants", c.F, () => byDepth([
    ["saguaro, far", -0.12, -84, saguaroSvg, { arms: 2, flowers: false }, 5102, 1.2, true],
    ["joshua tree", 0.33, -72, joshuaTreeSvg, { flowers: false }, 5101, 1.5, false],
    ["agave", -0.2, -44, agaveSvg, { stalk: true, tone: AGAVE_TONES[0] }, 5104, 1, false],
    ["ocotillo", 0.17, -30, ocotilloSvg, { leaves: true, flowers: true }, 5103, 1.1, false],
    ["prickly pear", 0.38, 54, pricklyPearSvg, { bloom: "fruit" }, 5105, 1, true],
    ["saguaro, near", -0.43, 60, saguaroSvg, { arms: 3, flowers: true }, 5106, 0.85, false],
    ["barrel cactus", -0.24, 90, barrelCactusSvg, { flowers: true, color: DESERT_FLOWERS[3] }, 5107, 0.9, false],
  ].map(([name, ux, dy, draw, t, seed, big, flip]) => { const y = g + dy * k, z = u * near(y) * big;
    return [name, y, `<g transform="translate(${f1(W * ux)} ${f1(y)}) scale(${(flip ? -z : z).toFixed(3)} ${z.toFixed(3)})">${draw(t, seed)}</g>`]; }).concat(
    // (the rocks, as the mountain trail's: each one a seat)
    [["rock, right", 0.1, -60, 5110], ["rock, back", 0.02, -104, 5111]].map(([name, ux, dy, seed]) => { const y = g + dy * k;
      return [name, y, rockSvg(W * ux, y, 36 * u * near(y), seed, "#b98a68")]; }))));
  // the lizard, on a flat stone (no seat: it is the lizard's), facing the middle, doing push-ups to the beat (the quick LFO)
  s += SEP + part("desert-lizard", [c.F, sceneLfoKey(o, "fast")], () => { const y = g + 86 * k, z = u * near(y) * 1.1;
    return obj("lizard", y, `<g transform="translate(${f1(W * 0.18)} ${f1(y)}) scale(${(-z).toFixed(3)} ${z.toFixed(3)})">${lizardSvg(o, 0.3)}</g>`); });
  // a tumbleweed rolling past now and then, in front of the friend where they arrive, bouncing on the beat (the quick LFO);
  // held still (a slow computer), it rests on the sand where it is placed
  s += SEP + part("desert-tumbleweed", [c.F, sceneLfoKey(o, "fast")], () => { const y = g + 64 * k, sx = -0.33 * W, z = u * near(y), r = 30 * z;
    const from = x0 - 90 * k - sx, to = x1 + 90 * k - sx;
    return obj("tumbleweed", y, `<g transform="translate(${f1(sx)} ${f1(y)})"><g><animateTransform attributeName="transform" type="translate" values="${f1(from)} 0;${f1(to)} 0;${f1(to)} 0" keyTimes="0;0.7;1" dur="15s" repeatCount="indefinite"/>` +
      `<ellipse cx="0" cy="2" rx="${f1(r * 0.9)}" ry="${f1(r * 0.18)}" fill="#000000" opacity="0.15"/>` +
      `<g>${lfoTimed(o, "fast", 0.6, `<animateTransform attributeName="transform" type="translate" values="0 0;0 ${f1(-r * 0.7)};0 0" keyTimes="0;0.5;1" calcMode="spline" keySplines="0.3 0.6 0.5 1;0.5 0 0.7 0.4" dur="0.6s" repeatCount="indefinite"/>`)}` +
      `<g transform="scale(${z.toFixed(3)})"><g><animateTransform attributeName="transform" type="rotate" values="0 0 -30;360 0 -30" dur="2.4s" repeatCount="indefinite"/>${tumbleweedSvg(5120)}</g></g></g></g></g>`); });
  return s;
}
// the desert's lizard, a collared lizard, facing right, its feet on the line y = 0, on its flat stone: its body doing
// push-ups about its hips (the quick LFO, off of its round on), its near front leg straightening under it
function lizardSvg(o, off) {
  const body = "#2f9e98", edge = "#1d6b66", up = own => lfoTimed(o, "fast", off, own);
  return `<ellipse cx="0" cy="4" rx="34" ry="6" fill="#a8865f"/><ellipse cx="-2" cy="1.5" rx="30" ry="4.5" fill="#c9a882"/>` +
    `<path d="M-8 -7 Q-30 -7 -48 -2 Q-30 -2 -8 -2 Z" fill="${body}" stroke="${edge}" stroke-width="0.8"/>` +
    `<path d="M-6 -4 L-11 0 L-15 0 M-3 -4 L-1 0 L3 0" fill="none" stroke="${edge}" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"/>` +
    `<g>${up(`<animateTransform attributeName="transform" type="rotate" values="0 -6 -5;-14 -6 -5;0 -6 -5" keyTimes="0;0.4;1" dur="0.6s" repeatCount="indefinite"/>`)}` +
    `<path d="M10 -4 L8 0" fill="none" stroke="${edge}" stroke-width="2" stroke-linecap="round"/>` +
    `<ellipse cx="4" cy="-6" rx="12" ry="4.5" fill="${body}" stroke="${edge}" stroke-width="0.8"/><path d="M-5 -3.5 Q4 -1.5 14 -4" fill="none" stroke="#9ee6dc" stroke-width="1.6" stroke-linecap="round"/>` +
    `<ellipse cx="19" cy="-8" rx="6" ry="4" fill="#e9c46a" stroke="#a07a2a" stroke-width="0.8"/><path d="M12 -10.5 Q13 -6.5 12 -2.5 M14.5 -11 Q15.5 -7.5 14.5 -3.5" fill="none" stroke="#1f2937" stroke-width="1.4"/>` +
    `<circle cx="21" cy="-9" r="1" fill="#1f2937"/></g>` +
    `<line x1="12" y1="0" x2="12" y2="-5" stroke="${edge}" stroke-width="2.2" stroke-linecap="round">${up(`<animate attributeName="y2" values="-5;-9.4;-5" keyTimes="0;0.4;1" dur="0.6s" repeatCount="indefinite"/>`)}</line>`;
}
// a tumbleweed: a ball of dry twigs, 60 across, resting on y = 0
function tumbleweedSvg(seed) {
  const R = seeded(seed), r = 30, P = (x, y) => `${f1(x)} ${f1(y)}`;
  let d = "", d2 = "";
  for (let i = 0; i < 12; i++) { const a = R(i) * 6.28, b = a + 1.6 + 2.2 * R(i + 20), ra = r * (0.55 + 0.45 * R(i + 40)), rb = r * (0.55 + 0.45 * R(i + 60));
    const seg = `M${P(ra * Math.cos(a), -r + ra * Math.sin(a))} Q${P(r * 0.8 * (R(i + 80) - 0.5), -r + r * 0.8 * (R(i + 90) - 0.5))} ${P(rb * Math.cos(b), -r + rb * Math.sin(b))} `;
    if (i % 3) d += seg; else d2 += seg; }
  return `<circle cx="0" cy="${-r}" r="${r}" fill="#c9a46a" opacity="0.25"/><circle cx="0" cy="${-r}" r="${r - 1}" fill="none" stroke="#9a7444" stroke-width="2.2" stroke-dasharray="13 7"/>` +
    `<path d="${d}" fill="none" stroke="#8a6a3e" stroke-width="1.8" stroke-linecap="round"/><path d="${d2}" fill="none" stroke="#dcc08a" stroke-width="1.5" stroke-linecap="round"/>`;
}
// a hawk soaring, seen from below (so the same whichever way it turns), its middle at 0, 0, 64 across its wings
function hawkSvg() {
  const wing = sd => `<path d="M${sd * 3} -3 Q${sd * 15} -10 ${sd * 31} -5 L${sd * 29} -2 L${sd * 32} 0 L${sd * 28} 2 L${sd * 30} 4 Q${sd * 16} 6 ${sd * 3} 5 Z" fill="#6b4a2e"/>` +
    `<path d="M${sd * 5} -1 Q${sd * 14} -5 ${sd * 23} -2 Q${sd * 14} 3 ${sd * 5} 3 Z" fill="#d9b991" opacity="0.85"/>`;
  return wing(-1) + wing(1) + `<path d="M-3 8 L-9 21 Q0 25 9 21 L3 8 Z" fill="#c2552b"/><ellipse cx="0" cy="1" rx="4.2" ry="10" fill="#5b3a22"/><circle cx="0" cy="-10" r="3.6" fill="#5b3a22"/>`;
}

// The jungle waterfall: a mossy cliff, the water falling (streaks running
// down it), a pool below with ripples, mist rising, big-leaved trees,
// vines swaying, parrots, flowers.
// The jungle waterfall's time of day (its own, so the cave behind it sees
// the same): day, or a tint over everything at sunset and at night, the
// moon out at night; and how fast its water falls (100 = the usual)
const FALL_TINT = { day: ["#ffffff", 0], sunset: ["#9a3412", 0.25], night: ["#0b1030", 0.55] };
const fallTimeOf = o => o.fallTime === "clock" ? skyTimeOf(o, false) : FALL_TINT[o.fallTime] ? o.fallTime : "day",   // (ticket 490: or by the clock)
      fallRateOf = o => Math.max(0.1, (o.fallRate ?? 100) / 100);
// the falls' pool: the line where the falls meet it, its water (an ellipse
// about (0, cy)), and the near bank's edge over its near side (for its
// drawing, and for a friend wading in it: WATER, ticket 122)
const poolOf = ({ g, k, W }) => { const pool = g - 70 * k; return { pool, cy: pool + 30 * k, rx: W * 0.42, ry: 55 * k, near: g + 20 * k }; };
function waterfallScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o, side } = c;
  const fw = (o.fallWidth ?? 100) / 100, vines = o.vines ?? 8, mist = (o.mist ?? 60) / 100, parrots = o.parrots ?? 2, rate = fallRateOf(o), time = fallTimeOf(o);
  const top = y0 + H * 0.12, pl = poolOf(c), pool = pl.pool;
  let s = SEP + part("wf-back", c.F, () => {
    let t = `<defs>${grad("bg-sky", "#9fd8c8", "#e8fff4", y0, g)}</defs>` + all("url(#bg-sky)");
    // the far jungle: layers of leaf pads, a crown here and there standing up
    // out of them (the shared leafy tree)
    t += `<defs>${leafyTreeDef("bg-jungle-tree", 81, { wide: 1.3 })}</defs>`;
    // (ticket 261: each of its pads and crowns more than 75% hidden by those in front of it and the cliff, left out;
    // the columns kept, their tops the far jungle's skyline; the cliff's outline by its corners, inside its curves)
    const jungle = [], box = (a, b, c2, d) => [[[a, b], [c2, b], [c2, d], [a, d]]];
    for (const [row, fill, yb, n] of [[0, "#5f9a6a", y0 + H * 0.3, 16], [1, "#3f8050", y0 + H * 0.42, 14]])
      for (let i = 0; i < n; i++) {
        const x = x0 - 20 * k + (W + 40 * k) * i / (n - 1), r = (40 + 25 * R(i + row * 20, 340)) * k, y = yb + 10 * k * R(i, 341);
        const j1 = Math.floor(R(i + row * 30, 342) * 8), s1 = (r / 95) * (R(i, 343) < 0.5 ? -1 : 1), j2 = Math.floor(R(i + row * 30, 344) * 8);
        jungle.push({ svg: `<rect x="${f1(x - r)}" y="${f1(y)}" width="${f1(2 * r)}" height="${f1(H)}" fill="${fill}"/>`, shape: box(x - r, y, x + r, y + H), keep: true },
                    { svg: padSvg(j1, x, y + r * 0.3, s1, r / 70, fill), shape: padOutline(j1, x, y + r * 0.3, s1, r / 70) },
                    { svg: padSvg(j2, x + r * 0.3, y - r * 0.2, (r / 130), r / 90, mix(fill, "#ffffff", 0.08)), shape: padOutline(j2, x + r * 0.3, y - r * 0.2, r / 130, r / 90) });
        if (R(i + row * 40, 345) < 0.25) jungle.push({ svg: use("bg-jungle-tree", x, y + r * 0.6, r * 3 / 330, r * 3 / 330, ` color="${fill}"`), shape: useOutline("bg-jungle-tree", x, y + r * 0.6, r * 3 / 330) });
      }
    t += seenOnly(jungle, [[[-W * 0.34, pool], [-W * 0.3, top + 40 * k], [-W * 0.06, top], [W * 0.08, top], [W * 0.3, top + 30 * k], [W * 0.36, pool]]]);
    // the cliff, mossy, the falls' lip at its top
    t += `<path d="M${P(-W * 0.34, pool)} L${P(-W * 0.3, top + 40 * k)} Q${P(-W * 0.2, top - 10 * k)} ${P(-W * 0.06, top)} L${P(W * 0.08, top)} Q${P(W * 0.22, top - 12 * k)} ${P(W * 0.3, top + 30 * k)} L${P(W * 0.36, pool)} Z" fill="#6e6a62"/>`;
    for (let i = 0; i < 14; i++) {
      const x = -W * 0.3 + W * 0.62 * R(i, 342), y = top + 20 * k + (pool - top - 40 * k) * R(i, 343);
      t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1((14 + 14 * R(i, 344)) * k)}" ry="${f1(7 * k)}" fill="${i % 2 ? "#4f8a3a" : "#5c5850"}" opacity="0.85"/>`;
    }
    return t;
  });
  // the falls: a sheet of water, streaks running down it, foam at the lip
  // (a doorway: behind them is a cave, unseen from here)
  s += SEP + `<g class="portal" data-to="cave">` + part("wf-falls", [c.F, fw, rate], () => {
    const half = 60 * fw * k;
    let t = `<defs><linearGradient id="bg-falls" x1="0" y1="0" x2="1" y2="0"><stop offset="0" stop-color="#9ad7f0"/><stop offset="0.5" stop-color="#e6f8ff"/><stop offset="1" stop-color="#9ad7f0"/></linearGradient></defs>` +
            `<path d="M${P(-half, top)} L${P(half, top)} L${P(half * 1.25, pool)} L${P(-half * 1.25, pool)} Z" fill="url(#bg-falls)" opacity="0.95"/>`;
    const n = Math.max(4, Math.round(10 * fw));
    for (let i = 0; i < n; i++) {
      const u = (i + 0.5) / n, xa = -half + 2 * half * u, xb = -half * 1.25 + 2.5 * half * u, dash = (30 + 30 * R(i, 345)) * k, gap = (20 + 20 * R(i, 346)) * k;
      t += `<line x1="${f1(xa)}" y1="${f1(top)}" x2="${f1(xb)}" y2="${f1(pool)}" stroke="#ffffff" stroke-width="${f1((2 + 3 * R(i, 347)) * k)}" stroke-dasharray="${f1(dash)} ${f1(gap)}" opacity="0.8">` +
           `<animate attributeName="stroke-dashoffset" values="0;${f1(-(dash + gap) * 2)}" dur="${((0.6 + 0.4 * R(i, 348)) / rate).toFixed(2)}s" repeatCount="indefinite"/></line>`;
    }
    return t + `<ellipse cx="0" cy="${f1(top)}" rx="${f1(half * 1.1)}" ry="${f1(6 * k)}" fill="#ffffff" opacity="0.9"/>`;
  }) + `</g>`;
  // the pool, ripples spreading (the near bank's wave, 4 and 1.1, is WATER's too)
  s += SEP + part("wf-pool", c.F, () => land(pool + 30 * k, 6, "#5f8a3a", 0.3) +
    `<ellipse cx="0" cy="${f1(pl.cy)}" rx="${f1(pl.rx)}" ry="${f1(pl.ry)}" fill="#3f9ad0"/>` +
    `<ellipse cx="0" cy="${f1(pool + 24 * k)}" rx="${f1(W * 0.34)}" ry="${f1(36 * k)}" fill="#62b6e4"/>` +
    [0, 1, 2].map(i => `<ellipse cx="0" cy="${f1(pool + 6 * k)}" rx="${f1(60 * k)}" ry="${f1(10 * k)}" fill="none" stroke="#ffffff" stroke-width="${f1(2 * k)}">` +
      `<animate attributeName="rx" values="${f1(40 * k)};${f1(200 * k)}" dur="2.4s" begin="${(i * 0.8).toFixed(1)}s" repeatCount="indefinite"/>` +
      `<animate attributeName="ry" values="${f1(6 * k)};${f1(30 * k)}" dur="2.4s" begin="${(i * 0.8).toFixed(1)}s" repeatCount="indefinite"/>` +
      `<animate attributeName="opacity" values="0.8;0" dur="2.4s" begin="${(i * 0.8).toFixed(1)}s" repeatCount="indefinite"/></ellipse>`).join("") +
    land(pl.near, 4, "#4f8a3a", 1.1));
  // the mist at the falls' foot, billowing (as much as the slider says; an
  // object at the depth of the pool's far edge, under it)
  // (ticket 155: with blurs off, each puff a radial gradient fading to
  // nothing at its edge, soft without a filter)
  if (mist > 0) s += SEP + obj("mist", pool, part("wf-mist", [c.F, mist, PERF.blur], () => (PERF.blur ? `<defs><filter id="bg-mist" x="-50%" y="-50%" width="200%" height="200%"><feGaussianBlur stdDeviation="${f1(14 * k)}"/></filter></defs><g filter="url(#bg-mist)" pointer-events="none">`
                    : `<defs><radialGradient id="bg-mist-soft"><stop offset="0.35" stop-color="#ffffff"/><stop offset="1" stop-color="#ffffff" stop-opacity="0"/></radialGradient></defs><g pointer-events="none">`) +
    [...Array(Math.round(3 + 6 * mist))].map((_, i) => {
      const x = (R(i, 349) - 0.5) * 220 * k, y = pool - (10 + 60 * R(i, 350)) * k, r = (40 + 40 * R(i, 351)) * k;
      return `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(r)}" ry="${f1(r * 0.55)}" fill="${PERF.blur ? "#ffffff" : "url(#bg-mist-soft)"}" opacity="${(0.25 + 0.35 * mist).toFixed(2)}">` +
             `<animate attributeName="cy" values="${f1(y)};${f1(y - 30 * k)};${f1(y)}" dur="${(4 + 3 * R(i, 352)).toFixed(1)}s" repeatCount="indefinite"/>` +
             `<animate attributeName="opacity" values="${(0.15 + 0.3 * mist).toFixed(2)};${(0.35 + 0.4 * mist).toFixed(2)};${(0.15 + 0.3 * mist).toFixed(2)}" dur="${(3 + 2 * R(i, 353)).toFixed(1)}s" repeatCount="indefinite"/></ellipse>`;
    }).join("") + `</g>`));
  // big-leaved jungle trees either side, flowers at their feet (each tree
  // an object, at its trunk's foot; the flowers flat on the ground)
  s += SEP + part("wf-trees", c.F, () => {
    let t = "";
    for (const sd of [-1, 1]) {
      const tx = sd * W * 0.43, tb = g + 60 * k, tt = y0 + H * 0.18;
      let tree = `<path d="M${P(tx - 22 * k, tb)} Q${P(tx - 10 * k, (tb + tt) / 2)} ${P(tx - sd * 20 * k, tt)} L${P(tx - sd * 4 * k, tt)} Q${P(tx + 14 * k, (tb + tt) / 2)} ${P(tx + 22 * k, tb)} Z" fill="#6b4a2b"/>`;
      for (let j = 0; j < 9; j++) {
        const a = (-160 + j * 40) * Math.PI / 180, l = (110 + 50 * R(j, 354)) * k, ex = tx - sd * 20 * k + Math.cos(a) * l, ey = tt + Math.sin(a) * l * 0.7 + 30 * k;
        tree += `<path d="M${P(tx - sd * 14 * k, tt)} Q${P((tx - sd * 14 * k + ex) / 2 + 10 * k, (tt + ey) / 2 - 40 * k)} ${P(ex, ey)} Q${P((tx - sd * 14 * k + ex) / 2 - 10 * k, (tt + ey) / 2 + 10 * k)} ${P(tx - sd * 14 * k, tt)} Z" ` +
             `fill="${["#2e8b3a", "#3fa34a", "#24753a"][j % 3]}" stroke="#1d5a2a" stroke-width="${f1(1.5 * k)}"/>`;
      }
      t += obj(`jungle tree, ${sd < 0 ? "left" : "right"}`, tb, tree);
      const fls = [];   // (ticket 284: the flowers by the trunk in rows by depth)
      for (let j = 0; j < 4; j++) {   // flowers by the trunk
        const fx = tx - sd * (30 + 40 * j) * k, fy = g + (30 + 10 * j) * k;
        fls.push({ y: fy, svg: [0, 72, 144, 216, 288].map(a => `<ellipse cx="${f1(fx)}" cy="${f1(fy - 8 * k)}" rx="${f1(4 * k)}" ry="${f1(9 * k)}" fill="${["#ff4f6d", "#ff8c1a", "#f2c12e", "#e85dbe"][j]}" transform="rotate(${a} ${f1(fx)} ${f1(fy)})"/>`).join("") +
             `<circle cx="${f1(fx)}" cy="${f1(fy)}" r="${f1(3.5 * k)}" fill="#ffd23f"/>` });
      }
      t += depthRows(`flowers, ${sd < 0 ? "left" : "right"}`, fls, 12 * k);
    }
    return t;
  });
  // vines hanging from the top, swaying (an object: hanging over the pool,
  // at the depth of its far edge, under them)
  if (vines > 0) s += SEP + obj("vines", pool, part("wf-vines", [c.F, vines, sceneLfoKey(o, "slow")], () => [...Array(vines)].map((_, i) => {
    const x = x0 + W * (0.04 + 0.92 * R(i, 355)), len = (120 + 220 * R(i, 356)) * k;
    let d = `M${P(x, y0 - 10 * k)}`, leaves = "";
    for (let j = 1; j <= 8; j++) {
      const y = y0 - 10 * k + len * j / 8, xx = x + 10 * k * Math.sin(j * 1.1 + i);
      d += ` L${P(xx, y)}`;
      leaves += `<ellipse cx="${f1(xx + (j % 2 ? 7 : -7) * k)}" cy="${f1(y)}" rx="${f1(7 * k)}" ry="${f1(3.5 * k)}" fill="#3fa34a" transform="rotate(${j % 2 ? 30 : -30} ${f1(xx)} ${f1(y)})"/>`;
    }
    return `<g>${lfoTimed(o, "slow", R(i, 357), `<animateTransform attributeName="transform" type="rotate" values="-3 ${f1(x)} ${f1(y0)};3 ${f1(x)} ${f1(y0)};-3 ${f1(x)} ${f1(y0)}" dur="${(4 + 2 * R(i, 357)).toFixed(1)}s" repeatCount="indefinite"/>`)}` +
           `<path d="${d}" fill="none" stroke="#3f7a2a" stroke-width="${f1(3 * k)}"/>${leaves}</g>`;
  }).join("")));
  // parrots on the branches (each an object, at the foot of the tree it sits in)
  s += SEP + part("wf-parrots", [c.F, parrots, sceneLfoKey(o, "fast")], () => [...Array(parrots)].map((_, i) => {
    const sd = i % 2 ? 1 : -1, x = sd * (W * 0.32 - 30 * k * Math.floor(i / 2)), y = y0 + H * (0.3 + 0.08 * Math.floor(i / 2)), f = sd < 0 ? 1 : -1;
    return obj(`parrot ${i + 1}`, g + 60 * k, `<g ${tf(x, y, k * f, k)}><path d="M-6 0 Q-14 22 -4 40 L4 40 Q10 20 6 0 Z" fill="${["#e53935", "#2e86de", "#27ae60"][i % 3]}"/>` +
           `<path d="M-4 38 L-8 66 L0 50 L6 68 L4 38 Z" fill="${["#2e86de", "#f2c12e", "#e53935"][i % 3]}"/>` +
           `<circle cx="2" cy="-4" r="9" fill="${["#e53935", "#2e86de", "#27ae60"][i % 3]}"/><circle cx="5" cy="-6" r="2.5" fill="#ffffff"/><circle cx="5.5" cy="-6" r="1.2" fill="#111"/>` +
           `<path d="M9 -6 Q18 -4 12 4 Q10 0 9 -2 Z" fill="#f2c12e"/><g>${sceneLfo(o, "fast", (i * 0.37) % 1, "transform", (v, amp) => `${f1((10 + 30 * amp) * (1 + v) / 2)} -6 8`,
             `<animateTransform attributeName="transform" type="rotate" values="0 -6 8;24 -6 8;0 -6 8" dur="${(0.6 + 0.1 * i).toFixed(1)}s" repeatCount="indefinite"/>`)}<path d="M-6 8 Q-18 20 -10 34" fill="none" stroke="#f2c12e" stroke-width="4"/></g></g>`);
  }).join(""));
  // the light at sunset or at night, over everything (an object in front of
  // everything, the friend too: where they stand among the objects, it is
  // over them all the same)
  // (ticket 490: by the clock, the moon in its phase where it is, the sun low at dusk, instead of the moon drawn always the same)
  const clock = o.fallTime === "clock";
  if (time !== "day") s += SEP + obj(time === "night" ? "dark" : "sunset light", Infinity, part("wf-tint", [c.F, time, clock ? skyKey(o) : ""], () => all(FALL_TINT[time][0]).replace("<rect ", `<rect pointer-events="none" opacity="${FALL_TINT[time][1]}" `) +
    // (the sky open above the cliff's top, and beside it above the jungle: behind them, the moon is hidden)
    (clock ? skyBodiesSvg(c, o, y0 + H * 0.4, y0 + 8 * k, "#0b1030", ([x, y]) => y < top - 14 * k || (Math.abs(x) > W * 0.33 && y < y0 + H * 0.28)) : time === "night" ? `<circle cx="${f1(-W * 0.32)}" cy="${f1(y0 + H * 0.12)}" r="${f1(26 * k)}" fill="#fff6d6" opacity="0.95"/><circle cx="${f1(-W * 0.32 + 10 * k)}" cy="${f1(y0 + H * 0.12 - 6 * k)}" r="${f1(22 * k)}" fill="#0b1030" opacity="0.35"/>` : "")));
  // (ticket 233: its old signpost, fixed links to places, is gone; the world's ways are the nav arrows, and a sign with directions, 🧭, a thing, says where they lead)
  return s;
}

// The camp site: day, sunset or night; tents round a campfire (on or out,
// its flames as big as the slider says, in a group that grows them),
// sparks, and on the fire nothing, veggie sausages on sticks, or a pot of
// soup hanging over it, steaming.
// the tents, in the order they are put up: where each stands (a share of
// the width), its color (the same inside, see tentScene) and its door's side
const CAMP_TENTS = [{ u: -0.32, color: "#e8762c", sd: 1, name: "🟧 orange" }, { u: 0.33, color: "#3b7dd8", sd: -1, name: "🟦 blue" },
                    { u: -0.12, color: "#27ae60", sd: 1, name: "🟩 green" }];
const CAMP_SKY = { day: ["#5aa9e6", "#dff2ff"], sunset: ["#3a2a6a", "#f2a65a"], night: ["#0b1030", "#24305a"] };
// Stars in a night sky, as the place's settings say: their shape (dots, or
// stars of four, five, six or eight points, or a mix of them), their size
// on average (many small, a few big; 100 = the usual, up to huge), their
// brightness on average (the brightest with a soft glow), and how much the
// big bright ones twinkle: turning a little, growing and shrinking,
// glimmering (and the small ones blink, more with more twinkle).
const STAR_SHAPES = [["dot", "• dots"], ["four", "✦ four points"], ["five", "★ five points"], ["six", "✶ six points"],
                     ["eight", "✴ eight points"], ["mix", "🎲 a mix"]];
const STAR_TINTS = ["#ffffff", "#fff4d6", "#e2ebff", "#fffbe8"];
// a star's outline about (0, 0), its points r out
function starPath(shape, r) {
  if (shape === "four") {   // a sparkle: four points, its sides curving in
    const c = 0.14 * r;
    return `M0 ${f1(-r)} Q${f1(c)} ${f1(-c)} ${f1(r)} 0 Q${f1(c)} ${f1(c)} 0 ${f1(r)} Q${f1(-c)} ${f1(c)} ${f1(-r)} 0 Q${f1(-c)} ${f1(-c)} 0 ${f1(-r)} Z`;
  }
  // five or six points; eight, the four between them shorter
  const n = { five: 5, six: 6, eight: 8 }[shape], inner = { five: 0.42, six: 0.48, eight: 0.26 }[shape], pts = [];
  for (let i = 0; i < 2 * n; i++) {
    const a = -Math.PI / 2 + i * Math.PI / n, rr = i % 2 ? r * inner : shape === "eight" && i % 4 === 2 ? r * 0.58 : r;
    pts.push(`${f1(rr * Math.cos(a))} ${f1(rr * Math.sin(a))}`);
  }
  return "M" + pts.join(" L") + " Z";
}
// n stars over the box (xa..xb, ya..yb), at the scene's scale k; R and the
// seed place them (each its own size, brightness, tint and shape)
function starsSvg(o, n, xa, xb, ya, yb, k, R, seed) {
  const pick = STAR_SHAPES.some(([v]) => v === o.starShape) ? o.starShape : "mix";
  const size = (o.starSize ?? 100) / 100, bright = (o.starBright ?? 85) / 100, tw = (o.starTwinkle ?? 40) / 100;
  let t = `<defs><radialGradient id="bg-star-glow"><stop offset="0" stop-color="#fffbe8" stop-opacity="0.6"/><stop offset="1" stop-color="#fffbe8" stop-opacity="0"/></radialGradient></defs>`;
  for (let i = 0; i < n; i++) {
    const x = xa + (xb - xa) * R(i, seed), y = ya + (yb - ya) * R(i, seed + 1), u = R(i, seed + 2);
    const f = 0.4 + 2.4 * u * u * u, op = Math.max(0.08, Math.min(1, bright * (0.55 + 0.9 * R(i, seed + 3))));
    const shape = pick !== "mix" ? pick : f < 0.6 ? "dot" : ["four", "five", "six", "eight", "four"][Math.floor(R(i, seed + 4) * 5)];
    // (a point star's ink is thin: it is drawn wider than a dot, to look as big)
    const r = 1.5 * k * size * f * (shape === "dot" ? 1 : 2.2), tint = STAR_TINTS[Math.floor(R(i, seed + 5) * STAR_TINTS.length)];
    const body = shape === "dot" ? `<circle r="${f1(r)}" fill="${tint}"/>` : `<path d="${starPath(shape, r)}" fill="${tint}"/>`;
    // (the glow hugs a big star closer, or big stars would fog the sky)
    const glow = op * f > 1.1 ? `<circle r="${f1(r * (1 + 1.6 / Math.max(1, Math.sqrt(size))))}" fill="url(#bg-star-glow)" opacity="${(Math.min(1, op * f / 2)).toFixed(2)}"/>` : "";
    // the big bright ones twinkle: turn, grow and shrink, glimmer, each at
    // its own pace; the small ones blink (a quarter of them)
    const big = tw > 0 && op * f > 0.9, d = (s0, s1) => (s0 + s1 * R(i, seed + 6)).toFixed(2);
    // (with the scene's quick LFO, each its own way in: sceneLfo)
    const glimmer = big || (tw > 0 && i % 4 === 0)
      ? sceneLfo(o, "fast", R(i, seed + 7), "opacity", v => (op * (1 - 0.75 * tw * (1 + v) / 2)).toFixed(2),
          `<animate attributeName="opacity" values="${op.toFixed(2)};${(op * (1 - 0.75 * tw)).toFixed(2)};${op.toFixed(2)}" dur="${d(1.4, 2.2)}s" repeatCount="indefinite"/>`) : "";
    let star = `<g opacity="${op.toFixed(2)}">${glimmer}${glow}${body}</g>`;
    if (big) star = `<g><animateTransform attributeName="transform" type="scale" values="1;${(1 + 0.45 * tw).toFixed(2)};1" dur="${d(1.1, 1.6)}s" repeatCount="indefinite"/>${star}</g>`;
    if (big && shape !== "dot")
      star = `<g><animateTransform attributeName="transform" type="rotate" values="0;${f1(18 * tw)};0;${f1(-18 * tw)};0" dur="${d(4, 4)}s" repeatCount="indefinite"/>${star}</g>`;
    t += `<g transform="translate(${f1(x)} ${f1(y)})">${star}</g>`;
  }
  return t;
}
// (ticket 50) the logs by the fire: each one's top, 100 long and 28 thick
// about its middle (dx, dy from the fire), as campsiteScene's "camp-logs"
// draws them; one sits on the left log facing the fire (right), on the
// right one facing it (left). Not the near log: there one would face the
// fire, away from us, and the friend has no back view.
SEATS.campsite = (c, B) => { const { g, k } = c, fx = 0, fy = g + 40 * k;
  return [[-150, 30, "left", "right"], [150, 30, "right", "left"]].map(([dx, dy, where, face]) =>
    ({ name: `log, ${where}`, x0: fx + (dx - 50) * k, x1: fx + (dx + 50) * k, y: fy + (dy - 14) * k, z: fy + (dy + 14) * k + 1, facing: [face] })); };
// (ticket 185, v3: lifted out of the camp site, unchanged, for the object library) a log to sit on,
// lying across, its middle at (x, y), its cut end to the right
const logSvg = (x, y, k) => `<rect x="${f1(x - 50 * k)}" y="${f1(y - 14 * k)}" width="${f1(100 * k)}" height="${f1(28 * k)}" rx="${f1(14 * k)}" fill="#7a5232" stroke="#4a2e1a" stroke-width="${f1(2 * k)}"/>` +
    `<ellipse cx="${f1(x + 50 * k)}" cy="${f1(y)}" rx="${f1(7 * k)}" ry="${f1(14 * k)}" fill="#c9a66b"/>`;
// (lifted out of the camp site, unchanged) the campfire's ring of stones and its crossed wood, round (fx, fy)
function campfireHearthSvg(fx, fy, k) {
  const P = (x, y) => `${f1(x)} ${f1(y)}`;   // (as backgroundSvg's)
  return [...Array(10)].map((_, i) => {
    const a = i / 10 * 2 * Math.PI;
    return `<ellipse cx="${f1(fx + 50 * k * Math.cos(a))}" cy="${f1(fy + 14 * k * Math.sin(a))}" rx="${f1(13 * k)}" ry="${f1(9 * k)}" fill="${["#8a8a8a", "#9c9c9c", "#7a7a7a"][i % 3]}" stroke="#555" stroke-width="${f1(k)}"/>`;
  }).join("") + `<path d="M${P(fx - 34 * k, fy + 4 * k)} L${P(fx + 30 * k, fy - 10 * k)} M${P(fx + 34 * k, fy + 4 * k)} L${P(fx - 30 * k, fy - 10 * k)}" stroke="#5a3a1c" stroke-width="${f1(10 * k)}" stroke-linecap="round"/>`;
}
// (lifted out of the camp site, unchanged) its flames and sparks, at (0, 0), drawn up from there;
// a warm glow round them at sunset and at night
function campfireFlamesSvg(o, night, time) {
      const flame = (x, top, w) => `M${x - w} 0 Q${x - w * 0.6} ${-top * 0.55} ${x} ${-top} Q${x + w * 0.6} ${-top * 0.55} ${x + w} 0 Z`;
      // (its warm glow, at sunset and at night, breathing with the slow LFO)
      return (night || time === "sunset" ? `<ellipse cx="0" cy="-30" rx="220" ry="140" fill="#ff9a3c" opacity="${night ? 0.18 : 0.1}">` +
          sceneLfo(o, "slow", 0.2, "opacity", (v, amp) => ((night ? 0.18 : 0.1) * (1 + (0.25 + 0.6 * amp) * v)).toFixed(3)) + `</ellipse>` : "") +
        [[-18, 80, 18, "#ff6a1a", 0.7], [16, 90, 18, "#ff6a1a", 0.85], [0, 120, 24, "#ff8c1a", 0.6], [-6, 70, 12, "#ffd23f", 0.5], [8, 76, 11, "#ffd23f", 0.65]].map(([x, top, w, col, d]) =>
          `<path d="${flame(x, top, w)}" fill="${col}">${lfoTimed(o, "fast", (x + 20) / 40, `<animate attributeName="d" values="${flame(x, top, w)};${flame(x + 3, top * 0.75, w)};${flame(x - 2, top * 1.1, w)};${flame(x, top, w)}" dur="${d}s" repeatCount="indefinite"/>`)}</path>`).join("") +
        [...Array(8)].map((_, i) => { const x = -20 + 40 * ((i * 37) % 10) / 10;
          return `<circle cx="${x}" cy="-60" r="2.5" fill="#ffd23f"><animate attributeName="cy" values="-60;-220" dur="${(1.2 + (i % 4) * 0.35).toFixed(2)}s" begin="${(i * 0.3).toFixed(1)}s" repeatCount="indefinite"/>` +
                 `<animate attributeName="opacity" values="1;0" dur="${(1.2 + (i % 4) * 0.35).toFixed(2)}s" begin="${(i * 0.3).toFixed(1)}s" repeatCount="indefinite"/></circle>`; }).join("");
}
function campsiteScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o, side } = c;
  // (ticket 490) by the clock: the time of day, the sun and the moon where and when the game says (🌍)
  const clock = o.campTime === "clock", time = clock ? skyTimeOf(o, false) : CAMP_SKY[o.campTime] ? o.campTime : "day", fire = o.fireOn ?? true, fz = (o.flameSize ?? 100) / 100;
  const food = o.campFood ?? "none", tents = o.tents ?? 2, night = time === "night";
  const fx = 0, fy = g + 40 * k;
  const starKeys = night ? [o.starShape, o.starSize, o.starBright, o.starTwinkle] : [];
  let s = SEP + part("camp-sky", [c.F, time, ...starKeys, sceneLfoKey(o, "fast"), sceneLfoKey(o, "slow"), clock ? skyKey(o) : ""], () => {
    const [a, b] = CAMP_SKY[time];
    let t = `<defs>${grad("bg-sky", a, b, y0, g - 100 * k)}${cloudDef("bg-cloud", 1)}</defs>` + all("url(#bg-sky)");
    if (clock) { if (night) t += starsSvg(o, 70, x0, x1, y0, g - 160 * k, k, R, 360);
      t += skyBodiesSvg(c, o, g - 100 * k, y0 + 12 * k, a) + (time === "day" ? [0, 1, 2].map(i => driftingCloud(x0 + W * (0.15 + 0.3 * i), y0 + H * (0.1 + 0.05 * (i % 2)), k * 0.7, i, o)).join("") : "");
      return t; }
    if (time === "day") t += `<circle cx="${f1(x1 - W * 0.15)}" cy="${f1(y0 + H * 0.13)}" r="${f1(34 * k)}" fill="#ffd23f"/>` +
      [0, 1, 2].map(i => driftingCloud(x0 + W * (0.15 + 0.3 * i), y0 + H * (0.1 + 0.05 * (i % 2)), k * 0.7, i, o)).join("");
    if (time === "sunset") t += `<circle cx="${f1(-W * 0.2)}" cy="${f1(g - 110 * k)}" r="${f1(46 * k)}" fill="#ff8c42" opacity="0.95"/>`;
    if (night) {
      // the stars, then the moon over them (its dark side hides those behind it)
      t += starsSvg(o, 70, x0, x1, y0, g - 160 * k, k, R, 360) +
           `<circle cx="${f1(x1 - W * 0.16)}" cy="${f1(y0 + H * 0.14)}" r="${f1(28 * k)}" fill="#f4eebc"/><circle cx="${f1(x1 - W * 0.16 + 12 * k)}" cy="${f1(y0 + H * 0.14 - 8 * k)}" r="${f1(24 * k)}" fill="#0b1030"/>`;
    }
    return t;
  });
  // the ground, the woods round the camp (darker at night)
  s += SEP + part("camp-ground", [c.F, time], () => {
    const tree = { day: ["#2f6e3a", "#3a7d44"], sunset: ["#2a4a3a", "#33553f"], night: ["#0f2018", "#13281d"] }[time];
    let t = `<defs>${PINE_DEF}</defs>` + land(g - 110 * k, 8, night ? "#1d3a24" : time === "sunset" ? "#4a6a3a" : "#7cc25a", 0.5), row = "";
    // (the row of pines an object, at their middle depth)
    for (let i = 0; i < 26; i++) row += use("bg-pine", x0 - 20 * k + (W + 40 * k) * i / 25, g - 100 * k + 10 * k * R(i, 364), k * (0.45 + 0.25 * R(i, 365)), k * (0.45 + 0.25 * R(i, 365)), ` color="${tree[i % 2]}"`);
    t += obj("woods", g - 95 * k, row) + land(g - 40 * k, 4, night ? "#24452c" : time === "sunset" ? "#5a7a40" : "#8bc56a", 1.3) +
         `<ellipse cx="${f1(fx)}" cy="${f1(fy + 6 * k)}" rx="${f1(170 * k)}" ry="${f1(45 * k)}" fill="${night ? "#3a2e22" : "#a8875a"}"/>`;
    return t;
  });
  // tents, each a doorway into its inside (each an object, standing where
  // its sides meet the ground; named by its color)
  s += SEP + part("camp-tents", [c.F, tents, time], () => CAMP_TENTS.slice(0, tents).map(({ u, color: col, sd, name }, i) => {
    const tx = W * u, tb = g - 40 * k + i * 6 * k, w = 130 * k, h = 110 * k, dark = night ? 0.45 : 0;
    return obj(`${name.split(" ").pop()} tent`, tb, `<g class="portal" data-to="tent" data-tent="${i + 1}"><polygon points="${P2(tx - w / 2, tb)} ${P2(tx, tb - h)} ${P2(tx + w / 2, tb)}" fill="${col}" stroke="#00000055" stroke-width="${f1(2 * k)}"/>` +
           `<polygon points="${P2(tx - 12 * k * sd, tb)} ${P2(tx, tb - h * 0.75)} ${P2(tx + 22 * k * sd, tb)}" fill="#2a1a12" opacity="0.75"/>` +
           `<line x1="${f1(tx)}" y1="${f1(tb - h)}" x2="${f1(tx)}" y2="${f1(tb - h - 14 * k)}" stroke="#555" stroke-width="${f1(2.5 * k)}"/>` +
           (dark ? `<polygon points="${P2(tx - w / 2, tb)} ${P2(tx, tb - h)} ${P2(tx + w / 2, tb)}" fill="#000814" opacity="${dark}"/>` : "") + `</g>`);
  }).join(""));
  // logs to sit on, round the fire (each an object, at its underside)
  s += SEP + part("camp-logs", c.F, () => [[-150, 30, "left"], [150, 30, "right"], [0, 100, "near"]].map(([dx, dy, where]) =>
    obj(`log, ${where}`, fy + (dy + 14) * k, logSvg(fx + dx * k, fy + dy * k, k))).join(""));
  // at night, the dark: in front of everything, the friend too, wherever
  // they stand; but the fire makes a pool of light round itself, clear at
  // the fire and dark far from it, so the fire stays bright and what stands
  // near it (the friend too) is lit by it
  if (night) s += SEP + obj("dark", Infinity, part("camp-night", c.F, () =>
    `<defs><radialGradient id="bg-camp-dark" gradientUnits="userSpaceOnUse" cx="${f1(fx)}" cy="${f1(fy - 50 * k)}" r="${f1(300 * k)}">` +
    `<stop offset="0.3" stop-color="#000814" stop-opacity="0"/><stop offset="1" stop-color="#000814" stop-opacity="0.28"/></radialGradient></defs>` +
    all("url(#bg-camp-dark)").replace("<rect ", `<rect pointer-events="none" `)));
  // the fire: stones and wood; its flames, sparks and glow in a group that
  // grows with the slider (out: smoke); all one object, at the fire's foot
  let campfire = SEP + part("camp-hearth", c.F, () => campfireHearthSvg(fx, fy, k));
  if (fire) {
    campfire += OPEN(`<g pointer-events="none" ${tf(fx, fy - 4 * k, fz * k)}>`) + part("camp-flames", [night, time, sceneLfoKey(o, "fast"), sceneLfoKey(o, "slow")], () => campfireFlamesSvg(o, night, time)) + CLOSE;
  } else {
    campfire += SEP + part("camp-smoke", c.F, () => [0, 1, 2].map(i => `<circle cx="${f1(fx + (i - 1) * 8 * k)}" cy="${f1(fy - 20 * k)}" r="${f1(10 * k)}" fill="#9a9aa2" opacity="0.5">` +
      `<animate attributeName="cy" values="${f1(fy - 20 * k)};${f1(fy - 170 * k)}" dur="3s" begin="${i}s" repeatCount="indefinite"/>` +
      `<animate attributeName="r" values="${f1(8 * k)};${f1(26 * k)}" dur="3s" begin="${i}s" repeatCount="indefinite"/>` +
      `<animate attributeName="opacity" values="0.5;0" dur="3s" begin="${i}s" repeatCount="indefinite"/></circle>`).join(""));
  }
  s += obj("campfire", fy, campfire);
  // what is cooking (an object over the fire, at its foot: sausages on
  // sticks, or a pot on a tripod standing round it)
  if (food === "sausages") s += SEP + obj("sausages", fy, part("camp-sausages", c.F, () => [[-1, -40], [1, -55], [-1, -70]].map(([sd, dy], i) => {
    const ex = fx + sd * 14 * k, ey = fy + dy * k, hx = fx + sd * 170 * k, hy = fy + (dy + 70) * k;
    return `<line x1="${f1(hx)}" y1="${f1(hy)}" x2="${f1(ex)}" y2="${f1(ey)}" stroke="#8b5e34" stroke-width="${f1(3.5 * k)}" stroke-linecap="round"/>` +
           `<rect x="${f1(ex - 26 * k)}" y="${f1(ey - 7 * k)}" width="${f1(36 * k)}" height="${f1(14 * k)}" rx="${f1(7 * k)}" fill="${["#b5674d", "#a0523a", "#c27a5a"][i]}" ` +
           `stroke="#5a2a1a" stroke-width="${f1(1.5 * k)}" transform="rotate(${f1(Math.atan2(ey - hy, ex - hx) * 180 / Math.PI)} ${f1(ex)} ${f1(ey)})"/>`;
  }).join("")));
  if (food === "soup") s += SEP + obj("soup", fy + 10 * k, part("camp-soup", c.F, () => {
    const top = fy - 200 * k, px = fx, py = fy - 95 * k;
    return [-1, 1].map(sd => `<line x1="${f1(fx + sd * 90 * k)}" y1="${f1(fy + 10 * k)}" x2="${f1(fx)}" y2="${f1(top)}" stroke="#6e4a2c" stroke-width="${f1(6 * k)}" stroke-linecap="round"/>`).join("") +
      `<line x1="${f1(fx)}" y1="${f1(top + 4 * k)}" x2="${f1(fx + 10 * k)}" y2="${f1(fy + 14 * k)}" stroke="#6e4a2c" stroke-width="${f1(6 * k)}" stroke-linecap="round"/>` +
      `<line x1="${f1(px)}" y1="${f1(top)}" x2="${f1(px)}" y2="${f1(py - 34 * k)}" stroke="#555" stroke-width="${f1(2.5 * k)}" stroke-dasharray="${f1(4 * k)} ${f1(2 * k)}"/>` +
      `<path d="M${P(px - 34 * k, py - 30 * k)} Q${P(px, py - 52 * k)} ${P(px + 34 * k, py - 30 * k)}" fill="none" stroke="#555" stroke-width="${f1(2.5 * k)}"/>` +
      `<path d="M${P(px - 38 * k, py - 30 * k)} Q${P(px - 40 * k, py + 20 * k)} ${P(px, py + 22 * k)} Q${P(px + 40 * k, py + 20 * k)} ${P(px + 38 * k, py - 30 * k)} Z" fill="#2b2b30" stroke="#111" stroke-width="${f1(2 * k)}"/>` +
      `<ellipse cx="${f1(px)}" cy="${f1(py - 30 * k)}" rx="${f1(36 * k)}" ry="${f1(7 * k)}" fill="#e8a33a"/>` +
      [0, 1, 2].map(i => `<path d="M${P(px + (i - 1) * 14 * k, py - 40 * k)} q${f1(-8 * k)} ${f1(-14 * k)} 0 ${f1(-28 * k)} q${f1(8 * k)} ${f1(-14 * k)} 0 ${f1(-28 * k)}" fill="none" stroke="#ffffff" stroke-width="${f1(3 * k)}" stroke-linecap="round" opacity="0">` +
        `<animate attributeName="opacity" values="0;0.75;0" dur="2.4s" begin="${(i * 0.8).toFixed(1)}s" repeatCount="indefinite"/>` +
        `<animateTransform attributeName="transform" type="translate" values="0 0;0 ${f1(-40 * k)}" dur="2.4s" begin="${(i * 0.8).toFixed(1)}s" repeatCount="indefinite"/></path>`).join("");
  }));
  // (ticket 233: its old signpost, fixed links to places, is gone; the world's ways are the nav arrows, and a sign with directions, 🧭, a thing, says where they lead)
  return s;
}
// Inside a tent at the camp site: its canvas all round, the same color as
// outside, its seams running to the far end, where the door stands open
// onto the camp (at the camp's own time of day, its fire lit or out: the
// camp's settings), a doorway back out. Each tent has things of its own:
// the orange one sleeping bags and a flashlight, the blue one books, a
// teddy bear and a lantern, the green one an explorer's map, binoculars,
// a backpack and a jar of fireflies.
// (the tent's frame, for its drawing and its seats alike: the far wall's foot, its half width, the eye's height where the
// seams meet, and a point on the far wall carried out toward us along its seam)
const tentFrame = c => { const { y0, g, W, H, k } = c, vy = y0 + H * 0.42;
  return { back: g - 70 * k, half: W * 0.3, vy, out: (x, y, to) => { const t = (to - y) / (y - vy); return [x + x * t, to]; } }; };
// (ticket 412, Matthew: "new laying down pose that matches the perspective of these sleeping bags, or any other surface
// against the L or R walls"; "click to lay down on the sleeping bags, once we can") The orange tent's sleeping bags are beds
// to lie along: from the open end, toward us, to the pillow at the far wall (along: those two ends' middles), facing us,
// in the bag's perspective (drawNow turns and shortens the friend to it). x0..x1 and y are its middle, for a click and its glow;
// z its open end, where the feet go; kz its middle's depth, which sizes the friend.
SEATS.tent = (c, B) => {
  if (Math.max(1, Math.min(CAMP_TENTS.length, Math.round(B.tent ?? 1))) !== 1) return [];
  const G = { k: c.k, y1: c.y1, ...tentFrame(c) };
  return TENT_BAGS.map(([name, a, b]) => { const xa = G.half * a, xb = G.half * b, { fy, ny, na, nb } = bagEnds(G, xa, xb), mid = (fy + ny) / 2;
    const l = (xa + na) / 2, r = (xb + nb) / 2;
    return { name, x0: Math.min(l, r), x1: Math.max(l, r), y: mid, z: ny - 4 * G.k, kz: mid, facing: ["front"], lie: true, flat: true,
             along: { near: [(na + nb) / 2, ny - 4 * G.k], far: [(xa + xb) / 2, fy + 2 * G.k], xa, xb, fy, ny, vy: G.vy, side: a < 0 ? -1 : 1 } }; });
};
function tentScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, o } = c;
  const n = Math.max(1, Math.min(CAMP_TENTS.length, Math.round(o.tent ?? 1))), T = CAMP_TENTS[n - 1];
  const time = o.campTime === "clock" ? skyTimeOf(o, false) : CAMP_SKY[o.campTime] ? o.campTime : "day", night = time === "night", fire = o.fireOn ?? true;   // (ticket 490)
  // the far wall (its peak, and its corners on the floor), the door in it,
  // and the point the seams run to (at the eye's height)
  const { back, half, vy, out } = tentFrame(c), apex = y0 + H * 0.08, top = y0 + H * 0.3, dh = W * 0.12;
  const seam = mix(T.color, "#000000", 0.55);
  let s = SEP + part("tent-shell", [c.F, n, time], () => {   // (time: its windows show the sky; the review after 0.0.385)
    const dark = mix(T.color, "#000000", 0.4), lit = mix(T.color, "#000000", 0.2), far = mix(T.color, "#ffffff", 0.1);
    let t = all(dark) + `<rect x="0" y="${f1(y0 - pad)}" width="${f1(x1 + pad)}" height="${f1(H + 2 * pad)}" fill="${lit}"/>`;
    // the seams along the walls, from the far wall's edges out past us
    for (const sd of [-1, 1]) for (let j = 1; j <= 3; j++) {
      const u = j / 4, ex = sd * half * u, ey = apex + (back - apex) * u, [ox, oy] = [ex + (ex - 0) * 12, ey + (ey - vy) * 12];
      t += `<line x1="${f1(ex)}" y1="${f1(ey)}" x2="${f1(ox)}" y2="${f1(oy)}" stroke="${seam}" stroke-width="${f1(2 * k)}" opacity="0.5"/>`;
    }
    // the far wall, the ridge above it, the groundsheet
    const [nx, ny] = out(half, back, y1 + pad);
    t += `<polygon points="${P2(0, apex)} ${P2(-half, back)} ${P2(half, back)}" fill="${far}" stroke="${seam}" stroke-width="${f1(2.5 * k)}"/>` +
         `<line x1="0" y1="${f1(apex)}" x2="0" y2="${f1(y0 - pad)}" stroke="${seam}" stroke-width="${f1(5 * k)}"/>` +
         `<polygon points="${P2(-half, back)} ${P2(half, back)} ${P2(nx, ny)} ${P2(-nx, ny)}" fill="#55606e" stroke="${seam}" stroke-width="${f1(2.5 * k)}"/>`;
    for (const u of [-0.5, 0, 0.5]) {
      const [lx, ly] = out(half * u, back, y1 + pad);
      t += `<line x1="${f1(half * u)}" y1="${f1(back)}" x2="${f1(lx)}" y2="${f1(ly)}" stroke="#4a5360" stroke-width="${f1(2 * k)}"/>`;
    }
    // a mesh window high on each wall, the sky behind it, drawn on the wall
    // in perspective: a point on a wall is u of the way down the far wall's
    // edge, carried out along its seam to sc times as far from where the
    // seams meet; equal steps in 1/sc are equal steps along the wall
    const onWall = (sd, u, sc) => [sd * half * u * sc, vy + (apex + (back - apex) * u - vy) * sc];
    const segment = (a, b) => `<line x1="${f1(a[0])}" y1="${f1(a[1])}" x2="${f1(b[0])}" y2="${f1(b[1])}" stroke="#333" stroke-width="${f1(0.8 * k)}" opacity="0.5"/>`;
    for (const sd of [-1, 1]) {
      const u0 = 0.385, u1 = 0.42, s0 = 2.67, s1 = 3.29, mWin = t.length;   // (ticket 284: each a wall object)
      t += `<polygon points="${[[u0, s0], [u0, s1], [u1, s1], [u1, s0]].map(([u, sc]) => P2(...onWall(sd, u, sc))).join(" ")}" ` +
           `fill="${CAMP_SKY[time][1]}" stroke="${seam}" stroke-width="${f1(3 * k)}" stroke-linejoin="round"/>`;
      // (the mesh: threads along the wall, toward where the seams meet, and across it, along the far wall's edge)
      for (let j = 1; j < 4; j++) { const u = u0 + (u1 - u0) * j / 4; t += segment(onWall(sd, u, s0), onWall(sd, u, s1)); }
      for (let j = 1; j < 6; j++) { const sc = 1 / (1 / s0 + (1 / s1 - 1 / s0) * j / 6); t += segment(onWall(sd, u0, sc), onWall(sd, u1, sc)); }
      t = wallSince(t, mWin, sd < 0 ? "window, left" : "window, right");
    }
    return t;
  });
  // the door, open onto the camp: its sky, the far trees, the fire
  s += SEP + part("tent-door", [c.F, n, time, fire, ...(night ? [o.starShape, o.starSize, o.starBright, o.starTwinkle] : []), sceneLfoKey(o, "fast")], () => {
    const [a, b] = CAMP_SKY[time], hor = back - 34 * k;
    const ground = night ? "#24452c" : time === "sunset" ? "#5a7a40" : "#8bc56a", tree = { day: "#2f6e3a", sunset: "#2a4a3a", night: "#0f2018" }[time];
    let v = `<defs><clipPath id="bg-tent-door"><polygon points="${P2(0, top)} ${P2(-dh, back)} ${P2(dh, back)}"/></clipPath>${grad("bg-tent-sky", a, b, top, hor)}${PINE_DEF}</defs>` +
            `<g class="portal" data-to="campsite"><g clip-path="url(#bg-tent-door)">` +
            `<rect x="${f1(-dh)}" y="${f1(top)}" width="${f1(2 * dh)}" height="${f1(back - top)}" fill="url(#bg-tent-sky)"/>`;
    // (the same night sky as outside the tent, its stars as they are there)
    if (night) v += starsSvg(o, 14, -dh, dh, top, top + (hor - top) * 0.8, k, R, 371);
    for (let i = 0; i < 7; i++) v += use("bg-pine", -dh + 2 * dh * (i + 0.5) / 7, hor + 6 * k * R(i, 374), k * (0.22 + 0.1 * R(i, 375)), k * (0.22 + 0.1 * R(i, 375)), ` color="${tree}"`);
    v += `<rect x="${f1(-dh)}" y="${f1(hor)}" width="${f1(2 * dh)}" height="${f1(back - hor + 2 * k)}" fill="${ground}"/>`;
    // the fire, small and far off (or its smoke)
    const fx = 0, fy = back - 6 * k;
    if (fire) v += [[-5, 22, 6, "#ff6a1a", 0.7], [5, 26, 6, "#ff8c1a", 0.6], [0, 16, 4, "#ffd23f", 0.5]].map(([x, h, w, col, d]) => {
      const fl = (hh) => `M${f1(fx + (x - w) * k)} ${f1(fy)} Q${f1(fx + x * k)} ${f1(fy - hh * k * 1.2)} ${f1(fx + (x + w) * k)} ${f1(fy)} Z`;
      return `<path d="${fl(h)}" fill="${col}"><animate attributeName="d" values="${fl(h)};${fl(h * 0.75)};${fl(h * 1.1)};${fl(h)}" dur="${d}s" repeatCount="indefinite"/></path>`;
    }).join("") + (night ? `<ellipse cx="${f1(fx)}" cy="${f1(fy - 8 * k)}" rx="${f1(40 * k)}" ry="${f1(22 * k)}" fill="#ff9a3c" opacity="0.25"/>` : "");
    else v += `<circle cx="${f1(fx)}" cy="${f1(fy - 10 * k)}" r="${f1(5 * k)}" fill="#9a9aa2" opacity="0.5"><animate attributeName="cy" values="${f1(fy - 10 * k)};${f1(top + 20 * k)}" dur="3s" repeatCount="indefinite"/>` +
              `<animate attributeName="opacity" values="0.5;0" dur="3s" repeatCount="indefinite"/></circle>`;
    v += `</g><polyline points="${P2(-dh, back)} ${P2(0, top)} ${P2(dh, back)}" fill="none" stroke="#2a2a2a" stroke-width="${f1(3 * k)}" stroke-dasharray="${f1(4 * k)} ${f1(3 * k)}"/></g>`;
    // the flaps, rolled back either side and tied
    for (const sd of [-1, 1]) {
      const fx2 = sd * (dh * 0.55 + 34 * k), fy2 = top + (back - top) * 0.5;
      v += `<polygon points="${P2(0, top)} ${P2(sd * dh, back)} ${P2(sd * (dh + 30 * k), back)} ${P2(fx2, fy2)}" fill="${mix(T.color, "#ffffff", 0.05)}" stroke="${seam}" stroke-width="${f1(1.5 * k)}"/>` +
           `<line x1="${f1(fx2 - sd * 12 * k)}" y1="${f1(fy2 - 6 * k)}" x2="${f1(fx2 + sd * 6 * k)}" y2="${f1(fy2 + 8 * k)}" stroke="#d9c08a" stroke-width="${f1(3 * k)}" stroke-linecap="round"/>`;
    }
    return v;
  });
  // the tent's own things on the groundsheet, either side of the middle (ticket 284: each an object of its own, what lies
  // flat on the floor, what stands at its depth: TENT_THINGS)
  for (const [name, at, svg] of TENT_THINGS[n - 1]({ k, P, R, back, half, out, y0, y1, g, W, H }))
    s += SEP + (at === "floor" ? floorObj(name, part("tent-thing-" + n + "-" + name, [c.F], () => svg)) : obj(name, at, part("tent-thing-" + n + "-" + name, [c.F], () => svg)));
  // night or sunset over the inside, then what glows (each in front of
  // everything, the friend too; what glows, painted after the night or the
  // sunset, then in front of it, else where it stands: the flashlight, on
  // the floor by the far wall; the lantern, hanging over about where the
  // friend stands; the jar, on the floor near us)
  if (night) s += SEP + obj("dark", Infinity, part("tent-night", c.F, () => all("#000814").replace("<rect ", `<rect pointer-events="none" opacity="0.4" `)));
  if (time === "sunset") s += SEP + obj("sunset light", Infinity, part("tent-sunset", c.F, () => all("#ff8c42").replace("<rect ", `<rect pointer-events="none" opacity="0.1" `)));
  const glow = [["flashlight beam", g - 20 * k], ["lantern", g], ["jar of fireflies", g + 40 * k]][n - 1];
  // (its light breathing with the slow LFO)
  s += SEP + obj(glow[0], time !== "day" ? Infinity : glow[1], part("tent-glow", [c.F, n, night, sceneLfoKey(o, "slow")], () => `<g>${sceneLfo(o, "slow", 0.3, "opacity", (v, amp) => (1 - (0.12 + 0.35 * amp) * (1 + v) / 2).toFixed(3))}` +
    TENT_GLOW[n - 1]({ k, P, R, back, half, out, y0, y1, g, W, H, night }) + `</g>`));
  return s;
}
// a sleeping bag along the tent, from by the far wall out toward us: its
// far end's two corners, and its color
// (its far end's height, its open end's, and the open end's two corners: for its drawing and its seat alike)
const bagEnds = ({ k, back, out, y1 }, xa, xb) => { const fy = back + 8 * k, ny = y1 - 6 * k; return { fy, ny, na: out(xa, fy, ny)[0], nb: out(xb, fy, ny)[0] }; };
function sleepingBag(G, xa, xb, color) {
  const { k } = G, { fy, ny, na, nb } = bagEnds(G, xa, xb);
  const flap = (u) => [xa + (na - xa) * u, xb + (nb - xb) * u, fy + (ny - fy) * u];
  const [fa, fb, fyy] = flap(0.3);
  return `<path d="M${f1(xa)} ${f1(fy)} Q${f1((xa + xb) / 2)} ${f1(fy - 10 * k)} ${f1(xb)} ${f1(fy)} L${f1(nb)} ${f1(ny)} Q${f1((na + nb) / 2)} ${f1(ny + 14 * k)} ${f1(na)} ${f1(ny)} Z" ` +
         `fill="${color}" stroke="${darker(color)}" stroke-width="${f1(2.5 * k)}" stroke-linejoin="round"/>` +
         // the top folded back, lighter, and the zip down its side
         `<path d="M${f1(xa)} ${f1(fy)} Q${f1((xa + xb) / 2)} ${f1(fy - 10 * k)} ${f1(xb)} ${f1(fy)} L${f1(fb)} ${f1(fyy)} Q${f1((fa + fb) / 2)} ${f1(fyy + 8 * k)} ${f1(fa)} ${f1(fyy)} Z" fill="${mix(color, "#ffffff", 0.35)}"/>` +
         `<line x1="${f1(fb)}" y1="${f1(fyy)}" x2="${f1(nb)}" y2="${f1(ny)}" stroke="#d8d8d8" stroke-width="${f1(1.5 * k)}" stroke-dasharray="${f1(3 * k)} ${f1(2 * k)}"/>` +
         // a pillow at its far end
         `<ellipse cx="${f1((xa + xb) / 2)}" cy="${f1(fy + 2 * k)}" rx="${f1(Math.abs(xb - xa) * 0.38)}" ry="${f1(11 * k)}" fill="#f4f1ea" stroke="#c9c4b8" stroke-width="${f1(1.5 * k)}"/>`;
}
// where the orange tent's flashlight lies, on the far end of the purple sleeping bag, its lens (the yellow end) up and to the right,
// for the flashlight and its beam alike (ticket 306, Matthew: the beam came out of its tail)
const tentFlashlightAt = G => `translate(${f1(G.W * 0.27)} ${f1(G.g - 20 * G.k)}) rotate(-25) scale(${G.k.toFixed(3)})`;
// (the orange tent's two sleeping bags: each its far end's corners, as shares of the far wall's half width, and its color)
const TENT_BAGS = [["sleeping bag, red", -0.88, -0.42, "#c0392b"], ["sleeping bag, purple", 0.42, 0.88, "#7d3c98"]];
const TENT_THINGS = [
  // orange: two sleeping bags, a flashlight on one
  // (ticket 284: each tent's things a list of objects, [name, where ("floor": lying flat; else its depth, standing), drawing])
  G => [...TENT_BAGS.map(([name, a, b, color]) => [name, "floor", sleepingBag(G, G.half * a, G.half * b, color)]),
    ["flashlight", "floor", `<g transform="${tentFlashlightAt(G)}"><rect x="-24" y="-7" width="40" height="14" rx="4" fill="#3a3a3a"/>` +
    `<rect x="14" y="-10" width="12" height="20" rx="3" fill="#f2c12e"/><rect x="-14" y="-3" width="8" height="6" rx="1" fill="#e53935"/></g>`]],
  // blue: a teddy bear on a blanket, a pile of books
  G => {
    const { k, W, g } = G, bx = -W * 0.31, by = g - 8 * k, rx = W * 0.3, ry = g + 6 * k;
    const blanket = `<polygon points="${P2(bx - 70 * k, by + 20 * k)} ${P2(bx + 60 * k, by + 20 * k)} ${P2(bx + 80 * k, by + 70 * k)} ${P2(bx - 90 * k, by + 70 * k)}" fill="#e6b450"/>` +
            [0, 1, 2].map(j => `<line x1="${f1(bx - 76 * k + j * 3 * k)}" y1="${f1(by + 34 * k + j * 14 * k)}" x2="${f1(bx + 66 * k + j * 5 * k)}" y2="${f1(by + 34 * k + j * 14 * k)}" stroke="#c0392b" stroke-width="${f1(4 * k)}"/>`).join("");
    const teddy = `<g transform="translate(${f1(bx)} ${f1(by)}) scale(${k.toFixed(3)})">` +
         `<ellipse cx="-22" cy="34" rx="12" ry="9" fill="#8b5a2b"/><ellipse cx="22" cy="34" rx="12" ry="9" fill="#8b5a2b"/>` +
         `<ellipse cx="0" cy="12" rx="30" ry="28" fill="#a0693a"/><ellipse cx="0" cy="18" rx="16" ry="16" fill="#d9a066"/>` +
         `<ellipse cx="-30" cy="4" rx="9" ry="14" fill="#8b5a2b" transform="rotate(25 -30 4)"/><ellipse cx="30" cy="4" rx="9" ry="14" fill="#8b5a2b" transform="rotate(-25 30 4)"/>` +
         `<circle cx="-18" cy="-40" r="10" fill="#8b5a2b"/><circle cx="18" cy="-40" r="10" fill="#8b5a2b"/><circle cx="-18" cy="-40" r="5" fill="#d9a066"/><circle cx="18" cy="-40" r="5" fill="#d9a066"/>` +
         `<circle cx="0" cy="-24" r="24" fill="#a0693a"/><ellipse cx="0" cy="-16" rx="10" ry="8" fill="#d9a066"/><circle cx="0" cy="-19" r="3.5" fill="#2a1a10"/>` +
         `<circle cx="-9" cy="-29" r="3" fill="#2a1a10"/><circle cx="9" cy="-29" r="3" fill="#2a1a10"/><path d="M-4 -12 Q0 -9 4 -12" fill="none" stroke="#2a1a10" stroke-width="1.5"/></g>`;
    let books = "";
    [["#2e86de", 0], ["#e53935", -4], ["#27ae60", 3], ["#f2c12e", -2]].forEach(([col, dx], j) =>
      books += `<rect x="${f1(rx - 34 * k + dx * k)}" y="${f1(ry - j * 13 * k)}" width="${f1(68 * k)}" height="${f1(12 * k)}" rx="${f1(2 * k)}" fill="${col}" stroke="${darker(col)}" stroke-width="${f1(1.2 * k)}"/>` +
           `<line x1="${f1(rx - 30 * k + dx * k)}" y1="${f1(ry - j * 13 * k + 3 * k)}" x2="${f1(rx + 30 * k + dx * k)}" y2="${f1(ry - j * 13 * k + 3 * k)}" stroke="#fff8e7" stroke-width="${f1(1.2 * k)}"/>`);
    books += `<rect x="${f1(rx - 28 * k)}" y="${f1(ry - 64 * k)}" width="${f1(52 * k)}" height="${f1(12 * k)}" rx="${f1(2 * k)}" fill="#9b59b6" transform="rotate(-8 ${f1(rx)} ${f1(ry - 58 * k)})"/>`;
    return [["blanket", "floor", blanket], ["teddy bear", by + 42 * k, teddy], ["books", ry + 12 * k, books]];
  },
  // green: a map with a route to an X, binoculars on it, a backpack
  G => {
    const { k, W, g, P } = G, mx = -W * 0.3, my = g + 10 * k, px = W * 0.31, py = g - 30 * k;
    return [["map", "floor", `<polygon points="${P2(mx - 60 * k, my - 30 * k)} ${P2(mx + 56 * k, my - 30 * k)} ${P2(mx + 78 * k, my + 36 * k)} ${P2(mx - 82 * k, my + 36 * k)}" fill="#f1e2b6" stroke="#b89a5a" stroke-width="${f1(2 * k)}"/>` +
      `<path d="M${P(mx - 60 * k, my + 24 * k)} Q${P(mx - 20 * k, my - 30 * k)} ${P(mx + 8 * k, my + 4 * k)} T${P(mx + 50 * k, my - 14 * k)}" fill="none" stroke="#c0392b" stroke-width="${f1(2.5 * k)}" stroke-dasharray="${f1(5 * k)} ${f1(4 * k)}"/>` +
      `<path d="M${P(mx + 44 * k, my - 22 * k)} L${P(mx + 58 * k, my - 8 * k)} M${P(mx + 58 * k, my - 22 * k)} L${P(mx + 44 * k, my - 8 * k)}" stroke="#c0392b" stroke-width="${f1(3.5 * k)}" stroke-linecap="round"/>` +
      `<path d="M${P(mx - 40 * k, my - 18 * k)} q${f1(8 * k)} ${f1(-8 * k)} ${f1(16 * k)} 0 M${P(mx - 10 * k, my + 22 * k)} q${f1(8 * k)} ${f1(-8 * k)} ${f1(16 * k)} 0" fill="none" stroke="#7a8a5a" stroke-width="${f1(2 * k)}"/>` +
      ""], ["binoculars", "floor", `<g transform="translate(${f1(mx - 4 * k)} ${f1(my + 8 * k)}) scale(${k.toFixed(3)})"><circle cx="-12" cy="0" r="11" fill="#2a2a2a"/><circle cx="12" cy="0" r="11" fill="#2a2a2a"/>` +
      `<circle cx="-12" cy="0" r="6" fill="#6fa8dc"/><circle cx="12" cy="0" r="6" fill="#6fa8dc"/><rect x="-4" y="-5" width="8" height="10" fill="#444"/></g>`],
      ["backpack", py + 46 * k, `<g transform="translate(${f1(px)} ${f1(py)}) scale(${k.toFixed(3)})"><rect x="-38" y="-50" width="76" height="96" rx="20" fill="#8e5a2b" stroke="#5e3a1a" stroke-width="3"/>` +
      `<path d="M-38 -14 Q0 -40 38 -14 L38 -40 Q0 -64 -38 -40 Z" fill="#a8703a" stroke="#5e3a1a" stroke-width="3"/><rect x="-24" y="4" width="48" height="32" rx="8" fill="#a8703a" stroke="#5e3a1a" stroke-width="2.5"/>` +
      `<rect x="-6" y="-22" width="12" height="16" rx="3" fill="#f2c12e"/><path d="M-30 -48 Q-44 -70 -20 -66 M30 -48 Q44 -70 20 -66" fill="none" stroke="#5e3a1a" stroke-width="5"/></g>`]];
  },
];
// what glows in each tent, over the night: the flashlight's beam, the
// lantern, the jar of fireflies
const TENT_GLOW = [
  // (out of its lens, the yellow end, along its shaft, widening: drawn where the flashlight is, so the two turn together;
  // and where it meets the tent's side wall, an oval of light on the canvas, narrow across and tall, as the wall goes
  // away from us: the beam's end, seen from the side, brightest in the middle. Matthew: "project an oval up there,
  // stretched to look psuedo 3d")
  G => `<defs><radialGradient id="bg-tent-spot"><stop offset="0" stop-color="#fffbe0" stop-opacity="0.95"/><stop offset="0.6" stop-color="#fff3a8" stop-opacity="0.7"/>` +
       `<stop offset="1" stop-color="#fff3a8" stop-opacity="0"/></radialGradient></defs>` +
       `<g transform="${tentFlashlightAt(G)}"><polygon points="26,-9 26,9 186,60 186,-60" fill="#fff3a8" opacity="${G.night ? 0.35 : 0.15}" pointer-events="none"/>` +
       `<ellipse cx="186" cy="0" rx="24" ry="66" fill="url(#bg-tent-spot)" opacity="${G.night ? 0.8 : 0.45}" pointer-events="none"/></g>`,
  G => {
    const { k, W, y0, H } = G, lx = -W * 0.17, ly = y0 + H * 0.17;
    return `<line x1="${f1(lx)}" y1="${f1(y0 - 10 * k)}" x2="${f1(lx)}" y2="${f1(ly - 22 * k)}" stroke="#333" stroke-width="${f1(2 * k)}"/>` +
      `<defs><radialGradient id="bg-tent-glow"><stop offset="0" stop-color="#ffd27a" stop-opacity="0.7"/><stop offset="1" stop-color="#ffd27a" stop-opacity="0"/></radialGradient></defs>` +
      `<circle cx="${f1(lx)}" cy="${f1(ly)}" r="${f1((G.night ? 170 : 90) * k)}" fill="url(#bg-tent-glow)" opacity="${G.night ? 0.8 : 0.45}" pointer-events="none"/>` +
      `<g transform="translate(${f1(lx)} ${f1(ly)}) scale(${k.toFixed(3)})"><path d="M-14 -22 L14 -22 L10 -30 L-10 -30 Z" fill="#2a2a2a"/>` +
      `<rect x="-14" y="-22" width="28" height="38" rx="5" fill="#ffd27a" stroke="#2a2a2a" stroke-width="3"/><rect x="-16" y="16" width="32" height="7" rx="2" fill="#2a2a2a"/>` +
      `<path d="M0 -12 Q6 -2 0 8 Q-6 -2 0 -12 Z" fill="#ff8c1a"><animate attributeName="opacity" values="1;0.7;1" dur="0.8s" repeatCount="indefinite"/></path></g>`;
  },
  G => {
    const { k, W, g, R } = G, jx = W * 0.2, jy = g + 40 * k;
    let t = `<defs><radialGradient id="bg-jar-glow"><stop offset="0" stop-color="#fff36a" stop-opacity="0.7"/><stop offset="1" stop-color="#fff36a" stop-opacity="0"/></radialGradient></defs>` +
            `<ellipse cx="${f1(jx)}" cy="${f1(jy - 26 * k)}" rx="${f1((G.night ? 110 : 50) * k)}" ry="${f1((G.night ? 90 : 42) * k)}" fill="url(#bg-jar-glow)" opacity="${G.night ? 0.85 : 0.4}" pointer-events="none"/>` +
            `<rect x="${f1(jx - 22 * k)}" y="${f1(jy - 60 * k)}" width="${f1(44 * k)}" height="${f1(60 * k)}" rx="${f1(10 * k)}" fill="#cfe9f7" opacity="0.55" stroke="#9cc3d8" stroke-width="${f1(2 * k)}"/>` +
            `<rect x="${f1(jx - 24 * k)}" y="${f1(jy - 68 * k)}" width="${f1(48 * k)}" height="${f1(10 * k)}" rx="${f1(3 * k)}" fill="#8a8a8a"/>`;
    for (let i = 0; i < 7; i++) {
      const fx = jx + (R(i, 376) - 0.5) * 30 * k, fy = jy - (8 + 44 * R(i, 377)) * k;
      t += `<circle cx="${f1(fx)}" cy="${f1(fy)}" r="${f1(2.6 * k)}" fill="#fff36a"><animate attributeName="opacity" values="1;0.15;1" dur="${(1 + R(i, 378)).toFixed(2)}s" repeatCount="indefinite"/>` +
           `<animate attributeName="cy" values="${f1(fy)};${f1(fy - 6 * k)};${f1(fy)}" dur="${(2 + R(i, 379)).toFixed(2)}s" repeatCount="indefinite"/></circle>`;
    }
    return t;
  },
];

// The cave behind the waterfall: rock all round, stalactites dripping,
// crystals glowing on the walls, glowworms twinkling on the roof, bats
// asleep upside down and an old chest; and at its mouth the falls seen
// from behind, as wide and as misty as the waterfall's own sliders make
// them, the light coming through: a doorway back out.
function caveScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, o } = c;
  const fw = (o.fallWidth ?? 100) / 100, mist = (o.mist ?? 60) / 100, rate = fallRateOf(o), time = fallTimeOf(o);
  const floor = g - 50 * k, mt = y0 + H * 0.1, mh = W * 0.24, cw = Math.min(mh, 70 * fw * k);
  const mouth = `M${P(-mh, floor)} L${P(-mh * 0.96, mt + 90 * k)} Q${P(-mh * 0.75, mt)} ${P(0, mt - 8 * k)} Q${P(mh * 0.75, mt)} ${P(mh * 0.96, mt + 90 * k)} L${P(mh, floor)} Z`;
  let s = SEP + part("cave-rock", c.F, () => {
    let t = all("#2a2630");
    for (let i = 0; i < 60; i++) {
      const x = x0 + W * R(i, 380), y = y0 + H * R(i, 381), r = (20 + 50 * R(i, 382)) * k;
      t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(r)}" ry="${f1(r * 0.6)}" fill="${R(i, 383) < 0.5 ? "#35303c" : "#221e27"}" opacity="0.8"/>`;
    }
    return t;
  });
  // the mouth: the light outside, the jungle blurred beyond, the curtain
  // of water falling across it, mist at its foot
  const clock = o.fallTime === "clock", moonUp = clock && skyNow(o).moon.alt > 0;   // (ticket 490: by the clock, the moon in its phase, when it is up)
  s += SEP + part("cave-mouth", [c.F, fw, mist, rate, time, PERF.blur, clock ? skyKey(o) : ""], () => {
    // (the light outside, by the falls' time of day)
    const sky = { day: ["#effcff", "#a9dcc6"], sunset: ["#ffe0b0", "#e8946a"], night: ["#4a5c88", "#1c2742"] }[time];
    let t = `<defs><clipPath id="bg-cave-mouth"><path d="${mouth}"/></clipPath>${grad("bg-cave-light", sky[0], sky[1], mt, floor)}` +
            `<linearGradient id="bg-cave-water" x1="0" y1="0" x2="1" y2="0"><stop offset="0" stop-color="#9ad7f0"/><stop offset="0.5" stop-color="#e6f8ff"/><stop offset="1" stop-color="#9ad7f0"/></linearGradient>` +
            (PERF.blur ? `<filter id="bg-cave-blur" x="-50%" y="-50%" width="200%" height="200%"><feGaussianBlur stdDeviation="${f1(10 * k)}"/></filter>` : "") + `</defs>` +
            `<g class="portal" data-to="waterfall"><g clip-path="url(#bg-cave-mouth)">` +
            `<rect x="${f1(-mh)}" y="${f1(mt - 20 * k)}" width="${f1(2 * mh)}" height="${f1(floor - mt + 20 * k)}" fill="url(#bg-cave-light)"/>` +
            // (ticket 155: with blurs off, the leaves outside are fainter, and sharp)
            (PERF.blur ? `<g filter="url(#bg-cave-blur)" opacity="0.6">` : `<g opacity="0.35">`) + [...Array(8)].map((_, i) => `<circle cx="${f1((R(i, 384) - 0.5) * 2 * mh)}" cy="${f1(mt + (floor - mt) * (0.2 + 0.6 * R(i, 385)))}" r="${f1((30 + 30 * R(i, 386)) * k)}" fill="${i % 2 ? "#3f8050" : "#5f9a6a"}"/>`).join("") + `</g>` +
            `<rect x="${f1(-mh)}" y="${f1(floor - 30 * k)}" width="${f1(2 * mh)}" height="${f1(30 * k)}" fill="#62b6e4" opacity="0.7"/>` +
            `<path d="M${P(-cw, mt - 20 * k)} L${P(cw, mt - 20 * k)} L${P(cw * 1.15, floor)} L${P(-cw * 1.15, floor)} Z" fill="url(#bg-cave-water)" opacity="0.85"/>`;
    const n = Math.max(4, Math.round(10 * fw));
    for (let i = 0; i < n; i++) {
      const u = (i + 0.5) / n, xa = -cw + 2 * cw * u, xb = -cw * 1.15 + 2.3 * cw * u, dash = (30 + 30 * R(i, 387)) * k, gap = (20 + 20 * R(i, 388)) * k;
      t += `<line x1="${f1(xa)}" y1="${f1(mt - 20 * k)}" x2="${f1(xb)}" y2="${f1(floor)}" stroke="#ffffff" stroke-width="${f1((2 + 3 * R(i, 389)) * k)}" stroke-dasharray="${f1(dash)} ${f1(gap)}" opacity="0.75">` +
           `<animate attributeName="stroke-dashoffset" values="0;${f1(-(dash + gap) * 2)}" dur="${((0.6 + 0.4 * R(i, 390)) / rate).toFixed(2)}s" repeatCount="indefinite"/></line>`;
    }
    // (ticket 155: with blurs off, each puff a radial gradient, as the waterfall's mist)
    if (mist > 0) t += (PERF.blur ? `<g filter="url(#bg-cave-blur)">` : `<defs><radialGradient id="bg-cave-mist-soft"><stop offset="0.35" stop-color="#ffffff"/><stop offset="1" stop-color="#ffffff" stop-opacity="0"/></radialGradient></defs><g>`) + [...Array(Math.round(2 + 5 * mist))].map((_, i) => {
      const x = (R(i, 391) - 0.5) * 2 * mh, y = floor - (10 + 30 * R(i, 392)) * k, r = (30 + 30 * R(i, 393)) * k;
      return `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(r)}" ry="${f1(r * 0.5)}" fill="${PERF.blur ? "#ffffff" : "url(#bg-cave-mist-soft)"}" opacity="${(0.25 + 0.4 * mist).toFixed(2)}">` +
             `<animate attributeName="cy" values="${f1(y)};${f1(y - 20 * k)};${f1(y)}" dur="${(4 + 3 * R(i, 394)).toFixed(1)}s" repeatCount="indefinite"/></ellipse>`;
    }).join("") + `</g>`;
    // (outside, the falls' time of day: its tint over all of it, the moon behind the water at night)
    if (time !== "day") t += (clock ? (moonUp ? moonPhaseSvg(-mh * 0.45, mt + 50 * k, 18 * k, skyNow(o).moon.phase, "#1c2742", skyFacesNorth(skyNow(o))) : "") : time === "night" ? `<circle cx="${f1(-mh * 0.45)}" cy="${f1(mt + 50 * k)}" r="${f1(18 * k)}" fill="#fff6d6"/>` : "") +
      `<rect x="${f1(-mh)}" y="${f1(mt - 20 * k)}" width="${f1(2 * mh)}" height="${f1(floor - mt + 20 * k)}" fill="${FALL_TINT[time][0]}" opacity="${FALL_TINT[time][1]}" pointer-events="none"/>`;
    return t + `</g><path d="${mouth}" fill="none" stroke="#1a171f" stroke-width="${f1(8 * k)}" stroke-linejoin="round"/></g>`;
  });
  // (the walls' gems and crystals are drawn before the floor and what
  // stands on it, as the back wall they are on: ticket 99)
  // gems all over the walls (not across the mouth, or on the floor), each
  // with its sparkle, drawn as the night sky's stars are and set the same
  // way (gemShape, gemSize, gemBright, gemTwinkle); and sparkles on the crystals
  const so = { starShape: o.gemShape, starSize: o.gemSize, starBright: o.gemBright, starTwinkle: o.gemTwinkle, lfoFast: o.lfoFast }, gems = Math.max(0, Math.round(o.caveGems ?? 16));
  // (ticket 284: the gems one wall object, each glowing crystal cluster one, the light in from the falls, the wet floor and the pebbles)
  s += SEP + wallObj("gems", part("cave-gems", [c.F, gems, o.gemShape, o.gemSize, o.gemBright, o.gemTwinkle, sceneLfoKey(o, "fast")], () => {
    let t = "", i = 0;
    for (let j = 0; i < gems && j < gems * 12; j++) {
      const x = x0 + W * (0.03 + 0.94 * R(j, 1210)), y = y0 + H * 0.05 + (floor - y0 - H * 0.12) * R(j, 1211);
      if (Math.abs(x) < mh * 1.08 && y > mt - 30 * k) continue;
      const col = ["#b388ff", "#5ee6ff", "#7dfcd2", "#ffd36e", "#ff8fb1"][j % 5], z = (0.5 + 0.6 * R(j, 1212)) * k, a = f1((R(j, 1213) - 0.5) * 70);
      t += `<polygon points="-5,0 -5,-12 0,-18 5,-12 5,0" fill="${col}" stroke="${mix(col, "#ffffff", 0.5)}" stroke-width="1.2" transform="translate(${f1(x)} ${f1(y)}) rotate(${a}) scale(${z.toFixed(3)})"/>` +
           starsSvg(so, 1, x, x, y - 12 * z, y - 12 * z, k, (n, seed) => R(j, seed), 1220);
      i++;
    }
    return t + [[-0.4, 0.5], [0.37, 0.42], [-0.3, 0.2]].map(([u, v], j) => starsSvg(so, 4, W * u - 30 * k, W * u + 30 * k, y0 + H * v - 10 * k, y0 + H * v + 30 * k, k, (n, seed) => R(n + 10 * j, seed), 1240)).join("");
  }));
  // crystals glowing on the walls, either side
  s += SEP + part("cave-crystals", [c.F, sceneLfoKey(o, "slow")], () => [[-0.4, 0.5, "#b388ff"], [0.37, 0.42, "#5ee6ff"], [-0.3, 0.2, "#5ee6ff"]].map(([u, v, col], j) => {
    const cx = W * u, cy = y0 + H * v;
    return wallObj(`crystals ${j + 1}`, `<ellipse cx="${f1(cx)}" cy="${f1(cy)}" rx="${f1(50 * k)}" ry="${f1(40 * k)}" fill="${col}" opacity="0.25">${lfoTimed(o, "slow", j / 3, `<animate attributeName="opacity" values="0.15;0.4;0.15" dur="${3 + j}s" repeatCount="indefinite"/>`)}</ellipse>` +
      [[-16, 0, 40, -20], [0, 4, 56, 0], [16, 2, 36, 22], [-6, 8, 28, -40], [10, 8, 26, 40]].map(([dx, dy, h, a]) =>
        `<polygon points="-6,0 -6,${-h + 8} 0,${-h} 6,${-h + 8} 6,0" fill="${col}" stroke="${mix(col, "#ffffff", 0.5)}" stroke-width="1.5" ` +
        `transform="translate(${f1(cx + dx * k)} ${f1(cy + 30 * k + dy * k)}) rotate(${a}) scale(${k.toFixed(3)})"/>`).join(""));
  }).join(""));
  // (ticket 471) at the back, left of the mouth (the old chest stands on the right): a dark archway, deeper in, to the 🦇
  // cave room (drawn as the cave room draws its way back here, caveArchSvg)
  s += SEP + part("cave-deeper", c.F, () => wallObj("archway to the cave room", `<g class="portal" data-to="caveroom">` +
    caveArchSvg(c, "bg-cave-deeper", -W * 0.29, floor + 4 * k, 64 * k, 118 * k, 4711, "#7a6a9a") + `</g>`));
  // the light falling in through the water, the floor, wet by the mouth
  s += SEP + part("cave-floor", c.F, () => {
    let t = `<path d="M${P(x0 - pad, floor + 10 * k)} Q${P(-W * 0.3, floor - 6 * k)} ${P(-mh, floor)} L${P(mh, floor)} Q${P(W * 0.3, floor - 6 * k)} ${P(x1 + pad, floor + 10 * k)} L${P(x1 + pad, y1 + pad)} L${P(x0 - pad, y1 + pad)} Z" fill="#3b3542"/>` +
            // (ticket 471, Matthew: "look at, and refine, the waterfall cave") its floor worn round, ledge after ledge, as the cave room's
            floorObj("the floor's ledges", caveLedgesSvg(c, floor + 4 * k, y1 - 8 * k, y => Math.max(0.45, Math.min(1.35, 0.5 + 0.9 * (y - floor) / (y1 - floor))))) +
            floorObj("wet floor", `<ellipse cx="0" cy="${f1(floor + 26 * k)}" rx="${f1(mh * 1.3)}" ry="${f1(22 * k)}" fill="#7fa3bd" opacity="0.25"/>`);
    let light = "", pebbles = "";
    for (let i = 0; i < 3; i++)
      light += `<polygon points="${P2(-cw * (0.8 - 0.6 * i), mt + 120 * k)} ${P2(-cw * (0.5 - 0.6 * i), mt + 120 * k)} ${P2(-W * (0.18 - 0.2 * i), y1 + pad)} ${P2(-W * (0.3 - 0.2 * i), y1 + pad)}" fill="#d8f4ff" opacity="0.08" pointer-events="none"/>`;
    for (let i = 0; i < 24; i++) {
      const x = x0 + W * R(i, 395), y = floor + 20 * k + (y1 - floor) * R(i, 396), r = (4 + 10 * R(i, 397)) * k;
      pebbles += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(r)}" ry="${f1(r * 0.55)}" fill="${["#4a4452", "#2e2934", "#57505f"][i % 3]}"/>`;
    }
    t += wallObj("light from the falls", light) + floorObj("pebbles", pebbles);
    // stalagmites at the sides, each an object standing at its foot
    for (const [u, h, w, name] of [[-0.44, 90, 30, "big stalagmite, left"], [-0.36, 50, 20, "small stalagmite, left"], [0.4, 110, 34, "big stalagmite, right"], [0.47, 60, 22, "small stalagmite, right"]]) {
      const x = W * u, b = floor + 30 * k;
      t += obj(name, b, `<path d="M${P(x - w * k, b)} Q${P(x - w * 0.3 * k, b - h * 0.6 * k)} ${P(x, b - h * k)} Q${P(x + w * 0.3 * k, b - h * 0.6 * k)} ${P(x + w * k, b)} Z" fill="#4a4352" stroke="#2a2630" stroke-width="${f1(2 * k)}"/>`);
    }
    return t;
  });
  // the roof: stalactites, a drop falling now and then, glowworms (one
  // object, "stalactites", the glowworms' light in it: they hang over the
  // stalagmites, which grow where their drops land, so they take the
  // stalagmites' depth)
  s += SEP + part("cave-roof", c.F, () => {
    let st = "";
    for (let i = 0; i < 16; i++) {
      const x = x0 + W * (i + 0.2 + 0.6 * R(i, 398)) / 16, len = (30 + 90 * R(i, 399)) * k, w = (10 + 16 * R(i, 400)) * k;
      if (Math.abs(x) < mh * 0.8) continue;
      st += `<path d="M${P(x - w, y0 - 20 * k)} L${P(x + w, y0 - 20 * k)} Q${P(x + w * 0.3, y0 + len * 0.6)} ${P(x, y0 + len)} Q${P(x - w * 0.3, y0 + len * 0.6)} ${P(x - w, y0 - 20 * k)} Z" fill="#4a4352"/>` +
           `<path d="M${P(x - w * 0.5, y0)} Q${P(x - w * 0.2, y0 + len * 0.6)} ${P(x, y0 + len)}" fill="none" stroke="#6a6274" stroke-width="${f1(2 * k)}"/>`;
      if (i % 5 === 1) st += `<circle cx="${f1(x)}" cy="${f1(y0 + len)}" r="${f1(2.5 * k)}" fill="#9ad7f0"><animate attributeName="cy" values="${f1(y0 + len)};${f1(floor + 20 * k)}" dur="1.2s" begin="${(i * 0.7).toFixed(1)}s" repeatCount="indefinite"/>` +
                            `<animate attributeName="opacity" values="1;1;0" dur="1.2s" begin="${(i * 0.7).toFixed(1)}s" repeatCount="indefinite"/></circle>`;
    }
    for (let i = 0; i < 40; i++) {
      const x = x0 + W * R(i, 401), y = y0 + H * 0.28 * R(i, 402);
      if (Math.abs(x) < mh && y > mt) continue;
      st += `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(1.6 * k)}" fill="#7dfcd2"${i % 3 ? "" : `><animate attributeName="opacity" values="1;0.2;1" dur="${(1.5 + 2 * R(i, 403)).toFixed(1)}s" repeatCount="indefinite"/></circle`}${i % 3 ? "/>" : ">"}`;
    }
    return obj("stalactites", floor + 30 * k, st);
  });
  // bats asleep, hanging from the roof; an old chest by the wall (two
  // objects: the bats among the stalactites, at their depth; the chest
  // standing on the floor)
  s += SEP + part("cave-bats-chest", c.F, () => obj("bats", floor + 30 * k, [[-0.3, 26], [-0.22, 12], [0.32, 18]].map(([u, dy], i) => caveBatSvg(W * u, y0 + dy * k, k, i)).join("")) + obj("chest", floor + 50 * k, `<g transform="translate(${f1(W * 0.3)} ${f1(floor + 50 * k)}) scale(${k.toFixed(3)})"><rect x="-46" y="-40" width="92" height="50" rx="4" fill="#8b5a2b" stroke="#4a2e1a" stroke-width="3"/>` +
    `<path d="M-46 -40 Q0 -76 46 -40 Z" fill="#a0693a" stroke="#4a2e1a" stroke-width="3"/><rect x="-50" y="-20" width="100" height="8" fill="#f2c12e"/><rect x="-8" y="-28" width="16" height="20" rx="3" fill="#f2c12e" stroke="#a07a10" stroke-width="2"/>` +
    [[-60, 12], [-50, 18], [54, 14], [62, 8]].map(([x, y]) => `<ellipse cx="${x}" cy="${y}" rx="7" ry="4" fill="#f2c12e" stroke="#a07a10" stroke-width="1.5"/>`).join("") + `</g>`));
  return s;
}

// (ticket 471) a bat asleep, hanging from the roof at (bx, by), swaying a little (i: its own pace): the cave's, and the cave room's
const caveBatSvg = (bx, by, k, i) => `<g transform="translate(${f1(bx)} ${f1(by)}) scale(${k.toFixed(3)})"><g><animateTransform attributeName="transform" type="rotate" values="-6;6;-6" dur="${4 + i}s" repeatCount="indefinite"/>` +
  `<line x1="0" y1="0" x2="0" y2="8" stroke="#111" stroke-width="2"/><path d="M0 6 Q-14 12 -10 34 Q0 44 10 34 Q14 12 0 6 Z" fill="#1a1620"/>` +
  `<path d="M-6 40 L-9 50 L-2 43 Z M6 40 L9 50 L2 43 Z" fill="#1a1620"/><circle cx="-3" cy="38" r="1.5" fill="#ffffff"/><circle cx="3" cy="38" r="1.5" fill="#ffffff"/></g></g>`;
// (ticket 471) A fractal edge from a to b: each piece's middle pushed out across it, by up to `rough` of its length, the
// roughness falling by about half each round (seeded, R(i, seed)); its points, for caveSmooth to draw round
function caveFractal(a, b, rounds, rough, R, seed) {
  let pts = [a, b];
  for (let d = 0; d < rounds; d++, rough *= 0.55) {
    const out = [pts[0]];
    for (let i = 1; i < pts.length; i++) {
      const [ax, ay] = pts[i - 1], [bx, by] = pts[i], L = Math.hypot(bx - ax, by - ay) || 1, off = (R(i + 64 * d, seed) - 0.5) * 2 * rough * L;
      out.push([(ax + bx) / 2 - (by - ay) / L * off, (ay + by) / 2 + (bx - ax) / L * off], pts[i]);
    }
    pts = out;
  }
  return pts;
}
// (a line through several points, each piece fractal: the points of all of them, in order)
const caveFractalPath = (pts, rounds, rough, R, seed) => pts.slice(1).flatMap((p, i) => caveFractal(pts[i], p, rounds, rough, R, seed + 7 * i).slice(i ? 1 : 0));
// (through points, worn round: a curve to each midpoint, the points its controls)
const caveSmooth = (pts, P) => pts.slice(1, -1).reduce((d, p, i) => d + ` Q${P(p[0], p[1])} ${P((p[0] + pts[i + 2][0]) / 2, (p[1] + pts[i + 2][1]) / 2)}`, `M${P(pts[0][0], pts[0][1])}`) +
  ` L${P(pts[pts.length - 1][0], pts[pts.length - 1][1])}`;
// (the height of a line of points at x, between the two it falls between)
const caveAlong = (pts, x) => { for (let i = 1; i < pts.length; i++) { const [ax, ay] = pts[i - 1], [bx, by] = pts[i];
  if ((x - ax) * (x - bx) <= 0) return bx === ax ? ay : ay + (by - ay) * (x - ax) / (bx - ax); } return pts[x < pts[0][0] ? 0 : pts.length - 1][1]; };
// (ticket 471) A natural archway into the rock, its foot at (x, foot), w wide and h high, its rim worn round and uneven
// (seed); dark inside, a little light far in (glow): the way between the cave behind the falls and the cave room, drawn
// the same way at both ends
function caveArchSvg(c, id, x, foot, w, h, seed, glow) {
  const { k, R, P } = c, pts = [];
  for (let i = 0; i <= 20; i++) {
    const a = Math.PI * (1 - i / 20), b = 1 + 0.12 * (R(i, seed) - 0.5) + 0.06 * (R(2 * i, seed + 1) - 0.5);
    pts.push([x + w / 2 * Math.cos(a) * b, foot - h * Math.pow(Math.sin(a), 0.55) * (i % 20 ? b : 1)]);
  }
  const d = caveSmooth(pts, P) + " Z";
  return `<defs><radialGradient id="${id}" cx="0.5" cy="0.75" r="0.65"><stop offset="0" stop-color="${glow}" stop-opacity="0.6"/><stop offset="0.5" stop-color="#110e15"/>` +
    `<stop offset="1" stop-color="#050407"/></radialGradient></defs><path d="${d}" fill="url(#${id})"/>` +
    `<path d="${d}" fill="none" stroke="#0a080c" stroke-width="${f1(3 * k)}" stroke-linejoin="round"/>` +
    `<path d="${caveSmooth(pts.slice(2, 19), P)}" fill="none" stroke="#6a6276" stroke-width="${f1(4 * k)}" stroke-linecap="round" opacity="0.7" transform="translate(${f1(-2 * k)} ${f1(-3 * k)})"/>`;
}
// (ticket 471) a cave's floor worn round and fractal: ledges after ledges toward us, from its far edge (back) to near, each
// a fractal line, lit on top and shadowed below; and bumps of three sizes on it; sized for their depth by persp(y)
function caveLedgesSvg(c, back, near, persp) {
  const { x0, x1, W, k, R, P } = c;
    let ledges = "";
    for (let i = 0; i < 6; i++) {
      const v = Math.pow((i + 0.6) / 6, 1.35), y = back + (near - back) * v, a = x0 - 30 * k + 0.1 * W * R(i, 4775), b = x1 + 30 * k - 0.1 * W * R(i, 4776);
      const pts = caveFractalPath([[a, y + 18 * k * R(i, 4777)], [x0 + W * (0.3 + 0.1 * R(i, 4778)), y - 10 * k * persp(y)], [x1 - W * (0.3 + 0.1 * R(i, 4779)), y + 6 * k * persp(y)], [b, y - 14 * k * R(i, 4780)]], 4, 0.04, R, 4781 + 13 * i);
      ledges += `<path d="${caveSmooth(pts, P)}" fill="none" stroke="#211d27" stroke-width="${f1(5 * k * persp(y))}" stroke-linecap="round" opacity="0.6" transform="translate(0 ${f1(2.5 * k * persp(y))})"/>` +
                `<path d="${caveSmooth(pts, P)}" fill="none" stroke="#6b6278" stroke-width="${f1(2.5 * k * persp(y))}" stroke-linecap="round" opacity="0.55"/>`;
    }
    let bumps = "";
    for (let j = 0; j < 3; j++) for (let i = 0; i < [10, 22, 44][j]; i++) {
      const y = back + 8 * k + (near - back) * Math.pow(R(i, 4800 + j), 0.8), x = x0 + W * R(i, 4803 + j), z = persp(y), rx = [58, 28, 12][j] * k * z * (0.7 + 0.6 * R(i, 4806 + j)), ry = rx * 0.26;
      bumps += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(rx)}" ry="${f1(ry)}" fill="#5a5266" opacity="0.4"/>` +
               `<path d="M${P(x - rx, y)} Q${P(x, y + ry * 2.2)} ${P(x + rx, y)}" fill="none" stroke="#221e28" stroke-width="${f1(Math.max(1, ry * 0.5))}" opacity="0.45"/>`;
    }
    return ledges + bumps;
}
// (ticket 471, Matthew: "a background that represents being in a cave room, rounded, fractal structure on the ground
// (walkable) suface. Different cave rock formations (6 styles for floor, and 6 styles for ceiling) --- look at, and refine,
// the waterfall cave") The 🦇 cave room, deeper in behind the falls: in through a dark archway at the back of the cave,
// and back out through its own. Its floor a rounded, fractal surface of worn rock, ledge over ledge, walkable all over
// (what stands on it is what stops the feet); its vault a fractal arch, its back wall lumpy rock, glowing here and there.
// Six styles of formations on the floor (caveFloor), each standing at its depth (the stalagmites, boulders, columns,
// crystal clusters and mushrooms stop the feet, as STANDING says; one sits on a boulder or a mushroom as on any; the
// rimstone pools lie flat), and six on the ceiling (caveCeiling). Whatever the styles: drops falling into a puddle (the
// fast LFO), the glow breathing (the slow one), two bats asleep.
const CAVE_FLOORS = [["stalagmites", "🗼 stalagmites"], ["boulders", "🪨 boulders"], ["columns", "🏛️ columns, floor to ceiling"],
                     ["rimstone", "💧 rimstone pools"], ["crystals", "💎 crystal clusters"], ["mushrooms", "🍄 glowing mushrooms"]];
const CAVE_CEILINGS = [["stalactites", "🦷 stalactites"], ["straws", "🥢 soda straws"], ["curtains", "🎐 curtains of rock"],
                       ["crack", "☀️ a crack of daylight"], ["crystals", "💎 crystals"], ["glowworms", "🐛 glowworms"]];
const CAVE_GLOWS = ["#b388ff", "#5ee6ff", "#7dfcd2", "#ffd36e", "#ff8fb1"];
function caveroomScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, o } = c;
  const floorSty = CAVE_FLOORS.some(([v]) => v === o.caveFloor) ? o.caveFloor : "stalagmites";
  const roofSty = CAVE_CEILINGS.some(([v]) => v === o.caveCeiling) ? o.caveCeiling : "stalactites";
  const back = g - 135 * k, line = g - 125 * k, near = y1 - 10 * k;   // (the floor's far edge in the middle; just in front of it, the back line, SCENE_FAR's)
  // the vault, from wall to wall over the room, and the floor's far edge, rising round to meet it at either side
  const vault = caveFractalPath([[x0 - 40 * k, g - 230 * k], [x0 + 0.12 * W, y0 + 120 * k], [x0 + 0.34 * W, y0 + 62 * k], [x1 - 0.3 * W, y0 + 56 * k], [x1 - 0.1 * W, y0 + 130 * k], [x1 + 40 * k, g - 220 * k]], 4, 0.07, R, 4720);
  const edge = caveFractalPath([[x0 - 40 * k, g - 230 * k], [x0 + 0.1 * W, back - 40 * k], [x0 + 0.3 * W, back - 6 * k], [x1 - 0.32 * W, back - 2 * k], [x1 - 0.1 * W, back - 34 * k], [x1 + 40 * k, g - 220 * k]], 4, 0.05, R, 4740);
  const persp = y => Math.max(0.45, Math.min(1.35, 0.5 + 0.9 * (y - back) / (near - back)));
  const fast = (off, own) => lfoTimed(o, "fast", off, own), slow = (off, own) => lfoTimed(o, "slow", off, own);
  // the back wall: rock, lump over lump, three sizes of them (the fractal look), and its glows breathing (the slow LFO)
  let s = SEP + part("caveroom-wall", [c.F, sceneLfoKey(o, "slow")], () => {
    let t = `<defs>${grad("bg-caveroom-wall", "#3a3344", "#2b2632", y0, back)}</defs>` + all("url(#bg-caveroom-wall)");
    for (let j = 0; j < 3; j++) for (let i = 0; i < [12, 26, 50][j]; i++) {
      const r = [64, 30, 14][j] * k * (0.7 + 0.6 * R(i, 4750 + j)), x = x0 + W * R(i, 4753 + j), y = y0 + (back - y0) * R(i, 4756 + j);
      t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(r)}" ry="${f1(r * 0.72)}" fill="${R(i, 4759 + j) < 0.5 ? "#433b4e" : "#2a2530"}" opacity="0.7"/>` +
           `<path d="M${P(x - r * 0.7, y - r * 0.2)} Q${P(x - r * 0.4, y - r * 0.68)} ${P(x + r * 0.2, y - r * 0.66)}" fill="none" stroke="#5a5266" stroke-width="${f1(Math.max(1, r * 0.08))}" stroke-linecap="round" opacity="0.6"/>`;
    }
    // glowing patches on the wall (lichen, or a seam of something bright), each breathing in its own time
    const glows = [[-0.36, 0.42, 0], [0.12, 0.3, 1], [0.38, 0.5, 2], [-0.08, 0.62, 3]].map(([u, v, j]) => {
      const x = W * u, y = y0 + (back - y0) * v, r = (46 + 20 * R(j, 4762)) * k, col = CAVE_GLOWS[(j * 2 + 1) % 5];
      return `<defs><radialGradient id="bg-caveroom-glow-${j}"><stop offset="0" stop-color="${col}" stop-opacity="0.75"/><stop offset="1" stop-color="${col}" stop-opacity="0"/></radialGradient></defs>` +
             `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(r)}" ry="${f1(r * 0.7)}" fill="url(#bg-caveroom-glow-${j})" opacity="0.45">` +
             slow(j / 4, `<animate attributeName="opacity" values="0.25;0.7;0.25" dur="${(4 + j).toFixed(1)}s" repeatCount="indefinite"/>`) + `</ellipse>` +
             [...Array(7)].map((_, n) => `<circle cx="${f1(x + (R(n + 9 * j, 4763) - 0.5) * r * 1.3)}" cy="${f1(y + (R(n + 9 * j, 4764) - 0.5) * r * 0.8)}" r="${f1((1.4 + 1.6 * R(n + 9 * j, 4765)) * k)}" fill="${col}"/>`).join("");
    }).join("");
    return t + wallObj("glowing patches", glows);
  });
  // the way back: a dark archway at the back, on the left, to the 🪨 cave behind the falls (lit far off by the falls)
  s += SEP + part("caveroom-arch", c.F, () => wallObj("archway to the cave", `<g class="portal" data-to="cave">` +
    caveArchSvg(c, "bg-caveroom-arch", -0.3 * W, back - 2 * k, 92 * k, 150 * k, 4770, "#9ad7f0") + `</g>`));
  // the floor: worn rock, rounded and fractal, ledge over ledge toward us, bumps of three sizes on it, pebbles; walkable
  s += SEP + part("caveroom-floor", c.F, () => {
    let t = `<defs>${grad("bg-caveroom-floor", "#2e2934", "#4b4455", back - 60 * k, y1)}</defs>` +
            `<path d="${caveSmooth(edge, P)} L${P(x1 + pad, y1 + pad)} L${P(x0 - pad, y1 + pad)} Z" fill="url(#bg-caveroom-floor)"/>` +
            `<path d="${caveSmooth(edge, P)}" fill="none" stroke="#1c1922" stroke-width="${f1(4 * k)}" stroke-linejoin="round"/>`;
    let pebbles = "";
    for (let i = 0; i < 30; i++) {
      const y = back + 10 * k + (near - back) * R(i, 4810), x = x0 + W * R(i, 4811), r = (2.5 + 6 * R(i, 4812)) * k * persp(y);
      pebbles += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(r)}" ry="${f1(r * 0.55)}" fill="${["#5c5468", "#2e2934", "#6a6276"][i % 3]}"/>`;
    }
    return t + floorObj("the floor's ledges", caveLedgesSvg(c, back, near, persp)) + floorObj("pebbles", pebbles);
  });
  // drops falling from the vault into a puddle, rings spreading on it (the fast LFO)
  s += SEP + part("caveroom-drips", [c.F, sceneLfoKey(o, "fast")], () => {
    const px = -0.1 * W, py = g + 30 * k, top = caveAlong(vault, px) + 30 * k;
    let t = floorObj("puddle", `<ellipse cx="${f1(px)}" cy="${f1(py)}" rx="${f1(40 * k)}" ry="${f1(10 * k)}" fill="#6fb6cf" opacity="0.4"/>` +
      `<ellipse cx="${f1(px - 8 * k)}" cy="${f1(py - 2 * k)}" rx="${f1(14 * k)}" ry="${f1(2.5 * k)}" fill="#d8f4ff" opacity="0.35"/>` +
      [0, 1].map(j => `<ellipse cx="${f1(px)}" cy="${f1(py)}" rx="${f1(4 * k)}" ry="${f1(1 * k)}" fill="none" stroke="#d8f4ff" stroke-width="${f1(1.2 * k)}" opacity="0">` +
        fast(0.12 + j * 0.2, `<animate attributeName="rx" values="${f1(4 * k)};${f1(34 * k)}" dur="1.6s" repeatCount="indefinite"/>`) +
        fast(0.12 + j * 0.2, `<animate attributeName="ry" values="${f1(1 * k)};${f1(8.5 * k)}" dur="1.6s" repeatCount="indefinite"/>`) +
        fast(0.12 + j * 0.2, `<animate attributeName="opacity" values="0.8;0" dur="1.6s" repeatCount="indefinite"/>`) + `</ellipse>`).join(""));
    t += obj("drops", py, `<circle cx="${f1(px)}" cy="${f1(top)}" r="${f1(2.6 * k)}" fill="#bfeaff">` +
      fast(0, `<animate attributeName="cy" values="${f1(top)};${f1(py)}" dur="1.6s" repeatCount="indefinite"/>`) +
      fast(0, `<animate attributeName="opacity" values="1;1;0" keyTimes="0;0.9;1" dur="1.6s" repeatCount="indefinite"/>`) + `</circle>`);
    return t;
  });
  // the floor's style: where its formations stand, front to back (the middle and the archway kept clear)
  const spots = [[-0.43, 0.12, 1], [0.41, 0.06, 1.1], [-0.13, 0.03, 0.75], [0.17, 0.08, 0.8], [0.3, 0.42, 0.9], [-0.32, 0.5, 0.95], [-0.45, 0.86, 1.2], [0.45, 0.8, 1.15]];
  s += SEP + part("caveroom-floor-" + floorSty, [c.F, sceneLfoKey(o, "slow"), sceneLfoKey(o, "fast")], () => {
    const at = ([u, v, z], i) => { const y = line + 6 * k + (near - 30 * k - line) * v; return { x: W * u, y, z: z * persp(y) * k, i }; };
    const S = spots.map(at);
    if (floorSty === "rimstone") return floorObj("rimstone pools", [[-0.3, 0.3, 1.3], [0.22, 0.2, 1.1], [0.05, 0.75, 1.5], [-0.38, 0.85, 1]].map(([u, v, z], i) => {
      const { x, y, z: zz } = at([u, v, z], i), rx = 70 * zz, ry = 16 * zz, pts = [];
      for (let n = 0; n <= 16; n++) { const a = n / 16 * 2 * Math.PI, b = 1 + 0.18 * (R(n + 17 * i, 4820) - 0.5); pts.push([x + rx * Math.cos(a) * b, y + ry * Math.sin(a) * b]); }
      const d = caveSmooth(pts, P) + " Z";
      return `<path d="${d}" fill="#4f9fb4" opacity="0.55"/><path d="${d}" fill="none" stroke="#c9bfa8" stroke-width="${f1(4 * zz)}" stroke-linejoin="round" opacity="0.85"/>` +
             `<path d="${d}" fill="none" stroke="#7d7363" stroke-width="${f1(2 * zz)}" transform="translate(0 ${f1(3 * zz)})" opacity="0.6"/>` +
             `<ellipse cx="${f1(x - rx * 0.3)}" cy="${f1(y - ry * 0.2)}" rx="${f1(rx * 0.3)}" ry="${f1(ry * 0.18)}" fill="#e6f8ff" opacity="0.3">` +
             fast(i / 4, `<animate attributeName="opacity" values="0.1;0.45;0.1" dur="1.2s" repeatCount="indefinite"/>`) + `</ellipse>`;
    }).join(""));
    return (floorSty === "columns" ? `<defs><clipPath id="bg-caveroom-under-vault"><path d="${caveSmooth(vault, P)} L${P(x1 + pad, y1 + pad)} L${P(x0 - pad, y1 + pad)} Z"/></clipPath></defs>` : "") + S.map(({ x, y, z, i }) => {
      if (floorSty === "stalagmites") {
        const h = (60 + 70 * R(i, 4830)) * z, w = (24 + 16 * R(i, 4831)) * z;
        return obj(`stalagmite ${i + 1}`, y, `<path d="M${P(x - w, y)} Q${P(x - w * 0.4, y - h * 0.5)} ${P(x - w * 0.08, y - h * 0.94)} Q${P(x, y - h * 1.02)} ${P(x + w * 0.08, y - h * 0.94)} Q${P(x + w * 0.4, y - h * 0.5)} ${P(x + w, y)} Z" fill="#5a5266" stroke="#2a2630" stroke-width="${f1(2 * k)}"/>` +
          [0.25, 0.48, 0.7].map(f => `<path d="M${P(x - w * (1 - f) * 0.92, y - h * f)} Q${P(x, y - h * f + 5 * z)} ${P(x + w * (1 - f) * 0.92, y - h * f)}" fill="none" stroke="#6e6579" stroke-width="${f1(2 * z)}" opacity="0.8"/>`).join("") +
          `<path d="M${P(x - w * 0.55, y - 4 * z)} Q${P(x - w * 0.3, y - h * 0.5)} ${P(x - w * 0.05, y - h * 0.85)}" fill="none" stroke="#8a8196" stroke-width="${f1(2 * z)}" opacity="0.5"/>` +
          `<ellipse class="flat" cx="${f1(x)}" cy="${f1(y)}" rx="${f1(w * 1.2)}" ry="${f1(w * 0.25)}" fill="#1c1922" opacity="0.35"/>`);
      }
      if (floorSty === "boulders") {
        const r = (38 + 30 * R(i, 4835)) * z;
        return obj(`boulder ${i + 1}`, y, `<ellipse class="flat" cx="${f1(x)}" cy="${f1(y)}" rx="${f1(r * 1.15)}" ry="${f1(r * 0.25)}" fill="#1c1922" opacity="0.4"/>` + rockSvg(x, y, r, 4836 + i, ["#6a6276", "#5a5266", "#7a7184"][i % 3]));
      }
      if (floorSty === "columns") {
        // (six at most: the two nearest are left out, so the picture is not all column; and none in front of the archway,
        // which a column, floor to ceiling, would hide)
        if (i > 5 || Math.abs(x + 0.3 * W) < 80 * k) return "";
        // (the review of 0.0.386: each column rises into the vault and is cut off along it, so it meets the ceiling wherever
        // the vault slopes; flat-topped at the vault, the outer ones stuck up into the rock once the rock was drawn behind them)
        const top = caveAlong(vault, x) - 40 * k, w = (34 + 18 * R(i, 4840)) * z, mid = y - (y - top) * (0.45 + 0.15 * R(i, 4841)), wm = w * (0.42 + 0.15 * R(i, 4842));
        const L = caveFractalPath([[x - w, y], [x - wm, mid], [x - w * 1.3, top]], 3, 0.06, R, 4843 + 5 * i), Rt = caveFractalPath([[x + w * 1.3, top], [x + wm, mid], [x + w, y]], 3, 0.06, R, 4846 + 5 * i);
        return obj(`column ${i + 1}`, y, `<g clip-path="url(#bg-caveroom-under-vault)"><path d="${caveSmooth(L, P)} ${caveSmooth(Rt, P).replace(/^M/, "L")} Z" fill="#5e5668" stroke="#2a2630" stroke-width="${f1(2 * k)}"/>` +
          [0.15, 0.3, 0.62, 0.8].map(f => { const yy = y - (y - top) * f, ww = f < 0.5 ? w - (w - wm) * f / 0.5 : wm + (w * 1.3 - wm) * (f - 0.5) / 0.5;
            return `<path d="M${P(x - ww * 0.95, yy)} Q${P(x, yy + 6 * z)} ${P(x + ww * 0.95, yy)}" fill="none" stroke="#776e83" stroke-width="${f1(2 * z)}" opacity="0.7"/>`; }).join("") +
          `</g><ellipse class="flat" cx="${f1(x)}" cy="${f1(y)}" rx="${f1(w * 1.3)}" ry="${f1(w * 0.28)}" fill="#1c1922" opacity="0.35"/>`);
      }
      if (floorSty === "crystals") {
        const col = CAVE_GLOWS[i % 5], h = (54 + 40 * R(i, 4850)) * z;
        return obj(`crystal cluster ${i + 1}`, y, `<ellipse class="fumes" cx="${f1(x)}" cy="${f1(y - h * 0.4)}" rx="${f1(h * 0.9)}" ry="${f1(h * 0.7)}" fill="${col}" opacity="0.2">` +
          slow(i / 8, `<animate attributeName="opacity" values="0.1;0.32;0.1" dur="4s" repeatCount="indefinite"/>`) + `</ellipse>` +
          [[-0.45, 0.55, -32], [0.4, 0.62, 28], [-0.15, 0.85, -10], [0.12, 1, 8], [0.62, 0.4, 48], [-0.65, 0.36, -52]].map(([dx, f, a]) => {
            const hh = h * f, ww = h * 0.13;
            return `<g transform="translate(${f1(x + dx * h * 0.3)} ${f1(y)}) rotate(${a})"><polygon points="${P2(-ww, 0)} ${P2(-ww, -hh * 0.82)} ${P2(0, -hh)} ${P2(ww, -hh * 0.82)} ${P2(ww, 0)}" fill="${col}" stroke="${mix(col, "#ffffff", 0.5)}" stroke-width="${f1(1.5 * z)}" opacity="0.92"/>` +
                   `<line x1="0" y1="0" x2="0" y2="${f1(-hh)}" stroke="${mix(col, "#ffffff", 0.6)}" stroke-width="${f1(1 * z)}" opacity="0.6"/></g>`; }).join("") +
          `<circle cx="${f1(x + h * 0.04)}" cy="${f1(y - h * 0.9)}" r="${f1(2.4 * z)}" fill="#ffffff">` + fast(i / 8, `<animate attributeName="opacity" values="0;1;0" dur="1.4s" repeatCount="indefinite"/>`) + `</circle>`);
      }
      // the glowing mushrooms: a big one and its little ones, glowing, spores drifting up from the big cap
      const col = ["#7dd3fc", "#a7f3d0", "#c4b5fd", "#fcd34d"][i % 4], hs = (26 + 14 * R(i, 4855)) * z, cr = (26 + 10 * R(i, 4856)) * z;   // (squat: a giant one is a stool to sit on)
      const cap = (cx, cy, r, h) => `<path d="M${P(cx - r * 0.18, cy)} L${P(cx - r * 0.12, cy - h)} L${P(cx + r * 0.12, cy - h)} L${P(cx + r * 0.18, cy)} Z" fill="#e8e0f0"/>` +
        `<path d="M${P(cx - r, cy - h + 2 * z)} Q${P(cx - r * 0.9, cy - h - r * 0.8)} ${P(cx, cy - h - r * 0.85)} Q${P(cx + r * 0.9, cy - h - r * 0.8)} ${P(cx + r, cy - h + 2 * z)} Q${P(cx, cy - h + r * 0.2)} ${P(cx - r, cy - h + 2 * z)} Z" fill="${col}" stroke="${mix(col, "#1a1620", 0.5)}" stroke-width="${f1(1.5 * z)}"/>` +
        [[-0.4, -0.45], [0.1, -0.62], [0.45, -0.35]].map(([dx, dy]) => `<circle cx="${f1(cx + dx * r)}" cy="${f1(cy - h + dy * r)}" r="${f1(r * 0.1)}" fill="#ffffff" opacity="0.8"/>`).join("");
      return obj(`mushroom ${i + 1}`, y, `<ellipse class="fumes" cx="${f1(x)}" cy="${f1(y - hs * 0.8)}" rx="${f1(cr * 2.2)}" ry="${f1(cr * 1.7)}" fill="${col}" opacity="0.18">` +
        slow(i / 8, `<animate attributeName="opacity" values="0.1;0.3;0.1" dur="4s" repeatCount="indefinite"/>`) + `</ellipse>` +
        cap(x + cr * 1.1, y + 2 * z, cr * 0.45, hs * 0.45) + cap(x, y, cr, hs) + cap(x - cr * 0.9, y + 3 * z, cr * 0.35, hs * 0.32) +
        `<g class="fumes">` + [0, 1, 2].map(n => `<circle cx="${f1(x + (n - 1) * cr * 0.4)}" cy="${f1(y - hs - cr)}" r="${f1(1.6 * z)}" fill="${col}" opacity="0">` +
          slow(n / 3 + i / 8, `<animate attributeName="cy" values="${f1(y - hs - cr)};${f1(y - hs - cr - 60 * z)}" dur="5s" repeatCount="indefinite"/>`) +
          slow(n / 3 + i / 8, `<animate attributeName="opacity" values="0;0.9;0" dur="5s" repeatCount="indefinite"/>`) + `</circle>`).join("") + `</g>`);
    }).join("");
  });
  // the ceiling: the near rock over the room, its edge the vault; its style hanging from it (behind the friend: it hangs
  // above them, at the back)
  s += SEP + part("caveroom-roof-" + roofSty, [c.F, sceneLfoKey(o, "slow"), sceneLfoKey(o, "fast")], () => {
    const xs = n => [...Array(n)].map((_, i) => x0 + W * (i + 0.15 + 0.7 * R(i, 4860 + n)) / n);
    // (the review after 0.0.385: the rock and its edge inside the ceiling's object, which sorts behind the friend; drawn loose,
    // it fell among the formations and covered a friend standing behind the nearest)
    let t = `<defs>${grad("bg-caveroom-roof", "#17141b", "#2a2530", y0 - pad, y0 + 130 * k)}</defs>` +
            `<path d="M${P(x0 - pad, y0 - pad)} L${P(x1 + pad, y0 - pad)} L${P(x1 + pad, vault[vault.length - 1][1])} ${caveSmooth([...vault].reverse(), P).replace(/^M/, "L")} L${P(x0 - pad, vault[0][1])} Z" fill="url(#bg-caveroom-roof)"/>` +
            `<path d="${caveSmooth(vault, P)}" fill="none" stroke="#4a4354" stroke-width="${f1(3 * k)}" stroke-linejoin="round" opacity="0.8"/>`, f = "", lightOnFloor = "";
    if (roofSty === "stalactites") xs(22).forEach((x, i) => {
      const top = caveAlong(vault, x) - 6 * k, len = (24 + 80 * R(i, 4865)) * k, w = (7 + 9 * R(i, 4866)) * k;
      f += `<path d="M${P(x - w, top)} L${P(x + w, top)} Q${P(x + w * 0.3, top + len * 0.6)} ${P(x, top + len)} Q${P(x - w * 0.3, top + len * 0.6)} ${P(x - w, top)} Z" fill="#4e4759" stroke="#2a2630" stroke-width="${f1(1.5 * k)}"/>` +
           `<path d="M${P(x - w * 0.5, top + 4 * k)} Q${P(x - w * 0.2, top + len * 0.6)} ${P(x, top + len)}" fill="none" stroke="#6f667b" stroke-width="${f1(1.5 * k)}"/>`;
      if (i % 4 === 1) f += `<circle cx="${f1(x)}" cy="${f1(top + len)}" r="${f1(2 * k)}" fill="#bfeaff">` +
        fast(i / 22, `<animate attributeName="cy" values="${f1(top + len)};${f1(top + len + 60 * k)}" dur="1.2s" repeatCount="indefinite"/>`) +
        fast(i / 22, `<animate attributeName="opacity" values="1;1;0" keyTimes="0;0.7;1" dur="1.2s" repeatCount="indefinite"/>`) + `</circle>`;
    });
    else if (roofSty === "straws") xs(46).forEach((x, i) => {
      const top = caveAlong(vault, x) - 4 * k, len = (14 + 46 * R(i, 4867)) * k;
      f += `<line x1="${f1(x)}" y1="${f1(top)}" x2="${f1(x + (R(i, 4868) - 0.5) * 3 * k)}" y2="${f1(top + len)}" stroke="#d8cdb8" stroke-width="${f1(2.6 * k)}" stroke-linecap="round" opacity="0.85"/>` +
           `<circle cx="${f1(x)}" cy="${f1(top + len + 2 * k)}" r="${f1(1.8 * k)}" fill="#e6f8ff">` + fast(i / 46, `<animate attributeName="opacity" values="0.2;1;0.2" dur="1.5s" repeatCount="indefinite"/>`) + `</circle>`;
    });
    else if (roofSty === "curtains") xs(7).forEach((x, i) => {
      // draperies: a sheet of rock hanging from the vault, its hem waving, banded light and dark (the "bacon" a cave grows)
      const top = caveAlong(vault, x) - 6 * k, wd = (40 + 30 * R(i, 4869)) * k, len = (60 + 70 * R(i, 4870)) * k, n = 6;
      const hem = [...Array(n + 1)].map((_, j) => { const u = 1 - j / n; return [x - wd / 2 + wd * u, top + len * (0.75 + 0.25 * Math.sin(u * Math.PI)) + (j % 2 ? 9 : -5) * k]; });
      f += `<path d="M${P(x - wd / 2, top)} L${P(x + wd / 2, top)} ${caveSmooth(hem, P).replace(/^M/, "L")} Z" fill="#a8774e" stroke="#5e3f26" stroke-width="${f1(1.5 * k)}" opacity="0.88"/>` +
           [0.2, 0.4, 0.6, 0.8].map((u, j) => `<line x1="${f1(x - wd / 2 + wd * u)}" y1="${f1(top)}" x2="${f1(x - wd / 2 + wd * u)}" y2="${f1(top + len * (0.7 + 0.2 * Math.sin(u * Math.PI)))}" stroke="${j % 2 ? "#7a4f2e" : "#ecd6b4"}" stroke-width="${f1(3 * k)}" opacity="0.7"/>`).join("");
    });
    else if (roofSty === "crack") {
      // a crack in the vault, daylight through it; its beam down to the floor, motes drifting in it (the slow LFO), ferns at its edge
      const cx = 0.16 * W, top = caveAlong(vault, cx), crack = caveFractal([cx - 34 * k, top - 10 * k], [cx + 40 * k, top - 4 * k], 4, 0.25, R, 4871);
      f += `<defs><linearGradient id="bg-caveroom-beam" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#fff6d0" stop-opacity="0.45"/><stop offset="1" stop-color="#fff6d0" stop-opacity="0"/></linearGradient></defs>` +
           `<polygon points="${P2(cx - 30 * k, top)} ${P2(cx + 36 * k, top)} ${P2(cx + 110 * k, g + 40 * k)} ${P2(cx - 60 * k, g + 40 * k)}" fill="url(#bg-caveroom-beam)" pointer-events="none"/>` +
           `<path d="${caveSmooth(crack, P)} ${caveSmooth([...crack].reverse().map(([x, y], j) => [x, y + (6 + 6 * R(j, 4872)) * k]), P).replace(/^M/, "L")} Z" fill="#fffbe6" stroke="#1a171f" stroke-width="${f1(2 * k)}"/>` +
           [[-38, 2], [42, 4], [-20, 8]].map(([dx, dy], j) => grassTuft(cx + dx * k, top + dy * k, 12 * k, 4873 + j, "#4f7f3a", 1.4 * k)).join("") +
           [...Array(9)].map((_, j) => { const mx = cx + (R(j, 4876) - 0.3) * 120 * k, my = top + (g - top) * R(j, 4877);
             return `<circle cx="${f1(mx)}" cy="${f1(my)}" r="${f1(1.4 * k)}" fill="#fff6d0" opacity="0.7">` + slow(j / 9, `<animate attributeName="cy" values="${f1(my)};${f1(my + 30 * k)};${f1(my)}" dur="7s" repeatCount="indefinite"/>`) + `</circle>`; }).join("");
      lightOnFloor = floorObj("daylight on the floor", `<ellipse cx="${f1(cx + 25 * k)}" cy="${f1(g + 40 * k)}" rx="${f1(90 * k)}" ry="${f1(18 * k)}" fill="#fff6d0" opacity="0.18" pointer-events="none"/>`);
    }
    else if (roofSty === "crystals") xs(14).forEach((x, i) => {
      // clusters of crystals growing down from the vault and up out of its hollows, each its own size and slant
      const top = caveAlong(vault, x) - 4 * k + 26 * k * R(i, 4880) * (i % 3 === 2 ? 1 : 0), col = CAVE_GLOWS[i % 5], z = k * (0.55 + 0.75 * R(i, 4878)), tilt = (R(i, 4881) - 0.5) * 40;
      f += [[-0.6, 22, -34], [-0.2, 30, -10], [0.15, 40, 6], [0.5, 26, 30], [0.85, 16, 52]].slice(0, 3 + (i % 3)).map(([dx, h, a]) => `<polygon points="-5,0 -5,${f1(h - 8)} 0,${f1(h)} 5,${f1(h - 8)} 5,0" fill="${col}" stroke="${mix(col, "#ffffff", 0.5)}" stroke-width="1.2" opacity="0.9" ` +
             `transform="translate(${f1(x + dx * 16 * z)} ${f1(top)}) rotate(${f1(a + tilt)}) scale(${z.toFixed(3)})"/>`).join("") +
           `<circle cx="${f1(x)}" cy="${f1(top + 30 * k)}" r="${f1(2.2 * k)}" fill="#ffffff">` + fast(i / 18, `<animate attributeName="opacity" values="0;1;0" dur="1.6s" repeatCount="indefinite"/>`) + `</circle>`;
    });
    else xs(30).forEach((x, i) => {
      // glowworms: threads of silk hanging, beads of light along them, glowing in turn (the slow LFO)
      const top = caveAlong(vault, x) - 4 * k, len = (20 + 80 * R(i, 4879)) * k;
      f += `<line x1="${f1(x)}" y1="${f1(top)}" x2="${f1(x)}" y2="${f1(top + len)}" stroke="#cfe" stroke-width="${f1(0.8 * k)}" opacity="0.45"/>` +
           [0.3, 0.55, 0.8, 1].map((u, j) => `<circle cx="${f1(x)}" cy="${f1(top + len * u)}" r="${f1((1 + 0.6 * j) * k)}" fill="#7dfcd2" opacity="0.8">` +
             (j === 3 ? slow(i / 30, `<animate attributeName="opacity" values="0.3;1;0.3" dur="4s" repeatCount="indefinite"/>`) : "") + `</circle>`).join("");
    });
    // two bats asleep, whatever the style
    f += caveBatSvg(-0.22 * W, caveAlong(vault, -0.22 * W) - 2 * k, k, 0) + caveBatSvg(0.33 * W, caveAlong(vault, 0.33 * W) - 2 * k, k, 1);
    return wallObj(`the ceiling's ${CAVE_CEILINGS.find(([v]) => v === roofSty)[1].replace(/^\S+\s/, "")}`, t + f) + lightOnFloor;
  });
  // the dark at the picture's edges, over everything: a cave lit only by what glows in it (see-through as a layer, its
  // opacity on itself, so ticket 261's audit counts nothing as hidden under it: 0.0.376's speed run found 13)
  s += SEP + part("caveroom-dark", c.F, () => obj("the dark", Infinity, `<defs><radialGradient id="bg-caveroom-dark" cx="0.5" cy="0.55" r="0.75"><stop offset="0.55" stop-color="#07060a" stop-opacity="0"/>` +
    `<stop offset="1" stop-color="#07060a" stop-opacity="1"/></radialGradient></defs><rect x="${f1(x0 - pad)}" y="${f1(y0 - pad)}" width="${f1(W + 2 * pad)}" height="${f1(H + 2 * pad)}" fill="url(#bg-caveroom-dark)" opacity="0.55" pointer-events="none"/>`));
  return s;
}

// (ticket 502, for M; Matthew: "farm yard -- placeable ojects are barn buildings (6 types) and fence parts ...") The 🚜 farm
// yard: packed earth with straw all over it and wheel ruts, a red barn and a farmhouse at the back behind a fence of posts and
// rails (drawn by the 🏚️ barn's and the 🪵 fence's own code), fields beyond them under rolling hills and a line of trees (the
// shared far layers), the sky. A windpump pours into the stable's water trough, its wheel turning with the slow LFO, as the
// clouds drift and the chimney's smoke sways; the hens (the stable's own, M: "chickens") peck, the barn's weather vane turns
// and wisps of straw blow across the yard with the quick one. A haystack with a mouse at its foot, and a little red tractor
// (the place's 🚜). The middle is left clear for the friend and for what is put here (fences snap to its tiles: tileSnap).
function farmhouseSvg(o) {
  const wall = "#f2ece0", line = "#b9ad98", roof = "#8e3b2f", trim = "#ffffff", shut = "#4f7a55";
  let gable = ""; for (let y = -168; y < -118; y += 10) { const hw = 96 * (y + 178) / 60; gable += `M${f1(-hw)} ${y} H${f1(hw)} `; }
  let clap = ""; for (let y = -108; y < 0; y += 10) clap += `M-96 ${y} H96 `;
  // (its smoke, rising from the chimney, sways with the slow LFO: behind the chimney's top)
  return (o ? swaying(o, `<path d="M55 -198 Q44 -222 58 -244 T52 -290" fill="none" stroke="#e9e9ef" stroke-width="10" stroke-linecap="round" opacity="0.6"/>`, 55, -198, 7, 0.4) : "") +
    `<ellipse cx="0" cy="3" rx="118" ry="10" fill="#000000" opacity="0.18"/>` +
    `<rect x="46" y="-196" width="18" height="50" fill="#9a4a3a" stroke="#5e2a20" stroke-width="1.5"/><rect x="43" y="-201" width="24" height="7" fill="#7a3a2e"/>` +
    `<path d="M-96 0 V-118 L0 -178 L96 -118 V0 Z" fill="${wall}" stroke="${line}" stroke-width="1.5"/><path d="${clap}${gable}" stroke="${line}" stroke-width="1" opacity="0.7"/>` +
    `<path d="M-110 -110 L0 -187 L110 -110" fill="none" stroke="${roof}" stroke-width="13" stroke-linejoin="round" stroke-linecap="round"/>` +
    `<circle cx="0" cy="-146" r="11" fill="#9cc7dd" stroke="${trim}" stroke-width="3"/><path d="M-11 -146 H11 M0 -157 V-135" stroke="${trim}" stroke-width="2"/>` +
    `<path d="M-83 -88 h9 v40 h-9 Z M-42 -88 h9 v40 h-9 Z M33 -88 h9 v40 h-9 Z M74 -88 h9 v40 h-9 Z" fill="${shut}"/>` +
    [-58, 58].map(x => `<rect x="${x - 15}" y="-88" width="30" height="40" fill="#9cc7dd" stroke="${trim}" stroke-width="3"/><path d="M${x} -88 V-48 M${x - 15} -68 H${x + 15}" stroke="${trim}" stroke-width="2"/>`).join("") +
    `<rect x="-16" y="-70" width="32" height="70" fill="#6b8f6e" stroke="#3e5a40" stroke-width="1.5"/><circle cx="9" cy="-36" r="2.2" fill="#e2c15a"/>` +
    `<path d="M-30 -74 L30 -74 L36 -84 L-36 -84 Z" fill="${roof}"/><path d="M-27 -74 V-5 M27 -74 V-5" stroke="${trim}" stroke-width="4"/>` +
    `<rect x="-34" y="-6" width="68" height="6" fill="#c9bfae" stroke="${line}" stroke-width="1"/>` +
    // (two sunflowers by its corner, taller than its windows: a surprise)
    `<path d="M-104 0 Q-106 -50 -102 -104 M-90 0 Q-88 -40 -92 -84" fill="none" stroke="#4f7f3a" stroke-width="3"/>` +
    `<path d="M-104 -40 q-12 -6 -16 4 q10 2 16 -4 Z M-91 -50 q12 -8 16 2 q-10 4 -16 -2 Z" fill="#5f8f48"/>` +
    [[-102, -110, 13], [-92, -89, 11]].map(([x, y, r]) => `<circle cx="${x}" cy="${y}" r="${r}" fill="#f2c12e" stroke="#e8a317" stroke-width="5" stroke-dasharray="3 2"/><circle cx="${x}" cy="${y}" r="${f1(r * 0.5)}" fill="#6b3f1d"/>`).join("");
}
function tractorSvg() {
  const red = "#c0392b", redD = "#7d2018", tire = "#26231f", hub = "#e8b33a";
  const wheel = (x, y, r) => `<circle cx="${x}" cy="${y}" r="${r}" fill="${tire}"/><circle cx="${x}" cy="${y}" r="${f1(r - 3)}" fill="none" stroke="#3d3833" stroke-width="5" stroke-dasharray="5 4"/>` +
    `<circle cx="${x}" cy="${y}" r="${f1(r * 0.45)}" fill="${hub}" stroke="#a07a1a" stroke-width="2"/><circle cx="${x}" cy="${y}" r="${f1(r * 0.14)}" fill="#a07a1a"/>`;
  return `<ellipse cx="0" cy="2" rx="80" ry="8" fill="#000000" opacity="0.2"/>` + farmFoot(-72, 68, 26) +
    `<rect x="26" y="-112" width="6" height="52" fill="#4a4a4a"/><rect x="24" y="-116" width="10" height="6" rx="2" fill="#333333"/>` +
    `<path d="M-12 -62 L54 -62 Q62 -62 64 -54 L66 -30 L-12 -30 Z" fill="${red}" stroke="${redD}" stroke-width="2"/>` +
    `<path d="M60 -56 V-34 M56 -58 V-32 M-4 -54 H46 M-4 -46 H46" stroke="${redD}" stroke-width="1.5" opacity="0.7"/><circle cx="62" cy="-58" r="4" fill="#fff3b0" stroke="${redD}" stroke-width="1"/>` +
    `<path d="M-56 -76 Q-58 -94 -46 -94 L-38 -94 L-38 -74 Z" fill="#3a3a3a"/><path d="M-6 -62 L-22 -86" stroke="#3a3a3a" stroke-width="3"/><ellipse cx="-22" cy="-88" rx="9" ry="3" fill="none" stroke="#3a3a3a" stroke-width="2.5"/>` +
    wheel(46, -20, 20) +
    `<path d="M-76 -40 Q-74 -82 -34 -82 Q4 -82 6 -40 L-4 -40 Q-6 -72 -34 -72 Q-62 -72 -66 -40 Z" fill="${red}" stroke="${redD}" stroke-width="2"/>` +
    wheel(-34, -36, 36);
}
function haystackSvg(seed) {
  const R = seeded(seed), id = `bg-hay-${seed}`;
  let strands = "";
  for (let j = 0; j < 46; j++) { const v = 0.06 + 0.88 * R(j), y = -v * 136, half = 76 * Math.sqrt(Math.max(0, 1 - v * v)) * 0.92, x = (R(j + 50) * 2 - 1) * half * 0.86, a = (R(j + 100) - 0.5) * 1.6;
    strands += `M${f1(x)} ${f1(y)} q${f1(4 * Math.cos(a))} ${f1(-3 + 6 * Math.sin(a))} ${f1(9 * Math.cos(a))} ${f1(-4 + 8 * Math.sin(a))} `; }
  return `<defs><linearGradient id="${id}" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#f0d98a"/><stop offset="1" stop-color="#c9a24a"/></linearGradient></defs>` +
    `<ellipse cx="0" cy="3" rx="88" ry="10" fill="#000000" opacity="0.2"/>` +
    `<path d="M-76 0 C-86 -66 -46 -138 0 -142 C46 -138 86 -66 76 0 Q0 8 -76 0 Z" fill="url(#${id})" stroke="${FARM_STRAW_D}" stroke-width="1.5"/>` +
    `<path d="${strands}" fill="none" stroke="${FARM_STRAW_D}" stroke-width="1.3" opacity="0.8"/>` +
    `<path d="M-8 -140 q-6 -10 -2 -14 M0 -142 q2 -12 6 -14 M8 -139 q8 -8 10 -6" fill="none" stroke="${FARM_STRAW_D}" stroke-width="1.5"/>` +
    `<path d="M30 -70 L58 -170" stroke="#9c6b40" stroke-width="4" stroke-linecap="round"/><path d="M52 -168 h12" stroke="#7a5230" stroke-width="5" stroke-linecap="round"/>` +
    // (a mouse peeking out at its foot)
    `<ellipse cx="-50" cy="-3" rx="9" ry="6" fill="#8c8a86"/><circle cx="-56" cy="-9" r="3.2" fill="#a9a6a0" stroke="#6e6b66" stroke-width="0.8"/><circle cx="-57" cy="-3" r="1" fill="#111111"/><circle cx="-59" cy="-1" r="0.9" fill="#e48a9a"/>`;
}
function windpumpSvg(o, seed) {
  const R = seeded(seed), steel = "#8d969c", dk = "#5d666c";
  let br = "";
  for (let y = 0; y > -200; y -= 40) { const h0 = 26 - 20 * (-y / 200), h1 = 26 - 20 * ((40 - y) / 200); br += `M${f1(-h0)} ${y} L${f1(h1)} ${y - 40} M${f1(h0)} ${y} L${f1(-h1)} ${y - 40} `; }
  let blades = "";
  for (let j = 0; j < 18; j++) { const a = j * 20 * Math.PI / 180, p = (r, da) => `${f1(r * Math.cos(a + da))} ${f1(r * Math.sin(a + da))}`;
    blades += `M${p(9, -0.07)} L${p(44, -0.12)} L${p(44, 0.07)} L${p(9, 0.04)} Z `; }
  return `<ellipse cx="0" cy="2" rx="34" ry="5" fill="#000000" opacity="0.18"/>` + farmFoot(-30, 30, 8) +
    `<path d="M-26 0 L-6 -200 M26 0 L6 -200" stroke="${steel}" stroke-width="4"/><path d="${br}" stroke="${steel}" stroke-width="1.8"/>` +
    `<rect x="-14" y="-206" width="28" height="6" fill="${dk}"/><path d="M0 -200 V-40" stroke="#6a6a6a" stroke-width="2"/>` +
    `<rect x="-6" y="-42" width="12" height="28" fill="#5a7a8a" stroke="#3a5060" stroke-width="1.5"/><path d="M6 -36 L24 -32 L24 -27 L6 -30 Z" fill="#5a7a8a" stroke="#3a5060" stroke-width="1"/>` +
    `<path d="M23 -27 Q25 -16 24 -4" stroke="#9fd3ea" stroke-width="3" fill="none" opacity="0.85"/>` +
    // (its tail vane, behind the wheel, out to the right; the wheel facing us a little to the left)
    `<path d="M0 -214 L58 -218" stroke="${steel}" stroke-width="3"/><path d="M40 -232 L68 -230 L68 -206 L40 -210 Z" fill="#c94a3a" stroke="${dk}" stroke-width="1.5"/>` +
    `<g transform="translate(0 -214) scale(0.85 1)"><g>${lfoTimed(o, "slow", R(1), `<animateTransform attributeName="transform" type="rotate" values="0;60" dur="5s" repeatCount="indefinite"/>`)}` +
    `<path d="${blades}" fill="#dfe4e7" stroke="${dk}" stroke-width="0.8"/><circle r="44" fill="none" stroke="${dk}" stroke-width="2"/><circle r="26" fill="none" stroke="${dk}" stroke-width="1.5"/>` +
    `<circle r="7" fill="${dk}"/></g></g>`;
}
function farmyardScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, o } = c;
  // (the fields from the horizon to near, the ground the buildings stand on, the back fence at fz: just behind the back line
  // that SCENE_FAR sets, 90 up)
  const horizon = g - 205 * k, near = g - 128 * k, fz = g - 94 * k;
  // the sky and a soft sun; the clouds drifting (the slow LFO)
  let s = SEP + part("farm-sky", c.F, () => `<defs>${grad("bg-farm-sky", "#74bdea", "#e6f4fb", y0, horizon)}` +
    `<radialGradient id="bg-farm-sun"><stop offset="0.3" stop-color="#fff3b0"/><stop offset="1" stop-color="#fff3b0" stop-opacity="0"/></radialGradient></defs>` + all("url(#bg-farm-sky)") +
    `<circle cx="${f1(x1 - W * 0.15)}" cy="${f1(y0 + H * 0.13)}" r="${f1(64 * k)}" fill="url(#bg-farm-sun)"/><circle cx="${f1(x1 - W * 0.15)}" cy="${f1(y0 + H * 0.13)}" r="${f1(22 * k)}" fill="#fff6c9"/>`);
  s += SEP + cloudsSvg(c, 3, 0.55, 502);
  // far off: rolling hills and a line of round trees, on the horizon (farLayers: wall objects of their own)
  s += SEP + part("farm-far", c.F, () => farLayers(c, horizon, { mountains: "hills", forest: "broadleaf", size: 70 }));
  // the fields: strips going off toward the hills, crops of five colours, furrows in every other one (ending on its edges),
  // hedges between the rows of them
  s += SEP + part("farm-fields", c.F, () => {
    const at = (u, v) => [u * W * 0.75 * (0.25 + 0.75 * v), horizon + (near - horizon) * v];
    const us = [-1.9, -1.2, -0.62, -0.2, 0.22, 0.66, 1.25, 1.95], vs = [0, 0.28, 0.6, 1], crops = ["#d9b85a", "#8fbf5e", "#a07c52", "#b9d77f", "#c9a24a", "#7fae55"];
    let t = `<rect x="${f1(x0 - pad)}" y="${f1(horizon - 2 * k)}" width="${f1(W + 2 * pad)}" height="${f1(near - horizon + 6 * k)}" fill="#a8c76e"/>`;
    for (let j = 0; j < 3; j++) for (let i = 0; i < us.length - 1; i++) {
      const q = [at(us[i], vs[j]), at(us[i + 1], vs[j]), at(us[i + 1], vs[j + 1]), at(us[i], vs[j + 1])], col = crops[(i * 2 + j * 3) % crops.length];
      t += `<polygon points="${q.map(p => P(...p)).join(" ")}" fill="${col}"/>`;
      if ((i + j) % 2 === 0) { let d = "";
        for (let m = 1; m < 6; m++) { const u = us[i] + (us[i + 1] - us[i]) * m / 6, [ax, ay] = at(u, vs[j]), [bx, by] = at(u, vs[j + 1]); d += `M${P(ax, ay)} L${P(bx, by)} `; }
        t += `<path d="${d}" stroke="${mix(col, "#000000", 0.22)}" stroke-width="${f1(1.2 * k)}" opacity="0.6"/>`; }
    }
    for (const v of [0.28, 0.6]) { const y = horizon + (near - horizon) * v, h = (2.5 + 5 * v) * k, n = 40;
      let d = `M${P(x0 - pad, y + h * 0.4)}`;
      for (let i = 0; i <= n; i++) { const x = x0 - pad + (W + 2 * pad) * i / n; d += ` Q${P(x - (W + 2 * pad) / n / 2, y - h * (0.8 + 0.6 * R(i, 5020 + v * 10)))} ${P(x, y)}`; }
      t += `<path d="${d} L${P(x1 + pad, y + h * 0.5)} L${P(x0 - pad, y + h * 0.5)} Z" fill="#4f7f3a"/>`; }
    return wallObj("fields", t);
  });
  // the ground behind the fence, grass, and the yard's packed earth in front of it (the earth a little lighter far off)
  s += SEP + part("farm-ground", c.F, () => `<defs>${grad("bg-farm-earth", "#d2b483", "#ac8a58", fz, y1)}</defs>` +
    `<rect x="${f1(x0 - pad)}" y="${f1(near - 2 * k)}" width="${f1(W + 2 * pad)}" height="${f1(fz - near + 6 * k)}" fill="#9fc46a"/>` +
    `<rect x="${f1(x0 - pad)}" y="${f1(fz - 2 * k)}" width="${f1(W + 2 * pad)}" height="${f1(y1 - fz + pad)}" fill="url(#bg-farm-earth)"/>`);
  // at the back: the red barn on the left, the farmhouse on the right (the 🏚️ barn's own drawing; its weather vane the quick LFO,
  // the chimney's smoke the slow); objects behind the fence, the friend always in front of them
  s += SEP + part("farm-buildings", [c.F, sceneLfoKey(o, "fast"), sceneLfoKey(o, "slow")], () => {
    const bx = -W * 0.17, by = g - 124 * k, hx = W * 0.36, hy = g - 122 * k;
    return obj("red barn", by, `<g ${tf(bx, by, 0.5 * k)}>${barnThingSvg({ style: "gambrel" }, 5021, o)}</g>`) +
           obj("farmhouse", hy, `<g ${tf(hx, hy, 0.45 * k)}>${farmhouseSvg(o)}</g>`);
  });
  // the back fence: wooden posts and rails right across (the 🪵 fence's own drawing, one run), grass along its foot
  s += SEP + part("farm-fence", c.F, () => {
    const fs = 0.314 * k, a = Math.floor((x0 - pad * 0.1) / fs / TILE_SPAN) * TILE_SPAN, b = Math.ceil((x1 + pad * 0.1) / fs / TILE_SPAN) * TILE_SPAN, posts = [];
    for (let x = a; x <= b; x += TILE_SPAN) posts.push(x);
    let grass = `M${P(x0 - 20 * k, fz + 3 * k)}`;
    for (let x = x0 - 20 * k, i = 0; x < x1 + 20 * k; x += 6 * k, i++) grass += ` L${P(x + 2 * k, fz - (4 + 6 * R(i, 5024)) * k)} L${P(x + 4 * k, fz + 2 * k)}`;
    return obj("fence, back", fz, `<g ${tf(0, fz, fs)}>${fenceSvg("wood", a, b, posts, 5022)}</g>`) +
           floorObj("grass by the fence", `<path d="${grass} L${P(x1 + 20 * k, fz + 4 * k)} L${P(x0 - 20 * k, fz + 4 * k)} Z" fill="#7fa94e"/>`);
  });
  // on the yard's floor: the tractor's tracks, its two wheels' side by side, from where it stands round toward us; patches of
  // darker earth, pebbles, straw all over
  s += SEP + part("farm-floor", c.F, () => {
    const rut = dz => `M${P(W * 0.13 - 16 * k, g - 64 * k + dz)} Q${P(-W * 0.12, g - 60 * k + dz * 1.4)} ${P(-W * 0.2 + dz * 1.6, y1 + pad)}`;
    let patches = "", pebbles = "", st = ["", ""];
    for (let i = 0; i < 9; i++) { const y = fz + 12 * k + (y1 - fz) * R(i, 5030), x = x0 + W * R(i, 5031), z = 0.5 + 0.8 * (y - fz) / (y1 - fz), rx = (30 + 40 * R(i, 5032)) * k * z;
      patches += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(rx)}" ry="${f1(rx * 0.22)}" fill="${i % 3 ? "#a08154" : "#d8bd8e"}" opacity="0.45"/>`; }
    for (let i = 0; i < 26; i++) { const y = fz + 6 * k + (y1 - fz) * R(i, 5033), x = x0 + W * R(i, 5034), r = (1.4 + 2.4 * R(i, 5035)) * k * (0.5 + 0.8 * (y - fz) / (y1 - fz));
      pebbles += `M${P(x - r, y)} a${f1(r)} ${f1(r * 0.6)} 0 1 0 ${f1(2 * r)} 0 a${f1(r)} ${f1(r * 0.6)} 0 1 0 ${f1(-2 * r)} 0 `; }
    for (let i = 0; i < 90; i++) { const v = Math.pow(R(i, 5036), 1.3), y = fz + 8 * k + (y1 - fz) * v, x = x0 + W * R(i, 5037), z = 0.5 + 0.9 * v, a = R(i, 5038) * Math.PI;
      st[i % 3 ? 0 : 1] += `M${P(x, y)} l${f1(Math.cos(a) * 10 * k * z)} ${f1(Math.sin(a) * 2.6 * k * z)} `; }
    return floorObj("wheel ruts", `<path d="${rut(-5 * k)} ${rut(5 * k)}" fill="none" stroke="#94744a" stroke-width="${f1(5 * k)}" stroke-linecap="round" opacity="0.4"/>` + patches) +
           floorObj("pebbles", `<path d="${pebbles}" fill="#8a7a66"/>`) +
           floorObj("straw", `<path d="${st[0]}" stroke="${FARM_STRAW}" stroke-width="${f1(1.6 * k)}" stroke-linecap="round" opacity="0.9"/><path d="${st[1]}" stroke="${FARM_STRAW_D}" stroke-width="${f1(1.6 * k)}" stroke-linecap="round" opacity="0.9"/>`);
  });
  // what stands in the yard, back to front: the windpump (its wheel the slow LFO) and the trough it pours into, on the left;
  // the tractor on the right; the haystack at the right, nearer (one sits in its shade: not in the way of the middle)
  s += SEP + part("farm-yard-things", [c.F, sceneLfoKey(o, "slow")], () => {
    const wx = -W * 0.31, wz = g - 70 * k, tx = -W * 0.31 + 30 * k, tz = g - 48 * k;
    return obj("windpump", wz, `<g ${tf(wx, wz, 0.55 * k)}>${windpumpSvg(o, 5023)}</g>`) +
           obj("water trough", tz, troughSvg(P, tx, tz, 1.0 * k)) +
           obj("tractor", g - 64 * k, `<g ${tf(W * 0.13, g - 64 * k, 0.62 * k)}>${tractorSvg()}</g>`) +
           obj("haystack", g + 50 * k, `<g ${tf(W * 0.38, g + 50 * k, 1.0 * k)}>${haystackSvg(5027)}</g>`);
  });
  // the hens (the stable's own: henSvg), pecking with the quick LFO, each her own way in; bigger nearer
  s += SEP + part("farm-hens", [c.F, sceneLfoKey(o, "fast")], () => [[-0.2, 62, 1, 0, 0, "hen, brown"], [0.2, 26, -1, 1, 1, "hen, white"], [-0.16, -36, 1, 2, 2, "hen, black"]]
    .map(([u, dy, sd, i, look, name]) => obj(name, g + dy * k, henSvg(o, W * u, g + dy * k, k * (1 + 0.0071 * dy), sd, i, look))).join(""));
  // wisps of straw blowing across the yard, with the quick LFO (each its own way in), fading in and out
  s += SEP + part("farm-blowing", [c.F, sceneLfoKey(o, "fast")], () => obj("blowing straw", g + 30 * k, [[-0.14, 22, 0], [0.04, 44, 1], [-0.3, 6, 2], [0.18, 66, 3]].map(([u, dy, j]) => {
    const x = W * u, y = g + dy * k, d = 70 * k * (0.7 + 0.6 * R(j, 5025)), off = j / 4 + 0.1 * R(j, 5026);
    return `<g opacity="0">${lfoTimed(o, "fast", off, `<animateTransform attributeName="transform" type="translate" values="0 0;${f1(d * 0.5)} ${f1(-14 * k)};${f1(d)} 0" dur="1.6s" repeatCount="indefinite"/>`)}` +
      lfoTimed(o, "fast", off, `<animate attributeName="opacity" values="0;0.95;0" dur="1.6s" repeatCount="indefinite"/>`) +
      `<path d="M${P(x, y)} l${f1(9 * k)} ${f1(-2 * k)} M${P(x + 3 * k, y + 2 * k)} l${f1(7 * k)} ${f1(1 * k)}" stroke="${FARM_STRAW}" stroke-width="${f1(1.8 * k)}" stroke-linecap="round"/></g>`;
  }).join("")));
  return s;
}

// The path through the woods, from the clearing to the mountain trail:
// tall trunks either side, ferns and mushrooms at their feet, sunbeams,
// the path winding off ahead; a signpost back to the clearing and on to
// the trail.
// (ticket 50) the two big mushrooms, the outer ones (the inner two are too
// small to sit on): each one's cap, its crown 34 of its units up (its
// curve, bowed toward 48, peaks there), within 12 each side of the crown,
// as woodpathScene's "wp-near" draws them
SEATS.woodpath = (c, B) => { const { W, g, k } = c;
  return [[-0.38, 70, 1.1, "mushroom, left outer"], [0.4, 60, 1, "mushroom, right outer"]].map(([u, dy, z, name]) => { const mx = W * u, my = g + dy * k, kk = k * z;
    return { name, x0: mx - 12 * kk, x1: mx + 12 * kk, y: my - 34 * kk, z: my + 1, facing: ["front", "left", "right"] }; }); };
function woodpathScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o, side } = c;
  const far = g - 170 * k;
  let s = SEP + part("wp-back", c.F, () => {
    let t = `<defs>${grad("bg-sky", "#d6efc2", "#f4f8dc", y0, far)}</defs>` + all("url(#bg-sky)"), trees = "";
    // far trunks, thin and pale, their crowns a green roof (one object,
    // "far trees", trunks and roof: standing at the top of the far ground's
    // wave, so the ground drawn after them never covers a friend behind them)
    for (let i = 0; i < 26; i++) {
      const x = x0 + W * (i + R(i, 404)) / 26, w = (6 + 6 * R(i, 405)) * k;
      trees += `<rect x="${f1(x - w / 2)}" y="${f1(y0 - pad)}" width="${f1(w)}" height="${f1(far + 10 * k - y0 + pad)}" fill="#8a7a62" opacity="0.7"/>`;
    }
    // (a roof of leaf pads, overlapping, a band of green behind them)
    trees += `<rect x="${f1(x0 - pad)}" y="${f1(y0 - pad)}" width="${f1(W + 2 * pad)}" height="${f1(pad + H * 0.06)}" fill="#4f8f5a"/>`;
    for (let i = 0; i < 30; i++) {
      const x = x0 - 30 * k + (W + 60 * k) * i / 29, y = y0 + H * (0.04 + 0.1 * R(i, 406)), z = (50 + 40 * R(i, 407)) * k / 80;
      trees += padSvg(Math.floor(R(i, 408) * 8), x, y, z * (R(i, 409) < 0.5 ? -1 : 1), z * 1.2, ["#5c9c63", "#4f8f5a", "#6aa86a"][i % 3]);
    }
    return t + obj("far trees", far - 6 * k, trees) + land(far, 6, "#86a95a", 0.7) + land(g - 80 * k, 8, "#6f9a48", 1.9);
  });
  // the sunbeams, slanting down through the leaves (an object, at the far
  // trees' depth, whose roof they come down through: behind the near trees
  // and a friend in front of the far ones, over a friend behind those)
  s += SEP + part("wp-beams", [c.F, sceneLfoKey(o, "slow")], () => obj("sunbeams", far - 6 * k, sunbeamsSvg("bg-wp-shaft", x0, W, k, y0 - 20 * k, g + 40 * k, 5, 88, -0.3, o)));
  // the path, winding off ahead, narrowing as it goes
  s += SEP + part("wp-path", c.F, () => {
    const L = [], Rr = [];
    for (let i = 0; i <= 20; i++) {
      const u = i / 20, y = y1 + 20 * k - (y1 + 20 * k - far) * u, x = W * 0.08 * Math.sin(u * 4.2) * u + W * 0.06 * u, w = (170 - 156 * u) * k;
      L.push(P2(x - w / 2, y)); Rr.push(P2(x + w / 2, y));
    }
    let t = `<polygon points="${[...L, ...Rr.reverse()].join(" ")}" fill="#c9a774" stroke="#a88a5a" stroke-width="${f1(2 * k)}"/>`;
    const mPeb = t.length;   // (ticket 284: the path's pebbles one floor object; the path itself stays terrain)
    for (let i = 0; i < 18; i++) {
      const u = R(i, 408), y = y1 - (y1 - far) * u, x = W * 0.08 * Math.sin(u * 4.2) * u + W * 0.06 * u + (R(i, 409) - 0.5) * (150 - 140 * u) * k;
      t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1((5 - 3 * u) * k)}" ry="${f1((3 - 2 * u) * k)}" fill="#a88a5a"/>`;
    }
    return floorSince(t, mPeb, "pebbles on the path");
  });
  // near trunks either side, ferns and mushrooms at their feet (each tree,
  // its ferns with it, an object at its foot; each mushroom one too)
  s += SEP + part("wp-near", c.F, () => {
    let t = "";
    for (const [u, w, b, name] of [[-0.46, 64, 70, "tree, left outer"], [-0.3, 40, 20, "tree, left inner"], [0.33, 44, 30, "tree, right inner"], [0.47, 70, 80, "tree, right outer"]]) {
      const x = W * u, base = g + b * k;
      let tr = `<path d="M${P(x - w * k / 2, y0 - pad)} L${P(x + w * k / 2, y0 - pad)} L${P(x + w * k / 2, base - 20 * k)} Q${P(x + w * k, base)} ${P(x + w * k * 0.9, base + 6 * k)} L${P(x - w * k * 0.9, base + 6 * k)} Q${P(x - w * k, base)} ${P(x - w * k / 2, base - 20 * k)} Z" fill="#5e4430" stroke="#3e2c1e" stroke-width="${f1(2 * k)}"/>`;
      for (let j = 0; j < 5; j++) {
        const bx = x + (R(j + u * 10, 410) - 0.5) * w * k * 0.7;
        tr += `<path d="M${P(bx, y0 + H * 0.1 * j)} q${f1(4 * k)} ${f1(40 * k)} 0 ${f1(90 * k)}" fill="none" stroke="#3e2c1e" stroke-width="${f1(2 * k)}" opacity="0.6"/>`;
      }
      // ferns, a fan of fronds
      for (const sd of [-1, 1]) for (let j = 0; j < 4; j++) {
        const a = (sd < 0 ? 200 : -20) + sd * j * 22, l = (40 + 16 * j) * k, fx = x + sd * w * k * 0.7, fy = base + 4 * k;
        const ex = fx + Math.cos(a * Math.PI / 180) * l * sd * -1 * -1, ey = fy - Math.abs(Math.sin(a * Math.PI / 180)) * l - 10 * k;
        tr += `<path d="M${P(fx, fy)} Q${P((fx + ex) / 2, ey - 10 * k)} ${P(ex, ey)}" fill="none" stroke="#3f8a3a" stroke-width="${f1(5 * k)}" stroke-linecap="round"/>`;
      }
      t += obj(name, base, tr);
    }
    // mushrooms, red with white spots
    for (const [u, dy, z, name] of [[-0.38, 70, 1.1, "mushroom, left outer"], [-0.25, 40, 0.8, "mushroom, left inner"], [0.4, 60, 1, "mushroom, right outer"], [0.27, 36, 0.7, "mushroom, right inner"]]) {
      const mx = W * u, my = g + dy * k, kk = k * z;
      t += obj(name, my, `<rect x="${f1(mx - 5 * kk)}" y="${f1(my - 22 * kk)}" width="${f1(10 * kk)}" height="${f1(22 * kk)}" rx="${f1(3 * kk)}" fill="#f4efe6"/>` +
           `<path d="M${P(mx - 22 * kk, my - 20 * kk)} Q${P(mx, my - 48 * kk)} ${P(mx + 22 * kk, my - 20 * kk)} Z" fill="#e53935"/>` +
           [[-9, -28], [4, -34], [12, -25]].map(([dx, dyy]) => `<circle cx="${f1(mx + dx * kk)}" cy="${f1(my + dyy * kk)}" r="${f1(3 * kk)}" fill="#ffffff"/>`).join(""));
    }
    return t;
  });
  // (ticket 233: its old signpost, fixed links to places, is gone; the world's ways are the nav arrows, and a sign with directions, 🧭, a thing, says where they lead)
  // butterflies, fluttering with the quick LFO (an object at the depth of
  // the ground under the nearer one)
  s += SEP + part("woodpath-butterflies", [c.F, sceneLfoKey(o, "fast"), sceneLfoKey(o, "wings")], () => obj("butterflies", g - 50 * k, butterfliesSvg(c, [[x0 + W * 0.18, g - 50 * k, 1], [x0 + W * 0.8, g - 90 * k, 1.1]])));
  return s;
}
// (ticket 503, for M; Matthew: "field/pasture for animals (like horses) to graze.") The 🌾 pasture: a wide green field
// rolling away to hills (farLayers' rolling hills and round trees, on its horizon), a post-and-rail fence along the back
// with a gate in it (shut: the world's ways are the nav arrows), a big shade tree, a water trough, wildflowers and tufts
// of grass in rows by depth (ticket 297) swaying with the slow LFO, clouds drifting with it, a bird on the gate post,
// butterflies, and horses grazing, side on, heads down to the grass, their tails swishing with the quick LFO, each its
// own way in. Its depth is the friend's: what stands at a depth is drawn at the size the friend's own horse is there (🔭
// at its usual 40, the friend arriving at the ground line), so a horse led into the pasture is as big as the ones
// grazing beside it. No hens (Matthew: "chickens -- already in stable"): the stable has them, pecking.
// (how far above the ground line, in k, a thing would shrink to nothing: sceneVanish's line at 🔭's usual 40)
const PASTURE_VANISH = 175;
const pastureDepth = up => Math.max(0.12, (PASTURE_VANISH - up) / PASTURE_VANISH);
// (a horse's own units, horseSvg's, to the picture's at `up` k above the ground line: the friend's horse's size there,
// HORSE_SCALE of the friend's units at the friend's size in the pasture, SCENE_FIG's, carried from the stage to this frame)
const pastureHorseScale = (k, up) => HORSE_SCALE * sceneFig("pasture") * 700 / (SCENE.bottom - SCENE.top) * pastureDepth(up) * k;
// The horses grazing: their coats, manes and face markings the stable's (HORSE_COATS, HORSE_MANES, HORSE_MARKS), where
// each stands (u: a share of the picture's width from its middle; up: k above the ground line), which way it faces
// (1 our right), and how far into the swish its tail starts (each its own); pastureHorses says how many.
// (ticket 503, after review: the friend walks behind every horse, not only the grey) Each stands far enough in front of
// the back line (SCENE_FAR's 157) that its footprint's back is more than LANE_MIN (24) in front of it, or solidsHere
// closes the lane behind it: none farther back than about 111 up (lanes of 28, 36 and 48 here); and their footprints
// apart, so the friend goes round each
const PASTURE_HORSES = [
  { name: "bay", coat: "#7b4a2a", mane: "#1f1f22", mark: "star", u: 0.18, up: 100, face: -1, off: 0.13 },
  { name: "grey", coat: "#c9c9cf", mane: "#f3efe6", mark: "none", u: 0.335, up: 84, face: -1, off: 0.58 },
  { name: "chestnut", coat: "#a0522d", mane: "#7a3a1a", mark: "blaze", u: -0.12, up: 111, face: -1, off: 0.81 }];
// The wildflowers in its grass: [name, its petals' colour, its middle's, how many petals, how wide each (radians)]
const PASTURE_FLOWERS = [["daisy", "#ffffff", "#f6c915", 9, 0.3], ["buttercup", "#f7d21e", "#d9a400", 5, 0.55], ["poppy", "#e0412f", "#2a1a1a", 4, 0.78],
                         ["cornflower", "#4c7be0", "#26408a", 8, 0.26], ["clover", "#d77fb8", "#b0558f", 10, 0.32]];
// A horse grazing, side on, facing our right, in horseSvg's units (its hooves on the ground at 0, its withers about 300
// up): its own drawing, not the standing horse turned (Matthew's rule for a new pose): its neck lowered in an arch from the
// withers, its head hanging down to the grass, the muzzle in it, the ears up at the poll, its mane falling down our side
// of the neck, the forelock down its forehead; one foreleg a step forward, as a horse stands to graze. Its legs are the
// friend's horse's (horseLeg), its far side darker, all four under its body (the belly's outline over their tops). Its
// tail hangs from the croup and swishes about its root with the quick LFO, `off` of a swish in. Its shadow lies flat, and
// its footprint (an unseen box, ticket 441's) is where its hooves stand, from its tail to its muzzle and a body's width
// deep, so the friend walks round it, in front of it or behind it.
function grazingHorseSvg(H, o, off, i) {
  const coat = H.coat, mane = H.mane, ln = mix(coat, "#000000", 0.42), far = mix(coat, "#000000", 0.25), maneD = mix(mane, "#000000", 0.3), white = "#f7f4ee";
  const P_ = ([x, y]) => `${f1(x)} ${f1(y)}`;
  const leg = (x, top, swing, flex, fore, near) => horseLeg([x, top], -top - 6, swing, flex, fore, near ? 17 : 15, near ? coat : far, false, 0);
  const dur = 1.4 + 0.35 * i;
  const tail = `<g>${sceneLfo(o, "fast", off, "transform", (v, amp) => `${f1((5 + 9 * amp) * v)} -146 -288`,
      `<animateTransform attributeName="transform" type="rotate" values="-5 -146 -288;9 -146 -288;-5 -146 -288" dur="${dur.toFixed(2)}s" begin="-${(off * dur).toFixed(2)}s" repeatCount="indefinite"/>`)}` +
    `<path d="M-148 -292 Q-198 -236 -186 -96 Q-170 -110 -162 -150 Q-157 -228 -136 -282 Z" fill="${mane}" stroke="${maneD}" stroke-width="2.5" stroke-linejoin="round"/></g>`;
  // (the body, its withers a little lower than standing, the weight on the forehand; the shoulder and the round of the haunch)
  const body = `<path d="M116 -282 C40 -308 -98 -318 -136 -302 C-176 -286 -178 -218 -152 -186 C-122 -160 -34 -158 80 -166 C140 -172 168 -212 150 -256 Z" fill="${coat}" stroke="${ln}" stroke-width="3"/>` +
    `<path d="M138 -198 Q118 -240 104 -282 M-80 -312 Q-100 -240 -146 -200" fill="none" stroke="${mix(coat, "#000000", 0.2)}" stroke-width="3" stroke-linecap="round" opacity="0.7"/>`;
  // (the neck: its crest arching forward from the withers and down to the poll, its underside from the breast to the throat)
  const neck = `<path d="M84 -292 Q196 -304 240 -156 L210 -116 Q172 -150 148 -222 Z" fill="${coat}" stroke="${ln}" stroke-width="3" stroke-linejoin="round"/>`;
  const crest = t => { const v = 1 - t; return [v * v * 86 + 2 * v * t * 196 + t * t * 240, v * v * -298 + 2 * v * t * -308 + t * t * -160]; };
  const inward = t => { const dx = 220 * (1 - t) + 88 * t, dy = -20 * (1 - t) + 296 * t, d = Math.hypot(dx, dy); return [-dy / d, dx / d]; };
  const top = [...Array(13)].map((_, j) => crest(j / 12));
  const under = [...Array(13)].map((_, j) => { const t = 1 - j / 12, [x, y] = crest(t), [nx, ny] = inward(t), d = j % 2 ? 13 : 23; return [x + nx * d, y + ny * d]; });
  const maneP = `<path d="M${top.map(P_).join(" L")} L${under.map(P_).join(" L")} Z" fill="${mane}" stroke="${maneD}" stroke-width="2" stroke-linejoin="round"/>`;
  // (the head, hanging from the poll: the forehead and the long nose in front, the round cheek and the jaw behind, the
  // muzzle down in the grass)
  const head = `<path d="M236 -158 Q254 -152 256 -134 L276 -46 Q284 -20 274 -8 Q262 -2 252 -10 Q246 -24 236 -46 L220 -82 Q200 -104 206 -124 Q214 -144 236 -158 Z" fill="${coat}" stroke="${ln}" stroke-width="3" stroke-linejoin="round"/>`;
  const ear = (a, b, c2, fill) => `<path d="M${a} L${b} L${c2} Z" fill="${fill}" stroke="${ln}" stroke-width="2.5" stroke-linejoin="round"/>`;
  const mark = H.mark === "star" ? `<path d="M250 -142 L256 -134 L250 -126 L244 -134 Z" fill="${white}"/>`
    : H.mark === "stripe" ? `<path d="M250 -128 L255 -129 L275 -40 L270 -38 Z" fill="${white}"/>`
    : H.mark === "blaze" ? `<path d="M244 -136 L256 -137 L277 -44 Q282 -26 274 -14 L266 -24 L261 -46 Z" fill="${white}"/>` : "";
  const face = mark + `<ellipse cx="238" cy="-118" rx="4.5" ry="5.5" fill="#1f1410"/><circle cx="239.5" cy="-120" r="1.5" fill="#ffffff"/>` +
    `<path d="M270 -30 q5 -2 6 3 q-2 3 -5 1" fill="#1f1410"/><path d="M254 -13 Q262 -15 270 -11" fill="none" stroke="${ln}" stroke-width="2"/>`;
  const forelock = `<path d="M234 -160 Q258 -154 259 -126 Q248 -136 230 -146 Z" fill="${mane}" stroke="${maneD}" stroke-width="1.8" stroke-linejoin="round"/>`;
  // (the grass it is pulling at, in front of its muzzle)
  const grass = `<path d="M256 2 q-3 -12 -9 -18 M264 2 q0 -14 3 -22 M272 2 q3 -10 9 -15 M281 2 q2 -6 7 -9" fill="none" stroke="#4f8a35" stroke-width="3.2" stroke-linecap="round"/>`;
  return `<ellipse class="flat" cx="40" cy="2" rx="215" ry="13" fill="#000000" opacity="0.14"/>` +
    `<rect data-footprint="1" x="-165" y="-24" width="452" height="26" fill="none" stroke="none"/>` +
    leg(100, -200, -5, 4, true, false) + leg(-131, -214, 5, 6, false, false) + leg(-115, -218, -3, 4, false, true) + leg(124, -204, 9, 0, true, true) +
    tail + body + neck + maneP + ear("226 -150", "212 -184", "238 -160", far) + head + face + forelock + ear("234 -156", "227 -194", "248 -162", coat) + grass;
}
// The pasture's water trough, in a horse's units as the horses are (its foot line at 0): planks on two low trestles, the
// water just under its rim, rings spreading on it with the quick LFO; its shadow flat; its footprint a trough's width deep
function pastureTroughSvg(o) {
  const wood = "#8a5a32", dk = "#4e3019", rim = "#a97445";
  return `<ellipse class="flat" cx="0" cy="2" rx="140" ry="10" fill="#000000" opacity="0.16"/>` +
    `<rect data-footprint="1" x="-130" y="-24" width="260" height="26" fill="none" stroke="none"/>` +
    `<path d="M-114 0 L-114 -16 L-100 -16 L-100 0 Z M100 0 L100 -16 L114 -16 L114 0 Z" fill="${dk}"/>` +
    // (inside, over the near rim: the far side's inner face, the water)
    `<path d="M-126 -100 L126 -100 L119 -111 L-119 -111 Z" fill="#3f2814"/>` +
    `<path d="M-122 -101 L122 -101 L117 -107 L-117 -107 Z" fill="#7cc0dc"/>` +
    `<ellipse cx="38" cy="-104" rx="4" ry="0.6" fill="none" stroke="#e6f8ff" stroke-width="1.6" opacity="0">` +
    lfoTimed(o, "fast", 0.3, `<animate attributeName="rx" values="4;30" dur="1.8s" repeatCount="indefinite"/>` +
      `<animate attributeName="ry" values="0.6;2.6" dur="1.8s" repeatCount="indefinite"/><animate attributeName="opacity" values="0.9;0" dur="1.8s" repeatCount="indefinite"/>`) + `</ellipse>` +
    // (its near side, planks, and the rim)
    `<path d="M-120 -14 L120 -14 L128 -100 L-128 -100 Z" fill="${wood}" stroke="${dk}" stroke-width="3" stroke-linejoin="round"/>` +
    `<path d="M-123 -43 L123 -43 M-126 -72 L126 -72 M-60 -16 L-62 -98 M60 -16 L62 -98" stroke="${dk}" stroke-width="2" opacity="0.55"/>` +
    `<path d="M-128 -100 L128 -100" stroke="${rim}" stroke-width="5" stroke-linecap="round"/>`;
}
function pastureScene(c) {
  const { x0, y0, x1, W, H, g, k, pad, R, P, grad, all, land, o } = c;
  const horses = Math.max(0, Math.min(PASTURE_HORSES.length, Math.round(o.pastureHorses ?? 3))), flowers = Math.max(0, Math.min(24, Math.round(o.pastureFlowers ?? 10)));
  const at = up => g - up * k, horizon = at(172), fenceUp = 162, fenceY = at(fenceUp), tree = { x: -0.4 * W, y: at(50) };
  // the sky, the sun, clouds drifting with the slow LFO
  let s = SEP + part("pasture-sky", [c.F, sceneLfoKey(o, "slow")], () => `<defs>${grad("bg-sky", "#79bdf0", "#e6f5ff", y0, horizon)}${cloudDef("bg-cloud", 1)}</defs>` + all("url(#bg-sky)") +
    `<defs><radialGradient id="bg-pasture-sun"><stop offset="0.5" stop-color="#fff6c8" stop-opacity="0.9"/><stop offset="1" stop-color="#fff6c8" stop-opacity="0"/></radialGradient></defs>` +
    `<circle cx="${f1(x1 - W * 0.12)}" cy="${f1(y0 + H * 0.11)}" r="${f1(66 * k)}" fill="url(#bg-pasture-sun)"/><circle cx="${f1(x1 - W * 0.12)}" cy="${f1(y0 + H * 0.11)}" r="${f1(34 * k)}" fill="#ffe680"/>` +
    [[0.14, 0.1, 0.75], [0.46, 0.05, 0.95], [0.7, 0.19, 0.6]].map(([u, v, z], i) => driftingCloud(x0 + W * u, y0 + H * v, k * z, i, o)).join(""));
  // far off: rolling hills and round trees on the horizon (the shared layers, ticket 212)
  s += SEP + part("pasture-far", c.F, () => farLayers(c, horizon, { mountains: "hills", forest: "broadleaf", beach: false, size: 170 }));
  // the field, rolling away in bands, paler farther off; the grass's texture (short strokes, smaller farther off); the
  // big tree's shade on it
  s += SEP + part("pasture-field", c.F, () => {
    let t = land(at(171), 2, "#b8d991", 0.4) + land(at(118), 6, "#a2cd77", 1.3) + land(at(50), 7, "#92c467", 2.2) + land(at(-30), 8, "#86bc5b", 0.9), d = "";
    for (let i = 0; i < 70; i++) { const up = 165 - 260 * R(i, 5010), f = pastureDepth(up), x = W * (R(i, 5011) - 0.5) * 1.05, y = at(up);
      d += `M${P(x, y)} l${f1(-2 * f * k)} ${f1(-9 * f * k)} M${P(x + 4 * f * k, y)} l${f1(2 * f * k)} ${f1(-8 * f * k)}`; }
    return t + `<path d="${d}" fill="none" stroke="#6e9f47" stroke-width="${f1(1.3 * k)}" stroke-linecap="round" opacity="0.5"/>` +
      `<ellipse cx="${f1(tree.x + 60 * k)}" cy="${f1(tree.y + 8 * k)}" rx="${f1(210 * k)}" ry="${f1(30 * k)}" fill="#3f7a32" opacity="0.28"/>`;
  });
  // the fence along the back, posts and three rails, the gate in it shut (five bars and a brace, its latch), and a
  // bird sitting on the gate's post (an object at the fence's foot: behind wherever the friend can stand). Drawn in a
  // horse's units at its depth (u), as the horses and the trough are: its posts 260 tall, a little under a horse's
  // withers. (ticket 503, after review: 162 up, the back line just in front of it, far enough back for the friend to
  // walk behind every horse.) Its rails, posts and gate are drawnBy "fence", as the witch's woods' are (ticket 368),
  // so 🔬's 🪵 fences chip finds them; the bird is not fence
  s += SEP + part("pasture-fence", c.F, () => {
    const u = pastureHorseScale(k, fenceUp), gx = 0.035 * W, gw = 570 * u;
    const ph = 260 * u, pw = 28 * u, gp = 305 * u, gpw = 41 * u, step = 500 * u, L = gx - gw / 2, Rt = gx + gw / 2, rt = 11.5 * u;
    let posts = "", rails = "";
    const post = (x, h, w) => `M${P(x - w / 2, fenceY)} L${P(x - w / 2, fenceY - h)} L${P(x, fenceY - h - 9.5 * u)} L${P(x + w / 2, fenceY - h)} L${P(x + w / 2, fenceY)} Z `;
    for (let x = L - step; x > x0 - pad - step; x -= step) posts += post(x, ph, pw);
    for (let x = Rt + step; x < x1 + pad + step; x += step) posts += post(x, ph, pw);
    for (const h of [0.3, 0.6, 0.88]) { const y = fenceY - h * ph;
      rails += `M${P(x0 - pad, y - rt)} L${P(L, y - rt)} L${P(L, y + rt)} L${P(x0 - pad, y + rt)} Z M${P(Rt, y - rt)} L${P(x1 + pad, y - rt)} L${P(x1 + pad, y + rt)} L${P(Rt, y + rt)} Z `; }
    const a = L + gpw / 2, b = Rt - gpw / 2, gh = 240 * u, bt = 9.5 * u, bw = 22 * u, bars = [0.12, 0.31, 0.5, 0.69, 0.88].map(h => { const y = fenceY - h * gh;
      return `M${P(a, y - bt)} L${P(b, y - bt)} L${P(b, y + bt)} L${P(a, y + bt)} Z`; }).join(" ") +
      ` M${P(a, fenceY - 0.05 * gh)} L${P(a + bw, fenceY - 0.05 * gh)} L${P(a + bw, fenceY - 0.95 * gh)} L${P(a, fenceY - 0.95 * gh)} Z` +
      ` M${P(b - bw, fenceY - 0.05 * gh)} L${P(b, fenceY - 0.05 * gh)} L${P(b, fenceY - 0.95 * gh)} L${P(b - bw, fenceY - 0.95 * gh)} Z`;
    const bx = L, by = fenceY - gp - 9.5 * u, q = 5.4 * u;
    const bird = `<path d="M${P(bx - 6 * q, by - 5 * q)} L${P(bx - 14 * q, by - 1 * q)} L${P(bx - 5 * q, by - 2 * q)} Z" fill="#5a3d2b"/>` +
      `<ellipse cx="${f1(bx)}" cy="${f1(by - 5 * q)}" rx="${f1(7 * q)}" ry="${f1(5 * q)}" fill="#6b4a33"/>` +
      `<ellipse cx="${f1(bx + 2.5 * q)}" cy="${f1(by - 4 * q)}" rx="${f1(4 * q)}" ry="${f1(3.5 * q)}" fill="#e2683a"/>` +
      `<circle cx="${f1(bx + 5 * q)}" cy="${f1(by - 10 * q)}" r="${f1(3.6 * q)}" fill="#6b4a33"/>` +
      `<path d="M${P(bx + 8.4 * q, by - 10.8 * q)} l${f1(3 * q)} ${f1(1 * q)} l${f1(-3 * q)} ${f1(1 * q)} Z" fill="#e8b923"/>` +
      `<circle cx="${f1(bx + 6 * q)}" cy="${f1(by - 11 * q)}" r="${f1(0.9 * q)}" fill="#111111"/>`;
    const sw = f1(5 * u);
    return obj("fence and gate", fenceY, drawnBy("fence", `<path d="${rails}" fill="#8a6a45" stroke="#5a4128" stroke-width="${sw}"/>` +
      `<path d="${posts}" fill="#7a5a38" stroke="#4e3822" stroke-width="${sw}"/>` +
      `<path d="${bars}" fill="#b08a5a" stroke="#6e5232" stroke-width="${f1(4.5 * u)}"/>` +
      `<path d="M${P(a + 19 * u, fenceY - 0.1 * gh)} L${P(b - 19 * u, fenceY - 0.9 * gh)}" stroke="#9a7648" stroke-width="${f1(19 * u)}" stroke-linecap="round"/>` +
      `<path d="${post(L, gp, gpw)}${post(Rt, gp, gpw)}" fill="#6e4f30" stroke="#4e3822" stroke-width="${sw}"/>` +
      `<rect x="${f1(b - 13 * u)}" y="${f1(fenceY - 0.62 * gh)}" width="${f1(32 * u)}" height="${f1(19 * u)}" fill="#3a3a3a"/>`) + bird);
  });
  // the big shade tree (an object at its foot), and the water trough by it, farther back (an object at its foot line)
  s += SEP + part("pasture-tree", c.F, () => obj("big tree", tree.y, `<g ${tf(tree.x, tree.y, 1.45 * k)}>${leafyTree(5030, { wide: 1.3, h: 330 })}</g>`));
  s += SEP + part("pasture-trough", [c.F, sceneLfoKey(o, "fast")], () => { const up = 95, sc = pastureHorseScale(k, up);
    return obj("water trough", at(up), `<g ${tf(-0.3 * W, at(up), sc)}>${pastureTroughSvg(o)}</g>`); });
  // the horses grazing (each an object where its hooves stand)
  s += SEP + part("pasture-horses", [c.F, horses, sceneLfoKey(o, "fast")], () => PASTURE_HORSES.slice(0, horses).map((Hh, i) => {
    const sc = pastureHorseScale(k, Hh.up), x = W * Hh.u, y = at(Hh.up);
    return obj(`grazing horse, ${Hh.name}`, y, `<g ${tf(x - Hh.face * 60 * sc, y, Hh.face * sc, sc)}>${grazingHorseSvg(Hh, o, Hh.off, i)}</g>`);
  }).join(""));
  // patches of wildflowers (daisies, buttercups, poppies, cornflowers, clover: two to four of a kind, each its petals
  // round its middle; a patch three shapes, its stems and leaves, its petals, its middles) and tufts of grass, smaller
  // farther off, in rows by depth (ticket 297: each row an object, the friend among them), each nearer row swaying a
  // little with the slow LFO, about its foot, its own way in (the far rows held still: their sway would hardly show, and
  // each swaying row is drawn again every frame across the picture's width: 60 frames a second first)
  s += SEP + part("pasture-grass", [c.F, flowers, sceneLfoKey(o, "slow")], () => {
    const items = [], band = 36 * k;
    for (let i = 0; i < flowers; i++) {
      const up = 132 - 226 * R(i, 5031), y = at(up), sz = pastureDepth(up) * k, x = W * (R(i, 5032) - 0.5) * 1.02, [, col, mid, n, w] = PASTURE_FLOWERS[i % PASTURE_FLOWERS.length];
      let stems = "", petals = "", mids = "";
      for (let j = 0, m = 2 + Math.floor(3 * R(i, 5033)); j < m; j++) {
        const fx = x + (j - (m - 1) / 2) * 9 * sz + (R(i * 5 + j, 5034) - 0.5) * 6 * sz, h = (30 + 24 * R(i * 5 + j, 5035)) * sz, lean = (R(i * 5 + j, 5036) - 0.5) * 12 * sz;
        const hx = fx + lean, hy = y - h, r = (col === "#d77fb8" ? 4.2 : 5.6) * sz, rot = R(i * 5 + j, 5037) * Math.PI;
        stems += `M${P(fx, y)} Q${P(fx + lean * 0.2, y - h * 0.6)} ${P(hx, hy)} `;
        for (let q = 0; q < n; q++) { const a = rot + q * 2 * Math.PI / n, pt = (b, rr) => P(hx + rr * Math.cos(b), hy + rr * Math.sin(b));
          petals += `M${P(hx, hy)} Q${pt(a - w, r * 0.85)} ${pt(a, r)} Q${pt(a + w, r * 0.85)} ${P(hx, hy)} Z `; }
        const r2 = (col === "#d77fb8" ? 1.6 : 1.9) * sz;
        mids += `M${P(hx - r2, hy)} a${f1(r2)} ${f1(r2)} 0 1 0 ${f1(2 * r2)} 0 a${f1(r2)} ${f1(r2)} 0 1 0 ${f1(-2 * r2)} 0 Z `;
      }
      // (two leaves at the patch's foot)
      stems += `M${P(x - 2 * sz, y)} q${f1(-6 * sz)} ${f1(-4 * sz)} ${f1(-10 * sz)} ${f1(-12 * sz)} M${P(x + 2 * sz, y)} q${f1(6 * sz)} ${f1(-3 * sz)} ${f1(11 * sz)} ${f1(-10 * sz)}`;
      // (each patch drawnBy "wildflowers", a plant drawing, so 🔬's 🌷 flowers chip finds it)
      items.push({ y, svg: drawnBy("wildflowers", `<path d="${stems}" fill="none" stroke="#4f8a35" stroke-width="${f1(1.6 * sz)}" stroke-linecap="round"/>` +
        `<path d="${petals}" fill="${col}"${col === "#ffffff" ? ` stroke="#d9d9d9" stroke-width="${f1(0.5 * sz)}"` : ""}/><path d="${mids}" fill="${mid}"/>`) });
    }
    for (let i = 0; i < 10; i++) { const up = 110 - 205 * R(i, 5038), y = at(up), f = pastureDepth(up);
      items.push({ y, svg: grassTuft(W * (R(i, 5039) - 0.5) * 1.02, y, (26 + 14 * R(i, 5040)) * f * k, 5041 + i, "#5e9a3c", 1.8 * f * k) }); }
    const rows = new Map();
    for (const it of items) { const j = Math.floor(it.y / band); rows.set(j, (rows.get(j) || "") + it.svg); }
    return [...rows.keys()].sort((a, b) => a - b).map((j, n) => { const foot = (j + 0.5) * band, d = 4.5 + (n % 3) * 0.8;
      if (foot < at(70)) return obj(`grass and flowers ${n + 1}`, foot, rows.get(j));
      return obj(`grass and flowers ${n + 1}`, foot, `<g transform="translate(0 ${f1(foot)})"><g>` +
        sceneLfo(o, "slow", ((n * 0.29) % 1), "transform", (v, amp) => f1((1.5 + 4 * amp) * v),
          `<animateTransform attributeName="transform" type="skewX" values="-2.5;2.5;-2.5" dur="${d.toFixed(1)}s" begin="-${(n * 1.3).toFixed(1)}s" repeatCount="indefinite"/>`, "skewX") +
        `<g transform="translate(0 ${f1(-foot)})">${rows.get(j)}</g></g></g>`); }).join("");
  });
  // butterflies over the flowers (an object at the depth of the ground under the nearer one)
  s += SEP + part("pasture-butterflies", [c.F, sceneLfoKey(o, "fast"), sceneLfoKey(o, "wings")], () =>
    obj("butterflies", at(10), butterfliesSvg(c, [[-0.24 * W, at(10), 0.9], [0.16 * W, at(60), 0.8]])));
  return s;
}
// Our imagination: a room of endless white, the empty white of a film's
// training world, the floor only a soft grey toward us; and on its far
// wall a big map of every place there is, each a framed picture of it to
// go through, a column to each kind of place (PLACE_GROUPS) with its name,
// the places down it joined like stations on a line by a dotted path. A
// picture of this room holds no pictures (they would go on forever).
// (ticket 99) The map and its pictures are on the wall, the rest is floor
// and glow. (ticket 145) Its only objects are the ladders, one at each
// column's right, floating a little off the floor at the wall's foot and
// bobbing slowly, to climb to the pictures up the column; its back wall is
// solid (SCENE_BACK), so the ladders are the way up.
const imaginationWall = (g, k) => g - 60 * k;
// (ticket 153, Matthew) The empty room: the imagination's room with nothing
// in it, its back wall only a line where it meets the floor, so the
// controls and the actions can be tried on an old, slow computer (the
// black laptop) with nothing in the picture moving but the friend. No
// objects, no animation, no music of its own; its back wall is solid, as
// the imagination's is.
function emptyroomScene(c) {
  const { x0, y1, g, W, k, pad, all } = c, wall = imaginationWall(g, k);
  return SEP + part("emptyroom", c.F, () => all("#ffffff") +
    `<rect x="${f1(x0 - pad)}" y="${f1(wall)}" width="${f1(W + 2 * pad)}" height="${f1(y1 - wall + pad)}" fill="#f1f3f7"/>` +
    `<line x1="${f1(x0 - pad)}" y1="${f1(wall)}" x2="${f1(x0 + W + pad)}" y2="${f1(wall)}" stroke="#c3c9d4" stroke-width="${f1(2 * k)}"/>`);
}
// (ticket 220, Matthew: "bubble space -- like the empty background, except
// populated with bubbles that collide with each other, and stick, and collide
// with the player and pop. Controls for number of bubbles initially, their
// rate of movement (the ambient wind in the room, jostling them around gently
// or quickly), and up to three 'bubble themes' (A, B and C) with controls for
// their color, etc") The empty room's wall and floor, in a paler blue, and a
// layer for the bubbles, in front of everything; bubbleTick moves them each
// frame while the place is shown (bubbleRun, from draw()).
const BUBBLE_COLORS = ["#9ad7ff", "#ffb3e6", "#c8f7c5", "#fff3a3", "#d7c4ff", "#ffffff", "#ffc9a3"];
const BUBBLE_POP_INSTS = [["bubble", "🫧 bloop"], ["pizz", "🎻 pluck"], ["musicbox", "🎶 music box"], ["bell", "🔔 bell"], ["zap", "⚡ zap"]];
function bubblespaceScene(c) {
  const { x0, y1, g, W, k, pad, all } = c, wall = imaginationWall(g, k);
  return SEP + part("bubblespace", c.F, () => all("#f6fbff") +
    `<rect x="${f1(x0 - pad)}" y="${f1(wall)}" width="${f1(W + 2 * pad)}" height="${f1(y1 - wall + pad)}" fill="#e9f2fb"/>` +
    `<line x1="${f1(x0 - pad)}" y1="${f1(wall)}" x2="${f1(x0 + W + pad)}" y2="${f1(wall)}" stroke="#bdd0e4" stroke-width="${f1(2 * k)}"/>`) +
    SEP + obj("bubbles", Infinity, `<g id="bubble-layer"></g>`);
}
// (ticket 290, for N: a room "made of bubbles", "the look of fractal bubble foam, with sizes and placements chosen to mimic
// that kind of pattern, on the walls and floors") The bubble room: a square room (roomFrame, as a tree room's), each of its
// surfaces (FOAM_SURFACES: the back wall, the side walls, the floor, the ceiling) covered as far as its settings say in
// bubble foam: a few big bubbles and more and more smaller ones packed into the gaps between them, as foam is (foamPack),
// each a rim, a film and, on the bigger ones, a shine, laid flat on its surface in the room's perspective (a side wall's, the
// floor's and the ceiling's foreshortened, a unit toward us a z of 1/800, as roomSpace lays things on the walls). With a
// rainbow sheen, the rims take turns at three tints. 〰️ how much the foam moves: some of its bubbles breathing, a few popping
// and growing back (lifeOf); still at 0 (Matthew: "dynamic, not static": moving from the start).
// Each surface's foam is an object of its own. Bubble makers (🫧, a placed thing) blow bubbles into it, or anywhere.
const FOAM_SURFACES = [["Back", "back wall", true], ["Left", "left wall", true], ["Right", "right wall", true], ["Floor", "floor", false], ["Ceil", "ceiling", false]];
// (ticket 383, ^du_report(12)'s next step 4, Matthew: "Yes, I want moving foam back on by default: build 383's layered foam") The
// moving foam as layers the browser moves without painting the picture again: the room's backdrop (its shell and its five
// surfaces' still pictures) under a see-through picture (#pic-under), and each moving bubble a page element of its own between
// them (#foam-layers), scaled by a CSS animation (foamLayersRun); none of the picture's own animations (the stuck-out balls hold
// still). The spike (diag-383): the room standing, Chrome at full speed 52 -> 60 fps, at the slow tier 22 -> 56-58, Firefox at
// Matthew's screen 28 -> 60. on: false draws it as before (SMIL inside the picture), to compare.
const FOAM_LAYERS = { on: true };
const foamMemo = new Map();
function foamPack(seed, w, h, rMax, rMin) {
  const mk = [seed, w, h, rMax, rMin].map(v => v.toFixed(2)).join();
  if (foamMemo.has(mk)) return foamMemo.get(mk);
  // (each at a spot tried at random, as big as it can be there, up to rMax, without overlapping one already in: so each
  // touches its neighbours, and the gaps left fill with smaller and smaller ones, as foam's do)
  const R = seeded(seed), out = [], gap = rMin * 0.12;
  for (let i = 0; i < 2600 && out.length < 260; i++) {
    const x = R(i * 3) * w, y = R(i * 3 + 1) * h;
    let r = rMax;
    for (const b of out) { const d = Math.hypot(b.x - x, b.y - y) - b.r - gap; if (d < r) { r = d; if (r < rMin) break; } }
    if (r >= rMin) out.push({ x, y, r, j: Math.floor(R(i * 3 + 2) * 3) });
  }
  if (foamMemo.size > 40) foamMemo.clear();
  foamMemo.set(mk, out);
  return out;
}
function bubbleroomScene(c) {
  const { x0, y0, y1, W, H, k, pad, P, o } = c, { floor, top, bw, vy, out } = roomFrame(c), wh = floor - top, B = o;   // (read through o: a change to one draws it again)
  const base = B.foamColor || "#9ad7ff", tints = B.foamRainbow ?? true ? [base, mix(base, "#ff9ad8", 0.45), mix(base, "#b6f5a8", 0.45)] : [base, base, base];
  const move = Math.max(0, Math.min(100, B.foamMove ?? 0)) / 100, seed = B.foamSeed ?? 1, layered = FOAM_LAYERS.on && move > 0;   // (ticket 383)
  const at = (X, Y, z) => { const f = 1 / (1 + z); return [X * f, vy + (Y - vy) * f]; };
  // (a bubble of radius r, in its surface's own units, about the origin; tf lays it on the surface)
  // (the smallest a rim alone; bigger ones a light inside the rim; the biggest a shine too: fewer shapes to draw)
  const bub = (r, j, tf, life = "") => { const mark = life.startsWith(" data-foam") ? life : ""; if (mark) life = "";   // (ticket 383: marked, to be a page layer)
    return `<g transform="${tf}"${mark}>${life ? `<g>${life}` : ""}<circle r="${f1(r)}" fill="${tints[j]}" fill-opacity="0.2" stroke="${tints[j]}" stroke-width="${f1(Math.max(0.8, r * 0.1))}"/>` +
    (r > 5 * k ? `<circle r="${f1(r * 0.9)}" fill="none" stroke="#ffffff" stroke-opacity="0.55" stroke-width="${f1(Math.max(0.5, r * 0.04))}"/>` : "") +
    (r > 9 * k ? `<ellipse cx="${f1(-r * 0.36)}" cy="${f1(-r * 0.38)}" rx="${f1(r * 0.26)}" ry="${f1(r * 0.15)}" fill="#ffffff" opacity="0.75" transform="rotate(-35 ${f1(-r * 0.36)} ${f1(-r * 0.38)})"/>` : "") + (life ? `</g>` : "") + `</g>`; };
  const mat = (a, b, cc, d, e, f) => `matrix(${[a, b, cc, d].map(v => v.toFixed(4)).join(" ")} ${f1(e)} ${f1(f)})`;
  // (ticket 290, Matthew on 0.0.238: "there's no dynamic aspect to the walls or floor --- Can we fix that without getting a
  // huge performance penalty?") Not the whole foam sliding (a few units, too little to see, and all of it drawn again every
  // frame): some of its bubbles, each on its own, breathing in and out with the slow LFO (each its own way in, its own
  // depth), and a few popping and growing back now and then, each drawing again only where it is; how many, 〰️ how much the
  // foam moves: up to half of the big ones breathing (16 at most a surface), and one in twenty-five of the middling ones
  // popping (4 at most): some 120 moving in all, each over a small patch (a whole surface moving was all of it drawn again
  // every frame)
  const lives = {};
  const lifeOf = (seedK, i, r) => { if (!move) return ""; const L = seeded(seedK * 31 + 7), v = L(i * 2), n = lives[seedK] || (lives[seedK] = { breathe: 0, pop: 0 });
    if (r >= 12 * k && v < 0.5 * move && n.breathe < 16) { n.breathe++; const a = (0.05 + 0.07 * L(i * 2 + 1)).toFixed(3);
      if (layered) return ` data-foam="b ${a} ${(3 + 3 * L(i * 2 + 1)).toFixed(1)} -${(6 * v).toFixed(1)}"`;   // (ticket 383: breathing, as a page layer)
      return lfoTimed(o, "slow", L(i * 2 + 1), `<animateTransform attributeName="transform" type="scale" values="1;${(1 + +a).toFixed(3)};${(1 - +a).toFixed(3)};1" dur="${(3 + 3 * L(i * 2 + 1)).toFixed(1)}s" begin="-${(6 * v).toFixed(1)}s" repeatCount="indefinite"/>`); }
    if (r >= 6 * k && r < 12 * k && v > 1 - 0.04 * move && n.pop < 4) { n.pop++; const d = (7 + 8 * L(i * 2 + 1)).toFixed(1);
      if (layered) return ` data-foam="p 0 ${d} -${(+d * L(i * 2 + 1)).toFixed(1)}"`;   // (ticket 383: popping, as a page layer)
      return `<animateTransform attributeName="transform" type="scale" values="1;1;0.05;0.05;1" keyTimes="0;0.72;0.75;0.8;1" dur="${d}s" begin="-${(+d * L(i * 2 + 1)).toFixed(1)}s" repeatCount="indefinite"/>`; }
    return ""; };
  // (ticket 290, Matthew: "bubble room with one bubble machine is stressing my modern laptop") What does not move is drawn
  // once as a picture: an <image> of a little SVG document of its own (a data: URL, so the still pictures can show it too),
  // which the browser paints as a copy of itself, not as its thousands of see-through shapes, each time something moving
  // over it (a breathing bubble, a maker's, the friend) is drawn again; what moves stays live over it
  // (each only as big as the part of it inside the picture: the picture's frame overscans far past what is seen, and a
  // picture is kept in memory at the size it is drawn)
  const imageOf = (inner, box, defs = "") => {
    const ax = Math.max(box[0], x0 - 2), ay = Math.max(box[1], y0 - 2), ax1 = Math.min(box[0] + box[2], x0 + W + 2), ay1 = Math.min(box[1] + box[3], y1 + 2);
    if (!(ax1 > ax && ay1 > ay)) return null;
    const [bx, by, bwd, bht] = [ax, ay, ax1 - ax, ay1 - ay].map(v => +f1(v));
    const doc = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="${bx} ${by} ${bwd} ${bht}" width="${bwd}" height="${bht}">${defs}${inner}</svg>`.replace(/"/g, "'");
    return { bx, by, bwd, bht, href: "data:image/svg+xml;charset=utf-8," + doc.replace(/%/g, "%25").replace(/#/g, "%23").replace(/</g, "%3C").replace(/>/g, "%3E") }; };
  const asImage = (inner, box, defs = "") => { const m = inner && imageOf(inner, box, defs);
    // (ticket 384: no press is taken by it: an <image> takes one anywhere in its rectangle, its see-through parts too, and the front
    // band of balls is a picture the size of the scene, in front of the friend: in the bubble room no press reached the friend, a
    // maker or a lever behind it, and the gate's drag never took hold. Scenery; what moves on it is live, over it)
    return m ? `<image x="${m.bx}" y="${m.by}" width="${m.bwd}" height="${m.bht}" preserveAspectRatio="none" pointer-events="none" href="${m.href}"/>` : ""; };
  const sets = (list, draw, seedK, box) => { let still = "", live = "";
    list.forEach((b, i) => { const life = lifeOf(seedK, i, b.r); if (life) live += draw(b, life); else still += draw(b, ""); });
    return asImage(still, box) + live; };
  const rMax = 40 * k, rMin = 4 * k, clip = (id, pts) => `<defs><clipPath id="${id}"><polygon points="${pts}"/></clipPath></defs>`;
  const fEdge = (W / 2 + pad) / bw, dSide = 800 * (1 - 1 / fEdge), fBot = (y1 + pad - vy) / (floor - vy), dFloor = 800 * (1 - 1 / fBot), fTop = (vy - (y0 - pad)) / (vy - top), dCeil = 800 * (1 - 1 / fTop);
  // (ticket 383: layered, the backdrop, the shell and the foam's surfaces, in a group of its own that foamLayersRun puts under the picture)
  let s = (layered ? SEP + '<g data-under="">' : "") + SEP + part("bubbleroom-shell", c.F, () => {
    let t = `<rect x="${f1(x0 - pad)}" y="${f1(y0 - pad)}" width="${f1(W + 2 * pad)}" height="${f1(H + 2 * pad)}" fill="#d9ecfb"/>`;
    t += `<rect x="${f1(-bw)}" y="${f1(top)}" width="${f1(2 * bw)}" height="${f1(wh)}" fill="#e6f3ff"/>`;
    for (const sd of [-1, 1]) { const [ax, ay] = out(sd * bw, top, y0 - pad), [bx, by] = out(sd * bw, floor, y1 + pad);
      t += `<polygon points="${P(sd * bw, top)} ${P(ax, ay)} ${P(sd * (W + pad), y0 - pad)} ${P(sd * (W + pad), y1 + pad)} ${P(bx, by)} ${P(sd * bw, floor)}" fill="#cfe3f6"/>`; }
    t += `<polygon points="${P(-bw, top)} ${P(bw, top)} ${P(...out(bw, top, y0 - pad))} ${P(...out(-bw, top, y0 - pad))}" fill="#eef7ff"/>`;
    t += `<polygon points="${P(-bw, floor)} ${P(bw, floor)} ${P(...out(bw, floor, y1 + pad))} ${P(...out(-bw, floor, y1 + pad))}" fill="#c4dcf3"/>`;
    return t + [[-bw, top, -bw, floor], [bw, top, bw, floor], [-bw, floor, bw, floor], [-bw, top, bw, top]].map(([a, b, cc, d]) => `<line x1="${f1(a)}" y1="${f1(b)}" x2="${f1(cc)}" y2="${f1(d)}" stroke="#b3cde6" stroke-width="${f1(2 * k)}"/>`).join("");
  });
  const key = K => [c.F, B["foam" + K], B["foam" + K + "H"], base, B.foamRainbow ?? true, move, move && !layered ? sceneLfoKey(o, "slow") : "", seed, layered];
  // the back wall: from the floor up, as high as its setting
  if (B.foamBack ?? true) s += SEP + part("bubbleroom-foam-back", key("Back"), () => { const h = wh * (B.foamBackH ?? 70) / 100;
    return wallObj("foam, back wall", clip("bg-foam-back", `${P(-bw, top)} ${P(bw, top)} ${P(bw, floor)} ${P(-bw, floor)}`) + `<g clip-path="url(#bg-foam-back)">` +
      sets(foamPack(seed * 7 + 1, 2 * bw, h, rMax, rMin), (b, life) => bub(b.r, b.j, `translate(${f1(-bw + b.x)} ${f1(floor - b.y)})`, life), 1, [-bw, top, 2 * bw, wh]) + `</g>`); });
  // the side walls: along each, from the back corner toward us, from the floor up
  for (const [sd, K] of [[-1, "Left"], [1, "Right"]]) if (B["foam" + K] ?? true) s += SEP + part("bubbleroom-foam-" + K, key(K), () => {
    const h = wh * (B["foam" + K + "H"] ?? 55) / 100, [ax, ay] = out(sd * bw, top, y0 - pad), [bx, by] = out(sd * bw, floor, y1 + pad);
    return wallObj(`foam, ${sd < 0 ? "left" : "right"} wall`, clip("bg-foam-" + K, `${P(sd * bw, top)} ${P(ax, ay)} ${P(sd * (W + pad), y0 - pad)} ${P(sd * (W + pad), y1 + pad)} ${P(bx, by)} ${P(sd * bw, floor)}`) +
      `<g clip-path="url(#bg-foam-${K})">` + sets(foamPack(seed * 7 + (sd < 0 ? 2 : 3), dSide, h, rMax, rMin), (b, life) => {
        const z = -b.x / 800, f = 1 / (1 + z), Y = floor - b.y, [ex, ey] = at(sd * bw, Y, z), q = f * f / 800;
        return bub(b.r, b.j, mat(sd * bw * q, (Y - vy) * q, 0, f, ex, ey), life); }, sd < 0 ? 2 : 3, sd < 0 ? [x0 - pad, y0 - pad, -bw - x0 + pad, H + 2 * pad] : [bw, y0 - pad, x0 + W + pad - bw, H + 2 * pad]) + `</g>`); });
  // the floor and the ceiling: out from the back wall toward us, as far as their settings
  for (const [K, Y, D, nm] of [["Floor", floor, dFloor, "floor"], ["Ceil", top, dCeil, "ceiling"]]) if (B["foam" + K] ?? true) s += SEP + part("bubbleroom-foam-" + K, key(K), () => {
    const d = D * (B["foam" + K + "H"] ?? (K === "Floor" ? 100 : 60)) / 100, edge = K === "Floor" ? y1 + pad : y0 - pad;
    const body = clip("bg-foam-" + K, `${P(-bw, Y)} ${P(bw, Y)} ${P(...out(bw, Y, edge))} ${P(...out(-bw, Y, edge))}`) + `<g clip-path="url(#bg-foam-${K})">` +
      sets(foamPack(seed * 7 + (K === "Floor" ? 4 : 5), 2 * bw, d, rMax, rMin), (b, life) => {
        const X = -bw + b.x, z = -b.y / 800, f = 1 / (1 + z), [ex, ey] = at(X, Y, z), q = f * f / 800;
        return bub(b.r, b.j, mat(f, 0, X * q, (Y - vy) * q, ex, ey), life); }, K === "Floor" ? 4 : 5, K === "Floor" ? [x0 - pad, floor, W + 2 * pad, y1 + pad - floor] : [x0 - pad, y0 - pad, W + 2 * pad, top - y0 + pad]) + `</g>`;
    return K === "Floor" ? floorObj("foam, floor", body) : wallObj("foam, ceiling", body); });
  // (ticket 290, Matthew: "each bubble on the floor and wall should have ones in the room that are stuck to it --- to create
  // a richer psuedo 3d experience"; "each surface (wall, floor) has a frequency of spawning bubbles that are adjacent,
  // sticking into the room's volume") Out of the foam, round bubbles stuck to it: from each of a surface's bigger bubbles,
  // by its setting's chance (foam<K>Out), one half to all as big, touching the surface there, its body out in the room (on
  // the floor sitting on it, on a wall standing out from it, under the ceiling hanging from it), drawn whole and round as a
  // ball is, shaded as a bubble seen whole, in the room's perspective; now and then a smaller one stuck on it in turn. Sorted
  // among the room's things by the floor under them, in bands (an object each), so the friend passes in front of and behind
  // them; bobbing a little, a band at a time, when the foam moves.
  if (layered) s += SEP + "</g>";   // (ticket 383: the backdrop's end)
  const balls = [];
  const fz = (X, z) => at(X, floor, z)[1];
  for (const [K, si] of [["Back", 1], ["Left", 2], ["Right", 3], ["Floor", 4], ["Ceil", 5]]) {
    const chance = (B["foam" + K + "Out"] ?? 0) / 100;
    if (!(B["foam" + K] ?? true) || !(chance > 0)) continue;
    const share = (B["foam" + K + "H"] ?? (K === "Floor" ? 100 : K === "Ceil" ? 60 : K === "Back" ? 70 : 55)) / 100, R2 = seeded(seed * 13 + si);
    const list = K === "Back" ? foamPack(seed * 7 + 1, 2 * bw, wh * share, rMax, rMin) : K === "Left" || K === "Right" ? foamPack(seed * 7 + si, dSide, wh * share, rMax, rMin)
               : foamPack(seed * 7 + si, 2 * bw, (K === "Floor" ? dFloor : dCeil) * share, rMax, rMin);
    list.forEach((b, i) => {
      if (b.r < 8 * k || R2(i * 4) >= chance) return;
      const rho = b.r * (0.5 + 0.5 * R2(i * 4 + 1)), sd = K === "Left" ? -1 : 1;
      // (its middle in the room: X across, Y up and down on the back wall's plane, d toward us in the room's units)
      const C = K === "Back" ? [-bw + b.x, floor - b.y, rho] : K === "Left" || K === "Right" ? [sd * (bw - rho), floor - b.y, b.x]
              : K === "Floor" ? [-bw + b.x, floor - rho, b.y] : [-bw + b.x, top + rho, b.y];
      // (the way out of its surface into the room, for one stuck on it)
      const N = K === "Back" ? [0, 0, 1] : K === "Left" || K === "Right" ? [-sd, 0, 0] : K === "Floor" ? [0, -1, 0] : [0, 1, 0];
      // (dz: how far the ball reaches toward us and away, as the floor shows depth, for whether the friend is in it)
      const add = (c, r) => { const z = -c[2] / 800, [x, y] = at(c[0], c[1], z), f = 1 / (1 + z);
        if (f > 0) balls.push({ x, y, r: r * f, j: b.j, d: c[2], fz: K === "Back" ? -Infinity : fz(c[0], z), fzHit: fz(c[0], z), dz: (floor - vy) * f * f * r / 800, K }); };
      add(C, rho);
      if (R2(i * 4 + 2) < 0.3) { const r2 = rho * (0.4 + 0.3 * R2(i * 4 + 3)), side = R2(i * 4 + 3) < 0.5 ? -1 : 1, t2 = rho + r2;
        // (out from the first, slant: away from the surface and to one side)
        const dir = N[0] || N[2] ? [N[0] * 0.7, -0.5 * side, N[2] * 0.7 + (N[0] ? 0.5 * side : 0)] : [0.6 * side, N[1] * 0.8, 0];
        add([C[0] + dir[0] * t2, C[1] + dir[1] * t2, C[2] + dir[2] * t2], r2); }
    });
  }
  if (balls.length) {
    s += SEP + part("bubbleroom-balldefs", [tints], () => `<defs>` + tints.map((c, j) => `<radialGradient id="bg-ball-${j}" cx="0.36" cy="0.32" r="0.72">` +
      `<stop offset="0" stop-color="#ffffff" stop-opacity="0.85"/><stop offset="0.25" stop-color="#ffffff" stop-opacity="0.18"/><stop offset="0.7" stop-color="${c}" stop-opacity="0.16"/>` +
      `<stop offset="0.93" stop-color="${c}" stop-opacity="0.55"/><stop offset="1" stop-color="${mix(c, "#5a7fb0", 0.3)}" stop-opacity="0.85"/></radialGradient>`).join("") + `</defs>`);
    // (each about its own middle, so some of the big ones, when the foam moves, can swell and shrink a little, each its own way
    // in: forty at most)
    let breathing = 0;
    const ballLife = (b, i) => move && !layered && b.r >= 12 * k && breathing < 40 && seeded(seed * 17 + 3)(i) < 0.4 * move && ++breathing ? lfoTimed(o, "slow", seeded(seed * 17 + 4)(i),
        `<animateTransform attributeName="transform" type="scale" values="1;1.07;0.95;1" dur="${(3.5 + 2 * seeded(seed * 17 + 4)(i)).toFixed(1)}s" repeatCount="indefinite"/>`) : "";
    const ball = (b, life) => {
      return `<g transform="translate(${f1(b.x)} ${f1(b.y)})"><g>${life}<circle r="${f1(b.r)}" fill="url(#bg-ball-${b.j})" stroke="${tints[b.j]}" stroke-width="${f1(Math.max(0.7, b.r * 0.05))}"/>` +
        (b.r > 8 * k ? `<ellipse cx="${f1(b.r * 0.38)}" cy="${f1(b.r * 0.42)}" rx="${f1(b.r * 0.2)}" ry="${f1(b.r * 0.09)}" fill="#ffffff" opacity="0.55" transform="rotate(-35 ${f1(b.r * 0.38)} ${f1(b.r * 0.42)})"/>` : "") + `</g></g>`; };
    const shadow = b => b.K === "Floor" ? `<ellipse cx="${f1(b.x)}" cy="${f1(b.y + b.r * 0.98)}" rx="${f1(b.r * 0.8)}" ry="${f1(b.r * 0.18)}" fill="#3a5a8a" opacity="0.14"/>` : "";
    const bob = (inner, n) => !move || layered ? inner : `<g>${sceneLfo(o, "slow", (n * 0.17) % 1, "transform", (v, amp) => `0 ${f1(-6 * k * move * v * (0.5 + amp))}`,
      `<animateTransform attributeName="transform" type="translate" values="0 0;0 ${f1(-6 * k * move)};0 0" dur="${(3.5 + (n % 4) * 0.6).toFixed(1)}s" repeatCount="indefinite"/>`, "translate")}${inner}</g>`;
    const ballKey = [c.F, ...FOAM_SURFACES.flatMap(([K]) => [B["foam" + K], B["foam" + K + "H"], B["foam" + K + "Out"]]), base, B.foamRainbow ?? true, move, move && !layered ? sceneLfoKey(o, "slow") : "", seed, layered];
    // (the back wall's against it, behind everything; the rest in bands by the floor under them, farther ones drawn first)
    const back = balls.filter(b => b.K === "Back").sort((a, b) => a.d - b.d), rest = balls.filter(b => b.K !== "Back").sort((a, b) => a.fz - b.fz || a.d - b.d);
    // (each lot: the still ones one picture, over the room's own extent of them, their gradients in it; the moving ones live)
    const ballDefs = `<defs>` + tints.map((cc, j) => `<radialGradient id="bg-ball-${j}" cx="0.36" cy="0.32" r="0.72"><stop offset="0" stop-color="#ffffff" stop-opacity="0.85"/><stop offset="0.25" stop-color="#ffffff" stop-opacity="0.18"/><stop offset="0.7" stop-color="${cc}" stop-opacity="0.16"/><stop offset="0.93" stop-color="${cc}" stop-opacity="0.55"/><stop offset="1" stop-color="${mix(cc, "#5a7fb0", 0.3)}" stop-opacity="0.85"/></radialGradient>`).join("") + `</defs>`;
    // (the back wall's against it, behind everything; the rest in bands by the floor under them; ids, and which of them
    // move, fixed here in one order for every drawing, so a lot drawn again with one popped picks the same ones)
    const bands = new Map();
    // (six bands down the floor seen, from the back wall's foot to the picture's; those under the floor seen, near us, in the last)
    const bandH = Math.max(20 * k, (y1 - floor) / 6);
    for (const b of rest) { const n = Math.max(0, Math.min(5, Math.floor((b.fz - floor) / bandH))); if (!bands.has(n)) bands.set(n, []); bands.get(n).push(b); }
    const lots = [];
    if (back.length) lots.push(["bubbles stuck out, back wall", back, "back"]);
    [...bands.entries()].forEach(([n, bs], i) => lots.push([`bubbles stuck out ${i + 1}`, bs, n]));
    lots.forEach(([name, bs, n], li) => bs.forEach((b, j) => { b.id = li + "." + j; b.obj = name; b.life = ballLife(b, 1000 * li + j); }));
    // (a lot's still ones one picture, over the extent of all of them (popped or not, so it keeps its place), its gradients in
    // it; the moving ones live, each marked to be hidden when popped; ticket 290, ballTick)
    const lotBox = bs => { let x0b = Infinity, y0b = Infinity, x1b = -Infinity, y1b = -Infinity;
      for (const b of bs) if (!b.life) { x0b = Math.min(x0b, b.x - b.r - 2); y0b = Math.min(y0b, b.y - b.r - 2); x1b = Math.max(x1b, b.x + b.r + 2); y1b = Math.max(y1b, b.y + b.r * 1.2 + 2); }
      return x0b < Infinity ? [x0b, y0b, x1b - x0b, y1b - y0b] : null; };
    const lotStill = bs => bs.filter(b => !b.life && !ballPops.has(b.id)).map(b => shadow(b) + ball(b, "")).join("") || " ";
    const lot = bs => { const box = lotBox(bs), popped = bs.filter(b => ballPops.has(b.id)).map(b => b.id).join(",");
      return (box ? asImage(lotStill(bs), box, ballDefs).replace("<image ", `<image data-pops="${popped}" `) : "") + bs.filter(b => b.life).map(b => `<g data-ball="${b.id}"${ballPops.has(b.id) ? ' style="display:none"' : ""}>${shadow(b) + ball(b, b.life)}</g>`).join(""); };
    lots.forEach(([name, bs, n], li) => {
      s += SEP + part("bubbleroom-balls-" + n, [...ballKey, n], () => n === "back" ? wallObj(name, bob(lot(bs), 0)) : obj(name, Math.max(...bs.map(b => b.fz)), bob(lot(bs), li)));
    });
    // (the room as drawn, for popping its bubbles: not a picture of the room, which has a depth of its own)
    if (o.depth === undefined) roomBalls = { list: lots.flatMap(([, bs]) => bs), tints, k, lots: new Map(lots.map(([name, bs]) => [name, bs])),
      href: name => { const bs = roomBalls.lots.get(name), box = bs && lotBox(bs), m = box && imageOf(lotStill(bs), box, ballDefs); return m ? m.href : null; } };
  } else if (o.depth === undefined) roomBalls = null;
  return s;
}
// The bubbles, as they move (in the picture's units): each drifts in the
// room's wind, a breeze slowly turning about, its strength the 🌬️ wind
// setting, with a little jostle of its own; bounces off the picture's edges;
// sticks to any it touches (a bond kept by a gentle spring, so stuck ones
// drift on as a clump); and pops when it touches the friend (a quick swell
// and fade), another drifting in from an edge a moment later, so the room
// keeps its number. The themes take turns: A always, B and C when switched on.
// (ticket 274) Where each bubble first appears, and how it first drifts, comes from the place's bubbleSeed (🎲 other
// places for the bubbles) and its number alone, so the settings decide the picture; a popped one comes back in
// turn, from the seed too. Held still (holdStill), they are not moved at all: touching ones are not even pushed apart.
const bubbleSim = { list: [], bonds: [], layer: null, key: "", last: 0, raf: 0, seed: null, backs: 0, kids: [], kidLayer: null };   // (kids, kidLayer: ticket 329)
const bubbleThemes = B => ["A", "B", "C"].filter(L => L === "A" || B["bubble" + L + "On"]).map(L => ({
  color: B["bubble" + L + "Color"], size: B["bubble" + L + "Size"] ?? 18, shine: (B["bubble" + L + "Shine"] ?? 70) / 100,
  sparkle: B["bubble" + L + "Sparkle"] || "none", sparkleColor: B["bubble" + L + "SparkleColor"] || "#ffffff" }));
// (ticket 290, for N: "the bubble room's bubbles stream out of bubble makers"; "each with controls over how it makes bubbles
// (the rate, the kind of bubble)") The bubbles from the bubble makers placed here, wherever here is: each maker blows its
// rate of them a second out of its wheel, of its theme (the friend's bubble themes, A, B or C, as bubble space's), sized by
// the maker; out its way (its right, or mirrored its left) and up, slowing, jostling a little, rising, then fading after a
// few seconds; a bubble touching the friend pops. Seventy at most, the oldest going first. Drawn in a layer of their own over
// the scene (#maker-bubbles; since ticket 340 in a picture of its own over it, #pic-over); still while the picture is held still.
const makerSim = { list: [], acc: {}, raf: 0, last: 0, n: 0, layer: null };
// (ticket 340, Matthew: "bubble room with one bubble machine is stressing my modern laptop"; a kid "pressing +bubble machine
// until it breaks") Two ways they scale. budget: all the makers here blow MAKER_BUDGET bubbles a second between them, each
// its share of its own rate, so seventy in the air are seventy, not a stream made and thrown away at the cap (256 makers
// blew 1,024 a second, each built, put in the picture and taken out again). overlay: their layer is in a picture of its own
// over the scene's (#pic-over, its viewBox the scene's), so a bubble moving draws only that picture again, not the
// thousands of shapes under it.
const MAKER_LEVERS = { budget: true, overlay: true }, MAKER_BUDGET = 12;
// (the picture over the scene's: inside its border, placed by style.css, its viewBox the scene's; no reads in a frame)
function makerOver() {
  let O = document.getElementById("pic-over");
  if (!O) {
    O = document.createElementNS("http://www.w3.org/2000/svg", "svg"); O.id = "pic-over"; O.setAttribute("aria-hidden", "true"); O.setAttribute("pointer-events", "none");
    pic.after(O);
  }
  const vb = pic.getAttribute("viewBox"); if (vb && O.getAttribute("viewBox") !== vb) O.setAttribute("viewBox", vb);
  return O;
}
// (ticket 290, Matthew: "the ones that extend into the room don't pop when we walk through them.. That's the critical
// path"; "We could have a new sub-cat for bubble collisions -- they are all circles right!") The bubble room's stuck-out
// bubbles as colliders of their own, circles: roomBalls, each one's middle and size on the screen, how far it reaches in
// depth (dz) and the floor under it (fzHit), its lot and id, as the room was last drawn (bubbleroomScene). One the friend's
// body is in, at their depth, pops (a swell and gone, as bubble space's), and grows back nine to fifteen seconds later;
// popped, its lot's still picture is drawn again without it (and a moving one hidden), on the page as it is, not in the
// scene's text, so nothing else is built again. 🔬 Inspection's "🫧 bubbles that pop" shows the circles.
let roomBalls = null;
const ballPops = new Map(), ballSim = { raf: 0, layer: null, touched: new Set(), n: 0 };
function ballRun() {
  const on = state.background.kind === "bubbleroom" && roomBalls && roomBalls.list.length;
  if (!on) { if (ballSim.raf) cancelAnimationFrame(ballSim.raf); ballSim.raf = 0; if (ballSim.layer) { ballSim.layer.remove(); ballSim.layer = null; } return; }
  if (!ballSim.layer) { const L = ballSim.layer = document.createElementNS("http://www.w3.org/2000/svg", "g"); L.id = "ball-pops"; L.setAttribute("pointer-events", "none"); }
  if (ballSim.layer.parentNode !== pic) pic.append(ballSim.layer);
  for (const name of ballSim.touched) ballApply(name, false);   // (a lot built again since: as it is now)
  if (!ballSim.raf) ballSim.raf = requestAnimationFrame(ballTick);
}
// a lot's bubbles shown as they are now: its picture without the popped ones, its moving ones hidden when popped
function ballApply(name, force = true) {
  const R = roomBalls, bs = R && R.lots.get(name), g = bs && bgShown.node && bgShown.node.querySelector(`[data-obj="${name}"]`);
  if (!g) return;
  const key = bs.filter(b => ballPops.has(b.id)).map(b => b.id).join(",");
  const img = g.querySelector("image");
  if (img && (force || img.dataset.pops !== key) && (img.dataset.pops ?? "") !== key) { const h = R.href(name); if (h) img.setAttribute("href", h); img.dataset.pops = key; }
  for (const b of bs) if (b.life) { const el = g.querySelector(`[data-ball="${b.id}"]`), d = ballPops.has(b.id) ? "none" : ""; if (el && el.style.display !== d) el.style.display = d; }
}
function ballTick(t) {
  ballSim.raf = 0;
  const R = roomBalls;
  if (state.background.kind !== "bubbleroom" || !R) return;
  const changed = new Set();
  for (const [id, p] of ballPops) if (t >= p.back) { ballPops.delete(id); changed.add(p.obj); }
  // (the friend's body, a little narrower than their reach, from where they are drawn; not while the picture is held still)
  const fr = stillAt === null ? friendBox() : null;
  if (fr) {
    const feet = fr[3], cx = (fr[0] + fr[2]) / 2, half = (fr[2] - fr[0]) * 0.36;
    for (const b of R.list) {
      if (ballPops.has(b.id) || b.x + b.r < cx - half || b.x - b.r > cx + half || b.y + b.r < fr[1] || b.y - b.r > feet) continue;
      if (Math.abs(b.fzHit - feet) > b.dz + 4 * R.k) continue;
      ballPop(b, t, changed);
    }
  }
  for (const name of changed) ballApply(name);
  ballSim.raf = requestAnimationFrame(ballTick);
}
// (ticket 499) one of the room's bubbles popped, by the friend's touch or by a fire's heat (firePops) alike: gone till 9 to 15
// seconds on, its lot (changed) to be shown again without it (ballApply)
function ballPop(b, t, changed) {
  const R = roomBalls, v = Math.sin(++ballSim.n * 12.9898) * 43758.5453;
  ballPops.set(b.id, { at: t, back: t + 9000 + 6000 * (v - Math.floor(v)), obj: b.obj }); changed.add(b.obj); ballSim.touched.add(b.obj);
  // (the pop: a copy of it swelling and gone, over everything, a quarter second)
  if (ballSim.layer) { const el = document.createElementNS("http://www.w3.org/2000/svg", "g"); el.setAttribute("transform", `translate(${f1(b.x)} ${f1(b.y)})`);
    el.innerHTML = `<g class="bubin pop">${bubbleSvg(0, 0, b.r, { color: R.tints[b.j], shine: 0.7 })}</g>`; ballSim.layer.append(el); setTimeout(() => el.remove(), 320); }
}
// (ticket 383) The layered foam, after each drawing: a scene mounted with a backdrop to put under the picture (data-under, drawn by
// bubbleroomScene when the foam moves) has it moved into #pic-under, a picture of its own under #pic (placed as #pic-over is, inside
// the border; #pic see-through, body.pic-see-through), and each of its moving bubbles (data-foam: "b amount seconds start" breathing,
// "p 0 seconds start" popping) made a page element in #foam-layers, between the two: a little picture of that bubble alone, placed in
// shares of the picture (so it follows any resize), scaled about its middle by a CSS animation, which the browser does without
// painting either picture again; the bubble in the backdrop hidden. Not while the picture is held still (holdStill: the archive) or
// the scenery is (a slow computer): then the backdrop stays in the picture, its bubbles still. Cheap when nothing is new.
let foamLayers = { group: null, on: false };
function foamLayersRun() {
  const B = state.background, want = B.kind === "bubbleroom" && stillAt === null && !sceneryStill();
  const fresh = bgShown.node ? bgShown.node.querySelector("[data-under]") : null;
  // (taken down: no longer wanted (another place, the picture held still, the scenery still on a slow computer), or the scene mounted
  // again with no backdrop to put under (the foam made still); the backdrop put back in the picture where it was, at once, its
  // moving bubbles shown in it, still)
  if (foamLayers.on && (!want || (!fresh && bgShown.node !== foamLayers.node))) {
    const { group, mark } = foamLayers;
    if (group && mark && mark.isConnected) { for (const el of group.querySelectorAll("[data-foam]")) el.style.visibility = ""; mark.replaceWith(group); }
    document.getElementById("pic-under")?.remove(); document.getElementById("foam-layers")?.remove(); document.body.classList.remove("pic-see-through");
    foamLayers = { group: null, on: false, watch: foamLayers.watch };
  }
  if (!want || !fresh) return;
  let U = document.getElementById("pic-under"), L = document.getElementById("foam-layers");
  if (!U) { U = document.createElementNS("http://www.w3.org/2000/svg", "svg"); U.id = "pic-under"; U.setAttribute("aria-hidden", "true"); U.setAttribute("pointer-events", "none"); }
  if (!L) { L = document.createElement("div"); L.id = "foam-layers"; L.setAttribute("aria-hidden", "true"); }
  if (U.nextSibling !== L || L.nextSibling !== pic) { pic.before(U); pic.before(L); }
  const vb = pic.getAttribute("viewBox"); if (vb && U.getAttribute("viewBox") !== vb) U.setAttribute("viewBox", vb);
  const mark = document.createComment("under"); fresh.before(mark);   // (where it goes back, taken down)
  U.replaceChildren(fresh);
  document.body.classList.add("pic-see-through");
  const V = pic.viewBox.baseVal, Ui = U.getScreenCTM(), made = [];
  if (Ui) { const Uinv = Ui.inverse();
    for (const el of fresh.querySelectorAll("[data-foam]")) {
      let m, q; try { m = Uinv.multiply(el.getScreenCTM()); q = boxThrough(el.getBBox(), m); } catch (e) { continue; }
      if (!q || !(q.width > 0)) continue;
      const pad = 0.15 * Math.max(q.width, q.height), x = q.x - pad, y = q.y - pad, w = q.width + 2 * pad, h = q.height + 2 * pad;
      const [kind, amt, dur, start] = el.dataset.foam.split(" "), a = +amt, name = (el.closest("[data-obj]") || { dataset: {} }).dataset.obj || "";
      const clip = (el.closest("[clip-path]") || { getAttribute: () => "" }).getAttribute("clip-path") || "";
      const d = document.createElement("div"); d.className = "foam-b"; d.dataset.obj = name;
      const pc = (v, of) => (100 * v / of).toFixed(3) + "%";
      d.style.left = pc(x - V.x, V.width); d.style.top = pc(y - V.y, V.height); d.style.width = pc(w, V.width); d.style.height = pc(h, V.height);
      d.style.animationName = kind === "p" ? "foam-pop" : "foam-breathe-" + (a < 0.07 ? 1 : a < 0.1 ? 2 : 3);
      d.style.animationDuration = dur + "s"; d.style.animationDelay = start + "s";
      d.innerHTML = `<svg viewBox="${[x, y, w, h].map(v => +v.toFixed(3)).join(" ")}" preserveAspectRatio="none"><g${clip ? ` clip-path="${clip}"` : ""}><g transform="matrix(${[m.a, m.b, m.c, m.d, m.e, m.f].map(v => +v.toFixed(6)).join(" ")})">${el.innerHTML}</g></g></svg>`;
      el.style.visibility = "hidden";
      made.push(d);
    } }
  // (in a box of their own where the scene's picture is drawn inside its frame: the picture fits the scene to its box, a band above
  // and below where the shapes differ, so shares of the frame itself put them a few pixels off; placed again when it is resized)
  const inner = document.createElement("div"); inner.className = "foam-in"; inner.append(...made);
  L.replaceChildren(inner);
  foamLayers = { group: fresh, mark, node: bgShown.node, on: true, inner, watch: foamLayers.watch };
  foamLayersPlace();
  if (!foamLayers.watch && typeof ResizeObserver === "function") { foamLayers.watch = new ResizeObserver(() => foamLayersPlace()); }
  if (foamLayers.watch) { foamLayers.watch.disconnect(); foamLayers.watch.observe(U); }
  foamLayersShow();
}
function foamLayersPlace() {
  const U = document.getElementById("pic-under"), L = document.getElementById("foam-layers"), inner = foamLayers.inner;
  if (!U || !L || !inner || !foamLayers.on) return;
  const M = U.getScreenCTM(), V = U.viewBox.baseVal, r = L.getBoundingClientRect();
  if (!M || !V || !V.width) return;
  const px = v => v.toFixed(2) + "px";
  inner.style.left = px(M.e + V.x * M.a - r.left); inner.style.top = px(M.f + V.y * M.d - r.top); inner.style.width = px(V.width * M.a); inner.style.height = px(V.height * M.d);
}
// (a foam surface hidden with 👁️ hides its moving bubbles too)
function foamLayersShow() {
  const L = foamLayers.on && foamLayers.inner; if (!L) return;
  const off = objectsOff(state.background);
  for (const d of L.children) { const v = off.has(d.dataset.obj) ? "none" : ""; if (d.style.display !== v) d.style.display = v; }
}
// (the friend's box, standing, in the picture's units: their feet's middle, as tall as they stand, a little over a third of
// that across, from figureT; ticket 290)
const friendBox = () => { const T = figureT; if (!(T && T.k > 0)) return null; const h = (T.base || 400) * T.k, x = T.dx, y = T.sg + T.dy;
  return [x - 0.28 * h, y - h, x + 0.28 * h, y]; };
const makerTheme = (B, L) => ({ color: B["bubble" + L + "Color"], size: B["bubble" + L + "Size"] ?? 18, shine: (B["bubble" + L + "Shine"] ?? 70) / 100,
  sparkle: B["bubble" + L + "Sparkle"] || "none", sparkleColor: B["bubble" + L + "SparkleColor"] || "#ffffff" });
function makerRun() {
  const B = state.background, has = B.kind !== "none" && (B.things || []).some(t => t.kind === "bubblemaker");
  if (!has) { if (makerSim.raf) cancelAnimationFrame(makerSim.raf); makerSim.raf = 0; makerSim.list = []; if (makerSim.layer) { makerSim.layer.remove(); makerSim.layer = null; } const O = document.getElementById("pic-over"); if (O) O.style.display = "none"; return; }
  // (the same layer every drawing, its bubbles in it: a drawing takes everything but the scene out of the picture, and this
  // puts it back, on top)
  let L = makerSim.layer;
  if (!L) { L = makerSim.layer = document.createElementNS("http://www.w3.org/2000/svg", "g"); L.id = "maker-bubbles"; L.setAttribute("pointer-events", "none"); makerSim.list = []; }
  if (MAKER_LEVERS.overlay) { const O = makerOver(); if (O.style.display) O.style.display = ""; if (L.parentNode !== O) O.append(L); }
  else { const O = document.getElementById("pic-over"); if (O) O.style.display = "none"; if (L.parentNode !== pic || pic.lastChild !== L) pic.append(L); }
  if (!makerSim.raf) { makerSim.last = 0; makerSim.raf = requestAnimationFrame(makerTick); }
}
function makerTick(t) {
  makerSim.raf = 0;
  const B = state.background, layer = makerSim.layer, F = sceneFrame, k = (F.bottom - F.top) / 700;
  const makers = (B.things || []).map((d, i) => [d, i]).filter(([d]) => d.kind === "bubblemaker");
  if (!makers.length || !layer || B.kind === "none") return;
  const dt = makerSim.last ? Math.min(0.05, Math.max(0, (t - makerSim.last) / 1000)) : 0; makerSim.last = t;
  if (stillAt !== null) { makerSim.raf = requestAnimationFrame(makerTick); return; }
  const L = makerSim.list, rnd = () => { const v = Math.sin(++makerSim.n * 12.9898) * 43758.5453; return v - Math.floor(v); };
  if (MAKER_LEVERS.overlay) makerOver();   // (the scene's viewBox, if it moved)
  // each maker blows its share (ticket 340: of MAKER_BUDGET a second between them all)
  const rateOf = d => Math.max(1, Math.min(12, d.rate ?? 4)), want = makers.reduce((a, [d]) => a + rateOf(d), 0), share = MAKER_LEVERS.budget ? Math.min(1, MAKER_BUDGET / want) : 1;
  for (const [d, i] of makers) {
    const s = depthSize(B, d.z - SCENE.ground) * (d.size ?? 100) / 100, sd = d.mirror ? -1 : 1, rate = rateOf(d) * share;
    makerSim.acc[i] = (makerSim.acc[i] || rnd()) + rate * dt;
    while (makerSim.acc[i] >= 1) {
      makerSim.acc[i] -= 1;
      const th = makerTheme(B, d.theme || "A"), r = Math.max(2, (th.size / 18) * 11 * s * (0.75 + 0.5 * rnd()));
      const el = document.createElementNS("http://www.w3.org/2000/svg", "g");
      el.innerHTML = `<g class="bubin">${bubbleSvg(0, 0, r, th)}</g>`; layer.append(el);
      const Wh = makerShape(d).wheels, wo = Wh[(makerSim.n >> 1) % Wh.length].out;   // (from each wheel by turns)
      L.push({ x: d.x + sd * wo[0] * s, y: d.z + wo[1] * s, vx: sd * (70 + 70 * rnd()) * s, vy: -(25 + 50 * rnd()) * s, r, el, born: t, life: 4500 + 3500 * rnd(), ph: rnd() * 6.28, s, roll: rnd() });
      if (L.length > 70) { const o = L.shift(); o.el.remove(); }
    }
  }
  // the friend: their box in the picture's units, from where they are drawn (figureT), not read back from the screen: a read
  // each frame made the browser lay the whole picture out again, thousands of shapes in the bubble room
  const fr = friendBox();
  // (ticket 341) what a moving bubble can stick to here, and how likely it is to: the bubble room's bubbles standing out of its
  // foam (roomBalls, where they are drawn, popped ones left out), or bubble space's own; each bubble rolls once, at its birth
  const targets = makerStickTargets(B), stick = (B.kind === "bubbleroom" ? B.foamStick ?? 30 : B.bubbleStick ?? 30) / 100;
  let stuckN = L.filter(b => b.stuck).length;
  for (let j = L.length - 1; j >= 0; j--) {
    const b = L[j], age = t - b.born;
    if (b.pop) { if (t - b.pop > 260) { b.el.remove(); L.splice(j, 1); } continue; }
    if (age > b.life || b.y < F.top - 40 * k || b.x < F.left - 60 * k || b.x > F.right + 60 * k) { b.el.remove(); L.splice(j, 1); continue; }
    if (b.stuck) {   // (stuck: where it touched, following what it stuck to; swelling a little till it fades; a popped hold lets it go)
      const T = b.stuck.to;
      if (T.gone && T.gone()) { b.stuck = null; b.vx = 0; b.vy = 20 * b.s; stuckN--; }
      else { b.x = T.x() + b.stuck.dx; b.y = T.y() + b.stuck.dy;
        if (fr && b.x + b.r > fr[0] && b.x - b.r < fr[2] && b.y + b.r > fr[1] && b.y - b.r < fr[3]) { b.pop = t; if (b.el.firstChild) b.el.firstChild.classList.add("pop"); continue; }
        const u = Math.min(1, (age - b.stuck.at) / (b.life - b.stuck.at)), fade = Math.min(1, (b.life - age) / 900);
        b.el.setAttribute("transform", `translate(${f1(b.x)} ${f1(b.y)}) scale(${(1 + 0.15 * u).toFixed(3)})`); b.el.setAttribute("opacity", fade.toFixed(2));
        continue; }
    }
    if (!b.stuck && age > 500 && b.roll < stick && stuckN < STICK_MAX && targets.length) {
      const T = targets.find(q => { const dx = b.x - q.x(), dy = b.y - q.y(), rr = q.r + b.r; return Math.abs(dx) < rr && Math.abs(dy) < rr && dx * dx + dy * dy < rr * rr && !q.gone(); });
      if (T) { const dx = b.x - T.x(), dy = b.y - T.y(), d = Math.hypot(dx, dy) || 1, rr = (T.r + b.r) * 0.94;
        b.stuck = { to: T, dx: dx / d * rr, dy: dy / d * rr, at: age }; b.life = age + 6000 + 6000 * rnd(); b.vx = b.vy = 0; stuckN++; continue; }
    }
    const damp = Math.pow(0.5, dt), w = t / 1000 * 1.3 + b.ph;
    b.vx = b.vx * damp + Math.cos(w) * 22 * b.s * dt; b.vy = b.vy * Math.pow(0.7, dt) - 14 * b.s * dt + Math.sin(w * 1.4) * 18 * b.s * dt;
    b.x += b.vx * dt; b.y += b.vy * dt;
    if (fr && age > 250 && b.x + b.r > fr[0] && b.x - b.r < fr[2] && b.y + b.r > fr[1] && b.y - b.r < fr[3]) { b.pop = t; if (b.el.firstChild) b.el.firstChild.classList.add("pop"); continue; }
    const fade = Math.min(1, (b.life - age) / 600, age / 150);
    b.el.setAttribute("transform", `translate(${f1(b.x)} ${f1(b.y)})`); b.el.setAttribute("opacity", fade.toFixed(2));
  }
  makerSim.raf = requestAnimationFrame(makerTick);
}
// (ticket 341) the bubbles a moving bubble can stick to here: each its middle (as functions, as bubble space's move) and size,
// and whether it has gone (popped); at most STICK_MAX stuck at once
const STICK_MAX = 30;
// (made once for the room as drawn, or for bubble space's list, not every frame: the bubble room has hundreds)
function makerStickTargets(B) {
  const src = B.kind === "bubbleroom" ? roomBalls : B.kind === "bubblespace" ? bubbleSim.list : null;
  if (makerSim.tsrc === src && makerSim.tkind === B.kind) return makerSim.targets;
  makerSim.tsrc = src; makerSim.tkind = B.kind;
  return makerSim.targets = !src ? [] : B.kind === "bubbleroom" ? src.list.map(q => ({ x: () => q.x, y: () => q.y, r: q.r, gone: () => ballPops.has(q.id) }))
    : src.map(q => ({ x: () => q.x, y: () => q.y, r: q.r, gone: () => !!(q.pop || q.back) }));
}
function bubbleRun() {
  if (state.background.kind !== "bubblespace") { if (bubbleSim.raf) cancelAnimationFrame(bubbleSim.raf); bubbleSim.raf = 0; bubbleSim.list = []; bubbleSim.bonds = []; bubbleSim.layer = null; bubbleSim.key = ""; bubbleKidsClear(); return; }
  if (!bubbleSim.raf) bubbleSim.raf = requestAnimationFrame(bubbleTick);
}
function bubbleTick(t) {
  bubbleSim.raf = 0;
  const B = state.background;
  if (B.kind !== "bubblespace") return;
  const layer = document.getElementById("bubble-layer"), F = sceneFrame, k = (F.bottom - F.top) / 700;
  if (!layer || !(F.right > F.left)) { bubbleSim.raf = requestAnimationFrame(bubbleTick); return; }
  const themes = bubbleThemes(B), n = Math.max(1, Math.min(80, Math.round(B.bubbleCount ?? 30))), depthAmt = Math.max(0, Math.min(100, B.bubbleDepth ?? 70)) / 100;
  const key = JSON.stringify([n, themes, depthAmt, F.left, F.top, F.right, F.bottom]);
  const seed = B.bubbleSeed ?? 1, first = seeded(seed), again = seeded(seed + 0.5), deep = seeded(seed + 0.25);
  if (bubbleSim.seed !== seed) { bubbleSim.seed = seed; bubbleSim.list = []; bubbleSim.bonds = []; bubbleSim.backs = 0; bubbleKidsClear(); }
  // (ticket 329) how far back each is (0 near, 1 far), the farthest first, so drawn behind; a far one smaller and slower
  const depths = Array.from({ length: n }, (_, i) => deep(i)).sort((a, b) => b - a), near = d => 1 - 0.6 * depthAmt * d;
  const at = (i, j, a, b) => a + (b - a) * first(i * 4 + j), rand = (a, b) => a + (b - a) * again(bubbleSim.backs++);
  const spot = (b, i) => { b.x = at(i, 0, F.left + b.r, F.right - b.r); b.y = at(i, 1, F.top + b.r, F.bottom - b.r); b.vx = at(i, 2, -20, 20) * k; b.vy = at(i, 3, -20, 20) * k; };
  if (key !== bubbleSim.key || layer !== bubbleSim.layer || layer.childNodes.length !== bubbleSim.list.length) {
    // (made again: the number, a theme, or the picture's size changed; those already there keep their places)
    const old = bubbleSim.list;
    bubbleSim.list = Array.from({ length: n }, (_, i) => { const th = themes[i % themes.length], b = old[i] ? { ...old[i] } : {}; b.th = th; b.d = depths[i]; b.sc = near(b.d); b.r = th.size * k * b.sc; b.pop = 0; b.back = 0; if (!old[i]) spot(b, i); return b; });
    bubbleSim.bonds = bubbleSim.bonds.filter(([i, j]) => i < n && j < n);
    layer.innerHTML = bubbleSim.list.map(b => `<g transform="translate(${f1(b.x)} ${f1(b.y)})"><g class="bubin">${bubbleSvg(0, 0, b.r, b.th)}</g></g>`).join("");
    bubbleSim.layer = layer; bubbleSim.key = key; bubbleSim.last = t;
  }
  if (stillAt !== null) { bubbleSim.last = t; bubbleSim.raf = requestAnimationFrame(bubbleTick); return; }   // (held still: drawn where they are)
  const dt = Math.min(0.05, Math.max(0, (t - bubbleSim.last) / 1000)); bubbleSim.last = t;
  const L = bubbleSim.list, wind = (B.bubbleWind ?? 30) / 100, ang = t / 1000 * 0.13, wx = Math.cos(ang) * 90 * k * wind, wy = Math.sin(ang * 1.7) * 45 * k * wind;
  // the friend: their box in the picture's units, from the screen
  let fr = null;
  const fig = document.getElementById("figure"), P = pic.getScreenCTM();
  if (fig && P && state.body) { const r = fig.getBoundingClientRect(), inv = P.inverse(), a = new DOMPoint(r.left, r.top).matrixTransform(inv), c = new DOMPoint(r.right, r.bottom).matrixTransform(inv);
    if (r.width > 2) fr = [Math.min(a.x, c.x), Math.min(a.y, c.y), Math.max(a.x, c.x), Math.max(a.y, c.y)]; }
  L.forEach((b, i) => {
    if (b.back) { if (t < b.back) return; b.back = 0; b.pop = 0; const sd = Math.floor(rand(0, 4));   // (in again, from an edge)
      b.x = sd === 0 ? F.left + b.r : sd === 1 ? F.right - b.r : rand(F.left + b.r, F.right - b.r); b.y = sd === 2 ? F.top + b.r : sd === 3 ? F.bottom - b.r : rand(F.top + b.r, F.bottom - b.r);
      b.vx = rand(-15, 15) * k; b.vy = rand(-15, 15) * k; const el = layer.childNodes[i]; if (el) { el.style.display = ""; const inn = el.firstChild; if (inn) inn.classList.remove("pop"); } }
    if (b.pop) return;
    // (the breeze, and a jostle of its own that turns slowly)
    const j = t / 1000 * (0.7 + 0.05 * (i % 7)) + i * 1.3;
    b.vx += (wx + Math.cos(j) * 30 * k * (0.3 + wind)) * b.sc * dt; b.vy += (wy + Math.sin(j * 1.3) * 30 * k * (0.3 + wind)) * b.sc * dt;
    const damp = Math.pow(0.55, dt); b.vx *= damp; b.vy *= damp;
  });
  // stuck together: each bond kept at its length, both moving together
  const bonded = new Set();
  for (const [i, j, rest] of bubbleSim.bonds) {
    const a = L[i], b = L[j]; if (a.pop || b.pop) continue;
    bonded.add(i + "," + j);
    const dx = b.x - a.x, dy = b.y - a.y, d = Math.hypot(dx, dy) || 1, e = (d - rest) / d * 0.5;
    a.x += dx * e; a.y += dy * e; b.x -= dx * e; b.y -= dy * e;
    const mx = (a.vx + b.vx) / 2, my = (a.vy + b.vy) / 2; a.vx = b.vx = mx; a.vy = b.vy = my;
  }
  // touching: they stick (a new bond), at most five to a bubble
  const deg = new Array(L.length).fill(0); for (const [i, j] of bubbleSim.bonds) { deg[i]++; deg[j]++; }
  for (let i = 0; i < L.length; i++) for (let j = i + 1; j < L.length; j++) {
    const a = L[i], b = L[j]; if (a.pop || b.pop || a.back || b.back || Math.abs(a.d - b.d) * depthAmt > BUBBLE_TOUCH_DEPTH * 0.7) continue;   // (ticket 329: one far behind another passes it by; ticket 369: by how far apart they are drawn, the same as before at 🔭 70, the start, and at 0, all in one plane, every touch sticks)
    const dx = b.x - a.x, dy = b.y - a.y, rr = a.r + b.r;
    if (Math.abs(dx) > rr || Math.abs(dy) > rr || dx * dx + dy * dy > rr * rr) continue;
    if (!bonded.has(i + "," + j) && deg[i] < 5 && deg[j] < 5) { bubbleSim.bonds.push([i, j, rr * 0.96]); deg[i]++; deg[j]++; bonded.add(i + "," + j); }
    else { const d = Math.hypot(dx, dy) || 1, push = (rr * 0.96 - d) / d * 0.5; if (push > 0) { a.x -= dx * push; a.y -= dy * push; b.x += dx * push; b.y += dy * push; } }
  }
  // moved, kept in the picture, and popped against the friend
  L.forEach((b, i) => {
    if (b.pop || b.back) return;
    b.x += b.vx * dt; b.y += b.vy * dt;
    if (b.x < F.left + b.r) { b.x = F.left + b.r; b.vx = Math.abs(b.vx) * 0.6; } else if (b.x > F.right - b.r) { b.x = F.right - b.r; b.vx = -Math.abs(b.vx) * 0.6; }
    if (b.y < F.top + b.r) { b.y = F.top + b.r; b.vy = Math.abs(b.vy) * 0.6; } else if (b.y > F.bottom - b.r) { b.y = F.bottom - b.r; b.vy = -Math.abs(b.vy) * 0.6; }
    const el = layer.childNodes[i];
    if (fr && b.x + b.r > fr[0] && b.x - b.r < fr[2] && b.y + b.r > fr[1] && b.y - b.r < fr[3]) { bubbleSpacePop(b, i, t, k, rand); return; }
    if (el) el.setAttribute("transform", `translate(${f1(b.x)} ${f1(b.y)})`);
  });
  bubbleKidsTick(t, dt, layer, F, k, fr, wx, wy, wind, rand);
  bubbleSim.raf = requestAnimationFrame(bubbleTick);
}
// (ticket 499) bubble space's pops, by the friend's touch or by a fire's heat (firePops) alike: bubble i swells and is gone,
// its bonds let go, its lines, its sound and maybe its smaller bubbles (bubblePopped), back in from an edge 1.4 s on; one of
// the smaller ones the same, once more at most (BUBBLE_KID_GENS). bubbleRand: the chance they draw on, from the place's
// seed (🎲), as bubbleTick's own
const bubbleRand = () => { const again = seeded((state.background.bubbleSeed ?? 1) + 0.5); return (a, b) => a + (b - a) * again(bubbleSim.backs++); };
function bubbleSpacePop(b, i, t, k, rand) {
  const el = bubbleSim.layer && bubbleSim.layer.childNodes[i];
  b.pop = t; b.back = t + 1400; bubbleSim.bonds = bubbleSim.bonds.filter(([p, q]) => p !== i && q !== i); bubblePopped(b, 0, k, rand);
  if (el && el.firstChild) { el.firstChild.classList.add("pop"); setTimeout(() => { if (b.pop && el.isConnected) el.style.display = "none"; }, 260); }
}
function bubbleKidPop(c, t, k, rand) { c.pop = t; if (c.el.firstChild) c.el.firstChild.classList.add("pop"); bubblePopped(c, c.gen, k, rand); }
// (ticket 329, Matthew: "when we 'pop' a bubble in bubble space: the likelihood of creating new, smaller bubbles (0 to
// 100); the number of new, smaller bubbles, on average -- same style as the popped bubble, for now", and "some more
// 'popping' animation, like the 'face head lines' that stick out of the face, or the rays that come out of the sun, but
// temporarily, and animated as getting big and disappearing, all around the popped bubble", with an optional sound) A pop:
// its lines (bubblePopRays), its sound (bubblePopSound), and, as likely as 💥 says, about 🫧 smaller bubbles of its own
// style flung out from it at its depth (bubbleSim.kids, in a layer of their own just after the bubbles', so the bubbles'
// own layer keeps one child a bubble). They drift on the same wind, pop on the friend too (and burst once more, no
// further), and fade out after a few seconds.
const BUBBLE_TOUCH_DEPTH = 0.2, BUBBLE_KIDS_MAX = 60, BUBBLE_KID_GENS = 2;
function bubbleKidsClear() { for (const c of bubbleSim.kids || []) c.el.remove(); bubbleSim.kids = []; }
function bubbleKidLayer(layer) {
  let K = bubbleSim.kidLayer;
  if (!K) { K = bubbleSim.kidLayer = document.createElementNS("http://www.w3.org/2000/svg", "g"); K.id = "bubble-kids"; K.setAttribute("pointer-events", "none"); }
  if (K.previousSibling !== layer) layer.after(K);
  return K;
}
function bubblePopped(b, gen, k, rand) {
  const B = state.background, layer = bubbleSim.layer; if (!layer) return;
  const K = bubbleKidLayer(layer);
  if (B.bubbleRays !== false) bubblePopRays(K, b.x, b.y, b.r, b.th);
  bubblePopSound(b.r / k);
  const kids = bubbleSim.kids || (bubbleSim.kids = []);
  if (gen >= BUBBLE_KID_GENS || rand(0, 100) >= (B.bubbleBurst ?? 40)) return;
  const avg = Math.max(1, Math.min(8, B.bubbleBurstCount ?? 3)), many = Math.max(1, Math.min(12, Math.round(avg * rand(0.5, 1.5)))), turn = rand(0, 6.283);
  for (let j = 0; j < many && kids.length < BUBBLE_KIDS_MAX; j++) {
    const r = b.r * rand(0.35, 0.55); if (r < 1.5 * k) break;
    const a = turn + j * 6.283 / many + rand(-0.3, 0.3), v = rand(60, 110) * k * b.sc, el = document.createElementNS("http://www.w3.org/2000/svg", "g");
    el.innerHTML = `<g class="bubin">${bubbleSvg(0, 0, r, b.th)}</g>`; K.append(el);
    kids.push({ x: b.x + Math.cos(a) * b.r * 0.5, y: b.y + Math.sin(a) * b.r * 0.5, vx: Math.cos(a) * v, vy: Math.sin(a) * v, r, th: b.th, d: b.d, sc: b.sc,
                born: bubbleSim.last, life: rand(4000, 7000), gen: gen + 1, el });
  }
}
function bubbleKidsTick(t, dt, layer, F, k, fr, wx, wy, wind, rand) {
  const kids = bubbleSim.kids; if (!kids || !kids.length) return;
  bubbleKidLayer(layer);
  for (let i = kids.length - 1; i >= 0; i--) {
    const c = kids[i], age = t - c.born;
    if (c.pop) { if (t - c.pop > 260) { c.el.remove(); kids.splice(i, 1); } continue; }
    if (age > c.life) { c.el.remove(); kids.splice(i, 1); continue; }
    const j = t / 1000 * 0.9 + i * 1.7;
    c.vx += (wx + Math.cos(j) * 30 * k * (0.3 + wind)) * c.sc * dt; c.vy += (wy + Math.sin(j * 1.3) * 30 * k * (0.3 + wind)) * c.sc * dt;
    const damp = Math.pow(0.4, dt); c.vx *= damp; c.vy *= damp;
    c.x = Math.max(F.left + c.r, Math.min(F.right - c.r, c.x + c.vx * dt)); c.y = Math.max(F.top + c.r, Math.min(F.bottom - c.r, c.y + c.vy * dt));
    // (born on the friend, each pops on them only once it has been clear of them: walk into it again)
    const on = fr && c.x + c.r > fr[0] && c.x - c.r < fr[2] && c.y + c.r > fr[1] && c.y - c.r < fr[3];
    if (fr && !on) c.clear = true;   // (a frame with no friend to measure is not one clear of them)
    else if (c.clear) { bubbleKidPop(c, t, k, rand); continue; }
    c.el.setAttribute("transform", `translate(${f1(c.x)} ${f1(c.y)})`);
    c.el.setAttribute("opacity", Math.min(1, (c.life - age) / 700, age / 120).toFixed(2));
  }
}
// the lines: ten, all round, just out from its rim, growing and gone (CSS: .pop-rays), in its colour
function bubblePopRays(K, x, y, r, th) {
  // (a shade darker than the bubble, so a pale bubble's lines still show on bubble space's pale wall)
  const g = document.createElementNS("http://www.w3.org/2000/svg", "g"), c = (th && th.color) || BUBBLE_DEFAULT.color, w = Math.max(1, r * 0.15);
  let t = "";
  for (let i = 0; i < 10; i++) { const a = i * 0.6283 + 0.3, ca = Math.cos(a), sa = Math.sin(a);
    t += `<line x1="${f1(ca * r * 1.2)}" y1="${f1(sa * r * 1.2)}" x2="${f1(ca * r * 1.9)}" y2="${f1(sa * r * 1.9)}"/>`; }
  g.setAttribute("transform", `translate(${f1(x)} ${f1(y)})`);
  g.innerHTML = `<g class="pop-rays" stroke="${c}" style="stroke: color-mix(in srgb, ${c} 65%, #2f5f86)" stroke-width="${f1(w)}" stroke-linecap="round">${t}</g>`;
  K.append(g); setTimeout(() => g.remove(), 460);
}
// the sound: 🔊 how loud (0, none; and none while the music is muted), on 🎹 an instrument of the music's, at 🎵 a pitch
// (a smaller bubble higher: an octave each time it halves) with ✨ its knob; at most one in 40 ms, so a burst is not a roar
let bubblePopLast = 0;
function bubblePopSound(size) {
  const B = state.background, vol = Math.max(0, Math.min(100, B.bubblePopVol ?? 40)) / 100, now = performance.now();
  if (vol <= 0 || state.music.mute || now - bubblePopLast < 40 || typeof audioReady !== "function" || !audioReady() || !tapeOut) return;
  bubblePopLast = now;
  const inst = INSTRUMENTS[B.bubblePopInst] ? B.bubblePopInst : "bubble", t = audio.currentTime + 0.01;
  const m = Math.max(48, Math.min(108, Math.round(84 + (B.bubblePopPitch ?? 0) + 12 * Math.log2(18 / Math.max(2, size)))));
  const out = gainOf(audio, tapeOut, 0.8 * vol);
  INSTRUMENTS[inst](audio, out, t, m, 0.25, 1, B.bubblePopKnob ?? 50);
  setTimeout(() => out.disconnect(), 4000);
}
function imaginationScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, all, o } = c;
  let s = SEP + part("imagination-room", c.F, () => {
    let t = `<defs><linearGradient id="bg-imag-floor" x1="0" y1="${f1(g - 60 * k)}" x2="0" y2="${f1(y1)}" gradientUnits="userSpaceOnUse">` +
            `<stop offset="0" stop-color="#ffffff"/><stop offset="1" stop-color="#e4e7ee"/></linearGradient>` +
            `<radialGradient id="bg-imag-glow"><stop offset="0" stop-color="#ffffff"/><stop offset="1" stop-color="#f3f5f9"/></radialGradient></defs>` + all("url(#bg-imag-glow)");
    t += `<rect x="${f1(x0 - pad)}" y="${f1(g - 60 * k)}" width="${f1(W + 2 * pad)}" height="${f1(y1 - g + 60 * k + pad)}" fill="url(#bg-imag-floor)"/>` +
         `<ellipse cx="0" cy="${f1(g + 8 * k)}" rx="${f1(W * 0.16)}" ry="${f1(14 * k)}" fill="#c9ced8" opacity="0.5"/>`;
    return t;
  });
  const groups = PLACE_GROUPS.filter(([gid]) => gid !== "imagination"), cols = groups.length, rows = Math.max(...groups.map(([, , kinds]) => kinds.length));
  const mx = W * 0.03, top = y0 + H * 0.05, bottom = g - 70 * k, cw = (W - 2 * mx) / cols, rh = (bottom - top - 26 * k) / rows;
  const fw = Math.min(cw * 0.78, rh * 0.86 / 0.74 * 0.8), depth = (o.depth ?? 0) + 1;
  const raster = depth === 1 && o.raster != null;   // (ticket 509: its pictures as pixels in play)
  // a warm glow behind the map, breathing with the slow LFO
  // (ticket 284: the glow and the map board wall objects of their own; its pictures are doorways, as everywhere)
  s += SEP + wallObj("glow", part("imagination-glow", [c.F, sceneLfoKey(o, "slow")], () => `<ellipse cx="0" cy="${f1((top + bottom) / 2)}" rx="${f1(W * 0.62)}" ry="${f1((bottom - top) * 0.75)}" fill="#fff3c4" opacity="0.25">` +
    sceneLfo(o, "slow", 0, "opacity", (v, amp) => (0.25 * (1 + (0.3 + 0.6 * amp) * v)).toFixed(3)) + `</ellipse>`));
  s += SEP + wallObj("map board", part("imagination-map", [c.F, cols, rows, sceneLfoKey(o, "fast")], () =>
    `<rect x="${f1(x0 + mx * 0.5)}" y="${f1(top - 6 * k)}" width="${f1(W - mx)}" height="${f1(bottom - top + 12 * k)}" rx="${f1(10 * k)}" fill="#ffffff" stroke="#d5dae3" stroke-width="${f1(2 * k)}"/>` +
    groups.map(([, name, kinds], i) => {
      const cx = x0 + mx + cw * (i + 0.5), ys = kinds.map((_, j) => top + 26 * k + rh * (j + 0.5));
      return `<text x="${f1(cx)}" y="${f1(top + 14 * k)}" text-anchor="middle" font-size="${f1(13 * k)}" font-weight="bold" fill="#5a6270" font-family="sans-serif">${name}</text>` +
        (ys.length > 1 ? `<line x1="${f1(cx)}" y1="${f1(ys[0])}" x2="${f1(cx)}" y2="${f1(ys[ys.length - 1])}" stroke="#b9c0cc" stroke-width="${f1(3 * k)}" stroke-dasharray="${f1(2 * k)} ${f1(6 * k)}" stroke-linecap="round">` +
          sceneLfo(o, "fast", i / cols, "stroke-dashoffset", (v, amp) => f1((2 + 5 * amp) * k * v)) + `</line>` : "");
    }).join("")));
  // the pictures, each a way there (in a picture of this room, only their frames)
  s += SEP + groups.map(([, , kinds], i) => kinds.map((kd, j) => {
    const cx = x0 + mx + cw * (i + 0.5), cy = top + 26 * k + rh * (j + 0.5), name = (PLACES.find(([v]) => v === kd) || [kd, kd])[1];
    return (depth > 2 ? `<g class="portal" data-to="${kd}"><rect x="${f1(cx - fw / 2)}" y="${f1(cy - fw * 0.37)}" width="${f1(fw)}" height="${f1(fw * 0.74)}" fill="#e8ebf0" stroke="#b8862f" stroke-width="${f1(3 * k)}"/></g>`
                      : framedSvg(kd, 45, o, depth, cx, cy - 6 * k, fw, !PERF.livePictures, raster)) +
      `<text x="${f1(cx)}" y="${f1(cy + fw * 0.37 + 4 * k)}" text-anchor="middle" font-size="${f1(9 * k)}" fill="#5a6270" font-family="sans-serif">${name}</text>`;
  }).join("")).join(SEP);
  // (ticket 145) the ladders: from just off the floor at the wall's foot up
  // to just under the column's name, in the gap at each column's right,
  // climbed till the hands are at its top (data-top), so the top row is in
  // reach; each bobs a
  // few units, slowly, at a pace and a phase of its own; its shadow on the
  // floor under it stays put
  const wall = imaginationWall(g, k), ltop = top + 22 * k, lw = Math.min((cw - fw) * 0.36, 13 * k), foot = wall - 5 * k;
  s += SEP + part("imagination-ladders", [c.F, cols, rows], () => groups.map((_, i) => {
    const lx = x0 + mx + cw * (i + 1);
    let t = "";
    for (let y = foot - 14 * k; y > ltop + 4 * k; y -= 20 * k)
      t += `<line x1="${f1(lx - lw)}" y1="${f1(y)}" x2="${f1(lx + lw)}" y2="${f1(y)}" stroke="#e3c37f" stroke-width="${f1(2.8 * k)}" stroke-linecap="round"/>`;
    t += [-1, 1].map(e => `<line x1="${f1(lx + e * lw)}" y1="${f1(ltop)}" x2="${f1(lx + e * lw)}" y2="${f1(foot)}" stroke="#c9a15a" stroke-width="${f1(3.4 * k)}" stroke-linecap="round"/>`).join("");
    return obj(`ladder ${i + 1}`, wall, `<ellipse cx="${f1(lx)}" cy="${f1(wall)}" rx="${f1(lw * 1.7)}" ry="${f1(3 * k)}" fill="#c9ced8" opacity="0.45"/>` +
      `<g data-ladder="1" data-top="hands"><animateTransform attributeName="transform" type="translate" values="0 0;0 ${f1(-3.5 * k)};0 0" dur="${(5.5 + 0.7 * (i % 3)).toFixed(1)}s" begin="${(-1.7 * i).toFixed(1)}s" repeatCount="indefinite" calcMode="spline" keySplines="0.45 0 0.55 1;0.45 0 0.55 1"/>${t}</g>`);
  }).join(""));
  return s;
}
// (ticket 185, Matthew, 2026-10-17) The forest crossing: a blank place for
// the world designer, a forest floor (olive green unless chosen) where up to
// eight brown paths meet: N (ahead, into the far woods), NE and NW (ahead
// and to the side), E and W (off the sides), S (toward us), SE and SW
// (toward us and to the side). Each crossing says which it has (crossN ...
// crossNW); a grid cell will say it for each crossing laid in it. Its things
// (things, which every place has since 0.0.100: rocks, bushes, trees and pines) stand where they were put,
// each sized for its depth as the friend is, and drag as one piece. Each
// thing is its own instance, { kind, x, z, seed, size, mirror }: what it is,
// where its feet are, and its own parameters, which its drawing takes (so
// two of a kind look different), all of them options the nature drawings
// already took (Matthew, 2026-10-17: expose them, change no drawing): for
// every kind, mirror (left for right, about its feet) and 🎲 another shape
// (a new seed: a tree's branches grow another way); a rock's colour (tone);
// a tree's height (h) and leaves (greens); a pine's tiers; a bush's
// flowers. THING_PARAMS lists them per kind ("all" for every kind), for the
// controls: a toggle, colours, a few choices, or an action.
const CROSS_DIRS = [["N", "⬆️ ahead"], ["NE", "↗️ ahead, right"], ["E", "➡️ right"], ["SE", "↘️ toward us, right"],
                    ["S", "⬇️ toward us"], ["SW", "↙️ toward us, left"], ["W", "⬅️ left"], ["NW", "↖️ ahead, left"]];
const CROSS_PATH = "#8a5528", CROSS_PATH_EDGE = "#5f3a1a";
const CROSS_FLOORS = ["#76843a", "#5f7a3a", "#8a9a4a", "#6b6b3a", "#4f6b3a", "#9a8a5a"];
const PINE_GREEN = "#2f6e3a";   // (the lake's and the crossing's pines)
const TOADSTOOL_CAPS = ["#d62828", "#f59e0b", "#7c3aed", "#2563eb", "#16a34a"];
const xmlEsc = s => String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
const xmlText = s => xmlEsc(String(s).slice(0, 12));
// (Matthew, 2026-10-17: slowly away from the places' own signs, toward the
// arrows; but M likes signs) A post with a board for each way chosen, each
// with the name of the square it leads to. A board can't turn to face its
// way, so where it is nailed says it: ways away from us (NW, NE) high up,
// across (W, E) in the middle, toward us (SW, SE) low down, each on its
// side of the post and pointing out, the diagonals tipped up (away) or down
// (toward us); N on top of the post, S across its foot; and each says its
// arrow. Eight fit without crowding: three a side, one above, one below,
// and a long name is squeezed onto its board rather than widening it.
const SIGN_AT = { N: [0, -268], NW: [-1, -218], NE: [1, -218], W: [-1, -174], E: [1, -174], SW: [-1, -130], SE: [1, -130], S: [0, -76] };
const SIGN_TILT = { NE: -9, NW: 9, SE: 9, SW: -9, E: 0, W: 0 };
function signpostSvg(boards) {
  let s = `<ellipse cx="0" cy="2" rx="34" ry="7" fill="#000000" opacity="0.18"/><rect x="-7" y="-256" width="14" height="256" rx="2" fill="#6e4a2c" stroke="#4a2e1a" stroke-width="1.5"/>`;
  for (const [d, label] of boards) {
    const [side, y] = SIGN_AT[d], words = `${WORLD_ARROWS[d]} ${label}`, n = [...words].length, want = n * 7.6 + 20, w = Math.min(156, Math.max(64, want)), h = 28;
    const text = (x, anchor) => `<text x="${f1(x)}" y="${f1(y + 5)}" text-anchor="${anchor}" font-size="14" font-weight="bold" fill="#fff8e7" font-family="sans-serif"` +
      (want > w ? ` textLength="${f1(w - 20)}" lengthAdjust="spacingAndGlyphs"` : "") + `>${xmlEsc(words)}</text>`;
    // (ticket 280) each board a group that says its way: a click on it takes that way, as the nav arrow does
    s += `<g class="sign-board" data-way="${d}">`;
    if (!side) { s += `<rect x="${f1(-w / 2)}" y="${f1(y - h / 2)}" width="${f1(w)}" height="${h}" rx="5" fill="#a0703f" stroke="#5e3d22" stroke-width="2.5"/>` + text(0, "middle") +
                      `<circle cx="0" cy="${f1(y)}" r="2" fill="#3a2a1a" opacity="0.6"/></g>`; continue; }
    const x0 = side * 5, x1 = side * (5 + w), tip = side * (5 + w + 14);
    s += `<g transform="rotate(${SIGN_TILT[d]} 0 ${y})"><polygon points="${f1(x0)},${f1(y - h / 2)} ${f1(x1)},${f1(y - h / 2)} ${f1(tip)},${y} ${f1(x1)},${f1(y + h / 2)} ${f1(x0)},${f1(y + h / 2)}" fill="#a0703f" stroke="#5e3d22" stroke-width="2.5" stroke-linejoin="round"/>` +
         text((x0 + x1) / 2, "middle") + `<circle cx="${f1(side * 11)}" cy="${y}" r="2" fill="#3a2a1a" opacity="0.6"/></g></g>`;
  }
  return s;
}
// the ways a sign in this place may point: its square's ways out that meet a way back (none off the grid)
function signWays(o) {
  const W = state.world;
  if (!W || !W.at || !W.cells[W.at] || W.cells[W.at].kind !== o.kind) return [];
  return worldLiveWays(W, W.at).filter(d => worldMeets(W, W.at, d));
}
// its boards: [way, the name of the square it leads to], for the ways chosen (all, until some are)
function signpostBoards(o, t) {
  const W = state.world, ok = signWays(o), names = ok.length ? worldNames(W) : {};
  return Object.keys(WORLD_DIRS).filter(d => ok.includes(d) && (t.dirs ? t.dirs.includes(d) : true)).map(d => [d, names[worldMeets(W, W.at, d)]]);
}
const ROCK_TONES = ["#8f8a80", "#b4afa4", "#6f8a94", "#c2a67a", "#a8634a", "#7f8a6a", "#4f4b48"];
// (ticket 323) a trap room's boulders: a rock tone named, or a surprise
const BOULDER_TONES = [...["🪨 grey", "🪨 pale", "🪨 blue grey", "🪨 sandstone", "🪨 red", "🪨 mossy", "🪨 dark"].map((name, i) => [ROCK_TONES[i], name]), ["surprise", "🎲 a surprise"]];
const BOULDER_KNOB = "#2f6fd6";   // (ticket 323: a boulder lever's knob, blue, N's)
// (ticket 323) a lever's knob: its own colour, or ("auto") red for trap doors and blue for boulders, as the trap room's
const KNOB_COLORS = ["auto", "#c0392b", BOULDER_KNOB, "#27ae60", "#f2c12e", "#8e44ad", "#ff8c42", "#1f1f22", "#ecf0f1"];
const knobOf = t => t.knob && t.knob !== "auto" ? t.knob : t.does === "boulder" ? BOULDER_KNOB : "#c0392b";
const LEAF_SETS = { summer: null, spring: ["#5f9e3a", "#74b04a", "#8cc45a", "#a3d46e", "#b8e080"], autumn: ["#9a3412", "#c2410c", "#d97706", "#eab308", "#b45309"],
                    blossom: ["#f9a8d4", "#f472b6", "#fbcfe8", "#ec4899", "#fce7f3"] };
// (ticket 219: the sea things' colours, here before THING_PARAMS and THING_DEFAULTS read them: declared after, they
// threw on loading, "Cannot access before initialization", and 0.0.140 stopped there)
const CORAL_COLORS = ["#ff6f91", "#ff9f1c", "#c084fc", "#f43f5e", "#fde047", "#fb7185", "#f8fafc"];
const SHELL_COLORS = ["#f8e1c7", "#f9a8d4", "#fde68a", "#e9d5ff", "#ffffff", "#fdba74"];
const STAR_COLORS = ["#fb923c", "#f43f5e", "#a855f7", "#facc15", "#38bdf8"];
const SEAFISH_COLORS = ["#ff9f1c", "#ffd23f", "#ff6f91", "#7bdff2", "#a3e635", "#c084fc"];
const SEAPLANT_COLORS = ["#2f8f4e", "#4caf50", "#8bc34a", "#c0ca33", "#d4a017", "#e57373", "#ab47bc"];
// (ticket 290, for N: "bubble makers: little boxes with turning wheels on the front, simplified to a few shapes at first";
// his picture, a round toy bubble machine, a wheel of bubble rings turning on its front, a handle on top, a base under it)
// A bubble maker, its foot at the origin, about 100 of its units tall, in one of MAKER_SHAPES: ⭕ round (his picture: a
// round body on a base, a handle on top), 📦 a box (the ticket's own words: a little box, its wheel on the front, on short
// feet), 🗼 a tower (tall, two wheels one over the other, a little roof); in its colour; each wheel of six rings turning on
// its front (faster the more bubbles a second it blows). Each shape's wheels (cx, cy, r) and where its bubbles leave them
// (out: its right, the way it blows; mirrored, its left), for makerTick: from each wheel by turns.
const MAKER_COLORS = ["#ff7eb6", "#4fa3ff", "#ffd23f", "#5fd068", "#a77bff", "#ff8a3d"];
const MAKER_SHAPES = {
  round: { wheels: [{ cx: 0, cy: -56, r: 26, out: [24, -70] }] },
  box: { wheels: [{ cx: 0, cy: -50, r: 24, out: [22, -62] }] },
  tower: { wheels: [{ cx: 0, cy: -48, r: 18, out: [17, -56] }, { cx: 0, cy: -96, r: 18, out: [17, -104] }] },
};
const makerShape = t => MAKER_SHAPES[t.shape] || MAKER_SHAPES.round;
const makerWheel = (cx, cy, r, dark, lite, dur) => `<circle cx="${cx}" cy="${cy}" r="${r + 4}" fill="${lite}" stroke="${dark}" stroke-width="2.5"/>` +
  `<g><animateTransform attributeName="transform" type="rotate" values="0 ${cx} ${cy};360 ${cx} ${cy}" dur="${dur}s" repeatCount="indefinite"/>` +
  [0, 1, 2, 3, 4, 5].map(i => { const a = i * Math.PI / 3, x = cx + Math.cos(a) * (r - 8 * r / 26), y = cy + Math.sin(a) * (r - 8 * r / 26), rr = 7 * Math.max(0.7, r / 26);
    return `<line x1="${cx}" y1="${cy}" x2="${f1(x)}" y2="${f1(y)}" stroke="${dark}" stroke-width="2"/><circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(rr)}" fill="#ffffff" fill-opacity="0.35" stroke="#ffffff" stroke-width="2.5"/>`; }).join("") +
  `</g><circle cx="${cx}" cy="${cy}" r="${f1(5 * Math.max(0.7, r / 26))}" fill="${dark}"/>`;
function bubbleMakerSvg(t) {
  const col = t.color || MAKER_COLORS[0], dark = mix(col, "#000000", 0.35), lite = mix(col, "#ffffff", 0.45), sh = t.shape || "round";
  const dur = Math.max(0.5, 8 / Math.max(1, t.rate ?? 4)).toFixed(2), W = makerShape(t).wheels;
  const wheels = W.map(w => makerWheel(w.cx, w.cy, w.r, dark, lite, dur)).join("");
  if (sh === "box") return `<rect x="-38" y="-8" width="12" height="8" rx="2" fill="${dark}"/><rect x="26" y="-8" width="12" height="8" rx="2" fill="${dark}"/>` +
    `<rect x="-44" y="-88" width="88" height="80" rx="10" fill="${col}" stroke="${dark}" stroke-width="3"/><path d="M-36 -80 V-24" stroke="${lite}" stroke-width="5" stroke-linecap="round" opacity="0.8"/>` +
    `<rect x="-14" y="-100" width="28" height="12" rx="4" fill="${dark}"/>` + wheels;
  if (sh === "tower") return `<rect x="-30" y="-12" width="60" height="12" rx="5" fill="${dark}"/>` +
    `<rect x="-26" y="-124" width="52" height="112" rx="8" fill="${col}" stroke="${dark}" stroke-width="3"/><path d="M-19 -116 V-24" stroke="${lite}" stroke-width="4" stroke-linecap="round" opacity="0.8"/>` +
    `<path d="M-32 -124 L0 -144 L32 -124 Z" fill="${dark}"/>` + wheels;
  const { cy } = W[0];
  return `<rect x="-36" y="-14" width="72" height="14" rx="6" fill="${dark}"/><rect x="-22" y="-22" width="44" height="10" rx="4" fill="${dark}"/>` +
    `<circle cx="0" cy="${cy}" r="40" fill="${col}" stroke="${dark}" stroke-width="3"/><path d="M-26 ${cy - 26} Q-36 ${cy - 6} -30 ${cy + 14}" fill="none" stroke="${lite}" stroke-width="5" stroke-linecap="round" opacity="0.8"/>` +
    `<path d="M-18 ${cy - 37} Q0 ${cy - 70} 18 ${cy - 37}" fill="none" stroke="${dark}" stroke-width="7" stroke-linecap="round"/>` + wheels;
}
// (ticket 471, Matthew: "Gems are extra things to add, including 6 styles"; "puddle on ground, with/without drip from ceiling.
// Little waterfall coming out of the wall, and trickling down the rock (the placeable object would be the water and the it
// runs down, into a puddle on the floor; all connected into one placeable object, for now --- 6 styles") The 🦇 cave things,
// for the cave room and anywhere: each drawn standing at 0, 0, in its own units, as thingSvg places it.
const GEM_STYLES = [["cut", "💎 cut"], ["cluster", "🔮 a cluster"], ["geode", "🪨 a geode"], ["point", "🔺 a single point"], ["rough", "🫘 rough"], ["heart", "💖 a heart"]];
const GEM_COLORS = ["#a855f7", "#3b82f6", "#e11d48", "#22c55e", "#fbbf24", "#e0f2fe"];
const CASCADE_STYLES = [["trickle", "💧 a trickle"], ["sheet", "🫗 a sheet over a ledge"], ["steps", "🪜 down steps"], ["spout", "⛲ a spout from a hole"], ["mossy", "🌿 mossy"], ["glowing", "✨ glowing"]];
const PUDDLE_RY = 11;   // (a puddle's depth either side of its middle, in its own units: the friend stands in it, in front of its back edge)
// (a sparkle on a gem: a four-point star at (x, y), twinkling with the fast LFO, each its own way in)
const gemSparkle = (o, x, y, r, off) => `<path d="M${f1(x)} ${f1(y - r)} L${f1(x + r * 0.25)} ${f1(y - r * 0.25)} L${f1(x + r)} ${f1(y)} L${f1(x + r * 0.25)} ${f1(y + r * 0.25)} L${f1(x)} ${f1(y + r)} L${f1(x - r * 0.25)} ${f1(y + r * 0.25)} L${f1(x - r)} ${f1(y)} L${f1(x - r * 0.25)} ${f1(y - r * 0.25)} Z" fill="#ffffff" opacity="0">` +
  lfoTimed(o, "fast", off, `<animate attributeName="opacity" values="0;1;0;0" keyTimes="0;0.15;0.3;1" dur="1.8s" repeatCount="indefinite"/>`) + `</path>`;
function gemThingSvg(t, seed, o) {
  const col = t.color || GEM_COLORS[0], lite = mix(col, "#ffffff", 0.55), dark = mix(col, "#000000", 0.35), R = seeded(seed), st = t.style || "cut";
  const glow = t.glow ?? true ? `<defs><radialGradient id="gem-glow-${seed}"><stop offset="0" stop-color="${col}" stop-opacity="0.7"/><stop offset="1" stop-color="${col}" stop-opacity="0"/></radialGradient></defs>` +
    `<ellipse class="fumes" cx="0" cy="-18" rx="44" ry="34" fill="url(#gem-glow-${seed})" opacity="0.5">` + lfoTimed(o, "slow", R(1), `<animate attributeName="opacity" values="0.3;0.7;0.3" dur="4s" repeatCount="indefinite"/>`) + `</ellipse>` : "";
  const shade = `<ellipse class="flat" cx="0" cy="1" rx="22" ry="4" fill="#000000" opacity="0.2"/>`;
  const prism = (x, h, a, w = 7) => `<g transform="translate(${x} 0) rotate(${f1(a)})"><polygon points="${-w},0 ${-w},${f1(-h * 0.8)} 0,${f1(-h)} ${w},${f1(-h * 0.8)} ${w},0" fill="${col}" stroke="${lite}" stroke-width="1.4" opacity="0.92"/>` +
    `<polygon points="0,0 0,${f1(-h)} ${w},${f1(-h * 0.8)} ${w},0" fill="${dark}" opacity="0.35"/></g>`;
  let body;
  if (st === "cluster") body = [[-12, 26, -30], [10, 30, 26], [-4, 40, -8], [4, 46, 6], [16, 18, 50], [-16, 16, -52]].map(([x, h, a]) => prism(x, h * (0.85 + 0.3 * R(x + 40)), a)).join("");
  else if (st === "point") body = prism(0, 62, (R(2) - 0.5) * 16, 10);
  else if (st === "geode") body = `<path d="M-30 0 Q-32 -34 0 -38 Q32 -34 30 0 Z" fill="#6b5a4a" stroke="#3a2e24" stroke-width="2.5"/><path d="M-22 -4 Q-23 -28 0 -31 Q23 -28 22 -4 Z" fill="${dark}"/>` +
    [...Array(14)].map((_, i) => { const a = Math.PI * (0.08 + 0.84 * i / 13), r = 20 + 3 * R(i + 9), x = -Math.cos(a) * r, y = -4 - Math.sin(a) * r * 1.1;
      return `<polygon points="${f1(x)},${f1(y)} ${f1(x * 0.6 - 3)},${f1(y * 0.6)} ${f1(x * 0.6 + 3)},${f1(y * 0.6)}" fill="${i % 2 ? col : lite}" opacity="0.95"/>`; }).join("");
  else if (st === "rough") body = `<path d="M-22 0 L-26 -14 L-16 -30 L2 -34 L20 -26 L26 -10 L18 0 Z" fill="${col}" stroke="${dark}" stroke-width="2" opacity="0.9"/>` +
    `<path d="M-16 -30 L-6 -16 L2 -34 M-6 -16 L20 -26 M-6 -16 L-4 0 M-6 -16 L-26 -14" fill="none" stroke="${lite}" stroke-width="1.5" opacity="0.7"/>`;
  else if (st === "heart") body = `<path d="M0 0 L-26 -24 Q-34 -40 -18 -46 Q-6 -48 0 -36 Q6 -48 18 -46 Q34 -40 26 -24 Z" fill="${col}" stroke="${dark}" stroke-width="2"/>` +
    `<path d="M0 -36 L0 0 M-26 -24 L0 -14 L26 -24 M-18 -46 L-10 -30 L0 -36 L10 -30 L18 -46" fill="none" stroke="${lite}" stroke-width="1.4" opacity="0.7"/>`;
  else body = `<path d="M-24 -26 L-14 -36 L14 -36 L24 -26 L0 0 Z" fill="${col}" stroke="${dark}" stroke-width="2"/><path d="M-24 -26 L24 -26" stroke="${lite}" stroke-width="1.6"/>` +
    `<path d="M-14 -36 L-8 -26 L0 -36 L8 -26 L14 -36 M-8 -26 L0 0 L8 -26 M-24 -26 L0 0" fill="none" stroke="${lite}" stroke-width="1.2" opacity="0.75"/><path d="M-14 -36 L0 -36 L-8 -26 Z" fill="#ffffff" opacity="0.35"/>`;
  return glow + shade + body + gemSparkle(o, st === "point" ? 3 : -6, st === "point" ? -54 : st === "cluster" ? -36 : -30, 7, R(3)) + gemSparkle(o, 10, -14, 4, R(4));
}
// a puddle: its edge uneven (its seed), a shine on it; with a drip, a drop falls into it from the top of the picture (top, in
// its own units) and rings spread (the fast LFO)
function puddleThingSvg(t, seed, o, top) {
  const R = seeded(seed), pts = [];
  for (let n = 0; n <= 18; n++) { const a = n / 18 * 2 * Math.PI, b = 1 + 0.22 * (R(n) - 0.5); pts.push([46 * Math.cos(a) * b, PUDDLE_RY * Math.sin(a) * b]); }
  const d = caveSmooth(pts, (x, y) => `${f1(x)} ${f1(y)}`) + " Z";
  let s = `<path d="${d}" fill="#6fb6cf" opacity="0.5"/><path d="${d}" fill="none" stroke="#2c3e46" stroke-width="2" opacity="0.4"/>` +
          `<ellipse cx="-12" cy="-3" rx="16" ry="2.6" fill="#e6f8ff" opacity="0.45"/>`;
  if (t.drip ?? true) s += [0, 1].map(j => `<ellipse cx="0" cy="0" rx="4" ry="1" fill="none" stroke="#e6f8ff" stroke-width="1.4" opacity="0">` +
      lfoTimed(o, "fast", 0.1 + j * 0.2, `<animate attributeName="rx" values="4;38" dur="1.6s" repeatCount="indefinite"/>`) +
      lfoTimed(o, "fast", 0.1 + j * 0.2, `<animate attributeName="ry" values="1;9" dur="1.6s" repeatCount="indefinite"/>`) +
      lfoTimed(o, "fast", 0.1 + j * 0.2, `<animate attributeName="opacity" values="0.85;0" dur="1.6s" repeatCount="indefinite"/>`) + `</ellipse>`).join("") +
    `<circle cx="0" cy="${f1(top)}" r="3" fill="#bfeaff">` + lfoTimed(o, "fast", 0, `<animate attributeName="cy" values="${f1(top)};0" dur="1.6s" repeatCount="indefinite"/>`) +
    lfoTimed(o, "fast", 0, `<animate attributeName="opacity" values="1;1;0" keyTimes="0;0.92;1" dur="1.6s" repeatCount="indefinite"/>`) + `</circle>`;
  return s;
}
// a little waterfall: a slab of rock, the water coming out near its top and running down its face into a pool at its foot,
// all one thing (the pool lies flat: the rock is what stands in the way); six kinds
function cascadeThingSvg(t, seed, o) {
  const R = seeded(seed), st = t.style || "trickle", Rs = (i, j) => seeded(seed + j)(i), P = (x, y) => `${f1(x)} ${f1(y)}`;
  const glowing = st === "glowing", water = glowing ? "#7df9ff" : "#b8e6f5", rock = st === "mossy" ? "#5d6152" : glowing ? "#4a4560" : "#6a6276";
  // (a lump of the cave's rock, wide at its foot and leaning, never a slab: an even one looked like a headstone)
  const edge = caveFractalPath([[-62, 0], [-58, -34], [-48, -78], [-30, -116], [-8, -138], [18, -132], [38, -104], [56, -62], [64, -20], [58, 0]], 3, 0.1, Rs, 11);
  let s = `<ellipse class="flat" cx="6" cy="6" rx="62" ry="13" fill="${water}" opacity="0.5"/><ellipse class="flat" cx="0" cy="4" rx="40" ry="5" fill="#ffffff" opacity="0.25"/>` +
          `<path d="${caveSmooth(edge, P)} Z" fill="${rock}" stroke="#2a2630" stroke-width="2.5"/>` +
          [[-36, -24, 18], [34, -30, 20], [-24, -84, 15], [30, -86, 14], [6, -116, 12]].map(([x, y, r]) => `<ellipse cx="${x}" cy="${y}" rx="${r}" ry="${f1(r * 0.7)}" fill="${mix(rock, "#000000", 0.18)}" opacity="0.6"/>`).join("") +
          [[-26, -100, 16], [14, -70, 20], [-10, -36, 14], [24, -112, 10], [-44, -50, 12], [44, -40, 12]].map(([x, y, r]) => `<path d="M${P(x - r, y)} Q${P(x - r * 0.4, y - r * 0.8)} ${P(x + r * 0.5, y - r * 0.7)}" fill="none" stroke="${mix(rock, "#ffffff", 0.25)}" stroke-width="2.5" stroke-linecap="round" opacity="0.6"/>`).join("");
  // where the water comes out: a dark crack or hole near the top
  const sx = st === "spout" ? -8 : 4, sy = st === "spout" ? -92 : -118;
  s += `<ellipse cx="${sx}" cy="${sy}" rx="${st === "spout" ? 9 : 12}" ry="${st === "spout" ? 7 : 4}" fill="#14111a"/>`;
  // the water's paths down, by kind
  let paths;
  if (st === "sheet") { s += `<path d="M-30 -84 L30 -84 L36 -76 L-34 -76 Z" fill="${mix(rock, "#000000", 0.25)}"/>`; paths = [-24, -14, -4, 6, 16, 26].map(x => `M${P(x * 0.6 + sx, sy)} Q${P(x * 0.9, -100)} ${P(x, -80)} L${P(x * 1.15, 4)}`); }
  else if (st === "steps") { s += [-96, -62, -28].map((y, j) => `<path d="M${P(-28 + j * 8, y)} L${P(22 + j * 8, y)} L${P(26 + j * 8, y + 6)} L${P(-30 + j * 8, y + 6)} Z" fill="${mix(rock, "#000000", 0.25)}"/>`).join("");
    paths = [-4, 4].map(dx => `M${P(sx + dx, sy)} L${P(10 + dx, -96)} L${P(20 + dx, -96)} L${P(18 + dx, -62)} L${P(28 + dx, -62)} L${P(26 + dx, -28)} L${P(34 + dx, -28)} L${P(30 + dx, 4)}`); }
  else if (st === "spout") paths = [-3, 0, 3].map(dx => `M${P(sx, sy + dx)} Q${P(30, sy - 20 + dx)} ${P(44 + dx * 2, 4)}`);
  else paths = [-1, 1, ...(st === "trickle" ? [] : [3])].map(j => `M${P(sx + j * 4, sy)} Q${P(sx + j * 10 + (R(j + 5) - 0.5) * 30, -80)} ${P(sx + j * 6, -50)} Q${P(sx + j * 12 + (R(j + 7) - 0.5) * 24, -24)} ${P(sx + j * 8, 4)}`);
  const wide = st === "sheet" ? 7 : st === "trickle" ? 3 : 4.5;
  s += paths.map((d, j) => `<path d="${d}" fill="none" stroke="${water}" stroke-width="${wide}" stroke-linecap="round" stroke-linejoin="round" opacity="0.8"/>` +
    `<path d="${d}" fill="none" stroke="#ffffff" stroke-width="${f1(wide * 0.45)}" stroke-dasharray="10 16" stroke-linecap="round" opacity="0.8">` +
    lfoTimed(o, "fast", j / paths.length, `<animate attributeName="stroke-dashoffset" values="0;-52" dur="1s" repeatCount="indefinite"/>`) + `</path>`).join("");
  // the splash in the pool: rings, and bubbles bobbing
  const fx = st === "spout" ? 44 : st === "steps" ? 30 : st === "sheet" ? 0 : sx + 8;
  s += [0, 1].map(j => `<ellipse cx="${fx}" cy="4" rx="4" ry="1.2" fill="none" stroke="#ffffff" stroke-width="1.3" opacity="0">` +
    lfoTimed(o, "fast", j / 2, `<animate attributeName="rx" values="4;24" dur="1.2s" repeatCount="indefinite"/>`) +
    lfoTimed(o, "fast", j / 2, `<animate attributeName="opacity" values="0.8;0" dur="1.2s" repeatCount="indefinite"/>`) + `</ellipse>`).join("");
  if (st === "mossy") s += [[-40, -20], [-30, -90], [36, -40], [20, -128], [-44, -60]].map(([x, y], j) => `<ellipse cx="${x}" cy="${y}" rx="${10 + 6 * R(j)}" ry="${5 + 3 * R(j + 9)}" fill="#4f7f3a" opacity="0.9"/>`).join("") +
    grassTuft(-38, 0, 18, seed + 3, "#4f7f3a", 2) + grassTuft(40, -2, 14, seed + 4, "#5f8f48", 2);
  if (glowing) s = `<defs><radialGradient id="cascade-glow-${seed}"><stop offset="0" stop-color="#7df9ff" stop-opacity="0.6"/><stop offset="1" stop-color="#7df9ff" stop-opacity="0"/></radialGradient></defs>` +
    `<ellipse class="fumes" cx="0" cy="-60" rx="80" ry="90" fill="url(#cascade-glow-${seed})" opacity="0.5">` + lfoTimed(o, "slow", R(2), `<animate attributeName="opacity" values="0.3;0.7;0.3" dur="4s" repeatCount="indefinite"/>`) + `</ellipse>` + s +
    [[-30, -50], [28, -96], [-18, -118]].map(([x, y], j) => gemSparkle(o, x, y, 5, R(j + 20))).join("");
  return s;
}
// (ticket 471, Matthew: "we also want to place cave passageways (dark openings that look like natural archways, holes in
// the wall, and holes in the floor)") A cave passage put in a place: a way through, to the place it says (🧭, any place
// of the list but a castle's or a tree house's rooms, which need more to say which), and back by ⬅️ as any way. Its
// opening is the doorway (glowing, gone through as any: the friend walks to its foot, or, a hole in the floor, its
// middle); the rock round it stands where it is drawn but is no more in the way than a door (one goes into it).
const PASSAGE_STYLES = [["arch", "🌑 a natural archway"], ["wallhole", "🕳️ a hole through a wall of rock"], ["floorhole", "⚫ a hole in the floor"]];
// (the review after 0.0.385: only places a doorway can take the friend to, enterScene's, so not 🌊 open water or 🪸 deep sea,
// reached only from the world's squares)
const passagePlaces = () => (typeof PLACES === "undefined" ? [] : PLACES).filter(([k]) => !["none", "castleroom", "treeroom"].includes(k) && (SCENES[k] || PICTURE_CHOICES.some(([q]) => q === k)));
// (ticket 525) where it leads by kind: the place it is linked to (dest), else the place its 🧭 says
const passageTo = t => (t.dest && passageDestOk(t.dest) && t.dest.kind) || t.to || "cave";   // (a square taken away: its own 🧭 place till it is back)
function passageThingSvg(t, seed) {
  const st = t.style || "arch", Rs = (i, j) => seeded(seed + j)(i), P = (x, y) => `${f1(x)} ${f1(y)}`, to = passageTo(t), c = { k: 1, R: Rs, P };
  // (ticket 525) a passage with its name (uid) is a way with a nav arrow too: data-way "p-<uid>" (worldWaysHud's arrow finds it),
  // data-passage (enterScene: to the square or inside it is linked to, its dest, out by its twin); no data-door, so measureMarks
  // keeps it a doorway, glowing its opening
  const portal = inner => `<g class="portal" data-to="${to}"${t.uid ? ` data-way="p-${t.uid}" data-passage="${t.uid}"` : ""}>${inner}</g>`;
  if (st === "floorhole") {
    const pts = [];
    for (let n = 0; n <= 18; n++) { const a = n / 18 * 2 * Math.PI, b = 1 + 0.2 * (Rs(n, 3) - 0.5); pts.push([48 * Math.cos(a) * b, 15 * Math.sin(a) * b]); }
    const d = caveSmooth(pts, P) + " Z";
    return `<defs><radialGradient id="passage-hole-${seed}" cx="0.5" cy="0.4" r="0.6"><stop offset="0" stop-color="#050407"/><stop offset="0.7" stop-color="#0d0b10"/><stop offset="1" stop-color="#2a2530"/></radialGradient></defs>` +
      portal(`<path d="${d}" fill="url(#passage-hole-${seed})"/><path d="${d}" fill="none" stroke="#6a6276" stroke-width="3" opacity="0.8"/>`) +
      [...Array(9)].map((_, i) => { const a = Math.PI * (0.05 + 0.9 * i / 8), x = 54 * Math.cos(a), y = 17 * Math.sin(a) + 2, r = 5 + 4 * Rs(i, 4);
        return `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(r)}" ry="${f1(r * 0.6)}" fill="${["#5c5468", "#6a6276", "#4a4352"][i % 3]}" stroke="#2a2630" stroke-width="1"/>`; }).join("");
  }
  // a lump of rock, an archway at its foot or a hole through it
  const edge = caveFractalPath([[-78, 0], [-76, -50], [-60, -104], [-30, -146], [6, -158], [40, -142], [66, -100], [80, -46], [76, 0]], 3, 0.08, Rs, 21);
  let r = `<ellipse class="flat" cx="0" cy="2" rx="80" ry="10" fill="#000000" opacity="0.2"/><path d="${caveSmooth(edge, P)} Z" fill="#5a5266" stroke="#2a2630" stroke-width="2.5"/>` +
    [[-48, -30, 16], [44, -40, 18], [-36, -110, 14], [30, -116, 13], [0, -138, 10]].map(([x, y, rr]) => `<ellipse cx="${x}" cy="${y}" rx="${rr}" ry="${f1(rr * 0.7)}" fill="#463f50" opacity="0.6"/>` +
      `<path d="M${P(x - rr, y)} Q${P(x - rr * 0.4, y - rr * 0.8)} ${P(x + rr * 0.5, y - rr * 0.7)}" fill="none" stroke="#7a7186" stroke-width="2.5" stroke-linecap="round" opacity="0.6"/>`).join("");
  if (st === "wallhole") {
    const pts = [];
    for (let n = 0; n <= 16; n++) { const a = n / 16 * 2 * Math.PI, b = 1 + 0.16 * (Rs(n, 5) - 0.5); pts.push([34 * Math.cos(a) * b, -78 + 30 * Math.sin(a) * b]); }
    const d = caveSmooth(pts, P) + " Z";
    return r + `<path d="M-40 -46 L40 -46 L46 -38 L-46 -38 Z" fill="#463f50"/>` +
      portal(`<defs><radialGradient id="passage-wall-${seed}" cx="0.5" cy="0.55" r="0.6"><stop offset="0" stop-color="#9a8ab8" stop-opacity="0.45"/><stop offset="0.5" stop-color="#110e15"/><stop offset="1" stop-color="#050407"/></radialGradient></defs>` +
        `<path d="${d}" fill="url(#passage-wall-${seed})"/><path d="${d}" fill="none" stroke="#0a080c" stroke-width="3"/>`);
  }
  return r + portal(caveArchSvg(c, `passage-arch-${seed}`, 0, 0, 74, 112, seed + 30, "#9a8ab8"));
}
// (ticket 471, Matthew: "mushrooms (already have these, but maybe more styles, for a dark cave?)") A cave mushroom, a thing
// of its own (the places' toadstools keep theirs): six kinds, glowing in their colour (the slow LFO), or not
const CAVE_MUSHROOMS = [["glowcap", "💡 glowing caps"], ["bells", "🔔 bells"], ["cluster", "🍄 a crowd"], ["shelves", "📚 shelves on a stump"], ["tall", "🕯️ tall and thin"], ["puffballs", "⚪ puffballs"]];
const CAVE_MUSHROOM_COLORS = ["#7dd3fc", "#a7f3d0", "#c4b5fd", "#fcd34d", "#f9a8d4", "#86efac"];
function caveMushroomSvg(t, seed, o) {
  const col = t.color || CAVE_MUSHROOM_COLORS[0], R = seeded(seed), st = t.style || "glowcap", rim = mix(col, "#1a1620", 0.45), stem = "#ece4f2";
  const glow = t.glow ?? true ? `<defs><radialGradient id="cmush-glow-${seed}"><stop offset="0" stop-color="${col}" stop-opacity="0.65"/><stop offset="1" stop-color="${col}" stop-opacity="0"/></radialGradient></defs>` +
    `<ellipse class="fumes" cx="0" cy="-30" rx="60" ry="46" fill="url(#cmush-glow-${seed})" opacity="0.5">` + lfoTimed(o, "slow", R(1), `<animate attributeName="opacity" values="0.3;0.75;0.3" dur="4s" repeatCount="indefinite"/>`) + `</ellipse>` : "";
  const shade = `<ellipse class="flat" cx="0" cy="1" rx="30" ry="5" fill="#000000" opacity="0.2"/>`;
  const cap = (x, h, r, tall = 0.8) => `<path d="M${f1(x - r * 0.16)} 0 L${f1(x - r * 0.1)} ${f1(-h)} L${f1(x + r * 0.1)} ${f1(-h)} L${f1(x + r * 0.16)} 0 Z" fill="${stem}"/>` +
    `<path d="M${f1(x - r)} ${f1(-h + 2)} Q${f1(x - r * 0.9)} ${f1(-h - r * tall)} ${f1(x)} ${f1(-h - r * tall * 1.05)} Q${f1(x + r * 0.9)} ${f1(-h - r * tall)} ${f1(x + r)} ${f1(-h + 2)} Q${f1(x)} ${f1(-h + r * 0.2)} ${f1(x - r)} ${f1(-h + 2)} Z" fill="${col}" stroke="${rim}" stroke-width="1.5"/>`;
  let body;
  if (st === "bells") body = [[-14, 30, 12], [6, 42, 14], [22, 24, 10]].map(([x, h, r]) => `<path d="M${f1(x - 1.5)} 0 Q${f1(x - 3)} ${f1(-h * 0.5)} ${f1(x)} ${f1(-h)} L${f1(x + 1.5)} ${f1(-h)} Q${f1(x + 2)} ${f1(-h * 0.5)} ${f1(x + 1.5)} 0 Z" fill="${stem}"/>` +
    `<path d="M${f1(x - r * 0.7)} ${f1(-h + r * 0.4)} Q${f1(x - r * 0.75)} ${f1(-h - r)} ${f1(x)} ${f1(-h - r * 1.1)} Q${f1(x + r * 0.75)} ${f1(-h - r)} ${f1(x + r * 0.7)} ${f1(-h + r * 0.4)} Q${f1(x)} ${f1(-h + r * 0.1)} ${f1(x - r * 0.7)} ${f1(-h + r * 0.4)} Z" fill="${col}" stroke="${rim}" stroke-width="1.4"/>`).join("");
  else if (st === "cluster") body = [...Array(9)].map((_, j) => { const x = (R(j + 3) - 0.5) * 50, h = 8 + 18 * R(j + 4), r = 5 + 6 * R(j + 5); return cap(x, h, r, 0.6); }).sort().join("");
  else if (st === "shelves") body = `<path d="M-20 0 L-16 -44 Q0 -50 16 -44 L20 0 Z" fill="#6b4a32" stroke="#3a2818" stroke-width="2"/><ellipse cx="0" cy="-45" rx="16" ry="4" fill="#a07a58"/>` +
    [[-16, -14, -1], [16, -24, 1], [-16, -34, -1], [16, -38, 1]].map(([x, y, e]) => `<path d="M${x} ${y} Q${x + e * 18} ${y - 6} ${x + e * 22} ${y + 2} Q${x + e * 12} ${y + 5} ${x} ${y + 3} Z" fill="${col}" stroke="${rim}" stroke-width="1.3"/>`).join("");
  else if (st === "tall") body = [[-8, 62, 7], [6, 48, 6], [16, 36, 5]].map(([x, h, r]) => cap(x, h, r, 1.3)).join("");
  else if (st === "puffballs") body = [[-14, 13], [8, 17], [24, 9]].map(([x, r]) => `<circle cx="${x}" cy="${-r + 1}" r="${r}" fill="${mix(col, "#ffffff", 0.45)}" stroke="${rim}" stroke-width="1.4"/>` +
    `<circle cx="${x - r * 0.35}" cy="${f1(-r * 1.35)}" r="${f1(r * 0.25)}" fill="#ffffff" opacity="0.6"/>`).join("");
  else body = cap(-16, 22, 12) + cap(4, 36, 20) + cap(24, 16, 9);
  return glow + shade + body + (t.glow ?? true ? [0, 1, 2].map(n => `<circle cx="${(n - 1) * 8}" cy="-50" r="1.6" fill="${col}" opacity="0">` +
    lfoTimed(o, "slow", n / 3 + R(2), `<animate attributeName="cy" values="-50;-100" dur="5s" repeatCount="indefinite"/>`) + lfoTimed(o, "slow", n / 3 + R(2), `<animate attributeName="opacity" values="0;0.9;0" dur="5s" repeatCount="indefinite"/>`) + `</circle>`).join("").replace(/^/, `<g class="fumes">`) + `</g>` : "");
}
// (ticket 471) what a treasure chest holds, open (seaChestSvg)
const CHEST_TREASURE = [["gold", "🪙 gold coins"], ["gems", "💎 gems"], ["crown", "👑 gold, gems and a crown"]];
// (ticket 504, for M; Matthew: "More animals as placeable objects with animation: * cows (4 poses) * pigs (4 poses)", and
// "chickens -- already in stable 30 in stable.") The 🐄 farm animals, to put anywhere: a cow, a pig and a chicken, each in
// four poses, each pose its own drawing, seen from the side facing our right (↔️ mirror turns one to face our left), its
// feet at 0, 0 in the stage's units, as thingSvg places a thing. At 🔎 100% they are sized against the friend as most
// places draw them (0.45 of their own size: about 200 units tall): a cow as tall at the shoulder as the friend's chest
// (about 105), a pig to the knee and a bit (about 39 at its back; the knee is 31), a chicken shin-high (about 28, comb
// and all). Each moves with the music's LFOs as its pose would (a tail swishing, an ear flicking, a jaw chewing, a belly
// breathing, a peck, a wingbeat), each animation begun its own way in from the animal's seed, so a field of them never
// moves in step. The chicken is the stable's hens (their outline, their brown and their white, a little smaller), in four
// poses and four coats; the stable's own two are left as they are.
const COW_POSES = [["stand", "🧍 standing"], ["graze", "🌱 grazing"], ["lie", "💤 lying down"], ["moo", "📢 mooing"]];
const COW_COATS = [["holstein", "🐄 black and white"], ["brown", "🟤 brown"], ["redwhite", "🔴 red and white"], ["highland", "🧡 Highland, ginger and shaggy"]];
const PIG_POSES = [["stand", "🧍 standing"], ["root", "👃 rooting"], ["side", "😴 lying on its side"], ["sit", "🪑 sitting"]];
const PIG_COATS = [["pink", "🐷 pink"], ["spots", "⚫ pink with black spots"], ["band", "🖤 black with a pink band"], ["ginger", "🧡 ginger"]];
const HEN_POSES = [["stand", "🧍 standing"], ["peck", "🌾 pecking"], ["nest", "🥚 sitting on a nest"], ["flap", "💨 flapping"]];
const HEN_COATS = [["white", "🤍 white"], ["brown", "🤎 brown"], ["black", "🖤 black"], ["speckled", "🍪 speckled"]];
// (each coat's colours: its hide; its patches, spots or band, if it has them; its muzzle or snout; a hen's wing; the
// Highland cow's long hair, its fringe over its eyes and its long horns)
const COW_COAT = { holstein: { hide: "#f4f1ea", patch: "#1f1f22", muzzle: "#eab6ad" }, brown: { hide: "#8b5a2b", muzzle: "#e3cfae" },
                   redwhite: { hide: "#f4f1ea", patch: "#9a3a1c", muzzle: "#efc0b4" }, highland: { hide: "#c4682b", muzzle: "#7a4424", shaggy: true } };
const PIG_COAT = { pink: { hide: "#f4a7b0", snout: "#ee8e9b" }, spots: { hide: "#f4b0b6", snout: "#ee8e9b", spot: "#2b2527" },
                   band: { hide: "#2e282a", snout: "#4a3f42", band: "#f2b6bd" }, ginger: { hide: "#c9682e", snout: "#dc9064" } };
// (the stable's hens are the white and the brown, in their colours)
const HEN_COAT = { white: { body: "#f5efe2", line: "#c9bca2", wing: "#ebe2cf" }, brown: { body: "#a0522d", line: "#6b3418", wing: "#8c4626" },
                   black: { body: "#2a272e", line: "#0e0d10", wing: "#34463f" }, speckled: { body: "#6e3518", line: "#3f1a08", wing: "#5e2c12", dots: true } };
// (the pig and the chicken are drawn a little big, in units that keep their shapes easy to read: each shrunk to its size here)
const PIG_SCALE = 0.85, HEN_SCALE = 0.85;
const farmP = (x, y) => `${f1(x)} ${f1(y)}`;
// (an animal's animation, timed by the scene's LFO of that pace, `off` of a round on; with no LFO, its own round of `dur`
// seconds, begun as far in: each animal, and each moving part of it, its own way in)
const farmMove = (o, pace, off, dur, anim) => lfoTimed(o, pace, off, anim.replace(/\/>$/, ` dur="${dur}s" begin="-${(off * dur).toFixed(2)}s" repeatCount="indefinite"/>`));
const farmTurn = (angles, x, y) => angles.map(a => `${a} ${x} ${y}`).join(";");
// (a move made n times in a round: its values, from and back to the first)
const farmTimes = (n, seq) => { const v = [seq[0]]; for (let i = 0; i < n; i++) v.push(...seq.slice(1)); return v; };
// (a patch or a spot: a closed smooth path round (cx, cy), its edge uneven by R from k on)
function farmBlob(cx, cy, rx, ry, R, k, n = 7) {
  const p = [...Array(n)].map((_, i) => { const a = 2 * Math.PI * (i + R(k + 20)) / n, f = 0.75 + 0.45 * R(k + i); return [cx + Math.cos(a) * rx * f, cy + Math.sin(a) * ry * f]; });
  const m = i => { const a = p[i % n], b = p[(i + 1) % n]; return farmP((a[0] + b[0]) / 2, (a[1] + b[1]) / 2); };
  let d = `M${m(0)}`;
  for (let i = 1; i <= n; i++) d += ` Q${farmP(p[i % n][0], p[i % n][1])} ${m(i)}`;
  return d + " Z";
}
// (a cow's or a pig's leg, from its top (x, top) down to the ground g, w wide: a foreleg straight down, a hind leg slanting
// back to its hock and down again; its hoof or trotter at the foot, hh high, cloven)
function hoofLeg(x, top, g, w, hind, col, sw, hoof, hh) {
  const k = top + (g - hh - top) * 0.55, fx = hind ? x - w * 0.1 : x, kx = hind ? x - w * 0.55 : x;
  const pts = [[x - w * 0.55, top], [x + w * 0.55, top], [kx + w * 0.45, k], [fx + w * 0.38, g - hh], [fx - w * 0.38, g - hh], [kx - w * 0.45, k]];
  return `<polygon points="${pts.map(([a, b]) => `${f1(a)},${f1(b)}`).join(" ")}" fill="${col}" stroke="${mix(col, "#000000", 0.5)}" stroke-width="${sw}" stroke-linejoin="round"/>` +
    `<path d="M${farmP(fx - w * 0.4, g - hh)} L${farmP(fx + w * 0.42, g - hh)} L${farmP(fx + w * 0.55, g)} L${farmP(fx - w * 0.45, g)} Z M${farmP(fx + w * 0.08, g - hh * 0.7)} L${farmP(fx + w * 0.12, g)}" ` +
    `fill="${hoof}" stroke="${mix(hoof, "#000000", 0.4)}" stroke-width="${f1(sw * 0.6)}" stroke-linejoin="round"/>`;
}
// THE COW. Standing, grazing and mooing it stands the same, square on its legs (its udder between the hind ones), and only
// its neck and head go another way: standing, its head held out, an ear flicking now and then; grazing, its head down in
// the grass, chewing; mooing, its head stretched up, its mouth opening for each moo and its moo going out. Lying, it rests
// on the ground, its legs folded under it, its belly breathing, chewing its cud. Its tail swishes when it stands.
const COW_HEADS = {
  stand: { neck: "M36 -105 C52 -110 70 -118 84 -116 L92 -76 C82 -64 70 -58 56 -58 Z", neckPatch: [62, -96, 10, 8],
           head: "M82 -117 C90 -120 98 -114 104 -104 L112 -94 C120 -90 123 -82 121 -74 C119 -68 113 -66 106 -67 L96 -70 C88 -72 82 -78 79 -88 C77 -98 76 -110 82 -117 Z",
           muzzle: "M110 -95 C119 -92 124 -82 121 -74 C119 -68 113 -66 106 -67 C103 -73 103 -88 110 -95 Z", nose: [117, -85], mouth: "M106 -71 Q112 -69 118 -71",
           eye: [93, -104], patch: [90, -105, 10, 9], ear: "M86 -108 Q74 -116 64 -110 Q72 -101 86 -103 Z", earAt: [86, -106],
           horn: "M84 -117 Q82 -127 88 -131 Q89 -123 91 -118 Z", horns: ["M82 -116 C72 -122 62 -124 58 -138 C56 -124 66 -112 80 -110 Z", "M86 -118 C94 -126 106 -130 108 -144 C112 -130 102 -118 90 -112 Z"],
           fringe: "M78 -120 C86 -126 100 -122 106 -108 L104 -98 L100 -104 L97 -94 L94 -103 L90 -92 L87 -102 L83 -94 L81 -104 L77 -98 C76 -106 75 -114 78 -120 Z" },
  graze: { neck: "M34 -105 C58 -106 78 -90 90 -66 L82 -36 C74 -48 66 -58 54 -62 Z", neckPatch: [62, -84, 9, 9],
           head: "M86 -68 C94 -70 100 -64 102 -56 L107 -32 C112 -26 113 -14 111 -6 C109 -1 102 0 96 -1 L92 -2 C88 -8 86 -16 84 -26 C82 -36 78 -48 80 -58 C81 -64 83 -67 86 -68 Z",
           muzzle: "M104 -30 C111 -26 113 -14 111 -6 C109 -1 102 0 96 -1 C94 -8 96 -20 98 -26 C99 -28 101 -30 104 -30 Z", nose: [107, -8], mouth: "M92 -6 Q95 -3 99 -2",
           eye: [95, -52], patch: [92, -54, 9, 10], ear: "M86 -60 Q74 -66 66 -58 Q74 -52 86 -55 Z", earAt: [86, -58],
           horn: "M88 -68 Q88 -78 94 -81 Q93 -74 94 -69 Z", horns: ["M84 -66 C74 -70 66 -72 62 -86 C60 -72 68 -62 82 -60 Z", "M92 -68 C100 -76 108 -78 112 -92 C114 -78 104 -66 96 -62 Z"],
           fringe: "M82 -70 C90 -76 102 -70 104 -58 L103 -46 L99 -52 L97 -40 L94 -50 L91 -38 L89 -49 L85 -40 L84 -52 L80 -46 C79 -54 78 -64 82 -70 Z" },
  lie:   { neck: "M34 -68 C48 -80 64 -94 80 -98 L92 -66 C80 -56 66 -46 56 -38 Z", neckPatch: [58, -72, 9, 8],
           head: "M80 -101 C88 -104 96 -100 102 -92 L112 -84 C119 -80 122 -73 120 -67 C118 -62 112 -60 106 -61 L96 -64 C88 -66 82 -72 79 -80 C77 -88 76 -96 80 -101 Z",
           muzzle: "M110 -86 C118 -82 122 -74 120 -67 C118 -62 112 -60 106 -61 C103 -67 104 -80 110 -86 Z", nose: [116, -77], mouth: "M105 -65 Q111 -63 117 -65",
           eye: [92, -90], patch: [90, -90, 10, 8], ear: "M84 -92 Q72 -98 64 -92 Q72 -85 84 -88 Z", earAt: [84, -90],
           horn: "M81 -100 Q79 -110 85 -114 Q86 -106 88 -101 Z", horns: ["M79 -100 C69 -106 59 -108 55 -122 C53 -108 63 -96 77 -94 Z", "M85 -102 C93 -110 105 -114 107 -128 C111 -114 101 -102 89 -96 Z"],
           fringe: "M76 -104 C84 -110 98 -106 104 -94 L102 -82 L98 -88 L95 -78 L92 -87 L88 -76 L85 -86 L81 -78 L79 -88 L75 -82 C74 -90 73 -98 76 -104 Z" },
  moo:   { neck: "M36 -105 C54 -116 72 -130 88 -134 L102 -110 C88 -98 72 -82 58 -62 Z", neckPatch: [66, -104, 10, 8],
           head: "M86 -138 C90 -145 98 -147 104 -146 L122 -151 C130 -153 134 -146 132 -138 L110 -130 C104 -128 100 -124 96 -120 C90 -122 84 -130 86 -138 Z",
           jaw: "M96 -124 L126 -126 C130 -123 129 -118 124 -117 L102 -114 C97 -114 94 -119 96 -124 Z", inside: "M108 -130 L132 -138 L128 -122 L100 -120 Z", hinge: [98, -122], tongue: [116, -124.5],
           muzzle: "M120 -151 C129 -153 134 -146 132 -138 L122 -134 C119 -139 117 -146 120 -151 Z", nose: [128, -145],
           eye: [100, -138], patch: [98, -138, 9, 8], ear: "M90 -132 Q78 -134 70 -126 Q80 -122 90 -127 Z", earAt: [90, -130],
           horn: "M88 -140 Q85 -148 90 -153 Q92 -146 94 -142 Z", horns: ["M86 -138 C76 -144 66 -146 62 -160 C60 -146 70 -134 84 -132 Z", "M92 -142 C100 -150 110 -154 112 -168 C116 -154 106 -142 96 -136 Z"],
           fringe: "M84 -142 C92 -150 106 -148 110 -138 L108 -126 L104 -132 L101 -122 L98 -131 L94 -120 L91 -130 L87 -122 L86 -132 L82 -126 C81 -132 81 -138 84 -142 Z" } };
function cowThingSvg(t, seed, o) {
  const pose = COW_HEADS[t.pose] ? t.pose : "stand", C = COW_COAT[t.coat] || COW_COAT.holstein, R = seeded(seed), H = COW_HEADS[pose], lying = pose === "lie";
  const hide = C.hide, ln = mix(hide, "#000000", 0.5), far = mix(hide, "#000000", 0.2), hoof = "#3b302a", horn = "#ebe2c8", tuft = C.patch || mix(hide, "#000000", 0.35);
  const paint = (d, col = hide, w = 2) => `<path d="${d}" fill="${col}" stroke="${mix(col, "#000000", 0.5)}" stroke-width="${w}" stroke-linejoin="round"/>`;
  const bodyD = lying ? "M40 -66 C8 -73 -42 -73 -68 -68 C-88 -64 -97 -36 -90 -14 C-86 -3 -74 0 -60 0 L32 0 C48 0 60 -10 61 -26 C62 -46 56 -62 40 -66 Z"
    : "M40 -104 C10 -110 -40 -110 -66 -106 L-80 -102 C-90 -96 -91 -74 -84 -62 C-79 -55 -66 -54 -56 -56 Q-12 -46 30 -55 C46 -58 58 -66 60 -80 C61 -92 54 -102 40 -104 Z";
  // (black and white, red and white: patches on its body and round an eye, each ending at its outline, clipped to it;
  // where they lie, by its seed)
  const bodyId = `cow-body-${lying ? "lie" : "up"}-${seed}`, headId = `cow-head-${pose}-${seed}`;
  const spots = lying ? [[-60, -54, 18, 12], [-22, -42, 20, 13], [14, -58, 14, 9], [-84, -30, 11, 11], [40, -30, 12, 11]]
    : [[-58, -94, 18, 12], [-22, -80, 20, 13], [12, -98, 14, 9], [-80, -72, 12, 11], [42, -74, 12, 13]];
  const patches = !C.patch ? "" : `<defs><clipPath id="${bodyId}"><path d="${bodyD}"/></clipPath></defs><g clip-path="url(#${bodyId})" fill="${C.patch}">` +
    spots.filter((_, i) => i < 3 || R(i + 10) < 0.8).map(([x, y, rx, ry], i) => `<path d="${farmBlob(x + (R(i) - 0.5) * 12, y + (R(i + 5) - 0.5) * 8, rx, ry, R, 30 + 10 * i)}"/>`).join("") + `</g>`;
  // (the Highland's long hair: hanging in a fringe under its belly when it stands, and lying in locks along its sides)
  const hair = !C.shaggy ? "" : (lying ? "" : (() => {
      const zig = [[-84, -62], [-80, -50], [-74, -57], [-68, -47], [-62, -55], [-54, -45], [-46, -52], [-36, -42], [-26, -50], [-16, -41], [-6, -48], [4, -41], [14, -49], [24, -43], [32, -52], [40, -47], [46, -56], [52, -52], [57, -62]];
      const z = zig.map(([x, y]) => farmP(x, y)).join(" L");
      return `<path d="M${z} L57 -68 Q-12 -60 -84 -68 Z" fill="${hide}"/><path d="M${z}" fill="none" stroke="${ln}" stroke-width="1.6" stroke-linejoin="round"/>`; })()) +
    `<path d="${lying ? "M-62 -60 q7 6 2 14 M-34 -64 q7 6 2 14 M-4 -64 q7 6 2 14 M22 -58 q7 6 2 14 M-50 -40 q7 6 2 14 M-18 -40 q7 6 2 14 M14 -38 q7 6 2 14"
      : "M-62 -98 q7 6 2 14 M-34 -101 q7 6 2 14 M-4 -100 q7 6 2 14 M22 -96 q7 6 2 14 M-48 -80 q7 6 2 14 M-16 -78 q7 6 2 14 M12 -76 q7 6 2 14"}" fill="none" stroke="${mix(hide, "#000000", 0.28)}" stroke-width="1.6" stroke-linecap="round"/>`;
  let s = `<ellipse class="flat" cx="-12" cy="1" rx="${lying ? 104 : 96}" ry="9" fill="#000000" opacity="0.18"/>`;
  if (!lying) {
    // (its far legs, a shade darker, a little behind the near; its udder; its near legs; its body over their tops)
    s += hoofLeg(30, -66, -4, 15, false, far, 2, hoof, 8) + hoofLeg(-54, -70, -4, 18, true, far, 2, hoof, 8) +
      `<path d="M-50 -55 Q-49 -44 -41 -43 Q-33 -44 -32 -54 Z" fill="${C.muzzle}" stroke="${mix(C.muzzle, "#000000", 0.4)}" stroke-width="1.6"/>` +
      `<path d="M-46 -44 L-46.5 -40 M-37 -44 L-36.5 -40" stroke="${mix(C.muzzle, "#000000", 0.2)}" stroke-width="2.4" stroke-linecap="round"/>` +
      hoofLeg(44, -66, 0, 15, false, hide, 2, hoof, 8) + hoofLeg(-66, -72, 0, 18, true, hide, 2, hoof, 8) + paint(bodyD) + patches + hair +
      `<g>${farmMove(o, "slow", R(1), 8, `<animateTransform attributeName="transform" type="rotate" values="${farmTurn(farmTimes(4, [0, 9, -4, 0]), -82, -100)}"/>`)}` +
      `<path d="M-82 -100 Q-93 -84 -90 -60" fill="none" stroke="${ln}" stroke-width="3.6" stroke-linecap="round"/><path d="M-82 -100 Q-93 -84 -90 -60" fill="none" stroke="${hide}" stroke-width="1.6" stroke-linecap="round"/>` +
      `<path d="M-90 -62 Q-97 -53 -91 -45 Q-85 -52 -88 -62 Z" fill="${tuft}" stroke="${mix(tuft, "#000000", 0.4)}" stroke-width="1"/></g>`;
  } else {
    // (lying: its body breathing, rising and falling three times a round of the slow LFO; its hind leg folded along its side,
    // the hoof forward; its foreleg tucked under its chest, the knee forward; its tail along the ground)
    s += `<g>${farmMove(o, "slow", R(1), 9, `<animateTransform attributeName="transform" type="scale" values="${farmTimes(3, ["1 1", "1 1.025", "1 1"]).join(";")}"/>`)}` +
      paint(bodyD) + patches + hair + `<path d="M-74 -54 C-56 -56 -40 -42 -40 -22 C-40 -12 -46 -6 -54 -4" fill="none" stroke="${ln}" stroke-width="2" stroke-linecap="round"/></g>` +
      paint("M-86 -1 L-86 -11 L-50 -9 L-48 -1 Z") + `<path d="M-50 -9 L-40 -8 L-38 -1 L-49 -1 Z" fill="${hoof}" stroke="#1f1814" stroke-width="1.2" stroke-linejoin="round"/>` +
      paint("M30 -28 C42 -26 54 -20 60 -12 C63 -6 60 -1 54 -1 L36 -1 C34 -4 34 -8 38 -9 L50 -9 C46 -16 38 -20 28 -20 Z") + `<path d="M38 -9 L30 -8 L28 -1 L36 -1 Z" fill="${hoof}" stroke="#1f1814" stroke-width="1.2" stroke-linejoin="round"/>` +
      `<path d="M-86 -60 C-98 -50 -100 -20 -94 -6 C-90 2 -80 2 -74 -1" fill="none" stroke="${ln}" stroke-width="3.6" stroke-linecap="round"/><path d="M-86 -60 C-98 -50 -100 -20 -94 -6 C-90 2 -80 2 -74 -1" fill="none" stroke="${hide}" stroke-width="1.6" stroke-linecap="round"/>` +
      `<ellipse cx="-71" cy="-1.5" rx="5" ry="2.6" fill="${tuft}"/>`;
  }
  // (its neck and head, as the pose holds them; a Highland's long horns, the far one behind its head, the near in front)
  if (C.shaggy) s += paint(H.horns[0], mix(horn, "#000000", 0.12), 1.6);
  s += paint(H.neck);
  if (pose === "moo") s += `<g>${farmMove(o, "slow", R(2), 8, `<animateTransform attributeName="transform" type="rotate" values="${farmTurn([-24, -24, 0, 0, -24, -24, -24, 0, 0, -24, -24], ...H.hinge)}" keyTimes="0;0.1;0.16;0.36;0.42;0.5;0.6;0.66;0.86;0.92;1"/>`)}` +
    `<path d="${H.inside}" fill="#4a1f22"/><ellipse cx="${H.tongue[0]}" cy="${H.tongue[1]}" rx="6" ry="2" fill="#d9737a"/>${paint(H.jaw)}</g>`;
  s += paint(H.head) + (C.patch ? `<defs><clipPath id="${headId}"><path d="${H.neck}"/><path d="${H.head}"/></clipPath></defs><g clip-path="url(#${headId})" fill="${C.patch}"><path d="${farmBlob(...H.patch, R, 90)}"/>` +
    (R(98) < 0.7 ? `<path d="${farmBlob(...H.neckPatch, R, 100)}"/>` : "") + `</g>` : "");
  const [nx, ny] = H.nose, muzzle = paint(H.muzzle, C.muzzle, 1.6) + `<ellipse cx="${nx}" cy="${ny}" rx="1.6" ry="1.3" fill="#3a2020"/>` +
    (H.mouth ? `<path d="${H.mouth}" fill="none" stroke="${mix(C.muzzle, "#000000", 0.45)}" stroke-width="1.4" stroke-linecap="round"/>` : "");
  // (grazing and lying, it chews: its muzzle working with the quick LFO, once a round)
  s += pose === "graze" || lying ? `<g>${farmMove(o, "fast", R(3), 1, `<animateTransform attributeName="transform" type="translate" values="0 0;0.8 1.4;0 0"/>`)}${muzzle}</g>` : muzzle;
  if (pose === "graze") s += grassTuft(99, 1, 12, seed + 5, "#5a9a3a", 1.8);   // (the grass it is eating, round its muzzle)
  const [ex, ey] = H.eye;
  s += lying ? `<ellipse cx="${ex}" cy="${ey}" rx="2.2" ry="1.4" fill="#1f1410"/><path d="M${ex - 2.8} ${ey - 1} Q${ex} ${ey - 2.8} ${ex + 2.8} ${ey - 1}" fill="none" stroke="${ln}" stroke-width="1.3"/>`
    : `<ellipse cx="${ex}" cy="${ey}" rx="2.2" ry="2.6" fill="#1f1410"/><circle cx="${ex + 0.8}" cy="${ey - 1}" r="0.8" fill="#ffffff"/>`;
  // (its ear; standing, flicking twice a round of the slow LFO; a Highland's long fringe over its eyes, and over its ear)
  const ear = paint(H.ear, hide, 1.6) + `<path d="${H.ear}" fill="${C.muzzle}" opacity="0.45" transform="translate(${f1(H.earAt[0] * 0.12)} ${f1(H.earAt[1] * 0.12)}) scale(0.88)"/>`;
  s += pose === "stand" ? `<g>${farmMove(o, "slow", R(4), 8, `<animateTransform attributeName="transform" type="rotate" values="${farmTurn([0, 0, -14, 5, 0, 0, -14, 5, 0, 0], ...H.earAt)}" keyTimes="0;0.2;0.23;0.26;0.29;0.7;0.73;0.76;0.79;1"/>`)}${ear}</g>` : ear;
  if (C.shaggy) s += paint(H.fringe, hide, 1.4) + `<path d="${H.fringe}" fill="none" stroke="${mix(hide, "#000000", 0.25)}" stroke-width="1" transform="translate(${f1(H.eye[0] * 0.2)} ${f1(H.eye[1] * 0.2)}) scale(0.8)"/>`;
  s += C.shaggy ? paint(H.horns[1], horn, 1.6) : paint(H.horn, horn, 1.4);
  // (mooing: its moo going out from its mouth as its jaw opens, the same way in as the jaw)
  if (pose === "moo") s += `<path class="fumes" d="M138 -146 Q143 -135 138 -124 M145 -151 Q152 -135 145 -119 M152 -156 Q161 -135 152 -114" fill="none" stroke="#5a5a5a" stroke-width="2" stroke-linecap="round" opacity="0.8">` +
    farmMove(o, "slow", R(2), 8, `<animate attributeName="opacity" values="0;0;0.8;0.8;0;0;0;0.8;0.8;0;0" keyTimes="0;0.1;0.16;0.36;0.42;0.5;0.6;0.66;0.86;0.92;1"/>`) + `</path>`;
  return s;
}
// THE PIG. Standing, its curly tail wiggling with the quick LFO and its ear flopping with the slow; rooting, its snout in
// the earth, nudging at it; lying on its side, asleep, its belly toward us and its legs out, breathing; sitting on its
// haunches, its head turning one way and the other. Spots and a band end at its outline, clipped to it.
const PIG_POSE = {
  stand: { body: "M26 -40 C6 -47 -24 -47 -38 -40 C-48 -34 -49 -18 -40 -12 C-28 -7 0 -7 20 -10 C32 -12 38 -21 36 -30 C35 -36 31 -39 26 -40 Z",
           legs: [[16, -18, -3, 7, 0], [-26, -18, -3, 8, 1], [24, -18, 0, 7.5, 0], [-34, -18, 0, 8.5, 1]],
           head: "M22 -42 C30 -48 42 -45 48 -36 L54 -29 C56 -26 56 -19 54 -17 C48 -13 40 -12 32 -14 C24 -16 18 -24 22 -42 Z",
           snout: [55, -23, 3.4, 6.4, 0], eye: [41, -32], cheek: [45, -22], mouth: "M44 -17 Q48 -15 51 -17",
           farEar: "M28 -42 Q32 -53 41 -50 Q37 -45 34 -41 Z", ear: "M31 -43 Q42 -51 50 -42 Q44 -38 38 -35 Q34 -38 31 -43 Z", earAt: [32, -43],
           tail: "M-46 -32 C-52 -33 -55 -38 -51 -41 C-47 -44 -43 -40 -46 -37 C-48 -35 -51 -37 -49 -39", tailAt: [-45, -32],
           spots: [[-26, -34, 7, 5], [-6, -24, 8, 6], [12, -36, 5, 4], [-38, -20, 5, 5]], band: "M2 -52 L22 -52 L28 2 L6 2 Z" },
  root:  { body: "M24 -36 C8 -45 -22 -50 -38 -44 C-49 -38 -50 -22 -41 -15 C-28 -9 0 -8 18 -8 C30 -9 35 -18 33 -26 C32 -31 28 -34 24 -36 Z",
           legs: [[18, -16, -3, 7, 0], [-26, -20, -3, 8, 1], [26, -16, 0, 7.5, 0], [-34, -20, 0, 8.5, 1]],
           head: "M20 -38 C30 -43 40 -38 44 -30 L51 -14 C54 -8 54 -4 50 -2 C44 0 36 -2 32 -6 C26 -12 20 -22 20 -38 Z",
           snout: [51, -4.5, 3, 5.6, 60], eye: [36, -28], cheek: [41, -16], mouth: "M38 -5 Q41 -3 44 -3",
           farEar: "M24 -38 Q28 -48 36 -46 Q32 -41 29 -37 Z", ear: "M27 -39 Q40 -43 45 -30 Q38 -28 33 -28 Q29 -32 27 -39 Z", earAt: [28, -39],
           tail: "M-47 -40 C-53 -41 -56 -46 -52 -49 C-48 -52 -44 -48 -47 -45 C-49 -43 -52 -45 -50 -47", tailAt: [-46, -40],
           spots: [[-26, -38, 7, 5], [-6, -26, 8, 6], [12, -32, 5, 4], [-40, -26, 5, 5]], band: "M0 -54 L20 -54 L28 2 L6 2 Z", turn: [22, -32] },
  side:  { body: "M-44 -6 C-52 -18 -40 -34 -16 -35 C6 -36 24 -32 31 -22 C35 -15 33 -8 27 -6 L-36 -4 C-40 -4 -43 -5 -44 -6 Z",
           head: "M24 -28 C33 -33 45 -30 51 -22 L56 -17 C58 -14 58 -8 56 -6 C50 -3 38 -3 30 -6 C22 -10 19 -20 24 -28 Z",
           snout: [56.5, -11.5, 2.8, 5, 0], shut: "M38 -21 Q40.5 -19 43 -21", cheek: [44, -12], mouth: "M45 -8 Q48 -6.5 51 -8",
           ear: "M30 -29 Q41 -36 48 -26 Q41 -24 35 -22 Q31 -25 30 -29 Z", earAt: [31, -28],
           tail: "M-44 -16 C-50 -17 -53 -22 -49 -25 C-45 -28 -41 -24 -44 -21 C-46 -19 -49 -21 -47 -23", tailAt: [-43, -16],
           spots: [[-24, -26, 7, 5], [-4, -22, 8, 5], [14, -28, 5, 4]], band: "M0 -40 L18 -40 L20 4 L2 4 Z" },
  sit:   { body: "M-26 0 C-38 -2 -40 -16 -34 -26 C-26 -38 -10 -46 4 -46 C14 -46 20 -40 20 -30 C20 -20 16 -10 12 -4 L-12 0 Z",
           legs: [[4, -24, -3, 7, 0]],
           head: "M2 -58 C9 -68 23 -69 30 -61 L37 -53 C39 -50 39 -43 37 -41 C32 -37 24 -37 17 -39 C8 -42 0 -49 2 -58 Z",
           snout: [38, -47, 3, 6, 0], eye: [25, -56], cheek: [28, -45], mouth: "M29 -41 Q33 -39 35 -41",
           farEar: "M8 -62 Q13 -72 21 -69 Q17 -64 14 -61 Z", ear: "M13 -62 Q25 -71 32 -60 Q27 -57 21 -54 Q16 -57 13 -62 Z", earAt: [14, -62],
           tail: "M-34 -3 C-40 -3 -43 -8 -39 -11 C-35 -14 -31 -10 -34 -7 C-36 -5 -39 -7 -37 -9", tailAt: [-33, -3],
           spots: [[-22, -26, 7, 5], [-2, -38, 7, 5], [8, -20, 5, 4]], band: "M-10 -54 L10 -54 L20 2 L-2 2 Z", turn: [10, -46] } };
function pigThingSvg(t, seed, o) {
  const pose = PIG_POSE[t.pose] ? t.pose : "stand", C = PIG_COAT[t.coat] || PIG_COAT.pink, R = seeded(seed), Q = PIG_POSE[pose];
  const hide = C.hide, ln = mix(hide, "#000000", 0.45), trot = C.band ? "#1c1718" : mix(hide, "#3a2020", 0.6), band = C.band || hide;
  const paint = (d, col = hide, w = 1.6) => `<path d="${d}" fill="${col}" stroke="${mix(col, "#000000", 0.45)}" stroke-width="${w}" stroke-linejoin="round"/>`;
  const clipId = `pig-body-${pose}-${seed}`;
  const coat = C.spot || C.band ? `<defs><clipPath id="${clipId}"><path d="${Q.body}"/></clipPath></defs><g clip-path="url(#${clipId})">` +
    (C.band ? `<path d="${Q.band}" fill="${C.band}"/>` : Q.spots.map(([x, y, rx, ry], i) => `<path d="${farmBlob(x + (R(i) - 0.5) * 6, y + (R(i + 5) - 0.5) * 4, rx, ry, R, 30 + 10 * i)}" fill="${C.spot}"/>`).join("")) + `</g>` : "";
  // (its curly tail, with a dark edge; wiggling, or not)
  const tailOf = (wig, off) => { const [tx, ty] = Q.tailAt, tl = `<path d="${Q.tail}" fill="none" stroke="${ln}" stroke-width="3.4" stroke-linecap="round"/><path d="${Q.tail}" fill="none" stroke="${hide}" stroke-width="1.7" stroke-linecap="round"/>`;
    return wig ? `<g>${farmMove(o, "fast", off, 1, `<animateTransform attributeName="transform" type="rotate" values="${farmTurn([0, 18, -8, 0], tx, ty)}"/>`)}${tl}</g>` : tl; };
  // (a leg: [x, its top, the ground, its width, a hind one]; the near pair in its coat, the far a shade darker; the band's
  // forelegs in the band's colour)
  const legOf = ([x, top, g, w, hind]) => { const col = !hind && C.band ? band : hide; return hoofLeg(x, top, g, w, !!hind, g < 0 ? mix(col, "#000000", 0.18) : col, 1.4, trot, 3); };
  const [sx, sy, srx, sry, sa] = Q.snout;
  const ear = paint(Q.ear, hide, 1.3) + `<path d="${Q.ear}" fill="${C.snout}" opacity="0.4" transform="translate(${f1(Q.earAt[0] * 0.15)} ${f1(Q.earAt[1] * 0.15)}) scale(0.85)"/>`;
  const head = (Q.farEar ? paint(Q.farEar, mix(hide, "#000000", 0.18), 1.2) : "") + paint(Q.head) +
    `<ellipse cx="${Q.cheek[0]}" cy="${Q.cheek[1]}" rx="3.2" ry="2" fill="#e8607a" opacity="${C.band ? 0 : 0.3}"/>` +
    `<g transform="rotate(${sa} ${sx} ${sy})"><ellipse cx="${sx}" cy="${sy}" rx="${srx}" ry="${sry}" fill="${C.snout}" stroke="${mix(C.snout, "#000000", 0.45)}" stroke-width="1.2"/>` +
    `<ellipse cx="${f1(sx + 0.4)}" cy="${f1(sy - sry * 0.4)}" rx="0.9" ry="1.3" fill="#3a2226"/><ellipse cx="${f1(sx + 0.4)}" cy="${f1(sy + sry * 0.4)}" rx="0.9" ry="1.3" fill="#3a2226"/></g>` +
    `<path d="${Q.mouth}" fill="none" stroke="${ln}" stroke-width="1.2" stroke-linecap="round"/>` +
    (Q.shut ? `<path d="${Q.shut}" fill="none" stroke="#2a1a1c" stroke-width="1.3" stroke-linecap="round"/>`
      : `<circle cx="${Q.eye[0]}" cy="${Q.eye[1]}" r="1.7" fill="#1f1416"/><circle cx="${Q.eye[0] + 0.6}" cy="${Q.eye[1] - 0.6}" r="0.6" fill="#ffffff"/>`);
  let s = `<ellipse class="flat" cx="0" cy="1" rx="${pose === "side" ? 54 : pose === "sit" ? 36 : 50}" ry="5" fill="#000000" opacity="0.18"/>`;
  if (pose === "stand") s += Q.legs.map(legOf).join("") + paint(Q.body) + coat + tailOf(true, R(1)) + head +
    `<g>${farmMove(o, "slow", R(2), 8, `<animateTransform attributeName="transform" type="rotate" values="${farmTurn([0, 0, 10, -3, 0, 0, 10, -3, 0, 0], ...Q.earAt)}" keyTimes="0;0.25;0.28;0.31;0.34;0.65;0.68;0.71;0.74;1"/>`)}${ear}</g>`;
  else if (pose === "root") s += `<g class="flat"><ellipse cx="51" cy="1" rx="11" ry="2.4" fill="#5a3e22" opacity="0.5"/>` +
      [[58, -1, 2], [61, 1, 1.6], [46, 1.5, 1.4], [55, 2, 1.2]].map(([x, y, r]) => `<ellipse cx="${x}" cy="${y}" rx="${r}" ry="${f1(r * 0.7)}" fill="#6b4a2a"/>`).join("") + `</g>` +
    Q.legs.map(legOf).join("") + paint(Q.body) + coat + tailOf(true, R(1)) +
    `<g>${farmMove(o, "fast", R(2), 1, `<animateTransform attributeName="transform" type="rotate" values="${farmTurn([0, -7, 2, 0], ...Q.turn)}"/>`)}${head + ear}</g>`;
  else if (pose === "side") {
    // (on its side, its belly toward us: its four legs out toward us from the belly, the upper pair a shade darker)
    // (its legs from the belly's edge, the top of each left open so it grows out of the belly; the upper pair behind)
    const stub = (x, col) => `<path d="M${f1(x - 3.5)} -8 L${f1(x - 1)} -1 L${f1(x + 6)} -1 L${f1(x + 3.5)} -8" fill="${col}" stroke="${mix(col, "#000000", 0.45)}" stroke-width="1.3" stroke-linejoin="round"/>` +
      `<path d="M${f1(x - 1)} -1 L${f1(x + 6)} -1 L${f1(x + 6.6)} 3 L${f1(x - 1.6)} 3 Z" fill="${trot}" stroke="${mix(trot, "#000000", 0.4)}" stroke-width="0.9" stroke-linejoin="round"/>`;
    s += `<g>${farmMove(o, "slow", R(1), 9, `<animateTransform attributeName="transform" type="scale" values="${farmTimes(3, ["1 1", "1.01 1.05", "1 1"]).join(";")}"/>`)}` +
      paint(Q.body) + coat + `<path d="M-36 -6 Q-6 -15 26 -8 Q-4 -4 -36 -6 Z" fill="${mix(hide, "#ffffff", 0.22)}"/>` +
      stub(-20, mix(hide, "#000000", 0.18)) + stub(18, mix(band, "#000000", 0.18)) + stub(-27, hide) + stub(11, band) + `</g>` +
      tailOf(false) + head + `<g>${farmMove(o, "slow", R(2), 8, `<animateTransform attributeName="transform" type="rotate" values="${farmTurn([0, 0, -8, 3, 0, 0, -8, 3, 0, 0], ...Q.earAt)}" keyTimes="0;0.3;0.33;0.36;0.39;0.75;0.78;0.81;0.84;1"/>`)}${ear}</g>`;
  } else {
    // (sitting: its far foreleg, its body, its near hind leg folded at its side with the trotter forward, its near foreleg)
    const fore = mix(band, "#000000", 0.45);
    s += tailOf(true, R(1)) + legOf(Q.legs[0]) + paint(Q.body) + coat +
      paint("M-14 -6 L0 -5 L1 -1 L-14 -1 Z") + `<path d="M0 -5 L5 -4.5 L6 0 L0.5 0 Z" fill="${trot}" stroke="${mix(trot, "#000000", 0.4)}" stroke-width="0.9" stroke-linejoin="round"/>` +
      paint("M-34 -12 C-34 -24 -20 -28 -12 -20 C-6 -14 -6 -4 -12 -1 L-28 -1 C-33 -3 -34 -7 -34 -12 Z") +
      `<path d="M10.5 -18 L11 -3 L17 -3 L17.5 -18" fill="${band}" stroke="${fore}" stroke-width="1.4" stroke-linejoin="round"/>` +
      `<path d="M10.8 -3 L17.2 -3 L17.8 0 L10.2 0 Z" fill="${trot}" stroke="${mix(trot, "#000000", 0.4)}" stroke-width="0.9" stroke-linejoin="round"/>` +
      `<g>${farmMove(o, "slow", R(2), 8, `<animateTransform attributeName="transform" type="rotate" values="${farmTurn([0, 0, -7, -7, 0, 0, 5, 5, 0, 0], ...Q.turn)}" keyTimes="0;0.15;0.2;0.38;0.43;0.58;0.63;0.82;0.87;1"/>`)}${head + ear}</g>`;
  }
  return `<g transform="scale(${PIG_SCALE})">${s}</g>`;
}
// THE CHICKEN, the stable's hens: standing, its head jerking with the quick LFO, blinking now and then; pecking, its body
// tipped forward and its head down at the grain, up and down again once a round of the quick LFO; sitting fluffed up on
// a nest of straw, an egg peeping out, breathing and turning its head; flapping, its wings beating with the wings' LFO
// (F, a quarter of a beat to start with) and its beak open.
const HEN_HEAD = (x, y, r, C, open) => `<circle cx="${x}" cy="${y}" r="${r}" fill="${C.body}" stroke="${C.line}" stroke-width="0.8"/>` +
  `<path d="M${f1(x - 2)} ${f1(y - r)} q2 -4 4 0 q2 -4 3 1 Z" fill="#d62828"/>` +
  (open ? `<path d="M${f1(x + r - 0.3)} ${f1(y - 1)} l4 0.6 l-4 1 Z M${f1(x + r - 0.3)} ${f1(y + 1)} l3.4 1.6 l-3.6 -0.2 Z" fill="#e8a33a"/>` : `<path d="M${f1(x + r - 1)} ${y} l4 1 l-4 1 Z" fill="#e8a33a"/>`) +
  `<path d="M${f1(x + 1)} ${f1(y + 3)} q1 3 -1 4" fill="#d62828"/>`;
function chickenThingSvg(t, seed, o) {
  const pose = HEN_POSES.some(([p]) => p === t.pose) ? t.pose : "peck", C = HEN_COAT[t.coat] || HEN_COAT.brown, R = seeded(seed);
  const paint = (d, col = C.body, w = 1) => `<path d="${d}" fill="${col}" stroke="${C.line}" stroke-width="${w}" stroke-linejoin="round"/>`;
  const line = d => `<path d="${d}" fill="none" stroke="${C.line}" stroke-width="0.8" stroke-linecap="round"/>`;
  const legs = (d, toes) => `<path d="${d}" fill="none" stroke="#e8a33a" stroke-width="1.6" stroke-linecap="round"/><path d="${toes}" fill="none" stroke="#e8a33a" stroke-width="1.1" stroke-linecap="round" stroke-linejoin="round"/>`;
  // (speckled: dots of white and of black over its body, inside its outline)
  const dots = (cx, cy, rx, ry) => !C.dots ? "" : [...Array(14)].map((_, i) => { const a = 2 * Math.PI * R(i + 50), u = Math.sqrt(R(i + 70));
    return `<circle cx="${f1(cx + Math.cos(a) * rx * u)}" cy="${f1(cy + Math.sin(a) * ry * u)}" r="0.95" fill="${i % 4 ? "#f5efe2" : "#1f1a16"}"/>`; }).join("");
  const eye = (x, y, blink) => `<ellipse cx="${x}" cy="${y}" rx="0.9" ry="0.9" fill="#111111">${blink ? farmMove(o, "slow", R(3), 8, `<animate attributeName="ry" values="0.9;0.9;0.1;0.9;0.9;0.1;0.9;0.9" keyTimes="0;0.4;0.42;0.44;0.8;0.82;0.84;1"/>`) : ""}</ellipse>`;
  let s = `<ellipse class="flat" cx="0" cy="0.5" rx="${pose === "nest" ? 20 : 14}" ry="2.5" fill="#000000" opacity="0.18"/>`;
  if (pose === "stand") s += legs("M-3 -8 L-4 0 M3 -8 L4 0", "M-7 0.6 L-4 0 L-0.5 0.8 M0.5 0.6 L4 0 L7.5 0.8") +
    paint("M-16 -18 Q-20 -30 -12 -28 Q-6 -14 6 -14 Q14 -14 14 -22 L10 -8 Q0 -2 -10 -8 Z") + line("M-15 -20 Q-18 -26 -14 -27.5 M-13 -19 Q-15 -24 -12 -26") +
    paint("M-9 -17 Q-2 -19 5 -15 Q2 -10 -4 -10 Q-9 -12 -9 -17 Z", C.wing, 0.8) + line("M-6 -13 Q-1 -12 3 -13.5") + dots(-2, -11, 8, 3.5) +
    `<g>${farmMove(o, "fast", R(1), 1, `<animateTransform attributeName="transform" type="translate" values="0 0;0 0;1.6 -0.4;1.6 -0.4;0 0;0 0" keyTimes="0;0.4;0.45;0.85;0.9;1"/>`)}` +
    HEN_HEAD(14, -24, 5, C) + eye(15.5, -25, true) + `</g>`;
  else if (pose === "peck") s += `<g class="flat">${[[20, 0.6], [23.5, -0.4], [18, 1.4], [25, 1]].map(([x, y]) => `<ellipse cx="${x}" cy="${y}" rx="1" ry="0.6" fill="#d9b25a"/>`).join("")}</g>` +
    legs("M-4 -6 L-5 0 M2 -6 L3 0", "M-8 0.6 L-5 0 L-1.5 0.8 M-0.5 0.6 L3 0 L6.5 0.8") +
    paint("M-14 -20 Q-20 -34 -10 -32 Q-4 -18 6 -15 Q11 -14 11 -11 L8 -6 Q-2 -1 -10 -6 Z") + line("M-13 -22 Q-17 -29 -11 -31 M-11 -21 Q-13 -27 -9 -29") +
    paint("M-9 -16 Q-2 -17 4 -12 Q1 -7 -5 -7 Q-10 -10 -9 -16 Z", C.wing, 0.8) + line("M-6 -11 Q-1 -10 2 -11") + dots(-3, -12, 7, 4) +
    // (its head down at the grain; once a round it comes up and goes down again)
    `<g>${farmMove(o, "fast", R(1), 1, `<animateTransform attributeName="transform" type="rotate" values="${farmTurn([0, -34, -34, 0, 0], 8, -12)}" keyTimes="0;0.2;0.5;0.68;1"/>`)}` +
    paint("M4 -15 Q12 -16 16 -9 L12 -5 Q9 -9 5 -9 Z", C.body, 0.8) + `<circle cx="15" cy="-5.5" r="4.4" fill="${C.body}" stroke="${C.line}" stroke-width="0.8"/>` +
    `<path d="M11.5 -9.5 q2 -3.5 4 -0.5 q2 -3 3.5 1.2 Z" fill="#d62828"/><path d="M17.5 -2.4 l2.2 3.2 l-3.8 -1.2 Z" fill="#e8a33a"/><path d="M14 -1.5 q-1 2.5 -3 2" fill="#d62828"/>` + eye(16.6, -6.2) + `</g>`;
  else if (pose === "nest") s += `<ellipse cx="0" cy="-6" rx="19" ry="6" fill="#9a7840" stroke="#6b5228" stroke-width="1"/><ellipse cx="0" cy="-7" rx="15" ry="3.4" fill="#5e4724"/>` +
    // (fluffed up on the nest, breathing three times a round of the slow LFO, its head turning to look about)
    `<g>${farmMove(o, "slow", R(1), 9, `<animateTransform attributeName="transform" type="scale" values="${farmTimes(3, ["1 1", "1.02 1.05", "1 1"]).join(";")}"/>`)}` +
    paint("M-16 -3 C-20 -12 -20 -22 -14 -27 C-10 -24 -6 -23 -2 -24 C6 -26 14 -22 15 -15 C16 -10 14 -5 12 -3 Z") + line("M-15 -12 Q-18 -20 -14 -25") +
    line("M10 -22 q2.2 1.2 1.4 3.4 q2.2 1.2 0.8 3.4 q2 1.4 0.2 3.2") + paint("M-13 -15 Q-5 -21 6 -17 Q2 -10 -6 -9 Q-12 -10 -13 -15 Z", C.wing, 0.8) +
    line("M-9 -12 q2 1.6 4 0 q2 1.6 4 0 q2 1.6 3 -0.6") + dots(-2, -15, 10, 5) +
    `<g>${farmMove(o, "slow", R(2), 8, `<animateTransform attributeName="transform" type="rotate" values="${farmTurn([0, 0, -12, -12, 0, 0], 10, -24)}" keyTimes="0;0.3;0.36;0.6;0.66;1"/>`)}` +
    HEN_HEAD(11, -27, 4.5, C) + eye(12.6, -28.2) + `</g></g>` +
    `<ellipse cx="14.5" cy="-6.5" rx="2.6" ry="3.3" fill="#f3ead8" stroke="#c9bca2" stroke-width="0.6"/>` +
    `<path d="M-19 -6 C-19 1 -10 2 0 2 C10 2 19 1 19 -6 C12 -5 -12 -5 -19 -6 Z" fill="#b8904c" stroke="#6b5228" stroke-width="1"/>` +
    `<path d="M-16 -3 l6 1.5 M-8 -1 l7 0.5 M2 -1 l6 -0.5 M10 -2.5 l6 -1.5 M-13 0 l5 -1.5 M5 0.5 l6 -1" stroke="#e0c070" stroke-width="0.9" stroke-linecap="round"/>`;
  else {
    // (flapping: both wings up, each beating down and back up with the wings' LFO, together)
    const flap = (inner, x, y) => `<g>${farmMove(o, "wings", R(1), 0.3, `<animateTransform attributeName="transform" type="rotate" values="${farmTurn([0, -55, 0], x, y)}"/>`)}${inner}</g>`;
    s += flap(paint("M3 -18 Q-2 -32 -12 -41 Q-11 -37 -15 -35 Q-11 -32 -15 -29 Q-10 -27 -12 -23 Q-4 -21 3 -16 Z", mix(C.wing, "#000000", 0.2), 0.8), 3, -18) +
      legs("M-3 -6 L-4 0 M3 -6 L5 0", "M-7 0.6 L-4 0 L-0.5 0.8 M1.5 0.6 L5 0 L8.5 0.8") +
      paint("M-15 -12 Q-20 -24 -12 -24 Q-6 -17 3 -20 Q8 -23 11 -30 L17 -28 Q16 -18 12 -11 Q4 -5 -8 -6 Q-13 -8 -15 -12 Z") + line("M-14 -14 Q-17 -21 -12.5 -23") + dots(-1, -12, 8, 3.5) +
      HEN_HEAD(15, -31, 4.6, C, true) + eye(16.4, -32) +
      flap(paint("M0 -17 Q-4 -30 -12 -38 Q-11 -34 -15 -32 Q-11 -29 -15 -26 Q-10 -24 -12 -20 Q-5 -18 -3 -13 Q-1 -14 0 -17 Z", C.wing, 0.8) + line("M-2.5 -18 L-11 -33 M-3.5 -16 L-12.5 -26"), 0, -17);
  }
  return `<g transform="scale(${HEN_SCALE})">${s}</g>`;
}
// (ticket 501, for M: "dessert -- sand dunes in distance-- placeable objects are 6 types of dessert plants (cactus,etc)";
// Matthew passing on M's ideas, the desert he means) The 🌵 desert plants: things to put anywhere, and the 🏜️ desert's own
// (drawn by the same code, so one standing there and one put somewhere else look alike). Each stands at 0, 0 in its own
// units, as thingSvg places it, lit from the right (the desert's sun); its spines a few tens of short strokes in one path,
// never hundreds of shapes, and what repeats (arms, blossoms, leaves of one width) one path each (60 frames a second first).
const DESERT_FLOWERS = ["#fde047", "#fb923c", "#f472b6", "#ef4444", "#ffffff", "#c084fc"];
const SAGUARO_ARMS = [[0, "🌱 none yet (a young one)"], [1, "1"], [2, "2"], [3, "3"], [4, "4"]];
const PEAR_BLOOMS = [["none", "🚫 nothing"], ["flowers", "🌼 flowers"], ["fruit", "🍒 fruit"]];
const AGAVE_TONES = ["#7fa8a0", "#6f9a5a", "#9ab8c8", "#8f9c62"];
// (a plant's shadow on the ground: flat, so its collision box is the plant's)
const plantShade = rx => `<ellipse class="flat" cx="0" cy="2" rx="${rx}" ry="${f1(rx * 0.2)}" fill="#000000" opacity="0.2"/>`;
// (blossoms at the points pts, r across: five round petals each, all in one path, their middles in another)
function bloomsSvg(pts, r, col, mid = mix(col, "#7c2d12", 0.5)) {
  if (!pts.length) return "";
  const at = (x, y, rad, deg) => `${f1(x + rad * Math.cos(deg * Math.PI / 180))} ${f1(y + rad * Math.sin(deg * Math.PI / 180))}`;
  let d = "", m = "";
  for (const [x, y] of pts) {
    d += `M${at(x, y, r * 0.35, -126)}`;
    for (let i = 0; i < 5; i++) { const a = -90 + 72 * i; d += ` Q${at(x, y, r * 1.1, a - 30)} ${at(x, y, r, a)} Q${at(x, y, r * 1.1, a + 30)} ${at(x, y, r * 0.35, a + 36)}`; }
    const q = r * 0.32;
    d += " Z "; m += `M${f1(x - q)} ${f1(y)} a${f1(q)} ${f1(q)} 0 1 0 ${f1(2 * q)} 0 a${f1(q)} ${f1(q)} 0 1 0 ${f1(-2 * q)} 0 `;
  }
  return `<path d="${d}" fill="${col}" stroke="${mix(col, "#000000", 0.3)}" stroke-width="${f1(r * 0.12)}"/><path d="${m}" fill="${mid}"/>`;
}
// 🌵 a saguaro: a tall ribbed column, rounded on top, and its arms (none on a young one): the first two low on either side,
// reaching out a little and up; the next two higher and farther out, nearer us (drawn over the low ones); spines in rows
// down its ribs; a woodpecker's hole now and then; its white flowers on the tops (flowers)
function saguaroSvg(t, seed) {
  const R = seeded(seed), arms = Math.max(0, Math.min(4, Math.round(t.arms ?? 2))), H = 350 + 50 * R(1), r = 22, top = -H + r;
  const green = "#4f8a46", dark = "#2b5a2e", lit = "#86bf6e", first = R(2) < 0.5 ? -1 : 1, P = (x, y) => `${f1(x)} ${f1(y)}`;
  const lowY = -H * (0.34 + 0.06 * R(3)), lowOut = 26 + 10 * R(4);
  // (the low arms, then the high ones: each group one outline, one fill, its lit side and its ribs, the high ones over the low)
  const arm = [0, 1].map(() => ({ d: "", rib: "", lit: "", sp: "" }));
  let spines = "";
  const tops = [[-10, top - r + 7], [0, top - r + 1], [10, top - r + 7]];
  for (let j = 0; j < arms; j++) {
    const sd = j % 2 ? -first : first, high = j >= 2, ya = high ? -H * (0.56 + 0.06 * R(5 + j)) : lowY - 12 * R(5 + j);
    const ax = sd * (r + (high ? lowOut + 36 + 8 * R(9 + j) : lowOut + 6 * R(9 + j))), yt = Math.max(top + 50, ya - (high ? 50 + 40 * R(13 + j) : 70 + 70 * R(13 + j)));
    const A = arm[high ? 1 : 0];
    A.d += `M${P(sd * 6, ya)} L${P(ax - sd * 14, ya)} Q${P(ax, ya)} ${P(ax, ya - 14)} L${P(ax, yt)} `;
    A.rib += `M${P(ax, ya - 16)} L${P(ax, yt - 6)} `; A.lit += `M${P(ax + 6, ya - 12)} L${P(ax + 6, yt - 4)} `;
    for (let y = ya - 30; y > yt + 4; y -= 30) A.sp += `M${P(ax - 2.5, y + 1.5)} L${P(ax, y)} L${P(ax + 2.5, y + 1.5)} `;
    tops.push([ax, yt - 12]);
  }
  // (the arms, behind the column)
  let s = plantShade(40) + arm.filter(A => A.d).map(A => `<path d="${A.d}" fill="none" stroke="${dark}" stroke-width="31" stroke-linecap="round" stroke-linejoin="round"/>` +
    `<path d="${A.d}" fill="none" stroke="${green}" stroke-width="27" stroke-linecap="round" stroke-linejoin="round"/>` +
    `<path d="${A.lit}" fill="none" stroke="${lit}" stroke-width="4" stroke-linecap="round" opacity="0.5"/><path d="${A.rib}" fill="none" stroke="${dark}" stroke-width="1.3" opacity="0.45"/>` +
    (A.sp ? `<path d="${A.sp}" fill="none" stroke="#f1e6c2" stroke-width="1.1" stroke-linecap="round"/>` : "")).join("");
  // the column, its shaded and its lit side, its ribs (each ending inside its rounded top), the spines down them
  // (ticket 501: and its spine, unseen, as wide as the column, so the whole column is in the friend's way, whatever its arms;
  // found by its tall shapes, a young one's, with no arms, was its stripes and ribs alone, short of its lit side by 10)
  s += `<path d="M${-r} 0 L${-r} ${f1(top)} A${r} ${r} 0 0 1 ${r} ${f1(top)} L${r} 0 Z" fill="${green}" stroke="${dark}" stroke-width="2.5"/>` +
       `<path data-spine="" d="M0 0 V${f1(top)}" fill="none" stroke="transparent" stroke-width="${2 * r}" pointer-events="none"/>` +
       `<path d="M-15 -2 L-15 ${f1(top)}" stroke="${dark}" stroke-width="8" opacity="0.18"/><path d="M10 -2 L10 ${f1(top - 8)}" stroke="${lit}" stroke-width="6" stroke-linecap="round" opacity="0.5"/>`;
  let ribs = "";
  for (const x of [-12, -4, 4, 12]) {
    const end = top - Math.sqrt(r * r - x * x) + 4;
    ribs += `M${P(x, -1)} L${P(x, end)} `;
    if (x !== 4) for (let y = -16; y > end + 6; y -= 30) spines += `M${P(x - 2.5, y + 1.5)} L${P(x, y)} L${P(x + 2.5, y + 1.5)} `;
  }
  s += `<path d="${ribs}" fill="none" stroke="${dark}" stroke-width="1.3" opacity="0.4"/><path d="${spines}" fill="none" stroke="#f1e6c2" stroke-width="1.1" stroke-linecap="round"/>`;
  if (R(7) < 0.55) { const hx = (R(8) < 0.5 ? -1 : 1) * 7, hy = -H * 0.72;   // (a woodpecker's hole, a small surprise)
    s += `<ellipse cx="${f1(hx)}" cy="${f1(hy)}" rx="5" ry="7" fill="#6b8f50"/><ellipse cx="${f1(hx)}" cy="${f1(hy + 0.5)}" rx="3.6" ry="5.4" fill="#2a1a10"/>`; }
  if (t.flowers ?? true) s += bloomsSvg(tops, 7, "#fffaf0", "#facc15");
  return drawnBy("saguaroSvg", s);
}
// 🍐 a prickly pear: flat round pads, two on the ground and more growing from their tops (how many by its seed), each with
// a few spines; on its top edges flowers in their colour, or its red fruit, or nothing (bloom)
function pricklyPearSvg(t, seed) {
  const R = seeded(seed), bloom = t.bloom ?? "flowers", col = t.color || DESERT_FLOWERS[0];
  const green = "#6aa84f", dark = "#2f6b2a", lit = "#a3d17f", P = (x, y) => `${f1(x)} ${f1(y)}`;
  // its pads: [x, y, half its width, half its length, its turn]
  const base = [[-24, -40, 24, 40, -14], [26, -34, 22, 36, 16], [-46, -104, 20, 32, -28], [-6, -112, 22, 36, 4], [44, -92, 19, 31, 26]];
  if (R(1) < 0.7) base.push([-24, -168, 18, 28, -14]);
  if (R(2) < 0.6) base.push([56, -148, 16, 26, 32]);
  if (R(3) < 0.4) base.push([-72, -152, 15, 24, -40]);
  const pads = base.map(([x, y, rx, ry, a], i) => [x + 8 * (R(10 + i) - 0.5), y + 6 * (R(20 + i) - 0.5), rx, ry, a + 14 * (R(30 + i) - 0.5)]);
  // (a point on a pad, from across it (u) and along it (v), each -1 to 1; and whether a point is inside one)
  const on = ([x, y, rx, ry, a], u, v) => { const c = Math.cos(a * Math.PI / 180), sn = Math.sin(a * Math.PI / 180); return [x + u * rx * c - v * ry * sn, y + u * rx * sn + v * ry * c]; };
  const inside = ([x, y, rx, ry, a], px, py) => { const c = Math.cos(a * Math.PI / 180), sn = Math.sin(a * Math.PI / 180), dx = px - x, dy = py - y, lu = (dx * c + dy * sn) / rx, lv = (-dx * sn + dy * c) / ry; return lu * lu + lv * lv < 1; };
  // (the highest drawn first, so each pad hides the foot of the one growing from it)
  const order = pads.map((p, i) => i).sort((i, j) => pads[i][1] - pads[j][1]);
  let s = plantShade(64), spines = "";
  for (const i of order) {
    const p = pads[i], [x, y, rx, ry, a] = p, [hx, hy] = on(p, 0.28, -0.12);
    s += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${rx}" ry="${ry}" transform="rotate(${f1(a)} ${f1(x)} ${f1(y)})" fill="${green}" stroke="${dark}" stroke-width="2"/>` +
         `<ellipse cx="${f1(hx)}" cy="${f1(hy)}" rx="${f1(rx * 0.45)}" ry="${f1(ry * 0.62)}" transform="rotate(${f1(a)} ${f1(hx)} ${f1(hy)})" fill="${lit}" opacity="0.35"/>`;
    for (const [u, v] of [[-0.45, -0.45], [0.4, -0.52], [0, -0.05], [-0.42, 0.42], [0.45, 0.38]]) { const [px, py] = on(p, u, v);
      spines += `M${P(px - 2.2, py - 1.6)} L${P(px, py)} L${P(px + 2.2, py - 1.6)} `; }
  }
  s += `<path d="${spines}" fill="none" stroke="#f3ecd0" stroke-width="1.1" stroke-linecap="round"/>`;
  // what grows on its top edges: on the upper pads, where no other pad covers the spot
  const spots = pads.filter(p => p[1] < -80).flatMap(p => [on(p, -0.38, -0.9), on(p, 0.32, -0.94)]).filter(([px, py]) => pads.filter(q => inside(q, px, py)).length <= 1);
  if (bloom === "flowers") s += bloomsSvg(spots, 8, col);
  else if (bloom === "fruit") s += spots.map(([px, py]) => `<ellipse cx="${f1(px)}" cy="${f1(py - 4)}" rx="5.5" ry="7.5" fill="#c0265f" stroke="#7a1238" stroke-width="1.2"/>` +
    `<circle cx="${f1(px + 1.8)}" cy="${f1(py - 7)}" r="1.6" fill="#f9a8d4"/>`).join("");
  return drawnBy("pricklyPearSvg", s);
}
// 🛢️ a barrel cactus: round and squat, its ribs running up to its crown, hooked red spines along them; a crown of flowers in
// their colour (flowers), or of spines
function barrelCactusSvg(t, seed) {
  const R = seeded(seed), w = 34 + 8 * R(1), h = 76 + 18 * R(2), col = t.color || DESERT_FLOWERS[1];
  const green = "#5b8f45", dark = "#2c5a28", lit = "#8cc070", P = (x, y) => `${f1(x)} ${f1(y)}`;
  const left = `M${P(-w * 0.9, 0)} C${P(-w * 1.15, -h * 0.35)} ${P(-w * 1.05, -h * 0.92)} ${P(0, -h)}`;
  let s = plantShade(46) + `<path d="${left} C${P(w * 1.05, -h * 0.92)} ${P(w * 1.15, -h * 0.35)} ${P(w * 0.9, 0)} Z" fill="${green}" stroke="${dark}" stroke-width="2.2"/>` +
    // (its shaded side and its lit side, inside its outline)
    `<path d="${left} Q${P(-w * 0.55, -h * 0.5)} ${P(-w * 0.35, 0)} Z" fill="${dark}" opacity="0.2"/>` +
    `<path d="M${P(w * 0.9, 0)} C${P(w * 1.15, -h * 0.35)} ${P(w * 1.05, -h * 0.92)} ${P(0, -h)} Q${P(w * 0.75, -h * 0.5)} ${P(w * 0.6, 0)} Z" fill="${lit}" opacity="0.3"/>`;
  let ribs = "", hooks = "";
  for (const u of [-0.75, -0.45, -0.15, 0.15, 0.45, 0.75]) {
    const a = [u * w * 0.82, -1], c = [u * w * 1.1, -h * 0.5], b = [u * w * 0.22, -h * 0.94], sd = u < 0 ? -1 : 1;
    ribs += `M${P(...a)} Q${P(...c)} ${P(...b)} `;
    for (const q of [0.28, 0.54, 0.78]) { const x = (1 - q) ** 2 * a[0] + 2 * (1 - q) * q * c[0] + q * q * b[0], y = (1 - q) ** 2 * a[1] + 2 * (1 - q) * q * c[1] + q * q * b[1];
      hooks += `M${P(x, y)} q${f1(sd * 3)} -6 ${f1(sd * 6)} -3 `; }
  }
  s += `<path d="${ribs}" fill="none" stroke="${dark}" stroke-width="1.5" opacity="0.5"/><path d="${hooks}" fill="none" stroke="#d9774a" stroke-width="1.4" stroke-linecap="round"/>`;
  if (t.flowers ?? true) s += bloomsSvg([-0.42, -0.21, 0, 0.21, 0.42].map((u, i) => [u * w, -h * 0.97 + Math.abs(u) * w * 0.3 - 3 * (i % 2)]), 7, col);
  else s += `<path d="${[-6, -3, 0, 3, 6].map(x => `M${P(x, -h + 1)} l${f1(x * 0.4)} -7`).join(" ")}" fill="none" stroke="#d9774a" stroke-width="1.4" stroke-linecap="round"/>`;
  return drawnBy("barrelCactusSvg", s);
}
// 🪴 an agave: a rosette of thick pointed leaves in its colour (tone), the outer ones bending down, a dark spine at each tip
// and teeth along the front ones; and, now and then, its tall flower stalk, branching at the top into yellow flowers (stalk)
function agaveSvg(t, seed) {
  const R = seeded(seed), tone = t.tone || AGAVE_TONES[0], dark = mix(tone, "#000000", 0.4), lit = mix(tone, "#ffffff", 0.35), P = (x, y) => `${f1(x)} ${f1(y)}`;
  let s = plantShade(70);
  if (t.stalk) {
    const top = -300 - 30 * R(1), lean = 10 * (R(2) - 0.5);
    let br = "", fl = "";
    for (let j = 0; j < 5; j++) { const y = top + 12 + j * 16, x = lean * (y / top), sd = j % 2 ? 1 : -1, ex = x + sd * (24 - j * 2), ey = y - 12;
      br += `M${P(x, y)} q${f1(sd * 14)} -2 ${f1(ex - x)} ${f1(ey - y)} `;
      for (const [dx, dy, q] of [[0, -4, 6], [sd * 6, 1, 4.5], [-sd * 5, 2, 4]]) fl += `M${P(ex + dx - q, ey + dy)} a${q} ${q} 0 1 0 ${2 * q} 0 a${q} ${q} 0 1 0 ${-2 * q} 0 `; }
    s += `<path d="M0 -30 Q${P(lean * 0.4, top * 0.5)} ${P(lean, top)}" fill="none" stroke="#7a6a32" stroke-width="7" stroke-linecap="round"/>` +
         `<path d="${br}" fill="none" stroke="#7a6a32" stroke-width="3" stroke-linecap="round"/><path d="${fl}" fill="#facc15" stroke="#ca8a04" stroke-width="1"/>`;
  }
  // its leaves, back to front: [angle (degrees, -90 straight up), length, half its width at the foot]
  const L = [[-100, 104, 12], [-80, 110, 12], [-122, 92, 12], [-58, 96, 12], [-145, 80, 13], [-35, 84, 13], [-165, 70, 13], [-15, 74, 13],
             [-112, 70, 14], [-68, 72, 14], [-140, 56, 15], [-40, 58, 15]];
  let ribs = "", teeth = "";
  L.forEach(([deg, len0, hw], i) => {
    const a = (deg + 8 * (R(10 + i) - 0.5)) * Math.PI / 180, len = len0 * (0.9 + 0.2 * R(20 + i)), ca = Math.cos(a), sa = Math.sin(a), nx = -sa, ny = ca;
    const droop = len * 0.22 * ca * ca, bx = ca * 8, by = -3, tx = ca * len, ty = Math.min(-2, sa * len + droop), mx = ca * len * 0.5, my = sa * len * 0.5 + droop * 0.25;
    const front = i >= 8, fill = front ? mix(tone, "#ffffff", 0.08) : i < 4 ? mix(tone, "#000000", 0.08) : tone;
    s += `<path d="M${P(bx - nx * hw, Math.min(0, by - ny * hw))} Q${P(mx - nx * hw, my - ny * hw)} ${P(tx, ty)} Q${P(mx + nx * hw, my + ny * hw)} ${P(bx + nx * hw, Math.min(0, by + ny * hw))} Z" fill="${fill}" stroke="${dark}" stroke-width="1.5" stroke-linejoin="round"/>`;
    ribs += `M${P(bx, by)} Q${P(mx, my)} ${P(bx + (tx - bx) * 0.82, by + (ty - by) * 0.82)} `;
    const ux = (tx - mx) / Math.hypot(tx - mx, ty - my), uy = (ty - my) / Math.hypot(tx - mx, ty - my);
    teeth += `M${P(tx, ty)} l${f1(ux * 7)} ${f1(uy * 7)} `;
    if (front) for (const q of [0.35, 0.6]) for (const e of [-1, 1]) { const ex = bx + (tx - bx) * q + e * nx * hw * (1 - q) * 0.9, ey = by + (ty - by) * q + e * ny * hw * (1 - q) * 0.9;
      teeth += `M${P(ex, ey)} l${f1(e * nx * 3 + ux * 2)} ${f1(e * ny * 3 + uy * 2)} `; }
  });
  s += `<path d="${ribs}" fill="none" stroke="${lit}" stroke-width="1.5" opacity="0.6"/><path d="${teeth}" fill="none" stroke="#3a2a1a" stroke-width="1.6" stroke-linecap="round"/>`;
  return drawnBy("agaveSvg", s);
}
// 🌴 a Joshua tree: a shaggy trunk branching twice, each branch bending its own way up, a spiky green tuft at each tip over
// a skirt of old dry leaves; in bloom, a creamy cluster at the tips (flowers)
function joshuaTreeSvg(t, seed) {
  const R = seeded(seed), P = (x, y) => `${f1(x)} ${f1(y)}`, widths = [24, 16, 11], segs = [], tips = [];
  let n = 0;
  const grow = (x, y, ang, len, d) => {
    const x2 = x + Math.cos(ang) * len, y2 = y + Math.sin(ang) * len, bend = len * 0.14 * (R(90 + n) - 0.5);
    segs.push([x, y, (x + x2) / 2 - Math.sin(ang) * bend, (y + y2) / 2 + Math.cos(ang) * bend, x2, y2, d]);
    if (d === 2) { tips.push([x2, y2, ang]); return; }
    const kids = d === 0 ? 2 + (R(1) < 0.5 ? 1 : 0) : (R(40 + n) < 0.3 ? 3 : 2);
    for (let j = 0; j < kids; j++) { n++;
      const spread = kids === 1 ? (R(50 + n) - 0.5) * 0.6 : (j / (kids - 1) - 0.5) * (1.3 + 0.4 * R(60 + n));
      grow(x2, y2, Math.max(-Math.PI + 0.45, Math.min(-0.45, ang + spread + (R(70 + n) - 0.5) * 0.3)), len * (0.62 + 0.18 * R(80 + n)), d + 1); }
  };
  grow(0, 0, -Math.PI / 2 + (R(2) - 0.5) * 0.2, 140 + 30 * R(3), 0);
  // (its wood: one outline and one fill for each thickness, the trunk's alone, so its trunk is found as a tree's is)
  const wood = d => segs.filter(q => q[6] === d).map(([x, y, cx, cy, x2, y2]) => `M${P(x, y)} Q${P(cx, cy)} ${P(x2, y2)}`).join(" ");
  let s = plantShade(46);
  for (const d of [0, 1, 2]) s += `<path d="${wood(d)}" fill="none" stroke="#3f3226" stroke-width="${widths[d] + 3}" stroke-linecap="round"/>`;
  for (const d of [0, 1, 2]) s += `<path d="${wood(d)}" fill="none" stroke="#7a6650" stroke-width="${widths[d]}" stroke-linecap="round"/>`;
  // its shag: short strokes down its wood
  let shag = "";
  for (const [x, y, cx, cy, x2, y2, d] of segs) for (let q = 0.15; q < 0.95; q += d ? 0.3 : 0.14) {
    const px = (1 - q) ** 2 * x + 2 * (1 - q) * q * cx + q * q * x2, py = (1 - q) ** 2 * y + 2 * (1 - q) * q * cy + q * q * y2, w = widths[d] * 0.35;
    shag += `M${P(px - w, py - w)} l${f1(w * 0.8)} ${f1(w * 1.3)} M${P(px + w * 0.3, py)} l${f1(w * 0.6)} ${f1(w * 1.2)} `; }
  s += `<path d="${shag}" fill="none" stroke="#5a4a3a" stroke-width="1.6" stroke-linecap="round"/>`;
  // the tufts at the tips, over their skirts of dry leaves
  let skirt = "", spikes = "", pale = "", bloom = "";
  tips.forEach(([x, y, ang], i) => {
    for (let j = 0; j < 7; j++) { const a = ang + Math.PI + (j / 6 - 0.5) * 1.3, L = 16 + 8 * R(100 + i * 10 + j); skirt += `M${P(x, y)} l${f1(Math.cos(a) * L)} ${f1(Math.sin(a) * L + 6)} `; }
    for (let j = 0; j < 14; j++) { const a = ang + (j / 13 - 0.5) * 4.6, L = 22 + 10 * R(200 + i * 15 + j); spikes += `M${P(x, y)} l${f1(Math.cos(a) * L)} ${f1(Math.sin(a) * L)} `; }
    for (let j = 0; j < 7; j++) { const a = ang + (j / 6 - 0.5) * 3, L = 16 + 8 * R(300 + i * 7 + j); pale += `M${P(x, y)} l${f1(Math.cos(a) * L)} ${f1(Math.sin(a) * L)} `; }
    if (t.flowers) { const fx = x + Math.cos(ang) * 10, fy = y + Math.sin(ang) * 10;
      for (const [dx, dy, q] of [[0, -3, 5.5], [-4, 3, 4.5], [4, 3, 4.5], [0, -10, 4]]) bloom += `M${P(fx + dx - q, fy + dy)} a${q} ${q} 0 1 0 ${2 * q} 0 a${q} ${q} 0 1 0 ${-2 * q} 0 `; }
  });
  s += `<path d="${skirt}" fill="none" stroke="#8a7350" stroke-width="2.4" stroke-linecap="round"/><path d="${spikes}" fill="none" stroke="#5f8436" stroke-width="3.2" stroke-linecap="round"/>` +
       `<path d="${pale}" fill="none" stroke="#a6bd5c" stroke-width="2.2" stroke-linecap="round"/>` + (bloom ? `<path d="${bloom}" fill="#f3ecd2" stroke="#c9bb8a" stroke-width="1"/>` : "");
  return drawnBy("joshuaTreeSvg", s);
}
// 🔥 an ocotillo: long thin whips fanning up out of one foot, each its own path (so it is found as a tree's trunk is),
// spined; leafed out after rain (leaves), and red flowers like flames at their tips (flowers)
function ocotilloSvg(t, seed) {
  const R = seeded(seed), n = 9 + Math.floor(3 * R(1)), P = (x, y) => `${f1(x)} ${f1(y)}`;
  const stems = [...Array(n)].map((_, i) => { const u = i / (n - 1) - 0.5, a = -Math.PI / 2 + u * 1.15 + (R(10 + i) - 0.5) * 0.18, len = 230 + 90 * R(20 + i) - 50 * Math.abs(u);
    const bx = u * 18 + (R(30 + i) - 0.5) * 4, bend = (R(40 + i) - 0.5) * 30;
    return { bx, cx: bx + Math.cos(a) * len * 0.5 - Math.sin(a) * bend, cy: Math.sin(a) * len * 0.5 + Math.cos(a) * bend, tx: bx + Math.cos(a) * len, ty: Math.sin(a) * len, i }; });
  const at = (S, q) => [(1 - q) ** 2 * S.bx + 2 * (1 - q) * q * S.cx + q * q * S.tx, 2 * (1 - q) * q * S.cy + q * q * S.ty];
  let s = plantShade(34), spines = "", leaves = "", flame = "", core = "";
  for (const S of stems) {
    s += `<path d="M${P(S.bx, 0)} Q${P(S.cx, S.cy)} ${P(S.tx, S.ty)}" fill="none" stroke="${S.i % 2 ? "#6e5d47" : "#7d6a50"}" stroke-width="4.2" stroke-linecap="round"/>`;
    const dx = S.tx - S.cx, dy = S.ty - S.cy, dl = Math.hypot(dx, dy), ux = dx / dl, uy = dy / dl, nx = -uy, ny = ux;
    for (const q of [0.25, 0.45, 0.65, 0.85]) { const [px, py] = at(S, q); spines += `M${P(px - nx * 4, py - ny * 4)} L${P(px + nx * 4, py + ny * 4)} `; }
    if (t.leaves ?? true) for (let j = 0; j < 5; j++) { const [px, py] = at(S, 0.2 + 0.14 * j), e = j % 2 ? 1 : -1;
      leaves += `M${P(px, py)} l${f1(e * nx * 6 + ux * 4)} ${f1(e * ny * 6 + uy * 4)} `; }
    if (t.flowers ?? true) { const [bx, by] = at(S, 0.96), ax = S.tx + ux * 18, ay = S.ty + uy * 18;
      flame += `M${P(bx + nx * 5, by + ny * 5)} Q${P(S.tx + nx * 7, S.ty + ny * 7)} ${P(ax, ay)} Q${P(S.tx - nx * 7, S.ty - ny * 7)} ${P(bx - nx * 5, by - ny * 5)} Z `;
      core += `M${P(S.tx + nx * 2.5, S.ty + ny * 2.5)} Q${P(S.tx + ux * 8 + nx * 3, S.ty + uy * 8 + ny * 3)} ${P(S.tx + ux * 13, S.ty + uy * 13)} Q${P(S.tx + ux * 8 - nx * 3, S.ty + uy * 8 - ny * 3)} ${P(S.tx - nx * 2.5, S.ty - ny * 2.5)} Z `; }
  }
  s += `<path d="${spines}" fill="none" stroke="#cbbfa8" stroke-width="1" stroke-linecap="round"/>` + (leaves ? `<path d="${leaves}" fill="none" stroke="#6aa121" stroke-width="3.6" stroke-linecap="round"/>` : "") +
       (flame ? `<path d="${flame}" fill="#e5402a" stroke="#9f1d10" stroke-width="0.8"/><path d="${core}" fill="#fb923c"/>` : "");
  return drawnBy("ocotilloSvg", s);
}
// (ticket 502, for M; Matthew: "farm yard -- placeable ojects are barn buildings (6 types) and fence parts (posts with lateral
// connections, 3 types)") The 🚜 farm things, for the farm yard and anywhere: a 🏚️ barn of six kinds and a 🪵 fence piece of
// three, each drawn standing at 0, 0 in its own units, as thingSvg places them (100% about the friend's height where they
// arrive: the eye there is 150 up, so a line below it bows down toward us and one above it up). The farm yard draws its own
// barn and fence with the same code, so they look the same there as put anywhere.
const BARN_STYLES = [["gambrel", "🟥 red barn with a hay loft"], ["round", "⭕ round barn"], ["stone", "🪨 stone barn"],
                     ["pole", "🌾 open pole barn, hay inside"], ["coop", "🐔 little chicken coop"], ["silo", "🌽 barn with a silo"]];
const BARN_COLORS = ["none", "#b03a2e", "#efe8da", "#5f80a6", "#6f8f4e", "#d9a93a", "#7a5236"];   // (its walls' colour: ∅ its kind's own)
const BARN_OWN = { gambrel: "#b03a2e", round: "#e6dcc2", stone: "#8a5a34", pole: "#8e9ca0", coop: "#e7d6a3", silo: "#b03a2e" };
const FARM_STRAW = "#e3c46a", FARM_STRAW_D = "#b8963e";
// (how far a level line at height y bows toward us, below the eye; away, above it)
const farmBow = y => (y + 150) * 0.04;
// (upright boards on a wall from x0 to x1, every `step`, from its top (topAt(x), on its outline) down to y1: one path)
const farmBoards = (x0, x1, topAt, y1, step, stroke, op = 0.35) => {
  let d = "";
  for (let x = x0 + step; x < x1 - 1; x += step) d += `M${f1(x)} ${f1(topAt(x))} V${f1(y1)} `;
  return `<path d="${d}" fill="none" stroke="${stroke}" stroke-width="1.5" opacity="${op}"/>`;
};
// (a barn door from x0 to x1, its top at y (to the ground): planks, a white frame and its X braces, two leaves)
const farmDoors = (x0, x1, y, fill, trim) => { const m = (x0 + x1) / 2;
  return `<rect x="${f1(x0)}" y="${f1(y)}" width="${f1(x1 - x0)}" height="${f1(-y)}" fill="${fill}"/>` +
    `<path d="M${f1(x0)} ${f1(y)} L${f1(m)} 0 M${f1(m)} ${f1(y)} L${f1(x0)} 0 M${f1(m)} ${f1(y)} L${f1(x1)} 0 M${f1(x1)} ${f1(y)} L${f1(m)} 0 M${f1(m)} ${f1(y)} V0" stroke="${trim}" stroke-width="4"/>` +
    `<rect x="${f1(x0)}" y="${f1(y)}" width="${f1(x1 - x0)}" height="${f1(-y)}" fill="none" stroke="${trim}" stroke-width="5"/>`; };
// (a window at (x, y), w by h: its glass, a white frame and its cross)
const farmWindow = (x, y, w, h, trim = "#f4efe4") => `<rect x="${f1(x - w / 2)}" y="${f1(y)}" width="${f1(w)}" height="${f1(h)}" fill="#3f5a6b" stroke="${trim}" stroke-width="3"/>` +
  `<path d="M${f1(x)} ${f1(y)} V${f1(y + h)} M${f1(x - w / 2)} ${f1(y + h / 2)} H${f1(x + w / 2)}" stroke="${trim}" stroke-width="2"/>`;
// (where a barn stands in the way: its foot, x0 to x1, d deep, for the collision box; ticket 441's data-footprint)
const farmFoot = (x0, x1, d = 44) => `<rect data-footprint="1" x="${x0}" y="${-d}" width="${x1 - x0}" height="${d + 4}" fill="none" stroke="none"/>`;
const farmShadow = (rx, cx = 0) => `<ellipse class="flat" cx="${cx}" cy="3" rx="${rx}" ry="${f1(rx * 0.11)}" fill="#000000" opacity="0.2"/>`;
// a rooster weather vane on a pole from (x, y) up, turning in the wind with the scene's quick LFO (its own way in: off)
function weatherVaneSvg(o, x, y, off) {
  const turn = sceneLfo(o, "fast", off, "transform", (v, amp) => `${(1 - (0.35 + 0.45 * amp) * (1 + v) / 2).toFixed(3)} 1`,
    `<animateTransform attributeName="transform" type="scale" values="1 1;0.45 1;1 1" dur="2.6s" repeatCount="indefinite"/>`, "scale");
  return `<path d="M${f1(x)} ${f1(y)} V${f1(y - 58)} M${f1(x - 12)} ${f1(y - 26)} H${f1(x + 12)}" stroke="#2b2b2b" stroke-width="2.5"/><circle cx="${f1(x)}" cy="${f1(y - 4)}" r="4" fill="#2b2b2b"/>` +
    `<g transform="translate(${f1(x)} ${f1(y - 40)})"><g>${turn}<path d="M-24 0 H18 M12 -4 L20 0 L12 4 M-24 0 l-6 -5 M-24 0 l-6 5" fill="none" stroke="#2b2b2b" stroke-width="2"/>` +
    `<path d="M-8 -3 Q-11 -12 -5 -14 Q-3 -22 3 -20 L8 -17 L4 -15 Q8 -8 4 -3 Z M-8 -5 Q-17 -12 -13 -21 Q-9 -13 -6 -9 Z" fill="#2b2b2b"/></g></g>`;
}
function barnThingSvg(t, seed, o) {
  const st = BARN_STYLES.some(([v]) => v === t.style) ? t.style : "gambrel", col = t.color && t.color !== "none" ? t.color : BARN_OWN[st];
  const dk = mix(col, "#000000", 0.32), lt = mix(col, "#ffffff", 0.3), trim = "#f4efe4", R = seeded(seed), P = (x, y) => `${f1(x)} ${f1(y)}`;
  if (st === "round") {
    // a round barn: a drum of boards under a cone of a roof, a cupola on top; shaded round (its own gradient)
    const id = `barn-round-${seed}`, top = x => -140 - farmBow(-140) * (1 - (x / 105) ** 2), bot = x => farmBow(0) * (1 - (x / 105) ** 2);
    let lines = "";
    for (let a = -75; a <= 75; a += 15) { const x = 105 * Math.sin(a * Math.PI / 180); lines += `M${P(x, top(x))} V${f1(bot(x))} `; }
    return `<defs><linearGradient id="${id}" x1="0" y1="0" x2="1" y2="0"><stop offset="0" stop-color="${dk}"/><stop offset="0.35" stop-color="${lt}"/><stop offset="0.65" stop-color="${col}"/><stop offset="1" stop-color="${dk}"/></linearGradient>` +
      `<linearGradient id="${id}-r" x1="0" y1="0" x2="1" y2="0"><stop offset="0" stop-color="#2f4a3a"/><stop offset="0.4" stop-color="#5f8a6c"/><stop offset="1" stop-color="#2f4a3a"/></linearGradient></defs>` +
      farmShadow(124) + farmFoot(-108, 108) +
      `<path d="M-105 -140 Q0 ${f1(-140 - 2 * farmBow(-140))} 105 -140 V0 Q0 ${f1(2 * farmBow(0))} -105 0 Z" fill="url(#${id})" stroke="${dk}" stroke-width="2"/>` +
      `<path d="${lines}" fill="none" stroke="${dk}" stroke-width="1.4" opacity="0.4"/>` +
      `<path d="M-26 ${f1(bot(-26))} V-62 Q-26 -88 0 -88 Q26 -88 26 -62 V${f1(bot(26))} Z" fill="${dk}" stroke="${trim}" stroke-width="4"/>` +
      `<path d="M-26 -62 L26 ${f1(bot(26))} M26 -62 L-26 ${f1(bot(-26))}" stroke="${trim}" stroke-width="3"/>` +
      farmWindow(-64, -116, 14, 20, trim) + farmWindow(64, -116, 14, 20, trim) +
      `<path d="M-118 -134 Q-96 -192 -22 -226 L22 -226 Q96 -192 118 -134 Q0 -128 -118 -134 Z" fill="url(#${id}-r)" stroke="#24382c" stroke-width="2"/>` +
      `<path d="M-118 -134 Q0 -128 118 -134" fill="none" stroke="${trim}" stroke-width="3"/>` +
      `<rect x="-17" y="-258" width="34" height="34" fill="${col}" stroke="${dk}" stroke-width="2"/><path d="M-11 -250 H11 M-11 -243 H11 M-11 -236 H11" stroke="${dk}" stroke-width="2.5"/>` +
      `<path d="M-24 -256 L0 -280 L24 -256 Z" fill="#2f4a3a" stroke="#24382c" stroke-width="2"/><line x1="0" y1="-280" x2="0" y2="-294" stroke="#2b2b2b" stroke-width="2.5"/>`;
  }
  if (st === "stone") {
    // a stone barn: its gable end of stones in courses (three tones, one path each), a slate roof's edge, an arched wooden door
    // (the door its colour), slits for windows, an owl hole up in the gable
    const id = `barn-stone-${seed}`, d = ["", "", ""], stone = (x, y, w, h) => `M${P(x + 3, y)} H${f1(x + w - 3)} Q${P(x + w, y)} ${P(x + w, y + 3)} V${f1(y + h - 3)} Q${P(x + w, y + h)} ${P(x + w - 3, y + h)} H${f1(x + 3)} Q${P(x, y + h)} ${P(x, y + h - 3)} V${f1(y + 3)} Q${P(x, y)} ${P(x + 3, y)} Z `;
    for (let row = 0; row < 10; row++) { const y = -20 * (row + 1); let x = -130 - 18 * R(row + 40), n = 0;
      while (x < 130) { const w = 26 + 20 * R(row * 17 + n); d[(row + n) % 3] += stone(x + 1, y + 1, w - 2, 18); x += w; n++; } }
    const plank = [-28, -19, -9.5, 0, 9.5, 19, 28].map(x => `M${P(x, 0)} V${f1(-62 - Math.sqrt(Math.max(0, 38 * 38 - x * x)) + 2)}`).join(" ");
    return `<defs><clipPath id="${id}"><path d="M-125 0 V-118 L0 -200 L125 -118 V0 Z"/></clipPath></defs>` + farmShadow(140) + farmFoot(-128, 128) +
      `<path d="M-125 0 V-118 L0 -200 L125 -118 V0 Z" fill="#6f6a62"/>` +
      `<g clip-path="url(#${id})">` + ["#a19c91", "#8c877d", "#b2ac9f"].map((c, j) => `<path d="${d[j]}" fill="${c}"/>`).join("") + `</g>` +
      `<path d="M-136 -112 L0 -210 L136 -112" fill="none" stroke="#3d4552" stroke-width="12" stroke-linejoin="round" stroke-linecap="round"/>` +
      `<path d="M-46 0 V-62 A46 46 0 0 1 46 -62 V0" fill="none" stroke="#c4bcab" stroke-width="10"/>` +
      `<path d="M-38 0 V-62 A38 38 0 0 1 38 -62 V0 Z" fill="${col}" stroke="${dk}" stroke-width="2"/><path d="${plank}" stroke="${dk}" stroke-width="1.5" opacity="0.7"/>` +
      `<path d="M-34 -24 H-6 M-34 -70 H-6 M6 -24 H34 M6 -70 H34" stroke="#2e2e36" stroke-width="5"/><circle cx="-6" cy="-44" r="3" fill="#2e2e36"/><circle cx="6" cy="-44" r="3" fill="#2e2e36"/>` +
      `<path d="M-86 -104 h8 v32 h-8 Z M78 -104 h8 v32 h-8 Z" fill="#2a2622"/><circle cx="0" cy="-156" r="10" fill="#2a2622" stroke="#c4bcab" stroke-width="3"/>`;
  }
  if (st === "pole") {
    // an open pole barn: a metal roof on four poles, open at the front, hay bales stacked inside (their twine, one path)
    const bales = [[-110, 0], [-56, 0], [-2, 0], [52, 0], [-83, 1], [-29, 1], [25, 1], [-56, 2], [-2, 2]];
    const top = x => -152 - 52 * (1 - Math.abs(x) / 142);
    let cor = ""; for (let x = -132; x <= 132; x += 10) cor += `M${P(x, top(x))} V-152 `;
    return farmShadow(150) + farmFoot(-132, 132, 50) +
      `<rect x="-126" y="-150" width="252" height="150" fill="#4a3a2a"/><path d="M-126 -150 L-110 -136 H110 L126 -150" fill="#3a2d20"/>` +
      `<path d="${bales.map(([x, r]) => `M${P(x, -26 * (r + 1))} h54 v26 h-54 Z`).join(" ")}" fill="${FARM_STRAW}" stroke="${FARM_STRAW_D}" stroke-width="1.5"/>` +
      `<path d="${bales.map(([x, r]) => `M${P(x + 16, -26 * (r + 1))} v26 M${P(x + 38, -26 * (r + 1))} v26`).join(" ")}" stroke="#8a6a32" stroke-width="1.6"/>` +
      `<path d="${[...Array(14)].map((_, j) => { const x = -120 + 240 * R(j + 60); return `M${P(x, -1)} l${f1(6 + 6 * R(j + 80))} ${f1(-2 - 3 * R(j + 90))}`; }).join(" ")}" stroke="${FARM_STRAW_D}" stroke-width="1.5"/>` +
      `<path d="${[-126, -44, 44, 126].map(x => `M${P(x - 5, 0)} V-150 h10 V0 Z`).join(" ")}" fill="#7a5634" stroke="#3e2a16" stroke-width="1.5"/>` +
      `<rect x="-140" y="-158" width="280" height="10" fill="#5e4026"/>` +
      `<path d="M-142 -152 L0 -204 L142 -152 Z" fill="${col}"/><path d="${cor}" fill="none" stroke="${dk}" stroke-width="1.5" opacity="0.5"/>` +
      `<path d="M-152 -146 L0 -210 L152 -146" fill="none" stroke="${dk}" stroke-width="7" stroke-linejoin="round" stroke-linecap="round"/>`;
  }
  if (st === "coop") {
    // a little chicken coop on legs: clapboards, a red roof, a round door with a ramp down, a nest box on its side, a window;
    // two of the stable's hens about it (henSvg), pecking with the quick LFO
    let cl = ""; for (let y = -104; y < -34; y += 10) cl += `M-48 ${y} H48 `;
    return farmShadow(78, -4) + farmFoot(-74, 60, 34) +
      `<path d="M-44 -34 V0 M40 -34 V0" stroke="#5e4026" stroke-width="7"/><path d="M-30 -34 V-4 M26 -34 V-4" stroke="#3e2a16" stroke-width="6"/>` +
      `<path d="${[...Array(10)].map((_, j) => `M${P(-50 + 100 * R(j + 30), 1)} l${f1(8 * R(j + 40))} ${f1(-3)}`).join(" ")}" stroke="${FARM_STRAW}" stroke-width="1.6"/>` +
      `<rect x="-72" y="-86" width="24" height="28" fill="${dk}" stroke="${mix(col, "#000000", 0.5)}" stroke-width="1.5"/><path d="M-76 -84 L-60 -94 L-46 -86" fill="none" stroke="#a8452e" stroke-width="5" stroke-linejoin="round"/>` +
      `<path d="M-48 -34 V-112 L0 -140 L48 -112 V-34 Z" fill="${col}" stroke="${dk}" stroke-width="2"/><path d="${cl}" stroke="${dk}" stroke-width="1" opacity="0.45"/>` +
      `<path d="M-58 -106 L0 -146 L58 -106" fill="none" stroke="#a8452e" stroke-width="9" stroke-linejoin="round" stroke-linecap="round"/>` +
      farmWindow(24, -100, 18, 16) +
      `<path d="M-12 -34 V-54 Q-12 -66 0 -66 Q12 -66 12 -54 V-34 Z" fill="#2b2016"/>` +
      `<path d="M-8 -34 L8 -34 L58 0 L42 0 Z" fill="#9a7048" stroke="#4e3420" stroke-width="1.5"/><path d="M6 -29 l8 0 M18 -21 l8 0 M30 -13 l8 0 M42 -5 l8 0" stroke="#4e3420" stroke-width="2.5"/>` +
      henSvg(o, -60, 2, 1.1, 1, 0, 0) + henSvg(o, 70, 3, 1.05, -1, 1, 2);
  }
  if (st === "silo") {
    // a barn with a tall silo beside it: the barn a plain gable, big doors, a loft window; the silo a cylinder of hoops (each
    // bowing as the eye sees it) under a metal dome, a ladder up its side
    const id = `barn-silo-${seed}`, peak = -172, topAt = x => Math.abs(x + 50) <= 80 ? -110 - 62 * (1 - Math.abs(x + 50) / 80) : -110;
    let hoops = ""; for (let y = -30; y > -300; y -= 30) hoops += `M36 ${y} Q72 ${f1(y + 2 * farmBow(y))} 108 ${y} `;
    let rungs = ""; for (let y = -20; y > -290; y -= 14) rungs += `M100 ${y} H107 `;
    return `<defs><linearGradient id="${id}" x1="0" y1="0" x2="1" y2="0"><stop offset="0" stop-color="#8e989c"/><stop offset="0.35" stop-color="#e2e6e8"/><stop offset="1" stop-color="#7f898e"/></linearGradient></defs>` +
      farmShadow(138, -12) + farmFoot(-136, 112) +
      `<path d="M-130 0 V-110 L-50 ${peak} L30 -110 V0 Z" fill="${col}" stroke="${dk}" stroke-width="2"/>` + farmBoards(-130, 30, topAt, 0, 16, dk) +
      `<path d="M-140 -103 L-50 ${peak - 8} L40 -103" fill="none" stroke="#4b3f3b" stroke-width="9" stroke-linejoin="round" stroke-linecap="round"/>` +
      `<path d="M-131 -108 L-50 ${peak + 2} L31 -108" fill="none" stroke="${trim}" stroke-width="3"/>` +
      farmDoors(-82, -18, -80, dk, trim) + farmWindow(-50, -146, 20, 18, trim) +
      `<path d="M36 -300 V0 Q72 ${f1(2 * farmBow(0))} 108 0 V-300 Z" fill="url(#${id})" stroke="#5f696e" stroke-width="2"/>` +
      `<path d="${hoops}" fill="none" stroke="#6f797e" stroke-width="2"/>` +
      `<path d="M36 -300 Q36 -336 72 -340 Q108 -336 108 -300 Q72 ${f1(-300 + 2 * farmBow(-300))} 36 -300 Z" fill="#a3aeb3" stroke="#5f696e" stroke-width="2"/><path d="M50 -318 Q62 -332 78 -334" fill="none" stroke="#ffffff" stroke-width="3" opacity="0.6"/>` +
      `<path d="M100 -294 V-6 M107 -294 V-6 ${rungs}" stroke="#4a5257" stroke-width="2"/>`;
  }
  // the red barn: its gambrel gable end facing us, boards to the roof line, white trim, big doors with X braces, the hay loft's
  // door open on its hay and the hook's beam over it, two windows; a weather vane on the ridge, turning with the quick LFO
  const topAt = x => Math.abs(x) >= 100 ? -130 - 62 * (120 - Math.abs(x)) / 20 : -192 - 44 * (100 - Math.abs(x)) / 100;
  return farmShadow(150) + farmFoot(-124, 124) +
    `<path d="M-120 0 V-130 L-100 -192 L0 -236 L100 -192 L120 -130 V0 Z" fill="${col}" stroke="${dk}" stroke-width="2"/>` + farmBoards(-120, 120, topAt, 0, 15, dk) +
    `<path d="M-130 -124 L-108 -196 L0 -245 L108 -196 L130 -124" fill="none" stroke="#4b3f3b" stroke-width="10" stroke-linejoin="round" stroke-linecap="round"/>` +
    `<path d="M-121 -128 L-101 -190 L0 -235 L101 -190 L121 -128" fill="none" stroke="${trim}" stroke-width="3"/>` +
    farmDoors(-46, 46, -100, dk, trim) + farmWindow(-84, -100, 26, 28, trim) + farmWindow(84, -100, 26, 28, trim) +
    `<rect x="-22" y="-178" width="44" height="42" fill="#2a1a10" stroke="${trim}" stroke-width="3"/>` +
    `<path d="M-20 -137 Q-14 -156 -4 -150 Q2 -164 10 -152 Q18 -158 20 -137 Z" fill="${FARM_STRAW}" stroke="${FARM_STRAW_D}" stroke-width="1.2"/>` +
    `<path d="M22 -178 L38 -182 L38 -132 L22 -136 Z" fill="${col}" stroke="${trim}" stroke-width="2.5"/>` +
    `<rect x="-7" y="-218" width="14" height="10" fill="#5e4026"/><path d="M0 -208 V-186" stroke="#6b5a40" stroke-width="1.5"/><circle cx="0" cy="-208" r="3" fill="#2b2b2b"/>` +
    weatherVaneSvg(o, 0, -245, R(1));
}
// The 🪵 fence: a run of it in its own units, standing on y = 0: posts at `posts` (x), its rails (or its wall) from a to b.
// A piece put in a place is one post, its rails reaching to the next piece's post (fencePieceSvg); the farm yard's own fence
// at its back is one run of many posts
const FENCE_STYLES = [["wood", "🪵 wooden post and rails"], ["picket", "🤍 white picket"], ["stone", "🪨 stone wall"]];
const FENCE_STUB = 20, FENCE_HALF = { wood: 7, picket: 6, stone: 13 };   // (a rail's end past a post with no piece beside it; half a post's width)
const fenceStyleOf = t => FENCE_STYLES.some(([v]) => v === t.style) ? t.style : "wood";
function fenceSvg(style, a, b, posts, seed) {
  const R = seeded(seed), P = (x, y) => `${f1(x)} ${f1(y)}`;
  const shade = `<ellipse class="flat" cx="${f1((a + b) / 2)}" cy="2" rx="${f1((b - a) / 2 + 8)}" ry="5" fill="#000000" opacity="0.16"/>`;
  const foot = `<rect data-footprint="1" x="${f1(a - 6)}" y="-8" width="${f1(b - a + 12)}" height="12" fill="none" stroke="none"/>`;
  if (style === "picket") {
    // white pickets, pointed, on two rails, none where a post stands; square posts with a cap and a point
    let pk = "";
    for (let x = a + 4; x + 9 <= b - 2; x += 15) if (!posts.some(p => Math.abs(x + 4.5 - p) < 11)) pk += `M${P(x, 0)} V-64 L${P(x + 4.5, -73)} L${P(x + 9, -64)} V0 Z `;
    return foot + shade + [-20, -50].map(y => `<rect x="${f1(a)}" y="${y}" width="${f1(b - a)}" height="6" fill="#ece7dc" stroke="#a9a293" stroke-width="1"/>`).join("") +
      (pk ? `<path d="${pk}" fill="#f6f3ec" stroke="#a9a293" stroke-width="1.2"/>` : "") +
      `<path d="${posts.map(x => `M${P(x - 6, 0)} V-80 H${f1(x + 6)} V0 Z`).join(" ")}" fill="#fbf9f4" stroke="#a9a293" stroke-width="1.2"/>` +
      `<path d="${posts.map(x => `M${P(x - 8, -80)} H${f1(x + 8)} V-85 H${f1(x - 8)} Z M${P(x - 4, -85)} L${P(x, -92)} L${P(x + 4, -85)} Z`).join(" ")}" fill="#ffffff" stroke="#a9a293" stroke-width="1"/>`;
  }
  if (style === "stone") {
    // a dry-stone wall: four courses of stones (three tones, a path each) over dark gaps, coping stones on edge along its top;
    // a pillar at each post, capped
    const d = ["", "", ""];
    for (let row = 0; row < 4; row++) { const y = -14 * (row + 1); let x = a - 14 * R(row + 7), n = 0;
      while (x < b) { const w = 16 + 16 * R(row * 31 + n), x0 = Math.max(a, x), x1 = Math.min(b, x + w);
        if (x1 - x0 > 5) d[(row + n) % 3] += `M${P(x0 + 3, y + 1)} H${f1(x1 - 3)} Q${P(x1 - 1, y + 1)} ${P(x1 - 1, y + 4)} V${f1(y + 10)} Q${P(x1 - 1, y + 13)} ${P(x1 - 3, y + 13)} H${f1(x0 + 3)} Q${P(x0 + 1, y + 13)} ${P(x0 + 1, y + 10)} V${f1(y + 4)} Q${P(x0 + 1, y + 1)} ${P(x0 + 3, y + 1)} Z `;
        x += w; n++; } }
    let cope = ""; for (let x = a; x + 4 < b; x += 9) cope += `M${P(x, -56)} v-7 q${f1(Math.min(9, b - x) / 2)} -4 ${f1(Math.min(9, b - x))} 0 v7 Z `;
    return foot + shade + `<rect x="${f1(a)}" y="-56" width="${f1(b - a)}" height="56" fill="#5f5a52"/>` + ["#a19c91", "#8c877d", "#b2ac9f"].map((c, j) => d[j] ? `<path d="${d[j]}" fill="${c}"/>` : "").join("") +
      `<path d="${cope}" fill="#a49e92" stroke="#5f5a52" stroke-width="1"/>` +
      `<path d="${posts.map(x => `M${P(x - 13, 0)} V-70 H${f1(x + 13)} V0 Z`).join(" ")}" fill="#9e988c" stroke="#5f5a52" stroke-width="1.5"/>` +
      `<path d="${posts.map(x => `M${P(x - 13, -24)} H${f1(x + 13)} M${P(x - 13, -48)} H${f1(x + 13)} M${P(x + 2, 0)} V-24 M${P(x - 5, -24)} V-48 M${P(x + 4, -48)} V-70`).join(" ")}" stroke="#6f6a62" stroke-width="1.2"/>` +
      `<path d="${posts.map(x => `M${P(x - 16, -70)} H${f1(x + 16)} V-78 Q${P(x, -84)} ${P(x - 16, -78)} Z`).join(" ")}" fill="#b3ad9f" stroke="#5f5a52" stroke-width="1.2"/>`;
  }
  // wooden post and rails: three rails of split wood, a light edge on each; posts with a pointed top, a darker side, grain
  const ys = [-26, -54, -80];
  return foot + shade + ys.map((y, j) => `<rect x="${f1(a)}" y="${y - 4}" width="${f1(b - a)}" height="8" rx="2" fill="${j % 2 ? "#a07650" : "#94693f"}" stroke="#4e3420" stroke-width="1.3"/>`).join("") +
    `<path d="${ys.map(y => `M${P(a + 2, y - 3)} H${f1(b - 2)}`).join(" ")}" stroke="#c49a6c" stroke-width="1.2" opacity="0.7"/>` +
    `<path d="${posts.map(x => `M${P(x - 7, 0)} V-92 L${P(x, -99)} L${P(x + 7, -92)} V0 Z`).join(" ")}" fill="#7a5634" stroke="#3e2a16" stroke-width="1.4"/>` +
    `<path d="${posts.map(x => `M${P(x + 3, -1)} V-93 L${P(x + 6, -92)} V-1 Z`).join(" ")}" fill="#5e4026"/>` +
    `<path d="${posts.map(x => `M${P(x - 3, -10)} Q${P(x - 1, -50)} ${P(x - 3, -86)}`).join(" ")}" fill="none" stroke="#4e3420" stroke-width="1" opacity="0.6"/>`;
}
// (ticket 502, Matthew: "let's start to plan fence creation as an in-game experience. Harvest mood let people pick up pieces of
// fence and move them tile by tile We don't have tiles yet. We havent' moved objects -- let's start experimenting.") Tiles: the
// first experiment. The floor of a place is a grid of tiles, square on the ground, TILE_SPAN across (a fence's span, post to
// post, in the stage's units where the friend arrives, and in a piece's own units) and as deep. Seen from the eye TILE_EYE in
// front of where the friend arrives (about a 60° view across the stage, which draws a round shadow on the floor as flat as
// the places draw theirs), a row of tiles farther back is narrower and shallower: row r's things are drawn at size
// TILE_EYE / (TILE_EYE - r TILE_SPAN) (row 0 where the friend arrives, nearer rows positive), its columns TILE_SPAN at that
// size apart, from the picture's middle; with 🔭 at 0 (no smaller going north) the rows are even. A thing whose kind says
// `tiles` (the 🪵 fence) snaps to the nearest tile when put or dragged (tileSnap), within the floor one can stand on
const TILE_SPAN = 110, TILE_EYE = 700;
const tileSpan = (B, z) => TILE_SPAN * depthSize(B, z - SCENE.ground);
const tileView = B => { const arrive = SCENE.ground + (ARRIVE[B.kind] || [0, 0])[1], v = sceneVanish(B); return { arrive, v, flat: !Number.isFinite(v) || v >= arrive - 1 }; };
const tileRowZ = (B, r) => { const { arrive, v, flat } = tileView(B); return flat ? arrive + r * TILE_SPAN * 0.22 : v + (arrive - v) * TILE_EYE / (TILE_EYE - r * TILE_SPAN); };
const tileRowOf = (B, z) => { const { arrive, v, flat } = tileView(B); if (flat) return (z - arrive) / (TILE_SPAN * 0.22);
  const s = (z - v) / (arrive - v); return s > 0 ? TILE_EYE * (1 - 1 / s) / TILE_SPAN : -Infinity; };
// (the floor one can stand on, from its back line, lo, to the picture's foot, hi; its rows from where a thing is a tenth of
// its size, the smallest drawn, to the nearest before the eye)
const tileFloor = B => { const F = sceneFrame, { flat } = tileView(B);
  return { lo: SCENE.ground + figYMin(F), hi: SCENE.ground + F.bottom - F.ground,
           rMin: flat ? -400 : Math.ceil(TILE_EYE * (1 - 10) / TILE_SPAN), rMax: flat ? 400 : Math.ceil(TILE_EYE / TILE_SPAN) - 1 }; };
function tileSnap(B, t) {
  if (!t || !(THING_KINDS[t.kind] || {}).tiles) return;
  const F = sceneFrame, { lo, hi, rMin, rMax } = tileFloor(B);
  let r = clamp(Math.round(tileRowOf(B, t.z)), rMin, rMax);
  while (r < rMax && tileRowZ(B, r) < lo) r++;
  while (r > rMin && tileRowZ(B, r) > hi) r--;
  const z = tileRowZ(B, r), w = tileSpan(B, z);
  let x = Math.round(t.x / w) * w;
  if (x < F.left) x += w; else if (x > F.right) x -= w;
  // (and the span of its tile, as it is put: its neighbours are a span away, whatever 🔭 says later, fenceLinks)
  t.z = Math.round(z * 10) / 10; t.x = Math.round(x * 10) / 10; t.tileW = Math.round(w * 10) / 10;
}
// (a fence piece tapped in, not dragged: on the tile nearest the last one put, along its row, the rest of the run's way first, as
// its style, size and side, so tapping again and again builds a fence; its row full, on the next row nearer, else farther, and
// on, from the tile nearest the last outward; null when there is none yet, or no free tile on the floor)
function tileNext(B, kind) {
  const T = (B.things || []).filter(t => t.kind === kind), last = T[T.length - 1];
  if (!last) return null;
  const F = sceneFrame, { lo, hi, rMin, rMax } = tileFloor(B), r0 = Math.round(tileRowOf(B, last.z));
  const taken = (x, z, w) => T.some(t => Math.abs(t.z - z) < 0.6 && Math.abs(t.x - x) < 0.3 * w);
  const back = T.length > 1 && T[T.length - 2].x > last.x && Math.abs(T[T.length - 2].z - last.z) < 0.6 ? -1 : 1;
  const as = (x, z) => ({ x, z, style: last.style, size: last.size, mirror: last.mirror });
  const w0 = tileSpan(B, last.z);
  for (let n = 1; n < 60; n++) for (const sd of [back, -back]) { const x = last.x + sd * n * w0;
    if (x >= F.left && x <= F.right && !taken(x, last.z, w0)) return as(x, last.z); }
  // (ticket 502, from a review: once its row was full, the next piece came out a plain wooden one somewhere else)
  for (let k = 1; k <= rMax - rMin; k++) for (const r of [r0 + k, r0 - k]) {
    if (r < rMin || r > rMax) continue;
    const z = Math.round(tileRowZ(B, r) * 10) / 10;
    if (!(z >= lo && z <= hi)) continue;
    const w = tileSpan(B, z), c = Math.round(last.x / w);
    for (let n = 0; n < 60; n++) for (const sd of n ? [back, -back] : [1]) { const x = Math.round((c + sd * n) * w * 10) / 10;
      if (x >= F.left && x <= F.right && !taken(x, z, w)) return as(x, z); }
  }
  return null;
}
// (whether a fence piece has one of its style a tile to its left on its row, and how far, in the stage's units, the one to its
// right is: 0 for none. A tile apart as the two were put (tileW, from tileSnap), so a run stays joined when 🔭 changes the
// tiles' size on its row afterwards; from a review: the runs at the back came apart)
function fenceLinks(B, i) {
  const T = B.things || [], t = T[i];
  if (!t || t.kind !== "fencepiece") return [false, 0];
  const w = t.tileW || tileSpan(B, t.z), st = fenceStyleOf(t);
  let left = false, right = 0;
  T.forEach((u, j) => {
    if (j === i || !u || u.kind !== "fencepiece" || fenceStyleOf(u) !== st || Math.abs(u.z - t.z) > 0.6) return;
    const dx = u.x - t.x, span = (w + (u.tileW || w)) / 2;
    if (Math.abs(Math.abs(dx) - span) > 0.3 * span) return;
    if (dx > 0) right = right ? Math.min(right, dx) : dx; else left = true;
  });
  return [left, Math.round(right * 10) / 10];
}
// a fence piece put in a place: its post, its rails (or wall) reaching to the post of the piece beside it on the right, or a
// stub on a side with none (a lone post has stubs both sides); the piece on its left reaches it. s: its size here, as thingSvg
// draws it (its own units are the stage's over s); mirrored, its own right is the picture's left
function fencePieceSvg(o, t, i, s) {
  const st = fenceStyleOf(t), [left, right] = fenceLinks(o, i), d = t.mirror ? -1 : 1;
  const r = right ? right / Math.max(0.01, s) - FENCE_HALF[st] : FENCE_STUB, l = left ? 0 : FENCE_STUB;
  return fenceSvg(st, Math.min(d * r, -d * l), Math.max(d * r, -d * l), [0], t.seed ?? 900 + i);
}
const THING_PARAMS = {
  // (Matthew, 2026-10-17: each object rescaled, from a good size: 100% is each kind's own, set in thingSvg)
  all: [["size", "🔎 size", "range", [25, 300, 5]], ["mirror", "↔️ mirror", "toggle"], ["seed", "🎲 another shape", "action", t => Math.floor(1000 + Math.random() * 90000)]],
  rock: [["tone", "🎨", "color", ROCK_TONES]],
  // (ticket 368) the witch's woods' things: a dead tree, a skull on a stake, a fence of stakes
  deadtree: [["h", "📏", "choice", [[160, "a stump"], [300, "tall"], [460, "giant"]]], ["droop", "🥀 weeping", "toggle"], ["color", "🎨", "color", ["#24192f", "#3b2c52", "#2a1f1a", "#4a3a30"]]],
  skullstake: [["eyes", "👁️ eyes", "choice", [[100, "usual"], [160, "big"], [220, "huge"]]]],
  palisade: [["stakes", "🪵 stakes", "choice", [[3, "3"], [6, "6"], [10, "10"], [16, "16"]]], ["skulls", "💀 skulls on it", "toggle"]],
  tree: [["h", "📏", "choice", [[280, "short"], [380, "tall"], [480, "taller"]]], ["leaves", "🍂", "choice", [["summer", "summer"], ["spring", "spring"], ["autumn", "autumn"], ["blossom", "🌸 blossom"]]]],
  pine: [["tiers", "🌲 tiers", "choice", [[4, "4"], [5, "5"], [6, "6"], [7, "7"], [8, "8"]]]],
  bush: [["flowers", "🌸", "color", ["none", "#f472b6", "#fde047", "#ffffff", "#a78bfa", "#ef4444"]]],
  toadstool: [["cap", "🍄", "color", TOADSTOOL_CAPS], ["glow", "✨ glowing", "toggle"]],
  // (ticket 318) moss: its kind (after five real mosses), where it grows, its colour (∅ its kind's own), lobes at its tips
  moss: [["style", "🌱 kind", "choice", MOSS_STYLE_CHOICES], ["on", "📍 on", "choice", MOSS_ON], ["color", "🎨", "color", MOSS_COLORS], ["lobes", "🟠 lobes at the tips", "toggle"]],
  // (ticket 319, for M) lichen: its kind (after real lichens), where it grows, its colour (∅ its kind's own)
  lichen: [["style", "🪨 kind", "choice", LICHEN_STYLE_CHOICES], ["on", "📍 on", "choice", MOSS_ON], ["color", "🎨", "color", LICHEN_COLORS]],
  potion: [["shape", "🧪", "choice", [["flask", "flask"], ["tall", "tall"], ["jar", "jar"], ["vial", "vial"]]], ["color", "🎨", "color", POTION_COLORS]],
  sign: [["text", "✏️ its words", "text", 12]],
  signpost: [["dirs", "🧭 boards", "dirs"]],
  campfire: [["burning", "🔥 burning", "toggle"]],
  // (ticket 204, Matthew: "let the world designer choose fancy or simple"; ticket 206: simple unless upgraded, one by one)
  cauldron: [["fumes", "💨 fumes", "choice", [["simple", "simple"], ["fancy", "✨ fancy"]]]],
  // (ticket 219) the sea things' own: colours, a shell's shape, a fish's bubbles, the chest open or shut, the anchor's chain
  coral: [["color", "🎨", "color", CORAL_COLORS]],
  shell: [["shape", "🐚", "choice", [["scallop", "scallop"], ["conch", "conch"], ["clam", "clam"]]], ["color", "🎨", "color", SHELL_COLORS]],
  starfish: [["color", "🎨", "color", STAR_COLORS]],
  seafish: [["color", "🎨", "color", SEAFISH_COLORS], ["bubbles", "🫧 bubbles", "toggle"]],
  chest: [["open", "🔓 open", "toggle"], ["treasure", "💰 in it", "choice", CHEST_TREASURE]],   // (ticket 471: what is in it)
  anchor: [["chain", "⛓️ broken chain", "toggle"]],
  // (ticket 284) a lever's 🔗: the trap doors here it opens and shuts (by their uid); a trap door's own: open or shut, its shape, its spikes
  // (ticket 323, N's: "each lever should choose going forward: controls doors; lets a boulder fall from somewhere, of a certain
  // style (including random/surprise me)"; "offer that param [the knob's colour] as a color selector generally")
  helmet: [["facing", "🪖 facing", "choice", [["toward", "toward us"], ["away", "away"], ["left", "⬅️ left"], ["right", "➡️ right"]]], ["plume", "🪶 plume", "color", ["#c0392b", "#f4f1ea", "#2f6db5", "#e0b030", "#1f1f22"]]],
  lever: [["does", "🎚️ it", "choice", [["doors", "🕳️ opens and shuts trap doors"], ["boulder", "🪨 drops a boulder into them"]]], ["works", "🔗 its trap doors", "wires"],
          ["rock", "🪨 the boulder", "choice", BOULDER_TONES], ["knob", "🎨 its knob", "color", KNOB_COLORS]],
  trapdoor: [["open", "🕳️ open", "toggle"], ["shape", "its opening", "choice", OPENING_SHAPES], ["spikes", "🔱 spikes inside", "toggle"]],
  // (ticket 290, for N) a bubble maker: how many bubbles a second, which of the friend's bubble themes it blows, its colour
  bubblemaker: [["shape", "🫧 its shape", "choice", [["round", "⭕ round"], ["box", "📦 a box"], ["tower", "🗼 a tower"]]], ["rate", "🫧 bubbles a second", "range", [1, 12, 1]], ["theme", "🫧 its bubbles", "choice", [["A", "🅰️ theme A"], ["B", "🅱️ theme B"], ["C", "©️ theme C"]]], ["color", "🎨", "color", MAKER_COLORS]],
  // (ticket 471) the cave things' own: a gem's kind, colour and glow; a puddle's drip; a little waterfall's kind
  gem: [["style", "💎 kind", "choice", GEM_STYLES], ["color", "🎨", "color", GEM_COLORS], ["glow", "✨ glowing", "toggle"]],
  puddle: [["drip", "💧 a drip from above", "toggle"]],
  cascade: [["style", "💦 kind", "choice", CASCADE_STYLES]],
  passage: [["style", "🕳️ kind", "choice", PASSAGE_STYLES], ["to", "🧭 leads to", "choice", passagePlaces], ["dest", "🧭 a cave door to a square, and back", "doordest"]],   // (ticket 525)
  cavemushroom: [["style", "🍄 kind", "choice", CAVE_MUSHROOMS], ["color", "🎨", "color", CAVE_MUSHROOM_COLORS], ["glow", "✨ glowing", "toggle"]],
  // (ticket 504, for M) the 🐄 farm animals' own: each its pose (four, each its own drawing) and its coat
  cow: [["pose", "🐄 pose", "choice", COW_POSES], ["coat", "🎨 coat", "choice", COW_COATS]],
  pig: [["pose", "🐖 pose", "choice", PIG_POSES], ["coat", "🎨 coat", "choice", PIG_COATS]],
  chicken: [["pose", "🐔 pose", "choice", HEN_POSES], ["coat", "🎨 coat", "choice", HEN_COATS]],
  // (ticket 501, for M) the desert plants' own: a saguaro's arms and flowers; a prickly pear's flowers or fruit and their colour; a
  // barrel cactus's crown of flowers and its colour; an agave's flower stalk and its leaves' colour; a Joshua tree in bloom; an
  // ocotillo's leaves and flowers
  saguaro: [["arms", "💪 arms", "choice", SAGUARO_ARMS], ["flowers", "🌸 flowers on top", "toggle"]],
  pricklypear: [["bloom", "🌼 on its pads", "choice", PEAR_BLOOMS], ["color", "🎨 its flowers", "color", DESERT_FLOWERS]],
  barrelcactus: [["flowers", "🌼 a crown of flowers", "toggle"], ["color", "🎨 its flowers", "color", DESERT_FLOWERS]],
  agave: [["stalk", "🌾 its tall flower stalk", "toggle"], ["tone", "🎨 its leaves", "color", AGAVE_TONES]],
  joshuatree: [["flowers", "🌼 in bloom", "toggle"]],
  ocotillo: [["leaves", "🍃 leafed out (after rain)", "toggle"], ["flowers", "🔥 red flowers at the tips", "toggle"]],
  // (ticket 502) the farm things' own: a barn's kind and its walls' colour (∅ its kind's own); a fence piece's kind
  barn: [["style", "🏚️ kind", "choice", BARN_STYLES], ["color", "🎨", "color", BARN_COLORS]],
  fencepiece: [["style", "🪵 kind", "choice", FENCE_STYLES]],
  kelp: [["color", "🎨", "color", SEAPLANT_COLORS]], seagrass: [["color", "🎨", "color", SEAPLANT_COLORS]],
  seafan: [["color", "🎨", "color", CORAL_COLORS]], sealettuce: [["color", "🎨", "color", SEAPLANT_COLORS]],
};
const THING_DEFAULTS = { deadtree: { h: 300, droop: false, color: "#24192f" }, skullstake: { eyes: 100 }, palisade: { stakes: 6, skulls: true }, rock: { tone: ROCK_TONES[0] },   // (ticket 368: the first three)
                         tree: { h: 380, leaves: "summer" }, pine: { tiers: 6 }, bush: { flowers: "none" },   // (ticket 369: swallowed by the note above, 0.0.287-0.0.297)
                         toadstool: { cap: TOADSTOOL_CAPS[0], glow: false }, moss: { style: "haircap", on: "floor", color: "none", lobes: true }, lichen: { style: "crust", on: "back", color: "none" }, potion: { shape: "flask", color: POTION_COLORS[5] }, sign: { text: "hello!" },
                         campfire: { burning: true }, cauldron: { fumes: "simple" },
                         coral: { color: CORAL_COLORS[0] }, shell: { shape: "scallop", color: SHELL_COLORS[0] }, starfish: { color: STAR_COLORS[0] },
                         seafish: { color: SEAFISH_COLORS[0], bubbles: true }, chest: { open: true, treasure: "gold" }, anchor: { chain: true }, lever: { on: false, works: [], does: "doors", rock: ROCK_TONES[0], knob: "auto" }, helmet: { facing: "toward", plume: "#c0392b" }, trapdoor: { open: false, shape: "circle", spikes: true },
                         bubblemaker: { shape: "round", rate: 4, theme: "A", color: MAKER_COLORS[0] },
                         kelp: { color: SEAPLANT_COLORS[0] }, seagrass: { color: SEAPLANT_COLORS[1] }, seafan: { color: CORAL_COLORS[2] }, sealettuce: { color: SEAPLANT_COLORS[2] },
                         gem: { style: "cut", color: GEM_COLORS[0], glow: true }, puddle: { drip: true }, cascade: { style: "trickle" }, passage: { style: "arch", to: "cave" }, cavemushroom: { style: "glowcap", color: CAVE_MUSHROOM_COLORS[0], glow: true },   // (ticket 471)
                         cow: { pose: "stand", coat: "holstein" }, pig: { pose: "stand", coat: "pink" }, chicken: { pose: "peck", coat: "brown" },   // (ticket 504)
                         saguaro: { arms: 2, flowers: true }, pricklypear: { bloom: "flowers", color: DESERT_FLOWERS[0] }, barrelcactus: { flowers: true, color: DESERT_FLOWERS[1] },
                         agave: { stalk: false, tone: AGAVE_TONES[0] }, joshuatree: { flowers: false }, ocotillo: { leaves: true, flowers: true },   // (ticket 501)
                         barn: { style: "gambrel", color: "none" }, fencepiece: { style: "wood" } };   // (ticket 502)
const thingParams = kind => [...THING_PARAMS.all.filter(([p]) => kind !== "lifted" || p !== "seed"), ...(THING_PARAMS[kind] || [])];   // (ticket 284: a lifted thing has one shape, its own)
// (ticket 219, Matthew: a bubble, eventually a concept of its own: "color,
// size and shininess (like stars we add or not, and controls over their color
// and type)"; "these initial bubbles can be the default, simple, small kind")
// A bubble at (cx, cy), radius r: a thin rim of its colour over a faint fill
// of it, a shine (a highlight, 0 to 1), and, if asked, a sparkle on it (a
// four-point star or a dot, of its own colour). The deep sea's fish and vent
// blow the default; bubble space and blown bubbles (tickets 220, 221) set the
// rest.
const BUBBLE_DEFAULT = { color: "#e6f6ff", shine: 0.7, sparkle: "none", sparkleColor: "#ffffff" };
function bubbleSvg(cx, cy, r, b = BUBBLE_DEFAULT) {
  const c = b.color || BUBBLE_DEFAULT.color, sh = Math.max(0, Math.min(1, b.shine ?? 0.7));
  let t = `<circle cx="${f1(cx)}" cy="${f1(cy)}" r="${f1(r)}" fill="${c}" fill-opacity="0.12" stroke="${c}" stroke-opacity="0.85" stroke-width="${f1(Math.max(0.6, r * 0.12))}"/>`;
  if (sh > 0) t += `<ellipse cx="${f1(cx - r * 0.35)}" cy="${f1(cy - r * 0.38)}" rx="${f1(r * 0.28)}" ry="${f1(r * 0.18)}" fill="#ffffff" opacity="${(0.25 + 0.65 * sh).toFixed(2)}" transform="rotate(-35 ${f1(cx - r * 0.35)} ${f1(cy - r * 0.38)})"/>`;
  if (b.sparkle === "star") { const x = cx + r * 0.3, y = cy - r * 0.45, a = r * 0.42, q = a * 0.22;
    t += `<path d="M${f1(x)} ${f1(y - a)} L${f1(x + q)} ${f1(y - q)} L${f1(x + a)} ${f1(y)} L${f1(x + q)} ${f1(y + q)} L${f1(x)} ${f1(y + a)} L${f1(x - q)} ${f1(y + q)} L${f1(x - a)} ${f1(y)} L${f1(x - q)} ${f1(y - q)} Z" fill="${b.sparkleColor || "#ffffff"}"/>`; }
  else if (b.sparkle === "dot") t += `<circle cx="${f1(cx + r * 0.3)}" cy="${f1(cy - r * 0.45)}" r="${f1(r * 0.16)}" fill="${b.sparkleColor || "#ffffff"}"/>`;
  return t;
}
// n bubbles rising from (x, y) by h, each in turn (dur seconds a rise, begun
// at a step of it), wobbling a little, fading out at the top, and again: out
// of sight, gone, and the next one up. `seed` keeps their sizes the same
// every drawing.
function risingBubbles(x, y, n, h, dur, r, seed, b = BUBBLE_DEFAULT) {
  const R = (i, j) => { const v = Math.sin(i * 127.1 + j * 311.7) * 43758.5453; return v - Math.floor(v); };
  let t = "";
  for (let i = 0; i < n; i++) {
    const rr = r * (0.6 + 0.8 * R(seed + i, 1)), wob = (R(seed + i, 2) - 0.5) * 8 * r, begin = -(dur * i / n).toFixed(2);
    t += `<g><animateTransform attributeName="transform" type="translate" values="0 0;${f1(wob)} ${f1(-h * 0.5)};${f1(-wob * 0.5)} ${f1(-h)}" dur="${dur}s" begin="${begin}s" repeatCount="indefinite"/>` +
         `<animate attributeName="opacity" values="0;1;1;0" keyTimes="0;0.08;0.8;1" dur="${dur}s" begin="${begin}s" repeatCount="indefinite"/>` + bubbleSvg(x, y, rr, b) + `</g>`;
  }
  return t;
}
// (ticket 219) The 🐚 sea things, each drawn standing at (0, 0) in its own
// units, as the other things are (thingSvg scales and places them).
const seaR = (seed, j) => { const v = Math.sin(seed * 127.1 + j * 311.7) * 43758.5453; return v - Math.floor(v); };
// coral: a branching stand, each branch splitting, round tips (its seed its shape)
function coralSvg(seed, color) {
  let t = "";
  const branch = (x, y, a, len, w, depth, j) => {
    const x2 = x + Math.cos(a) * len, y2 = y + Math.sin(a) * len;
    t += `<line x1="${f1(x)}" y1="${f1(y)}" x2="${f1(x2)}" y2="${f1(y2)}" stroke="${color}" stroke-width="${f1(w)}" stroke-linecap="round"/>`;
    if (depth <= 0) { t += `<circle cx="${f1(x2)}" cy="${f1(y2)}" r="${f1(w * 0.75)}" fill="${color}"/>`; return; }
    for (const s of [-1, 1]) branch(x2, y2, a + s * (0.35 + 0.35 * seaR(seed, j * 7 + s + 3)), len * (0.62 + 0.18 * seaR(seed, j * 5 + s)), w * 0.72, depth - 1, j * 2 + (s > 0 ? 1 : 0));
  };
  for (let i = 0; i < 3; i++) branch((i - 1) * 14, 0, -Math.PI / 2 + (i - 1) * 0.45, 46 + 12 * seaR(seed, 90 + i), 11, 3, 1 + i);
  return `<ellipse cx="0" cy="3" rx="40" ry="8" fill="#000000" opacity="0.18"/>` + t;
}
// a shell on the sand: a scallop (a fan, ribbed), a conch (a spiral cone), or a clam (two halves, a little open)
function seaShellSvg(shape, color) {
  const ink = mix(color, "#5b3a1a", 0.45);
  if (shape === "conch") return `<ellipse cx="0" cy="2" rx="30" ry="6" fill="#000000" opacity="0.18"/>` +
    `<path d="M-30 -6 Q-28 -30 0 -36 Q30 -30 34 -8 Q20 0 -30 -6 Z" fill="${color}" stroke="${ink}" stroke-width="2"/>` +
    [0.3, 0.55, 0.78].map(u => `<path d="M${f1(-30 + 64 * u)} -6 Q${f1(-26 + 60 * u)} -24 ${f1(-14 + 40 * u)} -32" fill="none" stroke="${ink}" stroke-width="1.6" opacity="0.7"/>`).join("") +
    `<path d="M-30 -6 L-42 -2 L-30 0 Z" fill="${color}" stroke="${ink}" stroke-width="1.6"/>`;
  if (shape === "clam") return `<ellipse cx="0" cy="2" rx="28" ry="6" fill="#000000" opacity="0.18"/>` +
    `<path d="M-26 -4 Q0 4 26 -4 Q20 -18 0 -20 Q-20 -18 -26 -4 Z" fill="${color}" stroke="${ink}" stroke-width="2"/>` +
    `<path d="M-24 -8 Q0 -40 24 -8 Q0 -14 -24 -8 Z" fill="${mix(color, "#ffffff", 0.25)}" stroke="${ink}" stroke-width="2"/>` +
    `<circle cx="0" cy="-10" r="4.5" fill="#fffaf0" stroke="#d1c4a8" stroke-width="1"/>`;
  return `<ellipse cx="0" cy="2" rx="28" ry="6" fill="#000000" opacity="0.18"/>` +
    `<path d="M0 0 L-26 -16 Q-22 -38 0 -42 Q22 -38 26 -16 Z" fill="${color}" stroke="${ink}" stroke-width="2" stroke-linejoin="round"/>` +
    [-18, -9, 0, 9, 18].map(x => `<line x1="0" y1="-2" x2="${f1(x)}" y2="${f1(-36 + Math.abs(x) * 0.5)}" stroke="${ink}" stroke-width="1.5" opacity="0.7"/>`).join("") +
    `<path d="M-7 0 L0 -6 L7 0 Z" fill="${color}" stroke="${ink}" stroke-width="1.5"/>`;
}
// a starfish lying on the sand, its arms curling a little, crawling slowly to and fro
function starfishSvg(color, seed) {
  const ink = mix(color, "#3b1a0a", 0.4);
  let d = "";
  for (let i = 0; i < 10; i++) { const a = -Math.PI / 2 + i * Math.PI / 5, r = i % 2 ? 9 : 26; d += `${i ? "L" : "M"}${f1(Math.cos(a) * r)} ${f1(Math.sin(a) * r * 0.45 - 8)} `; }
  const dots = [...Array(7)].map((_, i) => { const a = -Math.PI / 2 + i * 0.9, r = 5 + 12 * seaR(seed, i); return `<circle cx="${f1(Math.cos(a) * r)}" cy="${f1(Math.sin(a) * r * 0.45 - 8)}" r="1.6" fill="${mix(color, "#ffffff", 0.45)}"/>`; }).join("");
  const span = 50 + 30 * seaR(seed, 20), dur = (16 + 8 * seaR(seed, 21)).toFixed(1);
  return `<g><animateTransform attributeName="transform" type="translate" values="${f1(-span)} 0;${f1(span)} 0;${f1(-span)} 0" dur="${dur}s" repeatCount="indefinite"/>` +
    `<ellipse cx="0" cy="2" rx="26" ry="5" fill="#000000" opacity="0.16"/>` +
    `<g><animateTransform attributeName="transform" type="rotate" values="-8 0 -8;10 0 -8;-8 0 -8" dur="${(dur / 3).toFixed(1)}s" repeatCount="indefinite"/>` +
    `<path d="${d}Z" fill="${color}" stroke="${ink}" stroke-width="2" stroke-linejoin="round"/>${dots}</g></g>`;
}
// a fish swimming round a loop over its spot: farther (higher, smaller) on the
// way back, turning about at each end (its scale through zero: it turns in
// depth), its tail wagging, and, if it blows them, bubbles rising from it
function seaFishSvg(color, seed, bubbles = true) {
  const ink = mix(color, "#202020", 0.45), dur = (10 + 6 * seaR(seed, 30)).toFixed(1), A = 90 + 40 * seaR(seed, 31), y0 = -150 - 40 * seaR(seed, 32);
  // (round the loop: x = A cos θ, higher and smaller, farther, as sin θ grows; facing the way it goes, -sin θ: drawn
  // facing right, so mirrored on the way back; between two points either side of an end the scale passes zero)
  const N = 16, xs = [], sc = [];
  for (let i = 0; i <= N; i++) { const th = 2 * Math.PI * i / N, sn = Math.sin(th), z = 1 - 0.22 * sn;
    xs.push(`${f1(A * Math.cos(th))} ${f1(y0 - 34 * sn)}`); sc.push(`${((sn > 1e-9 ? -1 : 1) * z).toFixed(3)} ${z.toFixed(3)}`); }
  const body = `<ellipse cx="0" cy="0" rx="26" ry="13" fill="${color}" stroke="${ink}" stroke-width="2"/>` +
    `<g><animateTransform attributeName="transform" type="rotate" values="-18 -22 0;18 -22 0;-18 -22 0" dur="0.7s" repeatCount="indefinite"/>` +
    `<path d="M-22 0 L-40 -13 L-40 13 Z" fill="${color}" stroke="${ink}" stroke-width="2" stroke-linejoin="round"/></g>` +
    `<path d="M-4 -12 Q4 -24 14 -11" fill="${mix(color, "#ffffff", 0.2)}" stroke="${ink}" stroke-width="1.5"/>` +
    `<circle cx="13" cy="-3" r="4" fill="#ffffff"/><circle cx="14" cy="-3" r="2" fill="#111111"/>` +
    `<path d="M24 3 Q20 6 17 4" fill="none" stroke="${ink}" stroke-width="1.5"/>`;
  return `<ellipse cx="0" cy="2" rx="${f1(A * 0.5)}" ry="6" fill="#000000" opacity="0.08"/>` +
    `<g><animateTransform attributeName="transform" type="translate" values="${xs.join(";")}" dur="${dur}s" repeatCount="indefinite"/>` +
    `<g><animateTransform attributeName="transform" type="scale" values="${sc.join(";")}" dur="${dur}s" repeatCount="indefinite"/>${body}</g>` +
    (bubbles ? risingBubbles(0, -14, 3, 150, 2.6, 4, seed) : "") + `</g>`;
}
// a treasure chest on the sand: the castle's chest of gold, lid up, coins spilling (or shut)
// (ticket 471, Matthew: "More objects to refine and make placeable -- treasure chest") A treasure chest: planks banded with
// gold, a lock; open, its lid back and its treasure heaped (🪙 gold, 💎 gems, or 👑 both and a crown), a few coins spilled,
// sparkling (the fast LFO); shut, locked
function seaChestSvg(open, treasure = "gold", o = {}) {
  const gold = "#e8c33a", band = "#c9a227", coin = (x, y, r = 6) => `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${r}" ry="${f1(r / 2)}" fill="${gold}" stroke="#9a7a12" stroke-width="1"/>`;
  const gem = (x, y, col) => `<path d="M${f1(x - 5)} ${f1(y - 3)} L${f1(x - 2)} ${f1(y - 7)} L${f1(x + 2)} ${f1(y - 7)} L${f1(x + 5)} ${f1(y - 3)} L${f1(x)} ${f1(y + 3)} Z" fill="${col}" stroke="${mix(col, "#000000", 0.35)}" stroke-width="1"/>`;
  const planks = (y0, h) => [1, 2].map(i => `<line x1="-40" y1="${f1(y0 + h * i / 3)}" x2="40" y2="${f1(y0 + h * i / 3)}" stroke="#5e3d22" stroke-width="1.5" opacity="0.7"/>`).join("");
  let s = `<ellipse cx="0" cy="3" rx="54" ry="9" fill="#000000" opacity="0.2"/>` +
    `<rect x="-40" y="-40" width="80" height="40" rx="2" fill="#7a4f2a" stroke="#4a2e1a" stroke-width="2"/>` + planks(-40, 40);
  if (open) {
    s += `<path d="M-40 -40 L-36 -76 L44 -76 L40 -40 Z" fill="#8b5e34" stroke="#4a2e1a" stroke-width="2"/><path d="M-34 -44 L-31 -70 L38 -70 L35 -44 Z" fill="#4a2e1a" opacity="0.55"/>` +
      [-1, 1].map(e => `<path d="M${f1(-3 + e * 26)} -40 L${f1(-1 + e * 27)} -76 L${f1(5 + e * 27)} -76 L${f1(3 + e * 26)} -40 Z" fill="${band}"/>`).join("");
    // the heap: gold always, and gems or a crown as it says
    s += `<path d="M-38 -40 Q-20 -58 0 -54 Q22 -60 38 -40 Z" fill="${gold}" stroke="#9a7a12" stroke-width="1.2"/>` +
      [...Array(12)].map((_, j) => coin(-32 + 64 * seaR(j, 970), -40 - 6 * seaR(j, 971) - 6 * Math.sin(Math.PI * seaR(j, 970)))).join("");
    if (treasure !== "gold") s += [...Array(treasure === "gems" ? 7 : 4)].map((_, j) => gem(-26 + 52 * seaR(j, 974), -46 - 8 * seaR(j, 975), GEM_COLORS[j % 6])).join("");
    if (treasure === "crown") s += `<path d="M-6 -56 L-6 -70 L0 -63 L6 -72 L12 -63 L18 -70 L18 -56 Z" fill="${gold}" stroke="#9a7a12" stroke-width="1.4"/>` +
      [[0, -60, "#e11d48"], [6, -62, "#3b82f6"], [12, -60, "#22c55e"]].map(([x, y, col]) => `<circle cx="${x}" cy="${y}" r="1.8" fill="${col}"/>`).join("");
    s += [...Array(5)].map((_, j) => coin(-48 - 26 * seaR(j, 972), 2 + 6 * seaR(j, 973))).join("") +
      [[-14, -58, 6, 0], [16, -54, 5, 0.4], [-28, -46, 4, 0.7]].map(([x, y, r, off]) => gemSparkle(o, x, y, r, off)).join("");
  } else s += `<path d="M-42 -40 Q0 -62 42 -40 Z" fill="#8b5e34" stroke="#4a2e1a" stroke-width="2"/>` +
    [-1, 1].map(e => `<path d="M${f1(-3 + e * 26)} -40 Q${f1(-3 + e * 26)} ${f1(-56 + Math.abs(e) * 4)} ${f1(e * 27)} ${f1(-55 + Math.abs(e) * 4)} L${f1(6 + e * 26)} -55 Q${f1(3 + e * 26)} -50 ${f1(3 + e * 26)} -40 Z" fill="${band}"/>`).join("") +
    `<rect x="-7" y="-46" width="14" height="14" rx="2" fill="${band}" stroke="#6b5414" stroke-width="1"/><circle cx="0" cy="-41" r="2" fill="#3a2a10"/><rect x="-0.8" y="-41" width="1.6" height="5" fill="#3a2a10"/>`;
  return s + [-1, 1].map(e => `<rect x="${f1(-3 + e * 26)}" y="-40" width="6" height="40" fill="${band}"/>`).join("") +
    `<rect x="-40" y="-6" width="80" height="4" fill="${band}" opacity="0.9"/>`;
}
// an anchor half sunk in the sand, a length of chain still on its ring, broken off
function anchorSvg(chain) {
  const iron = "#56606a", ink = "#2e353b";
  let t = `<ellipse cx="0" cy="3" rx="60" ry="8" fill="#000000" opacity="0.2"/>` +
    `<g transform="rotate(-12 0 -40)"><rect x="-5" y="-110" width="10" height="104" fill="${iron}" stroke="${ink}" stroke-width="2"/>` +
    `<rect x="-30" y="-96" width="60" height="9" rx="3" fill="${iron}" stroke="${ink}" stroke-width="2"/>` +
    `<circle cx="0" cy="-118" r="10" fill="none" stroke="${iron}" stroke-width="6"/>` +
    `<path d="M-46 -30 Q-40 0 0 -2 Q40 0 46 -30 L38 -26 Q34 -10 0 -10 Q-34 -10 -38 -26 Z" fill="${iron}" stroke="${ink}" stroke-width="2"/>` +
    `<path d="M-46 -30 L-56 -40 L-40 -36 Z M46 -30 L56 -40 L40 -36 Z" fill="${iron}" stroke="${ink}" stroke-width="2"/></g>`;
  if (chain) for (let i = 0; i < 6; i++) { const x = 14 + i * 13, y = -126 + i * 18 + (i % 2) * 3;
    t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${i % 2 ? 4 : 8}" ry="${i % 2 ? 8 : 5}" fill="none" stroke="${iron}" stroke-width="3.5"/>`; }
  if (chain) t += `<path d="M86 -18 l5 -4 l-1 6 l4 -2" fill="none" stroke="${ink}" stroke-width="2"/>`;   // (where it broke)
  return t;
}
// the sea plants: kelp (tall ribbons with blades), sea grass (thin blades in a tuft), a sea fan (a lattice in a fan),
// sea lettuce (round wavy leaves in a heap); kelp and grass sway with the scene's slow LFO
function seaPlantSvg(kind, color, seed, o) {
  const dark = mix(color, "#0b2a1a", 0.35);
  if (kind === "seagrass") {
    let t = "";
    for (let i = 0; i < 9; i++) { const x = (i - 4) * 6, h = 70 + 50 * seaR(seed, i), bend = (seaR(seed, i + 20) - 0.5) * 30;
      t += `<path d="M${f1(x - 2.5)} 0 Q${f1(x + bend)} ${f1(-h * 0.55)} ${f1(x + bend * 1.3)} ${f1(-h)} Q${f1(x + bend + 2)} ${f1(-h * 0.55)} ${f1(x + 2.5)} 0 Z" fill="${i % 2 ? color : dark}"/>`; }
    return `<ellipse cx="0" cy="2" rx="34" ry="6" fill="#000000" opacity="0.16"/>` + swaying(o, t, 0, 0, 6, seaR(seed, 40), (4 + 2 * seaR(seed, 41)).toFixed(1));
  }
  if (kind === "seafan") {
    let t = `<path d="M0 0 L-60 -110 Q0 -150 60 -110 Z" fill="${color}" opacity="0.28"/>`;
    for (let i = 0; i <= 8; i++) { const a = -Math.PI / 2 + (i - 4) * 0.12; t += `<line x1="0" y1="0" x2="${f1(Math.cos(a) * 130)}" y2="${f1(Math.sin(a) * 130)}" stroke="${color}" stroke-width="3"/>`; }
    for (let j = 1; j <= 4; j++) { const r = j * 30; t += `<path d="M${f1(Math.cos(-Math.PI / 2 - 0.48) * r)} ${f1(Math.sin(-Math.PI / 2 - 0.48) * r)} Q0 ${f1(-r * 1.08)} ${f1(Math.cos(-Math.PI / 2 + 0.48) * r)} ${f1(Math.sin(-Math.PI / 2 + 0.48) * r)}" fill="none" stroke="${color}" stroke-width="2"/>`; }
    return `<ellipse cx="0" cy="2" rx="30" ry="6" fill="#000000" opacity="0.16"/>` + t + `<rect x="-4" y="-14" width="8" height="16" fill="${dark}"/>`;
  }
  if (kind === "sealettuce") {
    let t = "";
    for (let i = 0; i < 6; i++) { const x = (seaR(seed, i) - 0.5) * 50, y = -10 - 26 * seaR(seed, i + 10), r = 16 + 8 * seaR(seed, i + 20);
      let d = ""; for (let j = 0; j <= 12; j++) { const a = j * Math.PI / 6, rr = r * (0.85 + 0.15 * Math.sin(j * 2.3 + i)); d += `${j ? "L" : "M"}${f1(x + Math.cos(a) * rr)} ${f1(y + Math.sin(a) * rr * 0.7)} `; }
      t += `<path d="${d}Z" fill="${i % 2 ? color : mix(color, "#ffffff", 0.18)}" stroke="${dark}" stroke-width="1.5"/>`; }
    return `<ellipse cx="0" cy="2" rx="36" ry="7" fill="#000000" opacity="0.16"/>` + t;
  }
  // kelp
  let t = "";
  for (let i = 0; i < 3; i++) { const x = (i - 1) * 12, h = 170 + 60 * seaR(seed, i); let d = `M${f1(x)} 0`, blades = "";
    for (let j = 1; j <= 5; j++) { const yj = -h * j / 5, sd = (j + i) % 2 ? 1 : -1; d += ` Q${f1(x + sd * 12)} ${f1(-h * (j - 0.5) / 5)} ${f1(x)} ${f1(yj)}`;
      blades += `<path d="M${f1(x)} ${f1(yj + 8)} q${f1(sd * 20)} -10 ${f1(sd * 26)} -26 q${f1(-sd * 8)} 16 ${f1(-sd * 26)} 26 Z" fill="${color}"/>`; }
    t += `<path d="${d}" fill="none" stroke="${dark}" stroke-width="5" stroke-linecap="round"/>` + blades; }
  return `<ellipse cx="0" cy="2" rx="30" ry="6" fill="#000000" opacity="0.16"/>` + swaying(o, t, 0, 0, 5, seaR(seed, 42), (5 + 2 * seaR(seed, 43)).toFixed(1));
}
// (ticket 185, v3: the object library) The things, in groups, each drawn by
// a drawing the places already have (the fairy forest's toadstools, the
// witch's potions, the deep dungeon's skeleton, the clearing's plank sign
// on a post), none of them changed: a thing is that drawing standing on
// the floor at its own depth, sized as the friend is there. (0.0.113) And
// drawings lifted out of the places for it, each place's markup the same
// as before, which the safety net checks: the camp site's log and campfire,
// the picnic's bench, the castle kitchen's barrel, the witch's cauldron.
const THING_GROUPS = [["nature", "🌲 nature"], ["camp", "🏕️ camp"], ["magic", "🧪 magic"], ["spooky", "💀 spooky"], ["signs", "🪧 signs"], ["sea", "🐚 sea"], ["cave", "🦇 cave"], ["farm", "🚜 farm"], ["desert", "🌵 desert"], ["animals", "🐄 farm animals"], ["works", "🎚️ switches and traps"], ["bubbles", "🫧 bubbles"], ["castle", "🏰 castle"], ["places", "📦 from the places"]];
const THING_KINDS = { rock: { label: "🪨 rock", size: 1, group: "nature" }, bush: { label: "🌿 bush", size: 1, group: "nature" }, tree: { label: "🌳 tree", size: 1, group: "nature" },
                      pine: { label: "🌲 pine", size: 1, group: "nature" }, toadstool: { label: "🍄 toadstool", size: 1, group: "nature" }, moss: { label: "🌱 moss", size: 1, group: "nature" }, lichen: { label: "🪨 lichen", size: 1, group: "nature" },
                      log: { label: "🪵 log", size: 1, group: "camp" }, campfire: { label: "🔥 campfire", size: 1, group: "camp", moving: true }, bench: { label: "🪑 bench", size: 1, group: "camp" },
                      barrel: { label: "🛢️ barrel", size: 1, group: "camp" }, cauldron: { label: "🫕 cauldron", size: 1, group: "magic", moving: true },
                      potion: { label: "🧪 potion", size: 1, group: "magic" }, skeleton: { label: "💀 skeleton", size: 1, group: "spooky" }, deadtree: { label: "🌳 dead tree", size: 1, group: "spooky" }, skullstake: { label: "💀 skull on a stake", size: 1, group: "spooky" }, palisade: { label: "🪵 fence of stakes", size: 1, group: "spooky" },   // (deadtree, skullstake, palisade: ticket 368)
                      sign: { label: "🪧 sign", size: 1, group: "signs" },   // (ticket 369: the note above swallowed it, 0.0.287-0.0.297)
                      signpost: { label: "🧭 sign with directions", size: 1, group: "signs" },
                      // (ticket 219) the 🐚 sea things, for the deep sea (and anywhere): the moving ones are counted as the campfire is
                      coral: { label: "🪸 coral", size: 1, group: "sea" }, shell: { label: "🐚 shell", size: 1, group: "sea" },
                      starfish: { label: "⭐ starfish", size: 1, group: "sea", moving: true }, seafish: { label: "🐟 fish", size: 1, group: "sea", moving: true },
                      chest: { label: "💰 treasure chest", size: 1, group: "sea" }, anchor: { label: "⚓ anchor", size: 1, group: "sea" },
                      kelp: { label: "🌿 kelp", size: 1, group: "sea", moving: true }, seagrass: { label: "🌾 sea grass", size: 1, group: "sea", moving: true },
                      seafan: { label: "🪭 sea fan", size: 1, group: "sea" }, sealettuce: { label: "🥬 sea lettuce", size: 1, group: "sea" },
                      // (ticket 471) the 🦇 cave things, for the cave room (and anywhere): a gem, a puddle, a little waterfall
                      gem: { label: "💎 gem", size: 1, group: "cave", moving: true }, puddle: { label: "💧 puddle", size: 1, group: "cave", moving: true },
                      cascade: { label: "💦 little waterfall", size: 1, group: "cave", moving: true }, passage: { label: "🕳️ cave passage", size: 1, group: "cave" },
                      cavemushroom: { label: "🍄 cave mushroom", size: 1, group: "cave", moving: true },
                      // (ticket 504, for M) the 🐄 farm animals: a cow, a pig and a chicken, each in four poses, moving with the music
                      cow: { label: "🐄 cow", size: 1, group: "animals", moving: true }, pig: { label: "🐖 pig", size: 1, group: "animals", moving: true },
                      chicken: { label: "🐔 chicken", size: 1, group: "animals", moving: true },
                      // (ticket 501, for M) the 🌵 desert plants, for the desert (and anywhere): none of them moves
                      saguaro: { label: "🌵 saguaro", size: 1, group: "desert" }, pricklypear: { label: "🍐 prickly pear", size: 1, group: "desert" },
                      barrelcactus: { label: "🛢️ barrel cactus", size: 1, group: "desert" }, agave: { label: "🪴 agave", size: 1, group: "desert" },
                      joshuatree: { label: "🌴 Joshua tree", size: 1, group: "desert" }, ocotillo: { label: "🔥 ocotillo", size: 1, group: "desert" },
                      // (ticket 502, for M) the 🚜 farm things: a barn (its weather vane, a coop's hens, move), a piece of fence (it
                      // snaps to the floor's tiles: tiles, tileSnap)
                      barn: { label: "🏚️ barn", size: 1, group: "farm", moving: true }, fencepiece: { label: "🪵 fence", size: 1, group: "farm", tiles: true },
                      // (ticket 284) a 🎚️ lever, and 🕳️ trap doors it opens and shuts
                      lever: { label: "🎚️ lever", size: 1, group: "works" }, trapdoor: { label: "🕳️ trap door", size: 1, group: "works" },
                      // (ticket 284) any object of any place: its own buttons, a place's at a time (the Background tab's "places" group)
                      // (ticket 290, for N) a 🫧 bubble maker, blowing bubbles wherever it is put
                      bubblemaker: { label: "🫧 bubble maker", size: 1, group: "bubbles", moving: true },
                      // (ticket 435, Matthew: "also permits us to place helmets in each of the 4 poses (L, R, toward, away)") the
                      // trap room's helmet, the knight's (helmetSvg), standing on its rim, facing toward us, away, left or right
                      helmet: { label: "🪖 helmet", size: 1, group: "castle" },
                      lifted: { label: "📦 from a place", size: 1, group: "places" } };
// (ticket 284, Matthew: "audit each scene for objects that we can offer as new placed objects ... The ticket would be
// complete if we feel that every object in each scene is something that can also be placed") 📦 any object of any
// place, put in any place: { kind: "lifted", from: <a place>, name: <its object's name> }. Lifted from that place's own
// drawing at its defaults (as OBJECTS.md shows it), with the definitions it uses (ids prefixed by the place), and drawn
// as a thing at its new spot, scaled from the size it had at its own depth there to the size here. It keeps its name,
// so it meets the friend as it does at home (STANDING, TRUNKED: by name). A place is a kind, or "castleroom:<style>" or
// "treeroom:<n>", as tools/checks/objects180.mjs names them. Ways, doors and ladders are not offered, and any doorway
// painted inside one comes inert (as in a picture: painted-portal), so a lifted thing is never a way out.
const LIFT_SKIP = /^(way\b|doors?\b|side doors|arch to room|hatch|ladder|house sign|stairs|steps)/;
const LIFT_INERT = / data-(ladder|hole|pit|door|way|house|down|to|leaf)="[^"]*"/g;
const liftPlaces = () => [...PLACES.filter(([k]) => !["none", "castleroom", "treeroom"].includes(k)),
  ...[1, 2, 3, 4, 5, 6].map(r => [`treeroom:${r}`, `🪜 tree house room ${r}`]), ...Object.entries(ROOM_KINDS).map(([st, R]) => [`castleroom:${st}`, R.label])];
const liftPlaceLabel = place => (liftPlaces().find(([k]) => k === place) || [0, place])[1];
// (the place the friend is in, named as a lift place is)
const liftPlaceHere = B => B.kind === "castleroom" ? `castleroom:${(castleOf(B).rooms[Math.round(B.castleRoom ?? 1) - 1] || "king")}` : B.kind === "treeroom" ? `treeroom:${Math.round(B.inside ?? 1)}` : B.kind;
function liftSettings(place) {
  const B = structuredClone(DEFAULTS.background), [kind, sub] = place.split(":");
  if (kind === "castleroom") Object.assign(B, { kind, castle: "N", castleRoomsN: 6, castleRoom6N: sub, castleRoom: 6 });
  else if (kind === "treeroom") Object.assign(B, { kind, houses: 6, inside: +sub });
  else B.kind = kind;
  B.things = [];
  return B;
}
const LIFTED = new Map();
function liftedOf(place) {
  let L = LIFTED.get(place);
  if (L) return L;
  const B = liftSettings(place), objs = new Map();
  try {
    const full = prefixIds(unmarked(backgroundSvg(B.kind, SCENE.left, SCENE.top, -SCENE.left, SCENE.bottom, SCENE.ground, sceneOpts(B))), `L${place.replace(/[^a-z0-9]/gi, "")}-`)
      .replaceAll('class="portal"', 'class="painted-portal"').replace(LIFT_INERT, "");
    const doc = new DOMParser().parseFromString(`<svg xmlns="http://www.w3.org/2000/svg">${full}</svg>`, "image/svg+xml").documentElement;
    const byId = new Map([...doc.querySelectorAll("[id]")].map(e => [e.id, e]));
    for (const g of doc.querySelectorAll("g[data-obj]")) {
      const name = g.getAttribute("data-obj"), z = Number(g.getAttribute("data-z"));
      if (objs.has(name) || z === Infinity || !(Number.isFinite(z) || z === -Infinity) || LIFT_SKIP.test(name) || g.parentElement.closest("[data-obj]")) continue;
      // (the definitions it uses outside itself, and the ones those use)
      const defs = [], seen = new Set(), scan = s => { for (const m of s.matchAll(/url\(#([^)"']+)\)|href="#([^"]+)"/g)) { const id = m[1] || m[2], d = byId.get(id);
        if (d && !seen.has(id) && !g.contains(d)) { seen.add(id); defs.push(d.outerHTML); scan(d.outerHTML); } } };
      const inner = g.innerHTML;
      scan(inner);
      // (ticket 284: a wall or floor thing, at the back, is lifted from the ground line: it keeps its height above it)
      const z0 = Number.isFinite(z) ? z : SCENE.ground;
      objs.set(name, { z: z0, inner, defs: defs.length ? `<defs>${defs.join("")}</defs>` : "", s0: depthSize(B, z0 - SCENE.ground), cx: null, moves: /<animate/.test(inner) });
    }
  } catch (e) { console.warn("📦 could not take things from", place, e); }
  L = { B, objs };
  LIFTED.set(place, L);
  return L;
}
// (its middle, across: measured once, drawn alone in a hidden svg)
function liftCx(o) {
  if (o.cx != null) return o.cx;
  let cx = 0;
  try {
    let m = document.getElementById("lift-measure");
    if (!m) { m = document.createElementNS("http://www.w3.org/2000/svg", "svg"); m.id = "lift-measure"; m.setAttribute("aria-hidden", "true");
      m.setAttribute("style", "position: absolute; left: 0; top: 0; width: 1px; height: 1px; overflow: hidden; visibility: hidden; pointer-events: none"); document.body.append(m); }
    m.innerHTML = o.defs + `<g>${o.inner}</g>`;
    const b = m.lastElementChild.getBBox();
    cx = b.x + b.width / 2;
    m.innerHTML = "";
  } catch (e) {}
  return o.cx = cx;
}
const liftedThing = t => liftedOf(t.from || "").objs.get(t.name) || null;
// (a thing moves, for the guard of ticket 201: by its kind, or a lifted one by what it holds)
const thingMoves = t => t.kind === "lifted" ? !!(liftedThing(t) || {}).moves : !!(THING_KINDS[t.kind] || {}).moving;
// (ticket 284, Matthew: "It would be fun to have placeable switches, animate moving them, and then let the world designer
// wire up what they do -- open and close trap doors --- for a start") A 🎚️ lever on a little stone post, with the trap
// room's own lever (leverSvg) on its front: its handle leans one way when off and the other when on; leverPull turns it
// across. A 🕳️ trap door in the floor: shut, planks over its opening in a rim of broken stone (pitRim); open, the trap
// room's pit (pitSvg), a hole the friend's feet keep out of (data-hole, as a tree room's, drawn in thingSvg in the
// stage's units). Its depth among the objects is its back edge (thingDepth), so a friend standing on it shut is drawn over it.
const LEVER_TILT = 31, LEVER_K = 1.6, LEVER_PIVOT = -96 + 20 * LEVER_K, TRAP_RX = 110, TRAP_RY = 34;
function leverThingSvg(t, o = state.background) {
  const k = LEVER_K, a = t.on ? LEVER_TILT : -LEVER_TILT;
  return `<ellipse cx="0" cy="2" rx="40" ry="9" fill="#000000" opacity="0.18"/>` +
    `<rect x="-26" y="-100" width="52" height="100" rx="4" fill="#6e6e7a"/><rect x="14" y="-100" width="12" height="100" fill="#5a5a66"/><rect x="-26" y="-100" width="52" height="9" rx="3" fill="#82828e"/>` +
    leverSvg(-13 * k, -96, 0, LEVER_PIVOT, 0, LEVER_PIVOT - 44.4 * k, k, s => `<g class="lever-arm" data-pivot="0 ${f1(LEVER_PIVOT)}" transform="rotate(${a} 0 ${f1(LEVER_PIVOT)})">${s}</g>`, knobOf(t), leverShine(o));   // (ticket 317: the place's shine)
}
function trapdoorThingSvg(t, i) {
  const id = `thing-pit-${t.uid || i}`, sh = t.shape || "circle", rx = TRAP_RX, ry = TRAP_RY, seed = t.seed ?? 900 + i;
  const R = (a, b = 0) => { const v = Math.sin((a + seed) * 127.1 + b * 311.7) * 43758.5453; return v - Math.floor(v); };
  // (ticket 323) a boulder dropped into it by a boulder lever: in it (covering the hole, no way down), else none; it falls
  // from high above (boulderFall), its rock the lever's (t.rock: a tone, or a surprise drawn from t.rockSeed)
  const tone = t.rock === "surprise" ? ROCK_TONES[Math.floor(seeded(t.rockSeed ?? 3)(7) * ROCK_TONES.length)] : (t.rock || ROCK_TONES[0]);
  const boulder = t.boulder ? `<g data-boulder="0" data-drop="520">${rockSvg(0, ry * 0.55, rx * 0.8, (t.rockSeed ?? 3) * 7, tone)}</g>` : "";
  return pitLidSvg(0, 0, rx, ry, sh, 1, R, 0, id, !!t.open, t.spikes ?? true) + boulder;   // (ticket 307: as the trap room's pits, its lid swung by lidSwing)
}
// (ticket 368) the placed fence's parts, as the witch's woods draws its palisade: a pointed stake (its foot at (x, y), h tall,
// w wide, its colour by i), a skull's red eyes (steady, as the placed things hold no blink settings of their own), and a run
// of n stakes on a rail, a skull on every fourth from the second, centred on its foot
const stakeSvg = (x, y, h, w, i) => `<polygon points="${f1(x - w / 2 + 1)} ${f1(y)} ${f1(x - w / 2 + 1)} ${f1(y - h)} ${f1(x)} ${f1(y - h - 12)} ${f1(x + w / 2 - 1)} ${f1(y - h)} ${f1(x + w / 2 - 1)} ${f1(y)}" fill="${i % 2 ? "#6b5236" : "#7a5e3e"}" stroke="#2f2216" stroke-width="1.4"/>` +
  `<line x1="${f1(x - 3)}" y1="${f1(y - h * 0.8)}" x2="${f1(x - 2)}" y2="${f1(y - h * 0.3)}" stroke="#4a3622" stroke-width="1.2"/>`;
const skullGlow = (cx, cy, r) => `<circle cx="${f1(cx)}" cy="${f1(cy)}" r="${f1(r * 0.55)}" fill="#ff2626"/>`;
function palisadeSvg(n, skulls, seed) {
  const R = (i, j = 0) => { const v = Math.sin((i + seed) * 127.1 + j * 311.7) * 43758.5453; return v - Math.floor(v); }, step = 22, ph = 70, x0 = -step * (n - 1) / 2;
  let fence = `<ellipse cx="0" cy="2" rx="${f1(step * n * 0.55)}" ry="6" fill="#000000" opacity="0.18"/><rect x="${f1(x0 - step / 2)}" y="${f1(-ph * 0.55)}" width="${f1(step * n)}" height="6" fill="#3a2a1a"/>`, sk = "";
  for (let i = 0; i < n; i++) { const x = x0 + i * step, h = ph * (0.9 + 0.2 * R(i, 50));
    fence += stakeSvg(x, 0, h, step, i);
    if (skulls && i % 4 === 1) sk += drawnBy("skull", stakeSkullSvg(x, -h - 12, 1.3 + 1.2 * R(i, 51), 1, skullGlow)); }
  return drawnBy("fence", fence) + sk;
}
// (a thing's depth among the objects: where its feet are; a trap door's, its back edge)
// (the kinds whose drawings lfoTimed or sceneLfo time: their cache keys carry the LFOs' stamps, thingsLayer)
const LFO_THINGS = new Set(["gem", "puddle", "cascade", "cavemushroom", "chest", "kelp", "seagrass", "seafan", "sealettuce", "campfire", "cauldron",
                            "cow", "pig", "chicken", "barn"]);   // (the farm animals: ticket 504)
const thingDepth = (o, t) => t.kind === "trapdoor" ? t.z - TRAP_RY * depthSize(o, t.z - SCENE.ground) * (t.size ?? 100) / 100 :
  t.kind === "puddle" ? t.z - PUDDLE_RY * depthSize(o, t.z - SCENE.ground) * (t.size ?? 100) / 100 : t.z;   // (ticket 471: one stands in a puddle, in front of its back edge)
const thingLabel = t => t.fallen ? `🪨 fallen ${t.fallen}` : t.kind === "lifted" ? `📦 ${t.name} (${liftPlaceLabel(t.from)})` : (THING_KINDS[t.kind] || { label: t.kind }).label;
// (a thing's drawing, standing at x, its feet at depth z, sized as the friend is there)
// (ticket 206, Matthew, after the Ubuntu laptop: every cauldron's fumes simple, unless its own 💨 fumes
// are upgraded to ✨ fancy, one by one; 0.0.129's "auto" is simple now. Ticket 202's rule, cheap when
// two or more or the computer slow, is gone: a fancy one too many is marked by the guard instead)
const fumesCheap = t => t.fumes !== "fancy";
// (ticket 284) a lifted object, moved so the middle of its foot is at 0, 0, at the size it had at home
function liftedBody(t, at) {
  const L = liftedThing(t);
  if (!L) return at(`<text x="0" y="-10" font-size="40" text-anchor="middle">📦</text>`);   // (its place no longer has it: still here, to take away)
  return at(`<g transform="scale(${(1 / L.s0).toFixed(4)}) translate(${f1(-liftCx(L))} ${f1(-L.z)})">${L.defs}${L.inner}</g>`);
}
function thingSvg(o, t, i) {
  const s = depthSize(o, t.z - SCENE.ground) * (t.size ?? 100) / 100, seed = t.seed ?? 900 + i;
  const at = inner => `<g transform="translate(${f1(t.x)} ${f1(t.z)}) scale(${(t.mirror ? -s : s).toFixed(3)} ${s.toFixed(3)})">${inner}</g>`;
  const shadow = r => `<ellipse cx="0" cy="2" rx="${f1(r)}" ry="${f1(r * 0.22)}" fill="#000000" opacity="0.18"/>`;
  const body = t.kind === "lifted" ? liftedBody(t, at) :
               t.kind === "lever" ? at(leverThingSvg(t, o)) :
               t.kind === "helmet" ? at(helmetSvg(0, 0, 1.4, { plume: t.plume || "#c0392b", side: t.facing === "left" ? -1 : t.facing === "right" ? 1 : 0, back: t.facing === "away" })) :
               t.kind === "trapdoor" ? (t.open && !t.boulder ? `<g data-hole="${f1(t.x - TRAP_RX * s)} ${f1(t.x + TRAP_RX * s)} ${f1(t.z - TRAP_RY * s)} ${f1(t.z + TRAP_RY * s)}">` : "<g>") + at(trapdoorThingSvg(t, i)) + "</g>" :
               t.kind === "rock" ? at(rockSvg(0, 0, 38, seed, t.tone || ROCK_TONES[0])) :
               t.kind === "bush" ? at(bushSvg(0, 0, 70, seed, t.flowers && t.flowers !== "none" ? { flowers: [t.flowers] } : {})) :
               // (a pine's needles are currentColor: the places give theirs a green; with none, it took the page's text colour, white in dark mode: Matthew's laptop)
               t.kind === "pine" ? at(shadow(70) + `<g color="${PINE_GREEN}">${pineTree(seed, { tiers: t.tiers ?? 6 })}</g>`) :
               t.kind === "log" ? at(logSvg(0, -14, 1)) :
               t.kind === "campfire" ? at(campfireHearthSvg(0, 0, 1) + (t.burning ?? true ? `<g pointer-events="none" transform="translate(0 -4)">${campfireFlamesSvg(o, false, "day")}</g>` : "")) :
               t.kind === "bench" ? at(shadow(110) + benchSvg(0, 0, 1)) :
               t.kind === "barrel" ? at(shadow(40) + barrelSvg(0, 0, 2, 0, 0, 1, "#5e3d22")) :
               t.kind === "cauldron" ? at(witchCauldronSvg(0, 0, 1, o, -420, fumesCheap(t))) :
               t.kind === "toadstool" ? at(toadstool(0, 0, 1.3, t.cap || TOADSTOOL_CAPS[0], !!t.glow)) :
               t.kind === "moss" ? at(mossSvg(t.style || "haircap", seed, { on: t.on, color: t.color, lobes: t.lobes ?? true })) :   // (ticket 318)
               t.kind === "lichen" ? at(lichenSvg(t.style || "crust", seed, { on: t.on, color: t.color })) :   // (ticket 319)   // (Matthew: 2.4 was too big)
               t.kind === "potion" ? at(shadow(22) + potionSvg(t.shape || "flask", 0, 0, 2.2, t.color || POTION_COLORS[5])) :
               t.kind === "skeleton" ? at(shadow(60) + skeletonSvg(0, -80 * 2.6, 2.6, "sit")) :
               // (ticket 368: drawn as the witch's woods draws them)
               t.kind === "deadtree" ? at(shadow(50) + drawnBy("deadTree", deadTreeSvg(seed, 0, 0, t.h ?? 300, (t.h ?? 300) > 400 ? 26 : 16, t.color || "#24192f", (t.h ?? 300) > 400 ? 6 : (t.h ?? 300) < 200 ? 2 : 4, t.droop ? 1 : 0, 1, 0, 1))) :
               t.kind === "skullstake" ? at(shadow(14) + stakeSvg(0, 0, 70, 22, 0) + drawnBy("skull", stakeSkullSvg(0, -82, 1.6, (t.eyes ?? 100) / 100, skullGlow))) :
               t.kind === "palisade" ? at(palisadeSvg(t.stakes ?? 6, t.skulls ?? true, seed)) :
               t.kind === "signpost" ? at(`<g transform="scale(0.85)">${signpostSvg(signpostBoards(o, t))}</g>`) :
               t.kind === "sign" ? at(shadow(30) + `<rect x="-5" y="-150" width="10" height="150" fill="#6e4a2c"/>` + plankSign(0, -120, 1.3, xmlText(t.text ?? "hello!"))) :
               // (ticket 219) the sea things
               t.kind === "coral" ? at(coralSvg(seed, t.color || CORAL_COLORS[0])) :
               t.kind === "shell" ? at(seaShellSvg(t.shape || "scallop", t.color || SHELL_COLORS[0])) :
               t.kind === "starfish" ? at(starfishSvg(t.color || STAR_COLORS[0], seed)) :
               t.kind === "seafish" ? at(seaFishSvg(t.color || SEAFISH_COLORS[0], seed, t.bubbles ?? true)) :
               t.kind === "chest" ? at(seaChestSvg(t.open ?? true, t.treasure || "gold", o)) :
               t.kind === "bubblemaker" ? at(shadow(40) + bubbleMakerSvg(t)) :   // (ticket 290)
               t.kind === "anchor" ? at(anchorSvg(t.chain ?? true)) :
               // (ticket 471) the cave things
               t.kind === "gem" ? at(gemThingSvg(t, seed, o)) :
               t.kind === "puddle" ? at(puddleThingSvg(t, seed, o, (SCENE.top - t.z) / Math.max(0.05, s))) :
               t.kind === "cascade" ? at(cascadeThingSvg(t, seed, o)) :
               t.kind === "passage" ? at(passageThingSvg(t, seed)) :
               t.kind === "cavemushroom" ? at(caveMushroomSvg(t, seed, o)) :
               // (ticket 504) the farm animals
               t.kind === "cow" ? at(cowThingSvg(t, seed, o)) :
               t.kind === "pig" ? at(pigThingSvg(t, seed, o)) :
               t.kind === "chicken" ? at(chickenThingSvg(t, seed, o)) :
               // (ticket 501) the desert plants, drawn as the desert draws its own
               t.kind === "saguaro" ? at(saguaroSvg(t, seed)) : t.kind === "pricklypear" ? at(pricklyPearSvg(t, seed)) :
               t.kind === "barrelcactus" ? at(barrelCactusSvg(t, seed)) : t.kind === "agave" ? at(agaveSvg(t, seed)) :
               t.kind === "joshuatree" ? at(joshuaTreeSvg(t, seed)) : t.kind === "ocotillo" ? at(ocotilloSvg(t, seed)) :
               // (ticket 502) the farm things: a fence piece's rails reach the piece beside it (fencePieceSvg)
               t.kind === "barn" ? at(barnThingSvg(t, seed, o)) :
               t.kind === "fencepiece" ? at(fencePieceSvg(o, t, i, s)) :
               ["kelp", "seagrass", "seafan", "sealettuce"].includes(t.kind) ? at(seaPlantSvg(t.kind, t.color || THING_DEFAULTS[t.kind].color, seed, o)) :
               at(shadow(90) + leafyTree(seed, { h: t.h ?? 380, ...(LEAF_SETS[t.leaves] ? { greens: LEAF_SETS[t.leaves] } : {}) }));
  // (ticket 324: a lever marked, so it glows under the pointer as the trap room's do)
  return `<g class="thing" data-drag="thing:${i}"${t.kind === "lever" ? ' data-lever-thing=""' : ""} style="cursor: grab">${body}</g>`;
}
// (ticket 185) The things put in a place, any place: each an object at its
// own depth, so the friend goes behind or in front of it; each its own
// piece, cached on what it holds, so dragging one draws only that one
// again. From draw(), after the place's own (cached) drawing.
function thingsLayer(c) {
  // (a sign with directions also on the names of the squares it points to, which change elsewhere)
  // (ticket 249, Matthew: "rescale the placed objects when I drag the 'smaller going north' slider, including the main char.
  // That's the purpose of this slider -- it does not affect the static parts of the background, but it affects the
  // dynamic player, and placed objects") each sized by depthSize, so drawn again when the 🔭 setting moves its vanishing line
  const vanish = Math.round(10 * sceneVanish(c.o)), T = Array.isArray(c.o.things) ? c.o.things : [];
  // (the review after 0.0.385) and the LFOs a thing's animations are timed by (lfoTimed: the cave things, the chest's sparkles,
  // the sea plants): with them in its key, a thing is drawn again when the music starts or an LFO changes, not left out of time
  // (and only for those: the review of 0.0.386, so an LFO's change draws again only the things it times)
  const lfoKey = T.some(t => LFO_THINGS.has(t.kind)) ? ["fast", "slow", "wings"].map(p => { const L = sceneLfoName(c.o, p); return L ? L + lfoStamp(L) : "own"; }).join() : "";
  // (ticket 502: and a fence piece's neighbours, which its rails reach: fenceLinks)
  const extra = (t, i) => (t.kind === "signpost" ? JSON.stringify(signpostBoards(c.o, t)) : t.kind === "lever" ? [c.o.leverShine, c.o.leverShineColor, c.o.leverFlicker].join() : t.kind === "fencepiece" ? fenceLinks(c.o, i).join() : "") + (LFO_THINGS.has(t.kind) ? "|" + lfoKey : "");   // (ticket 317: and a lever's shine)
  // (ticket 380, ^du_report(12)'s next step 1: with 256 makers the layer cost a part() call each, its thing written out twice over, on
  // every drawing, 0.58 ms in the app's pane) the whole layer kept while nothing it draws from changes: the things, written out once,
  // and what else each reads; on a change each thing still comes from its own piece, so a drag makes one again, not all
  const key = JSON.stringify([c.F, c.o.kind, vanish, T, T.map(extra)]);
  if (key === thingsLayerMemo.key) return thingsLayerMemo.svg;
  const svg = T.map((t, i) => SEP + part("thing-" + i, [c.F, c.o.kind, vanish, t, extra(t, i)], () => obj(t.fallen ? `fallen ${t.fallen}` : `${t.kind === "lifted" ? t.name : t.kind} ${i + 1}`, thingDepth(c.o, t), thingSvg(c.o, t, i)))).join("");
  thingsLayerMemo = { key, svg };
  return svg;
}
let thingsLayerMemo = { key: null, svg: "" };
// (Matthew, 2026-10-17) The crossing's geometry, shared with its ways out in
// the world (worldWaysSvg): its floor reaches far up the picture (far), the
// paths meet a little below its middle (cy), and each path ends where it
// leaves (ends), N and its two neighbours at the far line.
// (ticket 218, Matthew: "align the nav arrow for SW with the trail being draw, favoring the placement of the nav
// arrow and moving the trail SW (and mirrored in the SE direction)") The SW and SE trails go from the meeting place
// through their arrows' spots (worldWaysSvg's AT: the picture's bottom corners), on past its bottom edge, so each
// ends under its arrow; they used to aim at the sides, and on a wide picture left them well above the arrows.
function crossGeom({ x0, x1, y1, g, W, k, pad }) {
  const far = g - 300 * k, cy = g - 190 * k, by = y1 + 60 * k;
  const through = (ax, ay) => { const t = (by - cy) / Math.max(1, ay - cy); return [ax * t, by]; };
  return { far, cy, ends: { N: [0, far + 2 * k], NE: [W * 0.3, far + 2 * k], NW: [-W * 0.3, far + 2 * k], E: [x1 + pad, cy + 14 * k], W: [x0 - pad, cy + 14 * k],
                            SE: through(x1 - 60 * k, y1 - 50 * k), SW: through(x0 + 60 * k, y1 - 50 * k), S: [0, by]} };
}
const CROSS_BACKS = [["woods", "🌲 tall trees and shafts of light"], ["sky", "☁️ sky, sun and clouds over the trees"], ["mountains", "⛰️ mountains far off"]];
function crossingScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o } = c;
  const { far, cy, ends } = crossGeom(c), floor = o.crossFloor || CROSS_FLOORS[0], back = CROSS_BACKS.some(([v]) => v === o.crossBack) ? o.crossBack : "woods";
  const wAt = y => (14 + 190 * Math.max(0, Math.min(1.2, (y - far) / (y1 + 20 * k - far)))) * k;
  // off in the distance (crossBack), all of it above the floor's far line:
  let s = "";
  if (back === "woods") {
    // tall trees, pale trunks under a roof of leaves, the light coming down through them
    s += SEP + part("cross-back", [c.F, back], () => {
      let t = `<defs>${grad("bg-cross-sky", "#cfe9bd", "#f1f6d6", y0, far)}</defs>` + all("url(#bg-cross-sky)"), trees = "";
      for (let i = 0; i < 30; i++) {
        const x = x0 + W * (i + R(i, 501)) / 30, w = (5 + 6 * R(i, 502)) * k;
        trees += `<rect x="${f1(x - w / 2)}" y="${f1(y0 - pad)}" width="${f1(w)}" height="${f1(far + 8 * k - y0 + pad)}" fill="#857560" opacity="0.65"/>`;
      }
      trees += `<rect x="${f1(x0 - pad)}" y="${f1(y0 - pad)}" width="${f1(W + 2 * pad)}" height="${f1(pad + H * 0.05)}" fill="#4a8650"/>`;
      for (let i = 0; i < 32; i++) {
        const x = x0 - 30 * k + (W + 60 * k) * i / 31, y = y0 + H * (0.03 + 0.07 * R(i, 503)), z = (40 + 30 * R(i, 504)) * k / 80;
        trees += padSvg(Math.floor(R(i, 505) * 8), x, y, z * (R(i, 506) < 0.5 ? -1 : 1), z * 1.2, ["#5c9c63", "#4a8650", "#68a468"][i % 3]);
      }
      return t + obj("far trees", far - 6 * k, trees);
    });
    s += SEP + part("cross-beams", [c.F, sceneLfoKey(o, "slow")], () => obj("sunbeams", far - 6 * k, sunbeamsSvg("bg-cross-shaft", x0, W, k, y0 - 20 * k, far + 20 * k, 4, 91, -0.3, o)));
  } else if (back === "sky") {
    // as at the forest clearing: the sky, the sun, drifting clouds, and the trees far off, each its own
    s += SEP + part("cross-back", [c.F, back], () => `<defs>${grad("bg-cross-sky", "#6ec2f2", "#e8f6ff", y0, far)}</defs>` + all("url(#bg-cross-sky)") +
      sunSvg(x1 - W * 0.16, y0 + H * 0.12, 26 * k) + land(far - 18 * k, 5, "#9ccf86", 0.4));
    s += SEP + cloudsSvg(c, 3, 0.7, 520);
    s += SEP + part("cross-far-trees", [c.F, back], () => {
      let t = "";
      for (let i = 0; i < 18; i++) {
        const x = x0 + W * (i + 0.2 + 0.6 * R(i, 521)) / 18, y = far + 4 * k * R(i, 522), sc = k * (0.2 + 0.08 * R(i, 523));
        t += i % 3 === 1 ? use("bg-cross-oak", x, y, sc) : use("bg-cross-pine", x, y, sc, sc, ` color="${["#2f6e3a", "#3a7d44", "#285e33"][i % 3]}"`);
      }
      return `<defs>${oakDef("bg-cross-oak", 1)}${pineDef("bg-cross-pine", 7)}</defs>` + obj("far woods", far, t);
    });
  } else {
    // as at the camp site and on the mountain trail: the sky, the sun, clouds high up, mountains far off, snow on the far ones, and far pines
    s += SEP + part("cross-back", [c.F, back], () => `<defs>${grad("bg-cross-sky", "#4f9fe0", "#e2f2ff", y0, far)}${cloudDef("bg-cloud", 1)}</defs>` + all("url(#bg-cross-sky)") +
      `<circle cx="${f1(x1 - W * 0.14)}" cy="${f1(y0 + H * 0.1)}" r="${f1(24 * k)}" fill="#fff6c8"/>`);
    s += SEP + part("cross-clouds", [c.F, back, sceneLfoKey(o, "slow")], () => [0, 1, 2].map(i => driftingCloud(x0 + W * (0.15 + 0.32 * i), y0 + H * (0.05 + 0.04 * (i % 2)), k * 0.5, i, o)).join(""));
    s += SEP + part("cross-peaks", [c.F, back], () => {
      let t = "";
      [[0, "#b6c6dc", 190, 9, true], [1, "#8ea3c0", 140, 7, true], [2, "#6d8296", 95, 6, false]].forEach(([r, fill, hgt, n, snow]) => {
        const base = far + r * 6 * k, pts = [[x0 - pad, base]];
        for (let i = 0; i <= n; i++) {
          const x = x0 - 60 * k + (W + 120 * k) * i / n + 30 * k * (R(i + r * 10, 523) - 0.5);
          pts.push([x - (W / n) * 0.45, base - hgt * k * (0.35 + 0.3 * R(i + r * 10, 524))], [x, base - hgt * k * (0.75 + 0.25 * R(i + r * 10, 525))]);
        }
        pts.push([x1 + pad, base], [x1 + pad, base + 30 * k], [x0 - pad, base + 30 * k]);
        t += `<polygon points="${pts.map(([x, y]) => P2(x, y)).join(" ")}" fill="${fill}"/>`;
        if (snow) for (let i = 2; i < pts.length - 3; i += 2) {
          const [px, py] = pts[i], [lx, ly] = pts[i - 1], [rx, ry] = pts[i + 1];
          t += snowCap(px, py, lx, ly, rx, ry, 0.24 * (base - py), i + r * 20);
        }
      });
      let row = "";
      for (let i = 0; i < 26; i++) row += use("bg-pine", x0 - 20 * k + (W + 40 * k) * i / 25, far + 14 * k + 4 * k * R(i, 526), k * (0.22 + 0.1 * R(i, 527)), k * (0.22 + 0.1 * R(i, 527)), ` color="${["#2f6e3a", "#3a7d44"][i % 2]}"`);
      return t + `<defs>${PINE_DEF}</defs>` + obj("far pines", far + 14 * k, row);
    });
  }
  // the forest floor, lighter far off, and its leaf litter
  s += SEP + part("cross-floor", [c.F, floor, far], () => {
    let t = `<defs>${grad("bg-cross-floor", mix(floor, "#ffffff", 0.18), floor, far, y1)}</defs>` + land(far, 5, "url(#bg-cross-floor)", 0.4);
    for (let i = 0; i < 60; i++) {
      const u = R(i, 507), y = far + (y1 - far) * u * u, x = x0 + W * R(i, 508), r = (2 + 4 * u) * k;
      t += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(r * 1.6)}" ry="${f1(r * 0.6)}" fill="${mix(floor, i % 2 ? "#3a2a10" : "#c8b060", 0.35)}" opacity="0.7"/>`;
    }
    return t;
  });
  // the paths: each from the meeting place to where it leaves, as wide as
  // its depth makes it; the meeting place itself, a worn patch
  // (in a world square, only the paths that lead somewhere: crossLive, set by worldKeep; else the ones switched on)
  const on = typeof o.crossLive === "string" ? o.crossLive.split(",").filter(Boolean) : CROSS_DIRS.map(([d]) => d).filter(d => o["cross" + d]);
  s += SEP + part("cross-paths", [c.F, on.join(",")], () => {
    let t = "";
    for (const d of on) {
      const [ex, ey] = ends[d], dx = ex, dy = ey - cy, L = Math.hypot(dx, dy) || 1, nx = -dy / L, ny = dx / L;
      const a = wAt(cy) * 0.5, b = wAt(Math.min(ey, y1 + 20 * k)) * 0.5;
      // (no outline: the paths overlap where they meet, and outlines drew seams there; Matthew, 2026-10-17)
      t += `<path d="M${f1(nx * a)} ${f1(cy + ny * a)} L${f1(ex + nx * b)} ${f1(ey + ny * b)} L${f1(ex - nx * b)} ${f1(ey - ny * b)} L${f1(-nx * a)} ${f1(cy - ny * a)} Z" fill="${CROSS_PATH}"/>`;
      for (let i = 0; i < 9; i++) {
        const u = (i + 0.5) / 9, px = ex * u + (R(i, 509 + d.length) - 0.5) * 0.6 * (a + (b - a) * u) * nx, py = cy + dy * u + (R(i, 510) - 0.5) * 0.3 * (a + (b - a) * u);
        t += `<ellipse cx="${f1(px)}" cy="${f1(py)}" rx="${f1((3 + 3 * u) * k)}" ry="${f1((1.6 + 1.4 * u) * k)}" fill="${CROSS_PATH_EDGE}" opacity="0.55"/>`;
      }
    }
    if (on.length) t += `<ellipse cx="0" cy="${f1(cy)}" rx="${f1(wAt(cy) * 0.95)}" ry="${f1(wAt(cy) * 0.36)}" fill="${CROSS_PATH}"/>`;
    return t;
  });
  // butterflies, as on the path through the woods
  s += SEP + part("cross-butterflies", [c.F, sceneLfoKey(o, "fast"), sceneLfoKey(o, "wings")], () =>
    obj("butterflies", g - 50 * k, butterfliesSvg(c, [[x0 + W * 0.22, g - 60 * k, 1], [x0 + W * 0.74, g - 100 * k, 0.8]])));
  return s;
}
const SCENES = { hut: hutScene, hutroom: hutroomScene, hutbed: hutbedScene, hutpotion: hutpotionScene, hutcenter: hutcenterScene, lake: lakeScene, house: houseScene, treehouse: treehouseScene, treetop: treetopScene, livingroom: livingroomScene, treeroom: treeroomScene,
                 witchroom: witchroomScene, healerroom: healerroomScene, fairyforest: fairyforestScene, clearing: clearingScene, mountains: mountainsScene, waterfall: waterfallScene, campsite: campsiteScene,
                 tent: tentScene, cave: caveScene, caveroom: caveroomScene, farmyard: farmyardScene, desert: desertScene, woodpath: woodpathScene, pasture: pastureScene, crossing: crossingScene, castle: castleScene, castleroom: castleroomScene, imagination: imaginationScene, emptyroom: emptyroomScene, bubblespace: bubblespaceScene, bubbleroom: bubbleroomScene };
// the beach's sea, from the horizon (sea) to the shore, where the foam and
// then the sand begin; and the cemetery's brook, at t from 0 (on the hills)
// to 1 (past the frame's foot): its middle, its height and half its width
// (for their drawings, and for a friend wading in them: WATER, ticket 122)
const beachLines = ({ g, k }) => ({ sea: g - 170 * k, shore: g - 55 * k });
// (ticket 186) open water's horizon: higher than the beach's, all the rest sea
const seaHorizon = ({ g, k }) => g - 230 * k;
// (ticket 212, Matthew: "having the option to display a shoreline, or treeline, in the distance, would be a good set of parameters to
// make the 'open water' more useful in more situations": "different forest styles, maybe 2 or 3. default is off"; "different mountain
// styles, maybe 2 or 3. default is off"; "forest and mountain styles layer and combine naturally"; "optional beach, drawn between the
// horizon and the forest") A distant shore, as backdrop layers on a horizon any place can use: back to front, mountains, a forest, a
// beach, each standing on the line in front of it, all scaled by the shore's size (ticket 213's "distant shore size": bigger, nearer).
// Silhouettes, a path or two each, drawn once (the place holds still: 60 frames a second first). farLayers(c, base, look) draws them
// and gives each its object (wallObj, so 👁️ hides it and 🔬 names it); look = { mountains, forest, beach, size }.
const SHORE_MOUNTAINS = [["none", "🚫 none"], ["snowy", "🏔️ snowy peaks"], ["blue", "⛰️ a blue range"], ["hills", "🌄 rolling hills"], ["mesas", "🏜️ red mesas"]];   // (mesas: ticket 501)
const SHORE_FORESTS = [["none", "🚫 none"], ["pines", "🌲 pines"], ["broadleaf", "🌳 round trees"], ["mixed", "🌲🌳 mixed"]];
function farLayers(c, base, look) {
  const { x0, x1, W, k, pad, R, P } = c, u = k * Math.max(0.25, Math.min(3, (look.size ?? 100) / 100)), L = x0 - pad, Rt = x1 + pad;
  const mountains = SHORE_MOUNTAINS.some(([v]) => v === look.mountains) ? look.mountains : "none", forest = SHORE_FORESTS.some(([v]) => v === look.forest) ? look.forest : "none";
  const bh = look.beach ? 7 * u : 0, treeFoot = base - bh * 0.55, hillFoot = (forest !== "none" ? treeFoot - 6 * u : base - bh) + 1;
  let s = "";
  // ⛰️ mountains: snowy peaks (two ranges, snow on the far one), a blue range (one, hazy), rolling hills (two, rounded)
  if (mountains !== "none") {
    let m = "";
    const range = (row, fill, hgt, n, seed, round) => {
      const foot = hillFoot - row * 5 * u, pts = [];
      let d = `M${P(L, foot)}`;
      for (let i = 0; i <= n; i++) {
        const x = x0 - 40 * u + (W + 80 * u) * i / n + (W / n) * 0.3 * (R(i + row * 17, seed) - 0.5), top = foot - hgt * u * (0.55 + 0.45 * R(i + row * 17, seed + 1));
        pts.push([x, top]);
        d += round ? ` Q${P(x - W / n / 2, top - hgt * u * 0.25)} ${P(x, top)}` : ` L${P(x - (W / n) * 0.42, foot - hgt * u * (0.25 + 0.2 * R(i + row * 17, seed + 2)))} L${P(x, top)}`;
      }
      const under = Math.min(foot + 8 * u, base);   // (never below the water line: the sea is in front, and 🔬 measures the layer by it)
      m += `<path d="${d} L${P(Rt, foot)} L${P(Rt, under)} L${P(L, under)} Z" fill="${fill}"/>`;
      return pts;
    };
    if (mountains === "snowy") {
      const far = range(1, "#b6c6dc", 70, 7, 611, false); range(0, "#8ea3c0", 48, 9, 621, false);
      for (const [x, y] of far) m += `<path d="M${P(x, y)} l${f1(-9 * u)} ${f1(13 * u)} l${f1(5 * u)} ${f1(-3 * u)} l${f1(4 * u)} ${f1(4 * u)} l${f1(4 * u)} ${f1(-4 * u)} l${f1(5 * u)} ${f1(3 * u)} Z" fill="#f4f8ff"/>`;
    } else if (mountains === "blue") range(0, "#9fb4d4", 58, 8, 631, false);
    else if (mountains === "mesas") {
      // (ticket 501, for M, the desert's far off) 🏜️ red mesas: flat tops, steep sides over slopes of fallen rock, two rows,
      // the far one hazier; each its own shape (so they may overlap), then the ground they stand on, all one path a row
      for (const [row, fill, hgt, n, seed] of [[1, "#c6a3a0", 46, 4, 681], [0, "#b38377", 32, 5, 691]]) {
        // (ticket 501: each row on or above the horizon, its mesas' feet no lower than the ground they stand on: the near row,
        // on the horizon itself with nothing in front, went 2 below it, over a band wound the other way: a pale seam at its foot)
        const foot = Math.min(hillFoot - row * 5 * u, base), under = Math.min(foot + 8 * u, base), low = Math.min(foot + 1, under);
        let d = under > foot ? `M${P(L, foot)} L${P(Rt, foot)} L${P(Rt, under)} L${P(L, under)} Z` : "";
        for (let i = 0; i < n; i++) {
          const j = i + row * 17, x = x0 + W * (i + 0.5) / n + (W / n) * 0.3 * (R(j, seed) - 0.5), w = (W / n) * (0.1 + 0.16 * R(j, seed + 1)), h = hgt * u * (0.6 + 0.4 * R(j, seed + 2)), sl = Math.min(8 * u, w * 0.6);
          d += ` M${P(x - w - 2.2 * sl, low)} L${P(x - w - sl, foot - h * 0.4)} L${P(x - w, foot - h)} L${P(x + w, foot - h + 2 * u * (R(j, seed + 3) - 0.5))} L${P(x + w + sl, foot - h * 0.45)} L${P(x + w + 2.4 * sl, low)} Z`;
        }
        m += `<path d="${d}" fill="${fill}"/>`;
      }
    } else { range(1, "#9db896", 34, 6, 641, true); range(0, "#7fa47c", 24, 8, 651, true); }
    s += wallObj("far mountains", m);
  }
  // 🌲 a forest: a pale far band and a dark near one, its tops pine points, round crowns, or both by turns
  if (forest !== "none") {
    let f = "";
    const band = (row, fill, hgt, n, seed) => {
      const foot = treeFoot - row * 3 * u, w = (W + 2 * pad) / n;
      let d = `M${P(L, foot)}`;
      for (let i = 0; i < n; i++) {
        const x = L + w * (i + 0.5) + w * 0.3 * (R(i + row * 31, seed) - 0.5), h = hgt * u * (0.7 + 0.3 * R(i + row * 31, seed + 1));
        const pine = forest === "pines" || (forest === "mixed" && R(i + row * 31, seed + 2) < 0.5);
        d += pine ? ` L${P(x - w * 0.48, foot - h * 0.3)} L${P(x, foot - h)} L${P(x + w * 0.48, foot - h * 0.3)}`
                  : ` Q${P(x - w * 0.62, foot - h * 1.05)} ${P(x, foot - h)} Q${P(x + w * 0.62, foot - h * 1.05)} ${P(x + w * 0.5, foot - h * 0.45)}`;
      }
      const under = Math.min(foot + 4 * u, base);
      f += `<path d="${d} L${P(Rt, foot)} L${P(Rt, under)} L${P(L, under)} Z" fill="${fill}"/>`;
    };
    const n = Math.max(24, Math.round((W + 2 * pad) / (9 * u)));
    band(1, "#5e8f6b", 15, n, 661); band(0, "#2f6b3a", 12, Math.round(n * 1.15), 671);
    s += wallObj("far forest", f);
  }
  // 🏖️ a beach: sand from the forest's foot down to the water, the foam at its edge
  if (look.beach) {
    let d = `M${P(L, base - bh)}`;
    for (let i = 0; i <= 16; i++) { const x = L + (W + 2 * pad) * i / 16; d += ` Q${P(x - (W + 2 * pad) / 32, base - bh - 1.5 * u * Math.sin(i * 1.3))} ${P(x, base - bh)}`; }
    s += wallObj("far beach", `<path d="${d} L${P(Rt, base)} L${P(L, base)} Z" fill="#ecd9a8"/><line x1="${f1(L)}" y1="${f1(base - 0.6 * u)}" x2="${f1(Rt)}" y2="${f1(base - 0.6 * u)}" stroke="#ffffff" stroke-width="${f1(1.2 * u)}" opacity="0.85"/>`);
  }
  return s;
}
const brookAt = ({ W, g, k, y1 }) => t => { const y = g - 62 * k + t * (y1 + 40 * k - (g - 62 * k));
  return [W * (0.2 + 0.2 * t) + W * 0.05 * Math.sin(t * Math.PI * 1.6), y, (8 + 70 * t * t) * k]; };
// (ticket 50) seats in the places backgroundSvg draws itself, as it draws
// them. Under the sea, the picnic bench: its plank's top (34 above the
// table's foot), between the sharks at its ends (their bodies come in to
// 98 of its 120 each side); one sits with their back to the table, facing us.
SEATS.underwater = (c, B) => { const { x1, W, g, k } = c, x = Math.max(x1 - W * 0.22, 230 * k), y = g + 30 * k;
  return [{ name: "picnic bench", x0: x - 98 * k, x1: x + 98 * k, y: y - 34 * k, z: y + 1, facing: ["front"] }]; };
// At the beach, the beach chairs (low): each one's seat, 68 across at its
// front edge, the hips 28 above the chair's feet, between the seat's back
// edge (34 up) and its front edge (18 up).
SEATS.beach = (c, B) => { const { W, g, k } = c;
  return [-1, 1].map(sd => { const x = sd * Math.max(W / 2 - 210 * k, 150 * k), b = g + 20 * k, cx = x + sd * 30 * k, cy = b - 10 * k;
    return { name: `beach chair, ${sd < 0 ? "left" : "right"}`, x0: cx - 34 * k, x1: cx + 34 * k, y: cy - 28 * k, z: cy + 1, facing: ["front"] }; })
    // (ticket 299) its blankets, to lie on, flat on the sand (where the beach draws them: the middle of each, its head toward
    // the sea's middle): not in anyone's way (flat)
    .concat([[-1, 70], [1, 95]].map(([sd, dy]) => { const x = sd * W * 0.2, y = g + dy * k - 21 * k, w = 150 * k;
      return { name: `beach blanket, ${sd < 0 ? "left" : "right"}`, x0: x - w / 2 + 7 * k, x1: x + w / 2 + 7 * k, y, z: y + 1, facing: ["front"], lie: true, flat: true, head: sd < 0 ? "right" : "left" }; })); };
// In the witch's woods, the giant tree's thickest root (the first of its
// humped roots, [30, 230, 30, 15]), where it humps up out of the ground
// beside the trunk on its way toward the middle: the top of its stroke
// over the highest stretch of its curve (sampled; within 3 of the top).
SEATS.witch = (c, B) => { const { x0, W, g, k } = c, x = x0 + W * 0.1, b = g + 40 * k;
  const [dx, reach, dy, rw] = [30, 230, 30, 15], sx = x + dx * k, ex = x + reach * k, ey = b + dy * k, hump = (18 + 14 * sceneR(0, 80)) * k;
  const m1 = sx + (ex - sx) * 0.35, m2 = sx + (ex - sx) * 0.7;
  const at = (t, p0, p1, p2, p3) => (1 - t) ** 3 * p0 + 3 * (1 - t) ** 2 * t * p1 + 3 * (1 - t) * t * t * p2 + t ** 3 * p3;
  const pts = [...Array(41)].map((_, i) => [at(i / 40, sx, m1, m2, ex), at(i / 40, b - 14 * k, b - 14 * k - hump, ey - hump * 0.6, ey)]);
  const top = Math.min(...pts.map(p => p[1])), high = pts.filter(p => p[1] <= top + 3 * k);
  return [{ name: "big root", x0: high[0][0], x1: high[high.length - 1][0], y: top - (rw + 3) * k / 2, z: b + 1, facing: ["front", "left", "right"] }]; };
