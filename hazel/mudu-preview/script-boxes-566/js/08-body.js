"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 8 of 26 of the game's script, run in order by index.html.

// ===================== THE BODY =====================
// Body units: the collar's top at y = -20, centered on x = 0. Every part
// is placed by an anchor of the part before it: the neck on the collar,
// the head's chin on the neck, hands at the ends of the arms, the pants'
// hips under the shirt's, and shoes on the pants' feet.

const SHOES = {
  sneaker: [[-2,0],[12,0],[14,12],[40,16],[44,26],[-12,26],[-14,10]],
  boot:    [[-12,-34],[12,-34],[13,8],[38,12],[42,26],[-13,26]],
  clown:   [[-10,0],[10,0],[14,8],[62,4],[74,16],[64,30],[-14,30],[-18,14]],
  // a witch's: an ankle boot whose pointed toe curls up (with a strap and
  // a buckle: shoeSvg)
  witchboot: [[-12,-30],[12,-30],[13,6],[32,12],[48,4],[54,-2],[50,12],[40,26],[-13,26]],
  // (ticket 522) a cowboy boot: a tall shaft, a pointed toe and a block heel (the arch cut up between); in shoeSvg a star on
  // the shaft (sh.star) and a spur, a toy star disc behind the heel (sh.spur: no shank, no points)
  cowboot: [[-12,-40],[12,-40],[12,4],[30,10],[48,21],[45,26],[8,26],[4,21],[-3,21],[-3,26],[-14,26],[-13,4]],
};
// BARE FEET WITH TOES, as the hands have fingers: in the foot's frame
// (x outward from the ankle, y down, in shoe units), a sole and toes,
// each toe a rounded stroke from (x1, y1) to (x2, y2), `w` its half-width,
// an optional round pad `tip`, and for talons a hooked claw at the end.
// Outline layer first, then fill, so toes and sole merge into one shape.
const TOED = {
  // a foot from the side, five toes along its front, the big toe first
  toes5: { sole: [14, 16, 20, 10], toes: [
    [28, 9, 42, 6.5, 3.2], [29, 13, 43, 12.5, 2.8], [29, 16.5, 41.5, 18, 2.5],
    [28, 20, 39, 23, 2.3], [26, 23, 35, 27, 2.1]] },
  // a small sole and four long, thin toes fanned forward, round pads
  alien4: { sole: [6, 14, 14, 10], tip: 4.2, toes: [
    [8, 12, 40, -2, 2.4], [8, 13, 46, 10, 2.4], [8, 14, 44, 21, 2.4], [8, 15, 36, 27, 2.4]] },
  // three very long toes with big round pads, like a tree frog's
  alien3: { sole: [6, 14, 14, 10], tip: 6, toes: [
    [8, 12, 46, 0, 2.6], [8, 14, 54, 14, 2.6], [8, 16, 44, 24, 2.6]] },
  // a bird's (or a raptor's) foot: three toes forward, one back, talons
  talons: { sole: [4, 16, 10, 8], talon: true, toes: [
    [6, 15, 34, 6, 3.6], [6, 17, 40, 18, 3.6], [6, 19, 32, 26, 3.4], [2, 18, -14, 24, 3.2]] },
};
// How far each kind of foot reaches out from the ankle, in shoe units,
// so the picture's frame can hold big feet (and big shoes).
const FOOT_REACH = { sneaker: 46, boot: 44, clown: 76, bare: 36, paw: 40, hoof: 34, witchboot: 58, cowboot: 52, ninjaboot: 48,
  "fairy-slipper": 60, "fairy-petal": 52, "fairy-leaf": 60, "fairy-sparkle": 48, "fairy-bare": 36,
                     toes5: 44, alien4: 52, alien3: 62, talons: 56 };
// A human foot with n toes (1 to 100), as the hands have n fingers: the
// toes along the front of the foot, the big toe first and longest; past
// five they fan further round it, thinner, to a fringe at 100. Five toes
// are the foot drawn above.
function humanToes(n) {
  n = Math.max(1, Math.min(100, Math.round(n)));
  if (n === 5) return TOED.toes5;
  const lo = n <= 5 ? -45 : -45 - Math.min(110, (n - 5) * 3), hi = n <= 5 ? 50 : 50 + Math.min(110, (n - 5) * 3);
  const toes = [];
  for (let i = 0; i < n; i++) {
    const t = n > 1 ? i / (n - 1) : 0.25, th = (lo + (hi - lo) * t) * Math.PI / 180, d = th * (n <= 5 ? 0.45 : 0.7);
    const bx = 14 + 16 * Math.cos(th), by = 16 + 8 * Math.sin(th), len = 14 - 4 * t;
    const w = Math.max(0.45, (3.2 - 1.1 * t) * Math.pow(Math.min(1, 5 / n), 0.8));
    toes.push([f1(bx), f1(by), f1(bx + len * Math.cos(d)), f1(by + len * Math.sin(d)), +w.toFixed(2)]);
  }
  return { sole: [14, 16, 20, 10], toes };
}
// An alien foot with n long thin toes (1 to 100), round pads at their
// ends, fanned forward from a small sole, wider as there are more; three
// and four are the feet drawn above.
function alienToes(n) {
  n = Math.max(1, Math.min(100, Math.round(n)));
  if (n === 3) return TOED.alien3;
  if (n === 4) return TOED.alien4;
  const half = n <= 1 ? 0 : Math.min(170, 20 + (n - 3) * 6), step = n > 1 ? 2 * half / (n - 1) : 0;
  const chord = Math.sin(Math.max(step, 8) * Math.PI / 360) * 40;
  const w = Math.max(0.5, Math.min(2.6, chord * 0.35)), tip = Math.max(1, Math.min(6, chord * 0.6)), toes = [];
  for (let i = 0; i < n; i++) {
    const a = (n > 1 ? -half + i * step : 0) * Math.PI / 180, len = 44 - 8 * Math.abs(n > 1 ? (i / (n - 1)) * 2 - 1 : 0);
    toes.push([8, 14, f1(8 + len * Math.cos(a)), f1(14 + len * Math.sin(a)), +w.toFixed(2)]);
  }
  return { sole: [6, 14, 14, 10], tip, toes };
}
// A bird's foot with n talons (1 to 100): one toe back and the rest
// forward, fanned wider and thinner as there are more, each with its hooked
// claw; four are the foot drawn above (three forward, one back).
function talonToes(n) {
  n = Math.max(1, Math.min(100, Math.round(n)));
  if (n === 4) return TOED.talons;
  const k = n >= 2 ? n - 1 : 1, half = k <= 1 ? 0 : Math.min(150, 17 + Math.max(0, k - 3) * 5), step = k > 1 ? 2 * half / (k - 1) : 0;
  const w = Math.max(0.6, Math.min(3.6, Math.sin(Math.max(step, 6) * Math.PI / 360) * 32 * 0.75)), toes = [];
  for (let i = 0; i < k; i++) {
    const u = k > 1 ? i / (k - 1) : 0.5, a = (-half + 2 * half * u) * Math.PI / 180, len = 34 - 7 * Math.abs(2 * u - 1);
    toes.push([6, 17, +(6 + len * Math.cos(a)).toFixed(1), +(17 + len * Math.sin(a)).toFixed(1), +w.toFixed(2)]);   // (numbers: a talon's claw is reckoned from them)
  }
  if (n >= 2) toes.push([2, 18, -14, 24, Math.min(3.2, w + 0.4)]);
  return { sole: [4, 16, 10, 8], talon: true, toes };
}
// what a foot's toes are, if it has toes
const toesOf = sh => sh.style === "human" ? humanToes(sh.toes ?? 5) : sh.style === "alien" ? alienToes(sh.alienToes ?? 3)
                   : sh.style === "talons" ? talonToes(sh.talonCount ?? 4) : TOED[sh.style];
// the feet whose toes are counted (their reach follows the count)
const COUNTED_FEET = { human: 1, alien: 1, talons: 1 };
// how far a foot reaches out from the ankle (toes counted, and a talon's
// claw past its toe), in shoe units
const footReach = sh => {
  const T = COUNTED_FEET[sh.style] && toesOf(sh);
  return T ? Math.max(...T.toes.map(([, , x2, , w]) => +x2 + w + (T.tip || 0) + (T.talon ? 13 : 0))) : FOOT_REACH[sh.style] ?? 76;
};
// and how far its toes reach down below the ankle (many toes fan round
// the foot, past its sole), in shoe units
const footDown = sh => {
  const T = COUNTED_FEET[sh.style] && toesOf(sh);
  return T ? Math.max(...T.toes.map(([, , , y2, w]) => +y2 + w + (T.tip || 0) + (T.talon ? 13 : 0))) : 0;
};
function toedFoot(x, y, side, sc, skin, T) {
  const st = darker(skin), lw = 2.5 / sc;
  const [sx, sy, srx, sry] = T.sole;
  const layer = (fill, grow) =>
    `<ellipse cx="${sx}" cy="${sy}" rx="${f1(srx + grow)}" ry="${f1(sry + grow)}" fill="${fill}"/>` +
    T.toes.map(([x1, y1, x2, y2, w]) =>
      `<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${fill}" stroke-width="${f1(2 * (w + grow))}" stroke-linecap="round"/>` +
      (T.tip ? `<circle cx="${x2}" cy="${y2}" r="${f1(T.tip + grow)}" fill="${fill}"/>` : "")).join("");
  // a talon: a curved point leaving the toe's end, hooking downward
  const talon = ([x1, y1, x2, y2, w]) => {
    const L = Math.hypot(x2 - x1, y2 - y1), dx = (x2 - x1) / L, dy = (y2 - y1) / L, nx = -dy, ny = dx;
    const P = (a, c) => `${f1(x2 + dx * a + nx * c)} ${f1(y2 + dy * a + ny * c)}`;
    const k = Math.sign(dx) || 1;                        // hook toward the ground
    return `<path d="M${P(0, -w)} Q${P(9, -w)} ${P(13, k * 6)} Q${P(5, w * 0.8)} ${P(0, w)} Z" ` +
           `fill="#f4ecd6" stroke="#5c4a2e" stroke-width="${f1(1.8 / sc)}" stroke-linejoin="round"/>`;
  };
  return `<g transform="translate(${f1(x)} ${f1(y)}) scale(${(side * sc).toFixed(3)} ${sc.toFixed(3)})">` +
    (T.talon ? T.toes.map(talon).join("") : "") + layer(st, lw) + layer(skin, 0) + `</g>`;
}
// A fairy's wings, behind her: for each side, from her back (just inside
// the shoulder) out and up; in their style and color, fluttering (a slow
// beat, both together) unless that is off
// (ticket 274, Matthew: "the .json file should be self-determined, in the sense that it fully determines the .svg
// output, and we get the one every time") The picture's clock, in seconds. Every animation phase drawn into the
// picture (a CSS delay here, a SMIL begin in lfoBegin) and every step of bubble space's bubbles reads it, never
// performance.now() itself. In play it is the page's clock; held (holdStill(t)), it stands at t, the picture's own
// SVG clock with it, and the bubbles stay where their seed put them, so a save drawn held at 0 is the same SVG every
// time. The picture archive draws every picture so; play never holds it.
let stillAt = null;
const picClock = () => stillAt ?? performance.now() / 1000;
function holdStill(t = 0) {
  stillAt = t; picGap = null;
  const p = document.getElementById("pic"); if (p && p.pauseAnimations) { p.pauseAnimations(); p.setCurrentTime(t); }
}
// how far into its cycle (of `dur` seconds, begun `begin` seconds in) a CSS
// animation is now, as the negative delay that starts it there
const fairyPhase = (dur, begin = 0) => (-(((picClock() - begin) % dur + dur) % dur)).toFixed(3);
// A flap with an LFO: the wing swings out as the LFO's wave rises, farther
// the deeper it is (8 degrees at no depth, 15 at the mixer's usual depth,
// 36 at full depth: more than the usual flap's 9, to beat with the music),
// once per LFO cycle, in time with the music. Its keyframes, made for the wave
// and depth (one set per side), go in the picture with it.
const wingLfoAngle = (lf, sd, u) => -sd * (8 + 28 * lf.amp) * (1 + lfoWave(lf.shape, u)) / 2;
function wingLfoFlap(lf, sd, shift = 0) {
  const name = `fairy-lfo-${sd < 0 ? "l" : "r"}-${lf.shape}-${Math.round(lf.amp * 100)}`, N = 16, stops = [];
  for (let i = 0; i <= N; i++) stops.push(`${(i / N * 100).toFixed(2)}% { transform: rotate(${wingLfoAngle(lf, sd, i / N).toFixed(2)}deg); }`);
  return { css: `@keyframes ${name} { ${stops.join(" ")} }`,
           style: `transform-origin: 0 0; animation: ${name} ${lf.dur.toFixed(4)}s linear ${(-(lf.phase + shift) * lf.dur).toFixed(4)}s infinite` };
}
// An LFO going round faster than the screen can show (a cycle in under
// about four frames) is drawn as the eye sees fast wings: a blur of K see-
// through copies of the wing, each flapping at the LFO's own speed, each an
// eighth of a cycle behind the last. Whenever the screen draws a frame it
// catches the K of them spread evenly round the cycle, so the blur covers
// the whole sweep, thickest where the wave lingers (a sine's ends) and
// nearly solid at the shoulder, where every copy overlaps; and as where it
// catches them shifts from frame to frame, the blur shimmers and moves. A
// fade that fast holds at its average instead (see fairyHouseFade and veil).
const LFO_BLUR = 1 / 15;
function wingLfoBlur(lf, sd, ax, ay, t) {
  const K = 8, op = (1 - Math.pow(0.1, 1 / K)).toFixed(3);
  return { css: wingLfoFlap(lf, sd).css, svg: `<g transform="translate(${ax} ${ay})">` + [...Array(K)].map((_, i) =>
    `<g style="${wingLfoFlap(lf, sd, i / K).style}" opacity="${op}"><g transform="translate(${-ax} ${-ay})">${t}</g></g>`).join("") + `</g>` };
}
// a wing's markings (veins, spots, a shine), kept inside the wing's outline d:
// drawn a little long and clipped, every vein ends exactly at the wing's edge
let wingClips = 0;
const insideWing = (d, inner) => { const id = "fw-clip-" + wingClips++;
  return `<clipPath id="${id}"><path d="${d}"/></clipPath><g clip-path="url(#${id})">${inner}</g>`; };
function fairyWingsSvg(style, color, w, L, flutter = true, lfo = null) {
  wingClips = 0;   // (the same wings, the same clip ids, every draw)
  const dk = darker(color), lt = mix(color, "#ffffff", 0.45), out = [];
  const lf = flutter ? lfoNow(lfo) : null, css = [];
  for (const sd of [-1, 1]) {
    const ax = sd * 16, ay = 24, X = v => f1(sd * v);
    let t = "";
    if (style === "butterfly") {
      const up = `M${X(16)} 20 C${X(w + 40)} -160 ${X(w + 200)} -150 ${X(w + 175)} -20 C${X(w + 150)} 34 ${X(60)} 44 ${X(16)} 30 Z`;
      const lo = `M${X(16)} 36 C${X(w + 120)} 40 ${X(w + 160)} 150 ${X(w + 70)} 176 C${X(30)} 178 ${X(12)} 100 ${X(16)} 36 Z`;
      t += `<path d="${lo}" fill="${color}" stroke="#1f1f22" stroke-width="7" stroke-linejoin="round"/>` +
           `<path d="${up}" fill="${color}" stroke="#1f1f22" stroke-width="7" stroke-linejoin="round"/>` +
           // (its veins and spots, each inside its own wing, the veins carried on to the wing's edge)
           insideWing(up, [[w + 150, -110], [w + 170, -40], [w + 90, -120], [w + 40, 30]].map(([x, y]) => `<path d="M${X(20)} 26 Q${X(x * 0.55)} ${f1(y * 0.6)} ${X(20 + (x - 20) * 1.5)} ${f1(26 + (y - 26) * 1.5)}" fill="none" stroke="#1f1f22" stroke-width="3"/>`).join("") +
             [[w + 170, -70], [w + 160, -10], [w + 130, -125]].map(([x, y]) => `<circle cx="${X(x)}" cy="${f1(y)}" r="6" fill="#ffffff"/>`).join("")) +
           insideWing(lo, [[w + 110, 130], [w + 40, 90]].map(([x, y]) => `<path d="M${X(20)} 40 Q${X(x * 0.6)} ${f1(y * 0.7)} ${X(20 + (x - 20) * 1.6)} ${f1(40 + (y - 40) * 1.6)}" fill="none" stroke="#1f1f22" stroke-width="3"/>`).join("") +
             [[w + 100, 150], [w + 60, 165], [w + 140, 110]].map(([x, y]) => `<circle cx="${X(x)}" cy="${f1(y)}" r="6" fill="#ffffff"/>`).join(""));
    } else if (style === "dragonfly") {
      const wing = (tx, ty, wd) => { const mx = tx * 0.55, my = ty * 0.55 + 18, d = `M${X(16)} ${ay} Q${X(mx)} ${f1(my - wd)} ${X(tx)} ${f1(ty)} Q${X(mx)} ${f1(my + wd)} ${X(16)} ${ay} Z`;
        return `<path d="${d}" fill="${color}" fill-opacity="0.5" stroke="${dk}" stroke-width="2.5"/>` +
          `<path d="M${X(16)} ${ay} Q${X(mx)} ${f1(my)} ${X(tx)} ${f1(ty)}" fill="none" stroke="${dk}" stroke-width="1.5"/>` +
          // (its cross-veins, edge to edge: drawn long, kept inside the wing)
          insideWing(d, [...Array(9)].map((_, j) => { const u = (j + 1) / 10, cx = 16 + (tx - 16) * u, cy = ay + (ty - ay) * u + 18 * Math.sin(Math.PI * u) * 0.5;
            return `<line x1="${X(cx - 5)}" y1="${f1(cy - wd * 1.2)}" x2="${X(cx + 5)}" y2="${f1(cy + wd * 1.2)}" stroke="${dk}" stroke-width="1" opacity="0.7"/>`; }).join("")); };
      t += wing(w + 210, 70, 26) + wing(w + 240, -60, 30) +
           `<path d="M${X(w + 120)} -40 l${X(14)} -6" stroke="#ffffff" stroke-width="3" opacity="0.6"/>`;
    } else if (style === "leaf") {
      const leaf = (tx, ty, wd) => { const mx = tx * 0.5, my = ty * 0.5 + ay * 0.5, d = `M${X(16)} ${ay} Q${X(mx - 10)} ${f1(my - wd)} ${X(tx)} ${f1(ty)} Q${X(mx + 10)} ${f1(my + wd)} ${X(16)} ${ay} Z`;
        let v = `<path d="${d}" fill="${color}" stroke="${dk}" stroke-width="3"/>`, veins = "";
        v +=
                `<path d="M${X(16)} ${ay} Q${X(mx)} ${f1(my)} ${X(tx)} ${f1(ty)}" fill="none" stroke="${dk}" stroke-width="2.5"/>`;
        // (its side veins, from the midrib out to the leaf's edge, and its shine: kept inside the leaf)
        for (let j = 1; j < 6; j++) { const u = j / 6, cx = 16 + (tx - 16) * u, cy = ay + (ty - ay) * u;
          for (const e of [-1, 1]) veins += `<path d="M${X(cx)} ${f1(cy)} q${X((tx - 16) * 0.08)} ${f1(e * wd * 0.9)} ${X((tx - 16) * 0.14)} ${f1(e * wd * 1.4)}" fill="none" stroke="${dk}" stroke-width="1.5" opacity="0.8"/>`; }
        return v + insideWing(d, veins + `<path d="M${X(mx * 0.7)} ${f1(my - wd * 0.4)} q${X(30)} -14 ${X(60)} -12" fill="none" stroke="${lt}" stroke-width="4" opacity="0.6"/>`); };
      t += leaf(w + 140, 120, 46) + leaf(w + 175, -110, 62);
    } else if (style === "petal") {
      const petal = (tx, ty, wd) => { const ang = Math.atan2(ty - ay, sd * (tx - 16)), len = Math.hypot(tx - 16, ty - ay);
        return `<g transform="translate(${X(16)} ${ay}) rotate(${f1(ang * 180 / Math.PI)})"><path d="M0 0 C${f1(len * 0.3)} ${f1(-wd)} ${f1(len * 1.05)} ${f1(-wd * 1.1)} ${f1(len)} 0 C${f1(len * 1.05)} ${f1(wd * 1.1)} ${f1(len * 0.3)} ${f1(wd)} 0 0 Z" fill="url(#fairy-petal)" stroke="${dk}" stroke-width="2.5"/>` +
          `<path d="M${f1(len * 0.15)} 0 L${f1(len * 0.8)} 0" stroke="${dk}" stroke-width="1.5" opacity="0.5"/></g>`; };
      t += petal(w + 120, 140, 44) + petal(w + 170, 20, 52) + petal(w + 130, -130, 56);
    } else {   // sparkle: a big round wing above, a smaller below, see-through, a shine, stars
      t += `<path d="M${X(16)} 22 C${X(w + 30)} -190 ${X(w + 220)} -150 ${X(w + 160)} -30 C${X(w + 120)} 30 ${X(50)} 36 ${X(16)} 28 Z" fill="${color}" fill-opacity="0.55" stroke="${lt}" stroke-width="4"/>` +
           `<path d="M${X(16)} 36 C${X(w + 110)} 40 ${X(w + 130)} 140 ${X(w + 50)} 150 C${X(24)} 150 ${X(12)} 90 ${X(16)} 36 Z" fill="${color}" fill-opacity="0.5" stroke="${lt}" stroke-width="4"/>` +
           `<path d="M${X(w + 40)} -110 Q${X(w + 110)} -140 ${X(w + 150)} -90" fill="none" stroke="#ffffff" stroke-width="5" opacity="0.7"/>` +
           [[w + 120, -100, 0], [w + 170, -40, 0.4], [w + 70, 100, 0.8], [w + 200, -130, 1.2]].map(([x, y, d]) =>
             `<path d="M${X(x)} ${f1(y - 10)} L${X(x + 3)} ${f1(y - 3)} L${X(x + 10)} ${f1(y)} L${X(x + 3)} ${f1(y + 3)} L${X(x)} ${f1(y + 10)} L${X(x - 3)} ${f1(y + 3)} L${X(x - 10)} ${f1(y)} L${X(x - 3)} ${f1(y - 3)} Z" fill="#fff7c2" style="animation: fairy-twinkle 1.6s ${fairyPhase(1.6, d)}s infinite ease-in-out"/>`).join("");
    }
    // (turning about the shoulder, (ax, ay): the CSS turn is about the
    // group's own origin, moved there)
    if (lf && lf.dur < LFO_BLUR) { const b = wingLfoBlur(lf, sd, ax, ay, t); css.push(b.css); out.push(b.svg); continue; }
    const flap = lf ? wingLfoFlap(lf, sd) : null;
    if (flap) css.push(flap.css);
    out.push(flutter ? `<g transform="translate(${ax} ${ay})"><g style="${flap ? flap.style : `transform-origin: 0 0; animation: fairy-flap-${sd < 0 ? "l" : "r"} 0.9s ${fairyPhase(0.9)}s infinite ease-in-out`}">` +
                       `<g transform="translate(${-ax} ${-ay})">${t}</g></g></g>` : `<g>${t}</g>`);
  }
  const defs = style === "petal" ? `<defs><radialGradient id="fairy-petal" cx="0.2" cy="0.5" r="0.9"><stop offset="0" stop-color="${lt}"/><stop offset="1" stop-color="${color}"/></radialGradient></defs>` : "";
  return (css.length ? `<style>${css.join(" ")}</style>` : "") + defs + out.join("");
}
// a fairy's hem along the bottom of her tunic: from x0 to x1 at y, its style's shape
const fairyHem = (style, x0, x1, y) => {
  const m = (x0 + x1) / 2, q = (x1 - x0);
  return style === "leaf" ? ` Q${f1(x0 + q * 0.1)} ${f1(y + 20)} ${f1(m)} ${f1(y + 34)} Q${f1(x1 - q * 0.1)} ${f1(y + 20)} ${f1(x1)} ${f1(y)}`
    : style === "bluebell" ? ` Q${f1(m)} ${f1(y + 32)} ${f1(x1)} ${f1(y)}`
    : style === "sparkle" ? ` L${f1(m)} ${f1(y + 22)} L${f1(x1)} ${f1(y)}`
    : style === "toadstool" ? ` Q${f1(m)} ${f1(y + 14)} ${f1(x1)} ${f1(y)}`
    : ` C${f1(x0 - q * 0.05)} ${f1(y + 26)} ${f1(m - q * 0.25)} ${f1(y + 34)} ${f1(m)} ${f1(y + 32)} C${f1(m + q * 0.25)} ${f1(y + 34)} ${f1(x1 + q * 0.05)} ${f1(y + 26)} ${f1(x1)} ${f1(y)}`;
};
function shoeSvg(x, y, side, sh, skin) {
  const sc = sh.size / 100;
  if (sh.style === "hoof") {
    // a deer's: a dark hoof, split down the front
    const cx = x + side * 8 * sc, cy = y + 13 * sc;
    return `<path d="M${f1(cx - side * 18 * sc)} ${f1(cy + 12 * sc)} L${f1(cx - side * 14 * sc)} ${f1(cy - 10 * sc)} ` +
           `Q${f1(cx + side * 6 * sc)} ${f1(cy - 16 * sc)} ${f1(cx + side * 20 * sc)} ${f1(cy + 12 * sc)} Z" ` +
           `fill="#3a2a1e" stroke="#1d140d" stroke-width="2.5" stroke-linejoin="round"/>` +
           `<line x1="${f1(cx + side * 4 * sc)}" y1="${f1(cy - 8 * sc)}" x2="${f1(cx + side * 6 * sc)}" y2="${f1(cy + 12 * sc)}" ` +
           `stroke="#1d140d" stroke-width="2"/>`;
  }
  if (sh.style === "paw") {
    const cx = x + side * 10 * sc, cy = y + 12 * sc;
    // (ticket 499) a dragon's: three claws, in its horns' colour, out of the paw's front edge by its toes
    const claws = !sh.claws ? "" : `<path d="${[-1, 0, 1].map(i => { const y0 = cy + i * 6 * sc, rim = 28 * Math.sqrt(1 - (i * 6 / 14) ** 2);
      return `M${f1(cx + side * (rim - 3) * sc)} ${f1(y0 - 3.5 * sc)} L${f1(cx + side * (rim + 9) * sc)} ${f1(y0 + i * sc)} L${f1(cx + side * (rim - 3) * sc)} ${f1(y0 + 3.5 * sc)} Z`; }).join(" ")}" ` +
      `fill="${sh.claws}" stroke="${darker(sh.claws)}" stroke-width="1.2" stroke-linejoin="round"/>`;
    return `<ellipse cx="${f1(cx)}" cy="${f1(cy)}" rx="${f1(28 * sc)}" ry="${f1(14 * sc)}" fill="${sh.color}" ` +
           `stroke="${darker(sh.color)}" stroke-width="2.5"/>` +
           [14, 21, 26].map((dx, i) => `<circle cx="${f1(cx + side * dx * sc)}" cy="${f1(cy + (i - 1) * 6 * sc)}" ` +
             `r="${f1(3.6 * sc)}" fill="${sh.pad}"/>`).join("") + claws;
  }
  // (ticket 288) a scuba suit's fins: a black foot pocket and a long blade on past the toes, ribbed, in the suit's stripes' color
  if (sh.style === "fin") {
    const c = sh.color || "#14b8a6", X = v => f1(x + side * v * sc), Y = v => f1(y + v * sc);
    return `<path data-dive="fin" d="M${X(-12)} ${Y(-14)} L${X(12)} ${Y(-14)} L${X(18)} ${Y(4)} L${X(70)} ${Y(-2)} Q${X(96)} ${Y(-6)} ${X(102)} ${Y(10)} Q${X(96)} ${Y(28)} ${X(70)} ${Y(26)} L${X(-14)} ${Y(26)} Z" ` +
           `fill="${c}" stroke="${darker(c)}" stroke-width="2.5" stroke-linejoin="round"/>` +
           `<path d="M${X(-12)} ${Y(-14)} L${X(12)} ${Y(-14)} L${X(18)} ${Y(4)} L${X(22)} ${Y(26)} L${X(-14)} ${Y(26)} Z" fill="#111827"/>` +
           [40, 58, 76].map(v => `<line x1="${X(v)}" y1="${Y(6)}" x2="${X(v + 12)}" y2="${Y(20)}" stroke="${darker(c)}" stroke-width="2"/>`).join("");
  }
  if (sh.style === "bare")
    return `<ellipse cx="${f1(x + side * 12 * sc)}" cy="${f1(y + 8 * sc)}" rx="${f1(22 * sc)}" ry="${f1(10 * sc)}" ` +
           `fill="${skin}" stroke="${darker(skin)}" stroke-width="2.5"/>`;
  // a fairy's shoes, in their color: curly slippers with a pompom, petals
  // wrapped round, a leaf wrapped round, ankle boots with stars, or bare
  // feet with a ring of flowers at the ankle
  if (sh.style && sh.style.startsWith("fairy-")) {
    const c = sh.color || "#f472b6", dk = darker(c), lt = mix(c, "#ffffff", 0.4), X = v => f1(x + side * v * sc), Y = v => f1(y + v * sc);
    const kind = sh.style.slice(6);
    if (kind === "bare")
      return `<ellipse cx="${X(12)}" cy="${Y(8)}" rx="${f1(22 * sc)}" ry="${f1(10 * sc)}" fill="${skin}" stroke="${darker(skin)}" stroke-width="2.5"/>` +
        [-12, -4, 4, 12].map((dx, i) => [0, 72, 144, 216, 288].map(a => `<circle cx="${f1(x + dx * sc + 3 * sc * Math.cos(a * Math.PI / 180))}" cy="${f1(y - 6 * sc + 3 * sc * Math.sin(a * Math.PI / 180))}" r="${f1(2.6 * sc)}" fill="${["#f472b6", "#fde047", "#ffffff", "#a78bfa"][i]}"/>`).join("") +
          `<circle cx="${f1(x + dx * sc)}" cy="${f1(y - 6 * sc)}" r="${f1(1.6 * sc)}" fill="#f59e0b"/>`).join("");
    if (kind === "slipper")
      return `<path d="M${X(-14)} ${Y(-8)} Q${X(-16)} ${Y(20)} ${X(4)} ${Y(22)} Q${X(36)} ${Y(24)} ${X(48)} ${Y(8)} Q${X(58)} ${Y(-6)} ${X(50)} ${Y(-16)} Q${X(44)} ${Y(-2)} ${X(30)} ${Y(0)} Q${X(10)} ${Y(-4)} ${X(-14)} ${Y(-8)} Z" fill="${c}" stroke="${dk}" stroke-width="2.5" stroke-linejoin="round"/>` +
        `<circle cx="${X(50)}" cy="${Y(-18)}" r="${f1(6 * sc)}" fill="${lt}" stroke="${dk}" stroke-width="1.5"/>` +
        `<path d="M${X(-10)} ${Y(-4)} Q${X(10)} ${Y(4)} ${X(28)} ${Y(2)}" fill="none" stroke="${lt}" stroke-width="2.5"/>`;
    if (kind === "petal")
      return `<ellipse cx="${X(12)}" cy="${Y(12)}" rx="${f1(26 * sc)}" ry="${f1(12 * sc)}" fill="${dk}"/>` +
        [[-6, -10, -30], [12, -14, 0], [30, -8, 30], [40, 6, 60]].map(([px, py, r]) =>
          `<ellipse cx="${X(px)}" cy="${Y(py + 12)}" rx="${f1(11 * sc)}" ry="${f1(17 * sc)}" transform="rotate(${f1(side * r)} ${X(px)} ${Y(py + 12)})" fill="${c}" stroke="${dk}" stroke-width="2"/>`).join("") +
        `<circle cx="${X(12)}" cy="${Y(0)}" r="${f1(4 * sc)}" fill="#fde047"/>`;
    if (kind === "leaf")
      return `<path d="M${X(-14)} ${Y(-6)} Q${X(-14)} ${Y(22)} ${X(10)} ${Y(22)} Q${X(40)} ${Y(22)} ${X(58)} ${Y(4)} Q${X(36)} ${Y(-8)} ${X(-14)} ${Y(-6)} Z" fill="${c}" stroke="${dk}" stroke-width="2.5"/>` +
        `<path d="M${X(-8)} ${Y(8)} Q${X(24)} ${Y(10)} ${X(54)} ${Y(4)}" fill="none" stroke="${dk}" stroke-width="2"/>` +
        [6, 20, 34].map(v => `<path d="M${X(v)} ${Y(9)} l${f1(side * 8 * sc)} ${f1(-7 * sc)} M${X(v)} ${Y(9)} l${f1(side * 8 * sc)} ${f1(7 * sc)}" stroke="${dk}" stroke-width="1.5"/>`).join("") +
        `<path d="M${X(-10)} ${Y(-10)} q${f1(side * 12 * sc)} ${f1(-14 * sc)} ${f1(side * 20 * sc)} ${f1(-2 * sc)}" fill="none" stroke="#15803d" stroke-width="2.5"/>`;
    // sparkle boots
    return `<path d="M${X(-13)} ${Y(-30)} L${X(13)} ${Y(-30)} L${X(14)} ${Y(4)} Q${X(36)} ${Y(4)} ${X(46)} ${Y(12)} Q${X(48)} ${Y(24)} ${X(36)} ${Y(26)} L${X(-14)} ${Y(26)} Z" fill="${c}" stroke="${dk}" stroke-width="2.5" stroke-linejoin="round"/>` +
      `<rect x="${f1(x - 15 * sc)}" y="${f1(y - 34 * sc)}" width="${f1(30 * sc)}" height="${f1(7 * sc)}" rx="${f1(3 * sc)}" fill="${lt}"/>` +
      [[0, -18], [24, 14], [6, 6]].map(([px, py]) => `<path d="M${X(px)} ${Y(py - 5)} L${X(px + 1.5)} ${Y(py - 1.5)} L${X(px + 5)} ${Y(py)} L${X(px + 1.5)} ${Y(py + 1.5)} L${X(px)} ${Y(py + 5)} L${X(px - 1.5)} ${Y(py + 1.5)} L${X(px - 5)} ${Y(py)} L${X(px - 1.5)} ${Y(py - 1.5)} Z" fill="#fff7c2"/>`).join("");
  }
  // (ticket 557) a ninja's boot, soft and quiet: no heel, a rounded toe split in two at its tip (the big toe's own pocket), a
  // thin soft sole; its shaft under the trousers, where the bands begin (ninjaShin, js/06-costumes.js)
  if (sh.style === "ninjaboot") {
    const c = sh.color || "#18181e", X = v => f1(x + side * v * sc), Y = v => f1(y + v * sc);
    return `<path d="M${X(-12)} ${Y(-30)} L${X(12)} ${Y(-30)} L${X(13)} ${Y(4)} Q${X(22)} ${Y(7)} ${X(32)} ${Y(10)} Q${X(46)} ${Y(9)} ${X(47.5)} ${Y(14.5)} ` +
           `Q${X(48)} ${Y(18.5)} ${X(39.5)} ${Y(18.5)} Q${X(47.5)} ${Y(20)} ${X(46)} ${Y(23.5)} Q${X(45)} ${Y(26)} ${X(40)} ${Y(26)} L${X(-9)} ${Y(26)} Q${X(-15)} ${Y(25)} ${X(-14)} ${Y(14)} Z" ` +
           `fill="${c}" stroke="${darker(c)}" stroke-width="2.5" stroke-linejoin="round"/>` +
           `<path d="M${X(-10)} ${Y(24.2)} L${X(40)} ${Y(24.2)} M${X(39.5)} ${Y(18.5)} Q${X(34)} ${Y(16)} ${X(29)} ${Y(10.5)}" fill="none" stroke="${mix(c, "#ffffff", 0.2)}" stroke-width="${f1(2.2 * sc)}" stroke-linecap="round"/>`;
  }
  const T = toesOf(sh);
  if (T) return toedFoot(x, y, side, sc, skin, T);
  const ps = SHOES[sh.style].map(([dx, dy]) => [x + side * dx * sc, y + dy * sc]);
  let s = poly(ps, sh.color, darker(sh.color));
  if (sh.style === "sneaker")
    s += line(x + side * -10 * sc, y + 22 * sc, x + side * 40 * sc, y + 22 * sc, 3 * sc, "#ffffff");
  if (sh.style === "clown")
    s += `<circle cx="${f1(x + side * 40 * sc)}" cy="${f1(y + 4 * sc)}" r="${f1(6 * sc)}" fill="#ffffff" stroke="${darker(sh.color)}" stroke-width="2"/>`;
  if (sh.style === "witchboot") {
    // a strap round the ankle, darker, and a gold buckle on its outside
    const st = mix(sh.color, "#000000", 0.35), bx = x + side * 6 * sc;
    s += `<rect x="${f1(x - 13 * sc)}" y="${f1(y - 16 * sc)}" width="${f1(26 * sc)}" height="${f1(8 * sc)}" fill="${st}"/>` +
         `<rect x="${f1(bx - 5 * sc)}" y="${f1(y - 18 * sc)}" width="${f1(10 * sc)}" height="${f1(12 * sc)}" rx="${f1(1.5 * sc)}" ` +
         `fill="none" stroke="#f2c230" stroke-width="${f1(2.6 * sc)}"/>`;
  }
  if (sh.style === "cowboot") {
    if (sh.star) s += `<polygon points="${starPts(x + side * 0.5 * sc, y - 10 * sc, 7 * sc)}" fill="${sh.star}" stroke="${darker(sh.star)}" stroke-width="1"/>`;
    if (sh.spur) s += `<polygon points="${starPts(x - side * 19 * sc, y + 15 * sc, 6.5 * sc)}" fill="#d1d5db" stroke="#6b7280" stroke-width="1.2"/>`;
  }
  return s;
}
// (ticket 140) A foot seen front on (walking toward us) or from behind
// (walking away), at the ankle (x, y), in the friend's units: a sneaker's
// toe box with its laces and white rubber toe, or its heel; bare, the top of
// the foot with its toes along the front, the big toe on the inside, or the
// heel. As the foot lifts (0 to 1), its sole comes into view: under the
// toe, from the front; from behind, tipped up toward us as it pushes off.
// For the default friend: sneakers, bare feet, and toes ("human").
const FRONT_FEET = { sneaker: true, bare: true, human: true };
function footFront(x, y, side, sh, skin, away, lift) {
  // (front on, a foot is about as wide as the leg above it: 1.45 times the side view's units)
  const sc = 1.45 * (sh.size ?? 100) / 100, X = v => f1(x + v * sc), Y = v => f1(y + v * sc), inner = x > 0 ? -1 : 1;
  if (sh.style === "sneaker") {
    const c = sh.color, dk = darker(c);
    const upper = `M${X(-11)} ${Y(-2)} L${X(11)} ${Y(-2)} Q${X(17)} ${Y(6)} ${X(16)} ${Y(16)} Q${X(15)} ${Y(24)} ${X(0)} ${Y(24)} Q${X(-15)} ${Y(24)} ${X(-16)} ${Y(16)} Q${X(-17)} ${Y(6)} ${X(-11)} ${Y(-2)} Z`;
    if (!away)
      return `<ellipse cx="${X(0)}" cy="${Y(23)}" rx="${f1(16 * sc)}" ry="${f1((2 + 6 * lift) * sc)}" fill="#ffffff" stroke="#9ca3af" stroke-width="2"/>` +
             `<path d="${upper}" fill="${c}" stroke="${dk}" stroke-width="2.5" stroke-linejoin="round"/>` +
             `<ellipse cx="${X(0)}" cy="${Y(-1)}" rx="${f1(8 * sc)}" ry="${f1(2.6 * sc)}" fill="${mix(c, "#000000", 0.45)}"/>` +
             [5, 10].map(v => `<line x1="${X(-6)}" y1="${Y(v)}" x2="${X(6)}" y2="${Y(v)}" stroke="#ffffff" stroke-width="${f1(2.2 * sc)}" stroke-linecap="round"/>`).join("") +
             `<path d="M${X(-15)} ${Y(17)} Q${X(0)} ${Y(27)} ${X(15)} ${Y(17)}" fill="none" stroke="#ffffff" stroke-width="${f1(3 * sc)}"/>`;
    // (from behind: the heel, its tab and the white sole's edge; lifting, the sole itself, treads and all)
    const ry = (3 + 14 * lift) * sc, cy = y + (24 - 6 * lift) * sc;
    return `<path d="${upper}" fill="${c}" stroke="${dk}" stroke-width="2.5" stroke-linejoin="round"/>` +
           `<rect x="${X(-3.5)}" y="${Y(-4)}" width="${f1(7 * sc)}" height="${f1(11 * sc)}" rx="${f1(2 * sc)}" fill="${dk}"/>` +
           `<path d="M${X(-15)} ${Y(19)} Q${X(0)} ${Y(27)} ${X(15)} ${Y(19)}" fill="none" stroke="#ffffff" stroke-width="${f1(3 * sc)}"/>` +
           (lift > 0.05 ? `<ellipse cx="${X(0)}" cy="${f1(cy)}" rx="${f1(15 * sc)}" ry="${f1(ry)}" fill="#f3f4f6" stroke="#9ca3af" stroke-width="2"/>` +
             [-0.5, 0, 0.5].map(u => `<line x1="${X(-9)}" y1="${f1(cy + u * ry)}" x2="${X(9)}" y2="${f1(cy + u * ry)}" stroke="#c7cbd1" stroke-width="${f1(2 * sc)}"/>`).join("") : "");
  }
  // bare: the foot, and its sole a little paler
  const dk = darker(skin), sole = mix(skin, "#ffffff", 0.3);
  if (!away) {
    const n = sh.style === "human" ? Math.max(1, Math.min(10, Math.round(sh.toes ?? 5))) : 0;
    const toes = [...Array(n)].map((_, i) => { const u = n === 1 ? 0.5 : i / (n - 1);
      return [x + inner * (12 - 24 * u) * sc, y + (21 + 2 * Math.sin(Math.PI * u)) * sc, ((i === 0 && n > 1 ? 5 : 3.8 - 1.1 * u)) * sc]; });
    return `<ellipse cx="${X(0)}" cy="${Y(22)}" rx="${f1(14 * sc)}" ry="${f1((2 + 5 * lift) * sc)}" fill="${sole}" stroke="${dk}" stroke-width="2"/>` +
           toes.map(([tx, ty, r]) => `<circle cx="${f1(tx)}" cy="${f1(ty)}" r="${f1(r + 2)}" fill="${dk}"/>`).join("") +
           `<ellipse cx="${X(0)}" cy="${Y(11)}" rx="${f1(14 * sc)}" ry="${f1(12 * sc)}" fill="${skin}" stroke="${dk}" stroke-width="2.5"/>` +
           toes.map(([tx, ty, r]) => `<circle cx="${f1(tx)}" cy="${f1(ty)}" r="${f1(r)}" fill="${skin}"/>`).join("");
  }
  const ry = (3 + 12 * lift) * sc, cy = y + (20 - 5 * lift) * sc;
  return `<ellipse cx="${X(0)}" cy="${Y(13)}" rx="${f1(13 * sc)}" ry="${f1(12 * sc)}" fill="${skin}" stroke="${dk}" stroke-width="2.5"/>` +
         `<path d="M${X(-7)} ${Y(21)} Q${X(0)} ${Y(25)} ${X(7)} ${Y(21)}" fill="none" stroke="${dk}" stroke-width="2"/>` +
         (lift > 0.05 ? `<ellipse cx="${X(0)}" cy="${f1(cy)}" rx="${f1(13 * sc)}" ry="${f1(ry)}" fill="${sole}" stroke="${dk}" stroke-width="2"/>` +
           (sh.style === "human" && lift > 0.3 ? [-8, -3, 2, 6, 9].map((v, i) => `<circle cx="${X(-inner * v)}" cy="${f1(cy - ry + 2 * sc)}" r="${f1((i ? 2.6 : 3.4) * sc)}" fill="${sole}" stroke="${dk}" stroke-width="1.5"/>`).join("") : "") : "");
}
// A hand with fingers, in the hand's frame (the arm points along +x): a
// palm and, for each finger, a rounded stroke fanned out at angle `a`
// (degrees, 0 = straight on), `len` and `w` in units of the hand size, and
// an optional round pad of radius `tip` at its end. The whole outline is
// drawn first, then the fill over it, so fingers and palm merge into one
// shape with one outline.
function fingered(r, c, st, fingers, palm = 0.75) {
  const lw = 2.5, P = r * palm;
  const parts = fingers.map(({ a, len, w, tip = 0, base = 0.6 }) => {
    const t = a * Math.PI / 180, dx = Math.cos(t), dy = Math.sin(t);
    const b = r * base, e = b + r * len;
    return { x1: dx * b, y1: dy * b, x2: dx * e, y2: dy * e, w: r * w, tip: r * tip };
  });
  const layer = (fill, grow) =>
    `<ellipse rx="${f1(P * 1.05 + grow)}" ry="${f1(P + grow)}" fill="${fill}"/>` +
    parts.map(p => {
      // a very thin finger gets a thinner outline, or it would be all outline
      const g = Math.min(grow, p.w * 0.9);
      return `<line x1="${f1(p.x1)}" y1="${f1(p.y1)}" x2="${f1(p.x2)}" y2="${f1(p.y2)}" ` +
             `stroke="${fill}" stroke-width="${f1(p.w * 2 + 2 * g)}" stroke-linecap="round"/>` +
             (p.tip ? `<circle cx="${f1(p.x2)}" cy="${f1(p.y2)}" r="${f1(p.tip + g)}" fill="${fill}"/>` : "");
    }).join("");
  return layer(st, lw) + layer(c, 0);
}
// n hooked talons (4 unless the claws slider says otherwise) fanned out
// from a paw of radius R, in the hand's frame (the arm along +x): each a
// curved, tapering point, wide where it leaves the paw and sharp at the
// tip. `clawLen` is a percent of the usual length (the hand size sets the
// paw; this sets only the claws). Up to 4 they keep the usual spacing;
// more fan out wider, to nearly all the way round at 100, and get thinner
// so each still shows.
// (ticket 499: fill and line, the claws' colours; a dragon's are its horns')
function talons(r, R, clawLen = 100, n = 4, fill = "#f4ecd6", line = "#5c4a2e") {
  const k = (clawLen ?? 100) / 100;
  n = Math.max(1, Math.min(100, Math.round(n ?? 4)));
  const span = n <= 4 ? 118 * (n - 1) / 3 : Math.min(330, 118 + (n - 4) * 20);
  const step = n > 1 ? span / (n - 1) : 0;
  const degs = n === 4 ? [-62, -22, 18, 56] : Array.from({ length: n }, (_, i) => -3 - span / 2 + i * step);
  // half a gap between neighbours, where they leave the paw
  const room = n > 4 ? R * 0.8 * Math.sin(step * Math.PI / 360) : Infinity;
  const sw = n > 4 ? Math.max(0.6, Math.min(2, room * 0.5)) : 2;
  return degs.map(deg => {
    const t = deg * Math.PI / 180, dx = Math.cos(t), dy = Math.sin(t);
    const nx = -dy, ny = dx;                              // across the talon
    const len = r * 1.4 * k, hook = r * 0.55 * Math.max(0.5, Math.min(k, 2));
    const w = Math.max(r * 0.03, Math.min(r * 0.25 * Math.min(1, 0.6 + 0.4 * k), room * 1.1));
    const P = (along, across) => `${f1(dx * along + nx * across)} ${f1(dy * along + ny * across)}`;
    return `<path d="M${P(R * 0.8, -w)} Q${P(R + len * 0.75, -w * 0.9)} ${P(R + len, hook)} ` +
           `Q${P(R + len * 0.45, w * 0.6)} ${P(R * 0.8, w)} Z" fill="${fill}" stroke="${line}" stroke-width="${f1(sw)}" stroke-linejoin="round"/>`;
  }).join("");
}
// The cat costumes (lion too) have claws on their paws.
const CLAWED = { cat: 1, tiger: 1, lion: 1, bobcat: 1, leopard: 1, cheetah: 1, jaguar: 1, wolf: 1, dragon: 1 };
// A suit's hands: paws in the fur color, with claws on a cat.
const suitHands = (C, kind) => ({ style: kind === "deer" || kind === "giraffe" ? "hoof" : "paw", size: Math.max(state.hands.size, 12), color: C.fur, pad: C.accent,   // (ticket 544: a giraffe's hooves too)
                                  claws: !!CLAWED[kind], clawLen: state.hands.clawLen ?? 100,
                                  clawCount: state.hands.clawCount ?? 4, clawColor: C.horn });   // (ticket 499: a dragon's in its horns' colour)
// How far a hand reaches from where it joins the arm, in any direction,
// in units of its size (fingers and claws stick out; a round hand doesn't).
const HAND_REACH = { round: 1, mitten: 1.65, star: 1.4, paw: 1.2, hoof: 1.2, claw: 2.4, pincer: 2.1,
                     human: 2.3, alien: 3.2, alien4: 2.7, alien3: 3.2 };
const handReach = h => {
  const k = (h.clawLen ?? 100) / 100;
  const per = h.style === "claw" ? 1.3 + 1.4 * k
            : h.style === "paw" && h.claws ? 1.6 + 1.4 * k
            : HAND_REACH[h.style] ?? 2;
  return per * h.size + 4;
};
// An alien hand with n long, thin fingers, each with a round pad at its
// tip, fanned out: 3 is the tree-frog hand (big pads), more spread wider
// with smaller pads, 100 is nearly a full circle of them.
function alienFingers(n) {
  n = Math.max(1, Math.min(100, Math.round(n)));
  const span = n <= 1 ? 0 : Math.min(330, 90 + (n - 3) * 14);
  const step = n > 1 ? span / (n - 1) : 90, half = Math.sin(step * Math.PI / 360);
  const w = Math.max(0.018, Math.min(0.2, 0.55 * half)), tip = Math.max(0.03, Math.min(0.36, 0.95 * half));
  const out = [];
  for (let i = 0; i < n; i++) {
    const a = n > 1 ? -span / 2 + i * step : 0, t = span ? Math.abs(a) / (span / 2) : 0;
    out.push({ a, len: 2.15 - 0.3 * t, w, tip });
  }
  return out;
}
// A human hand with n fingers, thumb included (1 is a thumb alone, a
// thumbs-up): the n - 1 fingers fan out
// wider as there are more of them (and thinner, so each still shows),
// the middle ones longest, and the thumb just past the first. At 100, a
// fringe almost all the way round the palm.
function manyFingers(n) {
  n = Math.max(1, Math.min(100, Math.round(n)));
  const k = n - 1, out = [];
  const span = k <= 1 ? 0 : Math.min(300, 24 * Math.min(k - 1, 3) + Math.max(0, k - 4) * 9);
  const step = k > 1 ? span / (k - 1) : 0;
  const w = k > 1 ? Math.max(0.05, Math.min(0.26, 0.75 * Math.sin(step * Math.PI / 360))) : 0.26;
  for (let i = 0; i < k; i++) {
    const a = k > 1 ? -span / 2 + i * step : 0, t = span ? Math.abs(a) / (span / 2) : 0;
    out.push({ a, len: 1.3 - 0.4 * t, w });
  }
  out.push({ a: -Math.min(span / 2 + 40, 175), len: 0.85, w: Math.max(w, 0.12), base: 0.5 });
  return out;
}
const starPoints = h => Math.max(1, Math.min(100, Math.round(h.starPoints ?? 5)));
// Each hand style's own angle slider, in radians, turning the hand about
// the wrist (0, as it always was, unless the slider says otherwise): a
// star's turns only as far as its next point, where it looks as it began;
// the others have no such symmetry, so they turn all the way round. (A
// round hand looks the same however it turns, so it has none.)
const HAND_TURN = { mitten: "mittenAngle", claw: "clawAngle", human: "humanAngle", alien: "alienAngle",
                    alien4: "alienAngle", alien3: "alienAngle" };
const handPeriod = h => h.style === "star" ? 2 * Math.PI / starPoints(h) : 2 * Math.PI;
function handSvg(x, y, angle, side, h, skin) {
  const c = h.color === "skin" ? skin : h.color, st = darker(c), r = h.size;
  // (the star turns its own points; the others turn whole, in the mirror
  // the left hand gets, so the two hands turn alike)
  const turn = HAND_TURN[h.style] ? (h[HAND_TURN[h.style]] ?? 0) * 180 / Math.PI : 0;
  const turned = inner => turn ? `<g transform="rotate(${f1(turn)})">${inner}</g>` : inner;
  const g = inner => `<g transform="translate(${f1(x)} ${f1(y)}) rotate(${f1(angle)}) scale(1 ${side})">${turned(inner)}</g>`;
  switch (h.style) {
    case "round":
      return g(`<circle r="${r}" fill="${c}" stroke="${st}" stroke-width="2.5"/>`);
    case "mitten":
      return g(`<ellipse cx="${r * 0.4}" rx="${r * 1.2}" ry="${r * 0.85}" fill="${c}" stroke="${st}" stroke-width="2.5"/>` +
               `<ellipse cx="${r * 0.1}" cy="${-r * 0.9}" rx="${r * 0.35}" ry="${r * 0.5}" fill="${c}" stroke="${st}" stroke-width="2.5"/>`);
    case "star": {
      // n points (5 unless the slider says otherwise), the first straight
      // up, then turned by the angle slider (radians, up to the turn to
      // the next point), with a dip between each two; a wide gap (1 or 2
      // points) dips in a rounded curve, so 1 is a teardrop and 2 a lens.
      // Many points get a thinner outline, so each still shows.
      const n = starPoints(h);
      const gap = 2 * Math.PI / n, dips = n >= 3 ? 1 : n === 2 ? 2 : 5;
      const ps = [];
      for (let i = 0; i < n; i++) {
        const a = -Math.PI / 2 + (h.starAngle ?? 0) + i * gap;
        ps.push([r * 1.4 * Math.cos(a), r * 1.4 * Math.sin(a)]);
        for (let j = 1; j <= dips; j++) {
          const b = a + gap * j / (dips + 1);
          ps.push([r * 0.55 * Math.cos(b), r * 0.55 * Math.sin(b)]);
        }
      }
      const sw = n > 5 ? Math.max(0.6, Math.min(2.5, Math.PI * r * 0.55 / n)) : 2.5;
      return g(poly(ps, c, st, sw));
    }
    case "paw": {
      // a costume's paw: fur, with a pad and three toe pads (and on a cat
      // costume, the claw hand's talons sticking out of it)
      const pad = h.pad;
      return g((h.claws ? talons(r, r * 1.0, h.clawLen, h.clawCount, h.clawColor, h.clawColor && darker(h.clawColor)) : "") +
               `<circle r="${r * 1.15}" fill="${c}" stroke="${st}" stroke-width="2.5"/>` +
               `<ellipse cx="${-r * 0.15}" rx="${r * 0.42}" ry="${r * 0.5}" fill="${pad}"/>` +
               [[0.62, -0.6], [0.86, 0], [0.62, 0.6]].map(([px, py]) =>
                 `<circle cx="${r * px}" cy="${r * py}" r="${r * 0.22}" fill="${pad}"/>`).join(""));
    }
    case "hoof": {
      // a deer suit's hand: a dark split hoof at the end of the furry arm
      return g(`<circle r="${r * 1.05}" fill="${c}" stroke="${st}" stroke-width="2.5"/>` +
               `<path d="M${r * 0.35} ${-r * 0.95} Q${r * 1.45} 0 ${r * 0.35} ${r * 0.95} Z" fill="#3a2a1e" stroke="#1d140d" stroke-width="2"/>` +
               `<line x1="${r * 0.4}" y1="0" x2="${r * 1.2}" y2="0" stroke="#1d140d" stroke-width="2"/>`);
    }
    case "pincer":
      // (ticket 515) a robot's grabber: two curved jaws, open a little, out of a round wrist (one path for both jaws)
      return g(`<path d="M${f1(0.2 * r)} ${f1(-0.55 * r)} Q${f1(1.4 * r)} ${f1(-1.05 * r)} ${f1(1.95 * r)} ${f1(-0.2 * r)} Q${f1(1.25 * r)} ${f1(-0.45 * r)} ${f1(0.6 * r)} ${f1(-0.1 * r)} Z ` +
               `M${f1(0.2 * r)} ${f1(0.55 * r)} Q${f1(1.4 * r)} ${f1(1.05 * r)} ${f1(1.95 * r)} ${f1(0.2 * r)} Q${f1(1.25 * r)} ${f1(0.45 * r)} ${f1(0.6 * r)} ${f1(0.1 * r)} Z" fill="${c}" stroke="${st}" stroke-width="2.5" stroke-linejoin="round"/>` +
               `<circle r="${f1(0.72 * r)}" fill="${c}" stroke="${st}" stroke-width="2.5"/>`);
    case "claw": {
      // a furry paw with four hooked talons
      return g(talons(r, r * 0.7, h.clawLen, h.clawCount) +
               `<ellipse cx="${r * 0.1}" rx="${r * 0.85}" ry="${r * 0.8}" fill="${c}" stroke="${st}" stroke-width="2.5"/>` +
               `<ellipse cx="${-r * 0.1}" cy="${r * 0.05}" rx="${r * 0.32}" ry="${r * 0.36}" fill="${st}" opacity="0.35"/>`);
    }
    case "human":
      if ((h.fingers ?? 5) !== 5) return g(fingered(r, c, st, manyFingers(h.fingers)));
      // a palm, four fingers (the middle one longest) and a thumb
      return g(fingered(r, c, st, [
        { a: -36, len: 1.15, w: 0.25 }, { a: -12, len: 1.35, w: 0.26 }, { a: 12, len: 1.25, w: 0.25 },
        { a: 35, len: 1.0, w: 0.23 }, { a: -95, len: 0.85, w: 0.28, base: 0.5 }]));
    case "alien":
      return g(fingered(r, c, st, alienFingers(h.alienFingers ?? 3), 0.55));
    // the 4- and 3-finger styles before the slider, so saved friends still draw
    case "alien4": return g(fingered(r, c, st, alienFingers(4), 0.6));
    case "alien3": return g(fingered(r, c, st, alienFingers(3), 0.55));
  }
  return "";
}

// A WITCH'S THINGS. The hat, in the round head's units: a wide brim just
// below the crown, a tall cone leaning over, bent at the tip, a band in
// the trim color and a gold buckle.
// (ticket 124) in profile (dir, the way she faces) its tip bends back,
// away from her face, and the band's buckle is seen edge on at its front
// (ticket 444: from behind, away, no buckle: it is at the band's front)
function witchHat(WC, rx = 128, dir = 0, away = false) {
  const c = WC.witchColor, ln = darker(c);
  const cone = `<path d="M46 42 Q72 -24 98 -84 Q112 -116 154 -126 Q128 -104 122 -80 Q134 -20 154 42 Q100 54 46 42 Z" ` +
    `fill="${c}" stroke="${ln}" stroke-width="3" stroke-linejoin="round"/>`;
  return `<ellipse cx="100" cy="44" rx="${f1(rx)}" ry="${f1(18 * Math.max(1, rx / 128) ** 0.5)}" fill="${c}" stroke="${ln}" stroke-width="3"/>` +
    (dir > 0 ? `<g transform="translate(200 0) scale(-1 1)">${cone}</g>` : cone) +
    `<path d="M52 24 Q100 34 149 24 L152 38 Q100 48 49 38 Z" fill="${WC.trimColor}"/>` +
    (away ? "" : dir ? `<rect x="${dir > 0 ? 141 : 51}" y="${dir > 0 ? 23 : 23}" width="8" height="16" rx="2" fill="none" stroke="#f2c230" stroke-width="3"/>`
         : `<rect x="89" y="25" width="22" height="16" rx="2" fill="none" stroke="#f2c230" stroke-width="3.5"/>`);
}
// (ticket 431, Matthew: "gnome costume, like this one (man and woman on Gnomes book cover, and in that book generally --
// 1977 --- we aren't copying any illustrations at all, we are being inspired by them for our super simple-styled drawings")
// A GNOME'S HAT, in the round head's units: a tall cone of felt from a base low over the brow (64), a little wider than the
// head there, its sides bowing out a little and its tip rounded, three times the head's height above the base; in profile
// (dir, the way they face) the tip leans back, away from the face; a soft fold along its base.
function gnomeHat(WC, dir = 0) {
  const c = WC.gnomeHat || "#d2382b", ln = darker(c), lean = dir ? -dir * 30 : 0, tx = 100 + lean, ty = -236;
  const d = `M14 66 Q${f1(26 + lean * 0.3)} -40 ${f1(tx - 7)} ${f1(ty + 10)} Q${f1(tx)} ${f1(ty - 4)} ${f1(tx + 7)} ${f1(ty + 10)} ` +
            `Q${f1(174 + lean * 0.3)} -40 186 66 Q100 82 14 66 Z`;
  return `<path d="${d}" fill="${c}" stroke="${ln}" stroke-width="3" stroke-linejoin="round"/>` +
    `<path d="M22 58 Q100 72 178 58" fill="none" stroke="${ln}" stroke-width="2.5" stroke-linecap="round" opacity="0.55"/>`;
}
// A cat sitting on the left shoulder (the picture's left), in body units,
// a tuxedo cat: its fur, with a white blaze down its nose, a white chest
// and white boots on its front paws ("off" leaves it all fur). Its tail
// hangs down behind; its eyes and nose in their own colors.
// (wag: its tail swings slowly about its root, as the healer's cat's does
// on its shelf)
// (wag: true for the healer's cat's own slow wag, or a cat's wag from catWag,
// with an LFO or at its own pace, begun its own offset into the cycle; curl:
// sitting on the floor, its tail curled round its front paws, the tip
// twitching with the wag, instead of hanging down over a shelf's edge)
function shoulderCat(w, WC, wag = false, curl = false) {
  const x = -(w - 4), y = -9, fur = WC.catFur, ln = darker(fur), wh = WC.catWhite;
  const tux = wh && wh !== "off", stroke = `fill="none" stroke="${fur}" stroke-width="6" stroke-linecap="round"`;
  const hang = `<path d="M${x + 8} ${y - 6} Q${x + 34} ${y + 6} ${x + 24} ${y + 42}" ${stroke}/>`;
  const curlD = v => `M${x + 11} ${y - 8} Q${x + 26} ${y + 2} ${x + 8} ${y + 3} Q${x - 6} ${y + 4} ${f1(x - 16 + v)} ${f1(y - 1 - 6 * v)}`;
  const tail = curl ? (wag && wag !== true ? wagged(wag, sh => `<path d="${curlD(0)}" ${stroke}>${wagAnim(wag, "d", v => curlD((1 + v) / 2), sh)}</path>`) : `<path d="${curlD(0)}" ${stroke}/>`)
    : wag === true ? `<g>${hang}<animateTransform attributeName="transform" type="rotate" values="-14 ${x + 8} ${y - 6};16 ${x + 8} ${y - 6};-14 ${x + 8} ${y - 6}" dur="3.4s" calcMode="spline" keySplines="0.45 0 0.55 1;0.45 0 0.55 1" repeatCount="indefinite"/></g>`
    : wag ? wagged(wag, sh => `<g>${hang}${wagAnim(wag, "transform", (v, amp) => `${f1(-(8 + 20 * amp) * v)} ${x + 8} ${y - 6}`, sh)}</g>`) : hang;
  return (curl ? "" : tail) +
    `<ellipse cx="${x}" cy="${y - 17}" rx="15" ry="18" fill="${fur}" stroke="${ln}" stroke-width="1.5"/>` +
    (tux ? `<path d="M${x - 9} ${y - 32} Q${x - 3} ${y - 26} ${x + 3} ${y - 32} Q${x + 6} ${y - 14} ${x - 3} ${y - 2} Q${x - 12} ${y - 14} ${x - 9} ${y - 32} Z" fill="${wh}"/>` : "") +
    (curl ? tail : "") +
    // the front paws, side by side at the front of its body
    [x - 8, x + 2].map(px => `<ellipse cx="${px}" cy="${y - 2}" rx="5" ry="4" fill="${tux ? wh : fur}" stroke="${ln}" stroke-width="1.2"/>`).join("") +
    poly([[x - 15, y - 44], [x - 13, y - 64], [x - 4, y - 52]], fur, ln, 1.5) +
    poly([[x + 1, y - 52], [x + 8, y - 64], [x + 9, y - 44]], fur, ln, 1.5) +
    `<circle cx="${x - 3}" cy="${y - 42}" r="13" fill="${fur}" stroke="${ln}" stroke-width="1.5"/>` +
    (tux ? `<path d="M${x - 4.5} ${y - 50} L${x - 1.5} ${y - 50} L${x + 1} ${y - 36} Q${x - 3} ${y - 31} ${x - 7} ${y - 36} Z" fill="${wh}"/>` : "") +
    [x - 8, x + 2].map(ex => `<ellipse cx="${ex}" cy="${y - 43}" rx="3.2" ry="3.8" fill="${WC.catEyes}"/>` +
                             `<ellipse cx="${ex}" cy="${y - 43}" rx="1" ry="3.2" fill="#111"/>`).join("") +
    `<path d="M${x - 5} ${y - 37} L${x - 1} ${y - 37} L${x - 3} ${y - 35} Z" fill="${WC.catNose}"/>`;
}
// A cat's wag: its tail swinging (or its tip twitching) once a cycle, with an
// LFO in time with the music, or else at its own pace (each cat its own,
// i its place in the castle's list); either way begun its own offset into the
// cycle, so no two cats wag together. The key says what its drawing depends on.
function catWag(C, i) {
  const lf = lfoNow(C.lfo), off = (C.offset ?? 0) / 100;
  if (lf) return { lf, off, key: [lf.dur.toFixed(4), lf.shape, lf.amp.toFixed(2), lfoBegin(lf, off)].join() };
  return { lf: null, off, dur: 2.6 + 0.37 * ((i * 5) % 8), key: "own" };
}
// The music's clock, in seconds (lfoNow's): where the tune is while it
// plays, else the page's own time. An LFO's wave is where this clock is,
// round its cycle.
// (ticket 198) The music's clock only while it runs: a tune "playing" in a suspended AudioContext (a page whose sound
// cannot start; headless Firefox on GitHub, where it lost 10 frames a second after any first click) has a clock that
// stands still, the picture's ran away from it, the gap was measured again every quarter second, and the whole scene
// built again each time, ticket 117's way. Stopped, the page's own clock, which keeps the picture's pace.
// (ticket 274) Held still, the picture's clock, whatever plays.
const musicSeconds = () => stillAt === null && playing && typeof audio !== "undefined" && audio && audio.state === "running"
  ? playing.s0 * playing.sd + audio.currentTime - playing.t0 : picClock();
// How far the picture's own clock (its animations') is ahead of the music's.
// The two run at the same speed, so the gap stays put; but the picture's
// clock moves on only once a frame, and the music's all the time, so a gap
// read afresh at every draw wobbled by a frame, and a slow frame wobbled it
// past lfoBegin's rounding: the scene's text changed from draw to draw, and
// the whole scene was built again on every move of a drag, slowing every
// frame further, until dragging the friend ground to a halt and the music
// stuttered (ticket 117). So the gap is measured once and kept, and measured
// again only when it truly moves (by more than PIC_GAP_SLACK seconds): the
// music starting, stopping or jumping. During a drag the gap is kept as it
// was, and measured again once it ends (ticket 133): a press on the picture
// can start the music, and measuring it then built the whole scene again in
// the middle of the drag.
const PIC_GAP_SLACK = 0.25;
let picGap = null, picDragging = false;
function picGapNow() {
  const pic = document.getElementById("pic"), now = pic && pic.getCurrentTime ? pic.getCurrentTime() : 0, g = now - musicSeconds();
  if (picGap === null || (Math.abs(g - picGap) > PIC_GAP_SLACK && !picDragging)) picGap = g;
  return picGap;
}
// when, in the picture's own time, an animation must have begun for its wave
// to be where the LFO's is now, `shift` of a cycle on (rounded, to a 20th of a
// second or a 16th of a cycle, so the scene's text stays the same from draw to
// draw while the music keeps time); an LFO too fast to show is a blur, in time
// with nothing, its copies just spread round the cycle
function lfoBegin(lf, shift) {
  if (lf.dur < LFO_BLUR) return -(((shift % 1) + 1) % 1) * lf.dur;
  const step = Math.min(0.05, lf.dur / 16);
  return Math.round(((picGapNow() - shift * lf.dur) % lf.dur + lf.dur) % lf.dur / step) * step;
}
// a wag's animation of `attr` (a rotation for "transform"): val(w, depth) its
// value at the wave's height w (-1 to 1); sh: a further shift, for a blur's copies
function wagAnim(W, attr, val, sh = 0, type = "rotate") {
  const N = 16, vals = [], kt = [], shape = W.lf ? W.lf.shape : "sine", amp = W.lf ? W.lf.amp : 0.4, dur = W.lf ? W.lf.dur : W.dur;
  for (let j = 0; j <= N; j++) { vals.push(val(lfoWave(shape, j / N), amp)); kt.push((j / N).toFixed(4)); }
  const begin = W.lf ? lfoBegin(W.lf, W.off + sh) : -(((W.off + sh) % 1) * dur);
  return `<${attr === "transform" ? `animateTransform attributeName="transform" type="${type}"` : `animate attributeName="${attr}"`} values="${vals.join(";")}" ` +
         `keyTimes="${kt.join(";")}" dur="${dur.toFixed(4)}s" begin="${begin.toFixed(4)}s" repeatCount="indefinite"/>`;
}
// a tail drawn wagging, by tailAt(shift): once, or with an LFO too fast to
// show, as the wings are (wingLfoBlur), a blur of 8 see-through copies, each
// an eighth of a cycle behind the last
const wagged = (W, tailAt) => W.lf && W.lf.dur < LFO_BLUR
  ? [...Array(8)].map((_, j) => `<g opacity="0.25">${tailAt(j / 8)}</g>`).join("") : tailAt(0);
// A scene's thing moving with the music: an animation of `attr` whose values
// follow the wave (val(w, depth)) of the LFO the scene's quick or slow things
// follow (pace "fast": o.lfoFast, D to start with; "slow": o.lfoSlow, E), begun
// `off` of a cycle on, so things side by side each go their own way; or, with
// no LFO, its own steady animation (own). It reads the LFO through the
// scene's options (sceneOpts), so the mixing board's changes to it draw the
// scene again; a part holding it takes sceneLfoKey(o, pace) among its inputs.
// (a third pace, "wings": o.lfoWings, F to start with, for butterflies' wings)
const sceneLfoName = (o, pace) => { const L = pace === "fast" ? o.lfoFast ?? "D" : pace === "wings" ? o.lfoWings ?? "F" : o.lfoSlow ?? "E"; return LFO_NAMES.includes(L) ? L : null; };
const sceneLfoKey = (o, pace) => { const L = sceneLfoName(o, pace); return L ? L + o["lfo:" + L] : "own"; };
function sceneLfo(o, pace, off, attr, val, own = "", type = "rotate") {
  const L = sceneLfoName(o, pace), lf = L && (void o["lfo:" + L], lfoNow(L));
  return lf ? wagAnim({ lf, off }, attr, val, 0, type) : own;
}
// An animation as it is written (its values, its keyTimes), but in time with
// the scene's quick or slow LFO: its round as long as the LFO's, begun where
// the LFO's wave is now, `off` of a round on (the wave's shape and depth are
// the animation's own); with no LFO, as written. For things that already move
// in their own way: flames, vines, glows, pennants. Each animation in `own`
// is retimed.
function lfoTimed(o, pace, off, own) {
  const L = sceneLfoName(o, pace), lf = L && (void o["lfo:" + L], lfoNow(L));
  if (!lf) return own;
  const begin = lfoBegin(lf, off).toFixed(4), dur = lf.dur.toFixed(4);
  return own.replace(/<(animate\w*) ([^>]*?)(\/?)>/g, (m, tag, attrs, close) =>
    `<${tag} begin="${begin}s" ${attrs.replace(/ ?begin="[^"]*"/, "").replace(/dur="[^"]*"/, `dur="${dur}s"`)}${close}>`);
}
// A thing swaying about its foot or its hook (x, y), by up to `deg` either
// way, with the scene's slow LFO, each its own way in (`off`), or at its own
// slow pace: palm fronds, seaweed, cages, lanterns
const swaying = (o, inner, x, y, deg, off, own = 5) => `<g>${sceneLfo(o, "slow", off, "transform", (v, amp) => `${f1(deg * (0.5 + 1.5 * amp) * v)} ${f1(x)} ${f1(y)}`,
  `<animateTransform attributeName="transform" type="rotate" values="${-deg} ${f1(x)} ${f1(y)};${deg} ${f1(x)} ${f1(y)};${-deg} ${f1(x)} ${f1(y)}" dur="${own}s" repeatCount="indefinite"/>`)}${inner}</g>`;
// Butterflies, each fluttering round its own little loop through the air
// about a spot [x, y, size]: their wings beating with the scene's wings LFO
// (F, a wingbeat that looks like one, to start with; one too fast to see is
// a blur, as the fairy's wings are), bobbing with its quick one, each a
// different way into both (or at their own pace)
function butterfliesSvg(c, spots, cols = ["#f59e0b", "#60a5fa", "#f472b6", "#a78bfa", "#fde047"]) {
  const { k, o } = c, L = sceneLfoName(o, "wings"), lf = L && (void o["lfo:" + L], lfoNow(L));
  return spots.map(([x, y, sz], i) => {
    const col = cols[i % cols.length], rx = (30 + 10 * (i % 3)) * k, ry = (12 + 6 * (i % 2)) * k, d = (7 + 2 * (i % 3)).toFixed(1), off = (i * 0.31) % 1;
    const wings = [-1, 1].map(sd => `<path d="M0 0 Q${sd * 9} -14 ${sd * 15} -8 Q${sd * 14} 0 0 0 Z" fill="${col}" stroke="#3a2a1a" stroke-width="0.7"/>` +
      `<path d="M0 0 Q${sd * 11} 4 ${sd * 9} 11 Q${sd * 3} 9 0 0 Z" fill="${mix(col, "#ffffff", 0.3)}" stroke="#3a2a1a" stroke-width="0.7"/>`).join("");
    const W = { lf, off }, beat = lf ? wagged(W, sh => `<g>${wagAnim(W, "transform", v => `${(0.2 + 0.8 * (1 + v) / 2).toFixed(2)} 1`, sh, "scale")}${wings}</g>`)
      : `<g><animateTransform attributeName="transform" type="scale" values="1 1;0.2 1;1 1" dur="${(0.35 + 0.05 * (i % 3)).toFixed(2)}s" repeatCount="indefinite"/>${wings}</g>`;
    const bob = sceneLfo(o, "fast", off, "transform", (v, amp) => `0 ${f1((3 + 9 * amp) * k * v)}`, "", "translate");
    return `<g><animateMotion path="M${f1(x - rx)} ${f1(y)} a${f1(rx)} ${f1(ry)} 0 1 0 ${f1(2 * rx)} 0 a${f1(rx)} ${f1(ry)} 0 1 0 ${f1(-2 * rx)} 0" dur="${d}s" begin="-${(i * 1.7).toFixed(1)}s" repeatCount="indefinite"/>` +
      `<g>${bob}<g transform="scale(${(sz * k).toFixed(3)})">${beat}<ellipse rx="1.6" ry="6" fill="#3a2a1a"/></g></g></g>`;
  }).join("");
}
// A cat asleep, curled up nose to tail, (0, 0) under its middle: it breathes
// slowly, the tip of its tail twitching with its wag, and now and then a "z"
// rises from it
function sleepingCat(WC, W, i) {
  const fur = WC.catFur, ln = darker(fur), wh = WC.catWhite, tux = wh && wh !== "off";
  const stroke = `fill="none" stroke="${fur}" stroke-width="6" stroke-linecap="round"`;
  const tailD = v => `M24 -9 Q32 1 12 2 Q-4 3 ${f1(-12 + 2 * v)} ${f1(-2 - 5 * v)}`;
  const breathe = (3.4 + 0.45 * (i % 4)).toFixed(2), zb = `-${(1.3 * i % 4).toFixed(1)}s`;
  return `<ellipse cx="2" cy="1" rx="31" ry="4" fill="#000000" opacity="0.18"/>` +
    `<g><animateTransform attributeName="transform" type="scale" values="1 1;1.02 1.06;1 1" dur="${breathe}s" begin="-${(0.9 * i).toFixed(1)}s" repeatCount="indefinite"/>` +
    `<ellipse cx="2" cy="-12" rx="28" ry="13" fill="${fur}" stroke="${ln}" stroke-width="1.5"/>` +
    `<path d="M8 -20 Q22 -22 25 -9" fill="none" stroke="${ln}" stroke-width="1.2" opacity="0.6"/></g>` +
    (tux ? `<ellipse cx="-12" cy="-3" rx="7" ry="3.5" fill="${wh}" stroke="${ln}" stroke-width="1"/>` : "") +
    wagged(W, sh => `<path d="${tailD(0)}" ${stroke}>${wagAnim(W, "d", v => tailD((1 + v) / 2), sh)}</path>`) +
    poly([[-29, -17], [-31, -30], [-22, -23]], fur, ln, 1.5) + poly([[-16, -24], [-10, -33], [-8, -19]], fur, ln, 1.5) +
    `<circle cx="-18" cy="-14" r="11" fill="${fur}" stroke="${ln}" stroke-width="1.5"/>` +
    (tux ? `<ellipse cx="-18" cy="-8.5" rx="5.5" ry="3.5" fill="${wh}"/>` : "") +
    `<path d="M-25 -15 q2.5 2 5 0 M-16 -15 q2.5 2 5 0" fill="none" stroke="${fur === "#1f1f22" ? "#8a8a96" : "#2a2420"}" stroke-width="1.3" stroke-linecap="round"/>` +
    `<path d="M-20 -10.5 L-16 -10.5 L-18 -8.5 Z" fill="${WC.catNose}"/>` +
    `<text x="-26" y="-30" font-size="10" font-weight="bold" font-family="sans-serif" fill="#ffffff" stroke="#5a4a8a" stroke-width="0.7" opacity="0">z` +
    `<animate attributeName="y" values="-28;-50" dur="4s" begin="${zb}" repeatCount="indefinite"/>` +
    `<animate attributeName="opacity" values="0;0.9;0" dur="4s" begin="${zb}" repeatCount="indefinite"/></text>`;
}
// A cat walking, seen from the side, facing right, (0, 0) on the ground
// under its middle: its legs swinging by pairs, its tail up, swaying with its wag
function walkingCat(WC, W, i) {
  const fur = WC.catFur, ln = darker(fur), wh = WC.catWhite, tux = wh && wh !== "off", step = (0.8 + 0.06 * (i % 3)).toFixed(2);
  const leg = (hx, ph, near) => `<g><line x1="${hx}" y1="-21" x2="${hx}" y2="-1" stroke="${near ? fur : mix(fur, "#000000", 0.3)}" stroke-width="5" stroke-linecap="round"/>` +
    (near && tux ? `<circle cx="${hx}" cy="-2" r="2.6" fill="${wh}"/>` : "") +
    `<animateTransform attributeName="transform" type="rotate" values="-22 ${hx} -21;22 ${hx} -21;-22 ${hx} -21" dur="${step}s" begin="-${(ph * step).toFixed(2)}s" repeatCount="indefinite"/></g>`;
  return `<ellipse cx="0" cy="0" rx="24" ry="3.5" fill="#000000" opacity="0.18"/>` + leg(-13, 0, false) + leg(13, 0.5, false) +
    wagged(W, sh => `<g><path d="M-17 -27 Q-34 -32 -29 -52" fill="none" stroke="${fur}" stroke-width="6" stroke-linecap="round"/>` +
      wagAnim(W, "transform", (v, amp) => `${f1((6 + 16 * amp) * v)} -17 -27`, sh) + `</g>`) +
    `<ellipse cx="0" cy="-25" rx="21" ry="10" fill="${fur}" stroke="${ln}" stroke-width="1.5"/>` +
    (tux ? `<ellipse cx="14" cy="-21" rx="6" ry="6.5" fill="${wh}"/>` : "") +
    leg(-11, 0.5, true) + leg(15, 0, true) +
    poly([[16, -38], [17, -50], [23, -41]], fur, ln, 1.5) + poly([[24, -42], [29, -51], [31, -37]], fur, ln, 1.5) +
    `<circle cx="23" cy="-33" r="10" fill="${fur}" stroke="${ln}" stroke-width="1.5"/>` +
    (tux ? `<ellipse cx="28" cy="-28.5" rx="5" ry="3.2" fill="${wh}"/>` : "") +
    `<ellipse cx="27" cy="-35" rx="2.3" ry="2.9" fill="${WC.catEyes}"/><ellipse cx="27.6" cy="-35" rx="0.8" ry="2.4" fill="#111"/>` +
    `<path d="M32 -31 L34 -31 L33 -29.5 Z" fill="${WC.catNose}"/>` +
    `<path d="M30 -29 L40 -31 M30 -28 L40 -27" stroke="${tux || fur === "#1f1f22" ? "#d8d8de" : "#3a3a3a"}" stroke-width="0.6"/>`;
}

// (ticket 432, Matthew: "wizard costume, somewhat like witch but also distinct, somehow") A WIZARD'S HAT, in the round
// head's units: a narrow brim (rx, as wide as the hair needs), a tall cone whose tip flops forward over the brim (in profile
// toward the face, where a witch's bends back; facing us, to our left), no band or buckle, and gold stars and a crescent moon
// on the cone, in the robe's star colour.
const starPts = (cx, cy, r) => Array.from({ length: 10 }, (_, i) => { const a = -Math.PI / 2 + i * Math.PI / 5, q = i % 2 ? r * 0.45 : r; return f1(cx + q * Math.cos(a)) + "," + f1(cy + q * Math.sin(a)); }).join(" ");
function wizardHat(WC, rx = 116, dir = 0) {
  const c = WC.wizardRobe || "#2a3a8c", ln = darker(c), st = WC.wizardStars || "#f2c230", f = dir || -1;
  const cone = `M50 42 Q66 -30 88 -110 Q94 -140 ${f1(100 + f * 10)} -156 Q${f1(100 + f * 40)} -162 ${f1(100 + f * 58)} -128 Q${f1(100 + f * 30)} -140 112 -112 Q132 -30 150 42 Q100 54 50 42 Z`;
  const moon = `<path d="M${f1(100 - f * 18)} -24 a16 16 0 1 0 ${f1(-f * 6)} 26 a12 12 0 1 1 ${f1(f * 6)} -26 Z" fill="${st}"/>`;
  return `<ellipse cx="100" cy="44" rx="${f1(rx)}" ry="${f1(14 * Math.max(1, rx / 116) ** 0.5)}" fill="${c}" stroke="${ln}" stroke-width="3"/>` +
    `<path d="${cone}" fill="${c}" stroke="${ln}" stroke-width="3" stroke-linejoin="round"/>` + moon +
    [[100 + f * 16, 6, 8], [96, -54, 7], [104 - f * 4, -96, 5.5]].map(([x, y, r]) => `<polygon points="${starPts(x, y, r)}" fill="${st}"/>`).join("");
}
// (ticket 434, Matthew: "robin hood -style costume (also see Lego's theme from the 1990s castle era with woodsman, the
// patterns on those figures are memorable)") A FORESTER'S CAP, in the round head's units: a felt crown over the top of the
// head from a band at 46, peaked at the front (facing us, its peak a point over the brow; in profile, a beak over the face,
// the brim turned up behind), and a long feather slanting back from the band.
function robinHat(WC, dir = 0, away = false) {
  const c = WC.robinHat || "#3f7d3a", ln = darker(c), fc = WC.robinFeather || "#d2382b", f = dir || 0;
  const crown = !f ? `M30 48 Q34 -6 100 -18 Q166 -6 170 48 Q100 ${away ? 40 : 66} 30 48 Z`
    : `M${f1(100 - f * 74)} 50 Q${f1(100 - f * 80)} -4 ${f1(100 - f * 10)} -14 Q${f1(100 + f * 60)} -6 ${f1(100 + f * 96)} 54 Q${f1(100 + f * 40)} 44 ${f1(100 - f * 74)} 50 Z`;
  const brim = !f ? `<path d="M26 46 Q100 ${away ? 62 : 58} 174 46 L176 36 Q100 46 24 36 Z" fill="${ln}"/>`
    : `<path d="M${f1(100 - f * 76)} 52 Q${f1(100 - f * 96)} 30 ${f1(100 - f * 100)} 8 Q${f1(100 - f * 70)} 30 ${f1(100 - f * 60)} 46 Z" fill="${ln}" stroke="${darker(ln)}" stroke-width="2"/>`;
  const fx = f ? 100 - f * 40 : 140, fy = 30, tip = f ? [100 - f * 140, -70] : [196, -76];
  const feather = `<path d="M${f1(fx)} ${f1(fy)} Q${f1((fx + tip[0]) / 2 + (f ? f * 10 : -14))} ${f1(fy - 70)} ${f1(tip[0])} ${f1(tip[1])} Q${f1((fx + tip[0]) / 2 + (f ? -f * 14 : 18))} ${f1(fy - 40)} ${f1(fx)} ${f1(fy)} Z" fill="${fc}" stroke="${darker(fc)}" stroke-width="2"/>` +
    `<path d="M${f1(fx)} ${f1(fy)} Q${f1((fx + tip[0]) / 2)} ${f1(fy - 56)} ${f1(tip[0])} ${f1(tip[1])}" fill="none" stroke="${darker(fc)}" stroke-width="1.5"/>`;
  return feather + `<path d="${crown}" fill="${c}" stroke="${ln}" stroke-width="3" stroke-linejoin="round"/>` + brim;
}
// A bow through the hand at H, in body units: a curved wooden bow, its back toward the side `side` the hand is on, its
// string straight between its tips through the hand
function bowSvg([hx, hy], side) {
  const t = [hx, hy - 78], b = [hx, hy + 78], bx = hx + side * 30;
  return `<path d="M${f1(t[0])} ${f1(t[1])} Q${f1(bx)} ${f1(hy)} ${f1(b[0])} ${f1(b[1])}" fill="none" stroke="#7a4f2a" stroke-width="6" stroke-linecap="round"/>` +
    `<line x1="${f1(t[0])}" y1="${f1(t[1])}" x2="${f1(b[0])}" y2="${f1(b[1])}" stroke="#e8e0cc" stroke-width="1.5"/>`;
}
// A quiver of arrows, in body units, slung on the back from the left shoulder down to the right hip: the case, a band, and the
// arrows' fletching out of its top; from the front only that fletching shows, over the shoulder. (ticket 445, Matthew: "L and
// R facing poses are mis-placing the quiver") In profile (dir, the way they face), on the back: the case along the back, just
// behind it, leaning a little back at its top, the fletching above it.
function quiverSvg(w, L, away, dir = 0) {
  const top = dir ? [-dir * (w + 16), -48] : [-(w - 6), -34], bot = dir ? [-dir * (w + 6), L * 0.62] : [w - 16, L * 0.62];
  const ang = Math.atan2(bot[1] - top[1], bot[0] - top[0]) * 180 / Math.PI - 90;
  const fletch = [-10, 0, 10].map((dx, i) => `<path d="M${f1(top[0] + dx)} ${f1(top[1] - 4)} l-5 -18 l5 6 l5 -6 Z" fill="${["#d2382b", "#f4f1ea", "#e3b72f"][i]}" stroke="#5a3d22" stroke-width="1"/>` +
    `<line x1="${f1(top[0] + dx)}" y1="${f1(top[1] - 4)}" x2="${f1(top[0] + dx)}" y2="${f1(top[1] + 10)}" stroke="#7a4f2a" stroke-width="2"/>`).join("");
  if (!away && !dir) return fletch;
  const len = Math.hypot(bot[0] - top[0], bot[1] - top[1]);
  return fletch + `<g transform="translate(${f1(top[0])} ${f1(top[1])}) rotate(${f1(ang)})"><rect x="-14" y="0" width="28" height="${f1(len)}" rx="8" fill="#7a4f2a" stroke="#4a2f16" stroke-width="2.5"/>` +
    `<rect x="-14" y="${f1(len * 0.2)}" width="28" height="6" fill="#4a2f16"/></g>`;
}
// (ticket 435) a knight's shield on his arm, a heater shield in his arms (armsFill), 76 tall; from behind, its back (his
// sword is the trap room's, swordSvg in js/13-castle-rooms-and-map.js, held upright)
function shieldSvg([hx, hy], WC, id, away) {
  const d = `M${f1(hx - 30)} ${f1(hy - 38)} L${f1(hx + 30)} ${f1(hy - 38)} L${f1(hx + 30)} ${f1(hy - 4)} Q${f1(hx + 28)} ${f1(hy + 24)} ${f1(hx)} ${f1(hy + 38)} Q${f1(hx - 28)} ${f1(hy + 24)} ${f1(hx - 30)} ${f1(hy - 4)} Z`;
  return away ? `<path d="${d}" fill="#7a4f2a" stroke="#3b3b44" stroke-width="2.5"/><rect x="${f1(hx - 4)}" y="${f1(hy - 30)}" width="8" height="56" fill="#4a2f16"/>`
              : armsFill(id, d, WC, false, [hx, hy - 6], 0.9, [hx, hy - 6], 46);
}
// A wizard's staff through the hand at H, in body units: a straight wooden staff, leaning a little, 150 above the hand and
// down to 110 below it, knotted at its head round an orb that glows in its colour.
// (ticket 433: a monk's ends in a knob, "knob"; a dryad's sprouts leaves, "leaves")
function staffSvg([hx, hy], orb) {
  const top = [hx + 10, hy - 150], b = [hx - 6, hy + 110];
  if (orb === "knob" || orb === "leaves") return line(top[0], top[1] - 6, b[0], b[1], 7, "#7a4f2a", "#4a2f16") +
    (orb === "knob" ? `<circle cx="${f1(top[0])}" cy="${f1(top[1] - 8)}" r="8" fill="#7a4f2a" stroke="#4a2f16" stroke-width="2"/>`
      : [[-1, -10, -30], [1, -2, 25], [-1, 8, -55], [1, 14, 55]].map(([s, dy, a]) => `<path d="M0 0 Q${f1(s * 6)} -10 0 -20 Q${f1(-s * 6)} -10 0 0 Z" transform="translate(${f1(top[0])} ${f1(top[1] + dy)}) rotate(${a})" fill="#4caf50" stroke="#2e6b30" stroke-width="1.5"/>`).join(""));
  return line(top[0], top[1], b[0], b[1], 7, "#7a4f2a", "#4a2f16") +
    `<path d="M${f1(top[0] - 9)} ${f1(top[1] + 4)} Q${f1(top[0] - 12)} ${f1(top[1] - 14)} ${f1(top[0])} ${f1(top[1] - 20)} Q${f1(top[0] + 12)} ${f1(top[1] - 14)} ${f1(top[0] + 9)} ${f1(top[1] + 4)}" fill="none" stroke="#7a4f2a" stroke-width="5" stroke-linecap="round"/>` +
    `<circle cx="${f1(top[0])}" cy="${f1(top[1] - 10)}" r="22" fill="${orb}" opacity="0.25"/>` +
    `<circle cx="${f1(top[0])}" cy="${f1(top[1] - 10)}" r="11" fill="${orb}" stroke="${darker(orb)}" stroke-width="2"/>` +
    `<circle cx="${f1(top[0] - 3.5)}" cy="${f1(top[1] - 14)}" r="3.5" fill="#ffffff" opacity="0.8"/>`;
}
// (tickets 436, 437, Matthew: "queen." "king.") A CROWN, in the round head's units, on the head (the hair tucked under its
// band, as under a hat): a gold band from 40 up to 20, points rising from it (the king's five and tall, the queen's three
// and dainty), a velvet cap inside in the robe's colour, pearls on the points and jewels on the band (facing us; none from
// behind).
function crownSvg(WC, king, cap, away = false) {
  const gold = "#e0b030", ln = "#8a6420", j = WC.crownJewel || "#d2382b", n = king ? 5 : 3, top = king ? -18 : -2;
  const xs = Array.from({ length: n }, (_, i) => 40 + 120 * (i + 0.5) / n);
  let pts = `M38 40 L38 20`;
  for (let i = 0; i < n; i++) pts += ` L${f1(xs[i] - 60 / n)} 20 L${f1(xs[i])} ${f1(top + (king && i % 2 ? 10 : 0))} L${f1(xs[i] + 60 / n)} 20`;
  pts += ` L162 20 L162 40 Z`;
  return `<path d="M44 22 Q100 ${king ? -40 : -24} 156 22 Z" fill="${cap}" stroke="${darker(cap)}" stroke-width="2"/>` +
    `<path d="${pts}" fill="${gold}" stroke="${ln}" stroke-width="2.5" stroke-linejoin="round"/>` +
    xs.map((x, i) => `<circle cx="${f1(x)}" cy="${f1(top + (king && i % 2 ? 10 : 0) - 2)}" r="4" fill="#f7f4ec" stroke="#c9c5bd" stroke-width="1"/>`).join("") +
    `<rect x="38" y="31" width="124" height="4" fill="${ln}" opacity="0.5"/>` +
    (away ? "" : [72, 100, 128].map((x, i) => `<rect x="${f1(x - 5)}" y="25" width="10" height="10" rx="2" transform="rotate(45 ${x} 30)" fill="${i === 1 ? j : mix(j, "#2f6db5", 0.6)}" stroke="#8a6420" stroke-width="1"/>`).join(""));
}
// A scepter through the hand at H, in body units: a gold rod 130 above the hand and 30 below, a jewel and a small cross on top
function scepterSvg([hx, hy], jewel) {
  const t = [hx + 6, hy - 130];
  return line(hx - 2, hy + 30, t[0], t[1], 6, "#e0b030", "#8a6420") +
    `<circle cx="${f1(t[0])}" cy="${f1(t[1] - 8)}" r="11" fill="#e0b030" stroke="#8a6420" stroke-width="2"/><circle cx="${f1(t[0])}" cy="${f1(t[1] - 8)}" r="5" fill="${jewel}"/>` +
    `<path d="M${f1(t[0])} ${f1(t[1] - 19)} L${f1(t[0])} ${f1(t[1] - 33)} M${f1(t[0] - 6)} ${f1(t[1] - 27)} L${f1(t[0] + 6)} ${f1(t[1] - 27)}" stroke="#e0b030" stroke-width="3.5" stroke-linecap="round"/>`;
}
// (ticket 522) A cowboy hat (COW_HATS[W.hatId] in W.hatColor), in the round head's units (x 100 the middle, the brim's line
// at COW_HAT_Y), stretched with the head as the witch's is: each view its own drawing. Facing us (and from behind, without
// what is on its front: the pinches, a big bow, a big star; a small bow or star at the back instead) a crown with a crease (or tall, or flat), its band, a brim curled up at
// both sides, rxHair widening it over big hair (at most half again); in profile the crown sloping down to its front, one
// dent, the band, the brim nearly edge on with its back turned up, a small star on the crown's side, a bow at the front and its back bow at the back. The feather
// is on the friend's right: our left facing us, our right from behind; in profile over the crown facing our right (their
// right side near), behind it facing our left. The straw's ribbon tails hang down the back, seen from behind.
function cowHatSvg(W, dir = 0, away = false, rxHair = 0) {
  const H = W.H, c = W.hatColor, ln = darker(c), y0 = COW_HAT_Y, cw = 66, top = H.top, curl = H.curl;
  const bw = Math.min(H.bw * 1.5, Math.max(H.bw, rxHair)), P = (x, y) => f1(x) + " " + f1(y);
  const light = parseInt(c.slice(1, 3), 16) + parseInt(c.slice(3, 5), 16) + parseInt(c.slice(5, 7), 16) > 600;
  const band = H.band === "darker" ? mix(c, "#000000", 0.45) : H.band === "bow" ? (light ? "#ec4899" : "#ffffff") : H.band;
  const fill = (d, f, sw = 3) => `<path d="${d}" fill="${f}" stroke="${darker(f)}" stroke-width="${sw}" stroke-linejoin="round"/>`;
  const thin = (d, col, sw, extra = "") => `<path d="${d}" fill="none" stroke="${col}" stroke-width="${sw}" stroke-linecap="round"${extra}/>`;
  const conchos = pts => `<path d="${pts.map(([x, y]) => `M${P(x - 4, y)} a4 4 0 1 0 8 0 a4 4 0 1 0 -8 0`).join(" ")}" fill="#d1d5db" stroke="#6b7280" stroke-width="1"/>`;
  const feather = (bx, by, tx, ty, bend) => fill(`M${P(bx, by)} Q${P((bx + tx) / 2 + bend, (by + ty) / 2 - 6)} ${P(tx, ty)} Q${P((bx + tx) / 2 - bend * 0.6, (by + ty) / 2 + 10)} ${P(bx, by)} Z`, H.feather, 2) +
                                            thin(`M${P(bx, by)} Q${P((bx + tx) / 2 + bend * 0.2, (by + ty) / 2)} ${P(tx, ty)}`, darker(H.feather), 1.5);
  const weave = ` stroke-dasharray="6 4" opacity="0.7"`, out = [];
  if (!dir) {
    const mx = away ? -1 : 1, mid = (y0 + top) / 2;
    const crown = H.crown === "flat" ? `M${P(100 - cw, y0 + 6)} L${P(100 - cw + 6, top)} Q${P(100, top - 3)} ${P(100 + cw - 6, top)} L${P(100 + cw, y0 + 6)} Z`
      : (() => { const tall = H.crown === "tall", dip = tall ? 9 : 13, bul = tall ? 9 : 3;
          return `M${P(100 - cw, y0 + 6)} Q${P(100 - cw - bul, mid)} ${P(100 - cw * 0.8, top + 8)} Q${P(100 - cw * 0.62, top - 3)} ${P(100 - cw * 0.34, top + 1)} ` +
                 `Q${P(100, top + dip)} ${P(100 + cw * 0.34, top + 1)} Q${P(100 + cw * 0.62, top - 3)} ${P(100 + cw * 0.8, top + 8)} Q${P(100 + cw + bul, mid)} ${P(100 + cw, y0 + 6)} Z`; })();
    out.push(fill(crown, c));
    if (!away && H.crown !== "flat")
      out.push(thin(`M${P(100 - cw * 0.44, top + 12)} Q${P(100 - cw * 0.3, top + 26)} ${P(100 - cw * 0.38, top + 42)} M${P(100 + cw * 0.44, top + 12)} Q${P(100 + cw * 0.3, top + 26)} ${P(100 + cw * 0.38, top + 42)}`, ln, 2.5));
    out.push(`<path d="M${P(100 - cw - 1, y0 - 13)} Q${P(100, y0 - 7)} ${P(100 + cw + 1, y0 - 13)} L${P(100 + cw + 1, y0 + 2)} Q${P(100, y0 + 8)} ${P(100 - cw - 1, y0 + 2)} Z" fill="${band}"/>`);
    if (H.conchos) out.push(conchos([[100 - cw * 0.62, y0 - 6], [100, y0 - 2], [100 + cw * 0.62, y0 - 6]]));
    if (H.star && !away) out.push(`<polygon points="${starPts(100, mid + 4, 15)}" fill="${H.star}" stroke="#8a6420" stroke-width="2" stroke-linejoin="round"/>`);
    if (H.feather) out.push(feather(100 - mx * (cw - 10), y0 - 8, 100 - mx * (cw + 30), top - 36, -mx * 16));
    out.push(fill(`M${P(100 - bw, y0 - curl)} Q${P(100 - bw * 0.55, y0 + 3)} ${P(100, y0 + 4)} Q${P(100 + bw * 0.55, y0 + 3)} ${P(100 + bw, y0 - curl)} ` +
                  `Q${P(100 + bw * 0.62, y0 + 17)} ${P(100, y0 + 18)} Q${P(100 - bw * 0.62, y0 + 17)} ${P(100 - bw, y0 - curl)} Z`, c));
    if (H.weave) out.push(thin(`M${P(100 - cw * 0.86, top + 30)} Q${P(100, top + 38)} ${P(100 + cw * 0.86, top + 30)} M${P(100 - cw * 0.95, top + 50)} Q${P(100, top + 58)} ${P(100 + cw * 0.95, top + 50)} ` +
                               `M${P(100 - bw * 0.8, y0 + 2 - curl * 0.5)} Q${P(100, y0 + 19)} ${P(100 + bw * 0.8, y0 + 2 - curl * 0.5)}`, ln, 2, weave));
    // (from behind, a bow hat's small bow at the back of its band, its loops and tails; a star hat's small star on its band)
    if (H.bow && away) out.push(fill(`M100 ${y0 - 5} Q80 ${y0 - 24} 74 ${y0 - 8} Q78 ${y0 + 6} 100 ${y0 - 5} Z M100 ${y0 - 5} Q120 ${y0 - 24} 126 ${y0 - 8} Q122 ${y0 + 6} 100 ${y0 - 5} Z ` +
                                     `M97 ${y0 - 2} L88 ${y0 + 34} L95 ${y0 + 31} Z M103 ${y0 - 2} L112 ${y0 + 34} L105 ${y0 + 31} Z`, band, 2) +
                                `<circle cx="100" cy="${y0 - 5}" r="6" fill="${band}" stroke="${darker(band)}" stroke-width="2"/>`);
    if (H.star && away) out.push(`<polygon points="${starPts(100, y0 - 6, 8)}" fill="${H.star}" stroke="#8a6420" stroke-width="1.5" stroke-linejoin="round"/>`);
    if (H.tails && away) out.push(thin(`M${P(97, y0 + 10)} L${P(88, y0 + 48)} M${P(103, y0 + 10)} L${P(114, y0 + 44)}`, band, 6));
    if (H.bow && !away) {
      const by = y0 - 4;   // (a big bow over the band and the brim's front: two loops, two tails, then its knot)
      out.push(fill(`M100 ${by} Q66 ${by - 34} 58 ${by - 8} Q62 ${by + 16} 100 ${by} Z M100 ${by} Q134 ${by - 34} 142 ${by - 8} Q138 ${by + 16} 100 ${by} Z ` +
                    `M96 ${by + 4} L80 ${by + 36} L92 ${by + 32} Z M104 ${by + 4} L120 ${by + 36} L108 ${by + 32} Z`, band, 2.5));
      out.push(`<circle cx="100" cy="${by}" r="9" fill="${band}" stroke="${darker(band)}" stroke-width="2.5"/>`);
    }
    return out.join("");
  }
  const X = d => 100 + dir * d, mid = (y0 + top) / 2, near = dir > 0;
  const featherP = H.feather ? feather(X(-cw * 0.1), y0 - 8, X(-cw - 34), top - 30, dir * 18) : "";
  if (featherP && !near) out.push(featherP);
  const crown = H.crown === "flat" ? `M${P(X(-cw), y0 + 2)} L${P(X(-cw + 6), top)} L${P(X(cw - 6), top + 2)} L${P(X(cw), y0 + 2)} Z`
    : (() => { const bul = H.crown === "tall" ? 9 : 3, back = H.crown === "tall" ? -6 : 0;   // (the ten-gallon's tall crown sloping a little back)
        return `M${P(X(-cw), y0 + 2)} Q${P(X(-cw - bul), mid)} ${P(X(-cw * 0.82 + back), top + 6)} Q${P(X(-cw * 0.35 + back), top - 4)} ${P(X(cw * 0.1 + back), top + 4)} ` +
               `Q${P(X(cw * 0.45), top + 8)} ${P(X(cw * 0.72), top + 16)} Q${P(X(cw + bul), mid + 6)} ${P(X(cw), y0 + 2)} Z`; })();
  out.push(fill(crown, c));
  if (H.crown !== "flat") out.push(thin(`M${P(X(cw * 0.5), top + 20)} Q${P(X(cw * 0.62), top + 32)} ${P(X(cw * 0.56), top + 46)}`, ln, 2.5));
  out.push(`<path d="M${P(X(-cw - 1), y0 - 13)} L${P(X(cw + 1), y0 - 13)} L${P(X(cw + 1), y0 + 2)} L${P(X(-cw - 1), y0 + 2)} Z" fill="${band}"/>`);
  if (H.conchos) out.push(conchos([[X(-cw * 0.5), y0 - 5], [X(cw * 0.35), y0 - 5]]));
  if (featherP && near) out.push(featherP);
  // (in profile the star hat's star is its own small star on the crown's side, as the feather is the feather's)
  if (H.star) out.push(`<polygon points="${starPts(X(cw * 0.32), mid + 2, 12)}" fill="${H.star}" stroke="#8a6420" stroke-width="2" stroke-linejoin="round"/>`);
  out.push(fill(`M${P(X(-bw), y0 - curl)} Q${P(X(-bw * 0.5), y0 + 8)} ${P(X(0), y0 + 8)} Q${P(X(bw * 0.55), y0 + 8)} ${P(X(bw), y0 - curl * 0.3)} ` +
                `Q${P(X(bw * 0.55), y0 + 1)} ${P(X(0), y0 + 1)} Q${P(X(-bw * 0.5), y0 + 1)} ${P(X(-bw), y0 - curl)} Z`, c));
  if (H.weave) out.push(thin(`M${P(X(-cw * 0.9), top + 32)} L${P(X(cw * 0.8), top + 40)} M${P(X(-bw * 0.8), y0 + 3)} Q${P(X(0), y0 + 6)} ${P(X(bw * 0.85), y0 + 3)}`, ln, 2, weave));
  if (H.bow) {
    out.push(fill(`M${P(X(cw), y0 - 6)} Q${P(X(cw + 28), y0 - 30)} ${P(X(cw + 22), y0 - 2)} Z M${P(X(cw + 1), y0 - 3)} L${P(X(cw + 10), y0 + 26)} L${P(X(cw + 3), y0 + 23)} Z`, band, 2));
    out.push(`<circle cx="${f1(X(cw + 1))}" cy="${y0 - 5}" r="6" fill="${band}" stroke="${darker(band)}" stroke-width="2"/>`);
    // (and the back bow's loop and tail at the crown's back)
    out.push(fill(`M${P(X(-cw), y0 - 6)} Q${P(X(-cw - 18), y0 - 22)} ${P(X(-cw - 14), y0 - 2)} Z M${P(X(-cw - 1), y0 - 3)} L${P(X(-cw - 8), y0 + 26)} L${P(X(-cw - 2), y0 + 23)} Z`, band, 2));
  }
  return out.join("");
}
// (ticket 522) A lasso held in the fist at H, in body units, the world's down below it: three coils of rope, each a little
// bigger and further out, and its free end hanging inside them, as ONE path drawn twice (a darker rope under, the rope over
// it dashed so its twist shows), and the honda, the small eye at the loop's end: 3 shapes, no transform, no animation.
// view "front" (facing us: round loops hanging toward the hand's own side, out; the honda low and outward), "back" (the
// same, the honda on the other side) or "side" (edge on, a third as wide, the honda toward the face, out the way they face).
// spin (running), LFO B's phase: an open loop held out and up beside the hand, turning through four drawings (half
// widths 44, 30, 12, 30), as the tin robot's key turns.
function lassoSvg([hx, hy], view, out, spin = null) {
  const P = (x, y) => f1(x) + " " + f1(y), ring = (cx, cy, rx, ry) => `M${P(cx - rx, cy)} a${f1(rx)} ${f1(ry)} 0 1 0 ${f1(2 * rx)} 0 a${f1(rx)} ${f1(ry)} 0 1 0 ${f1(-2 * rx)} 0`;
  let d, honda;
  if (spin != null) {
    // (the fourth drawing as wide as the second, but turned on round: the rope meets the loop, and the honda is, at its far side)
    const step = Math.floor((((spin % 1) + 1) % 1) * 4) % 4, rx = [44, 30, 12, 30][step], cx = hx + out * 52, cy = hy - 46, ex = cx - (step === 3 ? -1 : 1) * out * rx;
    d = `M${P(hx, hy)} Q${P(hx + out * 20, hy - 30)} ${P(ex, cy)} ` + ring(cx, cy, rx, 13);
    honda = [ex, cy];
  } else {
    const side = view === "side", k = side ? 0.32 : 1;
    const loops = [0, 1, 2].map(i => [hx + out * (side ? 2 + 2 * i : 5 + 5 * i), hy + 25 + 3 * i, (19 + 2.5 * i) * k, 26 + 3 * i]);
    d = loops.map(([cx, cy, rx, ry]) => `M${P(hx, hy - 2)} Q${P(cx - out * rx * 0.6, cy - ry * 0.9)} ${P(cx - out * rx, cy)} ` +
          `A${f1(rx)} ${f1(ry)} 0 1 ${out > 0 ? 0 : 1} ${P(cx + out * rx, cy)} Q${P(cx + out * rx * 0.6, cy - ry * 0.9)} ${P(hx, hy - 2)}`).join(" ") +
        ` M${P(hx, hy + 2)} Q${P(hx - out * 10, hy + 22)} ${P(hx - out * 4, hy + 44)}`;
    const [cx, cy, rx, ry] = loops[2], s = view === "back" ? -1 : 1;
    honda = side ? [cx + out * rx * 0.9, cy + ry * 0.5] : [cx + s * out * rx * 0.75, cy + ry * 0.62];
  }
  return `<path d="${d}" fill="none" stroke="${ROPE_DK}" stroke-width="7" stroke-linecap="round" stroke-linejoin="round"/>` +
         `<path d="${d}" fill="none" stroke="${ROPE}" stroke-width="4.2" stroke-linecap="round" stroke-dasharray="7 2.5"/>` +
         `<ellipse cx="${f1(honda[0])}" cy="${f1(honda[1])}" rx="5.5" ry="4.5" fill="none" stroke="${ROPE_DK}" stroke-width="3.5"/>`;
}
// (ticket 438, Matthew: "horse in all poses we have for people that 'make sense' for using in the game as a way to ride, or
// walk/run with (holding the horse's reins, and walking/running next to it). The horse need not sit, or crawl, or swim, etc.
// Just walking and running, and standing still: towards, away and L R poses, just our main character.") THE FRIEND'S HORSE, in
// the friend's units, standing on the ground at (0, g): its withers about the friend's shoulders. Three drawings of its own,
// none a turn of another: from the side (facing our right; mirrored for our left, as a horse is the same both sides), facing
// us (its face the stable's horses' own, horseFace, still), and from behind. Its legs step with the friend: walking, a
// four-beat walk (left hind, left fore, right hind, right fore, a quarter of a stride apart) on the friend's own stride (u,
// LFO B's phase); running, a canter, twice as quick, the hind pair then the fore pair, the body rising and falling;
// standing, square. H: its coat, mane and face marking (the stable's: HORSE_COATS, HORSE_MANES, HORSE_MARKS), its tack's
// color, white socks. Returned with it, `ring`: where a lead rein is tied on (the noseband's ring under its chin). Ridden
// (`ride`), it wears a saddle, `bits` are where the reins go (each side of its mouth, or of its neck from behind), `seat`
// is where the rider's hips go, `bob` how far the stride lifts them, and facing us or away it comes in two parts: `svg`,
// behind the rider, and `over`, before them (facing us its chest, forelegs, neck and face, the head carried lower; from
// behind its haunches, hind legs and tail), so the rider's legs hang down its sides.
// (ticket 457) the horse at 100 (body.horseSize): as big as the stable's horses at the same depth, face to face (their
// faces 2.1 times the friend's horse's as first drawn, measured in the stable at the stalls' depth)
const HORSE_SCALE = 2.1;
function horseLeg(top, len, swing, flex, fore, w, coat, sock, g, hoofOut = null, shoe = null) {
  // (a leg from its top down: an upper bone swinging by `swing` degrees (forward, the way it faces), the lower bending at
  // the knee (fore: the hoof folding back) or the hock by `flex`, a hoof; its outline round the bones, tapering)
  // (a hind leg at rest: its gaskin slanting back to the hock, its cannon straight down; bending, the hock goes further
  // back and the hoof comes forward under it)
  const rest = fore ? 0 : 8, a1 = (swing - rest - (fore ? 0 : 0.35 * flex)) * Math.PI / 180, a2 = (swing + (fore ? -flex : 0.65 * flex)) * Math.PI / 180;
  const up = len * (fore ? 0.48 : 0.5), lo = len * (fore ? 0.44 : 0.43);
  const K = [top[0] + Math.sin(a1) * up, top[1] + Math.cos(a1) * up], F = [K[0] + Math.sin(a2) * lo, K[1] + Math.cos(a2) * lo];
  const out = (P, Q, wa, wb) => { const dx = Q[0] - P[0], dy = Q[1] - P[1], d = Math.hypot(dx, dy) || 1, nx = -dy / d, ny = dx / d;
    return [[P[0] + nx * wa, P[1] + ny * wa], [Q[0] + nx * wb, Q[1] + ny * wb], [Q[0] - nx * wb, Q[1] - ny * wb], [P[0] - nx * wa, P[1] - ny * wa]]; };
  const u1 = out(top, K, w, w * 0.6), u2 = out(K, F, w * 0.55, w * 0.42), pt = ([x, y]) => `${f1(x)},${f1(y)}`;
  const M = [K[0] + (F[0] - K[0]) * 0.45, K[1] + (F[1] - K[1]) * 0.45];
  // (the hoof: along the lower bone, flat on the ground when it is straight down)
  const dx = Math.sin(a2), dy = Math.cos(a2), hx = -dy, hy = dx, hl = 14, hw = w * 0.6;
  const hoof = [[F[0] + hx * hw, F[1] + hy * hw], [F[0] - hx * hw, F[1] - hy * hw], [F[0] - hx * hw * 1.1 + dx * hl, F[1] - hy * hw * 1.1 + dy * hl], [F[0] + hx * hw * 1.1 + dx * hl, F[1] + hy * hw * 1.1 + dy * hl]];
  if (hoofOut) hoofOut.push([F[0] + dx * hl * 0.5, F[1]]);   // (ticket 492: where the hoof is, for its collision box)
  return `<polygon points="${[u1[0], u1[1], u2[1], u2[2], u1[2], u1[3]].map(pt).join(" ")}" fill="${coat}" stroke="${mix(coat, "#000000", 0.42)}" stroke-width="2.5" stroke-linejoin="round"/>` +
    (sock ? `<polygon points="${out(M, F, w * 0.5, w * 0.44).map(pt).join(" ")}" fill="#f7f4ee"/>` : "") +
    `<polygon points="${hoof.map(pt).join(" ")}" fill="#3b2f2a" stroke="#1f1612" stroke-width="2" stroke-linejoin="round"/>` +
    // (ticket 495, for M) its shoe: along the hoof's sole, in its colour
    (shoe ? (([a, b]) => `<line x1="${f1(a[0])}" y1="${f1(a[1])}" x2="${f1(b[0])}" y2="${f1(b[1])}" stroke="${shoe}" stroke-width="4.5" stroke-linecap="round"/>`)(
      [0.12, 0.88].map(u => [hoof[3][0] + (hoof[2][0] - hoof[3][0]) * u, hoof[3][1] + (hoof[2][1] - hoof[3][1]) * u])) : "");
}
// (ticket 497, for M) a mane falling down one side of the neck, seen from the front or from behind, in its style (not
// "loose": that one each view draws as it always has): along the line from a (its top, at the poll) to b (at the withers),
// out to the side sx (1, our right; -1, our left). bow: the bows' ribbon colour
function maneFall(style, a, b, sx, mane, maneD, bow) {
  const at = (t, o) => [a[0] + (b[0] - a[0]) * t + sx * o, a[1] + (b[1] - a[1]) * t], P = p => `${f1(p[0])} ${f1(p[1])}`;
  const fill = `fill="${mane}" stroke="${maneD}" stroke-width="2" stroke-linejoin="round"`;
  const strip = w => `<path d="M${P(at(0, 0))} L${P(at(1, 0))} L${P(at(1, w))} L${P(at(0, w))} Z" ${fill}/>`;
  if (style === "long") {
    const out = []; for (let i = 0; i <= 10; i++) out.push(at(1.2 * (1 - i / 10), 20 + 7 * Math.sin(i * 1.7) + (i % 2 ? -5 : 5)));
    return `<path d="M${P(at(0, -2))} L${P(at(1.2, -2))} L${P(out[0])} ${maneSmooth(out)} Z" ${fill}/>`;
  }
  if (style === "brush") {
    let d = `M${P(at(0, 0))} L${P(at(1, 0))}`; for (let i = 0; i <= 14; i++) d += ` L${P(at(1 - i / 14, i % 2 ? 7 : 12))}`;
    return `<path d="${d} Z" ${fill}/>`;
  }
  if (style === "buttons") return strip(5) + [0.08, 0.27, 0.46, 0.65, 0.84].map(t => maneKnot(at(t, 8), 6, mane, maneD)).join("");
  if (style === "curly") return strip(8) + [0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95].map((t, k) => maneCurl(at(t, k % 2 ? 15 : 9), 7, mane, maneD)).join("");
  if (style === "bows") return strip(5) + [0.15, 0.5, 0.85].map(t => maneBraid(at(t, 5), [sx * 0.25, 1], 38, mane, maneD, bow)).join("");
  return "";
}
// a smooth edge through points: each a control point, the curve passing between them
const maneSmooth = pts => pts.slice(1, -1).map((p, i) => `Q${f1(p[0])} ${f1(p[1])} ${f1((p[0] + pts[i + 2][0]) / 2)} ${f1((p[1] + pts[i + 2][1]) / 2)}`).join(" ") +
  ` L${f1(pts[pts.length - 1][0])} ${f1(pts[pts.length - 1][1])}`;
// a button braid: a knot of the mane rolled up, its plaits showing
const maneKnot = ([x, y], r, mane, maneD) => `<circle cx="${f1(x)}" cy="${f1(y)}" r="${r}" fill="${mane}" stroke="${maneD}" stroke-width="2"/>` +
  `<path d="M${f1(x - r * 0.55)} ${f1(y - r * 0.3)} q${f1(r * 0.55)} ${f1(r * 0.45)} ${f1(r * 1.1)} 0 M${f1(x - r * 0.55)} ${f1(y + r * 0.3)} q${f1(r * 0.55)} ${f1(r * 0.45)} ${f1(r * 1.1)} 0" fill="none" stroke="${maneD}" stroke-width="1.3" stroke-linecap="round"/>`;
// a curl: a ring of the mane, turning in
const maneCurl = ([x, y], r, mane, maneD) => `<circle cx="${f1(x)}" cy="${f1(y)}" r="${r}" fill="${mane}" stroke="${maneD}" stroke-width="2"/>` +
  `<path d="M${f1(x + r * 0.55)} ${f1(y)} A${f1(r * 0.55)} ${f1(r * 0.55)} 0 1 0 ${f1(x)} ${f1(y + r * 0.55)} A${f1(r * 0.3)} ${f1(r * 0.3)} 0 1 0 ${f1(x + r * 0.15)} ${f1(y - r * 0.05)}" fill="none" stroke="${maneD}" stroke-width="1.5" stroke-linecap="round"/>`;
// a plait hanging from s along dir, len long, its strands crossing, tied off with a bow
function maneBraid(s, dir, len, mane, maneD, bow) {
  const n = Math.hypot(dir[0], dir[1]), d = [dir[0] / n, dir[1] / n], at = k => [s[0] + d[0] * k, s[1] + d[1] * k], ang = Math.atan2(d[1], d[0]) * 180 / Math.PI - 90;
  let g = "";
  for (let j = 0; j < 4; j++) { const [x, y] = at(5 + j * (len - 10) / 4); g += `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="4.4" ry="6.2" transform="rotate(${f1(ang + (j % 2 ? 28 : -28))} ${f1(x)} ${f1(y)})" fill="${mane}" stroke="${maneD}" stroke-width="1.6"/>`; }
  const [ex, ey] = at(len), bowD = mix(bow, "#000000", 0.35);
  return g + `<path d="M${f1(ex)} ${f1(ey)} l-4 12 M${f1(ex)} ${f1(ey)} l4 12" stroke="${bow}" stroke-width="3" stroke-linecap="round"/>` +
    `<path d="M${f1(ex)} ${f1(ey)} l-12 -7 l0 14 Z M${f1(ex)} ${f1(ey)} l12 -7 l0 14 Z" fill="${bow}" stroke="${bowD}" stroke-width="1.4" stroke-linejoin="round"/><circle cx="${f1(ex)}" cy="${f1(ey)}" r="3.2" fill="${bow}" stroke="${bowD}" stroke-width="1.2"/>`;
}
// (ticket 498, Matthew: "let's start to think of costumes for the knight as having horse pieces (face helmets, look at
// images from Lego castles and knights) -- not just saddles"; then "just release and let the kids decide") The horse's
// armour, body.horseArmour (HORSE_ARMOURS), as the mock-up drew it (horse-armour.html, a page outside the repo; its shapes
// are the side view's here): 🛡️ full plate (a chanfron down its face with a plume, a crinet of plates down its neck's
// crest, a peytral over its chest, flanchards at its sides, a crupper over its rump); 🦁 half caparison (a steel chanfron, a
// cloth to below its belly, its hem scalloped, the emblem on its side); 🎪 tournament (a cloth over everything: its head
// with eye holes and ear sleeves, its neck, and down to its hooves, the hem dagged, a plume); 🐉 dragon helm (a dragon's
// head for a chanfron with horns, spikes and teeth, a scaly crinet, a cloth with a dagged hem); 🧱 toy helmet (one chunky
// moulded helmet with round eye holes and a plume in a clip, as on a toy castle's horse, and a short cloth); 🎀 parade
// (rosettes and ribbons on its bridle, a plume, a saddle cloth with tassels). Each piece is drawn from the side, from the
// front and from behind, each a drawing of its own (Matthew's rule for new angles). The cloth and the plume take the
// knight's arms when the friend wears the knight costume (horseArms), else the halter's colour and a light shade of it,
// with no emblem. The pieces are drawn on the body at rest and lifted with the stride's bob; the plumes and the parade's
// ribbons swing with the stride, as the tail does, and hang still while it stands (the friend's horse has no animations of
// its own: its face is drawn `still`).
const HORSE_STEEL = { S: "#b9c1ca", D: "#59626e", L: "#eef2f6" };
const HORSE_DRAGON = { S: "#4f7a52", D: "#22382a", L: "#9cc79a", horn: "#efe6cf", hornD: "#8a7f62", eye: "#c0392b" };
// (the sets with a cloth: its hem's height, and its cut)
const HORSE_CLOTHS = { half: [-150, "scallop"], tourney: [-34, "dag"], dragon: [-120, "dag"], toy: [-176, "scallop"] };
// the arms the armour's cloth and plume take: the knight's (a: state.costume, while it is the knight's), else the halter's
// colour and a light shade of it, quartered, no emblem, and a plume in the halter's colour
function horseArms(H) {
  const a = H.arms, t = H.tack || "#7a3a1a";
  return a ? { knightA: a.knightA || "#2f6db5", knightB: a.knightB || "#f4f1ea", tabardLayout: a.tabardLayout || "quartered", emblem: a.emblem || "none", plume: a.plumeColor || "#c0392b" }
    : { knightA: t, knightB: mix(t, "#ffffff", 0.6), tabardLayout: "quartered", emblem: "none", plume: t };
}
const swung = (sw, x, y, inner) => sw ? `<g transform="rotate(${f1(sw)} ${f1(x)} ${f1(y)})">${inner}</g>` : inner;
// a plume from the side: three feathers from a holder at (x, y), sweeping back (the mock-up's), swung by sw degrees
const horsePlumeSide = (x, y, c, tall, sw) => swung(sw, x, y, [[-40, 0], [-20, 6], [-58, -6]].map(([dx, dy], i) =>
  `<path d="M${f1(x)} ${f1(y)} C${f1(x - 6)} ${f1(y - 50 * tall)} ${f1(x + dx * 0.4)} ${f1(y - 80 * tall + dy)} ${f1(x + dx)} ${f1(y - 70 * tall + dy)} C${f1(x + dx * 0.5)} ${f1(y - 50 * tall)} ${f1(x + 4)} ${f1(y - 30 * tall)} ${f1(x + 4)} ${f1(y)} Z" fill="${i === 1 ? mix(c, "#ffffff", 0.35) : c}" stroke="${mix(c, "#000000", 0.35)}" stroke-width="1.5"/>`).join(""));
// a plume facing us: three feathers fanning up from (x, y), h tall, the middle one lighter, in front; short (ridden toward
// us, the head carried low before the rider), the plume sweeps back toward them and is seen end on: a low tuft of feather
// ends round its socket, the middle one lighter. The rider's chin is only about 2 above the socket there (in the face's
// units, the face 18 wide each way), so nothing rises past that (ticket 498, review: the upright tuft hid the rider's mouth,
// and the toy helmet hid their whole face)
const horsePlumeFront = (x, y, c, h, sw, short) => short ? swung(sw * 0.5, x, y, [-1, 1, -0.5, 0.5, 0].map((lx, i) => {
    const cx = x + lx * h * 0.3, cy = y + 0.5 + Math.abs(lx) * 1.5, r = h * (i > 3 ? 0.11 : 0.12);
    return `<ellipse cx="${f1(cx)}" cy="${f1(cy)}" rx="${f1(r * 1.2)}" ry="${f1(r * 0.6)}"${lx ? ` transform="rotate(${f1(lx * 25)} ${f1(cx)} ${f1(cy)})"` : ""} fill="${i > 3 ? mix(c, "#ffffff", 0.35) : c}" stroke="${mix(c, "#000000", 0.35)}" stroke-width="1.2"/>`; }).join(""))
  : swung(sw, x, y, [[-1, 0.86], [1, 0.86], [0, 1]].map(([lean, f]) => {
    const a = lean * h * 0.42, t = h * f, w = h * 0.2;
    return `<path d="M${f1(x - 2)} ${f1(y)} Q${f1(x + a * 0.2 - w)} ${f1(y - t * 0.55)} ${f1(x + a)} ${f1(y - t)} Q${f1(x + a * 0.2 + w)} ${f1(y - t * 0.55)} ${f1(x + 2)} ${f1(y)} Z" fill="${lean ? c : mix(c, "#ffffff", 0.35)}" stroke="${mix(c, "#000000", 0.35)}" stroke-width="1.2"/>`; }).join(""));
// a plume from behind: it sweeps back toward us, so two feathers arch up and over to each side, and the middle one rises
// short between them, its tip coming toward us
const horsePlumeBack = (x, y, c, h, sw) => swung(sw, x, y, [-1, 1].map(sd =>
  `<path d="M${f1(x)} ${f1(y)} C${f1(x + sd * h * 0.15)} ${f1(y - h * 1.15)} ${f1(x + sd * h * 0.7)} ${f1(y - h * 1.1)} ${f1(x + sd * h * 0.85)} ${f1(y - h * 0.4)} C${f1(x + sd * h * 0.55)} ${f1(y - h * 0.8)} ${f1(x + sd * h * 0.2)} ${f1(y - h * 0.75)} ${f1(x + sd * 3)} ${f1(y)} Z" fill="${c}" stroke="${mix(c, "#000000", 0.35)}" stroke-width="1.5"/>`).join("") +
  `<path d="M${f1(x - 3)} ${f1(y)} Q${f1(x - h * 0.2)} ${f1(y - h * 0.45)} ${f1(x)} ${f1(y - h * 0.72)} Q${f1(x + h * 0.2)} ${f1(y - h * 0.45)} ${f1(x + 3)} ${f1(y)} Z" fill="${mix(c, "#ffffff", 0.35)}" stroke="${mix(c, "#000000", 0.35)}" stroke-width="1.5"/>`);
// a rosette on the bridle at (x, y), r its petals' size: petals in the arms' first colour round a gold middle, two ribbons in
// the second hanging from it and swinging (sw)
const horseRosette = (x, y, r, K, sw) => swung(sw, x, y, `<path d="M${f1(x)} ${f1(y)} l${f1(-r * 0.9)} ${f1(r * 4.8)} M${f1(x)} ${f1(y)} l${f1(r * 1.1)} ${f1(r * 4.4)}" stroke="${mix(K.knightB, "#000000", 0.25)}" stroke-width="${f1(r * 1.15)}" stroke-linecap="round"/>` +
  `<path d="M${f1(x)} ${f1(y)} l${f1(-r * 0.9)} ${f1(r * 4.8)} M${f1(x)} ${f1(y)} l${f1(r * 1.1)} ${f1(r * 4.4)}" stroke="${K.knightB}" stroke-width="${f1(r * 0.8)}" stroke-linecap="round"/>`) +
  [0, 1, 2, 3, 4, 5, 6, 7].map(i => `<circle cx="${f1(x + Math.cos(i * Math.PI / 4) * r * 1.33)}" cy="${f1(y + Math.sin(i * Math.PI / 4) * r * 1.33)}" r="${f1(r)}" fill="${K.knightA}" stroke="${mix(K.knightA, "#000000", 0.35)}" stroke-width="0.8"/>`).join("") +
  `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(r)}" fill="#e0b030" stroke="#9a7413" stroke-width="0.8"/>`;
const horseRivets = (pts, c, r = 2.4) => pts.map(([x, y]) => `<circle cx="${f1(x)}" cy="${f1(y)}" r="${r}" fill="${c}"/>`).join("");
// a hem along y from x0 to x1 in n pieces, cut in scallops or in dags, as a path's next commands
const horseHem = (x0, x1, y, n, cut, deep) => { let d = ""; for (let i = 1; i <= n; i++) { const a = x0 + (x1 - x0) * (i - 1) / n, b = x0 + (x1 - x0) * i / n, m = (a + b) / 2;
  d += cut === "dag" ? ` L${f1(m)} ${f1(y + deep + 2)} L${f1(b)} ${f1(y)}` : ` Q${f1(m)} ${f1(y + deep)} ${f1(b)} ${f1(y)}`; } return d; };
// a tassel hanging from (x, y), in the arms' second colour with a gold knob; tassels: n of them along a line from a to b
const horseTassel = ([x, y], K) => `<path d="M${f1(x)} ${f1(y)} l-4 16 l8 0 Z" fill="${K.knightB}" stroke="${mix(K.knightB, "#000000", 0.4)}" stroke-width="1"/><circle cx="${f1(x)}" cy="${f1(y)}" r="3" fill="#e0b030"/>`;
const horseTassels = (a, b, n, K) => [...Array(n)].map((_, i) => { const t = (i + 0.5) / n; return horseTassel([a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t], K); }).join("");
// lames: plates overlapping down a neck seen from the front or behind, between y0 (its top) and y1, w(y) its half width
// there; c their colours (steel, or the dragon's scales); toward us their lower edges curve down, from behind their upper ones up
function horseLames(y0, y1, w, c, scales, behind) {
  let s = ""; const n = 5, step = (y1 - y0) / n;
  for (let i = 0; i < n; i++) { const a = y0 + i * step, b = Math.min(y1, a + step * 1.4), wa = w(a) + 3, wb = w(b) + 3;
    s += behind ? `<path d="M${f1(-wa)} ${f1(a + 4)} Q0 ${f1(a - 6)} ${f1(wa)} ${f1(a + 4)} L${f1(wb)} ${f1(b)} L${f1(-wb)} ${f1(b)} Z" fill="${c.S}" stroke="${c.D}" stroke-width="2" stroke-linejoin="round"/>`
      : `<path d="M${f1(-wa)} ${f1(a)} L${f1(wa)} ${f1(a)} L${f1(wb)} ${f1(b)} Q0 ${f1(b + 7)} ${f1(-wb)} ${f1(b)} Z" fill="${c.S}" stroke="${c.D}" stroke-width="2" stroke-linejoin="round"/>`;
    s += scales ? `<path d="M${f1(-wa * 0.5)} ${f1(a + step * 0.6)} q4 6 9 0 M${f1(wa * 0.15)} ${f1(a + step * 0.6)} q4 6 9 0" fill="none" stroke="${c.D}" stroke-width="1.2"/>`
      : `<path d="M${f1(-wa + 4)} ${f1(a + (behind ? 3 : 2))} L${f1(wa - 4)} ${f1(a + (behind ? 3 : 2))}" stroke="${c.L}" stroke-width="2" opacity="0.8"/>`; }
  return s;
}
// ---- from the side (facing our right), on the body at rest: the mock-up's pieces ----
// the crest of the neck, as the mane follows it, from the withers (t 0) to the poll (t 1), o in toward the neck (horseSvg's, at rest)
const horseCrestAt = (t, o) => { const v = 1 - t, x = v * v * 86 + 2 * v * t * 130 + t * t * 184, y = v * v * -302 + 2 * v * t * -392 + t * t * -454;
  const dx = 88 * (1 - t) + 108 * t, dy = -180 * (1 - t) - 124 * t, d = Math.hypot(dx, dy); return [x - dy / d * o, y + dx / d * o]; };
const HORSE_EYE = `<ellipse cx="216" cy="-436" rx="5" ry="6" fill="#1f1410"/><circle cx="218" cy="-438" r="1.6" fill="#ffffff"/>`;
function horseChanfronSide(kind, K, sw, tack) {
  if (kind === "toy") {
    // (one chunky moulded piece, round eye holes, a clip on top for its plume, a shine down it)
    const c = K.knightA, d = mix(c, "#000000", 0.4);
    return `<path d="M180 -468 Q210 -484 238 -466 L294 -410 Q304 -398 294 -388 L276 -392 L226 -424 L184 -446 Z" fill="${c}" stroke="${d}" stroke-width="3" stroke-linejoin="round"/>` +
      `<path d="M196 -470 Q214 -478 232 -466 L284 -416" fill="none" stroke="#ffffff" stroke-width="5" stroke-linecap="round" opacity="0.45"/>` +
      `<circle cx="216" cy="-436" r="10" fill="${mix(c, "#000000", 0.55)}"/>` + HORSE_EYE + `<rect x="190" y="-484" width="14" height="16" rx="3" fill="${c}" stroke="${d}" stroke-width="2"/>` + horsePlumeSide(197, -482, K.plume, 1, sw);
  }
  if (kind === "dragon") {
    // (a dragon's head: a horn sweeping back, spikes down the nose, teeth along its edge, a red ring round the eye)
    const m = HORSE_DRAGON;
    return `<path d="M196 -462 C178 -480 160 -500 132 -506 C158 -490 172 -472 184 -452 Z" fill="${m.horn}" stroke="${m.hornD}" stroke-width="2"/>` +
      `<path d="M184 -462 Q206 -472 228 -462 L288 -406 Q296 -394 290 -386 L270 -390 Q246 -404 228 -418 Q212 -432 196 -440 Q184 -446 184 -462 Z" fill="${m.S}" stroke="${m.D}" stroke-width="2.5" stroke-linejoin="round"/>` +
      [0, 1, 2, 3, 4].map(i => { const x = 214 + i * 15, y = -462 + i * 14; return `<path d="M${x - 6} ${y + 2} L${x + 2} ${y - 12} L${x + 6} ${y + 4} Z" fill="${m.horn}" stroke="${m.hornD}" stroke-width="1.2"/>`; }).join("") +
      [0, 1, 2, 3].map(i => { const x = 236 + i * 13, y = -410 + i * 7; return `<path d="M${x} ${y} l3 9 l4 -7 Z" fill="#ffffff" stroke="${m.hornD}" stroke-width="1"/>`; }).join("") +
      `<path d="M200 -450 q6 -2 10 2 M222 -446 q6 -2 10 2 M244 -428 q6 -2 10 2 M262 -414 q6 -2 10 2" fill="none" stroke="${m.D}" stroke-width="1.6" opacity="0.7"/>` +
      `<circle cx="216" cy="-436" r="10" fill="none" stroke="${m.eye}" stroke-width="3.5"/>` + HORSE_EYE;
  }
  if (kind === "cloth") {
    // (a cloth over the whole head in the arms, with an eye hole and a sleeve over the ear; the bridle over it, so the rein
    // has its ring; the plume in a gold band)
    const shape = "M174 -456 Q204 -478 230 -462 L294 -402 Q306 -380 288 -362 L254 -356 Q232 -370 210 -402 Q194 -418 174 -430 Z", tackD = mix(tack, "#000000", 0.35);
    return armsFill("horse-head-cloth", shape, K, false, [234, -414], 0.75, [234, -414], 90) + `<path d="M186 -456 L182 -500 L206 -462 Z" fill="${K.knightA}" stroke="#3b3b44" stroke-width="2"/>` +
      `<circle cx="216" cy="-436" r="9" fill="#2a2420"/>` + HORSE_EYE +
      `<path d="M240 -374 L272 -410" stroke="${tack}" stroke-width="7" stroke-linecap="round"/><path d="M204 -462 Q214 -420 244 -384" fill="none" stroke="${tackD}" stroke-width="5" stroke-linecap="round"/>` +
      `<circle cx="240" cy="-368" r="5" fill="none" stroke="#e8b923" stroke-width="2.5"/>` +
      `<path d="M196 -470 L200 -478" stroke="#e0b030" stroke-width="5"/>` + horsePlumeSide(198, -478, K.plume, 1.2, sw);
  }
  // (steel: a plate down the face from the poll to the nose, a ridge down its middle, rivets, a cup round the eye, a plume)
  const c = HORSE_STEEL;
  return `<path d="M184 -462 Q206 -472 228 -462 L288 -406 Q296 -394 290 -386 L270 -390 Q246 -404 228 -418 Q212 -432 196 -440 Q184 -446 184 -462 Z" fill="${c.S}" stroke="${c.D}" stroke-width="2.5" stroke-linejoin="round"/>` +
    `<path d="M196 -462 L286 -400" stroke="${c.L}" stroke-width="3" stroke-linecap="round" opacity="0.9"/>` + horseRivets([[200, -452], [244, -436], [270, -412], [282, -396]], c.D) +
    `<circle cx="216" cy="-436" r="10" fill="${mix(c.S, "#000000", 0.5)}" stroke="${c.D}" stroke-width="2"/>` + HORSE_EYE + horsePlumeSide(196, -466, K.plume, 1, sw);
}
// the crinet: plates overlapping down the neck's crest, from the withers up (scales: the dragon's, with scales drawn on)
function horseCrinetSide(c, scales) {
  let s = ""; const P = p => `${f1(p[0])} ${f1(p[1])}`;
  for (let i = 0; i < 8; i++) { const t0 = 0.04 + i * 0.115, t1 = t0 + 0.14;
    s += `<path d="M${P(horseCrestAt(t0, -4))} L${P(horseCrestAt(t1, -4))} L${P(horseCrestAt(t1, 32))} Q${P(horseCrestAt((t0 + t1) / 2, 40))} ${P(horseCrestAt(t0, 32))} Z" fill="${c.S}" stroke="${c.D}" stroke-width="2" stroke-linejoin="round"/>` +
      (scales ? `<path d="M${P(horseCrestAt(t0 + 0.02, 10))} q4 6 9 0 M${P(horseCrestAt(t0 + 0.06, 22))} q4 6 9 0" fill="none" stroke="${c.D}" stroke-width="1.2"/>`
        : `<path d="M${P(horseCrestAt(t0 + 0.01, 0))} L${P(horseCrestAt(t1 - 0.02, 0))}" stroke="${c.L}" stroke-width="2" opacity="0.8"/>`); }
  return s;
}
const horsePeytralSide = c => `<path d="M118 -298 Q174 -284 180 -234 Q178 -192 150 -176 L122 -184 Q144 -236 112 -282 Z" fill="${c.S}" stroke="${c.D}" stroke-width="2.5" stroke-linejoin="round"/>` +
  `<path d="M126 -288 Q168 -270 168 -228" fill="none" stroke="${c.L}" stroke-width="3" opacity="0.8"/><circle cx="160" cy="-232" r="8" fill="${c.L}" stroke="${c.D}" stroke-width="2"/>` + horseRivets([[130, -282], [172, -250], [160, -196]], c.D);
const horseCrupperSide = c => `<path d="M-50 -318 Q-150 -322 -178 -292 Q-194 -252 -176 -212 L-156 -220 Q-166 -258 -146 -286 Q-104 -304 -50 -302 Z" fill="${c.S}" stroke="${c.D}" stroke-width="2.5" stroke-linejoin="round"/>` +
  `<path d="M-60 -312 Q-140 -312 -166 -286" fill="none" stroke="${c.L}" stroke-width="3" opacity="0.8"/>` + horseRivets([[-80, -310], [-140, -300], [-172, -262]], c.D);
const horseFlanchardSide = c => `<path d="M-40 -292 L100 -292 Q110 -250 96 -206 Q30 -196 -36 -206 Q-48 -250 -40 -292 Z" fill="${c.S}" stroke="${c.D}" stroke-width="2.5" stroke-linejoin="round"/>` +
  `<path d="M-30 -282 L92 -282" stroke="${c.L}" stroke-width="2.5" opacity="0.8"/>` + horseRivets([[-30, -214], [30, -206], [88, -214]], c.D);
// the caparison: a cloth in the arms from the withers to the rump, down to its hem (hemY), cut in scallops or dags; the emblem
// on its side; and (neck) over the neck too. Its outline, horseCaparisonPath, is also where the tail lies over it: there, pushed
// out by o at the back and below the hem, so the tail covers the cloth's edge line where it crosses it
const horseCaparisonPath = (hemY, cut, o = 0) => `M150 -302 Q100 -316 40 -324 Q-60 -324 -140 -308 Q-182 -286 ${-192 - o} -236 L${-196 - o} ${hemY + o}` + (o ? ` L-196 ${hemY + o}` : "") +
  horseHem(-196, 186, hemY + o, 16, cut, 16) + ` L186 -236 Q184 -280 150 -302 Z`;
function horseCaparisonSide(K, hemY, cut, neck) {
  const d = horseCaparisonPath(hemY, cut);
  return armsFill("horse-cloth", d, K, false, [-60, -236], 1.6, [-5, -200], 260) +
    (neck ? armsFill("horse-neck-cloth", "M84 -298 Q128 -386 182 -448 L214 -420 Q206 -356 160 -248 Z", K, false, null, 1, [150, -350], 120) : "");
}
// the pieces of a set from the side: body (over the near legs' tops, under the saddle), neck, chest (over the saddle's
// girth), head (over the head), after (last); hideHead and hideMane where a cloth covers them; tailOver, the whole tail over
// the crupper (the near hind leg drawn after it); tailOn, the cloth's outline, where the tail lies over the cloth (below its
// hem the near hind leg still passes in front of the tail, as with no armour)
function horseArmourSide(set, K, sw, tack) {
  const cl = HORSE_CLOTHS[set], none = { body: "", neck: "", chest: "", head: "", after: "" }, on = cl && horseCaparisonPath(cl[0], cl[1], 1.3);
  if (set === "plate") return { ...none, body: horseFlanchardSide(HORSE_STEEL) + horseCrupperSide(HORSE_STEEL), neck: horseCrinetSide(HORSE_STEEL), chest: horsePeytralSide(HORSE_STEEL), head: horseChanfronSide("steel", K, sw, tack), tailOver: true };
  if (set === "half") return { ...none, body: horseCaparisonSide(K, cl[0], cl[1]), head: horseChanfronSide("steel", K, sw, tack), tailOn: on };
  if (set === "tourney") return { ...none, body: horseCaparisonSide(K, cl[0], cl[1], true), head: horseChanfronSide("cloth", K, sw, tack), hideHead: true, hideMane: true, tailOn: on };
  if (set === "dragon") return { ...none, body: horseCaparisonSide(K, cl[0], cl[1]), neck: horseCrinetSide(HORSE_DRAGON, true), head: horseChanfronSide("dragon", K, sw, tack), tailOn: on };
  if (set === "toy") return { ...none, body: horseCaparisonSide(K, cl[0], cl[1]), head: horseChanfronSide("toy", K, sw, tack), tailOn: on };
  if (set === "parade") {
    // (rosettes where the bridle's straps meet, at the poll and the noseband, a plume, tassels along the saddle cloth's edge)
    const edge = t => { const v = 1 - t; return [v * v * -26 + 2 * v * t * 30 + t * t * 92, v * v * -240 + 2 * v * t * -228 + t * t * -238]; };
    return { ...none, head: horseRosette(204, -462, 4.5, K, sw) + horseRosette(244, -386, 4.5, K, sw) + horsePlumeSide(196, -470, K.plume, 0.8, sw),
             after: [...Array(7)].map((_, i) => horseTassel(edge(0.04 + i * 0.15), K)).join("") };
  }
  return null;
}
// ---- facing us, on the body at rest ----
// the chest cloth: from over the chest down to its hem, over the forelegs' tops, the emblem on its breast
function horseClothFront(K, hemY, cut) {
  const d = `M-62 -334 Q0 -352 62 -334 Q90 -308 92 -256 L94 ${hemY}` + horseHem(94, -94, hemY, 8, cut, 14) + ` L-92 -256 Q-90 -308 -62 -334 Z`;
  return armsFill("horse-cloth", d, K, false, [0, -212], 1.2, [0, (hemY - 352) / 2], Math.max(110, (hemY + 352) / 2 + 12));
}
// the chanfron facing us, in the face's own units (horseFace's): a plate down the face from between the ears to above the
// nose, a ridge, rivets, cups round the eyes (the eyes drawn again in them), and the plume's socket between the ears
const horseFaceEyes = `<ellipse cx="-22" cy="-128" rx="4.8" ry="6.2" fill="#1f1410"/><circle cx="-20.4" cy="-130.5" r="1.5" fill="#ffffff"/><ellipse cx="22" cy="-128" rx="4.8" ry="6.2" fill="#1f1410"/><circle cx="23.6" cy="-130.5" r="1.5" fill="#ffffff"/>`;
function horseChanfronFront(kind, K, sw, tack, short) {
  if (kind === "toy") {
    // (one chunky helmet over the top of the head and down the face, round holes for the eyes, its ear tips showing, a clip
    // on top for the plume, a shine; ridden toward us (short) its dome is seen from above, flatter, and the clip and plume sit
    // low, below the rider's chin, as the steel chanfron's do)
    const c = K.knightA, d = mix(c, "#000000", 0.4), top = short ? -167 : -176;
    return `<path d="M-27 -146 Q-28 ${top + 2} 0 ${top} Q28 ${top + 2} 27 -146 L25 -118 Q21 -98 14 -90 Q0 -86 -14 -90 Q-21 -98 -25 -118 Z" fill="${c}" stroke="${d}" stroke-width="2.4" stroke-linejoin="round"/>` +
      `<path d="M-17 ${short ? -158 : -164} Q-23 -140 -18 -110" fill="none" stroke="#ffffff" stroke-width="4" stroke-linecap="round" opacity="0.45"/>` +
      `<circle cx="-20" cy="-128" r="8.5" fill="${mix(c, "#000000", 0.55)}"/><circle cx="20" cy="-128" r="8.5" fill="${mix(c, "#000000", 0.55)}"/>` + horseFaceEyes +
      (short ? `<rect x="-6" y="-170" width="12" height="8" rx="2.5" fill="${c}" stroke="${d}" stroke-width="1.6"/>` + horsePlumeFront(0, -168, K.plume, 46, sw, true)
        : `<rect x="-6" y="-186" width="12" height="12" rx="2.5" fill="${c}" stroke="${d}" stroke-width="1.6"/>` + horsePlumeFront(0, -184, K.plume, 46, sw, false));
  }
  if (kind === "dragon") {
    // (a dragon's mask: horns curving up and out, spikes up the ridge of the nose, teeth along its lower edge, red rings round
    // the eyes)
    const m = HORSE_DRAGON;
    return [-1, 1].map(sd => `<path d="M${sd * 12} -158 C${sd * 24} -176 ${sd * 30} -194 ${sd * 46} -204 C${sd * 36} -188 ${sd * 30} -172 ${sd * 22} -152 Z" fill="${m.horn}" stroke="${m.hornD}" stroke-width="1.4"/>`).join("") +
      `<path d="M-20 -162 Q0 -170 20 -162 L24 -134 Q20 -110 14 -90 Q0 -84 -14 -90 Q-20 -110 -24 -134 Z" fill="${m.S}" stroke="${m.D}" stroke-width="2" stroke-linejoin="round"/>` +
      [-158, -146, -134, -122, -110, -98].map(y => `<path d="M-3.5 ${y + 2} L0 ${y - 6} L3.5 ${y + 2} Z" fill="${m.horn}" stroke="${m.hornD}" stroke-width="0.9"/>`).join("") +
      [-10, -5, 0, 5, 10].map(x => `<path d="M${x - 2.2} -89 l2.2 6 l2.2 -6 Z" fill="#ffffff" stroke="${m.hornD}" stroke-width="0.8"/>`).join("") +
      `<path d="M-15 -150 q3 -3 6 0 M9 -150 q3 -3 6 0 M-14 -112 q3 -3 6 0 M8 -112 q3 -3 6 0 M-12 -100 q3 -3 6 0 M6 -100 q3 -3 6 0" fill="none" stroke="${m.D}" stroke-width="1.2" opacity="0.7"/>` +
      `<circle cx="-22" cy="-128" r="8" fill="none" stroke="${m.eye}" stroke-width="3"/><circle cx="22" cy="-128" r="8" fill="none" stroke="${m.eye}" stroke-width="3"/>` + horseFaceEyes;
  }
  if (kind === "cloth") {
    // (the cloth over the whole head: sleeves over the ears, eye holes, the bridle over it, the plume in a gold band)
    const tackD = mix(tack, "#3a2414", 0.45);
    return [-1, 1].map(sd => `<path d="M${sd * 8} -154 L${sd * 23} -190 L${sd * 25} -150 Z" fill="${K.knightA}" stroke="#3b3b44" stroke-width="1.6" stroke-linejoin="round"/>`).join("") +
      armsFill("horse-head-cloth", "M-18 -160 Q-27 -153 -25 -136 Q-24 -116 -17 -96 Q-14 -84 -15 -74 Q-20 -62 -15 -48 Q0 -40 15 -48 Q20 -62 15 -74 Q14 -84 17 -96 Q24 -116 25 -136 Q27 -153 18 -160 Q0 -167 -18 -160 Z", K, false, null, 1, [0, -104], 70) +
      `<ellipse cx="-21" cy="-128" rx="7.5" ry="8.5" fill="#2a2420"/><ellipse cx="21" cy="-128" rx="7.5" ry="8.5" fill="#2a2420"/>` + horseFaceEyes +
      [-1, 1].map(sd => `<path d="M${sd * 12.5} -82 Q${sd * 16} -98 ${sd * 18} -112" fill="none" stroke="${tackD}" stroke-width="3" stroke-linecap="round"/>`).join("") +
      `<path d="M-13.5 -84 Q0 -76 13.5 -84" fill="none" stroke="${tack}" stroke-width="4.5" stroke-linecap="round"/><circle cx="0" cy="-77" r="3" fill="none" stroke="#e8b923" stroke-width="1.6"/>` +
      `<rect x="-4" y="-170" width="8" height="8" rx="1.5" fill="#e0b030" stroke="#9a7413" stroke-width="1"/>` + horsePlumeFront(0, -168, K.plume, 52, sw, short);
  }
  const c = HORSE_STEEL;
  return `<path d="M-17 -161 Q0 -168 17 -161 L21 -134 Q18 -110 12 -90 Q0 -84 -12 -90 Q-18 -110 -21 -134 Z" fill="${c.S}" stroke="${c.D}" stroke-width="2" stroke-linejoin="round"/>` +
    `<path d="M0 -162 L0 -91" stroke="${c.L}" stroke-width="2" stroke-linecap="round" opacity="0.9"/>` + horseRivets([[-12, -152], [12, -152], [-9, -100], [9, -100]], c.D, 1.4) +
    `<circle cx="-22" cy="-128" r="8" fill="none" stroke="${c.S}" stroke-width="4"/><circle cx="22" cy="-128" r="8" fill="none" stroke="${c.S}" stroke-width="4"/>` +
    `<circle cx="-22" cy="-128" r="10" fill="none" stroke="${c.D}" stroke-width="1"/><circle cx="22" cy="-128" r="10" fill="none" stroke="${c.D}" stroke-width="1"/>` + horseFaceEyes +
    `<rect x="-4" y="-170" width="8" height="9" rx="1.5" fill="${c.S}" stroke="${c.D}" stroke-width="1.2"/>` + horsePlumeFront(0, -168, K.plume, 46, sw, short);
}
// the pieces of a set facing us: behind (at its sides, under the rider), neck (over the neck and the mane), chest (over the
// forelegs' tops), face (in the face's units, over it), hideFace and hideMane where a cloth covers them; D, how much lower the
// head is carried (ridden); the parade's cloth at its sides is horseSvg's, as the saddle blanket's
function horseArmourFront(set, K, D, sw, tack) {
  const cl = HORSE_CLOTHS[set], none = { behind: "", neck: "", chest: "", face: "" }, top = -432 + D, nw = y => 44 - 12 * (-296 - y) / Math.max(1, -296 - top);
  if (set === "plate") return { ...none, behind: [-1, 1].map(k => `<path d="M${k * 82} -316 L${k * 99} -310 Q${k * 103} -262 ${k * 96} -212 L${k * 80} -214 Z" fill="${HORSE_STEEL.S}" stroke="${HORSE_STEEL.D}" stroke-width="2.5" stroke-linejoin="round"/>`).join(""),
    neck: horseLames(top, -298, nw, HORSE_STEEL, false, false),
    chest: `<path d="M-58 -322 Q0 -340 58 -322 Q80 -298 78 -256 Q76 -216 60 -202 Q0 -194 -60 -202 Q-76 -216 -78 -256 Q-80 -298 -58 -322 Z" fill="${HORSE_STEEL.S}" stroke="${HORSE_STEEL.D}" stroke-width="2.5" stroke-linejoin="round"/>` +
      `<path d="M0 -332 L0 -198" stroke="${HORSE_STEEL.D}" stroke-width="1.6" opacity="0.6"/><path d="M-48 -318 Q-70 -292 -68 -250" fill="none" stroke="${HORSE_STEEL.L}" stroke-width="3" opacity="0.8"/>` +
      `<circle cx="0" cy="-262" r="10" fill="${HORSE_STEEL.L}" stroke="${HORSE_STEEL.D}" stroke-width="2"/>` + horseRivets([[-44, -320], [44, -320], [-68, -230], [68, -230], [-30, -204], [30, -204]], HORSE_STEEL.D),
    face: horseChanfronFront("steel", K, sw, tack, D > 0) };
  if (set === "half") return { ...none, chest: horseClothFront(K, cl[0], cl[1]), face: horseChanfronFront("steel", K, sw, tack, D > 0) };
  if (set === "tourney") return { ...none, neck: armsFill("horse-neck-cloth", `M-46 -294 L-34 ${f1(top - 2)} L34 ${f1(top - 2)} L46 -294 Z`, K, false, null, 1, [0, (top - 294) / 2], 80),
    chest: horseClothFront(K, cl[0], cl[1]), face: horseChanfronFront("cloth", K, sw, tack, D > 0), hideFace: true, hideMane: true };
  if (set === "dragon") return { ...none, neck: horseLames(top, -298, nw, HORSE_DRAGON, true, false), chest: horseClothFront(K, cl[0], cl[1]), face: horseChanfronFront("dragon", K, sw, tack, D > 0) };
  if (set === "toy") return { ...none, chest: horseClothFront(K, cl[0], cl[1]), face: horseChanfronFront("toy", K, sw, tack, D > 0) };
  if (set === "parade") {
    // (a browband under the ears with a rosette at each end, small ones where the noseband meets the cheek straps, a plume)
    return { ...none, face: `<path d="M-19 -149 Q0 -142 19 -149" fill="none" stroke="${tack}" stroke-width="3.5" stroke-linecap="round"/>` +
      horseRosette(-19, -149, 3.4, K, sw) + horseRosette(19, -149, 3.4, K, -sw) + horseRosette(-13.5, -84, 2.4, K, sw) + horseRosette(13.5, -84, 2.4, K, -sw) + horsePlumeFront(0, -156, K.plume, 40, sw, D > 0) };
  }
  return null;
}
// ---- from behind, on the body at rest ----
// the cloth over the rump, from over the haunches down to its hem, an emblem on each haunch
function horseClothBack(K, hemY, cut) {
  const d = `M-90 -244 Q-94 -338 -4 -342 L4 -342 Q94 -338 90 -244 L92 ${hemY}` + horseHem(92, -92, hemY, 8, cut, 14) + ` Z`;
  return armsFill("horse-cloth", d, K, true, null, 1, [0, (hemY - 342) / 2], Math.max(110, (hemY + 342) / 2 + 12)) + armsEmblem(K, [-46, -264], 0.9) + armsEmblem(K, [46, -264], 0.9);
}
// the head from behind: the chanfron's edges at the sides of the head and across its top, the plume's socket at the poll
function horseChanfronBack(kind, K, sw) {
  if (kind === "toy") {
    // (the chunky helmet's shell over the back of the head, its shine, the clip and its plume; the ear tips through it)
    const c = K.knightA, d = mix(c, "#000000", 0.4);
    return `<path d="M-30 -428 Q-32 -466 0 -468 Q32 -466 30 -428 L26 -412 Q0 -420 -26 -412 Z" fill="${c}" stroke="${d}" stroke-width="2.5" stroke-linejoin="round"/>` +
      `<path d="M-20 -456 Q-26 -440 -22 -424" fill="none" stroke="#ffffff" stroke-width="4" stroke-linecap="round" opacity="0.45"/>` +
      `<rect x="-7" y="-480" width="14" height="14" rx="3" fill="${c}" stroke="${d}" stroke-width="2"/>` + horsePlumeBack(0, -478, K.plume, 56, sw);
  }
  if (kind === "dragon") {
    // (the dragon helm's back: a green cap, the horns sweeping back toward us and out, spikes along its top)
    const m = HORSE_DRAGON;
    return `<path d="M-26 -438 Q0 -464 26 -438 L23 -424 Q0 -436 -23 -424 Z" fill="${m.S}" stroke="${m.D}" stroke-width="2" stroke-linejoin="round"/>` +
      [-1, 1].map(sd => `<path d="M${sd * 16} -450 C${sd * 36} -456 ${sd * 52} -444 ${sd * 60} -420 C${sd * 46} -432 ${sd * 32} -438 ${sd * 20} -436 Z" fill="${m.horn}" stroke="${m.hornD}" stroke-width="1.6"/>`).join("") +
      [-12, 0, 12].map(x => `<path d="M${x - 4} ${-450 - (x ? 0 : 3)} L${x} ${-464 - (x ? 0 : 3)} L${x + 4} ${-450 - (x ? 0 : 3)} Z" fill="${m.horn}" stroke="${m.hornD}" stroke-width="1"/>`).join("") +
      `<path d="M-14 -440 q4 -4 8 0 M6 -440 q4 -4 8 0" fill="none" stroke="${m.D}" stroke-width="1.2" opacity="0.7"/>`;
  }
  if (kind === "cloth") return [-1, 1].map(sd => `<path d="M${sd * 6} -446 L${sd * 26} -492 L${sd * 26} -438 Z" fill="${K.knightA}" stroke="#3b3b44" stroke-width="2" stroke-linejoin="round"/>`).join("") +
    `<rect x="-5" y="-462" width="10" height="9" rx="2" fill="#e0b030" stroke="#9a7413" stroke-width="1"/>` + horsePlumeBack(0, -460, K.plume, 62, sw);
  // (steel: the plate's edges showing past the head at its sides, below the ears, and the plume's socket at the poll)
  const c = HORSE_STEEL;
  return [-1, 1].map(sd => `<path d="M${sd * 20} -444 Q${sd * 31} -436 ${sd * 31} -414 L${sd * 24} -410 Q${sd * 26} -428 ${sd * 18} -436 Z" fill="${c.S}" stroke="${c.D}" stroke-width="1.8" stroke-linejoin="round"/>`).join("") +
    `<rect x="-5" y="-462" width="10" height="11" rx="2" fill="${c.S}" stroke="${c.D}" stroke-width="1.4"/>` + horsePlumeBack(0, -460, K.plume, 56, sw);
}
// the pieces of a set from behind: sides (beyond the forelegs, under the rider), neck (over the back of the neck and the
// mane, under the halter's strap), head (after the strap), rump (over the haunches and the hind legs' tops, under the tail)
function horseArmourBack(set, K, sw) {
  const cl = HORSE_CLOTHS[set], none = { sides: "", neck: "", head: "", rump: "" }, nw = y => 38 - 16 * (-300 - y) / 138;
  if (set === "plate") return { ...none, sides: [-1, 1].map(k => `<path d="M${k * 84} -318 L${k * 100} -312 Q${k * 104} -262 ${k * 97} -214 L${k * 82} -216 Z" fill="${HORSE_STEEL.S}" stroke="${HORSE_STEEL.D}" stroke-width="2.5" stroke-linejoin="round"/>`).join(""),
    neck: horseLames(-440, -302, nw, HORSE_STEEL, false, true), head: horseChanfronBack("steel", K, sw),
    rump: `<path d="M-80 -244 Q-84 -322 -4 -326 L4 -326 Q84 -322 80 -244 Q78 -206 56 -196 L-56 -196 Q-78 -206 -80 -244 Z" fill="${HORSE_STEEL.S}" stroke="${HORSE_STEEL.D}" stroke-width="2.5" stroke-linejoin="round"/>` +
      `<path d="M-70 -296 Q-50 -318 -10 -318 M70 -296 Q50 -318 10 -318" fill="none" stroke="${HORSE_STEEL.L}" stroke-width="3" opacity="0.8"/>` + horseRivets([[-72, -280], [72, -280], [-72, -224], [72, -224], [-40, -202], [40, -202]], HORSE_STEEL.D) };
  if (set === "half") return { ...none, head: horseChanfronBack("steel", K, sw), rump: horseClothBack(K, cl[0], cl[1]) };
  if (set === "tourney") return { ...none, neck: armsFill("horse-neck-cloth", "M-40 -298 Q-32 -400 -24 -440 Q0 -462 24 -440 Q32 -400 40 -298 Z", K, true, null, 1, [0, -380], 90),
    head: horseChanfronBack("cloth", K, sw), rump: horseClothBack(K, cl[0], cl[1]), hideMane: true };
  if (set === "dragon") return { ...none, neck: horseLames(-440, -302, nw, HORSE_DRAGON, true, true), head: horseChanfronBack("dragon", K, sw), rump: horseClothBack(K, cl[0], cl[1]) };
  if (set === "toy") return { ...none, head: horseChanfronBack("toy", K, sw), rump: horseClothBack(K, cl[0], cl[1]) };
  if (set === "parade") return { ...none, head: horseRosette(-24, -424, 4.2, K, sw) + horseRosette(24, -424, 4.2, K, -sw) + horsePlumeBack(0, -452, K.plume, 44, sw) };
  return null;
}
function horseSvg(view, gait, u, H, g, ride) {
  const coat = H.coat, mane = H.mane, ln = mix(coat, "#000000", 0.42), far = mix(coat, "#000000", 0.25), maneD = mix(mane, "#000000", 0.3), white = "#f7f4ee";
  const tack = H.tack, tackD = mix(tack, "#000000", 0.35);
  const run = gait === "run", walk = gait === "walk", p = run ? (2 * u) % 1 : u, T = 2 * Math.PI;
  const bob = run ? -10 * Math.abs(Math.sin(T * p)) : walk ? -2 * Math.abs(Math.sin(2 * T * p)) : 0, y = v => g + v + bob;
  // (each leg's swing and bend at its place in the stride)
  const step = off => { if (!walk && !run) return [0, 0]; const ph = T * (p + off), A = run ? 30 : 16, Fx = run ? 62 : 40;
    return [A * Math.sin(ph), Fx * Math.max(0, Math.cos(ph)) ** 1.5]; };
  const sock = !!H.socks, OFF = run ? { lh: 0, rh: 0.08, lf: 0.45, rf: 0.55 } : { lh: 0, lf: 0.25, rh: 0.5, rf: 0.75 };
  // (ticket 495, for M: "even more color choices for horse ... one color chooser per param") its shoes (off: none), its face
  // marking's colour (white), its saddle's leather, and its blanket (auto: a lighter shade of the halter's colour, as before)
  const shoe = H.shoes && H.shoes !== "off" ? H.shoes : null, markC = H.markColor || white;
  const tailSw = (run ? 10 : 6) * Math.sin(T * p), cloth = H.blanket && H.blanket !== "auto" ? H.blanket : mix(tack, "#ffffff", 0.2), leather = H.saddle || "#6b3f1f", leatherD = leather === "#6b3f1f" ? "#3e2410" : mix(leather, "#000000", 0.42);
  // (ticket 498) its armour (none: drawn exactly as before), the arms its cloth and plume take, its saddle blanket in their first
  // colour (unless one is chosen), how far the plumes and ribbons swing with the stride, and the pieces lifted with its bob
  const arm = H.armour && H.armour !== "none" && HORSE_ARMOURS.some(([id]) => id === H.armour) ? H.armour : null, K = arm ? horseArms(H) : null;
  const clothC = arm && !(H.blanket && H.blanket !== "auto") ? K.knightA : cloth, plumeSw = -(run ? 9 : 5) * Math.sin(T * p);
  const onBody = s => !s ? "" : g + bob ? `<g transform="translate(0 ${f1(g + bob)})">${s}</g>` : s;
  if (view === "side") {
    // (facing our right: its left side toward us, its right legs beyond, darker. X: the rear drawn in, a horse being about
    // as long as it is tall at the withers)
    const X = x => f1(x < 0 ? x * 0.82 : x);
    // (ticket 492: each hoof's place, [x, lifted, back]: the far legs a little behind the near)
    const hooves = [], leg = (x, top, fore, off, near) => { const [sw, fl] = step(off), at = [];
      const svg = horseLeg([+X(x), y(top)], g - y(top) - 6, sw, fl, fore, near ? 17 : 15, near ? coat : far, sock, g, at, shoe); hooves.push([at[0][0], g - at[0][1], near ? 0 : 8]); return svg; };
    const body = `<path d="M120 ${f1(y(-292))} C40 ${f1(y(-314))} ${X(-120)} ${f1(y(-318))} ${X(-165)} ${f1(y(-304))} C${X(-212)} ${f1(y(-288))} ${X(-216)} ${f1(y(-220))} ${X(-186)} ${f1(y(-186))} ` +
      `C${X(-150)} ${f1(y(-160))} ${X(-40)} ${f1(y(-156))} 82 ${f1(y(-166))} C142 ${f1(y(-174))} 172 ${f1(y(-216))} 152 ${f1(y(-260))} Z" fill="${coat}" stroke="${ln}" stroke-width="3"/>` +
      // (the shoulder, sloping back from the chest to the withers, and the round of the haunch)
      `<path d="M140 ${f1(y(-196))} Q112 ${f1(y(-250))} 96 ${f1(y(-300))} M${X(-96)} ${f1(y(-312))} Q${X(-120)} ${f1(y(-240))} ${X(-176)} ${f1(y(-200))}" fill="none" stroke="${mix(coat, "#000000", 0.2)}" stroke-width="3" stroke-linecap="round" opacity="0.7"/>`;
    const neck = `<path d="M84 ${f1(y(-298))} Q128 ${f1(y(-386))} 182 ${f1(y(-448))} L214 ${f1(y(-420))} Q206 ${f1(y(-356))} 160 ${f1(y(-248))} Z" fill="${coat}" stroke="${ln}" stroke-width="3" stroke-linejoin="round"/>`;
    const mark = H.mark === "star" ? `<path d="M232 ${f1(y(-458))} L238 ${f1(y(-450))} L232 ${f1(y(-442))} L226 ${f1(y(-450))} Z" fill="${markC}"/>`
      : H.mark === "stripe" ? `<path d="M232 ${f1(y(-456))} L236 ${f1(y(-458))} L286 ${f1(y(-398))} L282 ${f1(y(-394))} Z" fill="${markC}"/>`
      : H.mark === "blaze" ? `<path d="M226 ${f1(y(-458))} L240 ${f1(y(-460))} L292 ${f1(y(-398))} Q294 ${f1(y(-384))} 282 ${f1(y(-380))} L226 ${f1(y(-440))} Z" fill="${markC}"/>` : "";
    const head = `<path d="M178 ${f1(y(-452))} Q204 ${f1(y(-472))} 226 ${f1(y(-458))} L290 ${f1(y(-400))} Q302 ${f1(y(-380))} 286 ${f1(y(-366))} L256 ${f1(y(-360))} Q236 ${f1(y(-372))} 214 ${f1(y(-404))} Q198 ${f1(y(-420))} 178 ${f1(y(-432))} Z" fill="${coat}" stroke="${ln}" stroke-width="3" stroke-linejoin="round"/>` +
      `<path d="M188 ${f1(y(-458))} L184 ${f1(y(-494))} L204 ${f1(y(-464))} Z" fill="${coat}" stroke="${ln}" stroke-width="2.5" stroke-linejoin="round"/>` + mark +
      `<ellipse cx="216" cy="${f1(y(-436))}" rx="5" ry="6" fill="#1f1410"/><circle cx="218" cy="${f1(y(-438))}" r="1.6" fill="#ffffff"/>` +
      `<path d="M206 ${f1(y(-446))} Q216 ${f1(y(-450))} 226 ${f1(y(-444))}" fill="none" stroke="${ln}" stroke-width="1.6"/>` +
      `<path d="M280 ${f1(y(-388))} q6 -3 7 4 q-2 3 -6 1" fill="#1f1410"/><path d="M262 ${f1(y(-366))} Q272 ${f1(y(-370))} 284 ${f1(y(-368))}" stroke="${ln}" stroke-width="2" fill="none"/>` +
      // (the noseband round the nose and a strap up its cheek behind the eye, as the stable's halters; the ring under the chin)
      `<path d="M240 ${f1(y(-374))} L272 ${f1(y(-410))}" stroke="${tack}" stroke-width="7" stroke-linecap="round"/><path d="M204 ${f1(y(-462))} Q214 ${f1(y(-420))} 244 ${f1(y(-384))}" fill="none" stroke="${tackD}" stroke-width="5" stroke-linecap="round"/>` +
      `<circle cx="240" cy="${f1(y(-368))}" r="5" fill="none" stroke="#e8b923" stroke-width="2.5"/>`;
    const ms = H.maneStyle || "loose";
    // (ticket 497, for M) the crest, along the top of the neck from the withers (t 0) to the poll (t 1), and the way into the neck
    const crest = t => { const v = 1 - t; return [v * v * 86 + 2 * v * t * 130 + t * t * 184, y(v * v * -302 + 2 * v * t * -392 + t * t * -454)]; };
    const into = t => { const dx = 88 * (1 - t) + 108 * t, dy = -180 * (1 - t) - 124 * t, d = Math.hypot(dx, dy); return [-dy / d, dx / d]; };
    const crestAt = (t, o) => { const [x, yy] = crest(t), [nx, ny] = into(t); return [x + nx * o, yy + ny * o]; };
    const C = `M86 ${f1(y(-302))} Q130 ${f1(y(-392))} 184 ${f1(y(-454))}`, P = p => `${f1(p[0])} ${f1(p[1])}`, back = ` Q130 ${f1(y(-392))} 86 ${f1(y(-302))}`;
    const maneFill = `fill="${mane}" stroke="${maneD}" stroke-width="2" stroke-linejoin="round"`;
    const trimmed = w => { const pts = []; for (let i = 0; i <= 12; i++) pts.push(crestAt(1 - i / 12, w)); return `<path d="${C} L${pts.map(P).join(" L")} Z" ${maneFill}/>`; };
    const maneP = ms === "long" ? (() => {
        // (long: falling well down the neck in waves, the forelock long, over the forehead)
        const pts = []; for (let i = 0; i <= 12; i++) { const [x, yy] = crest(1 - i / 12); pts.push([x + 12 + 5 * Math.sin(i * 1.9), yy + 52 + (i % 2 ? 0 : 12)]); }
        return `<path d="${C} L196 ${f1(y(-456))} L${P(pts[0])} ${maneSmooth(pts)} Z" ${maneFill}/>`; })()
      : ms === "brush" ? (() => {
        // (short and standing up: a brush along the crest, as a zebra's; a tuft between the ears)
        let d = `M86 ${f1(y(-302))}`; for (let i = 0; i <= 24; i++) d += ` L${P(crestAt(i / 24, i % 2 ? -8 : -14))}`;
        return `<path d="${d} L184 ${f1(y(-454))}${back} Z" ${maneFill}/>` + `<path d="M193 ${f1(y(-458))} L190 ${f1(y(-474))} L197 ${f1(y(-464))} L201 ${f1(y(-478))} L204 ${f1(y(-461))} Z" ${maneFill}/>`; })()
      : ms === "buttons" ? trimmed(6) + [0.06, 0.18, 0.3, 0.42, 0.54, 0.66, 0.78, 0.9].map(t => maneKnot(crestAt(t, 8), 6.5, mane, maneD)).join("") + maneKnot([201, y(-462)], 5.5, mane, maneD)
      : ms === "curly" ? trimmed(10) + [0.04, 0.14, 0.24, 0.34, 0.44, 0.54, 0.64, 0.74, 0.84, 0.94].map((t, k) => maneCurl(crestAt(t, k % 2 ? 22 : 10), k % 2 ? 7 : 8.5, mane, maneD)).join("") +
          maneCurl([197, y(-462)], 5.5, mane, maneD) + maneCurl([206, y(-459)], 5, mane, maneD)
      : ms === "bows" ? trimmed(5) + [0.1, 0.3, 0.5, 0.7, 0.9].map(t => maneBraid(crestAt(t, 5), [0.18, 1], 44, mane, maneD, H.bows || cloth)).join("")
      : `<path d="M86 ${f1(y(-302))} Q130 ${f1(y(-392))} 184 ${f1(y(-454))} L196 ${f1(y(-456))} L176 ${f1(y(-430))} L168 ${f1(y(-412))} L150 ${f1(y(-398))} L144 ${f1(y(-372))} L124 ${f1(y(-360))} L118 ${f1(y(-332))} L100 ${f1(y(-322))} Z" fill="${mane}" stroke="${maneD}" stroke-width="2" stroke-linejoin="round"/>` +
        `<path d="M204 ${f1(y(-464))} Q218 ${f1(y(-452))} 222 ${f1(y(-432))} Q208 ${f1(y(-440))} 198 ${f1(y(-456))} Z" fill="${mane}"/>`;
    // (over the head: the long forelock lying on the forehead, above the eye; the bows' forelock, a bow between the ears)
    const maneOver = ms === "long" ? `<path d="M194 ${f1(y(-462))} Q224 ${f1(y(-472))} 246 ${f1(y(-444))} Q234 ${f1(y(-448))} 224 ${f1(y(-447))} Q212 ${f1(y(-454))} 198 ${f1(y(-452))} Z" ${maneFill}/>`
      : ms === "bows" ? maneBraid([200, y(-462)], [1, 0.9], 16, mane, maneD, H.bows || cloth) : "";
    const tail = `<path d="M${X(-180)} ${f1(y(-294))} Q${X(-236 + tailSw)} ${f1(y(-240))} ${X(-218 + tailSw)} ${f1(y(-128))} Q${X(-198 + tailSw)} ${f1(y(-148))} ${X(-194 + tailSw * 0.5)} ${f1(y(-210))} Q${X(-188)} ${f1(y(-252))} ${X(-170)} ${f1(y(-284))} Z" fill="${mane}" stroke="${maneD}" stroke-width="2" stroke-linejoin="round"/>`;
    // (ridden: a saddle cloth in the tack's color, the saddle on it, its seat dipping between the cantle and the pommel,
    // its flap, and the girth round the belly)
    // (ticket 498: the parade's saddle cloth, led too)
    const blanket = `<path d="M-30 ${f1(y(-314))} Q30 ${f1(y(-326))} 96 ${f1(y(-312))} L92 ${f1(y(-238))} Q30 ${f1(y(-228))} -26 ${f1(y(-240))} Z" fill="${clothC}" stroke="${tackD}" stroke-width="2.5" stroke-linejoin="round"/>`;
    const saddle = !ride ? (arm === "parade" ? blanket : "") : blanket +
      `<path d="M42 ${f1(y(-250))} L48 ${f1(y(-164))}" stroke="${tackD}" stroke-width="7" stroke-linecap="round"/>` +
      `<path d="M-16 ${f1(y(-344))} Q-6 ${f1(y(-320))} 30 ${f1(y(-320))} Q64 ${f1(y(-320))} 76 ${f1(y(-340))} L86 ${f1(y(-334))} Q88 ${f1(y(-300))} 70 ${f1(y(-292))} L-8 ${f1(y(-292))} Q-24 ${f1(y(-302))} -16 ${f1(y(-344))} Z" fill="${leather}" stroke="${leatherD}" stroke-width="2.5" stroke-linejoin="round"/>` +
      `<path d="M8 ${f1(y(-300))} L66 ${f1(y(-300))} L62 ${f1(y(-248))} Q36 ${f1(y(-240))} 12 ${f1(y(-248))} Z" fill="${mix(leather, "#000000", 0.12)}" stroke="${leatherD}" stroke-width="2" stroke-linejoin="round"/>`;
    // (the legs drawn in this order, their hooves listed so)
    const rf = leg(100, -200, true, OFF.rf, false), rh = leg(-160, -214, false, OFF.rh, false), lf = leg(122, -204, true, OFF.lf, true), lh = leg(-140, -218, false, OFF.lh, true);
    // (ticket 498) armoured: the near legs before the armour, the cloth hanging over their tops and under the saddle, the tail
    // over the cloth or the crupper, the head's pieces over the head; the mane and the head where nothing covers them. The near
    // hind leg still passes in front of the tail as it strides: with plate it is drawn after the tail (the crupper ends behind
    // its top); with a cloth the tail is drawn as with no armour, under the near legs, and again over the cloth, clipped to it
    const A = arm && horseArmourSide(arm, K, plumeSw, tack);
    const tailOn = A && A.tailOn ? `<clipPath id="horse-tail-cloth"><path${g + bob ? ` transform="translate(0 ${f1(g + bob)})"` : ""} d="${A.tailOn}"/></clipPath><g clip-path="url(#horse-tail-cloth)">${tail}</g>` : "";
    return { svg: !A ? rf + rh + tail + body + neck + maneP + saddle + head + maneOver + lf + lh
               : rf + rh + (A.tailOver ? "" : tail) + body + neck + (A.hideMane ? "" : maneP) + lf + (A.tailOver ? "" : lh) + onBody(A.body) + (A.tailOver ? tail + lh : tailOn) + onBody(A.neck) + saddle + onBody(A.chest) +
                 (A.hideHead ? "" : head + (A.hideMane ? "" : maneOver)) + onBody(A.head + A.after),
             over: "", ring: [240, y(-368)], bits: [[240, y(-368)], [240, y(-368)]], seat: [30, y(-326)], bob,
             // (ticket 492) its hooves, and its barrel: [x from, x to, its belly's height, its back's, how deep it is behind its hooves' line]
             hooves, barrel: [+X(-216), 172, 156 - bob, 318 - bob, 30] };
  }
  // (a leg seen end on, toward us or away: shorter as it lifts, the hoof's sole showing)
  const endOn = (x, top, w, fill, lift) => `<path d="M${f1(x - w)} ${f1(y(top))} L${f1(x + w)} ${f1(y(top))} L${f1(x + w * 0.64)} ${f1(g - 18 - lift)} L${f1(x - w * 0.64)} ${f1(g - 18 - lift)} Z" fill="${fill}" stroke="${ln}" stroke-width="2.5" stroke-linejoin="round"/>` +
    (sock ? `<path d="M${f1(x - w * 0.66)} ${f1(g - 70 - lift)} L${f1(x + w * 0.66)} ${f1(g - 70 - lift)} L${f1(x + w * 0.64)} ${f1(g - 18 - lift)} L${f1(x - w * 0.64)} ${f1(g - 18 - lift)} Z" fill="${white}"/>` : "") +
    `<rect x="${f1(x - w * 0.8)}" y="${f1(g - 18 - lift)}" width="${f1(w * 1.6)}" height="${lift ? 11 : 18}" rx="3" fill="${lift ? "#6a5848" : "#3b2f2a"}" stroke="#1f1612" stroke-width="2"/>` +
    (!shoe ? "" : lift ? `<path d="M${f1(x - w * 0.55)} ${f1(g - 16 - lift)} Q${f1(x)} ${f1(g - 5 - lift)} ${f1(x + w * 0.55)} ${f1(g - 16 - lift)}" fill="none" stroke="${shoe}" stroke-width="3.5" stroke-linecap="round"/>`
      : `<rect x="${f1(x - w * 0.8)}" y="${f1(g - 5)}" width="${f1(w * 1.6)}" height="5" rx="2" fill="${shoe}"/>`);
  const lifted = (off, sign) => { const [sw] = step(off); return Math.max(0, sign * sw) / (run ? 34 : 18) * (run ? 40 : 22); };
  if (view === "front") {
    // (facing us: the hind legs beyond; the chest, broad at the shoulders; the neck, its mane falling to one side; the
    // forelegs; the face hanging in front of the neck)
    const hindF = x => `<path d="M${f1(x - 13)} ${f1(y(-184))} L${f1(x + 13)} ${f1(y(-184))} L${f1(x + 9)} ${f1(g - 26)} L${f1(x - 9)} ${f1(g - 26)} Z" fill="${far}" stroke="${ln}" stroke-width="2"/><rect x="${f1(x - 11)}" y="${f1(g - 26)}" width="22" height="12" rx="3" fill="#2a211c"/>` + (shoe ? `<rect x="${f1(x - 11)}" y="${f1(g - 17.5)}" width="22" height="3.5" rx="1.5" fill="${shoe}"/>` : "");
    // (ridden, its head is carried lower, D, so more of the rider shows over it; centered, in front of them (ticket 458,
    // Matthew: "the horse head should be centered (and layered in front of) the rider, not off to the side"; 0.0.366 had
    // turned it aside, HX, kept at 0)
    // (ticket 498) armoured: its pieces at its sides behind, the neck's over its mane, the chest's over the forelegs' tops, the
    // face's over the face (the tournament's cloth in its place); the parade's saddle cloth, with tassels, led too
    const D = ride ? 110 : 0, HX = 0, A = arm && horseArmourFront(arm, K, D, plumeSw, tack), behind = hindF(-56) + hindF(56) + (A ? onBody(A.behind) : "") +
      (ride || arm === "parade" ? [-1, 1].map(k => `<path d="M${f1(k * 60)} ${f1(y(-334))} L${f1(k * 100)} ${f1(y(-328))} L${f1(k * 96)} ${f1(y(-246))} L${f1(k * 64)} ${f1(y(-250))} Z" fill="${clothC}" stroke="${tackD}" stroke-width="2.5" stroke-linejoin="round"/>` +
        (arm === "parade" ? onBody(horseTassels([k * 64, -250], [k * 96, -246], 3, K)) : "")).join("") : "");
    const near = `<path d="M-44 ${f1(y(-296))} L${-32 + HX} ${f1(y(-432 + D))} L${32 + HX} ${f1(y(-432 + D))} L44 ${f1(y(-296))} Z" fill="${coat}" stroke="${ln}" stroke-width="3" stroke-linejoin="round"/>` +
      (A && A.hideMane ? "" : (H.maneStyle || "loose") === "loose" ? `<path d="M${30 + HX} ${f1(y(-436 + D))} Q${50 + HX} ${f1(y(-410 + D))} ${42 + HX * 0.6} ${f1(y(-384 + D * 0.7))} Q54 ${f1(y(-358))} 46 ${f1(y(-332))} Q56 ${f1(y(-312))} 48 ${f1(y(-292))} L38 ${f1(y(-300))} Q${34 + HX * 0.5} ${f1(y(-370 + D * 0.5))} ${26 + HX} ${f1(y(-432 + D))} Z" fill="${mane}" stroke="${maneD}" stroke-width="2" stroke-linejoin="round"/>`
        : maneFall(H.maneStyle, [27 + HX, y(-434 + D)], [39, y(-298)], 1, mane, maneD, H.bows || cloth)) + (A ? onBody(A.neck) : "") +
      `<path d="M-60 ${f1(y(-326))} Q0 ${f1(y(-346))} 60 ${f1(y(-326))} Q86 ${f1(y(-300))} 84 ${f1(y(-252))} Q82 ${f1(y(-206))} 64 ${f1(y(-190))} L-64 ${f1(y(-190))} Q-82 ${f1(y(-206))} -84 ${f1(y(-252))} Q-86 ${f1(y(-300))} -60 ${f1(y(-326))} Z" fill="${coat}" stroke="${ln}" stroke-width="3" stroke-linejoin="round"/>` +
      `<path d="M0 ${f1(y(-300))} L0 ${f1(y(-206))} M-34 ${f1(y(-312))} Q-50 ${f1(y(-262))} -46 ${f1(y(-214))} M34 ${f1(y(-312))} Q50 ${f1(y(-262))} 46 ${f1(y(-214))}" fill="none" stroke="${mix(coat, "#000000", 0.2)}" stroke-width="3" stroke-linecap="round" opacity="0.6"/>` +
      endOn(-46, -214, 17, coat, lifted(OFF.lf, 1)) + endOn(46, -214, 17, coat, lifted(OFF.rf, 1)) + (A ? onBody(A.chest) : "") +
      `<g transform="translate(${HX} ${f1(y(-316 + D))}) scale(1.25)">${A && A.hideFace ? "" : horseFace(H.maneStyle === "bows" ? { ...H, bow: H.bows || cloth } : H, 0, tack, true)}${A ? A.face : ""}</g>`;
    return { svg: ride ? behind : behind + near, over: ride ? near : "", ring: [HX, y(-316 + D) - 77 * 1.25],
             bits: [-1, 1].map(k => [HX + k * 17, y(-316 + D) - 52 * 1.25]), seat: [0, y(-330)], bob,
             // (ticket 492: the forelegs toward us on the line, the hind legs drawn 26 up, that far back; its body, as long as it is, into the picture)
             hooves: [[-46, lifted(OFF.lf, 1), 0], [46, lifted(OFF.rf, 1), 0], [-56, 0, 26], [56, 0, 26]], barrel: [-56, 56, 184 - bob, 330 - bob, 110] };
  }
  // (from behind: the forelegs beyond; over the back, the neck and the back of the head, its ears, the mane falling to one
  // side; the haunches, the hind legs, the tail)
  const foreB = x => `<path d="M${f1(x - 12)} ${f1(y(-176))} L${f1(x + 12)} ${f1(y(-176))} L${f1(x + 9)} ${f1(g - 26)} L${f1(x - 9)} ${f1(g - 26)} Z" fill="${far}" stroke="${ln}" stroke-width="2"/><rect x="${f1(x - 11)}" y="${f1(g - 26)}" width="22" height="12" rx="3" fill="#2a211c"/>` + (shoe ? `<rect x="${f1(x - 11)}" y="${f1(g - 17.5)}" width="22" height="3.5" rx="1.5" fill="${shoe}"/>` : "");
  // (ticket 498) armoured: its pieces at its sides beyond the forelegs, the neck's over its mane and under the halter's strap,
  // the head's after the strap, the rump's over the haunches and the hind legs' tops, under the tail; the parade's saddle cloth
  const A = arm && horseArmourBack(arm, K, plumeSw);
  const behindB = foreB(-32) + foreB(32) + (A ? onBody(A.sides) : "") +
    `<path d="M-38 ${f1(y(-300))} Q-30 ${f1(y(-400))} -22 ${f1(y(-438))} Q0 ${f1(y(-458))} 22 ${f1(y(-438))} Q30 ${f1(y(-400))} 38 ${f1(y(-300))} Z" fill="${far}" stroke="${ln}" stroke-width="2.5" stroke-linejoin="round"/>` +
    [-1, 1].map(k => `<path d="M${f1(k * 8)} ${f1(y(-448))} L${f1(k * 24)} ${f1(y(-488))} L${f1(k * 24)} ${f1(y(-440))} Z" fill="${far}" stroke="${ln}" stroke-width="2" stroke-linejoin="round"/>`).join("") +
    (A && A.hideMane ? "" : (H.maneStyle || "loose") === "loose" ? `<path d="M-14 ${f1(y(-452))} Q-36 ${f1(y(-420))} -30 ${f1(y(-392))} Q-42 ${f1(y(-362))} -36 ${f1(y(-334))} Q-46 ${f1(y(-314))} -40 ${f1(y(-296))} L-30 ${f1(y(-300))} Q-22 ${f1(y(-380))} -6 ${f1(y(-446))} Z" fill="${mane}" stroke="${maneD}" stroke-width="2" stroke-linejoin="round"/>`
      : maneFall(H.maneStyle, [-9, y(-449)], [-32, y(-298)], -1, mane, maneD, H.bows || cloth)) + (A ? onBody(A.neck) : "") +
    `<path d="M-24 ${f1(y(-424))} Q0 ${f1(y(-414))} 24 ${f1(y(-424))}" fill="none" stroke="${tackD}" stroke-width="5" stroke-linecap="round"/>` + (A ? onBody(A.head) : "") +
    (ride || arm === "parade" ? [-1, 1].map(k => `<path d="M${f1(k * 70)} ${f1(y(-336))} L${f1(k * 102)} ${f1(y(-330))} L${f1(k * 98)} ${f1(y(-250))} L${f1(k * 74)} ${f1(y(-254))} Z" fill="${clothC}" stroke="${tackD}" stroke-width="2.5" stroke-linejoin="round"/>` +
      (arm === "parade" ? onBody(horseTassels([k * 74, -254], [k * 98, -250], 3, K)) : "")).join("") : "");
  const nearB = endOn(-48, -206, 17, coat, lifted(OFF.lh, -1)) + endOn(48, -206, 17, coat, lifted(OFF.rh, -1)) +
    `<path d="M-86 ${f1(y(-240))} Q-90 ${f1(y(-326))} -4 ${f1(y(-330))} L4 ${f1(y(-330))} Q90 ${f1(y(-326))} 86 ${f1(y(-240))} Q84 ${f1(y(-192))} 52 ${f1(y(-182))} L-52 ${f1(y(-182))} Q-84 ${f1(y(-192))} -86 ${f1(y(-240))} Z" fill="${coat}" stroke="${ln}" stroke-width="3"/>` +
    `<path d="M0 ${f1(y(-318))} L0 ${f1(y(-196))}" stroke="${ln}" stroke-width="2" opacity="0.5"/>` + (A ? onBody(A.rump) : "") +
    `<path d="M-14 ${f1(y(-322))} Q${f1(-26 + tailSw)} ${f1(y(-220))} ${f1(-12 + tailSw)} ${f1(y(-128))} L${f1(14 + tailSw)} ${f1(y(-128))} Q${f1(24 + tailSw)} ${f1(y(-220))} 14 ${f1(y(-322))} Z" fill="${mane}" stroke="${maneD}" stroke-width="2" stroke-linejoin="round"/>`;
  return { svg: ride ? behindB : behindB + nearB, over: ride ? nearB : "", ring: [-20, y(-422)], bits: [-1, 1].map(k => [k * 22, y(-420)]), seat: [0, y(-330)], bob,
           // (ticket 492: from behind, the hind legs on the line, the forelegs 26 up, that far back)
           hooves: [[-48, lifted(OFF.lh, -1), 0], [48, lifted(OFF.rh, -1), 0], [-32, 0, 26], [32, 0, 26]], barrel: [-48, 48, 176 - bob, 330 - bob, 110] };
}
// A broom through the hand at H, in body units: the stick, leaning a
// little, 120 above the hand and 52 below it, then the bristles flaring
// out under a band in the trim color.
function broomSvg([hx, hy], trim) {
  const top = [hx + 16, hy - 120], b = [hx - 8, hy + 52];
  const dx = b[0] - top[0], dy = b[1] - top[1], d = Math.hypot(dx, dy), ux = dx / d, uy = dy / d, nx = -uy, ny = ux;
  const at = (along, across) => [b[0] + ux * along + nx * across, b[1] + uy * along + ny * across];
  return line(top[0], top[1], b[0], b[1], 7, "#8b5a2b", "#4a2f16") +
    poly([at(0, 8), at(58, 25), at(66, 0), at(58, -25), at(0, -8)], "#d9a441", "#8a6420", 2.5) +
    [-2, -1, 0, 1, 2].map(k => { const [x1, y1] = at(4, k * 3), [x2, y2] = at(58, k * 9);
      return `<line x1="${f1(x1)}" y1="${f1(y1)}" x2="${f1(x2)}" y2="${f1(y2)}" stroke="#a87a2a" stroke-width="1.5"/>`; }).join("") +
    line(at(5, 9)[0], at(5, 9)[1], at(5, -9)[0], at(5, -9)[1], 6, trim);
}

// (ticket 518) THE THINGS A HAND HOLDS (HOLDABLES, js/01-state.js), each in each view its own drawing, never another
// turned or flipped (Matthew's rule): "front" (toward us: the costumes' drawings above, as they always were), "side" (in
// profile, `dir` the way they face) and "back" (from behind). The lasso and the bow already had their views; the shield its
// back. H the fist, in body units.
// A broom in profile: the stick leaning forward, the way they face; the bristles flaring down behind, seen across their round
function broomSideSvg([hx, hy], trim, dir) {
  const top = [hx + dir * 22, hy - 118], b = [hx - dir * 10, hy + 52];
  const dx = b[0] - top[0], dy = b[1] - top[1], d = Math.hypot(dx, dy), ux = dx / d, uy = dy / d, nx = -uy, ny = ux;
  const at = (along, across) => [b[0] + ux * along + nx * across, b[1] + uy * along + ny * across];
  return line(top[0], top[1], b[0], b[1], 7, "#8b5a2b", "#4a2f16") +
    poly([at(0, 7), at(56, 19), at(64, 0), at(56, -19), at(0, -7)], "#d9a441", "#8a6420", 2.5) +
    [-1, 0, 1].map(k => { const [x1, y1] = at(4, k * 3), [x2, y2] = at(56, k * 10);
      return `<line x1="${f1(x1)}" y1="${f1(y1)}" x2="${f1(x2)}" y2="${f1(y2)}" stroke="#a87a2a" stroke-width="1.5"/>`; }).join("") +
    line(at(5, 8)[0], at(5, 8)[1], at(5, -8)[0], at(5, -8)[1], 6, trim);
}
// A broom from behind: the stick leaning to their right (our left: the front's lean, seen from the other side), the band
// over the bristles' back, no ties showing
function broomBackSvg([hx, hy], trim) {
  const top = [hx - 16, hy - 120], b = [hx + 8, hy + 52];
  const dx = b[0] - top[0], dy = b[1] - top[1], d = Math.hypot(dx, dy), ux = dx / d, uy = dy / d, nx = -uy, ny = ux;
  const at = (along, across) => [b[0] + ux * along + nx * across, b[1] + uy * along + ny * across];
  return line(top[0], top[1], b[0], b[1], 7, "#7a4f22", "#4a2f16") +
    poly([at(0, 8), at(58, 25), at(66, 0), at(58, -25), at(0, -8)], "#c8933a", "#8a6420", 2.5) +
    line(at(7, 10)[0], at(7, 10)[1], at(7, -10)[0], at(7, -10)[1], 8, trim);
}
// A staff in profile: leaning forward, its head's knot hooked round the orb toward the way they face
function staffSideSvg([hx, hy], orb, dir) {
  const top = [hx + dir * 14, hy - 150], b = [hx - dir * 8, hy + 110];
  if (orb === "knob" || orb === "leaves") return line(top[0], top[1] - 6, b[0], b[1], 7, "#7a4f2a", "#4a2f16") +
    (orb === "knob" ? `<circle cx="${f1(top[0])}" cy="${f1(top[1] - 8)}" r="8" fill="#7a4f2a" stroke="#4a2f16" stroke-width="2"/>`
      : [[-10, -40], [-2, 30], [8, -60]].map(([dy, a]) => `<path d="M${f1(top[0])} ${f1(top[1] + dy)} Q${f1(top[0] + dir * 8)} ${f1(top[1] + dy - 8)} ${f1(top[0] + dir * (a > 0 ? 18 : 10))} ${f1(top[1] + dy - 18)} Q${f1(top[0] + dir * 2)} ${f1(top[1] + dy - 10)} ${f1(top[0])} ${f1(top[1] + dy)} Z" fill="#4caf50" stroke="#2e6b30" stroke-width="1.5"/>`).join(""));
  return line(top[0], top[1], b[0], b[1], 7, "#7a4f2a", "#4a2f16") +
    `<circle cx="${f1(top[0] + dir * 4)}" cy="${f1(top[1] - 12)}" r="22" fill="${orb}" opacity="0.25"/>` +
    `<circle cx="${f1(top[0] + dir * 4)}" cy="${f1(top[1] - 12)}" r="11" fill="${orb}" stroke="${darker(orb)}" stroke-width="2"/>` +
    `<path d="M${f1(top[0] - dir * 4)} ${f1(top[1] + 4)} Q${f1(top[0] - dir * 8)} ${f1(top[1] - 22)} ${f1(top[0] + dir * 6)} ${f1(top[1] - 26)} Q${f1(top[0] + dir * 16)} ${f1(top[1] - 24)} ${f1(top[0] + dir * 15)} ${f1(top[1] - 12)}" fill="none" stroke="#7a4f2a" stroke-width="5" stroke-linecap="round"/>` +
    `<circle cx="${f1(top[0] + dir * 8)}" cy="${f1(top[1] - 15)}" r="3" fill="#ffffff" opacity="0.8"/>`;
}
// A staff from behind: leaning to our left, the knot's back over the orb, which glows round it
function staffBackSvg([hx, hy], orb) {
  const top = [hx - 10, hy - 150], b = [hx + 6, hy + 110];
  if (orb === "knob" || orb === "leaves") return line(top[0], top[1] - 6, b[0], b[1], 7, "#6b4424", "#4a2f16") +
    (orb === "knob" ? `<circle cx="${f1(top[0])}" cy="${f1(top[1] - 8)}" r="8" fill="#6b4424" stroke="#4a2f16" stroke-width="2"/>`
      : [[1, -6, 30], [-1, 4, -35], [1, 12, 60]].map(([k, dy, a]) => `<path d="M0 0 Q${f1(k * 6)} -10 0 -20 Q${f1(-k * 6)} -10 0 0 Z" transform="translate(${f1(top[0])} ${f1(top[1] + dy)}) rotate(${a})" fill="#3f9a43" stroke="#2e6b30" stroke-width="1.5"/>`).join(""));
  return `<circle cx="${f1(top[0])}" cy="${f1(top[1] - 10)}" r="22" fill="${orb}" opacity="0.25"/>` +
    `<circle cx="${f1(top[0])}" cy="${f1(top[1] - 10)}" r="11" fill="${orb}" stroke="${darker(orb)}" stroke-width="2"/>` +
    line(top[0], top[1], b[0], b[1], 7, "#6b4424", "#4a2f16") +
    `<path d="M${f1(top[0] - 10)} ${f1(top[1] + 2)} Q${f1(top[0] - 13)} ${f1(top[1] - 16)} ${f1(top[0])} ${f1(top[1] - 22)} Q${f1(top[0] + 13)} ${f1(top[1] - 16)} ${f1(top[0] + 10)} ${f1(top[1] + 2)} Z" fill="#7a4f2a" stroke="#4a2f16" stroke-width="2"/>`;
}
// A scepter in profile: the rod leaning forward, the ball with its jewel on the side they face, the cross seen edge on
function scepterSideSvg([hx, hy], jewel, dir) {
  const t = [hx + dir * 8, hy - 130];
  return line(hx - dir * 2, hy + 30, t[0], t[1], 6, "#e0b030", "#8a6420") +
    `<circle cx="${f1(t[0])}" cy="${f1(t[1] - 8)}" r="11" fill="#e0b030" stroke="#8a6420" stroke-width="2"/>` +
    `<ellipse cx="${f1(t[0] + dir * 6)}" cy="${f1(t[1] - 8)}" rx="2.6" ry="5" fill="${jewel}"/>` +
    `<path d="M${f1(t[0])} ${f1(t[1] - 19)} L${f1(t[0])} ${f1(t[1] - 33)} M${f1(t[0] - 2)} ${f1(t[1] - 27)} L${f1(t[0] + 2)} ${f1(t[1] - 27)}" stroke="#e0b030" stroke-width="3.5" stroke-linecap="round"/>`;
}
// A scepter from behind: the ball's plain back (the jewel is on its front), a band round it, the cross
function scepterBackSvg([hx, hy]) {
  const t = [hx - 6, hy - 130];
  return line(hx + 2, hy + 30, t[0], t[1], 6, "#d4a52a", "#8a6420") +
    `<circle cx="${f1(t[0])}" cy="${f1(t[1] - 8)}" r="11" fill="#d4a52a" stroke="#8a6420" stroke-width="2"/>` +
    `<path d="M${f1(t[0] - 11)} ${f1(t[1] - 8)} Q${f1(t[0])} ${f1(t[1] - 3)} ${f1(t[0] + 11)} ${f1(t[1] - 8)}" fill="none" stroke="#8a6420" stroke-width="2"/>` +
    `<path d="M${f1(t[0])} ${f1(t[1] - 19)} L${f1(t[0])} ${f1(t[1] - 33)} M${f1(t[0] - 6)} ${f1(t[1] - 27)} L${f1(t[0] + 6)} ${f1(t[1] - 27)}" stroke="#d4a52a" stroke-width="3.5" stroke-linecap="round"/>`;
}
// A sword upright in the fist, in profile: the blade seen edge on, leaning a little forward, the crossguard end on, the
// grip in the hand, the pommel under it
function swordSideSvg([hx, hy], dir) {
  const tip = [hx + dir * 6, hy - 124], base = [hx + dir * 1, hy - 14];
  return `<polygon points="${f1(base[0] - 2.4)},${f1(base[1])} ${f1(tip[0] - 1.2)},${f1(tip[1] + 14)} ${f1(tip[0])},${f1(tip[1])} ${f1(tip[0] + 1.2)},${f1(tip[1] + 14)} ${f1(base[0] + 2.4)},${f1(base[1])}" fill="#c9ced6" stroke="#7d838e" stroke-width="1"/>` +
    `<line x1="${f1(base[0] + dir * 0.8)}" y1="${f1(base[1] - 2)}" x2="${f1(tip[0] + dir * 0.4)}" y2="${f1(tip[1] + 16)}" stroke="#f1f3f7" stroke-width="0.9"/>` +
    `<rect x="${f1(base[0] - 4)}" y="${f1(base[1] - 1)}" width="8" height="7" rx="2" fill="#b08d3a" stroke="#6e5520" stroke-width="0.9"/>` +
    `<rect x="${f1(hx - 3.2)}" y="${f1(base[1] + 6)}" width="6.4" height="20" rx="1.6" fill="#6e4a2c"/>` +
    `<circle cx="${f1(hx - dir * 0.5)}" cy="${f1(hy + 21)}" r="4.6" fill="#b08d3a" stroke="#6e5520" stroke-width="0.9"/>`;
}
// A sword upright in the fist, from behind: the blade's other flat, no gleam on it, the crossguard's back, the grip's
// binding hidden in the hand, the pommel
function swordBackSvg([hx, hy]) {
  const tip = [hx - 3, hy - 122], b = [hx - 1, hy - 12];
  return `<polygon points="${f1(b[0] - 4.8)},${f1(b[1])} ${f1(tip[0] - 3.7)},${f1(tip[1] + 17)} ${f1(tip[0])},${f1(tip[1])} ${f1(tip[0] + 3.7)},${f1(tip[1] + 17)} ${f1(b[0] + 4.8)},${f1(b[1])}" fill="#b9bec7" stroke="#7d838e" stroke-width="1"/>` +
    `<line x1="${f1(b[0])}" y1="${f1(b[1] - 4)}" x2="${f1(tip[0])}" y2="${f1(tip[1] + 20)}" stroke="#9aa0aa" stroke-width="1.5"/>` +
    `<rect x="${f1(hx - 15)}" y="${f1(b[1] - 1)}" width="28" height="5" rx="2" fill="#9c7a30" stroke="#6e5520" stroke-width="0.9"/>` +
    `<rect x="${f1(hx - 3.2)}" y="${f1(b[1] + 4)}" width="6.4" height="22" rx="1.6" fill="#5a3b22"/>` +
    `<circle cx="${f1(hx)}" cy="${f1(hy + 21)}" r="4.7" fill="#9c7a30" stroke="#6e5520" stroke-width="0.9"/>`;
}
// A shield in profile, held out the way they face: seen rim on, a narrow bowed lens of its face's colour edged in iron
function shieldSideSvg([hx, hy], WC, plain, dir) {
  const x = hx + dir * 10, face = plain ? "#8b5a2b" : WC.knightA || "#2f6db5";
  return `<path d="M${f1(x - dir * 3)} ${f1(hy - 38)} Q${f1(x + dir * 9)} ${f1(hy)} ${f1(x - dir * 1)} ${f1(hy + 38)} L${f1(x - dir * 6)} ${f1(hy + 36)} Q${f1(x + dir * 2)} ${f1(hy)} ${f1(x - dir * 8)} ${f1(hy - 38)} Z" fill="${face}" stroke="#3b3b44" stroke-width="2.5" stroke-linejoin="round"/>` +
    `<rect x="${f1(hx - 4)}" y="${f1(hy - 6)}" width="8" height="12" rx="2" fill="#4a2f16"/>`;
}
// A plain wooden shield toward us (anyone not a knight): planks, an iron rim and boss
function shieldPlainSvg([hx, hy]) {
  const d = `M${f1(hx - 30)} ${f1(hy - 38)} L${f1(hx + 30)} ${f1(hy - 38)} L${f1(hx + 30)} ${f1(hy - 4)} Q${f1(hx + 28)} ${f1(hy + 24)} ${f1(hx)} ${f1(hy + 38)} Q${f1(hx - 28)} ${f1(hy + 24)} ${f1(hx - 30)} ${f1(hy - 4)} Z`;
  return `<path d="${d}" fill="#a0703c" stroke="#3b3b44" stroke-width="3"/>` +
    [-10, 10].map(dx => `<line x1="${f1(hx + dx)}" y1="${f1(hy - 37)}" x2="${f1(hx + dx)}" y2="${f1(hy + 30 - Math.abs(dx))}" stroke="#7a4f2a" stroke-width="1.5"/>`).join("") +
    `<circle cx="${f1(hx)}" cy="${f1(hy - 6)}" r="8" fill="#9aa0aa" stroke="#3b3b44" stroke-width="2"/>`;
}
// A bow from behind: its string nearer us, over the bow, a leather wrap at the grip
function bowBackSvg([hx, hy], side) {
  const t = [hx, hy - 78], b = [hx, hy + 78], bx = hx + side * 30;
  return `<line x1="${f1(t[0])}" y1="${f1(t[1])}" x2="${f1(b[0])}" y2="${f1(b[1])}" stroke="#e8e0cc" stroke-width="1.5"/>` +
    `<path d="M${f1(t[0])} ${f1(t[1])} Q${f1(bx)} ${f1(hy)} ${f1(b[0])} ${f1(b[1])}" fill="none" stroke="#6b4424" stroke-width="6" stroke-linecap="round"/>` +
    `<path d="M${f1(hx + side * 13)} ${f1(hy - 9)} L${f1(hx + side * 13)} ${f1(hy + 9)}" stroke="#3b2414" stroke-width="7" stroke-linecap="round"/>`;
}
// (ticket 529) CRAWLING TOWARD US, each thing lies flat on the floor under its hand, its own drawing (never a standing one
// turned): seen from in front and a little above, so what runs across the picture is whole and what runs into it is seen
// short. Each lies out to the side, away from the body (s, the hand's side in the picture), the hand on its grip.
// A broom on the floor: the stick across, under the hand, the bristles fanned out beyond it, flat, their ties' band
function broomFloorSvg([hx, hy], trim, s) {
  const y = hy + 4, a = hx - s * 40, b = hx + s * 70, e = hx + s * 122;
  return `<ellipse cx="${f1((a + e) / 2)}" cy="${f1(y + 7)}" rx="${f1(Math.abs(e - a) / 2)}" ry="5" fill="#000" opacity="0.12"/>` +
    line(a, y, b, y, 7, "#8b5a2b", "#4a2f16") +
    poly([[b, y - 5], [e - s * 6, y - 11], [e, y], [e - s * 6, y + 9], [b, y + 5]], "#d9a441", "#8a6420", 2.5) +
    [-1, 0, 1].map(k => `<line x1="${f1(b + s * 4)}" y1="${f1(y + k * 2)}" x2="${f1(e - s * 8)}" y2="${f1(y + k * 6)}" stroke="#a87a2a" stroke-width="1.5"/>`).join("") +
    line(b + s * 5, y - 6, b + s * 5, y + 6, 6, trim);
}
// A staff on the floor: the shaft across, the head (orb, knob or leaves) lying out beyond the hand, the orb's glow flat on the floor
function staffFloorSvg([hx, hy], orb, s) {
  const y = hy + 4, a = hx - s * 70, t = hx + s * 120;
  const head = orb === "knob" ? `<ellipse cx="${f1(t + s * 6)}" cy="${f1(y)}" rx="8" ry="6" fill="#7a4f2a" stroke="#4a2f16" stroke-width="2"/>`
    : orb === "leaves" ? [[-1, -18], [1, 0], [-1, 18]].map(([k, dx]) => `<ellipse cx="${f1(t + s * (12 + dx / 3))}" cy="${f1(y + k * 6)}" rx="10" ry="4" fill="#4caf50" stroke="#2e6b30" stroke-width="1.5"/>`).join("")
    : `<ellipse cx="${f1(t + s * 12)}" cy="${f1(y + 2)}" rx="24" ry="9" fill="${orb}" opacity="0.25"/>` +
      `<circle cx="${f1(t + s * 12)}" cy="${f1(y - 3)}" r="10" fill="${orb}" stroke="${darker(orb)}" stroke-width="2"/>`;
  return `<ellipse cx="${f1((a + t) / 2)}" cy="${f1(y + 7)}" rx="${f1(Math.abs(t - a) / 2)}" ry="4" fill="#000" opacity="0.12"/>` + line(a, y, t, y, 7, "#7a4f2a", "#4a2f16") + head;
}
// A scepter on the floor: the rod across, the ball rolled against its end, the jewel up, the cross lying out flat
function scepterFloorSvg([hx, hy], jewel, s) {
  const y = hy + 4, a = hx - s * 26, t = hx + s * 104;
  return line(a, y, t, y, 6, "#e0b030", "#8a6420") +
    `<circle cx="${f1(t + s * 9)}" cy="${f1(y - 2)}" r="11" fill="#e0b030" stroke="#8a6420" stroke-width="2"/>` +
    `<ellipse cx="${f1(t + s * 9)}" cy="${f1(y - 5)}" rx="4" ry="2.6" fill="${jewel}"/>` +
    `<path d="M${f1(t + s * 20)} ${f1(y - 2)} L${f1(t + s * 34)} ${f1(y - 2)} M${f1(t + s * 28)} ${f1(y - 5)} L${f1(t + s * 28)} ${f1(y + 1)}" stroke="#e0b030" stroke-width="3.5" stroke-linecap="round"/>`;
}
// A sword on the floor: lying on its flat, the blade out across the floor from the fist, the crossguard into the picture
// (seen short), the pommel by the wrist
function swordFloorSvg([hx, hy], s) {
  const y = hy + 4, g = hx + s * 14, tip = hx + s * 128;
  return `<ellipse cx="${f1((g + tip) / 2)}" cy="${f1(y + 6)}" rx="${f1(Math.abs(tip - g) / 2)}" ry="3" fill="#000" opacity="0.12"/>` +
    `<polygon points="${f1(g)},${f1(y - 3.5)} ${f1(tip - s * 14)},${f1(y - 3)} ${f1(tip)},${f1(y)} ${f1(tip - s * 14)},${f1(y + 3)} ${f1(g)},${f1(y + 3.5)}" fill="#c9ced6" stroke="#7d838e" stroke-width="1"/>` +
    `<line x1="${f1(g + s * 2)}" y1="${f1(y - 0.8)}" x2="${f1(tip - s * 16)}" y2="${f1(y - 0.6)}" stroke="#f1f3f7" stroke-width="0.9"/>` +
    `<rect x="${f1(g - 3)}" y="${f1(y - 8)}" width="6" height="16" rx="2" fill="#b08d3a" stroke="#6e5520" stroke-width="0.9"/>` +
    `<circle cx="${f1(hx - s * 14)}" cy="${f1(y)}" r="4.6" fill="#b08d3a" stroke="#6e5520" stroke-width="0.9"/>`;
}
// A bow on the floor: lying on its side, its curve out across the floor, the string straight along the near edge
function bowFloorSvg([hx, hy], s) {
  const y = hy + 6, a = hx - s * 60, b = hx + s * 70, m = (a + b) / 2;
  return `<line x1="${f1(a)}" y1="${f1(y + 4)}" x2="${f1(b)}" y2="${f1(y + 4)}" stroke="#e8e0cc" stroke-width="1.5"/>` +
    `<path d="M${f1(a)} ${f1(y + 4)} Q${f1(m)} ${f1(y - 18)} ${f1(b)} ${f1(y + 4)}" fill="none" stroke="#6b4424" stroke-width="6" stroke-linecap="round"/>` +
    `<path d="M${f1(hx - 6)} ${f1(y - 5)} L${f1(hx + 6)} ${f1(y - 5)}" stroke="#3b2414" stroke-width="7" stroke-linecap="round"/>`;
}
// A shield on the floor: face up, flat, so seen short (an oval of its face in its colours, the boss in the middle), the
// hand on its rim
function shieldFloorSvg([hx, hy], WC, plain, s) {
  const cx = hx + s * 34, cy = hy + 6, A = plain ? "#a0703c" : WC.knightA || "#2f6db5", B2 = plain ? "#7a4f2a" : WC.knightB || "#f4f1ea";
  return `<ellipse cx="${f1(cx)}" cy="${f1(cy + 4)}" rx="36" ry="13" fill="#000" opacity="0.12"/>` +
    `<path d="M${f1(cx - 32)} ${f1(cy - 9)} L${f1(cx + 32)} ${f1(cy - 9)} Q${f1(cx + 30)} ${f1(cy + 6)} ${f1(cx)} ${f1(cy + 12)} Q${f1(cx - 30)} ${f1(cy + 6)} ${f1(cx - 32)} ${f1(cy - 9)} Z" fill="${A}" stroke="#3b3b44" stroke-width="2.5"/>` +
    `<path d="M${f1(cx)} ${f1(cy - 9)} L${f1(cx)} ${f1(cy + 12)} M${f1(cx - 31)} ${f1(cy - 1)} L${f1(cx + 31)} ${f1(cy - 1)}" stroke="${B2}" stroke-width="3"/>` +
    `<ellipse cx="${f1(cx)}" cy="${f1(cy - 1)}" rx="7" ry="3.5" fill="#9aa0aa" stroke="#3b3b44" stroke-width="1.5"/>`;
}
// A lasso on the floor (drawn only if one is ever held crawling: today it is put away, ticket 522): its coil flat, an oval
function lassoFloorSvg([hx, hy], s) {
  const cx = hx + s * 30, cy = hy + 6;
  return `<ellipse cx="${f1(cx)}" cy="${f1(cy)}" rx="28" ry="8" fill="none" stroke="#c8a165" stroke-width="3"/>` +
    `<ellipse cx="${f1(cx)}" cy="${f1(cy)}" rx="22" ry="6" fill="none" stroke="#a8834a" stroke-width="2.5"/>`;
}
// One held thing, by id, in a view ("front", "side", "back", or "floor": crawling toward us, ticket 529): o = { WC, kb (the broom's size), monk, dryad, knight, own ("L"/"R"), out (the hand's own
// side, or the way they face in profile), dir, spin (the lasso's twirl) }. The rein draws nothing here.
function heldSvg(id, H, view, o) {
  const WC = o.WC, side = view === "side", back = view === "back", dir = o.dir;
  const orb = o.monk ? (o.dryad ? "leaves" : "knob") : WC.staffOrb || "#7dd3fc";
  if (view === "floor") { const sd = Math.sign(H[0]) || 1;   // (ticket 529: crawling toward us)
    switch (id) {
      case "broom": { const k = o.kb; return `<g transform="translate(${f1(H[0])} ${f1(H[1])}) scale(${k.toFixed(3)}) translate(${f1(-H[0])} ${f1(-H[1])})">${broomFloorSvg(H, WC.trimColor, sd)}</g>`; }
      case "bow": return bowFloorSvg(H, sd);
      case "scepter": return scepterFloorSvg(H, WC.crownJewel || "#d2382b", sd);
      case "sword": return swordFloorSvg(H, sd);
      case "shield": return shieldFloorSvg(H, WC, !o.knight, sd);
      case "staff": return staffFloorSvg(H, orb, sd);
      case "lasso": return `<g data-western="lasso">${lassoFloorSvg(H, sd)}</g>`;
      default: return "";
    } }
  switch (id) {
    case "broom": { const k = o.kb, d = side ? broomSideSvg(H, WC.trimColor, dir) : back ? broomBackSvg(H, WC.trimColor) : broomSvg(H, WC.trimColor);
      return `<g transform="translate(${f1(H[0])} ${f1(H[1])}) scale(${k.toFixed(3)}) translate(${f1(-H[0])} ${f1(-H[1])})">${d}</g>`; }
    case "bow": return side ? bowSvg(H, -dir) : back ? bowBackSvg(H, o.out) : bowSvg(H, o.out);
    case "scepter": return side ? scepterSideSvg(H, WC.crownJewel || "#d2382b", dir) : back ? scepterBackSvg(H) : scepterSvg(H, WC.crownJewel || "#d2382b");
    case "sword": return side ? swordSideSvg(H, dir) : back ? swordBackSvg(H) : swordSvg(H[0] - 2, H[1] + 20, H[0] + 3, H[1] - 122, 1);   // (toward us, the trap room's sword upright)
    case "shield": return side ? shieldSideSvg(H, WC, !o.knight, dir) : back || o.knight ? shieldSvg(H, WC, o.own === "R" ? "shield-arms" : "shield-arms" + o.own, back) : shieldPlainSvg(H);
    case "staff": return side ? staffSideSvg(H, orb, dir) : back ? staffBackSvg(H, orb) : staffSvg(H, orb);
    case "lasso": return `<g data-western="lasso">${lassoSvg(H, view, side ? dir : o.out, o.spin)}</g>`;
    default: return "";
  }
}

// (ticket 499, Matthew: "optional dragon breath -- need an action for that, like bubbles -- shooting fire out of our
// mouth."; "let's get serious about animating fire shooting out of things. Mock up the dragon costume and 6 fire styles.")
// The six fires of the mock-up (dress-up/mockups/dragon-fire.html), each a fixed set of shapes, each shape moved by a CSS
// animation of its own (style.css: fi-… side on, fi-f-… toward us, fi-a-… away from us), begun its own way in (its delay),
// so the drawing is the same every time and the page does nothing a frame for it. In the friend's units, from where it
// leaves them, (0, 0): side on toward +x (whoever places it mirrors it for our left, and tilts it); toward us, coming at us
// about (0, 0), the mouth; away from us, up from (0, 0), the top of the head, smaller as it goes. A new angle is its own
// drawing (Matthew's rule): toward us and away are drawn as such, not the side-on fire turned.
const FIRE_STYLES = [["roar", "🔥 roaring flame"], ["balls", "☄️ fireballs"], ["twist", "🌀 twisting fire"], ["jet", "🔵 blue-hot jet"],
                     ["bricks", "🧱 brick fire"], ["bubbles", "🫧 fire bubbles"]];
// (ticket 499, Matthew: "let's have the dragon fire be the best tool against bubbles -- can you add logic for that -- the heat
// extend further than the fire.") Each fire's heat, about half again as far as its flames, in the friend's units: side on
// and away, a cone from (0, 0), how far it reaches and its half-angle in degrees (the mock-up's HEAT, side on); toward us, a
// disc about the mouth, its radius.
const FIRE_HEAT = {
  side: { roar: [600, 15], balls: [640, 9], twist: [570, 12], jet: [660, 6], bricks: [560, 12], bubbles: [500, 16] },
  front: { roar: 230, balls: 210, twist: 225, jet: 200, bricks: 235, bubbles: 200 },
  away: { roar: [380, 22], balls: [400, 14], twist: [360, 22], jet: [330, 12], bricks: [350, 18], bubbles: [360, 22] },
};
const fireStyleOf = B => FIRE_STYLES.some(([v]) => v === B.fireStyle) ? B.fireStyle : "roar";
// (the mock-up's shapes: a flame's puff, a smaller one, a round one for one coming at us, a toy flame piece standing on its clip)
const FIRE_BLOB = "M-26 0 C-14 -20 18 -22 30 0 C18 22 -14 20 -26 0 Z", FIRE_SMALL = "M-18 0 C-10 -14 12 -15 20 0 C12 15 -10 14 -18 0 Z";
const FIRE_ROUND = "M-22 0 C-22 -15 -11 -23 1 -23 C14 -22 23 -12 22 1 C21 14 12 22 -1 22 C-13 22 -22 13 -22 0 Z";
const FIRE_FLAME = "M-18 0 C-24 -16 -14 -28 -12 -44 C-6 -34 -4 -28 -2 -24 C0 -38 4 -50 10 -62 C12 -46 16 -38 14 -28 C20 -34 22 -40 24 -46 C30 -28 28 -10 18 0 Z";
const FIRE_BRICKS = [["#ff8c1a", "#b04c00"], ["#ffd21f", "#a07c00"], ["#f0401c", "#931a0a"]];
// (sparks flying out from a burst: n strokes round its middle, r long)
const fireBurst = (n, r, col) => { let s = ""; for (let k = 0; k < n; k++) { const a = k / n * Math.PI * 2;
  s += `<path d="M${f1(Math.cos(a) * r * 0.35)} ${f1(Math.sin(a) * r * 0.35)} L${f1(Math.cos(a) * r)} ${f1(Math.sin(a) * r)}" stroke="${col}" stroke-width="3.2" stroke-linecap="round"/>`; }
  return s + `<circle r="${f1(r * 0.25)}" fill="#fff8d0"/>`; };
const fireGlow = (g = "fi-g-glow", rx = 30, ry = 20) => `<ellipse class="fi-glow" rx="${rx}" ry="${ry}" fill="url(#${g})"/>`;
const fireAnim = (cls, delay, inner, extra = "") => `<g class="${cls}" style="animation-delay:${delay}s"${extra}>${inner}</g>`;
const fireShape = (cls, d, delay, fill, extra = "") => `<path class="${cls}" d="${d}" fill="${fill}" style="animation-delay:${delay}s"${extra}/>`;
const fireTurn = deg => `<g transform="rotate(${f1(deg)})">`;
const fireFlame = (f, st) => `<path d="${FIRE_FLAME}" fill="${f}" fill-opacity="0.82" stroke="${st}" stroke-width="2.2" stroke-linejoin="round"/>` +
  `<path d="M-10 -6 C-14 -18 -8 -26 -7 -36" stroke="#fff" stroke-opacity="0.75" stroke-width="3" fill="none" stroke-linecap="round"/>` +
  `<rect x="-9" y="-2" width="18" height="9" rx="2" fill="${f}" stroke="${st}" stroke-width="2"/>`;
const fireBubble = flame => `<circle r="19" fill="url(#fi-g-bub)" stroke="#ffb347" stroke-width="2.2"/>` +
  (flame ? `<path class="fi-bub-flame" d="M-6 8 C-10 0 -4 -6 0 -12 C2 -4 8 -2 6 8 Z" fill="#ffd23f"/>` : "") +
  `<ellipse cx="-7" cy="-8" rx="5" ry="3" fill="#fff" opacity="0.85" transform="rotate(-30 -7 -8)"/>`;
const FIRE_DRAW = {
  // side on, as the mock-up draws each
  side: {
    roar() {
      let s = "";
      for (let i = 0; i < 6; i++) s += fireTurn(-4 - (i % 3) * 3) + `<circle class="fi-smoke" r="20" fill="#5e5853" opacity="0" style="animation-delay:${-(i / 6 * 1.8).toFixed(2)}s"/></g>`;
      s += fireGlow();
      for (let i = 0; i < 26; i++) s += fireTurn(((i * 7) % 13 - 6) * 1.6) + fireShape("fi-roar", FIRE_BLOB, -(i / 26 * 0.9).toFixed(3), "#ffe14d") + `</g>`;
      for (let i = 0; i < 12; i++) s += fireTurn(((i * 5) % 7 - 3) * 1.4) + fireShape("fi-core", FIRE_BLOB, -(i / 12 * 0.45).toFixed(3), "#ffffff") + `</g>`;
      return s;
    },
    balls() {
      let s = fireGlow();
      for (let i = 0; i < 3; i++) {
        const d = -(i * 0.5);
        s += fireAnim("fi-fb", d, `<g class="fi-tail"><path d="M2 -15 C-40 -17 -86 -6 -120 0 C-86 6 -40 17 2 15 Z" fill="url(#fi-g-tail)"/><path d="M2 -8 C-26 -9 -52 -3 -70 0 C-52 3 -26 9 2 8 Z" fill="#ffe680"/></g><circle r="17" fill="url(#fi-g-ball)"/>`);
        s += `<g transform="translate(430 -4)">` + fireAnim("fi-burst", d, fireBurst(12, 36, "#ffb02e")) + `</g>`;
      }
      return s;
    },
    twist() {
      let s = fireGlow();
      for (const [r, fill] of [["a", "#ffc83d"], ["b", "#fff176"]]) {
        s += `<g class="fi-twirl">`;
        for (let i = 0; i < 20; i++) s += fireShape("fi-tw" + r, FIRE_BLOB, -(i / 20 * 1.1).toFixed(3), fill);
        s += `</g>`;
      }
      return s;
    },
    jet() {
      const cone = (len, hw) => `M0 ${f1(-hw * 0.45)} C${f1(len * 0.25)} ${-hw} ${f1(len * 0.65)} ${-hw} ${len} 0 C${f1(len * 0.65)} ${hw} ${f1(len * 0.25)} ${hw} 0 ${f1(hw * 0.45)} Z`;
      let s = fireGlow("fi-g-blueglow");
      for (let i = 0; i < 8; i++) s += fireTurn((i % 4 - 1.5) * 1.6) + fireShape("fi-jettip", FIRE_SMALL, -(i / 8 * 0.5).toFixed(3), "#ffb347") + `</g>`;
      s += `<g class="fi-jet"><path d="${cone(350, 26)}" fill="#1d4ed8" opacity="0.45"/><path d="${cone(300, 15)}" fill="#60a5fa" opacity="0.8"/><path d="${cone(210, 8)}" fill="#e0f2fe"/><path d="${cone(100, 4)}" fill="#fff"/></g>`;
      for (let i = 0; i < 10; i++) s += fireTurn(((i * 3) % 7 - 3) * 1.2) + `<path class="fi-spark" d="M0 0 L16 0" stroke="#fff" stroke-width="2.4" stroke-linecap="round" style="animation-delay:${-(i / 10 * 0.38).toFixed(3)}s"/></g>`;
      return s;
    },
    bricks() {
      let s = fireGlow();
      for (let i = 0; i < 7; i++) {
        const [f, st] = FIRE_BRICKS[i % 3], x = 34 + 54 * i, y = i % 2 ? 9 : -9, rot = 90 + (i % 2 ? 14 : -14), sc = 0.75 + i * 0.09;
        s += `<g transform="translate(${x} ${y}) rotate(${rot}) scale(${sc.toFixed(2)})">` + fireAnim("fi-brick", (i * 0.11).toFixed(2), fireFlame(f, st)) + `</g>`;
      }
      return s;
    },
    bubbles() {
      let s = fireGlow();
      const sizes = [0.8, 1.1, 0.9, 1.25, 0.75, 1, 1.15, 0.85];
      for (let i = 0; i < 8; i++) {
        const d = -(i / 8 * 2.4).toFixed(2);
        s += fireTurn((i % 4 - 1.5) * 5) + fireAnim("fi-fbub", d, `<g transform="scale(${sizes[i]})">${fireBubble(true)}</g>`) +
             `<g transform="translate(305 -22)">` + fireAnim("fi-bubpop", d, fireBurst(8, 26, "#ffb347")) + `</g></g>`;
      }
      return s;
    },
  },
  // toward us: each coming at us out of the mouth, spreading out round it and growing as it nears us, then gone
  front: {
    roar() {
      let s = "";
      for (let i = 0; i < 8; i++) s += fireTurn(i * 45 + 20) + `<circle class="fi-f-smoke" r="20" fill="#5e5853" opacity="0" style="animation-delay:${-(i / 8 * 1.8).toFixed(2)}s"/></g>`;
      s += fireGlow("fi-g-glow", 44, 40);
      for (let i = 0; i < 24; i++) s += fireTurn(i * 137.5) + fireShape("fi-f-roar", FIRE_ROUND, -(i / 24 * 1).toFixed(3), "#ffe14d") + `</g>`;
      for (let i = 0; i < 10; i++) s += fireTurn(i * 97 + 30) + fireShape("fi-f-core", FIRE_ROUND, -(i / 10 * 0.5).toFixed(3), "#ffffff") + `</g>`;
      return s;
    },
    balls() {
      let s = fireGlow("fi-g-glow", 40, 36);
      // (a ball seen coming at us: its tail hidden behind it, but for the flames licking out all round it)
      const corona = `<g class="fi-flick">` + [0, 1, 2, 3, 4, 5, 6, 7].map(j => `<path d="M14 -7 C24 -6 32 -2 36 0 C32 2 24 6 14 7 Z" fill="${j % 2 ? "#ff9b1f" : "#ffd23f"}" transform="rotate(${j * 45 + 10})"/>`).join("") + `</g>`;
      for (let i = 0; i < 3; i++) {
        const d = -(i * 0.5), a = [205, 330, 80][i];
        s += fireTurn(a) + fireAnim("fi-f-ball", d, corona + `<circle r="17" fill="url(#fi-g-ball)"/>`) +
             `<g transform="translate(64 0)">` + fireAnim("fi-f-burst", d, fireBurst(12, 36, "#ffb02e")) + `</g></g>`;
      }
      return s;
    },
    twist() {
      // (two ribbons, one red and one yellow, coming at us twisting round each other: a spiral of each, turning out from the mouth)
      let s = fireGlow("fi-g-glow", 40, 36);
      for (const [r, turn, fill] of [["a", 0, "#ffc83d"], ["b", 180, "#fff176"]]) {
        s += fireTurn(turn);
        for (let i = 0; i < 20; i++) s += fireShape("fi-f-tw" + r, FIRE_ROUND, -(i / 20 * 1.1).toFixed(3), fill);
        s += `</g>`;
      }
      return s;
    },
    jet() {
      // (looking down the jet: a white-hot middle in rings of blue, flickering; orange where it ends, and sparks flying at us)
      let s = fireGlow("fi-g-blueglow", 52, 50);
      for (let i = 0; i < 10; i++) s += fireTurn(i * 36 + 8) + fireShape("fi-f-jettip", FIRE_SMALL, -(i / 10 * 0.5).toFixed(3), "#ffb347") + `</g>`;
      s += `<g class="fi-f-jet"><circle r="38" fill="#1d4ed8" opacity="0.45"/><circle r="26" fill="#60a5fa" opacity="0.8"/><circle r="15" fill="#e0f2fe"/><circle r="7" fill="#fff"/></g>`;
      for (let i = 0; i < 12; i++) s += fireTurn(i * 67 + 15) + `<path class="fi-f-spark" d="M0 0 L12 0" stroke="#fff" stroke-width="2.4" stroke-linecap="round" style="animation-delay:${-(i / 12 * 0.38).toFixed(3)}s"/></g>`;
      return s;
    },
    bricks() {
      // (the pieces click on one by one round the mouth, spiralling out, each nearer us and so bigger, pointing out from it)
      let s = fireGlow("fi-g-glow", 40, 36);
      for (let i = 0; i < 9; i++) {
        const [f, st] = FIRE_BRICKS[i % 3], a = i * 100 + 20, r = 22 + 15 * i, sc = 0.45 + i * 0.13;
        s += `<g transform="rotate(${a}) translate(${r} 0) rotate(90) scale(${sc.toFixed(2)})">` + fireAnim("fi-brick", (i * 0.11).toFixed(2), fireFlame(f, st)) + `</g>`;
      }
      return s;
    },
    bubbles() {
      let s = fireGlow("fi-g-glow", 40, 36);
      const sizes = [0.8, 1.1, 0.9, 1.25, 0.75, 1, 1.15, 0.85];
      for (let i = 0; i < 8; i++) {
        const d = -(i / 8 * 2.4).toFixed(2);
        s += fireTurn(i * 137.5 + 10) + fireAnim("fi-f-bub", d, `<g transform="scale(${sizes[i]})">${fireBubble(true)}</g>`) +
             `<g transform="translate(66 0)">` + fireAnim("fi-f-bubpop", d, fireBurst(8, 26, "#ffb347")) + `</g></g>`;
      }
      return s;
    },
  },
  // away from us: from behind the head, up and away into the picture, each part smaller as it goes
  away: {
    roar() {
      let s = "";
      for (let i = 0; i < 6; i++) s += `<g transform="translate(${(i % 3 - 1) * 14} 0)"><circle class="fi-a-smoke" r="20" fill="#5e5853" opacity="0" style="animation-delay:${-(i / 6 * 1.8).toFixed(2)}s"/></g>`;
      s += fireGlow("fi-g-glow", 36, 26);
      for (let i = 0; i < 22; i++) s += fireTurn(((i * 7) % 11 - 5) * 1.2) + fireShape("fi-a-roar", FIRE_ROUND, -(i / 22 * 1).toFixed(3), "#ffe14d") + `</g>`;
      for (let i = 0; i < 10; i++) s += fireTurn(((i * 5) % 7 - 3) * 2) + fireShape("fi-a-core", FIRE_ROUND, -(i / 10 * 0.5).toFixed(3), "#ffffff") + `</g>`;
      return s;
    },
    balls() {
      let s = fireGlow("fi-g-glow", 32, 24);
      for (let i = 0; i < 3; i++) {
        const d = -(i * 0.5), a = [-5, 6, 0][i];
        // (its tail toward us, below it: short, seen end on)
        s += fireTurn(a) + fireAnim("fi-a-ball", d, `<g class="fi-tail"><path d="M-15 -2 C-16 18 -6 38 0 52 C6 38 16 18 15 -2 Z" fill="url(#fi-g-tailup)"/><path d="M-8 -2 C-9 10 -3 22 0 30 C3 22 9 10 8 -2 Z" fill="#ffe680"/></g><circle r="17" fill="url(#fi-g-ball)"/>`) +
             `<g transform="translate(0 -262)">` + fireAnim("fi-a-burst", d, fireBurst(12, 36, "#ffb02e")) + `</g></g>`;
      }
      return s;
    },
    twist() {
      let s = fireGlow("fi-g-glow", 32, 24);
      for (const [r, fill] of [["a", "#ffc83d"], ["b", "#fff176"]]) {
        s += `<g class="fi-a-twirl">`;
        for (let i = 0; i < 18; i++) s += fireShape("fi-a-tw" + r, FIRE_ROUND, -(i / 18 * 1.1).toFixed(3), fill);
        s += `</g>`;
      }
      return s;
    },
    jet() {
      // (the jet up and away from the top of the head: short, as it is seen end on, tapering to where it goes)
      const cone = (len, hw) => `M${f1(-hw * 0.45)} 0 C${-hw} ${f1(-len * 0.25)} ${-hw} ${f1(-len * 0.6)} 0 ${-len} C${hw} ${f1(-len * 0.6)} ${hw} ${f1(-len * 0.25)} ${f1(hw * 0.45)} 0 Z`;
      let s = fireGlow("fi-g-blueglow", 36, 26);
      for (let i = 0; i < 8; i++) s += fireTurn(-90 + (i % 4 - 1.5) * 3) + fireShape("fi-a-jettip", FIRE_SMALL, -(i / 8 * 0.5).toFixed(3), "#ffb347") + `</g>`;
      s += `<g class="fi-a-jet"><path d="${cone(200, 28)}" fill="#1d4ed8" opacity="0.45"/><path d="${cone(172, 17)}" fill="#60a5fa" opacity="0.8"/><path d="${cone(120, 9)}" fill="#e0f2fe"/><path d="${cone(60, 4)}" fill="#fff"/></g>`;
      for (let i = 0; i < 10; i++) s += fireTurn(-90 + ((i * 3) % 7 - 3) * 2.4) + `<path class="fi-a-spark" d="M0 0 L14 0" stroke="#fff" stroke-width="2.4" stroke-linecap="round" style="animation-delay:${-(i / 10 * 0.38).toFixed(3)}s"/></g>`;
      return s;
    },
    bricks() {
      // (a column of pieces going away up from the head, each farther and so smaller, clicking on one by one)
      let s = fireGlow("fi-g-glow", 32, 24);
      let y = -14;
      for (let i = 0; i < 7; i++) {
        const [f, st] = FIRE_BRICKS[i % 3], sc = 1.05 - i * 0.11, x = i % 2 ? 7 * sc : -7 * sc;
        s += `<g transform="translate(${f1(x)} ${f1(y)}) rotate(${i % 2 ? 12 : -12}) scale(${sc.toFixed(2)})">` + fireAnim("fi-brick", (i * 0.11).toFixed(2), fireFlame(f, st)) + `</g>`;
        y -= 40 * sc;
      }
      return s;
    },
    bubbles() {
      let s = fireGlow("fi-g-glow", 32, 24);
      const sizes = [0.8, 1.1, 0.9, 1.25, 0.75, 1, 1.15, 0.85];
      for (let i = 0; i < 8; i++) {
        const d = -(i / 8 * 2.4).toFixed(2);
        s += fireTurn((i % 4 - 1.5) * 7) + fireAnim("fi-a-bub", d, `<g transform="scale(${sizes[i]})">${fireBubble(true)}</g>`) +
             `<g transform="translate(0 -236)">` + fireAnim("fi-a-bubpop", d, fireBurst(8, 26, "#ffb347")) + `</g></g>`;
      }
      return s;
    },
  },
};
// its heat, as the mock-up shows it: the cone (or, coming at us, the disc) it reaches, faint, its edge shimmering
function fireHeatSvg(style, view) {
  const H = FIRE_HEAT[view][style], stroke = `stroke="#ffb066" stroke-opacity="0.5" stroke-width="3" stroke-dasharray="10 12"`;
  if (view === "front") return `<circle class="fi-heat" r="${H}" fill="url(#fi-g-heatf)" ${stroke}/>`;
  const [R, a] = H, x = f1(R * Math.cos(a * Math.PI / 180)), y = f1(R * Math.sin(a * Math.PI / 180));
  const cone = `<path class="fi-heat" d="M0 0 L${x} ${-y} A${R} ${R} 0 0 1 ${x} ${y} Z" fill="url(#fi-g-heat)" ${stroke}/>`;
  return view === "away" ? `<g transform="rotate(-90)">${cone}</g>` : cone;
}
// one fire: its heat (if shown) under its flames
const fireSvg = (style, view, heat) => (heat ? fireHeatSvg(style, view) : "") + FIRE_DRAW[view][style]();
// the open mouth it comes out of, in the friend's units for a head of the usual size (k its size over that): side on, a
// dark oval at the front of the face (over the fire's root, toward -x); toward us, a round "O", under the fire
const fireMouth = (view, k) => view === "side" ? `<ellipse cx="${f1(-5 * k)}" cy="0" rx="${f1(7 * k)}" ry="${f1(6 * k)}" fill="#5a1414" stroke="#000" stroke-width="${f1(1.8 * k)}"/>`
  : view === "front" ? `<ellipse rx="${f1(10 * k)}" ry="${f1(9 * k)}" fill="#5a1414" stroke="#000" stroke-width="${f1(1.8 * k)}"/><ellipse cy="${f1(4 * k)}" rx="${f1(5.5 * k)}" ry="${f1(3 * k)}" fill="#e75480" opacity="0.8"/>` : "";
// the gradients the fires use, once in the picture they are drawn in
const FIRE_DEFS = `<radialGradient id="fi-g-glow"><stop offset="0" stop-color="#fff8d0"/><stop offset="0.4" stop-color="#ffcf40" stop-opacity="0.9"/><stop offset="0.7" stop-color="#ff7a1a" stop-opacity="0.45"/><stop offset="1" stop-color="#ff7a1a" stop-opacity="0"/></radialGradient>` +
  `<radialGradient id="fi-g-blueglow"><stop offset="0" stop-color="#ffffff"/><stop offset="0.45" stop-color="#93c5fd" stop-opacity="0.85"/><stop offset="1" stop-color="#2563eb" stop-opacity="0"/></radialGradient>` +
  `<radialGradient id="fi-g-ball" fx="0.38" fy="0.38"><stop offset="0" stop-color="#ffffff"/><stop offset="0.3" stop-color="#fff3a0"/><stop offset="0.65" stop-color="#ffb02e"/><stop offset="1" stop-color="#e8401c"/></radialGradient>` +
  `<linearGradient id="fi-g-tail" x1="1" y1="0" x2="0" y2="0"><stop offset="0" stop-color="#ff9b1f"/><stop offset="1" stop-color="#e8401c" stop-opacity="0"/></linearGradient>` +
  `<linearGradient id="fi-g-tailup" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#ff9b1f"/><stop offset="1" stop-color="#e8401c" stop-opacity="0"/></linearGradient>` +
  `<radialGradient id="fi-g-bub"><stop offset="0" stop-color="#ffc850" stop-opacity="0.15"/><stop offset="0.75" stop-color="#ff8c1e" stop-opacity="0.35"/><stop offset="1" stop-color="#ff6e14" stop-opacity="0.8"/></radialGradient>` +
  `<radialGradient id="fi-g-heat" cx="0" cy="0.5" r="1"><stop offset="0" stop-color="#ff9a3c" stop-opacity="0.22"/><stop offset="0.7" stop-color="#ff9a3c" stop-opacity="0.1"/><stop offset="1" stop-color="#ff9a3c" stop-opacity="0.02"/></radialGradient>` +
  `<radialGradient id="fi-g-heatf"><stop offset="0" stop-color="#ff9a3c" stop-opacity="0.22"/><stop offset="0.7" stop-color="#ff9a3c" stop-opacity="0.1"/><stop offset="1" stop-color="#ff9a3c" stop-opacity="0.02"/></radialGradient>`;
