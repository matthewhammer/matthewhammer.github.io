"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 3 of 26 of the game's script, run in order by index.html.

// ===================== THE FACE (Kids' Choice, ported) =====================
// Drawn in its own units: 0..200 across, the head a circle of radius 80
// centered at (100, 110.6), its chin at (100, 190.6).

const PRESETS = [[220, 20, 60], [140, 190, 50], [170, 178, 186], [207, 160, 0]];
// A colour anywhere in the range of human skin, pale to very dark: an
// orange-ish hue, some colour but not a vivid one. Generous on purpose.
function isSkin([r, g, b]) {
  const max = Math.max(r, g, b), min = Math.min(r, g, b);
  if (max < 20 || max === min) return false;
  const s = (max - min) / max;
  const h = max === r ? 60 * ((g - b) / (max - min)) : max === g ? 120 + 60 * ((b - r) / (max - min))
    : 240 + 60 * ((r - g) / (max - min));
  return h >= 3 && h <= 55 && s >= 0.12 && s <= 0.85;
}
// Which row of feelings: every skin tone has the everyday ones (the
// cartoon yellow's row), so a skin colour never changes the feeling; an
// unnatural colour (red, green, grey) has its own silly ones.
function family(hex) {
  // the witch's green has a row of its own: witchy, never sick
  if (hex.toLowerCase() === WITCH_SKIN) return 4;
  const [r, g, b] = hexToRgb(hex);
  if (isSkin([r, g, b])) return 3;
  const d = PRESETS.map(([pr, pg, pb]) => (r - pr) ** 2 + (g - pg) ** 2 + (b - pb) ** 2);
  return d.indexOf(Math.min(...d));
}
function mood(f, brow, smile) {
  const up = brow < 50, smiling = smile >= 50;
  const table = [
    ["disgusted", "shocked", "mischievous", "angry"],
    ["sick", "relieved", "queasy", "grumpy"],
    ["curious", "puzzled", "scheming", "homesick"],
    ["happy", "worried", "determined", "sulky"],
    ["witchy", "spooked", "cackling", "hexed"],
  ][f];
  return table[(up ? 0 : 2) + (smiling ? 0 : 1)];
}

const mouthPath = v => `M60 150 Q100 ${110 + idiv(4 * v, 5)} 140 150`;
// The lips' two control points (the mouth's top and bottom curves), for a
// smile v and an opening o. Closed, both are the smile's own curve. A smile
// (or a flat mouth) opens like a D: its top flattens to a straight line and
// stops there, and its bottom drops deeper the more it opens. A frown opens
// the other way up: the bottom flattens and the top rises.
function lips(v, o) {
  const q = 110 + idiv(4 * v, 5), gap = Math.round(o * 0.4), t = Math.min(1, o / 100);
  const [u, l] = q >= 150
    ? [Math.round(q + (150 - q) * t), q + gap]
    : [q - gap, Math.round(q + (150 - q) * t)];
  return [Math.min(u, l), Math.max(u, l)];
}
function openMouth(v, o) {
  if (o === 0) return mouthPath(v);
  const [upper, lower] = lips(v, o);
  return `M60 150 Q100 ${lower} 140 150 Q100 ${upper} 60 150 Z`;
}
const browPath = v => `M55 78 Q100 ${50 + idiv(2 * v, 5)} 145 78`;

// THE NOSE: lines between the eyes and the mouth, from the bridge's top at
// (100, 106) down, in a box of nose width x nose height (30 x 30 face units
// at 100). Seen from the SIDE it is an L or a < in one of a few curves,
// pointing right (the letter L) or left (a backwards L); from the FRONT,
// both sides of the bridge, the nostrils, and the tip between them. Paths
// are in nose units: x across (+ = the way it points), y down, 1 = the
// box's height.
const NOSES = {
  // tilted so the bottom, where the nostril would be, sits near the middle
  L:     "M0.16 0 L-0.1 1 L0.16 1",
  angle: "M0 0 L0.55 0.72 L0.08 1",
  soft:  "M0.22 0 Q-0.12 0.9 0.02 1 Q0.24 1.04 0.26 0.82",
  hook:  "M0 0 Q0.8 0.45 0.55 0.95 Q0.35 1.08 0.12 0.9",     // "bulbous"
  // a witch's: long, out to the side, hooked down at the tip (and warts:
  // NOSE_WARTS)
  witch: "M0.06 0 Q0.85 0.3 1.45 0.86 Q1.3 1 1 0.96 Q0.55 0.93 0.22 1.02 Q0.06 1.06 0.04 0.96",
};
// a witch nose's warts: where, in the nose's units, and how big (wartR)
const NOSE_WARTS = [[0.6, 0.5, 0.16], [1.12, 0.84, 0.12], [0.36, 0.84, 0.11]];
// from the front: one side of the bridge flaring into a nostril that curls
// back up (drawn twice, mirrored), and the tip, drawn once between them
const FRONT_NOSES = {
  pointed: "M0.08 0.05 L0.26 0.72 Q0.42 0.88 0.28 1 Q0.16 1.06 0.07 0.94",
  round:   "M0.1 0.05 Q0.12 0.55 0.24 0.74 Q0.4 0.9 0.26 1.01 Q0.15 1.07 0.07 0.95",
};
const NOSE_TIP = "M-0.07 0.95 Q0 1.1 0.07 0.95";
// The top of the mouth, in face units: its lips are quadratic curves from
// (60, 150) to (140, 150), so a curve's highest point is halfway between
// the corners' y and its control's.
const lipTop = c => Math.min(150, 75 + c / 2) - 2.5;   // (less half the lip's line)
function mouthTopAsDrawn(F) {
  return lipTop(F.open === 0 ? 110 + idiv(4 * F.smile, 5) : lips(F.smile, F.open)[0]);
}
// WHERE THE FEATURES GO, for the head's width and height: a bigger head
// spreads the eyes, brows, nose and mouth apart (by 60% of its stretch) and
// grows the mouth, which has no size control of its own. Then the mouth
// moves down for three things: opening, which grows the mouth downward
// from where its closed top was; the nose, which the mouth's top as drawn
// must clear (a short nose never moves it); and the eyes, which it clears
// by 8 wherever they are above it (big eyes close together, never the
// usual ones). The head's lower half stretches by however far the mouth
// moved (`stretch`).
function faceLayout(F) {
  const kx = 1 + 0.6 * (F.faceW / 100 - 1), ky = 1 + 0.6 * (F.faceH / 100 - 1);
  // The eyes keep the eye gap between them, edge to edge, however big,
  // narrow or slanted they are, and however wide the face: each middle is
  // half the gap and the eye's own half-width from the face's middle
  // line. The default gap, 38, puts the default eyes where they always
  // were, 32 out. The brows go with the eyes.
  const eyeY = HEAD_C + (100 - HEAD_C) * ky, eyeDX = F.eyeGap / 2 + eyeHalfW(F);
  // (but never past the eyes' outer edges, or a wide gap pushed them out
  // of the face: the brow is 45 to a side, a pair 52)
  const bx = Math.min(eyeDX / 32, Math.max(1, (eyeDX + eyeHalfW(F)) / (F.unibrow ? 45 : 52)));
  const noseTop = eyeY + 6;
  const nose = F.noseView && F.noseView !== "none";
  const uh = 30 * F.noseH / 100, uw = 30 * F.noseW / 100;
  const mouthY0 = HEAD_C + (150 - HEAD_C) * ky;
  // The mouth's own scale: the face's stretch times the mouth size, the
  // same across and down, about the line through its corners.
  const ms = (F.mouthSize ?? 100) / 100, mx = kx * ms, my = ky * ms;
  // a frown's top rises as it opens: keep it where the closed mouth's was
  const riseDrop = F.open === 0 ? 0
    : Math.max(0, lipTop(110 + idiv(4 * F.smile, 5)) - mouthTopAsDrawn(F)) * my;
  // then clear the nose, measured to the mouth's top as drawn (a frown's
  // arch, an open mouth's upper lip), not to a flat mouth's line
  const top = mouthY0 + riseDrop + (mouthTopAsDrawn(F) - 150) * my;
  // (a witch's nose pointing straight out of the face hangs down its whole
  // length, 1.3 of the nose's height at the usual length)
  const hang = F.noseView === "witch" && F.witchDir === "center" ? 1.3 * (F.witchLen ?? 100) / 100 : 1.1;
  const noseDrop = nose ? Math.max(0, noseTop + hang * uh + 6 - top) : 0;
  // the eyes: along the mouth's top curve as drawn (a smile's corners, a
  // frown's middle, an open mouth's upper lip), it keeps 8 below each
  // eye's lower edge, found right above each point of the curve. A plain
  // eye's edge is an oval, its extent (eyeHalfW, eyeHalfH) grown by 8. A
  // glyph or an x is a box, slanted with the eye, since their points reach
  // its corners: an x's strokes, and a star's or heart's ink, which SVG
  // text draws lower than its middle (measured, in Firefox: 1.67 radii to
  // either side, 1.22 above, 1.97 below).
  const c = F.open === 0 ? 110 + idiv(4 * F.smile, 5) : lips(F.smile, F.open)[0];
  const er = 6 + idiv(F.eyes, 7), th = -F.tilt * Math.PI / 180;   // the right eye's slant
  const corners = F.xeyes ? [[-er, -er], [er, -er], [er, er], [-er, er]]
    : F.stars || F.hearts ? [[-1.67 * er, -1.22 * er], [1.67 * er, -1.22 * er], [1.67 * er, 1.97 * er], [-1.67 * er, 1.97 * er]]
    : null;
  const pad = 8 + (F.xeyes ? (3 + idiv(er, 6)) / 2 : 0);
  const box = corners && corners.map(([x, y]) => [x * Math.cos(th) - y * Math.sin(th), x * Math.sin(th) + y * Math.cos(th)]);
  const ew = eyeHalfW(F) + 8, eh = eyeHalfH(F) + 8;
  // (either eye: they are mirror images about the middle line)
  const below = x => {
    const ax = Math.abs(x) - eyeDX;
    if (box) return ax >= Math.min(...box.map(p => p[0])) - pad && ax <= Math.max(...box.map(p => p[0])) + pad
      ? eyeY + Math.max(...box.map(p => p[1])) + pad : -Infinity;
    const e = ax / ew;
    return Math.abs(e) < 1 ? eyeY + eh * Math.sqrt(1 - e * e) : -Infinity;
  };
  let eyeDrop = 0;
  for (let i = 0; i <= 40; i++) {
    const u = i / 40, x = 40 * mx * (2 * u - 1);     // from the face's middle line
    const lip = mouthY0 + riseDrop + (2 * u * (1 - u) * (c - 150) - 2.5) * my;
    eyeDrop = Math.max(eyeDrop, below(x) - lip);
  }
  const drop = Math.max(noseDrop, eyeDrop);
  const mouthY = mouthY0 + drop + riseDrop;
  // and the chin makes room below a deep mouth
  const bottom = mouthY + (75 + lips(F.smile, F.open)[1] / 2 + 2.5 - 150) * my;
  const chin = HEAD_C + 80 * (F.faceH / 100) * (1 + 0.25 * F.alien / 100);
  const stretch = Math.round(Math.max(drop + riseDrop, bottom + 12 - chin, 0));
  return { kx, ky, mx, my, bx, eyeY, eyeDX, noseTop, uh, uw, mouthY, stretch };
}
const mouthDrop = F => faceLayout(F).stretch;
// A wart's radius: r of the usual nose's height (30), shrunk a little, and
// grown with the nose -- its height, width and length together.
const wartR = (r, uh, uw, len) => Math.max(1.5, r * 30 * 0.7 * Math.sqrt(uh * uw * len) / 30);
function noseSvg(F) {
  let view = F.noseView;
  if (!view || view === "none") return "";
  // a witch's nose: out to the left or right, its length times the
  // length slider, or (C) straight out of the face, hanging down
  const witch = view === "witch" || ((view === "left" || view === "right") && F.nose === "witch");
  const len = view === "witch" ? (F.witchLen ?? 100) / 100 : 1;
  if (view === "witch") {
    if ((F.witchDir ?? "right") === "center") return witchNoseFront(F, len);
    view = F.witchDir;
  }
  const front = view === "front";
  const shape = front ? FRONT_NOSES[F.noseFront] || FRONT_NOSES.round : witch ? NOSES.witch : NOSES[F.nose] || NOSES.L;
  // as tall as asked: the mouth makes room (faceLayout)
  const { noseTop, uh, uw } = faceLayout(F);
  const sw = f1(3 + (F.noseH + F.noseW) / 200);
  // from the front, width spreads the nostrils (the bottom) much more than
  // the bridge (the top), for a broad nose
  const wAt = y => front ? 30 + (uw * 1.7 - 30) * Math.max(0, Math.min(1, y)) : uw;
  // A side nose widens from the top of its bridge, which stays between the
  // eyes where the usual width (30) puts it, not off under one eye. (From
  // the front it is mirrored, so centred.)
  const top = Number(shape.match(/-?[\d.]+/)[0]);
  const X = (sx, x, y) => 100 + sx * (front ? wAt(y) * x : top * 30 + (x - top) * uw * len);
  // (a witch's nose is filled, a shade darker than the skin: a nose, not
  // a line)
  const fill = witch ? mix(F.color, "#000000", 0.14) : "none";
  const at = (sx, d) => `<path d="${d.replace(/(-?[\d.]+) (-?[\d.]+)/g, (_, x, y) =>
      f1(X(sx, Number(x), Number(y))) + " " + f1(noseTop + uh * Number(y)))}" ` +
    `fill="${fill}" stroke="black" stroke-width="${sw}" stroke-linecap="round" stroke-linejoin="round"/>`;
  if (front) return at(1, shape) + at(-1, shape) + at(1, NOSE_TIP);
  const sx = view === "left" ? -1 : 1;
  return at(sx, shape) + (witch
    ? NOSE_WARTS.map(([x, y, r]) => `<circle cx="${f1(X(sx, x, y))}" cy="${f1(noseTop + uh * y)}" r="${f1(wartR(r, uh, uw, len))}" ` +
                                     `fill="${darker(F.color)}" stroke="black" stroke-width="1.5"/>`).join("")
    : "");
}
// A witch's nose seen straight on: from a narrow bridge it widens down to
// a hooked point, its length times `len`, nostrils either side of the
// point, warts on it. Filled a shade darker than the skin.
function witchNoseFront(F, len) {
  const { noseTop, uh, uw } = faceLayout(F);
  const sw = f1(3 + (F.noseH + F.noseW) / 200), L = 1.3 * len;
  const P = (x, y) => `${f1(100 + x * uw)} ${f1(noseTop + y * uh)}`;
  const d = `M${P(-0.12, 0)} Q${P(-0.26, L * 0.55)} ${P(-0.64, L * 0.86)} Q${P(-0.46, L * 1.03)} ${P(0, L)} ` +
            `Q${P(0.46, L * 1.03)} ${P(0.64, L * 0.86)} Q${P(0.26, L * 0.55)} ${P(0.12, 0)}`;
  return `<path d="${d}" fill="${mix(F.color, "#000000", 0.14)}" stroke="black" stroke-width="${sw}" stroke-linejoin="round" stroke-linecap="round"/>` +
    [-1, 1].map(sd => `<ellipse cx="${f1(100 + sd * 0.3 * uw)}" cy="${f1(noseTop + L * 0.9 * uh)}" rx="${f1(0.09 * uw + 1)}" ry="${f1(0.05 * uh + 1)}" fill="#111"/>`).join("") +
    [[0.14, 0.45, 0.13], [-0.3, 0.74, 0.11]].map(([x, y, r]) =>
      `<circle cx="${f1(100 + x * uw)}" cy="${f1(noseTop + y * L * uh)}" r="${f1(wartR(r, uh, uw, len))}" fill="${darker(F.color)}" stroke="black" stroke-width="1.5"/>`).join("");
}
function browPair(v) {
  const inner = 66 + idiv(v * 24, 100), top = Math.min(inner, 80) - 8;
  return `M48 80 Q68 ${top} 88 ${inner} M152 80 Q132 ${top} 112 ${inner}`;
}
const CRACK = "M38 60 L52 52 L66 63 L80 51 L94 63 L108 51 L122 63 L136 51 L150 62 L162 60";
const CONFETTI = ["deeppink", "orange", "dodgerblue", "limegreen", "gold", "mediumpurple", "red"];

function pieces(shown, burst, crackY = 60) {
  let s = "";
  for (let i = 0; i < 40 && i < shown; i++) {
    const x = 100 + idiv((mod(i * 53, 190) - 95) * burst, 100);
    const y = Math.round(crackY) - idiv((15 + mod(i * 29, 125)) * burst, 100);
    const spin = idiv(mod(i * 67, 360) * burst, 100);
    const kind = mod(i, 5);
    s += `<g transform="translate(${x} ${y}) rotate(${spin})">` + (kind < 3
      ? `<text font-size="${16 + mod(i * 7, 11)}" text-anchor="middle" dominant-baseline="middle">${["🍬", "🍭", "🍫"][kind]}</text>`
      : `<rect x="-6" y="-3" width="12" height="6" fill="${CONFETTI[mod(i, 7)]}"/>`) + "</g>";
  }
  return s;
}
// `angle` slants the eye about its middle (the eye angle, mirrored for
// the right eye by the caller).
// How far up the eyebrows go to clear the eyes: nothing at the usual
// sizes, and as far as big, tall or slanted eyes need -- above the head,
// cartoon style, if it comes to that. The eyes' top is found from the
// same sizes the eye is drawn with.
function browLift(F) {
  const r = 6 + idiv(F.eyes, 7);
  let reach;
  if (F.xeyes) reach = r;
  else if (F.stars || F.hearts) reach = r * 1.6;
  else {
    const irisR = F.iris > 0 ? r + F.iris * 0.25 : 0;
    const whiteR = irisR > 0 && F.whites > 0 ? irisR + F.whites * 0.25 : 0;
    const R = whiteR || irisR || r, k = 1 - 0.88 * F.oval / 100, t = F.tilt * Math.PI / 180;
    reach = Math.hypot(R * Math.sin(t), R * k * Math.cos(t));
  }
  // the brows' ends sit at y = 78; keep them 8 above the eyes' top
  return Math.max(0, reach - 14);
}
// How far one eye reaches to either side of its middle, as drawn: a plain
// eye's outermost ring (eye white, iris or pupil), squashed and slanted
// as eye() draws it; a star's or heart's glyph; an x's two strokes.
function eyeHalfW(F) {
  const r = 6 + idiv(F.eyes, 7), t = F.tilt * Math.PI / 180;
  if (F.xeyes) return r * (Math.abs(Math.cos(t)) + Math.abs(Math.sin(t)));
  if (F.stars || F.hearts) return r * (F.stars && F.hearts ? 1.75 : 1.5);
  const irisR = F.iris > 0 ? r + F.iris * 0.25 : 0;
  const whiteR = irisR > 0 && F.whites > 0 ? irisR + F.whites * 0.25 : 0;
  const R = whiteR || irisR || r, k = 1 - 0.88 * F.oval / 100;
  return Math.hypot(R * Math.cos(t), R * k * Math.sin(t));
}
// ...and how far above and below it.
function eyeHalfH(F) {
  const r = 6 + idiv(F.eyes, 7), t = F.tilt * Math.PI / 180;
  if (F.xeyes) return r * (Math.abs(Math.cos(t)) + Math.abs(Math.sin(t)));
  if (F.stars || F.hearts) return r * 1.5;
  const irisR = F.iris > 0 ? r + F.iris * 0.25 : 0;
  const whiteR = irisR > 0 && F.whites > 0 ? irisR + F.whites * 0.25 : 0;
  const R = whiteR || irisR || r, k = 1 - 0.88 * F.oval / 100;
  return Math.hypot(R * Math.sin(t), R * k * Math.cos(t));
}
function eye(x, size, stars, hearts, xs, F, angle = 0) {
  const r = 6 + idiv(size, 7);
  const turned = inner => `<g transform="rotate(${f1(angle)} ${x} 100)">${inner}</g>`;
  const glyph = (g, gx, gy, gs) =>
    `<text x="${gx}" y="${gy}" font-size="${gs}" text-anchor="middle" dominant-baseline="central">${g}</text>`;
  // (ticket 129) closed: a lid's line, curved down as a calm face's (😌),
  // as wide as the eye would be open; before any other kind of eye
  if (F.closed) {
    const R = F.iris > 0 ? r + F.iris * 0.25 + (F.whites > 0 ? F.whites * 0.25 : 0) : r;
    return turned(`<path d="M${f1(x - R)} 100 Q${x} ${f1(100 + R * 0.8)} ${f1(x + R)} 100" fill="none" stroke="black" stroke-width="${f1(3 + R / 7)}" stroke-linecap="round"/>`);
  }
  if (xs) return turned(`<path d="M${x - r} ${100 - r} L${x + r} ${100 + r} M${x - r} ${100 + r} L${x + r} ${100 - r}" ` +
                 `stroke="black" stroke-width="${3 + idiv(r, 6)}" stroke-linecap="round"/>`);
  if (hearts) return turned(glyph("❤️", x, 100, r * 3) + (stars ? glyph("⭐", x + r, 100 - r, idiv(r * 3, 2)) : ""));
  if (stars) return turned(glyph("⭐", x, 100, r * 3));
  // A plain eye, as the eye emoji draws one: an eye white, around an
  // iris, around the pupil -- the white and iris only when chosen -- and
  // a shine, a small dot on the side the light comes from, colored
  // somewhere between the pupil's color and the shine color.
  const irisR = F.iris > 0 ? r + F.iris * 0.25 : 0;
  const whiteR = irisR > 0 && F.whites > 0 ? irisR + F.whites * 0.25 : 0;
  const outer = irisR || r;
  const dot = mix(F.pupil, F.shine, F.shineMix / 100);
  const dotR = r * F.shineSize / 150;
  // eye shape: 0 round, 100 a very narrow oval -- the whole eye squashed
  // top to bottom about its middle, then slanted
  const k = 1 - 0.88 * F.oval / 100;
  // The shine is placed in the picture's own frame, toward the light, so
  // both eyes shine from the same side however they are slanted or
  // squashed: the light's offset is taken into the eye's own (slanted,
  // unsquashed) frame, kept within half the eye, and brought back out.
  const t = angle * Math.PI / 180, cs = Math.cos(t), sn = Math.sin(t);
  const wx = F.lightX * outer * 0.5, wy = F.lightY * outer * 0.5;
  let ux = cs * wx + sn * wy, uy = (-sn * wx + cs * wy) / k;
  const len = Math.hypot(ux, uy), most = outer * 0.55;
  if (len > most) { ux *= most / len; uy *= most / len; }
  const dx = cs * ux - sn * uy * k, dy = sn * ux + cs * uy * k;
  // (kept within 45% of the eye up to 150; past that, up to 450, the cap
  // grows with it, to three times)
  const shineR = Math.min(dotR, outer * k * 0.45 * Math.max(1, F.shineSize / 150));
  return `<g transform="rotate(${f1(angle)} ${x} 100) translate(${x} 100) scale(1 ${k.toFixed(3)}) translate(${-x} -100)">` +
    (whiteR ? `<circle cx="${x}" cy="100" r="${f1(whiteR)}" fill="#ffffff" stroke="black" stroke-width="2"/>` : "") +
    (irisR ? `<circle cx="${x}" cy="100" r="${f1(irisR)}" fill="${F.irisColor}"/>` : "") +
    `<circle cx="${x}" cy="100" r="${r}" fill="${F.pupil}"/>` + "</g>" +
    (shineR > 0.3 ? `<circle cx="${f1(x + dx)}" cy="${f1(100 + dy)}" r="${f1(shineR)}" fill="${dot}"/>` : "");
}
function ray(cx, cy, r0, len, width, color, dx, dy) {
  if (len <= 0) return "";
  return `<line x1="${cx + idiv(r0 * dx, 100)}" y1="${cy + idiv(r0 * dy, 100)}" ` +
         `x2="${cx + idiv((r0 + len) * dx, 100)}" y2="${cy + idiv((r0 + len) * dy, 100)}" ` +
         `stroke="${color}" stroke-width="${width}" stroke-linecap="round"/>`;
}
const FAN = [[100, 100], [94, 134], [94, 66], [77, 164], [77, 36]];
function sideLines(v) {
  const shown = v === 0 ? 0 : 1 + idiv(v * 4, 100);
  const len = 8 + idiv(v * 35, 100), width = 2 + idiv(v, 25);
  const color = ["#999999", "#666666", "#333333", "black", "black"][Math.min(idiv(v, 26), 4)];
  return FAN.slice(0, shown).map(([dx, oy]) =>
    ray(100, 111, 86, len, width, color, dx, oy - 100) +
    ray(100, 111, 86, len, width, color, -dx, oy - 100)).join("");
}
const UP_FAN = [[100, 100], [134, 94], [66, 94], [164, 77], [36, 77], [187, 50], [13, 50]];
const raysUp = (len, width, crackY = 60) =>
  UP_FAN.map(([ox, up]) => ray(100, Math.round(crackY), 18, len, width, "orange", ox - 100, -up)).join("");
function teeth(n, upper, lower) {
  let s = "";
  for (let j = 1; j <= n; j++) {
    const d = n + 1, x = 60 + idiv(80 * j, d), up = j % 2 === 1;
    const q = up ? lower : upper;
    const y = 150 + idiv(2 * j * (d - j) * (q - 150), d * d);
    const tip = up ? y - 10 : y + 10;
    s += `<polygon points="${x - 6},${y} ${x + 6},${y} ${x},${tip}" fill="white" stroke="black" stroke-width="2"/>`;
  }
  return s;
}
// The inside of an open mouth: a dark hole at the back, the tongue lying
// in the bottom, and teeth -- plain white bars along the inside of the
// top and bottom lips, or the spikes -- then the lips' outline over them.
// A tongue can also stick out, over the lower lip (or a closed mouth).
// All in the mouth's units: corners at (60, 150) and (140, 150).
function mouthSvg(F, o, upper, lower) {
  const path = openMouth(F.smile, o), tc = F.tongueColor || "#e75480";
  const outline = `<path d="${path}" stroke="black" fill="none" stroke-width="5" stroke-linejoin="round"/>`;
  // the lips' middles (a quadratic's midpoint is halfway to its control)
  const top = o === 0 ? 75 + (110 + idiv(4 * F.smile, 5)) / 2 : 75 + upper / 2;
  const bottom = o === 0 ? top : 75 + lower / 2, depth = bottom - top;
  const out = () => {
    const w = 26 * (F.tongueW ?? 100) / 100, len = 30 * (F.tongueLen ?? 100) / 100, r = w / 2;
    const y0 = o === 0 ? top - 2 : Math.max(top + 2, bottom - depth * 0.4), y1 = bottom + len;
    return `<path d="M${f1(100 - r)} ${f1(y0)} L${f1(100 - r)} ${f1(y1)} A${f1(r)} ${f1(r)} 0 0 0 ${f1(100 + r)} ${f1(y1)} ` +
           `L${f1(100 + r)} ${f1(y0)} Z" fill="${tc}" stroke="${darker(tc)}" stroke-width="2"/>` +
           `<path d="M100 ${f1(y0 + 4)} L100 ${f1(y0 + (y1 + r - y0) * 0.7)}" stroke="${darker(tc)}" stroke-width="2" stroke-linecap="round"/>`;
  };
  if (o === 0) return F.tongue === "out" ? out() + outline : outline;
  let s = `<defs><clipPath id="mouth-in"><path d="${path}"/></clipPath></defs>` +
          `<path d="${path}" fill="#5c1a1a"/><g clip-path="url(#mouth-in)">`;
  if (depth > 4)
    s += `<ellipse cx="100" cy="${f1(top + depth * 0.42)}" rx="22" ry="${f1(depth * 0.3)}" fill="#160606"/>`;
  if (F.tongue !== "none")
    s += `<ellipse cx="100" cy="${f1(bottom - depth * 0.06)}" rx="27" ry="${f1(Math.max(3, depth * 0.34))}" ` +
         `fill="${tc}" stroke="${darker(tc)}" stroke-width="1.5"/>`;
  if ((F.teethStyle || "normal") === "normal") {
    // a strip across the mouth just inside each lip, cut to the mouth's
    // shape: wide along a flat lip, short at a curved one's deepest point
    const t = Math.max(3, Math.min(10, depth * 0.2));
    s += `<rect x="55" y="${f1(top - 3)}" width="90" height="${f1(t + 3)}" fill="white"/>` +
         `<rect x="55" y="${f1(bottom - t * 0.85)}" width="90" height="${f1(t * 0.85 + 3)}" fill="white"/>`;
  }
  s += `</g>` + outline;
  if (F.teethStyle === "spikes") s += teeth(F.teeth, upper, lower);
  if (F.tongue === "out") s += out();
  return s;
}
const SICK = ["yellowgreen", "#c8c84a", "olivedrab", "#b5c22e"];
const BUMPS = [[8, 15, 7], [20, 40, 8], [30, 62, 8], [40, 88, 9], [50, 96, 8],
               [60, 88, 9], [70, 62, 8], [80, 40, 8], [92, 15, 7], [25, 12, 6]];
// Sickness, as in Kids' Choice, but the pile lands on the ground the
// friend stands on (`ground`, in face units) rather than under the chin.
// (ticket 131) sx: where the stream falls from and the pile is centred, in
// the mouth's units (100, the middle of the face, facing us); wide: the
// pile's width as a share of facing us (in profile, 0.4: a side view of it)
function sickPile(s, mouthY, ground, sx = 100, wide = 1) {
  if (s <= 0) return "";
  const blob = (x, y, r, i) => `<circle cx="${x}" cy="${y}" r="${r}" fill="${SICK[mod(i, 4)]}" stroke="#6b7a1f" stroke-width="1"/>`;
  // past 100 the pile keeps growing, at half speed
  const p = s > 100 ? 100 + idiv(s - 100, 2) : s > 30 ? idiv((s - 30) * 100, 70) : 0;
  // past 100 it spreads wider but no taller, so the friend is not buried
  const rx = (6 + idiv(p * 84, 100)) * 1.6 * wide, ry = (3 + idiv(Math.min(p, 100) * 42, 100)) * 1.6;
  const top = ground - ry;
  const reach = mouthY + idiv((top - mouthY) * Math.min(s, 40), 40);
  const size = 3 + idiv(s, 12);
  let out = `<path d="M${f1(sx)} ${mouthY} Q${f1(sx - 7)} ${idiv(mouthY + reach, 2)} ${f1(sx)} ${reach}" fill="none" stroke="yellowgreen" ` +
            `stroke-width="${4 + idiv(s, 10)}" stroke-linecap="round"/>`;
  if (p > 0) {
    out += `<path d="M${f1(sx - rx)} ${ground} A${f1(rx)} ${ry} 0 0 1 ${f1(sx + rx)} ${ground} Z" fill="#a8b83a" stroke="#6b7a1f" stroke-width="2"/>`;
    BUMPS.forEach(([bx, by, br], i) =>
      out += blob(f1(sx + (bx - 50) * 2 * rx / 100), ground - by * ry / 100, 1 + idiv(br * p, 100) * 1.6, i));
  }
  for (let j = 0; j < 6; j++)
    out += blob(f1(sx + (j % 2 === 0 ? -4 : 4)), mouthY + idiv((reach - mouthY) * (j + 1), 6), size - mod(j, 3), j);
  if (s >= 100)
    out += `<circle cx="84" cy="${ground - 38}" r="7"/><circle cx="116" cy="${ground - 38}" r="7"/>` +
           `<path d="M82 ${ground - 12} Q100 ${ground - 28} 118 ${ground - 12}" fill="none" stroke="black" stroke-width="4" stroke-linecap="round"/>`;
  return out;
}
