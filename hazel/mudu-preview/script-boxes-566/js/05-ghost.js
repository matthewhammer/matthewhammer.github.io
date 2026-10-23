"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 5 of 26 of the game's script, run in order by index.html.

// ===================== THE GHOST =====================
// A ghost, like the 👻 emoji but the friend's full size: a sheet over all
// of them, domed over the head, falling to a wavy hem just above the feet,
// its sleeves along the arms (as they are held) ending in round nubs; a
// few soft folds. Ragged (0 to 100): the hem's scallops turn into torn
// points of every length, and the edges fray. Its eyes where the friend's
// are, in the friend's own eye style (none, one in the middle, two, or a
// third in the middle of the forehead, like the three-eyed smiley on the
// poster for the movie Evolution), or, hidden, plain black holes as the
// emoji's; an "o" of a mouth. In body units; returns the sheet, the eyes
// and the mouth apart, for the visibility panel's parts.
const GHOST_COLORS = ["#f4f1ea", "#ffffff", "#e5e7eb", "#c4b5fd", "#a7f3d0", "#fde68a", "#fbcfe8", "#1f1f22"];
function ghostSvg(G, { F, tx, ty, s, w, L, feetY, arms, armW }) {
  const col = G.ghostColor || GHOST_COLORS[0], ln = darker(col), rag = Math.max(0, Math.min(1, (G.ghostRagged ?? 30) / 100));
  const rnd = (i, t) => { const v = Math.sin(i * 12.9898 + t * 78.233) * 43758.5453; return v - Math.floor(v); };
  const LY = faceLayout(F), at = (x, y) => [tx + x * s, ty + y * s];
  const [hcx, hcy] = at(100, HEAD_C), R = 80 * s * Math.max(headWidth(F) * (1 + 0.12 * F.alien / 100), headH(F)) + 14;
  const hemY = feetY - 8, half = Math.max(w + 45, R * 1.25), n = rag < 0.35 ? 5 : 4 + Math.round(3 * rag);
  // the outline: up the left side, over the dome, down the right side, then
  // the hem back from right to left in scallops (or torn points)
  const side = (sd, k) => { const pts = []; const top = hcy, bot = hemY - 4;
    for (let i = 0; i <= 8; i++) { const u = i / 8, y = top + (bot - top) * u, x = hcx + sd * (R + (half - R) * Math.pow(u, 1.4)) + sd * rag * 7 * (rnd(i + k, 3) - 0.5) * (u > 0.1 ? 1 : 0);
      pts.push([x, y]); }
    return pts; };
  const left = side(-1, 0).reverse(), right = side(1, 20);
  let d = `M${left.map(([x, y]) => f1(x) + " " + f1(y)).join(" L")} A${f1(R)} ${f1(R)} 0 0 1 ${f1(right[0][0])} ${f1(right[0][1])} L${right.slice(1).map(([x, y]) => f1(x) + " " + f1(y)).join(" L")}`;
  const x0 = right[right.length - 1][0], x1 = left[0][0], step = (x0 - x1) / n;
  for (let i = 1; i <= n; i++) {
    const a = x0 - (i - 1) * step, b = x0 - i * step, m = (a + b) / 2;
    // (smooth: round waves, as the emoji's; ragged: torn points, each its
    // own length and lean, longer the more ragged)
    if (rag < 0.35) d += ` Q${f1(m)} ${f1(hemY + 34 * (1 - rag))} ${f1(b)} ${f1(hemY - 4)}`;
    else { const deep = 20 + 50 * rag * (0.35 + 0.65 * rnd(i, 7)), lean = (rnd(i, 8) - 0.5) * step * 0.5, notch = i < n ? 10 + 22 * rag * rnd(i, 9) : 0;
      d += ` L${f1(m + lean)} ${f1(hemY + deep)} L${f1(b)} ${f1(hemY - 4 - notch)}`; }
  }
  d += " Z";
  // the sleeves, along the arms, behind the body, with round ends
  const sleeves = arms.map(({ A, H }) => `<line x1="${f1(A[0])}" y1="${f1(A[1])}" x2="${f1(H[0])}" y2="${f1(H[1])}" stroke="${ln}" stroke-width="${f1(armW + 30)}" stroke-linecap="round"/>` +
    `<line x1="${f1(A[0])}" y1="${f1(A[1])}" x2="${f1(H[0])}" y2="${f1(H[1])}" stroke="${col}" stroke-width="${f1(armW + 25)}" stroke-linecap="round"/>`).join("");
  // folds: soft lines down the sheet
  const folds = [-0.55, -0.15, 0.3, 0.62].map((u, i) => { const x = hcx + u * half * 0.8;
    return `<path d="M${f1(x)} ${f1(hcy + R * 0.6 + 20 * rnd(i, 5))} Q${f1(x + 14 * (rnd(i, 6) - 0.5))} ${f1((hcy + hemY) / 2)} ${f1(x + u * 18)} ${f1(hemY - 6)}" fill="none" stroke="${ln}" stroke-width="2.5" opacity="0.35" stroke-linecap="round"/>`; }).join("");
  const sheet = sleeves + `<path d="${d}" fill="${col}" stroke="${ln}" stroke-width="3" stroke-linejoin="round"/>` + folds;
  // the eyes: where the friend's are, in the friend's style, or black holes
  const count = Math.max(0, Math.min(3, Number(G.ghostEyes ?? 2))), hide = !!G.ghostHideEyes;
  const spots = count === 0 ? [] : count === 1 ? [[100, LY.eyeY]] : [[100 - LY.eyeDX, LY.eyeY], [100 + LY.eyeDX, LY.eyeY]];
  if (count === 3) spots.push([100, LY.eyeY - 2 * eyeHalfH(F) - 10]);
  const eyes = spots.map(([x, y], i) => hide
    ? `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(Math.max(7, eyeHalfW(F) * 0.7))}" ry="${f1(Math.max(10, eyeHalfH(F) * 1.05))}" fill="#1f1f22"/>`
    : `<g transform="translate(0 ${f1(y - 100)})">${eye(x, F.eyes, F.stars, F.hearts, F.xeyes, F, i === 1 && count > 1 ? -F.tilt : F.tilt)}</g>`).join("");
  const inFace = t => `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})">${t}</g>`;
  const mouth = `<ellipse cx="100" cy="${f1(LY.mouthY - 4)}" rx="14" ry="19" fill="#1f1f22"/>`;
  return { sheet, eyes: inFace(eyes), mouth: inFace(mouth) };
}
