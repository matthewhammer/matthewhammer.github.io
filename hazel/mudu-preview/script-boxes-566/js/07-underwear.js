"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 7 of 26 of the game's script, run in order by index.html.

// ===================== UNDERWEAR AND SWIMSUITS =====================
// Cartoon garments over a plain body shape, in body units (as the shirt
// and pants): the torso is skin from the collar (y = -20) to the waist
// (L), the hips skin down to the legs. Simple shapes, nothing more.

// A bikini's (and a one-piece's) bottom edge, as path commands from the
// right hip to the left: a T, the band's full width across the hips, then
// curved leg holes sweeping in to a narrow strip between the legs.
const tLegHoles = (hw, L) =>
  ` L${f1(hw)} ${f1(L + 9)} Q${f1(hw * 0.3)} ${f1(L + 12)} 9 ${f1(L + 34)}` +
  ` L-9 ${f1(L + 34)} Q${f1(-hw * 0.3)} ${f1(L + 12)} ${f1(-hw)} ${f1(L + 9)}`;
// (ticket 128) fit: on posed or bent legs, the friend's thighs to follow
// (shortLegs facing us; hipsSide and sideLegs in profile), else null
function undiesBottom(U, hw, fx, half, L, LL, fit = null) {
  const f = fillOf("pat-undies", U.color, U.pattern), st = darker(U.color);
  const P = ps => poly(ps, f, st);
  switch (U.style) {
    case "trunks": {           // swim trunks: like short shorts
      const len = Math.max(34, LL * 0.26);
      if (fit && fit.dir) return fit.hipsSide(f, st) + fit.sideLegs(len, half + 2, f, st) + fit.hipsSide(f, null).replace(/x="([^"]+)"/, (_, x) => `x="${f1(Number(x) + 1.5)}"`).replace(/width="([^"]+)"/, (_, w) => `width="${f1(Number(w) - 3)}"`) +
                                    line(-hw - 1, L - 8, hw + 1, L - 8, 4, st);
      if (fit) return P(fit.shortLegs(len, 16, half + 2, L - 12, hw + 1)) + line(-hw - 1, L - 8, hw + 1, L - 8, 4, st);
      return P([[-hw - 1, L - 12], [hw + 1, L - 12], [fx + half + 3, L + len], [fx - half - 1, L + len],
                [0, L + 16], [-(fx - half - 1), L + len], [-(fx + half + 3), L + len]]) +
             line(-hw - 1, L - 8, hw + 1, L - 8, 4, st);
    }
    case "briefs":
      // (posed: the leg holes rise toward the inside, along each thigh)
      if (fit && fit.dir) return fit.hipsSide(f, st, 20);
      if (fit) return P(fit.shortLegs(20, 26, half, L - 8, hw, half * 0.3, 32));
      return P([[-hw, L - 8], [hw, L - 8], [fx + half, L + 20], [fx - half * 0.3, L + 32], [0, L + 26],
                [-(fx - half * 0.3), L + 32], [-(fx + half), L + 20]]);
    case "bikini":
    case "onepiece":           // (a one-piece's bottom is drawn with its top)
      return U.style === "bikini"
        ? `<path d="M${f1(-hw)} ${f1(L - 2)} L${f1(hw)} ${f1(L - 2)}${tLegHoles(hw, L)} Z" fill="${f}" ` +
          `stroke="${st}" stroke-width="2.5" stroke-linejoin="round"/>`
        : "";
  }
  return "";
}

function undiesTop(U, w, nw, hw, fx, L) {
  const f = fillOf("pat-undies", U.color, U.pattern), st = darker(U.color);
  const strap = side => line(side * w * 0.5, 16, side * (nw + 7), -18, 4.5, st) +
                        line(side * w * 0.5, 16, side * (nw + 7), -18, 2, U.color);
  if (U.style === "onepiece") {
    // from a scooped neckline down over the hips, with straps
    const d = `M${f1(-w - 2)} 12 Q0 26 ${f1(w + 2)} 12 L${f1(w + 2)} ${f1(L)} L${f1(hw)} ${f1(L)}` +
              tLegHoles(hw, L) + ` L${f1(-w - 2)} ${f1(L)} Z`;
    return strap(-1) + strap(1) + `<path d="${d}" fill="${f}" stroke="${st}" stroke-width="2.5" stroke-linejoin="round"/>`;
  }
  if (!U.top) return "";
  // a top: two rounded triangles on a band, with straps
  let s = strap(-1) + strap(1);
  for (const side of [-1, 1])
    s += `<path d="M${f1(side * 2)} 36 Q${f1(side * w * 0.45)} 8 ${f1(side * (w + 1))} 36 Z" fill="${f}" ` +
         `stroke="${st}" stroke-width="2.5" stroke-linejoin="round"/>`;
  return s + `<rect x="${f1(-w - 1)}" y="33" width="${f1(2 * w + 2)}" height="8" rx="3" fill="${f}" stroke="${st}" stroke-width="2.5"/>`;
}
