"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 6 of 26 of the game's script, run in order by index.html.

// ===================== COSTUMES =====================
// An animal costume: a hood with the animal's face on its brim, above the
// wearer's own face, and either a full-body suit -- fur, paws, a belly and
// a tail -- or, for the falcon, wings worn on the back and a barred tail.
// Choosing an animal dresses it in its natural colors (see set()).
const ANIMALS = {
  tiger:  { suit: true,  fur: "#e8862a", marks: "#1f1a17", belly: "#f7ead8", eyes: "#e0a800", accent: "#e89aa0", furPattern: "stripes" },
  cat:    { suit: true,  fur: "#1f1f22", marks: "#3a3a40", belly: "#2e2e33", eyes: "#c6d43a", accent: "#f4a0b0", furPattern: "none" },
  lion:   { suit: true,  fur: "#d9a441", marks: "#8a4b1c", belly: "#f1d9a8", eyes: "#c88a1e", accent: "#5a3a2a", furPattern: "none" },
  bear:   { suit: true,  fur: "#6b4226", marks: "#4a2c17", belly: "#b08458", eyes: "#2b1a10", accent: "#1d1410", furPattern: "none" },
  falcon: { suit: false, fur: "#55616d", marks: "#23282e", belly: "#f2ece0", eyes: "#1a1410", accent: "#f2c230", furPattern: "none" },
  raccoon: { suit: true, fur: "#8d9096", marks: "#26262a", belly: "#e4e1da", eyes: "#3b2a1a", accent: "#26262a", furPattern: "none" },
  bobcat: { suit: true,  fur: "#c79a68", marks: "#4b3020", belly: "#f3e6cf", eyes: "#d9a520", accent: "#d98a8a", furPattern: "spots" },
  leopard: { suit: true, fur: "#e3b04b", marks: "#2b1d12", belly: "#f7ecd3", eyes: "#9bbf3a", accent: "#c97a7a", furPattern: "rosettes", spot: "auto" },
  eagle:  { suit: false, fur: "#4a3222", marks: "#2a1a10", belly: "#ffffff", eyes: "#f2c230", accent: "#f2b705", furPattern: "none" },
  // a black jaguar: nearly black, its rosettes just showing
  jaguar: { suit: true, fur: "#1e1c22", marks: "#3d3944", belly: "#2b2830", eyes: "#d4c23a", accent: "#3a363f", furPattern: "rosettes", spot: "auto" },
  cheetah: { suit: true, fur: "#e8b54a", marks: "#1c1410", belly: "#f8eed8", eyes: "#c88a1e", accent: "#2a1d14", furPattern: "spots" },
  // a grey wolf: tall pointed ears, a bushy tail with a dark tip, claws
  wolf: { suit: true, fur: "#8b8f97", marks: "#3f4249", belly: "#e8e6e1", eyes: "#e0a020", accent: "#1d1d1f", furPattern: "none" },
  // a deer, spotted like a fawn: antlers, long ears, a short white tail,
  // hooves
  deer: { suit: true, fur: "#a8682f", marks: "#fbf3e4", belly: "#f3e3c3", eyes: "#3b2414", accent: "#2a1a10", furPattern: "spots" },
  // (ticket 499) a dragon: scales, horns, spines, teeth, slit eyes, nostrils, a plated belly, claws, bat wings and a long
  // spiked tail (its own colours are dragonScales, dragonBelly, dragonWings and dragonHorns: dragonLook, below)
  dragon: { suit: true, fur: "#3f8f4a", marks: "#d0453a", belly: "#e9d27a", eyes: "#f2b705", accent: "#f4ecd6", furPattern: "none" },
  // (ticket 530) a Sasquatch: big shaggy fur, a tan muzzle with a big friendly grin, small round ears, big feet (its own
  // colours are its style's: sasquatchLook, below)
  sasquatch: { suit: true, fur: "#7a4e2d", marks: "#55361f", belly: "#d9b38c", eyes: "#3b2414", accent: "#d9b38c", furPattern: "shag" },
  // (ticket 544) a giraffe: brown patches on cream all over, a long neck up from the hood with the giraffe's own head on top
  // (horns, ears, a soft muzzle), a short mane, a tufted tail, hooves (its own colours are its style's: giraffeLook, below)
  giraffe: { suit: true, fur: "#b5652b", marks: "#6e3514", belly: "#f6e7c8", eyes: "#2b1a10", accent: "#7a4420", furPattern: "giraffe" },
  // a whole falcon suit: feathers all over, wings on the arms, talons
  falconsuit: { suit: true, fur: "#55616d", marks: "#23282e", belly: "#f2ece0", eyes: "#1a1410", accent: "#f2c230", furPattern: "none" },
  // and a bald eagle's: dark brown feathers, white head and tail
  eaglesuit: { suit: true, fur: "#4a3222", marks: "#2a1a10", belly: "#ffffff", eyes: "#f2c230", accent: "#f2b705", furPattern: "none" },
  // (ticket 536) a moth: a fuzzy suit, broad soft wings on the arms as a bird suit's are, feathery antennae, a fluffy collar
  // (its colours are the luna moth's body; choosing a style puts on that style's: MOTH_STYLES, js/01-state.js)
  moth: { suit: true, fur: "#f4f1e6", marks: "#c9b98f", belly: "#ffffff", eyes: "#2a2233", accent: "#e7b8cb", furPattern: "none" },
};
// cats (pointed ears, a triangle nose) and birds (no suit: wings and a tail)
const FELINE = { cat: 1, tiger: 1, bobcat: 1, leopard: 1, cheetah: 1, jaguar: 1 };
const BIRDS = { falcon: 1, eagle: 1, falconsuit: 1, eaglesuit: 1 };
// the bird suits: feathers all over, wings on the arms, talons
const BIRD_SUIT = { falconsuit: 1, eaglesuit: 1 };
// the bald eagle's head (hat or suit): white, with its beak
const eagleHead = kind => kind === "eagle" || kind === "eaglesuit";
const isBird = kind => !!BIRDS[kind];
// the falcon's head (hat or suit): its hood, moustache and face
const falconHead = kind => kind === "falcon" || kind === "falconsuit";
const lookOf = kind => { const { suit, ...look } = ANIMALS[kind]; return look; };
// (ticket 499) the dragon's look, from its own four colours (a chooser each, in 🐯 Costume's 🐉 the dragon): its scales are the
// suit's fur (the paws' pads and the scales' lines a shade darker), its belly, its wings and spines (wing), and its horns,
// teeth and claws (horn); its eyes are the animal eye colour, as any animal's
const dragonLook = c => { const S = c.dragonScales || "#3f8f4a", dk = mix(S, "#000000", 0.3);
  return { ...c, fur: S, marks: dk, accent: dk, belly: c.dragonBelly || "#e9d27a", wing: c.dragonWings || "#d0453a", horn: c.dragonHorns || "#f4ecd6", furPattern: "none" }; };
// (ticket 530) the Sasquatch's look, from its style (SASQUATCH_STYLES, js/01-state.js) and that style's colour: its fur the
// colour, its tufts a shade darker (the yeti's pale blue), its muzzle, belly and pads the style's face colour (or the fur
// lightened), its eyes the animal eye colour; and the style itself (S), for the drawing's extras
const sasquatchStyleOf = c => SASQUATCH_STYLES[c.sasquatchStyle] ? c.sasquatchStyle : "bigfoot";
const sasquatchLook = c => { const id = sasquatchStyleOf(c), S = SASQUATCH_STYLES[id], fur = c["sasquatchColor_" + id] || S.colors[0];
  const face = S.face || mix(fur, "#ffffff", 0.45);
  return { ...c, fur, marks: S.marks || mix(fur, "#000000", 0.3), belly: face, accent: mix(face, "#000000", 0.25), furPattern: "shag", sq: S, sqId: id }; };
// (ticket 544) the giraffe's look, from its style (GIRAFFE_STYLES, js/01-state.js) and that style's colour: its fur the
// patches' colour (the outlines a shade darker, as any suit's), the cream between them its belly, its mane (marks) the
// style's, its eyes the animal eye colour; and the style itself (gf), for the coat and the drawing
const giraffeStyleOf = c => GIRAFFE_STYLES[c.giraffeStyle] ? c.giraffeStyle : "reticulated";
const giraffeLook = c => { const id = giraffeStyleOf(c), S = GIRAFFE_STYLES[id], fur = c["giraffeColor_" + id] || S.colors[0];
  return { ...c, fur, marks: S.mane, belly: S.ground, accent: mix(fur, "#000000", 0.3), furPattern: "giraffe", gf: S, gfId: id }; };
const costumeOn = () => (ANIMALS[state.costume.kind] ? (state.costume.kind === "dragon" ? dragonLook(state.costume) : state.costume.kind === "sasquatch" ? sasquatchLook(state.costume) : state.costume.kind === "giraffe" ? giraffeLook(state.costume) : state.costume) : null);
// (ticket 433, Matthew: "monk/dryad costume with hooded cloak, and a staff (optional, like how the witch has an optional
// broom)") A CLOTH HOOD, its colour, while one is up (the monk's or the dryad's cloak): over the hair, framing the face. In the
// round head's units (stretched with the head, as hair is), on the animal hood's frame (hoodGeom): its back, behind the head,
// a cowl with a peak at the top and its drape down onto the shoulders; its front, the same shape with the face's opening cut
// out (from the forehead, at 64, to below the chin; in profile toward the face), drawn over the face but under its features;
// from behind, closed over the head.
const clothHood = () => state.costume.kind === "monk" && state.costume.hoodUp !== false ? (state.costume.monkCloak || "#6b4a2b") : null;
// (ticket 435) the knight's helmet: the trap room's great helm, worn (helmetSvg in js/13-castle-rooms-and-map.js)
// The knight's arms, as a tabard, a shield and the emblem show them: its two colours laid out (quartered, halves, plain) in a
// shape, clipped to it, and the emblem in its middle; HERALDRY's emblems in a unit box about (0, 0), 30 across.
const EMBLEMS = {
  none: () => "",
  cross: c => `<path d="M-4 -15 H4 V-4 H15 V4 H4 V15 H-4 V4 H-15 V-4 H-4 Z" fill="${c}"/>`,
  chevron: c => `<path d="M-16 8 L0 -8 L16 8 L16 16 L0 0 L-16 16 Z" fill="${c}"/>`,
  falcon: c => `<path d="M0 -6 Q-8 -14 -18 -10 Q-12 -6 -10 0 Q-6 2 -3 0 L-5 12 L0 8 L5 12 L3 0 Q6 2 10 0 Q12 -6 18 -10 Q8 -14 0 -6 Z" fill="${c}"/><circle cx="0" cy="-9" r="3" fill="${c}"/>`,
  crown: c => `<path d="M-14 10 L-14 -6 L-7 2 L0 -12 L7 2 L14 -6 L14 10 Z" fill="${c}"/>`,
};
// (the fields only as big as the shape, about its middle [cx, cy] and `half` out: a clip does not shrink what is measured)
function armsFill(id, shape, WC, mirror = false, emblemAt = null, k = 1, mid = emblemAt || [0, 0], half = 160) {
  const A = WC.knightA || "#2f6db5", B = WC.knightB || "#f4f1ea", lay = WC.tabardLayout || "quartered";
  const [l, r] = mirror ? [B, A] : [A, B], [cx, cy] = mid, x0 = cx - half, y0 = cy - half, H = 2 * half;
  const R = (x, y, w, h, c) => `<rect x="${f1(x)}" y="${f1(y)}" width="${f1(w)}" height="${f1(h)}" fill="${c}"/>`;
  const fields = lay === "plain" ? R(x0, y0, H, H, A)
    : lay === "halves" ? R(x0, y0, half, H, l) + R(cx, y0, half, H, r)
    : R(x0, y0, half, half, l) + R(cx, y0, half, half, r) + R(x0, cy, half, half, r) + R(cx, cy, half, half, l);
  const em = emblemAt ? armsEmblem(WC, emblemAt, k) : "";
  return `<clipPath id="${id}"><path d="${shape}"/></clipPath><g clip-path="url(#${id})">${fields}</g>${em}<path d="${shape}" fill="none" stroke="#3b3b44" stroke-width="2.5" stroke-linejoin="round"/>`;
}
// the emblem alone, on its round, at `at`, k its size (ticket 498: armsFill's, and on its own where a cloth shows it twice, as
// the horse's caparison does on its two haunches seen from behind)
function armsEmblem(WC, at, k = 1) {
  if (!EMBLEMS[WC.emblem] || WC.emblem === "none") return "";
  const A = WC.knightA || "#2f6db5", B = WC.knightB || "#f4f1ea", lay = WC.tabardLayout || "quartered";
  return `<g transform="translate(${f1(at[0])} ${f1(at[1])}) scale(${k.toFixed(3)})"><circle r="19" fill="${lay === "plain" ? B : "#e0b030"}" opacity="0.95"/>${EMBLEMS[WC.emblem](lay === "plain" ? A : "#7a1f2b")}</g>`;
}
// (ticket 447, Matthew: "the monk/druid hood doesn't read that way in L and R poses --- due to the placement of the point -- it
// should come out like a larger triangle, sticking away from the face, not in the middle of the crown, on top") In profile its
// peak is a larger triangle out behind the head, pointing away from the face, the cowl round the crown and down the back.
function clothHoodPath(F, dir = 0) {
  const { cy, ry } = hoodGeom(F), top = cy - ry, bot = cy + ry, b = dir ? -dir * 14 : 0, p = dir ? -dir * 10 : 0;
  // (ticket 448, Matthew: "I really like the new facing away view of the hood. Let's mimic that for L and R, but adjusted for
  // that viewpoint --- now the peak hangs down behind our head"; "it doesn't 'stick out' at all") In profile the hood is round
  // over the head, and its peak a flap hanging down behind it (clothHoodFlap), as from behind.
  // (ticket 449, Matthew: "the hood should be more like the long hair (default hair), in that it falls down over the shoulders,
  // but with a rounded shape in front facing view, near the neck area (like a beard, sort of)"; "less 'bell shaped' at the
  // bottom of the hood"; "more like how heavy fabric would fall against the body, not outward") Its sides fall straight
  // down onto the shoulders, as long hair does (to about 268), and in front of the neck it rounds under the chin.
  if (dir) { const X = d => f1(100 + dir * d);
    return `M${X(112)} ${f1(cy)} Q${X(116)} ${f1(top + 4)} ${X(40)} ${f1(top - 8)} Q${X(-40)} ${f1(top - 14)} ${X(-96)} ${f1(top + 16)} ` +
           `Q${X(-120)} ${f1(cy)} ${X(-118)} 232 L${X(-110)} 270 Q${X(-60)} 282 ${X(-10)} 272 Q${X(60)} 300 ${X(94)} 252 ` +
           `C${X(110)} 226 ${X(116)} ${f1(cy + 50)} ${X(112)} ${f1(cy)} Z`; }
  // (ticket 451, Matthew: "front view looks good now, except we want a rounded crown, with no peak sticking up now")
  return `M-10 ${f1(cy)} Q-12 ${f1(top + 8)} 60 ${f1(top - 8)} Q100 ${f1(top - 16)} 140 ${f1(top - 8)} Q212 ${f1(top + 8)} 210 ${f1(cy)} ` +
         `C214 ${f1(cy + 50)} 206 230 202 268 Q186 276 168 262 Q150 302 100 304 Q50 302 32 262 Q14 276 -2 268 C-6 230 -14 ${f1(cy + 50)} -10 ${f1(cy)} Z`;
}
function clothHoodOpening(F, dir = 0) {
  const top = 64, bot = headChin(F) + 12, cy = (top + bot) / 2, ry = (bot - top) / 2, cx = 100 + dir * 40, rx = dir ? 62 : 72;
  return `M${f1(cx - rx)} ${f1(cy)} A${f1(rx)} ${f1(ry)} 0 1 0 ${f1(cx + rx)} ${f1(cy)} A${f1(rx)} ${f1(ry)} 0 1 0 ${f1(cx - rx)} ${f1(cy)} Z`;
}
const clothHoodBack = (F, c, dir = 0) => `<path d="${clothHoodPath(F, dir)}" fill="${darker(c)}" stroke="${darker(darker(c))}" stroke-width="4" stroke-linejoin="round"/>`;
// (ticket 449, Matthew: "maybe rounded/beveled to indicate that") a soft fold just inside its bottom edge, the cloth's thickness
const clothHoodHem = (c, dir = 0) => { const X = d => f1(100 + dir * d), st = `fill="none" stroke="${darker(c)}" stroke-width="3" stroke-linecap="round" opacity="0.45"`;
  return dir ? `<path d="M${X(-102)} 262 Q${X(-60)} 272 ${X(-12)} 264 Q${X(52)} 286 ${X(84)} 248" ${st}/>`
             : `<path d="M8 260 Q22 266 38 254 Q54 290 100 292 Q146 290 162 254 Q178 266 192 260" ${st}/>`; };
const clothHoodFront = (F, c, dir = 0) => `<path d="${clothHoodPath(F, dir)} ${clothHoodOpening(F, dir)}" fill="${c}" fill-rule="evenodd" stroke="${darker(c)}" stroke-width="4" stroke-linejoin="round"/>` +
  `<path d="${clothHoodOpening(F, dir)}" fill="none" stroke="${darker(darker(c))}" stroke-width="3" opacity="0.6"/>` + clothHoodHem(c, dir) + (dir ? clothHoodFlap(F, c, dir) : "");
// (ticket 448) the peak, in profile: a pointed flap from the crown's back hanging down behind the head, past the neck, shaded
function clothHoodFlap(F, c, dir) {
  const { cy, ry } = hoodGeom(F), top = cy - ry, bot = cy + ry, X = d => f1(100 + dir * d);
  return `<path data-hood="flap" d="M${X(-58)} ${f1(top + 2)} Q${X(-110)} ${f1(top + 14)} ${X(-116)} ${f1(cy)} L${X(-106)} ${f1(bot + 56)} Q${X(-98)} ${f1(bot + 66)} ${X(-90)} ${f1(bot + 54)} L${X(-74)} ${f1(cy + 6)} Q${X(-70)} ${f1(top + 24)} ${X(-58)} ${f1(top + 2)} Z" ` +
    `fill="${mix(c, "#000000", 0.08)}" stroke="${darker(c)}" stroke-width="3.5" stroke-linejoin="round"/>` +
    `<path d="M${X(-62)} ${f1(top + 8)} L${X(-98)} ${f1(bot + 56)} L${X(-76)} ${f1(cy + 6)} Z" fill="#000000" opacity="0.1"/>`;
}
// (ticket 447, Matthew: "the away facing view also looks wrong, like L and R --- the peak should come toward us now that the face
// is facing away") From behind, the hood's top is round and its peak comes toward us: a pointed flap over the back of the head,
// its tip down over the shoulders, shaded on one side.
function clothHoodAway(F, c) {
  const { cy, ry } = hoodGeom(F), top = cy - ry, bot = cy + ry, ln = darker(c);
  const round = `M-10 ${f1(cy)} Q-12 ${f1(top + 8)} 60 ${f1(top - 8)} Q100 ${f1(top - 16)} 140 ${f1(top - 8)} Q212 ${f1(top + 8)} 210 ${f1(cy)} ` +
                `C214 ${f1(cy + 50)} 206 230 202 268 Q100 300 -2 268 C-6 230 -14 ${f1(cy + 50)} -10 ${f1(cy)} Z`;   // (ticket 449: falling straight, no flare)
  const flap = `M54 ${f1(top + 18)} Q100 ${f1(top + 2)} 146 ${f1(top + 18)} L108 ${f1(bot + 52)} Q100 ${f1(bot + 62)} 92 ${f1(bot + 52)} Z`;
  return `<path d="${round}" fill="${c}" stroke="${ln}" stroke-width="4" stroke-linejoin="round"/>` +
    `<path d="M10 258 Q100 286 190 258" fill="none" stroke="${ln}" stroke-width="3" stroke-linecap="round" opacity="0.45"/>` +
    `<path data-hood="closed" d="${flap}" fill="${mix(c, "#000000", 0.08)}" stroke="${ln}" stroke-width="3.5" stroke-linejoin="round"/>` +
    `<path d="M100 ${f1(top + 10)} L100 ${f1(bot + 54)} L146 ${f1(top + 18)} Z" fill="#000000" opacity="0.1"/>`;
}

// (ticket 557, for M) THE NINJA'S HEAD: a black mask that hides the whole face, fitted tight (Matthew: "look at the hooded
// monk/dryad, but let's make the mask fit tight, not so loose"), and over it the gold head covering. In the face's units,
// on the head's own outline as drawn (headPoints: its width, height and alien shape already in it, so no hairStretch), grown
// NINJA_FIT all round from the head's middle: the cloth hugs the head and the neck (the neck in the cloth: drawNow's
// neckC). The face is not drawn under it (drawNow), nor the hair, the ears or a beard (`hooded`); a narrow slit across the
// eyes is black, the eyes in it only with 👀 (ninjaEyes, off), drawn as the monk's eyes in the dark are, inside the slit.
// Every view its own drawing: facing us (dir 0) the slit across both eyes, a face cloth's edge under it; in profile (dir -1
// or 1) the slit on the near side, open at the front edge, and the cloth over the nose a small bump (with a nose); from
// behind (away) no slit. `turn` (ticket 144, turned partway) slides what is on the face, as faceSvg slides the features.
const NINJA_FIT = 5, NINJA_BW = 11;
function ninjaMaskPts(F, grow = NINJA_FIT) {
  return headPoints(F).map(([x, y]) => { const dx = x - 100, dy = y - HEAD_C, r = Math.hypot(dx, dy) || 1, k = (r + grow) / r; return [100 + dx * k, HEAD_C + dy * k]; });
}
const ninjaPath = pts => "M" + pts.map(([x, y]) => f1(x) + " " + f1(y)).join(" L") + " Z";
// (ticket 274's rule: a clip's id named by its shape, not by chance, so two friends' clips never share one)
const ninjaClip = (pre, d) => { let h = 2166136261; for (let i = 0; i < d.length; i++) h = Math.imul(h ^ d.charCodeAt(i), 16777619);
  const id = pre + (h >>> 0).toString(36); return { id, def: `<clipPath id="${id}"><path d="${d}"/></clipPath>` }; };
// the slit's middle line, the band's line (above the brows, below the crown) and the head's mask outline
function ninjaGeom(F) {
  const LY = faceLayout(F), crown = headCrown(F), pts = ninjaMaskPts(F);
  return { LY, pts, eyeY: LY.eyeY, bandY: crown + 0.55 * (LY.eyeY - crown), top: Math.min(...pts.map(p => p[1])), hh: 8 };
}
function ninjaMask(F, dir = 0, away = false, turn = 0) {
  const G = ninjaGeom(F), { pts, eyeY, hh, LY } = G, P = (x, y) => f1(x) + " " + f1(y), soft = mix(NINJA.cloth, "#ffffff", 0.14);
  // (the cloth's sheen: a lighter arc inside the outline over the crown, toward the back of the head in profile)
  const sheenPts = ninjaMaskPts(F, NINJA_FIT - 9), i0 = dir > 0 ? 42 : dir < 0 ? 58 : 40, sheen = sheenPts.slice(i0, i0 + 11);
  let s = "";
  // (in profile, the cloth over the nose: a small bump at the front edge, under the mask's own outline)
  const noseOn = F.noseView && F.noseView !== "none";
  if (dir && !away && noseOn) { const ny = LY.eyeY + 6 + 15 * F.noseH / 100, nh = Math.max(8, 13 * F.noseH / 100), ex = acrossAt(pts, ny)[dir > 0 ? 1 : 0], b = Math.min(11, 3 + 6 * F.noseW / 100);
    s += `<path d="M${P(ex - dir * 6, ny - nh)} Q${P(ex + dir * b, ny - 2)} ${P(ex + dir * (b - 2), ny + nh * 0.6)} Q${P(ex + dir * 2, ny + nh)} ${P(ex - dir * 6, ny + nh)} Z" fill="${NINJA.cloth}" stroke="${NINJA.line}" stroke-width="3" stroke-linejoin="round"/>`; }
  s += `<path data-ninja="mask" d="${ninjaPath(pts)}" fill="${NINJA.cloth}" stroke="${NINJA.line}" stroke-width="3.5" stroke-linejoin="round"/>` +
       `<path d="M${sheen.map(([x, y]) => P(x, y)).join(" L")}" fill="none" stroke="${soft}" stroke-width="7" stroke-linecap="round" stroke-linejoin="round" opacity="0.6"/>`;
  if (away) return s;
  const cx = 100 + turn * 14, edge = (y, t) => { const [l, r] = acrossAt(pts, y); return dir ? (dir > 0 ? r : l) : t < 0 ? l : r; };
  if (!dir) {
    // facing us: the slit across both eyes, inside the head's outline, an almond narrower than the eyes are tall
    const reach = LY.eyeDX + eyeHalfW(F) + 12, xl = Math.max(edge(eyeY, -1) + 12, cx - reach), xr = Math.min(edge(eyeY, 1) - 12, cx + reach), q = (xr - xl) * 0.25;
    s += `<path data-ninja="slit" d="M${P(xl, eyeY)} C${P(xl + q, eyeY - hh * 1.35)} ${P(xr - q, eyeY - hh * 1.35)} ${P(xr, eyeY)} C${P(xr - q, eyeY + hh * 1.35)} ${P(xl + q, eyeY + hh * 1.35)} ${P(xl, eyeY)} Z" fill="${NINJA.slit}"/>`;
    // the face cloth's top edge, just under the slit, from side to side
    const wy = eyeY + hh + 6;
    s += `<path d="M${P(edge(wy, -1) + 3, wy + 2)} Q${P(cx, wy + 7)} ${P(edge(wy, 1) - 3, wy + 2)}" fill="none" stroke="${soft}" stroke-width="2.5" stroke-linecap="round" opacity="0.7"/>`;
    return s;
  }
  // in profile: the slit from behind the near eye to the face's front edge, and the face cloth's edge under it
  const sh = 24 * headWidth(F), x0 = 100 + dir * (sh + LY.eyeDX - eyeHalfW(F) - 8), xe = edge(eyeY), xm = x0 + (xe - x0) * 0.55;
  s += `<path data-ninja="slit" d="M${P(x0, eyeY)} Q${P(xm, eyeY - hh * 1.2)} ${P(xe, eyeY - hh)} L${P(xe, eyeY + hh * 0.8)} Q${P(xm, eyeY + hh * 1.1)} ${P(x0, eyeY)} Z" fill="${NINJA.slit}"/>`;
  const wy = eyeY + hh + 6;
  s += `<path d="M${P(x0 - dir * 14, wy + 1)} Q${P(xm, wy + 4)} ${P(edge(wy) - dir * 2, wy + 1)}" fill="none" stroke="${soft}" stroke-width="2.5" stroke-linecap="round" opacity="0.7"/>`;
  return s;
}
// the eye slit alone, as a clip for the eyes in it (ninjaEyes), the same shape as ninjaMask's: { id, def }
function ninjaSlitClip(F, dir = 0, turn = 0) {
  const m = ninjaMask(F, dir, false, turn).match(/data-ninja="slit" d="([^"]+)"/);
  return m ? ninjaClip("ninja-slit-", m[1]) : null;
}
// THE HEAD COVERING (ninjaBand): a gold band round the head above the brows and below the crown, and a second band from the
// middle of the forehead over the top of the head to the back, joined to the first in front and behind (an inverted T on
// the forehead), the gem where they meet in front (ninjaGemSvg), a round gold clasp where they meet behind. Both bands are
// drawn inside the mask's outline grown by one (a band's thickness), so their ends meet its edge as they go round it: their
// outlines first and then their gold, so the T is one piece. Facing us and from behind, the band round the head bows down a
// little at the middle (a ring seen from just above it) and the band over the top rises from the junction; in profile the
// round band crosses the whole head and the band over the top follows the head's outline from the forehead to the back,
// the gem standing out at the forehead's edge.
function ninjaBand(F, WC, dir = 0, away = false, turn = 0) {
  const G = ninjaGeom(F), { pts, bandY: yb, top } = G, bw = NINJA_BW, bow = 4, P = (x, y) => f1(x) + " " + f1(y), clip = ninjaClip("ninja-band-", ninjaPath(ninjaMaskPts(F, NINJA_FIT + 1)));
  const cx = 100 + (away ? 0 : turn * 14), stroke = (d, c, w) => `<path d="${d}" fill="none" stroke="${c}" stroke-width="${f1(w)}" stroke-linejoin="round"/>`;
  let round, over, gem = "", clasp = "";
  if (!dir) {
    round = `M${P(-60, yb - bow)} Q${P(cx, yb + 2 * bow)} ${P(260, yb - bow)}`;
    over = `M${P(cx, yb + bow)} Q${P(cx + (100 - cx) * 0.2, (yb + top) / 2)} ${P(100 + (cx - 100) * 0.3, top - 20)}`;
    if (away) clasp = `<circle cx="${f1(cx)}" cy="${f1(yb + bow)}" r="7.5" fill="${NINJA.gold}" stroke="${NINJA.goldLine}" stroke-width="2"/><circle cx="${f1(cx - 2.5)}" cy="${f1(yb + bow - 2.5)}" r="2.2" fill="${NINJA.goldLight}"/>`;
    else gem = ninjaGemSvg(WC.ninjaGem, WC.ninjaGemShape, cx, yb + bow, 10, ninjaGemFx(WC));
  } else {
    // in profile: the round band straight across, the band over the top along the head's outline, inset by half its width
    round = `M${P(-60, yb)} L${P(260, yb)}`;
    const inner = ninjaMaskPts(F, NINJA_FIT + 1 - bw / 2), [xl, xr] = acrossAt(inner, yb);
    const arc = inner.slice(36).filter(([, y]) => y < yb);
    over = `M${P(xl, yb)} L${arc.map(([x, y]) => P(x, y)).join(" L")} L${P(xr, yb)}`;
    if (!away) gem = ninjaGemSide(WC.ninjaGem, WC.ninjaGemShape, acrossAt(pts, yb)[dir > 0 ? 1 : 0] + dir, yb, 11, dir, ninjaGemFx(WC));
  }
  const shine = !dir ? `M${P(-60, yb - bow - 3)} Q${P(cx, yb + 2 * bow - 3)} ${P(260, yb - bow - 3)}` : `M${P(-60, yb - 3)} L${P(260, yb - 3)}`;
  return clip.def + `<g clip-path="url(#${clip.id})">${stroke(round + " " + over, NINJA.goldLine, bw + 3.5)}${stroke(round + " " + over, NINJA.gold, bw)}${stroke(shine, NINJA.goldLight, 2.5)}</g>` + clasp + gem;
}
// a gem's outline, about (cx, cy), r its size (a round gem's radius), one of NINJA_GEM_SHAPES; and how far each reaches above
// and below its middle, in r (for its side view)
const NINJA_GEM_REACH = { round: [1, 1], oval: [1.08, 1.08], rhombus: [1.22, 1.22], teardrop: [1.3, 0.85], star: [1.2, 1], heart: [1.05, 1.05] };
function ninjaGemPath(shape, cx, cy, r) {
  const P = (x, y) => f1(cx + x * r) + " " + f1(cy + y * r);
  switch (shape) {
    case "oval": return `M${P(-0.74, 0)} A${f1(0.74 * r)} ${f1(1.08 * r)} 0 1 0 ${P(0.74, 0)} A${f1(0.74 * r)} ${f1(1.08 * r)} 0 1 0 ${P(-0.74, 0)} Z`;
    case "rhombus": return `M${P(0, -1.22)} L${P(0.82, 0)} L${P(0, 1.22)} L${P(-0.82, 0)} Z`;
    case "teardrop": return `M${P(0, -1.3)} C${P(0.3, -0.85)} ${P(0.85, -0.45)} ${P(0.85, 0)} A${f1(0.85 * r)} ${f1(0.85 * r)} 0 0 1 ${P(-0.85, 0)} C${P(-0.85, -0.45)} ${P(-0.3, -0.85)} ${P(0, -1.3)} Z`;
    case "star": return "M" + Array.from({ length: 10 }, (_, i) => { const a = -Math.PI / 2 + i * Math.PI / 5, q = i % 2 ? 0.52 : 1.2; return P(q * Math.cos(a), q * Math.sin(a)); }).join(" L") + " Z";
    case "heart": return `M${P(0, 1.05)} C${P(-0.35, 0.72)} ${P(-1.1, 0.2)} ${P(-1.05, -0.35)} C${P(-1, -0.95)} ${P(-0.25, -1.05)} ${P(0, -0.5)} C${P(0.25, -1.05)} ${P(1, -0.95)} ${P(1.05, -0.35)} C${P(1.1, 0.2)} ${P(0.35, 0.72)} ${P(0, 1.05)} Z`;
    default: return `M${P(-1, 0)} A${f1(r)} ${f1(r)} 0 1 0 ${P(1, 0)} A${f1(r)} ${f1(r)} 0 1 0 ${P(-1, 0)} Z`;
  }
}
const ninjaGemOf = kind => NINJA_GEMS[kind] || NINJA_GEMS.ruby;
const ninjaShapeOf = shape => NINJA_GEM_REACH[shape] ? shape : "round";
// (ticket 560, Matthew, with a picture of a cut oval ruby: "For the gems -- we also want the default (ruby) to have apparent
// cut marks, as a gemstone ... Please do the same for the others (beleved edge lines)") THE GEM'S CUT, each shape's own, in r
// about the stone's middle: `rim`, points on its girdle round from the top, clockwise, a corner (where a crown facet's far
// point is) then the point between it and the next corner, and so on; `c` the table's middle and `t` how far out it reaches
// toward each corner (the table is the corners drawn in toward c, so it follows the shape); `split` (the star) cuts each
// point's facet in two down its middle, a lit half and a shaded one. A handful of facets: 6 corners (4 on the diamond shape,
// 5 on the star, one a point; `glint`, how far out toward the light the glint is, if not 0.62), each a kite from the table out to its corner and a triangle from each table edge out to the
// rim, which tile the crown between the table and the girdle. The rim's points are on the outline itself, and its arcs
// between them bulge outward (the heart's cleft is a point of its own), so no facet reaches past the stone.
const ninjaRing = (rx, ry, n = 12) => Array.from({ length: n }, (_, i) => { const a = -Math.PI / 2 + i * 2 * Math.PI / n; return [rx * Math.cos(a), ry * Math.sin(a)]; });
const NINJA_GEM_CUTS = {
  round: { rim: ninjaRing(1, 1), c: [0, 0], t: 0.5 },
  oval: { rim: ninjaRing(0.74, 1.08), c: [0, 0], t: 0.5 },
  rhombus: { rim: [[0, -1.22], [0.41, -0.61], [0.82, 0], [0.41, 0.61], [0, 1.22], [-0.41, 0.61], [-0.82, 0], [-0.41, -0.61]], c: [0, 0], t: 0.46, glint: 0.45 },
  // (on ninjaGemPath's curves: a third and two thirds along each, and every 30 degrees round the bottom's arc)
  teardrop: { rim: [[0, -1.3], [0.354, -0.863], [0.696, -0.437], [0.85, 0], [0.736, 0.425], [0.425, 0.736], [0, 0.85], [-0.425, 0.736], [-0.736, 0.425], [-0.85, 0], [-0.696, -0.437], [-0.354, -0.863]], c: [0, 0.04], t: 0.5 },
  star: { rim: Array.from({ length: 10 }, (_, i) => { const a = -Math.PI / 2 + i * Math.PI / 5, q = i % 2 ? 0.52 : 1.2; return [q * Math.cos(a), q * Math.sin(a)]; }), c: [0, 0], t: 0.3, split: true, glint: 0.4 },
  // (a corner toward each lobe's top and down each side, the cleft and the bottom tip between corners)
  heart: { rim: [[0.372, -0.839], [0.811, -0.778], [1.05, -0.35], [0.878, 0.184], [0.439, 0.663], [0, 1.05], [-0.439, 0.663], [-0.878, 0.184], [-1.05, -0.35], [-0.811, -0.778], [-0.372, -0.839], [0, -0.5]], c: [0, 0.08], t: 0.48 },
};
// the gem's tones, from its colour, its light and its outline: the table a little deeper than the stone, the shaded facets
// deeper still, the lit ones halfway to its light, the flashes its light (not near white: the white is the cuts', below)
const ninjaGemTones = g => ({ table: mix(g.c, g.line, 0.18), dark: mix(g.c, g.line, 0.5), light: mix(g.c, g.light, 0.45), flash: g.light });
// (ticket 560, Matthew: "gems don't appear faceted yet -- at all", then "we need to add sharp, bright white lines to
// indicate the 'cuts', perhaps with an animation that makes them 'glimmer' -- and other animations optional too for
// stars/shine on them, all over") THE CUTS IN WHITE, AND THE GEM'S LIGHTS. At its true size the gem is a few pixels across,
// and its cuts were a pale line a quarter of a pixel wide: unseen. Each cut is now drawn twice in white, `w` wide in the
// stone's own units and never thinner than NINJA_CUT_PX on the screen (non-scaling-stroke), so it is sharp small and in
// proportion big. `fx`, the ninja's rows: `glimmer` (✨, on at first), a wider, brighter copy of each group of cuts
// flashing in turn clockwise round the stone, a light running round its cuts; `stars` (⭐, off at first), four-pointed stars that
// swell and vanish at `spots` all over the stone ([x, y, size in r]), each at its own pace; `shine` (🌟, off at first), a
// pale band sweeping across the stone from the light, clipped to it. Every animation keeps time with the picture's clock
// (begun before 0, as the tails' wags), so redrawing the friend never restarts one.
const NINJA_CUT_PX = 0.8, NINJA_GLIMMER_S = 2.4, NINJA_SHINE_S = 3.6;
const ninjaGemFx = WC => ({ glimmer: WC.ninjaGlimmer !== false, stars: !!WC.ninjaGemStars, shine: !!WC.ninjaGemShine });
const ninjaCut = (d, w, px) => `<path d="${d}" fill="none" stroke="#ffffff" stroke-width="${f1(w)}" stroke-linejoin="round" stroke-linecap="round"/>` +
  `<path d="${d}" fill="none" stroke="#ffffff" stroke-width="${px}" stroke-linejoin="round" stroke-linecap="round" vector-effect="non-scaling-stroke"/>`;
const ninjaStar = s => { const P = (x, y) => f1(x) + " " + f1(y); return `M${P(0, -s)} Q0 0 ${P(s, 0)} Q0 0 ${P(0, s)} Q0 0 ${P(-s, 0)} Q0 0 ${P(0, -s)} Z`; };
function ninjaGemLights(fx, { groups, stone, c: [cx, cy], r, w, spots }) {
  let s = "";
  if (fx.glimmer) s += groups.map((d, j) => `<g opacity="0">${ninjaCut(d, 2 * w, 2 * NINJA_CUT_PX)}<animate attributeName="opacity" values="0;1;0;0" ` +
    `keyTimes="0;0.07;0.18;1" dur="${NINJA_GLIMMER_S}s" begin="${f1((j / groups.length - 1) * NINJA_GLIMMER_S)}s" repeatCount="indefinite"/></g>`).join("");
  if (fx.shine) {   // (a band across the light's way, from beyond the stone's upper left to beyond its lower right, then waiting there)
    const c = ninjaClip("ninja-shine-", stone), u = 0.7071, L = 2 * r, b = 0.22 * r, far = f1(1.9 * r * u);
    const P = (a, q) => f1(cx + a * u + q * u) + " " + f1(cy + a * u - q * u);
    s += c.def + `<g clip-path="url(#${c.id})"><path d="M${P(-b, -L)} L${P(b, -L)} L${P(b, L)} L${P(-b, L)} Z" fill="#ffffff" opacity="0.65" transform="translate(-${far} -${far})">` +
      `<animateTransform attributeName="transform" type="translate" values="-${far} -${far};${far} ${far};${far} ${far}" keyTimes="0;0.3;1" dur="${NINJA_SHINE_S}s" ` +
      `begin="-0.5s" repeatCount="indefinite"/></path></g>`;
  }
  if (fx.stars) s += spots.map(([x, y, k], i) => `<g transform="translate(${f1(x)} ${f1(y)})"><path d="${ninjaStar(k * r)}" fill="#ffffff" transform="scale(0)">` +
    `<animateTransform attributeName="transform" type="scale" values="0;1;0;0" keyTimes="0;0.12;0.26;1" dur="${(1.9 + 0.43 * i).toFixed(2)}s" ` +
    `begin="${(-0.71 * i).toFixed(2)}s" repeatCount="indefinite"/></path></g>`).join("");
  return s;
}
// the gem facing us: a gold setting a size bigger, the stone, its crown's facets in three tones over it (one path each: lit
// from up and to our left, the kites there flash and the next ones are lit, the rest the stone's own colour, the triangles
// between them shaded but those nearest the light), the table, the cuts in white (a group to each corner: the two cuts out
// from it to the rim and the table's edge on to the next), the girdle's outline, a glint on the crown toward the light, and
// over it all the lights (`fx`: ninjaGemLights)
function ninjaGemSvg(kind, shape, cx, cy, r, fx = {}) {
  const g = ninjaGemOf(kind), sh = ninjaShapeOf(shape), T = ninjaGemTones(g), cut = NINJA_GEM_CUTS[sh], R = cut.rim, k = R.length / 2;
  const at = ([x, y]) => [cx + x * r, cy + y * r], P = ([x, y]) => f1(x) + " " + f1(y), poly = pts => "M" + pts.map(P).join(" L") + " Z";
  const C = at(cut.c), corner = j => at(R[(2 * j) % R.length]), mid = j => at(R[(2 * ((j + k) % k) + 1) % R.length]);
  const tab = Array.from({ length: k }, (_, j) => { const q = corner(j); return [C[0] + (q[0] - C[0]) * cut.t, C[1] + (q[1] - C[1]) * cut.t]; });
  const lit = pts => { const m = pts.reduce((a, p) => [a[0] + p[0] / pts.length, a[1] + p[1] / pts.length], [0, 0]), dx = m[0] - C[0], dy = m[1] - C[1], l = Math.hypot(dx, dy) || 1;
    return (-0.6 * dx - 0.8 * dy) / l; };
  const tone = { flash: [], light: [], dark: [] }, put = (t, pts) => { if (t) tone[t].push(poly(pts)); };
  for (let j = 0; j < k; j++) {
    const Tj = tab[j], Tn = tab[(j + 1) % k], Rj = corner(j), Qj = mid(j), Qp = mid(j - 1);
    if (cut.split) { const a = [Tj, Qp, Rj], b = [Tj, Rj, Qj];   // (the star's points: the half nearer the light lit, the other shaded)
      const [hi, lo] = lit(a) >= lit(b) ? [a, b] : [b, a]; put(lit(hi) > 0.3 ? "flash" : "light", hi); put("dark", lo); }
    else { const s = [Tj, Qj, Rj, Qp], v = lit(s); put(v > 0.55 ? "flash" : v > 0 ? "light" : null, s); }
    const tri = [Tj, Tn, Qj], v = lit(tri); put(v > 0.5 ? null : "dark", tri);
  }
  const groups = tab.map((Tj, j) => `M${P(mid(j - 1))} L${P(Tj)} L${P(mid(j))}` + (cut.split ? ` M${P(Tj)} L${P(corner(j))}` : "") + ` M${P(Tj)} L${P(tab[(j + 1) % k])}`);
  const lw = v => f1(v * r), stone = ninjaGemPath(sh, cx, cy, r), glint = [C[0] - 0.6 * (cut.glint ?? 0.62) * r, C[1] - 0.8 * (cut.glint ?? 0.62) * r];
  // (the stars: a big one at the glint, and smaller ones at every other corner of the rim, round the stone)
  const spots = [[...glint, 0.8], ...Array.from({ length: k }, (_, j) => j).filter(j => j % 2).map(j => [...corner(j), 0.5])];
  return `<g data-ninja="gem"><path d="${ninjaGemPath(sh, cx, cy, r * 1.32)}" fill="${NINJA.gold}" stroke="${NINJA.goldLine}" stroke-width="2" stroke-linejoin="round"/>` +
    `<path d="${stone}" fill="${g.c}"/>` +
    ["dark", "light", "flash"].map(t => tone[t].length ? `<path d="${tone[t].join(" ")}" fill="${T[t]}"/>` : "").join("") +
    `<path d="${poly(tab)}" fill="${T.table}"/>` +
    ninjaCut(groups.join(" "), 0.05 * r, NINJA_CUT_PX) +
    `<path d="${stone}" fill="none" stroke="${g.line}" stroke-width="${lw(0.14)}" stroke-linejoin="round"/>` +
    `<circle cx="${f1(glint[0])}" cy="${f1(glint[1])}" r="${lw(0.09)}" fill="#ffffff"/>` +
    ninjaGemLights(fx, { groups, stone, c: [cx, cy], r, w: 0.05 * r, spots }) + `</g>`;
}
// the gem from the side, at the forehead's edge (x, cy), toward `dir`: its setting's rim, and the stone standing out of it, as
// tall as the shape is facing us, cut (ticket 560): a flat table at its front edge between two sloping crown facets, the upper
// one lit, the lower shaded, their cuts in white (pointed, the diamond shape and the star: two facets meeting at the point),
// and the lights over it (`fx`: ninjaGemLights, each cut its own group, the stars at the front, on the lit facet and low)
function ninjaGemSide(kind, shape, x, cy, r, dir, fx = {}) {
  const g = ninjaGemOf(kind), T = ninjaGemTones(g), sh = ninjaShapeOf(shape), [up, down] = NINJA_GEM_REACH[sh], y0 = cy - up * r, y1 = cy + down * r, P = (a, b) => f1(a) + " " + f1(b);
  const out = 0.8 * r, xs = x + dir * 3, pointed = sh === "rhombus" || sh === "star", h = y1 - y0, xf = xs + dir * out * 0.75;
  const ta = pointed ? [xs + dir * out, cy] : [xf, y0 + h * 0.3], tb = pointed ? ta : [xf, y1 - h * 0.3], ym = pointed ? cy : (ta[1] + tb[1]) / 2;
  const stone = `M${P(xs, y0)} L${P(...ta)} L${P(...tb)} L${P(xs, y1)} Z`;
  const groups = pointed ? [`M${P(xs, ym)} L${P(...ta)}`] : [`M${P(xs, ym)} L${P(...ta)}`, `M${P(...ta)} L${P(...tb)}`, `M${P(...tb)} L${P(xs, ym)}`];
  const spots = [[...ta, 0.7], [(2 * xs + ta[0]) / 3, (y0 + ta[1] + ym) / 3, 0.5], [xs + dir * out * 0.3, y1 - h * 0.2, 0.45]];
  return `<g data-ninja="gem"><path d="M${P(x - dir * 2, y0 - 1.5)} L${P(xs, y0 - 1.5)} L${P(xs, y1 + 1.5)} L${P(x - dir * 2, y1 + 1.5)} Z" fill="${NINJA.gold}" stroke="${NINJA.goldLine}" stroke-width="1.8" stroke-linejoin="round"/>` +
    `<path d="${stone}" fill="${pointed ? g.c : T.table}"/>` +
    `<path d="M${P(xs, y0)} L${P(...ta)} L${P(xs, ym)} Z" fill="${T.light}"/><path d="M${P(xs, ym)} L${P(...tb)} L${P(xs, y1)} Z" fill="${T.dark}"/>` +
    ninjaCut(groups.join(" "), 0.55, NINJA_CUT_PX) +
    `<path d="${stone}" fill="none" stroke="${g.line}" stroke-width="1.6" stroke-linejoin="round"/>` +
    ninjaGemLights(fx, { groups, stone, c: [(xs + ta[0]) / 2, cy], r, w: 0.55, spots }) + `</g>`;
}
// THE BOOTS' BANDS (ninjaBoots), over the trousers from the ankle up the shin to below the knee, along the leg's own joints
// (J, from the hip; the shin from the knee, or a straight leg's middle, to the ankle), `half` the trouser leg's half width:
// "braided" (the default; Matthew: "we want the bands to be braided together, not wrapped around laterally"), two strands
// in two tones crossing the shin from edge to edge, over and under in turn, between a tie at the ankle and one at the top;
// "wrapped", straight round the shin, band above band, each bowed a little toward the ankle as a ring is seen from above.
// Each set of strokes is one path (an outline under, the fills over), so the shapes stay few whatever the shin's length.
// (ticket 561, M's design, Matthew: "M says that she wants the bands on the shins/ankles to show through the skin color
// between them -- let's have that on be default, but a toggle to change to 'full darkness' mode") `skin`, the friend's skin
// colour, shows between the bands, as if they were wound over bare legs: a strip of it as wide as the bands under them, from
// the ankle's tie to the top one (wrapped: from the first band to the last, the bands narrower so the skin shows between
// them); null, 🌑 full darkness (ninjaBetween "dark"): the trousers under the bands, drawn exactly as before.
// (ticket 561, M: "ninja should have bands, with skin showing through, on the wrists/forearms") The same bands on each
// forearm, J the arm's joints (shoulder, elbow, hand), `half` the sleeve's half width, `from` the glove's reach up the
// forearm from the hand: the bands from there to below the elbow, a little thinner and fewer for the forearm's width.
function ninjaShin(J, half, style, skin = null, from = 0) {
  if (!J || J.length < 2) return "";
  const F = J[J.length - 1], K = J.length > 2 ? J[J.length - 2] : [(J[0][0] + F[0]) / 2, (J[0][1] + F[1]) / 2];
  const dx = K[0] - F[0], dy = K[1] - F[1], len = Math.hypot(dx, dy) || 1, nx = -dy / len, ny = dx / len, top = 0.84, wd = half + 1;
  const lo = from ? Math.min(0.45, from / len) : 0, span = lo ? len * (top - lo) : len * top, tl = lo ? len * (top - lo) / top : len;   // (lo: where the bands begin)
  const at = (t, s) => { const u = lo ? lo + t * (top - lo) / top : t; return f1(F[0] + dx * u + nx * s) + " " + f1(F[1] + dy * u + ny * s); };
  const lines = (d, c, w) => d ? `<path d="${d}" fill="none" stroke="${c}" stroke-width="${f1(w)}"/>` : "";
  const bare = (t0, t1) => skin ? `<path data-ninja="skin" d="M${at(t0, 0)} L${at(t1, 0)}" fill="none" stroke="${skin}" stroke-width="${f1(2 * wd)}"/>` : "";
  if (style === "wrapped") {
    const n = lo ? Math.max(3, Math.round(span / 10)) : Math.max(4, Math.round(span / 11)), step = span / n, bow = Math.min(3, step * 0.3) / tl, bw = skin ? step * 0.5 : step - 2.2;
    const d = Array.from({ length: n }, (_, i) => { const t = (i + 0.5) * top / n; return `M${at(t, -wd)} Q${at(t - bow, 0)} ${at(t, wd)}`; }).join(" ");
    return bare(0.5 * top / n, (n - 0.5) * top / n) + lines(d, NINJA.bandLine, skin ? bw + 2.2 : step + 1.5) + lines(d, NINJA.bandA, bw) +
      lines(Array.from({ length: n }, (_, i) => { const t = (i + 0.5) * top / n + (skin ? 0.22 * bw : 0.3 * step) / tl; return `M${at(t, -wd * 0.8)} Q${at(t - bow, 0)} ${at(t, wd * 0.8)}`; }).join(" "), NINJA.bandB, 1.6);
  }
  const n = lo ? Math.max(2, Math.round(span / 15)) : Math.max(3, Math.round(span / 17)), sw = lo ? 4.5 : 6.5, e = wd - 3.5, under = [[], []], over = [[], []];   // (e: the strands turn just inside the leg's edge, their corners on it)
  for (let i = 0; i < n; i++) { const t0 = (0.06 + i * (top - 0.06) / n), t1 = 0.06 + (i + 1) * (top - 0.06) / n, s = i % 2 ? 1 : -1;
    const a = `M${at(t0, s * e)} L${at(t1, -s * e)}`, b = `M${at(t0, -s * e)} L${at(t1, s * e)}`;
    (i % 2 ? under : over)[0].push(a); (i % 2 ? over : under)[1].push(b); }
  const ties = `M${at(0.03, -wd)} L${at(0.03, wd)} M${at(top, -wd)} L${at(top, wd)}`;
  return bare(0.03, top) + lines(under[0].concat(under[1]).join(" "), NINJA.bandLine, sw + 2.5) + lines(under[0].join(" "), NINJA.bandA, sw) + lines(under[1].join(" "), NINJA.bandB, sw) +
    lines(over[0].concat(over[1]).join(" "), NINJA.bandLine, sw + 2.5) + lines(over[0].join(" "), NINJA.bandA, sw) + lines(over[1].join(" "), NINJA.bandB, sw) +
    lines(ties, NINJA.bandLine, lo ? 7 : 9.5) + lines(ties, NINJA.bandA, lo ? 5 : 7);
}

// Fur: the fur color with little tufts, and stripes, spots or patches in
// the markings color. Feathers: the feather color with curved bars.
// A rosette's middle: "auto" is a warm brown mixed into the fur, "off"
// leaves the fur showing through (just the broken ring), or any color.
const rosetteMiddle = C =>
  !C.spot || C.spot === "auto" ? mix(C.fur, "#7a4a10", 0.35) : C.spot === "off" ? "none" : C.spot;
function furDefs(C, kind) {
  // (ticket 499) a dragon's scales: rows of little arcs, each row half a scale along from the last, a shade darker than the
  // scales (the mock-up's pattern)
  if (kind === "dragon") {
    const sc = `<rect width="24" height="20" fill="${C.fur}"/><path d="M0 10 a6 6 0 0 0 12 0 a6 6 0 0 0 12 0 M-6 20 a6 6 0 0 0 12 0 a6 6 0 0 0 12 0 a6 6 0 0 0 12 0 ` +
               `M-6 0 a6 6 0 0 0 12 0 a6 6 0 0 0 12 0 a6 6 0 0 0 12 0" stroke="${C.marks}" fill="none" stroke-width="1.6" opacity="0.8"/>`;
    return `<pattern id="pat-fur" width="24" height="20" patternUnits="userSpaceOnUse">${sc}</pattern><pattern id="pat-bars" width="24" height="20" patternUnits="userSpaceOnUse">${sc}</pattern>`;
  }
  // (ticket 530) a Sasquatch's shag: the fur with six long tufts hanging down in each 40 tile, one path (the pattern is drawn
  // once however big the suit)
  if (kind === "sasquatch") {
    const sh = `<rect width="40" height="40" fill="${C.fur}"/><path d="M3 4 l3 9 l3 -9 M21 2 l3 10 l3 -10 M12 18 l3 9 l3 -9 M30 19 l3 10 l3 -10 M4 30 l3 9 l3 -9 M22 30 l3 9 l3 -9" ` +
               `stroke="${C.marks}" fill="none" stroke-width="1.8" stroke-linejoin="round" opacity="0.85"/>`;
    return `<pattern id="pat-fur" width="40" height="40" patternUnits="userSpaceOnUse">${sh}</pattern><pattern id="pat-bars" width="40" height="40" patternUnits="userSpaceOnUse"><rect width="40" height="40" fill="${C.fur}"/></pattern>`;
  }
  // (ticket 544) a giraffe's coat: its style's patches (GIRAFFE_COATS) in the fur colour on the cream (belly), one path a
  // tile (drawn once however big the suit)
  if (kind === "giraffe") {
    const G = GIRAFFE_COATS[(C.gf && C.gf.coat) || "net"] || GIRAFFE_COATS.net;
    return `<pattern id="pat-fur" width="${G.w}" height="${G.h}" patternUnits="userSpaceOnUse"><rect width="${G.w}" height="${G.h}" fill="${C.belly}"/>` +
           `<path d="${G.d}" fill="${C.fur}" stroke="${C.fur}" stroke-width="2" stroke-linejoin="${G.join}"/></pattern>` +
           `<pattern id="pat-bars" width="40" height="40" patternUnits="userSpaceOnUse"><rect width="40" height="40" fill="${C.fur}"/></pattern>`;
  }
  if (BIRD_SUIT[kind])   // feathers: the color with curved bars, all over
    return `<pattern id="pat-fur" width="24" height="12" patternUnits="userSpaceOnUse">` +
      `<rect width="24" height="12" fill="${C.fur}"/>` +
      `<path d="M0 9 Q6 6 12 9 Q18 12 24 9" stroke="${C.marks}" stroke-width="2.5" fill="none"/></pattern>` +
      `<pattern id="pat-bars" width="24" height="12" patternUnits="userSpaceOnUse">` +
      `<rect width="24" height="12" fill="${C.fur}"/>` +
      `<path d="M0 9 Q6 6 12 9 Q18 12 24 9" stroke="${C.marks}" stroke-width="2.5" fill="none"/></pattern>`;
  const tuft = mix(C.fur, "#000000", 0.3), m = C.marks;
  const marks = {
    // (stripes are not tiled: wrapStripes draws them on each part)
    spots:   `<circle cx="10" cy="10" r="4.5" fill="${m}"/><circle cx="30" cy="28" r="4" fill="${m}"/><circle cx="28" cy="8" r="2.5" fill="${m}"/>`,
    patches: `<ellipse cx="12" cy="14" rx="11" ry="8" fill="${m}"/><ellipse cx="34" cy="34" rx="9" ry="7" fill="${m}"/>`,
    // a leopard's rosettes: broken rings with a darker middle
    rosettes: [[11, 12, 5.5], [31, 30, 5]].map(([x, y, r]) =>
      `<circle cx="${x}" cy="${y}" r="${r}" fill="${rosetteMiddle(C)}" stroke="${m}" stroke-width="2.4" stroke-dasharray="5 2.6"/>`).join("") +
      `<circle cx="31" cy="9" r="2.2" fill="${m}"/><circle cx="9" cy="31" r="2" fill="${m}"/>`,
  }[C.furPattern] || "";
  return `<pattern id="pat-fur" width="40" height="40" patternUnits="userSpaceOnUse">` +
    `<rect width="40" height="40" fill="${C.fur}"/>${marks}` +
    `<path d="M5 22 q2 -4 4 0 M24 16 q2 -4 4 0 M14 36 q2 -4 4 0 M33 5 q2 -4 4 0" stroke="${tuft}" ` +
    `stroke-width="1.3" fill="none" opacity="0.7"/></pattern>` +
    `<pattern id="pat-bars" width="24" height="12" patternUnits="userSpaceOnUse">` +
    `<rect width="24" height="12" fill="${C.fur}"/>` +
    `<path d="M0 9 Q6 6 12 9 Q18 12 24 9" stroke="${m}" stroke-width="2.5" fill="none"/></pattern>`;
}

// WRAPPED STRIPES, a striped suit's (a tiger's, as Daniel Tiger wears
// them): each stripe comes round from the back, over a part's outline,
// and tapers to a point before the part's middle -- never across it.
const wrapStripes = C => C && C.furPattern === "stripes" && ANIMALS[state.costume.kind].suit &&
                         !BIRD_SUIT[state.costume.kind];
// One stripe from b (on or past the outline) to its tip t, w wide where it
// starts, bowed a little.
function stripe(b, t, w, color) {
  const dx = t[0] - b[0], dy = t[1] - b[1], d = Math.hypot(dx, dy) || 1;
  const nx = -dy / d * w / 2, ny = dx / d * w / 2, mx = (b[0] + t[0]) / 2, my = (b[1] + t[1]) / 2;
  return `<path d="M${f1(b[0] + nx)} ${f1(b[1] + ny)} Q${f1(mx + nx * 0.9)} ${f1(my + ny * 0.9)} ${f1(t[0])} ${f1(t[1])} ` +
         `Q${f1(mx - nx * 0.4)} ${f1(my - ny * 0.4)} ${f1(b[0] - nx)} ${f1(b[1] - ny)} Z" fill="${color}"/>`;
}
// Stripes on a limb, a band W wide from A to E: in from both edges every
// so often, from `from` along it, each ending short of the middle line by
// 15% of the half-width (a limb is narrow: shorter, they barely showed),
// and a little further along than it starts, so the two sides make a
// shallow V pointing down the limb.
function limbStripes(A, E, W, color, id, from = 0) {
  const dx = E[0] - A[0], dy = E[1] - A[1], len = Math.hypot(dx, dy);
  if (len < 1) return "";
  const ux = dx / len, uy = dy / len, nx = -uy, ny = ux, h = W / 2, gap = Math.max(18, W * 0.9);
  const at = (t, k) => [A[0] + ux * t + nx * k, A[1] + uy * t + ny * k];
  const band = [at(0, h), at(len, h), at(len, -h), at(0, -h)];
  let s = `<clipPath id="${id}"><polygon points="${pts(band)}"/></clipPath><g clip-path="url(#${id})">`;
  for (let t = from + gap * 0.6; t < len - gap * 0.4; t += gap)
    for (const side of [1, -1]) s += stripe(at(t, side * (h + 4)), at(t + gap * 0.2, side * h * 0.15), gap * 0.5, color);
  return s + `</g>`;
}
// Stripes on the body, in from both sides, every 24 down it, sloping a
// little down toward the middle; the belly, drawn over them, hides their
// tips, so they end before the middle.
function torsoStripes(outline, w, L, color) {
  let s = `<clipPath id="torso-stripes"><polygon points="${pts(outline)}"/></clipPath><g clip-path="url(#torso-stripes)">`;
  for (let y = 14; y < L - 10; y += 24)
    for (const side of [1, -1]) s += stripe([side * (w + 10), y], [side * w * 0.42, y + 7], 10, color);
  return s + `</g>`;
}
// Stripes on the legs (the suit's legs, one shape from the waist): on
// each leg, in from its outer edge and, below the crotch, its inner one,
// each ending short of the leg's middle line by 30% of the way.
function legStripes(outline, hw, fx, half, L, LL, crotch, color) {
  let s = `<clipPath id="leg-stripes"><polygon points="${pts(outline)}"/></clipPath><g clip-path="url(#leg-stripes)">`;
  for (let y = L + 14; y < L + LL - 8; y += 24)
    for (const side of [1, -1]) {
      const xo = hw + (fx + half - hw) * (y - (L - 10)) / (LL + 10);
      const xi = y > L + crotch ? (fx - half) * (y - (L + crotch)) / (LL - crotch) : null;
      const c = xi === null ? xo * 0.5 : (xo + xi) / 2;
      s += stripe([side * (xo + 8), y], [side * (c + (xo - c) * 0.3), y + 6], 10, color);
      // (starting in the gap between the legs, never in the other leg)
      if (xi !== null && c - xi > 8)
        s += stripe([side * Math.max(xi - 8, xi * 0.5), y], [side * (c - (c - xi) * 0.3), y + 6], 10, color);
    }
  return s + `</g>`;
}

// THE HOOD, in the round head's units (it stretches with the face, as hair
// does): behind the head, the hood and its ears (and a lion's mane); over
// the forehead, the brim, with the animal's face on it.
// It hugs the head as drawn: 32.6 above the crown and 27.4 below the chin
// (which an open mouth moves down), at any height. Drawn inside the
// face's stretch, so its ends are placed where they land once stretched.
function hoodGeom(F) {
  const H = F.faceH / 100, Hs = headH(F);
  const top = HEAD_C - 80 - 32.6 / Hs, bottom = HEAD_C + (80 * H + headDrop(F) + 27.4) / Hs;
  return { cy: (top + bottom) / 2, rx: 106, ry: (bottom - top) / 2 };
}
function hoodEllipse(F) {
  const { cy, rx, ry } = hoodGeom(F);
  return `cx="100" cy="${f1(cy)}" rx="${rx}" ry="${f1(ry)}"`;
}
// (dir: the way they face, 0 toward us or from behind (away), -1 or 1 in profile; only the dragon's is drawn for each)
function hoodBack(C, kind, F, dir = 0, away = false) {
  const HOOD = hoodEllipse(F);
  // a bald eagle's head is white
  const line = darker(C.fur), fill = eagleHead(kind) ? C.belly : ANIMALS[kind].suit ? "url(#pat-fur)" : C.fur;
  // (ticket 499) a dragon's horns and spines, behind the hood's own shape, which hides their roots
  if (kind === "dragon") return dragonHoodBack(C, F, dir, away) + `<ellipse ${HOOD} fill="${fill}" stroke="${line}" stroke-width="4"/>`;
  // (ticket 536) a moth's feathery antennae and fuzzy crown, behind the hood's own shape, which hides their roots; no ears
  if (kind === "moth") return mothHoodBack(C, F, dir, away) + `<ellipse ${HOOD} fill="${fill}" stroke="${line}" stroke-width="4"/>`;
  let s = "";
  if (kind === "lion") {
    const pts = [];
    for (let i = 0; i < 48; i++) {
      // the head's outline plus the mane's own 50-70, however tall the
      // head (placed where it lands once stretched)
      const t = i * Math.PI / 24, r = i % 2 ? 130 : 150, H = F.faceH / 100, Hs = headH(F);
      const grown = Math.sin(t) > 0 ? headDrop(F) : eyeLift(F);
      pts.push([100 + r * Math.cos(t), HEAD_C + ((80 * H + grown) * Math.sin(t) + (r - 80) * Math.sin(t)) / Hs]);
    }
    s += poly(pts, C.marks, darker(C.marks), 3);
  }
  for (const side of [-1, 1]) {
    const x = 100 + side * 68;
    if (kind === "wolf") {
      // tall pointed ears, pale inside
      s += poly([[100 + side * 30, 22], [100 + side * 80, -78], [100 + side * 96, 34]], C.fur, line, 3) +
           poly([[100 + side * 46, 16], [100 + side * 78, -54], [100 + side * 88, 24]], C.belly, "none", 0);
    } else if (kind === "deer") {
      // antlers first, their bases under the hood, then long ears out to
      // the side, pale inside
      const beam = `M${100 + side * 38} 14 Q${100 + side * 52} -50 ${100 + side * 96} -104 ` +
                   `M${100 + side * 48} -34 Q${100 + side * 30} -64 ${100 + side * 34} -94 ` +
                   `M${100 + side * 68} -66 Q${100 + side * 60} -100 ${100 + side * 66} -126 ` +
                   `M${100 + side * 84} -88 Q${100 + side * 104} -96 ${100 + side * 118} -86`;
      s += `<path d="${beam}" fill="none" stroke="#7a5a36" stroke-width="12" stroke-linecap="round"/>` +
           `<path d="${beam}" fill="none" stroke="#d9c29a" stroke-width="7" stroke-linecap="round"/>`;
      for (const [rx, ry, fill, w] of [[36, 15, C.fur, 3], [24, 8, C.belly, 0]])
        s += `<ellipse cx="${100 + side * 100}" cy="34" rx="${rx}" ry="${ry}" transform="rotate(${side * -22} ${100 + side * 100} 34)" ` +
             `fill="${fill}"${w ? ` stroke="${line}" stroke-width="${w}"` : ""}/>`;
    } else if (kind === "cat" || kind === "bobcat") {
      s += poly([[100 + side * 36, 16], [100 + side * 92, -46], [100 + side * 96, 40]], C.fur, line, 3) +
           poly([[100 + side * 50, 12], [100 + side * 86, -30], [100 + side * 88, 26]],
                kind === "bobcat" ? C.belly : C.accent, "none", 0);
      if (kind === "bobcat")  // the black tufts on a bobcat's ear tips
        s += poly([[100 + side * 88, -38], [100 + side * 96, -74], [100 + side * 95, -36]], C.marks, C.marks, 2);
    } else if (kind === "raccoon")
      // smaller, rounder ears with pale rims
      s += `<ellipse cx="${100 + side * 70}" cy="2" rx="22" ry="28" transform="rotate(${side * 25} ${100 + side * 70} 2)" ` +
           `fill="${C.belly}" stroke="${line}" stroke-width="3"/>` +
           `<ellipse cx="${100 + side * 70}" cy="6" rx="13" ry="18" transform="rotate(${side * 25} ${100 + side * 70} 6)" fill="${C.fur}"/>`;
    else if (!isBird(kind) && kind !== "giraffe") {   // (ticket 544: a giraffe's ears are on its own head, up its neck)
      const r = kind === "lion" ? 20 : kind === "sasquatch" ? 17 : 25;   // (ticket 530: a Sasquatch's small round ears)
      s += `<circle cx="${x}" cy="20" r="${r}" fill="${C.fur}" stroke="${line}" stroke-width="3"/>` +
           `<circle cx="${x}" cy="22" r="${r * 0.5}" fill="${kind === "tiger" ? "#ffffff" : C.belly}"/>`;
    }
  }
  s += `<ellipse ${HOOD} fill="${fill}" stroke="${line}" stroke-width="4"/>`;
  // wrapped stripes on the hood's sides, below the brim: in from the
  // edge toward the face, which covers their tips
  if (wrapStripes(C)) {
    const { cy, rx, ry } = hoodGeom(F);
    s += `<clipPath id="hood-stripes"><ellipse ${HOOD}/></clipPath><g clip-path="url(#hood-stripes)">`;
    for (const deg of [-12, 12, 36, 60])
      for (const side of [1, -1]) {
        const t = deg * Math.PI / 180, c = Math.cos(t) * side, sn = Math.sin(t);
        s += stripe([100 + (rx + 6) * c, cy + (ry + 6) * sn], [100 + (rx - 26) * c, cy + (ry - 26) * sn], 11, C.marks);
      }
    s += `</g>`;
  }
  if (falconHead(kind))  // the peregrine's dark "moustache" down each cheek
    for (const side of [-1, 1])
      s += `<path d="M${100 + side * 84} 52 Q${100 + side * 96} 96 ${100 + side * 84} 132 ` +
           `Q${100 + side * 72} 96 ${100 + side * 72} 52 Z" fill="${C.marks}"/>`;
  return s;
}
// (ticket 124) A hood in profile, in its own units, over the head: the fur
// over the top (the brim, down to its line at 54) and over the back of the
// head behind the face (from 18 behind its middle), the face showing in
// front of that; on the brim, the animal's face on our side only, carried
// to the front as the friend's own features are.
function hoodSide(C, kind, F, dir) {
  const { cy, rx, ry } = hoodGeom(F), line = darker(C.fur), HOOD = hoodEllipse(F);
  const fill = falconHead(kind) ? C.marks : eagleHead(kind) ? C.belly : ANIMALS[kind].suit ? "url(#pat-fur)" : C.fur;
  // (the face's opening: an oval at the front, from under the brim's line
  // down past the chin, and from behind the ear out past the hood's front,
  // so the hood is open there; its edge outlined where it crosses the hood)
  const fx = 100 + dir * 0.48 * rx, fcy = (54 + cy + ry) / 2 + 4, frx = 0.8 * rx, fry = (cy + ry - 54) / 2 + 6;
  const hole = `cx="${f1(fx)}" cy="${f1(fcy)}" rx="${f1(frx)}" ry="${f1(fry)}"`;
  // (ticket 151: a clip, the mask's white square with the oval cut out, not
  // a mask, which the browser draws again offscreen on every frame)
  const cap = `<defs><clipPath id="hood-open"><path clip-rule="evenodd" d="M-500 -500 H700 V700 H-500 Z M${f1(fx - frx)} ${f1(fcy)} a${f1(frx)} ${f1(fry)} 0 1 0 ${f1(2 * frx)} 0 a${f1(frx)} ${f1(fry)} 0 1 0 ${f1(-2 * frx)} 0 Z"/></clipPath>` +
              `<clipPath id="hood-whole"><ellipse ${HOOD}/></clipPath></defs>` +
              `<ellipse ${HOOD} fill="${fill}" stroke="${line}" stroke-width="4" clip-path="url(#hood-open)"/>` +
              `<ellipse ${hole} fill="none" stroke="${line}" stroke-width="4" clip-path="url(#hood-whole)"/>`;
  // (on the brim, the animal's face on our side only, a little to the front)
  // (ticket 499: a dragon's, drawn for the side: its brow, its slit eye, a nostril; and its teeth along the top of the opening)
  const all = kind === "dragon" ? "" : hoodFront(C, kind, F), cut = all.indexOf("/>", all.indexOf("<line ")) + 2;
  const features = kind === "dragon" ? dragonBrimSide(C, dir) : all.slice(cut);
  const teeth = kind === "dragon" ? dragonTeethSide(C, fx, fcy, frx, fry, dir, cy, ry) : "";
  return cap + teeth + (kind === "sasquatch" ? sasquatchTop(C, F, dir, false) : "") + `<defs><clipPath id="hood-near"><rect x="${dir > 0 ? 100 : -4000}" y="-4000" width="4100" height="8000"/></clipPath></defs>` +
         `<g transform="translate(${f1(dir * 12)} 0)"><g clip-path="url(#hood-near)">${features}</g></g>`;
}
function hoodFront(C, kind, F) {
  const HOOD = hoodEllipse(F);
  const brimFill = falconHead(kind) ? C.marks : eagleHead(kind) ? C.belly : ANIMALS[kind].suit ? "url(#pat-fur)" : C.fur;
  let s = `<ellipse ${HOOD} fill="${brimFill}" stroke="${darker(C.fur)}" stroke-width="4" clip-path="url(#hood-brim)"/>` +
          `<line x1="0" y1="54" x2="200" y2="54" stroke="${darker(C.fur)}" stroke-width="4" clip-path="url(#hood-inside)"/>`;
  const shine = (x, y, r) => `<circle cx="${x - r * 0.35}" cy="${y - r * 0.35}" r="${r * 0.28}" fill="#ffffff"/>`;
  // (ticket 536) a moth's: a fluffy tuft between two big round glossy eyes (the animal eye colour), and its tongue curled up
  // under them; no nose, whiskers or claws (its antennae and crown are behind the hood: mothHoodBack)
  if (kind === "moth") {
    const ln = darker(C.fur), tuft = mix(C.belly, C.fur, 0.25);
    s += `<path data-moth="tuft" d="M90 20 Q86 10 94 8 Q98 0 106 6 Q115 9 110 20 Q106 29 100 27 Q94 29 90 20 Z" fill="${tuft}" stroke="${mix(tuft, "#000000", 0.25)}" stroke-width="2" stroke-dasharray="3 2"/>`;
    for (const x of [72, 128])
      s += `<circle cx="${x}" cy="28" r="17" fill="${C.eyes}" stroke="${ln}" stroke-width="2.5"/>` + shine(x, 28, 17) +
           `<circle cx="${x + 6}" cy="35" r="2.6" fill="#ffffff" opacity="0.7"/>`;
    return s + `<path data-moth="tongue" d="M100 31 C100 44 112 48 113 40 C114 33 104 33 105 39" fill="none" stroke="${mix(C.accent, "#000000", 0.25)}" stroke-width="3.4" stroke-linecap="round"/>`;
  }
  // (ticket 499) a dragon's: a brow ridge over each slit-pupilled eye, two nostrils (no nose or whiskers), and its teeth along
  // the brim's edge, the top of the hood's opening, pointing down at the face
  if (kind === "dragon") {
    const ln = darker(C.fur);
    for (const [x, sd] of [[78, -1], [122, 1]])
      s += `<path d="M${x - sd * 16} 14 Q${x} 3 ${x + sd * 18} 13" fill="none" stroke="${ln}" stroke-width="4" stroke-linecap="round"/>` +
           `<ellipse cx="${x}" cy="24" rx="9" ry="8" fill="${C.eyes}" stroke="#111" stroke-width="2"/><ellipse cx="${x}" cy="24" rx="1.8" ry="6.6" fill="#111"/>` + shine(x, 24, 8);
    s += [-1, 1].map(sd => `<ellipse cx="${100 + sd * 9}" cy="42" rx="4.4" ry="2.7" transform="rotate(${sd * 20} ${100 + sd * 9} 42)" fill="${ln}"/>`).join("");
    let d = "";
    for (let i = 0; i < 12; i++) { const x = 25 + i * 150 / 11; d += `M${f1(x - 5)} 53 L${f1(x)} 67 L${f1(x + 5)} 53 Z`; }
    return s + `<g clip-path="url(#hood-inside)"><path data-dragon="teeth" d="${d}" fill="${C.horn}" stroke="${darker(C.horn)}" stroke-width="1.8" stroke-linejoin="round"/></g>`;
  }
  // (ticket 530) a Sasquatch's: its shaggy crown and its extras (before the brim's line, so the profile, which keeps what
  // follows the line, draws its own: sasquatchTop), a shaggy brow over big round friendly eyes, a big muzzle, a wide flat
  // nose, and its grin, two square teeth (or a plain smile); no whiskers or claws
  if (kind === "sasquatch") {
    const cut = s.indexOf("<line ");
    s = s.slice(0, cut) + sasquatchTop(C, F, 0, false) + s.slice(cut);
    const ln = darker(C.fur);
    s += `<path d="M58 16 Q66 2 80 8 Q90 2 100 10 Q110 2 120 8 Q134 2 142 16 Q130 10 120 14 Q110 9 100 16 Q90 9 80 14 Q70 10 58 16 Z" fill="${C.marks}" stroke="${ln}" stroke-width="2" stroke-linejoin="round"/>`;
    for (const x of [79, 121])
      s += `<circle cx="${x}" cy="26" r="10" fill="#ffffff" stroke="#111" stroke-width="2"/><circle cx="${x}" cy="27" r="5" fill="${C.eyes}"/>` + shine(x, 26, 9);
    s += `<ellipse cx="100" cy="44" rx="26" ry="12" fill="${C.belly}" stroke="${mix(C.belly, "#000000", 0.25)}" stroke-width="1.5"/>` +
         `<path d="M88 37 Q100 31 112 37 Q110 43 100 43 Q90 43 88 37 Z" fill="#3a2418"/>`;
    s += C.sasquatchGrin !== false
      ? `<path d="M84 45 Q100 45 116 45 Q114 55 100 55 Q86 55 84 45 Z" fill="#7a2e2e" stroke="#3a2418" stroke-width="1.6" stroke-linejoin="round"/>` +
        `<path d="M93 45.5 h6 v4.5 h-6 Z M101 45.5 h6 v4.5 h-6 Z" fill="#ffffff" stroke="#3a2418" stroke-width="0.8"/>`
      : `<path d="M90 48 Q100 53 110 48" fill="none" stroke="#3a2418" stroke-width="2.2" stroke-linecap="round"/>`;
    return s;
  }
  // (ticket 544) a giraffe's: the brim plain in its coat, the foot of its neck; its face is its own head's, up the neck
  // (giraffeNeckSvg)
  if (kind === "giraffe") return s;
  if (falconHead(kind)) {
    for (const x of [76, 124])
      s += `<circle cx="${x}" cy="26" r="12" fill="${C.accent}"/>` +
           `<circle cx="${x}" cy="26" r="8" fill="${C.eyes}"/>` + shine(x, 26, 8);
    s += `<path d="M92 31 Q100 26 108 31 L107 36 L93 36 Z" fill="${C.accent}"/>` +
         `<path d="M93 36 L107 36 Q109 47 100 58 Q101 47 93 36 Z" fill="#59636e" stroke="#2a2f35" stroke-width="1.5"/>`;
    return s;
  }
  if (eagleHead(kind)) {
    // a stern brow, yellow eyes, and a big hooked yellow beak
    for (const [x, side] of [[78, -1], [122, 1]])
      s += `<circle cx="${x}" cy="26" r="9" fill="${C.eyes}" stroke="#4a3a20" stroke-width="2"/>` +
           `<circle cx="${x}" cy="26" r="4" fill="#111"/>` + shine(x, 26, 6) +
           `<path d="M${x - side * 14} 14 L${x + side * 10} 19" stroke="#8a7a60" stroke-width="3" stroke-linecap="round"/>`;
    s += `<path d="M86 34 Q100 26 114 34 Q116 50 104 62 Q100 64 98 58 Q104 52 100 46 Q93 44 86 34 Z" ` +
         `fill="${C.accent}" stroke="${darker(C.accent)}" stroke-width="2" stroke-linejoin="round"/>`;
    return s;
  }
  if (kind === "raccoon")  // the bandit mask
    s += `<path d="M58 22 Q66 10 86 16 Q100 22 114 16 Q134 10 142 22 Q138 38 120 36 Q106 34 100 30 ` +
         `Q94 34 80 36 Q62 38 58 22 Z" fill="${C.marks}"/>`;
  if (kind === "cheetah")  // the cheetah's black "tear" lines, eyes to mouth
    s += `<path d="M84 31 Q83 40 89 48 M116 31 Q117 40 111 48" stroke="${C.marks}" stroke-width="3.2" ` +
         `fill="none" stroke-linecap="round"/>`;
  if (kind === "leopard")  // a few small spots on the brow
    s += [[86, 6, 2.6], [100, 2, 3], [114, 6, 2.6], [70, 12, 2.2], [130, 12, 2.2]].map(([x, y, r]) =>
      `<circle cx="${x}" cy="${y}" r="${r}" fill="${C.marks}"/>`).join("");
  if (kind === "tiger")
    s += `<path d="M90 2 L100 14 L110 2 M66 10 L80 18 M134 10 L120 18 M60 24 L74 28 M140 24 L126 28" ` +
         `stroke="${C.marks}" stroke-width="4" stroke-linecap="round" fill="none"/>`;
  for (const x of [78, 122]) {
    s += `<ellipse cx="${x}" cy="24" rx="9" ry="8" fill="${C.eyes}" stroke="${kind === "raccoon" ? "#cfcfcf" : "#111"}" stroke-width="2"/>`;
    s += kind === "cat" ? `<ellipse cx="${x}" cy="24" rx="2.2" ry="7" fill="#111"/>`
                        : `<circle cx="${x}" cy="24" r="3.6" fill="#111"/>`;
    s += shine(x, 24, 8);
  }
  if (kind === "bear")
    s += `<ellipse cx="100" cy="42" rx="18" ry="11" fill="${C.belly}"/>`;
  else
    s += `<circle cx="92" cy="44" r="8" fill="${C.belly}"/><circle cx="108" cy="44" r="8" fill="${C.belly}"/>`;
  s += FELINE[kind]
    ? `<path d="M94 35 L106 35 L100 42 Z" fill="${C.accent}"/>`
    : `<path d="M91 34 Q100 31 109 34 Q105 43 100 44 Q95 43 91 34 Z" fill="${C.accent}"/>`;
  if (kind !== "bear" && kind !== "deer") {
    const wc = kind === "cat" || kind === "raccoon" || kind === "jaguar" ? "#e8e8e8" : "#222";
    s += `<path d="M86 44 L60 40 M86 47 L60 50 M114 44 L140 40 M114 47 L140 50" stroke="${wc}" stroke-width="1.6" stroke-linecap="round"/>`;
  }
  return s;
}

// THE SUIT'S EXTRAS, in body units: a tail out to the side (drawn behind
// the body), and the falcon's wings and fanned tail.
function tailSvg(C, kind, hw, L, floor = Infinity, lie = null) {
  if (kind === "moth") return "";   // (ticket 536: a moth has no tail; a luna's long tails are on its hindwings)
  const line = darker(C.fur), x0 = hw * 0.55, y0 = L + 5;
  if (kind === "bear")
    return `<circle cx="${f1(hw + 4)}" cy="${f1(L - 14)}" r="17" fill="url(#pat-fur)" stroke="${line}" stroke-width="3"/>`;
  if (kind === "sasquatch")   // (ticket 530) the bear's stub, shaggy round its edge
    return `<circle cx="${f1(hw + 4)}" cy="${f1(L - 14)}" r="17" fill="url(#pat-fur)" stroke="${line}" stroke-width="8" stroke-dasharray="4 3"/>`;
  if (kind === "giraffe") {   // (ticket 544) a thin tail swinging out and down, its long dark tuft hanging from the end to the
    // ground at most (sitting, it reaches out along it instead); lying down (lie: the side of the body on the ground, and the
    // legs' length), along the ground by the legs, the tuft toward the feet
    const drop = clamp(floor - 50 - L, 20, 100), sd = lie ? lie.side : 1, P = (x, y) => f1(sd * x) + " " + f1(y);
    const [ex, ey] = lie ? [hw + 6, L + 0.55 * lie.LL] : [hw + 34 + 0.6 * (100 - drop), L + drop];
    const d = lie ? `M${P(x0, L - 4)} C${P(hw + 10, L + 10)} ${P(hw + 8, L + 0.3 * lie.LL)} ${P(ex, ey)}` : `M${P(x0, L - 4)} C${P(hw + 26, L + 4)} ${P(hw + 40 + 0.3 * (100 - drop), L + drop / 2)} ${P(ex, ey)}`;
    return `<path data-giraffe="tail" d="${d}" fill="none" stroke="${line}" stroke-width="9" stroke-linecap="round"/><path d="${d}" fill="none" stroke="${C.fur}" stroke-width="5" stroke-linecap="round"/>` +
           `<path d="M${P(ex - 3, ey - 4)} C${P(ex - 13, ey + 8)} ${P(ex - 14, ey + 28)} ${P(ex - 9, ey + 42)} L${P(ex - 4, ey + 36)} L${P(ex, ey + 49)} L${P(ex + 4, ey + 36)} L${P(ex + 9, ey + 42)} ` +
           `C${P(ex + 14, ey + 28)} ${P(ex + 13, ey + 8)} ${P(ex + 3, ey - 4)} Z" fill="${C.marks}" stroke="${darker(C.marks)}" stroke-width="2" stroke-linejoin="round"/>`;
  }
  if (kind === "deer")  // a short white tail, flicked up
    return `<ellipse cx="${f1(hw + 8)}" cy="${f1(L - 16)}" rx="11" ry="17" transform="rotate(35 ${f1(hw + 8)} ${f1(L - 16)})" ` +
           `fill="${C.belly}" stroke="${line}" stroke-width="3"/>`;
  if (kind === "bobcat")  // a short stubby tail with a black tip
    return `<ellipse cx="${f1(hw + 10)}" cy="${f1(L - 12)}" rx="13" ry="20" transform="rotate(30 ${f1(hw + 10)} ${f1(L - 12)})" ` +
           `fill="url(#pat-fur)" stroke="${line}" stroke-width="3"/>` +
           `<ellipse cx="${f1(hw + 19)}" cy="${f1(L - 27)}" rx="7" ry="8" transform="rotate(30 ${f1(hw + 19)} ${f1(L - 27)})" fill="${C.marks}"/>`;
  const d = kind === "cat" || kind === "leopard" || kind === "cheetah" || kind === "jaguar"
    ? `M${f1(x0)} ${f1(y0)} C${f1(hw + 80)} ${f1(L + 30)} ${f1(hw + 110)} ${f1(L - 30)} ${f1(hw + 70)} ${f1(L - 110)}`
    : `M${f1(x0)} ${f1(y0)} C${f1(hw + 70)} ${f1(L + 20)} ${f1(hw + 95)} ${f1(L + 90)} ${f1(hw + 60)} ${f1(L + 150)}`;
  const wd = { tiger: 16, cat: 11, lion: 10, raccoon: 22, leopard: 13, cheetah: 12, jaguar: 14, wolf: 26 }[kind];
  const path = (stroke, width, extra = "") =>
    `<path d="${d}" fill="none" stroke="${stroke}" stroke-width="${width}" stroke-linecap="round" ${extra}/>`;
  let s = path(line, wd + 5) + path(C.fur, wd);
  if (kind === "tiger") s += path(C.marks, wd, `stroke-dasharray="7 11"`);
  if (kind === "raccoon") s += path(C.marks, wd, `stroke-dasharray="11 11"`);   // the rings
  if (kind === "leopard" || kind === "cheetah" || kind === "jaguar") s += path(C.marks, wd * 0.45, `stroke-dasharray="1 13"`);  // spots
  if (kind === "lion")
    s += `<circle cx="${f1(hw + 60)}" cy="${f1(L + 156)}" r="14" fill="${C.marks}" stroke="${darker(C.marks)}" stroke-width="2"/>`;
  if (kind === "wolf")  // the bushy tail's dark tip
    s += `<path d="${d}" fill="none" stroke="${C.marks}" stroke-width="${wd}" stroke-linecap="round" pathLength="100" ` +
         `stroke-dasharray="0 78 22"/>`;
  return s;
}
// A falcon suit's wing, built on one arm: the arm runs along its leading
// edge, from the shoulder A to the hand H, and flight feathers trail on
// the side that faces down, longest in the middle, with long primaries
// past the hand. It moves with the arm, as the arms slider moves it.
function armWing(C, { A, H, side }, armW, edge = 1) {
  const len = Math.hypot(H[0] - A[0], H[1] - A[1]) || 1;
  const ux = (H[0] - A[0]) / len, uy = (H[1] - A[1]) / len;
  // the trailing side: whichever perpendicular points more downward (or,
  // for a level arm, away from the body)
  let nx = -uy, ny = ux;
  if (ny < -0.15 || (Math.abs(ny) <= 0.15 && nx * side < 0)) { nx = -nx; ny = -ny; }
  const Q = (t, d) => [A[0] + ux * t + nx * d, A[1] + uy * t + ny * d];
  // (ticket 124: seen from the side, `edge` below 1, the wing is turned
  // nearly edge on: its feathers' depth and its reach past the hand less)
  const lead = armW / 2 + 6, ext = len * 0.75 * (edge < 1 ? 0.55 : 1), total = len + ext, D = (len * 0.55 + 26) * edge;
  const depth = t => 12 * Math.max(edge, 0.5) + D * Math.pow(Math.sin(Math.PI * Math.min(1, t / total)), 0.8);
  const k = 8, feathers = [];
  for (let i = 0; i <= k; i++) {
    const t = total * (1 - i / k) * 0.97;
    feathers.push(Q(t, depth(t)), Q(Math.max(0, t - total / k / 2), depth(t) * 0.78));
  }
  const outline = [Q(-4, -lead), Q(len * 0.55, -lead - 4), Q(len, -lead + 1), Q(total, 2), ...feathers, Q(0, lead + 8)];
  let s = poly(outline, "url(#pat-fur)", darker(C.fur), 3);
  // the coverts along the arm, darker, and the flight feathers' shafts
  s += poly([Q(-2, -lead + 2), Q(len * 0.55, -lead - 1), Q(len, -lead + 3), Q(len + ext * 0.35, 4), Q(len, lead + 6),
             Q(0, lead + 6)], mix(C.fur, C.marks, 0.45), "none", 0);
  for (let i = 0; i < k; i++) {
    const t = total * (1 - i / k) * 0.97;
    const [x1, y1] = Q(Math.min(t, len), lead + 4), [x2, y2] = Q(t, depth(t) * 0.92);
    s += `<line x1="${f1(x1)}" y1="${f1(y1)}" x2="${f1(x2)}" y2="${f1(y2)}" stroke="${C.marks}" stroke-width="2" stroke-linecap="round"/>`;
  }
  return s;
}
function falconWings(C, w, fill = "url(#pat-bars)") {
  let s = "";
  for (const side of [-1, 1]) {
    const P = [[18, -2], [w + 20, -62], [w + 95, -88], [w + 175, -70], [w + 140, -40], [w + 120, -8],
               [w + 92, 16], [w + 60, 34], [w + 28, 46], [20, 44]].map(([x, y]) => [side * x, y]);
    s += poly(P, fill, darker(C.fur), 3);
    for (const [x1, y1, x2, y2] of [[w + 60, -40, w + 168, -68], [w + 50, -25, w + 138, -40],
                                    [w + 42, -10, w + 118, -8], [w + 34, 6, w + 90, 16], [w + 26, 20, w + 58, 34]])
      s += `<line x1="${side * x1}" y1="${y1}" x2="${side * x2}" y2="${y2}" stroke="${C.marks}" stroke-width="2.5" stroke-linecap="round"/>`;
  }
  return s;
}
function falconTail(C, hw, L, LL, fill = "url(#pat-bars)") {
  const tl = L + LL * 0.62;
  const fan = [[-hw * 0.45, L - 20], [-(hw + 28), tl - 10], [-hw * 0.5, tl + 8], [0, tl + 12],
               [hw * 0.5, tl + 8], [hw + 28, tl - 10], [hw * 0.45, L - 20]];
  return poly(fan, fill, darker(fill.startsWith("url") ? C.fur : fill), 3) +
    `<polyline points="${pts(fan.slice(1, 6))}" fill="none" stroke="${C.belly}" stroke-width="6" stroke-linejoin="round"/>`;
}

// (ticket 536, Matthew: "moth costume, 3 styles. Follow the bird costume examples for wing ideas, but we want moth wings
// here, and antenna, etc") THE MOTH, a suit as the bird suits are: a moth's wings on its arms, each built on one arm as
// armWing builds a falcon suit's (the arm along the forewing's leading edge, from the shoulder A to the hand H and on to the
// wing's round tip; the wing out on the arm's trailing side by armWing's rule, so it lies along the side as the arm is
// lowered and spreads as it is raised, and in profile, `edge` 0.3, it is turned nearly edge on); feathery antennae and a
// fuzzy crown behind its hood (mothHoodBack), its face on the brim (hoodFront), a fluffy collar (mothCollar) and a banded
// belly (mothBands); in its style's colours (mothOf: MOTH_STYLES, js/01-state.js, and the style's chosen wing colour). Each
// piece is a group marked data-moth, nothing in one turned.
const mothStyleOf = c => MOTH_STYLES[c.mothStyle] ? c.mothStyle : "luna";
const mothOf = c => { const id = mothStyleOf(c), S = MOTH_STYLES[id], wing = c["mothColor_" + id] || S.colors[0];
  return { ...S, id, wing, hind: S.hind || wing, spots: c.mothSpots !== false }; };
// a closed smooth path through points (each piece a cubic from its neighbours, as smoothPath's, round the loop)
const mothLoop = P => { const n = P.length, p = i => P[(i + n) % n];
  let d = `M${f1(P[0][0])} ${f1(P[0][1])}`;
  for (let i = 0; i < n; i++) { const a = p(i - 1), b = p(i), c = p(i + 1), e = p(i + 2);
    d += ` C${f1(b[0] + (c[0] - a[0]) / 6)} ${f1(b[1] + (c[1] - a[1]) / 6)} ${f1(c[0] - (e[0] - b[0]) / 6)} ${f1(c[1] - (e[1] - b[1]) / 6)} ${f1(c[0])} ${f1(c[1])}`; }
  return d + " Z"; };
// One wing's shape on its arm, in body units: Q(t, d), t along the arm from the shoulder and d out on its trailing side
// (tipped toward the ground by MOTH_DROP, so a lowered arm's wing hangs down along the side rather than across the body);
// `fore`, the forewing, broad and round-tipped past the hand; `hind`, the hindwing, rounder, out from under it; `tail`, a
// luna's long tail hanging from the hindwing's edge (nearly straight down, a little curl, a broad tip). Their
// points also make the picture's frame (drawNow).
const MOTH_DROP = 0.6;
function mothWingGeom({ A, H, side }, armW, edge, M) {
  const len = Math.hypot(H[0] - A[0], H[1] - A[1]) || 1, ux = (H[0] - A[0]) / len, uy = (H[1] - A[1]) / len;
  let nx = -uy, ny = ux;
  if (ny < -0.15 || (Math.abs(ny) <= 0.15 && nx * side < 0)) { nx = -nx; ny = -ny; }
  const gl = Math.hypot(nx, ny + MOTH_DROP), gx = nx / gl, gy = (ny + MOTH_DROP) / gl;
  const Q = (t, d) => [A[0] + ux * t + gx * d, A[1] + uy * t + gy * d];
  const L = len, ext = L * 0.3 * (edge < 1 ? 0.6 : 1), lead = armW / 2 + 5, D = (L * 0.6 + 26) * edge + 8 * (1 - edge);
  const fore = [[-4, -lead + 2], [L * 0.5, -lead - 3], [L * 0.98, -lead - 1], [L + ext, 0], [L + ext * 0.8, D * 0.42], [L, D * 0.86],
                [L * 0.78, D], [L * 0.45, D * 0.62], [L * 0.12, D * 0.3], [-2, lead + 2]];
  const hind = [[L * 0.08, lead * 0.6], [L * 0.5, D * 0.5], [L * 0.82, D * 0.9], [L * 0.76, D * 1.2], [L * 0.55, D * 1.32],
                [L * 0.3, D * 1.12], [L * 0.1, D * 0.75], [-2, lead + 4]];
  let tail = null;
  if (M.tail) {
    const [bx, by] = Q(L * 0.66, D * 1.22), tl = Math.hypot(gx * 0.3, gy * 0.3 + 1), vx = gx * 0.3 / tl, vy = (gy * 0.3 + 1) / tl, px = -vy, py = vx;
    const T = L * 0.72 * Math.max(edge, 0.6), c = s => [bx + vx * T * s + px * 7 * Math.sin(Math.PI * s) * side, by + vy * T * s + py * 7 * Math.sin(Math.PI * s) * side];
    const wd = s => s > 0.85 ? 6.5 : 8 * (1 - s) + 3, S = [0, 0.35, 0.7, 0.92, 1];
    tail = [...S.map(s => { const [x, y] = c(s); return [x + px * wd(s), y + py * wd(s)]; }),
            ...S.slice().reverse().map(s => { const [x, y] = c(s); return [x - px * wd(s), y - py * wd(s)]; })];
  }
  return { Q, L, D, lead, ext, fore: fore.map(([t, d]) => Q(t, d)), hind: hind.map(([t, d]) => Q(t, d)), tail, hindTD: hind };
}
const mothWingPts = (arm, armW, edge, M) => { const G = mothWingGeom(arm, armW, edge, M); return [...G.fore, ...G.hind, ...(G.tail || [])]; };
// an eyespot at Q(t, d), r across: its rim and ring one shape (a stroke round it), its middle, a shine; its axes along the
// arm and across it, squashed as the wing is edge on (an ellipse drawn as two arcs, nothing turned)
function mothSpot(G, t, d, r, [rim, ring, mid], edge, what = "spot") {
  const [cx, cy] = G.Q(t, d), [ax, ay] = G.Q(t + 1, d), a = Math.atan2(ay - cy, ax - cx), deg = f1(a * 180 / Math.PI), k = Math.max(edge, 0.35);
  const ell = (rr, fill, extra = "") => { const dx = Math.cos(a) * rr, dy = Math.sin(a) * rr;
    return `<path d="M${f1(cx - dx)} ${f1(cy - dy)} A${f1(rr)} ${f1(rr * k)} ${deg} 1 0 ${f1(cx + dx)} ${f1(cy + dy)} A${f1(rr)} ${f1(rr * k)} ${deg} 1 0 ${f1(cx - dx)} ${f1(cy - dy)} Z" fill="${fill}"${extra}/>`; };
  return ell(r * 0.85, ring, ` data-moth="${what}" stroke="${rim}" stroke-width="${f1(r * 0.3)}"`) + ell(r * 0.42, mid) +
         `<circle cx="${f1(cx - r * 0.2)}" cy="${f1(cy - r * 0.2 * k)}" r="${f1(Math.max(1.2, r * 0.14))}" fill="#ffffff" opacity="0.85"/>`;
}
// a band across a wing, from a to b ([t, d] each), bowing toward the wing's tip by `bow`: wavy, or zigzag
function mothBand(G, a, b, bow, [col, w, zig]) {
  const P = [];
  for (let i = 0; i <= 8; i++) { const s = i / 8;
    P.push(G.Q(a[0] + (b[0] - a[0]) * s + bow * Math.sin(Math.PI * s) + (zig ? (i % 2 ? 4 : -4) : 2.5 * Math.sin(s * 9)), a[1] + (b[1] - a[1]) * s)); }
  return `<path d="${zig ? "M" + P.map(p => f1(p[0]) + " " + f1(p[1])).join(" L") : smoothPath(P)}" fill="none" stroke="${col}" stroke-width="${w}" stroke-linecap="round" stroke-linejoin="round"/>`;
}
// One wing (M: mothOf), behind to front: the luna's tail, the hindwing (its soft fringe, its band along its edge, its
// eyespot), then the forewing (its fringe, its veins, the band along its leading edge, its bands across, its eyespot or a
// night moth's kidney ring, its dusting of scales). `far`: in profile, the wing on the far arm, a shade darker and plain
// (its markings would crowd the near wing's, just in front of it)
function mothWing(C, arm, armW, edge, M, far = false) {
  if (far) { const dk = c => mix(c, "#000000", 0.2); M = { ...M, wing: dk(M.wing), hind: dk(M.hind), costa: dk(M.costa), spots: false, bands: [], hindBand: null, kidney: false, dust: null }; }
  const G = mothWingGeom(arm, armW, edge, M), { L, D, lead, ext } = G, Q = G.Q;
  const W = M.wing, HW = M.hind, wl = mix(W, "#000000", 0.42), hl = mix(HW, "#000000", 0.42);
  const fringe = (d, col) => `<path d="${d}" fill="none" stroke="${col}" stroke-width="7" stroke-dasharray="1.6 2.2" stroke-linecap="round"/>`;
  const hd = mothLoop(G.hind), fd = mothLoop(G.fore), P = p => f1(p[0]) + " " + f1(p[1]);
  let s = G.tail ? `<path data-moth="tail" d="${mothLoop(G.tail)}" fill="${HW}" stroke="${hl}" stroke-width="2"/>` : "";
  s += fringe(hd, mix(HW, "#ffffff", 0.4)) + `<path d="${hd}" fill="${HW}" stroke="${hl}" stroke-width="2.2"/>`;
  if (M.hindBand) { const c = [L * 0.4, D * 0.85], in_ = ([t, d]) => G.Q(c[0] + (t - c[0]) * 0.86, c[1] + (d - c[1]) * 0.86);
    s += `<path d="${smoothPath(G.hindTD.slice(2, 7).map(in_))}" fill="none" stroke="${M.hindBand[0]}" stroke-width="${M.hindBand[1]}" stroke-linecap="round"/>`; }
  if (M.spots && M.spot[3]) s += mothSpot(G, L * 0.5, D * 0.98, M.spot[3], M.spot, edge);
  s += fringe(fd, mix(W, "#ffffff", 0.4)) + `<path d="${fd}" fill="${W}" stroke="${wl}" stroke-width="2.2"/>`;
  const base = Q(L * 0.04, 0);
  s += `<path d="${[Q(L + ext * 0.7, D * 0.2), Q(L * 0.98, D * 0.62), Q(L * 0.74, D * 0.9), Q(L * 0.42, D * 0.58)].map(p => `M${P(base)} L${P(p)}`).join(" ")}" fill="none" stroke="${wl}" stroke-width="1.2" opacity="0.35"/>`;
  s += `<path d="${smoothPath([Q(0, -lead + 5.5), Q(L * 0.5, -lead + 2.5), Q(L * 0.98, -lead + 3.5), Q(L + ext * 0.85, 2)])}" fill="none" stroke="${M.costa}" stroke-width="6" stroke-linecap="round"/>`;
  const [b1, b2] = M.bands;
  if (b1) s += mothBand(G, [L * 0.82, -lead + 6], [L * 0.68, D * 0.86], 6, b1);
  if (b2) s += mothBand(G, [L * 0.36, -lead + 6], [L * 0.24, D * 0.5], 4, b2);
  if (M.kidney) s += mothSpot(G, L * 0.62, D * 0.3, 8, [M.costa, W, mix(W, M.costa, 0.35)], edge, "kidney");
  else if (M.spots && M.foreSpot) s += mothSpot(G, L * 0.62, D * 0.42, M.foreSpot, M.spot, edge);
  if (M.dust) s += `<path d="M${P(Q(L * 0.28, D * 0.15))} L${P(Q(L * 1.02, D * 0.3))} M${P(Q(L * 0.22, D * 0.42))} L${P(Q(L * 0.92, D * 0.7))} M${P(Q(L * 0.5, D * 0.72))} L${P(Q(L * 0.84, D * 0.92))}" ` +
                  `fill="none" stroke="${M.dust}" stroke-width="2.6" stroke-dasharray="0.1 7" stroke-linecap="round" opacity="0.55"/>`;
  return `<g data-moth="wing">${s}</g>`;
}
// (ticket 536) Behind the hood, in the round head's units: the fuzzy crown along its top (round puffs, the dots of one
// thick stroke with round ends, on a darker one for their edge, the hood's own shape over half of them) and the two feathery antennae rising from it, each a curved shaft,
// its barbs in one path and a pale vane behind them; toward us and from behind a V, out to either side; in profile (dir)
// both rising and curving forward, the far one behind, a shade darker. MOTH_ANT_UP: how far above the hood's top they
// reach (the picture's frame, drawNow).
const MOTH_ANT_UP = 84;
function mothHoodBack(C, F, dir, away) {
  const { cy, rx, ry } = hoodGeom(F), top = cy - ry, M = mothOf(C), P = (x, y) => f1(x) + " " + f1(y);
  const pt = a => [100 + (rx + 3) * Math.cos(a * Math.PI / 180), cy + (ry + 3) * Math.sin(a * Math.PI / 180)];
  const crown = `M${P(...pt(200))} A${f1(rx + 3)} ${f1(ry + 3)} 0 0 1 ${P(...pt(340))}`, fz = mix(C.fur, C.belly, 0.5);
  let s = `<path data-moth="crown" d="${crown}" fill="none" stroke="${mix(fz, "#000000", 0.3)}" stroke-width="16" stroke-dasharray="0.1 9" stroke-linecap="round"/>` +
          `<path d="${crown}" fill="none" stroke="${fz}" stroke-width="12.5" stroke-dasharray="0.1 9" stroke-linecap="round"/>`;
  // each antenna [base, bend, tip], a quadratic curve
  const ants = dir ? [[[100 - dir * 2, top + 16], [100 - dir * 8, top - 46], [100 + dir * 28, top - MOTH_ANT_UP + 12]],
                      [[100 + dir * 14, top + 16], [100 + dir * 8, top - 50], [100 + dir * 52, top - MOTH_ANT_UP + 4]]]
                   : [-1, 1].map(sd => [[100 + sd * 22, top + 16], [100 + sd * 26, top - 48], [100 + sd * 66, top - MOTH_ANT_UP + 6]]);
  const one = ([B, K, T], col) => {
    const at = t => [(1 - t) * (1 - t) * B[0] + 2 * (1 - t) * t * K[0] + t * t * T[0], (1 - t) * (1 - t) * B[1] + 2 * (1 - t) * t * K[1] + t * t * T[1]];
    const tan = t => { const x = 2 * (1 - t) * (K[0] - B[0]) + 2 * t * (T[0] - K[0]), y = 2 * (1 - t) * (K[1] - B[1]) + 2 * t * (T[1] - K[1]), l = Math.hypot(x, y) || 1; return [x / l, y / l]; };
    let barbs = ""; const L = [], R = [];
    for (let k = 0; k <= 10; k++) { const t = 0.32 + 0.66 * k / 10, [x, y] = at(t), [tx, ty] = tan(t), len = 3 + 11 * Math.pow(Math.sin(Math.PI * (t - 0.26) / 0.76), 0.7);
      for (const sd of [1, -1]) { const bx = (-ty * sd * 0.85 + tx * 0.5) * len, by = (tx * sd * 0.85 + ty * 0.5) * len;
        barbs += `M${P(x, y)} l${P(bx, by)} `; (sd > 0 ? L : R).push([x + bx, y + by]); } }
    return `<path d="${mothLoop([...L, ...R.reverse()])}" fill="${col}" opacity="0.3"/>` +
           `<path d="${barbs}" fill="none" stroke="${col}" stroke-width="2" stroke-linecap="round"/>` +
           `<path d="M${P(...B)} Q${P(...K)} ${P(...T)}" fill="none" stroke="${mix(col, "#000000", 0.25)}" stroke-width="3.2" stroke-linecap="round"/>`;
  };
  s += `<g data-moth="antennae">${ants.map((a, i) => one(a, dir && !i ? mix(M.antenna, "#000000", 0.25) : M.antenna)).join("")}</g>`;
  return s;
}
// (ticket 536) A moth's fluffy collar round the neck, over the wings' roots, in body units (the collar's line at y -20, nw
// its half width): a ring of round tufts with a dashed fuzzy edge; narrower in profile
function mothCollar(C, nw, dir) {
  const rx = dir ? nw + 10 : nw + 20, ry = 15, cy = -6, n = 14, fz = mix(C.belly, C.fur, 0.2);
  const at = (a, k) => [rx * k * Math.cos(a), cy + ry * k * Math.sin(a)];
  let d = `M${f1(at(0, 1)[0])} ${f1(at(0, 1)[1])}`;
  for (let i = 0; i < n; i++) { const a = 2 * Math.PI * (i + 0.5) / n, b = 2 * Math.PI * (i + 1) / n, [qx, qy] = at(a, 1.22), [x, y] = at(b, 1);
    d += ` Q${f1(qx)} ${f1(qy)} ${f1(x)} ${f1(y)}`; }
  return `<g data-moth="collar"><path d="${d} Z" fill="${fz}" stroke="${mix(fz, "#000000", 0.3)}" stroke-width="2.4" stroke-dasharray="2.5 2" stroke-linejoin="round"/></g>`;
}
// (ticket 536) Fuzzy bands across a moth's belly (toward us) or its back (from behind), in the markings colour: four, each
// bowed a little, inside the oval cx, cy, rx, ry, in one path
function mothBands(C, cx, cy, rx, ry) {
  const d = [-0.55, -0.18, 0.2, 0.56].map(k => { const y = cy + k * ry, h = rx * Math.sqrt(1 - k * k) * 0.88;
    return `M${f1(cx - h)} ${f1(y)} Q${f1(cx)} ${f1(y + 7)} ${f1(cx + h)} ${f1(y)}`; }).join(" ");
  return `<g data-moth="bands"><path d="${d}" fill="none" stroke="${C.marks}" stroke-width="6" stroke-linecap="round" stroke-dasharray="3 1.6" opacity="0.7"/></g>`;
}

// (ticket 499, Matthew: "new costume to add -- dragon." "look at Lego castles and knights for inspiration." "wings, tail and
// optional dragon breath"; and on the mock-up, mockups/dragon-fire.html: "dragon tail spikes should be facing upward, not
// downward :)") THE DRAGON, a suit as the wolf's is, with a dragon's pieces: on the hood two horns sweeping back, a row of
// spines down its back, teeth along the top of its opening, slit-pupilled eyes and nostrils (dragonHoodBack, hoodFront,
// dragonBrimSide, dragonTeethSide, dragonHoodSpines); scales for fur (furDefs); a plated belly (dragonPlates); claws on the
// paws and feet; bat wings behind the shoulders (dragonWingsSvg: flapping, open and still, or folded); a long tapering tail,
// its spikes standing up along its top, a spade at its tip (dragonTailSvg). Each view (toward us, from behind, from the side)
// is a drawing of its own; our left and right mirror each other. The breath is another ticket's.
// Behind the hood, in the round head's units (dy: how far the hood's top is from the mock-up's, -2, as the head grows):
// toward us its horns rising and curving back, and the first spine of the row showing over the crown; in profile (the mock-
// up's, facing our right; mirrored for our left) the row of spines down the back of the hood, tallest in the middle, and the
// two horns sweeping back, the far one a shade darker; from behind, its horns as they sweep back toward us, their undersides.
function dragonHoodBack(C, F, dir, away) {
  const { cy, ry } = hoodGeom(F), top = cy - ry, dy = top + 2, H = C.horn, HD = darker(H), W = C.wing, WD = mix(W, "#000000", 0.3);
  const up = d => d.replace(/(-?[\d.]+) (-?[\d.]+)/g, (_, x, y) => `${x} ${f1(Number(y) + dy)}`);
  const horn = (d, ridges, fill = H) => `<path data-dragon="horn" d="${up(d)}" fill="${fill}" stroke="${HD}" stroke-width="3.5" stroke-linejoin="round"/>` +
    `<path d="${up(ridges)}" fill="none" stroke="${HD}" stroke-width="2.5" stroke-linecap="round" opacity="0.7"/>`;
  const mirror = t => `<g transform="translate(200 0) scale(-1 1)">${t}</g>`;
  if (dir) {
    let sp = "";
    for (let a = -118; a >= -206; a -= 14.5) {
      const pt = deg => [100 + 106 * Math.cos(deg * Math.PI / 180), cy + ry * Math.sin(deg * Math.PI / 180)];
      const t0 = pt(a + 6), t1 = pt(a - 6), m = pt(a), nx = Math.cos(a * Math.PI / 180) / 106, ny = Math.sin(a * Math.PI / 180) / ry, nn = Math.hypot(nx, ny);
      const h = 30 - Math.abs(a + 150) * 0.12, ap = [m[0] + nx / nn * h, m[1] + ny / nn * h];
      sp += `M${f1(t0[0])} ${f1(t0[1])} L${f1(ap[0])} ${f1(ap[1])} L${f1(t1[0])} ${f1(t1[1])} Z `;
    }
    const s = `<path data-dragon="spines" d="${sp}" fill="${W}" stroke="${WD}" stroke-width="3" stroke-linejoin="round"/>` +
      horn("M58 34 C46 -10 18 -40 -14 -56 C26 -32 76 -2 92 26 Z", "M44 4 Q56 0 66 8 M28 -18 Q38 -22 46 -14", mix(H, "#000000", 0.12)) +
      horn("M110 18 C105 -30 78 -70 48 -95 C88 -62 138 -28 152 14 Z", "M106 -16 Q120 -22 134 -12 M92 -42 Q104 -48 114 -40 M76 -64 Q84 -70 92 -64");
    return dir > 0 ? s : mirror(s);
  }
  if (away) {
    const r = horn("M122 4 C122 -22 138 -44 164 -58 C178 -64 192 -60 198 -50 C186 -46 176 -34 170 -18 C168 -4 168 8 166 20 Z", "M132 -26 Q144 -20 157 -26 M148 -46 Q158 -40 170 -46") +
      `<path d="${up("M198 -50 C186 -46 176 -34 170 -18 C168 -4 168 8 166 20 L156 16 C158 -2 162 -26 178 -44 Z")}" fill="#000000" opacity="0.15"/>`;
    return r + mirror(r);
  }
  const r = horn("M124 4 C132 -30 160 -58 198 -72 C194 -42 184 -12 166 20 Z", "M138 -16 Q154 -6 176 -10 M158 -44 Q170 -34 188 -40");
  return `<path data-dragon="spines" d="${up("M90 14 L100 -18 L110 14 Z")}" fill="${W}" stroke="${WD}" stroke-width="3" stroke-linejoin="round"/>` + r + mirror(r);
}
// From behind, the row of spines down the middle of the hood, on it (drawn after it): each standing up toward us, lit on one
// side, the top one over the crown, smaller toward the nape
function dragonHoodSpines(C, F) {
  const { cy, ry } = hoodGeom(F), top = cy - ry, W = C.wing, WD = mix(W, "#000000", 0.3);
  let lit = "", shade = "";
  for (let i = 0; i < 7; i++) {
    const y = top + 10 + i * ry * 1.5 / 6, h = 26 - i * 2, b = 11 - i * 0.8;
    lit += `M${f1(100 - b)} ${f1(y)} L100 ${f1(y - h)} L${f1(100 + b)} ${f1(y)} Z `;
    shade += `M100 ${f1(y - h)} L${f1(100 + b)} ${f1(y)} L100 ${f1(y - 3)} Z `;
  }
  return `<path data-dragon="spines" d="${lit}" fill="${W}" stroke="${WD}" stroke-width="2.5" stroke-linejoin="round"/><path d="${shade}" fill="#000000" opacity="0.18"/>`;
}
// On the brim in profile, before hoodSide's clip and shift to the front (our side only): a brow ridge over a slit-pupilled eye,
// and a nostril at the snout's front, over the opening
const dragonBrimSide = (C, dir) => { const X = v => f1(100 + dir * (v - 100)), ln = darker(C.fur);
  return `<path d="M${X(106)} 14 Q${X(122)} 3 ${X(140)} 13" fill="none" stroke="${ln}" stroke-width="4" stroke-linecap="round"/>` +
    `<ellipse cx="${X(122)}" cy="24" rx="9" ry="8" fill="${C.eyes}" stroke="#111" stroke-width="2"/><ellipse cx="${X(122)}" cy="24" rx="1.8" ry="6.6" fill="#111"/>` +
    `<circle cx="${X(122 - 2.8 * dir)}" cy="21.2" r="2.2" fill="#ffffff"/>` +
    `<ellipse cx="${X(160)}" cy="42" rx="5" ry="3" transform="rotate(${dir * 18} ${X(160)} 42)" fill="${ln}"/>`; };
// In profile, its teeth along the top of the hood's opening (the oval fx, fcy, frx, fry: hoodSide's), pointing in at the face;
// only where that edge is on the hood (its oval about 100, cy, 106 by ry), not where the opening runs out past its front
function dragonTeethSide(C, fx, fcy, frx, fry, dir, cy, ry) {
  const pt = deg => { const t = deg * Math.PI / 180; return [fx + dir * frx * Math.cos(t), fcy + fry * Math.sin(t)]; };
  let d = "";
  for (let a = -152; a <= -42; a += 12.2) {
    if (((pt(a + 4)[0] - 100) / 106) ** 2 + ((pt(a + 4)[1] - cy) / ry) ** 2 > 0.96) continue;
    const t0 = pt(a - 4), t1 = pt(a + 4), m = pt(a), v = [fx - m[0], fcy - m[1]], vn = Math.hypot(v[0], v[1]) || 1, ap = [m[0] + v[0] / vn * 15, m[1] + v[1] / vn * 15];
    d += `M${f1(t0[0])} ${f1(t0[1])} L${f1(ap[0])} ${f1(ap[1])} L${f1(t1[0])} ${f1(t1[1])} Z `;
  }
  return `<g clip-path="url(#hood-whole)"><path data-dragon="teeth" d="${d}" fill="${C.horn}" stroke="${darker(C.horn)}" stroke-width="1.8" stroke-linejoin="round"/></g>`;
}
// The plated belly: lines across the suit's belly (its oval: cy, rx, ry, in the middle), each bowed down a little and ending on
// the oval's outline, a shade darker than the belly; one path
function dragonPlates(C, cy, rx, ry) {
  let d = "";
  for (let i = 0; i < 7; i++) { const y = cy - ry * 0.76 + i * ry * 1.6 / 6, hw = rx * Math.sqrt(Math.max(0, 1 - ((y - cy) / ry) ** 2));
    d += `M${f1(-hw)} ${f1(y)} Q0 ${f1(y + 5)} ${f1(hw)} ${f1(y)} `; }
  return `<path data-dragon="plates" d="${d}" fill="none" stroke="${mix(C.belly, "#000000", 0.28)}" stroke-width="2.2"/>`;
}
// A dragon's tail along a cubic curve P (four points), half w0 wide at the root tapering to w1, in the scales; its spikes
// standing up along its top side (the side its normals point up on, the same side all along), each between that side's
// outward line and straight up, so none points down; from behind, where it comes toward us (t under `mid`), along its middle,
// straight up; and a spade at its tip. `up`: which way is up in the units it is drawn in (lying down, the friend is turned)
function dragonTail(C, P, w0, w1, mid = 0, up = [0, -1]) {
  const at = t => { const u = 1 - t; return [0, 1].map(k => u * u * u * P[0][k] + 3 * u * u * t * P[1][k] + 3 * u * t * t * P[2][k] + t * t * t * P[3][k]); };
  const tan = t => { const u = 1 - t, d = [0, 1].map(k => 3 * u * u * (P[1][k] - P[0][k]) + 6 * u * t * (P[2][k] - P[1][k]) + 3 * t * t * (P[3][k] - P[2][k])), n = Math.hypot(d[0], d[1]) || 1; return [d[0] / n, d[1] / n]; };
  const wd = t => w0 * (1 - t) + w1 * t, k = w0 / 21, W = C.wing, WD = mix(W, "#000000", 0.3);
  let sum = 0; for (let i = 0; i <= 10; i++) { const d = tan(i / 10); sum += -d[1] * up[0] + d[0] * up[1]; }
  const sg = sum >= 0 ? 1 : -1, nTop = d => [-d[1] * sg, d[0] * sg];
  const Lp = [], Rp = [];
  for (let i = 0; i <= 28; i++) { const t = i / 28, p = at(t), n = nTop(tan(t)), w = wd(t); Lp.push([p[0] + n[0] * w, p[1] + n[1] * w]); Rp.push([p[0] - n[0] * w, p[1] - n[1] * w]); }
  const body = "M" + Lp.concat(Rp.reverse()).map(q => f1(q[0]) + " " + f1(q[1])).join(" L") + " Z";
  let edge = "", middle = "";
  for (let t = 0.12; t < 0.9; t += 0.095) {
    const p = at(t), d = tan(t), n = nTop(d), w = wd(t), h = (22 * (1 - t) + 8) * k, onMid = t < mid;
    let u = onMid ? up : [n[0] + up[0], n[1] + up[1]]; const ul = Math.hypot(u[0], u[1]) || 1; u = [u[0] / ul, u[1] / ul];
    const e = onMid ? [-up[1], up[0]] : d, b = onMid ? p : [p[0] + n[0] * (w - 3), p[1] + n[1] * (w - 3)], s = 9 * k, back = onMid ? 0 : 6 * k;
    const tri = `M${f1(b[0] - e[0] * s)} ${f1(b[1] - e[1] * s)} L${f1(b[0] + u[0] * h - d[0] * back)} ${f1(b[1] + u[1] * h - d[1] * back)} L${f1(b[0] + e[0] * s)} ${f1(b[1] + e[1] * s)} Z `;
    if (onMid) middle += tri; else edge += tri;
  }
  const e = at(1), d = tan(1), n = [d[1], -d[0]], q = (a, b) => `${f1(e[0] + (d[0] * a + n[0] * b) * k)} ${f1(e[1] + (d[1] * a + n[1] * b) * k)}`;
  const spikes = t => t ? `<path data-dragon="spikes" d="${t}" fill="${W}" stroke="${WD}" stroke-width="2" stroke-linejoin="round"/>` : "";
  return spikes(edge) + `<path data-dragon="tail" d="${body}" fill="url(#pat-fur)" stroke="${darker(C.fur)}" stroke-width="2.5" stroke-linejoin="round"/>` + spikes(middle) +
    `<path data-dragon="spade" d="M${q(-6, 0)} Q${q(-12, 22)} ${q(6, 20)} Q${q(16, 12)} ${q(34, 0)} Q${q(16, -12)} ${q(6, -20)} Q${q(-12, -22)} ${q(-6, 0)} Z" fill="${W}" stroke="${WD}" stroke-width="2.5" stroke-linejoin="round"/>`;
}
// The tail in each view, in the friend's units (hw: the hips' half width; L the waist; ground under the feet): toward us,
// from behind the hips out to our right and down onto the ground; in profile (the mock-up's), out behind the hips, down to
// the ground and along it; from behind, from the bottom of the back down over the seat, toward us, then out along the ground.
// And two poses of its own (how): lying down (lie: the side the ground is on, +1 or -1, in the friend's turned units; w, LL),
// along the ground by the legs and out past the feet, its spikes up off the ground; crawling toward us (rise: where the
// hips show, over the back behind the head), up from behind the back and over, as a cat's tail goes up.
// (ticket 499, review: crawling or swimming in profile, the friend turned about the hips, the profile's tail turned with them
// went up from the hips with its spikes pointing at the head, against Matthew's "dragon tail spikes should be facing upward,
// not downward". So that pose has a curve of its own (turn: the turn, in degrees; floor: the ground's height in the
// picture's units), laid out level in the picture: from the back of the hips out behind them, arching a little up, then down
// to the ground and along it (swimming, floor is a little under the water's top); each point turned back into the friend's
// turned units, and `up` the picture's up in them)
function dragonTailSvg(C, hw, L, ground, dir, away, how = null) {
  if (how && how.lie) { const g = how.lie, x = g * (how.w - 6);
    return dragonTail(C, [[g * hw * 0.3, L + 8], [x, L + 50], [x, L + how.LL * 0.8], [g * (how.w - 12), L + how.LL + 90]], 20, 4, 0, [-g, 0]); }
  if (how && how.turn && dir) { const a = how.turn * Math.PI / 180, c = Math.cos(a), sn = Math.sin(a), fl = how.floor;
    const toPic = ([x, y]) => [x * c - (y - L) * sn, L + x * sn + (y - L) * c], toBody = ([x, y]) => [x * c + (y - L) * sn, L - x * sn + (y - L) * c];
    const r = toPic([-dir * hw * 0.8, L - 10]);
    return dragonTail(C, [r, [r[0] - dir * 95, r[1] - 26], [r[0] - dir * 150, fl - 24], [r[0] - dir * 250, fl - 22]].map(toBody), 21, 4, 0, [-sn, -c]); }
  if (how && how.rise !== undefined) { const y = how.rise;
    return dragonTail(C, [[hw * 0.3, y + 6], [hw + 30, y - 40], [hw + 100, y - 60], [hw + 160, y - 16]], 18, 4); }
  if (away) return dragonTail(C, [[0, L - 6], [4, L + 70], [24, ground - 6], [hw + 165, ground - 20]], 22, 4, 0.45);
  if (dir) { const x = hw * 0.8; return dragonTail(C, [[-dir * x, L - 10], [-dir * (x + 97), L], [-dir * (x + 112), ground - 20], [-dir * (x + 210), ground - 24]], 21, 4); }
  return dragonTail(C, [[hw * 0.45, L + 8], [hw + 80, L + 20], [hw + 70, ground - 14], [hw + 175, ground - 22]], 20, 4);
}
// One bat wing from its root (0, 0), in the friend's units: its arm up to the wrist Wr, three fingers out from there to the
// tips T1 to T3, the skin between them in scallops bowed in toward the wrist and back to the body at Bk; its thumb's claw at
// the wrist (in the horns' colour); `ridge`, from behind: the bones lit along their tops
function batWing(C, [Wr, T1, T2, T3, Bk], fill, bone, bw, ridge = null) {
  const P = ([x, y]) => `${f1(x)} ${f1(y)}`, o = Math.sign(Wr[0]) || 1;
  const sc = (a, b, k) => { const mx = (a[0] + b[0]) / 2, my = (a[1] + b[1]) / 2; return `Q${f1(mx + (Wr[0] - mx) * k)} ${f1(my + (Wr[1] - my) * k)} ${P(b)}`; };
  const bones = `M0 0 L${P(Wr)} L${P(T1)} M${P(Wr)} L${P(T2)} M${P(Wr)} L${P(T3)}`;
  return `<path d="M0 0 L${P(Wr)} L${P(T1)} ${sc(T1, T2, 0.25)} ${sc(T2, T3, 0.25)} ${sc(T3, Bk, 0.2)} Z" fill="${fill}" stroke="${bone}" stroke-width="3" stroke-linejoin="round"/>` +
    `<path d="${bones}" fill="none" stroke="${bone}" stroke-width="${bw}" stroke-linecap="round" stroke-linejoin="round"/>` +
    (ridge ? `<path d="${bones}" fill="none" stroke="${ridge}" stroke-width="${f1(bw * 0.4)}" stroke-linecap="round" stroke-linejoin="round"/>` : "") +
    `<path d="M${P(Wr)} l${o * 6} -16 l${-o * 12} 10 Z" fill="${C.horn}" stroke="${darker(C.horn)}" stroke-width="1.5" stroke-linejoin="round"/>`;
}
// 🪽 its wings: flapping (slowly, about the shoulder, with the LFO chosen in dragonWingLfo, or at their own slow beat), open
// and still, or folded
const DRAGON_WINGS = [["flap", "🪽 flapping"], ["open", "🦇 open and still"], ["folded", "🌂 folded"]];
// The wings in each view, in the friend's units (w the shirt's half width as drawn, L its length): toward us, one each side
// behind the shoulders, spread up and out; in profile (the mock-up's), the near one bigger, spread back behind the shoulders,
// the far one up and forward behind the head, darker; from behind, their backs over the friend's back, the bones lit along
// their tops. Folded, each lies against the back, its wrist over the shoulder. Flapping, each turns about its root, the two in
// step (sign: which way turns its tip up), a little open on the whole.
// (ticket 499, review: crawling or swimming in profile, turned with the friend, the far wing hung down under the head through
// the floor.) Turned about the hips by `turn` (degrees) in profile, open or flapping, they have a drawing of their own, laid
// out in the picture: both raised up off the back, the near one up and back over the hips, the far one, darker, up and a
// little forward, behind it; each point turned back into the friend's turned units. Folded, they lie on the back as ever.
function dragonWingsSvg(C, WC, w, L, dir, away, turn = 0) {
  const how = DRAGON_WINGS.some(([v]) => v === WC.dragonWingsHow) ? WC.dragonWingsHow : "flap", folded = how === "folded";
  const W = C.wing, WD = mix(W, "#000000", 0.3), Wf = { lf: how === "flap" ? lfoNow(WC.dragonWingLfo) : null, off: 0, dur: 1.7 };
  const one = (R, pts, fill, bone, sign, ridge = null) => {
    const wing = batWing(C, pts, fill, bone, folded ? 4 : 5, ridge);
    const inner = how !== "flap" ? wing : wagged(Wf, sh => `<g>${wagAnim(Wf, "transform", (v, amp) => f1(sign * (3 + (6 + 12 * amp) * v)), sh)}${wing}</g>`);
    return `<g data-dragon="wing" transform="translate(${f1(R[0])} ${f1(R[1])})">${inner}</g>`; };
  if (dir) {
    const X = ps => ps.map(([x, y]) => [dir * x, y]);
    if (folded) return one([-dir * w * 0.15, -14], X([[-10, -84], [-30, -40], [-34, 4], [-26, 36], [4, 20]]), WD, mix(WD, "#000000", 0.3), -dir) +
                       one([-dir * w * 0.6, -8], X([[-20, -70], [-44, -18], [-50, L * 0.38], [-40, L * 0.72], [-2, L * 0.5]]), W, WD, dir);
    if (turn) { const a = turn * Math.PI / 180, c = Math.cos(a), sn = Math.sin(a), T = ps => X(ps).map(([x, y]) => [x * c + y * sn, -x * sn + y * c]);
      return one([-dir * w * 0.3, -16], T([[24, -150], [80, -212], [120, -150], [100, -84], [30, -22]]), WD, mix(WD, "#000000", 0.3), -dir) +
             one([-dir * w * 0.6, -8], T([[-40, -128], [-150, -160], [-205, -80], [-180, -8], [-40, 8]]), W, WD, dir); }
    return one([dir * 14, -12], X([[56, -158], [136, -203], [151, -138], [111, -88], [21, -28]]), WD, mix(WD, "#000000", 0.3), -dir) +
           one([-dir * w * 0.6, -8], X([[-98, -112], [-228, -87], [-216, 3], [-153, 56], [-10, 70]]), W, WD, dir);
  }
  const both = (R, ps, fill, ridge) => [-1, 1].map(sd => one([sd * R[0], R[1]], ps.map(([x, y]) => [sd * x, y]), fill, WD, -sd, ridge)).join("");
  if (away) return folded ? both([12, 0], [[w * 0.5, -62], [w * 0.66, -14], [w * 0.72, L * 0.4], [w * 0.56, L * 0.76], [2, L * 0.62]], mix(W, "#000000", 0.12), mix(W, "#ffffff", 0.25))
                          : both([12, 0], [[w + 28, -110], [w + 158, -86], [w + 178, 10], [w + 116, 94], [w * 0.35 - 12, L * 0.42]], mix(W, "#000000", 0.12), mix(W, "#ffffff", 0.25));
  return folded ? both([18, 6], [[w - 4, -70], [w + 8, -10], [w + 12, L * 0.35], [w + 2, L * 0.68], [w - 28, L * 0.55]], W)
                : both([20, 10], [[w + 20, -115], [w + 150, -92], [w + 170, 4], [w + 108, 90], [w - 26, 102]], W);
}

// (ticket 431) THE GNOMES (inspired by the gnome man and woman of Huygen and Poortvliet's book, 1977, drawn in the game's own
// plain way: shapes and patterns, no illustration). The man: a long-sleeved tunic over trousers, belted, its skirt below the
// belt; felt boots; a beard and a moustache. The woman: a white blouse, a laced bodice, an apron, and a skirt of upright
// stripes. Both: the tall pointed hat (gnomeHat). GNOME_STRIPES: the skirt's stripes after its own color.
const GNOME_STRIPES = ["#d2382b", "#2e7d32", "#e3b72f", "#7b3f8c", "#2f6db5"];
const GNOME_CLOTH = "#f7f4ec";
// (the man's beard and the wizard's: the friend's own, from 💇 Hair since ticket 442: beardSvg in js/04-hair.js)
// The woman's bodice, in body units: over the blouse from the chest down to the waist, its sides the torso's, laced up the
// front (white cross-lacing; edge on in profile, at the front); from behind, its back, plain.
function gnomeBodice(WC, w, L, dir, away) {
  const c = WC.gnomeBodice || "#d2382b", ln = darker(c), top = 0.3 * L;
  const body = `<path d="M${f1(-(w - 2))} ${f1(top)} Q0 ${f1(top + 10)} ${f1(w - 2)} ${f1(top)} L${f1(w + 2)} ${f1(L)} L${f1(-(w + 2))} ${f1(L)} Z" fill="${c}" stroke="${ln}" stroke-width="2.5" stroke-linejoin="round"/>`;
  if (away) return body;
  const x = dir ? dir * (w - 8) : 0, n = 4, lace = [];
  for (let i = 0; i < n; i++) { const y0 = top + 10 + i * (L - top - 18) / n, y1 = y0 + (L - top - 18) / n;
    lace.push(dir ? `M${f1(x)} ${f1(y0)} L${f1(x)} ${f1(y1)}` : `M-9 ${f1(y0)} L9 ${f1(y1)} M9 ${f1(y0)} L-9 ${f1(y1)}`); }
  return body + `<path d="${lace.join(" ")}" fill="none" stroke="${GNOME_CLOTH}" stroke-width="2.5" stroke-linecap="round"/>`;
}
// Her apron, in body units: from the waist down the front of the skirt, a little narrower than the hips and not so long as
// the skirt, with its band round the waist; in profile, a strip at the front; from behind, only the band and its bow.
function gnomeApron(w, L, hem, dir, away) {
  const ln = "#cfc8b8", band = `<rect x="${f1(-(w + 3))}" y="${f1(L - 8)}" width="${f1(2 * w + 6)}" height="9" fill="${GNOME_CLOTH}" stroke="${ln}" stroke-width="2"/>`;
  if (away) return band + `<path d="M0 ${f1(L - 4)} q-16 -12 -22 2 q8 10 22 -2 q16 -12 22 2 q-8 10 -22 -2 M-2 ${f1(L - 2)} l-8 22 M2 ${f1(L - 2)} l8 22" fill="${GNOME_CLOTH}" stroke="${ln}" stroke-width="2"/>`;
  const a = dir ? [dir * (w - 18), dir * (w + 6)] : [-(w - 6), w - 6], b = L + 0.82 * (hem - L);
  const [x0, x1] = [Math.min(...a), Math.max(...a)];
  return `<path d="M${f1(x0)} ${f1(L)} L${f1(x1)} ${f1(L)} L${f1(x1 + (dir ? 0 : 6))} ${f1(b)} Q${f1((x0 + x1) / 2)} ${f1(b + 6)} ${f1(x0 - (dir ? 0 : 6))} ${f1(b)} Z" fill="${GNOME_CLOTH}" stroke="${ln}" stroke-width="2.5" stroke-linejoin="round"/>` + band;
}
// (ticket 288, Matthew: "new costumes for the ocean and the deep sea. A snorkel, the most minimal change to the friend's
// appearance. An air tank and a face mask, for scuba, separate from the scuba suit. A scuba suit, modern and stylish.
// An antique deep-sea diving suit, made of metal and glass, with an air hose reaching up to the top of the scene") The
// two suits are costumes (state.costume.kind "scuba", "diver"); the snorkel, the mask and the tank are gear, worn with
// any of them but the diver's helmet (costume.snorkel, .mask, .tank). What goes on the head is in the face's units (x
// 100 the middle; facing us, from behind, or in profile, dir -1 or 1); the rest in the friend's own.
const DIVE_GLASS = "#cfeefc";
const ellD = (x, y, a, b) => `M${f1(x - a)} ${f1(y)} a${f1(a)} ${f1(b)} 0 1 0 ${f1(2 * a)} 0 a${f1(a)} ${f1(b)} 0 1 0 ${f1(-2 * a)} 0 Z`;
const diveHead = F => { const top = headCrown(F), chin = headChin(F); return { LY: faceLayout(F), hw: 80 * headWidth(F), top, chin, cy: (top + chin) / 2, ry: (chin - top) / 2 }; };
// the antique helmet: a round brass (or copper, or steel) bell round the whole head, the front port's glass over the face
// (a hole in the metal, so the face shows through, faintly blue, its rim bolted), a small port at each side, a shine,
// and the air hose's fitting on top toward the back; from behind, all metal. fitting: where the hose goes in
function diverHelmet(F, WC, dir, away) {
  const H = diveHead(F), m = WC.diverMetal || "#c9a227", dk = darker(m), lt = mix(m, "#ffffff", 0.45);
  const rx = H.hw * 1.2 + 10, ry = H.ry * 1.16 + 10, cx = 100, cy = H.cy;
  // (in profile, the port over the face's front, which the face's features are drawn toward, seen at a slant)
  const pr = 0.62 * H.hw + 6, pa = dir ? 0.4 * pr : pr, px = dir ? cx + dir * 0.6 * H.hw : cx, py = H.LY.eyeY + 14;
  let t = `<path data-dive="helmet" d="${ellD(cx, cy, rx, ry)} ${away ? "" : ellD(px, py, pa, pr)}" fill="${m}" fill-rule="evenodd" stroke="${dk}" stroke-width="5"/>` +
          `<path d="M${f1(cx - 0.78 * rx)} ${f1(cy - 0.3 * ry)} Q${f1(cx - 0.6 * rx)} ${f1(cy - 0.84 * ry)} ${f1(cx - 0.05 * rx)} ${f1(cy - 0.93 * ry)}" fill="none" stroke="${lt}" stroke-width="8" stroke-linecap="round" opacity="0.8"/>`;
  if (!away) {
    t += `<path d="${ellD(px, py, pa, pr)}" fill="${DIVE_GLASS}" fill-opacity="0.2" stroke="${dk}" stroke-width="10"/><path d="${ellD(px, py, pa, pr)}" fill="none" stroke="${m}" stroke-width="4"/>`;
    for (let i = 0; i < 8; i++) { const a = i * Math.PI / 4; t += `<circle cx="${f1(px + Math.cos(a) * (pa + 3))}" cy="${f1(py + Math.sin(a) * (pr + 3))}" r="3.4" fill="${lt}" stroke="${dk}" stroke-width="1.5"/>`; }
  }
  // (Matthew: "Adding more details -- like rivits and seams in the metal") its seams, riveted: one over the crown (down
  // to the port facing us; down the back from behind; down the side in profile), and the raised neck ring round its foot
  const rivet = (x, y, r = 2.6) => `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(r)}" fill="${lt}" stroke="${dk}" stroke-width="1.2"/>`;
  const along = (P, n) => Array.from({ length: n }, (_, i) => { const u = (i + 0.5) / n, [a, b, c] = P;
    return rivet((1 - u) * (1 - u) * a[0] + 2 * u * (1 - u) * b[0] + u * u * c[0], (1 - u) * (1 - u) * a[1] + 2 * u * (1 - u) * b[1] + u * u * c[1]); }).join("");
  const seam = (P, n) => `<path data-dive="seam" d="M${f1(P[0][0])} ${f1(P[0][1])} Q${f1(P[1][0])} ${f1(P[1][1])} ${f1(P[2][0])} ${f1(P[2][1])}" fill="none" stroke="${dk}" stroke-width="2.5"/>` + along(P, n);
  const crown = dir ? [[cx + dir * 0.15 * rx, cy - ry], [cx - dir * 0.55 * rx, cy - 0.2 * ry], [cx - dir * 0.45 * rx, cy + 0.62 * ry]]
              : away ? [[cx, cy - ry], [cx, cy], [cx, cy + 0.66 * ry]] : [[cx, cy - ry], [cx, (cy - ry + py - pr) / 2], [cx, py - pr - 9]];
  t += seam(crown, away ? 6 : dir ? 5 : 3);
  const ring = [[cx - 0.93 * rx, cy + 0.5 * ry], [cx, cy + 1.02 * ry], [cx + 0.93 * rx, cy + 0.5 * ry]];
  t += `<path data-dive="ring" d="M${f1(ring[0][0])} ${f1(ring[0][1])} Q${f1(ring[1][0])} ${f1(ring[1][1])} ${f1(ring[2][0])} ${f1(ring[2][1])}" fill="none" stroke="${dk}" stroke-width="12" stroke-linecap="round"/>` +
       `<path d="M${f1(ring[0][0])} ${f1(ring[0][1])} Q${f1(ring[1][0])} ${f1(ring[1][1])} ${f1(ring[2][0])} ${f1(ring[2][1])}" fill="none" stroke="${m}" stroke-width="7" stroke-linecap="round"/>` + along(ring, 9);
  const port = (x, y, a, b) => `<path d="${ellD(x, y, a, b)}" fill="${DIVE_GLASS}" fill-opacity="0.55" stroke="${dk}" stroke-width="6"/>` +
    Array.from({ length: 6 }, (_, i) => rivet(x + Math.cos(i * Math.PI / 3) * (a + 5), y + Math.sin(i * Math.PI / 3) * (b + 5), 2)).join("");
  if (dir) t += port(cx - dir * 0.15 * rx, cy - 0.05 * ry, 0.22 * rx, 0.22 * rx);
  else if (!away) t += port(cx - 0.9 * rx, cy, 0.1 * rx, 0.22 * ry) + port(cx + 0.9 * rx, cy, 0.1 * rx, 0.22 * ry);
  const fx = cx - (dir || 0) * 0.4 * rx, fy = cy - ry + 6;
  t += `<rect x="${f1(fx - 11)}" y="${f1(fy - 20)}" width="22" height="24" rx="4" fill="${m}" stroke="${dk}" stroke-width="3"/>`;
  // (where it leaks, when it does: the fitting, and its side, the exhaust valve)
  return { svg: t, fitting: [fx, fy - 20], exhaust: [cx + (dir ? -dir * 0.92 : 0.92) * rx, cy + 0.1 * ry] };
}
// its corselet: the brass collar on the shoulders the helmet sits on, a bolt at each side and the middle, in the friend's units
function diverCorselet(WC, w, dir) {
  const m = WC.diverMetal || "#c9a227", dk = darker(m), lt = mix(m, "#ffffff", 0.45), x = dir ? 0.95 * w : w + 8;
  // (a seam set in from its edge, studded with rivets all along the edge, and a wing nut at each shoulder)
  const edge = u => [u * x, 30 + 16 * (1 - u * u)];
  return `<path data-dive="corselet" d="M${f1(-x)} 30 Q${f1(-x)} -26 0 -32 Q${f1(x)} -26 ${f1(x)} 30 Q0 46 ${f1(-x)} 30 Z" fill="${m}" stroke="${dk}" stroke-width="4"/>` +
    `<path d="M${f1(-x + 9)} 26 Q${f1(-x + 9)} -16 0 -22 Q${f1(x - 9)} -16 ${f1(x - 9)} 26" fill="none" stroke="${dk}" stroke-width="2" opacity="0.7"/>` +
    Array.from({ length: 9 }, (_, i) => { const [ex, ey] = edge(-0.9 + 1.8 * i / 8); return `<circle cx="${f1(ex)}" cy="${f1(ey - 7)}" r="3" fill="${lt}" stroke="${dk}" stroke-width="1.2"/>`; }).join("") +
    [-1, 1].map(sd => `<path d="M${f1(sd * 0.62 * x - 7)} -6 L${f1(sd * 0.62 * x + 7)} -6 L${f1(sd * 0.62 * x + 3)} 0 L${f1(sd * 0.62 * x - 3)} 0 Z" fill="${lt}" stroke="${dk}" stroke-width="1.5"/>`).join("");
}
// its air hose: one path from the helmet's fitting (a) up to the top of the scene (b), swaying slowly (a single path, as
// the cauldron's light fumes are, and still on a slow computer)
function diverHose(WC, a, b, still) {
  const c = WC.hoseColor || "#4b5563", [ax, ay] = a, [bx, by] = b, h = ay - by;
  const d = k => `M${f1(ax)} ${f1(ay)} C${f1(ax - 40 * k)} ${f1(ay - 0.35 * h)} ${f1(bx + 60 * k)} ${f1(by + 0.45 * h)} ${f1(bx)} ${f1(by)}`;
  return `<path data-hose="1" d="${d(1)}" fill="none" stroke="${c}" stroke-width="11" stroke-linecap="round">` +
    (still ? "" : `<animate attributeName="d" values="${d(1)};${d(-0.7)};${d(1)}" dur="6s" repeatCount="indefinite" calcMode="spline" keySplines="0.45 0 0.55 1;0.45 0 0.55 1"/>`) + `</path>`;
}
// a face mask: its lens over the eyes and the top of the nose, its frame and nose pocket, and the strap round the head;
// in profile, the lens at the face's front and the strap back over the head; from behind, the strap
function maskSvg(F, WC, dir, away) {
  const H = diveHead(F), c = WC.maskColor || "#0ea5e9", dk = darker(c), y = H.LY.eyeY, [xl, xr] = acrossAt(headPoints(F), y);
  const strap = (x0, x1) => `<line x1="${f1(x0)}" y1="${f1(y)}" x2="${f1(x1)}" y2="${f1(y)}" stroke="${c}" stroke-width="8" stroke-linecap="round"/>`;
  if (away) return `<path data-dive="mask" d="M${f1(xl - 3)} ${f1(y)} Q100 ${f1(y + 10)} ${f1(xr + 3)} ${f1(y)}" fill="none" stroke="${c}" stroke-width="9" stroke-linecap="round"/>`;
  const top = y - eyeHalfH(F) - 14, bot = y + eyeHalfH(F) + 20;
  if (dir) { const x0 = 100 + dir * (profileEdge(F) + 6), x1 = x0 - dir * 38;
    return strap(x1, 100 - dir * H.hw) + `<rect data-dive="mask" x="${f1(Math.min(x0, x1))}" y="${f1(top)}" width="38" height="${f1(bot - top)}" rx="12" fill="${DIVE_GLASS}" fill-opacity="0.45" stroke="${c}" stroke-width="7"/>`; }
  const hx = H.LY.eyeDX + eyeHalfW(F) + 12;
  return strap(xl - 2, 100 - hx) + strap(100 + hx, xr + 2) +
    `<rect data-dive="mask" x="${f1(100 - hx)}" y="${f1(top)}" width="${f1(2 * hx)}" height="${f1(bot - top)}" rx="18" fill="${DIVE_GLASS}" fill-opacity="0.35" stroke="${c}" stroke-width="7"/>` +
    `<path d="M${f1(100 - 15)} ${f1(bot - 3)} L${f1(100 - 10)} ${f1(bot + 16)} L${f1(100 + 10)} ${f1(bot + 16)} L${f1(100 + 15)} ${f1(bot - 3)} Z" fill="${c}" stroke="${dk}" stroke-width="2"/>`;
}
// a snorkel: a J of tube from the mouth, round to the side of the head and up past its top, a band at its tip; on the
// friend's left (our right, facing us), on our side in profile
function snorkelSvg(F, WC, dir, away) {
  const H = diveHead(F), c = WC.snorkelColor || "#facc15", dk = darker(c), mY = H.LY.mouthY, up = H.top - 36;
  const d = away ? `M${f1(100 - H.hw - 8)} ${f1(mY - 20)} L${f1(100 - H.hw - 8)} ${f1(up)}`
          : dir ? `M${f1(100 + dir * (profileEdge(F) - 4))} ${f1(mY + 4)} Q${f1(100 + dir * 0.1 * H.hw)} ${f1(mY + 18)} ${f1(100 - dir * 0.05 * H.hw)} ${f1(mY - 30)} L${f1(100 - dir * 0.1 * H.hw)} ${f1(up)}`
          : `M${f1(100 + 16)} ${f1(mY + 4)} Q${f1(100 + H.hw + 12)} ${f1(mY + 16)} ${f1(100 + H.hw + 12)} ${f1(mY - 26)} L${f1(100 + H.hw + 12)} ${f1(up)}`;
  const tip = away ? [100 - H.hw - 8, up] : dir ? [100 - dir * 0.1 * H.hw, up] : [100 + H.hw + 12, up];
  return `<path d="${d}" fill="none" stroke="${dk}" stroke-width="16" stroke-linecap="round"/><path data-dive="snorkel" d="${d}" fill="none" stroke="${c}" stroke-width="11" stroke-linecap="round"/>` +
    `<rect x="${f1(tip[0] - 9)}" y="${f1(tip[1] - 6)}" width="18" height="10" rx="3" fill="${dk}"/>` +
    (away ? "" : `<ellipse cx="${f1(dir ? 100 + dir * (profileEdge(F) - 2) : 100 + 10)}" cy="${f1(mY + 4)}" rx="9" ry="7" fill="#1f2937"/>`);
}
// an air tank on the back: a cylinder down from the shoulders, its valve on top; facing us, behind them (its straps over
// the shirt: tankStraps); in profile, behind the back; from behind, over it
function tankSvg(WC, w, L, dir) {
  const c = WC.tankColor || "#facc15", dk = darker(c), lt = mix(c, "#ffffff", 0.5), tw = Math.max(24, 0.4 * w), x = dir ? -dir * (0.5 * w + 0.9 * tw) : 0;
  const top = -12, bot = Math.max(top + 3 * tw, 0.78 * L);
  return `<rect data-dive="tank" x="${f1(x - tw)}" y="${f1(top)}" width="${f1(2 * tw)}" height="${f1(bot - top)}" rx="${f1(tw)}" fill="${c}" stroke="${dk}" stroke-width="3"/>` +
    `<line x1="${f1(x - 0.45 * tw)}" y1="${f1(top + 0.8 * tw)}" x2="${f1(x - 0.45 * tw)}" y2="${f1(bot - 0.8 * tw)}" stroke="${lt}" stroke-width="5" stroke-linecap="round"/>` +
    `<rect x="${f1(x - 7)}" y="${f1(top - 20)}" width="14" height="22" rx="3" fill="#374151"/><circle cx="${f1(x + (dir || 1) * 10)}" cy="${f1(top - 12)}" r="6" fill="#111827"/>`;
}
function tankStraps(w, L, dir, away) {
  // (grey webbing, edged darker: it shows on a dark suit as on a light shirt)
  const s = "#9ca3af", band = `<rect x="${f1(-(w + 3))}" y="${f1(0.7 * L - 6)}" width="${f1(2 * w + 6)}" height="12" fill="${s}" stroke="#4b5563" stroke-width="2"/>`;
  if (away) return band;
  if (dir) return `<line x1="${f1(-dir * 0.15 * w)}" y1="-16" x2="${f1(-dir * 0.15 * w)}" y2="${f1(0.7 * L)}" stroke="${s}" stroke-width="10"/>` + band;
  return [1, -1].map(sd => `<line x1="${f1(sd * 0.55 * w)}" y1="-18" x2="${f1(sd * 0.45 * w)}" y2="${f1(0.7 * L)}" stroke="${s}" stroke-width="10"/>`).join("") + band +
    `<rect x="-9" y="${f1(0.7 * L - 9)}" width="18" height="18" rx="3" fill="#e5e7eb" stroke="#4b5563" stroke-width="2"/>`;
}
// a scuba suit's stripes: a bright yoke over the shoulders and chest (or back), a zip down the back, and a stripe down
// the outside of each leg (scubaLegs)
function scubaYoke(WC, w, nw, L, dir, away) {
  const a = WC.scubaAccent || "#14b8a6", x = w + 3;
  return `<path data-dive="yoke" d="M${f1(-x)} -8 L${f1(-nw)} -20 L${f1(nw)} -20 L${f1(x)} -8 L${f1(w)} 24 L0 ${f1(dir ? 24 : 46)} L${f1(-w)} 24 Z" fill="${a}" stroke="${darker(a)}" stroke-width="2"/>` +
    (away ? `<line x1="0" y1="-18" x2="0" y2="${f1(L - 6)}" stroke="#9ca3af" stroke-width="3" stroke-dasharray="4 3"/>` : "");
}

// (ticket 288, Matthew: "we should have a param for \"leaks\" -- the default is fine (zero) and up to 100, it should be
// more and more bubbly, like the bubble machine we added earlier tonight") A diving suit's leaks: bubbles streaming out
// of where it leaks (leakPts, from the drawing: the old helmet's fitting and exhaust and its collar's edges; a scuba
// suit's shoulders and knees, its tank's valve and mouthpiece, a snorkel's tip), "🫧 leaks" of 100 at most a second
// eighteen, the bigger the more it leaks; each rising, wobbling, and fading after a few seconds or past the top. In a
// layer of their own over the scene (#leak-bubbles), as the bubble makers' are; still while the picture is held still.
let leakPts = null;
const leakSim = { list: [], acc: 0, raf: 0, last: 0, n: 0, layer: null };
function leakRun() {
  if (!leakPts) { if (leakSim.raf) cancelAnimationFrame(leakSim.raf); leakSim.raf = 0; for (const b of leakSim.list) b.el.remove(); leakSim.list = []; if (leakSim.layer) { leakSim.layer.remove(); leakSim.layer = null; } return; }
  let L = leakSim.layer;
  if (!L) { L = leakSim.layer = document.createElementNS("http://www.w3.org/2000/svg", "g"); L.id = "leak-bubbles"; L.setAttribute("pointer-events", "none"); leakSim.list = []; }
  if (L.parentNode !== pic || pic.lastChild !== L) pic.append(L);
  if (!leakSim.raf) { leakSim.last = 0; leakSim.raf = requestAnimationFrame(leakTick); }
}
function leakTick(t) {
  leakSim.raf = 0;
  const P = leakPts, layer = leakSim.layer, F = sceneFrame;
  if (!P || !layer) return;
  const dt = leakSim.last ? Math.min(0.05, Math.max(0, (t - leakSim.last) / 1000)) : 0; leakSim.last = t;
  if (stillAt !== null) { leakSim.raf = requestAnimationFrame(leakTick); return; }
  const L = leakSim.list, k = P.k, rnd = () => { const v = Math.sin(++leakSim.n * 12.9898) * 43758.5453; return v - Math.floor(v); };
  leakSim.acc += 18 * P.amt * dt;
  while (leakSim.acc >= 1) {
    leakSim.acc -= 1;
    const [x, y] = P.pts[Math.floor(rnd() * P.pts.length)], r = Math.max(1.5, k * (5 + (6 + 16 * P.amt) * rnd()));
    const el = document.createElementNS("http://www.w3.org/2000/svg", "g");
    el.innerHTML = `<g class="bubin">${bubbleSvg(0, 0, r, { color: "#dff6ff", shine: 0.85 })}</g>`; layer.append(el);   // (ticket 499: in a group of its own, to pop)
    L.push({ x, y, vx: (rnd() - 0.5) * 60 * k, vy: -(60 + 90 * rnd()) * k, r, el, born: t, life: 2000 + 2600 * rnd(), ph: rnd() * 6.28 });
    if (L.length > 90) { const o = L.shift(); o.el.remove(); }
  }
  for (let j = L.length - 1; j >= 0; j--) {
    const b = L[j], age = t - b.born;
    if (b.pop) { if (t - b.pop > 260) { b.el.remove(); L.splice(j, 1); } continue; }   // (ticket 499: popped by a fire's heat, swelling and gone)
    if (age > b.life || b.y < F.top - 30) { b.el.remove(); L.splice(j, 1); continue; }
    b.vy -= 70 * k * dt; b.vx *= Math.pow(0.4, dt); b.x += (b.vx + Math.sin(t / 1000 * 4 + b.ph) * 26 * k) * dt; b.y += b.vy * dt;
    b.el.setAttribute("transform", `translate(${f1(b.x)} ${f1(b.y)})`); b.el.setAttribute("opacity", Math.min(1, (b.life - age) / 500, age / 120).toFixed(2));
  }
  leakSim.raf = requestAnimationFrame(leakTick);
}

// (ticket 515, Matthew: "robot costume -- multiple styles -- do 8") THE ROBOT. costume.kind "robot"; costume.robotStyle one
// of ROBOT_STYLES (js/01-state.js, its traits the data these read); robotColor_<style> its colour (each style its own, as
// the fairy's wings); robotLights its lights ("auto": the style's own). A costume in the friend's own clothes, as the knight
// is: drawNow replaces the shirt and pants (sleeves and legs in the style's limb colour; the box bot keeps the friend's own
// trousers and shoes) and draws these pieces in its own slots. Each view is a drawing of its own (facing us; from the side,
// dir -1 or 1, left and right mirrored; from behind, away), never a turned, skewed or scaled copy of another view's:
//   robotHead   over the head, in the face's units (x 100 its middle), where the knight's helmet goes; a faceplate or a
//               screen (tin, helper) draws eyes and a mouth of its own that follow the friend's mood, and the face is not
//               drawn under it; a window or an opening shows the friend's face in the views `see` names
//   robotTorso  over the shirt, where the knight's tabard goes; from behind, its back; crawling toward us or away, a
//               drawing of its own (robotCrawlTorso), outside the body's turned or shortened group
//   robotBack   what stands off the back, where wings go: behind them facing us, over the back from behind; crawling
//               toward us or away, a drawing of its own over the back (robotCrawlBack), never the from-behind one turned
//   robotArm    on each arm, in that arm's own slot, so a nearer arm's piece is over a farther one's
//   robotLeg    on each leg, in the order the legs are drawn
// Speed (the first rule): no masks, filters, gradients or animations; a few shapes each, a row of rivets, gears or slots
// one path, ridges and stripes one dashed stroke; nothing blinks or moves by itself (the tin's key turns with the stride).
const ROBOT_SILVER = "#c9ced6";
function robotOf(WC) {
  const id = ROBOT_STYLES[WC.robotStyle] ? WC.robotStyle : "tin", S = ROBOT_STYLES[id], main = WC["robotColor_" + id] || S.colors[0];
  const lt = !WC.robotLights || WC.robotLights === "auto" ? S.lights : WC.robotLights;
  // (the hero's red trim turns white on the red swatch)
  const R = { id, S, main, dk: darker(main), lt, ltk: darker(lt), red: main === "#dc2626" ? "#f1f5f9" : "#e23b3b" };
  const tint = c => c === "main" ? main : c === "darker" ? R.dk : c === "red" ? R.red : c;
  return { ...R, limb: tint(S.limb), hand: tint(S.hand), boot: tint(S.boot) };
}
const robotHands = (R, hands) => ({ ...hands, style: R.S.hands, size: hands.size * (R.S.handK || 1), color: R.hand });
const robotFeet = (R, shoes) => R.S.feet === "own" ? shoes : { style: "boot", size: shoes.size * (R.S.footK || 1), color: R.boot };
const robotSees = (R, view) => R.S.see.split(" ").includes(view);
// (each piece marked by what it is, data-robot head, eyes, torso, back, arm or leg, for the checks and 🔬: a group, no shape)
const rbG = (what, t) => t ? `<g data-robot="${what}">${t}</g>` : "";
// (small pieces of path: an outline through points, a box with round corners, a filled path)
const rbLine = ps => "M" + ps.map(([x, y]) => f1(x) + " " + f1(y)).join(" L");
function rbRect(x0, y0, x1, y1, r = 0) {
  const a = Math.min(x0, x1), b = Math.max(x0, x1), c = Math.min(y0, y1), d = Math.max(y0, y1), q = Math.min(r, (b - a) / 2, (d - c) / 2);
  return q > 0 ? `M${f1(a + q)} ${f1(c)} L${f1(b - q)} ${f1(c)} Q${f1(b)} ${f1(c)} ${f1(b)} ${f1(c + q)} L${f1(b)} ${f1(d - q)} Q${f1(b)} ${f1(d)} ${f1(b - q)} ${f1(d)} ` +
                 `L${f1(a + q)} ${f1(d)} Q${f1(a)} ${f1(d)} ${f1(a)} ${f1(d - q)} L${f1(a)} ${f1(c + q)} Q${f1(a)} ${f1(c)} ${f1(a + q)} ${f1(c)} Z`
               : `M${f1(a)} ${f1(c)} L${f1(b)} ${f1(c)} L${f1(b)} ${f1(d)} L${f1(a)} ${f1(d)} Z`;
}
const rbPath = (d, fill, stroke = null, sw = 3, more = "") => `<path d="${d}" fill="${fill}"${stroke ? ` stroke="${stroke}" stroke-width="${sw}" stroke-linejoin="round"` : ""}${more}/>`;
const rbStroke = (d, stroke, sw, more = "") => `<path d="${d}" fill="none" stroke="${stroke}" stroke-width="${sw}"${more.includes("stroke-linecap") ? "" : ' stroke-linecap="round"'} stroke-linejoin="round"${more}/>`;
const rbDots = (ps, r) => ps.map(([x, y, rr]) => ellD(x, y, rr ?? r, rr ?? r)).join(" ");
// a gear: n teeth round (cx, cy), r to their tips, and its hole (filled evenodd)
const rbGear = (cx, cy, r, n = 8) => rbLine(Array.from({ length: 4 * n }, (_, i) => { const a = (i - 0.5) * Math.PI / (2 * n), rr = i % 4 < 2 ? r : 0.74 * r;
  return [cx + rr * Math.cos(a), cy + rr * Math.sin(a)]; })) + " Z " + ellD(cx, cy, 0.28 * r, 0.28 * r);
// the wind-up key's bow, a figure of eight: its lobes above and below (cx, cy), each `a` across and `h` tall; or left and right
const rbBow = (cx, cy, a, h, across = false) => [-1, 1].map(s => across
  ? `M${f1(cx)} ${f1(cy)} C${f1(cx + s * 0.3 * h)} ${f1(cy - a)} ${f1(cx + s * h)} ${f1(cy - a)} ${f1(cx + s * h)} ${f1(cy)} C${f1(cx + s * h)} ${f1(cy + a)} ${f1(cx + s * 0.3 * h)} ${f1(cy + a)} ${f1(cx)} ${f1(cy)} Z`
  : `M${f1(cx)} ${f1(cy)} C${f1(cx - a)} ${f1(cy + s * 0.3 * h)} ${f1(cx - a)} ${f1(cy + s * h)} ${f1(cx)} ${f1(cy + s * h)} C${f1(cx + a)} ${f1(cy + s * h)} ${f1(cx + a)} ${f1(cy + s * 0.3 * h)} ${f1(cx)} ${f1(cy)} Z`).join(" ");

// The head, in the face's units (x 100 the middle; the head from headCrown to headChin, 80 * headWidth either side), worn
// as the knight's helmet is. o: { dir, away, closed (the eyes shut, meditating), lieBack (lying face up) }. Returns its
// svg, its eyes (a faceplate's, drawn on the eyes' veil), `behind` (the inside of a window the face looks out of: one
// solid shape, drawn under the face, so nothing behind the friend shows round it), and how far up and out it reaches (for
// the picture's frame).
function robotHead(R, F, o) {
  const H = diveHead(F), LY = H.LY, hw = H.hw, { dir, away } = o, m = R.main, dk = R.dk, lt = R.lt, P = rbPath;
  const smile = Math.max(-1, Math.min(1, ((F.smile ?? 85) - 50) / 50));   // (the mood: 1 a big smile, -1 a frown; 50 or more smiles)
  const X = d => 100 + dir * d, Y = k => H.cy + k * H.ry;   // (across: toward the face in profile; down, in the head's half heights)
  let svg = "", eyes = "", behind = "", top = H.top, half = hw;
  switch (R.S.head) {
    // 🔑 the tin: a tall box with round top corners, a cone ear each side, one antenna with a bobble; facing us a printed
    // tin face (two round eyes, a grille mouth bending with the mood, rivets round it); in profile its side, the ear end on;
    // from behind a battery cover and a riveted seam
    case "tinbox": {
      const bw = 1.15 * hw, bot = H.chin + 12, ht = 2.6 * H.ry, tp = bot - ht, r = 0.3 * bw, ey = tp + 0.36 * ht, my = tp + 0.66 * ht;
      const sk = "#8a929c", box = (a, b) => `M${f1(a)} ${f1(bot)} L${f1(a)} ${f1(tp + r)} Q${f1(a)} ${f1(tp)} ${f1(a + r)} ${f1(tp)} L${f1(b - r)} ${f1(tp)} Q${f1(b)} ${f1(tp)} ${f1(b)} ${f1(tp + r)} L${f1(b)} ${f1(bot)} Z`;
      const ant = x => `<path d="M${f1(x)} ${f1(tp + 2)} L${f1(x)} ${f1(tp - 34)}" stroke="${sk}" stroke-width="6"/>` + P(ellD(x, tp - 42, 10, 10), lt, R.ltk, 3);
      const cone = sd => { const x0 = 100 + sd * (bw - 2), x1 = 100 + sd * (bw + 28); return `M${f1(x0)} ${f1(ey - 20)} L${f1(x1)} ${f1(ey - 7)} L${f1(x1)} ${f1(ey + 7)} L${f1(x0)} ${f1(ey + 20)} Z`; };
      const ring = (a, b, ins) => { const ps = [], n = 4;
        for (let i = 0; i <= n; i++) ps.push([a + ins + i * (b - a - 2 * ins) / n, tp + ins], [a + ins + i * (b - a - 2 * ins) / n, bot - ins]);
        for (let i = 1; i < 4; i++) ps.push([a + ins, tp + ins + i * (ht - 2 * ins) / 4], [b - ins, tp + ins + i * (ht - 2 * ins) / 4]);
        return P(rbDots(ps, 3.2), ROBOT_SILVER, sk, 1); };
      top = tp - 54; half = bw + 30;
      if (!dir) {
        svg = P(cone(-1) + " " + cone(1), ROBOT_SILVER, sk, 3) + P(box(100 - bw, 100 + bw), m, dk, 4) + ant(100) + ring(100 - bw, 100 + bw, 13);
        if (away) svg += P(rbRect(100 - 0.5 * bw, tp + 0.3 * ht, 100 + 0.5 * bw, tp + 0.74 * ht, 8), mix(m, "#000000", 0.15), dk, 3);
        else {
          // (the grille: its outline and bars one path, bending down in the middle to smile, up to frown)
          const x0 = 100 - 0.5 * bw, x1 = 100 + 0.5 * bw, mh = 24, c = 12 * smile, yAt = (x, y) => { const t = (x - x0) / (x1 - x0); return y + 4 * t * (1 - t) * c; };
          let d = `M${f1(x0)} ${f1(my)} Q100 ${f1(my + 2 * c)} ${f1(x1)} ${f1(my)} L${f1(x1)} ${f1(my + mh)} Q100 ${f1(my + mh + 2 * c)} ${f1(x0)} ${f1(my + mh)} Z`;
          for (let i = 1; i < 6; i++) { const x = x0 + i * (x1 - x0) / 6; d += ` M${f1(x)} ${f1(yAt(x, my))} L${f1(x)} ${f1(yAt(x, my) + mh)}`; }
          svg += P(d, "#2b2b2b", ROBOT_SILVER, 3.5);
          const ex = 0.42 * bw;
          eyes = P(ellD(100 - ex, ey, 17, 17) + " " + ellD(100 + ex, ey, 17, 17), lt, dk, 4) +
                 (o.closed ? rbStroke(`M${f1(100 - ex - 11)} ${f1(ey)} L${f1(100 - ex + 11)} ${f1(ey)} M${f1(100 + ex - 11)} ${f1(ey)} L${f1(100 + ex + 11)} ${f1(ey)}`, "#1f1f22", 4)
                           : P(ellD(100 - ex, ey, 7, 7) + " " + ellD(100 + ex, ey, 7, 7), "#1f1f22"));
        }
      } else {
        const bd = 0.85 * bw, fx = X(bd);
        half = bd + 4;
        svg = P(box(100 - bd, 100 + bd), m, dk, 4) + ant(100) + ring(100 - bd, 100 + bd, 12) +
              P(ellD(X(-0.06 * bd), ey + 2, 18, 18), ROBOT_SILVER, sk, 3) +
              P(rbRect(fx, my, fx - dir * 13, my + 24 + 2 * Math.max(0, 12 * smile) * 0.5, 3), "#2b2b2b", ROBOT_SILVER, 3);
        eyes = P(ellD(fx - dir * 3, ey, 7, 17), lt, dk, 3.5) +
               (o.closed ? rbStroke(`M${f1(fx - dir * 8)} ${f1(ey)} L${f1(fx + dir * 3)} ${f1(ey)}`, "#1f1f22", 3.5) : P(ellD(fx - dir * 1, ey, 3, 7), "#1f1f22"));
      }
      break;
    }
    // 📦 the box bot: a cardboard cube, a square hole cut for the face (the friend's own, mood and all), two pipe cleaners
    // with pom-poms; from the side its side panel with a marker dial (nobody sees into a box from the side: no face), the
    // front's edge and the hole's; from behind a packing-tape cross
    case "cube": {
      const bw = 1.25 * hw, bot = H.chin + 18, tp = bot - 1.96 * bw, wx = 0.64 * bw, wy0 = LY.eyeY - 42, wy1 = LY.mouthY + 32;
      const pipes = xs => rbStroke(xs.map(x => `M${f1(x)} ${f1(tp + 2)} Q${f1(x - 12)} ${f1(tp - 14)} ${f1(x)} ${f1(tp - 30)} Q${f1(x + 12)} ${f1(tp - 46)} ${f1(x)} ${f1(tp - 62)}`).join(" "), lt, 5) +
                         P(rbDots(xs.map(x => [x, tp - 70]), 11), lt, R.ltk, 2);
      top = tp - 84; half = bw;
      if (!dir && !away) { svg = P(rbRect(100 - bw, tp, 100 + bw, bot, 6) + " " + rbRect(100 - wx, wy0, 100 + wx, wy1, 3), m, dk, 4, ' fill-rule="evenodd"') + pipes([100 - 0.45 * bw, 100 + 0.45 * bw]);
        behind = P(rbRect(100 - wx, wy0, 100 + wx, wy1, 3), mix(m, "#000000", 0.5)); }   // (the box's dark inside, round the face)
      else if (away) svg = P(rbRect(100 - bw, tp, 100 + bw, bot, 6), m, dk, 4) +
                            `<path d="M${f1(100 - bw + 2)} ${f1((tp + bot) / 2)} L${f1(100 + bw - 2)} ${f1((tp + bot) / 2)} M100 ${f1(tp + 2)} L100 ${f1(bot - 2)}" stroke="#b8bec6" stroke-width="22" opacity="0.9"/>` +
                            pipes([100 - 0.45 * bw, 100 + 0.45 * bw]);
      else {
        const d = 0.92 * bw, fx = X(d), cx = X(-0.1 * bw), cy = tp + 0.56 * (bot - tp), rr = 0.3 * bw;
        svg = P(rbRect(100 - d, tp, 100 + d, bot, 6), m, dk, 4) +
              P(rbRect(fx, tp, fx + dir * 12, wy0, 2) + " " + rbRect(fx, wy1, fx + dir * 12, bot, 2), mix(m, "#ffffff", 0.25), dk, 3) +
              P(rbRect(fx, wy0, fx + dir * 12, wy1, 0), mix(m, "#000000", 0.5)) +
              rbStroke(ellD(cx, cy, rr, rr) + ` M${f1(cx)} ${f1(cy)} L${f1(cx + dir * 0.6 * rr)} ${f1(cy - 0.6 * rr)}`, dk, 3) +
              pipes([X(0.22 * bw), X(-0.28 * bw)]);
        half = d + 14;
      }
      break;
    }
    // 🤗 the helper bot: a white ball, a dark screen for a face (two glowing eyes and a smile following the mood; arcs when
    // the eyes shut), a disc ear each side, a short nub with a light; in profile the screen a crescent at its front; from
    // behind a speaker grille
    case "ball": {
      const r = 1.3 * hw, cy = H.chin + 14 - r, nt = cy - r, scr = "#20303a";
      const nub = x => P(rbRect(x - 6, nt - 16, x + 6, nt + 6, 3), "#f4f6f8", dk, 2.5) + P(ellD(x, nt - 20, 8, 8), lt, R.ltk, 2);
      const ball = P(ellD(100, cy, r, r), "#f4f6f8", dk, 4), c = 0.1 * r * smile;
      top = nt - 30; half = r + 17;
      if (!dir) {
        svg = P(ellD(100 - r, cy, 17, 17) + " " + ellD(100 + r, cy, 17, 17), m, dk, 3) + ball + nub(100);
        if (away) svg += rbStroke([-0.1, 0.05, 0.2, 0.35].map((v, i) => `M${f1(100 - (0.36 - 0.06 * i) * r)} ${f1(cy + v * r)} L${f1(100 + (0.36 - 0.06 * i) * r)} ${f1(cy + v * r)}`).join(" "), dk, 5);
        else {
          const y0 = cy - 0.42 * r, y1 = cy + 0.44 * r;
          svg += P(rbRect(100 - 0.74 * r, y0, 100 + 0.74 * r, y1, 0.3 * r), scr, dk, 3) +
                 rbStroke(`M${f1(100 - 0.62 * r)} ${f1(y0 + 0.3 * r)} Q${f1(100 - 0.6 * r)} ${f1(y0 + 0.08 * r)} ${f1(100 - 0.36 * r)} ${f1(y0 + 0.07 * r)}`, "#ffffff", 4, ' opacity="0.3"') +
                 rbStroke(`M${f1(100 - 0.24 * r)} ${f1(cy + 0.18 * r)} Q100 ${f1(cy + 0.18 * r + 2 * c)} ${f1(100 + 0.24 * r)} ${f1(cy + 0.18 * r)}`, lt, 6);
          const ex = 0.3 * r, ey = cy - 0.1 * r;
          eyes = o.closed ? rbStroke([-1, 1].map(s => `M${f1(100 + s * ex - 12)} ${f1(ey + 4)} Q${f1(100 + s * ex)} ${f1(ey - 8)} ${f1(100 + s * ex + 12)} ${f1(ey + 4)}`).join(" "), lt, 5)
                          : P(ellD(100 - ex, ey, 0.1 * r, 0.15 * r) + " " + ellD(100 + ex, ey, 0.1 * r, 0.15 * r), lt);
        }
      } else {
        const a1 = 40 * Math.PI / 180, a2 = 34 * Math.PI / 180;
        half = r + 4;
        svg = ball + P(ellD(X(-0.12 * r), cy + 0.04 * r, 0.18 * r, 0.18 * r), m, dk, 3) + nub(X(-0.06 * r)) +
              P(`M${f1(X(r * Math.cos(a1)))} ${f1(cy - r * Math.sin(a1))} A${f1(r)} ${f1(r)} 0 0 ${dir > 0 ? 1 : 0} ${f1(X(r * Math.cos(a2)))} ${f1(cy + r * Math.sin(a2))} Q${f1(X(0.52 * r))} ${f1(cy)} ${f1(X(r * Math.cos(a1)))} ${f1(cy - r * Math.sin(a1))} Z`, scr, dk, 3) +
              rbStroke(`M${f1(X(0.66 * r))} ${f1(cy + 0.2 * r)} Q${f1(X(0.76 * r))} ${f1(cy + 0.2 * r + 1.4 * c)} ${f1(X(0.86 * r))} ${f1(cy + 0.16 * r)}`, lt, 5);
        const ex = X(0.76 * r), ey = cy - 0.12 * r;
        eyes = o.closed ? rbStroke(`M${f1(ex - 6)} ${f1(ey + 3)} Q${f1(ex)} ${f1(ey - 6)} ${f1(ex + 6)} ${f1(ey + 3)}`, lt, 4.5) : P(ellD(ex, ey, 0.055 * r, 0.14 * r), lt);
      }
      break;
    }
    // 🚀 the space bot: a glass bubble all round the head on a collar ring (the friend's face inside, in every view), a snug
    // cap in its colour over the crown (the hair hidden), a light round its band, two lights on the collar
    case "bubble": {
      const rx = 1.5 * hw, ry = 1.62 * H.ry, cy = H.chin + 44 - ry, glass = ' fill-opacity="0.22"', cu = H.top - 12;
      const collar = cx => P(rbRect(cx - 0.66 * rx, H.chin + 24, cx + 0.66 * rx, H.chin + 50, 8), m, dk, 3) + P(rbDots([[cx - 0.36 * rx, H.chin + 37], [cx + 0.36 * rx, H.chin + 37]], 5), lt, R.ltk, 1.5);
      const bubble = cx => P(ellD(cx, cy, rx + (dir ? 4 : 0), ry), "#cfe8ff", "#8fb3cf", 4, glass) +
                           rbStroke(`M${f1(cx - 0.74 * rx)} ${f1(cy - 0.32 * ry)} Q${f1(cx - 0.58 * rx)} ${f1(cy - 0.84 * ry)} ${f1(cx - 0.12 * rx)} ${f1(cy - 0.95 * ry)}`, "#ffffff", 7, ' opacity="0.85"');
      top = cy - ry - 4; half = rx + 18;
      if (away) svg = P(ellD(100, H.cy - 4, hw + 5, H.ry + 6), m, dk, 3) + rbStroke(`M${f1(100 - hw + 2)} ${f1(H.cy + 44)} Q100 ${f1(H.cy + 66)} ${f1(100 + hw - 2)} ${f1(H.cy + 44)}`, lt, 6) + bubble(100) + collar(100);
      else if (!dir) svg = P(`M${f1(100 - hw - 4)} ${f1(Y(0.1))} Q${f1(100 - hw - 6)} ${f1(cu)} 100 ${f1(cu)} Q${f1(100 + hw + 6)} ${f1(cu)} ${f1(100 + hw + 4)} ${f1(Y(0.1))} Q${f1(100 + 0.62 * hw)} ${f1(Y(-0.63))} 100 ${f1(Y(-0.66))} Q${f1(100 - 0.62 * hw)} ${f1(Y(-0.63))} ${f1(100 - hw - 4)} ${f1(Y(0.1))} Z`, m, dk, 3) +
                           rbStroke(`M${f1(100 - hw + 4)} ${f1(Y(-0.08))} Q${f1(100 - 0.55 * hw)} ${f1(Y(-0.5))} 100 ${f1(Y(-0.53))} Q${f1(100 + 0.55 * hw)} ${f1(Y(-0.5))} ${f1(100 + hw - 4)} ${f1(Y(-0.08))}`, lt, 6) + bubble(100) + collar(100);
      else svg = P(`M${f1(X(0.62 * hw))} ${f1(Y(-0.58))} Q${f1(X(0.5 * hw))} ${f1(cu)} ${f1(X(-0.15 * hw))} ${f1(cu)} Q${f1(X(-hw - 8))} ${f1(cu + 6)} ${f1(X(-hw - 6))} ${f1(Y(0.12))} Q${f1(X(-hw - 2))} ${f1(Y(0.69))} ${f1(X(-0.6 * hw))} ${f1(Y(0.77))} Q${f1(X(-0.2 * hw))} ${f1(Y(0.2))} ${f1(X(0.62 * hw))} ${f1(Y(-0.58))} Z`, m, dk, 3) +
                 rbStroke(`M${f1(X(0.5 * hw))} ${f1(Y(-0.48))} Q${f1(X(-0.2 * hw))} ${f1(Y(0.07))} ${f1(X(-0.52 * hw))} ${f1(Y(0.64))}`, lt, 6) + bubble(X(-12)) + collar(X(-10));
      break;
    }
    // 🦾 the big mech: a short wide box, low with no neck, the friend in its cockpit window behind faint glass in a thick
    // frame, a hazard stripe on the brow, two headlamps, an orange beacon on top (still, never blinking); in profile deeper
    // than it is wide, the window a strip on its front edge, a vent on its side; from behind a grille and two bolts
    case "cockpit": {
      const bw = 1.25 * hw, tp = H.top - 14, bot = H.chin + 30, frame = "#3f3f46";
      const beacon = x => P(`M${f1(x - 18)} ${f1(tp + 2)} Q${f1(x - 18)} ${f1(tp - 30)} ${f1(x)} ${f1(tp - 30)} Q${f1(x + 18)} ${f1(tp - 30)} ${f1(x + 18)} ${f1(tp + 2)} Z`, lt, R.ltk, 3) + P(ellD(x - 6, tp - 17, 4.5, 4.5), "#fff4dc");
      top = tp - 34; half = 1.2 * bw;
      if (!dir && !away) {
        const wx = 0.64 * bw, w0 = LY.eyeY - 40, w1 = LY.mouthY + 30;
        behind = P(rbRect(100 - wx, w0, 100 + wx, w1, 10), "#33383f");   // (the cockpit's dark inside)
        svg = beacon(100) + P(rbRect(100 - bw, tp, 100 + bw, bot, 22) + " " + rbRect(100 - wx, w0, 100 + wx, w1, 10), m, dk, 4, ' fill-rule="evenodd"') +
              P(rbRect(100 - wx, w0, 100 + wx, w1, 10), "#bae6fd", frame, 9, ' fill-opacity="0.2"') +
              `<path d="M${f1(100 - bw + 36)} ${f1(tp + 21)} L${f1(100 + bw - 36)} ${f1(tp + 21)}" stroke="#1f2937" stroke-width="13" stroke-dasharray="11 9"/>` +
              P(rbRect(100 - bw + 8, tp + 12, 100 - bw + 26, tp + 30, 3) + " " + rbRect(100 + bw - 26, tp + 12, 100 + bw - 8, tp + 30, 3), lt, R.ltk, 2.5);
      } else if (away) svg = beacon(100) + P(rbRect(100 - bw, tp, 100 + bw, bot, 22), m, dk, 4) +
          rbStroke([0, 1, 2, 3, 4].map(i => `M${f1(100 - 0.5 * bw)} ${f1(tp + 54 + 22 * i)} L${f1(100 + 0.5 * bw)} ${f1(tp + 54 + 22 * i)}`).join(" "), frame, 7) +
          P(rbDots([[100 - 0.8 * bw, bot - 20], [100 + 0.8 * bw, bot - 20]], 7), ROBOT_SILVER, frame, 2.5);
      else {
        const xb = X(-1.2 * bw), xf = X(0.9 * bw), s0 = X(0.4 * hw), s1 = X(0.98 * hw), w0 = LY.eyeY - 38, w1 = LY.mouthY + 26;
        behind = P(rbRect(s0, w0, s1, w1, 8), "#33383f");
        svg = beacon(X(-0.15 * bw)) + P(rbRect(xb, tp, xf, bot, 22) + " " + rbRect(s0, w0, s1, w1, 8), m, dk, 4, ' fill-rule="evenodd"') +
              P(rbRect(s0, w0, s1, w1, 8), "#bae6fd", frame, 8, ' fill-opacity="0.2"') +
              P(rbRect(xf - dir * 26, tp + 12, xf - dir * 8, tp + 30, 3), lt, R.ltk, 2.5) +
              rbStroke([0, 1, 2].map(i => `M${f1(X(-1.0 * bw))} ${f1(H.cy - 10 + 22 * i)} L${f1(X(-0.35 * bw))} ${f1(H.cy - 10 + 22 * i)}`).join(" "), frame, 8);
      }
      break;
    }
    // ⚡ the hero bot: a helmet with cheek guards (the friend's face in its opening, the chin clear), a wide shallow V crest
    // of gold with a red gem at its root, a gold visor over the eyes (tinted, see-through), a vent at each ear; in profile one
    // broad blade sweeping back; from behind a fin down its middle and the blades' darker backs
    case "helm": {
      const sx = hw + 12, g = 0.6 * hw, gy = H.chin - 24, ct = H.top - 16, gold = "#facc15", gk = "#a16207";
      const crestV = (fill, line) => P([-1, 1].map(s => `M100 ${f1(ct + 18)} L${f1(100 + s * 132)} ${f1(ct - 62)} Q${f1(100 + s * 64)} ${f1(ct + 2)} 100 ${f1(ct + 42)} Z`).join(" "), fill, line, 3);
      const gem = x => P(ellD(x, ct + 30, 10, 10), "#ef4444", "#7f1d1d", 2.5);
      top = ct - 66; half = 136;
      if (away) svg = crestV(darker(gold), "#713f12") +
          P(`M${f1(100 - sx)} ${f1(H.chin - 14)} L${f1(100 - sx)} ${f1(H.cy)} Q${f1(100 - sx)} ${f1(ct)} 100 ${f1(ct)} Q${f1(100 + sx)} ${f1(ct)} ${f1(100 + sx)} ${f1(H.cy)} L${f1(100 + sx)} ${f1(H.chin - 14)} Q100 ${f1(H.chin + 6)} ${f1(100 - sx)} ${f1(H.chin - 14)} Z`, m, dk, 4) +
          P(`M94 ${f1(ct + 6)} Q100 ${f1(ct - 12)} 106 ${f1(ct + 6)} L105 ${f1(H.chin - 4)} L95 ${f1(H.chin - 4)} Z`, mix(m, "#000000", 0.22), dk, 2);
      else if (!dir) {
        svg = P(`M${f1(100 - sx + 4)} ${f1(gy)} L${f1(100 - sx)} ${f1(H.cy)} Q${f1(100 - sx)} ${f1(ct)} 100 ${f1(ct)} Q${f1(100 + sx)} ${f1(ct)} ${f1(100 + sx)} ${f1(H.cy)} L${f1(100 + sx - 4)} ${f1(gy)} ` +
                `Q${f1(100 + g + 18)} ${f1(gy + 6)} ${f1(100 + g)} ${f1(gy - 6)} L${f1(100 + g)} ${f1(LY.eyeY + 4)} Q${f1(100 + g)} ${f1(LY.eyeY - 36)} 100 ${f1(LY.eyeY - 38)} Q${f1(100 - g)} ${f1(LY.eyeY - 36)} ${f1(100 - g)} ${f1(LY.eyeY + 4)} L${f1(100 - g)} ${f1(gy - 6)} Q${f1(100 - g - 18)} ${f1(gy + 6)} ${f1(100 - sx + 4)} ${f1(gy)} Z`, m, dk, 4) +
              rbStroke([-1, 1].map(s => [0, 1, 2].map(i => `M${f1(100 + s * (g + 14))} ${f1(H.cy + 8 + 12 * i)} L${f1(100 + s * (sx - 12))} ${f1(H.cy + 8 + 12 * i)}`).join(" ")).join(" "), dk, 4) +
              crestV(gold, gk) + gem(100) +
              P(rbRect(100 - g - 10, LY.eyeY - 20, 100 + g + 10, LY.eyeY + 18, 12), "#ffd34d", "#b45309", 3, ' fill-opacity="0.45"');
      } else {
        svg = P(`M${f1(X(-sx - 2))} ${f1(H.chin - 16)} L${f1(X(-sx - 4))} ${f1(H.cy)} Q${f1(X(-sx - 4))} ${f1(ct)} ${f1(X(-0.1 * sx))} ${f1(ct)} Q${f1(X(sx - 2))} ${f1(ct + 2)} ${f1(X(sx - 4))} ${f1(LY.eyeY - 24)} ` +
                `L${f1(X(0.3 * hw))} ${f1(LY.eyeY - 20)} L${f1(X(0.22 * hw))} ${f1(gy)} Q${f1(X(0.05 * hw))} ${f1(gy + 10)} ${f1(X(-0.4 * hw))} ${f1(H.chin - 14)} Z`, m, dk, 4) +
              rbStroke([0, 1, 2].map(i => `M${f1(X(-0.3 * hw))} ${f1(H.cy + 4 + 12 * i)} L${f1(X(-0.72 * hw))} ${f1(H.cy + 4 + 12 * i)}`).join(" "), dk, 4) +
              P(`M${f1(X(0.62 * hw))} ${f1(ct + 18)} L${f1(X(-1.3 * hw))} ${f1(ct - 76)} Q${f1(X(-0.1 * hw))} ${f1(ct - 4)} ${f1(X(0.45 * hw))} ${f1(ct + 42)} Z`, gold, gk, 3) + gem(X(0.58 * hw)) +
              P(rbRect(X(0.24 * hw), LY.eyeY - 20, X(hw + 6), LY.eyeY + 16, 9), "#ffd34d", "#b45309", 3, ' fill-opacity="0.45"');
        top = ct - 80; half = 1.3 * hw + 6;
      }
      break;
    }
    // 📺 the TV bot: an old set for a head, the friend's face on its screen (behind glass glowing faintly, with a glare),
    // two knobs and a speaker on the wider bezel, rabbit-ear antennas, a short neck stalk; in profile its deep side, the
    // screen edge on, the picture tube's housing tapering back (flat, lying face up: its own drawing, so nothing goes
    // through the floor); from behind the tube's round back, vents and the socket
    case "set": {
      const ht = 2.3 * H.ry, bot = H.chin + 8, tp = bot - ht, cl = 100 - 1.36 * hw, cr = 100 + 1.84 * hw, sw = 1.12 * hw, sy0 = tp + 12, sy1 = bot - 12, cream = "#d9d2c3";
      const stalk = x => P(rbRect(x - 14, bot - 6, x + 14, bot + 30, 3), cream, darker(cream), 2.5);
      const ears = (x0, xa, xb) => rbStroke(`M${f1(xa)} ${f1(tp - 144)} L${f1(x0)} ${f1(tp + 2)} L${f1(xb)} ${f1(tp - 144)}`, "#d1d5db", 5) + P(rbDots([[xa, tp - 148], [xb, tp - 148]], 8), "#374151");
      top = tp - 158; half = 1.84 * hw;
      if (!dir && !away) {
        behind = P(rbRect(100 - sw, sy0, 100 + sw, sy1, 22), mix(lt, "#000000", 0.72));   // (the screen, dark round the face's picture)
        svg = stalk(100) + ears(100, 100 - 0.42 * hw, 100 + 0.42 * hw) +
              P(rbRect(cl, tp, cr, bot, 16) + " " + rbRect(100 - sw, sy0, 100 + sw, sy1, 22), m, dk, 4, ' fill-rule="evenodd"') +
              P(rbRect(100 - sw, sy0, 100 + sw, sy1, 22), lt, lt, 5, ' fill-opacity="0.15"') +
              rbStroke(`M${f1(100 - sw + 16)} ${f1(sy0 + 50)} Q${f1(100 - sw + 18)} ${f1(sy0 + 16)} ${f1(100 - sw + 54)} ${f1(sy0 + 12)}`, "#ffffff", 6, ' opacity="0.6"') +
              P(rbDots([[cr - 27, tp + 46], [cr - 27, tp + 86]], 12), cream, dk, 3) +
              rbStroke([0, 1, 2, 3].map(i => `M${f1(cr - 42)} ${f1(tp + 118 + 12 * i)} L${f1(cr - 12)} ${f1(tp + 118 + 12 * i)}`).join(" "), dk, 4);
      } else if (away) {
        const a = 100 - (cr - 100), b = 100 + (100 - cl);
        svg = stalk(100) + ears(100, 100 - 0.42 * hw, 100 + 0.42 * hw) + P(rbRect(a, tp, b, bot, 16), m, dk, 4) +
              P(rbRect(100 - 0.95 * hw, tp + 24, 100 + 0.95 * hw, bot - 24, 40), mix(m, "#000000", 0.25), dk, 3) +
              rbStroke([0, 1, 2, 3].map(i => `M${f1(100 - 0.45 * hw)} ${f1(tp + 52 + 16 * i)} L${f1(100 + 0.45 * hw)} ${f1(tp + 52 + 16 * i)}`).join(" "), "#1f2937", 5) +
              P(rbRect(100 - 13, bot - 64, 100 + 13, bot - 44, 3), "#1f2937");
      } else {
        const xf = X(0.95 * hw), xb = X(o.lieBack ? -0.9 * hw : -0.35 * hw), xt = X(-1.45 * hw);
        svg = stalk(X(-0.1 * hw)) + ears(X(-0.1 * hw), X(-0.6 * hw), X(-0.32 * hw)) +
              (o.lieBack ? "" : P(rbLine([[xb, tp + 14], [xt, tp + 0.3 * ht], [xt, bot - 0.3 * ht], [xb, bot - 14]]) + " Z", mix(m, "#000000", 0.3), dk, 3) +
                                rbStroke([0, 1, 2].map(i => `M${f1(X(-0.55 * hw))} ${f1(H.cy - 18 + 18 * i)} L${f1(X(-1.15 * hw))} ${f1(H.cy - 18 + 18 * i)}`).join(" "), "#1f2937", 4)) +
              P(rbRect(xb, tp, xf, bot, 12), m, dk, 4) + P(rbRect(xf - dir * 16, tp, xf, bot, 6), mix(m, "#000000", 0.15), dk, 3) +
              P(rbRect(xf, sy0 + 6, xf + dir * 6, sy1 - 6, 2), lt, lt, 1.5) + P(ellD(xf - dir * 8, tp + 42, 7, 7), cream, dk, 2.5);
        half = o.lieBack ? 0.95 * hw : 1.45 * hw;
      }
      break;
    }
    // 🕷️ the spider bot: a close round dome over the crown and down the sides to the jaw, the face in an oval opening (no
    // cheek guards), six red lens eyes in two rows on the brow; in profile three lenses at its front; from behind a ridge
    case "dome": {
      const ox = hw + 12, oy = H.ry + 12, cy = H.cy - 6, glint = "#ffffff";
      top = cy - oy - 4; half = ox;
      if (!dir && !away) {
        const L1 = [[100 - 17, cy - oy + 26, 8.5], [100 + 17, cy - oy + 26, 8.5], [100 - 37, cy - oy + 44, 6.5], [100 - 12, cy - oy + 46, 6.5], [100 + 12, cy - oy + 46, 6.5], [100 + 37, cy - oy + 44, 6.5]];
        svg = P(ellD(100, cy, ox, oy) + " " + ellD(100, (LY.eyeY + LY.mouthY) / 2 + 3, 0.74 * hw, 60), m, dk, 4, ' fill-rule="evenodd"') +
              P(rbDots(L1, 0), lt, "#450a0a", 2) + P(rbDots(L1.map(([x, y, r]) => [x - r * 0.35, y - r * 0.35, r * 0.3]), 0), glint);
      } else if (away) svg = P(ellD(100, cy, ox, oy), m, dk, 4) + P([0, 1, 2].map(i => rbRect(92, cy - oy + 14 + 46 * i, 108, cy - oy + 52 + 46 * i, 7)).join(" "), mix(m, "#000000", 0.3), dk, 2);
      else {
        const L1 = [[X(0.66 * hw), cy - oy + 30, 8], [X(0.84 * hw), cy - oy + 48, 6.5], [X(0.5 * hw), cy - oy + 44, 6]];
        svg = P(`M${f1(X(0.8 * hw))} ${f1(Y(-0.5))} Q${f1(X(hw + 4))} ${f1(cy - oy + 6)} ${f1(X(0.05 * hw))} ${f1(cy - oy)} Q${f1(X(-ox - 6))} ${f1(cy - oy + 8)} ${f1(X(-ox))} ${f1(cy)} ` +
                `Q${f1(X(-ox + 4))} ${f1(cy + oy - 6)} ${f1(X(-0.2 * hw))} ${f1(cy + oy)} L${f1(X(0.18 * hw))} ${f1(cy + oy - 8)} Q${f1(X(0.34 * hw))} ${f1(Y(0.12))} ${f1(X(0.8 * hw))} ${f1(Y(-0.5))} Z`, m, dk, 4) +
              P(rbDots(L1, 0), lt, "#450a0a", 2) + P(rbDots(L1.map(([x, y, r]) => [x - r * 0.35, y - r * 0.35, r * 0.3]), 0), glint);
      }
      break;
    }
  }
  return { svg, eyes, behind, top, half };
}

// The torso, in the friend's own units (the shirt from the neck at -20 to its hem at L; w its half width, as drawn in this
// view: 0.62 of it in profile), over the shirt. back: seen from behind. cr: crawling toward us or away (robotCrawlTorso).
function robotTorso(R, w, L, dir, back, cr = null) {
  const m = R.main, dk = R.dk, lt = R.lt, P = rbPath;
  if (cr) return robotCrawlTorso(R, w, L, cr);
  switch (R.S.torso) {
    // 🔑 a straight tin column, flaring at the hips into a short hem (front and back in profile); facing us a dark window
    // with two gears and a light
    case "column": {
      const c = w + 3, h = w + 13, y0 = L - 18, y1 = L + 12;
      let t = P(`M${f1(-c)} -8 Q${f1(-c)} -24 ${f1(-c + 14)} -24 L${f1(c - 14)} -24 Q${f1(c)} -24 ${f1(c)} -8 L${f1(c)} ${f1(y0)} L${f1(h)} ${f1(y1)} L${f1(-h)} ${f1(y1)} L${f1(-c)} ${f1(y0)} Z`, m, dk, 3) +
              `<path d="M${f1(-c)} ${f1(y0)} L${f1(c)} ${f1(y0)}" stroke="${dk}" stroke-width="2.5"/>`;
      if (!dir && !back) { const gx = 0.52 * w, g0 = 12, g1 = 12 + 0.32 * L, gm = (g0 + g1) / 2;
        t += P(rbRect(-gx, g0, gx, g1, 6), "#333333", dk, 2.5) + P(rbGear(-0.2 * w, gm + 2, 0.22 * w) + " " + rbGear(0.15 * w, gm - 4, 0.15 * w, 7), ROBOT_SILVER, "#6b7280", 1.5, ' fill-rule="evenodd"') +
             P(ellD(0.38 * w, g0 + 10, 5, 5), lt, R.ltk, 1.5); }
      return t;
    }
    // 📦 a big cardboard box from the shoulders to just below the hips, its top flaps sticking up at the corners, a strip of
    // tape across; facing us a marker dial and three bottle-cap buttons, from behind a marker arrow; in profile its side
    // (0.7 as deep as it is wide) and the near flap
    case "box": {
      const y0 = -26, y1 = L + 16, tape = (a, b) => P(rbRect(a, L - 32, b, L - 18, 0), "#b8bec6", "#8f969e", 1.5), flap = mix(m, "#000000", 0.12);
      if (dir) { const d = 1.47 * w;
        return P(`M${f1(-d + 4)} ${f1(y0)} L${f1(-d - 6)} ${f1(y0 - 24)} L${f1(d + 6)} ${f1(y0 - 30)} L${f1(d - 4)} ${f1(y0)} Z`, flap, dk, 2.5) + P(rbRect(-d, y0, d, y1, 3), m, dk, 3) + tape(-d, d); }
      const b = 1.3 * w, flaps = [-1, 1].map(s => `M${f1(s * b)} ${f1(y0)} L${f1(s * (b + 22))} ${f1(y0 - 30)} L${f1(s * 0.54 * b)} ${f1(y0 - 36)} L${f1(s * 0.58 * b)} ${f1(y0)} Z`).join(" ");
      let t = P(flaps, flap, dk, 2.5) + P(rbRect(-b, y0, b, y1, 3), m, dk, 3) + tape(-b, b);
      if (back) t += rbStroke(`M0 ${f1(0.62 * L)} L0 ${f1(0.12 * L)} M-16 ${f1(0.12 * L + 18)} L0 ${f1(0.12 * L)} L16 ${f1(0.12 * L + 18)}`, dk, 4);
      else { const cx = -0.42 * b, cy = 0.3 * L, r = 0.2 * b;
        t += rbStroke(ellD(cx, cy, r, r) + ` M${f1(cx)} ${f1(cy)} L${f1(cx + 0.65 * r)} ${f1(cy - 0.6 * r)}`, dk, 3) +
             P(rbDots([[0.3 * b, 0.22 * L], [0.55 * b, 0.22 * L], [0.42 * b, 0.36 * L]], 7), lt, R.ltk, 2); }
      return t;
    }
    // 🤗 a white egg from the shoulders over the hips, widest at the belly, a coloured band round it; facing us a glowing
    // heart, from behind a charging hatch; in profile rounder in front, like a belly. It is the body: the shirt's shape is not
    // drawn under it (drawNow)
    case "egg": {
      const y0 = -26, y1 = L + 22, cy = (y0 + y1) / 2, ry = (y1 - y0) / 2, wh = "#f4f6f8";
      const band = (cx, rx) => rbStroke(`M${f1(cx - 0.97 * rx)} ${f1(cy + 12)} Q${f1(cx)} ${f1(cy + 36)} ${f1(cx + 0.97 * rx)} ${f1(cy + 12)}`, m, 10, ' stroke-linecap="butt"');
      if (dir) { const rx = 1.45 * w, cx = dir * 6;
        return P(`M${f1(cx)} ${f1(y0)} C${f1(cx + dir * 1.25 * rx)} ${f1(y0)} ${f1(cx + dir * 1.2 * rx)} ${f1(y1)} ${f1(cx)} ${f1(y1)} C${f1(cx - dir * 1.05 * rx)} ${f1(y1)} ${f1(cx - dir * 1.1 * rx)} ${f1(y0)} ${f1(cx)} ${f1(y0)} Z`, wh, dk, 3) + band(cx, 0.9 * rx); }
      const rx = 1.35 * w;
      let t = P(ellD(0, cy, rx, ry), wh, dk, 3) + band(0, rx);
      if (back) t += P(rbRect(-0.42 * rx, cy - 0.52 * ry, 0.42 * rx, cy - 0.02 * ry, 10) + ` M-10 ${f1(cy - 0.3 * ry)} L10 ${f1(cy - 0.3 * ry)}`, mix(wh, m, 0.18), dk, 2.5);
      else { const hy = cy - 0.42 * ry;
        t += P(`M0 ${f1(hy + 10)} C-18 ${f1(hy - 4)} -12 ${f1(hy - 20)} 0 ${f1(hy - 9)} C12 ${f1(hy - 20)} 18 ${f1(hy - 4)} 0 ${f1(hy + 10)} Z`, lt, R.ltk, 2); }
      return t;
    }
    // 🚀 a rounded capsule chest plate; facing us a dial with its needle
    case "capsule": {
      const pl = "#eef2f6", pk = "#9aa6b2";
      if (dir) return P(rbRect(-(w + 2), -22, w + 2, L - 14, w), pl, pk, 3);
      if (back) return P(rbRect(-(w - 2), -22, w - 2, L - 14, 0.5 * w), "#e4e9ef", pk, 3);
      return P(rbRect(-(w - 2), -22, w - 2, L - 14, 0.5 * w), pl, pk, 3) + P(ellD(0, 0.3 * L, 17, 17), "#1f2937", dk, 3) +
             rbStroke(`M0 ${f1(0.3 * L)} L11 ${f1(0.3 * L - 9)}`, lt, 3.5);
    }
    // 🦾 a square chest plate and a belt; facing us a round fan vent (its shoulder blocks are the arms' pieces: robotArm)
    case "chest": {
      const belt = (a, b) => P(rbRect(a, L - 18, b, L - 3, 0), "#27272a", "#18181b", 2);
      if (dir) return P(rbRect(dir * (w - 12), -8, dir * (w + 6), 0.62 * L, 4), m, dk, 3) + belt(-(w + 3), w + 3);
      if (back) return P(rbRect(-(w - 8), -8, w - 8, 0.62 * L, 8), mix(m, "#000000", 0.15), dk, 3) + belt(-(w + 4), w + 4);
      const vy = 0.3 * L;
      return P(rbRect(-(w - 6), -8, w - 6, 0.62 * L, 8), m, dk, 3) + P(ellD(0, vy, 22, 22), "#3f3f46", dk, 3) +
             P([0, 1, 2, 3].map(i => { const a = i * Math.PI / 2 + 0.4, c = Math.cos(a), s = Math.sin(a);
               return rbLine([[3 * c, vy + 3 * s], [18 * c - 7 * s, vy + 18 * s + 7 * c], [18 * c + 5 * s, vy + 18 * s - 5 * c]]) + " Z"; }).join(" "), "#9ca3af") + belt(-(w + 4), w + 4);
    }
    // ⚡ a silver chest plate with a glowing round core, and a V belt with a gold buckle; from behind a back plate with two
    // thruster vents (no core); in profile the plate edge on
    case "plate": {
      const silver = ROBOT_SILVER, red = R.red;
      const vbelt = s => P(`M${f1(-(w + 2))} ${f1(L - 20)} L0 ${f1(L + s)} L${f1(w + 2)} ${f1(L - 20)} L${f1(w + 2)} ${f1(L - 7)} L0 ${f1(L + s + 13)} L${f1(-(w + 2))} ${f1(L - 7)} Z`, red, darker(red), 2);
      if (dir) return P(`M${f1(dir * (w - 8))} -16 L${f1(dir * (w + 4))} -10 L${f1(dir * (w + 2))} ${f1(0.5 * L)} L${f1(dir * (w - 12))} ${f1(0.58 * L)} Z`, silver, "#6b7280", 2.5) +
                      P(rbRect(-(w + 2), L - 20, w + 2, L - 7, 0), red, darker(red), 2);
      if (back) return P(rbRect(-(w - 10), -12, w - 10, 0.5 * L, 12), mix(m, "#000000", 0.2), dk, 3) +
                       P(rbRect(-0.55 * w, 0.18 * L, -0.15 * w, 0.4 * L, 6) + " " + rbRect(0.15 * w, 0.18 * L, 0.55 * w, 0.4 * L, 6), lt, R.ltk, 2) + vbelt(-4);
      return P(`M${f1(-(w - 4))} -16 L${f1(w - 4)} -16 L${f1(w - 10)} ${f1(0.5 * L)} Q0 ${f1(0.68 * L)} ${f1(-(w - 10))} ${f1(0.5 * L)} Z`, silver, "#6b7280", 2.5) +
             P(ellD(0, 0.24 * L, 22, 22), "none", dk, 4) + P(ellD(0, 0.24 * L, 15, 15), lt, R.ltk, 2) + vbelt(4) +
             P(rbRect(-8, L - 2, 8, L + 12, 2), "#facc15", "#a16207", 2);
    }
    // 📺 no shell: the friend's own body in cream, a small control panel facing us
    case "panel":
      return dir || back ? "" : P(rbRect(-0.42 * w, 0.2 * L, 0.42 * w, 0.2 * L + 44, 5), mix("#d9d2c3", "#000000", 0.12), "#8a8274", 2.5) +
                                P(rbDots([[-0.18 * w, 0.2 * L + 22], [0.18 * w, 0.2 * L + 22]], 7), lt, R.ltk, 2);
    // 🕷️ a segmented torso plate: its bands across the front, the back, or the side
    case "segments": {
      const ys = [0.12, 0.34, 0.56, 0.78].map(f => f * L), a = dir ? w : w - 2, k = back ? -8 : 10;
      return rbStroke(ys.map(y => dir ? `M${f1(-a)} ${f1(y)} Q0 ${f1(y + 6)} ${f1(a)} ${f1(y + 2)}` : `M${f1(-a)} ${f1(y)} Q0 ${f1(y + k)} ${f1(a)} ${f1(y)}`).join(" "), dk, 3);
    }
  }
  return "";
}

// Crawling toward us or away (ticket 286's crawl: the body runs into the picture, its back up), the torso's own drawing, in
// the friend's units as the picture has them, never the from-behind drawing turned over or seen short. cr: { to } crawling
// toward us (the back rising behind the head from the shoulders at -20, that share of its length: the hips far, up the
// picture), or { away } (the back rising from the hips at L toward the head, that share: the shoulders far). Fy carries a
// place along the back (in the shirt's own units, neck -20 to hem L) to the picture; what stands off the back by h shows
// 0.9 h higher (seen from a little above). Each shell shows its back as seen from there and, where it has depth, its near
// end (toward us the shoulders' end, away the hips'): the tin's column end on, the box's open top and its side flaps (toward
// us) or its closed bottom (away), the helper's egg round (its cross-section, not a flat ring), the plates thin on the back.
function robotCrawlTorso(R, w, L, cr) {
  const m = R.main, dk = R.dk, lt = R.lt, P = rbPath, to = !!cr.to, k = cr.to || cr.away;
  const Fy = y => to ? -k * y - 20 * (1 + k) : L - k * (L - y);
  const near = (yTop, yBot) => to ? Fy(yTop) : Fy(yBot);   // (the near end's line in the picture: the shoulders' toward us, the hips' away)
  const band = (x0, x1, ya, yb, r, fill, line, sw = 2.5) => P(rbRect(x0, Fy(ya), x1, Fy(yb), r), fill, line, sw);
  const pts = ps => rbLine(ps.map(([x, y, h = 0]) => [x, Fy(y) - 0.9 * h]));
  switch (R.S.torso) {
    // 🔑 the column: its back, the hem flaring at the hips, and its near end on (the shoulders' end, rounded; the hem's rim)
    case "column": {
      const c = w + 3, h = w + 13, y0 = L - 18, y1 = L + 12, sh = mix(m, "#000000", 0.18);
      const end = to ? P(rbRect(-c, Fy(-24), c, Fy(-24) + 24, 12), sh, dk, 3) : P(rbRect(-h, Fy(y1) - 2, h, Fy(y1) + 14, 4), sh, dk, 3);
      return end + P(rbRect(-c, Fy(-24), c, Fy(y0), 6), m, dk, 3) + P(pts([[-c, y0], [c, y0], [h, y1], [-h, y1]]) + " Z", m, dk, 3);
    }
    // 📦 the box: its back with the tape and the marker arrow; toward us its open top, dark inside, the side flaps opened out
    // either side; away its closed bottom end on, the flaps far up at the top's corners
    case "box": {
      const y0 = -26, y1 = L + 16, b = 1.3 * w, flap = mix(m, "#000000", 0.12), inside = mix(m, "#000000", 0.5);
      let t = "";
      if (to) { const e = Fy(y0), d = 40;
        t = P([-1, 1].map(s => rbLine([[s * b, e + 2], [s * (b + 30), e - 8], [s * (b + 26), e + d + 6], [s * b, e + d - 2]]) + " Z").join(" "), flap, dk, 2.5) +
            P(rbRect(-b, e, b, e + d, 3), inside, dk, 3); }
      else t = P([-1, 1].map(s => pts([[s * b, y0], [s * (b + 22), y0 - 30], [s * 0.54 * b, y0 - 36], [s * 0.58 * b, y0]]) + " Z").join(" "), flap, dk, 2.5) +
               P(rbRect(-b, Fy(y1), b, Fy(y1) + 30, 3), flap, dk, 3);
      return t + band(-b, b, y0, y1, 3, m, dk, 3) + band(-b, b, L - 32, L - 18, 0, "#b8bec6", "#8f969e", 1.5) +
             rbStroke(pts([[0, 0.62 * L], [0, 0.12 * L]]) + " " + pts([[-16, 0.12 * L + 18], [0, 0.12 * L], [16, 0.12 * L + 18]]), dk, 4);
    }
    // 🤗 the egg, round from either end: its widest round (as wide as the egg, as deep as it is from the side, and the length
    // it shows), the band round its middle arching over its back (its lower half under the egg), the hatch on top (away)
    case "egg": {
      const y0 = -26, y1 = L + 22, cy = (y0 + y1) / 2, ry = (y1 - y0) / 2, rx = 1.35 * w, dz = 0.9 * w, wh = "#f4f6f8";
      const Ry = Math.hypot(k * ry, 0.9 * dz), bc = Fy(cy + 24), a = 0.97 * rx, bb = 0.7 * dz;
      return P(ellD(0, Fy(cy), rx, Ry), wh, dk, 3) + rbStroke(`M${f1(-a)} ${f1(bc)} A${f1(a)} ${f1(bb)} 0 0 1 ${f1(a)} ${f1(bc)}`, m, 10, ' stroke-linecap="butt"') +
             (to ? "" : P(rbRect(-0.42 * rx, Fy(cy - 0.52 * ry) - 0.55 * dz, 0.42 * rx, Fy(cy - 0.02 * ry) - 0.55 * dz, 8), mix(wh, m, 0.18), dk, 2.5));
    }
    // 🚀 the back plate on the back, its near edge's thickness under it
    case "capsule": { const pk = "#9aa6b2", y = near(-22, L - 14);
      return P(rbRect(-(w - 2), y, w - 2, y + 7, 3), mix("#e4e9ef", "#000000", 0.15), pk, 2) + band(-(w - 2), w - 2, -22, L - 14, 0.4 * w, "#e4e9ef", pk, 3); }
    // 🦾 the back plate and the belt round the waist (near, away; far up, toward us)
    case "chest": { const y = near(-8, 0.62 * L);
      return P(rbRect(-(w - 8), y, w - 8, y + 7, 3), mix(m, "#000000", 0.3), dk, 2) + band(-(w - 8), w - 8, -8, 0.62 * L, 8, mix(m, "#000000", 0.15), dk, 3) +
             band(-(w + 4), w + 4, L - 18, L - 3, 0, "#27272a", "#18181b", 2); }
    // ⚡ the back plate with its two thruster vents, and the V belt at the waist
    case "plate": { const red = R.red, y = near(-12, 0.5 * L);
      return P(rbRect(-(w - 10), y, w - 10, y + 7, 3), mix(m, "#000000", 0.35), dk, 2) + band(-(w - 10), w - 10, -12, 0.5 * L, 10, mix(m, "#000000", 0.2), dk, 3) +
             P(rbRect(-0.55 * w, Fy(0.18 * L), -0.15 * w, Fy(0.4 * L), 4) + " " + rbRect(0.15 * w, Fy(0.18 * L), 0.55 * w, Fy(0.4 * L), 4), lt, R.ltk, 2) +
             P(pts([[-(w + 2), L - 20], [0, L - 4], [w + 2, L - 20], [w + 2, L - 7], [0, L + 9], [-(w + 2), L - 7]]) + " Z", red, darker(red), 2);
    }
    // 📺 no shell: the friend's own body
    case "panel": return "";
    // 🕷️ the bands across the back, bowed up over its round top
    case "segments":
      return rbStroke([0.12, 0.34, 0.56, 0.78].map(f => { const y = Fy(f * L); return `M${f1(-(w - 2))} ${f1(y)} Q0 ${f1(y - 18)} ${f1(w - 2)} ${f1(y)}`; }).join(" "), dk, 3);
  }
  return "";
}

// A piece on an arm (in the friend's units), drawn in that arm's slot. o: { dir (in profile), far (the far arm, behind),
// w (the torso's half width as drawn), armW, sleeve (the sleeve's points) }
function robotArm(R, arm, o) {
  const m = R.main, dk = R.dk, P = rbPath, [ax] = arm.A, sd = arm.side, prof = !!o.dir, b = o.dir || 1;
  switch (R.S.limbs) {
    // 🔑 a bolt at the elbow
    case "bolts": { const E = arm.E || [(arm.A[0] + arm.H[0]) / 2, (arm.A[1] + arm.H[1]) / 2]; return P(ellD(E[0], E[1], 6.5, 6.5), "#8a929c", "#4b5563", 2); }
    // 📦 dryer hose, 🚀 an accordion: its ribs one dashed stroke along the sleeve (and the space bot's shoulder pad)
    case "hose": case "accordion": {
      const ribs = rbStroke(rbLine(o.sleeve), R.S.limbs === "hose" ? "#7d848c" : "#9aa6b2", o.armW + 7, ' stroke-dasharray="2.5 6" stroke-linecap="butt"');
      if (R.S.limbs === "hose") return ribs;
      const cx = prof ? ax - (o.far ? b * 12 : 0) : sd * (o.w - 4), cy = o.far ? -8 : -2, r = 23;
      return ribs + P(`M${f1(cx - r)} ${f1(cy + 10)} A${r} ${r} 0 0 1 ${f1(cx + r)} ${f1(cy + 10)} Z`, o.far ? mix(m, "#000000", 0.2) : m, dk, 3);
    }
    // 🦾 a big block on the shoulder with a hazard stripe (after the arm, so a raised arm comes out from under it)
    case "blocks": {
      const fill = o.far ? mix(m, "#000000", 0.2) : m;
      if (prof) { const cx = ax - (o.far ? b * 16 : 0), cy = o.far ? -6 : 0;
        return P(rbRect(cx - 32, cy - 26, cx + 32, cy + 30, 12), fill, dk, 3) + `<path d="M${f1(cx - 22)} ${f1(cy + 18)} L${f1(cx + 22)} ${f1(cy + 18)}" stroke="#1f2937" stroke-width="9" stroke-dasharray="8 7"/>`; }
      const x0 = sd * (o.w - 24), x1 = sd * (o.w + 46);
      return P(rbRect(x0, -28, x1, 30, 12), fill, dk, 3) + `<path d="M${f1(x0 + sd * 10)} 18 L${f1(x1 - sd * 10)} 18" stroke="#1f2937" stroke-width="9" stroke-dasharray="8 7"/>`;
    }
    // ⚡ a pointed shoulder plate, flaring up and out to a point, its edge in the red trim
    case "plates": {
      const fill = o.far ? mix(m, "#000000", 0.2) : m;
      if (prof) { const cx = ax - (o.far ? b * 12 : 0), cy = o.far ? -5 : 0;
        return P(rbLine([[cx + b * 24, cy + 18], [cx + b * 22, cy - 8], [cx - b * 30, cy - 46], [cx - b * 26, cy + 20]]) + " Z", fill, R.red, 4); }
      return P(rbLine([[sd * (o.w - 18), -8], [sd * (o.w + 44), -42], [sd * (o.w + 30), 24], [sd * (o.w - 6), 20]]) + " Z", fill, R.red, 4);
    }
  }
  return "";
}

// A piece on a leg (its joints J, hip to foot; `half` the pants leg's half width; away: seen from behind): the tin's ridges,
// the mech's knee pad
function robotLeg(R, J, half, dir, away = false) {
  if (R.S.limbs === "bolts") {
    // (the ridges across the thigh and across the shin, each its own run of dashes, as wide as the leg less a margin: each run
    // stops short of the knee by as far as its ends would reach past the knee's middle line, so a bent knee's two runs never
    // cross, and the shin's short of the foot; a knee folded past 100 degrees (sitting cross-legged, meditating: the leg drawn
    // as a flat fold) has no shin ridges, and its thigh's stop short leaves few or none)
    const hr = half - 4, unit = (A, B) => { const l = Math.hypot(B[0] - A[0], B[1] - A[1]) || 1; return [(B[0] - A[0]) / l, (B[1] - A[1]) / l, l]; };
    const run = (A, B, a, b) => { const [ux, uy, l] = unit(A, B); return l - a - b < 6 ? "" : `M${f1(A[0] + ux * a)} ${f1(A[1] + uy * a)} L${f1(B[0] - ux * b)} ${f1(B[1] - uy * b)}`; };
    let d;
    if (J.length < 3) d = run(J[0], J[1], 0, 24);
    else { const [t1, t2] = [unit(J[0], J[1]), unit(J[1], J[2])], bend = Math.acos(Math.max(-1, Math.min(1, t1[0] * t2[0] + t1[1] * t2[1])));
      const gap = bend > 3 ? Infinity : hr * Math.tan(bend / 2) + 3;
      d = [run(J[0], J[1], 0, gap), bend <= 1.75 ? run(J[1], J[2], gap, 24) : ""].filter(Boolean).join(" "); }
    return d ? rbStroke(d, "#9aa1ab", f1(2 * hr), ' stroke-dasharray="2.5 10" stroke-linecap="butt"') : "";
  }
  // (the mech's knee pad: on the front of the knee, so none from behind)
  if (R.S.limbs === "blocks" && !away) { const K = J.length > 2 ? J[1] : [(J[0][0] + J[1][0]) / 2, (J[0][1] + J[1][1]) / 2], x = K[0] + (dir ? dir * 5 : 0);
    return rbPath(rbRect(x - 16, K[1] - 16, x + 16, K[1] + 16, 5), R.main, R.dk, 3); }
  return "";
}

// What stands off the back (in the friend's units), drawn where wings are. o: { stride (walking or running, which of the
// key's three drawings: 0, 1, 2), jump (in the air: the space bot's flames), sitting, lying (any facing: the spider's legs
// folded), lieBack (lying face up: what is on the back is under them, not drawn), crawl ({ to } crawling toward us or
// { away }: robotCrawlTorso's), crawlTo (crawling toward us: behind the back turned up, only what rises from behind it),
// over (crawling toward us: its own drawing over that back), turn and phase (crawling or swimming in profile: the turn
// about the hips, and LFO B's phase, the spider's legs stepping by it), floor (the floor, or the water's depth) }
function robotBack(R, w, L, dir, away, o = {}) {
  const m = R.main, dk = R.dk, lt = R.lt, P = rbPath, kind = R.S.back;
  if (o.lieBack && kind !== "legs") return "";
  if (o.crawlTo && !o.over && kind !== "legs") return "";
  if (o.over && kind === "legs") return "";
  // (crawling toward us, over the back turned up, or crawling away: each piece's own drawing there)
  if (o.over || (o.crawl && o.crawl.away)) return robotCrawlBack(R, w, L, o);
  switch (kind) {
    // 🔑 the wind-up key: hidden facing us; from the side its stem straight out of the back at the shoulder blades, its bow
    // seen full on (its iconic outline); from behind seen end on, the hub and the bow edge on, an upright bar. Walking or
    // running it turns: three drawings of its own in turn (full on, half turned, edge on; from behind the bar upright, then
    // slanting one way, then the other), never a rotate()
    case "key": {
      const k = o.stride || 0, steel = "#9aa1ab";
      if (!dir && !away) return "";
      if (away) { const a = [90, 30, 150][k] * Math.PI / 180, c = Math.cos(a), s = Math.sin(a), y = 34, l = 30, t = 5;
        return P(rbLine([[c * l - s * t, y - s * l - c * t], [c * l + s * t, y - s * l + c * t], [-c * l + s * t, y + s * l + c * t], [-c * l - s * t, y + s * l - c * t]]) + " Z", lt, R.ltk, 2.5) +
               P(ellD(0, y, 9, 9), lt, R.ltk, 2.5); }
      const x0 = -dir * (w + 3), x1 = x0 - dir * 26, y = 30, bx = x1 - dir * 12;
      const bow = k === 0 ? rbBow(bx, y, 13, 26) : k === 1 ? rbBow(bx, y, 13, 13) : rbRect(bx - 13, y - 3, bx + 13, y + 3, 2);
      return P(rbRect(x0, y - 4, x1, y + 4, 0), steel, "#4b5563", 2) + P(bow, lt, R.ltk, 2.5) + P(ellD(x0, y, 8, 8), lt, R.ltk, 2.5);
    }
    // 🚀 the rocket pack: two tubes behind, their nose cones over the shoulders, fins at the waist, the nozzles' rims (in the
    // lights); in profile one tube along the back, its fin out behind; jumping, a still flame under each nozzle
    case "rockets": {
      const r = 13, top = -66, bot = L - 14;
      const tube = x => `M${f1(x - r)} ${f1(bot)} L${f1(x - r)} ${f1(top + 1.6 * r)} Q${f1(x - r)} ${f1(top + 0.2 * r)} ${f1(x)} ${f1(top)} Q${f1(x + r)} ${f1(top + 0.2 * r)} ${f1(x + r)} ${f1(top + 1.6 * r)} L${f1(x + r)} ${f1(bot)} Z`;
      const fin = (x, s) => `M${f1(x + s * r)} ${f1(bot - 46)} L${f1(x + s * (r + 26))} ${f1(bot + 8)} L${f1(x + s * r)} ${f1(bot)} Z`;
      const rim = x => rbRect(x - r - 2, bot - 2, x + r + 2, bot + 8, 3);
      const flame = (x, k) => `M${f1(x - k * (r - 2))} ${f1(bot + 8)} Q${f1(x - k * (r + 4))} ${f1(bot + 8 + 30 * k)} ${f1(x)} ${f1(bot + 8 + 58 * k)} Q${f1(x + k * (r + 4))} ${f1(bot + 8 + 30 * k)} ${f1(x + k * (r - 2))} ${f1(bot + 8)} Z`;
      const xs = dir ? [-dir * (w + 14)] : away ? [-(w - 18), w - 18] : [-(w + 2), w + 2], out = x => dir ? -dir : Math.sign(x);
      let t = (o.jump ? P(xs.map(x => flame(x, 1)).join(" "), "#f97316") + P(xs.map(x => flame(x, 0.55)).join(" "), "#fde047") : "") +
              P(xs.map(x => fin(x, out(x))).join(" "), mix(m, "#000000", 0.12), dk, 2.5) + P(xs.map(tube).join(" "), m, dk, 3) + P(xs.map(rim).join(" "), lt, R.ltk, 2);
      return t;
    }
    // 🦾 the battery box on the back, a lightning bolt on it: hidden facing us
    case "battery": {
      if (!dir && !away) return "";
      const bolt = (cx, cy) => rbLine([[cx + 5, cy - 18], [cx - 7, cy + 2], [cx + 1, cy + 2], [cx - 4, cy + 18], [cx + 9, cy - 4], [cx + 1, cy - 4], [cx + 6, cy - 18]]) + " Z";
      if (away) return P(rbRect(-28, 10, 28, 86, 6), m, dk, 3) + P(bolt(0, 48), lt, R.ltk, 1.5);
      const a = -dir * (w - 2), b = -dir * (w + 30);
      return P(rbRect(a, 6, b, 86, 6), m, dk, 3) + P(bolt((a + b) / 2, 46), lt, R.ltk, 1.5);
    }
    // 📺 the curly power cord down the back to its plug at the waist: hidden facing us
    case "cord": {
      if (!dir && !away) return "";
      const x = away ? 0 : -dir * (w + 6), y0 = away ? -12 : 4, n = 7, step = (L - 30 - y0) / n, s = away ? 1 : -dir;
      let d = `M${f1(away ? 0 : -dir * (w - 2))} ${f1(y0)} L${f1(x)} ${f1(y0)}`;
      for (let i = 0; i < n; i++) d += ` a7 7 0 1 ${s > 0 ? 1 : 0} 0 ${f1(step)}`;
      const py = y0 + n * step;
      return rbStroke(d, "#374151", 4) + P(rbRect(x - 9, py, x + 9, py + 16, 3) + ` M${f1(x - 5)} ${f1(py + 16)} L${f1(x - 5)} ${f1(py + 25)} L${f1(x - 2)} ${f1(py + 25)} L${f1(x - 2)} ${f1(py + 16)} Z M${f1(x + 2)} ${f1(py + 16)} L${f1(x + 2)} ${f1(py + 25)} L${f1(x + 5)} ${f1(py + 25)} L${f1(x + 5)} ${f1(py + 16)} Z`, "#374151");
    }
    // 🕷️ four jointed legs arching out of the back, two each side, their knees above the shoulders, their tips at about knee
    // height (above the seat, sitting); in profile all four fanning back, the far pair darker; from behind over the back with
    // the abdomen pack; lying down, folded tight along the side (knee by the shoulder, tip by the hip); crawling toward us,
    // rising from behind the back turned up (not turned over with it)
    case "legs": {
      const ln = dk, far = mix(dk, "#000000", 0.35), shine = mix(m, "#ffffff", 0.35), joint = mix(m, "#000000", 0.6);
      const legs = (sets) => sets.map(([ps, col]) => rbStroke(ps.map(rbLine).join(" "), col, 7)).join("") +
                             sets.map(([ps]) => rbStroke(ps.map(p => rbLine(p.slice(0, 2).map(([x, y]) => [x, y - 2.5]))).join(" "), shine, 2.5)).join("") +
                             P(rbDots(sets.flatMap(([ps]) => ps.map(p => p[1])), 5), joint);
      const sac = (cx, rx) => P(ellD(cx, 0.56 * L, rx, 0.36 * L), m, dk, 3) + rbStroke(`M${f1(cx - 0.9 * rx)} ${f1(0.6 * L)} Q${f1(cx)} ${f1(0.6 * L + 14)} ${f1(cx + 0.9 * rx)} ${f1(0.6 * L)}`, "#facc15", 8, ' stroke-linecap="butt"');
      if (o.lying) {
        if (dir) { const x = -dir * w * 0.4, f = s => -dir * (w + s);
          return legs([[[[[x, 8], [f(8), -14], [f(10), L - 12]], [[x, 26], [f(14), 6], [f(16), L + 4]]], ln]]); }
        return legs([-1, 1].map(s => [[[[s * 20, 6], [s * (w + 10), -16], [s * (w + 12), L - 14]], [[s * 20, 26], [s * (w + 18), 6], [s * (w + 20), L + 4]]], ln]));
      }
      // (crawling or swimming in profile, turned about the hips: a drawing of its own laid out level in the picture, two legs
      // reaching forward and two back over the body, their knees high over the back and their tips down by the floor (or in
      // the water), the far pair darker; each point carried back into the friend's turned units, as the dragon's tail is)
      // (they step as the friend goes, by LFO B's phase: a front leg on one side with the back leg on the other, each lifted
      // while it swings forward, the other two planted)
      if (o.turn) { const a = o.turn * Math.PI / 180, ca = Math.cos(a), sa = Math.sin(a), s = dir || 1, ph = 2 * Math.PI * (o.phase || 0);
        const lev = ([x, y]) => [x * ca - (y - L) * sa, L + x * sa + (y - L) * ca], back = ([x, y]) => [x * ca + (y - L) * sa, L - x * sa + (y - L) * ca];
        const [rx, ry] = lev([-s * w * 0.5, 0.3 * L]), fy = o.floor - 10;
        const leg = (b, h, c, d = 0, i = 0) => { const sw = Math.sin(ph + i * Math.PI), up = 14 * Math.max(0, Math.cos(ph + i * Math.PI));
          return [[rx + s * d, ry], [rx + s * (d + b + 6 * sw), ry - h - 0.5 * up], [rx + s * (d + c + 18 * sw), fy - up]].map(back); };
        return legs([[[leg(62, 100, 156, 8, 1), leg(-58, 96, -150, -8, 0)], far], [[leg(42, 88, 112, 0, 0), leg(-40, 86, -112, 0, 1)], ln]]); }
      const tipY = o.sitting ? [L - 40, L - 22] : [L + 56, L + 76];
      if (dir) { const x = -dir * w * 0.4, f = s => -dir * (w + s);
        return legs([[[[[x, 6], [f(8), -72], [f(70), tipY[0] - 6]], [[x, 26], [f(44), -46], [f(118), tipY[1] - 10]]], far],
                     [[[[x, 10], [f(24), -62], [f(92), tipY[0] + 6]], [[x, 30], [f(62), -32], [f(142), tipY[1]]]], ln]]) + sac(-dir * (w + 10), 26); }
      // (crawling toward us, rising from behind the back turned up, their tips on the floor, the front pair's a little further
      // back, so higher in the picture)
      const kb = o.crawlTo ? o.crawl.to : 0, dy = o.crawlTo ? -kb * 0.2 * L - 20 * (1 + kb) - 10 : 0, tip = i => o.crawlTo ? o.floor - (i ? 10 : 24) : tipY[i] + dy;
      const set = s => [[[[s * 20, 6 + dy], [s * (w + 34), -64 + dy], [s * (w + 100), tip(0)]], [[s * 20, 26 + dy], [s * (w + 62), -34 + dy], [s * (w + 128), tip(1)]]], ln];
      return legs([set(-1), set(1)]) + (away ? sac(0, 0.62 * w) : "");
    }
  }
  return "";
}
// Crawling, the back's pieces as they are seen there, each its own drawing (never the from-behind drawing turned over or
// seen short): over the back turned up behind the head (o.over: crawling toward us) or over the back seen from behind and a
// little above (o.crawl.away). Fy as robotCrawlTorso's; what stands up off the back shows 0.9 of its height up the picture.
function robotCrawlBack(R, w, L, o) {
  const m = R.main, dk = R.dk, lt = R.lt, P = rbPath, to = !!o.crawl.to, k = o.crawl.to || o.crawl.away;
  const Fy = y => to ? -k * y - 20 * (1 + k) : L - k * (L - y);
  const pts = ps => rbLine(ps.map(([x, y, h = 0]) => [x, Fy(y) - 0.9 * h]));
  switch (R.S.back) {
    // 🔑 the key standing straight up off the back, the bow full on across its top (crawling toward us, the stem long enough
    // that the bow shows over the tall tin head)
    case "key": { const y = Fy(30), st = to ? 42 : 34;
      return P(rbRect(-4, y - st, 4, y, 0), "#9aa1ab", "#4b5563", 2) + P(rbBow(0, y - st - 8, 11, 20, true), lt, R.ltk, 2.5) + P(ellD(0, y, 9, 9), lt, R.ltk, 2.5); }
    // 🚀 the two tubes lying along the back, seen along their length: toward us their nose cones end on (the tips at us), the
    // fins far up by the nozzles; away the nozzles end on, rimmed in the lights, the fins out beside them, the cones far up
    case "rockets": {
      const r = 13, top = -66, bot = L - 14, xs = [-(w - 18), w - 18], h = r + 2;
      const tube = x => pts([[x - r, top + r, h], [x - r, bot, h], [x + r, bot, h], [x + r, top + r, h]]) + " Z";
      const fin = x => { const s = Math.sign(x); return pts([[x + s * r, bot - 46, h], [x + s * (r + 26), bot + 8, h], [x + s * r, bot, h]]) + " Z"; };
      const endOn = (x, y, rr) => ellD(x, Fy(y) - 0.9 * h, rr, 0.9 * rr);
      let t = P(xs.map(fin).join(" "), mix(m, "#000000", 0.12), dk, 2.5) + P(xs.map(tube).join(" "), m, dk, 3);
      if (to) t += P(xs.map(x => rbRect(x - r - 2, Fy(bot) - 0.9 * h - 4, x + r + 2, Fy(bot) - 0.9 * h + 5, 3)).join(" "), lt, R.ltk, 2) +   // (the nozzles' rims, far up, seen from the side)
                   P(xs.map(x => endOn(x, top + r, r)).join(" "), m, dk, 3) + P(rbDots(xs.map(x => [x - 3, Fy(top + r) - 0.9 * h - 3]), 3), mix(m, "#ffffff", 0.6));
      else t += P(xs.map(x => endOn(x, top + r, r)).join(" "), m, dk, 3) + P(xs.map(x => endOn(x, bot, r + 2)).join(" "), lt, R.ltk, 2) + P(xs.map(x => endOn(x, bot, r - 4)).join(" "), "#27272a");
      return t;
    }
    // 🦾 the battery box on the back: its top with the bolt, and its near end
    case "battery": {
      const h = 30, ya = 10, yb = 86, ye = to ? ya : yb, mx = ([x, y]) => [x, y, h];
      const bolt = [[5, -18], [-7, 2], [1, 2], [-4, 18], [9, -4], [1, -4], [6, -18]].map(([x, y]) => mx([x, 48 + y]));
      return P(rbRect(-28, Fy(ye) - 0.9 * h, 28, Fy(ye), 4), mix(m, "#000000", 0.18), dk, 3) + P(rbRect(-28, Fy(ya) - 0.9 * h, 28, Fy(yb) - 0.9 * h, 5), m, dk, 3) +
             P(pts(bolt) + " Z", lt, R.ltk, 1.5);
    }
    // 📺 the curly cord lying down the back in its coils, its plug at the waist (away, near us, its pins toward us; toward
    // us, far up the back)
    case "cord": {
      const y0 = -12, n = 7, py = L - 30, step = (Fy(py) - Fy(y0)) / n;
      let d = `M0 ${f1(Fy(y0))}`;
      for (let i = 0; i < n; i++) d += ` a7 7 0 1 1 0 ${f1(step)}`;
      const e = Fy(py), dirn = Math.sign(step), pe = e + dirn * 12;
      return rbStroke(d, "#374151", 4) + P(rbRect(-9, e, 9, pe, 3) + " " + rbRect(-5, pe, -2, pe + dirn * 7, 0) + " " + rbRect(2, pe, 5, pe + dirn * 7, 0), "#374151");
    }
    // 🕷️ crawling away: the four legs out of the back, their knees up over it and their tips down on the floor either side
    // (the front pair, further away, higher in the picture), and the abdomen pack round on the back
    case "legs": {
      const ln = dk, shine = mix(m, "#ffffff", 0.35), joint = mix(m, "#000000", 0.6), fl = o.floor;
      const ps = [-1, 1].flatMap(s => [[[s * 20, Fy(6) - 10], [s * (w + 34), Fy(6) - 84], [s * (w + 100), fl - 24]], [[s * 20, Fy(26) - 10], [s * (w + 62), Fy(26) - 62], [s * (w + 128), fl - 10]]]);
      const rx = 0.62 * w, sy = Fy(0.56 * L) - 14, sr = Math.hypot(k * 0.36 * L, 14);
      return rbStroke(ps.map(rbLine).join(" "), ln, 7) + rbStroke(ps.map(p => rbLine(p.slice(0, 2).map(([x, y]) => [x, y - 2.5]))).join(" "), shine, 2.5) + P(rbDots(ps.map(p => p[1]), 5), joint) +
             P(ellD(0, sy, rx, sr), m, dk, 3) + rbStroke(`M${f1(-0.9 * rx)} ${f1(sy + 4)} Q0 ${f1(sy - 10)} ${f1(0.9 * rx)} ${f1(sy + 4)}`, "#facc15", 8, ' stroke-linecap="butt"');
    }
  }
  return "";   // (back "none", the box's, the helper's and the hero's: nothing stands off it; its details are the torso's)
}
// How far the robot reaches out sideways past its torso (w), in the friend's units, in this view: for the picture's frame,
// and, lying down, for the ground under them (the box's flaps, the mech's blocks, the hero's plates, the fins, the legs;
// lying face up, only how deep its torso is, what is on its back being under them)
function robotOut(R, w, dir, lying) {
  const T = R.S.torso, legs = R.S.back === "legs";
  if (lying) return legs ? 24 : dir ? ({ box: 0.47 * w + 4, egg: 0.6 * w }[T] || 6) : ({ column: 13, box: 0.3 * w + 24, egg: 0.35 * w, capsule: 42, chest: 48, plate: 46 }[T] || 0);
  if (legs) return dir ? 150 : 132;
  return (dir ? { column: 60, box: 0.47 * w + 8, egg: 0.6 * w, capsule: 58, chest: 34, plate: 32, panel: 32 } : { column: 13, box: 0.3 * w + 24, egg: 0.35 * w, capsule: 42, chest: 48, plate: 46 })[T] || 0;
}

// ===================== THE COWGIRL AND THE COWBOY (ticket 522) =====================
// (M and Matthew: "Cowgirl and Cowboy, 3 variations of style each. 6 styles of hat. Lasso to hold. No guns please.")
// Built as the gnome and the witch are, in the friend's own clothes: drawNow replaces the shirt and pants (westernOf's
// `shirt` and `pants`, so every pose draws them) and adds these pieces in its slots, every view each piece's own drawing
// (dir: 0 facing us or away, -1/1 in profile; away: from behind; left and right mirror by dir). The hat (cowHatSvg) and the
// lasso (lassoSvg) are in js/08-body.js, beside the witch's hat and the staff. Nothing gun-like: belts carry only buckles,
// the badge is a toy star, the spurs are star discs.
// westernOf(WC, kind): the style and the hat read into one object, every "main" its chosen colour
function westernOf(WC, kind) {
  const table = kind === "cowgirl" ? COWGIRL_STYLES : COWBOY_STYLES, id = table[WC[kind + "Style"]] ? WC[kind + "Style"] : Object.keys(table)[0];
  const S = table[id], main = WC[kind + "Color_" + id] ?? S.colors[0], m = v => v === "main" ? main : v;
  const hatId = WC.cowHat === "none" ? null : COW_HATS[WC.cowHat] ? WC.cowHat : "cattleman", H = hatId ? COW_HATS[hatId] : null;
  const n = parseInt(main.slice(1), 16), reddish = (n >> 16) > 150 && ((n >> 8) & 255) < 110 && (n & 255) < 110;
  const res = o => Object.fromEntries(Object.entries(o).map(([k, v]) => [k, m(v)]));
  return { id, S, main, kind, shirt: res(S.shirt), pants: res(S.pants), vest: m(S.vest), bootStar: m(S.bootStar),
           bandana: S.bandana === "auto" ? (reddish ? "#1e3a8a" : "#c0392b") : S.bandana,
           hatId, H, hatColor: hatId ? WC["cowHatColor_" + hatId] ?? H.colors[0] : null };
}
// the boots: SHOES.cowboot in the style's colour, its star and spurs
const westernFeet = (W, shoes) => ({ style: "cowboot", size: shoes.size, color: W.S.boot, star: W.bootStar || null, spur: !!W.S.spur });
// fringe: strands along a path, ONE dashed stroke (butt caps: each dash a strand as long as the stroke is wide); a light
// fringe gets a darker dashed stroke under it, a little wider, so it shows on light scenes
const fringeSvg = (d, col, len, under) => (under ? `<path d="${d}" fill="none" stroke="${under}" stroke-width="${len + 2}" stroke-dasharray="4.3 1.4" stroke-dashoffset="0.9"/>` : "") +
                                          `<path d="${d}" fill="none" stroke="${col}" stroke-width="${len}" stroke-dasharray="2.5 3.2"/>`;
// a path through points pushed out by `by` along each segment's normal toward `side` (a joint by the two's average),
// stopping `cut` short of the last point
const offsetPath = (pts, side, by, cut = 0) => {
  const segN = (a, b) => { const dx = b[0] - a[0], dy = b[1] - a[1], l = Math.hypot(dx, dy) || 1; return [side * dy / l, -side * dx / l]; };
  const q = pts.slice(); if (cut && q.length > 1) { const a = q[q.length - 2], b = q[q.length - 1], l = Math.hypot(b[0] - a[0], b[1] - a[1]) || 1, k = Math.max(0, (l - cut) / l); q[q.length - 1] = [a[0] + (b[0] - a[0]) * k, a[1] + (b[1] - a[1]) * k]; }
  const ns = q.slice(1).map((b, i) => segN(q[i], b));
  return "M" + q.map((p, i) => { const n = i === 0 ? ns[0] : i === q.length - 1 ? ns[ns.length - 1] : [(ns[i - 1][0] + ns[i][0]) / 2, (ns[i - 1][1] + ns[i][1]) / 2];
    return f1(p[0] + n[0] * by) + " " + f1(p[1] + n[1] * by); }).join(" L");
};
// (ticket 530) A SASQUATCH'S SHAG along an edge: the fringe's trick, fur tufts as dashes of one thick stroke (butt caps), a
// darker stroke under them half a dash along, so each edge is two shapes however long; pts the edge's points, pushed out
// by `by` toward `side` (offsetPath), `len` how long the tufts are (times the style's shag)
// (several edges at once: [[pts, side], ...], one path of each, still two shapes)
const shagEdge = (pts, side, by, len, C) => { const E = side === "many" ? pts.filter(([p]) => p && p.length > 1) : pts && pts.length > 1 ? [[pts, side]] : [];
  if (!E.length) return ""; const d = E.map(([p, sd]) => offsetPath(p, sd, by)).join(" "), k = (C.sq && C.sq.shag) || 1, L = f1(len * k);
  return `<path d="${d}" fill="none" stroke="${mix(C.fur, "#000000", 0.35)}" stroke-width="${f1(len * k + 3)}" stroke-dasharray="6 5" stroke-dashoffset="6"/>` +
         `<path d="${d}" fill="none" stroke="${C.fur}" stroke-width="${L}" stroke-dasharray="6 5"/>`; };
// The hood's top, in the round head's units (the hood's middle at x 100), its own drawing toward us, from behind and in
// profile (dir; left and right mirror): the shaggy crown along the hood's top, and the style's extras (the yeti's frosty
// tips, the mossy's moss and leaves, the fluffy's bow), at the head's left toward us, its right from behind, at the back of
// the head in profile
function sasquatchTop(C, F, dir, away) {
  const { cy, rx, ry } = hoodGeom(F), S = C.sq || {}, k = S.shag || 1, P = (x, y) => f1(x) + " " + f1(y);
  const arc = (a0, a1, grow) => { const p = a => [100 + (rx + grow) * Math.cos(a * Math.PI / 180), cy + (ry + grow) * Math.sin(a * Math.PI / 180)];
    return `M${P(...p(a0))} A${f1(rx + grow)} ${f1(ry + grow)} 0 ${a1 - a0 > 180 ? 1 : 0} 1 ${P(...p(a1))}`; };
  const [a0, a1] = dir ? (dir > 0 ? [160, 330] : [210, 380]) : away ? [150, 390] : [195, 345];
  let s = `<path data-sasquatch="crown" d="${arc(a0, a1, 2)}" fill="none" stroke="${darker(C.fur)}" stroke-width="${f1(16 * k + 3)}" stroke-dasharray="6 4" stroke-dashoffset="5"/>` +
          `<path d="${arc(a0, a1, 2)}" fill="none" stroke="${C.fur}" stroke-width="${f1(16 * k)}" stroke-dasharray="5.5 4.5"/>`;
  if (S.frost) s += `<path d="${arc(a0, a1, 2 + 6 * k)}" fill="none" stroke="${S.frost}" stroke-width="5" stroke-dasharray="5.5 4.5"/>`;
  const X = dx => 100 + (away ? -1 : dir ? -dir : 1) * dx, top = cy - ry;
  if (S.moss) s += `<path d="${[[-38, 22, 13], [30, 8, 10], [56, 46, 9]].map(([dx, dy, r]) => `M${P(X(dx) - r, top + dy)} a${r} ${f1(r * 0.7)} 0 1 0 ${2 * r} 0 a${r} ${f1(r * 0.7)} 0 1 0 ${-2 * r} 0 Z`).join(" ")}" fill="${S.moss}" opacity="0.9"/>`;
  if (S.leaves) s += `<path d="${[[16, 2, -1], [46, 18, 1]].map(([dx, dy, t]) => { const x = X(dx), y = top + dy;
    return `M${P(x, y)} Q${P(x + 10, y - 16 * t - 6)} ${P(x + 24, y - 8)} Q${P(x + 12, y + 6)} ${P(x, y)} Z M${P(x, y)} L${P(x + 22, y - 8)}`; }).join(" ")}" fill="${S.leaves}" stroke="${darker(S.leaves)}" stroke-width="1.5" stroke-linejoin="round"/>`;
  if (S.bow) { const x = X(52), y = top + 18;
    s += `<path d="M${P(x, y)} Q${P(x - 28, y - 24)} ${P(x - 30, y + 2)} Q${P(x - 22, y + 18)} ${P(x, y)} Z M${P(x, y)} Q${P(x + 28, y - 24)} ${P(x + 30, y + 2)} Q${P(x + 22, y + 18)} ${P(x, y)} Z M${P(x - 6, y)} a6 6 0 1 0 12 0 a6 6 0 1 0 -12 0 Z" fill="${S.bow}" stroke="${darker(S.bow)}" stroke-width="2" stroke-linejoin="round"/>`; }
  return s;
}
// Its big footprints on the ground (ticket 530), standing still, in the friend's units: a trail of four, walked in from
// behind them (in profile) or from our left (toward us or away), the nearest by their feet; each print one path, a sole and
// four round toes, the colour of wet mud
function sasquatchPrints(C, hw, ground, dir, k) {
  const P = (x, y) => f1(x) + " " + f1(y), sz = 14 * k;
  const print = (x, y, f) => `M${P(x - f * sz * 0.9, y)} q${f1(f * sz * 0.2)} ${f1(-sz * 0.45)} ${f1(f * sz * 1.1)} ${f1(-sz * 0.38)} q${f1(f * sz * 0.8)} ${f1(sz * 0.08)} ${f1(f * sz * 0.8)} ${f1(sz * 0.38)} q0 ${f1(sz * 0.3)} ${f1(-f * sz * 0.8)} ${f1(sz * 0.38)} q${f1(-f * sz * 1)} ${f1(sz * 0.08)} ${f1(-f * sz * 1.1)} ${f1(-sz * 0.38)} Z ` +
    [-0.3, -0.1, 0.1, 0.3].map(t => { const r = sz * 0.19, cx = x + f * sz * (1.12 - Math.abs(t) * 0.5), cyy = y + t * sz * 1.25; return `M${P(cx - r, cyy)} a${f1(r)} ${f1(r)} 0 1 0 ${f1(2 * r)} 0 a${f1(r)} ${f1(r)} 0 1 0 ${f1(-2 * r)} 0 Z`; }).join(" ");
  const f = dir || 1, pts = dir ? [0, 1, 2, 3].map(i => [-dir * (hw + 70 + i * 62 * k), ground + (i % 2 ? 4 : 14)])
                                : [0, 1, 2, 3].map(i => [-(hw + 70 + i * 58 * k), ground + 8 - i * 7]);
  return `<path data-sasquatch="prints" d="${pts.map(([x, y]) => print(x, y, f)).join(" ")}" fill="${mix("#6b4a2b", "#000000", 0.15)}" opacity="0.45"/>`;
}
// (ticket 544) THE GIRAFFE. Its coats, furDefs' tile (w by h): "net", the reticulated giraffe's patches, a Voronoi diagram of
// twelve seeds that tiles, each cell shrunk for the cream lines between them, and each crossing the tile's edge repeated a
// tile over; "masai", the same cells further apart, a lobe out at each corner and notches cut into each side.
const GIRAFFE_COATS = {
  net: { w: 108, h: 124, join: "round", d: "M-3 19l-3-18 13-13 15 6-2 20-15 11zM-3 143l-3-18 13-13 15 6-2 20-15 11zM105 19l-3-18 13-13 15 6-2 20-15 11zM105 143l-3-18 13-13 15 6-2 20-15 11zM26 14l2-20 11-4 20 13 0 11-14 9zM26 138l2-20 11-4 20 13 0 11-14 9zM65 14l0-12 10-7 21 7 3 16-25 5zM65 138l0-12 10-7 21 7 3 16-25 5zM8 39l-1-8 16-12 19 9-4 18-10 6zM48 28l13-9 10 9 1 20-10 5-17-7zM-31 29l24-5 8 7 1 9-16 15-16-8zM77 29l24-5 8 7 1 9-16 15-16-8zM-8 79l-2-19 16-15 19 12 0 15-25 12zM100 79l-2-19 16-15 19 12 0 15-25 12zM31 57l10-6 18 7-2 21-14 5-12-11zM63 80l2-22 10-5 17 7 2 19-16 8zM25 113l-16-6-5-18 23-11 12 10-3 20zM45 89l15-4 15 7-3 22-10 7-20-12zM3 108l-13 12-20-7 3-21 16-8 9 5zM111 108l-13 12-20-7 3-21 16-8 9 5z" },
  masai: { w: 108, h: 124, join: "miter", d: "M-2 19l2-8-5-3 4-4-5-4 7 0 0-6 5 1 2-7 4 6 10 0-4 9 2 2-4 5 3 4-7 1-1 5-4 0-3 5-2-6zM-2 143l2-8-5-3 4-4-5-4 7 0 0-6 5 1 2-7 4 6 10 0-4 9 2 2-4 5 3 4-7 1-1 5-4 0-3 5-2-6zM106 19l2-8-5-3 4-4-5-4 7 0 0-6 5 1 2-7 4 6 10 0-4 9 2 2-4 5 3 4-7 1-1 5-4 0-3 5-2-6zM106 143l2-8-5-3 4-4-5-4 7 0 0-6 5 1 2-7 4 6 10 0-4 9 2 2-4 5 3 4-7 1-1 5-4 0-3 5-2-6zM27 14l5-7-4-3 4-4-4-6 7 1 3-5 7 9 5-2 1 6 7 0-2 4 2 7-8 1-5 7-4-6-6 1-1-4zM27 138l5-7-4-3 4-4-4-6 7 1 3-5 7 9 5-2 1 6 7 0-2 4 2 7-8 1-5 7-4-6-6 1-1-4zM95 3l-2 7 6 8-12-2 0 5-8-3-5 5-2-6-7-3 3-4-3-7 7-1 3-7 5 6 8-1 2 3zM95 127l-2 7 6 8-12-2 0 5-8-3-5 5-2-6-7-3 3-4-3-7 7-1 3-7 5 6 8-1 2 3zM9 39l1-4-3-4 7-1 3-4 4 0 2-6 5 6 4-2 3 5 5 0-3 4 3 4-4 3 2 6-7 0-2 6-5-8-8 1 2-6zM49 29l7-1 5-9 3 7 6 2-3 6 4 6-4 2 4 5-5 1-4 5-4-6-5 1-1-3-7 1 4-6-3-3 5-4zM-30 30l9 1 5-4 5 4 5-7 2 5 6 2-3 4 2 4-8 2 1 8-5-1-3 5-4-5-5 1-2-3-4 0 3-5-4-4 3-4zM78 30l9 1 5-4 5 4 5-7 2 5 6 2-3 4 2 4-8 2 1 8-5-1-3 5-4-5-5 1-2-3-4 0 3-5-4-4 3-4zM-7 78l2-7-4-3 3-3-4-5 8-2 2-7 4 1 2-6 4 8 7-3 0 7 8-1-3 7 3 8-11 0-2 6-6-2-6 8-2-5zM101 78l2-7-4-3 3-3-4-5 8-2 2-7 4 1 2-6 4 8 7-3 0 7 8-1-3 7 3 8-11 0-2 6-6-2-6 8-2-5zM32 57l6-1 4-4 3 6 7-2 1 3 6-1-5 8 4 4-4 3 3 6-9-1-5 5-5-9-7-1 3-6zM64 79l5-7-5-5 6-3-4-6 6 0 3-4 5 6 5-1 1 3 6-1-3 5 4 6-4 2 3 4-8 2-6 6-5-7zM26 113l-4-5-6 0-1-3-5 1 2-6-5-4 2-3-4-4 11 0 0-5 7 1 4-7 4 9 7 2-4 7 3 3-5 2 4 7-7 0zM46 90l8 0 6-5 6 7 8 1-5 6 3 5-4 2 4 8-6 0-4 6-4-7-7 2-1-6-7-1 4-4-3-6 5-3zM3 108l-7 1-1 4-3 1-2 5-6-5-4 3-1-6-8 2 5-7-3-4 4-4-3-5 6 0 3-6 3 4 3-6 3 5 5 0-2 6 5 5-2 2zM111 108l-7 1-1 4-3 1-2 5-6-5-4 3-1-6-8 2 5-7-3-4 4-4-3-5 6 0 3-6 3 4 3-6 3 5 5 0-2 6 5 5-2 2z" },
};
// (the giraffe's head's size against its drawing's own units)
const GIRAFFE_HEAD = 1.15;
// Where the giraffe's neck goes, in the face's units (x 100 the head's middle; its stretch, a wide by b tall, is the hood's):
// its foot (base) inside the hood's top, which hides it; the way it leaves the hood (d0: up the body; turned with the friend
// by rot, crawling or swimming in profile or lying down, in the picture's upright frame its drawing is turned back into,
// about base); its top (end), H from the foot, straight up (in profile leaning forward), or, turned, out along the body and
// up (rise: lying down or swimming, more up than along); its half width at the foot (hb) and the top (ht), deeper in
// profile; and how far its head reaches up (up) and out (out) from the top, for the picture's frame (giraffeReach).
// Crawling toward us or away (squash) it leans that way, seen short. Its length is the style's (neck) times giraffeNeck, a
// percent.
function giraffeGeom(C, F, o = {}) {
  const S = C.gf || GIRAFFE_STYLES.reticulated, a = headWidth(F) * (1 + 0.12 * F.alien / 100), b = headH(F);
  const { cy, ry } = hoodGeom(F), dir = o.away ? 0 : o.dir || 0, r = (o.rot || 0) * Math.PI / 180;
  const base = [100 - dir * 8 * a, HEAD_C + (cy - ry + 34 - HEAD_C) * b];
  const H = 185 * S.neck * clamp((Number(C.giraffeNeck ?? 100) || 100) / 100, 0.5, 2.5) * b * (o.squash ? 0.4 : 1);
  const k = o.rise ? 0.35 : 0.6, d0 = [Math.sin(r), -Math.cos(r)], u = [k * d0[0], k * d0[1] - (1 - k)], ul = Math.hypot(u[0], u[1]) || 1;
  const end = [base[0] + H * u[0] / ul + dir * 22 * a * Math.max(0, Math.cos(r)), base[1] + H * u[1] / ul];
  return { S, a, b, dir, base, d0, H, end, hb: (dir ? 58 : 48) * a, ht: (dir ? 33 : 22) * a, hk: GIRAFFE_HEAD, up: 124 * GIRAFFE_HEAD * b, out: (dir ? 100 : 86) * GIRAFFE_HEAD * a };
}
// How far the neck and head reach in the face's units, standing (not turned): the top (y) and the farthest either side of
// the head's middle (x), for drawNow's frame
const giraffeReach = (C, F, o) => { const G = giraffeGeom(C, F, o); return { top: G.end[1] - G.up, side: Math.abs(G.end[0] - 100) + G.out }; };
// THE GIRAFFE'S NECK AND HEAD, in the face's units (drawNow puts it in the face's frame, with the lid when the head bursts,
// before the face, so the hood hides the neck's foot): the neck along a cubic from its foot (leaving along d0) to its top
// (upright there), tapering, in the coat; its short mane, in profile a row of short stiff tufts along the back of the neck
// (the side away from the face), from behind a band down its middle (toward us it is hidden behind the neck); and on top
// the giraffe's own head, upright, its own drawing toward us, in profile (dir; left and right mirror) and from behind
// (away). Turned with the friend (rot), all of it is drawn in the picture's upright frame (turned back about the foot), so
// the neck leaves the hood along the body and bends up, and the head stays upright. Each piece a data-giraffe group or path.
function giraffeNeckSvg(C, F, o = {}) {
  const G = giraffeGeom(C, F, o), { S, a, b, base, d0, H, end, hb, ht, hk } = G, away = !!o.away, rot = o.rot || 0, P = (x, y) => f1(x) + " " + f1(y);
  // (in profile, the way the head faces: the friend's; turned past upright, the way the neck goes out along the body, so
  // lying down it looks on ahead, not back over them)
  const dir = G.dir && Math.cos(rot * Math.PI / 180) < 0.25 ? Math.sign(d0[0]) || G.dir : G.dir;
  const c1 = [base[0] + d0[0] * 0.45 * H, base[1] + d0[1] * 0.45 * H], c2 = [end[0], end[1] + 0.35 * H];
  const at = t => { const s = 1 - t; return [0, 1].map(i => s * s * s * base[i] + 3 * s * s * t * c1[i] + 3 * s * t * t * c2[i] + t * t * t * end[i]); };
  const tan = t => { const s = 1 - t, v = [0, 1].map(i => 3 * s * s * (c1[i] - base[i]) + 6 * s * t * (c2[i] - c1[i]) + 3 * t * t * (end[i] - c2[i])), l = Math.hypot(v[0], v[1]) || 1;
    return [v[0] / l, v[1] / l]; };
  // (a point t of the way up, out to one side, k 1 or -1, by its half width and `by` more)
  const edge = (t, k, by = 0) => { const p = at(t), v = tan(t), w = ht + (hb - ht) * Math.pow(1 - t, 1.5) + by; return [p[0] - k * v[1] * w, p[1] + k * v[0] * w]; };
  // (through points, smoothly: quadratic curves between their midpoints)
  const smooth = pts => pts.slice(1, -1).map((p, i) => { const q = pts[i + 2]; return ` Q${P(p[0], p[1])} ${P((p[0] + q[0]) / 2, (p[1] + q[1]) / 2)}`; }).join("") + ` L${P(...pts[pts.length - 1])}`;
  const ts = Array.from({ length: 11 }, (_, i) => i / 10), Lp = ts.map(t => edge(t, 1)), Rp = ts.map(t => edge(t, -1)).reverse();
  let s = `<path data-giraffe="neck" d="M${P(...Lp[0])}${smooth(Lp)} L${P(...Rp[0])}${smooth(Rp)} Z" fill="url(#pat-fur)" stroke="${darker(C.fur)}" stroke-width="4" stroke-linejoin="round"/>`;
  const mane = pts => `<path data-giraffe="mane" d="M${pts.map(p => P(...p)).join(" L")} Z" fill="${S.mane}" stroke="${darker(S.mane)}" stroke-width="1.5" stroke-linejoin="round"/>`;
  const t0 = 0.1, t1 = 0.96, K = 12, tk = i => t0 + (t1 - t0) * i / K;
  if (away) {   // (down its middle, its edges tufted)
    const side = k => Array.from({ length: K + 1 }, (_, i) => { const p = at(tk(i)), v = tan(tk(i)), w = (i % 2 ? 11 : 6) * a; return [p[0] - k * v[1] * w, p[1] + k * v[0] * w]; });
    s += mane([...side(1), ...side(-1).reverse()]);
  } else if (dir) {   // (along the back: the side whose outward way, at the top, is away from the face)
    const k = Math.sign(dir * tan(1)[1]) || -dir, M = 13 * a;
    s += mane([edge(t0, k, -3), ...Array.from({ length: K + 1 }, (_, i) => edge(tk(i), k, i % 2 ? M : 0.3 * M)), ...ts.filter(t => t > t0 && t < t1).reverse().map(t => edge(t, k, -3))]);
  }
  const head = away ? giraffeHeadBack(C, S) : dir ? giraffeHeadSide(C, S) : giraffeHeadFront(C, S);
  s += `<g data-giraffe="head" transform="translate(${P(...end)}) scale(${(hk * (dir < 0 ? -a : a)).toFixed(3)} ${(hk * b).toFixed(3)})">${head}</g>`;
  return rot ? `<g transform="rotate(${f1(-rot)} ${P(...base)})">${s}</g>` : s;
}
// The giraffe's head's pieces, in its own units (the neck's top at 0 0, up -y; about as big as the friend's face): its
// horns (ossicones: [x, y] at the foot, [x, y] at the top, each), skin-covered in the coat's colour with their dark tufts
// (a baby's shorter, the tufts big and fluffy); a few small patches (the masai's jagged) at [x, y, r]; eyes ([x, y] each)
// with a shine and lashes, a baby's bigger; the ears ([side] each: out to that side toward us, behind in profile)
function giraffeHorns(C, S, horns, far = false) {
  const lift = S.baby ? 10 : 0, d = horns.map(([x0, y0, x1, y1]) => `M${x0} ${y0} L${x1} ${y1 + lift}`).join(" "), tuft = S.baby ? 12 : 8.5;
  const ink = far ? mix(S.mane, "#000000", 0.2) : S.mane, skin = mix(C.fur, C.belly, far ? 0.15 : 0.35);
  return `<path d="${d}" fill="none" stroke="${darker(C.fur)}" stroke-width="14" stroke-linecap="round"/><path d="${d}" fill="none" stroke="${skin}" stroke-width="9" stroke-linecap="round"/>` +
         `<path d="${horns.map(([, , x, y]) => `M${x - tuft} ${y + lift} a${tuft} ${tuft} 0 1 0 ${2 * tuft} 0 a${tuft} ${tuft} 0 1 0 ${-2 * tuft} 0 Z`).join(" ")}" fill="${ink}" stroke="${darker(ink)}" stroke-width="2"${S.baby ? ` stroke-dasharray="3 2"` : ""}/>`;
}
const giraffeSpots = (C, S, spots) => `<path d="${spots.map(([x, y, r]) => { const n = S.coat === "masai" ? 8 : 6, a0 = (x * 7 + y * 3) % 60;
  return "M" + Array.from({ length: n }, (_, i) => { const q = (a0 + i * 360 / n) * Math.PI / 180, k = S.coat === "masai" ? (i % 2 ? 0.72 : 1.12) : [1, 0.86, 1.1, 0.9, 1.04, 0.82][i];
    return f1(x + Math.cos(q) * r * k) + " " + f1(y + Math.sin(q) * r * k); }).join(" L") + "Z"; }).join(" ")}" fill="${C.fur}" stroke="${C.fur}" stroke-width="1.5" stroke-linejoin="round"/>`;
function giraffeEyes(C, S, eyes, lashes) {
  const k = S.baby ? 1.3 : 1, rx = 8 * k, ry = 9.5 * k;
  return `<path d="${eyes.map(([x, y]) => `M${f1(x - rx)} ${y} a${f1(rx)} ${f1(ry)} 0 1 0 ${f1(2 * rx)} 0 a${f1(rx)} ${f1(ry)} 0 1 0 ${f1(-2 * rx)} 0 Z`).join(" ")}" fill="${C.eyes}" stroke="#111111" stroke-width="1.5"/>` +
         `<path d="${eyes.map(([x, y]) => { const r = 2.6 * k, cx = x - 2.6 * k, cy = y - 3.6 * k; return `M${f1(cx - r)} ${f1(cy)} a${f1(r)} ${f1(r)} 0 1 0 ${f1(2 * r)} 0 a${f1(r)} ${f1(r)} 0 1 0 ${f1(-2 * r)} 0 Z`; }).join(" ")}" fill="#ffffff"/>` +
         `<path d="${lashes(k)}" fill="none" stroke="#2b1a10" stroke-width="1.8" stroke-linecap="round"/>`;
}
function giraffeEars(C, sides, back = false) {
  const ear = sx => `M${24 * sx} -66 C${44 * sx} -86 ${74 * sx} -82 ${86 * sx} -64 C${72 * sx} -52 ${44 * sx} -50 ${24 * sx} -56 Z`;
  const inner = sx => `M${32 * sx} -64 C${48 * sx} -76 ${66 * sx} -73 ${76 * sx} -64 C${62 * sx} -58 ${46 * sx} -56 ${32 * sx} -59 Z`;
  return `<path d="${sides.map(ear).join(" ")}" fill="${back ? mix(C.belly, C.fur, 0.3) : C.belly}" stroke="${darker(C.fur)}" stroke-width="3" stroke-linejoin="round"/>` +
         (back ? "" : `<path d="${sides.map(inner).join(" ")}" fill="${mix(C.fur, C.belly, 0.55)}"/>`);
}
// toward us: the ears out to the sides, the horns, the head long and narrowing to the soft muzzle, small patches over its
// brow and cheeks, big eyes at its sides, two nostrils and a smile
function giraffeHeadFront(C, S) {
  const head = "M0 -96 C24 -96 36 -84 36 -64 C36 -42 29 -14 25 6 C22 26 13 36 0 36 C-13 36 -22 26 -25 6 C-29 -14 -36 -42 -36 -64 C-36 -84 -24 -96 0 -96 Z";
  return giraffeEars(C, [-1, 1]) + giraffeHorns(C, S, [[-15, -84, -21, -112], [15, -84, 21, -112]]) +
    `<defs><clipPath id="giraffe-face"><path d="${head}"/></clipPath></defs><path d="${head}" fill="${C.belly}"/>` +
    `<g clip-path="url(#giraffe-face)">${giraffeSpots(C, S, [[0, -76, 9], [-20, -80, 6.5], [20, -80, 6.5], [-8, -96, 6], [9, -95, 6], [0, -55, 5.5], [-27, -28, 8], [27, -28, 8], [-31, -64, 5], [31, -64, 5]])}` +
    `<path d="M-30 4 C-22 -12 22 -12 30 4 L30 40 L-30 40 Z" fill="${S.muzzle}"/></g><path d="${head}" fill="none" stroke="${darker(C.fur)}" stroke-width="3.5"/>` +
    giraffeEyes(C, S, [[-21, -50], [21, -50]], k => [-1, 1].map(sx => `M${f1(sx * (21 + 5 * k))} ${f1(-50 - 7 * k)} l${f1(sx * 5)} -5 M${f1(sx * (21 + 1 * k))} ${f1(-50 - 9 * k)} l${f1(sx * 2)} -6 M${f1(sx * (21 + 8 * k))} ${f1(-50 - 3 * k)} l${f1(sx * 6)} -2`).join(" ")) +
    `<path d="M-6 13 Q-12 18 -10 25 Q-5 21 -6 13 Z M6 13 Q12 18 10 25 Q5 21 6 13 Z" fill="${mix(S.muzzle, "#000000", 0.6)}"/>` +
    `<path d="M-10 29 Q0 34 10 29" fill="none" stroke="${darker(C.fur)}" stroke-width="2" stroke-linecap="round"/>`;
}
// in profile (facing our right; drawn mirrored for our left): the far horn behind, the head from the back of the skull
// forward and down to the muzzle, patches on its cheek, the near horn, the ear back from behind the horns, an eye with
// its lashes, a nostril at the muzzle's tip and a smile
function giraffeHeadSide(C, S) {
  const head = "M-32 -40 C-36 -64 -16 -80 6 -76 C30 -72 54 -50 76 -32 C92 -20 98 -4 88 6 C78 14 60 12 44 10 C24 8 6 14 -10 14 C-26 14 -38 8 -37 -8 C-36 -22 -34 -32 -32 -40 Z";
  return giraffeHorns(C, S, [[-14, -66, -24, -98]], true) +
    `<defs><clipPath id="giraffe-face-side"><path d="${head}"/></clipPath></defs><path d="${head}" fill="${C.belly}"/>` +
    `<g clip-path="url(#giraffe-face-side)">${giraffeSpots(C, S, [[-14, -28, 8], [-22, -56, 6], [2, -64, 6], [6, -20, 7], [36, -50, 5.5], [24, -6, 6], [-28, -6, 6]])}` +
    `<path d="M58 -50 C78 -40 104 -24 100 2 C98 16 70 16 46 12 C48 -8 50 -30 58 -50 Z" fill="${S.muzzle}"/></g><path d="${head}" fill="none" stroke="${darker(C.fur)}" stroke-width="3.5"/>` +
    giraffeHorns(C, S, [[-2, -70, -10, -104]]) +
    `<path d="M-14 -60 C-36 -80 -64 -80 -78 -66 C-62 -54 -36 -50 -16 -52 Z" fill="${C.belly}" stroke="${darker(C.fur)}" stroke-width="3" stroke-linejoin="round"/>` +
    `<path d="M-22 -60 C-38 -71 -58 -72 -68 -66 C-56 -60 -38 -57 -22 -57 Z" fill="${mix(C.fur, C.belly, 0.55)}"/>` +
    giraffeEyes(C, S, [[20, -46]], k => `M${f1(20 - 4 * k)} ${f1(-46 - 8.5 * k)} l-3 -6 M20 ${f1(-46 - 9.5 * k)} l0 -7 M${f1(20 + 4 * k)} ${f1(-46 - 8.5 * k)} l3 -6`) +
    `<path d="M84 -14 Q90 -18 91 -11 Q87 -9 84 -14 Z" fill="${mix(S.muzzle, "#000000", 0.6)}"/>` +
    `<path d="M94 4 Q82 10 66 8" fill="none" stroke="${darker(C.fur)}" stroke-width="2" stroke-linecap="round"/>`;
}
// from behind: the ears' backs, the horns, the back of the head (its muzzle away from us, out of sight) with its patches
function giraffeHeadBack(C, S) {
  const head = "M0 -96 C24 -96 36 -84 36 -64 C36 -42 32 -16 26 0 C18 12 -18 12 -26 0 C-32 -16 -36 -42 -36 -64 C-36 -84 -24 -96 0 -96 Z";
  return giraffeEars(C, [-1, 1], true) + giraffeHorns(C, S, [[-15, -84, -21, -112], [15, -84, 21, -112]]) +
    `<defs><clipPath id="giraffe-face-back"><path d="${head}"/></clipPath></defs><path d="${head}" fill="${C.belly}"/>` +
    `<g clip-path="url(#giraffe-face-back)">${giraffeSpots(C, S, [[0, -72, 10], [-22, -78, 7], [22, -78, 7], [-24, -40, 8], [24, -40, 8], [0, -36, 9], [-10, -94, 6], [10, -94, 6], [0, -8, 7]])}</g>` +
    `<path d="${head}" fill="none" stroke="${darker(C.fur)}" stroke-width="3.5"/>`;
}
// Over the shirt (where the witch's belt goes), in body units (the collar's top at y -20, the shirt's half width w, its hem
// at L, the collar's half width nw): the rodeo star's chest fringe, the sheriff's vest, badge and string tie, the rider's
// yoke, buckle and number card, the prairie collar's bow, a bandana, the trail boss's shirt panel, lapels and capelet, the
// belt and its buckle. Facing us, front details; in profile, a side's own; from behind (also crawling toward us, where
// the back is turned up), the back's.
function westernTorso(W, w, L, nw, dir, away) {
  const S = W.S, P = (x, y) => f1(x) + " " + f1(y), X = d => dir ? dir * d : d, out = [];
  const fill = (d, f, sw = 2.5, line = darker(f)) => `<path d="${d}" fill="${f}" stroke="${line}" stroke-width="${sw}" stroke-linejoin="round"/>`;
  const star = (x, y, r, f, line) => `<polygon points="${starPts(x, y, r)}" fill="${f}" stroke="${line}" stroke-width="2" stroke-linejoin="round"/>`;
  // the rider's white yoke: facing us two points on the chest, from behind one, in profile one toward the front
  if (S.yoke) out.push(fill(dir ? `M${P(X(-(w + 2)), -6)} L${P(X(w + 2), -6)} L${P(X(w + 1), 14)} L${P(X(w * 0.35), 36)} L${P(X(-w * 0.1), 14)} L${P(X(-(w + 1)), 18)} Z`
    : away ? `M${P(-(w + 3), -6)} Q${P(0, -14)} ${P(w + 3, -6)} L${P(w + 2, 14)} L${P(0, 40)} L${P(-(w + 2), 14)} Z`
    : `M${P(-(w + 3), -6)} L${P(-nw, -20)} Q${P(0, -14)} ${P(nw, -20)} L${P(w + 3, -6)} L${P(w + 2, 14)} L${P(w * 0.5, 36)} L${P(w * 0.18, 14)} L${P(-w * 0.18, 14)} L${P(-w * 0.5, 36)} L${P(-(w + 2), 14)} Z`, S.yoke));
  // the rodeo star's fringe across the chest: a U facing us, shallower behind, a shorter arc toward the front in profile
  if (S.chestFringe) out.push(fringeSvg(dir ? `M${P(X(-w * 0.2), 10)} Q${P(X(w * 0.45), 36)} ${P(X(w + 2), 14)}` : `M${P(-w, 8)} Q${P(0, away ? 34 : 48)} ${P(w, 8)}`, S.fringe, 14, S.fringeUnder));
  // the trail boss's cream shirt panel between the duster's open fronts and their edges (one path); in profile its strip at
  // the chest's front
  if (S.duster && !away) out.push(dir ? fill(`M${P(X(w - 14), -18)} L${P(X(w), -16)} L${P(X(w + 2), L - 8)} L${P(X(w - 10), L - 8)} Z`, S.panel, 2)
    : fill(`M${P(-nw, -20)} L${P(nw, -20)} L${P(nw * 0.7, L)} L${P(-nw * 0.7, L)} Z`, S.panel, 2) +
      `<path d="M${P(-nw * 0.7, L)} L${P(-nw - 2, -20)} M${P(nw * 0.7, L)} L${P(nw + 2, -20)}" fill="none" stroke="${darker(W.main)}" stroke-width="3"/>`);
  // the sheriff's open vest: two front panels with western points at the hem; one panel at the side; one across the back
  if (W.vest) out.push(fill(dir ? `M${P(X(-(w + 1)), -4)} L${P(X(w - 8), -10)} L${P(X(w - 8), L - 2)} L${P(X(w * 0.2), L + 6)} L${P(X(-(w + 2)), L - 4)} Z`
    : away ? `M${P(-(w + 1), -4)} Q${P(0, -12)} ${P(w + 1, -4)} L${P(w + 2, L - 4)} L${P(-(w + 2), L - 4)} Z`
    : `M${P(-(w + 1), -4)} L${P(-nw - 2, -20)} L${P(-10, L * 0.5)} L${P(-10, L - 2)} L${P(-w * 0.5, L + 7)} L${P(-(w + 2), L - 4)} Z ` +
      `M${P(w + 1, -4)} L${P(nw + 2, -20)} L${P(10, L * 0.5)} L${P(10, L - 2)} L${P(w * 0.5, L + 7)} L${P(w + 2, L - 4)} Z`, W.vest));
  // his badge, a toy star, on his left chest (our right facing us); in profile whole on his left side (facing our left),
  // a gold sliver edge on at the chest's front from his right; none on his back
  if (S.badge && !away) out.push(!dir ? star(w * 0.48, 26, 13, S.badge, "#8a6420") : dir < 0 ? star(X(w * 0.66), 24, 12, S.badge, "#8a6420")
    : `<path d="M${P(X(w - 2), 16)} L${P(X(w - 1), 36)}" stroke="${S.badge}" stroke-width="5" stroke-linecap="round"/>`);
  if (S.belt) {
    out.push(`<rect x="${f1(-(w + 3))}" y="${f1(L - 16)}" width="${f1(2 * w + 6)}" height="12" fill="${S.belt}" stroke="${darker(S.belt)}" stroke-width="2"/>`);
    const big = S.bigBuckle ? 1.35 : 1;   // (the rider's trophy buckle, with a star in it)
    if (!away) out.push(dir ? `<rect x="${f1(dir * (w + 3) - (dir > 0 ? 7 : 0))}" y="${f1(L - 10 - 10 * big)}" width="7" height="${f1(20 * big)}" rx="2" fill="${S.buckle}" stroke="${darker(S.buckle)}" stroke-width="1.5"/>`
      : `<ellipse cx="0" cy="${f1(L - 10)}" rx="${f1(14 * big)}" ry="${f1(10 * big)}" fill="${S.buckle}" stroke="${darker(S.buckle)}" stroke-width="2.5"/>` +
        (S.bigBuckle ? `<polygon points="${starPts(0, L - 10, 7)}" fill="#8a6420"/>` : ""));
  }
  // the rider's contestant's card on his back, below where long hair ends, its 7 a path (no font)
  if (S.number && away) out.push(`<rect x="-24" y="68" width="48" height="44" rx="3" fill="#ffffff" stroke="#94a3b8" stroke-width="2"/>` +
                                 `<path d="M-9 78 L10 78 L-1 104" fill="none" stroke="#1f1f22" stroke-width="5" stroke-linejoin="round" stroke-linecap="round"/>`);
  // the sheriff's string tie and its silver slide (one string at the front in profile)
  if (S.tie && !away) out.push(dir ? `<path d="M${P(X(nw - 4), -18)} L${P(X(nw + 1), 34)}" stroke="${S.tie}" stroke-width="2.5" stroke-linecap="round"/>`
    : `<path d="M-3 -16 L-7 38 M3 -16 L7 38" fill="none" stroke="${S.tie}" stroke-width="2.5" stroke-linecap="round"/><ellipse cx="0" cy="-8" rx="6" ry="7" fill="#d1d5db" stroke="#6b7280" stroke-width="1.5"/>`);
  // the prairie collar's bow: two loops and two tails, one path; half of it at the front in profile; none behind
  if (S.collarBow && !away) out.push(fill(dir ? `M${P(X(nw - 2), -18)} Q${P(X(nw + 14), -28)} ${P(X(nw + 12), -12)} Z M${P(X(nw - 2), -16)} L${P(X(nw + 4), 2)} L${P(X(nw), 2)} Z`
    : `M0 -16 Q-16 -30 -18 -14 Q-14 -6 0 -16 Z M0 -16 Q16 -30 18 -14 Q14 -6 0 -16 Z M-2 -14 L-8 6 L-3 4 Z M2 -14 L8 6 L3 4 Z`, S.collarBow, 2, "#c9c2b4"));
  // a bandana at the neck (never over the face): its point down the chest facing us, at the front of the neck in profile
  // with its knot's tails at the nape, its knot and tails from behind; white dots on the cow-print's
  if (S.bandana) { const b = W.bandana, dots = S.bandana === "auto", dot = (x, y) => `M${P(x - 2.6, y)} a2.6 2.6 0 1 0 5.2 0 a2.6 2.6 0 1 0 -5.2 0`;
    const neck = `M${P(-(nw - 2), -30)} L${P(nw - 2, -30)} L${P(nw + 4, -18)} L${P(-(nw + 4), -18)} Z`;
    if (dir) out.push(fill(`M${P(X(-(nw - 2)), -30)} L${P(X(nw - 2), -30)} L${P(X(nw + 5), -18)} L${P(X(-(nw + 4)), -18)} Z M${P(X(nw * 0.1), -22)} L${P(X(nw + 12), -18)} L${P(X(nw + 3), 12)} Z ` +
                           `M${P(X(-nw), -26)} L${P(X(-(nw + 16)), -6)} L${P(X(-(nw + 7)), -4)} Z`, b, 2) + (dots ? `<path d="${dot(X(nw + 4), -8)}" fill="#ffffff"/>` : ""));
    else if (away) out.push(fill(`${neck} M-3 -24 L-15 0 L-6 2 Z M3 -24 L15 0 L6 2 Z M-6 -24 a6 5 0 1 0 12 0 a6 5 0 1 0 -12 0`, b, 2));
    else out.push(fill(`${neck} M${P(-(nw + 6), -19)} Q${P(0, -10)} ${P(nw + 6, -19)} L0 26 Z`, b, 2) +
                  (dots ? `<path d="${dot(-nw * 0.42, -8)} ${dot(nw * 0.42, -8)} ${dot(0, 9)}" fill="#ffffff"/>` : "")); }
  // the trail boss's capelet over the shoulders, a shade darker than the duster: facing us open at the front (two pieces,
  // one path), round the back from behind, its own shape at the side in profile
  if (S.duster) { const c = mix(W.main, "#000000", 0.15), cw = w + 12, cy = 34;
    out.push(fill(dir ? `M${P(X(-8), -24)} Q${P(X(w), -24)} ${P(X(w + 8), 0)} L${P(X(w + 6), cy)} Q${P(0, cy + 8)} ${P(X(-(w + 10)), cy - 6)} L${P(X(-(w + 6)), 0)} Z`
      : away ? `M${P(-nw - 2, -22)} Q${P(0, -14)} ${P(nw + 2, -22)} L${P(cw, -2)} L${P(cw, cy)} Q${P(0, cy + 12)} ${P(-cw, cy)} L${P(-cw, -2)} Z`
      : [-1, 1].map(s => `M${P(s * (nw + 2), -22)} Q${P(s * w, -24)} ${P(s * cw, -2)} L${P(s * cw, cy)} Q${P(s * (cw + nw) / 2, cy + 8)} ${P(s * (nw + 4), cy - 4)} Z`).join(" "), c, 2.5, darker(W.main))); }
  return out.join("");
}
// the rodeo star's sleeve fringe: strands standing out from the sleeve's outer edge (in profile, the arm's back edge), one
// dashed stroke over its dark under-stroke, following the arm through every pose
function westernArm(W, sleevePts, armW, side, dir) {
  if (!W.S.sleeveFringe || !sleevePts) return "";
  return fringeSvg(offsetPath(sleevePts, dir ? -dir : side, (armW + 9) / 2 + 3), W.S.fringe, 11, W.S.fringeUnder);
}
// the cow-print's chaps over a pants leg, J its joints from the hip, along the leg's own joints (so they bend with the
// knee): an outline stroke and a fill stroke wider than the pants, stopping 14 above the ankle; from behind only a strip
// down the outside (chaps are open at the back). On a leg folded past 100 degrees (meditating), along the thigh only.
// The spots: one path of blobs placed along the joints (three; two on the strip behind)
function westernChap(W, J, half, side, dir, away) {
  const S = W.S, wide = 2 * half + 8, P2 = (x, y) => f1(x) + " " + f1(y);
  let cut = 14;
  if (J.length > 2) { const [A, K, F] = J, ax = A[0] - K[0], ay = A[1] - K[1], fx = F[0] - K[0], fy = F[1] - K[1];
    const inner = Math.acos(Math.max(-1, Math.min(1, (ax * fx + ay * fy) / ((Math.hypot(ax, ay) * Math.hypot(fx, fy)) || 1)))) * 180 / Math.PI;
    if (180 - inner > 100) { J = [A, K]; cut = 0; } }
  { const [A, B2] = J, l = Math.hypot(B2[0] - A[0], B2[1] - A[1]) || 1, k = Math.min(0.4, 12 / l);   // (from 12 below the hip: the jeans show above)
    J = [[A[0] + (B2[0] - A[0]) * k, A[1] + (B2[1] - A[1]) * k], ...J.slice(1)]; }
  const d = away ? offsetPath(J, side, half * 0.55, cut) : offsetPath(J, side, 0, cut), sw = away ? half * 0.95 : wide;
  let s = `<path d="${d}" fill="none" stroke="${S.chapLine}" stroke-width="${f1(sw + 4)}" stroke-linejoin="round"/>` +
          `<path d="${d}" fill="none" stroke="${S.chaps}" stroke-width="${f1(sw)}" stroke-linejoin="round"/>`;
  const segs = J.slice(1).map((b, i) => [J[i], b]), len = segs.reduce((a, [p, q]) => a + Math.hypot(q[0] - p[0], q[1] - p[1]), 0) - cut;
  const at = t => { let r = t * len; for (const [p, q] of segs) { const l = Math.hypot(q[0] - p[0], q[1] - p[1]) || 1; if (r <= l) { const k = r / l; return [p[0] + (q[0] - p[0]) * k, p[1] + (q[1] - p[1]) * k, (q[0] - p[0]) / l, (q[1] - p[1]) / l]; } r -= l; }
    const [, q] = segs[segs.length - 1]; return [q[0], q[1], 0, 1]; };
  const blob = (t, off, r) => { const [x, y, ux, uy] = at(t), cx = x + side * uy * off, cy = y - side * ux * off, a = r, b = r * 0.7;
    return `M${P2(cx - a, cy)} Q${P2(cx - a, cy - b)} ${P2(cx - a * 0.2, cy - b * 1.1)} Q${P2(cx + a, cy - b * 0.9)} ${P2(cx + a * 0.9, cy + b * 0.1)} Q${P2(cx + a * 0.8, cy + b)} ${P2(cx, cy + b * 0.9)} Q${P2(cx - a * 0.9, cy + b * 1.1)} ${P2(cx - a, cy)} Z`; };
  const spots = away ? [[0.3, half * 0.55, half * 0.3], [0.72, half * 0.6, half * 0.26]]
                     : [[0.16, -half * 0.25, half * 0.48], [0.5, half * 0.35, half * 0.42], [0.82, -half * 0.15, half * 0.4]];
  return s + `<path d="${spots.map(([t, o, r]) => blob(t, o, r)).join(" ")}" fill="${S.spots}"/>`;
}
// the rodeo star's skirt: fringe under its hem, following the skirt drawNow draws (to the hem at sl below the waist; ticket
// 546: from xR to xL, where the skirt is drawn wider over posed legs)
function westernHem(W, hw, L, sl, xL = -hw - 30, xR = hw + 30) {
  if (!W.S.hemFringe) return "";
  return fringeSvg(`M${f1(xR)} ${f1(L + sl + 8)} Q${f1((xL + xR) / 2)} ${f1(L + sl + 22)} ${f1(xL)} ${f1(L + sl + 8)}`, W.S.fringe, 16, W.S.fringeUnder);
}
// the trail boss's duster skirts, from the waist to `hem` (mid-shin standing, L + 0.72 LL; on the lap meditating), over
// the trousers and under the arms (the gnome tunic's slot): facing us two panels flaring apart, the trousers between; in profile one panel, its front edge straight
// and its back flaring behind (the legs walk inside it, as in a robe); from behind one panel and its centre vent
function westernCoat(W, w, hw, L, hem, dir, away) {
  if (!W.S.duster) return "";
  const c = W.main, cd = darker(c), P = (x, y) => f1(x) + " " + f1(y);
  const fill = d => `<path d="${d}" fill="${c}" stroke="${cd}" stroke-width="2.5" stroke-linejoin="round"/>`;
  if (dir) return fill(`M${P(-dir * (w + 2), L - 16)} L${P(dir * (w + 2), L - 16)} L${P(dir * (w + 4), hem)} L${P(-dir * (w + 34), hem - 6)} Z`);
  if (away) return fill(`M${P(-(hw + 4), L - 16)} L${P(hw + 4, L - 16)} L${P(hw + 26, hem)} L${P(-(hw + 26), hem)} Z`) +
                   `<path d="M0 ${f1(L + 20)} L0 ${f1(hem)}" stroke="${cd}" stroke-width="2.5"/>`;
  return fill(`M${P(-(hw + 4), L - 16)} L${P(-8, L - 16)} L${P(-hw * 0.55, hem)} L${P(-(hw + 26), hem)} Z ` +
              `M${P(hw + 4, L - 16)} L${P(8, L - 16)} L${P(hw * 0.55, hem)} L${P(hw + 26, hem)} Z`);
}
