"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 25 of 26 of the game's script, run in order by index.html.

// ===================== THE WORLD =====================
// (ticket 185, Matthew, 2026-10-17) The world designer's model. The world
// (state.world, WORLD_DEFAULT) is places laid on a grid: cells keyed
// "column,row", each a place and its ways out by direction (a forest
// crossing's are its paths). Two neighbours are joined through the ways on
// their shared side, never by wiring a door to a door: the way E of one
// meets the way W of the square to its east, NE meets SW, and so on. A way
// that leads off the map, into an empty square, or to a place with no way
// back is a mistake, listed, as the castle's map lists its own. The insides
// (a house's living room, the castle's rooms, the witch's hut...) are
// reached through their outside's door, so they are off the grid, each with
// an id of its own; so are the places reached their own way (our
// imagination, the empty room). Since ticket 524 (Matthew: "we need to make
// each room, and its params, available to world builder") an inside can be
// put in a square too, as any place: then it is that square, and its
// outside's door leads there. world.at is the square the friend is in.
const WORLD_DIRS = { N: [0, -1], NE: [1, -1], E: [1, 0], SE: [1, 1], S: [0, 1], SW: [-1, 1], W: [-1, 0], NW: [-1, -1] };
const WORLD_OPP = { N: "S", NE: "SW", E: "W", SE: "NW", S: "N", SW: "NE", W: "E", NW: "SE" };
const WORLD_ARROWS = { N: "⬆️", NE: "↗️", E: "➡️", SE: "↘️", S: "⬇️", SW: "↙️", W: "⬅️", NW: "↖️" };
const INSIDES = { house: ["livingroom"], treehouse: ["treeroom", "treetop"], witch: ["witchroom"], clearing: ["healerroom"], campsite: ["tent"],
                  waterfall: ["cave", "caveroom"], castle: ["castleroom"], lake: ["underwater"], hut: ["hutroom", "hutbed", "hutpotion", "hutcenter"] };
const INSIDE_KINDS = new Set(Object.values(INSIDES).flat());
const ELSEWHERE = ["imagination", "emptyroom", "bubblespace", "bubbleroom"];
// (ticket 524) the insides an outside has several of, each by its own number: a tree house's rooms, the camp's tents, a
// castle's rooms. A square holding one says which (its key, kept with the square: cellKeys), from 1 to insideCount.
const INSIDE_WHICH = { treeroom: "inside", tent: "tent", castleroom: "castleRoom" };
function insideCount(B, kind) {
  if (kind === "treeroom") return Math.max(1, Math.min(6, Math.round(B.houses ?? 4)));
  if (kind === "tent") return CAMP_TENTS.length;
  if (kind === "castleroom") { const id = CASTLE_DEFAULTS[B.castle] ? B.castle : "N"; return Math.max(1, Math.min(ROOM_MAX, Math.round(B["castleRooms" + id] ?? CASTLE_ROOMS[id].length))); }   // (castleOf's count, not the whole castle: ticket 167)
  return 1;
}
// (ticket 524) a square holds the place shown: its kind, and for an inside an outside has several of, that one (castle
// room 2 is not castle room 1's square, though both are castle rooms)
function cellHolds(cell, B) {
  const w = INSIDE_WHICH[B.kind];
  return !!cell && cell.kind === B.kind && (!w || Math.round(cell[w] ?? 1) === Math.round(B[w] ?? 1));
}
// (ticket 524) the first of an inside an outside has several of that no square holds (1 when every one is in a square)
function worldFreeWhich(W, B, kind) {
  const w = INSIDE_WHICH[kind];
  if (!w) return undefined;
  for (let n = 1; n <= insideCount(B, kind); n++) if (!Object.values(W.cells).some(c => cellHolds(c, { kind, [w]: n }))) return n;
  return 1;
}
// (ticket 195, M) The waterfall's north is the waterfall itself, into the
// cave behind it: "having both and having them mean different things is
// too strange", so no way north on the grid there (WORLD_NO_WAY: never a
// way, as off the map's edge; one kept from before is ignored). And the
// ways a place's own scene has no room for, which 🔗 all passageways
// leaves out (WORLD_UNNATURAL): the beach's sea, the castle and the house
// at their backs, the wood path's diagonals (thick woods either side of its
// one path). A forest crossing has all eight; an inside put in a square
// (ticket 524) all its own, as any place.
const WORLD_NO_WAY = { waterfall: ["N"] };
const WORLD_UNNATURAL = { beach: ["N", "NE", "NW"], castle: ["N"], house: ["N"], woodpath: ["NE", "NW", "SE", "SW"] };
const worldNoWay = (kind, d) => (WORLD_NO_WAY[kind] || []).includes(d);
const worldNatural = (kind, d) => !worldNoWay(kind, d) && !(WORLD_UNNATURAL[kind] || []).includes(d);
// (ticket 186) open water meets open water, and the beach's sea side (its N and corners), nothing else
const SEA_SIDE = ["N", "NE", "NW"], DEEP_WATER = new Set(["sea", "deepsea"]);
function worldNaturalPair(a, d, b) {
  if (worldNoWay(a, d) || worldNoWay(b, WORLD_OPP[d])) return false;
  // (ticket 219: the deep sea is deep water too: it meets open water and other deep sea any way, and nothing on land)
  if (DEEP_WATER.has(a) || DEEP_WATER.has(b)) return (DEEP_WATER.has(a) && DEEP_WATER.has(b)) || (a === "beach" && b === "sea" && SEA_SIDE.includes(d)) || (b === "beach" && a === "sea" && SEA_SIDE.includes(WORLD_OPP[d]));
  return worldNatural(a, d) && worldNatural(b, WORLD_OPP[d]);
}
const worldWays = cell => !cell ? [] : (cell.kind === "crossing" ? CROSS_DIRS.map(([d]) => d).filter(d => cell["cross" + d]) : (cell.ways || [])).filter(d => !worldNoWay(cell.kind, d));
// (Matthew, 2026-10-17: a crossing missing a passageway has no trail and no
// arrow that way) A forest crossing in the world shows only the paths that
// lead somewhere: switched on, and met by a way back from the square beside
// it. Its others are not drawn, and are no mistake. Any other place shows
// all its ways, a way to nowhere grey with 🚧, a mistake to put right.
function worldLiveWays(W, key) {
  const cell = W.cells[key], ways = worldWays(cell);
  if (!cell || cell.kind !== "crossing") return ways;
  return ways.filter(d => { if (cell.links && cell.links[d]) return !!worldTarget(W, key, d);   // (ticket 406: a way to another world leads somewhere)
    const nb = worldNeighbour(W, key, d), other = W.cells[nb.key]; return !nb.off && other && worldWays(other).includes(WORLD_OPP[d]); });
}
const placeName = kind => (PLACES.find(p => p[0] === kind) || [kind, kind])[1];
// a square's name, as on a map: a letter for the column, a number for the row
const squareName = key => { const [c, r] = key.split(",").map(Number); return String.fromCharCode(65 + c) + (r + 1); };
// (Matthew, 2026-10-17) Every square has a name of its own, for the signs and
// arrows that point to it: the designer's (cell.name), else its place's,
// with where it is (A5) when another square holds that place too. Two
// squares with one name is a warning (worldProblems).
const WORLD_NAME_MAX = 24;
function worldNames(W, theirs = true) {   // (theirs false: the names the places give, as the box's hint)
  const keys = worldKeys(W), count = {};
  for (const k of keys) { const kd = W.cells[k].kind; count[kd] = (count[kd] || 0) + 1; }
  return Object.fromEntries(keys.map(k => { const c = W.cells[k], own = theirs ? String(c.name || "").trim().slice(0, WORLD_NAME_MAX) : "";
    return [k, own || (count[c.kind] > 1 ? `${placeName(c.kind)} ${squareName(k)}` : placeName(c.kind))]; }));
}
const worldName = (W, key) => worldNames(W)[key] || "";
// (M, 2026-10-17: so there's no way to make that mistake) a way out of a square off the map's edge can't be turned on
const worldOffMap = (W, key, d) => worldNeighbour(W, key, d).off;
// (M, 2026-10-17; ticket 191, Matthew: and "when taking away a passage
// fixes the error -- especially when passages are placed that lead to empty
// grid squares") The ways out of a square whose toggle would put a warning
// right: off ones that, on, would leave fewer warnings (one meeting a way
// that waits for it, or joining a square no one could reach), and on ones
// that, taken away, would (one into an empty square, off the map, or to a
// square with no way back). Never one that only trades one warning for
// another.
function worldWaysToToggle(W, key, now = worldProblems(W).length) {
  const cell = W.cells[key], out = { on: [], off: [] };
  if (!cell) return out;
  const ways = worldWays(cell);
  for (const d of Object.keys(WORLD_DIRS)) {
    const isOn = ways.includes(d);
    if (cell.links && cell.links[d]) continue;   // (ticket 406: a way linked to another world is the designer's, not a mistake to toggle)
    if (!isOn && (worldOffMap(W, key, d) || worldNoWay(cell.kind, d))) continue;
    const c2 = cell.kind === "crossing" ? { ...cell, ["cross" + d]: !isOn } : { ...cell, ways: isOn ? ways.filter(x => x !== d) : [...ways, d] };
    if (worldProblems({ ...W, cells: { ...W.cells, [key]: c2 } }).length < now) out[isOn ? "off" : "on"].push(d);
  }
  return out;
}
const worldWaysToTurnOn = (W, key) => worldWaysToToggle(W, key).on;
// (ticket 195, M: "place matching passage ways on all adjacent grid squares, but only if both sides
// the scene naturally have those passage ways there") every pair of neighbouring squares whose places
// both have that way by nature, joined both ways; how many ways were added
function worldJoinAll(W) {
  let added = 0;
  const order = Object.keys(WORLD_DIRS);
  for (const key of worldKeys(W)) {
    const a = W.cells[key];
    for (const d of order) {   // (ticket 524: an inside in a square is joined as any place)
      const nb = worldNeighbour(W, key, d), b = W.cells[nb.key];
      if (nb.off || !b || !worldNaturalPair(a.kind, d, b.kind) || worldWays(a).includes(d)) continue;
      if (a.kind === "crossing") a["cross" + d] = true;
      else a.ways = order.filter(x => x === d || worldWays(a).includes(x));
      added++;
    }
  }
  // (the square the friend is in, a crossing: its paths are the place's settings too)
  const at = W.at && W.cells[W.at], B = state.background;
  if (at && at.kind === "crossing" && B.kind === "crossing") for (const d of order) B["cross" + d] = !!at["cross" + d];
  return added;
}
// (ticket 191: "the red square on the grid should correspond to the placement of the dotted red
// suggestion", not its neighbour) the squares whose ways could put a warning right; and for a warning
// no way can (a name twice, no start), its own square
function worldFixSquares(W, P = worldProblems(W)) {
  const now = P.length, red = new Set();
  for (const key of worldKeys(W)) { const t = worldWaysToToggle(W, key, now); if (t.on.length || t.off.length) red.add(key); }
  for (const p of P) if (!p.dir && !/can't be reached/.test(p.text) && p.key) red.add(p.key);
  return red;
}
// the square the way d out of key leads to, if it meets a way back there; else null
function worldMeets(W, key, d) {
  const nb = worldNeighbour(W, key, d), other = W.cells[nb.key];
  return !nb.off && other && worldWays(other).includes(WORLD_OPP[d]) ? nb.key : null;
}
const worldKeys = W => Object.keys(W.cells).sort((a, b) => { const [ac, ar] = a.split(",").map(Number), [bc, br] = b.split(",").map(Number); return ar - br || ac - bc; });
function worldNeighbour(W, key, d) {
  const [c, r] = key.split(",").map(Number), [dx, dy] = WORLD_DIRS[d], nc = c + dx, nr = r + dy;
  return { key: nc + "," + nr, off: nc < 0 || nr < 0 || nc >= W.cols || nr >= W.rows };
}
// the places off the grid, each with its id: the insides, by their outside's square, then the rest. (ticket 524) One
// that is in a square of its own is not off the grid: its outside's door leads to that square (worldSquareFor); an inside
// an outside has several of (INSIDE_WHICH) stays, as the square holds only one of them.
function worldOffGrid(W) {
  const out = [], has = new Set(Object.values(W.cells).map(c => c.kind)); let n = 0;
  for (const key of worldKeys(W)) for (const kind of INSIDES[W.cells[key].kind] || []) if (INSIDE_WHICH[kind] || !has.has(kind))
    out.push({ id: "inside " + (++n), kind, from: key, ...(INSIDE_WHICH[kind] ? { which: worldFreeWhich(W, state.background, kind) } : {}) });   // (which one: one not in a square)
  ELSEWHERE.forEach((kind, i) => { if (!has.has(kind)) out.push({ id: "elsewhere " + (i + 1), kind, from: null }); });
  return out;
}
// (ticket 524) a place off the grid, gone to from 🗺️ World design (its chip tapped): an inside through its outside's
// square (the friend in that square, so the inside is that square's: instanceOf), a place reached its own way as it is
function worldVisit(o) {
  const W = state.world, B = state.background;
  if (o.from && W.cells[o.from]) worldEnter(o.from, null, false);
  B.kind = o.kind;
  if (o.which) B[INSIDE_WHICH[o.kind]] = o.which;   // (one not in a square, so the friend is not taken to one that is: worldKeep)
  [B.figX, B.figY] = ARRIVE[o.kind] || [0, 0]; arriveSettle = true; arriveSide = null;
  changed();
}
function worldProblems(W) {
  const P = [], name = key => `${placeName(W.cells[key].kind)} (${squareName(key)})`;
  for (const key of worldKeys(W)) {
    const cell = W.cells[key];   // (ticket 524: an inside in a square is a square as any other, no mistake)
    for (const d of cell.kind === "crossing" ? worldLiveWays(W, key) : worldWays(cell)) {   // (a crossing's dead ends are no mistake: not drawn)
      // (ticket 406: a way linked to a square of a world leads there, if that world and square are still there)
      if (cell.links && cell.links[d]) { if (!worldTarget(W, key, d)) P.push({ key, dir: d, text: `The way ${WORLD_ARROWS[d]} out of ${name(key)} is linked to a world or a square that is gone.` }); continue; }
      const nb = worldNeighbour(W, key, d), other = W.cells[nb.key];
      if (nb.off) P.push({ key, dir: d, text: `The way ${WORLD_ARROWS[d]} out of ${name(key)} leads off the map.` });
      else if (!other) P.push({ key, dir: d, to: nb.key, text: `The way ${WORLD_ARROWS[d]} out of ${name(key)} leads to an empty square, ${squareName(nb.key)}.` });
      else if (!worldWays(other).includes(WORLD_OPP[d])) P.push({ key, dir: d, to: nb.key, text: `The way ${WORLD_ARROWS[d]} out of ${name(key)} meets ${name(nb.key)}, which has no way back ${WORLD_ARROWS[WORLD_OPP[d]]}.` });
    }
  }
  // every square reached from the friend's start, through ways that meet
  const seen = new Set(), q = W.cells[W.start] ? [W.start] : [];
  if (!q.length) P.push({ key: null, text: W.start ? `The friend starts at ${squareName(W.start)}, an empty square.` : "The world has no start." });
  q.forEach(k => seen.add(k));
  while (q.length) {
    const key = q.shift();
    for (const d of worldWays(W.cells[key])) {
      const nb = worldNeighbour(W, key, d), other = W.cells[nb.key];
      if (!nb.off && other && worldWays(other).includes(WORLD_OPP[d]) && !seen.has(nb.key)) { seen.add(nb.key); q.push(nb.key); }
    }
  }
  if (seen.size) for (const key of worldKeys(W)) if (!seen.has(key))
    P.push({ key, text: `${name(key)} can't be reached from where the friend starts (${squareName(W.start)}).` });
  // (ticket 525) a cave door whose square or world is gone, or whose twin there, its way back, is gone
  const doorAt = (T, where) => { for (const t of T || []) if (t.kind === "passage" && t.dest) {
    const at = where.key ? name(where.key) : placeName(where.off.split(":")[0]);
    if (!passageDestOk(t.dest)) P.push({ key: where.key || null, text: `The 🕳️ cave passage in ${at} leads to a world or a square that is gone.` });
    else if (!passageTwin(t)) P.push({ key: where.key || null, text: `The 🕳️ cave passage in ${at} has no way back: its passage there is gone.` });
  } };
  for (const key of worldKeys(W)) doorAt(W.cells[key].things, { key });
  for (const [off, T] of Object.entries(W.off || {})) doorAt(T, { off });
  // two squares with one name: a sign could not say which it means
  const names = worldNames(W), by = {};
  for (const [k, nm] of Object.entries(names)) (by[nm.toLowerCase()] = by[nm.toLowerCase()] || []).push(k);
  for (const ks of Object.values(by)) if (ks.length > 1)
    P.push({ key: ks[0], to: ks[1], text: `${ks.length === 2 ? "Two" : ks.length} squares are named "${names[ks[0]]}": ${ks.map(squareName).join(", ")}. Give one a name of its own.` });
  return P;
}
// What a square keeps of its place, as the Background tab edits it
// (state.background), so the square opened is the square changed: its
// things, any place's; and a forest crossing's paths and floor.
const CROSS_KEYS = ["crossN", "crossNE", "crossE", "crossSE", "crossS", "crossSW", "crossW", "crossNW", "crossFloor", "crossBack"];
// (ticket 524) and an inside an outside has several of: which one it is (INSIDE_WHICH)
const cellKeys = kind => kind === "crossing" ? ["things", ...CROSS_KEYS] : INSIDE_WHICH[kind] ? ["things", INSIDE_WHICH[kind]] : ["things"];
// where the friend comes in, walking into a square from a side: just
// inside that side, as if they had come through it (the stage's units, as
// figX and figY: across, and nearer or farther than the ground line)
const WORLD_IN = { N: [0, -70], S: [0, 75], E: [270, 30], W: [-270, 30], NE: [210, -55], NW: [-210, -55], SE: [230, 70], SW: [-230, 70] };
// go to a square: the friend into its place, with its own settings; from a
// side when walking in through it, else where the place puts them
function worldEnter(key, side = null, draw = true) {
  const W = state.world, cell = W.cells[key];
  if (!cell) return;
  W.at = key;
  const B = state.background;
  B.kind = cell.kind;
  for (const k of cellKeys(cell.kind)) B[k] = structuredClone(cell[k] ?? (k === "things" ? [] : DEFAULTS.background[k]));
  [B.figX, B.figY] = side ? WORLD_IN[side] : ARRIVE[cell.kind] || [0, 0]; arriveSettle = true; arriveSide = side;   // (ticket 312: arrowsClear)
  if (draw) changed();
}
// The ways out of the square the friend is in (Matthew, 2026-10-17: "how do
// I access those passageways in the game?"; then: only when near them, and
// on top of the picture's buttons; and their words on short lines, kept on
// the screen). In the picture, each way is an invisible doorway (data-way)
// at the edge it leads off: N at the far end of the ground, S at the
// bottom, E and W at the sides, the four between at the corners. It is what
// the friend walks to, what glows when they are near, what X goes through.
// Over the picture and its buttons, in #ways, each way's arrow, a bobbing
// triangle pointing out, shown only while the friend's feet are within
// reach of it (WAY_NEAR): then it is on top, and a tap on it walks through;
// hidden, it takes no taps. Its words, where it leads, on two short lines,
// on its side toward the middle of the picture. A way that is a mistake,
// leading nowhere, is grey, 🚧, and goes nowhere. A forest crossing shows
// only its live ways (worldLiveWays).
// (Matthew, 2026-10-17: M finds the warnings distracting) The World design
// tab's warnings are hidden unless asked for: a chip near its top shows how
// many there are, and "show warnings" lists them and marks their squares in
// red. Whether they show is the browser's own (localStorage
// "dress-up-world-warnings"), not the friend's, so not in saves.
const WORLD_WARN_KEY = "dress-up-world-warnings";
let worldWarnShown = false;
try { worldWarnShown = localStorage.getItem(WORLD_WARN_KEY) === "1"; } catch (_) { /* (hidden) */ }
function worldWarnShow(on) {
  worldWarnShown = on;
  try { if (on) localStorage.setItem(WORLD_WARN_KEY, "1"); else localStorage.removeItem(WORLD_WARN_KEY); } catch (_) { /* (for this visit only) */ }
}
// (Matthew, 2026-10-17: by the feet, not the whole body, and on the floor,
// with depth and perspective) A way is a spot on the ground: N at the far
// line, S at the near edge, E and W at the sides. How near the friend is to
// it is measured on the floor: the feet and the spot each taken where they
// are on the ground, the perspective undone (floorAt: across by the size
// a thing has there, away as the eye's distance at the arrival over it),
// so a step toward the horizon covers more ground than one near us.
// Near is within WAY_NEAR, or half the way's distance from the middle of
// the floor if that is less (the near edge is only a step in front of
// where the friend arrives: S would show from the middle). Feet gone past
// a way, toward its edge, count as at it (wayNearness: below 1, near).
const WAY_NEAR = 170, WAY_DEPTH = 500;   // (the floor's units: the stage's, at the place's arrival size)
const WAY_OUT = { N: [0, 1], S: [0, -1], E: [1, 0], W: [-1, 0], NE: [1, 1], NW: [-1, 1], SE: [1, -1], SW: [-1, -1] };   // (outward, on the floor: across, away)
let worldWaysAt = [];   // (each way's [direction, x, y, angle, leads to], as last drawn)
function floorAt(B, x, y) { const s = Math.max(0.05, depthSize(B, y - SCENE.ground)); return [x / s, WAY_DEPTH / s]; }
// (ticket 187) how near the feet are to a spot on the floor, as a share of
// how near counts (below 1: within reach): within WAY_NEAR, or half the
// spot's distance from the floor's middle if that is less (never under
// PASS_MIN); o, a way's outward direction, counts feet gone past the spot,
// toward its edge, as at it. The nav arrows' measure, and every passage's
// that stands on the floor (doors, signs, ladders, pits: passesNear).
const PASS_MIN = 60;
function floorNearness(x, y, o = null) {
  // (0.0.159: a spot past the back limit, where the friend cannot go (the clearing's castle sign, its tree house's
  // doorway), is measured at the limit: since 0.0.153's 🔭 shrinks a place's far edge so much, the depth between the
  // limit and a spot beyond it, in the floor's own measure, had put such passages out of anyone's reach; passages187)
  const yb = SCENE.ground + figYMin(sceneFrame);
  if (y < yb) y = yb;
  const B = state.background, a = floorAt(B, x, y), f = floorAt(B, B.figX ?? 0, SCENE.ground + (B.figY ?? 0));
  const c = floorAt(B, 0, SCENE.ground + (ARRIVE[B.kind] || [0, 0])[1]);
  const at = i => o && o[i] && (f[i] - a[i]) * o[i] > 0 ? a[i] : f[i];
  return Math.hypot(at(0) - a[0], at(1) - a[1]) / Math.max(PASS_MIN, Math.min(WAY_NEAR, Math.hypot(a[0] - c[0], a[1] - c[1]) / 2));
}
// (ticket 369, world185 in 0.0.326's speed run: 0.0.321's spacing moved E's and W's arrows up off SE's and SW's, and with them where
// the way stood on the floor, so a friend walked to the east edge at an everyday depth was no longer near E and saw no arrow) a way's
// spot on the floor, where the feet are judged near it and where a walk to it goes, is where it was before the arrows were spaced
// (wayHomes, kept by worldWaysSvg); its arrow is drawn spaced
let wayHomes = {};
function waySpot(d) {
  const w = worldWaysAt.find(e => e[0] === d);
  return !w ? null : wayHomes[d] || [w[1], w[2]];
}
function wayNearness(d) {
  const p = waySpot(d);
  return p ? floorNearness(p[0], p[1], WAY_OUT[d] || null) : Infinity;   // (ticket 525: a cave door's arrow, by its foot, no outward way)
}
// (the way X goes through, aimed at: its arrow glows, as a door's frame does)
function worldWaysAim(d) {
  for (const el of document.querySelectorAll("#ways .way-arrow")) { const on = el.dataset.way === d; if (el.classList.contains("aimed") !== on) el.classList.toggle("aimed", on); }
}
// (ticket 233, Matthew: "the implicit exit on the S of some rooms was missing an indicator to the game player -- but
// the nav arrows are the UX way we've chosen"; "rather than render a sign to indicate it and offer a click, we use the
// nav arrow UX now") A place off the grid has ways toward us too: the living room's door out (livingOut), a tree room's
// (treeBackN), a castle room's (castleSouthWays). Each that leads somewhere is a way as a world square's is, an
// invisible doorway at the bottom edge (data-way, and its own data-to: enterScene goes there, not to a square beside)
// and an arrow over the picture when the friend is near; one leading nowhere is not there at all.
function insideWays(B) {
  const to = kd => { const P = kd && kd !== "none" && PLACES.find(p => p[0] === kd); return P ? [{ d: "S", to: kd, attrs: `data-to="${kd}"`, label: P[1] }] : []; };
  if (B.kind === "livingroom") return to(B.livingOut);
  if (B.kind === "hutroom") return to(B.hutCenter ? "hutcenter" : "hut");   // (ticket 278: toward us, out of the grass hut; 282: into its center, with one)
  if (B.kind === "hutcenter") return to("hut");
  if (B.kind === "treetop") return to(B.treetopBack);   // (ticket 516)
  if (B.kind === "treeroom") return to(B["treeBack" + Math.max(1, Math.min(6, B.houses ?? 4, Math.round(B.inside ?? 1)))]);
  if (B.kind === "castleroom") { const K = castleOf(B); return castleSouthWays(K, Math.max(1, Math.min(K.rooms.length, Math.round(B.castleRoom ?? 1)))); }
  return [];
}
// (a way's doorway: invisible, and not tapped, the arrow over the picture is; its shape, the triangle, is what the glow frames)
// (ticket 313) an arrow's triangle's box in the picture, turned to its angle: [left, top, right, bottom]
const wayTriBox = (x, y, a, k) => { const r = 28 * k, c = Math.cos(a * Math.PI / 180), s = Math.sin(a * Math.PI / 180), P = [[-0.6 * r, -0.8 * r], [0.9 * r, 0], [-0.6 * r, 0.8 * r]].map(([u, v]) => [x + u * c - v * s, y + u * s + v * c]);
  return [Math.min(...P.map(q => q[0])), Math.min(...P.map(q => q[1])), Math.max(...P.map(q => q[0])), Math.max(...P.map(q => q[1]))]; };
// (ticket 313, Matthew: "the nav arrows as objects are often still overlapped in terms of their bounding boxes -- maybe we should space
// them further still, on each background?" "especially SE and E, and SW and W") On every background, a side's arrow (E, W) slid up off
// the corner's below it (SE, SW), a gap between their boxes, and not onto the far corner's above it (NE, NW) where they meet across:
// with no room for both gaps, midway between the two
function waysSpaced(AT, k) {
  const gap = 14 * k, across = (a, b) => Math.min(a[2], b[2]) > Math.max(a[0], b[0]);
  for (const [side, low, high] of [["E", "SE", "NE"], ["W", "SW", "NW"]]) {
    const S = AT[side], b = wayTriBox(...S, k), up = S[1] - b[1], down = b[3] - S[1], L = wayTriBox(...AT[low], k), H = wayTriBox(...AT[high], k);
    const most = across(b, L) ? L[1] - gap - down : Infinity, least = across(b, H) ? H[3] + gap + up : -Infinity;
    if (S[1] > most) S[1] = most >= least ? most : (most + least) / 2;
  }
  return AT;
}
const wayTri = (x, y, a, k) => { const r = 28 * k;
  return `<g transform="translate(${f1(x)} ${f1(y)}) rotate(${a})"><path d="M${f1(-0.6 * r)} ${f1(-0.8 * r)} L${f1(0.9 * r)} 0 L${f1(-0.6 * r)} ${f1(0.8 * r)} Z" fill="#000000" opacity="0"/></g>`; };
function insideWaysSvg(B, x0, y0, x1, y1) {
  const IW = insideWays(B), k = (y1 - y0) / 700;
  // (where the world's ways toward us are: worldWaysSvg's AT)
  const AT = { S: [0, y1 - 30 * k, 90], SE: [x1 - 60 * k, y1 - 50 * k, 40], SW: [x0 + 60 * k, y1 - 50 * k, 140] };
  const ways = IW.map(w => [w.d, ...AT[w.d], { kind: w.to }, w.label, w.attrs]); wayHomes = {};
  worldWaysAt = [...ways, ...doorWays(B, k)];   // (ticket 525: and the cave doors' arrows; their doorways are the passages' own)
  worldWaysHud();
  if (!IW.length) return "";
  return SEP + part("inside-ways", [B.kind, IW.map(w => w.d + w.attrs).join(), x0, y0, x1, y1], () => ways.map(([d, x, y, a, , , attrs]) =>
    obj(`way ${d}`, Infinity, `<g class="portal way-out" data-door="1" data-way="${d}" ${attrs} pointer-events="none">${wayTri(x, y, a, k)}</g>`)).join(""));
}
function worldWaysSvg(B, x0, y0, x1, y1, g) {
  const W = state.world, cell = W && W.at && W.cells[W.at];
  if (!cell || cell.kind !== B.kind) return insideWaysSvg(B, x0, y0, x1, y1);
  const ways = worldLiveWays(W, W.at);
  const target = d => worldTarget(W, W.at, d);   // (ticket 406: the square beside it, or a square of a world it is linked to)
  const k = (y1 - y0) / 700;
  // (a forest crossing's at its own paths' ends: crossGeom; any other place's at its far ground and sides)
  const G = B.kind === "crossing" ? crossGeom({ x0, x1, y1, g, W: x1 - x0, k, pad: 0 }) : null;
  // (0.0.151: 0.0.149 put the far ways (N, NE, NW) at the place's back limit where that was nearer; on a shallow floor,
  // the witch's woods, or in a small window, they crowded the E and W arrows (arrows208), so they are at the far line
  // again: a tap walks as far as the back limit, and walking on into the edge takes the way, wherever its arrow is)
  const far = G ? G.far + 10 * k : g - 150 * k, mid = G ? G.cy + 14 * k : g + 40 * k, side = G ? 0.6 : 0.55;
  const AT = { N: [0, far - 6 * k, -90], S: [0, y1 - 30 * k, 90], E: [x1 - 45 * k, mid, 0], W: [x0 + 45 * k, mid, 180],
               NE: [x1 * side, far, -40], NW: [x0 * side, far, -140], SE: [x1 - 60 * k, y1 - 50 * k, 40], SW: [x0 + 60 * k, y1 - 50 * k, 140] };
  const homes = Object.fromEntries(Object.entries(AT).map(([d, [x, y]]) => [d, [x, y]]));
  waysSpaced(AT, k);   // (ticket 313)
  wayHomes = homes;   // (ticket 369: where each way stands on the floor, as before the spacing)
  const compass = ways.map(d => { const t = target(d); return [d, ...AT[d], t ? t.cell : null, worldTargetName(t)]; });
  // (ticket 524) an inside in a square of its own keeps its way toward us (insideWays: the living room's door out, a
  // tree room's, a castle room's), where the square has no way that side: still a way back to its outside
  if (INSIDE_KINDS.has(B.kind)) for (const w of insideWays(B)) if (!ways.includes(w.d)) compass.push([w.d, ...AT[w.d], { kind: w.to }, w.label, w.attrs]);
  worldWaysAt = [...compass, ...doorWays(B, k)];   // (ticket 525: and the cave doors' arrows, after the compass's; their doorways are the passages' own)
  worldWaysHud();
  if (!compass.length) return "";
  const sig = compass.map(([d, , , , to, nm, attrs]) => d + (to ? to.kind + ":" + nm : "-") + (attrs || "")).join();
  return SEP + part("world-ways", [W.at, B.kind, sig, x0, y0, x1, y1, g], () => compass.map(([d, x, y, a, to, , attrs]) => {
    const tri = wayTri(x, y, a, k);
    return obj(`way ${d}`, Infinity, to ? `<g class="portal way-out" data-door="1" data-way="${d}"${attrs ? " " + attrs : ""} pointer-events="none">${tri}</g>` : `<g class="way-out nowhere" pointer-events="none">${tri}</g>`);
  }).join(""));
}
// The arrows over the picture: made again when the ways change, placed and
// shown or hidden on every drawing (cheap: a few at most)
let waysShown = null;
function worldWaysHud() {
  const layer = document.getElementById("ways");
  if (!layer) return;
  const sig = worldWaysAt.map(([d, , , , to, nm]) => d + (to ? to.kind + ":" + nm : "-")).join();
  if (sig !== waysShown) {
    waysShown = sig;
    layer.replaceChildren(...worldWaysAt.map(([d, , , a, to, nm]) => {
      const b = document.createElement("button");
      b.className = "way-arrow" + (to ? "" : " nowhere"); b.dataset.way = d; b.type = "button";
      b.style.setProperty("--a", a + "deg");
      const door = worldWaysAt.find(e => e[0] === d)[7];   // (ticket 525: a cave door's arrow: its glyph, its words' side)
      b.setAttribute("aria-label", door ? `Go through the ${door.name}, to ${nm}` : to ? `Go ${d}, to ${nm}` : `The way ${d} leads nowhere yet`);
      const tri = document.createElement("span"); tri.className = "way-tri";
      const words = document.createElement("span"); words.className = "way-words";
      words.append(door ? door.glyph : to ? WORLD_ARROWS[d] : "🚧 " + WORLD_ARROWS[d], document.createElement("br"), to ? nm : "nowhere yet");
      b.append(tri, words);
      b.addEventListener("click", () => { const p = to && bgShown.node && bgShown.node.querySelector(`.portal[data-way="${d}"]`); if (p && !entering) enterScene(p); });
      return b;
    }));
  }
  if (!worldWaysAt.length) return;
  const M = pic.getScreenCTM(), box = layer.getBoundingClientRect(), B = state.background;
  if (!M || !box.width) return;
  // (ticket 217) ♿: every arrow shown (the far ones dimmer, in their own colour), their size, their words' size
  const A = state.access || DEFAULTS.access, sc = (A.arrowSize ?? 100) / 100, all = !!A.allArrows, edge = 22 * sc;
  const vars = [["--arrow-scale", String(sc)], ["--arrow-dim", String((A.arrowDim ?? 45) / 100)], ["--arrow-color", A.arrowColor || "#38bdf8"],
                ["--label-stay", Math.max(100, A.labelStay ?? 500) + "ms"]];   // (ticket 404)
  for (const [k, v] of vars) if (layer.style.getPropertyValue(k) !== v) layer.style.setProperty(k, v);
  const at = [];
  worldWaysAt.forEach(([d, x, y, a, , , , door], i) => {
    const el = layer.children[i];
    if (!el) return;
    const p = new DOMPoint(x, y).matrixTransform(M), near = wayNearness(d) < 1;
    // (kept inside the picture, by its size)
    const left = Math.max(edge, Math.min(box.width - edge, p.x - box.left)), top = Math.max(edge, Math.min(box.height - edge, p.y - box.top));
    // (its words toward the middle: left of an arrow on the right, below one at the top...)
    const side = door ? door.side : /E/.test(d) && !/W/.test(d) ? "left" : /W/.test(d) ? "right" : d === "N" ? "below" : "above";
    if (el.dataset.side !== side) el.dataset.side = side;
    if (el.classList.contains("near") !== near) el.classList.toggle("near", near);
    if (el.classList.contains("always") !== all) el.classList.toggle("always", all);
    at.push({ d, el, x: left, y: top, a, side, shown: all || near || el.classList.contains("aimed"), far: all && !near && !el.classList.contains("aimed") });
  });
  waysApart(at.filter(w => w.shown), box, sc);
  for (const w of at) { const l = w.x.toFixed(0) + "px", t = w.y.toFixed(0) + "px"; if (w.el.style.left !== l) w.el.style.left = l; if (w.el.style.top !== t) w.el.style.top = t;
    if (w.el.classList.contains("terse") !== !!w.terse) w.el.classList.toggle("terse", !!w.terse); }
}
// (ticket 217, Matthew: "toggle to show the planned path each time it's created
// and being executed in game") ♿ the planned path (walkPath, ticket 215) drawn
// over the picture while the walk to a passage lasts: dotted, its corners
// marked, the part walked faded; gone when they arrive
// (ticket 217) ♿ the words' size: a variable on the page, and the class that scales the place's words, only off 100
let gameFontShown = null;
function accessApply() {
  const A = state.access || DEFAULTS.access, al = document.getElementById("access-layer");
  if (al) for (const [k, v] of [["--path-color", A.pathColor || "#fde047"], ["--box-color", A.boxColor || "#ef4444"],
                                 ["--path-width", (A.pathWidth ?? 4) + "px"], ["--box-width", (A.boxWidth ?? 2) + "px"], ["--box-font", (A.boxFont ?? 11) + "px"],   // (ticket 230)
                                 ["--path-alt", A.pathAltColor || "#c084fc"], ["--made-color", A.madeColor || "#ffffff"], ["--made-width", (A.madeWidth ?? 2) + "px"], ["--made-font", (A.madeFont ?? 10) + "px"]])   // (ticket 258)
    if (al.style.getPropertyValue(k) !== v) al.style.setProperty(k, v);
  const gf = (A.gameFont ?? 100) / 100;
  if (gf === gameFontShown) return;
  gameFontShown = gf;
  document.documentElement.style.setProperty("--game-font", String(gf));
  document.body.classList.toggle("game-font", gf !== 1);
}
// (ticket 217, Matthew: "show the collision boxes you are using for collision tests, if you can") ♿ what the
// friend's feet are kept out of, as solidAt tests it: each solid's box on the floor (grown by the margin the feet
// keep, SOLID_MARGIN), named; a castle room's side walls (castleWalls: the half width at each depth); and the back
// limit the feet stop at (figYMin: a back wall's foot, else the frame's). Drawn over the picture, in the picture's
// own units through the screen; only when shown, and the page changed only when they move.
// (0.0.139, Matthew: "some bug with scaling the font or arrow size misplaces them in the scene"; "collision boxes
// should not disappear until we untoggle them") The overlays have a layer of their own (#access-layer): put in the
// arrows' (#ways), whose children are its arrows by index, the boxes put first shifted every arrow onto another's
// place, and each new square's arrows replaced the layer, boxes and all.
let solidsShown = "";
// (ticket 283, Matthew: "The water effect is really good, at the castle moat, and the tree house stream, etc." but nothing showed
// what makes it; "Let's try to represent the internal game logic on screen, and let's give toggles to prevent it from getting
// overwhelming") 💧 where the friend is in water here (WATER: wading to the knees in a stream or a brook, SHALLOW_WATER; in
// to the waist, or swimming, anywhere else): the place's own test sampled in rows across the picture once a drawing of the
// place, the runs in it drawn as bands, in the toggle's colour, with what it means written on it
let waterShown = "", waterHudRuns = { key: "", runs: [] };
function waterHud() {
  const layer = document.getElementById("access-layer");
  if (!layer) return;
  const A = state.access || DEFAULTS.access, B = state.background, at = A.showWater && B.kind !== "none" ? waterHere(B) : null;
  let ov = layer.querySelector("svg.water-hud");
  if (!at) { if (ov) ov.remove(); waterShown = ""; return; }
  const M = pic.getScreenCTM(), box = layer.getBoundingClientRect(), F = sceneFrame;
  if (!M || !box.width) return;
  const key = [bgShown.text && bgShown.text.length, B.kind, F.left, F.top, F.right, F.bottom].join();
  if (waterHudRuns.key !== key) {   // (rows 5 apart, each its runs of points in the water, across the picture)
    const step = 5, runs = [];
    for (let y = F.top; y <= F.bottom; y += step) { let x0 = null;
      for (let x = F.left; x <= F.right + 3; x += 3) { const w = x <= F.right && at(x, y);
        if (w && x0 === null) x0 = x; else if (!w && x0 !== null) { runs.push([x0, y, x - x0]); x0 = null; } } }
    waterHudRuns = { key, runs, step };
  }
  const sig = [key, M.a, M.e, M.f, A.waterColor].join();
  if (sig === waterShown && ov) return;
  waterShown = sig;
  if (!ov) { ov = document.createElementNS("http://www.w3.org/2000/svg", "svg"); ov.setAttribute("class", "water-hud"); layer.append(ov); }
  const s1 = Math.hypot(M.a, M.b), col = A.waterColor || "#38bdf8", sc = (x, y) => { const q = new DOMPoint(x, y).matrixTransform(M); return [q.x - box.left, q.y - box.top]; };
  const bands = waterHudRuns.runs.map(([x, y, w]) => { const [px, py] = sc(x, y); return `<rect x="${px.toFixed(1)}" y="${(py - waterHudRuns.step * s1 / 2).toFixed(1)}" width="${(w * s1).toFixed(1)}" height="${(waterHudRuns.step * s1 + 0.6).toFixed(1)}"/>`; }).join("");
  const top = waterHudRuns.runs[0], [lx, ly] = top ? sc(top[0], top[1]) : [0, 0];
  ov.setAttribute("style", "position:absolute;inset:0;width:100%;height:100%;pointer-events:none;overflow:visible");
  ov.innerHTML = `<g fill="${col}" fill-opacity="0.28">${bands}</g>` + (top ? `<text x="${lx.toFixed(1)}" y="${(ly - 4).toFixed(1)}" font-size="${A.boxFont || 11}" font-weight="bold" fill="${col}" stroke="#000" stroke-width="0.6" paint-order="stroke">💧 ${SHALLOW_WATER[B.kind] ? "water: wading, to the knees" : "water: in to the waist, or swimming where it is deep"}</text>` : "");
}
// (ticket 290) 🫧 the bubble room's bubbles that pop, as circles: each stuck-out one not popped now, dashed, in its colour
let bubbleHitsShown = "";
function bubbleHitsHud() {
  const layer = document.getElementById("access-layer");
  if (!layer) return;
  const A = state.access || DEFAULTS.access, on = !!A.showBubbleHits && state.background.kind === "bubbleroom" && roomBalls && roomBalls.list.length;
  let ov = layer.querySelector("svg.bubble-hits");
  if (!on) { if (ov) ov.remove(); bubbleHitsShown = ""; return; }
  const M = pic.getScreenCTM(), box = layer.getBoundingClientRect();
  if (!M || !box.width) return;
  const sc = (x, y) => { const q = new DOMPoint(x, y).matrixTransform(M); return [q.x - box.left, q.y - box.top]; }, s1 = Math.hypot(M.a, M.b);
  const live = roomBalls.list.filter(b => !ballPops.has(b.id)), sig = [live.length, M.a, M.e, M.f, A.bubbleHitColor].join();
  if (sig === bubbleHitsShown && ov) return;
  bubbleHitsShown = sig;
  if (!ov) { ov = document.createElementNS("http://www.w3.org/2000/svg", "svg"); ov.setAttribute("class", "bubble-hits"); layer.append(ov); }
  ov.innerHTML = live.map(b => { const [x, y] = sc(b.x, b.y); return `<circle cx="${x.toFixed(1)}" cy="${y.toFixed(1)}" r="${(b.r * s1).toFixed(1)}"/>`; }).join("");
  ov.setAttribute("style", `position:absolute;inset:0;width:100%;height:100%;pointer-events:none;overflow:visible;fill:none;stroke:${A.bubbleHitColor || "#38bdf8"};stroke-width:1.5;stroke-dasharray:4 3`);
}
// (ticket 283: "A sub-panel of toggles for the collision boxes, showing their sub-categories") what 🧱 collision boxes draws,
// each kind a chip (state.access.solidCats: on unless switched off); hidden by the stylesheet (svg.solid-boxes[data-hide])
const SOLID_CATS = [["things", "🪨 things in the way"], ["seats", "🪑 seats and beds (sit, lie down)"], ["walls", "🧱 a room's side walls"], ["back", "↔️ the back line"], ["pits", "🕳️ pits (you fall in)"], ["walks", "🌉 bridges (the way along)"], ["you", "🧍 you"]];   // (ticket 299: seats and beds a chip of their own)
const solidCatOn = cat => ((state.access || DEFAULTS.access).solidCats || {})[cat] !== false;
function solidsHud() {
  const layer = document.getElementById("access-layer");
  if (!layer) return;
  const on = !!(state.access || DEFAULTS.access).showSolids && state.background.kind !== "none";
  let ov = layer.querySelector("svg.solid-boxes");
  if (!on) { if (ov) ov.remove(); solidsShown = ""; return; }
  const M = pic.getScreenCTM(), box = layer.getBoundingClientRect();
  if (!M || !box.width) return;
  const F = sceneFrame, g = SCENE.ground, W = castleWalls(state.background), m = 0;   // (the soles have their own size now: a box as it is, no margin)
  const sc = (x, z) => { const q = new DOMPoint(x, z).matrixTransform(M); return [q.x - box.left, q.y - box.top]; };
  const pts = a => a.map(q => q.map(v => v.toFixed(1)).join(",")).join(" ");
  const back = g + figYMin(F), zs = Array.from({ length: 9 }, (_, i) => back + (F.bottom - back) * i / 8);
  // (each its footprint on the floor, and its front face up as tall as the thing, o.h, so it stands over what it is)
  // (ticket 299: a seat or a bed marked, sit or lie, and a flat one, a blanket, in nobody's way, its place to lie shown too)
  const flats = seatsHere(state.background).filter(t => t.flat).map(t => ({ name: t.name, seat: "lie", faces: [], q: [[t.x0, t.y - 10], [t.x1, t.y - 10], [t.x1, t.y + 10], [t.x0, t.y + 10]].map(([x, z]) => sc(x, z)) }));
  // (ticket 337) an oval one: its oval on the floor (24 points round it, starting at its front right, so its name sits by its
  // front as a box's does), and each band its front half standing up from the floor to the band's top, a rim at each height
  const ovalPts = (o, lift = 0, from = 0, to = 2 * Math.PI, n = 24) => { const E = ovalOf(o);
    return Array.from({ length: n + 1 }, (_, i) => { const a = from + (to - from) * i / n; return sc(E.cx + E.rx * Math.cos(a), E.cz + E.rz * Math.sin(a) - lift); }); };
  // (a thing in parts named once, by its top or its frame: not on every leg)
  const named = o => !o.part || !["leg", "post"].includes(o.part);
  // (ticket 413: a thing one can sit on here, at the friend's size, marked as a seat too)
  const sitOnThings = new Set(thingSeats(state.background).map(t => t.name)), seatOf = o => o.seat || (sitOnThings.has(o.name) ? "sit" : undefined);
  const solids = solidsHere().map(o => o.oval ? { name: o.name, seat: seatOf(o), named: named(o), q: ovalPts(o, 0, Math.PI / 4, Math.PI / 4 + 2 * Math.PI),
      faces: solidBands(o).map(b => { const hi = Number.isFinite(b.hi) ? b.hi : 0; return ovalPts(o, b.lo, Math.PI, 0, 12).concat(ovalPts(o, hi, 0, Math.PI, 12)); }) }
    : { name: o.name, seat: seatOf(o), named: named(o), q: [[o.x0 - m, o.back], [o.x1 + m, o.back], [o.x1 + m, o.front], [o.x0 - m, o.front]].map(([x, z]) => sc(x, z)),
    // (0.0.153: a trunk's faces, its bands up from the ground, leaning as a palm's trunk leans)
    faces: solidBands(o).map(b => { const hi = Number.isFinite(b.hi) ? b.hi : 0; return [[b.x0 - m, o.front - b.lo], [b.x1 + m, o.front - b.lo], [b.x1 + m, o.front - hi], [b.x0 - m, o.front - hi]].map(([x, z]) => sc(x, z)); }) });
  const walls = W ? [-1, 1].map(sd => zs.map(z => sc(sd * (W.half(z) - m), z))) : [];
  const line = [sc(F.left, back), sc(F.right, back)];
  // (0.0.157) a trap room's pits: the hole one falls into (PIT_EDGE of it), which a planned walk goes round
  const pitsShown = pitsHere().map(q => { const c0 = sc(q.cx, q.cy), cx = sc(q.cx + PIT_EDGE * q.rx, q.cy), cz = sc(q.cx, q.cy + PIT_EDGE * q.ry);
    return [...[c0[0], c0[1], Math.abs(cx[0] - c0[0]), Math.abs(cz[1] - c0[1])].map(v => +v.toFixed(1)), q.name, q.shape || "circle"]; });   // (ticket 252: by its opening's shape)
  // (ticket 245) the tree house's rope bridges: the way along each, the only way to go on one (no box: no way across it)
  const walks = treeWalks().map(W => ({ name: `bridge ${W.a + 1}–${W.b + 1}`, at: Array.from({ length: 25 }, (_, i) => sc(...walkAt(W, i / 24))) }));
  const sig = JSON.stringify([solids.map(o => pts(o.q) + o.faces.map(pts).join("|")), flats.map(o => pts(o.q)), walls.map(pts), pts(line), pitsShown, walks.map(w => pts(w.at))]);
  if (sig !== solidsShown || !ov) {
    solidsShown = sig;
    if (!ov) { ov = document.createElementNS("http://www.w3.org/2000/svg", "svg"); ov.setAttribute("class", "solid-boxes"); layer.prepend(ov); }
    ov.innerHTML = `<polyline class="back" points="${pts(line)}"/>` + walls.map(w => `<polyline class="wall" points="${pts(w)}"/>`).join("") +
      walks.map(w => `<g data-walk="${xmlEsc(w.name)}"><polyline class="walk" points="${pts(w.at)}"/><polyline class="walk-mid" points="${pts(w.at)}"/></g>` +
        `<text x="${(w.at[12][0] - 30).toFixed(1)}" y="${(w.at[12][1] + 22).toFixed(1)}">${xmlEsc(w.name)}: along it</text>`).join("") +
      pitsShown.map(([x, y, rx, ry, name, sh]) => `<g data-solid="${xmlEsc(name)}"><path class="pit" d="${openingD(sh, x, y, rx, ry)}"/></g><text x="${(x - rx + 4).toFixed(1)}" y="${(y + ry + 13).toFixed(1)}">${xmlEsc(name)}: falls in</text>`).join("") +
      [...solids, ...flats].map(o => `<g data-solid="${xmlEsc(String(o.name))}"${o.seat ? ` class="seat"` : ""}>${o.faces.map(f => `<polygon class="face" points="${pts(f)}"/>`).join("")}<polygon points="${pts(o.q)}"/></g>` + (o.named === false ? "" : `<text x="${o.q[3][0].toFixed(1)}" y="${(o.q[3][1] + 13).toFixed(1)}">${xmlEsc(String(o.name)) + (o.seat ? (o.seat === "lie" ? ": 🛏️ lie down" : ": 🪑 sit") : "")}</text>`)).join("") +
      `<g class="you"><polygon/><polygon/><polygon class="torso"/><polygon class="face torso"/><g class="horse-boxes"></g><text>you</text></g>`;
    solidsHitShown = -1;
  }
  { const hide = SOLID_CATS.map(([c]) => c).filter(c => !solidCatOn(c)).join(" "); if ((ov.dataset.hide || "") !== hide) ov.dataset.hide = hide; }
  // (ticket 229, Matthew: "let's also show the collision box of the player, and let's animate ... the event of a
  // collision, by the two boxes glowing and then fading back to normal colors. Add fade rate") the friend's own box: to
  // the game they are a point at their feet, kept out of each box by the margin, so a box that wide about their feet,
  // as deep as a step; and on a collision (solidHit) it and the box it met (or the walls) glow and fade back, a CSS
  // animation as long as ♿'s glow fade, begun again while they keep pushing
  const Bg = state.background, fx = Bg.figX ?? 0, fz = g + (Bg.figY ?? 0), you = ov.querySelector(".you");
  if (you) { const soles = feetSoles(fx, fz), polys = you.querySelectorAll("polygon"), foot = s => [[s[0], s[2]], [s[1], s[2]], [s[1], s[3]], [s[0], s[3]]].map(([x, z]) => sc(x, z));
    // (Matthew: one under each foot, the size of its sole; and the torso's, as wide as the gap between them, its face
    // standing from the crotch, the hips' joints, up through the head, the crown, as drawn: figureT, faceFrame)
    for (let i = 0; i < 2; i++) polys[i].setAttribute("points", soles[i] ? pts(foot(soles[i])) : "");
    const t = torsoBox(fx, soles), T = figureT, J = poseJoints, up = y => T.dy + T.sg + T.k * (y - T.g);
    // (ticket 286: crawling, the torso's face from under the belly to the top, as it lies: crawlShape's heights)
    const C = crawlShape(), crotch = C && t ? up(T.g) - t[4] : J && J.legs && J.legs.length ? up(Math.max(...J.legs.map(l => l.H[1]))) : fz,
          crown = C && t ? up(T.g) - t[5] : up(faceFrame.ty + headCrown(state.face) * faceFrame.s);
    polys[2].setAttribute("points", t ? pts(foot(t)) : "");
    polys[3].setAttribute("points", t ? pts([[t[0], crotch], [t[1], crotch], [t[1], crown], [t[0], crown]].map(([x, y]) => sc(x, y))) : "");
    // (ticket 492) the horse's: each hoof, and its body (its footprint, and its face from its belly to its back); ridden, the
    // rider's box in the torso's place, from the hips on the saddle to the crown, and no soles of their own
    const HB = horseBoxes(fx, fz), hg = you.querySelector(".horse-boxes");
    if (HB && horseNow.ride) { const r = riderBox(fx, fz); polys[0].setAttribute("points", ""); polys[1].setAttribute("points", "");
      polys[2].setAttribute("points", pts(foot(r))); polys[3].setAttribute("points", pts([[r[0], r[3] - r[4]], [r[1], r[3] - r[4]], [r[1], r[3] - r[5]], [r[0], r[3] - r[5]]].map(([x, y]) => sc(x, y)))); }
    const hk = HB ? JSON.stringify([HB.hooves, HB.body].flat(2).map(v => Math.round(v))) : "";
    if (hg && hg.dataset.k !== hk) { hg.dataset.k = hk; hg.innerHTML = !HB ? "" : HB.hooves.map(h => `<polygon points="${pts(foot(h))}"/>`).join("") +
      `<polygon class="torso" points="${pts(foot(HB.body))}"/><polygon class="face torso" points="${pts([[HB.body[0], HB.body[3] - HB.body[4]], [HB.body[1], HB.body[3] - HB.body[4]], [HB.body[1], HB.body[3] - HB.body[5]], [HB.body[0], HB.body[3] - HB.body[5]]].map(([x, y]) => sc(x, y)))}"/>`; }
    const q0 = sc(Math.min(...soles.map(s => s[0])), Math.max(...soles.map(s => s[3]))); you.lastChild.setAttribute("x", (q0[0] + 2).toFixed(1)); you.lastChild.setAttribute("y", (q0[1] + 12).toFixed(1)); }
  if (!swingHudOn && Bg.kind === "treehouse") { swingHudOn = true; requestAnimationFrame(swingHud); }   // (ticket 240)
  const A = state.access || DEFAULTS.access, fade = Math.max(100, A.glowFade ?? 800) + "ms";
  if (ov.style.getPropertyValue("--glow-fade") !== fade) ov.style.setProperty("--glow-fade", fade);
  if (solidHit.n !== solidsHitShown && solidHit.name && performance.now() - solidHit.t < 1000) {
    solidsHitShown = solidHit.n;
    const els = [you, ...(solidHit.name === "wall" ? ov.querySelectorAll(".wall") : ov.querySelectorAll(`[data-solid="${CSS.escape(xmlEsc(String(solidHit.name)))}"]`))];
    for (const el of els) if (el) { el.classList.remove("hit"); void el.getBoundingClientRect(); el.classList.add("hit"); }
  }
}
let solidsHitShown = -1;
// (ticket 258, Matthew: "under accessibility (using that word as a pun now, and using that panel because it also has
// collision box controls) ... showing all of the 'objects' that are baked into the background options, with their
// bounding box and label. OBJECTS.md content should appear, right? I'm very curious to see how the decomposition into
// re-usable procedures visually represented, even if those objects are not positionable, and are fixed in the scene.
// Let's start with all plants, in all scenes"; "maybe this toggle should really be a toggle that reveals a sub-panel of
// toggles, per category of object") ♿ 🧩 what a place is made of: its objects (its objects list, OBJECTS.md's: each
// obj() and its name), each boxed and named; and its plants, each boxed and named by the procedure that drew it (its
// drawing's [data-proc], drawnBy; a <use> by the drawing it uses), nested as they are drawn (a tree's leaf pads inside
// the tree); each kind a toggle of its own (MADE_CATS, access.madeCats), with how many are here (madeCounts); a legend.
// (ticket 283: "boxes for layering, separate from collision": the objects; ticket 369: this note sat at the end of the
// first line, and swallowed the leafy trees and pines after it, so they were counted but never boxed or named)
const MADE_CATS = [["objects", "🧩 objects (each place's list; their boxes are what sorts them front to back)", true], ["trees", "🌳 leafy trees", true], ["pines", "🌲 pines", true],
                   ["palms", "🌴 palms", true], ["bushes", "🌿 bushes", true], ["flowers", "🌷 flowers", true], ["leafy", "🍀 ferns, vines and leaves", true],
                   ["pads", "🍃 leaf pads", false], ["grass", "🌾 grass", false],
                   ["skulls", "💀 skulls", true], ["fences", "🪵 fences", true]];   // (ticket 368)
// (each procedure's kind)
const PROC_CAT = { leafyTree: "trees", pineTree: "pines", palm: "palms", palmTrunkSvg: "palms", beachPalm: "palms", bushSvg: "bushes", hibiscus: "bushes",
                   tulip: "flowers", daisy: "flowers", paradise: "flowers", fern: "leafy", monstera: "leafy", vine: "leafy", ivy: "leafy", kelp: "leafy",
                   lilyPads: "leafy", padSvg: "pads", padDef: "pads", grassTuft: "grass", deadTree: "trees", skull: "skulls", fence: "fences",   // (ticket 368: the witch's woods' and the placed ones)
                   wildflowers: "flowers" };   // (ticket 503: the pasture's patches of wildflowers; its fence is "fence")
const madeCatOn = cat => { const m = (state.access || DEFAULTS.access).madeCats || {}; return cat in m ? !!m[cat] : !!(MADE_CATS.find(c => c[0] === cat) || [])[2]; };
let madeShown = "", madeCounts = {};
const procHue = name => [...name].reduce((a, ch) => (a * 31 + ch.charCodeAt(0)) % 360, 17);
// (a <use>'s procedure: the one its drawing is, or holds)
const useProc = el => { const id = (el.getAttribute("href") || el.getAttribute("xlink:href") || "").slice(1), d = id && document.getElementById(id);
  if (!d) return null; if (d.dataset.proc) return d.dataset.proc; const q = d.querySelector("[data-proc]"); return q ? q.dataset.proc : null; };
function madeOfHud() {
  const layer = document.getElementById("access-layer");
  if (!layer) return;
  const A = state.access || DEFAULTS.access, node = bgShown.node, on = !!A.showMadeOf && !!node && state.background.kind !== "none";
  let ov = layer.querySelector("svg.made-of"), leg = layer.querySelector(".made-legend");
  if (!on) { if (ov) ov.remove(); if (leg) leg.remove(); madeShown = ""; return; }
  const box = layer.getBoundingClientRect();
  if (!box.width) return;
  const cats = MADE_CATS.map(c => c[0]).filter(madeCatOn), sig = [box.width, box.height, cats.join()].join("|");   // (its colour, line and names' size: CSS, accessApply)
  if (ov && madeShown === sig && ov.dataset.text === String(bgShown.text.length) && madeOfHud.text === bgShown.text) return;
  madeShown = sig; madeOfHud.text = bgShown.text;
  // (a box on the screen, in the layer's own units, cut to it; none when it is not seen)
  const cut = el => { const r = el.getBoundingClientRect(); const x0 = Math.max(0, r.left - box.left), y0 = Math.max(0, r.top - box.top), x1 = Math.min(box.width, r.right - box.left), y1 = Math.min(box.height, r.bottom - box.top);
    return x1 - x0 > 1 && y1 - y0 > 1 ? [x0, y0, x1, y1] : null; };
  const rect = (q, cls, style = "") => `<rect class="${cls}"${style} x="${q[0].toFixed(1)}" y="${q[1].toFixed(1)}" width="${(q[2] - q[0]).toFixed(1)}" height="${(q[3] - q[1]).toFixed(1)}"/>`;
  const label = (q, text, style = "") => q[2] - q[0] >= 36 && q[3] - q[1] >= 14 ? `<text${style} x="${(q[0] + 3).toFixed(1)}" y="${(q[1] + 11).toFixed(1)}">${xmlEsc(text)}</text>` : "";
  let t = "";
  const counts = {}, procs = {};
  // the plants, each by its procedure
  for (const el of node.querySelectorAll("[data-proc], use")) {
    if (el.closest(PROC_HIDDEN)) continue;
    const name = el.tagName.toLowerCase() === "use" ? useProc(el) : el.dataset.proc, cat = name && PROC_CAT[name];
    if (!cat) continue;
    const q = cut(el);
    if (!q) continue;
    counts[cat] = (counts[cat] || 0) + 1; procs[name] = (procs[name] || 0) + 1;
    if (!cats.includes(cat)) continue;
    const st = ` style="--c:hsl(${procHue(name)} 85% 62%)"`;
    t += `<g class="proc">` + rect(q, "", st) + label(q, name, st) + `</g>`;
  }
  // the objects, OBJECTS.md's
  counts.objects = 0;
  for (const o of bgShown.objs) { const q = cut(o.node); if (!q) continue; counts.objects++;
    if (cats.includes("objects")) t += `<g class="obj">` + rect(q, "under") + rect(q, "") + label(q, o.name) + `</g>`; }
  madeCounts = counts;
  if (!ov) { ov = document.createElementNS("http://www.w3.org/2000/svg", "svg"); ov.setAttribute("class", "made-of"); layer.prepend(ov); }
  ov.dataset.text = String(bgShown.text.length);
  ov.innerHTML = t;
  if (!leg) { leg = document.createElement("div"); leg.className = "made-legend"; layer.append(leg); }
  const shown = Object.entries(procs).filter(([nm]) => cats.includes(PROC_CAT[nm])).sort((a, b) => b[1] - a[1]);
  leg.innerHTML = `🧩 ${counts.objects} objects` + (shown.length ? " · drawn by: " + shown.map(([nm, n]) => `<b style="--c:hsl(${procHue(nm)} 85% 62%)">${xmlEsc(nm)}</b> ×${n}`).join(" · ") : "");
  for (const f of madeSyncs) f(); for (const f of hudMadeSyncs) f();   // (the 🔬 chips' counts, and nothing else: ticket 236)
}
const PROC_HIDDEN = "defs, clipPath, mask, pattern, symbol";
// (ticket 240) ♿'s collision boxes at the tree house: every frame while they show, the swing's box turned about its
// hook as the seat hangs now, and the friend's boxes with it while they sit on it (the drawing's swing is a SMIL
// animation, running between drawings: so its own loop, stopping when the boxes, the swing or the place go)
let swingHudOn = false;
function swingHud() {
  const ov = document.querySelector("#access-layer svg.solid-boxes"), S = ov ? swingNow() : null;
  if (!S) { swingHudOn = false; return; }
  const box = document.getElementById("access-layer").getBoundingClientRect(), tf = `rotate(${S.deg.toFixed(2)} ${(S.hook.x - box.left).toFixed(1)} ${(S.hook.y - box.top).toFixed(1)})`;
  const g = ov.querySelector('[data-solid="swing"]'), you = ov.querySelector(".you"), B = state.background, on = !!(state.body.sit && B.seat === "swing");
  if (g && g.getAttribute("transform") !== tf) g.setAttribute("transform", tf);
  if (you) { if (on) { if (you.getAttribute("transform") !== tf) you.setAttribute("transform", tf); } else if (you.hasAttribute("transform")) you.removeAttribute("transform"); }
  requestAnimationFrame(swingHud);
}
// (ticket 221, Matthew: "a new 'create bubbles' action for the player, where
// they can blow bubbles out of their mouth of any style (up to three at once)
// in any scene, or either hand. They can choose which of the three places they
// come from.") While 🫧 is held (the controller's, or B on the keyboard), a
// bubble every BLOW_EVERY ms from the friend's mouth or chosen hand (the
// mouth in the face's units, faceFrame; a hand, poseJoints; both through
// figureT into the picture's units), the themes switched on taking turns
// (bubble space's themes, bubbleSvg); each floats up and drifts, wobbling,
// and is gone at the picture's top or after BLOW_LIFE. Drawn over the
// picture in #access-layer, by blowTick, which runs while any are up.
const BLOW_EVERY = 140, BLOW_LIFE = 7000, BLOW_POP = 250;
let blowPad = false, blowKey = false;
const blowSim = { list: [], raf: 0, last: 0, next: 0, turn: 0, shown: "" };
function blowGo() { if (!blowSim.raf) { blowSim.last = performance.now(); blowSim.raf = requestAnimationFrame(blowTick); } }
function blowFrom() {
  const B = state.body, T = figureT, J = poseJoints, at = (x, y) => [T.dx + T.k * x, T.dy + T.sg + T.k * (y - T.g)];
  if (!J || !faceFrame) return null;
  const dir = facingOf ? facingOf(B) : 0;
  if (B.blowFrom === "left" || B.blowFrom === "right") {
    const hs = J.arms.map(a => at(a.H[0], a.H[1])).sort((p, q) => p[0] - q[0]);
    return hs.length ? (B.blowFrom === "left" ? hs[0] : hs[hs.length - 1]) : null;
  }
  const f = faceFrame; return at(f.tx + (100 + 38 * dir) * f.s, f.ty + 150 * f.s);
}
function blowTick(t) {
  blowSim.raf = 0;
  const layer = document.getElementById("access-layer"), Bg = state.background, F = sceneFrame, k = (F.bottom - F.top) / 700;
  const dt = Math.min(0.05, Math.max(0, (t - blowSim.last) / 1000)); blowSim.last = t;
  const holding = (blowPad || blowKey) && Bg.kind !== "none";
  if (holding && t >= blowSim.next) {
    const p = blowFrom(), themes = ["A", "B", "C"].filter(L => state.body["blow" + L]).map(L => ({ color: Bg["bubble" + L + "Color"], size: Bg["bubble" + L + "Size"] ?? 18,
      shine: (Bg["bubble" + L + "Shine"] ?? 70) / 100, sparkle: Bg["bubble" + L + "Sparkle"] || "none", sparkleColor: Bg["bubble" + L + "SparkleColor"] || "#ffffff" }));
    if (p && themes.length) {
      const th = themes[blowSim.turn++ % themes.length], dir = facingOf ? facingOf(state.body) : 0, fromMouth = state.body.blowFrom !== "left" && state.body.blowFrom !== "right";
      blowSim.list.push({ x: p[0], y: p[1], vx: ((fromMouth ? dir * 60 : 0) + (Math.random() - 0.5) * 50) * k, vy: (-40 - 40 * Math.random()) * k, r: th.size * k * (0.7 + 0.5 * Math.random()), th, born: t, ph: Math.random() * 6 });
      if (blowSim.list.length > 90) blowSim.list.shift();
    }
    blowSim.next = t + BLOW_EVERY;
  }
  // up and drifting, wobbling; gone above the picture or when old
  // (ticket 499: one popped by a fire's heat swells and fades where it was, BLOW_POP ms, as bubble space's do)
  blowSim.list = blowSim.list.filter(b => {
    if (b.pop) return t - b.pop < BLOW_POP;
    b.vy += -8 * k * dt; b.vx *= Math.pow(0.6, dt);
    b.x += (b.vx + Math.sin(t / 400 + b.ph) * 14 * k) * dt; b.y += b.vy * dt;
    return b.y + b.r > F.top && t - b.born < BLOW_LIFE;
  });
  let ov = layer && layer.querySelector("svg.blown");
  if (layer && blowSim.list.length) {
    const M = pic.getScreenCTM(), box = layer.getBoundingClientRect();
    if (M && box.width) {
      if (!ov) { ov = document.createElementNS("http://www.w3.org/2000/svg", "svg"); ov.setAttribute("class", "blown"); layer.append(ov); }
      ov.innerHTML = blowSim.list.map(b => { const q = new DOMPoint(b.x, b.y).matrixTransform(M), age = (t - b.born) / BLOW_LIFE, p = b.pop ? Math.min(1, (t - b.pop) / BLOW_POP) : 0,
        fade = (age > 0.8 ? (1 - age) / 0.2 : 1) * (1 - p);
        return `<g opacity="${Math.max(0, fade).toFixed(2)}">${bubbleSvg(q.x - box.left, q.y - box.top, b.r * M.a * (1 + 0.7 * p), b.th)}</g>`; }).join("");
    }
  } else if (ov) ov.remove();
  if (holding || blowSim.list.length) blowSim.raf = requestAnimationFrame(blowTick);
}
// (ticket 499, Matthew: "optional dragon breath -- need an action for that, like bubbles -- shooting fire out of our mouth.";
// "let's have the dragon fire be the best tool against bubbles -- can you add logic for that -- the heat extend further than
// the fire.") 🔥 Breathing fire, held as 🫧 blowing bubbles is: 🔥 on the controller (the right trigger, button 7; on the
// on-screen controller under start) or F on the keyboard. Anyone can, whatever they wear, in any place. Its fire is one of
// six (🔥 its fire, in 🎮 Controls: FIRE_STYLES, drawn by fireSvg), from the mouth, open while it breathes, or a hand (🫲 our
// left, 🫱 our right, as blown bubbles'), at 📏 its size: side on, it goes the way the friend faces, a little down from the
// mouth; facing us, from the mouth it comes at us, and facing away, it goes away from us, up past their head (each its own
// drawing); from a hand facing us or away, out to that hand's side. It is drawn in a picture of its own over the scene's
// (#fire-over, its viewBox the scene's, as the bubble makers' #pic-over), built once and then only moved: each frame while it
// is held, fireTick puts it where the friend was last drawn (figureT, faceFrame, poseJoints), then pops every bubble whose
// edge comes inside its heat (FIRE_HEAT, about half again as far as its flames: firePops). Let go, it fades and stops, and
// nothing runs for it.
let firePad = false, fireKey = false;
const fireSim = { raf: 0, shown: "", tf: "", out: 0, at: null, until: 0 };
function fireGo() { if (!fireSim.raf) fireSim.raf = requestAnimationFrame(fireTick); }
// (ticket 523, Matthew: "let's also add a 'fire' button within the dragon costume, with chips for testing each type, so the
// person trying on the costume need not switch to game controls to try") In 🐯 Costume's 🐉 the dragon box (the control
// kind "breathe", js/20-controls.js): a 🔥 button breathes fire while it is held (fireBtn: a pointer down on it till it
// lifts or leaves it, or Space or Enter on it; let go too when the window loses the focus or the browser's tab is hidden,
// when the panel's tab changes (the button gone from the page: fireTick), and when the pointer lifts anywhere), a tap
// breathing FIRE_TAP ms at least, so it is seen; and each fire's chip, tapped, breathes that fire for FIRE_TRY ms
// (fireSim.until), long enough for each to show what it does (the brick fire's pieces click on, one after another, and pop
// off; the fire bubbles fly and burst, one every 0.3 s), so the six can be tried one after another without holding anything.
// Its own flag: firePad is the controller's, written every frame one is live, and fireKey is F's, cleared by F's keyup.
// Neither redraws the friend (no changed()); a chip's setting does, as any setting does.
const FIRE_TAP = 250, FIRE_TRY = 2000;
let fireBtn = null;   // (the 🔥 button held: { el, at, id: the pointer's id, or "key" }; null when not)
function fireBtnDown(b, id) { fireBtn = { el: b, at: performance.now(), id }; setPressed(b, true); fireGo(); }
function fireBtnUp() {
  if (!fireBtn) return;
  const { el: b, at } = fireBtn;
  fireBtn = null; setPressed(b, false);
  fireSim.until = Math.max(fireSim.until, at + FIRE_TAP); fireGo();
}
function fireTry() { fireSim.until = performance.now() + FIRE_TRY; fireGo(); }
function fireHoldButton() {
  const b = el("button", { type: "button", class: "fire-hold", "aria-pressed": "false", title: "Hold to breathe fire (as 🔥 on the controller, or F on the keyboard)" }, "🔥 hold to breathe fire");
  b.addEventListener("pointerdown", e => {
    if (e.button > 0) return;
    e.preventDefault();
    // (a touch is held by what it began on till it lifts, so it never leaves it: let go of that, so sliding off lets go)
    try { if (e.target.hasPointerCapture && e.target.hasPointerCapture(e.pointerId)) e.target.releasePointerCapture(e.pointerId); } catch (_) { /* (a synthetic press) */ }
    fireBtnDown(b, e.pointerId);
  });
  b.addEventListener("pointerleave", e => { if (fireBtn && fireBtn.id === e.pointerId) fireBtnUp(); });
  b.addEventListener("keydown", e => { if (e.key !== " " && e.key !== "Enter") return; e.preventDefault(); if (!e.repeat && !fireBtn) fireBtnDown(b, "key"); });
  b.addEventListener("keyup", e => { if ((e.key === " " || e.key === "Enter") && fireBtn && fireBtn.id === "key") fireBtnUp(); });
  b.addEventListener("blur", () => { if (fireBtn && fireBtn.el === b) fireBtnUp(); });
  b.addEventListener("contextmenu", e => e.preventDefault());
  return b;
}
// (the pointer lifted anywhere, heard before anything else: the button may be gone, its tab built again while it was held)
for (const k of ["pointerup", "pointercancel"]) document.addEventListener(k, e => { if (fireBtn && fireBtn.id === e.pointerId) fireBtnUp(); }, true);
window.addEventListener("blur", fireBtnUp);
document.addEventListener("visibilitychange", () => { if (document.hidden) fireBtnUp(); });
const fireSize = B => Math.max(0.3, Math.min(3, (Number(B.fireSize) || 100) / 100));
// where it comes from and which way it goes, in the picture's units: { x, y, k (the friend's scale), view ("side", "front",
// "away"), sx (side on: 1 toward our right, -1 our left), tilt (degrees, down), mouth (the open mouth's size, 0 none) }
function fireAt() {
  const B = state.body, T = figureT, J = poseJoints, f = faceFrame, Fc = state.face;
  if (!J || !f || !(T && T.k > 0)) return null;
  const dir = facingOf(B), L = J.L ?? 150, a = (J.turn || 0) * Math.PI / 180, ca = Math.cos(a), sa = Math.sin(a), lift = J.lift || 0, k = T.k;
  // (the friend's own units into the picture's: lifted onto a saddle, turned about the hips as drawn: crawling, swimming, lying)
  const at = (x, y) => { const yy = y + lift - L, X = x * ca - yy * sa, Y = L + x * sa + yy * ca; return [T.dx + k * X, T.dy + T.sg + k * (Y - T.g)]; };
  if (B.fireFrom === "left" || B.fireFrom === "right") {
    const hs = J.arms.map(q => at(q.H[0], q.H[1])).sort((p, q) => p[0] - q[0]);
    if (!hs.length) return null;
    const p = B.fireFrom === "left" ? hs[0] : hs[hs.length - 1], sx = dir || (B.fireFrom === "left" ? -1 : 1);
    return { x: p[0] + sx * 12 * k, y: p[1], k, view: "side", sx, tilt: 0, mouth: 0 };
  }
  // (the mouth's middle, in the face's units: its line where faceLayout puts it, between its lips as they are)
  const LY = faceLayout(Fc), [up, lo] = lips(Fc.smile, Fc.open), my = LY.mouthY + (75 + (up + lo) / 4 - 150) * LY.my, head = f.s / 0.575;
  const face = (x, y) => at(f.tx + x * f.s, f.ty + y * f.s);
  // (side on, where the lips meet the front of the face; away, the top of the head; toward us, the mouth, turned as the face is)
  const p = dir ? face(100 + dir * (profileEdge(Fc) - 8), my) : B.facing === "back" ? face(100, headCrown(Fc) + 14) : face(100 + turnOf(B) * 14, my);
  return dir ? { x: p[0], y: p[1], k, view: "side", sx: dir, tilt: 6, mouth: head }
    : B.facing === "back" ? { x: p[0], y: p[1], k, view: "away", sx: 1, tilt: 0, mouth: 0 } : { x: p[0], y: p[1], k, view: "front", sx: 1, tilt: 0, mouth: head };
}
// its heat in the picture's units: a cone (from x, y along ux, uy, R long, a its half-angle) or, coming at us, a disc; and
// which way it is seen (view), for the bubble room's depth (firePops)
function fireHeatAt(A, B) {
  const st = fireStyleOf(B), K = A.k * fireSize(B);
  if (A.view === "front") return { x: A.x, y: A.y, disc: FIRE_HEAT.front[st] * K, view: A.view };
  const [R, deg] = FIRE_HEAT[A.view][st], th = A.view === "away" ? -Math.PI / 2 : A.tilt * Math.PI / 180;
  return { x: A.x, y: A.y, ux: A.view === "away" ? 0 : A.sx * Math.cos(th), uy: Math.sin(th), R: R * K, a: deg * Math.PI / 180, view: A.view };
}
// whether a bubble at x, y of radius r has its edge inside the heat (the mock-up's hot(b)): how far along the heat it is if
// so (for a disc, how far from its middle), else -1
function fireHot(H, x, y, r) {
  const dx = x - H.x, dy = y - H.y, dist = Math.hypot(dx, dy);
  if (H.disc) return dist - r <= H.disc ? dist : -1;
  const lx = dx * H.ux + dy * H.uy, ly = dy * H.ux - dx * H.uy;
  if (lx <= 0 || dist - r > H.R) return -1;
  return Math.abs(Math.atan2(ly, lx)) <= H.a + Math.asin(Math.min(1, r / Math.max(dist, r))) ? lx : -1;
}
// every bubble here in the heat pops, each kind as a touch pops it (its swell, and where the code gives it them, its sound
// and smaller bubbles): bubble space's and the smaller ones they throw out (which fly out into the heat and pop in turn, a
// generation or two: BUBBLE_KID_GENS); the bubble room's stuck out of its foam, where the fire goes in the room's depth (below);
// the bubble makers'; blown ones (with bubble space's pop sound); a diving suit's leaks. A few hundred sums a frame at most,
// and only while it is held.
// (the bubble room's, a frame: at most FIRE_BALLS.frame of them, the nearest the mouth first, so its heat sweeps out through
// them, from at most FIRE_BALLS.lots of the room's lots, as each lot popped has its still picture built again (ballApply): a
// breath at 📏 200 had popped up to 390 at once, each with its swelling copy, where the friend's touch pops a few)
const FIRE_BALLS = { frame: 10, lots: 2 };
function firePops(H, t) {
  const B = state.background, F = sceneFrame, k = (F.bottom - F.top) / 700, hot = (x, y, r) => fireHot(H, x, y, r) >= 0;
  if (stillAt !== null || B.kind === "none") return;
  if (B.kind === "bubblespace" && bubbleSim.layer) {
    const rand = bubbleRand();
    bubbleSim.list.forEach((b, i) => { if (!b.pop && !b.back && hot(b.x, b.y, b.r)) bubbleSpacePop(b, i, t, k, rand); });
    for (const c of [...(bubbleSim.kids || [])]) if (!c.pop && hot(c.x, c.y, c.r)) bubbleKidPop(c, t, k, rand);
  }
  if (B.kind === "bubbleroom" && roomBalls) {
    // (in the room's depth, by the floor under each (fzHit) against the friend's feet, give or take the ball's own depth, as
    // the friend's touch: side on, the fire stays at the friend's depth, as deep as the heat is wide there; facing away it
    // goes into the room, up the picture, so all it covers at their depth or behind them; facing us it comes at us, so all
    // it covers at their depth or in front of them. Both had been tested as side on is, the friend's depth give or take the
    // heat's width: facing away almost none of what it covered popped, and facing us the room behind them did)
    const R = roomBalls, feet = figureT.dy + figureT.sg, changed = new Set(), hits = [];
    for (const b of R.list) {
      if (ballPops.has(b.id)) continue;
      const d = fireHot(H, b.x, b.y, b.r);
      if (d < 0) continue;
      const tol = b.dz + 4 * R.k, dz = b.fzHit - feet;
      if (H.view === "away" ? dz > tol : H.view === "front" ? dz < -tol : Math.abs(dz) > tol + d * Math.tan(H.a)) continue;
      hits.push([d, b]);
    }
    hits.sort((p, q) => p[0] - q[0]);
    let n = 0;
    for (const [, b] of hits) {
      if (n >= FIRE_BALLS.frame) break;
      if (changed.size >= FIRE_BALLS.lots && !changed.has(b.obj)) continue;
      ballPop(b, t, changed); n++;
    }
    for (const name of changed) ballApply(name);
  }
  for (const b of makerSim.list) if (!b.pop && hot(b.x, b.y, b.r)) { b.pop = t; if (b.el.firstChild) b.el.firstChild.classList.add("pop"); }
  for (const b of blowSim.list) if (!b.pop && hot(b.x, b.y, b.r)) { b.pop = t; bubblePopSound(b.r / k); }
  for (const b of leakSim.list) if (!b.pop && hot(b.x, b.y, b.r)) { b.pop = t; if (b.el.firstChild) b.el.firstChild.classList.add("pop"); }
}
// the picture it is drawn in: over the scene's (and over the bubble makers'), inside its border, its viewBox the scene's
function fireLayer() {
  let O = document.getElementById("fire-over");
  if (!O) { O = document.createElementNS("http://www.w3.org/2000/svg", "svg"); O.id = "fire-over"; O.setAttribute("aria-hidden", "true"); O.setAttribute("pointer-events", "none");
    (document.getElementById("pic-over") || pic).after(O); }
  const vb = pic.getAttribute("viewBox"); if (vb && O.getAttribute("viewBox") !== vb) O.setAttribute("viewBox", vb);
  return O;
}
function fireTick(t) {
  // (ticket 523: the 🔥 button gone from the page while held, its tab changed or its panel built again, or hidden, the
  // costume changed from the dragon: let go. Before the frame's own request is cleared, so letting go asks for no second one)
  if (fireBtn && (!fireBtn.el.isConnected || fireBtn.el.closest("[hidden]"))) fireBtnUp();
  fireSim.raf = 0;
  const B = state.body, A = (firePad || fireKey || fireBtn || t < fireSim.until) && !figAway && !figBurst ? fireAt() : null, O = document.getElementById("fire-over");
  fireSim.at = A;
  if (A) {
    // (built again only when what it draws changes: its fire, which way it is seen, its heat shown, the mouth's size)
    const L = fireLayer(), st = fireStyleOf(B), heat = B.fireHeat !== false, key = [st, A.view, heat, A.mouth.toFixed(2)].join(" ");
    if (key !== fireSim.shown || !L.querySelector(".fi-fire")) {
      const mouth = fireMouth(A.view, A.mouth);
      L.innerHTML = `<defs>${FIRE_DEFS}</defs><g class="fi-fire" data-fire="${st} ${A.view}"><g class="fi-way">${A.view === "front" ? mouth : ""}<g class="fi-big">${fireSvg(st, A.view, heat)}</g>${A.view === "side" ? mouth : ""}</g></g>`;
      fireSim.shown = key; fireSim.tf = "";
    }
    // (then only moved: where, the friend's scale; the way it goes; its size)
    const g = L.querySelector(".fi-fire"), tf = [`translate(${f1(A.x)} ${f1(A.y)}) scale(${A.k.toFixed(4)})`, `scale(${A.sx} 1) rotate(${A.tilt})`, `scale(${fireSize(B).toFixed(3)})`];
    if (tf.join("|") !== fireSim.tf) { g.setAttribute("transform", tf[0]); g.querySelector(".fi-way").setAttribute("transform", tf[1]); g.querySelector(".fi-big").setAttribute("transform", tf[2]); fireSim.tf = tf.join("|"); }
    if (g.classList.contains("fi-out")) g.classList.remove("fi-out");
    if (L.style.display) L.style.display = "";
    fireSim.out = 0;
    firePops(fireHeatAt(A, B), t);
    fireSim.raf = requestAnimationFrame(fireTick);
    return;
  }
  // (let go, or no friend to breathe it: it fades, then is hidden, its animations stopped, and this rests; but while the 🔥
  // button is still held or a chip's breath still goes on, with no friend to breathe it (spinning through a passage, gone to
  // bubbles under a boulder) it waits for them: nothing else would start it again, as F's key repeats and the controller's
  // every frame do)
  const g = O && O.querySelector(".fi-fire"), wait = () => { if (fireBtn || t < fireSim.until) fireSim.raf = requestAnimationFrame(fireTick); };
  if (!g || O.style.display === "none") { fireSim.out = 0; wait(); return; }
  if (!fireSim.out) { fireSim.out = t; g.classList.add("fi-out"); }
  if (t - fireSim.out < 220) { fireSim.raf = requestAnimationFrame(fireTick); return; }
  O.style.display = "none"; fireSim.out = 0; wait();
}
// (0.0.145, Matthew: "into FS mode, need to reposition the accessibility help, not make it wrong, or make it
// disappear") Over the picture, everything placed through the screen (the nav arrows, the collision boxes, the planned
// path) is placed again whenever the picture changes size (full screen, the divider, a turned phone), once its new
// size is laid out: the arrows had kept the smaller picture's places, all in its top left, till something was drawn.
let pathShown = "";
if (window.ResizeObserver && document.getElementById("pic"))   // (the element: the name pic is declared in a later part)
  // (a resize as the page loads comes before the later parts have run: pitsHere, in 26, was not defined yet, and with ♿'s
  // collision boxes on from a save it threw; nothing to place again till they have)
  new ResizeObserver(() => { if (typeof pitsHere !== "function") return; solidsShown = ""; pathShown = ""; madeShown = ""; worldWaysHud(); solidsHud(); pathHud(); madeOfHud(); }).observe(document.getElementById("pic"));
function pathHud() {
  const layer = document.getElementById("access-layer");
  if (!layer) return;
  const A = state.access || DEFAULTS.access, P = walkPath, live = !!(A.showPath && P && entering && P.pts && P.pts.length > 1);
  let ov = layer.querySelector("svg.walk-path");
  if (!live) { if (ov) ov.remove(); pathShown = ""; return; }
  const M = pic.getScreenCTM(), box = layer.getBoundingClientRect();
  if (!M || !box.width) return;
  const g = SCENE.ground, pts = P.pts.map(([x, y]) => { const q = new DOMPoint(x, g + y).matrixTransform(M); return [q.x - box.left, q.y - box.top]; });
  const sig = pts.map(q => q.map(v => v.toFixed(0)).join(",")).join(" ") + "|" + P.leg;
  if (sig === pathShown && ov) return;
  pathShown = sig;
  if (!ov) { ov = document.createElementNS("http://www.w3.org/2000/svg", "svg"); ov.setAttribute("class", "walk-path"); layer.append(ov); }
  const line = (a, cls) => a.length > 1 ? `<polyline class="${cls}" points="${a.map(q => q.map(v => v.toFixed(1)).join(",")).join(" ")}"/>` : "";
  const leg = Math.min(P.leg ?? 0, pts.length - 1);
  // (ticket 266: a queue of clicks, each click's walk by turns in the path's colour and its alt colour)
  const alt = i => P.segs && P.segs[i] % 2 ? " alt" : "";
  ov.innerHTML = pts.slice(1).map((q, i) => line([pts[i], q], (i < leg ? "walked" : "ahead") + alt(i + 1))).join("") +
    pts.slice(1).map((q, i) => `<circle class="${(i < leg ? "walked" : "ahead") + alt(i + 1)}" cx="${q[0].toFixed(1)}" cy="${q[1].toFixed(1)}" r="${i === pts.length - 2 || (P.segs && P.segs[i + 2] !== P.segs[i + 1]) ? 7 : 5}"/>`).join("");
}
// (ticket 208, Matthew: the arrows shown at once "often overlap each other in
// the corners": E's and SE's, near the bottom right of a wide picture, where
// the floor seen is short) Arrows shown together kept apart: where two would
// overlap (the triangle and its words), one slides along its edge, away from
// the other: an arrow on a side (E, W) up or down, one at the top or bottom
// (N, S) or a corner's (along the floor's front or far edge) across; kept in
// the picture. Measured by arithmetic, each one's words measured once (their
// size is their text's): nothing read back from the page on a drawing. The
// triangle's box is its 44px square turned to its angle (SE's: about 62px
// across), and reaches 5px further along the angle at the top of its bob
// (way-bob); taken as a plain 44px, E still sat over SE by a few px.
const WAY_GAP = 6;
function waysApart(ws, box, sc = 1) {
  if (ws.length < 2) return;
  // (ticket 217: the words measured again when the game's word size changes; the arrows' size, sc, scales all of it)
  const fk = String((state.access || DEFAULTS.access).gameFont ?? 100);
  for (const w of ws) { if (!w.el.__words || w.el.__words[2] !== fk) { const t = w.el.querySelector(".way-words"); w.el.__words = t ? [t.offsetWidth, t.offsetHeight, fk] : [0, 0, fk]; } }
  const rect = w => { const ww = w.terse ? 0 : w.el.__words[0] * sc, wh = w.terse ? 0 : w.el.__words[1] * sc, gap = 26 * sc, c = Math.cos(w.a * Math.PI / 180), s = Math.sin(w.a * Math.PI / 180), h = 22 * sc * (Math.abs(c) + Math.abs(s)), b = 5 * sc;
    const r = [w.x - h + Math.min(0, b * c), w.y - h + Math.min(0, b * s), w.x + h + Math.max(0, b * c), w.y + h + Math.max(0, b * s)];
    const t = w.side === "left" ? [w.x - gap - ww, w.y - wh / 2, w.x - gap, w.y + wh / 2] : w.side === "right" ? [w.x + gap, w.y - wh / 2, w.x + gap + ww, w.y + wh / 2] :
              w.side === "below" ? [w.x - ww / 2, w.y + gap, w.x + ww / 2, w.y + gap + wh] : [w.x - ww / 2, w.y - gap - wh, w.x + ww / 2, w.y - gap];
    return [Math.min(r[0], t[0]), Math.min(r[1], t[1]), Math.max(r[2], t[2]), Math.max(r[3], t[3])]; };
  const across = w => !(w.d === "E" || w.d === "W");   // (E and W slide up or down; the rest across)
  const edge = w => w.d.length === 1;                  // (a side's or the top's or bottom's: free to slide; a corner's stays where it points)
  const overlap = (a, b) => Math.min(a[2], b[2]) - Math.max(a[0], b[0]) > -WAY_GAP && Math.min(a[3], b[3]) - Math.max(a[1], b[1]) > -WAY_GAP;
  // (ticket 227: eight arrows, big, in a small picture do not all fit along its edges, however many passes; still over
  // another after the passes, a far arrow, shown only because ♿ shows every one, drops its words, just its triangle
  // (its words back when the friend comes near), and the passes run again)
  for (const w of ws) w.terse = false;
  // (0.0.159: up to four rounds, not two: still over another after its partner dropped its words, a far arrow drops its own; at twice the size N kept three lines of words over S)
  for (let round = 0; round < 4; round++) {
    if (round) {
      let dropped = false;
      for (let i = 0; i < ws.length; i++) for (let j = i + 1; j < ws.length; j++) if (overlap(rect(ws[i]), rect(ws[j]))) {
        const f = [ws[j], ws[i]].find(w => w.far && !w.terse);
        if (f) { f.terse = true; dropped = true; }
      }
      if (!dropped) break;
    }
  for (let pass = 0; pass < 16; pass++) {   // (ticket 217: eight arrows at twice the size need more than four)
    let moved = false;
    for (let i = 0; i < ws.length; i++) for (let j = i + 1; j < ws.length; j++) {
      const a = rect(ws[i]), b = rect(ws[j]), ox = Math.min(a[2], b[2]) - Math.max(a[0], b[0]), oy = Math.min(a[3], b[3]) - Math.max(a[1], b[1]);
      if (ox <= -WAY_GAP || oy <= -WAY_GAP) continue;
      // (which slides: a side's before a corner's; of two alike, the second; it goes away from the other, or toward the middle)
      for (const [m, o, rm, ro] of edge(ws[j]) || !edge(ws[i]) ? [[ws[j], ws[i], b, a], [ws[i], ws[j], a, b]] : [[ws[i], ws[j], a, b], [ws[j], ws[i], b, a]]) {
        const h = across(m), lo = 22 * sc, hi = (h ? box.width : box.height) - 22 * sc, k = h ? "x" : "y";
        const cm = h ? (rm[0] + rm[2]) / 2 : (rm[1] + rm[3]) / 2, co = h ? (ro[0] + ro[2]) / 2 : (ro[1] + ro[3]) / 2;
        const dir = cm !== co ? Math.sign(cm - co) : Math.sign((h ? box.width : box.height) / 2 - m[k]) || 1;
        const need = (h ? ox : oy) + WAY_GAP, to = Math.max(lo, Math.min(hi, m[k] + dir * need));
        if (Math.abs(to - m[k]) >= 1) { m[k] = to; moved = true; break; }
      }
    }
    if (!moved) break;
  }
  }
  // (ticket 214, the twice-size check's overlaps: W between NW and SW, or N over E at a picture's corner, each pass
  // pushing a side's arrow off one neighbour onto the other, ended over one of them) Last, a side's arrow still over
  // another goes to the nearest place along its edge clear of every other (or, along its edge none, inward from it),
  // seeing all of them at once; a corner's stays where it points.
  const clear = w => ws.every(x => x === w || !overlap(rect(w), rect(x)));
  for (const w of ws) {
    if (!edge(w) || clear(w)) continue;
    for (const k of across(w) ? ["x", "y"] : ["y", "x"]) {
      const at = w[k], size = k === "x" ? box.width : box.height, lo = 22 * sc, hi = size - 22 * sc;
      // (its box's reach along k from where it is: the places just before and just after each other arrow's box)
      const rw = rect(w), i0 = k === "x" ? 0 : 1, before = at - rw[i0], after = rw[i0 + 2] - at;
      const tries = [at, ...ws.filter(x => x !== w).flatMap(x => { const r = rect(x); return [r[i0] - WAY_GAP - after - 1, r[i0 + 2] + WAY_GAP + before + 1]; })]
        .filter(v => v >= lo && v <= hi).sort((a, b) => Math.abs(a - at) - Math.abs(b - at));
      const v = tries.find(v => { w[k] = v; const ok = clear(w); w[k] = at; return ok; });
      if (v !== undefined) { w[k] = v; break; }
    }
  }
}
// (ticket 406, Matthew: "a default world: the current one; a set of world templates, starting with the default world; a set
// of world instances, at first just the default world, called start; any other instance can be given a name, and is
// otherwise 'world N' for a fresh N; each world has its own squares, and worlds connect through special nav arrows (a toggle,
// off by default) that link to any nav arrow as its neighbor, chosen by the designer: which world instance, and where in it
// this square should be imagined as overlaying, as a neighbor: a grid position chosen graphically on a small version of that
// instance's world grid, labelled but with minimal detail") Many worlds. `state.world` is the world the friend is in, as
// always (every designer above works on it unchanged), now with an `id` ("w1" for the first) and a `name` ("start"; empty:
// "world N", N from its id); the others wait in `state.worldsAway`, by id. Going to another world keeps the square left
// (worldKeep), takes its own settings off the place (instanceUnapply), swaps the two, and enters the other at its square.
// A world is made from a template: the first world as the game lays it out, one square of the empty room, or a world kept
// as one (`state.worldTemplates`, by id). A way can be linked to a square of any world (`cell.links[d] = { world, key }`):
// walking out that way leads there, come in from the side facing back, as if that square were the neighbour on this side;
// a link is made both ways (the square there gets the way back, linked here), so there is always a way back.
const WORLD_FIRST = "w1";
function worldsInit(S = state) {
  const W = S && S.world;
  if (!W) return;
  if (!W.id) W.id = WORLD_FIRST;
  if (W.id === WORLD_FIRST && W.name === undefined) W.name = "start";
  if (!S.worldsAway || typeof S.worldsAway !== "object" || Array.isArray(S.worldsAway)) S.worldsAway = {};
  if (!S.worldTemplates || typeof S.worldTemplates !== "object" || Array.isArray(S.worldTemplates)) S.worldTemplates = {};
}
const worldNum = W => +String(W.id || "").replace(/\D/g, "") || 1;
const worldTitle = W => String(W.name || "").trim() || "world " + worldNum(W);
function worldsAll() { worldsInit(); return [state.world, ...Object.values(state.worldsAway)].sort((a, b) => worldNum(a) - worldNum(b)); }
function worldById(id) { worldsInit(); return state.world.id === id ? state.world : state.worldsAway[id] || null; }
// where a way leads: the square of a world it is linked to, or else the square beside it, when that has a way back; null
// when it leads nowhere
function worldTarget(W, key, d) {
  const cell = W && W.cells[key], L = cell && cell.links && cell.links[d];
  if (L) { const T = worldById(L.world), c = T && T.cells[L.key]; return c ? { W: T, key: L.key, cell: c, link: true } : null; }
  const m = worldMeets(W, key, d);
  return m ? { W, key: m, cell: W.cells[m], link: false } : null;
}
// (its words on the arrow: the square's name, and the world's, when it is another)
const worldTargetName = t => !t ? null : t.W !== state.world ? `🌍 ${worldTitle(t.W)}: ${worldName(t.W, t.key)}` : worldName(t.W, t.key);
// to another world, at its square (or where the friend was in it, or its start); from a side when walking in
function worldSwitch(id, key = null, side = null, draw = true) {
  worldsInit();
  const from = state.world;
  if (from.id === id) { if (key && from.cells[key]) worldEnter(key, side, draw); return; }
  const to = state.worldsAway[id];
  if (!to) return;
  worldKeep();
  instanceUnapply(from);
  state.worldsAway[from.id] = from; delete state.worldsAway[id];
  state.world = to;
  const at = key && to.cells[key] ? key : to.cells[to.at] ? to.at : to.start;
  if (to.cells[at]) worldEnter(at, key ? side : null, draw); else if (draw) changed();
}
const WORLD_SMALL = { cols: 3, rows: 3, start: "1,1", cells: { "1,1": { kind: "emptyroom", ways: [] } } };
function worldTemplatesAll() {
  worldsInit();
  return [{ id: "first", name: "🗺️ the first world", world: WORLD_DEFAULT }, { id: "small", name: "🔲 one square: the empty room", world: WORLD_SMALL },
          ...Object.entries(state.worldTemplates).map(([id, t]) => ({ id, name: "📐 " + t.name, world: t.world }))];
}
// (a world's copy for a new world or a template: its squares and things, never its links, which pair with other worlds)
function worldCopy(W) {
  const c = clone(W);
  for (const k of ["id", "name", "applied", "tpl", "at"]) delete c[k];
  for (const cell of Object.values(c.cells || {})) delete cell.links;
  // (ticket 525: its cave doors into another world unlinked; those within it kept, their world "" until the copy has its id: worldNew)
  const doors = T => { for (const t of T || []) if (t.kind === "passage" && t.dest) { if (t.dest.world === W.id || t.dest.world === "") t.dest.world = ""; else { delete t.dest; delete t.twin; } } };
  for (const cell of Object.values(c.cells || {})) doors(cell.things);
  for (const T of Object.values(c.off || {})) doors(T);
  return c;
}
function worldNew(tplId) {
  const all = worldTemplatesAll(), T = all.find(t => t.id === tplId) || all[0], n = 1 + Math.max(0, ...worldsAll().map(worldNum));
  const W = Object.assign(worldCopy(T.world), { id: "w" + n, name: "" });
  for (const T2 of [...Object.values(W.cells || {}).map(c => c.things), ...Object.values(W.off || {})]) for (const t of T2 || []) if (t.kind === "passage" && t.dest && t.dest.world === "") t.dest.world = W.id;   // (ticket 525)
  W.at = W.start;
  state.worldsAway[W.id] = W;
  worldSwitch(W.id);
  return W.id;
}
function worldKeepTemplate() {
  worldsInit(); worldKeep();
  const n = 1 + Math.max(0, ...Object.keys(state.worldTemplates).map(k => +k.replace(/\D/g, "") || 0));
  state.worldTemplates["t" + n] = { name: worldTitle(state.world), world: worldCopy(state.world) };
  changed();
  return "t" + n;
}
// (this world taken away, never the last: the links into it from the others go too)
function worldDrop() {
  const all = worldsAll(), gone = state.world;
  if (all.length < 2) return false;
  const next = all.find(w => w !== gone);
  for (const w of all) for (const c of Object.values(w.cells)) for (const [d, L] of Object.entries(c.links || {})) if (L.world === gone.id) delete c.links[d];
  // (ticket 525: and the cave doors into it, unlinked: they lead by kind again)
  passagesEach(T => { for (const t of T) if (t.kind === "passage" && t.dest && t.dest.world === gone.id) { delete t.dest; delete t.twin; } });
  instanceUnapply(gone);
  delete state.worldsAway[next.id];
  state.world = next;
  const at = next.cells[next.at] ? next.at : next.start;
  if (next.cells[at]) worldEnter(at); else changed();
  return true;
}
// (a square's way on: its ways, or a crossing's path; for the square shown, its place's settings too)
function worldWayOn(W, key, d) {
  const c = W.cells[key];
  if (!c) return;
  if (c.kind === "crossing") { c["cross" + d] = true; if (W === state.world && W.at === key && state.background.kind === "crossing") state.background["cross" + d] = true; }
  else if (!worldWays(c).includes(d)) { const order = Object.keys(WORLD_DIRS); c.ways = order.filter(x => x === d || (c.ways || []).includes(x)); }
}
function worldWayOff(W, key, d) {
  const c = W.cells[key];
  if (!c) return;
  if (c.kind === "crossing") { c["cross" + d] = false; if (W === state.world && W.at === key && state.background.kind === "crossing") state.background["cross" + d] = false; }
  else c.ways = (c.ways || []).filter(x => x !== d);
}
// a link taken off one way, and the link back from where it led; the ways stay where they still meet a square beside them
function worldUnlinkAt(W, key, d) {
  const c = W.cells[key], L = c && c.links && c.links[d];
  if (!L) return;
  delete c.links[d];
  if (!worldMeets(W, key, d)) worldWayOff(W, key, d);
  const T = worldById(L.world), tc = T && T.cells[L.key], back = WORLD_OPP[d], LB = tc && tc.links && tc.links[back];
  if (LB && LB.world === W.id && LB.key === key) { delete tc.links[back]; if (!worldMeets(T, L.key, back)) worldWayOff(T, L.key, back); }
}
// the way d out of the square the friend is in, linked to a square of a world; and that square's way back, linked here
function worldLink(d, worldId, key) {
  worldsInit();
  const W = state.world, here = W.at, cell = W.cells[here], T = worldById(worldId), tc = T && T.cells[key], back = WORLD_OPP[d];
  if (!WORLD_DIRS[d] || !cell || !tc || (T === W && key === here) || worldNoWay(cell.kind, d) || worldNoWay(tc.kind, back)) return false;
  worldUnlinkAt(W, here, d);
  worldUnlinkAt(T, key, back);
  cell.links = { ...(cell.links || {}), [d]: { world: T.id, key } }; worldWayOn(W, here, d);
  tc.links = { ...(tc.links || {}), [back]: { world: W.id, key: here } }; worldWayOn(T, key, back);
  changed();
  return true;
}
// a way out taken: the friend walks to it, then is in the square beside
// it, come in from the side it leads to; the back chip can bring them back
// (stepping through it its own way, facing it: east, to the right; here: from an edge, where they stand)
function worldWalk(door, here = false) {
  const W = state.world, d = door.dataset.way, t = scriptWay(W, d), B = state.background;   // (ticket 531, phase 2: 📜 World's rule, when it is on)
  if (!t) return;
  const from = { kind: B.kind, figX: B.figX ?? 0, figY: B.figY ?? 0, to: t.cell.kind, worldAt: W.at, worldId: W.id };
  tripStart(door, "walk");
  // (ticket 406: through a way linked to another world, into that world, at its square, come in from the side facing back)
  walkInto(door, () => { trips.push(from); if (t.W !== W) worldSwitch(t.W.id, t.key, WORLD_OPP[d], false); else worldEnter(t.key, WORLD_OPP[d], false); }, true, "step", null, { out: WORLD_DIRS[d], here });
}
// (ticket 525, Matthew: "Also need a way to place cave doors --- as objects with nav arrows associated with them."; "world
// design panel I mean.") A 🕳️ cave passage is a cave door: its own name (uid), and, linked (its card's 🧭, in 🗺️ World design or
// 🏞️ Background), where it leads (dest: a square, { world, key, kind }; or a place off the grid, { world, off: its placeKey,
// kind, from: its outside's square, and which one }) and its twin there, a passage of the same kind with its dest here, so
// there is always a way back (the contract: doorways go somewhere sensible). Linking puts the twin in; taking either away,
// or unlinking, takes both. Unlinked, it leads by kind as before (its 🧭 place, back by ⬅️). Either way it has a nav arrow,
// as the compass's ways (doorWays, appended to worldWaysAt): over its opening, near when the feet are near its foot.
const passageUid = () => "p" + Math.floor(Math.random() * 36 ** 6).toString(36);
const passageHome = d => !d ? null : d.key ? "sq:" + d.world + ":" + d.key : "off:" + d.world + ":" + d.off;
// (the list of things a dest's place keeps: the place shown's own, state.background's, when it is that one; else its square's, or W.off's)
function passageThings(d, make = false) {
  if (!d) return null;
  if (passageHome(d) === worldHome) { const B = state.background; return Array.isArray(B.things) ? B.things : make ? (B.things = []) : null; }
  const T = worldById(d.world);
  if (!T) return null;
  if (d.key) { const c = T.cells[d.key]; return !c ? null : Array.isArray(c.things) ? c.things : make ? (c.things = []) : null; }
  if (make && !T.off) T.off = {};
  return T.off && Array.isArray(T.off[d.off]) ? T.off[d.off] : make ? (T.off[d.off] = []) : null;
}
// (where the place shown is, as a dest: its square, or off the grid)
function passageHere() {
  const W = state.world, B = state.background;
  if (!W || !worldHome) return null;
  if (worldHome.startsWith("sq:")) return { world: W.id, key: W.at, kind: B.kind };
  const w = INSIDE_WHICH[B.kind];
  return { world: W.id, off: placeKey(B), kind: B.kind, from: instanceOf(W) ? W.at : null, ...(w ? { [w]: B[w] ?? 1 } : {}) };
}
const passageOf = uid => (state.background.things || []).find(t => t.kind === "passage" && t.uid === uid) || null;
const passageTwin = t => { const T = t && t.dest ? passageThings(t.dest) : null; return T ? T.find(x => x.kind === "passage" && x.uid === t.twin) || null : null; };
// (a dest still there: its world, and its square, holding the kind it had)
function passageDestOk(d) {
  const T = d && worldById(d.world);
  return !!T && (!d.key || !!T.cells[d.key]);
}
// t (a passage in the place shown) linked to dest, and a twin put there leading back; false when dest is here
function passageLink(t, dest) {
  const here = passageHere();
  if (!t || t.kind !== "passage" || !here || !dest || passageHome(dest) === passageHome(here) || !passageDestOk(dest)) return false;
  passageUnlink(t, false);
  const T = passageThings(dest, true);
  if (!T) return false;
  if (!t.uid) t.uid = passageUid();
  // (where the friend comes in there: a step in front of it, at the place's arrival)
  const a = ARRIVE[dest.kind] || [0, 60], tw = { kind: "passage", x: Math.round(a[0]), z: Math.round(SCENE.ground + Math.max(0, a[1] - 30)), seed: 900 + Math.floor(Math.random() * 9000),
    size: 100, mirror: false, style: t.style || "arch", to: here.kind, uid: passageUid(), dest: here, twin: t.uid };
  T.push(tw);
  t.dest = structuredClone(dest); t.twin = tw.uid;
  changed();
  return true;
}
// t's link taken off, and its twin taken away (t stays, leading by kind again)
function passageUnlink(t, draw = true) {
  if (!t || !t.dest) return;
  const T = passageThings(t.dest), i = T ? T.findIndex(x => x.kind === "passage" && x.uid === t.twin) : -1;
  if (i >= 0) T.splice(i, 1);
  delete t.dest; delete t.twin;
  if (draw) changed();
}
// (every list of things of every world, the place shown's own too: fn(list, where), where a dest-like { world, key } or { world, off })
function passagesEach(fn) {
  for (const W of worldsAll()) {
    for (const [key, c] of Object.entries(W.cells || {})) if (Array.isArray(c.things)) fn(c.things, { world: W.id, key });
    for (const [off, T] of Object.entries(W.off || {})) if (Array.isArray(T)) fn(T, { world: W.id, off });
  }
  if (Array.isArray(state.background.things)) fn(state.background.things, null);
}
// (squares renamed in world id: each door's dest naming one moved along, mv(key) its new key; and a door into an off-grid place
// put on the grid, put: { off, key, kind }, now leading to that square)
function passagesRemap(id, mv, put = null) {
  passagesEach(T => { for (const t of T) {
    const d = t.kind === "passage" && t.dest;
    if (!d || d.world !== id) continue;
    if (put && d.off === put.off) { t.dest = { world: id, key: put.key, kind: put.kind }; continue; }
    if (d.key) d.key = mv(d.key);
    if (d.from) d.from = mv(d.from);
  } });
}
// (a square about to hold another place: the doors into it unlinked, their twins there going with it)
function passagesUnlinkInto(id, key) {
  passagesEach(T => { for (const t of T) if (t.kind === "passage" && t.dest && t.dest.world === id && t.dest.key === key) { delete t.dest; delete t.twin; } });
}
// each cave door's nav arrow: [id, x, y, angle, { kind }, its words, "", { glyph, side, name }], its spot on the floor its foot (wayHomes)
const PASSAGE_TOP = { arch: 112, wallhole: 108, floorhole: 15 };
function doorWays(B, k) {
  const out = [];
  for (const t of B.things || []) {
    if (t.kind !== "passage" || !t.uid) continue;
    const d = t.dest, T = d && passageDestOk(d) ? worldById(d.world) : null, st = t.style || "arch";
    const s = depthSize(B, t.z - SCENE.ground) * (t.size ?? 100) / 100, floor = st === "floorhole";
    const name = T && d.key ? worldTargetName({ W: T, key: d.key, cell: T.cells[d.key] }) : placeName(passageTo(t));
    const id = "p-" + t.uid;
    wayHomes[id] = [t.x, t.z];
    out.push([id, t.x, t.z - (PASSAGE_TOP[st] || 112) * s - (floor ? 40 : 30) * k, floor ? 90 : -90, { kind: T && d.key ? T.cells[d.key].kind : passageTo(t) }, name, "",
              { glyph: "🕳️", side: t.x < 0 ? "right" : "left", name: "cave passage" }]);
  }
  return out;
}
// through a linked cave door: walked to, then there, a step in front of its twin; the back chip brings them back
function passageWalk(door, t) {
  const W = state.world, B = state.background, d = t.dest, tw = passageTwin(t), T = d && worldById(d.world);
  if (!tw || !passageDestOk(d)) return false;
  const kind = d.key ? T.cells[d.key].kind : d.kind;
  const from = { kind: B.kind, inside: B.inside, tent: B.tent, castleRoom: B.castleRoom, figX: B.figX ?? 0, figY: B.figY ?? 0, to: kind, worldAt: W.at, worldId: W.id };
  const fx = Math.round(tw.x), fy = Math.round(tw.z - SCENE.ground + 30);
  tripStart(door, "walk");
  walkInto(door, () => {
    trips.push(from);
    const sq = d.key || (d.from && T.cells[d.from] ? d.from : null);
    if (T !== state.world) worldSwitch(T.id, sq, null, false); else if (sq) worldEnter(sq, null, false);
    const b = state.background;
    if (!d.key) { b.kind = d.kind; const w = INSIDE_WHICH[d.kind]; if (w) b[w] = d[w] ?? 1; }
    b.figX = fx; b.figY = fy; arriveSettle = true; arriveSide = null;
  }, true, "step", null, {});
  return true;
}
// and back: the place edited where the friend is, kept in its square
// (from changed() and settle(): cheap, and only for the place in use).
// (0.0.114, Matthew: a potion put in one square was in every square) A
// place's own doorway (a sign, a door, a picture), the back chip and
// Surprise me change the place without the world knowing, and the things
// went along. So here, on every change, the place shown is given its home:
// its square (the friend's; or, come by a place's own doorway, the square
// of that place beside it, else the first), or, off the grid (an inside,
// our imagination), its own list in W.off, by placeKey (a castle room, a
// tree room or a tent by which one too). Come to a home, the things shown
// are its own; staying, they are kept in it. With no square yet (a first
// visit, after starting over, an older save), the place's things as they
// are go into the square found for it.
let worldHome = null;
const placeKey = B => B.kind + (B.kind === "castleroom" ? ":" + castleOf(B).id + ":" + (B.castleRoom ?? 1) : B.kind === "treeroom" ? ":" + (B.inside ?? 1) : B.kind === "tent" ? ":" + (B.tent ?? 1) : "");
// (ticket 524) an inside an outside has several of: only a square holding that one (tree room 3 through house 3's door);
// with none, it is its outside's, off the grid, as before
function worldSquareFor(W, kind, B = null) {
  const fits = k => cellHolds(W.cells[k], B && B.kind === kind ? B : { kind });
  const nb = W.at && W.cells[W.at] ? Object.keys(WORLD_DIRS).map(d => worldNeighbour(W, W.at, d)).find(n => !n.off && fits(n.key)) : null;
  return nb ? nb.key : worldKeys(W).find(fits) || null;
}
function worldKeep() {
  const W = state.world, B = state.background;
  if (!W || !W.cells) return;
  // (ticket 525: a cave passage from before has its name given, so it has its nav arrow too)
  if (Array.isArray(B.things)) for (const t of B.things) if (t.kind === "passage" && !t.uid) t.uid = passageUid();
  let cell = W.at && W.cells[W.at];
  const adopting = !W.at;
  if (B.kind !== "none" && !cellHolds(cell, B)) { const key = worldSquareFor(W, B.kind, B); if (key) { W.at = key; cell = W.cells[key]; } }
  worldsInit();
  const sq = cellHolds(cell, B), home = B.kind === "none" ? null : sq ? "sq:" + W.id + ":" + W.at : "off:" + W.id + ":" + placeKey(B);   // (ticket 406: by world, as two worlds have squares of one name)
  if (adopting) worldHome = home;
  if (home !== worldHome) {   // (just come: the place's own things, never those of the place left)
    worldHome = home;
    if (sq) for (const k of cellKeys(cell.kind)) { const v = structuredClone(cell[k] ?? (k === "things" ? [] : DEFAULTS.background[k])); if (JSON.stringify(v) !== JSON.stringify(B[k])) B[k] = v; }
    else if (home) { const v = structuredClone((W.off || {})[placeKey(B)] || []); if (JSON.stringify(v) !== JSON.stringify(B.things)) B.things = v; }
  } else if (sq) {
    for (const k of cellKeys(cell.kind)) if (JSON.stringify(cell[k]) !== JSON.stringify(B[k])) cell[k] = structuredClone(B[k]);
  } else if (home) {
    const pk = placeKey(B), T = Array.isArray(B.things) ? B.things : [], kept = (W.off || {})[pk] || [];
    if (JSON.stringify(kept) !== JSON.stringify(T)) { W.off = W.off || {}; if (T.length) W.off[pk] = structuredClone(T); else delete W.off[pk]; }
  }
  // (a crossing in its square draws the paths that lead somewhere: worldLiveWays,
  // as a setting of its own, so its cached drawing knows when they change)
  if (sq && cell.kind === "crossing") { const live = worldLiveWays(W, W.at).join(","); if (B.crossLive !== live) B.crossLive = live; }
  else if (B.crossLive !== undefined) delete B.crossLive;
  instanceKeep(W);
}
// (ticket 300, part 4, Matthew: "each background has instances ..., each with its own settings, independent of the other
// instances. In 🏞️ Background, choose whether the change is to the template (each background's default instance) or to a
// specific instance we have named") A square is an instance of its place: its own settings (cell.own) over the place's,
// the template (state.background's own). The square the friend is in (or whose inside they are in: a castle's rooms are
// its castle's) is shown as itself: its own put into state.background, what they replaced kept aside (W.tpl) to put back
// on leaving (W.applied: the square shown so). A change made there (a slider, a choice, 🎲, a lever pulled, 👁️), seen as a
// setting different from the drawing before (instanceLast), goes where 🏞️ Background's "✏️ changes here go to" says
// (instEdit): 📍 only this square, into its own; 🧩 the template (the start), into the place's own, and a setting this
// square had of its own follows the template again. ↺ (instanceReset) puts the square back to the template. What is not
// a setting of the place is never an instance's: where the friend is, their size, what they sit on, the square's things
// and a crossing's paths (cellKeys: the square's already), the music's LFOs, the choice itself.
const INSTANCE_SKIP = new Set(["kind", "figX", "figY", "figSize", "figUnit", "seat", "things", "onBridge", "onPlatform", "inside", "castleRoom", "tent", "crossLive", "lfoFast", "lfoSlow", "lfoWings", "instEdit", "big", "designerView", ...CROSS_KEYS]);
let instanceLast = null, instanceState = null;
const instanceOf = W => { const B = state.background, cell = W && W.at && W.cells[W.at];
  return cell && (cellHolds(cell, B) || (INSIDES[cell.kind] || []).includes(B.kind)) ? W.at : null; };
function instanceUnapply(W) { const B = state.background; for (const [k, v] of Object.entries(W.tpl || {})) { if (v === undefined) delete B[k]; else B[k] = structuredClone(v); } W.tpl = {}; W.applied = null; }
function instanceApply(W, key) {
  const B = state.background, cell = key && W.cells[key];
  W.tpl = {}; W.applied = key || null;
  if (cell) for (const [k, v] of Object.entries(cell.own || {})) { W.tpl[k] = B[k] === undefined ? undefined : structuredClone(B[k]); B[k] = structuredClone(v); }
}
const instanceSnap = () => { const B = state.background, o = {}; for (const k of Object.keys(B)) if (!INSTANCE_SKIP.has(k)) o[k] = JSON.stringify(B[k]); return o; };
function instanceKeep(W) {
  const B = state.background, key = instanceOf(W);
  if (instanceState !== state) { instanceState = state; instanceLast = null; }   // (a save loaded, or started over: what was set is no change)
  if ((W.applied || null) !== key) { instanceUnapply(W); instanceApply(W, key); instanceLast = instanceSnap(); return; }
  const now = instanceSnap(), cell = key && W.cells[key];
  if (instanceLast && cell) for (const k of new Set([...Object.keys(now), ...Object.keys(instanceLast)])) {
    if (now[k] === instanceLast[k]) continue;
    const was = instanceLast[k] === undefined ? undefined : JSON.parse(instanceLast[k]);
    if ((B.instEdit || "template") === "instance") { cell.own = cell.own || {}; W.tpl = W.tpl || {}; if (!(k in W.tpl)) W.tpl[k] = was; cell.own[k] = B[k] === undefined ? null : structuredClone(B[k]); }
    else if (cell.own && k in cell.own) { delete cell.own[k]; if (W.tpl) delete W.tpl[k]; }
  }
  instanceLast = now;
}
// (this square's own settings gone: the template's again)
function instanceReset() { const W = state.world, key = W && W.applied, cell = key && W.cells[key]; if (!cell) return; instanceUnapply(W); cell.own = {}; instanceApply(W, key); instanceLast = instanceSnap(); }
// A place chosen in the Background tab is that place's square: the first
// square holding it (its things come with it); a place the world doesn't
// have keeps its own things off the grid (worldKeep).
function worldChoose(kind) {
  const W = state.world, key = worldKeys(W).find(k => W.cells[k].kind === kind), B = state.background;
  if (key) { W.at = key; for (const k of cellKeys(kind)) B[k] = structuredClone(W.cells[key][k] ?? (k === "things" ? [] : DEFAULTS.background[k])); }
  // (a place off the grid: worldKeep gives it its own things, W.off's; the square the friend was in stays W.at)
}
// ---- the world designer's hands (v2): putting places in squares, moving
// and taking them away, the grid's size, and a drag that works the same
// with a mouse and a finger
// the places a square can hold: every place
// (ticket 300, Matthew: "every background is categorized and available for a world design, even the very abstract ones")
// our imagination, the empty room and the bubble places too; and (ticket 524, Matthew: "we need to make each room, and its
// params, available to world builder and this one is not dragable") the insides, the cave room behind the falls among them
const worldPalette = () => PLACES.map(([k]) => k).filter(k => k !== "none");
// a place's usual ways out, put in a square: as in the first world, else east and west
const worldUsualWays = kind => { const c = Object.values(WORLD_DEFAULT.cells).find(c => c.kind === kind); return c && c.ways ? [...c.ways] : ["E", "W"]; };
// (ticket 232, Matthew: "'all passageways' does not respect the edge of the grid ... and it introduces issues that can
// be avoided") a square's ways off the map's edge taken away: where a place is put, a square moved or the grid made
// smaller, and by 🔗 all passageways; how many went. (A crossing's paths that way are dead ends, not drawn and no
// mistake, part of its own look: they stay.)
function worldTrimEdges(W, keys = worldKeys(W)) {
  let gone = 0;
  for (const key of keys) {
    const c = W.cells[key];
    if (!c || c.kind === "crossing" || !Array.isArray(c.ways)) continue;
    const keep = c.ways.filter(d => !worldOffMap(W, key, d) || (c.links && c.links[d]));   // (ticket 406: a linked way leads off this world on purpose)
    gone += c.ways.length - keep.length; c.ways = keep;
  }
  return gone;
}
// (ticket 524) o: a place off the grid (worldOffGrid), its chip dropped here: that place, with what it has, its things kept off
// the grid (W.off) and the settings of its own its outside's square kept for it (cell.own, those that are this place's only)
function worldPut(key, kind, o = null) {
  const W = state.world, w = INSIDE_WHICH[kind], which = o && o.which || 1;
  const from = o && o.from && W.cells[o.from], fromOwn = from && from.own || {};
  const view = k => Object.assign(Object.create(state.background), fromOwn, { kind: k, ...(w ? { [w]: which } : {}) });
  const pk = o ? placeKey(view(kind)) : null, things = pk && W.off && W.off[pk] ? structuredClone(W.off[pk]) : [];
  if (W.cells[key]) passagesUnlinkInto(W.id, key);
  const own = {};
  if (from) for (const [k, v] of Object.entries(fromOwn)) { const when = SHOWN_WHEN["background." + k]; if (when && when(view(kind)) && !when(view(from.kind))) own[k] = structuredClone(v); }
  W.cells[key] = kind === "crossing" ? { kind, ...Object.fromEntries(CROSS_DIRS.map(([d]) => ["cross" + d, ["N", "E", "S", "W"].includes(d)])), crossFloor: CROSS_FLOORS[0], crossBack: "woods", things }
                                     : { kind, ways: worldUsualWays(kind), things, ...(w ? { [w]: which } : {}), ...(Object.keys(own).length ? { own } : {}) };   // (ticket 524: which one)
  if (pk && W.off) { delete W.off[pk]; passagesRemap(W.id, k => k, { off: pk, key, kind }); }
  worldTrimEdges(W, [key]);   // (ticket 232: its usual ways, but none off the edge where it is put)
  if (!W.start || !W.cells[W.start]) W.start = key;
  if (W.at === key) W.at = null;   // (the square the friend was in holds another place now)
  changed();
}
// a square moved onto another: they swap, or it moves into the empty one; the start and the friend's square go with them
function worldMove(from, to) {
  const W = state.world;
  if (from === to || !W.cells[from]) return;
  const a = W.cells[from], b = W.cells[to], swap = k => k === from ? to : k === to && b ? from : k;
  W.cells[to] = a; if (b) W.cells[from] = b; else delete W.cells[from];
  worldTrimEdges(W, [to, from]);   // (ticket 232: moved to an edge, no way off it)
  W.start = swap(W.start); W.at = swap(W.at);
  passagesRemap(W.id, swap);   // (ticket 525: the doors into them follow)
  changed();
}
// (ticket 300, Matthew: "removing a grid square does not remove its instance from the world: it goes into a list of
// instances 'unreachable in the current world design' (unreachable, for short), from which it can be put back into the
// grid without setting again what makes it differ from the template") a square taken away waits in W.unreachable as it
// was (its place, its ways, its things, its name, a crossing's paths; where it was), the newest last, 24 at most
const WORLD_UNREACHABLE_MAX = 24;
function worldRemove(key) {
  const W = state.world, cell = W.cells[key];
  if (!cell) return;
  W.unreachable = [...(W.unreachable || []), { ...structuredClone(cell), from: key }].slice(-WORLD_UNREACHABLE_MAX);
  delete W.cells[key];
  if (W.at === key) W.at = null;
  if (W.start === key) W.start = worldKeys(W)[0] || null;
  changed();
}
// one put back: into the square given (whatever was there waits in its place), else its own if that is empty, else the
// first empty one; none empty, it stays
function worldRestore(i, key = null) {
  const W = state.world, U = W.unreachable || [], u = U[i];
  if (!u) return;
  if (!key && u.from && !W.cells[u.from] && (([c, r]) => c >= 0 && r >= 0 && c < W.cols && r < W.rows)(u.from.split(",").map(Number))) key = u.from;   // (ticket 320: a left or top edge taken away leaves some off the grid's start)
  if (!key) { outer: for (let r = 0; r < W.rows; r++) for (let c = 0; c < W.cols; c++) if (!W.cells[c + "," + r]) { key = c + "," + r; break outer; } }
  if (!key) return;
  const { from, ...cell } = u, was = W.cells[key];
  W.unreachable = U.filter((_, j) => j !== i).concat(was ? [{ ...structuredClone(was), from: key }] : []);
  W.cells[key] = cell;
  worldTrimEdges(W, [key]);
  if (!W.start || !W.cells[W.start]) W.start = key;
  if (W.at === key) W.at = null;
  changed();
}
// and one forgotten for good
function worldForget(i) { const W = state.world; W.unreachable = (W.unreachable || []).filter((_, j) => j !== i); changed(); }
// the grid one column or row bigger or smaller (up to 12 by 12; smaller only while that column or row is empty)
// (ticket 320, Matthew: "world design +/- row -- toggle for left/top edge, which is not the default") near: at the left and top edges
// instead, every square moving over by the column or row added or taken away (taken away only while that edge's is empty, as at the
// far edges: "I like that we cannot remove a col or row that's non-empty")
let worldNearEdge = false;
function worldResize(dc, dr, near = false) {
  const W = state.world, cols = W.cols + dc, rows = W.rows + dr;
  if (cols < 1 || rows < 1 || cols > 12 || rows > 12) return;
  const at = k => k.split(",").map(Number);
  if (near ? worldKeys(W).some(k => { const [c, r] = at(k); return (dc < 0 && c === 0) || (dr < 0 && r === 0); })
           : worldKeys(W).some(k => { const [c, r] = at(k); return c >= cols || r >= rows; })) return;
  if (near) worldShift(W, dc, dr);
  W.cols = cols; W.rows = rows;
  worldTrimEdges(W);   // (ticket 232: a smaller grid, the squares along its new edge given no way off it)
  changed();
}
// (ticket 320) every square's key moved by (dc, dr), and all that names a square by its key with it: the start, the friend's square,
// the square shown as itself (W.applied, and the place the friend came to: worldHome), where each square waiting to be put back was
function worldShift(W, dc, dr) {
  const mv = k => { if (typeof k !== "string" || !/^-?\d+,-?\d+$/.test(k)) return k; const [c, r] = k.split(",").map(Number); return (c + dc) + "," + (r + dr); };
  W.cells = Object.fromEntries(Object.entries(W.cells).map(([k, v]) => [mv(k), v]));
  W.start = mv(W.start); W.at = mv(W.at); if (W.applied) W.applied = mv(W.applied);
  for (const u of W.unreachable || []) if (u.from) u.from = mv(u.from);
  passagesRemap(W.id, mv);   // (ticket 525: the doors into them follow)
  if (typeof worldHome === "string" && worldHome.startsWith("sq:")) worldHome = "sq:" + mv(worldHome.slice(3));
}
// A press that becomes a drag after a few pixels: a ghost of it follows the
// finger or the mouse, and on letting go, onDrop(the element under it, the
// event); a press that never moved is a tap (onTap, or the element's own
// click). The click after a drag is swallowed.
function pressDrag(el, ghost, { onDrop, onTap = null }) {
  el.style.touchAction = "none";
  el.addEventListener("pointerdown", e => {
    if (e.button !== 0) return;
    const x0 = e.clientX, y0 = e.clientY;
    let g = null;
    const move = ev => {
      if (!g && Math.hypot(ev.clientX - x0, ev.clientY - y0) > 8) { g = document.createElement("div"); g.className = "drag-ghost"; g.textContent = ghost(); document.body.append(g); }
      if (g) { g.style.left = ev.clientX + "px"; g.style.top = ev.clientY + "px"; ev.preventDefault(); }
    };
    const up = ev => {
      window.removeEventListener("pointermove", move); window.removeEventListener("pointerup", up); window.removeEventListener("pointercancel", up);
      if (g) {
        g.remove();
        const stop = c => { c.stopPropagation(); c.preventDefault(); };
        el.addEventListener("click", stop, { capture: true, once: true }); setTimeout(() => el.removeEventListener("click", stop, { capture: true }), 0);
        if (ev.type === "pointerup") onDrop(document.elementFromPoint(ev.clientX, ev.clientY), ev);
      } else if (onTap && ev.type === "pointerup") onTap(ev);
    };
    window.addEventListener("pointermove", move); window.addEventListener("pointerup", up); window.addEventListener("pointercancel", up);
  });
}
// (ticket 439, Matthew: "System for creating people templates, and multiple people. Use the new 'multi-world' system you
// created as the inspiration for its functions -- we want a single .json file for MUDU to have a list of people, some of
// whom are marked as templates (to make the list of those shorter in a large syste) --- but any person can become a
// template by toggling it. As a template, we can create a new instance ('knight 1' versus 'knight 2') and style them
// differently, and place them into the game.") Many people, as worlds are many. The friend in the picture, the one every
// tab dresses and every control moves, is the person `state.person` names ({ id: "p1" for the first, name, template, from:
// the template they were made from }); their look is the state's own parts (PERSON_PARTS), so every tab works on them
// unchanged. The others wait in `state.peopleAway`, by id, each with their look and, put in a place, where ({ world, key,
// x, y }: a square of a world and their spot in it), where they stand, drawn as the friend would be (personFigure). Going
// to another person (personSwitch) leaves the one dressed till now standing where they were, and brings the other into the
// picture at their own place (or, with none, here, the one left stepping aside). Any person can be a template (⭐); a new
// person from one is a copy of their look named after it, "knight 1", "knight 2". All of it is saved in the game's .json.
const PERSON_PARTS = ["face", "hair", "body", "shirt", "hands", "pants", "undies", "shoes", "costume", "rightHand", "rightShoe", "visible"];
const PERSON_FIRST = "p1", PERSON_NAME_MAX = 24, PERSON_ASIDE = 130;
function peopleInit(S = state) {
  if (!S) return;
  if (!S.person || typeof S.person !== "object" || Array.isArray(S.person)) S.person = {};
  if (!S.person.id) S.person.id = PERSON_FIRST;
  if (!S.peopleAway || typeof S.peopleAway !== "object" || Array.isArray(S.peopleAway)) S.peopleAway = {};
}
const personNum = P => +String(P.id || "").replace(/\D/g, "") || 1;
const personTitle = P => String(P.name || "").trim() || (P.id === PERSON_FIRST ? "the friend" : "person " + personNum(P));
function peopleAll() { peopleInit(); return [state.person, ...Object.values(state.peopleAway)].sort((a, b) => personNum(a) - personNum(b)); }
const personLookNow = () => Object.fromEntries(PERSON_PARTS.map(k => [k, clone(state[k])]));
// (a look as kept, made whole: each part over its defaults, so a person kept before a setting existed still draws)
const personLookWhole = look => Object.fromEntries(PERSON_PARTS.map(k => [k, { ...clone(DEFAULTS[k]), ...clone((look || {})[k] || {}) }]));
// where the friend stands now: this world's square and their spot in it; none outside the places
function personHere() {
  worldsInit();
  const BG = state.background, W = state.world;
  return BG.kind === "none" || !W.at || !W.cells[W.at] ? null : { world: W.id, key: W.at, x: BG.figX ?? 0, y: BG.figY ?? 0 };
}
// (a spot beside one, free: the nearest of a step, two, three... to either side (toward the middle first) where no one else
// put in that square stands within most of a step, inside the scene; `skip`, a person not to count)
function personAside(at, skip = null) {
  if (!at) return null;
  const others = Object.values(state.peopleAway).filter(P => P.id !== skip && P.place && P.place.world === at.world && P.place.key === at.key);
  const free = x => Math.abs(x) < SCENE.left * -0.88 && Math.abs(x - at.x) >= 0.7 * PERSON_ASIDE && others.every(P => Math.abs(P.place.x - x) >= 0.7 * PERSON_ASIDE || Math.abs(P.place.y - at.y) > 40);
  const first = at.x > 0 ? -1 : 1;
  for (let i = 1; i <= 5; i++) for (const sd of [first, -first]) { const x = at.x + sd * i * PERSON_ASIDE; if (free(x)) return { ...at, x }; }
  return { ...at, x: at.x + first * PERSON_ASIDE };
}
// the person whose look and place a template or a list row means: the one dressed now, as they are, or one away
const personRec = id => id === state.person.id ? { ...state.person, look: personLookNow(), place: null } : state.peopleAway[id] || null;
function personSwitch(id) {
  peopleInit();
  const from = state.person, to = state.peopleAway[id];
  if (!to || from.id === id) return false;
  const here = personHere();
  // (the one left stays where they stood; when the other has no place of their own, the other comes here, and the one left
  // steps aside)
  state.peopleAway[from.id] = { id: from.id, name: from.name || "", template: !!from.template, from: from.from || null, look: personLookNow(),
                                place: to.place || !here ? here : personAside(here, to.id) };
  delete state.peopleAway[id];
  const look = personLookWhole(to.look);
  for (const k of PERSON_PARTS) state[k] = look[k];
  state.person = { id: to.id, name: to.name || "", template: !!to.template, from: to.from || null };
  const P = to.place;
  if (P && worldById(P.world) && worldById(P.world).cells[P.key]) {
    if (P.world !== state.world.id) worldSwitch(P.world, P.key, null, false); else if (P.key !== state.world.at) worldEnter(P.key, null, false);
    state.background.figX = P.x; state.background.figY = P.y;
  }
  changed();
  return true;
}
// a new person from a template (any person, as they are now): their look, named after them with the next free number
function personNew(tplId) {
  peopleInit();
  const T = personRec(tplId);
  if (!T) return null;
  const all = peopleAll(), n = 1 + Math.max(0, ...all.map(personNum)), base = personTitle(T).replace(/\s+\d+$/, ""), names = new Set(all.map(personTitle));
  let k = 1; while (names.has(`${base} ${k}`)) k++;
  const P = { id: "p" + n, name: `${base} ${k}`.slice(0, PERSON_NAME_MAX), template: false, from: T.id, look: clone(T.look), place: null };
  state.peopleAway[P.id] = P;
  personSwitch(P.id);
  return P.id;
}
// (ticket 459, Matthew: "permit '+ from costume' and show a box -- on top of other panels, not within them? -- with the
// costume chips to choose (or cancel at the bottom) --- experimenting with a new UX, instead of a drop down, to compare") a
// new person in a costume: a fresh friend (the game's own look) wearing it, put on as the 🐯 Costume tab puts it on (set():
// a beard given, a hat's colour, an animal's colours), named after it with the next free number ("knight 1")
function personNewCostume(kind, label) {
  peopleInit();
  const all = peopleAll(), n = 1 + Math.max(0, ...all.map(personNum)), names = new Set(all.map(personTitle));
  const base = String(label || kind).replace(/[^\p{L}\p{N}\s'-]/gu, "").replace(/\s+/g, " ").trim() || kind;
  let k = 1; while (names.has(`${base} ${k}`)) k++;
  const P = { id: "p" + n, name: `${base} ${k}`.slice(0, PERSON_NAME_MAX), template: false, from: null, look: personLookWhole(null), place: null };
  state.peopleAway[P.id] = P;
  personSwitch(P.id);
  set("costume", "kind", kind);
  return P.id;
}
// (the costumes, as the 🐯 Costume tab lists them: [kind, its chip's words], none left out)
const personCostumes = () => ((TABS.find(t => t[1] === "costume") || [, , []])[2].find(r => r[0] === "choice" && r[1] === "kind") || [])[3]?.filter(([v]) => v !== "none") || [];
// The box to choose a costume in: laid over the panel where it is on the screen (not inside it), the costumes' chips, and ✖
// cancel at its bottom; Escape or a tap outside it closes it too
// (ticket 459; generalised in ticket 518) a box of chips laid over the panel: its title, a chip each ([value, label, on]: on,
// the one chosen now, pressed), then ✖ cancel at its bottom; a chip, ✖, Escape or a tap outside closes it
function chipBox(title, aria, chips, onPick, chipClass = "person-pick-chip", data = "kind") {
  document.querySelector(".person-pick")?.remove();
  const close = () => { box.remove(); document.removeEventListener("keydown", esc, true); document.removeEventListener("pointerdown", away, true); };
  const esc = e => { if (e.key === "Escape") { e.stopPropagation(); e.preventDefault(); close(); } };
  const away = e => { if (!box.contains(e.target)) close(); };
  const box = el("div", { class: "person-pick", role: "dialog", "aria-label": aria }, [
    el("div", { class: "cap" }, title),
    el("div", { class: "person-pick-chips" }, chips.map(([v, lab, on]) => el("button", { class: chipClass, ["data-" + data]: v, ...(on === undefined ? {} : { "aria-pressed": String(!!on) }),
                                                                                        onclick: () => { close(); onPick(v, lab); } }, lab))),
    el("div", { class: "person-pick-foot" }, [el("button", { class: "person-pick-cancel", onclick: close }, "✖ cancel")])]);
  const host = document.getElementById("panel"), r = host ? host.getBoundingClientRect() : { left: 8, right: innerWidth - 8, top: 8, bottom: innerHeight - 8 };
  const top = Math.max(8, r.top), bottom = Math.min(innerHeight - 8, Math.max(r.bottom, top + 240)), left = Math.max(8, r.left), right = Math.min(innerWidth - 8, r.right);
  Object.assign(box.style, { left: left + "px", top: top + "px", width: Math.max(220, right - left) + "px", maxHeight: (bottom - top) + "px" });
  document.body.append(box);
  document.addEventListener("keydown", esc, true);
  setTimeout(() => document.addEventListener("pointerdown", away, true), 0);
  return box;
}
function personCostumeBox() {
  return chipBox("➕ a new person in a costume", "a new person in a costume", personCostumes(), (v, lab) => personNewCostume(v, lab));
}
// (ticket 518) what one hand holds (key holdR or holdL), chosen in a chip box: the costume's own, nothing, or each thing
// (the horse's rein only while there is a horse)
function holdChipBox(part, key, label) {
  const now = state[part][key] || "auto", horse = state.body.horse && state.body.horse !== "none";
  return chipBox(label, label, HOLD_CHOICES.filter(([v]) => v !== "rein" || horse).map(([v, lab]) => [v, lab, v === now]), v => set(part, key, v), "person-pick-chip hold-pick-chip", "hold");
}
function personSetTemplate(id, on) { const P = id === state.person.id ? state.person : state.peopleAway[id]; if (P) { P.template = !!on; changed(); } }
function personPlace(id, at) { const P = state.peopleAway[id]; if (P) { P.place = at || null; changed(); } }
// (one away taken out of the game; never the one dressed now)
function personDrop(id) { if (!state.peopleAway[id]) return false; delete state.peopleAway[id]; for (const P of peopleAll()) if (P.from === id) P.from = null; changed(); return true; }
// Where a person stands, said: the world (when another), the square's place and name
function personWhere(P) {
  if (!P.place) return "not in any place";
  const W = worldById(P.place.world), c = W && W.cells[P.place.key];
  const nm = worldName(W, P.place.key);
  return !c ? "🚧 their place is gone" : `📍 ${W !== state.world ? "🌍 " + worldTitle(W) + ": " : ""}${nm}${nm.includes(squareName(P.place.key)) ? "" : " (" + squareName(P.place.key) + ")"}`;
}
// The people put in places, drawn: each one's figure drawn as the friend would be, with no scene, off the screen, once per
// look (peopleDrawMissing, after a drawing that wanted one: never inside one), kept by its look (personDrawn); each one's
// node kept by person (personNodes) and put among the scene's objects by depth, as the friend is (peopleShow).
const personDrawn = new Map(), personNodes = new Map();
let personBusy = false, personWanted = false;
function peopleDrawMissing() {
  personWanted = false;
  if (personBusy) return;
  const want = Object.values(state.peopleAway).filter(P => P.place && !personDrawn.has(JSON.stringify(P.look)));
  if (!want.length) return;
  // (not while the friend is held, climbing, dragged or going in somewhere: the drawing's own state is busy then)
  if (poseHeld || climb || drag || entering) { setTimeout(peopleDrawMissing, 400); return; }
  personBusy = true;
  const real = document.getElementById("pic"), tmp = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  tmp.setAttribute("style", "position:fixed;left:-10000px;top:0;width:400px;height:600px;visibility:hidden;pointer-events:none");
  const keep = Object.fromEntries(PERSON_PARTS.map(k => [k, state[k]])), keepBG = state.background;
  const keepG = { poseJoints, faceFrame, naturalFrame, figureT, figShown, objs: bgShown.objs };
  try {
    real.id = "pic-real"; tmp.id = "pic"; document.body.append(tmp);
    figShown = [];
    for (const P of want) {
      const look = personLookWhole(P.look);
      for (const k of PERSON_PARTS) state[k] = look[k];
      state.body.anim = "none"; state.background = { ...keepBG, kind: "none" };
      drawNow();
      const fig = tmp.querySelector("#figure"), far = tmp.querySelector("#figure-far"), defs = tmp.querySelector("defs");
      if (fig) personDrawn.set(JSON.stringify(P.look), { svg: (defs ? defs.outerHTML : "") + (far ? far.outerHTML : "") + fig.outerHTML, g: naturalFrame.ground });
    }
  } finally {
    for (const k of PERSON_PARTS) state[k] = keep[k];
    state.background = keepBG;
    ({ poseJoints, faceFrame, naturalFrame, figureT, figShown } = keepG); bgShown.objs = keepG.objs;
    tmp.remove(); real.id = "pic";
    personBusy = false;
  }
  if (personDrawn.size > 200) personDrawn.delete(personDrawn.keys().next().value);
  changed();
}
// (each id in a person's figure made their own, as the friend's are kept: pat-shirt, the veils' masks)
function personOwnIds(svg, pre) {
  const ids = new Set([...svg.matchAll(/\sid="([^"]+)"/g)].map(m => m[1]));
  for (const id of ids) svg = svg.split(`id="${id}"`).join(`id="${pre}${id}"`).split(`url(#${id})`).join(`url(#${pre}${id})`).split(`href="#${id}"`).join(`href="#${pre}${id}"`);
  return svg;
}
function peopleShow(pic, scenery, feetZ) {
  for (const n of pic.querySelectorAll("g.person-placed")) n.remove();
  if (personBusy || !scenery) return;
  const BG = state.background, W = state.world;
  const here = Object.values(state.peopleAway).filter(P => P.place && P.place.world === W.id && P.place.key === W.at);
  if (!here.length) return;
  const items = [];
  for (const P of here) {
    const f = personDrawn.get(JSON.stringify(P.look));
    if (!f) { if (!personWanted) { personWanted = true; setTimeout(peopleDrawMissing, 0); } continue; }
    const x = P.place.x, y = P.place.y, z = SCENE.ground + y, k = (BG.figSize ?? 100) / 100 * sceneFig(BG.kind) * depthSize(BG, y);
    const sig = [JSON.stringify(P.look), x, y, k.toFixed(4), f.g].join("|");
    let kept = personNodes.get(P.id);
    if (!kept || kept.sig !== sig) {
      const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
      g.setAttribute("class", "person-placed"); g.setAttribute("data-person", P.id); g.setAttribute("data-obj", "👤 " + personTitle(P));
      g.innerHTML = `<ellipse cx="${f1(x)}" cy="${f1(z)}" rx="${f1(70 * k)}" ry="${f1(12 * k)}" fill="#000" opacity="0.08"/>` +
                    `<g transform="translate(${f1(x)} ${f1(z)}) scale(${k.toFixed(4)}) translate(0 ${f1(-f.g)})">${personOwnIds(f.svg, "pp" + P.id + "-")}</g>`;
      kept = { sig, node: g }; personNodes.set(P.id, kept);
    }
    kept.node.setAttribute("data-obj", "👤 " + personTitle(P));
    items.push({ node: kept.node, z });
  }
  for (const id of [...personNodes.keys()]) if (!state.peopleAway[id]) personNodes.delete(id);
  // (by depth among the scene's objects, as the friend: one farther than them before them, one nearer after them)
  const friend = document.getElementById("figure-far") || document.getElementById("figure"), friendAhead = objectAhead(feetZ);
  for (const { node, z } of items.sort((a, b) => a.z - b.z)) {
    const ahead = objectAhead(z);
    if (z <= feetZ && ahead === friendAhead && friend && friend.parentNode) friend.parentNode.insertBefore(node, friend);
    else if (ahead) ahead.parentNode.insertBefore(node, ahead);
    else pic.append(node);
  }
}
