"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 17 of 26 of the game's script, run in order by index.html.

function waterRow(at, y, step) {
  // (tried every 8 across, and where it goes in or out of the water, the
  // edge found to within one by halving)
  const yc = y + step / 2, segs = [];
  const edge = (a, b, wa) => { for (let i = 0; i < 3; i++) { const m = (a + b) / 2; if (at(m, yc) === wa) a = m; else b = m; } return (a + b) / 2; };
  let from = null, prev = false, px = WATER_X0;
  for (let x = WATER_X0; x <= WATER_X1; x += 8) {
    const wet = at(x, yc);
    if (x === WATER_X0) { if (wet) from = x; }
    else if (wet !== prev) { const e = edge(px, x, prev); if (wet) from = e; else { segs.push([from, e]); from = null; } }
    prev = wet; px = x;
  }
  if (from !== null) segs.push([from, WATER_X1]);
  return segs;
}
function waterRuns(B, bgText, x0, x1, y0, y1, step = 2) {
  const at = waterHere(B), key = B.kind + ":" + step, runs = [];
  if (!at) return runs;
  if (waterRows.key !== key || waterRows.bg !== bgText) waterRows = { key, bg: bgText, rows: new Map() };
  for (let y = Math.floor(y0 / step) * step; y < y1; y += step) {
    let segs = waterRows.rows.get(y);
    if (!segs) waterRows.rows.set(y, segs = waterRow(at, y, step));
    for (const [a, b] of segs) { const l = Math.max(a, x0), r = Math.min(b, x1); if (r > l) runs.push([l, y, r - l]); }
  }
  return runs;
}
// (ticket 50) Where people can sit, place by place: SEATS[kind](c, B), with c
// the stage (stageC) and B the place's settings, gives the place's seats
// (SEATS itself is declared at the top, with SCENE_FIG, as each place's
// lines come before this),
// each { name, x0, x1, y, z, facing }: its name as a child reads it (the
// object's own name in the objects list, where it is one), the span of its
// top across (x0 to x1, stage units), the height of its top, where a
// sitter's hips go (y), the depth of the floor in front of it, where their
// feet go (z, as an object's depth: a sitter is sorted among the objects
// there), and the ways one can face on it, the usual first ("front",
// "left", "right"). Built from the same helpers as the place's drawing, so
// the two can't drift. A place's lines sit next to its scene function.
const placeSeats = B => { try { return (SEATS[B.kind]?.(stageC(), B) ?? []).filter(Boolean); } catch (e) { return []; } };
// (ticket 413, Matthew: "audit of all objects to determine: do they have the approximate size of a seat, sometimes, at any
// scale? (rocks often do); do they have a reasonable top surface for sitting (rocks do, but cauldrons do not, since they would
// burn us...)"; "don't sit on bushes -- they would poke us too much"; "lean towards doing it, unless there's a reason to
// discuss") Things with a top to sit on, by their objects list's name, wherever they stand, a place's own or put there:
// where on their height the seat is (at: 1 its top; a bench's or a chair's seat half way up, under its back), how much of
// their width (span), and the ways to face on it. Not hot (cauldrons, stoves, a campfire), not prickly (bushes), not sharp
// (stalagmites, stakes, coral), not alive, not a grave, nothing that would break or that one would sink into. The audit, every
// object and why: dress-up/SEATS.md.
const SIT_ANY = ["front", "left", "right"];
const SIT_TOPS = new Map([
  ...["rock", "boulder", "stone", "stump", "log", "fallen", "toadstool", "mushroom", "cavemushroom"].map(n => [n, { at: 0.94, span: 0.6, facing: SIT_ANY }]),
  ...["barrel", "barrels", "water barrel", "crate", "box", "chest", "toy chest", "flour sack", "hay heap", "hay cart"].map(n => [n, { at: 0.98, span: 0.8, facing: SIT_ANY }]),
  ...["bench", "chair"].map(n => [n, { at: 0.5, span: 0.8, facing: ["front"] }]),
]);
// (how high and wide a seat is for the friend there, as shares of their height at its depth. The places' own seats, measured so
// (ticket 413): a front step 0.04, rocks 0.06 to 0.09, chairs, logs and mushrooms 0.11 to 0.13, toadstools, a wagon and barrels
// 0.2 to 0.23, couches 0.34 to 0.38, the swing 0.46 and a throne on its dais 0.52; the narrowest, a mushroom, 0.08 across)
const SIT_LOW = 0.05, SIT_HIGH = 0.42, SIT_WIDE = 0.07;
// (the seats such things give here: each from its box as the friend meets it, solidsHere's, so the seat is where the thing
// is; only where its seat is a seat's height and width for the friend at their size, at its depth; none in another place, nor
// for a thing that is a seat of the place's own already, nor with 🧱 placed things out of the way)
function thingSeats(B) {
  if (B !== state.background || B.kind === "none" || !bgShown.node) return [];
  const parts = new Map();
  for (const o of solidsHere()) if (o.standing && !o.small && SIT_TOPS.has(objBase(o.name))) { if (!parts.has(o.name)) parts.set(o.name, []); parts.get(o.name).push(o); }
  if (!parts.size) return [];
  const H0 = (figureT.base || 400) * figScale(B) * sceneFig(B.kind), out = [];
  for (const [name, ps] of parts) {
    const how = SIT_TOPS.get(objBase(name)), x0 = Math.min(...ps.map(p => p.x0)), x1 = Math.max(...ps.map(p => p.x1));
    const z = Math.max(...ps.map(p => p.front)), top = how.at * Math.max(...ps.map(p => p.h)), fh = H0 * depthSize(B, z - SCENE.ground);
    if (!(fh > 0 && top >= SIT_LOW * fh && top <= SIT_HIGH * fh && x1 - x0 >= SIT_WIDE * fh)) continue;
    const m = (x1 - x0) * (1 - how.span) / 2;
    out.push({ name, x0: x0 + m, x1: x1 - m, y: z - top, z: z + 1, facing: how.facing, thing: true });
  }
  return out;
}
const seatsHere = B => placeSeats(B).concat(thingSeats(B));
// the seat the friend sits on now (a seat here of the name kept, while they sit), or null
const seatNow = () => { const B = state.background; if (!state.body.sit || !B.seat || B.kind === "none") return null;   // (lying on a bed, sit too: ticket 299)
  return placeSeats(B).find(t => t.name === B.seat) || thingSeats(B).find(t => t.name === B.seat) || null; };
// (ticket 123) In deep water (not the shallow stream or brook) the friend
// treads water: both arms scull out and in together, the legs kick in turn,
// and they bob a little, twice a stroke; one stroke to LFO B's wave (2 beats
// to start), so it moves with the music. As much as "🏊 treading water"
// says; none while a joint is held. The turns and bends for now (degrees,
// as the pose's, by side), and the bob, or null.
// (ticket 343, Matthew, in the bubble room: "like swimming -- we need special animations: standing on bubbles, with a param for
// how much they are moving under us --- the more they do, the more we 'brace our feet' and 'stick out our arms'; walking on
// bubbles -- with arms out sideways for balance ... more precarious than the usual walk. running and crawling, etc can remain
// the same for now") Where the floor is bubbles (the bubble room's floor foam), by "〰️ how much the bubbles move under you"
// (bubbleFloorWobble, w 0 to 1): standing, a balancing stance, feet apart and knees bent, arms out to the sides (in profile
// one forward and one back, as a tightrope walker's seen from the side), the body swaying about the hips and the arms rocking
// against it; walking, a careful walk, shorter steps on bent knees, the arms out and rocking against each step, the body
// tipping a little each way. An arm's angle here is its angle out from hanging down (outA): 90 - arms - the delta, so the
// friend's own arms slider is taken back out
const bubbleFloor = () => { const W = state.background;
  return W.kind === "bubbleroom" && (W.foamFloor ?? true) && !state.body.sit && !climb && !isJump(state.body.anim) && bubbleWob() > 0; };   // (still bubbles, 0: as on any floor)
const bubbleWob = () => Math.max(0, Math.min(100, Number(state.background.bubbleFloorWobble ?? 60) || 0)) / 100;
const outA = (B, deg) => 90 - (B.arms ?? 0) - deg;
function bubbleStance() {
  if (!bubbleFloor()) return null;
  const B = state.body, w = bubbleWob(), dir = facingOf(B), t = picClock(), sway = Math.sin(2 * Math.PI * t / 2.6), jig = Math.sin(2 * Math.PI * t / 0.9);
  const tip = (2 + 5 * w) * sway + 1.5 * w * jig, rock = 10 * w * sway;
  const knees = { KneeL: 6 + 22 * w, KneeR: 6 + 22 * w }, out = 40 + 40 * w;
  if (!dir) return { ArmL: outA(B, out + rock), ArmR: outA(B, out - rock), ElbowL: 8, ElbowR: 8, LegL: 4 + 10 * w, LegR: 4 + 10 * w, ...knees, rot: tip, grounded: true, balance: w };
  // (in profile: the near arm forward, the far one back, both raised)
  const fwd = 30 + 35 * w, A = sd => sd === dir ? outA(B, fwd + rock) : outA(B, -(fwd - rock));
  return { ArmL: A(1), ArmR: A(-1), ElbowL: 6, ElbowR: 6, LegL: 6 * w, LegR: -6 * w, ...knees, rot: tip, grounded: true, balance: w };
}
function bubbleWalk(u, dir) {
  const B = state.body, w = bubbleWob(), s = Math.sin(2 * Math.PI * u), c = Math.cos(2 * Math.PI * u), both = (k, f) => ({ [k + "L"]: f(1), [k + "R"]: f(-1) });
  const tip = (2 + 4 * w) * s, out = 55 + 25 * w, rock = 12 * w * s;
  if (!dir) return { ...both("Step", sd => (0.16 - 0.07 * w) * Math.max(0, sd * s)), ...both("Arm", sd => outA(B, out + sd * rock)), ...both("Elbow", () => 8),
                     ...both("Knee", () => 8 + 16 * w), lift: (3 - 1.5 * w) * Math.abs(s), frontFeet: 0.16 - 0.06 * w, rot: tip, balance: w };
  // (from the side: the stride shorter, the knee leading as ever (ticket 309), on bent knees; the arms spread, rocking)
  const amp = 24 * (1 - 0.45 * w), ph = sd => 2 * Math.PI * u + (sd > 0 ? 0 : Math.PI), thigh = sd => amp * Math.sin(ph(sd)), shin = sd => amp * Math.sin(ph(sd) - WALK_LAG);
  const fwd = 40 + 30 * w;
  return { ...both("Leg", thigh), ...both("Knee", sd => 4 + 14 * w + Math.max(0, 1.7 * (thigh(sd) - shin(sd)))),
           ...both("Arm", sd => sd === dir ? outA(B, fwd + rock) : outA(B, -(fwd - rock))), ...both("Elbow", () => 6), lift: (3 - 1.5 * w) * Math.abs(c), rot: tip, balance: w };
}
function treadNow() {
  const W = state.background, amt = Math.max(0, Number(W.waterTread ?? 100) || 0) / 100;
  if (!amt || poseHeld || W.kind === "none" || SHALLOW_WATER[W.kind] || !inWater(W, W.figX ?? 0, SCENE.ground + (W.figY ?? 0))) return null;
  return { ...treadStroke(amt, lfoNow("B").phase), water: true };
}
// (the arms angled down and sweeping out and in under the surface, the
// forearms near level: not lifted out of the water)
function treadStroke(amt, u) {
  const s = Math.sin(2 * Math.PI * u), c = Math.cos(2 * Math.PI * u), arm = amt * (6 + 10 * s), elbow = amt * (18 + 18 * c);
  return { ArmL: arm, ArmR: arm, ElbowL: elbow, ElbowR: elbow,
           LegL: amt * (12 + 10 * c), LegR: amt * (12 - 10 * c), KneeL: amt * (45 + 35 * s), KneeR: amt * (45 - 35 * s),
           bob: amt * 4 * Math.cos(4 * Math.PI * u) };
}
// (ticket 132) The friend's animation for now, chosen in the Body tab: its
// turns and bends (degrees, by side: L their left), added to the pose's;
// how far it lifts them off the ground (lift), a turn of the whole friend
// about the hips (rot, crawling), and a ladder's rungs going by (ladder,
// climbing). One stride to LFO B's wave (2 beats to start), as treading,
// so it moves with the music. Without one chosen, deep water still treads.
const ANIMS = [["none", "⏸️ still"], ["walk", "🚶 walking"], ["stairs", "🔼 walking up or down stairs"], ["run", "🏃 running"], ["swim", "🏊 swimming"], ["crawl", "🐛 crawling"], ["climb", "🪜 climbing (with ladder)"], ["climbup", "🧗 climbing"], ["jump", "🐸 short, fast jump"], ["jumphigh", "🦘 high, slow jump"], ["crouch", "🧎 crouching down"]];
// (walking, running and crawling are seen from the side: chosen facing us, the friend turns to our right)
const ANIM_SIDEWAYS = { walk: true, run: true, crawl: true, stairs: true };   // (ticket 308: and on stairs)
// (ticket 316) pulling a lever: when it began, and which hand (1 their left, -1 their right)
let pullAt = 0, pullSide = -1;
// (ticket 309) walking, how far behind the thigh the shin swings, in the stride's turn (radians): the knee leads, the foot follows
const WALK_LAG = 1.1;
// (ticket 287) swimming flat, from the side: how far the friend is turned about the hips, short of lying level, so the
// head rides higher than the feet and the face looks ahead; and the waterline, that share of the body's half width
// above the hips (0: through them; below 0, more of them above the water)
const SWIM_TILT = 76, SWIM_FLOAT = 0, SWIM_REACH = 50;
// (ticket 137) Climbing a ladder that stands in the place ("🧗 climbing"):
// the friend goes to the ladder nearest them, just in front of it at its
// depth, facing it (away from us), and climbs it, CLIMB_SPEED stage units a second at the
// size they are there; at the top they go through it if it is a doorway (a
// dungeon's ladder up, a tree house room's ladder up, a ladder up the tree
// to its house: the house), or stay up there holding on (the hay loft).
// A place's ladders are its drawing's [data-ladder] groups, measured when
// a climb starts (startClimb, after a drawing, never in draw()). climb is
// null (to look for one), { none } (no ladder here, or, put down, none
// within reach: far), or the ladder: { place (the place it is in), x, z
// (its depth), top, door (its doorway's data-to and data-house, found
// again at the top: the scene's text, and so its nodes, may change on the
// way up, as the music starting re-times it), start, done, through }.
// Chosen, or in a new place, they go to the nearest ladder anywhere; put
// down by a drag, only to one within reach (CLIMB_REACH)
// (ticket 145) Climbed by hand, with the controller (climb.manual): at a
// ladder's foot, up gets on it (mountLadder); up and down move them on it
// (climb.got, in the picture's units, as draw() lifts them), and down at
// its foot, or an arrow to the side there, steps off. Their stroke follows
// how far they have climbed (CLIMB_STROKE of their own units a stroke),
// and they hold still while the arrows do.
let climb = null, climbLook = 0, climbAnywhere = true;
// (ticket 187, Matthew: a ladder gone up as a passage climbed in slow motion, 70; ⬆ on the controller climbs at 300, "a fast, pleasing" pace: one pace now)
const CLIMB_HAND = 300, CLIMB_SPEED = CLIMB_HAND, CLIMB_REACH = 140, CLIMB_STROKE = 160;
// (ticket 174) a climbing hand's height above its shoulder, as a share of the
// arm's length: reaching up (HI) and pulled down near the shoulder (LO); and
// the share of a cycle a hand spends holding a rung and pulling (the rest
// it reaches up for the next)
const CLIMB_HAND_HI = 0.92, CLIMB_HAND_LO = 0.25, CLIMB_PULL = 0.62;
const climbPlace = BG => [BG.kind, BG.inside, BG.castleRoom, BG.tent].join(":");
// (ticket 245, Matthew: "some logic for walking across bridges, on the outside of the tree house, with the correct figure
// size, and collision boxes ... that indicate the curved path that's allowed -- it's very linear in this case, and no N/S
// is permitted, except to re-orient the pose") The tree house's rope bridges as walkways, outside: each one's deck from
// house a's end (p, u = 0) to house b's (q, u = 1), sagging through m (treeBridgeCurve), in the picture's units, and s,
// the tree's units' size there. A friend on one (B.onBridge, { j, u }: which, how far along) stands on its deck, as
// tall as BRIDGE_TALL of the tree's units (the houses' doors are 72: they fit), in front of the tree; along it is the
// only way to go, ⬆ and ⬇ only turning them; let go of over one, they are put on it.
// (ticket 352, Matthew: "we crawl when we run out of tree houses, but on the exterior view, we are tall enough to go into
// the doors -- and quite small") Outside, the default friend is BRIDGE_TALL of the tree's units (a house's, times its size),
// and any other in proportion to their own height against the default's, BRIDGE_BASE (in their own units: ground to crown,
// plus the 90 the drawing adds), as inside: so a long-legged friend is taller outside too. 70 (60 till 0.0.302) makes the
// default as large against a cabin's door (72) as against a tree room's door out, inside (0.89 of each), and the same rule,
// doorTooLow's 0.97, says who crawls through either
const BRIDGE_TALL = 70, BRIDGE_BASE = 452, BRIDGE_REACH = 24;   // (452: the default friend's, legs 140)   // (how near a bridge a friend let go of is put on it, in the stage's units)
const BRIDGE_PACE = 0.5;   // (ticket 246: on a bridge, small, they go at half the controller's speed, a small friend's steps)
function treeWalks(B = state.background) {
  if (B.kind !== "treehouse") return [];
  const n = Math.max(1, Math.min(6, B.houses ?? 4)), hz = (B.treeHouseSize ?? 100) / 100, s = (B.treeSize ?? 100) / 100 * (SCENE.bottom - SCENE.top) / 700, g = SCENE.ground;
  const at = ([x, y]) => [x * s, g + y * s];
  return TREE_BRIDGES.filter(([a, b]) => a < n && b < n).slice(0, B.bridges ?? 2).map(([a, b], j) => {
    const { p, m, q } = treeBridgeCurve(a, b, hz);
    return { j, a, b, p: at(p), m: at(m), q: at(q), s, hz };
  });
}
// the point u of the way along a walkway, and which way it runs there (not of unit length)
const walkAt = (W, u) => [0, 1].map(i => (1 - u) * (1 - u) * W.p[i] + 2 * u * (1 - u) * W.m[i] + u * u * W.q[i]);
// (ticket 331, Matthew: "we should animate a part of walking on the bridge ... where we meet the scale of the destination tree
// house before going inside the door") the friend's size along a bridge, from the one house's to the other's
const bridgeScale = (W, u) => houseScale(W.a, W.hz) * (1 - u) + houseScale(W.b, W.hz) * u;
const walkDir = (W, u) => [0, 1].map(i => 2 * (1 - u) * (W.m[i] - W.p[i]) + 2 * u * (W.q[i] - W.m[i]));
// where the friend is on a bridge, if they are on one: its walkway, how far along, and the point there
function bridgeNow() {
  const B = state.background, o = B.onBridge;
  if (!o || B.kind !== "treehouse") return null;
  const W = treeWalks(B)[o.j];
  if (!W) return null;
  const u = clamp(+o.u || 0, 0, 1), [x, y] = walkAt(W, u);
  return { W, j: o.j, u, x, y };
}
// the bridge nearest (x, y), and how far along it, if one is within reach of it
function nearBridge(x, y, reach) {
  let best = null;
  for (const W of treeWalks()) for (let i = 0; i <= 60; i++) {
    const u = i / 60, [px, py] = walkAt(W, u), d = Math.hypot(px - x, py - y);
    if (d <= reach && (!best || d < best.d)) best = { j: W.j, u, d };
  }
  return best;
}
// onto bridge j, u of the way along (the friend's feet there: figX, figY), or off whichever they are on
function bridgeOn(j, u) {
  const B = state.background, W = treeWalks(B)[j];
  if (!W) return;
  u = clamp(u, 0, 1); B.onBridge = { j, u: +u.toFixed(4) };
  const [x, y] = walkAt(W, u); B.figX = +x.toFixed(1); B.figY = +(y - SCENE.ground).toFixed(1);
  if (B.seat) { state.body.sit = false; B.seat = null; }
}
// (off it, at its foot: below where they were, on the ground the tree stands on)
function bridgeOff() {
  const B = state.background;
  if (!B.onBridge) return;
  delete B.onBridge;
  B.figY = 0;
}
// (ticket 269, Matthew: "when I climb to the top of the external tree house ladder, it looks great --- but then I get to
// the top and there's no motion plan of walking to the door, and walking through it (it's fine for it to open magically
// as we get to the top of the stairs), but the animation, again, of going through the doorway, with clipping, is very
// compelling") At a house's ladder's top: on its platform (B.onPlatform: the house, and where along it, in the tree's
// units), the friend the tree's size as on a bridge (BRIDGE_TALL), walked along to the door, then through it, seen only
// inside the doorway as they go (figClipRect), and in the room, fading in.
function platformNow() {
  const B = state.background, o = B.onPlatform;
  if (!o || B.kind !== "treehouse") return null;
  const i = o.house - 1, [hx, hy] = TREE_SLOTS[i] || [], hz = (B.treeHouseSize ?? 100) / 100, s = (B.treeSize ?? 100) / 100 * (SCENE.bottom - SCENE.top) / 700;
  if (hx === undefined) return null;
  return { x: o.x * s, y: SCENE.ground + (hy + (o.back || 0)) * s, s, hz, hx, hy, sc: houseScale(i, hz) };
}
let figClipRect = null;
// (ticket 331: opt.x, from where along the platform, as off a bridge's end, else the ladder's top; opt.then, once in the room)
function platformIn(house, opt = {}) {
  const B = state.background, body = state.body, i = house - 1, [hx] = TREE_SLOTS[i], hz = (B.treeHouseSize ?? 100) / 100;
  const lx = opt.x ?? hx + Math.sign(hx) * 52 * hz, nearDoor = 36 * houseScale(i, hz);
  climb = null; entering = true; delete B.onBridge;
  B.onPlatform = { house, x: lx, back: 0 }; body.anim = "walk"; body.facing = lx > hx ? "left" : "right"; body.turn = "";
  let last = 0, phase = "walk", t0 = 0, opened = false;
  // (ticket 352) too tall for the cabin's door (72 of the house's units at its arch), by doorTooLow's rule (their crown over
  // 0.97 of it): through it crawling, as through a tree room's door inside
  const crownOwn = youHeights(true).crown / Math.max(1e-6, figureT.k), low = crownOwn * figScale(B) * BRIDGE_TALL / BRIDGE_BASE > 0.97 * 72;
  // (ticket 331, Matthew: "whenever we enter a treehouse door, we could animate the door opening, like in the castle, but we
  // don't") its door swung in as they come near it, open by the time they are at it
  const open = () => { if (opened) return; opened = true; const L = bgShown.node && bgShown.node.querySelector(`.portal[data-house="${house}"] [data-cabin-leaf]`); if (L) leafOpen(L); };
  const step = t => {
    const dt = last ? Math.min(0.1, (t - last) / 1000) : 0; last = t;
    const o = B.onPlatform; if (!o || B.kind !== "treehouse") { entering = false; figClipRect = null; return; }
    if (phase === "walk") {
      const d = hx - o.x, sp = 70 * dt;   // (in the tree's units a second)
      if (Math.abs(d) <= sp) { o.x = hx; phase = "in"; t0 = t; body.facing = "back"; if (low) body.anim = "crawl"; open(); }
      else o.x += Math.sign(d) * sp;
      if (Math.abs(d) < nearDoor) open();
    } else {
      // (through the door: back into it, seen only inside its opening, the cabin's door, M-15 0 V-56 Q0 -72 15 -56 V0)
      const P = platformNow(), u = Math.min(1, (t - t0) / 700);
      figClipRect = { x0: (hx - 15 * P.sc) * P.s, x1: (hx + 15 * P.sc) * P.s, y0: SCENE.ground + (P.hy - 66 * P.sc) * P.s, y1: SCENE.ground + P.hy * P.s };
      o.back = -10 * P.sc * u;
      if (u >= 1) {
        figClipRect = null; delete B.onPlatform; entering = false;
        trips.push({ kind: "treehouse", inside: B.inside, tent: B.tent, castleRoom: B.castleRoom, figX: B.figX ?? 0, figY: B.figY ?? 0, to: "treeroom" });
        const [fx, fy] = ARRIVE.treeroom || [0, 0];
        Object.assign(B, { kind: "treeroom", inside: house, figX: fx, figY: fy }); body.anim = "none"; body.facing = "front"; padWant = null;
        changed();
        if (opt.then) opt.then();
        const fade = q => { for (const n of figShown) if (n.style) n.style.opacity = q; };
        const t1 = performance.now(), f = tt => { const v = Math.min(1, (tt - t1) / WALK_IN_MS); if (!entering) fade(v < 1 ? v.toFixed(2) : ""); if (v < 1 && !entering) requestAnimationFrame(f); };
        fade("0"); requestAnimationFrame(f);
        return;
      }
    }
    moved(); requestAnimationFrame(step);
  };
  changed(); requestAnimationFrame(step);
}
// (ticket 246) the end of the bridge they are on that they would go in at: the end they face along it, else the nearer;
// the house there (1-based), and the one at its other end
function bridgeEnd(at = bridgeNow()) {
  if (!at) return null;
  const f = state.body.facing, toRight = at.W.q[0] >= at.W.p[0];
  const end = f === "right" ? (toRight ? 1 : 0) : f === "left" ? (toRight ? 0 : 1) : at.u < 0.5 ? 0 : 1;
  return { j: at.j, end, house: (end ? at.W.b : at.W.a) + 1, other: (end ? at.W.a : at.W.b) + 1 };
}
// along the bridge they are on, ds (in the picture's units) toward the picture's right (or left, less than 0): at its
// ends, they stop
function bridgeStep(ds) {
  const at = bridgeNow();
  if (!at) return false;
  const { W } = at, toRight = W.q[0] >= W.p[0] ? 1 : -1;
  let u = at.u;
  // (a few small steps along the curve: its length per u changes along it)
  for (let i = 0; i < 4; i++) { const [dx, dy] = walkDir(W, u); u = clamp(u + toRight * ds / 4 / Math.max(1e-6, Math.hypot(dx, dy)), 0, 1); }
  bridgeOn(at.j, u);
  return true;
}
// (a place left, and come to again later, starts off the bridge: drawn anywhere but outside the tree house, the bridge
// they were on is let go of; ticket 246: not by the world's square, which coming out of a room can set)
function bridgeKeep() {
  const B = state.background;
  if (B.onBridge && B.kind !== "treehouse") delete B.onBridge;
  if (B.onPlatform && (B.kind !== "treehouse" || !entering)) delete B.onPlatform;   // (ticket 269: only on the way in)
}
// (a [data-ladder] group, in the picture's own units, the friend's: through
// the screen, as the scene may sit in a nested svg of its own; its depth is
// the object it is in, else its foot; and the doorway it is, if it is one)
function ladderAt(el, Pinv) {
  const b = el.getBBox(), E = el.getScreenCTM(), m = Pinv && E ? Pinv.multiply(E) : null;
  if (!m || !b.height) return null;
  const at = (x, y) => new DOMPoint(x, y).matrixTransform(m), lo = at(b.x + b.width / 2, b.y + b.height), hi = at(b.x + b.width / 2, b.y);
  // (ticket 238: a hatch's ladder, its foot at the hole's near edge, data-down, whatever depth its object is drawn at)
  const ob = el.closest("[data-obj]"), z = el.dataset.down !== undefined ? +el.dataset.down : ob && Number.isFinite(Number(ob.dataset.z)) ? Number(ob.dataset.z) : lo.y;
  const p = el.closest(".portal"), house = el.dataset.house;
  const door = p ? { to: p.dataset.to, house: p.dataset.house, room: p.dataset.room, tent: p.dataset.tent } : house ? { to: "treeroom", house } : null;
  // (ticket 252: a ladder up through a ceiling's opening, data-up: the opening's line, above which the climber is cut off)
  return { x: lo.x, z, top: Math.min(lo.y, hi.y), door, hands: el.dataset.top === "hands", down: el.dataset.down !== undefined ? +el.dataset.down : null,
           up: el.dataset.up !== undefined ? +el.dataset.up : null };
}
function startClimb() {
  climbLook = 0;
  const B = state.body, BG = state.background, node = bgShown.node;
  if (B.anim !== "climbup" || drag || entering || BG.kind === "none" || !node || climb || bridgeNow()) return;   // (ticket 245: none on a bridge)
  const fx = BG.figX ?? 0, fz = SCENE.ground + (BG.figY ?? 0), P = pic.getScreenCTM(), Pinv = P ? P.inverse() : null;
  let best = null;
  for (const el of node.querySelectorAll("[data-ladder]")) {
    const L = ladderAt(el, Pinv);
    if (!L) continue;
    const d = Math.hypot(L.x - fx, L.z - fz);
    if (!best || d < best.d) best = { ...L, d };
  }
  const anywhere = climbAnywhere;
  climbAnywhere = true;
  if (!best || (!anywhere && best.d > CLIMB_REACH)) { climb = { none: true, far: !!best, place: climbPlace(BG) }; changed(); return; }
  BG.figX = Math.round(best.x); BG.figY = Math.round(best.z - SCENE.ground + 2);
  B.facing = "back";   // (facing the ladder, away from us: ticket 138)
  climb = { place: climbPlace(BG), x: best.x, z: best.z + 2, top: best.top, clipUp: best.up ?? null, door: best.door, hands: best.hands, start: performance.now(), done: false, through: false };
  changed();
}
// (ticket 145) A place's ladders and its pictures (and windows: a doorway
// with a view of its own, a nested svg), in the picture's units, measured
// after its drawing is mounted (never in draw(): it reads layout) and again
// whenever the scene's text changes (ticket 187: with any input, not only a
// controller: every passage glows when near). marks.pics' box is the doorway's own
// bounds in its own units, for its glow.
let marks = { text: null, ladders: [], pics: [] }, marksLook = 0;
function measureMarks() {
  marksLook = 0;
  const node = bgShown.node, P = pic.getScreenCTM();
  if (!node || !P) return;
  // (ticket 187, Matthew 2026-10-17: "no more pictures vs passages:
  // pictures are passages too") Every passage, of every kind, in one list
  // (marks.pics, its old name): a picture or window, a door, a doorway or a
  // sign with no mark, a ▼, a pit, a nav arrow's doorway, a ladder that is a
  // way (a tree house's, the dungeon's trapdoor: the ladder stands for the
  // doorway it leads to). Each with its kind, its box for the glow, and
  // where the feet go to use it (spot, on the floor: under a door or a sign,
  // a ladder's foot, a pit's middle, a nav arrow's place); a picture is
  // reached by the chest, as before (passesNear).
  const Pinv = P.inverse(), ladders = [], pics = [], F = sceneFrame;
  const stage = r => { const a = new DOMPoint(r.x, r.y).matrixTransform(Pinv), b = new DOMPoint(r.x + r.width, r.y + r.height).matrixTransform(Pinv);
    return { x0: Math.min(a.x, b.x), y0: Math.min(a.y, b.y), x1: Math.max(a.x, b.x), y1: Math.max(a.y, b.y) }; };
  const floorTop = SCENE.ground + figYMin(F), floorBot = F.bottom, under = q => [(q.x0 + q.x1) / 2, clamp(q.y1, floorTop, floorBot)];
  const bbox = el => { const b = el.getBBox(); return { x: b.x, y: b.y, width: b.width, height: b.height }; };
  const laddered = new Set();   // (doorways a ladder leads into: the ladder is their passage)
  for (const el of node.querySelectorAll("[data-ladder]")) {
    const L = ladderAt(el, Pinv);
    if (!L) continue;
    ladders.push({ ...L, el });   // (ticket 216: its element, for ⬇ down a hatch: climbLadder, as X)
    // (ticket 173: a ladder that is a way somewhere glows, as pictures and doors do, and X climbs it)
    if (!L.door) continue;
    const r = el.getBoundingClientRect();
    if (!r.width || !r.height) continue;
    const pd = el.closest(".portal");
    if (pd) laddered.add(pd);
    else if (el.dataset.house) for (const q of node.querySelectorAll(`.portal[data-house="${CSS.escape(el.dataset.house)}"]`)) laddered.add(q);
    pics.push({ ...stage(r), el, box: bbox(el), ladder: true, kind: "ladder", spot: [L.x, L.z] });
  }
  for (const d of node.querySelectorAll(".portal")) {
    const dd = d.dataset;
    if (laddered.has(d) || !(dd.to || dd.way || dd.pit)) continue;   // (a ladder's doorway; or one that leads nowhere)
    const view = d.querySelector(":scope > svg, :scope > image");
    if (dd.pit) {   // (ticket 162: walked onto, it falls; through it, it is walked to)
      const r = d.getBoundingClientRect(), [cx, cy] = dd.pit.split(" ").map(Number);
      // (ticket 267, Matthew: "let's limit the pit click hover-over box to have two separate boxes: pit itself (maybe even
      // an oval for it) sign above it. the combination is so large, I can't find anywhere else to click on the floor") two
      // marks: its hole (glowing in its opening's own shape) and its sign, each its own box; the floor round them free
      const [, , rx, ry] = dd.pit.split(" ").map(Number), k = (SCENE.bottom - SCENE.top) / 700, sh = openingShape(dd.shape || "circle");
      if (r.width && r.height) {
        pics.push({ x0: cx - rx, y0: cy - ry, x1: cx + rx, y1: cy + ry, el: d, box: { x: cx - rx, y: cy - ry, width: 2 * rx, height: 2 * ry }, outline: openingD(sh, cx, cy, rx * 1.12, ry * 1.25), kind: "pit", spot: [cx, cy],
                    within: (x, y, m = 0) => inOpening(sh, cx, cy, rx + m, ry + m, x, y) });   // (a pointer over its hole: in its shape, not its box's corners)
        const t = d.querySelector("text"), sg = t && t.previousElementSibling;
        if (t && sg && sg.tagName.toLowerCase() === "rect") { const a = sg.getBoundingClientRect(), bb = sg.getBBox();
          pics.push({ ...stage(a), el: d, box: { x: bb.x, y: bb.y, width: bb.width, height: bb.height }, kind: "pit", spot: [cx, cy] }); }
      }
      continue;
    }
    // (ticket 147: a door glows too, its box its doorway's own: the first shape in it, not its sign)
    if (!view && d.hasAttribute("data-door")) {
      const shape = d.querySelector("path, rect"), M = d.getScreenCTM();
      if (!shape || !M) continue;
      const r = shape.getBoundingClientRect();
      if (!r.width || !r.height) continue;
      const Mi = M.inverse(), p = new DOMPoint(r.x, r.y).matrixTransform(Mi), q = new DOMPoint(r.x + r.width, r.y + r.height).matrixTransform(Mi);
      const box = { x: Math.min(p.x, q.x), y: Math.min(p.y, q.y), width: Math.abs(q.x - p.x), height: Math.abs(q.y - p.y) }, at = stage(r);
      // (a nav arrow's doorway stands where its arrow is, and feet past it count as at it; a ▼ at the bottom edge, as the nav arrow S)
      const w = dd.way && worldWaysAt.find(e => e[0] === dd.way);
      if (dd.way && !w) continue;
      // (ticket 233: a way toward us out of a room, a nav arrow too, as the world's: insideWays)
      const kind = dd.way ? "nav" : "door";
      pics.push({ ...at, el: d, box, door: true, kind, spot: w ? waySpot(dd.way) : under(at), out: dd.way ? WAY_OUT[dd.way] : null });   // (ticket 369: the way's spot on the floor, not its spaced arrow)
      continue;
    }
    if (!view) {   // (a doorway or a sign with no mark of its own: its whole drawing, the feet under it)
      const r = d.getBoundingClientRect();
      if (!r.width || !r.height) continue;
      const at = stage(r);
      pics.push({ ...at, el: d, box: bbox(d), kind: d.querySelector("text") ? "sign" : "doorway", spot: under(at) });
      continue;
    }
    const r = svgBox(view);
    if (!r.width || !r.height) continue;
    // (its glow's box: its frame, the doorway's own rects but the shadow
    // (see-through); else the view, a little larger. Not the doorway's
    // bounds: the scene inside a picture spills past it, hidden)
    const frames = [...d.querySelectorAll(":scope > rect:not([opacity])")].map(f => f.getBBox()).filter(f => f.width && f.height);
    const vb = { x: view.x.baseVal.value, y: view.y.baseVal.value, width: view.width.baseVal.value, height: view.height.baseVal.value };
    const box = frames.length ? frames.reduce((u, f) => { const x = Math.min(u.x, f.x), y = Math.min(u.y, f.y); return { x, y, width: Math.max(u.x + u.width, f.x + f.width) - x, height: Math.max(u.y + u.height, f.y + f.height) - y }; })
                              : { x: vb.x - 0.04 * vb.width, y: vb.y - 0.04 * vb.height, width: 1.08 * vb.width, height: 1.08 * vb.height };
    pics.push({ ...stage(r), el: d, box, kind: "picture" });
  }
  marks = { text: bgShown.text, ladders, pics };
  showEnterGlow();
}
// (the ladder whose foot the friend stands at, if any)
function ladderNear() {
  const BG = state.background, fx = BG.figX ?? 0, fz = SCENE.ground + (BG.figY ?? 0), k = figureT.k;
  if (marks.text !== bgShown.text) return null;
  let best = null;
  for (const L of marks.ladders) {
    const dx = Math.abs(L.x - fx), dz = Math.abs(L.z - fz);
    if (dx > Math.max(24, 150 * k) || dz > Math.max(16, 110 * k)) continue;
    if (!best || dx + dz < best.d) best = { ...L, d: dx + dz };
  }
  return best;
}
function mountLadder(L) {
  const B = state.body, BG = state.background;
  if (BG.seat) standUp();
  if (B.pose !== "stand") strikePose("stand");
  B.anim = "climbup"; B.facing = "back";
  BG.figX = Math.round(L.x); BG.figY = Math.round(L.z - SCENE.ground + 2);
  climb = { place: climbPlace(BG), x: L.x, z: L.z + 2, top: L.top, clipUp: L.up ?? null, door: L.door, hands: L.hands, start: performance.now(), done: false, through: false, manual: true, got: 0 };
}
// (ticket 187) The passages within reach, nearest first, each scored by how
// far it is over how far reach goes (below 1, within reach), so every kind
// weighs against every other: a picture by the chest (within twice the
// chest's height of it, about an arm's length, and the feet not far below
// its bottom, about their height: ticket 145), every other passage by the
// feet on the floor, in depth (floorNearness, as the nav arrows). The
// nearest is the one: it glows, and X, Enter or the 🚪 button take it; the
// others within reach glow dimmer (Matthew, 2026-10-17: "at most one is
// highlighted as offering the go through nearest passage action; the others
// that are also nearby can show up too, but dimmer"). Any input, not only a
// controller: a tap goes through too.
let enterAt = null, enterGlow = null;
const passDims = new Map();   // (the dimmer glows: passage element -> its glow)
// (ticket 199, Matthew: "when we hover over where a passage's arrow would
// appear ... use the pointer to offer that as an override": the pointer
// over a passage, or near where a nav arrow would be, makes that one the
// bright one, the nearest dimmer; X, Enter or a click take it, the friend
// walking, or swimming, to it first)
let hoverPass = null;
function passUnderPointer(e) {
  // (not over the friend, or a thing put in the place: those are for dragging; a place's own movable scenery is no matter)
  // (ticket 471: a cave passage put in the place is a doorway, though inside a thing)
  if (marks.text !== bgShown.text || !e.target || !e.target.closest || e.target.closest("#figure") || (e.target.closest("[data-drag^='thing:']") && !e.target.closest(".thing .portal"))) return null;
  const P = pic.getScreenCTM();
  if (!P) return null;
  let best = null;
  for (const p of marks.pics) if (p.kind === "nav" && p.spot) {
    // (ticket 404, Matthew: "the 'hover over' feature of nav buttons sometimes seem misaligned?") near where its arrow is
    // drawn, not its spot on the floor: worldWaysHud spaces the arrows apart and keeps them inside the picture, so in
    // the fairy forest the arrow to bubble space sat well above its spot, and the flower below it lit it
    const tri = document.querySelector(`#ways .way-arrow[data-way="${p.el.dataset.way}"] .way-tri`), r = tri && tri.getBoundingClientRect();
    const s = r && r.width ? { x: r.left + r.width / 2, y: r.top + r.height / 2 } : new DOMPoint(p.spot[0], p.spot[1]).matrixTransform(P);
    const d = Math.hypot(s.x - e.clientX, s.y - e.clientY);
    if (d < 44 && (!best || d < best.d)) best = { d, p };
  }
  if (best) return best.p;
  const q = picUnits(e), m = 10 / Math.max(0.01, P.a);   // (nearly over: ten pixels round it)
  return marks.pics.filter(p => p.kind !== "nav" && p.el.isConnected && (p.within ? p.within(q.x, q.y, m) : q.x >= p.x0 - m && q.x <= p.x1 + m && q.y >= p.y0 - m && q.y <= p.y1 + m))
                   .sort((a, b) => (a.x1 - a.x0) * (a.y1 - a.y0) - (b.x1 - b.x0) * (b.y1 - b.y0))[0] || null;
}
function passesNear(reach = 1) {
  const BG = state.background, J = poseJoints, T = figureT;
  if (BG.kind === "none" || entering || marks.text !== bgShown.text) return [];
  let chest = null;
  if (J && J.arms.length && J.legs.length) {
    const at = (x, y) => [T.dx + T.k * x, T.dy + T.sg + T.k * (y - T.g)];
    const mean = ps => [ps.reduce((s, p) => s + p[0], 0) / ps.length, ps.reduce((s, p) => s + p[1], 0) / ps.length];
    const A = mean(J.arms.map(a => a.A)), H = mean(J.legs.map(l => l.H)), [cx, cy] = at((A[0] + H[0]) / 2, (A[1] + H[1]) / 2);
    chest = { cx, cy, torso: T.k * Math.hypot(A[0] - H[0], A[1] - H[1]), feet: Math.max(...J.legs.map(l => at(l.F[0], l.F[1])[1])) };
  }
  const out = [];
  for (const p of marks.pics) {
    if (!p.el.isConnected) continue;
    let n;
    if (p.kind === "picture") {
      if (!chest) continue;
      const { cx, cy, torso, feet } = chest, d = Math.hypot(Math.max(p.x0 - cx, 0, cx - p.x1), Math.max(p.y0 - cy, 0, cy - p.y1));
      if (feet - p.y1 > 3.6 * torso) continue;
      n = d / (2 * torso);
    } else if (p.spot) n = floorNearness(p.spot[0], p.spot[1], p.out);
    else continue;
    if (n < reach) out.push({ p, n });
  }
  // (a tie, as where a place's own old path and a nav arrow end at the same edge: the nav arrow, the world's
  // way, wins; Matthew, 2026-10-17: slowly away from the places' own signs, toward the arrows)
  return out.sort((a, b) => (a.n - b.n) || ((b.p.kind === "nav") - (a.p.kind === "nav")));
}
function enterTarget() { const q = passesNear(); return q.length ? q[0].p : null; }
// (a glow's rounded frame round a passage's box, inside its doorway: in front of it, behind whatever stands nearer)
function passFrame(b, dim, outline) {
  // (ticket 267: a pit's hole glows in its own shape)
  if (outline) return dim ? `<path d="${outline}" fill="none" stroke="#ffd23f" stroke-opacity="0.5" stroke-width="3" stroke-dasharray="9 6" vector-effect="non-scaling-stroke"/>`
    : `<path d="${outline}" fill="#fff6a0" fill-opacity="0.14" stroke="#ffd23f" stroke-opacity="0.75" stroke-width="7" vector-effect="non-scaling-stroke"/><path d="${outline}" fill="none" stroke="#fffbe0" stroke-width="2.5" vector-effect="non-scaling-stroke"/>`;
  const e = Math.max(2, 0.03 * Math.min(b.width, b.height)), r = 3 * e, at = `x="${f1(b.x - e)}" y="${f1(b.y - e)}" width="${f1(b.width + 2 * e)}" height="${f1(b.height + 2 * e)}" rx="${f1(r)}"`;
  return dim ? `<rect ${at} fill="none" stroke="#ffd23f" stroke-opacity="0.5" stroke-width="3" stroke-dasharray="9 6" vector-effect="non-scaling-stroke"/>`
             : `<rect ${at} fill="#fff6a0" fill-opacity="0.14" stroke="#ffd23f" stroke-opacity="0.75" stroke-width="7" vector-effect="non-scaling-stroke"/>` +
               `<rect ${at} fill="none" stroke="#fffbe0" stroke-width="2.5" vector-effect="non-scaling-stroke"/>`;
}
// (ticket 256, Matthew: "when we get close to a closed door on a side wall, in the tree house or the castle, let's have
// it open and reveal what's behind it"; "when the door like this in the tree house is glowing, it should animate
// opening. And when it unglows, it closes") A side wall's door (a [data-leaf] in a .portal[data-door]) swings open while
// it is the one the glow is on, to its data-open shape as walkInto swings it, its planks and handle fading; and shut
// again when the glow moves off it. glowLeaf: the door the glow has opened.
let glowLeaf = null;
const DOOR_SWING_S = 0.45;
function doorSwing(leaf, open) {
  if (!leaf || !leaf.isConnected || leaf.dataset.swung === (open ? "open" : "shut")) return;
  leaf.dataset.swung = open ? "open" : "shut";
  const wide = leaf.getAttribute("data-open");
  for (const p of leaf.querySelectorAll("path")) {
    for (const a of p.querySelectorAll("animate[data-swing]")) a.remove();
    const shut = p.getAttribute("d"), a = document.createElementNS("http://www.w3.org/2000/svg", "animate");
    for (const [k, v] of [["attributeName", "d"], ["from", open ? shut : wide], ["to", open ? wide : shut], ["dur", DOOR_SWING_S + "s"], ["fill", "freeze"], ["begin", "indefinite"], ["data-swing", ""]]) a.setAttribute(k, v);
    p.append(a);
    try { a.beginElement(); } catch (e) { /* (an old browser: the door stays as it is) */ }
  }
  for (const n of leaf.querySelectorAll("line, ellipse")) { n.style.transition = `opacity ${DOOR_SWING_S}s`; n.style.opacity = open ? "0" : ""; }
}
function showEnterGlow() {
  if (marks.text !== bgShown.text && bgShown.node) return;   // (measured again in a moment: the glows kept till then)
  // (ticket 246: on a rope bridge, the house at the end they would go in at, alone)
  const be = bridgeEnd(), bp = be && marks.pics.find(q => q.el.isConnected && q.el.matches(`.portal[data-to="treeroom"][data-house="${be.house}"]`));
  const near = be ? [] : passesNear(), hover = !be && hoverPass && marks.pics.find(q => q.el === hoverPass.el && q.el.isConnected), p = be ? bp || null : hover || (near.length ? near[0].p : null);
  enterAt = p;
  // (ticket 256: the door the glow is on open, the one it left shut)
  const leaf = p && p.kind !== "nav" && p.el.matches && p.el.matches(".portal[data-door]") ? p.el.querySelector("[data-leaf]") : null;
  // (ticket 264, Matthew: "it opens and then shuts, and then I fade through the shut door") not shut while it is being
  // walked through: the glow leaves it as the walk begins
  if (leaf !== glowLeaf && !(entering && !leaf)) { doorSwing(glowLeaf, false); glowLeaf = leaf; doorSwing(leaf, true); }
  // (a nav arrow's glow is its arrow over the picture: aimed, or near and dimmer)
  worldWaysAim(p && (p.kind === "nav" || (p.el.dataset && /^p-/.test(p.el.dataset.way || ""))) ? p.el.dataset.way : null);   // (ticket 525: a cave door's arrow too)
  const NS = "http://www.w3.org/2000/svg", put = (el, box, dim, outline) => {
    const g = document.createElementNS(NS, "g");
    g.setAttribute("class", "enter-glow" + (dim ? " dim" : "")); g.setAttribute("pointer-events", "none");
    g.innerHTML = passFrame(box, dim, outline); el.append(g); return g;
  };
  // the dimmer ones
  const dims = new Set(near.map(q => q.p).filter(q => q !== p && q.kind !== "nav"));
  for (const [el, g] of passDims) if (!g.isConnected || ![...dims].some(q => q.el === el)) { g.remove(); passDims.delete(el); }
  for (const q of dims) if (!passDims.has(q.el)) passDims.set(q.el, put(q.el, q.box, true, q.outline));
  // the one
  if (!p || p.kind === "nav") { if (enterGlow) enterGlow.remove(); enterGlow = null; return; }
  if (enterGlow && enterGlow.isConnected && enterGlow.parentNode === p.el) return;
  if (enterGlow) enterGlow.remove();
  enterGlow = put(p.el, p.box, false, p.outline);
}
// (ticket 256, Matthew: "we can walk through an open door, by just colliding with it") walking on into a side wall at
// the door the glow has opened in it, between its feet's depths, the way it faces: through it. The door's opening's
// first and last points are its feet, near and far (its first shape, M near ... far Z), in the picture's units.
function doorPushed(vx) {
  const leaf = glowLeaf, door = leaf && leaf.isConnected && leaf.dataset.swung === "open" ? leaf.closest(".portal[data-door]") : null;
  const shape = door && door.querySelector("path");
  if (!shape || entering || Math.abs(vx) < 0.3) return false;
  const M = pic.getScreenCTM(), E = shape.getScreenCTM(), n = (shape.getAttribute("d").match(/-?\d+(\.\d+)?/g) || []).map(Number);
  if (!M || !E || n.length < 4) return false;
  const T = M.inverse().multiply(E), pt = (x, y) => new DOMPoint(x, y).matrixTransform(T);
  const near = pt(n[0], n[1]), far = pt(n[n.length - 2], n[n.length - 1]), B = state.background, z = SCENE.ground + (B.figY ?? 0), slack = 6;
  if (Math.sign(vx) !== Math.sign(near.x) || z < Math.min(near.y, far.y) - slack || z > Math.max(near.y, far.y) + slack) return false;
  enterScene(door);
  return true;
}
function enterPicture() {
  // (ticket 246) on a rope bridge: along it to the end they face, and into the house there; (ticket 264, Matthew: "sometimes I
  // click on the treehouse across the bridge, but it's overridden by still standing next to the current tree house ... We
  // should favor the click target, not where are feet are") a house clicked (enterAt, set by the click) at one of its
  // ends: that one
  const onBr = !entering && bridgeNow();
  if (onBr) {
    const h = enterAt && enterAt.el && enterAt.el.dataset && enterAt.el.dataset.to === "treeroom" ? +enterAt.el.dataset.house - 1 : -1;
    if (enterGlow) enterGlow.remove(); enterGlow = null; enterAt = null;
    if (h === onBr.W.a || h === onBr.W.b) bridgeGoIn({ j: onBr.j, end: h === onBr.W.b ? 1 : 0, house: h + 1, other: (h === onBr.W.b ? onBr.W.a : onBr.W.b) + 1 });
    else bridgeGoIn();
    return;
  }
  const p = enterAt;
  if (!p || !p.el.isConnected || entering) return;
  if (p.ladder && climb && !climb.none) return;   // (on a ladder already: climbing it)
  if (climb && !climb.none) climb.through = true;   // (off the ladder, arriving: see draw)
  if (enterGlow) enterGlow.remove();
  enterGlow = null; enterAt = null;
  for (const g of passDims.values()) g.remove();
  passDims.clear();
  if (p.ladder) climbLadder(p.el);   // (ticket 173)
  else enterScene(p.el);
}
// (at the top of a ladder that is a doorway: through it, as a click would)
function climbThrough() {
  const c = climb, node = bgShown.node;
  if (!c || !c.door || !node || entering || state.body.anim !== "climbup") return;
  // (ticket 269) up a tree house's ladder from the ground: onto its platform, along it to the door, and in
  if (state.background.kind === "treehouse" && c.door.to === "treeroom" && c.door.house && !c.down) { platformIn(+c.door.house); return; }
  // (ticket 264: down a hatch's ladder, its own way through, not another to the same place: a tree room's window leads
  // outside too, and was taken, the friend coming out at the default spot)
  const all = [...node.querySelectorAll(`.portal[data-to="${c.door.to}"]` + ["house", "room", "tent"].map(k => c.door[k] ? `[data-${k}="${c.door[k]}"]` : "").join(""))];
  const door = (c.down && all.find(d => d.querySelector("[data-down]"))) || all[0];
  if (!door) return;
  c.through = true;
  enterScene(door);
}
// (ticket 142: two of them, "jump" short and fast and "jumphigh" high and
// slow, each with its own height and speed in the Body tab)
// (ticket 138) Jumping, once, not in a loop: crouching, arms back (the
// beginning), up in the air with the arms up and down again (the middle),
// landing on bent knees and standing up (the end); then back to what they
// were doing (jumpPrev: from the chip, standing still). JUMP_TIMES: where
// each part ends, in seconds. Their shadow stays on the ground.
let jumpStart = 0, jumpPrev = "none", jumpEnd = 0;
const JUMP_TIMES = [0.24, 0.82, 1.12];
const isJump = a => a === "jump" || a === "jumphigh";
// (ticket 143) where they stand is deep water (not a shallow stream)
const deepHere = () => { const W = state.background; return W.kind !== "none" && !SHALLOW_WATER[W.kind] && inWater(W, W.figX ?? 0, SCENE.ground + (W.figY ?? 0)); };
// (ticket 143) The jumps in deep water, moving the friend inside the water's
// mask (bob, a transform in their units: the waterline stays on the water),
// over JUMP_TIMES as the jumps on land. The short one, as in a pool jumping
// for a ball: a dip, up fast till the waist is out, the arms reaching up,
// and back in. The high one, turned to the side, a dolphin's dive: sinking
// a little, the arms going up in front of the head, out of the water in an
// arc, head first, the body turning over (from leaning forward to head
// down), in again ahead of where they left, and a roll back upright under
// the water; endJump moves them on by as much (jumpAhead). Facing us or away,
// the high one goes straight up and down, the arms overhead. A splash where
// they leave the water and where they come back in (splash: where, how far on).
let jumpAhead = 0;
function waterJump(B, hi, t, H, dir, lean, both) {
  const [c, f, l] = JUMP_TIMES, ease = x => x * x * (3 - 2 * x), Lb = state.shirt.length ?? 150;
  const up = side => -90 - (B.arms ?? 0) - lean(side), straight = -(B.elbows ?? 0), mixA = (a, b, u) => a + (b - a) * u;
  const splashAt = (x, t0) => t >= t0 && t < t0 + 0.32 ? { x, p: (t - t0) / 0.32 } : null;
  if (hi && dir) {
    const Hd = 1.2 * H, D = 0.9 * H;
    let dy, angle, dx = 0, armsUp, legs = 0;
    if (t < c) { const u = ease(t / c); dy = 18 * u; angle = 25 * u; armsUp = u; }
    else if (t < f) { const u = (t - c) / (f - c); dy = 18 * (1 - u) + 40 * u - Hd * Math.sin(Math.PI * u); angle = 25 + 130 * u; dx = D * u; armsUp = 1; }
    else { const u = ease((t - f) / (l - f)); dy = 40 * (1 - u); angle = 155 + 205 * u; dx = D; armsUp = 1 - u; legs = Math.sin(Math.PI * u); }
    jumpAhead = dir * D;
    return { ...both("Arm", sd => mixA(0, up(sd) + 4 * sd, armsUp)), ...both("Elbow", () => mixA(0, straight, armsUp)),
             ...both("Leg", sd => 14 * legs * sd), ...both("Knee", () => 20 * legs), water: true, jump: true,
             bob: `translate(${f1(dir * dx)} ${f1(dy)}) rotate(${f1(dir * angle)} 0 ${f1(Lb)})`, splash: splashAt(0, c + 0.05) || splashAt(dir * D, f - 0.06) };
  }
  jumpAhead = 0;
  const Hs = hi ? 1.2 * H : 1.4 * H, reach = hi ? 1 : 0.85;
  let dy, arms, knees;
  if (t < c) { const u = ease(t / c); dy = 10 * u; arms = 0; knees = 40 * u; }
  else if (t < f) { const u = (t - c) / (f - c); dy = 10 * (1 - u) - Hs * Math.sin(Math.PI * u); arms = Math.min(1, 2 * u) * reach; knees = 40 * (1 - u) + 25 * Math.sin(2 * Math.PI * u); }
  else { const u = (t - f) / (l - f); dy = 12 * Math.sin(Math.PI * u); arms = reach * (1 - u); knees = 15 * Math.sin(Math.PI * u); }
  return { ...both("Arm", sd => mixA(0, up(sd) + (hi ? 4 : 16) * sd, arms)), ...both("Elbow", () => mixA(0, hi ? straight : 12, arms)),
           ...both("Leg", sd => 6 * sd), ...both("Knee", () => knees), water: true, jump: true,
           bob: `translate(0 ${f1(dy)})`, splash: splashAt(0, c + 0.05) || splashAt(0, f - 0.04) };
}
function startJump(prev = "none", kind = "jump") {
  if (!isJump(prev)) jumpPrev = prev;
  jumpStart = performance.now();
  state.body.anim = kind;
}
function endJump() {
  jumpEnd = 0;
  // (after a dolphin's dive, they are where they came back up: ticket 143)
  if (jumpAhead) { const F = sceneFrame, BG = state.background; BG.figX = Math.round(clamp((BG.figX ?? 0) + jumpAhead * figureT.k, F.left, F.right)); jumpAhead = 0; }
  if (isJump(state.body.anim)) { state.body.anim = jumpPrev || "none"; solidUnstick(); changed();   // (ticket 149)
    // (ticket 162: landing on a pit's hole falls in)
    const BG = state.background, pit = !entering && BG.kind !== "none" ? pitAt(BG.figX ?? 0, SCENE.ground + (BG.figY ?? 0)) : null;
    if (pit) enterScene(pit); }
}
// (ticket 438) riding the friend's horse: astride, the walk or run the horse's, not theirs. In profile the thighs go
// forward and down along its side and the shins back; facing us or away, the legs go down either side of it; the hands
// forward and low, holding the reins. In place of moveNow's limbs, while body.horse is "ride" (riding()).
const RIDE_ANIMS = ["none", "walk", "run"];
const riding = B => state.body.horse === "ride" && B.pose !== "lie" && RIDE_ANIMS.includes(B.anim || "none") && !poseHeld;
function rideMove(B) {
  const dir = facingOf(B), both = (k, f) => ({ [k + "L"]: f(1), [k + "R"]: f(-1) });
  if (dir) return { ...both("Leg", () => 45), ...both("Knee", () => 75), ...both("Arm", () => 12), ...both("Elbow", () => 50), riding: true };
  return { ...both("Leg", () => 34), ...both("Knee", () => 18), ...both("Arm", () => 12), ...both("ElbowCurl", () => 70), riding: true };
}
function moveNow() {
  const B = state.body, a = B.anim;
  if (!a || a === "none" || poseHeld) return treadNow() || (!poseHeld && bubbleStance());   // (ticket 343: on bubbles, balancing)
  const u = lfoNow("B").phase, T = 2 * Math.PI, sin = f => Math.sin(T * u * f), cos = f => Math.cos(T * u * f);
  const dir = facingOf(B), lean = side => dir ? (side === dir ? -14 : 4) : 0;
  const both = (k, f) => ({ [k + "L"]: f(1), [k + "R"]: f(-1) });
  // (ticket 287, Matthew: "the current swim is for upright treading, not horz motion"; "let's use what we learned about
  // pose in crawling for swimming --- Now, our limbs are out in front, fully, and behind us, fully, and flapping")
  // Swimming through deep water ("swim"; "swimfast", running, twice as quick), from the side: turned about the hips to
  // lie along the surface (rot, as the crawl turns: SWIM_TILT), the arms straight, windmilling half a turn apart, each
  // from fully in front (overhead) down under the body and back to the hip, then up over the back and in front again
  // (the front crawl); the legs straight back from the hips, kicking in turn twice a stroke, the toes pointed. Still in
  // the water, they tread, upright, as before.
  if ((a === "swim" || a === "swimfast") && dir && !poseHeld && deepHere()) {
    const p = a === "swimfast" ? (2 * u) % 1 : u, k = Math.sin(4 * Math.PI * p), t0 = (B.knees ?? 0) / 2, b0 = B.knees ?? 0;
    // (the hand goes in ahead of the head and a little below it, SWIM_REACH short of overhead, where the big head
    // hid it, and comes out past the hip, a little up, as a swimmer's does)
    return { ...both("Arm", sd => -90 + SWIM_REACH + 360 * ((p + (sd === dir ? 0 : 0.5)) % 1) - (B.arms ?? 0) - lean(sd)), ...both("Elbow", () => -(B.elbows ?? 0)),
             ...both("Leg", sd => 14 * sd * k - t0), ...both("Knee", sd => 14 * Math.max(0, sd * k) - b0), ...both("Foot", () => -70),
             rot: SWIM_TILT * dir, float: SWIM_FLOAT, swim: true };
  }
  // Toward us or away, the friend lying along the surface runs into the picture, seen short as the crawl is (squash: the
  // back; away, the head beyond it, the legs toward us, each seen short (Step), kicking, the soles showing; toward, the
  // face at the surface, a little of the back behind it, the legs hidden). Each arm windmills in the plane along the
  // swim, seen from behind or in front and a little above: its hand drops in the picture by the share drop() says
  // (up, negative), the arm a little out from the shoulder, seen short by what it leaves of its length (ArmStep)
  if ((a === "swim" || a === "swimfast") && !dir && !poseHeld && deepHere()) {
    const away = B.facing === "back", p = a === "swimfast" ? (2 * u) % 1 : u, k = Math.sin(4 * Math.PI * p);
    const drop = sd => { const ph = 2 * Math.PI * (1 - ((p + (sd > 0 ? 0 : 0.5)) % 1)), f = Math.cos(ph), up = Math.sin(ph);
      return away ? -(0.45 * f + 0.89 * up) : 0.45 * f - 0.89 * up; };
    return { ...both("Arm", sd => (drop(sd) < 0 ? -75 : 75) - (B.arms ?? 0)), ...both("Elbow", () => -(B.elbows ?? 0)),
             ...both("ArmStep", sd => 1 - Math.min(1, Math.abs(drop(sd)))),
             ...both("Step", sd => away ? 0.5 - 0.12 * sd * k : 0), frontFeet: away ? 0.5 : 0,
             squash: 0.45, back: 0.25, chin: 0.3, swim: true };
  }
  if (a === "swim") return treadNow() || treadStroke(1, u);
  // (ticket 141) swimming fast (the controller's run, in water): the stroke twice as quick
  if (a === "swimfast") return { ...treadStroke(1, (2 * u) % 1), water: true };
  // (ticket 141) crouching down, held there: the knees bent, the arms a little forward
  // (ticket 414's audit: facing us, the elbows' 25 degrees bent them out to the sides, in the picture; a crouch's arms reach
  // forward for balance, toward us: a curl. In profile the bend is forward already)
  if (a === "crouch") return { ...both("Leg", () => 34), ...both("Knee", () => 75), ...both("Arm", sd => -18 - lean(sd)), ...both("Elbow", () => dir ? 25 : 0), ...(dir ? {} : both("ElbowCurl", () => 55)), held: true, grounded: true };
  if (a === "walk" && bubbleFloor()) return bubbleWalk(u, dir);   // (ticket 343)
  if (a === "walk") {
    // (each leg swinging forward and back, its knee bending as it comes
    // through; each arm swinging with the other leg; a small lift each step)
    // (ticket 140) toward us or away: each foot in turn raised as its leg
    // swings toward us (or away), the leg seen shorter (Step: how much
    // shorter, a share of it), never splayed out to the side, and the feet
    // seen front on (or from behind), the sole showing as a foot lifts
    // (frontFeet); a small lift each step
    const s = sin(1), c = cos(1);
    if (!dir) return { ...both("Step", sd => 0.16 * Math.max(0, sd * s)), ...both("Arm", sd => 8 * sd * s), ...both("Elbow", () => 10),
                       lift: 3 * Math.abs(s), frontFeet: 0.16 };
    // (ticket 309, Matthew: "our knees should lead each step, and our foot follows, as two separate parts of the motion, in
    // phase") from the side: the thigh swings, and the shin follows it WALK_LAG behind in the stride (the foot follows the
    // knee): as the thigh comes through, the knee bends, leading, the foot trailing; then the foot swings on to catch up and
    // land, the leg straightening; while the foot is down the leg stays straight (a knee never bends backward)
    const ph = sd => 2 * Math.PI * u + (sd > 0 ? 0 : Math.PI), thigh = sd => 24 * Math.sin(ph(sd)), shin = sd => 24 * Math.sin(ph(sd) - WALK_LAG);
    return { ...both("Leg", thigh), ...both("Knee", sd => 4 + Math.max(0, 1.7 * (thigh(sd) - shin(sd)))),
             ...both("Arm", sd => 20 * sd * s), ...both("Elbow", () => 14), lift: 3 * Math.abs(c) };
  }
  // (ticket 308, Matthew: "two new animations: walking up/down stairs ... with a flag saying if we are facing towards up
  // the stairs, or down. no need to render actual stairs ... Walking up a stairs is a kind of climbing with our feet.
  // Walking down is a kind of controlled falling, like running but more extreme.") Up (B.stairsWay "up"): each knee in
  // turn lifted high and bent, as onto the next step, the other leg pushing back off the one below, a rise each step.
  // Down, twice as quick: each foot in turn reaching on and down, its leg nearly straight, the other knee bending under
  // the weight (they stand on the lower foot, grounded: the reaching one, as if on the step below), the arms up and out
  // for balance; toward us or away, each leg seen short as it lifts or reaches (Step), the soles showing
  // (ticket 316, Matthew: "pull it --- new animation for reaching up and pulling down with dominate hand") pulling a
  // lever: the dominant hand (pullSide) reaching up, straight, over 0.35 s, a moment's grip, then pulling down to the
  // shoulder's height with the elbow bending, the knees giving a little with it, and back to rest; startPull times it
  if (a === "pull") {
    const t = (performance.now() - pullAt) / 1000, ease = x => x * x * (3 - 2 * x), up = -95 - (B.arms ?? 0), down = 35 - (B.arms ?? 0);
    const [arm, elbow, knee] = t < 0.35 ? [up * ease(t / 0.35), 0, 0] : t < 0.5 ? [up, 0, 0] : t < 0.85 ? [up + (down - up) * ease((t - 0.5) / 0.35), 60 * ease((t - 0.5) / 0.35), 14 * ease((t - 0.5) / 0.35)]
                             : [down * (1 - ease(Math.min(1, (t - 0.85) / 0.25))), 60 * (1 - ease(Math.min(1, (t - 0.85) / 0.25))), 14 * (1 - ease(Math.min(1, (t - 0.85) / 0.25)))];
    // (from behind, drawn as facing us without the face: their right hand shows on our right, so the other side's arm)
    const side = (pullSide > 0) !== (B.facing === "back") ? "L" : "R";
    return { ["Arm" + side]: arm, ["Elbow" + side]: elbow, ...both("Knee", () => knee), grounded: true };
  }
  if (a === "stairs") {
    const up = (B.stairsWay ?? "up") !== "down", s = sin(up ? 1 : 2), c = cos(up ? 1 : 2);
    if (up) {
      if (!dir) return { ...both("Step", sd => 0.42 * Math.max(0, sd * s)), ...both("Arm", sd => 12 * sd * s), ...both("Elbow", () => 20), lift: 7 * Math.abs(s), frontFeet: 0.42 };
      return { ...both("Leg", sd => 62 * Math.max(0, sd * s) - 10 * Math.max(0, -sd * s)), ...both("Knee", sd => 8 + 92 * Math.max(0, sd * s)),
               ...both("Arm", sd => 24 * sd * s - 6), ...both("Elbow", () => 22), lift: 7 * Math.abs(s) };
    }
    if (!dir) return { ...both("Step", sd => 0.24 * Math.max(0, sd * s)), ...both("Arm", sd => -24 + 8 * sd * s), ...both("Elbow", () => 12), lift: 2 * Math.abs(c), frontFeet: 0.24, grounded: true };
    return { ...both("Leg", sd => 30 * Math.max(0, sd * s) - 6 * Math.max(0, -sd * s)), ...both("Knee", sd => 6 + 58 * Math.max(0, -sd * s)),
             ...both("Arm", sd => -28 + 12 * sd * s), ...both("Elbow", () => 18), grounded: true };
  }
  if (a === "run") {
    // (twice as quick: long strides, the back foot kicked high, the elbows
    // bent, the arms pumping, a bound each step)
    const s = sin(2), c = cos(2);
    if (!dir) return { ...both("Step", sd => 0.3 * Math.max(0, sd * s)), ...both("Arm", sd => 14 * sd * s - 10), ...both("Elbow", () => 80),
                       lift: 8 * Math.abs(s), frontFeet: 0.3 };
    return { ...both("Leg", sd => 40 * sd * s), ...both("Knee", sd => 25 + 70 * Math.max(0, sd * c)),
             ...both("Arm", sd => 32 * sd * s - 10), ...both("Elbow", () => 85), lift: 9 * Math.abs(c) };
  }
  if (a === "crawl") {
    // (on hands and knees: turned about the hips, by as much as puts the
    // hands and the knees on the ground together for this friend's arms,
    // thighs and body (cos turn = (arm - thigh) / body, between 50 and 100
    // degrees); each thigh straight down, each shin level behind, the feet
    // soles up, the arms straight down; a hand and the other knee forward
    // together)
    const body = Math.max(40, (state.shirt.length ?? 150) - 8), thigh = (B.legLen ?? 140) / 2, arm = B.armLen ?? 95;
    const tilt = Math.max(50, Math.min(100, Math.acos(Math.max(-1, Math.min(1, (arm - thigh) / body))) * 180 / Math.PI));
    const s = sin(1), t0 = (B.knees ?? 0) / 2, b0 = B.knees ?? 0, swing = sd => 12 * sd * s;
    // (ticket 286, Matthew: toward and away "look more like someone crawling up or down the walls of the room"; his picture,
    // a baby crawling away on a carpet, seen from behind and a little above) Toward us or away, the friend on the floor,
    // the body running into the picture: its back seen short (squash: the shoulders, the neck and the arms drawn that share
    // of their height above the hips, the head on top of them, not squashed), the arms straight down to the floor, each leg
    // seen short (Step: the thigh down to the knee, the shin along the floor toward us), from behind the soles of the feet
    // showing (frontFeet); a hand and the other knee reaching on in turn (the leg and the arm a little longer, then
    // shorter), a small bob; standing on the legs as drawn (grounded). The arms straight and whole (upright, so not seen
    // short), a little out from the shoulders, to each hand flat on the floor beside the body (Matthew, of 0.0.256: "the
    // hand position seems off ... like we are keeping our hands on our hips?"). Toward us, the head is nearest: down in front of
    // the shoulders (chin, a share of the arm below them), the arms straight down beside it to the hands on the floor,
    // and behind it a little of the back (back: the shirt turned over, that share of its height); the legs hidden behind
    // it (Matthew: "not so much torso, and almost no legs at all -- none maybe")
    if (!dir) { const away = B.facing === "back";
      return { ...both("Step", sd => 0.5 - 0.1 * sd * s), ...both("Arm", sd => 90 - (B.arms ?? 0) - (away ? 10 : 12) + 5 * sd * s), ...both("Elbow", () => -(B.elbows ?? 0)),
               frontFeet: away ? 0.5 : 0, squash: 0.45, lift: 2 * Math.abs(s), grounded: true, back: 0.25, chin: 0.45 }; }
    return { ...both("Leg", sd => tilt - swing(sd) - t0), ...both("Knee", sd => 90 - swing(sd) - b0),
             ...both("Arm", sd => 90 - tilt - (B.arms ?? 0) - lean(sd) + 14 * sd * s), ...both("Elbow", () => -(B.elbows ?? 0)),
             ...both("Foot", () => -(180 - tilt)), rot: dir ? tilt * dir : 0 };
  }
  if (isJump(a)) {
    const hi = a === "jumphigh", rate = Math.max(0.1, Number(hi ? B.jumpHighS ?? 75 : B.jumpShortS ?? 150) || 100) / 100;
    const t = (performance.now() - jumpStart) / 1000 * rate, [c, f, l] = JUMP_TIMES, ease = x => x * x * (3 - 2 * x);
    const H = Math.max(0, Number(hi ? B.jumpHighH ?? 90 : B.jumpShortH ?? 30) || 0) / 100 * (B.legLen ?? 140);
    if (t >= l) { if (!jumpEnd) jumpEnd = setTimeout(endJump, 0); return treadNow(); }
    if (deepHere()) return waterJump(B, hi, t, H, dir, lean, both);
    let bend, arm, lift = 0;
    if (t < c) { const u = ease(t / c); bend = 0.8 * u; arm = 35 * u; }
    else if (t < f) { const u = (t - c) / (f - c); lift = H * 4 * u * (1 - u); bend = 0.8 * (1 - u) * (1 - u) + 0.35 * Math.sin(Math.PI * u); arm = 35 - 150 * Math.sin(Math.PI * Math.min(1, u * 1.6)); }
    else { const u = (t - f) / (l - f); bend = 0.7 * Math.sin(Math.PI * Math.min(1, u * 1.3)); arm = -25 * (1 - u); }
    return { ...both("Leg", () => 40 * bend), ...both("Knee", () => 85 * bend), ...both("Arm", () => arm), ...both("Elbow", () => 15), lift, jump: true, grounded: true };
  }
  if (a === "fall") {
    // (ticket 162) falling into a pit: the arms flung up, reaching as in
    // climbing; the legs a little apart and bent. Held (no frames of its
    // own): the drop moves the drawing, which a new drawing would undo
    const up = side => -80 - (B.arms ?? 0) - lean(side);
    return { ...both("Arm", sd => up(sd) + 18 * sd), ...both("Elbow", () => 22),
             ...both("Leg", sd => 10 * sd), ...both("Knee", () => 26), held: true };
  }
  if (a === "climbup") {
    // (ticket 137) up a ladder that stands in the place (see climb, below),
    // facing it, away from us: the stroke of climbing with a ladder of their own, without
    // one; still while there is no ladder to climb, and holding on, still,
    // at its top (held: no more frames)
    if (!climb || climb.none) return treadNow();
    // (ticket 174, Matthew: the arms "should be taking turns grabbing upward,
    // and pulling the body, but they just wave around instead") Each hand in
    // turn holds a rung while the body rises past it, its height above the
    // shoulder falling from CLIMB_HAND_HI to CLIMB_HAND_LO of the arm's length
    // exactly as far as the body rises, so the hand stays on the rung; then it
    // lets go and reaches up for the next, in a quick arc a little out. The
    // other hand is half a cycle behind: both hold for a moment between. A
    // cycle is reckoned by how far they have climbed (climb.at), not by the
    // clock, so the hands keep to the rungs at any speed, by hand or not, and
    // stop with the climb. The legs step on the same cycle. Each arm's two
    // bones from the shoulder to where its hand is, the elbow out (as posing
    // by touch solves them: poseTo).
    const J = poseJoints, al = J && J.armLen ? J.armLen : 120, half = al / 2, deg = r => r * 180 / Math.PI;
    const rise = (climb.at || 0) / (figureT.k || 1), cyc = (CLIMB_HAND_HI - CLIMB_HAND_LO) * al / CLIMB_PULL, w = ((rise / cyc) % 1 + 1) % 1;
    const hand = sd => {
      const p = (w + (sd > 0 ? 0 : 0.5)) % 1;
      let h, out;
      if (p < CLIMB_PULL) { h = CLIMB_HAND_HI - (CLIMB_HAND_HI - CLIMB_HAND_LO) * p / CLIMB_PULL; out = 0.12; }
      else { const q = (p - CLIMB_PULL) / (1 - CLIMB_PULL), e = q * q * (3 - 2 * q); h = CLIMB_HAND_LO + (CLIMB_HAND_HI - CLIMB_HAND_LO) * e; out = 0.12 + 0.22 * Math.sin(Math.PI * q); }
      // (in the arm's own terms: x out from the body, y down; the hand above the shoulder)
      const dx = out * al, dy = -h * al, d = Math.max(0.2 * half, Math.min(Math.hypot(dx, dy), 2 * half - 0.01));
      const phi = deg(Math.atan2(dy, dx)), g = deg(Math.acos(d / (2 * half)));
      return { up: phi + g, bend: 2 * g };
    };
    const hl = hand(1), hr = hand(-1), s = Math.sin(2 * Math.PI * w);
    return { ArmL: hl.up - (B.arms ?? 0) - lean(1), ArmR: hr.up - (B.arms ?? 0) - lean(-1), ElbowL: hl.bend - (B.elbows ?? 0), ElbowR: hr.bend - (B.elbows ?? 0),
             ...both("Leg", sd => 8 * Math.max(0, sd * s)), ...both("Knee", sd => 55 * Math.max(0, sd * s)), held: climb.manual || climb.done };
  }
  if (a === "climb") {
    // (ticket 137) seen from the side, the ladder in front of them: a hand
    // reaching up and forward for the next rung while the other pulls, the
    // knee on its side coming up to the next rung, the rungs going down
    if (dir) {
      const s = sin(1), up = side => -52 - (B.arms ?? 0) - lean(side);
      return { ...both("Arm", sd => up(sd) - 22 * sd * s), ...both("Elbow", sd => 6 + 14 * Math.max(0, -sd * s)),
               ...both("Leg", sd => 8 + 36 * Math.max(0, sd * s)), ...both("Knee", sd => 16 + 62 * Math.max(0, sd * s)), lift: 5 * Math.abs(s), ladder: u };
    }
    // (facing us on a ladder: a hand reaching up for the next rung while the
    // other pulls, the knee on its side lifting, the rungs going down)
    const s = sin(1), up = side => -80 - (B.arms ?? 0) - lean(side);
    return { ...both("Arm", sd => up(sd) + 30 * sd * s), ...both("Elbow", sd => 20 + 30 * Math.max(0, -sd * s)),
             ...both("Leg", sd => 8 * Math.max(0, sd * s)), ...both("Knee", sd => 55 * Math.max(0, sd * s)), lift: 5 * Math.abs(s), ladder: u };
  }
  return treadNow();
}
// The friend standing in water, in their own units: what was drawn of them
// (`inner`) see-through below the waterline, their waist (wl), as much as
// "🌊 see-through in the water" says; and a wake round them on the water as
// big as its slider says: rings spreading and fading, as at the foot of the
// falls, and foam going round where the water meets them, each ring's far
// half behind them and its near half in front. The waterline is a gentle
// wave, and still: a moving mask would draw all of them again every frame.
// The rings keep fixed times (constant begins), so drawing the friend again,
// as every move of a drag does, never jumps them. hw: their half width at
// the waist. With both settings at 0, nothing changes.
function wading(B, inner, wl, hw, most = Infinity, bob = 0, water = "") {
  const see = Math.max(0, Math.min(100, Number(B.waterSee ?? 70) || 0)) / 100, wake = Math.max(0, Number(B.waterWake ?? 100) || 0) / 100;
  // (treading, the friend bobs in the water: they move, the waterline doesn't)
  // (ticket 143: or a jump in the water, a whole transform)
  let svg = typeof bob === "string" ? `<g transform="${bob}">${inner}</g>` : bob ? `<g transform="translate(0 ${f1(bob)})">${inner}</g>` : inner;
  // (white above the waterline, a bow each 40 or so across, 4 up or down;
  // below it, white only where the water is in the picture (`water`, its
  // shape round them), and faint, so they are faint there and hidden where
  // the shore, the sand or the grass is in front of them)
  const X = hw + 80, n = 2 * Math.ceil(X / 40), st = 2 * X / n;
  let line = `M-5000 ${f1(wl)} H${f1(-X)}`;
  for (let i = 0; i < n; i++) line += ` q${f1(st / 2)} ${i % 2 ? 8 : -8} ${f1(st)} 0`;
  // (ticket 151) Two clipped copies, not a mask: the friend whole above the
  // wave, and below it, only where the water is, faint (1 - see). A mask is
  // drawn again offscreen, pixel by pixel, on every frame the friend moves:
  // walking in the castle's moat at 1920 wide it held the page to 49 frames
  // a second, and swimming in the duck pond to 52 (59 and 60 without it).
  const body = svg;
  svg = `<clipPath id="water-above"><path d="${line} H5000 V-5000 H-5000 Z"/></clipPath><g clip-path="url(#water-above)">${body}</g>` +
        (see < 1 && !sceneryStill() && !SPEED_TEST.noUnder ? `<clipPath id="water-below"><path d="${line} H5000 V5000 H-5000 Z"/></clipPath><clipPath id="water-in">${water}</clipPath>` +
                   `<g clip-path="url(#water-below)"><g clip-path="url(#water-in)"${see > 0 ? ` opacity="${(1 - see).toFixed(2)}"` : ""}>${body}</g></g>` : "");
  if (wake <= 0 || SPEED_TEST.noRings) return svg;
  const stillWake = sceneryStill();   // (ticket 164: on a slow computer, the rings held still)
  // (three rings a third of the way apart, as flat as the pool's; the foam
  // in dashes, going round)
  const r0 = hw + 12, r1 = Math.max(r0 + 20, Math.min(r0 + 220 * wake, most)), flat = 0.16;
  const ring = begin => { const t = `dur="2.4s" begin="${begin}" repeatCount="indefinite"`;
    return `<ellipse cx="0" cy="${f1(wl)}" rx="${f1(r0)}" ry="${f1(r0 * flat)}" fill="none" stroke="#ffffff" stroke-width="4" opacity="0.8">` +
      `<animate attributeName="rx" values="${f1(r0)};${f1(r1)}" ${t}/><animate attributeName="ry" values="${f1(r0 * flat)};${f1(r1 * flat)}" ${t}/>` +
      `<animate attributeName="opacity" values="0.8;0" ${t}/></ellipse>`; };
  const rf = hw + 6, rings = `<ellipse cx="0" cy="${f1(wl)}" rx="${f1(rf)}" ry="${f1(rf * flat)}" fill="none" stroke="#ffffff" stroke-width="7" stroke-dasharray="9 7" stroke-linecap="round" opacity="0.75">` +
    `<animate attributeName="stroke-dashoffset" values="0;-32" dur="1.2s" begin="0s" repeatCount="indefinite"/></ellipse>` +
    (stillWake ? "" : ["0s", "-0.8s", "-1.6s"].map(ring).join(""));   // (still: the foam only, without its dashes going round)
  const ringsHere = stillWake ? rings.replace(ANIMS_RE, "") : rings;
  // (the rings are on the water: only where it is, as the friend's under-water part)
  return `<clipPath id="water-far"><rect x="-5000" y="-5000" width="10000" height="${f1(wl + 5000)}"/></clipPath>` +
         `<clipPath id="water-near"><rect x="-5000" y="${f1(wl)}" width="10000" height="5000"/></clipPath>` +
         `<clipPath id="water-area">${water}</clipPath>` +
         `<g clip-path="url(#water-far)"><g clip-path="url(#water-area)">${ringsHere}</g></g>${svg}` +
         `<g clip-path="url(#water-near)"><g clip-path="url(#water-area)">${ringsHere}</g></g>`;
}

// A scene's options, as it reads them: its background's settings, and, as
// o["lfo:A"] (B, C), what it shows of an LFO: its length, shape and depth, and
// where its wave is. The mixer's LFOs are not background settings, so a scene
// following one reads it this way, and a change to that LFO on the mixing
// board draws that scene again (partByReads), and no other.
// (ticket 242: and "tree:busy", what a tree room measured standing before its wall, so a new measure builds the scene again)
// (ticket 338: "fallen:cover", the pits a fallen rock lies on, as one string: the scene's cache compares what it read by identity,
// and a rock dragged within the place's things is the same array)
const sceneOpts = BG => new Proxy(BG, { get: (t, key) => typeof key === "string" && key.startsWith("lfo:") ? lfoStamp(key.slice(4)) : key === "tree:busy" ? treeBusy.key : key === "fallen:cover" ? fallenCover(t) : key === "raster" ? (stillAt === null ? RASTER_GEN : null) : t[key] });   // (ticket 509: "raster", in play only)
const lfoStamp = L => { const lf = lfoNow(L); return lf ? [lf.dur.toFixed(4), lf.shape, lf.amp.toFixed(2), lfoBegin(lf, 0)].join() : ""; };
// (ticket 121) The friend posed limb by limb: each arm's own turn at the
// shoulder and bend at the elbow, each leg's at the hip and knee, in
// degrees, added to the sliders' (which move both alike). By the friend's
// own side: L is their left, on our right (side 1); R their right (side -1).
// (ticket 124) which way the friend faces: 0 toward us, -1 in profile to our
// left, 1 to our right
const facingOf = B => B.facing === "left" ? -1 : B.facing === "right" ? 1 : 0;
// (ticket 144) facing us, turned partway: -1 to our left, 1 to our right, 0
// not turned. The cheap three-quarter view: the drawing facing us, the
// body narrower, the far arm behind it, the face's features slid the way
// they turn. Only facing us; set by the controller's diagonals toward us,
// and cleared wherever the facing is set otherwise.
const turnOf = B => B.facing !== "front" ? 0 : B.turn === "left" ? -1 : B.turn === "right" ? 1 : 0;
const POSE_KEYS = ["poseArmL", "poseArmR", "poseElbowL", "poseElbowR", "poseLegL", "poseLegR", "poseKneeL", "poseKneeR", "poseFootL", "poseFootR"];
// (ticket 129) the pose menu: whole poses for the friend, each the pose's
// own turns and bends (degrees, by side: L their left, on our right),
// with what else the pose needs. Standing is all of them zero.
const POSES = {
  // cross-legged on the ground, hands in the lap, eyes closed: each thigh
  // out and a little down, each shin back across in front of the other,
  // the feet turned soles up
  meditate: { pose: { poseLegL: 72, poseLegR: 72, poseKneeL: 158, poseKneeR: 150, poseFootL: 70, poseFootR: 70,
                      poseArmL: 30, poseArmR: 30, poseElbowL: -90, poseElbowR: -90 }, closed: true },
  // dances, one picked at random each time, never the same twice in a row
  // (ticket 136) jumping up into a hug, held up off the ground: the legs as
  // sitting (thighs forward, shins down), so in profile they wrap round the
  // one hugged, the near leg in front of them and the far one behind (see
  // legApart in draw); the arms reach forward round their neck
  hug: { pose: { poseArmL: -40, poseArmR: -34, poseElbowL: 40, poseElbowR: 34, poseKneeL: 12, poseKneeR: 12 } },
  dance: [
    { name: "disco", pose: { poseArmL: -105, poseElbowL: 10, poseArmR: 40, poseElbowR: 95, poseLegR: 28, poseKneeR: 40 } },
    { name: "ballet", pose: { poseArmL: -150, poseElbowL: -35, poseArmR: -45, poseElbowR: 15, poseLegL: 55, poseKneeL: 95, poseFootL: 40 } },
    { name: "hooray", pose: { poseArmL: -135, poseElbowL: -10, poseArmR: -135, poseElbowR: -10, poseLegL: 18, poseLegR: 18 } },
    { name: "robot", pose: { poseArmL: -55, poseElbowL: 90, poseArmR: -55, poseElbowR: -90, poseKneeL: 35, poseKneeR: 5 } },
    { name: "kick", pose: { poseArmL: -60, poseElbowL: 60, poseArmR: -60, poseElbowR: 60, poseLegR: 80, poseKneeR: 10, poseFootR: 30 } },
  ],
};
let lastDance = null;
function strikePose(which) {
  const B = state.body, F = state.face;
  for (const k of POSE_KEYS) B[k] = 0;
  B.pose = which; B.sit = which === "sit";
  // (standing and sitting keep the way they face; the others face us: ticket 132)
  if (which === "stand" || which === "sit") { F.closed = false; return; }
  // (a hug is seen from the side: facing us, they turn to our right)
  if (which === "hug") { F.closed = false; if (!facingOf(B)) B.facing = "right"; Object.assign(B, POSES.hug.pose); return; }
  // (ticket 299) lying down, the body level, its head the way lieHead says (turned about the hips as the crawl turns them:
  // drawNow), the arms along its sides, the legs straight. (Matthew: "this laying looks valid, but its 'laying on one's
  // side' not facing upwards, like is expected. Both are valuable.") On the back, face up (lieHow "back", the usual): the
  // friend from the side, facing away from where the head goes, so turned level the face looks up; on the side, facing us.
  if (which === "lie") { F.closed = false; B.turn = "";
    if (B.lieHow === "side") { B.facing = "front"; B.poseArmL = B.poseArmR = 80 - (B.arms ?? 0); return; }
    B.facing = B.lieHead === "right" ? "left" : "right";
    const d = facingOf(B);   // (in profile each arm leans its own way: straight down along the body, as turned)
    B.poseArmL = 84 - (B.arms ?? 0) - (d === 1 ? -14 : 4); B.poseArmR = 84 - (B.arms ?? 0) - (d === -1 ? -14 : 4); return; }
  B.facing = "front"; B.turn = "";
  let P = POSES[which];
  if (Array.isArray(P)) { const left = P.filter(d => d.name !== lastDance); P = left[Math.floor(Math.random() * left.length)]; lastDance = P.name; }
  Object.assign(B, P.pose);
  F.closed = !!P.closed;
}
const poseOf = (B, what, side) => Number(B["pose" + what + (side > 0 ? "L" : "R")]) || 0;
const posed = B => POSE_KEYS.some(k => Number(B[k]));
// where each joint was drawn, in the figure's units, for grabbing them
// (draw fills it in): shoulders, elbows, hands; hips, knees, feet, by side
let poseJoints = null;
// (ticket 492) the friend's horse as last drawn, for its collision boxes (horseBoxes): its view, ridden or not, where it
// stands (X, in the friend's units), which way (k), its size (hs), its hooves and barrel (horseSvg's) and its seat's
// height; null with no horse drawn
let horseNow = null;
// while a joint is held (the drag below): which, and the ground the friend
// stood on when it was grabbed, kept till it is let go, so the friend stays
// put under the finger even as the lower foot changes
let poseHeld = null;
// (ticket 123) the treading friend's own redraws: the next frame asked for, and when the last was drawn
let treadLoop = 0, treadLast = 0;
// (ticket 164) Is this a slow computer? The time a drawing takes, on
// average (a drawing that built the place anew leaves it out: that says
// nothing about the computer): over SLOW_IN ms, after 30 drawings, it is
// slow, and under SLOW_OUT it is not again (so it does not flip back and
// forth). Kept in this browser ("dress-up-slow"), so a slow computer starts
// still next time, and is measured again. On a slow computer the scenery's
// animations, the wake and the faint copy under the water hold still
// (sceneryStill), unless 🌀 moving scenery on a slow computer is on.
const SLOW_IN = 6, SLOW_OUT = 2.5;
let slowAvg = 0, slowN = 0, slowFlag = false;
// (ticket 510) A place whose still picture keeps needing new parts is given up on: its scenery moves there for the rest of
// the visit to the game (a reload measures afresh), each time the friend is in it. Each part is the whole place drawn into a screen-size picture; walking past many objects' depths, the fairy
// forest made 20 to 40 of them in a 4 s walk and walked at 50 frames a second, where live it walks at 60 on the same computers.
// More than STILL_CHURN new parts within STILL_CHURN_MS gives it up, but only where the place drawn live is cheap here
// (liveDrawAvg, its drawings' average while not held still, under STILL_GIVE_UP_MS): on a truly slow computer (the black
// laptop's pond: 30 frames a second live, 60 still) the still picture is the point, churn or not. 🧪's passes
// (SPEED_TEST.still) are never given up.
const STILL_CHURN = 10, STILL_CHURN_MS = 4000, STILL_GIVE_UP_MS = 9;
let liveDrawAvg = 0;
const stillGaveUp = new Set();
let stillMade = [];
const stillPlace = () => { const B = state.background; return [B.kind, B.castle, B.castleRoom, B.inside, B.houses, state.world && state.world.at].join("/"); };
try { slowFlag = localStorage.getItem("dress-up-slow") === "1"; } catch (e) { /* (measured afresh) */ }
// (ticket 164) Or frames dropping while the friend moves: the black laptop
// draws the friend in 1 ms, as fast as Matthew's laptop, yet the duck pond
// was choppy there. Over each CHOPPY_FRAMES (60) frames of movement (moved(), the
// controller's steps and a drag's frames; a gap over 250 ms is a pause,
// not a frame), more than CHOPPY_LATE of them later than 25 ms, in
// CHOPPY_WINDOWS such windows in a row, makes this computer choppy (the
// black laptop's duck pond: 62 to 100% late; a fast one has 0 to 9%), and
// it stays so for the visit (0.0.93: 60 frames a window, not 120: at the
// iPad's 22 frames a second two windows of 120 took 11 s of moving before
// the places were held still, on every visit): with the places held still its frames come
// back, and it would only flip back and forth. Not across visits (0.0.92,
// ticket 172): kept in the browser, one bad moment held Matthew's laptop
// still on every visit after (its 🧪 on 0.0.91: 57 to 61 frames a second
// in every pass, yet found choppy); each visit measures afresh. A trip
// through a doorway does not count: its arrival's frame is long by nature.
// (ticket 382, ^du_report(12)'s next step 3: "the slow tier's first walk in the room ran at 5 to 12 frames a second before the game
// held the scenery still; find out sooner (from the first frames) that a computer is slow") Each walk's first frames decide a plainly
// slow computer at once: after CHOPPY_WARM frames (a walk's first frames are long by nature: the step's start, a part drawn new),
// CHOPPY_EARLY more of which at least CHOPPY_EARLY_LATE are late make it choppy then; one that is only sometimes late is still found
// by the two windows above. On a fast computer a walk's first frames are on time.
const CHOPPY_LATE = 0.25, CHOPPY_WINDOWS = 2, CHOPPY_FRAMES = 60, CHOPPY_WARM = 3, CHOPPY_EARLY = 12, CHOPPY_EARLY_LATE = 0.75;
let choppy = false, moveFrameLast = 0, moveFrames = 0, moveLate = 0, moveSuspend = false, choppyRun = 0, walkFrames = 0, walkLate = 0;
try { localStorage.removeItem("dress-up-choppy"); } catch (e) { /* (nothing kept) */ }
function noteMoveFrame(t) {
  if (moveSuspend || entering) return;   // (🧪 the speed check; a trip)
  const g = t - moveFrameLast;
  moveFrameLast = t;
  if (g <= 0 || g > 250) { walkFrames = walkLate = 0; return; }   // (a pause: the next frames start a new walk)
  moveFrames++;
  if (g > 25) moveLate++;
  if (++walkFrames > CHOPPY_WARM && walkFrames <= CHOPPY_WARM + CHOPPY_EARLY) {
    if (g > 25) walkLate++;
    if (!choppy && walkFrames === CHOPPY_WARM + CHOPPY_EARLY && walkLate >= CHOPPY_EARLY_LATE * CHOPPY_EARLY) { choppy = true; setTimeout(changed, 0); }
  }
  if (moveFrames < CHOPPY_FRAMES) return;
  choppyRun = moveLate > CHOPPY_LATE * moveFrames ? choppyRun + 1 : 0;
  if (!choppy && choppyRun >= CHOPPY_WINDOWS) { choppy = true; setTimeout(changed, 0); }
  moveFrames = moveLate = 0;
}
// (SPEED_TEST.still: 🧪's passes held still; slowFlag would not do, as draw()
// takes it back after one drawing on a computer that draws fast: 0.0.92)
// (ticket 274) Held still (holdStill), never: a held picture is the full scene, whatever this computer's speed, so
// the settings alone decide it.
const sceneryStill = () => (SPEED_TEST.still || (stillAt === null && (slowFlag || choppy) && !stillGaveUp.has(stillPlace()))) && !PERF.slowMoving;
// (ticket 164, 0.0.86) 🧪's test passes, each taking one part of a place
// away while the friend swims, to find what costs the black laptop its
// frames: holder, a group drawn instead of the place's scenery (empty, or
// the place as a still picture), with the friend in front of it all;
// noUnder, no faint copy of the friend under the water; noRings, no rings;
// (0.0.87) noPic, no still picture (below); bitmap, the still picture as a
// bitmap. Only the speed check sets them, and it puts them back.
const SPEED_TEST = { holder: null, noUnder: false, noRings: false, noPic: false, svg: false, still: false };
// (ticket 164, 0.0.87) On a slow computer, while the friend moves, the
// place is drawn as still pictures. The black laptop's 🧪 (0.0.86) swam the
// duck pond at 30 frames a second as it is, and at 59 with its scenery one
// still picture: Firefox paints a picture once instead of its ~1,000 shapes
// every frame. The scene is cut where the friend goes in it (before the
// first standing object nearer than their feet, and their far arm's place
// before that: see draw), and each part is one picture, so the friend stays
// behind and in front of the same things as ever. The scene itself stays,
// hidden, so doorways, ladders and the ✨ glow measure it as before (a copy
// of the glow is shown over its picture). The pictures are made before they
// are needed, for the place and the friend's depth, and again when the
// friend goes past an object's depth; until they are ready the scene is
// drawn as it is. STILL_PIC_HOLD ms after the last move the scene itself is
// shown again, where clicks reach it. Since 0.0.93 each part is a bitmap
// (drawn once into a canvas at the screen's own pixels): the iPad's 🧪 on
// 0.0.92 swam the pond at 24 frames a second with the parts as svg pictures
// (Safari draws an svg picture again every frame) and at 60 as bitmaps; an
// svg picture is the fallback where a bitmap can't be made (bitmapOk).
const STILL_PIC_HOLD = 400, STILL_PARTS = 6, STILL_RES = "defs, linearGradient, radialGradient, pattern, clipPath, mask, filter, symbol, marker, style";
const stillPic = { gen: 0, node: null, text: null, vb: null, bitmap: false, off: "", parts: new Map(), shown: [], on: false, onNode: null, glow: null, glowOf: null, bitmapOk: null, drawsOn: 0, draws: 0,
  painted: new WeakSet(), sceneHidden: false };   // (ticket 177: the pictures that have had a frame to paint; whether the scene is hidden)
let stillPicUntil = 0, stillPicEnd = 0;
const stillPicWanted = () => !!bgShown.node && sceneryStill() && !SPEED_TEST.holder && !SPEED_TEST.noPic && !(typeof SWINGS !== "undefined" && SWINGS.size);   // (not while a swing runs: ticket 349's blink)
// (the parts' pictures, kept while the place, its frame and its hidden objects stay as they are)
function stillPicReset() {
  const vb = pic.getAttribute("viewBox"), off = [...objectsOff(state.background)].join(",");
  // (the picture's size on the screen too, for a bitmap's pixels: in steps of 32 px)
  const bitmap = !SPEED_TEST.svg, size = bitmap ? Math.round(pic.getBoundingClientRect().width * (devicePixelRatio || 1) / 32) : 0;
  if (stillPic.node === bgShown.node && stillPic.text === bgShown.text && stillPic.vb === vb && stillPic.bitmap === bitmap && stillPic.size === size && stillPic.off === off) return;
  showStillPic(null);
  for (const p of stillPic.parts.values()) { if (p.img) p.img.remove(); if (p.url) URL.revokeObjectURL(p.url); }
  Object.assign(stillPic, { gen: stillPic.gen + 1, node: bgShown.node, text: bgShown.text, vb, bitmap, size, off, parts: new Map(), shown: [], painted: new WeakSet() });
}
// a part's picture: what the scene draws from just after object lo - 1 (from
// its start, for 0) to just before object hi (to its end, for the last),
// counting bgShown.objs in their drawing order
function stillPart(lo, hi) {
  const key = lo + ":" + hi;
  let p = stillPic.parts.get(key);
  if (p) return p;
  p = { lo, hi, img: null, url: null, ready: false, used: performance.now() };
  stillPic.parts.set(key, p);
  // (ticket 510: too many new parts too fast here, and the place's scenery moves again)
  const t = p.used;
  stillMade = stillMade.filter(x => t - x < STILL_CHURN_MS); stillMade.push(t);
  if (stillMade.length > STILL_CHURN && !SPEED_TEST.still && liveDrawAvg < STILL_GIVE_UP_MS) { stillGaveUp.add(stillPlace()); stillMade = []; setTimeout(() => moved(true), 0); }
  const gen = stillPic.gen, node = stillPic.node, bitmap = stillPic.bitmap, vb = stillPic.vb, NS = "http://www.w3.org/2000/svg";
  setTimeout(async () => {
    if (gen !== stillPic.gen || !node.isConnected || stillPic.parts.get(key) !== p) return;
    const objs = bgShown.objs;
    objs.forEach((o, i) => o.node.setAttribute("data-still-obj", i));
    for (const n of figShown) if (n.setAttribute) n.setAttribute("data-still-out", "");
    const copy = node.cloneNode(true);
    objs.forEach(o => o.node.removeAttribute("data-still-obj"));
    for (const n of figShown) if (n.removeAttribute) n.removeAttribute("data-still-out");
    copy.removeAttribute("style");
    for (const n of copy.querySelectorAll("[data-still-out], [data-still-pic], .enter-glow")) n.remove();
    const full = copy.cloneNode(true), defs = document.createElementNS(NS, "defs");
    // (pos: 0 before the first object, k + 1 from object k on)
    let pos = 0;
    const walk = el => {
      for (const c of [...el.children]) {
        if (c.matches(STILL_RES)) continue;
        const k = c.getAttribute("data-still-obj");
        if (k !== null) pos = +k + 1;
        else if (c.querySelector("[data-still-obj]")) { walk(c); continue; }   // (a group with objects in it, part by part)
        if (pos < lo || pos > hi) { for (const r of c.querySelectorAll(STILL_RES)) defs.append(r); c.remove(); }
      }
    };
    walk(copy);
    // (a shape drawn again elsewhere by <use>: kept for it, if the part left it out)
    for (const u of copy.querySelectorAll("use")) {
      const id = (u.getAttribute("href") || u.getAttributeNS("http://www.w3.org/1999/xlink", "href") || "").replace(/^#/, "");
      if (!id || copy.querySelector(`[id="${CSS.escape(id)}"]`) || defs.querySelector(`[id="${CSS.escape(id)}"]`)) continue;
      const t = full.querySelector(`[id="${CSS.escape(id)}"]`);
      if (t) defs.append(t.cloneNode(true));
    }
    copy.prepend(defs);
    // (the picture's frame and 3% more round it: a scene may draw a sliver past the frame, at its edges)
    const [x, y, w, h] = (([x, y, w, h]) => [x - 0.03 * w, y - 0.03 * h, 1.06 * w, 1.06 * h])(vb.split(" ").map(Number));
    const doc = `<svg xmlns="${NS}" xmlns:xlink="http://www.w3.org/1999/xlink" width="${f1(w)}" height="${f1(h)}" viewBox="${f1(x)} ${f1(y)} ${f1(w)} ${f1(h)}">` +
                new XMLSerializer().serializeToString(copy) + `</svg>`;
    let url = URL.createObjectURL(new Blob([doc], { type: "image/svg+xml" }));
    if (bitmap) {
      // (drawn once into pixels at this screen's size: see above)
      try {
        const im = new Image(); im.src = url; await im.decode();
        // (pixels per unit as the screen shows the picture: its frame, without the 3% round it, across its box)
        const r = pic.getBoundingClientRect(), k = Math.min(r.width / (w / 1.06), r.height / (h / 1.06)) * (devicePixelRatio || 1);
        const c = document.createElement("canvas"); c.width = Math.max(1, Math.round(w * k)); c.height = Math.max(1, Math.round(h * k));
        c.getContext("2d").drawImage(im, 0, 0, c.width, c.height);
        const blob = await new Promise(res => c.toBlob(res, "image/png"));
        if (!blob) throw new Error("no bitmap");
        URL.revokeObjectURL(url); url = URL.createObjectURL(blob); stillPic.bitmapOk = true;
      } catch (e) { stillPic.bitmapOk = false; }
    }
    if (gen !== stillPic.gen || !node.isConnected || stillPic.parts.get(key) !== p) { URL.revokeObjectURL(url); return; }   // (a new place, or this part let go)
    const img = document.createElementNS(NS, "image");
    for (const [k, v] of [["x", x], ["y", y], ["width", w], ["height", h]]) img.setAttribute(k, f1(v));
    img.setAttribute("preserveAspectRatio", "none"); img.setAttribute("pointer-events", "none"); img.setAttribute("data-still-pic", "");
    img.style.visibility = "hidden";
    // (ticket 177: ready only once its pixels are decoded, not just loaded: a
    // picture shown before then paints nothing for a frame, and with the scene
    // hidden behind it that frame is the page's white, a flash while moving.
    // Decoded twice over, as browsers differ: the element's own decode() where
    // it has one, an Image of the same URL kept beside it, and decoding="sync"
    // so a picture not yet decoded is painted late, never blank)
    img.setAttribute("decoding", "sync");
    const pre = new Image();
    const loaded = new Promise(res => img.addEventListener("load", res, { once: true }));
    img.setAttribute("href", url);
    pre.src = url;
    // (0.0.97: the element's own decode() waits at most a quarter second. In
    // Chrome it never settled, for an <image> not yet in the page, so no
    // part was ever ready, and a slow computer drew its place live: the
    // gate's still-pictures case, 0% of drawings with them in 0.0.95 and
    // 0.0.96, 99% before. The kept Image's decode is the one that counts.)
    const own = img.decode ? Promise.race([img.decode().catch(() => {}), new Promise(r => setTimeout(r, 250))]) : null;
    // (0.0.116, Matthew: find why the still pictures never switch on in
    // Chrome. stills164 on GitHub: both decodes finish, but Chrome never
    // fires the <image>'s load, not in 3 s, so no part was ever ready: the
    // gate's 0% since 0.0.95. Its load waits at most a quarter second too;
    // the kept Image's decode, which does finish, still counts)
    const loadedOrNot = Promise.race([loaded, new Promise(r => setTimeout(r, 250))]);
    Promise.all([loadedOrNot, own, pre.decode().catch(() => {})])
      .then(() => { if (gen === stillPic.gen && stillPic.parts.get(key) === p) { p.pre = pre; p.ready = true; } });
    node.append(img);   // (hidden, at the scene's end, until wanted)
    Object.assign(p, { img, url });
  }, 0);
  return p;
}
// (from draw, before the friend is put in: the parts for where the friend
// goes, behind (their far arm's object) and ahead (their own), made if they
// are not yet; while the friend moves and all are ready, the parts, in their
// drawing order at the scene's end)
function stillLayers(farNode, behind, ahead) {
  if (!stillPicWanted()) return null;
  stillPicReset();
  const N = bgShown.objs.length, at = n => { const i = n ? bgShown.objs.findIndex(o => o.node === n) : -1; return i < 0 ? N : i; };
  const r = at(ahead), f = farNode ? at(behind) : r;
  const parts = [stillPart(0, f), f < r ? stillPart(f + 1, r) : null, r < N ? stillPart(r + 1, N) : null];
  // (at most STILL_PARTS kept: each is a picture the size of the screen in memory; the longest unused goes)
  const now = performance.now();
  for (const q of parts) if (q) q.used = now;
  if (stillPic.parts.size > STILL_PARTS) {
    const old = [...stillPic.parts.entries()].filter(([, q]) => !parts.includes(q)).sort((a, b) => a[1].used - b[1].used);
    for (const [k, q] of old.slice(0, stillPic.parts.size - STILL_PARTS)) {
      if (q.img) q.img.remove(); if (q.url) URL.revokeObjectURL(q.url);
      stillPic.parts.delete(k); stillPic.shown = stillPic.shown.filter(m => m !== q.img);
    }
  }
  if (!(performance.now() < stillPicUntil && !poseHeld && !entering && parts.every(q => !q || q.ready))) return null;
  const imgs = parts.filter(Boolean).map(q => q.img);
  if (imgs.some((m, i) => i && !(imgs[i - 1].compareDocumentPosition(m) & Node.DOCUMENT_POSITION_FOLLOWING))) bgShown.node.append(...imgs);
  return parts;
}
// the pictures on (the scene hidden, the friend and the glow's copy shown)
// or off, written only when it changes
function showStillPic(parts) {
  const node = bgShown.node;
  if (stillPic.on && stillPic.onNode !== node) { stillPic.on = false; stillPic.shown = []; stillPic.sceneHidden = false; }
  const want = parts ? parts.filter(Boolean).map(q => q.img) : [];
  if (want.length !== stillPic.shown.length || want.some((m, i) => m !== stillPic.shown[i])) {
    for (const m of stillPic.shown) if (!want.includes(m)) m.style.visibility = "hidden";
    for (const m of want) m.style.visibility = "visible";
    stillPic.shown = want;
  }
  const on = !!parts;
  // (ticket 177) a picture shown for the first time may paint a frame late,
  // on a browser that decodes it only then: the scene stays visible under it
  // for that frame, and is hidden two frames on, so a late picture shows the
  // scene, never the page's colour (a white flash while moving)
  const fresh = on && want.some(m => !stillPic.painted.has(m));
  if (node && (on !== stillPic.on || stillPic.sceneHidden !== (on && !fresh))) {
    node.style.visibility = on && !fresh ? "hidden" : "";
    stillPic.sceneHidden = on && !fresh; stillPic.on = on; stillPic.onNode = node;
  }
  if (fresh) {
    const shownNow = stillPic.shown;
    requestAnimationFrame(() => requestAnimationFrame(() => {
      for (const m of shownNow) stillPic.painted.add(m);
      if (stillPic.on && stillPic.shown === shownNow && stillPic.onNode === node && node) { node.style.visibility = "hidden"; stillPic.sceneHidden = true; }
    }));
  }
  if (on) for (const n of figShown) if (n.style) n.style.visibility = "visible";
  // (the ✨ glow on a picture to go into: a copy over the part its picture is in)
  const g = on && enterGlow && enterGlow.isConnected ? enterGlow : null;
  if (stillPic.glow && (!g || stillPic.glowOf !== g)) { stillPic.glow.remove(); stillPic.glow = stillPic.glowOf = null; }
  if (g) {
    if (!stillPic.glow) {
      const m = pic.getScreenCTM().inverse().multiply(g.getScreenCTM()), w = document.createElementNS("http://www.w3.org/2000/svg", "g");
      w.setAttribute("transform", `matrix(${[m.a, m.b, m.c, m.d, m.e, m.f].map(v => +v.toFixed(4)).join(" ")})`);
      w.setAttribute("data-still-pic", ""); w.setAttribute("pointer-events", "none"); w.style.visibility = "visible";
      w.append(g.cloneNode(true));
      Object.assign(stillPic, { glow: w, glowOf: g });
    }
    let k = 0;
    for (const o of bgShown.objs) { if (o.node === g || o.node.contains(g)) { k++; break; } if (g.compareDocumentPosition(o.node) & Node.DOCUMENT_POSITION_FOLLOWING) break; k++; }
    const q = parts.find(q => q && q.lo <= k && k <= q.hi);
    if (q && stillPic.glow.previousSibling !== q.img) q.img.after(stillPic.glow);
  }
}
// (from moved(): the pictures hold while moves keep coming; after the last,
// one more drawing shows the scene itself)
function stillPicMoved(t) {
  stillPicUntil = t + STILL_PIC_HOLD;
  if (!stillPic.on && !stillPic.parts.size) return;
  clearTimeout(stillPicEnd);
  stillPicEnd = setTimeout(() => { stillPicEnd = 0; if (stillPic.on && performance.now() >= stillPicUntil) draw(); }, STILL_PIC_HOLD + 50);
}
