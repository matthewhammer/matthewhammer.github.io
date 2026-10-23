"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 16 of 26 of the game's script, run in order by index.html.

// each group's objects put in order by depth, in the places they held (a
// picture of a place, or the view out of a window, is in the same order)
function sortObjects(root) {
  const objs = [...root.querySelectorAll("[data-obj]")];
  for (const parent of new Set(objs.map(n => n.parentNode))) {
    const here = objs.filter(n => n.parentNode === parent), sorted = here.slice().sort((a, b) => (objDepth(a) - objDepth(b)) || 0);   // (two in front tie, in their text order)
    if (sorted.every((n, i) => n === here[i])) continue;
    const slots = here.map(n => { const m = n.ownerDocument.createComment(""); n.before(m); return m; });
    sorted.forEach((n, i) => slots[i].replaceWith(n));
  }
}
function sceneObjects(root) {
  sortObjects(root);
  return [...root.querySelectorAll("[data-obj]")].filter(n => {
    for (let q = n.parentNode; q && q !== root; q = q.parentNode)
      if (q.hasAttribute("transform") || q.hasAttribute("data-obj") || q.hasAttribute("data-drag") || q.classList.contains("portal")) return false;
    return true;
  }).map(n => ({ name: n.dataset.obj, z: objDepth(n), node: n, layer: n.dataset.layer || null }));   // (ticket 284: wall, floor, or standing)
}
const objectAhead = feet => (bgShown.objs.find(o => o.z > feet) || {}).node || null;
// (ticket 335) the tree house scene's houses, highest first, each marked with its platform's height on the screen: the first
// lower than y (by more than a few units: a bridge meets a platform level with it), before which a friend up at y is drawn
function houseAhead(y) {
  if (state.background.kind !== "treehouse") return null;
  const k = (SCENE.bottom - SCENE.top) / 700;
  for (const o of bgShown.objs) { if (!/^tree house \d$/.test(o.name)) continue; const f = o.node.querySelector("[data-floor]"); if (f && +f.dataset.floor > y + 6 * k) return o.node; }
  return null;
}
// which list of hidden objects a place keeps: a castle room's by its style,
// a room in the tree house by its number, any other by its kind
function objPlace(B) {
  if (B.kind === "castleroom") return "castleroom:" + (castleOf(B).rooms[Math.round(B.castleRoom ?? 1) - 1] ?? "king");
  if (B.kind === "treeroom") return "treeroom:" + Math.round(B.inside ?? 1);
  return B.kind;
}
const objectsOff = B => new Set(((B.objectsOff || {})[objPlace(B)]) || []);
function showObjects() {
  const off = objectsOff(state.background);
  // (ticket 163: written only when it changes; every drawing, each write
  // made the browser restyle that object, every frame of a walk)
  // (ticket 335: the tree house hidden hides its houses too, objects of their own since)
  for (const o of bgShown.objs) { const d = off.has(o.name) || (off.has("tree house") && /^tree house \d$/.test(o.name)) ? "none" : ""; if (o.node.style.display !== d) o.node.style.display = d; }
  if (foamLayers.on) foamLayersShow();   // (ticket 383: a foam surface's moving bubbles with it)
}
// the scene's element, its pieces those of the last scene where their text
// is the same (see SEP), the rest parsed
function mountScene(text) {
  const tMount = performance.now();   // (⏱️ a trip being timed)
  const NS = "http://www.w3.org/2000/svg", root = document.createElementNS(NS, "g"), stack = [root], kept = new Map();
  root.id = "bg";
  bgShown.parsed = 0;
  for (const piece of text.split(SEP)) {
    if (!piece) continue;
    if (piece === "</g>") { stack.pop(); continue; }
    let node;
    if (/^<g [^>]*>$/.test(piece)) {   // a group's own tag, made new (it is small)
      const holder = document.createElementNS(NS, "g");
      holder.innerHTML = piece + "</g>";
      node = holder.firstChild;
      stack[stack.length - 1].append(node);
      stack.push(node);
      continue;
    }
    node = bgShown.kept.get(piece);
    if (!node || kept.has(piece)) {
      node = document.createElementNS(NS, "g");
      node.innerHTML = piece;
      bgShown.parsed += piece.length;
    }
    kept.set(piece, node);
    stack[stack.length - 1].append(node);
  }
  bgShown.kept = kept;
  if (tripTimer) tripWork("building the place", tMount);
  // (ticket 322) a lever's handle or a lid swinging when the room is built again: carried on in it at once
  if (typeof SWINGS !== "undefined") for (const sw of SWINGS.values()) sw.redo(root);
  return root;
}
// the figure's size and place in its scene (k about its feet, at y = g),
// and the scene's frame: for dragging the figure, and the face in it
// A background's stage: the frame the usual figure got in a scene, its
// ground at 320 (the usual figure's), in picture units.
const SCENE = { left: -405.9, top: -195, bottom: 406.2, ground: 320 };
// the frame the figure itself needs (before a stage replaces it), for
// fitting it to the stage
let naturalFrame = { left: -200, top: -200, ground: 0 };
let figureT = { k: 1, dx: 0, dy: 0, g: 0, sg: 0 }, sceneFrame = { left: -200, top: -200, right: 200, bottom: 200, ground: 0, back: -Infinity };   // where the face is, for dragging it
// (ticket 145) A solid back wall: the deepest the friend's feet go in a
// place whose back wall can't be walked through (the imagination's: its
// ladders are the way up), a little in front of the wall's foot, in the
// stage's units; else the frame's own limit. The controller, a drag and the
// place control all keep to it (figYMin, a figY).
const SCENE_BACK = { imagination: () => imaginationWall(SCENE.ground, (SCENE.bottom - SCENE.top) / 700) + 3, emptyroom: () => imaginationWall(SCENE.ground, (SCENE.bottom - SCENE.top) / 700) + 3, bubblespace: () => imaginationWall(SCENE.ground, (SCENE.bottom - SCENE.top) / 700) + 3 };
const figYMin = F => Math.max((F.top - F.ground) * 0.6, (F.back ?? -Infinity) - F.ground);
// (ticket 149) What the friend can't walk through. A castle room's walls,
// from its perspective (roomFrame, as the room is drawn): the back wall's
// foot is SCENE_BACK's, and the side walls close in toward it, so how far
// across the friend's feet can go depends on how deep they are (walls: the
// half width at a depth). And the things one sits on (SEATS: a barrel, a
// bench, a chair, a throne), as boxes on the ground: across their top's
// span (x0 to x1) and from the floor in front of them (z, where a sitter's
// feet go) back by half their height, at most SOLID_DEPTH of their width;
// their height is their top's above that floor. Low ones can be jumped: while jumping, lower than half
// the friend's legs by the short jump, lower than their legs by the high
// one. The controller and a drag move the friend through solidMove, which
// slides along what they meet; already inside one (put there by a setting,
// or arriving), they can walk out. Closed ticket 50's "collision shape for
// the places it makes no sense to stand", begun with what we know the
// shape of.
const SOLID_DEPTH = 0.4, SOLID_MARGIN = 6;
let wallsMemo = { text: undefined, kind: null, W: null };
function castleWalls(B) {
  if (B.kind !== "castleroom" && B.kind !== "treeroom" && B.kind !== "bubbleroom" && B.kind !== "treetop") return null;   // (ticket 516: and the round room at the top of the tree)   // (ticket 223: the tree house rooms are square rooms too; ticket 290: and the bubble room)
  if (wallsMemo.text === bgShown.text && wallsMemo.kind === B.kind && bgShown.text) return wallsMemo.W;
  wallsMemo = { text: bgShown.text, kind: B.kind, W: castleWallsOf(B) };
  return wallsMemo.W;
}
function castleWallsOf(B) {
  // (ticket 516) the round room at the top of the tree: its wall round the back half of the floor's ellipse (topGeom), the
  // back at its foot there; at y, as wide as the ellipse is there, the picture's whole width from its sides' feet on
  if (B.kind === "treetop") { const { Yf, Df, R } = topGeom(stageC());
    return { back: Yf, stairs: false, half: y => y <= Yf ? 0 : y >= Yf + Df ? R : R * Math.sqrt(1 - ((Yf + Df - y) / Df) ** 2) }; }
  if (B.kind === "treeroom" || B.kind === "bubbleroom") { const { floor, bw, vy } = roomFrame(stageC()); return { back: floor, stairs: false, half: y => Math.max(0, bw * (y - vy) / (floor - vy)) }; }
  const style = castleOf(B).rooms[Math.round(B.castleRoom ?? 1) - 1], deep = style === "hallway" || style === "dining";
  const c = deep ? { ...stageC(), eye: 0.3 } : stageC(), { floor, bw, vy } = roomFrame(c);
  const back = vy + (floor - vy) / (1 + (deep ? HALL_STRETCHES * 0.8 : 0)), line = y => Math.max(0, bw * (y - vy) / (floor - vy));
  // (ticket 253, Matthew: "the cut offs don't permit me to reach the doors! We may need more than 2 segments for those
  // 'side' cut offs") the spiral staircase's sides follow its floor out to each door on its landings: a polyline through
  // each door's foot (spiralDoorAt), out past it by a friend's reach, from the floor's front corners in to the top;
  // never narrower than the straight sides
  if (style === "spiral") {
    // (its doors are up its stairs, above the eye's height: no point at the vanishing line, which pinched the sides between
    // two landings; above the highest door, as wide as there)
    const pts = [[floor, bw], ...[0, 1, 2, 3].map(j => { const d = spiralDoorAt(c, j); return [d.y, Math.abs(d.x) + 40 * c.k]; })].sort((a, b) => b[0] - a[0]);
    const poly = y => { for (let i = 1; i < pts.length; i++) { const [ya, ha] = pts[i - 1], [yb, hb] = pts[i]; if (y <= ya && y >= yb) return ya === yb ? Math.max(ha, hb) : ha + (hb - ha) * (y - ya) / (yb - ya); }
                        return y > pts[0][0] ? pts[0][1] : pts[pts.length - 1][1]; };
    return { back, stairs: true, half: y => Math.max(line(y), poly(y)) };
  }
  return { back, stairs: false, half: line };
}
// (read once a drawing, before the drawing mounts the place's scene: worked out afresh, not memoized on the scene's text)
// (not the spiral stair's room: walking up it is going up the stairs, to its doors higher up)
SCENE_BACK.castleroom = () => { const W = castleWallsOf(state.background); return W && !W.stairs ? W.back + 3 : -Infinity; };
// (ticket 176) The flat rooms' back walls: where each one's floor meets its
// wall as the room is drawn (livingroomScene, treeroomScene, witchroomScene,
// healerroomScene; the tent's far wall, tentScene), in front of which the
// friend stops, as in the imagination
for (const [kind, up] of [["livingroom", 120], ["treeroom", 110], ["bubbleroom", 110], ["witchroom", 100], ["healerroom", 100], ["tent", 70], ["hutroom", HUT_UP], ["hutbed", HUT_UP], ["hutpotion", HUT_UP], ["hutcenter", HUT_UP], ["treetop", HUT_UP]])
  SCENE_BACK[kind] = () => SCENE.ground - up * (SCENE.bottom - SCENE.top) / 700 + 3;
// (ticket 214, Matthew: "the horz blue line in each scene is wrong, and always too high -- it doesn't correspond with
// where we'd walk or run -- we'd need to fly") Each outdoor place's back limit at the far edge of its own ground, as
// drawn (in the frame's units, k = its height / 700, as the scenes draw): the meadow's edge, the shore, a lake's far
// side, the water's horizon, a fence, a moat, a house's wall. Without one, a place had the frame's stand-in, 60% of the
// way up to the picture's top: among the trees and clouds. Each value is how far above the ground line (g) its far
// edge is, from the scene's code where it says (the forest path's far ground, the crossing's far line, the beach's
// sea, the lake's far shore, open water's horizon), else read from a picture of it (ticket 214's audit).
// (ticket 298, Matthew: "Blue horz line in this scene is not accurate -- too low"; "would be easier to walk around the tree
// house trunk if the horizon line was corrected": the tree house's meadow runs back to its far row of trees, 124 up, its
// land drawn to 130: 118, just in front of those trees, room to walk round behind the trunk)
// (ticket 310, Matthew: "it's possible to walk behind either of them, and the dotted horizon line coincides with the
// horizon, not the facade of the building. instead, the building's facade has accurate collision boxes ... permitting a
// space for feet to squeeze between that box and the back horizon line": the house's lawn's far edge, 205 up, the
// castle's meadow's, 150 up; the buildings' own boxes are SCENE_BOXES')
// (ticket 503) the pasture's: just in front of its fence (162 up), far enough back that the friend walks behind each of
// its horses grazing (PASTURE_HORSES: a horse moved farther back must keep LANE_MIN behind its footprint)
const SCENE_FAR = { forest: 60, underwater: 40, deepsea: 30, witch: 22, cemetery: 100, house: 200, treehouse: 118, clearing: 140,
                    fairyforest: 110, mountains: 110, waterfall: 90, campsite: 100, cave: 60, caveroom: 130, farmyard: 90, desert: 120, castle: 138,
                    woodpath: 165, pasture: 157, crossing: 295, beach: 165, sea: 225, lake: 245, hut: 120 };
for (const [kind, up] of Object.entries(SCENE_FAR))
  SCENE_BACK[kind] = (top = SCENE.top, bottom = SCENE.bottom) => SCENE.ground - up * (bottom - top) / 700 + 3;
// (ticket 176) The things that stand in a place and can't be walked
// through, by their name in the scene (the words before any comma or
// number: "rock, left" and "toadstool 3" are a rock and a toadstool), each a
// box under its drawing: across its drawing's width (a tree, a lamp or a
// sign only its trunk or post: TRUNK of that width, at least 16 units,
// under its middle), and from where it stands back as a seat's, by half
// its height, at most SOLID_DEPTH of its width; its height is its drawing's
// top above where it stands, so a low one (a rock, a log, a bush) is jumped
// as a low seat is. Not solid: what one goes into or up (houses, huts,
// doors, ladders, stairs), what is alive (people, animals), what hangs or
// flies, what lies flat, any object hidden (objectsOff), a seat (already
// one), and anything where one swims.
// (ticket 284: the grass hut bedroom's bed and stump too, Matthew: "hut objects are all missing collision boxes?")
// (ticket 358: and what the audit found standing with no box: the cemetery's stones, the cave's stalagmites, the hut's woodpile
// and the hut center's fire, the trap room's helmet, the greenhouse's watering can, the house's toys, a chair not sat on, the
// sea floor's wreck and vent)
// (ticket 504: and the farm animals put in a place, which, alive as they are, stand in the way as a bench does: a cow, a
// pig, a chicken; the stable's own hens do not)
const STANDING = new Set(["headstone", "cross", "stag stone", "big stalagmite", "small stalagmite", "stalagmite", "crystal cluster", "cascade", "cavemushroom",   // (the last four: the cave room's, the little waterfall, the cave mushroom: ticket 471)
   "cow", "pig", "chicken",
   "pricklypear", "prickly pear", "barrelcactus", "barrel cactus", "agave",   // (ticket 501: the low desert plants, as things and in the desert)
   "grazing horse",   // (ticket 503: the pasture's horses, big as a hay cart: walked round, not through, by their footprints)
   "barn", "fencepiece", "haystack", "tractor", "windpump",   // (ticket 502: the farm things, each by its foot (data-footprint), and the farm yard's)
   "woodpile", "helmet", "watering can", "teddy", "truck", "blocks", "chair", "shipwreck", "vent", "palisade", "fallen", "anchor", "anvil", "armor", "barrel", "barrels", "basket", "bed", "bench", "bookcase", "bubblemaker", "coral", "lever", "skeleton", "box", "bucket", "buckets", "bunk beds", "bush", "cage", "campfire",
  "cat tree", "cauldron", "chest", "chest of gold", "coal heap", "column", "counter", "crate", "drum kit", "easel", "firewood", "flour sack", "forge",
  "fountain", "green bush", "hay cart", "hay heap", "keyboard", "log", "mushroom", "oven", "paint table", "picnic table", "pink bush", "plant", "potion shelves",
  "rock", "boulder", "sand castle", "snake tank", "soup pot", "stone", "stove", "stump", "table", "tea table", "telescope", "toadstool", "toy chest", "tree trunk",
  "water barrel", "water trough", "well", "white bush", "worktable", "yellow bush"]);
const TRUNKED = new Set(["big tree", "giant tree", "deadtree", "skullstake", "tree",   // (big tree, giant tree: ticket 358)
   "saguaro", "joshuatree", "joshua tree", "ocotillo",   // (ticket 501: the tall desert plants, in the way by their columns, trunks and stems, not their arms)
   "great tree", "jungle tree", "palm tree", "palm", "pine", "willow", "lamp", "umbrella", "sign", "signpost"]);
// (ticket 214's audit: the living room's plant had a box as wide as its leaves) a pot's plant stands on its pot: its foot,
// as a trunk's is found, but however short
const POTTED = new Set(["plant", "bird of paradise", "monstera", "hibiscus", "bromeliad"]);   // (ticket 358: the greenhouse's, in their pots; not the hut center's fire, whose drawing is its light on the floor too, 378 wide)
// (ticket 358, Matthew: "most deep sea objects should have a very small collision box. Each plant extending upwards, but also
// shells beyond a certain small size --- It can be a very small box in all cases, but it will help the motion planning and
// execution look more natural") the sea floor's plants and big shells: a small box at the foot, a quarter of the drawing's
// width (6 to 22 units), 8 deep, 40 high at most; a shell narrower than SMALL_SHELL none (walked over)
const SMALL_BOX = new Set(["kelp", "seaweed", "coral", "shell", "seagrass", "seafan", "sealettuce"]), SMALL_SHELL = 26;
const TRUNK = 0.14, SWIM_PLACES = new Set(["lake"]);   // (ticket 358: under the sea the friend walks the sea floor, so its things stand in the way)
const objBase = name => name.split(",")[0].replace(/\s+\d+$/, "").trim();
// a box (x, y, width, height) through a matrix, as the box round its four corners
function boxThrough(r, m) {
  const pts = [[r.x, r.y], [r.x + r.width, r.y], [r.x, r.y + r.height], [r.x + r.width, r.y + r.height]].map(([x, y]) => [m.a * x + m.c * y + m.e, m.b * x + m.d * y + m.f]);
  const xs = pts.map(q => q[0]), ys = pts.map(q => q[1]);
  return { x: Math.min(...xs), y: Math.min(...ys), width: Math.max(...xs) - Math.min(...xs), height: Math.max(...ys) - Math.min(...ys) };
}
// (Matthew: "palm trees, and other curved things, need about 4 or 5 different collision boxes, not just one") A trunk's
// boxes, TRUNK_BANDS of them stacked from its foot to its top, each as wide as the trunk is at that height: its parts
// (the tall shapes standingHere found) followed along their length (getPointAtLength, through each one's matrix; a
// stroked one grown by half its stroke), so a palm's leaning trunk has its boxes leaning with it. Each band: x0, x1,
// and lo, hi, its heights above the ground where the thing stands (z).
// (ticket 345) base: that many bands more in its lowest third (BASE_SHARE), where a trunk spreads at its foot (the tree
// house's braid curving out to the ground), the rest of it in n - 1
const TRUNK_BANDS = 5, BASE_SHARE = 1 / 3;
function trunkBands(parts, z, n = TRUNK_BANDS, base = 0) {
  const pts = [];
  for (const [sh, m] of parts) {
    let L = 0; try { L = sh.getTotalLength(); } catch (e) { continue; }
    const st = sh.getAttribute("stroke"), hw = st && st !== "none" ? (parseFloat(sh.getAttribute("stroke-width")) || 1) / 2 * Math.hypot(m.a, m.b) : 0;
    for (let i = 0; i <= 48 && L > 0; i++) { const p = sh.getPointAtLength(L * i / 48); pts.push([m.a * p.x + m.c * p.y + m.e, m.b * p.x + m.d * p.y + m.f, hw]); }
  }
  if (pts.length < 2) return null;
  const top = Math.min(...pts.map(p => p[1] - p[2])), bot = Math.max(z, ...pts.map(p => p[1])), out = [];
  const cut = base ? bot - (bot - top) * BASE_SHARE : bot, up = base ? n - 1 : n;
  const edges = [...Array(up + 1)].map((_, i) => top + (cut - top) * i / up).concat([...Array(base)].map((_, i) => cut + (bot - cut) * (i + 1) / base));
  for (let i = 0; i + 1 < edges.length; i++) {
    const yA = edges[i], yB = edges[i + 1], inb = pts.filter(p => p[1] + p[2] >= yA && p[1] - p[2] <= yB);
    if (inb.length) out.push({ x0: Math.min(...inb.map(p => p[0] - p[2])), x1: Math.max(...inb.map(p => p[0] + p[2])), lo: Math.max(0, z - yB), hi: z - yA });
  }
  if (out.length) out.bot = bot;   // (ticket 355: how far down the trunk is drawn, below its object's depth where its foot spreads)
  return out.length ? out : null;
}
// (ticket 283, Matthew: "placed trees have no collision boxes. A toggle (on by default) where they do: slim vertically, precise
// at the base, and in segments for trees that lean a lot") the things put in a place are objects of it, measured as its own
// (a tree by its trunk, in bands up it); 🏞️ Background's "🧱 placed things are in your way" (thingsSolid, on at the start)
// takes them out of the way when off. More kinds of thing stand in the way since (a bench, a barrel, a skeleton, an anchor,
// a bubble maker, a lever, coral)
const isPlaced = o => !!(o.node.closest && o.node.closest('[data-drag^="thing:"]')) || !!o.node.querySelector('[data-drag^="thing:"]');
// (ticket 381, ^du_report(12)'s next step 2: "measure a placed thing's boxes once and keep them by its kind, place and size, so a
// scene built again does not measure every maker through the screen (standingHere: 42 ms at 256 makers, on every tap that adds one)")
// A placed thing's boxes, kept under what its drawing is made from (the picture's frame, the place, 🔭's vanishing line, the thing
// itself, a signpost's boards): the same drawing in the same place has the same boxes; only a new or changed thing is measured
const STANDING_KEPT = new Map();
function standingHere(B, seats) {
  if (SWIM_PLACES.has(B.kind) || !bgShown.node) return [];
  const off = objectsOff(B), k = (SCENE.bottom - SCENE.top) / 700, out = [];
  // (its box in the friend's units, through the screen, as ladderAt does: the scene may sit in a nested svg of its own)
  const picEl = document.getElementById("pic"), P = picEl && picEl.getScreenCTM(), Pinv = P ? P.inverse() : null;
  const frame = [SCENE.left, SCENE.top, SCENE.bottom, SCENE.ground, B.kind, Math.round(10 * sceneVanish(B))];
  // (ticket 502: and a fence piece's neighbours, whose posts its rails reach: fenceLinks)
  const keptKey = o => { const dg = o.node.querySelector('[data-drag^="thing:"]') || (o.node.closest && o.node.closest('[data-drag^="thing:"]')), i = dg && +dg.dataset.drag.slice(6), t = dg && (B.things || [])[i];
    return t ? JSON.stringify([frame, t, t.kind === "signpost" ? signpostBoards(B, t) : t.kind === "fencepiece" ? fenceLinks(B, i) : ""]) : null; };
  for (const o of bgShown.objs) {
    if (B.thingsSolid === false && isPlaced(o)) continue;
    if (off.has(o.name) || seats.has(o.name)) continue;
    const key = keptKey(o), kept = key && STANDING_KEPT.get(key);
    if (kept) { out.push(...kept.map(b => ({ ...b, name: o.name }))); continue; }
    const from = out.length;
    measureStanding(o);
    if (key) { if (STANDING_KEPT.size > 2000) STANDING_KEPT.clear(); STANDING_KEPT.set(key, out.slice(from)); }
  }
  return out.concat(laddersHere());
  function measureStanding(o) {
    // (ticket 241) a drawing that marks its trunk (data-trunk: the tree house's braid) is a trunk, measured by it alone
    const marked = o.node.querySelector("[data-trunk]"), root = marked || o.node;
    const base = objBase(o.name), trunk = TRUNKED.has(base) || !!marked, potted = POTTED.has(base);
    const small = SMALL_BOX.has(base);
    if (!(trunk || small || POTTED.has(base) || STANDING.has(base)) || !Number.isFinite(o.z)) return;
    let b = null;
    // (ticket 214's audit: a cauldron's box stood up past the picture's top, its fumes measured as the pot) what rises
    // from a thing, .fumes, is no part of its box: hidden while it is measured
    // (ticket 304, Matthew: the king's chest's box "reaches too far to the left, where there are only some coins on the
    // ground and no chest") nor what lies flat on the floor beside it: a part marked .flat (coins spilled), and its shadow
    // (a faint black ellipse under it)
    const fumes = [...o.node.querySelectorAll('.fumes, .flat, ellipse[fill="#000"], ellipse[fill="#000000"]')]
      .filter(el => !el.matches("ellipse") || el.closest(".fumes, .flat") || +(el.getAttribute("opacity") ?? el.getAttribute("fill-opacity") ?? 1) < 0.45);
    for (const f of fumes) f.style.display = "none";
    try {   // (Firefox throws for a drawing not shown)
      const r = root.getBBox(), E = root.getScreenCTM(), m = Pinv && E ? Pinv.multiply(E) : null;
      if (m && r.width > 0 && r.height > 0) {
        const pts = [[r.x, r.y], [r.x + r.width, r.y], [r.x, r.y + r.height], [r.x + r.width, r.y + r.height]].map(([x, y]) => [m.a * x + m.c * y + m.e, m.b * x + m.d * y + m.f]);
        const xs = pts.map(q => q[0]), ys = pts.map(q => q[1]);
        b = { x: Math.min(...xs), y: Math.min(...ys), width: Math.max(...xs) - Math.min(...xs), height: Math.max(...ys) - Math.min(...ys) };
        // (ticket 355: a trunk's whole object, its foot in the floor too, which a marked trunk's own shapes leave out: how far down it is drawn)
        if (trunk && marked) { const E2 = o.node.getScreenCTM(), m2 = E2 && Pinv.multiply(E2), q2 = m2 && boxThrough(o.node.getBBox(), m2); if (q2) b.bottom = q2.y + q2.height; }
      }
    } catch (e) { return; }
    finally { for (const f of fumes) f.style.display = ""; }
    if (!b || !(b.width > 0) || !(b.height > 0)) return;
    // (ticket 441, Matthew: "collision box for fountain in the castle greenhouse looks off --- I should say -- collision oval")
    // A drawing that marks its footprint ([data-footprint]: an unseen shape where it meets the floor) is that wide and that
    // deep, its oval (a round thing) or its box there: the greenhouse fountain's foot line is its pool's middle and it stands
    // tall with its jet, so a box from its foot back by half its height held an oval behind the pool, above it on the screen.
    const fpEl = o.node.querySelector("[data-footprint]");
    if (fpEl && Pinv) { let q = null; try { const E = fpEl.getScreenCTM(); q = E && boxThrough(fpEl.getBBox(), Pinv.multiply(E)); } catch (e) { q = null; }
      if (q && q.width > 0 && q.height > 0) { out.push(...solidParts(base, { name: o.name, x0: q.x, x1: q.x + q.width, back: q.y, front: q.y + q.height, h: Math.max(0, o.z - b.y), standing: true }, k)); return; } }
    if (small) { if (base === "shell" && b.width < SMALL_SHELL * k) return;   // (ticket 358: a small box at its foot, or none)
      const w = clamp(0.25 * b.width, 6 * k, 22 * k), mid = b.x + b.width / 2;
      out.push({ name: o.name, x0: mid - w / 2, x1: mid + w / 2, back: o.z - 8 * k, front: o.z, h: Math.min(40 * k, Math.max(0, o.z - b.y)), standing: true, small: true }); return; }
    // (0.0.147, Matthew: "the sign post and trees each have a box that looks misaligned") a trunk or a post is not
    // always in the middle of its drawing: a canopy spreads to one side, a signpost's boards hang off its post. Its
    // box is where its tall parts reach its foot (the trunk, the post: each at least a quarter of its height, ending
    // in its lowest tenth, not as wide as the whole), else the middle, as before.
    // (0.0.153: each part measured as the whole is, its own box through its own matrix: the beach's palms, a trunk drawn
    // as one stroked curve, were missed through the screen's boxes, and stood on their crowns' middles)
    let foot = null, bands = null;
    // (ticket 257) a drawing that gives its trunk's spine ([data-spine]: pieces along it, each as wide as the trunk there,
    // as a palm's stacked segments do) is measured by it, in as many bands as it asks (data-bands)
    const spines = trunk && Pinv ? [...o.node.querySelectorAll("[data-spine]")] : [];
    if (spines.length) {
      const parts = []; for (const sh of spines) { try { const E = sh.getScreenCTM(); if (E) parts.push([sh, Pinv.multiply(E)]); } catch (e) { /* (not shown) */ } }
      bands = parts.length ? trunkBands(parts, o.z, +spines[0].dataset.bands || TRUNK_BANDS) : null;
    } else if ((trunk || potted) && Pinv) {
      const parts = [];
      for (const sh of root.querySelectorAll("rect, path, polygon, polyline, line, ellipse")) {
        let m, q;
        try { const E = sh.getScreenCTM(); m = E && Pinv.multiply(E); q = m && boxThrough(sh.getBBox(), m); } catch (e) { continue; }
        if (!q || (!q.width && !q.height)) continue;
        if (q.y + q.height < b.y + 0.9 * b.height || (trunk && q.height < 0.25 * b.height) || q.width > 0.6 * b.width) continue;
        parts.push([sh, m, q]);
      }
      if (parts.length) {
        foot = [Math.min(...parts.map(p => p[2].x)), Math.max(...parts.map(p => p[2].x + p[2].width))];
        if (trunk) bands = trunkBands(parts, o.z, TRUNK_BANDS, marked ? +marked.dataset.base || 0 : 0);
      }
    }
    // (a trunk's foot is its lowest band's width, the part on the ground)
    if (bands) { const fb = bands[bands.length - 1]; foot = [fb.x0, fb.x1]; }
    const mid = foot ? (foot[0] + foot[1]) / 2 : b.x + b.width / 2, h = Math.max(0, o.z - b.y);
    const w = foot ? (potted ? foot[1] - foot[0] : Math.min(0.4 * b.width, Math.max(16 * k, foot[1] - foot[0]))) : trunk ? Math.min(b.width, Math.max(16 * k, TRUNK * b.width)) : b.width;
    // (ticket 355, Matthew: "why can I walk through these collision boxes near the corner, for the tree?" the tree room's trunk
    // was drawn down to 9 units in front of its box, its foot spreading over the floor: a trunk's box reaches as far forward
    // as its trunk is drawn, so the feet stop at its foot, not inside it)
    const drawnBot = Math.max(bands && Number.isFinite(bands.bot) ? bands.bot : -Infinity, Number.isFinite(b.bottom) ? b.bottom : -Infinity);
    const front = trunk && Number.isFinite(drawnBot) ? Math.min(Math.max(o.z, drawnBot), o.z + 0.5 * w) : o.z;   // (at most half its width further: a giant's roots are not its trunk)
    out.push(...solidParts(base, { name: o.name, x0: mid - w / 2, x1: mid + w / 2, back: o.z - clamp(0.5 * h, 8, Math.max(8, SOLID_DEPTH * w)), front, h, standing: true, ...(bands ? { bands } : {}) }, k));
  }
}
// (ticket 358, Matthew: "ladders in the tree house scene should have collision boxes near the base but do not. All ladders
// probably") Every ladder (but a hatch's, whose hole is in the way already) a box at its foot, as wide as it is there (8 to 70
// units) and 10 deep, ending 2 short of its foot: a climb puts the friend just in front of the foot (startClimb: z + 2), so a
// ladder is gone round by a walk and still climbed
function laddersHere() {
  const node = bgShown.node, picEl = document.getElementById("pic"), P = picEl && picEl.getScreenCTM();
  if (!node || !P || typeof ladderAt !== "function") return [];
  const Pinv = P.inverse(), k = (SCENE.bottom - SCENE.top) / 700, out = [];
  node.querySelectorAll("[data-ladder]").forEach((el, i) => {
    if (el.dataset.down !== undefined) return;
    let q = null; try { const E = el.getScreenCTM(); q = E && boxThrough(el.getBBox(), Pinv.multiply(E)); } catch (e) { return; }
    const L = q && q.width > 0 ? ladderAt(el, Pinv) : null;
    if (!L || !Number.isFinite(L.z)) return;
    const w = clamp(q.width, 8 * k, 70 * k);
    out.push({ name: `ladder ${i + 1}`, x0: L.x - w / 2, x1: L.x + w / 2, back: L.z - 12 * k, front: L.z - 2, h: Math.max(20 * k, L.z - L.top), standing: true, ladder: true });
  });
  return out;
}
// (ticket 337, Matthew: "round / oval objects get oval collision regions, not 'boxes'; let's have tables be precise, in
// terms of boxes for legs and top"; "simillar for beds and other funiture") A round thing (ROUND) is an oval on the floor,
// the one its box holds (oval: solidAt, planPath, 🔬 meet and draw it so); a table is its top (a band at its height, as
// wide and deep as it is) and four legs (each a box from the floor up to the top); bunk beds their four posts and two bunks;
// a bed its legs and its frame; a bench its legs and its seat and back. Each part keeps the thing's name (🔬's glow, a seat's
// name, getting out of it); its part says which. The friend meets a part above their ankles with their legs' box (youBoxes),
// so a low table's top still stops them, and only one high enough is walked under.
const ROUND = new Set(["fallen", "barrel", "basket", "bucket", "cauldron", "soup pot", "stump", "rock", "boulder", "stone", "toadstool", "mushroom", "bush", "green bush",
  "pink bush", "white bush", "yellow bush", "campfire", "fountain", "well", "water barrel", "coal heap", "hay heap", "haystack"]);   // (haystack: ticket 502)
function solidParts(base, o, k) {
  if (ROUND.has(base) && !o.bands) return [{ ...o, oval: true }];
  const w = o.x1 - o.x0, d = o.front - o.back, h = o.h;
  if (!(h > 0) || w < 24 * k || !["table", "tea table", "bed", "bunk beds", "bench"].includes(base)) return [o];
  const legW = clamp(0.09 * w, 4 * k, 12 * k), legD = clamp(0.3 * d, 3 * k, 10 * k);
  const box = (x0, x1, back, front, lo, hi, part) => ({ ...o, x0, x1, back, front, h: hi, part, bands: lo > 0 ? [{ x0, x1, lo, hi }] : undefined });
  const legs = (hi, round = false) => { const cx = (o.x0 + o.x1) / 2, cz = (o.back + o.front) / 2, ix = 0.55 * w / 2, iz = 0.55 * d / 2;   // (a round top's legs inside its oval)
    const at = round ? [[cx - ix - legW / 2, cz + iz - legD / 2], [cx + ix - legW / 2, cz + iz - legD / 2], [cx - ix - legW / 2, cz - iz - legD / 2], [cx + ix - legW / 2, cz - iz - legD / 2]]
                     : [[o.x0, o.front - legD], [o.x1 - legW, o.front - legD], [o.x0, o.back], [o.x1 - legW, o.back]];
    return at.map(([x, b]) => box(x, x + legW, b, b + legD, 0, hi, "leg")); };
  const across = (lo, hi, part) => box(o.x0, o.x1, o.back, o.front, lo, hi, part);
  if (base === "table") { const t = clamp(0.2 * h, 5 * k, 16 * k); return [across(h - t, h, "top"), ...legs(h - t)]; }
  // (the healer's tea table: a round top on one pedestal in its middle)
  if (base === "tea table") { const t = clamp(0.2 * h, 5 * k, 16 * k), cx = (o.x0 + o.x1) / 2, cz = (o.back + o.front) / 2, pw = Math.max(6 * k, 0.12 * w), pd = Math.max(4 * k, 0.3 * d);
    return [{ ...across(h - t, h, "top"), oval: true }, box(cx - pw / 2, cx + pw / 2, cz - pd / 2, cz + pd / 2, 0, h - t, "leg")]; }
  if (base === "bunk beds") return [...legs(h).map(q => ({ ...q, part: "post" })), across(0.12 * h, 0.42 * h, "lower bunk"), across(0.56 * h, 0.82 * h, "upper bunk")];
  if (base === "bed") return [...legs(0.3 * h), across(0.25 * h, h, "frame")];
  return [...legs(0.5 * h), across(0.42 * h, h, "seat and back")];   // (a bench)
}
// (does the box x0..x1 by z0..z1 reach into o's oval on the floor?)
const ovalOf = (o, gx = 0, gz = 0) => ({ cx: (o.x0 + o.x1) / 2, cz: (o.back + o.front) / 2, rx: (o.x1 - o.x0) / 2 + gx, rz: (o.front - o.back) / 2 + gz });
// (ticket 369) an oval grown so that the friend's feet (gx either side, gz deep) kept outside it never meet it. Adding gx and gz to
// its radii fell short at its corners: a flat cauldron and a stride's wide, shallow reach, a foot a little deeper met it where it
// is wider. Scaling it by 1 + t (t the feet's box in its radii) held, but grew a flat oval far too wide for a little depth (a bush 48
// across grew to 118 round the planner's corners) and closed the lanes between the tree house's bushes; so each radius grows by
// √2 times its margin, which holds the feet's box (within an oval of √2 its half-sizes) for feet wide and shallow as they are
const grownOval = (o, gx, gz) => { const E = ovalOf(o); return { cx: E.cx, cz: E.cz, rx: E.rx + Math.SQRT2 * gx, rz: E.rz + Math.SQRT2 * gz }; };
function ovalMeets(o, x0, x1, z0, z1) {
  const E = ovalOf(o), px = clamp(E.cx, x0, x1), pz = clamp(E.cz, z0, z1);
  return ((px - E.cx) / E.rx) ** 2 + ((pz - E.cz) / E.rz) ** 2 < 1;
}
// (does the segment pass through the inside of the oval (cx, cz, rx, rz)? as segBox does a box's)
function segOval(ax, az, bx, bz, E) {
  const ux = (ax - E.cx) / E.rx, uz = (az - E.cz) / E.rz, dx = (bx - ax) / E.rx, dz = (bz - az) / E.rz;
  const a = dx * dx + dz * dz, b = 2 * (ux * dx + uz * dz), c = ux * ux + uz * uz - 1;
  if (a < 1e-12) return c < 0;
  const disc = b * b - 4 * a * c; if (disc <= 0) return false;
  const r = Math.sqrt(disc), t0 = Math.max(0, (-b - r) / (2 * a)), t1 = Math.min(1, (-b + r) / (2 * a));
  return t1 - t0 > 1e-6;
}
// (ticket 237, Matthew: "collision detection should avoid walking over the trap doors in the floor of the tree houses")
// a hole in the floor (a tree room's trapdoor: data-hole, its box on the floor), a solid as low as a step: the feet
// don't walk onto it, a walk goes round it (planPath), a jump clears it, and 🔬 Inspection's collision boxes show it; its ladder
// down is taken from its near edge, as before
const HOLE_H = 30;
const holesHere = () => bgShown.node ? [...bgShown.node.querySelectorAll("[data-hole]")].map(el => {
  const [x0, x1, back, front] = el.dataset.hole.split(" ").map(Number); return { name: "trapdoor", x0, x1, back, front, h: HOLE_H }; }) : [];
// (ticket 310) A place's buildings that one walks round: a box at the building's foot, as wide as the building and only as
// deep as it looks (the house's facade, a little; the castle, back to its moat's far edge), so there is room behind it to
// the back line at the horizon. The house: its building as drawn (#bg-building, its flower bed with it); the castle: its
// towers' span, as castleSvg draws them (out to 180 of its units either side), at its size (castleAt)
const SCENE_BOXES = {
  house: c => { const el = bgShown.node && bgShown.node.querySelector("#bg-building"), picEl = document.getElementById("pic"), P = picEl && picEl.getScreenCTM();
    if (!el || !P) return [];
    const r = el.getBoundingClientRect(), Pi = P.inverse(), a = new DOMPoint(r.left, r.top).matrixTransform(Pi), b = new DOMPoint(r.right, r.bottom).matrixTransform(Pi), yb = c.g - HOUSE_BASE * c.k;
    return [{ name: "house", x0: a.x, x1: b.x, back: yb - 18 * c.k, front: yb + 2, h: yb - a.y }]; },
  castle: (c, B) => { const K = castleOf(B), { foot, z } = castleAt(c, K), M = moatOf(K.moat), half = 180 * z;
    return [{ name: "castle", x0: -half, x1: half, back: foot + z * (K.moat > 0 ? M.cy - M.ry + 4 : -24), front: foot + 2, h: (66 + 18 * K.levels + 50) * z }]; },
};
const sceneBoxesHere = B => { try { return SCENE_BOXES[B.kind] && !objectsOff(B).has(B.kind === "castle" ? "castle" : "house") ? SCENE_BOXES[B.kind](stageC(), B) : []; } catch (e) { return []; } };
// (the solids here, made again only when the place, its seats or its hidden objects change)
let solidMemo = { text: undefined, kind: null, off: "", list: [] };
function solidsHere() {
  const B = state.background;
  if (B.kind === "none") return [];
  const offKey = [...objectsOff(B)].join("|") + (B.thingsSolid === false ? "|no-things" : "");   // (ticket 283: placed things out of the way, or not)
  if (solidMemo.text !== bgShown.text || solidMemo.kind !== B.kind || solidMemo.off !== offKey || !bgShown.text) {
    const seats = placeSeats(B);   // (a place's own: a thing's seat, ticket 413, is made from these solids, thingSeats)
    // (ticket 299: a flat one, a blanket, in nobody's way; each marked as a seat, for 🔬's own chip, sit or lie)
    solidMemo = { text: bgShown.text, kind: B.kind, off: offKey, list: seats.filter(t => !t.flat).map(t => { const w = t.x1 - t.x0, d = clamp(0.5 * (t.z - t.y), 8, Math.max(8, SOLID_DEPTH * w));   // (as deep as half its height, at most SOLID_DEPTH of its width: a couch is not as deep as it is wide)
      return { name: t.name, x0: t.x0, x1: t.x1, back: t.z - d, front: t.z, h: t.z - t.y, seat: t.lie ? "lie" : "sit", ...(ROUND.has(objBase(t.name)) ? { oval: true } : {}) }; }).concat(sceneBoxesHere(B)).concat(standingHere(B, new Set(seats.map(t => t.name))), holesHere()) };
    // (ticket 332, Matthew: "why is the path planned through the collision boxes of the corner tree trunk?") A solid
    // standing nearly against the back line reaches it: the tree room's corner trunk stopped 4 units short of it, a lane
    // the feet (3 deep) could stand in, walk along with the controller, and plan no way out of. A gap under LANE_MIN is
    // no way behind anything, so the box goes back to the line
    const backLine = SCENE.ground + figYMin(sceneFrame);
    if (Number.isFinite(backLine)) solidMemo.list = solidMemo.list.map(o => !o.pit && o.back > backLine - 0.5 && o.back - backLine < LANE_MIN ? { ...o, back: backLine - 1, lane: o.back } : o);
  }
  return solidMemo.list;
}
const LANE_MIN = 24;   // (16 till 0.0.288: ticket 358 found a 17-unit pocket behind the cave's small stalagmite, walled in by the big one and the picture's edge)
// the friend's legs, hip to foot, in the stage's units (for what a jump clears)
const legReach = () => poseJoints && poseJoints.legs && poseJoints.legs.length
  ? figureT.k * Math.max(...poseJoints.legs.map(l => Math.hypot(l.F[0] - l.H[0], l.F[1] - l.H[1]))) : 100 * figureT.k;
// what stops the friend's feet at (x, z): a wall, a solid, or null
// (Matthew: "two small collision boxes under the current friend, not one ... Place one under each foot, about the
// size of the foot"; "the more that they coincide with 'the bottom of a shoe' in size the more realistic the game play
// will become") The friend meets things with the soles of their feet: a box under each foot, where it is drawn (its
// middle from poseJoints' foot, at the friend's scale, figureT.k), as wide as the shoe seen this way (across, facing
// us; its length, in profile: footR), a step deep. feetSoles(x, z): the two, were the feet at (x, z); feetSpan(): how
// far they reach either side of the middle, and how deep.
// (0.0.153, with the trunks' bands) each box also has its heights above the ground, [4] and [5]: a sole's from the
// ground to the ankle, the torso's from the hips to the crown (youHeights), so a thing meets the boxes at its heights
// (ticket 286: upright, their crown as they stand, the head not lowered for a crawl: whether a door is too low for them)
function youHeights(upright = false) {
  const J = poseJoints, T = figureT;
  if (!J || !J.legs || !J.legs.length || !T || !(T.k > 0)) return { ankle: 20, hips: 80, crown: 200 };
  // (ticket 492) ridden, they are up on the horse: the hooves' fetlocks are the ankles that meet the floor's things, the hips
  // on the saddle's seat, the crown that far over them as they sit (a low door or bridge meets a rider's head)
  if (horseNow && horseNow.ride) { const s = T.k * horseNow.hs, hips = horseNow.seat * s, hipsY = Math.max(...J.legs.map(l => l.H[1])), crownY = faceFrame.ty + headCrown(state.face) * faceFrame.s;
    return { ankle: HORSE_FETLOCK * s, hips, crown: hips + Math.max(0, (hipsY - crownY) * T.k) }; }
  const C = !upright && crawlShape();
  if (C) return { ankle: C.ankle, hips: C.torso[4], crown: C.torso[5] };
  // (ticket 544) upright, a giraffe stands as tall as the top of its neck and head (giraffeReach): a door too low for that
  // it ducks through crawling, its neck leaning down the way it goes, seen short (giraffeGeom's squash)
  const top = upright && state.costume.kind === "giraffe" ? Math.min(headCrown(state.face), giraffeReach(costumeOn(), state.face, {}).top) : headCrown(state.face);
  const foot = Math.max(...J.legs.map(l => l.F[1])), hips = Math.max(...J.legs.map(l => l.H[1])), crown = faceFrame.ty - (upright ? faceFrame.drop || 0 : 0) + top * faceFrame.s;
  return { ankle: Math.max(4, 0.2 * (foot - hips) * T.k), hips: Math.max(0, (foot - hips) * T.k), crown: Math.max(0, (foot - crown) * T.k) };
}
// (ticket 286, Matthew: "when we crawl, our collision box does not match our torso -- it stays upright?") Crawling, the
// friend meets things where they are as drawn. From the side, turned about the hips (J.turn): a box on the floor under
// the hands, one under the knees and the feet, and the body lying between them, from under the belly (half the hips'
// height) to the top of the head. Toward us, the hands nearest, each a box; away, the soles nearest, as walking; and the
// body running back into the picture behind them, J.depth deep (its back seen short), across the arms, from under the
// belly to the crown. Offsets from where they stand (x, z) and heights, in the picture's units; null when not crawling.
function crawlShape() {
  const J = poseJoints, T = figureT, F = faceFrame;
  if (!J || !J.crawl || !J.legs || !J.legs.length || !J.arms || !(T && T.k > 0) || !F) return null;
  const k = T.k, h = y => Math.max(0, (T.g - y) * k), hd = Math.max(2.5, 5 * k), ankle = Math.max(4, 0.1 * J.LL * k);
  const hr = Math.max(8, 0.35 * (J.handR || 40)), r = Math.max(10, (HEAD_C - headCrown(state.face)) * F.s), head = [0, F.ty + HEAD_C * F.s];
  const xs = (ps, pad) => [(Math.min(...ps.map(p => p[0])) - pad) * k, (Math.max(...ps.map(p => p[0])) + pad) * k];
  if (J.crawl === "side") {
    const a = J.turn * Math.PI / 180, c = Math.cos(a), sn = Math.sin(a), P = ([x, y]) => [x * c - (y - J.L) * sn, J.L + x * sn + (y - J.L) * c];
    const back = xs(J.legs.flatMap(l => [P(l.K), P(l.F)]), J.legW / 2), front = xs(J.arms.map(q => P(q.H)), hr);
    const spine = [...J.legs.map(l => P(l.H)), ...J.arms.map(q => P(q.A))], hc = P(head);
    const body = xs(spine.concat([[hc[0] - r, 0], [hc[0] + r, 0]]), 0);
    return { ankle, soles: [[...back, -hd, hd, 0, ankle], [...front, -hd, hd, 0, ankle]],
             torso: [...body, -hd, hd, 0.5 * Math.min(...spine.map(p => h(p[1]))), h(hc[1]) + r * k] };
  }
  // (the crown as tall as they crawl, the arm upright and the head on it, as from the side: from behind, the head drawn
  // higher, beyond the back, is farther, not taller)
  const crown = Math.min(h(F.ty + headCrown(state.face) * F.s), (0.9 * J.armLen + 2 * r) * k), across = xs(J.arms.flatMap(q => [q.A, q.H]), hr), depth = J.depth * k;
  const soles = J.crawl === "toward" ? J.arms.map(q => [...xs([q.H], hr), -hd, hd, 0, ankle]) : null;
  return { ankle, soles, torso: [...across, -depth, hd, 0.25 * crown, crown] };
}
// (ticket 492, Matthew: "When 'riding horse' or 'leading horse' -- we need the collision boxes to be more accurate. our
// actual body is above the horse. the horse has a body, and feet. Do the same we are doing for humanoid bodies, but extended
// for horses (four legs, not just two)") The friend's horse meets things as it is drawn (horseNow): a box under each of its
// four hooves, as wide as a hoof, a step deep, up to its fetlock (the pair drawn farther back, seen end on, that much
// deeper); and its body, from its belly up to its back, along its barrel, as deep as the horse is wide seen side on, or as
// long as it is seen end on: so what is lower than its belly passes under it, between its legs. horseBoxes(x, z): those,
// were the friend's feet at (x, z); null with no horse.
const HORSE_FETLOCK = 46;
function horseBoxes(x, z) {
  const H = horseNow, T = figureT;
  if (!H || !T || !(T.k > 0) || !H.barrel) return null;
  const s = T.k * H.hs, at = hx => x + T.k * (H.X + H.k * H.hs * hx), hw = Math.max(3, 11 * s), hd = Math.max(2.5, 6 * s);
  // (ticket 496: led from its far side, the horse stands nearer than the friend, its depth the other way)
  const f = H.far ? -1 : 1, hooves = H.hooves.map(([hx, , back]) => { const cx = at(hx), zz = z - f * back * s; return [cx - hw, cx + hw, zz - hd, zz + hd, 0, HORSE_FETLOCK * s]; });
  const [b0, b1, belly, top, deep] = H.barrel, xa = at(b0), xb = at(b1);
  return { hooves, body: [Math.min(xa, xb), Math.max(xa, xb), H.far ? z - hd : z - deep * s, H.far ? z + deep * s : z + hd, belly * s, top * s] };
}
// (ticket 492) ridden: the rider's box, up on the horse, from the hips on its saddle to their crown, as wide as their hips
// (a sliver at least), at the barrel's middle depth seen side on, at the hooves' line seen end on
function riderBox(x, z) {
  const H = horseNow, T = figureT, Y = youHeights(), s = T.k * H.hs, half = Math.max(4, 16 * s), hd = Math.max(2.5, 6 * s), zr = z - (H.view === "side" ? 12 : 0) * s;
  return [x - half, x + half, zr - hd, zr + hd, Y.hips, Y.crown];
}
function feetSoles(x, z) {
  const J = poseJoints, T = figureT;
  if (!J || !J.legs || !J.legs.length || !T || !(T.k > 0)) return [[x - SOLID_MARGIN, x + SOLID_MARGIN, z - 3, z + 3, 0, 20]];
  // (ticket 492) ridden, the soles on the floor are the horse's hooves: theirs are up beside its barrel
  const HB = horseNow && horseNow.ride ? horseBoxes(x, z) : null;
  if (HB) return HB.hooves;
  const C = crawlShape();
  if (C && C.soles) return C.soles.map(([a, b, d0, d1, lo, hi]) => [x + a, x + b, z + d0, z + d1, lo, hi]);
  const prof = facingOf(state.body) !== 0, hw = T.k * Math.max(8, Math.min(45, (J.footR || 50) * (prof ? 0.6 : 0.42))), hd = Math.max(2.5, hw * 0.35), up = youHeights().ankle;
  return J.legs.map(l => { const fx = x + T.k * l.F[0]; return [fx - hw, fx + hw, z - hd, z + hd, 0, up]; });
}
const feetSpan = () => { const S = feetSoles(0, 0); return [Math.max(...S.map(s => Math.max(-s[0], s[1]))), Math.max(...S.map(s => s[3]))]; };
// (Matthew: "one final box, that coincides with the center of the torso, but narrower, to indicate that we can sometimes
// squeeze -- Otherwise, the two feet can pass over a narrow pole, and we won't collide"; "the feet gap is the same gap as
// the width of the box above them, for the torso -- it extends from the crotch through the head") Under the middle of
// the torso, between the soles, a box as wide as the gap between them (none when they meet), as deep as a sole: a pole
// that would pass between the feet meets it. youBoxes(x, z): the soles and it, all that solidAt tests.
// (0.0.153) With the feet together (in profile, the soles overlap) a sliver still, so a palm's leaning trunk, at the
// body's height, is met side on as well.
function torsoBox(x, S) {
  if (!S.length) return null;
  const C = crawlShape();   // (ticket 286: crawling, the body as it lies, from where they stand: z, between the soles' fronts and backs)
  if (C) { const z = (Math.min(...S.map(s => s[2])) + Math.max(...S.map(s => s[3]))) / 2, t = C.torso; return [x + t[0], x + t[1], z + t[2], z + t[3], t[4], t[5]]; }
  const [a, b] = S.length < 2 || S[0][0] <= S[1][0] ? [S[0], S[1] || S[0]] : [S[1], S[0]], gap = b[0] - a[1], half = Math.max(gap / 2, 2 * (figureT.k || 1)), H = youHeights();
  return [x - half, x + half, a[2], a[3], H.hips, H.crown];
}
// (ticket 337: and, standing, their legs' box, ankle to hips, as the torso's across: what meets a thing above the ankles and
// below the hips, a table's top, a bed's frame; before it, nothing did. Before the torso's, which stays the last)
// (ticket 492) with a horse: ridden, its hooves, its body and the rider up on it; led, theirs and its, the torso's last
const youBoxes = (x, z) => { const HB = horseBoxes(x, z);
  if (HB && horseNow.ride) return HB.hooves.concat([HB.body, riderBox(x, z)]);
  const own = youBoxesOwn(x, z);
  return HB ? own.slice(0, -1).concat(HB.hooves, [HB.body], own.slice(-1)) : own; };
const youBoxesOwn = (x, z) => { const S = feetSoles(x, z), t = torsoBox(x, S); if (!t) return S;
  if (crawlShape()) return S.concat([t]);
  const H = youHeights(); return S.concat([[t[0], t[1], t[2], t[3], H.ankle, H.hips], t]); };
// a solid's boxes up from the ground: a trunk's bands, else one as tall as the thing (o.h)
const solidBands = o => o.bands || [{ x0: o.x0, x1: o.x1, lo: 0, hi: o.h > 0 ? o.h : Infinity }];
// (ticket 240, Matthew: "the swing's collision box does not swing at the tree house. our collision boxes don't swing
// either") The tree house's swing as it is now, read from its drawing (its seat's group is turned by the swing's own
// animation, which the screen matrix includes): the angle it hangs at, its hook on the screen, and how far its seat is
// from where it hangs at rest, across, in the stage's units. Its box (a seat's, solidsHere) is met where the seat is;
// 🔬 Inspection's collision boxes turn its box, and the friend's on it, about the hook (swingHud).
const SWING_HOOK = [395, -346], SWING_SEAT = [395, -66];   // (in the tree's units: treeInner's "tree-swing")
function swingNow() {
  const seat = state.background.kind === "treehouse" && bgShown.node ? bgShown.node.querySelector("[data-swing-seat]") : null, g = seat && seat.parentNode;
  const M = g && g.getScreenCTM(), P = g && g.parentNode.getScreenCTM(), S = pic.getScreenCTM();
  if (!M || !P || !S) return null;
  const Si = S.inverse(), at = (m, [x, y]) => new DOMPoint(x, y).matrixTransform(m), stage = q => q.matrixTransform(Si);
  return { deg: (Math.atan2(M.b, M.a) - Math.atan2(P.b, P.a)) * 180 / Math.PI, hook: at(P, SWING_HOOK), dx: stage(at(M, SWING_SEAT)).x - stage(at(P, SWING_SEAT)).x };
}
// (ticket 248: all, every one, and whether past a wall: { wall, list }; solidMove lets the friend out of the ones they
// are in, and into no other)
function solidAt(x, z, all = false) {
  const B = state.background, W = castleWalls(B), S = youBoxes(x, z), sw = B.kind === "treehouse" ? swingNow() : null;
  const wall = !!(W && S.some(s => Math.max(Math.abs(s[0]), Math.abs(s[1])) > W.half(z)));
  if (wall && !all) return { wall: true };
  const sitting = state.body.sit && B.seat, a = state.body.anim, jumping = isJump(a);   // (lying on a bed too: sit, ticket 299)
  const clears = jumping ? (a === "jumphigh" ? 1 : 0.5) * legReach() : 0, list = [];
  for (const o0 of solidsHere()) {
    const o = sw && o0.name === "swing" ? { ...o0, x0: o0.x0 + sw.dx, x1: o0.x1 + sw.dx } : o0;   // (ticket 240: where the seat is now)
    if (sitting && o.name === B.seat) continue;
    if (o.h < clears) continue;
    // (ticket 345: a trunk's bands met where they are drawn: a sole deep in its footprint, behind its foot, is level on the
    // screen with the trunk that much higher up, so it is measured against the band there, as the eye sees it; the tree
    // house's braid curving out at the ground no longer stops a friend standing beside it, further back)
    if (S.some(s => { if (!(s[3] > o.back && s[2] < o.front)) return false; const up = o.bands && !o.part ? Math.max(0, o.front - (s[2] + s[3]) / 2) : 0;
        if (o.oval && !ovalMeets(o, s[0], s[1], s[2], s[3])) return false;   // (ticket 337: its oval, not its box)
        return solidBands(o).some(b => s[1] > b.x0 && s[0] < b.x1 && s[5] + up > b.lo && s[4] + up < b.hi); })) {   // (a sole, or the torso's box, over one of its boxes, at its heights)
      if (!all) return o;
      list.push(o);
    }
  }
  return all ? { wall, list } : null;
}
// a move of the friend's feet from (x0, y0) to (x, y) (figX, figY): where
// they end up, sliding along a wall or a solid they meet; inside one
// already, any move that does not go deeper into it
// (ticket 229) the last thing a move was stopped by (a solid's name, or "wall"), and when: 🔬 Inspection's collision boxes glow it
let solidHit = { name: null, n: 0, t: 0 };
function solidMove(x0, y0, x, y) {
  const g = SCENE.ground;
  // (ticket 369, the speed run's collide176: walking into a small stone the friend crept 4 to 9 units into it: at the moments
  // of a stride when neither foot touched it, a step went through, then the front foot swung into it, again and again. A
  // step across is also tried with the feet where the stride's front foot reaches (its reach at the friend's size, as
  // planned walks keep: stridePlus), and refused when that meets something they are not already in)
  let hit = solidAt(x, g + y);
  const dx = x - x0, walking = Math.abs(dx) > 1e-6 && !isJump(state.body.anim);   // (any step across but a jump's: the pad walks with body.anim "none", drawn from the pad)
  if (!hit && walking) {
    // (the feet as they are, moved on till the front one is where the stride's front foot reaches: 0.45 of its widest, which is
    // the foot behind; measured in the witch's room, 25 ahead against 51 behind)
    const bx = youBoxes(x, g + y), front = dx < 0 ? x - Math.min(...bx.map(q => q[0])) : Math.max(...bx.map(q => q[1])) - x;
    const was0 = solidAt(x0, g + y0, true), lead = solidAt(x + Math.sign(dx) * Math.max(0, 0.45 * strideReach() - front), g + y);
    if (lead && !lead.wall && !was0.list.some(o => o.name === lead.name)) {   // (the step across refused outright: the fallbacks below
      // test only where the feet are now, which is clear, and let it through)
      const now = performance.now(); if (lead.name !== solidHit.name || now - solidHit.t > 120) solidHit = { name: lead.name, n: solidHit.n + 1, t: now };
      return solidAt(x0, g + y) ? [x0, y0] : [x0, y]; }
    // (and in one already, by a swung foot, not a step across further into it, toward its middle, even at a moment no foot
    // touches it: the depth kept, the step across refused; away from it, as any step)
    else if (was0.list.some(o => Math.abs(x - (o.x0 + o.x1) / 2) < Math.abs(x0 - (o.x0 + o.x1) / 2) - 0.01)) return [x0, y];
  }
  if (!hit) return [x, y];
  const was = solidAt(x0, g + y0, true);
  // (inside a solid already: let them out, by any move that does not go deeper into it, nearer its middle. 0.0.152: any
  // move at all, as it was, once the soles swung with the feet (0.0.150) let a walk through everything: the front foot
  // swung into a cauldron, a rock, the couch, and the next step counted as getting out)
  // (ticket 223: past a side wall already, a castle room's or a tree room's, come back from a door in it: any move toward the middle)
  // (ticket 248: out of every one they are in, and into no other: one foot inside a toy chest let a walk on into the tree
  // trunk beyond it, the move counted as getting out of the chest)
  const deeper = (o, xx, yy) => { const cx = (o.x0 + o.x1) / 2, cz = (o.back + o.front) / 2;
    return Math.abs(xx - cx) < Math.abs(x0 - cx) - 0.01 || Math.abs(g + yy - cz) < Math.abs(g + y0 - cz) - 0.01; };
  const ok = (xx, yy) => { const now = solidAt(xx, g + yy, true);
    if (now.wall && !(was.wall && Math.abs(xx) <= Math.abs(x0))) return false;
    return now.list.every(o => was.list.some(p => p.name === o.name) && !deeper(o, xx, yy)); };
  if ((was.wall || was.list.length) && ok(x, y)) return [x, y];
  { const now = performance.now(), name = hit.wall ? "wall" : hit.name;
    if (name !== solidHit.name || now - solidHit.t > 120) solidHit = { name, n: solidHit.n + 1, t: now }; }
  if (ok(x, y0)) return [x, y0];
  if (ok(x0, y)) return [x0, y];
  // (a wall alone: the farthest across it lets them, at the new depth)
  const W = castleWalls(state.background);
  if (W && !was.wall && !was.list.length) { const h = W.half(g + y) - feetSpan()[0] - 0.5, cx = clamp(x, -h, h); if (ok(cx, y)) return [cx, y]; }
  return [x0, y0];
}
// (ticket 215, Matthew: "when the player clicks a picture, or another
// passageway, we should plan a path for walking, swimming or climbing to it,
// and do it in a natural way, avoiding obstacles in the room, and walking (or
// swimming) around them"; "something very simple for now") A path for the
// feet, in figX/figY terms, from (x0, y0) to (x1, y1): straight when nothing
// stands in the way; else round the corners of the solids in the way (each
// grown by the margin the feet keep, and PATH_PAD more), the shortest, deeper
// counting 1/0.6 as far, as walking does. The solids are boxes on the floor
// (solidsHere), so a corner each is enough; a castle room's side walls only
// narrow toward the back, so a corner past them is left out, and a straight
// line between two points inside stays inside. A solid they start or end in
// is left out (let out, or let in). The points after the start, the end last;
// just the end when no way round is found. walkPath keeps the last one made,
// for showing it (ticket 217).
const PATH_PAD = 9;   // (5 till 0.0.201: a swinging foot grazed a cauldron's corner on a slow frame, ticket 236's run)
let walkPath = null;
function segBox(ax, az, bx, bz, x0, z0, x1, z1) {   // (does the segment pass through the box's inside? Liang-Barsky)
  let t0 = 0, t1 = 1; const dx = bx - ax, dz = bz - az;
  for (const [p, q] of [[-dx, ax - x0], [dx, x1 - ax], [-dz, az - z0], [dz, z1 - az]]) {
    if (p === 0) { if (q <= 0) return false; continue; }
    const r = q / p;
    if (p < 0) { if (r > t1) return false; if (r > t0) t0 = r; } else { if (r < t0) return false; if (r < t1) t1 = r; }
  }
  return t1 - t0 > 1e-6;
}
// (ticket 369) how much further the stride's front foot reaches than the feet as they stand now (the friend's own units: 22 + 0.44
// of the legs' length, at their size), for solidMove's step across
const strideReach = () => (22 + 0.44 * (state.body.legLen ?? 140)) * (figureT.k || 1);
const stridePlus = () => Math.max(0, (22 + 0.44 * (state.body.legLen ?? 140)) * (figureT.k || 1) - feetSpan()[0]);
function planPath(x0, y0, x1, y1, more = 0) {
  // (the boxes grown by the soles' reach either side, and their depth: the middle kept that far out; `more` further, for
  // a walk whose steps swing wider than the feet standing, ticket 251's)
  const [sx0, sz] = feetSpan(), sx = sx0 + more, g = SCENE.ground, z0 = g + y0, z1 = g + y1;
  // (0.0.157, Matthew: "planned path goes through the pit --- they don't seem registered in the collision system, but
  // they should have their own status as something to avoid when planning a path") a trap room's pits, the holes one
  // falls into (pitsHere, PIT_EDGE of each), gone round too, unless the walk starts or ends in one; it is the middle of
  // the feet that falls in, so a pit is kept PATH_PAD off, not grown by the soles' reach as a solid is
  const gx = o => o.pit ? PATH_PAD : sx, gz = o => o.pit ? PATH_PAD : sz;
  const inside = (o, x, z, e = 0) => { if (o.oval) { const E = grownOval(o, gx(o) + e, gz(o) + e); return ((x - E.cx) / E.rx) ** 2 + ((z - E.cz) / E.rz) ** 2 < 1; }   // (ticket 337)
    return x > o.x0 - gx(o) - e && x < o.x1 + gx(o) + e && z > o.back - gz(o) - e && z < o.front + gz(o) + e; };
  // (0.0.156: the soles' reach can touch a thing the walk starts or ends beside, the healer's soup pot, the bunk beds;
  // such a thing was left out altogether, and the walk went straight through it. Left out now only when the middle of
  // the feet is in it; touched, a leg from or to that end must miss its own box, not grown)
  const rawIn = (o, x, z) => o.oval ? ovalMeets(o, x, x, z, z) : x > o.x0 && x < o.x1 && z > o.back && z < o.front;
  const pits = pitsHere().map(q => ({ name: "pit", pit: true, x0: q.cx - PIT_EDGE * q.rx, x1: q.cx + PIT_EDGE * q.rx, back: q.cy - PIT_EDGE * q.ry, front: q.cy + PIT_EDGE * q.ry }));
  // (ticket 248: each across as wide as its boxes up to the friend's crown, as walking meets them, not its foot's alone: a
  // tree's trunk wider above its foot, the tree house's, had a planned walk brush through it)
  const crown = youHeights().crown, wide = o => { const bs = solidBands(o).filter(b => b.lo < crown);
    return bs.length ? { ...o, x0: Math.min(o.x0, ...bs.map(b => b.x0)), x1: Math.max(o.x1, ...bs.map(b => b.x1)) } : o; };
  // (ticket 366, Matthew: "when sitting, click to walk seems broken") the seat (or bed) being got off is in nobody's way, as
  // solidAt has it: on a throne the feet are behind its box, up on the dais, and since ticket 332 a plan with no way round
  // walks nowhere, so every walk from a throne went nowhere
  const leaving = state.body.sit && state.background.seat;
  const solids = solidsHere().map(wide).concat(pits).filter(o => !rawIn(o, x0, z0) && !rawIn(o, x1, z1) && !(leaving && o.name === leaving));
  const nearA = new Set(solids.filter(o => inside(o, x0, z0))), nearB = new Set(solids.filter(o => inside(o, x1, z1)));
  const hits = (ax, az, bx, bz, a = false, b = false) => solids.some(o => { const raw = (a && nearA.has(o)) || (b && nearB.has(o));
    if (o.oval) return segOval(ax, az, bx, bz, raw ? ovalOf(o) : grownOval(o, gx(o), gz(o)));   // (ticket 337)
    return raw ? segBox(ax, az, bx, bz, o.x0, o.back, o.x1, o.front) : segBox(ax, az, bx, bz, o.x0 - gx(o), o.back - gz(o), o.x1 + gx(o), o.front + gz(o)); });
  if (!solids.length || !hits(x0, z0, x1, z1, true, true)) return [[x1, y1]];
  const W = castleWalls(state.background), F = sceneFrame;
  const free = (x, z) => !solids.some(o => inside(o, x, z, 1)) && (!W || Math.abs(x) < W.half(z) - sx) && x > F.left && x < F.right && z - g >= figYMin(F) && z <= F.bottom;
  const pts = [[x0, z0], [x1, z1]];
  for (const o of solids) { const mx = gx(o) + PATH_PAD, mz = gz(o) + PATH_PAD;
    // (ticket 337: round an oval, eight points on an octagon just outside it, not its box's four corners)
    if (o.oval) { const E = grownOval(o, mx, mz), f = 1 / Math.cos(Math.PI / 8);
      for (let j = 0; j < 8; j++) { const a = j * Math.PI / 4 + Math.PI / 8, p = [E.cx + E.rx * f * Math.cos(a), E.cz + E.rz * f * Math.sin(a)]; if (free(...p)) pts.push(p); }
      continue; }
    for (const p of [[o.x0 - mx, o.back - mz], [o.x1 + mx, o.back - mz], [o.x0 - mx, o.front + mz], [o.x1 + mx, o.front + mz]]) if (free(...p)) pts.push(p); }
  // (ticket 332) a walk starting (or ending) against a solid, behind it near the back line where its back corners are past
  // the line and left out: the points just past its sides at that depth, the way out along the gap and round
  for (const [near, zz] of [[nearA, z0], [nearB, z1]]) for (const o of near) { const mx = gx(o) + PATH_PAD;
    for (const p of [[o.x0 - mx, zz], [o.x1 + mx, zz]]) if (free(...p)) pts.push(p); }
  const cost = (a, b) => Math.hypot(b[0] - a[0], (b[1] - a[1]) / 0.6), n = pts.length;
  const dist = Array(n).fill(Infinity), prev = Array(n).fill(-1), done = Array(n).fill(false);
  dist[0] = 0;
  for (;;) {   // (Dijkstra over the corners each sees: a few dozen at most)
    let u = -1;
    for (let i = 0; i < n; i++) if (!done[i] && dist[i] < Infinity && (u < 0 || dist[i] < dist[u])) u = i;
    if (u < 0 || u === 1) break;
    done[u] = true;
    for (let v = 0; v < n; v++) if (!done[v] && !hits(pts[u][0], pts[u][1], pts[v][0], pts[v][1], u === 0 || v === 0, u === 1 || v === 1)) {
      const d = dist[u] + cost(pts[u], pts[v]);
      if (d < dist[v]) { dist[v] = d; prev[v] = u; }
    }
  }
  // (ticket 332) no way round: never the straight line through what is in the way (as till 0.0.277, followed with no
  // collisions, through the trunk); as near the end as a way goes, the walk marked short (no sitting or going in after it)
  // (ticket 369, the speed run's back298: the friend walked into the tree house's lane between its bushes, the ladder at one end
  // and the trunk at the other, and with the stride's margin no plan led out; the margin is a wish, the feet's own reach is what
  // standing needs: no way with it, the way with the feet's own reach, before a short walk)
  if (dist[1] === Infinity && more > 0) { const again = planPath(x0, y0, x1, y1, 0); if (!again.short) return again; }
  let end = 1;
  if (dist[1] === Infinity) { end = 0; for (let i = 2; i < n; i++) if (dist[i] < Infinity && cost(pts[i], pts[1]) < cost(pts[end], pts[1])) end = i; }
  const out = [];
  for (let v = end; v > 0; v = prev[v]) out.unshift([pts[v][0], pts[v][1] - g]);
  if (end !== 1) out.short = true;
  return out;
}
// (after a jump: landed inside a solid, out of it)
// (ticket 248: the nearest spot across that is clear of every solid, back the way they came first when as near: a high
// jump stopped in the air by a tree just past a mushroom came down inside the mushroom, and its nearer edge was inside
// the tree)
// (at most `most` across: a walk's stop is settled a step at most, never carried off past what it stood between)
// (ticket 184, the controller audit: the friend arrived inside the healer's tea table, and in the tree house picked
// from the menu inside its trunk, x 0, where a step toward the middle never let them out) Set wherever the friend is
// put where a place says they arrive (ARRIVE); draw, once the new place is on the screen, moves them out of what they
// stand in, the nearest spot across that is clear.
let arriveSettle = false;
// (tickets 311 and 312, Matthew: "When we spawn from the S into the N part of a square pair --- it'd be better to be placed
// just off from the nav arrows, so the next click is not obscured by our body"; then, over the default world, 125 of its
// 138 ways in landed the friend on an arrow) Come in by a world way (arriveSide, the side come in from: worldEnter), once
// the place is drawn: with a nav arrow shown under the friend, they step aside to the nearest spot clear of every arrow
// that would show with them there (one shows as they near it), on the floor and in no solid; at equal steps inward from the east
// edge, to the right from anywhere else (Matthew's picture: to the right of the ⬆️ and ⬇️)
let arriveSide = null;
function arrowsClear(side, again = true) {
  const B = state.background, fig = document.getElementById("figure"), M = pic.getScreenCTM(), layer = document.getElementById("ways");
  if (!fig || !M || !layer || entering || B.onBridge || B.onPlatform || state.body.sit || climb) return;
  // (ticket 369, the speed run's arrows312: in the water the strokes swing the arms out and in, the friend's box 97 to 125 across
  // over a stroke; measured narrow, they stood a pad clear and an arm then covered the arrow: room for the stroke's swing there)
  const f = fig.getBoundingClientRect(), mv = moveNow(), pad = 6 + (mv && mv.water ? 0.15 * f.width : 0), meets = (t, px) => t.left - pad < f.right + px && f.left + px < t.right + pad && t.top < f.bottom && f.top < t.bottom;
  const tri = el => el.querySelector(".way-tri"), rect = el => tri(el) && tri(el).getBoundingClientRect();
  const shown = [...layer.querySelectorAll(".way-arrow.near")].map(rect).filter(t => t && t.width);
  if (!shown.some(t => meets(t, 0))) return;
  // (each arrow that would show bright with the friend there, near it: ♿'s all arrows shows the far ones dim, which a friend
  // may stand over, as on 🔬 Inspection's pages, where every arrow shows)
  const els = [...layer.children];
  const showsAt = x => { const keep = B.figX; B.figX = x; const on = worldWaysAt.map(([d], i) => wayNearness(d) < 1 ? els[i] && rect(els[i]) : null).filter(t => t && t.width); B.figX = keep; return on; };
  const F = sceneFrame, z = SCENE.ground + (B.figY ?? 0), x0 = B.figX ?? 0, s = Math.abs(M.a) || 1, pref = /E/.test(side) ? -1 : 1;
  for (let m = 6; m < layer.getBoundingClientRect().width * 0.6; m += 6) for (const px of [pref * m, -pref * m]) {
    const x = x0 + px / s;
    // (ticket 369, arrows312's arrivals in a solid: the first clear spot, -194.6 by the house's bucket, rounded to -195, back on its edge;
    // rounded the way the step goes, and that spot tested)
    const xr = px > 0 ? Math.ceil(x) : Math.floor(x);
    if (!(xr > F.left + 20 && xr < F.right - 20) || showsAt(xr).some(t => meets(t, px)) || solidAt(xr, z)) continue;
    B.figX = xr; changed();
    if (again) setTimeout(() => arrowsClear(side, false), 0);   // (once more: an arrow come near shows, and the arrows make room for it)
    return;
  }
}
function solidUnstick(most = 400) {
  const B = state.background, x = B.figX ?? 0, z = SCENE.ground + (B.figY ?? 0), o = solidAt(x, z);
  if (!o || o.wall) return;
  const F = sceneFrame, f = state.body.facing, back = f === "left" ? 1 : f === "right" ? -1 : x - o.x0 < o.x1 - x ? -1 : 1;
  for (let d = 1; d <= most; d++) for (const xx of [x + back * d, x - back * d])
    if (xx > F.left && xx < F.right && !solidAt(xx, z)) { B.figX = Math.round(xx + Math.sign(xx - x)); return; }
}

// The parts of the body each with its own visibility: a slider each
// (0 invisible to 100 all there), and the shirt and pants stand for the
// body and the legs under them.
const VISIBLE_PARTS = ["head", "eyes", "hair", "shirt", "pants", "feet", "hands"];
const visibility = () => Object.fromEntries(VISIBLE_PARTS.map(k =>
  [k, Math.max(0, Math.min(100, Number(state.visible?.[k] ?? 100))) / 100]));
// The masks the see-through layers need: each layer of a part but its last
// is hidden where the part's later layers are drawn (their shapes, inked
// black over white); the last one needs none.
function unveil(svg, layers) {
  // (ticket 161: without exact see-through, each layer its own see-through, no masks)
  if (!PERF.exactSeeThrough) return svg.replace(/ data-veil="(\w+) (\d+)"/g, "");
  let masks = "";
  for (const [g, N] of Object.entries(layers))
    for (let n = 1; n < N; n++) {
      masks += `<mask id="veil-m-${g}-${n}" maskUnits="userSpaceOnUse" x="-5000" y="-5000" width="10000" height="10000">` +
               `<rect x="-5000" y="-5000" width="10000" height="10000" fill="white"/>`;
      for (let m = n + 1; m <= N; m++) masks += `<use href="#veil-${g}-${m}" filter="url(#veil-ink)"/>`;
      masks += `</mask>`;
    }
  svg = svg.replace(/ data-veil="(\w+) (\d+)"/g, (_, g, n) => Number(n) < layers[g] ? ` mask="url(#veil-m-${g}-${n})"` : "");
  if (!masks) return svg;
  return `<defs><filter id="veil-ink" x="-0.5" y="-0.5" width="2" height="2" color-interpolation-filters="sRGB">` +
         `<feColorMatrix type="matrix" values="0 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 1 0"/></filter>${masks}</defs>` + svg;
}

// WATER (ticket 122): where in a place the friend stands in water. Each
// place with water to stand in has a test of whether a point on the stage
// (in its units, the ground line at 320) is in it, made from the same lines
// the place's drawing uses: lakeLines and lakeBeach, beachLines, poolOf,
// castleAt and moatOf, streamAt and brookAt. Under the sea is not here: the
// friend is all under water there already.
// (the stage as a scene function sees it, in the picture: backgroundSvg's c)
const stageC = () => { const x0 = SCENE.left, y0 = SCENE.top, x1 = -SCENE.left, y1 = SCENE.bottom, W = x1 - x0, H = y1 - y0;
  return { x0, y0, x1, y1, g: SCENE.ground, W, H, k: H / 700, pad: Math.max(W, H) }; };
// (a curve from a to b, bowed by q halfway across: its height at x, or null past its ends)
const bowY = (a, q, b, x) => { const t = (x - a[0]) / (b[0] - a[0]);
  return t < 0 || t > 1 ? null : (1 - t) * (1 - t) * a[1] + 2 * t * (1 - t) * q[1] + t * t * b[1]; };
// (the height at x of the wavy edge backgroundSvg's land(at, amp, fill, phase)
// draws: a bow to each twelfth of its width; keep the two alike)
const landY = ({ x0, W, k, pad }, at, amp, phase, x) => {
  const span = W + 2 * pad, i = Math.max(1, Math.min(12, Math.ceil((x - x0 + pad) / span * 12)));
  const xa = x0 - pad + span * (i - 1) / 12, xb = x0 - pad + span * i / 12;
  return bowY([xa, at], [xb - span / 24, at + amp * k * Math.sin(i * 1.7 + phase)], [xb, at], x) ?? at;
};
// (a winding ribbon of water, drawn through its points [middle, y, half its
// width] going down the picture, as a stream is (sampled): is (x, y) in it?)
const inRibbon = (pts, x, y) => {
  // (the first piece whose lower end is at or below y, found by halving)
  if (pts.length < 2 || y < pts[0][1] || y > pts[pts.length - 1][1]) return false;
  let lo = 1, hi = pts.length - 1;
  while (lo < hi) { const mid = (lo + hi) >> 1; if (pts[mid][1] < y) lo = mid + 1; else hi = mid; }
  const [xa, ya, ha] = pts[lo - 1], [xb, yb, hb] = pts[lo];
  const t = yb > ya ? (y - ya) / (yb - ya) : 0;
  return Math.abs(x - (xa + (xb - xa) * t)) < ha + (hb - ha) * t;
};
const WATER = {
  // the lake, from its far shore to its near one, but for the little beach
  lake: c => { const { horizon, shore } = lakeLines(c), [a, q, b] = lakeBeach(c, shore);
    return (x, y) => y > horizon && y < landY(c, shore, 5, 1.1, x) && y < (bowY(a, q, b, x) ?? Infinity); },
  // (ticket 186) open water: all of it, from the horizon down
  sea: c => { const hz = seaHorizon(c); return (x, y) => y > hz; },
  // the sea, from the horizon to the sand (the foam at its edge is sea too)
  beach: c => { const { sea, shore } = beachLines(c); return (x, y) => y > sea && y < landY(c, shore + 4 * c.k, 6, 1.4, x); },
  // the falls' pool, but where the near bank covers it
  waterfall: c => { const p = poolOf(c);
    return (x, y) => (x / p.rx) * (x / p.rx) + ((y - p.cy) / p.ry) * ((y - p.cy) / p.ry) < 1 && y < landY(c, p.near, 4, 1.1, x); },
  // the moat's near half, but for the drawbridge (its far half is behind the
  // walls, and a friend standing there is behind the whole castle); none
  // when the castle has no moat
  castle: (c, B) => { const K = castleOf(B); if (K.moat <= 0) return null;
    const { foot, z } = castleAt(c, K), M = moatOf(K.moat);
    return (x, y) => { const u = x / z, v = (y - foot) / z;
      return v > 0 && (u / M.rx) * (u / M.rx) + ((v - M.cy) / M.ry) * ((v - M.cy) / M.ry) < 1 && !(v < M.len && Math.abs(u) < bridgeHalf(v)); }; },
  // the tree house's stream and the cemetery's brook, as their outlines are drawn
  treehouse: c => { const pts = sampled(streamAt(c), 30); return (x, y) => inRibbon(pts, x, y); },
  cemetery: c => { const pts = sampled(brookAt(c), 20); return (x, y) => inRibbon(pts, x, y); },
};
// a place's test, or null where there is no water to stand in
const waterHere = B => WATER[B.kind]?.(stageC(), B) ?? null;
const inWater = (B, x, y) => { const at = waterHere(B); return !!at && at(x, y); };
// (a stream and a brook are shallow: in them the friend is in to the knees,
// not the waist)
const SHALLOW_WATER = { treehouse: true, cemetery: true };
// (how far the water reaches across from (x, y), to the nearer bank, up to
// `most`: a wake keeps to the water, in a narrow stream as in a lake)
const waterAcross = (B, x, y, most) => { const at = waterHere(B); let d = 0;
  while (d < most && at && at(x - d - 4, y) && at(x + d + 4, y)) d += 4;
  return d; };
// (the water's own shape round a spot on the stage: rows `step` apart, each
// the runs across it of points in the place's water, as [x, y, width])
// (ticket 163) Each row is worked out once for a place, across the whole
// stage and past it, and kept until the place's drawing changes; a drawing
// takes the rows round the friend and cuts them to its window. Before, the
// runs round the friend were worked out again whenever they moved, hundreds
// of tests of the place's water every drawing, and a swim in the duck pond
// drew 12 times a second on a computer 6 times slower than Matthew's.
const WATER_X0 = -1000, WATER_X1 = 1000;
let waterRows = { key: null, bg: null, rows: new Map() };
