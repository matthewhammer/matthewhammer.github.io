"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 1 of 26 of the game's script, run in order by index.html.

// ===================== STATE =====================

// Each tree house's own colors, for the kinds of house that have them: a
// cat's (a tuxedo cat to start), a fancy house's, a craftsman's. Kept per
// house: house3's cat can be orange while house5's is a tuxedo.
const HOUSE_COLOR_DEFAULTS = {
  cat: { catFur: "#1f1f22", catWhite: "#ffffff", catEyes: "#c6d43a" },
  fancy: { fancyWalls: "#d9c3e6", fancyRoof: "#6a4c93", fancyTrim: "#ffffff" },
  craftsman: { craftWalls: "#9aa982", craftRoof: "#5a3e2b", craftTrim: "#f4efe6", craftGable: "#c9b27a" },
};
// A figure's usual size in each background, a share of its full size: as
// big as the scene's things make sense beside it (small by a waterfall or
// a tree house, bigger by a couch). 100 on the size slider is this, in
// whatever scene: the slider grows or shrinks the friend from the place's
// own usual size. (Before 0.0.21, 100 was the full size everywhere: a
// friend saves which it was sized in, figUnit; see upgradeFigSize.)
// (ticket 50) where people can sit, per place: see seatsHere
const SEATS = {};
const SCENE_FIG = { forest: 0.45, underwater: 0.45, deepsea: 0.45, beach: 0.45, sea: 0.45, witch: 0.4, cemetery: 0.45, lake: 0.45, house: 0.35,
                    treehouse: 0.3, treeroom: 0.65, treetop: 0.6, livingroom: 0.7, witchroom: 0.6, healerroom: 0.6, hut: 0.42, hutroom: 0.7, hutbed: 0.62, hutpotion: 0.6, hutcenter: 0.6, fairyforest: 0.4, clearing: 0.45, mountains: 0.45,
                    waterfall: 0.37, campsite: 0.3, tent: 0.5, cave: 0.4, caveroom: 0.4, farmyard: 0.4, desert: 0.45, woodpath: 0.45, pasture: 0.45, crossing: 0.45, castle: 0.35, castleroom: 0.55, bubblespace: 0.3, bubbleroom: 0.6, imagination: 0.3, emptyroom: 0.3 };
const sceneFig = kind => SCENE_FIG[kind] ?? 0.45;
// (ticket 457, Matthew: "Maybe we add a body param for 'default size in scene' scale --- otherwise the gnome has to be re-set
// for each background as we play around with them") the friend's own size (body.size, a share), in every place, on top of
// the place's size for them (background.figSize): figScale, the two together, wherever the friend's size in a scene is
const bodyScale = () => Math.max(0.1, Math.min(5, (Number(state.body.size ?? 100) || 100) / 100));
const figScale = B => (B.figSize ?? 100) / 100 * bodyScale();
// (ticket 99) The friend is smaller farther back and bigger nearer, as a
// place draws what stands in it. A place's vanishing height is the y where
// a thing standing there would shrink to nothing (the horizon of its
// ground); the friend's size is their slider's times the place's own
// (SCENE_FIG), times (feet - vanish) / (arrival - vanish): exactly the
// place's own size where they arrive (ARRIVE, else the ground line, 320),
// kept between 0.3 and 1.5. A castle room's comes from its perspective
// (roomFrame: the eye's height on the wall); rooms drawn flatter, with big
// things at their back walls, vanish far above the picture.
const SCENE_VANISH = { treeroom: -60, treetop: -60, livingroom: -60, witchroom: -60, healerroom: -60, hutroom: -60, hutbed: -60, hutpotion: -60, hutcenter: -60, tent: -60, imagination: -60, emptyroom: -60, bubblespace: -60, bubbleroom: -60 };
// (ticket 214, Matthew: "this scene, and the forest clearing scene, and maybe others, should scale the player down more
// dramatically as they go 'into' the scene (heading N)"; "each picture should have its own setting for how much we get
// smaller as we move a certain number of fractions of picture N. north-ward scaling factor") Each place's 🔭 setting,
// background.north_<kind>: how much smaller the friend, and what is put there, gets for each tenth of the picture's
// height farther north than where they arrive, in percent; it sets the place's vanishing height (sceneVanish), so all
// of it shrinks as one. Outdoors 40 (the mountain trail's far edge about 0.4 of the size, the clearing's about 0.2);
// the rooms 16 and anywhere else 23, as their vanishing heights had it (-60, 60). A castle room keeps its own.
const NORTH_OUT = ["forest", "underwater", "deepsea", "witch", "cemetery", "house", "treehouse", "clearing", "fairyforest", "mountains",
                   "waterfall", "campsite", "cave", "castle", "woodpath", "pasture", "crossing", "beach", "sea", "lake", "desert", "farmyard"];   // (desert: ticket 501)
const NORTH_ROOMS = ["treeroom", "treetop", "livingroom", "witchroom", "healerroom", "hutroom", "hutbed", "hutpotion", "hutcenter", "tent", "imagination", "emptyroom", "bubblespace", "bubbleroom", "caveroom"];
const northDefault = kind => NORTH_OUT.includes(kind) ? 40 : NORTH_ROOMS.includes(kind) ? 16 : 23;
function sceneVanish(B) {
  if (B.kind === "castleroom") {
    const H = SCENE.bottom - SCENE.top, k = H / 700, top = SCENE.top + H * 0.1, floor = SCENE.ground - 110 * k;
    const style = castleOf(B).rooms[Math.round(B.castleRoom ?? 1) - 1];
    return top + (floor - top) * (style === "hallway" || style === "dining" ? 0.3 : 0.55);
  }
  const pct = B["north_" + B.kind];
  if (!Number.isFinite(pct)) return SCENE_VANISH[B.kind] ?? 60;
  return pct > 0 ? SCENE.ground + (ARRIVE[B.kind] || [0, 0])[1] - (SCENE.bottom - SCENE.top) / 10 / (pct / 100) : -Infinity;
}
// (0.0.153: as small as a tenth, not 0.3, so a deep place's far edge, the crossing's, the forest path's, is far off)
function depthSize(B, figY) {
  const v = sceneVanish(B), arrive = SCENE.ground + (ARRIVE[B.kind] || [0, 0])[1];
  if (!Number.isFinite(v)) return 1;   // (🔭 at 0: the same size anywhere)
  return Math.max(0.1, Math.min(1.5, (SCENE.ground + figY - v) / (arrive - v)));
}
// The three castles, N's, M's and D's, each with its own settings: how many
// levels (1 to 5), its stone, how many trees grow round it, and how wide
// its moat is (and so its drawbridge), from none to very wide. The forest
// clearing shows the one picked, far off; its own scene shows it up close.
const CASTLES = [["N", "🏰 N's castle", "#e53935"], ["M", "🏰 M's castle", "#f2c12e"], ["D", "🏰 D's castle", "#3b82f6"]];
const CASTLE_DEFAULTS = { N: { levels: 3, stone: "#b8b8c4", trees: 6, moat: 100 },
                          M: { levels: 4, stone: "#e2c9a0", trees: 10, moat: 60 },
                          D: { levels: 5, stone: "#7c8b6f", trees: 16, moat: 72 } };
const CASTLE_STONES = ["#b8b8c4", "#e2c9a0", "#8a8f98", "#e8e4da", "#c98f7a", "#7c8b6f", "#5a5f6e"];
// a castle's banners and flags: two colors of its owner's, used in turn,
// as stripes, or as checkers; and its roofs' color
const CASTLE_FLAG_COLORS = ["#e53935", "#f2c12e", "#3b82f6", "#22c55e", "#6a2c91", "#f472b6", "#f97316", "#ffffff", "#1f1f22"];
const CASTLE_FLAG_DEFAULTS = { N: ["#e53935", "#ffffff"], M: ["#f2c12e", "#6a2c91"], D: ["#6a2c91", "#22c55e"] };
// (each castle's own to start with: how its flags' two colors go, and its roofs; D's chosen by D, 2026-10-15)
const CASTLE_FLAG_STYLE_DEFAULTS = { N: "alternate", M: "alternate", D: "stripes" };
const CASTLE_ROOF_DEFAULTS = { N: "#3b5ea8", M: "#3b5ea8", D: "#7c3aed" };
const CASTLE_FLAG_STYLES = [["alternate", "↔️ in turn"], ["stripes", "▥ stripes"], ["checkers", "▦ checkers"]];
// a castle's carpets (the hallway's, the king's room's)
const CASTLE_CARPETS = ["#a3122a", "#1e3a8a", "#14532d", "#6a2c91", "#7c2d12", "#d4a017", "#1f1f22"];
// Each castle has two color themes, A and B, each its two colors, how they
// go (in turn, stripes, checkers) and a carpet color; each room takes one
// (its banners, shields and carpet), so two hallways or two king's rooms
// can look different. A is the castle's own (its flags outside too); B
// starts as A with its two colors swapped and its carpet in A's second
// color; rooms take A and B by turns to start with (odd rooms A).
const CARPET_COLORS = [...new Set([...CASTLE_CARPETS, ...CASTLE_FLAG_COLORS])];
const THEMES = [["A", "🎨 theme A"], ["B", "🎨 theme B"]];
// (ticket 294) a click on the ground while the friend is walking to one: a new walk there, in place of the one under way, or
// one more walk, after it (ticket 266's queue of clicks)
const CLICK_PLANS = [["replace", "🔁 walks there instead"], ["queue", "➕ walks there next, after"]];
// where the hallway's far end, the foot of a spiral staircase, leads: the
// castle's first spiral staircase room, a room of its own choosing, or nowhere
// A castle is rooms by number (up to ROOM_MAX), each with its own exits. A
// hallway's: up to four doors in its walls, each to the outside or to a room
// by its number (its own too), and the stairs at its far end. By default,
// only room 1 leads out: its first door does; in any other room, the first
// leads to room 1, and the others to the castle's other rooms in turn.
// Every other room has a way out of its own (castleOut: the king's room's
// sign toward us, the trap room's arch, a dungeon's ladder, the greenhouse's
// arch out), set per room; "the usual way" is out of the castle from room 1,
// up to the trap room from a dungeon, and back to room 1 from any other.
const ROOM_MAX = 13, ROOM_NS = [...Array(ROOM_MAX)].map((_, i) => i + 1);
// (a castle's rooms by number, as choices: only those it has)
const castleRoomCount = (b, id) => Math.max(1, Math.min(ROOM_MAX, Math.round(b["castleRooms" + id] ?? CASTLE_ROOMS[id].length)));
// (ticket 167) a room's style by the rule castleOf's room list follows, for
// a choice's label: castleOf builds the whole castle (every room's exits,
// flags and residents), and these labels are drawn for every door and
// hall end of every castle on every refresh of the 🏞️ Background tab;
// built there, it held the duck pond's swimming to 2 to 15 frames a second
const roomStyleOf = (b, id, n) => {
  if (n > castleRoomCount(b, id)) return "king";
  const st = b["castleRoom" + n + id] ?? CASTLE_ROOMS[id][n - 1] ?? "king";
  return (roomStyleSet ||= new Set(CASTLE_ROOM_CHOICES.map(([v]) => v))).has(st) ? st : "king";   // (ticket 197: a set, not a search, as it runs for every room's every chip)
};
let roomStyleSet = null;
const roomChoices = id => ROOM_NS.map(n => [String(n), b => `${roomLabel(roomStyleOf(b, id, n)).split(" ")[0]} room ${n}`, b => n <= castleRoomCount(b, id)]);
const hallDoorDests = id => [["out", "🏰 out of the castle"], ...roomChoices(id)];
const hallEnds = id => [["auto", "🌀 the first spiral stairs"], ...roomChoices(id), ["none", "🧱 nowhere"]];
const hallDoorKey = (id, n, j) => "castleHallDoor" + n + "_" + j + id;
// (how many doors a room's walls hold: a hallway four to a side, one pair to each stretch of it, and
// the dining room three to a side (ticket 113, M's idea); the other rooms two to a side)
const DOOR_MAX = 8, DOOR_JS = [...Array(DOOR_MAX)].map((_, j) => j + 1);
const roomDoorMax = st => ({ hall: 8, long: 6 })[ROOM_KINDS[st]?.exits] ?? 4;
const hallDoorDefault = (n, j) => String((n === 1 ? ["out", ...ROOM_NS.filter(m => m !== 1)] : [1, ...ROOM_NS.filter(m => m !== n && m !== 1)])[j - 1]);
// (ticket 233, Matthew: "each S exit of each room that has one can be: nothing, another castle room, outside of the
// castle") a room whose way out is toward us (S: the king's room, the dining room, the spiral stairs; ROOM_S_OUT) may
// lead nowhere too; another room's way out (an arch, a ladder) always leads somewhere
const ROOM_S_OUT = ["side", "long", "spiral"];
const wayOutDests = (id, n) => [["auto", "✨ the usual way"], ["none", "🧱 nowhere", b => !!n && ROOM_S_OUT.includes(ROOM_KINDS[roomStyleOf(b, id, n)]?.exits)],
                                ["out", "🏰 out of the castle"], ...roomChoices(id)];
// (the way toward us, at the bottom of the picture, a nav arrow when near: where it leads, if anywhere)
const backDests = id => [["none", "🧱 nowhere"], ["out", "🏰 out of the castle"], ...roomChoices(id)];
const CASTLE_ROOFS = ["#3b5ea8", "#b91c1c", "#2f6e3a", "#5a5f6e", "#7c3aed", "#c2410c", "#1f2937"];
// A castle's rooms: up to ROOM_MAX (13), each in a style. The styles are by who made
// them up (N's, M's and D's), and any castle may use any of them. A trap
// room's two pits drop into the castle's dungeons.
// The castle's room styles, one entry each: its label, whose idea it was
// (N's, or more besides), its drawing (r: its pits and its bones, for the
// trap room and the dungeons), its exits (side: doors in its side walls and a
// sign out, set per room, and a color theme, as the king's room; hall: a
// hallway's doors and stairs, set per room; trap: one arch out; ladder: a
// ladder up to the trap room; all: an arch to every other room) and its
// stone's tone. ROOM_STYLES, ROOM_TONE, SIDE_DOOR_ROOMS and the room's
// drawing (castleroomScene) and exits (castleWays) all come from this.
const ROOM_KINDS = {
  king: { label: "👑 king's room", by: "N", exits: "side", tone: "#a9a2b4", draw: (c, K) => kingRoomSvg(c, K) },
  greenhouse: { label: "🌿 greenhouse", by: "N", exits: "all", tone: "#c9c3b0", draw: (c, K) => greenhouseSvg(c, K) },
  trap: { label: "🕳️ trap room", by: "N", exits: "trap", tone: "#8f8a96", draw: (c, K, r) => trapRoomSvg(c, K, r.pits, r.pitShapes, r.trap) },
  dungeon1: { label: "🗝️ dungeon", by: "N", exits: "ladder", tone: "#6c6f7a", draw: (c, K, r) => dungeonSvg(c, K, false, r.bones) },
  dungeon2: { label: "⛓️ deep dungeon", by: "N", exits: "ladder", tone: "#5d6b62", draw: (c, K, r) => dungeonSvg(c, K, true, r.bones) },
  hallway: { label: "🕯️ hallway", by: "N", exits: "hall", tone: "#8a8597", draw: (c, K, R) => hallwaySvg(c, K, R) },
  catroom: { label: "🐈 cat room", by: "more", exits: "side", draw: (c, K) => catRoomSvg(c, K) },
  stable: { label: "🐴 stable", by: "more", exits: "side", draw: (c, K, r) => stableSvg(c, K, r.hens) },   // (ticket 506: and its hens)
  dining: { label: "🍽️ dining room", by: "M", exits: "long", tone: "#9c8f84", draw: (c, K, r) => diningRoomSvg(c, K, r) },
  kitchen: { label: "🍲 kitchen", by: "more", exits: "side", tone: "#b8a58c", draw: (c, K) => kitchenSvg(c, K) },
  smithy: { label: "⚒️ smithy", by: "more", exits: "side", tone: "#5e5650", draw: (c, K) => smithySvg(c, K) },
  spiral: { label: "🌀 spiral staircase", by: "N", exits: "spiral", tone: "#8d8796", draw: (c, K) => spiralSvg(c, K) },
};
const ROOM_STYLES = [["N", "N's rooms"], ["M", "M's rooms"], ["more", "more rooms"]].map(([by, name]) =>
  [by, name, Object.entries(ROOM_KINDS).filter(([, R]) => R.by === by).map(([st, R]) => [st, R.label])]);
// the rooms whose exits are their own (K.hall): a hallway's doors and stairs; the
// others', doors in their side walls, as the king's room's, and a sign out toward
// us; each of these rooms also takes a color theme
const SIDE_DOOR_ROOMS = Object.keys(ROOM_KINDS).filter(st => ["side", "long", "spiral"].includes(ROOM_KINDS[st].exits)), OWN_EXIT_ROOMS = ["hallway", ...SIDE_DOOR_ROOMS];
// The cat room's cats: the castle's own, up to eight, each with its fur, its
// tuxedo white (or none), its eyes, how it is (sitting, asleep, walking), where
// in the room it is, its size, and its wag: with an LFO (in time with the
// music) or its own, and how far into the wag it starts, each its own (at
// random to begin with), so no two wag together
const CAT_FURS = ["#1f1f22", "#6f6f7d", "#e8862a", "#6b4226", "#ffffff", "#d9b38c", "#8b5cf6"];
const CAT_WHITES = ["#ffffff", "off", "#f7ead8", "#ffd1dc", "#c0c0c0", "#1f1f22"];
const CAT_EYES = ["#c6d43a", "#e0a800", "#3b82f6", "#22c55e", "#f28c28"];
const CAT_POSES = [["sit", "🐈 sitting"], ["sleep", "😴 curled up asleep"], ["walk", "🐾 walking"]];
const CAT_SPOTS = [["fire", "🔥 by the fire"], ["treeL", "🌳 on the left cat tree"], ["walkway", "🌉 on the high walkway"], ["window", "🪟 in the window seat"],
                   ["treeR", "🌳 on the right cat tree"], ["basket", "🧺 in the basket"], ["shelfL", "🪵 on the left shelf"], ["shelfR", "🪵 on the right shelf"],
                   ["mantel", "🕯️ on the mantel"], ["rug", "🧶 on the rug"], ["box", "📦 in the box"], ["bridge", "🪢 on the rope bridge"]];
const CAT_ROOM_CATS = [
  { fur: "#e8862a", white: "#ffffff", eyes: "#22c55e", pose: "sleep", spot: "fire", offset: 17 },
  { fur: "#1f1f22", white: "#ffffff", eyes: "#c6d43a", pose: "sit", spot: "treeL", offset: 62 },
  { fur: "#6f6f7d", white: "off", eyes: "#e0a800", pose: "walk", spot: "walkway", offset: 38 },
  { fur: "#ffffff", white: "off", eyes: "#3b82f6", pose: "sit", spot: "window", offset: 85 },
  { fur: "#6b4226", white: "#f7ead8", eyes: "#f28c28", pose: "sit", spot: "treeR", offset: 9 },
  { fur: "#6f6f7d", white: "#ffffff", eyes: "#22c55e", pose: "sleep", spot: "basket", offset: 51 },
  { fur: "#1f1f22", white: "off", eyes: "#e0a800", pose: "sit", spot: "box", offset: 74 },
  { fur: "#d9b38c", white: "#ffffff", eyes: "#3b82f6", pose: "walk", spot: "bridge", offset: 29 }];
const CAT_ROOM_COUNT = 6;
const catKey = (id, j, what) => "castleCat" + j + what + id;
// The stable's horses: the castle's own, up to four, one to a stall, each
// with its coat, its mane, the marking on its face, and its nod: with an LFO
// or its own, and how far into the nod it starts, each its own
const HORSE_COATS = ["#7b4a2a", "#a0522d", "#2a2a2e", "#c9c9cf", "#f3efe6", "#d9a95b", "#5a3a22"];
const HORSE_MANES = ["#1f1f22", "#5a3a22", "#7a3a1a", "#f3efe6", "#c0c0c0", "#f3e6c8", "#8b5cf6"];
const HORSE_MARKS = [["none", "none"], ["star", "⭐ a star"], ["stripe", "❕ a stripe"], ["blaze", "⬜ a blaze"]];
// (ticket 497, for M: "can we adjust the mane style? the default is good -- can we think of 5 others, to bring the total to
// 6?") the friend's horse's mane: loose as it always was, and five more (the stable's horses keep theirs loose)
const HORSE_MANE_STYLES = [["loose", "🐴 loose (the usual)"], ["long", "🌊 long and flowing"], ["buttons", "🔘 button braids"], ["brush", "🪮 short, standing up"], ["curly", "➰ curly"], ["bows", "🎀 braids with bows"]];
// (ticket 498, Matthew: "let's start to think of costumes for the knight as having horse pieces (face helmets, look at images
// from Lego castles and knights) -- not just saddles"; "just release and let the kids decide") the friend's horse's armour:
// none, or one of the six sets in the mock-up (horse-armour.html, outside the repo), all of them for the kids to choose from
const HORSE_ARMOURS = [["none", "none"], ["plate", "🛡️ full plate"], ["half", "🦁 half caparison"], ["tourney", "🎪 tournament"], ["dragon", "🐉 dragon helm"], ["toy", "🧱 toy helmet"], ["parade", "🎀 parade"]];
const STABLE_HORSES = [
  { coat: "#7b4a2a", mane: "#1f1f22", mark: "star", offset: 23 },
  { coat: "#c9c9cf", mane: "#f3efe6", mark: "none", offset: 71 },
  { coat: "#a0522d", mane: "#7a3a1a", mark: "blaze", offset: 46 },
  { coat: "#d9a95b", mane: "#f3e6c8", mark: "stripe", offset: 8 }];
const STABLE_HORSE_COUNT = 3;
const horseKey = (id, j, what) => "castleHorse" + j + what + id;
// (ticket 506, for M; Matthew: "chickens -- already in stable 30 in stable.", then "just release and let the kids decide")
// how many hens peck about a stable's floor: each stable room its own, 0 to 30, two to start with, as it always had
const STABLE_HENS = 2, STABLE_HENS_MAX = 30;
const stableHensKey = (n, id) => `castleHens${n}${id}`;
// The castle's residents, by kind: who lives in which room style, up to how
// many (`count`: the setting, its most, how many to start with), and each
// one's settings (`fields`: [the key's last word, its control, its label,
// its colors, choices, [min, max, everyday min, everyday max] or, for an LFO,
// the "none" choice's name, and its default, from the kind's list]). From
// this come the settings' keys and defaults (DEFAULTS), castleOf's lists, the
// controls (TABS), when they show (SHOWN_WHEN) and the mixing board's names.
// (a cat's settings, the cat room's and a throne's cat's alike: ticket 293)
const CAT_FIELDS = [["Fur", "color", "🐈 its fur", CAT_FURS, d => d.fur], ["White", "color", "🤍 tuxedo white (muzzle and bib)", CAT_WHITES, d => d.white],
                    ["Eyes", "color", "👀 its eyes", CAT_EYES, d => d.eyes], ["Pose", "choice", "🐾 it is", CAT_POSES, d => d.pose],
                    ["Spot", "choice", "📍 where it is", CAT_SPOTS, d => d.spot], ["Size", "range", "📏 its size", [20, 300, 80, 120], () => 100],
                    ["Lfo", "lfo", "〰️ its tail wags with", "its own wag", () => "D"], ["Offset", "range", "↪️ how far into the wag it starts (%)", [0, 99, 0, 99], d => d.offset]];
// (ticket 293, Matthew: "Each king's room permits up to 6 thrones, each with a size and a theme control. The default,
// going forward, is 2 (a king's and a queen's), in theme A and theme B"; "🐈 has a cat ... and with a cat, all of the
// cat's options"; "please try to reuse code where possible --- these are the same controls as the cat room") A throne's
// own settings, then its cat's: the cat room's own (but where it sits: on the throne), shown while it has one (a field's
// sixth, when it shows); the cat's size is CatSize, the throne's Size
const KING_THRONES = [
  { theme: "A", cat: "none", fur: "#e8862a", white: "#ffffff", eyes: "#22c55e", pose: "sit", offset: 12 },
  { theme: "B", cat: "none", fur: "#1f1f22", white: "#ffffff", eyes: "#c6d43a", pose: "sleep", offset: 57 },
  { theme: "A", cat: "none", fur: "#6f6f7d", white: "off", eyes: "#e0a800", pose: "sit", offset: 33 },
  { theme: "B", cat: "none", fur: "#ffffff", white: "off", eyes: "#3b82f6", pose: "walk", offset: 81 },
  { theme: "A", cat: "none", fur: "#6b4226", white: "#f7ead8", eyes: "#f28c28", pose: "sleep", offset: 5 },
  { theme: "B", cat: "none", fur: "#d9b38c", white: "#ffffff", eyes: "#3b82f6", pose: "sit", offset: 46 }];
const RESIDENTS = {
  cats: { room: "catroom", key: "castleCat", list: CAT_ROOM_CATS, count: ["castleCats", 8, CAT_ROOM_COUNT],
          group: ["castleCatList", id => `🐈 ${id}'s castle: its cats, in its cat room`, "🐈 how many cats"], box: ["castleCatBox", j => `🐈 cat ${j}`],
          follows: i => `🐈 cat ${i + 1}'s tail`, fields: CAT_FIELDS },
  thrones: { room: "king", key: "castleThrone", list: KING_THRONES, count: ["castleThrones", 6, 2],
             group: ["castleThroneList", id => `👑 ${id}'s castle: its thrones, in its king's room`, "👑 how many thrones"], box: ["castleThroneBox", j => `👑 throne ${j}`],
             follows: i => `🐈 throne ${i + 1}'s cat's tail`, followsIf: one => one.cat === "cat",
             fields: [["Size", "range", "📏 its size", [40, 200, 70, 140], () => 100], ["Theme", "choice", "🎨 its colors", THEMES, d => d.theme],
                      ["Cat", "choice", "🐈 a cat on it", [["none", "no cat"], ["cat", "🐈 a cat"]], d => d.cat],
                      ...CAT_FIELDS.filter(f => f[0] !== "Spot").map(f => f[0] === "Size" ? ["CatSize", f[1], "📏 the cat's size", f[3], f[4], one => one.cat === "cat"] : [...f, one => one.cat === "cat"])] },
  horses: { room: "stable", key: "castleHorse", list: STABLE_HORSES, count: ["castleHorses", 4, STABLE_HORSE_COUNT],
            group: ["castleHorseList", id => `🐴 ${id}'s castle: its horses, in its stable`, "🐴 how many horses"], box: ["castleHorseBox", j => `🐴 horse ${j}`],
            follows: i => `🐴 horse ${i + 1}'s nod`,
            fields: [["Coat", "color", "🐴 its coat", HORSE_COATS, d => d.coat], ["Mane", "color", "💈 its mane", HORSE_MANES, d => d.mane],
                     ["Mark", "choice", "⭐ on its face", HORSE_MARKS, d => d.mark], ["Lfo", "lfo", "〰️ it nods with", "its own nod", () => "E"],
                     ["Offset", "range", "↪️ how far into the nod it starts (%)", [0, 99, 0, 99], d => d.offset]] },
};
// a resident's setting, as saved (v), made good: a choice one of its own, a
// number within its range, an LFO one there is (or none), else its default
const residentField = ([, kind, , opts, dflt], v, d) => kind === "choice" ? (opts.some(([o]) => o === v) ? v : dflt(d))
  : kind === "range" ? Math.max(opts[0], Math.min(opts[1], v ?? dflt(d)))
  : kind === "lfo" ? (v === "none" ? "none" : LFO_NAMES.includes(v) ? v : dflt(d)) : v ?? dflt(d);
// a castle's residents of a kind, as castleOf lists them ({ fur, white, ... })
const residentsOf = (R, o, id) => R.list.slice(0, Math.max(0, Math.min(R.count[1], Math.round(o[R.count[0] + id] ?? R.count[2]))))
  .map((d, i) => Object.fromEntries(R.fields.map(f => [f[0].toLowerCase(), residentField(f, o[R.key + (i + 1) + f[0] + id], d)])));
const CASTLE_ROOM_CHOICES = ROOM_STYLES.flatMap(([, , list]) => list);
const roomLabel = st => (CASTLE_ROOM_CHOICES.find(([v]) => v === st) || [st, st])[1];
// skeletons and skulls: none unless asked for, room by room (0 up to what
// the room's drawing has); the dungeons are the rooms that have them
const ROOM_BONES = { dungeon1: 2, dungeon2: 4 };
const roomBonesKey = (id, n, st) => "castleBones" + n + id + st;
const CASTLE_ROOMS = { N: ["king", "greenhouse", "trap", "dungeon1", "dungeon2"], M: ["greenhouse", "king"], D: ["king", "trap", "dungeon1"] };
// (ticket 271) a trap room's pit's opening, by its castle and its room: pit i (1, 2) of room n
const pitShapeKey = (i, n, id) => `castlePit${i}_${n}${id}`;
// (ticket 305, Matthew: "up to two pits L and R (toggle for that in the background settings). and two levers, L and R. and wired
// up either normal, or reversed, or random (potentially one lever for two pits)") A trap room's own, by its room number, as its
// pits' shapes are: each pit there or not; each lever pulled or not; how the levers work the pits; and, wired at random, a seed
// drawn again each time the friend comes in (trapReshuffle). A pit opens when the lever working it is up, and shuts, planks over
// it, when that lever is pulled.
const pitOnKey = (i, n, id) => `castlePitOn${i}_${n}${id}`, leverKey = (side, n, id) => `castleLever${side}_${n}${id}`;
// (ticket 334, Matthew: "add a spikes toggle for the other pits in the castle, and default to 'off'") a trap room's pits' spikes, each room's own
const pitSpikesKey = (n, id) => `castlePitSpikes${n}${id}`;
const leverWireKey = (n, id) => `castleLeverWire${n}${id}`, leverSeedKey = (n, id) => `castleLeverSeed${n}${id}`;
const LEVER_WIRES = [["normal", "↕️ normal: each its own side's pit"], ["reversed", "🔀 reversed: each the other side's"], ["random", "🎲 random, new each time you come in"]];
// which lever works each pit, the left and the right: "L" or "R" (at random, perhaps one lever for both)
function trapWiring(o, n, id) {
  const mode = o[leverWireKey(n, id)] || "normal";
  if (mode === "reversed") return ["R", "L"];
  if (mode === "random") { const r = seeded(o[leverSeedKey(n, id)] ?? 1); return [0, 1].map(i => r(i) < 0.5 ? "L" : "R"); }
  return ["L", "R"];
}
// (ticket 307, Matthew: "let's have them closed by default now that we can open them with the switch") a lever never
// touched is pulled down, so its pit is shut; one pulled is as it was set
const leverDown = (o, side, n, id) => !!(o[leverKey(side, n, id)] ?? true);
// (ticket 323, N, through Matthew: "another pair of levers, mounted under the current pair (let's use a distinct color for
// the knob, like blue ...). In this case, the blue levers do not shut the doors, but rather let 'boulders' fall from the
// ceiling of the room down to cover the holes") a trap room's boulder levers: a blue-knobbed pair under the red one (there
// unless the room says not, boulderOnKey); each, pulled down, lets the boulder of the pit it works (by the room's wiring,
// as the red pair) fall from the ceiling into the hole, which is then no way down; pulled up again, the boulder rises
// away. The boulder's rock: a tone of the rocks put in places (ROCK_TONES), or a surprise; its shape, and a surprise's
// tone, drawn from a seed set each time one falls (boulderSeedKey), so the settings decide the picture.
const boulderOnKey = (n, id) => `castleBoulders${n}${id}`, boulderKey = (side, n, id) => `castleBoulder${side}_${n}${id}`;
// (ticket 328: a seed for each pit's boulder, set only when that boulder falls: one for the room made the right lever's
// boulder draw the left one anew, Matthew: "switching the R switch replaces the L rock")
const boulderToneKey = (n, id) => `castleBoulderTone${n}${id}`, boulderSeedKey = (i, n, id) => `castleBoulderSeed${i}_${n}${id}`;
// (ticket 328, Matthew: "params in the room for each, in terms of animation speed") how fast its levers move, its boulders
// fall and its trap doors swing: a share of each one's usual time (100), faster above
const leverSpeedKey = (n, id) => `castleLeverSpeed${n}${id}`, boulderSpeedKey = (n, id) => `castleBoulderSpeed${n}${id}`, lidSpeedKey = (n, id) => `castleLidSpeed${n}${id}`;
// (ticket 338, Matthew: "let's let them be placeable -- so we as the user/tester can move them after they fall ... maybe a special
// name 'fallen 1, 2, 3'") a boulder that has landed is a placed rock ("fallen N", fallen: N, pit: its pit's key, px pz: where it
// landed, prad: how big the pit's rock is); it covers its pit while it lies about where it landed, dragged off it the pit is open
const fallenPitKey = (n, id, i) => `${id}:${n}:${i}`;
const fallenNear = t => Math.hypot(t.x - t.px, (t.z - t.pz) / 0.6) < 0.4 * (t.prad || 80);
const fallenCover = o => (Array.isArray(o.things) ? o.things : []).filter(t => t.fallen && fallenNear(t)).map(t => t.pit).sort().join(",");
// (in a scene, read through sceneOpts as one string, so moving a rock draws the room again; else from the things themselves)
const fallenOn = (o, key) => { const c = o["fallen:cover"]; return typeof c === "string" ? c.split(",").includes(key) : (Array.isArray(o.things) ? o.things : []).some(t => t.fallen && t.pit === key && fallenNear(t)); };
function trapState(o, n, id) {
  const wire = trapWiring(o, n, id), levers = { L: leverDown(o, "L", n, id), R: leverDown(o, "R", n, id) };
  const boulders = o[boulderOnKey(n, id)] !== false ? { L: !!o[boulderKey("L", n, id)], R: !!o[boulderKey("R", n, id)] } : null;
  return { wire, levers, on: [1, 2].map(i => o[pitOnKey(i, n, id)] !== false), open: [0, 1].map(i => !levers[wire[i]]),
           // (ticket 338: covered, a fallen rock lying on it)
           boulders, covered: [0, 1].map(i => !!boulders && fallenOn(o, fallenPitKey(n, id, i))), tone: o[boulderToneKey(n, id)] || "#8f8a80", seeds: [0, 1].map(i => o[boulderSeedKey(i, n, id)] ?? 5 + i),
           speed: { lever: o[leverSpeedKey(n, id)] || 100, boulder: o[boulderSpeedKey(n, id)] || 100, lid: o[lidSpeedKey(n, id)] || 100 },
           spikes: !!o[pitSpikesKey(n, id)] };   // (ticket 334)
}
// (wired at random: drawn again each time the friend comes into the room, so each visit is a puzzle of its own)
function trapReshuffle(B) {
  const K = castleOf(B), n = Math.max(1, Math.min(K.rooms.length, Math.round(B.castleRoom ?? 1)));
  if (K.rooms[n - 1] === "trap" && B[leverWireKey(n, K.id)] === "random") B[leverSeedKey(n, K.id)] = 1 + Math.floor(Math.random() * 999999);
}
// (ticket 365, castlefs365: castleOf({ ...state.background, castle: id }) copied the background's ~1,900 settings on every call, 0.4 ms,
// and 84 theme flags made one each on every change, 31 ms of the Background tab's 37 in the app's pane) castleOf of another castle
// than the one chosen, read through a view: the settings themselves, the castle named over them (0.03 ms, the same castle)
const castleView = (o, id) => Object.create(o, { castle: { value: id, enumerable: true } });
const castleOf = o => {
  const id = CASTLE_DEFAULTS[o.castle] ? o.castle : "N", d = CASTLE_DEFAULTS[id];
  const fl = CASTLE_FLAG_DEFAULTS[id], flagA = o["castleFlagA" + id] ?? fl[0];
  return { id, flag: flagA, flagA, flagB: o["castleFlagB" + id] ?? fl[1],
           flagStyle: CASTLE_FLAG_STYLES.some(([v]) => v === o["castleFlagStyle" + id]) ? o["castleFlagStyle" + id] : CASTLE_FLAG_STYLE_DEFAULTS[id],
           roof: o["castleRoof" + id] ?? CASTLE_ROOF_DEFAULTS[id], carpet: o["castleCarpet" + id] ?? CASTLE_CARPETS[0],
           themeB: { flagA: o["castleFlagA_B" + id] ?? fl[1], flagB: o["castleFlagB_B" + id] ?? fl[0], carpet: o["castleCarpet_B" + id] ?? fl[1],
                     flagStyle: CASTLE_FLAG_STYLES.some(([v]) => v === o["castleFlagStyle_B" + id]) ? o["castleFlagStyle_B" + id] : CASTLE_FLAG_STYLE_DEFAULTS[id], theme: "B" },
           roomTheme: ROOM_NS.map(n => ["A", "B"].includes(o["castleRoomTheme" + n + id]) ? o["castleRoomTheme" + n + id] : n % 2 ? "A" : "B"),
           // each room's own exits, when it is a hallway (castleWays)
           hall: ROOM_NS.map(n => ({
             doors: [...Array(Math.max(0, Math.min(roomDoorMax(o["castleRoom" + n + id] ?? CASTLE_ROOMS[id][n - 1] ?? "king"), Math.round(o["castleHallDoors" + n + id] ?? 3))))].map((_, j) => String(o[hallDoorKey(id, n, j + 1)] ?? hallDoorDefault(n, j + 1))),
             end: String(o["castleHallEnd" + n + id] ?? "auto"), out: String(o["castleOut" + n + id] ?? "auto"), back: String(o["castleBack" + n + id] ?? "none"), far: String(o["castleFar" + n + id] ?? "none") })),
           levels: Math.max(1, Math.min(5, Math.round(o["castleLevels" + id] ?? d.levels))),
           // (ticket 252: the trap room's two pits' openings, the same in the dungeons' ceilings below them)
           // (ticket 271, Matthew: "the trap room pit style is a property of the room number, not all pit rooms") each room's own:
           // pitShapesAt[n - 1], the castle's old setting where a room has none of its own
           pitShapes: [1, 2].map(i => openingShape(o["castlePitShape" + i + id] ?? "circle")),
           pitShapesAt: ROOM_NS.map(n => [1, 2].map(i => openingShape(o[pitShapeKey(i, n, id)] ?? o["castlePitShape" + i + id] ?? "circle"))),
           stone: o["castleStone" + id] ?? d.stone, trees: Math.max(0, Math.round(o["castleTrees" + id] ?? d.trees)),
           moat: Math.max(0, (o["castleMoat" + id] ?? d.moat) / 100),
           rooms: [...Array(Math.max(1, Math.min(ROOM_MAX, Math.round(o["castleRooms" + id] ?? CASTLE_ROOMS[id].length))))].map((_, i) => {
             const st = o["castleRoom" + (i + 1) + id] ?? CASTLE_ROOMS[id][i] ?? "king";
             return CASTLE_ROOM_CHOICES.some(([v]) => v === st) ? st : "king"; }),
           places: ROOM_NS.map(n => Math.max(4, Math.min(18, Math.round(o["castlePlaces" + n + id] ?? 10)))),
           // (ticket 506) each stable's hens, by its room number (a number that is none: the two it always had)
           hens: ROOM_NS.map(n => { const v = Math.round(o[stableHensKey(n, id)] ?? STABLE_HENS); return Number.isFinite(v) ? Math.max(0, Math.min(STABLE_HENS_MAX, v)) : STABLE_HENS; }),
           bones: ROOM_NS.map(n => Object.fromEntries(Object.entries(ROOM_BONES).map(([st, most]) =>
             [st, Math.max(0, Math.min(most, Math.round(o[roomBonesKey(id, n, st)] ?? 0)))]))),
           // its residents: its cats, who live in its cat room (or rooms), its horses, in its stable
           ...Object.fromEntries(Object.entries(RESIDENTS).map(([kind, R]) => [kind, residentsOf(R, o, id)])) };
};
// the fill for a castle's i-th flag or banner: its two colors in turn, or
// both, as stripes or checkers (a pattern, its squares u across)
const castleFlagFill = (K, i, u = 6) => K.flagStyle === "alternate" ? (i % 2 ? K.flagB : K.flagA) : `url(#cflag-${K.id}${K.theme ?? "A"}-${K.flagStyle}-${Math.round(u * 10)})`;
const castleFlagDefs = (K, u = 6) => K.flagStyle === "alternate" ? "" :
  `<defs><pattern id="cflag-${K.id}${K.theme ?? "A"}-${K.flagStyle}-${Math.round(u * 10)}" patternUnits="userSpaceOnUse" width="${f1(2 * u)}" height="${f1(2 * u)}">` +
  `<rect width="${f1(2 * u)}" height="${f1(2 * u)}" fill="${K.flagA}"/>` +
  (K.flagStyle === "stripes" ? `<rect x="${f1(u)}" width="${f1(u)}" height="${f1(2 * u)}" fill="${K.flagB}"/>`
                             : `<rect width="${f1(u)}" height="${f1(u)}" fill="${K.flagB}"/><rect x="${f1(u)}" y="${f1(u)}" width="${f1(u)}" height="${f1(u)}" fill="${K.flagB}"/>`) + `</pattern></defs>`;
// the owner's letter, readable on any banner: dark on a light one, white on
// a dark one, outlined in the other; on stripes and checkers, on a round
// badge of its own (the outline's color)
const inkOn = hex => { const [r, g, b] = hexToRgb(hex).map(v => v / 255); return 0.299 * r + 0.587 * g + 0.114 * b > 0.6 ? ["#1f1f22", "#ffffff"] : ["#ffffff", "#1f1f22"]; };
const castleLetter = (K, i, x, y, size, family = "sans-serif") => {
  const [ink, edge] = inkOn(K.flagStyle === "alternate" ? (i % 2 ? K.flagB : K.flagA) : K.flagA);
  return (K.flagStyle === "alternate" ? "" : `<circle cx="${f1(x)}" cy="${f1(y - size * 0.36)}" r="${f1(size * 0.62)}" fill="${edge}" stroke="${ink}" stroke-width="${f1(size * 0.06)}"/>`) + `<text x="${f1(x)}" y="${f1(y)}" text-anchor="middle" font-size="${f1(size)}" font-weight="bold" fill="${ink}" stroke="${edge}" stroke-width="${f1(size * 0.16)}" paint-order="stroke" stroke-linejoin="round" font-family="${family}">${K.id}</text>`;
};

// The fairy: wings in five styles and a tunic in five, each style with its
// own colors (the first its usual one); shoes of the fairy's own; her own
// hair, face and hands. Her wings flutter.
const FAIRY_WINGS = {
  butterfly: { name: "🦋 butterfly", colors: ["#f59e0b", "#60a5fa", "#f472b6", "#a78bfa", "#34d399", "#fde047"] },
  dragonfly: { name: "🪰 dragonfly", colors: ["#bfe9ff", "#d9f99d", "#e9d5ff", "#fef3c7", "#ffffff"] },
  leaf:      { name: "🍃 leaf", colors: ["#4ade80", "#84cc16", "#facc15", "#f97316", "#a3e635"] },
  petal:     { name: "🌸 petal", colors: ["#f9a8d4", "#ffffff", "#fde68a", "#c4b5fd", "#fca5a5"] },
  sparkle:   { name: "✨ sparkle", colors: ["#e9d5ff", "#bae6fd", "#fbcfe8", "#fef9c3", "#d1fae5"] },
};
const FAIRY_TUNICS = {
  petal:     { name: "🌸 petals", colors: ["#f9a8d4", "#fda4af", "#fde68a", "#c4b5fd", "#ffffff"] },
  leaf:      { name: "🍃 leaves", colors: ["#4ade80", "#65a30d", "#a3e635", "#d97706", "#15803d"] },
  bluebell:  { name: "🔔 bluebell", colors: ["#93c5fd", "#a5b4fc", "#c4b5fd", "#67e8f9", "#ffffff"] },
  sparkle:   { name: "✨ sparkle", colors: ["#c4b5fd", "#f0abfc", "#93c5fd", "#fde68a", "#99f6e4"] },
  toadstool: { name: "🍄 toadstool", colors: ["#dc2626", "#ea580c", "#9333ea", "#f59e0b", "#db2777"] },
};
// (ticket 515, Matthew: "robot costume -- multiple styles -- do 8") The robot: eight styles, each with its own colours (the
// first its usual one), the colour of its lights (💡 lights' "auto") and its traits, all of them read by its drawing
// (js/06-costumes.js): `head`, the head's drawing (robotHead: its face, a plate, screen, window or opening, and what is on
// top are the head's own); `torso` (robotTorso); `back`, what stands off the back (robotBack: "none" when nothing does, its
// back's details then the torso's); `limbs`, the arms' and legs' pieces (robotArm, robotLeg); `hands` (and how much
// bigger, handK) and `feet` (footK; "own", the box bot's: the friend's own trousers and shoes); `see`, the views the
// friend's own face is drawn in ("front", "side", "back"; none under the tin's faceplate and the helper's screen, which
// draw their own eyes and mouth following the friend's mood); `limb`, the sleeves' and legs' colour; `hand`, `boot`,
// theirs ("main" the style's colour, "darker" a shade of it, "red" the hero's red, or a colour).
const ROBOT_STYLES = {
  tin:    { name: "🔑 wind-up tin", colors: ["#d23a2e", "#2563eb", "#16a34a", "#0d9488", "#f97316", "#7c3aed"], lights: "#f6c445",
            head: "tinbox", torso: "column", back: "key", limbs: "bolts", hands: "round", feet: "boot",
            see: "", limb: "#c9ced6", hand: "#c9ced6", boot: "main" },
  box:    { name: "📦 box bot", colors: ["#c9a06b", "#f4f1ea", "#8b6a43", "#ef4444", "#3b82f6", "#22c55e"], lights: "#e53935",
            head: "cube", torso: "box", back: "none", limbs: "hose", hands: "mitten", feet: "own",
            see: "front", limb: "#b8bec6", hand: "#b8bec6", boot: "main" },
  helper: { name: "🤗 helper bot", colors: ["#4aa3df", "#f472b6", "#34d399", "#facc15", "#a78bfa", "#fb923c"], lights: "#39e0ff",
            head: "ball", torso: "egg", back: "none", limbs: "plain", hands: "round", handK: 1.2, feet: "boot",
            see: "", limb: "#f4f6f8", hand: "#f4f6f8", boot: "main" },
  space:  { name: "🚀 space bot", colors: ["#ef6c3a", "#e63946", "#2563eb", "#10b981", "#7c3aed", "#facc15"], lights: "#6ee7b7",
            head: "bubble", torso: "capsule", back: "rockets", limbs: "accordion", hands: "pincer", feet: "boot",
            see: "front side back", limb: "#dfe5ec", hand: "main", boot: "main" },
  mech:   { name: "🦾 big mech", colors: ["#f2b705", "#ea580c", "#6b7d3a", "#64748b", "#2563eb", "#dc2626", "#f8fafc"], lights: "#ff7a00",
            head: "cockpit", torso: "chest", back: "battery", limbs: "blocks", hands: "pincer", handK: 1.5, feet: "boot", footK: 1.35,
            see: "front side", limb: "#3f3f46", hand: "main", boot: "#3f3f46" },
  hero:   { name: "⚡ hero bot", colors: ["#2556c4", "#dc2626", "#f1f5f9", "#111827", "#16a34a", "#7c3aed"], lights: "#7ff3ff",
            head: "helm", torso: "plate", back: "none", limbs: "plates", hands: "mitten", feet: "boot",
            see: "front side", limb: "main", hand: "red", boot: "red" },
  tv:     { name: "📺 TV bot", colors: ["#8a5a33", "#e11d48", "#14b8a6", "#f59e0b", "#f1f5f9", "#94a3b8"], lights: "#b7f5d0",
            head: "set", torso: "panel", back: "cord", limbs: "plain", hands: "round", feet: "boot",
            see: "front", limb: "#d9d2c3", hand: "#d9d2c3", boot: "darker" },
  spider: { name: "🕷️ spider bot", colors: ["#334155", "#111827", "#7c3aed", "#0f766e", "#b91c1c", "#ca8a04"], lights: "#ef4444",
            head: "dome", torso: "segments", back: "legs", limbs: "plain", hands: "pincer", feet: "boot",
            see: "front side", limb: "main", hand: "darker", boot: "darker" },
};
const ROBOT_LIGHTS = ["auto", "#f6c445", "#39e0ff", "#6ee7b7", "#ff7a00", "#ef4444", "#f472b6", "#ffffff"];
// (ticket 536, Matthew: "moth costume, 3 styles. Follow the bird costume examples for wing ideas, but we want moth wings
// here, and antenna, etc") The 🦋 moth: an animal suit (ANIMALS.moth, js/06-costumes.js) with a moth's wings on its arms,
// as the bird suits' are (mothWing), feathery antennae and a fuzzy crown on its hood, a fluffy collar and a banded belly, in
// three styles offered as the Sasquatch's are: mothStyle (a choice row), mothColor_<id> (each style its wings' own colour
// row, its first swatch the usual one, shown only while it is chosen), and mothSpots (👁️ its eyespots), all in 🐯
// Costume's 🦋 the moth box. Each style: `colors` (its wings); `body`, the suit's own colours, put on when the style (or the
// costume) is chosen (set(), js/20-controls.js), which the animal colour rows then change as any animal's; `hind`, the
// hindwings' colour (null: the wings' own); `costa`, the band along each forewing's leading edge; `bands`, the wavy lines
// across the wings ([colour, width], "zig" lines zigzag); `hindBand`, one along each hindwing's edge; `spot`, an eyespot's rim, ring, middle and how big (on the
// hindwings; `foreSpot` how big on the forewings, 0 none); `tail`, the hindwings' long tails; `antenna`, their colour;
// `dust`, a dusting of darker scales; `kidney`, the small kidney-shaped ring a night moth has on each forewing.
const MOTH_STYLES = {
  luna:    { name: "🌙 luna moth", colors: ["#b8e6a0", "#9fdcc8", "#e8f09a", "#f5c4da", "#c6d4f5"], hind: null, costa: "#8a3b5c",
             body: { fur: "#f4f1e6", marks: "#c9b98f", belly: "#ffffff", accent: "#e7b8cb", furPattern: "none" },
             bands: [["#e9f5c8", 3]], hindBand: null, spot: ["#7a3450", "#f2c94c", "#dff3ff", 9], foreSpot: 6, tail: true, antenna: "#d4ae4f" },
  emperor: { name: "🦚 emperor moth", colors: ["#c9773d", "#b0563a", "#9a6e4c", "#d9a05b", "#a77bb8", "#d9708e"], hind: "#e3913f", costa: "#efe2cf",
             body: { fur: "#8d5d3b", marks: "#4b2d1d", belly: "#e2c29a", accent: "#f2d6a2", furPattern: "none" },
             bands: [["#f6ead6", 4], ["#4a2a1a", 2.5]], hindBand: ["#f6ead6", 3.5], spot: ["#24160e", "#f4bd45", "#2c3a86", 15], foreSpot: 13, tail: false, antenna: "#5b3a24" },
  night:   { name: "🌫️ night moth", colors: ["#9b9081", "#7f766b", "#b3a993", "#8c7b6a", "#a4a9b2"], hind: "#d9905f", costa: "#5d544a",
             body: { fur: "#8f8578", marks: "#4f473f", belly: "#cfc5b4", accent: "#6d6154", furPattern: "none" },
             bands: [["#3f3832", 3, "zig"], ["#3f3832", 3, "zig"]], hindBand: ["#2e2a26", 5], spot: ["#2e2a26", "#2e2a26", "#2e2a26", 0], foreSpot: 0, tail: false,
             antenna: "#6c6156", dust: "#4a4239", kidney: true },
};
// (ticket 518, Matthew: "body can hold things in each hand. Audit all MUDU objects to hold (staff, broom, sword, shield, horse
// rein) and add every option to body sub-panel, somehow.") HOLDABLES: everything a hand can hold, by id, in drawing order
// (the order the costumes' own were drawn in). `name` its chip; `ext` how far it reaches from the hand, for the picture's
// frame (up, down, out to the side, 0 for no room; the broom's times its size); `fist`, the hand closed round it. Their drawings are
// heldSvg's (js/08-body.js): toward us, in profile and from behind, each view its own drawing. body.holdR and body.holdL
// (the friend's OWN right and left hands) are "auto" (the costume's own, as before: heldAuto), "none", or an id here.
// The rein draws nothing here: it decides which hand the horse's rein goes to (js/18-drawing.js), shown only with a horse.
const HOLDABLES = {
  broom:   { name: "🧹 broom", ext: [140, 140, 60], fist: true },
  bow:     { name: "🏹 bow", ext: [90, 90, 50], fist: true },
  scepter: { name: "👑 scepter", ext: [170, 0, 0], fist: true },
  sword:   { name: "🗡️ sword", ext: [145, 0, 24], fist: true },
  shield:  { name: "🛡️ shield", ext: [0, 45, 40], fist: true },
  staff:   { name: "🪄 staff", ext: [190, 120, 40], fist: true },
  lasso:   { name: "➿ lasso", ext: [0, 64, 44], fist: true },
  rein:    { name: "🐴 the horse's rein", ext: [0, 0, 0], fist: true },
};
const HOLD_CHOICES = [["auto", "🎭 the costume's own"], ["none", "✋ nothing"], ...Object.entries(HOLDABLES).map(([id, h]) => [id, h.name])];
// What a costume puts in each own hand by itself ("auto"): the broom, staff, scepter, sword and lasso in the left (side +1
// facing us), the bow and the shield in the right, each while its own costume setting has it on
function heldAuto(kind, WC) {
  const L = kind === "witch" ? (WC.broom ? "broom" : null) : kind === "wizard" || kind === "monk" ? (WC.staff !== false ? "staff" : null)
    : kind === "queen" || kind === "king" ? (WC.scepter !== false ? "scepter" : null) : kind === "knight" ? (WC.sword !== false ? "sword" : null)
    : kind === "cowgirl" || kind === "cowboy" ? (WC.lasso !== false ? "lasso" : null) : null;
  const R = kind === "robin" ? (WC.bow !== false ? "bow" : null) : kind === "knight" ? (WC.shield !== false ? "shield" : null) : null;
  return { L, R };
}
// What each own hand holds: an explicit choice wins, and takes its item out of the other hand's "auto" (a lasso moved to the
// right hand leaves the left); the rein only with a horse
function heldItems(body, kind, WC) {
  const auto = heldAuto(kind, WC), v = own => body["hold" + own] || "auto";
  const pick = (own, other) => { const x = v(own);
    if (x === "auto") { const a = auto[own]; return a && v(other) !== a ? a : null; }
    return HOLDABLES[x] && (x !== "rein" || (body.horse && body.horse !== "none")) ? x : null; };
  return { L: pick("L", "R"), R: pick("R", "L") };
}
// (ticket 522, M and Matthew: "Cowgirl and Cowboy, 3 variations of style each. 6 styles of hat. Lasso to hold. No guns
// please.") The cowgirl's and the cowboy's styles, each with its own colours (the first its usual one, its colour row's) and
// the traits the drawing reads (js/06-costumes.js, westernOf): `shirt` and `pants` (what replaces the friend's own, "main"
// the style's chosen colour); `hat`, its usual hat (choosing the style puts it on until a kid picks a hat themselves,
// cowHatAuto, set()); over the shirt `chestFringe` (the rodeo star's U of fringe), `vest`, `badge` (a toy star), `tie`,
// `yoke`, `number` (the card on the back), `collarBow`, `bandana` ("auto": red with white dots, navy on a reddish shirt),
// `duster` (the trail boss's long coat: its shirt panel, capelet and skirts) and `belt`, `buckle`, `bigBuckle`; on the arms
// `sleeveFringe`; on the legs `chaps` (`chapLine`, `spots`); at the hem `hemFringe`, `ruffle` (a petticoat's frill); on the
// feet `boot` (SHOES.cowboot), `bootStar` and `spur` (a toy star disc, nothing sharp). No guns, holsters or knives.
const COWGIRL_STYLES = {
  rodeo:     { name: "🌟 rodeo star", colors: ["#ec4899", "#14b8a6", "#8b5cf6", "#f97316", "#ef4444", "#3b82f6"], hat: "bow",
               shirt: { style: "long", collar: "v", color: "main", pattern: "plain" }, pants: { style: "skirt", skirtLen: 42, color: "#f4f1ea", pattern: "plain" },
               fringe: "#f4f1ea", fringeUnder: "#b8b2a6", chestFringe: true, sleeveFringe: true, hemFringe: true,
               belt: "#f4f1ea", buckle: "#e0b030", boot: "#f4f1ea", bootStar: "main", spur: false },
  prairie:   { name: "🌻 prairie", colors: ["#dc2626", "#2563eb", "#16a34a", "#9333ea", "#ea580c", "#db2777"], hat: "straw",
               shirt: { style: "long", collar: "round", color: "main", pattern: "gingham", patternSize: 16, patternColor: "#ffffff" },
               pants: { style: "dress", skirtLen: 86, color: "main", pattern: "gingham", patternSize: 16, patternColor: "#ffffff" },
               ruffle: "#ffffff", collarBow: "#ffffff", boot: "#6b4226", spur: false },
  cowprint:  { name: "🐮 cow-print", colors: ["#facc15", "#ef4444", "#a855f7", "#14b8a6", "#f4f1ea", "#fb923c"], hat: "cattleman",
               shirt: { style: "long", collar: "v", color: "main", pattern: "plain" }, pants: { style: "long", color: "#3b5b8c", pattern: "plain" },
               chaps: "#f7f3ea", chapLine: "#c9c2b4", spots: "#1f1a17", bandana: "auto", belt: "#6b4226", buckle: "#d1d5db", boot: "#8b5a2b", spur: true },
};
const COWBOY_STYLES = {
  sheriff:   { name: "⭐ sheriff", colors: ["#6b4226", "#1f1f22", "#c8a27a", "#7a2e2e", "#475569", "#1e3a8a"], hat: "star",
               shirt: { style: "long", collar: "flat", color: "#f4f1ea", pattern: "plain" }, pants: { style: "long", color: "#3b5b8c", pattern: "plain" },
               vest: "main", badge: "#e0b030", tie: "#1f1f22", belt: "#1f1f22", buckle: "#d1d5db", boot: "#1f1f22", spur: true },
  rider:     { name: "🏅 rodeo rider", colors: ["#dc2626", "#2563eb", "#7c3aed", "#0d9488", "#1f1f22", "#f97316"], hat: "tengallon",
               shirt: { style: "long", collar: "flat", color: "main", pattern: "plain" }, pants: { style: "long", color: "#3b5b8c", pattern: "plain" },
               yoke: "#f4f1ea", number: true, belt: "#6b4226", buckle: "#e0b030", bigBuckle: true, boot: "#c8a27a", spur: true },
  trailboss: { name: "🧥 trail boss", colors: ["#b08a5a", "#3f3f46", "#5b3a1e", "#1f2937", "#6b7d3a", "#e8dcc4"], hat: "feather",
               shirt: { style: "long", collar: "flat", color: "main", pattern: "plain" }, pants: { style: "long", color: "#4b5563", pattern: "plain" },
               duster: true, panel: "#e9e1cf", bandana: "#7f1d1d", boot: "#1f1f22", spur: true },
};
// (ticket 530, Matthew: "Sasquatch costume") The 🦶 Sasquatch: an animal suit (ANIMALS.sasquatch, js/06-costumes.js) in four
// styles, offered as the robot's are: sasquatchStyle (a choice row), sasquatchColor_<id> (each style its own colour row, its
// first swatch the usual one, shown only while it is chosen), all in 🐯 Costume's 🦶 the Sasquatch box. Each style: `colors`,
// `face` (the muzzle, belly and pads), `shag` (how long its tufts are), `footK` (its feet's size), and its extras, every one
// read by the drawing (sasquatchLook, shagEdge, sasquatchTop): `frost` (blue tuft tips), `moss` and `leaves` (stuck in the
// fur), `bow` (on its head).
const SASQUATCH_STYLES = {
  bigfoot: { name: "🦶 Bigfoot", colors: ["#7a4e2d", "#5b3a1e", "#a0703f", "#3f2a1a", "#8b6a43"], face: "#d9b38c", shag: 1, footK: 1.45 },
  yeti:    { name: "❄️ yeti", colors: ["#f4f1ea", "#dbeafe", "#e5e7eb", "#c7d2fe"], face: "#dbeafe", marks: "#bfdbfe", shag: 1.1, footK: 1.3, frost: "#60a5fa" },
  mossy:   { name: "🌿 mossy", colors: ["#6b5a2e", "#4d5b2a", "#7c6a3a", "#3f4a2a"], face: null, shag: 1, footK: 1.3, moss: "#65a30d", leaves: "#84cc16" },
  fluffy:  { name: "🍭 fluffy", colors: ["#f9a8d4", "#c4b5fd", "#fdba74", "#99f6e4", "#fde68a"], face: null, shag: 1.3, footK: 1.2, bow: "#ec4899" },
};
// (ticket 544, Matthew, for his friend Nick: "giraffe as a costume in dress up") The 🦒 giraffe: an animal suit
// (ANIMALS.giraffe, js/06-costumes.js) in a giraffe's patched coat, its long neck rising from the hood over the friend's
// head with the giraffe's own head on top, in three styles offered as the Sasquatch's are: giraffeStyle (a choice row),
// giraffeColor_<id> (each style its patches' own colour row, its first swatch the usual one, shown only while it is
// chosen), and giraffeNeck (how long the neck is, in percent), all in 🐯 Costume's 🦒 the giraffe box. Each style:
// `colors` (its patches), `ground` (the cream between them), `coat` (GIRAFFE_COATS, js/06-costumes.js: "net", the
// reticulated giraffe's patches, or "masai", jagged and leaf-like), `neck` (its neck's length, times giraffeNeck), `mane`
// (its mane and its tail's tuft), `muzzle`, and `baby` (big eyes, fluffy tufts on its horns), every one read by the drawing
// (giraffeLook, giraffeNeckSvg).
const GIRAFFE_STYLES = {
  reticulated: { name: "🦒 reticulated", colors: ["#b5652b", "#8c4a1e", "#cf8a3c", "#5c3317", "#7c4dbe"], ground: "#f6e7c8", coat: "net", neck: 1, mane: "#6e3514", muzzle: "#efdcb6" },
  masai:       { name: "🌳 Masai", colors: ["#5a3418", "#3f2412", "#7a4a26", "#4a3a2a"], ground: "#ecd9b4", coat: "masai", neck: 1.1, mane: "#2e1b0e", muzzle: "#e6d2aa" },
  baby:        { name: "🍼 baby", colors: ["#d68f52", "#e3a46a", "#c7834f", "#f0a6b9", "#9cc3e6"], ground: "#fbf1dc", coat: "net", neck: 0.7, mane: "#a8653a", muzzle: "#f8ead0", baby: true },
};
// The hats, one setting for both (cowHat, or "none"), each with its own colours. In the round head's units (x 100 the
// middle): the brim's line at y COW_HAT_Y, the hair tucked above COW_HAT_LINE (under a flat brim too). `crown` "pinch" (a
// crease and two front pinches), "tall" (taller and rounder) or "flat"; `top` the crown's top; `bw` the brim's half width;
// `curl` how far its sides (in profile, its back) turn up; `band` ("darker": the hat's colour toward black; "bow": white,
// pink on a light hat); and its ornaments: conchos, the straw's weave and ribbon tails, a bow, a star, a feather.
const COW_HAT_Y = 58, COW_HAT_LINE = 60;
const COW_HATS = {
  cattleman: { name: "🐂 cattleman", colors: ["#8b5a2b", "#c8a27a", "#1f1f22", "#f4f1ea", "#6b7280", "#7a2e2e"], crown: "pinch", top: -16, bw: 128, curl: 16, band: "darker" },
  tengallon: { name: "🪣 ten-gallon", colors: ["#f4f1ea", "#c8a27a", "#8b5a2b", "#1f1f22", "#9ca3af", "#e9d5a8"], crown: "tall", top: -70, bw: 146, curl: 22, band: "#3b2414", conchos: true },
  straw:     { name: "🌾 straw", colors: ["#e9c96b", "#f3e3a6", "#d4a84a", "#c9b38a", "#f4f1ea", "#b45309"], crown: "pinch", top: 0, bw: 158, curl: 4, band: "#c0392b", weave: true, tails: true },
  bow:       { name: "🎀 bow", colors: ["#f472b6", "#f4f1ea", "#a78bfa", "#38bdf8", "#facc15", "#ef4444"], crown: "pinch", top: -10, bw: 122, curl: 18, band: "bow", bow: true },
  star:      { name: "🌠 star hat", colors: ["#1f1f22", "#8b5a2b", "#f4f1ea", "#c8a27a", "#7a2e2e", "#475569"], crown: "pinch", top: -24, bw: 132, curl: 14, band: "#c9ced6", star: "#e0b030" },
  feather:   { name: "🪶 feather", colors: ["#4b5563", "#1f1f22", "#7c3a2e", "#2f4f6f", "#c8a27a", "#3f6212"], crown: "flat", top: -6, bw: 134, curl: 3, band: "#1f1f22", feather: "#d2382b" },
};
// the lasso's rope (the monk's rope colour) over a darker one
const ROPE = "#c8a46a", ROPE_DK = "#8a6a3a";
// (ticket 557, for M: "ninja costume -- all black shirt and pants. black gloves. black face mask (we don't see the face at
// all ...) special ninja boots/shoes (very quiet), with a braided pattern of bands ... golden head band ... with a separate,
// connected golden band going from 3rd eye/ forehead to the back of the head ... a ruby (or other gem), with various shape
// options mounted in the inverted T of golden bands") The 🥷 ninja, in the friend's own clothes as the cowboy is: the cloth,
// near black with a hint of blue so its outlines still show (NINJA.cloth), the gloves a shade darker (NINJA.glove), the eye
// slit black; the boots' bands (NINJA_BOOTS, ninjaBoots: braided, two tones crossing over and under, or wrapped straight
// round, band above band); and the head covering (ninjaBand, on by default): gold bands round the head and over it, front
// to back, and a gem where they meet on the forehead, its kind (NINJA_GEMS: its colour, its light facet, its outline) and
// its shape (NINJA_GEM_SHAPES, drawn by ninjaGemPath in js/06-costumes.js). The rows are in 🐯 Costume's 🥷 the ninja box.
const NINJA = { cloth: "#24252d", line: "#0c0c10", glove: "#18181e", slit: "#030304", gold: "#e0b030", goldLine: "#8a6420", goldLight: "#f6dc7a",
                bandA: "#4d5263", bandB: "#71778b", bandLine: "#111216" };
const NINJA_BOOTS = [["braided", "🪢 braided bands"], ["wrapped", "🧣 bands wrapped round"]];
// (ticket 561, M's design) what shows between the bands: the friend's own skin (the default), or 🌑 full darkness (the
// trousers under them, as 0.0.410 drew it; Matthew: "which I think looks more natural")
const NINJA_BETWEEN = [["skin", "🦵 skin between the bands"], ["dark", "🌑 full darkness"]];
const NINJA_GEMS = {
  ruby:     { name: "🔴 ruby", c: "#d0103a", light: "#ff7a96", line: "#6e0520" },
  emerald:  { name: "🟢 emerald", c: "#119a4c", light: "#7aeaa8", line: "#05502a" },
  sapphire: { name: "🔵 sapphire", c: "#1d4ed8", light: "#86a8ff", line: "#0e266e" },
  amethyst: { name: "🟣 amethyst", c: "#8b3fc8", light: "#d6aaff", line: "#43186e" },
  topaz:    { name: "🟡 topaz", c: "#f0a020", light: "#ffe394", line: "#8a5206" },
  diamond:  { name: "💎 diamond", c: "#d9f1ff", light: "#ffffff", line: "#6f93aa" },
};
const NINJA_GEM_SHAPES = [["round", "⚪ round"], ["oval", "🥚 oval"], ["rhombus", "♦️ diamond shape"], ["teardrop", "💧 teardrop"], ["star", "⭐ star"], ["heart", "❤️ heart"]];
const FAIRY_SHOES = [["slipper", "🩰 curly slippers"],["petal", "🌸 petal shoes"], ["leaf", "🍃 leaf wraps"], ["sparkle", "✨ sparkle boots"], ["bare", "🌼 bare feet, flower anklets"]];
const FAIRY_SHOE_COLORS = ["#f472b6", "#a78bfa", "#4ade80", "#fde047", "#60a5fa", "#ffffff"];

// (ticket 185) The world: places laid on a grid, each joined to its
// neighbours by the ways out on their shared sides (see 25-world.js). A
// part of DEFAULTS, so that Load and the autosave keep it. Its first world
// is today's, laid out (Matthew, 2026-10-17): the pond in the middle (C3),
// the land across it a forest crossing (C2) to visit, not only to see; the
// beach on the top edge (C1), looking out on water, the witch's woods and
// the mountains either side; the clearing east of the pond, the wood path
// and the fairy forest north of it, the castle east, the house and the
// tree house south-east, the camp and the waterfall south-west; forest
// crossings between; the friend starting at the clearing; the insides off
// the grid.
const WORLD_DEFAULT_0183 = {"cols": 5, "rows": 6, "start": "3,3", "cells": {"0,0": {"kind": "sea", "ways": ["E"], "things": []}, "1,0": {"kind": "sea", "ways": ["E", "W"], "things": []}, "2,0": {"kind": "sea", "ways": ["E", "S", "W"], "things": []}, "3,0": {"kind": "sea", "ways": ["E", "W"], "things": []}, "4,0": {"kind": "sea", "ways": ["W"], "things": []}, "1,1": {"kind": "witch", "ways": ["E", "S"], "things": []}, "2,1": {"kind": "beach", "ways": ["N", "E", "S", "W"], "things": []}, "3,1": {"kind": "mountains", "ways": ["S", "W"], "things": []}, "0,2": {"kind": "cemetery", "ways": ["E", "S"], "things": []}, "1,2": {"kind": "forest", "ways": ["N", "E", "S", "W"], "things": []}, "2,2": {"kind": "crossing", "crossN": true, "crossNE": false, "crossE": true, "crossSE": false, "crossS": true, "crossSW": false, "crossW": true, "crossNW": false, "crossFloor": "#76843a", "crossBack": "sky", "things": []}, "3,2": {"kind": "woodpath", "ways": ["N", "S", "W"], "things": []}, "4,2": {"kind": "fairyforest", "ways": ["SW"], "things": []}, "0,3": {"kind": "crossing", "crossN": true, "crossNE": false, "crossE": true, "crossSE": false, "crossS": false, "crossSW": false, "crossW": false, "crossNW": false, "crossFloor": "#76843a", "crossBack": "woods", "things": []}, "1,3": {"kind": "crossing", "crossN": true, "crossNE": false, "crossE": true, "crossSE": false, "crossS": true, "crossSW": false, "crossW": true, "crossNW": false, "crossFloor": "#76843a", "crossBack": "mountains", "things": []}, "2,3": {"kind": "lake", "ways": ["N", "E", "S", "W"], "things": []}, "3,3": {"kind": "clearing", "ways": ["N", "NE", "E", "S", "W"], "things": []}, "4,3": {"kind": "castle", "ways": ["S", "W"], "things": []}, "1,4": {"kind": "campsite", "ways": ["N", "E"], "things": []}, "2,4": {"kind": "crossing", "crossN": true, "crossNE": false, "crossE": true, "crossSE": false, "crossS": false, "crossSW": true, "crossW": true, "crossNW": false, "crossFloor": "#76843a", "crossBack": "sky", "things": []}, "3,4": {"kind": "house", "ways": ["N", "E", "W"], "things": []}, "4,4": {"kind": "treehouse", "ways": ["N", "W"], "things": []}, "1,5": {"kind": "waterfall", "ways": ["NE"], "things": []}}};
// (ticket 260, Matthew: "Let's use this world design grid as the new default in future releases") the world from his
// save of 2026-10-17 20:09, its world alone: 6 by 7, the clearing still the start (3,3), deep sea down the west side
// and open water on top, the things he put in its squares; the one above (to 0.0.183) is upgraded to it, untouched
// (0.0.190, Matthew: "please disconnect grid squares A4 and B5 --- that was a mistake that I fixed, and I guess I forgot to
// resave": A4's way SE and B5's way NW gone)
const WORLD_DEFAULT = {"cols": 6, "rows": 7, "start": "3,3", "cells": {"0,0": {"kind": "deepsea", "ways": ["E", "S"], "things": [{"kind": "shell", "x": -282, "z": 325, "seed": 1334, "size": 100, "mirror": false, "shape": "clam", "color": "#ffffff"}, {"kind": "shell", "x": 308, "z": 327, "seed": 7663, "size": 100, "mirror": false, "shape": "conch", "color": "#fdba74"}, {"kind": "shell", "x": 125, "z": 388, "seed": 2019, "size": 100, "mirror": false, "shape": "scallop", "color": "#f8e1c7"}, {"kind": "shell", "x": -203, "z": 347, "seed": 2932, "size": 100, "mirror": false, "shape": "scallop", "color": "#f8e1c7"}, {"kind": "shell", "x": -10, "z": 376, "seed": 2653, "size": 100, "mirror": false, "shape": "scallop", "color": "#f8e1c7"}, {"kind": "shell", "x": 182, "z": 335, "seed": 9180, "size": 100, "mirror": false, "shape": "scallop", "color": "#f8e1c7"}, {"kind": "shell", "x": -146, "z": 364, "seed": 3121, "size": 100, "mirror": false, "shape": "scallop", "color": "#f8e1c7"}, {"kind": "shell", "x": -51, "z": 400, "seed": 2749, "size": 100, "mirror": false, "shape": "scallop", "color": "#f8e1c7"}, {"kind": "shell", "x": 239, "z": 352, "seed": 8125, "size": 100, "mirror": false, "shape": "scallop", "color": "#f8e1c7"}]}, "1,0": {"kind": "sea", "ways": ["E", "SE", "SW", "W"], "things": []}, "2,0": {"kind": "sea", "ways": ["E", "S", "W"], "things": []}, "3,0": {"kind": "sea", "ways": ["E", "SW", "W"], "things": []}, "4,0": {"kind": "sea", "ways": ["W"], "things": []}, "1,1": {"kind": "witch", "ways": ["E", "SE", "S"], "things": []}, "2,1": {"kind": "beach", "ways": ["N", "NE", "E", "SE", "S", "SW", "W", "NW"], "things": []}, "3,1": {"kind": "mountains", "ways": ["E", "SE", "S", "SW", "W"], "things": []}, "0,2": {"kind": "deepsea", "ways": ["N", "S"], "things": []}, "1,2": {"kind": "forest", "ways": ["N", "NE", "E", "SE", "S"], "things": []}, "2,2": {"kind": "crossing", "crossN": true, "crossNE": true, "crossE": true, "crossSE": true, "crossS": true, "crossSW": true, "crossW": true, "crossNW": true, "crossFloor": "#76843a", "crossBack": "sky", "things": []}, "3,2": {"kind": "woodpath", "ways": ["N", "NE", "E", "S", "W", "NW"], "things": []}, "4,2": {"kind": "fairyforest", "ways": ["N", "SW", "W", "NW"], "things": []}, "0,3": {"kind": "deepsea", "ways": ["N"], "things": [{"kind": "chest", "x": -260, "z": 330, "seed": 71399, "size": 100, "mirror": false, "open": false}, {"kind": "chest", "x": 309, "z": 392, "seed": 20207, "size": 145, "mirror": true, "open": false}, {"kind": "chest", "x": 192, "z": 372, "seed": 7656, "size": 100, "mirror": false, "open": false}, {"kind": "chest", "x": -286, "z": 402, "seed": 8337, "size": 100, "mirror": false, "open": false}, {"kind": "kelp", "x": -10, "z": 376, "seed": 8207, "size": 100, "mirror": false, "color": "#2f8f4e"}, {"kind": "kelp", "x": 182, "z": 335, "seed": 8954, "size": 100, "mirror": false, "color": "#2f8f4e"}, {"kind": "kelp", "x": -146, "z": 364, "seed": 6851, "size": 100, "mirror": false, "color": "#2f8f4e"}, {"kind": "kelp", "x": 47, "z": 393, "seed": 2290, "size": 100, "mirror": false, "color": "#2f8f4e"}, {"kind": "kelp", "x": 239, "z": 352, "seed": 8251, "size": 100, "mirror": false, "color": "#2f8f4e"}, {"kind": "kelp", "x": -338, "z": 406, "seed": 7807, "size": 100, "mirror": false, "color": "#2f8f4e"}, {"kind": "starfish", "x": 104, "z": 340, "seed": 6691, "size": 100, "mirror": false, "color": "#fb923c"}, {"kind": "seafish", "x": -224, "z": 369, "seed": 6711, "size": 100, "mirror": false, "color": "#ff9f1c", "bubbles": true}]}, "1,3": {"kind": "crossing", "crossN": true, "crossNE": true, "crossE": true, "crossSE": true, "crossS": true, "crossSW": true, "crossW": false, "crossNW": true, "crossFloor": "#76843a", "crossBack": "mountains", "things": []}, "2,3": {"kind": "lake", "ways": ["N", "E", "SE", "S", "SW", "W", "NW"], "things": [{"kind": "pine", "x": -260, "z": 330, "seed": 7989, "size": 100, "mirror": false, "tiers": 6}, {"kind": "pine", "x": 213, "z": 329, "seed": 6198, "size": 205, "mirror": false, "tiers": 6}]}, "3,3": {"kind": "clearing", "ways": ["N", "NE", "E", "SE", "S", "SW", "W", "NW"], "things": []}, "4,3": {"kind": "castle", "ways": ["S", "SW", "W"], "things": []}, "1,4": {"kind": "campsite", "ways": ["N", "NE", "E", "SE", "S", "SW", "W"], "things": []}, "2,4": {"kind": "crossing", "crossN": true, "crossNE": true, "crossE": true, "crossSE": true, "crossS": true, "crossSW": true, "crossW": true, "crossNW": true, "crossFloor": "#76843a", "crossBack": "sky", "things": []}, "3,4": {"kind": "house", "ways": ["N", "NE", "E", "SE", "S", "SW", "W", "NW"], "things": []}, "4,4": {"kind": "treehouse", "ways": ["N", "E", "SW", "W", "NW"], "things": []}, "1,5": {"kind": "crossing", "crossN": true, "crossNE": true, "crossE": true, "crossSE": false, "crossS": true, "crossSW": false, "crossW": true, "crossNW": true, "crossFloor": "#76843a", "crossBack": "woods", "things": [{"kind": "rock", "x": -260, "z": 330, "seed": 7542, "size": 100, "mirror": false, "tone": "#8f8a80"}, {"kind": "rock", "x": -68, "z": 359, "seed": 8222, "size": 100, "mirror": false, "tone": "#b4afa4"}, {"kind": "rock", "x": 125, "z": 388, "seed": 4640, "size": 100, "mirror": false, "tone": "#6f8a94"}, {"kind": "rock", "x": -203, "z": 347, "seed": 7742, "size": 100, "mirror": false, "tone": "#c2a67a"}, {"kind": "tree", "x": -130, "z": 369, "seed": 6975, "size": 100, "mirror": false, "h": 380, "leaves": "summer"}, {"kind": "tree", "x": 222, "z": 380, "seed": 5384, "size": 100, "mirror": false, "h": 380, "leaves": "summer"}]}, "4,1": {"kind": "treehouse", "ways": ["S", "SW", "W"], "things": []}, "0,1": {"kind": "deepsea", "ways": ["N", "NE", "S"], "things": [{"kind": "seagrass", "x": -260, "z": 330, "seed": 8167, "size": 100, "mirror": false, "color": "#4caf50"}, {"kind": "seafan", "x": 245, "z": 340, "seed": 3582, "size": 100, "mirror": false, "color": "#c084fc"}, {"kind": "sealettuce", "x": 138, "z": 368, "seed": 5171, "size": 100, "mirror": false, "color": "#8bc34a"}, {"kind": "coral", "x": -235, "z": 370, "seed": 38069, "size": 100, "mirror": true, "color": "#fde047"}]}, "0,4": {"kind": "waterfall", "ways": ["NE", "E", "SE", "S"], "things": [{"kind": "rock", "x": -312, "z": 254, "seed": 8049, "size": 300, "mirror": false, "tone": "#8f8a80"}, {"kind": "rock", "x": -303, "z": 246, "seed": 3981, "size": 295, "mirror": false, "tone": "#b4afa4"}, {"kind": "rock", "x": 125, "z": 388, "seed": 1374, "size": 100, "mirror": false, "tone": "#6f8a94"}, {"kind": "rock", "x": -329, "z": 275, "seed": 5615, "size": 100, "mirror": false, "tone": "#c2a67a"}, {"kind": "rock", "x": -10, "z": 376, "seed": 9571, "size": 100, "mirror": false, "tone": "#a8634a"}, {"kind": "rock", "x": 182, "z": 335, "seed": 4613, "size": 100, "mirror": false, "tone": "#7f8a6a"}, {"kind": "tree", "x": 332, "z": 268, "seed": 8644, "size": 300, "mirror": false, "h": 380, "leaves": "summer"}, {"kind": "pine", "x": -302, "z": 406, "seed": 8567, "size": 100, "mirror": false, "tiers": 6}, {"kind": "pine", "x": -384, "z": 246, "seed": 9890, "size": 100, "mirror": false, "tiers": 6}, {"kind": "pine", "x": -292, "z": 279, "seed": 6334, "size": 100, "mirror": false, "tiers": 6}, {"kind": "pine", "x": -360, "z": 342, "seed": 3961, "size": 100, "mirror": false, "tiers": 6}]}, "0,5": {"kind": "forest", "ways": ["N", "NE", "E"], "things": [{"kind": "rock", "x": -260, "z": 330, "seed": 3479, "size": 100, "mirror": false, "tone": "#8f8a80"}, {"kind": "rock", "x": -307, "z": 319, "seed": 4293, "size": 200, "mirror": false, "tone": "#b4afa4"}, {"kind": "rock", "x": 125, "z": 388, "seed": 2572, "size": 100, "mirror": false, "tone": "#6f8a94"}, {"kind": "rock", "x": -284, "z": 358, "seed": 1255, "size": 100, "mirror": false, "tone": "#c2a67a"}, {"kind": "rock", "x": -204, "z": 310, "seed": 7266, "size": 100, "mirror": false, "tone": "#a8634a"}, {"kind": "rock", "x": -164, "z": 331, "seed": 1209, "size": 100, "mirror": false, "tone": "#7f8a6a"}, {"kind": "rock", "x": -146, "z": 364, "seed": 5332, "size": 100, "mirror": false, "tone": "#4f4b48"}, {"kind": "rock", "x": -230, "z": 386, "seed": 2212, "size": 100, "mirror": false, "tone": "#8f8a80"}, {"kind": "rock", "x": -88, "z": 385, "seed": 1993, "size": 100, "mirror": false, "tone": "#b4afa4"}]}, "2,5": {"kind": "cemetery", "ways": ["N", "NE", "E", "W", "NW"], "things": []}, "3,5": {"kind": "lake", "ways": ["N", "NE", "E", "W", "NW"], "things": []}, "4,5": {"kind": "castle", "ways": ["W", "NW"], "things": []}, "5,4": {"kind": "forest", "ways": ["W"], "things": [{"kind": "rock", "x": -106, "z": 293, "seed": 9349, "size": 100, "mirror": false, "tone": "#8f8a80"}, {"kind": "rock", "x": 93, "z": 293, "seed": 3760, "size": 100, "mirror": false, "tone": "#b4afa4"}, {"kind": "rock", "x": 357, "z": 406, "seed": 4428, "size": 100, "mirror": false, "tone": "#6f8a94"}, {"kind": "rock", "x": -259, "z": 387, "seed": 3435, "size": 100, "mirror": false, "tone": "#c2a67a"}, {"kind": "rock", "x": -3, "z": 406, "seed": 1717, "size": 100, "mirror": false, "tone": "#a8634a"}, {"kind": "rock", "x": 381, "z": 379, "seed": 1648, "size": 100, "mirror": false, "tone": "#7f8a6a"}, {"kind": "rock", "x": -223, "z": 406, "seed": 2525, "size": 100, "mirror": false, "tone": "#4f4b48"}, {"kind": "rock", "x": 233, "z": 406, "seed": 4636, "size": 100, "mirror": false, "tone": "#8f8a80"}, {"kind": "rock", "x": 364, "z": 349, "seed": 5501, "size": 100, "mirror": false, "tone": "#b4afa4"}, {"kind": "rock", "x": 87, "z": 406, "seed": 8667, "size": 100, "mirror": false, "tone": "#6f8a94"}, {"kind": "rock", "x": 297, "z": 314, "seed": 9413, "size": 100, "mirror": false, "tone": "#c2a67a"}, {"kind": "rock", "x": -348, "z": 406, "seed": 6844, "size": 100, "mirror": false, "tone": "#a8634a"}, {"kind": "rock", "x": -142, "z": 398, "seed": 5444, "size": 100, "mirror": false, "tone": "#7f8a6a"}, {"kind": "rock", "x": 375, "z": 321, "seed": 1488, "size": 100, "mirror": false, "tone": "#4f4b48"}, {"kind": "rock", "x": -83, "z": 406, "seed": 3132, "size": 100, "mirror": false, "tone": "#8f8a80"}]}}};
const DEFAULTS = {
  world: WORLD_DEFAULT,
  // (ticket 406) the other worlds, by id (the one the friend is in is `world`), and the worlds kept as templates, by id
  worldsAway: {}, worldTemplates: {},
  // (ticket 439) the person being dressed (their look is the parts below), and the other people, by id
  person: { id: "p1", name: "", template: false, from: null }, peopleAway: {},
  // (ticket 482) 📜 Scripting: the rules on or off, and each module's text (null: the shipped one, scriptsState)
  scripts: { on: false, common: null, rooms: {} },
  face:  { color: "#cfa000", size: 46, smile: 85, brow: 30, sick: 0, burst: 0, candy: 0,
           stars: false, hearts: false, xeyes: false, closed: false, unibrow: false, eyes: 50, sides: 0,
           rays: 0, teeth: 5, open: 0, teethStyle: "normal",
           tongue: "in", tongueLen: 100, tongueW: 100, tongueColor: "#e75480",
           lightX: -0.6, lightY: -0.6, shine: "#ffffff", shineMix: 100, shineSize: 45,
           pupil: "#000000", iris: 0, irisColor: "#3b82f6", whites: 0, oval: 0, tilt: 0,
           faceW: 100, faceH: 100, alien: 0, eyeGap: 38, noseView: "none", nose: "L", noseFront: "round", noseW: 100, noseH: 100,
           witchDir: "right", witchLen: 100, mouthSize: 100,
           ears: "free", earSize: 100 },   // (ticket 125, M's: ears, their shape and size)
  hair:  { style: "long", color: "#2b2b2b", length: 100, bangs: 100, beard: "none", beardShade: "auto", moustache: true, beardLength: 100, braidWidth: 100 },
  body:  { horse: "none", horseCoat: "#7b4a2a", horseMane: "#1f1f22", horseMark: "star", horseTack: "#7a3a1a", horseSocks: false, horseSize: 100,   // (ticket 492: the horse, in 🧍 Body)
           horseShoes: "off", horseMarkColor: "#f7f4ee", horseSaddle: "#6b3f1f", horseBlanket: "auto",   // (ticket 495, for M: more colours)
           horseLead: "left",   // (ticket 496: which side of it they lead it from)
           horseManeStyle: "loose", horseBows: "#ec4899",   // (ticket 497, for M: how its mane is done, and its bows' colour)
           horseArmour: "none",   // (ticket 498: its armour, none to start with)
           blowFrom: "mouth", blowA: true, blowB: false, blowC: false,   // (ticket 221: blowing bubbles)
           fireStyle: "roar", fireFrom: "mouth", fireSize: 100, fireHeat: true,   // (ticket 499: breathing fire, its fire, where from, how big, its heat shown)
           lieHead: "left", lieHow: "back", stairsWay: "up",   // (ticket 308: up or down the stairs)
           holdR: "auto", holdL: "auto",   // (ticket 518: what each own hand holds, HOLDABLES)
           hand: "right",   // (ticket 316: the dominant hand, the one that pulls a lever: right, left, or both, by a coin flip)   // (ticket 299: lying down, the way the head points; on the back, face up, or on the side, facing us)
           neck: 22, neckW: 26, arms: 55, elbows: 0, elbowCurl: 0, elbowSwing: 0, armLen: 95, armW: 16, legLen: 140, legW: 20, knees: 0, kneeCurl: 0, kneeSwing: 0,
           // (ticket 121) each limb's own pose, dragged in the picture: on
           // top of the sliders' turn and bend, by the friend's own side (L
           // their left, on our right)
           poseArmL: 0, poseArmR: 0, poseElbowL: 0, poseElbowR: 0, poseLegL: 0, poseLegR: 0, poseKneeL: 0, poseKneeR: 0,
           // (ticket 129) each foot's own turn, by side, and both feet's
           poseFootL: 0, poseFootR: 0, feetTurn: 0,
           // (ticket 132) the Body tab: the pose chosen, and the animation on top of it
           pose: "stand", anim: "none",
           // (ticket 136) jumping up into a hug: how high they are held, in a
           // share of their legs' length
           hugLift: 50,
           // (ticket 142) the two jumps: how high (a share of a leg's length)
           // and how fast (100 = as 0.0.66 jumped)
           jumpShortH: 30, jumpShortS: 150, jumpHighH: 90, jumpHighS: 75, runSpeed: 100, accel: 0,
           // (ticket 124) which way the friend faces (toward us, or in profile
           // to our left or right), and whether they sit
           facing: "front", sit: false,
           // (ticket 457) their own size in every place (100: as the place sets it)
           size: 100,
           // (ticket 144) facing us but turned partway to our left or right
           // (a controller's diagonal toward us), or not ("")
           turn: "" },
  shirt: { style: "tee", collar: "v", width: 60, length: 150, color: "#5b8bd0", pattern: "plain",
           patternSize: 14, patternColor: "auto" },
  hands: { style: "round", different: false, size: 14, color: "skin", fingers: 5, alienFingers: 3, clawLen: 100, clawCount: 4, starPoints: 5, starAngle: 0,
           mittenAngle: 0, clawAngle: 0, humanAngle: 0, alienAngle: 0 },
  pants: { style: "long", color: "#34495e", pattern: "plain", patternSize: 14, patternColor: "auto", skirtLen: 50 },
  undies: { on: false, style: "trunks", top: false, color: "#3b82f6", pattern: "plain", patternSize: 14,
            patternColor: "auto" },
  shoes: { style: "sneaker", different: false, size: 100, color: "#c0392b", toes: 5, alienToes: 3, talonCount: 4 },
  // the figure's size and place in a background scene (dragged there)
  background: { kind: "none", figSize: 100, figUnit: "scene", figX: 0, figY: 0, objectsOff: {}, chickenLegs: false, legSize: 100, hutNear: 0,
                leverShine: 100, leverShineColor: "#ffd27a", leverFlicker: 60,   // (ticket 317: the levers' shine: its size, its colour, how much it flickers with the torches)
                // (standing in water: how see-through below the waist, and how big a wake: ticket 122)
                waterSee: 70, waterWake: 100, waterTread: 100, seat: null,   // (ticket 50: the seat the friend sits on here, by name)
                // (ticket 220) 🫧 bubble space: how many, the wind, and themes A (always), B and C (when on)
                bubbleCount: 30, bubbleWind: 30, bubbleSeed: 1,   // (ticket 274: where the bubbles first are, from this)
                // (ticket 329) smaller farther back; a pop's smaller bubbles (how likely, how many), its lines, its sound
                bubbleDepth: 70, bubbleBurst: 40, bubbleBurstCount: 3, bubbleRays: true,
                bubblePopVol: 40, bubblePopInst: "bubble", bubblePopPitch: 0, bubblePopKnob: 50,
                bubbleAColor: "#9ad7ff", bubbleASize: 18, bubbleAShine: 70, bubbleASparkle: "none", bubbleASparkleColor: "#ffffff",
                bubbleBOn: false, bubbleBColor: "#ffb3e6", bubbleBSize: 26, bubbleBShine: 85, bubbleBSparkle: "star", bubbleBSparkleColor: "#fff5b0",
                // (ticket 290) the bubble room: foam on each wall (how high), on the floor and the ceiling (how far out), its colour,
                // a rainbow sheen, how much it moves, and its seed
                // (Matthew, 0.0.237: "the bubbles on the wall/floor should be dynamic, not static": moving from the start) and how many
                // bubbles stick out from each surface into the room
                // (Matthew's own, from his save of 16:56 on 0.0.238: "these are good for the bubble room defaults")
                foamBack: true, foamBackH: 98, foamLeft: true, foamLeftH: 84, foamRight: true, foamRightH: 82, foamFloor: true, foamFloorH: 86,
                foamCeil: true, foamCeilH: 95, foamColor: "#9ad7ff", foamRainbow: true, foamMove: 88, foamSeed: 58909, foamStick: 30, bubbleStick: 30, bubbleFloorWobble: 60,   // (foamStick, bubbleStick: ticket 341; bubbleFloorWobble: 343)
                foamBackOut: 94, foamLeftOut: 82, foamRightOut: 82, foamFloorOut: 94, foamCeilOut: 20,
                instEdit: "template", thingsSolid: true,   // (ticket 283: placed things in the way)   // (ticket 300: in a world square, changes go to the place's template, or 📍 only that square)
                bubbleCOn: false, bubbleCColor: "#c8f7c5", bubbleCSize: 11, bubbleCShine: 60, bubbleCSparkle: "dot", bubbleCSparkleColor: "#ffffff",
                eyeRate: 30, eyeGlow: 70, skullMax: 300, skullEye: 100, blinkApart: false, cauldronFumes: "simple",
                ducks: 5, duckSize: 100, cattails: 14, clouds: 4, cloudSize: 100,
                houseSize: 100, treeSize: 100, toys: 6, flowers: 18,
                houses: 4, bridges: 2, ladders: 3, leaves: 60, treeHouseSize: 100,
                castle: "N", ...Object.fromEntries(Object.entries(CASTLE_DEFAULTS).flatMap(([id, d]) =>
                  [["castleLevels" + id, d.levels], ["castleStone" + id, d.stone], ["castlePitShape1" + id, "circle"], ["castlePitShape2" + id, "circle"],
                  // (ticket 271: each room's own pit shapes; ticket 369: this note sat before the trees and moat, and swallowed them)
                  ...ROOM_NS.flatMap(n => [1, 2].map(i => [pitShapeKey(i, n, id), "circle"])), ["castleTrees" + id, d.trees], ["castleMoat" + id, d.moat],
                   ["castleFlagA" + id, CASTLE_FLAG_DEFAULTS[id][0]], ["castleFlagB" + id, CASTLE_FLAG_DEFAULTS[id][1]], ["castleFlagStyle" + id, CASTLE_FLAG_STYLE_DEFAULTS[id]], ["castleRoof" + id, CASTLE_ROOF_DEFAULTS[id]], ["castleCarpet" + id, CASTLE_CARPETS[0]],
                   ["castleFlagA_B" + id, CASTLE_FLAG_DEFAULTS[id][1]], ["castleFlagB_B" + id, CASTLE_FLAG_DEFAULTS[id][0]],
                   ["castleFlagStyle_B" + id, CASTLE_FLAG_STYLE_DEFAULTS[id]], ["castleCarpet_B" + id, CASTLE_FLAG_DEFAULTS[id][1]],
                   ...ROOM_NS.map(n => ["castleRoomTheme" + n + id, n % 2 ? "A" : "B"]),
                   ...ROOM_NS.map(n => [pitSpikesKey(n, id), false]),   // (ticket 334)
                   ...ROOM_NS.flatMap(n => [[pitOnKey(1, n, id), true], [pitOnKey(2, n, id), true], [leverWireKey(n, id), "normal"], [boulderOnKey(n, id), true], [boulderToneKey(n, id), "#8f8a80"], [leverSpeedKey(n, id), 100], [boulderSpeedKey(n, id), 100], [lidSpeedKey(n, id), 100]]),   // (ticket 305)
                   ...ROOM_NS.flatMap(n => [["castleHallDoors" + n + id, 3], ["castleHallEnd" + n + id, "auto"], ["castleOut" + n + id, "auto"], ["castleBack" + n + id, "none"], ["castleFar" + n + id, "none"], ["castlePlaces" + n + id, 10],
                     ...DOOR_JS.map(j => [hallDoorKey(id, n, j), hallDoorDefault(n, j)])]),
                   ["castleRooms" + id, CASTLE_ROOMS[id].length], ...ROOM_NS.map(n => ["castleRoom" + n + id, CASTLE_ROOMS[id][n - 1] ?? "king"]),
                   ...ROOM_NS.flatMap(n => Object.keys(ROOM_BONES).map(st => [roomBonesKey(id, n, st), 0])),
                   ...ROOM_NS.map(n => [stableHensKey(n, id), STABLE_HENS]),   // (ticket 506: each stable's hens)
                   ...Object.values(RESIDENTS).flatMap(R => [[R.count[0] + id, R.count[2]], ...R.list.flatMap((d, i) => R.fields.map(f => [R.key + (i + 1) + f[0] + id, f[4](d)]))])])),
                castleRoom: 1, livingOut: "house", ...Object.fromEntries([1, 2, 3, 4, 5, 6].map(n => ["treeBack" + n, "none"])),
                ...Object.fromEntries([1, 2, 3, 4, 5, 6].map(n => ["hatchShape" + n, "square"])),   // (ticket 252: each tree house's trapdoor's opening)
                ...Object.fromEntries([1, 2, 3, 4, 5, 6].map(n => ["hatchShut" + n, false])),
                hatchShapeTop: "square", hatchShutTop: false, treetopBack: "none", topFurnish: "none",   // (ticket 517: how the round room is furnished) (ticket 516: the round room at the top of the tree: its trapdoor's opening, shut or open, its way toward us)   // (ticket 334: each tree house's trap door, open till its lever is pulled)
                house1: "fox", house2: "owl", house3: "frog", house4: "bunny", house5: "bear", house6: "cat",
                ...Object.fromEntries([1, 2, 3, 4, 5, 6].flatMap(n => Object.values(HOUSE_COLOR_DEFAULTS).flatMap(d => Object.entries(d).map(([k, v]) => [k + n, v])))),
                lfoFast: "D", lfoSlow: "E", lfoWings: "F",
                inside: 1, room1: "bunks", room2: "kitchen", room3: "books", room4: "lookout", room5: "art", room6: "music",
                windowSize: 100, toys: 10,
                witchX: 150, witchY: 0, witchSize: 100, cauldronSize: 100, crows: 3, frogs: 4, snakes: 2,
                healerSize: 100, healerCatFur: "#e8862a", healerCatWhite: "#ffffff", healerCatEyes: "#22c55e",
                fairyTime: "day", mushrooms: 14, fairyHouseSeen: 100, fairyHouseLfo: "none",
                seeHouse: true, seeTree: true, seeWitch: true, seeCastle: true, seeHealer: true,
                peakHeight: 100, mtClouds: 5, birds: 3, farLake: true,
                pastureHorses: 3, pastureFlowers: 10,   // (ticket 503: the pasture's horses grazing and its patches of wildflowers)
                fallWidth: 100, vines: 8, mist: 60, parrots: 2, fallRate: 100, fallTime: "day",
                caveGems: 16, gemShape: "four", gemSize: 100, gemBright: 90, gemTwinkle: 60,
                caveFloor: "stalagmites", caveCeiling: "stalactites",   // (ticket 471: the cave room's formations)
                desertFar: "mesas",   // (ticket 501: what the desert has far off, farLayers' mountains)
                skyWhere: "denver", skyLat: 39.74, skyLon: -104.99, skyYear: 2026, skyMonth: 10, skyDay: 20, skyHour: 20,   // (ticket 490: 🌍 where and when, for a sky by the clock)
                campTime: "day", fireOn: true, flameSize: 100, campFood: "none", tents: 2, tent: 1,
                crossN: true, crossNE: false, crossE: true, crossSE: false, crossS: true, crossSW: false, crossW: true, crossNW: false, crossFloor: "#76843a", crossBack: "woods", things: [],
                seaMountains: "none", seaForest: "none", seaBeach: false, seaShoreSize: 100,   // (ticket 212: the open water's distant shore, off by default)
                starShape: "mix", starSize: 100, starBright: 85, starTwinkle: 40,
                couchSize: 100, pillows: 4, pictures: 3, picDetail: 70, picSize: 100,
                // (ticket 278) the living rooms' colors, the grass hut's blanket and potion
                couchColor: "#2f6f86", rugColors: "blue", hutCouchColor: "#7a5a3a", hutRugColors: "straw", hutBlanket: "#b8434f", hutPotion: "#7c3aed", hutRooms: 1, hutCenter: false,
                pic1: "lake", pic2: "beach", pic3: "livingroom", pic4: "forest", pic5: "treehouse", pic6: "underwater",
                view: "lake", viewDetail: 80,
                // each tree house room's own two pictures
                ...Object.fromEntries([["lake", "forest"], ["beach", "livingroom"], ["forest", "underwater"], ["treehouse", "lake"], ["house", "witch"], ["cemetery", "beach"]]
                  .flatMap((two, h) => two.map((kd, i) => [`r${h + 1}pic${i + 1}`, kd]))) },
  // the sliders pinned to the full-screen controls, as "part.key"
  hud: { pins: [], min: [] },
  costume: { kind: "none", fur: "#e8862a", marks: "#1f1a17", belly: "#f7ead8", eyes: "#e0a800",
             accent: "#e89aa0", furPattern: "stripes", spot: "auto",
             monkWho: "monk", monkCloak: "#6b4a2b", hoodUp: true, bareFeet: false, hoodFace: true, hoodDim: 0, hoodEyes: true, queenRobe: "#5b2a86", kingRobe: "#7a1f2b", crownJewel: "#d2382b", mantle: true, scepter: true,
             knightA: "#2f6db5", knightB: "#f4f1ea", tabardLayout: "quartered", emblem: "falcon", plume: true, plumeColor: "#c0392b", helmOpen: false, sword: true, shield: true,
             dragonScales: "#3f8f4a", dragonBelly: "#e9d27a", dragonWings: "#d0453a", dragonHorns: "#f4ecd6", dragonWingsHow: "flap", dragonWingLfo: "none",   // (ticket 499)
             robinTunic: "#3f7d3a", robinHat: "#3f7d3a", robinFeather: "#d2382b", bow: true, quiver: true,
             wizardRobe: "#2a3a8c", wizardStars: "#f2c230", staff: true, staffOrb: "#7dd3fc", beardGiven: null,
             gnomeWho: "man", gnomeHat: "#d2382b", gnomeTunic: "#3f8f8a", gnomeTrousers: "#7d8b6a",
             gnomeBodice: "#d2382b", gnomeSkirt: "#d2382b", gnomeBoots: "#e8e4da",
             witchColor: "#6a2c91", webColor: "#3fae4a", webStyle: "tiled", trimColor: "#f28c28", witchCat: false, broom: false,
             catFur: "#1f1f22", catWhite: "#ffffff", catEyes: "#c6d43a", catNose: "#f4a0b0", catSize: 100, broomSize: 100, bootColor: "#1f1f22",
             scubaColor: "#1f2937", scubaAccent: "#14b8a6", diverMetal: "#c9a227", diverCloth: "#b9a27a", diverHose: true, hoseColor: "#4b5563",
             leaks: 0, snorkel: false, snorkelColor: "#facc15", mask: false, maskColor: "#0ea5e9", tank: false, tankColor: "#facc15",
             wingStyle: "butterfly", fairyOutfit: "dress", tunicStyle: "petal", ghostRagged: 30, ghostEyes: "2", ghostHideEyes: false, ghostColor: "#f4f1ea", fairyShoes: "slipper", fairyShoeColor: FAIRY_SHOE_COLORS[0], wingFlutter: true, wingLfo: "none", wreath: true, wreathFlowers: "mix",
             ...Object.fromEntries(Object.entries(FAIRY_WINGS).map(([id, w]) => ["wingColor_" + id, w.colors[0]])),
             ...Object.fromEntries(Object.entries(FAIRY_TUNICS).map(([id, t]) => ["tunicColor_" + id, t.colors[0]])),
             // (ticket 515) the robot: its style, its lights, and each style's own colour (the fairy's way)
             robotStyle: "tin", robotLights: "auto", ...Object.fromEntries(Object.entries(ROBOT_STYLES).map(([id, r]) => ["robotColor_" + id, r.colors[0]])),
             // (ticket 536) the moth: its style, each style's wing colour, and its eyespots
             mothStyle: "luna", mothSpots: true, ...Object.fromEntries(Object.entries(MOTH_STYLES).map(([id, t]) => ["mothColor_" + id, t.colors[0]])),
             // (ticket 522) the cowgirl and the cowboy: each one's style and each style's colour, the hat (both share it) and its colour
             // for each hat, and the lasso (its hand is 🧍 Body's, holdL/holdR, since ticket 518)
             cowgirlStyle: "rodeo", cowboyStyle: "sheriff", cowHat: "cattleman", cowHatAuto: true, lasso: true,
             ...Object.fromEntries(Object.entries(COWGIRL_STYLES).map(([id, t]) => ["cowgirlColor_" + id, t.colors[0]])),
             ...Object.fromEntries(Object.entries(COWBOY_STYLES).map(([id, t]) => ["cowboyColor_" + id, t.colors[0]])),
             ...Object.fromEntries(Object.entries(COW_HATS).map(([id, t]) => ["cowHatColor_" + id, t.colors[0]])),
             // (ticket 557) the ninja: its boots' bands, its head covering (on), the gem's kind and shape, eyes in the slit (off)
             ninjaBoots: "braided", ninjaBand: true, ninjaGem: "ruby", ninjaGemShape: "round", ninjaEyes: false,
             ninjaBetween: "skin",   // (ticket 561, M's design: the skin between the boots' bands; "dark", 🌑 full darkness)
             ninjaGlimmer: true, ninjaGemStars: false, ninjaGemShine: false,   // (ticket 560: the gem's cuts glimmer; its stars and shine, off)
             // (ticket 530) the Sasquatch: its style, each style's colour, its footprints and its grin
             sasquatchStyle: "bigfoot", sasquatchPrints: true, sasquatchGrin: true,
             ...Object.fromEntries(Object.entries(SASQUATCH_STYLES).map(([id, t]) => ["sasquatchColor_" + id, t.colors[0]])),
             // (ticket 544) the giraffe: its style, each style's patch colour, and its neck's length
             giraffeStyle: "reticulated", giraffeNeck: 100,
             ...Object.fromEntries(Object.entries(GIRAFFE_STYLES).map(([id, t]) => ["giraffeColor_" + id, t.colors[0]])) },
};
const clone = o => JSON.parse(JSON.stringify(o));
// before the nose views: a nose was a shape (or "none"), a direction and
// one size
// (0.0.188, Matthew: 👆 walk where you click "so helpful, let's have it on by default") a save from 0.0.187, the hour it
// was off by default, comes back with it on, once; one turned off with its chip since is kept off (the chip marks it,
// clickWalkOn: set)
// (ticket 271) a save from 0.0.175 to 0.0.195, its pits' shapes one setting for the whole castle: each room's own now
function upgradePitShapes(next, saved) {
  const B = next.background, S = saved && saved.background;
  if (!B || !S) return;
  for (const [id] of CASTLES) for (const i of [1, 2]) { const v = S["castlePitShape" + i + id]; if (v === undefined) continue;
    for (const n of ROOM_NS) if (S[pitShapeKey(i, n, id)] === undefined) B[pitShapeKey(i, n, id)] = v; }
}
function upgradeClickWalk(next, saved) {
  const A = next.access, S = saved && saved.access;
  if (A && S && S.clickWalk === false && !S.clickWalkOn) A.clickWalk = true;
}
function upgradeFace(f, saved) {
  // before the teeth choice, teeth were the spikes, and 0 meant none
  if (saved && saved.teethStyle === undefined && saved.teeth !== undefined)
    f.teethStyle = saved.teeth > 0 ? "spikes" : "normal";
  if (!saved || saved.noseView !== undefined || saved.nose === undefined) return;
  f.noseView = saved.nose === "none" ? "none"
             : saved.noseDir === "left" ? "left" : saved.noseDir === "center" ? "front" : "right";
  f.nose = saved.nose === "none" ? "L" : saved.nose;
  f.noseFront = saved.nose === "angle" ? "pointed" : "round";
  if (saved.noseSize !== undefined) f.noseW = f.noseH = saved.noseSize;
}
// before the alien finger slider there were two alien styles
function upgradeHands(h) {
  if (h.style === "alien4" || h.style === "alien3") { h.alienFingers = h.style === "alien4" ? 4 : 3; h.style = "alien"; }
}
// Before 0.0.3 the friend's right hand had only a style of its own, and
// took the rest from the hands, as it drew: "leftStyle" in 0.0.1 (the hand
// on our left: the friend faces us), "rightStyle" in 0.0.2.
function upgradeRightHand(st, saved) {
  const h = st.hands, mine = h.leftStyle && h.leftStyle !== "same" ? h.leftStyle : h.rightStyle;
  if (h.leftStyle && h.leftStyle !== "same") h.different = true;
  if (!(saved && saved.rightHand)) {
    for (const k of HAND_OWN) st.rightHand[k] = h[k];
    if (mine) st.rightHand.style = mine;
  }
  delete h.leftStyle; delete h.rightStyle;
}
// What each hand has of its own: its style and that style's settings.
// With different hands, "hands" holds the friend's left hand's, and
// "rightHand" their right hand's; the color and the size are both hands'.
const HAND_OWN = ["style", "fingers", "alienFingers", "clawLen", "clawCount", "starPoints", "starAngle",
                  "mittenAngle", "clawAngle", "humanAngle", "alienAngle"];
DEFAULTS.rightHand = Object.fromEntries(HAND_OWN.map(k => [k, DEFAULTS.hands[k]]));
// The feet the same way: with different feet, "shoes" holds the friend's
// left foot's (on our right: they face us), and "rightShoe" their right
// foot's; the color and the size are both feet's.
const SHOE_OWN = ["style", "toes", "alienToes", "talonCount"];
DEFAULTS.rightShoe = Object.fromEntries(SHOE_OWN.map(k => [k, DEFAULTS.shoes[k]]));
// before 0.0.17 there was one tree house room, of a kind ("room"): now
// each house has its own, and "inside" says which house you are in
function upgradeInside(bg, saved) {
  const old = saved && saved.background;
  if (old && old.room && old.inside === undefined) bg.inside = Math.max(1, ["bunks", "kitchen", "books", "lookout"].indexOf(old.room) + 1);
  // (0.0.114, Matthew: the living room needs its ▼ back outside) "behind you"
  // was livingBack, "nowhere" at first, and every save says so; it is
  // livingOut now, the house and yard at first. A save that named a place
  // there keeps it.
  if (old && old.livingBack && old.livingBack !== "none" && old.livingOut === undefined) bg.livingOut = old.livingBack;
  delete bg.livingBack;
}
// before 0.0.21 a figure's size in a background was a share of its full
// size (100 filled the stage); now 100 is the scene's own usual size
// (SCENE_FIG). A friend saved at the old usual size gets the new usual
// size; one sized on purpose keeps the size it had in its scene.
// (ticket 185) the world as 0.0.99 to 0.0.102 laid it out, untouched (the
// same places in the same squares, nothing put in any): the world laid out
// again since 0.0.103 (the pond in the middle, the beach on the top edge)
const WORLD_FIRST_LAYOUT = {"2,0":"mountains","0,1":"witch","1,1":"cemetery","2,1":"woodpath","3,1":"fairyforest","0,2":"forest","1,2":"crossing","2,2":"clearing",
                            "3,2":"castle","0,3":"lake","1,3":"crossing","2,3":"crossing","3,3":"house","4,3":"treehouse","0,4":"beach","2,4":"waterfall","3,4":"campsite"};
// (ticket 186) the first world as 0.0.123 laid it out: today's without its top row of open water
// (each square a row up), the beach with no way N
const rowBy = (key, by) => key && key.replace(/,(\d+)$/, (_, r) => "," + (+r + by));
const WORLD_DEFAULT_0123 = (() => { const W = structuredClone(WORLD_DEFAULT_0183), cells = {};
  for (const [k, c] of Object.entries(W.cells)) if (!/,0$/.test(k)) cells[rowBy(k, -1)] = c;
  cells["2,0"].ways = cells["2,0"].ways.filter(d => d !== "N");
  return { ...W, rows: W.rows - 1, start: rowBy(W.start, -1), cells }; })();
// (ticket 195) and as 0.0.103 to 0.0.122 laid it out: its waterfall's only way was N, which is now
// the waterfall itself; it goes NE to the forest crossing (C4, now C5) instead
const WORLD_DEFAULT_0103 = (() => { const W = structuredClone(WORLD_DEFAULT_0123); W.cells["1,4"].ways = ["N"]; W.cells["1,3"].ways = ["N", "E", "S"]; W.cells["2,3"].crossSW = false; return W; })();
const rawWays = c => c.kind === "crossing" ? ["N", "NE", "E", "SE", "S", "SW", "W", "NW"].filter(d => c["cross" + d]) : (c.ways || []);
const sameWorld = (C, D) => Object.keys(C).length === Object.keys(D).length && Object.keys(D).every(k => C[k] && C[k].kind === D[k].kind &&
  rawWays(C[k]).join() === rawWays(D[k]).join() && !(C[k].things || []).length && !C[k].name);
function upgradeWorld(next) {
  const W = next.world, C = (W && W.cells) || {}, keys = Object.keys(C);
  if (keys.length === Object.keys(WORLD_FIRST_LAYOUT).length && keys.every(k => WORLD_FIRST_LAYOUT[k] === C[k].kind) && keys.every(k => !(C[k].things || C[k].crossThings || []).length))
    next.world = clone(WORLD_DEFAULT);
  // (ticket 260: and the defaults before Matthew's world, untouched, to it: where they were is not where they are on it, so
  // from its start)
  else if (sameWorld(C, WORLD_DEFAULT_0103.cells) || sameWorld(C, WORLD_DEFAULT_0123.cells) || sameWorld(C, WORLD_DEFAULT_0183.cells)) next.world = clone(WORLD_DEFAULT);
}
// (ticket 185) the things put in a place: since 0.0.100 every place's, as
// things; in 0.0.98 and 0.0.99 only a forest crossing had them, as crossThings
// (0.0.109) And the tree room's toy count, which shared the name: its
// slider was "things" too, a number, so the defaults' second "things" (the
// list) won, and the tree rooms drew one toy, not ten; it is "toys" now. A
// save with a number for things gets it back as toys, and an empty list.
function upgradeThings(next, saved) {
  const bg = next.background, sb = saved && saved.background;
  if (sb && Array.isArray(sb.crossThings) && !Array.isArray(sb.things)) bg.things = sb.crossThings;
  delete bg.crossThings;
  if (!Array.isArray(bg.things)) {
    if (typeof bg.things === "number" && !(sb && "toys" in sb)) bg.toys = bg.things;
    bg.things = [];
  }
  for (const cell of Object.values((next.world && next.world.cells) || {})) {
    if (Array.isArray(cell.crossThings)) { if (!Array.isArray(cell.things)) cell.things = cell.crossThings; delete cell.crossThings; }
    if (cell.things !== undefined && !Array.isArray(cell.things)) cell.things = [];   // (one never given things stays so: a save loads back as it was)
  }
  // (ticket 206: 0.0.129's cauldron fumes "auto" are simple now, the default)
  const lists = [bg.things, ...Object.values((next.world && next.world.cells) || {}).map(c => c.things), ...Object.values((next.world && next.world.off) || {})];
  for (const T of lists) if (Array.isArray(T)) for (const t of T) if (t && t.kind === "cauldron" && t.fumes === "auto") t.fumes = "simple";
}
// (ticket 340, Matthew 2026-10-18 23:39: one bubble maker in the bubble room, "About 19 frames a second here", in Firefox) The
// cost was the room's own moving foam, not the maker: held still, the same room ran at 52 frames a second and more there. A costly
// effect is off at the start (his rule, 10-16), so the foam is still by default since 0.0.306; a save still at the old default
// (88) is made still once (foamMoveSince marks it done), and an amount chosen since is kept
// (ticket 383, Matthew 2026-10-19: "Yes, I want moving foam back on by default": since 0.0.334 the moving bubbles are page layers
// that cost next to nothing, foamLayersRun) moving by default again (88); a save made still by 0.0.306 (at 0, marked 306) moves once,
// one from before 0.0.306 keeps its amount, and an amount chosen since is kept
function upgradeFoamMove(bg, saved) {
  const old = saved && saved.background, since = old && old.foamMoveSince;
  if (old && since !== undefined && since < 383 && old.foamMove === 0) bg.foamMove = 88;
  bg.foamMoveSince = 383;
}
function upgradeFigSize(bg, saved) {
  const old = saved && saved.background;
  if (old && old.figSize !== undefined && old.figUnit === undefined && old.figSize !== 100)
    bg.figSize = Math.max(10, Math.round(old.figSize / sceneFig(old.kind)));
  bg.figUnit = "scene";
}
// before 0.0.15 there were three toed styles, each with its own count
// (ticket 492, Matthew: "The horse settings should be under 'body', not costume") before 0.0.380 the friend's horse was
// in 🐯 Costume (costume.horse...): a saved friend keeps theirs, in 🧍 Body now, and a pinned horse slider with it
const HORSE_KEYS = ["horse", "horseCoat", "horseMane", "horseMark", "horseTack", "horseSocks", "horseSize"];
function upgradeHorse(st, saved) {
  const old = saved && saved.costume, had = saved && saved.body && saved.body.horse !== undefined;
  for (const k of HORSE_KEYS) { if (old && old[k] !== undefined && !had) st.body[k] = old[k]; delete st.costume[k]; }
  for (const list of [st.hud && st.hud.pins, st.hud && st.hud.min]) if (Array.isArray(list))
    for (let i = 0; i < list.length; i++) if (typeof list[i] === "string" && HORSE_KEYS.some(k => list[i] === "costume." + k)) list[i] = list[i].replace(/^costume\./, "body.");
  // (the review after 0.0.385) and every person away (a look kept in peopleAway), by the same rule
  for (const P of Object.values((st && st.peopleAway) || {})) { const L = P && P.look, c = L && L.costume; if (!c) continue;
    const hadP = !!(L.body && L.body.horse !== undefined);
    for (const k of HORSE_KEYS) if (c[k] !== undefined) { if (!hadP) (L.body = L.body || {})[k] = c[k]; delete c[k]; } }
}
// (ticket 518) from 0.0.401 to ticket 518, the lasso's hand was the costume's lassoHand: "right" is now the right hand
// holding it, chosen in 🧍 Body (holdR "lasso"); "left" was what "auto" does. For the friend and every person away.
function upgradeLassoHand(st) {
  const one = L => { const c = L && L.costume; if (!c || c.lassoHand === undefined) return;
    if (c.lassoHand === "right" && (c.kind === "cowgirl" || c.kind === "cowboy") && c.lasso !== false) { const b = L.body = L.body || {}; if ((b.holdR || "auto") === "auto") b.holdR = "lasso"; }
    delete c.lassoHand; };
  one(st);
  for (const P of Object.values((st && st.peopleAway) || {})) one(P && P.look);
}
function upgradeFeet(st, saved) {
  if (!(saved && saved.rightShoe)) for (const k of SHOE_OWN) st.rightShoe[k] = st.shoes[k];
  for (const f of [st.shoes, st.rightShoe]) {
    if (f.style === "toes5") { f.style = "human"; f.toes = 5; }
    if (f.style === "alien4" || f.style === "alien3") { f.alienToes = f.style === "alien4" ? 4 : 3; f.style = "alien"; }
  }
}
// the music: the background's own tune (none without one) or a chosen one;
// the volume and mute belong to the device, not the friend: Load and Start
// over keep them
// how much of each part of the body shows (VISIBILITY): all of it
DEFAULTS.visible = { all: 100, head: 100, eyes: 100, hair: 100, shirt: 100, pants: 100, feet: 100, hands: 100 };
// (ticket 141) the game controller: each action's button, by the browser's
// "standard" numbers (0 the bottom face button, 1 the right, 2 the left, 3 the
// top, 8 select, 9 start): on a SNES-style pad A (right) the short jump, B
// (bottom) the high one, X (top) run, Y (left) crouch. (The arrows walk.)
// (ticket 146) R (5, the right shoulder) turns crawling on or off. A pad
// taught its buttons (🎓 in the Controls tab) keeps its own layout, by its
// name, in localStorage["dress-up-pads"], put back whenever it connects.
DEFAULTS.controls = { jumpShort: 1, jumpHigh: 0, run: 3, crouch: 2, crawlToggle: 5, back: 8, door: 9, enter: 4, bubbles: 6, fire: 7 };   // (ticket 221: 🫧 on the left trigger, and beside select on the screen; ticket 499: 🔥 on the right trigger, and beside start)
DEFAULTS.music = { track: "scene", volume: 60, mute: false, tempo: 100, transpose: 0, interval: 12, mix: {} };
// (ticket 217, Matthew) ♿ Accessibility: every nav arrow shown, how bright the
// far ones are and their colour (not the near ones' warm white), the arrows'
// size, the size of the words in the game, and the planned path shown
DEFAULTS.access = { allArrows: false, arrowDim: 45, arrowColor: "#38bdf8", arrowSize: 100, gameFont: 100, labelStay: 500, showPath: false, showSolids: false, pathColor: "#fde047", boxColor: "#ef4444", glowFade: 800,
                    pathWidth: 4, boxWidth: 2, boxFont: 11,   // (ticket 230: the path's and the boxes' lines, the boxes' names, in px)
                    showMadeOf: false, madeCats: {}, madeFont: 10, madeColor: "#ffffff", madeWidth: 2,
                    clickWalk: true, clickWalkColor: "#22d3ee", pathAltColor: "#c084fc", clickPlan: "replace", leverFar: false,   // (leverFar: ticket 364)
                    showBubbleHits: false, bubbleHitColor: "#38bdf8",   // (ticket 290: 🫧 the bubble room's bubbles that pop, as circles)
                    showWater: false, waterColor: "#22d3ee", solidCats: {} };   // (ticket 283: 💧 where the friend is in water; the collision boxes' kinds, each on unless off)   // (ticket 251: 👆 walk where you click, and its glow's colour; on at first since 0.0.188, Matthew: "so helpful, let's have it on by default")   // (ticket 258: 🧩 what a place is made of; each kind's chip, MADE_CATS's when unset)
// (ticket 236, Matthew: "Turn on the accessibility features during all tests, because: * we want them to always work.
// * the screenshots may be more informative.") A page opened at index.html#inspect starts with every 🔬 overlay on: all
// the arrows, the planned path, the collision boxes and what each place is made of; as its defaults (so a check that
// starts over keeps them) and over a saved friend's settings. CI's runs open the game so; a check can still turn one off.
const INSPECT_ALL = (() => { try { return /(^#|&)inspect(&|$)/.test(location.hash); } catch (_) { return false; } })();
const INSPECT_ON = { allArrows: true, showPath: true, showSolids: true, showMadeOf: true, showBubbleHits: true, showWater: true };
if (INSPECT_ALL) Object.assign(DEFAULTS.access, INSPECT_ON);
for (const kd of [...NORTH_OUT, ...NORTH_ROOMS]) DEFAULTS.background["north_" + kd] = northDefault(kd);   // (ticket 214: each place's 🔭)
let state = clone(DEFAULTS);
// (ticket 155, Matthew: "The priority is 60 FPS at all times, for all
// actions that a game controller does in the scene.") What a background
// can draw that costs frames is kept OFF until it can be made fast, each
// one switched on in the ⚙️ advanced tab, which says how many there are
// in the place and can measure what they cost here. The settings are this
// computer's (localStorage "dress-up-perf"), not the friend's: a slow one,
// like the black laptop, keeps them off whatever friend is loaded. A new
// one: an entry in PERF_FEATURES, a flag the drawing reads (PERF.id), and
// what to count. perfSet empties the scene caches, so every place draws
// again with it.
const PERF_FEATURES = [
  { id: "blur", label: "🌫️ soft blurs", what: "the soft edges in pictures on walls and in the imagination's map, the waterfall's mist and the leaves outside the cave (Gaussian blurs). Off, pictures are sharp and the mist is a soft gradient.",
    count: () => document.querySelectorAll("#bg feGaussianBlur").length + [...document.querySelectorAll('#bg image[href^="data:image/svg"]')].filter(i => i.getAttribute("href").includes("feGaussianBlur")).length + document.querySelectorAll("#bg image[data-blur]").length, unit: ["blur", "blurs"] },
  // (ticket 158: every framed picture, not only the imagination's: the living
  // room's three held it to 49 frames a second dragging, 60 held still. A
  // window's view of outside stays moving: it has no frame)
  { id: "livePictures", label: "🎞️ moving pictures", what: "framed pictures drawn live, each place's own animations playing: on the walls of the living room, the healer's hut and the castle's rooms, and the 22 in the 💭 imagination's map. Off, each is that place held still, as an image. A window's view of outside moves either way.",
    count: () => [...document.querySelectorAll("#bg .portal")].filter(p => p.querySelector(":scope > rect") && p.querySelector(":scope > svg")).length, unit: ["picture drawn live", "pictures drawn live"] },
  // (ticket 164: on a computer too slow for them, a place's animations, the
  // wake and the faint copy under the water cost it most of its frames)
  { id: "slowMoving", label: "🌀 moving scenery and water on a slow computer", what: "on a computer measured to be slow (a drawing of the friend takes over 6 ms, on average, or frames come late while the friend moves), each place is held still: its animations (ducks, the sun's rays, the fumes, fairies' wings), the rings round the friend in water, and the faint part of the friend under the water; and while the friend moves, the place is drawn as one still picture, its standing things (trees, rocks) as they are. On, everything moves anyway. A fast computer is never held still.",
    count: () => 0, unit: ["", ""], status: () => `this computer: ${slowFlag || choppy ? "slow" : "not slow"} (a drawing takes ${slowAvg.toFixed(1)} ms, on average${slowN < 30 ? ", still measuring" : ""}${choppy ? "; frames came late while the friend moved" : ""})${slowFlag || choppy ? (PERF.slowMoving ? ": everything moves anyway" : ": the places are held still") : ": everything moves"}` },
  // (ticket 161: Matthew found the beach and the empty room nearly frozen;
  // a friend half see-through walked at 36 frames a second in the empty
  // room, 60 without these masks)
  { id: "exactSeeThrough", label: "👻 exact see-through", what: "a see-through friend (👻 Visibility) drawn so that a part never shows through itself where its layers overlap, with masks the browser draws again on every frame the friend moves. Off, each layer is see-through on its own: where two layers of one part overlap (the hair behind the head and in front of it), the one behind shows a little.",
    count: () => document.querySelectorAll('#pic [mask^="url(#veil-m-"]').length, unit: ["see-through mask", "see-through masks"] },
];
const PERF = Object.fromEntries(PERF_FEATURES.map(f => [f.id, false]));
try { Object.assign(PERF, JSON.parse(localStorage.getItem("dress-up-perf") || "{}")); } catch (e) { /* (all off) */ }
function perfSet(id, on, keep = true) {
  PERF[id] = !!on;
  if (keep) try { localStorage.setItem("dress-up-perf", JSON.stringify(PERF)); } catch (e) { /* (for this visit only) */ }
  PARTS.clear(); READS.clear(); TRIMMED.clear(); STILLS.clear(); SCENE_PARTS.clear();
  bgShown.text = null; bgShown.kept = new Map();
  changed();
}
// (ticket 155) Frames a second, counted each second while the ⚙️ advanced
// tab is shown (a frame the page could draw: idle, it is the screen's
// rate), and a drawing's cost on average (changed() times draw()).
const perfMeter = { node: null, loop: 0, t0: 0, n: 0, ms: 0, draws: 0 };
function perfMeterTick(t) {
  perfMeter.loop = 0;
  if (!perfMeter.node || !perfMeter.node.isConnected || document.hidden) { perfMeter.t0 = 0; return; }
  if (!perfMeter.t0) Object.assign(perfMeter, { t0: t, n: 0, ms: 0, draws: 0 });
  else perfMeter.n++;
  if (t - perfMeter.t0 >= 1000) {
    const fps = Math.round(1000 * perfMeter.n / (t - perfMeter.t0));
    perfMeter.node.textContent = `🎞️ frames a second now: ${fps}` + (perfMeter.draws ? ` · a drawing takes ${(perfMeter.ms / perfMeter.draws).toFixed(1)} ms (${perfMeter.draws} in the last second)` : " · nothing drawn in the last second");
    Object.assign(perfMeter, { t0: t, n: 0, ms: 0, draws: 0 });
  }
  perfMeter.loop = requestAnimationFrame(perfMeterTick);
}
// (ticket 163, Matthew) 🧪 This computer's speed, to compare two computers
// (his laptop and the black laptop) and set the frame-rate gate's slowdown
// from the ratio: the same work everywhere, in the empty room, with the
// friend as they start (the defaults; Matthew: "so all params are fixed and
// controlled across all samples") and the music off. (1) The friend drawn 60 times back to back: the
// median ms of a drawing, the page's main thread at work. (2) 2 s walking
// across the room as the controller walks them: frames a second, the
// longest pause between frames, drawings a second. (3) 2 s walking in
// place, as with no background. Then everything as it was.
// (4-14: the duck pond, each part of it away in turn, and music; 15-20, ticket 344: the bubble room, standing, walking
// through it, its foam still, and with 1, 4 and 16 bubble makers)
// (ticket 166) Where the speed lines are pasted for now: a public Etherpad
// (Riseup's; anyone with the address can read and edit it, and it is
// deleted after 60 days with no edits), read back as text from
// <pad>/export/txt. To be replaced by a Fumola canister we control.
const SPEED_PAD = "https://pad.riseup.net/p/LgBrn8VRzEJxrsmMW82I";
// (ticket 166) Its replacement: a log in the Fumola canister (Adapton/fumola,
// crates/fumola_canister, branch ic-canister), which keeps entries
// {key, value} per space, appended by POST <canister>/s/<space>/log and read
// back by GET there (tools/speedlog.py). Null until the canister is
// deployed, and then the 📮 buttons show beside the 📝 links. A device can
// point it elsewhere (a local canister, a test) with
// localStorage["dress-up-speed-log"].
const SPEED_LOG_DEPLOYED = null;
const SPEED_LOG = (() => { try { return localStorage.getItem("dress-up-speed-log") || SPEED_LOG_DEPLOYED; } catch (e) { return SPEED_LOG_DEPLOYED; } })();
// (a line sent to the log, on a press, never by itself: the key says what
// it is, which version and when; the button says whether it went)
function sendSpeedLine(kind, line, btn) {
  if (!SPEED_LOG || !line) return Promise.resolve(false);
  if (btn) btn.textContent = "📮 sending…";
  const key = `${kind} ${VERSION.number} ${new Date().toISOString()}`;
  return fetch(SPEED_LOG, { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ key, value: line }) })
    .then(r => { if (!r.ok) throw new Error(r.status); if (btn) btn.textContent = "📮 sent ✓"; return true; })
    .catch(() => { if (btn) btn.textContent = "📮 not sent: copy and paste it"; return false; });
}
// (Matthew, 2026-10-16, on 0.0.90: pauses going through a castle door and
// into a pit, on his laptop's Firefox, where headless Chrome and Firefox
// show none: the browser's own painting, which only his laptop can show)
// ⏱️ time the next trip: armed, the next trip through a doorway (a click,
// X, 🚪, walking onto a pit) records every frame from its start until
// TRIP_TAIL ms after the friend has arrived and stands: its gap, the trip's
// phase then (tripPhase, set as a trip goes on), and the game's own work in
// it (drawing, building a place: tripWork); then a line to copy and paste,
// as 🧪's: the long frames, each phase's frames, this computer.
const TRIP_TAIL = 1500, TRIP_LONG = 50;
let tripTimer = null, tripPhase = "";
function tripArm(report) { tripTimer = { armed: true, report }; report("⏱️ ready: now go through a doorway (click a door or a pit, press X or 🚪, or walk onto a pit)…"); }
// (the phase after "arriving", two frames on: the frame that builds the new place and the one that first paints it are "arriving")
function tripNext(ph) { if (!tripTimer) { tripPhase = ph; return; } requestAnimationFrame(() => requestAnimationFrame(() => { if (tripPhase === "arriving") tripPhase = ph; })); }
function tripWork(what, t) { const T = tripTimer; if (T && !T.armed && !T.done) T.work.push([t - T.t0, performance.now() - t, what]); }
function tripStart(door, how) {
  const T0 = tripTimer;
  if (!T0 || !T0.armed) return;
  const B = state.background, T = { report: T0.report, t0: performance.now(), how, frames: [], work: [], standAt: 0, done: false,
    from: B.kind + (B.kind === "castleroom" ? " " + (B.castleRoom ?? 1) : ""), to: door.dataset.to + (door.dataset.room ? " " + door.dataset.room : "") };
  tripTimer = T; tripPhase = how === "spin" ? "spinning" : "walking";
  T.report("⏱️ timing this trip…");
  let last = 0;
  const f = t => {
    if (tripTimer !== T) return;
    if (last) T.frames.push([last - T.t0, t - last, tripPhase]);
    last = t;
    // (standing: not entering, not landing a jump, done fading or spinning in)
    if (!entering && !isJump(state.body.anim) && !["arriving", "fading in", "spinning out"].includes(tripPhase)) { if (!T.standAt) { T.standAt = t; tripPhase = "arrived"; } }
    else T.standAt = 0;
    if (T.standAt && t - T.standAt > TRIP_TAIL) { T.done = true; tripReport(T); tripTimer = null; return; }
    requestAnimationFrame(f);
  };
  requestAnimationFrame(f);
}
function tripReport(T) {
  const F = T.frames, all = F.reduce((a, f) => a + f[1], 0) || 1, fps = n => Math.round(1000 * n.length / (n.reduce((a, f) => a + f[1], 0) || 1));
  const workIn = (a, b) => T.work.filter(([t]) => t >= a - 1 && t < b);
  const long = F.filter(f => f[1] > TRIP_LONG).map(([t, g, ph]) => { const w = workIn(t, t + g), ms = w.reduce((a, x) => a + x[1], 0), place = w.filter(x => x[2] === "building the place").reduce((a, x) => a + x[1], 0);
    return { at: +(t / 1000).toFixed(2), ms: Math.round(g), phase: ph, game: Math.round(ms), place: Math.round(place) }; });
  const phases = [...new Set(F.map(f => f[2]))].map(ph => { const n = F.filter(f => f[2] === ph); return { phase: ph, s: +(n.reduce((a, f) => a + f[1], 0) / 1000).toFixed(2), fps: fps(n), worst: Math.round(Math.max(...n.map(f => f[1]))) }; });
  const ua = navigator.userAgent, browser = (ua.match(/(Firefox|Edg|Chrome|Safari)\/(\d+)/) || [, "a browser", ""]).slice(1).join(" ").trim();
  const data = { v: VERSION.number, trip: `${T.from} > ${T.to}`, how: T.how, s: +(all / 1000).toFixed(2), frames: F.length, fps: fps(F), long, phases,
                 game: Math.round(T.work.reduce((a, x) => a + x[1], 0)), browser, screen: `${screen.width}x${screen.height}`, dpr: devicePixelRatio,
                 pic: (r => `${Math.round(r.width)}x${Math.round(r.height)}`)(pic.getBoundingClientRect()), found: { slow: slowFlag, choppy, slowMoving: PERF.slowMoving } };
  T.report(`⏱️ Dress Up ${VERSION.number}, a trip from ${T.from} to ${T.to} (${T.how}): ${data.s} s, ${F.length} frames, ${data.fps} fps · ` +
           (long.length ? `long frames: ${long.map(l => `${l.ms} ms at ${l.at} s (${l.phase}; the game's own work ${l.game} ms${l.place ? ", building the place " + l.place : ""})`).join(", ")}` : `no frame over ${TRIP_LONG} ms`) +
           ` · ${phases.map(p => `${p.phase} ${p.fps} fps (longest ${p.worst} ms)`).join(", ")} · ${browser}, ${devicePixelRatio}× · DATA ${JSON.stringify(data)}`);
}
async function speedCheck(report) {
  if (perfBusy) return;
  perfBusy = true;
  measuringNote("Measuring this computer's speed", 62);
  const saved = JSON.stringify(state), wait = ms => new Promise(r => setTimeout(r, ms));
  const keep = { slowFlag, choppy, slowMoving: PERF.slowMoving };
  // (a pass: frames a second, the longest pause, the share of frames later
  // than 25 ms, the friend's drawings a second, a drawing's mean ms)
  const frames = (ms, step) => new Promise(done => {
    const gaps = []; let t0 = 0, last = 0, d0 = movedDraws, ms0 = movedMs;
    const f = t => {
      if (!t0) t0 = last = t; else { gaps.push(t - last); }
      const dt = Math.min(0.1, (t - last) / 1000); last = t;
      if (t - t0 >= ms) {
        const s = gaps.reduce((a, b) => a + b, 0), dn = movedDraws - d0;
        done({ fps: Math.round(1000 * gaps.length / s), worst: Math.round(Math.max(...gaps)), late: Math.round(100 * gaps.filter(g => g > 25).length / gaps.length),
               dps: Math.round(1000 * dn / s), ms: dn ? +((movedMs - ms0) / dn).toFixed(1) : null });
        return;
      }
      if (step) step(dt);
      requestAnimationFrame(f);
    };
    requestAnimationFrame(f);
  });
  const across = (B, body, speed, anim) => { let dir = 1; return dt => {
    body.anim = anim; B.figX = (B.figX ?? 0) + dir * speed * dt;
    if (B.figX > 150) dir = -1; if (B.figX < -150) dir = 1;
    body.facing = dir > 0 ? "right" : "left";
    moved();
  }; };
  try {
    // (the late-frame trigger rests during the check, which must not decide this computer is choppy)
    moveSuspend = true;
    PERF.slowMoving = true;   // (the scenery moves, whatever this computer was found to be, except in the pass held still)
    state = clone(DEFAULTS); state.music.mute = true;
    let B = state.background, body = state.body;
    Object.assign(B, { kind: "emptyroom", figX: -150, figY: 0 }); body.anim = "none"; body.facing = "right";
    changed(); await wait(600);
    report("🧪 1 of 20: drawing the friend…"); await wait(50);
    body.anim = "walk";
    const times = [];
    for (let i = 0; i < 60; i++) { const t = performance.now(); draw(); times.push(performance.now() - t); }
    times.sort((a, b) => a - b);
    const med = times[30];
    report("🧪 2 of 20: walking across the empty room…"); await wait(300);
    const room = await frames(2000, across(B, body, PAD_SPEED.walk, "walk"));
    report("🧪 3 of 20: walking in place…"); B.figX = 0; changed(); await wait(300);
    const inPlace = await frames(2000, null);
    // (ticket 164, Matthew: the duck pond, swimming across, as the controller swims them, in the water's middle row)
    B.kind = "lake"; B.figX = -150; body.anim = "swim";
    { const ys = []; for (let y = -200; y <= 86; y += 2) if (inWater(B, 0, SCENE.ground + y) && inWater(B, -150, SCENE.ground + y) && inWater(B, 150, SCENE.ground + y)) ys.push(y); B.figY = ys[ys.length >> 1] ?? 0; }
    changed(); report("🧪 4 of 20: swimming in the duck pond…"); await wait(1200);
    const pond = await frames(2000, across(B, body, PAD_SPEED.swim, "swim"));
    report("🧪 5 of 20: the duck pond held still…");
    SPEED_TEST.still = true; PERF.slowMoving = false; SPEED_TEST.noPic = true; changed(); await wait(800);   // (held still, not yet a picture: 0.0.86's pass)
    const pondStill = await frames(2000, across(B, body, PAD_SPEED.swim, "swim"));
    SPEED_TEST.still = false; PERF.slowMoving = true; SPEED_TEST.noPic = false;
    // (0.0.86, Matthew: which part of the pond costs the frames; each pass
    // swims as the first, moving, with one part taken away: SPEED_TEST)
    const without = async (n, what, on, off) => {
      report(`🧪 ${n} of 20: the duck pond ${what}…`); on(); changed(); await wait(800);
      for (let i = 0; i < 40 && stillPicWanted() && !(stillPic.parts.size && [...stillPic.parts.values()].every(q => q.ready)); i++) await wait(100);   // (a slow computer's pictures, made)
      stillPic.draws = stillPic.drawsOn = 0;
      const r = await frames(2000, across(B, body, PAD_SPEED.swim, "swim"));
      if (stillPic.draws) r.pic = Math.round(100 * stillPic.drawsOn / stillPic.draws);   // (the share of drawings with the still picture)
      off(); changed(); return r;
    };
    const NS = "http://www.w3.org/2000/svg", holder = () => { const g = document.createElementNS(NS, "g"); g.id = "bg"; return g; };
    const pondNoScene = await without(6, "without its scenery", () => { SPEED_TEST.holder = holder(); }, () => { SPEED_TEST.holder = null; });
    const pondNoUnder = await without(7, "without the friend's part under the water", () => { SPEED_TEST.noUnder = true; }, () => { SPEED_TEST.noUnder = false; });
    const pondNoRings = await without(8, "without the rings", () => { SPEED_TEST.noRings = true; }, () => { SPEED_TEST.noRings = false; });
    // (0.0.87: as held still, but the place a still picture while the friend
    // moves, as a slow computer now draws it (stillPic); 0.0.86's pass 9 made
    // the whole place one picture, the friend in front of everything)
    // (0.0.93: pass 9 the pictures as svg, pass 10 as bitmaps, as the game draws them since the iPad's 0.0.92 line)
    const pondPicture = await without(9, "with its scenery a still picture (svg)", () => { SPEED_TEST.still = true; PERF.slowMoving = false; SPEED_TEST.svg = true; }, () => { SPEED_TEST.still = false; PERF.slowMoving = true; SPEED_TEST.svg = false; });
    // (and both of the friend's water parts away at once)
    const pondBitmap = await without(10, "with its scenery a still bitmap", () => { SPEED_TEST.still = true; PERF.slowMoving = false; }, () => { SPEED_TEST.still = false; PERF.slowMoving = true; });
    const pondNoWater = await without(11, "without the rings and the part under water", () => { SPEED_TEST.noRings = SPEED_TEST.noUnder = true; }, () => { SPEED_TEST.noRings = SPEED_TEST.noUnder = false; });
    report("🧪 12 of 20: the duck pond with its music…");
    state.music.mute = false; changed(); await wait(1200);
    const pondMusic = await frames(2000, across(B, body, PAD_SPEED.swim, "swim"));
    // (Matthew: after the pond, the empty room was as slow as the pond,
    // until the page was loaded again: the room once more, as the first
    // time, then with its music)
    report("🧪 13 of 20: the empty room, after the pond…");
    state.music.mute = true; Object.assign(B, { kind: "emptyroom", figX: -150, figY: 0 }); body.anim = "walk"; body.facing = "right"; changed(); await wait(600);
    const roomAfter = await frames(2000, across(B, body, PAD_SPEED.walk, "walk"));
    report("🧪 14 of 20: the empty room after the pond, with its music…");
    state.music.mute = false; changed(); await wait(1200);
    const roomAfterMusic = await frames(2000, across(B, body, PAD_SPEED.walk, "walk"));
    // (ticket 344, Matthew: "let's add bubble room to the suite of tests that we automate with the advanced tab. I want to
    // record some tests on the black laptop and ipad tonight") 🫧 the bubble room, as it starts: standing, then walking
    // across near us through the bubbles stuck out into it (popping them), then the same with its foam still; then with 1, 4
    // and 16 bubble makers along its floor, walking; and how many shapes its scene is
    state.music.mute = true;
    const maker = (i, n) => ({ kind: "bubblemaker", x: Math.round(-260 + 520 * (i + 0.5) / n), z: i % 2 ? 300 : 345, size: 100, rate: 4, theme: "A", color: MAKER_COLORS[i % MAKER_COLORS.length], mirror: i % 2 === 1 });
    // (since 0.0.306 the foam is still at the start, ticket 340: moving, as it was, for the first two passes; still for the rest)
    Object.assign(B, { kind: "bubbleroom", figX: -150, figY: 40, things: [], foamMove: 88 }); body.anim = "none"; body.facing = "right"; changed();
    report("🧪 15 of 20: the bubble room, standing…"); await wait(1500);
    const bubbleNodes = bgShown.node ? bgShown.node.querySelectorAll("*").length : null;
    const bubbleStand = await frames(2000, null);
    report("🧪 16 of 20: walking through the bubble room…"); ballPops.clear(); changed(); await wait(300);
    const bubbleWalk = await frames(2000, across(B, body, PAD_SPEED.walk, "walk")); bubbleWalk.popped = ballPops.size;
    report("🧪 17 of 20: the bubble room, its foam still…"); B.foamMove = 0; ballPops.clear(); changed(); await wait(800);
    const bubbleStill = await frames(2000, across(B, body, PAD_SPEED.walk, "walk"));   // (and the makers' passes with it still: what they cost alone)
    const withMakers = async (n, step) => { report(`🧪 ${step} of 20: the bubble room with ${n} bubble maker${n > 1 ? "s" : ""}…`);
      B.things = Array.from({ length: n }, (_, i) => maker(i, n)); ballPops.clear(); changed(); await wait(1500);
      const r = await frames(2000, across(B, body, PAD_SPEED.walk, "walk")); r.bubbles = makerSim.list.length; return r; };
    const bubbleM1 = await withMakers(1, 18), bubbleM4 = await withMakers(4, 19), bubbleM16 = await withMakers(16, 20);
    B.things = []; changed();
    const ua = navigator.userAgent, browser = (ua.match(/(Firefox|Edg|Chrome|Safari)\/(\d+)/) || [, "a browser", ""]).slice(1).join(" ").trim();
    const p = r => `${r.fps} fps (longest pause ${r.worst} ms, ${r.late}% late)`;
    const data = { v: VERSION.number, draw: +med.toFixed(2), room, inPlace, pond, pondStill, pondNoScene, pondNoUnder, pondNoRings, pondPicture, pondBitmap, bitmapOk: stillPic.bitmapOk, pondNoWater, pondMusic, roomAfter, roomAfterMusic, bubble: { nodes: bubbleNodes, stand: bubbleStand, walk: bubbleWalk, still: bubbleStill, m1: bubbleM1, m4: bubbleM4, m16: bubbleM16 }, browser, platform: navigator.platform || "",
                   screen: `${screen.width}x${screen.height}`, dpr: devicePixelRatio, cores: navigator.hardwareConcurrency || null, memory: navigator.deviceMemory || null,
                   pic: (r => `${Math.round(r.width)}x${Math.round(r.height)}`)(pic.getBoundingClientRect()), found: { slow: keep.slowFlag, choppy: keep.choppy } };
    report(`🧪 Dress Up ${VERSION.number}: a drawing ${med.toFixed(1)} ms · empty room, walking: ${p(room)} · in place: ${inPlace.fps} fps · duck pond, swimming: ${p(pond)} · held still: ${p(pondStill)} · without its scenery: ${p(pondNoScene)} · without the part under water: ${p(pondNoUnder)} · without the rings: ${p(pondNoRings)} · scenery a still picture: ${p(pondPicture)} · a still bitmap: ${p(pondBitmap)} · without the rings and the part under water: ${p(pondNoWater)} · with music: ${p(pondMusic)} · the empty room after the pond: ${p(roomAfter)}, with its music: ${p(roomAfterMusic)} · 🫧 the bubble room (${bubbleNodes} shapes), standing: ${bubbleStand.fps} fps, walking through it: ${p(bubbleWalk)}, its foam still: ${p(bubbleStill)}, with 1 bubble maker: ${p(bubbleM1)}, 4: ${p(bubbleM4)}, 16: ${p(bubbleM16)} · ${browser}, ${devicePixelRatio}× · DATA ${JSON.stringify(data)}`);
  } finally {
    state = JSON.parse(saved);
    Object.assign(SPEED_TEST, { holder: null, noUnder: false, noRings: false, noPic: false, svg: false, still: false });
    slowFlag = keep.slowFlag; choppy = keep.choppy; PERF.slowMoving = keep.slowMoving;
    moveSuspend = false; moveFrames = moveLate = 0; walkFrames = walkLate = 0;
    perfBusy = false;
    measuringNote(null);
    changed();
  }
}
// (ticket 155) What a feature costs here: the friend walks right and back
// for 2 s with it off, then 2 s with it on, as the controller walks them
// (no controller needed), counting frames and timing each drawing; then
// the friend, the place and the setting as they were.
let perfBusy = false;
// (ticket 179, Matthew) while 🧪 or 📏 measures: the ⚙️ Advanced tab's
// buttons rest (copying, pasting or starting another measure mid-way would
// spoil it or copy half a line), and a notice over everything says to wait
// and touch nothing, with the seconds gone against about how long it takes
let measuringTimer = 0;
function measuringNote(what, secs) {
  const note = document.getElementById("measuring"), on = !!what;
  document.body.classList.toggle("measuring", on);
  for (const b of document.querySelectorAll("#panel .perfrow button")) b.disabled = on;
  for (const a of document.querySelectorAll("#panel .perfrow a")) { a.classList.toggle("resting", on); if (on) a.setAttribute("aria-disabled", "true"); else a.removeAttribute("aria-disabled"); }
  clearInterval(measuringTimer); measuringTimer = 0;
  if (!on) { note.hidden = true; note.replaceChildren(); return; }
  const t0 = performance.now(), big = el("span", { class: "big" }, `⏳ ${what}: please wait, and touch nothing`), line = el("span"), bar = el("i");
  note.replaceChildren(big, line, el("div", { class: "bar" }, [bar]));
  const tick = () => { const s = (performance.now() - t0) / 1000;
    line.textContent = `${Math.floor(s)} s so far, of about ${secs} s`; bar.style.width = `${Math.min(100, 100 * s / secs).toFixed(1)}%`; };
  tick(); note.hidden = false;
  measuringTimer = setInterval(tick, 250);
}
async function perfMeasure(f, report) {
  if (perfBusy) return;
  perfBusy = true;
  measuringNote(`Measuring ${f.label}`, 5);
  const B = state.background, body = state.body, keep = { x: B.figX, y: B.figY, anim: body.anim, facing: body.facing, was: PERF[f.id] };
  const walk = on => new Promise(done => {
    perfSet(f.id, on, false);
    let t0 = 0, last = 0, frames = 0, ms = 0, draws = 0;
    const step = t => {
      if (!t0) t0 = last = t;
      const dt = Math.min(0.1, (t - last) / 1000), u = (t - t0) / 2000;
      last = t;
      if (u >= 1) { done({ fps: 1000 * frames / (t - t0), ms: ms / Math.max(1, draws) }); return; }
      frames++;
      body.anim = "walk"; body.facing = u < 0.5 ? "right" : "left";
      if (B.kind !== "none") B.figX = Math.round(10 * clamp((B.figX ?? 0) + (u < 0.5 ? 1 : -1) * PAD_SPEED.walk * dt, sceneFrame.left, sceneFrame.right)) / 10;
      const a = performance.now(); changed(); ms += performance.now() - a; draws++;
      requestAnimationFrame(step);
    };
    setTimeout(() => requestAnimationFrame(step), 500);   // (a moment for the place, drawn anew, to settle)
  });
  report(`📏 measuring ${f.label}: off…`);
  const off = await walk(false);
  report(`📏 measuring ${f.label}: on…`);
  const on = await walk(true);
  Object.assign(B, { figX: keep.x, figY: keep.y }); Object.assign(body, { anim: keep.anim, facing: keep.facing });
  perfSet(f.id, keep.was, false);
  perfBusy = false;
  measuringNote(null);
  const line = r => `${Math.round(r.fps)} frames a second, ${r.ms.toFixed(1)} ms a drawing`;
  report(`📏 walking here, off: ${line(off)}; on: ${line(on)}.`);
}
try {
  const saved = JSON.parse(localStorage.getItem("dress-up-v1") || "null");
  if (saved) for (const k of Object.keys(DEFAULTS)) state[k] = { ...DEFAULTS[k], ...(saved[k] || {}) };
  upgradeHands(state.hands);
  upgradeRightHand(state, saved);
  upgradeFeet(state, saved);
  upgradeHorse(state, saved);
  upgradeLassoHand(state);
  upgradeInside(state.background, saved);
  upgradeFigSize(state.background, saved);
  upgradeFoamMove(state.background, saved);
  upgradeThings(state, saved);
  upgradeWorld(state);
  upgradeClickWalk(state, saved);
  upgradePitShapes(state, saved);
  if (INSPECT_ALL) Object.assign(state.access, INSPECT_ON);   // (ticket 236)
  upgradeFace(state.face, saved.face);
  if (state.body.sit && state.body.pose === "stand") state.body.pose = "sit";   // (ticket 132)
  // pants length moved to the legs (leg length)
  if (saved && saved.pants && saved.pants.length && !(saved.body && saved.body.legLen))
    state.body.legLen = saved.pants.length;
} catch (_) { /* no storage: start fresh */ }
function persist() { try { localStorage.setItem("dress-up-v1", JSON.stringify(state)); } catch (_) {} }

// (ticket 134) This version, shown after the game's name, and newer ones.
// A release changes both this and version.json beside the page (the same
// number and date), in the same commit. Each minute the page asks for
// version.json (afresh, past every cache); when it names a newer version, a
// chip after this one says so, and nothing else happens until the player
// clicks it. Then the page loads the new version, at an address of its own
// so no cache hands back the old one; the friend, the place and every other
// setting are kept (saved as they always are, and read back on loading).
// (ticket 194, M) light or dark: auto (the system's), light or dark, this
// browser's own (localStorage "dress-up-theme"), not the friend's, so not in
// saves; set as early as the script runs, chosen in ⚙️ Advanced
const LOOKS = [["auto", "🌗 auto"], ["light", "☀️ light"], ["dark", "🌙 dark"]];
function themeNow() { try { const t = localStorage.getItem("dress-up-theme"); return t === "light" || t === "dark" ? t : "auto"; } catch (_) { return "auto"; } }
function themeSet(t) {
  if (t === "light" || t === "dark") document.documentElement.dataset.theme = t; else delete document.documentElement.dataset.theme;
  try { if (t === "light" || t === "dark") localStorage.setItem("dress-up-theme", t); else localStorage.removeItem("dress-up-theme"); } catch (_) { /* (for this visit) */ }
}
themeSet(themeNow());
const VERSION = { number: "0.0.411", released: "2026-10-22" };
const newerVersion = (a, b) => {
  const x = String(a).split(".").map(Number), y = String(b).split(".").map(Number);
  for (let i = 0; i < Math.max(x.length, y.length); i++) if ((x[i] || 0) !== (y[i] || 0)) return (x[i] || 0) > (y[i] || 0);
  return false;
};
let offered = null;
// (ticket 184) A release goes out first, and GitHub's computers check its
// speed after (.github/workflows/dress-up-speed.yml), into speed.json beside
// this page. When this version failed the frame-rate gate, 🚧🐢 says so after
// its number (🚧 when one of the checks failed), a link to the run; when it
// passed, or is still being checked, nothing shows. Read with version.json.
function markSpeed(r) {
  const el = document.getElementById("speed-mark");
  if (!el) return;
  const bad = !!r && (r.result === "slow" || r.result === "broken");
  el.hidden = !bad;
  if (!bad) return;
  el.textContent = r.result === "slow" ? "🚧🐢 slow" : "🚧 being fixed";
  el.title = (r.result === "slow" ? "This version runs slower than it should" : "Something in this version needs fixing") +
    " on our test computer" + (Array.isArray(r.failed) && r.failed.length ? " (" + r.failed.join(", ") + ")" : "") + ". The next version will fix it.";
  if (typeof r.run === "string" && /^https:\/\/github\.com\//.test(r.run)) el.href = r.run;
}
async function checkVersion() {
  if (document.hidden) return;
  try {
    const r = await fetch("speed.json?t=" + Date.now(), { cache: "no-store" });
    if (r.ok) markSpeed((await r.json())[VERSION.number]);
  } catch (_) { /* no speed.json yet, or offline: no mark */ }
  try {
    const r = await fetch("version.json?t=" + Date.now(), { cache: "no-store" });
    if (!r.ok) return;
    const v = await r.json();
    if (!v || typeof v.number !== "string" || !/^\d+(\.\d+)*$/.test(v.number) || !newerVersion(v.number, VERSION.number)) return;
    if (offered && !newerVersion(v.number, offered.number)) return;
    offered = v;
    const chip = document.getElementById("newer");
    chip.textContent = "🆕 " + v.number;
    chip.title = "Version " + v.number + (v.released ? ", out " + v.released : "") + ": click to change to it. Your friend, the place and the settings stay as they are.";
    chip.hidden = false;
    if (askNewOn()) askNew(v);   // (ticket 302)
  } catch (_) { /* offline, or no version.json here (a copy on this computer): nothing to offer */ }
}
// (ticket 302, M: a pop-up that says "a new version is available, would you like to switch?"; Matthew: off by default, a
// toggle in ⚙️ Advanced) This browser's own, as light or dark is (localStorage "dress-up-ask-new"), not the friend's. Once
// for each newer version offered (checkVersion offers each once): 🔄 switch loads it as the chip does; not now leaves the chip
const askNewOn = () => { try { return localStorage.getItem("dress-up-ask-new") === "on"; } catch (_) { return false; } };
function askNewSet(on) {
  try { if (on) localStorage.setItem("dress-up-ask-new", "on"); else localStorage.removeItem("dress-up-ask-new"); } catch (_) { /* (for this visit) */ }
  if (on && offered) askNew(offered); else if (!on) { const card = document.getElementById("ask-new"); if (card) card.remove(); }
}
function askNew(v) {
  let card = document.getElementById("ask-new");
  if (!card) { card = document.createElement("div"); card.id = "ask-new"; card.setAttribute("role", "dialog"); card.setAttribute("aria-label", "A new version"); document.body.append(card); }
  const words = document.createElement("p"), which = document.createElement("p"), row = document.createElement("div"), yes = document.createElement("button"), no = document.createElement("button");
  words.textContent = "🆕 A new version is available, would you like to switch?";
  which.className = "hint"; which.textContent = "version " + v.number + (v.released ? ", out " + v.released : "") + " · your friend, the place and the settings stay as they are";
  row.className = "row"; yes.textContent = "🔄 switch"; no.textContent = "not now";
  yes.addEventListener("click", switchToOffered); no.addEventListener("click", () => card.remove());
  row.append(yes, no); card.replaceChildren(words, which, row);
  yes.focus({ preventScroll: true });
}
// (the chip's click, and the pop-up's 🔄 switch: the new version, at an address of its own so no cache hands back the old one)
function switchToOffered() {
  if (!offered) return;
  persist();
  location.replace(location.pathname + "?v=" + encodeURIComponent(offered.number) + location.hash);
}
// (ticket 486, Matthew: "hardening MUDU against lack of internet on a tablet, desktop, or mobile phone") The game keeps a copy
// of itself on this device (sw.js), from any visit over https, so a game added to the home screen opens in airplane mode. Not
// on a copy served from this computer (localhost, the checks on GitHub), where a kept copy would hide a change, unless
// localStorage "dress-up-offline" is "on" (offline486 does that).
const OFFLINE_CACHE = "dress-up";
const offlineOn = () => { if (location.protocol === "https:") return true; try { return localStorage.getItem("dress-up-offline") === "on"; } catch (_) { return false; } };
const offlineCan = () => "serviceWorker" in navigator && typeof caches !== "undefined";
// (Chrome and Edge, on Android and computers, offer their own install: kept for the 📲 button in ⚙️ Advanced)
let offlinePrompt = null;
function offlineStart() {
  addEventListener("beforeinstallprompt", e => { e.preventDefault(); offlinePrompt = e; });
  if (!offlineCan() || !offlineOn()) return;
  navigator.serviceWorker.register("sw.js").catch(() => { /* (private windows, and browsers that keep nothing: the game as before) */ });
}
// How much of this version is kept here: the page, its stylesheet and every part, as the ⚙️ Advanced panel says.
async function offlineKept() {
  if (!offlineCan() || !offlineOn()) return { can: offlineCan(), on: false, kept: 0, total: 0 };
  const want = [new URL("./", location.href).href, ...[...document.querySelectorAll("script[src], link[rel=stylesheet][href]")].map(e => e.src || e.href)]
    .filter(u => new URL(u).origin === location.origin);
  try {
    const c = await caches.open(OFFLINE_CACHE);
    const kept = (await Promise.all(want.map(u => c.match(u)))).filter(Boolean).length;
    return { can: true, on: true, kept, total: want.length };
  } catch (_) { return { can: false, on: true, kept: 0, total: want.length }; }
}
function showVersion() {
  const el = document.getElementById("version");
  if (el) { el.textContent = VERSION.number + " · " + VERSION.released; el.title = "This is version " + VERSION.number + ", out " + VERSION.released + " (what changed: VERSIONS.md on GitHub)"; }
  const chip = document.getElementById("newer");
  if (chip) chip.addEventListener("click", switchToOffered);
  setTimeout(checkVersion, 5000);
  setInterval(checkVersion, 60000);
  document.addEventListener("visibilitychange", () => { if (!document.hidden) checkVersion(); });
}
