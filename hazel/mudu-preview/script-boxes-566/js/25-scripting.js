"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 482, Matthew: "an editor panel in MUDU, under a new control panel called 'Scripting' --- with a list of editors, one per
// room that needs logic, starting only with the dungeon in the castle. The editors are showing sub-programs of a large, global
// program with global vars, using the $GameUpdate [monad] ... The programs can use sub-routines defined in a common editor that
// they all have in scope in their subprograms. In terms of a full Hazel AST, each sub-editor (for each room/square in the world)
// could be a module named by that square's name. Names should be unique, and we should enforce that.") 📜 Scripting: the game's
// logic in Hazel, run by the game. The panel's editors are the bodies of the modules of one program: 📚 Common, then one module
// per room that has logic, named by the room (TrapRoom, for the castle's trap room over its dungeons). Before them, the program
// declares what a rule may say and what it is told:
//   Cmd     what to do, as data, each command with the rest after it: Flip("lever_L", then), Say("text", then), Done.
//           This is the $GameUpdate monad as data, for now: the game interprets it (scriptRun), and a Flip of a lever is the
//           game's own pull, its animation and its pits.
//   Event   what happened: Pull("L") or Pull("R"), a red lever pulled.
//   Globals the room's variables, as the game holds them: (lever_L = Bool, lever_R = Bool, wiring = String), a lever's true is up.
// A room's module gives on_event : (Globals, Event) -> Cmd. Hazel itself comes from a branch build's library (window.Hazel), loaded
// when the tab opens or a script first runs; until then, and whenever the program has errors or is not finished, the levers work as
// they always have. Off to start (state.scripts.on).
// (ticket 520, Matthew: "we aren't using tiles yet, right? Wasn't that the point of the Hazel-MUDU integration?") Hazel builds the
// program from the editors' texts itself (Hazel.runProgram, scriptSpec), and under the editors Hazel's own app, in a frame, shows the
// whole program as read-only tiles with Hazel's own error marks (scriptFrameMount).

// (tickets 482, 520) The Hazel build the game uses: its library (hazel-lib.js, window.Hazel) and its app in a frame (index.html#embed).
// For now the embed branch's build, hazelgrove/hazel#2719 (stacked on #2711), which has Hazel.runProgram and the frame; when #2719
// merges into #2711, this goes back to "https://hazel.org/build/js-library/", until the library ships with the game.
const SCRIPT_BUILD = "https://hazel.org/build/js-library-embed/";
const SCRIPT_LIB_URL = SCRIPT_BUILD + "hazel-lib.js";
// (the frame's messages are taken from this origin only, and posted to it only: SCRIPT_BUILD's)
const SCRIPT_HAZEL_ORIGIN = SCRIPT_BUILD.split("/").slice(0, 3).join("/");
// (ticket 578) the tiles from embed-editable (stacked on embed-selectable, on #2719's branch): the frame that takes edits; the
// library stays SCRIPT_BUILD's, the same Hazel otherwise
const SCRIPT_FRAME_URL = "https://hazel.org/build/embed-editable/index.html#embed";

// the rooms that have logic so far, each a module of the program; module names must be unique (scriptNames checks it)
const SCRIPT_ROOMS = [
  { name: "TrapRoom", label: "🏰 the trap room, over the castle's dungeons", kind: "trap" },
  { name: "TreeRoom", label: "🌳 the tree houses' trap doors, and the round room's at the top", kind: "tree" },
  { name: "Square", label: "🎚️ placed levers and trap doors, in any square or room", kind: "square" },
  { name: "World", label: "🗺️ the world's ways: which square a way out leads to", kind: "world" },
  { name: "Castle", label: "🏰 the castle's ways: where a pit drops you, and a dungeon's ladder leads", kind: "castle" },
];

// (ticket 566, Matthew: "Notice that in Languages of Play, the examples use sum types with typed constructions where we are using a
// lot of strings (names of directions and squares/rooms on map) --- Let's try to match the prescription of that paper, and use the
// sum types that each of these languages has already"; Languages of Play, Martens and Hammer, FDG 2017) The rules' vocabulary as
// sums, not text: a direction, a lever's side, a wiring, a castle room's way-out setting, a square's kind (Place) and a castle
// room's (Room), each from the game's own lists (PLACES, ROOM_KINDS), so they cannot drift from it, and a room's exits (Exit).
// Hazel declares them at the program's start: declared inside module Common, their constructors are in scope only in Common
// (js-library-embed's Hazel, 2026-10-23: a room's N is "Constructor is not defined"), which a module exporting its constructors
// would change. Fumola's variants (#N) need no declaring. A thing's uid and kind stay text: names, not kinds.
const SCRIPT_DIRS = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"];
// (in node, where the game's lists are not loaded: the same lists, held equal to them by tools/checks/scriptboxes566.mjs)
const SCRIPT_PLACES_KNOWN = ["none", "emptyroom", "bubblespace", "bubbleroom", "imagination", "forest", "underwater", "deepsea", "beach", "sea", "witch", "cemetery",
  "lake", "house", "farmyard", "treehouse", "treeroom", "treetop", "livingroom", "witchroom", "clearing", "healerroom", "fairyforest", "mountains", "waterfall",
  "campsite", "tent", "cave", "caveroom", "woodpath", "pasture", "crossing", "castle", "castleroom", "hut", "hutroom", "hutbed", "hutpotion", "hutcenter", "desert"];
const SCRIPT_ROOMS_KNOWN = ["king", "greenhouse", "trap", "dungeon1", "dungeon2", "hallway", "catroom", "stable", "dining", "kitchen", "smithy", "spiral"];
const SCRIPT_EXITS = ["side", "long", "spiral", "trap", "ladder", "all", "hall"];
const scriptCap = k => k[0].toUpperCase() + k.slice(1);
// (a square's kind "none" is NoPlace: None is the option's)
const scriptPlace = k => k === "none" ? "NoPlace" : scriptCap(String(k));
const scriptExit = x => "Exit" + scriptCap(String(x));
const scriptPlaces = () => typeof PLACES !== "undefined" ? PLACES.map(([k]) => k) : SCRIPT_PLACES_KNOWN;
const scriptRoomKinds = () => typeof ROOM_KINDS !== "undefined" ? Object.keys(ROOM_KINDS) : SCRIPT_ROOMS_KNOWN;
function SCRIPT_SUMS_TEXT() {
  const sum = (name, ks) => `type ${name} = ${ks.map(k => "+ " + k).join(" ")} in`;
  return [sum("Dir", SCRIPT_DIRS), sum("Side", ["L", "R"]), sum("Wiring", ["Normal", "Reversed", "Random"]), sum("Out", ["Auto", "OutOfCastle", "Nowhere", "ToRoom(Int)"]),
    sum("Place", scriptPlaces().map(scriptPlace)), sum("Room", scriptRoomKinds().map(scriptCap)), sum("Exit", SCRIPT_EXITS.map(scriptExit))].join("\n");
}
// The boundary: the game hands a rule its globals and event with text in them, as always, and takes back commands as always; here
// the text becomes constructors ({tag}, which Hazel's library and Fumola's literal writer take as a sum's value) on the way in,
// and back on the way out (Flip's side to the lever's name, GoSquare's direction to its text). Both engines' handlers are wrapped
// (scriptTyped), so the game, the checks and the tools keep passing what they pass.
const scriptTag = t => ({ tag: t });
function scriptTypedGlobals(g) {
  if (!g || typeof g !== "object") return g;
  const out = g.out === "auto" ? scriptTag("Auto") : g.out === "out" ? scriptTag("OutOfCastle") : g.out === "none" ? scriptTag("Nowhere") : { tag: "ToRoom", value: +g.out_room };
  const t = { ...g, wiring: scriptTag(scriptCap(String(g.wiring || "normal"))), rooms: (g.rooms || []).map(r => scriptTag(scriptCap(String(r)))),
    exits: (g.exits || []).map(x => scriptTag(scriptExit(x))), out,
    cells: (g.cells || []).map(c => ({ ...c, kind: scriptTag(scriptPlace(c.kind)), ways: (c.ways || []).map(scriptTag) })) };
  delete t.out_room;
  return t;
}
const scriptTypedEvent = ev => ev && (ev.tag === "Pull" || ev.tag === "PullBoulder" || ev.tag === "Way") && typeof ev.value === "string" ? { tag: ev.tag, value: scriptTag(ev.value) } : ev;
function scriptUntypedCmd(c) {
  if (!c || typeof c !== "object" || !Array.isArray(c.value)) return c;
  const v = c.value.slice(), k = v.pop();
  if (c.tag === "Flip" && v[0] && v[0].tag) v[0] = "lever_" + v[0].tag;
  if (c.tag === "GoSquare" && v[2] && v[2].tag) v[2] = v[2].tag;
  return { tag: c.tag, value: [...v, scriptUntypedCmd(k)] };
}
const scriptTyped = f => (g, ev) => scriptUntypedCmd(f(scriptTypedGlobals(g), scriptTypedEvent(ev)));

// (ticket 531) the whole game's vocabulary, for every room that has logic: what a rule may say (Cmd), what happened (Event), a placed
// thing as a rule sees it (Thing), and what it is told (Globals, built by each room's globals in SCRIPT_ACTS: add a field in both).
// No randomness and no clock in Hazel: the game's draws (draws, from a random wiring's seed) come in through Globals.
const SCRIPT_PREAMBLE = `${SCRIPT_SUMS_TEXT()}
type Cmd = + Done + Flip(Side, Cmd) + Say(String, Cmd) + SetPit(Int, Bool, Cmd) + DropBoulder(Int, Cmd) + LiftBoulder(Int, Cmd) + SetHatch(Int, Bool, Cmd) + OpenDoor(Int, Bool, Cmd) + DropRock(Int, Bool, Cmd) + Wait(Int, Cmd) + GoRoom(Int, Cmd) + GoSquare(Int, Int, Dir, Cmd) in
type Event = + Pull(Side) + PullBoulder(Side) + PullHatch(Int) + PullThing(Int) + FallPit(Int) + WayOut(Int) + Way(Dir) in
type Thing = (at = Int, uid = String, kind = String, on = Bool, works = [String], does = String, open = Bool, boulder = Bool) in
type Cell = (col = Int, row = Int, kind = Place, ways = [Dir]) in
type Globals = (lever_L = Bool, lever_R = Bool, wiring = Wiring, draws = [Float], pits_on = [Bool], covered = [Bool], hatch = Bool, things = [Thing], rooms = [Room], exits = [Exit], out = Out, at_col = Int, at_row = Int, cols = Int, rows = Int, cells = [Cell]) in`;

const SCRIPT_COMMON = `# Sub-routines every room can use, as Common.wire and so on. #
let flip2 = fun (a, b) : (Side, Side) -> Flip(a, Flip(b, Done));
# the i-th of a list of Bools, false past its end #
let nth = fun (xs, i) : ([Bool], Int) -> case xs | [] => false | x :: rest => if i == 0 then x else nth(rest, i - 1) end;
# which lever works each pit, the left and the right: normal, reversed, or at random by the game's two draws #
let wire = fun (mode, draws) : (Wiring, [Float]) -> case mode | Reversed => (R, L) | Random => (case draws | a :: b :: _ => (if a <. 0.5 then L else R, if b <. 0.5 then L else R) | _ => (L, R) end) | Normal => (L, R) end;
# a red lever up? (true is up) #
let up = fun (l, r, side) : (Bool, Bool, Side) -> case side | L => l | R => r end;
# each pit open: open when the lever working it is up #
let pits_open = fun (l, r, w) : (Bool, Bool, (Side, Side)) -> let (w0, w1) = w in (up(l, r, w0), up(l, r, w1));
# a pit that changed from was to now: SetPit, else nothing #
let pit_moves = fun (i, was, now, k) : (Int, Bool, Bool, Cmd) -> if was == now then k else SetPit(i, now, k);
let member = fun (s, xs) : (String, [String]) -> case xs | [] => false | x :: rest => x == s || member(s, rest) end;
# a way among a square's ways #
let has_way = fun (d, ds) : (Dir, [Dir]) -> case ds | [] => false | x :: rest => x == d || has_way(d, rest) end;
# where room kind k first is in a castle's rooms, counting from 1; 0 when it is not there #
let room_at = fun (rs, k) : ([Room], Room) -> case rs | [] => 0 | x :: rest => if x == k then 1 else (let i = room_at(rest, k) in if i == 0 then 0 else i + 1) end;
# the i-th room's exits, counting from 1; none past the end #
let exit_of = fun (xs, i) : ([Exit], Int) -> case xs | [] => None | x :: rest => if i == 1 then Some(x) else exit_of(rest, i - 1) end;
let rooms = fun rs : [Room] -> case rs | [] => 0 | _ :: rest => 1 + rooms(rest) end;
# the world's eight directions: each one's step across and down the map, and the way back #
let dir = fun d : Dir -> case d | N => (0, 0 - 1) | NE => (1, 0 - 1) | E => (1, 0) | SE => (1, 1) | S => (0, 1) | SW => (0 - 1, 1) | W => (0 - 1, 0) | NW => (0 - 1, 0 - 1) end;
let opp = fun d : Dir -> case d | N => S | NE => SW | E => W | SE => NW | S => N | SW => NE | W => E | NW => SE end`;

const SCRIPT_TRAP = `# A red lever pulled: flip it, and each pit wired to it opens or shuts. #
# A blue lever pulled: each of its pits gets a boulder, or loses one. #
let on_event = fun (g, e) : (Globals, Event) ->
  let w = Common.wire(g.wiring, g.draws) in
  let (w0, w1) = w in
  case e
  | Pull(side) =>
    let (was0, was1) = Common.pits_open(g.lever_L, g.lever_R, w) in
    let l = if side == L then !g.lever_L else g.lever_L in
    let r = if side == R then !g.lever_R else g.lever_R in
    let (now0, now1) = Common.pits_open(l, r, w) in
    Flip(side, Common.pit_moves(0, was0, now0, Common.pit_moves(1, was1, now1, Done)))
  | PullBoulder(side) =>
    let mine0 = w0 == side && Common.nth(g.pits_on, 0) in
    let mine1 = w1 == side && Common.nth(g.pits_on, 1) in
    let lift = (mine0 && Common.nth(g.covered, 0)) || (mine1 && Common.nth(g.covered, 1)) in
    let one = fun (i, mine, k) : (Int, Bool, Cmd) -> if !mine then k else if lift then LiftBoulder(i, k) else DropBoulder(i, k) in
    one(0, mine0, one(1, mine1, Done))
  | _ => Done
  end`;

// (ticket 531, Matthew: "let's do an experimental PR to dress-up where we try to export as much game logic as possible into the Hazel
// panels for each square (or common logic) --- and do the speedruns again to compare against plain JS") one build, both ways:
// ?logic=hazel runs every room's logic through the shipped texts (📜 on, whatever the friend saved), ?logic=js the plain JS (📜 off,
// whatever was saved); no switch, the panel's (state.scripts.on). Read once, at start.
const SCRIPT_TREE = `# A tree room's trap-door lever, or the round room's at the top (house 0): #
# the trap door shuts if open, opens if shut. #
let on_event = fun (g, e) : (Globals, Event) ->
  case e
  | PullHatch(n) => SetHatch(n, !g.hatch, Done)
  | _ => Done
  end`;

const SCRIPT_SQUARE = `# A placed lever pulled: each trap door wired to it swings, #
# or, a boulder lever, gets its rock or loses it. #
let on_event = fun (g, e) : (Globals, Event) ->
  case e
  | PullThing(i) =>
    let find = fun ts : [Thing] -> case ts | [] => None | t :: rest => if t.at == i then Some(t) else find(rest) end in
    let doors = fun (lever, ts) : (Thing, [Thing]) ->
      case ts
      | [] => Done
      | d :: rest =>
        let k = doors(lever, rest) in
        if !(d.kind == "trapdoor" && Common.member(d.uid, lever.works)) then k
        else if lever.does == "boulder" then (if d.boulder == !lever.on then k else DropRock(d.at, !lever.on, k))
        else OpenDoor(d.at, !d.open, k)
      end in
    (case find(g.things) | Some(t) => doors(t, g.things) | None => Done end)
  | _ => Done
  end`;

// (ticket 531, phase 2) the world's ways, decided when one is walked through: the square beside this one that way, if it is on the
// map, there, and has a way back (worldMeets and worldWays' rule, 25-world.js). A way linked to another world stays the JS's.
const SCRIPT_WORLD = `# Places with no way out on a side, whatever their ways say: the waterfall's N, its cliff. #
let no_way = fun (kind, d) : (Place, Dir) -> kind == Waterfall && d == N;
let find = fun (cs, c, r) : ([Cell], Int, Int) -> case cs | [] => None | x :: rest => if x.col == c && x.row == r then Some(x) else find(rest, c, r) end;
# A way out walked through: into the square beside it, coming in from the side facing back, #
# if that square is on the map and has a way back; else nowhere. #
let on_event = fun (g, e) : (Globals, Event) ->
  case e
  | Way(d) =>
    let (dx, dy) = Common.dir(d) in
    let c = g.at_col + dx in
    let r = g.at_row + dy in
    let back = Common.opp(d) in
    if c < 0 || r < 0 || c >= g.cols || r >= g.rows then Done
    else (case find(g.cells, c, r)
      | Some(x) => if Common.has_way(back, x.ways) && !no_way(x.kind, back) then GoSquare(c, r, back, Done) else Done
      | None => Done
      end)
  | _ => Done
  end`;

// (ticket 531, phase 2) the castle's ways, decided when they are taken (never per frame, never per drawing): which room a trap
// room's pit drops you into, and where a dungeon's ladder up leads (wayOutOf's rule, 13-castle-rooms-and-map.js). The rooms come in as
// their kinds, room 1 first, with each one's exits (ROOM_KINDS' exits), and this room's way out setting
const SCRIPT_CASTLE = `# Where room n's way out leads: 0 out of the castle, a room, or -1 nowhere. #
# out is its setting; Auto: a dungeon's ladder up to the trap room, room 1 out, any other to room 1. #
# Nowhere only where the way out is toward us (side doors, a long room's, a spiral's); elsewhere Auto. #
let way_out = fun (rooms, exits, n, out) : ([Room], [Exit], Int, Out) ->
  let ex = Common.exit_of(exits, n) in
  let toward_us = (case ex | Some(ExitSide) => true | Some(ExitLong) => true | Some(ExitSpiral) => true | _ => false end) in
  let ladder = (case ex | Some(ExitLadder) => true | _ => false end) in
  case (case out | Nowhere => (if toward_us then Nowhere else Auto) | o => o end)
  | Auto => (let t = Common.room_at(rooms, Trap) in if ladder && t > 0 && !(t == n) then t else if n == 1 then 0 else 1)
  | OutOfCastle => 0
  | Nowhere => 0 - 1
  | ToRoom(r) => if r >= 1 && r <= Common.rooms(rooms) then r else 0 - 1
  end;
# A trap room's pit fallen into (0 the left, 1 the right): #
# the first pit drops into the dungeon, the second into the deep dungeon; #
# a castle with only one of them, both into that one. #
let on_event = fun (g, e) : (Globals, Event) ->
  let d1 = Common.room_at(g.rooms, Dungeon1) in
  let d2 = Common.room_at(g.rooms, Dungeon2) in
  case e
  | FallPit(i) =>
    let to = if i == 0 then (if d1 > 0 then d1 else d2) else (if d2 > 0 then d2 else d1) in
    if to == 0 then Done else GoRoom(to, Done)
  | WayOut(n) => GoRoom(way_out(g.rooms, g.exits, n, g.out), Done)
  | _ => Done
  end`;

// each room kind's shipped text: rules that do just what the plain JS does, so 📜 on plays the same game (the fair comparison)
const SCRIPT_TEXTS = { trap: SCRIPT_TRAP, tree: SCRIPT_TREE, square: SCRIPT_SQUARE, world: SCRIPT_WORLD, castle: SCRIPT_CASTLE };
const SCRIPT_LOGIC = (() => { try { const v = new URLSearchParams(location.search || "").get("logic"); return ["hazel", "js", "fumola", "fumola-list"].includes(v) ? v : ""; } catch (e) { return ""; } })();
// (ticket 542) ?logic=fumola and ?logic=fumola-list: every room's logic through the same rules written in Fumola (25-fumola.js), run
// by Fumola's wasm build, whatever the panel or the save says; fumola holds the World rule's map in Fumola, fumola-list passes it in
const SCRIPT_FUMOLA = SCRIPT_LOGIC === "fumola" || SCRIPT_LOGIC === "fumola-list";
const SCRIPT_DEFAULTS = () => ({ common: SCRIPT_COMMON, rooms: Object.fromEntries(SCRIPT_ROOMS.map(r => [r.name, SCRIPT_TEXTS[r.kind] || ""])) });

// (ticket 566, Matthew: "each script box can be either: baked-in JS, or hazel code boxes, or fumola code boxes (3-way chip per code
// box) ... we default to builtin JS, but there's a \"master toggle\" chip in the game script panel, at the top, that switches the
// code boxes all"; "let's have the jS versions show up as JS code, but read only. The hazel and fumola boxes are for editing.")
// Which engine runs a room's rule: "js" (the game's own code), "hazel" or "fumola". A room's own chip (engines[room]) if it has
// one; else the master chip's (all); else, for a save from before, the old 📜 switch (on: Hazel). ?logic= overrides all of it.
const SCRIPT_ENGINES = [["js", "JS"], ["hazel", "Hazel"], ["fumola", "🍄 Fumola"]];
// (Matthew, 2026-10-23: "for Hazel, let's use something hexagonal instead of the puzzle piece ... or place into a hex shaped box,
// like a tile in hazel") Hazel's name drawn as one of its tiles: a box with pointed ends (style.css, .hazel-tile)
const scriptHazelTile = (text = "Hazel") => el("span", { class: "hazel-tile" }, text);
const scriptEngineLabel = (v, lab) => v === "hazel" ? scriptHazelTile(lab) : lab;
function scriptEngine(room) {
  if (SCRIPT_LOGIC === "hazel") return "hazel";
  if (SCRIPT_FUMOLA) return "fumola";
  if (SCRIPT_LOGIC === "js") return "js";
  const S = state.scripts, e = S && ((S.engines && S.engines[room]) || S.all);
  return e === "hazel" || e === "fumola" || e === "js" ? e : S && S.on ? "hazel" : "js";
}
const scriptsOn = () => SCRIPT_ROOMS.some(r => scriptEngine(r.name) !== "js");

// (ticket 566) the Fumola boxes' texts, as the Hazel boxes' are kept: 📚 Common and each room's module, from 25-fumola.js's
// shipped rules until edited (read there at run time, as that file comes after this one)
const SCRIPT_FUMOLA_DEFAULTS = () => typeof FUMOLA_COMMON === "undefined" ? { common: "", rooms: {} }
  : { common: FUMOLA_COMMON, rooms: { ...FUMOLA_ROOM_TEXTS } };

function scriptsState() {
  const S = state.scripts || (state.scripts = { on: false, common: null, rooms: {} });
  if (S.common == null) S.common = SCRIPT_COMMON;
  S.rooms = S.rooms || {};
  for (const r of SCRIPT_ROOMS) if (S.rooms[r.name] == null) S.rooms[r.name] = SCRIPT_TEXTS[r.kind] || "";
  S.engines = S.engines || {};
  const FD = SCRIPT_FUMOLA_DEFAULTS(), F = S.fumola || (S.fumola = { common: null, rooms: {} });
  if (F.common == null && FD.common) F.common = FD.common;
  F.rooms = F.rooms || {};
  for (const r of SCRIPT_ROOMS) if (F.rooms[r.name] == null && FD.rooms[r.name] != null) F.rooms[r.name] = FD.rooms[r.name];
  // (2026-10-23) a Hazel module whose text is the shipped one but for spaces and line breaks (broken narrow for tiles seen in a
  // small box, an hour's bug): the shipped one again, broken at ⚙️'s wrap
  const bare = t => String(t || "").replace(/\s+/g, "");
  const wrapAt = (S.editor && S.editor.wrap >= 20) ? S.editor.wrap : 77;
  if (S.common !== SCRIPT_COMMON && bare(S.common) === bare(SCRIPT_COMMON)) S.common = scriptFlow("hazel", SCRIPT_COMMON, wrapAt);
  for (const r of SCRIPT_ROOMS) { const d = SCRIPT_TEXTS[r.kind]; if (d && S.rooms[r.name] !== d && bare(S.rooms[r.name]) === bare(d)) S.rooms[r.name] = scriptFlow("hazel", d, wrapAt); }
  // (2026-10-23) a save holding the texts as shipped before their types, unchanged: the typed ones in their place (FUMOLA_UNTYPED)
  if (typeof FUMOLA_UNTYPED === "function") {
    if (FD.common && F.common !== FD.common && F.common === FUMOLA_UNTYPED(FD.common)) F.common = FD.common;
    for (const r of SCRIPT_ROOMS) { const d = FD.rooms[r.name]; if (d && F.rooms[r.name] !== d && F.rooms[r.name] === FUMOLA_UNTYPED(d)) F.rooms[r.name] = d; }
  }
  return S;
}

// (ticket 566) each room's own JS, as the game runs it: the source of its functions (Function.prototype.toString), never a copy
// that could drift from them. A name alone is the whole function; [name, from, to] the lines of a long one from the one holding
// from to the one holding to (the castle's pits are decided inside its scene)
const SCRIPT_JS = {
  TrapRoom: ["roomLeverPull", "roomLeverFlip", "roomBoulderPull"],
  TreeRoom: ["hatchLeverPull", "hatchSet"],
  Square: ["leverPull", "leverPullApply"],
  World: ["worldTarget", "worldMeets"],
  Castle: [["castleroomScene", "where a trap room's pits lead", "const pits ="], "wayOutOf", "scriptGo"],
};
function scriptJsSource(room) {
  return (SCRIPT_JS[room] || []).map(spec => {
    const [name, from, to] = Array.isArray(spec) ? spec : [spec];
    const f = typeof window !== "undefined" ? window[name] : undefined;
    if (typeof f !== "function") return `// ${name}: not in this page`;
    const src = Function.prototype.toString.call(f);
    if (!from) return src;
    const lines = src.split("\n"), a = lines.findIndex(l => l.includes(from)), b = lines.findIndex((l, i) => i >= a && l.includes(to));
    return a < 0 || b < 0 ? src : `// in ${name}(), the lines that decide it:\n` + lines.slice(a, b + 1).map(l => l.replace(/^ {2}/, "")).join("\n");
  }).join("\n\n");
}

// (Matthew: "Names should be unique, and we should enforce that") a module name from a room's or square's name: a capital, then
// letters and digits; a second of the same name gets a number
function scriptModuleName(label, taken) {
  let base = String(label).replace(/[^A-Za-z0-9]+/g, " ").trim().split(" ").filter(Boolean).map(w => w[0].toUpperCase() + w.slice(1)).join("") || "Room";
  if (!/^[A-Z]/.test(base)) base = "R" + base;
  let n = base, i = 2;
  while (taken.has(n) || n === "Common") n = base + i++;
  taken.add(n);
  return n;
}
const scriptNames = () => { const seen = new Set(); for (const r of SCRIPT_ROOMS) { if (seen.has(r.name) || r.name === "Common") return false; seen.add(r.name); } return true; };

// a module's body as written, without a last ";" (the program puts each in its braces; Hazel reads a member after a last ";" as a hole)
const scriptBody = t => String(t || "").replace(/;\s*$/, "").trim();

// (ticket 520) the program as Hazel.runProgram takes it, and as the frame shows it: the preamble, a module per editor (📚 Common, then
// each room's) with its text as written, and what the program gives: each room's on_event, held to the type a rule has (so a rule of
// another type is an error, and its room's), and the number of rooms (always two fields or more: a labeled tuple of one may not
// come back as an object). A blank room has no on_event: an error, its room's, so its levers work as before.
const scriptTail = () => "(" + [...SCRIPT_ROOMS.map(r => `${r.name.toLowerCase()} = (${r.name}.on_event : (Globals, Event) -> Cmd)`), `rooms = ${SCRIPT_ROOMS.length}`].join(", ") + ")";
function scriptSpec() {
  const S = SCRIPT_LOGIC === "hazel" ? SCRIPT_DEFAULTS() : scriptsState();
  // (Matthew, 2026-10-23: "auto line wrap doesn't work on hazel still?": the program's start, the types, went to the tiles as
  // written, a type to a line; now broken at ⚙️'s wrap, as the modules are: only spaces and breaks change)
  const wrapAt = (S.editor && S.editor.wrap >= 20) ? S.editor.wrap : 77;
  return { preamble: scriptFlow("hazel", SCRIPT_PREAMBLE, wrapAt),
    modules: [{ name: "Common", text: scriptBody(S.common) }, ...SCRIPT_ROOMS.map(r => ({ name: r.name, text: scriptBody(S.rooms[r.name]) }))],
    tail: scriptTail() };
}

// The program as one text, joined by the page: for a library without runProgram (an older build, or one cached from before)
function scriptProgram() {
  const S = SCRIPT_LOGIC === "hazel" ? SCRIPT_DEFAULTS() : scriptsState();
  return [SCRIPT_PREAMBLE,
    `module Common = {\n${scriptBody(S.common) || "let nothing = ()"}\n} in`,
    ...SCRIPT_ROOMS.map(r => `module ${r.name} = {\n${scriptBody(S.rooms[r.name]) || "let on_event = fun (g, e) : (Globals, Event) -> Done"}\n} in`),
    // (always two fields or more: a labeled tuple of one may not come back as an object)
    "(" + [...SCRIPT_ROOMS.map(r => `${r.name.toLowerCase()} = ${r.name}.on_event`), `rooms = ${SCRIPT_ROOMS.length}`].join(", ") + ")"].join("\n");
}

// (ticket 520) what Hazel says of a program of modules (runProgram's result, or the frame's status), in lines for the panel: each
// error with its module's name ("TrapRoom: ..."; the preamble's and the tail's, "the program: ..."), then what is not finished,
// each module's holes and its brackets or keywords without their partner; and whether it is ready to run (Hazel's ok, and nothing
// in those lines)
const scriptPlural = (n, one, many) => `${n} ${n === 1 ? one : many}`;
function scriptReport(r) {
  const errors = Array.from((r && r.errors) || []).map(e => e && typeof e === "object" ? `${e.module || "the program"}: ${e.message}` : String(e));
  const unfinished = [], put = (who, holes, open) => {
    const bits = [];
    if (holes > 0) bits.push(scriptPlural(holes, "hole to fill", "holes to fill"));
    if (open > 0) bits.push(scriptPlural(open, "bracket or keyword without its partner", "brackets or keywords without their partners"));
    if (bits.length) unfinished.push(`${who}: ${bits.join(", ")}`);
  };
  const H = (r && r.holes) || {}, I = (r && r.incomplete) || {};
  for (const m of new Set([...Object.keys(H), ...Object.keys(I)])) put(m, +H[m] || 0, +I[m] || 0);
  put("the program", +(r && r.programHoles) || 0, +(r && r.programIncomplete) || 0);
  return { errors, unfinished, ok: !!(r && r.ok === true) && !errors.length && !unfinished.length };
}

let hazelLoading = null;
// window.Hazel, loaded once
function hazelLoad() {
  if (window.Hazel) return Promise.resolve(window.Hazel);
  if (!hazelLoading) hazelLoading = new Promise((ok, no) => {
    const s = document.createElement("script");
    s.src = SCRIPT_LIB_URL; s.async = true;
    s.onload = () => window.Hazel ? ok(window.Hazel) : no(new Error("no window.Hazel"));
    s.onerror = () => { hazelLoading = null; no(new Error("Hazel did not load")); };
    document.head.append(s);
  });
  return hazelLoading;
}

// the program, analyzed and run once per text: its errors, what is not finished, whether it is ready, and its rooms' handlers. Through
// Hazel.runProgram (ticket 520: Hazel builds the program from the module texts) where the library has it; otherwise the text the page
// joins, through Hazel.run, ready when it has no errors (via says which)
let scriptCompiled = { key: null, via: "", errors: [], unfinished: [], ready: false, handlers: {}, ms: 0 };
function scriptCompile() {
  const H = window.Hazel;
  if (!H) return null;
  const viaProgram = typeof H.runProgram === "function", spec = viaProgram ? scriptSpec() : null;
  const key = viaProgram ? "program:" + JSON.stringify(spec) : "text:" + scriptProgram();
  if (scriptCompiled.key === key) return scriptCompiled;
  const t0 = performance.now();
  let errors = [], unfinished = [], ready = false, handlers = {};
  try {
    const r = viaProgram ? H.runProgram(spec) : H.run(key.slice(5));
    if (viaProgram) ({ errors, unfinished, ok: ready } = scriptReport(r));
    else { errors = Array.from(r.errors || []).map(String); ready = !errors.length; }
    const v = r.value || {};
    for (const room of SCRIPT_ROOMS) { const f = v[room.name.toLowerCase()]; if (typeof f === "function") handlers[room.name] = scriptTyped(f); }   // (ticket 566: text to sums and back)
    // (fine by Hazel, yet a room's rule not a function here: said, and not ready)
    if (ready) for (const room of SCRIPT_ROOMS) if (!handlers[room.name]) { errors.push(`${room.name}: its on_event did not come back as a function`); ready = false; }
  } catch (e) { errors = [String(e && e.message || e)]; ready = false; }
  scriptCompiled = { key, via: viaProgram ? "runProgram" : "run", errors, unfinished, ready, handlers, ms: performance.now() - t0 };
  hzPush(hzCounters().compileMs, scriptCompiled.ms);   // (ticket 531)
  return scriptCompiled;
}

// (ticket 568, Matthew 2026-10-23: "we extend the advanced 'measurement' tools with one specifically for measuring the script overhead,
// in each case") what each room's script costs, from the calls made while playing: per event, the whole event's ms in each engine
// (JS's own handler, Hazel, Fumola: from the guard to the last command acted on), median and worst, and Hazel's and Fumola's function
// alone (the event in, the commands out); each engine's set-up (its last compile); a frame being 1000/60 ms. For 🔬 the scripts' cost
// in ⚙️ Advanced, and for the speed runs (scriptCost(), JSON-ready)
const SCRIPT_TAG_ROOMS = { Pull: "TrapRoom", PullBoulder: "TrapRoom", PullHatch: "TreeRoom", PullThing: "Square", FallPit: "Castle", WayOut: "Castle", Way: "World" };
function scriptCost() {
  const Z = hzCounters(), F = Z.fn || { hazel: {}, fumola: {} };
  const stat = a => { if (!a || !a.length) return null; const s = [...a].sort((x, y) => x - y);
    return { n: s.length, median: +s[Math.floor(s.length / 2)].toFixed(3), worst: +s[s.length - 1].toFixed(3) }; };
  const tags = [...new Set([...Object.keys(Z.js || {}), ...Object.keys(Z.hazel || {}), ...Object.keys(Z.fumola || {})])]
    .sort((a, b) => Object.keys(SCRIPT_TAG_ROOMS).indexOf(a) - Object.keys(SCRIPT_TAG_ROOMS).indexOf(b));
  return {
    frameMs: +(1000 / 60).toFixed(2),
    compile: { hazel: scriptCompiled && scriptCompiled.ready ? +scriptCompiled.ms.toFixed(1) : null,
      fumola: typeof fumolaCompiled !== "undefined" && fumolaCompiled && fumolaCompiled.ready ? +fumolaCompiled.ms.toFixed(1) : null },
    rows: tags.map(tag => ({ tag, room: SCRIPT_TAG_ROOMS[tag] || null, js: stat(Z.js && Z.js[tag]), hazel: stat(Z.hazel && Z.hazel[tag]),
      fumola: stat(Z.fumola && Z.fumola[tag]), hazelFn: stat(F.hazel[tag]), fumolaFn: stat(F.fumola[tag]) })),
  };
}
function scriptCostClear() { const Z = hzCounters(); Z.js = {}; Z.hazel = {}; Z.fumola = {}; Z.fn = { hazel: {}, fumola: {} }; Z.ms = []; Z.runMs = []; Z.calls = 0; }

// a few words over the picture, for a script's Say
function scriptSay(text) {
  let b = document.getElementById("script-say");
  if (!b) { b = el("div", { id: "script-say", class: "script-say" }); document.body.append(b); }
  b.textContent = "📜 " + text; b.hidden = false;
  clearTimeout(scriptSay.t); scriptSay.t = setTimeout(() => { b.hidden = true; }, 3000);
}

// (ticket 531) the time Hazel and the plain JS each take per event, for the speed runs to read (tools/hazellogic531.mjs): calls, each
// handler call's ms (ms), each command walk's (runMs), each compile's (compileMs), the commands not understood (unknown), and per event
// tag the whole event's ms in each mode (hazel, js: from the guard to the last command acted on, the drawing after it not counted)
const HZ_KEEP = 5000;
// (ticket 542: under ?logic=fumola and fumola-list, the whole event's ms per tag under fumola, not hazel)
const hzCounters = () => window.__hz || (window.__hz = { calls: 0, ms: [], runMs: [], compileMs: [], unknown: 0, unknownTags: [], mismatch: 0, hazel: {}, fumola: {}, js: {} });
const hzPush = (a, v) => { if (a.length < HZ_KEEP) a.push(Math.round(v * 1000) / 1000); };
// (the plain JS's own handler, timed the same way: the whole event)
function hzJs(tag, f) { const t0 = performance.now(); try { return f(); } finally { const Z = hzCounters(); hzPush(Z.js[tag] || (Z.js[tag] = []), performance.now() - t0); } }

// the commands a rule gave, run in order, each by the room's act (its tag's method, with the command's fields before the rest);
// Wait(ms, rest) runs the rest later. A tag the room does not know ends the walk, counted (__hz.unknown) and said once.
// end() runs once, when the walk ends, however it ends (after a Wait too, so what the walk gathered stays with its own event)
function scriptRun(cmd, act, depth = 0, end = () => {}) {
  if (!cmd || depth > 200 || cmd.tag === "Done") return end();
  const v = Array.isArray(cmd.value) ? cmd.value : [cmd.value], k = v.length > 1 ? v[v.length - 1] : null, args = v.length > 1 ? v.slice(0, -1) : v;
  if (cmd.tag === "Say") { scriptSay(String(args[0])); return scriptRun(k, act, depth + 1, end); }
  if (cmd.tag === "Wait") { setTimeout(() => scriptRun(k, act, depth + 1, end), Math.max(0, +args[0] || 0)); return; }
  if (typeof act[cmd.tag] === "function" && act[cmd.tag](...args) !== false) return scriptRun(k, act, depth + 1, end);
  const Z = hzCounters(); Z.unknown++; if (Z.unknownTags.length < 20) Z.unknownTags.push(String(cmd.tag));
  if (Z.unknown === 1) scriptSay(`a command this room does not know: ${cmd.tag}`);
  end();
}

// (the empty fields of a room's globals that are not its own)
const scriptNoGlobals = () => ({ lever_L: false, lever_R: false, wiring: "normal", draws: [], pits_on: [], covered: [], hatch: false, things: [], rooms: [], exits: [], out: "auto", out_room: -1, at_col: 0, at_row: 0, cols: 0, rows: 0, cells: [] });
// (ticket 531) the event's room, its globals and how its commands act: per room kind (SCRIPT_ROOMS' kind), filled in as rules move
const SCRIPT_ACTS = {
  trap: {
    here: () => { const B = state.background; if (B.kind !== "castleroom") return false; const K = castleOf(B), n = Math.max(1, Math.min(K.rooms.length, Math.round(B.castleRoom ?? 1))); return K.rooms[n - 1] === "trap"; },
    globals: () => { const B = state.background, K = castleOf(B), n = Math.max(1, Math.min(K.rooms.length, Math.round(B.castleRoom ?? 1)));
      const T = trapState(B, n, K.id), r = seeded(B[leverSeedKey(n, K.id)] ?? 1);
      return { lever_L: !leverDown(B, "L", n, K.id), lever_R: !leverDown(B, "R", n, K.id), wiring: B[leverWireKey(n, K.id)] || "normal", draws: [r(0), r(1)],
        pits_on: T.on, covered: T.covered, hatch: false, things: [], rooms: [], exits: [], out: "auto", out_room: -1, at_col: 0, at_row: 0, cols: 0, rows: 0, cells: [] }; },
    // (a Flip moves the lever only; each SetPit swings its pit's lid, the rule having said which: ticket 531. A pit the rule says
    // otherwise than the drawing, which follows the levers, is counted in __hz.mismatch)
    // (each event its own gathered boulders, handed to done when its walk ends)
    act: () => { const got = { drop: [], lift: [] }; return { got, Flip: name => { if (name === "lever_L") roomLeverFlip("L", false); else if (name === "lever_R") roomLeverFlip("R", false); else return false; },
                  SetPit: (i, open) => { if (i !== 0 && i !== 1) return false; if (!roomPitSwing(i, !!open)) hzCounters().mismatch++; },
                  // (a blue lever: the boulders the rule names, gathered, then dropped or lifted together when the walk ends, as the
                  // JS's roomBoulderAct; none named, the lever still turns, down)
                  DropBoulder: i => { if (i !== 0 && i !== 1) return false; got.drop.push(i); },
                  LiftBoulder: i => { if (i !== 0 && i !== 1) return false; got.lift.push(i); } }; },
    done: (ev, a) => { if (ev.tag !== "PullBoulder") return; const { drop, lift } = a.got;
      roomBoulderAct(String(ev.value), lift.length ? lift : drop, lift.length > 0); },
  },
  // (a tree room's trap door, PullHatch(n), n its house, 0 the round room at the top; hatch: that door shut)
  tree: {
    here: ev => { const k = state.background.kind; return (k === "treeroom" || k === "treetop") && ev.value === hatchHouse(); },
    globals: () => ({ ...scriptNoGlobals(), hatch: !!state.background[hatchKey()] }),
    act: () => ({ SetHatch: (n, shut) => { if (n !== hatchHouse()) return false; hatchSet(!!shut); } }),
  },
  // (a placed lever, PullThing(i), i its place in the square's things; things: its levers and trap doors, each with its place (at).
  // Copied whole for each pull, so its cost grows with the square's levers and doors: the speed runs measure it)
  square: {
    here: ev => { const T = (state.background.things || [])[ev.value]; return !!T && T.kind === "lever"; },
    globals: () => ({ ...scriptNoGlobals(), things: (state.background.things || []).flatMap((t, at) => t.kind === "lever" || t.kind === "trapdoor"
      ? [{ at, uid: String(t.uid || ""), kind: t.kind, on: !!t.on, works: (t.works || []).map(String), does: String(t.does || ""), open: !!t.open, boulder: !!t.boulder }] : []) }),
    act: () => { const doors = []; return { doors, OpenDoor: (at, open) => { doors.push([at, "open", !!open]); }, DropRock: (at, down) => { doors.push([at, "boulder", !!down]); } }; },
    done: (ev, a) => { leverPullApply(ev.value, a.doors); },
  },
  // (ticket 531, phase 2: a world's way out walked through; scriptWay) Way(d), d one of WORLD_DIRS; cells: every square with its
  // place and its ways as set (a crossing's, its paths switched on), unfiltered: the rule knows the waterfall's cliff. Copied
  // whole for each way, so its cost grows with the map: the speed runs measure it. GoSquare(col, row, side): there, in from side
  world: {
    here: ev => { const W = state.world, cell = W && W.at && W.cells[W.at]; return !!cell && !!WORLD_DIRS[ev.value] && !(cell.links && cell.links[ev.value]); },
    globals: () => { const W = state.world, [ac, ar] = W.at.split(",").map(Number);
      return { ...scriptNoGlobals(), at_col: ac, at_row: ar, cols: W.cols, rows: W.rows, cells: Object.entries(W.cells).map(([k, cell]) => { const [col, row] = k.split(",").map(Number);
        return { col, row, kind: String(cell.kind), ways: (cell.kind === "crossing" ? CROSS_DIRS.map(([d]) => d).filter(d => cell["cross" + d]) : cell.ways || []).map(String) }; }) }; },
    act: () => { const got = { go: null }; return { got, GoSquare: (c, r, side) => { got.go = { key: Math.round(+c) + "," + Math.round(+r), side: String(side) }; } }; },
  },
  // (ticket 531, phase 2: a castle's ways, as they are taken; scriptGo) FallPit(i): a trap room's pit i fallen into; WayOut(n): a
  // dungeon's (a ladder room's) one way, its ladder up, taken from room n. GoRoom(r) says
  // where to: r a room, 0 out of the castle, below 0 nowhere; the walk is the JS's (enterScene), to that room
  castle: {
    here: ev => { const B = state.background; if (B.kind !== "castleroom") return false; const K = castleOf(B), n = castleRoomNow(B, K);
      return ev.tag === "FallPit" ? K.rooms[n - 1] === "trap" : ev.tag === "WayOut" && ev.value === n && ROOM_KINDS[K.rooms[n - 1]].exits === "ladder"; },
    globals: () => { const B = state.background, K = castleOf(B), n = castleRoomNow(B, K), out = K.hall[n - 1].out, r = Number(out);
      return { ...scriptNoGlobals(), rooms: K.rooms.slice(), exits: K.rooms.map(st => ROOM_KINDS[st].exits), out, out_room: Number.isInteger(r) ? r : -1 }; },
    act: () => { const got = { go: null }; return { got, GoRoom: r => { got.go = Math.round(+r); } }; },
  },
};
// (the castle room the friend is in, as the game reads it)
const castleRoomNow = (B, K = castleOf(B)) => Math.max(1, Math.min(K.rooms.length, Math.round(B.castleRoom ?? 1)));

// (ticket 531, phase 2) a world's way out walked through (worldWalk): with 📜 on and ready, the World rule says which square it leads
// to (worldTarget's answer, or null: nowhere); what worldTarget says is checked against it, a difference counted in __hz.mismatch.
// Otherwise, and for a way linked to another world, worldTarget, as always
function scriptWay(W, d) {
  if (!scriptEvent("World", { tag: "Way", value: d })) return hzJs("Way", () => worldTarget(W, W.at, d));
  if (!scriptEvent.act) return worldTarget(W, W.at, d);   // (the rule stopped: the JS's answer, not counted as a mismatch)
  const go = scriptEvent.act.got.go, t = go && W.cells[go.key] ? { W, key: go.key, cell: W.cells[go.key], link: false } : null;
  const js = worldTarget(W, W.at, d);
  if ((js ? js.key : null) !== (t ? t.key : null) || (go && go.side !== WORLD_OPP[d])) hzCounters().mismatch++;
  return t;
}

// (ticket 531, phase 2) a castle door or pit taken: with 📜 on and ready, the Castle rule says where it leads, and enterScene goes
// there ({to, room}; false: nowhere, so not at all); what the drawing put on the door (decided when it was drawn) is checked
// against it, a difference counted in __hz.mismatch. null: the JS's own way, the door's data-to and data-room, as always
function scriptGo(door) {
  const B = state.background, pit = door.dataset.pit;
  if (B.kind !== "castleroom" || (!pit && door.dataset.to !== "castle" && door.dataset.to !== "castleroom")) return null;
  if (!pit) { const K = castleOf(B); if (ROOM_KINDS[K.rooms[castleRoomNow(B, K) - 1]].exits !== "ladder") return null; }
  // (a pit, by its side: the left's middle is left of the room's; else a door, which in a dungeon is its ladder up)
  const ev = pit ? { tag: "FallPit", value: +pit.split(" ")[0] < 0 ? 0 : 1 } : { tag: "WayOut", value: castleRoomNow(B) };
  if (!scriptEvent("Castle", ev)) { hzJs(ev.tag, () => door.dataset.room); return null; }
  if (!scriptEvent.act) return null;   // (the rule stopped: the door's own way, not counted as a mismatch)
  const got = scriptEvent.act.got, r = got && got.go, go = r == null || r < 0 ? false : r === 0 ? { to: "castle", room: undefined } : { to: "castleroom", room: String(r) };
  if ((go ? go.to : null) !== door.dataset.to || (go ? go.room : undefined) !== door.dataset.room) hzCounters().mismatch++;
  return go;
}

// (ticket 531; ticket 482's scriptPull) an event in a room, with 📜 on and the program ready: the room's rule says what happens, its
// commands are acted on, and true. Otherwise false, and the caller's own JS runs as it always has: 📜 off, Hazel not loaded yet,
// not ready (errors, a hole to fill, a bracket left open), no handler for the room, or not in that room. (ticket 542) Under
// ?logic=fumola and fumola-list, the rule is Fumola's (fumolaCompile, 25-fumola.js), with the same handler shape: the rest is the same
function scriptEvent(room, ev) {
  scriptEvent.act = null;
  if (!scriptsOn()) return false;
  const R = SCRIPT_ROOMS.find(r => r.name === room), A = R && SCRIPT_ACTS[R.kind];
  if (!A || !A.here(ev)) return false;
  // (ticket 566: the room's own engine; JS, the caller's own code)
  const eng = scriptEngine(room), fum = eng === "fumola";
  if (eng === "js") return false;
  if (fum ? !fumolaReady() : !window.Hazel) { (fum ? fumolaLoad() : hazelLoad()).catch(() => {}); return false; }
  const t0 = performance.now(), Z = hzCounters();
  const C = fum ? fumolaCompile() : scriptCompile(), f = C && C.ready && C.handlers[room];
  if (!f) return false;
  let cmd;
  const t1 = performance.now();
  try { cmd = f(A.globals(ev), ev); } catch (e) { scriptSay("the rule stopped: " + (e && e.message || e)); Z.stopped = (Z.stopped || 0) + 1; return true; }
  const t2 = performance.now();
  const act = scriptEvent.act = A.act(ev);   // (kept, for a caller that reads what the walk gathered: scriptGo)
  scriptRun(cmd, act, 0, () => { if (A.done) A.done(ev, act); });
  const t3 = performance.now();
  const side = fum ? Z.fumola || (Z.fumola = {}) : Z.hazel;
  Z.calls++; hzPush(Z.ms, t2 - t1); hzPush(Z.runMs, t3 - t2); hzPush(side[ev.tag] || (side[ev.tag] = []), t3 - t0);
  { const F = Z.fn || (Z.fn = { hazel: {}, fumola: {} }), e = fum ? F.fumola : F.hazel; hzPush(e[ev.tag] || (e[ev.tag] = []), t2 - t1); }   // (ticket 568: the function alone, per engine)
  return true;
}

// (ticket 520) ⬢ Hazel's tiles: under the editors, Hazel's own app in a frame (SCRIPT_FRAME_URL, its embed mode; the messages are
// protocol 1 of docs/hazel-as-a-js-library.md on hazelgrove/hazel#2719's branch) showing the whole program as one read-only cell of
// tiles, with Hazel's own error marks. The page posts the program ({type: "set", ...scriptSpec()}) when the frame says "ready" and
// then on the panel's change timer, no more often (scriptFramePost); the frame posts back its height ("size") and what Hazel says
// of the program ("status"), each only when it changes, and "crash". Taken only from SCRIPT_HAZEL_ORIGIN, with the frame as source.
//   Lazy: made only with the 📜 tab open, after showTab's layout (boxLayout and tabDesigner move the panel's rows, and a frame moved
// in the page loads again), and gone with the tab (showTab empties the panel: the frame's page goes with it), so the next visit
// loads it again. While the game is full screen the panel is visibility: hidden, which keeps a frame laid out and, in some
// browsers, drawing every animation frame; so there the frame is display: none (style.css), which browsers throttle (the 60
// frames a second rule). A frame first laid out undisplayed measures Hazel's font as nothing (the hidden-pane lesson), so one is
// never made while the panel is hidden; Hazel measures its font again when it is shown.
//   A frame that has not said "ready" in SCRIPT_FRAME_WAIT, that says "crash" before it, or that speaks another protocol, is taken
// away, its words in its place and a button to start it again: the text boxes work all along. The wait counts only the time the
// frame can draw (scriptFrameCanDraw: the page shown, the game not full screen, the frame on screen), as Hazel says "ready" only
// after its first display, which needs the frame's animation frames, and browsers stop those in a frame out of view, undisplayed
// or in a hidden tab; so does the ten seconds before "no program" is said. A "crash" after "ready" is said by the tiles and they
// are kept (Hazel keeps its last model and goes on), unless SCRIPT_FRAME_CRASHES come within a minute. Its height is the one
// posted, at most SCRIPT_FRAME_MAX; its box scrolls past a screenful (style.css).
//   The keys: a click in the frame gives it the keyboard, as any frame takes it; the tiles are only for looking, and the game's keys
// (B, F, Enter, Escape) are wanted back, so a frame given the focus gives it back (scriptFrameBlur).
const SCRIPT_FRAME_WAIT = 30000, SCRIPT_FRAME_QUIET = 10000, SCRIPT_FRAME_TICK = 1000;
const SCRIPT_FRAME_MAX = 4000, SCRIPT_FRAME_CRASHES = 3;
// the frame now: { iframe, holder, phase ("loading", "ready", "late", "crashed"), made, readyMs, build, sent, status, height, why (the
// words it was taken away with, or "quiet": ready, yet no status SCRIPT_FRAME_QUIET on), crashes (the times of its crashes after ready)
// and oops (the last one's words), blurs, inView (false while off screen), waited and quiet (the ms it could draw while loading, and
// since ready), tick (the timer counting them) and seen (its IntersectionObserver) }
let scriptFrame = null, scriptFrameListening = false;
const scriptFrameHidden = () => document.hidden || document.body.classList.contains("big");
const scriptFrameCanDraw = F => !scriptFrameHidden() && F.inView !== false;
// (once a second while loading, or ready with no status yet: the time the frame could draw, counted; nothing after that)
function scriptFrameTick(F) {
  clearTimeout(F.tick);
  F.tick = setTimeout(() => {
    if (scriptFrame !== F || !F.iframe.isConnected) { scriptFrameEnd(F); return; }
    const can = scriptFrameCanDraw(F);
    if (F.phase === "loading") {
      if (can && (F.waited += SCRIPT_FRAME_TICK) >= SCRIPT_FRAME_WAIT) { scriptFrameStop(F, "late"); return; }
    } else if (F.phase === "ready" && !F.status) {
      if (can && (F.quiet += SCRIPT_FRAME_TICK) >= SCRIPT_FRAME_QUIET) { F.why = "quiet"; scriptFrameShow(F); return; }
    } else return;
    scriptFrameTick(F);
  }, SCRIPT_FRAME_TICK);
}
function scriptFrameEnd(F) { clearTimeout(F.tick); if (F.seen) { F.seen.disconnect(); F.seen = null; } }
function scriptFrameMount(holder) {
  if (!holder || !holder.isConnected || (scriptFrame && scriptFrame.holder === holder && scriptFrame.iframe.isConnected)) return;
  // (not while the panel is hidden: later, if the tab is still open then)
  if (scriptFrameHidden()) { setTimeout(() => scriptFrameMount(holder), 1000); return; }
  if (scriptFrame) scriptFrameEnd(scriptFrame);
  if (!scriptFrameListening) {
    scriptFrameListening = true;
    window.addEventListener("message", scriptFrameMessage);
    window.addEventListener("blur", scriptFrameBlur);
  }
  const wrap = holder.querySelector(".script-frame-wrap");
  const iframe = el("iframe", { class: "script-frame", src: SCRIPT_FRAME_URL, tabindex: "-1",
    title: SCRIPT_TILES_EDIT ? "Hazel's tiles: the whole program, to edit, with Hazel's own marks where something is wrong" : "Hazel's tiles: the whole program, read only, with Hazel's own marks where something is wrong" });
  const F = scriptFrame = { iframe, holder, phase: "loading", made: performance.now(), readyMs: null, build: null, sent: null, status: null, height: 0, why: "",
    crashes: [], oops: "", blurs: 0, inView: undefined, waited: 0, quiet: 0, tick: 0, seen: null };
  wrap.replaceChildren(iframe);
  if (typeof IntersectionObserver === "function") { F.seen = new IntersectionObserver(es => { for (const e of es) F.inView = e.isIntersecting; }); F.seen.observe(iframe); }
  scriptFrameTick(F);
  scriptFrameShow(F);
}
function scriptFrameStop(F, phase, why = "") {
  scriptFrameEnd(F);
  F.phase = phase; F.why = why;
  F.iframe.remove();
  scriptFrameShow(F);
}
function scriptFrameMessage(e) {
  const F = scriptFrame;
  if (!F || e.origin !== SCRIPT_HAZEL_ORIGIN || !F.iframe.contentWindow || e.source !== F.iframe.contentWindow) return;
  const m = e.data || {};
  if (m.type === "ready") {
    // (a build that speaks other messages, as a branch build may after a push: its fields would be misread, so it is put away)
    if (m.protocol !== 1) { scriptFrameStop(F, "crashed", `they speak protocol ${m.protocol}, and this game knows protocol 1`); return; }
    if (F.phase === "loading") { F.phase = "ready"; F.readyMs = performance.now() - F.made; }
    F.build = m.build || null; F.sent = null;
    if (SCRIPT_TILES_EDIT) F.iframe.contentWindow.postMessage({ type: "editable", on: true }, SCRIPT_HAZEL_ORIGIN);
    { const dark = scriptPageDark(); F.iframe.contentWindow.postMessage({ type: "theme", dark, contrast: dark }, SCRIPT_HAZEL_ORIGIN); }   // (578: the page's theme)
    scriptTileFont({ ready: true, iframe: F.iframe });   // (587: ⚙️'s font)
    scriptFramePost();
    // (a frame that takes no program says nothing back: it takes programs only from the pages it knows, EmbedProtocol.re's
    // site_origins and dev_hosts, so a copy of the game served from elsewhere, or from disk, has no tiles; said, after a while)
    F.quiet = 0; if (!F.status) scriptFrameTick(F);
  } else if (m.type === "size") {
    const h = Math.max(24, Math.min(SCRIPT_FRAME_MAX, Math.ceil(+m.height || 0)));
    if (h !== F.height) { F.height = h; F.iframe.style.height = h + "px"; }
    return;
  } else if (m.type === "status") F.status = m;
  else if (m.type === "edit") { scriptTilesEdit(F, m.text); return; }
  else if (m.type === "caret") { scriptCaretTo(F.iframe, +m.top || 0); return; }
  else if (m.type === "goto") { scriptGoto(String(m.name || "")); return; }
  else if (m.type === "crash") {
    const why = String(m.message || ""), t = performance.now();
    // (before ready, or again and again: taken away; otherwise said, and the tiles kept, as Hazel keeps its last program and goes on)
    F.crashes = F.crashes.filter(x => t - x < 60000); F.crashes.push(t);
    if (F.phase !== "ready" || F.crashes.length >= SCRIPT_FRAME_CRASHES) { scriptFrameStop(F, "crashed", why); return; }
    F.oops = why || "an error";
  }
  else return;
  scriptFrameShow(F);
}
// (ticket 578, Matthew: "a real tile editing experience is top priority now") the tiles take edits: the page asks for an editable
// cell ({type: "editable", on: true}, embed-editable's protocol), and after each edit the frame posts the program's text, as Hazel
// lays it out ({type: "edit", text, edits}). Its modules are found in it (scriptTilesModules) and written to the Hazel boxes, and
// that program is noted as the frame's own, so it is not posted back (which would set it again, and lose the caret)
const SCRIPT_TILES_EDIT = true;
// each module's body in a program's text: module Name = { ... } in, the braces matched outside strings and # comments #; the
// body's common indent taken off. null when a module's braces do not match
function scriptTilesModules(text) {
  const out = {}, re = /(^|\n)\s*module\s+([A-Za-z_]\w*)\s*=\s*\{/g;
  let m;
  while ((m = re.exec(text))) {
    let i = re.lastIndex, depth = 1;
    for (; i < text.length && depth > 0; i++) {
      const c = text[i];
      if (c === '"') { for (i++; i < text.length && text[i] !== '"' && text[i] !== "\n"; i++) if (text[i] === "\\") i++; }
      else if (c === "#") { for (i++; i < text.length && text[i] !== "#" && text[i] !== "\n"; i++); }
      else if (c === "{") depth++;
      else if (c === "}") depth--;
    }
    if (depth !== 0) return null;
    const lines = text.slice(re.lastIndex, i - 1).replace(/^\s*\n/, "").replace(/\s+$/, "").split("\n");
    const ind = Math.min(...lines.filter(l => l.trim()).map(l => l.match(/^ */)[0].length));
    out[m[2]] = lines.map(l => l.slice(Number.isFinite(ind) ? ind : 0)).join("\n");
    re.lastIndex = i;
  }
  return out;
}
let scriptTilesEdited = null;   // (the panel's hook: boxes filled again, the program saved and compiled; scriptingPanel sets it)
function scriptTilesEdit(F, text) {
  const mods = scriptTilesModules(String(text || ""));
  if (!mods) return;
  const S = scriptsState();
  let any = false;
  if (mods.Common != null && mods.Common !== S.common) { S.common = mods.Common; any = true; }
  for (const r of SCRIPT_ROOMS) if (mods[r.name] != null && mods[r.name] !== S.rooms[r.name]) { S.rooms[r.name] = mods[r.name]; any = true; }
  F.sent = JSON.stringify(F.mod ? scriptTileSpec(F) : scriptSpec());   // (the frame has this one: not posted back, so its caret stays)
  if (any && scriptTilesEdited) scriptTilesEdited();
}
// (ticket 578, Matthew 2026-10-23: "each Hazel box (Common and each room) shows Hazel's tiles in place of its text box, with a chip to
// switch back to text") a box's own tiles: a frame per Hazel box, told its module ({type: "focus", module}: embed-editable shows that
// module's body alone, read in the program's context), that it may edit, and the page's theme; given the whole program as the big
// frame is, on the same timer. Made when its box first comes into view, so six Hazels do not start at once. An edit writes that
// module's text (the frame posts the body), the program is noted as the frame's own (not posted back to it: the caret stays), and
// the other frames get it, keeping their carets (their bodies did not change).
//   The theme (Matthew: "dark mode, high contrast, if the ambient page has the setting 'dark'. I want that default in general
// everywhere"): dark and high contrast while the page is dark (its theme switch says dark, or auto with the system dark), light
// otherwise; sent again when it changes.
const scriptPageDark = () => { const t = document.documentElement.getAttribute("data-theme"); return t === "dark" || (t !== "light" && !!(window.matchMedia && matchMedia("(prefers-color-scheme: dark)").matches)); };
const scriptTileFrames = new Set();
let scriptTilesListening = false;
function scriptTileTheme(T) {
  if (!T.ready || !T.iframe || !T.iframe.contentWindow) return;
  const dark = scriptPageDark();
  T.iframe.contentWindow.postMessage({ type: "theme", dark, contrast: dark }, SCRIPT_HAZEL_ORIGIN);
}
function scriptTileFrame(mod) {
  // (never empty: an empty frame-wrap is display: none, and so never comes into view, and its frame would never be made)
  const holder = el("div", { class: "script-frame-wrap script-tile-wrap" }, [el("div", { class: "hint script-tile-wait" }, `⏳ Hazel's tiles for ${mod}…`)]);
  const T = { mod, holder, iframe: null, ready: false, sent: null, height: 0 };
  if (!scriptTilesListening) {
    scriptTilesListening = true;
    window.addEventListener("message", scriptTileMessage);
    const again = () => { for (const F of scriptTileFrames) scriptTileTheme(F); };
    new MutationObserver(again).observe(document.documentElement, { attributes: true, attributeFilter: ["data-theme"] });
    if (window.matchMedia) matchMedia("(prefers-color-scheme: dark)").addEventListener("change", again);
  }
  const mount = () => {
    if (T.iframe || !holder.isConnected) return;
    T.iframe = el("iframe", { class: "script-frame script-tile-frame", src: SCRIPT_FRAME_URL, tabindex: "-1", title: `Hazel's tiles: module ${mod}, to edit` });
    holder.replaceChildren(T.iframe);
    scriptTileFrames.add(T);
  };
  // (in view: made; a box moved, as into the ⛶ view, loads its frame again, and says ready again. Looked for every 0.7 s: an
  // IntersectionObserver made while the panel was being laid out never called back, in a box later moved into the ⛶ view)
  // (preloaded, Matthew 2026-10-23: "let's pre-load the tile editor for each code block on the LHS, even if not in view"; then "let's
  // give the first editor a head start if it's not done in 2 sec, 3 sec, 4 sec, etc --- and then when another one becomes visible
  // -- can we prioritize it somehow?") queued, in the panel's order, and made one at a time (scriptTilesPump)
  T.mount = mount;
  scriptTileQueue.push(T);
  setTimeout(scriptTilesPump, 0);
  T.post = () => {
    if (!T.ready || !T.iframe || !T.iframe.isConnected) return;
    const spec = scriptTileSpec(T), key = JSON.stringify(spec);
    if (key === T.sent) return;
    T.sent = key;
    T.iframe.contentWindow.postMessage({ type: "set", ...spec }, SCRIPT_HAZEL_ORIGIN);
  };
  return T;
}
// (Matthew, 2026-10-24: "is the hazel editor respecting the edge of the code window (wrapping) or the line-col limit we set of 77? ...
// as I change that limit, it does not respond. Does not respond to window size changes either.") A box's tiles get its program with
// each line broken to fit (579's flow: only spaces and breaks change) at the narrower of ⚙️'s wrap and the box's own width in Hazel's
// characters, less the line numbers' gutter; a copy for the tiles, not written into the stored text, so a wider box or a bigger wrap
// gets the long lines back. Sent again when ⚙️ changes (scriptEditorApply) and when the window or the ⛶ view's divider changes the
// box's width. A frame being typed in keeps its text and caret until then (its edit is noted as sent: scriptTilesEdit)
function scriptTileCols(T) {
  const E = scriptEditorSettings(), w = T.holder ? T.holder.clientWidth : 0;
  const ch = 0.71 * E.size;   // (Hazel's tiles: about 12.8 px a character at 18 px, measured in its frame, 2026-10-23)
  const fit = w > 0 ? Math.floor(w / ch) - (E.lines ? 5 : 1) - 2 : Infinity;
  return Math.max(20, Math.min(E.wrap, fit));
}
function scriptTileSpec(T) {
  const spec = scriptSpec(), cols = scriptTileCols(T);
  return { ...spec, preamble: scriptFlow("hazel", spec.preamble, cols), modules: spec.modules.map(m => ({ ...m, text: scriptFlow("hazel", m.text, cols) })) };
}
var scriptTilesReflowT = null;   // (var: scriptEditorApply may run before this line)
function scriptTilesReflowSoon() { clearTimeout(scriptTilesReflowT); scriptTilesReflowT = setTimeout(scriptTilesPost, 300); }
window.addEventListener("resize", scriptTilesReflowSoon);

// The queue of tiles not made yet, and the pump that makes them: the first at once; each next one when the one before says ready, or
// after 2 s, then 3 s, 4 s and so on, whichever comes first (a slow one does not hold up the rest for long). One in view jumps the
// queue: made at once, looked for every 0.4 s, as is the box opened at max size (scriptTilesFirst)
const scriptTileQueue = [];
let scriptTilesMade = 0, scriptTilesWait = null, scriptTilesWaiting = null, scriptTilesLook = null;
const scriptTileShown = T => { const r = T.holder.getBoundingClientRect(); return r.width > 0 && r.bottom > 0 && r.top < window.innerHeight && r.right > 0 && r.left < window.innerWidth; };
function scriptTilesMake(T) {
  const i = scriptTileQueue.indexOf(T); if (i >= 0) scriptTileQueue.splice(i, 1);
  if (T.iframe || !T.holder.isConnected) return;
  T.mount(); scriptTilesMade++;
  scriptTilesWaiting = T;
  clearTimeout(scriptTilesWait);
  scriptTilesWait = setTimeout(() => { scriptTilesWaiting = null; scriptTilesPump(); }, (scriptTilesMade + 1) * 1000);
}
function scriptTilesPump() {
  for (let i = scriptTileQueue.length - 1; i >= 0; i--) { const T = scriptTileQueue[i]; if (T.iframe || (!T.holder.isConnected && !document.body.contains(T.holder))) scriptTileQueue.splice(i, 1); }
  // (one in view first, whatever is loading)
  const shown = scriptTileQueue.find(T => T.holder.getBoundingClientRect().width > 0 && scriptTileShown(T));
  if (shown) scriptTilesMake(shown);
  if (!scriptTilesWaiting || scriptTilesWaiting.ready) {
    const next = scriptTileQueue.find(T => T.holder.getBoundingClientRect().width > 0);
    if (next) scriptTilesMake(next);
  }
  if (scriptTileQueue.length && !scriptTilesLook) scriptTilesLook = setInterval(() => { if (!scriptTileQueue.length) { clearInterval(scriptTilesLook); scriptTilesLook = null; return; } if (scriptTileQueue.some(scriptTileShown)) scriptTilesPump(); }, 400);
}
// (a box opened at max size: its tiles made now, if they are still waiting)
function scriptTilesFirst(box) { for (const T of [...scriptTileQueue]) if (box.contains(T.holder)) scriptTilesMake(T); }
// (^ticket(587)) a name revealed in a box's tiles (mod: Common or a room's name; "types": the whole program's tiles, whose start
// has the types): that box shown (pinned, at max size), its frame told {type: "reveal", name} now, or when it is ready
function scriptReveal(mod, name) {
  const M = scriptMax;
  let box = null, send = null;
  if (mod === "types") {
    box = document.querySelector(".script-box.script-tiles");
    // (a frame still starting: asked again until it is ready and has its program (its first status), for 20 s, not dropped)
    const t0 = performance.now();
    send = () => { const F = scriptFrame; if (F && F.phase === "ready" && F.status && F.iframe.contentWindow) { F.revealing = true; F.iframe.contentWindow.postMessage({ type: "reveal", name }, SCRIPT_HAZEL_ORIGIN); return true; }
      if (performance.now() - t0 < 20000) setTimeout(send, 250); return false; };
  } else {
    box = document.querySelector(`.script-box[data-room="${mod}"]`);
    const T = [...scriptTileFrames, ...scriptTileQueue].find(T => T.mod === mod && box && box.contains(T.holder));
    if (T && !T.iframe) scriptTilesMake(T);
    send = () => { if (T && T.ready && T.iframe && T.iframe.contentWindow) { T.iframe.contentWindow.postMessage({ type: "reveal", name }, SCRIPT_HAZEL_ORIGIN); return true; } if (T) T.reveal = name; return true; };
  }
  if (!box) return;
  if (M && M.sec !== box) scriptMaxOpen(M.boxes, box);
  else if (!M) box.scrollIntoView({ block: "start" });
  send();
}
// (^ticket(586), Matthew: "goto def does not work") a goto definition a frame could not answer in its own cell: the name found in the
// program (a dotted Module.name; a type, or a constructor of one, at the program's start; else Common's, else a room's) and revealed
function scriptGoto(name) {
  const S = scriptsState(), parts = String(name).split(".");
  if (parts.length > 1) { scriptReveal(parts[0], parts[parts.length - 1]); return; }
  const pre = SCRIPT_PREAMBLE.split("\n");
  if (pre.some(l => new RegExp("^type\\s+" + name + "\\b").test(l))) { scriptReveal("types", name); return; }
  const owner = pre.find(l => /^type\s+\w+\s*=/.test(l) && new RegExp("\\+\\s*" + name + "\\b").test(l));
  if (owner) { scriptReveal("types", owner.match(/^type\s+(\w+)/)[1]); return; }
  const defines = text => scriptOutline(String(text || ""), "hazel").some(o => o.name === name);
  if (defines(S.common)) { scriptReveal("Common", name); return; }
  const room = SCRIPT_ROOMS.find(r => defines(S.rooms[r.name]));
  if (room) scriptReveal(room.name, name);
}
// a frame's caret, after a reveal: the view holding the frame scrolled to it
function scriptCaretTo(iframe, top) {
  // (from the frame up: the whole program's frame scrolls in its own wrap, inside the pinned box; begun at the box, the wrap was
  // never found and a goto to Globals stayed at the top (Matthew, 2026-10-23))
  let view = iframe.parentElement;
  while (view && view !== document.body && !(view.scrollHeight > view.clientHeight + 4 && /auto|scroll/.test(getComputedStyle(view).overflowY))) view = view.parentElement;
  if (!view || view === document.body) { window.scrollBy(0, iframe.getBoundingClientRect().top + top - 80); return; }
  view.scrollTop += iframe.getBoundingClientRect().top + top - view.getBoundingClientRect().top - 80;
}
// (^ticket(587), Matthew 2026-10-23: "font size chips don't affect hazel tile editor yet?") ⚙️'s size and font, sent to a box's tiles
// ({type: "font", size, family}: Hazel's own code font set, and measured again), at start and on change
function scriptTileFont(T) {
  if (!T.ready || !T.iframe || !T.iframe.contentWindow) return;
  const E = scriptEditorSettings(), fam = (SCRIPT_FONTS.find(([n]) => n === E.font) || [])[1];
  T.iframe.contentWindow.postMessage({ type: "font", size: E.size, family: fam }, SCRIPT_HAZEL_ORIGIN);
}
function scriptTilesPost() { for (const T of [...scriptTileFrames]) { if (!T.iframe || !T.iframe.isConnected) { scriptTileFrames.delete(T); continue; } T.post(); } }
function scriptTileMessage(e) {
  if (e.origin !== SCRIPT_HAZEL_ORIGIN) return;
  const T = [...scriptTileFrames].find(T => T.iframe && T.iframe.contentWindow === e.source);
  if (!T) return;
  const m = e.data || {};
  if (m.type === "ready") {
    T.ready = true; T.sent = null; T.holder.dataset.ready = "1"; scriptMaxNavSoon();
    if (scriptTilesWaiting === T) { scriptTilesWaiting = null; setTimeout(scriptTilesPump, 0); }   // (the next one, now)
    const w = T.iframe.contentWindow;
    // (the module's text, broken to fit, is sent by T.post: scriptTileSpec, not written into the stored text)
    scriptTileTheme(T);
    w.postMessage({ type: "editable", on: true }, SCRIPT_HAZEL_ORIGIN);
    w.postMessage({ type: "lines", on: scriptEditorSettings().lines }, SCRIPT_HAZEL_ORIGIN);   // (Hazel's own line numbers, as ⚙️ says)
    scriptTileFont(T);   // (587: ⚙️'s size and font, Hazel's too)
    w.postMessage({ type: "focus", module: T.mod }, SCRIPT_HAZEL_ORIGIN);
    T.post();
    if (T.reveal) { const n = T.reveal; T.reveal = null; setTimeout(() => w.postMessage({ type: "reveal", name: n }, SCRIPT_HAZEL_ORIGIN), 300); }
  } else if (m.type === "size") {
    const h = Math.max(24, Math.min(SCRIPT_FRAME_MAX, Math.ceil(+m.height || 0)));
    if (h !== T.height) { T.height = h; T.iframe.style.height = h + "px"; }
  } else if (m.type === "caret") { scriptCaretTo(T.iframe, +m.top || 0);
  } else if (m.type === "goto") { scriptGoto(String(m.name || ""));
  } else if (m.type === "edit") {
    const S = scriptsState(), text = String(m.text || "");
    if (T.mod === "Common") S.common = text; else if (SCRIPT_ROOMS.some(r => r.name === T.mod)) S.rooms[T.mod] = text;
    T.sent = JSON.stringify(scriptSpec());   // (this frame has it: not posted back)
    if (scriptTilesEdited) scriptTilesEdited();
  }
}

// the program to the frame, when it is ready and the program is not the one it has
function scriptFramePost() {
  const F = scriptFrame;
  if (!F || F.phase !== "ready" || !F.iframe.isConnected || !F.iframe.contentWindow) return;
  const spec = scriptSpec(), key = JSON.stringify(spec);
  if (key === F.sent) return;
  F.sent = key;
  F.iframe.contentWindow.postMessage({ type: "set", ...spec }, SCRIPT_HAZEL_ORIGIN);
}
// (the page's window losing the focus: to the frame, after a click in it; the click done, the focus back to the page)
function scriptFrameBlur() {
  setTimeout(() => {
    const F = scriptFrame;
    if (!F || document.activeElement !== F.iframe || SCRIPT_TILES_EDIT) return;   // (578: tiles that take edits keep the keys)
    F.blurs++; F.iframe.blur(); window.focus();
  }, 200);
}
// the frame's state, in words, by it: starting, what Hazel says of the program (fine, or each module's errors and holes), or why it
// is gone, with a button to start it again
function scriptFrameShow(F) {
  const line = F.holder.querySelector(".script-frame-status"), list = F.holder.querySelector(".script-frame-errors");
  if (!line || !list) return;
  let words = "", items = [], again = false;
  if (F.phase === "loading") words = "⏳ Hazel's tiles are starting (the text boxes above work meanwhile)…";
  else if (F.phase === "late") { words = `Hazel's tiles did not start in ${SCRIPT_FRAME_WAIT / 1000} s, so they are put away: the text boxes above keep working.`; again = true; }
  else if (F.phase === "crashed") { words = "Hazel's tiles stopped" + (F.why ? ` (${F.why})` : "") + ": the text boxes above keep working."; again = true; }
  else if (!F.status) words = F.why === "quiet"
    ? `Hazel's tiles have had no program: they take one only from matthewhammer.org and localhost, not from this page's address (${location.origin}).`
    : "Hazel's tiles: reading the program…";
  else {
    const R = scriptReport(F.status);
    words = R.ok ? "✅ As Hazel reads it, the program is fine." : R.errors.length
      ? `❗ As Hazel reads it, the program has ${scriptPlural(R.errors.length, "error", "errors")}, marked in the tiles:`
      : "✏️ As Hazel reads it, the program is not finished:";
    items = [...R.errors, ...R.unfinished];
    // (the frame's build and the library's can differ for up to ten minutes after a deploy: a branch build is cached)
    const lib = window.Hazel && window.Hazel.build;
    if (lib && F.build && lib !== F.build && lib !== "local" && F.build !== "local") words += ` (the tiles are from Hazel ${F.build.slice(0, 10)}, the levers' Hazel from ${lib.slice(0, 10)}: a reload in ten minutes makes them one)`;
  }
  if (F.phase === "ready" && F.oops) words += ` (Hazel's tiles had an error, ${F.oops}, and carry on)`;
  line.replaceChildren(words, ...(again ? [" ", el("button", { class: "script-frame-again", onclick: () => { scriptFrame = null; scriptFrameMount(F.holder); } }, "↻ start the tiles again")] : []));
  list.replaceChildren(...items.slice(0, 8).map(t => el("li", {}, t)));
}

// the 📜 Scripting tab: a switch, Hazel's state, then an editor per module, each with the module's name; the program's errors
// under them, at each change, a moment after typing stops; then (ticket 520) Hazel's tiles of the whole program
// (ticket 577) colours for the code: a small tokenizer per language, drawn in a <pre> under the editor's text (which is transparent,
// its caret not), so the text box stays a plain text box. Hazel's comments close with # as they open; Fumola's and JS's are // and /* */
const SCRIPT_KEYWORDS = {
  hazel: "let in fun case end if then else type module test rec and or",
  fumola: "public let var func fun switch case if else return do while for in module type and or not true false null ignore",
  js: "function const let var return if else for while of in new true false null undefined this switch case break continue default typeof",
};
function scriptTokens(lang, text) {
  const kw = new Set(SCRIPT_KEYWORDS[lang].split(" "));
  const comment = lang === "hazel" ? /#[^#\n]*#?/y : /\/\/[^\n]*|\/\*[\s\S]*?(\*\/|$)/y;
  const rules = [["c", comment], ["s", /"(\\.|[^"\\\n])*"?/y], ["n", /\d+(\.\d+)?/y], ["v", lang === "fumola" ? /#[A-Za-z_]\w*/y : /(?!)/y],
    ["w", /[A-Za-z_]\w*/y], ["o", /=>|->|\+\+|[|=<>!+\-*\/%:;,.?#@&^~]+/y], ["t", /\s+|./y]];
  const out = [];
  for (let i = 0; i < text.length;) {
    for (const [k, re] of rules) {
      re.lastIndex = i; const m = re.exec(text);
      if (!m || !m[0]) continue;
      const w = m[0];
      out.push([k === "w" ? (kw.has(w) ? "k" : /^[A-Z]/.test(w) ? "C" : "") : k === "t" ? "" : k, w]);
      i += w.length; break;
    }
  }
  return out;
}
// (under an editor, a last "\n" more, so a last empty line has its height, as it has in the text box)
// (ticket 581) a read-only box (not under an editor) numbers its lines from 1, each number a span the text's copy leaves out
function scriptHighlight(pre, lang, text, under = false) {
  const kids = [], num = n => el("span", { class: "ln", "aria-hidden": "true" }, String(n));
  let n = 1;
  if (!under) kids.push(num(n));
  for (const [k, w] of scriptTokens(lang, text)) {
    const parts = under ? [w] : w.split("\n");
    parts.forEach((part, j) => {
      if (j > 0) { kids.push("\n"); kids.push(num(++n)); }
      if (part) kids.push(k ? el("span", { class: "tk-" + k }, part) : document.createTextNode(part));
    });
  }
  if (under) kids.push("\n");
  pre.replaceChildren(...kids);
}

// (ticket 579, Matthew: "can we adjust the auto-indentation to take the actual code box width into account?") a line longer than
// the box is laid out as a pretty-printer would, to the box's width: broken at the outermost places the code allows, all of them
// at once, so a case's arms each get a line, and a block's statements; then each piece still too long, the same way. The places,
// best first: before a case's | ; before a sum's + (or a sum's, in arithmetic: only spaces either way) ; before then and else ; after ; and before a closing } ; after , and { and in ; after => and ->.
// Each piece goes on 2 deeper than the line, 2 more for each bracket open there; a closing bracket back at its opener's depth.
// Only spaces and line breaks change, never inside a string or a comment (one token each), so a rule means what it meant; lines
// already narrow enough are left as they are
// (Matthew, 2026-10-24, the tiles at a narrow wrap: a long condition, a || b || c, had no place to break; last, so it is used only in a
// piece nothing else breaks)
const SCRIPT_BREAKS = [[["|"], []], [["+"], []], [["then", "else"], []], [["}"], [";"]], [[], [",", "{", "in"]], [[], ["=>", "->"]], [["||", "&&"], []]];
function scriptFlowPieces(lang, toks, indent, cols, out) {
  const text = toks.map(([, w]) => w).join("").trim();
  if (indent + text.length <= cols) { out.push(" ".repeat(indent) + text); return; }
  // (Matthew, 2026-10-23: "line wrap is not adjusting to big text and small screen") a comment alone, too long: several comments,
  // its words shared out (Hazel's # ... # each closed; // each to its line's end)
  const real = toks.filter(([, w]) => w.trim());
  if (real.length === 1 && real[0][0] === "c" && !/^\/\*/.test(text)) {
    const hz = text.startsWith("#"), words = (hz ? text.replace(/^#\s*/, "").replace(/\s*#$/, "") : text.replace(/^\/\/\s*/, "")).split(/\s+/);
    const room = Math.max(12, cols - indent - (hz ? 4 : 3));
    let cur = "";
    const flush = () => { if (cur) out.push(" ".repeat(indent) + (hz ? "# " + cur + " #" : "// " + cur)); cur = ""; };
    for (const w of words) { if (cur && cur.length + 1 + w.length > room) flush(); cur = cur ? cur + " " + w : w; }
    flush(); return;
  }
  // each place a line may break: the token index the next piece starts at, the bracket depth there, and how good a place it is
  const places = [];
  let depth = 0;
  toks.forEach(([k, w], i) => {
    const code = k === "o" || k === "k" || (k === "" && /^[()[\]{}]$/.test(w));
    if (k === "c" && i > 0) places.push({ at: i, depth, rank: -1 });   // (a comment after code: first to go to its own line)
    if (code && i > 0) SCRIPT_BREAKS.forEach(([before], rank) => { if (before.includes(w)) places.push({ at: i, depth: w === "}" ? depth : depth, rank }); });
    if (k === "" || k === "o") for (const ch of w) depth += "([{".includes(ch) ? 1 : ")]}".includes(ch) ? -1 : 0;
    if (code && i + 1 < toks.length) SCRIPT_BREAKS.forEach(([, after], rank) => { if (after.includes(w)) places.push({ at: i + 1, depth, rank, after: w }); });
  });
  if (!places.length) { out.push(" ".repeat(indent) + text); return; }
  const low = Math.min(...places.map(p => p.depth));
  const atLow = places.filter(p => p.depth === low);
  const best = Math.min(...atLow.map(p => p.rank));
  // (Matthew, 2026-10-23: "I don't understand the indentation for Fumola in these types either") a block's members cut apart inside
  // its braces: cut after its { too, so the first member is not left on the opener's line ({#Pull : Side; ... on one, the rest under it)
  const chosen = atLow.filter(p => p.rank === best || (best === 3 && p.after === "{"));
  const cuts = [...new Set(chosen.map(p => p.at))].sort((x, y) => x - y);
  // (and members cut apart at the line's own level, after ; or , or in, are siblings: each at the line's indent, not 2 deeper than
  // the first; the line a list of a sum's cases, or of statements, broke as a staircase)
  const sibling = new Set(low === 0 ? chosen.filter(p => [";", ",", "in"].includes(p.after)).map(p => p.at) : []);
  let from = 0;
  for (const c of [...cuts, toks.length]) {
    const piece = toks.slice(from, c);
    if (piece.some(([, w]) => w.trim())) {
      const first = piece.find(([, w]) => w.trim())[1];
      const ind = from === 0 || sibling.has(from) ? indent : indent + 2 * (/^[)\]}]/.test(first) ? Math.max(0, low - 1) : Math.max(1, low));
      scriptFlowPieces(lang, from === 0 ? piece : piece, ind, cols, out);
    }
    from = c;
  }
}
function scriptFlowLine(lang, line, cols) {
  if (line.length <= cols) return [line];
  const base = line.match(/^ */)[0].length, out = [];
  scriptFlowPieces(lang, scriptTokens(lang, line.slice(base)), base, cols, out);
  return out;
}
function scriptFlow(lang, text, cols) {
  if (!(cols > 20)) return text;
  return text.split("\n").flatMap(l => scriptFlowLine(lang, l, cols)).join("\n");
}
// (ticket 581: and line numbers, on unless turned off, "start at 1 for each box")
// (ticket 580, Matthew: "a sub-panel called script editor settings": every room's engine, font size, font, syntax colours on or
// off, and the longest line before it is broken, default 77; "please make the panel minimal; with minimal label length"; "always be
// knolling") kept with the scripts, so a save keeps them; drawn on the page's root, so the ⛶ view (outside the panel) has them too
const SCRIPT_FONTS = [["sys", "ui-monospace, SFMono-Regular, Menlo, monospace"], ["Menlo", "Menlo, Monaco, monospace"],
  ["Consolas", "Consolas, 'Liberation Mono', monospace"], ["DejaVu", "'DejaVu Sans Mono', monospace"], ["Courier", "'Courier New', Courier, monospace"]];
const SCRIPT_SIZES = [11, 12.5, 14, 16, 18];
function scriptEditorSettings() {
  const S = scriptsState(), E = S.editor || (S.editor = {});
  if (!SCRIPT_SIZES.includes(E.size)) E.size = 12.5;
  if (!SCRIPT_FONTS.some(([n]) => n === E.font)) E.font = "sys";
  if (typeof E.colours !== "boolean") E.colours = true;
  if (!(E.wrap >= 20 && E.wrap <= 400)) E.wrap = 77;
  if (typeof E.lines !== "boolean") E.lines = true;   // (ticket 581)
  if (E.hazel !== "text") E.hazel = "tiles";   // (ticket 578: Hazel's boxes in tiles, or text)
  return E;
}
function scriptEditorApply() {
  const E = scriptEditorSettings(), R = document.documentElement.style;
  if (typeof scriptTilesReflowSoon === "function") scriptTilesReflowSoon();   // (wrap, size or line numbers: the tiles broken to fit again)
  R.setProperty("--script-size", E.size + "px");
  R.setProperty("--script-font", SCRIPT_FONTS.find(([n]) => n === E.font)[1]);
  document.body.classList.toggle("script-plain", !E.colours);
  document.body.classList.toggle("script-nolines", !E.lines);
  for (const b of document.querySelectorAll(".script-hazel-body")) if (b.show) b.show();
  // (Matthew, 2026-10-23: "if we have 'tiles' selected, we shouldn't see both": in tiles, no text at all, the types' read-only
  // listing included; the outline still names them)
  document.body.classList.toggle("script-tiles-on", E.hazel === "tiles");
  // (Matthew, 2026-10-23: "line numbers vanished" in the tiles: Hazel draws its own, told by ⚙️'s lines, as every box's tiles are)
  for (const T of scriptTileFrames) if (T.ready && T.iframe && T.iframe.contentWindow) T.iframe.contentWindow.postMessage({ type: "lines", on: E.lines }, SCRIPT_HAZEL_ORIGIN);
  for (const T of scriptTileFrames) scriptTileFont(T);
  if (scriptFrame && scriptFrame.phase === "ready") scriptTileFont({ ready: true, iframe: scriptFrame.iframe });
  for (const ta of document.querySelectorAll("textarea.script-editor, pre.script-js")) if (ta.reflow) ta.reflow();
}
// how many characters of the code's font fit across a text box, its padding and scroll bar taken off
function scriptCols(ta) {
  const c = scriptCols.ctx || (scriptCols.ctx = document.createElement("canvas").getContext("2d"));
  const cs = getComputedStyle(ta);
  c.font = cs.font;
  const w = ta.clientWidth - parseFloat(cs.paddingLeft) - parseFloat(cs.paddingRight);
  return Math.min(scriptEditorSettings().wrap, Math.floor(w / c.measureText("0123456789").width * 10) - 1);   // (580: at most the setting)
}

// (ticket 577, Matthew: "let's add a 'max size' button for each code splice") ⛶ on a box: the page's code at full size, an outline
// of every box on the left (📚 Common first, then the rooms, each with the language it shows) and the chosen box on the right. The
// box itself moves there, editors and all, and back to its place when another is chosen or the view is closed (✖ or Escape)
let scriptMax = null;
function scriptMaxOpen(boxes, sec) {
  if (!scriptMax) {
    const nav = el("nav", { class: "script-max-nav", "aria-label": "the code, box by box" });
    const main = el("div", { class: "script-max-main" });
    const close = el("button", { class: "script-max-close", title: "back to the panel (Escape)", onclick: () => scriptMaxClose() }, "✖ back");
    // (Matthew, 2026-10-23: "please make LHS vs RHS of FS script panel dragable") a divider between the outline and the box, dragged
    // to set the outline's width (remembered in this browser)
    const divider = el("div", { class: "script-max-divider", role: "separator", "aria-orientation": "vertical", "aria-label": "drag to widen or narrow the outline", tabindex: "0" });
    // (Matthew, 2026-10-23: "in FS mode, lets hide the same script editor settings panel under a 'show editor settings here' chip
    // that defaults to off", "just below the 'back' chip, on LHS") the panel's settings box, moved here while the chip is on, and
    // back to its place when it is off or the view closes (scriptMaxSettings)
    const setChip = el("button", { class: "script-max-settings-chip", "aria-pressed": "false", onclick: () => scriptMaxSettings(setChip.getAttribute("aria-pressed") !== "true") }, "⚙️ show editor settings here");
    const setHolder = el("div", { class: "script-max-settings" });
    const wrap = el("div", { class: "script-max", role: "dialog", "aria-label": "the game's code, at full size" },
      [el("div", { class: "script-max-left" }, [close, setChip, setHolder, nav]), divider, main]);
    const setW = px => { const w = Math.max(140, Math.min(window.innerWidth - 240, px)); wrap.style.setProperty("--script-max-left", w + "px"); try { localStorage.setItem("scriptMaxLeft", String(w)); } catch (e) {} };
    try { const w = +localStorage.getItem("scriptMaxLeft"); if (w) setW(w); } catch (e) {}
    divider.addEventListener("pointerdown", e => {
      e.preventDefault(); divider.setPointerCapture(e.pointerId); wrap.classList.add("script-max-dragging");
      const move = ev => setW(ev.clientX - wrap.getBoundingClientRect().left - 10);
      const up = () => { divider.removeEventListener("pointermove", move); divider.removeEventListener("pointerup", up); wrap.classList.remove("script-max-dragging"); };
      divider.addEventListener("pointermove", move); divider.addEventListener("pointerup", up);
    });
    divider.addEventListener("keydown", e => { const w = wrap.querySelector(".script-max-left").getBoundingClientRect().width; if (e.key === "ArrowLeft") setW(w - 20); else if (e.key === "ArrowRight") setW(w + 20); else return; e.preventDefault(); e.stopPropagation(); });
    const key = e => { if (e.key === "Escape") { e.stopPropagation(); scriptMaxClose(); } };
    wrap.addEventListener("keydown", key);
    document.body.append(wrap);
    document.body.classList.add("script-maxed");
    scriptMax = { wrap, nav, main, boxes, sec: null, spot: null, setChip, setHolder, setSpot: null };
  }
  const M = scriptMax;
  // (Matthew, 2026-10-23: "let's pre-load the tile editor for each code block on the LHS, even if not in view": the chosen box is
  // pinned over the main area where it is in the page, not moved there, since a frame moved in the page loads again)
  if (M.sec && M.sec !== sec) scriptMaxUnpin(M.sec);
  M.sec = sec;
  sec.classList.add("script-max-pinned");
  scriptTilesFirst(sec);
  scriptMaxPlace();
  if (!M.placing) {
    M.placing = () => scriptMaxPlace();
    window.addEventListener("resize", M.placing);
    if (typeof ResizeObserver === "function") { M.ro = new ResizeObserver(M.placing); M.ro.observe(M.main); }
  }
  scriptReflowSoon(sec); if (M.prev && M.prev !== sec) scriptReflowSoon(M.prev); M.prev = sec;
  scriptMaxNav();
  const ta = sec.querySelector("textarea"); if (ta) ta.focus({ preventScroll: true });   // (and so laid out to its new width: 579)
}
// (ticket 582, Matthew: "more of an 'outline' structure on LHS": Hazel's types apart from Common's other definitions, and "each
// function for each code listing as an up-to-date way to scroll to that function") under each box, what its code defines, in
// order: each type, then each function (Hazel's let, Fumola's func, JS's function), from the text as it is now; a click scrolls
// that line into view (opening the box first)
function scriptOutline(text, lang) {
  const out = [];
  text.split("\n").forEach((line, i) => {
    let m;
    if ((m = line.match(lang === "fumola" ? /^\s*(?:public\s+)?type\s+([A-Za-z_]\w*)/ : /^type\s+([A-Za-z_]\w*)/))) out.push({ kind: "type", name: m[1], line: i });
    else if (lang === "hazel" && (m = line.match(/^let\s+([a-z_]\w*)\s*=/))) out.push({ kind: "fn", name: m[1], line: i });
    else if (lang === "fumola" && (m = line.match(/^\s*(?:public\s+)?func\s+([A-Za-z_]\w*)/))) out.push({ kind: "fn", name: m[1], line: i });
    else if (lang === "js" && (m = line.match(/^\s*(?:async\s+)?function\s*\*?\s*([A-Za-z_$][\w$]*)/))) out.push({ kind: "fn", name: m[1], line: i });
  });
  return out;
}
// each code listing in a box, in order: its element, its text and its language
function scriptListings(box) {
  return [...box.querySelectorAll("pre.script-types, pre.script-js, textarea.script-editor")].map(n =>
    n.tagName === "TEXTAREA" ? { node: n, text: n.value, lang: n.dataset.lang } : { node: n, text: n.textContent.replace(/^\d+/gm, ""), lang: n.classList.contains("script-types") ? "hazel" : "js" });
}
// (ticket 584, Matthew: "when we select something, have a subtle bg highlight for those lines on RHS") a band behind the chosen
// definition's lines, from its first to the last before the next one (less the comments and blank lines leading into that one);
// one band at a time. Under an editor it sits between the colours and the text box, kept at its lines as the box scrolls; in a
// read-only box, inside it, at its line numbers' marks
function scriptBand(node, line, end) {
  for (const b of document.querySelectorAll(".script-band")) { if (b.unscroll) b.unscroll(); b.remove(); }
  const band = el("div", { class: "script-band", "aria-hidden": "true" });
  if (node.tagName === "TEXTAREA") {
    const cs = getComputedStyle(node), lh = parseFloat(cs.lineHeight), top0 = parseFloat(cs.paddingTop) + parseFloat(cs.borderTopWidth);
    const place = () => { band.style.top = (top0 + line * lh - node.scrollTop) + "px"; band.style.height = ((end - line + 1) * lh) + "px"; };
    place(); node.addEventListener("scroll", place); band.unscroll = () => node.removeEventListener("scroll", place);
    node.before(band);
    setTimeout(place, 60); setTimeout(place, 400);
  } else {
    const marks = node.querySelectorAll(".ln"); if (!marks[line]) return;
    const a = marks[line], z = marks[end] || a, lh = parseFloat(getComputedStyle(node).lineHeight);
    band.style.top = a.offsetTop + "px"; band.style.height = (z.offsetTop - a.offsetTop + lh) + "px";
    node.prepend(band);
  }
}
function scriptMaxJump(box, node, line, end = line, name = null) {
  const M = scriptMax; if (!M) return;
  // (^ticket(587), Matthew 2026-10-23: "when the editor is the hazel tile editor, let's have click on LHS mean 'select tile' on RHS")
  // in tiles, Hazel puts the caret on the definition and selects it (reveal), and says where, and the view scrolls there; a type,
  // being the program's start, in the whole program's tiles
  if (name && document.body.classList.contains("script-tiles-on") && ((node.tagName === "TEXTAREA" && node.closest("[hidden]")) || (node.matches && node.matches("pre.script-types")))) {
    scriptReveal(node.matches("pre.script-types") ? "types" : (box.dataset.room || ""), name);
    return;
  }
  // (Matthew, 2026-10-23: "LHS selection triggers incorrect range on RHS": a box opened at max size is laid out again at its new
  // width, and its lines move; the outline's were the old layout's. Now the box opened first, then, laid out, the definition
  // looked for again by its name in the text as it is, and its lines counted as the outline counts them)
  if (M.sec !== box) { scriptMaxOpen(M.boxes, box); setTimeout(() => scriptMaxJump(box, node, line, end, name), 80); return; }
  if (name) {
    const lang = node.tagName === "TEXTAREA" ? node.dataset.lang : node.classList.contains("script-types") ? "hazel" : "js";
    const text = node.tagName === "TEXTAREA" ? node.value : node.textContent.replace(/^\d+/gm, "");
    const lines = text.split("\n"), os = scriptOutline(text, lang), i = os.findIndex(o => o.name === name);
    if (i >= 0) {
      line = os[i].line; end = (os[i + 1] ? os[i + 1].line : lines.length) - 1;
      while (end > line && /^\s*(#|\/\/|$)/.test(lines[end])) end--;
    }
  }
  // (Matthew, 2026-10-23: "Fumola LHS click to select also seems off": the band was placed before the text box scrolled to the
  // line, and kept up only by its scroll events, which do not always come; now a text box's band is placed after the scroll, from
  // where the box is then, and again a moment later)
  if (node.tagName !== "TEXTAREA") scriptBand(node, line, end);
  // (Matthew, 2026-10-23: "LHS nav bar does not respond to click -- a recent regression?" A Hazel box in tiles hides its text box,
  // and the jump scrolled that. Now, in tiles, the view goes to the line's row in the tiles: the frame's height over the text's
  // lines, as Hazel lays the text out a line to a row; the band drawn over the tiles there)
  const hidden = node.tagName === "TEXTAREA" && node.closest("[hidden]");
  const tilesWrap = hidden && node.closest(".script-hazel-body") && node.closest(".script-hazel-body").querySelector(".script-tile-wrap");
  if (tilesWrap) {
    const n = Math.max(1, node.value.split("\n").length), frame = tilesWrap.querySelector("iframe");
    const top0 = 8, rowH = frame ? Math.max(8, (frame.getBoundingClientRect().height - 2 * top0) / n) : 20;
    const y = tilesWrap.getBoundingClientRect().top + top0 + line * rowH;
    const view = M.sec || M.main, vtop = view.getBoundingClientRect().top;
    view.scrollTop += y - vtop - 40;
    for (const b of document.querySelectorAll(".script-band")) { if (b.unscroll) b.unscroll(); b.remove(); }
    const band = el("div", { class: "script-band", "aria-hidden": "true" });
    band.style.top = (top0 + line * rowH + tilesWrap.scrollTop) + "px"; band.style.height = ((end - line + 1) * rowH) + "px";
    tilesWrap.prepend(band);
    return;
  }
  if (node.tagName === "TEXTAREA") {
    // (the review, Matthew 2026-10-23: "follow your recommendations") a text box's definition selected with the browser's own
    // selection, its lines from the first to the last, not a band drawn over it: the browser keeps it on the text however the
    // box wraps or scrolls (two bands drifted that day)
    const ls = node.value.split("\n"), at = ls.slice(0, line).reduce((n, l) => n + l.length + 1, 0);
    const to = ls.slice(0, end + 1).reduce((n, l) => n + l.length + 1, 0) - 1;
    for (const b of document.querySelectorAll(".script-band")) { if (b.unscroll) b.unscroll(); b.remove(); }
    node.scrollIntoView({ block: "nearest" });
    node.focus({ preventScroll: true }); node.setSelectionRange(at, Math.max(at, to), "backward");
    const cs = getComputedStyle(node), lh = parseFloat(cs.lineHeight);
    node.scrollTop = Math.max(0, line * lh - 8);
    // (Matthew, 2026-10-23: "need to scroll to things even if not visible in viewport") a text box as tall as its text does not
    // scroll itself: the view around it does, to the line
    const y = node.getBoundingClientRect().top + parseFloat(cs.paddingTop) + line * lh - node.scrollTop;
    const view = M.sec || M.main, top = view.getBoundingClientRect().top;
    if (y < top + 8 || y > top + view.clientHeight - 2 * lh) view.scrollTop += y - top - 40;
  } else {
    const mark = node.querySelectorAll(".ln")[line];
    if (mark) mark.scrollIntoView({ block: "start" });
  }
}
// (Matthew, 2026-10-23: "Hazel functions give their type sigs in LHS") each Hazel function's type as Hazel infers it, not only as
// written: the program, its tail replaced by one probe per function, each held to () (let _p : () = Common.nth in), so Hazel's
// analysis says, for each, "Expecting type () but got inconsistent type [Bool], Int -> Bool". One analysis per program text,
// after the outline is drawn; the outline drawn again with the types. A module with an error of its own gives no types
// (a Fumola function's signature, from its annotations, as Hazel's are shown: (T1, T2) -> R, "Common." left off. None unless every
// parameter and the result are annotated)
function scriptFumolaSig(head) {
  const m = head.match(/func\s+[A-Za-z_]\w*\s*\(([^)]*(?:\([^)]*\)[^)]*)*)\)\s*:\s*([^{=]+?)\s*[{=]/);
  if (!m) return null;
  const parts = []; let d = 0, from = 0, ps = m[1];
  for (let i = 0; i <= ps.length; i++) { const c = ps[i]; if (c === "(" || c === "[" || c === "{") d++; else if (c === ")" || c === "]" || c === "}") d--; else if ((c === "," && d === 0) || i === ps.length) { parts.push(ps.slice(from, i)); from = i + 1; } }
  const ts = parts.filter(x => x.trim()).map(x => { const k = x.indexOf(":"); return k < 0 ? null : x.slice(k + 1).trim(); });
  if (ts.some(t => t == null)) return null;
  const short = t => t.replace(/\bCommon\./g, "");
  const args = ts.length === 1 ? short(ts[0]) : "(" + ts.map(short).join(", ") + ")";
  return args + " -> " + short(m[2].trim());
}
let scriptTypesCache = { key: null, types: new Map() };
function scriptHazelTypes() {
  if (!window.Hazel || typeof window.Hazel.analyze !== "function") return null;
  // (^ticket(587), Matthew 2026-10-23: "maybe we need to re-architect the JS library to expose more of modular editors?") from
  // Hazel itself, where the library has it (Hazel.outlineProgram, the branch lib-outline-587): each member's type as Hazel infers
  // it, the program's sums and products by their names; else the probe below
  if (typeof window.Hazel.outlineProgram === "function") {
    const spec = scriptSpec(), key = "outline:" + JSON.stringify(spec);
    if (scriptTypesCache.key === key) return scriptTypesCache.types;
    const types = new Map();
    try { const o = window.Hazel.outlineProgram(spec); for (const m of o.modules || []) for (const x of m.members || []) if (x.type) types.set(m.name + "." + x.name, x.type); } catch (e) {}
    scriptTypesCache = { key, types };
    return types;
  }
  const S = scriptsState(), mods = [{ name: "Common", text: scriptBody(S.common) }, ...SCRIPT_ROOMS.map(r => ({ name: r.name, text: scriptBody(S.rooms[r.name]) }))];
  const probes = mods.flatMap(m => scriptOutline(m.text, "hazel").filter(o => o.kind === "fn").map(o => `${m.name}.${o.name}`));
  const prog = scriptProgram().split("\n").slice(0, -1).join("\n") + "\n" + probes.map((p, i) => `let probe${i} : () = ${p} in`).join("\n") + "\n0";
  if (scriptTypesCache.key === prog) return scriptTypesCache.types;
  const types = new Map();
  try {
    for (const e of (window.Hazel.analyze(prog).errors || [])) {
      const m = String(e).match(/Error in term:\s*\n\s*(\S+)\s*\nNature of error: Expecting type \(\) but got inconsistent type ([^\n]+)/);
      if (m) types.set(m[1], m[2].trim());
    }
  } catch (e) {}
  for (const [k, t] of types) types.set(k, scriptTypeNames(t));
  scriptTypesCache = { key: prog, types };
  return types;
}
// Hazel prints a sum by its cases, not its name (Flip(+ L + R, Cmd), and a recursive one as rec Cmd -> ...): each of the
// program's own sums folded back to its name, the smallest first (so a bigger one's cases read as its declaration does), then
// spaced again
function scriptTypeNames(t) {
  const sums = SCRIPT_PREAMBLE.split(/\bin\b\s*\n?/).map(d => d.match(/^\s*type\s+(\w+)\s*=\s*\+?([\s\S]*)$/)).filter(m => m && /^\s*\+/.test(m[0].slice(m[0].indexOf("=") + 1)))
    .map(([, name, body]) => [name, body.replace(/\s+/g, "")]).sort((a, b) => a[1].length - b[1].length);
  const esc = x => x.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  let u = t.replace(/\s+/g, "");
  for (let again = true, n = 0; again && n < 4; n++) {
    again = false;
    for (const [name, body] of sums) {
      const v = u.replace(new RegExp("rec" + name + "->\\+?" + esc(body), "g"), name).replace(new RegExp("(^|[(,>\\[])\\+?" + esc(body) + "(?=$|[),\\]-])", "g"), "$1" + name);
      if (v !== u) { u = v; again = true; }
    }
  }
  // (a tuple on either side of an arrow, in brackets, as the code writes it: Hazel leaves them out)
  const top = (x, sep) => { const out = []; let d = 0, from = 0; for (let i = 0; i < x.length; i++) { const c = x[i]; if ("([".includes(c)) d++; else if (")]".includes(c)) d--; else if (d === 0 && x.startsWith(sep, i)) { out.push(x.slice(from, i)); from = i + sep.length; i += sep.length - 1; } } out.push(x.slice(from)); return out; };
  u = top(u, "->").map(side => top(side, ",").length > 1 ? "(" + side + ")" : side).join("->");
  return u.replace(/,/g, ", ").replace(/->/g, " -> ").replace(/\+/g, " + ");
}
// (Matthew, 2026-10-23: "for each sum type being defined, it says the number of 'cases'") a sum's constructors, counted: each + and
// a capital after the =, outside brackets (a payload's types are not cases); a record or an alias has none, and says nothing
function scriptCases(def) {
  const body = def.slice(def.indexOf("=") + 1).replace(/\s+in\s*$/, "").replace(/;\s*$/, "");
  // (Fumola's, in Motoko's notation: a variant {#A; #B : T}, its cases each # at the braces' own depth)
  if (/^\s*\{\s*#/.test(body)) { let d = 0, n = 0; for (const c of body) { if ("([{".includes(c)) d++; else if (")]}".includes(c)) d--; else if (c === "#" && d === 1) n++; } return n; }
  if (!/^\s*\+/.test(body)) return 0;
  let depth = 0, n = 0;
  for (let i = 0; i < body.length; i++) {
    const ch = body[i];
    if ("([{".includes(ch)) depth++; else if (")]}".includes(ch)) depth--;
    else if (ch === "+" && depth === 0 && /^\+\s*[A-Z]/.test(body.slice(i))) n++;
  }
  return n;
}
// (Matthew: "each type has an emoji, right justified, that says: sum type, product type, etc") what kind of type it defines, by
// what follows its =: a sum's + , a product's ( (a tuple, labeled or not), a list's [ , a function's -> , else a name (an alias)
const SCRIPT_TYPE_KINDS = { sum: ["➕", "sum type"], product: ["📦", "product type"], list: ["📋", "list type"], fn: ["➡️", "function type"], alias: ["🔗", "another name for a type"] };
function scriptTypeKind(def) {
  const body = def.slice(def.indexOf("=") + 1).replace(/\s+in\s*$/, "").trim();
  return body.startsWith("+") || /^\{\s*#/.test(body) ? "sum" : /->/.test(body.replace(/\([^()]*\)/g, "")) ? "fn"
    : body.startsWith("(") || body.startsWith("{") ? "product" : body.startsWith("[") ? "list" : "alias";
}
// (Matthew: "for functions, let's say N lines --- and when they have type () -> ... no params, and no result, just effects, ---
// let's use the magic wand emoji") a function's line count, right-justified, and 🪄 for one that takes nothing and only acts:
// Hazel's, by its inferred type (() -> Cmd, () -> ()); Fumola's and JS's, by no parameters and no return of a value
const fnTail = (lang, sig, ls) => {
  const head = ls[0] || "";
  // (Matthew, 2026-10-23, nth and rooms wanded: "the magic wand seems misapplied --- we haven't put optional type annotations on
  // the Fumola code?" The test took any () on the line, xs.size()'s too. Now the function's OWN parameters must be empty; and a
  // Fumola function gives back its last expression, with no return to look for, so without a type none can be said to give back
  // nothing: no wand there, until its code carries types)
  const wand = sig ? /^\(\)\s*->\s*(\(\)|Cmd)$/.test(sig)
    : lang === "js" && /\bfunction\s*\*?\s*[\w$]*\s*\(\s*\)/.test(head) && !ls.some(l => /\breturn\s+[^;\s]/.test(l));
  return el("span", { class: "script-max-tail" }, [el("span", { class: "script-max-cases" }, ls.length + (ls.length === 1 ? " line" : " lines")),
    el("span", { class: "script-max-tkind", title: wand ? "takes nothing, and only acts" : "", "aria-label": wand ? "takes nothing, and only acts" : "" }, wand ? "🪄" : "")]);
};
const cases = def => {
  const n = scriptCases(def), [emoji, what] = SCRIPT_TYPE_KINDS[scriptTypeKind(def)];
  return [el("span", { class: "script-max-tail" }, [...(n ? [el("span", { class: "script-max-cases" }, n + (n === 1 ? " case" : " cases"))] : []),
    el("span", { class: "script-max-tkind", title: what, "aria-label": what }, emoji)])];
};
function scriptMaxNav() {
  const M = scriptMax; if (!M) return;
  // (the Hazel types: as known for this program text, else asked for after this drawing, and drawn again with them)
  const types = scriptTypesCache.key && scriptTypesCache.types;
  const modOf = b => b.dataset.room;
  if (window.Hazel && !scriptMaxNav.asking) { scriptMaxNav.asking = true; setTimeout(() => { const before = scriptTypesCache.key; scriptHazelTypes(); scriptMaxNav.asking = false; if (scriptTypesCache.key !== before) scriptMaxNav(); }, 0); }
  M.nav.replaceChildren(...M.boxes.map(b => {
    const eng = SCRIPT_ENGINES.find(([v]) => v === b.shown()) || [, ""];
    const lang = eng[0] ? scriptEngineLabel(eng[0], eng[1]) : "";
    // (Matthew, 2026-10-23: "Let's put a spinner on that chip on the LHS for each code block") its tiles not ready yet: the tile spins
    const tw = b.querySelector(".script-tile-wrap");
    if (lang && lang.classList && tw && !tw.closest("[hidden]") && tw.dataset.ready !== "1") lang.classList.add("loading");
    const name = [...b.querySelector(".box-name").childNodes].filter(n => n.tagName !== "BUTTON").map(n => n.textContent).join("");
    const a = el("button", { class: "script-max-item", "aria-current": String(b === M.sec), onclick: () => scriptMaxOpen(M.boxes, b) },
      [el("span", {}, name), el("span", { class: "script-max-lang" }, lang)]);
    const subs = scriptListings(b).flatMap(({ node, text, lang }) => { const lines = text.split("\n"), os = scriptOutline(text, lang); return os.map((o, i) => {
      // (584) its lines: up to the next definition, less the comments and blank lines that lead into that one
      let end = (os[i + 1] ? os[i + 1].line : lines.length) - 1;
      while (end > o.line && /^\s*(#|\/\/|$)/.test(lines[end])) end--;
      const sig = o.kind !== "fn" ? null : lang === "hazel" ? (types ? types.get(modOf(b) + "." + o.name) : null)
        : lang === "fumola" ? scriptFumolaSig(lines.slice(o.line, o.line + 3).join(" ")) : null;
      return el("button", { class: "script-max-sub script-max-" + o.kind + (sig ? " script-max-sig" : ""), title: (o.kind === "type" ? "type " : "") + o.name + ", lines " + (o.line + 1) + "-" + (end + 1), onclick: () => scriptMaxJump(b, node, o.line, end, o.name) },
        o.kind === "type" ? [el("span", { class: "script-max-kind" }, "type"), el("span", { class: "script-max-tname" }, o.name), ...cases(lines.slice(o.line, end + 1).join(" "))]
          : [el("span", { class: "script-max-fname" }, [o.name, fnTail(lang, sig, lines.slice(o.line, end + 1))]), ...(sig ? [el("span", { class: "script-max-type-sig", title: o.name + " : " + sig }, ": " + sig)] : [])]); }); });
    return el("div", { class: "script-max-group" }, [a, ...(subs.length ? [el("div", { class: "script-max-subs" }, subs)] : [])]);
  }));
}
let scriptMaxNavTimer = null;
const scriptMaxNavSoon = () => { if (!scriptMax) return; clearTimeout(scriptMaxNavTimer); scriptMaxNavTimer = setTimeout(scriptMaxNav, 300); };
// a box moved (into the ⛶ view or back) has a new width: its code laid out again, once it is in place (579)
const scriptReflowSoon = box => setTimeout(() => { for (const n of box.querySelectorAll("textarea.script-editor, pre.script-js")) if (n.reflow) n.reflow(); }, 0);
// the settings box shown in the max-size view's left column (on), or back in the panel (off)
function scriptMaxSettings(on) {
  const M = scriptMax; if (!M) return;
  const box = M.setSpot ? null : document.querySelector("#panel .script-settings");
  if (on && !M.setSpot && box) { M.setSpot = el("div", { hidden: "" }); box.replaceWith(M.setSpot); M.setHolder.replaceChildren(box); }
  if (!on && M.setSpot) { const b = M.setHolder.firstChild; if (b) M.setSpot.replaceWith(b); else M.setSpot.remove(); M.setSpot = null; }
  setPressed(M.setChip, !!M.setSpot);
}
// the pinned box set over the main area: its place and size
function scriptMaxPlace() {
  const M = scriptMax; if (!M || !M.sec) return;
  scriptTilesReflowSoon();   // (the box's width may have changed: its tiles broken to fit again)
  const r = M.main.getBoundingClientRect(), st = M.sec.style;
  st.left = r.left + "px"; st.top = r.top + "px"; st.width = r.width + "px"; st.height = r.height + "px";
}
function scriptMaxUnpin(sec) {
  sec.classList.remove("script-max-pinned");
  for (const k of ["left", "top", "width", "height"]) sec.style[k] = "";
  scriptReflowSoon(sec);
}
function scriptMaxClose() {
  const M = scriptMax; if (!M) return;
  if (M.sec) scriptMaxUnpin(M.sec);
  scriptMaxSettings(false);
  if (M.placing) { window.removeEventListener("resize", M.placing); if (M.ro) M.ro.disconnect(); }
  M.wrap.remove(); document.body.classList.remove("script-maxed");
  scriptMax = null;
}

function scriptingPanel(box) {
  const S = scriptsState();
  const status = el("div", { class: "hint script-status" }, "Hazel: loading…");
  const errs = el("ul", { class: "script-errors" });
  const fstatus = el("div", { class: "hint script-fumola-status" });
  const ferrs = el("ul", { class: "script-errors script-fumola-errors" });
  const uses = eng => SCRIPT_ROOMS.some(r => scriptEngine(r.name) === eng);
  const sync = () => {
    scriptFramePost();   // (ticket 520: the tiles, on the same timer)
    scriptTilesPost();   // (578: each box's own tiles)
    if (window.Hazel) {
      const C = scriptCompile();
      status.textContent = `Hazel ${window.Hazel.version}: the program ${C.errors.length ? `has ${scriptPlural(C.errors.length, "error", "errors")}, so its rooms run their JS`
        : !C.ready ? "is not finished, so its rooms run their JS" : "is fine"} (analyzed and run in ${Math.round(C.ms)} ms)`;
      status.title = C.via === "runProgram" ? "built by Hazel from the modules (Hazel.runProgram)" : "joined by the game, run by Hazel.run (this Hazel has no runProgram)";
      errs.replaceChildren(...[...C.errors.slice(0, 8), ...C.unfinished].map(e => el("li", {}, e)));
    }
    // (ticket 566) Fumola's, once a room uses it or its box is shown: loaded then, not before
    if (typeof fumolaReady !== "function") return;   // (a page without 25-fumola.js)
    if (!fumolaReady()) { fstatus.textContent = uses("fumola") || boxes.some(b => b.shown() === "fumola") ? "Fumola: loading…" : ""; ferrs.replaceChildren(); return; }
    const F = fumolaCompile();
    fstatus.textContent = `Fumola: the rules ${F.errors.length ? "have an error, so its rooms run their JS" : "are fine"} (defined in ${Math.round(F.ms)} ms)`;
    ferrs.replaceChildren(...F.errors.slice(0, 4).map(e => el("li", {}, e)));
  };
  const wantFumola = () => typeof fumolaLoad !== "function" ? null : fumolaLoad().then(sync, () => { fstatus.textContent = "Fumola did not load (the game's copy, " + FUMOLA_BUILD + "): its rooms run their JS"; });
  hazelLoad().then(sync, () => { status.textContent = "Hazel did not load (it comes from " + SCRIPT_LIB_URL + "): its rooms run their JS"; });
  if (uses("fumola")) wantFumola();
  let timer = null;
  const later = () => { clearTimeout(timer); timer = setTimeout(() => { changed(); sync(); }, 600); };
  // (each change written to the state as it is now, not as the tab found it: a load or a start over replaces it, and then the
  // boxes are filled again from the new one, below)
  const editor = (modName, get, put, lang) => {
    const ta = el("textarea", { class: "script-editor", spellcheck: "false", rows: "9", wrap: "off", "data-module": modName, "data-lang": lang });
    const hl = el("pre", { class: "script-hl", "aria-hidden": "true" });
    let paint = () => { scriptHighlight(hl, lang, ta.value, true); hl.scrollTop = ta.scrollTop; hl.scrollLeft = ta.scrollLeft; };
    // (ticket 579) the text as stored (src), shown broken to the box's width, from src each time (so a wider box takes the breaks
    // a narrower one made back out, up to the ones typed): again when the box's width changes, and when the
    // box is left (lines typed long). The caret keeps its place: the same count of non-space characters before it. A typed change
    // stores the text as shown
    let src = get(), cols = 0;
    const flow = () => {
      const n = scriptCols(ta); if (n === cols && ta.value) return; cols = n;
      const t = scriptFlow(lang, src, cols); if (t === ta.value) return;
      const had = document.activeElement === ta, k = had ? ta.value.slice(0, ta.selectionStart).replace(/\s/g, "").length : 0;
      ta.value = t; paint(); scriptMaxNavSoon();
      if (had) { let i = 0; for (let m = 0; i < t.length && m < k; i++) if (!/\s/.test(t[i])) m++; ta.setSelectionRange(i, i); }
    };
    ta.value = src;
    ta.refill = () => { const want = get(); if (want !== src) { src = want; ta.value = want; cols = 0; flow(); paint(); } };
    ta.addEventListener("input", () => { src = ta.value; put(src); paint(); later(); scriptMaxNavSoon(); for (const b of ta.parentNode.querySelectorAll(".script-band")) { if (b.unscroll) b.unscroll(); b.remove(); } });
    ta.addEventListener("blur", () => { cols = 0; flow(); });
    ta.reflow = () => { cols = 0; if (ta.clientWidth) flow(); };
    ta.addEventListener("focus", () => { if (ta.clientWidth) flow(); });
    // (a ResizeObserver says when the width changes, in a shown page; a hidden one, or a browser without it: soon after it is made, as a hidden page has no frames)
    if (typeof ResizeObserver === "function") new ResizeObserver(() => { if (ta.clientWidth) flow(); }).observe(ta);
    setTimeout(() => { if (ta.clientWidth) flow(); }, 50);
    ta.addEventListener("scroll", () => { hl.scrollTop = ta.scrollTop; hl.scrollLeft = ta.scrollLeft; });
    // (the game's keys move the friend: in an editor they are the editor's)
    ta.addEventListener("keydown", e => e.stopPropagation());
    paint();
    // (ticket 577: the colours under the text, the same size as it, however the box is resized)
    // (ticket 581) the line numbers beside it, from 1, scrolled with it; lines are not wrapped by the box (579 breaks them), so
    // each number stays beside its line
    const gutter = el("pre", { class: "script-ln", "aria-hidden": "true" });
    const number = () => { const n = ta.value.split("\n").length; if (gutter.n !== n) { gutter.n = n; gutter.textContent = Array.from({ length: n }, (_, i) => i + 1).join("\n") + "\n"; gutter.style.width = (String(n).length + 1) + "ch"; } gutter.scrollTop = ta.scrollTop; };
    ta.addEventListener("input", number); ta.addEventListener("scroll", () => { gutter.scrollTop = ta.scrollTop; });
    const repaint = paint; paint = () => { repaint(); number(); };
    number();
    const wrap = el("div", { class: "script-edit" }, [gutter, el("div", { class: "script-edit-in" }, [hl, ta])]);
    wrap.refill = ta.refill;
    return wrap;
  };
  // (578) a Hazel box's body: its tiles (a frame for its module) or its text box, as ⚙️'s "hazel" row says (Matthew, 2026-10-23:
  // "please place in editor settings --- not once per box"); tiles unless it says text
  const hazelBody = (mod, textEditor) => {
    let T = null;
    const tilesHolder = el("div", { class: "script-tiles-in" });
    const wrap = el("div", { class: "script-hazel-body" }, [tilesHolder, textEditor]);
    wrap.show = () => {
      const v = scriptEditorSettings().hazel;
      textEditor.hidden = v !== "text"; tilesHolder.hidden = v !== "tiles";
      if (v === "tiles" && !T) { T = scriptTileFrame(mod); tilesHolder.replaceChildren(T.holder); }
    };
    wrap.show();
    return wrap;
  };
  // (a read-only box laid out to its width too, as an editor is (579): again when its width changes, and on a setting's change;
  // its line numbers take 5 columns)
  const roPre = (cls, lang, text) => {
    const p = el("pre", { class: cls, tabindex: "0" });
    let cols = -1;
    p.flow = () => { const n = p.clientWidth ? scriptCols(p) - (scriptEditorSettings().lines ? 5 : 0) : 0; if (n === cols) return; cols = n; scriptHighlight(p, lang, n ? scriptFlow(lang, text, n) : text); scriptMaxNavSoon(); };
    p.reflow = () => { cols = -1; p.flow(); };
    p.flow();
    if (typeof ResizeObserver === "function") new ResizeObserver(() => p.flow()).observe(p);
    setTimeout(() => p.flow(), 50);
    return p;
  };
  const typesPre = () => roPre("script-js script-types", "hazel", SCRIPT_PREAMBLE.trim());
  const jsPre = text => roPre("script-js", "js", text);
  // (ticket 566) a chip per engine; on, the one shown (and, for a room, the one that runs)
  const chips = (now, pick, cls) => {
    const bs = SCRIPT_ENGINES.map(([v, lab]) => [v, el("button", { "data-engine": v, "aria-pressed": String(now() === v), onclick: () => pick(v) }, scriptEngineLabel(v, lab))]);
    const row = el("div", { class: "row script-engines " + cls }, bs.map(([, b]) => b));
    row.refresh = () => { for (const [v, b] of bs) setPressed(b, now() === v); };
    return row;
  };
  // (ticket 577) ⛶: this box at full size, with the outline of them all (scriptMaxOpen)
  // (the 📜 tiles first in its outline: Matthew, 2026-10-23, maxed 📚 Common's text box looking for them, "doesn't look like tiles")
  const maxBtn = sec => el("button", { class: "script-max-btn", title: "full size, with every box listed beside it", onclick: () => scriptMaxOpen([tiles, ...boxes], sec()) }, "⛶ max size");
  // a room's box: its chip, then its code in that engine. JS: the game's own functions, to read; Hazel and Fumola: to edit
  const roomBox = r => {
    const body = el("div", { class: "script-body" });
    let shown = null;
    const show = () => {
      const eng = shown = scriptEngine(r.name);
      if (eng === "js") body.replaceChildren(el("div", { class: "hint" }, "the game's own code, as it runs (to read: switch to Hazel or Fumola to write the rule)"),
        jsPre(scriptJsSource(r.name)));
      else if (eng === "hazel") body.replaceChildren(el("div", { class: "hint" }, `module ${r.name}, in Hazel`),
        hazelBody(r.name, editor(r.name, () => scriptsState().rooms[r.name], v => { scriptsState().rooms[r.name] = v; }, "hazel")));
      else body.replaceChildren(el("div", { class: "hint" }, `module ${r.name}, in Fumola (as text: ⚙️'s tiles are Hazel's; Fumola has none in the game yet)`),
        editor(r.name, () => scriptsState().fumola.rooms[r.name] || "", v => { scriptsState().fumola.rooms[r.name] = v; }, "fumola"));
    };
    const row = chips(() => scriptEngine(r.name), v => { const T = scriptsState(); T.engines[r.name] = v; if (v === "fumola") wantFumola(); refresh(); changed(); sync(); }, "script-room-engines");
    const sec = el("section", { class: "box script-box", "data-room": r.name }, [el("div", { class: "box-name" }, [r.label, maxBtn(() => sec)]), row, body]);
    sec.refresh = () => { row.refresh(); if (shown !== scriptEngine(r.name)) show(); else for (const ta of body.querySelectorAll("textarea.script-editor")) ta.refill(); };
    sec.shown = () => shown;
    show();
    return sec;
  };
  // 📚 Common: the helpers the Hazel or the Fumola rooms call; its chip only chooses which to show (the JS has none: each function
  // carries its own)
  const commonBox = () => {
    const body = el("div", { class: "script-body" });
    const view = () => { const v = scriptsState().commonView; return v === "hazel" || v === "fumola" || v === "js" ? v : uses("fumola") && !uses("hazel") ? "fumola" : "hazel"; };
    let shown = null;
    const show = () => {
      const v = shown = view();
      if (v === "js") body.replaceChildren(el("div", { class: "hint" }, "The game's JS has no common module: each room's functions carry what they need (switch a room to JS to read them)."));
      else if (v === "hazel") body.replaceChildren(
        // (Matthew, 2026-10-23: "we still show plain text first, before tiles") the module first, in tiles or text; then the sums
        // every room names, read-only (they are declared at the program's start, since the game's Hazel does not yet put a
        // module's constructors in scope outside it: hazelgrove/hazel#2729 does, on dev; once it does, they move into Common)
        // (Matthew, 2026-10-23: "the LHS should be ordered by the line number ordering of the RHS": the types first, as the program
        // has them, then the module; the outline follows the box from top to bottom. In tiles the types' text is hidden, and the
        // outline's type rows open the whole program's tiles)
        el("div", { class: "hint script-types-hint" }, "the types every Hazel room can name (read-only: at the program's start until the game's Hazel has hazelgrove/hazel#2729)"),
        typesPre(),
        el("div", { class: "hint" }, "module Common, in Hazel: in scope in every Hazel room, as Common.name"),
        hazelBody("Common", editor("Common", () => scriptsState().common, t => { scriptsState().common = t; }, "hazel")));
      else body.replaceChildren(el("div", { class: "hint" }, "module Common, in Fumola: in scope in every Fumola room, as Common.name (as text: ⚙️'s tiles are Hazel's; Fumola has none in the game yet)"),
        editor("Common", () => scriptsState().fumola.common || "", t => { scriptsState().fumola.common = t; }, "fumola"));
    };
    const row = chips(view, v => { scriptsState().commonView = v; if (v === "fumola") wantFumola(); show(); row.refresh(); scriptMaxNav(); changed(); }, "script-common-engines");
    const sec = el("section", { class: "box script-box", "data-room": "Common" }, [el("div", { class: "box-name" }, ["📚 Common", maxBtn(() => sec)]), row, body]);
    sec.refresh = () => { row.refresh(); if (shown !== view()) show(); else for (const ta of body.querySelectorAll("textarea.script-editor")) ta.refill(); };
    sec.shown = () => shown;
    show();
    return sec;
  };
  // the master chip: every room at once (its own chips forgotten), and 📚 Common shown in that engine; none on while rooms differ
  const allNow = () => { const es = new Set(SCRIPT_ROOMS.map(r => scriptEngine(r.name))); return es.size === 1 ? [...es][0] : "mixed"; };
  const master = chips(allNow, v => { const T = scriptsState(); T.all = v; T.engines = {}; T.on = v === "hazel"; T.commonView = v; if (v === "fumola") wantFumola(); refresh(); changed(); sync(); }, "script-master");
  const mixed = el("span", { class: "hint script-mixed" });
  // (ticket 580) ⚙️ the editor's settings, knolled: a name and its chips, row under row
  const E0 = scriptEditorSettings();
  const pickRow = (name, opts, now, pick) => {
    const bs = opts.map(([v, lab]) => [v, el("button", { "aria-pressed": String(now() === v), onclick: () => { pick(v); for (const [w, b] of bs) setPressed(b, now() === w); scriptEditorApply(); changed(); } }, lab)]);
    return [el("span", { class: "cap" }, name), el("div", { class: "row" }, bs.map(([, b]) => b))];
  };
  const wrapIn = el("input", { type: "number", min: "20", max: "400", step: "1", value: String(E0.wrap), "aria-label": "longest line before it is broken",
    onchange: e => { const n = Math.round(+e.target.value); if (n >= 20 && n <= 400) { scriptEditorSettings().wrap = n; scriptEditorApply(); changed(); } else e.target.value = scriptEditorSettings().wrap; } });
  wrapIn.addEventListener("keydown", e => e.stopPropagation());
  // (Matthew, 2026-10-23: "Just have a '?' chip for this text -- default is not shown") the panel's explanation, behind a ? chip
  const aboutText = el("div", { class: "hint script-about", hidden: "" }, "The game's logic, room by room, in one of three: the game's own JS (to read), or a rule you can write in Hazel or in Fumola. A rule's on_event(globals, event) gives what to do, as Cmd, each command with the rest after it: Flip(L, Done) flips the left lever. 📚 Common's helpers are in scope in every room of its language, as Common.name. A rule that has an error, or is not finished, leaves its room to the JS.");
  const aboutChip = el("button", { class: "script-about-chip", "aria-pressed": "false", "aria-label": "what this panel is", title: "what this panel is",
    onclick: () => { aboutText.hidden = !aboutText.hidden; setPressed(aboutChip, !aboutText.hidden); } }, "?");
  const settings = el("section", { class: "box script-settings" }, [
    el("div", { class: "box-name" }, ["⚙️ script editor settings", aboutChip]),
    aboutText,
    el("div", { class: "script-settings-grid" }, [
      el("span", { class: "cap" }, "all"), el("div", {}, [master, mixed]),
      ...pickRow("size", SCRIPT_SIZES.map(n => [n, String(n)]), () => scriptEditorSettings().size, v => { scriptEditorSettings().size = v; }),
      ...pickRow("font", SCRIPT_FONTS.map(([n]) => [n, n]), () => scriptEditorSettings().font, v => { scriptEditorSettings().font = v; }),
      ...pickRow("colour", [[true, "on"], [false, "off"]], () => scriptEditorSettings().colours, v => { scriptEditorSettings().colours = v; }),
      ...pickRow("hazel", [["tiles", scriptHazelTile("tiles")], ["text", "✏️ text"]], () => scriptEditorSettings().hazel, v => { scriptEditorSettings().hazel = v; }),
      ...pickRow("lines", [[true, "on"], [false, "off"]], () => scriptEditorSettings().lines, v => { scriptEditorSettings().lines = v; }),
      el("span", { class: "cap" }, "wrap"), el("div", { class: "row" }, [wrapIn]),
    ]),
  ]);
  scriptEditorApply();
  const boxes = [commonBox(), ...SCRIPT_ROOMS.map(roomBox)];
  const refresh = () => { master.refresh(); mixed.textContent = allNow() === "mixed" ? "mixed" : ""; for (const b of boxes) b.refresh(); scriptMaxNav(); };
  // (ticket 520) Hazel's tiles: made once the tab is laid out (scriptFrameMount)
  const tiles = el("section", { class: "box script-box script-tiles" }, [
    el("div", { class: "box-name" }, ["the ", scriptHazelTile(), " program, in its tiles", maxBtn(() => tiles)]),
    el("div", { class: "hint script-frame-status" }, "Hazel's tiles start once this tab is open."),
    el("ul", { class: "script-errors script-frame-errors" }),
    el("div", { class: "script-frame-wrap" }),
    el("div", { class: "hint" }, SCRIPT_TILES_EDIT ? "Type in the tiles: each edit goes to the Hazel boxes, and theirs to the tiles. A click on the game gives its keys back." : "For looking: the tiles show what the Hazel boxes hold, with Hazel's own marks where something is wrong; you write in the boxes above. A click in the tiles leaves the keys with the game."),
  ]);
  tiles.shown = () => "hazel"; tiles.refresh = () => {};
  // (578) an edit in the tiles: the boxes filled again from it, saved, and compiled a moment later (not posted back: scriptTilesEdit)
  scriptTilesEdited = () => { refresh(); changed(); clearTimeout(timer); timer = setTimeout(sync, 600); };
  box.append(
    ...(SCRIPT_LOGIC ? [el("div", { class: "hint" }, SCRIPT_LOGIC === "hazel" ? "This page was opened with ?logic=hazel: every room runs the shipped Hazel rules, whatever the chips or the boxes say."
      : SCRIPT_FUMOLA ? `This page was opened with ?logic=${SCRIPT_LOGIC}: every room runs the shipped Fumola rules, in Fumola's wasm build, whatever the chips or the boxes say.`
      : "This page was opened with ?logic=js: every room runs the game's own code, whatever the chips say.")] : []),
    settings,
    status, errs, fstatus, ferrs,
    ...boxes,
    tiles,
    el("details", { class: "script-preamble" }, [el("summary", {}, "what every Hazel module can name (the program's start)"), el("pre", {}, SCRIPT_PREAMBLE)]),
  );
  refresh();
  // (a 📂 load or a ↺ start over replaces state, and so state.scripts, with the tab open: each box and chip filled again from
  // the new one, before a keystroke can write the old box's text into it, and Hazel and the tiles told; per round, one compare)
  let filled = S;
  syncs.push(() => {
    if (state.scripts === filled) return;
    filled = scriptsState();
    refresh();
    clearTimeout(timer); sync();
  });
  // (after showTab: its layout moves the rows, and a frame moved in the page loads again; after the tab's first paint, too)
  requestAnimationFrame(() => setTimeout(() => scriptFrameMount(tiles), 0));
  // (and on a timer too: a page not painting has no animation frames, and the whole program's tiles, which a type in the outline
  // opens, waited for the first click on one, ~6 s more (the outline test, 2026-10-23); the mount does nothing a second time)
  setTimeout(() => scriptFrameMount(tiles), 400);
}

// (ticket 531) under ?logic=hazel, Hazel loaded from the start, so the first event is not the one that waits for it; and the
// program compiled as soon as it is (~780 ms on the runners), not in the first event: compiled there, a lever's pull stalled the
// page past startPull's timers, the pull's end and its step aside (leverAct) fired together, and the friend walked off the lever
if (SCRIPT_LOGIC === "hazel") hazelLoad().then(() => setTimeout(() => { try { scriptCompile(); } catch (e) {} }, 0)).catch(() => {});
