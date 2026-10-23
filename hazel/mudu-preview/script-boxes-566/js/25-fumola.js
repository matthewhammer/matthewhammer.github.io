"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 542, Matthew: "open a PR against dress-up that does the Fumola versions") The room rules of 25-scripting.js (ticket 531),
// written again in Fumola and run by Fumola's wasm build, beside the Hazel and the plain JS: ?logic=fumola (the World rule's map held
// in Fumola) and ?logic=fumola-list (the map passed in with each way, as Hazel's rule has it). Run after 25-scripting.js, whose
// scriptEvent asks fumolaCompile for the handlers under those two; the handlers have Hazel's signature, (globals, ev) -> cmd, with
// the same {tag, value} commands, so scriptRun and the rooms' acts are the same in every mode.
//   The boundary is text in, JSON out. Fumola's build takes only source text: the rules are defined once in an instance (its
// top-level modules persist), then each event is a call written as text, its globals and event as Fumola literals (fumolaLit:
// records { a = 1; b = "x" }, arrays [...], variants #Pull(#L), a tuple payload in its own parentheses); the answer comes back as
// JSON ({tag, value}: variants as {tag: "Variant", value: {name, value}}, numbers as strings), read by fumolaValue.
//   tools/fumola_rules_check.mjs runs this file in node against Fumola's wasm build: every case the rules can get, the same answers
// as the Hazel rules'. tools/checks/fumola542.mjs plays the same in a browser, against ?logic=js.

// Fumola's wasm build, pinned (Matthew, 2026-10-22: "actually pin it"): the two files fumola.org served on 2026-10-22, from
// Adapton/fumola's main at 2ec634b5df (its pages deploy of 2026-10-18), copied into vendor/fumola/2ec634b5df/, so a later
// fumola.org deploy cannot change the game's rules under it. To move to a newer build, copy its two files into a folder named by
// its commit and change this line. Relative to the page (index.html).
// (made absolute: an import() from this file resolves a relative path against js/, not against the page)
const FUMOLA_BUILD = typeof document !== "undefined" && typeof URL !== "undefined" && document.baseURI
  ? new URL("vendor/fumola/877b155851/", document.baseURI).href : "vendor/fumola/877b155851/";
const FUMOLA_JS_URL = FUMOLA_BUILD + "fumola_wasm.js", FUMOLA_WASM_URL = FUMOLA_BUILD + "fumola_wasm_bg.wasm";

// The rules, module by module, as 25-scripting.js has them in Hazel (SCRIPT_COMMON, SCRIPT_TRAP, ...), the same answers, in the
// same vocabulary: variants for directions, sides, wirings, square and room kinds, exits and way-out settings (#N, #L, #Random,
// #Waterfall, #Trap, #ExitLadder, #ToRoom(3); ticket 566, Languages of Play), the game's text turned into them at the boundary
// (scriptTyped). Since Fumola's semantics fixes (ticket 571; the build in vendor/fumola/877b155851/): text != is right, literal
// patterns of every kind, whole numbers mix (a step is (c + dx, r + dy), as Hazel's), Floats compare (the draws go in as they are).
// ASCII only (a non-ASCII character once hid everything after it from Fumola's reader).
const FUMOLA_COMMON = `module Common {
  // The game's vocabulary, the Hazel rules' types in Motoko's notation (Matthew, 2026-10-23: "use the hazel types to inform the
  // Fumola annotations, which are not enforced, but can be informative for both of us, and others"; "someday -- fumola will
  // enforce types like Motoko/Hazel does"). Fumola reads them and does not check them yet: a type is a comment it can parse.
  public type Dir = {#N; #NE; #E; #SE; #S; #SW; #W; #NW};
  public type Side = {#L; #R};
  public type Wiring = {#Normal; #Reversed; #Random};
  public type Out = {#Auto; #OutOfCastle; #Nowhere; #ToRoom : Int};
  public type Place = {
    #NoPlace; #Emptyroom; #Bubblespace; #Bubbleroom; #Imagination; #Forest; #Underwater; #Deepsea; #Beach; #Sea;
    #Witch; #Cemetery; #Lake; #House; #Farmyard; #Treehouse; #Treeroom; #Treetop; #Livingroom; #Witchroom;
    #Clearing; #Healerroom; #Fairyforest; #Mountains; #Waterfall; #Campsite; #Tent; #Cave; #Caveroom; #Woodpath;
    #Pasture; #Crossing; #Castle; #Castleroom; #Hut; #Hutroom; #Hutbed; #Hutpotion; #Hutcenter; #Desert
  };
  public type Room = {#King; #Greenhouse; #Trap; #Dungeon1; #Dungeon2; #Hallway; #Catroom; #Stable; #Dining; #Kitchen; #Smithy; #Spiral};
  public type Exit = {#ExitSide; #ExitLong; #ExitSpiral; #ExitTrap; #ExitLadder; #ExitAll; #ExitHall};
  // what a rule says to do, each command with the rest after it
  public type Cmd = {
    #Done; #Flip : (Side, Cmd); #Say : (Text, Cmd); #SetPit : (Int, Bool, Cmd); #DropBoulder : (Int, Cmd);
    #LiftBoulder : (Int, Cmd); #SetHatch : (Int, Bool, Cmd); #OpenDoor : (Int, Bool, Cmd); #DropRock : (Int, Bool, Cmd);
    #Wait : (Int, Cmd); #GoRoom : (Int, Cmd); #GoSquare : (Int, Int, Dir, Cmd)
  };
  public type Event = {#Pull : Side; #PullBoulder : Side; #PullHatch : Int; #PullThing : Int; #FallPit : Int; #WayOut : Int; #Way : Dir};
  public type Thing = {at : Int; uid : Text; kind : Text; on : Bool; works : [Text]; does : Text; open : Bool; boulder : Bool};
  public type Cell = {col : Int; row : Int; kind : Place; ways : [Dir]};
  public type Globals = {
    lever_L : Bool; lever_R : Bool; wiring : Wiring; draws : [Float]; pits_on : [Bool]; covered : [Bool]; hatch : Bool;
    things : [Thing]; rooms : [Room]; exits : [Exit]; out : Out; at_col : Int; at_row : Int; cols : Int; rows : Int; cells : [Cell]
  };
  // two levers flipped, one after the other
  public func flip2(a : Side, b : Side) : Cmd { #Flip(a, #Flip(b, #Done)) };
  // the i-th of a list of Bools, false past its end
  public func nth(xs : [Bool], i : Int) : Bool { if (i < xs.size()) { xs[i] } else { false } };
  // which lever works each pit, the left and the right: normal, reversed, or at random by the game's two draws
  public func wire(mode : Wiring, draws : [Float]) : (Side, Side) {
    switch (mode) {
      case (#Reversed) { (#R, #L) };
      case (#Random) { if (draws.size() < 2) { (#L, #R) } else { (if (draws[0] < 0.5) { #L } else { #R }, if (draws[1] < 0.5) { #L } else { #R }) } };
      case _ { (#L, #R) }
    }
  };
  // a red lever up? (true is up)
  public func up(l : Bool, r : Bool, side : Side) : Bool { switch (side) { case (#L) { l }; case _ { r } } };
  // each pit open: open when the lever working it is up
  public func pits_open(l : Bool, r : Bool, w : (Side, Side)) : (Bool, Bool) { let (w0, w1) = w; (up(l, r, w0), up(l, r, w1)) };
  // a pit that changed from was to now: SetPit, else nothing
  public func pit_moves(i : Int, was : Bool, now : Bool, k : Cmd) : Cmd { if (was == now) { k } else { #SetPit(i, now, k) } };
  public func member(s : Text, xs : [Text]) : Bool { var found = false; for (x in xs.vals()) { if (x == s) { found := true } }; found };
  // a way among a square's ways
  public func has_way(d : Dir, ds : [Dir]) : Bool { var found = false; for (x in ds.vals()) { if (x == d) { found := true } }; found };
  // where room kind k first is in a castle's rooms, counting from 1; 0 when it is not there
  public func room_at(rs : [Room], k : Room) : Int { var i = 0; var at = 0; for (x in rs.vals()) { i := i + 1; if (at == 0 and x == k) { at := i } }; at };
  // the i-th room's exits, counting from 1; null past the end
  public func exit_of(xs : [Exit], i : Int) : ?Exit { if (i >= 1 and i <= xs.size()) { ?(xs[i - 1]) } else { null } };
  public func rooms(rs : [Room]) : Int { rs.size() };
  // the world's eight directions: each one's step across and down the map, and the way back
  public func dir(d : Dir) : (Int, Int) {
    switch (d) {
      case (#N) { (0, -1) }; case (#NE) { (1, -1) }; case (#E) { (1, 0) }; case (#SE) { (1, 1) };
      case (#S) { (0, 1) }; case (#SW) { (-1, 1) }; case (#W) { (-1, 0) }; case _ { (-1, -1) }
    }
  };
  public func opp(d : Dir) : Dir {
    switch (d) {
      case (#N) { #S }; case (#NE) { #SW }; case (#E) { #W }; case (#SE) { #NW };
      case (#S) { #N }; case (#SW) { #NE }; case (#W) { #E }; case _ { #SE }
    }
  }
}`;

const FUMOLA_TRAP = `module TrapRoom {
  // a blue lever's pit i: nothing if not its own, else its rock lifted or a rock dropped
  func one(i : Int, mine : Bool, lift : Bool, k : Common.Cmd) : Common.Cmd { if (not mine) { k } else if (lift) { #LiftBoulder(i, k) } else { #DropBoulder(i, k) } };
  // A red lever pulled: flip it, and each pit wired to it opens or shuts.
  // A blue lever pulled: each of its pits gets a boulder, or loses one.
  public func on_event(g : Common.Globals, e : Common.Event) : Common.Cmd {
    let w = Common.wire(g.wiring, g.draws);
    let (w0, w1) = w;
    switch (e) {
      case (#Pull(side)) {
        let (was0, was1) = Common.pits_open(g.lever_L, g.lever_R, w);
        let l = if (side == #L) { not g.lever_L } else { g.lever_L };
        let r = if (side == #R) { not g.lever_R } else { g.lever_R };
        let (now0, now1) = Common.pits_open(l, r, w);
        #Flip(side, Common.pit_moves(0, was0, now0, Common.pit_moves(1, was1, now1, #Done)))
      };
      case (#PullBoulder(side)) {
        let mine0 = (w0 == side) and Common.nth(g.pits_on, 0);
        let mine1 = (w1 == side) and Common.nth(g.pits_on, 1);
        let lift = (mine0 and Common.nth(g.covered, 0)) or (mine1 and Common.nth(g.covered, 1));
        one(0, mine0, lift, one(1, mine1, lift, #Done))
      };
      case _ { #Done }
    }
  }
}`;

const FUMOLA_TREE = `module TreeRoom {
  // A tree room's trap-door lever, or the round room's at the top (house 0):
  // the trap door shuts if open, opens if shut.
  public func on_event(g : Common.Globals, e : Common.Event) : Common.Cmd {
    switch (e) {
      case (#PullHatch(n)) { #SetHatch(n, not g.hatch, #Done) };
      case _ { #Done }
    }
  }
}`;

const FUMOLA_SQUARE = `module Square {
  // the first thing at place i, from the j-th on
  func find(ts : [Common.Thing], j : Int, i : Int) : ?Common.Thing { if (j == ts.size()) { null } else if (ts[j].at == i) { ?(ts[j]) } else { find(ts, j + 1, i) } };
  // each trap door the lever works, from the j-th thing on, in order: it swings, or, a boulder lever, gets its rock or loses it
  func doors(lever : Common.Thing, ts : [Common.Thing], j : Int) : Common.Cmd {
    if (j == ts.size()) { #Done }
    else {
      let d = ts[j];
      let k = doors(lever, ts, j + 1);
      if (not (d.kind == "trapdoor" and Common.member(d.uid, lever.works))) { k }
      else if (lever.does == "boulder") { if (d.boulder == (not lever.on)) { k } else { #DropRock(d.at, not lever.on, k) } }
      else { #OpenDoor(d.at, not d.open, k) }
    }
  };
  // A placed lever pulled: each trap door wired to it swings,
  // or, a boulder lever, gets its rock or loses it.
  public func on_event(g : Common.Globals, e : Common.Event) : Common.Cmd {
    switch (e) {
      case (#PullThing(i)) { switch (find(g.things, 0, i)) { case (?t) { doors(t, g.things, 0) }; case null { #Done } } };
      case _ { #Done }
    }
  }
}`;

// (two ways to have the map: on_event takes it in each call, as Hazel's rule does, ?logic=fumola-list; way takes only where the
// friend is, the map's size and the way, and finds the square in the map held here, a HashMap keyed by (col, row), made again by
// hold whenever the game's cells change, ?logic=fumola)
const FUMOLA_WORLD = `var world_held = HashMap.new();
module World {
  // Places with no way out on a side, whatever their ways say: the waterfall's N, its cliff.
  func no_way(kind : Common.Place, d : Common.Dir) : Bool { kind == #Waterfall and d == #N };
  // into square (c, r), come in from back, if it has a way back; else nowhere
  func decide(x : Common.Cell, back : Common.Dir, c : Int, r : Int) : Common.Cmd { if (Common.has_way(back, x.ways) and not (no_way(x.kind, back))) { #GoSquare(c, r, back, #Done) } else { #Done } };
  func find_at(cs : [Common.Cell], i : Int, c : Int, r : Int) : ?Common.Cell { if (i == cs.size()) { null } else { let x = cs[i]; if (x.col == c and x.row == r) { ?x } else { find_at(cs, i + 1, c, r) } } };
  // the square a way leads to, if it is on the map: (c, r), or null
  func step(ac : Int, ar : Int, d : Common.Dir, cols : Int, rows : Int) : ?(Int, Int) {
    let (dx, dy) = Common.dir(d);
    let c = ac + dx;
    let r = ar + dy;
    if (c < 0 or r < 0 or c >= cols or r >= rows) { null } else { ?(c, r) }
  };
  // A way out walked through: into the square beside it, coming in from the side facing back,
  // if that square is on the map and has a way back; else nowhere.
  public func on_event(g : Common.Globals, e : Common.Event) : Common.Cmd {
    switch (e) {
      case (#Way(d)) {
        switch (step(g.at_col, g.at_row, d, g.cols, g.rows)) {
          case (?(c, r)) { switch (find_at(g.cells, 0, c, r)) { case (?x) { decide(x, Common.opp(d), c, r) }; case null { #Done } } };
          case null { #Done }
        }
      };
      case _ { #Done }
    }
  };
  // the same, the square found in the map held here
  public func way(ac : Int, ar : Int, cols : Int, rows : Int, d : Common.Dir) : Common.Cmd {
    switch (step(ac, ar, d, cols, rows)) {
      case (?(c, r)) { switch (HashMap.get(world_held, (c, r))) { case (?x) { decide(x, Common.opp(d), c, r) }; case null { #Done } } };
      case null { #Done }
    }
  };
  // the map held here made again from the game's cells
  public func hold(cells : [Common.Cell]) : Int {
    var m = HashMap.new();
    for (x in cells.vals()) { let (m2, _) = HashMap.put(m, (x.col, x.row), x); m := m2 };
    world_held := m;
    cells.size()
  }
}`;

const FUMOLA_CASTLE = `module Castle {
  // Where room n's way out leads: 0 out of the castle, a room, or -1 nowhere.
  // out is its setting; Auto: a dungeon's ladder up to the trap room, room 1 out, any other to room 1.
  // Nowhere only where the way out is toward us (side doors, a long room's, a spiral's); elsewhere Auto.
  public func way_out(rooms : [Common.Room], exits : [Common.Exit], n : Int, out : Common.Out) : Int {
    let ex = Common.exit_of(exits, n);
    let toward_us = ex == ?(#ExitSide) or ex == ?(#ExitLong) or ex == ?(#ExitSpiral);
    let v = if (out == #Nowhere and not toward_us) { #Auto } else { out };
    switch (v) {
      case (#Auto) { let t = Common.room_at(rooms, #Trap); if (ex == ?(#ExitLadder) and t > 0 and t != n) { t } else if (n == 1) { 0 } else { 1 } };
      case (#OutOfCastle) { 0 };
      case (#Nowhere) { -1 };
      case (#ToRoom(r)) { if (r >= 1 and r <= Common.rooms(rooms)) { r } else { -1 } };
      case _ { -1 }
    }
  };
  // A trap room's pit fallen into (0 the left, 1 the right):
  // the first pit drops into the dungeon, the second into the deep dungeon;
  // a castle with only one of them, both into that one.
  public func on_event(g : Common.Globals, e : Common.Event) : Common.Cmd {
    let d1 = Common.room_at(g.rooms, #Dungeon1);
    let d2 = Common.room_at(g.rooms, #Dungeon2);
    switch (e) {
      case (#FallPit(i)) {
        let to = if (i == 0) { if (d1 > 0) { d1 } else { d2 } } else { if (d2 > 0) { d2 } else { d1 } };
        if (to == 0) { #Done } else { #GoRoom(to, #Done) }
      };
      case (#WayOut(n)) { #GoRoom(way_out(g.rooms, g.exits, n, g.out), #Done) };
      case _ { #Done }
    }
  }
}`;

// (2026-10-23) the texts as shipped before their types (ticket 566's): a save holding one of them, unchanged, gets the typed one
// (FUMOLA_UNTYPED: the shipped text with its type block and each function's annotations taken out, so a save is compared with that)
const FUMOLA_UNTYPED_HEADS = [["flip2(a : Side, b : Side) : Cmd", "flip2(a, b)"], ["nth(xs : [Bool], i : Int) : Bool", "nth(xs, i)"], ["wire(mode : Wiring, draws : [Float]) : (Side, Side)", "wire(mode, draws)"], ["up(l : Bool, r : Bool, side : Side) : Bool", "up(l, r, side)"], ["pits_open(l : Bool, r : Bool, w : (Side, Side)) : (Bool, Bool)", "pits_open(l, r, w)"], ["pit_moves(i : Int, was : Bool, now : Bool, k : Cmd) : Cmd", "pit_moves(i, was, now, k)"], ["member(s : Text, xs : [Text]) : Bool", "member(s, xs)"], ["has_way(d : Dir, ds : [Dir]) : Bool", "has_way(d, ds)"], ["room_at(rs : [Room], k : Room) : Int", "room_at(rs, k)"], ["exit_of(xs : [Exit], i : Int) : ?Exit", "exit_of(xs, i)"], ["rooms(rs : [Room]) : Int", "rooms(rs)"], ["dir(d : Dir) : (Int, Int)", "dir(d)"], ["opp(d : Dir) : Dir", "opp(d)"], ["one(i : Int, mine : Bool, lift : Bool, k : Common.Cmd) : Common.Cmd", "one(i, mine, lift, k)"], ["on_event(g : Common.Globals, e : Common.Event) : Common.Cmd", "on_event(g, e)"], ["find(ts : [Common.Thing], j : Int, i : Int) : ?Common.Thing", "find(ts, j, i)"], ["doors(lever : Common.Thing, ts : [Common.Thing], j : Int) : Common.Cmd", "doors(lever, ts, j)"], ["no_way(kind : Common.Place, d : Common.Dir) : Bool", "no_way(kind, d)"], ["decide(x : Common.Cell, back : Common.Dir, c : Int, r : Int) : Common.Cmd", "decide(x, back, c, r)"], ["find_at(cs : [Common.Cell], i : Int, c : Int, r : Int) : ?Common.Cell", "find_at(cs, i, c, r)"], ["step(ac : Int, ar : Int, d : Common.Dir, cols : Int, rows : Int) : ?(Int, Int)", "step(ac, ar, d, cols, rows)"], ["way(ac : Int, ar : Int, cols : Int, rows : Int, d : Common.Dir) : Common.Cmd", "way(ac, ar, cols, rows, d)"], ["hold(cells : [Common.Cell]) : Int", "hold(cells)"], ["way_out(rooms : [Common.Room], exits : [Common.Exit], n : Int, out : Common.Out) : Int", "way_out(rooms, exits, n, out)"]];
const FUMOLA_UNTYPED = text => {
  let t = text.replace(/  \/\/ The game's vocabulary[\s\S]*?\n  \};\n(?=  \/\/ two levers)/, "");
  for (const [a, b] of FUMOLA_UNTYPED_HEADS) t = t.split("func " + a + " {").join("func " + b + " {");
  return t;
};
// the whole program, defined once in an instance; and each room's module (SCRIPT_ROOMS' names)
const FUMOLA_SOURCE = [FUMOLA_COMMON, FUMOLA_TRAP, FUMOLA_TREE, FUMOLA_SQUARE, FUMOLA_WORLD, FUMOLA_CASTLE].join(";\n");
const FUMOLA_ROOMS = ["TrapRoom", "TreeRoom", "Square", "World", "Castle"];
// (ticket 566) each room's text by its module's name, for the 📜 panel's Fumola boxes (25-scripting.js keeps them, edited, in
// state.scripts.fumola), and the program the boxes make: 📚 Common, then the rooms in this order. Under ?logic=fumola or
// fumola-list, the shipped rules, whatever the boxes hold, as ?logic=hazel runs the shipped Hazel
const FUMOLA_ROOM_TEXTS = { TrapRoom: FUMOLA_TRAP, TreeRoom: FUMOLA_TREE, Square: FUMOLA_SQUARE, World: FUMOLA_WORLD, Castle: FUMOLA_CASTLE };
function fumolaSource() {
  if (typeof SCRIPT_FUMOLA === "undefined" || SCRIPT_FUMOLA || typeof scriptsState !== "function") return FUMOLA_SOURCE;
  const F = scriptsState().fumola;
  return [F.common, ...FUMOLA_ROOMS.map(r => F.rooms[r] || "")].map(t => String(t || "").replace(/;\s*$/, "").trim()).filter(Boolean).join(";\n");
}

// A JS value as a Fumola literal: true/false; a whole number (a Nat, or an Int below 0); a text, quoted, which may
// hold no quote, backslash or character outside printable ASCII (Fumola's text literals have no escapes; the game's kinds, uids,
// rooms and ways are plain); an array as [...]; {tag, value} as a variant, #Tag, #Tag(x), or #Tag((a, b)) for an array; any other
// object as a record { name = value; ... }. Anything else throws, and the rule stops (the caller's JS goes on, as for Hazel)
const FUMOLA_PLAIN = /^[ !#-[\]-~]*$/;
function fumolaLit(v) {
  if (v === true || v === false) return String(v);
  if (typeof v === "number") {
    if (Number.isInteger(v)) return String(v);
    // (ticket 571: a Float, written with a point and no exponent, which Fumola's reader takes)
    if (Number.isFinite(v)) { const t = v.toFixed(17).replace(/0+$/, ""); return t.endsWith(".") ? t + "0" : t; }
    throw new Error(`a number Fumola is not given here: ${v}`);
  }
  if (typeof v === "string") {
    if (!FUMOLA_PLAIN.test(v)) throw new Error(`a text Fumola cannot be given: ${JSON.stringify(v).slice(0, 40)}`);
    return `"${v}"`;
  }
  if (Array.isArray(v)) return "[" + v.map(fumolaLit).join(", ") + "]";
  if (v && typeof v === "object") {
    const ks = Object.keys(v);
    if (typeof v.tag === "string" && ks.every(k => k === "tag" || k === "value")) {
      if (!/^[A-Za-z]\w*$/.test(v.tag)) throw new Error(`a variant Fumola cannot be given: ${v.tag}`);
      return v.value === undefined ? `#${v.tag}` : Array.isArray(v.value) ? `#${v.tag}((${v.value.map(fumolaLit).join(", ")}))` : `#${v.tag}(${fumolaLit(v.value)})`;
    }
    return "{ " + ks.map(k => { if (!/^[A-Za-z_]\w*$/.test(k)) throw new Error(`a field Fumola cannot be given: ${k}`); return `${k} = ${fumolaLit(v[k])}`; }).join("; ") + " }";
  }
  throw new Error(`a value Fumola is not given: ${String(v).slice(0, 40)}`);
}

// Fumola's answer (its JSON, parsed) as a JS value: a variant as {tag} or {tag, value}, a tuple's payload as an array (so a command
// is the shape Hazel's are: {tag: "Flip", value: ["lever_L", {tag: "Done"}]}), numbers as numbers, a record as an object
function fumolaValue(v) {
  switch (v && v.tag) {
    case "Variant": { const p = v.value.value; return p == null ? { tag: v.value.name } : { tag: v.value.name, value: fumolaValue(p) }; }
    case "Tuple": case "List": return v.value.map(fumolaValue);
    case "Int": case "Float": return Number(v.value);
    case "Bool": case "String": return v.value;
    case "Record": return Object.fromEntries(Object.entries(v.value).map(([k, x]) => [k, fumolaValue(x)]));
    case "Option": return fumolaValue(v.value);
    case "Null": case "Unit": return null;
    default: throw new Error(`an answer from Fumola the game cannot read: ${JSON.stringify(v).slice(0, 80)}`);
  }
}

// a room's globals as Fumola is given them: as Hazel's are (since ticket 571, Floats compare: the draws need no turning into Nats)
const fumolaGlobals = g => g;

// The rules in a new instance of the wasm build F (its exports: fumola_create, fumola_eval_top, fumola_eval_scratch), and each room's
// handler, (globals, ev) -> cmd. A call runs in scratch (fumola_eval_scratch: its answer only, the instance left as it was), where
// the build has it; defining the rules and holding the map run at the top level (fumola_eval_top), and stay. mode: "fumola" (the
// World rule's map held in the instance, made again when the cells change: their text is the signature) or "fumola-list" (the
// map passed in each call). An answer that is not ok throws, with Fumola's words.
function fumolaRules(F, mode = "fumola", source = FUMOLA_SOURCE) {
  const now = () => (typeof performance === "object" ? performance.now() : Date.now());
  const id = F.fumola_create(), scratch = typeof F.fumola_eval_scratch === "function";
  const run = (text, keep) => {
    const r = JSON.parse(keep || !scratch ? F.fumola_eval_top(id, text) : F.fumola_eval_scratch(id, text));
    if (!r || r.ok !== true) throw new Error(`Fumola: ${r && (r.error || r.kind) || "no answer"}`);
    return r;
  };
  const t0 = now();
  run(source, true);
  const ms = now() - t0;
  const call = text => fumolaValue(run(text, false));
  const on = room => (g, ev) => call(`${room}.on_event(${fumolaLit(fumolaGlobals(g))}, ${fumolaLit(ev)})`);
  const handlers = Object.fromEntries(FUMOLA_ROOMS.map(r => [r, on(r)]));
  const held = { text: null, builds: 0 };
  if (mode !== "fumola-list") handlers.World = (g, ev) => {
    if (!ev || ev.tag !== "Way") return { tag: "Done" };
    const cells = fumolaLit(g.cells);
    if (cells !== held.text) { run(`World.hold(${cells})`, true); held.text = cells; held.builds++; }
    return call(`World.way(${fumolaLit(g.at_col)}, ${fumolaLit(g.at_row)}, ${fumolaLit(g.cols)}, ${fumolaLit(g.rows)}, ${fumolaLit(ev.value)})`);
  };
  // (ticket 566: the rules speak in variants; the game's text converted on the way in and back, as for Hazel's: scriptTyped, in
  // 25-scripting.js, loaded with this file in the page and in the tools)
  if (typeof scriptTyped === "function") for (const r of Object.keys(handlers)) handlers[r] = scriptTyped(handlers[r]);
  return { id, mode, ms, scratch, handlers, held, call, run };
}

// ---- in the page: the build loaded once (under ?logic=fumola or fumola-list only), the rules compiled once ----
// (fumolaLoadedAt: when it was ready, in ms from the page's start, for the checks to read)
let fumolaLoading = null, fumolaBuild = null, fumolaLoadedAt = null;
function fumolaLoad() {
  if (fumolaBuild) return Promise.resolve(fumolaBuild);
  if (!fumolaLoading) fumolaLoading = import(FUMOLA_JS_URL)
    .then(async m => { await m.default({ module_or_path: FUMOLA_WASM_URL }); fumolaBuild = m; fumolaLoadedAt = performance.now(); return m; })
    .catch(e => { fumolaLoading = null; throw e; });
  return fumolaLoading;
}
const fumolaReady = () => !!fumolaBuild;
// (scriptCompile's shape: ready, handlers, errors, ms; once per text, as Hazel's: ticket 566's boxes can change it. A new text gets a
// new instance, and the old one is dropped)
let fumolaCompiled = null;
function fumolaCompile() {
  if (!fumolaBuild) return null;
  const source = fumolaSource();
  if (fumolaCompiled && fumolaCompiled.source === source) return fumolaCompiled;
  if (fumolaCompiled && fumolaCompiled.rules && typeof fumolaBuild.fumola_drop === "function") try { fumolaBuild.fumola_drop(fumolaCompiled.rules.id); } catch (e) {}
  try {
    const R = fumolaRules(fumolaBuild, SCRIPT_LOGIC === "fumola-list" ? "fumola-list" : "fumola", source);
    fumolaCompiled = { via: "fumola", ready: true, errors: [], unfinished: [], handlers: R.handlers, ms: R.ms, rules: R, source };
  } catch (e) { fumolaCompiled = { via: "fumola", ready: false, errors: [String(e && e.message || e)], unfinished: [], handlers: {}, ms: 0, source }; }
  hzPush(hzCounters().compileMs, fumolaCompiled.ms);
  return fumolaCompiled;
}

// (as Hazel's under ?logic=hazel: loaded from the start, and the rules defined as soon as it is, not in the first event)
if (typeof SCRIPT_FUMOLA !== "undefined" && SCRIPT_FUMOLA) fumolaLoad().then(() => setTimeout(() => { try { fumolaCompile(); } catch (e) {} }, 0)).catch(() => {});
