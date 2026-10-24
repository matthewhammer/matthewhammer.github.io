"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 20 of 26 of the game's script, run in order by index.html.

// ===================== CONTROLS =====================

// a witch's green skin
const WITCH_SKIN = "#6fa34a";
// the skin swatches that mean something, marked so: what they mean
const SKIN_MARKS = { "#cfa000": ["😀", "smiley yellow"], "#aab2ba": ["👽", "alien grey"], "#dc143c": ["😡", "mad red"], "#8cbe32": ["🤢", "sick green"],
                     [WITCH_SKIN]: ["🧙", "witch green"] };
const SKINS = ["#cfa000", "#dc143c", "#8cbe32", "#aab2ba", "#ffdbac", "#f1c27d", "#e0ac69", "#c68642", "#8d5524", WITCH_SKIN];
const BRIGHT = ["#e74c3c", "#ff69b4", "#f39c12", "#f1c40f", "#2ecc71", "#5b8bd0", "#34495e", "#8e44ad", "#ffffff", "#2b2b2b"];
const HAIRS = ["#2b2b2b", "#6f4e37", "#e8c35a", "#b5562b", "#ff69b4", "#3b82f6", "#22c55e", "#8b5cf6"];
const PATTERN_COLORS = ["auto", "#ffffff", "#ff69b4", "#f1c40f", "#e74c3c", "#2ecc71", "#5b8bd0", "#2b2b2b"];
const FURS = ["#e8862a", "#1f1f22", "#d9a441", "#6b4226", "#55616d", "#ffffff", "#9ca3af", "#ff69b4", "#8b5cf6"];
const MARKS = ["#1f1a17", "#3a3a40", "#8a4b1c", "#4a2c17", "#23282e", "#ffffff", "#ff69b4"];
const BELLIES = ["#f7ead8", "#2e2e33", "#f1d9a8", "#b08458", "#f2ece0", "#ffffff", "#ffd1dc"];
const ANIMAL_EYES = ["#e0a800", "#c6d43a", "#c88a1e", "#2b1a10", "#1a1410", "#3b82f6", "#22c55e"];
const ACCENTS = ["#e89aa0", "#f4a0b0", "#5a3a2a", "#1d1410", "#f2c230", "#ff69b4"];
const PATTERNS = [["plain", "plain"], ["stripes", "stripes"], ["dots", "dots"], ["hearts", "♥ hearts"], ["stars", "★ stars"]];

// the friend's right hand: its own style and settings, or the same as the
// other's (the color and the size are always both hands')
const rightHandOf = h => (h.different ? { ...h, ...state.rightHand } : h);
// and the friend's right foot
const rightShoeOf = sh => (sh.different ? { ...sh, ...state.rightShoe } : sh);
const FOOT_STYLES = [["sneaker", "sneakers"], ["boot", "boots"], ["clown", "clown shoes"], ["bare", "bare feet"],
                     ["human", "🦶 toes"], ["alien", "👽 alien toes"], ["talons", "🦅 talons"]];
// one foot's own controls: its style, and the toe counts
const SHOE_CTLS = [
  ["choice", "style", "style", FOOT_STYLES],
  ["range", "toes", "🦶 number of toes", 1, 100, 4, 6],
  ["range", "alienToes", "👽 number of alien toes", 1, 100, 2, 6],
  ["range", "talonCount", "🦅 number of talons", 1, 100, 3, 6],
];
const FOOT_SLIDER_STYLE = { toes: "human", alienToes: "alien", talonCount: "talons" };
const HAND_STYLES = [["round", "round"], ["mitten", "mittens"], ["star", "stars"], ["claw", "claws"],
                     ["human", "✋ human"], ["alien", "👽 alien"]];
// one hand's own controls: its style, and the sliders for each style
const HAND_CTLS = [
  ["choice", "style", "style", HAND_STYLES],
  ["range", "fingers", "✋ number of fingers", 1, 100, 5, 7],
  ["range", "alienFingers", "👽 number of alien fingers", 1, 100, 2, 6],
  ["range", "starPoints", "⭐ number of star points", 1, 100, 4, 8],
  ["range", "clawCount", "🐾 number of claws", 1, 100, 3, 6],
  ["range", "clawLen", "🐾 claw length", 20, 900, 70, 140],
  ["turn", "starAngle", "⭐ star angle (radians)", handPeriod],
  ["turn", "mittenAngle", "🧤 mitten angle (radians)", handPeriod],
  ["turn", "clawAngle", "🐾 claw angle (radians)", handPeriod],
  ["turn", "humanAngle", "✋ human hand angle (radians)", handPeriod],
  ["turn", "alienAngle", "👽 alien hand angle (radians)", handPeriod],
];
// the style each of a hand's sliders is for
const SLIDER_STYLE = { fingers: "human", alienFingers: "alien", starPoints: "star", clawCount: "claw", clawLen: "claw",
                       starAngle: "star", mittenAngle: "mitten", clawAngle: "claw", humanAngle: "human", alienAngle: "alien" };
// a cat costume's paws have claws, set with the claw sliders
const DIVING_SUITS = new Set(["scuba", "diver"]);   // (ticket 371)
const clawedSuit = () => !!CLAWED[state.costume.kind] && !!costumeOn();
// a hand's slider shows when the hand wears its style
const handSliderShown = (key, h) => h.style === SLIDER_STYLE[key] || ((key === "clawLen" || key === "clawCount") && clawedSuit());
// The places, and the kinds of place they are, for the Background tab's
// two rows (as the music's): every place is of exactly one kind.
// (ticket 490) the place's time of day is by the clock (🌍 where and when shows)
// (the review after 0.0.385) night, as chosen or by the clock (the stars are drawn either way)
const nightBy = (t, b, sunrise) => t === "night" || (t === "clock" && skyTimeOf(b, sunrise) === "night");
const clockHere = b => ((b.kind === "waterfall" || b.kind === "cave") ? b.fallTime : (b.kind === "campsite" || b.kind === "tent") ? b.campTime : b.kind === "fairyforest" ? b.fairyTime : null) === "clock";
const PLACES = [["none", "none"], ["emptyroom", "🔲 empty room"], ["bubblespace", "🫧 bubble space"], ["bubbleroom", "🫧 the bubble room"], ["imagination", "💭 our imagination"], ["forest", "🌲 forest"], ["underwater", "🐠 underwater"], ["deepsea", "🪸 deep sea"],
  ["beach", "🏖️ beach"], ["sea", "🌊 open water"], ["witch", "🧙 witch's woods"],
  ["cemetery", "🌿 cemetery"], ["lake", "🦆 lake"], ["house", "🏠 house and yard"], ["farmyard", "🚜 farm yard"],   // (farmyard: ticket 502, for M)
  ["treehouse", "🌳 tree house"], ["treeroom", "🪜 inside the tree house"], ["treetop", "⭕ the round room at the top of the tree"],   // (treetop: ticket 516)
  ["livingroom", "🛋️ living room"], ["witchroom", "🧙 inside the witch's hut"],
  ["clearing", "🌲 forest clearing"], ["healerroom", "🌿 inside the healer's hut"], ["fairyforest", "🧚 fairy forest"], ["mountains", "⛰️ mountain trail"],
  ["waterfall", "💦 jungle waterfall"], ["campsite", "🏕️ camp site"],
  ["tent", "⛺ inside a tent"], ["cave", "🪨 cave behind the falls"], ["caveroom", "🦇 cave room"],
  ["woodpath", "🥾 path through the woods"], ["pasture", "🌾 pasture"], ["crossing", "🛤️ forest crossing"], ["castle", "🏰 castle"], ["castleroom", "🚪 inside the castle"],
  // (ticket 278, for M) the grass hut, and its three rooms
  ["hut", "🛖 grass hut"], ["hutroom", "🛋️ inside the grass hut"], ["hutbed", "🛏️ the grass hut's bedroom"], ["hutpotion", "🧪 the grass hut's potion room"], ["hutcenter", "🔥 the grass hut's center"],
  // (ticket 501, for M) the desert
  ["desert", "🏜️ desert"]];
// (where the way toward us can lead from a room outside the castle: any place but those found by a number of their own)
const BEHIND_PLACES = [["none", "🧱 nowhere"], ...PLACES.filter(([kd]) => !["none", "castleroom", "treeroom"].includes(kd))];
const PLACE_GROUPS = [
  ["imagination", "💭 imagination", ["emptyroom", "imagination"]],
  // (ticket 290, for N: bubbles, a kind of place of its own: bubble space, moved here, and the bubble room)
  ["bubbles", "🫧 bubbles", ["bubblespace", "bubbleroom"]],
  ["woods", "🌲 woods", ["forest", "clearing", "healerroom", "fairyforest", "woodpath", "crossing", "mountains", "pasture"]],   // (ticket 503: the pasture, out in the open as the mountain trail is)
  ["water", "💧 water", ["lake", "beach", "sea", "underwater", "deepsea", "waterfall"]],
  ["camping", "🏕️ camping", ["campsite", "tent", "cave", "caveroom"]],
  ["home", "🏠 home", ["house", "livingroom", "treehouse", "treeroom", "treetop", "farmyard"]],   // (ticket 502: the 🚜 farm yard, outdoors with the house's yard)
  ["witch", "🧙 the witch's", ["witch", "witchroom", "cemetery"]],
  ["castles", "🏰 castles", ["castle", "castleroom"]],
  ["hut", "🛖 the grass hut", ["hut", "hutroom", "hutbed", "hutpotion", "hutcenter"]],
  ["desert", "🏜️ desert", ["desert"]],   // (ticket 501, for M: a kind of its own, as the bubbles are)
];
const groupOfPlace = v => v === "none" ? "none" : (PLACE_GROUPS.find(g => g[2].includes(v)) || [null])[0];
console.assert(PLACES.every(([v]) => groupOfPlace(v)) && PLACE_GROUPS.flatMap(g => g[2]).length === PLACES.length - 1,
               "every place is of exactly one kind");

// (ticket 221) the bubble themes' settings, in 🏞️ Background (bubble space) and 🎮 Controls (blowing bubbles) alike: the
// themes are the friend's bubbles wherever they are
const BUBBLE_THEME_CTLS = [
  ["group", "bubbleA", "🅰️ theme A", [
    ["color", "bubbleAColor", "🅰️ color", BUBBLE_COLORS],
    ["range", "bubbleASize", "🅰️ size", 4, 60, 10, 30],
    ["range", "bubbleAShine", "🅰️ shine", 0, 100, 40, 90],
    ["choice", "bubbleASparkle", "🅰️ sparkle", [["none", "none"], ["star", "⭐ star"], ["dot", "• dot"]]],
    ["color", "bubbleASparkleColor", "🅰️ sparkle color", BUBBLE_COLORS],
  ]],
  ["group", "bubbleB", "🅱️ theme B", [
    ["flip", "bubbleBOn", ["🅱️ theme B", "🅱️ theme B"]],
    ["color", "bubbleBColor", "🅱️ color", BUBBLE_COLORS],
    ["range", "bubbleBSize", "🅱️ size", 4, 60, 10, 30],
    ["range", "bubbleBShine", "🅱️ shine", 0, 100, 40, 90],
    ["choice", "bubbleBSparkle", "🅱️ sparkle", [["none", "none"], ["star", "⭐ star"], ["dot", "• dot"]]],
    ["color", "bubbleBSparkleColor", "🅱️ sparkle color", BUBBLE_COLORS],
  ]],
  ["group", "bubbleC", "©️ theme C", [
    ["flip", "bubbleCOn", ["©️ theme C", "©️ theme C"]],
    ["color", "bubbleCColor", "©️ color", BUBBLE_COLORS],
    ["range", "bubbleCSize", "©️ size", 4, 60, 10, 30],
    ["range", "bubbleCShine", "©️ shine", 0, 100, 40, 90],
    ["choice", "bubbleCSparkle", "©️ sparkle", [["none", "none"], ["star", "⭐ star"], ["dot", "• dot"]]],
    ["color", "bubbleCSparkleColor", "©️ sparkle color", BUBBLE_COLORS],
  ]],
];
const TABS = [
  // (ticket 129) whole poses, first (Matthew: top level, before the face)
  // (ticket 132: "Body", with poses, the way they face, and animations)
  ["🧍 Body", "body", [
    ["choice", "pose", "🤸 poses", [["stand", "🧍 standing"], ["sit", "🪑 sitting"], ["meditate", "🧘 cross-legged, meditating"], ["dance", "💃 dancing (a new dance each time)"], ["hug", "🤗 jumping up into a hug"], ["lie", "🛌 lying down"]]],
    ["choice", "lieHow", "🛌 lying", [["back", "🛌 on the back, face up"], ["side", "🛏️ on the side, facing us"]]],   // (ticket 299)
    ["choice", "hand", "✋ dominant hand (it pulls the levers)", [["right", "🫱 right"], ["left", "🫲 left"], ["both", "🪙 both: a coin flip each time"]]],   // (ticket 316)
    // (ticket 518) what each hand holds: a chip naming it, a tap for a box of every choice (holdChipBox). The right hand first:
    // it is on our left when they face us, as with 'pair'.
    ["hold", "holdR", "🫱 right hand holds"],
    ["hold", "holdL", "🫲 left hand holds"],
    ["choice", "lieHead", "🛌 head toward", [["left", "⬅️ our left"], ["right", "➡️ our right"]]],
    ["range", "hugLift", "🙌 held up (100 = a leg's length)", 0, 300, 40, 90],
    ["choice", "facing", "↔️ facing", [["front", "🧍 toward us"], ["left", "⬅️ to the left"], ["right", "➡️ to the right"], ["back", "⬆️ away from us"]]],
    ["choice", "anim", "🎬 animations", ANIMS],
    ["choice", "stairsWay", "🔼 on the stairs, facing", [["up", "🔼 up them: climbing with the feet"], ["down", "🔽 down them: a controlled fall"]]],   // (ticket 308)
    // (ticket 457) their own size in every place, on top of the place's size for them (a gnome made small once, everywhere)
    ["range", "size", "📏 their own size in every place (100 = as the place sets it)", 20, 300, 50, 150],
    // (ticket 492: in 🧍 Body since 0.0.380, Matthew: "The horse settings should be under 'body', not costume")
    // (ticket 438) the friend's horse, with them whatever they wear: led by its rein; its coat, mane and face marking (the
    // stable's horses' own), its halter and rein, white socks
    ["choice", "horse", "🐴 with the friend", [["none", "no horse"], ["lead", "🚶 led by its rein"], ["ride", "🏇 ridden"]]],
    // (ticket 496) which side of it they walk on, leading it: one side, whichever way they go
    ["choice", "horseLead", "🚶 lead it from its", [["left", "⬅️ left side (the usual)"], ["right", "➡️ right side"]]],
    // (ticket 498: "just release and let the kids decide") its armour: none, or one of six sets; the cloth and the plume in the
    // knight's arms when the friend is a knight, else in its halter's colour
    ["choice", "horseArmour", "🛡️ its armour", HORSE_ARMOURS],
    ["color", "horseCoat", "🐴 its coat", HORSE_COATS],
    ["color", "horseMane", "💈 its mane and tail", HORSE_MANES],
    ["choice", "horseManeStyle", "💇 its mane", HORSE_MANE_STYLES],   // (ticket 497, for M)
    ["color", "horseBows", "🎀 its bows", ["#ec4899", "#d2382b", "#3b82f6", "#e8c33a", "#22c55e", "#7c3aed", "#ffffff"]],
    ["choice", "horseMark", "⭐ on its face", HORSE_MARKS],
    ["color", "horseTack", "🪢 its halter and rein", ["#7a3a1a", "#1f1f22", "#b91c1c", "#1d4ed8", "#15803d", "#e0b030"]],
    ["toggle", "horseSocks", "🧦 white socks"],
    // (ticket 495, for M: "even more color choices for horse ... one color chooser per param") its shoes, its face marking's
    // colour, and, ridden, its saddle and its blanket
    ["color", "horseShoes", "🧲 its horseshoes", ["off", "#9ca3af", "#e5e7eb", "#e8c33a", "#d2382b", "#3b82f6", "#ec4899", "#22c55e"]],
    ["color", "horseMarkColor", "⭐ the colour of the mark on its face", ["#f7f4ee", "#fde68a", "#f9a8d4", "#93c5fd", "#c4b5fd", "#86efac", "#1f1f22"]],
    ["color", "horseSaddle", "🏇 its saddle", ["#6b3f1f", "#1f1f22", "#7a1f2b", "#e8e4da", "#c9a227", "#3f2a6b", "#ec4899"]],
    ["color", "horseBlanket", "🟦 its saddle blanket", ["auto", "#1d4ed8", "#b91c1c", "#15803d", "#f59e0b", "#ec4899", "#ffffff", "#7c3aed"]],
    // (ticket 457) its size: 100, as big as the stable's horses, whatever the friend's size in the scene
    ["range", "horseSize", "📏 its size (100 = as big as the stable's horses)", 20, 200, 40, 150],
    ["group", "jumps", "🐸🦘 the two jumps", [
      ["range", "jumpShortH", "🐸 short jump: how high (100 = a leg's length)", 5, 200, 20, 45],
      ["range", "jumpShortS", "🐸 short jump: how fast (100 = steady)", 25, 400, 120, 180],
      ["range", "jumpHighH", "🦘 high jump: how high (100 = a leg's length)", 5, 300, 70, 130],
      ["range", "jumpHighS", "🦘 high jump: how fast (100 = steady)", 25, 400, 60, 90],
    ]],
  ]],
  ["😀 Face", "face", [
    ["color", "color", "skin & head color", SKINS],
    ["range", "size", "head size", 8, 750, 36, 64],
    ["range", "faceW", "face width", 40, 600, 85, 115],
    ["range", "faceH", "face height", 40, 600, 85, 115],
    ["range", "alien", "face shape (round ↔ alien)", 0, 100, 0, 0],
    ["range", "eyeGap", "eye gap", 0, 600, 25, 55],
    ["range", "smile", "smile", -100, 200, 0, 100],
    ["range", "mouthSize", "👄 mouth size", 20, 400, 70, 140],
    ["range", "brow", "slant (up ↔ down)", -100, 200, 0, 100],   // (ticket 392: "eyebrows" said 🤨 the brows again)
    ["toggle", "unibrow", "one big eyebrow"],
    ["choice", "noseView", "👃 nose", [["none", "none"], ["right", "side, L →"], ["left", "side, ← backwards L"], ["front", "front"], ["witch", "🧙 witch, warts"]]],
    ["choice", "nose", "side shape", [["L", "L"], ["angle", "<"], ["soft", "soft"], ["hook", "bulbous"]]],
    ["choice", "noseFront", "front shape", [["pointed", "pointed"], ["round", "round"]]],
    ["choice", "ears", "👂 ears", [["none", "none"], ["free", "lobe hanging free"], ["attached", "lobe attached"], ["elf", "🧝 elf or fairy, pointy"]]],
    ["range", "earSize", "👂 ear size", 30, 250, 70, 140],
    ["choice", "witchDir", "🧙 witch nose points", [["left", "L ←"], ["center", "C"], ["right", "R →"]]],
    ["range", "witchLen", "🧙 witch nose length", 30, 400, 70, 200],
    ["range", "noseH", "nose height", 20, 300, 70, 150],
    ["range", "noseW", "nose width", 20, 300, 70, 150],
    ["range", "open", "mouth open", 0, 300, 0, 100],
    ["choice", "teethStyle", "🦷 teeth", [["normal", "normal"], ["spikes", "spiky"], ["none", "none"]]],
    ["range", "teeth", "spiky teeth", 0, 15, 0, 5],
    ["choice", "tongue", "👅 tongue", [["in", "in"], ["out", "sticking out"], ["none", "none"]]],
    ["range", "tongueLen", "tongue length", 10, 300, 60, 160],
    ["range", "tongueW", "tongue width", 30, 250, 70, 140],
    ["color", "tongueColor", "tongue color", ["#e75480", "#ff6f91", "#d62f4b", "#c71585", "#8e44ad", "#2ecc71", "#3b82f6"]],
    ["range", "sides", "side lines", 0, 300, 0, 100],
    ["range", "burst", "💥 explode", 0, 300, 0, 100],
    ["range", "candy", "🍬 candy", 0, 300, 0, 100],
    ["range", "rays", "head rays", 0, 300, 0, 100],
    ["range", "sick", "🤢 sickness", 0, 300, 0, 100],
  ]],
  ["👁 Eyes", "face", [
    ["range", "eyes", "eye size", -35, 300, 0, 100],
    ["range", "oval", "eye shape (round ↔ narrow)", 0, 100, 0, 60],
    ["range", "tilt", "eye angle (flat ↔ slanted)", -60, 60, 0, 0],
    ["pad", "light", "✨ where the light comes from"],
    ["color", "shine", "shine color", ["#ffffff", "#fff3b0", "#ffd1dc", "#aee6ff", "#c8ffc8", "#ffb347"]],
    ["range", "shineMix", "shine (dark ↔ bright)", 0, 100, 60, 100],
    ["range", "shineSize", "shine size", 0, 450, 25, 70],
    ["color", "pupil", "pupil color", ["#000000", "#3b2314", "#1e3a8a", "#14532d", "#7f1d1d", "#4c1d95"]],
    ["range", "iris", "iris size (0 = no iris)", 0, 150, 0, 0],
    ["color", "irisColor", "iris color", ["#3b82f6", "#22c55e", "#8b5a2b", "#a0782c", "#9ca3af", "#8b5cf6", "#ef4444"]],
    ["range", "whites", "eye white size (0 = none)", 0, 150, 0, 0],
    ["toggle", "stars", "⭐ star eyes"],
    ["toggle", "hearts", "❤️ heart eyes"],
    ["toggle", "xeyes", "✖ x eyes"],
    ["toggle", "closed", "😌 eyes closed"],
  ]],
  ["💇 Hair", "hair", [
    ["choice", "style", "style", [["none", "none"], ["short", "short"], ["long", "long"], ["curly", "curly"],
                                  ["spiky", "spiky"], ["afro", "afro"], ["pigtails", "pigtails"]]],
    ["range", "length", "📏 length", 30, 400, 70, 180],
    ["range", "bangs", "✂️ bangs (0 = none, 100 = down to the brows)", 0, 300, 40, 140],
    ["color", "color", "color", HAIRS],
    // (ticket 442) the beard: four styles, its colour ("auto": the hair's), and a moustache with it
    ["choice", "beard", "🧔 beard", [["none", "none"], ["short", "✂️ short"], ["full", "🧔 full"], ["long", "🧙 long"], ["goatee", "🐐 goatee"], ["braided", "🪢 braided"]]],
    ["range", "beardLength", "📏 beard length", 30, 300, 60, 180],   // (ticket 443: a share of the style's own)
    ["range", "braidWidth", "🪢 braid width (apart ↔ tight)", 30, 250, 60, 180],
    ["color", "beardShade", "🧔 beard's color", ["auto", BEARD_WHITE, "#c9c5bd", "#8d8172", "#6b4226", "#c0612b", "#e8c35a", "#2b2b2b"]],
    ["toggle", "moustache", "👨 moustache"],
  ]],
  ["🦒 Neck, arms & legs", "body", [
    ["range", "neck", "neck length", 0, 420, 4, 60],
    ["range", "neckW", "neck width", 2, 120, 12, 40],
    ["range", "arms", "arms (up ↔ down)", -180, 180, -75, 85],
    ["range", "elbows", "💪 elbows bend (− down, + up)", -150, 150, 0, 45],
    ["range", "elbowCurl", "🏋️ elbows curl (in ↔ out, as lifting a weight)", 0, 150, 0, 40],   // (ticket 414)
    ["range", "elbowSwing", "🫖 elbows swing (side to side, as a kettle swung across a table: − in, + out)", -90, 90, -20, 20],
    ["range", "armLen", "arm length", 10, 600, 60, 130],
    ["range", "armW", "arm width", 3, 60, 10, 24],
    ["range", "legLen", "leg length", 20, 420, 70, 200],
    ["range", "feetTurn", "👣 feet turn (toes down ↔ up)", -90, 180, -30, 60],
    ["range", "legW", "leg width", 4, 70, 14, 28],
    ["range", "knees", "🦵 knees bend (out, so the friend crouches)", 0, 150, 0, 25],
    ["range", "kneeCurl", "🦶 knees curl (the foot back, behind)", 0, 150, 0, 30],   // (ticket 414)
    ["range", "kneeSwing", "↔️ knees swing (the foot side to side: − in, + out)", -90, 90, -15, 15],
    ["note", "poseHow", () => posed(state.body) ? "🤸 Posed: each arm and leg as you dragged it." : "🤸 To pose the friend, drag a hand, a foot, an elbow or a knee in the picture."],
    ["action", "unpose", "🧍 stand straight again", () => { for (const k of POSE_KEYS) state.body[k] = 0; if (state.body.pose === "meditate" || state.body.pose === "dance" || state.body.pose === "hug") state.body.pose = state.body.sit ? "sit" : "stand"; }],
  ]],
  ["👕 Shirt", "shirt", [
    ["choice", "style", "style", [["tee", "t-shirt"], ["long", "long sleeves"], ["tank", "tank top"]]],
    ["choice", "collar", "collar", [["v", "V"], ["round", "round"], ["flat", "flat"]]],
    ["color", "color", "color", BRIGHT],
    ["choice", "pattern", "pattern", PATTERNS],
    ["range", "patternSize", "pattern size", 4, 60, 10, 24],
    ["color", "patternColor", "pattern color", PATTERN_COLORS],
    ["range", "width", "width", 10, 120, 40, 80],
    ["range", "length", "length", 20, 300, 100, 190],
  ]],
  ["🖐 Hands", "hands", [
    ["flip", "different", ["🖐️🖐️ hands are the same", "🖐️✊ hands are different"]],
    // different hands get a column each, placed as the hands are in the
    // picture (the friend faces us, so their right hand is on our left),
    // each with its own style and that style's sliders
    ["pair", "rightHand", ["⬅️ right hand", "left hand ➡️"], "hands", HAND_CTLS],
    // the same hands: one style, and its sliders after the color
    HAND_CTLS[0],
    ["color", "color", "color", ["skin", ...BRIGHT]],
    ...HAND_CTLS.slice(1),
    ["range", "size", "size", 2, 80, 6, 28],
  ]],
  ["👖 Pants", "pants", [
    ["choice", "style", "style", [["long", "long"], ["shorts", "shorts"], ["skirt", "skirt"], ["dress", "dress"]]],
    ["range", "skirtLen", "skirt length", 10, 140, 30, 100],
    ["color", "color", "color", BRIGHT],
    ["choice", "pattern", "pattern", PATTERNS],
    ["range", "patternSize", "pattern size", 4, 60, 10, 24],
    ["color", "patternColor", "pattern color", PATTERN_COLORS],
  ]],
  ["🩱 Swim & undies", "undies", [
    ["toggle", "on", "🩱 no clothes: just underwear or a swimsuit"],
    ["choice", "style", "style", [["trunks", "swim trunks"], ["briefs", "briefs"], ["bikini", "bikini"],
                                  ["onepiece", "one-piece"]]],
    ["toggle", "top", "with a top (bra / bikini top)"],
    ["color", "color", "color", BRIGHT],
    ["choice", "pattern", "pattern", PATTERNS],
    ["range", "patternSize", "pattern size", 4, 60, 10, 24],
    ["color", "patternColor", "pattern color", PATTERN_COLORS],
  ]],
  ["👟 Feet", "shoes", [
    ["flip", "different", ["👟👟 feet are the same", "👟🦶 feet are different"]],
    // different feet get a column each, as the hands do: the friend's
    // right foot on our left
    ["pair", "rightShoe", ["⬅️ right foot", "left foot ➡️"], "shoes", SHOE_CTLS, FOOT_SLIDER_STYLE],
    SHOE_CTLS[0],
    ["color", "color", "color", BRIGHT],
    ...SHOE_CTLS.slice(1),
    ["range", "size", "size", 20, 310, 60, 170],
  ]],
  // how see-through each part is: the shirt the body under it, never a
  // bare one, and the pants the legs
  ["👻 Visibility", "visible", [
    ["every", "all", "every part at once", [["🙂 visible", 100], ["👻 ghost", 50], ["🫥 invisible", 0]]],
    ["master", "all", "🧍 full body: moves every part", 0, 100],
    ["range", "head", "🙂 head", 0, 100],
    ["range", "eyes", "👀 eyes", 0, 100],
    ["range", "hair", "💇 hair", 0, 100],
    ["range", "shirt", "👕 shirt: the body, neck & arms", 0, 100],
    ["range", "pants", "👖 pants: the hips & legs", 0, 100],
    ["range", "feet", "👟 feet", 0, 100],
    ["range", "hands", "🖐 hands", 0, 100],
  ]],
  ["🐯 Costume", "costume", [
    ["choice", "kind", "costume", [["none", "none"], ["tiger", "🐯 tiger"], ["cat", "🐈‍⬛ black cat"],
                                   ["lion", "🦁 lion"], ["bear", "🐻 bear"], ["falcon", "🦅 peregrine falcon"],
                                   ["raccoon", "🦝 raccoon"], ["bobcat", "🐈 bobcat"], ["leopard", "🐆 leopard"],
                                   ["eagle", "🦅 bald eagle"], ["cheetah", "🐆 cheetah"], ["jaguar", "🐈‍⬛ black jaguar"], ["moth", "🦋 moth"],
                                   ["falconsuit", "🦅 falcon suit"], ["eaglesuit", "🦅 bald eagle suit"],
                                   ["wolf", "🐺 wolf"], ["deer", "🦌 deer"], ["dragon", "🐉 dragon"], ["sasquatch", "🦶 Sasquatch"], ["witch", "🧙 witch"], ["fairy", "🧚 fairy"], ["ghost", "👻 ghost"], ["gnome", "🍄 gnome"], ["wizard", "🧙‍♂️ wizard"], ["monk", "🧘 monk or dryad"], ["robin", "🏹 Robin Hood"], ["knight", "🛡️ knight"], ["queen", "👸 queen"], ["king", "🤴 king"], ["robot", "🤖 robot"], ["cowgirl", "👢 cowgirl"], ["cowboy", "🤠 cowboy"],
                                   ["ninja", "🥷 ninja"],
                                   ["giraffe", "🦒 giraffe"],   // (ticket 544)
                                   ["scuba", "🤿 scuba suit"], ["diver", "⚓ old diving suit"]]],
    // (ticket 288) diving: the two suits' colors, the old one's hose; and gear worn with any costume but the old helmet
    ["color", "scubaColor", "🤿 suit's color", ["#1f2937", "#111827", "#1e3a8a", "#0f766e", "#7c2d12", "#581c87", "#be123c"]],
    ["color", "scubaAccent", "🤿 stripes & fins", ["#14b8a6", "#f97316", "#facc15", "#ec4899", "#22c55e", "#38bdf8", "#ffffff"]],
    ["color", "diverMetal", "⚓ helmet's metal", ["#c9a227", "#b87333", "#9ca3af", "#d4af37"]],
    ["color", "diverCloth", "⚓ suit's canvas", ["#b9a27a", "#8b7355", "#6b7280", "#d6c7a1"]],
    ["toggle", "diverHose", "⚓ air hose up to the surface"],
    ["color", "hoseColor", "⚓ hose's color", ["#4b5563", "#1f2937", "#7c2d12", "#d6c7a1"]],
    ["range", "leaks", "🫧 leaks (more and more bubbles)", 0, 100, 0, 100],
    ["toggle", "snorkel", "🤿 snorkel"],
    ["color", "snorkelColor", "🤿 snorkel's color", ["#facc15", "#f97316", "#22c55e", "#ec4899", "#38bdf8", "#1f2937"]],
    ["toggle", "mask", "🥽 face mask"],
    ["color", "maskColor", "🥽 mask's color", ["#0ea5e9", "#1f2937", "#f97316", "#ec4899", "#22c55e", "#facc15"]],
    ["toggle", "tank", "🛢️ air tank"],
    ["color", "tankColor", "🛢️ tank's color", ["#facc15", "#9ca3af", "#f97316", "#e5e7eb", "#1f2937", "#ef4444"]],
    // the ghost: its sheet, its eyes, and how see-through it is (the
    // visibility panel's all-at-once controls, and their LFO)
    ["range", "ghostRagged", "👻 how ragged (0 = smooth)", 0, 100, 0, 70],
    ["choice", "ghostEyes", "👀 eyes", [["0", "none"], ["1", "one"], ["2", "two"], ["3", "three"]]],
    ["toggle", "ghostHideEyes", "🕳️ hide eyes (black holes)"],
    ["color", "ghostColor", "👻 the sheet's color", GHOST_COLORS],
    ["of", "visible", ["every", "all", "👻 see-through: every part at once", [["🙂 visible", 100], ["👻 ghost", 50], ["🫥 invisible", 0]]], c => c.kind === "ghost"],
    ["of", "visible", ["master", "all", "🧍 full body: moves every part", 0, 100], c => c.kind === "ghost"],
    ["color", "fur", "fur / feather color", FURS],
    ["choice", "furPattern", "fur pattern", [["none", "plain"], ["stripes", "stripes"], ["spots", "spots"],
                                             ["rosettes", "rosettes"], ["patches", "patches"]]],
    ["color", "marks", "markings color (stripes, mane, bars)", MARKS],
    ["color", "spot", "rosette middle color", ["auto", "off", "#7a4a10", "#c8892e", "#5a3a1a", "#1c1410", "#ffffff", "#ff69b4", "#8b5cf6"]],
    ["color", "belly", "belly & muzzle color", BELLIES],
    ["color", "eyes", "animal eye color", ANIMAL_EYES],
    ["color", "accent", "nose, ear & beak color", ACCENTS],
    ["color", "witchColor", "🧙 hat & dress color", ["#6a2c91", "#1f1f22", "#2e7d32", "#1e3a8a", "#b71c1c", "#f28c28", "#ff69b4"]],
    ["color", "webColor", "🕸️ spider web color", ["#3fae4a", "#ffffff", "#c0c0c0", "#f28c28", "#1f1f22", "#b38bff"]],
    ["choice", "webStyle", "🕸️ spider web", [["tiled", "all over"], ["few", "a few webs"]]],
    ["color", "trimColor", "hat band & stocking color", ["#f28c28", "#3fae4a", "#6a2c91", "#1f1f22", "#ffffff", "#e53935"]],
    ["toggle", "witchCat", "🐈‍⬛ cat on the shoulder"],
    ["color", "catFur", "🐈‍⬛ cat's fur", ["#1f1f22", "#6b4226", "#e8862a", "#9ca3af", "#ffffff", "#8b5cf6"]],
    ["color", "catWhite", "🐈‍⬛ cat's tuxedo (nose, chest, paws)", ["#ffffff", "off", "#f7ead8", "#ffd1dc", "#c0c0c0", "#1f1f22"]],
    ["color", "catEyes", "🐈‍⬛ cat's eyes", ["#c6d43a", "#e0a800", "#3b82f6", "#22c55e", "#f28c28"]],
    ["color", "catNose", "🐈‍⬛ cat's nose", ["#f4a0b0", "#ff69b4", "#1f1f22", "#8a4b1c"]],
    ["range", "catSize", "🐈‍⬛ cat's size", 10, 1000, 70, 160],
    ["toggle", "broom", "🧹 broom"],
    ["range", "broomSize", "🧹 broom's size", 10, 1000, 70, 160],
    ["color", "bootColor", "👢 boots & belt color", ["#1f1f22", "#6a2c91", "#4a2c17", "#b71c1c", "#2e7d32", "#ffffff"]],
    // (ticket 431) the gnome: a man or a woman, the tall pointed hat, and each one's clothes; felt boots
    ["choice", "gnomeWho", "🍄 the gnome", [["man", "👴 gnome man"], ["woman", "👵 gnome woman"]]],
    ["color", "gnomeHat", "🍄 pointed hat's color", ["#d2382b", "#7b3f8c", "#2f6db5", "#2e7d32", "#e8862a", "#c2185b"]],
    ["color", "gnomeTunic", "🍄 tunic's color", ["#3f8f8a", "#2f6db5", "#2e7d32", "#7b3f8c", "#b71c1c", "#8b5a2b"]],
    ["color", "gnomeTrousers", "🍄 trousers' color", ["#7d8b6a", "#5b6b4a", "#6b5a45", "#4b5563", "#2f3e5c"]],
    ["color", "gnomeBodice", "🍄 bodice's color", ["#d2382b", "#2e7d32", "#2f6db5", "#7b3f8c", "#1f1f22"]],
    ["color", "gnomeSkirt", "🍄 skirt's first stripe", ["#d2382b", "#e8862a", "#c2185b", "#2f6db5", "#2e7d32"]],
    ["color", "gnomeBoots", "🥾 felt boots' color", ["#e8e4da", "#8d8172", "#6b4226", "#1f1f22", "#b71c1c"]],
    // (tickets 436, 437) the queen and the king: each one's robe, the crown's jewel, the mantle, the scepter
    ["color", "queenRobe", "👸 gown's color", ["#5b2a86", "#7a1f2b", "#1e3a8a", "#1f5a4a", "#c2185b", "#e0b030"]],
    ["color", "kingRobe", "🤴 robe's color", ["#7a1f2b", "#5b2a86", "#1e3a8a", "#1f5a4a", "#e0b030"]],
    ["color", "crownJewel", "💎 the jewels' color", ["#d2382b", "#2f6db5", "#2e7d32", "#7b3f8c", "#f4f1ea"]],
    ["toggle", "mantle", "👑 ermine mantle"],
    ["toggle", "scepter", "👑 scepter"],
    // (ticket 499) the dragon: a colour each for its scales, its belly, its wings and spines, and its horns, teeth and claws; its
    // wings flapping, open and still, or folded, and what they flap with
    ["color", "dragonScales", "🐉 its scales", ["#3f8f4a", "#2f6db5", "#8a2b2b", "#5b3f8c", "#1f1f22", "#c9a227", "#7a8a3a", "#e8862a"]],
    ["color", "dragonBelly", "🐉 its belly", ["#e9d27a", "#f4ecd6", "#e8a87c", "#9ad0c2", "#c9c5bd", "#f9a8d4"]],
    ["color", "dragonWings", "🦇 its wings and spines", ["#d0453a", "#f28c28", "#7b3f8c", "#2f6db5", "#2e7d32", "#1f1f22", "#e9d27a"]],
    ["color", "dragonHorns", "🦷 its horns, teeth and claws", ["#f4ecd6", "#e0b030", "#c9c5bd", "#1f1f22", "#8a5a2b", "#ffffff"]],
    ["choice", "dragonWingsHow", "🪽 its wings", DRAGON_WINGS],
    ["choice", "dragonWingLfo", "〰️ wings flap with", [["none", "their own slow beat"], ...LFO_NAMES.map(L => [L, "LFO " + L])]],
    // (ticket 523) and its fire, tried on here: 🎮 Controls' 🔥 its fire (the friend's, one setting in both places), a chip
    // tapped breathing it for a moment, and a 🔥 button to hold, so the controls need not be opened to try it
    ["of", "body", ["breathe", "fireStyle", "🔥 its fire (tap one to try it, or hold 🔥 to breathe it)", FIRE_STYLES], c => c.kind === "dragon"],
    // (ticket 536) the moth: three styles, each its own wing colour (shown while it is chosen, as the Sasquatch's fur), and
    // its eyespots; its body's colours are the animal colour rows above, put on in the style's when it is chosen
    ["choice", "mothStyle", "🦋 style", Object.entries(MOTH_STYLES).map(([id, t]) => [id, t.name])],
    ...Object.entries(MOTH_STYLES).map(([id, t]) => ["color", "mothColor_" + id, "🦋 its wings", t.colors]),
    ["toggle", "mothSpots", "👁️ eyespots"],
    // (ticket 515) the robot: one of eight styles, its colour (each style its own, shown while it is chosen, as the fairy's
    // wings), and its lights ("auto": each style's own)
    ["choice", "robotStyle", "🤖 style", Object.entries(ROBOT_STYLES).map(([id, r]) => [id, r.name])],
    ...Object.entries(ROBOT_STYLES).map(([id, r]) => ["color", "robotColor_" + id, "🤖 its colour", r.colors]),
    ["color", "robotLights", "💡 its lights", ROBOT_LIGHTS],
    // (ticket 522) the cowgirl and the cowboy: three styles each, each its own colour (shown while it is chosen, as the
    // robot's), a hat of six (or none) with a colour each, and a lasso
    // (which hand holds it is 🧍 Body's 🫴 holding, since ticket 518)
    ["choice", "cowgirlStyle", "👢 style", Object.entries(COWGIRL_STYLES).map(([id, t]) => [id, t.name])],
    ...Object.entries(COWGIRL_STYLES).map(([id, t]) => ["color", "cowgirlColor_" + id, "👢 its colour", t.colors]),
    ["choice", "cowboyStyle", "🤠 style", Object.entries(COWBOY_STYLES).map(([id, t]) => [id, t.name])],
    ...Object.entries(COWBOY_STYLES).map(([id, t]) => ["color", "cowboyColor_" + id, "🤠 its colour", t.colors]),
    ["choice", "cowHat", "🎩 hat", [["none", "🚫 no hat"], ...Object.entries(COW_HATS).map(([id, h]) => [id, h.name])]],
    ...Object.entries(COW_HATS).map(([id, h]) => ["color", "cowHatColor_" + id, "🎩 hat's colour", h.colors]),
    ["toggle", "lasso", "➿ lasso"],
    // (ticket 557) the ninja: its boots' bands, braided or wrapped; the gold head covering, on by default, its gem and the
    // gem's shape (shown while the covering is on); and the eyes in the mask's slit, off (the face is not seen)
    ["choice", "ninjaBoots", "🥾 the boots' bands", NINJA_BOOTS],
    ["choice", "ninjaBetween", "🦵 between the bands", NINJA_BETWEEN],   // (ticket 561, M's design: the skin, at first)
    ["toggle", "ninjaBand", "👑 golden head band"],
    ["choice", "ninjaGem", "💎 the gem", Object.entries(NINJA_GEMS).map(([id, g]) => [id, g.name])],
    ["choice", "ninjaGemShape", "💎 the gem's shape", NINJA_GEM_SHAPES],
    // (ticket 560) the gem's lights: its white cuts glimmer (on at first); stars twinkle on it, and a shine sweeps it (off)
    ["toggle", "ninjaGlimmer", "✨ the gem's cuts glimmer"],
    ["toggle", "ninjaGemStars", "⭐ stars twinkle on the gem"],
    ["toggle", "ninjaGemShine", "🌟 a shine sweeps the gem"],
    ["toggle", "ninjaEyes", "👀 eyes in the slit"],
    // (ticket 530) the Sasquatch: four styles, each its own colour (shown while it is chosen, as the robot's), its big
    // footprints on the ground, and its grin
    ["choice", "sasquatchStyle", "🦶 style", Object.entries(SASQUATCH_STYLES).map(([id, t]) => [id, t.name])],
    ...Object.entries(SASQUATCH_STYLES).map(([id, t]) => ["color", "sasquatchColor_" + id, "🦶 its fur", t.colors]),
    ["toggle", "sasquatchPrints", "👣 footprints"],
    ["toggle", "sasquatchGrin", "🙂 big grin"],
    // (ticket 544) the giraffe: three styles, each its patches' own colour (shown while it is chosen, as the Sasquatch's), and
    // how long its neck is (a percent of the style's)
    ["choice", "giraffeStyle", "🦒 style", Object.entries(GIRAFFE_STYLES).map(([id, t]) => [id, t.name])],
    ...Object.entries(GIRAFFE_STYLES).map(([id, t]) => ["color", "giraffeColor_" + id, "🦒 its patches", t.colors]),
    ["range", "giraffeNeck", "🦒 neck's length", 50, 250, 70, 160],
    // (ticket 435) the knight: his arms (two colours, how they are laid out, an emblem), his helmet's plume and visor, sword and shield
    ["color", "knightA", "🛡️ arms' first color", ["#2f6db5", "#8a2b2b", "#1f1f22", "#2e7d32", "#e0b030", "#7b3f8c"]],
    ["color", "knightB", "🛡️ arms' second color", ["#f4f1ea", "#e0b030", "#1f1f22", "#8a2b2b", "#2f6db5"]],
    ["choice", "tabardLayout", "🛡️ the arms", [["quartered", "▚ quartered"], ["halves", "◧ halves"], ["plain", "▢ plain"]]],
    ["choice", "emblem", "🛡️ emblem", [["none", "none"], ["cross", "✚ cross"], ["chevron", "⌃ chevron"], ["falcon", "🦅 falcon"], ["crown", "👑 crown"]]],
    ["toggle", "plume", "🪶 plume"],
    ["color", "plumeColor", "🪶 plume's color", ["#c0392b", "#f4f1ea", "#2f6db5", "#e0b030", "#1f1f22"]],
    ["toggle", "helmOpen", "🪖 face showing (an open helm)"],
    ["toggle", "sword", "🗡️ sword"],
    ["toggle", "shield", "🛡️ shield"],
    // (ticket 434) Robin Hood: his tunic, his cap and its feather, his bow and quiver
    ["color", "robinTunic", "🏹 tunic's color", ["#3f7d3a", "#2e5e2a", "#7a5232", "#8a2b2b", "#6b7280"]],
    ["color", "robinHat", "🏹 cap's color", ["#3f7d3a", "#8a2b2b", "#7a5232", "#2e5e2a", "#e3b72f"]],
    ["color", "robinFeather", "🪶 feather's color", ["#d2382b", "#f4f1ea", "#e3b72f", "#2f6db5", "#1f1f22"]],
    ["toggle", "bow", "🏹 bow"],
    ["toggle", "quiver", "🏹 quiver of arrows"],
    // (ticket 433) the monk or the dryad: the cloak and its hood, up or down (the staff: the wizard's row)
    ["choice", "monkWho", "🧘 who", [["monk", "🧘 monk"], ["dryad", "🌿 dryad"]]],
    ["color", "monkCloak", "🧥 cloak's color", ["#6b4a2b", "#4f6b3a", "#7a7468", "#3f4a5c", "#8a3b2b", "#e8e0cc"]],
    ["toggle", "hoodUp", "🧥 hood up"],
    ["toggle", "bareFeet", "🦶 bare feet (the shoes hidden in the cloak)"],
    // (ticket 453) the face in the hood: seen and dimmed, or dark with eyes in it
    ["toggle", "hoodFace", "👤 see the face in the hood"],
    ["range", "hoodDim", "🌑 the face in shadow (clear ↔ black)", 0, 100, 0, 70],
    ["toggle", "hoodEyes", "👀 eyes in the dark"],
    // (ticket 432) the wizard: his robe and its stars, his staff and its orb (his beard is the friend's, in 💇 Hair)
    ["color", "wizardRobe", "🧙‍♂️ robe & hat color", ["#2a3a8c", "#1f1f22", "#5b2a86", "#7a1f2b", "#1f5a4a", "#6b7280"]],
    ["color", "wizardStars", "⭐ stars & moon color", ["#f2c230", "#e5e7eb", "#93c5fd", "#f9a8d4"]],
    ["toggle", "staff", "🪄 staff"],
    ["color", "staffOrb", "🔮 the staff's orb", ["#7dd3fc", "#a78bfa", "#86efac", "#fca5a5", "#fde047"]],
    // the fairy: wings and tunic, each in a style with its own colors; shoes
    ["choice", "wingStyle", "🧚 wings", Object.entries(FAIRY_WINGS).map(([id, w]) => [id, w.name])],
    ...Object.entries(FAIRY_WINGS).map(([id, w]) => ["color", "wingColor_" + id, "🧚 wings' color", w.colors]),
    ["toggle", "wingFlutter", "🧚 wings flutter"],
    ["choice", "wingLfo", "〰️ wings flap with", [["none", "no LFO"], ...LFO_NAMES.map(L => [L, "LFO " + L])]],
    ["choice", "fairyOutfit", "🧚 wears", [["dress", "👗 dress"], ["pants", "👕 shirt + pants"]]],
    ["choice", "tunicStyle", "🧚 tunic", Object.entries(FAIRY_TUNICS).map(([id, t]) => [id, t.name])],
    ...Object.entries(FAIRY_TUNICS).map(([id, t]) => ["color", "tunicColor_" + id, "🧚 tunic's color", t.colors]),
    ["toggle", "wreath", "🌸 a wreath of flowers"],
    ["choice", "wreathFlowers", "🌸 the wreath's flowers", Object.entries(WREATH_SETS).map(([id, w]) => [id, w.name])],
    ["choice", "fairyShoes", "🧚 shoes", FAIRY_SHOES],
    ["color", "fairyShoeColor", "🧚 shoes' color", FAIRY_SHOE_COLORS],
  ]],
  // (ticket 439) the people: who is being dressed, the others and where they stand, templates and new people from them
  ["👥 People", "person", [
    ["people", "list", ""],
  ]],
  ["🏞️ Background", "background", [
    // (ticket 285, ^du_report(5)'s step 2) 🏞️ Background as a designer: its full screen, and the game as its first card there
    ["bgbig", "big", "⛶ full screen"],
    ["bgview", "designerView", ""],   // (its own key: "view" is the living room window's, shown only there)
    ["places", "kind", "background", PLACES],
    // (ticket 317) the levers' shine, wherever there are levers (the trap room's, placed ones)
    ["range", "leverShine", "✨ levers' shine: its size (0 = none)", 0, 200, 0, 200],
    ["color", "leverShineColor", "✨ levers' shine: its colour", ["#ffd27a", "#ffb347", "#ffffff", "#9ad7ff", "#ff6b6b", "#c4b5fd"]],
    ["range", "leverFlicker", "🔥 levers' shine: how much it flickers, as the torches do", 0, 100, 0, 100],
    // (ticket 300) in a square of the world: where a change made here goes (the place's template, every one of it that has
    // no setting of its own; or this square alone), and this square back to the template
    ["choice", "instEdit", "✏️ changes here go to", [["template", "🧩 the template: every square of this place"], ["instance", "📍 only this square"]]],
    ["action", "instReset", "↺ this square back to the template (its own settings gone)", () => instanceReset()],
    ["range", "figSize", "🧍 size in the scene", 10, 1000, 100, 100],
    // (ticket 214) each place's own: how much smaller going north (Surprise me leaves it as it is)
    ...[...NORTH_OUT, ...NORTH_ROOMS].map(kd => ["range", "north_" + kd, "🔭 smaller going north (% for each tenth of the picture)", 0, 80, northDefault(kd), northDefault(kd)]),
    ["place", "figPos", "🧍 place in the scene (or drag the figure)"],
    ["objects", "objectsOff", "👁️ what stands in the scene, back to front (tap to hide or show)"],
    ["seats", "seat", "🪑 sit on (or drag the friend onto a seat)"],
    // the friend standing in water, where a place has water to stand in (WATER, ticket 122)
    ["range", "waterSee", "🌊 see-through under the water (0 = not at all)", 0, 100, 50, 90],
    ["range", "waterWake", "💧 a wake round the friend in the water (0 = none)", 0, 250, 60, 160],
    ["range", "waterTread", "🏊 treading water where it's deep (0 = standing still)", 0, 200, 60, 140],
    // (ticket 220, Matthew) 🫧 bubble space's own: how many bubbles, the room's wind, and up to three bubble themes
    ["group", "bubbles", "🫧 bubble space", [
      ["range", "bubbleCount", "🫧 how many", 5, 80, 20, 40],
      ["range", "bubbleWind", "🌬️ wind", 0, 100, 20, 60],
      // (ticket 274) the bubbles' seed: the same seed, the same places, every time the place is drawn
      ["action", "bubbleSeed", "🎲 other places for the bubbles", () => { state.background.bubbleSeed = 1 + Math.floor(Math.random() * 99999); }],
      // (ticket 329, Matthew) the bubbles smaller farther back; a pop's smaller bubbles, in its style; lines out from it, as
      // the sun's rays, growing and gone; and its sound, with an instrument's settings as the mixing board's
      ["range", "bubbleDepth", "🔭 smaller farther back (0 = all one size)", 0, 100, 40, 90],
      ["range", "bubbleStick", "🫧 how often a bubble maker's bubbles stick to these (0 = never)", 0, 100, 10, 70],   // (ticket 341)
      ["range", "bubbleBurst", "💥 how likely a pop makes smaller bubbles", 0, 100, 20, 80],
      ["range", "bubbleBurstCount", "🫧 how many smaller bubbles, about", 1, 8, 2, 5],
      ["toggle", "bubbleRays", "✨ lines out from a pop"],
      ["range", "bubblePopVol", "🔊 a pop's sound (0 = none)", 0, 100, 20, 70],
      ["choice", "bubblePopInst", "🎹 the pop's instrument", BUBBLE_POP_INSTS],
      ["range", "bubblePopPitch", "🎵 the pop's pitch (half steps; smaller bubbles higher)", -12, 12, -5, 5],
      ["range", "bubblePopKnob", "✨ the instrument's knob (bloop, ring, fall)", 0, 100, 20, 80],
      ...BUBBLE_THEME_CTLS,
    ]],
    // (ticket 290, for N) the bubble room's own: its foam on each of its walls, its floor and its ceiling, how far up (or out)
    // each goes, the foam's colour, how much it moves, another foam
    ["group", "bubbleroom", "🫧 the bubble room", [
      ...FOAM_SURFACES.flatMap(([K, name, up]) => [["toggle", "foam" + K, `🫧 foam on the ${name}`], ["range", "foam" + K + "H", `${up ? "↕️ how high" : "↔️ how far out"}, on the ${name}`, 0, 100, 30, 100],
        ["range", "foam" + K + "Out", `🔵 bubbles stuck out into the room, from the ${name} (how many)`, 0, 100, 10, 60]]),
      ["color", "foamColor", "🎨 the foam's colour", BUBBLE_COLORS],
      ["toggle", "foamRainbow", "🌈 a rainbow sheen on the foam"],
      ["range", "foamMove", "〰️ how much the foam moves (0 = still; it moves from the start, its bubbles as layers that cost the computer little)", 0, 100, 0, 60],
      ["range", "foamStick", "🫧 how often moving bubbles stick to the bubbles standing out (0 = never)", 0, 100, 10, 70],   // (ticket 341)
      ["range", "bubbleFloorWobble", "〰️ how much the bubbles move under you (0 = still)", 0, 100, 20, 90],   // (ticket 343)
      ["action", "foamSeed", "🎲 another foam", () => { state.background.foamSeed = 1 + Math.floor(Math.random() * 99999); }],
    ]],
    // what in the scene moves with the music: its quick things and its slow things (SCENE_LFO_THINGS)
    ["choice", "lfoFast", "〰️ quick things in the scene move with", [["none", "their own pace"], ...LFO_NAMES.map(L => [L, "LFO " + L])]],
    ["choice", "lfoSlow", "〰️ slow things in the scene move with", [["none", "their own pace"], ...LFO_NAMES.map(L => [L, "LFO " + L])]],
    ["choice", "lfoWings", "〰️ wings in the scene beat with", [["none", "their own pace"], ...LFO_NAMES.map(L => [L, "LFO " + L])]],
    // the witch's woods: the hut behind the fence, on chicken legs or not
    ["toggle", "chickenLegs", "🐔 chicken legs on the hut"],
    ["range", "legSize", "🐔 leg size", 20, 300, 80, 140],
    ["range", "hutNear", "🛖 hut: far ↔ near (behind the fence)", 0, 100, 0, 60],
    ["range", "eyeRate", "💀 skull eyes: blinks a minute (0 = steady)", 0, 240, 20, 60],
    ["flip", "blinkApart", ["💀 skull eyes blink in sync", "💀 skull eyes blink out of sync"]],
    ["range", "skullMax", "💀 skulls: biggest size (% of the usual; the smallest is the usual)", 100, 3000, 200, 500],
    ["range", "skullEye", "💀 skull eye size (the skulls grow to hold them)", 50, 500, 80, 200],
    ["range", "eyeGlow", "💀 skull eyes: red glow size", 0, 100, 40, 90],
    // (ticket 206, Matthew: her cauldron simple, unless upgraded here; in her woods and in her hut)
    ["choice", "cauldronFumes", "💨 her cauldron's fumes", [["simple", "simple"], ["fancy", "✨ fancy"]]],
    // the lake
    ["range", "ducks", "🦆 ducks", 0, 16, 2, 9],
    ["range", "duckSize", "🦆 duck size", 30, 300, 70, 160],
    ["range", "cattails", "🌾 cattails", 0, 40, 6, 24],
    ["range", "clouds", "☁️ clouds", 0, 10, 1, 7],
    ["range", "cloudSize", "☁️ cloud size", 40, 250, 70, 170],
    // the house and yard
    ["range", "houseSize", "🏠 house size", 50, 130, 80, 115],
    ["range", "treeSize", "🌳 tree size", 40, 200, 70, 150],
    ["range", "treeHouseSize", "🏡 tree house size", 50, 250, 80, 160],
    // (ticket 285, ^du_report(5)'s finding: one setting in two rows, and SHOWN_WHEN has one rule per key, so the house's never showed)
    ["of", "background", ["range", "toys", "🧸 toys on the lawn", 0, 16, 2, 10], b => b.kind === "house"],
    ["range", "flowers", "🌷 flowers", 0, 48, 6, 36],
    // the tree house
    ["range", "houses", "🏡 houses in the tree", 1, 6, 2, 6],
    ["range", "bridges", "🌉 rope bridges", 0, 5, 0, 5],
    ["range", "ladders", "🪜 ladders", 0, 6, 1, 6],
    ["range", "leaves", "🍃 leaves", 0, 100, 30, 100],
    // each house's kind, and then its own colors, for the kinds with colors
    ...[1, 2, 3, 4, 5, 6].flatMap(n => [["choice", "house" + n, "🏡 house " + n, HOUSE_ANIMALS],
      ...HOUSE_COLOR_CTLS.map(([k, label, colors]) => ["color", k + n, `🏡 house ${n}: ${label}`, colors])]),
    // inside the tree house
    ["choice", "inside", "🏡 which tree house", [1, 2, 3, 4, 5, 6].map(n => [n, "🏡 " + n])],
    ...[1, 2, 3, 4, 5, 6].flatMap(n => [["choice", "room" + n, `🏡 ${n}: its room`, ROOM_CHOICES],
      ...[1, 2].map(i => ["choice", `r${n}pic${i}`, `🏡 ${n}: picture ${i} on its wall`, PICTURE_CHOICES]),
      ["choice", "treeBack" + n, `🏡 ${n}: ⬇️ the way toward us leads to`, BEHIND_PLACES],
      ["choice", "hatchShape" + n, `🏡 ${n}: 🕳️ its trapdoor's opening`, OPENING_SHAPES],   // (ticket 252)
      ["toggle", "hatchShut" + n, `🏡 ${n}: 🎚️ its trap door shut (its lever pulled)`]]),   // (ticket 334)
    // (ticket 516) the round room at the top of the tree: its one trap door (down to the house below), its lever, its way toward us
    ["choice", "hatchShapeTop", "⭕ 🕳️ its trapdoor's opening", OPENING_SHAPES],
    ["toggle", "hatchShutTop", "⭕ 🎚️ its trap door shut (its lever pulled)"],
    ["choice", "treetopBack", "⭕ ⬇️ the way toward us leads to", BEHIND_PLACES.filter(([k]) => k !== "treetop")],
    ["choice", "topFurnish", "⭕ 🪑 how it is furnished", TOP_FURNISH],   // (ticket 517)
    ["range", "windowSize", "🪟 window size", 40, 220, 70, 160],
    ["of", "background", ["range", "toys", "🧸 things in the room", 0, 24, 4, 20], b => b.kind === "treeroom"],   // (ticket 285: its own rule, as the house's)
    // the mountain trail
    ["range", "peakHeight", "🏔️ how high the peaks are", 50, 170, 70, 140],
    ["range", "mtClouds", "☁️ clouds", 0, 10, 2, 8],
    ["range", "birds", "🦅 birds circling", 0, 8, 1, 5],
    ["toggle", "farLake", "🏞️ the lake and village far below"],
    // (ticket 503, for M) the pasture: how many horses graze in it, and how many patches of wildflowers grow in its grass
    ["range", "pastureHorses", "🐴 horses grazing", 0, 3, 0, 3],
    ["range", "pastureFlowers", "🌼 patches of wildflowers in the grass", 0, 24, 4, 16],
    // the jungle waterfall
    ["range", "fallWidth", "💦 waterfall width", 40, 250, 70, 160],
    ["range", "vines", "🌿 vines", 0, 20, 4, 14],
    ["range", "mist", "🌫️ mist", 0, 100, 30, 90],
    ["range", "parrots", "🦜 parrots", 0, 6, 1, 4],
    ["range", "fallRate", "💦 how fast it falls (100 = the usual)", 25, 400, 50, 250],
    ["choice", "fallTime", "🌗 time of day (outside the cave too)", [["day", "☀️ day"], ["sunset", "🌇 sunset"], ["night", "🌙 night"], ["clock", "🕰️ by the clock (🌍 where and when)"]]],
    // (ticket 490) 🌍 where and when, for a sky by the clock: the place (or its own latitude and longitude) and the game's date
    // and hour (the sun's own time there), never the device's; shown where the time of day is by the clock
    ["group", "skyWhen", "🌍 the sky: where and when", [
      ["choice", "skyWhere", "📍 where on Earth", SKY_PLACES.map(([v, l]) => [v, l])],
      ["range", "skyLat", "🧭 latitude (north +, south −)", -89, 89, -60, 70],
      ["range", "skyLon", "🧭 longitude (east +, west −)", -180, 180, -150, 150],
      ["range", "skyYear", "📅 the year", 2000, 2050, 2020, 2030],
      ["range", "skyMonth", "📅 the month (1 = January)", 1, 12, 1, 12],
      ["range", "skyDay", "📅 the day of the month", 1, 31, 1, 31],
      ["range", "skyHour", "🕰️ the hour (the sun's own time there: 12 is noon)", 0, 23, 0, 23],
      ["note", "skySays", () => skySays(state.background)]]],
    // the cave behind the falls: gems on its walls, sparkling
    ["range", "caveGems", "💎 gems on the walls", 0, 40, 6, 30],
    ["choice", "gemShape", "✨ sparkles: shape", STAR_SHAPES],
    ["range", "gemSize", "✨ sparkles: size (100 = the usual, up to huge)", 25, 2000, 60, 600],
    ["range", "gemBright", "✨ sparkles: brightness", 5, 100, 50, 100],
    ["range", "gemTwinkle", "✨ sparkles: twinkle (the big bright ones turn, grow and shrink, and glimmer)", 0, 100, 20, 100],
    // (ticket 471) the cave room: what stands on its floor, and what hangs from its ceiling
    ["choice", "caveFloor", "🪨 on the cave room's floor", CAVE_FLOORS],
    ["choice", "caveCeiling", "🦇 on the cave room's ceiling", CAVE_CEILINGS],
    // (ticket 501, for M) the desert: what it has far off (farLayers' mountains, as a distant shore's)
    ["choice", "desertFar", "🏜️ far off in the desert", SHORE_MOUNTAINS],
    // the camp site
    ["choice", "campTime", "🌗 time of day", [["day", "☀️ day"], ["sunset", "🌇 sunset"], ["night", "🌙 night"], ["clock", "🕰️ by the clock (🌍 where and when)"]]],
    ["choice", "starShape", "⭐ stars: shape", STAR_SHAPES],
    ["range", "starSize", "⭐ stars: size (100 = the usual, up to huge)", 25, 2000, 60, 600],
    ["range", "starBright", "⭐ stars: brightness", 5, 100, 50, 100],
    ["range", "starTwinkle", "✨ stars: twinkle (the big bright ones turn, grow and shrink, and glimmer)", 0, 100, 20, 100],
    ["toggle", "fireOn", "🔥 the campfire is lit"],
    ["range", "flameSize", "🔥 flame size", 20, 250, 60, 160],
    ["choice", "campFood", "🍲 on the fire", [["none", "nothing"], ["sausages", "🌭 veggie sausages on sticks"], ["soup", "🍲 a pot of soup"]]],
    ["range", "tents", "⛺ tents", 0, 3, 1, 3],
    ["choice", "tent", "⛺ which tent", CAMP_TENTS.map((T, i) => [i + 1, `⛺ ${i + 1}: ${T.name}`])],
    // (ticket 185) the things put in any place; and the forest crossing: which paths meet, its floor
    ["things", "things", "🪨 things to put here, then drag them where they go"],
    ["toggle", "thingsSolid", "🧱 placed things are in your way (a tree by its trunk)"],   // (ticket 283)
    // (ticket 249, Matthew: "for the toggles for N/S/E/W, etc.... use the UX we made for world designer, with the 9 cells -- the
    // center cell be empty in this case") as the world designer's ways out, round an empty middle
    ["compass", "crossPaths", "🛤️ the paths that meet here", "cross"],
    ["color", "crossFloor", "🌿 the forest floor", CROSS_FLOORS],
    ["choice", "crossBack", "🌄 off in the distance", CROSS_BACKS],
    // (ticket 212) the open water's distant shore, off by default: its layers, back to front, and its size (bigger: nearer)
    ["group", "seaShore", "🏝️ a distant shore", [
      ["choice", "seaMountains", "⛰️ mountains", SHORE_MOUNTAINS],
      ["choice", "seaForest", "🌲 a forest", SHORE_FORESTS],
      ["toggle", "seaBeach", "🏖️ a beach (between the water and the forest)"],
      ["range", "seaShoreSize", "📏 its size (bigger: nearer)", 25, 300, 50, 200]]],
    // the forest clearing: what is far off down its paths
    ["toggle", "seeHouse", "🏠 see the family house"],
    ["toggle", "seeTree", "🌳 see the tree house"],
    ["toggle", "seeWitch", "🧙 see the witch's hut"],
    ["toggle", "seeCastle", "🏰 see the castle"],
    // the castles: which one stands in the clearing, and each one's own
    // settings (only the one picked shows its sliders)
    ["choice", "castle", "🏰 whose castle", CASTLES.map(([id, label]) => [id, label])],
    ...CASTLES.flatMap(([id]) => [
      ["group", "castleLook" + id, `🏰 ${id}'s castle`, [
        ["range", "castleLevels" + id, "🏰 levels", 1, 5, 1, 5],
        ["color", "castleStone" + id, "🪨 stone", CASTLE_STONES],
        ["range", "castleTrees" + id, "🌳 trees nearby", 0, 16, 0, 12],
        ["range", "castleMoat" + id, "🌊 moat and drawbridge", 0, 250, 0, 200],
        ["color", "castleRoof" + id, "🔺 roofs", CASTLE_ROOFS]]],
      ["group", "castleThemes" + id, `🎨 ${id}'s castle: its color themes`, [
        ["group", "castleThemeA" + id, "🎨 theme A (and the castle's own flags)", [
          ["color", "castleFlagA" + id, "🚩 color 1", CASTLE_FLAG_COLORS],
          ["color", "castleFlagB" + id, "🚩 color 2", CASTLE_FLAG_COLORS],
          ["choice", "castleFlagStyle" + id, "🚩 the two colors", CASTLE_FLAG_STYLES],
          ["color", "castleCarpet" + id, "🟥 carpet", CARPET_COLORS]]],
        ["group", "castleThemeB" + id, "🎨 theme B", [
          ["color", "castleFlagA_B" + id, "🚩 color 1", CASTLE_FLAG_COLORS],
          ["color", "castleFlagB_B" + id, "🚩 color 2", CASTLE_FLAG_COLORS],
          ["choice", "castleFlagStyle_B" + id, "🚩 the two colors", CASTLE_FLAG_STYLES],
          ["color", "castleCarpet_B" + id, "🟥 carpet", CARPET_COLORS]]]]],
      ["group", "castleRoomList" + id, `🚪 ${id}'s castle: its rooms`, [
        ["castlejump", "castleGo" + id, "🚶 go straight to"],
        ["range", "castleRooms" + id, "🚪 how many rooms", 1, ROOM_MAX, 1, 6],
        ["castledesigner", "castleDesign" + id, "🗺️ castle designer"],
      ]],
      ...ROOM_NS.map(n => ["group", "castleRoomBox" + n + id, `🚪 ${id}'s castle: room ${n}`, [
        ["choice", "castleRoom" + n + id, `🚪 room ${n} is`, CASTLE_ROOM_CHOICES],
        // (ticket 252) a trap room's pits' openings, as the dungeons' ceilings below them have them too; (ticket 271) this room's
        ["choice", pitShapeKey(1, n, id), "🕳️ its first pit: its opening", OPENING_SHAPES],
        ["choice", pitShapeKey(2, n, id), "🕳️ its second pit: its opening", OPENING_SHAPES],
        // (ticket 305) each pit there or not, and how the two levers on its wall work them
        ["toggle", pitOnKey(1, n, id), "🕳️ the left pit"], ["toggle", pitOnKey(2, n, id), "🕳️ the right pit"],
        ["toggle", pitSpikesKey(n, id), "🔱 spikes in its pits"],   // (ticket 334)
        ["choice", leverWireKey(n, id), "🎚️ the levers work", LEVER_WIRES],
        // (ticket 323, N's) the blue boulder levers, under the red ones, and their boulders' rock
        ["toggle", boulderOnKey(n, id), "🪨 boulder levers (blue knobs): a boulder falls into a pit"], ["choice", boulderToneKey(n, id), "🪨 the boulders' rock", BOULDER_TONES],
        // (ticket 328) how fast each moves: a share of its usual time, faster above 100
        ["range", leverSpeedKey(n, id), "🎚️ how fast the levers move", 25, 300, 50, 200], ["range", boulderSpeedKey(n, id), "🪨 how fast the boulders fall", 25, 300, 50, 200], ["range", lidSpeedKey(n, id), "🕳️ how fast the trap doors swing", 25, 300, 50, 200],
        ["choice", "castleRoomTheme" + n + id, "🎨 its colors", THEMES],
        ...Object.entries(ROOM_BONES).map(([st, most]) => ["range", roomBonesKey(id, n, st), "💀 skeletons (0 = none)", 0, most, 0, 0]),
        ["range", "castlePlaces" + n + id, "🍽️ places at table", 4, 18, 4, 18],
        // (ticket 506, for M: "chickens -- already in stable 30 in stable.") a stable's hens, pecking about its floor
        ["range", stableHensKey(n, id), "🐔 hens (0 = none, up to 30)", 0, STABLE_HENS_MAX, 1, 12],
        // (ticket 197: the one room's style, not the whole castle built to read it, on every refresh)
        ["range", "castleHallDoors" + n + id, "🚪 doors in its walls", 0, DOOR_MAX, 0, 4, b => roomDoorMax(roomStyleOf(b, id, n))],
        ...DOOR_JS.map(k => ["choice", hallDoorKey(id, n, k), `🚪 door ${k} leads to`, hallDoorDests(id)]),
        ["choice", "castleFar" + n + id, "🚪 the door in its far wall leads to", backDests(id)],
        ["choice", "castleHallEnd" + n + id, "🌀 the stairs at its end lead to", hallEnds(id)],
        ["choice", "castleOut" + n + id, "↩️ its way out leads to", wayOutDests(id, n)],
        ["choice", "castleBack" + n + id, "⬇️ the way toward us leads to", backDests(id)]],
        b => b.kind === "castleroom" && (b.castle ?? "N") === id && Math.round(b.castleRoom ?? 1) === n]),
      // (its residents, each kind in a box of its own, each one in its own box in that: RESIDENTS)
      ...Object.values(RESIDENTS).map(R => ["group", R.group[0] + id, R.group[1](id), [
        ["range", R.count[0] + id, R.group[2], 0, R.count[1], 0, R.count[1]],
        ...R.list.map((_, i) => ["group", R.box[0] + (i + 1) + id, R.box[1](i + 1), R.fields.map(([what, kind, label, opts]) => {
          const key = R.key + (i + 1) + what + id;
          return kind === "range" ? ["range", key, label, ...opts] : kind === "lfo" ? ["choice", key, label, [["none", opts], ...LFO_NAMES.map(L => [L, "LFO " + L])]] : [kind, key, label, opts];
        })])]])]),
    ["toggle", "seeHealer", "🌿 see the healer's hut"],
    // inside the witch's hut (the witch herself: drag her)
    ["range", "witchSize", "🧙 the witch's size (drag her to move her)", 30, 250, 70, 150],
    ["range", "cauldronSize", "🫕 cauldron size", 40, 250, 70, 160],
    ["range", "crows", "🐦‍⬛ crows in cages", 0, 6, 1, 6],
    ["range", "frogs", "🐸 frogs in jars", 0, 8, 2, 8],
    ["range", "snakes", "🐍 snakes in the tank", 0, 6, 1, 5],
    // the fairy forest: its time of day, its ring of toadstools, its fairy house
    ["choice", "fairyTime", "🌗 time of day", [...FAIRY_TIMES, ["clock", "🕰️ by the clock (🌍 where and when)"]]],
    ["range", "mushrooms", "🍄 toadstools in the ring", 4, 30, 8, 24],
    ["range", "fairyHouseSeen", "🏡 the fairy house: how much it shows (0 = hidden)", 0, 100, 40, 100],
    ["choice", "fairyHouseLfo", "〰️ the fairy house fades with", [["none", "no LFO"], ...LFO_NAMES.map(L => [L, "LFO " + L])]],
    // inside the healer's hut (the healer herself: drag her)
    ["range", "healerSize", "🌿 the healer's size (drag her to move her)", 30, 250, 70, 150],
    ["color", "healerCatFur", "🐈 the healer's cat: fur", ["#e8862a", "#1f1f22", "#6b4226", "#9ca3af", "#ffffff", "#8b5cf6"]],
    ["color", "healerCatWhite", "🐈 the healer's cat: tuxedo (nose, chest, paws)", ["#ffffff", "off", "#f7ead8", "#ffd1dc", "#c0c0c0", "#1f1f22"]],
    ["color", "healerCatEyes", "🐈 the healer's cat: eyes", ["#22c55e", "#c6d43a", "#e0a800", "#3b82f6", "#f28c28"]],
    // the living room
    ["choice", "view", "🪟 outside the window", PICTURE_CHOICES],
    ["choice", "livingOut", "⬇️ the way toward us (the door out) leads to", BEHIND_PLACES],
    ["range", "viewDetail", "🪟 outside the window: detail (0 = a blur, 100 = all of it)", 0, 100, 30, 100],
    ["range", "couchSize", "🛋️ couch size", 60, 140, 80, 120],
    ["range", "pillows", "🛋️ pillows", 0, 8, 2, 7],
    // (ticket 278) each living room's own colors: the house's, and the grass hut's
    ["color", "couchColor", "🛋️ couch color", COUCH_COLORS],
    ["choice", "rugColors", "🟠 rug colors", RUG_CHOICES],
    // (ticket 282) the grass hut from outside: one hut, or a cluster of them, and a bigger one in the middle
    ["range", "hutRooms", "🛖 huts in the cluster (its rooms)", 1, 6, 1, 6],
    ["toggle", "hutCenter", "🔥 the hut center: a bigger hut in the middle, the way in"],
    ["color", "hutCouchColor", "🛋️ couch color", ["#7a5a3a", ...COUCH_COLORS.filter(x => x !== "#7a5a3a")]],
    ["choice", "hutRugColors", "🟠 rug colors", RUG_CHOICES],
    ["color", "hutBlanket", "🛏️ blanket color", ["#b8434f", "#2f6f86", "#5f7f3a", "#6a4c93", "#c98a2a", "#f6eedd"]],
    ["color", "hutPotion", "🧪 the potion's color", ["#7c3aed", "#22c55e", "#ef4444", "#3b82f6", "#f59e0b", "#ec4899"]],
    // pictures on the walls, of the other scenes
    ["range", "pictures", "🖼️ pictures on the wall", 0, 6, 1, 6],
    ["range", "picDetail", "🖼️ picture detail (0 = a blur, 100 = all of it)", 0, 100, 20, 100],
    ["range", "picSize", "🖼️ picture size", 50, 150, 70, 130],
    ["choice", "pic1", "🖼️ picture 1", PICTURE_CHOICES],
    ["choice", "pic2", "🖼️ picture 2", PICTURE_CHOICES],
    ["choice", "pic3", "🖼️ picture 3", PICTURE_CHOICES],
    ["choice", "pic4", "🖼️ picture 4", PICTURE_CHOICES],
    ["choice", "pic5", "🖼️ picture 5", PICTURE_CHOICES],
    ["choice", "pic6", "🖼️ picture 6", PICTURE_CHOICES],
  ]],
  ["🎮 Controls", "controls", [
    ["padmap", "map", "🎮 game controller"],
    // (ticket 221, Matthew: "a new 'create bubbles' action for the player, where they can blow bubbles out of their
    // mouth of any style (up to three at once) in any scene, or either hand. They can choose which of the three places
    // they come from.") Held (🫧 on the controller, B on the keyboard): from the mouth or a hand, of the themes switched on here
    ["of", "body", ["group", "blowing", "🫧 blowing bubbles", [
      ["choice", "blowFrom", "from", [["mouth", "👄 mouth"], ["left", "🫲 left hand"], ["right", "🫱 right hand"]]],
      ["flip", "blowA", ["🅰️ theme A", "🅰️ theme A"]], ["flip", "blowB", ["🅱️ theme B", "🅱️ theme B"]], ["flip", "blowC", ["©️ theme C", "©️ theme C"]],
    ]]],
    ["of", "background", ["group", "blowThemes", "🎨 the bubble themes", BUBBLE_THEME_CTLS.map(([kd, key, label, ctls]) => [kd, key, label, ctls.filter(c => c[0] !== "flip")])]],   // (the looks only: B and C on in bubble space is bubble space's)
    // (ticket 499, Matthew: "optional dragon breath -- need an action for that, like bubbles -- shooting fire out of our mouth.";
    // "Mock up the dragon costume and 6 fire styles.") Held (🔥 on the controller, F on the keyboard): one of the mock-up's six
    // fires, from the mouth or a hand, as the bubbles' from; its heat pops bubbles, and shows as a shimmer if asked
    ["of", "body", ["group", "firing", "🔥 breathing fire", [
      ["choice", "fireStyle", "🔥 its fire", FIRE_STYLES],
      ["choice", "fireFrom", "🧭 from where", [["mouth", "👄 mouth"], ["left", "🫲 left hand"], ["right", "🫱 right hand"]]],
      ["range", "fireSize", "📏 how big (100 = as drawn)", 50, 200, 80, 140],
      ["flip", "fireHeat", ["🌡️ show its heat", "🌡️ show its heat"]],
    ]]],
    // (ticket 294, Matthew: "let the player choose which they prefer, and repeat that same choice under ♿ Accessibility")
    // a click while walking to one: ♿'s own setting, shown here too while 👆 walk where you click is on
    ["of", "access", ["choice", "clickPlan", "👆 a click while walking to one", CLICK_PLANS]],
    // (ticket 188, Matthew) how the friend moves: kept with the friend (body), not with a controller's buttons
    ["of", "body", ["group", "moving", "🏃 how your friend moves", [
      ["range", "runSpeed", "🏃 top running speed (100 = as it was)", 50, 250, 100, 100],
      ["range", "accel", "🚀 speeding up and slowing down (0 = at once)", 0, 100, 0, 0],
    ]]],
  ]],
  ["🎵 Music", "music", [
    ["musicbig", "big", "⛶ full screen"],
    ["flip", "mute", ["🔊 sound on: tap to mute", "🔇 muted: tap for sound"]],
    ["note", "now", nowPlaying],
    ["transport", "where", "⏯️ where the tune is"],
    ["tunes", "track", "🎶 genre/track selection"],   // (ticket 226, Matthew: "reduce that long text to 'genre/track selection'")
    ["range", "volume", "🔊 volume", 0, 100, 40, 80],
    ["range", "tempo", "🏃 tempo (% of the tune's own speed)", 25, 300, 80, 130],
    ["range", "transpose", "🎹 transpose (half steps: + higher, − lower)", -24, 24, -5, 5],
    ["nudge", "transpose", "🎹 move the tune by an interval", -24, 24, "interval"],
    ["mixer", "mix", "🎚️ mixing board"],
  ]],
  // (ticket 185, Matthew) after the music, before Advanced: the world designer
  ["🗺️ World design", "world", [
    ["worldbig", "big", "⛶ full screen"],
    ["world", "grid", "🗺️ world design"],
  ]],
  // (ticket 155) last, where the GitHub chip was
  // (ticket 217, Matthew) before Advanced: what helps a player see and read
  // (Matthew, 2026-10-17: "lets change accessibility emoji to a microscope emoji"; then: "Let's rename it --- 'inspection',
  // since that's the new scope and use case": it shows what places are made of, ticket 258, as well as helping a player)
  ["🔬 Inspection", "access", [
    // (These overlays are for showing someone new to video games how it works, and for showing a problem as it is seen.)
    // (ticket 230) each a row, accbox: its chip, and while it is on, its colour and its sizes beside it, boxed; the words'
    // size in a box of its own
    ["accbox", "allArrows", "🧭 all arrows", [["color", "arrowColor", "🎨"], ["range", "arrowDim", "🌗 brightness", 10, 100], ["range", "arrowSize", "↕️ size", 50, 250]]],
    ["accbox", "showPath", "👣 planned path", [["color", "pathColor", "🎨"], ["color", "pathAltColor", "🎨 alt"], ["range", "pathWidth", "✏️ line", 1, 12]]],   // (ticket 266: a queue of clicks' walks by turns)
    ["accbox", "showSolids", "🧱 collision boxes", [["color", "boxColor", "🎨"], ["range", "boxWidth", "✏️ line", 1, 8], ["range", "boxFont", "🏷️ names", 8, 24],
      ["range", "glowFade", "✨ glow ms", 100, 3000], ["solidcats", "solidCats"]]],   // (ticket 283: each kind it draws, a chip)   // (ticket 229: a collision's glow, fading back in so long)
    // (ticket 290, Matthew: "We could have a new sub-cat for bubble collisions -- they are all circles right!") the bubble
    // room's stuck-out bubbles, each a circle that pops when the friend walks into it
    ["accbox", "showBubbleHits", "🫧 bubbles that pop (circles)", [["color", "bubbleHitColor", "🎨"]]],
    // (ticket 283) the game's own rules, each a toggle: where the friend is in water (not a collision box: a test of its own)
    ["accbox", "showWater", "💧 water (where you wade or swim)", [["color", "waterColor", "🎨"]]],
    // (ticket 258, Matthew: "a toggle that reveals a sub-panel of toggles, per category of object") each kind of thing a
    // chip of its own, with how many are here (madeOfHud)
    // (Matthew: "let's give 'what its made of' boxes the same stroke and color controls and the other inspection toggles")
    ["accbox", "showMadeOf", "🧩 what it's made of", [["color", "madeColor", "🎨"], ["range", "madeWidth", "✏️ line", 1, 8], ["range", "madeFont", "🏷️ names", 8, 20], ["cats", "madeCats"]]],
  ]],
  // (Matthew, 2026-10-17: "move 'words in the game' to a new 'accessibility' chip that only has that, for now, and another
  // copy of the 'all arrows' chip, since that's also useful --- it's the same setting, duplicated") the same settings
  // (state.access) as 🔬 Inspection's: all arrows in both
  ["♿ Accessibility", "access", [
    ["accbox", "allArrows", "🧭 all arrows", [["color", "arrowColor", "🎨"], ["range", "arrowDim", "🌗 brightness", 10, 100], ["range", "arrowSize", "↕️ size", 50, 250]]],
    // (ticket 404, Matthew: "they disappear too quickly -- let's slow it down by a control in accessibility, default 2x
    // slower than now, with room on each side in slider, but more room up than down") how long an arrow's words stay
    // once the pointer leaves it: 250 ms was the old fade, now the slider's least; 500 the default; up to 1500
    ["accbox", null, "🔤 words in the game", [["range", "gameFont", "🔤 text size", 60, 250], ["range", "labelStay", "⏳ arrow words stay, ms", 250, 1500]]],
    // (ticket 251) the ground under the pointer glowing where the friend can go, and a click there walks them to it
    ["accbox", "clickWalk", "👆 walk where you click", [["color", "clickWalkColor", "🎨"], ["choice", "clickPlan", "👆 a click while walking", CLICK_PLANS]]],
  ]],
  // (ticket 482) the game's logic in Hazel
  ["📜 Scripting", "scripting", [
    ["scripting", "scripts", "📜 scripting"],
  ]],
  ["⚙️ Advanced", "advanced", [
    ["advanced", "perf", "⚙️ advanced"],
  ]],
];

let tab = 0;
let syncs = [];   // each control's refresh, run after every change
// (ticket 379) a sync writes the page only where it changes: the same value written again is still a change to the browser, which
// styles the panel anew; one round in 🏞️ Background wrote 3,995 times, 3,773 of them what was already there (aria-pressed, hidden,
// an eye's 👁️), 45,000 times in a 3 s swim against the 😀 Face tab's 700
const setPressed = (el, on) => { const v = typeof on === "string" ? on : String(!!on); if (el.getAttribute("aria-pressed") !== v) el.setAttribute("aria-pressed", v); };
const setHidden = (el, h) => { h = !!h; if (el.hidden !== h) el.hidden = h; };
const setText = (el, t) => { t = String(t); if (el.textContent !== t) el.textContent = t; };
// (ticket 236: the 🧩 chips' counts alone, which madeOfHud refreshes from inside a drawing: all of syncs there re-ran
// every control's refresh, the things guard's among them, clearing its latch whenever what a place is made of was shown)
let madeSyncs = [], hudMadeSyncs = [];   // (the pinned controls' own, renderHud's)
let hudSyncs = [];   // ...and the full-screen controls'

function set(part, key, value) {
  if (part === "background" && key === "kind" && value !== state.background.kind) worldChoose(value);   // (ticket 185: a place is its square)
  const wasBare = state.background.kind === "none", wasKind = part === "costume" && key === "kind" ? state.costume.kind : null;
  state[part][key] = value;
  // (ticket 371, Matthew: "something about the tank seems buggy. Switched to fairy costume and its still there?") the diving
  // gear (ticket 288: worn with any costume) comes off with a diving suit: out of the scuba suit or the old one, into another
  // costume, the snorkel, the mask and the tank too; any of them can be put on again with any costume
  if (wasKind && DIVING_SUITS.has(wasKind) && !DIVING_SUITS.has(value)) Object.assign(state.costume, { snorkel: false, mask: false, tank: false });
  if (part === "access" && key === "clickWalk") state.access.clickWalkOn = true;   // (0.0.188: turned on or off by the player: kept so, upgradeClickWalk)
  // (ticket 132) the Body tab: a pose, from scratch; an animation seen from
  // the side turns a friend who faces us to our right
  if (part === "body" && key === "pose") strikePose(value);
  if (part === "body" && key === "anim" && ANIM_SIDEWAYS[value] && state.body.facing === "front") state.body.facing = "right";   // (away from us, they step in place: ticket 138)
  if (part === "body" && key === "anim" && value === "climb") { state.body.facing = "front"; state.body.turn = ""; }   // (the ladder is behind them, facing us)
  if (part === "body" && key === "facing") state.body.turn = "";   // (ticket 144: a chip's way, not turned)
  if (part === "body" && key === "anim" && value === "climbup") state.body.facing = "back";   // (up a place's ladder, facing it: ticket 138)
  if (part === "body" && key === "anim") climb = null;   // (ticket 137: a climb starts afresh)
  if (part === "body" && (key === "lieHead" || key === "lieHow") && state.body.pose === "lie") strikePose("lie");   // (ticket 299: turned the new way)
  if (part === "body" && key === "anim" && isJump(value)) startJump("none", value);   // (ticket 138: once, then still)
  // (walking, running, crawling and climbing start from standing: sitting
  // or cross-legged, they had nothing to move with)
  if (part === "body" && key === "anim" && value !== "none" && value !== "swim" && state.body.pose !== "stand") strikePose("stand");
  // choosing an animal dresses it in its natural colors
  if (part === "costume" && key === "kind" && ANIMALS[value]) Object.assign(state.costume, lookOf(value));
  // (ticket 536) and a moth in its style's: choosing a style, or the moth, puts on that style's body colours (the animal
  // colour rows change them after)
  if (part === "costume" && (key === "mothStyle" || key === "kind") && state.costume.kind === "moth") Object.assign(state.costume, MOTH_STYLES[mothStyleOf(state.costume)].body);
  // (ticket 442) the wizard and the gnome man wear a beard, from 💇 Hair: put on (white; long, or full) when the friend has
  // none, and taken off again on leaving the costume, or the gnome becoming a woman, if it is still the one put on
  if (part === "costume" && (key === "kind" || key === "gnomeWho")) {
    const C = state.costume, H = state.hair, want = C.kind === "wizard" ? "long" : C.kind === "king" || (C.kind === "gnome" && C.gnomeWho !== "woman") ? "full" : null;
    if (C.beardGiven && C.beardGiven !== want) { if (H.beard === C.beardGiven) { H.beard = "none"; if (H.beardShade === BEARD_WHITE) H.beardShade = "auto"; } C.beardGiven = null; }
    if (want && !BEARDS[H.beard]) { H.beard = want; H.beardShade = BEARD_WHITE; C.beardGiven = want; }
  }
  // (ticket 433: a monk's cloak brown and a dryad's moss green, while it is one of those two)
  if (part === "costume" && key === "monkWho" && ["#6b4a2b", "#4f6b3a"].includes(state.costume.monkCloak)) state.costume.monkCloak = value === "dryad" ? "#4f6b3a" : "#6b4a2b";
  // (ticket 431: as on the book's cover, the gnome man's hat red and the woman's purple, while the hat is one of those two)
  if (part === "costume" && key === "gnomeWho" && ["#d2382b", "#7b3f8c"].includes(state.costume.gnomeHat)) state.costume.gnomeHat = value === "woman" ? "#7b3f8c" : "#d2382b";
  // (ticket 522: choosing a cowgirl's or cowboy's style, or the costume, puts on the style's usual hat, until a kid picks a
  // hat themselves (cowHatAuto false), so a hat a kid picked is never changed)
  if (part === "costume" && key === "cowHat") state.costume.cowHatAuto = false;
  if (part === "costume" && (key === "cowgirlStyle" || key === "cowboyStyle" || (key === "kind" && (value === "cowgirl" || value === "cowboy")))) {
    const C = state.costume, k = key === "kind" ? value : key === "cowgirlStyle" ? "cowgirl" : "cowboy", T = k === "cowgirl" ? COWGIRL_STYLES : COWBOY_STYLES;
    const S = T[C[k + "Style"]] || Object.values(T)[0];
    if (C.kind === k && C.cowHatAuto !== false) C.cowHat = S.hat;
  }
  // a skirt starts at skirt length and a dress at dress length
  if (part === "pants" && key === "style" && value === "skirt") state.pants.skirtLen = 50;
  if (part === "pants" && key === "style" && value === "dress") state.pants.skirtLen = 95;
  if (part === "background" && key === "kind") trips.length = 0;
  // you are always inside a house the tree has: going into house 6 grows
  // the tree to six houses, and fewer houses take you to the last one left
  if (part === "background" && key === "inside" && value > (state.background.houses ?? 4)) state.background.houses = value;
  if (part === "background" && key === "houses" && (state.background.inside ?? 1) > value) state.background.inside = value;
  // the same for the tents: going into tent 3 puts up three
  if (part === "background" && key === "tent" && value > (state.background.tents ?? 2)) state.background.tents = value;
  if (part === "background" && key === "tents" && value > 0 && (state.background.tent ?? 1) > value) state.background.tent = value;
  // arriving at a place lands the friend on its ground (not in the fire)
  if (part === "background" && key === "kind") { [state.background.figX, state.background.figY] = ARRIVE[value] || [0, 0]; arriveSettle = true; }
  // different hands start alike; then each changes on its own
  if (part === "hands" && key === "different" && value)
    for (const k of HAND_OWN) state.rightHand[k] = state.hands[k];
  if (part === "shoes" && key === "different" && value)
    for (const k of SHOE_OWN) state.rightShoe[k] = state.shoes[k];
  // a bikini comes with its top; trunks and briefs start without one
  if (part === "undies" && key === "style") state.undies.top = value === "bikini";
  // a figure too big for the stage, once a background is chosen, shrinks
  // to fit it (once: after that its size is the slider's)
  if (part === "background" && key === "kind" && value !== "none" && wasBare) {
    // (the usual figure, at its usual size, stands well inside the stage:
    // only a much bigger one shrinks)
    const n = naturalFrame, fit = Math.min((SCENE.ground - SCENE.top) / (n.ground - n.top), SCENE.left / n.left);
    if (fit < 0.98 * figScale(state.background) * sceneFig(value))
      state.background.figSize = Math.max(10, Math.floor(100 * fit / (sceneFig(value) * bodyScale())));   // (ticket 457: their own size too)
  }
  changed();
}
// (ticket 163) A frame of movement (the controller walking or climbing, a
// drag) does what an animation with no background does: it draws, and
// feeds the music. The rest of changed() (every control's refresh, the
// full-screen controls, the save to localStorage, the music's and the back
// chip's state) waits for settle(), a few times a second while it goes on,
// and once when it stops: on a slow computer (the black laptop) those, every
// frame, were what a walk in a place had and an animation with no
// background did not.
// And on a computer where a drawing costs more than a frame can spare
// (DRAW_SLOW ms, on average), the friend is drawn at most every MOVE_EVERY
// ms while they move: as an animation with no background is (treadTick),
// which looked fine on the black laptop when every place did not. The
// picture's own frames stay at 60; the friend's place and pose catch up
// 30 times a second. The last move is always drawn (force).
const DRAW_SLOW = 6, MOVE_EVERY = 30;
let settleTimer = 0, drawAvg = 0, movedLast = 0, movedTrail = 0, movedDraws = 0, movedMs = 0;
function moved(force = false) {
  const t = performance.now();
  if (!force) noteMoveFrame(t);   // (ticket 164: frames dropping while moving)
  stillPicMoved(t);   // (ticket 164: the place a still picture while moving, on a slow computer)
  if (!force && drawAvg > DRAW_SLOW && t - movedLast < MOVE_EVERY) {
    // (the latest move drawn soon even if no other comes: a held-still drag, a stop)
    if (!movedTrail) movedTrail = setTimeout(() => { movedTrail = 0; moved(true); }, MOVE_EVERY - (t - movedLast));
    if (!settleTimer) settleTimer = setTimeout(settle, 250);
    return;
  }
  clearTimeout(movedTrail); movedTrail = 0;
  movedLast = t;
  draw();
  const ms = performance.now() - t;
  drawAvg = drawAvg ? 0.85 * drawAvg + 0.15 * ms : ms;
  perfMeter.ms += ms; perfMeter.draws++; movedDraws++; movedMs += ms;
  feedMusic();
  if (!settleTimer) settleTimer = setTimeout(settle, 250);
}
// (ticket 379, from 0.0.327's speed run: walking with 🏞️ Background open, 56 frames a second and 7% of them late, every other tab
// 59.5 to 60: its round of syncs, 4,239 of them, 6.6 ms in the app's pane, ran inside one frame every 250 ms) While the friend is
// moving, the round goes in slices of at most SETTLE_SLICE ms, one a frame, the rest of settle after the last; still (the round
// when they stop), or in a hidden tab (no frames), all at once as before. A settle due while a round is going on runs after it.
const SETTLE_SLICE = 3;
let settling = null;
function settle() {
  clearTimeout(settleTimer); settleTimer = 0;
  if (settling) { settling.again = true; return; }
  worldKeep();   // (ticket 185: a forest crossing edited in its square, kept there, before it is saved)
  flagCastles = null;   // (ticket 365, as in changed)
  const rest = () => { for (const f of hudSyncs) f(); persist(); musicSync(); backSync();
    if (!hud.hidden && hudKey() !== hudShown) renderHud(); };
  if (performance.now() - movedLast > 200 || document.hidden) { for (const f of syncs) f(); rest(); return; }
  const list = syncs, job = settling = { i: 0, again: false };
  const step = () => {
    if (syncs !== list) { settling = null; return; }   // (the tab built again: its own round, changed's, has run)
    const t = performance.now();
    while (job.i < list.length) { list[job.i++](); if (!(job.i & 31) && performance.now() - t > SETTLE_SLICE) break; }
    if (job.i < list.length) { requestAnimationFrame(step); return; }
    settling = null; rest();
    if (job.again && !settleTimer) settleTimer = setTimeout(settle, 0);
  };
  step();
}
function changed() {
  worldKeep();   // (ticket 185, as in settle: before the drawing, which reads a crossing's live paths from it)
  const t0 = performance.now(); draw(); perfMeter.ms += performance.now() - t0; perfMeter.draws++;   // (ticket 155: for the ⚙️ advanced tab's meter)
  flagCastles = null;   // (ticket 365: each round of syncs reads the castles afresh, once: flagCastle)
  for (const f of syncs) f(); for (const f of hudSyncs) f(); persist(); musicSync(); backSync(); feedMusic();
  // a new friend (loaded, started over, surprised) or a scene turned on or
  // off rebuilds the HUD; a slider moving in it does not
  if (!hud.hidden && hudKey() !== hudShown) renderHud();
}

// (ticket 185, v3) the object library's group open, while the game is open
let thingGroup = "nature", liftPlace = null;   // (ticket 284: the place 📦 takes from, once chosen)
// (ticket 200) VERSIONS.md as cards: a version's heading (its number and
// date), its paragraphs and its points, **bold**, `code` and [links] as such,
// all else as text; fetched once (afresh) and kept while the game is open
let newsHtml = null;
const newsInline = t => t.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
  .replace(/\*\*(.+?)\*\*/g, "<b>$1</b>").replace(/`([^`]+)`/g, "<code>$1</code>")
  .replace(/\[([^\]]+)\]\((https?:[^)\s]+)\)/g, '<a href="$2" target="_blank" rel="noopener">$1</a>');
function newsCards(md) {
  const out = [];
  let card = null, list = null;
  for (const line of md.split("\n")) {
    const h = line.match(/^## (\S+)(?: \(([^)]*)\))?/);
    if (h) { if (card) out.push(card + (list ? "</ul>" : "") + "</div>"); list = null;
      card = `<div class="news-card${h[1] === VERSION.number ? " now" : ""}" data-version="${newsInline(h[1])}"><div class="news-head"><b>${newsInline(h[1])}</b>${h[2] ? ` <span class="hint">${newsInline(h[2])}</span>` : ""}${h[1] === VERSION.number ? ' <span class="news-this">this one</span>' : ""}</div>`; continue; }
    if (!card) continue;
    const sub = line.match(/^#{3,} (.*)$/);
    if (sub) { if (list) { card += "</ul>"; list = null; } card += `<div class="news-sub">${newsInline(sub[1])}</div>`; continue; }
    const li = line.match(/^\s*[-*] (.*)$/);
    if (li) { if (!list) { card += "<ul>"; list = true; } card += `<li>${newsInline(li[1])}</li>`; continue; }
    if (line.trim()) { if (list) { card += "</ul>"; list = null; } card += `<p>${newsInline(line.trim())}</p>`; }
  }
  if (card) out.push(card + (list ? "</ul>" : "") + "</div>");
  return out.join("");
}
function newsLoad(box) {
  if (newsHtml) { box.innerHTML = newsHtml; return; }
  fetch(`VERSIONS.md?v=${VERSION.number}`, { cache: "no-store" }).then(r => r.ok ? r.text() : Promise.reject(r.status))
    .then(md => { newsHtml = newsCards(md) || '<div class="hint">No versions found.</div>'; if (box.isConnected) box.innerHTML = newsHtml; })
    .catch(() => { if (box.isConnected) box.innerHTML = '<div class="hint">📰 The list of versions could not be read just now.</div>'; });
}
// (ticket 201, Matthew: N put fifteen or more cauldrons in one place, and the
// Ubuntu laptop ground to a halt) Things that move (a cauldron: its flames,
// bubbles and fumes, about 240 moving parts, the fumes' spiral most of them;
// measured on GitHub's computer, still: 0 cauldrons 60 frames a second, 5
// 49, 10 25, 15 14; a campfire, about twenty)
// are watched: after one is put in a place, and every ten seconds while the
// things are listed, a second of this computer's frames (framesNow: the
// browser's own pace of frames, cheap); below THING_FPS_MIN, or more than
// THING_MOVING_MAX moving parts in the place's things, no more moving
// things can be put there (their buttons greyed, 🐢), and the things moving
// the most are marked, in their rows and in the picture, as the ones to take away.
const THING_FPS_MIN = 48, THING_MOVING_MAX = 1000;   // (about four cauldrons: GitHub's computer near 50 there)
let thingFps = null, thingFpsAt = 0, thingFpsBusy = false;
// (ticket 203, Matthew: "when any kind of object has a red box around it, no
// more objects of any kind can be added until the red box is resolved"; N
// kept adding cauldrons past the marks) Too busy once, the place stays shut
// (thingsLatch): every button greyed and adding refused (a drag too), till
// it is emptied, or a fresh second of frames, measured after a thing is
// taken away, is smooth again (THING_FPS_MIN and a little more)
let thingsLatch = false, thingsMeasured = "";
const thingMoving = el => el.querySelectorAll("animate, animateTransform, animateMotion").length + (el.getAnimations ? el.getAnimations({ subtree: true }).length : 0);
const thingsMovingNow = () => [...document.querySelectorAll('#bg [data-drag^="thing:"]')].map(el => ({ i: +el.dataset.drag.slice(6), el, n: thingMoving(el) }));
// (ticket 206, Matthew: "red dots for both mandatory and placed objects") the place's own cauldron (the
// witch's, in her woods and her hut), its fumes upgraded to fancy: counted and marked as a thing put there is
const ownMovingNow = () => state.background.cauldronFumes === "fancy" ? [...document.querySelectorAll('#bg [data-obj="cauldron"]')].map(el => ({ i: "own", el, n: thingMoving(el) })) : [];
function framesNow(ms = 1000) {
  return new Promise(res => { let n = 0, done = false; const t0 = performance.now();
    const end = () => { if (done) return; done = true; res(Math.round(10 * n * 1000 / Math.max(1, performance.now() - t0)) / 10); };
    const f = t => { if (done) return; n++; if (t - t0 < ms) requestAnimationFrame(f); else end(); };
    requestAnimationFrame(f); setTimeout(end, ms * 1.6); });   // (a hidden page gives no frames: it ends anyway)
}
function thingCheck(then) {
  if (thingFpsBusy || document.hidden) return;
  thingFpsBusy = true;
  setTimeout(() => framesNow().then(fps => { thingFps = fps; thingFpsAt = performance.now(); thingFpsBusy = false; if (then) then(); }), 300);
}
// (ticket 224: the world designer's; ticket 285, Matthew: the background designer's too, "a key feature of world designer")
// The dragger between a designer's two sides: its --left, a share of the layout's width, kept in this browser under key
function designerSplit(layout, split, key) {
  try { const v = localStorage.getItem(key); if (v) layout.style.setProperty("--left", v); } catch (e) { /* (the usual split) */ }
  split.addEventListener("pointerdown", e => {
    e.preventDefault(); split.setPointerCapture(e.pointerId);
    const move = ev => { const r = layout.getBoundingClientRect(); layout.style.setProperty("--left", clamp(100 * (ev.clientX - r.left) / r.width, 25, 80).toFixed(1) + "%"); };
    const up = () => { split.removeEventListener("pointermove", move); try { localStorage.setItem(key, layout.style.getPropertyValue("--left")); } catch (err) { /* (for this visit) */ } };
    split.addEventListener("pointermove", move); split.addEventListener("pointerup", up, { once: true }); split.addEventListener("pointercancel", up, { once: true });
  });
}
function el(tag, attrs = {}, kids = []) {
  const e = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (k.startsWith("on")) e.addEventListener(k.slice(2), v); else e.setAttribute(k, v);
  }
  for (const k of [].concat(kids)) e.append(k);
  return e;
}

// Iris color and eye whites only mean something once there is an iris.
const SHOWN_WHEN = {
  // (ticket 212) the open water's distant shore, there only; its size once there is a shore
  "background.seaShore": b => b.kind === "sea",
  "background.seaShoreSize": b => (b.seaMountains ?? "none") !== "none" || (b.seaForest ?? "none") !== "none" || !!b.seaBeach,
  // (ticket 230: the ♿ settings show beside their toggle, while it is on: accbox)
  "body.unpose": posed,
  "body.lieHead": b => b.pose === "lie", "body.lieHow": b => b.pose === "lie",   // (ticket 299)
  "body.stairsWay": b => b.anim === "stairs",   // (ticket 308)
  "access.clickPlan": a => !!a.clickWalk,   // (ticket 294: in 🎮 Controls, while 👆 is on)
  "body.hugLift": b => b.pose === "hug",
  "face.irisColor": f => f.iris > 0,
  "face.earSize": f => !!f.ears && f.ears !== "none",
  "face.nose": f => f.noseView === "right" || f.noseView === "left",
  "face.noseFront": f => f.noseView === "front",
  "face.witchDir": f => f.noseView === "witch",
  "face.witchLen": f => f.noseView === "witch",
  "face.teeth": f => f.teethStyle === "spikes",
  "face.tongueLen": f => f.tongue === "out",
  "face.tongueW": f => f.tongue === "out",
  "face.tongueColor": f => f.tongue !== "none",
  "face.noseH": f => f.noseView !== "none",
  "face.noseW": f => f.noseView !== "none",
  "face.whites": f => f.iris > 0,
  "costume.fur": c => !!ANIMALS[c.kind] && c.kind !== "dragon" && c.kind !== "sasquatch" && c.kind !== "giraffe",   // (ticket 499: a dragon's colours are its own, below)
  "costume.furPattern": c => !!(ANIMALS[c.kind] && ANIMALS[c.kind].suit) && c.kind !== "dragon" && c.kind !== "sasquatch" && c.kind !== "giraffe",
  ...Object.fromEntries(["dragonScales", "dragonBelly", "dragonWings", "dragonHorns", "dragonWingsHow"].map(k => ["costume." + k, c => c.kind === "dragon"])),
  "costume.dragonWingLfo": c => c.kind === "dragon" && (c.dragonWingsHow ?? "flap") === "flap",
  // (ticket 536) the moth's rows: each style's wing colour only while it is chosen; the eyespots for a style that has them
  "costume.mothStyle": c => c.kind === "moth",
  ...Object.fromEntries(Object.keys(MOTH_STYLES).map(id => ["costume.mothColor_" + id, c => c.kind === "moth" && mothStyleOf(c) === id])),
  "costume.mothSpots": c => c.kind === "moth" && (S => !!(S.spot[3] || S.foreSpot))(MOTH_STYLES[mothStyleOf(c)]),
  "costume.witchColor": c => c.kind === "witch",
  "hair.beardShade": h => !!BEARDS[h.beard], "hair.moustache": h => !!BEARDS[h.beard], "hair.beardLength": h => !!BEARDS[h.beard],
  "hair.braidWidth": h => !!(BEARDS[h.beard] && BEARDS[h.beard].braid),
  "costume.wizardRobe": c => c.kind === "wizard", "costume.wizardStars": c => c.kind === "wizard", "costume.staff": c => c.kind === "wizard" || c.kind === "monk",
  "costume.queenRobe": c => c.kind === "queen", "costume.kingRobe": c => c.kind === "king", "costume.crownJewel": c => c.kind === "queen" || c.kind === "king",
  "costume.mantle": c => c.kind === "queen" || c.kind === "king", "costume.scepter": c => c.kind === "queen" || c.kind === "king",
  "costume.knightA": c => c.kind === "knight", "costume.knightB": c => c.kind === "knight" && c.tabardLayout !== "plain" || c.kind === "knight" && c.emblem !== "none",
  "costume.tabardLayout": c => c.kind === "knight", "costume.emblem": c => c.kind === "knight", "costume.plume": c => c.kind === "knight",
  "costume.plumeColor": c => c.kind === "knight" && c.plume !== false, "costume.helmOpen": c => c.kind === "knight", "costume.sword": c => c.kind === "knight", "costume.shield": c => c.kind === "knight",
  "costume.robinTunic": c => c.kind === "robin", "costume.robinHat": c => c.kind === "robin", "costume.robinFeather": c => c.kind === "robin",
  "costume.bow": c => c.kind === "robin", "costume.quiver": c => c.kind === "robin",
  "costume.monkWho": c => c.kind === "monk", "costume.monkCloak": c => c.kind === "monk", "costume.hoodUp": c => c.kind === "monk", "costume.bareFeet": c => c.kind === "monk",
  "costume.hoodFace": c => c.kind === "monk" && c.hoodUp !== false, "costume.hoodDim": c => c.kind === "monk" && c.hoodUp !== false && c.hoodFace !== false,
  "costume.hoodEyes": c => c.kind === "monk" && c.hoodUp !== false && c.hoodFace === false, "costume.staffOrb": c => c.kind === "wizard" && c.staff !== false,
  "costume.gnomeWho": c => c.kind === "gnome", "costume.gnomeHat": c => c.kind === "gnome", "costume.gnomeBoots": c => c.kind === "gnome",
  "costume.gnomeTunic": c => c.kind === "gnome" && c.gnomeWho !== "woman", "costume.gnomeTrousers": c => c.kind === "gnome" && c.gnomeWho !== "woman",
  "costume.gnomeBodice": c => c.kind === "gnome" && c.gnomeWho === "woman", "costume.gnomeSkirt": c => c.kind === "gnome" && c.gnomeWho === "woman",
  ...Object.fromEntries(["wingStyle", "wingFlutter", "fairyOutfit", "tunicStyle", "fairyShoes", "fairyShoeColor"].map(k => ["costume." + k, c => c.kind === "fairy"])),
  "costume.wingLfo": c => c.kind === "fairy" && c.wingFlutter !== false,
  "costume.wreath": c => c.kind === "fairy",
  // (ticket 495) the mark's colour with a mark; the saddle's and its blanket's while ridden (the only time they are drawn)
  "body.horseLead": b => b.horse === "lead",   // (ticket 496)
  "body.horseBows": b => (b.horse || "none") !== "none" && b.horseManeStyle === "bows",   // (ticket 497)
  "body.horseMarkColor": b => (b.horse || "none") !== "none" && (b.horseMark || "none") !== "none",
  // (ticket 498: the parade armour's cloth is its saddle blanket, led too)
  "body.horseSaddle": b => b.horse === "ride", "body.horseBlanket": b => b.horse === "ride" || (b.horse === "lead" && b.horseArmour === "parade"),
  ...Object.fromEntries(["horseCoat", "horseMane", "horseManeStyle", "horseArmour", "horseMark", "horseTack", "horseSocks", "horseSize", "horseShoes"].map(k => ["body." + k, c => (c.horse || "none") !== "none"])),
  ...Object.fromEntries(["ghostRagged", "ghostEyes", "ghostHideEyes", "ghostColor"].map(k => ["costume." + k, c => c.kind === "ghost"])),
  "costume.wreathFlowers": c => c.kind === "fairy" && c.wreath !== false,
  ...Object.fromEntries(Object.keys(FAIRY_WINGS).map(id => ["costume.wingColor_" + id, c => c.kind === "fairy" && (c.wingStyle ?? "butterfly") === id])),
  ...Object.fromEntries(Object.keys(FAIRY_TUNICS).map(id => ["costume.tunicColor_" + id, c => c.kind === "fairy" && (c.tunicStyle ?? "petal") === id])),
  "costume.robotStyle": c => c.kind === "robot", "costume.robotLights": c => c.kind === "robot",   // (ticket 515)
  ...Object.fromEntries(Object.keys(ROBOT_STYLES).map(id => ["costume.robotColor_" + id, c => c.kind === "robot" && (ROBOT_STYLES[c.robotStyle] ? c.robotStyle : "tin") === id])),
  // (ticket 522) each style's colour while it is chosen; the hat, its colour (the hat worn's) and the lasso for both
  "costume.cowgirlStyle": c => c.kind === "cowgirl", "costume.cowboyStyle": c => c.kind === "cowboy",
  ...Object.fromEntries(Object.keys(COWGIRL_STYLES).map(id => ["costume.cowgirlColor_" + id, c => c.kind === "cowgirl" && (COWGIRL_STYLES[c.cowgirlStyle] ? c.cowgirlStyle : "rodeo") === id])),
  ...Object.fromEntries(Object.keys(COWBOY_STYLES).map(id => ["costume.cowboyColor_" + id, c => c.kind === "cowboy" && (COWBOY_STYLES[c.cowboyStyle] ? c.cowboyStyle : "sheriff") === id])),
  // (ticket 557) the ninja's rows; the gem's only with the head band on
  ...Object.fromEntries(["ninjaBoots", "ninjaBetween", "ninjaBand", "ninjaEyes"].map(k => ["costume." + k, c => c.kind === "ninja"])),
  ...Object.fromEntries(["ninjaGem", "ninjaGemShape", "ninjaGlimmer", "ninjaGemStars", "ninjaGemShine"].map(k => ["costume." + k, c => c.kind === "ninja" && c.ninjaBand !== false])),
  // (ticket 530) the Sasquatch's rows, each style's colour only while it is chosen
  ...Object.fromEntries(["sasquatchStyle", "sasquatchPrints", "sasquatchGrin"].map(k => ["costume." + k, c => c.kind === "sasquatch"])),
  ...Object.fromEntries(Object.keys(SASQUATCH_STYLES).map(id => ["costume.sasquatchColor_" + id, c => c.kind === "sasquatch" && (SASQUATCH_STYLES[c.sasquatchStyle] ? c.sasquatchStyle : "bigfoot") === id])),
  // (ticket 544) the giraffe's rows, each style's colour only while it is chosen
  ...Object.fromEntries(["giraffeStyle", "giraffeNeck"].map(k => ["costume." + k, c => c.kind === "giraffe"])),
  ...Object.fromEntries(Object.keys(GIRAFFE_STYLES).map(id => ["costume.giraffeColor_" + id, c => c.kind === "giraffe" && giraffeStyleOf(c) === id])),
  "costume.cowHat": c => c.kind === "cowgirl" || c.kind === "cowboy",
  ...Object.fromEntries(Object.keys(COW_HATS).map(id => ["costume.cowHatColor_" + id, c => (c.kind === "cowgirl" || c.kind === "cowboy") && (c.cowHat === "none" ? null : COW_HATS[c.cowHat] ? c.cowHat : "cattleman") === id])),
  "costume.lasso": c => c.kind === "cowgirl" || c.kind === "cowboy",
  "costume.webColor": c => c.kind === "witch",
  "costume.webStyle": c => c.kind === "witch",
  "costume.trimColor": c => c.kind === "witch",
  "costume.witchCat": c => c.kind === "witch",
  "costume.catFur": c => c.kind === "witch" && c.witchCat,
  "costume.catWhite": c => c.kind === "witch" && c.witchCat,
  "costume.catEyes": c => c.kind === "witch" && c.witchCat,
  "costume.catNose": c => c.kind === "witch" && c.witchCat,
  "costume.broom": c => c.kind === "witch",
  "costume.catSize": c => c.kind === "witch" && c.witchCat,
  "costume.broomSize": c => c.kind === "witch" && c.broom,
  "costume.bootColor": c => c.kind === "witch",
  "costume.scubaColor": c => c.kind === "scuba", "costume.scubaAccent": c => c.kind === "scuba",   // (ticket 288)
  "costume.diverMetal": c => c.kind === "diver", "costume.diverCloth": c => c.kind === "diver", "costume.diverHose": c => c.kind === "diver",
  "costume.hoseColor": c => c.kind === "diver" && c.diverHose !== false,
  "costume.leaks": c => c.kind === "scuba" || c.kind === "diver" || !!c.tank || !!c.snorkel,
  "costume.snorkel": c => c.kind !== "diver", "costume.snorkelColor": c => c.kind !== "diver" && c.snorkel,
  "costume.mask": c => c.kind !== "diver", "costume.maskColor": c => c.kind !== "diver" && c.mask,
  "costume.tank": c => c.kind !== "diver", "costume.tankColor": c => c.kind !== "diver" && c.tank,
  "background.figSize": b => b.kind !== "none",
  ...Object.fromEntries([...NORTH_OUT, ...NORTH_ROOMS].map(kd => ["background.north_" + kd, b => b.kind === kd])),
  "background.lfoFast": b => !!sceneLfoThings(b).fast, "background.lfoSlow": b => !!sceneLfoThings(b).slow, "background.lfoWings": b => !!sceneLfoThings(b).wings,
  "background.figPos": b => b.kind !== "none",
  ...Object.fromEntries(["leverShine", "leverShineColor", "leverFlicker"].map(k => ["background." + k, b => b.kind === "treetop" || (b.kind === "castleroom" && roomHereNowOf(b)[1] === "trap") || (b.things || []).some(t => t.kind === "lever")])),   // (ticket 317)
  "background.objectsOff": b => b.kind !== "none",
  "background.bubbles": b => b.kind === "bubblespace",   // (ticket 220)
  "background.bubbleBurstCount": b => (b.bubbleBurst ?? 40) > 0,   // (ticket 329)
  "background.bubblePopInst": b => (b.bubblePopVol ?? 40) > 0, "background.bubblePopPitch": b => (b.bubblePopVol ?? 40) > 0, "background.bubblePopKnob": b => (b.bubblePopVol ?? 40) > 0,
  "background.bubbleroom": b => b.kind === "bubbleroom",   // (ticket 290)
  "background.instEdit": b => !!instanceOf(state.world),   // (ticket 300)
  "background.instReset": b => { const W = state.world, k = instanceOf(W); return !!(k && W.cells[k] && W.cells[k].own && Object.keys(W.cells[k].own).length); },
  "background.waterSee": b => !!waterHere(b), "background.waterWake": b => !!waterHere(b),
  "background.waterTread": b => !!waterHere(b) && !SHALLOW_WATER[b.kind],
  "background.chickenLegs": b => b.kind === "witch",
  "background.legSize": b => b.kind === "witch" && b.chickenLegs,
  "background.hutNear": b => b.kind === "witch",
  "background.eyeRate": b => b.kind === "witch",
  "background.eyeGlow": b => b.kind === "witch",
  "background.skullMax": b => b.kind === "witch",
  "background.blinkApart": b => b.kind === "witch" && b.eyeRate > 0 && b.eyeGlow > 0,
  "background.skullEye": b => b.kind === "witch",
  "background.cauldronFumes": b => b.kind === "witch" || b.kind === "witchroom",
  "background.ducks": b => b.kind === "lake",
  "background.duckSize": b => b.kind === "lake",
  "background.cattails": b => b.kind === "lake",
  "background.clouds": b => b.kind === "lake",
  "background.cloudSize": b => b.kind === "lake",
  "background.houseSize": b => b.kind === "house",
  "background.treeSize": b => b.kind === "house" || b.kind === "treehouse",
  "background.treeHouseSize": b => b.kind === "treehouse",
  "background.flowers": b => b.kind === "house",
  "background.houses": b => b.kind === "treehouse",
  "background.bridges": b => b.kind === "treehouse",
  "background.ladders": b => b.kind === "treehouse",
  "background.leaves": b => b.kind === "treehouse",
  ...Object.fromEntries([1, 2, 3, 4, 5, 6].flatMap(n => Object.entries(HOUSE_COLOR_DEFAULTS).flatMap(([kind, d]) =>
    Object.keys(d).map(k => ["background." + k + n, b => b.kind === "treehouse" && b.houses >= n && b["house" + n] === kind])))),
  "background.house1": b => b.kind === "treehouse" && b.houses >= 1,
  "background.house2": b => b.kind === "treehouse" && b.houses >= 2,
  "background.house3": b => b.kind === "treehouse" && b.houses >= 3,
  "background.house4": b => b.kind === "treehouse" && b.houses >= 4,
  "background.house5": b => b.kind === "treehouse" && b.houses >= 5,
  "background.house6": b => b.kind === "treehouse" && b.houses >= 6,
  "background.inside": b => b.kind === "treeroom",
  ...Object.fromEntries(["peakHeight", "mtClouds", "birds", "farLake"].map(key => ["background." + key, b => b.kind === "mountains"])),
  ...Object.fromEntries(["pastureHorses", "pastureFlowers"].map(key => ["background." + key, b => b.kind === "pasture"])),   // (ticket 503)
  ...Object.fromEntries(["vines", "parrots"].map(key => ["background." + key, b => b.kind === "waterfall"])),
  // (the cave sees the falls from behind: their width and mist)
  ...Object.fromEntries(["fallWidth", "mist", "fallRate", "fallTime"].map(key => ["background." + key, b => b.kind === "waterfall" || b.kind === "cave"])),
  ...Object.fromEntries(["caveGems", "gemShape", "gemSize", "gemBright", "gemTwinkle"].map(key => ["background." + key, b => b.kind === "cave"])),
  ...Object.fromEntries(["caveFloor", "caveCeiling"].map(key => ["background." + key, b => b.kind === "caveroom"])),   // (ticket 471)
  "background.desertFar": b => b.kind === "desert",   // (ticket 501)
  // (ticket 490) 🌍 where and when: where this place's time of day is by the clock; its own latitude and longitude for 📍
  ...Object.fromEntries(["skyWhen", "skyWhere", "skyYear", "skyMonth", "skyDay", "skyHour", "skySays"].map(key => ["background." + key, b => clockHere(b)])),
  ...Object.fromEntries(["skyLat", "skyLon"].map(key => ["background." + key, b => clockHere(b) && b.skyWhere === "custom"])),
  ...Object.fromEntries(["campFood", "tents"].map(key => ["background." + key, b => b.kind === "campsite"])),
  ...Object.fromEntries(["crossPaths", "crossFloor", "crossBack"].map(key => ["background." + key, b => b.kind === "crossing"])),
  "background.things": b => b.kind !== "none",
  "background.thingsSolid": b => b.kind !== "none" && (b.things || []).length > 0,   // (ticket 283)
  // (a tent's door looks out on the camp: its time of day and its fire)
  ...Object.fromEntries(["campTime", "fireOn"].map(key => ["background." + key, b => b.kind === "campsite" || b.kind === "tent"])),
  ...Object.fromEntries(["starShape", "starSize", "starBright", "starTwinkle"].map(key =>
    ["background." + key, b => ((b.kind === "campsite" || b.kind === "tent") && nightBy(b.campTime, b, false)) || (b.kind === "fairyforest" && nightBy(b.fairyTime, b, true)) || (b.kind === "castleroom" && castleOf(b).rooms[Math.round(b.castleRoom ?? 1) - 1] === "trap")])),   // (and the trap room's: the sparkles of a boulder lifted)
  "background.tent": b => b.kind === "tent",
  "background.flameSize": b => b.kind === "campsite" && b.fireOn,
  ...Object.fromEntries(["seeHouse", "seeTree", "seeWitch", "seeCastle", "seeHealer"].map(key => ["background." + key, b => b.kind === "clearing"])),
  "background.castle": b => b.kind === "clearing" || b.kind === "castle" || b.kind === "castleroom",
  ...Object.fromEntries(CASTLES.flatMap(([id]) => ["castleLevels", "castleStone", "castleTrees", "castleMoat", "castleRoof"].map(key =>
    ["background." + key + id, b => (b.kind === "clearing" || b.kind === "castle") && (b.castle ?? "N") === id]))),
  // (the flags show in the king's room too: its banners and shields)
  ...Object.fromEntries(CASTLES.flatMap(([id]) => ["castleFlagA", "castleFlagB", "castleFlagStyle"].map(key =>
    ["background." + key + id, b => (b.kind === "clearing" || b.kind === "castle" || b.kind === "castleroom") && (b.castle ?? "N") === id]))),
  // (the carpet, when the castle has a hallway among its rooms)
  ...Object.fromEntries(CASTLES.map(([id]) => ["background.castleCarpet" + id, b => (b.kind === "castle" || b.kind === "castleroom") && (b.castle ?? "N") === id])),
  // (the castle's groups: its look where it is seen, its rooms and each room's own at the castle and inside it)
  ...Object.fromEntries(CASTLES.flatMap(([id]) => {
    const atCastle = b => (b.kind === "castle" || b.kind === "castleroom") && (b.castle ?? "N") === id;
    const nRooms = b => Math.max(1, Math.min(ROOM_MAX, Math.round(b["castleRooms" + id] ?? CASTLE_ROOMS[id].length)));
    const typeOf = (b, n) => b["castleRoom" + n + id] ?? CASTLE_ROOMS[id][n - 1];
    const isHall = (b, n) => typeOf(b, n) === "hallway", hasDoors = (b, n) => OWN_EXIT_ROOMS.includes(typeOf(b, n));
    // (its cats: in a cat room of its, and at the castle when it has one)
    const livesHere = st => b => atCastle(b) && (b.kind === "castle" ? ROOM_NS.some(n => n <= nRooms(b) && typeOf(b, n) === st)
                                                                      : typeOf(b, Math.round(b.castleRoom ?? 1)) === st);

    return [["background.castleLook" + id, b => (b.kind === "clearing" || atCastle(b)) && (b.castle ?? "N") === id],
            ["background.castleThemes" + id, b => (b.kind === "clearing" || atCastle(b)) && (b.castle ?? "N") === id], ["background.castleThemeB" + id, atCastle],
            ...ROOM_NS.map(n => ["background.castleRoomTheme" + n + id, b => OWN_EXIT_ROOMS.includes(typeOf(b, n))]),
            // (its residents: where they live, and as many as there are)
            ...Object.values(RESIDENTS).flatMap(R => [["background." + R.group[0] + id, livesHere(R.room)],
              ...R.list.map((_, i) => ["background." + R.box[0] + (i + 1) + id, b => livesHere(R.room)(b) && i + 1 <= Math.round(b[R.count[0] + id] ?? R.count[2])]),
              // (ticket 293: a field shown only when its sixth says, by the resident's settings: a throne's cat's, while it has one)
              ...R.list.flatMap((_, i) => R.fields.filter(f => f[5]).map(f => ["background." + R.key + (i + 1) + f[0] + id, b => !!f[5](residentsOf(R, b, id)[i] || {})]))]),
            ["background.castleRoomList" + id, atCastle], ["background.castleGo" + id, atCastle], ["background.castleDesign" + id, atCastle],
            ...ROOM_NS.flatMap(n => [...[1, 2].map(i => ["background." + pitShapeKey(i, n, id), b => typeOf(b, n) === "trap"]),
              ...[1, 2].map(i => ["background." + pitOnKey(i, n, id), b => typeOf(b, n) === "trap"]), ["background." + leverWireKey(n, id), b => typeOf(b, n) === "trap"],
              ["background." + pitSpikesKey(n, id), b => typeOf(b, n) === "trap"],   // (ticket 334)
              ["background." + boulderOnKey(n, id), b => typeOf(b, n) === "trap"], ["background." + boulderToneKey(n, id), b => typeOf(b, n) === "trap" && b[boulderOnKey(n, id)] !== false],   // (ticket 323)
              ["background." + leverSpeedKey(n, id), b => typeOf(b, n) === "trap"], ["background." + lidSpeedKey(n, id), b => typeOf(b, n) === "trap"], ["background." + boulderSpeedKey(n, id), b => typeOf(b, n) === "trap" && b[boulderOnKey(n, id)] !== false],   // (ticket 328)   // (ticket 305)   // (ticket 271: in a trap room's own box)
              // (ticket 365: the castle designer full screen shows every room, in order)
              ["background.castleRoomBox" + n + id, b => atCastle(b) && n <= nRooms(b) && (b.kind === "castle" || Math.round(b.castleRoom ?? 1) === n || document.body.classList.contains("castle-big"))],
              ["background.castleHallDoors" + n + id, b => hasDoors(b, n)], ["background.castleHallEnd" + n + id, b => isHall(b, n)],
              ["background.castleOut" + n + id, b => !isHall(b, n)], ["background.castleFar" + n + id, b => typeOf(b, n) === "dining"],
              // (the door behind us: where a room has no sign at the bottom already; never a dungeon, a pit with no way in but from above)
              ["background.castleBack" + n + id, b => ["hall", "trap", "all"].includes(ROOM_KINDS[typeOf(b, n)]?.exits)], ["background.castlePlaces" + n + id, b => typeOf(b, n) === "dining"],
              ["background." + stableHensKey(n, id), b => typeOf(b, n) === "stable"],   // (ticket 506)
              ...DOOR_JS.map(k => ["background." + hallDoorKey(id, n, k), b => hasDoors(b, n) && k <= Math.min(roomDoorMax(typeOf(b, n)), Math.round(b["castleHallDoors" + n + id] ?? 3))])])];
  })),
  ...Object.fromEntries(CASTLES.flatMap(([id]) => [["background.castleRooms" + id, b => (b.kind === "castle" || b.kind === "castleroom") && (b.castle ?? "N") === id],
    ...ROOM_NS.map(n => ["background.castleRoom" + n + id,
      b => (b.kind === "castle" || b.kind === "castleroom") && (b.castle ?? "N") === id && n <= (b["castleRooms" + id] ?? CASTLE_ROOMS[id].length)]),
    ...ROOM_NS.flatMap(n => Object.keys(ROOM_BONES).map(st => ["background." + roomBonesKey(id, n, st),
      b => (b.kind === "castle" || b.kind === "castleroom") && (b.castle ?? "N") === id && n <= (b["castleRooms" + id] ?? CASTLE_ROOMS[id].length) &&
           (b["castleRoom" + n + id] ?? CASTLE_ROOMS[id][n - 1] ?? "king") === st]))])),
  ...Object.fromEntries(["witchSize", "cauldronSize", "crows", "frogs", "snakes"].map(key => ["background." + key, b => b.kind === "witchroom"])),
  ...Object.fromEntries(["healerSize", "healerCatFur", "healerCatWhite", "healerCatEyes"].map(key => ["background." + key, b => b.kind === "healerroom"])),
  ...Object.fromEntries(["fairyTime", "mushrooms", "fairyHouseSeen", "fairyHouseLfo"].map(key => ["background." + key, b => b.kind === "fairyforest"])),
  "background.livingOut": b => b.kind === "livingroom",
  ...Object.fromEntries([1, 2, 3, 4, 5, 6].map(n => ["background.treeBack" + n, b => b.kind === "treeroom" && (b.inside ?? 1) === n])),
  // (ticket 252: a trapdoor's opening, in its house, and in the house below, whose ceiling it is)
  ...Object.fromEntries([1, 2, 3, 4, 5, 6].map(n => ["background.hatchShut" + n, b => b.kind === "treeroom" && Math.round(b.inside ?? 1) === n && houseLinks(b, n - 1).some(L => L.way === "down")])),   // (ticket 334: this house's, with a way down)
  ...Object.fromEntries([1, 2, 3, 4, 5, 6].map(n => ["background.hatchShape" + n, b => { if (b.kind !== "treeroom") return false; const i = Math.round(b.inside ?? 1);
    return (i === n && houseLinks(b, n - 1).some(L => L.way === "down")) || houseLinks(b, i - 1).some(L => L.way === "up" && L.to === n - 1); }])),
  ...Object.fromEntries([1, 2, 3, 4, 5, 6].flatMap(n => [["background.room" + n, b => b.kind === "treeroom" && (b.inside ?? 1) === n],
    ...[1, 2].map(i => [`background.r${n}pic${i}`, b => b.kind === "treeroom" && (b.inside ?? 1) === n && b.pictures >= i])])),
  "background.windowSize": b => b.kind === "treeroom" || b.kind === "treetop",
  // (ticket 516: the round room's trap door, its opening also the ceiling's in the house below, whose ladder goes up to it)
  "background.hatchShapeTop": b => b.kind === "treetop" || (b.kind === "treeroom" && Math.max(1, Math.min(6, b.houses ?? 4, Math.round(b.inside ?? 1))) === topHouse(b)),
  "background.hatchShutTop": b => b.kind === "treetop",
  "background.treetopBack": b => b.kind === "treetop",
  "background.topFurnish": b => b.kind === "treetop",
  "background.couchSize": b => b.kind === "livingroom" || b.kind === "hutroom",
  "background.view": b => b.kind === "livingroom" || b.kind === "hutroom",
  "background.viewDetail": b => b.kind === "livingroom" || b.kind === "hutroom",
  "background.pillows": b => b.kind === "livingroom" || b.kind === "hutroom",
  "background.couchColor": b => b.kind === "livingroom",
  "background.rugColors": b => b.kind === "livingroom",
  "background.hutCouchColor": b => b.kind === "hutroom",
  "background.hutRugColors": b => b.kind === "hutroom",
  "background.hutBlanket": b => b.kind === "hutbed",
  "background.hutPotion": b => b.kind === "hutpotion",
  "background.hutRooms": b => b.kind === "hut",
  "background.hutCenter": b => b.kind === "hut",
  "background.pictures": b => b.kind === "livingroom" || b.kind === "treeroom" || b.kind === "hutroom",
  "background.picDetail": b => (b.kind === "livingroom" || b.kind === "treeroom" || b.kind === "hutroom") && b.pictures > 0,
  "background.picSize": b => (b.kind === "livingroom" || b.kind === "hutroom") && b.pictures > 0,
  "background.pic1": b => (b.kind === "livingroom" || b.kind === "hutroom") && b.pictures >= 1,
  "background.pic2": b => (b.kind === "livingroom" || b.kind === "hutroom") && b.pictures >= 2,
  "background.pic3": b => b.kind === "livingroom" && b.pictures >= 3,
  "background.pic4": b => b.kind === "livingroom" && b.pictures >= 4,
  "background.pic5": b => b.kind === "livingroom" && b.pictures >= 5,
  "background.pic6": b => b.kind === "livingroom" && b.pictures >= 6,
  "costume.marks": c => !!ANIMALS[c.kind] && c.kind !== "dragon" && c.kind !== "sasquatch" && c.kind !== "giraffe",
  "costume.spot": c => c.furPattern === "rosettes" && !!(ANIMALS[c.kind] && ANIMALS[c.kind].suit) && c.kind !== "dragon",
  "costume.belly": c => !!ANIMALS[c.kind] && c.kind !== "dragon" && c.kind !== "sasquatch" && c.kind !== "giraffe",
  "costume.eyes": c => !!ANIMALS[c.kind],
  "costume.accent": c => !!ANIMALS[c.kind] && c.kind !== "dragon" && c.kind !== "sasquatch" && c.kind !== "giraffe",
  "hair.length": h => ["long", "curly", "pigtails", "spiky", "afro"].includes(h.style),
  "hair.bangs": h => !!BANG_LOCKS[h.style],
  "pants.skirtLen": p => p.style === "skirt" || p.style === "dress",
  // the same hands: one style menu, and its sliders below the color;
  // different hands: a column each (claws: also a cat costume's paws)
  "hands.style": h => !h.different,
  "hands.rightHand": h => !!h.different,
  ...Object.fromEntries(Object.keys(SLIDER_STYLE).map(k =>
    ["hands." + k, h => (!h.different && handSliderShown(k, h)) || ((k === "clawLen" || k === "clawCount") && clawedSuit())])),
  // the feet as the hands: one style menu and its toe slider, or a column each
  "shoes.style": sh => !sh.different,
  "shoes.rightShoe": sh => !!sh.different,
  "shoes.toes": sh => !sh.different && sh.style === "human",
  "shoes.alienToes": sh => !sh.different && sh.style === "alien",
  "shoes.talonCount": sh => !sh.different && sh.style === "talons",
  "undies.style": u => u.on,
  "undies.top": u => u.on && u.style !== "onepiece",
  "undies.color": u => u.on,
  "undies.pattern": u => u.on,
  "undies.patternSize": u => u.on && u.pattern !== "plain",
  "undies.patternColor": u => u.on && u.pattern !== "plain",
  "shirt.patternSize": c => c.pattern !== "plain",
  "shirt.patternColor": c => c.pattern !== "plain",
  "pants.patternSize": c => c.pattern !== "plain",
  "pants.patternColor": c => c.pattern !== "plain",
};

// Pinning a slider puts it in the full-screen controls (the HUD); the
// pins are part of the friend, saved and loaded with it.
const pinned = id => (state.hud.pins || []).includes(id);
// a HUD slider folded away to its name: part of the friend too
const folded = id => (state.hud.min || []).includes(id);
function toggleMin(id) {
  const min = state.hud.min || [];
  state.hud.min = folded(id) ? min.filter(x => x !== id) : [...min, id];
  changed();
}
function togglePin(id) {
  const pins = state.hud.pins || [];
  state.hud.pins = pinned(id) ? pins.filter(x => x !== id) : [...pins, id];
  changed(); renderHud();
}
// (while the HUD is built: its sliders have no pins -- pinning is for
// the editor, not the performance)
let buildingHud = false;
// The castle designer, in its rooms' box: the castle as a map (Cytoscape.js,
// loaded from a CDN the first time it is opened), each room a node and each
// way between rooms an arrow, the room you are in ringed. Tap a way to set
// where it leads, a room to see its ways (or go there); below the map, what
// looks wrong, each with a fix. Doors are two-way to start with: setting
// where a way leads gives the room it leads to a door back, where it has
// space for one.
const CYTOSCAPE = { src: "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.30.4/cytoscape.min.js",
                    sri: "sha512-xpXUCbrkyJCwC5noJXZqec9rSXdRgFfDoL7Q9/pCPe54Y04OlqwPobMcNuA5NPJ7DRR51OIr6xcOH2wUspyKDQ==" };
let cytoscapeLoad = null;
const loadCytoscape = () => cytoscapeLoad ||= new Promise((res, rej) => {
  if (window.cytoscape) return res(window.cytoscape);
  document.head.append(el("script", { src: CYTOSCAPE.src, integrity: CYTOSCAPE.sri, crossorigin: "anonymous", referrerpolicy: "no-referrer",
    onload: () => res(window.cytoscape), onerror: () => { cytoscapeLoad = null; rej(new Error("the map didn't load")); } }));
});
const MAP_STYLE = [
  { selector: "node", style: { label: "data(label)", "text-valign": "center", "text-halign": "center", "text-wrap": "wrap", "font-size": 13, "line-height": 1.1,
                               shape: "round-rectangle", width: 50, height: 46, "background-color": "#efe6d2", "border-width": 2, "border-color": "#8a6d3b", color: "#2a1d0e" } },
  { selector: "node.outside", style: { width: 76, "background-color": "#cfe8c4", "border-color": "#4f7a43" } },
  { selector: "node.here", style: { "border-color": "#7c5cff", "border-width": 5 } },
  { selector: "node.picked", style: { "border-color": "#e8b923", "border-width": 5 } },
  { selector: "edge", style: { width: 2.5, "curve-style": "bezier", "line-color": "#b9a98f", "target-arrow-color": "#b9a98f", "target-arrow-shape": "triangle",
                               label: "data(label)", "font-size": 12, "text-background-color": "#f6f0e2", "text-background-opacity": 0.9, "text-background-padding": 1, "text-background-shape": "roundrectangle" } },
  { selector: "edge.pit, edge.ladder", style: { "line-style": "dashed" } },
  { selector: "edge.fixed", style: { "line-color": "#8c8579", "target-arrow-color": "#8c8579" } },
  { selector: "edge.bad", style: { "line-color": "#e0533b", "target-arrow-color": "#e0533b", width: 3.5 } },
  { selector: "edge.picked", style: { "line-color": "#e8b923", "target-arrow-color": "#e8b923", width: 5 } },
];
const MAP_LAYOUT = n => n <= 6 ? { name: "breadthfirst", roots: ["r0"], directed: false, spacingFactor: 1.1, padding: 14, animate: false }
                             : { name: "circle", startAngle: -Math.PI / 2, padding: 14, animate: false, sort: (a, b) => +a.id().slice(1) - +b.id().slice(1) };
const MAP_FIXED = { pit: "A trap room's pits drop into the castle's dungeons (the first, then the deep one); a dungeon's ladder climbs back.",
                    arch: "The greenhouse has an archway to every room but the dungeons (they are fallen into).",
                    gate: "The castle's gate always leads into room 1." };
function castleDesigner(part, key, box, syncs) {
  const id = key.replace("castleDesign", ""), B = () => state[part];
  let open = false, cy = null, sig = "", picked = null, twoWay = true;
  const toggle = el("button", { onclick: () => { open = !open; picked = null; draw(); } });
  // (ticket 365) and the castle designer full screen (setCastleBig): open there, whatever the toggle says here
  const bigChip = el("button", { class: "castlebig designer-chip", onclick: () => setCastleBig(true) }, CASTLE_BIG[0]);
  const map = el("div", { class: "map" }), note = el("p", { class: "note" }), rooms = el("div", { class: "row" }), edit = el("div", { class: "row" }),
        tools = el("div", { class: "row" }), probs = el("div"), panel = el("div", { class: "designer" }, [map, note, rooms, edit, tools, probs]);
  box.append(el("div", { class: "row castle-in" }, [toggle, bigChip]), panel);
  let mapH = "", shownLook = "";
  const eid = e => `e${e.from}-${e.kind}${e.j ?? e.i ?? ""}${e.way ? "w" : ""}`;
  const pickOf = e => e.key || eid(e);
  const go = n => { const b = B(); if (n) { b.kind = "castleroom"; b.castleRoom = n; } else b.kind = "castle"; [b.figX, b.figY] = ARRIVE[b.kind] || [0, 0]; arriveSettle = true; changed(); };
  const setWay = (e, v) => {
    const b = B(); b[e.key] = v;
    if (twoWay) { const K = castleOf({ ...b, castle: id }), x = roomExits(K, e.from).find(x => x.key === e.key); if (x && x.to) mapWayBack(b, id, e.from, x.to); }
    changed();
  };
  const draw = () => {
    const big = document.body.classList.contains("castle-big");
    setText(toggle, open ? "🗺️ close the castle designer" : "🗺️ open the castle designer");
    setPressed(toggle, open);
    { const d = open || big ? "" : "none"; if (panel.style.display !== d) panel.style.display = d; }
    if (!open && !big) { shownLook = ""; return; }
    const b = castleView(B(), id), K = castleOf(b);
    const here = b.kind === "castleroom" ? Math.max(1, Math.min(K.rooms.length, Math.round(b.castleRoom ?? 1))) : 0;
    // (ticket 365, castlefs365 at the black laptop's tier: open full screen, this ran on every change, a walk's every step, its lists
    // and the map made again: 34 fps walking against the side panel's 42) only when what it shows has changed: the rooms, their ways,
    // the room you are in, what is picked, the doors' setting, the map loaded
    const look = JSON.stringify([big, b.kind, here, picked, twoWay, K, !!window.cytoscape]);   // (the whole castle as castleGraph and castleProblems read it)
    if (look === shownLook) return;
    shownLook = look;
    const E = castleGraph(K), P = castleProblems(K);
    const bad = new Set(P.filter(p => p.e).map(p => eid(p.e)));
    // the rooms, as chips too (the map's nodes, for a small screen or when the map can't load)
    rooms.replaceChildren(el("span", { class: "note" }, "rooms:"), ...[0, ...K.rooms.map((_, i) => i + 1)].map(r =>
      el("button", { "aria-pressed": picked?.room === r, onclick: () => { picked = { room: r }; draw(); } }, r ? `${roomLabel(K.rooms[r - 1]).split(" ")[0]} ${r}` : "🏰 outside")));
    // what is picked: a way, and where it can lead; or a room, and its ways
    const e = picked?.way && E.find(x => pickOf(x) === picked.way);
    if (e) {
      const opts = !e.key ? [] : e.key.startsWith("castleOut") ? wayOutDests(id, e.from) : e.key.startsWith("castleBack") || e.key.startsWith("castleFar") ? backDests(id) : e.key.startsWith("castleHallEnd") ? hallEnds(id) : hallDoorDests(id);
      edit.replaceChildren(el("span", {}, `${mapExitName(K, e)} leads to:`), ...(e.key ? opts.filter(([, , vis]) => !vis || vis(b)).map(([v, lab]) =>
        el("button", { "aria-pressed": String(B()[e.key]) === String(v), onclick: () => setWay(e, v) }, typeof lab === "function" ? lab(b) : lab))
        : [el("span", { class: "note" }, MAP_FIXED[e.kind] || "This way is fixed.")]));
    } else if (picked && picked.room !== undefined) {
      const r = picked.room, X = E.filter(x => x.from === r);
      edit.replaceChildren(el("span", {}, `${mapRoomName(K, r)}:`), ...X.map(x => el("button", { onclick: () => { picked = { way: pickOf(x) }; draw(); } },
        `${EXIT_ICONS[x.kind]} ${x.kind === "door" ? "door " + x.j : x.way ? "way out" : EXIT_WORDS[x.kind]} → ${x.to === null ? (x.nowhere ? "🧱 nowhere" : "❓") : mapRoomName(K, x.to)}`)),
        el("button", { onclick: () => go(r) }, "🚶 go there"));
    } else edit.replaceChildren(el("span", { class: "note" }, "Tap a room or an arrow on the map (or a room above) to see where its ways lead, and change them."));
    // (ticket 365: full screen, every room's box is in plain view below the map: the one picked marked)
    for (const g of document.querySelectorAll(`[data-group^="castleRoomBox"][data-group$="${id}"]`)) g.classList.toggle("picked", big && picked?.room === +(g.dataset.group.match(/^castleRoomBox(\d+)/) || [])[1]);
    // two-way doors, and putting everything right at once
    const fixable = P.filter(p => mapCanFix(B(), id, p));
    const fixAll = el("button", { onclick: () => {
      for (let i = 0; i < 40; i++) { const Q = castleProblems(castleOf(castleView(B(), id))), p = Q.find(p => mapCanFix(B(), id, p)); if (!p) break; mapFix(B(), id, p); }
      changed(); } }, `🔧 fix everything (${fixable.length})`);
    fixAll.disabled = !fixable.length;
    tools.replaceChildren(
      el("button", { "aria-pressed": twoWay, title: "Setting where a way leads also gives the room it leads to a door back, where it has space", onclick: () => { twoWay = !twoWay; draw(); } }, "↔️ two-way doors"), fixAll);
    // what looks wrong
    probs.replaceChildren(P.length ? el("ul", { class: "problems" }, P.map(p => el("li", {}, [el("span", {}, (p.kind === "oneway" ? "↗️ " : p.kind === "missing" ? "❓ " : "⚠️ ") + p.text),
        (plan => plan ? el("button", { onclick: () => { plan.apply(B()); changed(); } }, plan.text) : el("span", { class: "note" }, "(can't be changed here: it's fixed)"))(mapPlan(B(), id, p))])))
      : el("p", { class: "note" }, "✅ Nothing looks wrong: every room can be reached from outside, and has a way back out."));
    // the map itself
    if (!window.cytoscape) {
      note.textContent = "Loading the map…";
      loadCytoscape().then(() => draw(), () => { note.textContent = "The map couldn't load (it needs the internet the first time it opens). The rooms and their ways still work above and below."; });
      return;
    }
    note.textContent = "🟣 the room you're in · 🔴 a way with no way back · dashed: a pit or a ladder";
    const nodes = [{ data: { id: "r0", label: "🏰\noutside" }, classes: "outside" + (b.kind === "castle" ? " here" : "") + (picked?.room === 0 ? " picked" : "") },
      ...K.rooms.map((st, i) => ({ data: { id: "r" + (i + 1), label: `${roomLabel(st).split(" ")[0]}\n${i + 1}` },
                                   classes: (here === i + 1 ? "here" : "") + (picked?.room === i + 1 ? " picked" : "") }))];
    const edges = E.filter(x => x.to !== null).map(x => ({ data: { id: eid(x), source: "r" + x.from, target: "r" + x.to, label: EXIT_ICONS[x.kind] },
      classes: [x.kind, x.key ? "set" : "fixed", bad.has(eid(x)) ? "bad" : "", picked?.way === pickOf(x) ? "picked" : ""].join(" ") }));
    const now = K.rooms.join();
    const h = big ? "min(58vh, 600px)" : (K.rooms.length > 6 ? 460 : 340) + "px", grew = h !== mapH;
    if (grew) { map.style.height = h; mapH = h; }
    if (!cy) {
      cy = window.cytoscape({ container: map, elements: [...nodes, ...edges], style: MAP_STYLE, layout: MAP_LAYOUT(K.rooms.length), minZoom: 0.3, maxZoom: 2.5, wheelSensitivity: 0.3 });
      cy.on("tap", "edge", ev => { const x = castleGraph(castleOf(castleView(B(), id))).find(x => eid(x) === ev.target.id()); picked = x ? { way: pickOf(x) } : null; setTimeout(draw); });
      cy.on("tap", "node", ev => { picked = { room: +ev.target.id().slice(1) }; setTimeout(draw);
        // (ticket 365: full screen, the room's own box brought into view)
        const g = document.body.classList.contains("castle-big") && document.querySelector(`[data-group="castleRoomBox${picked.room}${id}"]`); if (g) g.scrollIntoView({ block: "nearest" }); });
      cy.on("tap", ev => { if (ev.target === cy) { picked = null; setTimeout(draw); } });
      sig = now;
    } else if (now !== sig) { cy.elements().remove(); cy.add([...nodes, ...edges]); cy.layout(MAP_LAYOUT(K.rooms.length)).run(); sig = now; }
    else { cy.edges().remove(); cy.add(edges); for (const nd of nodes) cy.getElementById(nd.data.id).classes(nd.classes); }
    cy.resize(); if (now !== sig || !cy.__fitted || grew) { cy.fit(undefined, 14); cy.__fitted = true; }
  };
  syncs.push(draw);
}
// (ticket 486) ✈️ How to add the game to this device, so it opens in airplane mode, and whether this version is kept here yet
// (sw.js keeps it; offlineKept counts it). The steps for each kind of device; 📲 where the browser offers its own install.
function offlineHelp() {
  const status = el("div", { class: "hint offline-status" }, "…");
  const install = el("button", { class: "offline-install", hidden: "", onclick: async () => {
    const p = offlinePrompt; if (!p) return; offlinePrompt = null; install.hidden = true; try { await p.prompt(); } catch (_) { /* (dismissed) */ } } }, "📲 add Dress Up to this device");
  const say = async () => {
    install.hidden = !offlinePrompt;
    const k = await offlineKept(), t = !k.can ? "⚠️ This browser can't keep a copy of the game (a private window keeps nothing). Open it in a normal window."
      : !k.on ? "💻 This copy comes from this computer, which keeps none: the game at matthewhammer.org keeps a copy of itself."
      : k.total && k.kept === k.total ? "✅ This version (" + VERSION.number + ") is kept on this device: it opens without the Internet."
      : "⏳ Keeping a copy on this device: " + k.kept + " of " + k.total + " files so far. Leave the game open a moment, with the Internet.";
    if (status.textContent !== t) status.textContent = t;
  };
  const step = (who, how) => el("li", {}, [el("b", {}, who), " " + how]);
  const d = el("details", { class: "how offline-help" }, [
    el("summary", {}, ["✈️ play without the Internet (airplane mode): add the game to this device"]),
    status, install,
    el("ol", {}, [
      step("📱 iPad or iPhone, in Safari:", "tap Share (the square with an arrow up), then \"Add to Home Screen\", then Add."),
      step("🤖 Android phone or tablet, in Chrome:", "tap ⋮ (top right), then \"Add to Home screen\" or \"Install app\"."),
      step("💻 A computer, in Chrome or Edge:", "click the install button at the right end of the address bar, or ⋮ then \"Cast, save and share\" then \"Install page as app\" (Edge: ··· then Apps then \"Install this site as an app\")."),
      step("🍎 A Mac, in Safari:", "File, then \"Add to Dock\"."),
      step("🦊 Firefox:", "nothing to add: once the game has opened here with the Internet, this same address opens without it. A bookmark finds it again.")]),
    el("div", { class: "hint" }, "Then open Dress Up once from its new icon while the Internet is still on. After that it works in airplane mode, and your friends and settings are saved on the device as always. When the Internet is back, a newer version shows as 🆕 after the game's name, as before.")]);
  d.addEventListener("toggle", () => { if (d.open) say(); });
  let timer = setInterval(() => { if (!d.isConnected) { clearInterval(timer); timer = null; } else if (d.open) say(); }, 2000);
  say();
  return d;
}
// (ticket 524) 🗺️ World design's ⚙️ box of the place's settings: open or not, kept while the game is open (closed to start:
// closed, it builds nothing; open, it costs what the 🏞️ Background tab does, ticket 167)
let worldSettingsOpen = false;
// (ticket 406, factored out for ticket 525) a world's grid drawn small: each square its emoji and its name (A1...); at(key, cell)
// says whether it can be picked ({ ok, chosen, self, why }); picked, pick(key). attr: the data attribute naming its square
function worldMiniGrid(T, at, pick, attr) {
  const mini = el("div", { class: "world-mini" });
  mini.style.setProperty("--cols", T.cols);
  for (let r = 0; r < T.rows; r++) for (let c = 0; c < T.cols; c++) {
    const key = c + "," + r, cell = T.cells[key], a = at(key, cell);
    mini.append(cell && a.ok
      ? el("button", { class: "world-mini-cell" + (a.chosen ? " chosen" : ""), [attr]: key, title: `${squareName(key)}: ${worldName(T, key)}`,
                       onclick: () => pick(key) }, [el("span", {}, placeName(cell.kind).split(" ")[0]), el("small", {}, squareName(key))])
      : el("span", { class: "world-mini-cell empty", title: squareName(key) + a.why }, a.self ? "📍" : squareName(key)));
  }
  return mini;
}
// (ticket 525) a cave door's 🧭 on its card: where it leads now (✖ to unlink: its twin there goes); a world's chips; its grid, small
// (a square tapped: linked there, a twin put there leading back); and its places off the grid, chips (an inside of a square,
// the cave room behind the falls among them). The world chosen is the page's only (doorWorld), not saved.
let doorWorld = null;
function doorDestParts(t, label, again) {
  const here = passageHere(), hereHome = passageHome(here), d = t.dest, worlds = worldsAll();
  if (!worlds.some(w => w.id === doorWorld)) doorWorld = state.world.id;
  const T = worldById(doorWorld), out = [el("span", { class: "hint" }, label)];
  if (d) { const ok = passageDestOk(d), Tw = ok && worldById(d.world);
    const name = !ok ? "🚧 gone" : (Tw !== state.world ? "🌍 " + worldTitle(Tw) + ": " : "") + (d.key ? worldName(Tw, d.key) + " (" + squareName(d.key) + ")" : placeName(d.kind));
    out.push(el("div", { class: "row door-dest" }, [el("span", {}, "→ " + name + (passageTwin(t) ? ", and back" : ", 🚧 no way back")),
      el("button", { class: "door-unlink", title: "Take this link off (and the passage there that leads back)", onclick: () => { passageUnlink(t); again(); } }, "✖")])); }
  if (!here) return [...out, el("span", { class: "hint" }, "(the place shown has no square or home yet)")];
  out.push(el("div", { class: "row door-worlds" }, worlds.map(w => el("button", { "aria-pressed": String(w.id === doorWorld), "data-door-world": w.id,
    onclick: () => { doorWorld = w.id; again(); } }, "🌍 " + worldTitle(w) + (w === state.world ? " (this one)" : "")))));
  if (!T) return out;
  out.push(worldMiniGrid(T, key => { const self = passageHome({ world: T.id, key }) === hereHome;
      return { ok: !self, chosen: !!d && d.world === T.id && d.key === key, self, why: self ? ": this square" : ": empty" }; },
    key => { if (passageLink(t, { world: T.id, key, kind: T.cells[key].kind })) again(); }, "data-door-to"));
  // (the places off the grid there: as worldOffGrid lists them, each one's home its placeKey)
  const offs = worldOffGrid(T).map(o => { const w = INSIDE_WHICH[o.kind], v = Object.assign(Object.create(state.background), { kind: o.kind, ...(w ? { [w]: o.which || 1 } : {}) });
    return { world: T.id, off: placeKey(v), kind: o.kind, from: o.from, ...(w ? { [w]: o.which || 1 } : {}) }; }).filter(o => passageHome(o) !== hereHome);
  if (offs.length) out.push(el("div", { class: "row door-offs" }, offs.map(o => el("button", { class: "world-off-chip", "data-door-off": o.off, "aria-pressed": String(!!d && passageHome(d) === passageHome(o)),
    title: placeName(o.kind) + (o.from ? " (through " + squareName(o.from) + ")" : ""), onclick: () => { if (passageLink(t, o)) again(); } }, placeName(o.kind) + (o.from ? " · " + squareName(o.from) : "")))));
  return out;
}
function control(part, [kind, key, label, ...rest], opts = {}) {
  // another tab's control, shown here when this tab's settings say (its
  // settings are that tab's: the ghost's see-through-ness is the
  // visibility panel's)
  if (kind === "of") return control(key, label, { when: rest[0] ? () => rest[0](state[part]) : null });
  // (ticket 249) a compass: a toggle each way (key prefix + direction), NW N NE / W · E / SW S SE, its middle empty, as
  // the world designer's ways out
  if (kind === "compass") {
    const pre = rest[0], names = Object.fromEntries(CROSS_DIRS);
    const box = el("div", { class: "world-compass" }, ["NW", "N", "NE", "W", null, "E", "SW", "S", "SE"].map(d => {
      if (!d) return el("span", { class: "world-compass-mid empty" }, "");
      const b = el("button", { "data-way": d, title: names[d] || d, onclick: () => set(part, pre + d, !state[part][pre + d]) }, WORLD_ARROWS[d]);
      syncs.push(() => { const on = String(!!state[part][pre + d]); if (b.getAttribute("aria-pressed") !== on) b.setAttribute("aria-pressed", on); });
      return b; }));
    const g = el("div", { class: "chipset ctlgroup compass-group", "data-key": key }, [el("div", { class: "cap" }, [label]), box]);
    const when = SHOWN_WHEN[part + "." + key];
    if (when) syncs.push(() => { setHidden(g, !when(state[part])); });
    return g;
  }
  // a group: its controls in a dashed box with its name, shown or hidden together
  // (and, with a test, marked when it holds: the castle room you are in)
  if (kind === "group") {
    const g = el("div", { class: "chipset ctlgroup", "data-group": key }, [el("div", { class: "cap" }, [label]), ...rest[0].map(c => control(part, c))]);   // (ticket 365: placed by its key, as a row is)
    const when = SHOWN_WHEN[part + "." + key], here = rest[1];
    if (when || here) syncs.push(() => { if (when) setHidden(g, !when(state[part])); if (here) { const on = !!here(state[part]); if (g.classList.contains("here") !== on) g.classList.toggle("here", on); } });
    return g;
  }
  // (ticket 523, Matthew: "let's also add a 'fire' button within the dragon costume, with chips for testing each type, so the
  // person trying on the costume need not switch to game controls to try") a choice of fire, the same row "choice" builds (its
  // setting, wherever else it is shown, as 🎮 Controls' 🔥 its fire), with a 🔥 button over its chips that breathes fire while
  // held (fireHoldButton), and a chip tapped breathing its fire for a moment as it is chosen (fireTry; js/25-world.js)
  if (kind === "breathe") {
    const box = control(part, ["choice", key, label, rest[0]], opts), chips = box.querySelector(":scope > .row");
    chips.addEventListener("click", e => { if (e.target.closest("button")) fireTry(); });   // (after the chip's own click has chosen it)
    box.insertBefore(el("div", { class: "row fire-hold-row" }, [fireHoldButton()]), chips);
    return box;
  }
  // (ticket 230, Matthew: "for each of: all arrows ..., planned path ..., collision boxes (show a brick wall, not a red
  // box please): chip toggle, as we have now, and then when on, color and stroke/font-size controls. -- all on one line,
  // if possible. Each one gets a box around it to organize it when enabled. the other sliders can maybe be in another
  // box, organized away. Try to make each slider less long in this view, and use the mixer/music as inspiration") A
  // row: its chip (key, the toggle; null: a box of sliders alone, named), and while it is on, its colour pickers and
  // short sliders beside it, wrapping where the panel is narrow, the row boxed as a mixer's strip
  if (kind === "accbox") {
    const row = el("div", { class: "accbox" + (key ? "" : " plain") }), more = el("div", { class: "accmore" });
    const chip = key ? el("button", { class: "flip", onclick: () => set(part, key, !state[part][key]) }, [label]) : el("span", { class: "cap" }, [label]);
    for (const [k2, key2, lab2, lo, hi] of rest[0]) {
      const now = () => state[part][key2] ?? DEFAULTS[part][key2];
      // (ticket 283) the collision boxes' kinds, a chip each, on unless switched off
      if (k2 === "solidcats") {
        const box = el("div", { class: "madecats" });
        for (const [cat, name] of SOLID_CATS) {
          const b = el("button", { class: "flip", onclick: () => set(part, key2, { ...(state[part][key2] || {}), [cat]: !solidCatOn(cat) }) }, name);
          box.append(b);
          syncs.push(() => { const on = solidCatOn(cat); if (b.getAttribute("aria-pressed") !== String(on)) b.setAttribute("aria-pressed", on); });
        }
        more.append(box);
        continue;
      }
      if (k2 === "cats") {
        // (ticket 258) a chip per kind (MADE_CATS), on or off, with how many are in the place now
        const box = el("div", { class: "madecats" });
        for (const [cat, name] of MADE_CATS) {
          const n = el("span", { class: "val" }), b = el("button", { class: "flip", onclick: () => set(part, key2, { ...(state[part][key2] || {}), [cat]: !madeCatOn(cat) }) }, [name, " ", n]);
          box.append(b);
          const f = () => { const on = madeCatOn(cat), c = madeCounts[cat] || 0, txt = "×" + c; if (b.getAttribute("aria-pressed") !== String(on)) b.setAttribute("aria-pressed", on);
            if (n.textContent !== txt) n.textContent = txt; b.classList.toggle("none-here", !c); };
          syncs.push(f); madeSyncs.push(f);
        }
        more.append(box);
        continue;
      }
      // (ticket 294) a choice: a chip for each, the one chosen pressed
      if (k2 === "choice") {
        const box = el("div", { class: "madecats" });
        for (const [v, name] of lo) { const b = el("button", { class: "flip", onclick: () => set(part, key2, v) }, name); box.append(b);
          syncs.push(() => { const on = now() === v; if (b.getAttribute("aria-pressed") !== String(on)) b.setAttribute("aria-pressed", on); }); }
        more.append(el("span", { class: "accitem" }, [lab2]), box);
        continue;
      }
      if (k2 === "color") {
        const inp = el("input", { type: "color", title: lab2 + " color", oninput: e => set(part, key2, e.target.value) });
        more.append(el("label", { class: "accitem" }, [lab2, inp]));
        syncs.push(() => { if (inp.value !== now()) inp.value = now(); });
      } else {
        const v = el("span", { class: "val" }), inp = el("input", { type: "range", min: lo, max: hi, oninput: e => set(part, key2, Number(e.target.value)) });
        more.append(el("label", { class: "accitem" }, [el("span", {}, [lab2]), inp, v]));
        syncs.push(() => { const x = now(); if (+inp.value !== x) inp.value = x; if (v.textContent !== String(x)) v.textContent = x; });
      }
    }
    row.append(chip, more);
    syncs.push(() => { const on = !key || !!state[part][key]; if (row.classList.contains("on") !== on) row.classList.toggle("on", on); setHidden(more, !on); if (key) setPressed(chip, on); });
    return row;
  }
  const val = el("span", { class: "val" });
  // (ticket 375, ^du_report(5)'s rule 4, a row on one line: what a label explains, its last parenthesis ("(100 = a leg's length)",
  // "(round ↔ narrow)"), is a hint on pointing, not words in the row)
  const hintM = typeof label === "string" && (label.match(/^(.*\S)\s+\(([^()]+)\)$/) || label.match(/^([^:]{2,24}):\s+(\S.{5,})$/));   // (and after a colon: "👕 shirt: the body, neck & arms")
  const name = el("span", { class: "name", ...(hintM ? { title: hintM[2] } : {}) }, [hintM ? hintM[1] : label]);
  const lbl = el("div", { class: "lbl" }, [el("span", {}, [name]), val]);
  const box = el("div", { class: "ctl", ...(typeof key === "string" ? { "data-key": key } : {}) }, [lbl]);   // (ticket 250: placed by its key: the Music tab's first cards)
  if ((kind === "range" || kind === "turn" || kind === "place" || kind === "master") && buildingHud) {
    const id = part + "." + key, eye = el("button", { class: "eye", title: "Fold this slider away, or open it (it stays pinned)",
                                                       onclick: () => { toggleMin(id); } }, "👁️");
    name.before(eye);
    syncs.push(() => { setPressed(eye, !folded(id)); if (box.classList.contains("min") !== folded(id)) box.classList.toggle("min", folded(id)); });
  }
  if ((kind === "range" || kind === "turn" || kind === "master") && !(part === "background" && key === "figSize") && !buildingHud) {
    // the pin, left of the name (the figure's size is always in the HUD:
    // no pin)
    const id = part + "." + key, pin = el("button", { class: "pin", title: "Pin to the full-screen controls",
                                                       onclick: () => togglePin(id) }, "📌");
    name.before(pin);
    syncs.push(() => setPressed(pin, pinned(id)));
  }
  // chips to go straight to the castle or into one of its rooms, by number (to try a castle out)
  if (kind === "castlejump") {
    const row = el("div", { class: "row" });
    box.append(row);
    let shownJump = "";
    syncs.push(() => {
      const B = state[part], K = castleOf(B), here = B.kind === "castle" ? 0 : B.kind === "castleroom" ? Math.round(B.castleRoom ?? 1) : -1;
      // (ticket 379: made again only when its rooms or the room you are in change, not on every refresh)
      const sig = K.rooms.join() + "|" + here; if (sig === shownJump) return; shownJump = sig;
      row.replaceChildren(...[0, ...K.rooms.map((_, i) => i + 1)].map(n => el("button", { "aria-pressed": n === here, onclick: () => {
        if (n) { B.kind = "castleroom"; B.castleRoom = n; } else B.kind = "castle";
        [B.figX, B.figY] = ARRIVE[B.kind] || [0, 0]; arriveSettle = true;
        changed();
      } }, n ? `${roomLabel(K.rooms[n - 1]).split(" ")[0]} ${n}` : "🏰 outside")));
    });
  }
  if (kind === "castledesigner" && !buildingHud) castleDesigner(part, key, box, syncs);
  // (ticket 99) the scene's objects, back to front, as small chips: a tap
  // hides one (🙈) or shows it again (👁️); the friend's chip (🧍) stands
  // among them where the friend stands, so a drag moves it along the list
  if (kind === "objects") {
    const row = el("div", { class: "row objs" }), me = el("span", { class: "me", title: "where the friend stands: in front of every object to the left, behind every one to the right" }, "🧍");
    box.append(row);
    let shown = "", chips = [];
    syncs.push(() => {
      const B = state[part], list = bgShown.objs, off = objectsOff(B), at = objPlace(B);
      const sig = at + "|" + list.map(o => o.name + "@" + o.z).join("|");
      if (sig !== shown) {
        shown = sig;
        // (ticket 285, ^du_report(5)'s step 2: "what stands in the place is a card each, its picture clipped from the scene
        // (OBJECTS.md's), lit in the game while pointed at") its picture shows in the designer's full screen only
        const lit = (o, on) => { const g = bgShown.node && bgShown.node.querySelector(`[data-obj="${CSS.escape(o.name)}"]`); if (g) g.classList.toggle("obj-lit", on); };
        chips = list.map(o => [o, el("button", { class: "obj", onpointerenter: () => lit(o, true), onpointerleave: () => lit(o, false), "data-layer": o.layer || "stands", title: o.layer ? `${o.name}: on the ${o.layer}, always behind the friend, in nobody's way` : `${o.name}: stands at depth ${Number.isFinite(o.z) ? Math.round(o.z) : "in front of everything"} (the friend's feet are at ${Math.round(SCENE.ground)} on the ground line)`,
          onclick: () => { const all = { ...(B.objectsOff || {}) }, now = new Set(all[at] || []);
                           if (now.has(o.name)) now.delete(o.name); else now.add(o.name);
                           if (now.size) all[at] = [...now]; else delete all[at];
                           B.objectsOff = all; changed(); } }, [el("img", { class: "obj-pic", src: objPicture(at, o.name), alt: "", loading: "lazy", onerror: e => e.target.remove() }),
                                                               el("span", { class: "eye" }), " " + o.name])]);
        row.replaceChildren(...chips.map(([, b]) => b));
      }
      for (const [o, b] of chips) { const hid = off.has(o.name); setText(b.querySelector(".eye"), hid ? "🙈" : "👁️"); if (b.classList.contains("off") !== hid) b.classList.toggle("off", hid); setPressed(b, !hid); }
      const feet = SCENE.ground + (B.figY ?? 0), i = list.findIndex(o => o.z > feet);
      { const ref = i < 0 ? null : chips[i][1]; if (me.nextSibling !== ref || me.parentNode !== row) row.insertBefore(me, ref); }   // (ticket 379: moved only when its place changes)
      setHidden(box, !list.length);
    });
  }
  if (kind === "seats") {
    // (ticket 50) a chip for each seat here, and one to stand up again
    const row = el("div", { class: "row objs" });
    box.append(row);
    let shown = "";
    syncs.push(() => {
      const list = seatsHere(state.background), now = seatNow(), sig = state.background.kind + "|" + list.map(t => t.name).join("|") + "|" + (now ? now.name : "");
      if (sig !== shown) {
        shown = sig;
        row.replaceChildren(...list.map(t => el("button", { class: "obj" + (now && now.name === t.name ? " on" : ""), "aria-pressed": !!(now && now.name === t.name),
                                                             onclick: () => sitOn(t) }, "🪑 " + t.name)),
                            ...(state.body.sit ? [el("button", { class: "obj", onclick: () => standUp() }, "🧍 stand up")] : []));
      }
      setHidden(box, !list.length);
    });
  }
  // (ticket 439) the people: a chip each (tapped, that person comes into the picture to be dressed and moved; the one left
  // stays where they stood); the one dressed now: their name, ⭐ a template or not; a new person from a template; then the
  // others, each with where they stand (a square of a world), 📍 put here beside the friend, ✖ out of the places, ⭐ and
  // 🗑️. Kept while nothing in it changes (shown).
  if (kind === "people") {
    const wrap = el("div", { class: "people" });
    box.append(wrap);
    let shown = null, newTpl = null;
    const render = () => {
      peopleInit();
      const me = state.person, all = peopleAll(), here = personHere(), tpls = all.filter(p => p.template);
      const sig = JSON.stringify([me, all.map(p => [p.id, personTitle(p), !!p.template, p.from, p.place || null]), here && [here.world, here.key], newTpl,
                                  all.filter(p => p.place).map(p => personWhere(p))]);
      if (sig === shown) return;
      shown = sig;
      if (!tpls.some(t => t.id === newTpl)) newTpl = tpls.length ? tpls[0].id : null;
      const mark = p => (p.template ? "⭐ " : "🧍 ") + personTitle(p);
      const chips = all.map(p => el("button", { class: "person-chip", "aria-pressed": String(p === me), "data-person": p.id,
        title: p === me ? "The one in the picture: every tab dresses them, and they are the one who moves" : "Dress this person: they come into the picture, and the one now stays where they stand",
        onclick: () => { if (p !== me) personSwitch(p.id); } }, mark(p)));
      const nameBox = el("input", { type: "text", maxlength: String(PERSON_NAME_MAX), size: "14", value: String(me.name || ""), placeholder: personTitle({ ...me, name: "" }), "aria-label": "this person's name",
        onchange: e => { me.name = e.target.value.trim().slice(0, PERSON_NAME_MAX); changed(); } });
      const pick = tpls.length ? el("select", { "aria-label": "the template for a new person", onchange: e => { newTpl = e.target.value; shown = null; render(); } },
        tpls.map(t => el("option", { value: t.id, ...(t.id === newTpl ? { selected: "" } : {}) }, "⭐ " + personTitle(t)))) : null;
      const others = all.filter(p => p !== me).map(p => el("div", { class: "row person-row", "data-person-row": p.id }, [
        el("span", { class: "person-name" }, mark(p)), el("span", { class: "hint person-where" }, personWhere(p)),
        el("button", { class: "person-here", ...(here ? {} : { disabled: "" }), title: here ? "Put them here, beside the friend" : "Go to a place first: they stand in places", onclick: () => personPlace(p.id, personAside(here, p.id)) }, "📍 here"),
        ...(p.place ? [el("button", { class: "person-out", title: "Take them out of the places (they stay in the list)", onclick: () => personPlace(p.id, null) }, "✖ out")] : []),
        el("button", { class: "person-tpl", "aria-pressed": String(!!p.template), title: "A template: new people can be made from them", onclick: () => personSetTemplate(p.id, !p.template) }, "⭐"),
        el("button", { class: "person-drop", title: "Take this person out of the game", onclick: () => { if (confirm(`Take ${personTitle(p)} out of the game? This can't be undone.`)) personDrop(p.id); } }, "🗑️")]));
      wrap.replaceChildren(el("div", { class: "cap" }, "👥 people"), el("div", { class: "row people-chips" }, chips),
        el("div", { class: "hint" }, `🧍 dressing ${personTitle(me)}: every tab dresses them, and they are the one who moves`),
        el("label", { class: "row person-name-row" }, [el("span", { class: "hint" }, "✏️ their name:"), nameBox]),
        el("div", { class: "row" }, [el("button", { class: "person-template", "aria-pressed": String(!!me.template), onclick: () => personSetTemplate(me.id, !me.template) }, "⭐ a template: new people can be made from them")]),
        el("div", { class: "row person-new-row" }, tpls.length ? [el("span", { class: "hint" }, "➕ a new person from"), pick, el("button", { class: "person-new", onclick: () => personNew(newTpl) }, "➕ new person")]
                                                         : [el("span", { class: "hint" }, "➕ to make new people, make someone ⭐ a template")]),
        // (ticket 459: or from a costume, chosen in a box laid over the panels, to compare with the dropdown above)
        el("div", { class: "row" }, [el("button", { class: "person-from-costume", title: "A new person in a costume: choose it in a box over the panels", onclick: () => personCostumeBox() }, "➕ from a costume")]),
        ...(others.length ? [el("div", { class: "cap" }, "🧍 the others"), ...others] : []));
    };
    syncs.push(render);
  }
  // (ticket 159, Matthew) the music, full screen: this tab's controls take
  // the whole window, and the screen where the browser allows (setMusicBig)
  if (kind === "musicbig") box.append(el("button", { class: "musicbig designer-chip", onclick: () => setMusicBig(!document.body.classList.contains("music-big")) },
                                         document.body.classList.contains("music-big") ? MUSIC_BIG[1] : MUSIC_BIG[0]));
  // (ticket 210, Matthew) the world designer full screen, as the music's (setWorldBig)
  if (kind === "worldbig") box.append(el("button", { class: "worldbig designer-chip", onclick: () => setWorldBig(!document.body.classList.contains("world-big")) },
                                         document.body.classList.contains("world-big") ? WORLD_BIG[1] : WORLD_BIG[0]));
  // (ticket 285, ^du_report(5)'s step 2) 🏞️ Background, as a designer: full screen as the world designer is (setBgBig)
  if (kind === "bgbig") box.append(el("button", { class: "bgbig designer-chip", onclick: () => setBgBig(!document.body.classList.contains("bg-big")) },
                                      document.body.classList.contains("bg-big") ? BG_BIG[1] : BG_BIG[0]),
                                  // (ticket 365: and the castle designer's ✕ back, in its place while the castle designer is full screen)
                                  el("button", { class: "castlebig designer-chip castle-bar-chip", onclick: () => setCastleBig(false) }, CASTLE_BIG[1]));
  // and its first card, the game itself (placePic puts the picture in it), folded by its 👁️ as the world designer's is
  if (kind === "bgview") {
    const view = el("div", { class: "world-view bg-view" }, [el("div", { class: "row world-view-head" }, [
      el("button", { class: "eye", onclick: () => worldViewShow(worldViewHidden) }, "👁️"), el("span", { class: "hint" }, "📺 the game")]), el("div", { class: "bg-view-pic" })]);
    worldViewMark(view);
    box.querySelector(".lbl")?.remove();
    box.append(view);
  }
  // (ticket 185) the forest crossing's things: a button for each kind puts
  // one in, at a free spot near the meeting place; drag it from there in
  // the picture; one more takes them all away. Under them, a row for each
  // thing: its own parameters (thingParams: ↔️ mirror for all, a rock's
  // colours as swatches), and ✖ to take it away.
  if (kind === "things") {
    // (ticket 375: a thing's picture on its card, its drawing in the scene copied once the scene is drawn: the report's "show the thing")
    const thingPic = i => { const sv = document.createElementNS("http://www.w3.org/2000/svg", "svg"); sv.setAttribute("class", "tpic"); sv.setAttribute("aria-hidden", "true");
      setTimeout(() => { const g = document.querySelector(`#bg [data-drag="thing:${i}"]`); if (!g || !sv.isConnected) return;
        try { const b = g.getBBox(); if (!(b.width > 0 && b.height > 0)) return; const c = g.cloneNode(true); c.removeAttribute("transform"); c.removeAttribute("data-drag");
          for (const e of [c, ...c.querySelectorAll("[id]")]) e.removeAttribute("id"); for (const e of c.querySelectorAll(".cost-glow, animate, animateTransform, animateMotion")) e.remove();
          const m = Math.max(b.width, b.height) * 0.08; sv.setAttribute("viewBox", `${b.x - m} ${b.y - m} ${b.width + 2 * m} ${b.height + 2 * m}`); sv.replaceChildren(c); } catch (e) { /* (not drawn) */ } }, 60);
      return sv; };
    const n = el("span", { class: "hint" }), list = el("div", { class: "cross-list" });
    let shown = null;
    const rows = () => {
      // (a thing's words are typed, and its size slid, in place: not in the signature, or each key or step would
      // build the row again, and the box lose the caret, the slider the finger; a slider not in hand follows its thing)
      const T = state.background.things || [], sig = JSON.stringify(T.map(t => [t.kind, thingParams(t.kind).map(([p, , how]) => how === "text" || how === "range" ? null : t[p] ?? null),
                                                                              t.kind === "signpost" ? signWays(state.background).join() : "", t.kind === "passage" ? [t.twin, doorWorld, worldsAll().map(w => w.id + worldTitle(w)).join()] : ""]));   // (ticket 525)
      if (sig === shown) {
        for (const r of list.querySelectorAll('input[type="range"][data-thing]')) {
          const t = T[+r.dataset.thing], v = t && (t[r.dataset.param] ?? 100);
          if (t && document.activeElement !== r && +r.value !== v) { r.value = String(v); r.nextElementSibling.textContent = v + "%"; }
        }
        return;
      }
      shown = sig;
      // (ticket 375, Matthew 10-18 13:49: "If we go deep enough, there's a lack of [knolling] here (see the individual objects placed in
      // scene controls)") Each thing put here a card, the same size as its neighbours in a grid: its picture (its drawing in the scene),
      // its name once and ✖ at its top; its one-tap actions and switches as a line of chips; each other setting a line of its own
      const partsFor = (t, i, [p, label, how, opts]) => {
          const now = t[p] ?? (THING_DEFAULTS[t.kind] || {})[p], set = v => () => { t[p] = v; changed(); };
          if (how === "color") return [el("span", { class: "hint" }, label), ...opts.map(col => el("button", { title: col, "aria-pressed": String((now ?? opts[0]) === col),
            style: col === "none" ? "width: 1.6em; height: 1.6em; padding: 0; border-radius: 50%;" : `background: ${col}; width: 1.6em; height: 1.6em; padding: 0; border-radius: 50%;`,
            onclick: set(col) }, col === "none" ? "∅" : ""))];
          // (ticket 471: a list may be a function, read now: a cave passage's places, which this part defines after the things)
          if (how === "choice") return [el("span", { class: "hint" }, label), ...(typeof opts === "function" ? opts() : opts).map(([v, name]) => el("button", { "aria-pressed": String(now === v), onclick: set(v) }, name))];
          if (how === "action") return [el("button", { onclick: () => { t[p] = opts(t); changed(); } }, label)];
          if (how === "range") { const [lo, hi, st] = opts, shownAt = el("span", { class: "hint" }, `${now ?? 100}%`);
            return [el("span", { class: "hint" }, label), el("input", { type: "range", min: String(lo), max: String(hi), step: String(st), value: String(now ?? 100), "aria-label": label, "data-thing": String(i), "data-param": p,
              oninput: e => { t[p] = +e.target.value; shownAt.textContent = t[p] + "%"; changed(); } }), shownAt]; }
          // (a sign's boards: a chip per way this square has that meets a way back; all of them until some are chosen)
          if (how === "dirs") { const ok = signWays(state.background), cur = t[p] ?? ok;
            if (!ok.length) return [el("span", { class: "hint" }, label + ": no ways out here that meet a way back")];
            return [el("span", { class: "hint" }, label), ...ok.map(d => el("button", { "aria-pressed": String(cur.includes(d)), title: d,
              onclick: () => { const c = t[p] ?? ok; t[p] = Object.keys(WORLD_DIRS).filter(x => x === d ? !c.includes(d) : c.includes(x)); changed(); } }, WORLD_ARROWS[d]))]; }
          // (ticket 284) a lever's 🔗: a chip for each trap door here, named by its number in this list; pressed, wired
          if (how === "wires") { const all = state.background.things || [], doors = all.filter(x => x.kind === "trapdoor" && x.uid), cur = t[p] || [];
            if (!doors.length) return [el("span", { class: "hint" }, label + ": put a 🕳️ trap door here first")];
            // (ticket 303, M: "they seem 'wired' together", and were not: a chip says which, 🔗 joined, ➕ not)
            return [el("span", { class: "hint" }, label), ...doors.map(d => { const on = cur.includes(d.uid), n = all.indexOf(d) + 1;
              return el("button", { "aria-pressed": String(on), title: on ? `🕳️ trap door ${n} opens and shuts with this lever: tap to unjoin it` : `🕳️ trap door ${n} is not joined to this lever: tap to join it`,
                onclick: () => { const c = t[p] || []; t[p] = c.includes(d.uid) ? c.filter(u => u !== d.uid) : [...c, d.uid]; changed(); } }, `${on ? "🔗" : "➕"} 🕳️ ${n}`); })]; }
          if (how === "doordest") return doorDestParts(t, label, () => { shown = null; rows(); });
          if (how === "text") return [el("span", { class: "hint" }, label), el("input", { type: "text", maxlength: String(opts), size: String(opts), value: String(now ?? ""), "aria-label": label, oninput: e => { t[p] = e.target.value.slice(0, opts); changed(); } })];
          return [el("button", { "aria-pressed": String(!!t[p]), onclick: () => { t[p] = !t[p]; changed(); } }, label)];
      };
      list.replaceChildren(...T.map((t, i) => { const chips = [], lines = [];
        for (const q of thingParams(t.kind)) { if (t.kind === "passage" && q[0] === "to" && t.dest) continue;   // (ticket 525: linked, it leads where its dest says)
          const parts = partsFor(t, i, q), how = q[2];
          if (["color", "choice", "range", "dirs", "wires", "text", "doordest"].includes(how)) lines.push(el("div", { class: "tline tline-" + how }, parts)); else chips.push(...parts); }
        return el("div", { class: "row thingcard" }, [el("div", { class: "thead" }, [thingPic(i), el("span", { class: "name" }, `${thingLabel(t)} ${i + 1}`),
          el("button", { class: "tdel", title: "Take it away", onclick: () => { passageUnlink(t, false); /* (ticket 525: a cave door's twin goes with it) */ T.splice(i, 1); changed(); } }, "✖")]), ...(chips.length ? [el("div", { class: "tline tchips" }, chips)] : []), ...lines]); }));
    };
    // (v2: dragged onto the picture, it stands where it was let go, kept within
    // the ground the friend can stand on; tapped, near the middle, a little
    // apart from the last)
    const add = (kd, at = null, extra = {}) => {
      const B = state.background, T = B.things || (B.things = []), i = T.length, F = sceneFrame;
      // (ticket 502: a 🪵 fence piece tapped in goes on the tile beside the last one, as it is, so tapping again builds a fence: tileNext)
      const next = !at && (THING_KINDS[kd] || {}).tiles ? tileNext(B, kd) : null;
      const x = next ? next.x : at ? Math.round(clamp(at.x, F.left, F.right)) : Math.round((((i * 0.37) % 1) - 0.5) * 520);
      const z = next ? next.z : at ? Math.round(SCENE.ground + clamp(at.y - SCENE.ground, figYMin(F), F.bottom - F.ground)) : Math.round(SCENE.ground + 10 + (i * 29) % 70);
      if (thingsLatch) return;   // (ticket 203: shut till the marked ones are dealt with)
      T.push({ kind: kd, x, z, seed: 900 + Math.floor(Math.random() * 9000), size: 100, mirror: false, ...structuredClone(THING_DEFAULTS[kd] || {}), ...(kd === "rock" ? { tone: ROCK_TONES[i % ROCK_TONES.length] } : {}),
               ...(kd === "trapdoor" ? { uid: "d" + Math.floor(Math.random() * 36 ** 6).toString(36) } : {}), ...(kd === "passage" ? { uid: passageUid() } : {}), ...extra });   // (ticket 525: a cave door's name, for its arrow and its twin)   // (ticket 284: a trap door's name for the levers wired to it)
      // (ticket 303, M: a lever and a trap door put in one place work together at once, whichever came first; a chip unjoins them)
      const nu = T[T.length - 1];
      if (next) Object.assign(nu, { style: next.style ?? nu.style, size: next.size ?? nu.size, mirror: !!next.mirror });
      tileSnap(B, nu);   // (ticket 502: a thing that stands on the floor's tiles goes onto the nearest; any other stays where it is put)
      if (kd === "lever") nu.works = T.filter(x => x.kind === "trapdoor" && x.uid).map(x => x.uid);
      if (kd === "trapdoor") for (const L of T) if (L.kind === "lever") L.works = [...(L.works || []), nu.uid];
      changed();
      if (thingMoves(T[T.length - 1])) thingCheck(guard);   // (ticket 201: how this computer takes it)
    };
    // (ticket 201) more moving things allowed here? and which move the most
    const slowNote = el("div", { class: "hint thing-slow", hidden: "" });
    const guard = () => {
      const moving = [...thingsMovingNow(), ...ownMovingNow()], total = moving.reduce((s, m) => s + m.n, 0), most = Math.max(0, ...moving.map(m => m.n));
      const slow = thingFps != null && thingFps < THING_FPS_MIN && total > 0, over = total > THING_MOVING_MAX;
      // (ticket 203) shut once too busy; open again only when emptied, or measured smooth since the last change
      const sig = JSON.stringify([state.background.kind, state.background.cauldronFumes, state.background.kind === "bubbleroom" ? state.background.foamMove ?? 0 : 0, (state.background.things || []).map(t => [t.kind, t.fumes, t.burning])]);   // (a cauldron made simple, a fire put out: measured again)
      if (slow || over) thingsLatch = true;
      else if (thingsLatch && (!total || (thingFps != null && thingFps >= THING_FPS_MIN + 4 && thingsMeasured === sig))) thingsLatch = false;
      if (thingsLatch && thingsMeasured !== sig && !thingFpsBusy) { const at = sig; thingCheck(() => { thingsMeasured = at; guard(); }); }
      const blocked = thingsLatch;
      for (const b of items.querySelectorAll("button[data-kind]")) {
        if (b.disabled !== blocked) b.disabled = blocked;
        const t = blocked ? "🐢 Nothing more here for now: this place is as busy as this computer can show smoothly. Take away one of the things marked first." : "Drag into the picture, or tap";
        if (b.title !== t) b.title = t;
      }
      const costly = new Set(blocked ? moving.filter(m => m.n > 0 && m.n >= most * 0.5).map(m => m.i) : []);
      [...list.children].forEach((r, i) => { const on = costly.has(i); if (r.classList.contains("thing-costly") !== on) r.classList.toggle("thing-costly", on); });
      for (const m of moving) {
        const g = m.el.querySelector(":scope > .cost-glow");
        if (costly.has(m.i) && !g) { const NS = "http://www.w3.org/2000/svg", w = document.createElementNS(NS, "g"), b = m.el.getBBox();
          w.setAttribute("class", "cost-glow"); w.setAttribute("pointer-events", "none");
          w.innerHTML = `<rect x="${f1(b.x - 6)}" y="${f1(b.y - 6)}" width="${f1(b.width + 12)}" height="${f1(b.height + 12)}" rx="10" fill="none" stroke="#d9480f" stroke-width="3" stroke-dasharray="9 6" vector-effect="non-scaling-stroke"/>`;
          m.el.append(w); }
        else if (!costly.has(m.i) && g) g.remove();
      }
      // (what to do about it: take a marked thing away; a marked cauldron's fumes made simple; her own, in 🏞️ Background)
      const T = state.background.things || [], fancy = [...costly].some(i => (T[i] || {}).kind === "cauldron" && T[i].fumes === "fancy");
      // (ticket 340: in the bubble room its own moving foam costs more than any thing put there)
      const foam = state.background.kind === "bubbleroom" && (state.background.foamMove ?? 0) > 0 && !foamLayers.on;   // (ticket 383: layered, the foam costs little)
      const t = blocked ? `🐢 ${slow || thingFps != null && thingFps < THING_FPS_MIN + 4 ? `About ${Math.round(thingFps)} frames a second here` : `${total} moving parts here`}: nothing more can be put here until ` +
        ([foam && "the foam is made still (it costs the most here: 🏞️ Background, 〰️ how much the foam moves, to 0)", [...costly].some(i => i !== "own") && "one of the things marked (they move the most) is taken away", fancy && "a marked cauldron's 💨 fumes made simple",
         costly.has("own") && "her cauldron (marked) made simple: 🏞️ Background, 💨 her cauldron's fumes"].filter(Boolean).join(", or ") || "it moves smoothly again") + "." : "";
      if (slowNote.textContent !== t) slowNote.textContent = t;
      if (slowNote.hidden !== !t) slowNote.hidden = !t;
    };
    const addButton = (kd, T, extra = {}) => {
      const b = el("button", { title: "Drag into the picture, or tap", "data-kind": kd }, "+ " + T.label);
      pressDrag(b, () => T.label.split(" ")[0], { onTap: () => add(kd, null, extra), onDrop: (under, ev) => { if (under && under.closest && under.closest("#pic")) add(kd, picUnits(ev), extra); } });
      return b;
    };
    // (v3: the library in groups: a chip per group, its things' buttons under it; the group open is kept while the game is open)
    const groups = el("div", { class: "row thing-groups" }), items = el("div", { class: "row thing-items" });
    const showGroup = () => {
      for (const b of groups.children) setPressed(b, b.dataset.group === thingGroup);
      if (thingGroup === "places") {
        // (ticket 284) 📦 every object of a place, chosen from a list of the places; the one the friend is in first
        const P = liftPlaces(), here = liftPlaceHere(state.background), cur = P.some(([k]) => k === liftPlace) ? liftPlace : [here, ...P.map(([k]) => k)].find(k => P.some(([q]) => q === k) && liftedOf(k).objs.size) || "forest";   // (here, if anything stands here; else the first place that has something)
        const pick = el("select", { "aria-label": "📦 the place to take from", onchange: e => { liftPlace = e.target.value; showGroup(); } },
          P.map(([k, label]) => el("option", k === cur ? { value: k, selected: "" } : { value: k }, label)));
        const names = [...liftedOf(cur).objs.keys()];
        items.replaceChildren(el("span", { class: "hint" }, "📦 from"), pick,
          ...(names.length ? names.map(name => addButton("lifted", { label: "📦 " + name }, { from: cur, name })) : [el("span", { class: "hint" }, "nothing standing here to take: choose another place")]));
      } else items.replaceChildren(...Object.entries(THING_KINDS).filter(([, T]) => T.group === thingGroup).map(([kd, T]) => addButton(kd, T)));
      if (typeof guard === "function") guard();
    };
    groups.append(...THING_GROUPS.map(([g, name]) => el("button", { "data-group": g, onclick: () => { thingGroup = g; showGroup(); } }, name)));
    showGroup();
    box.append(groups, items, slowNote, el("div", { class: "row" }, [el("button", { onclick: () => { for (const t of state.background.things || []) passageUnlink(t, false); state.background.things = []; changed(); } }, "🧹 take them all away"), n]), list);
    syncs.push(() => { const m = (state.background.things || []).length, t = m ? `${m} here: drag one to move it` : ""; if (n.textContent !== t) n.textContent = t; rows(); guard();
      // (ticket 201: every ten seconds, while there are things here that move)
      if (performance.now() - thingFpsAt > 10000 && box.isConnected && ((state.background.things || []).some(thingMoves) || ownMovingNow().length)) thingCheck(guard); });
  }
  // (ticket 185, Matthew) 🗺️ World design: places laid out on a big grid,
  // each joined to its neighbours by the doors on their shared side (no door
  // wired by hand; a door with no place beside it, or one that does not line
  // up with its partner, is a mistake to fix), and each filled from a library
  // of things.
  if (kind === "world") {
    // (v2) a palette of places to drag onto squares (a tap puts one in the
    // first empty square); the grid, a square per cell: its place, its ways
    // out as marks on its edges, a mark when a mistake is about it, where the
    // friend is; a tap goes there, a drag moves it (onto another square:
    // they swap) or takes it away (onto 🗑️); buttons for the grid's size;
    // then the square the friend is in: its ways out, the start, its
    // things; then the places off the grid with their ids. The mistakes are
    // near the top, only how many unless "show warnings" is on (M found them
    // distracting): then listed, and their squares red (worldWarnShown); in
    // two lists (Matthew: a lot of them overwhelm), those about the square
    // the friend is in (its own, or a way into it: a warning's key or to),
    // then those about the other squares.
    const palette = el("div", { class: "world-palette row", "data-box-name": "🎨 places to put in a square (drag one onto the grid)" }),   // (ticket 488: each part a box, named once)
       grid = el("div", { class: "world-grid" }), gridWrap = el("div", { class: "world-grid-wrap" }, [grid]), trash = el("div", { class: "world-trash hint" }, "🗑️ drop a square here to take it away (it waits below, unreachable, to be put back)"), unreach = el("div", { class: "world-unreach" });
    // (ticket 320) the size buttons at the right and bottom edges, or, with ↖️ on (not the default), at the left and top
    const sizeBtns = [["＋ column", 1, 0], ["－ column", -1, 0], ["＋ row", 0, 1], ["－ row", 0, -1]].map(([t, dc, dr]) => el("button", { "data-size": t, onclick: () => worldResize(dc, dr, worldNearEdge) }, t));
    const sizeWords = () => { for (const b of sizeBtns) { const t = b.dataset.size, w = worldNearEdge ? t + (/column/.test(t) ? " at the left" : " at the top") : t; if (b.textContent !== w) b.textContent = w;
      b.title = (/＋/.test(t) ? "Add " : "Take away ") + (/column/.test(t) ? "a column at the " + (worldNearEdge ? "left" : "right") : "a row at the " + (worldNearEdge ? "top" : "bottom")) + " edge" + (/－/.test(t) ? " (only while it is empty)" : "") + (worldNearEdge ? ": every square moves over" : ""); }
      nearChip.setAttribute("aria-pressed", String(worldNearEdge)); };
    const nearChip = el("button", { class: "flip", title: "＋ and － add or take away at the left and top edges instead of the right and bottom", onclick: () => { worldNearEdge = !worldNearEdge; sizeWords(); } }, "↖️ left and top edges");
    sizeWords();
    const sizes = el("div", { class: "row" }, [...sizeBtns, nearChip,
      el("button", { title: "The world as the game first lays it out: every square, and the things put in them, as they were", onclick: () => { if (confirm("Start the world over, as the game first lays it out? The squares and the things put in them go back as they were.")) { instanceUnapply(state.world); worldsInit(); const { id, name } = state.world;   // (ticket 406: this world again, still itself; the other worlds' links into it go)
          for (const w of Object.values(state.worldsAway)) for (const c of Object.values(w.cells)) for (const [d, L] of Object.entries(c.links || {})) if (L.world === id) delete c.links[d];
          state.world = Object.assign(clone(DEFAULTS.world), { id, name }); worldChoose(state.background.kind); changed(); }   /* (ticket 195: the place shown takes its square's own settings, a crossing's paths too, not the other way round) */ } }, "🌍 the first world again")]);
    // (ticket 524, Matthew: "we need to make each room, and its params, available to world builder"; "world design panel I
    // mean") ⚙️ the settings of the place shown (a square's, or an inside's of it: the cave room's floor and ceiling), the
    // 🏞️ Background tab's own rows (bgPlaceRows), so ✏️ 📍 only this square / 🧩 the template decides where a change goes,
    // as there. A toggle, closed to start; it builds the tab again (showTab), so the rows are built once, never in render.
    const settings = el("div", { class: "world-settings", "data-box-name": "⚙️ this place's settings" }, [
      el("div", { class: "row" }, [el("button", { class: "flip world-settings-toggle", "aria-pressed": String(worldSettingsOpen),
        title: "This place's own settings, as the 🏞️ Background tab has them: here, a change goes where ✏️ says (only this square, or every square of this place)",
        onclick: () => { worldSettingsOpen = !worldSettingsOpen; showTab(); } }, worldSettingsOpen ? "⚙️ hide this place's settings" : "⚙️ show this place's settings")]),
      ...(worldSettingsOpen ? [el("div", { class: "world-settings-boxes" }, bgPlaceRows())] : [])]);
    syncs.push(() => { const kd = state.background.kind, t = `⚙️ ${placeName(kd)}'s settings`, none = kd === "none";
      if (settings.dataset.boxName !== t) settings.dataset.boxName = t; if (settings.hidden !== none) settings.hidden = none; });
    const notHere = el("div", { class: "world-alert", role: "status", hidden: "" }), here = el("div", { class: "world-here" }), probs = el("div", { class: "world-probs" }), off = el("div", { class: "world-off", "data-box-name": "🚪 off the grid: the places inside others (tap one to go there, or drag it onto a square)" });
    // (ticket 195, M) every neighbour joined that can be
    const joinNote = el("span", { class: "hint" }), joinAll = el("button", { title: "Join every pair of neighbouring squares whose places both have a way that way (a forest crossing all eight; never into the beach's sea, a castle's or a house's back, or the waterfall itself), and take away every way that leads off the map's edge",
      // (ticket 232: and every way off the map's edge taken away, saying how many)
      onclick: () => { const cut = worldTrimEdges(state.world), n = worldJoinAll(state.world); changed();
        joinNote.textContent = (n ? `🔗 ${n} way${n === 1 ? "" : "s"} added` : "🔗 every neighbour that can be is joined") + (cut ? `; ${cut} off the edge taken away` : ""); } }, "🔗 all passageways");
    const warnCount = el("span", { class: "world-warn-count" }), warnChip = el("button", { class: "world-warn-chip", onclick: () => { worldWarnShow(!worldWarnShown); shown = null; render(); } });
    const warnings = el("div", { class: "row world-warn" }, [warnCount, warnChip]);
    const emojiOf = kind => placeName(kind).split(" ")[0];
    // (ticket 300: by kind, as 🏞️ Background sorts them, each kind's places together)
    const pal = worldPalette();
    for (const [, gname, kinds] of PLACE_GROUPS) {
      const here = kinds.filter(kd => pal.includes(kd));
      if (!here.length) continue;
      const row = el("div", { class: "world-palette-kind" }, [el("span", { class: "world-palette-name" }, gname)]);
      for (const kd of here) {
        const chip = el("button", { class: "world-chip", "data-kind": kd, title: "Drag " + placeName(kd) + " onto a square (or tap: the first empty square)" }, placeName(kd));
        pressDrag(chip, () => emojiOf(kd), { onDrop: under => { const sq = under && under.closest && under.closest("[data-square]"); if (sq) worldPut(sq.dataset.square, kd); },
          onTap: () => { const W = state.world; for (let r = 0; r < W.rows; r++) for (let c = 0; c < W.cols; c++) if (!W.cells[c + "," + r]) { worldPut(c + "," + r, kd); return; } } });
        row.append(chip);
      }
      palette.append(row);
    }
    let shown = null;
    // (ticket 406) the worlds' box and the links' editor: which way, which world (UI only, kept while the game is open)
    const worldsBox = el("div", { class: "world-worlds" });
    let linkOpen = false, linkDir = null, linkWorld = null, newTpl = "first";
    // (ticket 406) this square's ways linked to squares of worlds: a toggle, off to start; open, a way to choose (a compass),
    // a world (its chips), then a square on that world's grid drawn small (each square its emoji and its name, A1...):
    // tapped, the way leads there, and that square's way back leads here. Each link listed, ✖ to take it off.
    const linksEditor = (W, at) => {
      const links = Object.entries(at.links || {}), wrap = el("div", { class: "world-links" });
      const list = links.map(([d, L]) => { const T = worldById(L.world), ok = T && T.cells[L.key];
        return el("div", { class: "row world-link" }, [el("span", {}, `${WORLD_ARROWS[d]} ${d} → ${ok ? (T === W ? "" : "🌍 " + worldTitle(T) + ": ") + worldName(T, L.key) + " (" + squareName(L.key) + ")" : "🚧 gone"}`),
          el("button", { class: "world-unlink", title: "Take this link off (and the link back)", onclick: () => { worldUnlinkAt(W, W.at, d); changed(); } }, "✖")]); });
      wrap.append(el("div", { class: "row" }, [el("button", { class: "flip world-links-toggle", "aria-pressed": String(linkOpen),
        title: "Link a way out of this square to a square of another world (or of this one): walking out that way leads there",
        onclick: () => { linkOpen = !linkOpen; shown = null; render(); } }, `🌍 ways to other worlds${links.length ? " (" + links.length + ")" : ""}`)]), ...list);
      if (!linkOpen) return wrap;
      const dirs = Object.keys(WORLD_DIRS).filter(d => !worldNoWay(at.kind, d));
      if (!dirs.includes(linkDir)) linkDir = dirs[0] || null;
      const worlds = worldsAll();
      if (!worlds.some(w => w.id === linkWorld)) linkWorld = (worlds.find(w => w !== W) || W).id;
      const T = worldById(linkWorld);
      wrap.append(el("div", { class: "hint" }, "1. the way out:"), el("div", { class: "world-compass world-link-compass" }, ["NW", "N", "NE", "W", null, "E", "SW", "S", "SE"].map(d => d === null
          ? el("span", { class: "world-compass-mid", title: placeName(at.kind) }, placeName(at.kind).split(" ")[0])
          : el("button", { "aria-pressed": String(d === linkDir), "data-link-dir": d, ...(dirs.includes(d) ? {} : { disabled: "" }), class: at.links && at.links[d] ? "linked" : "",
                           onclick: () => { linkDir = d; shown = null; render(); } }, WORLD_ARROWS[d]))),
        el("div", { class: "hint" }, "2. the world:"), el("div", { class: "row" }, worlds.map(w => el("button", { "aria-pressed": String(w.id === linkWorld), "data-link-world": w.id,
          onclick: () => { linkWorld = w.id; shown = null; render(); } }, "🌍 " + worldTitle(w) + (w === W ? " (this one)" : "")))),
        el("div", { class: "hint" }, "3. the square it leads to:"));
      if (T) {
        const cur = at.links && at.links[linkDir];
        wrap.append(worldMiniGrid(T, (key, cell) => { const self = T === W && key === W.at;
          return { ok: !self && !(cell && worldNoWay(cell.kind, WORLD_OPP[linkDir])), chosen: cur && cur.world === T.id && cur.key === key, self, why: self ? ": this square" : cell ? ": no way back that side" : ": empty" }; },
          key => { if (linkDir && worldLink(linkDir, T.id, key)) { shown = null; render(); } }, "data-link-to"));
      }
      return wrap;
    };
    // (ticket 406) the worlds: one chip each (tapped, the friend goes there); this world's name; a new world from a template;
    // this world kept as a template; this world taken away (never the last)
    const worldsRender = () => {
      const W = state.world, all = worldsAll();
      const nameBox = el("input", { type: "text", maxlength: String(WORLD_NAME_MAX), size: "14", value: String(W.name || ""), placeholder: "world " + worldNum(W), "aria-label": "this world's name",
        onchange: e => { const v = e.target.value.trim().slice(0, WORLD_NAME_MAX); W.name = v; changed(); } });
      const tpls = worldTemplatesAll();
      if (!tpls.some(t => t.id === newTpl)) newTpl = tpls[0].id;
      const pick = el("select", { "aria-label": "the template for a new world", onchange: e => { newTpl = e.target.value; } }, tpls.map(t => el("option", { value: t.id, ...(t.id === newTpl ? { selected: "" } : {}) }, t.name)));
      worldsBox.replaceChildren(el("div", { class: "cap" }, "🌍 worlds"),
        el("div", { class: "row world-chips" }, all.map(w => el("button", { class: "world-world", "aria-pressed": String(w === W), "data-world": w.id,
          title: w === W ? "The world the friend is in" : "Go to this world", onclick: () => { if (w !== W) worldSwitch(w.id); } }, "🌍 " + worldTitle(w)))),
        el("label", { class: "row world-name-row" }, [el("span", { class: "hint" }, "✏️ this world's name:"), nameBox]),
        el("div", { class: "row" }, [el("span", { class: "hint" }, "➕ a new world from"), pick, el("button", { class: "world-new", onclick: () => worldNew(newTpl) }, "➕ new world")]),
        el("div", { class: "row" }, [el("button", { class: "world-keep-tpl", title: "Keep this world, its squares and things, as a template for new worlds", onclick: () => worldKeepTemplate() }, "📐 keep this world as a template"),
          ...(all.length > 1 ? [el("button", { class: "world-drop", title: "Take this world away, and every link into it", onclick: () => { if (confirm(`Take ${worldTitle(W)} away, with its squares and the things in them, and every way linked into it? This can't be undone.`)) worldDrop(); } }, "🗑️ take this world away")] : [])]));
    };
    const render = () => {
      worldsInit();
      const W = state.world, P = worldProblems(W), at = W.at && W.cells[W.at];
      const sig = JSON.stringify([W.id, worldsAll().map(w => [w.id, worldTitle(w), w.cols, w.rows, Object.entries(w.cells).map(([k, c]) => [k, c.kind, c.name || "", c.links || null])]), Object.keys(state.worldTemplates),
        linkOpen, linkDir, linkWorld, newTpl, state.background.kind, at && INSIDE_WHICH[at.kind] ? [state.background[INSIDE_WHICH[at.kind]], insideCount(state.background, at.kind)] : null, W.cols, W.rows, W.start, W.at, Object.entries(W.cells).map(([k, c]) => [k, c.kind, worldWays(c), c.name || ""]), P.map(p => p.text), worldWarnShown, (W.unreachable || []).map(u => [u.kind, u.from, u.name || ""])]);
      if (sig === shown) return;
      shown = sig;
      worldsRender();
      const bad = worldWarnShown ? worldFixSquares(W, P) : new Set();   // (ticket 191: red where a fix is suggested)
      grid.style.setProperty("--cols", W.cols); grid.style.setProperty("--rows", W.rows);   // (ticket 210: the squares' size, from the room, in the stylesheet)
      // (ticket 224, Matthew: "in FS mode for world design, let's show A, B, C ... and 1, 2, 3 as the row and column
      // labels, with scaled font") the columns' letters over the grid and the rows' numbers beside it, as squareName
      // names them (A1 at the top left) (.world-lab; full screen only till ticket 255, now in the tab too)
      const lab = (cls, t) => el("div", { class: "world-lab " + cls, "aria-hidden": "true" }, t);
      grid.replaceChildren(lab("corner", ""), ...Array.from({ length: W.cols }, (_, c) => lab("col", String.fromCharCode(65 + c))), ...Array.from({ length: W.cols * W.rows }, (_, i) => {
        const key = (i % W.cols) + "," + Math.floor(i / W.cols), cell = W.cells[key];
        if (!cell) return el("div", { class: "world-cell empty", title: squareName(key) + ": empty: drag a place here", "data-square": key }, squareName(key));
        // (its words: the designer's name for it, if it has one, else its place's)
        const name = placeName(cell.kind), [emoji, ...words] = name.split(" "), ways = worldLiveWays(W, key), own = String(cell.name || "").trim();
        const b = el("button", { class: "world-cell" + (bad.has(key) ? " bad" : "") + (W.at === key ? " here" : "") + (W.start === key ? " start" : ""), "data-square": key, "data-kind": cell.kind,
                                 title: `${squareName(key)}: ${own ? own + " (" + name + ")" : name}` + (ways.length ? ", ways out " + ways.map(d => WORLD_ARROWS[d]).join(" ") : "") + ". Tap to go there; drag to move it.",
                                 onclick: () => worldEnter(key) },
                     [el("span", { class: "world-emoji" }, emoji), el("span", { class: "world-name" }, own || words.join(" ")), ...ways.map(d => el("span", { class: "world-way w-" + d + (cell.links && cell.links[d] ? " linked" : "") }, ""))]);
        pressDrag(b, () => emoji, { onDrop: under => { if (!under || !under.closest) return; if (under.closest(".world-trash")) worldRemove(key); else { const sq = under.closest("[data-square]"); if (sq) worldMove(key, sq.dataset.square); } } });
        return b;
      }).flatMap((sq, i) => i % W.cols ? [sq] : [lab("row", String(i / W.cols + 1)), sq]));
      // the square the friend is in: its ways out (a forest crossing's are its paths), the start
      // (ticket 524) or an inside of it the friend is in (the cave room behind its falls): the square's card, named by both,
      // a way back to the square, and no ways out (they are the outside's); ⚙️ below has the inside's own settings
      const inside = !!at && at.kind !== state.background.kind && instanceOf(W) === W.at;
      if (at && (at.kind === state.background.kind || inside)) {
        const ways = worldWays(at), fix = worldWarnShown ? worldWaysToToggle(W, W.at) : { on: [], off: [] }, toggle = d => () => {
          if (at.links && at.links[d]) { worldUnlinkAt(W, W.at, d); changed(); return; }   // (ticket 406: a linked way off: its link, and the link back, go)
          if (at.kind === "crossing") state.background["cross" + d] = !state.background["cross" + d];   // (its paths, which worldKeep keeps in its square)
          else at.ways = ways.includes(d) ? ways.filter(x => x !== d) : [...ways, d].sort((x, y) => Object.keys(WORLD_DIRS).indexOf(x) - Object.keys(WORLD_DIRS).indexOf(y));
          changed();
        };
        // (its name: typed, it is kept on Enter or leaving the box (not each key, or the panel built again would take the caret); emptied, the place's own again)
        const nameBox = el("input", { type: "text", maxlength: String(WORLD_NAME_MAX), size: "18", value: String(at.name || ""), placeholder: worldNames(W, false)[W.at], "aria-label": "this square's name",
          onchange: e => { const v = e.target.value.trim().slice(0, WORLD_NAME_MAX); if (v) at.name = v; else delete at.name; changed(); } });
        // (ticket 224, Matthew: "The N, NE, E, SE, S, etc buttons that toggle nav arrows for each grid square should be
        // arranged in a little 9-cell grid (with the icon of the current square shown in the center of the 9)"; "name on
        // signs field ... has no margin/padding around it, and should have more emphasis on this FS mode meaning larger
        // font"; "when they are shown, ... the warnings below the nav arrow add/remove controls, not at the top of the
        // panel -- and use some dotted sub-panel to organize them more. The errors for other squares can be hidden
        // entirely until we make that the 'active square' to edit.") the ways out as a compass round the square's own
        // emoji; its name box with room round it and in it; with the warnings shown, this square's in a dotted box
        // under the compass, the other squares' only counted (their squares red on the grid)
        const wayBtn = d => {
            // (M, 2026-10-17) off the map's edge: can't be turned on (one on from before can still be turned off);
            // with the warnings shown, one off that would put a warning right, starred
            // (ticket 191) and one on that, taken away, would put a warning right: dotted too, ✕
            const on = ways.includes(d), none = worldNoWay(at.kind, d), off = worldOffMap(W, W.at, d) || none, hint = !on && fix.on.includes(d), drop = on && fix.off.includes(d);
            return el("button", { "aria-pressed": String(on), title: drop ? d + ": on, but taking it away would put a warning right" : none ? d + ": the waterfall itself, not a way to the next square" : off ? (on ? d + ": leads off the map; turn it off" : d + ": the edge of the map") : hint ? d + ": off, but on it would put a warning right" : d,
                                  class: hint ? "way-suggest" : drop ? "way-suggest-off" : off && on ? "way-offmap" : "", ...(off && !on ? { disabled: "" } : {}), "data-way": d, onclick: toggle(d) }, WORLD_ARROWS[d]);
        };
        const mid = el("span", { class: "world-compass-mid", "data-kind": at.kind, title: placeName(at.kind) }, emojiOf(at.kind));
        // (ticket 524) a square holding an inside an outside has several of: which one (a tree house's room, a tent, a castle room)
        const whichKey = !inside && INSIDE_WHICH[at.kind], B0 = state.background, whichNow = whichKey ? Math.round(B0[whichKey] ?? 1) : 0;
        const whichRow = whichKey ? [el("div", { class: "row world-which" }, [el("span", { class: "hint" }, "which one:"),
          ...Array.from({ length: insideCount(B0, at.kind) }, (_, i) => el("button", { "aria-pressed": String(whichNow === i + 1), "data-which": String(i + 1),
            onclick: () => { at[whichKey] = B0[whichKey] = i + 1; changed(); } }, `${emojiOf(at.kind)} ${at.kind === "treeroom" ? "room" : at.kind === "tent" ? "tent" : "room"} ${i + 1}`))])] : [];
        here.replaceChildren(el("div", { class: "cap" }, inside ? `📍 ${squareName(W.at)}: ${worldName(W, W.at)} → ${placeName(state.background.kind)} (inside)` : `📍 ${squareName(W.at)}: ${worldName(W, W.at)}`),
          ...(inside ? [el("div", { class: "row" }, [el("button", { class: "world-to-square", title: "Back out to the square this place is inside", onclick: () => worldEnter(W.at) }, `↩️ to ${squareName(W.at)}`)])] : []),
          el("label", { class: "row world-name-row" }, [el("span", { class: "hint" }, "✏️ its name on signs:"), nameBox]),
          ...whichRow,
          ...(inside ? [] : [el("div", { class: "world-ways-out" }, [el("span", { class: "hint" }, "ways out:"),
            el("div", { class: "world-compass" }, ["NW", "N", "NE", "W", null, "E", "SW", "S", "SE"].map(d => d ? wayBtn(d) : mid))])]),
          // (ticket 249, Matthew: "expose the three far away options to the world designer") a forest crossing's 🌄 off in
          // the distance, here too (its square keeps it: CROSS_KEYS)
          ...(at.kind === "crossing" && !inside ? [el("div", { class: "row world-back-row" }, [el("span", { class: "hint" }, "🌄 off in the distance:"),
            ...CROSS_BACKS.map(([v, name]) => el("button", { "aria-pressed": String((state.background.crossBack || "woods") === v), "data-back": v,
              // (Matthew, 0.0.191: "does change the background, but it stays stale when clicked": this panel is built
              // again only when the world changes; the chips shown chosen as they are clicked)
              onclick: e => { state.background.crossBack = v; for (const b of e.currentTarget.parentNode.querySelectorAll("[data-back]")) b.setAttribute("aria-pressed", String(b.dataset.back === v)); changed(); } }, name))])] : []),
          probs,
          el("div", { class: "row" }, [el("button", { "aria-pressed": String(W.start === W.at), onclick: () => { W.start = W.at; changed(); } }, W.start === W.at ? "🏁 the friend starts here" : "🏁 start here")]),
          ...(inside ? [] : [linksEditor(W, at)]));
      } else {
        // (ticket 210, Matthew) the picture shows a place that is not the square the friend is in (one chosen in
        // 🏞️ Background, or not on the grid yet): say so, and where it is on the grid if it is there
        here.replaceChildren(el("div", { class: "hint" }, "Tap a square to go there and change it."), probs);
      }
      // (at the top of the tab, over the warnings, so it is seen)
      // (ticket 524: an inside of the friend's square is shown from that square: instanceOf)
      const kind = state.background.kind, onGrid = !instanceOf(W) && worldPalette().includes(kind) ? Object.keys(W.cells).filter(k => W.cells[k].kind === kind) : null;
      notHere.hidden = !onGrid;
      notHere.textContent = !onGrid ? "" : onGrid.length
        ? `📺 Showing ${placeName(kind)} in the viewport, not from a square of the world design: it is at ${onGrid.map(squareName).join(", ")}; tap ${onGrid.length === 1 ? "it" : "one"} to go there.`
        : `📺 Showing ${placeName(kind)} in the viewport, not currently placed into the world design.`;
      warnCount.textContent = P.length ? `⚠️ ${P.length} warning${P.length === 1 ? "" : "s"}` : "✅ no warnings";
      warnChip.textContent = worldWarnShown ? "hide warnings" : "show warnings";
      warnChip.setAttribute("aria-pressed", String(worldWarnShown));
      probs.hidden = !worldWarnShown;
      const atKey = W.at && W.cells[W.at] ? W.at : null, mine = p => !!atKey && (p.key === atKey || p.to === atKey);
      const hereP = P.filter(mine), elseP = P.filter(p => !mine(p)), list = ps => ps.map(p => el("div", { class: "world-prob" }, p.text));
      // (ticket 224: the other squares' only counted, their squares red on the grid: tapped, a square's own are listed)
      probs.replaceChildren(...(!P.length ? [el("div", { class: "hint" }, "✅ Every way out meets a way back, and every place can be reached.")] : [
        ...(atKey ? [el("div", { class: "world-probs-here" }, [el("div", { class: "cap" }, `📍 This square, ${squareName(atKey)}: ${hereP.length ? hereP.length : "nothing to put right"}`), ...list(hereP)])] : []),
        ...(elseP.length ? [el("div", { class: "world-probs-else", "data-count": String(elseP.length) }, [el("div", { class: "hint" },
          `🗺️ ${atKey ? "Other squares" : "The squares"}: ${elseP.length}, red on the grid: tap one to see its own`)])] : [])]));
      // (ticket 300) the squares taken away, unreachable: each a chip to drag back onto a square (tap: the first empty one), or onto 🗑️ to forget it
      const U = W.unreachable || [];
      unreach.replaceChildren(...(U.length ? [el("div", { class: "hint" }, `🫥 unreachable in this world design (${U.length}): taken away, kept as they were; drag one back onto a square, or tap it`),
        el("div", { class: "row" }, U.map((u, i) => { const [emoji, ...words] = placeName(u.kind).split(" "), own = String(u.name || "").trim();
          const chip = el("button", { class: "world-chip", title: `${own || words.join(" ")}, from ${squareName(u.from)}: drag onto a square to put it back (tap: the first empty square); onto 🗑️ to forget it` }, `${emoji} ${own || words.join(" ")} (was ${squareName(u.from)})`);
          pressDrag(chip, () => emoji, { onDrop: under => { if (!under || !under.closest) return; if (under.closest(".world-trash")) worldForget(i); else { const sq = under.closest("[data-square]"); if (sq) worldRestore(i, sq.dataset.square); } }, onTap: () => worldRestore(i) });
          return chip; }))] : []));
      // (ticket 524) each place off the grid a chip: tapped, the friend goes there (worldVisit: an inside through its outside's
      // square); dragged onto a square, it is put there as any place (worldPut), a square of its own
      off.replaceChildren(el("div", { class: "row world-off-chips" }, worldOffGrid(W).map(o => {
        const [emoji] = placeName(o.kind).split(" "), on = state.background.kind === o.kind && (!o.which || Math.round(state.background[INSIDE_WHICH[o.kind]] ?? 1) === o.which) && (o.from ? W.at === o.from : !instanceOf(W));
        const chip = el("button", { class: "world-chip world-off-chip", "data-kind": o.kind, ...(o.from ? { "data-from": o.from } : {}), "aria-pressed": String(on),
          title: `${o.id}: ${placeName(o.kind)}` + (o.from ? `, through ${placeName(W.cells[o.from].kind)} (${squareName(o.from)})` : "") + ". Tap to go there; drag onto a square to put it there." },
          `${placeName(o.kind)}` + (o.from ? ` (${squareName(o.from)})` : ""));
        pressDrag(chip, () => emoji, { onDrop: under => { const sq = under && under.closest && under.closest("[data-square]"); if (sq) worldPut(sq.dataset.square, o.kind, o); }, onTap: () => worldVisit(o) });
        return chip; })));
    };
    // (ticket 224, Matthew: "in FS mode for world designer, we want to let the user scroll the RHS (with controls and
    // things to add) and the LHS (the grid of squares) stays in place, and maximal -- maybe also a dragger for LHS vs RHS
    // size"; "all of the sub-panels of FS world designer should use the improved sub-panel design of the FS music panel
    // (squar-ish sub-panels, next to each other, and organized into a grid of their own)") The grid's column (and 🗑️
    // under it), a dragger, and the rest. In the tab, one under another in the old order (the stylesheet's order);
    // full screen, the grid on the left, held in place as the right side scrolls past it, as large as the window
    // allows, the dragger setting the split (kept in this browser), the rest as cards in a grid of their own.
    const things = control("background", ["things", "things", "🪨 things here: drag one into the picture, then about"]);
    things.classList.add("world-things");
    const gridcol = el("div", { class: "world-gridcol" }, [gridWrap, el("div", { class: "row" }, [trash]), unreach]);
    const split = el("div", { class: "world-split", role: "separator", "aria-orientation": "vertical", title: "Drag: more room for the grid, or for the controls" });
    // (ticket 231, Matthew: "show the in-game view as the first sub-panel, the full width of the RHS of the full screen
    // view") full screen, the right side's first card: the picture itself, put in it by placePic, held in place as the
    // cards below scroll; its 👁️ folds it away (worldViewShow), shown unless folded
    const view = el("div", { class: "world-view" }, [el("div", { class: "row world-view-head" }, [
      el("button", { class: "eye", onclick: () => worldViewShow(worldViewHidden) }, "👁️"), el("span", { class: "hint" }, "📺 in-game view")]), el("div", { class: "world-view-pic" })]);
    worldViewMark(view);
    const layout = el("div", { class: "world-layout" }, [
      el("div", { class: "world-rest" }, [view, worldsBox, notHere, warnings, palette, el("div", { class: "world-sizes", "data-box-name": "📐 the world's size" }, [sizes, el("div", { class: "row" }, [joinAll, joinNote])]), here, settings, things, off]),
      split, gridcol]);
    designerSplit(layout, split, "dress-up-world-split");
    box.append(el("div", { class: "hint world-intro" }, "🗺️ The world: a place in each square, joined to the squares beside it through the ways out on their shared sides. Drag places onto squares; tap a square to go there and change it."), layout);
    syncs.push(render);
  }
  // (ticket 155, Matthew) ⚙️ Advanced: the game's code on GitHub; its
  // version, and a newer one to change to (a phone hides the title, where
  // they are otherwise); the frame rate now; and the background features
  // kept off until they are fast (PERF_FEATURES), each with how many there
  // are in this place, and a measure of what it costs on this computer.
  if (kind === "scripting") { scriptingPanel(box); return box; }   // (ticket 482)
  if (kind === "advanced") {
    // (syncs run on every frame of a walk: the page is changed only when a value changes)
    const put = (node, text) => { if (node.textContent !== text) node.textContent = text; };
    const ver = el("span", { class: "hint" }), up = el("button", { class: "newer", hidden: "", onclick: () => document.getElementById("newer").click() });
    const meter = el("div", { class: "hint" });
    // (ticket 194, M) light or dark
    const themes = el("div", { class: "row theme-row" }, [el("span", { class: "hint" }, "light or dark:"), ...LOOKS.map(([t, label]) =>
      el("button", { "data-theme-choice": t, "aria-pressed": String(themeNow() === t), onclick: () => { themeSet(t); for (const b of themes.querySelectorAll("button")) b.setAttribute("aria-pressed", String(b.dataset.themeChoice === t)); } }, label))]);
    // (ticket 325, Matthew: "advanced panel links to matthewhammer.org/hazel/dress-up page") the game's own page: its music, pictures and reports
    // (lines only, for printing: ticket 593)
    const lineChip = el("button", { class: "lineart-chip", "aria-pressed": String(lineArtNow()), onclick: () => { lineArtSet(!lineArtNow()); lineChip.setAttribute("aria-pressed", String(lineArtNow())); } }, "🖍️ lines only (no fills, for printing)");
    themes.append(lineChip);
    box.append(themes, el("div", { class: "row" }, [el("a", { href: "https://matthewhammer.org/hazel/dress-up/", target: "_blank", rel: "noopener" }, "🏠 this game's page: its music, pictures and reports ↗")]),
               el("div", { class: "row" }, [el("a", { href: "https://github.com/matthewhammer/kids-choice-games", target: "_blank", rel: "noopener" }, "💻 this game's code, on GitHub ↗")]),
               // (ticket 500, Matthew: "the two licences, credit 'Matthew Hammer and family (M and N)', whole repo.") who made it, and on what terms
               el("div", { class: "row licence" }, [el("span", { class: "hint" }, ["📜 © 2026 Matthew Hammer and family (M and N). Reuse is welcome, with credit: the code under ",
                 el("a", { href: "https://github.com/matthewhammer/kids-choice-games/blob/main/LICENSE", target: "_blank", rel: "noopener" }, "Apache 2.0 ↗"),
                 ", and the pictures, music and words under ",
                 el("a", { href: "https://creativecommons.org/licenses/by/4.0/", target: "_blank", rel: "noopener" }, "CC BY 4.0 ↗"), "."])]),
               // (ticket 474, Matthew: "add link and citation for https://arxiv.org/abs/1703.05410 into the MUDU advanced panel, near the top --- the
               // link says 'See also -- Languages of Play: Towards Semantic Foundations For Game Interfaces (2017)'"; "there's an HTML version ... let's
               // offer that in the advanced panel (after citation)") the paper, its authors, and its HTML version
               el("div", { class: "row see-also" }, [
                 el("span", { class: "hint" }, "📖 See also —"),
                 el("a", { href: "https://arxiv.org/abs/1703.05410", target: "_blank", rel: "noopener" }, "Languages of Play: Towards Semantic Foundations For Game Interfaces (2017) ↗"),
                 el("span", { class: "hint" }, "Chris Martens and Matthew A. Hammer, arXiv:1703.05410;"),
                 el("a", { href: "https://arxiv.org/html/1703.05410v1", target: "_blank", rel: "noopener" }, "as a web page ↗")]),
               // (ticket 486, Matthew: "In the advanced tab, let's offer an expandable panel that instructs the user how to add the app to
               // their device (esp mobile and tablets) so they can use in Airplane mode.") folded, open on a tap
               offlineHelp(),
               el("div", { class: "row" }, [ver, up]),
               // (ticket 302, M) a pop-up when a new version is out: off unless switched on here, this browser's own
               (() => { const b = el("button", { class: "ask-new-toggle", "aria-pressed": String(askNewOn()), onclick: () => { askNewSet(!askNewOn()); b.setAttribute("aria-pressed", String(askNewOn())); } },
                                    "🔔 a pop-up when a new version is out");
                        return el("div", { class: "row" }, [b]); })(),
               // (ticket 364, Matthew, for N) pull a lever from where the friend stands, not walking to it: standing where the boulder
               // falls, and pulling its lever; saved with the friend's settings (access.leverFar), off to start
               (() => { const A = () => state.access || (state.access = clone(DEFAULTS.access));
                        const b = el("button", { class: "lever-far-toggle", "aria-pressed": String(!!A().leverFar), onclick: () => { A().leverFar = !A().leverFar; b.setAttribute("aria-pressed", String(A().leverFar)); changed(); } },
                                    "🎚️ pull levers from where you stand (no walking to them)");
                        syncs.push(() => setPressed(b, !!A().leverFar));
                        return el("div", { class: "row" }, [b]); })(),
               meter,
               el("div", { class: "hint" }, "🚀 Kept off until they are fast: 60 frames a second, for everything the controller does, comes first. Switched on, each says how many it draws in this place; 📏 walks the friend here with it off, then on, and says what it costs on this computer."));
    perfMeter.node = meter;
    // (ticket 163) 🧪 this computer's speed, a line to copy and compare
    const speedOut = el("input", { type: "text", readonly: "", class: "padline", placeholder: "🧪 the result shows here", onfocus: e => e.target.select() });
    const speedBtn = el("button", { onclick: () => { speedCopy.textContent = "📋 copy"; speedCheck(t => { speedOut.value = t; }); } }, "🧪 this computer's speed");
    const speedCopy = el("button", { onclick: () => { speedOut.select(); try { navigator.clipboard.writeText(speedOut.value).then(() => { speedCopy.textContent = "📋 copied"; }, () => document.execCommand("copy")); } catch (e) { document.execCommand("copy"); } } }, "📋 copy");
    // (ticket 166: for now, a shared page to paste the line into, readable from anywhere, until a
    // Fumola canister of our own takes its place; the black laptop has no easy way to send it)
    const speedPad = el("a", { href: SPEED_PAD, target: "_blank", rel: "noopener", title: "A shared page anyone with its address can read and write: paste the line there" }, "📝 then paste it here");
    // (ticket 166: with the canister's log deployed, a press sends the line there instead)
    const speedSend = el("button", { title: "Sends the line to Matthew's log, where he reads it", onclick: () => sendSpeedLine("speed", speedOut.value, speedSend) }, "📮 send it to Matthew");
    box.append(el("div", { class: "perfrow" }, [el("div", { class: "row" }, [speedBtn, speedCopy, ...(SPEED_LOG ? [speedSend] : []), speedPad]), speedOut,
      el("div", { class: "hint" }, "About 40 seconds, in the empty room and then swimming in the duck pond (moving, held still, then without one part at a time: the scenery, the friend's part under the water, the rings; its scenery as a still picture; with its music), then the empty room again, with the friend as they start, the same on every computer; then your friend and everything back as it was. Run it on two computers and compare the lines: the drawing times' ratio is how much slower one is.")]));
    // (⏱️ time the next trip: a line to copy and paste, as 🧪's)
    const tripOut = el("input", { type: "text", readonly: "", class: "padline", placeholder: "⏱️ the trip's timing shows here", onfocus: e => e.target.select() });
    const tripCopy = el("button", { onclick: () => { tripOut.select(); try { navigator.clipboard.writeText(tripOut.value).then(() => { tripCopy.textContent = "📋 copied"; }, () => document.execCommand("copy")); } catch (e) { document.execCommand("copy"); } } }, "📋 copy");
    const tripBtn = el("button", { onclick: () => { tripCopy.textContent = "📋 copy"; tripArm(t => { tripOut.value = t; }); } }, "⏱️ time the next trip");
    const tripPad = el("a", { href: SPEED_PAD, target: "_blank", rel: "noopener", title: "A shared page anyone with its address can read and write: paste the line there" }, "📝 then paste it here");
    const tripSend = el("button", { title: "Sends the line to Matthew's log, where he reads it", onclick: () => sendSpeedLine("trip", tripOut.value, tripSend) }, "📮 send it to Matthew");
    box.append(el("div", { class: "perfrow" }, [el("div", { class: "row" }, [tripBtn, tripCopy, ...(SPEED_LOG ? [tripSend] : []), tripPad]), tripOut,
      el("div", { class: "hint" }, "Press it, then go through a doorway: click a castle door or a pit, press X or 🚪, or walk onto a pit. Every frame is timed from the start of the trip until the friend has arrived and stood still a moment; the line says which frames were long, in which part of the trip (walking, stepping in, falling, arriving, landing), and how much of each was the game's own work.")]));
    // (ticket 568) 🔬 the scripts' cost: per event the scripts handle, its time in each engine against JS's own, from the calls made
    // while playing (scriptCost); drawn again once a second while the tab is open, not on every frame
    const costBody = el("tbody"), costNote = el("div", { class: "hint" });
    const costCell = (x, js) => { if (!x) return el("td", { class: "n muted" }, "—");
      const over = js && x !== js ? ` (${(x.median / Math.max(0.001, js.median)).toFixed(0)}×)` : "";
      return el("td", { class: "n", title: `${x.n} calls; median ${x.median} ms, worst ${x.worst} ms; ${(100 * x.median / (1000 / 60)).toFixed(1)}% of a frame` }, `${x.median} / ${x.worst}${over}`); };
    const costDraw = () => {
      const C = typeof scriptCost === "function" ? scriptCost() : { rows: [], compile: {} };
      costBody.replaceChildren(...(C.rows.length ? C.rows.map(r => el("tr", {}, [el("td", {}, `${r.tag}`), el("td", { class: "muted" }, r.room || ""),
        costCell(r.js), costCell(r.hazel, r.js), costCell(r.hazelFn), costCell(r.fumola, r.js), costCell(r.fumolaFn)]))
        : [el("tr", {}, el("td", { colspan: "7", class: "muted" }, "no calls yet: pull a lever, fall in a pit, take a way out, with 📜 Scripting's engine on JS, Hazel or Fumola"))]));
      costNote.textContent = `set-up: Hazel ${C.compile.hazel == null ? "—" : C.compile.hazel + " ms"}, Fumola ${C.compile.fumola == null ? "—" : C.compile.fumola + " ms"} · a frame is 16.7 ms · ms median / worst; (n×) is the median over JS's own`;
    };
    const costBtn = el("button", { onclick: () => { if (typeof scriptCostClear === "function") scriptCostClear(); costDraw(); } }, "🧹 start again");
    box.append(el("div", { class: "perfrow script-cost" }, [el("div", { class: "row" }, [el("span", { class: "cap" }, "🔬 the scripts' cost"), costBtn]),
      el("div", { class: "scroll" }, el("table", { class: "cost" }, [el("thead", {}, el("tr", {}, ["event", "room", "JS", "Hazel", "Hazel's rule alone", "Fumola", "Fumola's rule alone"].map(h => el("th", {}, h)))), costBody])),
      costNote,
      el("div", { class: "hint" }, "Each event a room's script answers (a lever pulled, a pit fallen in, a way taken), timed in the game as you play: the whole event in each engine, from the game's call to the last command acted on, and for Hazel and Fumola the rule's function alone (the event in, the commands out). Switch 📜 Scripting's engine and play the same rooms to fill each column; 🧹 starts the counts again.")]));
    costDraw(); { const t = setInterval(() => { if (!costBody.isConnected) { clearInterval(t); return; } costDraw(); }, 1000); }
    if (!perfMeter.loop) perfMeter.loop = requestAnimationFrame(perfMeterTick);
    const rows = PERF_FEATURES.map(f => {
      const chip = el("button", { onclick: () => perfSet(f.id, !PERF[f.id]) }), here = el("div", { class: "hint" }), result = el("div", { class: "hint" });
      const measure = el("button", { onclick: () => perfMeasure(f, t => { result.textContent = t; }) }, "📏 measure here");
      box.append(el("div", { class: "perfrow" }, [el("div", { class: "row" }, [chip, measure]), el("div", { class: "hint" }, f.what), here, result]));
      return [f, chip, here];
    });
    // (ticket 200, Matthew: "M wants to read the list of recent features as we work") what's new: every
    // version and its notes, from VERSIONS.md beside the game, as cards, newest first, this one marked;
    // the tab's last part, scrolling on its own
    const news = el("div", { class: "news", tabindex: "0", "aria-label": "what's new: every version and its notes" }, el("div", { class: "hint" }, "📰 …"));
    box.append(el("div", { class: "cap news-cap" }, "📰 What's new, version by version"), news);
    newsLoad(news);
    let counted = "";
    syncs.push(() => {
      put(ver, `🏷️ version ${VERSION.number}, out ${VERSION.released}`);
      if (up.hidden !== !offered) up.hidden = !offered;
      if (offered) put(up, "🆕 change to " + offered.number);
      // (counted again only when the place is drawn anew)
      const key = `${bgShown.text ? bgShown.text.length : 0}:${state.background.kind}:${JSON.stringify(PERF)}`;
      for (const [f, chip, here] of rows) {
        put(chip, `${f.label}: ${PERF[f.id] ? "on" : "off"}`);
        if (chip.getAttribute("aria-pressed") !== String(PERF[f.id])) chip.setAttribute("aria-pressed", PERF[f.id]);
        if (f.status) put(here, f.status());
        else if (key !== counted) { const n = PERF[f.id] ? f.count() : 0; put(here, PERF[f.id] ? `here: ${n} ${n === 1 ? f.unit[0] : f.unit[1]}` : "off (as it starts): none drawn"); }
      }
      counted = key;
    });
  }
  // (ticket 138) the game controller: whether one is there, then each of its
  // buttons with a menu of what it does (lit while held), and the usual layout
  if (kind === "padmap") {
    const status = el("div", { class: "hint" }), rows = el("div", { class: "padmap" }), down = el("div", { class: "hint" });
    const how = el("div", { class: "hint" }, ["Its arrows (or its left stick) walk the friend that way, diagonals too, from standing or sitting: ⬆️ away from us, into the place, ⬇️ toward us. " +
                                             "Hold 🏃 run with an arrow to run; hold 🧎 crouch to crouch down, or crawl with an arrow. In water they swim, and run swims fast. " +
                                             "To change a button, click 🎯 set, then press the button you want."]);
    const rowsOf = PAD_ACTIONS.map(([a, name]) => {
      const which = el("span", { class: "pwhich" }), learn = el("button", { onclick: () => { padLearn = padLearn === a ? null : a; changed(); } }, "🎯 set");
      const row = el("div", { class: "padrow" }, [el("span", { class: "pbtn" }, name), which, learn]);
      rows.append(row);
      return [a, which, learn, row];
    });
    // (ticket 146) 🎓 teach me your controller: press each button as asked
    const ask = el("div", { class: "padteach" }), line = el("input", { type: "text", readonly: "", class: "padline", onfocus: e => e.target.select() });
    const copy = el("button", { onclick: () => { line.select(); try { navigator.clipboard.writeText(line.value).then(() => { copy.textContent = "📋 copied"; }, () => document.execCommand("copy")); } catch (e) { document.execCommand("copy"); } } }, "📋 copy");
    const teach = el("button", { onclick: () => { padLearn = null; padTeach = { step: 0, got: {} }; copy.textContent = "📋 copy"; changed(); } }, "🎓 teach me your controller");
    const skip = el("button", { onclick: () => { if (padTeach && !padTeach.done) { if (++padTeach.step >= PAD_TEACH.length) padTaught(); changed(); } } }, "skip: it has none");
    const stop = el("button", { onclick: () => { padTeach = null; changed(); } }, "stop");
    const askText = el("span", { class: "padask" }), done = el("div", { class: "hint" }, [line, copy]);
    ask.append(askText, skip, stop);
    // (ticket 150) the on-screen controller, for anyone, with or without a real one
    const onscreen = el("button", { onclick: () => touchWant(!touchPad.want) });
    box.append(status, onscreen, how, teach, ask, done, rows, down, el("button", { onclick: () => { padLearn = null; padTeach = null; Object.assign(state[part], padId === TOUCH_ID ? TOUCH_CONTROLS : DEFAULTS.controls); padKeep(); changed(); } }, "↺ the usual buttons"));
    syncs.push(() => {
      for (const [a, which, learn, row] of rowsOf) {
        const i = state[part][a];
        which.textContent = padLearn === a ? "press a button…" : padButtonName(i);
        learn.setAttribute("aria-pressed", padLearn === a);
        row.classList.toggle("down", i >= 0 && padDown.has(i));
      }
      status.textContent = padName ? `🎮 ${padName}: ready${padMapping === "standard" ? "" : " (its buttons are numbered its own way: set them with 🎯 if they are not where you want)"}.`
                                   : "🎮 No game controller yet: plug one in (USB) or pair it, then press one of its buttons. Or use the one on the screen.";
      onscreen.textContent = touchPad.want ? "🎮 hide the controller on the screen" : "🎮 a controller on the screen, to touch or click";
      onscreen.setAttribute("aria-pressed", touchPad.want);
      down.textContent = padName ? "held now: " + ([...padDown].map(padButtonName).join(", ") || "nothing") + (padCrawl ? " · 🐛 crawling is on" : "") : "";
      const teaching = !!padTeach && !padTeach.done;
      teach.hidden = !padName || teaching;
      ask.hidden = !teaching;
      if (teaching) askText.textContent = `🎓 ${padTeach.step + 1} of ${PAD_TEACH.length}: press ${PAD_TEACH[padTeach.step][1]}`;
      done.hidden = !(padTeach && padTeach.done);
      if (padTeach && padTeach.done) line.value = padTeach.line;
    });
  }
  if (kind === "action") {
    const b = el("button", { onclick: () => { rest[0](); changed(); } }, label);
    lbl.replaceChildren();
    box.append(b);
  }
  if (kind === "range") {
    const [lo, hi, , , limit] = rest;
    const input = el("input", { type: "range", min: lo, max: hi,
                                oninput: e => set(part, key, Number(e.target.value)) });
    box.append(input);
    // (with a limit of its own, from the settings, as a room's doors are: the slider stops there, and shows no more)
    syncs.push(() => { const most = typeof limit === "function" ? limit(state[part]) : hi, v = Math.min(most, state[part][key]);   // (ticket 197: written only when it changes)
                       if (+input.max !== most) input.max = most; if (+input.value !== v) input.value = v; if (val.textContent !== String(v)) val.textContent = v; });
    // a part's see-through-ness can follow one of the mixer's LFOs: a chip
    // beside its name, a tap for the next (none, A, B, C)
    if (part === "visible" && VISIBLE_PARTS.includes(key) && !buildingHud) {
      const lk = "lfo_" + key, now = () => LFO_NAMES.includes(state.visible[lk]) ? state.visible[lk] : "none";
      const chip = el("button", { class: "ms lfo-chip", title: "Fade in and out with an LFO (the music's): tap for the next",
        onclick: () => { const all = ["none", ...LFO_NAMES]; state.visible[lk] = all[(all.indexOf(now()) + 1) % all.length]; changed(); } });
      lbl.insertBefore(chip, val);
      syncs.push(() => { setText(chip, now() === "none" ? "〰️ no LFO" : "〰️ LFO " + now()); setPressed(chip, now() !== "none"); });
    }
  } else if (kind === "turn") {
    // an angle in radians, from 0 to a period that the other settings
    // decide (a star's turn to its next point), in hundredths shown; past
    // the period (fewer points made it shorter) it shows, as it draws,
    // the same turn within the period
    const period = () => rest[0](state[part]);
    const input = el("input", { type: "range", min: 0, step: "any",
                                oninput: e => set(part, key, Number(e.target.value)) });
    box.append(input);
    syncs.push(() => {
      const p = period(), v = state[part][key] ?? 0, w = v > p ? v % p : v;
      input.max = p; input.value = w; val.textContent = w.toFixed(2);
    });
  } else if (kind === "toggle") {
    const b = el("button", { onclick: () => set(part, key, !state[part][key]) });
    box.querySelector(".lbl").textContent = "";
    box.append(b);
    syncs.push(() => { setText(b, label + (state[part][key] ? ": on" : ": off"));
                       setPressed(b, !!state[part][key]); });
  } else if (kind === "tunes") {
    // two rows of chips: the kind of music (or the scene's own tune), then
    // that kind's tunes; picking a kind plays its first tune, unless the
    // tune playing is already of that kind
    // (each row in a dashed box with its name, so the kinds and the tunes
    // read apart)
    const kinds = el("div", { class: "row" }), tunes = el("div", { class: "row" });
    const tuneCap = el("div", { class: "cap" });
    // (ticket 226, Matthew: compact, as the music's full screen: each row's name at its start, no dashed boxes)
    box.classList.add("tunesctl");
    const kindBox = el("div", { class: "chipset" }, [el("div", { class: "cap" }, "genre"), kinds]);
    const tuneBox = el("div", { class: "chipset" }, [tuneCap, tunes]);
    const kindBtns = [["scene", "🏞️ the scene's own"], ...TUNE_GROUPS.map(([g, text]) => [g, text])].map(([g, text]) => {
      const b = el("button", { onclick: () => {
        if (g === "scene") return set(part, key, "scene");
        const list = TUNE_GROUPS.find(x => x[0] === g)[2];
        if (!list.includes(state[part][key])) set(part, key, list[0]);
      } }, text);
      kinds.append(b); return [g, b];
    });
    box.append(kindBox, tuneBox);
    let shownKind, tuneBtns = [];
    syncs.push(() => {
      const now = state[part][key], g = groupOfTune(now);
      for (const [v, b] of kindBtns) setPressed(b, v === g);
      if (g !== shownKind) {
        shownKind = g;
        const list = g && g !== "scene" ? TUNE_GROUPS.find(x => x[0] === g)[2] : [];
        tuneBtns = list.map(id => [id, el("button", { onclick: () => set(part, key, id) }, TUNES[id].name)]);
        tunes.replaceChildren(...tuneBtns.map(([, b]) => b));
        tuneBox.hidden = !tuneBtns.length;
        tuneCap.textContent = tuneBtns.length ? `track (${TUNE_GROUPS.find(x => x[0] === g)[1]})` : "";
      }
      for (const [v, b] of tuneBtns) setPressed(b, v === now);
    });
  } else if (kind === "every") {
    // chips that set every slider of the part at once (visibility's
    // invisible, visible and ghost), each pressed while all are at it
    const row = el("div", { class: "row" });
    const keys = () => TABS.find(t => t[1] === part)[2].filter(c => c[0] === "range").map(c => c[1]);
    const bs = rest[0].map(([text, v]) => {
      const b = el("button", { onclick: () => { for (const k of keys()) state[part][k] = v; state[part][key] = v; changed(); } }, text);
      row.append(b); return [v, b];
    });
    box.append(row);
    syncs.push(() => { for (const [v, b] of bs) setPressed(b, keys().every(k => state[part][k] === v)); });
  } else if (kind === "master") {
    // one slider for every slider of the part: moving it sets each of them
    // to its value, wherever they were. It keeps its own value; once a part
    // is moved on its own, the parts no longer all match it, and it dims
    // ("mixed") until it is moved again.
    const [lo, hi] = rest;
    const keys = () => TABS.find(t => t[1] === part)[2].filter(c => c[0] === "range").map(c => c[1]);
    const own = () => state[part][key] ?? hi;
    const input = el("input", { type: "range", min: lo, max: hi,
      oninput: e => {
        const v = Number(e.target.value);
        state[part][key] = v;
        for (const k of keys()) state[part][k] = v;
        changed();
      } });
    box.classList.add("master");
    box.append(input);
    // and one LFO for them all: a tap sets every part to follow the next one
    // (none, A, B, C); "mixed" when they follow different ones
    if (part === "visible" && !buildingHud) {
      const lfoNow = () => { const ls = keys().map(k => LFO_NAMES.includes(state[part]["lfo_" + k]) ? state[part]["lfo_" + k] : "none"); return ls.every(l => l === ls[0]) ? ls[0] : "mixed"; };
      const chip = el("button", { class: "ms lfo-chip", title: "Every part fades in and out with one LFO (the music's): tap for the next",
        onclick: () => { const all = ["none", ...LFO_NAMES], cur = lfoNow(), next = all[(all.indexOf(cur === "mixed" ? "none" : cur) + 1) % all.length];
          for (const k of keys()) state[part]["lfo_" + k] = next; changed(); } });
      lbl.insertBefore(chip, val);
      syncs.push(() => { const l = lfoNow(); setText(chip, l === "none" ? "〰️ no LFO" : l === "mixed" ? "〰️ mixed LFOs" : "〰️ LFO " + l); setPressed(chip, l !== "none"); });
    }
    syncs.push(() => {
      const v = own(), mixed = keys().some(k => (state[part][k] ?? hi) !== v);
      input.value = v; val.textContent = mixed ? `${v} · mixed` : v;
      box.classList.toggle("mixed", mixed);
      box.title = mixed ? "Some parts were moved on their own; move this to set them all again" : "";
    });
  } else if (kind === "nudge") {
    // three chips for the slider above: down by the interval, the interval
    // (a tap goes to the next, a half step up to the octave), up by it
    const [lo, hi, ikey] = rest, iv = () => state[part][ikey] || 12;
    const down = el("button", { title: "down by the interval", onclick: () => set(part, key, Math.max(lo, (state[part][key] || 0) - iv())) }, "➖");
    const ivBtn = el("button", { title: "the interval: tap for the next", onclick: () => set(part, ikey, nextInterval(iv())) });
    const up = el("button", { title: "up by the interval", onclick: () => set(part, key, Math.min(hi, (state[part][key] || 0) + iv())) }, "➕");
    box.append(el("div", { class: "row" }, [down, ivBtn, up]));
    syncs.push(() => { setText(ivBtn, "🎵 " + INTERVALS[iv()]); setText(val, INTERVALS[iv()] + ` (${iv()} half step${iv() === 1 ? "" : "s"})`); });
  } else if (kind === "mixer") {
    box.classList.add("mixctl");   // (its strips carry their own colours)
    mixerBox(box);
  } else if (kind === "transport") {
    transportBox(box);
  } else if (kind === "note") {
    // a line of words that says how things are: the label is a function
    const p = el("p", { class: "now" });
    lbl.remove();
    box.append(p);
    syncs.push(() => { setText(p, label()); });
  } else if (kind === "flip") {
    // one chip that says which way it is now; a tap turns it the other way
    // (ticket 285, ^du_report(5): the bubble themes' on and off looked the same) a flip whose two words are the same says only
    // what it is, and is lit while on, as ♿'s chips are
    const b = el("button", { class: "flip" + (label[0] === label[1] ? " lit" : ""), onclick: () => set(part, key, !state[part][key]) });
    lbl.remove();
    box.append(b);
    syncs.push(() => { setText(b, label[state[part][key] ? 1 : 0]);
                       setPressed(b, !!state[part][key]); });
  } else if (kind === "pair") {
    // two columns side by side, mirrored about the middle: the first part's
    // on the left, each with its name, then its controls, a slider showing
    // when its own part wears the slider's style
    const [otherPart, ctls, styleOf = SLIDER_STYLE] = rest;
    box.classList.add("pair");
    box.replaceChildren(...[[key, label[0]], [otherPart, label[1]]].map(([p, text]) =>
      el("div", { class: "side" }, [el("div", { class: "lbl" }, [text]), ...ctls.map(c => {
        const b = control(p, c, { when: styleOf[c[1]] ? h => h.style === styleOf[c[1]] : null });
        // (the column's name says whose style it is)
        if (c[0] === "choice") b.querySelector(".lbl").remove();
        return b;
      })])));
  } else if (kind === "hold") {
    // (ticket 518) one chip naming what the hand holds now; a tap opens a box over the panel with every choice
    const b = el("button", { class: "hold-chip", onclick: () => holdChipBox(part, key, label) });
    box.append(el("div", { class: "row" }, [b]));
    syncs.push(() => { const v = state[part][key] || "auto", t = (HOLD_CHOICES.find(([x]) => x === v) || HOLD_CHOICES[0])[1]; setText(b, t); });
  } else if (kind === "choice") {
    const row = el("div", { class: "row" });
    // (an option may come with a test of the settings: shown only when it holds, as a room the castle has)
    // (and its text may follow the settings too: a castle room's emoji, its type's)
    const bs = rest[0].map(([v, text, ok]) => {
      const b = el("button", { onclick: () => set(part, key, v) }, typeof text === "function" ? "" : text);
      row.append(b); return [v, b, ok, text];
    });
    box.append(row);
    // (ticket 197: written only when it changes: a write each refresh, for every chip of every door of every room, cost the Background tab most of its 26 ms)
    syncs.push(() => { for (const [v, b, ok, text] of bs) {
      const on = String(state[part][key] === v); if (b.getAttribute("aria-pressed") !== on) b.setAttribute("aria-pressed", on);
      if (ok) { const h = !ok(state[part]); if (b.hidden !== h) b.hidden = h; }
      if (typeof text === "function") { const t = text(state[part]); if (b.textContent !== t) b.textContent = t; } } });
  } else if (kind === "places") {
    // two rows, as the music's: the kind of place (or none), then that
    // kind's places, each in a dashed box with its name; picking a kind goes
    // to its first place, unless the place now is already of that kind
    const kinds = el("div", { class: "row" }), places = el("div", { class: "row" });
    const placeCap = el("div", { class: "cap" });
    const kindBox = el("div", { class: "chipset" }, [el("div", { class: "cap" }, "kind of place"), kinds]);
    const placeBox = el("div", { class: "chipset" }, [placeCap, places]);
    const label = v => (rest[0].find(([x]) => x === v) || [v, v])[1];
    const kindBtns = [["none", "⬜ no background"], ...PLACE_GROUPS.map(([g, text]) => [g, text])].map(([g, text]) => {
      const b = el("button", { onclick: () => {
        if (g === "none") return set(part, key, "none");
        const list = PLACE_GROUPS.find(x => x[0] === g)[2];
        if (!list.includes(state[part][key])) set(part, key, list[0]);
      } }, text);
      kinds.append(b); return [g, b];
    });
    box.append(kindBox, placeBox);
    let shownKind, placeBtns = [];
    syncs.push(() => {
      const now = state[part][key], g = groupOfPlace(now);
      for (const [v, b] of kindBtns) setPressed(b, v === g);
      if (g !== shownKind) {
        shownKind = g;
        const list = g && g !== "none" ? PLACE_GROUPS.find(x => x[0] === g)[2] : [];
        // (ticket 285: "the place is picked from pictures of the places (the picture archive's)", in the designer's full screen)
        placeBtns = list.map(id => [id, el("button", { onclick: () => set(part, key, id) }, [el("img", { class: "place-thumb", src: placeThumb(id), alt: "", loading: "lazy",
                                                                                                 onerror: e => e.target.remove() }), label(id)])]);
        places.replaceChildren(...placeBtns.map(([, b]) => b));
        placeBox.hidden = !placeBtns.length;
        placeCap.textContent = placeBtns.length ? `place choices: ${PLACE_GROUPS.find(x => x[0] === g)[1]}` : "";   // (ticket 461: Matthew's name, was "places")
      }
      for (const [v, b] of placeBtns) setPressed(b, v === now);
    });
  } else if (kind === "color") {
    const row = el("div", { class: "row" });
    const bs = rest[0].map(c => {
      const b = c === "skin" || c === "auto" || c === "off"
        ? el("button", { class: "swatch skin",
                         title: c === "skin" ? "same as skin" : c === "off" ? (key === "horseShoes" ? "no shoes" : "none: the fur shows through")
                              : key === "spot" ? "a warm brown" : key === "beardShade" ? "the hair's color" : key === "robotLights" ? "each robot's own lights"
                              // (ticket 498: with armour on, the horse's blanket on auto is the armour's cloth colour)
                              : key === "horseBlanket" ? "a lighter shade of the halter's, or with armour the armour's colour" : "a lighter shade",
                         onclick: () => set(part, key, c) }, c)
        : key === "color" && part === "face" && SKIN_MARKS[c]
        ? el("button", { class: "swatch", title: SKIN_MARKS[c][1], "aria-label": SKIN_MARKS[c][1],
                         style: `background:${c};font-size:18px;line-height:1`, onclick: () => set(part, key, c) }, SKIN_MARKS[c][0])
        : el("button", { class: "swatch", title: c, style: `background:${c}`, onclick: () => set(part, key, c) });
      row.append(b); return [c, b];
    });
    const pick = el("input", { type: "color", title: "any color",
                               oninput: e => set(part, key, e.target.value) });
    row.append(pick);
    box.append(row);
    syncs.push(() => {
      for (const [c, b] of bs) setPressed(b, state[part][key] === c);
      if (state[part][key].startsWith("#")) pick.value = state[part][key];
    });
  } else if (kind === "place") {
    // The stage in small, its sky and its ground, and the figure a box as
    // tall as it stands, its feet where they are: tap or drag to put the
    // feet there (near the middle, exactly the middle).
    const NS = "http://www.w3.org/2000/svg", W = -2 * SCENE.left, H = SCENE.bottom - SCENE.top;
    const svg = document.createElementNS(NS, "svg");
    svg.setAttribute("viewBox", `${SCENE.left} ${SCENE.top} ${W} ${H}`);
    svg.setAttribute("class", "place");
    svg.innerHTML = `<rect x="${SCENE.left}" y="${SCENE.top}" width="${W}" height="${SCENE.ground - SCENE.top}" fill="#9fd8f5"/>` +
      `<rect x="${SCENE.left}" y="${SCENE.ground}" width="${W}" height="${SCENE.bottom - SCENE.ground}" fill="#e8d39a"/>` +
      `<line x1="0" y1="${SCENE.top}" x2="0" y2="${SCENE.bottom}" stroke="#ffffff" stroke-width="3" stroke-dasharray="12 10" opacity="0.8"/>` +
      `<rect id="fig" rx="14" fill="#7c3aed" fill-opacity="0.75" stroke="#ffffff" stroke-width="5"/>`;
    const fig = svg.querySelector("#fig");
    const at = e => {
      const p = new DOMPoint(e.clientX, e.clientY).matrixTransform(svg.getScreenCTM().inverse()), F = sceneFrame;
      let x = clamp(p.x, F.left, F.right), y = clamp(p.y - F.ground, figYMin(F), F.bottom - F.ground);
      if (Math.abs(x) < W * 0.03) x = 0;
      if (Math.abs(y) < H * 0.03) y = 0;
      state.background.figX = Math.round(x); state.background.figY = Math.round(y); changed();
    };
    let held = false;
    svg.addEventListener("pointerdown", e => { held = true; svg.setPointerCapture(e.pointerId); at(e); });
    svg.addEventListener("pointermove", e => { if (held) at(e); });
    svg.addEventListener("pointerup", () => { held = false; });
    svg.addEventListener("pointercancel", () => { held = false; });
    box.append(svg);
    syncs.push(() => {
      const B = state.background, k = figScale(B) * sceneFig(B.kind);
      const h = Math.max(30, (naturalFrame.ground - naturalFrame.top) * k), w = Math.max(16, h * 0.4);
      const fx = B.figX ?? 0, fy = SCENE.ground + (B.figY ?? 0);
      for (const [n, v] of [["x", f1(fx - w / 2)], ["y", f1(fy - h)], ["width", f1(w)], ["height", f1(h)]]) if (fig.getAttribute(n) !== String(v)) fig.setAttribute(n, v);   // (ticket 379)
      setText(val, !B.figX && !B.figY ? "middle" : "");
    });
  } else if (kind === "pad") {
    // A square to tap or drag: the sun marks where the light comes from,
    // and the shine sits on that side of each eye.
    const NS = "http://www.w3.org/2000/svg";
    const svg = document.createElementNS(NS, "svg");
    svg.setAttribute("viewBox", "-1.2 -1.2 2.4 2.4");
    svg.setAttribute("class", "pad");
    svg.innerHTML = `<rect x="-1.2" y="-1.2" width="2.4" height="2.4" rx="0.2" fill="#87ceeb"/>` +
      `<circle r="0.45" fill="#f5c542" stroke="#333" stroke-width="0.04"/>` +
      `<circle cx="-0.15" cy="-0.08" r="0.07"/><circle cx="0.15" cy="-0.08" r="0.07"/>` +
      `<g id="sun"><circle r="0.16" fill="#fff59d" stroke="#f59e0b" stroke-width="0.05"/></g>`;
    const sun = svg.querySelector("#sun");
    const at = e => {
      const p = new DOMPoint(e.clientX, e.clientY).matrixTransform(svg.getScreenCTM().inverse());
      const c = v => Math.round(Math.max(-1, Math.min(1, v)) * 100) / 100;
      state[part].lightX = c(p.x); state[part].lightY = c(p.y); changed();
    };
    let held = false;
    svg.addEventListener("pointerdown", e => { held = true; svg.setPointerCapture(e.pointerId); at(e); });
    svg.addEventListener("pointermove", e => { if (held) at(e); });
    svg.addEventListener("pointerup", () => { held = false; });
    svg.addEventListener("pointercancel", () => { held = false; });
    box.append(svg);
    syncs.push(() => sun.setAttribute("transform", `translate(${state[part].lightX} ${state[part].lightY})`));
  }
  const when = "when" in opts ? opts.when : SHOWN_WHEN[part + "." + key];
  if (when) syncs.push(() => { setHidden(box, !when(state[part])); });
  return box;
}

// (ticket 456, Matthew, from M's request: "when we have activated a RHS menu panel (e.g., background), and we scroll the RHS
// into the sub-panels -- retain a small band above them saying 'background / sub-panel-1 / ... / sub-panel-N' --- we use
// slashes as separators to give a tree-path into the panel that we hover over"; "to at least see the start of the breadcrumb
// trail ('background') in that fixed band") The band over the panel (#crumbs, kept at the top as the page scrolls): the tab's
// name, then the boxes and groups round what the pointer is over (or what has the keyboard's focus), outermost first, their
// emoji left out; the tab's name alone when the pointer leaves the panel. (ticket 461, Matthew: "bread crumbs look good, but
// they don't go as deep as they could -- for instance, background / the place / kind of place vs background / the place /
// places ('place choices', let's say)") As deep as it goes: every set of chips with a heading (a group, or a part of a row,
// as the place picker's kinds and its choices: a heading "X: Y" said as X) and the row itself, by its name; a heading that
// is hidden, or says only the tab's name, is not said.
const crumbWords = t => String(t || "").replace(/[\p{Extended_Pictographic}\u200D\uFE0F\u20E3]/gu, "").replace(/\s+/g, " ").trim();
function crumbsShow(target) {
  const band = document.getElementById("crumbs"), panel = document.getElementById("panel");
  if (!band || !TABS[tab]) return;
  const inner = [], tabWords = crumbWords(TABS[tab][0]).toLowerCase();
  for (let x = target; x && x !== panel && x.nodeType === 1; x = x.parentElement) {
    const set = x.matches(".chipset");
    const head = x.matches("section.box") ? x.querySelector(":scope > .box-name") : set ? x.querySelector(":scope > .cap")
               : x.matches(".ctl") ? x.querySelector(":scope > .lbl .name") : null;
    // (a header hidden, as one that says only its box's words again is (ticket 392), is not said either)
    let t = head && head.getClientRects().length ? crumbWords(head.textContent) : "";
    if (set && t.includes(":")) t = t.split(":")[0].trim();
    if (t && inner[0] !== t && t.toLowerCase() !== tabWords) inner.unshift(t);
  }
  const text = [tabWords, ...inner].join(" / ");
  if (band.textContent !== text) band.textContent = text;
}
{ const panel = document.getElementById("panel");
  if (panel) { panel.addEventListener("pointerover", e => crumbsShow(e.target)); panel.addEventListener("focusin", e => crumbsShow(e.target));
               panel.addEventListener("pointerleave", () => crumbsShow(null)); } }
function showTab() {
  syncs = []; madeSyncs = []; mixLights = null;
  delete document.body.dataset.lit;   // (ticket 377: no part left lit by a box no longer there)
  const tabs = document.getElementById("tabs"), panel = document.getElementById("panel");
  tabs.replaceChildren(...TABS.map(([name], i) =>
    el("button", { "aria-pressed": i === tab, onclick: () => { tab = i; showTab(); } }, name)));
  const [, part, ctls] = TABS[tab];
  panel.dataset.part = part;
  panel.replaceChildren(...ctls.map(c => control(part, c)));
  if (part === "background") bgLayout(panel);   // (ticket 285)
  else if (part === "advanced") advLayout(panel);   // (ticket 375)
  else boxLayout(panel, TABS[tab][0], ctls);   // (ticket 375: its rows in boxes, as the designers' are)
  tidyGroups(panel);   // (ticket 375: and every group in it named once, a bubble theme drawn)
  // (ticket 375) every tab without a designer of its own is one (tabDesigner); one with its own leaves the generic full screen
  if (part === "music" || part === "world") designerHeads(panel, TABS[tab][0]);   // (ticket 392)
  if (!["music", "world", "background"].includes(part)) tabDesigner(panel, TABS[tab][0]);
  else if (document.body.classList.contains("tab-big")) setTabBig(false);
  placePic();   // (ticket 231: the world designer's view built again, or gone with its tab)
  crumbsShow(null);   // (ticket 456: the band, the tab's name)
  changed();
}
