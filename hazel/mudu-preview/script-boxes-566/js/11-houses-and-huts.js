"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 11 of 26 of the game's script, run in order by index.html.

function treehouseScene(c) {
  const { x0, y0, W, H, k, g, grad, all, land, o, R } = c;
  const n = Math.max(1, Math.min(6, o.houses ?? 4)), ts = (o.treeSize ?? 100) / 100;
  const bridges = o.bridges ?? 2, ladders = o.ladders ?? 3, leaves = o.leaves ?? 60;
  let s = SEP + part("tree-sky", c.F, () => `<defs>${grad("bg-sky", "#5eb6ee", "#e6f6ff", y0, g)}</defs>` + all("url(#bg-sky)") +
                                       land(g - 130 * k, 12, "#a5d48e", 0.3) + land(g - 30 * k, 5, "#79b956", 0.9));
  s += SEP + cloudsSvg(c, 3, 0.9, 110);
  // the forest round the great tree: far pines and round trees on the
  // hills, nearer ones, bushes, and flowers in the grass (an oak, a pine, a
  // tulip and a daisy, each drawn once)
  s += SEP + part("tree-woods", c.F, () => {
    const { x1, y1, P } = c, side = c.side;
    let t = `<defs>${oakDef("bg-woods-oak", 1)}${pineDef("bg-pine", 3)}${FLOWER_DEFS}</defs>`;
    const tree = (i, x, y, sc) => i % 3 === 1 ? use("bg-woods-oak", x, y, sc * (R(i, 211) < 0.5 ? -1 : 1), sc)
                                              : use("bg-pine", x, y, sc, sc, ` color="${["#2f6e3a", "#3a7d44", "#285e33"][i % 3]}"`);
    // (the far row one object, at its middle depth; the stream and the
    // flowers lie flat on the ground, so they come next, under all that
    // stands nearer: the nearer trees, a row each side, and each bush)
    let far = "";
    for (let i = 0; i < 30; i++)   // the far row, small, on the far hills
      far += tree(i, x0 - 30 * k + (W + 60 * k) * i / 29 + 14 * k * (R(i, 210) - 0.5), g - 128 * k + 10 * k * R(i, 212), k * (0.32 + 0.2 * R(i, 213)));
    t += obj("trees, far row", g - 123 * k, far);
    // a stream winding down from the hills, past the tree, out at the front
    const sp = streamAt(c), Lb = [], Rb = [];
    for (const [x, y, hw] of sampled(sp, 30)) { Lb.push(P(x - hw, y)); Rb.push(P(x + hw, y)); }
    t += `<defs><linearGradient id="bg-stream" gradientUnits="userSpaceOnUse" x1="0" y1="${f1(g - 125 * k)}" x2="0" y2="${f1(y1)}">` +
         `<stop offset="0" stop-color="#9fdcf7"/><stop offset="1" stop-color="#2f86c6"/></linearGradient></defs>` +
         `<polygon points="${[...Lb, ...Rb.reverse()].join(" ")}" fill="url(#bg-stream)" stroke="#6f9b4a" stroke-width="${f1(3 * k)}" stroke-linejoin="round"/>`;
    // (ticket 42) its ripples flowing down it, a little faster than the brook's
    t += flowRipples(sp, [...Array(14)].map((_, i) => 0.12 + 0.83 * i / 13), 2.2, hw => { const l = hw * 0.5;
      return `<path d="M${f1(-l)} 0 Q0 ${f1(-l * 0.25)} ${f1(l)} 0" fill="none" stroke="#ffffff" stroke-width="${f1(hw * 0.045)}" opacity="0.7"/>`; });
    return t;
  });
  // flowers in the grass (ticket 261, Matthew: "looks like we are wasting time drawing flowers behind the tree house
  // tree?"): one more than 75% behind the trunk's dark core, at the tree's size now, left out; a part of their own, so
  // the size slider builds only them again
  // (ticket 297: in rows by depth, each an object, the friend among them: depthRows)
  s += SEP + part("tree-flowers", [c.F, ts], () => {
    const { y1 } = c, sp = streamAt(c), z = ts * k, core = [[[-81.6 * z, g + 10 * z], [-48 * z, g - 780 * z], [48 * z, g - 780 * z], [81.6 * z, g + 10 * z]]];
    const items = [];
    for (let i = 0; i < 48; i++) {
      const d = R(i, 220), x = x0 + W * R(i, 221), y = g - 22 * k + (y1 - g + 10 * k) * d, sc = k * (0.8 + 0.7 * d);
      if (inStream(sp, x, y, 8 * k)) continue;
      // (a flower's box: its head, 16 wide, and its stem, 34 tall in all at its scale, standing on its foot)
      items.push({ y, svg: use(i % 2 ? "bg-daisy" : "bg-tulip", x, y, sc, sc, ` color="${FLOWER_COLORS[i % 6]}"`), shape: [[[x - 8 * sc, y - 34 * sc], [x + 8 * sc, y - 34 * sc], [x + 8 * sc, y + sc], [x - 8 * sc, y + sc]]] });
    }
    return depthRows("flowers", seenOnly(items, core, CULL_MOST, true), 12 * k);
  });
  s += SEP + part("tree-near", c.F, () => {
    const { x1, y1, P } = c, side = c.side, sp = streamAt(c);
    const tree = (i, x, y, sc) => i % 3 === 1 ? use("bg-woods-oak", x, y, sc * (R(i, 211) < 0.5 ? -1 : 1), sc)
                                              : use("bg-pine", x, y, sc, sc, ` color="${["#2f6e3a", "#3a7d44", "#285e33"][i % 3]}"`);
    const near = ["", ""], stand = [];
    for (let i = 0; i < 12; i++)   // nearer ones, at the meadow's edge, left and right
      near[side(i) < 0 ? 0 : 1] += tree(i + 1, side(i) * (W * 0.2 + W * 0.32 * R(i, 214)), g - 42 * k + 8 * k * R(i, 215), k * (0.55 + 0.3 * R(i, 216)));
    stand.push(["trees, left", g - 38 * k, near[0]], ["trees, right", g - 38 * k, near[1]]);
    for (let i = 0; i < 16; i++) {  // bushes
      const x = side(i) * (W * 0.12 + W * 0.36 * R(i, 217)), y = g - 18 * k + 50 * k * R(i, 218), r = (16 + 14 * R(i, 219)) * k;
      if (inStream(sp, x, y, r * 1.4)) continue;   // (no bushes in the water)
      stand.push([`bush, ${side(i) < 0 ? "left" : "right"} ${Math.floor(i / 2) + 1}`, y, bushSvg(x, y, r * 1.4, 300 + i)]);
    }
    return byDepth(stand);
  });
  // the tree, its leaves, houses, bridges and ladders: built in the tree's
  // units, so its size slider builds none of them again
  // (the whole group one object, standing where its trunk meets the ground)
  s += obj("tree house", g, OPEN(`<g id="bg-tree" ${tf(0, g, ts * k)}>`) + treeInner(c, false) + CLOSE);
  // (ticket 335, Matthew: "layer bug: I'm on top of the owl tree house that should be 'in front' of me") each house an
  // object of its own, after the tree at the tree's depth, highest first (a lower house covers the one above it), and
  // marked with its platform's height on the screen (data-floor): the friend up on a bridge or a platform is drawn just
  // before the first house lower than them (houseAhead), so the nearer houses below cover them, those level with them
  // or above do not
  const hz = (o.treeHouseSize ?? 100) / 100;
  for (const h of treeHouseParts(o, n, hz))
    s += obj(`tree house ${h.i + 1}`, g, OPEN(`<g data-floor="${f1(g + h.y * ts * k)}" ${tf(0, g, ts * k)}>`) + h.svg + CLOSE);
  // butterflies, fluttering with the quick LFO
  // (at the depth of the ground under the nearest)
  s += obj("butterflies", g - 60 * k, part("treehouse-butterflies", [c.F, sceneLfoKey(o, "fast"), sceneLfoKey(o, "wings")], () => butterfliesSvg(c, [[x0 + W * 0.12, g - 80 * k, 1], [x0 + W * 0.86, g - 60 * k, 1.1]])));
  return s;
}

// a picture on a wall: another scene, drawn at the stage's frame, painted
// back to front as far as its detail says (the sky first), and blurred
// the less detail it has; a picture of this room holds its own pictures
// once, not forever
const PICTURE_KINDS = ["lake", "beach", "livingroom", "forest", "treehouse", "underwater", "house", "witch", "cemetery", "treeroom"];
const SCENE_PARTS = new Map(), TRIMMED = new Map(), STILLS = new Map();
// (ticket 509) A still picture in play, as pixels. On a computer drawing without a graphics card (GitHub's, measured), an SVG
// image is not kept as pixels: whatever moves near it, the browser draws all its shapes again, and the imagination's 36 framed
// pictures, live animations among them, walked at about 48 frames a second on two of three such computers (60 with the
// pictures hidden, or every animation off). Drawn once into a canvas and kept as a WebP, a picture costs what any image does.
// Only in play (the scene's "raster" option): a held picture (the archive) keeps the SVG, and a save or a postcard gets it
// back (unrastered). RASTER_GEN counts the
// batches made, so the scene, which reads it, is drawn again with them.
const RASTERS = new Map(), RASTER_SRC = new Map(), RASTER_JOBS = [];   // (the still picture's SVG -> its pixels, and back)
let RASTER_GEN = 0, rasterBusy = false, rasterMade = false;
function rasterize(key, href, w, h) {
  if (RASTERS.has(key)) return;
  if (RASTERS.size > 120) { RASTERS.clear(); RASTER_SRC.clear(); RASTER_JOBS.length = 0; RASTER_GEN++; }
  RASTERS.set(key, null);   // (being made, or none to be had here: the SVG stays)
  RASTER_JOBS.push([key, href, w, h]);
  // (the newest first to wait: settings that change the pictures queue new ones, and the old ones would hold their SVG text)
  while (RASTER_JOBS.length > 60) RASTERS.delete(RASTER_JOBS.shift()[0]);
  rasterNext();
}
// one at a time, in the page's idle moments, so walking in does not stall while 36 are made
function rasterNext() {
  if (rasterBusy || !RASTER_JOBS.length) return;
  rasterBusy = true;
  const idle = window.requestIdleCallback || (f => setTimeout(f, 30));
  idle(deadline => {
    if (deadline && deadline.timeRemaining() < 8) { rasterBusy = false; rasterNext(); return; }   // (too little time in this one)
    const [key, href, w, h] = RASTER_JOBS.shift();
    if (RASTERS.get(key) !== null) { rasterBusy = false; rasterNext(); return; }   // (made already, or let go)
    const img = new Image();
    let over = false;
    // (the scene drawn again once the queue is empty, not once a picture: each drawing of it is kept)
    const done = () => { if (over) return; over = true; clearTimeout(guard); rasterBusy = false;
      if (!RASTER_JOBS.length && rasterMade) { rasterMade = false; RASTER_GEN++; setTimeout(() => moved(true), 0); }
      rasterNext(); };
    const guard = setTimeout(done, 8000);   // (a picture that never loads holds up no others)
    img.onload = () => {
      try {
        // (as fine as the picture can be shown: full screen on this screen, at its pixel ratio, a tenth to spare)
        const s = Math.min(6, Math.max(3, 1.1 * (screen.width || 1920) / (-2 * SCENE.left) * (window.devicePixelRatio || 1))), c = document.createElement("canvas");
        c.width = Math.max(1, Math.round(w * s)); c.height = Math.max(1, Math.round(h * s));
        c.getContext("2d").drawImage(img, 0, 0, c.width, c.height);
        // (WebP where the browser makes it; Safari gives PNG for it, which is pixels all the same)
        const url = c.toDataURL("image/webp", 0.92);
        if (/^data:image\/(webp|png)/.test(url)) { RASTERS.set(key, url); RASTER_SRC.set(url, href); rasterMade = true; }
      } catch (e) { }
      done();
    };
    img.onerror = done;
    img.src = href;
  });
}
// (a copy of the picture leaving the page, a save or a postcard: its pictures back as SVG, which print sharp at any size)
function unrastered(root) {
  for (const i of root.querySelectorAll("image[data-raster]")) {
    const svg = RASTER_SRC.get(i.getAttribute("href"));
    if (svg) { i.setAttribute("href", svg); for (const a of ["data-raster", "data-blur", "decoding"]) i.removeAttribute(a); }
  }
  return root;
}
function pictureSvg(kind, detail, o, depth, x, y, w, h, still = false, raster = false) {
  const opts = new Proxy({}, { get: (_, key) => (key === "depth" ? depth : o[key]) });
  const full = partByReads(`picture:${kind}:${depth}`, opts, oo =>
    prefixIds(unmarked(backgroundSvg(kind, SCENE.left, SCENE.top, -SCENE.left, SCENE.bottom, SCENE.ground, oo)), `p${depth}${kind}-`));
  let doc = SCENE_PARTS.get(full);
  if (!doc) {
    doc = new DOMParser().parseFromString(`<svg xmlns="http://www.w3.org/2000/svg">${full}</svg>`, "image/svg+xml").documentElement;
    sortObjects(doc);   // (painted in the place's order: see sceneObjects)
    if (SCENE_PARTS.size > 40) SCENE_PARTS.clear();
    SCENE_PARTS.set(full, doc);
  }
  // its drawings in the order they are painted, into its groups too
  // (kept by scene and detail: another slider in the room trims nothing)
  const trimmed = full + "\u0000" + detail;
  let body = TRIMMED.get(trimmed);
  if (body === undefined) {
    const root = doc.cloneNode(true), drawn = [];
    const walk = el => { for (const e of el.children) if (e.tagName === "g") walk(e); else if (e.tagName !== "defs") drawn.push(e); };
    walk(root);
    for (const e of drawn.slice(2 + Math.round((drawn.length - 2) * detail / 100))) e.remove();
    if (TRIMMED.size > 200) TRIMMED.clear();
    TRIMMED.set(trimmed, body = root.innerHTML);
  }
  // (the picture's own doorways are only paint: the picture as a whole is the way into its place)
  body = body.replaceAll('class="portal"', 'class="painted-portal"');
  const blur = PERF.blur ? (1 - detail / 100) * 14 : 0, fid = `bg-blur-${depth}-${Math.round(blur * 10)}`;   // (ticket 155: blurs off by default)
  const W = -2 * SCENE.left, H = SCENE.bottom - SCENE.top;
  const inner = blur > 0.2 ? `<defs><filter id="${fid}" x="-5%" y="-5%" width="110%" height="110%"><feGaussianBlur stdDeviation="${f1(blur)}"/></filter></defs><g filter="url(#${fid})">${body}</g>` : body;
  // (ticket 154) A still picture: the place drawn by its own code, as
  // always, but held still (its animations taken out) and shown as an
  // image, which the browser draws into pixels once and keeps. The
  // imagination's map holds 22 pictures, 7,700 shapes and 24 blurs; drawn
  // live, any move of the friend made the browser draw them again: 18
  // frames a second at 1920 wide on Matthew's laptop, far less on the black
  // laptop. Small and blurred, their animations hardly showed.
  if (still) {
    const key = `${trimmed}\u0001${f1(w)}x${f1(h)}`;
    let href = STILLS.get(key);
    if (href === undefined) {
      const doc = `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="${f1(w)}" height="${f1(h)}" viewBox="${SCENE.left} ${SCENE.top} ${W} ${H}" preserveAspectRatio="xMidYMid slice">` +
                  inner.replace(/<(animate|animateTransform|animateMotion|set)\b[^>]*?(\/>|>[\s\S]*?<\/\1>)/g, "") + `</svg>`;
      // (only what a data URL in an attribute must not hold as it is)
      href = "data:image/svg+xml;charset=utf-8," + doc.replace(/%/g, "%25").replace(/#/g, "%23").replace(/"/g, "%22").replace(/&/g, "%26");
      if (STILLS.size > 80) STILLS.clear();
      STILLS.set(key, href);
    }
    // (ticket 509: as pixels, once made, kept by the still picture itself: the key above holds its animations' begin times,
    // which move with the music's clock (when it starts, stops or jumps); data-blur keeps ⚙️'s count of soft blurs right;
    // decoding="sync": the picture never drawn for a frame without them)
    if (raster) {
      const px = RASTERS.get(href);
      if (px) return `<image x="${f1(x)}" y="${f1(y)}" width="${f1(w)}" height="${f1(h)}" preserveAspectRatio="xMidYMid slice" data-raster="1" decoding="sync"${inner === body ? "" : ' data-blur="1"'} href="${px}"/>`;
      rasterize(href, href, w, h);
    }
    return `<image x="${f1(x)}" y="${f1(y)}" width="${f1(w)}" height="${f1(h)}" preserveAspectRatio="xMidYMid slice" href="${href}"/>`;
  }
  return `<svg x="${f1(x)}" y="${f1(y)}" width="${f1(w)}" height="${f1(h)}" viewBox="${SCENE.left} ${SCENE.top} ${W} ${H}" preserveAspectRatio="xMidYMid slice">` + inner + `</svg>`;
}
// a framed picture, its middle at (x, y), w wide
const framedSvg = (kind, detail, o, depth, x, y, w, still = false, raster = false) => {
  const h = w * 0.74, fr = w * 0.07, mat = w * 0.04;
  return `<g class="portal" data-to="${kind}"><rect x="${f1(x - w / 2 + 4)}" y="${f1(y - h / 2 + 6)}" width="${f1(w)}" height="${f1(h)}" fill="#000000" opacity="0.18"/>` +
         `<rect x="${f1(x - w / 2)}" y="${f1(y - h / 2)}" width="${f1(w)}" height="${f1(h)}" fill="#b8862f" stroke="#7a5518" stroke-width="2"/>` +
         `<rect x="${f1(x - w / 2 + fr)}" y="${f1(y - h / 2 + fr)}" width="${f1(w - 2 * fr)}" height="${f1(h - 2 * fr)}" fill="#f7f2e6"/>` +
         pictureSvg(kind, detail, o, depth, x - w / 2 + fr + mat, y - h / 2 + fr + mat, w - 2 * fr - 2 * mat, h - 2 * fr - 2 * mat, still, raster) + `</g>`;
};
// (each picture's scene is a choice of its own: pic1, pic2, ...)
const PICTURE_CHOICES = [["forest", "🌲 forest"], ["underwater", "🐠 underwater"], ["beach", "🏖️ beach"], ["witch", "🧙 witch's woods"],
  ["cemetery", "🌿 cemetery"], ["lake", "🦆 lake"], ["house", "🏠 house and yard"], ["treehouse", "🌳 tree house"],
  ["treeroom", "🪜 inside the tree house"], ["livingroom", "🛋️ living room"], ["witchroom", "🧙 inside the witch's hut"],
  ["clearing", "🌲 forest clearing"], ["mountains", "⛰️ mountain trail"], ["waterfall", "💦 jungle waterfall"], ["campsite", "🏕️ camp site"]];
const picturesSvg = (o, here, slots, scale, keyOf = i => "pic" + (i + 1)) => {
  const depth = (o.depth ?? 0) + 1, count = depth > 2 ? 0 : Math.min(slots.length, o.pictures ?? 3), detail = o.picDetail ?? 70;
  return slots.slice(0, count).map((sl, i) => {
    if (!sl) return "";   // (ticket 242: no clear place on the wall for it)
    const [x, y, w] = sl, chosen = o[keyOf(i)], kind = SCENES[chosen] || PICTURE_CHOICES.some(([kd]) => kd === chosen) ? chosen : PICTURE_KINDS[i];
    return framedSvg(kind, detail, o, depth, x, y, w * scale, !PERF.livePictures);   // (ticket 158: held still unless moving pictures are on)
  }).join(SEP);
};

// (ticket 278, Matthew for M: "Please reuse the rendering logic in both places, and color options (or color theme
// options, as in the castle) for couches, rugs, etc.") The living room's couch and rug, shared by the house's living
// room and the grass hut's: each room keeps its own colors (couchColor and rugColors in the house; hutCouchColor and
// hutRugColors in the hut). At the house's first colors they draw as they always did.
const COUCH_COLOR = "#2f6f86";
const COUCH_COLORS = [COUCH_COLOR, "#b8434f", "#5f7f3a", "#6a4c93", "#c98a2a", "#7a5a3a", "#4a4a55"];
// (a couch's four tones from its color: its back, its seat, its arms, its cushions; the first color's, as they were)
const couchShades = col => col === COUCH_COLOR ? ["#2f6f86", "#2a6377", "#337a93", "#3a8aa6"]
  : [col, mix(col, "#000000", 0.12), mix(col, "#ffffff", 0.08), mix(col, "#ffffff", 0.18)];
// (in the couch's own units: 600 wide, its feet at 0; `id` names its part, so each room's is its own)
const couchSvg = (id, col) => part(id + "-couch", [col], () => { const [back, seat, arm, cush] = couchShades(col);
  return `<rect x="-300" y="-6" width="600" height="12" rx="6" fill="#000000" opacity="0.15"/>` +
    [-270, 270].map(x => `<rect x="${x - 8}" y="-20" width="16" height="22" fill="#4a2e1a"/>`).join("") +
    `<rect x="-260" y="-250" width="520" height="160" rx="40" fill="${back}"/>` +
    `<rect x="-250" y="-120" width="500" height="100" rx="18" fill="${seat}"/>` +
    [-1, 1].map(sd => `<rect x="${sd < 0 ? -300 : 230}" y="-170" width="70" height="160" rx="30" fill="${arm}"/>`).join("") +
    [-1, 0, 1].map(i => `<rect x="${-232 + (i + 1) * 156}" y="-130" width="152" height="46" rx="16" fill="${cush}" stroke="${seat}" stroke-width="2"/>`).join(""); });
// a round rug, its rings outside in
const RUG_COLORS = [["blue", "🔵 blue and gold", ["#3d6e8f", "#e8c870", "#c0504d", "#3d6e8f"]], ["straw", "🌾 woven straw", ["#b08a4a", "#e6d29a", "#8a6a2a", "#d8bf7a"]],
  ["rose", "🌸 roses", ["#c2185b", "#f8bbd0", "#7b1fa2", "#f48fb1"]], ["leaf", "🌿 leaves", ["#4a7a3a", "#c5e1a5", "#8d6e63", "#7cb342"]],
  ["sunset", "🌅 sunset", ["#d84315", "#ffb74d", "#6a1b9a", "#ffd54f"]], ["night", "🌙 night sky", ["#283593", "#9fa8da", "#fdd835", "#3949ab"]]];
const RUG_CHOICES = RUG_COLORS.map(([id, label]) => [id, label]);
const rugColorsOf = id => (RUG_COLORS.find(r => r[0] === id) || RUG_COLORS[0])[2];
const rugSvg = (c, cols, dy = 6) => [1, 0.78, 0.56, 0.34].map((u, i) =>
  `<ellipse cx="0" cy="${f1(c.g + dy * c.k)}" rx="${f1(c.W * 0.42 * u)}" ry="${f1(70 * c.k * u)}" fill="${cols[i]}"/>`).join("");

// (ticket 517: the living room's lamp on its little table, its feet at ly; the round room at the top of the tree has it too)
const lampSvg = (c, lx, ly) => { const { k, P, o } = c;
  return `<rect x="${f1(lx - 40 * k)}" y="${f1(ly - 110 * k)}" width="${f1(80 * k)}" height="${f1(10 * k)}" fill="#6b4423"/>` +
    [-32, 32].map(dx => `<line x1="${f1(lx + dx * k)}" y1="${f1(ly - 100 * k)}" x2="${f1(lx + dx * 1.1 * k)}" y2="${f1(ly)}" stroke="#6b4423" stroke-width="${f1(6 * k)}"/>`).join("") +
    `<rect x="${f1(lx - 6 * k)}" y="${f1(ly - 200 * k)}" width="${f1(12 * k)}" height="${f1(90 * k)}" fill="#c9a94a"/>` +
    `<polygon points="${P(lx - 30 * k, ly - 200 * k)} ${P(lx + 30 * k, ly - 200 * k)} ${P(lx + 44 * k, ly - 150 * k)} ${P(lx - 44 * k, ly - 150 * k)}" fill="#f6e7b5" transform="translate(0 ${f1(-50 * k)})"/>` +
    `<ellipse cx="${f1(lx)}" cy="${f1(ly - 190 * k)}" rx="${f1(60 * k)}" ry="${f1(30 * k)}" fill="#fff6c8" opacity="0.35">` +
      sceneLfo(o, "slow", 0.2, "opacity", (v, amp) => (0.35 * (1 + (0.25 + 0.6 * amp) * v)).toFixed(3)) + `</ellipse>`; };
// (ticket 517: the living room's potted plant, its pot's foot at py)
function plantSvg(c, px, py) {
  const { k, P, R } = c;
  let plant = `<path d="M${P(px - 30 * k, py - 60 * k)} H${f1(px + 30 * k)} L${P(px + 22 * k, py)} H${f1(px - 22 * k)} Z" fill="#c0663a"/>`;
  for (let i = 0; i < 9; i++) {
    const a = -Math.PI / 2 + (i - 4) * 0.3, l = (90 + 40 * R(i, 130)) * k;
    plant += `<path d="M${P(px, py - 60 * k)} Q${P(px + Math.cos(a) * l * 0.4 - 20 * k, py - 60 * k + Math.sin(a) * l * 0.6)} ${P(px + Math.cos(a) * l, py - 60 * k + Math.sin(a) * l)} Q${P(px + Math.cos(a) * l * 0.5 + 20 * k, py - 60 * k + Math.sin(a) * l * 0.5)} ${P(px, py - 60 * k)} Z" fill="${i % 2 ? "#2f8a3a" : "#3fa34a"}"/>`;
  }
  return plant;
}
// a living room: a couch in the middle, pillows on it, pictures on the wall
// (ticket 50) the couch: its three cushions' tops, in its own units, across
// -232 to 236 and at 112 above its feet, as livingroomScene draws it
SEATS.livingroom = (c, B) => { const cs = (B.couchSize ?? 100) / 100, k = c.k, base = c.g - 40 * k;
  return [{ name: "couch", x0: -232 * cs * k, x1: 236 * cs * k, y: base - 112 * cs * k, z: base + 1, facing: ["front", "left", "right"] }]; };
function livingroomScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, all, o } = c;
  const cs = (o.couchSize ?? 100) / 100, pillows = o.pillows ?? 4, ps = (o.picSize ?? 100) / 100;
  const floor = g - 120 * k;
  // (the room's part is two: its walls and floor, and what stands on the
  // floor, which comes after the window and the pictures on the wall)
  const [room, things] = part("living-room", [c.F, sceneLfoKey(o, "fast"), sceneLfoKey(o, "slow"), o.rugColors ?? "blue"], () => {
    let t = `<defs><pattern id="bg-paper" patternUnits="userSpaceOnUse" width="${f1(36 * k)}" height="${f1(36 * k)}">` +
            `<rect width="${f1(36 * k)}" height="${f1(36 * k)}" fill="#efe1c6"/><rect width="${f1(14 * k)}" height="${f1(36 * k)}" fill="#e7d5b4"/>` +
            `<circle cx="${f1(25 * k)}" cy="${f1(18 * k)}" r="${f1(2.5 * k)}" fill="#d9b98a"/></pattern></defs>` + all("url(#bg-paper)");
    // the floor: boards, a baseboard, a round rug
    t += `<rect x="${f1(x0 - pad)}" y="${f1(floor)}" width="${f1(W + 2 * pad)}" height="${f1(y1 - floor + pad)}" fill="#a8743f"/>`;
    for (let i = -14; i <= 14; i++)
      t += `<line x1="${f1(i * W / 22)}" y1="${f1(floor)}" x2="${f1(i * W / 7)}" y2="${f1(y1 + pad)}" stroke="#8a5d30" stroke-width="${f1(1.5 * k)}"/>`;
    t += `<rect x="${f1(x0 - pad)}" y="${f1(floor - 14 * k)}" width="${f1(W + 2 * pad)}" height="${f1(16 * k)}" fill="#ffffff" stroke="#d8cdb8" stroke-width="${f1(1.5 * k)}"/>` +
         floorObj("rug", rugSvg(c, rugColorsOf(o.rugColors ?? "blue")));   // (ticket 284: the rug, the wall clock, the window and its curtains: wall and floor objects)
    // a lamp on a little table on the right, a plant on the left
    // (each an object, where it stands on the floor)
    const lx = x1 - W * 0.1, ly = floor + 30 * k;
    const lamp = obj("lamp", ly, lampSvg(c, lx, ly));
    // a little wall clock high on the right (above the pictures, as the small one on the left is), its pendulum swinging with the quick LFO
    const mClock = t.length;
    { const cx = x1 - W * 0.217, cy = y0 + H * 0.075, r = 15 * k;
      t += `<rect x="${f1(cx - r - 6 * k)}" y="${f1(cy - r - 8 * k)}" width="${f1(2 * r + 12 * k)}" height="${f1(2 * r + 50 * k)}" rx="${f1(8 * k)}" fill="#7a4a26" stroke="#4a2c14" stroke-width="${f1(2 * k)}"/>` +
           `<path d="M${P(cx - r - 10 * k, cy - r - 8 * k)} L${P(cx, cy - r - 30 * k)} L${P(cx + r + 10 * k, cy - r - 8 * k)} Z" fill="#6b4423"/>` +
           `<rect x="${f1(cx - r + 2 * k)}" y="${f1(cy + r + 6 * k)}" width="${f1(2 * r - 4 * k)}" height="${f1(34 * k)}" rx="${f1(4 * k)}" fill="#3a2414"/>` +
           `<g>${sceneLfo(o, "fast", 0, "transform", (v, amp) => `${f1((10 + 18 * amp) * v)} ${f1(cx)} ${f1(cy + r + 8 * k)}`,
             `<animateTransform attributeName="transform" type="rotate" values="-14 ${f1(cx)} ${f1(cy + r + 8 * k)};14 ${f1(cx)} ${f1(cy + r + 8 * k)};-14 ${f1(cx)} ${f1(cy + r + 8 * k)}" dur="1.2s" calcMode="spline" keySplines="0.45 0 0.55 1;0.45 0 0.55 1" repeatCount="indefinite"/>`)}` +
           `<line x1="${f1(cx)}" y1="${f1(cy + r + 8 * k)}" x2="${f1(cx)}" y2="${f1(cy + r + 30 * k)}" stroke="#c9a227" stroke-width="${f1(2 * k)}"/><circle cx="${f1(cx)}" cy="${f1(cy + r + 32 * k)}" r="${f1(5.5 * k)}" fill="#e8b923" stroke="#9a7a12" stroke-width="${f1(1.2 * k)}"/></g>` +
           `<circle cx="${f1(cx)}" cy="${f1(cy)}" r="${f1(r)}" fill="#fff8e7" stroke="#c9a227" stroke-width="${f1(2.5 * k)}"/>` +
           [...Array(12)].map((_, j) => { const a = j * Math.PI / 6; return `<circle cx="${f1(cx + Math.cos(a) * r * 0.78)}" cy="${f1(cy + Math.sin(a) * r * 0.78)}" r="${f1(1.3 * k)}" fill="#3a2414"/>`; }).join("") +
           `<line x1="${f1(cx)}" y1="${f1(cy)}" x2="${f1(cx)}" y2="${f1(cy - r * 0.6)}" stroke="#3a2414" stroke-width="${f1(2 * k)}" stroke-linecap="round"/>` +
           `<line x1="${f1(cx)}" y1="${f1(cy)}" x2="${f1(cx + r * 0.45)}" y2="${f1(cy + r * 0.2)}" stroke="#3a2414" stroke-width="${f1(2 * k)}" stroke-linecap="round"/>`; }
    t = wallSince(t, mClock, "wall clock");
    const px = x0 + W * 0.08, py = floor + 50 * k;
    const plant = plantSvg(c, px, py);
    return [t, lamp + obj("plant", py, plant)];
  });
  let s = SEP + room;
  // the window above the couch, and what is outside it: a scene of its
  // own, at its own detail (a window in a picture of the room shows it too,
  // once, not forever)
  const wx = 170 * k, wy0 = y0 + H * 0.05, wy1 = y0 + H * 0.36, depth = (o.depth ?? 0) + 1;
  if (depth <= 2) {
    const view = PICTURE_CHOICES.some(([kd]) => kd === o.view) ? o.view : "lake";
    s += SEP + `<g class="portal" data-to="${view}">` + pictureSvg(view, o.viewDetail ?? 80, o, depth, -wx, wy0, 2 * wx, wy1 - wy0) + `</g>`;
  }
  s += wallObj("window and curtains", part("living-window", c.F, () => {
    const fw = 10 * k;
    let t = `<rect x="${f1(-wx)}" y="${f1(wy0)}" width="${f1(2 * wx)}" height="${f1(wy1 - wy0)}" fill="#ffffff" opacity="0.06"/>` +
            `<path d="M${P(-wx * 0.7, wy0)} L${P(-wx * 0.2, wy0)} L${P(-wx * 0.6, wy1)} L${P(-wx * 0.95, wy1)} Z" fill="#ffffff" opacity="0.1"/>` +
            `<rect x="${f1(-wx)}" y="${f1(wy0)}" width="${f1(2 * wx)}" height="${f1(wy1 - wy0)}" fill="none" stroke="#ffffff" stroke-width="${f1(fw)}"/>` +
            `<path d="M${P(-wx / 3, wy0)} V${f1(wy1)} M${P(wx / 3, wy0)} V${f1(wy1)} M${P(-wx, (wy0 + wy1) / 2)} H${f1(wx)}" stroke="#ffffff" stroke-width="${f1(fw * 0.6)}"/>` +
            `<rect x="${f1(-wx - 14 * k)}" y="${f1(wy1)}" width="${f1(2 * wx + 28 * k)}" height="${f1(9 * k)}" fill="#f4efe6" stroke="#d8cdb8" stroke-width="${f1(k)}"/>`;
    // curtains, tied back, and their rod
    for (const sd of [-1, 1]) {
      const ex = sd * (wx + 34 * k), ix = sd * (wx - 26 * k), tie = wy0 + (wy1 - wy0) * 0.62;
      t += `<path d="M${P(ex, wy0 - 16 * k)} L${P(ix, wy0 - 16 * k)} Q${P(ix + sd * 10 * k, tie - 30 * k)} ${P(sd * (wx + 6 * k), tie)} ` +
           `Q${P(ix + sd * 4 * k, tie + 40 * k)} ${P(sd * (wx - 4 * k), wy1 + 50 * k)} L${P(ex, wy1 + 50 * k)} Z" fill="#b8434f"/>` +
           `<path d="M${P(sd * (wx + 6 * k), tie)} L${P(ex, tie + 4 * k)}" stroke="#e8c870" stroke-width="${f1(4 * k)}"/>`;
    }
    return `<g pointer-events="none">` + t + `<rect x="${f1(-wx - 50 * k)}" y="${f1(wy0 - 22 * k)}" width="${f1(2 * wx + 100 * k)}" height="${f1(7 * k)}" rx="${f1(3 * k)}" fill="#6b4423"/></g>`;
  }));
  // the pictures, either side of the window
  s += SEP + picturesSvg(o, "livingroom", [[-255, y0 + H * 0.27, 210 * k], [255, y0 + H * 0.27, 210 * k], [-230, y0 + H * 0.07, 95 * k],
                                     [230, y0 + H * 0.07, 95 * k], [-350, y0 + H * 0.07, 80 * k], [350, y0 + H * 0.07, 80 * k]], ps);
  // the lamp and the plant, in front of the wall
  s += SEP + things;
  // the couch and its pillows, in the couch's own units
  // (the whole group one object, standing on its feet)
  s += obj("couch", g - 40 * k, OPEN(`<g id="bg-couch" ${tf(0, g - 40 * k, cs * k)}>`) + couchSvg("living", o.couchColor ?? COUCH_COLOR) +
    SEP + part("living-pillows", [pillows], () => `<defs><g id="bg-pillow"><path d="M-36 -34 Q0 -44 36 -34 Q44 0 36 34 Q0 44 -36 34 Q-44 0 -36 -34 Z" fill="currentColor" stroke="#00000033" stroke-width="2"/>` +
      `<circle cx="0" cy="0" r="5" fill="#00000022"/></g></defs>` +
      [...Array(pillows)].map((_, i) => use("bg-pillow", -195 + (i % 5) * 97 + (i >= 5 ? 48 : 0), -165 + (i >= 5 ? 22 : 0), 1, 1, ` color="${["#f2c12e", "#e85d75", "#ffffff", "#8bc34a", "#ff8c42", "#9b59b6", "#00b894", "#e53935"][i % 8]}"`)).join("")) +
    CLOSE);
  // (the way toward us, out to the house and yard at first, Matthew 2026-10-17: since ticket 233 a nav arrow, not
  // drawn here: insideWays, 25-world.js)
  return s;
}

// inside a tree house: a real room under a sloped roof, a window onto the
// leaves, pictures on the plank wall, and the things that room keeps
// the colors a house of each colored kind can take
const HOUSE_COLOR_CTLS = [
  ["catFur", "🐱 cat's fur", ["#1f1f22", "#6f6f7d", "#e8862a", "#6b4226", "#ffffff", "#8b5cf6"]],
  ["catWhite", "🐱 tuxedo white (muzzle and bib)", ["#ffffff", "off", "#f7ead8", "#ffd1dc", "#c0c0c0", "#1f1f22"]],
  ["catEyes", "🐱 cat's eyes", ["#c6d43a", "#e0a800", "#3b82f6", "#22c55e", "#f28c28"]],
  ["fancyWalls", "🎀 walls", ["#d9c3e6", "#bcd7ea", "#f7d6e0", "#fff1b8", "#c8e6c9", "#ffffff"]],
  ["fancyRoof", "🎀 roof", ["#6a4c93", "#2e5a88", "#b03a5b", "#3f7f3a", "#5a5a66", "#e8762c"]],
  ["fancyTrim", "🎀 trim", ["#ffffff", "#f2c12e", "#f7a8c8", "#bfe3f5", "#1f1f22"]],
  ["craftWalls", "🔨 walls", ["#9aa982", "#c9a66b", "#8a9bb0", "#b5674d", "#d8cfb8", "#6f7f5a"]],
  ["craftRoof", "🔨 roof", ["#5a3e2b", "#3a3a40", "#6b2f2a", "#2f4a3a", "#7a6a55"]],
  ["craftTrim", "🔨 trim (posts, window)", ["#f4efe6", "#c9b27a", "#2f4a3a", "#7a2f2a", "#1f1f22"]],
  ["craftGable", "🔨 gable", ["#c9b27a", "#f4efe6", "#8a9bb0", "#b5674d", "#5a3e2b"]],
];
const ROOM_CHOICES = [["bunks", "🛏️ bunk room"], ["kitchen", "🍳 kitchen"], ["books", "📚 reading nook"], ["lookout", "🔭 lookout"],
                      ["art", "🎨 art studio"], ["music", "🥁 music room"]];
const HOUSE_ANIMALS = [["fox", "🦊 fox"], ["owl", "🦉 owl"], ["frog", "🐸 frog"], ["bunny", "🐰 bunny"], ["bear", "🐻 bear"], ["cat", "🐱 cat"],
                       ["cabin", "🪵 cabin"], ["fancy", "🎀 fancy house"], ["craftsman", "🔨 craftsman"]];
// each room its own: its wood, its floor, its window, where the tree's
// trunk comes up through it, and where branches poke through its walls
// ([x, y, size, flip, turn], from a hole in the wall or the trunk)
const ROOM_LOOK = {
  art: { planks: ["#e9d8c4", "#e2cfb8", "#efdfcc", "#dcc8ae"], floor: "#a87a4f", window: "wide", trunk: -372,
         branches: [[-70, -150, 1, 1, -8], [120, -178, 0.85, -1, 12]] },
  music: { planks: ["#8a4b3a", "#7d4334", "#94523f", "#83473a"], floor: "#5e3d22", window: "round", trunk: 372,
           branches: [[-80, -172, 0.9, 1, -15], [60, -175, 0.8, -1, 8]] },
  bunks: { planks: ["#b9824c", "#c08a52", "#ad7742", "#b47e48"], floor: "#9a6a3d", window: "square", trunk: -372,
           branches: [[-70, -150, 1, 1, -8], [120, -178, 0.85, -1, 12]] },
  kitchen: { planks: ["#e8dcc8", "#e0d2bb", "#ede3d2", "#d9cbb2"], floor: "#8a5d36", window: "wide", trunk: 260,
             branches: [[-40, -168, 0.9, -1, 0], [235, -60, 0.8, -1, -25]] },
  books: { planks: ["#7a5232", "#6e4a2c", "#845a37", "#734d2f"], floor: "#5e3d22", window: "arch", trunk: 0,
           branches: [[-34, -120, 1.15, -1, -12], [34, -140, 1.15, 1, 8]] },
  lookout: { planks: ["#9aa5ad", "#8e99a1", "#a3adb4", "#949fa7"], floor: "#7d6a55", window: "round", trunk: 372,
             branches: [[-80, -172, 0.9, 1, -15], [345, -90, 1, -1, 10]] },
};
// a trunk of several strands, apart where they come up through the floor,
// crossing as they rise and merging into one above (a fig's, a banyan's)
// (ticket 516: from, how far up the braid the drawing begins, 0 to 1: the round room at the top of the tree shows the braid
// as it is up there, its strands close and crossing, not splayed at its foot; 0, the whole braid, as before)
function braidSvg(cx, top, bottom, w, k, { strands = 3, taper = 0, base = "#2a1a0e", from = 0 } = {}) {
  let t = `<ellipse cx="${f1(cx)}" cy="${f1(bottom)}" rx="${f1(from ? w * 0.9 * (1 - 0.75 * from) : w * 0.9)}" ry="${f1(14 * k)}" fill="${base}"/>`;
  // a darker core behind the strands, so they read as one tree
  const core = [0, 1].map(v => { const u = from ? from + (1 - from) * v : v; return [w * (0.34 - 0.14 * u), bottom + (top - bottom) * v]; });
  t += `<polygon points="${f1(cx - core[0][0])},${f1(core[0][1])} ${f1(cx - core[1][0])},${f1(core[1][1])} ${f1(cx + core[1][0])},${f1(core[1][1])} ${f1(cx + core[0][0])},${f1(core[0][1])}" fill="#3f2716"/>`;
  for (let j = 0; j < strands; j++) {
    const L = [], Rt = [];
    for (let i = 0; i <= 24; i++) {
      const v = i / 24, u = from ? from + (1 - from) * v : v, y = bottom + (top - bottom) * v, spread = 1 - 0.75 * u;
      const x = cx + w * 0.5 * spread * Math.sin(u * 5 + j * 2.1) + (j - (strands - 1) / 2) * w * 0.7 / (strands - 1) * spread;
      const half = w * (0.6 / strands) * (1 + 0.25 * Math.sin(u * 9 + j)) * (1 - taper * u);
      L.push([x - half, y]); Rt.push([x + half, y]);
    }
    const mid = L.map(([x, y], i) => [(x + Rt[i][0]) / 2 + 3 * k * Math.sin(i * 1.7 + j), y]);
    t += `<polygon points="${[...L, ...Rt.reverse()].map(([x, y]) => `${f1(x)},${f1(y)}`).join(" ")}" fill="${["#6e4a2c", "#7a5232", "#5e3d22"][j % 3]}" stroke="#3a2414" stroke-width="${f1(2 * k)}"/>` +
         `<polyline points="${mid.map(([x, y]) => `${f1(x)},${f1(y)}`).join(" ")}" fill="none" stroke="#3a2414" stroke-width="${f1(1.5 * k)}" opacity="0.5"/>`;
  }
  return t;
}
const ROOMS = { bunks: "bunk room", kitchen: "kitchen", books: "reading nook", lookout: "lookout", art: "art studio", music: "music room" };
// where each room's wall is free for pictures (in the stage's units)
const ROOM_PICTURES = { bunks: [[-255, 115, 110], [95, -105, 85]], kitchen: [[95, -125, 80], [-60, -150, 60]],
                        books: [[175, -25, 120], [305, 10, 70]], lookout: [[175, -20, 120], [-335, 140, 80]],
                        art: [[95, -125, 80], [-60, -150, 60]], music: [[-60, -150, 60], [95, -125, 60]] };
// what joins a tree house (0-based) to the others, outside: a rope bridge
// (on its left or right), its ladder down (to the house below, or the
// ground: to -1), the ladder up from the house above; and their emoji
function houseLinks(o, i) {
  const n = Math.max(1, Math.min(6, o.houses ?? 4)), bridges = o.bridges ?? 2, ladders = o.ladders ?? 3, out = [];
  if (i >= n) return out;
  for (const [a, b] of TREE_BRIDGES.filter(([a, b]) => a < n && b < n).slice(0, bridges))
    if (a === i || b === i) { const j = a === i ? b : a; out.push({ to: j, way: TREE_SLOTS[j][0] > TREE_SLOTS[i][0] ? "right" : "left" }); }
  const has = j => j < Math.min(n, ladders), down = j => (LADDER_TO[j] >= 0 && LADDER_TO[j] < n ? LADDER_TO[j] : -1);
  if (has(i)) out.push({ to: down(i), way: "down" });
  for (let j = 0; j < n; j++) if (has(j) && down(j) === i) out.push({ to: j, way: "up" });
  // (ticket 516) the house below the round room at the top of the tree (topHouse): a ladder up to it, whatever 🪜 says (to: "top")
  if (i === topHouse(o) - 1) out.push({ to: "top", way: "up" });
  return out.map(L => ({ ...L, label: L.to === "top" ? "⭕" : L.to < 0 ? "🌳" : houseEmoji(o, L.to + 1) }));
}
const houseEmoji = (o, n) => {
  const kind = o["house" + n], e = HOUSE_ANIMALS.find(([v]) => v === kind) || HOUSE_ANIMALS.find(([v]) => v === TREE_SLOTS[n - 1][2]);
  return e[1].split(" ")[0];
};
// a wooden sign, its words in the middle at (x, y)
const plankSign = (x, y, k, text) => `<rect x="${f1(x - 52 * k)}" y="${f1(y - 18 * k)}" width="${f1(104 * k)}" height="${f1(36 * k)}" rx="${f1(5 * k)}" fill="#a0703f" stroke="#5e3d22" stroke-width="${f1(2.5 * k)}"/>` +
  `<text x="${f1(x)}" y="${f1(y + 7 * k)}" text-anchor="middle" font-size="${f1(20 * k)}" font-weight="bold" fill="#fff8e7" font-family="sans-serif">${text}</text>`;
// (ticket 247) the picture seen through a tree room's door to a bridge, in its opening nb nt ft fb (near edge's foot and
// top, far edge's top and foot): on(u, v), a point u along the door from its near edge and v down it; the far house drawn
// upright at (cx, py), hs its size; the bridge's deck from the threshold (near, left to right on the screen) out to it (far),
// its rails ending at ry. (ticket 331: walkInto measures the same, to walk the friend out along it)
function doorViewGeom(nb, nt, ft, fb) {
  const lerp = (p, q, u) => [p[0] + (q[0] - p[0]) * u, p[1] + (q[1] - p[1]) * u], on = (u, v) => lerp(lerp(nt, ft, u), lerp(nb, fb, u), v);
  const wd = Math.abs(on(1, 0.5)[0] - on(0, 0.5)[0]), top0 = on(0.5, 0)[1], cx = on(0.5, 0.5)[0], py = on(0.5, 0.6)[1];
  const hs = Math.max(0.01, Math.min(0.62 * wd / 170, 0.9 * (py - top0) / 245)), pw = 62 * hs, ry = py - 30 * hs;
  // (left to right on the screen: on the right-hand wall a door's near edge, u = 0, is its right one)
  const lr = (a, b) => a[0] <= b[0] ? [a, b] : [b, a], near = lr(on(0, 1), on(1, 1)), rails = lr(on(0, 0.7), on(1, 0.7));
  return { on, wd, top0, cx, py, hs, pw, ry, near, rails, far: [[cx - pw, py + 14 * hs], [cx + pw, py + 14 * hs]] };
}
function waysSvg(c, floor, links) {
  const { x0, x1, y0, W, H, k, P } = c;
  let t = "";
  const sides = { left: 0, right: 0 };
  // (ticket 247: the houses the bridges lead to, as they look outside; the eyes' drawing, for their heads)
  const houses = treeHouses(c.o, Math.max(1, Math.min(6, c.o.houses ?? 4)), (c.o.treeHouseSize ?? 100) / 100);
  if (links.some(L => L.way === "left" || L.way === "right")) t += `<defs>${CABIN_DEFS}</defs>`;
  for (const L of links) {
    // (ticket 516: "top", the ladder up to the round room at the top of the tree: its own place, its own trap door's settings)
    const top = L.to === "top", go = top ? `data-to="treetop"` : L.to < 0 ? `data-to="treehouse"` : `data-to="treeroom" data-house="${L.to + 1}"`, name = top ? "⭕ top" : L.to < 0 ? "🌳 out" : `${L.label} ${L.to + 1}`;
    // (ticket 334: a shut trap door is no way down, so not a doorway; nor, from the house below, a way up)
    const shutHere = (L.way === "down" && !!c.o["hatchShut" + Math.max(1, Math.min(6, c.o.houses ?? 4, Math.round(c.o.inside ?? 1)))]) || (L.way === "up" && !!c.o[top ? "hatchShutTop" : "hatchShut" + (L.to + 1)]);
    t += shutHere ? `<g data-shut="">` : `<g class="portal" ${go}${L.way === "left" || L.way === "right" ? ` data-door=""` : ""}>`;
    if (L.way === "left" || L.way === "right") {
      // (ticket 223) a door in the side wall, as a castle room's side doors are (roomSpace), the rope bridge going out
      // through it: the first a side near the back corner, the second nearer us (TREE_DOOR_Z), its sign higher
      const sd = L.way === "left" ? -1 : 1, j = Math.min(TREE_DOOR_Z.length - 1, sides[L.way]++), [z0, z1] = TREE_DOOR_Z[j];
      // (ticket 352: TREE_DOOR_H tall, for the default friend to walk through, as through the cabin's door outside)
      const S = roomSpace(c), h = TREE_DOOR_H * k, at = (z, y) => S.at(sd * S.bw, y, z), zm = (z0 + z1) / 2, km = k / (1 + zm);
      const nb = at(z0, floor), nt = at(z0, floor - h), ft = at(z1, floor - h), fb = at(z1, floor);
      const quad = (a, b, d, e) => `M${P(...a)} L${P(...b)} L${P(...d)} L${P(...e)} Z`, lerp = (p, q, u) => [p[0] + (q[0] - p[0]) * u, p[1] + (q[1] - p[1]) * u];
      // (ticket 247, Matthew: "enhance the picture we see out the doors of the tree house that are for bridges, so they
      // resemble the target of the bridge") seen through it as it opens: the house the bridge leads to, as it looks
      // outside (its animal, its colors: houseLook, as the tree draws it), upright, at the far end of the bridge's planks
      // and ropes, which run out to it from the threshold; leaves and sky behind; all inside the doorway
      const vid = `bg-doorview-${L.to + 1}-${sd < 0 ? "l" : "r"}`, tgt = houses.find(u => u.i === L.to);
      const { on, cx, py, hs, pw, ry, near, rails, far } = doorViewGeom(nb, nt, ft, fb), mixP = (u, a, b) => [a[0] + (b[0] - a[0]) * u, a[1] + (b[1] - a[1]) * u];
      // (its clip defined after it: a door's first shape is its opening, which walkInto measures, and one in a clipPath has no box)
      t += `<g data-view="${L.to + 1}" clip-path="url(#${vid})">` +
           `<path d="${quad(nb, nt, ft, fb)}" fill="#a8dcf5"/>` +
           [[0.3, 0.12, 22, "#4fb04f"], [0.7, 0.07, 18, "#3c9a40"], [0.12, 0.3, 16, "#3c9a40"], [0.9, 0.34, 15, "#4fb04f"]].map(([u, v, r, col]) => { const q = on(u, v); return `<circle cx="${f1(q[0])}" cy="${f1(q[1])}" r="${f1(r * km)}" fill="${col}"/>`; }).join("") +
           (tgt ? `<g ${tf(cx, py, hs)}><g color="${tgt.look.walls}">${CABIN_SVG}</g>${tgt.look.head}</g>` : "") +
           // (the bridge: its deck from the threshold out to the house's platform, planks across it closer together farther off)
           `<path d="M${P(...near[0])} L${P(...near[1])} L${P(...far[1])} L${P(...far[0])} Z" fill="#8b5e34"/>` +
           [0.2, 0.38, 0.53, 0.65, 0.75, 0.83, 0.9].map(u => { const a = mixP(u, far[0], near[0]), b = mixP(u, far[1], near[1]); return `<line x1="${f1(a[0])}" y1="${f1(a[1])}" x2="${f1(b[0])}" y2="${f1(b[1])}" stroke="#5e3d22" stroke-width="${f1(1.5 * km)}"/>`; }).join("") +
           [[rails[0], [cx - pw, ry]], [rails[1], [cx + pw, ry]]].map(([a, b]) => `<line x1="${f1(a[0])}" y1="${f1(a[1])}" x2="${f1(b[0])}" y2="${f1(b[1])}" stroke="#d8c49a" stroke-width="${f1(2.5 * km)}"/>`).join("") + `</g>` +
           `<defs><clipPath id="${vid}"><path d="${quad(nb, nt, ft, fb)}"/></clipPath></defs>`;
      // (ticket 216: a plank door, swung open as the passage is taken (walkInto: [data-leaf] to its data-open shape,
      // the same commands); hinged at its far edge, as the castle's are: open, a sliver there)
      const zo = z1 - 0.12 * (z1 - z0), open = quad(at(zo, floor + 4 * k), at(zo, floor - h - 4 * k), ft, fb), knob = on(0.16, 0.52);
      t += `<g data-leaf="" data-open="${open}"><path d="${quad(nb, nt, ft, fb)}" fill="#9a6838" stroke="#5e3d22" stroke-width="${f1(3 * km)}" stroke-linejoin="round"/>` +
           [0.25, 0.5, 0.75].map(u => { const a = on(u, 0.02), b = on(u, 0.98); return `<line x1="${f1(a[0])}" y1="${f1(a[1])}" x2="${f1(b[0])}" y2="${f1(b[1])}" stroke="#6b4423" stroke-width="${f1(2 * km)}"/>`; }).join("") +
           `<ellipse cx="${f1(knob[0])}" cy="${f1(knob[1])}" rx="${f1(4.5 * km)}" ry="${f1(4.5 * km)}" fill="#e2c26a" stroke="#5e3d22" stroke-width="${f1(1.5 * km)}"/></g>` +
           `<path d="${quad(nb, nt, ft, fb)}" fill="none" stroke="#5e3d22" stroke-width="${f1(9 * km)}" stroke-linejoin="round"/>`;
      // (ticket 352: on the door's top, as the doors grew to TREE_DOOR_H: above it, the sign went up under the room's map sign)
      const sk = Math.max(0.85 * k, km), top = on(0.5, 0.1);
      t += plankSign(Math.max(x0 + 56 * sk, Math.min(x1 - 56 * sk, top[0])), top[1] + 4 * sk, sk, `${name} ${sd < 0 ? "⬅️" : "➡️"}`);
    } else if (L.way === "down") {
      // a hatch in the floor, its lid up, the ladder's top poking out
      const hx = (x0 + W * 0.26) * TREE_FIT, hy = floor + 72 * k;   // (ticket 223: toward the middle, with what stands at the back wall)
      // (ticket 252: its opening's shape, this house's setting: the lid up at its far edge, and the hole)
      const me = Math.max(1, Math.min(6, c.o.houses ?? 4, Math.round(c.o.inside ?? 1))), sh = openingShape(c.o["hatchShape" + me]);
      t += hatchWaySvg(c, name, hx, hy, sh, !!c.o["hatchShut" + me], "bg-hatch" + me, hx + 120 * k);
    } else {
      // a ladder up through a hole in the ceiling
      // (ticket 223: toward the middle, with what stands at the back wall; up to the ceiling where it is over the
      // ladder's foot, the square room's ceiling coming toward us from the back wall's top)
      const lx = (x1 - W * 0.4) * TREE_FIT, { top: rt, vy } = roomFrame(c), cy = vy + (rt - vy) * (floor + 14 * k - vy) / (floor - vy);
      // (ticket 252: the opening of the trapdoor above, its house's setting, seen from below)
      t += `<path d="${openingD(openingShape(c.o[top ? "hatchShapeTop" : "hatchShape" + (L.to + 1)]), lx, cy, 40 * k, 12 * k, true)}" fill="${shutHere ? "#7a5230" : "#2a1a0e"}"${shutHere ? ` stroke="#4a3220" stroke-width="${f1(2 * k)}"` : ""}/>` +
           // (ticket 252: data-up, the opening's line: climbing up, the friend is cut off above it; ticket 334: shut above, a ladder to climb no more)
           (shutHere ? `<g>` : `<g data-ladder="1" data-up="${f1(cy)}">`) + [-22, 22].map(e => `<line x1="${f1(lx + e * k)}" y1="${f1(floor + 14 * k)}" x2="${f1(lx + e * 0.85 * k)}" y2="${f1(cy - 4 * k)}" stroke="#a0703f" stroke-width="${f1(5 * k)}"/>`).join("") +
           [...Array(Math.floor((floor + 14 * k - cy) / (30 * k)))].map((_, r) => `<line x1="${f1(lx - 20 * k)}" y1="${f1(floor - r * 30 * k)}" x2="${f1(lx + 20 * k)}" y2="${f1(floor - r * 30 * k)}" stroke="#c08a52" stroke-width="${f1(4 * k)}"/>`).join("") + `</g>` +
           plankSign(lx, y0 + H * 0.24, k, `${name} ⬆️`);
    }
    t += `</g>`;
  }
  return t;
}
// (ticket 516) A hatch in a floor, its middle at (hx, hy), its opening's shape sh: open, its lid up at the hole's far edge, the hole
// (a solid), the ladder's top poking out (climbed down); shut, the lid across it, no way down; its sign at (sx, hy - 30k).
// Moved verbatim out of waysSvg, which the round room at the top of the tree shares: a tree room's hatch, byte for byte.
// What goes round it (the doorway or the shut group) is the caller's
function hatchWaySvg(c, name, hx, hy, sh, shut, lidId, sx) {
  const { k, P } = c;
  // (ticket 334, Matthew: "each tree house with a trap door has a lever on the wall. the default is the open state.
  // switching it leads to the closed state. reuse what you can from the dungeon here") shut: the trap room's plank lid
  // across the opening (lidSvg, its shape), no hole to keep off (no data-hole: walked over), no ladder (no way down);
  // the lid is always drawn, folded away at its hinge while open, for lidSwing to swing when the lever is pulled
  const hinge = hy + k - 19 * k;
  const flat = `<g class="pit-lid" data-pit-i="hatch" data-hinge="${f1(hinge)}"${shut ? "" : ` transform="${lidAt(hinge, 0)}" visibility="hidden"`}>` + lidSvg(hx, hy + k, 76 * k, 19 * k, sh, k, lidId) + `</g>`;
  if (shut) return `<path d="${openingD(sh, hx, hy + k, 76 * k, 19 * k)}" fill="#2a1a0e" stroke="#5e3d22" stroke-width="${f1(4 * k)}"/>` + flat + plankSign(sx, hy - 30 * k, k, `${name} ⬇️`);
  const lid = { square: `<polygon points="${P(hx - 60 * k, hy - 18 * k)} ${P(hx + 60 * k, hy - 18 * k)} ${P(hx + 55 * k, hy - 74 * k)} ${P(hx - 55 * k, hy - 74 * k)}"`,
                circle: `<ellipse cx="${f1(hx)}" cy="${f1(hy - 46 * k)}" rx="${f1(60 * k)}" ry="${f1(28 * k)}"`,
                triangle: `<polygon points="${P(hx - 60 * k, hy - 18 * k)} ${P(hx + 60 * k, hy - 18 * k)} ${P(hx, hy - 78 * k)}"` }[sh];
  return lid + ` fill="#8b5e34" stroke="#5e3d22" stroke-width="${f1(3 * k)}"/>` +
       // (ticket 237, Matthew: "collision detection should avoid walking over the trap doors in the floor of the tree
       // houses") its hole, on the floor, a solid (data-hole: across, and back to front; solidsHere)
       `<path data-hole="${f1(hx - 70 * k)} ${f1(hx + 70 * k)} ${f1(hy - 18 * k)} ${f1(hy + 20 * k)}" d="${openingD(sh, hx, hy + k, 76 * k, 19 * k)}" fill="#2a1a0e" stroke="#5e3d22" stroke-width="${f1(4 * k)}"/>` +
       // (ticket 199, Matthew: "some ladders don't animate (ones going down trap doors in tree house) ... let's make them all
       // animate") the ladder a ladder too, climbed down: data-down, the hole's near edge, below which the friend goes out of sight
       `<g data-ladder="1" data-down="${f1(hy + 20 * k)}">` +
       [-22, 22].map(e => `<line x1="${f1(hx + e * k)}" y1="${f1(hy + 14 * k)}" x2="${f1(hx + e * 0.9 * k)}" y2="${f1(hy - 50 * k)}" stroke="#a0703f" stroke-width="${f1(5 * k)}"/>`).join("") +
       [-36, -14, 8].map(dy => `<line x1="${f1(hx - 21 * k)}" y1="${f1(hy + dy * k)}" x2="${f1(hx + 21 * k)}" y2="${f1(hy + dy * k)}" stroke="#c08a52" stroke-width="${f1(4 * k)}"/>`).join("") + `</g>` +
       flat + plankSign(sx, hy - 30 * k, k, `${name} ⬇️`);
}
// (ticket 334) the trap door's lever: on the back wall above the hatch, the trap room's (leverSvg, its shine; data-lever "H",
// pulled as the trap room's are: roomLeverPull), its handle up while the trap door is open, pulled down to shut it
function hatchLeverSvg(c, floor, open) {
  const { x0, W, k } = c, hx = (x0 + W * 0.26) * TREE_FIT, px = hx + 64 * k, py = floor - 112 * k, a = (open ? 1 : -1) * LEVER_TILT;
  return `<g data-lever="H" style="cursor: pointer">` + leverSvg(px - 13 * k, py - 20 * k, px, py, px, py - 44.4 * k, k,
    s => `<g class="lever-arm" data-pivot="${f1(px)} ${f1(py)}" transform="rotate(${a} ${f1(px)} ${f1(py)})">${s}</g>`, undefined, leverShine(c.o)) + `</g>`;
}
// (ticket 50) a tree house room's seats, by which room it is (picked as
// treeroomScene picks it): the bunk room's bottom bunk, the top of its
// mattress (at floor - 170) between the posts at its ends, facing us; the
// reading nook's beanbag, across the top of it (its peak at floor - 60),
// sunk 10 into it, as one sinks into a beanbag
// (ticket 223, Matthew: "let's redesign the rooms to feature square interior walls, like other rooms from the castle,
// to accommodate more doors") The tree house rooms are square rooms, as the castle's are (roomFrame, roomSpace): a back
// wall, side walls going off toward us, a ceiling of rafters and a floor of boards in perspective, and the doors to the
// rope bridges on the side walls, at two depths a side (TREE_DOOR_Z), never behind the furniture. What stands at the
// back wall (the window, the pictures, the trunk and its limbs, the furniture and its things) is drawn TREE_FIT of its
// size, toward the middle, about the back wall's foot, to fit between the side walls: treeFitText fits a part's
// drawing so (inside each object, its depth with it, so the objects stay the scene's own: sceneObjects passes over an
// object inside a transformed group), and the seats with it (SEATS.treeroom).
// (ticket 352, Matthew: "we crawl when we run out of tree houses, but on the exterior view, we are tall enough to go into the
// doors") a side door to a bridge, TREE_DOOR_H of the room's units tall (172 till 0.0.302, which the default friend crawled
// through: 1.27 of it), so the default walks through it as through the cabin's door outside (0.9 of each); taller ones crawl
const TREE_FIT = 0.72, TREE_DOOR_Z = [[-0.15, -0.03], [-0.32, -0.2]], TREE_DOOR_H = 246;
// (ticket 284) how far from the trunk's middle what hangs on the wall beside it stops: its braid's half width and a little
const TREE_CLEAR = 100;
function treeFitText(text, floor) {
  const TF = `transform="translate(0 ${f1(floor)}) scale(${TREE_FIT}) translate(0 ${f1(-floor)})"`;
  let out = "", inObj = false;
  for (const seg of text.split(SEP)) {
    if (!seg) continue;
    // (ticket 284: a wall or floor thing's tag carries its data-layer too, kept; its depth, back, as it is)
    const m = seg.match(/^<g data-obj="([^"]*)" data-z="([^"]*)"( data-layer="[^"]*")?>$/);
    if (m) { const z = Number(m[2]); out += OPEN(`<g data-obj="${m[1]}" data-z="${Number.isFinite(z) ? f1(floor + (z - floor) * TREE_FIT) : m[2]}"${m[3] || ""}>`) + `<g ${TF}>`; inObj = true; }
    else if (seg === "</g>" && inObj) { out += "</g>" + CLOSE; inObj = false; }
    else out += inObj ? seg : SEP + `<g ${TF}>${seg}</g>` + SEP;
  }
  return out;
}
SEATS.treeroom = (c, B) => {
  const { x0, x1, g, W, k } = c, inside = Math.max(1, Math.min(6, B.houses ?? 4, Math.round(B.inside ?? 1))), chosen = B["room" + inside];
  const room = ROOMS[chosen] ? chosen : HOUSE_ROOMS[inside - 1], floor = g - 110 * k, f = TREE_FIT;
  // (no seat on the bottom bunk: at their size there, a sitter's head went
  // through the top bunk)
  // (ticket 223: where the beanbag is drawn, TREE_FIT toward the middle about the back wall's foot)
  // (ticket 299: the bottom bunk to lie on, its pillow at our left; the furniture drawn TREE_FIT about the floor)
  if (room === "bunks") { const bx = (x1 - W * 0.2) * f, w = 210 * k * f;
    return [{ name: "bunk beds", x0: bx - w / 2, x1: bx + w / 2, y: floor - 174 * k * f, z: floor + 10 * k * f + 1, facing: ["front"], lie: true, head: "left" }]; }
  if (room === "books") { const gx = (x0 + W * 0.2) * f;
    return [{ name: "beanbag", x0: gx - 36 * k * f, x1: gx + 36 * k * f, y: floor - 50 * k * f, z: floor + 60 * k * f + 1, facing: ["front", "left", "right"] }]; }
  return [];
};
// (ticket 517: the bunk beds, their middle at bx, standing on floor, and their ladder; the toy chest at chestX; things toys
// on them both: [the bunks, the chest])
function bunkBedsSvg(c, bx, floor, things, chestX) {
  const { k } = c, top = floor - 330 * k, mid = floor - 170 * k, w = 210 * k;
  // (the bunks and their ladder an object, standing on their posts, floor + 10k; the
  // chest another; each toy is drawn with the one it sits on)
  let bed = [-1, 1].map(sd => `<rect x="${f1(bx + sd * w / 2 - 7 * k)}" y="${f1(top - 20 * k)}" width="${f1(14 * k)}" height="${f1(floor + 30 * k - top)}" fill="#6e4a2c"/>`).join("");
  for (const [y, q] of [[top, "#3b7dd8"], [mid, "#d64545"]])
    bed += `<rect x="${f1(bx - w / 2)}" y="${f1(y)}" width="${f1(w)}" height="${f1(22 * k)}" fill="#f3efe6"/>` +
         `<rect x="${f1(bx - w / 2 + 50 * k)}" y="${f1(y - 4 * k)}" width="${f1(w - 50 * k)}" height="${f1(30 * k)}" fill="${q}"/>` +
         [0, 1, 2, 3].map(j => `<rect x="${f1(bx - w / 2 + 60 * k + j * 36 * k)}" y="${f1(y + 4 * k)}" width="${f1(14 * k)}" height="${f1(14 * k)}" fill="#ffffff" opacity="0.5"/>`).join("") +
         `<rect x="${f1(bx - w / 2 + 6 * k)}" y="${f1(y - 16 * k)}" width="${f1(42 * k)}" height="${f1(18 * k)}" rx="${f1(8 * k)}" fill="#ffffff"/>` +
         `<rect x="${f1(bx - w / 2)}" y="${f1(y + 22 * k)}" width="${f1(w)}" height="${f1(12 * k)}" fill="#6e4a2c"/>`;
  const lx = bx - w / 2 - 34 * k;
  bed += `<line x1="${f1(lx)}" y1="${f1(top)}" x2="${f1(lx)}" y2="${f1(floor + 20 * k)}" stroke="#8b5e34" stroke-width="${f1(6 * k)}"/><line x1="${f1(lx + 26 * k)}" y1="${f1(top)}" x2="${f1(lx + 26 * k)}" y2="${f1(floor + 20 * k)}" stroke="#8b5e34" stroke-width="${f1(6 * k)}"/>` +
       [...Array(8)].map((_, j) => `<line x1="${f1(lx)}" y1="${f1(top + 20 * k + j * 42 * k)}" x2="${f1(lx + 26 * k)}" y2="${f1(top + 20 * k + j * 42 * k)}" stroke="#a0703f" stroke-width="${f1(5 * k)}"/>`).join("");
  // stuffed toys on the beds, the rest on a chest by the window
  let chest = `<rect x="${f1(chestX)}" y="${f1(floor - 70 * k)}" width="${f1(150 * k)}" height="${f1(90 * k)}" fill="#8b5e34" stroke="#5e3d22" stroke-width="${f1(3 * k)}"/>`;
  [...Array(things)].forEach((_, i) => {
    const on = Math.floor(i / 4) % 3, at = [[bx + w / 2 - 30 * k - (i % 4) * 40 * k, top - 2 * k], [bx + w / 2 - 30 * k - (i % 4) * 40 * k, mid - 2 * k], [chestX + 20 * k + (i % 4) * 36 * k, floor - 70 * k]][on];
    const toy = use(`bg-toy-${["teddy", "ball", "blocks", "teddy"][i % 4]}`, at[0], at[1] - Math.floor(i / 12) * 30 * k, k * 0.9);
    if (on < 2) bed += toy; else chest += toy;
  });
  return [bed, chest];
}
// (ticket 517: the tree room kitchen's counter, from cx, its top at top; its feet top + 130k)
const kitchenCounterSvg = (c, cx, top) => { const { W, k } = c;
  return `<rect x="${f1(cx)}" y="${f1(top)}" width="${f1(W * 0.34)}" height="${f1(130 * k)}" fill="#7a4f2a"/>` +
           `<rect x="${f1(cx - 8 * k)}" y="${f1(top - 12 * k)}" width="${f1(W * 0.34 + 16 * k)}" height="${f1(14 * k)}" fill="#d9c7a8"/>` +
           [0, 1, 2].map(j => `<rect x="${f1(cx + 12 * k + j * W * 0.11)}" y="${f1(top + 14 * k)}" width="${f1(W * 0.1)}" height="${f1(96 * k)}" fill="#8b5e34" stroke="#5e3d22" stroke-width="${f1(2 * k)}"/><circle cx="${f1(cx + 12 * k + j * W * 0.11 + W * 0.085)}" cy="${f1(top + 60 * k)}" r="${f1(3.5 * k)}" fill="#f2c12e"/>`).join(""); };
// (ticket 517: its little stove, from sx, a kettle on it; its feet top + 130k)
const stoveSvg = (c, sx, top) => { const { k, P } = c;
  return `<rect x="${f1(sx)}" y="${f1(top - 30 * k)}" width="${f1(110 * k)}" height="${f1(160 * k)}" fill="#3a3a3a"/><rect x="${f1(sx + 14 * k)}" y="${f1(top + 30 * k)}" width="${f1(82 * k)}" height="${f1(60 * k)}" fill="#e8762c" opacity="0.8"/>` +
           `<ellipse cx="${f1(sx + 55 * k)}" cy="${f1(top - 50 * k)}" rx="${f1(30 * k)}" ry="${f1(22 * k)}" fill="#c0392b"/><path d="M${P(sx + 82 * k, top - 54 * k)} L${P(sx + 104 * k, top - 70 * k)}" stroke="#c0392b" stroke-width="${f1(7 * k)}"/>`; };
// (ticket 517: the reading nook's lantern, its cord from top down to the lantern's top at ly, at lx)
const nookLanternSvg = (c, lx, roofAt, ly) => { const { k, P } = c;
  return `<line x1="${f1(lx)}" y1="${f1(roofAt - 4 * k)}" x2="${f1(lx)}" y2="${f1(ly)}" stroke="#333" stroke-width="${f1(2 * k)}"/>` +
          `<path d="M${P(lx - 5 * k, roofAt - 2 * k)} q${f1(5 * k)} ${f1(-7 * k)} ${f1(10 * k)} 0" fill="none" stroke="#333" stroke-width="${f1(2.5 * k)}"/>` +
          `<rect x="${f1(lx - 16 * k)}" y="${f1(ly)}" width="${f1(32 * k)}" height="${f1(42 * k)}" rx="${f1(6 * k)}" fill="#ffe08a" stroke="#333" stroke-width="${f1(3 * k)}"/>`; };
// (ticket 517: the reading nook's beanbag, its middle at gx, its foot at floor + 60k)
const beanbagSvg = (c, gx, floor) => { const { k, P } = c;
  return `<path d="M${P(gx - 90 * k, floor + 60 * k)} Q${P(gx - 100 * k, floor - 70 * k)} ${P(gx, floor - 60 * k)} Q${P(gx + 100 * k, floor - 70 * k)} ${P(gx + 90 * k, floor + 60 * k)} Z" fill="#e85d75"/>`; };
// (ticket 517: the lookout's telescope on its tripod, at tx, its feet at floor + 20k, aimed up to our left)
const telescopeSvg = (c, tx, floor) => { const { k } = c, ty = floor - 150 * k;
  return `<g transform="translate(${f1(2 * tx)} 0) scale(-1 1)">` + [-1, 0, 1].map(sd => `<line x1="${f1(tx)}" y1="${f1(ty)}" x2="${f1(tx + sd * 50 * k)}" y2="${f1(floor + 20 * k)}" stroke="#5e3d22" stroke-width="${f1(6 * k)}"/>`).join("") +
           `<g transform="rotate(-28 ${f1(tx)} ${f1(ty)})"><rect x="${f1(tx - 90 * k)}" y="${f1(ty - 14 * k)}" width="${f1(150 * k)}" height="${f1(28 * k)}" rx="${f1(8 * k)}" fill="#c9a94a" stroke="#7a5518" stroke-width="${f1(3 * k)}"/>` +
           `<rect x="${f1(tx - 110 * k)}" y="${f1(ty - 18 * k)}" width="${f1(26 * k)}" height="${f1(36 * k)}" rx="${f1(5 * k)}" fill="#8a6a1a"/></g></g>`; };
// (ticket 242, Matthew: "let's not place any pictures in the tree house behind any ladders, or other furniture. For
// ladders esp, we can easily click on the picture when we meant the ladder, or vice versa -- best just to separate them
// on the screen more") A tree room's pictures hang where nothing stands in front of them. What stands there is measured
// once the room is drawn (treeBusyMeasure: its furniture and trunk, its ladder up, its hatch, its side doors, and the
// window's frame), once for each room and its settings; then each picture goes to the nearest clear place on the back
// wall to its own (TREE_PIC_CLEAR from anything, TREE_PIC_LADDER from a ladder or hatch), or, with none, is left out.
// Until the room has been measured, its first drawing, no pictures.
const TREE_PIC_CLEAR = 14, TREE_PIC_LADDER = 44;
let treeBusy = { key: null, boxes: null }, treeKeyNow = null;
const treeBusyKey = (o, room, c) => JSON.stringify([o.inside ?? 1, room, o.houses ?? 4, o.ladders ?? 3, o.bridges ?? 2, o.toys ?? 10, o.windowSize ?? 100, c.F]);
function placeTreePictures(slots, busy, c) {
  const { top, bw } = roomFrame(c), floor = c.g - 110 * c.k, pad = TREE_PIC_CLEAR * c.k, placed = [];
  const hit = (r, b, m) => r[0] < b[2] + m && r[2] > b[0] - m && r[1] < b[3] + m && r[3] > b[1] - m;
  return slots.map(([x, y, w]) => {
    const h = w * 0.74, rect = (px, py) => [px - w / 2, py - h / 2, px + w / 2, py + h / 2];
    const clear = (px, py) => { const r = rect(px, py); return r[0] > -bw + pad && r[2] < bw - pad && r[1] > top + pad && r[3] < floor - pad &&
      !busy.some(b => hit(r, b.r, b.m)) && !placed.some(p => hit(r, p, pad)); };
    // (the nearest clear place: along the wall either way, at its own height, then a little higher or lower)
    let best = null;
    for (const dy of [0, -50 * c.k, 50 * c.k, -100 * c.k]) for (let d = 0; d <= 2 * bw; d += 10 * c.k) {
      if (best && d + 2 * Math.abs(dy) >= best.cost) break;
      for (const px of d ? [x - d, x + d] : [x]) if (clear(px, y + dy)) { best = { px, py: y + dy, cost: d + 2 * Math.abs(dy) }; break; }
    }
    if (!best) return null;
    placed.push(rect(best.px, best.py));
    return [best.px, best.py, w];
  });
}
// (after a tree room is drawn: what stands in front of its back wall, in the stage's units; a new room or new settings
// drawn again with its pictures placed)
function treeBusyMeasure() {
  if (state.background.kind !== "treeroom" || !bgShown.node || !treeKeyNow || treeBusy.key === treeKeyNow) return;
  const P = pic.getScreenCTM();
  if (!P) return;
  const Pinv = P.inverse(), k = (SCENE.bottom - SCENE.top) / 700, boxes = [];
  const add = (node, m) => { try { const E = node.getScreenCTM(), q = E && boxThrough(node.getBBox(), Pinv.multiply(E)); if (q && q.width > 0 && q.height > 0) boxes.push({ r: [q.x, q.y, q.x + q.width, q.y + q.height], m: m * k }); } catch (e) { /* (not drawn) */ } };
  // (each piece by itself, every shape, sign and rung: an object's own box is no good, the side doors' spans the room
  // from one side wall to the other, and the trunk's reaches out along its limbs)
  for (const o of bgShown.objs) {
    const ladder = o.name === "hatch" || o.name === "ladder up";
    // (ticket 244: the house's sign, hanging in front of the wall, as a whole: its map made it tall)
    if (o.name === "house sign" || o.name === "hatch lever") add(o.node, TREE_PIC_CLEAR);   // (ticket 334: and the trap door's lever)
    else if (ladder || o.name === "side doors" || STANDING.has(objBase(o.name)))
      for (const sh of o.node.querySelectorAll("rect, path, polygon, polyline, line, ellipse, circle, use, image, text")) add(sh, ladder ? TREE_PIC_LADDER : TREE_PIC_CLEAR);
  }
  const win = bgShown.node.querySelector('.portal[data-to="treehouse"][clip-path]');
  if (win && win.parentNode) add(win.parentNode, TREE_PIC_CLEAR);
  treeBusy = { key: treeKeyNow, boxes, slots: null };
  setTimeout(changed, 0);
}
// The tree rooms' window, in its own units (about ±100), its shape one of round, square, wide, arch: the sky, one of the
// tree's limbs swaying with the slow LFO and its leaf pads fluttering with the quick one (the wind is outdoors), its frame.
// (ticket 516: id keeps its clip and its sky's ids apart from another window's in the same room, off its motion's own way
// in: the round room at the top of the tree has two; the defaults, the square rooms' one, byte for byte)
function roomWindowSvg(o, shape, id = "", off = 0) {
  const clip = { round: `<circle cx="0" cy="0" r="80"/>`, square: `<rect x="-80" y="-70" width="160" height="140"/>`,
                    wide: `<rect x="-120" y="-55" width="240" height="110"/>`, arch: `<path d="M-60 70 V-20 A60 60 0 0 1 60 -20 V70 Z"/>` }[shape];
  return `<defs><clipPath id="bg-win${id}">${clip}</clipPath><linearGradient id="bg-view${id}" x1="0" y1="-80" x2="0" y2="80" gradientUnits="userSpaceOnUse">` +
           `<stop offset="0" stop-color="#7cc7f2"/><stop offset="1" stop-color="#d9f1ff"/></linearGradient></defs>` +
           `<g class="portal" data-to="treehouse" clip-path="url(#bg-win${id})"><rect x="-100" y="-100" width="200" height="200" fill="url(#bg-view${id})"/>` +
           // (outside: one of the tree's limbs, forking into branches, a leaf pad on each tip, as on the
           // tree outside; the limb sways in the wind with the slow LFO, each pad flutters with the quick one;
           // it runs on straight along its own tangent at both ends, so no end shows in even the widest window)
           swaying(o, `<path d="M-170 67 L-110 46 Q-20 14 104 -12 L160 -24" stroke="#6e4a2c" stroke-width="13" fill="none" stroke-linecap="round"/>` +
             // (the branches leave the limb on its curve; the pads sit at their tips and on the limb itself,
             // all within the smallest window's view, about ±70 across and ±45 up and down)
             `<path d="M-12 16 Q-30 0 -40 -18 M38 3 Q50 -10 48 -24" stroke="#6e4a2c" stroke-width="6" fill="none" stroke-linecap="round"/>` +
             [[-40, -20, 0.34, 1, "#4fb04f"], [48, -26, 0.3, 3, "#3c9a40"], [-62, 26, 0.28, 5, "#58b050"], [79, -12, 0.28, 6, "#3c9a40"]].map(([x, y, z, j, col], i) =>
               `<g>${sceneLfo(o, "fast", (i * 0.29 + off) % 1, "transform", (v, amp) => `${f1((2 + 6 * amp) * v)} ${x} ${y}`,
                 `<animateTransform attributeName="transform" type="rotate" values="-2 ${x} ${y};2 ${x} ${y};-2 ${x} ${y}" dur="${(0.8 + 0.15 * i).toFixed(2)}s" repeatCount="indefinite"/>`)}` +
               padSvg(j, x, y, z * (i % 2 ? -1 : 1), z, col) + `</g>`).join(""), -170, 67, 3, 0.2 + off, 6) + `</g>` +
           ({ round: `<circle cx="0" cy="0" r="80" fill="none" stroke="#5e3d22" stroke-width="14"/><path d="M-80 0 H80 M0 -80 V80" stroke="#5e3d22" stroke-width="6"/>`,
              square: `<rect x="-80" y="-70" width="160" height="140" fill="none" stroke="#5e3d22" stroke-width="14"/><path d="M0 -70 V70 M-80 0 H80" stroke="#5e3d22" stroke-width="6"/>` +
                      `<rect x="-96" y="70" width="192" height="12" fill="#5e3d22"/>`,
              wide: `<rect x="-120" y="-55" width="240" height="110" fill="none" stroke="#f4efe6" stroke-width="12"/><path d="M-40 -55 V55 M40 -55 V55 M-120 0 H120" stroke="#f4efe6" stroke-width="6"/>` +
                    `<rect x="-134" y="55" width="268" height="12" fill="#f4efe6"/>`,
              arch: `<path d="M-60 70 V-20 A60 60 0 0 1 60 -20 V70 Z" fill="none" stroke="#3a2414" stroke-width="14"/><path d="M0 -80 V70 M-60 10 H60" stroke="#3a2414" stroke-width="6"/>` }[shape]);
}
function treeroomScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, all, o } = c;
  const inside = Math.max(1, Math.min(6, o.houses ?? 4, Math.round(o.inside ?? 1))), chosen = o["room" + inside];
  const room = ROOMS[chosen] ? chosen : HOUSE_ROOMS[inside - 1], ws = (o.windowSize ?? 100) / 100, things = o.toys ?? 10;
  const floor = g - 110 * k;
  const look = ROOM_LOOK[room];
  // (ticket 223) a square room, as a castle room is: the back wall's planks, the side walls' going off toward us, the
  // ceiling's boards and rafters, and the floor's boards, all toward the eye's height (roomFrame)
  const { top, bw, vy, out } = roomFrame(c);
  let s = SEP + part("room-shell2", [c.F, room], () => {
    const side = mix(look.planks[1], "#000000", 0.28), ceil = mix(look.planks[2], "#000000", 0.42), seam = mix(look.planks[0], "#000000", 0.5);
    let t = all(mix(look.planks[3], "#000000", 0.35));
    // the back wall: planks, a knot here and there
    const n = 14, pw = 2 * bw / n;
    for (let i = 0; i < n; i++) {
      const x = -bw + i * pw;
      t += `<rect x="${f1(x)}" y="${f1(top)}" width="${f1(pw - 3 * k)}" height="${f1(floor - top)}" fill="${look.planks[i % 4]}"/>`;
      if (R(i, 140) < 0.5) t += `<ellipse cx="${f1(x + pw / 2)}" cy="${f1(top + (floor - top) * (0.15 + 0.7 * R(i, 141)))}" rx="${f1(5 * k)}" ry="${f1(8 * k)}" fill="#7a4f2a"/>`;
    }
    // the side walls, their planks upright, nearer ones farther apart
    for (const sd of [-1, 1]) {
      const [ax, ay] = out(sd * bw, top, y0 - pad), [bx, by] = out(sd * bw, floor, y1 + pad);
      t += `<polygon points="${P(sd * bw, top)} ${P(ax, ay)} ${P(sd * (W + pad), y0 - pad)} ${P(sd * (W + pad), y1 + pad)} ${P(bx, by)} ${P(sd * bw, floor)}" fill="${side}"/>`;
      for (let j = 1; j <= 12; j++) {
        const x = sd * (bw + (W * 0.5 + pad - bw) * (j / 12) ** 1.7), f = Math.abs(x) / bw;
        t += `<line x1="${f1(x)}" y1="${f1(vy + (top - vy) * f)}" x2="${f1(x)}" y2="${f1(vy + (floor - vy) * f)}" stroke="${seam}" stroke-width="${f1(2 * k * f)}" opacity="0.7"/>`;
      }
    }
    // the ceiling: boards toward us, rafters across
    t += `<polygon points="${P(-bw, top)} ${P(bw, top)} ${P(...out(bw, top, y0 - pad))} ${P(...out(-bw, top, y0 - pad))}" fill="${ceil}"/>`;
    for (let i = -7; i <= 7; i++) { const [ex, ey] = out(i * bw / 7, top, y0 - pad); t += `<line x1="${f1(i * bw / 7)}" y1="${f1(top)}" x2="${f1(ex)}" y2="${f1(ey)}" stroke="${seam}" stroke-width="${f1(1.5 * k)}" opacity="0.6"/>`; }
    for (const f of [1.12, 1.35, 1.75, 2.5]) { const y = vy + (top - vy) * f; if (y < y0 - pad) break;
      t += `<line x1="${f1(-W - pad)}" y1="${f1(y)}" x2="${f1(W + pad)}" y2="${f1(y)}" stroke="#5e3d22" stroke-width="${f1(10 * k * f)}"/>`; }
    // the floor: boards toward the back wall, and across
    t += `<polygon points="${P(-bw, floor)} ${P(bw, floor)} ${P(...out(bw, floor, y1 + pad))} ${P(...out(-bw, floor, y1 + pad))}" fill="${look.floor}"/>`;
    for (let i = -10; i <= 10; i++) { const [ex, ey] = out(i * bw / 10, floor, y1 + pad); t += `<line x1="${f1(i * bw / 10)}" y1="${f1(floor)}" x2="${f1(ex)}" y2="${f1(ey)}" stroke="#7a4f2a" stroke-width="${f1(2 * k)}"/>`; }
    t += floorRows(c, mix(look.floor, "#000000", 0.3));
    // the corners, and a board along the foot of the back wall
    for (const sd of [-1, 1]) t += `<line x1="${f1(sd * bw)}" y1="${f1(top)}" x2="${f1(sd * bw)}" y2="${f1(floor)}" stroke="${seam}" stroke-width="${f1(4 * k)}"/>`;
    t += `<rect x="${f1(-bw)}" y="${f1(floor - 6 * k)}" width="${f1(2 * bw)}" height="${f1(10 * k)}" fill="#6b4423"/>`;
    return t;
  });
  // the window: its view and its frame, in its own units, sized by its group
  s += OPEN(`<g ${tf((x0 + W * 0.27) * TREE_FIT, floor + (y0 + H * 0.3 - floor) * TREE_FIT, ws * k * TREE_FIT)}>`) + part("room-window", [look.window, sceneLfoKey(o, "fast"), sceneLfoKey(o, "slow")], () => roomWindowSvg(o, look.window)) + CLOSE;
  // the tree, up through the floor and out through the roof; its branches
  // poke through the walls, a few pale leaves on them (it is dim in here)
  // (the tree's big limbs pass through the room, bare and still: leaves grow outside, in the
  // sun, and the wind is out there too; see the window)
  // (the trunk an object, standing where it comes up through the floor, with the limbs
  // that leave it; a limb out of the wall, far from it, is the wall's, drawn first,
  // behind everything: it crosses no trunk, and the ways drawn over it stay over it)
  const limb = ([x, y, sc, fl, a]) => `<use href="#bg-wallbranch" transform="translate(${f1(x)} ${f1(y)}) rotate(${a}) scale(${+(sc * k * fl).toFixed(3)} ${+(sc * k).toFixed(3)})"/>`;
  const onTrunk = ([x]) => Math.abs(x - look.trunk) < 100 * k;
  s += SEP + part("room-tree", [c.F, room], () => treeFitText(
    `<defs><g id="bg-wallbranch"><ellipse cx="0" cy="0" rx="11" ry="15" fill="#2a1a0e"/>` +
    `<path d="M-2 -8 Q60 -24 140 -52 L142 -44 Q64 -10 -2 8 Z" fill="#6e4a2c" stroke="#3a2414" stroke-width="1.5"/>` +
    `</g></defs>` + look.branches.filter(b => !onTrunk(b)).map((b, i) => wallObj(`limb through the wall ${i + 1}`, limb(b))).join("") +
    // (ticket 243, Matthew, with a picture of room 4: "need more segments for this feature of this tree house room (NE
    // corner) -- the tree's box is too big") the braid marked, data-trunk: the trunk's box its braid's alone, in bands
    // up it (standingHere), not its whole drawing's with the limbs that leave it
    obj("tree trunk", floor + 40 * k, `<g data-trunk="">` + braidSvg(look.trunk, y0 - pad, floor + 40 * k, 100 * k, k) + `</g>` + look.branches.filter(onTrunk).map(limb).join("")), floor));
  // pictures on the far side of the wall from the window, each where nothing stands in front of it (ticket 242: the
  // room the friend is in; one seen in a picture on a wall, smaller and drawn inside it, keeps its usual places)
  const slots0 = ROOM_PICTURES[room].map(([x, y, w]) => [x * TREE_FIT, floor + (y - floor) * TREE_FIT, w * k * TREE_FIT]);
  if (o.depth > 0) s += SEP + picturesSvg(o, "treeroom", slots0, 1, i => `r${inside}pic${i + 1}`);
  else {
    treeKeyNow = treeBusyKey(o, room, c);
    // (read through the scene's options, so the scene, cached on what it reads, is built again once measured)
    const busy = o["tree:busy"] === treeKeyNow ? treeBusy.boxes : null;
    // (placed once for the room and its settings, not on every drawing: a walk draws every frame)
    if (busy && !treeBusy.slots) treeBusy.slots = placeTreePictures(slots0, busy, c);
    if (busy) s += SEP + picturesSvg(o, "treeroom", treeBusy.slots, 1, i => `r${inside}pic${i + 1}`);
  }
  // the room's furniture and its things
  s += SEP + part("room-" + room, [c.F, things], () => treeFitText((() => {
    let t = "";
    if (room === "bunks") {
      const bx = x1 - W * 0.2, [bed, chest] = bunkBedsSvg(c, bx, floor, things, x0 + W * 0.12);
      t += `<defs>${toyDefs}</defs>` + obj("bunk beds", floor + 10 * k, bed) + obj("toy chest", floor + 20 * k, chest);
    } else if (room === "kitchen") {
      const cx = x0 + W * 0.05, top = floor - 120 * k;
      // shelves of jars
      // (ticket 284, Matthew: "Move the jars, map, pennants and records clear of the trunk, then make them objects";
      // "always be knolling") on the wall between the stove and the trunk, clear of both: five jars a shelf, shelf under
      // shelf, as many shelves as the jars need (two to four), the column ending TREE_CLEAR short of the trunk's braid;
      // a wall object, behind the friend and all that stands
      const per = 5, rows = Math.max(2, Math.min(4, Math.ceil(things / per))), sw = 28 * k + (per - 1) * W * 0.032 + 20 * k, jx = look.trunk - TREE_CLEAR * k - sw;
      const sy = [...Array(rows)].map((_, j) => y0 + H * (0.36 + 0.107 * j)), mJars = t.length;
      t += sy.map(y => `<rect x="${f1(jx)}" y="${f1(y)}" width="${f1(sw)}" height="${f1(8 * k)}" fill="#6e4a2c"/>`).join("") +
           [...Array(things)].map((_, i) => {
             const row = Math.floor(i / per) % rows, x = jx + 14 * k + (i % per) * W * 0.032, y = sy[row], h = (26 + 14 * R(i, 150)) * k;
             return `<rect x="${f1(x)}" y="${f1(y - h)}" width="${f1(20 * k)}" height="${f1(h)}" rx="${f1(4 * k)}" fill="${["#e8c870", "#c0504d", "#7cb342", "#f2a65a", "#8e6bbf"][i % 5]}" opacity="0.9" stroke="#ffffff" stroke-width="${f1(1.5 * k)}"/>` +
                    `<rect x="${f1(x - 1 * k)}" y="${f1(y - h - 6 * k)}" width="${f1(22 * k)}" height="${f1(7 * k)}" fill="#8b5e34"/>`;
           }).join("");
      t = wallSince(t, mJars, "shelves of jars");
      // (the counter and the stove each an object, both down to top + 130k, floor + 10k)
      t += obj("counter", floor + 10 * k, kitchenCounterSvg(c, cx, top)) +
           // a little stove, a kettle on it
           obj("stove", floor + 10 * k, stoveSvg(c, cx + W * 0.36, top));
      // a table and two stools on the right
      // (an object, its legs down to floor + 84k)
      const tx = x1 - W * 0.18;
      t += obj("table", floor + 84 * k, `<rect x="${f1(tx - 90 * k)}" y="${f1(floor - 40 * k)}" width="${f1(180 * k)}" height="${f1(14 * k)}" fill="#8b5e34"/>` +
           [-70, 70].map(dx => `<rect x="${f1(tx + dx * k - 6 * k)}" y="${f1(floor - 26 * k)}" width="${f1(12 * k)}" height="${f1(110 * k)}" fill="#6e4a2c"/>`).join("") +
           `<circle cx="${f1(tx)}" cy="${f1(floor - 58 * k)}" r="${f1(18 * k)}" fill="#e53935"/>`);
    } else if (room === "books") {
      const bx = x1 - W * 0.36, w = W * 0.32, top = y0 + H * 0.5;
      t += `<rect x="${f1(bx)}" y="${f1(top)}" width="${f1(w)}" height="${f1(floor + 20 * k - top)}" fill="#6e4a2c"/>`;
      const shelves = 4, sh = (floor + 20 * k - top) / shelves;
      for (let j = 0; j < shelves; j++) t += `<rect x="${f1(bx + 8 * k)}" y="${f1(top + j * sh + 8 * k)}" width="${f1(w - 16 * k)}" height="${f1(sh - 14 * k)}" fill="#4a2e1a"/>`;
      let shelf = 0, at = bx + 12 * k;
      t += [...Array(things * 2)].map((_, i) => {
        const bw = (10 + 8 * R(i, 160)) * k, bh = (sh - 24 * k) * (0.7 + 0.28 * R(i, 161));
        if (at + bw > bx + w - 12 * k) { shelf++; at = bx + 12 * k; }
        if (shelf >= shelves) return "";
        const x = at; at += bw + 2 * k;
        return `<rect x="${f1(x)}" y="${f1(top + (shelf + 1) * sh - 6 * k - bh)}" width="${f1(bw)}" height="${f1(bh)}" fill="${["#c0392b", "#2e86de", "#27ae60", "#f2c12e", "#8e44ad", "#e67e22", "#16a085"][i % 7]}"/>`;
      }).join("");
      // a beanbag and a lantern
      // (the lantern hangs on a cord from the roof itself: the roof's edge, at the cord, is
      // where the slope above, from x0 - pad to x0 + 0.3 W, crosses it)
      // (the bookcase and its books an object, standing on the floor at floor + 20k; the
      // beanbag another, at its foot, floor + 60k; the lantern hangs at the wall, from the
      // roof's edge there, a wall object since ticket 284: drawn first, crossing neither)
      const gx = x0 + W * 0.2, lx = x0 + W * 0.1, ly = y0 + H * 0.36 * (1 - W * 0.1 / (W * 0.3)) + 60 * k;
      const roofAt = y0 + H * 0.36 * (1 - (lx - (x0 - pad)) / (x0 + W * 0.3 - (x0 - pad)));
      t = wallObj("lantern", nookLanternSvg(c, lx, roofAt, ly)) +
          obj("bookcase", floor + 20 * k, t) +
          obj("beanbag", floor + 60 * k, beanbagSvg(c, gx, floor));
    } else if (room === "art") {
      // an easel with a child's painting, a table of paint jars, paint on the
      // floor, and drawings pinned up (the things: jars, then drawings)
      const ex = x1 - W * 0.22, top = floor - 270 * k, cols = ["#e53935", "#3b7dd8", "#f2c12e", "#27ae60", "#8e44ad", "#ff8c42"];
      t += floorObj("paint spills", [[-60, 30], [-30, 50], [40, 20], [70, 60], [10, 80]].map(([dx, dy], i) =>
             `<ellipse cx="${f1(dx * k)}" cy="${f1(floor + dy * k)}" rx="${f1(16 * k)}" ry="${f1(6 * k)}" fill="${cols[i]}" opacity="0.8"/>`).join("")) +
           // (the easel an object, at its splayed legs' feet, floor + 30k; the paint on the floor a floor object, ticket 284)
           obj("easel", floor + 30 * k, [-1, 1].map(sd => `<line x1="${f1(ex)}" y1="${f1(top)}" x2="${f1(ex + sd * 60 * k)}" y2="${f1(floor + 30 * k)}" stroke="#8b5e34" stroke-width="${f1(7 * k)}"/>`).join("") +
           `<line x1="${f1(ex)}" y1="${f1(top)}" x2="${f1(ex)}" y2="${f1(floor + 20 * k)}" stroke="#6e4a2c" stroke-width="${f1(6 * k)}"/>` +
           `<rect x="${f1(ex - 80 * k)}" y="${f1(top + 20 * k)}" width="${f1(160 * k)}" height="${f1(120 * k)}" fill="#fffdf6" stroke="#8b5e34" stroke-width="${f1(4 * k)}"/>` +
           `<circle cx="${f1(ex + 45 * k)}" cy="${f1(top + 50 * k)}" r="${f1(14 * k)}" fill="#ffd23f"/>` +
           `<path d="M${P(ex - 78 * k, top + 138 * k)} Q${P(ex, top + 80 * k)} ${P(ex + 78 * k, top + 138 * k)} Z" fill="#7cc25a"/>` +
           `<rect x="${f1(ex - 40 * k)}" y="${f1(top + 82 * k)}" width="${f1(36 * k)}" height="${f1(30 * k)}" fill="#e85d75"/>` +
           `<polygon points="${P(ex - 46 * k, top + 82 * k)} ${P(ex - 22 * k, top + 62 * k)} ${P(ex + 2 * k, top + 82 * k)}" fill="#3b7dd8"/>` +
           `<rect x="${f1(ex - 90 * k)}" y="${f1(top + 140 * k)}" width="${f1(180 * k)}" height="${f1(8 * k)}" fill="#6e4a2c"/>`);
      // (the table and its jars an object, its legs down to ty + 92k)
      const tx = x0 + W * 0.2, ty = floor - 70 * k;
      t += obj("paint table", ty + 92 * k, `<rect x="${f1(tx - 110 * k)}" y="${f1(ty)}" width="${f1(220 * k)}" height="${f1(12 * k)}" fill="#8b5e34"/>` +
           [-95, 95].map(dx => `<rect x="${f1(tx + dx * k - 5 * k)}" y="${f1(ty + 12 * k)}" width="${f1(10 * k)}" height="${f1(80 * k)}" fill="#6e4a2c"/>`).join("") +
           [...Array(Math.min(things, 10))].map((_, i) => { const x = tx - 98 * k + i * 21 * k;
             return `<rect x="${f1(x)}" y="${f1(ty - 26 * k)}" width="${f1(16 * k)}" height="${f1(26 * k)}" rx="${f1(3 * k)}" fill="${cols[i % 6]}" stroke="#ffffff" stroke-width="${f1(1.5 * k)}"/>` +
                    (i % 2 ? `<line x1="${f1(x + 8 * k)}" y1="${f1(ty - 24 * k)}" x2="${f1(x + 14 * k)}" y2="${f1(ty - 52 * k)}" stroke="#c9a66b" stroke-width="${f1(2.5 * k)}"/>` : ""); }).join(""));
      const pins = [...Array(Math.max(0, things - 10))].map((_, i) => {
        const x = 150 + (i % 6) * 34 * k, y = y0 + H * 0.12 + Math.floor(i / 6) * 46 * k, c = cols[i % 6];
        return `<rect x="${f1(x)}" y="${f1(y)}" width="${f1(28 * k)}" height="${f1(36 * k)}" fill="#ffffff" stroke="#d8cdb8" stroke-width="${f1(k)}" transform="rotate(${(i % 3 - 1) * 5} ${f1(x + 14 * k)} ${f1(y)})"/>` +
               `<circle cx="${f1(x + 14 * k)}" cy="${f1(y + 15 * k)}" r="${f1(7 * k)}" fill="${c}"/><circle cx="${f1(x + 14 * k)}" cy="${f1(y + 2 * k)}" r="${f1(2.5 * k)}" fill="#e53935"/>`;
      }).join("");
      if (pins) t += wallObj("pinned drawings", pins);   // (ticket 284: a wall object, when there are any)
    } else if (room === "music") {
      // a drum kit, a keyboard on its stand, records on the wall (the
      // things), and notes in the air
      // (the records, on the wall: ticket 284, four a row, the last ending TREE_CLEAR short of the trunk's braid, clear of
      // the ladder; a wall object, behind the friend and all that stands)
      const recs = [...Array(things)].map((_, i) => {
        const x = look.trunk - TREE_CLEAR * k - 17 * k - (3 - (i % 4)) * 40 * k, y = y0 + H * 0.14 + (Math.floor(i / 4) % 5) * 42 * k;
        return `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(17 * k)}" fill="#1f1f22"/><circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(11 * k)}" fill="none" stroke="#333" stroke-width="${f1(k)}"/>` +
               `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(5.5 * k)}" fill="${["#e53935", "#f2c12e", "#3b7dd8", "#27ae60", "#ff8c42", "#e85d75"][i % 6]}"/>`;
      }).join("");
      if (recs) t += wallObj("records", recs);
      // (the drum kit an object, its stands and drum down to dy; the keyboard another, its
      // stand down to floor + 20k, the notes over it with it)
      const dx = x1 - W * 0.26, dy = floor + 14 * k;
      t += obj("drum kit", dy, [[-110, -150, 30], [105, -160, 34]].map(([ox, oy, r]) => `<line x1="${f1(dx + ox * k)}" y1="${f1(dy + oy * k)}" x2="${f1(dx + ox * 0.8 * k)}" y2="${f1(dy)}" stroke="#555" stroke-width="${f1(3 * k)}"/>` +
             `<ellipse cx="${f1(dx + ox * k)}" cy="${f1(dy + oy * k)}" rx="${f1(r * k)}" ry="${f1(5 * k)}" fill="#e2b73c" stroke="#a8862a" stroke-width="${f1(1.5 * k)}"/>`).join("") +
           [-42, 42].map(ox => `<ellipse cx="${f1(dx + ox * k)}" cy="${f1(dy - 112 * k)}" rx="${f1(24 * k)}" ry="${f1(14 * k)}" fill="#c0392b" stroke="#7a1f1f" stroke-width="${f1(2 * k)}"/>` +
             `<ellipse cx="${f1(dx + ox * k)}" cy="${f1(dy - 118 * k)}" rx="${f1(22 * k)}" ry="${f1(7 * k)}" fill="#f4efe6"/>`).join("") +
           `<circle cx="${f1(dx)}" cy="${f1(dy - 52 * k)}" r="${f1(52 * k)}" fill="#c0392b" stroke="#7a1f1f" stroke-width="${f1(3 * k)}"/>` +
           `<circle cx="${f1(dx)}" cy="${f1(dy - 52 * k)}" r="${f1(40 * k)}" fill="#f4efe6"/><text x="${f1(dx)}" y="${f1(dy - 44 * k)}" text-anchor="middle" font-size="${f1(24 * k)}">⭐</text>` +
           `<line x1="${f1(dx - 88 * k)}" y1="${f1(dy - 70 * k)}" x2="${f1(dx - 92 * k)}" y2="${f1(dy)}" stroke="#555" stroke-width="${f1(3 * k)}"/>` +
           `<ellipse cx="${f1(dx - 88 * k)}" cy="${f1(dy - 72 * k)}" rx="${f1(28 * k)}" ry="${f1(10 * k)}" fill="#d8d8d8" stroke="#888" stroke-width="${f1(2 * k)}"/>`);
      const kx = x0 + W * 0.22, ky = floor - 95 * k;
      t += obj("keyboard", floor + 20 * k, [-1, 1].map(sd => `<line x1="${f1(kx - 70 * k)}" y1="${f1(ky + 30 * k)}" x2="${f1(kx + 70 * k)}" y2="${f1(floor + 20 * k)}" stroke="#333" stroke-width="${f1(5 * k)}" transform="scale(${sd} 1)" transform-origin="${f1(kx)} 0"/>`).join("") +
           `<rect x="${f1(kx - 115 * k)}" y="${f1(ky)}" width="${f1(230 * k)}" height="${f1(34 * k)}" rx="${f1(4 * k)}" fill="#222"/>` +
           `<rect x="${f1(kx - 108 * k)}" y="${f1(ky + 10 * k)}" width="${f1(216 * k)}" height="${f1(20 * k)}" fill="#ffffff"/>` +
           [...Array(15)].map((_, i) => [1, 2, 4, 5, 6].includes(i % 7) ? `<rect x="${f1(kx - 108 * k + (i + 0.65) * 14.4 * k)}" y="${f1(ky + 10 * k)}" width="${f1(7 * k)}" height="${f1(12 * k)}" fill="#222"/>` : "").join("") +
           [[-60, -150, "♪"], [-10, -190, "♫"], [40, -160, "♪"]].map(([nx, ny, ch]) => `<text x="${f1(kx + nx * k)}" y="${f1(ky + ny * k)}" font-size="${f1(30 * k)}" fill="#f2c12e">${ch}</text>`).join(""));
    } else {
      // the lookout: a telescope at the window, a map, and pennants overhead
      // (the map and the pennants, on the wall: drawn before the telescope, which they do not cross; ticket 284: wall objects,
      // the map clear of the trunk and above the floor, the pennants' cord tied at the trunk)
      const mx = look.trunk - TREE_CLEAR * k - 170 * k, my = floor - 40 * k - 120 * k, mMap = t.length;
      t += `<rect x="${f1(mx)}" y="${f1(my)}" width="${f1(170 * k)}" height="${f1(120 * k)}" fill="#f3e3b8" stroke="#a0703f" stroke-width="${f1(3 * k)}"/>` +
           `<path d="M${P(mx + 20 * k, my + 90 * k)} Q${P(mx + 60 * k, my + 30 * k)} ${P(mx + 100 * k, my + 70 * k)} T${P(mx + 150 * k, my + 30 * k)}" fill="none" stroke="#c0392b" stroke-width="${f1(3 * k)}" stroke-dasharray="${f1(6 * k)} ${f1(5 * k)}"/>` +
           `<text x="${f1(mx + 140 * k)}" y="${f1(my + 40 * k)}" font-size="${f1(26 * k)}" fill="#c0392b" font-weight="bold">X</text>`;
      t = wallSince(t, mMap, "treasure map");
      const n = Math.max(things, 1), a = x0 + W * 0.04, b = look.trunk - TREE_CLEAR * k, top = y0 + H * 0.06, mPennants = t.length;
      t += `<path d="M${P(a, top)} Q${P((a + b) / 2, top + 60 * k)} ${P(b, top)}" fill="none" stroke="#555" stroke-width="${f1(2 * k)}"/>` +
           [...Array(things)].map((_, i) => {
             const u = (i + 0.5) / n, x = a + (b - a) * u, y = (1 - u) * (1 - u) * top + 2 * u * (1 - u) * (top + 60 * k) + u * u * top;
             return `<polygon points="${P(x - 14 * k, y)} ${P(x + 14 * k, y)} ${P(x, y + 34 * k)}" fill="${["#e53935", "#f2c12e", "#2e86de", "#27ae60", "#8e44ad"][i % 5]}"/>`;
           }).join("");
      t = wallSince(t, mPennants, "pennants");
      // (the telescope an object, its tripod's feet at floor + 20k (its mirroring group
      // keeps every y); the map is on the wall, and the pennants are strung along it)
      const tx = x0 + W * 0.3;
      t += obj("telescope", floor + 20 * k, telescopeSvg(c, tx, floor));
    }
    return t;
  })(), floor));
  // the ways out: a door to each house a rope bridge joins this one to, a
  // hatch down its ladder, a ladder up to the house above; each a doorway
  // (the doors are one object, at the back wall's foot, floor (ticket 192, Matthew: drawn as
  // scenery after the room's objects, they covered the friend standing in front of them); the
  // hatch is one, at its hole's front edge, hy + 20k = floor + 92k in waysSvg, and the ladder up
  // another, at its foot, floor + 14k: each drawn by itself, in the order waysSvg draws them all)
  const links = houseLinks(o, inside - 1), ways = way => links.filter(L => way.includes(L.way));
  // (the houses' looks: their animals, their colors, the sign's map's and the bridge doors' views; tickets 244, 247)
  const looksKey = [o.houses, o.bridges, o.ladders, o.treeHouseSize, ...treeHouses(o, Math.max(1, Math.min(6, o.houses ?? 4)), 1).map(h => [h.animal, h.look.cols])];
  s += SEP + part("room-ways", [c.F, links, [1, 2, 3, 4, 5, 6].map(n => o["hatchShape" + n]).join(), looksKey, [1, 2, 3, 4, 5, 6].map(n => o["hatchShut" + n] ? 1 : 0).join(""), o.hatchShapeTop, o.hatchShutTop, o.leverShine, o.leverShineColor, o.leverFlicker], () => {   // (ticket 252: its trapdoors' shapes, here and above; ticket 334: its trap door shut or open, its lever's shine)
    const down = ways(["down"]), up = ways(["up"]);
    const doors = ways(["left", "right"]);
    // (ticket 238, Matthew: "we get (the main char) gets drawn below the ladder, the trap door flap, and we don't visibly
    // go into the hole") the hatch (its lid up at the hole's far edge, the hole, the ladder's top) at the lid's foot, the
    // hole's far edge (hy - 18), not its near one: anyone at the near edge, climbing down, or beside it is in front of
    // it; only one standing behind the hole is behind its lid
    return (doors.length ? obj("side doors", floor, waysSvg(c, floor, doors)) : "") +
           (down.length ? obj("hatch", floor + 54 * k, waysSvg(c, floor, down)) + wallObj("hatch lever", hatchLeverSvg(c, floor, !o["hatchShut" + inside])) : "") +
           (up.length ? obj("ladder up", floor + 14 * k, waysSvg(c, floor, up)) : "");
  });
  // which house this is: a sign hanging from the rafters
  // (drawn after the room's objects, so it is in front of everything, an object at
  // Infinity, as the dark and the sign below are: the friend is always under it)
  // (ticket 244) under its name, a map of the whole tree house, this house ringed in white (treeMapSvg): as wide as the
  // room's corner leaves room for, the map as wide as the sign
  s += SEP + obj("house sign", Infinity, part("room-sign", [c.F, inside, houseEmoji(o, inside), looksKey], () => {
    const sx = x1 - W * 0.14, sy = y0 + H * 0.12, hw = Math.max(46 * k, Math.min(76 * k, W * 0.14 - 4 * k));
    const [bx, by, bw, bh] = TREE_MAP_BOX, m = (2 * hw - 14 * k) / bw, my = sy + 38 * k, mh = bh * m;
    // (hanging still on its ropes: no wind indoors)
    return (`<path d="M${P(sx - (hw - 16 * k), sy - 60 * k)} L${P(sx - (hw - 16 * k), sy)} M${P(sx + hw - 16 * k, sy - 60 * k)} L${P(sx + hw - 16 * k, sy)}" stroke="#d8c49a" stroke-width="${f1(2 * k)}"/>` +
           `<rect x="${f1(sx - hw)}" y="${f1(sy - 4 * k)}" width="${f1(2 * hw)}" height="${f1(49 * k + mh)}" rx="${f1(5 * k)}" fill="#a0703f" stroke="#5e3d22" stroke-width="${f1(2.5 * k)}"/>` +
           `<text x="${f1(sx)}" y="${f1(sy + 25 * k)}" text-anchor="middle" font-size="${f1(24 * k)}" font-weight="bold" fill="#fff8e7" font-family="sans-serif">${houseEmoji(o, inside)} ${inside}</text>` +
           `<rect x="${f1(sx - hw + 7 * k)}" y="${f1(my)}" width="${f1(2 * hw - 14 * k)}" height="${f1(mh)}" rx="${f1(3 * k)}" fill="#c99d63" stroke="#5e3d22" stroke-width="${f1(1.5 * k)}"/>` +
           `<g class="tree-map" ${tf(sx - hw + 7 * k - bx * m, my - by * m, m)}>${treeMapSvg(o, inside)}</g>`);
  }));
  // dim in here: darker toward the corners
  // (over everything, the friend too: in front, at Infinity)
  s += SEP + obj("dark", Infinity, part("room-dim", c.F, () => `<defs><radialGradient id="bg-dim" gradientUnits="userSpaceOnUse" cx="${f1((x0 + x1) / 2)}" cy="${f1(y0 + H * 0.42)}" r="${f1(0.62 * Math.hypot(W, H))}">` +
    `<stop offset="0.5" stop-color="#1a0f05" stop-opacity="0"/><stop offset="1" stop-color="#1a0f05" stop-opacity="0.5"/></radialGradient></defs>` +
    all("url(#bg-dim)").replace("<rect ", `<rect pointer-events="none" `)));
  // (the way toward us, where it leads somewhere: since ticket 233 a nav arrow, not drawn here: insideWays, 25-world.js)
  return s;
}
// (ticket 516, Matthew: "New background --- round tree house interior, inspired by hut."; "Can we show a round treehouse room
// with a tree trunk in the center, and lots of limbs reaching into the roof?") The round room at the top of the tree, inside:
// the grass hut's round geometry (hutGeom: we stand near its front, facing the back half of its wall) at half the picture's
// width, so its wall meets the picture's sides, its wall lower so the roof shows; the thatched dome's underside, its rafters
// running up to a peak behind the trunk; a wall of upright planks (the bunk room's wood) round the back half; a floor of
// boards in rings round the trunk's foot, like a tree's rings. The trunk rises through the middle: the braid as it is up
// there (braidSvg from halfway up), its five strands, and the crown's own limbs (crownLimbs, as outside) leave it: the lowest
// four run out nearly level and rest on the wall plate, as rafters do; the next two go up through the thatch into the
// daylight; the last two rise steeply out of the picture. Bare and still: the wind is outdoors (seen through the windows).
// Its ways: one hatch down to the house below (topHouse) and back up its ladder, the windows out to the tree house, the way
// toward us (treetopBack). Its furnishing, ticket 517's: the part "top-furnish", in one of five styles (topFurniture)
const topGeom = c => { const G = hutGeom(c, { r: 0.5, top: 0.31, dt: 65 }), { y0, H } = c, apex = [0, y0 - H * 0.11];
  // (a point on the cone of thatch above the wall's top at th, v of the way up to the peak; the wall's height at th, a share of the back's)
  const cone = (th, v) => { const [px, py] = G.at(th, 0); return [px + (apex[0] - px) * v, py + (apex[1] - py) * v]; };
  return { ...G, apex, cone, tall: th => (G.fy(th) - G.ty(th)) / (G.Yf - G.Yt), cy: G.Yf + G.Df,
           // (a point on the floor, r from the trunk's middle at th: for ticket 517's furniture)
           floorAt: (r, th) => [r * Math.cos(th), G.Yf + G.Df + r * (G.Df / G.R) * Math.sin(th)] }; };
const TOP_IN = { trunk: 190, fp: 95, from: 0.5 };   // (the trunk inside: its width, its footprint's half width, where up the braid it begins)
// the crown's limbs inside (crownLimbs j 0..TOP.limbs - 1, in their order up the trunk, each to its own side, as outside):
// where each leaves the trunk (p0), its bend (p1), its end (p2), its widths; what it ends in (kind: "plate", "hole", "up")
function topLimbsIn(c) {
  const { y0, k } = c, G = topGeom(c), PI = Math.PI, tw = TOP_IN.trunk * k, sc = tw / 124;
  return crownLimbs(c.R).slice(0, TOP.limbs).map(L => {
    const { j, sd } = L, w0 = 1.15 * L.w * sc, w1 = w0 * 0.6, p0 = [sd * tw * 0.2, G.Yt + 40 * k - 26 * k * j];
    if (j < 4) {   // (out to the wall's plate, nearly level, the lowest ones widest)
      const th = 1.5 * PI + sd * PI * (0.14 + 0.3 * (1 - (j >> 1))), p2 = [G.x(th) - sd * w1 * 0.2, G.ty(th) - w1 * 0.25];
      return { ...L, kind: "plate", th, w0, w1, p0, p1: [p0[0] + (p2[0] - p0[0]) * 0.5, Math.min(p0[1], p2[1]) - 22 * k], p2 };
    }
    if (j < 6) {   // (up through the thatch, into the daylight)
      const th = 1.5 * PI + sd * PI * 0.24, p2 = G.cone(th, j === 4 ? 0.3 : 0.4);
      return { ...L, kind: "hole", th, w0, w1, p0, p1: [p0[0] + (p2[0] - p0[0]) * 0.6, p0[1] - 8 * k], p2 };
    }
    const p2 = [sd * (110 + 60 * (j - 6)) * k, y0 - 50 * k];   // (steeply up, out of the picture's top: j 6 behind the trunk, j 7 over it)
    return { ...L, kind: "up", th: null, w0, w1, p0, p1: [p0[0] + sd * 40 * k, p0[1] - 50 * k], p2 };
  });
}
const qPt = (L, u) => [0, 1].map(a => (1 - u) * (1 - u) * L.p0[a] + 2 * u * (1 - u) * L.p1[a] + u * u * L.p2[a]);
// a limb inside: bark, its edge, a streak along it; where it meets the thatch, a straw collar over its end; on the plate, a tuft
function topLimbSvg(c, L) {
  const { k, R } = c, [ex, ey] = L.p2;
  let t = taperSvg(L.p0, L.p1, L.p2, L.w0, L.w1, "#6e4a2c").replace("/>", ` stroke="#3a2414" stroke-width="${f1(1.6 * k)}"/>`) +
    `<path d="M${f1(L.p0[0])} ${f1(L.p0[1])} Q${f1(L.p1[0])} ${f1(L.p1[1])} ${f1(ex)} ${f1(ey)}" fill="none" stroke="#3a2414" stroke-width="${f1(1.3 * k)}" opacity="0.45"/>`;
  if (L.kind === "up") return t;
  const n = L.kind === "hole" ? 9 : 5, rx = L.w1 * (L.kind === "hole" ? 0.75 : 0.6) + 4 * k, ry = rx * 0.62;
  let d = "";
  for (let i = 0; i < n; i++) { const a = Math.PI + Math.PI * i / (n - 1), fx = ex + Math.cos(a) * rx, fy = ey + Math.sin(a) * ry * (L.kind === "hole" ? 1 : 0.4), l = (7 + 6 * R(L.j * 9 + i, 5163)) * k;
    d += `M${f1(fx)} ${f1(fy)} q${f1((i - n / 2) * 0.4 * k)} ${f1(l * 0.5)} ${f1((i - n / 2) * 0.8 * k)} ${f1(l)} `; }
  return t + `<path d="${d.trim()}" stroke="#c9b45a" stroke-width="${f1(2.6 * k)}" fill="none" stroke-linecap="round"/>`;
}
function topShellInSvg(c) {
  const { x0, y0, x1, y1, W, H, k, pad, R } = c, G = topGeom(c), PI = Math.PI, top = G.ring(0), bottom = G.ring(1);
  const toApex = v => top.map(([x, y]) => [x + (G.apex[0] - x) * v, y + (G.apex[1] - y) * v]);
  // the dome of thatch from inside: straw, rings of it toward the peak, a few bright gaps, the rafters (every fourth heavy)
  let t = `<polygon points="${f1(x0 - pad)},${f1(y0 - pad)} ${f1(x1 + pad)},${f1(y0 - pad)} ${ptsOf(top.slice().reverse())}" fill="#c9a35a"/>`;
  t += `<path d="${[0.2, 0.4, 0.58, 0.74].map(v => "M" + ptsOf(toApex(v)).replace(/ /g, " L")).join(" ")}" fill="none" stroke="#b8924a" stroke-width="${f1(5 * k)}"/>`;
  let gaps = "";
  for (let i = 0; i < 9; i++) { const v = 0.1 + 0.6 * R(i, 5164), th = PI + PI * (0.08 + 0.84 * R(i, 5165)), [x, y] = G.cone(th, v), l = (10 + 12 * R(i, 5166)) * k * (1 - v);
    gaps += `M${f1(x - l)} ${f1(y)} Q${f1(x)} ${f1(y - 3 * k)} ${f1(x + l)} ${f1(y)} `; }
  t += `<path d="${gaps.trim()}" fill="none" stroke="#fff3c4" stroke-width="${f1(2 * k)}" opacity="0.7"/>`;
  let thin = "", heavy = "";
  for (let j = 0; j <= 20; j++) { const [x, y] = G.at(PI + PI * j / 20, 0), d = `M${f1(x)} ${f1(y)} L${f1(G.apex[0])} ${f1(G.apex[1])} `; if (j % 4) thin += d; else heavy += d; }
  t += `<path d="${thin.trim()}" stroke="#a8823a" stroke-width="${f1(1.6 * k)}"/><path d="${heavy.trim()}" stroke="#6e4a28" stroke-width="${f1(7 * k)}"/>`;
  // the wall: upright planks round the back half (the bunk room's wood), narrower as the wall turns away, a knot here and there
  t += `<polygon points="${ptsOf(top)} ${ptsOf(bottom.slice().reverse())}" fill="#5e3d22"/>`;
  const planks = ["", "", "", ""], N = 30;
  let seams = "", knots = "";
  for (let i = 0; i < N; i++) {
    const a = PI + PI * i / N, b = PI + PI * (i + 1) / N, Q = [G.at(a, 0), G.at(b, 0), G.at(b, 1), G.at(a, 1)];
    if (Math.abs(Q[1][0] - Q[0][0]) < 2 * k) continue;
    planks[i % 4] += `M${ptsOf(Q).replace(/ /g, " L")} Z `;
    seams += `M${f1(Q[0][0])} ${f1(Q[0][1])} L${f1(Q[3][0])} ${f1(Q[3][1])} `;
    if (R(i, 5161) < 0.4) { const m = (a + b) / 2, [kx, ky] = G.at(m, 0.2 + 0.6 * R(i, 5162)), rx = 4 * k * G.s(m), ry = 7 * k;
      knots += `M${f1(kx - rx)} ${f1(ky)} a${f1(rx)} ${f1(ry)} 0 1 0 ${f1(2 * rx)} 0 a${f1(rx)} ${f1(ry)} 0 1 0 ${f1(-2 * rx)} 0 `; }
  }
  t += planks.map((d, i) => `<path d="${d.trim()}" fill="${ROOM_LOOK.bunks.planks[i]}"/>`).join("") +
       `<path d="${seams.trim()}" stroke="#7a4f2a" stroke-width="${f1(1.5 * k)}"/><path d="${knots.trim()}" fill="#7a4f2a"/>`;
  // the wall plate under the eaves (the lowest limbs rest on it), the eaves' fringe hanging over it, a skirting along the floor
  t += `<polyline points="${ptsOf(G.ring(0.012))}" fill="none" stroke="#5e3d22" stroke-width="${f1(14 * k)}"/>`;
  const straw = ["", "", ""];
  for (let i = 0; i < 50; i++) { const th = PI + PI * (i + 0.5) / 50, [x, y] = G.at(th, 0), l = (10 + 12 * R(i, 5166)) * k * (0.6 + 0.4 * G.s(th));
    straw[i % 3] += `M${f1(x)} ${f1(y - 8 * k)} q0 ${f1(l * 0.5)} ${f1((R(i, 5167) - 0.5) * 6 * k)} ${f1(l)} `; }
  t += straw.map((d, i) => `<path d="${d.trim()}" fill="none" stroke="${["#c9b45a", "#a8963a", "#d8c870"][i]}" stroke-width="${f1(3 * k)}" stroke-linecap="round"/>`).join("") +
       `<polyline points="${ptsOf(G.ring(0.985))}" fill="none" stroke="#6b4423" stroke-width="${f1(9 * k)}"/>`;
  // the floor: boards in rings round the trunk's foot, like a tree's rings, their ends staggered ring by ring; inside the
  // wall's foot (a clip, drawn once with the room)
  const floorPts = `${ptsOf(bottom)} ${f1(x1 + pad)},${f1(y1 + pad)} ${f1(x0 - pad)},${f1(y1 + pad)}`, ry = G.Df / G.R, r0 = TOP_IN.fp * k * 1.1;
  t += `<polygon points="${floorPts}" fill="#9a6a3d"/><defs><clipPath id="bg-top-floor"><polygon points="${floorPts}"/></clipPath></defs><g clip-path="url(#bg-top-floor)">`;
  const rr = j => r0 + j * 64 * k * (1 + 0.1 * j);
  let rings = "", ends = "";
  for (let j = 0; j <= 9; j++) { const r = rr(j), b = r * ry * 1.2;
    rings += `M${f1(-r)} ${f1(G.cy)} a${f1(r)} ${f1(b)} 0 1 0 ${f1(2 * r)} 0 a${f1(r)} ${f1(b)} 0 1 0 ${f1(-2 * r)} 0 `;
    if (j < 9) for (let q = 0; q < 7; q++) { const a = 2 * PI * (q + 0.5 * (j % 2) + 0.3 * R(j * 10 + q, 5172)) / 7, r1 = rr(j + 1);
      ends += `M${f1(Math.cos(a) * r)} ${f1(G.cy + Math.sin(a) * r * ry * 1.2)} L${f1(Math.cos(a) * r1)} ${f1(G.cy + Math.sin(a) * r1 * ry * 1.2)} `; } }
  return t + `<path d="${rings.trim()}" fill="none" stroke="#6e4a2c" stroke-width="${f1(1.8 * k)}"/><path d="${ends.trim()}" stroke="#6e4a2c" stroke-width="${f1(1.4 * k)}"/></g>` +
    `<polyline points="${ptsOf(bottom)}" fill="none" stroke="#4a2e14" stroke-width="${f1(6 * k)}" opacity="0.6"/>`;
}
// the hatch at the front, on the house below's side (sd), and the lever on its post beside it: where, for the drawing, the
// arrivals (a step from the hatch, toward the middle) and ticket 517's furniture (kept clear)
// (ticket 407: the lever's post midway between the hole's edge and the round wall: the friend walked to it stands with a sole
// either side of its foot, and at 0.43 the near sole was 3 px into the hole's solid; the span between them is ~102 px, the
// soles ~97 px across, so there is little room either way)
const topHatchAt = (c, o = c.o) => { const sd = topSide(o); return { sd, hx: sd * c.W * 0.3, hy: c.g + 26 * c.k, px: sd * c.W * 0.437, pz: c.g + 4 * c.k }; };
// (ticket 517, Matthew: "5 styles with objects we have already. How should we furnish a round room?") The round room's
// furnishing, picked by a chip row in its own settings (topFurnish), each style built from drawings the game already
// has, each by its own code (the hut's living room, the hut bedroom, the tree rooms', the healer's, the hut potion room's):
// what stands follows the curved wall, its back to it; a bench wraps the trunk; shelves sit on the trunk; what hangs hangs
// from the limbs. Each style keeps clear the hatch's quarter of the floor (in front, on the house below's side, sd), the
// lever's post, where the friend arrives, the lane behind the trunk and the ring round it, and only low things stand under
// the windows. The styles are laid out for the hatch on the right (sd 1) and mirrored for it on the left: the one piece a
// style may stand in front goes on the side away from the hatch.
const TOP_FURNISH = [["none", "🚫 none"], ["parlour", "🛋️ hut parlour"], ["loft", "🛏️ sleeping loft"], ["reading", "📚 reading ring"],
  ["potions", "🧪 potion nook"], ["tea", "🍵 tea and kitchen"]];
// where a piece stands: r from the trunk's middle at th on the floor (topGeom's floorAt; for sd 1, mirrored for -1), its
// drawing's anchor (ax, ay: its middle at its foot, in its own room's units) put there; sized by depth as the friend is
// (SCENE_VANISH), from the size it stands at in its own room (its foot there at from, times size); squeezed across as the
// wall turns away, as the windows are, never under 0.55 (a turned piece is the same drawing, squeezed: not a new angle). at() takes a point
// of the drawing to the picture (for its seat); g() wraps the drawing; fp() is its footprint on the floor, an unseen oval
// (data-footprint, as the trunk's), half width hw and half depth hd in its own units
function topSpot(c, sd, r, th, ax, ay, from, size = 1, squeeze = true, across = 1) {
  const G = topGeom(c), [x, y] = G.floorAt(r, th), X = sd * x, v = SCENE_VANISH.treetop, f = size * (y - v) / (from - v);
  const fx = f * across * (squeeze && Math.sin(th) < 0 ? Math.max(0.55, G.s(th)) : 1);
  return { X, Y: y, f, fx,
    at: (px, py) => [X + (px - ax) * fx, y + (py - ay) * f],
    g: t => `<g transform="translate(${f1(X)} ${f1(y)}) scale(${+fx.toFixed(4)} ${+f.toFixed(4)}) translate(${f1(-ax)} ${f1(-ay)})">${t}</g>`,
    fp: (hw, hd, dy = 0) => `<ellipse data-footprint="" cx="${f1(X)}" cy="${f1(y + dy * f - hd * f)}" rx="${f1(hw * fx)}" ry="${f1(hd * f)}" fill="none"/>` };
}
// a limb's point to hang from: the plate limb (j 0 to 3) on that side of the trunk (side, -1 or 1), u along it; the lowest
// two (j 0, 1) carry the lanterns at u 0.35, so hangings go farther out on them, or on the next two (j 2, 3)
const topHang = (LM, side, low, u) => { const L = LM.slice(low ? 0 : 2, low ? 2 : 4).find(L => L.sd === side) || LM[low ? 0 : 2]; return qPt(L, u); };
// a style's pieces: each { name, z (its foot), svg (a function: drawn only for the scene), seat (a seat's line, if one) }; a
// wall thing has z "wall", a floor thing "floor"
function topFurniture(c, o) {
  const style = o.topFurnish ?? "none";
  if (!TOP_FURNISH.some(([s]) => s === style) || style === "none") return [];
  const { W, g, k } = c, PI = Math.PI, sd = topSide(o), G = topGeom(c), out = [];
  const LM = () => topLimbsIn(c), treeFloor = g - 110 * k, healerFloor = g - 100 * k, hutFloor = g - HUT_UP * k;
  const seatOf = (p, name, x0, x1, y, z, facing, more = {}) => { const [a, sy] = p.at(x0, y), [b] = p.at(x1, y), [, sz] = p.at(x0, z);
    return { name, x0: Math.min(a, b), x1: Math.max(a, b), y: sy, z: sz + 1, facing, ...more }; };
  const put = (name, p, svg, fp, seat) => out.push({ name, z: p.Y, svg: () => p.g(svg()) + (fp || ""), seat });
  if (style === "parlour") {
    // 🛋️ the grass hut's living room, curved: its couch against the wall away from the hatch, under the window there,
    // turned toward the trunk, its pillows on it; the rug round the trunk's foot; the lamp on the hatch's side, the plant on
    // the other; the wind chime from a limb
    const cs = 0.65, p = topSpot(c, sd, G.R - 70 * k, 1.3 * PI, 0, 0, g - 40 * k, cs);
    put("couch", p, () => `<g ${tf(0, 0, k)}>` + couchSvg("hutroom", o.hutCouchColor ?? "#7a5a3a") + hutPillowsSvg(o.pillows ?? 4) + `</g>`, p.fp(230 * k, 22 * k),
      seatOf(p, "couch", -232 * k, 236 * k, -112 * k, 0, ["front", "left", "right"]));
    out.push({ name: "rug", z: "floor", svg: () => `<g transform="translate(0 ${f1(G.cy)}) scale(0.6) translate(0 ${f1(-G.cy)})">` +
      rugSvg(c, rugColorsOf(o.hutRugColors ?? "straw"), (G.cy - g) / k) + `</g>` });
    const lp = topSpot(c, sd, G.R - 60 * k, 1.79 * PI, 0, 0, g - 90 * k, 0.75);
    put("lamp", lp, () => lampSvg(c, 0, 0), lp.fp(40 * k, 10 * k));
    const pp = topSpot(c, sd, G.R - 35 * k, 1.06 * PI, 0, 0, g - 70 * k, 0.9);
    put("plant", pp, () => plantSvg(c, 0, 0), pp.fp(26 * k, 10 * k));
    out.push({ name: "wind chime", z: "wall", svg: () => windChimeSvg(c, ...topHang(LM(), -sd, false, 0.62)) });
  }
  if (style === "loft") {
    // 🛏️ the tree room's bunk beds, tall, near the picture's side away from the hatch (clear of the window), and their toy
    // chest; the hut's bed of logs along the wall on the hatch's side, low, under that window; the stump with its candle
    // beside it, toward the trunk (outside the ring); the dreamcatcher over the bed, from a limb
    const toys = o.toys ?? 10, bp = topSpot(c, sd, G.R - 55 * k, 1.2 * PI, 0, treeFloor + 10 * k, treeFloor + 10 * k, TREE_FIT);
    const bunks = () => bunkBedsSvg(c, 0, treeFloor, toys, -75 * k);
    put("bunk beds", bp, () => `<defs>${toyDefs}</defs>` + bunks()[0], bp.fp(110 * k, 22 * k, 0),
      seatOf(bp, "bunk beds", -105 * k, 105 * k, treeFloor - 174 * k, treeFloor + 10 * k, ["front"], { lie: true, head: "left" }));
    const cp = topSpot(c, sd, G.R - 60 * k, 1.34 * PI, 0, treeFloor + 20 * k, treeFloor + 20 * k, TREE_FIT);
    put("toy chest", cp, () => bunks()[1], cp.fp(75 * k, 18 * k));
    const by = g + 34 * k, hp = topSpot(c, sd, G.R - 70 * k, 1.8 * PI, 0, by, by, 0.75), bl = o.hutBlanket ?? "#b8434f";
    put("bed", hp, () => hutBedSvg(c, 0, by, bl), hp.fp(210 * k, 22 * k),
      seatOf(hp, "bed", -198 * k, 198 * k, by - 108 * k, by, ["front"], { lie: true, head: "left" }));
    const sy = g + 20 * k, spt = topSpot(c, sd, G.R - 200 * k, 1.8 * PI, 0, sy, sy, 0.8);
    put("stump", spt, () => stumpCandleSvg(c, 0, sy), spt.fp(30 * k, 9 * k));
    out.push({ name: "dreamcatcher", z: "wall", svg: () => { const [dx, dy] = topHang(LM(), sd, false, 0.55);
      return swaying(o, dreamcatcherSvg(c, dx, dy, dy + 60 * k), dx, dy, 5, 0.2); } });
  }
  if (style === "reading") {
    // 📚 the reading nook and the lookout: the bookcase's books on shelves bent round the trunk's front (on the trunk, no
    // footprint of their own); the beanbag in front, away from the hatch; the telescope under the window on the hatch's
    // side, aimed out; the nook's lantern from a limb
    out.push({ name: "shelves on the trunk", z: G.cy + 0.5, svg: () => topTrunkShelvesSvg(c, o.toys ?? 10) });
    const gp = topSpot(c, sd, 290 * k, 0.78 * PI, 0, treeFloor + 60 * k, treeFloor + 60 * k, 0.55);
    put("beanbag", gp, () => beanbagSvg(c, 0, treeFloor), gp.fp(80 * k, 20 * k),
      seatOf(gp, "beanbag", -36 * k, 36 * k, treeFloor - 50 * k, treeFloor + 60 * k, ["front", "left", "right"]));
    const tp = topSpot(c, sd, G.R - 70 * k, 1.7 * PI, 0, treeFloor + 20 * k, treeFloor + 20 * k, TREE_FIT, false);
    put("telescope", tp, () => telescopeSvg(c, 0, treeFloor), tp.fp(50 * k, 14 * k));
    out.push({ name: "lantern", z: "wall", svg: () => { const [lx, ly] = topHang(LM(), -sd, false, 0.6); return nookLanternSvg(c, lx, ly, ly + 60 * k); } });
  }
  if (style === "potions") {
    // 🧪 the healer's room and the hut potion room: her shelves of potions and her cat, tall, near the picture's side away
    // from the hatch; the hut's curved shelves of bottles high on the wall on the hatch's side; herbs drying from the four
    // plate limbs (beyond the lanterns); the cauldron on its fire in front, away from the hatch; her worktable under the
    // window away from the hatch
    const cat = { catFur: o.healerCatFur ?? "#e8862a", catWhite: o.healerCatWhite ?? "#ffffff", catEyes: o.healerCatEyes ?? "#22c55e", catNose: "#f4a0b0" };
    const sp = topSpot(c, sd, G.R - 45 * k, 1.09 * PI, 0, 0, healerFloor, 0.8), [shelves, catSvg] = potionShelvesSvg(c, -110 * k, 110 * k, -300 * k, 0, cat);
    put("potion shelves", sp, () => shelves + catSvg, sp.fp(118 * k, 18 * k));
    [0.6, 0.78].forEach((u, row) => out.push({ name: `shelf of potions ${row + 1}`, z: "wall", svg: () => hutShelfSvg(c, G, u, row, (sd > 0 ? 1.74 : 1.12) * PI, (sd > 0 ? 1.88 : 1.26) * PI) }));
    [0, 1, 2, 3].forEach(i => out.push({ name: `drying herbs ${i + 1}`, z: "wall", svg: () => { const L = LM()[i], [hx, hy] = qPt(L, i < 2 ? 0.55 : 0.6);
      return herbBundleSvg(c, i, hx, hy + L.w0 * 0.2 - 8 * k, 0.9); } }));
    const pp = topSpot(c, sd, 340 * k, 0.85 * PI, 0, 0, g, 0.8);
    put("cauldron", pp, () => hutCauldronSvg(c, 0, 0, o.hutPotion ?? "#7c3aed"), pp.fp(90 * k, 17 * k, 29 * k));
    const wy = g + 20 * k, wp = topSpot(c, sd, G.R - 70 * k, 1.33 * PI, 0, wy + 64 * k, wy + 64 * k, 0.8);
    put("worktable", wp, () => worktableSvg(c, 0, wy), wp.fp(100 * k, 14 * k));
  }
  if (style === "tea") {
    // 🍵 the healer's tea table and its two chairs in front, away from the hatch, the chairs facing the table; the tree
    // room kitchen's counter low along the wall under the window away from the hatch, its stove tall on the hatch's side;
    // a bench of three logs (the hut center's) wrapping the trunk's front, away from the hatch, to sit on facing us
    const ty = g + 34 * k, tp = topSpot(c, sd, 340 * k, 0.78 * PI, 0, ty + 42 * k, ty + 42 * k, 0.85), set = () => teaSetSvg(c, 0, ty);   // (drawn only for the scene: a seat's c has no P)
    const tz = tp.at(0, ty + 44 * k)[1];
    out.push({ name: "chair, left", z: tz, svg: () => tp.g(set()[0]) + `<ellipse data-footprint="" cx="${f1(tp.at(-92 * k, 0)[0])}" cy="${f1(tz - 10 * k * tp.f)}" rx="${f1(26 * k * tp.fx)}" ry="${f1(10 * k * tp.f)}" fill="none"/>`,
      seat: seatOf(tp, "chair, left", -118 * k, -66 * k, ty + 4 * k, ty + 44 * k, ["right"]) });
    out.push({ name: "chair, right", z: tz, svg: () => tp.g(set()[1]) + `<ellipse data-footprint="" cx="${f1(tp.at(92 * k, 0)[0])}" cy="${f1(tz - 10 * k * tp.f)}" rx="${f1(26 * k * tp.fx)}" ry="${f1(10 * k * tp.f)}" fill="none"/>`,
      seat: seatOf(tp, "chair, right", 66 * k, 118 * k, ty + 4 * k, ty + 44 * k, ["left"]) });
    put("tea table", tp, () => set()[2], tp.fp(40 * k, 10 * k));
    const top = -130 * k, cp = topSpot(c, sd, G.R - 60 * k, 1.28 * PI, 0, 0, treeFloor + 10 * k, TREE_FIT);
    put("counter", cp, () => kitchenCounterSvg(c, -W * 0.17, top), cp.fp(W * 0.17 + 8 * k, 16 * k));
    const vp = topSpot(c, sd, G.R - 60 * k, 1.81 * PI, 0, 0, treeFloor + 10 * k, TREE_FIT);
    put("stove", vp, () => stoveSvg(c, -55 * k, top), vp.fp(55 * k, 16 * k));
    // (each log along the ring, shorter as it turns side on: the same drawing, squeezed)
    [0.56, 0.72, 0.88].forEach((u, i) => { const lp = topSpot(c, sd, 170 * k, u * PI, 0, 14 * k, g + 74 * k, 1, false, Math.max(0.55, Math.sin(u * PI)));
      put(`log ${i + 1}`, lp, () => logSvg(0, 0, k), lp.fp(50 * k, 14 * k), seatOf(lp, `log ${i + 1}`, -50 * k, 50 * k, -14 * k, 14 * k, ["front"])); });
  }
  return out;
}
// (ticket 517) the reading ring's shelves on the trunk: three planks bent round its front, low to high, each a short arc
// at the trunk's width there, the bookcase's books standing on them (its colours, sizes and seeds), toys of them twice as
// many books as there are things in the room (all one drawing: shelves sit on the trunk, so no footprint)
function topTrunkShelvesSvg(c, things) {
  const { k, R } = c, G = topGeom(c), tw = TOP_IN.trunk * k, foot = G.cy;
  let t = "", i = 0;
  [[40, 0.53], [105, 0.49]].forEach(([up, half], row) => {
    const y = foot - up * k, rx = tw * half, ry = rx * 0.22, a0 = 0.18 * Math.PI, a1 = 0.82 * Math.PI, pts = [];
    for (let j = 0; j <= 16; j++) { const a = a0 + (a1 - a0) * j / 16; pts.push([rx * Math.cos(a), y + ry * Math.sin(a)]); }
    let books = "";
    const n = Math.min(9, Math.max(3, Math.round(things * 2 / 3)));
    for (let j = 0; j < n; j++, i++) { const a = a0 + (a1 - a0) * (j + 0.5) / n, bx = rx * Math.cos(a), by = y + ry * Math.sin(a), sq = 0.45 + 0.55 * Math.sin(a);
      const bw = (10 + 8 * R(i, 160)) * k * sq, bh = (34 + 18 * R(i, 161)) * k;
      books += `<rect x="${f1(bx - bw / 2)}" y="${f1(by - bh)}" width="${f1(bw)}" height="${f1(bh)}" fill="${["#c0392b", "#2e86de", "#27ae60", "#f2c12e", "#8e44ad", "#e67e22", "#16a085"][i % 7]}" stroke="#3a2414" stroke-width="${f1(k)}"/>`; }
    t += books + `<polyline points="${ptsOf(pts)}" fill="none" stroke="#4a2e1a" stroke-width="${f1(11 * k)}" stroke-linecap="round"/>` +
      `<polyline points="${ptsOf(pts)}" fill="none" stroke="#6e4a2c" stroke-width="${f1(7 * k)}" stroke-linecap="round"/>` +
      [0.25, 0.75].map(u => { const a = a0 + (a1 - a0) * u; return `<path d="M${f1(rx * Math.cos(a))} ${f1(y + ry * Math.sin(a) + 4 * k)} l${f1(-4 * k * Math.cos(a))} ${f1(14 * k)}" stroke="#4a2e1a" stroke-width="${f1(4 * k)}"/>`; }).join("");
  });
  return t;
}
function treetopScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, o } = c, PI = Math.PI, G = topGeom(c), ws = (o.windowSize ?? 100) / 100;
  const n = topHouse(o), { sd, hx, hy, px, pz } = topHatchAt(c), slow = sceneLfoKey(o, "slow"), fast = sceneLfoKey(o, "fast");
  const LM = topLimbsIn(c), tw = TOP_IN.trunk * k, foot = G.cy;
  let s = SEP + part("top-shell", c.F, () => topShellInSvg(c));
  // the windows, high on the back wall either side of the trunk (the tree rooms' window, round, its own ids), squeezed across
  // as the wall turns away; out of either, a spin out to the tree house
  const win = (th, id, off) => { const [wx, wy] = G.at(th, 0.3), sc = ws * 0.55 * k * G.tall(th);
    return part("top-window" + id, [c.F, ws, fast, slow], () => `<g ${tf(wx, wy, sc * Math.max(0.4, G.s(th)), sc)}>` + roomWindowSvg(o, "round", id, off) + `</g>`); };
  s += SEP + wallObj("window, left", win(1.31 * PI, "-l", 0)) + SEP + wallObj("window, right", win(1.69 * PI, "-r", 0.5));
  // where the limbs meet the thatch: the holes the upper two go up through (dark, a sliver of daylight on each upper edge), the
  // notches the lower four rest in on the plate
  s += SEP + wallObj("limbs in the thatch", part("top-holes", c.F, () => LM.filter(L => L.kind !== "up").map(L => { const [ex, ey] = L.p2;
    if (L.kind === "plate") return `<ellipse cx="${f1(ex)}" cy="${f1(ey + L.w1 * 0.1)}" rx="${f1(L.w1 * 0.7)}" ry="${f1(L.w1 * 0.45)}" fill="#2a1a0e"/>`;
    const rx = L.w1 * 0.75 + 4 * k, ry = rx * 0.62;
    return `<ellipse cx="${f1(ex)}" cy="${f1(ey)}" rx="${f1(rx)}" ry="${f1(ry)}" fill="#2a1a0e"/>` +
      `<path d="M${f1(ex - rx * 0.85)} ${f1(ey - ry * 0.7)} Q${f1(ex)} ${f1(ey - ry * 1.25)} ${f1(ex + rx * 0.85)} ${f1(ey - ry * 0.7)}" fill="none" stroke="#fff3c4" stroke-width="${f1(2.5 * k)}" opacity="0.9"/>`; }).join("")));
  // the daylight: three pale shafts from the two holes and the brightest notch down to the floor, slanting outward, away
  // from the lanterns (hung nearer the trunk, so no shaft reads as a lantern's beam), each
  // breathing with the slow LFO (as the hut center's from its smoke hole); where each lands, and the windows' light, pale patches
  const shafts = LM.filter(L => L.kind === "hole" || L.j === 3).map((L, i) => { const [ex, ey] = L.p2, fx = ex * 1.3 + L.sd * 20 * k, fy = G.Yf + (24 + 16 * i) * k; return { ex, ey, fx, fy, i }; });
  s += SEP + wallObj("daylight", part("top-light", [c.F, slow], () => shafts.map(({ ex, ey, fx, fy, i }) =>
    `<polygon points="${P(ex - 9 * k, ey)} ${P(ex + 9 * k, ey)} ${P(fx + 36 * k, fy)} ${P(fx - 36 * k, fy)}" fill="#fff6c8" opacity="0.12" pointer-events="none">` +
    sceneLfo(o, "slow", 0.17 + 0.29 * i, "opacity", (v, amp) => (0.12 * (1 + (0.2 + 0.5 * amp) * v)).toFixed(3)) + `</polygon>`).join("")));
  s += SEP + floorObj("sunlight on the floor", part("top-patches", [c.F, ws], () => shafts.map(({ fx, fy }) => `<ellipse cx="${f1(fx)}" cy="${f1(fy)}" rx="${f1(40 * k)}" ry="${f1(10 * k)}" fill="#fff6c8" opacity="0.15" pointer-events="none"/>`).join("") +
    [-1, 1].map(u => `<ellipse cx="${f1(u * W * 0.2)}" cy="${f1(G.Yf + 40 * k)}" rx="${f1(56 * k * ws)}" ry="${f1(12 * k)}" fill="#fff6c8" opacity="0.1" pointer-events="none"/>`).join("")));
  // two lanterns hanging still from the lowest limbs, on cords (hutLanternSvg's), their glows breathing with the slow LFO
  for (const L of LM.slice(0, 2)) { const [lx, ly] = qPt(L, 0.35);
    s += SEP + obj(L.sd < 0 ? "lantern, left" : "lantern, right", G.Yf + 30 * k, part("top-lantern" + L.j, [c.F, slow], () => hutLanternSvg(c, lx, L.sd < 0 ? 0.6 : 0.1, ly + L.w0 * 0.3, ly + 70 * k))); }
  // the tree: the limbs that go back, and the one behind the trunk (j 6), drawn first, so they leave from behind it; the trunk
  // (marked: its box the braid's, and its footprint on the floor, an unseen oval, data-footprint); the limb over it (j 7);
  // a knot hole on its front (the squirrel's, an object of its own)
  const hole = [-tw * 0.12, G.Yt + 150 * k];
  s += SEP + obj("tree trunk", foot, part("top-tree", c.F, () => LM.filter(L => L.j !== 7).map(L => topLimbSvg(c, L)).join("") +
    `<g data-trunk="">` + braidSvg(0, y0 - pad, foot, tw, k, { strands: 5, taper: 0.5, from: TOP_IN.from }) + `</g>` +
    LM.filter(L => L.j === 7).map(L => topLimbSvg(c, L)).join("") +
    `<ellipse cx="${f1(hole[0])}" cy="${f1(hole[1])}" rx="${f1(17 * k)}" ry="${f1(22 * k)}" fill="#1f140b" stroke="#4e321c" stroke-width="${f1(4 * k)}"/>` +
    `<ellipse data-footprint="" cx="0" cy="${f1(foot)}" rx="${f1(TOP_IN.fp * k)}" ry="${f1(TOP_IN.fp * k * G.Df / G.R)}" fill="none"/>`));
  // the squirrel: peeking out of the knot hole with the quick LFO and ducking back behind its lower lip (alive: no solid)
  { const [sx, sy] = hole;
    s += SEP + obj("squirrel", foot + 1, part("top-squirrel", [c.F, fast], () =>
      `<g>${sceneLfo(o, "fast", 0.4, "transform", (v, amp) => `0 ${f1(10 * k * (1 + v) / 2)}`, `<animateTransform attributeName="transform" type="translate" values="0 ${f1(16 * k)};0 0;0 0;0 ${f1(16 * k)};0 ${f1(16 * k)}" keyTimes="0;0.15;0.45;0.6;1" dur="4.2s" repeatCount="indefinite"/>`, "translate")}` +
      `<ellipse cx="${f1(sx)}" cy="${f1(sy - 2 * k)}" rx="${f1(11 * k)}" ry="${f1(10 * k)}" fill="#b5652e"/><path d="M${P(sx - 9 * k, sy - 9 * k)} l${f1(2 * k)} ${f1(-9 * k)} l${f1(5 * k)} ${f1(7 * k)} M${P(sx + 9 * k, sy - 9 * k)} l${f1(-2 * k)} ${f1(-9 * k)} l${f1(-5 * k)} ${f1(7 * k)}" fill="#b5652e"/>` +
      `<circle cx="${f1(sx - 4 * k)}" cy="${f1(sy - 4 * k)}" r="${f1(2.2 * k)}" fill="#1f1a17"/><circle cx="${f1(sx + 4 * k)}" cy="${f1(sy - 4 * k)}" r="${f1(2.2 * k)}" fill="#1f1a17"/><ellipse cx="${f1(sx)}" cy="${f1(sy + 2 * k)}" rx="${f1(3 * k)}" ry="${f1(2 * k)}" fill="#e8c9a0"/></g>` +
      `<path d="M${P(sx - 19 * k, sy + 5 * k)} Q${P(sx, sy + 24 * k)} ${P(sx + 19 * k, sy + 5 * k)} L${P(sx + 21 * k, sy + 24 * k)} L${P(sx - 21 * k, sy + 24 * k)} Z" fill="#6e4a2c"/>` +
      `<path d="M${P(sx - 19 * k, sy + 5 * k)} Q${P(sx, sy + 24 * k)} ${P(sx + 19 * k, sy + 5 * k)}" fill="none" stroke="#4e321c" stroke-width="${f1(4 * k)}"/>`)); }
  // (ticket 517's furnishing, topFurniture: its pieces each an object at its foot's depth, kept clear of the arrival spot,
  // the hatch, the lever's post and a lane behind the trunk; none to start with. Drawn once with its settings: what moves in
  // it (the chime, the candle, the potion's bubbles) moves by the slow or quick LFO)
  s += SEP + part("top-furnish", [c.F, o.topFurnish ?? "none", sd, slow, fast, o.hutCouchColor, o.pillows, o.hutRugColors, o.toys, o.hutBlanket, o.hutPotion,
    o.healerCatFur, o.healerCatWhite, o.healerCatEyes], () => topFurniture(c, o).map(p => p.z === "wall" ? wallObj(p.name, p.svg()) : p.z === "floor" ? floorObj(p.name, p.svg()) : obj(p.name, p.z, p.svg())).join(""));
  // the hatch, at the front on the house below's side: its lid up, the hole (a solid), the ladder's top (climbed down);
  // shut, its lid across it and no way down. A tree room's hatch (hatchWaySvg), its sign toward the middle
  const shut = !!o.hatchShutTop, label = `${houseEmoji(o, n)} ${n}`, sh = openingShape(o.hatchShapeTop);
  s += SEP + obj("hatch", hy - 18 * k, part("top-hatch", [c.F, n, sd, sh, shut, label], () => (shut ? `<g data-shut="">` : `<g class="portal" data-to="treeroom" data-house="${n}">`) +
    hatchWaySvg(c, label, hx, hy, sh, shut, "bg-hatchtop", hx - sd * 112 * k) + `</g>`));
  // its lever, on a short post beside it (pulling one walks the friend to the floor under it: on the curved wall that spot
  // would be outside the room), its handle up while the trap door is open
  s += SEP + obj("hatch lever", pz, part("top-lever", [c.F, sd, shut, o.leverShine, o.leverShineColor, o.leverFlicker], () => { const py = pz - 70 * k, a = (shut ? -1 : 1) * LEVER_TILT;
    return `<g data-lever="H" style="cursor: pointer"><ellipse cx="${f1(px)}" cy="${f1(pz)}" rx="${f1(20 * k)}" ry="${f1(5 * k)}" fill="#000" opacity="0.2"/>` +
      `<rect x="${f1(px - 9 * k)}" y="${f1(py - 30 * k)}" width="${f1(18 * k)}" height="${f1(pz - py + 30 * k)}" rx="${f1(3 * k)}" fill="#7a5232" stroke="#4a2e18" stroke-width="${f1(2 * k)}"/>` +
      leverSvg(px - 13 * k, py - 20 * k, px, py, px, py - 44.4 * k, k, t => `<g class="lever-arm" data-pivot="${f1(px)} ${f1(py)}" transform="rotate(${a} ${f1(px)} ${f1(py)})">${t}</g>`, undefined, leverShine(o)) + `</g>`; }));
  // which room this is: a sign hanging still on two cords from the nearest limb on the right, a map of the tree on it, this
  // room ringed (treeMapSvg's "top")
  const looksKey = [o.houses, o.bridges, o.ladders, o.treeHouseSize, ...treeHouses(o, Math.max(1, Math.min(6, o.houses ?? 4)), 1).map(h => [h.animal, h.look.cols])];
  s += SEP + obj("house sign", Infinity, part("top-sign", [c.F, looksKey], () => {
    const hw = Math.max(46 * k, Math.min(76 * k, W * 0.14 - 4 * k)), sx = Math.min(x1 - hw - 8 * k, W * 0.43), L = LM[1];
    let u = 0.5; for (let i = 0; i <= 40; i++) if (qPt(L, i / 40)[0] <= sx) u = i / 40;
    const hookY = qPt(L, u)[1] + L.w0 * 0.25, sy = hookY + 34 * k, [bx, by, bw, bh] = TOP_MAP_BOX, m = (2 * hw - 14 * k) / bw, my = sy + 38 * k, mh = bh * m;
    return `<path d="M${P(sx - (hw - 16 * k), hookY)} L${P(sx - (hw - 16 * k), sy)} M${P(sx + hw - 16 * k, hookY)} L${P(sx + hw - 16 * k, sy)}" stroke="#d8c49a" stroke-width="${f1(2 * k)}"/>` +
      `<rect x="${f1(sx - hw)}" y="${f1(sy - 4 * k)}" width="${f1(2 * hw)}" height="${f1(49 * k + mh)}" rx="${f1(5 * k)}" fill="#a0703f" stroke="#5e3d22" stroke-width="${f1(2.5 * k)}"/>` +
      `<text x="${f1(sx)}" y="${f1(sy + 25 * k)}" text-anchor="middle" font-size="${f1(24 * k)}" font-weight="bold" fill="#fff8e7" font-family="sans-serif">⭕ top</text>` +
      `<rect x="${f1(sx - hw + 7 * k)}" y="${f1(my)}" width="${f1(2 * hw - 14 * k)}" height="${f1(mh)}" rx="${f1(3 * k)}" fill="#c99d63" stroke="#5e3d22" stroke-width="${f1(1.5 * k)}"/>` +
      `<g class="tree-map" ${tf(sx - hw + 7 * k - bx * m, my - by * m, m)}>${treeMapSvg(o, "top", TOP_MAP_BOX)}</g>`; }));
  // warm in the middle, dark toward the edge: the tree rooms' dim, over everything
  s += SEP + obj("dark", Infinity, part("top-dim", c.F, () => `<defs><radialGradient id="bg-top-dim" gradientUnits="userSpaceOnUse" cx="0" cy="${f1(y0 + H * 0.45)}" r="${f1(0.62 * Math.hypot(W, H))}">` +
    `<stop offset="0.5" stop-color="#1a0f05" stop-opacity="0"/><stop offset="1" stop-color="#1a0f05" stop-opacity="0.45"/></radialGradient></defs>` +
    c.all("url(#bg-top-dim)").replace("<rect ", `<rect pointer-events="none" `)));
  return s;
}
SEATS.treetop = (c, B) => topFurniture(c, B).filter(p => p.seat).map(p => p.seat);   // (ticket 517: its style's seats)
// Inside the witch's hut: log walls, herbs and a crooked shelf of potions,
// a cauldron bubbling over its fire, crows in hanging cages, frogs in jars,
// snakes in a glass tank, and the witch herself, who can be dragged about
// (data-drag: her place is the scene's witchX, witchY). Her door goes back
// out to her woods.
const WITCH_DEFS = `<g id="bg-crow"><path d="M-14 4 Q-4 -12 12 -6 L20 -9 L14 -2 Q8 10 -6 10 Z" fill="#1f1f22"/>` +
  `<circle cx="11" cy="-10" r="7" fill="#1f1f22"/><path d="M17 -12 L27 -9 L17 -7 Z" fill="#e0a800"/><circle cx="12" cy="-12" r="1.6" fill="#ffffff"/>` +
  `<path d="M-14 4 L-24 10 L-14 9 Z" fill="#1f1f22"/><path d="M-2 10 L-2 18 M4 10 L4 18" stroke="#e0a800" stroke-width="1.6"/></g>` +
  `<g id="bg-cage"><path d="M-26 30 V-6 Q-26 -34 0 -38 Q26 -34 26 -6 V30" fill="none" stroke="#b08a43" stroke-width="2"/>` +
  [-16, -6, 4, 14].map(x => `<path d="M${x} 30 V-6 Q${x} -30 0 -38" fill="none" stroke="#b08a43" stroke-width="1.5"/>`).join("") +
  `<ellipse cx="0" cy="30" rx="28" ry="6" fill="#6b4a2b"/><circle cx="0" cy="-42" r="5" fill="none" stroke="#b08a43" stroke-width="2"/></g>` +
  `<g id="bg-frogjar"><rect x="-17" y="-40" width="34" height="40" rx="6" fill="#cfe8f0" opacity="0.45" stroke="#e8f6fa" stroke-width="1.5"/>` +
  `<ellipse cx="0" cy="-10" rx="11" ry="8" fill="#4f9a3a"/><circle cx="-6" cy="-18" r="4" fill="#4f9a3a"/><circle cx="6" cy="-18" r="4" fill="#4f9a3a"/>` +
  `<circle cx="-6" cy="-19" r="1.8" fill="#111"/><circle cx="6" cy="-19" r="1.8" fill="#111"/><path d="M-5 -9 Q0 -6 5 -9" stroke="#2a5e22" stroke-width="1.2" fill="none"/>` +
  `<rect x="-19" y="-46" width="38" height="8" rx="2" fill="#7a4b28"/></g>`;
// the witch, in her own units, standing at (0, 0), about 300 tall
const WITCH_SVG = `<path d="M-10 -40 L-70 -230" stroke="#8b5e34" stroke-width="6" stroke-linecap="round"/>` +
  `<path d="M-76 -246 L-58 -236 L-90 -200 Z" fill="#c9a66b"/>` +
  `<ellipse cx="-18" cy="-4" rx="16" ry="7" fill="#1f1f22"/><ellipse cx="18" cy="-4" rx="16" ry="7" fill="#1f1f22"/>` +
  `<path d="M-30 -150 L-55 -8 L55 -8 L30 -150 Z" fill="#6a2c91"/><path d="M-55 -8 L-45 -18 L-35 -8 L-25 -18 L-15 -8 L-5 -18 L5 -8 L15 -18 L25 -8 L35 -18 L45 -8 L55 -8 Z" fill="#4a1d66"/>` +
  `<rect x="-30" y="-100" width="60" height="9" fill="#1f1f22"/><rect x="-7" y="-102" width="14" height="13" fill="none" stroke="#f2c230" stroke-width="3"/>` +
  `<path d="M-28 -146 Q-60 -120 -48 -80" fill="none" stroke="#6a2c91" stroke-width="18" stroke-linecap="round"/>` +
  `<path d="M28 -146 Q60 -150 72 -180" fill="none" stroke="#6a2c91" stroke-width="18" stroke-linecap="round"/>` +
  `<circle cx="-48" cy="-76" r="10" fill="#7cc25a"/><circle cx="76" cy="-186" r="10" fill="#7cc25a"/>` +
  `<path d="M-30 -200 Q-44 -150 -38 -120 M30 -200 Q44 -150 38 -120" stroke="#2b2b2b" stroke-width="12" stroke-linecap="round" fill="none"/>` +
  `<circle cx="0" cy="-185" r="30" fill="#7cc25a"/>` +
  `<path d="M2 -188 L34 -170 L4 -176 Z" fill="#6aa84a"/><circle cx="24" cy="-174" r="3" fill="#5a8a3a"/>` +
  `<ellipse cx="-12" cy="-194" rx="5" ry="6" fill="#ffffff"/><ellipse cx="12" cy="-196" rx="5" ry="6" fill="#ffffff"/>` +
  `<circle cx="-11" cy="-193" r="2.5" fill="#c0392b"/><circle cx="13" cy="-195" r="2.5" fill="#c0392b"/>` +
  `<path d="M-14 -168 Q0 -160 14 -170" stroke="#2a3a1a" stroke-width="3" fill="none"/>` +
  `<ellipse cx="0" cy="-212" rx="58" ry="11" fill="#1f1f22"/>` +
  `<path d="M-30 -214 Q-14 -260 8 -300 Q14 -290 34 -276 Q20 -268 28 -214 Z" fill="#1f1f22"/>` +
  `<rect x="-30" y="-232" width="58" height="10" fill="#6a2c91" transform="rotate(-3 0 -227)"/>`;
function witchroomScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, all, o } = c;
  const floor = g - 100 * k, cz = (o.cauldronSize ?? 100) / 100, wz = (o.witchSize ?? 100) / 100;
  const crows = o.crows ?? 3, frogs = o.frogs ?? 4, snakes = o.snakes ?? 2;
  // the hut: log walls, a beam with herbs, cobwebs, a round window with the moon
  let s = SEP + part("wroom-shell", [c.F, moonOfDateKey(o)], () => {   // (ticket 490: and its window's moon)
    let t = all("#3a2a3a");
    for (let i = 0; i < 16; i++) {
      const y = y0 - 10 * k + i * 30 * k;
      if (y > floor) break;
      t += `<rect x="${f1(x0 - pad)}" y="${f1(y)}" width="${f1(W + 2 * pad)}" height="${f1(27 * k)}" rx="${f1(13 * k)}" fill="${["#5a4048", "#4e3740", "#634850"][i % 3]}"/>`;
    }
    t += `<rect x="${f1(x0 - pad)}" y="${f1(floor)}" width="${f1(W + 2 * pad)}" height="${f1(y1 - floor + pad)}" fill="#3b2a22"/>`;
    for (let i = -14; i <= 14; i++)
      t += `<line x1="${f1(i * W / 20)}" y1="${f1(floor)}" x2="${f1(i * W / 6)}" y2="${f1(y1 + pad)}" stroke="#2a1d17" stroke-width="${f1(2 * k)}"/>`;
    const mRound = t.length;   // (ticket 284: its windows, the herbs on the beam, the cobwebs, the shelves of potions and of frogs, the crows in their cages: wall objects)
    // the round window, the moon in it
    const wx = 0, wy = y0 + H * 0.14;
    t += `<circle cx="${f1(wx)}" cy="${f1(wy)}" r="${f1(48 * k)}" fill="#1c2340"/>` + moonOfDateSvg(o, wx + 14 * k, wy - 10 * k, 18 * k, "#1c2340") +   // (ticket 490: the date's moon)
         `<circle cx="${f1(wx)}" cy="${f1(wy)}" r="${f1(48 * k)}" fill="none" stroke="#2a3a8a" stroke-width="${f1(9 * k)}"/><path d="M${P(wx - 48 * k, wy)} H${f1(wx + 48 * k)} M${P(wx, wy - 48 * k)} V${f1(wy + 48 * k)}" stroke="#2a3a8a" stroke-width="${f1(4 * k)}"/>`;
    t = wallSince(t, mRound, "round window");
    // a window either side of the door, as outside: blue frames, the dark woods beyond
    for (const sd of [-1, 1]) {
      const cx = sd * W * 0.24, top = y0 + H * 0.25, w = 92 * k, h = 104 * k;
      const mWin = t.length;
      t += `<rect x="${f1(cx - w / 2)}" y="${f1(top)}" width="${f1(w)}" height="${f1(h)}" fill="#3a2a5a"/>` +
           `<rect x="${f1(cx - w / 2)}" y="${f1(top + h * 0.55)}" width="${f1(w)}" height="${f1(h * 0.45)}" fill="#24304a"/>` +
           [-0.3, 0.05, 0.35].map((u, j) => `<polygon points="${P(cx + u * w - 12 * k, top + h)} ${P(cx + u * w, top + h * (0.2 + 0.12 * j))} ${P(cx + u * w + 12 * k, top + h)}" fill="#162418"/>`).join("") +
           `<circle cx="${f1(cx + sd * 22 * k)}" cy="${f1(top + 20 * k)}" r="${f1(2 * k)}" fill="#ffffff"/>` +
           `<rect x="${f1(cx - w / 2)}" y="${f1(top)}" width="${f1(w)}" height="${f1(h)}" fill="none" stroke="#2a3a8a" stroke-width="${f1(7 * k)}"/>` +
           `<path d="M${P(cx, top)} V${f1(top + h)}" stroke="#2a3a8a" stroke-width="${f1(4 * k)}"/>`;
      t = wallSince(t, mWin, sd < 0 ? "window, left" : "window, right");
    }
    // the beam across, herbs hanging from it, cobwebs in the corners
    const by = y0 + H * 0.02;
    t += `<rect x="${f1(x0 - pad)}" y="${f1(by)}" width="${f1(W + 2 * pad)}" height="${f1(16 * k)}" fill="#2a1a12"/>`;
    // (ticket 336, Matthew: "unnamed objects hanging in witches interior ... Let's say they are 'drying-herbs'") each bundle
    // an object of its own, "drying herbs 1" and on, left to right
    let herbN = 0;
    for (let i = 0; i < 9; i++) {
      const hx = x0 + W * (0.06 + 0.11 * i) + 10 * k * R(i, 300);
      if (Math.abs(hx) < 70 * k) continue;
      t += wallObj(`drying herbs ${++herbN}`, `<line x1="${f1(hx)}" y1="${f1(by + 14 * k)}" x2="${f1(hx)}" y2="${f1(by + 40 * k)}" stroke="#c9b27a" stroke-width="${f1(1.5 * k)}"/>` +
           [-8, 0, 8].map(dx => `<path d="M${P(hx, by + 40 * k)} Q${P(hx + dx * k, by + 60 * k)} ${P(hx + dx * 1.4 * k, by + 78 * k)}" stroke="${["#5f8a3a", "#7a6a3a", "#4a7a5a"][i % 3]}" stroke-width="${f1(4 * k)}" fill="none" stroke-linecap="round"/>`).join(""));
    }
    for (const [cx, sd] of [[x0, 1], [x1, -1]])
      t += wallObj(sd > 0 ? "cobweb, left" : "cobweb, right", [20, 40, 60].map(r => `<path d="M${P(cx, by + 16 * k + r * k)} Q${P(cx + sd * r * 0.5 * k, by + 16 * k + r * 0.5 * k)} ${P(cx + sd * r * k, by + 16 * k)}" fill="none" stroke="#d8d8e0" stroke-width="${f1(1.2 * k)}" opacity="0.7"/>`).join("") +
           [0.3, 0.6, 0.9].map(a => `<line x1="${f1(cx)}" y1="${f1(by + 16 * k)}" x2="${f1(cx + sd * 64 * k * Math.cos(a * Math.PI / 2))}" y2="${f1(by + 16 * k + 64 * k * Math.sin(a * Math.PI / 2))}" stroke="#d8d8e0" stroke-width="${f1(1.2 * k)}" opacity="0.7"/>`).join(""));
    const mPotions = t.length;
    // the crooked shelf of potions, high on the right
    const sx = W * 0.11, sy = y0 + H * 0.47;
    t += `<polygon points="${P(sx, sy)} ${P(sx + W * 0.26, sy - 8 * k)} ${P(sx + W * 0.26, sy)} ${P(sx, sy + 8 * k)}" fill="#2a1a12"/>`;
    for (let i = 0; i < 7; i++) {
      const bx = sx + 14 * k + i * W * 0.035, h = (26 + 20 * R(i, 301)) * k, cc = ["#7cc25a", "#c0392b", "#8e44ad", "#3fd0e0", "#f2c12e", "#e85d75", "#27ae60"][i];
      t += `<rect x="${f1(bx)}" y="${f1(sy - 2 * k - i * 1.1 * k - h)}" width="${f1(16 * k)}" height="${f1(h)}" rx="${f1(6 * k)}" fill="${cc}" opacity="0.85" stroke="#ffffff" stroke-width="${f1(k)}"/>` +
           `<rect x="${f1(bx + 4 * k)}" y="${f1(sy - 2 * k - i * 1.1 * k - h - 7 * k)}" width="${f1(8 * k)}" height="${f1(8 * k)}" fill="#7a4b28"/>`;
    }
    return wallSince(t, mPotions, "shelf of potions");
  });
  // the door back out to the woods, in the middle, as outside: red, braced
  s += SEP + part("wroom-door", c.F, () => {
    // (ticket 352: 242 tall, 196 till 0.0.302, which the default friend crawled out through: 1.10 of it; now 0.89, as the tree's)
    const w = 96 * k, top = floor - 242 * k;
    // (0.0.114, Matthew: her door no longer glowed or worked: the friend arrived in front of it, and took
    // the pointer; they arrive to one side now, ARRIVE.witchroom; and a door, data-door, so X glows it too)
    return `<g class="portal" data-to="witch" data-door="1"><rect x="${f1(-w / 2 - 8 * k)}" y="${f1(top - 8 * k)}" width="${f1(w + 16 * k)}" height="${f1(floor - top + 8 * k)}" fill="#2a1a12"/>` +
           `<rect x="${f1(-w / 2)}" y="${f1(top)}" width="${f1(w)}" height="${f1(floor - top)}" fill="#c0392b" stroke="#4a1410" stroke-width="${f1(3 * k)}"/>` +
           `<path d="M${P(-w / 2 + 10 * k, top + 12 * k)} L${P(w / 2 - 10 * k, floor - 12 * k)} M${P(w / 2 - 10 * k, top + 12 * k)} L${P(-w / 2 + 10 * k, floor - 12 * k)}" stroke="#4a1410" stroke-width="${f1(6 * k)}"/>` +
           `<circle cx="${f1(w / 2 - 16 * k)}" cy="${f1(floor - 92 * k)}" r="${f1(5 * k)}" fill="#f2c230"/>` +
           plankSign(0, top - 30 * k, k, "🌲 out") + `</g>`;
  });
  // frogs in jars on a shelf on the left
  s += wallObj("frogs in jars", part("wroom-frogs", [c.F, frogs], () => {
    const fx = -W * 0.37, fy = y0 + H * 0.47, n = Math.min(frogs, 8);
    return `<defs>${WITCH_DEFS}</defs><rect x="${f1(fx - 16 * k)}" y="${f1(fy)}" width="${f1(W * 0.26)}" height="${f1(9 * k)}" fill="#2a1a12"/>` +
           [...Array(n)].map((_, i) => use("bg-frogjar", fx + 8 * k + i * 26 * k, fy, k * 0.8)).join("");
  }));
  // crows in cages, hanging from the beam
  // (the beam runs along the top of the back wall, as the herbs on it do: the cages hang
  // at the wall; drawn before the objects, so always behind the friend; they cross none of them, the snake tank they
  // once came after included; since ticket 284 each a wall object, which keeps all of that)
  s += SEP + part("wroom-crows", [c.F, crows, sceneLfoKey(o, "slow")], () => {
    const by = y0 + H * 0.02 + 16 * k, spots = [-0.36, 0.3, -0.22, 0.18, -0.44, 0.42];
    return `<defs>${WITCH_DEFS}</defs>` + spots.slice(0, Math.min(crows, 6)).map((u, i) => {
      const x = W * u, hang = (60 + 30 * R(i, 302)) * k;
      // (each cage swinging on its chain with the slow LFO, its own way in)
      return wallObj(`crow in a cage ${i + 1}`, swaying(o, `<line x1="${f1(x)}" y1="${f1(by)}" x2="${f1(x)}" y2="${f1(by + hang)}" stroke="#555" stroke-width="${f1(1.5 * k)}"/>` +
             use("bg-crow", x, by + hang + 50 * k, k * 0.9 * (i % 2 ? -1 : 1), k * 0.9) + use("bg-cage", x, by + hang + 40 * k, k * 0.9), x, by, 4, R(i, 302), 5));
    }).join("");
  });
  // snakes in a glass tank on a table on the right
  // (the table and its tank an object, the table's legs down to ty + 82k, floor + 22k)
  s += SEP + obj("snake tank", floor + 22 * k, part("wroom-snakes", [c.F, snakes], () => {
    const tx = x1 - W * 0.12, ty = floor - 60 * k, tw = 130 * k, th = 80 * k;
    let t = `<rect x="${f1(tx - 90 * k)}" y="${f1(ty)}" width="${f1(180 * k)}" height="${f1(12 * k)}" fill="#3a2418"/>` +
            [-80, 80].map(dx => `<rect x="${f1(tx + dx * k - 5 * k)}" y="${f1(ty + 12 * k)}" width="${f1(10 * k)}" height="${f1(70 * k)}" fill="#2a1a12"/>`).join("") +
            `<rect x="${f1(tx - tw / 2)}" y="${f1(ty - th)}" width="${f1(tw)}" height="${f1(th)}" fill="#7a6a3a" opacity="0.35"/>`;
    for (let i = 0; i < Math.min(snakes, 6); i++) {
      const yy = ty - 12 * k - i * 11 * k, ph = i * 1.3, cc = ["#4f9a3a", "#c0392b", "#f2c12e", "#8e44ad", "#27ae60", "#e8762c"][i];
      let d = `M${P(tx - tw / 2 + 10 * k, yy)}`;
      for (let j = 1; j <= 12; j++) d += ` L${P(tx - tw / 2 + 10 * k + j * (tw - 30 * k) / 12, yy - 8 * k * Math.sin(j * 0.9 + ph))}`;
      t += `<path d="${d}" fill="none" stroke="${cc}" stroke-width="${f1(7 * k)}" stroke-linecap="round" stroke-linejoin="round"/>` +
           `<path d="${d}" fill="none" stroke="#1f1f22" stroke-width="${f1(2 * k)}" stroke-dasharray="${f1(3 * k)} ${f1(8 * k)}"/>` +
           `<circle cx="${f1(tx + tw / 2 - 20 * k)}" cy="${f1(yy - 8 * k * Math.sin(12 * 0.9 + ph))}" r="${f1(6 * k)}" fill="${cc}"/>`;
    }
    return t + `<rect x="${f1(tx - tw / 2)}" y="${f1(ty - th)}" width="${f1(tw)}" height="${f1(th)}" fill="#cfe8f0" opacity="0.18" stroke="#e8f6fa" stroke-width="${f1(2.5 * k)}"/>` +
               `<rect x="${f1(tx - tw / 2 - 4 * k)}" y="${f1(ty - th - 6 * k)}" width="${f1(tw + 8 * k)}" height="${f1(7 * k)}" fill="#3a2418"/>`;
  }));
  // the cauldron over its fire, bubbling, its size its group's
  // (an object with its fumes, standing where its group stands its fire, floor + 50k)
  s += obj("cauldron", floor + 50 * k, OPEN(`<g ${tf(-W * 0.22, floor + 50 * k, cz * k)}>`) + part("wroom-cauldron", [sceneLfoKey(o, "fast")], () =>
    `<path d="M-60 0 L60 0 M-50 -6 L50 6 M-50 6 L50 -6" stroke="#5a3a1a" stroke-width="10" stroke-linecap="round"/>` +
    [[-30, 0], [0, 6], [30, -4]].map(([fx, ph]) => `<path d="M${fx - 16} -4 Q${fx} -${46 + ph} ${fx + 16} -4 Z" fill="#ff8c1a">${lfoTimed(o, "fast", (fx + 30) / 90, `<animate attributeName="d" dur="${(0.6 + ph / 30).toFixed(2)}s" repeatCount="indefinite" ` +
      `values="M${fx - 16} -4 Q${fx} -${46 + ph} ${fx + 16} -4 Z;M${fx - 16} -4 Q${fx + 6} -${30 + ph} ${fx + 16} -4 Z;M${fx - 16} -4 Q${fx} -${46 + ph} ${fx + 16} -4 Z"/>`)}</path>` +
      `<path d="M${fx - 8} -4 Q${fx} -${26 + ph} ${fx + 8} -4 Z" fill="#ffd23f"/>`).join("") +
    `<path d="M-70 -40 Q-80 -130 0 -136 Q80 -130 70 -40 Q60 -20 0 -18 Q-60 -20 -70 -40 Z" fill="#1f1f22"/>` +
    `<path d="M-60 -24 L-70 -6 M60 -24 L70 -6" stroke="#1f1f22" stroke-width="8"/>` +
    `<ellipse cx="0" cy="-124" rx="74" ry="16" fill="#2b2b30"/><ellipse cx="0" cy="-122" rx="64" ry="11" fill="#7cfc3a"/>` +
    [[-30, 0.9], [-5, 1.3], [22, 1.1], [40, 0.7]].map(([bx, d]) => `<circle cx="${bx}" cy="-124" r="6" fill="#b6ff7a" opacity="0.9">` +
      `<animate attributeName="cy" values="-120;-150;-120" dur="${d}s" repeatCount="indefinite"/><animate attributeName="r" values="3;8;0" dur="${d}s" repeatCount="indefinite"/></circle>`).join("") +
    "") + CLOSE +
    // (ticket 206: simple unless upgraded in 🏞️ Background)
    SEP + part("wroom-fumes", [c.F, cz, o.cauldronFumes === "fancy"], () => (o.cauldronFumes === "fancy" ? helixSvg : helixCheapSvg)(-W * 0.22, floor + 50 * k - 128 * cz * k, y0 - 80 * k, 14 * cz * k, 50 * cz * k, k)));
  // the witch: dragged anywhere (her place), her size her group's
  // (an object at her feet, where her place puts them)
  s += obj("witch", g + (o.witchY ?? 0), OPEN(`<g data-drag="witch" style="cursor: grab" ${tf(o.witchX ?? 150, g + (o.witchY ?? 0), wz * k)}>`) + part("wroom-witch", [], () => WITCH_SVG) + CLOSE);
  // dim and glowing: darker toward the corners, a green glow over the cauldron
  // (over everything, the friend too: in front, at Infinity; the whole room's, so not the cauldron's)
  s += SEP + obj("dark", Infinity, part("wroom-dim", c.F, () => `<defs><radialGradient id="bg-wdim" gradientUnits="userSpaceOnUse" cx="${f1(-W * 0.2)}" cy="${f1(floor - 40 * k)}" r="${f1(0.75 * Math.hypot(W, H))}">` +
    `<stop offset="0" stop-color="#7cfc3a" stop-opacity="0.12"/><stop offset="0.35" stop-color="#140a14" stop-opacity="0"/><stop offset="1" stop-color="#140a14" stop-opacity="0.55"/></radialGradient></defs>` +
    all("url(#bg-wdim)").replace("<rect ", `<rect pointer-events="none" `)));
  return s;
}
// The healer: a friendly woman in a green dress and a cream apron, a red
// heart on its pocket (the heart on her hut's sign), long auburn hair with
// flowers in it by her ears, rosy cheeks, smiling. She sways gently from
// side to side, and stirs her soup with a long wooden spoon in her right
// hand (on the picture's left), its end at (-110, -48), in the pot. In her
// own units, her feet at (0, 0); drawn 1.3 times as big (HEALER_Z).
// (her sway, timed by the scene's slow LFO in the healer's hut: lfoTimed)
const HEALER_SWAY = `<animateTransform attributeName="transform" type="rotate" values="-2.5 0 0;2.5 0 0;-2.5 0 0" dur="4.6s" calcMode="spline" keySplines="0.45 0 0.55 1;0.45 0 0.55 1" repeatCount="indefinite"/>`;
const HEALER_SVG = (() => {
  const skin = "#e2a67c", hair = "#8a4a2a", dress = "#5f9e6e", apron = "#f6eedd", dk = darker(dress), sk = darker(skin);
  const flower = (x, y, petal) => [0, 72, 144, 216, 288].map(a => { const r = a * Math.PI / 180;
      return `<ellipse cx="${f1(x + 4.6 * Math.cos(r))}" cy="${f1(y + 4.6 * Math.sin(r))}" rx="4.6" ry="3.4" transform="rotate(${a} ${f1(x + 4.6 * Math.cos(r))} ${f1(y + 4.6 * Math.sin(r))})" fill="${petal}" stroke="${darker(petal)}" stroke-width="0.8"/>`; }).join("") +
    `<circle cx="${x}" cy="${y}" r="2.8" fill="#facc15" stroke="#ca8a04" stroke-width="0.8"/>`;
  const leaf = (x, y, a) => `<ellipse cx="${x}" cy="${y}" rx="6" ry="2.6" transform="rotate(${a} ${x} ${y})" fill="#4a9a52" stroke="#2f6e3a" stroke-width="0.8"/>`;
  const limb = (pts, w, c) => `<path d="M${pts.map(p => p.join(" ")).join(" L")}" fill="none" stroke="${darker(c)}" stroke-width="${w + 3}" stroke-linecap="round" stroke-linejoin="round"/>` +
                              `<path d="M${pts.map(p => p.join(" ")).join(" L")}" fill="none" stroke="${c}" stroke-width="${w}" stroke-linecap="round" stroke-linejoin="round"/>`;
  const body =
    // her long hair, behind her
    `<path d="M-34 -205 Q-48 -160 -45 -112 Q-32 -102 -20 -110 L20 -110 Q32 -102 45 -112 Q48 -160 34 -205 Q0 -234 -34 -205 Z" fill="${hair}" stroke="${darker(hair)}" stroke-width="2"/>` +
    // her shoes, her dress, her apron with its pocket and heart
    `<ellipse cx="-16" cy="-4" rx="13" ry="6" fill="#6b4226"/><ellipse cx="16" cy="-4" rx="13" ry="6" fill="#6b4226"/>` +
    `<path d="M-26 -150 Q-30 -120 -40 -80 Q-52 -40 -60 -12 Q-30 -4 0 -7 Q30 -4 60 -12 Q52 -40 40 -80 Q30 -120 26 -150 Q0 -158 -26 -150 Z" fill="${dress}" stroke="${dk}" stroke-width="3" stroke-linejoin="round"/>` +
    `<path d="M-22 -112 Q-27 -70 -35 -30 Q0 -23 35 -30 Q27 -70 22 -112 Q0 -116 -22 -112 Z" fill="${apron}" stroke="#d9cbb0" stroke-width="2"/>` +
    `<rect x="-28" y="-117" width="56" height="7" rx="3" fill="${apron}" stroke="#d9cbb0" stroke-width="1.5"/>` +
    `<rect x="-13" y="-74" width="26" height="20" rx="4" fill="none" stroke="#d9cbb0" stroke-width="2"/>` +
    `<path d="M0 -58 L-7 -65 A3.6 3.6 0 0 1 0 -69.5 A3.6 3.6 0 0 1 7 -65 Z" fill="#e53935"/>` +
    // her left arm (the picture's right), its hand on her apron
    limb([[24, -146], [38, -114]], 12, dress) + limb([[38, -114], [20, -97]], 9, skin) + `<circle cx="19" cy="-96" r="7" fill="${skin}" stroke="${sk}" stroke-width="1.5"/>` +
    // her neck and head, her face: kind eyes, rosy cheeks, a smile
    `<rect x="-7" y="-166" width="14" height="18" fill="${skin}"/>` +
    `<circle cx="0" cy="-188" r="30" fill="${skin}" stroke="${sk}" stroke-width="2"/>` +
    [-1, 1].map(e => `<ellipse cx="${e * 11}" cy="-191" rx="4" ry="5" fill="#2b1d14"/><circle cx="${e * 11 + 1.5}" cy="-193" r="1.5" fill="#ffffff"/>` +
      `<path d="M${e * 17} -202 Q${e * 11} -207 ${e * 5} -203" fill="none" stroke="${darker(hair)}" stroke-width="2.5" stroke-linecap="round"/>` +
      `<circle cx="${e * 18}" cy="-179" r="6" fill="#f28b8b" opacity="0.45"/>`).join("") +
    `<path d="M-2 -185 Q0 -182 2 -185" fill="none" stroke="${sk}" stroke-width="1.5" stroke-linecap="round"/>` +
    `<path d="M-12 -176 Q0 -162 12 -176 Q0 -169 -12 -176 Z" fill="#9b3b3b" stroke="#6b2b2b" stroke-width="1.5" stroke-linejoin="round"/>` +
    // her hair over her head: a side-swept fringe, locks down past her cheeks
    `<path d="M-31 -192 Q-35 -224 0 -223 Q35 -224 31 -192 Q26 -206 12 -209 Q2 -201 -8 -210 Q-24 -207 -31 -192 Z" fill="${hair}" stroke="${darker(hair)}" stroke-width="1.5"/>` +
    [-1, 1].map(e => `<path d="M${e * 30} -197 Q${e * 38} -162 ${e * 35} -128" fill="none" stroke="${hair}" stroke-width="9" stroke-linecap="round"/>`).join("") +
    // flowers in her hair, by her ears, with leaves
    leaf(-36, -172, 30) + leaf(-28, -170, -40) + flower(-31, -178, "#f472b6") +
    leaf(36, -174, -30) + leaf(28, -171, 40) + flower(31, -180, "#ffffff") + flower(36, -190, "#c4b5fd");
  // her right arm (the picture's left), stirring: the arm and the spoon
  // turn a little about her shoulder, back and forth
  const stir = `<g><animateTransform attributeName="transform" type="rotate" values="0 -24 -146;-6 -24 -146;0 -24 -146;5 -24 -146;0 -24 -146" dur="2.6s" repeatCount="indefinite"/>` +
    `<line x1="-56" y1="-120" x2="-110" y2="-48" stroke="#7a5228" stroke-width="7" stroke-linecap="round"/>` +
    `<line x1="-56" y1="-120" x2="-110" y2="-48" stroke="#c08a50" stroke-width="4" stroke-linecap="round"/>` +
    limb([[-24, -146], [-44, -118]], 12, dress) + limb([[-44, -118], [-64, -106]], 9, skin) +
    `<circle cx="-65" cy="-105" r="7.5" fill="${skin}" stroke="${sk}" stroke-width="1.5"/></g>`;
  // and the whole of her sways, slowly, about her feet
  return `<g>${HEALER_SWAY}${body}${stir}</g>`;
})();
const HEALER_Z = 1.3;
// Bottles of potions, neatly kept: each shelf one kind of bottle, their
// colors in rainbow order along it, each corked and labelled
const POTION_COLORS = ["#e53935", "#f57c00", "#fbc02d", "#7cb342", "#26a69a", "#1e88e5", "#5e35b1", "#d81b60"];
function potionSvg(kind, x, y, k, color) {
  const cork = `fill="#9a6b3f" stroke="#6e4a28" stroke-width="${f1(0.8 * k)}"`, glass = `fill="${color}" fill-opacity="0.85" stroke="${darker(color)}" stroke-width="${f1(1.2 * k)}"`;
  const shine = (x0, y0, h) => `<line x1="${f1(x0)}" y1="${f1(y0)}" x2="${f1(x0)}" y2="${f1(y0 + h)}" stroke="#ffffff" stroke-opacity="0.55" stroke-width="${f1(2 * k)}" stroke-linecap="round"/>`;
  const label = (cy, w) => `<rect x="${f1(x - w / 2)}" y="${f1(cy - 4 * k)}" width="${f1(w)}" height="${f1(8 * k)}" rx="${f1(1.5 * k)}" fill="#f6eedd"/>` +
                           `<line x1="${f1(x - w / 3)}" y1="${f1(cy)}" x2="${f1(x + w / 3)}" y2="${f1(cy)}" stroke="#8a7a5a" stroke-width="${f1(0.9 * k)}"/>`;
  if (kind === "flask")   // a round flask with a long neck
    return `<rect x="${f1(x - 3 * k)}" y="${f1(y - 34 * k)}" width="${f1(6 * k)}" height="${f1(14 * k)}" ${glass}/><circle cx="${f1(x)}" cy="${f1(y - 11 * k)}" r="${f1(11 * k)}" ${glass}/>` +
           `<rect x="${f1(x - 3.5 * k)}" y="${f1(y - 39 * k)}" width="${f1(7 * k)}" height="${f1(6 * k)}" rx="${f1(1.5 * k)}" ${cork}/>` + shine(x - 5 * k, y - 17 * k, 7 * k);
  if (kind === "tall")    // a tall slim bottle
    return `<path d="M${f1(x - 6 * k)} ${f1(y)} V${f1(y - 30 * k)} Q${f1(x - 6 * k)} ${f1(y - 36 * k)} ${f1(x - 2.5 * k)} ${f1(y - 38 * k)} V${f1(y - 44 * k)} H${f1(x + 2.5 * k)} V${f1(y - 38 * k)} Q${f1(x + 6 * k)} ${f1(y - 36 * k)} ${f1(x + 6 * k)} ${f1(y - 30 * k)} V${f1(y)} Z" ${glass}/>` +
           `<rect x="${f1(x - 3 * k)}" y="${f1(y - 49 * k)}" width="${f1(6 * k)}" height="${f1(6 * k)}" rx="${f1(1.5 * k)}" ${cork}/>` + label(y - 16 * k, 10 * k) + shine(x - 3.5 * k, y - 30 * k, 10 * k);
  if (kind === "jar")     // a square jar, a wide cork, a label
    return `<rect x="${f1(x - 9 * k)}" y="${f1(y - 26 * k)}" width="${f1(18 * k)}" height="${f1(26 * k)}" rx="${f1(3 * k)}" ${glass}/>` +
           `<rect x="${f1(x - 7 * k)}" y="${f1(y - 31 * k)}" width="${f1(14 * k)}" height="${f1(6 * k)}" rx="${f1(1.5 * k)}" ${cork}/>` + label(y - 13 * k, 14 * k) + shine(x - 6 * k, y - 23 * k, 8 * k);
  // a small vial
  return `<rect x="${f1(x - 4 * k)}" y="${f1(y - 20 * k)}" width="${f1(8 * k)}" height="${f1(20 * k)}" rx="${f1(4 * k)}" ${glass}/>` +
         `<rect x="${f1(x - 3 * k)}" y="${f1(y - 24 * k)}" width="${f1(6 * k)}" height="${f1(5 * k)}" rx="${f1(1.2 * k)}" ${cork}/>` + shine(x - 1.5 * k, y - 17 * k, 6 * k);
}
// Inside the healer's hut: warm. Round cream walls under a thatched roof,
// herbs drying from the beam, a round window full of sun; a stone hearth
// with a fire, and on it a big pot of soup, steaming, that the healer
// stirs; shelves and shelves of potions, neatly kept, her cat up on the top
// one, its tail hanging down and slowly swinging; a worktable of tools,
// herbs and bowls; a little table with two chairs and tea, to sit and
// talk; a braided rug; and the door back out to the clearing. She can be
// dragged anywhere (her place is the scene's healerX, healerY), at her size.
// (ticket 50) the tea table's two chairs: each seat's top, 52 across at ty + 4
// (the chairs at the table's sides, 92 out, ty = g + 34, as healerroomScene
// draws them), each facing the table, its back on the far side
SEATS.healerroom = c => { const { W, g, k } = c, tx = W * 0.13, ty = g + 34 * k;
  return [[-1, "chair, left", "right"], [1, "chair, right", "left"]].map(([sd, name, way]) => { const x = tx + sd * 92 * k;
    return { name, x0: x - 26 * k, x1: x + 26 * k, y: ty + 4 * k, z: ty + 44 * k + 1, facing: [way] }; }); };
// (ticket 517: the healer's shelves of potions, posts at sx0 and sx1, the top shelf at top, standing on floor; and her cat
// sitting up on the top one: [shelves, cat])
function potionShelvesSvg(c, sx0, sx1, top, floor, cat) {
  const { k } = c, n = 5, gap = (floor - 20 * k - top) / (n - 0.3);
  let t = [sx0, sx1].map(x => `<rect x="${f1(x - 7 * k)}" y="${f1(top - 50 * k)}" width="${f1(14 * k)}" height="${f1(floor - top + 50 * k)}" fill="#8b5e34" stroke="#6e4a28" stroke-width="${f1(1.5 * k)}"/>`).join("");
  const kinds = ["flask", "tall", "jar", "vial", "jar"];
  for (let r = 0; r < n; r++) {
    const sy = top + r * gap;
    t += `<rect x="${f1(sx0 - 10 * k)}" y="${f1(sy)}" width="${f1(sx1 - sx0 + 20 * k)}" height="${f1(9 * k)}" fill="#a0703f" stroke="#6e4a28" stroke-width="${f1(1.5 * k)}"/>`;
    // (the top shelf keeps room at its left end for the cat)
    const from = sx0 + (r === 0 ? 70 : 20) * k, to = sx1 - 18 * k, kind = kinds[r], step = kind === "vial" ? 18 * k : kind === "tall" ? 22 * k : 28 * k;
    const m = Math.floor((to - from) / step) + 1;
    for (let i = 0; i < m; i++) t += potionSvg(kind, from + i * (to - from) / Math.max(1, m - 1), sy, k * (kind === "jar" && r === 4 ? 1.25 : 1), POTION_COLORS[Math.floor(i * POTION_COLORS.length / m)]);
  }
  return [t, `<g transform="translate(${f1(sx0 + 32 * k)} ${f1(top)}) scale(${(1.15 * k).toFixed(3)})">${shoulderCat(4, cat, true)}</g>`];
}
// (ticket 517: the healer's round tea table, its middle at tx, its chairs either side, 92 out, ty their seats' height:
// [the left chair, the right one, the table and its tea])
function teaSetSvg(c, tx, ty) {
  const { k, P } = c;
  const chair = (x, sd) => `<path d="M${P(x + sd * 22 * k, ty - 66 * k)} V${f1(ty + 40 * k)}" stroke="#7a4b28" stroke-width="${f1(6 * k)}" stroke-linecap="round"/>` +
    [0.2, 0.45].map(u => `<line x1="${f1(x + sd * 22 * k)}" y1="${f1(ty - 66 * k + u * 60 * k)}" x2="${f1(x + sd * 22 * k)}" y2="${f1(ty - 66 * k + u * 60 * k)}" stroke="#7a4b28"/>`).join("") +
    `<rect x="${f1(x - 26 * k)}" y="${f1(ty + 4 * k)}" width="${f1(52 * k)}" height="${f1(8 * k)}" rx="${f1(3 * k)}" fill="#a0703f" stroke="#6e4a28" stroke-width="${f1(1.5 * k)}"/>` +
    `<rect x="${f1(x + sd * 22 * k - (sd > 0 ? 6 : 0) * k)}" y="${f1(ty - 64 * k)}" width="${f1(6 * k)}" height="${f1(70 * k)}" fill="#8b5e34"/>` +
    [[-22, 1], [22, 1]].map(([dx]) => `<line x1="${f1(x + dx * k)}" y1="${f1(ty + 12 * k)}" x2="${f1(x + dx * k)}" y2="${f1(ty + 44 * k)}" stroke="#6e4a28" stroke-width="${f1(5 * k)}"/>`).join("") +
    `<rect x="${f1(x + sd * 22 * k - (sd > 0 ? 9 : -3) * k)}" y="${f1(ty - 70 * k)}" width="${f1(6 * k)}" height="${f1(4 * k)}" fill="#6e4a28"/>`;
  return [chair(tx - 92 * k, -1), chair(tx + 92 * k, 1),
    `<rect x="${f1(tx - 5 * k)}" y="${f1(ty - 10 * k)}" width="${f1(10 * k)}" height="${f1(52 * k)}" fill="#6e4a28"/><ellipse cx="${f1(tx)}" cy="${f1(ty + 42 * k)}" rx="${f1(24 * k)}" ry="${f1(6 * k)}" fill="#6e4a28"/>` +
      `<ellipse cx="${f1(tx)}" cy="${f1(ty - 10 * k)}" rx="${f1(62 * k)}" ry="${f1(14 * k)}" fill="#a0703f" stroke="#6e4a28" stroke-width="${f1(2 * k)}"/>` +
      // a teapot, two cups on saucers, a little vase of flowers
      `<ellipse cx="${f1(tx - 6 * k)}" cy="${f1(ty - 26 * k)}" rx="${f1(16 * k)}" ry="${f1(13 * k)}" fill="#5f9e6e" stroke="#3f7a4e" stroke-width="${f1(1.5 * k)}"/>` +
      `<path d="M${P(tx + 9 * k, ty - 28 * k)} q${f1(12 * k)} ${f1(-2 * k)} ${f1(16 * k)} ${f1(-12 * k)}" fill="none" stroke="#5f9e6e" stroke-width="${f1(4 * k)}" stroke-linecap="round"/>` +
      `<path d="M${P(tx - 21 * k, ty - 32 * k)} q${f1(-9 * k)} ${f1(4 * k)} 0 ${f1(12 * k)}" fill="none" stroke="#5f9e6e" stroke-width="${f1(3 * k)}"/><circle cx="${f1(tx - 6 * k)}" cy="${f1(ty - 40 * k)}" r="${f1(3 * k)}" fill="#3f7a4e"/>` +
      [-40, 34].map(dx => `<ellipse cx="${f1(tx + dx * k)}" cy="${f1(ty - 14 * k)}" rx="${f1(10 * k)}" ry="${f1(3 * k)}" fill="#f6eedd" stroke="#d9cbb0" stroke-width="${f1(k)}"/>` +
        `<path d="M${P(tx + dx * k - 6 * k, ty - 24 * k)} V${f1(ty - 16 * k)} Q${P(tx + dx * k, ty - 13 * k)} ${P(tx + dx * k + 6 * k, ty - 16 * k)} V${f1(ty - 24 * k)} Z" fill="#ffffff" stroke="#d9cbb0" stroke-width="${f1(k)}"/>` +
        `<ellipse cx="${f1(tx + dx * k)}" cy="${f1(ty - 24 * k)}" rx="${f1(6 * k)}" ry="${f1(2 * k)}" fill="#b5834a"/>`).join("") +
      `<rect x="${f1(tx + 14 * k)}" y="${f1(ty - 30 * k)}" width="${f1(8 * k)}" height="${f1(16 * k)}" rx="${f1(3 * k)}" fill="#9ccfe0" stroke="#6aa8c0" stroke-width="${f1(k)}"/>` +
      [[-4, -42, "#f472b6"], [3, -46, "#facc15"], [8, -40, "#ffffff"]].map(([dx, dy, cc]) => `<line x1="${f1(tx + 18 * k)}" y1="${f1(ty - 30 * k)}" x2="${f1(tx + 18 * k + dx * k)}" y2="${f1(ty + dy * k + 2 * k)}" stroke="#4a9a52" stroke-width="${f1(1.5 * k)}"/><circle cx="${f1(tx + 18 * k + dx * k)}" cy="${f1(ty + dy * k)}" r="${f1(4 * k)}" fill="${cc}"/>`).join("")];
}
// (ticket 517: the healer's worktable, its middle at tx, its top at ty, its legs down to ty + 64k, and all on it)
function worktableSvg(c, tx, ty) {
  const { k, P } = c, tw = 190 * k;
  let t = [-0.42, 0.42].map(u => `<rect x="${f1(tx + u * tw - 6 * k)}" y="${f1(ty)}" width="${f1(12 * k)}" height="${f1(64 * k)}" fill="#6e4a28"/>`).join("") +
          `<rect x="${f1(tx - tw / 2)}" y="${f1(ty - 12 * k)}" width="${f1(tw)}" height="${f1(14 * k)}" rx="${f1(3 * k)}" fill="#a0703f" stroke="#6e4a28" stroke-width="${f1(2 * k)}"/>`;
  const at = u => tx + u * tw / 2, b = ty - 12 * k;
  // the mortar and pestle
  t += `<path d="M${P(at(-0.8) - 18 * k, b - 22 * k)} Q${P(at(-0.8), b + 4 * k)} ${P(at(-0.8) + 18 * k, b - 22 * k)} Z" fill="#9a9488" stroke="#6b665c" stroke-width="${f1(1.5 * k)}"/>` +
       `<ellipse cx="${f1(at(-0.8))}" cy="${f1(b - 22 * k)}" rx="${f1(18 * k)}" ry="${f1(5 * k)}" fill="#7a756a"/>` +
       `<line x1="${f1(at(-0.8) + 4 * k)}" y1="${f1(b - 22 * k)}" x2="${f1(at(-0.8) + 16 * k)}" y2="${f1(b - 46 * k)}" stroke="#b0aa9c" stroke-width="${f1(6 * k)}" stroke-linecap="round"/>`;
  // bowls of powders and petals
  t += [[-0.45, "#f2c12e"], [-0.18, "#c0392b"], [0.12, "#7cb342"]].map(([u, cc]) =>
    `<path d="M${P(at(u) - 16 * k, b - 12 * k)} Q${P(at(u), b + 2 * k)} ${P(at(u) + 16 * k, b - 12 * k)} Z" fill="#f6eedd" stroke="#c9b48a" stroke-width="${f1(1.2 * k)}"/>` +
    `<ellipse cx="${f1(at(u))}" cy="${f1(b - 12 * k)}" rx="${f1(14 * k)}" ry="${f1(4 * k)}" fill="${cc}"/>`).join("");
  // herb bundles lying on it
  t += [[0.36, "#5f8a3a"], [0.52, "#a78bfa"]].map(([u, cc]) => [-6, -2, 2, 6].map(d =>
    `<line x1="${f1(at(u) - 22 * k)}" y1="${f1(b - 4 * k + d * 0.3 * k)}" x2="${f1(at(u) + 18 * k)}" y2="${f1(b - 6 * k + d * k)}" stroke="${cc}" stroke-width="${f1(3 * k)}" stroke-linecap="round"/>`).join("") +
    `<rect x="${f1(at(u) - 16 * k)}" y="${f1(b - 8 * k)}" width="${f1(5 * k)}" height="${f1(7 * k)}" fill="#c9b27a"/>`).join("");
  // scissors and a knife
  t += [-1, 1].map(e => `<circle cx="${f1(at(0.74) - 8 * k)}" cy="${f1(b - 4 * k + e * 4 * k)}" r="${f1(4 * k)}" fill="none" stroke="#c0392b" stroke-width="${f1(2 * k)}"/>`).join("") +
       `<path d="M${P(at(0.74) - 4 * k, b - 6 * k)} L${P(at(0.74) + 16 * k, b - 3 * k)} M${P(at(0.74) - 4 * k, b - 2 * k)} L${P(at(0.74) + 16 * k, b - 5 * k)}" stroke="#9aa0aa" stroke-width="${f1(2 * k)}"/>` +
       `<line x1="${f1(at(0.9) - 14 * k)}" y1="${f1(b - 3 * k)}" x2="${f1(at(0.9) - 2 * k)}" y2="${f1(b - 3 * k)}" stroke="#6e4a28" stroke-width="${f1(4 * k)}" stroke-linecap="round"/>` +
       `<path d="M${P(at(0.9) - 2 * k, b - 5 * k)} L${P(at(0.9) + 18 * k, b - 3 * k)} L${P(at(0.9) - 2 * k, b - 1 * k)} Z" fill="#c9ced6"/>`;
  // a little brass balance, high at the back
  const bx = at(-0.02), bb = b - 2 * k;
  t += `<line x1="${f1(bx)}" y1="${f1(bb)}" x2="${f1(bx)}" y2="${f1(bb - 44 * k)}" stroke="#c99a2e" stroke-width="${f1(3 * k)}"/>` +
       `<line x1="${f1(bx - 26 * k)}" y1="${f1(bb - 44 * k)}" x2="${f1(bx + 26 * k)}" y2="${f1(bb - 44 * k)}" stroke="#c99a2e" stroke-width="${f1(3 * k)}"/>` +
       [-1, 1].map(e => `<line x1="${f1(bx + e * 26 * k)}" y1="${f1(bb - 44 * k)}" x2="${f1(bx + e * 26 * k)}" y2="${f1(bb - 26 * k)}" stroke="#c99a2e" stroke-width="${f1(1 * k)}"/>` +
         `<path d="M${P(bx + e * 26 * k - 10 * k, bb - 26 * k)} Q${P(bx + e * 26 * k, bb - 18 * k)} ${P(bx + e * 26 * k + 10 * k, bb - 26 * k)} Z" fill="#e0b84a" stroke="#a87a1e" stroke-width="${f1(k)}"/>`).join("") +
       `<ellipse cx="${f1(bx)}" cy="${f1(bb)}" rx="${f1(10 * k)}" ry="${f1(3 * k)}" fill="#c99a2e"/>`;
  return t;
}
function healerroomScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, all, o } = c;
  const floor = g - 100 * k, hz = (o.healerSize ?? 100) / 100, potX = -W * 0.24, wallTop = y0 + H * 0.06;
  // the room: thatch above, cream walls, posts, a beam with herbs, the floor and its rug
  let s = SEP + part("hroom-shell", c.F, () => {
    let t = all("#ecd9b4");
    // the thatch: straw, its rafters running up to the roof's peak, high above
    t += `<rect x="${f1(x0 - pad)}" y="${f1(y0 - pad)}" width="${f1(W + 2 * pad)}" height="${f1(wallTop - y0 + pad)}" fill="#c9a35a"/>`;
    for (let i = -12; i <= 12; i++)
      t += `<line x1="${f1(i * W / 40)}" y1="${f1(y0 - H * 0.6)}" x2="${f1(i * W / 10)}" y2="${f1(wallTop)}" stroke="${i % 3 ? "#a8823a" : "#7a5a2a"}" stroke-width="${f1((i % 3 ? 1.5 : 5) * k)}"/>`;
    // the walls: daub, a little uneven, posts at intervals
    for (let i = 0; i < 26; i++)
      t += `<ellipse cx="${f1(x0 + W * R(i, 1101))}" cy="${f1(wallTop + (floor - wallTop) * R(i, 1102))}" rx="${f1((30 + 40 * R(i, 1103)) * k)}" ry="${f1((10 + 12 * R(i, 1104)) * k)}" fill="#e2cca2" opacity="0.5"/>`;
    for (const u of [-0.5, -0.36, 0.5, 0.36])
      t += `<rect x="${f1(W * u - 9 * k)}" y="${f1(wallTop)}" width="${f1(18 * k)}" height="${f1(floor - wallTop)}" fill="#8b5e34" stroke="#6e4a28" stroke-width="${f1(1.5 * k)}"/>`;
    t += `<rect x="${f1(x0 - pad)}" y="${f1(wallTop - 4 * k)}" width="${f1(W + 2 * pad)}" height="${f1(16 * k)}" fill="#6e4a28"/>`;
    // the floor: warm boards, and a round braided rug
    t += `<rect x="${f1(x0 - pad)}" y="${f1(floor)}" width="${f1(W + 2 * pad)}" height="${f1(y1 - floor + pad)}" fill="#a8743f"/>`;
    for (let i = -14; i <= 14; i++)
      t += `<line x1="${f1(i * W / 20)}" y1="${f1(floor)}" x2="${f1(i * W / 6)}" y2="${f1(y1 + pad)}" stroke="#8a5a2e" stroke-width="${f1(2 * k)}"/>`;
    const mRug = t.length;   // (ticket 284: the rug, the round window, the herbs, the fireplace and what is on its mantel: wall and floor objects)
    t += [["#c0392b", 1], ["#e8a33a", 0.82], ["#f3d27a", 0.66], ["#7a9a3a", 0.5], ["#c0392b", 0.34], ["#f3d27a", 0.18]].map(([cc, u]) =>
      `<ellipse cx="${f1(W * 0.02)}" cy="${f1(g + 46 * k)}" rx="${f1(200 * k * u)}" ry="${f1(44 * k * u)}" fill="${cc}"/>`).join("");
    t = floorSince(t, mRug, "rug");
    const mRound = t.length;
    // the round window, high in the middle: sun, a green hill, flowers; its beam on the floor
    const wx = 0, wy = y0 + H * 0.17, wr = 46 * k;
    t += `<polygon points="${P(wx - 40 * k, wy + 20 * k)} ${P(wx + 40 * k, wy + 20 * k)} ${P(wx + 230 * k, g + 60 * k)} ${P(wx + 30 * k, g + 70 * k)}" fill="#fff3c4" opacity="0.2"/>` +
         `<defs><clipPath id="bg-hwin"><circle cx="${f1(wx)}" cy="${f1(wy)}" r="${f1(wr)}"/></clipPath></defs>` +
         `<g clip-path="url(#bg-hwin)"><rect x="${f1(wx - wr)}" y="${f1(wy - wr)}" width="${f1(2 * wr)}" height="${f1(2 * wr)}" fill="#bfe6ff"/>` +
         `<circle cx="${f1(wx + 20 * k)}" cy="${f1(wy - 22 * k)}" r="${f1(12 * k)}" fill="#ffd23f"/>` +
         `<ellipse cx="${f1(wx - 10 * k)}" cy="${f1(wy + 46 * k)}" rx="${f1(80 * k)}" ry="${f1(36 * k)}" fill="#7cc25a"/>` +
         [[-24, 18, "#f472b6"], [-6, 24, "#facc15"], [14, 16, "#ffffff"], [28, 26, "#c4b5fd"]].map(([dx, dy, cc]) => `<circle cx="${f1(wx + dx * k)}" cy="${f1(wy + dy * k)}" r="${f1(3 * k)}" fill="${cc}"/>`).join("") + `</g>` +
         `<circle cx="${f1(wx)}" cy="${f1(wy)}" r="${f1(wr)}" fill="none" stroke="#8b5e34" stroke-width="${f1(9 * k)}"/>` +
         `<path d="M${P(wx - wr, wy)} H${f1(wx + wr)} M${P(wx, wy - wr)} V${f1(wy + wr)}" stroke="#8b5e34" stroke-width="${f1(4 * k)}"/>`;
    t = wallSince(t, mRound, "round window");
    // herbs drying in bundles from the beam (not across the window); (ticket 336) each an object of its own, "drying herbs 1" and on
    let herbN = 0;
    for (let i = 0; i < 12; i++) {
      const hx = x0 + W * (0.04 + 0.08 * i) + 8 * k * R(i, 1105), by = wallTop + 12 * k;
      if (Math.abs(hx) < 70 * k) continue;
      const cc = ["#5f8a3a", "#a78bfa", "#7a9a3a", "#f2c12e", "#4a8a4a"][i % 5], len = (50 + 24 * R(i, 1106)) * k;
      t += wallObj(`drying herbs ${++herbN}`, `<line x1="${f1(hx)}" y1="${f1(by)}" x2="${f1(hx)}" y2="${f1(by + 18 * k)}" stroke="#c9b27a" stroke-width="${f1(1.5 * k)}"/>` +
           `<rect x="${f1(hx - 4 * k)}" y="${f1(by + 16 * k)}" width="${f1(8 * k)}" height="${f1(5 * k)}" fill="#c9b27a"/>` +
           [-10, -5, 0, 5, 10].map(dx => `<path d="M${P(hx, by + 20 * k)} Q${P(hx + dx * 0.6 * k, by + 20 * k + len * 0.5)} ${P(hx + dx * k, by + 20 * k + len)}" stroke="${cc}" stroke-width="${f1(3.5 * k)}" fill="none" stroke-linecap="round"/>`).join("") +
           (i % 2 ? [-8, 0, 8].map(dx => `<circle cx="${f1(hx + dx * k)}" cy="${f1(by + 20 * k + len)}" r="${f1(3 * k)}" fill="${cc === "#a78bfa" ? "#c4b5fd" : "#f7f2c4"}"/>`).join("") : ""));
    }
    return t;
  });
  // the hearth: a stone fireplace in the wall, its mantel with candles and jars
  s += SEP + part("hroom-hearth", c.F, () => {
    const hw = 130 * k, top = floor - 190 * k;
    let t = `<rect x="${f1(potX - hw * 0.55)}" y="${f1(wallTop)}" width="${f1(hw * 1.1)}" height="${f1(top - wallTop)}" fill="#a39a8c"/>`;
    // (its stones, row on row all the way down to the mantel, each row half a stone along
    // from the last, kept on the breast)
    t += `<clipPath id="bg-hbreast"><rect x="${f1(potX - hw * 0.55)}" y="${f1(wallTop)}" width="${f1(hw * 1.1)}" height="${f1(top - wallTop)}"/></clipPath><g clip-path="url(#bg-hbreast)">`;
    for (let row = 0, sy = wallTop; sy < top; row++, sy += 22 * k)
      for (let col = -1; col < 5; col++) { const i = row * 5 + col + 5, sx = potX - hw * 0.55 + (col + (row % 2) * 0.5) * hw * 0.22;
        t += `<rect x="${f1(sx)}" y="${f1(sy)}" width="${f1(hw * 0.2)}" height="${f1(19 * k)}" rx="${f1(4 * k)}" fill="${["#b8afa0", "#a8a090", "#c2b9aa"][(i * 7) % 3]}"/>`; }
    t += `</g>`;
    t += `<path d="M${P(potX - hw, floor)} V${f1(top + 30 * k)} Q${P(potX - hw, top)} ${P(potX - hw + 30 * k, top)} H${f1(potX + hw - 30 * k)} Q${P(potX + hw, top)} ${P(potX + hw, top + 30 * k)} V${f1(floor)} Z" fill="#b8afa0" stroke="#8a8274" stroke-width="${f1(3 * k)}"/>` +
         `<path d="M${P(potX - hw * 0.68, floor)} V${f1(top + 80 * k)} A${f1(hw * 0.68)} ${f1(hw * 0.55)} 0 0 1 ${P(potX + hw * 0.68, top + 80 * k)} V${f1(floor)} Z" fill="#3a2418"/>` +
         `<ellipse cx="${f1(potX)}" cy="${f1(floor - 20 * k)}" rx="${f1(hw * 0.6)}" ry="${f1(50 * k)}" fill="#ff9a3c" opacity="0.35"/>` +
         `<rect x="${f1(potX - hw - 14 * k)}" y="${f1(top - 12 * k)}" width="${f1(2 * hw + 28 * k)}" height="${f1(14 * k)}" rx="${f1(3 * k)}" fill="#6e4a28"/>`;
    t = wallObj("fireplace", t);
    const mMantel = t.length;
    // on the mantel: two candles, three jars of herbs, a little heart
    for (const [dx, kind] of [[-0.85, "candle"], [-0.5, "jar"], [-0.2, "jar"], [0.15, "heart"], [0.45, "jar"], [0.82, "candle"]]) {
      const x = potX + dx * hw, b = top - 12 * k;
      t += kind === "candle"
        ? `<rect x="${f1(x - 5 * k)}" y="${f1(b - 30 * k)}" width="${f1(10 * k)}" height="${f1(30 * k)}" fill="#f6eedd"/><ellipse cx="${f1(x)}" cy="${f1(b - 37 * k)}" rx="${f1(3.5 * k)}" ry="${f1(7 * k)}" fill="#ffb53b"/><circle cx="${f1(x)}" cy="${f1(b - 34 * k)}" r="${f1(16 * k)}" fill="#ffcf6b" opacity="0.25"/>`
        : kind === "heart" ? `<path d="M${P(x, b - 4 * k)} L${P(x - 12 * k, b - 16 * k)} A${f1(6 * k)} ${f1(6 * k)} 0 0 1 ${P(x, b - 24 * k)} A${f1(6 * k)} ${f1(6 * k)} 0 0 1 ${P(x + 12 * k, b - 16 * k)} Z" fill="#e53935"/>`
        : `<rect x="${f1(x - 10 * k)}" y="${f1(b - 26 * k)}" width="${f1(20 * k)}" height="${f1(26 * k)}" rx="${f1(4 * k)}" fill="#cfe8d0" fill-opacity="0.7" stroke="#8aa88a" stroke-width="${f1(1.2 * k)}"/>` +
          `<path d="M${P(x - 6 * k, b - 4 * k)} Q${P(x, b - 20 * k)} ${P(x + 6 * k, b - 4 * k)}" fill="#5f8a3a"/><rect x="${f1(x - 8 * k)}" y="${f1(b - 31 * k)}" width="${f1(16 * k)}" height="${f1(6 * k)}" fill="#9a6b3f"/>`;
    }
    return wallSince(t, mMantel, "things on the mantel");
  });
  // the door back out to the clearing, in the middle: an arched brown door, as outside
  // (drawn before the objects, in the wall behind them all: it crosses none, the pot it
  // once came after included)
  s += SEP + part("hroom-door", c.F, () => {
    const w = 100 * k, top = floor - 200 * k;
    return `<g class="portal" data-to="clearing"><path d="M${P(-w / 2 - 10 * k, floor)} V${f1(top + w / 2)} A${f1(w / 2 + 10 * k)} ${f1(w / 2 + 10 * k)} 0 0 1 ${P(w / 2 + 10 * k, top + w / 2)} V${f1(floor)} Z" fill="#6e4a28"/>` +
           `<path d="M${P(-w / 2, floor)} V${f1(top + w / 2)} A${f1(w / 2)} ${f1(w / 2)} 0 0 1 ${P(w / 2, top + w / 2)} V${f1(floor)} Z" fill="#7a5a3a" stroke="#4a3220" stroke-width="${f1(3 * k)}"/>` +
           [-0.25, 0, 0.25].map(u => `<line x1="${f1(u * w)}" y1="${f1(top + (u ? 8 : 0) * k)}" x2="${f1(u * w)}" y2="${f1(floor)}" stroke="#5a3e26" stroke-width="${f1(2.5 * k)}"/>`).join("") +
           `<circle cx="${f1(w / 2 - 16 * k)}" cy="${f1(floor - 90 * k)}" r="${f1(5 * k)}" fill="#f2c230"/>` +
           `<path d="M${P(0, top + 56 * k)} L${P(-12 * k, top + 44 * k)} A${f1(6 * k)} ${f1(6 * k)} 0 0 1 ${P(0, top + 36 * k)} A${f1(6 * k)} ${f1(6 * k)} 0 0 1 ${P(12 * k, top + 44 * k)} Z" fill="#e53935"/>` +
           plankSign(0, top - 28 * k, k, "🌲 out") + `</g>`;
  });
  // the big pot of soup on its fire, in front of the hearth: flames, the
  // pot on three legs, golden soup with carrots and herbs in it, steam
  // (an object, with its fire and the stones round it, at g: where the healer stands
  // at first, beside it, stirring; its legs reach g + 4k, but she stirs it from in front
  // of its side, her spoon in the soup, so it stands no nearer than her feet)
  s += SEP + obj("soup pot", g, part("hroom-pot", [c.F, sceneLfoKey(o, "fast")], () => {
    const py = g - 62 * k, rx = 74 * k;
    let t = `<ellipse cx="${f1(potX)}" cy="${f1(g + 12 * k)}" rx="${f1(92 * k)}" ry="${f1(18 * k)}" fill="#6b625a"/>` +
            [...Array(10)].map((_, i) => { const a = i / 10 * 2 * Math.PI; return `<ellipse cx="${f1(potX + Math.cos(a) * 88 * k)}" cy="${f1(g + 12 * k + Math.sin(a) * 16 * k)}" rx="${f1(14 * k)}" ry="${f1(8 * k)}" fill="${i % 2 ? "#8a8274" : "#a39a8c"}"/>`; }).join("") +
            [-1, 1].map(e => `<line x1="${f1(potX - 50 * k)}" y1="${f1(g + 10 * k + e * 4 * k)}" x2="${f1(potX + 50 * k)}" y2="${f1(g + 10 * k - e * 4 * k)}" stroke="#5a3a1a" stroke-width="${f1(9 * k)}" stroke-linecap="round"/>`).join("");
    t += [[-34, 0], [-6, 6], [24, -4], [44, 3]].map(([fx, ph]) => { const x = potX + fx * k, b = g + 6 * k, h = (40 + ph) * k;
      return `<path d="M${f1(x - 14 * k)} ${f1(b)} Q${f1(x)} ${f1(b - h)} ${f1(x + 14 * k)} ${f1(b)} Z" fill="#ff8c1a">${lfoTimed(o, "fast", (fx + 34) / 80, `<animate attributeName="d" dur="${(0.6 + ph / 30).toFixed(2)}s" repeatCount="indefinite" ` +
        `values="M${f1(x - 14 * k)} ${f1(b)} Q${f1(x)} ${f1(b - h)} ${f1(x + 14 * k)} ${f1(b)} Z;M${f1(x - 14 * k)} ${f1(b)} Q${f1(x + 5 * k)} ${f1(b - h * 0.7)} ${f1(x + 14 * k)} ${f1(b)} Z;M${f1(x - 14 * k)} ${f1(b)} Q${f1(x)} ${f1(b - h)} ${f1(x + 14 * k)} ${f1(b)} Z"/>`)}</path>` +
        `<path d="M${f1(x - 7 * k)} ${f1(b)} Q${f1(x)} ${f1(b - h * 0.55)} ${f1(x + 7 * k)} ${f1(b)} Z" fill="#ffd23f"/>`; }).join("");
    // the pot
    t += [-0.7, 0, 0.7].map(u => `<line x1="${f1(potX + u * 50 * k)}" y1="${f1(g - 20 * k)}" x2="${f1(potX + u * 62 * k)}" y2="${f1(g + 4 * k)}" stroke="#2a2a30" stroke-width="${f1(7 * k)}" stroke-linecap="round"/>`).join("") +
         `<path d="M${P(potX - rx, py)} Q${P(potX - rx * 1.12, g - 30 * k)} ${P(potX - rx * 0.62, g - 14 * k)} Q${P(potX, g - 6 * k)} ${P(potX + rx * 0.62, g - 14 * k)} Q${P(potX + rx * 1.12, g - 30 * k)} ${P(potX + rx, py)} Z" fill="#2b2b30" stroke="#18181c" stroke-width="${f1(3 * k)}"/>` +
         `<path d="M${P(potX - rx * 0.8, g - 46 * k)} Q${P(potX - rx * 0.5, g - 22 * k)} ${P(potX - rx * 0.1, g - 18 * k)}" fill="none" stroke="#ff9a3c" stroke-opacity="0.35" stroke-width="${f1(5 * k)}" stroke-linecap="round"/>` +
         [-1, 1].map(e => `<path d="M${P(potX + e * rx * 0.95, py + 8 * k)} q${f1(e * 16 * k)} ${f1(-4 * k)} ${f1(e * 14 * k)} ${f1(14 * k)}" fill="none" stroke="#2b2b30" stroke-width="${f1(6 * k)}"/>`).join("") +
         `<ellipse cx="${f1(potX)}" cy="${f1(py)}" rx="${f1(rx + 4 * k)}" ry="${f1(19 * k)}" fill="#3a3a42" stroke="#18181c" stroke-width="${f1(3 * k)}"/>` +
         `<ellipse cx="${f1(potX)}" cy="${f1(py + 2 * k)}" rx="${f1(rx - 4 * k)}" ry="${f1(14 * k)}" fill="#f0a032"/>` +
         `<ellipse cx="${f1(potX - 10 * k)}" cy="${f1(py)}" rx="${f1(rx * 0.5)}" ry="${f1(6 * k)}" fill="#f7c062" opacity="0.7"/>` +
         [[-40, 4, "#e8762c"], [-14, -2, "#e8762c"], [22, 6, "#e8762c"], [44, 0, "#e8762c"], [-28, -6, "#4a9a52"], [6, 8, "#4a9a52"], [36, -6, "#4a9a52"], [0, -4, "#f6eedd"]].map(([dx, dy, cc]) =>
           `<ellipse cx="${f1(potX + dx * k)}" cy="${f1(py + 2 * k + dy * k)}" rx="${f1(5 * k)}" ry="${f1(2.6 * k)}" fill="${cc}"/>`).join("");
    // steam, rising and fading, in turn
    t += [-30, 6, 38].map((dx, i) => `<path d="M${f1(potX + dx * k)} ${f1(py - 6 * k)} q${f1(10 * k)} ${f1(-14 * k)} 0 ${f1(-28 * k)} q${f1(-10 * k)} ${f1(-14 * k)} 0 ${f1(-28 * k)}" fill="none" stroke="#ffffff" stroke-width="${f1(5 * k)}" stroke-linecap="round" opacity="0">` +
      `<animate attributeName="opacity" values="0;0.55;0" dur="3.2s" begin="${(i * 1.05).toFixed(2)}s" repeatCount="indefinite"/>` +
      `<animateTransform attributeName="transform" type="translate" values="0 0;0 ${f1(-40 * k)}" dur="3.2s" begin="${(i * 1.05).toFixed(2)}s" repeatCount="indefinite"/></path>`).join("");
    return t;
  }));
  // the shelves of potions, floor to beam on the right, neatly kept; her
  // cat sits up on the top one, at its left end, its tail hanging down
  const cat = { catFur: o.healerCatFur ?? "#e8862a", catWhite: o.healerCatWhite ?? "#ffffff", catEyes: o.healerCatEyes ?? "#22c55e", catNose: "#f4a0b0" };
  // (the shelves and their potions an object, standing on the floor at the wall, at
  // floor; the cat on them another, at the floor under her, the same)
  s += SEP + part("hroom-shelves", [c.F, cat.catFur, cat.catWhite, cat.catEyes], () => {
    const sx0 = W * 0.13, sx1 = W * 0.47, top = wallTop + 70 * k, [shelves, catSvg] = potionShelvesSvg(c, sx0, sx1, top, floor, cat);
    return obj("potion shelves", floor, shelves) + obj("cat", floor, catSvg);
  });
  // a little round table with two chairs and tea, to sit and talk
  s += SEP + part("hroom-tea", c.F, () => {
    const tx = W * 0.13, ty = g + 34 * k, [left, right, table] = teaSetSvg(c, tx, ty);
    // (each chair an object, its legs down to ty + 44k; the table and its tea another, its
    // foot at ty + 42k)
    return obj("chair, left", ty + 44 * k, left) + obj("chair, right", ty + 44 * k, right) + obj("tea table", ty + 42 * k, table);
  });
  // her worktable at the front on the right: a mortar and pestle, bowls of
  // powders, bundles of herbs, scissors, a knife, a little brass balance
  s += SEP + part("hroom-work", c.F, () => {
    const tx = W * 0.37, ty = g + 20 * k;
    // (the worktable and all on it an object, its legs down to ty + 64k)
    return obj("worktable", ty + 64 * k, worktableSvg(c, tx, ty));
  });
  // the healer: dragged anywhere (her place), her size her group's; at
  // first by the pot, her spoon in it
  // (data-x, data-y: where she is, for a drag to start from before she has a place of her own)
  // (her spoon's end, (-110, -48) in her units, at the soup's middle, g - 62k)
  // (her feet on the ground: at her usual size, the spoon's end is in the soup)
  // (an object at her feet, g + hy)
  const hx = o.healerX ?? potX + 110 * HEALER_Z * hz * k, hy = o.healerY ?? 0;
  s += obj("healer", g + hy, OPEN(`<g data-drag="healer" data-x="${f1(hx)}" data-y="${f1(hy)}" style="cursor: grab" ${tf(hx, g + hy, HEALER_Z * hz * k)}>`) + part("hroom-healer", [sceneLfoKey(o, "slow")], () => HEALER_SVG.replace(HEALER_SWAY, lfoTimed(o, "slow", 0.1, HEALER_SWAY))) + CLOSE);
  // warm: the fire's glow over the room, a little darker toward the corners
  // (over everything, the friend too: in front, at Infinity; the whole room's, so not the pot's)
  s += SEP + obj("warm light", Infinity, part("hroom-warm", c.F, () => `<defs><radialGradient id="bg-hwarm" gradientUnits="userSpaceOnUse" cx="${f1(potX)}" cy="${f1(g - 40 * k)}" r="${f1(0.8 * Math.hypot(W, H))}">` +
    `<stop offset="0" stop-color="#ffb347" stop-opacity="0.2"/><stop offset="0.4" stop-color="#ffb347" stop-opacity="0.05"/><stop offset="1" stop-color="#5a2a0a" stop-opacity="0.3"/></radialGradient></defs>` +
    all("url(#bg-hwarm)").replace("<rect ", `<rect pointer-events="none" `)));
  return s;
}
// The fairy forest: a glade in the woods at the time of day its chips say,
// a ring of toadstools in the grass, and a great old tree with a fairy
// house in it: a round door between its roots, lit windows, shelf-fungus
// steps up to a little balcony, lanterns, a chimney. The house shows as
// much as its slider says, and can fade in and out with one of the three
// LFOs, in time with the music. Fireflies at sunset and at night, the
// night's stars as the camp site's settings say; a sign back to the
// clearing.
const FAIRY_TIMES = [["day", "☀️ day"], ["sunrise", "🌅 sunrise"], ["sunset", "🌇 sunset"], ["night", "🌙 night"]];
const FAIRY_SKY = { day: ["#7cc4f0", "#e6f6ff"], sunrise: ["#f0a0c4", "#ffe6b0"], sunset: ["#5a3a8a", "#ff9a5a"], night: ["#0b1030", "#283066"] };
// the light's tint over the greens and the bark, and how much of it
const FAIRY_TINT = { day: ["#ffffff", 0], sunrise: ["#f4a6c4", 0.18], sunset: ["#5a2a4a", 0.3], night: ["#0b1030", 0.62] };
// the fairy house's see-through-ness, and its fade with an LFO: an
// animation of its opacity begun so the wave is where the music's is (its
// start rounded, so the scene's text stays the same from draw to draw)
function fairyHouseFade(o) {
  void o["lfo:" + o.fairyHouseLfo];   // (so a change to its LFO draws the scene again: sceneOpts)
  const a = Math.max(0, Math.min(1, (o.fairyHouseSeen ?? 100) / 100)), lf = lfoNow(o.fairyHouseLfo);
  if (!lf) return { a, key: "", anim: "" };
  // (too fast to show: its average, the wave's being 0 for every shape)
  if (lf.dur < LFO_BLUR) return { a: a * (1 - lf.amp / 2), key: "blur", anim: "" };
  const begin = lfoBegin(lf, 0).toFixed(4), N = 16, vals = [], kt = [];
  for (let i = 0; i <= N; i++) { const u = i / N; vals.push((a * (1 - lf.amp * (1 - lfoWave(lf.shape, u)) / 2)).toFixed(3)); kt.push(u.toFixed(4)); }
  return { a, key: [lf.dur.toFixed(4), lf.shape, lf.amp.toFixed(2), begin].join(),
           anim: `<animate attributeName="opacity" values="${vals.join(";")}" keyTimes="${kt.join(";")}" dur="${lf.dur.toFixed(4)}s" begin="${begin}s" repeatCount="indefinite"/>` };
}
// a toadstool: a cream stem, a red cap with white spots, (x, y) its foot
const toadstool = (x, y, s, cap = "#d62828", glow = false) =>
  (glow ? `<ellipse cx="${f1(x)}" cy="${f1(y - 22 * s)}" rx="${f1(30 * s)}" ry="${f1(22 * s)}" fill="#fff3a8" opacity="0.22"/>` : "") +
  `<ellipse cx="${f1(x)}" cy="${f1(y)}" rx="${f1(11 * s)}" ry="${f1(3 * s)}" fill="#000000" opacity="0.2"/>` +
  `<path d="M${f1(x - 6 * s)} ${f1(y)} Q${f1(x - 7 * s)} ${f1(y - 12 * s)} ${f1(x - 4 * s)} ${f1(y - 20 * s)} L${f1(x + 4 * s)} ${f1(y - 20 * s)} Q${f1(x + 7 * s)} ${f1(y - 12 * s)} ${f1(x + 6 * s)} ${f1(y)} Z" fill="#f6eedd" stroke="#c9b48a" stroke-width="${f1(1.2 * s)}"/>` +
  `<path d="M${f1(x - 20 * s)} ${f1(y - 18 * s)} Q${f1(x - 20 * s)} ${f1(y - 40 * s)} ${f1(x)} ${f1(y - 41 * s)} Q${f1(x + 20 * s)} ${f1(y - 40 * s)} ${f1(x + 20 * s)} ${f1(y - 18 * s)} Q${f1(x)} ${f1(y - 13 * s)} ${f1(x - 20 * s)} ${f1(y - 18 * s)} Z" fill="${cap}" stroke="${darker(cap)}" stroke-width="${f1(1.5 * s)}"/>` +
  [[-11, -26, 3], [-2, -34, 3.4], [9, -27, 2.8], [13, -21, 2], [-15, -21, 2]].map(([dx, dy, r]) => `<circle cx="${f1(x + dx * s)}" cy="${f1(y + dy * s)}" r="${f1(r * s)}" fill="#ffffff"/>`).join("");
// (ticket 50) the three biggest toadstools in the ring: the top of each
// one's cap (41 of its units up at the crown; within 12 each side of it the
// dome is still nearly as high), placed and sized as fairyforestScene's
// "fairy-ring" places them, and named as it names them
SEATS.fairyforest = (c, B) => { const { W, g, k } = c, n = Math.max(4, Math.min(30, Math.round(B.mushrooms ?? 14)));
  const rx0 = W * 0.06, ry0 = g + 40 * k, rrx = 190 * k, rry = 44 * k;
  return [...Array(n)].map((_, i) => { const a = (i + 0.3 * sceneR(i, 1330)) / n * 2 * Math.PI, x = rx0 + Math.cos(a) * rrx, y = ry0 + Math.sin(a) * rry;
      return { i, x, y, s: k * (0.75 + 0.5 * (y - (ry0 - rry)) / (2 * rry)) * (0.85 + 0.3 * sceneR(i, 1331)) }; })
    .sort((p, q) => q.s - p.s).slice(0, 3)
    .map(({ i, x, y, s }) => ({ name: `toadstool ${i + 1}`, x0: x - 12 * s, x1: x + 12 * s, y: y - 41 * s, z: y + 1, facing: ["front", "left", "right"] })); };
function fairyforestScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o } = c;
  // (ticket 490) by the clock: the time of day, the sun and the moon where and when the game says (🌍)
  const clock = o.fairyTime === "clock", time = clock ? skyTimeOf(o, true) : FAIRY_SKY[o.fairyTime] ? o.fairyTime : "day", night = time === "night", dusk = time === "sunset" || night;
  const [tint, ta] = FAIRY_TINT[time], tn = col => ta ? mix(col, tint, ta) : col;
  const starKeys = night ? [o.starShape, o.starSize, o.starBright, o.starTwinkle] : [];
  // the sky: its sun low or high, or the moon and stars
  let s = SEP + part("fairy-sky", [c.F, time, ...starKeys, sceneLfoKey(o, "fast"), clock ? skyKey(o) : ""], () => {
    const [a, b] = FAIRY_SKY[time];
    let t = `<defs>${grad("bg-sky", a, b, y0, g - 150 * k)}</defs>` + all("url(#bg-sky)");
    if (clock) return t + (night ? starsSvg(o, 70, x0, x1, y0, g - 170 * k, k, R, 1300) : "") + skyBodiesSvg(c, o, g - 150 * k, y0 + 12 * k, a);
    if (time === "day") t += sunSvg(x1 - W * 0.14, y0 + H * 0.12, 30 * k, true, o);
    if (time === "sunrise") t += `<circle cx="${f1(x0 + W * 0.2)}" cy="${f1(g - 170 * k)}" r="${f1(44 * k)}" fill="#ffd27a" opacity="0.95"/>`;
    if (time === "sunset") t += `<circle cx="${f1(x1 - W * 0.22)}" cy="${f1(g - 165 * k)}" r="${f1(48 * k)}" fill="#ff8c42" opacity="0.95"/>`;
    if (night) t += starsSvg(o, 70, x0, x1, y0, g - 170 * k, k, R, 1300) +
      `<circle cx="${f1(x1 - W * 0.18)}" cy="${f1(y0 + H * 0.14)}" r="${f1(28 * k)}" fill="#f4eebc"/><circle cx="${f1(x1 - W * 0.18 + 12 * k)}" cy="${f1(y0 + H * 0.14 - 8 * k)}" r="${f1(24 * k)}" fill="${FAIRY_SKY.night[0]}"/>`;
    return t;
  });
  if (!night) s += SEP + cloudsSvg(c, 3, 0.7, 150);
  // the woods round the glade: rows of pines and leafy trees, paler far off
  s += SEP + part("fairy-woods", [c.F, time], () => {
    let t = `<defs>${pineDef("bg-fpine", 7)}${leafyTreeDef("bg-foak", 23, { greens: ["#285e2a", "#2f7d32", "#3a8a3c", "#47a047", "#58b050"].map(tn), bark: tn("#6b4a2b") })}</defs>` +
            land(g - 160 * k, 8, tn("#a9d48e"), 0.4);
    // (each row an object, standing at its trees' middle depth, base + 6k; ticket 261: a tree more than 75% hidden by
    // the row in front and the grass drawn after, left out)
    t += treeRows([[0, g - 150 * k, ["#4f8f5a", "#5c9c63"]], [1, g - 110 * k, ["#2f6b3a", "#3c7d45"]]].map(([row, base, cols]) => {
      const items = [];
      for (let i = 0, x = x0 - 50 * k; x < x1 + 50 * k; i++, x += (row ? 52 : 36) * k) {
        const h = (row ? 150 : 100) * k * (0.8 + 0.4 * R(i, 1310 + row)), b = base + 12 * k * R(i, 1312 + row), sx = (h / 330) * (R(i, 1316) < 0.5 ? -1 : 1);
        items.push(R(i, 1314 + row) < 0.6 ? { svg: use("bg-fpine", x, b, h / 200, h / 200, ` color="${tn(cols[i % 2])}"`), shape: useOutline("bg-fpine", x, b, h / 200) }
                                          : { svg: use("bg-foak", x, b, sx, h / 330), shape: useOutline("bg-foak", x, b, sx, h / 330) });
      }
      return [row ? "trees, near row" : "trees, far row", base + 6 * k, items];
    }), [landCover(g - 70 * k, 6, k, x0 - pad, x1 + pad, y1 + pad)]);
    return t + land(g - 70 * k, 6, tn("#8bc56a"), 1.1) + land(g - 20 * k, 3, tn("#7cc25a"), 0.6);
  });
  // the great old tree, on the left: its crown, then a thick trunk with roots
  const tx = -W * 0.3, tb = g + 20 * k;
  // two branches reaching out from its trunk, a lantern hanging from each (in
  // the house's part, below): each a curve from inside the trunk out to its
  // leafy end, and where on it the lantern's string is tied
  const fairyBranches = [[[-40, -300], [-110, -322], [-168, -392], -120], [[40, -268], [100, -284], [168, -352], 118]].map(([a, q, b, lx]) => {
    const A = [tx + a[0] * k, tb + a[1] * k], Q = [tx + q[0] * k, tb + q[1] * k], B = [tx + b[0] * k, tb + b[1] * k];
    const pt = u => [0, 1].map(d => (1 - u) * (1 - u) * A[d] + 2 * u * (1 - u) * Q[d] + u * u * B[d]);
    let u = 0.5; for (let it = 0; it < 30; it++) { const x = pt(u)[0]; u += (tx + lx * k - x) / (B[0] - A[0]) * 0.8; u = Math.max(0, Math.min(1, u)); }
    return { A, Q, B, pt, tie: pt(u), u };
  });
  // (an object, standing at its trunk's foot, tb)
  s += SEP + obj("great tree", tb, part("fairy-tree", [c.F, time], () => {
    const bark = tn("#6b4a2b"), dk = mix(bark, "#000000", 0.35);
    let t = `<defs>${leafyTreeDef("bg-fbig", 41, { wide: 1.5, greens: ["#285e2a", "#2f7d32", "#3a8a3c", "#47a047", "#58b050"].map(tn), bark })}</defs>` +
            use("bg-fbig", tx, tb - 120 * k, 2.2 * k);
    // (its two branches, out of the trunk: thick near it, thinner out to a tuft of leaves)
    for (const { A, Q, B, pt } of fairyBranches) {
      const M = pt(0.5), Q2 = [(Q[0] + B[0]) / 2, (Q[1] + B[1]) / 2], Q1 = [(A[0] + Q[0]) / 2, (A[1] + Q[1]) / 2];
      t += `<path d="M${P(...A)} Q${P(...Q1)} ${P(...M)}" fill="none" stroke="${bark}" stroke-width="${f1(22 * k)}" stroke-linecap="round"/>` +
           `<path d="M${P(...M)} Q${P(...Q2)} ${P(...B)}" fill="none" stroke="${bark}" stroke-width="${f1(12 * k)}" stroke-linecap="round"/>` +
           `<path d="M${P(...A)} Q${P(...Q1)} ${P(...M)} Q${P(...Q2)} ${P(...B)}" fill="none" stroke="${dk}" stroke-width="${f1(1.6 * k)}" opacity="0.6" transform="translate(0 ${f1(4 * k)})"/>` +
           [[0, 0, 26, 15], [-14, -12, 20, 12], [14, -10, 20, 12], [6, 10, 18, 11], [-10, 8, 17, 10]].map(([dx, dy, rx, ry], j) =>
             `<ellipse cx="${f1(B[0] + dx * k)}" cy="${f1(B[1] + dy * k)}" rx="${f1(rx * k)}" ry="${f1(ry * k)}" fill="${tn(["#2f7d32", "#3a8a3c", "#47a047", "#285e2a", "#58b050"][j])}"/>`).join("");
    }
    // the trunk: wide, flaring into roots, bark lines up it
    t += `<path d="M${P(tx - 120 * k, tb)} Q${P(tx - 70 * k, tb - 30 * k)} ${P(tx - 62 * k, tb - 110 * k)} L${P(tx - 46 * k, tb - 380 * k)} L${P(tx + 46 * k, tb - 380 * k)} L${P(tx + 62 * k, tb - 110 * k)} Q${P(tx + 70 * k, tb - 30 * k)} ${P(tx + 120 * k, tb)} Z" fill="${bark}" stroke="${dk}" stroke-width="${f1(3 * k)}"/>` +
         [[-150, -40], [-90, -55], [100, -50], [160, -38]].map(([dx, h]) => `<path d="M${P(tx + dx * 0.4 * k, tb - 40 * k)} Q${P(tx + dx * 0.8 * k, tb + h * 0.2 * k)} ${P(tx + dx * k, tb + 6 * k)}" fill="none" stroke="${bark}" stroke-width="${f1(18 * k)}" stroke-linecap="round"/>`).join("") +
         [...Array(9)].map((_, i) => { const x = tx + (-40 + 10 * i) * k; return `<path d="M${P(x, tb - 20 * k)} Q${P(x + (R(i, 1320) - 0.5) * 16 * k, tb - 200 * k)} ${P(x * 0.98 + tx * 0.02, tb - 370 * k)}" fill="none" stroke="${dk}" stroke-width="${f1(1.8 * k)}" opacity="0.6"/>`; }).join("");
    return t;
  }));
  // the fairy house in it (its own see-through, and its LFO)
  // (an object with its lanterns, at the tree's foot, tb, as the tree is: drawn just after
  // it, in its trunk; when it is all faded away, it is not there)
  const fade = fairyHouseFade(o);
  const house = part("fairy-house", [c.F, time, fade.a, fade.key, sceneLfoKey(o, "slow")], () => {
    if (fade.a <= 0 && !fade.anim) return "";
    const warm = dusk ? "#ffd27a" : "#ffe9a8", lit = dusk ? 0.9 : 0.6;
    let t = `<g opacity="${fade.a.toFixed(2)}">${fade.anim}`;
    // the round door between the roots, its step a little toadstool
    const dy = tb - 6 * k, dw = 58 * k, dh = 92 * k;
    t += `<path d="M${P(tx - dw / 2, dy)} V${f1(dy - dh + dw / 2)} A${f1(dw / 2)} ${f1(dw / 2)} 0 0 1 ${P(tx + dw / 2, dy - dh + dw / 2)} V${f1(dy)} Z" fill="#3a8a4a" stroke="#22562c" stroke-width="${f1(3 * k)}"/>` +
         [-0.18, 0.18].map(u => `<line x1="${f1(tx + u * dw)}" y1="${f1(dy - dh + 10 * k)}" x2="${f1(tx + u * dw)}" y2="${f1(dy)}" stroke="#22562c" stroke-width="${f1(2 * k)}"/>`).join("") +
         `<circle cx="${f1(tx + dw * 0.32)}" cy="${f1(dy - dh * 0.42)}" r="${f1(4 * k)}" fill="#f2c230"/>` +
         `<circle cx="${f1(tx)}" cy="${f1(dy - dh + 24 * k)}" r="${f1(9 * k)}" fill="${warm}" opacity="${lit}" stroke="#22562c" stroke-width="${f1(2 * k)}"/>` +
         toadstool(tx + dw * 0.75, tb + 8 * k, 0.8 * k, "#d62828", night);
    // round windows up the trunk, lit, flower boxes under them
    for (const [wx, wy, wr] of [[tx - 22 * k, tb - 180 * k, 18 * k], [tx + 20 * k, tb - 270 * k, 15 * k]]) {
      t += `<circle cx="${f1(wx)}" cy="${f1(wy)}" r="${f1(wr + 5 * k)}" fill="#4a3220"/>` +
           `<circle cx="${f1(wx)}" cy="${f1(wy)}" r="${f1(wr)}" fill="${warm}" opacity="${lit}"/>` +
           `<path d="M${P(wx - wr, wy)} H${f1(wx + wr)} M${P(wx, wy - wr)} V${f1(wy + wr)}" stroke="#4a3220" stroke-width="${f1(2.5 * k)}"/>` +
           (dusk ? `<circle cx="${f1(wx)}" cy="${f1(wy)}" r="${f1(wr * 2.4)}" fill="#ffd27a" opacity="0.18"/>` : "") +
           `<rect x="${f1(wx - wr - 4 * k)}" y="${f1(wy + wr + 3 * k)}" width="${f1(2 * wr + 8 * k)}" height="${f1(8 * k)}" rx="${f1(2 * k)}" fill="#8b5e34"/>` +
           [-0.6, -0.2, 0.2, 0.6].map((u, j) => `<circle cx="${f1(wx + u * wr)}" cy="${f1(wy + wr + 2 * k)}" r="${f1(3.5 * k)}" fill="${["#f472b6", "#facc15", "#ffffff", "#c4b5fd"][j]}"/>`).join("");
    }
    // shelf-fungus steps winding up the trunk to a little balcony
    for (const [sx, sy, sd] of [[-52, -60, -1], [52, -110, 1], [-54, -160, -1], [54, -215, 1], [-50, -268, -1], [50, -320, 1]])
      t += `<path d="M${P(tx + sx * k, tb + sy * k)} Q${P(tx + (sx + sd * 26) * k, tb + (sy - 8) * k)} ${P(tx + (sx + sd * 34) * k, tb + (sy + 3) * k)} Q${P(tx + (sx + sd * 16) * k, tb + (sy + 8) * k)} ${P(tx + sx * k, tb + (sy + 5) * k)} Z" fill="#e8c88a" stroke="#a8823a" stroke-width="${f1(1.5 * k)}"/>`;
    const by = tb - 360 * k;
    t += `<rect x="${f1(tx - 70 * k)}" y="${f1(by)}" width="${f1(140 * k)}" height="${f1(9 * k)}" rx="${f1(3 * k)}" fill="#8b5e34"/>` +
         [...Array(9)].map((_, i) => `<line x1="${f1(tx - 66 * k + i * 16.5 * k)}" y1="${f1(by)}" x2="${f1(tx - 66 * k + i * 16.5 * k)}" y2="${f1(by - 24 * k)}" stroke="#8b5e34" stroke-width="${f1(3 * k)}"/>`).join("") +
         `<line x1="${f1(tx - 70 * k)}" y1="${f1(by - 24 * k)}" x2="${f1(tx + 70 * k)}" y2="${f1(by - 24 * k)}" stroke="#8b5e34" stroke-width="${f1(4 * k)}"/>`;
    // lanterns hanging from the branches either side, glowing at dusk: each
    // string tied round its branch (its underside, the branch's half-width below its middle)
    for (const { tie } of fairyBranches) {
      const lx = tie[0], top = tie[1] + 5 * k, ly = top + 34 * k;
      t += swaying(o, `<line x1="${f1(lx)}" y1="${f1(top)}" x2="${f1(lx)}" y2="${f1(ly)}" stroke="#4a3220" stroke-width="${f1(1.5 * k)}"/>` +
           `<ellipse cx="${f1(lx)}" cy="${f1(top)}" rx="${f1(3 * k)}" ry="${f1(1.6 * k)}" fill="none" stroke="#4a3220" stroke-width="${f1(1.2 * k)}"/>` +
           (dusk ? `<circle cx="${f1(lx)}" cy="${f1(ly + 12 * k)}" r="${f1(30 * k)}" fill="#ffd27a" opacity="0.25"/>` : "") +
           `<rect x="${f1(lx - 8 * k)}" y="${f1(ly)}" width="${f1(16 * k)}" height="${f1(22 * k)}" rx="${f1(4 * k)}" fill="${warm}" opacity="${dusk ? 1 : 0.8}" stroke="#4a3220" stroke-width="${f1(2 * k)}"/>` +
           `<path d="M${P(lx - 10 * k, ly)} L${P(lx, ly - 8 * k)} L${P(lx + 10 * k, ly)} Z" fill="#4a3220"/>`, lx, top, 7, lx > tx ? 0.55 : 0.1, 4);
    }
    // a little chimney out of the trunk, smoke curling up
    const cx = tx + 40 * k, cy = tb - 200 * k;
    t += `<rect x="${f1(cx)}" y="${f1(cy - 6 * k)}" width="${f1(34 * k)}" height="${f1(10 * k)}" fill="#7a756a"/><rect x="${f1(cx + 24 * k)}" y="${f1(cy - 40 * k)}" width="${f1(12 * k)}" height="${f1(40 * k)}" fill="#7a756a"/>` +
         [0, 1].map(i => `<path d="M${f1(cx + 30 * k)} ${f1(cy - 44 * k)} q${f1(10 * k)} ${f1(-12 * k)} 0 ${f1(-24 * k)} q${f1(-10 * k)} ${f1(-12 * k)} 0 ${f1(-24 * k)}" fill="none" stroke="#e8e8f0" stroke-width="${f1(5 * k)}" stroke-linecap="round" opacity="0">` +
           `<animate attributeName="opacity" values="0;0.5;0" dur="4s" begin="${i * 2}s" repeatCount="indefinite"/><animateTransform attributeName="transform" type="translate" values="0 0;0 ${f1(-36 * k)}" dur="4s" begin="${i * 2}s" repeatCount="indefinite"/></path>`).join("");
    return t + `</g>`;
  });
  s += SEP + (house && obj("fairy house", tb, house));
  // the ring of toadstools in the grass, on the right of the middle: the
  // far ones smaller, the near ones bigger; glowing a little at night
  // (each toadstool an object at its foot, numbered round the ring, so one can stand
  // in the ring, behind its near side and in front of its far side; the grass in it is flat)
  const n = Math.max(4, Math.min(30, Math.round(o.mushrooms ?? 14)));
  const rx0 = W * 0.06, ry0 = g + 40 * k, rrx = 190 * k, rry = 44 * k;
  s += SEP + part("fairy-ring", [c.F, n, night], () => {
    const ms = [...Array(n)].map((_, i) => { const a = (i + 0.3 * R(i, 1330)) / n * 2 * Math.PI; return [rx0 + Math.cos(a) * rrx, ry0 + Math.sin(a) * rry, i]; })
      .sort((p, q) => p[1] - q[1]);
    return `<ellipse cx="${f1(rx0)}" cy="${f1(ry0)}" rx="${f1(rrx * 0.9)}" ry="${f1(rry * 0.8)}" fill="${night ? "#2a5a3a" : "#9fd27e"}" opacity="0.5"/>` +
      ms.map(([x, y, i]) => obj(`toadstool ${i + 1}`, y, toadstool(x, y, k * (0.75 + 0.5 * (y - (ry0 - rry)) / (2 * rry)) * (0.85 + 0.3 * R(i, 1331)), ["#d62828", "#e85d04", "#d62828", "#c1121f"][i % 4], night))).join("");
  });
  // flowers, tufts, bushes and stones in the near grass
  s += SEP + part("fairy-near", [c.F, time], () => {
    let t = `<defs>${FLOWER_DEFS}</defs>`;
    const fl = [];   // (ticket 297: the flowers and tufts in rows by depth, the friend among them)
    for (let i = 0; i < 34; i++) {
      const d = R(i, 1340), x = x0 + W * R(i, 1341), y = g + 10 * k + (y1 - g) * d;
      if (Math.abs(x - rx0) < rrx * 1.05 && Math.abs(y - ry0) < rry * 1.3) continue;
      fl.push({ y, svg: i % 3 === 0 ? grassTuft(x, y, (14 + 10 * d) * k, i + 1340, tn("#4f9a3a"), 2 * k)
                                    : use(i % 2 ? "bg-daisy" : "bg-tulip", x, y, k * (0.8 + 0.6 * d), k * (0.8 + 0.6 * d), ` color="${tn(FLOWER_COLORS[i % 6])}"`) });
    }
    t += depthRows("flowers and grass", fl, 12 * k);
    // (the bush and the stones are objects, each at its foot)
    t += obj("bush", g + 6 * k, bushSvg(x1 - W * 0.08, g + 6 * k, 70 * k, 1350, { greens: ["#285e2a", "#2f7d32", "#3a8a3c", "#4a9a46", "#5aad52"].map(tn) })) +
         obj("stone, left", g + 60 * k, rockSvg(x0 + W * 0.1, g + 60 * k, 26 * k, 1351, tn("#9a9488"))) + obj("stone, right", g + 90 * k, rockSvg(x1 - W * 0.3, g + 90 * k, 18 * k, 1352, tn("#9a9488")));
    return t;
  });
  // fireflies at sunset and at night, drifting and blinking; sparkles by day
  // (drawn after the objects, flying all over: in front of everything, at Infinity)
  s += SEP + obj(dusk ? "fireflies" : "sparkles", Infinity, part("fairy-lights", [c.F, time, sceneLfoKey(o, "fast")], () => [...Array(dusk ? 18 : 10)].map((_, i) => {
    const x = x0 + W * (0.08 + 0.84 * R(i, 1360)), y = g - 260 * k + 300 * k * R(i, 1361), dx = (R(i, 1362) - 0.5) * 60 * k, dyy = (R(i, 1363) - 0.5) * 40 * k, d = (5 + 5 * R(i, 1364)).toFixed(1);
    return dusk
      ? `<g><animateTransform attributeName="transform" type="translate" values="0 0;${f1(dx)} ${f1(dyy)};0 0" dur="${d}s" repeatCount="indefinite"/>` +
        `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(9 * k)}" fill="#fff3a8" opacity="0.25"/><circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(3 * k)}" fill="#fffbd0">` +
        lfoTimed(o, "fast", R(i, 1366), `<animate attributeName="opacity" values="1;0.15;1" dur="${(1.4 + R(i, 1365) * 1.6).toFixed(2)}s" repeatCount="indefinite"/>`) + `</circle></g>`
      : `<path d="${starPath("four", 6 * k)}" transform="translate(${f1(x)} ${f1(y)})" fill="#ffffff" opacity="0">` +
        lfoTimed(o, "fast", R(i, 1366), `<animate attributeName="opacity" values="0;0.9;0" dur="${(2 + R(i, 1365) * 2).toFixed(2)}s" begin="${(R(i, 1366) * 3).toFixed(2)}s" repeatCount="indefinite"/>`) + `</path>`;
  }).join("")));
  // (ticket 233: no 🌲 clearing ➡️ sign back; the world's ways are the nav arrows)
  // the light: a dusk glow, or the night's blue over everything
  // (over everything, the friend too: in front, at Infinity, named for its time)
  if (time !== "day") s += SEP + obj(night ? "dark" : time + " light", Infinity, part("fairy-light", [c.F, time], () =>
    all(night ? "#0b1030" : time === "sunset" ? "#5a2a4a" : "#f4a6c4").replace("<rect ", `<rect pointer-events="none" opacity="${night ? 0.22 : 0.1}" `)));
  return s;
}
// A clearing in the forest, and far off down its paths, things to see,
// each shown or not by its own switch: the family house, the tree house,
// the witch's hut, a castle, a healer's hut. The ones with a scene of their
// own close up are doorways into it.
// the witch's hut from afar: dark pines round a log hut on chicken legs,
// its window glowing, smoke curling from its crooked chimney
function farWitchSvg(o) {
  // (on chicken legs, as long as hers, or sat on the ground: as in her woods)
  const ls = (o.legSize ?? 100) / 100, legH = o.chickenLegs ? 46 * ls : 0, dy = 42 - legH;
  // (dark pines round it: the shared pine, in her woods' dusk greens)
  let t = `<defs>${pineDef("bg-far-witch-pine", 91)}</defs>` + [-120, -70, 80, 130, 30].map((x, i) => {
    const h = 110 + 40 * (i % 2); return use("bg-far-witch-pine", x, 90, h / 200, h / 200, ` color="${["#1f3a2a", "#24402e", "#18301f"][i % 3]}"`); }).join("");
  if (legH) t += [-1, 1].map(sd => `<path d="M${sd * 18} ${f1(44 + dy)} L${sd * 22} ${f1(86 - legH * 0.35)} L${sd * 34} 84 M${sd * 22} ${f1(86 - legH * 0.35)} L${sd * 14} 86" stroke="#e0552b" stroke-width="${f1(7 * Math.sqrt(ls))}" stroke-linecap="round" fill="none"/>`).join("");
  t += `<g transform="translate(0 ${f1(dy)})">` +
       `<rect x="-50" y="-26" width="100" height="70" fill="#5a3a2a" stroke="#2a1a12" stroke-width="3"/>` +
       [-12, 2, 16, 30].map(y => `<line x1="-50" y1="${y}" x2="50" y2="${y}" stroke="#2a1a12" stroke-width="2"/>`).join("") +
       `<polygon points="-64,-24 0,-78 66,-22" fill="#3b2f2a" stroke="#1d1410" stroke-width="3"/>` +
       `<rect x="-14" y="-6" width="26" height="22" fill="#ffd23f"/><path d="M-14 5 H12 M-1 -6 V16" stroke="#5a3a2a" stroke-width="3"/>` +
       `<polygon points="28,-60 44,-64 46,-30 30,-34" fill="#2a1a12"/>` +
       `<path d="M38 -66 Q26 -96 46 -116 Q62 -136 48 -160" fill="none" stroke="#c9c9d6" stroke-width="10" stroke-linecap="round" opacity="0.6"/></g>`;
  // her fence: crooked pickets either side of the gate, rails across
  t += [-1, 1].map(sd => `<path d="M${sd * 22} 84 H${sd * 130} M${sd * 22} 100 H${sd * 130}" stroke="#4a3626" stroke-width="5"/>` +
         [...Array(8)].map((_, i) => { const x = sd * (28 + i * 14), lean = ((i * 7) % 5 - 2) * 2;
           return `<polygon points="${x - 5},108 ${x + 5},108 ${x + 5 + lean},${72 - (i % 3) * 4} ${x + lean},${62 - (i % 3) * 4} ${x - 5 + lean},${72 - (i % 3) * 4}" fill="#5a4432" stroke="#2a1d14" stroke-width="1.5"/>`; }).join("")).join("") +
       `<path d="M-20 108 L-18 74 M20 108 L18 74" stroke="#2a1d14" stroke-width="5"/>`;
  // skulls on her fence posts, as big as her woods' skulls (the biggest
  // grow with their slider), their eyes as big and as glowing
  const big = Math.min(4, (o.skullMax ?? 300) / 100), eye = (o.skullEye ?? 100) / 100, glow = (o.eyeGlow ?? 70) / 100;
  t += [-112, -56, 56, 112].map((x, i) => {
    const r = 9 * (1 + (big - 1) * (i % 2 ? 0.5 : 0.2)), y = 58 - r * 0.6, er = 2.6 * eye * r / 9;
    return `<circle cx="${x}" cy="${f1(y)}" r="${f1(r)}" fill="#ece6d6" stroke="#8a8476" stroke-width="1.2"/>` +
           `<rect x="${f1(x - r * 0.5)}" y="${f1(y + r * 0.55)}" width="${f1(r)}" height="${f1(r * 0.5)}" fill="#ece6d6"/>` +
           [-1, 1].map(sd => (glow > 0 ? `<circle cx="${f1(x + sd * r * 0.38)}" cy="${f1(y)}" r="${f1(er * (1.4 + 2 * glow))}" fill="#ff2a2a" opacity="${(0.25 + 0.3 * glow).toFixed(2)}"/>` : "") +
             `<circle cx="${f1(x + sd * r * 0.38)}" cy="${f1(y)}" r="${f1(er)}" fill="#ff3b3b"/>`).join("");
  }).join("");
  return t;
}
// a castle's moat, in the castle's own units (its walls' foot at y = 0), as
// wide as its setting m: an ellipse, and the drawbridge's length across it;
// and the drawbridge's half width, l down from the gate (for its drawing,
// and for a friend wading in the moat: WATER, ticket 122)
const moatOf = m => ({ rx: 187 + 45 * m, ry: 14 + 22 * m, cy: 12, len: 26 + 24 * m });
const bridgeHalf = l => 34 + l * 0.16;
// where a castle stands up close, on the stage: its walls' foot, and its size there
const castleAt = ({ g, k, y0 }, K) => { const foot = g - 34 * k; return { foot, z: Math.min(1.25 * k, (foot - y0 - 20 * k) / (226 + 38 * K.levels)) }; };
// a castle on its hill: towers with pointed roofs and flags, a gate
function castleSvg(K = castleOf({})) {
  const stone = K.stone, dark = darker(stone), L = K.levels, m = K.moat;
  // (each level adds to the walls, the towers and the keep; at 3 levels it
  // is the castle the clearing always had)
  const wallH = 66 + 18 * L, towerH = 60 + 40 * L, keepTop = -(wallH + 50 + 20 * L);
  const teeth = (x, w, y) => [...Array(Math.floor(w / 16))].map((_, i) => `<rect x="${x + i * 16}" y="${y - 10}" width="10" height="10" fill="${stone}"/>`).join("");
  const r = (i, sd) => { const v = Math.sin(i * 12.9898 + sd * 78.233) * 43758.5453; return v - Math.floor(v); };
  // the moat round the castle's foot (its far side hidden by the walls), as
  // wide as the castle's own setting: none, or out to very wide
  const { rx, ry, cy, len } = moatOf(m);
  const moat = m <= 0 ? "" : `<ellipse cx="0" cy="${f1(cy)}" rx="${f1(rx)}" ry="${f1(ry)}" fill="#3f8fc8" stroke="#2c6e9e" stroke-width="3"/>` +
    `<ellipse cx="0" cy="${f1(cy + 6)}" rx="${f1(rx - 32)}" ry="${f1(Math.max(4, ry - 14))}" fill="#62b0e0" opacity="0.6"/>`;
  // the drawbridge, down across the moat from the gate, its chains up to the wall
  const sp = bridgeHalf;
  const bridge = m <= 0 ? "" : `<polygon points="-34,0 34,0 ${f1(sp(len))},${f1(len)} ${f1(-sp(len))},${f1(len)}" fill="#8b5e34" stroke="#5e3d22" stroke-width="2"/>` +
    [...Array(Math.floor(len / 10))].map((_, j) => { const y = 10 * (j + 1); return `<line x1="${f1(-sp(y))}" y1="${y}" x2="${f1(sp(y))}" y2="${y}" stroke="#5e3d22" stroke-width="2"/>`; }).join("") +
    `<path d="M-30 -70 L${f1(-sp(len) + 2)} ${f1(len - 2)} M30 -70 L${f1(sp(len) - 2)} ${f1(len - 2)}" stroke="#555" stroke-width="2.5" stroke-dasharray="5 3"/>`;
  // a low fence outside the moat, round its near side, open at the bridge
  const fx = (m <= 0 ? 230 : rx + 38), fy = (m <= 0 ? 30 : ry + 14);
  const posts = [...Array(23)].map((_, i) => { const a = Math.PI * (i + 0.5) / 23; return [fx * Math.cos(a), 16 + fy * Math.sin(a)]; }).filter(([x]) => Math.abs(x) > 50);
  const fence = [-1, 1].map(sd => { const side = posts.filter(([x]) => Math.sign(x) === sd).sort((p, q) => p[0] - q[0]);
      return `<polyline points="${side.map(([x, y]) => `${f1(x)},${f1(y - 14)}`).join(" ")}" fill="none" stroke="#6e4a2c" stroke-width="4"/>`; }).join("") +
    posts.map(([x, y]) => `<rect x="${f1(x - 3)}" y="${f1(y - 22)}" width="6" height="24" fill="#7a5232" stroke="#4a2e1a" stroke-width="1"/>`).join("");
  // trees round it, behind and beside the walls: the more, the further out
  const trees = (K.trees ? `<defs>${leafyTreeDef("bg-ctree0", 11)}${leafyTreeDef("bg-ctree1", 23)}${pineDef("bg-cpine", 7)}</defs>` : "") +
    [...Array(K.trees)].map((_, i) => {
      const sd = i % 2 ? 1 : -1, j = Math.floor(i / 2), x = sd * (175 + 52 * j + 18 * r(i, 1)), y = -24 - 26 * r(i, 2) - (j % 2) * 18, h = 0.75 + 0.45 * r(i, 3);
      return r(i, 4) < 0.5 ? use("bg-cpine", x, y, 0.85 * h, 0.85 * h, ` color="${["#2f6e3a", "#3a7d44", "#285e33"][i % 3]}"`)
                           : use(`bg-ctree${i % 2}`, x, y, 0.44 * h * (r(i, 5) < 0.5 ? -1 : 1), 0.44 * h);
    }).join("");
  const mound = `<path d="M${f1(-fx - 40)} 70 Q-120 -40 0 -30 Q130 -40 ${f1(fx + 40)} 70 Z" fill="#7cb85a"/>`;
  // the stonework: courses of blocks on a tower or the keep, its shaded side
  const courses = (x, y, w, h) => [...Array(Math.floor(h / 16))].map((_, j) => `<line x1="${x}" y1="${f1(y + 16 * (j + 1))}" x2="${x + w}" y2="${f1(y + 16 * (j + 1))}" stroke="${dark}" stroke-width="1" opacity="0.45"/>` +
      [...Array(Math.floor(w / 20))].map((_, i) => `<line x1="${x + 10 + i * 20 + (j % 2) * 10}" y1="${f1(y + 16 * j)}" x2="${x + 10 + i * 20 + (j % 2) * 10}" y2="${f1(y + 16 * (j + 1))}" stroke="${dark}" stroke-width="1" opacity="0.35"/>`).join("")).join("") +
    `<rect x="${x + w * 0.62}" y="${y}" width="${f1(w * 0.38)}" height="${h}" fill="#000000" opacity="0.1"/>`;
  // a cone roof's tiles and its sunny side
  const tiles = (cx, base, hw, h) => [0.3, 0.55, 0.78].map(u => `<line x1="${f1(cx - hw * (1 - u))}" y1="${f1(base - h * u)}" x2="${f1(cx + hw * (1 - u))}" y2="${f1(base - h * u)}" stroke="${darker(K.roof)}" stroke-width="2"/>`).join("") +
    `<polygon points="${f1(cx - hw)},${base} ${cx},${base - h} ${f1(cx - hw * 0.35)},${base}" fill="#ffffff" opacity="0.12"/>`;
  // ivy climbing a corner
  const ivy = (x, y, h, sd) => drawnBy("ivy", [...Array(Math.floor(h / 9))].map((_, j) => `<circle cx="${f1(x + sd * (4 + 6 * r(j, 7 + sd)))}" cy="${f1(y - j * 9)}" r="${f1(5 + 3 * r(j, 9))}" fill="${["#3c8a3e", "#2f7d32", "#4a9a46"][j % 3]}"/>`).join(""));
  // lily pads and reeds on the moat
  const pads = m <= 0 ? "" : [...Array(7)].map((_, i) => { const a = Math.PI * (0.15 + 0.7 * r(i, 11)), x = Math.cos(a) * rx * 0.86 * (r(i, 12) < 0.5 ? -1 : 1), y = cy + Math.sin(a) * ry * 0.7;
      if (Math.abs(x) < 50) return "";
      return `<path d="M${f1(x)} ${f1(y)} m-9 0 a9 4 0 1 0 18 0 l-9 0 Z" fill="#4f9a3a"/>` + (i % 3 === 0 ? `<circle cx="${f1(x + 3)}" cy="${f1(y - 3)}" r="3" fill="#f9a8d4"/>` : ""); }).join("") +
    [-1, 1].map(sd => [...Array(6)].map((_, j) => { const x = sd * (rx - 6 - j * 7); return `<path d="M${f1(x)} ${f1(cy + 2)} q${f1(sd * 3)} -16 ${f1(sd * 2)} -26" fill="none" stroke="#5a8a3a" stroke-width="2"/>` +
      (j % 2 ? `<ellipse cx="${f1(x + sd * 2)}" cy="${f1(cy - 22)}" rx="2.5" ry="6" fill="#6b4423"/>` : ""); }).join("")).join("");
  // the moat's reflection of the walls, wavering
  const shine = m <= 0 ? "" : `<rect x="-110" y="${f1(cy - ry + 6)}" width="220" height="${f1(Math.min(30, ry))}" fill="${stone}" opacity="0.25"/>` +
    [...Array(5)].map((_, j) => `<line x1="${f1(-90 + j * 40)}" y1="${f1(cy - ry + 12 + j * 3)}" x2="${f1(-70 + j * 40)}" y2="${f1(cy - ry + 12 + j * 3)}" stroke="#ffffff" stroke-width="2" opacity="0.5"/>`).join("");
  return castleFlagDefs(K, 4) + trees + mound + moat + shine + pads +
    `<rect x="-120" y="${-wallH}" width="240" height="${wallH}" fill="${stone}" stroke="${dark}" stroke-width="3"/>` +
    // (its blocks, under everything on the wall, kept on the wall)
    `<clipPath id="castle-wall-${K.id}-${L}"><rect x="-120" y="${-wallH}" width="240" height="${wallH}"/></clipPath><g clip-path="url(#castle-wall-${K.id}-${L})">` +
    [...Array(18)].map((_, i) => `<rect x="${-110 + (i % 6) * 38 + (Math.floor(i / 6) % 2) * 19}" y="${-wallH + 10 + Math.floor(i / 6) * 34}" width="30" height="14" fill="none" stroke="${dark}" stroke-width="1" opacity="0.6"/>`).join("") + `</g>` +
    teeth(-120, 240, -wallH) +
    [-1, 1].map(sd => `<rect x="${sd * 150 - 30}" y="${-towerH}" width="60" height="${towerH}" fill="${stone}" stroke="${dark}" stroke-width="3"/>` +
      courses(sd * 150 - 30, -towerH, 60, towerH) + teeth(sd * 150 - 34, 68, -towerH + 2) +
      `<polygon points="${sd * 150 - 38},${-towerH} ${sd * 150},${-towerH - 70} ${sd * 150 + 38},${-towerH}" fill="${K.roof}"/>` + tiles(sd * 150, -towerH, 38, 70) +
      `<line x1="${sd * 150}" y1="${-towerH - 70}" x2="${sd * 150}" y2="${-towerH - 100}" stroke="#555" stroke-width="3"/><polygon points="${sd * 150},${-towerH - 100} ${sd * 150 + 26},${-towerH - 92} ${sd * 150},${-towerH - 84}" fill="${castleFlagFill(K, 1, 4)}"/>` +
      [...Array(L)].map((_, j) => `<rect x="${sd * 150 - 8}" y="${-towerH + 40 + 40 * j}" width="16" height="26" rx="8" fill="${j % 2 ? "#3a3a48" : "#ffd66b"}"/>`).join("") +
      ivy(sd * 150 - sd * 30, 0, Math.min(towerH * 0.6, 120), -sd)).join("") +
    `<rect x="-40" y="${keepTop}" width="80" height="${-keepTop - wallH}" fill="${stone}" stroke="${dark}" stroke-width="3"/>` + teeth(-40, 80, keepTop) +
    courses(-40, keepTop, 80, -keepTop - wallH) +
    [...Array(Math.max(0, L - 1))].map((_, j) => `<rect x="-8" y="${keepTop + 24 + 34 * j}" width="16" height="22" rx="8" fill="#3a3a48"/>`).join("") +
    // (the keep's banner: its owner's letter, in its owner's color)
    `<polygon points="-48,${keepTop} 0,${keepTop - 70} 48,${keepTop}" fill="${K.roof}"/>` + tiles(0, keepTop, 48, 70) + `<line x1="0" y1="${keepTop - 70}" x2="0" y2="${keepTop - 120}" stroke="#555" stroke-width="3"/>` +
    `<polygon points="0,${keepTop - 120} 46,${keepTop - 108} 0,${keepTop - 96}" fill="${castleFlagFill(K, 0, 4)}"/>` +
    castleLetter(K, 0, 15, keepTop - 102, 16) +
    // (the gate's arch, and its portcullis: bars from the arch down, and bars across, all inside the arch)
    `<path d="M-34 0 V-56 A34 34 0 0 1 34 -56 V0 Z" fill="#3a2a1a"/>` +
    `<clipPath id="castle-gate-${K.id}-${L}"><path d="M-34 0 V-56 A34 34 0 0 1 34 -56 V0 Z"/></clipPath><g clip-path="url(#castle-gate-${K.id}-${L})">` +
    [-20, -7, 7, 20].map(x => `<line x1="${x}" y1="-92" x2="${x}" y2="0" stroke="#6a5a4a" stroke-width="3"/>`).join("") +
    [-70, -46, -22].map(y => `<line x1="-36" y1="${y}" x2="36" y2="${y}" stroke="#6a5a4a" stroke-width="2.5"/>`).join("") + `</g>` +
    [-80, 80].map(x => `<rect x="${x - 8}" y="${-wallH + 30}" width="16" height="24" rx="8" fill="#3a3a48"/>`).join("") +
    // banners hanging from the wall, in the owner's color, the owner's letter on them
    // (from just under the battlements, as long as the wall is tall, and
    // never down to the ground or the moat)
    (() => { const bl = Math.min(wallH * 0.55, wallH - 22), bt = -wallH + 4;
      return [-1, 1].map((sd, i) => `<path d="M${sd * 102 - 13} ${bt} h26 v${f1(bl)} l-13 -9 l-13 9 Z" fill="${castleFlagFill(K, i, 4)}" stroke="#e8b923" stroke-width="1.5"/>` +
        castleLetter(K, i, sd * 102, bt + Math.min(bl * 0.55, 34) + 6, 16, "serif")).join(""); })() +
    // torches either side of the gate
    [-1, 1].map(sd => `<rect x="${sd * 46 - 2}" y="-62" width="4" height="12" fill="#3a3a44"/><circle cx="${sd * 46}" cy="-68" r="12" fill="#ffcf6b" opacity="0.35"/>` +
      `<path d="M${sd * 46} -76 q5 6 0 10 q-5 -4 0 -10 Z" fill="#ffb53b"/>`).join("") +
    bridge + fence;
}
// a healer's hut: round, thatched, a heart on its sign, herbs growing by it
function healerSvg() {
  // a grove round it: trees behind and either side, their trunks in the ground at y = 40
  // (leafy trees, as everywhere: their feet in the ground at y = 40)
  const grove = `<defs>${leafyTreeDef("bg-htree0", 31)}${leafyTreeDef("bg-htree1", 37)}</defs>` +
    [[-150, 10, 52], [-105, -5, 62], [120, -10, 66], [165, 15, 50], [-40, -40, 58], [55, -45, 54]].map(([x, y, r], i) => {
      const z = ((40 - y) + 1.7 * r) / 330;
      return use(`bg-htree${i % 2}`, x, 40, z * (i % 3 ? 1 : -1), z);
    }).join("");
  return grove + [-90, -70, -50, 60, 80, 100].map((x, i) => `<path d="M${x} 40 Q${x - 6} 20 ${x - 10} 6 M${x} 40 Q${x + 4} 18 ${x + 10} 8" stroke="${["#5f8a3a", "#7a9a3a", "#4a8a4a"][i % 3]}" stroke-width="4" fill="none"/>` +
      `<circle cx="${x - 10}" cy="6" r="4" fill="${["#c39bd3", "#f7a8c8", "#f2c12e"][i % 3]}"/>`).join("") +
    `<path d="M-60 40 V-20 Q0 -40 60 -20 V40 Z" fill="#e8d5b0" stroke="#a88a5a" stroke-width="3"/>` +
    // (the thatched roof, peaking at -96, where its thatch meets and its smoke comes out;
    // each line of thatch from just inside its lower edge, -14 - 44 t (1 - t), up to the peak)
    `<path d="M-80 -14 Q0 -178 80 -14 Q0 -36 -80 -14 Z" fill="#c9a35a" stroke="#8a6a2a" stroke-width="3"/>` +
    [-50, -25, 0, 25, 50].map(x => { const t = (x * 1.4 + 80) / 160;
      return `<path d="M${x * 1.4} ${f1(-16 - 44 * t * (1 - t))} Q${x} -60 ${x * 0.3} -94" stroke="#a8823a" stroke-width="2" fill="none"/>`; }).join("") +
    `<path d="M-16 40 V4 Q0 -10 16 4 V40 Z" fill="#7a5a3a"/>` +
    `<rect x="24" y="-10" width="34" height="26" rx="4" fill="#ffffff" stroke="#a88a5a" stroke-width="2"/>` +
    `<path d="M41 12 L31 2 A5 5 0 0 1 41 -4 A5 5 0 0 1 51 2 Z" fill="#e53935"/>` +
    `<path d="M10 -96 Q2 -120 16 -140" fill="none" stroke="#d8d8e0" stroke-width="8" stroke-linecap="round" opacity="0.6"/>`;
}
// what is far off in the clearing, where (x as a share of the width, how
// far up from the clearing's near edge), its size, and its doorway
const CLEARING_THINGS = [
  // (each as its own scene has it: the tree house is that scene's very tree,
  // its houses, bridges and ladders; the house its size; the witch's hut its
  // legs and its skulls)
  { key: "seeCastle", at: [0.1, 120], size: 0.42, foot: 62, draw: o => { const K = castleOf(o);
      return part("far-castle", [K.id, K.levels, K.stone, K.trees, K.moat, K.flagA, K.flagB, K.flagStyle, K.roof], () => castleSvg(K)); }, to: "castle" },
  { key: "seeTree", at: [-0.2, 95], size: 0.25, foot: 6, grow: o => (o.treeSize ?? 100) / 100, draw: (o, c) => unmarked(treeInner(c)), to: "treehouse" },
  { key: "seeHouse", at: [-0.36, 70], size: 0.3, foot: 4, grow: o => (o.houseSize ?? 100) / 100, draw: () => part("house-building", [], houseBuildingSvg), to: "house" },
  { key: "seeWitch", at: [0.25, 95], size: 0.52, foot: 108, draw: o => part("far-witch", [!!o.chickenLegs, o.legSize ?? 100, o.skullMax ?? 300, o.skullEye ?? 100, o.eyeGlow ?? 70], () => farWitchSvg(o)), to: "witch" },
  { key: "seeHealer", at: [0.36, 40], size: 0.58, foot: 40, draw: () => part("far-healer", [], healerSvg), to: "healerroom" },
];

// (ticket 278, Matthew for M: "new background: hut, made of grass (top) and sticks (walls, and door). Rounded and
// similar in style to our exterior drawing of the healer's hut, except explicitly made of sticks and grass"; then
// "inside of hut, a room --- decorated as a living room, with a couch, like our current living room, but with hut
// walls"; "more interior hut rooms: bedroom, and potion room (similar objects to healer's hut inside, but hut walls,
// floor, ceiling)") The grass hut, outside and in. Its three rooms share one shell (hutShellSvg): a grass ceiling,
// its rafters running up to the peak; walls of upright sticks, lashed with twine; an earth floor strewn with straw.
// Their doors are sticks too (hutDoorSvg): the living room's lead to the bedroom and the potion room, and theirs
// back; the living room's way out is toward us, to the hut outside (insideWays).
const HUT_UP = 110;   // (how far above the ground line its rooms' floors meet their walls: SCENE_BACK)
const HUT_STICKS = ["#9a6a3c", "#a8784a", "#8b5e34", "#b0844f"];
// (ticket 281, Matthew: "for the interior of the hut --- let's try doing a rounded shape ... we did that already in the
// spiral staircase in a more complex setting (spiral floor) --- here the floor is flat, and it's a simpler special
// case") The rooms are round: we stand inside and see the back half of the hut's wall, a cylinder of sticks, under
// its dome of thatch. hutGeom gives the wall's points: th from π (the left) round the back (3π/2) to 2π (the right),
// u from 0 at the wall's top to 1 at the floor. As the spiral staircase's courses do, a ring of the wall bows down
// above our eyes and up below them: the floor's edge rises to the back, the wall's top sinks to it; the sticks crowd
// toward the sides, where the wall turns away from us.
// (ticket 516: its radius r of the picture's width, its wall's top `top` of the picture's height down, its sides' top dt lower;
// the defaults the grass hut's, byte for byte; the round room at the top of the tree passes its own: topGeom)
function hutGeom(c, { r = 0.62, top = 0.16, dt = 70 } = {}) {
  const { y0, H, W, g, k } = c, R = W * r, Yf = g - HUT_UP * k, Yt = y0 + H * top, Df = 90 * k, Dt = dt * k;
  const x = th => R * Math.cos(th), s = th => Math.max(0, -Math.sin(th));   // (s: 1 at the back, 0 at the sides)
  const fy = th => Yf + Df * (1 - s(th)), ty = th => Yt - Dt * (1 - s(th));
  const at = (th, u) => [x(th), ty(th) + u * (fy(th) - ty(th))];
  const thOf = px => Math.PI + Math.acos(Math.max(-1, Math.min(1, -px / R)));   // (the angle whose x is px)
  const ring = (u, a = Math.PI, b = 2 * Math.PI, n = 48) => [...Array(n + 1)].map((_, i) => at(a + (b - a) * i / n, u));
  return { R, Yf, Yt, Df, Dt, x, s, fy, ty, at, thOf, ring };
}
const ptsOf = pts => pts.map(([x, y]) => `${f1(x)},${f1(y)}`).join(" ");
function hutShellSvg(c, id) {
  const { x0, y0, x1, y1, W, H, k, pad, R } = c, G = hutGeom(c), PI = Math.PI;
  return part(id + "-shell", c.F, () => {
    const top = G.ring(0), bottom = G.ring(1), apex = [0, y0 - H * 0.35];
    // the dome of thatch above, its rafters running up to the peak, rings of it bowing as the wall's top does
    let t = `<polygon points="${f1(x0 - pad)},${f1(y0 - pad)} ${f1(x1 + pad)},${f1(y0 - pad)} ${ptsOf(top.slice().reverse())}" fill="#b8a24a"/>`;
    for (const v of [0.28, 0.52, 0.72]) t += `<polyline points="${ptsOf(top.map(([x, y]) => [x + (apex[0] - x) * v, y + (apex[1] - y) * v]))}" fill="none" stroke="#c9b45a" stroke-width="${f1(5 * k)}"/>`;
    for (let j = 0; j <= 20; j++) { const [x, y] = G.at(PI + PI * j / 20, 0);
      t += `<line x1="${f1(x)}" y1="${f1(y)}" x2="${f1(apex[0])}" y2="${f1(apex[1])}" stroke="${j % 4 ? "#9a8a3a" : "#6e5a2a"}" stroke-width="${f1((j % 4 ? 1.6 : 6) * k)}"/>`; }
    // the wall: a dark ground, then the sticks, upright, each at its angle round the hut, narrower where the wall turns
    t += `<polygon points="${ptsOf(top)} ${ptsOf(bottom.slice().reverse())}" fill="#5a3a1e"/>`;
    const N = 70;
    for (let i = 0; i < N; i++) {
      const th = PI + PI * (i + 0.5) / N, w = G.R * (PI / N) * G.s(th), [xa, ya] = G.at(th, 0), [, yb] = G.at(th, 1);
      if (w < 1.5 * k) continue;
      t += `<line x1="${f1(xa)}" y1="${f1(ya - 4 * k)}" x2="${f1(xa)}" y2="${f1(yb)}" stroke="#5a3a1e" stroke-width="${f1(w)}"/>` +
           `<line x1="${f1(xa)}" y1="${f1(ya - 4 * k)}" x2="${f1(xa)}" y2="${f1(yb)}" stroke="${HUT_STICKS[i % 4]}" stroke-width="${f1(Math.max(1, w - 1.6 * k))}" stroke-linecap="round"/>`;
      if (i % 3 === 1) { const [, yk] = G.at(th, 0.15 + 0.7 * R(i, 2782)); t += `<ellipse cx="${f1(xa)}" cy="${f1(yk)}" rx="${f1(Math.min(2 * k, w / 3))}" ry="${f1(3.5 * k)}" fill="#5a3a1e"/>`; }
    }
    // twine lashing the sticks, two rings round the wall
    for (const u of [0.3, 0.72]) t += `<polyline points="${ptsOf(G.ring(u))}" fill="none" stroke="#d8c48a" stroke-width="${f1(4.5 * k)}"/>` +
      `<polyline points="${ptsOf(G.ring(u + 0.012))}" fill="none" stroke="#b8a46a" stroke-width="${f1(1.5 * k)}"/>`;
    // the eaves' fringe, grass hanging over the wall's top all the way round
    for (let i = 0; i < 90; i++) { const th = PI + PI * (i + 0.5) / 90, [x, y] = G.at(th, 0), l = (12 + 16 * R(i, 2783)) * k * (0.6 + 0.4 * G.s(th)), lean = (R(i, 2784) - 0.5) * 8 * k;
      t += `<path d="M${f1(x)} ${f1(y - 6 * k)} q${f1(lean * 0.5)} ${f1(l * 0.5)} ${f1(lean)} ${f1(l)}" fill="none" stroke="${["#c9b45a", "#a8963a", "#d8c870"][i % 3]}" stroke-width="${f1(3 * k)}" stroke-linecap="round"/>`; }
    // the floor: packed earth inside the wall's foot, straw strewn on it
    t += `<polygon points="${ptsOf(bottom)} ${f1(x1 + pad)},${f1(y1 + pad)} ${f1(x0 - pad)},${f1(y1 + pad)}" fill="#a88456"/>` +
         `<polyline points="${ptsOf(bottom)}" fill="none" stroke="#7a5a34" stroke-width="${f1(6 * k)}" opacity="0.6"/>`;
    for (let i = 0; i < 70; i++) {
      const fx = x0 + W * R(i, 2785), floorAt = G.fy(G.thOf(fx)), fy = floorAt + 10 * k + (y1 - floorAt) * R(i, 2786), a = R(i, 2787) * Math.PI, l = (10 + 14 * R(i, 2788)) * k;
      t += `<line x1="${f1(fx)}" y1="${f1(fy)}" x2="${f1(fx + Math.cos(a) * l)}" y2="${f1(fy + Math.sin(a) * l * 0.35)}" stroke="${i % 2 ? "#d8bf7a" : "#c9ac64"}" stroke-width="${f1(2 * k)}" stroke-linecap="round"/>`;
    }
    return t;
  });
}
// a door of sticks lashed to two crosspieces, in a frame of thicker ones, a twine loop for its handle, in the round
// wall at angle th (narrower the more the wall turns away there): a doorway to `to`, its sign above it, square to us
// (drawn in the wall, before what stands in the room)
function hutDoorSvg(c, th, to, label) {
  const { k } = c, G = hutGeom(c), x = G.x(th), base = G.fy(th), sx = Math.max(0.4, G.s(th)), w = 50 * k, h = 196 * k;
  let t = `<g class="portal" data-to="${to}"><g transform="translate(${f1(x)} ${f1(base)}) scale(${sx.toFixed(3)} 1)">` +
    `<rect x="${f1(-w - 10 * k)}" y="${f1(-h - 10 * k)}" width="${f1(2 * w + 20 * k)}" height="${f1(h + 10 * k)}" rx="${f1(8 * k)}" fill="#4a2e14"/>`;
  for (let i = 0; i < 7; i++)
    t += `<rect x="${f1(-w + i * 2 * w / 7)}" y="${f1(-h)}" width="${f1(2 * w / 7 - 1.5 * k)}" height="${f1(h)}" rx="${f1(5 * k)}" fill="${HUT_STICKS[(i + 1) % 4]}" stroke="#5a3a1e" stroke-width="${f1(k)}"/>`;
  t += [0.2, 0.74].map(u => `<rect x="${f1(-w - 5 * k)}" y="${f1(-h + h * u)}" width="${f1(2 * w + 10 * k)}" height="${f1(13 * k)}" rx="${f1(6 * k)}" fill="#7a5230" stroke="#4a2e14" stroke-width="${f1(k)}"/>`).join("") +
       `<circle cx="${f1(w - 15 * k)}" cy="${f1(-96 * k)}" r="${f1(7 * k)}" fill="none" stroke="#d8c48a" stroke-width="${f1(3 * k)}"/></g>` +
       plankSign(x, base - h - 30 * k, k, label) + `</g>`;
  return t;
}
const HUT_DOOR_TH = 0.28 * Math.PI;   // (how far round from the side a room's door stands: π + this on the left, 2π − this on the right)
// a lantern hanging from the ceiling on a cord, its glow breathing with the slow LFO
// (ticket 516: hung from `hook`, its cord's top, and `y` its middle, when given: the round room's hang from the tree's limbs)
const hutLanternSvg = (c, x, off, hook = c.y0 + c.H * 0.05, y = c.y0 + c.H * 0.2) => { const { k, o, P } = c;
  return `<line x1="${f1(x)}" y1="${f1(hook)}" x2="${f1(x)}" y2="${f1(y - 20 * k)}" stroke="#5a3a1e" stroke-width="${f1(2 * k)}"/>` +
    `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(46 * k)}" fill="#ffe7a0" opacity="0.3">` + sceneLfo(o, "slow", off, "opacity", (v, amp) => (0.3 * (1 + (0.3 + 0.6 * amp) * v)).toFixed(3)) + `</circle>` +
    `<path d="M${P(x - 14 * k, y - 20 * k)} H${f1(x + 14 * k)} L${P(x + 18 * k, y + 18 * k)} H${f1(x - 18 * k)} Z" fill="#ffd36b" stroke="#6e4a28" stroke-width="${f1(3 * k)}"/>` +
    [-0.5, 0, 0.5].map(u => `<line x1="${f1(x + u * 14 * k)}" y1="${f1(y - 20 * k)}" x2="${f1(x + u * 18 * k)}" y2="${f1(y + 18 * k)}" stroke="#6e4a28" stroke-width="${f1(2 * k)}"/>`).join("") +
    `<path d="M${P(x - 10 * k, y - 20 * k)} L${P(x, y - 30 * k)} L${P(x + 10 * k, y - 20 * k)} Z" fill="#6e4a28"/>`; };
// the grass hut from outside, in its own units (as healerSvg), feet in the ground at y = 40 (its front at 49).
// (ticket 281, Matthew: "the exterior ... should also be round -- I'm imagining a cylinder with a dome on top, but more
// complex in terms of shapes that make it up"; "the 'dome' hangs off -- its grass thatching") A cylinder of upright
// sticks, seen a little from above (its foot and its top bow toward us), lashed with two rings of twine; over it a dome
// of thatch, wider than the wall, its eave hanging down over the wall's top all round; the thatch in rings up to the
// peak, each a ragged fringe of grass; the tuft at the peak tied off with twine.
// (ticket 282) sfx keeps its ids apart from another hut's drawn beside it; without a door (a hut of a cluster, not
// its way in), a round window where the door would be.
function hutSvg(sfx = "", door = true) {
  const RW = 64, RE = 94, DH = 104, AY = -18 - DH, bow = t => Math.sin(t);   // (t from 0, the right, round the front, to π, the left; the dome DH tall, its peak at AY)
  const wallAt = (t, y, b = 9) => [RW * Math.cos(t), y + b * bow(t)];
  const arc = (r, y, b, a = 0, z = Math.PI, n = 32) => [...Array(n + 1)].map((_, i) => { const t = a + (z - a) * i / n; return [r * Math.cos(t), y + b * Math.sin(t)]; });
  const pts = P => P.map(([x, y]) => `${f1(x)},${f1(y)}`).join(" ");
  const top = arc(RW, -14, 9), foot = arc(RW, 40, 9), wall = pts(top) + " " + pts(foot.slice().reverse());
  let t = `<defs><linearGradient id="bg-hut-round${sfx}" x1="0" y1="0" x2="1" y2="0"><stop offset="0" stop-color="#000000" stop-opacity="0.35"/>` +
    `<stop offset="0.3" stop-color="#000000" stop-opacity="0"/><stop offset="0.75" stop-color="#000000" stop-opacity="0"/><stop offset="1" stop-color="#000000" stop-opacity="0.4"/></linearGradient>` +
    `<radialGradient id="bg-hut-dome${sfx}" cx="0.38" cy="0.3" r="0.8"><stop offset="0" stop-color="#d8d070"/><stop offset="0.6" stop-color="#b5ae48"/><stop offset="1" stop-color="#8f8a2e"/></radialGradient>` +
    `<clipPath id="bg-hut-wall${sfx}"><polygon points="${wall}"/></clipPath></defs>` +
    `<ellipse cx="0" cy="46" rx="104" ry="14" fill="#000000" opacity="0.16"/><polygon points="${wall}" fill="#5a3a1e"/><g clip-path="url(#bg-hut-wall${sfx})">`;
  // the sticks, each at its angle round the front: narrower toward the sides, where the wall turns away
  for (let i = 0, n = 26; i < n; i++) {
    const tt = Math.PI * (i + 0.5) / n, [x, yt] = wallAt(tt, -14), [, yb] = wallAt(tt, 40), w = RW * Math.PI / n * Math.sin(tt);
    t += `<line x1="${f1(x)}" y1="${f1(yt - 6)}" x2="${f1(x)}" y2="${f1(yb + 2)}" stroke="#5a3a1e" stroke-width="${f1(w)}"/>` +
         `<line x1="${f1(x)}" y1="${f1(yt - 6)}" x2="${f1(x)}" y2="${f1(yb + 2)}" stroke="${HUT_STICKS[i % 4]}" stroke-width="${f1(Math.max(0.8, w - 1.1))}"/>`;
  }
  t += [2, 26].map(y => `<polyline points="${pts(arc(RW, y, 9))}" fill="none" stroke="#d8c48a" stroke-width="2.6"/>`).join("") +
    `<rect x="-66" y="-30" width="132" height="90" fill="url(#bg-hut-round${sfx})"/></g>`;
  // the door, in the front of the wall, its foot on the wall's foot; a round little window, turned with the wall
  const dp = "M-18 49 V8 Q0 -6 18 8 V49 Z";
  if (door) t += `<defs><clipPath id="bg-hut-door${sfx}"><path d="${dp}"/></clipPath></defs><path d="${dp}" fill="#4a2e14"/><g clip-path="url(#bg-hut-door${sfx})">` +
    [-15, -9, -3, 3, 9].map((x, i) => `<rect x="${x}" y="-8" width="5.6" height="60" rx="2.6" fill="${HUT_STICKS[(i + 2) % 4]}" stroke="#5a3a1e" stroke-width="0.7"/>`).join("") +
    [16, 36].map(y => `<rect x="-19" y="${y}" width="38" height="4.5" rx="2" fill="#7a5230"/>`).join("") + `</g>` +
    `<path d="${dp}" fill="none" stroke="#4a2e14" stroke-width="2.5"/><circle cx="11" cy="27" r="2.6" fill="none" stroke="#d8c48a" stroke-width="1.4"/>`;
  else t += `<circle cx="0" cy="12" r="12" fill="#3a2614" stroke="#7a5230" stroke-width="3.5"/><path d="M-12 12 H12 M0 0 V24" stroke="#9a6a3c" stroke-width="2"/>`;
  { const tw = 0.95, [wx, wy] = wallAt(tw, 6), sx = Math.sin(tw);
    t += `<g transform="translate(${f1(wx)} ${f1(wy)}) scale(${sx.toFixed(3)} 1)"><circle r="9" fill="#3a2614" stroke="#7a5230" stroke-width="3"/><path d="M-9 0 H9 M0 -9 V9" stroke="#9a6a3c" stroke-width="1.8"/></g>`; }
  // the dome of thatch: wider than the wall, its eave hanging down over the wall's top
  const eaveFront = arc(RE, -18, 16), eaveBack = arc(RE, -18, -10, Math.PI, 2 * Math.PI);
  const kc = 0.5523, dome = `M${-RE} -18 C${-RE} ${f1(-18 - kc * DH)} ${f1(-kc * RE)} ${AY} 0 ${AY} C${f1(kc * RE)} ${AY} ${RE} ${f1(-18 - kc * DH)} ${RE} -18 ` + eaveFront.map(([x, y]) => `L${f1(x)} ${f1(y)}`).join(" ") + " Z";
  t += `<path d="${dome}" fill="url(#bg-hut-dome${sfx})" stroke="#6e6a22" stroke-width="2"/>`;
  // the thatch in rings up to the peak, each a ragged fringe of grass hanging over the one below
  for (let j = 1; j <= 5; j++) {
    const v = j / 6, r = RE * Math.sqrt(1 - v * v) * 0.98, y = -18 - DH * v, b = 16 * (1 - v);
    const P2 = arc(r, y, b, 0.08, Math.PI - 0.08, 20);
    t += `<polyline points="${pts(P2)}" fill="none" stroke="#9a9434" stroke-width="2"/>`;
    P2.forEach(([x, yy], i) => { const l = 7 + ((i * 5 + j) % 5); t += `<path d="M${f1(x)} ${f1(yy - 1)} q${(i % 3) - 1} ${f1(l / 2)} ${(i % 3) - 1.5} ${l}" stroke="${["#c9c25a", "#a8a23e", "#d8d070"][(i + j) % 3]}" stroke-width="2.2" fill="none" stroke-linecap="round"/>`; });
  }
  // the eave's fringe, longest at the front, where it hangs over the wall
  eaveFront.forEach(([x, y], i) => { if (i % 1) return; const l = 10 + ((i * 7) % 7) + 6 * Math.sin(Math.PI * i / (eaveFront.length - 1));
    t += `<path d="M${f1(x)} ${f1(y - 3)} q${(i % 3) - 1} ${f1(l / 2)} ${(i % 3) - 1.5} ${f1(l)}" stroke="${["#c9c25a", "#a8a23e", "#d8d070"][i % 3]}" stroke-width="2.6" fill="none" stroke-linecap="round"/>`; });
  // the tuft at the peak, tied off with twine
  t += `<g transform="translate(0 ${AY})"><path d="M-8 4 Q-14 -18 -20 -24 M0 2 Q0 -22 2 -32 M8 4 Q14 -16 20 -22" stroke="#a8a23e" stroke-width="3.5" fill="none" stroke-linecap="round"/>` +
    `<rect x="-10" y="-2" width="20" height="7" rx="3" fill="#d8c48a" stroke="#8a7a4a" stroke-width="1"/></g>`;
  return t;
}
// (ticket 282, Matthew: "maybe the exterior should have the option of rendering a single cylinder + dome hut, or a
// cluster of them forming a bigger structure"; "rotational symmetry when there are 3 huts (put into a triad formation),
// or two huts side by side for two, and more rotational symmetry for 4+"; "center of the hut complex is another, larger
// cylinder + dome -- it's a special room called 'hut center', not counted in the number of rooms, which go from 1--6")
// Where each hut of the cluster stands, seen from above, in the hut's own units (hutSvg's): n huts round a ring, one at
// the front when n is odd, two side by side there when it is even, the ring wide enough that neighbors' walls meet and
// their eaves overlap; with the hut center in the middle, HUT_CENTER times as big, wide enough to clear it. dz is how
// far back each stands (drawn HUT_TILT as far up), the list farthest first, as they are drawn.
const HUT_CENTER = 1.3, HUT_TILT = 0.4;
function hutCluster(n, center) {
  const r = n === 1 && !center ? 0 : Math.max([0, 0, 62, 74, 88, 104, 122][n], center ? 150 : 0), L = [];
  for (let i = 0; i < n; i++) { const a = -Math.PI / 2 + 2 * Math.PI * (i + (n % 2 ? 0 : 0.5)) / n; L.push({ x: r * Math.cos(a), dz: r * Math.sin(a), sc: 1 }); }
  if (center) L.push({ x: 0, dz: 0, sc: HUT_CENTER, center: true });
  return L.sort((a, b) => b.dz - a.dz || a.x - b.x);
}
// outside: a meadow at the woods' edge, the hut in the middle, a path to its door, sticks piled for the fire
function hutScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, grad, all, land, o } = c;
  let s = SEP + part("hut-sky", c.F, () => `<defs>${grad("bg-sky", "#7ec8f2", "#eaf7ff", y0, g - 150 * k)}</defs>` + all("url(#bg-sky)") +
    land(g - 215 * k, 9, "#a8d48e", 0.4) + land(g - 160 * k, 6, "#94c874", 1.1));
  s += SEP + cloudsSvg(c, 3, 0.8, 278);
  // the woods behind: a far row of trees, an object at their middle depth
  s += SEP + part("hut-far-woods", c.F, () => {
    let t = "";
    for (let i = 0; i < 30; i++)
      t += i % 3 === 1 ? use("bg-hut-oak", x0 - 30 * k + (W + 60 * k) * i / 29, g - 205 * k + 8 * k * R(i, 2790), k * (0.32 + 0.15 * R(i, 2791)))
                       : use("bg-hut-pine", x0 - 30 * k + (W + 60 * k) * i / 29, g - 205 * k + 8 * k * R(i, 2790), k * (0.3 + 0.16 * R(i, 2791)), k * (0.3 + 0.16 * R(i, 2791)),
                             ` color="${["#2f6e3a", "#3a7d44", "#285e33"][i % 3]}"`);
    return `<defs>${oakDef("bg-hut-oak", 1)}${pineDef("bg-hut-pine", 5)}</defs>` + obj("far woods", g - 200 * k, t);
  });
  // the meadow: grass, the path to the door, flowers either side of it
  s += SEP + part("hut-meadow", c.F, () => {
    let t = land(g - 125 * k, 4, "#86c262", 0.7) +
      `<path d="M${P(-70 * k, y1 + pad)} Q${P(-10 * k, g + 60 * k)} ${P(-22 * k, g + 10 * k)} L${P(22 * k, g + 10 * k)} Q${P(30 * k, g + 60 * k)} ${P(70 * k, y1 + pad)} Z" fill="#d9c08a"/>`;
    const fl = [];
    for (let i = 0; i < 44; i++) {
      const d = R(i, 2792), x = x0 + W * R(i, 2793), y = g - 60 * k + (y1 - g + 60 * k) * d;
      if (Math.abs(x) < (60 + 40 * d) * k) continue;
      fl.push({ y, svg: use(i % 2 ? "bg-daisy" : "bg-tulip", x, y, k * (0.8 + 0.6 * d), k * (0.8 + 0.6 * d), ` color="${FLOWER_COLORS[i % 6]}"`) });
    }
    return `<defs>${FLOWER_DEFS}</defs>` + t + depthRows("flowers", fl, 12 * k);   // (ticket 297: in rows by depth)
  });
  // the hut, or its cluster, every door the way in (one object, at the nearest hut's feet); its smoke rising from the
  // peak of the hut center, or of the farthest hut, swaying with the slow LFO
  const L = hutCluster(Math.max(1, Math.min(6, Math.round(o.hutRooms ?? 1))), !!o.hutCenter), into = o.hutCenter ? "hutcenter" : "hutroom";
  const near = Math.min(...L.map(h => h.dz)), wide = Math.max(...L.map(h => Math.abs(h.x) + 94 * h.sc));
  // (its nearest foot where one hut's is, its tuft's top no higher: (49 + 154) of one hut's units at 2.1)
  const Y = h => -h.dz * HUT_TILT, foot = Math.max(...L.map(h => Y(h) + 49 * h.sc)), tall = Math.max(...L.map(h => -Y(h) + 154 * h.sc));
  const hz = Math.min(2.1, 300 / wide, 426 / (foot + tall)) * k, hy = g + 73 * k - foot * hz;
  const top = L.find(h => h.center) || L[0], sx = (x, y) => P((top.x + x * top.sc) * hz, hy + (Y(top) + y * top.sc) * hz);
  s += SEP + part("hut-building", [], () => `<defs><g id="bg-hut-one">${hutSvg()}</g><g id="bg-hut-plain">${hutSvg("-p", false)}</g></defs>`) +
    obj("hut", hy + foot * hz, `<g ${tf(0, hy, hz)}>` + L.map(h => `<g class="portal" data-to="${h.center ? "hutcenter" : into}">` +
      `<use href="#bg-hut-${h.center || h.dz <= near + 1 ? "one" : "plain"}" ${tf(h.x, Y(h), h.sc)}/></g>`).join("") + `</g>` +
    swaying(o, `<path d="M${sx(4, -150)} Q${sx(-10, -178)} ${sx(6, -204)} T${sx(0, -256)}" fill="none" stroke="#e4e4ea" stroke-width="${f1(9 * hz * top.sc)}" stroke-linecap="round" opacity="0.55"/>`,
      (top.x + 4 * top.sc) * hz, hy + (Y(top) - 150 * top.sc) * hz, 6, 0.3));
  // sticks piled for the fire, beside the hut (an object)
  const aside = wide * hz > W * 0.3 - 80 * k;   // (ticket 282: a wide cluster's woodpile in front of it, to the right)
  s += SEP + part("hut-woodpile", [c.F, aside], () => { const wx = W * (aside ? 0.36 : 0.3), wy = g + (aside ? 80 : 20) * k;
    let t = `<ellipse cx="${f1(wx)}" cy="${f1(wy + 4 * k)}" rx="${f1(70 * k)}" ry="${f1(10 * k)}" fill="#000000" opacity="0.15"/>`;
    for (let row = 0; row < 4; row++) for (let i = 0; i < 6 - row; i++) {
      const sx = wx + (i - (5 - row) / 2) * 22 * k, sy = wy - row * 15 * k;
      t += `<rect x="${f1(sx - 30 * k)}" y="${f1(sy - 8 * k)}" width="${f1(60 * k)}" height="${f1(14 * k)}" rx="${f1(7 * k)}" fill="${HUT_STICKS[(i + row) % 4]}" stroke="#5a3a1e" stroke-width="${f1(1.2 * k)}"/>` +
           `<circle cx="${f1(sx + 30 * k)}" cy="${f1(sy - 1 * k)}" r="${f1(6 * k)}" fill="#d8b48a" stroke="#5a3a1e" stroke-width="${f1(k)}"/>`;
    }
    return obj("woodpile", wy + 4 * k, t); });
  // butterflies over the meadow, bobbing with the quick LFO, their wings with the wings' LFO
  // (ticket 284: an object, as the butterflies are everywhere else)
  s += SEP + obj("butterflies", g - 40 * k, part("hut-butterflies", [c.F, sceneLfoKey(o, "fast"), sceneLfoKey(o, "wings")], () => butterfliesSvg(c, [[-W * 0.32, g - 40 * k, 1.1], [W * 0.18, g - 120 * k, 0.9], [-W * 0.1, g + 30 * k, 1]])));
  return s;
}
// inside: its living room, the couch and the rug as the house's, with the hut's walls; the doors to its other rooms
// (ticket 517: the hut couch's pillows, in the couch's own units; the round room at the top of the tree has them too)
const hutPillowsSvg = pillows => `<defs><g id="bg-hutpillow"><path d="M-36 -34 Q0 -44 36 -34 Q44 0 36 34 Q0 44 -36 34 Q-44 0 -36 -34 Z" fill="currentColor" stroke="#00000033" stroke-width="2"/>` +
      `<path d="M-30 -10 H30 M-30 10 H30" stroke="#00000022" stroke-width="3"/></g></defs>` +
      [...Array(pillows)].map((_, i) => use("bg-hutpillow", -195 + (i % 5) * 97 + (i >= 5 ? 48 : 0), -165 + (i >= 5 ? 22 : 0), 1, 1, ` color="${["#e8c870", "#c0504d", "#f6eedd", "#7cb342", "#ff8c42", "#8e7cc3", "#26a69a", "#d84315"][i % 8]}"`)).join("");
// (ticket 517: the hut's wind chime, hanging from (cx, cy), swinging with the quick LFO)
function windChimeSvg(c, cx, cy) {
  const { k, o } = c;
  const chime = `<line x1="${f1(cx)}" y1="${f1(cy)}" x2="${f1(cx)}" y2="${f1(cy + 40 * k)}" stroke="#5a3a1e" stroke-width="${f1(1.5 * k)}"/>` +
    `<rect x="${f1(cx - 26 * k)}" y="${f1(cy + 40 * k)}" width="${f1(52 * k)}" height="${f1(7 * k)}" rx="${f1(3 * k)}" fill="#7a5230"/>` +
    [-20, -10, 0, 10, 20].map((dx, i) => `<line x1="${f1(cx + dx * k)}" y1="${f1(cy + 47 * k)}" x2="${f1(cx + dx * k)}" y2="${f1(cy + 60 * k)}" stroke="#d8c48a" stroke-width="${f1(k)}"/>` +
      `<rect x="${f1(cx + dx * k - 2.5 * k)}" y="${f1(cy + 60 * k)}" width="${f1(5 * k)}" height="${f1((26 + 6 * (i % 3)) * k)}" rx="${f1(2.5 * k)}" fill="${HUT_STICKS[i % 4]}"/>`).join("");
  return `<g>${sceneLfo(o, "fast", 0, "transform", (v, amp) => `${f1((6 + 12 * amp) * v)} ${f1(cx)} ${f1(cy)}`,
    `<animateTransform attributeName="transform" type="rotate" values="-6 ${f1(cx)} ${f1(cy)};6 ${f1(cx)} ${f1(cy)};-6 ${f1(cx)} ${f1(cy)}" dur="1.4s" repeatCount="indefinite"/>`)}${chime}</g>`;
}
function hutroomScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, P, o } = c;
  const cs = (o.couchSize ?? 100) / 100, pillows = o.pillows ?? 4, ps = (o.picSize ?? 100) / 100;
  let s = SEP + hutShellSvg(c, "hutroom");
  // (ticket 284: its rug, window, wind chime and lantern objects of their own, on the floor and the walls)
  s += SEP + floorObj("rug", part("hutroom-rug", [c.F, o.hutRugColors ?? "straw"], () => rugSvg(c, rugColorsOf(o.hutRugColors ?? "straw"))));
  // the window above the couch: its view (a doorway into it), a frame of sticks, a grass mat rolled up above it
  const wx = 150 * k, wy0 = y0 + H * 0.2, wy1 = y0 + H * 0.44, depth = (o.depth ?? 0) + 1;
  if (depth <= 2) {
    const view = PICTURE_CHOICES.some(([kd]) => kd === o.view) ? o.view : "lake";
    s += SEP + `<g class="portal" data-to="${view}">` + pictureSvg(view, o.viewDetail ?? 80, o, depth, -wx, wy0, 2 * wx, wy1 - wy0) + `</g>`;
  }
  s += SEP + wallObj("window", part("hutroom-window", c.F, () => `<g pointer-events="none">` +
    [[-wx, wy0, 2 * wx, 0], [-wx, wy1, 2 * wx, 0], [-wx, wy0, 0, 1], [wx, wy0, 0, 1]].map(([x, y, l, up]) =>
      `<rect x="${f1(x - 8 * k)}" y="${f1(y - 8 * k)}" width="${f1(up ? 16 * k : l + 16 * k)}" height="${f1(up ? wy1 - wy0 + 16 * k : 16 * k)}" rx="${f1(8 * k)}" fill="#8b5e34" stroke="#4a2e14" stroke-width="${f1(1.5 * k)}"/>`).join("") +
    `<path d="M${P(0, wy0)} V${f1(wy1)}" stroke="#7a5230" stroke-width="${f1(7 * k)}"/>` +
    `<rect x="${f1(-wx - 14 * k)}" y="${f1(wy0 - 30 * k)}" width="${f1(2 * wx + 28 * k)}" height="${f1(24 * k)}" rx="${f1(12 * k)}" fill="#c9b45a" stroke="#8a7a3a" stroke-width="${f1(2 * k)}"/>` +
    [-0.6, 0.6].map(u => `<line x1="${f1(u * wx)}" y1="${f1(wy0 - 30 * k)}" x2="${f1(u * wx)}" y2="${f1(wy0 - 6 * k)}" stroke="#d8c48a" stroke-width="${f1(4 * k)}"/>`).join("") + `</g>`));
  // the doors: to the bedroom on the left, the potion room on the right
  s += SEP + part("hutroom-doors", c.F, () => hutDoorSvg(c, Math.PI + HUT_DOOR_TH, "hutbed", "🛏️ bedroom") + hutDoorSvg(c, 2 * Math.PI - HUT_DOOR_TH, "hutpotion", "🧪 potions"));
  // two pictures high on the walls, over the doors
  { const G = hutGeom(c), xl = G.x(Math.PI + HUT_DOOR_TH);
    s += SEP + picturesSvg(o, "hutroom", [[xl, y0 + H * 0.27, 96 * k], [-xl, y0 + H * 0.27, 96 * k]], ps); }
  // a wind chime of little sticks, swinging with the quick LFO; a lantern, glowing with the slow
  s += SEP + wallObj("wind chime", part("hutroom-hangings", [c.F, sceneLfoKey(o, "fast")], () => {
    const cx = -W * 0.22, cy = y0 + H * 0.05;
    return windChimeSvg(c, cx, cy);
  })) + SEP + wallObj("lantern", part("hutroom-lantern", [c.F, sceneLfoKey(o, "slow")], () => hutLanternSvg(c, W * 0.22, 0.4)));
  // the couch and its pillows, as the house's, in the hut's own colors (one object, standing on its feet)
  s += obj("couch", g - 40 * k, OPEN(`<g id="bg-hutcouch" ${tf(0, g - 40 * k, cs * k)}>`) + couchSvg("hutroom", o.hutCouchColor ?? "#7a5a3a") +
    SEP + part("hutroom-pillows", [pillows], () => hutPillowsSvg(pillows)) +
    CLOSE);
  return s;
}
SEATS.hutroom = (c, B) => SEATS.livingroom(c, B);
// (ticket 299) the grass hut's bed, to lie on, its pillow at our left (where hutbedScene draws it: its mattress's top)
SEATS.hutbed = c => { const { W, g, k } = c, bx = -W * 0.16, by = g + 34 * k, w = 210 * k;
  return [{ name: "bed", x0: bx - w + 12 * k, x1: bx + w - 12 * k, y: by - 108 * k, z: by + 1, facing: ["front"], lie: true, head: "left" }]; };
// (ticket 282) the hut center, the cluster's middle hut and its biggest, its way in when it has one: a round room with
// a door to each of the others (the bedroom on the left, the living room at the back, the potion room on the right), a
// fire in a ring of stones in the middle of the floor, logs either side of it to sit on (as the camp site's), its smoke
// rising to the hole at the dome's peak, where the daylight falls in
const HUT_FIRE = 30;   // (the fire's middle, below the ground line)
function hutcenterScene(c) {
  const { y0, g, W, k, pad, P, o } = c, fy = g + HUT_FIRE * k;
  let s = SEP + hutShellSvg(c, "hutcenter");
  // the daylight from the smoke hole, a pale shaft down to the fire, breathing with the slow LFO
  s += SEP + wallObj("daylight", part("hutcenter-light", [c.F, sceneLfoKey(o, "slow")], () => `<path d="M${P(-70 * k, y0 - pad)} L${P(70 * k, y0 - pad)} L${P(170 * k, fy)} L${P(-170 * k, fy)} Z" fill="#fff6c8" opacity="0.14" pointer-events="none">` +
    sceneLfo(o, "slow", 0.1, "opacity", (v, amp) => (0.14 * (1 + (0.2 + 0.5 * amp) * v)).toFixed(3)) + `</path>`));
  s += SEP + part("hutcenter-doors", c.F, () => hutDoorSvg(c, Math.PI + HUT_DOOR_TH, "hutbed", "🛏️ bedroom") + hutDoorSvg(c, 1.5 * Math.PI, "hutroom", "🛋️ living room") +
    hutDoorSvg(c, 2 * Math.PI - HUT_DOOR_TH, "hutpotion", "🧪 potions"));
  s += SEP + wallObj("lantern, left", part("hutcenter-lantern-l", [c.F, sceneLfoKey(o, "slow")], () => hutLanternSvg(c, -W * 0.3, 0))) +
       SEP + wallObj("lantern, right", part("hutcenter-lantern-r", [c.F, sceneLfoKey(o, "slow")], () => hutLanternSvg(c, W * 0.3, 0.5)));
  // the smoke, rising from the fire to the hole, swaying with the slow LFO
  s += SEP + wallObj("smoke", swaying(o, `<path d="M${P(0, fy - 150 * k)} Q${P(-34 * k, fy - 270 * k)} ${P(12 * k, fy - 380 * k)} T${P(0, y0 - pad)}" fill="none" stroke="#e4e4ea" stroke-width="${f1(22 * k)}" stroke-linecap="round" opacity="0.3" pointer-events="none"/>`,
    0, fy - 150 * k, 5, 0.2));
  // the logs, then the fire (objects, standing on the floor)
  s += SEP + [-1, 1].map(u => obj(u < 0 ? "log, left" : "log, right", fy + 44 * k, part(u < 0 ? "hutcenter-log-l" : "hutcenter-log-r", c.F, () => logSvg(150 * u * k, fy + 30 * k, k)))).join("");
  s += SEP + obj("fire", fy + 14 * k, part("hutcenter-hearth", c.F, () => campfireHearthSvg(0, fy, k * 1.1)) +
    OPEN(`<g pointer-events="none" ${tf(0, fy - 4 * k, k)}>`) + part("hutcenter-flames", [sceneLfoKey(o, "fast"), sceneLfoKey(o, "slow")], () => campfireFlamesSvg(o, true, "night")) + CLOSE);
  return s;
}
SEATS.hutcenter = (c, B) => { const { g, k } = c, fy = g + HUT_FIRE * k;
  return [[-150, 30, "left", "right"], [150, 30, "right", "left"]].map(([dx, dy, where, face]) =>
    ({ name: `log, ${where}`, x0: (dx - 50) * k, x1: (dx + 50) * k, y: fy + (dy - 14) * k, z: fy + (dy + 14) * k + 1, facing: [face] })); };
// its bedroom: a bed of logs with a blanket in its own color, a stump beside it with a candle, a dreamcatcher over it,
// a round window onto the night
// (ticket 517: the hut bedroom's dreamcatcher, its cord from top, its hoop's middle at (dx, dy); the caller sways it)
function dreamcatcherSvg(c, dx, top, dy) {
  const { k, P } = c, r = 26 * k;
  return `<line x1="${f1(dx)}" y1="${f1(top)}" x2="${f1(dx)}" y2="${f1(dy - r)}" stroke="#5a3a1e" stroke-width="${f1(1.5 * k)}"/>` +
    `<circle cx="${f1(dx)}" cy="${f1(dy)}" r="${f1(r)}" fill="none" stroke="#9a6a3c" stroke-width="${f1(4 * k)}"/>` +
    [...Array(8)].map((_, i) => { const a = i * Math.PI / 4; return `<line x1="${f1(dx)}" y1="${f1(dy)}" x2="${f1(dx + Math.cos(a) * r)}" y2="${f1(dy + Math.sin(a) * r)}" stroke="#d8c48a" stroke-width="${f1(1.2 * k)}"/>`; }).join("") +
    `<circle cx="${f1(dx)}" cy="${f1(dy)}" r="${f1(5 * k)}" fill="#3ba3a3"/>` +
    [-14, 0, 14].map((ox, i) => `<line x1="${f1(dx + ox * k)}" y1="${f1(dy + r)}" x2="${f1(dx + ox * k)}" y2="${f1(dy + r + (30 + 8 * (i % 2)) * k)}" stroke="#d8c48a" stroke-width="${f1(1.2 * k)}"/>` +
      `<path d="M${P(dx + ox * k, dy + r + (30 + 8 * (i % 2)) * k)} q${f1(-6 * k)} ${f1(10 * k)} 0 ${f1(24 * k)} q${f1(6 * k)} ${f1(-14 * k)} 0 ${f1(-24 * k)} Z" fill="${["#e8c870", "#f6eedd", "#c0504d"][i]}"/>`).join("");
}
// (ticket 517: the hut's bed of logs, its middle at bx, its feet at by, the blanket in bl)
function hutBedSvg(c, bx, by, bl) {
  const { k, P } = c, w = 210 * k;
  let t = `<ellipse cx="${f1(bx)}" cy="${f1(by + 6 * k)}" rx="${f1(w + 20 * k)}" ry="${f1(16 * k)}" fill="#000000" opacity="0.15"/>`;
  for (let i = 0; i < 8; i++) t += `<rect x="${f1(bx - w + i * 2 * w / 8)}" y="${f1(by - 230 * k)}" width="${f1(2 * w / 8 - 2 * k)}" height="${f1(150 * k)}" rx="${f1(8 * k)}" fill="${HUT_STICKS[i % 4]}" stroke="#5a3a1e" stroke-width="${f1(k)}"/>`;
  t += `<rect x="${f1(bx - w - 8 * k)}" y="${f1(by - 220 * k)}" width="${f1(2 * w + 16 * k)}" height="${f1(14 * k)}" rx="${f1(7 * k)}" fill="#7a5230" stroke="#4a2e14" stroke-width="${f1(k)}"/>` +
    [-1, 1].map(sd => `<rect x="${f1(bx + sd * w - 9 * k)}" y="${f1(by - 100 * k)}" width="${f1(18 * k)}" height="${f1(104 * k)}" rx="${f1(8 * k)}" fill="#7a5230" stroke="#4a2e14" stroke-width="${f1(k)}"/>`).join("") +
    `<rect x="${f1(bx - w)}" y="${f1(by - 70 * k)}" width="${f1(2 * w)}" height="${f1(40 * k)}" rx="${f1(12 * k)}" fill="#8b5e34" stroke="#4a2e14" stroke-width="${f1(1.5 * k)}"/>` +
    `<rect x="${f1(bx - w + 6 * k)}" y="${f1(by - 108 * k)}" width="${f1(2 * w - 12 * k)}" height="${f1(44 * k)}" rx="${f1(16 * k)}" fill="#f3ead2"/>` +
    `<ellipse cx="${f1(bx - w * 0.62)}" cy="${f1(by - 112 * k)}" rx="${f1(52 * k)}" ry="${f1(20 * k)}" fill="#ffffff" stroke="#d8d0bc" stroke-width="${f1(1.5 * k)}"/>` +
    `<path d="M${P(bx - w * 0.36, by - 116 * k)} Q${P(bx + w * 0.2, by - 128 * k)} ${P(bx + w - 6 * k, by - 112 * k)} L${P(bx + w - 6 * k, by - 60 * k)} L${P(bx - w * 0.4, by - 60 * k)} Z" fill="${bl}" stroke="${mix(bl, "#000000", 0.25)}" stroke-width="${f1(2 * k)}"/>`;
  for (let i = 0; i < 5; i++) for (let j = 0; j < 2; j++)
    t += `<rect x="${f1(bx - w * 0.3 + i * w * 0.26)}" y="${f1(by - 108 * k + j * 22 * k)}" width="${f1(w * 0.2)}" height="${f1(16 * k)}" rx="${f1(4 * k)}" fill="${mix(bl, (i + j) % 2 ? "#ffffff" : "#000000", 0.18)}" opacity="0.8"/>`;
  return t;
}
// (ticket 517: the stump with its candle, its foot at (sx, sy), the flame flickering with the quick LFO)
const stumpCandleSvg = (c, sx, sy) => { const { k, o } = c;
  return `<rect x="${f1(sx - 30 * k)}" y="${f1(sy - 70 * k)}" width="${f1(60 * k)}" height="${f1(70 * k)}" rx="${f1(6 * k)}" fill="#8b5e34" stroke="#4a2e14" stroke-width="${f1(1.5 * k)}"/>` +
    `<ellipse cx="${f1(sx)}" cy="${f1(sy - 70 * k)}" rx="${f1(30 * k)}" ry="${f1(9 * k)}" fill="#d8b48a" stroke="#8a6a3a" stroke-width="${f1(1.2 * k)}"/>` +
    `<ellipse cx="${f1(sx)}" cy="${f1(sy - 70 * k)}" rx="${f1(16 * k)}" ry="${f1(4.5 * k)}" fill="none" stroke="#b08a5a" stroke-width="${f1(k)}"/>` +
    `<rect x="${f1(sx - 6 * k)}" y="${f1(sy - 112 * k)}" width="${f1(12 * k)}" height="${f1(40 * k)}" rx="${f1(2 * k)}" fill="#f6eedd"/>` +
    `<ellipse cx="${f1(sx)}" cy="${f1(sy - 122 * k)}" rx="${f1(5 * k)}" ry="${f1(10 * k)}" fill="#ffb53b">` +
      sceneLfo(o, "fast", 0.5, "transform", (v, amp) => `1 ${(1 + (0.1 + 0.2 * amp) * v).toFixed(3)}`, "", "scale") + `</ellipse>` +
    `<circle cx="${f1(sx)}" cy="${f1(sy - 120 * k)}" r="${f1(30 * k)}" fill="#ffe7a0" opacity="0.25"/>`; };
function hutbedScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, o } = c;
  let s = SEP + hutShellSvg(c, "hutbed");
  // (ticket 284: its rug, window and dreamcatcher objects of their own)
  s += SEP + floorObj("rug", part("hutbed-rug", c.F, () => rugSvg(c, rugColorsOf("rose"), 46).replace(/rx="[0-9.]+"/g, m => `rx="${f1(parseFloat(m.slice(4)) * 0.6)}"`)));
  // the round window, high in the middle: the night sky, a moon, stars twinkling with the quick LFO
  s += SEP + wallObj("window", part("hutbed-window", [c.F, sceneLfoKey(o, "fast"), moonOfDateKey(o)], () => { const wx = W * 0.08, wy = y0 + H * 0.28, wr = 52 * k;
    return `<defs><clipPath id="bg-hutbed-win"><circle cx="${f1(wx)}" cy="${f1(wy)}" r="${f1(wr)}"/></clipPath></defs>` +
      `<g clip-path="url(#bg-hutbed-win)"><rect x="${f1(wx - wr)}" y="${f1(wy - wr)}" width="${f1(2 * wr)}" height="${f1(2 * wr)}" fill="#1f2a5a"/>` +
      moonOfDateSvg(o, wx + 18 * k, wy - 16 * k, 14 * k, "#1f2a5a") +   // (ticket 490: the date's moon, where the crescent was)
      [[-30, -20], [-12, -34], [-34, 10], [10, 20], [30, 14], [-6, 0]].map(([dx, dy], i) => `<circle cx="${f1(wx + dx * k)}" cy="${f1(wy + dy * k)}" r="${f1(2.2 * k)}" fill="#ffffff">` +
        sceneLfo(o, "fast", i / 6, "opacity", (v, amp) => (0.6 + (0.2 + 0.2 * amp) * v).toFixed(2)) + `</circle>`).join("") +
      `<ellipse cx="${f1(wx - 20 * k)}" cy="${f1(wy + 50 * k)}" rx="${f1(70 * k)}" ry="${f1(26 * k)}" fill="#1f3a2a"/></g>` +
      `<circle cx="${f1(wx)}" cy="${f1(wy)}" r="${f1(wr)}" fill="none" stroke="#8b5e34" stroke-width="${f1(10 * k)}"/>` +
      `<path d="M${P(wx - wr, wy)} H${f1(wx + wr)} M${P(wx, wy - wr)} V${f1(wy + wr)}" stroke="#7a5230" stroke-width="${f1(4 * k)}"/>`; }));
  // the door back to the living room, on the right
  s += SEP + part("hutbed-door", c.F, () => hutDoorSvg(c, 2 * Math.PI - HUT_DOOR_TH, "hutroom", "🛋️ living room"));
  // the dreamcatcher, hanging over the bed, swaying with the slow LFO
  const bx = -W * 0.16;
  s += SEP + wallObj("dreamcatcher", part("hutbed-dream", [c.F, sceneLfoKey(o, "slow")], () => { const dx = bx, dy = y0 + H * 0.2;
    return swaying(o, dreamcatcherSvg(c, dx, y0 + H * 0.05, dy), dx, y0 + H * 0.05, 5, 0.2); }));
  // the bed: a frame of logs, a headboard of sticks, the mattress, the blanket in its color, a pillow (one object)
  const by = g + 34 * k, bed = part("hutbed-bed", [c.F, o.hutBlanket ?? "#b8434f"], () => hutBedSvg(c, bx, by, o.hutBlanket ?? "#b8434f"));
  // a stump beside the bed, a candle on it, its flame flickering with the quick LFO (an object)
  const sx = bx + 270 * k, sy = g + 20 * k;
  const stump = part("hutbed-stump", [c.F, sceneLfoKey(o, "fast")], () => stumpCandleSvg(c, sx, sy));
  s += SEP + byDepth([["bed", by, bed], ["stump", sy, stump]]);
  return s;
}
// its potion room: a cauldron on its fire, the potion in it bubbling in its own color; shelves of bottles glowing;
// herbs drying from a beam; a table with a mortar, a pestle and a book of recipes
// (ticket 517: a shelf of potions curving along a round wall (G, hutGeom's or topGeom's), u down it, from ta to tb; row
// picks its bottles' colours and kinds; some glowing with the slow LFO)
function hutShelfSvg(c, G, u, row, ta, tb) {
  const { k, o } = c;
  let t = "";
  const plank = G.ring(u, ta, tb, 24);
  t += `<polyline points="${ptsOf(plank)}" fill="none" stroke="#4a2e14" stroke-width="${f1(15 * k)}" stroke-linecap="round"/>` +
       `<polyline points="${ptsOf(plank)}" fill="none" stroke="#7a5230" stroke-width="${f1(11 * k)}" stroke-linecap="round"/>`;
  for (let i = 0; i < 6; i++) {
    const th = ta + (tb - ta) * (i + 0.5) / 6, [bx, sy0] = G.at(th, u), sy = sy0 - 6 * k, z = 0.85 + 0.25 * (1 - G.s(th)), cc = ["#e53935", "#8e24aa", "#43a047", "#1e88e5", "#fdd835", "#ff7043"][(i + row * 2) % 6], kind = (i + row) % 3;
    const glow = (i + row) % 4 === 0 ? `<circle cx="${f1(bx)}" cy="${f1(sy - 22 * k)}" r="${f1(26 * k)}" fill="${cc}" opacity="0.2">` + sceneLfo(o, "slow", (i + row) / 7, "opacity", (v, amp) => (0.2 * (1 + (0.4 + 0.5 * amp) * v)).toFixed(3)) + `</circle>` : "";
    const bottle = kind === 0
      ? `<circle cx="0" cy="-17" r="16" fill="${cc}" fill-opacity="0.85" stroke="#2a2a30" stroke-width="1.5"/><rect x="-5" y="-46" width="10" height="16" fill="#cfe8e0" stroke="#2a2a30" stroke-width="1"/>`
      : kind === 1
      ? `<rect x="-10" y="-52" width="20" height="52" rx="5" fill="${cc}" fill-opacity="0.85" stroke="#2a2a30" stroke-width="1.5"/><rect x="-4" y="-64" width="8" height="12" fill="#cfe8e0" stroke="#2a2a30" stroke-width="1"/>`
      : `<path d="M-6 -40 L-18 0 H18 L6 -40 Z" fill="${cc}" fill-opacity="0.85" stroke="#2a2a30" stroke-width="1.5"/>`;
    t += glow + `<g ${tf(bx, sy, k * z)}>${bottle}<rect x="-4" y="${kind === 1 ? -70 : kind === 0 ? -52 : -48}" width="8" height="7" rx="2" fill="#a87a4a"/></g>`;
  }
  return t;
}
// (ticket 517: a bundle of drying herbs, the i-th, tied at (hx, by), z its size, swaying a little with the slow LFO)
function herbBundleSvg(c, i, hx, by, z) {
  const { k, P, R, o } = c;
  const cc = ["#5f8a3a", "#a78bfa", "#7a9a3a", "#f2c12e", "#4a8a4a"][i % 5], len = (46 + 22 * R(i, 2794)) * k * z;
  const bundle = `<line x1="${f1(hx)}" y1="${f1(by + 8 * k)}" x2="${f1(hx)}" y2="${f1(by + 24 * k)}" stroke="#c9b27a" stroke-width="${f1(1.5 * k)}"/>` +
    [-10, -5, 0, 5, 10].map(dx => `<path d="M${P(hx, by + 26 * k)} Q${P(hx + dx * 0.6 * k * z, by + 26 * k + len * 0.5)} ${P(hx + dx * k * z, by + 26 * k + len)}" stroke="${cc}" stroke-width="${f1(3.5 * k * z)}" fill="none" stroke-linecap="round"/>`).join("");
  return swaying(o, bundle, hx, by + 8 * k, 4, i / 10);
}
// (ticket 517: the cauldron on its fire, in its ring of stones round (px, g), the potion bubbling, timed by the quick LFO)
function hutCauldronSvg(c, px, g, potion) {
  const { k, P, o } = c;
  const py = g - 60 * k, rx = 70 * k;
  let t = `<ellipse cx="${f1(px)}" cy="${f1(g + 12 * k)}" rx="${f1(90 * k)}" ry="${f1(17 * k)}" fill="#6b625a"/>` +
    [...Array(10)].map((_, i) => { const a = i / 10 * 2 * Math.PI; return `<ellipse cx="${f1(px + Math.cos(a) * 86 * k)}" cy="${f1(g + 12 * k + Math.sin(a) * 15 * k)}" rx="${f1(13 * k)}" ry="${f1(7 * k)}" fill="${i % 2 ? "#8a8274" : "#a39a8c"}"/>`; }).join("") +
    [-1, 1].map(e => `<line x1="${f1(px - 46 * k)}" y1="${f1(g + 10 * k + e * 4 * k)}" x2="${f1(px + 46 * k)}" y2="${f1(g + 10 * k - e * 4 * k)}" stroke="#5a3a1a" stroke-width="${f1(9 * k)}" stroke-linecap="round"/>`).join("");
  t += [[-30, 0], [-4, 6], [22, -4], [40, 3]].map(([fx, ph]) => { const x = px + fx * k, b = g + 6 * k, h = (38 + ph) * k;
    return `<path d="M${f1(x - 13 * k)} ${f1(b)} Q${f1(x)} ${f1(b - h)} ${f1(x + 13 * k)} ${f1(b)} Z" fill="#ff8c1a">` + lfoTimed(o, "fast", (fx + 30) / 70, `<animate attributeName="d" dur="${(0.6 + ph / 30).toFixed(2)}s" repeatCount="indefinite" ` +
      `values="M${f1(x - 13 * k)} ${f1(b)} Q${f1(x)} ${f1(b - h)} ${f1(x + 13 * k)} ${f1(b)} Z;M${f1(x - 13 * k)} ${f1(b)} Q${f1(x + 5 * k)} ${f1(b - h * 0.7)} ${f1(x + 13 * k)} ${f1(b)} Z;M${f1(x - 13 * k)} ${f1(b)} Q${f1(x)} ${f1(b - h)} ${f1(x + 13 * k)} ${f1(b)} Z"/>`) + `</path>` +
      `<path d="M${f1(x - 6 * k)} ${f1(b)} Q${f1(x)} ${f1(b - h * 0.5)} ${f1(x + 6 * k)} ${f1(b)} Z" fill="#ffd23f"/>`; }).join("");
  t += [-0.7, 0, 0.7].map(u => `<line x1="${f1(px + u * 48 * k)}" y1="${f1(g - 20 * k)}" x2="${f1(px + u * 60 * k)}" y2="${f1(g + 4 * k)}" stroke="#2a2a30" stroke-width="${f1(7 * k)}" stroke-linecap="round"/>`).join("") +
    `<path d="M${P(px - rx, py)} Q${P(px - rx * 1.14, g - 30 * k)} ${P(px - rx * 0.62, g - 14 * k)} Q${P(px, g - 6 * k)} ${P(px + rx * 0.62, g - 14 * k)} Q${P(px + rx * 1.14, g - 30 * k)} ${P(px + rx, py)} Z" fill="#2b2b30" stroke="#18181c" stroke-width="${f1(3 * k)}"/>` +
    `<ellipse cx="${f1(px)}" cy="${f1(py)}" rx="${f1(rx + 4 * k)}" ry="${f1(18 * k)}" fill="#3a3a42" stroke="#18181c" stroke-width="${f1(3 * k)}"/>` +
    `<ellipse cx="${f1(px)}" cy="${f1(py + 2 * k)}" rx="${f1(rx - 4 * k)}" ry="${f1(13 * k)}" fill="${potion}"/>` +
    `<ellipse cx="${f1(px - 12 * k)}" cy="${f1(py)}" rx="${f1(rx * 0.45)}" ry="${f1(5 * k)}" fill="${mix(potion, "#ffffff", 0.35)}" opacity="0.7"/>`;
  // the potion's bubbles, rising and popping, timed by the quick LFO
  t += [-36, -10, 18, 40].map((dx, i) => `<circle cx="${f1(px + dx * k)}" cy="${f1(py)}" r="${f1((4 + i % 2 * 2) * k)}" fill="${mix(potion, "#ffffff", 0.5)}" opacity="0.8">` +
    lfoTimed(o, "fast", i / 4, `<animate attributeName="cy" values="${f1(py + 2 * k)};${f1(py - 26 * k)}" dur="${(1.1 + 0.3 * i).toFixed(1)}s" repeatCount="indefinite"/>` +
      `<animate attributeName="opacity" values="0.9;0" dur="${(1.1 + 0.3 * i).toFixed(1)}s" repeatCount="indefinite"/>`) + `</circle>`).join("");
  return t;
}
function hutpotionScene(c) {
  const { x0, y0, x1, y1, g, W, H, k, pad, R, P, o } = c;
  const floor = g - HUT_UP * k, potion = o.hutPotion ?? "#7c3aed";
  let s = SEP + hutShellSvg(c, "hutpotion");
  s += SEP + part("hutpotion-door", c.F, () => hutDoorSvg(c, Math.PI + HUT_DOOR_TH, "hutroom", "🛋️ living room"));
  // the shelves on the back wall, right of the middle, curving with it; their bottles, some glowing with the slow LFO
  // (ticket 284: each shelf an object of its own, "shelf of potions 1" to 3, top to bottom)
  [0.36, 0.56, 0.76].forEach((u, row) => { s += SEP + wallObj(`shelf of potions ${row + 1}`, part("hutpotion-shelf-" + row, [c.F, sceneLfoKey(o, "slow")], () => {
    return hutShelfSvg(c, hutGeom(c), u, row, 1.47 * Math.PI, 1.8 * Math.PI);
  })); });
  // herbs drying in bundles from the beam round the wall's top, swaying a little with the slow LFO
  // (ticket 336, Matthew: "unnamed objects hanging ... Let's say they are 'drying-herbs'": each bundle an object of its own,
  // "drying herbs 1" to 6, left to right; the beam they hang from one too)
  s += SEP + wallObj("beam", part("hutpotion-beam", c.F, () => `<polyline points="${ptsOf(hutGeom(c).ring(0.06))}" fill="none" stroke="#6e4a28" stroke-width="${f1(14 * k)}"/>`));
  for (let i = 0; i < 6; i++) s += SEP + wallObj(`drying herbs ${i + 1}`, part("hutpotion-herbs-" + i, [c.F, sceneLfoKey(o, "slow")], () => {
    const G = hutGeom(c);
    let t = "";
    {   // (over the left of the wall: the shelves have the right)
      const th = Math.PI * (1.08 + 0.066 * i), [hx, by0] = G.at(th, 0.06), by = by0 - 2 * k, z = 0.8 + 0.3 * (1 - G.s(th));
      t += herbBundleSvg(c, i, hx, by, z);
    }
    return t;
  }));
  // the table on the left, a mortar and pestle and an open book on it (an object)
  const tx = -W * 0.2, ty = g + 10 * k;
  const table = part("hutpotion-table", c.F, () =>
    `<rect x="${f1(tx - 90 * k)}" y="${f1(ty - 96 * k)}" width="${f1(180 * k)}" height="${f1(16 * k)}" rx="${f1(6 * k)}" fill="#8b5e34" stroke="#4a2e14" stroke-width="${f1(1.5 * k)}"/>` +
    [-70, 70].map(dx => `<rect x="${f1(tx + dx * k - 8 * k)}" y="${f1(ty - 82 * k)}" width="${f1(16 * k)}" height="${f1(84 * k)}" rx="${f1(7 * k)}" fill="#7a5230" stroke="#4a2e14" stroke-width="${f1(k)}"/>`).join("") +
    `<path d="M${P(tx - 62 * k, ty - 98 * k)} Q${P(tx - 50 * k, ty - 128 * k)} ${P(tx - 20 * k, ty - 128 * k)} Q${P(tx - 8 * k, ty - 128 * k)} ${P(tx + 4 * k, ty - 98 * k)} Z" fill="#9a9a9a" stroke="#5a5a60" stroke-width="${f1(1.5 * k)}"/>` +
    `<line x1="${f1(tx - 30 * k)}" y1="${f1(ty - 122 * k)}" x2="${f1(tx - 6 * k)}" y2="${f1(ty - 150 * k)}" stroke="#7a7a80" stroke-width="${f1(7 * k)}" stroke-linecap="round"/>` +
    `<path d="M${P(tx + 14 * k, ty - 98 * k)} L${P(tx + 20 * k, ty - 118 * k)} L${P(tx + 50 * k, ty - 112 * k)} L${P(tx + 80 * k, ty - 118 * k)} L${P(tx + 84 * k, ty - 98 * k)} Z" fill="#f6eedd" stroke="#8a6a3a" stroke-width="${f1(1.5 * k)}"/>` +
    `<line x1="${f1(tx + 50 * k)}" y1="${f1(ty - 112 * k)}" x2="${f1(tx + 50 * k)}" y2="${f1(ty - 98 * k)}" stroke="#8a6a3a" stroke-width="${f1(k)}"/>` +
    [0, 1, 2].map(j => `<line x1="${f1(tx + 26 * k)}" y1="${f1(ty - 112 * k + j * 4 * k)}" x2="${f1(tx + 44 * k)}" y2="${f1(ty - 113 * k + j * 4 * k)}" stroke="#b0a080" stroke-width="${f1(k)}"/>`).join(""));
  // the cauldron on its fire, in the middle: flames flickering with the quick LFO, the potion bubbling (an object)
  const px = W * 0.06, cauldron = part("hutpotion-cauldron", [c.F, sceneLfoKey(o, "fast"), potion], () => hutCauldronSvg(c, px, g, potion));
  s += SEP + byDepth([["table", ty, table], ["cauldron", g, cauldron]]);
  return s;
}
