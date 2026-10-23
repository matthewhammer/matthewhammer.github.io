"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 24 of 26 of the game's script, run in order by index.html.

// ===================== SURPRISE, SAVE, START OVER =====================

const pick = xs => xs[Math.floor(Math.random() * xs.length)];
const rnd = (lo, hi) => lo + Math.floor(Math.random() * (hi - lo + 1));
// one control's surprise
function surpriseOne(part, [kind, key, , ...rest]) {
  if (kind === "range") state[part][key] = rnd(rest[2] ?? rest[0], rest[3] ?? rest[1]);
  // (a star turned any way still looks like a star; other hands keep
  // their usual angle most times)
  else if (kind === "turn")
    state[part][key] = key === "starAngle" || Math.random() < 0.25 ? Math.random() * rest[0](state[part]) : 0;
  else if (kind === "toggle") state[part][key] = Math.random() < 0.3;
  // (the hands are mostly the same)
  else if (kind === "flip") state[part][key] = Math.random() < 0.25;
  else if (kind === "choice" || kind === "places") state[part][key] = pick(rest[0])[0];
  // (a pair's first column has a part of its own: the friend's right hand)
  else if (kind === "pair") for (const c of rest[1]) surpriseOne(key, c);
  // (ticket 518: a hand mostly keeps the costume's own; now and then holds something, or nothing)
  else if (kind === "hold") state[part][key] = Math.random() < 0.7 ? "auto" : pick(HOLD_CHOICES.filter(([v]) => v !== "rein"))[0];
  else if (kind === "color") state[part][key] = pick(rest[0]);
}
document.getElementById("surprise").addEventListener("click", () => {
  // (the music is not part of the surprise, nor what is see-through)
  for (const [, part, ctls] of TABS.filter(t => t[1] !== "music" && t[1] !== "visible" && t[1] !== "controls" && t[1] !== "world" && t[1] !== "advanced" && t[1] !== "access"))
    for (const c of ctls) surpriseOne(part, c);
  if (state.body.pose === "hug") state.body.pose = "stand";   // (a hug needs someone to hug: ticket 136)
  state.background.cauldronFumes = "simple";   // (ticket 206: fancy fumes only when chosen; a slow computer stays smooth)
  // keep the surprises friendly: mostly not sick, not exploded, a normal-ish head
  const F = state.face;
  if (Math.random() < 0.75) F.sick = 0;
  if (Math.random() < 0.6) { F.burst = 0; F.rays = 0; }
  // irises about 4 times in 10, and eye whites around about half of those
  F.iris = Math.random() < 0.4 ? rnd(15, 60) : 0;
  F.whites = F.iris > 0 && Math.random() < 0.5 ? rnd(10, 60) : 0;
  // now and then, an alien: a pointed head and slanted almond eyes
  const alien = Math.random() < 0.15;
  F.alien = alien ? rnd(50, 100) : 0;
  F.tilt = alien ? rnd(15, 35) : 0;
  if (alien) F.oval = rnd(45, 75);
  // and on their own, a somewhat alien head about 1 time in 5, and
  // slanted eyes, up or down, about 1 time in 4
  if (!alien && Math.random() < 0.2) F.alien = rnd(15, 70);
  if (!alien && Math.random() < 0.25) F.tilt = rnd(-20, 35);
  // swimwear now and then (about 1 time in 10), its top to suit its style
  state.undies.on = Math.random() < 0.1;
  state.undies.top = state.undies.style === "bikini" || (state.undies.style !== "onepiece" && Math.random() < 0.2);
  // a costume about 1 time in 4, in its natural colors
  state.costume.kind = Math.random() < 0.25 ? pick(Object.keys(ANIMALS)) : "none";
  if (ANIMALS[state.costume.kind]) Object.assign(state.costume, lookOf(state.costume.kind));
  changed();
});
document.getElementById("reset").addEventListener("click", () => {
  const { volume, mute } = state.music;
  state = clone(DEFAULTS); state.music.volume = volume; state.music.mute = mute; changed();
});
// SAVE: up to three files with one name, stamped with the local date and time
// so each save is new: my-friend-2026-10-10-16-29-05.png, .svg and .json.
// The picture as it is drawn (.png), the drawing itself, which Firefox
// opens and which prints sharp at any size (.svg), and the settings, which
// Load brings back (.json).
function download(blob, name) {
  const a = el("a", { href: URL.createObjectURL(blob), download: name });
  document.body.append(a); a.click(); a.remove();
  setTimeout(() => URL.revokeObjectURL(a.href), 10000);
}
// which files Save writes: all three to start with; unchecking one is
// remembered on this device
const formatBoxes = [...document.querySelectorAll("#formats input")];
// Tablets and phones save one file per tap: an iPad kept only the last of
// three downloads (the .png). There Save writes ONE file, chosen with
// radio buttons (the settings, to load again, unless changed), and the
// other two are a tap away in the links under the buttons. (iPadOS's
// Safari says it is a Mac; its touch points give it away.)
const oneFile = /iPad|iPhone|iPod|Android/.test(navigator.userAgent) ||
                (/Macintosh/.test(navigator.userAgent) && navigator.maxTouchPoints > 1);
if (oneFile) {
  document.getElementById("formats-label").textContent = "save (one file per tap here):";
  let chosen = "json";
  try { chosen = localStorage.getItem("dress-up-save-one") || "json"; } catch (_) {}
  for (const b of formatBoxes) {
    b.type = "radio"; b.name = "save-format"; b.checked = b.value === chosen;
    b.addEventListener("change", () => {
      try { localStorage.setItem("dress-up-save-one", b.value); } catch (_) {}
    });
  }
} else {
  try {
    const off = JSON.parse(localStorage.getItem("dress-up-save-off") || "[]");
    for (const b of formatBoxes) b.checked = !off.includes(b.value);
  } catch (_) {}
  for (const b of formatBoxes) b.addEventListener("change", () => {
    try { localStorage.setItem("dress-up-save-off", JSON.stringify(formatBoxes.filter(x => !x.checked).map(x => x.value))); } catch (_) {}
  });
}
const wanted = f => formatBoxes.some(b => b.value === f && b.checked);
// Several downloads from one click: some browsers let only the first
// through, ask first ("allow multiple downloads?"), or drop ones that
// start after the click has finished (the .png is drawn first). So the
// .svg and .json start during the click, the .png right after, and each
// file also gets a link under the buttons: a click on a link always works.
// (ticket 275, Matthew: "another .json file format that's called a 'full snapshot' file, where we store the date and
// time it was created --- that one is a super-set of the .json file format we want for the picture archive") The
// .json Save writes is a full snapshot: the picture file, { app, version, state }, which alone decides the picture
// (ticket 274), and "extra", fields that (1) do not decide the picture and (2) are there for the person who saved it:
// when it was saved, and with which version of the game (the one that draws it the same again). Load never reads
// "extra"; a tool keeps the picture file by dropping it, knowing none of its names (the picture archive keeps that).
const snapshotFile = (d = new Date()) => ({ app: "dress-up", version: 1, extra: { saved: d.toISOString(), game: VERSION.number }, state });
const pictureFile = file => { const { extra, saved, ...p } = file; return p; };   // (saved: a top-level time, before 0.0.203)
document.getElementById("save").addEventListener("click", () => {
  if (!formatBoxes.some(b => b.checked)) { alert("Check at least one kind of file to save."); return; }
  const d = new Date(), two = n => String(n).padStart(2, "0");
  const name = `my-friend-${d.getFullYear()}-${two(d.getMonth() + 1)}-${two(d.getDate())}-` +
               `${two(d.getHours())}-${two(d.getMinutes())}-${two(d.getSeconds())}`;
  const pic = document.getElementById("pic");
  const vb = pic.viewBox.baseVal, scale = 1000 / vb.height;
  // a standalone copy at its own size; both the .svg and the .png keep a
  // transparent background
  const copy = unrastered(pic.cloneNode(true));   // (ticket 509: its pictures as SVG again)
  copy.setAttribute("width", Math.round(vb.width * scale));
  copy.setAttribute("height", 1000);
  const svgText = '<?xml version="1.0" encoding="UTF-8"?>\n' + new XMLSerializer().serializeToString(copy);
  const saved = document.getElementById("saved");
  saved.replaceChildren("Saved:");
  const files = [];
  // each file: saved now if it is wanted, and a link either way when only
  // one file can be saved per tap (so the others are one tap away)
  const give = (blob, file) => {
    const kind = file.split(".").pop();
    if (wanted(kind)) download(blob, file);
    else if (!oneFile) return;
    const a = el("a", { href: URL.createObjectURL(blob), download: file, title: "save " + file }, kind);
    files.push([kind, a]);
    const order = ["json", "png", "svg"];
    const links = files.slice().sort((x, y) => order.indexOf(x[0]) - order.indexOf(y[0])).map(x => x[1]);
    saved.replaceChildren(...(oneFile
      ? ["Saved the ." + formatBoxes.find(b => b.checked).value + " — tap to save: ",
         ...links.flatMap((x, k) => k ? [" · ", x] : [x])]
      : ["Saved ", ...links.flatMap((x, k) => k ? [" · ", x] : [x]), " — not in your downloads? click each one."]));
  };
  if (wanted("svg") || oneFile) give(new Blob([svgText], { type: "image/svg+xml" }), name + ".svg");
  if (wanted("json") || oneFile)
    give(new Blob([JSON.stringify(snapshotFile(d), null, 2)],
                  { type: "application/json" }), name + ".json");
  if (!wanted("png") && !oneFile) return;
  const img = new Image();
  img.onload = () => {
    const c = document.createElement("canvas");
    c.width = Math.round(vb.width * scale); c.height = 1000;
    c.getContext("2d").drawImage(img, 0, 0);
    c.toBlob(blob => give(blob, name + ".png"));
  };
  img.onerror = () => saved.append(" (the .png couldn't be drawn)");
  img.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(svgText);
});
// (ticket 43) A POSTCARD: the picture as Save draws its .png, in a card's
// cream border with a thin frame round the picture, and a line written
// under it (the picture's own line to start with, or whatever is typed),
// as one .png ready to send: my-postcard-2026-10-16-21-55-04.png.
function postcardBlob(caption) {
  return new Promise((resolve, reject) => {
    const pic = document.getElementById("pic"), vb = pic.viewBox.baseVal, scale = 1000 / vb.height;
    const w = Math.round(vb.width * scale), h = 1000, pad = 48, band = caption ? 160 : 0;
    const copy = unrastered(pic.cloneNode(true));   // (ticket 509: its pictures as SVG again)
    copy.setAttribute("width", w); copy.setAttribute("height", h);
    const img = new Image();
    img.onload = () => {
      const c = document.createElement("canvas"); c.width = w + 2 * pad; c.height = h + 2 * pad + band;
      const g = c.getContext("2d");
      g.fillStyle = "#fffdf7"; g.fillRect(0, 0, c.width, c.height);
      g.drawImage(img, pad, pad, w, h);
      g.strokeStyle = "#d8cdb8"; g.lineWidth = 4; g.strokeRect(pad - 2, pad - 2, w + 4, h + 4);
      if (caption) {
        const font = px => `${px}px "Comic Sans MS", "Chalkboard SE", "Marker Felt", "Comic Neue", cursive`;
        let px = 64; g.font = font(px);
        // smaller until it fits, to a point; then two lines, split at the space nearest the middle
        while (px > 36 && g.measureText(caption).width > w) { px -= 4; g.font = font(px); }
        let lines = [caption];
        if (g.measureText(caption).width > w) {
          let at = -1;
          for (let i = 0; i < caption.length; i++)
            if (caption[i] === " " && (at < 0 || Math.abs(i - caption.length / 2) < Math.abs(at - caption.length / 2))) at = i;
          if (at > 0) lines = [caption.slice(0, at), caption.slice(at + 1)];
        }
        g.fillStyle = "#3a2f28"; g.textAlign = "center"; g.textBaseline = "middle";
        const mid = h + 2 * pad + band / 2 - pad / 2;
        lines.forEach((l, k) => g.fillText(l, c.width / 2, mid + (k - (lines.length - 1) / 2) * px * 1.15, w));
      }
      c.toBlob(b => b ? resolve(b) : reject(new Error("no picture")), "image/png");
    };
    img.onerror = () => reject(new Error("the picture couldn't be drawn"));
    img.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(copy));
  });
}
document.getElementById("postcard").addEventListener("click", async () => {
  const d = new Date(), two = n => String(n).padStart(2, "0");
  const name = `my-postcard-${d.getFullYear()}-${two(d.getMonth() + 1)}-${two(d.getDate())}-` +
               `${two(d.getHours())}-${two(d.getMinutes())}-${two(d.getSeconds())}.png`;
  const saved = document.getElementById("saved");
  try {
    const blob = await postcardBlob(document.getElementById("postcard-text").value.trim());
    download(blob, name);
    // (drawn after the click: a browser may not let the download through, so a link too)
    saved.replaceChildren("Saved ", el("a", { href: URL.createObjectURL(blob), download: name, title: "save " + name }, "the postcard"),
                          oneFile ? "" : " — not in your downloads? click it.");
  } catch (e) { saved.replaceChildren("The postcard couldn't be drawn."); }
});
// LOAD: a saved .json (or a bare state object); anything it lacks keeps
// its default, so friends saved before a feature existed still load. Only its
// state is read: never its extra fields (ticket 275), nor an older file's saved time.
document.getElementById("load").addEventListener("click", () => document.getElementById("load-file").click());
document.getElementById("load-file").addEventListener("change", async e => {
  const file = e.target.files[0];
  e.target.value = "";
  if (!file) return;
  try {
    const data = JSON.parse(await file.text());
    const saved = data && data.state ? data.state : data;
    if (!saved || typeof saved !== "object") throw new Error("no settings in this file");
    const next = clone(DEFAULTS);
    for (const k of Object.keys(DEFAULTS)) next[k] = { ...DEFAULTS[k], ...(saved[k] || {}) };
    upgradeHands(next.hands);
    upgradeRightHand(next, saved);
    upgradeFeet(next, saved);
    upgradeHorse(next, saved);
    upgradeLassoHand(next);
    upgradeInside(next.background, saved);
    upgradeFigSize(next.background, saved);
    upgradeFoamMove(next.background, saved);
    upgradeThings(next, saved);
    upgradeWorld(next);
    upgradeClickWalk(next, saved);
    upgradePitShapes(next, saved);
    upgradeFace(next.face, saved.face);
    next.music = { ...next.music, volume: state.music.volume, mute: state.music.mute };
    state = next;
    changed();
  } catch (err) {
    alert("That file didn't load: " + err.message);
  }
});
