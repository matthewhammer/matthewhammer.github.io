"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 22 of 26 of the game's script, run in order by index.html.

// ===================== FULL SCREEN =====================
// The picture fills the window, the controls hidden -- and the screen too,
// where the browser allows it (not an iPhone's); an X, Escape, or leaving
// the browser's full screen brings the controls back.
const maxBtn = document.getElementById("max");
// (keepScreen: the music's full screen takes over, and the screen stays full)
function setBig(on, keepScreen) {
  document.body.classList.toggle("big", on);
  if (!on) setHud(false);
  maxBtn.textContent = on ? "✕" : "⤢";
  maxBtn.title = on ? "Back to the controls" : "Full screen";
  maxBtn.setAttribute("aria-label", maxBtn.title);
  if (keepScreen) return;
  try {
    if (on && document.documentElement.requestFullscreen && !document.fullscreenElement)
      document.documentElement.requestFullscreen().catch(() => {});
    else if (!on && document.fullscreenElement) document.exitFullscreen().catch(() => {});
  } catch (e) { /* the window-filling picture is enough */ }
}
maxBtn.addEventListener("click", () => setBig(!document.body.classList.contains("big")));
// (ticket 159, Matthew) The 🎵 Music tab full screen: what is playing, where
// the tune is, the tunes and the mixing board take the whole window (the
// picture, the title and the other tabs hidden), and the screen where the
// browser allows; ✕, Escape or leaving the browser's full screen bring the
// game back.
// (ticket 170, Matthew) It opens from the game's full screen too (🎚️, next
// to the mute): then ✕ goes back there, the screen still full, the tab and
// the HUD as they were; leaving the browser's full screen (leaving) goes
// back to the game with its controls.
const MUSIC_BIG = ["⛶ the music, full screen", "✕ back to the game"];
let musicFrom = null;
function setMusicBig(on, leaving) {
  const musicTab = TABS.findIndex(t => t[1] === "music");
  if (on && document.body.classList.contains("big")) {
    musicFrom = { tab, hud: !hud.hidden };
    setBig(false, true);
    if (tab !== musicTab) { tab = musicTab; showTab(); }
  }
  designerShell("music", on);
  const from = on ? null : musicFrom;
  if (!on) musicFrom = null;
  if (from && from.tab !== tab) { tab = from.tab; showTab(); }
  if (from && !leaving) { setBig(true); if (from.hud) setHud(true); return; }
  designerScreen(on);
}
// (ticket 210, Matthew: "full screen mode for world designer, follow the music
// panel for a model") The 🗺️ World design tab full screen, as the music's:
// its controls take the whole window, the grid as big as it allows, and the
// screen where the browser allows; ✕, Escape or leaving the browser's full
// screen bring the game back.
const WORLD_BIG = ["⛶ the world designer, full screen", "✕ back to the game"];
// (ticket 285, ^du_report(5)'s step 1: "the designer's shell the two full screens share") A tab full screen as a designer:
// body.designer-big, with the designer's own class (music-big, world-big); its controls the whole window under a slim
// bar that names the tab, its chip there saying ✕ back; ✕, Escape or leaving the browser's full screen going back.
// DESIGNERS: each one's class, its chip's class (also designer-chip, which the bar's CSS keys on) and words.
// (ticket 285, ^du_report(5)'s step 2) and 🏞️ Background as a designer, made for one place (setBgBig)
const BG_BIG = ["⛶ the background designer, full screen", "✕ back to the game"];
const TAB_BIG = ["⛶ full screen", "✕ back to the game"];   // (ticket 375: every other tab, a designer the same way)
// (ticket 365, Matthew: "castle designer FS mode -- after we do knowlling on all panels") 🏰 the castle designer full screen: the
// background designer's two columns (bg-big: its boxes, the game's card, the dragger between) holding only 🏰 the castles, the
// map big across the top, every room a box of its own in order, its picture first (the one you are in marked), then the castle's
// look, its themes and who lives there (style.css, body.castle-big)
const CASTLE_BIG = ["⛶ the castle designer, full screen", "✕ back to the game"];
const DESIGNERS = { music: { cls: "music-big", chip: "musicbig", words: MUSIC_BIG }, world: { cls: "world-big", chip: "worldbig", words: WORLD_BIG },
                    background: { cls: "bg-big", chip: "bgbig", words: BG_BIG }, tab: { cls: "tab-big", chip: "tabbig", words: TAB_BIG },
                    castle: { cls: "castle-big", chip: "castlebig", words: CASTLE_BIG } };
function designerShell(name, on) {
  const D = DESIGNERS[name];
  document.body.classList.toggle(D.cls, on);
  document.body.classList.toggle("designer-big", Object.values(DESIGNERS).some(x => document.body.classList.contains(x.cls)));
  for (const b of document.querySelectorAll("button." + D.chip)) b.textContent = D.words[on ? 1 : 0];
  if (on) document.querySelector(".side").scrollTop = 0;
}
// (the screen too, where the browser allows: the window-filling designer is enough where it does not)
function designerScreen(on) {
  try {
    if (on && document.documentElement.requestFullscreen && !document.fullscreenElement)
      document.documentElement.requestFullscreen().catch(() => {});
    else if (!on && document.fullscreenElement) document.exitFullscreen().catch(() => {});
  } catch (e) { /* the window-filling designer is enough */ }
}
function setWorldBig(on) {
  designerShell("world", on);
  placePic();
  designerScreen(on);
}
function setBgBig(on) {
  if (!on && document.body.classList.contains("castle-big")) { castleFrom = null; designerShell("castle", false); changed(); }   // (ticket 365: never the castle's without the background's)
  designerShell("background", on);
  placePic();
  designerScreen(on);
}
// (ticket 365) from the side panel (✕ back to the game) or from the background designer (✕ back to it); leaving: the browser's full
// screen left, all the way back
let castleFrom = null;
function setCastleBig(on, leaving) {
  if (on === document.body.classList.contains("castle-big")) return;
  if (on) {
    castleFrom = document.body.classList.contains("bg-big") ? "bg" : "side";
    const bgTab = TABS.findIndex(t => t[1] === "background");
    if (tab !== bgTab) { tab = bgTab; showTab(); }
  }
  const toBg = !on && castleFrom === "bg" && !leaving;
  designerShell("castle", on);
  if (!toBg) designerShell("background", on);
  if (on && castleFrom === "bg") for (const b of document.querySelectorAll("button.castlebig")) b.textContent = "✕ back to the background designer";
  if (!on) castleFrom = null;
  changed();   // (every room's box shown or the one again, the map as big as it gets or small again)
  placePic();
  if (!toBg) designerScreen(on);
}
function setTabBig(on) {
  designerShell("tab", on);
  placePic();
  designerScreen(on);
}
// (ticket 375, ^du_report(5)'s rules 7 and 8: every big tab a designer that opens with the game as its first card; the side panel the
// designer's boxes, smaller, not a different layout) A tab without a designer of its own, once laid out: its ⛶ chip first (the one way
// in, as the music's, the world's and the background's), then its content and, beside it in the full screen, the game's card (placePic
// puts the picture in it) with the dragger between; outside the full screen the two columns are display: contents, the panel as it was
function tabDesigner(panel, name) {
  const kids = [...panel.children];
  // (Matthew, 2026-10-23: "top-level FS chip for scripts should be the same as 'max size' on Common please") the 📜 Scripting tab's
  // ⛶ opens the scripts' own full size, at 📚 Common (scriptMaxOpen), not the tab's designer
  const chip = el("button", { class: "tabbig designer-chip", onclick: () => {
    const common = !document.body.classList.contains("tab-big") && panel.querySelector('.script-box[data-room="Common"] .script-max-btn');
    if (common) { common.click(); return; }
    setTabBig(!document.body.classList.contains("tab-big"));
  } }, document.body.classList.contains("tab-big") ? TAB_BIG[1] : TAB_BIG[0]);
  const bar = el("div", { class: "ctl tab-bar", "data-name": name }, [chip]);
  const view = el("div", { class: "world-view tab-view" }, [el("div", { class: "row world-view-head" }, [
    el("button", { class: "eye", onclick: () => worldViewShow(worldViewHidden) }, "👁️"), el("span", { class: "hint" }, "📺 the game")]), el("div", { class: "tab-view-pic" })]);
  worldViewMark(view);
  const split = el("div", { class: "world-split tab-split", role: "separator", "aria-orientation": "vertical", title: "Drag: more room for the boxes, or for the game" });
  const layout = el("div", { class: "tab-layout" }, [el("div", { class: "tab-content" }, kids), split, el("div", { class: "tab-viewcol" }, [view])]);
  designerSplit(layout, split, "dress-up-tab-split");
  panel.replaceChildren(bar, layout);
}
// (ticket 375) ⚙️ Advanced, one long row of seventeen parts till now, in boxes: 🌗 the look, 🏠 this game (its page, its code, See
// also, its version), ✈️ without the Internet (ticket 486), 🎮 playing (a pop-up for new versions, the levers), 🧪 tests and timing, 🚀 effects kept off till fast (its long note a
// hint on the box's name), 📰 what's new (last, as it was)
function advLayout(panel) {
  const root = panel.querySelector(":scope > .ctl"); if (!root) return;
  const kids = [...root.children], txt = c => (c.textContent || "").trim(), lbl = kids.find(c => c.classList.contains("lbl"));
  const pick = test => kids.filter(c => !c.dataset.boxed && test(c)).map(c => { c.dataset.boxed = "1"; return c; });
  const box = (name, list, hint) => { if (!list.length) return null; const h = el("h3", { class: "box-name", ...(hint ? { title: hint } : {}) }, [name]);
    return el("section", { class: "box tabbox advbox" }, [h, ...list]); };
  const note = kids.find(c => c.classList.contains("hint") && /^🚀/.test(txt(c)));
  const made = [
    box("🌗 the look", pick(c => c.classList.contains("theme-row"))),
    box("🏠 this game", pick(c => c.classList.contains("row") && (/^(\u{1F3E0}|\u{1F4BB})/u.test(txt(c)) || !!c.querySelector("button.newer") || c.classList.contains("see-also") || c.classList.contains("licence")))),   // (the version's row by its button: its words come after; ticket 474's See also, near the top, between the code and the version; ticket 500's licence, under the code)
    box("✈️ without the Internet", pick(c => c.classList.contains("offline-help"))),   // (ticket 486: adding the game to a device, for airplane mode, near the top where a parent looks)
    box("🎮 playing", pick(c => (c.classList.contains("row") && /^(🔔|🎚️)/.test(txt(c))) || (c.classList.contains("hint") && !/^🚀/.test(txt(c))))),   // (and its note, filled as it is used)
    box("🧪 tests and timing", pick(c => c.classList.contains("perfrow") && /^(🧪|⏱️)/.test(txt(c)))),
    box("🚀 kept off till they are fast", pick(c => c.classList.contains("perfrow")), note ? txt(note).replace(/^🚀\s*/, "") : ""),
    box("📰 what's new", pick(c => /news/.test(c.className)).filter(c => !c.classList.contains("news-cap"))),
  ].filter(Boolean);
  if (note) note.remove();
  for (const c of kids) if (c.classList.contains("news-cap")) c.remove();   // (the box names it)
  if (lbl) lbl.style.display = "none";   // ("⚙️ advanced": the tab names it)
  const rest = kids.filter(c => c.isConnected && c.parentNode === root && c !== lbl);
  root.replaceChildren(...(lbl ? [lbl] : []), el("div", { class: "boxes tabboxes" }, [...made, ...(rest.length ? [box("⚙️ more", rest)] : [])]));
  // (a test's long account of what it does, folded under ℹ️, open on a tap: rule 4, a row on one line, and a fold that works on a touch
  // screen, where a hint on pointing does not)
  for (const h of root.querySelectorAll(".advbox .perfrow > .hint")) if ((h.textContent || "").length > 140) {
    const d = el("details", { class: "how" }, [el("summary", {}, ["ℹ️ how it works"])]); h.before(d); d.append(h); }
}
// (ticket 375) Every group (a box inside a box: .chipset.ctlgroup, with its name in .cap), in any tab: nothing in it repeats its name
// (🅰️ theme A's rows were "🅰️ color", "🅰️ size"), and a bubble theme's group shows its bubble by its name, drawn from its settings
// (the report's "show the thing": a theme as what it draws)
const LEAD_MARK = /^[\p{Extended_Pictographic}\u200D\uFE0F\u20E3]+\s*/u;
// (a castle's theme as a little flag on its pole, its carpet a strip under it, kept in step with its settings)
// (ticket 365: one castleOf for each castle in a round of syncs, not one for each of its 84 flags; the settings stay as they are while
// the syncs run, and the castles are forgotten once the round is over)
let flagCastles = null;
const flagCastle = id => { if (!flagCastles) { flagCastles = {}; queueMicrotask(() => { flagCastles = null; }); } return flagCastles[id] || (flagCastles[id] = castleOf(castleView(state.background, id))); };
function themeFlag(id, theme, big) {
  const sv = document.createElementNS("http://www.w3.org/2000/svg", "svg"); sv.setAttribute("viewBox", "0 0 30 30"); sv.setAttribute("aria-hidden", "true"); sv.setAttribute("class", big ? "flag-pic" : "flag-chip");
  let was = "";
  syncs.push(() => { const K = flagCastle(id), T = theme === "B" ? { id, ...K.themeB } : { id, theme: "A", flagA: K.flagA, flagB: K.flagB, flagStyle: K.flagStyle, carpet: K.carpet };
    const k = JSON.stringify(T); if (k === was) return; was = k;
    sv.innerHTML = castleFlagDefs(T, 3) + `<rect x="3" y="25" width="24" height="4" rx="1" fill="${T.carpet}"/><line x1="6" y1="2" x2="6" y2="24" stroke="#8a8f9a" stroke-width="1.6"/>` +
      (T.flagStyle === "alternate" ? `<path d="M7 3 H27 V10 H7 Z" fill="${T.flagA}"/><path d="M7 10 H27 V17 L17 13 L7 17 Z" fill="${T.flagB}"/><path d="M7 3 H27 V17 L17 13 L7 17 Z" fill="none" stroke="#555" stroke-width="0.6"/>`   // (in turn: both, one over the other)
        : `<path d="M7 3 H27 V17 L17 13 L7 17 Z" fill="${castleFlagFill(T, 0, 3)}" stroke="#555" stroke-width="0.6"/>`); });
  return sv;
}
function tidyGroups(panel) {
  // (each room's "🎨 its colors": its two chips each with its theme's flag, so the choice shows what it chooses: one decision, seen)
  for (const r of panel.querySelectorAll('.ctl[data-key^="castleRoomTheme"]')) { const id = r.dataset.key.slice(-1);
    for (const b of r.querySelectorAll(":scope > .row > button")) { const t = /theme B/.test(b.textContent) ? "B" : /theme A/.test(b.textContent) ? "A" : null; if (t) b.prepend(themeFlag(id, t, 0)); } }
  for (const g of panel.querySelectorAll(".chipset.ctlgroup")) {
    const cap = g.querySelector(":scope > .cap"); if (!cap) continue;
    const ct = cap.textContent.trim(), mark = (ct.match(LEAD_MARK) || [""])[0].trim(), word = ct.split(" ").pop();
    for (const n of g.querySelectorAll(":scope > .ctl > .lbl .name")) {
      let t = n.textContent.trim();
      if (mark && t.startsWith(mark)) t = t.slice(mark.length).trim();
      const m = word && t.match(new RegExp("^" + word.replace(/[.*+?^${}()|[\]\\]/g, "\\$&").replace(/s$/, "") + "s?(?:'s)?\\s+(\\S.*)$", "u"));
      if (m) t = m[1];
      if (t && t !== n.textContent) n.textContent = t;
    }
    // (the report's step 3, a castle's themes: each drawn as its flag, its carpet under it, by its name)
    const flagRow = g.querySelector(':scope > .ctl[data-key^="castleFlagA"]'), fm = flagRow && flagRow.dataset.key.match(/^castleFlagA(_B)?([A-Z])$/);
    if (fm) { const pic = themeFlag(fm[2], fm[1] ? "B" : "A", 1); pic.classList.add("cap-pic"); cap.prepend(pic); }
    const first = g.querySelector(':scope > .ctl[data-key$="Color"]'), L = first && (first.dataset.key.match(/^bubble([ABC])Color$/) || [])[1];
    if (L && typeof makerTheme === "function" && typeof bubbleSvg === "function") {
      const sv = document.createElementNS("http://www.w3.org/2000/svg", "svg"); sv.setAttribute("viewBox", "-15 -15 30 30"); sv.setAttribute("class", "cap-pic"); sv.setAttribute("aria-hidden", "true");
      cap.prepend(sv); let was = "";
      syncs.push(() => { const th = makerTheme(state.background, L), k = JSON.stringify(th); if (k !== was) { was = k; sv.innerHTML = bubbleSvg(0, 0, 12, th); } });
    }
  }
}
// (ticket 392, Matthew: "there are still redundant headers (pose panel says "the pose" and then "poses", similar for the facing
// and moving panel next to it). Same for dominant hand -- the controls in the panel are already labeled enough, and there isn't
// more than one sub control.") A box says its name once. With one row or group showing, that row's name or group's header is
// the box's own, hidden (its words kept as the box name's tooltip; a slider keeps its name); with several, a row's name or a
// group's header whose words are all the box's ("↔️ facing" in ↔️ facing and moving, "🤸 poses" in 🤸 the pose) is hidden.
// A sync, since rows show and hide (a place's own box holds every place's rows) and a box's name can change with the place.
const HEAD_SKIP = new Set(["the", "its", "her", "his", "and", "&", "of", "both"]);   // (not "a": 🅰️ theme A is theme A)
const headWords = t => t.replace(/[\p{Extended_Pictographic}\u200D\uFE0F]/gu, " ").toLowerCase().split(/[\s,:;()]+/)
  .map(w => w.replace(/'s$/, "").replace(/s$/, "")).filter(w => w && !HEAD_SKIP.has(w));
// (a header's words all the box's; a word ending in one of the box's counts: "eyebrows" in 🤨 the brows)
const headRepeats = (head, name) => { const hw = headWords(head), nw = headWords(name);
  return hw.length > 0 && hw.every(w => nw.some(b => w === b || (b.length >= 4 && w.endsWith(b)))); };
const rowHead = r => r.matches(".ctlgroup") ? r.querySelector(":scope > .cap") : r.querySelector(":scope > .lbl .name");
function boxHeads(box, name, rows) {
  const heads = rows.map(r => [r, rowHead(r)]).filter(([, h]) => h), boxName = box.querySelector(":scope > .box-name");
  if (!heads.length) return;
  let was = "";
  syncs.push(() => {
    const shown = rows.filter(r => !r.hidden), nm = name(), key = nm + "|" + shown.length + "|" + heads.map(([r]) => r.hidden ? 0 : 1).join("");
    if (key === was) return;
    was = key;
    let tip = "";
    for (const [r, h] of heads) {
      const text = h.textContent.trim(), slider = !!r.querySelector(":scope > input[type=range]");
      const dup = !slider && !r.hidden && !h.hidden && (shown.length === 1 || headRepeats(text, nm));
      // (a row's label holding only its name goes with it; one with a value shown keeps the value)
      const L = h.classList.contains("cap") ? null : h.closest(".lbl"), whole = L && !L.textContent.replace(h.textContent, "").trim();
      const target = whole ? L : h;
      (whole ? h : L || h).classList.remove("head-dup");
      target.classList.toggle("head-dup", dup);
      if (dup && shown.length === 1 && !headRepeats(text, nm)) tip = text;
    }
    if (boxName && (tip || boxName.dataset.tip)) { if (tip) { boxName.title = tip; boxName.dataset.tip = "1"; } else { boxName.removeAttribute("title"); delete boxName.dataset.tip; } }
  });
}
// (ticket 392) The two designers' tabs, 🎵 Music and 🗺️ World design, the same: a row's name that its one button says again
// ("⛶ full screen" over "⛶ the world designer, full screen"), and the name of a tab's one row that the tab says ("🗺️ world
// design" under 🗺️ World design), are hidden. Once, as the tab is built: these rows do not come and go.
function designerHeads(panel, tabName) {
  const rows = [...panel.querySelectorAll(":scope > .ctl")].filter(r => !r.hidden), named = [];
  for (const r of rows) {
    const n = r.querySelector(":scope > .lbl .name"), L = n && n.closest(".lbl"); if (!n) continue;
    const btns = r.querySelectorAll(":scope > button, :scope > .row > button");
    if (btns.length === 1 && headRepeats(n.textContent, btns[0].textContent)) { L.classList.add("head-dup"); continue; }
    named.push([r, n, L]);
  }
  const alone = rows.filter(r => !r.querySelector(":scope > .lbl.head-dup")).length === 1;
  if (alone && named.length === 1 && !named[0][0].querySelector(":scope > input[type=range]") && headRepeats(named[0][1].textContent, tabName)) {
    const [, n, L] = named[0]; (L.textContent.replace(n.textContent, "").trim() ? n : L).classList.add("head-dup"); }
}
// (ticket 375) The Background tab's rows in boxes, a box for each thing in the picture, the same in the side panel and the full screen
// (^du_report(5)'s step 2; till now its full screen made each row a box): 🗺️ the place, the place's own (named by the place, as it
// changes), 🧍 the friend here, 👁️ default background objects, 🪨 things to put here, 🎵 its music's motion, 🏰 the castles. By the row's key.
const BG_BOX = [["place", "🗺️ the place", k => k === "kind" || k === "instEdit", true],
                ["friend", "🧍 the friend here", k => /^(figSize|figPos|seat|north_|water(See|Wake|Tread)|blow|swim|tread)/.test(k)],
                ["stands", "👁️ default background objects", k => k === "objectsOff", true],   // (ticket 456, Matthew: "let's call this -- \"default background objects\" instead"; was 👁️ what stands here)
                ["things", "🪨 things to put here", k => k === "things", true],
                ["music", "🎵 the music moves it", k => /^lfo/.test(k)],
                ["castles", "🏰 the castles", (k, row) => /^castle/.test(k) || (row.classList.contains("ctlgroup") && /castle|🏰|🚪|room/i.test((row.querySelector(":scope > .cap") || {}).textContent || "")), true]];
function bgBoxes(rows) {
  const own = { rows: [] }, by = Object.fromEntries(BG_BOX.map(([id]) => [id, []]));
  for (const r of rows) { const k = r.dataset.key || "", hit = BG_BOX.find(([, , test]) => test(k, r)); (hit ? by[hit[0]] : own.rows).push(r); }
  // (the place's row named "background": 🗺️ the place names it)
  for (const r of by.place) if (r.dataset.key === "kind") { const L = r.querySelector(":scope > .lbl"); if (L && !L.querySelector(".val:not(:empty)")) L.hidden = true; }
  const box = (name, list, wide, dyn, id) => { if (!list.length) return null;
    const h = el("h3", { class: "box-name" }, [name]), b = el("section", { class: "box tabbox" + (wide ? " box-wide" : ""), ...(id ? { "data-box": id } : {}) }, [h, ...list]);
    syncs.push(() => { const hide = list.every(r => r.hidden); if (b.hidden !== hide) b.hidden = hide; if (dyn) { const t = dyn(); if (h.textContent !== t) h.textContent = t; } });
    boxHeads(b, () => h.textContent, list);
    return b; };
  const placeName = () => { const p = PLACES.find(q => q[0] === state.background.kind); return p ? p[1] : "🏞️ the place's own"; };
  return [box(BG_BOX[0][1], by.place, true), box(placeName(), own.rows, false, placeName),
          ...BG_BOX.slice(1).map(([id, name, , wide]) => box(name, by[id], wide, null, id))].filter(Boolean);
}
// (ticket 524, Matthew: "we need to make each room, and its params, available to world builder"; "world design panel I
// mean") The place's own settings as 🗺️ World design's ⚙️ box shows them: the 🏞️ Background tab's rows, built from the
// same TABS entries (so the same settings, each shown by its own SHOWN_WHEN only where it applies), less what is not the
// place's or what World design has already (the place chosen, the full screen and view, the things and the forest
// crossing's paths and distance (the square's card has them), BG_BOX's 🧍 the friend here), in bgBoxes' boxes. Built once
// as the tab is (control() adds a sync a row: never from a sync).
const BG_PLACE_SKIP = new Set(["kind", "big", "designerView", "things", "thingsSolid", "crossPaths", "crossBack"]);
function bgPlaceRows() {
  const friend = BG_BOX.find(([id]) => id === "friend")[2], ctls = (TABS.find(t => t[1] === "background") || [, , []])[2];
  const keyOf = c => c[0] === "of" ? c[2][1] : c[1];
  return bgBoxes(ctls.filter(c => !BG_PLACE_SKIP.has(keyOf(c)) && !friend(keyOf(c))).map(c => control("background", c)));
}
// (ticket 285) The Background tab's rows, once built, in the designer's two columns: its boxes (.bg-boxes), and beside them
// the game's card (.bg-viewcol); outside the full screen the two are display: contents, and the tab is as it always was
function bgLayout(panel) {
  const kids = [...panel.children], bar = kids.find(c => c.querySelector(":scope > button.designer-chip")), view = kids.find(c => c.querySelector(":scope > .bg-view"));
  const boxes = el("div", { class: "bg-boxes" }, bgBoxes(kids.filter(c => c !== bar && c !== view)));
  // (Matthew: "no drag between LHS and RHS -- that's a key feature of world designer": the same dragger, kept apart from the world's)
  const split = el("div", { class: "world-split bg-split", role: "separator", "aria-orientation": "vertical", title: "Drag: more room for the boxes, or for the game" });
  const layout = el("div", { class: "bg-layout" }, [boxes, split, el("div", { class: "bg-viewcol" }, view ? [view] : [])]);
  designerSplit(layout, split, "dress-up-bg-split");
  panel.replaceChildren(...(bar ? [bar] : []), layout);
  // (ticket 365, the report's "show the thing") each castle room's box, full screen, opens with its kind of room as the picture archive
  // has it (asked for only while the castle designer is full screen; a picture that cannot load hides)
  for (const g of panel.querySelectorAll('[data-group^="castleRoomBox"]')) {
    const [, n, id] = g.dataset.group.match(/^castleRoomBox(\d+)([A-Z])$/) || []; if (!n) continue;
    const img = el("img", { class: "room-pic", alt: "", loading: "lazy", onerror: e => { e.target.hidden = true; } }), cap = g.querySelector(":scope > .cap"), was = cap.textContent;
    cap.after(img);
    // (rule 1, named once: full screen the box is named by its number and its kind, "2 · 🌿 greenhouse", and its "room 2 is" row says
    // no more than its chips do)
    const kind = g.querySelector(`:scope > .ctl[data-key="castleRoom${n}${id}"]`); if (kind) kind.classList.add("room-kind");
    // (its kind read as castleOf reads it, not the whole castle built for it on every change: castlefs365)
    const kindOf = () => { const raw = state.background["castleRoom" + n + id] ?? CASTLE_ROOMS[id][+n - 1] ?? "king"; return CASTLE_ROOM_CHOICES.some(([v]) => v === raw) ? raw : "king"; };
    syncs.push(() => { const big = document.body.classList.contains("castle-big"), st = big ? kindOf() : null, t = st ? `${n} · ${roomLabel(st)}` : was;
      if (cap.textContent !== t) cap.textContent = t;
      if (!big) return; const src = st ? PICTURES_SITE + "castleroom--" + st + ".webp" : "";
      if (img.dataset.src !== src) { img.dataset.src = src; img.hidden = !src; if (src) img.src = src; } });
  }
}
// (ticket 375, Matthew: "redesign all panels in menu of dress up that are not following the guidance and insights outlined in the
// knowling report"; "does the non FS sub panel system look as well as the FS version? Are they each knowlled equally?") Every tab's
// rows, once built, in boxes, as ^du_report(5) has it: a box for each thing you can see (its name once, emoji first), the boxes in a
// grid (.boxes: one column in the side panel's width, more as it widens), a box hidden while every row in it is. TAB_BOXES: by tab
// name, its boxes, each [name, its rows' keys]; a row named in none goes in a last box named by the tab; a tab not listed is left
// as it is (its own layout: the designers, the controller, accessibility, advanced)
const TAB_BOXES = {
  "🧍 Body": [["🤸 the pose", ["pose", "lieHow", "lieHead", "hugLift"]], ["↔️ facing and moving", ["facing", "anim", "stairsWay"]],
              ["✋ the dominant hand", ["hand"]], ["🫴 holding", ["holdR", "holdL"]], ["📏 their size", ["size"]], ["🐴 the horse", ["horse", "horseLead", "horseArmour", "horseSize", "horseCoat", "horseMane", "horseManeStyle", "horseBows", "horseMark", "horseMarkColor", "horseTack", "horseSocks", "horseShoes", "horseSaddle", "horseBlanket"]], ["🐸 the jumps", ["jumps"]]],
  "😀 Face": [["🙂 the head", ["color", "size", "faceW", "faceH", "alien", "eyeGap", "sides", "rays"], ["face"]], ["🤨 the brows", ["brow", "unibrow"]],
              ["👄 the mouth", ["smile", "mouthSize", "open", "teethStyle", "teeth", "tongue", "tongueLen", "tongueW", "tongueColor"]],
              ["👃 the nose", ["noseView", "nose", "noseFront", "noseH", "noseW", "witchDir", "witchLen"]], ["👂 the ears", ["ears", "earSize"]],
              ["💥 faces it makes", ["burst", "candy", "sick"]]],
  "👁 Eyes": [["👁 the eyes", ["eyes", "oval", "tilt", "whites"], ["eye"]], ["⚫ pupil and iris", ["pupil", "iris", "irisColor"]],
             ["✨ the shine", ["light", "shine", "shineMix", "shineSize"]], ["⭐ funny eyes", ["stars", "hearts", "xeyes", "closed"]]],
  "💇 Hair": [["💇 the hair", ["style", "length", "bangs", "color"]], ["🧔 the beard", ["beard", "beardLength", "braidWidth", "beardShade", "moustache"]]],
  "🦒 Neck, arms & legs": [["🦒 the neck", ["neck", "neckW"]], ["💪 the arms", ["arms", "elbows", "elbowCurl", "elbowSwing", "armLen", "armW"]],
                          ["🦵 the legs", ["legLen", "legW", "knees", "kneeCurl", "kneeSwing", "feetTurn"]], ["🤸 posed", ["poseHow", "unpose"]]],
  "👕 Shirt": [["👕 the shirt", ["style", "collar", "color", "width", "length"]], ["🔷 its pattern", ["pattern", "patternSize", "patternColor"]]],
  "🖐 Hands": [["🖐 both hands", ["different", "style", "color", "size"]], ["🖐🖐 each hand", ["rightHand"]],
              ["🔢 how many", ["fingers", "alienFingers", "starPoints", "clawCount", "clawLen"], ["number of"]],
              ["🔄 the angle", ["starAngle", "mittenAngle", "clawAngle", "humanAngle", "alienAngle"]]],
  "👖 Pants": [["👖 the pants", ["style", "skirtLen", "color"]], ["🔷 its pattern", ["pattern", "patternSize", "patternColor"]]],
  "🩱 Swim & undies": [["🩱 the swimsuit", ["on", "style", "top", "color"]], ["🔷 its pattern", ["pattern", "patternSize", "patternColor"]]],
  "👟 Feet": [["👟 both feet", ["different", "style", "color", "size"]], ["👟👟 each foot", ["rightShoe"]],
             ["🔢 how many", ["toes", "alienToes", "talonCount"], ["number of"]]],
  "👻 Visibility": [["🧍 the whole friend", ["all"]], ["👻 each part", ["head", "eyes", "hair", "shirt", "pants", "feet", "hands"]]],
  // (ticket 488, ^du_report(5)'s rules: Matthew, "how can we best 'Knowl' the panels") the controller, the bubbles, moving
  "🎮 Controls": [["🎮 the game controller", ["map"]], ["🫧 bubbles", ["blowing", "blowThemes"]], ["🔥 fire", ["firing"]], ["🏃 moving", ["moving", "clickPlan"]]],   // (ticket 499: breathing fire)
  "🐯 Costume": [["🐯 the costume", ["kind"]], ["🐾 the animal", ["fur", "furPattern", "marks", "spot", "belly", "eyes", "accent"]],
                ["🤿 the scuba suit", ["scubaColor", "scubaAccent"]], ["⚓ the old diving suit", ["diverMetal", "diverCloth", "diverHose", "hoseColor"]],
                ["🥽 diving gear", ["snorkel", "snorkelColor", "mask", "maskColor", "tank", "tankColor", "leaks"]],
                ["👻 the ghost", ["ghostRagged", "ghostEyes", "ghostHideEyes", "ghostColor", "visible"]],
                ["🧙‍♂️ the wizard", ["wizardRobe", "wizardStars"]], ["🧘 the monk or dryad", ["monkWho", "monkCloak", "hoodUp", "hoodFace", "hoodDim", "hoodEyes", "bareFeet"]],
                ["🪄 the staff", ["staff", "staffOrb"]], ["🏹 Robin Hood", ["robinTunic", "robinHat", "robinFeather", "bow", "quiver"]],
                ["👑 the queen or king", ["queenRobe", "kingRobe", "crownJewel", "mantle", "scepter"]],
                ["🐉 the dragon", ["dragonScales", "dragonBelly", "dragonWings", "dragonHorns", "dragonWingsHow", "dragonWingLfo", "fireStyle"]],   // (ticket 523: its fire, the friend's)
                ["🦋 the moth", ["mothStyle", ...Object.keys(MOTH_STYLES).map(id => "mothColor_" + id), "mothSpots"]],   // (ticket 536)
                ["🤖 the robot", ["robotStyle", ...Object.keys(ROBOT_STYLES).map(id => "robotColor_" + id), "robotLights"]],   // (ticket 515)
                ["🥷 the ninja", ["ninjaBoots", "ninjaBetween", "ninjaBand", "ninjaGem", "ninjaGemShape", "ninjaGlimmer", "ninjaGemStars", "ninjaGemShine", "ninjaEyes"]],   // (ticket 557; 561: what shows between the bands)
                ["🦶 the Sasquatch", ["sasquatchStyle", ...Object.keys(SASQUATCH_STYLES).map(id => "sasquatchColor_" + id), "sasquatchPrints", "sasquatchGrin"]],   // (ticket 530)
                ["🦒 the giraffe", ["giraffeStyle", ...Object.keys(GIRAFFE_STYLES).map(id => "giraffeColor_" + id), "giraffeNeck"]],   // (ticket 544)
                ["🤠 the outfit", ["cowgirlStyle", ...Object.keys(COWGIRL_STYLES).map(id => "cowgirlColor_" + id), "cowboyStyle", ...Object.keys(COWBOY_STYLES).map(id => "cowboyColor_" + id)]],   // (ticket 522)
                ["🎩 the hat", ["cowHat", ...Object.keys(COW_HATS).map(id => "cowHatColor_" + id)]], ["➿ the lasso", ["lasso"]],
                ["🛡️ the knight", ["knightA", "knightB", "tabardLayout", "emblem", "plume", "plumeColor", "helmOpen", "sword", "shield"]],
                ["🍄 the gnome", ["gnomeWho", "gnomeHat", "gnomeTunic", "gnomeTrousers", "gnomeBodice", "gnomeSkirt", "gnomeBoots"]],
                ["🧙 the witch", ["witchColor", "webColor", "webStyle", "trimColor", "bootColor"]],
                ["🐈‍⬛ her cat", ["witchCat", "catFur", "catWhite", "catEyes", "catNose", "catSize"]], ["🧹 her broom", ["broom", "broomSize"]],
                ["🧚 the fairy", ["wingStyle", "wingColor_butterfly", "wingColor_dragonfly", "wingColor_leaf", "wingColor_petal", "wingColor_sparkle",
                                  "wingFlutter", "wingLfo", "fairyOutfit", "tunicStyle", "tunicColor_petal", "tunicColor_leaf", "tunicColor_bluebell",
                                  "tunicColor_sparkle", "tunicColor_toadstool", "fairyShoes", "fairyShoeColor"]],
                ["🌸 the wreath", ["wreath", "wreathFlowers"]]],
};
// (ticket 377, the report's rule 7, mirror the picture) the parts of the friend each tab's boxes are about (the drawing's veil layers,
// marked data-part): pointed at, a box lights them in the game (body[data-lit], style.css), however often the friend is drawn again
const ALL_PARTS = "head eyes hair shirt pants feet hands";
const BOX_PARTS = { "🧍 Body": ALL_PARTS, "😀 Face": "head", "👁 Eyes": "eyes", "💇 Hair": "hair", "👕 Shirt": "shirt", "🖐 Hands": "hands", "👖 Pants": "pants",
  "🩱 Swim & undies": "shirt pants", "👟 Feet": "feet", "👻 Visibility": ALL_PARTS, "🐯 Costume": ALL_PARTS,
  "🦒 Neck, arms & legs": { "🦒 the neck": "shirt", "💪 the arms": "shirt hands", "🦵 the legs": "pants feet", "🤸 posed": ALL_PARTS } };
const boxParts = (tabName, boxName) => { const m = BOX_PARTS[tabName]; return !m ? "" : typeof m === "string" ? m : m[boxName] || ""; };
function boxLayout(panel, tabName, ctls) {
  const plan = TAB_BOXES[tabName];
  if (!plan) return;
  const rows = [...panel.children], boxes = plan.map(([name]) => ({ name, rows: [] })), rest = { name: tabName, rows: [] };
  // (ticket 488) a row of another part ("of": 🎮 Controls' bubbles, from 🧍 Body and 🏞️ Background) by its own key, unless the
  // plan names its part (🐯 Costume's 👻 visibility rows)
  const keyOf = c => { const k = c && typeof c[1] === "string" ? c[1] : null;
    return c && c[0] === "of" && c[2] && typeof c[2][1] === "string" && !plan.some(([, keys]) => keys.includes(k)) ? c[2][1] : k; };
  rows.forEach((r, i) => { const key = keyOf(ctls[i]), j = key ? plan.findIndex(([, keys]) => keys.includes(key)) : -1;
    (j >= 0 ? boxes[j] : rest).rows.push(r); });
  const made = [...boxes, rest].filter(b => b.rows.length).map((b, j) => {
    // (the report's rule 1: nothing inside repeats the box's name: "mouth size" in 👄 the mouth is "size"; the words dropped are
    // the name's last and any the plan adds, at a label's start, after its emoji)
    const words = j < plan.length ? [b.name.split(" ").pop(), ...(plan[j][2] || [])] : [];
    for (const r of b.rows) for (const n of r.querySelectorAll(":scope > .lbl .name")) { const t = n.textContent;
      for (const w of words) { const m = t.match(new RegExp("^((?:[\\p{Extended_Pictographic}\\u200D\\uFE0F]\\s*)*)(?:the\\s+|its\\s+)?" + w.replace(/s$/, "") + "s?(?:'s)?\\s+(\\S.*)$", "u"));
        if (m) { n.textContent = m[1] + m[2]; break; } }
      // (and a row whose name is only the box's: "costume" in 🐯 the costume, "pattern" in 🔷 its pattern: the box names it)
      const bare = n.textContent.replace(/^[\p{Extended_Pictographic}\u200D\uFE0F\s]+/u, "").trim();
      if (words.length && bare && words.some(w => bare === w || bare === w.replace(/s$/, ""))) {
        // (a slider keeps a name: its hint, "dark ↔ bright" for ✨ the shine's shine, where it has one)
        if (r.querySelector(":scope > input[type=range]")) { if (n.title) { n.textContent = n.title; n.removeAttribute("title"); } continue; }
        n.hidden = true; const L = n.closest(".lbl"); if (L && !L.textContent.replace(n.textContent, "").trim()) L.hidden = true; } }
    const box = el("section", { class: "box tabbox" }, [el("h3", { class: "box-name" }, [b.name]), ...b.rows]);
    syncs.push(() => { const hide = b.rows.every(r => r.hidden); if (box.hidden !== hide) box.hidden = hide; });
    boxHeads(box, () => b.name, b.rows);
    const parts = boxParts(tabName, b.name);
    if (parts) { box.addEventListener("pointerenter", () => { document.body.dataset.lit = parts; }); box.addEventListener("pointerleave", () => { if (document.body.dataset.lit === parts) delete document.body.dataset.lit; }); }
    return box; });
  panel.replaceChildren(el("div", { class: "boxes tabboxes" }, made));
}
// (a place's picture, the picture archive's thumbnail of it with the friend; the castle's rooms by the room shown now, or the king's)
const PICTURES_SITE = "https://matthewhammer.org/dress-up-pictures/pictures/thumbs/backgrounds-with-friend/";
const placeThumb = id => PICTURES_SITE + (id === "castleroom" ? "castleroom--" + (state.background.kind === "castleroom" ? objPlace(state.background).split(":")[1] : "king") : id) + ".webp";
// (an object's picture, OBJECTS.md's, clipped from its place: dress-up/objects/<place>/<name>.webp)
const objPicture = (at, name) => `objects/${at.replace(":", "-")}/${name.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "")}.webp`;
// (ticket 231, Matthew: "show the in-game view as the first sub-panel, the full width of the RHS of the full screen
// view. Then a way to scroll the panels below and keep that in-game view fixed. An eye-ball to hide it ... off by
// default, so we are showing it by default") The world designer full screen, on a wide window: the picture itself
// (.pic-box: #pic with its arrows and buttons, live, to walk in and to drag things into) moves into the right side's
// first card (.world-view-pic), held under the bar as the cards below it scroll; ticket 224's dragger sizes it. The
// card's 👁️ folds it away to its name and back, kept in this browser. Leaving full screen, a narrow window, or the
// tab built again (showTab) puts the picture where it belongs again.
const WORLD_VIEW_KEY = "dress-up-world-view", worldWide = matchMedia("(min-width: 900px)");
// (held, not looked up: the tab built again takes the card, and the picture in it, out of the page until placed)
const picBoxEl = document.querySelector(".pic-box"), picHome = document.querySelector(".pic-area");
let worldViewHidden = false, picSlot = null;
try { worldViewHidden = localStorage.getItem(WORLD_VIEW_KEY) === "hidden"; } catch (e) { /* (shown) */ }
function placePic() {
  // (ticket 285: the world designer's card, or the background designer's)
  const bg = document.body.classList.contains("bg-big"), tb = document.body.classList.contains("tab-big"), box = picBoxEl, home = picHome, slot = document.querySelector(bg ? ".bg-view-pic" : tb ? ".tab-view-pic" : ".world-view-pic");
  if (!box || !home) return;
  const into = (bg || tb || document.body.classList.contains("world-big")) && worldWide.matches && !worldViewHidden && slot ? slot : null;
  if (into !== picSlot && picAreaWatch) { if (picSlot) picAreaWatch.unobserve(picSlot); if (into) picAreaWatch.observe(into); }
  picSlot = into;
  if (into && box.parentNode !== into) into.append(box);
  if (!into && box.parentNode !== home) home.insertBefore(box, home.querySelector(".bar"));
  document.body.classList.toggle("pic-in-world", !!into);
  picAreaSize = null; fitPic();
}
// the card's 👁️, lit while the view shows (as the HUD's eyes)
function worldViewMark(card) {
  card.classList.toggle("folded", worldViewHidden);
  const eye = card.querySelector(".eye");
  eye.setAttribute("aria-pressed", String(!worldViewHidden));
  eye.title = worldViewHidden ? "Show the in-game view here" : "Hide the in-game view (more room for the controls)";
  eye.setAttribute("aria-label", eye.title);
}
function worldViewShow(on) {
  worldViewHidden = !on;
  try { if (on) localStorage.removeItem(WORLD_VIEW_KEY); else localStorage.setItem(WORLD_VIEW_KEY, "hidden"); } catch (e) { /* (for this visit) */ }
  for (const card of document.querySelectorAll(".world-view")) worldViewMark(card);
  placePic();
}
worldWide.addEventListener("change", placePic);
document.addEventListener("keydown", e => {
  if (e.key === "Escape" && document.body.classList.contains("castle-big")) { setCastleBig(false); return; }   // (ticket 365: back where it came from, one step)
  if (e.key === "Escape" && document.body.classList.contains("world-big")) setWorldBig(false);
  if (e.key === "Escape" && document.body.classList.contains("bg-big")) setBgBig(false);
  if (e.key === "Escape" && document.body.classList.contains("tab-big")) setTabBig(false);
  if (e.key === "Escape" && document.body.classList.contains("big")) setBig(false);
  if (e.key === "Escape" && document.body.classList.contains("music-big")) setMusicBig(false);
});
document.addEventListener("fullscreenchange", () => {
  if (!document.fullscreenElement && document.body.classList.contains("big")) setBig(false);
  if (!document.fullscreenElement && document.body.classList.contains("music-big")) setMusicBig(false, true);
  if (!document.fullscreenElement && document.body.classList.contains("world-big")) setWorldBig(false);
  if (!document.fullscreenElement && document.body.classList.contains("castle-big")) setCastleBig(false, true);
  if (!document.fullscreenElement && document.body.classList.contains("bg-big")) setBgBig(false);
});
