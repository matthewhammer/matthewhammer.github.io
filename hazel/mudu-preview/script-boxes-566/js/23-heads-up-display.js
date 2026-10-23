"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 23 of 26 of the game's script, run in order by index.html.

// ===================== THE HEADS-UP DISPLAY =====================
// In full screen, a strip of controls along the bottom, hidden until its
// button is pressed: a scene's own controls (the figure's size; drag the
// figure to move it), then every pinned slider, made by control() as in
// the panel, with their own refreshes.
const hud = document.getElementById("hud"), hudBtn = document.getElementById("hud-btn");
const hudKey = () => JSON.stringify(state.hud.pins || []) + (state.background.kind !== "none") + !!state.hands.different + !!state.shoes.different;
let hudShown = "";
function renderHud() {
  if (hud.hidden) return;
  hudShown = hudKey();
  const saved = syncs, savedMade = madeSyncs;
  syncs = []; madeSyncs = [];
  buildingHud = true;
  const boxes = [];
  if (state.background.kind !== "none")
    for (const spec of TABS.find(t => t[1] === "background")[2].filter(c => c[1] === "figSize" || c[1] === "figPos")) boxes.push(control("background", spec));
  for (const id of state.hud.pins || []) {
    const [part, key] = id.split("."), tabEntry = TABS.find(t => t[1] === part);
    // (the every-part chips share the full-body slider's key; it is the slider that is pinned)
    const spec = (part === "rightHand" ? HAND_CTLS : part === "rightShoe" ? SHOE_CTLS : tabEntry ? tabEntry[2] : []).find(c => c[1] === key && c[0] !== "every");
    if (!spec) continue;
    // one hand's own slider says whose it is
    const whose = part === "rightHand" ? "⬅️ right hand: " : part === "hands" && SLIDER_STYLE[key] && state.hands.different ? "left hand ➡️: " : "";
    const foot = part === "rightShoe" ? "⬅️ right foot: " : part === "shoes" && FOOT_SLIDER_STYLE[key] && state.shoes.different ? "left foot ➡️: " : "";
    if (foot) {
      const [kind, , label, ...rest] = spec;
      boxes.push(control(part, [kind, key, foot + label, ...rest],
                         { when: sh => (part === "shoes" || state.shoes.different) && sh.style === FOOT_SLIDER_STYLE[key] }));
      continue;
    }
    if (!whose) { boxes.push(control(part, spec)); continue; }
    const [kind, , label, ...rest] = spec;
    boxes.push(control(part, [kind, key, whose + label, ...rest],
                       { when: h => (part === "hands" || state.hands.different) && handSliderShown(key, h) }));
  }
  if (!boxes.length)
    boxes.push(el("div", { class: "empty" }, "Pin sliders with 📌 in the controls, and they show up here."));
  // (only when there is nothing else: a scene always has its size and place)
  buildingHud = false;
  hud.replaceChildren(...boxes);
  hudSyncs = syncs; hudMadeSyncs = madeSyncs;
  syncs = saved; madeSyncs = savedMade;
  for (const f of hudSyncs) f();
}
function setHud(on) {
  hud.hidden = !on;
  hudBtn.setAttribute("aria-pressed", on);
  if (on) renderHud(); else { hud.replaceChildren(); hudSyncs = []; hudMadeSyncs = []; }
}
hudBtn.addEventListener("click", () => setHud(hud.hidden));
document.getElementById("music-fs").addEventListener("click", () => setMusicBig(true));
