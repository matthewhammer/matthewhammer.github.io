"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 21 of 26 of the game's script, run in order by index.html.

// ===================== THE PICTURE'S SIZE, AND THE DIVIDER =====================
// (ticket 116) The picture is as large as its place allows, whatever its
// shape: as wide as its column, unless that would make it taller than the
// room under it leaves; then as tall as that (on a phone, 40% of the
// screen's height; in full screen, the whole window, less the HUD). Called
// after every draw (a picture without a scene changes shape with the
// friend) and whenever its place changes size.
let picFit = "";
// (ticket 133) The picture's area is measured only when it may have changed
// size (a ResizeObserver, the window, the big-picture switch): reading it
// right after a drawing changed the picture made the browser lay the whole
// page out again, on every drawing, every move of a drag.
const narrowMedia = matchMedia("(max-width: 760px)");
let picAreaSize = null, picAreaWatch = null;
function fitPic() {
  const pic = document.getElementById("pic"), area = document.querySelector(".pic-area");
  if (!pic || !area) return;
  if (!picAreaWatch && typeof ResizeObserver === "function") {
    picAreaWatch = new ResizeObserver(() => { picAreaSize = null; fitPic(); });
    picAreaWatch.observe(area);
    window.addEventListener("resize", () => { picAreaSize = null; });
  }
  const vb = pic.viewBox.baseVal, ar = vb && vb.width > 0 && vb.height > 0 ? vb.width / vb.height : 4 / 3;
  const big = document.body.classList.contains("big"), view = document.body.classList.contains("pic-in-world"), narrow = narrowMedia.matches && !big && !view;
  if (!picAreaSize || picAreaSize.big !== big || picAreaSize.narrow !== narrow || picAreaSize.view !== view || !picAreaWatch) {
    // (the caption and buttons under it, in the same area, take their share of its height)
    const bar = big ? null : area.querySelector(".bar"), slot = view ? document.querySelector(document.body.classList.contains("bg-big") ? ".bg-view-pic" : ".world-view-pic") : null;   // (ticket 285: whichever designer's card holds it)
    // (ticket 231) in the world designer's right side (placePic): its card's width, and at most 60% of the window
    // under the bar (56px, as the stylesheet's), so the cards below it keep room to scroll
    picAreaSize = slot ? { big, narrow, view, W: slot.clientWidth, H: (window.innerHeight - 56) * 0.6 }
      : { big, narrow, view, W: area.clientWidth, H: narrow ? window.innerHeight * 0.4 : area.clientHeight - (bar ? bar.offsetHeight : 0) };
  }
  const { W, H } = picAreaSize;
  if (!(W > 0) || !(H > 0)) { picAreaSize = null; return; }
  let w = W, h = W / ar;
  if (h > H) { h = H; w = H * ar; }
  const key = Math.floor(w) + "x" + Math.floor(h);
  if (key === picFit) return;
  picFit = key; pic.style.width = Math.floor(w) + "px"; pic.style.height = Math.floor(h) + "px";
  if (typeof foamLayersPlace === "function") foamLayersPlace();   // (ticket 383: the layered foam where the picture is drawn now)
}
// The divider between the picture's column and the controls: dragged, it
// moves (and the place it was left, as a share of the window, is
// remembered in this browser); double-clicked, or never dragged, the
// picture's column is as wide as the picture can be at the window's
// height (between 40% and 70% of the window, and leaving the controls
// 320 pixels); the arrow keys move it too.
const appEl = document.querySelector(".app"), divider = document.getElementById("divider");
// (ticket 491, Matthew: "fix for breadcrumbs in single column mode (on my desktop at half screen width)") In one column
// (760 px and less) the picture's stage sticks at the top of the window and the panel scrolls under it, so the band of
// breadcrumbs (#crumbs), sticky too, sticks just under the stage instead of across the picture: --stage-h, the stage's
// height, kept by a ResizeObserver (read only when the stage changes size, as ticket 133 asks).
const stageEl = document.querySelector(".stage");
if (stageEl && typeof ResizeObserver === "function") {
  let stageH = -1;
  new ResizeObserver(() => {
    const h = Math.ceil(stageEl.offsetHeight);
    if (h !== stageH) { stageH = h; appEl.style.setProperty("--stage-h", h + "px"); }
  }).observe(stageEl);
}
const SPLIT_KEY = "dress-up-split";
const splitBounds = () => [260, Math.max(260, appEl.clientWidth - 18 - 320)];
function setSplit(px, keep = false) {
  if (matchMedia("(max-width: 760px)").matches) { appEl.style.removeProperty("--split"); fitPic(); return; }
  const total = appEl.clientWidth, [lo, hi] = splitBounds();
  if (px == null) {
    let f = NaN; try { f = parseFloat(localStorage.getItem(SPLIT_KEY)); } catch (_) {}
    if (f > 0 && f < 1) px = f * total;
    else { const vb = document.getElementById("pic").viewBox.baseVal, ar = vb && vb.height > 0 ? vb.width / vb.height : 4 / 3;
      const area = document.querySelector(".pic-area");
      px = Math.max(total * 0.4, Math.min(total * 0.7, ((area.clientHeight || window.innerHeight * 0.8) - (area.querySelector(".bar")?.offsetHeight ?? 0)) * ar)); }
  }
  px = Math.max(lo, Math.min(hi, px));
  appEl.style.setProperty("--split", Math.round(px) + "px");
  divider.setAttribute("aria-valuenow", String(Math.round(100 * px / total)));
  if (keep) try { localStorage.setItem(SPLIT_KEY, String(px / total)); } catch (_) {}
  fitPic();
}
let splitDrag = null;
divider.addEventListener("pointerdown", e => {
  if (e.button !== 0) return;
  splitDrag = { id: e.pointerId }; divider.setPointerCapture(e.pointerId);
  divider.classList.add("drag"); document.body.classList.add("dragging-split"); e.preventDefault();
});
divider.addEventListener("pointermove", e => {
  if (!splitDrag || e.pointerId !== splitDrag.id) return;
  splitDrag.px = e.clientX - appEl.getBoundingClientRect().left - 7;
  setSplit(splitDrag.px);
});
const endSplitDrag = e => {
  if (!splitDrag || e.pointerId !== splitDrag.id) return;
  if (splitDrag.px != null) setSplit(splitDrag.px, true);
  splitDrag = null; divider.classList.remove("drag"); document.body.classList.remove("dragging-split");
};
divider.addEventListener("pointerup", endSplitDrag);
divider.addEventListener("pointercancel", endSplitDrag);
divider.addEventListener("dblclick", () => { try { localStorage.removeItem(SPLIT_KEY); } catch (_) {} setSplit(); });
divider.addEventListener("keydown", e => {
  const d = { ArrowLeft: -40, ArrowRight: 40 }[e.key];
  if (!d) return;
  e.preventDefault();
  setSplit(document.querySelector(".stage").getBoundingClientRect().width + d, true);
});
window.addEventListener("resize", () => setSplit());
if (window.ResizeObserver) new ResizeObserver(fitPic).observe(document.querySelector(".pic-area"));
setSplit();

// The save menu: its button opens and closes it; a click anywhere else, or
// Escape, closes it. It stays open after a save, for the links to the files.
const saveOpen = document.getElementById("save-open"), savePop = document.getElementById("save-pop");
function setSavePop(on) {
  savePop.hidden = !on; saveOpen.setAttribute("aria-expanded", String(on));
  // (ticket 43) the postcard's words start as the picture's own line, until changed
  const t = document.getElementById("postcard-text"), mood = document.getElementById("mood").textContent;
  if (on && (!t.value || t.value === t.dataset.auto)) { t.value = mood; t.dataset.auto = mood; }
}
saveOpen.addEventListener("click", () => setSavePop(savePop.hidden));
document.addEventListener("pointerdown", e => { if (!savePop.hidden && !e.target.closest(".save-menu")) setSavePop(false); });
document.addEventListener("keydown", e => { if (e.key === "Escape" && !savePop.hidden) setSavePop(false); });
