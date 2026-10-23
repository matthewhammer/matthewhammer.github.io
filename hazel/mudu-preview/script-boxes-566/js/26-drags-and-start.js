"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 26 of 26 of the game's script, run in order by index.html.

// ===================== DRAG THE MOUTH AND BROW =====================
// As in Kids' Choice: a press above the eyes grabs the brow, below them
// the mouth, and dragging down raises the value: a smile, or a glower.

let drag = null;
const pic = document.getElementById("pic");
// the pointer in the picture's units, then in the figure's (undoing its
// size and place in a scene), then in the face's
const picUnits = e => new DOMPoint(e.clientX, e.clientY).matrixTransform(pic.getScreenCTM().inverse());
function faceUnits(e) {
  const p = picUnits(e), T = figureT;
  const x = (p.x - T.dx) / T.k, y = (p.y - T.dy - T.sg) / T.k + T.g;
  return [(x - faceFrame.tx) / faceFrame.s, (y - faceFrame.ty) / faceFrame.s];
}
// in a scene, a press on the figure (not its face) picks it up
const onFigure = e => {
  if (state.background.kind === "none") return false;
  const t = document.elementFromPoint(e.clientX, e.clientY);
  return !!(t && t.closest && t.closest("#figure"));
};
const onFace = e => {
  const [x, y] = faceUnits(e);
  return (x - 100) ** 2 + (y - 110) ** 2 <= 90 ** 2;
};
// On a touch screen the picture pinches and pans like the rest of the
// page; only a one-finger touch that starts on the face is kept for the
// drag (the page used to opt the whole picture out of zooming). A second
// finger lets go of the face, so a pinch goes to the browser.
// in a scene, a press on a thing that moves (data-drag: the witch) picks it up
const onThing = e => {
  const t = document.elementFromPoint(e.clientX, e.clientY);
  return t && t.closest ? t.closest("#bg [data-drag]") : null;
};
// POSING (ticket 121), as in M's games on the iPad: a press on a hand, a
// foot, an elbow or a knee (the nearest within a fingertip, or within a
// big hand's or shoe's size) takes hold of it, and dragging poses that limb
// alone: a hand or a foot reaches for the finger, the elbow or knee bending
// the way it already bent; an elbow or a knee swings the whole arm or leg
// round its shoulder or hip. Each limb's turn and bend is kept on top of
// the sliders' (poseOf), so the sliders still move both alike.
const figUnits = (e, T = figureT) => { const p = picUnits(e); return [(p.x - T.dx) / T.k, (p.y - T.dy - T.sg) / T.k + T.g]; };
// (ticket 135) A press is on a joint only when it is really on it: a hand or
// a foot within its own size and a little more (8 screen pixels for a mouse
// or a pen, 18 for a finger), an elbow or a knee within its limb's width.
// Anywhere else on the friend the press moves them. (It was 30 pixels round
// every joint, and in a place, where the friend is small, the elbows' and
// hands' circles covered most of the torso: dragging the friend took an arm.)
function onJoint(e) {
  if (!poseJoints || entering) return null;
  const [x, y] = figUnits(e), px = 1 / (pic.getScreenCTM().a * figureT.k), slop = (e.pointerType === "touch" ? 18 : 8) * px;
  let best = null;
  const consider = (what, side, P, r) => { const d = Math.hypot(x - P[0], y - P[1]); if (d <= r && (!best || d < best.d)) best = { what, side, d, P, r }; };
  // (ticket 369, bridge245: a friend small on a rope bridge, 54 pixels tall in the app's pane and less on GitHub's, was all hands and
  // feet: their 12-pixel least size and the slop covered every point not on the face, so a press could only pose a limb, never take
  // the friend off the bridge) a hand's or a foot's reach at most an eighth of the friend's height on the screen, never less than the
  // hand or the foot itself; a friend of ordinary size (160 pixels tall and more under a mouse, 240 under a finger) is as before
  const tall = ((document.getElementById("figure") || { getBoundingClientRect: () => ({ height: 1e9 }) }).getBoundingClientRect().height || 1e9) * px;
  const reach = r => Math.max(r, Math.min(Math.max(r, 12 * px) + slop, tall / 8));
  for (const a of poseJoints.arms) { consider("hand", a.side, a.H, reach(poseJoints.handR)); consider("elbow", a.side, a.E, poseJoints.armW * 0.6 + 2 * px); }
  for (const l of poseJoints.legs) { consider("foot", l.side, l.F, reach(poseJoints.footR)); consider("knee", l.side, l.K, poseJoints.legW * 0.6 + 2 * px); }
  return best;
}
// (ticket 135) Under a mouse or a pen, what a press would take glows: the
// orb of a hand, a foot, an elbow or a knee it would pose, or the friend's
// body when it would move them. Drawn over the picture, kept across
// drawings, changed only when what is under the pointer changes.
let hoverOn = null;
function hoverTarget(e) {
  if (drag || e.pointerType === "touch") return null;
  const joint = onJoint(e);
  if (joint) return { kind: "joint", what: joint.what, side: joint.side, key: "j" + joint.what + joint.side };
  if (onThing(e) && !onFigure(e)) return null;
  if (onFace(e)) return null;   // (a press on the face moves the brows or the smile)
  return onFigure(e) ? { kind: "figure", key: "f" } : null;
}
function hoverGlowSvg() {
  if (!hoverOn || !poseJoints) return "";
  const T = figureT, at = (x, y) => [T.dx + T.k * x, T.dy + T.sg + T.k * (y - T.g)], J = poseJoints;
  const ring = (cx, cy, r) => `<circle cx="${f1(cx)}" cy="${f1(cy)}" r="${f1(r + 7)}" fill="none" stroke="#fff6a0" stroke-opacity="0.45" stroke-width="7"/>` +
                              `<circle cx="${f1(cx)}" cy="${f1(cy)}" r="${f1(r + 3)}" fill="#fff6a0" fill-opacity="0.18" stroke="#fffbe0" stroke-width="2.5"/>`;
  if (hoverOn.kind === "joint") {
    const limb = hoverOn.what === "hand" || hoverOn.what === "elbow" ? J.arms.find(a => a.side === hoverOn.side) : J.legs.find(l => l.side === hoverOn.side);
    if (!limb) return "";
    const P = { hand: limb.H, elbow: limb.E, foot: limb.F, knee: limb.K }[hoverOn.what], r = { hand: J.handR, foot: J.footR, elbow: J.armW / 2, knee: J.legW / 2 }[hoverOn.what];
    const [cx, cy] = at(P[0], P[1]);
    return ring(cx, cy, Math.max(6, T.k * Math.min(r, 40)));
  }
  // the body: shoulders to hips, as wide as the wider of the two, rounded
  const xs = [...J.arms.map(a => a.A[0]), ...J.legs.map(l => l.H[0])], ys = [...J.arms.map(a => a.A[1]), ...J.legs.map(l => l.H[1])];
  const pad = Math.max(J.armW, 10), [x0, y0] = at(Math.min(...xs) - pad, Math.min(...ys) - pad), [x1, y1] = at(Math.max(...xs) + pad, Math.max(...ys) + pad);
  const rx = Math.min(x1 - x0, y1 - y0) * 0.3;
  return `<rect x="${f1(x0)}" y="${f1(y0)}" width="${f1(x1 - x0)}" height="${f1(y1 - y0)}" rx="${f1(rx)}" fill="#fff6a0" fill-opacity="0.12" stroke="#fff6a0" stroke-opacity="0.5" stroke-width="7"/>` +
         `<rect x="${f1(x0)}" y="${f1(y0)}" width="${f1(x1 - x0)}" height="${f1(y1 - y0)}" rx="${f1(rx)}" fill="none" stroke="#fffbe0" stroke-width="2.5"/>`;
}
function showHover() {
  const old = document.getElementById("hover-glow");
  if (old) old.remove();
  const svg = hoverGlowSvg();
  if (!svg) return;
  const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
  g.id = "hover-glow"; g.setAttribute("pointer-events", "none"); g.innerHTML = svg;
  pic.append(g);
}
function hoverAt(e) {
  const t = hoverTarget(e), key = t ? t.key : null;
  if (key === (hoverOn ? hoverOn.key : null)) return;
  hoverOn = t; showHover();
}
// a limb's new turn (and, for a hand or foot, bend) for its joint at
// (x, y), in the figure's units
function poseTo({ what, side }, [x, y]) {
  const B = state.body, sk = side > 0 ? "L" : "R", J = poseJoints, deg = r => r * 180 / Math.PI, rad = d => d * Math.PI / 180;
  const norm = d => ((d % 360) + 540) % 360 - 180, r1 = v => Math.round(v * 10) / 10;
  // (two bones from the root R, the first `a` long and the second `b`: the
  // first's angle so the second's end is at the finger; of the two ways the
  // middle joint can go, the one nearer where it is now; then the second's)
  const reach = (R, M0, a, b, own, at) => {
    const dx = x - R[0], dy = y - R[1], d = Math.max(Math.abs(a - b) + 0.01, Math.min(Math.hypot(dx, dy), a + b - 0.01)), phi = own(dx, dy);
    const g = deg(Math.acos(Math.max(-1, Math.min(1, (a * a + d * d - b * b) / (2 * a * d)))));
    const off = t => Math.hypot(at(t)[0] - M0[0], at(t)[1] - M0[1]), first = off(phi + g) <= off(phi - g) ? phi + g : phi - g, M = at(first);
    return [first, own(x - M[0], y - M[1])];
  };
  // (ticket 124: in profile the arms and legs point the way the friend
  // faces, `out`, and the far arm is held a little higher; sitting, the
  // legs start from the sitting angles, and facing us the thighs are short)
  const dir = facingOf(B), lean = side === dir ? -14 : dir ? 4 : 0;
  if (what === "hand" || what === "elbow") {
    // (angles as the arms slider measures them: from level, + down, mirrored for the side)
    const arm = J.arms.find(a => a.side === side), A = arm.A, half = J.armLen / 2, o = arm.out ?? side, own = (dx, dy) => deg(Math.atan2(dy, o * dx));
    const at = t => [A[0] + o * half * Math.cos(rad(t)), A[1] + half * Math.sin(rad(t))];
    if (what === "elbow") { B["poseArm" + sk] = r1(norm(own(x - A[0], y - A[1]) - B.arms - lean)); return; }
    const [up, fore] = reach(A, arm.E, half, half, own, at);
    B["poseArm" + sk] = r1(norm(up - B.arms - lean));
    B["poseElbow" + sk] = r1(norm(up - fore - (B.elbows ?? 0)));
  } else {
    // (angles from straight down, + out, mirrored for the side)
    const leg = J.legs.find(l => l.side === side), Hp = leg.H, half = J.LL / 2, o = leg.out ?? side, own = (dx, dy) => deg(Math.atan2(o * dx, dy));
    const base = J.legBase || { t0: (B.knees ?? 0) / 2, b0: B.knees ?? 0, thigh: 1 }, a = half * base.thigh;
    const at = t => [Hp[0] + o * a * Math.sin(rad(t)), Hp[1] + a * Math.cos(rad(t))];
    if (what === "knee") { B["poseLeg" + sk] = r1(norm(own(x - Hp[0], y - Hp[1]) - base.t0)); return; }
    const [thigh, shin] = reach(Hp, leg.K, a, half, own, at);
    B["poseLeg" + sk] = r1(norm(thigh - base.t0));
    B["poseKnee" + sk] = r1(norm(thigh - shin - base.b0));
  }
}
// (ticket 350, Matthew: "the red and blue levers do not work with touch on M's ipad") a touch sends pointerdown, then
// touchstart: a press on the trap room's levers (drag.roomLever, pulled when let go) was dropped here, not being on a joint,
// the face, the friend or a thing, so the finger lifted on nothing; kept now, the page held still under it
pic.addEventListener("touchstart", e => {
  const onRoomLever = e.target && e.target.closest && e.target.closest("[data-lever]");
  if (e.touches.length === 1 && (onRoomLever || onJoint(e.touches[0]) || onFace(e.touches[0]) || onFigure(e.touches[0]) || onThing(e.touches[0]))) e.preventDefault();
  else drag = null;
}, { passive: false });
pic.addEventListener("touchmove", e => {
  if (drag && e.touches.length === 1) e.preventDefault();
}, { passive: false });
pic.addEventListener("pointerleave", () => { if (hoverOn) { hoverOn = null; showHover(); } });
// (ticket 369) a press the page made itself (a check's PointerEvent) has no pointer to capture; as the on-screen pad's
function picCapture(id) { try { pic.setPointerCapture(id); } catch (_) { /* (a synthetic press) */ } }
// (ticket 378, Matthew: "after closing a tree house trap door and reopening it -- we cannot click to activate walking and entering
// it"; "other glowing things ... are also inactive on click") A press the page took itself (a lever pulled, the friend or a thing
// held, a sign's board) was taken twice: at its letting go, and again by the click the browser sends after it, which went through the
// passage the pointer was near (hoverPass; with the pointer held, the click's target is the picture, not the friend or the lever, so
// its own test missed them): a click on a lever sent the friend out of the room while walking to it, the trip and the walk at odds,
// and a trip that never ends leaves every passage dead (entering). pressTaken: that click is the press's, and goes nowhere else
let pressTaken = false;
pic.addEventListener("pointerdown", e => {
  if (!e.isPrimary) return;
  pressTaken = false;
  if (hoverOn) { hoverOn = null; showHover(); }
  const joint = onJoint(e);
  if (joint) {
    drag = { pose: joint, T: { ...figureT } };
    poseHeld = { what: joint.what, side: joint.side, g: figureT.g };
    picCapture(e.pointerId); picDragging = true;   // (a drag: picGapNow waits for letGo)
    changed();   // (the ring)
    return;
  }
  // (ticket 305) the trap room's levers, on its wall: as a placed lever, a press let go before SIGN_DRAG_PX pulls one
  const roomLever = !onFigure(e) && e.target.closest && e.target.closest("[data-lever]");
  if (roomLever) { drag = { roomLever: roomLever.dataset.lever, cx: e.clientX, cy: e.clientY, far: false }; picCapture(e.pointerId); picDragging = true; return; }
  const thing = onThing(e);
  if (thing && !onFigure(e)) {
    const p = picUnits(e), B = state.background, name = thing.dataset.drag;
    // (ticket 185: a thing put in a place, "thing:i", is things[i], its feet at depth z)
    const T = name.startsWith("thing:") ? (B.things || [])[+name.slice(6)] : null;
    if (name.startsWith("thing:") && !T) return;
    drag = T ? { thing: name, put: +name.slice(6), x0: T.x, y0: T.z - SCENE.ground, px: p.x, py: p.y, drawn: { x: T.x, z: T.z } }
             : { thing: name, x0: B[name + "X"] ?? +(thing.dataset.x ?? 0), y0: B[name + "Y"] ?? +(thing.dataset.y ?? 0), px: p.x, py: p.y };
    // (ticket 280, Matthew: "click is a nav action, and drag is still moving the sign post") pressed on a signpost's board:
    // its way, and where the press began; it moves only once the pointer has gone SIGN_DRAG_PX, and a press let go before
    // then takes the board's way, as its nav arrow does
    const board = e.target.closest && e.target.closest(".sign-board[data-way]");
    if (board) Object.assign(drag, { board: board.dataset.way, cx: e.clientX, cy: e.clientY, far: false });
    // (ticket 284) pressed on a 🎚️ lever: as a board, a press let go before it has gone SIGN_DRAG_PX pulls it
    else if (T && T.kind === "lever") Object.assign(drag, { lever: true, cx: e.clientX, cy: e.clientY, far: false });
    // (ticket 471, the review after 0.0.385) pressed on a 🕳️ cave passage's opening: as a board, a press let go before it has
    // gone SIGN_DRAG_PX goes through it, as any doorway clicked; moved further, the passage is dragged
    else if (T && e.target.closest && e.target.closest(".thing .portal")) Object.assign(drag, { door: true, cx: e.clientX, cy: e.clientY, far: false });
    // (ticket 413) pressed on a thing one can sit on, with 👆 on: as a lever, a press let go before SIGN_DRAG_PX walks there and
    // sits on it; moved further, it is dragged as any thing
    else if (T && (state.access || DEFAULTS.access).clickWalk) { const st = thingSeatOf(thing); if (st) Object.assign(drag, { sit: st.name, cx: e.clientX, cy: e.clientY, far: false }); }
    picCapture(e.pointerId); picDragging = true;
    return;
  }
  if (!onFace(e)) {
    if (!onFigure(e)) return;
    const p = picUnits(e);
    drag = { figure: true, x0: state.background.figX ?? 0, y0: state.background.figY ?? 0, px: p.x, py: p.y };
    picCapture(e.pointerId); picDragging = true;
    return;
  }
  const [x, y] = faceUnits(e);
  const feature = y < faceLayout(state.face).eyeY - 12 - browLift(state.face) ? "brow" : "smile";
  drag = { feature, v0: state.face[feature], y0: e.clientY };
  picCapture(e.pointerId); picDragging = true;
});
// (ticket 133) A drag's moves come faster than frames (a 125 Hz mouse, a
// 240 Hz pencil, and all of them while the page is busy): each move updates
// the friend at once, and the picture is drawn once a frame, for the newest
// move. A timer stands in where frames do not come.
let dragFrame = 0, dragTimer = 0;
function dragDraw(last = false) { cancelAnimationFrame(dragFrame); clearTimeout(dragTimer); dragFrame = dragTimer = 0; moved(last === true); }   // (ticket 163: a drag's frames only draw)
function dragChanged() {
  if (dragFrame || dragTimer) return;
  dragFrame = requestAnimationFrame(dragDraw); dragTimer = setTimeout(dragDraw, 50);
}
// (ticket 254, Matthew: "the dragging performance issue we had in prior days is re-surfacing, but now with placed
// objects in the scene designer. when we drag them, the music pauses and glitches") A placed thing dragged is moved on
// the screen, every move, by a transform of its own drawing: to where it is now, at its size there (depthSize). The
// picture is drawn for real (its depth among the rest, the passages measured) only every THING_DRAG_DRAW ms and on
// letting go. A whole drawing at each move cost five times a friend's drag (measured: 10.7 ms a frame against 2.1, on
// GitHub's computers: the thing's piece of the scene built and mounted again, every passage measured again), enough on
// a slow computer to starve the music.
const THING_DRAG_DRAW = 200;
function thingDragMove(T) {
  const B = state.background, g = SCENE.ground, a = drag.drawn;
  const el = bgShown.node && bgShown.node.querySelector(`[data-drag="thing:${drag.put}"]`), host = el && (el.closest("[data-obj]") || el);
  if (host) {
    const r = depthSize(B, T.z - g) / Math.max(0.01, depthSize(B, a.z - g));
    host.setAttribute("transform", `translate(${f1(T.x)} ${f1(T.z)}) scale(${r.toFixed(4)}) translate(${f1(-a.x)} ${f1(-a.z)})`);
    drag.host = host;
  }
  if (!drag.redraw) drag.redraw = setTimeout(() => thingDragDraw(T), THING_DRAG_DRAW);
}
function thingDragDraw(T) {
  if (!drag || !drag.redraw) return;
  drag.redraw = 0;
  if (drag.host) { drag.host.removeAttribute("transform"); drag.host = null; }
  moved(true);
  drag.drawn = { x: T.x, z: T.z };
}
// One step per screen pixel, however big the picture is drawn.
pic.addEventListener("pointermove", e => {
  if (!drag) { hoverAt(e); walkSpotShow(e); return; }   // (ticket 251: the ground a click would walk to, glowing)
  if (drag.roomLever) { if (!drag.far && Math.hypot(e.clientX - drag.cx, e.clientY - drag.cy) >= SIGN_DRAG_PX) drag.far = true; return; }   // (ticket 305)
  if (drag.pose) { poseTo(drag.pose, figUnits(e, drag.T)); dragChanged(); return; }
  if (drag.thing) {
    if ((drag.board || drag.lever || drag.sit || drag.door) && !drag.far) { if (Math.hypot(e.clientX - drag.cx, e.clientY - drag.cy) < SIGN_DRAG_PX) return; drag.far = true; }
    // kept in the scene, as the figure is
    const p = picUnits(e), F = sceneFrame, B = state.background;
    const x = Math.round(clamp(drag.x0 + p.x - drag.px, F.left, F.right)), y = Math.round(clamp(drag.y0 + p.y - drag.py, figYMin(F), F.bottom - F.ground));
    const T = drag.put !== undefined && (B.things || [])[drag.put];
    // (ticket 502: a 🪵 fence piece goes from tile to tile as it is dragged, tileSnap; its rails reach their new neighbours at the next drawing)
    if (T) { T.x = x; T.z = SCENE.ground + y; tileSnap(B, T); thingDragMove(T); return; }   // (ticket 254)
    B[drag.thing + "X"] = x; B[drag.thing + "Y"] = y;
    dragChanged();
    return;
  }
  if (drag.figure) {
    // kept in the scene: its feet within the frame, from the frame's top
    // half down to just below the ground
    const p = picUnits(e), F = sceneFrame, BG = state.background;
    // (ticket 245) their feet over a rope bridge, outside the tree house: on it; off it again, where they are put
    const rx = drag.x0 + p.x - drag.px, ry = drag.y0 + p.y - drag.py;
    const br = BG.kind === "treehouse" ? nearBridge(rx, SCENE.ground + ry, BRIDGE_REACH * (SCENE.bottom - SCENE.top) / 700) : null;
    if (br) { bridgeOn(br.j, br.u); dragChanged(); return; }
    const x = Math.round(clamp(rx, F.left, F.right)), y = Math.round(clamp(ry, figYMin(F), F.bottom - F.ground));
    if (BG.onBridge) { delete BG.onBridge; BG.figX = x; BG.figY = y; }
    // (ticket 149: walls and solids stop a dragged friend too, who slides along them)
    else [BG.figX, BG.figY] = solidMove(BG.figX ?? 0, BG.figY ?? 0, x, y);
    dragChanged();
    return;
  }
  state.face[drag.feature] = clamp(Math.round(drag.v0 + (e.clientY - drag.y0)), -100, 200);
  dragChanged();
});
// (ticket 133) The picture's animations go on during a drag: the ducks keep
// paddling while you move the friend. (Since ticket 118 they held still
// during a drag, as drawing the witch's fumes was most of a frame; since
// 0.0.55 the fumes are cheap, and holding still bought nothing, measured.)
// (ticket 50) Sitting on a seat: the friend sits, facing a way the seat
// allows, across it where they were (or in its middle), and is moved up or
// down until their hips are on its top (their size changes with the depth,
// so a few rounds); kept by the seat's name. Standing up again puts their
// feet on the floor in front of it.
function sitOn(t) {
  if (t.lie) return lieOn(t);   // (ticket 299: a bed or a blanket, to lie on)
  const B = state.background, body = state.body;
  body.sit = true; body.pose = "sit"; B.seat = t.name;
  if (t.facing && t.facing.length && !t.facing.includes(body.facing || "front")) body.facing = t.facing[0];
  body.turn = "";   // (ticket 144: seated, not turned)
  const fx = B.figX ?? 0, m = Math.min(12, (t.x1 - t.x0) / 3);
  B.figX = Math.round(fx >= t.x0 + m && fx <= t.x1 - m ? fx : (t.x0 + t.x1) / 2);
  B.figY = Math.round(t.z - SCENE.ground);
  for (let i = 0; i < 4; i++) {
    draw();
    const T = figureT, hips = T.sg + T.dy + T.k * (poseJoints.legs[0].H[1] - T.g);
    B.figY = Math.round(B.figY + (t.y - hips));
  }
  changed();
}
// (ticket 299) Lying on a bed or a blanket: the friend lies down on it, the way its pillow says, the hips at its middle,
// resting on its top (the lowest of them as turned there: drawNow); sit too, so the seat is kept as a seat is
function lieOn(t) {
  const B = state.background, body = state.body;
  body.lieHead = t.head || "left"; strikePose("lie"); body.sit = true; B.seat = t.name;   // (the head first: on the back, it says which way they face)
  // (ticket 412) along a sleeping bag: on the back, facing us, the arms along the sides, the feet at its open end (drawNow
  // turns them to its slant and shortens them to its length, the head on its pillow)
  if (t.along) { body.facing = "front"; body.poseArmL = body.poseArmR = 80 - (body.arms ?? 0);
    B.figX = Math.round(t.along.near[0]); B.figY = Math.round(t.along.near[1] - SCENE.ground); changed(); return; }
  // (sunk in a little: it rests on its lower arm, which lifted the body clear of a soft bed)
  B.figX = Math.round((t.x0 + t.x1) / 2); B.figY = Math.round(t.y - SCENE.ground + (t.flat ? 4 : 10) * (SCENE.bottom - SCENE.top) / 700);
  changed();
}
function standUp() {
  const B = state.background, t = seatNow();
  state.body.sit = false; state.body.pose = "stand"; B.seat = null;
  if (t) B.figY = Math.round(t.z - SCENE.ground);
  changed();
}
// (a friend let go of over a seat, their feet between its top and the floor
// in front of it, sits on it; one sitting on a seat and let go of elsewhere
// stands up)
function dropOnSeat() {
  const B = state.background; if (B.kind === "none" || B.onBridge) return;   // (ticket 245: on a bridge, no seat)
  const x = B.figX ?? 0, y = SCENE.ground + (B.figY ?? 0), was = seatNow();
  const t = seatsHere(B).find(t => x >= t.x0 && x <= t.x1 && y >= t.y - 10 && y <= t.z + 18);
  if (t && (!was || was.name !== t.name)) sitOn(t);
  else if (!t && was) { state.body.sit = false; B.seat = null; changed(); }
}
const SIGN_DRAG_PX = 6;
const letGo = () => {
  if (dragFrame || dragTimer) dragDraw(true);   // (the last move, drawn before letting go)
  const way = drag && drag.board && !drag.far ? drag.board : null, pull = drag && drag.lever && !drag.far ? drag.put : null;
  const pullRoom = drag && drag.roomLever && !drag.far ? drag.roomLever : null;   // (ticket 305)
  const sitName = drag && drag.sit && !drag.far ? drag.sit : null;   // (ticket 413)
  const goDoor = drag && drag.door && !drag.far ? drag.put : null;   // (ticket 471: a cave passage pressed, not moved)
  picDragging = false;
  // (a joint let go: the friend settles on their lower foot, the frame round them)
  const posing = drag && drag.pose, placed = drag && drag.figure, put = drag && drag.put !== undefined;
  if (drag) pressTaken = true;   // (ticket 378: its click is this press's)
  if (put) { clearTimeout(drag.redraw); if (drag.host) drag.host.removeAttribute("transform"); }   // (ticket 254: drawn where it was let go)
  drag = null;
  if (put) changed();
  if (posing) { poseHeld = null; changed(); }
  if (placed) dropOnSeat();
  if (placed && state.body.anim === "climbup") { climb = null; climbAnywhere = false; changed(); }   // (ticket 137: a ladder within reach of where they were put)
  if (way) { const p = bgShown.node && bgShown.node.querySelector(`.portal[data-way="${way}"]`); if (p && !entering) enterScene(p); }   // (ticket 280: a board's way, as its arrow's click)
  // (ticket 471: the passage found again, drawn anew as it was let go)
  if (goDoor !== null) { const p = bgShown.node && bgShown.node.querySelector(`[data-drag="thing:${goDoor}"] .portal`); if (p && !entering) enterScene(p); }
  if (pull !== null) leverAct(`[data-drag="thing:${pull}"]`, () => leverPull(pull));   // (ticket 284; ticket 316: walked to and pulled)
  if (pullRoom) leverAct(`[data-lever="${pullRoom}"]`, () => roomLeverPull(pullRoom));   // (ticket 305)
  if (sitName && !entering && !(state.body.sit && state.background.seat === sitName)) {   // (ticket 413: as a seat clicked)
    const t = seatsHere(state.background).find(q => q.name === sitName);
    if (t) { walkSpotHide(); walkPlanned((t.x0 + t.x1) / 2, t.z + 6, () => sitOn(t)); }
  }
};
// (ticket 413) the seat a thing put in the place gives, at the friend's size, by its object's name, or null
function thingSeatOf(thingEl) {
  const o = thingEl && (bgShown.objs || []).find(o => o.node === thingEl || (o.node.contains && (o.node.contains(thingEl) || thingEl.contains(o.node))));
  return o ? thingSeats(state.background).find(t => t.name === o.name) || null : null;
}
// (ticket 316, Matthew: "when we trigger a lever for a trap door, we plan an animation and path: walk to the lever, facing
// it. pull it ... then after the pull, the door opens, animated") With 👆 walk where you click on, a lever pressed is a
// planned action: the friend walks to the floor just in front of its foot (a wall lever's: the back line before it),
// turns to face it, and pulls it with the dominant hand (startPull), and it goes over at the pull, its door swinging
// as before; already there, they pull it at once. Without 👆, it goes over at once, as before.
function leverAct(sel, act) {
  const A = state.access || DEFAULTS.access, BG = state.background, el = bgShown.node && bgShown.node.querySelector(sel), M = pic.getScreenCTM();
  // (ticket 364, Matthew: "a toggle to switch the levers without walking to them, so I can stand on the place where the
  // boulder falls, and still trigger that animation, for N") ⚙️ Advanced's 🎚️ pull levers from where you stand: at once, as before 316
  if (!A.clickWalk || A.leverFar || !el || !M || BG.kind === "none" || BG.onBridge) { act(); return; }
  const r = el.getBoundingClientRect(), c = new DOMPoint((r.left + r.right) / 2, r.bottom).matrixTransform(M.inverse());
  const g = SCENE.ground, k = (SCENE.bottom - SCENE.top) / 700, F = sceneFrame, z = Math.max(g + figYMin(F) + 4, Math.min(F.bottom - 4, c.y + 8 * k));
  // (ticket 378: pulled, they step aside, toward the room's middle, so the lever they stood before is in sight and in reach of a click
  // again: in front of it, their body covered it, and a click on it took hold of them instead)
  const aside = () => { const el2 = bgShown.node && bgShown.node.querySelector(sel), M2 = pic.getScreenCTM();   // (found again: the pull draws the place anew)
    if (clickRun || entering || drag || state.body.anim !== "none" || !el2 || !M2) return; const fb = friendBox(), r2 = el2.getBoundingClientRect(), q0 = new DOMPoint(r2.left, r2.top).matrixTransform(M2.inverse()), q1 = new DOMPoint(r2.right, r2.top).matrixTransform(M2.inverse());
    if (!fb) return; const half = (fb[2] - fb[0]) / 2, x = BG.figX ?? 0, lx0 = Math.min(q0.x, q1.x), lx1 = Math.max(q0.x, q1.x); if (x + half < lx0 || x - half > lx1) return;
    const side = (lx0 + lx1) / 2 > (F.left + F.right) / 2 ? -1 : 1, tx = side > 0 ? lx1 + half + 10 * k : lx0 - half - 10 * k;
    if (tx > F.left + half && tx < F.right - half) walkPlanned(tx, g + (BG.figY ?? 0)); };
  const pull = () => { if (BG.seat) standUp(); state.body.facing = "back"; state.body.turn = ""; startPull(act); setTimeout(aside, 1250); };
  if (Math.hypot(c.x - (BG.figX ?? 0), z - (g + (BG.figY ?? 0))) < 24 * k && !clickRun) { pull(); return; }
  walkPlanned(c.x, z, pull);
}
function startPull(then) {
  const body = state.body, h = body.hand ?? "right";
  pullSide = h === "left" ? 1 : h === "right" ? -1 : Math.random() < 0.5 ? 1 : -1;
  if (body.pose !== "stand") strikePose("stand");
  body.anim = "pull"; pullAt = performance.now(); changed();
  setTimeout(then, 560);
  setTimeout(() => { if (body.anim === "pull") { body.anim = "none"; changed(); } }, 1150);
}
// (ticket 284) A 🎚️ lever pulled: it flips over, and so does every 🕳️ trap door here it is wired to (its 🔗); the picture is
// drawn with it over, and then its handle on the screen turns across from where it was, in LEVER_TURN ms
// (ticket 307, Matthew: "the switch should animate too -- rocking back and forth somehow": it swings past and rocks back, settling)
const LEVER_TURN = 1400;   // (ticket 328: slower, to be seen; 900 till 0.0.231)
function leverPull(i) {
  const T0 = (state.background.things || [])[i];
  if (!T0 || T0.kind !== "lever") return;
  if (scriptEvent("Square", { tag: "PullThing", value: i })) return;   // (ticket 531: 📜 Square's rule, when it is on)
  hzJs("PullThing", () => {
    const B = state.background, T = B.things[i], doors = [];
    // (ticket 323: a boulder lever drops a boulder into each of its trap doors as it goes over, and lifts it out as it comes back)
    for (const d of B.things.entries()) { const [j, t] = d;
      if (t.kind !== "trapdoor" || !(T.works || []).includes(t.uid)) continue;
      if (T.does === "boulder") { if (!!t.boulder !== !T.on) doors.push([j, "boulder", !T.on]); } else doors.push([j, "open", !t.open]); }
    leverPullApply(i, doors);
  });
}
// (ticket 531) a placed lever's pull, decided (by the JS above, or a 📜 rule's OpenDoor and DropRock): it flips over, and each trap door
// named is opened or shut ([j, "open", Bool]) or has its boulder dropped or lifted ([j, "boulder", Bool]); then the animations
function leverPullApply(i, doors) {
  const B = state.background, T = (B.things || [])[i];
  if (!T || T.kind !== "lever") return;
  T.on = !T.on;
  const swung = [], dropped = [], seed = 1 + Math.floor(Math.random() * 999999);
  for (const [j, what, v] of doors) { const d = B.things[j];
    if (!d || d.kind !== "trapdoor") continue;
    if (what === "boulder") { d.boulder = v; d.rock = T.rock || ROCK_TONES[0]; if (v) d.rockSeed = seed + j; dropped.push([j, v]); }
    else { d.open = v; swung.push([j, v]); } }
  changed();
  turnArm(`[data-drag="thing:${i}"] .lever-arm`, T.on ? LEVER_TILT : -LEVER_TILT);
  for (const [j, open] of swung) lidSwing(`[data-drag="thing:${j}"] .pit-lid`, open);   // (ticket 307)
  // (a boulder lifted out is drawn no more once it has gone: drawn again, held, for its rising; ticket 323)
  for (const [j, down] of dropped) if (down) boulderFall(`[data-drag="thing:${j}"] [data-boulder]`, true, BOULDER_FALL, boulderHits, null, boulderFalling);
  // (a placed boulder lever lifting its boulders: sparkles from the lever and from each trap door they leave)
  if (T.does === "boulder" && !T.on) { sparkleFrom(`[data-drag="thing:${i}"] .lever-arm circle`, 14); for (const [j] of dropped) sparkleFrom(`[data-drag="thing:${j}"]`, 22); }
}
// (ticket 305) The trap room's lever pulled: it flips over, and so does every pit it works (trapState); its handle turns across
function roomLeverPull(side) {
  if (side[0] === "B") return roomBoulderPull(side.slice(1));   // (ticket 323: a blue one)
  if (side === "H") return hatchLeverPull();   // (ticket 334: a tree room's trap door's)
  if (scriptEvent("TrapRoom", { tag: "Pull", value: side })) return;   // (ticket 482: 📜 Scripting's rule for the trap room, when it is on)
  hzJs("Pull", () => roomLeverFlip(side));   // (ticket 531: timed as the rule is)
}
// a red lever flipped, and the pits it works with it (the pull itself, and a script's Flip; ticket 482)
// (ticket 531: lids false, a 📜 rule's Flip, whose SetPits swing the lids)
function roomLeverFlip(side, lids = true) {
  const B = state.background;
  if (B.kind !== "castleroom") return;
  const K = castleOf(B), n = Math.max(1, Math.min(K.rooms.length, Math.round(B.castleRoom ?? 1))), key = leverKey(side, n, K.id);
  const was = trapState(B, n, K.id).open;
  B[key] = !leverDown(B, side, n, K.id);   // (ticket 307: never touched, it was down)
  changed();
  const up = !B[key], sd = side === "L" ? -1 : 1, T = trapState(B, n, K.id), now = T.open;
  turnArm(`[data-lever="${side}"] .lever-arm`, (up ? 1 : -1) * sd * LEVER_TILT, LEVER_TURN * 100 / T.speed.lever);   // (ticket 328: the room's speeds)
  if (!lids) return;
  for (const j of [0, 1]) if (now[j] !== was[j]) lidSwing(`.pit-lid[data-pit-i="${j}"]`, now[j], PIT_SWING * 100 / T.speed.lid);   // (ticket 307)
  // (Matthew: "opened by the red lever (trap door) while standing on it, and no falling sequence was triggered") a pit opened
  // under the friend: in they fall, as walking onto it does, once its lid has swung most of the way
  if (now.some((o, j) => o && !was[j])) setTimeout(fallIfOverPit, 0.6 * PIT_SWING * 100 / T.speed.lid);
}
// (ticket 531) a trap room's pit's lid swung open or shut, as a 📜 rule's SetPit says, and in they fall if it opened under the
// friend; false when the drawing (trapState, from the levers) has it otherwise
function roomPitSwing(j, open) {
  const B = state.background;
  if (B.kind !== "castleroom") return false;
  const K = castleOf(B), n = Math.max(1, Math.min(K.rooms.length, Math.round(B.castleRoom ?? 1))), T = trapState(B, n, K.id);
  lidSwing(`.pit-lid[data-pit-i="${j}"]`, open, PIT_SWING * 100 / T.speed.lid);
  if (open) setTimeout(fallIfOverPit, 0.6 * PIT_SWING * 100 / T.speed.lid);
  return T.open[j] === open;
}
// (ticket 334) A tree room's trap door lever pulled: the trap door shuts (its lid swinging across, the ladder gone, the floor
// walked over) or opens again; a friend standing on it as it opens is moved off the hole
// (ticket 531: the house, 0 for the round room at the top, and its key; hatchSet, the door shut or opened, by the JS or a 📜 rule)
const hatchHouse = () => { const B = state.background; return B.kind === "treetop" ? 0 : Math.max(1, Math.min(6, B.houses ?? 4, Math.round(B.inside ?? 1))); };
const hatchKey = () => hatchHouse() === 0 ? "hatchShutTop" : "hatchShut" + hatchHouse();
function hatchLeverPull() {
  const B = state.background;
  // (ticket 516: and the round room at the top of the tree's, on its post)
  if (B.kind !== "treeroom" && B.kind !== "treetop") return;
  if (scriptEvent("TreeRoom", { tag: "PullHatch", value: hatchHouse() })) return;   // (ticket 531: 📜 TreeRoom's rule, when it is on)
  hzJs("PullHatch", () => hatchSet(!B[hatchKey()]));
}
function hatchSet(shut) {
  const B = state.background, key = hatchKey();
  B[key] = shut;
  changed();
  const open = !B[key];
  turnArm(`[data-lever="H"] .lever-arm`, (open ? 1 : -1) * LEVER_TILT);
  lidSwing(`.pit-lid[data-pit-i="hatch"]`, open);
  if (open) setTimeout(() => { solidUnstick(200); changed(); }, 0);
}
// (the friend, standing still, over a pit's open hole: falling in, as walking onto it does; not mid-jump or on a trip)
function fallIfOverPit() {
  const BG = state.background;
  if (entering || BG.kind === "none" || isJump(state.body.anim)) return;
  const pit = pitAt(BG.figX ?? 0, SCENE.ground + (BG.figY ?? 0));
  if (pit) enterScene(pit);
}
// (ticket 323) A trap room's boulder lever pulled: it flips over; each pit it works (by the room's wiring) has its boulder
// fall from the ceiling into the hole, or rise away again. A new seed each time one falls: a new rock (its shape, and its
// tone when the room's boulders are a surprise)
function roomBoulderPull(side) {
  if (scriptEvent("TrapRoom", { tag: "PullBoulder", value: side })) return;   // (ticket 531: 📜 TrapRoom's rule, when it is on)
  hzJs("PullBoulder", () => {
    const B = state.background;
    if (B.kind !== "castleroom") return;
    const K = castleOf(B), n = Math.max(1, Math.min(K.rooms.length, Math.round(B.castleRoom ?? 1)));
    // (ticket 338) the pits this lever works: a fallen rock still on any, lifted away (each rising, and gone from the place's
    // things); else into each a new rock falls, which once landed is a placed rock, "fallen N", dragged where one likes
    const T0 = trapState(B, n, K.id), mine = [0, 1].filter(j => T0.wire[j] === side && T0.on[j]), lift = mine.some(j => T0.covered[j]);
    roomBoulderAct(side, mine, lift);
  });
}
// (ticket 531) what a blue lever does once it is decided (by the JS above, or a 📜 rule's DropBoulder and LiftBoulder): the pits
// mine, lifting their rocks or dropping new ones
function roomBoulderAct(side, mine, lift) {
  const B = state.background;
  if (B.kind !== "castleroom") return;
  const K = castleOf(B), n = Math.max(1, Math.min(K.rooms.length, Math.round(B.castleRoom ?? 1))), key = boulderKey(side, n, K.id);
  const T0 = trapState(B, n, K.id);
  B[key] = !lift;
  if (!lift) for (const j of mine) B[boulderSeedKey(j, n, K.id)] = 1 + Math.floor(Math.random() * 999999);   // (ticket 328: a new rock, its own seed)
  changed();
  const down = B[key], sd = side === "L" ? -1 : 1, sp = trapState(B, n, K.id).speed, dur = BOULDER_FALL * 100 / sp.boulder;
  turnArm(`[data-lever="B${side}"] .lever-arm`, (down ? -1 : 1) * sd * LEVER_TILT, LEVER_TURN * 100 / sp.lever);
  if (!lift) { for (const j of mine) boulderFall(`[data-boulder="${j}"]`, true, dur, boulderHits, g => boulderLanded(g, fallenPitKey(n, K.id, j)), boulderFalling); return; }
  // (Matthew: "when the boulder lifts, can we have some sparkles shoot out of the boulder and lever -- like lifting is magic?")
  sparkleFrom(`[data-lever="B${side}"] .lever-arm circle`, 14);
  setTimeout(fallIfOverPit, 0.7 * dur);   // (a boulder lifted from an open pit the friend stands on: in they fall)
  const keys = mine.filter(j => T0.covered[j]).map(j => fallenPitKey(n, K.id, j));
  const rocks = (B.things || []).filter(t => t.fallen && keys.includes(t.pit) && fallenOn({ things: [t] }, t.pit));
  for (const t of rocks) { const sel = `[data-drag="thing:${B.things.indexOf(t)}"]`;
    swingOn(sel, dur, (g, u) => g.setAttribute("transform", `translate(0 ${f1(-520 * u * u)})`));
    [0, 0.18, 0.36, 0.54].forEach((u, m) => setTimeout(() => sparkleFrom(sel, m ? 12 : 22), u * dur)); }
  setTimeout(() => { if (!rocks.length) return; B.things = (B.things || []).filter(t => !rocks.includes(t)); changed(); }, dur + 60);
}
// (ticket 338) a room's boulder landed: the place's things gain it, a rock where it lies, as big and of the tone and shape it
// was drawn with, named "fallen N" (the place's next number), its pit's key and where it landed kept; the room's own rock back
// up at the ceiling, hidden, the next to fall
function boulderLanded(g, pit) {
  const B = state.background, [x, z, rad, seed, tone] = (g.dataset.at || "").split(" ");
  if (!g.dataset.at || B.kind !== "castleroom") return;
  const T = B.things || (B.things = []), N = 1 + Math.max(0, ...T.map(t => t.fallen || 0));
  const X = +x, Z = +z, R = +rad, ds = depthSize(B, Z - SCENE.ground) || 1;
  T.push({ kind: "rock", x: Math.round(X), z: Math.round(Z), seed: +seed, size: Math.round(100 * R / (38 * ds)), mirror: false, tone, fallen: N, pit, px: Math.round(X), pz: Math.round(Z), prad: Math.round(R) });
  g.setAttribute("transform", `translate(0 ${f1(-(+g.dataset.drop))})`); g.setAttribute("visibility", "hidden");
  changed();
}
// (a boulder falling from the ceiling into its pit, faster as it falls, a little bounce as it lands; or rising away, slowly
// at first, out of sight; in BOULDER_FALL ms, carried on through a redrawing as every swing is)
const BOULDER_FALL = 1300;   // (ticket 328: slower, to be seen; 720 in 0.0.231)
// (ticket 349: onLand(g), once, as it lands: whether it fell on the friend)
function boulderFall(sel, down, dur = BOULDER_FALL, onLand = null, onDone = null, onFall = null) {
  let landed = false, finished = false;
  swingOn(sel, dur, (g, u) => { const H = +g.dataset.drop;
    if (u >= 1 && onDone && !finished) { finished = true; setTimeout(() => onDone(g), 0); }   // (ticket 338: once, at its end)
    if (down && u >= 0.8 && !landed) { landed = true; if (onLand) onLand(g); }
    const y = down ? (u < 0.8 ? -H * (1 - (u / 0.8) ** 2) : -H * 0.05 * Math.sin(Math.PI * (u - 0.8) / 0.2)) : -H * u * u;
    if (u < 1) { g.removeAttribute("visibility"); g.setAttribute("transform", `translate(0 ${f1(y)})`); if (down && onFall && u < 0.8) onFall(g, y); return; }   // (ticket 415: each frame of the drop)
    if (down) g.removeAttribute("transform"); else { g.setAttribute("transform", `translate(0 ${f1(-H)})`); g.setAttribute("visibility", "hidden"); } });
}
// (ticket 322) A swing on the screen, found again each frame by its selector in the scene, not held as one element: the
// room drawn again in the middle of it (in Firefox the music's clock can stand still while the picture's runs, so the
// gap between them is measured again and the scene's text changes) gets the swing carried on in its new drawing, where
// before the new drawing showed its end at once. One swing per selector: a new one stops the last. By frames, a timer
// standing in where frames do not come, as for a drag; its clock starts at its first frame, not at the pull (a room slow
// to draw again would otherwise use it up unseen). apply(el, u) draws it at u (0 to 1), and once more at 1 to end it.
const SWINGS = new Map();
// (Matthew: "one glitchy frame, where the boulder is still there after being lifted back up"; "similar glitch for the trap
// door itself") A swing changes the page, not the scene's text, and a slow computer's still pictures (stillPart) are copies
// of the page kept by that text: one copied before or during a swing came back for a frame after it. A swing marks them out
// of date as it starts and as it ends, and none is used while one runs (stillPicWanted).
function swingOn(sel, dur, apply) {
  const was = SWINGS.get(sel); if (was) was.stop();
  stillPic.text = null;
  let raf = 0, tm = 0, t0 = null, done = false, lastU = 0;
  // (redo: drawn again at once where it is, in a room just built (mountScene), not a frame later at its end)
  const me = { stop: () => { done = true; cancelAnimationFrame(raf); clearTimeout(tm); }, redo: root => { const el = root.querySelector(sel); if (el) apply(el, lastU); } };
  SWINGS.set(sel, me);
  const step = () => { cancelAnimationFrame(raf); clearTimeout(tm); if (done) return;
    const el = bgShown.node && bgShown.node.querySelector(sel);
    const now = performance.now(); if (t0 === null) t0 = now;
    const u = Math.min(1, (now - t0) / dur); lastU = u;
    if (el) apply(el, u);
    if (u < 1) { raf = requestAnimationFrame(step); tm = setTimeout(step, 40); } else { done = true; stillPic.text = null; if (SWINGS.get(sel) === me) SWINGS.delete(sel); } };
  const el = bgShown.node && bgShown.node.querySelector(sel);
  if (el) apply(el, 0);
  raf = requestAnimationFrame(step); tm = setTimeout(step, 40);
}
// (a lever's handle, found by sel, turned across to its new angle `to` from the other side, past it and rocking back
// (ticket 307), in LEVER_TURN ms)
function turnArm(sel, to, dur = LEVER_TURN) {
  swingOn(sel, dur, (arm, u) => { const [px, py] = arm.dataset.pivot.split(" ").map(Number);
    const a = u <= 0 ? -to : u < 1 ? to - 2 * to * Math.exp(-5 * u) * Math.cos(3 * Math.PI * u) : to;   // (past it by a third, back, and still)
    arm.setAttribute("transform", `rotate(${f1(a)} ${f1(px)} ${f1(py)})`); });
}
// (ticket 307) a pit's lid, found by sel, swung on the screen, after the picture is drawn with it as it ends: open, from
// across the opening down into the pit on its hinge (falling, faster as it goes), the shade deepening; shut, back up
// across it (slowing as it lands); in PIT_SWING ms
const PIT_SWING = 800;   // (ticket 328: slower, to be seen; 520 till 0.0.231)
function lidSwing(sel, open, dur = PIT_SWING) {
  swingOn(sel, dur, (lid, u) => {
    const y0 = +lid.dataset.hinge, shade = lid.querySelector(".lid-shade");
    const at = s => { lid.setAttribute("transform", lidAt(y0, s)); if (shade) shade.setAttribute("opacity", (0.7 * (1 - s)).toFixed(2)); };
    if (u < 1) { lid.removeAttribute("visibility"); at(Math.max(0.001, open ? 1 - u * u : 1 - (1 - u) * (1 - u))); return; }
    if (open) { at(0); lid.setAttribute("visibility", "hidden"); }
    else { lid.removeAttribute("transform"); if (shade) shade.setAttribute("opacity", "0"); } });
}
pic.addEventListener("pointerup", letGo);
// (ticket 133: a touch the browser takes over, as a scroll or a gesture, ends
// the drag too, or the friend stays held)
pic.addEventListener("pointercancel", letGo);
pic.addEventListener("pointercancel", letGo);

// Through a doorway (a picture, a window, a tree house, a door: .portal in
// the scene) into its scene: the friend flies to it, shrinking to fit it
// and spinning, slowly and then fast, and lands in the new scene where a
// friend starts, at the size they had.
let entering = false;
// (ticket 199) the pointer over a passage, or near where a nav arrow would be: the bright one
// (ticket 236: on the picture's box, not the picture alone: with 🧭 every arrow shown, the pointer on an arrow is over the
// arrow, not the picture, and it stayed dim, never the one picked)
pic.parentNode.addEventListener("pointermove", e => {
  if (e.buttons || drag || entering) return;
  const h = passUnderPointer(e);
  if ((h && h.el) !== (hoverPass && hoverPass.el)) { hoverPass = h; showEnterGlow(); }
});
pic.parentNode.addEventListener("pointerleave", () => { if (hoverPass) { hoverPass = null; showEnterGlow(); } });
pic.addEventListener("click", e => {
  if (pressTaken) { pressTaken = false; return; }   // (ticket 378: a press the page took itself, already done at its letting go)
  // (ticket 199: the one under the pointer, even far off: walked or swum to, and gone through)
  if (hoverPass && !entering && e.target.closest && !e.target.closest("#figure, [data-drag^='thing:']")) {
    const h = marks.pics.find(q => q.el === hoverPass.el && q.el.isConnected);
    if (h) { enterAt = h; hoverPass = null; enterPicture(); return; }
  }
  // (ticket 173: a ladder clicked is climbed, whether or not it is in a doorway)
  const ladder = !entering && e.target.closest && e.target.closest("#bg [data-ladder]");
  if (ladder) { climbLadder(ladder); return; }
  const door = !entering && e.target.closest && e.target.closest("#bg .portal");
  if (door) { enterScene(door); return; }
  // (ticket 299) a seat or a bed clicked, with 👆 on: walked to, and sat on, or lain on
  const seat = seatAtPointer(e);
  if (seat) { walkSpotHide(); walkPlanned((seat.x0 + seat.x1) / 2, seat.flat ? seat.z : seat.z + 6, () => sitOn(seat)); return; }
  // (ticket 266) walking to a click already: another click on the ground is queued after it
  if (clickRun) { const spot = walkableAt(e); if (spot) walkPlanned(spot.x, spot.z); return; }
  // (ticket 187) a passage behind something standing in front of it (a fence, a bush, a sign's post):
  // through it too; never one behind the friend or a thing put there, which are for dragging
  if (entering || !e.target.closest || e.target.closest("#figure, [data-drag], #ways") || marks.text !== bgShown.text) return;
  const q = picUnits(e), hit = marks.pics.filter(p => p.kind !== "nav" && p.el.isConnected && (p.within ? p.within(q.x, q.y) : q.x >= p.x0 && q.x <= p.x1 && q.y >= p.y0 && q.y <= p.y1))
                                         .sort((a, b) => (a.x1 - a.x0) * (a.y1 - a.y0) - (b.x1 - b.x0) * (b.y1 - b.y0))[0];
  if (hit) { enterAt = hit; enterPicture(); return; }
  // (ticket 251) on the ground, with 👆 walk where you click on: walked there, round what stands in the way
  const spot = walkableAt(e);
  if (spot) { walkSpotHide(); walkPlanned(spot.x, spot.z); }
});
// (ticket 251, Matthew: "accessibility toggle, and walk to click feature. When enabled, we show a glowing circle of some
// custom color over any area on the ground that's 'walkable' when the cursor is over it. When clicked, we plan a path to
// that point, and execute the path plan.") Where a click would walk the friend to: the floor under the pointer, when ♿
// 👆 is on and they could stand there (inside the place's floor and its walls, not in a solid or over a pit), and the
// pointer is on nothing that does its own thing (the friend, a thing put there, a passage or a ladder)
function walkableAt(e) {
  const BG = state.background, A = state.access || DEFAULTS.access;
  if (!A.clickWalk || BG.kind === "none" || (entering && !clickRun) || drag || (hoverPass && !clickRun)) return null;   // (ticket 266: while walking to a click, more)
  const t = e.target;
  if (!t || !t.closest || !t.closest("#pic") || t.closest("#figure, [data-drag], .portal, [data-ladder], #ways")) return null;
  const q = picUnits(e), F = sceneFrame, y = q.y - SCENE.ground;
  if (q.x < F.left || q.x > F.right || y < figYMin(F) || q.y > F.bottom) return null;
  if (solidAt(q.x, q.y) || pitAt(q.x, q.y)) return null;   // (walls too: solidAt's)
  return { x: q.x, z: q.y };
}
// (ticket 299, Matthew: "point to sit, and point to lie down"; "pointing at a place to sit or lie down shows a pop-up
// that says so") The seat or bed under the pointer, with 👆 on: its box, from where a sitter's feet go (z) up past its top
// by as much again, across its span (a blanket's, flat, a band about its middle); the friend not on it already
function seatAtPointer(e) {
  const BG = state.background, A = state.access || DEFAULTS.access;
  if (!A.clickWalk || BG.kind === "none" || drag || !e.target || !e.target.closest || !e.target.closest("#pic") || e.target.closest("#figure, .portal, [data-ladder], #ways")) return null;
  // (ticket 413: over a thing put here, its own seat if it gives one; never another's through it)
  const put = e.target.closest("[data-drag]");
  if (put) { const t = put.dataset.drag.startsWith("thing:") ? thingSeatOf(put) : null; return t && t.name !== (state.body.sit && BG.seat) ? t : null; }
  const q = picUnits(e), k = (SCENE.bottom - SCENE.top) / 700;
  return seatsHere(BG).find(t => t.name !== (state.body.sit && BG.seat) && q.x >= t.x0 && q.x <= t.x1 &&
    (t.flat ? Math.abs(q.y - t.y) <= 24 * k : q.y >= t.y - Math.max(20 * k, t.z - t.y) && q.y <= t.z + 6)) || null;
}
// its glow: an ellipse on the floor there, as wide as the friend's step at that depth, in ♿'s colour, pulsing
// (ticket 299: over a seat or a bed, its box glowing instead, and what a click does there: sit, or lie down)
function walkSpotShow(e) {
  const layer = document.getElementById("access-layer"), seat = layer && seatAtPointer(e), s = !seat && walkableAt(e);
  let ov = layer && layer.querySelector("svg.walk-spot");
  if (seat) {
    const M = pic.getScreenCTM(), box = layer.getBoundingClientRect(); if (!M) return;
    const at = (x, y) => { const p = new DOMPoint(x, y).matrixTransform(M); return [p.x - box.left, p.y - box.top]; };
    const top = seat.flat ? seat.y - 18 * (SCENE.bottom - SCENE.top) / 700 : seat.y, [ax, ay] = at(seat.x0, top), [bx, by] = at(seat.x1, seat.flat ? seat.y + 18 * (SCENE.bottom - SCENE.top) / 700 : seat.z);
    if (!ov) { ov = document.createElementNS("http://www.w3.org/2000/svg", "svg"); ov.setAttribute("class", "walk-spot"); layer.append(ov); }
    const say = (seat.lie ? "🛏️ lie down on the " : "🪑 sit on the ") + seat.name.replace(/,.*$/, "").replace(/\s+\d+$/, "");   // (ticket 413: "the rock", not "the rock 1")
    ov.innerHTML = `<rect class="seat-spot" x="${ax.toFixed(1)}" y="${ay.toFixed(1)}" width="${(bx - ax).toFixed(1)}" height="${Math.max(6, by - ay).toFixed(1)}" rx="6"/>` +
      `<text class="seat-say" x="${((ax + bx) / 2).toFixed(1)}" y="${(ay - 8).toFixed(1)}" text-anchor="middle">${xmlEsc(say)}</text>`;
    ov.style.setProperty("--spot", (state.access || DEFAULTS.access).clickWalkColor || "#22d3ee");
    return;
  }
  if (ov && !ov.querySelector("ellipse")) { ov.remove(); ov = null; }
  if (!layer || !s) { walkSpotHide(); return; }
  const M = pic.getScreenCTM(), box = layer.getBoundingClientRect();
  if (!M) return;
  const c = new DOMPoint(s.x, s.z).matrixTransform(M), k = (SCENE.bottom - SCENE.top) / 700, rx = 34 * k * depthSize(state.background, s.z - SCENE.ground) * M.a;
  if (!ov) { ov = document.createElementNS("http://www.w3.org/2000/svg", "svg"); ov.setAttribute("class", "walk-spot"); ov.innerHTML = "<ellipse/>"; layer.append(ov); }
  const el = ov.firstChild;
  for (const [a, v] of [["cx", c.x - box.left], ["cy", c.y - box.top], ["rx", rx], ["ry", rx * 0.36]]) el.setAttribute(a, v.toFixed(1));
  ov.style.setProperty("--spot", (state.access || DEFAULTS.access).clickWalkColor || "#22d3ee");
}
function walkSpotHide() { const ov = document.querySelector("#access-layer svg.walk-spot"); if (ov) ov.remove(); }
pic.addEventListener("pointerleave", walkSpotHide);
// (the walk: planned round what stands in the way, planPath, its legs walked one after another, walkTo; 👣 shows the plan)
// (ticket 266, Matthew: "queue of clicks. when I click, I have to wait for the plan to animate before I can see my mouse
// over and click again. let's lift that, and permit unlimited plans, that consist of multiple clicks"; "each segment
// gets a distinct color -- so maybe we have two colors --- main color and alt color, and we alternate along the
// multi-click plan when its shown") A click while walking to one before it: its own walk, planned from where that one
// ends, added to the run (clickRun) and walked on after it, as many as are clicked; each click's points marked in
// walkPath.segs (0, 1, 2...), 👣 drawing them by turns in its colour and its alt colour. The controller ends the run.
// (ticket 294, Matthew: "in play it is often expected, and more useful, to cancel the current plan and replace it with
// the one for the next click") Or, as ♿ 👆 says (clickPlan), a click while walking stops the walk where the friend is and
// plans a new one from there (walkStop: the leg under way stops, without walking on); "queue", as before, adds to it.
let clickRun = null;
// (ticket 299: then, what to do on getting there, the last click's: sit, or lie down)
// (ticket 367, Matthew: "spiral stair topology is still perplexing the path planner") In the spiral staircase, a click behind
// the bottom steps, beside the column, was reached the long way round the column (612 units for 131 straight): the gap
// between the steps and the column is narrower than the feet. The room has its own way, up the steps' middle (spiralPath,
// ticket 268, used for its doors); a click walk takes it when the floor plan has to go far round (over twice the straight
// way, or no way at all) and the stairs are the shorter
// (ticket 369: walk251, paths215, the speed run's) how much further than the feet as they stand a walk's steps reach: a stride
// in profile spreads the soles about 22 + 0.44 of the legs' length (the friend's own units: 84 for the default, legs 140)
// either side, against 49 standing toward us and 22 in profile, so a walk planned from a stance kept the stance's reach (or
// 10 more) and brushed a cauldron, a big root, with a foot; the stride's reach at the friend's size now, whatever the stance
const strideMore = () => Math.max(10, stridePlus());   // (stridePlus: 16's, solidMove's too)
function planWalk(x0, y0, x1, y1, more = 0) {
  const P = planPath(x0, y0, x1, y1, more), BG = state.background;
  if (BG.kind !== "castleroom" || !castleWallsOf(BG).stairs) return P;
  const len = pts => pts.reduce((a, p, i) => { const q = i ? pts[i - 1] : [x0, y0]; return a + Math.hypot(p[0] - q[0], (p[1] - q[1]) / 0.6); }, 0);
  const lp = len(P), direct = Math.hypot(x1 - x0, (y1 - y0) / 0.6);
  if (!P.short && lp <= 2 * direct) return P;
  const S = spiralPath(x0, y0, x1, y1);
  return P.short || len(S) < lp ? S : P;
}
function walkPlanned(x, z, then = null) {
  const BG = state.background, g = SCENE.ground;
  if (clickRun && (state.access || DEFAULTS.access).clickPlan !== "queue") { walkStop++; clickRun = null; entering = false; }
  if (clickRun) {
    const [lx, ly] = clickRun.end, path = planWalk(lx, ly, x, z - g, strideMore());
    for (const p of path) { walkPath.pts.push(p); walkPath.segs.push(clickRun.n); }
    clickRun.n++; clickRun.end = path.short ? (path.length ? path[path.length - 1] : [lx, ly]) : [x, z - g]; clickRun.then = path.short ? null : then;
    return;
  }
  bridgeOff();
  const from = [BG.figX ?? 0, BG.figY ?? 0], path = planWalk(from[0], from[1], x, z - g, strideMore());   // (the steps swing wider than the feet standing; ticket 367: the spiral room's stairs)
  if (path.short) then = null;   // (ticket 332: no way there, as near as a way goes, and nothing done after: not sat, not gone in)
  const run = clickRun = { n: 1, end: [x, z - g], then };
  walkPath = { pts: [from, ...path], segs: [0, ...path.map(() => 0)], made: performance.now(), leg: 0 };
  let k = 0;
  const leg = () => {
    if (clickRun !== run) { if (!entering) { state.body.anim = "none"; changed(); } return; }
    if (++k >= walkPath.pts.length) { const done = run.then; clickRun = null; state.body.anim = "none"; changed(); if (done) done(); return; }
    walkPath.leg = k - 1;
    walkTo(walkPath.pts[k][0], g + walkPath.pts[k][1], leg, k < walkPath.pts.length - 1);   // (ticket 321: more legs to come, walked on without a stop)
  };
  leg();
}
// (ticket 221) B held on the keyboard: bubbles, as 🫧 on the controller
document.addEventListener("keydown", e => {
  if ((e.key !== "b" && e.key !== "B") || (e.target.closest && e.target.closest("input, textarea, select, [contenteditable]"))) return;
  blowKey = true; blowGo();
});
document.addEventListener("keyup", e => { if (e.key === "b" || e.key === "B") blowKey = false; });
window.addEventListener("blur", () => { blowKey = false; });
// (ticket 499) F held on the keyboard: breathing fire, as 🔥 on the controller
document.addEventListener("keydown", e => {
  if ((e.key !== "f" && e.key !== "F") || e.ctrlKey || e.metaKey || e.altKey || (e.target.closest && e.target.closest("input, textarea, select, [contenteditable]"))) return;
  fireKey = true; fireGo();
});
document.addEventListener("keyup", e => { if (e.key === "f" || e.key === "F") fireKey = false; });
window.addEventListener("blur", () => { fireKey = false; });
// (ticket 187) Enter: through the passage that glows, as X
document.addEventListener("keydown", e => {
  if (e.key !== "Enter" || e.repeat || (e.target.closest && e.target.closest("input, textarea, select, button, [contenteditable]"))) return;
  if (enterAt) { e.preventDefault(); enterPicture(); }
});
// a nested <svg>'s own box on the screen, from its x, y, width and height
function svgBox(el) {
  const m = el.parentNode.getScreenCTM(), g = k => el[k].baseVal.value;
  const p = new DOMPoint(g("x"), g("y")).matrixTransform(m), q = new DOMPoint(g("x") + g("width"), g("y") + g("height")).matrixTransform(m);
  return { x: Math.min(p.x, q.x), y: Math.min(p.y, q.y), width: Math.abs(q.x - p.x), height: Math.abs(q.y - p.y) };
}
// The friend flies into a box on the screen (a doorway, or the back chip),
// shrinking to fit it and spinning, slowly and then fast; then the scene
// changes (arrive) and they land, spinning out, at their own size.
// (ticket 193, Matthew 2026-10-17: going into a picture, the friend froze
// in place and we waited; coming out, some limbs waited in their places,
// never spinning) The friend is drawn in pieces, the far arm apart among the
// scene's objects (figShown), and every drawing puts fresh pieces in: the
// spin turned one piece, in a group a drawing during it emptied, while a
// still friend was put back. So the spin turns a copy of all of them, on
// top of the picture, while the friend itself is hidden (figAway: every
// drawing hides its pieces again), going in and coming out. The copy's ids
// are its own, so its masks and patterns are its own, not the hidden ones'.
let figAway = false;
// (ticket 349, Matthew, for N: "we turn into a stack of rainbow bubbles, and spill out, in all directions, and pop, at
// different rates. then, as we were before, we fade back in, re-generating and continuing, after the animation of bubbles
// completes. it's simulating getting crushed by a rock in a whimsical way that N wanted. -- no blood, just rainbow bubbles,
// getting wild.") A boulder landing on the friend: where it lands (its drawing's box, as it lands), the friend's feet on
// it. Then the friend goes to rainbow bubbles part by part as the boulder comes down on them (ticket 415: head, torso,
// legs, feet; burstPart), each part hidden and in its place bubbles of its shape, coloured from red at the top to violet
// at the feet, which spill out every way, drift, slow and float, each popping at its own time; once every part has gone
// the friend is hidden whole (figBurst, as a spin hides them), and when the last bubble has gone, they are back where
// they stood (out from under the boulder: solidUnstick), fading in, and on as before. The controls rest while it plays
// (entering).
let figBurst = false, burstLayer = null, sparkleLayer = null;
// (lifting is magic: stars shoot out of what is lifted and what lifts it, drawn as the night sky's are, by its ⭐ settings (shape,
// size, brightness), spinning out, slowing, fading in about a second; from the middle of what sel finds, n of them)
function sparkleFrom(sel, n = 16) {
  const el = bgShown.node && bgShown.node.querySelector(sel), P = pic.getScreenCTM();
  if (!el || !P) return;
  const r = el.getBoundingClientRect(), mid = new DOMPoint(r.left + r.width / 2, r.top + r.height / 2).matrixTransform(P.inverse());
  sparkleBurst(mid.x, mid.y, n, Math.max(20, Math.min(120, Math.hypot(r.width, r.height) / Math.hypot(P.a, P.b))));
}
function sparkleBurst(x, y, n, span) {
  const B = state.background, NS = "http://www.w3.org/2000/svg", k = (sceneFrame.bottom - sceneFrame.top) / 700, rnd = Math.random;
  const pick = STAR_SHAPES.some(([v]) => v === B.starShape) && B.starShape !== "dot" ? B.starShape : "mix", size = (B.starSize ?? 100) / 100, bright = Math.max(0.3, (B.starBright ?? 85) / 100);
  if (!sparkleLayer) { sparkleLayer = document.createElementNS(NS, "g"); sparkleLayer.id = "sparkles"; sparkleLayer.setAttribute("pointer-events", "none"); }
  burstKeep();
  const tints = [...STAR_TINTS, "#ffe680", "#c4b5fd", "#7dd3fc", "#f9a8d4"], list = [];
  for (let i = 0; i < n; i++) {
    const shape = pick !== "mix" ? pick : ["four", "five", "six", "eight", "four"][Math.floor(rnd() * 5)], rr = (3 + 5 * rnd()) * k * size * 2.2;
    const g = document.createElementNS(NS, "g");
    g.innerHTML = `<circle r="${f1(rr * 1.8)}" fill="${tints[i % tints.length]}" opacity="0.25"/><path d="${starPath(shape, rr)}" fill="${tints[(i * 3) % tints.length]}"/>`;
    sparkleLayer.append(g);
    const a = rnd() * Math.PI * 2, sp = (span * 1.6 + 120 * k) * (0.5 + rnd());
    list.push({ g, x: x + Math.cos(a) * span * 0.25 * rnd(), y: y + Math.sin(a) * span * 0.25 * rnd(), vx: Math.cos(a) * sp, vy: Math.sin(a) * sp - 60 * k, rot: rnd() * 90, spin: (rnd() - 0.5) * 720, life: 700 + 700 * rnd() });
  }
  const t0 = performance.now(); let last = t0;
  const step = t => {
    const dt = Math.min(0.05, (t - last) / 1000), e = t - t0; last = t; burstKeep();
    let alive = 0;
    for (const s of list) {
      if (!s.g.isConnected && e > 0) continue;
      const u = e / s.life;
      if (u >= 1) { s.g.remove(); continue; }
      alive++;
      const damp = Math.pow(0.2, dt); s.vx *= damp; s.vy = s.vy * damp + 40 * k * dt; s.x += s.vx * dt; s.y += s.vy * dt; s.rot += s.spin * dt;
      s.g.setAttribute("transform", `translate(${f1(s.x)} ${f1(s.y)}) rotate(${f1(s.rot)}) scale(${(1 - 0.5 * u).toFixed(2)})`);
      s.g.setAttribute("opacity", (bright * (u < 0.15 ? u / 0.15 : 1 - (u - 0.15) / 0.85)).toFixed(2));
    }
    if (alive) requestAnimationFrame(step);
  };
  requestAnimationFrame(step);
}
const BURST_N = 52, BURST_POP = [450, 2400], BURST_NEXT = 90, BURST_STUCK = 6000;
// (ticket 415, Matthew, for N: "turning each major part that we draw and control independently into bubbles, as they are
// hit -- first head, then torso, then legs and feet") The parts by their layers' data-part (veil, in 18-drawing.js), in the
// order a boulder meets them. A part gone to bubbles is hidden by the picture's data-burst (style.css), which outlasts a
// drawing; once all four have gone, the friend is hidden whole (figBurst), as before, with anything else they wear.
const BURST_PARTS = [["head", ["head", "eyes", "hair"]], ["torso", ["shirt", "hands"]], ["legs", ["pants"]], ["feet", ["feet"]]];
// (the burst under way: when it began, the friend's box then, the parts gone, their bubbles, the parts still to go in turn)
let bursting = null;
// (each drawing takes everything but the scene out of the picture: the burst's bubbles put back on top, so none blink out)
const burstKeep = () => { if (burstLayer && burstLayer.parentNode !== pic) pic.append(burstLayer);
  if (sparkleLayer) { if (sparkleLayer.childNodes.length) { if (sparkleLayer.parentNode !== pic) pic.append(sparkleLayer); } else if (sparkleLayer.parentNode) sparkleLayer.remove(); } };
// (an element's box in the picture's units, [x0, y0, x1, y1])
function picBox(el, inv) {
  const r = el.getBoundingClientRect();
  if (!r.width && !r.height) return null;
  const a = new DOMPoint(r.left, r.top).matrixTransform(inv), c = new DOMPoint(r.right, r.bottom).matrixTransform(inv);
  return [Math.min(a.x, c.x), Math.min(a.y, c.y), Math.max(a.x, c.x), Math.max(a.y, c.y)];
}
// (the boxes of the friend's layers of these parts; none for a part not drawn, as at 0 on its 👁️ slider)
function partBoxes(names) {
  const P = pic.getScreenCTM();
  if (!P) return [];
  const inv = P.inverse(), sel = names.map(p => `[data-part="${p}"]`).join(","), out = [];
  for (const n of figShown) {
    if (n.nodeType !== 1) continue;
    for (const el of [...(n.matches(sel) ? [n] : []), ...n.querySelectorAll(sel)]) { const b = picBox(el, inv); if (b) out.push(b); }
  }
  return out;
}
const unionBox = bs => bs.length ? [Math.min(...bs.map(b => b[0])), Math.min(...bs.map(b => b[1])), Math.max(...bs.map(b => b[2])), Math.max(...bs.map(b => b[3]))] : null;
const boxArea = b => b ? (b[2] - b[0]) * (b[3] - b[1]) : 0;
// (a boulder with this box, landed, is on the friend: their feet under it, anywhere but its very top and edges; each rock's
// shape is its own, and the friend may stand at the pit's back)
function underBoulder([x0, y0, x1, y1]) {
  const B = state.background, x = B.figX ?? 0, z = SCENE.ground + (B.figY ?? 0), bw = x1 - x0, bh = y1 - y0;
  return x > x0 + 0.08 * bw && x < x1 - 0.08 * bw && z > y0 + 0.1 * bh && z < y1 + 0.1 * bh;
}
// (ticket 415) Each frame of a boulder's fall, y the drop still to go in its parent's units. Where it will land on the
// friend, each part bursts as the boulder's lowest edge reaches the part's top: head first, each part after the one before.
function boulderFalling(g, y) {
  const P = pic.getScreenCTM();
  if (!P || figBurst || !g || !g.parentNode || !g.parentNode.getScreenCTM) return;
  const inv = P.inverse(), b = picBox(g, inv), fr = friendBox();
  if (!b || !fr) return;
  const dy = -y * inv.multiply(g.parentNode.getScreenCTM()).d;
  if (!underBoulder([b[0], b[1] + dy, b[2], b[3] + dy])) return;
  if (b[3] < fr[1] - 0.2 * (fr[3] - fr[1])) return;   // (still above the friend: nothing to measure yet)
  for (const [name, names] of BURST_PARTS) {
    if (bursting && bursting.gone.has(name)) continue;
    const pb = unionBox(partBoxes(names));
    if (pb && b[3] < pb[1]) return;
    burstPart(name);
  }
}
function boulderHits(g) {
  if (bursting) { friendBurst(); return; }   // (already coming down on them: the parts left, in turn)
  const P = pic.getScreenCTM();
  if (!P || figBurst || !g) return;
  const b = picBox(g, P.inverse());
  if (b && underBoulder(b)) friendBurst();
}
// (every part not gone yet, in turn, BURST_NEXT ms apart: a boulder landing where none was met on its way down)
function friendBurst() {
  const rest = BURST_PARTS.map(([n]) => n).filter(n => !(bursting && bursting.gone.has(n)));
  if (!rest.length || !burstPart(rest[0])) return;
  bursting.queue = rest.slice(1);
  bursting.next = performance.now() - bursting.t0 + BURST_NEXT;
}
// (one part to bubbles: hidden, and in its place bubbles of its shape, as many as its share of the friend; the first part
// starts the burst, and the controls rest while it plays: entering)
function burstPart(name) {
  if (!bursting) {
    const fr = friendBox();
    if (!fr) return false;
    const NS = "http://www.w3.org/2000/svg", layer = burstLayer = document.createElementNS(NS, "g");
    layer.id = "friend-burst"; layer.setAttribute("pointer-events", "none"); pic.append(layer);
    const areas = BURST_PARTS.map(([, names]) => boxArea(unionBox(partBoxes(names))));
    bursting = { t0: performance.now(), fr, gone: new Set(), bubs: [], queue: [], next: 0, hit: 0, last: null, layer, areas, total: areas.reduce((a, b) => a + b, 0) || 1 };
    entering = true; padWant = null;
    requestAnimationFrame(burstStep);
  }
  const k = BURST_PARTS.findIndex(([n]) => n === name);
  if (k < 0 || bursting.gone.has(name)) return true;
  const boxes = partBoxes(BURST_PARTS[k][1]);
  bursting.gone.add(name); bursting.hit = performance.now() - bursting.t0;
  pic.setAttribute("data-burst", [...bursting.gone].join(" "));
  partBubbles(boxes, Math.max(4, Math.min(30, Math.round(BURST_N * bursting.areas[k] / bursting.total))));
  if (bursting.gone.size === BURST_PARTS.length) { figBurst = true; draw(); }
  return true;
}
// (n rainbow bubbles in these layers' boxes, each layer by its size, coloured by height on the friend: red at the top to
// violet at the feet; they spill out from the part's middle)
function partBubbles(boxes, n) {
  if (!boxes.length) return;
  const { fr, layer } = bursting, NS = "http://www.w3.org/2000/svg", h = fr[3] - fr[1], rnd = Math.random, now = performance.now() - bursting.t0;
  const areas = boxes.map(boxArea), sum = areas.reduce((a, b) => a + b, 0), u = unionBox(boxes), cx = (u[0] + u[2]) / 2, cy = (u[1] + u[3]) / 2;
  for (let i = 0; i < n; i++) {
    let pick = rnd() * sum, j = 0;
    while (j < boxes.length - 1 && (pick -= areas[j]) > 0) j++;
    const bx = boxes[j]; let p, q;
    do { p = rnd() * 2 - 1; q = rnd() * 2 - 1; } while (p * p + q * q > 1);   // (in the oval that fills its box)
    const x = (bx[0] + bx[2]) / 2 + p * (bx[2] - bx[0]) / 2, y = (bx[1] + bx[3]) / 2 + q * (bx[3] - bx[1]) / 2;
    const v = Math.max(0, Math.min(1, (y - fr[1]) / h)), r = h * (0.035 + 0.045 * rnd()), hue = Math.round(v * 290 + 20 * (rnd() - 0.5)), col = `hsl(${hue}, 95%, 60%)`;
    const el = document.createElementNS(NS, "g"); el.setAttribute("transform", `translate(${f1(x)} ${f1(y)})`);
    // (a bubble full of its colour, vivid: a rainbow, not a few pale rings)
    el.innerHTML = `<g class="bubin"><circle r="${f1(r)}" fill="${col}" fill-opacity="0.38"/>${bubbleSvg(0, 0, r, { color: col, shine: 0.9 })}</g>`; layer.append(el);
    const dx = x - cx, dy = y - cy, d = Math.hypot(dx, dy) || 1, sp = h * (0.6 + 1.4 * rnd());
    bursting.bubs.push({ x, y, vx: dx / d * sp + (rnd() - 0.5) * h * 0.5, vy: dy / d * sp * 0.7 - h * 0.3 * rnd(), el, pop: now + BURST_POP[0] + (BURST_POP[1] - BURST_POP[0]) * rnd(), gone: false });
  }
}
function burstStep(t) {
  const B = bursting;
  if (!B) return;
  const e = t - B.t0, h = B.fr[3] - B.fr[1], dt = Math.min(0.05, (t - (B.last ?? t)) / 1000); B.last = t;
  if (B.layer.parentNode !== pic) pic.append(B.layer);   // (a drawing takes it out: back on top)
  if (B.queue.length && e >= B.next) { burstPart(B.queue.shift()); B.next = e + BURST_NEXT; }
  // (a fall cut short, its boulder never landing: the parts left go anyway, so the friend is never left half there)
  else if (!B.queue.length && B.gone.size < BURST_PARTS.length && e - B.hit > BURST_STUCK) friendBurst();
  for (const b of B.bubs) {
    if (b.gone) continue;
    if (e >= b.pop) { b.gone = true; if (b.el.firstChild) b.el.firstChild.classList.add("pop"); setTimeout(() => b.el.remove(), 260); continue; }
    const damp = Math.pow(0.35, dt); b.vx *= damp; b.vy = b.vy * damp - h * 0.25 * dt;   // (slowing, then floating up)
    b.x += b.vx * dt; b.y += b.vy * dt; b.el.setAttribute("transform", `translate(${f1(b.x)} ${f1(b.y)})`);
  }
  const end = B.gone.size === BURST_PARTS.length ? Math.max(0, ...B.bubs.map(b => b.pop)) + 300 : Infinity;
  if (e < end) { requestAnimationFrame(burstStep); return; }
  // (back, where they stood, out from under the boulder; fading in, as through a door)
  B.layer.remove(); burstLayer = null; bursting = null; figBurst = false; pic.removeAttribute("data-burst");
  solidUnstick(400); changed();
  const fade = o => { for (const n of figShown) if (n.style) n.style.opacity = o; };
  const t1 = performance.now(), f = tt => { const v = Math.min(1, (tt - t1) / WALK_IN_MS); fade(v < 1 ? v.toFixed(2) : ""); if (v < 1) requestAnimationFrame(f); else entering = false; };
  fade("0"); requestAnimationFrame(f);
}
function figHide(on) { figAway = on; for (const n of figShown) if (n.style) n.style.visibility = on ? "hidden" : ""; }
function figCopy() {
  const NS = "http://www.w3.org/2000/svg", g = document.createElementNS(NS, "g"), P = pic.getScreenCTM().inverse();
  g.setAttribute("pointer-events", "none"); g.setAttribute("class", "fig-spin");
  for (const n of figShown) {
    if (n.nodeType !== 1) continue;
    let html = n.outerHTML;
    for (const id of new Set([...html.matchAll(/\sid="([^"]+)"/g)].map(m => m[1])))
      html = html.split(`"${id}"`).join(`"${id}-spin"`).split(`#${id})`).join(`#${id}-spin)`).split(`"#${id}"`).join(`"#${id}-spin"`);
    const holder = document.createElementNS(NS, "g"), par = n.parentNode, m = par && par.getScreenCTM ? P.multiply(par.getScreenCTM()) : null;
    if (m) holder.setAttribute("transform", `matrix(${[m.a, m.b, m.c, m.d, m.e, m.f].map(v => +v.toFixed(4)).join(" ")})`);
    holder.innerHTML = html;
    for (const c of holder.children) { c.style.visibility = ""; c.style.opacity = ""; }
    g.append(holder);
  }
  pic.append(g);
  return g;
}
function flyInto(box, arrive) {
  if (!document.getElementById("figure")) return;
  entering = true;
  const inv = pic.getScreenCTM().inverse();
  const at = r => new DOMPoint(r.x + r.width / 2, r.y + r.height / 2).matrixTransform(inv);
  // (the friend's box: all their pieces)
  const rs = figShown.filter(n => n.getBoundingClientRect).map(n => n.getBoundingClientRect()).filter(r => r.width && r.height);
  if (!rs.length) { entering = false; arrive(); changed(); return; }
  const x0 = Math.min(...rs.map(r => r.left)), y0 = Math.min(...rs.map(r => r.top)), x1 = Math.max(...rs.map(r => r.right)), y1 = Math.max(...rs.map(r => r.bottom));
  const fr = { x: x0, y: y0, width: x1 - x0, height: y1 - y0 }, f0 = at(fr), d0 = at(box);
  const s = Math.min(box.width / fr.width, box.height / fr.height) * 0.75, dx = d0.x - f0.x, dy = d0.y - f0.y;
  const wrap = figCopy();
  figHide(true);
  wrap.style.transformBox = "fill-box";
  wrap.style.transformOrigin = "center";
  const step = (u, turn, k, o = 1) => ({ transform: `translate(${f1(dx * u)}px, ${f1(dy * u)}px) rotate(${turn}deg) scale(${+k.toFixed(3)})`, opacity: o });
  const fly = wrap.animate([step(0, 0, 1), step(0.2, 45, 1 - (1 - s) * 0.2), step(0.55, 240, 1 - (1 - s) * 0.6),
                            step(0.85, 720, 1 - (1 - s) * 0.9), step(1, 1260, s), step(1, 1620, s * 0.2, 0)],
                           { duration: 2000, easing: "ease-in", fill: "forwards" });
  fly.onfinish = () => {
    wrap.remove();
    tripPhase = "arriving";
    arrive();
    entering = false; tripNext("spinning out");
    changed();   // (drawn in the new place, still hidden)
    // landing: a quick spin out, slowing, to the friend's own size: the copy, then the friend
    const land = figCopy();
    land.style.transformBox = "fill-box";
    land.style.transformOrigin = "center bottom";
    land.animate([{ transform: "rotate(-360deg) scale(0.2)", opacity: 0 }, { transform: "rotate(0deg) scale(1)", opacity: 1 }],
                 { duration: 600, easing: "ease-out" }).onfinish = () => {
      land.remove(); figHide(false);
      if (tripPhase === "spinning out") tripPhase = "arrived";
    };
  };
}
// (ticket 138, as actions since ticket 141) A game controller, through the
// browser's Gamepad API. Played as a video game is, not as a puppet: the
// arrows (buttons 12 to 15 on a "standard" pad), or the left stick, walk the
// friend that way, from standing or sitting (a seat too), the diagonals as
// well, deeper into the place at 0.6 of the pace; they face the way they go
// (up: away from us, down: toward us; a diagonal keeps the one of its two
// ways they faced, else the side one: no diagonal postures yet, ticket 144).
// Held with an arrow, 🏃 run runs; 🧎 crouch crouches down, or crawls with
// an arrow. In water, walking is swimming and running is swimming fast. The
// two jumps go on top of whatever they are doing, and two buttons go places
// (back, the nearest doorway). Which button does what: state.controls (an
// action's button number; -1 none), set in the 🎮 Controls tab, where "🎯 set"
// learns the next button pressed (padLearn). The pad changes the animation
// only when what it is told changes (padWant), so it never undoes a chip;
// nor while they are up a place's ladder. Polled each frame while a pad is
// connected, never without one.
const PAD_ACTIONS = [["jumpShort", "🐸 short, fast jump"], ["jumpHigh", "🦘 high, slow jump"], ["run", "🏃 run (held, with an arrow)"],
                     ["crouch", "🧎 crouch down (held); with an arrow, crawl"], ["crawlToggle", "🐛 crawling on or off (press)"],
                     ["back", "⬅️ back the way they came"], ["door", "🚪 through the nearest passage"], ["enter", "✨ through the passage that glows"],
                     ["bubbles", "🫧 blow bubbles (held)"], ["fire", "🔥 breathe fire (held)"]];   // (tickets 221, 499)
// (ticket 146) teaching a pad its buttons, by their SNES names: pressed in
// this order. Then A is the short jump, B the high one, X into the picture
// that glows (ticket 145), Y crouch, R crawling on or off, select back,
// start the nearest doorway; run stays on the button it had (Matthew:
// "perfect"), taking it from any other that also wanted it.
const PAD_TEACH = [["B", "B, the bottom button"], ["A", "A, the right one"], ["X", "X, the top one"], ["Y", "Y, the left one"],
                   ["L", "L, the left shoulder"], ["R", "R, the right shoulder"], ["select", "select"], ["start", "start"]];
let padId = "", padTeach = null, padCrawl = false;
const padLayouts = () => { try { return JSON.parse(localStorage.getItem("dress-up-pads") || "{}") || {}; } catch (e) { return {}; } };
function padKeep() {
  if (!padId) return;
  try { const all = padLayouts(); all[padId] = Object.fromEntries(PAD_ACTIONS.map(([a]) => [a, state.controls[a] ?? -1])); localStorage.setItem("dress-up-pads", JSON.stringify(all)); } catch (e) { /* (kept for this visit only) */ }
}
function padTaught() {
  const G = padTeach.got, C = state.controls, run = C.run, at = k => (Number.isInteger(G[k]) ? G[k] : -1);
  Object.assign(C, { jumpShort: at("A"), jumpHigh: at("B"), enter: at("X"), crouch: at("Y"), crawlToggle: at("R"), back: at("select"), door: at("start") });
  for (const [a] of PAD_ACTIONS) if (a !== "run" && C[a] === run && run >= 0) C[a] = -1;
  padTeach = { done: true, got: G, line: `${padId || padName} (${padMapping === "standard" ? "standard numbers" : "its own numbers"}): ` +
    PAD_TEACH.map(([k]) => `${k}=${at(k)}`).join(", ") + `, run=${run}` };
  padKeep();
}
const PAD_PLACES = ["bottom", "right", "left", "top", "left shoulder", "right shoulder", "left trigger", "right trigger", "select", "start", "left stick", "right stick", "up", "down", "left", "right", "home"];
const padButtonName = i => !(i >= 0) ? "—" : padMapping === "standard" && PAD_PLACES[i] ? `${PAD_PLACES[i]} (button ${i})` : `button ${i}`;
const PAD_SPEED = { none: 160, walk: 90, run: 230, crawl: 55, swim: 70, swimfast: 150, crouch: 0 };
// (ticket 188, Matthew: "slider to control max running speed, and an
// acceleration/deceleration speed slider -- the current behavior, which can
// be the default, is immediate") body.runSpeed scales running and fast
// swimming (100: as before); body.accel, 0 to 100, is how long the friend
// takes to get up to full running speed, and to stop from it (0: at once,
// as before; 100: PAD_RAMP seconds), coasting to a stop, legs still going,
// after the arrows are let go. padVel, their speed now, in stage units a second.
const PAD_RAMP = 1.2;
const padTop = key => (PAD_SPEED[key] ?? PAD_SPEED.none) * (key === "run" || key === "swimfast" ? (state.body.runSpeed ?? 100) / 100 : 1);
let padVel = [0, 0];
let padLoop = 0, padName = "", padMapping = "", padDown = new Set(), padMoving = false, padLast = 0, padWant = "none", padLearn = null;
function padPoll(t) {
  padLoop = 0;
  // (ticket 150: a real one first; else the on-screen one, if it is shown)
  const real = [...(navigator.getGamepads ? navigator.getGamepads() : [])].find(Boolean);
  touchShow(!real && touchPad.want, !real);
  const g = real || (touchPad.want ? touchPad.pad : null);
  // (ticket 499: and what was held is let go: 🔥 or 🫧 held as the on-screen controller was hidden, or the game controller
  // unplugged, kept burning or blowing, this loop stopped and nothing else let it go)
  if (!g) { padName = ""; padDown = new Set(); padLast = 0; blowPad = false; firePad = false; if (padMoving) { padMoving = false; changed(); } return; }
  padName = (g.id || "").replace(/\s*\(.*$/, "").trim() || "a game controller";
  padMapping = g.mapping || "";
  // (and again when the settings are replaced: ↺ start over or 📂 load put
  // the usual buttons back, and the on-screen controller kept the standard
  // pad's layout, ✨ on L and crawling on R, instead of its own)
  if ((g.id || padName) !== padId || padFor !== state.controls) padUse(g.id || padName);
  if (g === touchPad.pad) touchLabels();
  const dt = padLast ? Math.min(0.1, (t - padLast) / 1000) : 0;
  padLast = t;
  const now = new Set(g.buttons.map((b, i) => (b.pressed ? i : -1)).filter(i => i >= 0));
  const arrows = padMapping === "standard" ? [12, 13, 14, 15] : [];
  const pressed = [...now].filter(i => !padDown.has(i) && !arrows.includes(i)), lit = now.size !== padDown.size || pressed.length > 0;
  padDown = now;
  const C = state.controls, B = state.body, BG = state.background;
  // learning a button: the next one pressed is the action's (and no other's)
  if (padLearn && pressed.length) {
    for (const [a] of PAD_ACTIONS) if (C[a] === pressed[0]) C[a] = -1;
    C[padLearn] = pressed[0]; padLearn = null; padKeep(); changed();
    padLoop = requestAnimationFrame(padPoll); return;
  }
  // (ticket 146) teaching the pad: each button pressed is the next one asked for
  if (padTeach && !padTeach.done && pressed.length) {
    padTeach.got[PAD_TEACH[padTeach.step][0]] = pressed[0];
    if (++padTeach.step >= PAD_TEACH.length) padTaught();
    changed();
    padLoop = requestAnimationFrame(padPoll); return;
  }
  const btn = a => Number.isInteger(C[a]) && C[a] >= 0 ? C[a] : -2, held = a => now.has(btn(a)), hit = a => pressed.includes(btn(a));
  if (hit("back")) { if (!backBtn.hidden) backBtn.click(); }
  if (hit("door")) padDoor();
  if (hit("crawlToggle")) { padCrawl = !padCrawl; padWant = null; }   // (ticket 146: on or off; arrows then crawl)
  // the arrows, or the left stick past its dead middle (at most one step a frame's worth, diagonals too)
  const stick = v => (Math.abs(v ?? 0) > 0.25 ? v : 0);
  let vx = clamp((now.has(15) ? 1 : 0) - (now.has(14) ? 1 : 0) + stick(g.axes[0]), -1, 1), vy = clamp((now.has(13) ? 1 : 0) - (now.has(12) ? 1 : 0) + stick(g.axes[1]), -1, 1);
  const len = Math.hypot(vx, vy);
  if (len > 1) { vx /= len; vy /= len; }
  const onLadder = B.anim === "climbup" && climb && !climb.none;
  // (ticket 245) on a rope bridge, ⬆ and ⬇ only turn them, away from us and toward us: they stay where they are
  const onBr = !onLadder && !drag && !entering ? bridgeNow() : null;
  if (onBr && Math.abs(vy) > 0.5 && Math.abs(vx) < 0.75 * Math.abs(vy)) {
    const f = vy < 0 ? "back" : "front";
    if (B.facing !== f || B.turn) { B.facing = f; B.turn = ""; changed(); }
    vx = 0; vy = 0;
  }
  // (ticket 145) a ladder by hand: at its foot, up gets on it; on it, up and
  // down climb (a chip's climb too, taken over), and down at its foot, or an
  // arrow to the side there, steps off
  const ud = Math.abs(vy) > 0.5 && Math.abs(vx) < 0.75 * Math.abs(vy) ? Math.sign(vy) : 0;
  // (ticket 216, Matthew: "we trigger going 'down' the ladder by pressing up, and animating an upward climb;
  // that's backwards") A hatch's ladder (a tree house's trapdoor: down) is taken with ⬇, standing at the hatch,
  // and goes down it as a tap or X does (walkInto: the climb down, cut off below the hatch's near edge, as the
  // trap room's pits cut them off); ⬆ mounts only a ladder that goes up.
  if (!onLadder && !onBr && ud && BG.kind !== "none" && !entering && !drag && !isJump(B.anim)) {
    const L = ladderNear(), fx = BG.figX ?? 0, fz = SCENE.ground + (BG.figY ?? 0), k = figureT.k;
    const atHatch = L && L.down != null && L.door && Math.abs(L.x - fx) < Math.max(20, 70 * k) && Math.abs(L.z - fz) < Math.max(14, 50 * k);
    if (ud > 0 && atHatch && L.el) { climbLadder(L.el); padMoving = false; padWant = null; changed(); padLoop = requestAnimationFrame(padPoll); return; }
    if (ud < 0 && L && L.down == null) { mountLadder(L); padMoving = false; padWant = null; changed(); padLoop = requestAnimationFrame(padPoll); return; }
  }
  if (onLadder && ud && !climb.manual) Object.assign(climb, { manual: true, got: climb.at ?? 0 });
  if (onLadder && climb.manual && !entering && !climb.down) {
    if (climb.got <= 0 && (ud > 0 || (!ud && Math.abs(vx) > 0.5))) { B.anim = "none"; climb = null; padWant = null; changed(); }
    else if (ud) { climb.got = clamp(climb.got - ud * CLIMB_HAND * (held("run") ? 1.8 : 1) * figureT.k * dt, 0, climb.all ?? Infinity); moved(); }
  }
  if ((vx || vy) && clickRun) clickRun = null;   // (ticket 266: the controller takes over from a queue of clicks, after the step under way)
  const moving = !!(vx || vy) && BG.kind !== "none" && !entering && !drag && !onLadder;
  // (ticket 188: let go, still coasting to a stop, when speeding up and slowing down take time)
  const ramp = (state.body.accel ?? 0) / 100 * PAD_RAMP;
  const coasting = !moving && ramp > 0 && Math.hypot(padVel[0], padVel[1]) > 2 && BG.kind !== "none" && !entering && !drag && !onLadder;
  if (!moving && !coasting) padVel = [0, 0];
  const wet = BG.kind !== "none" && !SHALLOW_WATER[BG.kind] && inWater(BG, BG.figX ?? 0, SCENE.ground + (BG.figY ?? 0));
  const run = held("run"), low = held("crouch") || padCrawl;
  const want = moving ? (low ? (wet ? "swim" : "crawl") : wet ? (run ? "swimfast" : "swim") : run ? "run" : "walk") : coasting && padWant ? padWant : low && !wet ? "crouch" : "none";
  if (isJump(B.anim)) jumpPrev = want;   // (landing into what they are told now)
  else if (want !== padWant && !onLadder) {
    padWant = want;
    if (want !== "none" && BG.seat) standUp();
    // (as its chip would, but keeping the way they face: they turn as they go, below)
    if (B.anim !== want) {
      if (want !== "none" && want !== "swim" && want !== "swimfast" && B.pose !== "stand") strikePose("stand");
      B.anim = want; climb = null;
      changed();
    }
  }
  if (hit("jumpShort") && !onLadder) { startJump(B.anim, "jump"); changed(); }
  if (hit("jumpHigh") && !onLadder) { startJump(B.anim, "jumphigh"); changed(); }
  if (hit("enter")) enterPicture();   // (ticket 145)
  blowPad = held("bubbles");   // (ticket 221: bubbles while it is held)
  if (blowPad) blowGo();
  firePad = held("fire");   // (ticket 499: fire while it is held)
  if (firePad) fireGo();
  if (moving || coasting) {
    // (ticket 188: the speed they are told, reached at once, or over the ramp's time: a step toward it each frame)
    const top = padTop(isJump(B.anim) ? jumpPrev : B.anim), want = moving ? [vx * top, vy * top] : [0, 0];
    if (!ramp) padVel = want;
    else { const step = padTop("run") / ramp * dt, ex = want[0] - padVel[0], ey = want[1] - padVel[1], e = Math.hypot(ex, ey);
           padVel = e <= step ? want : [padVel[0] + ex / e * step, padVel[1] + ey / e * step]; }
    const F = sceneFrame;
    // (ticket 245) on a rope bridge: along it, as far as its ends, facing the way they go
    if (onBr) {
      bridgeStep(padVel[0] * dt * BRIDGE_PACE);
      if (moving) { B.facing = vx < 0 ? "left" : "right"; B.turn = ""; }
      padMoving = true; moved();
      // (ticket 246) at an end, pushing on toward it: into the house there, through its door to this bridge
      const at = bridgeNow(), be = at && moving && Math.abs(vx) > 0.5 ? bridgeEnd(at) : null;
      if (be && Math.abs(at.u - be.end) < 1e-4) { padMoving = false; bridgeIn(be); }
      padLoop = requestAnimationFrame(padPoll); return;
    }
    // (ticket 149: walls and solids stop them, and they slide along)
    const wantX = Math.round(10 * clamp((BG.figX ?? 0) + padVel[0] * dt, F.left, F.right)) / 10;
    const [mx, my] = solidMove(BG.figX ?? 0, BG.figY ?? 0, wantX, Math.round(10 * clamp((BG.figY ?? 0) + padVel[1] * dt * 0.6, figYMin(F), F.bottom - F.ground)) / 10);
    // (ticket 256: stopped by a side wall where the door in it stands open: through it)
    if (moving && !entering && !isJump(B.anim) && Math.abs(mx - wantX) > 0.05 && solidHit.name === "wall" && performance.now() - solidHit.t < 250 && doorPushed(vx)) {
      padMoving = false; padWant = null; changed(); padLoop = requestAnimationFrame(padPoll); return;
    }
    if (mx === (BG.figX ?? 0) && my === (BG.figY ?? 0) && !moving) padVel = [0, 0];   // (stopped by a wall while coasting)
    BG.figX = mx; BG.figY = my;
    // (ticket 214, Matthew: "all collision directions that correspond to: edge of the N 'back wall' (or horizon); E, S
    // or W edges of the screen; Or their diagonals, to trigger the enter passage action") pushing on into an edge they
    // stand at, or a corner, takes the way out that way, if there is one: the corner's own, else either side's
    if (moving && !entering && !isJump(B.anim)) {
      const yMin = figYMin(F), yMax = F.bottom - F.ground;
      const atN = my <= yMin + 0.5 && vy < -0.3, atS = my >= yMax - 0.5 && vy > 0.3, atW = mx <= F.left + 0.5 && vx < -0.3, atE = mx >= F.right - 0.5 && vx > 0.3;
      if ((atN || atS || atW || atE) && edgeGo(edgeWays(atN, atS, atW, atE, (mx - F.left) / Math.max(1, F.right - F.left), (my - yMin) / Math.max(1, yMax - yMin)))) {
        padMoving = false; padWant = null; changed(); padLoop = requestAnimationFrame(padPoll); return;
      }
    }
    // (ticket 162: walking onto a pit's hole, not jumping, falls in)
    if (!isJump(B.anim) && !entering) { const pit = pitAt(mx, SCENE.ground + my); if (pit) enterScene(pit); }   // (the poll goes on: it must schedule itself again)
    // (which way they face: a diagonal keeps the one of its two ways they faced, else the side one; coasting, as they were)
    const ax = Math.abs(vx), ay = Math.abs(vy), m = Math.max(ax, ay) || 1;
    const side = ax > 0.38 * m ? (vx < 0 ? "left" : "right") : null, depth = ay > 0.38 * m ? (vy < 0 ? "back" : "front") : null;
    // (ticket 144) a diagonal toward us: facing us, turned partway the side's way; away from us, as before
    if (!moving) { /* (coasting: facing as they were) */ }
    else if (side && depth === "front") { B.facing = "front"; B.turn = side; }
    else { B.facing = !side ? depth : !depth ? side : B.facing === side || B.facing === depth ? B.facing : side; B.turn = ""; }
    padMoving = true;
    moved();   // (ticket 163)
  } else if (padMoving) {
    // (stopped: on a seat if they were put down by one; up a ladder within reach, climbing)
    padMoving = false;
    dropOnSeat();
    // (ticket 248: standing, their feet planted apart as a step's are not: a foot that reached into a thing as they
    // walked is in it now; out of it, back the way they came)
    if (!state.body.sit && !BG.onBridge && BG.kind !== "none") solidUnstick(Math.max(8, feetSpan()[0]));
    if (B.anim === "climbup") { climb = null; climbAnywhere = false; }
    changed();
  } else if (lit) changed();   // (the Controls tab shows which buttons are down)
  padLoop = requestAnimationFrame(padPoll);
}
// (ticket 214) the way out at an edge: a world square's way (its nav arrow's doorway) that way, or at the bottom a ▼
// (Matthew: "the animations on each edge look strange, we are turning and facing away from the edge we are initially
// going into") taken from where they stand, on through the edge, facing it: not walked back to the doorway's spot first
// (Matthew: "if I walk into the corner that I'm facing (NW), I get a path plan to walk N, not NW"; "check each corner
// passage can be triggered by colliding into that corner, up to some limit like 1/4 of the screen width away from the
// corner") The ways to try, in order, pushing into an edge at u across the picture (0 its left side, 1 its right) and v
// down its floor (0 the back limit, 1 the bottom): in a corner, its way, then either edge's; within EDGE_CORNER of a
// corner along an edge (a quarter of the picture's width along the back limit or the bottom, of the floor's depth
// along a side), the corner's way, then the edge's own; else the edge's own only.
const EDGE_CORNER = 0.25;
function edgeWays(atN, atS, atW, atE, u, v) {
  const ns = atN ? "N" : atS ? "S" : "", we = atW ? "W" : atE ? "E" : "";
  if (ns && we) return [ns + we, ns, we];
  if (ns) { const c = u < EDGE_CORNER ? "W" : u > 1 - EDGE_CORNER ? "E" : ""; return c ? [ns + c, ns] : [ns]; }
  const c = v < EDGE_CORNER ? "N" : v > 1 - EDGE_CORNER ? "S" : "";
  return c ? [c + we, we] : [we];
}
function edgeGo(ways) {
  const node = bgShown.node;
  if (!node) return false;
  for (const c of ways) {
    const door = node.querySelector(`.portal[data-way="${c}"]`);   // (a world square's way, or a room's toward us: ticket 233)
    if (door) { enterScene(door, true); return true; }
  }
  return false;
}
// (the passage nearest the friend, however far: walked to and gone through; ticket 187: every kind, measured as X's)
function padDoor() {
  if (entering || state.background.kind === "none") return;
  const all = passesNear(Infinity);
  if (all.length) { enterAt = all[0].p; enterPicture(); }
}
window.addEventListener("gamepadconnected", e => {
  padName = ((e.gamepad && e.gamepad.id) || "").replace(/\s*\(.*$/, "").trim() || "a game controller";
  padMapping = (e.gamepad && e.gamepad.mapping) || "";
  // (ticket 146: a pad taught its buttons gets its own layout back)
  padUse((e.gamepad && e.gamepad.id) || padName);
  if (!padLoop) padLoop = requestAnimationFrame(padPoll);
  changed();
});
window.addEventListener("gamepaddisconnected", () => { if (!padLoop) padLoop = requestAnimationFrame(padPoll); });
// (a pad's own layout, when the pad in use changes: the one kept by its id;
// else the on-screen controller's own to start with; else, coming from the
// on-screen one, the usual buttons)
let padFor = null;   // (the settings padUse last put a layout into)
function padUse(id) {
  const kept = padLayouts()[id], C = state.controls, like = L => Object.keys(L).every(a => C[a] === L[a]);
  const fresh = padFor !== C;
  padFor = C;
  if (kept) Object.assign(C, kept);
  else if (id === TOUCH_ID && (id !== padId || fresh)) Object.assign(C, TOUCH_CONTROLS);
  else if (id !== TOUCH_ID && (padId === TOUCH_ID || like(TOUCH_CONTROLS))) Object.assign(C, DEFAULTS.controls);
  // (an action newer than the layout kept, as ticket 145's ✨: on its usual
  // button, unless another action has that one)
  const usual = id === TOUCH_ID ? TOUCH_CONTROLS : DEFAULTS.controls;
  if (kept) for (const [a] of PAD_ACTIONS) if (!(a in kept)) C[a] = PAD_ACTIONS.some(([o]) => o !== a && C[o] === usual[a]) ? -1 : usual[a];
  padId = id;
}
// (ticket 150) The on-screen controller, for a tablet or a phone with no
// game controller (an iPad has no USB port for ours), or any screen without
// one: a SNES controller's layout in the screen's bottom corners, to touch
// or click (touchpad, built the first time it is shown). It is a pad like a
// real one (touchPad.pad, with the standard numbers), read by padPoll every
// frame while it is shown, so every action works as with a real controller
// and is set in the same 🎮 Controls tab. Its own layout is kept by its id
// (TOUCH_ID), as a real pad's is, and starts as TOUCH_CONTROLS: Matthew's
// taught one (A the short jump, B the high one, X into the picture that glows, Y crouch, select
// back, start the nearest doorway), with run on R and crawling on or off on
// L, so the right thumb holds run while the left one steers. Offered (🎮, at
// the picture's bottom right) while no game controller is connected, and
// hidden while one is. Whether it is shown is this browser's own
// (localStorage "dress-up-touchpad"), not the friend's.
const TOUCH_ID = "on-screen controller";
// (ticket 499: 🫧 and 🔥 on their own buttons here too, so a layout kept from before an action came in gets it on its button)
const TOUCH_CONTROLS = { jumpShort: 1, jumpHigh: 0, run: 5, crouch: 2, crawlToggle: 4, back: 8, door: 9, enter: 3, bubbles: 6, fire: 7 };
const touchPad = { want: false, shown: false, el: null, keys: "",
  pad: { id: TOUCH_ID, index: -1, connected: true, mapping: "standard", axes: [0, 0, 0, 0], timestamp: 0,
         buttons: Array.from({ length: 17 }, () => ({ pressed: false, value: 0, at: 0, tok: 0 })) } };
try { touchPad.want = localStorage.getItem("dress-up-touchpad") === "1"; } catch (e) { /* (not shown to start) */ }
const padBtn = document.getElementById("pad-btn");
padBtn.addEventListener("click", () => touchWant(!touchPad.want));
function touchWant(on) {
  touchPad.want = on;
  try { localStorage.setItem("dress-up-touchpad", on ? "1" : "0"); } catch (e) { /* (for this visit only) */ }
  if (!padLoop) padLoop = requestAnimationFrame(padPoll);
  changed();
}
// (shown or not; and the 🎮 that offers it, only while no game controller is connected)
function touchShow(on, offer) {
  if (padBtn.hidden !== !offer) padBtn.hidden = !offer;   // (each frame: the page is changed only when this changes)
  if (on === touchPad.shown) return;
  padBtn.setAttribute("aria-pressed", on);
  padBtn.title = on ? "Hide the controller on the screen" : "A controller on the screen: arrows and buttons to touch or click";
  touchPad.shown = on;
  if (!touchPad.el) touchBuild();
  touchPad.el.hidden = !on;
  if (!on) for (const b of touchPad.pad.buttons) { b.pressed = false; b.value = 0; b.tok++; }
  if (!on) for (const x of touchPad.el.querySelectorAll(".down")) x.classList.remove("down");
}
// A press holds its button down until the finger (or the mouse) lets go,
// for at least 70 ms, so a tap shorter than a frame is still seen by padPoll
function touchSet(i, on, x) {
  const b = touchPad.pad.buttons[i], tok = ++b.tok;
  if (on) { b.pressed = true; b.value = 1; b.at = performance.now(); if (x) x.classList.add("down"); return; }
  if (!b.pressed) return;
  setTimeout(() => { if (b.tok !== tok) return; b.pressed = false; b.value = 0; if (x) x.classList.remove("down"); }, Math.max(0, b.at + 70 - performance.now()));
}
function touchBuild() {
  const hold = (x, i) => {
    x.addEventListener("pointerdown", e => { e.preventDefault(); try { x.setPointerCapture(e.pointerId); } catch (_) { /* (a synthetic press) */ } touchSet(i, true, x); });
    for (const k of ["pointerup", "pointercancel", "lostpointercapture"]) x.addEventListener(k, () => touchSet(i, false, x));
    x.addEventListener("contextmenu", e => e.preventDefault());
    return x;
  };
  const btn = (cls, i, name) => { const x = hold(el("button", { type: "button", class: cls, "aria-label": name }, [name, el("small")]), i); x.dataset.b = i; return x; };
  // the arrows: where a finger is on the cross, from its middle, is the
  // direction (a diagonal between two arms), and it slides from one to the next
  const arm = (cls, sign) => el("div", { class: "arm " + cls }, sign);
  const arms = { 12: arm("u", "▲"), 13: arm("d", "▼"), 14: arm("l", "◀"), 15: arm("r", "▶") };
  const dpad = el("div", { class: "dpad pad", role: "group", "aria-label": "arrows" }, [el("div", { class: "hub" }), ...Object.values(arms)]);
  let held = null;
  const aim = e => {
    const r = dpad.getBoundingClientRect(), dx = (e.clientX - r.left) / r.width * 2 - 1, dy = (e.clientY - r.top) / r.height * 2 - 1;
    const far = Math.hypot(dx, dy) > 0.18, a = Math.atan2(dy, dx), c = Math.cos(a), s = Math.sin(a);
    const on = { 12: far && s < -0.38, 13: far && s > 0.38, 14: far && c < -0.38, 15: far && c > 0.38 };
    for (const i in on) if (on[i] !== touchPad.pad.buttons[i].pressed || on[i]) touchSet(+i, on[i], arms[i]);
  };
  dpad.addEventListener("pointerdown", e => { e.preventDefault(); held = e.pointerId; try { dpad.setPointerCapture(e.pointerId); } catch (_) { /* (a synthetic press) */ } aim(e); });
  dpad.addEventListener("pointermove", e => { if (e.pointerId === held) aim(e); });
  for (const k of ["pointerup", "pointercancel", "lostpointercapture"])
    dpad.addEventListener(k, e => { if (e.pointerId !== held) return; held = null; for (const i in arms) touchSet(+i, false, arms[i]); });
  dpad.addEventListener("contextmenu", e => e.preventDefault());
  const abxy = el("div", { class: "abxy pad", role: "group", "aria-label": "buttons" }, [btn("bx", 3, "X"), btn("by", 2, "Y"), btn("ba", 1, "A"), btn("bb", 0, "B")]);
  touchPad.el = el("div", { class: "touchpad", id: "touchpad", hidden: "" }, [
    // (ticket 221: 🫧 blow bubbles, under select; ticket 499: 🔥 breathe fire, under start, each a column of its own, so none covers another)
    el("div", { class: "half l" }, [btn("sh", 4, "L"), dpad, el("div", { class: "sms" }, [btn("sm", 8, "select"), btn("sm", 6, "🫧")])]),
    el("div", { class: "half r" }, [btn("sh", 5, "R"), el("div", { class: "sms" }, [btn("sm", 9, "start"), btn("sm", 7, "🔥")]), abxy])]);
  document.body.append(touchPad.el);
  touchPad.keys = "";
  touchLabels();
}
// (each button shows what it does: its action's emoji, from the 🎮 Controls tab's layout)
function touchLabels() {
  const C = state.controls, key = JSON.stringify(C);
  if (!touchPad.el || key === touchPad.keys) return;
  touchPad.keys = key;
  for (const x of touchPad.el.querySelectorAll("button[data-b]")) {
    const a = PAD_ACTIONS.find(([k]) => C[k] === +x.dataset.b);
    x.lastChild.textContent = a ? a[1].split(" ")[0] : "";
    x.title = `${x.firstChild.textContent}: ${a ? a[1] : "nothing yet (set one in the 🎮 Controls tab)"}`;
    x.classList.toggle("idle", !a);
  }
}
if (touchPad.want) padLoop = requestAnimationFrame(padPoll);
// Where a friend lands, arriving in a place, if not in its middle: on the
// ground, by the camp's fire and not in it, on the bank of the falls' pool
// and not in the water, and to one side of a doorway at the far end (a
// tent's door, the cave's mouth) rather than in front of it ([x, y] from
// the middle of the stage's ground).
const ARRIVE = { hut: [-120, 50], hutroom: [0, 40], hutbed: [130, 40], hutpotion: [-60, 50], hutcenter: [-260, 60], witchroom: [-170, 0], crossing: [0, -20], imagination: [0, 70], emptyroom: [0, 70], bubblespace: [0, 70], bubbleroom: [0, 40], healerroom: [60, 40], fairyforest: [50, 40], campsite: [110, -10], waterfall: [-130, 40], tent: [-95, 25], cave: [-150, 25], caveroom: [-120, 40], farmyard: [0, 30], desert: [-40, 30], castle: [-210, 30], castleroom: [0, 40],
  // (ticket 516: the round room at the top of the tree: at the front, on the side away from its hatch (the house below's side), under a window)
  get treetop() { return [-topSide(state.background) * 170, 50]; } };
// Where each trip through a doorway began, the last on top: the back chip
// goes back through them, one at a time, while the friend is in the scene
// the last trip went to.
const trips = [];
function enterScene(door, here = false) {
  // (ticket 185: a way out of a world square, to the one beside it; ticket 233: a room's way toward us has its own data-to)
  // (ticket 525: a cave door linked to a square or a place off the grid: there, out by its twin; unlinked, by kind, below)
  if (door.dataset.passage) { const t = passageOf(door.dataset.passage); if (t && t.dest && passageWalk(door, t)) return; }
  if (door.dataset.way && !door.dataset.to) { worldWalk(door, here); return; }
  let to = door.dataset.to, room = door.dataset.room;
  const house = door.dataset.house, tent = door.dataset.tent, B = state.background;
  if (!SCENES[to] && !PICTURE_CHOICES.some(([kd]) => kd === to)) return;
  // (a picture's own box: the scene inside it spills past it, hidden)
  const view = door.querySelector(":scope > svg, :scope > image");
  const from = { kind: B.kind, inside: B.inside, tent: B.tent, castleRoom: B.castleRoom, figX: B.figX ?? 0, figY: B.figY ?? 0, to };
  // (ticket 264, Matthew: "when I come out of the treehouse from a ladder, I should respawn at the base of the ladder,
  // facing the viewer/player") down a tree room's ladder to the ground: outside at that house's ladder's foot (treeLadder's
  // place, through the tree's size), a step in front of it; facing us, as every arrival
  const downTo = B.kind === "treeroom" && to === "treehouse" && door.querySelector("[data-down]") ? B.inside : null;
  const arrive = () => {
    trips.push(from);
    const [figX, figY] = ARRIVE[to] || [0, 0];
    Object.assign(B, { kind: to, figX, figY }); arriveSettle = true;
    if (house) B.inside = +house;
    if (tent) B.tent = +tent;
    if (room) B.castleRoom = +room;
    if (to === "castleroom") trapReshuffle(B);   // (ticket 305)
    // (ticket 516) up the ladder into the round room at the top of the tree: a step from its hatch, toward the middle; down
    // its hatch into the house below: at the foot of that room's ladder up, a step toward the middle (not its middle, [0, 0],
    // which in some rooms is the trunk)
    if (to === "treetop" && from.kind === "treeroom") { const c = stageC(), { sd, hx } = topHatchAt(c, B); B.figX = Math.round(hx - sd * 110 * c.k); B.figY = Math.round(30 * c.k); }
    if (to === "treeroom" && from.kind === "treetop") { const c = stageC(), lx = (c.x1 - c.W * 0.4) * TREE_FIT; B.figX = Math.round(lx - 60 * c.k); B.figY = Math.round(-50 * c.k); }
    if (downTo) { const i = downTo - 1, [x] = TREE_SLOTS[i], hz = (B.treeHouseSize ?? 100) / 100, s = (B.treeSize ?? 100) / 100 * (SCENE.bottom - SCENE.top) / 700;
      B.figX = Math.round(clamp((x + Math.sign(x) * 52 * hz) * s, sceneFrame.left + 20, sceneFrame.right - 20)); B.figY = Math.round(8 * s); }
  };
  // (ticket 147, Matthew: the spin only for pictures and windows, "it's the most magical looking";
  // through any other way the friend walks, or, at a ladder's top, steps)
  // (ticket 173: a doorway reached by a ladder, a tree's house or a ladder's way up, is climbed to, unless the friend is through it already)
  // (ticket 246, Matthew: "a transition on bridges where a door passageway that leads to a bridge transitions outside of
  // the treehouse, and lets the game player walk along that curved route, to the other side") a tree room's door to a
  // rope bridge leads out onto the bridge, outside, at this house's end; and on a bridge, its houses are gone into at its
  // ends, walked to along it
  if (B.kind === "treeroom" && to === "treeroom" && door.dataset.door !== undefined && house) {
    const here = B.inside, there = +house;
    tripStart(door, "walk");
    walkInto(door, () => {
      trips.push(from);
      B.kind = "treehouse";
      const W = treeWalks(B).find(w => (w.a === here - 1 && w.b === there - 1) || (w.a === there - 1 && w.b === here - 1));
      if (W) bridgeOn(W.j, W.a === here - 1 ? 0 : 1);
      else { const [fx, fy] = ARRIVE.treehouse || [0, 0]; Object.assign(B, { figX: fx, figY: fy }); }
    }, !(climb && climb.through), "step", null, { doorView: true });
    return;
  }
  const onBr = bridgeNow();
  if (onBr && to === "treeroom" && house && [onBr.W.a, onBr.W.b].includes(+house - 1)) { bridgeGoIn({ j: onBr.j, end: onBr.W.b === +house - 1 ? 1 : 0, house: +house, other: (onBr.W.b === +house - 1 ? onBr.W.a : onBr.W.b) + 1 }); return; }
  const lad = !view && !(climb && climb.through) ? ladderOf(door) : null;
  if (lad) { climbLadder(lad); return; }
  // (ticket 531, phase 2: 📜 the Castle rule says where a castle pit leads, when it is on; false: nowhere)
  const go = scriptGo(door);
  if (go === false) return;
  if (go) { to = go.to; room = go.room; from.to = to; }
  tripStart(door, view ? "spin" : door.dataset.pit ? "fall" : "walk");   // (⏱️)
  // (ticket 199) a picture out of reach: walked (or swum) to first, under it, then flown into
  const far = view && !passesNear().some(q => q.p.el === door), pp = far && marks.pics.find(q => q.el === door);
  if (pp) walkTo((pp.x0 + pp.x1) / 2, clamp(pp.y1, SCENE.ground + figYMin(sceneFrame), sceneFrame.bottom), () => flyInto(svgBox(view), arrive));
  else if (view) flyInto(svgBox(view), arrive);
  else if (door.dataset.pit) walkInto(door, () => { arrive(); landFall = true; }, true, "fall");   // (ticket 162)
  else walkInto(door, arrive, !(climb && climb.through), "step", null, WORLD_DIRS[door.dataset.way] ? { out: WORLD_DIRS[door.dataset.way], here } : {});   // (ticket 525: never a cave door's "p-" way)
}
// (ticket 246) along the rope bridge they are on, to its end (0 or 1), walking (3 s at most), then on
function bridgeWalkTo(end, then) {
  const at = bridgeNow();
  if (!at) return;
  const B = state.body, { W } = at, dir = (end ? 1 : -1) * (W.q[0] >= W.p[0] ? 1 : -1);
  let len = 0;
  for (let i = 0, prev = walkAt(W, at.u); i < 20; i++) { const q = walkAt(W, at.u + (end - at.u) * (i + 1) / 20); len += Math.hypot(q[0] - prev[0], q[1] - prev[1]); prev = q; }
  const speed = Math.max(PAD_SPEED.walk * BRIDGE_PACE, len / 3);
  entering = true; padWant = null;
  if (state.background.seat) standUp();
  if (B.pose !== "stand") strikePose("stand");
  let last = 0;
  const step = t => {
    const dt = last ? Math.min(0.1, (t - last) / 1000) : 0; last = t;
    const a = bridgeNow();
    if (!a) { B.anim = "none"; entering = false; changed(); return; }
    if (Math.abs(a.u - end) < 1e-4) { B.anim = "none"; entering = false; then(); return; }
    bridgeStep(dir * speed * dt);
    B.anim = "walk"; B.facing = dir < 0 ? "left" : "right"; B.turn = "";
    moved(); requestAnimationFrame(step);
  };
  requestAnimationFrame(step);
}
// (ticket 246) at a bridge's end, into the house there: its room, just inside its door to the bridge they came along,
// facing us, fading in, as through any door
// (ticket 331, Matthew: "when we walk across the bridge by click, we animate the walk, but then we do not animate going into
// the door --- we do it pretty well for the ladder climbs outside of the tree house, so can't we reuse that again?") off the
// bridge's end onto the house's platform, along it to the door and back through it, seen only inside the doorway, as from
// the ladder's top (platformIn)
function bridgeIn(be) {
  const at = bridgeNow();
  platformIn(be.house, { x: at ? at.x / at.W.s : undefined, then: () => {
    // (a step into the room from the door's foot, toward its middle)
    const B = state.background, d = bgShown.node && bgShown.node.querySelector(`.portal[data-door][data-house="${be.other}"]`), P = pic.getScreenCTM(), F = sceneFrame;
    if (!d || !P) return;
    const r = (d.querySelector("path, rect") || d).getBoundingClientRect(), foot = new DOMPoint(r.x + r.width / 2, r.y + r.height).matrixTransform(P.inverse());
    B.figX = Math.round(clamp(foot.x * 0.8, F.left, F.right)); B.figY = Math.round(clamp(foot.y - SCENE.ground, figYMin(F), F.bottom - SCENE.ground));
    changed();
  } });
}
// (ticket 246) along the bridge to the end they would go in at (bridgeEnd), and into the house there
function bridgeGoIn(be = bridgeEnd()) {
  if (!be || entering) return;
  bridgeWalkTo(be.end, () => bridgeIn(be));
}
// (ticket 199) to a spot on the floor, walking (swimming in deep water) as
// walkInto does, a long way faster, 2.5 s at most; then on
// (ticket 294: a walk stopped, a new one in its place, is stopped by walkStop going on: its steps end where they are)
// (ticket 321, Matthew: "there's a bouncing and flashing visually (on the path itself only) when we do multi-segment paths
// ... each time we reach a new segment, there's a visual interruption": each leg ended the walk, standing still and not
// walking for a drawing, so 👣's path was taken down and put up again and the friend stood a moment) more: another leg
// follows, so this one hands straight on to it, still walking
let walkStop = 0;
function walkTo(x, y, then, more = false) {
  bridgeOff();   // (ticket 245: from a rope bridge, down at its foot first)
  const BG = state.background, B = state.body, F = sceneFrame;
  const tx = clamp(x, F.left, F.right), ty = clamp(y - SCENE.ground, figYMin(F), F.bottom - F.ground);
  entering = true;
  if (BG.seat) standUp();
  if (B.pose !== "stand") strikePose("stand");
  const speed = Math.max(PAD_SPEED.walk, Math.hypot(tx - (BG.figX ?? 0), (ty - (BG.figY ?? 0)) / 0.6) / 2.5), t0 = performance.now();
  let last = 0;
  const stop = walkStop;
  const step = t => {
    if (walkStop !== stop) return;   // (ticket 294: stopped, a new walk in its place)
    const dt = last ? Math.min(0.1, (t - last) / 1000) : 0; last = t;
    const dx = tx - (BG.figX ?? 0), dy = ty - (BG.figY ?? 0), d = Math.hypot(dx, dy / 0.6), sp = speed * dt;
    if (d <= sp + 0.5 || t - t0 > WALK_IN_MAX) { BG.figX = tx; BG.figY = ty; if (more) { then(); return; } B.anim = "none"; entering = false; draw(); then(); return; }
    BG.figX = Math.round(10 * ((BG.figX ?? 0) + dx * sp / d)) / 10; BG.figY = Math.round(10 * ((BG.figY ?? 0) + dy * sp / d)) / 10;
    B.anim = deepHere() ? "swim" : "walk"; B.facing = Math.abs(dx) > Math.abs(dy) ? (dx < 0 ? "left" : "right") : dy < 0 ? "back" : "front"; B.turn = "";
    moved(); requestAnimationFrame(step);
  };
  requestAnimationFrame(step);
}
// (ticket 162) A trap room's pits, from their [data-pit] (middle, half
// widths): the one whose hole (PIT_EDGE of it, so a foot on the rim does not
// fall) the friend's feet at (x, z) are over, or null. Walking onto one, not
// jumping, falls in (padPoll, endJump).
// (0.0.157, Matthew: "the mechanism for falling a pit works -- let's integrate that with the rest of collision
// detection") A pit is one of the things the friend's boxes meet, as a solid is: not one that stops them, one they
// fall into, when the middle of each sole (feetSoles) is over its hole; ♿'s collision boxes show it, named, the fall
// glows it as a collision glows a solid (solidHit), and a planned walk goes round it (planPath).
const PIT_EDGE = 0.8;
let pitMemo = { text: undefined, pits: [] };
function pitsHere() {
  if (pitMemo.text !== bgShown.text) pitMemo = { text: bgShown.text, pits: bgShown.node ? [...bgShown.node.querySelectorAll(".portal[data-pit]")].map(el => {
    const [cx, cy, rx, ry] = el.dataset.pit.split(" ").map(Number), room = el.dataset.room;
    return { el, cx, cy, rx, ry, shape: openingShape(el.dataset.shape || "circle"), name: room ? `pit to room ${room}` : "pit" }; }) : [] };
  return pitMemo.pits;
}
const overPit = (q, x, z) => inOpening(q.shape || "circle", q.cx, q.cy, PIT_EDGE * q.rx, PIT_EDGE * q.ry, x, z);   // (ticket 252: by its opening's shape)
function pitAt(x, z) {
  const P = pitsHere();
  if (!P.length) return null;
  const mids = feetSoles(x, z).map(s => [(s[0] + s[1]) / 2, (s[2] + s[3]) / 2]), q = P.find(q => mids.every(([mx, mz]) => overPit(q, mx, mz)));
  if (!q) return null;
  solidHit = { name: q.name, n: solidHit.n + 1, t: performance.now() };   // (the fall glows it and the friend's boxes)
  return q.el;
}
// (after a fall, in the place below: dropping in from above the picture, and landing on bent knees)
let landFall = false;
function fallLand() {
  landFall = false;
  const fig = document.getElementById("figure");
  if (!fig) return;
  const NS = "http://www.w3.org/2000/svg", w = document.createElementNS(NS, "g"), h = fig.getBBox().height || 300;
  fig.replaceWith(w); w.append(fig);
  // (nothing drawn while they drop in, or the drawing would put them down at once: the controller waits,
  // as on a trip, and its wish is "none" already, so it asks for no drawing)
  entering = true; padWant = "none"; tripNext("dropping in");
  w.animate([{ transform: `translate(0px, ${f1(-1.6 * h)}px)` }, { transform: "translate(0px, 0px)" }], { duration: FALL_DROP_MS * 0.8, easing: "ease-in" })
   .onfinish = () => {
    entering = false; tripPhase = "landing";
    // (the high jump's landing: its last part, the knees bending and straightening)
    const B = state.body, rate = Math.max(0.1, Number(B.jumpHighS ?? 75) || 100) / 100;
    startJump("none", "jumphigh"); jumpStart = performance.now() - JUMP_TIMES[1] / rate * 1000;
    changed();
  };
}
const FALL_TEETER_MS = 250, FALL_DROP_MS = 600;
// (ticket 147) Through a doorway on foot: the friend walks to the doorway's
// foot as the controller walks them (PAD_SPEED.walk, deeper at 0.6 of the
// pace), as near as the place lets them; a door swings open by itself as
// they come (its [data-leaf] to its data-open shape, by magic: no reaching
// for a handle yet); they step on into it, fading, and fade in where they
// arrive. walk false: from a ladder's top, the step and the fade only.
const WALK_IN_STEP = 30, WALK_IN_MS = 450, WALK_IN_MAX = 4000;
// (ticket 173) The ladder that leads to a doorway: one inside it (a dungeon's
// or a tree room's way up), or, for a tree's house, that tree's ladder
// ([data-ladder][data-house], drawn apart from the house)
function ladderOf(door) {
  const inside = door.querySelector("[data-ladder]");
  if (inside) return inside;
  const house = door.dataset.house, node = bgShown.node;
  return house && door.dataset.to === "treeroom" && node ? node.querySelector(`[data-ladder][data-house="${house}"]`) : null;
}
// (ticket 173, Matthew: clicking a ladder, the walk to it is good, "but we
// should also animate the climb upward") To a ladder's foot on foot, as
// through a doorway (walkInto), then up it as the 🧗 chip climbs (climb,
// going on in draw), through at its top if it is a way somewhere
// (climbThrough), else holding on up there.
function climbLadder(el) {
  if (entering || !el || !el.isConnected) return;
  const P = pic.getScreenCTM(), L = P ? ladderAt(el, P.inverse()) : null;
  if (!L) return;
  walkInto(el, null, true, "climb", L);
}
// (ticket 162) Falling into a pit (cx, cy, rx, ry, its hole): facing us,
// the arms flung up, a moment's teeter, then down into it, ever faster,
// hidden below the hole's near rim (a clip that stays as the friend drops)
function fallDown([cx, cy, rx, ry], then) {
  const B = state.body, NS = "http://www.w3.org/2000/svg";
  B.anim = "fall"; B.facing = "front"; B.turn = ""; tripPhase = "teetering"; draw();
  setTimeout(() => {
    tripPhase = "falling";
    draw();
    const fig = document.getElementById("figure");
    if (!fig) { then(); return; }
    const h = fig.getBBox().height || 300, clip = document.createElementNS(NS, "clipPath"), id = "fall-clip";
    clip.id = id; clip.setAttribute("clipPathUnits", "userSpaceOnUse");
    clip.innerHTML = `<path d="M${f1(cx - 4 * rx)} ${f1(cy - 4000)} H${f1(cx + 4 * rx)} V${f1(cy)} H${f1(cx + rx)} A${f1(rx)} ${f1(ry)} 0 0 1 ${f1(cx - rx)} ${f1(cy)} H${f1(cx - 4 * rx)} Z"/>`;
    const outer = document.createElementNS(NS, "g"), inner = document.createElementNS(NS, "g");
    outer.setAttribute("clip-path", `url(#${id})`);
    fig.replaceWith(outer); outer.append(clip, inner); inner.append(fig);
    inner.animate([{ transform: "translate(0px, 0px)" }, { transform: `translate(0px, ${f1(1.3 * h)}px)` }], { duration: FALL_DROP_MS, easing: "ease-in", fill: "forwards" })
      .onfinish = then;
  }, FALL_TEETER_MS);
}
// (opt.out: the way they step through, else on toward the doorway, or deeper; opt.here: from where they stand, not walking to it)
// (ticket 239) a doorway too low for the friend: their crown, standing at its depth (their size there), over 0.97 of its
// opening (its first shape, r on the screen, from the top to its foot); then how far before its threshold they crawl
const CRAWL_IN_PACE = 0.55;
// (ticket 331) out through a tree room's door to a bridge: the crawl through it, shrinking; standing up beyond it; the walk
// along the bridge drawn there; and how far along it they go, from and to (a share of the way to its far house)
const DOOR_OUT_MS = 1300, DOOR_STAND_MS = 350, DOOR_DECK_MS = 1200, DOOR_DECK = [0.12, 0.45];
// (ticket 331) the picture seen through a tree room's door to a bridge (shape, its opening, as waysSvg draws it), in the
// picture's units: deck(u), a point u of the way along the bridge drawn there, from the threshold's middle to the far
// house's door; tall(u), the default friend's height there, as wide as its deck there, to their height at the far house
// (as tall as BRIDGE_TALL of its size, as outside: any other in proportion, over BRIDGE_BASE, ticket 352); and its opening, to
// be seen inside of (svg)
function doorViewOf(shape) {
  const M = pic.getScreenCTM(), S = shape.getScreenCTM && shape.getScreenCTM(), n = (shape.getAttribute("d") || "").match(/-?\d*\.?\d+(?:e-?\d+)?/g);
  if (!M || !S || !n || n.length < 8) return null;
  const m = M.inverse().multiply(S), q = [0, 2, 4, 6].map(i => { const p = new DOMPoint(+n[i], +n[i + 1]).matrixTransform(m); return [p.x, p.y]; });
  const G = doorViewGeom(...q), [nx, ny] = G.on(0.5, 1), nearW = Math.abs(G.near[1][0] - G.near[0][0]), farW = 2 * G.pw;
  const tallFar = BRIDGE_TALL * G.hs * figScale(state.background);
  return { deck: u => [nx + (G.cx - nx) * u, ny + (G.py - ny) * u], tall: u => tallFar * (nearW + (farW - nearW) * u) / farW,
           svg: `<path d="M${q.map(p => p.map(f1).join(" ")).join(" L")} Z"/>` };
}
function doorTooLow(r, inv, foot) {
  const BG = state.background, top = new DOMPoint(r.x, r.y).matrixTransform(inv).y, open = foot.y - top;
  if (BG.kind === "clearing") return null;   // (ticket 352: the clearing's doors are far views, the house's small far off: a trip there, never a squeeze)
  if (!(open > 0) || !(figureT.k > 0)) return null;
  const tall = youHeights(true).crown / figureT.k * figScale(BG) * sceneFig(BG.kind) * depthSize(BG, foot.y - SCENE.ground);
  return tall > 0.97 * open ? { tall, open, stretch: Math.max(40, 0.9 * tall) } : null;
}
// (ticket 216) a door's leaf swung open: each of its shapes to its data-open shape (the same commands), its planks and knob
// (lines, ellipses) hidden; what puts it back as it was
// (ticket 331: a tree house's door outside too, platformIn's, as the friend comes to it)
function leafOpen(leaf) {
  const to = leaf.getAttribute("data-open"), undo = [];
  for (const p of leaf.querySelectorAll("path")) {
    const a = document.createElementNS("http://www.w3.org/2000/svg", "animate");
    for (const [k, v] of [["attributeName", "d"], ["to", to], ["dur", "0.6s"], ["fill", "freeze"], ["begin", "indefinite"]]) a.setAttribute(k, v);
    p.append(a); undo.push(() => a.remove());
    try { a.beginElement(); } catch (e) { /* (an old browser: the door stays as it is) */ }
  }
  for (const n of leaf.querySelectorAll("line, ellipse")) { const o = n.style.opacity; n.style.opacity = "0"; undo.push(() => { n.style.opacity = o; }); }
  return () => { for (const u of undo) u(); };
}
// (ticket 264) a door in a side wall being walked through: the wall's x there and which side it is on (-1 left), the
// friend cut off past it (draw); none else
let doorClip = null;
// (ticket 331) out through a tree room's door to a bridge, the friend drawn in the picture seen through the doorway: their
// feet (x, y, in the picture's units), their size (k), and the depth they are sorted at (z, the door's); none else (draw)
let figOver = null;
function walkInto(door, arrive, walk = true, how = "step", lad = null, opt = {}) {
  const P = pic.getScreenCTM(), BG = state.background, B = state.body, F = sceneFrame;
  if (!P) return;
  doorClip = null; figOver = null; figClipRect = null;   // (ticket 331: none left over from a trip cut short, whose doorway this one would keep)
  bridgeOff();   // (ticket 245: from a rope bridge, down at its foot first)
  entering = true;
  const inv = P.inverse(), shape = door.querySelector("path, rect") || door, r = shape.getBoundingClientRect();
  // (a pit: its hole's middle; any other doorway: its foot)
  const pit = how === "fall" && door.dataset.pit ? door.dataset.pit.split(" ").map(Number) : null;
  // (a ladder (ticket 173): its foot, where a climb starts)
  const foot = lad ? { x: lad.x, y: lad.z + 2 } : pit ? { x: pit[0], y: pit[1] } : new DOMPoint(r.x + r.width / 2, r.y + r.height).matrixTransform(inv);
  const tx = opt.here ? BG.figX ?? 0 : clamp(foot.x, F.left, F.right), ty = opt.here ? BG.figY ?? 0 : clamp(foot.y - SCENE.ground, figYMin(F), F.bottom - F.ground);
  // (ticket 239, Matthew: "it makes sense that the doors of the tree house are smaller than ordinary doors -- Can we do a
  // nice check to see if we are currently 'too tall' and if so, part of the path plan for going out is crawling at the
  // end of the animation, near to the threshold of the door") a door lower than the friend standing at it: the last
  // stretch to its threshold, about a body's length, and the step through it, crawled, at a crawl's pace (not a world
  // way's or a room's way toward us, a ladder or a pit: those go their own ways)
  const low = walk && !lad && !pit && !opt.out && door.hasAttribute("data-door") ? doorTooLow(r, inv, foot) : null;
  // (ticket 264) a door in a side wall (one with a leaf, off to a side of the room): which side, for the clip going through
  const side = !lad && !pit && !opt.out && door.hasAttribute("data-door") && door.querySelector("[data-leaf]") && Math.abs(foot.x) > 60 ? Math.sign(foot.x) : 0;
  // (ticket 331, Matthew: "We need to make the scale down extreme to match how we draw the tree house outside the door, which
  // is very shrunk down"; "we should do that gradually as we crawl out, and onto the bridge -- we should animate a part of
  // walking on the bridge (after standing up, post crawl)") a tree room's door to a bridge: out into the picture seen
  // through it, shrinking to the size of the bridge drawn there, then up and along a part of it, smaller still; then out
  const V = opt.doorView && side ? doorViewOf(shape) : null;
  // (the way in: on toward the doorway from where they stop; deeper into the place if it is straight ahead)
  let ux = foot.x - tx, uy = foot.y - (SCENE.ground + ty);
  if (opt.out) [ux, uy] = opt.out;
  else if (Math.hypot(ux, uy) < 12) { ux = 0; uy = -1; }
  const un = Math.hypot(ux, uy); ux /= un; uy /= un;
  const face = (vx, vy) => Math.abs(vx) > Math.abs(vy) ? (vx < 0 ? "left" : "right") : vy < 0 ? "back" : "front";
  // the door, opening
  const leaf = door.querySelector("[data-leaf]"), undo = [];
  if (leaf) undo.push(leafOpen(leaf));
  if (BG.seat) standUp();
  if (B.pose !== "stand" && walk) strikePose("stand");
  const fade = o => { for (const n of figShown) if (n.style) n.style.opacity = o; };
  // (ticket 215) the way there, round what stands in it (planPath): a point at a time
  // (ticket 268: in the spiral staircase, up or down along its steps' middle)
  const spiral = BG.kind === "castleroom" && castleWallsOf(BG).stairs;
  const from = [BG.figX ?? 0, BG.figY ?? 0], path = walk && !opt.here ? (spiral ? spiralPath(from[0], from[1], tx, ty) : planPath(from[0], from[1], tx, ty, strideMore())) : [[tx, ty]];
  walkPath = { pts: [from, ...path], made: performance.now(), leg: 0 };
  // (deeper counts 1 / 0.6 as far, as the controller walks; a long way is walked faster, to take 2.5 s at most)
  const dist = path.reduce((a, p, i) => { const q = i ? path[i - 1] : from; return a + Math.hypot(p[0] - q[0], (p[1] - q[1]) / 0.6); }, 0), speed = Math.max(PAD_SPEED.walk, dist / 2.5);
  let leg = 0;
  let last = 0, phase = walk ? "walk" : "in", t0 = performance.now(), tIn = 0;
  const done = () => {
    for (const u of undo) u();
    tripPhase = "arriving";
    arrive();
    B.anim = "none"; B.facing = "front"; B.turn = "";   // (standing, in the place they came into, facing us)
    entering = false;
    changed();
    if (landFall) { fallLand(); return; }   // (ticket 162: from a pit, dropping in and landing; no fade)
    tripNext("fading in");
    // (fading in where they arrive)
    const t1 = performance.now(), f = t => { const u = Math.min(1, (t - t1) / WALK_IN_MS); if (!entering) fade(u < 1 ? u.toFixed(2) : ""); if (u < 1 && !entering) requestAnimationFrame(f); else if (tripPhase === "fading in") tripPhase = "arrived"; };
    fade("0"); requestAnimationFrame(f);
  };
  const step = t => {
    const dt = last ? Math.min(0.1, (t - last) / 1000) : 0; last = t;
    // (ticket 378) no leg to walk: a plan that found no way (planPath's empty list) threw here, its frames stopped, and entering stayed
    // set, every passage dead after it; with no way to walk, they go through from where they are, as a trip did before walks were planned
    if (phase === "walk" && !path[leg]) { BG.figX = tx; BG.figY = ty; phase = "in"; tIn = t; walkPath.leg = path.length; }
    if (phase === "walk") {
      const crawling = !!low && Math.hypot(tx - (BG.figX ?? 0), (ty - (BG.figY ?? 0)) / 0.6) < low.stretch;
      // (ticket 331, Matthew: "crawling out of a small door looks okay, except we aren't clipping yet") a crawl reaches a body's
      // length ahead of the feet, so through a door in a side wall its head is past the wall's line before the step in: cut
      // off there from the moment the crawl begins, as the step in is
      if (crawling && side && !doorClip) doorClip = { x: foot.x, side };
      const [wx, wy] = path[leg], dx = wx - (BG.figX ?? 0), dy = wy - (BG.figY ?? 0), d = Math.hypot(dx, dy / 0.6), sp = (crawling ? CRAWL_IN_PACE : 1) * speed * dt;
      if ((d <= sp + 0.5) && leg < path.length - 1 && t - t0 <= WALK_IN_MAX) { BG.figX = wx; BG.figY = wy; walkPath.leg = ++leg; moved(); }
      else if (d <= sp + 0.5 || t - t0 > WALK_IN_MAX) { BG.figX = tx; BG.figY = ty; phase = "in"; tIn = t; walkPath.leg = path.length; }
      else {
        const k = sp / d;
        BG.figX = Math.round(10 * ((BG.figX ?? 0) + dx * k)) / 10; BG.figY = Math.round(10 * ((BG.figY ?? 0) + dy * k)) / 10;
        B.anim = crawling ? "crawl" : deepHere() ? "swim" : "walk"; B.facing = face(dx, dy); B.turn = "";   // (ticket 199: swimming, in deep water)
        moved();
      }
    }
    if (phase === "in" && pit) { fallDown(pit, () => done()); return; }
    if (phase === "in" && lad && lad.down != null) {
      // (ticket 199) down a hatch's ladder: at its near edge, facing it, climbing down at ⬆'s pace,
      // out of sight below the edge (draw cuts them off there), then through to the room below
      for (const u of undo) u();
      BG.figX = Math.round(lad.x); BG.figY = Math.round(lad.down - 4 - SCENE.ground);
      B.facing = "back"; B.anim = "climbup"; padWant = null;
      climb = { place: climbPlace(BG), x: lad.x, z: lad.down - 4, top: lad.top, door: lad.door, hands: false, start: performance.now(), done: false, through: false, manual: true, got: 0, down: true, clip: lad.down };
      entering = false; tripPhase = "climbing down";
      let lastD = 0;
      const down = t2 => {
        const dt2 = lastD ? Math.min(0.1, (t2 - lastD) / 1000) : 0; lastD = t2;
        if (!climb || !climb.down || climb.going || state.body.anim !== "climbup") return;
        climb.got -= CLIMB_HAND * figureT.k * dt2; draw(); requestAnimationFrame(down);
      };
      changed(); requestAnimationFrame(down);
      return;
    }
    if (phase === "in" && lad) {
      // (ticket 173: at the ladder's foot, facing it, up it as the 🧗 chip climbs)
      for (const u of undo) u();
      BG.figX = Math.round(lad.x); BG.figY = Math.round(lad.z - SCENE.ground + 2);
      B.facing = "back"; B.anim = "climbup"; padWant = null;
      climb = { place: climbPlace(BG), x: lad.x, z: lad.z + 2, top: lad.top, clipUp: lad.up ?? null, door: lad.door, hands: lad.hands, start: performance.now(), done: false, through: false };
      entering = false; tripPhase = "climbing";
      changed();
      return;
    }
    if (phase === "in" && V) {
      // (through the doorway crawling (walking, through one tall enough), shrinking to the size of the bridge beyond it, a
      // little way onto it; seen on the room's side of the wall's line and, past it, inside the doorway; there they stand,
      // face along it, away from us, and walk a part of it, smaller as they go, seen only inside the doorway)
      if (!tIn) tIn = t;
      if (!V.from) { tripPhase = "stepping in";
        Object.assign(V, { k0: figureT.k, from: [BG.figX ?? 0, SCENE.ground + (BG.figY ?? 0)], face: B.facing === "left" || B.facing === "right" ? B.facing : side < 0 ? "left" : "right",
          room: side < 0 ? `<rect x="${f1(foot.x)}" y="-4000" width="8000" height="8000"/>` : `<rect x="-8000" y="-4000" width="${f1(8000 + foot.x)}" height="8000"/>` }); }
      const e = t - tIn, at = (p, k, anim, facing, clip) => { figOver = { x: p[0], y: p[1], k, z: foot.y }; B.anim = anim; B.facing = facing; B.turn = ""; figClipRect = { svg: clip }; };
      if (e < DOOR_OUT_MS) {
        const u = e / DOOR_OUT_MS, s = u * u * (3 - 2 * u), p = V.deck(DOOR_DECK[0]), k1 = V.tall(DOOR_DECK[0]) / BRIDGE_BASE;
        at([V.from[0] + (p[0] - V.from[0]) * s, V.from[1] + (p[1] - V.from[1]) * s], V.k0 * Math.pow(k1 / V.k0, s), low ? "crawl" : "walk", V.face, V.room + V.svg);
      } else if (e < DOOR_OUT_MS + DOOR_STAND_MS) at(V.deck(DOOR_DECK[0]), V.tall(DOOR_DECK[0]) / BRIDGE_BASE, "none", "back", V.svg);
      else if (e < DOOR_OUT_MS + DOOR_STAND_MS + DOOR_DECK_MS) {
        const u = DOOR_DECK[0] + (DOOR_DECK[1] - DOOR_DECK[0]) * (e - DOOR_OUT_MS - DOOR_STAND_MS) / DOOR_DECK_MS;
        at(V.deck(u), V.tall(u) / BRIDGE_BASE, "walk", "back", V.svg);
      } else { figOver = null; figClipRect = null; doorClip = null; done(); return; }
      draw(); requestAnimationFrame(step); return;
    }
    if (phase === "in") {
      if (!tIn) { tIn = t; tripPhase = "stepping in"; }
      if (side && !doorClip) doorClip = { x: foot.x, side: side };
      // (ticket 264: through a door in a side wall, on past the wall's line, the friend cut off at it, not faded; a little
      // longer, as far as their body is wide)
      const u = Math.min(1, (t - tIn) / (side ? WALK_IN_MS * 2 : WALK_IN_MS)), step = side ? Math.max(WALK_IN_STEP, 120 * figureT.k) : WALK_IN_STEP;
      if (walk) { B.anim = low ? "crawl" : deepHere() ? "swim" : "walk"; B.facing = face(ux, uy); B.turn = ""; }
      BG.figX = tx + ux * step * u; BG.figY = ty + uy * step * 0.6 * u;
      draw(); if (!side) fade((1 - u).toFixed(2));   // (drawn now, not as moved() may, later: the fade goes on what is drawn)
      if (u >= 1) { doorClip = null; done(); return; }
    }
    requestAnimationFrame(step);
  };
  requestAnimationFrame(step);
}
const backBtn = document.getElementById("back");
// (a place's name, from PLACES itself: until 0.0.233 it was read from the 🏞️ Background tab's first row, which since 0.0.213 is
// the full-screen chip, so a trip that left a way back threw here and stopped, the friend stuck going in: Matthew, "stuck in
// the climb into tree house animation")
const sceneName = kd => (PLACES.find(([v]) => v === kd) || [kd, kd])[1];
function backSync() {
  const last = trips[trips.length - 1], show = !!last && last.to === state.background.kind;
  backBtn.hidden = !show;
  if (show) {
    const name = sceneName(last.kind);
    backBtn.textContent = `⬅️ ${name.split(" ")[0]} back`;
    backBtn.title = "back to the " + name.replace(/^\S+\s/, "");
    backBtn.setAttribute("aria-label", backBtn.title);
  }
}
backBtn.addEventListener("click", () => {
  const last = trips[trips.length - 1];
  if (entering || !last) return;
  flyInto(backBtn.getBoundingClientRect(), () => {
    trips.pop();
    // (ticket 185: back to the square left, its own things and paths; ticket 406: in the world left)
    if (last.worldAt) { if (last.worldId && state.world.id !== last.worldId && worldById(last.worldId)) worldSwitch(last.worldId, last.worldAt, null, false); else worldEnter(last.worldAt, null, false); }
    Object.assign(state.background, { kind: last.kind, figX: last.figX, figY: last.figY });
    if (last.inside) state.background.inside = last.inside;
    if (last.tent) state.background.tent = last.tent;
    if (last.castleRoom) state.background.castleRoom = last.castleRoom;
  });
});

showTab();
// (the divider's place again, now that the first picture is drawn and its shape known)
setSplit();
// (this version after the game's name, and a newer one offered when there is one: ticket 134)
showVersion();
// (a copy of the game kept on this device, for airplane mode: ticket 486)
offlineStart();
