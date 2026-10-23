"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 18 of 26 of the game's script, run in order by index.html.

function draw() {
  const t = performance.now(), text = bgShown.text;
  drawNow();
  if (tripTimer) tripWork("drawing", t);   // (⏱️ a trip being timed)
  if (bgShown.text !== text) return;
  const ms = performance.now() - t;
  if (!sceneryStill()) liveDrawAvg = liveDrawAvg ? 0.9 * liveDrawAvg + 0.1 * ms : ms;   // (ticket 510: the place drawn live, for the churn guard)
  slowAvg = slowN ? 0.9 * slowAvg + 0.1 * ms : ms;
  if (++slowN < 30) return;
  const was = slowFlag;
  if (!slowFlag && slowAvg > SLOW_IN) slowFlag = true;
  else if (slowFlag && slowAvg < SLOW_OUT) slowFlag = false;
  if (slowFlag !== was) { try { localStorage.setItem("dress-up-slow", slowFlag ? "1" : "0"); } catch (e) { /* (for this visit) */ } setTimeout(changed, 0); }
}
// (the scene's drawing held still: its animations taken out, once for each text)
const ANIMS_RE = /<(animate|animateTransform|animateMotion|set)\b[^>]*?(\/>|>[\s\S]*?<\/\1>)/g;
let stillMemo = { in: null, out: null };
const stillScene = text => { if (text !== stillMemo.in) stillMemo = { in: text, out: text.replace(ANIMS_RE, "") }; return stillMemo.out; };
function drawNow() {
  const { face: F, body: B } = state;
  // (ticket 123) treading water: the strokes for now, on top of any pose
  // (ticket 132: any animation from the Body tab, the same way; `tread` is only treading in water)
  // (ticket 438: riding the horse, the rider's own limbs, rideMove, in place of the animation's)
  const move = riding(B) ? rideMove(B) : moveNow(), tread = move && move.water ? move : null, ride = !!(move && move.riding);
  const poseT = (what, side) => poseOf(B, what, side) + (move ? move[what + (side > 0 ? "L" : "R")] || 0 : 0);
  // (ticket 124) which way they face (0 toward us; -1, 1 in profile to our
  // left or right), and whether they sit. In profile the friend is narrower,
  // their arms and legs come from under the shoulder and hip, and point the
  // way they face; the side nearer us is their -dir side (facing our right,
  // their right shoulder is toward us)
  const dir = facingOf(B), sitting = !ride && ((!!B.sit && B.pose !== "lie") || B.pose === "hug");   // (ticket 136: a hug's legs are a sitter's; ticket 299: lying on a bed, not)
  const q = dir ? 0 : turnOf(B);   // (ticket 144: turned partway, the far side's sign)
  // (ticket 138) facing away from us: drawn as facing us (dir 0), but the
  // back of the head is hair, with no face on it, and what is on their back
  // (wings, a tail) is in front of them
  const away = B.facing === "back";
  let { shirt: SH, pants: P } = state;
  const skin = F.color, skinLine = darker(skin);
  // A costume: a suit is fur over everything below the hood; a falcon is
  // wings and a tail over the clothes.
  const C = costumeOn(), kind = state.costume.kind, suit = C && ANIMALS[kind].suit;
  // (ticket 499) a dragon: the suit, with horns, spines and teeth on its hood, scales, a plated belly, claws, bat wings and a tail
  const dragon = kind === "dragon";
  // (ticket 544) a giraffe: the suit in its style's coat, its long neck and its own head up over the hood (giraffeNeckSvg),
  // hooves, a tail with a dark tuft
  const giraffe = kind === "giraffe";
  // (ticket 530) a Sasquatch: the suit in its style's colours, shaggy along its arms, legs and sides (shagEdge: two shapes an
  // edge), big feet, and its footprints; each piece a group marked data-sasquatch, nothing in one turned
  const sasq = kind === "sasquatch", sqG = (what, t) => t ? `<g data-sasquatch="${what}">${t}</g>` : "";
  const sqArm = (arm, d) => sasq ? sqG("arm", shagEdge(armJoints(arm), d ? -d : arm.side, (armW + 9) / 2, 9, C)) : "";
  // (ticket 557: a ninja's bare arms, legs and neck are in the cloth, the wrists in the gloves, js/06-costumes.js)
  const limb = suit ? C.fur : kind === "ninja" ? NINJA.glove : skin, limbLine = suit ? darker(C.fur) : kind === "ninja" ? NINJA.line : skinLine;
  // A witch: a long dress -- long sleeves, a long skirt with a ragged hem
  // -- in the hat's color with a spider web on it; striped stockings; a
  // pointed hat; and, if chosen, a cat on her shoulder and a broom in her
  // hand. Her own hair and face show.
  const witch = kind === "witch", WC = state.costume, fairy = kind === "fairy";
  // (ticket 431) a gnome: the man, or the woman (gnomeWho), each under the tall pointed hat
  const gnome = kind === "gnome", gnomeW = gnome && WC.gnomeWho === "woman", gnomeM = gnome && !gnomeW;
  // (ticket 432) a wizard: a long robe with stars, a gold cord, a long beard, the hat with stars and a moon, a staff
  const wizard = kind === "wizard";
  // (ticket 433) a monk or a dryad: a hooded cloak to the feet, a rope belt, a staff; the hood up hides the hair
  const monk = kind === "monk", hooded = !!clothHood() || kind === "knight" || kind === "robot" || kind === "ninja";   // (ticket 515: a robot's head too; 557: a ninja's mask)
  // (ticket 434) a forester (Robin Hood): a green tunic with dagged edges over hose, a strap across the chest, a quiver, a bow
  const robin = kind === "robin";
  // (ticket 435) a knight: mail under a tabard in his arms, a helmet (the hair under it), a sword and a shield
  const knight = kind === "knight";
  // (tickets 436, 437) a queen or a king: a robe to the floor, a gold girdle, an ermine mantle behind, a crown, a scepter
  const royal = kind === "queen" || kind === "king", king = kind === "king", robeC = king ? WC.kingRobe || "#7a1f2b" : WC.queenRobe || "#5b2a86";
  // (ticket 522) a cowgirl or a cowboy: a style each (W: its clothes, pieces and colours, and the hat, W.H, shared by both)
  const western = kind === "cowgirl" || kind === "cowboy", W = western ? westernOf(WC, kind) : null;
  // (ticket 557) a ninja: all in black, gloves, soft boots with bands up the shin, a mask hiding the face, and the gold head
  // covering with its gem (ninjaMask, ninjaBand, ninjaShin in js/06-costumes.js)
  const ninja = kind === "ninja", njG = (what, t) => t ? `<g data-ninja="${what}">${t}</g>` : "";
  // (ticket 561) its bands on a forearm, from the glove up to below the elbow, the skin between them unless 🌑 full darkness
  const njArm = arm => ninja ? njG("armbands", ninjaShin(armJoints(arm), (armW + 9) / 2, WC.ninjaBoots, WC.ninjaBetween === "dark" ? null : skin, 0.85 * state.hands.size)) : "";
  // (a fairy: her tunic in its style's color, and the style's pattern)
  const fTunic = FAIRY_TUNICS[WC.tunicStyle] ? WC.tunicStyle : "petal", fCol = WC["tunicColor_" + fTunic] ?? FAIRY_TUNICS[fTunic].colors[0];
  const fWing = FAIRY_WINGS[WC.wingStyle] ? WC.wingStyle : "butterfly", fWingCol = WC["wingColor_" + fWing] ?? FAIRY_WINGS[fWing].colors[0];
  // (her brim covers her hair: as wide as its widest reach and 14 more,
  // up to 300; and the hair above the brim is inside the hat, unseen)
  const brimRx = witch ? Math.min(300, Math.max(128, hairReach(state.hair).half + 14)) : wizard ? Math.min(260, Math.max(116, hairReach(state.hair).half + 10)) : 128;
  // (y: the brim's line where the hair is drawn: 40 in the hair's own
  // units, stretched with the head; the bangs give it in the face's)
  const hatLine = witch || wizard || robin || royal ? 40 : gnome ? 62 : western && W.H ? COW_HAT_LINE : null;
  const tucked = (inner, id, y = hatLine) => hatLine !== null ? `<clipPath id="${id}"><rect x="-4000" y="${f1(y)}" width="8200" height="8000"/></clipPath>` +
                                        `<g clip-path="url(#${id})">${inner}</g>` : inner;
  // (her web is tiled all over the dress, or "a few" separate webs on it,
  // as the Hazel slide draws them)
  const fewWebs = witch && WC.webStyle === "few", webPat = fewWebs ? "plain" : "web";
  if (witch) {
    SH = { ...SH, style: "long", color: WC.witchColor, pattern: webPat, patternSize: 96, patternColor: WC.webColor };
    P = { ...P, style: "dress", skirtLen: 72, color: WC.witchColor, pattern: webPat, patternSize: 96, patternColor: WC.webColor };
  }
  if (royal) { SH = { ...SH, style: "long", color: robeC, pattern: "plain" }; P = { ...P, style: "dress", skirtLen: 100, color: robeC, pattern: "plain" }; }
  if (knight) { const mail = { color: "#9ca3af", pattern: "dots", patternSize: 9, patternColor: "#6b7280" }; SH = { ...SH, style: "long", ...mail }; P = { ...P, style: "long", ...mail }; }
  if (robin) { SH = { ...SH, style: "long", color: WC.robinTunic || "#3f7d3a", pattern: "plain" }; P = { ...P, style: "long", color: "#5a3d22", pattern: "plain" }; }
  if (monk) {
    const cloak = { color: WC.monkCloak || "#6b4a2b", ...(WC.monkWho === "dryad" ? { pattern: "leaves", patternSize: 26, patternColor: mix(WC.monkCloak || "#4f6b3a", "#000000", 0.3) } : { pattern: "plain" }) };
    SH = { ...SH, style: "long", ...cloak }; P = { ...P, style: "dress", skirtLen: 94, ...cloak };
  }
  if (wizard) {
    const robe = { color: WC.wizardRobe || "#2a3a8c", pattern: "stars", patternSize: 30, patternColor: WC.wizardStars || "#f2c230" };
    SH = { ...SH, style: "long", ...robe }; P = { ...P, style: "dress", skirtLen: 96, ...robe };
  }
  if (gnomeM) {
    SH = { ...SH, style: "long", color: WC.gnomeTunic || "#3f8f8a", pattern: "plain" };
    P = { ...P, style: "long", color: WC.gnomeTrousers || "#7d8b6a", pattern: "plain" };
  }
  if (gnomeW) {
    SH = { ...SH, style: "long", color: GNOME_CLOTH, pattern: "plain" };
    P = { ...P, style: "dress", skirtLen: 84, color: WC.gnomeSkirt || GNOME_STRIPES[0], pattern: "gnomeskirt", patternSize: 64 };
  }
  if (fairy) {
    const pat = fTunic === "toadstool" ? { pattern: "dots", patternSize: 34, patternColor: "#ffffff" } : fTunic === "sparkle" ? { pattern: "stars", patternSize: 26, patternColor: "#fde047" } : { pattern: "plain" };
    SH = { ...SH, style: "tank", color: fCol, ...pat };
    // (in a dress; or a shirt and pants: her tunic top over pants a shade
    // deeper, its hem a short frill at her hips, see below)
    P = WC.fairyOutfit === "pants" ? { ...P, style: "pants", color: mix(fCol, "#000000", 0.28), pattern: "plain" }
                                   : { ...P, style: "dress", skirtLen: 58, color: fCol, ...pat };
  }
  // (ticket 288) a scuba suit: long sleeves and legs in its color, its stripes and fins (below); the old diving suit:
  // canvas, a little wider, heavy boots, and over the head and shoulders its helmet and corselet (below)
  const scuba = kind === "scuba", diver = kind === "diver";
  if (scuba) { SH = { ...SH, style: "long", color: WC.scubaColor, pattern: "plain" }; P = { ...P, style: "long", color: WC.scubaColor, pattern: "plain" }; }
  if (diver) { SH = { ...SH, style: "long", color: WC.diverCloth, pattern: "plain", width: SH.width * 1.1 }; P = { ...P, style: "long", color: WC.diverCloth, pattern: "plain" }; }
  // (ticket 515) a robot: one of ROBOT_STYLES (R: its colours and traits), its sleeves and legs in its limb colour (the box
  // bot's legs and feet the friend's own); its pieces below, each in its own slot, each view its own drawing
  const robot = kind === "robot", R = robot ? robotOf(WC) : null;
  if (robot) { SH = { ...SH, style: "long", collar: "flat", color: R.limb, pattern: "plain" }; if (R.S.feet !== "own") P = { ...P, style: "long", color: R.limb, pattern: "plain" }; }
  if (western) { SH = { ...SH, ...W.shirt }; P = { ...P, ...W.pants }; }   // (ticket 522)
  if (ninja) { SH = { ...SH, style: "long", collar: "v", color: NINJA.cloth, pattern: "plain" }; P = { ...P, style: "long", color: NINJA.cloth, pattern: "plain" }; }   // (ticket 557)
  const w = dir ? SH.width * 0.62 : q ? SH.width * 0.84 : SH.width, L = SH.length;
  const nw = Math.max(22, B.neckW / 2 + 6);           // the collar fits the neck

  // ARMS: from each shoulder, at the arms angle, to a hand. (ticket 99)
  // Each bends at its elbow, halfway: the forearm turned from the upper
  // arm by the elbows' bend (+ up, - down; both arms alike, mirrored), the
  // hand at the forearm's angle. Unbent (0), an arm is one straight line.
  // (ticket 121) An arm posed on its own turns and bends by its own pose
  // on top of the sliders' (poseOf), and is always drawn through its elbow.
  const a = B.arms * Math.PI / 180, armLen = B.armLen, armW = B.armW, elbow = B.elbows ?? 0, eb = elbow * Math.PI / 180;
  const elbowCurl = (Number(B.elbowCurl) || 0) * Math.PI / 180, elbowSwing = (Number(B.elbowSwing) || 0) * Math.PI / 180;   // (ticket 414)
  // (in profile, both from the shoulder and to the front, `out`, the far one
  // held a little higher so both show)
  const arms = [1, -1].map(side => {
    // (turned partway, the far shoulder a little in: ticket 144)
    const A = dir ? [-dir * 3, 8] : [side * (w - (side === q ? 14 : 6)), 8], out = dir || side;
    const turn = B.arms + poseT("Arm", side) + (dir ? (side === dir ? -14 : 4) : 0), bend = elbow + poseT("Elbow", side);
    if (!dir && turn === B.arms && bend === elbow) {
      if (!eb) return { side, out, A, H: [A[0] + side * armLen * Math.cos(a), A[1] + armLen * Math.sin(a)],
                        angle: side > 0 ? B.arms : 180 - B.arms, joints: null };
      const E = [A[0] + side * armLen / 2 * Math.cos(a), A[1] + armLen / 2 * Math.sin(a)];
      const H = [E[0] + side * armLen / 2 * Math.cos(a - eb), E[1] + armLen / 2 * Math.sin(a - eb)];
      return { side, out, A, E, H, angle: side > 0 ? B.arms - elbow : 180 - B.arms + elbow, joints: [A, E, H] };
    }
    const ta = turn * Math.PI / 180, fa = (turn - bend) * Math.PI / 180;
    // (ticket 287: an arm reaching toward us or away is seen short from the shoulder, as a leg's step is: ArmStep)
    const ak = dir ? 1 : 1 - Math.max(0, Math.min(0.9, poseT("ArmStep", side) || 0));
    const E = [A[0] + out * ak * armLen / 2 * Math.cos(ta), A[1] + ak * armLen / 2 * Math.sin(ta)];
    const H = [E[0] + out * ak * armLen / 2 * Math.cos(fa), E[1] + ak * armLen / 2 * Math.sin(fa)];
    return { side, out, A, E, H, angle: out > 0 ? turn - bend : 180 - turn + bend, joints: [A, E, H] };
  }).map(arm => {
    // (ticket 414) the elbows' curl and swing, on top of all that: the forearm turned toward the front of the body (a curl
    // with a weight), then about the upper arm (side to side, a kettle swung across a table), and seen as it then points
    // (an animation's own curl too, ElbowCurlL/R: the crouch's, facing us)
    const t = turned3(arm.A, arm.E || [(arm.A[0] + arm.H[0]) / 2, (arm.A[1] + arm.H[1]) / 2], arm.H, elbowCurl + poseT("ElbowCurl", arm.side) * Math.PI / 180, elbowSwing * arm.side, dir ? [dir, 0, 0] : [0, 0, 1]);
    return t ? { ...arm, E: t.J, H: t.T, joints: [arm.A, t.J, t.T], angle: t.angle ?? arm.angle } : arm;
  });
  // (in profile, the arm on our side is in front of the body, the other behind)
  // (turned partway, as in profile: the far arm wholly behind, the near one in front: ticket 144)
  const back = dir || q;
  const farArms = back ? arms.filter(x => x.side === back) : arms, nearArms = back ? arms.filter(x => x.side === -back) : arms;
  // an arm's joints (shoulder, elbow, hand; or shoulder and hand), and its
  // sleeve's: the share `f` of the arm's length from the shoulder, round the elbow
  const armJoints = arm => arm.joints || [arm.A, arm.H];
  const alongArm = (arm, f) => {
    if (!arm.joints) return [arm.A, [arm.A[0] + (arm.H[0] - arm.A[0]) * f, arm.A[1] + (arm.H[1] - arm.A[1]) * f]];
    const { A, E, H } = arm;
    return f <= 0.5 ? [A, [A[0] + (E[0] - A[0]) * 2 * f, A[1] + (E[1] - A[1]) * 2 * f]] : [A, E, [E[0] + (H[0] - E[0]) * (2 * f - 1), E[1] + (H[1] - E[1]) * (2 * f - 1)]];
  };
  // no clothes: underwear or a swimsuit instead of the shirt and pants
  // (a full animal suit still covers everything)
  const U = state.undies, undies = U.on && !suit && !witch && !fairy && !scuba && !diver && !gnome && !wizard && !monk && !robin && !knight && !royal && !robot && !western && !ninja;
  const sleeve = suit ? 0.97 : undies ? 0 : { tee: 0.38, long: 0.95, tank: 0 }[SH.style];

  // PANTS: their hips under the shirt's hem, their feet under the legs.
  // (ticket 99) Each leg bends at its knee, halfway: bent, the knees go out
  // and the feet stay under the hips, the thigh and the shin each leaning
  // half the knees' bend, so the legs reach less far down and the friend
  // crouches (lower on the ground by as much). Unbent (0), a leg is straight.
  // (ticket 121) A leg posed on its own: its thigh turned out from straight
  // down by half the knees' bend and its own turn, its shin back in by the
  // knees' bend and its own; then the feet need not be level, and the
  // friend stands on the lower (feetY).
  const knee = (B.knees ?? 0) * Math.PI / 360;
  const hw = w - 2, LL = B.legLen, fx = dir ? 3 : hw / 2 + 6;
  // (the legs by a pose: posed or not, and each leg's joints)
  // (ticket 124) Sitting: facing us, the thighs come toward us, so they are
  // drawn short (`thigh`), the knees a little apart and the shins straight
  // down; in profile the thighs go forward, level, and the shins hang down.
  // In profile a knee bends forward, the way the friend faces.
  const legBase = { t0: sitting ? (dir ? 88 : 10) : (B.knees ?? 0) / 2, b0: sitting ? (dir ? 88 : 10) : (B.knees ?? 0), thigh: sitting && !dir ? 0.3 : 1 };
  // (ticket 140: a step toward us or away shortens the leg from the hip, as seen)
  const stepOf = (pose, side) => (dir ? 0 : Math.max(0, Math.min(0.6, pose("Step", side) || 0)));
  const foreshort = (joints, k) => (k ? joints.map(([x, y], i) => (i ? [x, L + (y - L) * (1 - k)] : [x, y])) : joints);
  const bonesFlat = pose => { const posed = !!dir || sitting || [1, -1].some(side => pose("Leg", side) || pose("Knee", side) || stepOf(pose, side));
    return { posed, bones: [1, -1].map(side => {
      const Hp = [side * fx, L], out = dir || side;
      if (!posed) { const fy = knee ? L + LL * Math.cos(knee) : L + LL;
        return { side, out, joints: knee ? [Hp, [side * (fx + LL / 2 * Math.sin(knee)), L + LL / 2 * Math.cos(knee)], [side * fx, fy]] : [Hp, [side * fx, fy]] }; }
      const tt = (legBase.t0 + pose("Leg", side)) * Math.PI / 180, st = tt - (legBase.b0 + pose("Knee", side)) * Math.PI / 180;
      const K = [Hp[0] + out * LL / 2 * legBase.thigh * Math.sin(tt), Hp[1] + LL / 2 * legBase.thigh * Math.cos(tt)];
      return { side, out, joints: foreshort([Hp, K, [K[0] + out * LL / 2 * Math.sin(st), K[1] + LL / 2 * Math.cos(st)]], stepOf(pose, side)) };
    }) }; };
  // (ticket 414) the knees' curl and swing, on top of that: the shin folded back, behind the body (away from us, or behind
  // the way they face), then swung about the thigh's line, the foot in or out; such a leg counts as posed, so they stand on
  // the lower of its foot and knee
  const kneeCurl = (Number(B.kneeCurl) || 0) * Math.PI / 180, kneeSwing = (Number(B.kneeSwing) || 0) * Math.PI / 180;
  const bonesBy = pose => { const r = bonesFlat(pose);
    if (!kneeCurl && !kneeSwing && ![1, -1].some(sd => pose("KneeCurl", sd))) return r;
    return { posed: true, bones: r.bones.map(b => { const J = b.joints, K = J.length > 2 ? J[1] : [(J[0][0] + J[1][0]) / 2, (J[0][1] + J[1][1]) / 2];
      const t = turned3(J[0], K, J[J.length - 1], kneeCurl + (pose("KneeCurl", b.side) || 0) * Math.PI / 180, -kneeSwing * b.side, dir ? [-dir, 0, 0] : [0, 0, -1]);
      // (facing us, a shin turned more than 30 degrees away puts its foot behind the leg: its shoe drawn before the leg, below)
      return t ? { ...b, joints: [J[0], t.J, t.T], behind: !dir && t.dz < -0.5 } : b; }) }; };
  const { posed: legPosed, bones: legBones } = bonesBy(poseT);
  const legOf = side => legBones[side > 0 ? 0 : 1];
  const footAt = side => legOf(side).joints[legOf(side).joints.length - 1];
  const feetY = Math.max(footAt(1)[1], footAt(-1)[1]);
  const kneeAt = side => { const J = legOf(side).joints; return J.length > 2 ? J[1] : [(J[0][0] + J[1][0]) / 2, (J[0][1] + J[1][1]) / 2]; };
  const legJoints = side => legOf(side).joints;
  // each pants leg is wide enough for the leg inside it
  const half = Math.max(hw / 2, B.legW / 2 + 6);
  // (ticket 515: a robot stands on the feet it wears, its boots at their own size, the mech's 1.35 times, never bare; the box
  // bot's are the friend's own shoes. Every other costume as before: the friend's shoes)
  const ninjaBoots = ninja ? { style: "ninjaboot", size: state.shoes.size, color: NINJA.glove } : null;   // (ticket 557: soft and quiet, drawn by shoeSvg)
  const worn = robot ? robotFeet(R, state.shoes) : western ? westernFeet(W, state.shoes) : ninja ? ninjaBoots : state.shoes;   // (ticket 522: and a cowgirl or cowboy on their boots)
  const shoeSc = worn.size / 100;
  const standOn = (fy, kneeY) => Math.max(fy + (worn.style === "bare" ? 18 : 30) * shoeSc, kneeY);
  // (treading, they float where they would stand: the ground of their legs
  // as posed, not as they kick, or every kick would jolt them up and down)
  // (ticket 141: a crouch and a jump stand on their feet as bent: grounded)
  const still = move && !move.rot && !move.grounded && bonesBy((what, side) => poseOf(B, what, side));
  // (ticket 286: crawling toward us, the hands are nearest, on the floor, and the legs are hidden behind the body)
  const crawlTo = move && move.squash && !dir && !away;
  const ground = crawlTo ? Math.max(...arms.map(x => x.H[1])) + 12
               : still ? standOn(Math.max(...still.bones.map(l => l.joints[l.joints.length - 1][1])), still.posed ? Math.max(...still.bones.map(l => l.joints[1][1])) + B.legW / 2 : -Infinity)
                       : standOn(feetY, legPosed ? Math.max(kneeAt(1)[1], kneeAt(-1)[1]) + B.legW / 2 : -Infinity);   // (or kneels, a knee lowest)
  poseJoints = { armLen, LL, armW, legW: B.legW, legBase,
                 arms: arms.map(x => ({ side: x.side, out: x.out, A: x.A, H: x.H, E: x.E || [(x.A[0] + x.H[0]) / 2, (x.A[1] + x.H[1]) / 2] })),
                 legs: legBones.map(l => ({ side: l.side, out: l.out, H: l.joints[0], K: kneeAt(l.side), F: footAt(l.side) })),
                 handR: Math.min(80, 0.7 * Math.max(handReach(state.hands), handReach(rightHandOf(state.hands)))),
                 footR: Math.min(90, 0.7 * Math.max(footReach(worn), footReach(rightShoeOf(worn))) * shoeSc) };

  // HEAD: its chin on the top of the neck.
  const neckTop = -20 - B.neck;
  const s = F.size / 80;
  // (ticket 286: crawling toward us or away, the back seen short (move.squash): the head down on the neck as it is drawn)
  // (toward us, the head is nearest: down in front of the shoulders, between the arms; the shirt turned over behind it,
  // a little of the back rising from the shoulders (-20), seen from in front and above: flipped)
  const squash = move && move.squash && !dir ? move.squash : 0;
  const headDrop = !squash ? 0 : crawlTo ? -20 + (move.chin ?? 0.3) * armLen - neckTop : (L - neckTop) * (1 - squash);
  const squashed = t => squash ? `<g transform="matrix(1 0 0 ${squash.toFixed(3)} 0 ${f1(L * (1 - squash))})">${t}</g>` : t;
  const backK = crawlTo ? move.back ?? squash : 0;
  const flipped = t => `<g transform="matrix(1 0 0 ${(-backK).toFixed(3)} 0 ${f1(-20 * (1 + backK))})">${t}</g>`;
  const ty = neckTop + 6 - (headChin(F) - 4) * s + headDrop, tx = -100 * s;
  faceFrame = { tx, ty, s, drop: headDrop };
  // (ticket 124: in profile the hair sits toward the back of the head, in the face's units)
  const side = dir && !C && !diver && !hooded ? sideHair(state.hair, F, dir) : null;   // (ticket 288: none under the old helmet)
  // (ticket 515) a robot's head, in the face's units, and how far up and out it reaches; lying face up (on the back, seen from
  // the side: lieHead to our right faces our left), what is on the back is under them; turned the other way (↔️ facing, after
  // the pose), they lie face down, and the back's pieces stand up off it as in profile
  const lying = !move && B.pose === "lie", lieBack = lying && !!dir && (B.lieHead === "right" ? -1 : 1) === dir;
  const RH = robot ? robotHead(R, F, { dir, away, closed: !!F.closed, lieBack }) : null;
  // (ticket 536) a moth: the suit, a moth's wings on its arms as a bird suit's (mothWing: its style's, M), feathery antennae
  // and a fuzzy crown on its hood, a fluffy collar, a banded belly; each piece a group marked data-moth, nothing in one turned
  const moth = kind === "moth", M = moth ? mothOf(WC) : null;

  // The picture's frame grows to hold everything.
  const handYs = arms.map(x => x.H[1]);
  // each hand's own reach past the end of its arm: big mittens and long
  // fingers stuck out of the frame (and out of the saved picture)
  // (a falcon suit's wing reaches past the hand by about an arm's length)
  // (either hand's: the left may wear its own style)
  const reach = BIRD_SUIT[kind] ? B.armLen * 1.4 + 50
    : robot ? handReach(robotHands(R, state.hands)) + 12   // (ticket 515)
    : (suit ? handReach(suitHands(C, kind))
            : Math.max(handReach(state.hands), handReach(rightHandOf(state.hands)))) + 12;
  // room for big hair on a tall head, a lid blown far off, and candy
  // and hair, stretched with the head (it hangs and spreads in face units)
  const hr = C || hooded ? { top: 0, bottom: 0, half: 0 } : hairReach(state.hair);
  const hairY = y => HEAD_C + (y - HEAD_C) * headH(F);
  // (ticket 442) the friend's beard, from 💇 Hair (none under the old diving helmet or as a ghost), and how far below the
  // chin it reaches, for the picture's bottom
  const beardStyle = BEARDS[state.hair.beard] && kind !== "diver" && kind !== "ghost" && kind !== "robot" && kind !== "ninja" ? state.hair.beard : null;   // (ticket 557: none under the mask)
  const flying = F.burst > 0 ? idiv(F.burst * 7, 10) + idiv(F.burst * 4, 10) + 40 : 0;   // see topFlies
  const faceTop = Math.min(-90, HEAD_C - 175 * headH(F), headCrown(F) + 50.6 - 1.4 * F.burst - 40,
                           hairY(hr.top) - 40 - flying, (C ? (kind === "deer" || dragon ? -140 : -90) : 0) - flying,
                           witch ? hairY(-130) - 20 - flying : 0, gnome ? hairY(-240) - 20 - flying : 0, wizard ? hairY(-170) - 20 - flying : 0, hooded ? hairY(hoodGeom(F).cy - hoodGeom(F).ry - 40) - 10 - flying : 0, robin ? hairY(-90) - 10 - flying : 0, knight ? hairY(-40) - 10 - flying : 0, royal ? hairY(king ? -60 : -40) - 10 - flying : 0, RH ? RH.top - 10 - flying : 0, western && W.H ? hairY(W.H.top - 10) - 10 - flying : 0);
  // (ticket 518) WHAT EACH HAND HOLDS (heldItems: the costume's own, "auto", or what 🧍 Body's 🫴 holding chose), in the
  // arm that is that own hand: the left is side +1 facing us (arms[0], where the broom, staff, scepter, sword and lasso
  // always were), side -1 from behind, so from behind a thing stays in its own hand. The lasso is put away while the hands
  // are busy (crawling, swimming or treading, climbing, lying: ticket 522); running, its loop twirls (LFO B's phase).
  const handsBusy = (move && (move.swim || move.squash || move.rot || move.water || move.ladder != null)) || /climb|swim/.test(B.anim || "") || B.pose === "lie";
  // (swimming out of the water, or in shallow water, is a tread stroke with neither swim nor water set: still busy, 522's rule)
  // (ticket 528: and the other way, the own hand an arm is: from behind, side +1 is the right hand, so its style goes there too)
  const ownOf = side => (away ? -side : side) > 0 ? "L" : "R";
  const armOf = own => arms.find(x => x.side === (away ? -1 : 1) * (own === "L" ? 1 : -1)) || null;
  const heldNow = heldItems(B, kind, WC);
  const held = Object.keys(HOLDABLES).flatMap(id => ["L", "R"].filter(own => heldNow[own] === id).map(own => ({ own, item: id, arm: armOf(own) })))
    .filter(h => h.arm && !(h.item === "lasso" && handsBusy) && !(move && move.swim && h.item !== "rein"));
  // (ticket 529: swimming, everything held is put away, as the lasso is: both arms windmill over the head and under the
  // body each stroke, so a thing in the hand would sweep through the friend; crawling toward us, each lies flat on the
  // floor under its hand, heldSvg's "floor" drawings, reaching out to the side, not up)
  const lassoArm = (held.find(h => h.item === "lasso") || {}).arm || null, lassoH = lassoArm ? lassoArm.H : null;
  const lassoSpin = lassoH && move && B.anim === "run" ? (lfoNow("B") || {}).phase || 0 : null;
  // (and a cat on the shoulder, 64 above where it sits and 42 below, its
  // tail out 34 to the side, all times its size)
  const kb = (state.costume.broomSize ?? 100) / 100, catK = witch && WC.witchCat ? (WC.catSize ?? 100) / 100 : 0;
  // (each held thing's reach from its hand, HOLDABLES' ext: a broom's times its size, a twirling lasso's up and out)
  const heldExt = h => { if (crawlTo) return [16, 24, h.item === "broom" ? 130 * kb : 130];
    const [u, d, o] = HOLDABLES[h.item].ext, k = h.item === "broom" ? kb : 1, spin = h.item === "lasso" && lassoSpin != null;
    return [spin ? 70 : u * k, d * k, spin ? 110 : o * k]; };
  const heldUp = held.map(h => heldExt(h)[0] ? h.arm.H[1] - heldExt(h)[0] : 0), heldDown = held.map(h => heldExt(h)[1] ? h.arm.H[1] + heldExt(h)[1] : 0);
  const heldOut = held.map(h => heldExt(h)[2] ? -(Math.abs(h.arm.H[0]) + heldExt(h)[2]) : 0);
  // (posed limbs: room for their elbows, knees and feet wherever they went)
  const armPosed = [1, -1].some(side => poseT("Arm", side) || poseT("Elbow", side));
  const posedYs = [...(armPosed ? arms.filter(x => x.E).map(x => x.E[1] - armW - 20) : []), ...(legPosed ? legBones.flatMap(l => l.joints.slice(1)).map(([, y]) => y - B.legW - 40) : [])];
  let top = Math.min(ty + faceTop * s, ...handYs.map(y => y - Math.max(40, reach)), ...heldUp, ...posedYs,
                       catK ? -9 - 70 * catK : 0, dragon && dir ? -255 : 0);   // (ticket 499: the far wing, up over the head in profile)
  const pileP = F.sick > 100 ? 100 + (F.sick - 100) / 2 : F.sick > 30 ? (F.sick - 30) * 100 / 70 : 0;
  const pileRx = (6 + pileP * 84 / 100) * 1.6 * s;
  // and a long skirt or dress, which can reach past the feet
  const skirtHem = !suit && !undies && (P.style === "skirt" || P.style === "dress")
    ? L + (feetY - L) * (P.skirtLen ?? 50) / 100 + 30 : 0;
  const toesDown = suit || witch ? 0 : feetY + Math.max(footDown(worn), footDown(rightShoeOf(worn))) * shoeSc + 12;
  let bottom = Math.max(ground + 24, toesDown, ty + hairY(hr.bottom) * s + 16, ...handYs.map(y => y + reach), skirtHem, beardStyle ? ty + (headChin(F) + beardLong(beardStyle, state.hair) + (BEARDS[beardStyle].braid ? 60 : 16)) * s : 0,
                          ...heldDown, catK ? -9 + 46 * catK : 0, sasq ? feetY + 30 * C.sq.footK * state.shoes.size / 100 + 10 : 0);   // (ticket 530: its big feet)
  const hairHalf = hr.half * headWidth(F) * (1 + 0.12 * F.alien / 100) * s;
  let left = Math.min(-200, C ? -(w + 185) : -200, sasq && WC.sasquatchPrints !== false ? -(hw + 70 + 3 * 62 * C.sq.footK + 30) : -200, fairy ? -(w + 270) : -200, dragon ? -Math.max(w + 230, hw + 250) : -200, ...arms.map(x => x.H[0] - Math.max(40, reach)), ...arms.map(x => -x.H[0] - Math.max(40, reach)),
                        ...arms.filter(x => x.E).map(x => -Math.abs(x.E[0]) - armW - 20), knee ? -Math.abs(kneeAt(1)[0]) - B.legW - 30 : 0,
                        ...(legPosed ? legBones.flatMap(l => [-Math.abs(l.joints[1][0]) - B.legW - 30, -Math.abs(l.joints[2][0]) - 60 - Math.max(footReach(worn), footReach(rightShoeOf(worn))) * shoeSc]) : []),
                        -(fx + half + 70), -(fx + (witch ? FOOT_REACH.witchboot : Math.max(footReach(worn), footReach(rightShoeOf(worn)))) * shoeSc + 20), -(w + 40), -(pileRx + 20), -(B.neckW / 2 + 40), -(hairHalf + 20),
                        -((80 + (C ? (kind === "deer" ? 60 : 30) : witch ? brimRx - 80 : 0)) * headWidth(F) * (1 + 0.12 * F.alien / 100) * s + 20),
                        ...heldOut, western && W.H ? -(Math.min(1.5 * W.H.bw, Math.max(W.H.bw, hairReach(state.hair).half + 12)) * headWidth(F) * (1 + 0.12 * F.alien / 100) * s + 20) : 0,
                        catK ? -(w - 4) - 24 * catK : 0, catK ? -(-(w - 4) + 40 * catK) : 0,
                        robot ? -(w + robotOut(R, w, dir, false) + 16) : 0, RH ? -(RH.half * s + 20) : 0);   // (ticket 515: a robot's head, its blocks, plates, fins, key or legs)
  // (ticket 536) a moth's wings, wherever its arms take them (their outlines' points, and a little for the fringe and the
  // curves between them), and its antennae over its hood (stretched with the head, as the hood is)
  if (moth) { const P = arms.flatMap(arm => mothWingPts(arm, armW, dir ? 0.3 : 1, M)), { cy: hcy, ry: hry } = hoodGeom(F);
    top = Math.min(top, ...P.map(p => p[1] - 14), ty + (HEAD_C + (hcy - hry - MOTH_ANT_UP - 14 - HEAD_C) * headH(F) - flying) * s);
    bottom = Math.max(bottom, ...P.map(p => p[1] + 14)); left = Math.min(left, ...P.map(p => -Math.abs(p[0]) - 14)); }
  // (ticket 454, Matthew: "look into the dress-up-pictures for clipped pictures ... see if you can find any and what caused
  // them. Fix those if possible." The archive's 0.0.347 run had the head cut off at the frame's side lying down and crawling
  // in profile, and the top of the head cut jumping into a hug: the frame was the friend's standing one.) Turned about the
  // hips (crawling, swimming, lying: the turn drawNow gives them below, rot), the frame is the standing one's corners turned
  // the same way, as wide as they reach and as tall above the ground; held up into a hug, its top goes up by the lift.
  {
    const turn0 = move && move.rot ? move.rot : !move && B.pose === "lie" && state.background.kind === "none" ? (B.lieHead === "right" ? 90 : -90) : 0;
    // (ticket 544) the giraffe's neck and head, up over the hood, raise the frame's top as a hat does (faceTop), and reach out
    // to either side: its ears toward us and from behind, its muzzle ahead and its ear behind in profile (giraffeReach);
    // turned, its neck bends up from the body (below), so the frame is turned without them
    if (giraffe && !turn0) { const R = giraffeReach(C, F, { dir, away, squash }); top = Math.min(top, ty + (R.top - 10 - flying) * s); left = Math.min(left, -(R.side * s + 20)); }
    if (turn0) {
      const a = turn0 * Math.PI / 180, c = Math.cos(a), sn = Math.sin(a);
      const pts = [[left, top], [-left, top], [left, bottom], [-left, bottom]].map(([x, y]) => [x * c - (y - L) * sn, L + x * sn + (y - L) * c]);
      left = Math.min(left, -Math.max(...pts.map(p => Math.abs(p[0]))));
      top = Math.min(top, ground - (Math.max(...pts.map(p => p[1])) - Math.min(...pts.map(p => p[1]))));
      // (ticket 544) a giraffe's neck, turned, bends up from the body and its head stays upright (giraffeNeckSvg): its top, as
      // high over the turned frame's foot, and as far out, as it is drawn
      if (giraffe) { const G = giraffeGeom(C, F, { dir, away, rot: turn0, rise: (!move && B.pose === "lie") || !!(move && move.swim) }), bx = tx + G.base[0] * s, by = ty + G.base[1] * s;
        const hx = bx * c - (by - L) * sn + (G.end[0] - G.base[0]) * s, hy = L + bx * sn + (by - L) * c + (G.end[1] - G.base[1]) * s;
        top = Math.min(top, ground - (Math.max(...pts.map(p => p[1])) - (hy - G.up * s)) - 10); left = Math.min(left, -(Math.abs(hx) + G.out * s + 20)); }
    }
    if (B.pose === "hug" && !turn0) top -= LL * Math.max(0, Number(B.hugLift ?? 50) || 0) / 100;
  }
  // (ticket 438) the friend's horse, led by its rein, beside them: in profile behind them, its shoulder at theirs and its
  // head ahead of them (mirrored, facing our left); toward us at their right (our left), away at their right (our right).
  // Not while they sit, lie, swim, climb or crawl. The frame grows to hold it.
  const horseView = (B.horse || "none") !== "none" && !sitting && B.pose !== "lie" && !(move && (move.water || move.ladder != null || move.squash || move.rot))
    ? (dir ? "side" : away ? "back" : "front") : null;
  // (ridden: under them, the saddle's seat at their hips, and they are lifted onto it, rideLift, the frame's top with them)
  // (ticket 457, Matthew: "horse needs its own 'size compared to main friend' param, to adjust the scales of different
  // characters on the horse, for it to look right (e.g., small gnome on normal horse, next to other horses in stable scene)";
  // "we want that the 'default horse' and its scale looks right in the stable scene"; "test with a small gnome (size in each
  // scene is small)"; "after we have that refinement, adjusting the horse to look right on the character should be possible to
  // set just once, and then reuse in different scenes") its size, hs, in the friend's units: its own slider (horseSize, 100: as
  // big as the stable's horses, HORSE_SCALE, measured face to face at the stalls' depth) over the friend's own size (body.size,
  // bodyScale), so the place's size for people (figSize) sizes it, never the rider's own: a small gnome leaves their horse as
  // big as it was, in every place. Everything about it (where it stands, the seat, the reins, the frame) goes by hs.
  const horseHs = HORSE_SCALE * Math.max(0.2, Number(B.horseSize ?? 100) / 100) / bodyScale(), friendFrame = { left, top };
  // (ticket 496, Matthew: "Let the user choose between guide on left of horse, or guide on right -- currently it's not realistic
  // because we flip between those options depending on L versus R movement") led, the friend keeps to one side of the horse, its
  // left (the usual) or its right: seen from the front or behind, the horse stands at their other side; seen side on, on the
  // horse's far side the horse is drawn in front of them (leadFar)
  const leadR = (B.horseLead || "left") === "right";
  const leadFar = !ride && horseView === "side" && (leadR ? dir > 0 : dir < 0);
  // (on its far side they walk at its head, by its chest, under its neck: at its shoulder, as on the near side, its body hid them)
  const horseX = ride ? (horseView === "side" ? -dir * 30 * horseHs : 0) : horseView === "side" ? -dir * (leadFar ? 210 * horseHs : 130 * horseHs - 50) : (horseView === "front" ? -1 : 1) * (leadR ? -1 : 1) * (84 * horseHs + 66);
  const rideLift = ride && horseView ? ground - 326 * horseHs - L - 12 : 0;
  // (ticket 498: an armour's plume, or the dragon helm's horns, over its head)
  const plumed = (B.horseArmour || "none") !== "none" ? (horseView === "side" ? 80 : 50) : 0;
  if (horseView) { left = Math.min(left, -(Math.abs(horseX) + (horseView === "side" ? 310 : 100) * horseHs + 20)); top = Math.min(top + rideLift - 12, ground - ((horseView === "side" ? 520 : 570) + plumed) * horseHs); bottom = Math.max(bottom, ground + 16 * horseHs + 10); }   // (its shadow's reach)
  // A background is a stage of its own, the same whatever the figure
  // (SCENE): the frame the usual figure gets in a scene. The figure stands
  // on its ground by its feet, at its size and where it was dragged, so
  // changing the figure never moves it or the scenery (fitting the scene
  // to the figure did: a taller face slid the figure sideways). A figure
  // grown past the stage's edges is cut off there; its size slider brings
  // it back.
  const scened = state.background.kind !== "none";
  naturalFrame = { ...friendFrame, ground };   // (ticket 457: the friend's own, without their horse)
  if (scened) ({ left, top, bottom } = SCENE);
  const width = -2 * left;

  // VISIBILITY: each part of the body as see-through as its slider says
  // (100 all there, 0 gone). A part is drawn in layers between other
  // parts' (the shirt's arms under the hair, and again over the face):
  // each layer is see-through on its own, and masked where a later layer
  // of the same part covers it (see unveil), so a ghost's arm is no less
  // see-through where its two layers overlap. A layer is added from where
  // the picture's text was (`at`) to its end.
  const seen = visibility(), layers = {};
  // (a part following an LFO fades with its wave, never past its slider;
  // the animation picks up where the wave is now, so a redraw keeps time)
  const pic0 = document.getElementById("pic"), tNow = pic0 && pic0.getCurrentTime ? pic0.getCurrentTime() : 0;
  const veil = (g, inner) => {
    const a = seen[g], lf = visLfo(g);
    if (!inner) return inner;
    if (a >= 1 && !lf) return `<g data-part="${g}">${inner}</g>`;   // (ticket 377: marked, so a box pointed at in the panel lights its part here)
    if (a <= 0) return "";
    const n = layers[g] = (layers[g] ?? 0) + 1;
    let anim = "", shown = a;
    // (an LFO too fast to show: the part held at its average, LFO_BLUR)
    if (lf && lf.dur < LFO_BLUR) shown = a * (1 - lf.amp / 2);
    else if (lf) {
      const N = 16, vals = [], kt = [];
      for (let i = 0; i <= N; i++) { const u = i / N; vals.push((a * (1 - lf.amp * (1 - lfoWave(lf.shape, u)) / 2)).toFixed(3)); kt.push(u.toFixed(4)); }
      anim = `<animate attributeName="opacity" values="${vals.join(";")}" keyTimes="${kt.join(";")}" dur="${lf.dur.toFixed(4)}s" begin="${(tNow - lf.phase * lf.dur).toFixed(4)}s" repeatCount="indefinite"/>`;
    }
    return `<g opacity="${shown.toFixed(2)}" data-veil="${g} ${n}" data-part="${g}">${anim}<g id="veil-${g}-${n}">${inner}</g></g>`;
  };
  const veilFrom = (g, at) => { svg = svg.slice(0, at) + veil(g, svg.slice(at)); };
  const meanSeen = Object.values(seen).reduce((x, y) => x + y, 0) / VISIBLE_PARTS.length;

  let svg = `<defs>${royal ? patternDef("pat-ermine", "#f7f4ec", "ermine", 18, "#1f1f22") : ""}${patternDef("pat-shirt", SH.color, SH.pattern, SH.patternSize, SH.patternColor)}${patternDef("pat-pants", P.color, P.pattern, P.patternSize, P.patternColor)}` +
            `${patternDef("pat-undies", U.color, U.pattern, U.patternSize, U.patternColor)}${C ? furDefs(C, kind) : ""}</defs>`;
  // the background first, behind everything, its ground at the feet
  const BG = state.background;
  // (kept under the scene's own options, read as it was drawn: the
  // figure's sliders draw no scenery again; see part above)
  // (ticket 185) after the place, its things and the world square's ways out,
  // each a piece cached on what it holds: outside the place's own cache,
  // which compares the settings it read by identity, so a thing changed in
  // place (moved, turned, its leaves) would draw nothing new inside it
  const bgFull = scened ? partByReads(`scene:${BG.kind}`, sceneOpts(BG), o => backgroundSvg(BG.kind, left, top, -left, bottom, SCENE.ground, o)) +
                          thingsLayer({ o: BG, F: [left, top, -left, bottom, SCENE.ground] }) + worldWaysSvg(BG, left, top, -left, bottom, SCENE.ground) : null;
  const sceneStill = sceneryStill(), bgText = bgFull && sceneStill ? stillScene(bgFull) : bgFull;   // (ticket 164: held still on a slow computer)
  if (document.body.classList.contains("still-scenery") !== sceneStill) document.body.classList.toggle("still-scenery", sceneStill);
  // and in a scene, the figure's feet (its ground, g) on the stage's
  // ground (sg), at its own size about them, where it was dragged
  // (a joint held: the ground the friend stood on when it was grabbed)
  // (in water, the friend is in it: their waterline, the waist or in a
  // stream the knees, is where they stand on the water's surface, and what
  // is below it sinks into the water, see-through (wading, below))
  // (treading, they are in deeper: the water up to half an arm below the
  // shoulders, so even short arms sweep under it, never above the
  // shoulders, never below the waist; Matthew: the arms should be in the water)
  // (ticket 245: on a rope bridge, outside the tree house, high over the stream: never in it)
  if (scened) bridgeKeep();
  // (ticket 269: on a house's platform, on the way in from its ladder, as on a bridge)
  const onBr = scened ? bridgeNow() || platformNow() : null;
  // (ticket 287: swimming flat, the waterline move.float of the body's half width above the hips, the turn's middle;
  // swimming toward us, at the chin: the head and the arms coming over above the water, the rest under it)
  const swimming = !!(move && move.swim);
  const wet = scened && !onBr && !figOver && inWater(BG, BG.figX ?? 0, SCENE.ground + (BG.figY ?? 0)),
        wl = SHALLOW_WATER[BG.kind] ? (L + feetY) / 2 : swimming && crawlTo ? -20 + move.chin * armLen + 6 : swimming ? L - (move.float ?? 0) * w : tread ? Math.max(12, Math.min(L, 8 + 0.5 * armLen)) : L;
  const gStand = poseHeld && scened ? poseHeld.g : wet ? wl : ground;
  // (ticket 132) an animation lifts the friend (a step, a bound, a pull up
  // the ladder); crawling turns them about the hips (rot), and they rest on
  // the lowest of their hands, knees and feet as turned
  // (ticket 299: lying down, turned level about the hips, the head to our left or right, resting on the side below)
  // (ticket 412: lying along a sleeping bag, not turned level: turned to its slant about the feet, below)
  const lieAlong = !move && B.pose === "lie" && scened ? (seatNow() || {}).along || null : null;
  const rot = move && move.rot ? move.rot : !move && B.pose === "lie" && !lieAlong ? (B.lieHead === "right" ? 90 : -90) : 0;
  // (ticket 286: crawling, the friend meets things as drawn: turned about the hips from the side, or running back into
  // the picture toward us or away, the back that deep, seen short: crawlShape)
  Object.assign(poseJoints, { turn: rot, L, crawl: rot ? "side" : squash ? (away ? "away" : "toward") : null, depth: squash ? squash * (L + 20) : 0 });
  let gFig = gStand;
  if (rot && !swimming) {
    const a = rot * Math.PI / 180, turned = ([x, y]) => L + x * Math.sin(a) + (y - L) * Math.cos(a);
    gFig = Math.max(...arms.map(x => turned(x.H) + 10), ...[1, -1].map(sd => turned(kneeAt(sd)) + B.legW / 2), ...[1, -1].map(sd => turned(footAt(sd)) + 8),
                    B.pose === "lie" && !move ? L + w + 2 + (robot ? Math.max(robotOut(R, w, dir, true), RH.half * s - w) : 0) : -Infinity);   // (ticket 299: lying, on the body's back or side too, as thick as it is; ticket 515: a robot as wide as it reaches)
  } else if (move && move.lift && !wet && !poseHeld) gFig = gStand + move.lift;
  // (ticket 136) jumping up into a hug, they are held up off the ground
  // (their shadow stays on it); not in water, nor while a joint is held,
  // whose ground was kept as it was
  const hugLift = B.pose === "hug" && !rot && !wet ? LL * Math.max(0, Number(B.hugLift ?? 50) || 0) / 100 : 0;
  if (hugLift && !poseHeld) gFig += hugLift;
  // (ticket 287: swimming, turned inside the waterline, which stays level: below, in the water)
  let rotTf = rot && !swimming ? ` rotate(${rot} 0 ${f1(L)})` : "";
  // (seated, sized by the seat's depth, not by where their feet hang: ticket 50)
  const sizeSeat = scened ? seatNow() : null;
  // (ticket 331: out through a tree room's door to a bridge, in the picture seen through it: figOver, where and how big)
  // (ticket 245) on a rope bridge: their feet on its deck, as tall as BRIDGE_TALL of the tree's units (times their
  // slider's share); ticket 352: and their own height's against the default's (BRIDGE_BASE), as inside, not whatever their shape
  figureT = figOver && scened ? { k: figOver.k, dx: figOver.x, dy: figOver.y - SCENE.ground, g: gFig, sg: SCENE.ground }
          : onBr ? { k: figScale(BG) * BRIDGE_TALL * (onBr.W ? onBr.W.s * bridgeScale(onBr.W, onBr.u) : onBr.s * onBr.sc) / BRIDGE_BASE, dx: onBr.x, dy: onBr.y - SCENE.ground, g: gFig, sg: SCENE.ground }
          // (ticket 270: up the spiral staircase, their size the floor's straight below their step, not the screen's height)
          : scened ? { k: figScale(BG) * sceneFig(BG.kind) * depthSize(BG, sizeSeat ? (sizeSeat.kz ?? sizeSeat.z) - SCENE.ground : (BG.kind === "castleroom" && castleWallsOf(BG).stairs ? spiralFloorY(BG.figX ?? 0, BG.figY ?? 0) : null) ?? BG.figY ?? 0), dx: BG.figX ?? 0, dy: BG.figY ?? 0, g: gFig, sg: SCENE.ground }
                   : { k: 1, dx: 0, dy: 0, g: gFig === gStand ? ground : gFig + ground - gStand, sg: ground };
  figureT.base = Math.max(1, ground - neckTop + 90);   // (ticket 331: their height standing, in their own units, that a size in the picture's divides)
  sceneFrame = { left, top, right: -left, bottom, ground: figureT.sg, back: scened && SCENE_BACK[BG.kind] ? SCENE_BACK[BG.kind](top, bottom) : -Infinity };
  // (ticket 137) climbing a place's ladder: up it as time goes by (not while
  // dragged), at its depth (their feet are sorted there still), their
  // shadow left at its foot; to the top, or, up a doorway, till their head
  // is through. A new place ends a climb (and climbing, if it went through)
  if (climb && climb.place !== climbPlace(BG)) { if (climb.through) B.anim = "none"; climb = null; }
  let climbUp = 0;
  if (climb && !climb.none && B.anim === "climbup" && scened && !poseHeld && !drag) {
    const tall = (ground - neckTop + 90) * figureT.k, all0 = Math.max(0.4 * (climb.z - climb.top), climb.z - climb.top - (climb.door ? 0.55 * tall : climb.hands ? 0.85 * tall : 0));
    // (ticket 252: up through a ceiling's opening, on till the feet are past it, cut off above it as they go: no fade)
    const all = climb.clipUp != null && climb.door ? Math.max(all0, climb.z - climb.clipUp + 6) : all0;
    // (ticket 145: by hand, as far as the controller has taken them; through a doorway at its top)
    // (ticket 199: down a hatch's ladder, the lift goes below where they started, till they are out of sight, then through)
    const got = climb.down ? Math.min(0, climb.got) : climb.manual ? clamp(climb.got, 0, all) : Math.min(all, (performance.now() - climb.start) / 1000 * CLIMB_SPEED * figureT.k);
    climb.all = all; climb.at = got;
    if (climb.down) { if (got <= -tall && climb.door && !climb.going) { climb.going = true; setTimeout(climbThrough, 100); } }
    else if (climb.manual) { if (got >= all && climb.door && !climb.going) { climb.going = true; setTimeout(climbThrough, 250); } }
    else if (got >= all && !climb.done) { climb.done = true; if (climb.door) setTimeout(climbThrough, 250); }
    climbUp = got / figureT.k;
    figureT.g += climbUp;
  }
  svg += `<g id="figure" transform="translate(${f1(figureT.dx)} ${f1(figureT.dy + figureT.sg)}) scale(${figureT.k.toFixed(3)}) translate(0 ${f1(-figureT.g)})${rotTf}">`;
  // (ticket 42) on the tree house's swing they swing with its seat: the same
  // rotation about the same hook, at the same time, added to where they are
  // (the hook in their own units); not while a joint is held
  if (sizeSeat && sizeSeat.name === "swing" && BG.kind === "treehouse" && !poseHeld) {
    const ts = sizeSeat.x0 / 358, hx = 395 * ts, hy = sizeSeat.y - 274 * ts;
    const lx = (hx - figureT.dx) / figureT.k, ly = (hy - figureT.dy - figureT.sg) / figureT.k + figureT.g, at = `${f1(lx)} ${f1(ly)}`;
    svg += lfoTimed(BG, "slow", 0.15, `<animateTransform attributeName="transform" type="rotate" additive="sum" values="-14 ${at};14 ${at};-14 ${at}" dur="2.6s" repeatCount="indefinite" calcMode="spline" keySplines="0.45 0 0.55 1;0.45 0 0.55 1"/>`);
  }
  if (meanSeen > 0 && !wet && !rot && !(ride && horseView)) svg += `<ellipse cx="0" cy="${f1(ground + hugLift + climbUp + (move && move.jump && !wet ? move.lift : 0))}" rx="${f1(hw + 70)}" ry="12" fill="#000" opacity="${(0.08 * meanSeen).toFixed(3)}"/>`;
  // (ticket 132) climbing: a ladder behind the friend, its rungs going down two a stride
  // (ticket 137: seen from the side, the ladder stands in front of them,
  // side on: its two rails nearly one behind the other, the far one a
  // little farther on and darker, and each rung seen end on, a short stub
  // between them)
  if (move && move.ladder != null && dir) {
    const x = dir * (hw + 20), x2 = x + dir * 9, top = -380, gap = 48, off = (move.ladder * 2 * gap) % gap;
    let t = `<g class="ladder"><line x1="${f1(x2)}" y1="${top}" x2="${f1(x2)}" y2="${f1(ground + 6)}" stroke="#5e3f24" stroke-width="7" stroke-linecap="round"/>`;
    for (let y = top + off; y < ground; y += gap) t += `<line x1="${f1(x)}" y1="${f1(y)}" x2="${f1(x2)}" y2="${f1(y)}" stroke="#9c6b3d" stroke-width="7" stroke-linecap="round"/>`;
    svg += t + `<line x1="${f1(x)}" y1="${top}" x2="${f1(x)}" y2="${f1(ground + 6)}" stroke="#7a5230" stroke-width="9" stroke-linecap="round"/></g>`;
  } else if (move && move.ladder != null) {
    const x = hw + 26, top = -380, gap = 48, off = (move.ladder * 2 * gap) % gap;
    let t = `<g class="ladder"><line x1="${f1(-x)}" y1="${top}" x2="${f1(-x)}" y2="${f1(ground + 6)}" stroke="#7a5230" stroke-width="9" stroke-linecap="round"/>` +
            `<line x1="${f1(x)}" y1="${top}" x2="${f1(x)}" y2="${f1(ground + 6)}" stroke="#7a5230" stroke-width="9" stroke-linecap="round"/>`;
    for (let y = top + off; y < ground; y += gap) t += `<line x1="${f1(-x)}" y1="${f1(y)}" x2="${f1(x)}" y2="${f1(y)}" stroke="#9c6b3d" stroke-width="7" stroke-linecap="round"/>`;
    svg += t + "</g>";
  }
  // (ticket 530) a Sasquatch's footprints on the ground, standing still on it (not moving, sitting, lying, in water or riding)
  if (sasq && WC.sasquatchPrints !== false && !move && !rot && !wet && !ride && !sitting && (B.pose || "stand") === "stand") svg += sasquatchPrints(C, hw, ground, dir, C.sq.footK);
  // (ticket 438) the horse, before the friend: its own shadow, then it, stepping with their stride
  let horseRing = null, horseBits = null, horseOver = "", horseFront = "", lift = 0;
  horseNow = null;
  if (horseView && !wet) {
    const gait = B.anim === "walk" ? "walk" : B.anim === "run" ? "run" : "stand", k = horseView === "side" ? dir : 1;
    const h = horseSvg(horseView, gait, gait === "stand" ? 0 : lfoNow("B").phase,
                       { coat: B.horseCoat, mane: B.horseMane, mark: B.horseMark, tack: B.horseTack, socks: B.horseSocks, shoes: B.horseShoes, markColor: B.horseMarkColor, saddle: B.horseSaddle, blanket: B.horseBlanket, maneStyle: B.horseManeStyle, bows: B.horseBows,
                         armour: B.horseArmour, arms: knight ? WC : null }, 0, ride);   // (ticket 498: its armour, in the knight's arms while they are a knight)
    // (drawn on the ground at 0, scaled by hs about its hooves; its points into the friend's units, T)
    const hs = horseHs, T = ([x, y]) => [horseX + k * hs * x, ground + hs * y], at = `translate(${f1(horseX)} ${f1(ground)}) scale(${(k * hs).toFixed(4)} ${hs.toFixed(4)})`;
    const horseG = `<g class="horse" data-horse="${horseView} ${gait}" data-size="${hs.toFixed(3)}"${leadFar ? ' data-far=""' : ""}><ellipse cx="${f1(horseX - k * hs * (horseView === "side" ? 20 : 0))}" cy="${f1(ground)}" rx="${f1((horseView === "side" ? 225 : 100) * hs)}" ry="${f1(14 * hs)}" fill="#000" opacity="0.08"/>` +
           `<g transform="${at}">${h.svg}</g></g>`;
    if (leadFar) horseFront = horseG; else svg += horseG;   // (ticket 496: on its far side, the horse after the friend, over them)
    horseRing = h.ring && T(h.ring);
    horseNow = { view: horseView, ride: !!ride, far: leadFar, X: horseX, k, hs, hooves: h.hooves || [], barrel: h.barrel, seat: -h.seat[1] };   // (ticket 492; far: ticket 496)
    if (ride) { lift = rideLift + h.bob * hs; horseBits = h.bits.map(b => { const [x, y] = T(b); return [x, y - lift]; });
      if (h.over) horseOver = `<g class="horse-over" transform="${at}">${h.over}</g>`; }
  }
  poseJoints.lift = lift;   // (ticket 499: riding, how far they are lifted onto the saddle, for where their mouth and hands are now)
  const figAt = svg.length;
  // behind everything: long hair's back layer, in the face's frame
  if (!C && !diver && !hooded && !dir && !away) svg += veil("hair", `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})">` +
                 `<g transform="${topFlies(F)}"><g transform="${hairStretch(F)}">${tucked(hairBackSvg(state.hair), "under-hat-back")}</g></g></g>`);

  // behind everything: a falcon's wings and tail, or a suit's tail
  // (a bald eagle's: plain dark brown wings and a white tail)
  // (wings go with the shirt, the body; a tail with the pants, the hips)
  // (ticket 124: in profile, wings on the back are seen turned away from us,
  // in perspective: the near wing, the half that spreads behind them,
  // narrowed to under half, out from the back of the shoulders, and the far
  // one a little behind and above it, shaded; the half that spread toward
  // the face is hidden)
  const turnedAway = t => {
    if (!dir) return t;
    const half = `<clipPath id="wings-back"><rect x="${dir > 0 ? -4000 : 0}" y="-4000" width="4000" height="8000"/></clipPath><g clip-path="url(#wings-back)">${t}</g>`;
    return `<g transform="translate(${f1(-dir * (w * 0.55 + 10))} -10) scale(0.42 1)" opacity="0.8">${half.replace('id="wings-back"', 'id="wings-back-far"').replace("url(#wings-back)", "url(#wings-back-far)")}</g>` +
           `<g transform="translate(${f1(-dir * w * 0.55)} 0) scale(0.42 1)">${half}</g>`;
  };
  const backAt = svg.length;
  let tail = "";
  if (kind === "falcon") { svg += veil("shirt", turnedAway(falconWings(C, w))); tail = falconTail(C, hw, L, LL); }
  else if (kind === "eagle") { svg += veil("shirt", turnedAway(falconWings(C, w, C.fur))); tail = falconTail(C, hw, L, LL, C.belly); }
  else if (kind === "falconsuit") tail = falconTail(C, hw, L, LL, "url(#pat-fur)");
  else if (kind === "eaglesuit") tail = falconTail(C, hw, L, LL, C.belly);
  // (ticket 499) a dragon's bat wings behind the shoulders (over the back, from behind; seen short with it, crawling away, as
  // the tank is) and its long tail, each view drawn for itself
  // (review: crawling or swimming in profile, turned about the hips, each has a drawing of its own laid out in the picture,
  // the tail along the ground there, the floor (gFig, as turned) or just under the water's top)
  else if (dragon) { const sideTurn = dir && move && move.rot && (move.swim || B.anim === "crawl") ? move.rot : 0;
    const wg = dragonWingsSvg(C, WC, w, L, dir, away, sideTurn); svg += veil("shirt", squash && away ? squashed(wg) : wg); tail = dragonTailSvg(C, hw, L, ground, dir, away,
    !move && B.pose === "lie" && rot ? { lie: Math.sign(rot), w, LL } : crawlTo ? { rise: -backK * L - 20 * (1 + backK) } : sideTurn ? { turn: sideTurn, floor: swimming ? wl + 28 : gFig } : null); }
  // (a suit's tail swings out to our right; in profile, out behind them)
  else if (suit) { const tl = [C, kind, hw, L, rot ? Infinity : ground, giraffe && !move && B.pose === "lie" && rot ? { side: (dir > 0 ? -1 : 1) * Math.sign(rot), LL } : null];
    tail = dir > 0 ? `<g transform="scale(-1 1)">${tailSvg(...tl)}</g>` : tailSvg(...tl); }   // (ticket 544: a giraffe's tuft no lower than the ground; lying down, along it)
  else if (fairy) svg += veil("shirt", turnedAway(fairyWingsSvg(fWing, fWingCol, w, L, WC.wingFlutter !== false, WC.wingLfo)));
  // (ticket 515) a robot's back: behind them facing us, over the back from behind (onBack); crawling away, its own drawing
  // over the back seen from behind and above (robotCrawlBack); crawling toward us only what rises from behind the back (the
  // spider's legs; the rest is its own drawing over the back, below); walking or running, the tin's key turns (its three
  // drawings by the stride's phase); in the air, the space bot's flames; crawling or swimming in profile, the spider's legs
  // step by LFO B's phase
  const robotTurn = robot && dir && move && move.rot && (move.swim || B.anim === "crawl") ? move.rot : 0;
  const robotBackO = robot ? { stride: move && !move.water && (B.anim === "walk" || B.anim === "run") ? Math.floor(lfoNow("B").phase * 3) % 3 : 0,
                               jump: !!(move && move.jump && (move.lift || 0) > 0 && !move.water), sitting: sitting || B.pose === "meditate", lying, lieBack, crawlTo,
                               crawl: crawlTo ? { to: backK } : squash && away ? { away: squash } : null,
                               turn: robotTurn, phase: robotTurn ? (lfoNow("B") || {}).phase || 0 : 0, floor: swimming ? wl + 28 : gFig } : null;
  if (robot) { const rb = rbG("back", robotBack(R, w, L, dir, away, robotBackO)); if (rb) svg += veil("shirt", rb); }
  // (tickets 436, 437) a royal mantle from the shoulders to the floor, behind them (over them from behind: as wings are), its
  // ermine hem; in profile it hangs behind the back
  if (royal && WC.mantle !== false) { const mc = mix(robeC, "#000000", 0.18), fl = feetY + 8, b = dir ? -dir * (w * 0.6) : 0, sp = dir ? 0.5 : 1;
    svg += veil("shirt", `<path d="M${f1(b - (w + 6))} -14 Q${f1(b - (w + 26) * sp)} ${f1(L)} ${f1(b - (w + 50) * sp)} ${f1(fl)} L${f1(b + (w + 50) * sp)} ${f1(fl)} Q${f1(b + (w + 26) * sp)} ${f1(L)} ${f1(b + w + 6)} -14 Z" fill="${mc}" stroke="${darker(mc)}" stroke-width="2.5" stroke-linejoin="round"/>` +
      `<rect x="${f1(b - (w + 50) * sp)}" y="${f1(fl - 16)}" width="${f1(2 * (w + 50) * sp)}" height="16" fill="url(#pat-ermine)" stroke="#c9c5bd" stroke-width="1.5"/>`); }
  // (ticket 286: crawling toward us, the tail goes up from behind the back, turned over with it)
  if (tail) svg += veil("pants", crawlTo && !dragon ? flipped(tail) : tail);   // (ticket 499: a dragon's, crawling toward us, drawn rising)
  // (ticket 288: an air tank on the back; ticket 371, Matthew: "crawling animation for scuba tank has it on our belly --
  // inverted from correct": crawling toward us, it lies on the back, which rises behind the head: drawn over the back, below;
  // crawling away, seen short with the back it lies on)
  if (WC.tank && !diver && !crawlTo) { const tk = tankSvg(WC, w, L, dir); svg += veil("shirt", squash && away ? squashed(tk) : tk); }
  // (ticket 138: facing away, the wings and the tail are drawn last, over them)
  const onBack = away ? svg.slice(backAt) : "";
  if (away) svg = svg.slice(0, backAt);

  // legs, then shoes, then pants over both: long pants cover a boot's top
  // (the legs are the pants' to see through: invisible pants are
  // invisible legs, never bare ones)
  const legsAt = svg.length;
  // (in profile, the far leg first, then the one on our side)
  const legOrder = dir ? [dir, -dir] : [1, -1];
  // (ticket 136, a hug) in a place, turned sideways and not in water, the
  // far leg (its skin, stocking, shoe and pants leg) is drawn apart with the
  // far arm (farDeep, below), a body's depth behind their feet: so a leg can
  // go behind someone they hug while the near one is in front. Otherwise
  // both legs are drawn here, exactly as before
  const feetOver = legPosed && [1, -1].some(side => footAt(side)[1] <= kneeAt(side)[1] + 12);
  const legApart = scened && dir && !wet && !feetOver;
  const legSides = legApart ? [-dir] : legOrder, farLeg = { skin: "", shoe: "", pants: "" };
  const legSkin = side => limbThrough(legJoints(side), B.legW, limb, limbLine);
  // (striped stockings)
  const stocking = side => legJoints(side).length > 2 ? `<path d="M${legJoints(side).map(([x, y]) => f1(x) + " " + f1(y)).join(" L")}" fill="none" stroke="${WC.trimColor}" stroke-width="${f1(B.legW)}" stroke-dasharray="9 9"/>`
                  : `<line x1="${f1(side * fx)}" y1="${f1(L)}" x2="${f1(side * fx)}" y2="${f1(feetY)}" stroke="${WC.trimColor}" ` +
             `stroke-width="${f1(B.legW)}" stroke-dasharray="9 9"/>`;
  for (const side of legSides) svg += legSkin(side);
  if (witch) for (const side of legSides) svg += stocking(side);
  if (legApart) farLeg.skin = legSkin(dir) + (witch ? stocking(dir) : "");
  veilFrom("pants", legsAt);
  const feet = BIRD_SUIT[kind] ? { style: "talons", size: state.shoes.size }
             : kind === "deer" || giraffe ? { style: "hoof", size: state.shoes.size }   // (ticket 544: a giraffe's hooves too)
             : scuba ? { style: "fin", size: state.shoes.size, color: WC.scubaAccent }
             : diver ? { style: "boot", size: state.shoes.size * 1.25, color: "#3f3a33" }
             : witch ? { style: "witchboot", size: state.shoes.size, color: WC.bootColor ?? "#1f1f22" }
             : gnome ? { style: "boot", size: state.shoes.size, color: WC.gnomeBoots || "#e8e4da" }
             : wizard ? { style: "boot", size: state.shoes.size, color: "#3b2f4a" }
             // (ticket 452, Matthew: "druid should not have boots, but should use whatever feet params are there for the character
             // -- also, give a toggle for 'bare feet', where we ignore those params while wearing the druid") the monk or dryad: the
             // friend's own feet, or bare (their shoes hidden in the cloak: the story of the costume)
             : monk ? (WC.bareFeet ? { style: "bare", size: state.shoes.size } : state.shoes)
             : robin ? { style: "boot", size: state.shoes.size, color: "#6b4226" }
             : knight ? { style: "boot", size: state.shoes.size, color: "#6b7280" }
             : royal ? { style: "boot", size: state.shoes.size, color: "#e0b030" }
             : western ? westernFeet(W, state.shoes)   // (ticket 522: cowboy boots)
             : ninja ? ninjaBoots   // (ticket 557)
             : robot ? robotFeet(R, state.shoes)   // (ticket 515: boots in its colour, the mech's bigger; the box bot's the friend's own)
             : fairy ? { style: "fairy-" + (FAIRY_SHOES.some(([v]) => v === WC.fairyShoes) ? WC.fairyShoes : "slipper"), size: state.shoes.size, color: WC.fairyShoeColor || FAIRY_SHOE_COLORS[0] }
             : suit ? { style: "paw", size: state.shoes.size * (sasq ? C.sq.footK : 1), color: C.fur, pad: C.accent, claws: dragon ? C.horn : null } : state.shoes;   // (ticket 530: a Sasquatch's big feet)   // (ticket 499: a dragon's feet clawed)
  // (a falcon suit's talons are yellow, like a falcon's feet)
  // (with different feet, the friend's right foot, on our left, wears its own)
  const feetAt = svg.length;
  // (ticket 129) each foot turns about its ankle, as a hand does: both by
  // "👣 feet turn" (toes up +), and each by the pose's own turn, by side
  const shoeOf = side => {
    const [ax, ay] = footAt(side), turn = -(dir || side) * ((Number(B.feetTurn) || 0) + poseT("Foot", side));   // (an animation's too: ticket 132)
    const own = feet === state.shoes && side < 0 ? rightShoeOf(feet) : feet;
    // (ticket 140: walking toward us or away, the friend's own sneakers or bare feet seen front on, or from behind)
    if (move && move.frontFeet && !dir && feet === state.shoes && FRONT_FEET[own.style])
      return footFront(ax, ay, side, own, skin, away, Math.min(1, poseT("Step", side) / move.frontFeet));
    const shoe = shoeSvg(ax, ay, dir || side, own, BIRD_SUIT[kind] ? C.accent : skin);
    return turn ? `<g transform="rotate(${f1(turn)} ${f1(ax)} ${f1(ay)})">${shoe}</g>` : shoe;
  };
  for (const side of legSides) if (!legOf(side).behind) svg += shoeOf(side);
  if (legApart) farLeg.shoe = shoeOf(dir);
  veilFrom("feet", feetAt);
  // (ticket 414: a foot behind its leg, the knee curled back, seen from the front: its shoe under the leg and the pants)
  const behindShoes = legSides.filter(side => legOf(side).behind).map(shoeOf).join("");
  // (ticket 129) a foot resting up on the other leg, as cross-legged, is
  // drawn over the pants, after them (feetOver, above)
  const feetLater = feetOver ? svg.slice(feetAt) : "";
  if (feetOver) svg = svg.slice(0, feetAt);
  // (ticket 414: after that, which cuts the picture's text at feetAt: put in before it, these moved feetAt and lost the body)
  if (behindShoes) svg = svg.slice(0, legsAt) + veil("feet", behindShoes) + svg.slice(legsAt);
  const pf = suit ? "url(#pat-fur)" : fillOf("pat-pants", P.color, P.pattern);
  const ps = suit ? darker(C.fur) : darker(P.color);
  const pantsAt = svg.length;
  if (undies) {
    // no clothes: plain skin from the waist to the tops of the legs, and
    // over it the underwear or swimsuit bottom
    // (ticket 128: on posed or bent legs, both follow each thigh, as pants
    // do; straight, they are drawn exactly as before)
    const fitted = legPosed || knee;
    // a point t down a leg from its hip (no farther than its knee), and the
    // way out across the thigh there
    const legFrame = (side, t) => {
      const J = legJoints(side), K = kneeAt(side), u = Math.min(t, LL / 2) / (LL / 2);
      const dx = K[0] - J[0][0], dy = K[1] - J[0][1], l = Math.hypot(dx, dy) || 1;
      return { p: [J[0][0] + dx * u, J[0][1] + dy * u], n: [side * dy / l, -side * dx / l] };
    };
    // facing us: an outline from the waist down each thigh to len, w half
    // as wide there, up to the crotch (rising as the thighs turn up)
    const shortLegs = (len, crotch, w, top, waist, wIn = w, lenIn = len) => {
      const td = Math.max(0.2, Math.min(...[1, -1].map(side => (kneeAt(side)[1] - L) / (LL / 2))));
      const edge = (side, t, ww) => { const { p, n } = legFrame(side, t); return [p[0] + n[0] * ww, p[1] + n[1] * ww]; };
      return [[-waist, top], [waist, top], edge(1, len, w), edge(1, lenIn, -wIn), [0, L + crotch * td], edge(-1, lenIn, -wIn), edge(-1, len, w)];
    };
    // in profile: the hips, rounded at the seat, and a band down each thigh
    // to len, the far one first, as profile pants are drawn
    const hipsSide = (fill, line, tall = half * 1.4) => `<rect x="${f1(-hw - 2)}" y="${f1(L - 10)}" width="${f1(2 * hw + 4)}" height="${f1(10 + tall)}" rx="${f1(Math.min(half * 0.7, (10 + tall) / 2))}" fill="${fill}"` +
                                                       (line ? ` stroke="${line}" stroke-width="2.5"/>` : `/>`);
    const sideLegs = (len, w, fill, line) => { const band = side => { const { p } = legFrame(side, len), H = legJoints(side)[0];
        const d = `M${f1(H[0])} ${f1(H[1])} L${f1(p[0])} ${f1(p[1])}`;
        return `<path d="${d}" fill="none" stroke="${line}" stroke-width="${f1(2 * w + 5)}"/><path d="${d}" fill="none" stroke="${fill}" stroke-width="${f1(2 * w)}"/>`; };
      if (legApart) farLeg.pants += band(dir);   // (ticket 136: the far one apart)
      return legSides.map(band).join(""); };
    svg += !fitted ? poly([[-hw, L - 10], [hw, L - 10], [fx + half, L + 34], [fx - half, L + 34], [0, L + 14],
                           [-(fx - half), L + 34], [-(fx + half), L + 34]], limb, limbLine)
         : dir ? hipsSide(limb, limbLine)
         : poly(shortLegs(34, 14, half, L - 10, hw), limb, limbLine);
    svg += undiesBottom(U, hw, fx, half, L, LL, fitted ? { dir, shortLegs, hipsSide, sideLegs } : null);
  } else if (!suit && (P.style === "skirt" || P.style === "dress")) {
    // a skirt flares a little; a dress flares more as it gets longer and
    // has a scalloped hem. The length is a share of the legs' length.
    const sl = (feetY - L) * (P.skirtLen ?? 50) / 100;
    const robe = P.style === "dress" && (monk || wizard || king);
    // (ticket 546) its flare at the hem, by its style (below); and, where posed legs reach out past its straight sides above
    // the hem (riding, crouching, running), the cloth over them (skirtOver): its outline from the hem's left end round to
    // its right end, the hem between them as wide (xL to xR, its middle xm); null, drawn as it always was
    const flare = P.style === "skirt" ? hw + 30 : robe ? hw + 6 + sl * 0.05 : hw + 26 + sl * 0.25;
    const over = legPosed || knee ? skirtOver([1, -1].map(legJoints), B.legW / 2 + 4, hw, L - 10, flare, L + sl) : null;
    const xL = over ? over.xL : -flare, xR = over ? over.xR : flare, xm = (xL + xR) / 2;
    const sides = over ? "M" + over.side.map(([x, y]) => f1(x) + " " + f1(y)).join(" L") : `M${-hw} ${L - 10} L${hw} ${L - 10} L${f1(flare)} ${f1(L + sl)}`;
    if (P.style === "skirt")
      svg += `<path d="${sides} Q${f1(xm)} ${f1(L + sl + 14)} ${f1(xL)} ${f1(L + sl)} Z" ` +
             `fill="${pf}" stroke="${ps}" stroke-width="2.5" stroke-linejoin="round"/>` + (western ? westernHem(W, hw, L, sl, xL, xR) : "");   // (ticket 522: the rodeo star's fringe)
    // (ticket 450, Matthew: "less bell shaped robe at the ankle/feet area -- it's a robe, not a dress") a robe (the monk's or
    // dryad's, the wizard's, the king's) falls nearly straight from the waist to a straight hem, its weight against the body
    else if (robe) {
      const fl = flare, hem = L + sl;
      svg += `<path d="${sides} Q${f1(xm)} ${f1(hem + 8)} ${f1(xL)} ${f1(hem)} Z" fill="${pf}" stroke="${ps}" stroke-width="2.5" stroke-linejoin="round"/>` +
             `<path d="M${f1(xL + 6)} ${f1(hem - 8)} Q${f1(xm)} ${f1(hem)} ${f1(xR - 6)} ${f1(hem - 8)}" fill="none" stroke="${ps}" stroke-width="2" opacity="0.45"/>` +
             [-0.45, 0.45].map(k => `<path d="M${f1(k * hw)} ${f1(L + 0.2 * sl)} L${f1(over ? xm + k * (xR - xL) * 0.51 : k * fl * 1.02)} ${f1(hem - 6)}" stroke="${ps}" stroke-width="2" opacity="0.3"/>`).join("");
    }
    else {
      const fl = flare, n0 = witch ? 9 : fairy ? 7 : 6;
      // (ticket 546: a hem made wider over the legs keeps its scallops, points or petals about as wide as they were)
      const n = over ? Math.max(n0, Math.round(n0 * (xR - xL) / (2 * fl))) : n0;
      // (a toadstool's white frill peeps out under its hem)
      // (ticket 522: and a prairie dress's petticoat, 7 lower, so it reads as a ruffle)
      if ((fairy && fTunic === "toadstool") || (western && W.S.ruffle)) { const fy = L + sl + (fairy ? 0 : 7);
        let fr = `M${f1(xL - 6)} ${f1(fy - 6)}`;
        for (let i = 0; i < 12; i++) { const xa = over ? xL - 6 + (xR - xL + 12) * i / 12 : -fl - 6 + (2 * fl + 12) * i / 12, xb = over ? xL - 6 + (xR - xL + 12) * (i + 1) / 12 : -fl - 6 + (2 * fl + 12) * (i + 1) / 12;
          fr += ` Q${f1((xa + xb) / 2)} ${f1(fy + 26)} ${f1(xb)} ${f1(fy - 6)}`; }
        svg += `<path d="${fr} Z" fill="#fffaf0" stroke="#d6cfc0" stroke-width="2"/>`;
      }
      let d = sides;
      for (let i = 1; i <= n; i++) {
        const x0 = over ? xR - (i - 1) * (xR - xL) / n : fl - (i - 1) * 2 * fl / n, x1 = over ? xR - i * (xR - xL) / n : fl - i * 2 * fl / n;
        // (a witch's hem is ragged: points, not scallops; a fairy's is her
        // tunic's: petals, leaves, bells, a sparkly zigzag, a toadstool's)
        d += witch ? ` L${f1((x0 + x1) / 2)} ${f1(L + sl + 18)} L${f1(x1)} ${f1(L + sl)}`
           : fairy ? fairyHem(fTunic, x0, x1, L + sl)
                   : ` Q${f1((x0 + x1) / 2)} ${f1(L + sl + 16)} ${f1(x1)} ${f1(L + sl)}`;
      }
      svg += `<path d="${d} Z" fill="${pf}" stroke="${ps}" stroke-width="2.5" stroke-linejoin="round"/>`;
      // a few webs on the skirt: two side by side, a smaller one below
      if (fewWebs)
        svg += spiderWeb(-0.5 * fl, L + 0.42 * sl, 0.24 * fl, WC.webColor) + spiderWeb(0.5 * fl, L + 0.42 * sl, 0.24 * fl, WC.webColor) +
               spiderWeb(0, L + 0.8 * sl, 0.18 * fl, WC.webColor);
    }
  } else {
    const len = !suit && P.style === "shorts" ? LL * 0.38 : LL;
    // (bent legs: each pants leg follows its thigh and, past the knee, its
    // shin, as wide across as it is when straight)
    const crotch = Math.min(len * 0.3, 40) * Math.cos(knee), wd = half / Math.cos(knee);
    const legEdge = (side, t) => { const [hx, hy] = [side * fx, L], [kx, ky] = kneeAt(side);
      return t <= LL / 2 ? [hx + (kx - hx) * t / (LL / 2), hy + (ky - hy) * t / (LL / 2)] : [kx + (side * fx - kx) * (t - LL / 2) / (LL / 2), ky + (feetY - ky) * (t - LL / 2) / (LL / 2)]; };
    const legSide = side => { const end = legEdge(side, len), mid = len > LL / 2 ? [kneeAt(side)] : [];
      return { out: [...mid.map(([x, y]) => [x + side * wd, y]), [end[0] + side * wd, end[1]]], in: [[end[0] - side * wd, end[1]], ...mid.map(([x, y]) => [x - side * wd, y])] }; };
    // (ticket 121: legs posed on their own, each pants leg along its own
    // thigh and shin, as wide across them as when straight, mitred round
    // the knee; the crotch rises as the thighs turn up)
    const along = (J, t) => t <= LL / 2 ? [J[0][0] + (J[1][0] - J[0][0]) * t / (LL / 2), J[0][1] + (J[1][1] - J[0][1]) * t / (LL / 2)]
                                        : [J[1][0] + (J[2][0] - J[1][0]) * (t - LL / 2) / (LL / 2), J[1][1] + (J[2][1] - J[1][1]) * (t - LL / 2) / (LL / 2)];
    const outward = (side, P0, P1) => { const dx = P1[0] - P0[0], dy = P1[1] - P0[1], l = Math.hypot(dx, dy) || 1; return [side * dy / l, -side * dx / l]; };
    const posedSide = side => { const J = legJoints(side), end = along(J, len), n1 = outward(side, J[0], J[1]), n2 = outward(side, J[1], J[2]);
      const bentOn = len > LL / 2, d = Math.max(0.3, 1 + n1[0] * n2[0] + n1[1] * n2[1]), m = [(n1[0] + n2[0]) * half / d, (n1[1] + n2[1]) * half / d], ne = bentOn ? n2 : n1;
      return { out: [...(bentOn ? [[J[1][0] + m[0], J[1][1] + m[1]]] : []), [end[0] + ne[0] * half, end[1] + ne[1] * half]],
               in: [[end[0] - ne[0] * half, end[1] - ne[1] * half], ...(bentOn ? [[J[1][0] - m[0], J[1][1] - m[1]]] : [])] }; };
    const thighsDown = Math.max(0.2, Math.min(...[1, -1].map(side => (legJoints(side)[1][1] - L) / (LL / 2))));
    const legs = legPosed ? [[-hw, L - 10], [hw, L - 10], ...posedSide(1).out, ...posedSide(1).in, [0, L + Math.min(len * 0.3, 40) * thighsDown],
                             ...posedSide(-1).in.slice().reverse(), ...posedSide(-1).out.slice().reverse()]
      : !knee ? [[-hw, L - 10], [hw, L - 10], [fx + half, L + len], [fx - half, L + len],
                          [0, L + Math.min(len * 0.3, 40)], [-(fx - half), L + len], [-(fx + half), L + len]]
      : [[-hw, L - 10], [hw, L - 10], ...legSide(1).out, ...legSide(1).in, [0, L + crotch],
         ...legSide(-1).in.slice().reverse(), ...legSide(-1).out.slice().reverse()];
    if (dir) {
      // (ticket 124) in profile: a pants leg along each thigh and shin, as
      // wide across as when straight, the far one first; then the hips over
      // their tops, rounded at the seat, and the hips again without their
      // outline, so no seam shows where the legs come out
      const hips = (line) => `<rect x="${f1(-hw - 2)}" y="${f1(L - 10)}" width="${f1(2 * hw + 4)}" height="${f1(10 + half * 1.4)}" rx="${f1(half * 0.7)}" fill="${pf}"` +
                             (line ? ` stroke="${ps}" stroke-width="2.5"/>` : `/>`);
      let t = hips(true);
      const pantsLeg = side => { const J = legJoints(side), end = along(J, len), Q = len > LL / 2 ? [J[0], J[1], end] : [J[0], end];
        const d = "M" + Q.map(([x, y]) => f1(x) + " " + f1(y)).join(" L");
        return `<path d="${d}" fill="none" stroke="${ps}" stroke-width="${f1(2 * half + 5)}" stroke-linejoin="round"/>` +
               `<path d="${d}" fill="none" stroke="${pf}" stroke-width="${f1(2 * half)}" stroke-linejoin="round"/>`; };
      const legBits = side => robot ? rbG("leg", robotLeg(R, legJoints(side), half, dir)) : western && W.S.chaps ? westernChap(W, legJoints(side), half, side, dir, away) : sasq ? sqG("leg", shagEdge(legJoints(side), -dir, half, 10, C))
        : ninja ? njG("bands", ninjaShin(legJoints(side), half, WC.ninjaBoots, WC.ninjaBetween === "dark" ? null : skin)) : "";   // (ticket 515: a robot's ridges or knee pads, with each leg; 522: chaps; 557: a ninja's bands up the shin)
      for (const side of legSides) t += pantsLeg(side) + legBits(side);
      if (legApart) farLeg.pants += pantsLeg(dir) + legBits(dir);   // (ticket 136: the far one apart)
      svg += t + `<g transform="translate(0 1.5)">${hips(false).replace(/x="([^"]+)"/, (_, x) => `x="${f1(Number(x) + 1.5)}"`).replace(/width="([^"]+)"/, (_, w) => `width="${f1(Number(w) - 3)}"`)}</g>`;
    } else svg += poly(legs, pf, ps);
    if (wrapStripes(C) && legPosed)   // (along each thigh and shin, as an arm's are)
      for (const side of [1, -1]) { const J = legJoints(side), end = along(J, len);
        const marks = len > LL / 2 ? limbStripes(J[0], J[1], 2 * half, C.marks, `leg-stripes${side}`, 14) + limbStripes(J[1], end, 2 * half, C.marks, `leg-stripes${side}b`, 0)
                                   : limbStripes(J[0], end, 2 * half, C.marks, `leg-stripes${side}`, 14);
        if (legApart && side === dir) farLeg.pants += marks; else svg += marks; }
    else if (wrapStripes(C)) svg += legStripes(legs, hw, fx, half, L, len, Math.min(len * 0.3, 40), C.marks);
  }
  // (ticket 124) sitting, facing us: the knees toward us, round, in the
  // pants or bare (none under a skirt or dress, which hides them)
  // (a crease across each pants knee; a bare knee, round. Two round knees
  // in pants looked like a seat, Matthew said)
  if (sitting && !dir && !away && (suit || undies || !(P.style === "skirt" || P.style === "dress"))) {
    const longPants = suit || (!undies && P.style !== "shorts");
    for (const side of [1, -1]) { const [kx, ky] = kneeAt(side);
      svg += longPants ? `<path d="M${f1(kx - half * 0.7)} ${f1(ky + 4)} Q${f1(kx)} ${f1(ky + 13)} ${f1(kx + half * 0.7)} ${f1(ky + 4)}" fill="none" stroke="${ps}" stroke-width="3" stroke-linecap="round"/>`
                       : `<ellipse cx="${f1(kx)}" cy="${f1(ky + 3)}" rx="${f1(B.legW / 2 + 1)}" ry="${f1((B.legW / 2 + 1) * 0.75)}" fill="${limb}" stroke="${limbLine}" stroke-width="2"/>`; }
  }
  if (robot && !dir) for (const side of [1, -1]) svg += rbG("leg", robotLeg(R, legJoints(side), half, 0, away));   // (ticket 515: facing us, both legs'; from behind, no knee pads)
  if (western && W.S.chaps && !dir) for (const side of [1, -1]) svg += westernChap(W, legJoints(side), half, side, 0, away);   // (ticket 522: facing us or from behind, both chaps)
  if (sasq && !dir) svg += sqG("leg", shagEdge([1, -1].map(side => [legJoints(side), side]), "many", half, 10, C));   // (ticket 530: down each leg's outside)
  if (ninja && !dir) for (const side of [1, -1]) svg += njG("bands", ninjaShin(legJoints(side), half, WC.ninjaBoots, WC.ninjaBetween === "dark" ? null : skin));   // (ticket 557: facing us or from behind, both shins; 561: the skin between the bands, unless 🌑)
  veilFrom("pants", pantsAt);
  // (ticket 288: a scuba suit's stripe down the outside of each leg)
  if (scuba) svg += veil("pants", [1, -1].map(side => { const J = legJoints(side);
    return `<path d="M${J.map(([x, y]) => f1(x + side * (B.legW / 2 + 2)) + " " + f1(y)).join(" L")}" fill="none" stroke="${WC.scubaAccent}" stroke-width="6" stroke-linecap="round"/>`; }).join(""));
  svg += feetLater;
  // (ticket 431) a gnome man's tunic: its skirt below the belt, over the trousers, to the top of the thighs, flaring a little
  if (gnomeM && !undies) {
    const sl = LL * 0.2, fl = hw + 10 + sl * 0.2, tc = SH.color;
    svg += veil("shirt", `<path d="M${f1(-hw - 2)} ${f1(L - 10)} L${f1(hw + 2)} ${f1(L - 10)} L${f1(fl)} ${f1(L + sl)} Q0 ${f1(L + sl + 8)} ${f1(-fl)} ${f1(L + sl)} Z" ` +
                         `fill="${tc}" stroke="${darker(tc)}" stroke-width="2.5" stroke-linejoin="round"/>`);
  }
  // (ticket 434) a forester's tunic: its dagged hem below the belt, over the hose (under the arms, as the gnome's is)
  if (robin && !undies) { const tc = SH.color, hemY = L + LL * 0.18, n = 7, sx = (2 * hw + 8) / n;
    let hem = `M${f1(-hw - 4)} ${f1(L - 10)} L${f1(hw + 4)} ${f1(L - 10)} L${f1(hw + 4)} ${f1(hemY)}`;
    for (let i = 0; i < n; i++) { const x0 = hw + 4 - i * sx; hem += ` L${f1(x0 - sx / 2)} ${f1(hemY + 12)} L${f1(x0 - sx)} ${f1(hemY)}`; }
    svg += veil("shirt", `<path d="${hem} Z" fill="${tc}" stroke="${darker(tc)}" stroke-width="2.5" stroke-linejoin="round"/>`); }
  // (ticket 522) the trail boss's duster skirts, over the trousers, under the arms
  // (meditating, the skirts end on the lap, by the crossed feet, as a robe's hem does; sitting, as standing)
  if (western && W.S.duster) { const coatHem = B.pose === "meditate" ? Math.min(L + LL * 0.72, L + (feetY - L) * 0.72) : L + LL * 0.72;
    svg += veil("shirt", `<g data-western="coat">${squash && away ? squashed(westernCoat(W, w, hw, L, coatHem, dir, away)) : westernCoat(W, w, hw, L, coatHem, dir, away)}</g>`); }   // (crawling away, seen short with the back)
  // a fairy in a shirt and pants: her tunic's hem, a short frill at her
  // hips over the pants, in her tunic's style (the shirt's to see through)
  if (fairy && WC.fairyOutfit === "pants" && !undies) {
    const sl = LL * 0.17, fl = hw + 14 + sl * 0.25, n = 7, tf0 = fillOf("pat-shirt", SH.color, SH.pattern), ts = darker(SH.color);
    let fr = "";
    if (fTunic === "toadstool") {
      let u = `M${f1(-fl - 5)} ${f1(L + sl - 5)}`;
      for (let i = 0; i < 12; i++) { const xa = -fl - 5 + (2 * fl + 10) * i / 12, xb = -fl - 5 + (2 * fl + 10) * (i + 1) / 12; u += ` Q${f1((xa + xb) / 2)} ${f1(L + sl + 20)} ${f1(xb)} ${f1(L + sl - 5)}`; }
      fr += `<path d="${u} Z" fill="#fffaf0" stroke="#d6cfc0" stroke-width="2"/>`;
    }
    let d = `M${-hw} ${L - 10} L${hw} ${L - 10} L${f1(fl)} ${f1(L + sl)}`;
    for (let i = 1; i <= n; i++) d += fairyHem(fTunic, fl - (i - 1) * 2 * fl / n, fl - i * 2 * fl / n, L + sl);
    svg += veil("shirt", fr + `<path d="${d} Z" fill="${tf0}" stroke="${ts}" stroke-width="2.5" stroke-linejoin="round"/>`);
  }

  // (ticket 286: crawling toward us, the hips and the legs are behind the back, hidden: Matthew, "shoulders, head and
  // arms ... but not so much torso, and almost no legs at all -- none maybe")
  if (crawlTo) svg = svg.slice(0, legsAt);
  // (Matthew, of the drawing without them: "we should see knees between the arms here") but the knees, on the floor
  // between the arms, beyond the hands and under the chin: each thigh's front, seen short, down to its knee (in the pants,
  // or a skirt or a dress draped over it, a witch's too, with a crease; bare in shorts, round); drawn with the arms, below,
  // over long hair hanging down (Matthew: the witch's should show too)
  let kneesTo = "";
  if (crawlTo && !move.swim) {
    const longPants = suit || (!undies && P.style !== "shorts"), kr = B.legW / 2 + 6, chinY = -20 + (move.chin ?? 0.3) * armLen;
    const ky = chinY + 0.85 * (Math.max(...arms.map(x => x.H[1])) - chinY), top = chinY + 4;   // (Matthew: "knees should be closer down, nearer the hands")
    for (const side of [1, -1]) { const kx = side * fx;
      kneesTo += veil("pants", `<rect data-knee="${side}" x="${f1(kx - kr)}" y="${f1(top)}" width="${f1(2 * kr)}" height="${f1(ky + kr * 0.5 - top)}" rx="${f1(kr)}" ` +
                           `fill="${longPants ? pf : limb}" stroke="${longPants ? ps : limbLine}" stroke-width="2.5"/>` +
                           (longPants ? `<path d="M${f1(kx - kr * 0.6)} ${f1(ky - kr * 0.3)} Q${f1(kx)} ${f1(ky + kr * 0.15)} ${f1(kx + kr * 0.6)} ${f1(ky - kr * 0.3)}" fill="none" stroke="${ps}" stroke-width="3" stroke-linecap="round"/>` : "")); }
  }
  // neck, behind the shirt so it shows in the collar
  // (the neck, the arms and the body are the shirt's to see through: an
  // invisible shirt is an invisible body, never a bare one)
  const bodyAt = svg.length;
  const neckC = ninja ? NINJA.cloth : robot && R.S.head !== "bubble" ? R.limb : limb;   // (ticket 515: a robot's neck in its metal; inside the space bot's glass, the friend's own; 557: a ninja's, the mask's cloth)
  const neckSvg = `<rect x="${-B.neckW / 2}" y="${neckTop}" width="${B.neckW}" height="${B.neck + 44}" rx="4" ` +
                  `fill="${neckC}" stroke="${robot || ninja ? darker(neckC) : limbLine}" stroke-width="2.5"/>`;
  if (!(squash && away)) svg += neckSvg;

  // arms, sleeves and shirt
  const sf = suit ? "url(#pat-fur)" : fillOf("pat-shirt", SH.color, SH.pattern);
  const ss = suit ? darker(C.fur) : darker(SH.color);
  // (a falcon suit's arms are inside its wings, drawn after the body)
  const winged = !!BIRD_SUIT[kind] || moth;   // (ticket 536: and a moth's)
  // (ticket 522, the lasso; ticket 518, everything held: a thing in the far hand, in profile or turned partway, is drawn with
  // that hand, behind the body, and goes into #figure-far with it; the rest after the shirt, before the rein and the hands)
  const heldFar = (dir || q) && !winged && !squash ? held.filter(h => farArms.includes(h.arm)) : [], heldView = crawlTo ? "floor" : dir ? "side" : away ? "back" : "front";
  const heldDraw = h => heldSvg(h.item, h.arm.H, heldView, { WC, kb, monk, dryad: WC.monkWho === "dryad", knight, own: h.own, out: h.arm.side, dir, spin: lassoSpin });
  const farAt = svg.length;
  if (!winged && !squash) for (const arm of farArms) svg += limbThrough(armJoints(arm), armW, limb, limbLine);
  // (a sleeve: the shirt's share of the arm, round the elbow)
  const sleeveOf = arm => alongArm(arm, sleeve);
  if (sleeve > 0 && !winged && !squash) for (const arm of farArms) svg += limbThrough(sleeveOf(arm), armW + 14, ss);
  // (a hand: with different hands, the friend's right hand, on our left when
  // they face us, wears its own style; one holding the broom is closed round
  // it, a round fist)
  const handOf = ({ side, out, H, angle }) => { const hands = suit ? suitHands(C, kind) : robot ? robotHands(R, state.hands) : state.hands, own = !suit && !robot && ownOf(side) === "R" ? rightHandOf(hands) : hands;   // (ticket 515: a robot's hands; ticket 528: by own hand, armOf's)
    const gl = ninja ? { ...own, color: NINJA.glove } : own;   // (ticket 557: black gloves, in the friend's own hand style)
    return handSvg(H[0], H[1], angle, out, held.some(h => h.arm.side === side && HOLDABLES[h.item].fist) ? { ...gl, style: "round" } : gl, skin); };
  // (ticket 286: crawling toward us or away, each arm drawn whole, straight down to its hand flat on the floor, seen short)
  const crawlArm = arm => limbThrough(armJoints(arm), armW, limb, limbLine) +
    (sleeve > 0 ? limbThrough(sleeveOf(arm), armW + 14, ss) + limbThrough(sleeveOf(arm), armW + 9, sf) : "") +
    (wrapStripes(C) ? (P => limbStripes(P[0], P[1], armW + 9, C.marks, `arm-stripes${arm.side}`, 22) + (P.length > 2 ? limbStripes(P[1], P[2], armW + 9, C.marks, `arm-stripes${arm.side}b`, 0) : ""))(sleeveOf(arm)) : "") +
    (robot ? rbG("arm", robotArm(R, arm, { dir: 0, w, armW, sleeve: sleeveOf(arm) })) : "") + (western ? westernArm(W, sleeveOf(arm), armW, arm.side, 0) : "") + sqArm(arm, 0) + njArm(arm);   // (ticket 515: a robot's, at its shoulder; 530: a Sasquatch's shag; 561: a ninja's bands)
  const flatHand = arm => move.swim ? handOf(arm) : `<g transform="translate(0 ${f1(arm.H[1] * 0.4)}) scale(1 0.6)">${handOf(arm)}</g>`;   // (swimming, as they are)
  // (in profile, or turned partway, the far arm's sleeve over it, and its hand, behind the body)
  if ((dir || q) && !winged && !squash) {
    if (sleeve > 0) for (const arm of farArms) svg += limbThrough(sleeveOf(arm), armW + 9, sf);
    if (robot) for (const arm of farArms) svg += rbG("arm", robotArm(R, arm, { dir, far: true, w, armW, sleeve: sleeveOf(arm) }));   // (ticket 515)
    if (western) for (const arm of farArms) svg += westernArm(W, sleeveOf(arm), armW, arm.side, dir);   // (ticket 522: the far sleeve's fringe)
    if (ninja) for (const arm of farArms) svg += njArm(arm);   // (ticket 561)
    // (ticket 530: a Sasquatch's far arm, behind the body, without its shag: it barely showed, and the shapes stay under the cap)
    for (const h of heldFar) svg += heldDraw(h);
    const farHandsAt = svg.length;
    for (const arm of farArms) svg += handOf(arm);
    veilFrom("hands", farHandsAt);
  }
  // (ticket 130) in a place, turned sideways and not in water, the far arm,
  // its sleeve and its hand are drawn apart from the rest of the friend, a
  // body's depth behind them among the place's things (draw, below): so an
  // arm can reach round someone they stand just in front of, for a hug
  const farDeep = scened && dir && !winged && !wet ? svg.slice(farAt) : "";
  if (farDeep) svg = svg.slice(0, farAt);
  // (ticket 138: from behind, a shirt's neckline is a shallow round one)
  const collar = { v: [[0, 18]], round: [[-nw * 0.7, -6], [0, -1], [nw * 0.7, -6]], flat: [] }[away && SH.collar === "v" ? "round" : SH.collar];
  const shoulder = !suit && SH.style === "tank" ? [[nw + 12, -20], [w, 30]] : [[w + 4, -6], [w, 44]];
  const torso = [[-nw, -20], ...collar, [nw, -20], ...shoulder, [w + 2, L], [-(w + 2), L],
                 ...shoulder.map(([x, y]) => [-x, y]).reverse()];
  const bareBody = [[-(w + 2), L + 2], [-w, 44], [-(w + 4), -6], [-nw, -20], [nw, -20], [w + 4, -6], [w, 44], [w + 2, L + 2]];
  const shirtAt = svg.length;
  if (undies) {
    // a plain body shape in skin, no collar, then the swimsuit or top on it
    // (outlined on every side but the bottom, so it runs on into the hips
    // without a line at the waist)
    const body = bareBody;
    svg += poly(body, limb, "none", 0) +
           `<polyline points="${pts(body)}" fill="none" stroke="${limbLine}" stroke-width="2.5" stroke-linejoin="round"/>`;
    svg += undiesTop(U, w, nw, hw, fx, L);
  } else if (!(robot && R.S.torso === "egg")) svg += poly(torso, sf, ss);   // (ticket 515: the helper bot's egg is its body, in place of the shirt's)
  // (ticket 288) a scuba suit's yoke (and zip, from behind); an air tank's straps; the old diving suit's brass corselet
  if (scuba && !undies) svg += scubaYoke(WC, w, nw, L, dir, away);
  if (WC.tank && !diver) svg += tankStraps(w, L, dir, away || crawlTo);   // (ticket 371: crawling toward us, it is the back that shows: its band, not the chest's buckle)
  if (diver) svg += diverCorselet(WC, w, dir);
  // a few webs on the bodice, side by side
  if (fewWebs && !undies)
    svg += spiderWeb(-0.42 * hw, -20 + 0.48 * (L + 20), 0.3 * hw, WC.webColor) + spiderWeb(0.42 * hw, -20 + 0.48 * (L + 20), 0.3 * hw, WC.webColor);
  // (tickets 436, 437) the royal ermine collar over the shoulders (a cape-let), and a gold girdle at the waist
  if (royal && !undies) { const cw = w + 10;
    svg += `<path d="M${f1(-cw)} -6 Q${f1(-cw)} -24 ${f1(-nw - 4)} -24 Q0 ${f1(away ? -18 : -8)} ${f1(nw + 4)} -24 Q${f1(cw)} -24 ${f1(cw)} -6 L${f1(cw - 4)} 26 Q0 ${f1(away ? 36 : 44)} ${f1(-(cw - 4))} 26 Z" fill="url(#pat-ermine)" stroke="#c9c5bd" stroke-width="2" stroke-linejoin="round"/>` +
           `<path d="M${f1(-(w + 2))} ${f1(L - 10)} Q0 ${f1(L - 4)} ${f1(w + 2)} ${f1(L - 10)}" fill="none" stroke="#e0b030" stroke-width="6" stroke-linecap="round"/>`; }
  // (ticket 435) a knight's tabard over the mail, in his arms, the emblem on his chest (none on his back; at his side, one
  // colour); a belt at the waist, its buckle facing us only
  if (knight && !undies) { const tb = L + LL * 0.24, top = -18;
    const shape = dir ? `M${f1(-(w - 2))} ${f1(top)} L${f1(w - 2)} ${f1(top)} L${f1(w + 4)} ${f1(tb)} L${f1(-(w + 4))} ${f1(tb)} Z`
                      : `M${f1(-(w - 6))} ${f1(top)} Q0 ${f1(top + (away ? 4 : 16))} ${f1(w - 6)} ${f1(top)} L${f1(hw + 8)} ${f1(tb)} L${f1(-(hw + 8))} ${f1(tb)} Z`;
    svg += dir ? `<path d="${shape}" fill="${WC.knightA || "#2f6db5"}" stroke="#3b3b44" stroke-width="2.5"/>` : armsFill("tabard", shape, WC, away, away ? null : [0, L * 0.42], 1.2, [0, L * 0.42], Math.max(hw + 12, (tb - top) / 2 + 30));
    svg += `<rect x="${f1(-(w + 4))}" y="${f1(L - 14)}" width="${f1(2 * w + 8)}" height="10" fill="#4a2f16"/>` + (away || dir ? "" : `<rect x="-8" y="${f1(L - 16)}" width="16" height="14" rx="2" fill="none" stroke="#e0b030" stroke-width="3"/>`);
  }
  if (robot && !squash) svg += rbG("torso", robotTorso(R, w, L, dir, away));   // (ticket 515: a robot's shell, chest or back; crawling, its own drawing, below)
  // (ticket 522) a cowgirl's or cowboy's pieces over the shirt; crawling toward us, the back's, turned up with it
  if (western) svg += `<g data-western="torso">${westernTorso(W, w, L, nw, dir, away || crawlTo)}</g>`;
  // (ticket 434) a forester's: the tunic's dagged hem over the hose, its dagged cape on the shoulders, a belt with a pouch,
  // the quiver's strap across the chest (the other way from behind, with the quiver on the back)
  if (robin && !undies) { const tc = SH.color, tl = darker(tc);
    const cw = w + 8, cy = 30, m = 6, cs = 2 * cw / m;
    let cape = `M${f1(-nw - 2)} -22 Q0 ${away ? -14 : -4} ${f1(nw + 2)} -22 L${f1(cw)} -6 L${f1(cw)} ${f1(cy)}`;
    for (let i = 0; i < m; i++) { const x0 = cw - i * cs; cape += ` L${f1(x0 - cs / 2)} ${f1(cy + 12)} L${f1(x0 - cs)} ${f1(cy)}`; }
    svg += `<path d="${cape} L${f1(-cw)} -6 Z" fill="${mix(tc, "#000000", 0.15)}" stroke="${tl}" stroke-width="2.5" stroke-linejoin="round"/>`;
    const sd = away ? -1 : 1;
    svg += `<path d="M${f1(-sd * (w - 8))} 6 L${f1(sd * (w - 4))} ${f1(L - 18)}" stroke="#6b4226" stroke-width="9" stroke-linecap="round"/>` +
           `<rect x="${f1(-(w + 3))}" y="${f1(L - 16)}" width="${f1(2 * w + 6)}" height="11" fill="#6b4226" stroke="#4a2f16" stroke-width="2"/>` +
           (away ? "" : `<rect x="${f1(dir ? dir * (w - 18) - 9 : -w * 0.5)}" y="${f1(L - 8)}" width="18" height="22" rx="4" fill="#8a5a2b" stroke="#4a2f16" stroke-width="2"/>`);
    if (WC.quiver !== false) svg += quiverSvg(w, L, away, dir);
  }
  // (ticket 433) a monk's or a dryad's rope belt (the wizard's cord, in rope), and the hood down: a cowl folded on the shoulders
  if (monk && !undies) { const gx = dir ? dir * (w - 4) : -10, cy = L - 14, rope = WC.monkWho === "dryad" ? "#8a6a3a" : "#c8a46a";
    svg += `<path d="M${f1(-(w + 2))} ${f1(cy)} Q0 ${f1(cy + 6)} ${f1(w + 2)} ${f1(cy)}" fill="none" stroke="${rope}" stroke-width="5" stroke-linecap="round"/>` +
           (away ? "" : `<path d="M${f1(gx)} ${f1(cy + 2)} l-3 44 M${f1(gx + 4)} ${f1(cy + 2)} l4 36" stroke="${rope}" stroke-width="4" stroke-linecap="round"/>`);   // (ticket 444: its ends at the front only)
    if (!hooded) { const c = WC.monkCloak || "#6b4a2b";
      svg += `<path d="M${f1(-(w + 6))} -10 Q0 ${f1(away ? -24 : 34)} ${f1(w + 6)} -10 Q${f1(w * 0.6)} -30 0 -26 Q${f1(-w * 0.6)} -30 ${f1(-(w + 6))} -10 Z" fill="${darker(c)}" stroke="${darker(darker(c))}" stroke-width="3" stroke-linejoin="round"/>`; } }
  // (ticket 432) a wizard's gold cord round his waist, its knot and tassels hanging at the front (at the side, in profile)
  if (wizard && !undies) { const gx = dir ? dir * (w - 4) : 6, cy = L - 14;
    svg += `<path d="M${f1(-(w + 2))} ${f1(cy)} Q0 ${f1(cy + 6)} ${f1(w + 2)} ${f1(cy)}" fill="none" stroke="#e0b030" stroke-width="5" stroke-linecap="round"/>` +
           (away ? "" : `<circle cx="${f1(gx)}" cy="${f1(cy + 3)}" r="5" fill="#e0b030"/><path d="M${f1(gx - 2)} ${f1(cy + 6)} l-4 30 M${f1(gx + 2)} ${f1(cy + 6)} l5 26" stroke="#e0b030" stroke-width="3.5" stroke-linecap="round"/>`); }   // (ticket 444: its knot at the front only)
  // (ticket 431) a gnome woman's bodice over her blouse, and her apron over the skirt
  if (gnomeW) svg += gnomeBodice(WC, w, L, dir, away) + gnomeApron(w, L, skirtHem ? Math.max(L + 40, skirtHem - 30) : L + 120, dir, away);
  // a witch's belt round her waist, its buckle in the middle (and a gnome man's, brown)
  if (witch || gnomeM) {
    const bc = gnomeM ? "#5b3a1e" : WC.bootColor ?? "#1f1f22";
    svg += `<rect x="${f1(-(w + 3))}" y="${f1(L - 16)}" width="${f1(2 * w + 6)}" height="12" fill="${bc}" stroke="${darker(bc)}" stroke-width="2"/>` +
           // (ticket 444, Matthew: "the walking away pose shouldn't show a belt buckle, just the forwards facing pose, L and R")
           (away ? "" : dir ? `<rect x="${f1(dir * (w + 3) - (dir > 0 ? 8 : 0))}" y="${f1(L - 19)}" width="8" height="18" rx="2" fill="none" stroke="#f2c230" stroke-width="3"/>`   // (in profile, edge on, at her front)
                : `<rect x="-11" y="${f1(L - 19)}" width="22" height="18" rx="2.5" fill="none" stroke="#f2c230" stroke-width="4"/>` +
                  `<line x1="-3" y1="${f1(L - 10)}" x2="9" y2="${f1(L - 10)}" stroke="#f2c230" stroke-width="2.5" stroke-linecap="round"/>`);
  }
  if (wrapStripes(C) && !undies) svg += torsoStripes(torso, w, L, C.marks);
  if (suit && kind !== "eaglesuit" && !giraffe && !away)  // the suit's belly (a bald eagle is brown all over; none on its back: ticket 138; nor on a giraffe, patched all over, ticket 544)
    svg += `<ellipse cx="0" cy="${f1(L * 0.58)}" rx="${f1(w * 0.55)}" ry="${f1(L * 0.36)}" fill="${C.belly}"/>` + (dragon ? dragonPlates(C, L * 0.58, w * 0.55, L * 0.36) : "");   // (ticket 499: plated)
  // (ticket 536) a moth's fuzzy bands, across its belly, or from behind across its back
  if (moth) svg += away ? mothBands(C, 0, L * 0.6, w * 0.8, L * 0.34) : mothBands(C, 0, L * 0.58, w * 0.55, L * 0.36);
  // (ticket 530) a Sasquatch's shaggy sides, from the shoulder to the hip: both toward us and from behind, the back's in profile
  if (sasq && !undies) svg += sqG("torso", shagEdge((dir ? [-dir] : [1, -1]).map(sd => [[[sd * (w + 2), 2], [sd * w, 44], [sd * (w + 2), L - 4]], sd]), "many", 0, 10, C));
  if (crawlTo && svg.length > shirtAt) svg = svg.slice(0, shirtAt) + flipped(svg.slice(shirtAt));   // (ticket 286: the back, rising behind the head; ticket 515: none to turn under the helper bot's egg, no empty group)
  // (ticket 371: the tank on it, turned over with it, standing out of it by its own thickness: its far end over the back's line)
  if (crawlTo && WC.tank && !diver) svg += veil("shirt", `<g transform="translate(0 ${f1(-0.8 * Math.max(24, 0.4 * w))})">${flipped(tankSvg(WC, w, L, dir))}</g>`);
  // (ticket 515: a robot's shell crawling toward us, its own drawing over the back turned up, not turned with it; then what
  // stands off that back, each its own drawing: the key standing up, the pack, the battery, the cord)
  if (crawlTo && robot) { svg += rbG("torso", robotTorso(R, w, L, 0, true, robotBackO.crawl)); const rb = rbG("back", robotBack(R, w, L, dir, away, { ...robotBackO, over: true })); if (rb) svg += veil("shirt", rb); }
  veilFrom("shirt", bodyAt);
  if (squash && !crawlTo && svg.length > bodyAt) svg = svg.slice(0, bodyAt) + squashed(svg.slice(bodyAt));   // (ticket 286: the neck and the body, seen short; the arms are drawn whole, below)
  if (robot && squash && away) svg += veil("shirt", rbG("torso", robotTorso(R, w, L, 0, true, robotBackO.crawl)));   // (ticket 515: crawling away, the shell's own drawing, not seen short with the body)
  const bodyEnd = svg.length;
  if (squash && away) svg += veil("shirt", squashed(neckSvg));   // (crawling away, the neck is beyond the back, under the hair: not moved with it, below)
  // (ticket 125) the ears, facing us: out at the sides of the head at the
  // eyes' height, behind the head; under long hair, which covers them, but
  // over shorter hair, and an elf's poke out through any hair
  const earsOn = F.ears && F.ears !== "none" && !C && !diver && !hooded, earK = (F.earSize ?? 100) / 100;
  const earsFront = () => { const ey = faceLayout(F).eyeY + 10, [xl, xr] = acrossAt(headPoints(F), ey);
    return veil("head", `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})">` +
                        earSvg(F.ears, earK, F.color, xl + 6, ey, -1) + earSvg(F.ears, earK, F.color, xr - 6, ey, 1) + `</g>`); };
  const longHair = state.hair.style === "long" || (state.hair.style === "curly" && hairLen(state.hair) > 1.05);
  if (earsOn && !dir && longHair && F.ears !== "elf") svg += earsFront();
  // the hair, in the face's frame: over the body and shirt, but under the
  // arms, which are drawn again over it (long or curly hair hangs beside
  // them), and behind the head (its front, the bangs, comes again over the
  // face: see bangsSvg)
  // (in profile, what hangs behind the head, on the head as drawn)
  if (!C && !diver && !hooded)
    svg += veil("hair", `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})">` +
           (side ? `<g transform="${topFlies(F)}">${tucked(side.back, "under-hat")}</g></g>`
                 : `<g transform="${topFlies(F)}"><g transform="${hairStretch(F)}">${tucked(hairSvg(state.hair), "under-hat")}</g></g></g>`));

  if (earsOn && !dir && !(longHair && F.ears !== "elf")) svg += earsFront();
  // (ticket 286: crawling toward us, the arms reach down from behind the head to the floor: over the hair hanging
  // down, which hid them, and under the face, which is nearer than the shoulders; the hands come last, below)
  if (crawlTo) svg += kneesTo + (winged ? "" : veil("shirt", arms.map(crawlArm).join("")));
  if (winged && !moth) svg += veil("shirt", arms.map(arm => dir ? armWing(C, arm, armW, 0.3) : armWing(C, arm, armW)).join(""));
  // (ticket 536) a moth's wings, the same way (in profile the far one first, shaded), and its fluffy collar over their roots
  if (moth) svg += veil("shirt", (dir ? [...farArms, ...nearArms] : arms).map(arm => mothWing(C, arm, armW, dir ? 0.3 : 1, M, !!dir && farArms.includes(arm))).join("") + mothCollar(C, nw, dir));

  // the face, in its own units
  const groundInFace = (ground - ty) / s;
  // (the eyes see-through on their own: an invisible head can keep its
  // eyes, floating)
  const face = faceSvg(F, groundInFace, true, dir, q, away);
  const inFace = t => `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})">${t}</g>`;
  let faceEyes = "";   // (ticket 453: the eyes as drawn, for the druid's eyes in the dark)
  // (ticket 138: from behind, the head and its hair, no face)
  // (in a costume, the hood's back closed over the head: its ears, mane or
  // horns, and no face on its brim)
  // (ticket 515: a robot's window, solid inside, under the face that looks out of it)
  if (robot && RH.behind && !away) svg += veil("head", inFace(`<g data-robot="behind" transform="${topFlies(F)}">${RH.behind}</g>`));
  // (ticket 544) the giraffe's neck, its mane and its own head, up from the hood, before it, so the hood hides the neck's foot
  // (with the lid when the head bursts); turned with the friend (rot), drawn turned back upright
  if (giraffe) svg += veil("head", inFace(`<g transform="${topFlies(F)}">${giraffeNeckSvg(C, F, { dir, away, rot, squash, rise: lying || swimming })}</g>`));
  if (away && C) {
    const fill = falconHead(kind) ? C.marks : eagleHead(kind) ? C.belly : ANIMALS[kind].suit ? "url(#pat-fur)" : C.fur;
    svg += veil("head", inFace(`<g transform="${topFlies(F)}"><g transform="${hairStretch(F)}">${hoodBack(C, kind, F, 0, true)}` +
                               `<ellipse ${hoodEllipse(F)} fill="${fill}" stroke="${darker(C.fur)}" stroke-width="4"/>${dragon ? dragonHoodSpines(C, F) : ""}${sasq ? sasquatchTop(C, F, 0, true) : ""}</g></g>`));   // (ticket 499: a dragon's spines down it)
  } else if (away) { svg += (robot && !robotSees(R, "back") || ninja ? "" : veil("head", inFace(face.under))) + (diver || robot || ninja ? "" : hooded
      ? veil("head", inFace(`<g transform="${topFlies(F)}"><g transform="${hairStretch(F)}">${knight ? "" : clothHoodAway(F, clothHood())}</g></g>`))   // (ticket 433: the hood closed over the head; 435: a knight's helmet)
      : veil("hair", inFace(`<g transform="${topFlies(F)}">${western && W.H ? tucked(backHair(state.hair, F), "under-hat-away", HEAD_C + (hatLine - HEAD_C) * headH(F)) : backHair(state.hair, F)}</g>`))); }   // (ticket 522: under a cowboy hat, from behind too)
  // (ticket 515) under a robot's faceplate, a screen, or a box seen from the side, the face is not drawn (its own eyes and
  // mouth follow the mood, below); a window or an opening shows it
  else if (robot && !robotSees(R, dir ? "side" : "front")) { /* (faceFrame stays, for bubbles and fire) */ }
  // (ticket 557) under a ninja's mask the face is not drawn (the mask, below); only its eyes, kept for 👀 eyes in the slit:
  // facing us as drawn, in profile the near eye moved to the front as the profile's features are
  else if (ninja) { if (WC.ninjaEyes) faceEyes = !dir ? face.eyes : `<defs><clipPath id="face-near-n"><rect x="${dir > 0 ? 100 : -4000}" y="-4000" width="4100" height="8000"/></clipPath></defs>` +
                                                  `<g transform="translate(${f1(dir * 24 * headWidth(F))} 0)"><g clip-path="url(#face-near-n)">${face.eyes}</g></g>`; }
  else if (!dir) { faceEyes = inFace(face.eyes); svg += veil("head", inFace(face.under)) + veil("eyes", faceEyes) + veil("head", inFace(face.over)); }
  else {
    // (ticket 124) in profile: the face's features on our side only (one
    // eye and brow, half the mouth), moved toward the front of the head; a
    // nose, if they have one, seen from the side at its front edge,
    // pointing the way they face
    const sh = 24 * headWidth(F), edge = profileEdge(F);
    const near = t => `<defs><clipPath id="face-near"><rect x="${dir > 0 ? 100 : -4000}" y="-4000" width="4100" height="8000"/></clipPath></defs>` +
                      `<g transform="translate(${f1(dir * sh)} 0)"><g clip-path="url(#face-near)">${t}</g></g>`;
    // (the nose, from the side: a bump out of the face's front edge, as tall
    // and as long as its sliders say; a witch's, her long side nose there)
    const noseOn = F.noseView && F.noseView !== "none", NL = faceLayout(F), witchNose = F.noseView === "witch" || F.nose === "witch";
    const ex = acrossAt(headPoints(F), NL.noseTop + NL.uh / 2)[dir > 0 ? 1 : 0];
    const nose = !noseOn ? "" : witchNose
      ? `<g transform="translate(${f1(ex - 100 - dir * 6)} 0)">${noseSvg({ ...F, noseView: dir > 0 ? "right" : "left", nose: "witch" })}</g>`
      : `<path d="M${f1(ex - dir * 3)} ${f1(NL.noseTop)} Q${f1(ex + dir * NL.uw * 0.55)} ${f1(NL.noseTop + NL.uh * 0.55)} ${f1(ex + dir * NL.uw * 0.5)} ${f1(NL.noseTop + NL.uh * 0.85)} ` +
        `Q${f1(ex + dir * NL.uw * 0.2)} ${f1(NL.noseTop + NL.uh * 1.05)} ${f1(ex - dir * 3)} ${f1(NL.noseTop + NL.uh * 0.95)} Z" fill="${F.color}" stroke="black" stroke-width="3" stroke-linejoin="round"/>` +
        `<rect x="${f1(ex - 3)}" y="${f1(NL.noseTop + 3)}" width="6" height="${f1(NL.uh * 0.85)}" fill="${F.color}"/>`;
    // (the mouth: its far half, carried to the front, so the lips meet the
    // face's front edge and a smile turns up toward the cheek, not the
    // front, which read as a frown; and an ear, where the hair leaves it)
    const rest = face.over.slice(noseSvg(F).length), mi = rest.indexOf('<g transform="translate(100 ');
    const brows = mi < 0 ? rest : rest.slice(0, mi), mouth = mi < 0 ? "" : rest.slice(mi);
    const lips = `<defs><clipPath id="face-far"><rect x="${dir > 0 ? -4000 : 100}" y="-4000" width="4100" height="8000"/></clipPath></defs>` +
                 `<g transform="translate(${f1(dir * (edge - 4))} 0)"><g clip-path="url(#face-far)">${mouth}</g></g>`;
    // (the ear, as chosen, seen whole: its front edge toward the face, its rim toward the back)
    const earAt = !earsOn ? null : side ? side.ear : [100 - dir * 0.12 * 80 * headWidth(F), HEAD_C + 5];
    const earSide = earAt ? earSvg(F.ears, earK, F.color, earAt[0] + dir * 8 * earK, earAt[1], -dir) : "";
    faceEyes = inFace(near(face.eyes).replace('id="face-near"', 'id="face-near-e"').replace("url(#face-near)", "url(#face-near-e)"));
    svg += veil("head", inFace(face.under + earSide)) + veil("eyes", faceEyes) +
           veil("head", inFace(near(brows) + lips + face.sick + nose));
  }
  // and the front of the hair over them all: the bangs (on the lid, when
  // the head explodes)
  // (under a witch's hat, from its brim down: the brim's line as the hat is
  // stretched with the head, which big eyes make taller)
  // (in profile, the hair on the head itself, over its top and back)
  // (ticket 442) the beard and moustache from 💇 Hair (the gnome man's and the wizard's too), under the bangs and a hat
  if (beardStyle && !away) svg += veil("hair", inFace(beardSvg(F, beardStyle, beardShade(state.hair), dir, state.hair.moustache !== false, state.hair)));
  // (ticket 557) a ninja's mask over the whole head, tight, the face hidden: the eyes only in the slit, with 👀 (as the monk's
  // eyes in the dark: drawn again without their whites, so black pupils vanish and coloured ones show); and the gold head
  // covering over it, flying off with the lid as a hat does
  if (ninja) {
    svg += veil("head", inFace(njG("mask", ninjaMask(F, dir, away, q))));
    const slit = faceEyes && !away ? ninjaSlitClip(F, dir, q) : null;
    if (slit) svg += veil("eyes", inFace(slit.def + `<g clip-path="url(#${slit.id})">${faceEyes.replace(/fill="#ffffff" stroke="black"/g, 'fill="none" stroke="none"')}</g>`));
    if (WC.ninjaBand !== false) svg += veil("head", inFace(`<g transform="${topFlies(F)}">${njG("band", ninjaBand(F, WC, dir, away, q))}</g>`));
  }
  // (ticket 453, Matthew: "see head? if yes, show the head as is, but permit a slider to control how dim it is (fading to black
  // in the extreme). if no --- we see black in the hood, and see eyes is an option still (if no see head)--- black eyes won't
  // show up, but various other eye styles will") the druid's face in the hood: dimmed toward black (hoodDim, a share), or
  // not seen (hoodFace false), the hood's window black, and its eyes in the dark if asked (hoodEyes: drawn again without their
  // whites, so black pupils vanish and coloured irises, stars, hearts and crosses show)
  if (monk && clothHood() && !away) {
    const dark = op => veil("head", `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})"><g transform="${topFlies(F)}"><g transform="${hairStretch(F)}">` +
                                    `<path data-hood="dark" d="${clothHoodOpening(F, dir)}" fill="#000000" opacity="${op}"/></g></g></g>`);
    if (WC.hoodFace === false) { svg += dark(1); if (WC.hoodEyes !== false && faceEyes) svg += veil("eyes", faceEyes.replace(/fill="#ffffff" stroke="black"/g, 'fill="none" stroke="none"').replace(/id="face-near-e"/g, 'id="face-near-dark"').replace(/url\(#face-near-e\)/g, "url(#face-near-dark)")); }
    else if ((Number(WC.hoodDim) || 0) > 0) svg += dark((Math.min(100, Number(WC.hoodDim)) / 100).toFixed(2));
  }
  if (!C && !diver && !hooded && !away) svg += veil("hair", inFace(side ? `<g transform="${topFlies(F)}">${tucked(side.cap, "under-hat-bangs", HEAD_C + ((hatLine ?? 40) - HEAD_C) * headH(F))}</g>`
                                          : tucked(bangsSvg(state.hair, F), "under-hat-bangs", HEAD_C + ((hatLine ?? 40) - HEAD_C) * headH(F))));
  // a fairy's wreath of flowers, over her hair
  if (fairy && WC.wreath !== false) svg += veil("hair", inFace(wreathSvg(F, WC.wreathFlowers)));
  // (ticket 288) gear on the head: a face mask and a snorkel; with a tank and no snorkel, its air hose from the shoulder to
  // a mouthpiece. Or the old helmet over the whole head, its air hose up to the top of the scene, behind it (in front of
  // it, from behind)
  if (!diver) {
    if (WC.mask) svg += veil("head", inFace(maskSvg(F, WC, dir, away)));
    if (WC.snorkel) svg += veil("head", inFace(snorkelSvg(F, WC, dir, away)));
    if (WC.tank && !WC.snorkel && !away) {
      const mx = tx + (100 + (dir ? dir * (profileEdge(F) - 4) : 10)) * s, my = ty + (faceLayout(F).mouthY + 4) * s, sx = dir ? -dir * 0.3 * w : 0.55 * w;
      svg += veil("head", `<path d="M${f1(sx)} -12 Q${f1(sx + (dir ? dir * 40 : 50))} ${f1(my + 30)} ${f1(mx)} ${f1(my)}" fill="none" stroke="#1f2937" stroke-width="7" stroke-linecap="round"/>` +
                          `<circle cx="${f1(mx)}" cy="${f1(my)}" r="${f1(14 * s)}" fill="#374151"/>`);
    }
  } else {
    const hm = diverHelmet(F, WC, dir, away), a0 = [tx + hm.fitting[0] * s, ty + hm.fitting[1] * s];
    const topY = (sceneFrame.top - figureT.dy - figureT.sg) / figureT.k + figureT.g - 10;
    // (ticket 371: the body turned about the hips, crawling in profile or lying down, the hose still rises from the helmet's
    // fitting, wherever the turn has put it, straight up to the surface: counter-turned, not pointing the way the head does)
    // (the turn: rotTf, or swimming, the inner group's below; the same, rot about the hips)
    const th = (rot || 0) * Math.PI / 180, a = [a0[0] * Math.cos(th) - (a0[1] - L) * Math.sin(th), L + a0[0] * Math.sin(th) + (a0[1] - L) * Math.cos(th)];
    const hose = WC.diverHose !== false && topY < a[1] - 40 ? veil("head", (rot ? `<g transform="rotate(${-rot} 0 ${f1(L)})">` : "") + diverHose(WC, a, [a[0] - (dir || 1) * 70, topY], sceneryStill()) + (rot ? `</g>` : "")) : "";
    svg += (away ? "" : hose) + veil("head", inFace(hm.svg)) + (away ? hose : "");
  }
  // (ticket 288) where a diving suit leaks, as 🫧 leaks says: the old helmet's fitting and exhaust and its collar's edges;
  // a scuba suit's shoulders and knees; a tank's valve and mouthpiece; a snorkel's tip (in the picture's units, turned
  // with the friend: leakPts, for leakTick)
  const leakAmt = Math.max(0, Math.min(100, Number(WC.leaks) || 0)) / 100;
  if (leakAmt && (scuba || diver || WC.tank || WC.snorkel)) {
    const inF = ([x, y]) => [tx + x * s, ty + y * s], DH = diveHead(F), pts = [];
    if (diver) { const hm = diverHelmet(F, WC, dir, away); pts.push(inF(hm.fitting), inF(hm.exhaust), [-w, 28], [w, 28]); }
    else {
      if (scuba) pts.push(...arms.map(x => x.A), kneeAt(1), kneeAt(-1));
      if (WC.tank) pts.push([dir ? -dir * (0.5 * w + 24) : 0, neckTop - 10]);
      if (WC.tank && !WC.snorkel && !away) pts.push(inF([100 + (dir ? dir * (profileEdge(F) - 4) : 10), DH.LY.mouthY + 4]));
      if (WC.snorkel) pts.push(inF([away ? 100 - DH.hw - 8 : dir ? 100 - dir * 0.1 * DH.hw : 100 + DH.hw + 12, DH.top - 40]));
    }
    const T = figureT, a = (rot || 0) * Math.PI / 180, ca = Math.cos(a), sa = Math.sin(a);
    leakPts = { amt: leakAmt, k: T.k, pts: pts.map(([x, y]) => { const X = x * ca - (y - L) * sa, Y = L + x * sa + (y - L) * ca; return [T.dx + T.k * X, T.dy + T.sg + T.k * (Y - T.g)]; }) };
  } else leakPts = null;
  // (ticket 431) a gnome's hat, stretched with the head as hair is, flying off with the lid
  if (gnome) svg += veil("head", `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})"><g transform="${topFlies(F)}">` +
                    `<g transform="${hairStretch(F)}">${gnomeHat(WC, dir)}</g></g></g>`);
  // (ticket 522) a cowboy hat, as the gnome's is, its brim wider over big hair
  if (western && W.H) svg += veil("head", `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})"><g transform="${topFlies(F)}">` +
                    `<g data-western="hat" transform="${hairStretch(F)}">${cowHatSvg(W, dir, away, hairReach(state.hair).half + 12)}</g></g></g>`);
  // (ticket 435) a knight's helmet: the trap room's great helm, worn over the whole head (closed, as there; open, the face in
  // its window), its plume falling back, mirrored facing our right, its back from behind
  if (knight) svg += veil("head", `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})"><g transform="${topFlies(F)}">` +
                     `<g transform="${hairStretch(F)}">${helmetSvg(100, 204, 3.45, { worn: true, plume: WC.plume !== false ? WC.plumeColor || "#c0392b" : false, side: dir, back: away, open: !!WC.helmOpen && !away && !dir })}</g></g></g>`);
  if (royal) svg += veil("head", `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})"><g transform="${topFlies(F)}">` +
                    `<g transform="${hairStretch(F)}">${crownSvg(WC, king, robeC, away)}</g></g></g>`);
  // (ticket 515) a robot's head, over the whole head (in the face's units as they are, as the old diving helmet is), and a
  // faceplate's own eyes on the eyes' veil
  if (robot) svg += veil("head", inFace(`<g data-robot="head" transform="${topFlies(F)}">${RH.svg}</g>`)) + (RH.eyes ? veil("eyes", inFace(`<g data-robot="eyes" transform="${topFlies(F)}">${RH.eyes}</g>`)) : "");
  if (robin) svg += veil("head", `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})"><g transform="${topFlies(F)}">` +
                    `<g transform="${hairStretch(F)}">${robinHat(WC, dir, away)}</g></g></g>`);
  if (wizard) svg += veil("head", `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})"><g transform="${topFlies(F)}">` +
                     `<g transform="${hairStretch(F)}">${wizardHat(WC, brimRx, dir)}</g></g></g>`);
  // a witch's hat over it, stretched with the head as hair is (and flying
  // off with the lid); her cat on her shoulder
  if (witch) {
    svg += veil("head", `<g transform="translate(${f1(tx)} ${f1(ty)}) scale(${s.toFixed(4)})"><g transform="${topFlies(F)}">` +
           `<g transform="${hairStretch(F)}">${witchHat(WC, brimRx, dir, away)}</g></g></g>`);
    // (the cat grows or shrinks about where it sits)
    if (WC.witchCat)
      svg += `<g transform="translate(${f1(-(w - 4))} -9) scale(${((WC.catSize ?? 100) / 100).toFixed(3)}) translate(${f1(w - 4)} 9)">` +
             `${shoulderCat(w, WC)}</g>`;
  }

  // (ticket 286: crawling away, the back is nearer than the head: it hides the hair hanging down, and the head shows beyond it)
  // (and the arms, upright, so drawn whole, not seen short: from the shoulders as the back is drawn, behind it, down to
  // the hands flat on the floor beside it)
  const armsAway = squash && away && !winged ? `<g transform="translate(0 ${f1((1 - squash) * (L - arms[0].A[1]))})">` +
                   veil("shirt", arms.map(crawlArm).join("")) + veil("hands", arms.map(flatHand).join("")) + `</g>` : "";
  if (squash && away) svg = svg.slice(0, bodyAt) + svg.slice(bodyEnd) + armsAway + svg.slice(bodyAt, bodyEnd);
  svg += onBack;   // (ticket 138: wings and a tail, seen from behind)
  // then the arms again, over the hair and the face: raised arms cross
  // the chin and cheeks in front of the head, not behind it
  // (only outside the body, where they showed before: the body still
  // covers their tops, outline and all)
  // (a bird suit's wings stay behind the face, or they would hide it)
  const armsAt = svg.length;
  if (!winged && (dir || q) && !squash) {
    // (in profile, or turned partway, the arm on our side wholly in front of the body)
    for (const arm of nearArms) svg += limbThrough(armJoints(arm), armW, limb, limbLine);
    if (sleeve > 0) for (const arm of nearArms) svg += limbThrough(sleeveOf(arm), armW + 14, ss) + limbThrough(sleeveOf(arm), armW + 9, sf);
    if (robot) for (const arm of nearArms) svg += rbG("arm", robotArm(R, arm, { dir, w, armW, sleeve: sleeveOf(arm) }));   // (ticket 515)
    if (western) for (const arm of nearArms) svg += westernArm(W, sleeveOf(arm), armW, arm.side, dir);   // (ticket 522)
    if (sasq) for (const arm of nearArms) svg += sqArm(arm, dir);   // (ticket 530)
    if (ninja) for (const arm of nearArms) svg += njArm(arm);   // (ticket 561)
    if (wrapStripes(C))
      for (const arm of nearArms) { const P = sleeveOf(arm);
        svg += limbStripes(P[0], P[1], armW + 9, C.marks, `arm-stripes${arm.side}`, 22) + (P.length > 2 ? limbStripes(P[1], P[2], armW + 9, C.marks, `arm-stripes${arm.side}b`, 0) : ""); }
  } else if (!winged && !squash) {
    // (ticket 151: a clip, everywhere but the body grown by 1.5, as the
    // mask with its black body stroked 3 wide was; the mask was drawn again
    // offscreen on every frame: dragging the friend round the castle at 1920
    // wide held 44 frames a second, 60 without it)
    // (ticket 515: the helper bot's egg, drawn in place of the shirt and wider than the shoulders, is white like its sleeves:
    // its arms come out of its sides in front of it, outline and all, unclipped, so a forearm folded across it keeps its edge)
    const eggArms = robot && R.S.torso === "egg";
    if (!eggArms) svg += `<clipPath id="off-body"><path clip-rule="evenodd" d="M-5000 -5000 H5000 V5000 H-5000 Z M${pts(growPoly(undies ? bareBody : torso, 1.5)).replace(/ /g, " L")} Z"/></clipPath>` +
                         `<g clip-path="url(#off-body)">`;
    for (const arm of arms) svg += limbThrough(armJoints(arm), armW, limb, limbLine);
    if (sleeve > 0) for (const arm of arms) svg += limbThrough(sleeveOf(arm), armW + 14, ss);
    if (!eggArms) svg += `</g>`;
    if (sleeve > 0) for (const arm of arms) svg += limbThrough(sleeveOf(arm), armW + 9, sf);
    if (robot) for (const arm of arms) svg += rbG("arm", robotArm(R, arm, { dir: 0, w, armW, sleeve: sleeveOf(arm) }));   // (ticket 515)
    if (western) for (const arm of arms) svg += westernArm(W, sleeveOf(arm), armW, arm.side, 0);   // (ticket 522)
    if (sasq) svg += sqG("arm", shagEdge(arms.map(arm => [armJoints(arm), arm.side]), "many", (armW + 9) / 2, 9, C));   // (ticket 530: both arms, one pair of shapes)
    if (ninja) for (const arm of arms) svg += njArm(arm);   // (ticket 561)
    // (from past the shoulder, where the fur sleeve joins the body)
    if (wrapStripes(C))
      for (const arm of arms) { const P = sleeveOf(arm);   // (round a bent elbow: the upper arm's, then the forearm's)
        svg += P.length === 2 ? limbStripes(P[0], P[1], armW + 9, C.marks, `arm-stripes${arm.side}`, 22)
                              : limbStripes(P[0], P[1], armW + 9, C.marks, `arm-stripes${arm.side}`, 22) + limbStripes(P[1], P[2], armW + 9, C.marks, `arm-stripes${arm.side}b`, 0); }
  }
  veilFrom("shirt", armsAt);

  // (ticket 518) what the hands hold, but the far hand's (drawn behind the body, above): the costumes' own in the order they
  // always were (the broom, the bow, the scepter, the sword, the shield, the staff, the lasso), each in its view's own drawing
  for (const h of held) if (!heldFar.includes(h)) svg += heldDraw(h);

  // and the hands last, in front of everything, the face too: raised
  // arms put them by the cheeks or over the eyes, not behind the head
  // (ticket 438) the horse's rein, from its noseband's ring to the hand nearer it, hanging a little: under the hand
  if (horseBits && nearArms.length) {
    // (ridden: each near hand's rein to the bit on its side (in profile, the one rein to the ring), in the lifted frame;
    // from behind, hidden by them)
    if (!away)
    // (ticket 518: a hand holding something else lets go of its rein)
    for (const arm of nearArms.filter(a => held.every(h => h.arm !== a || h.item === "rein"))) { const [x0, y0] = horseBits[horseBits.length > 1 && arm.H[0] > 0 ? 1 : 0], [x1, y1] = arm.H;
      svg += `<path class="rein" d="M${f1(x0)} ${f1(y0)} Q${f1((x0 + x1) / 2)} ${f1(Math.max(y0, y1) + 12)} ${f1(x1)} ${f1(y1)}" fill="none" stroke="${state.body.horseTack || "#7a3a1a"}" stroke-width="5" stroke-linecap="round"/>`; }
  } else if (horseRing && nearArms.length) {
    // (ticket 518: in the hand chosen to hold it, else the nearer near hand)
    const d = a => Math.hypot(a.H[0] - horseRing[0], a.H[1] - horseRing[1]), hand = (held.find(h => h.item === "rein") || {}).arm || nearArms.reduce((a, b) => d(b) < d(a) ? b : a);
    const [x0, y0] = horseRing, [x1, y1] = hand.H, sag = 30 + 0.12 * Math.hypot(x1 - x0, y1 - y0);
    svg += `<path class="rein" d="M${f1(x0)} ${f1(y0)} Q${f1((x0 + x1) / 2)} ${f1(Math.max(y0, y1) + sag)} ${f1(x1)} ${f1(y1)}" fill="none" stroke="${state.body.horseTack || "#7a3a1a"}" stroke-width="5" stroke-linecap="round"/>`;
  }
  const handsAt = svg.length;
  if (!winged && !(squash && away)) {
    // (ticket 286: crawling toward us, each hand flat on the floor; away, drawn with the arms, behind the back, above)
    for (const arm of nearArms) svg += crawlTo ? flatHand(arm) : handOf(arm);
  }
  veilFrom("hands", handsAt);
  // a ghost: the sheet over all of the friend instead (what was drawn of
  // them is dropped, and its see-through layers with it)
  if (kind === "ghost") {
    svg = svg.slice(0, figAt);
    for (const g of Object.keys(layers)) delete layers[g];
    const gh = ghostSvg(WC, { F, tx, ty, s, w, L, feetY, arms, armW });
    svg += veil("shirt", gh.sheet) + (away ? "" : veil("head", gh.mouth) + veil("eyes", gh.eyes));   // (from behind, no face: ticket 138)
  }
  // standing in water: see-through below the waist, a wake round them (ticket 122)
  // (in a stream, to the knees; the wake out to the nearer bank at most)
  if (wet) {
    // (the water's shape round where they stand, from a little above it, for
    // the rings' far halves, down past their feet; in their own units)
    const T = figureT, sx = T.dx, sy = T.sg + T.dy, wake = Math.max(0, Number(BG.waterWake ?? 100) || 0) / 100;
    const most = waterAcross(BG, sx, sy, 400) / T.k;
    const R = Math.min(420, (hw + 80 + Math.min(220 * wake, most)) * T.k), down = (Math.max(ground, feetY + 40) - wl + 30) * T.k;
    const water = waterRuns(BG, bgText, Math.round(sx - R), Math.round(sx + R), Math.round(sy - 0.2 * R - 12), Math.round(sy + down))
      .map(([x, y, w]) => `<rect x="${f1((x - sx) / T.k)}" y="${f1((y - sy) / T.k + T.g)}" width="${f1(w / T.k)}" height="${f1(2 / T.k + 0.6)}"/>`).join("");
    const inner = swimming && rot ? `<g transform="rotate(${rot} 0 ${f1(L)})">${svg.slice(figAt)}</g>` : svg.slice(figAt);
    svg = svg.slice(0, figAt) + wading(BG, inner, wl, hw, most, tread ? tread.bob : 0, water);
    // (ticket 143) a splash where a jump leaves the water or comes back in:
    // drops thrown up and out in a crown, falling and fading
    if (tread && tread.splash) {
      const { x, p } = tread.splash, r = hw + 10;
      for (let i = 0; i < 9; i++) { const a = Math.PI * (0.1 + 0.8 * i / 8), sp = 40 + 50 * Math.abs(Math.sin(i * 1.7));
        svg += `<circle cx="${f1(x + Math.cos(a) * (r * 0.6 + sp * p) * 1.3)}" cy="${f1(wl - Math.sin(a) * sp * 1.6 * p + 160 * p * p)}" r="${f1(6 - 3 * p)}" fill="#ffffff" opacity="${(0.9 * (1 - p)).toFixed(2)}"/>`; }
      svg += `<ellipse cx="${f1(x)}" cy="${f1(wl)}" rx="${f1(r * (0.8 + 0.8 * p))}" ry="${f1(r * 0.16 * (0.8 + 0.8 * p))}" fill="none" stroke="#ffffff" stroke-width="5" opacity="${(0.9 * (1 - p)).toFixed(2)}"/>`;
    }
  }
  // (ticket 438: riding, the friend lifted onto the saddle, and the horse's near parts over their legs)
  if (lift) svg = svg.slice(0, figAt) + `<g class="rider" transform="translate(0 ${f1(lift)})">${svg.slice(figAt)}</g>` + horseOver;
  svg += horseFront;   // (ticket 496: led from its far side, the horse in front of the friend)
  svg += `</g>`;   // the figure
  // (ticket 136: the far leg first, see-through with the pants and the feet,
  // then the far arm, see-through with the shirt, as it was)
  const farLegSvg = legApart ? veil("pants", farLeg.skin) + veil("feet", farLeg.shoe) + veil("pants", farLeg.pants) : "";
  if (farDeep || farLegSvg) svg += `<g id="figure-far" transform="translate(${f1(figureT.dx)} ${f1(figureT.dy + figureT.sg)}) scale(${figureT.k.toFixed(3)}) translate(0 ${f1(-figureT.g + lift)})${rotTf}">${farLegSvg}${veil("shirt", farDeep)}</g>`;
  svg = unveil(svg, layers);
  // a held joint: a ring round it, where the finger is
  if (poseHeld && poseJoints) {
    const J = poseHeld.what === "hand" || poseHeld.what === "elbow" ? poseJoints.arms.find(x => x.side === poseHeld.side) : poseJoints.legs.find(x => x.side === poseHeld.side);
    const [jx, jy] = { hand: J.H, elbow: J.E, foot: J.F, knee: J.K }[poseHeld.what], T = figureT;
    svg += `<circle cx="${f1(T.dx + T.k * jx)}" cy="${f1(T.dy + T.sg + T.k * (jy - T.g))}" r="${f1(28 * T.k + 6)}" fill="#ffffff" fill-opacity="0.3" stroke="#ffffff" stroke-width="2.5" pointer-events="none"/>`;
  }

  const pic = document.getElementById("pic");
  // (kept as it was while a joint is held: the picture's frame growing
  // round a reaching hand would slide it out from under the finger)
  // (ticket 151: only when it changes. Setting it, even to what it was,
  // makes the browser paint the whole picture again, every drawing: walking
  // in the castle at 1920 wide held 56 frames a second, 60 without)
  const vb = `${f1(left)} ${f1(top)} ${f1(width)} ${f1(bottom - top)}`;
  if (!poseHeld && pic.getAttribute("viewBox") !== vb) pic.setAttribute("viewBox", vb);
  // the scenery: the node parsed from it last time, if it has not changed
  // (a figure's slider redraws the figure, not the scene), and left where
  // it is: the rest is replaced around it. Taking it out and putting it
  // back on every redraw restarted all of its animations and re-rendered
  // all of it, so dragging the friend past a cauldron's fumes crawled.
  const fresh = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  fresh.innerHTML = svg;
  if (scened && bgText !== bgShown.text) { bgShown.node = mountScene(bgText); bgShown.text = bgText; bgShown.objs = sceneObjects(bgShown.node); bgShown.helix = true; }
  if (!scened) bgShown.objs = [];
  const scenery = scened ? SPEED_TEST.holder || bgShown.node : null;   // (🧪's test passes: SPEED_TEST)
  for (const c of [...pic.childNodes]) if (c !== scenery && !(c.classList && c.classList.contains("fig-spin"))) c.remove();   // (ticket 193: a spin's copy stays)
  for (const n of figShown) n.remove();   // (last time's friend, who may have stood inside the scene)
  for (const w of pic.querySelectorAll("g.fig-clip")) w.remove();   // (ticket 199: and their clip, climbing down)
  if (scenery) {
    if (scenery.parentNode !== pic) pic.append(scenery);
    if (fresh.firstChild) pic.insertBefore(fresh.firstChild, scenery);   // the defs, before the scene
  }
  // (ticket 184: arrived inside something, out of it, once the place is on the screen to measure)
  // (not on a bridge or a platform, sitting or climbing: those stand where they stand)
  if (arriveSettle && scened) { arriveSettle = false;
    if (!state.body.sit && !BG.onBridge && !BG.onPlatform && !climb) { const was = BG.figX; solidUnstick(400); if (BG.figX !== was) setTimeout(changed, 0); }
    if (arriveSide) { const sd = arriveSide; arriveSide = null; setTimeout(() => arrowsClear(sd), 0); } }   // (ticket 312: off the nav arrows, once drawn)
  // the friend, among the scene's objects by depth (ticket 99): just before
  // the first standing nearer than their feet, or in front of everything
  figShown = [...fresh.childNodes];
  // (seated, they are sorted at the seat's own depth: ticket 50)
  const onSeat = scenery && !poseHeld ? seatNow() : null;
  // (ticket 245: on a bridge, just in front of the tree house, all of them: the far arm too)
  const feetZ = onSeat ? onSeat.z : figOver && scened ? figOver.z : onBr ? SCENE.ground + 0.5 : figureT.sg + figureT.dy;   // (ticket 331: out a door, at the door's depth)
  // (ticket 130: the far arm, when it is apart, a body's depth farther back:
  // before the first thing standing nearer than that, so behind the rest)
  const farNode = figShown.find(n => n.id === "figure-far"), rest = figShown.filter(n => n !== farNode);
  // (ticket 335: up on a bridge or a platform, just before the first tree house lower than their feet, the nearer houses below
  // covering them; else as before)
  const up = onBr && scenery && !SPEED_TEST.holder ? houseAhead(onBr.y) : null;
  const behind = farNode && scenery && !SPEED_TEST.holder ? up || objectAhead(onBr || figOver ? feetZ : feetZ - 26 * figureT.k) : null;
  const ahead = scenery && !SPEED_TEST.holder ? up || objectAhead(feetZ) : null;
  // (ticket 164: on a slow computer, while the friend moves, the scene as
  // still pictures at its end, cut where the friend goes, the friend between)
  const stills = scened && scenery === bgShown.node ? stillLayers(farNode, behind, ahead) : null;
  if (stills) {
    const B = stills[1] && stills[1].img, C = stills[2] && stills[2].img;
    if (farNode) scenery.insertBefore(farNode, B || C || null);
    for (const n of rest) scenery.insertBefore(n, C || null);
  } else {
    if (behind) behind.parentNode.insertBefore(farNode, behind);
    else if (farNode) pic.append(farNode);
    if (ahead) for (const n of rest) ahead.parentNode.insertBefore(n, ahead);
    else pic.append(...rest);
  }
  peopleShow(pic, scenery, feetZ);   // (ticket 439: the people put in this place, among its objects by depth)
  showObjects();
  // (ticket 268) the spiral staircase's steps in front of the friend: each one that covers them seen through, while it does
  if (scened && BG.kind === "castleroom" && scenery) { const steps = scenery.querySelectorAll("[data-front-step]"), f = steps.length && document.getElementById("figure");
    if (f) { const r = f.getBoundingClientRect(); for (const st of steps) { const q = st.getBoundingClientRect(), over = q.right > r.left && q.left < r.right && q.bottom > r.top && q.top < r.bottom;
      if (st.classList.contains("seen-through") !== over) st.classList.toggle("seen-through", over); } } }
  if (scened && BG.kind === "treeroom") treeBusyMeasure();   // (ticket 242: where its pictures can hang, measured once a room is drawn)
  if (hoverOn) showHover();   // (the hover glow, over the new drawing: ticket 135)
  if (scened) showEnterGlow();   // (the passage X, Enter or a tap would take, and the others within reach: tickets 145, 187)
  // (the fumes' Web Animations, for the segments the new scene brought: ticket 118)
  if (bgShown.helix && scenery) { bgShown.helix = false; if (!sceneryStill()) startHelix(scenery); }   // (ticket 164: the fumes held still on a slow computer)
  if (stills || stillPic.on) showStillPic(stills);   // (ticket 164: the still pictures on, or off again)
  if (lieAlong) lyingAlong(lieAlong);   // (ticket 412: lying along a sleeping bag, a view of its own)
  if (figAway || figBurst) for (const n of figShown) if (n.style) n.style.visibility = "hidden";   // (ticket 349: and burst into bubbles)   // (ticket 193: a spin's copy is shown instead)
  // (ticket 199) climbing down a hatch: every piece of the friend cut off below its near edge, in the picture's own units
  // (ticket 252) and climbing up through a ceiling's opening: cut off above it
  const clipDown = climb && climb.down && climb.clip != null, clipUp = climb && !climb.down && climb.clipUp != null && climb.door;
  // (ticket 264, Matthew: "we should clip the animation of walking through it, in the planned motion -- the trap doors and
  // pits all look great, BTW -- that's the standard now") walking through a door in a side wall (doorClip, walkInto):
  // every piece of the friend cut off past the wall's line there, on the room's side only
  const clipSide = !!doorClip && entering, clipRect = figClipRect && entering ? figClipRect : null;   // (ticket 269: in through a house's door, only its opening seen)
  // (ticket 274) each clip named by its shape and its turn, not by chance (Math.random), so the drawing is the same every time
  let clipN = 0;
  if (((clipDown || clipUp) && B.anim === "climbup") || clipSide || clipRect) for (const n of figShown) {
    if (n.nodeType !== 1 || !n.parentNode) continue;
    const NS = "http://www.w3.org/2000/svg", w = document.createElementNS(NS, "g"), shape = (clipRect ? clipRect.svg || `<rect x="${f1(clipRect.x0)}" y="${f1(clipRect.y0)}" width="${f1(clipRect.x1 - clipRect.x0)}" height="${f1(clipRect.y1 - clipRect.y0)}"/>` : clipSide ? (doorClip.side < 0 ? `<rect x="${f1(doorClip.x)}" y="-4000" width="8000" height="8000"/>` : `<rect x="-8000" y="-4000" width="${f1(8000 + doorClip.x)}" height="8000"/>`)
      : clipDown ? `<rect x="-4000" y="-4000" width="8000" height="${f1(4000 + climb.clip)}"/>`
      : `<rect x="-4000" y="${f1(climb.clipUp)}" width="8000" height="8000"/>`);
    let hx = 2166136261; for (let i = 0; i < shape.length; i++) hx = Math.imul(hx ^ shape.charCodeAt(i), 16777619);
    const id = "fig-clip-" + (hx >>> 0).toString(36) + "-" + clipN++;
    w.setAttribute("class", "fig-clip"); w.setAttribute("clip-path", `url(#${id})`);
    w.innerHTML = `<clipPath id="${id}" clipPathUnits="userSpaceOnUse">` + shape + `</clipPath>`;
    n.replaceWith(w); w.append(n);
  }
  worldWaysHud(); pathHud(); solidsHud(); madeOfHud(); accessApply(); bubbleRun(); makerRun(); leakRun(); ballRun(); foamLayersRun(); bubbleHitsHud(); waterHud(); burstKeep();   // (0.0.145: the arrows placed again on every drawing too)   // (ticket 220: bubble space's bubbles, moving while it is shown)   // (ticket 217: the planned path and the collision boxes, when ♿ shows them; the words' size)
  if (scened) { stillPic.draws++; if (stills) stillPic.drawsOn++; }   // (for 🧪)
  // a blown lid tilts about the crack, and on a tall head its top (and
  // long hair or a hood's back below it) swings far out: frame what was
  // drawn, kept centred
  if (F.burst > 0 && !scened && !poseHeld) {
    const b = pic.getBBox(), l = Math.min(left, b.x - 12, -(b.x + b.width + 12));
    const t = Math.min(top, b.y - 12), btm = Math.max(bottom, b.y + b.height + 12);
    if (l < left || t < top || btm > bottom)
      pic.setAttribute("viewBox", `${f1(l)} ${f1(t)} ${f1(-2 * l)} ${f1(btm - t)}`);
  }
  // the feeling, and where, as each place takes it (PLACE_PHRASE)
  const BGK = state.background.kind, where = PLACE_PHRASE[BGK];
  const place = typeof where === "function" ? where(state.background) : where || "";
  document.getElementById("mood").textContent = "feeling " + mood(family(F.color), F.brow, F.smile) + (place ? " " + place : "") +
    (B.anim === "climbup" && (!scened || (climb && climb.none)) ? (climb && climb.far ? " (🧗 put them by a ladder to climb it)" : " (🧗 no ladder here to climb)") : "");
  fitPic();
  if (move && !move.held && !treadLoop) treadLoop = requestAnimationFrame(treadTick);
  if (B.anim === "climbup" && scened && !climb && !drag && !entering && !climbLook) climbLook = setTimeout(startClimb, 0);   // (ticket 137)
  if (scened && marks.text !== bgShown.text && !marksLook) marksLook = setTimeout(measureMarks, 0);   // (tickets 145, 187: with any input, not only a controller)
}
// (ticket 546, M: in a dress, riding the horse, squatting or running, "the legs stick out of the dress as if it had strange
// holes in it") A skirt, a dress or a robe is drawn standing: straight sides from the waist (±hw at y0) flaring to its hem
// (±fl at hemY). Legs posed out past those sides (astride the horse, the knees out in a crouch, a stride's thighs, a lap
// sitting) came out through its side, the cloth carrying on below them. Then the cloth is the convex outline over the waist,
// the hem's ends and every part of each leg above the hem (each leg's joints `legs`, round as thick as the leg is drawn,
// `r`), so a leg only ever comes out from under the hem. Above the waist line it stays the waist, except where a thigh
// reaches out past the hips (sitting in profile: the lap). null while the legs keep inside the straight sides (standing,
// and most steps): drawn exactly as before. Else { side, xL, xR }: the outline from the hem's left end, up over the waist,
// down to its right end, and the hem's two ends.
function skirtOver(legs, r, hw, y0, fl, hemY) {
  const K = 12, rr = r / Math.cos(Math.PI / K), pts = [[-hw, y0], [hw, y0], [fl, hemY], [-fl, hemY]];
  const ring = ([x, y]) => { for (let i = 0; i < K; i++) { const a = 2 * Math.PI * i / K, px = x + rr * Math.cos(a), py = y + rr * Math.sin(a);
    pts.push([px, Math.min(hemY, Math.abs(px) <= hw ? Math.max(y0, py) : py)]); } };
  for (const J of legs) {
    // (down the leg from the hip, to where it first goes under the hem)
    if (J[0][1] < hemY) ring(J[0]);
    for (let i = 1; i < J.length; i++) { const A = J[i - 1], B = J[i];
      if (B[1] <= hemY) { ring(B); continue; }
      if (A[1] < hemY) ring([A[0] + (B[0] - A[0]) * (hemY - A[1]) / (B[1] - A[1]), hemY]);
      break; }
  }
  const inside = ([x, y]) => y >= y0 - 0.5 && Math.abs(x) <= hw + (fl - hw) * Math.max(0, Math.min(1, (y - y0) / (hemY - y0))) + 1.5;
  if (pts.every(inside)) return null;
  // (the convex outline: Andrew's monotone chain, on points rounded as they are drawn)
  const P = pts.map(([x, y]) => [Math.round(x * 10) / 10, Math.round(y * 10) / 10]).sort((a, b) => a[0] - b[0] || a[1] - b[1]);
  const cross = (o, a, b) => (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0]);
  const chain = list => { const h = []; for (const p of list) { while (h.length >= 2 && cross(h[h.length - 2], h[h.length - 1], p) <= 0) h.pop(); h.push(p); } h.pop(); return h; };
  const hull = [...chain(P), ...chain(P.slice().reverse())], n = hull.length, hy = Math.round(hemY * 10) / 10;
  const low = hull.map((p, i) => i).filter(i => hull[i][1] >= hy);
  const iL = low.reduce((a, b) => (hull[b][0] < hull[a][0] ? b : a)), iR = low.reduce((a, b) => (hull[b][0] > hull[a][0] ? b : a));
  // (from the hem's left end the other way round from its right end, up the left side, over the waist and down the right)
  const d = (iL + 1) % n === iR ? -1 : 1, side = [];
  for (let i = iL; ; i = (i + d + n) % n) { side.push(hull[i]); if (i === iR) break; }
  return { side, xL: hull[iL][0], xR: hull[iR][0] };
}
// (ticket 123) While the friend treads water they are drawn again about 30
// times a second: not while a drag draws them anyway, nor while the page is
// hidden. Only the friend is drawn; the scene is kept (see draw).
// (ticket 414, Matthew: "two sliders for elbows bend --- elbow muscle in-out (like doing a curl with a weight); elbow rotation,
// side to side (like moving a tea kettle from one side of the table to another, by swing our arm, and rotating laterally at
// elbow)"; "same for knees too") A limb's second bone turned in three dimensions, then seen: root R, joint J and tip T in the
// picture's units (the first bone R to J stays as posed), the second bone curled by `curl` toward `front` (a direction: toward
// us, or the way they face in profile; for a knee, the back), then swung by `swing` about the first bone's line. The tip is
// where the bone then points, seen from the front of the picture: a bone pointing toward us or away is seen short. Its angle
// in the picture too (as a hand's), and its depth (toward us +). null when there is nothing to turn.
function turned3(R, J, T, curl, swing, front) {
  if (!curl && !swing) return null;
  const ux = J[0] - R[0], uy = J[1] - R[1], ul = Math.hypot(ux, uy), vx = T[0] - J[0], vy = T[1] - J[1], L = Math.hypot(vx, vy);
  if (!(ul > 0) || !(L > 0)) return null;
  const u = [ux / ul, uy / ul, 0], dot = (p, q) => p[0] * q[0] + p[1] * q[1] + p[2] * q[2];
  let v = [vx / L, vy / L, 0];
  if (curl) { const fp = front.map((c, i) => c - dot(front, v) * v[i]), fl = Math.hypot(...fp);
    if (fl > 1e-6) v = v.map((c, i) => Math.cos(curl) * c + Math.sin(curl) * fp[i] / fl); }
  if (swing) { const c = Math.cos(swing), sn = Math.sin(swing), k = dot(u, v), x = [u[1] * v[2] - u[2] * v[1], u[2] * v[0] - u[0] * v[2], u[0] * v[1] - u[1] * v[0]];
    v = v.map((vi, i) => vi * c + x[i] * sn + u[i] * k * (1 - c)); }
  let seen = Math.hypot(v[0], v[1]);
  // (a bone pointing straight at us or away is seen end on: kept a sliver long, its way as before, so what is drawn along it
  // (a sleeve, a pants leg) has a way to go: at 0 length the pants and the shirt were lost to NaN)
  if (seen < 0.06) { const ox = vx / L, oy = vy / L, w0 = seen > 1e-9 ? 0.06 / seen : 0;
    v = seen > 1e-9 ? [v[0] * w0, v[1] * w0, v[2]] : [ox * 0.06, oy * 0.06, v[2]]; seen = 0.06; }
  return { J, T: [J[0] + L * v[0], J[1] + L * v[1]], angle: seen > 0.05 ? Math.atan2(v[1], v[0]) * 180 / Math.PI : null, depth: L * v[2], dz: v[2] };
}
// (ticket 412, Matthew: "lying with head and feet at a 45 degree angle --- as in the tent. it's specialized to that, I think.
// L/R symmetry works, but otherwise it's specialized for laying down near the L or R wall edge of a room, where the pseudo 3d
// perspective requires a new approach, just for that"; before it, "we just need more angles of the body, as their own drawing
// routine, not cheap 2d transformations of the poses that we have now") Lying face up along a sleeping bag by a side wall, the
// feet at its open end toward us, the head on its pillow by the far wall: a view of the friend of its own, drawn in the bag's
// perspective, not the standing friend turned or skewed. The figure's group holds this drawing instead, in the picture's units.
function lyingAlong(A) {
  const f = document.getElementById("figure");
  if (!f) return;
  f.removeAttribute("transform");
  f.innerHTML = lieAlongSvg(A, state);
  const far = document.getElementById("figure-far"); if (far) far.style.display = "none";
}
// The body is volumes on the bag's floor, placed by at(xr, a, h): xr along the bag from its open end (0) to its far end (LIE_LEN),
// a across it (-0.5 its edge by the wall, 0.5 the room's), h up from the floor, all in the bag's widths; the bag's own edges
// run to where the tent's seams meet (vy), so a step along it is shorter on the screen the farther it is. Drawn far to near
// and the wall's side before the room's, so what is nearer us covers what is farther (Matthew: "the torso obscures the back
// arm, etc"): the hair on the pillow, the head (a ball: round from any side) and its face, looking up; the neck; the arm by
// the wall; the body, its top and its side toward the room; the room's arm; the legs; the feet, pointing up. The friend's own
// colours: skin, hair, shirt (its sleeves by style), pants (long or short), shoes. Not yet: costumes, hair styles one by one,
// the face's own settings (eyes, brows), patterns.
const LIE_LEN = 2.4;
function lieAlongSvg(A, S) {
  const { xa, xb, fy, ny, vy, side } = A, i0 = 1 / (ny - vy), i1 = 1 / (fy - vy), cx = (xa + xb) / 2, Wf = Math.abs(xb - xa);
  const at = (xr, a, h = 0) => { const y = vy + 1 / (i0 + (i1 - i0) * xr / LIE_LEN), m = (y - vy) / (fy - vy); return [(cx - side * a * Wf) * m, y - h * Wf * m, Wf * m]; };
  const P = p => `${f1(p[0])} ${f1(p[1])}`, lw = p => f1(Math.max(0.6, 0.018 * p[2]));
  // (a limb: tapered, round at both ends, from one point to another, each with its radius in the bag's widths)
  const capsule = (p0, r0, p1, r1, fill) => { const R0 = r0 * p0[2], R1 = r1 * p1[2], dx = p1[0] - p0[0], dy = p1[1] - p0[1], d = Math.hypot(dx, dy) || 1, nx = -dy / d, ny2 = dx / d;
    const A0 = [p0[0] + nx * R0, p0[1] + ny2 * R0], B1 = [p1[0] + nx * R1, p1[1] + ny2 * R1], C1 = [p1[0] - nx * R1, p1[1] - ny2 * R1], D0 = [p0[0] - nx * R0, p0[1] - ny2 * R0];
    return `<path d="M${P(A0)} L${P(B1)} A${f1(R1)} ${f1(R1)} 0 0 0 ${P(C1)} L${P(D0)} A${f1(R0)} ${f1(R0)} 0 0 0 ${P(A0)} Z" fill="${fill}" stroke="${darker(fill)}" stroke-width="${lw(p0)}" stroke-linejoin="round"/>`; };
  const skin = S.face.color, hair = S.hair.color, shirt = S.shirt.color, pants = S.pants.color, shoe = S.shoes.color;
  const sleeve = S.shirt.style === "tank" ? 0 : S.shirt.style === "long" ? 1 : 0.45, longPants = !/short|skirt/.test(S.pants.style || "long");
  // (ticket 425: each part as see-through as its 👁️ slider says, as standing: the arms, the neck and the body the shirt's, the
  // legs the pants', the hands and the feet their own)
  const seen = visibility(), V = (part, svg) => !svg || seen[part] >= 1 ? svg : seen[part] <= 0 ? "" : `<g opacity="${seen[part].toFixed(2)}">${svg}</g>`;
  let s = "";
  // the head: a ball on the pillow, round on the screen, the face up (Matthew: "the face should be facing up -- not looking
  // towards us"), the hair on its far side (Matthew: "the hair should be on the far face of the head ball, not the top of it
  // (the head is reclined now, looking upward)"), behind the head and spread on the pillow
  const hx = 1.8, hc = 0.29, hr = 0.25;   // (the head smaller than the friend's own, so that, a ball raised by the pillow, it stays on the bag)
  const hp = at(hx, 0, hc), R = hr * hp[2], hair3 = lyingHair(S.hair, { at, P, lw, hx, hp, R, hair });
  s += V("hair", hair3.behind);
  let head = `<circle class="lie-head" cx="${f1(hp[0])}" cy="${f1(hp[1])}" r="${f1(R)}" fill="${skin}" stroke="${darker(skin)}" stroke-width="${lw(hp)}"/>`;
  // Lying face up, seen from the feet and above: the face looks at the ceiling, so it shows on the head's upper part, shortened,
  // its forehead and hairline at the far top, then the eyes, the nose and, nearer us, the mouth; below it the underside of
  // the chin and jaw, in shade. The ears at the head's sides.
  const lo = (y0, bow, a) => `<path d="M${f1(hp[0] - Math.sqrt(R * R - (y0 - hp[1]) ** 2))} ${f1(y0)} Q${f1(hp[0])} ${f1(y0 + bow)} ${f1(hp[0] + Math.sqrt(R * R - (y0 - hp[1]) ** 2))} ${f1(y0)} ` +
    `A${f1(R)} ${f1(R)} 0 ${y0 > hp[1] ? 0 : 1} 1 ${f1(hp[0] - Math.sqrt(R * R - (y0 - hp[1]) ** 2))} ${f1(y0)} Z" ${a}/>`;
  head += lo(hp[1] + 0.1 * R, 0.14 * R, `fill="${darker(skin)}" opacity="0.45"`);   // (under the chin)
  for (const sd of [-1, 1]) head += `<ellipse cx="${f1(hp[0] + sd * 0.97 * R)}" cy="${f1(hp[1] - 0.2 * R)}" rx="${f1(0.13 * R)}" ry="${f1(0.2 * R)}" fill="${skin}" stroke="${darker(skin)}" stroke-width="${lw(hp)}"/>`;
  s += V("head", head) + V("hair", hair3.over);
  let eyes = "";
  for (const sd of [-1, 1]) { const ex = hp[0] + sd * 0.27 * R, ey = hp[1] - 0.44 * R;
    // (its whites, the pupils at their far edge: looking up at the ceiling, past the forehead)
    eyes += `<ellipse cx="${f1(ex)}" cy="${f1(ey)}" rx="${f1(0.1 * R)}" ry="${f1(0.06 * R)}" fill="#ffffff" stroke="${darker(skin)}" stroke-width="${f1(Math.max(0.5, 0.02 * R))}"/>` +
            `<ellipse cx="${f1(ex)}" cy="${f1(ey - 0.025 * R)}" rx="${f1(0.05 * R)}" ry="${f1(0.035 * R)}" fill="#1f1f1f"/>` +
            `<path d="M${f1(ex - 0.12 * R)} ${f1(ey - 0.1 * R)} Q${f1(ex)} ${f1(ey - 0.14 * R)} ${f1(ex + 0.12 * R)} ${f1(ey - 0.1 * R)}" fill="none" stroke="${darker(hair)}" stroke-width="${f1(Math.max(0.6, 0.035 * R))}" stroke-linecap="round"/>`; }
  s += V("eyes", eyes);
  s += V("head", `<path d="M${f1(hp[0] - 0.06 * R)} ${f1(hp[1] - 0.22 * R)} Q${f1(hp[0])} ${f1(hp[1] - 0.15 * R)} ${f1(hp[0] + 0.06 * R)} ${f1(hp[1] - 0.22 * R)}" fill="none" stroke="${darker(skin)}" stroke-width="${f1(Math.max(0.6, 0.04 * R))}" stroke-linecap="round"/>` +   // (the nose)
    `<path d="M${f1(hp[0] - 0.15 * R)} ${f1(hp[1] - 0.06 * R)} Q${f1(hp[0])} ${f1(hp[1] + 0.02 * R)} ${f1(hp[0] + 0.15 * R)} ${f1(hp[1] - 0.06 * R)}" fill="none" stroke="#7a2e2e" stroke-width="${f1(Math.max(0.7, 0.05 * R))}" stroke-linecap="round"/>`);
  // the neck
  s += V("shirt", capsule(at(1.56, 0, 0.17), 0.1, at(1.64, 0, 0.2), 0.1, skin));
  // an arm, shoulder to hand, along the body: its sleeve as the shirt's style, then the hand
  const arm = ac => { const sh = at(1.5, ac, 0.12), hd = at(0.97, ac * 1.04, 0.07), el = at(1.5 - 0.53 * sleeve, ac * 1.02, 0.1);
    return V("shirt", (sleeve > 0 ? capsule(sh, 0.085, el, 0.075, shirt) : "") + capsule(sleeve > 0 ? el : sh, 0.068, hd, 0.06, skin)) +
      V("hands", `<circle cx="${f1(hd[0])}" cy="${f1(hd[1])}" r="${f1(0.075 * hd[2])}" fill="${skin}" stroke="${darker(skin)}" stroke-width="${lw(hd)}"/>`); };
  s += arm(-0.38);   // (the arm by the wall: behind the body)
  // the body: its top, the shirt's front, and its side toward the room, seen below that top's edge
  const T = (xr, a) => at(xr, a, xr > 1.2 ? 0.24 : 0.21), B0 = (xr, a) => at(xr, a, 0);
  s += V("shirt", `<path d="M${P(T(0.95, 0.26))} Q${P(T(1.25, 0.33))} ${P(T(1.56, 0.31))} L${P(B0(1.56, 0.31))} Q${P(B0(1.25, 0.33))} ${P(B0(0.95, 0.26))} Z" fill="${darker(shirt)}"/>` +
    `<path d="M${P(T(0.95, -0.26))} Q${P(T(1.25, -0.33))} ${P(T(1.56, -0.31))} Q${P(at(1.66, 0, 0.27))} ${P(T(1.56, 0.31))} Q${P(T(1.25, 0.33))} ${P(T(0.95, 0.26))} Q${P(at(0.86, 0, 0.23))} ${P(T(0.95, -0.26))} Z" ` +
    `fill="${shirt}" stroke="${darker(shirt)}" stroke-width="${lw(T(1.25, 0))}" stroke-linejoin="round"/>`);
  s += V("pants", `<path d="M${P(T(0.96, -0.25))} Q${P(at(0.9, 0, 0.22))} ${P(T(0.96, 0.25))}" fill="none" stroke="${darker(pants)}" stroke-width="${lw(T(0.96, 0))}"/>`);   // (the waist)
  s += arm(0.38);    // (the arm by the room: in front of the body)
  // the legs, hip to ankle, the wall's first; long pants to the ankle, short ones to the knee
  const leg = ac => { const hp2 = at(0.96, ac, 0.11), kn = at(0.55, ac, 0.1), an = at(0.2, ac, 0.08);
    return longPants ? capsule(hp2, 0.11, an, 0.085, pants) : capsule(hp2, 0.11, kn, 0.1, pants) + capsule(kn, 0.085, an, 0.07, skin); };
  s += V("pants", leg(-0.13) + leg(0.13));
  // the feet, pointing up: each shoe standing on its heel at the open end, its sole toward us
  const foot = ac => { const b = at(0.15, ac, 0.03), t = at(0.15, ac, 0.27), sole = at(0.11, ac, 0.05);
    return capsule(b, 0.085, t, 0.08, shoe) + `<ellipse cx="${f1(sole[0])}" cy="${f1(sole[1])}" rx="${f1(0.07 * sole[2])}" ry="${f1(0.035 * sole[2])}" fill="#f2f2f2" opacity="0.8"/>`; };
  s += V("feet", foot(-0.13) + foot(0.13));
  return `<g data-lie="along">${s}</g>`;
}
// (ticket 425) The friend's own hair, lying face up on a pillow, seen from the feet: `behind`, drawn before the head (on the
// pillow, and round the head's far side, the crown toward the pillow), and `over`, the hairline at the head's far edge, its
// fringe deeper with more bangs. Each style as it would lie: none; short, close round the head; long, fanned out on the
// pillow, the farther the longer; curly, a ring of curls; spiky, spikes out from the crown; an afro, a big puff round the
// head; pigtails, two tails out on the pillow, tied.
function lyingHair(H, { at, P, lw, hx, hp, R, hair }) {
  const style = H.style || "short", L = Math.max(0.3, Math.min(4, (H.length ?? 100) / 100)), bangs = Math.max(0, Math.min(3, (H.bangs ?? 100) / 100));
  if (style === "none" || style === "bald") return { behind: "", over: "" };
  const line = darker(hair), sw = lw(hp), F = `fill="${hair}" stroke="${line}" stroke-width="${sw}"`;
  const pillow = (xr, a) => at(xr, a, 0.07);
  // (a soft patch on the pillow round the head: its reach along the bag beyond the head, and across)
  const spread = (along, across) => { const q = [pillow(hx + along, 0), pillow(hx, -across), pillow(hx - 0.2, 0), pillow(hx, across)];
    return `<path d="M${P(q[0])} Q${P(pillow(hx + along, -across))} ${P(q[1])} Q${P(pillow(hx - 0.2, -across))} ${P(q[2])} Q${P(pillow(hx - 0.2, across))} ${P(q[3])} Q${P(pillow(hx + along, across))} ${P(q[0])} Z" ${F}/>`; };
  const rim = (k = 1) => `<ellipse cx="${f1(hp[0])}" cy="${f1(hp[1] - 0.16 * R * k)}" rx="${f1((1 + 0.1 * k) * R)}" ry="${f1((1 + 0.04 * k) * R)}" ${F}/>`;
  // (round the head's far half, n points at r times its radius: angles across the top of the circle on the screen)
  const round = (n, r, from = 200, to = 340) => [...Array(n)].map((_, i) => { const a = (from + (to - from) * i / Math.max(1, n - 1)) * Math.PI / 180; return [hp[0] + Math.cos(a) * r * R, hp[1] + Math.sin(a) * r * R, a]; });
  let behind = "";
  if (style === "short") behind = spread(0.12, 0.22) + rim(0.6);
  else if (style === "long") behind = spread(0.24 + 0.16 * L, 0.3 + 0.1 * L) + rim(1);
  else if (style === "curly") { behind = spread(0.18 + 0.1 * L, 0.26 + 0.08 * L) + rim(1);
    for (const [x, y] of round(9, 1.05, 170, 370)) behind += `<circle cx="${f1(x)}" cy="${f1(y)}" r="${f1(0.24 * R * (0.8 + 0.2 * L))}" ${F}/>`; }
  else if (style === "spiky") { behind = rim(0.8);
    for (const [x, y, a] of round(9, 0.95, 185, 355)) { const len = R * (0.35 + 0.2 * L), w = 0.16 * R, t = [x + Math.cos(a) * len, y + Math.sin(a) * len], n2 = [-Math.sin(a) * w, Math.cos(a) * w];
      behind += `<path d="M${f1(x + n2[0])} ${f1(y + n2[1])} L${f1(t[0])} ${f1(t[1])} L${f1(x - n2[0])} ${f1(y - n2[1])} Z" ${F} stroke-linejoin="round"/>`; } }
  else if (style === "afro") { const r = 1.42 + 0.12 * Math.min(2, L), c = [hp[0], hp[1] - 0.22 * R];
    behind = `<ellipse cx="${f1(c[0])}" cy="${f1(c[1])}" rx="${f1(r * R)}" ry="${f1((r - 0.08) * R)}" ${F}/>`;
    for (let i = 0; i < 14; i++) { const a = i / 14 * 2 * Math.PI; behind += `<circle cx="${f1(c[0] + Math.cos(a) * r * R * 0.95)}" cy="${f1(c[1] + Math.sin(a) * (r - 0.08) * R * 0.95)}" r="${f1(0.24 * R)}" fill="${hair}"/>`; } }
  else if (style === "pigtails") { behind = spread(0.12, 0.2) + rim(0.7);
    // (out to the sides and back along the pillow, never past the bag's edges: the wall is on one side; more length goes along)
    for (const sd of [-1, 1]) { const root = at(hx + 0.02, sd * 0.24, 0.14), mid = pillow(hx + 0.1 + 0.08 * L, sd * Math.min(0.42, 0.36 + 0.04 * L)), tip = pillow(hx + 0.16 + 0.16 * Math.min(2.5, L), sd * Math.min(0.46, 0.4 + 0.04 * L));
      const thick = 0.09 * root[2];
      behind += `<path d="M${P(root)} Q${P(mid)} ${P(tip)}" fill="none" stroke="${line}" stroke-width="${f1(thick * 2 + 2 * +sw)}" stroke-linecap="round"/>` +
                `<path d="M${P(root)} Q${P(mid)} ${P(tip)}" fill="none" stroke="${hair}" stroke-width="${f1(thick * 2)}" stroke-linecap="round"/>` +
                `<circle cx="${f1(root[0] + (mid[0] - root[0]) * 0.25)}" cy="${f1(root[1] + (mid[1] - root[1]) * 0.25)}" r="${f1(thick * 0.9)}" fill="#e11d48"/>`; } }
  else behind = spread(0.2, 0.28) + rim(1);   // (any other style: as before, a patch and the far side)
  // (the hairline: at the far edge, a little lower with more bangs; an afro's and curls' bumpy)
  const hy = hp[1] - (0.9 - 0.08 * Math.min(2, bangs)) * R, hw = Math.sqrt(Math.max(0, R * R - (hy - hp[1]) ** 2));
  const over = `<path d="M${f1(hp[0] - hw)} ${f1(hy)} A${f1(R)} ${f1(R)} 0 0 1 ${f1(hp[0] + hw)} ${f1(hy)} Q${f1(hp[0])} ${f1(hy + (style === "afro" || style === "curly" ? 0.14 : 0.08) * R)} ${f1(hp[0] - hw)} ${f1(hy)} Z" ${F} stroke-linejoin="round"/>`;
  return { behind, over };
}
function treadTick(t) {
  treadLoop = 0;
  if (!moveNow()) return;
  if (!drag && !padMoving && !document.hidden && t - treadLast >= 32) { treadLast = t; draw(); feedMusic(); return; }
  treadLoop = requestAnimationFrame(treadTick);
}

// Where the friend is, after their feeling ("feeling happy in the forest"):
// each place with the word it takes, in, at, on, near or outside of; a
// castle room by its own name and number; a castle by its owner.
const PLACE_PHRASE = { sea: "swimming in 🌊 open water", deepsea: "swimming in the 🪸 deep sea", bubblespace: "in 🫧 bubble space", bubbleroom: "in 🫧 the bubble room", imagination: "in 💭 our imagination", emptyroom: "in the 🔲 empty room", forest: "in the 🌲 forest", underwater: "🐠 under the sea", beach: "at the 🏖️ beach", witch: "in the 🧙 witch's woods",
  cemetery: "in the 🌿 cemetery", lake: "at the 🦆 lake", house: "outside of the 🏠 house", treehouse: "near the 🌳 tree house",
  treeroom: "in the 🪜 tree house", livingroom: "in the 🛋️ living room", witchroom: "in the 🧙 witch's hut", clearing: "in the 🌲 forest clearing",
  hut: "outside the 🛖 grass hut", hutroom: "in the 🛖 grass hut's living room", hutbed: "in the 🛖 grass hut's bedroom", hutpotion: "in the 🛖 grass hut's potion room", hutcenter: "in the 🛖 grass hut's 🔥 center", treetop: "in the ⭕ round room at the top of the 🌳 tree",
  healerroom: "in the 🌿 healer's hut", fairyforest: "in the 🧚 fairy forest", mountains: "on the ⛰️ mountain trail", waterfall: "at the 💦 jungle waterfall",
  campsite: "at the 🏕️ camp site", tent: "in a ⛺ tent", cave: "in the 🪨 cave behind the falls", caveroom: "in the 🦇 cave room", farmyard: "in the 🚜 farm yard", desert: "in the 🏜️ desert", woodpath: "on the 🥾 path through the woods", pasture: "in the 🌾 pasture", crossing: "at the 🛤️ forest crossing",
  castle: b => `outside of ${castleOf(b).id}'s 🏰 castle`,
  castleroom: b => { const K = castleOf(b), n = Math.max(1, Math.min(K.rooms.length, Math.round(b.castleRoom ?? 1)));
    return `in the ${roomLabel(K.rooms[n - 1])} (room ${n} of ${K.id}'s castle)`; } };
