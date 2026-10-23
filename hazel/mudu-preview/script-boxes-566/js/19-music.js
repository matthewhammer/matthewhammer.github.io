"use strict";
// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 178) Part 19 of 26 of the game's script, run in order by index.html.

// ===================== MUSIC =====================
// Each background has a tune of its own, and any tune can play over any
// background (or none). A tune is written as notes, MIDI numbers (60 is
// middle C) in a small text form, and played by little synthesizers made
// of Web Audio oscillators and noise: no sound files. Browsers allow sound
// only after a tap or a key, so the music starts with the first one; it
// rests while the page is hidden.

const NOTE_STEP = { C: 0, D: 2, E: 4, F: 5, G: 7, A: 9, B: 11 };
// "C4" is 60, "F#3" 54, "Bb2" 46
function midiOf(name) {
  const m = /^([A-G])([#b]?)(\d)$/.exec(name);
  if (!m) throw new Error("not a note: " + name);
  return 12 * (Number(m[3]) + 1) + NOTE_STEP[m[1]] + (m[2] === "#" ? 1 : m[2] === "b" ? -1 : 0);
}
// a line of notes, each with its length in steps (sixteenth notes):
// "E5:2 G5:2 C5+E5+G5:4 r:2", "+" for notes played together, "r" a rest,
// "|" a bar line (only for reading). A length may be a fraction, for
// triplets: "E5:4/3" is an eighth-note triplet (three to a beat), "E5:8/3"
// a quarter-note triplet (three to two beats).
// (the steps counted in 720ths, whole numbers, so that thirds, fifths and
// sixths add up exactly and a line ends on its bar)
const TICK = 720;
function lineOf(text) {
  const events = []; let at = 0;
  for (const tok of text.trim().split(/\s+/)) {
    if (tok === "|") continue;
    const [what, len] = tok.split(":"), [a, b = 1] = (len || "").split("/").map(Number), n = a / b;
    if (!(n > 0)) throw new Error("no length: " + tok);
    if (what !== "r") events.push({ at: at / TICK, len: n, notes: what.split("+").map(midiOf) });
    at += Math.round(n * TICK);
  }
  return { events, steps: at / TICK };
}
// a drum's hits, a character a step, repeated: "x" a hit, "o" a soft one.
// Hits in parentheses share a beat (4 steps) evenly, or as many steps as
// the number before them says: "(xxx)" a beat's triplets, "8(xxx)" three
// hits over two beats (against the beat's two: a polyrhythm), "16(xxx)"
// three over the bar's four.
function hitsOf(text, note = 60) {
  const events = []; let at = 0;   // (in 720ths of a step, as lineOf)
  const hit = (ch, ticks) => {
    if (ch === "x" || ch === "o") events.push({ at: at / TICK, len: ticks / TICK, notes: [note], vel: ch === "x" ? 1 : 0.55 });
    at += ticks;
  };
  for (const [, span, group, ch] of text.replace(/[\s|]/g, "").matchAll(/(\d*)\(([^)]*)\)|(.)/g))
    if (ch !== undefined) hit(ch, TICK);
    else for (const c of group) hit(c, Math.round((span ? Number(span) : 4) * TICK / group.length));
  return { events, steps: at / TICK };
}
// a chord by name, its notes up from the root at an octave: "Am", "G7", "Bb"
const CHORD_SHAPES = { "": [0, 4, 7], m: [0, 3, 7], "7": [0, 4, 7, 10], m7: [0, 3, 7, 10], maj7: [0, 4, 7, 11], dim: [0, 3, 6], "9": [0, 4, 7, 10, 14] };   // (ticket 301: the 9, funk's)
function chordOf(name, oct) {
  const m = /^([A-G][#b]?)(.*)$/.exec(name);
  return CHORD_SHAPES[m[2]].map(s => midiOf(m[1] + oct) + s);
}
// Parts made from a tune's chords, one chord a bar of so many steps:
// a bass line, [step, semitones above the root, length] in every bar
const bassOf = (chords, bar, pat, oct) => ({ steps: chords.length * bar, events: chords.flatMap((c, i) =>
  pat.map(([at, up, len]) => ({ at: i * bar + at, len, notes: [chordOf(c, oct)[0] + up] }))) });
// the chord, struck at these steps of every bar
const stabsOf = (chords, bar, ats, len, oct) => ({ steps: chords.length * bar, events: chords.flatMap((c, i) =>
  ats.map(at => ({ at: i * bar + at, len, notes: chordOf(c, oct) }))) });
// a line in one meter over chords in another: a pattern `len` steps long,
// [step, chord tone (0 the root, 1 the next one up...), length, octaves up],
// played over and over, each note taken from the chord sounding at its step
// (one chord every `bar` steps): so a funk bass in 4 keeps to a waltz's chords in 3
const crossOf = (chords, bar, len, pat, oct) => {
  const gcd = (a, b) => b ? gcd(b, a % b) : a, steps = chords.length * bar * len / gcd(chords.length * bar, len), events = [];
  for (let rep = 0; rep * len < steps; rep++) for (const [at, tn, l, up = 0] of pat) {
    const s0 = rep * len + at, ns = chordOf(chords[Math.floor(s0 / bar) % chords.length], oct);
    events.push({ at: s0, len: l, notes: [ns[tn % ns.length] + 12 * (Math.floor(tn / ns.length) + up)] });
  }
  return { steps, events };
};
// the chord, struck at these steps of every bar, each for its own length: [step, length]
const rhythmOf = (chords, bar, pat, oct) => ({ steps: chords.length * bar, events: chords.flatMap((c, i) =>
  pat.map(([at, len]) => ({ at: i * bar + at, len, notes: chordOf(c, oct) }))) });
// a broken chord: its notes in this order, one every so many steps (past
// the chord's last note, the same notes an octave up)
const arpOf = (chords, bar, order, every, oct) => ({ steps: chords.length * bar, events: chords.flatMap((c, i) => {
  const ns = chordOf(c, oct), tone = j => ns[j % ns.length] + 12 * Math.floor(j / ns.length);
  return Array.from({ length: bar / every }, (_, j) => ({ at: i * bar + j * every, len: every, notes: [tone(order[j % order.length])] }));
}) });

// --- the instruments: each plays one note, at time t, for dur seconds
const hz = m => 440 * Math.pow(2, (m - 69) / 12);
const noiseBufs = new WeakMap();
function noiseBuf(A) {
  if (!noiseBufs.has(A)) {
    const b = A.createBuffer(1, A.sampleRate, A.sampleRate), d = b.getChannelData(0);
    for (let i = 0; i < d.length; i++) d[i] = Math.random() * 2 - 1;
    noiseBufs.set(A, b);
  }
  return noiseBufs.get(A);
}
// a gain into dest (a node or a setting): fixed, ...
function gainOf(A, dest, value) { const g = A.createGain(); g.gain.value = value; g.connect(dest); return g; }
// ... rising to its peak and dying away (tau: how fast) ...
function amp(A, dest, t, peak, attack, tau) {
  const g = A.createGain(); g.gain.setValueAtTime(0, t); g.gain.linearRampToValueAtTime(peak, t + attack);
  g.gain.setTargetAtTime(0, t + attack, tau); g.connect(dest); return g;
}
// ... or holding until the note ends, then letting go
function held(A, dest, t, peak, attack, end, release) {
  const g = A.createGain(), from = Math.max(t + attack, end);
  g.gain.setValueAtTime(0, t); g.gain.linearRampToValueAtTime(peak, t + attack);
  g.gain.setValueAtTime(peak, from); g.gain.setTargetAtTime(0, from, release); g.connect(dest); return g;
}
function tone(A, dest, type, f, t, stop, detune = 0) {
  const o = A.createOscillator(); o.type = type; o.frequency.setValueAtTime(f, t);
  if (detune) o.detune.setValueAtTime(detune, t);
  o.connect(dest); o.start(t); o.stop(stop); return o;
}
function noise(A, dest, t, stop) {
  const s = A.createBufferSource(); s.buffer = noiseBuf(A); s.loop = true;
  s.connect(dest); s.start(t, Math.random() * 0.5); s.stop(stop); return s;
}
function filter(A, dest, type, f, q = 0.7) {
  const b = A.createBiquadFilter(); b.type = type; b.frequency.value = f; b.Q.value = q; b.connect(dest); return b;
}
// Each instrument has a knob of its own on the mixing board, x from 0 to
// 100, at 50 as written: how long it rings, how bright, how much vibrato...
// "ring" turns 0..100 into a quarter as long .. four times as long.
const ring = x => Math.pow(2, (x - 50) / 25);
const INSTRUMENTS = {
  // a steel pan: a bright ring that dies away, its octave strong
  steelpan(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), tau = 0.32 * ring(x), g = amp(A, out, t, 0.3 * v, 0.004, tau);
    for (const [r, a] of [[1, 1], [2, 0.55], [3.01, 0.18]]) tone(A, gainOf(A, g, a), "sine", f * r, t, t + 7 * tau);
    tone(A, amp(A, out, t, 0.05 * v, 0.002, 0.025), "triangle", f * 4, t, t + 0.2);
  },
  // a reggae organ's chop: short and buzzy
  organ(A, out, t, m, dur, v, x = 50) {
    const end = t + dur, lp = filter(A, held(A, out, t, 0.08 * v, 0.004, end, 0.03), "lowpass", 400 * Math.pow(2, x / 25));
    tone(A, lp, "sawtooth", hz(m), t, end + 0.25, -7); tone(A, lp, "square", hz(m), t, end + 0.25, 7);
  },
  // a round bass, with an edge so a small speaker can still play it
  bass(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), end = t + dur * 0.9, lp = filter(A, held(A, out, t, 0.22 * v, 0.008, end, 0.05), "lowpass", 300 + 7 * x);
    tone(A, lp, "triangle", f, t, end + 0.4); tone(A, gainOf(A, lp, 0.7 * x / 100), "square", f, t, end + 0.4);
  },
  kick(A, out, t, m, dur, v, x = 50) {
    const o = tone(A, amp(A, out, t, 0.9 * v, 0.002, 0.09 * ring(x)), "sine", 150, t, t + 0.65 * ring(x));
    o.frequency.exponentialRampToValueAtTime(45, t + 0.12);
  },
  rim(A, out, t, m, dur, v, x = 50) {
    const k = ring(x);
    noise(A, filter(A, amp(A, out, t, 0.5 * v, 0.001, 0.025 * k), "bandpass", 2600, 3), t, t + 0.2 * k);
    tone(A, amp(A, out, t, 0.3 * v, 0.001, 0.015 * k), "triangle", 420, t, t + 0.12 * k);
  },
  snare(A, out, t, m, dur, v, x = 50) {
    const k = ring(x);
    noise(A, filter(A, amp(A, out, t, 0.4 * v, 0.001, 0.07 * k), "bandpass", 1800, 0.8), t, t + 0.5 * k);
    tone(A, amp(A, out, t, 0.2 * v, 0.001, 0.04 * k), "triangle", 200, t, t + 0.25 * k);
  },
  // a cowbell: two square tones, filtered, quick to fade
  cowbell(A, out, t, m, dur, v, x = 50) {
    const k = ring(x), bp = filter(A, amp(A, out, t, 0.2 * v, 0.001, 0.07 * k), "bandpass", 1000, 2);
    tone(A, bp, "square", 560, t, t + 0.5 * k); tone(A, bp, "square", 845, t, t + 0.5 * k);
  },
  hat(A, out, t, m, dur, v, x = 50) { noise(A, filter(A, amp(A, out, t, 0.3 * v, 0.001, 0.018 * ring(x)), "highpass", 7500), t, t + 0.15 * ring(x)); },
  shaker(A, out, t, m, dur, v, x = 50) { noise(A, filter(A, amp(A, out, t, 0.22 * v, 0.012, 0.03 * ring(x)), "bandpass", 5500, 1.2), t, t + 0.25 * ring(x)); },
  // a hand drum, tuned to its note
  // (its knob tunes it, a half step at a time, up to an octave either way)
  conga(A, out, t, m, dur, v, x = 50) {
    const f = hz(m) * Math.pow(2, Math.round((x - 50) * 12 / 50) / 12), o = tone(A, amp(A, out, t, 0.42 * v, 0.002, 0.1), "sine", f * 1.4, t, t + 0.7);
    o.frequency.exponentialRampToValueAtTime(f, t + 0.04);
    return o;
  },
  // a bubble: a quick rising bloop
  bubble(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), o = tone(A, amp(A, out, t, 0.22 * v, 0.004, 0.05), "sine", f, t, t + 0.4);
    o.frequency.exponentialRampToValueAtTime(f * (1 + 0.024 * x), t + 0.09);
  },
  // a plucked string, short
  pizz(A, out, t, m, dur, v, x = 50) { tone(A, filter(A, amp(A, out, t, 0.3 * v, 0.003, 0.13 * ring(x)), "lowpass", 1400), "triangle", hz(m), t, t + 7 * 0.13 * ring(x)); },
  // a guitar's strum, bright, then mellow
  pluck(A, out, t, m, dur, v, x = 50) {
    const lp = filter(A, amp(A, out, t, 0.14 * v, 0.003, 0.16 * ring(x)), "lowpass", 3200);
    lp.frequency.setTargetAtTime(700, t, 0.07); tone(A, lp, "sawtooth", hz(m), t, t + 7 * 0.16 * ring(x));
  },
  // a reggae guitar's chop: short, bright, no low end
  chop(A, out, t, m, dur, v, x = 50) {
    const lp = filter(A, amp(A, out, t, 0.18 * v, 0.002, 0.045 * ring(x)), "lowpass", 2600);
    tone(A, filter(A, lp, "highpass", 500), "sawtooth", hz(m), t, t + 0.35 * ring(x));
  },
  // a melodica: reedy, a little vibrato
  melodica(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), end = t + dur * 0.92, lp = filter(A, held(A, out, t, 0.12 * v, 0.03, end, 0.08), "lowpass", 1900);
    const o = tone(A, lp, "square", f, t, end + 0.6); tone(A, lp, "sawtooth", f, t, end + 0.6, 4);
    tone(A, gainOf(A, o.frequency, f * 0.008 * x / 100), "sine", 5.5, t, end + 0.6);
  },
  // a low buzzy bass: two bright waves through a filter that closes as the note goes on
  // (its knob is its wah: how far the filter opens, and how sharply it sings)
  buzz(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), end = t + dur * 0.9, open = 600 + 24 * x, lp = filter(A, held(A, out, t, 0.1 * v, 0.005, end, 0.04), "lowpass", open, 1 + x / 10);
    lp.frequency.setValueAtTime(open, t); lp.frequency.setTargetAtTime(260, t, 0.08);
    tone(A, lp, "sawtooth", f, t, end + 0.3, -6); tone(A, lp, "square", f, t, end + 0.3, 6);
  },
  // a funk bass through a wah pedal: buzzy (a saw and a square a little
  // apart, with a saw an octave down for weight), plucked (it dies away
  // quickly), and on every note its filter sings up and back down, wah-wah
  // (its knob is how wide the wah opens)
  wahbass(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), end = t + Math.min(dur, 0.7), lo = 220, hi = 900 + 32 * x;
    const lp = filter(A, amp(A, out, t, 0.17 * v, 0.004, 0.12 + 0.12 * Math.min(1, dur * 2)), "lowpass", lo, 6 + x / 10);
    lp.frequency.setValueAtTime(lo, t); lp.frequency.linearRampToValueAtTime(hi, t + 0.045); lp.frequency.setTargetAtTime(lo, t + 0.045, 0.08);
    tone(A, lp, "sawtooth", f, t, end + 0.4, -8); tone(A, lp, "square", f, t, end + 0.4, 8); tone(A, gainOf(A, lp, 0.6), "sawtooth", f / 2, t, end + 0.4);
  },
  // a dance synth's lead: two saws a little apart and a square an octave
  // down, through a filter that opens bright on each note and closes
  // (its knob is how bright it opens)
  lead(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), end = t + dur * 0.85, peak = 1200 + 70 * x;
    const lp = filter(A, held(A, out, t, 0.06 * v, 0.004, end, 0.05), "lowpass", peak, 4);
    lp.frequency.setValueAtTime(peak, t); lp.frequency.setTargetAtTime(peak * 0.35, t + 0.01, 0.12);
    for (const d of [-12, 12]) tone(A, lp, "sawtooth", f, t, end + 0.3, d);
    tone(A, gainOf(A, lp, 0.5), "square", f / 2, t, end + 0.3);
  },
  // a laser zap: a square tone falling fast, for a chase
  // (its knob is how far it falls)
  zap(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), o = tone(A, amp(A, out, t, 0.07 * v, 0.002, 0.08), "square", f, t, t + 0.4);
    o.frequency.setTargetAtTime(f / (1.5 + x / 25), t, 0.05);
  },
  // a harpsichord: a bright plucked string and its octave, quick to fade, a click as the quill plucks
  harpsichord(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), tau = 0.35 * ring(x), lp = filter(A, amp(A, out, t, 0.25 * v, 0.002, tau), "lowpass", 5000);
    tone(A, lp, "sawtooth", f, t, t + 7 * tau); tone(A, gainOf(A, lp, 0.4), "square", f * 2, t, t + 7 * tau, 3);
    noise(A, filter(A, amp(A, out, t, 0.04 * v, 0.001, 0.01), "highpass", 3000), t, t + 0.06);
  },
  // a vibraphone: a soft mallet's ring, wobbling with its motor
  // (its knob is its motor's speed: 0 stops the wobble)
  vibes(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), wob = gainOf(A, amp(A, out, t, 0.28 * v, 0.003, 0.8), 1);
    tone(A, wob, "sine", f, t, t + 5); tone(A, gainOf(A, wob, 0.18), "sine", f * 4, t, t + 2);
    if (x > 0) tone(A, gainOf(A, wob.gain, 0.3), "sine", 0.11 * x, t, t + 5);
  },
  // a plucked violin: brighter and shorter than the cello's pizz
  fiddle(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), tau = 0.09 * ring(x), lp = filter(A, amp(A, out, t, 0.4 * v, 0.002, tau), "lowpass", 2800);
    tone(A, lp, "triangle", f, t, t + 0.1 + 7 * tau); tone(A, gainOf(A, lp, 0.25), "sawtooth", f, t, t + 0.1 + 7 * tau);
  },
  // a ghostly voice, wavering, like a theremin
  ghost(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), end = t + dur, g = held(A, out, t, 0.15 * v, 0.08, end, 0.12), o = tone(A, g, "sine", f, t, end + 0.8);
    tone(A, gainOf(A, o.frequency, f * 0.03 * x / 100), "sine", 5.5, t, end + 0.8);
    tone(A, gainOf(A, g, 0.15), "triangle", f * 2, t, end + 0.8);
  },
  // a bell, its overtones out of tune with each other, ringing long
  bell(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), k = ring(x);
    for (const [r, a, tau] of [[1, 1, 1.4], [2, 0.5, 0.9], [2.76, 0.4, 0.6], [5.4, 0.2, 0.3]])
      tone(A, amp(A, out, t, 0.14 * v * a, 0.003, tau * k), "sine", f * r, t, t + 7 * tau * k);
  },
  // a soft pad: three saws a little out of tune, gently filtered
  pad(A, out, t, m, dur, v, x = 50) {
    const end = t + dur, lp = filter(A, held(A, out, t, 0.035 * v, 0.05 + 0.011 * x, end, 0.5), "lowpass", 900);
    for (const d of [-9, 0, 9]) tone(A, lp, "sawtooth", hz(m), t, end + 3.5, d);
  },
  // a music box: a sweet ping and its ring
  musicbox(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), k = ring(x);
    tone(A, amp(A, out, t, 0.18 * v, 0.002, 0.45 * k), "sine", f, t, t + 7 * 0.45 * k);
    tone(A, amp(A, out, t, 0.045 * v, 0.002, 0.12 * k), "sine", f * 4, t, t + 7 * 0.12 * k);
  },
  // a flute: soft, with a little vibrato once the note settles
  flute(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), end = t + dur * 0.95, g = held(A, out, t, 0.15 * v, 0.05, end, 0.06), o = tone(A, g, "sine", f, t, end + 0.5);
    tone(A, gainOf(A, g, 0.3), "triangle", f, t, end + 0.5);
    const depth = A.createGain(); depth.gain.setValueAtTime(0, t); depth.gain.linearRampToValueAtTime(f * 0.012 * x / 100, t + 0.25);
    depth.connect(o.frequency); tone(A, depth, "sine", 5, t, end + 0.5);
  },
  // --- the funk band, for the classic beats ---
  // a bass guitar, played with the fingers: a thump as the string is
  // plucked, then a round note that fades while it is held
  // (its knob is its growl: how much of the string's edge comes through)
  bassgtr(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), end = t + dur * 0.92, g = held(A, out, t, 0.26 * v, 0.004, end, 0.04);
    const lp = filter(A, g, "lowpass", 380 + 9 * x, 1.2);
    lp.frequency.setValueAtTime(900 + 18 * x, t); lp.frequency.setTargetAtTime(380 + 9 * x, t, 0.06);
    tone(A, lp, "triangle", f, t, end + 0.3); tone(A, gainOf(A, lp, 0.45), "sawtooth", f, t, end + 0.3);
    noise(A, filter(A, amp(A, out, t, 0.05 * v, 0.001, 0.008), "bandpass", 1200, 1.5), t, t + 0.05);
  },
  // a bongo: a quick, high, tight drum, a slap and a ring that falls to
  // its note (its knob tunes it, a half step at a time, as a conga's does)
  bongo(A, out, t, m, dur, v, x = 50) {
    const f = hz(m) * Math.pow(2, Math.round((x - 50) * 12 / 50) / 12);
    const o = tone(A, amp(A, out, t, 0.38 * v, 0.001, 0.055), "sine", f * 1.6, t, t + 0.4);
    o.frequency.exponentialRampToValueAtTime(f, t + 0.025);
    noise(A, filter(A, amp(A, out, t, 0.12 * v, 0.001, 0.012), "bandpass", 3200, 1.2), t, t + 0.06);
    return o;
  },
  // an open hi-hat: the hat, let ring
  openhat(A, out, t, m, dur, v, x = 50) { noise(A, filter(A, amp(A, out, t, 0.24 * v, 0.002, 0.11 * ring(x)), "highpass", 7000), t, t + 0.8 * ring(x)); },
  // a tonewheel organ: its drawbars (sines at a note's octaves and
  // fifths), a click as a key goes down, and the wobble of a spinning
  // speaker (its knob is how fast the speaker spins)
  tonewheel(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), end = t + dur * 0.96, g = held(A, out, t, 0.075 * v, 0.006, end, 0.05);
    const wob = gainOf(A, g, 1);
    for (const [r, a] of [[0.5, 0.6], [1, 1], [1.5, 0.5], [2, 0.7], [3, 0.3], [4, 0.25]]) tone(A, gainOf(A, wob, a), "sine", f * r, t, end + 0.4);
    if (x > 0) tone(A, gainOf(A, wob.gain, 0.22), "sine", 0.12 * x, t, end + 0.4);
    tone(A, amp(A, out, t, 0.03 * v, 0.001, 0.01), "square", f * 4, t, t + 0.05);
  },
  // a funk guitar's chord: short, bright and dry, a wah's peak on it
  // (its knob is where the wah sits, from dark to bright)
  funkgtr(A, out, t, m, dur, v, x = 50) {
    const bp = filter(A, amp(A, out, t, 0.2 * v, 0.002, 0.05 + 0.05 * Math.min(1, dur * 3)), "bandpass", 500 + 30 * x, 3);
    tone(A, bp, "sawtooth", hz(m), t, t + 0.4); tone(A, gainOf(A, bp, 0.5), "square", hz(m), t, t + 0.4, 5);
  },
  // a twangy lead guitar: a bright pluck that sings on, bending up into
  // its note, with a whammy bar's wobble as it is held
  // (its knob is how wide the wobble is)
  twang(A, out, t, m, dur, v, x = 50) {
    const f = hz(m), end = t + dur * 0.95, g = held(A, out, t, 0.11 * v, 0.003, end, 0.12);
    const lp = filter(A, g, "lowpass", 3200, 2.5);
    lp.frequency.setValueAtTime(4200, t); lp.frequency.setTargetAtTime(1800, t, 0.25);
    const a = tone(A, lp, "sawtooth", f * 0.97, t, end + 0.8), b = tone(A, gainOf(A, lp, 0.6), "square", f * 0.97, t, end + 0.8, 6);
    for (const o of [a, b]) {
      o.frequency.setTargetAtTime(f, t, 0.02);
      const depth = A.createGain(); depth.gain.setValueAtTime(0, t); depth.gain.linearRampToValueAtTime(f * 0.0003 * x, t + 0.35);
      depth.connect(o.frequency); tone(A, depth, "sine", 5.2, t, end + 0.8);
    }
  },
  // a bird: two quick chirps
  bird(A, out, t, m, dur, v, x = 50) {
    const f = hz(m);
    for (const dt of [0, 0.11]) {
      const o = tone(A, amp(A, out, t + dt, 0.12 * v, 0.004, 0.03), "sine", f, t + dt, t + dt + 0.25);
      o.frequency.exponentialRampToValueAtTime(f * (1 + 0.014 * x), t + dt + 0.06);
    }
  },
};

// --- the tunes: so many beats a minute (a beat is 4 steps), steps a bar
// ("bar", 16 unless it says), the steps shifted later by "swing" (every
// other sixteenth, a share of a step) or "swing8" (every other eighth, for
// a jazzy lilt), how long the room rings ("room", seconds), its loudness
// beside the other tunes ("level"), and the parts: a name, an instrument,
// its notes, its level, how much goes to the room ("rev") and to the echo
// ("echo", a repeat every "echoSteps"), and a shift in semitones.
// The "form" is the tune from start to end: sections of so many bars, each
// with the parts it names (by name; several parts may share one), each
// part starting its notes again at the section's start, and the whole
// section moved up or down ("up", semitones) if it says.
const S = (bars, names, up = 0) => ({ bars, names: names.split(" "), up });
// --- the classic beats: a funk band's tune, made from its lines,
// patterns and plan. Each tune's sections give each player a turn in front:
// the bongos open it, then the bass, the bongos again, the drum kit, the
// organ, the rhythm guitar and the lead guitar take a solo each, over the
// band or alone. (Our own tunes, in the style of the funk instrumentals
// hip-hop grew up on: a breakbeat, bongos, an organ, a twangy guitar.)
const FUNK_SECTIONS = {
  intro: "bongoHi bongoLo",
  kitIn: "bongoHi bongoLo kick snare hat",
  groove: "kick snare hat openhat bass rhythm organ bongoHi fill",
  theme: "kick snare hat openhat bass rhythm organ theme bongoHi bongoLo fill",
  bassSolo: "kick hat bassSolo fill",
  bongoSolo: "soloHi soloLo kick",
  drumBreak: "breakKick breakSnare breakHat openhat fill",
  organSolo: "kick snare hat bass organSolo",
  rhythmSolo: "kick snare hat bass rhythmSolo",
  gtrSolo: "kick snare hat openhat bass rhythm gtrSolo fill",
  outro: "bongoHi bongoLo kick hat",
};
function funkTune(c) {
  const ch = c.chords;
  return { bpm: c.bpm, swing: c.swing || 0, room: 1.6, level: 1, echoSteps: 3, lfos: c.lfos || {}, parts: [
    { name: "kick", inst: "kick", level: 0.9, line: hitsOf(c.kick) },
    { name: "snare", inst: "snare", level: 0.55, rev: 0.15, line: hitsOf(c.snare) },
    { name: "hat", inst: "hat", level: 0.35, line: hitsOf(c.hat) },
    { name: "openhat", inst: "openhat", level: 0.3, line: hitsOf(c.open) },
    { name: "breakKick", inst: "kick", level: 0.95, line: hitsOf(c.bKick) },
    { name: "breakSnare", inst: "snare", level: 0.6, rev: 0.15, line: hitsOf(c.bSnare) },
    { name: "breakHat", inst: "hat", level: 0.4, line: hitsOf(c.bHat) },
    // a roll on the snare into the next section, in the last bar of eight:
    // faster and faster, in triplets (three over two beats, three to a
    // beat, six to a beat)
    { name: "fill", inst: "snare", level: 0.5, line: hitsOf(".".repeat(112) + "8(xxx) (xox) (xxxxxx)") },
    // (the bongos' pitch may follow one of the tune's LFOs: c.lfo, by part)
    { name: "bongoHi", inst: "bongo", level: 0.7, rev: 0.15, line: hitsOf(c.hi, 71), lfo: (c.lfo || {}).hi },
    { name: "bongoLo", inst: "bongo", level: 0.75, rev: 0.15, line: hitsOf(c.lo, 64), lfo: (c.lfo || {}).lo },
    { name: "soloHi", inst: "bongo", level: 0.8, rev: 0.15, line: hitsOf(c.soloHi, 71), lfo: (c.lfo || {}).soloHi },
    { name: "soloLo", inst: "bongo", level: 0.85, rev: 0.15, line: hitsOf(c.soloLo, 64), lfo: (c.lfo || {}).soloLo },
    { name: "bass", inst: "bassgtr", level: 0.95, line: lineOf(c.bass) },
    { name: "bassSolo", inst: "bassgtr", level: 1, line: lineOf(c.bassSolo) },
    { name: "rhythm", inst: "funkgtr", level: 0.45, rev: 0.1, line: rhythmOf(ch, 16, c.rhythm, 4) },
    { name: "rhythmSolo", inst: "funkgtr", level: 0.6, rev: 0.1, line: rhythmOf(ch, 16, c.rhythmSolo, 4) },
    { name: "organ", inst: "tonewheel", level: 0.5, rev: 0.2, line: rhythmOf(ch, 16, c.organ, 3) },
    { name: "organSolo", inst: "tonewheel", level: 0.85, rev: 0.2, line: lineOf(c.organSolo) },
    { name: "theme", inst: "twang", level: 0.8, rev: 0.3, echo: c.echo, line: lineOf(c.theme) },
    { name: "gtrSolo", inst: "twang", level: 0.8, rev: 0.3, echo: 0.1, line: lineOf(c.gtrSolo) },
  ], form: c.plan.map(([what, bars, up]) => S(bars, FUNK_SECTIONS[what], up || 0)) };
}
// The spooky waltz, funked: the same waltz, quicker (205) and four half
// steps down, its parts as they were (the bass lines louder: Matthew's call),
// over a funk beat in 4, more drums, the bongo breakdown's bongos and their
// solo, and a pair of wah basses, one low, popping its octaves, one higher,
// answering it, both keeping to the waltz's chords (crossOf). The funk's big
// beats land on the waltz's, five ways, A to E, for Matthew to choose from:
//   A  four to a bar: a whole funk bar in each waltz bar, its four beats
//      against the waltz's three, the kick on every waltz downbeat
//   B  half-time over two bars: a funk bar across two waltz bars, the kick on
//      the first's downbeat and the snare on the second's
//   C  the waltz in 4: each waltz bar re-set in four beats (its first beat
//      held for two) under an ordinary half-time funk bar
//   D  the kick with the waltz: the kick on the waltz's own beats, the snare
//      and the hats in 4 over it
//   E  a shuffle over two bars: a funk shuffle across two waltz bars in the
//      waltz's own triplets, the snare on the second's downbeat
// Each pattern is n slots spread over a funk bar of L steps; each its own level, so all five are
// about as loud as one another (checked offline).
const WITCH_FUNK = {
  A: { level: 0.48, what: "four to a bar", bar: 12, L: 12, n: 16, kick: "x.x....x..x.....", snare: "....x..o.o..x..o", hat: "xoxoxoxoxoxoxoxo", open: "..............x.",
       shaker: "xooxxooxxooxxoox", cowbell: "x...x...x.x.x...", conga: "..x...xx..x..x.x", hi: "x..x..x.x..x.xx.", lo: "..x...x...x...x.",
       soloHi: "x.xx.x.xx.x.xxxx", soloLo: "x...x...x...x...",
       wahLo: [[0, 0, 2], [3, 0, 1, 1], [5, 0, 1], [7, 2, 1], [10, 0, 1, 1], [11, 0, 1], [13, 1, 1], [14, 2, 1]],
       wahHi: [[2, 1, 1, 1], [4, 2, 1, 1], [6, 0, 1, 2], [9, 2, 1, 1], [12, 1, 1, 1], [15, 2, 1, 1]] },
  B: { level: 0.6, what: "half-time over two bars", bar: 12, L: 24, n: 16, kick: "x.x.......x..x..", snare: "...o..o.x..o...o", hat: "xoxoxoxoxoxoxoxo", open: "..............x.",
       shaker: "xooxxooxxooxxoox", cowbell: "x.....x.x...x...", conga: "..x..xx...x..x.x", hi: "x..x..x.x..x.xx.", lo: "..x...x...x...x.",
       soloHi: "x.xx.x.xx.x.xxxx", soloLo: "x...x...x...x...",
       wahLo: [[0, 0, 3], [3, 0, 1, 1], [6, 0, 1], [8, 0, 2], [11, 0, 1, 1], [12, 2, 1], [14, 1, 2]],
       wahHi: [[2, 1, 1, 1], [5, 2, 1, 1], [7, 0, 1, 2], [10, 2, 1, 1], [13, 1, 1, 1], [15, 2, 1, 1]] },
  C: { level: 0.5, what: "the waltz in 4", bar: 16, L: 16, n: 16, inFour: true, kick: "x.x.......x..x..", snare: "...o..o.x..o...o", hat: "x.x.x.x.x.x.x.x.", open: "..............x.",
       shaker: "xoxoxoxoxoxoxoxo", cowbell: "x.....x.x...x...", conga: "..x..xx...x..x.x", hi: "x..x..x.x..x.xx.", lo: "..x...x...x...x.",
       soloHi: "x.xx.x.xx.x.xxxx", soloLo: "x...x...x...x...",
       wahLo: [[0, 0, 3], [3, 0, 1, 1], [6, 0, 1], [8, 0, 2], [11, 0, 1, 1], [12, 2, 1], [14, 1, 2]],
       wahHi: [[2, 1, 1, 1], [5, 2, 1, 1], [7, 0, 1, 2], [10, 2, 1, 1], [13, 1, 1, 1], [15, 2, 1, 1]] },
  D: { level: 0.5, what: "kick with the waltz", bar: 12, L: 16, n: 16, kickL: 12, kick: "x.......x.x.", snare: "..o.....x..o..o.", hat: "x.x.x.x.x.x.x.x.", open: "..............x.",
       shaker: "xoxoxoxoxoxoxoxo", cowbell: "x...x...x.x.x...", conga: "..x...xx..x..x.x", hi: "x..x..x.x..x.xx.", lo: "..x...x...x...x.",
       soloHi: "x.xx.x.xx.x.xxxx", soloLo: "x...x...x...x...", bassL: 12, bassN: 12,
       wahLo: [[0, 0, 3], [4, 0, 1, 1], [6, 0, 1], [8, 2, 2], [10, 0, 1, 1], [11, 1, 1]],
       wahHi: [[2, 1, 1, 1], [5, 2, 1, 1], [7, 0, 1, 2], [9, 2, 1, 1]] },
  E: { level: 0.6, what: "shuffle over two bars", bar: 12, L: 24, n: 12, kick: "x....x..x...", snare: ".o..o.x..o.o", hat: "x.xx.xx.xx.x", open: "..........x.",
       shaker: "xoxxoxxoxxox", cowbell: "x..x..x.xx..", conga: "..x..xx..x.x", hi: "x.xx..x.xx.x", lo: "..x..x..x..x",
       soloHi: "x.xx.xx.xxxx", soloLo: "x..x..x..x..",
       wahLo: [[0, 0, 2], [2, 0, 1, 1], [3, 0, 1], [5, 2, 1], [6, 0, 2], [8, 0, 1, 1], [9, 1, 1], [11, 2, 1]],
       wahHi: [[1, 1, 1, 1], [4, 2, 1, 1], [7, 0, 1, 2], [10, 2, 1, 1]] },
};
// (ticket 301, for M: "more versions of \"🛋️ couch-cushion rave\" ... More variations of similar duration. From version to
// version, vary the drums (experiment with triplets and polyrhythms, and drum solos) and the chord progressions (different
// ones, especially upbeat and funky ones)") Five more raves beside the first, as the funked waltzes stand beside the
// witch's: each its own two progressions (A, then B, a chord a bar), its drums (hitsOf: "16(...)" spreads hits over a bar,
// so triplets and three-against-four), its bass and stabs on its chords, a hook made of its chords' tones (so it fits
// whatever they are), and a drum solo in one of them; each as long as the first, about 52 bars. (Matthew: "we want each
// new track to feel upbeat -- so major chords, or minor chords but uptempo": major keys, or funk's 9ths, at 120 to 128.)
const CUSHION_RAVE = {
  A: { what: "triplet shuffle", bpm: 122, A: ["D", "G", "A", "D"], B: ["Bm", "G", "A", "A7"],
       kick: "x...x...x...x...", snare: "....x.......x...", hats: "16(xoxxoxxoxxox)", extra: ["shaker", "16(x.xx.xx.xx.x)"],
       bass: [[0, 0, 3], [3, 7, 1], [6, 0, 2], [8, 12, 2], [11, 7, 1], [14, 10, 2]], stabs: [2, 6, 10, 14], hookA: [4, 3, 2, 0, 2, 3, 4, 6], hookB: [0, 2, 3, 4, 6, 4, 3, 2],
       // (ticket 360) Matthew's post-production, his save of 2026-10-18 20:48: the hooks three octaves down, darker and
       // roomier, the stabs, sparkle and ghost two down, the first bass one, the hats and shaker in the room
       mix: { "hookA·lead": { upN: -3, knob: 79, tone: 28, rev: 51, level: 2 }, "hookB·lead": { upN: -3, knob: 82, tone: 26, rev: 59, echo: 69, level: 8 },
              "stabsA·lead": { upN: -2 }, "sparkle·musicbox": { upN: -2, rev: 85, echo: 50 }, "ghost·ghost": { upN: -2 }, "bassA·buzz": { upN: -1 },
              "extra·shaker": { rev: 24, echo: 0 }, "hats·hat": { echo: 22, level: 5, rev: 40 } } },
  B: { what: "three against four", bpm: 120, A: ["C", "F", "G", "C"], B: ["Am7", "F", "G", "G7"],
       kick: "x...x...x...x...", snare: "....x.......x..o", hats: "..x...x...x...x.", extra: ["cowbell", "16(xxx)"], extra2: ["conga", "16(x.xx.xx.x)", 62],
       bass: [[0, 0, 2], [3, 0, 1], [5, 7, 1], [8, 0, 2], [10, 12, 2], [13, 7, 1], [14, 10, 2]], stabs: [3, 7, 11, 15], hookA: [2, 4, 6, 4, 2, 0, 2, 4], hookB: [6, 5, 4, 2, 4, 5, 6, 8],
       // (ticket 360) Matthew's post-production, his save of 2026-10-18 20:48: both basses louder, wah wide open, toned
       // down and in the room, panned apart; the first an octave down
       mix: { "bassA·buzz": { upN: -1, level: 7, echo: 49, rev: 36, tone: 75, knob: 100, pan: 4 },
              "bassB·buzz": { knob: 95, level: 5, pan: -7, echo: 30, rev: 18, tone: 77 } } },
  C: { what: "a drum solo", bpm: 124, A: ["E9", "A9", "E9", "B7"], B: ["A9", "E9", "B7", "A7"], solo: true, wah: true,
       kick: "x..x..x...x.x...", snare: "....x..o.o..x..o", hats: "xoxoxoxoxoxoxoxo", extra: ["openhat", "..............x."],
       bass: [[0, 0, 2], [2, 12, 1], [4, 0, 1], [7, 10, 1], [8, 0, 2], [11, 7, 1], [12, 12, 2], [15, 10, 1]], stabs: [0, 6, 12], hookA: [0, 2, 4, 3, 2, 0, 3, 2], hookB: [4, 3, 2, 0, 2, 3, 4, 5] },
  D: { what: "breakbeat", bpm: 126, A: ["G", "C", "D", "G"], B: ["Em", "C", "D", "D7"],
       kick: "x..x..x...x.x...", snare: "....x..x.x..x..o", hats: "x.xxx.xxx.xxx.xx", extra: ["conga", "8(xxx)", 62],
       bass: [[0, 0, 2], [3, 7, 1], [6, 0, 1], [7, 12, 1], [10, 0, 2], [13, 10, 1], [14, 7, 2]], stabs: [2, 5, 10, 13], hookA: [4, 2, 4, 6, 4, 2, 0, 2], hookB: [2, 4, 5, 6, 5, 4, 2, 0] },
  E: { what: "half-time, triplet fills", bpm: 128, A: ["A", "D", "E", "A"], B: ["F#m", "D", "E", "E7"], half: true,
       kick: "x.........x.....", snare: "........x.......", hats: "x.x.x.x.x.x.x.x.", extra: ["bongo", "16(xxxxxx)", 71],
       bass: [[0, 0, 4], [6, 7, 2], [10, 0, 2], [12, 12, 4]], stabs: [0, 8], hookA: [0, 0, 2, 4, 6, 4, 2, 4], hookB: [6, 6, 4, 2, 0, 2, 4, 2] },
};
// (ticket 301, Matthew: "and turtle steppers' dub") Four more turtle steppers dubs beside it, its echoes and its sonar,
// each with drums and chords of its own: triplets, the one-drop, three against four, and a drum break; upbeat (major, or
// the minor one fast, at the super-dub's pace)
const TURTLE_DUB = {
  A: { what: "triplet shuffle, fast", bpm: 150, chords: ["Am", "Am", "Dm", "Am", "F", "G", "Am", "Am"], sonar: "A6:1 r:63 E6:1 r:63",
       drums: { hat: "16(x.xx.xx.xx.x)", rim: "........x...16(..x)", extra: [["shaker", "16(xoxxoxxoxxox)", 0.35]] },
       tunes: [[[8, 2, 2], [10, 3, 2], [12, 4, 4], [20, 4, 4], [24, 3, 4], [28, 2, 4]], [[0, 4, 4], [8, 6, 8], [16, 5, 4], [20, 4, 4], [24, 2, 8]]] },
  B: { what: "one-drop", bpm: 84, chords: ["G", "C", "D", "G", "Em", "C", "D", "D"], sonar: "G6:1 r:63 D6:1 r:63", swing: 0.2,
       drums: { kick: "........x.......", rim: "........x.......", hat: "x.x.x.x.x.x.x.x.", extra: [["shaker", "..x...x...x...x.", 0.3]] },
       bassPat: [[0, 0, 4], [6, 7, 2], [8, 0, 2], [11, 4, 1], [12, 7, 4]], chop: [2, 6, 10, 14],
       tunes: [[[4, 2, 2], [6, 3, 2], [8, 4, 6], [20, 3, 2], [22, 2, 2], [24, 0, 6]], [[0, 4, 4], [4, 5, 4], [8, 4, 8], [16, 2, 4], [20, 3, 4], [24, 2, 8]]] },
  C: { what: "rockers, three against four", bpm: 88, chords: ["F", "Bb", "C7", "F", "Bb", "C7", "F", "C7"], sonar: "F6:1 r:63 C6:1 r:63",
       drums: { kick: "x.....x.x.....x.", rim: "....x.......x...", hat: "xoxoxoxoxoxoxoxo", extra: [["cowbell", "16(xxx)", 0.3], ["conga", "16(x.xx.xx.x)", 0.45, 62]] },
       bassPat: [[0, 0, 2], [3, 0, 1], [6, 7, 2], [10, 0, 2], [12, 10, 2], [14, 12, 2]],
       tunes: [[[0, 3, 2], [2, 2, 2], [4, 1, 4], [12, 2, 4], [16, 3, 2], [18, 4, 2], [20, 5, 8]], [[0, 5, 4], [6, 4, 2], [8, 3, 8], [20, 1, 4], [24, 2, 8]]] },
  D: { what: "a drum break", bpm: 92, chords: ["E9", "A9", "E9", "B7", "A9", "A9", "E9", "B7"], sonar: "E6:1 r:63 B5:1 r:63",
       drums: { hat: "..x...x...x.x.x.", break: [["snare", "16(x.x.x) 8(xxx) 8(xxxxx) 16(xxxxxx) 4(xxxx) 4(xxx) 8(xxxxxx)", 0.55], ["rim", "8(x.x) 8(xx.x) 16(x.xxx.x) 8(xxx) 8(xxxx)", 0.6], ["kick", "x.......x...x...x.x.x.x.x...x...", 0.8]] },
       tunes: [[[8, 2, 4], [12, 4, 4], [16, 3, 4], [20, 2, 4], [24, 0, 8]], [[0, 4, 4], [4, 6, 4], [8, 5, 8], [16, 4, 4], [20, 2, 4], [24, 4, 8]]],
       form: [S(8, "kick bass sonar"), S(8, "kick bass sonar drums chop"), S(8, "kick bass sonar drums chop tuneA bubbles"),
              S(4, "break"), S(8, "kick bass sonar drums chop tuneB bubbles"), S(8, "kick bass sonar drums chop tuneA bubbles"), S(4, "break kick bass")] },
  // (ticket 374, Matthew: "turtle stepper's dub D is awlful -- the others -- A, B, C are great. Let's make several replacement
  // candidates"; "just add them as D-1, D-2, etc") Four in A to C's way, plain chords (triads, a 7th to turn home: D's ninths made
  // its chops and tunes muddy) and drums each style keeps (no racing fives and sevens), named D-1 to D-4 beside D till he picks
  D1: { label: "D-1", what: "steppers, four on the floor", bpm: 136, chords: ["Dm", "Dm", "Bb", "C", "Dm", "Dm", "Gm", "A7"], sonar: "D6:1 r:63 A5:1 r:63",
        drums: { kick: "x...x...x...x...", rim: "....x.......x...", hat: "..x...x...x...x.", extra: [["shaker", "x.xxx.xxx.xxx.xx", 0.3]] },
        bassPat: [[0, 0, 2], [3, 0, 1], [4, 7, 2], [7, 0, 1], [8, 12, 2], [11, 7, 1], [12, 10, 2], [14, 7, 2]], chop: [2, 6, 10, 14],
        tunes: [[[0, 2, 4], [4, 3, 4], [8, 4, 6], [14, 3, 2], [16, 2, 4], [20, 1, 4], [24, 2, 8]], [[0, 5, 4], [4, 4, 4], [8, 3, 8], [16, 4, 2], [18, 3, 2], [20, 2, 4], [24, 0, 8]]] },
  D2: { label: "D-2", what: "rub-a-dub, lilting", bpm: 96, swing: 0.25, chords: ["D", "G", "A", "D", "Bm", "G", "A", "A"], sonar: "D6:1 r:63 A5:1 r:63",
        drums: { kick: "x.......x.......", rim: "....x.......x...", hat: "x.x.x.x.x.x.x.x.", extra: [["shaker", "..x...x...x...x.", 0.3], ["cowbell", "x...............", 0.18]] },
        bassPat: [[0, 0, 3], [3, 7, 1], [4, 12, 2], [6, 7, 2], [8, 0, 3], [11, 7, 1], [12, 12, 2], [14, 7, 2]], chop: [4, 7, 12, 15],
        tunes: [[[0, 0, 2], [2, 1, 2], [4, 2, 4], [8, 3, 4], [12, 2, 4], [16, 1, 2], [18, 2, 2], [20, 3, 4], [24, 4, 8]], [[0, 4, 4], [4, 3, 2], [6, 2, 2], [8, 1, 8], [16, 2, 4], [20, 1, 4], [24, 0, 8]]] },
  D3: { label: "D-3", what: "the drum break, made plain", bpm: 92, chords: ["Em", "Am", "Em", "B7", "Am", "Am", "Em", "B7"], sonar: "E6:1 r:63 B5:1 r:63",
        drums: { kick: "x.....x.x.......", rim: "....x.......x...", hat: "..x...x...x.x.x.",
                 break: [["snare", "....x.......x.......x...x.x.xxxx", 0.5], ["rim", "x.x.x.x.x.x.x.x.", 0.45], ["kick", "x.......x.......x.......x.x.....", 0.8]] },
        bassPat: [[0, 0, 2], [3, 7, 1], [6, 0, 1], [7, 12, 1], [10, 0, 2], [13, 10, 1], [14, 7, 2]],
        tunes: [[[8, 2, 4], [12, 4, 4], [16, 3, 4], [20, 2, 4], [24, 0, 8]], [[0, 4, 4], [4, 5, 4], [8, 4, 8], [16, 3, 4], [20, 2, 4], [24, 3, 8]]],
        form: [S(8, "kick bass sonar"), S(8, "kick bass sonar drums chop"), S(8, "kick bass sonar drums chop tuneA bubbles"),
               S(4, "break"), S(8, "kick bass sonar drums chop tuneB bubbles"), S(8, "kick bass sonar drums chop tuneA bubbles"), S(4, "break kick bass")] },
  D4: { label: "D-4", what: "heartbeat, hand drums", bpm: 76, swing: 0.1, chords: ["Am", "Am", "Em", "Em", "F", "G", "Am", "Am"], sonar: "A5:1 r:63 E6:1 r:63",
        drums: { kick: "x.x.....x.x.....", rim: "............x...", hat: "x...x...x...x...", extra: [["conga", "....x.xx....x.x.", 0.45, 62], ["shaker", "..x...x...x...x.", 0.28]] },
        bassPat: [[0, 0, 4], [4, 7, 4], [8, 0, 4], [12, 10, 2], [14, 7, 2]], chop: [6, 14],
        tunes: [[[0, 2, 8], [8, 3, 8], [16, 4, 6], [22, 3, 2], [24, 2, 8]], [[0, 3, 4], [4, 2, 4], [8, 1, 8], [16, 2, 4], [20, 3, 4], [24, 0, 8]]] },
};
const turtleDub = v => { const c = TURTLE_DUB[v];
  return steppersTune({ bpm: c.bpm, chords: c.chords, lines: { sonar: c.sonar, bubbles: "r:9 " + c.sonar.split(" ")[0].replace(/[0-9]/, "7") + " r:21 " + c.sonar.split(" ")[2] + " r:17 " + c.sonar.split(" ")[0] + " r:33 " + c.sonar.split(" ")[2] + " r:44" },
    drums: { ...c.drums, swing: c.swing }, bassPat: c.bassPat, tunes: c.tunes, chop: c.chop, form: c.form, bassInst: "wahbass" }); };
function cushionRave(v) {
  const c = CUSHION_RAVE[v], A = c.A, B = c.B;
  const drum = (name, inst, pat, level, note) => ({ name, inst, level, line: hitsOf(pat, note) });
  const extra = (name, e, level) => e ? [drum(name, e[0], e[1], level, e[2] ?? 60)] : [];
  // (the solo: four bars of the drums alone, the snare and the bongos racing in fives, sixes and threes over the kick)
  const solo = c.solo ? [drum("soloSnare", "snare", "8(xxxxx) 8(xxx) 4(xxxx) 4(xxx) 16(xxxxxx) 8(xxxxx) 8(xxxxxxx) 16(xxxxxxxxxxxx)", 0.55),
                         drum("soloBongo", "bongo", "16(x.x.xx.x.x.x) 8(xxx) 8(xxxx) 16(xxxxxxxxxx) 4(xxxxx) 4(xxxxx) 8(xxxxxx)", 0.7, 71),
                         drum("soloKick", "kick", "x.......x.......x...x...x.x.x.x.", 0.85)] : [];
  return { bpm: c.bpm, room: 1.8, level: 1.1, parts: [
      { name: "bassA", inst: c.wah ? "wahbass" : "buzz", level: c.wah ? 1.4 : 1, line: bassOf(A, 16, c.bass, 2) },
      { name: "bassB", inst: c.wah ? "wahbass" : "buzz", level: c.wah ? 1.4 : 1, line: bassOf(B, 16, c.bass, 2) },
      { name: "stabsA", inst: "lead", level: 0.42, rev: 0.25, line: stabsOf(A, 16, c.stabs, 1, 4) },
      { name: "stabsB", inst: "lead", level: 0.42, rev: 0.25, line: stabsOf(B, 16, c.stabs, 1, 4) },
      { name: "sparkle", inst: "musicbox", level: 0.45, rev: 0.4, line: arpOf(A, 16, [0, 1, 2, 3, 4, 3, 2, 1], 2, 5) },
      { name: "hookA", inst: "lead", rev: 0.2, line: arpOf(A, 16, c.hookA, 2, 5) },
      { name: "hookB", inst: "lead", rev: 0.2, line: arpOf(B, 16, c.hookB, 2, 5) },
      { name: "ghost", inst: "ghost", level: 0.6, rev: 0.5, line: arpOf(A, 16, [2], 16, 5) },
      drum("kick", "kick", c.kick, 0.85), drum("snare", "snare", c.snare, 0.45),
      { ...drum("hats", "hat", c.hats, 0.38) }, ...extra("extra", c.extra, 0.4), ...extra("extra2", c.extra2, 0.45),
      drum("roll", "snare", c.half ? "................ ................ ................ 16(xxxxxxxxxxxx)" : "................ ................ x...x...x...x... 8(xxx)8(xxxxxx)", 0.45),
      ...solo,
    ], form: [
      S(4, "sparkle kick"), S(4, "sparkle kick hats bassA"), S(8, "kick snare hats extra extra2 bassA stabsA hookA"),
      c.solo ? S(4, "soloSnare soloBongo soloKick") : S(4, "sparkle ghost"), S(4, "sparkle ghost roll"), S(8, "kick snare hats extra extra2 bassB stabsB hookB"),
      S(8, "kick snare hats extra extra2 bassA stabsA hookA sparkle", 2), S(8, "kick snare hats extra extra2 bassB stabsB hookB roll", 2), S(4, "kick bassA"),
    ] };
}
// (ticket 360) Matthew, after his board for A and B: "I like more low octave play in the rhythm and melody in general --
// and more groovy bass, with more wah and effects (and more level)." The raves' starting mix from his board: the hooks
// three octaves down, darker and roomier, the stabs, sparkle and ghost two, both basses loud, the wah open, wet and
// panned apart, the first an octave down. A keeps his board as he left it; B his basses with these hooks
const RAVE_LOW = { "hookA·lead": { upN: -3, knob: 80, tone: 27, rev: 55, level: 4 }, "hookB·lead": { upN: -3, knob: 80, tone: 27, rev: 55, echo: 50, level: 6 },
                   "stabsA·lead": { upN: -2 }, "stabsB·lead": { upN: -2 }, "sparkle·musicbox": { upN: -2, rev: 85, echo: 50 }, "ghost·ghost": { upN: -2 } };
const raveBass = inst => ({ [`bassA·${inst}`]: { upN: -1, level: 7, echo: 49, rev: 36, tone: 75, knob: 100, pan: 4 },
                            [`bassB·${inst}`]: { knob: 95, level: 5, pan: -7, echo: 30, rev: 18, tone: 77 } });
const raveMix = v => { const c = CUSHION_RAVE[v]; return v === "A" ? c.mix : { ...RAVE_LOW, ...raveBass(c.wah ? "wahbass" : "buzz"), ...(c.mix || {}) }; };
// and the steppers dubs' (A to D), the same way: the bass a wah bass, loud, open and wet; the melodicas two octaves
// down and the chop one, in the room
const DUB_LOW = { "bass·wahbass": { level: 7, knob: 90, tone: 75, echo: 40, rev: 30 }, "tuneA·melodica": { upN: -2, rev: 60, tone: 60 },
                  "tuneB·melodica": { upN: -2, rev: 60, tone: 60, pan: -4 }, "chop·chop": { upN: -1, echo: 70, level: 4, pan: 4 } };
function witchFunk(v) {
  const c = WITCH_FUNK[v], { bar, L, n } = c, chords = ["Dm", "Dm", "Gm", "Dm", "Bb", "A7", "Dm", "A7"];
  // (n slots over L steps: one funk bar's pattern)
  const slots = (pat, len = L, note) => hitsOf(`${len}(${pat})`, note);
  // (C: a waltz bar of 12 steps re-set as 16, its first beat held two)
  const four = s => { const b = Math.floor(s / 12), r = s - 12 * b; return 16 * b + (r < 4 ? 2 * r : r + 4); };
  const inFour = line => ({ steps: line.steps / 12 * 16, events: line.events.map(e => ({ ...e, at: four(e.at), len: Math.max(0.5, four(e.at + e.len) - four(e.at)) })) });
  const W = TUNES.witch.make().parts, waltz = (name, more = {}) => { const p = { ...W.find(q => q.name === name), ...more }; return c.inFour ? { ...p, line: inFour(p.line) } : p; };
  const bL = c.bassL || L, bN = c.bassN || n, bass = pat => crossOf(chords, bar, bL, pat.map(([at, tn, len, up]) => [at * bL / bN, tn, len * bL / bN, up]), 2);
  const sec = 8 * bar;
  return { bpm: 205, bar, room: 2.4, level: c.level,
    lfos: { A: { beats: 8, shape: "triangle", depth: 2 }, B: { beats: 4, shape: "sine", depth: 4 }, C: { beats: 24, shape: "saw", depth: 3 } },
    parts: [
      waltz("oom", { level: 1.8 }), waltz("pah"), waltz("ghostA"), waltz("ghostB"), waltz("bell"), waltz("twinkles"),
      { name: "kick", inst: "kick", level: 0.9, line: slots(c.kick, c.kickL || L) },
      { name: "snare", inst: "snare", level: 0.55, rev: 0.15, line: slots(c.snare) },
      { name: "hat", inst: "hat", level: 0.32, line: slots(c.hat) },
      { name: "openhat", inst: "openhat", level: 0.28, line: slots(c.open) },
      // (more drums: a shaker, a cowbell, congas)
      { name: "shaker", inst: "shaker", level: 0.35, line: slots(c.shaker) },
      { name: "cowbell", inst: "cowbell", level: 0.3, line: slots(c.cowbell) },
      { name: "conga", inst: "conga", level: 0.5, rev: 0.1, line: slots(c.conga, L, 62) },
      // a roll on the snare into the next section, in its last 16 steps
      { name: "fill", inst: "snare", level: 0.5, line: hitsOf(".".repeat(sec - 16) + "8(xxx) (xox) (xxxxxx)") },
      // the bongos, as the bongo breakdown's, and their solo: the low one keeping the beat, the high one running
      { name: "bongoHi", inst: "bongo", level: 0.7, rev: 0.15, line: slots(c.hi, L, 71), lfo: "B" },
      { name: "bongoLo", inst: "bongo", level: 0.75, rev: 0.15, line: slots(c.lo, L, 64) },
      { name: "soloHi", inst: "bongo", level: 0.8, rev: 0.15, line: hitsOf(`${L}(${c.soloHi}) ${L}(xxxxxxxxxxxx)`, 71), lfo: "A" },
      { name: "soloLo", inst: "bongo", level: 0.85, rev: 0.15, line: slots(c.soloLo, L, 64), lfo: "C" },
      // the wah basses: the low one on the big beats, popping up its octave; the high one answering off them
      { name: "wahLo", inst: "wahbass", level: 1.6, line: bass(c.wahLo) },
      { name: "wahHi", inst: "wahbass", level: 1.1, rev: 0.1, line: bass(c.wahHi) },
    ], form: [
      S(8, "oom pah bell bongoHi bongoLo", -4),
      S(8, "oom pah bell ghostA kick snare hat bongoHi bongoLo shaker fill", -4),
      S(8, "pah wahLo wahHi kick snare hat openhat ghostB twinkles bongoHi bongoLo cowbell conga fill", -4),
      S(8, "pah bell wahLo wahHi kick snare hat openhat ghostA shaker conga bongoHi bongoLo fill", -4),
      S(8, "soloHi soloLo kick wahLo", -4),
      S(8, "pah bell ghostB twinkles kick snare hat openhat wahLo wahHi bongoHi bongoLo cowbell shaker conga fill", -4),
      S(8, "oom pah bell twinkles bongoHi bongoLo", -4),
    ] };
}
const TUNES = {
  forest: { name: "🌲 a walk in the woods", make: () => {
    const chords = ["G", "C", "G", "D", "G", "C", "D", "G"], minor = ["Em", "C", "G", "D", "Em", "Am", "D", "G"];
    return { bpm: 100, room: 2, level: 1.3, parts: [
      { name: "fluteA", inst: "flute", rev: 0.25, line: lineOf(
        "D5:2 G5:2 B5:2 A5:2 G5:4 D5:4 | E5:2 G5:2 C6:2 B5:2 A5:4 G5:4 | B5:2 A5:2 G5:2 A5:2 B5:4 D6:4 | A5:4 F#5:2 E5:2 D5:8 | " +
        "D5:2 G5:2 B5:2 D6:2 C6:2 B5:2 A5:4 | G5:2 E5:2 C5:2 E5:2 G5:4 A5:4 | F#5:2 A5:2 D6:2 C6:2 B5:2 A5:2 F#5:4 | G5:12 r:4") },
      { name: "fluteB", inst: "flute", rev: 0.25, line: lineOf(
        "B5:4 G5:4 D5:4 G5:4 | C6:4 B5:2 A5:2 G5:4 E5:4 | D5:2 E5:2 G5:2 A5:2 B5:8 | A5:2 B5:2 A5:2 F#5:2 D5:8 | " +
        "G5:2 A5:2 B5:4 D6:4 B5:4 | C6:2 B5:2 A5:2 G5:2 E5:8 | F#5:4 A5:4 C6:4 F#5:4 | G5:16") },
      // a third tune, wandering into E minor, over chords of its own
      { name: "fluteC", inst: "flute", rev: 0.3, line: lineOf(
        "E5:4 G5:2 B5:2 E6:4 D6:4 | C6:4 B5:2 A5:2 G5:4 E5:4 | D5:2 G5:2 B5:4 A5:2 G5:2 D5:4 | F#5:4 A5:4 D6:8 | " +
        "E6:4 D6:2 B5:2 G5:4 E5:4 | A5:4 C6:2 E6:2 D6:4 C6:4 | B5:2 A5:2 F#5:2 A5:2 D5:8 | G5:12 r:4") },
      // plucked violins answering the flute, low and steady
      { name: "counter", inst: "fiddle", level: 1, rev: 0.25, line: lineOf(
        "B4:4 D5:4 G4:4 D5:4 | C5:4 E5:4 G4:4 E5:4 | B4:4 G4:4 D5:4 B4:4 | A4:4 F#4:4 D4:4 F#4:4 | " +
        "G4:4 B4:4 D5:4 B4:4 | E5:4 C5:4 G4:4 C5:4 | F#4:4 A4:4 C5:4 A4:4 | G4:8 r:8") },
      { name: "arps", inst: "pizz", level: 0.7, rev: 0.2, line: arpOf(chords, 16, [0, 1, 2, 1], 4, 4) },
      { name: "arpsMinor", inst: "pizz", level: 0.7, rev: 0.2, line: arpOf(minor, 16, [0, 1, 2, 1], 4, 4) },
      { name: "bass", inst: "bass", level: 0.6, line: bassOf(chords, 16, [[0, 0, 6], [8, 7, 6]], 2) },
      { name: "bassMinor", inst: "bass", level: 0.6, line: bassOf(minor, 16, [[0, 0, 6], [8, 7, 6]], 2) },
      { name: "shaker", inst: "shaker", level: 0.5, line: hitsOf("o.x.o.x.o.x.o.x.") },
      { name: "kick", inst: "kick", level: 0.4, line: hitsOf("x.......x.......") },
      { name: "birds", inst: "bird", level: 0.8, rev: 0.3, line: lineOf("r:20 E7:1 r:43 G7:1 r:30 D7:1 r:32") },
      // a cuckoo, far off
      { name: "cuckoo", inst: "flute", level: 0.7, rev: 0.45, line: lineOf("r:8 D6:2 B5:6 r:16 D6:2 B5:6 r:24") },
      // dewdrops, high and few
      { name: "dewdrops", inst: "musicbox", level: 0.5, rev: 0.5, line: lineOf(
        "r:6 D7:1 r:13 B6:1 r:21 G6:1 r:11 A6:1 r:19 D7:1 r:9 E7:1 r:20 B6:1 r:22") },
      // a woodpecker tapping, into the echo
      { name: "woodpecker", inst: "rim", level: 0.8, rev: 0.3, echo: 0.4, line: hitsOf(
        "x.x.x.x.x....... ................ ......x.x.x.x... ................") },
    ], form: [
      S(8, "arps birds"), S(8, "arps bass shaker birds fluteA"), S(8, "arps bass shaker kick fluteB"),
      S(8, "arpsMinor bassMinor shaker fluteC cuckoo"), S(8, "arps birds dewdrops woodpecker"),
      S(8, "arps bass shaker kick fluteB", 5), S(8, "arps bass shaker kick birds fluteA counter"),
      S(8, "arpsMinor bassMinor fluteC dewdrops"), S(8, "arps bass shaker kick birds fluteA counter woodpecker"), S(4, "arps birds cuckoo"),
    ] };
  } },
  beach: { name: "🏖️ beach calypso", make: () => {
    const chords = ["C", "F", "G7", "C", "Am", "F", "G7", "C"], bridge = ["Am", "F", "C", "G", "Am", "F", "G", "G7"];
    return { bpm: 116, room: 1.5, level: 0.72, lfos: { A: { beats: 8, shape: "sine", depth: 2 } }, parts: [
      { name: "panA", inst: "steelpan", rev: 0.2, line: lineOf(
        "E5:2 G5:2 C6:3 G5:1 A5:2 G5:2 E5:4 | F5:2 A5:2 C6:4 A5:2 F5:2 A5:4 | G5:2 B5:2 D6:3 B5:1 G5:2 F5:2 D5:4 | E5:3 D5:1 C5:4 r:2 G4:2 C5:2 E5:2 | " +
        "A5:2 C6:2 E6:3 C6:1 A5:2 G5:2 E5:4 | F5:2 G5:2 A5:2 C6:2 A5:2 G5:2 F5:4 | D6:2 C6:2 B5:2 A5:2 G5:2 F5:2 D5:4 | C5:4 E5:2 G5:2 C6:6 r:2") },
      { name: "panB", inst: "steelpan", rev: 0.2, line: lineOf(
        "G5:2 E5:2 G5:2 C6:2 E6:4 D6:4 | C6:2 A5:2 F5:2 A5:2 C6:4 A5:4 | B5:2 D6:2 F6:2 D6:2 B5:4 G5:4 | C6:4 G5:4 E5:4 C5:4 | " +
        "E5:2 A5:2 C6:2 E6:2 D6:4 C6:4 | A5:2 C6:2 F6:4 E6:2 D6:2 C6:4 | B5:2 C6:2 D6:2 B5:2 G5:2 A5:2 B5:4 | C6:12 r:4") },
      // a third tune, over a bridge of its own: A minor, F, C, G
      { name: "panC", inst: "steelpan", rev: 0.2, line: lineOf(
        "E5:2 A5:2 C6:2 A5:2 E5:4 r:4 | F5:2 A5:2 C6:2 F6:2 E6:4 C6:4 | G5:2 C6:2 E6:4 D6:2 C6:2 G5:4 | B5:2 D6:2 G6:4 F6:2 D6:2 B5:4 | " +
        "A5:2 C6:2 E6:2 A6:2 G6:4 E6:4 | F6:2 E6:2 C6:2 A5:2 C6:4 F6:4 | D6:2 B5:2 G5:2 B5:2 D6:4 G6:4 | F6:4 D6:4 B5:4 G5:4") },
      // a second pan, the chord broken under the tune
      { name: "pan2", inst: "steelpan", level: 0.45, rev: 0.25, line: arpOf(chords, 16, [0, 1, 2, 1], 4, 5) },
      { name: "pluck", inst: "pluck", level: 0.8, rev: 0.15, line: stabsOf(chords, 16, [2, 6, 10, 14], 2, 4) },
      { name: "pluckBridge", inst: "pluck", level: 0.8, rev: 0.15, line: stabsOf(bridge, 16, [2, 6, 10, 14], 2, 4) },
      { name: "bass", inst: "bass", line: bassOf(chords, 16, [[0, 0, 5], [6, 7, 2], [8, 12, 5], [14, 7, 2]], 2) },
      { name: "bassBridge", inst: "bass", line: bassOf(bridge, 16, [[0, 0, 5], [6, 7, 2], [8, 12, 5], [14, 7, 2]], 2) },
      { name: "drums", inst: "kick", level: 0.8, line: hitsOf("x.....x.x.......") },
      { name: "drums", inst: "rim", level: 0.6, line: hitsOf("....x.......x...") },
      { name: "shaker", inst: "shaker", level: 0.5, line: hitsOf("xoxoxoxoxoxoxoxo") },
      { name: "conga", inst: "conga", level: 0.6, line: hitsOf("..x..x.x..x..xx.", midiOf("A3")) },
      // a second conga, higher, answering the first
      { name: "conga2", inst: "conga", level: 0.5, line: hitsOf("x...x..x.x...x..", midiOf("E4")), lfo: "A" },
      // a carnival whistle
      { name: "whistle", inst: "flute", level: 0.7, rev: 0.3, line: lineOf("G6:1 r:1 G6:1 r:1 G6:2 r:10 | r:16 | r:16 | r:16") },
    ], form: [
      S(4, "conga shaker bass"), S(8, "conga shaker bass pluck drums panA"), S(8, "conga shaker bass pluck drums panB"),
      S(8, "conga shaker drums pluckBridge bassBridge panC"), S(8, "conga conga2 shaker panA"),
      S(8, "conga conga2 shaker bass pluck drums pan2 panA whistle", 2), S(8, "conga shaker drums pluckBridge bassBridge panC whistle", 2),
      S(8, "conga conga2 shaker bass pluck drums pan2"), S(8, "conga conga2 shaker bass pluck drums panB", 2), S(4, "conga shaker whistle"),
    ] };
  } },
  soca: { name: "🥁 beach soca jump-up", make: () => {
    const chords = ["D", "G", "A", "D", "Bm", "G", "A", "D"], bridge = ["Bm", "G", "D", "A", "Bm", "G", "A", "A"];
    return { bpm: 128, room: 1.5, level: 0.7, parts: [
      { name: "panA", inst: "steelpan", rev: 0.2, line: lineOf(
        "F#5:2 A5:2 D6:2 A5:2 F#5:2 A5:1 B5:1 A5:4 | G5:2 B5:2 D6:2 B5:2 G5:2 A5:1 B5:1 G5:4 | A5:2 C#6:2 E6:2 C#6:2 A5:2 B5:1 C#6:1 E6:4 | D6:3 C#6:1 D6:2 A5:2 F#5:4 r:4 | " +
        "B5:2 D6:2 F#6:2 D6:2 B5:2 C#6:1 D6:1 B5:4 | G5:2 B5:2 D6:2 E6:2 D6:2 B5:2 G5:4 | E6:2 D6:2 C#6:2 B5:2 A5:2 G5:2 E5:4 | D6:4 A5:2 F#5:2 D5:4 r:4") },
      { name: "panB", inst: "steelpan", rev: 0.2, line: lineOf(
        "A5:1 A5:1 r:2 A5:1 A5:1 r:2 F#5:2 A5:2 D6:4 | B5:1 B5:1 r:2 B5:1 B5:1 r:2 G5:2 B5:2 D6:4 | C#6:1 C#6:1 r:2 C#6:1 C#6:1 r:2 A5:2 C#6:2 E6:4 | D6:2 F#6:2 E6:2 D6:2 A5:8 | " +
        "B5:1 B5:1 r:2 B5:1 B5:1 r:2 F#5:2 B5:2 D6:4 | D6:2 B5:2 G5:2 B5:2 D6:2 E6:2 D6:4 | E6:2 C#6:2 A5:2 C#6:2 E6:2 F#6:2 E6:4 | D6:4 A5:4 D6:8") },
      // a bridge tune: B minor, G, D, A
      { name: "panC", inst: "steelpan", rev: 0.2, line: lineOf(
        "F#5:4 B5:4 D6:4 C#6:2 B5:2 | B5:4 G5:4 D5:8 | A5:4 D6:4 F#6:4 E6:2 D6:2 | C#6:8 A5:8 | " +
        "D6:2 C#6:2 B5:4 F#5:4 B5:4 | G5:4 B5:4 D6:4 G6:4 | E6:4 C#6:4 A5:4 E5:4 | A5:12 r:4") },
      { name: "chop", inst: "chop", level: 0.9, line: stabsOf(chords, 16, [2, 6, 10, 14], 1, 4) },
      { name: "chopB", inst: "chop", level: 0.9, line: stabsOf(bridge, 16, [2, 6, 10, 14], 1, 4) },
      { name: "bass", inst: "bass", line: bassOf(chords, 16, [[0, 0, 3], [3, 0, 1], [6, 7, 2], [8, 12, 3], [11, 7, 1], [14, 0, 2]], 2) },
      { name: "bassB", inst: "bass", line: bassOf(bridge, 16, [[0, 0, 3], [3, 0, 1], [6, 7, 2], [8, 12, 3], [11, 7, 1], [14, 0, 2]], 2) },
      // buzzy synth stabs, for jumping
      { name: "stabs", inst: "buzz", level: 0.5, rev: 0.15, line: rhythmOf(chords, 16, [[0, 2], [3, 1], [6, 2], [10, 1], [12, 2]], 3) },
      { name: "kick", inst: "kick", level: 0.8, line: hitsOf("x...x...x...x...") },
      { name: "snare", inst: "snare", level: 0.6, line: hitsOf("....x.......x...") },
      { name: "hat", inst: "hat", level: 0.6, line: hitsOf("..x...x...x...x.") },
      { name: "bell", inst: "cowbell", level: 0.5, line: hitsOf("x..x..x...x..x..") },
      // the build before a jump: the snare faster and faster
      { name: "roll", inst: "snare", level: 0.5, line: hitsOf("....x...x...x... x.x.x.x.x.x.x.x. xxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxx") },
      { name: "whistle", inst: "flute", level: 0.7, rev: 0.3, line: lineOf("A6:1 r:1 A6:1 r:1 A6:2 r:10 | r:16 | r:16 | r:16") },
    ], form: [
      S(4, "kick bell"), S(4, "kick bell bass hat"), S(8, "kick bell bass hat snare chop panA"), S(8, "kick bell bass hat snare chop panB"),
      S(8, "bell bassB chopB snare panC"), S(4, "bell roll"),
      S(8, "kick bell bass hat snare chop stabs panA whistle"), S(8, "kick bell bass hat snare chop stabs panB"),
      S(8, "kick bell hat bassB chopB panC"), S(4, "bell roll"),
      S(8, "kick bell bass hat snare chop stabs panA", 2), S(4, "kick bell whistle"),
    ] };
  } },
  roots: { name: "🌴 sunny reggae", make: () => {
    const chords = ["G", "Em", "C", "D", "G", "Em", "C", "D"], bridge = ["C", "D", "G", "Em"];
    return { bpm: 76, swing: 0.2, room: 2, level: 0.9, parts: [
      { name: "drums", inst: "kick", line: hitsOf("........x.......") },
      { name: "drums", inst: "rim", level: 0.7, rev: 0.15, line: hitsOf("........x.......") },
      { name: "drums", inst: "hat", level: 0.5, line: hitsOf("o.x.o.x.o.x.o.x.") },
      // sixteenth-note hats, for a busier stretch
      { name: "hat16", inst: "hat", level: 0.35, line: hitsOf("xoxoxoxoxoxoxoxo") },
      { name: "chop", inst: "chop", echo: 0.2, line: stabsOf(chords, 16, [4, 12], 1, 4) },
      // the chop and a rim shot thrown deep into the echo, for the dub sections
      { name: "chopDub", inst: "chop", echo: 0.6, rev: 0.3, line: stabsOf(chords, 16, [4, 12], 1, 4) },
      { name: "rimDub", inst: "rim", level: 0.7, echo: 0.6, rev: 0.3, line: hitsOf("............x...") },
      { name: "organ", inst: "organ", level: 0.45, line: stabsOf(chords, 16, [2, 3, 6, 7, 10, 11, 14, 15], 1, 3) },
      { name: "bass", inst: "bass", line: lineOf(
        "r:2 G2:2 B2:2 D3:2 G3:4 F#3:2 D3:2 | r:2 E2:2 G2:2 B2:2 E3:4 D3:2 B2:2 | r:2 C3:2 E3:2 G3:2 C3:4 D3:2 E3:2 | r:2 D3:2 F#3:2 A3:2 D3:4 C3:2 A2:2") },
      { name: "vampA", inst: "melodica", level: 0.6, rev: 0.2, echo: 0.15, line: rhythmOf(chords, 16, [[2, 1], [6, 1], [8, 3], [14, 1]], 4) },
      { name: "vampB", inst: "melodica", level: 0.8, rev: 0.2, echo: 0.15, line: lineOf(
        "r:8 G4:2 r:2 F#4:1 r:1 D4:1 r:1 | r:8 E4:2 r:2 D4:1 r:1 B3:1 r:1 | r:8 C4:2 r:2 D4:1 r:1 E4:1 r:1 | r:8 D4:2 r:2 C4:1 r:1 A3:1 r:1") },
      // the bridge: C, D, G, E minor, its own bass, chop, bubble and vamp
      { name: "bassBridge", inst: "bass", line: lineOf(
        "r:2 C3:2 E3:2 G3:2 C3:4 B2:2 A2:2 | r:2 D3:2 F#3:2 A3:2 D3:4 C3:2 A2:2 | r:2 G2:2 B2:2 D3:2 G3:4 F#3:2 D3:2 | r:2 E2:2 G2:2 B2:2 E3:4 D3:2 B2:2") },
      { name: "chopBridge", inst: "chop", echo: 0.2, line: stabsOf(bridge, 16, [4, 12], 1, 4) },
      { name: "organBridge", inst: "organ", level: 0.45, line: stabsOf(bridge, 16, [2, 3, 6, 7, 10, 11, 14, 15], 1, 3) },
      { name: "vampBridge", inst: "melodica", level: 0.6, rev: 0.2, echo: 0.15, line: rhythmOf(bridge, 16, [[2, 1], [6, 1], [8, 3], [14, 1]], 4) },
    ], form: [
      S(4, "drums bass"), S(8, "drums bass chop organ vampA"), S(8, "drums bass chop organ vampB"),
      S(8, "drums bass chopDub rimDub"), S(8, "drums bassBridge chopBridge organBridge vampBridge"),
      S(8, "drums hat16 bass chop organ vampA vampB"), S(8, "drums bass chopDub"),
      S(8, "drums bass chop organ vampA vampB"), S(4, "drums bass"),
    ] };
  } },
  underwater: { name: "🐠 bubble reggae", make: () => {
    const chords = ["Am", "Dm", "Am", "Dm", "F", "G", "Am", "Am"];
    return { bpm: 72, swing: 0.22, room: 3, echoSteps: 3, level: 0.85, parts: [
      { name: "drums", inst: "kick", line: hitsOf("........x.......") },
      { name: "drums", inst: "rim", level: 0.7, rev: 0.2, line: hitsOf("........x.......") },
      { name: "drums", inst: "hat", level: 0.5, line: hitsOf("x.o.x.o.x.o.x.o.") },
      { name: "organ", inst: "organ", level: 0.9, rev: 0.2, echo: 0.35, line: stabsOf(chords, 16, [4, 12], 2, 4) },
      { name: "bass", inst: "bass", line: lineOf(
        "r:2 A2:2 A2:2 C3:2 E3:4 D3:2 C3:2 | r:2 D3:2 D3:2 F3:2 A3:4 G3:2 F3:2 | r:2 A2:2 A2:2 C3:2 E3:4 D3:2 C3:2 | r:2 D3:2 D3:2 F3:2 A3:4 G3:2 F3:2 | " +
        "r:2 F2:2 F2:2 A2:2 C3:4 A2:2 F2:2 | r:2 G2:2 G2:2 B2:2 D3:4 B2:2 G2:2 | r:2 A2:2 A2:2 C3:2 E3:4 G3:2 E3:2 | A2:4 r:4 E2:4 G2:4") },
      { name: "panA", inst: "steelpan", rev: 0.35, echo: 0.3, line: lineOf(
        "r:4 E5:2 A5:2 C6:4 B5:2 A5:2 | D5:4 F5:2 A5:6 r:4 | r:4 E5:2 A5:2 C6:2 E6:2 D6:2 C6:2 | A5:8 r:8 | " +
        "r:4 F5:2 A5:2 C6:4 A5:4 | B5:2 D6:2 B5:2 G5:2 r:4 D5:4 | E5:4 A5:4 C6:4 B5:4 | A5:12 r:4") },
      { name: "panB", inst: "steelpan", rev: 0.35, echo: 0.3, line: lineOf(
        "A5:2 C6:2 E6:4 D6:2 C6:2 A5:4 | F5:2 A5:2 D6:4 C6:2 A5:2 F5:4 | E5:4 r:4 E6:4 r:4 | D6:2 C6:2 A5:4 F5:8 | " +
        "C6:4 A5:2 F5:2 A5:4 C6:4 | D6:4 B5:2 G5:2 B5:4 D6:4 | E6:4 C6:4 A5:8 | r:16") },
      { name: "bubbles", inst: "bubble", level: 0.9, rev: 0.5, echo: 0.2, line: lineOf(
        "r:3 C6:1 r:9 G6:1 r:14 E6:1 r:5 A6:1 r:22 D6:1 r:7 F6:1 r:30 B5:1 r:11 E6:1 r:19") },
    ], form: [
      S(4, "bass bubbles"), S(4, "bass bubbles drums"), S(8, "bass bubbles drums organ panA"),
      S(8, "bass bubbles drums organ"), S(8, "bass bubbles drums organ panB"), S(8, "bass bubbles drums organ panA"),
    ] };
  } },
  steppers: { name: "🐢 turtle steppers dub", make: () => steppersTune() },
  // (ticket 263, Matthew: "One new track for music -- 'turtle super-dub step', based on the last .json file I shared with
  // you ... I tweaked the settings, and am currently playing: ▶ playing: 🐢 turtle steppers dub (153 beats a minute, down
  // 12 half steps). Please use those settings for a new version of that song, with a new title, in the track list, same
  // category. use fire emoji as the emoji.") turtle steppers dub as he set it in his save of 2026-10-17 20:09: 153 beats
  // a minute (207% of 74), an octave down, his octave presses on the mixing board baked into the parts (the bass, the
  // sonar's bell and the first melodica back up from there), and the rest of his board its own mix to start from (mix:
  // copied into the player's own at their first change, chan)
  superdub: { name: "🔥 turtle super-dub step", make: () => steppersTune({ bpm: 153, down: 12, ups: { "bass·bass": 12, "sonar·bell": 12, "tuneA·melodica": 24 } }),
    mix: { "kick·kick": { echo: 0 }, "drums·rim": { echo: 0 }, fx: { repeats: 70, time: 7, room: 186 },
           "tuneA·melodica": { knob: 100, pan: -4, rev: 68, level: -7, tone: 45 }, "chop·chop": { tone: 84, level: 6, echo: 81, pan: -4 } } },
  // (ticket 346, Matthew: "mint another version of the turtle dub from these settings ... ▶ playing: 🐢 turtle steppers dub
  // (112 beats a minute, down 13 half steps). Please add to track list") turtle steppers dub as he set it in his save of
  // 2026-10-18 17:45: 112 beats a minute (152% of 74), 13 half steps down, his octave presses on the mixing board baked into
  // the parts (the bass three octaves back up, the sonar's bell and the chop one, the first melodica one more down, the
  // second three), and the rest of his board its own mix to start from, as the super-dub step's (ticket 263)
  deepdub: { name: "🌊 turtle deep-dub", make: () => steppersTune({ bpm: 112, down: 13, ups: { "bass·bass": 36, "sonar·bell": 12, "chop·chop": 12, "tuneA·melodica": -12, "tuneB·melodica": -36 } }),
    mix: { "bass·bass": { level: 9, rev: 41, tone: 17, echo: 39, knob: 10 }, "kick·kick": { echo: 0 }, "drums·rim": { echo: 0 }, fx: { repeats: 70, time: 7, room: 186 },
           "tuneA·melodica": { knob: 100, pan: -4, rev: 68, level: -7, tone: 45 }, "chop·chop": { tone: 84, level: 6, echo: 81, pan: -4 } } },
  // (ticket 290, Matthew: "let's use the turtle dub step, but slowed down, and new melody and chords. the fire emoji one
  // please. 'super dub step' variation.") The bubble room's own: the super-dub step's groove and its octaves (its kick, rim,
  // hats, chop and bass, his presses on the board), slower, at 116 beats a minute, in A minor, its chords going round
  // through F, C and G to a bright E at the end of each eight bars, with tunes of its own, a sonar's ping and bubbles to fit
  bubbledub: { name: "🫧 bubble super-dub step", make: () => steppersTune({ bpm: 116, down: 12, ups: { "bass·bass": 12, "sonar·bell": 12, "tuneA·melodica": 24 },
      chords: ["Am", "F", "C", "G", "Am", "F", "G", "E"], lines: {
        sonar: "A6:1 r:63 E6:1 r:63",
        tuneA: "r:4 A4:2 C5:2 E5:4 A5:4 | A5:4 G5:2 F5:2 C5:8 | r:4 E5:2 G5:2 C6:6 B5:2 | B5:4 A5:2 G5:2 D5:8 | " +
               "r:4 C6:2 B5:2 A5:4 E5:4 | F5:4 A5:4 C6:8 | B5:4 D6:4 G5:4 B5:4 | G#5:8 E5:4 r:4",
        tuneB: "E5:4 r:4 A5:4 r:4 | C6:8 A5:4 F5:4 | G5:4 r:4 E5:4 G5:4 | D6:8 B5:8 | A5:4 C6:4 E6:8 | D6:4 C6:4 A5:8 | B5:4 G5:4 D5:8 | B4:4 E5:4 G#5:8",
        bubbles: "r:9 A6:1 r:21 E6:1 r:17 C7:1 r:33 G6:1 r:11 B6:1 r:32" } }),
    mix: { "kick·kick": { echo: 0 }, "drums·rim": { echo: 0 }, fx: { repeats: 70, time: 7, room: 186 },
           "tuneA·melodica": { knob: 100, pan: -4, rev: 68, level: -7, tone: 45 }, "chop·chop": { tone: 84, level: 6, echo: 81, pan: -4 } } },
  lullaby: { name: "🎐 steel-pan lullaby", make: () => {
    const chords = ["F", "Dm", "Bb", "C", "F", "Dm", "Bb", "C"];
    return { bpm: 84, room: 2.5, level: 1, parts: [
      { name: "panA", inst: "steelpan", rev: 0.3, line: lineOf(
        "A5:4 C6:4 A5:2 G5:2 F5:4 | F5:4 A5:4 D6:8 | D6:4 C6:2 Bb5:2 A5:4 G5:4 | G5:8 E5:4 C5:4 | " +
        "A5:2 Bb5:2 C6:4 F6:4 C6:4 | D6:4 C6:2 A5:2 F5:8 | Bb5:4 D6:4 F6:4 D6:4 | C6:8 E5:4 G5:4") },
      { name: "panB", inst: "steelpan", rev: 0.3, line: lineOf(
        "C6:6 A5:2 F5:8 | D6:6 C6:2 A5:8 | Bb5:4 D6:4 F6:8 | E6:6 D6:2 C6:8 | A5:4 C6:4 F6:4 C6:4 | F6:4 D6:4 A5:8 | G5:4 Bb5:4 D6:4 F6:4 | E6:8 G5:8") },
      // a second pan, the chord broken gently under the tune
      { name: "arps", inst: "steelpan", level: 0.45, rev: 0.3, line: arpOf(chords, 16, [0, 1, 2, 1], 4, 5) },
      { name: "bass", inst: "bass", level: 0.6, line: bassOf(chords, 16, [[0, 0, 6], [6, 7, 2], [8, 0, 6], [14, 7, 2]], 2) },
      { name: "hands", inst: "shaker", level: 0.5, line: hitsOf("o.o.o.o.o.o.o.o.") },
      { name: "hands", inst: "conga", level: 0.4, line: hitsOf("x.....x.x.......", midiOf("C4")) },
      { name: "bubbles", inst: "bubble", level: 0.7, rev: 0.5, line: lineOf("r:14 C6:1 r:29 A6:1 r:41 F6:1 r:41") },
      { name: "twinkles", inst: "musicbox", level: 0.5, rev: 0.5, line: lineOf(
        "r:4 C7:1 r:11 | r:8 A6:1 r:7 | r:4 F6:1 r:11 | r:12 G6:1 r:3 | r:2 C7:1 r:13 | r:6 D7:1 r:9 | r:4 Bb6:1 r:11 | r:8 E6:1 r:7") },
    ], form: [
      S(8, "arps bubbles"), S(8, "arps bubbles bass panA"), S(8, "arps bubbles bass hands panB"),
      S(8, "arps bubbles twinkles"), S(8, "arps bubbles bass hands panA twinkles"), S(4, "arps bubbles"),
    ] };
  } },
  witch: { name: "🧙 spooky waltz", make: () => {
    // three beats a bar: 12 steps
    const chords = ["Dm", "Dm", "Gm", "Dm", "Bb", "A7", "Dm", "A7"];
    return { bpm: 120, bar: 12, room: 3, level: 1.1, parts: [
      { name: "oom", inst: "bass", line: bassOf(chords, 12, [[0, 0, 3]], 2) },
      { name: "pah", inst: "pizz", level: 0.6, rev: 0.3, line: stabsOf(chords, 12, [4, 8], 2, 4) },
      { name: "ghostA", inst: "ghost", rev: 0.45, line: lineOf(
        "A4:4 D5:4 F5:4 | E5:4 D5:4 C#5:4 | D5:4 G5:4 Bb5:4 | A5:8 r:4 | Bb5:4 A5:4 G5:4 | F5:4 E5:4 C#5:4 | D5:4 F5:2 E5:2 D5:4 | C#5:6 E5:2 A4:4") },
      { name: "ghostB", inst: "ghost", rev: 0.45, line: lineOf(
        "F5:8 E5:4 | D5:4 A4:8 | Bb4:4 D5:4 G5:4 | F5:12 | D5:4 F5:4 Bb5:4 | A5:4 G5:4 E5:4 | F5:4 E5:4 D5:4 | C#5:12") },
      { name: "bell", inst: "bell", level: 0.8, rev: 0.5, line: lineOf("D3:24 r:24 A2:24 r:24") },
      { name: "twinkles", inst: "musicbox", level: 0.6, rev: 0.6, line: lineOf("r:10 A6:1 r:13 F6:1 r:11 D6:1 r:23 C#7:1 r:12 E6:1 r:22") },
    ], form: [
      S(8, "oom pah bell"), S(8, "oom pah bell ghostA"), S(8, "oom pah ghostB twinkles"),
      S(8, "oom bell twinkles"), S(8, "oom pah bell ghostA twinkles"), S(8, "oom pah bell ghostB"),
    ] };
  } },
  // The spooky waltz, funked, five ways (Matthew picks): see witchFunk
  ...Object.fromEntries(Object.entries(WITCH_FUNK).map(([v, c]) => ["witchfunk" + v, { name: `🧙 funked waltz ${v}: ${c.what}`, make: () => witchFunk(v) }])),
  // (ticket 301: the couch-cushion raves, A to E, for M; and the turtle steppers dubs, A to D; ticket 374: D-1 to D-4 beside D)
  ...Object.fromEntries(Object.entries(TURTLE_DUB).map(([v, c]) => ["steppers" + v, { name: `🐢 turtle steppers dub ${c.label || v}: ${c.what}`, make: () => turtleDub(v), mix: DUB_LOW }])),
  ...Object.fromEntries(Object.entries(CUSHION_RAVE).map(([v, c]) => ["cushions" + v, { name: `🛋️ couch-cushion rave ${v}: ${c.what}`, make: () => cushionRave(v), mix: raveMix(v) }])),
  // Peaceful waltzes: three beats a bar (12 steps), oom-pah-pah under a
  // tune, two eight-bar halves, each with its own chords
  castlewaltz: { name: "🏰 castle waltz", make: () => {
    // stately, in G: a flute over harpsichord oom-pah-pah, strings holding the chords, bells
    const A = ["G", "Em", "C", "D7", "G", "Em", "Am", "D7"], B = ["C", "G", "Am", "D7", "G", "C", "D7", "G"];
    return { bpm: 112, bar: 12, room: 2.5, level: 1.05, parts: [
      { name: "oomA", inst: "bass", level: 0.7, line: bassOf(A, 12, [[0, 0, 4]], 2) },
      { name: "oomB", inst: "bass", level: 0.7, line: bassOf(B, 12, [[0, 0, 4]], 2) },
      { name: "pahA", inst: "harpsichord", level: 0.4, rev: 0.2, line: stabsOf(A, 12, [4, 8], 3, 4) },
      { name: "pahB", inst: "harpsichord", level: 0.4, rev: 0.2, line: stabsOf(B, 12, [4, 8], 3, 4) },
      { name: "fluteA", inst: "flute", rev: 0.35, line: lineOf(
        "D5:8 B4:4 | G5:8 E5:4 | E5:4 G5:4 C6:4 | B5:8 A5:4 | G5:4 D5:4 B4:4 | E5:6 F#5:2 G5:4 | A5:4 C6:4 E5:4 | D5:12") },
      { name: "fluteB", inst: "flute", rev: 0.35, line: lineOf(
        "E5:4 G5:4 C6:4 | B5:8 G5:4 | C6:4 B5:4 A5:4 | F#5:8 D5:4 | G5:6 A5:2 B5:4 | C6:4 E6:4 C6:4 | B5:4 A5:4 F#5:4 | G5:12") },
      { name: "stringsA", inst: "fiddle", level: 0.35, rev: 0.4, line: lineOf("B4:12 | B4:12 | C5:12 | C5:12 | B4:12 | G4:12 | A4:12 | F#4:12") },
      { name: "stringsB", inst: "fiddle", level: 0.35, rev: 0.4, line: lineOf("G4:12 | G4:12 | A4:12 | A4:12 | B4:12 | C5:12 | C5:12 | B4:12") },
      { name: "bells", inst: "bell", level: 0.5, rev: 0.5, line: lineOf("G4:48 D4:48") },
    ], form: [
      S(8, "oomA pahA"), S(8, "oomA pahA fluteA"), S(8, "oomB pahB fluteB stringsB"),
      S(8, "oomA pahA fluteA stringsA bells"), S(8, "oomB pahB fluteB stringsB bells"), S(8, "oomA pahA stringsA"),
    ] };
  } },
  ballroom: { name: "👑 ballroom waltz", make: () => {
    // in D: fiddles leading, harpsichord broken chords, a flute answering in the second half
    const A = ["D", "Bm", "G", "A7", "D", "Bm", "Em", "A7"], B = ["G", "D", "Em", "A7", "D", "G", "A7", "D"];
    return { bpm: 120, bar: 12, room: 3, level: 1.05, parts: [
      { name: "oomA", inst: "bass", level: 0.7, line: bassOf(A, 12, [[0, 0, 4]], 2) },
      { name: "oomB", inst: "bass", level: 0.7, line: bassOf(B, 12, [[0, 0, 4]], 2) },
      { name: "pahA", inst: "pizz", level: 0.5, rev: 0.25, line: stabsOf(A, 12, [4, 8], 2, 4) },
      { name: "pahB", inst: "pizz", level: 0.5, rev: 0.25, line: stabsOf(B, 12, [4, 8], 2, 4) },
      { name: "violinsA", inst: "fiddle", rev: 0.35, line: lineOf(
        "F#5:4 A5:4 D6:4 | D6:8 B5:4 | B5:4 G5:4 D5:4 | E5:4 A5:4 C#6:4 | D6:6 E6:2 F#6:4 | F#6:4 D6:4 B5:4 | G5:4 B5:4 E6:4 | C#6:12") },
      { name: "violinsB", inst: "fiddle", rev: 0.35, line: lineOf(
        "D6:4 B5:4 G5:4 | A5:8 F#5:4 | G5:4 B5:4 E6:4 | E6:8 C#6:4 | D6:4 F#6:4 A6:4 | G6:6 F#6:2 E6:4 | E6:4 G6:4 C#6:4 | D6:12") },
      { name: "harpA", inst: "harpsichord", level: 0.3, rev: 0.2, line: arpOf(A, 12, [0, 1, 2, 3, 2, 1], 2, 4) },
      { name: "harpB", inst: "harpsichord", level: 0.3, rev: 0.2, line: arpOf(B, 12, [0, 1, 2, 3, 2, 1], 2, 4) },
      { name: "fluteB", inst: "flute", level: 0.6, rev: 0.4, line: lineOf("r:12 | r:8 A5:4 | B5:12 | r:8 C#6:4 | r:12 | B5:8 G5:4 | A5:12 | F#5:12") },
    ], form: [
      S(8, "oomA pahA harpA"), S(8, "oomA pahA violinsA"), S(8, "oomB pahB violinsB fluteB"),
      S(8, "oomA pahA harpA violinsA"), S(8, "oomB pahB harpB violinsB fluteB"), S(8, "oomA pahA harpA"),
    ] };
  } },
  healerwaltz: { name: "🌿 healer's waltz", make: () => {
    // slow and warm, in F: vibes singing over soft chords, plucked oom-pah-pah, a music box
    const A = ["F", "Dm", "Bb", "C7", "F", "Am", "Bb", "C7"], B = ["Bb", "F", "Gm", "C7", "F", "Dm", "Bb", "C7"];
    return { bpm: 90, bar: 12, room: 3, level: 1, parts: [
      { name: "oomA", inst: "bass", level: 0.55, line: bassOf(A, 12, [[0, 0, 4]], 2) },
      { name: "oomB", inst: "bass", level: 0.55, line: bassOf(B, 12, [[0, 0, 4]], 2) },
      { name: "pahA", inst: "pluck", level: 0.35, rev: 0.3, line: stabsOf(A, 12, [4, 8], 2, 4) },
      { name: "pahB", inst: "pluck", level: 0.35, rev: 0.3, line: stabsOf(B, 12, [4, 8], 2, 4) },
      { name: "padA", inst: "pad", level: 0.3, rev: 0.5, line: rhythmOf(A, 12, [[0, 12]], 4) },
      { name: "padB", inst: "pad", level: 0.3, rev: 0.5, line: rhythmOf(B, 12, [[0, 12]], 4) },
      { name: "tuneA", inst: "vibes", rev: 0.45, line: lineOf(
        "A5:8 C6:4 | D6:8 A5:4 | Bb5:4 D6:4 F6:4 | E6:8 C6:4 | F6:4 E6:4 C6:4 | E6:6 D6:2 C6:4 | D6:4 Bb5:4 G5:4 | C6:12") },
      { name: "tuneB", inst: "vibes", rev: 0.45, line: lineOf(
        "D6:8 F6:4 | C6:8 A5:4 | Bb5:4 G5:4 D6:4 | C6:8 Bb5:4 | A5:6 Bb5:2 C6:4 | D6:4 F6:4 A5:4 | Bb5:4 C6:4 D6:4 | E5:8 G5:4") },
      { name: "musicbox", inst: "musicbox", level: 0.45, rev: 0.6, line: lineOf("r:10 C7:1 r:13 A6:1 r:11 F6:1 r:23 G6:1 r:12 E6:1 r:22") },
    ], form: [
      S(8, "oomA padA"), S(8, "oomA pahA padA tuneA"), S(8, "oomB pahB padB tuneB musicbox"),
      S(8, "oomA pahA padA tuneA musicbox"), S(8, "oomB pahB padB tuneB"), S(8, "oomA padA musicbox"),
    ] };
  } },
  bats: { name: "🦇 bat-wing boogie", make: () => {
    // a low, buzzy bass going round and round, and plucked violins creeping over it
    const A = ["Cm", "Cm", "Ab", "G7"], B = ["Fm", "Cm", "Ab", "G7"];
    return { bpm: 104, room: 2, level: 1.1, parts: [
      { name: "bassA", inst: "buzz", line: lineOf(
        "C2:2 C3:2 G2:2 C3:2 Eb2:2 C3:2 G2:2 Bb2:2 | C2:2 C3:2 G2:2 C3:2 Eb2:2 C3:2 G2:2 Bb2:2 | " +
        "Ab1:2 Ab2:2 Eb2:2 Ab2:2 C2:2 Ab2:2 Eb2:2 G2:2 | G1:2 G2:2 D2:2 G2:2 B1:2 G2:2 F2:2 D2:2") },
      { name: "bassB", inst: "buzz", line: lineOf(
        "F1:2 F2:2 C2:2 F2:2 Ab1:2 F2:2 C2:2 Eb2:2 | C2:2 C3:2 G2:2 C3:2 Eb2:2 C3:2 G2:2 Bb2:2 | " +
        "Ab1:2 Ab2:2 Eb2:2 Ab2:2 C2:2 Ab2:2 Eb2:2 G2:2 | G1:2 G2:2 D2:2 G2:2 B1:2 G2:2 F2:2 D2:2") },
      { name: "violinsA", inst: "fiddle", rev: 0.25, line: lineOf(
        "G5:1 r:1 G5:1 r:1 Eb5:2 C5:2 D5:1 r:1 Eb5:1 r:1 F5:2 G5:2 | Ab5:2 G5:2 F5:2 Eb5:2 D5:4 r:4 | " +
        "C6:1 r:1 C6:1 r:1 Ab5:2 Eb5:2 F5:1 r:1 G5:1 r:1 Ab5:2 C6:2 | B5:2 C6:2 D6:2 B5:2 G5:4 r:4") },
      { name: "violinsB", inst: "fiddle", rev: 0.25, line: lineOf(
        "F5:2 Ab5:2 C6:4 Bb5:2 Ab5:2 G5:4 | G5:2 Eb5:2 C5:4 D5:2 Eb5:2 G5:4 | Ab5:2 C6:2 Eb6:4 D6:2 C6:2 Ab5:4 | G5:2 F5:2 D5:2 B4:2 G4:8") },
      { name: "plucksA", inst: "fiddle", level: 0.45, line: stabsOf(A, 16, [4, 12], 1, 4) },
      { name: "plucksB", inst: "fiddle", level: 0.45, line: stabsOf(B, 16, [4, 12], 1, 4) },
      { name: "drums", inst: "kick", level: 0.6, line: hitsOf("x.......x.......") },
      { name: "drums", inst: "rim", level: 0.5, rev: 0.2, line: hitsOf("....x.......x...") },
      { name: "drums", inst: "hat", level: 0.4, line: hitsOf("x.x.x.x.x.x.x.x.") },
      { name: "ghost", inst: "ghost", level: 0.8, rev: 0.5, line: lineOf("C5:16 B4:16 Ab4:16 G4:16") },
    ], form: [
      S(4, "bassA"), S(4, "bassA drums"), S(8, "bassA drums violinsA"), S(8, "bassB drums violinsB plucksB"),
      S(4, "bassA ghost"), S(8, "bassA drums violinsA plucksA"), S(8, "bassB drums violinsB plucksB"), S(4, "bassA drums"),
    ] };
  } },
  // (ticket 372, Matthew: "new bat wing boogie remix track, based on these settings and adjustments", his save of 23:07) the
  // bat-wing boogie at his tempo (166% of its 104) and down 14 half steps (its own shift, on top of the music's transpose), with
  // his mix for it as its own: each channel's octave, level, knob, pan, tone, reverb and echo, and his five dups
  batsremix: { name: "🦇 bat-wing boogie remix", key: "Bb minor", make: () => ({ ...TUNES.bats.make(), bpm: 173, shift: -14 }),
    mix: {"bassA·buzz": {"upN": 0, "knob": 55, "pan": -3, "tone": 100, "rev": 50, "echo": 0, "level": 10}, "bassB·buzz": {"upN": 1, "level": 8, "knob": 100, "pan": 8}, "violinsA·fiddle": {"upN": 1}, "plucksB·fiddle": {"upN": 2, "level": 10, "rev": 80, "echo": 64}, "dups": [{"of": "plucksB·fiddle", "name": "plucksB dup"}, {"of": "drums·hat", "name": "drums dup"}, {"of": "bassA·buzz", "name": "bassA dup"}, {"of": "violinsA·fiddle", "name": "violinsA dup"}, {"of": "plucksB·fiddle", "name": "plucksB dup 2"}], "plucksB dup·fiddle": {"upN": 0, "level": 10, "rev": 80, "echo": 64}, "drums dup·hat": {"rev": 62, "mute": true}, "bassA dup·buzz": {"upN": 3, "knob": 99, "pan": -4, "tone": 91, "rev": 49, "echo": 46, "level": 2, "solo": false}, "violinsA dup·fiddle": {"upN": -2, "level": 5, "solo": false}, "plucksB dup 2·fiddle": {"upN": -1, "level": 10, "rev": 80, "echo": 64, "solo": false}} },
  // Four chase tunes, made like the bat-wing boogie: synth parts layered
  // one on another, drums dropping out and crashing back in. They are
  // written at a dancing speed, so the tempo slider can take them on a
  // chase (try 250%).
  treechase: { name: "🌳 tree-house chase", make: () => {
    // a pumping bass, plucks racing up and down, a synth hook, laser zaps
    const A = ["Am", "F", "C", "G"], B = ["Dm", "Am", "F", "E"];
    const pump = [[0, 0, 2], [2, 12, 2], [4, 0, 2], [6, 12, 2], [8, 0, 2], [10, 12, 2], [12, 0, 2], [14, 12, 2]];
    return { bpm: 110, room: 1.5, level: 1.1, parts: [
      { name: "bassA", inst: "buzz", line: bassOf(A, 16, pump, 2) },
      { name: "bassB", inst: "buzz", line: bassOf(B, 16, pump, 2) },
      { name: "arpsA", inst: "pluck", level: 0.5, rev: 0.2, line: arpOf(A, 16, [0, 1, 2, 3, 4, 3, 2, 1], 1, 4) },
      { name: "arpsB", inst: "pluck", level: 0.5, rev: 0.2, line: arpOf(B, 16, [0, 1, 2, 3, 4, 3, 2, 1], 1, 4) },
      { name: "hookA", inst: "lead", rev: 0.2, line: lineOf(
        "A5:2 r:1 A5:1 C6:2 A5:2 E6:3 D6:1 C6:2 B5:2 | A5:2 r:1 A5:1 C6:2 A5:2 F5:4 G5:4 | " +
        "E5:2 r:1 E5:1 G5:2 C6:2 E6:3 D6:1 C6:2 G5:2 | B5:2 r:1 B5:1 D6:2 B5:2 G5:4 r:4") },
      { name: "hookB", inst: "lead", rev: 0.2, line: lineOf(
        "D6:4 F6:4 E6:2 D6:2 C6:4 | C6:4 E6:4 D6:2 C6:2 A5:4 | F5:2 A5:2 C6:2 F6:2 E6:2 C6:2 A5:4 | G#5:2 B5:2 E6:2 G#6:2 E6:8") },
      { name: "padA", inst: "pad", level: 0.7, rev: 0.4, line: rhythmOf(A, 16, [[0, 16]], 3) },
      { name: "padB", inst: "pad", level: 0.7, rev: 0.4, line: rhythmOf(B, 16, [[0, 16]], 3) },
      { name: "kick", inst: "kick", level: 0.8, line: hitsOf("x...x...x...x...") },
      { name: "snare", inst: "snare", level: 0.5, rev: 0.2, line: hitsOf("....x.......x...") },
      { name: "hats", inst: "hat", level: 0.35, line: hitsOf("..x...x...x...x.") },
      { name: "hats16", inst: "hat", level: 0.3, line: hitsOf("xoxoxoxoxoxoxoxo") },
      { name: "roll", inst: "snare", level: 0.45, line: hitsOf("................ ................ x...x...x...x... x.x.x.x.xxxxxxxx") },
      { name: "zaps", inst: "zap", level: 0.6, rev: 0.3, line: lineOf("r:12 E6:1 r:3 | r:16 | r:12 A6:1 r:1 A6:1 r:1 | r:16") },
    ], form: [
      S(4, "arpsA"), S(4, "arpsA bassA hats"), S(4, "arpsA bassA hats roll"),
      S(8, "arpsA bassA kick snare hats16 hookA"), S(8, "arpsB bassB kick snare hats16 hookB padB zaps"),
      S(4, "padA arpsA"), S(4, "padA arpsA bassA roll"),
      S(8, "arpsA bassA kick snare hats16 hookA zaps", 2), S(8, "arpsB bassB kick snare hats16 hookB padB", 2), S(4, "bassA kick"),
    ] };
  } },
  duckdash: { name: "🦆 duck-pond dash", make: () => {
    // quick and sunny: a bouncing bass, steel pan and plucks chasing each
    // other, hand drums under a four-on-the-floor kick, a bird or two
    const A = ["F", "C", "Dm", "Bb"], B = ["Gm", "Am", "Bb", "C"];
    const bounce = [[0, 0, 2], [3, 12, 1], [4, 0, 2], [6, 7, 2], [8, 0, 2], [11, 12, 1], [12, 7, 2], [14, 12, 2]];
    return { bpm: 112, room: 1.5, level: 1.1, parts: [
      { name: "bassA", inst: "bass", line: bassOf(A, 16, bounce, 2) },
      { name: "bassB", inst: "bass", line: bassOf(B, 16, bounce, 2) },
      { name: "arpsA", inst: "pluck", level: 0.45, rev: 0.2, line: arpOf(A, 16, [0, 2, 1, 2, 3, 2, 1, 2], 1, 4) },
      { name: "arpsB", inst: "pluck", level: 0.45, rev: 0.2, line: arpOf(B, 16, [0, 2, 1, 2, 3, 2, 1, 2], 1, 4) },
      { name: "panA", inst: "steelpan", rev: 0.25, line: lineOf(
        "A5:2 C6:2 A5:1 G5:1 F5:2 C6:4 A5:4 | G5:2 E5:2 C5:2 E5:2 G5:4 C6:4 | " +
        "A5:2 F5:2 D5:2 F5:2 A5:2 D6:2 C6:4 | Bb5:2 A5:2 G5:2 F5:2 D5:4 F5:4") },
      { name: "panB", inst: "steelpan", rev: 0.25, line: lineOf(
        "G5:3 Bb5:3 D6:2 C6:4 Bb5:4 | A5:3 C6:3 E6:2 D6:4 C6:4 | Bb5:3 D6:3 F6:2 E6:4 D6:4 | E6:2 D6:2 C6:2 Bb5:2 A5:2 G5:2 E5:4") },
      { name: "kick", inst: "kick", level: 0.75, line: hitsOf("x...x...x...x...") },
      { name: "snare", inst: "snare", level: 0.4, rev: 0.2, line: hitsOf("....x.......x...") },
      { name: "congas", inst: "conga", level: 0.6, line: hitsOf("..x.x..x..x.x..x", 64) },
      { name: "congas", inst: "conga", level: 0.6, line: hitsOf("x.....x.x.....x.", 57) },
      { name: "shaker", inst: "shaker", level: 0.6, line: hitsOf("xoxoxoxoxoxoxoxo") },
      { name: "cowbell", inst: "cowbell", level: 0.35, line: hitsOf("x..x..x...x..x..") },
      { name: "roll", inst: "conga", level: 0.6, line: hitsOf("................ ................ x.x.x.x.x.x.x.x. xxxxxxxxxxxxxxxx", 64) },
      { name: "birds", inst: "bird", level: 0.5, rev: 0.4, line: lineOf("r:8 C7:1 r:23 G6:1 r:19 E7:1 r:11") },
    ], form: [
      S(4, "arpsA congas"), S(4, "arpsA bassA congas shaker"), S(8, "arpsA bassA kick congas shaker panA"),
      S(8, "arpsB bassB kick snare shaker cowbell panB"), S(4, "bassA congas birds"), S(4, "bassA congas shaker roll"),
      S(8, "arpsA bassA kick snare shaker cowbell panA", 2), S(8, "arpsB bassB kick snare congas shaker panB", 2), S(4, "arpsA kick birds"),
    ] };
  } },
  cushions: { name: "🛋️ couch-cushion rave", make: () => {
    // a bass on the off-beats, synth stabs, a music box spinning on top,
    // and a ghost swooping through the quiet part
    const A = ["Em", "C", "G", "D"], B = ["Am", "C", "D", "B7"];
    return { bpm: 108, room: 1.8, level: 1.1, parts: [
      { name: "bassA", inst: "buzz", line: bassOf(A, 16, [[2, 0, 2], [6, 0, 2], [10, 0, 2], [14, 12, 2]], 2) },
      { name: "bassB", inst: "buzz", line: bassOf(B, 16, [[2, 0, 2], [6, 0, 2], [10, 0, 2], [14, 12, 2]], 2) },
      { name: "stabsA", inst: "lead", level: 0.45, rev: 0.25, line: stabsOf(A, 16, [0, 3, 6, 10, 12], 1, 4) },
      { name: "stabsB", inst: "lead", level: 0.45, rev: 0.25, line: stabsOf(B, 16, [0, 3, 6, 10, 12], 1, 4) },
      { name: "sparkle", inst: "musicbox", level: 0.5, rev: 0.4, line: arpOf(A, 16, [0, 1, 2, 3, 4, 5, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5], 1, 5) },
      { name: "hookA", inst: "lead", rev: 0.2, line: lineOf(
        "B5:3 B5:3 G5:2 E5:4 G5:2 A5:2 | G5:3 G5:3 E5:2 C5:4 E5:2 G5:2 | D6:3 D6:3 B5:2 G5:4 B5:2 D6:2 | F#6:3 E6:3 D6:2 A5:4 F#5:4") },
      { name: "hookB", inst: "lead", rev: 0.2, line: lineOf(
        "C6:2 B5:2 A5:2 E5:2 A5:4 C6:4 | E6:2 D6:2 C6:2 G5:2 E6:4 G6:4 | F#6:2 E6:2 D6:2 A5:2 F#6:4 A6:4 | D#6:4 B5:4 F#5:4 D#5:4") },
      { name: "ghost", inst: "ghost", level: 0.7, rev: 0.5, line: lineOf("E5:32 B5:32") },
      { name: "kick", inst: "kick", level: 0.85, line: hitsOf("x...x...x...x...") },
      { name: "snare", inst: "snare", level: 0.45, rev: 0.25, line: hitsOf("....x.......x...") },
      { name: "hats", inst: "hat", level: 0.4, line: hitsOf("..x...x...x...x.") },
      { name: "shaker", inst: "shaker", level: 0.5, line: hitsOf("xoxoxoxoxoxoxoxo") },
      { name: "roll", inst: "snare", level: 0.45, line: hitsOf("................ ................ x...x...x...x... x.x.x.x.xxxxxxxx") },
    ], form: [
      S(4, "sparkle kick"), S(4, "sparkle kick hats bassA"), S(8, "kick snare hats bassA stabsA hookA"),
      S(4, "sparkle ghost"), S(4, "sparkle ghost roll"), S(8, "kick snare hats shaker bassB stabsB hookB"),
      S(8, "kick snare hats shaker bassA stabsA hookA sparkle", 2), S(8, "kick snare hats shaker bassB stabsB hookB", 2), S(4, "kick bassA"),
    ] };
  } },
  housechase: { name: "🏠 round-the-house chase", make: () => {
    // a spy's creeping bass riff, a harpsichord racing over it, violin
    // stabs, and a hook that runs off up the stairs; each time round, higher
    const A = ["Dm", "Dm", "Bb", "A7"];
    return { bpm: 104, room: 1.5, level: 1.1, parts: [
      { name: "riff", inst: "buzz", line: lineOf(
        "D2:1 D2:1 r:1 D2:1 F2:2 D2:2 G2:2 D2:2 G#2:2 A2:2 | D2:1 D2:1 r:1 D2:1 F2:2 D2:2 G2:2 D2:2 G#2:2 A2:2 | " +
        "Bb1:1 Bb1:1 r:1 Bb1:1 D2:2 Bb1:2 C2:2 Bb1:2 C#2:2 D2:2 | A1:1 A1:1 r:1 A1:1 C#2:2 A1:2 E2:2 A1:2 G2:2 C#2:2") },
      { name: "harp", inst: "harpsichord", level: 0.5, rev: 0.2, line: arpOf(A, 16, [0, 1, 2, 1, 0, 1, 2, 3], 1, 4) },
      { name: "stabs", inst: "fiddle", level: 0.55, rev: 0.2, line: stabsOf(A, 16, [0, 6, 12], 1, 4) },
      { name: "hook", inst: "lead", rev: 0.2, line: lineOf(
        "D6:2 r:2 D6:2 r:2 F6:2 E6:2 D6:2 C#6:2 | D6:2 r:2 D6:2 r:2 A6:4 G6:2 F6:2 | " +
        "Bb5:2 r:2 Bb5:2 r:2 D6:2 C6:2 Bb5:2 A5:2 | A5:2 C#6:2 E6:2 G6:2 A6:4 r:4") },
      { name: "kick", inst: "kick", level: 0.8, line: hitsOf("x.x...x.x.x...x.") },
      { name: "snare", inst: "snare", level: 0.5, rev: 0.2, line: hitsOf("....x.......x..x") },
      { name: "hats16", inst: "hat", level: 0.3, line: hitsOf("xoxoxoxoxoxoxoxo") },
      { name: "toms", inst: "conga", level: 0.7, line: hitsOf("x..x..x.x..x..x.", 50) },
      { name: "roll", inst: "snare", level: 0.45, line: hitsOf("................ ................ x...x...x...x... x.x.x.x.xxxxxxxx") },
      { name: "zaps", inst: "zap", level: 0.55, rev: 0.3, line: lineOf("r:14 D7:1 r:1 | r:16 | r:12 A6:1 r:1 A6:1 r:1 | r:16") },
    ], form: [
      S(4, "riff"), S(4, "riff hats16"), S(4, "riff hats16 kick harp"),
      S(8, "riff kick snare hats16 harp hook stabs"), S(8, "riff kick snare hats16 harp hook stabs zaps", 5),
      S(4, "harp toms"), S(4, "harp toms roll"),
      S(8, "riff kick snare hats16 hook stabs"), S(8, "riff kick snare hats16 harp hook stabs zaps", 7), S(4, "riff kick"),
    ] };
  } },
  harpsichord: { name: "🕯️ haunted harpsichord", make: () => {
    // broken chords over a bass that creeps down a step a bar, and a clock ticking
    const chords = ["Dm", "A7", "Dm", "G", "Gm", "Dm", "A7", "Dm"];
    return { bpm: 92, room: 2.5, level: 1.3, parts: [
      { name: "arps", inst: "harpsichord", level: 0.55, rev: 0.25, line: arpOf(chords, 16, [0, 1, 2, 3, 4, 3, 2, 1], 1, 4) },
      { name: "bass", inst: "harpsichord", rev: 0.25, line: lineOf(
        "D3:8 D3:8 | C#3:8 C#3:8 | C3:8 C3:8 | B2:8 B2:8 | Bb2:8 Bb2:8 | A2:8 A2:8 | A2:8 E2:8 | D2:16") },
      { name: "tune", inst: "harpsichord", level: 1.3, rev: 0.25, line: lineOf(
        "A5:4 F5:2 D5:2 A5:4 D6:4 | C#6:4 E5:2 G5:2 Bb5:4 A5:4 | F5:4 A5:2 F5:2 D5:8 | B5:4 G5:2 D5:2 B4:4 D5:4 | " +
        "Bb5:4 G5:2 D5:2 Bb4:4 D5:4 | A5:4 F5:2 D5:2 A4:4 F5:4 | E5:2 F5:2 G5:2 A5:2 Bb5:2 A5:2 G5:2 E5:2 | D5:12 r:4") },
      { name: "drone", inst: "pad", level: 0.8, rev: 0.4, line: lineOf("D2:64 A1:64") },
      { name: "bell", inst: "bell", level: 0.6, rev: 0.5, line: lineOf("D3:1 r:63 A2:1 r:63") },
      { name: "clock", inst: "rim", level: 0.25, rev: 0.3, line: hitsOf("x...x...x...x...") },
    ], form: [
      S(8, "arps clock"), S(8, "arps bass clock"), S(8, "arps bass tune drone"),
      S(8, "bass tune bell"), S(8, "arps bass tune drone bell"), S(8, "arps bass clock"),
    ] };
  } },
  vibes: { name: "🔮 crystal-ball vibes", make: () => {
    // a spooky lounge, swung: vibraphone, a walking bass, finger snaps
    const chords = ["Am", "F7", "E7", "Am", "Dm", "F7", "E7", "E7"];
    return { bpm: 96, swing8: 0.66, room: 2.5, level: 1, parts: [
      { name: "walk", inst: "bass", level: 0.55, line: lineOf(
        "A2:4 B2:4 C3:4 E3:4 | F2:4 A2:4 C3:4 Eb3:4 | E2:4 G#2:4 B2:4 D3:4 | A2:4 E2:4 A2:4 G#2:4 | " +
        "D3:4 F3:4 A3:4 F3:4 | F2:4 A2:4 C3:4 A2:4 | E2:4 F2:4 G#2:4 B2:4 | E3:4 D3:4 B2:4 G#2:4") },
      { name: "comp", inst: "vibes", level: 0.3, rev: 0.3, line: stabsOf(chords, 16, [0, 6, 10], 2, 4) },
      { name: "tune", inst: "vibes", rev: 0.3, line: lineOf(
        "E5:6 C5:2 A4:8 | Eb5:6 C5:2 A4:4 F4:4 | G#4:4 B4:4 D5:4 F5:4 | E5:12 r:4 | " +
        "F5:6 D5:2 A4:8 | A5:6 F5:2 Eb5:4 C5:4 | B4:4 D5:4 F5:4 G#5:4 | A5:12 r:4") },
      { name: "ghost", inst: "ghost", level: 0.6, rev: 0.5, line: lineOf("A4:16 | C5:16 | B4:16 | A4:16 | D5:16 | C5:16 | B4:8 G#4:8 | A4:16") },
      { name: "snaps", inst: "rim", level: 0.4, rev: 0.25, line: hitsOf("....x.......x...") },
      { name: "brushes", inst: "shaker", level: 0.8, line: hitsOf("x.o.x.o.x.o.x.o.") },
      { name: "twinkles", inst: "musicbox", level: 0.5, rev: 0.6, line: lineOf("r:6 E7:1 r:25 C7:1 r:31 A6:1 r:23 B6:1 r:39") },
    ], form: [
      S(8, "comp walk snaps"), S(8, "comp walk snaps brushes tune"), S(8, "comp walk snaps brushes ghost twinkles"),
      S(8, "comp walk snaps brushes tune ghost"), S(8, "walk snaps brushes"), S(8, "comp walk snaps brushes tune twinkles"),
    ] };
  } },
  spiders: { name: "🕷️ spider's tiptoe", make: () => {
    // plucked violins on tiptoe, plucked cellos under them
    const A = ["Em", "Em", "Am", "B7"], B = ["C", "Am", "B7", "Em"];
    return { bpm: 120, room: 2, level: 1.2, parts: [
      { name: "cellosA", inst: "fiddle", line: bassOf(A, 16, [[0, 0, 1], [4, 7, 1], [8, 0, 1], [12, 7, 1]], 3) },
      { name: "cellosB", inst: "fiddle", line: bassOf(B, 16, [[0, 0, 1], [4, 7, 1], [8, 0, 1], [12, 7, 1]], 3) },
      { name: "violinsA", inst: "fiddle", rev: 0.2, line: lineOf(
        "E5:1 r:1 F#5:1 r:1 G5:1 r:1 G#5:1 r:1 A5:2 r:2 B5:2 r:2 | B5:1 r:1 A#5:1 r:1 B5:1 r:1 G5:1 r:1 E5:4 r:4 | " +
        "A5:1 r:1 B5:1 r:1 C6:1 r:1 C#6:1 r:1 D6:2 r:2 E6:2 r:2 | D#6:1 r:1 E6:1 r:1 D#6:1 r:1 C6:1 r:1 B5:4 r:4") },
      { name: "violinsB", inst: "fiddle", rev: 0.2, line: lineOf(
        "G5:2 E5:2 C5:2 E5:2 G5:2 C6:2 B5:2 G5:2 | A5:2 E5:2 C5:2 E5:2 A5:2 C6:2 B5:2 A5:2 | " +
        "B5:2 F#5:2 D#5:2 F#5:2 A5:2 F#5:2 D#5:2 B4:2 | E5:2 r:2 B4:2 r:2 E5:4 r:4") },
      { name: "harmonyA", inst: "fiddle", level: 0.4, line: stabsOf(A, 16, [2, 6, 10, 14], 1, 4) },
      { name: "harmonyB", inst: "fiddle", level: 0.4, line: stabsOf(B, 16, [2, 6, 10, 14], 1, 4) },
      { name: "tick", inst: "rim", level: 0.3, rev: 0.2, line: hitsOf("x.......x.......") },
      { name: "ghost", inst: "ghost", level: 0.7, rev: 0.5, line: lineOf("B4:32 C5:16 B4:16") },
      { name: "drips", inst: "vibes", level: 0.5, rev: 0.5, line: lineOf("r:12 B6:1 r:19 G6:1 r:17 E7:1 r:13") },
    ], form: [
      S(4, "cellosA"), S(8, "cellosA violinsA tick"), S(8, "cellosB violinsB harmonyB drips"), S(8, "cellosA violinsA harmonyA tick"),
      S(4, "tick ghost drips"), S(8, "cellosB violinsB harmonyB tick"), S(8, "cellosA violinsA harmonyA tick drips"),
    ] };
  } },
  cemetery: { name: "🌿 a peaceful garden", make: () => {
    const chords = ["F", "C", "Dm", "Bb", "F", "Bb", "C", "F"], minor = ["Dm", "Bb", "F", "C", "Dm", "Gm", "C", "F"];
    return { bpm: 66, room: 2.5, level: 1.35, parts: [
      { name: "pad", inst: "pad", rev: 0.4, line: stabsOf(chords, 16, [0], 16, 3) },
      { name: "padMinor", inst: "pad", rev: 0.4, line: stabsOf(minor, 16, [0], 16, 3) },
      { name: "box", inst: "musicbox", level: 0.7, rev: 0.35, line: arpOf(chords, 16, [0, 1, 2, 3, 4, 3, 2, 1], 2, 4) },
      { name: "boxMinor", inst: "musicbox", level: 0.7, rev: 0.35, line: arpOf(minor, 16, [0, 1, 2, 3, 4, 3, 2, 1], 2, 4) },
      { name: "fluteA", inst: "flute", level: 0.8, rev: 0.35, shift: -12, line: lineOf(
        "A5:6 G5:2 F5:8 | E5:6 F5:2 G5:8 | F5:4 E5:4 D5:4 F5:4 | D5:12 r:4 | C5:4 F5:4 A5:4 C6:4 | Bb5:6 A5:2 G5:8 | G5:4 A5:2 G5:2 E5:4 C5:4 | F5:12 r:4") },
      { name: "fluteB", inst: "flute", level: 0.8, rev: 0.35, shift: -12, line: lineOf(
        "C6:8 A5:8 | G5:8 E5:8 | F5:4 A5:4 D6:8 | D6:6 C6:2 Bb5:8 | A5:4 C6:4 F5:8 | D6:8 Bb5:8 | C6:4 Bb5:4 G5:4 E5:4 | F5:16") },
      // a third tune, in D minor, over its own chords
      { name: "fluteC", inst: "flute", level: 0.8, rev: 0.4, shift: -12, line: lineOf(
        "D6:6 C6:2 A5:8 | Bb5:6 A5:2 F5:8 | A5:4 C6:4 F6:8 | E6:6 D6:2 C6:8 | F6:4 E6:4 D6:4 A5:4 | Bb5:4 D6:4 G6:8 | E6:4 D6:4 C6:4 G5:4 | A5:12 r:4") },
      // soft vibraphone chords, wobbling slowly
      { name: "vibes", inst: "vibes", level: 0.35, rev: 0.4, line: stabsOf(chords, 16, [0], 16, 4) },
      { name: "bass", inst: "bass", level: 0.45, line: bassOf(chords, 16, [[0, 0, 16]], 2) },
      { name: "birds", inst: "bird", level: 0.6, rev: 0.4, line: lineOf("r:20 E7:1 r:43 G7:1 r:30 D7:1 r:32") },
      // wind chimes
      { name: "chimes", inst: "musicbox", level: 0.6, rev: 0.6, line: lineOf(
        "r:3 C7:1 r:9 A6:1 r:2 F6:1 r:13 G6:1 r:7 C7:1 r:21 D7:1 r:5 A6:1 r:11 F6:1 r:25 C7:1 r:23") },
    ], form: [
      S(8, "pad box"), S(8, "pad box fluteA"), S(8, "pad fluteB birds vibes"), S(8, "padMinor boxMinor fluteC"),
      S(8, "box birds chimes"), S(8, "pad box vibes bass fluteA", 2), S(8, "padMinor boxMinor fluteC chimes"),
      S(8, "pad box fluteB birds bass"), S(4, "box chimes"),
    ] };
  } },
  bongobreak: { name: "🥁 bongo breakdown", make: () => funkTune({
    // E dorian at 112: bongos open it and break it down halfway; the bass, the kit, the organ and the guitars each get a turn
    bpm: 112, swing: 0.08, echo: 0.12, chords: ["Em7", "Em7", "A7", "A7"],
    bass: "E2:2 r:1 E2:1 r:2 E3:1 r:1 D3:2 B2:2 G2:2 A2:2 | E2:2 r:2 G2:1 r:1 A2:2 r:2 B2:1 r:1 D3:2 E3:2 | A2:2 r:1 A2:1 r:2 A3:1 r:1 G3:2 E3:2 C#3:2 D3:2 | A2:2 r:2 C#3:1 r:1 D3:2 r:2 E3:1 r:1 G3:2 B2:2",
    bassSolo: "E2:1 r:1 E3:1 E2:1 r:2 E2:1 G2:1 r:1 A2:1 B2:2 D3:1 E3:1 D3:1 B2:1 | E2:2 r:1 E3:1 r:1 E2:1 G2:2 A2:1 r:1 B2:1 D3:2 E3:1 G3:2 | A2:1 r:1 A3:1 A2:1 r:2 A2:1 C#3:1 r:1 D3:1 E3:2 G3:1 A3:1 G3:1 E3:1 | A2:2 r:1 A3:1 r:1 A2:1 C#3:2 D3:1 r:1 E3:1 G3:2 E3:1 D3:2 | E2:1 r:1 E3:1 E2:1 r:2 E2:1 G2:1 r:1 A2:1 B2:2 D3:1 E3:1 D3:1 B2:1 | E2:1 E2:1 r:1 E2:1 G2:1 r:1 A2:1 r:1 B2:1 B2:1 r:1 D3:1 E3:2 D3:1 B2:1 | A2:1 A2:1 r:1 A2:1 C#3:1 r:1 D3:1 r:1 E3:1 E3:1 r:1 G3:1 A3:2 G3:1 E3:1 | D3:2 C#3:2 B2:2 A2:2 G2:2 F#2:2 E2:4",
    theme: "E4:3 G4:1 A4:2 B4:6 r:4 | D5:2 B4:2 A4:2 G4:2 A4:8 | C#5:3 E5:1 D5:2 C#5:2 A4:4 r:4 | G4:2 A4:2 B4:2 D5:2 E5:8 | G5:3 F#5:1 E5:2 D5:6 r:4 | B4:2 D5:2 E5:2 G5:2 F#5:4 E5:4 | E5:2 C#5:2 A4:2 C#5:2 E5:4 G5:2 F#5:2 | E5:12 r:4",
    organSolo: "B4:2 D5:2 E5:4 r:2 D5:1 E5:1 G5:2 E5:2 | D5:2 B4:2 A4:2 B4:2 r:4 E4:2 G4:2 | A4:2 C#5:2 E5:4 r:2 F#5:1 G5:1 A5:2 G5:2 | E5:3 D5:1 C#5:2 A4:2 r:4 E5:2 G5:2 | B5:4 A5:2 G5:2 E5:2 D5:2 E5:4 | G5:1 A5:1 G5:1 E5:1 D5:2 B4:2 D5:4 r:4 | C#5:2 E5:2 A5:4 G5:2 E5:2 C#5:2 A4:2 | B4:2 D5:2 E5:8 r:4",
    gtrSolo: "E5:1 G5:1 A5:1 B5:1 A5:2 G5:2 E5:2 D5:2 E5:4 | r:2 B4:1 D5:1 E5:2 G5:2 A5:1 G5:1 E5:2 D5:4 | A5:1 B5:1 C#6:1 E6:1 C#6:2 B5:2 A5:2 G5:2 E5:4 | r:2 E5:1 F#5:1 G5:2 A5:2 B5:4 A5:4 | E6:4 D6:2 B5:2 D6:2 B5:2 A5:4 | G5:1 A5:1 G5:1 E5:1 G5:1 E5:1 D5:2 E5:8 | C#5:2 E5:2 G5:2 A5:2 C#6:2 B5:2 A5:2 G5:2 | E5:16",
    kick: "x.....x.x..x.... x.....x...x..x..", snare: "....x..o.o..x..o ....x..o.o..x.o.",
    hat: "xoxoxoxoxoxoxoxo xoxoxoxoxoxoxoxo", open: "................ ..............x.",
    bKick: "x.x.....x.x..x.. x.x...x...x.....", bSnare: "....x..o..o.x..o .o..x..o.o..x.xx",
    bHat: "x.x.xxx.x.x.xxx. x.x.xxx.x.x.x.x.",
    // (the high bongo ends every other bar in triplets; the low one plays
    // three over the last two beats of it, against the kick's two)
    hi: "x..x..x.x..x.xx. x..x..x.x..x(xxx)", lo: "..x...x...x...x. ..x...x.8(xxx)",
    // the solo: the low bongo keeps the beat while the high one plays
    // three to it, then three over two beats, then six to a beat
    soloHi: "x.xx.x.xx.x.xxxx (xox)(xox)(xox)(xox) 8(xxx)8(xxx) (xxxxxx)(xoxoxo)8(xxx)",
    soloLo: "..x...x...x...x. x...x...x...x... x...x...x...x... x...x...x.x.x...",
    rhythm: [[0, 1], [3, 1], [6, 2], [10, 1], [12, 1], [14, 1]], rhythmSolo: [[0, 1], [2, 1], [3, 1], [6, 1], [7, 1], [8, 1], [10, 1], [11, 1], [14, 1], [15, 1]],
    organ: [[0, 7], [10, 5]],
    // the high bongo rising and falling once a bar; in the solo, the high
    // one talking, up and down every two beats, the low one climbing over
    // eight bars
    lfos: { A: { beats: 4, shape: "triangle", depth: 2 }, B: { beats: 2, shape: "sine", depth: 4 }, C: { beats: 32, shape: "saw", depth: 3 } },
    lfo: { hi: "A", soloHi: "B", soloLo: "C" },
    plan: [["intro", 4], ["kitIn", 4], ["groove", 8], ["theme", 8], ["bassSolo", 8], ["bongoSolo", 8], ["drumBreak", 8], ["organSolo", 8], ["rhythmSolo", 4], ["gtrSolo", 8], ["theme", 8, 2], ["outro", 4], ["intro", 4]],
  }) },
  canyon: { name: "🏜️ canyon cruise", make: () => funkTune({
    // D minor at 104, wide open: a twangy guitar with a long echo over a rolling bass; bongos and a dusty breakbeat
    bpm: 104, swing: 0, echo: 0.25, chords: ["Dm", "Dm", "Bb", "C"],
    bass: "D2:2 r:1 D2:1 A2:2 r:1 D3:1 r:2 C3:2 A2:2 F2:2 | D2:2 r:2 F2:2 G2:2 A2:2 r:2 C3:2 A2:2 | Bb1:2 r:1 Bb1:1 F2:2 r:1 Bb2:1 r:2 A2:2 F2:2 D2:2 | C2:2 r:1 C2:1 G2:2 r:1 C3:1 r:2 E2:2 G2:4/3 E2:4/3 C#2:4/3",
    bassSolo: "D2:1 r:1 D3:1 D2:1 r:1 D2:1 F2:1 G2:1 A2:2 C3:1 D3:1 r:1 C3:1 A2:2 | D2:2 D3:1 r:1 C3:2 A2:2 G2:1 A2:1 F2:2 D2:4 | Bb1:1 r:1 Bb2:1 Bb1:1 r:1 Bb1:1 D2:1 F2:1 G2:2 A2:1 Bb2:1 r:1 A2:1 F2:2 | C2:1 r:1 C3:1 C2:1 r:1 C2:1 E2:1 G2:1 Bb2:2 A2:1 G2:1 E2:2 C#2:2 | D2:1 r:1 D3:1 D2:1 r:1 D2:1 F2:1 G2:1 A2:2 C3:1 D3:1 r:1 C3:1 A2:2 | D2:1 D2:1 r:1 D3:1 r:1 C3:1 A2:1 r:1 G2:1 A2:1 r:1 C3:1 D3:2 A2:2 | Bb1:2 F2:1 Bb2:1 r:2 C2:2 G2:1 C3:1 r:2 E2:2 G2:2 | A2:2 G2:2 F2:2 E2:2 D2:8",
    theme: "A4:3 D5:1 F5:4 E5:2 D5:2 A4:4 | C5:2 D5:2 E5:2 F5:2 E5:4 r:4 | D5:3 F5:1 Bb5:4 A5:2 F5:2 D5:4 | E5:2 G5:2 C6:4 Bb5:2 A5:2 G5:4 | A5:6 G5:2 F5:8/3 E5:8/3 D5:8/3 | D5:2 E5:2 F5:2 A5:2 G5:4 F5:4 | Bb4:3 D5:1 F5:4 E5:3 C5:1 E5:4 | D5:12 r:4",
    organSolo: "D5:2 F5:2 A5:3 G5:1 F5:2 D5:2 r:4 | C5:1 D5:1 F5:2 G5:2 A5:2 C6:2 A5:2 r:4 | Bb5:4 A5:2 F5:2 D5:4 F5:2 A5:2 | G5:2 E5:2 C5:2 E5:2 G5:4 r:4 | A5:1 Bb5:1 A5:1 F5:1 D5:2 F5:2 A5:4 D6:4 | C6:2 A5:2 G5:2 F5:2 E5:4 D5:4 | F5:2 D5:2 Bb4:2 D5:2 E5:2 G5:2 C#5:4 | D5:12 r:4",
    gtrSolo: "D5:1 F5:1 G5:1 A5:1 C6:2 A5:2 G5:2 F5:2 D5:4 | r:2 A4:1 C5:1 D5:2 F5:2 E5:1 D5:1 C5:2 D5:4 | F5:1 G5:1 A5:1 Bb5:1 D6:4 Bb5:2 A5:2 F5:4 | E5:1 G5:1 C6:2 Bb5:2 G5:2 E5:4 C5:4 | A5:4 C6:2 D6:2 C6:2 A5:2 G5:4 | F5:1 G5:1 F5:1 D5:1 F5:1 D5:1 C5:2 D5:8 | Bb4:2 D5:2 F5:2 Bb5:2 C6:2 E5:2 G5:2 C#6:2 | D6:16",
    kick: "x.......x.x..... x..x....x.......", snare: "....x.......x... ....x..o....x..o",
    hat: "x.x.x.x.x.x.x.x. x.x.x.x.x.x.(xxx)", open: "......x......... ..............x.",
    bKick: "x..x..x...x.x... x..x..x...x..x..", bSnare: "....x.o.....x.oo .o..x..o.o..x..x",
    bHat: "xoxoxoxoxoxoxoxo xoxoxoxoxoxoxoxo",
    // (the low bongo every six steps, a dotted quarter: three of its
    // beats for every two of the band's, coming round to the bar's start
    // every three bars)
    hi: "x.x..x.xx.x..x.x", lo: "x..o..",
    soloHi: "x.x.x.xxx.x.x.xx x..xx..xx..xx.x. x.x.x.xxx.x.x.xx xxx.xxx.xxx.xxxx",
    soloLo: ".x...x...x...x.. ..x...x...x...x. .x...x...x...x.. x...x...x.......",
    rhythm: [[0, 2], [4, 1], [6, 1], [10, 2], [14, 1]], rhythmSolo: [[0, 1], [1, 1], [4, 1], [6, 1], [7, 1], [10, 1], [12, 1], [13, 1], [15, 1]],
    organ: [[0, 8], [8, 8]],
    // the low bongo, a hit every six steps, round its LFO in six beats:
    // four hits to a turn, a little tune of four notes coming round every
    // bar and a half; the high one drifting over two bars
    lfos: { A: { beats: 6, shape: "triangle", depth: 5 }, B: { beats: 8, shape: "sine", depth: 2 } },
    lfo: { lo: "A", hi: "B", soloHi: "B" },
    plan: [["intro", 4], ["kitIn", 4], ["groove", 8], ["theme", 8], ["bassSolo", 8], ["bongoSolo", 4], ["drumBreak", 8], ["organSolo", 8], ["rhythmSolo", 4], ["gtrSolo", 8], ["theme", 8, 2], ["outro", 4]],
  }) },
  skatepark: { name: "🛹 skatepark strut", make: () => funkTune({
    // A minor at 96, laid back and swung, a hip-hop beat: a fat bass, ghost notes on the snare, an organ that takes its time
    bpm: 96, swing: 0.18, echo: 0.15, chords: ["Am7", "Am7", "Dm7", "E7"],
    bass: "A1:3 r:1 A2:1 r:2 C2:1 r:2 E2:2 G2:2 A2:2 | A1:2 r:4 E2:2 r:2 G2:1 A2:1 r:2 C3:2 | D2:3 r:1 D3:1 r:2 F2:1 r:2 A2:2 C3:2 D3:2 | E2:3 r:1 E3:1 r:2 G#2:1 r:2 B2:2 D3:2 G#2:2",
    bassSolo: "A1:2 A2:1 r:1 G2:1 E2:1 r:2 C2:1 D2:1 E2:2 G2:1 A2:1 C3:2 | A2:1 G2:1 E2:1 r:1 A1:4 r:2 E2:1 G2:1 A2:2 C3:2 | D2:2 D3:1 r:1 C3:1 A2:1 r:2 F2:1 G2:1 A2:2 C3:1 D3:1 F3:2 | E2:2 E3:1 r:1 D3:1 B2:1 r:2 G#2:1 A2:1 B2:2 D3:1 E3:1 G#3:2 | A1:2 A2:1 r:1 G2:1 E2:1 r:2 C2:1 D2:1 E2:2 G2:1 A2:1 C3:2 | A1:1 A1:1 r:2 C2:1 D2:1 E2:1 r:1 G2:2 A2:2 E2:4 | D2:2 F2:1 A2:1 r:2 E2:2 G#2:1 B2:1 r:2 D3:2 E3:2 | E3:2 D3:2 C3:2 B2:2 A2:8",
    theme: "E5:2 r:2 E5:1 D5:1 C5:2 A4:4 r:4 | G4:8/3 A4:8/3 C5:8/3 D5:2 E5:4 D5:2 | F5:2 r:2 F5:1 E5:1 D5:2 A4:4 r:4 | G#4:2 B4:2 D5:2 E5:2 G#5:4 E5:4 | A5:3 G5:1 E5:2 D5:2 C5:4 r:4 | D5:2 E5:2 G5:2 A5:2 G5:4 E5:4 | F5:2 D5:2 A4:2 D5:2 E5:2 G#5:2 B5:4 | A5:12 r:4",
    organSolo: "A4:2 C5:2 E5:2 G5:2 A5:4 G5:2 E5:2 | D5:2 C5:2 A4:4 r:4 E5:1 G5:1 A5:2 | D5:2 F5:2 A5:2 C6:2 D6:4 C6:2 A5:2 | G#5:2 B5:2 E6:4 D6:2 B5:2 G#5:4 | C6:3 B5:1 A5:2 G5:2 E5:4 r:4 | G5:1 A5:1 G5:1 E5:1 D5:2 C5:2 A4:4 r:4 | D5:2 F5:2 A5:2 F5:2 E5:2 G#5:2 B5:2 D6:2 | C6:2 B5:2 A5:8 r:4",
    gtrSolo: "A4:1 C5:1 D5:1 E5:1 G5:2 E5:2 D5:2 C5:2 A4:4 | r:2 E5:1 G5:1 A5:2 C6:2 B5:1 A5:1 G5:2 E5:4 | D5:1 F5:1 A5:1 C6:1 D6:4 C6:2 A5:2 F5:4 | E5:1 G#5:1 B5:2 D6:2 B5:2 G#5:4 E5:4 | A5:4 C6:2 E6:2 D6:2 C6:2 A5:4 | G5:1 A5:1 G5:1 E5:1 G5:1 E5:1 D5:2 E5:8 | F5:2 A5:2 D6:2 C6:2 B5:2 G#5:2 E5:2 D5:2 | A4:16",
    kick: "x......x..x..... x.x.......x.....", snare: "....x.......x... ....x..o....x.o.",
    hat: "x.x.x.x.x.x.x.x. x.x.x.x.(xxx)(xoxoxo)", open: "................ ..........x.....",
    bKick: "x..x......x.x... x..x...x..x.....", bSnare: "....x..o....x... .o..x..o..o.x.x.",
    bHat: "x.xxx.x.x.xxx.x. x.x.x.xxx.x.x.x.",
    hi: "x.x...x.x.x...x.", lo: "....x.......x..x",
    soloHi: "x.x.xx.x.x.x.xx. x..x.x..x.x.x.xx x.x.xx.x.x.x.xx. x.xxx.x.xxxxx.x.",
    soloLo: "...x.......x.... ..x...x...x..... ...x.......x.... x.x.x...........",
    rhythm: [[0, 1], [2, 1], [7, 2], [10, 1], [12, 2]], rhythmSolo: [[0, 1], [2, 1], [3, 1], [5, 1], [7, 1], [8, 1], [10, 1], [12, 1], [13, 1], [15, 1]],
    // (its second half three chords over two beats)
    organ: [[0, 6], [8, 8 / 3], [8 + 8 / 3, 8 / 3], [8 + 16 / 3, 8 / 3]],
    // the high bongo round its LFO every three beats, against the bar's
    // four; in the solo, a slow climb over four bars
    lfos: { A: { beats: 3, shape: "sine", depth: 3 }, B: { beats: 16, shape: "saw", depth: 5 } },
    lfo: { hi: "A", soloHi: "B" },
    plan: [["intro", 4], ["kitIn", 4], ["groove", 8], ["theme", 8], ["bassSolo", 8], ["bongoSolo", 4], ["drumBreak", 4], ["organSolo", 8], ["rhythmSolo", 4], ["gtrSolo", 8], ["theme", 8, 2], ["outro", 4]],
  }) },
};
// each background's own tune
const SCENE_TUNE = { hut: "forest", hutroom: "cushions", hutbed: "lullaby", hutpotion: "healerwaltz", hutcenter: "lullaby", forest: "forest", underwater: "underwater", deepsea: "underwater", beach: "beach", sea: "beach", witch: "witch", cemetery: "cemetery",
                     lake: "duckdash", house: "housechase", treehouse: "treechase", treeroom: "treechase", treetop: "treechase", livingroom: "cushions",
                     witchroom: "witch", clearing: "forest", mountains: "forest", waterfall: "steppers", campsite: "lullaby", healerroom: "healerwaltz", fairyforest: "healerwaltz",
                     tent: "lullaby", cave: "steppers", caveroom: "steppers", farmyard: "forest", desert: "canyon", woodpath: "forest", pasture: "forest", crossing: "forest", castle: "castlewaltz", castleroom: "ballroom",
                     bubbleroom: "bubbledub" };   // (ticket 290: the bubble room's own, 🫧 bubble super-dub step; Matthew: "oh there no music and no LFO?")
const TUNE_CHOICES = [["scene", "🏞️ the scene's own"], ...Object.entries(TUNES).map(([id, t]) => [id, t.name])];
// the kinds of music, each with its tunes: the music tab's first row of
// turtle steppers dub, and (ticket 263) its super-dub step: at another tempo, `down` half steps lower, each pitched part
// `ups` (by its mixing-board channel, name·inst) back up from there; a drum keeps its pitch, as transposing leaves it
// (ticket 290: and its chords, and its tunes, its sonar's ping and its bubbles, `lines`, of its own: the bubble room's)
// (ticket 301: and its versions, for M: drums of their own (kick, rim, hat, and more), a bass pattern, tunes made of their
// chords' tones (tunes: two patterns, [step, chord tone, length, octaves up] over two bars, crossOf: so they fit any chords),
// and a form of their own (a drum break); drums: { break } the break's parts, drums alone)
function steppersTune({ bpm = 74, down = 0, ups = {}, chords = ["Em", "Em", "Am", "Em", "C", "D", "Em", "Em"], lines = {}, drums = {}, bassPat = null, tunes = null, chop = null, form = null, bassInst = "bass" } = {}) {
    const T = { bpm, swing: drums.swing ?? 0.15, room: 3, echoSteps: 3, level: 0.85, parts: [
      { name: "kick", inst: "kick", level: 0.8, line: hitsOf(drums.kick || "x...x...x...x...") },
      { name: "drums", inst: "rim", level: 0.7, rev: 0.3, echo: 0.3, line: hitsOf(drums.rim || "........x.......") },
      { name: "drums", inst: "hat", level: 0.5, line: hitsOf(drums.hat || "..x...x...x...x.") },
      ...(drums.extra || []).map(([inst, pat, level = 0.45, note = 60]) => ({ name: "perc", inst, level, rev: 0.2, line: hitsOf(pat, note) })),
      ...(drums.break || []).map(([inst, pat, level = 0.6, note = 60]) => ({ name: "break", inst, level, rev: 0.25, echo: 0.3, line: hitsOf(pat, note) })),
      { name: "chop", inst: "chop", echo: 0.5, rev: 0.2, line: stabsOf(chords, 16, chop || [4, 12], 1, 4) },
      { name: "bass", inst: bassInst, line: bassOf(chords, 16, bassPat || [[0, 0, 3], [4, 0, 2], [6, 7, 2], [8, 0, 3], [12, 12, 2], [14, 10, 2]], 2) },
      // a ping, like sonar, far off in the echo
      { name: "sonar", inst: "bell", level: 0.35, rev: 0.5, echo: 0.6, line: lineOf(lines.sonar || "E6:1 r:63 B5:1 r:63") },
      { name: "tuneA", inst: "melodica", rev: 0.4, echo: 0.45, line: tunes ? crossOf(chords, 16, 32, tunes[0], 5) : lineOf(lines.tuneA ||
        "r:8 B4:2 D5:2 E5:4 | G5:4 E5:4 r:8 | r:8 C5:2 E5:2 A5:4 | G5:2 F#5:2 E5:4 r:8 | " +
        "r:4 G5:4 E5:4 C5:4 | D5:4 F#5:4 A5:8 | B5:8 G5:4 E5:4 | E5:12 r:4") },
      { name: "tuneB", inst: "melodica", rev: 0.4, echo: 0.45, line: tunes ? crossOf(chords, 16, 32, tunes[1], 5) : lineOf(lines.tuneB ||
        "E5:4 r:4 G5:4 r:4 | B5:8 r:8 | A5:4 r:4 C6:4 r:4 | B5:4 G5:4 E5:8 | E5:4 G5:4 C6:8 | D6:4 A5:4 F#5:8 | G5:4 F#5:4 E5:8 | r:16") },
      { name: "bubbles", inst: "bubble", level: 0.9, rev: 0.5, echo: 0.2, line: lineOf(lines.bubbles || "r:9 E6:1 r:21 B6:1 r:17 G6:1 r:33 D6:1 r:11 A6:1 r:32") },
    ], form: form || [
      S(8, "kick bass sonar"), S(8, "kick bass sonar drums chop"), S(8, "kick bass sonar drums perc chop tuneA bubbles"),
      S(8, "kick bass sonar chop"), S(8, "kick bass sonar drums perc chop tuneB bubbles"), S(8, "kick bass sonar drums perc chop tuneA bubbles"),
    ] };
    if (down || Object.keys(ups).length) for (const q of T.parts) if (!PERCUSSION.has(q.inst)) q.shift = -down + (ups[q.name + "·" + q.inst] || 0);
    return T;
}
// chips picks a kind, the second its tune
const TUNE_GROUPS = [
  ["beats", "🥁 classic beats", ["bongobreak", "canyon", "skatepark"]],
  ["reggae", "🌴 reggae", ["roots", "underwater", "steppers", ...Object.keys(TURTLE_DUB).map(v => "steppers" + v), "superdub", "deepdub", "bubbledub"]],   // (ticket 263: its super-dub step beside it; ticket 290: and the bubble room's)
  ["calypso", "🏖️ calypso", ["beach", "soca", "lullaby"]],
  ["house", "🏠 house", ["cushions", ..."ABCDE".split("").map(v => "cushions" + v)]],   // (ticket 301: and its five versions)
  ["groove", "🕺 groove", ["bats", "batsremix", "vibes"]],
  ["waltz", "💃 waltz", ["witch", ..."ABCDE".split("").map(v => "witchfunk" + v), "castlewaltz", "ballroom", "healerwaltz"]],
  ["chase", "🏃 chase", ["treechase", "duckdash", "housechase"]],
  ["story", "📖 storybook", ["forest", "harpsichord", "spiders", "cemetery"]],
];
const groupOfTune = id => id === "scene" ? "scene" : (TUNE_GROUPS.find(g => g[2].includes(id)) || [null])[0];
// (every tune in one kind, and only one)
for (const id of Object.keys(TUNES))
  if (TUNE_GROUPS.filter(g => g[2].includes(id)).length !== 1) console.error("the tune " + id + " is not in exactly one kind of music");
// Moving music by an interval: a tap on its chip goes to the next, from a
// half step up to the octave, then round again
const INTERVALS = [null, "half step", "whole step", "minor 3rd", "major 3rd", "4th", "tritone", "5th",
                   "minor 6th", "major 6th", "minor 7th", "major 7th", "octave"];
const nextInterval = iv => (iv || 12) % 12 + 1;

// a tune made ready to play: every part's notes in one list, by step,
// section after section of its form
const songs = {};
// (ticket 361, Matthew: "On each instrument, permit a 'dup' chip, that duplicates the instrument in the mix. The default is to
// name the new one 'X dup' for 'X' as the original. The usecase is changing a part up or down an octave, to boost that part a
// bit in the mix") A tune's dups, in its mix (saved with it): each { of: the channel copied, name: the copy's }; the song
// gains each as a part of its own, the same notes on the same instrument, playing wherever the original plays (dupOf), so
// it has its strip, its knobs and its octave presses; songOf builds again when the dups change
const dupsOf = id => (mixOf(id).dups || []).filter(d => d && d.of && d.name);
// (ticket 363, Matthew: "let's store key data in the songs going forward, and display it") Each tune's key as it sounds (its
// parts' shifts in: the deep-dub, 13 half steps down, is in E-flat minor), found once from its notes by a key-finding pass
// (Krumhansl-Kessler profiles, weighted by length, drums left out) and checked against its chords where it has them (the
// bubble super-dub's Am-F-C-G-...-E is A minor, which its notes leave close to C major). A tune made from now on says its key
// on its TUNES entry (key: "D major"), which wins over this table; tools/checks/key363.mjs checks every tune has one and
// that its notes agree. Shown on the playing line, moved with 🎵 transpose
const TUNE_KEYS = {
  forest: "G major", beach: "C major", soca: "D major", roots: "G major", underwater: "A minor", steppers: "E minor",
  superdub: "E minor", deepdub: "Eb minor", bubbledub: "A minor", lullaby: "F major", witch: "D minor", witchfunkA: "D minor",
  witchfunkB: "D minor", witchfunkC: "D minor", witchfunkD: "D minor", witchfunkE: "D minor", steppersA: "A minor", steppersB: "G major",
  steppersC: "F major", steppersD: "E major", steppersD1: "D minor", steppersD2: "D major", steppersD3: "E minor", steppersD4: "A minor", cushionsA: "D major", cushionsB: "C major", cushionsC: "E major", cushionsD: "G major",
  cushionsE: "A major", castlewaltz: "G major", ballroom: "D major", healerwaltz: "F major", bats: "C minor", treechase: "A minor",
  duckdash: "F major", cushions: "E minor", housechase: "D minor", harpsichord: "D minor", vibes: "A minor", spiders: "E minor",
  cemetery: "F major", bongobreak: "E minor", canyon: "D minor", skatepark: "A minor" };
const KEY_NAMES = ["C", "C#", "D", "Eb", "E", "F", "F#", "G", "Ab", "A", "Bb", "B"];
const tuneKey = id => (TUNES[id] && TUNES[id].key) || TUNE_KEYS[id] || null;
// a key moved n half steps: its tonic, by name, the mode kept ("A minor" up 2 is "B minor")
function keyMoved(key, n) {
  if (!key) return null;
  const [t, mode] = key.split(" "), i = KEY_NAMES.indexOf(t.replace("D#", "Eb").replace("Db", "C#").replace("Gb", "F#").replace("G#", "Ab").replace("A#", "Bb"));
  return i < 0 ? key : KEY_NAMES[(((i + Math.round(n || 0)) % 12) + 12) % 12] + " " + mode;
}
// (ticket 362, Matthew: "every channel (esp useful for dups) should offer a reflection setting. similar to transpose, but
// extended. default reflection is up/down by one octave, transposed") A channel's reflection (mix channel .reflect: { mode, n,
// follow }), made when the song is built, beyond the octave presses (which stay transpose, the default): "key", n steps up
// or down the tune's scale, staying in its key; "mirror", each note turned about the key's tonic nearest the part, kept in
// key; "back", each bar backward; "canon", n beats later; "counter", against another part (follow): each note a third or a
// sixth from the note it sounds then, in key, near where it was, moving against it where it can. Its key is the tune's as
// it sounds (tuneKey), so a note is placed by its sounding pitch (with the part's shift)
const REFLECTS = [["none", "🪞 none"], ["key", "🎼 in key"], ["mirror", "🪞 mirror"], ["back", "⏪ backward"], ["canon", "🔁 canon"], ["counter", "🎶 counter"]];
const SCALES = { major: [0, 2, 4, 5, 7, 9, 11], minor: [0, 2, 3, 5, 7, 8, 10] };
function reflectSong(id, T, events, steps) {
  const mix = mixOf(id), key = tuneKey(id), bar = T.bar || 16;
  const [ton, mode] = (key || "C major").split(" "), tonic = Math.max(0, KEY_NAMES.indexOf(ton)), sc = SCALES[mode] || SCALES.major;
  const inKey = (m, sh) => sc.includes((((m + sh - tonic) % 12) + 12) % 12);
  const snap = (m, sh) => { for (let d = 0; d < 7; d++) { if (inKey(m - d, sh)) return m - d; if (inKey(m + d, sh)) return m + d; } return m; };
  const step = (m, n, sh) => { let x = snap(m, sh), k = Math.abs(n); const dir = Math.sign(n); while (k > 0) { x += dir; if (inKey(x, sh)) k--; } return x; };
  T.parts.forEach((p, i) => {
    const R = (mix[channelKey(p)] || {}).reflect; if (!R || !R.mode || R.mode === "none" || PERCUSSION.has(p.inst)) return;
    const sh = p.shift || 0, mine = events.filter(e => e.part === i), n = Math.round(R.n || 0);
    if (R.mode === "key") for (const e of mine) e.notes = (e.notes || []).map(m => step(m, n || 2, sh));
    else if (R.mode === "mirror") { const all = mine.flatMap(e => e.notes || []), mean = all.length ? all.reduce((a, b) => a + b) / all.length : 60;
      const axis = Math.round((mean - (tonic - sh)) / 12) * 12 + (tonic - sh);
      for (const e of mine) e.notes = (e.notes || []).map(m => snap(2 * axis - m, sh)); }
    else if (R.mode === "back") for (const e of mine) { const s0 = Math.floor(e.at / bar) * bar; e.at = Math.max(s0, s0 + bar - (e.at - s0) - (e.len || 1)); }
    else if (R.mode === "canon") for (const e of mine) e.at = (e.at + 4 * Math.max(1, n || 2)) % steps;
    else if (R.mode === "counter") { const L = T.parts.findIndex(q => channelKey(q) === R.follow); if (L < 0 || L === i) return;
      const lead = events.filter(e => e.part === L), lsh = T.parts[L].shift || 0;
      let prev = null, prevLead = null;
      for (const e of mine) { const at = lead.filter(q => q.at <= e.at && e.at < q.at + (q.len || 1)), m0 = (e.notes || [])[0];
        if (!at.length || m0 === undefined) continue;
        const ln = Math.max(...at.flatMap(q => q.notes || [])) + lsh, cands = [];
        for (let c = m0 - 12; c <= m0 + 12; c++) if (inKey(c, sh) && [3, 4, 8, 9].includes((((c + sh - ln) % 12) + 12) % 12)) cands.push(c);
        if (!cands.length) continue;
        const score = c => Math.abs(c - (prev ?? m0)) + (prev !== null && prevLead !== null && Math.sign(c - prev) === Math.sign(ln - prevLead) && c !== prev ? 3 : 0);
        const c = cands.sort((a, b) => score(a) - score(b))[0]; e.notes = [c]; prev = c; prevLead = ln; } }
  });
  events.sort((a, b) => a.at - b.at);
}
function songOf(id) {
  const dk = JSON.stringify([dupsOf(id), Object.entries(mixOf(id)).filter(([, c]) => c && c.reflect && c.reflect.mode && c.reflect.mode !== "none").map(([k, c]) => [k, c.reflect])]);
  if (songs[id] !== undefined && (songs[id] === null || songs[id].dk === dk)) return songs[id];
  try {
    const T = TUNES[id].make(), bar = T.bar || 16, events = [];
    for (const d of dupsOf(id)) { const q = T.parts.find(r => channelKey(r) === d.of); if (q) T.parts.push({ ...q, name: d.name, dupOf: q.dupOf || q.name }); }
    // (without a form: every part all the time, as long as the longest)
    const form = T.form || [{ bars: Math.max(...T.parts.map(p => p.line.steps)) / bar, names: null, up: 0 }];
    let start = 0;
    for (const sec of form) {
      const len = sec.bars * bar;
      T.parts.forEach((p, part) => {
        if (sec.names && !sec.names.includes(p.dupOf || p.name)) return;   // (a dup plays where its original does)
        for (let rep = 0; rep * p.line.steps < len; rep++)
          for (const e of p.line.events) {
            const at = rep * p.line.steps + e.at;
            if (at < len) events.push({ ...e, at: start + at, part, up: sec.up || 0 });
          }
      });
      start += len;
    }
    events.sort((a, b) => a.at - b.at);
    reflectSong(id, T, events, start);   // (ticket 362)
    return songs[id] = { T, steps: start, events, dk };
  } catch (err) {
    console.error("the tune " + id + " didn't build:", err);
    return songs[id] = null;
  }
}
// a long fading hiss: the ring of a room, for the reverb
function roomOf(A, secs) {
  const n = Math.floor(A.sampleRate * secs), b = A.createBuffer(2, n, A.sampleRate);
  for (let c = 0; c < 2; c++) { const d = b.getChannelData(c); for (let i = 0; i < n; i++) d[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / n, 3); }
  return b;
}
// a tune's way out, a channel strip for each part, as on a mixing board:
// its tone (a filter, wide open unless the mixer closes it), its level,
// where it sits from left to right, and what it sends to the room and to
// the echo (each repeat softer and duller), all into one gain. The parts'
// notes go in at the tone.
function busesOf(A, T, into) {
  const rev = A.createConvolver(); rev.buffer = roomOf(A, T.room || 2);
  const revOut = gainOf(A, into, 1); rev.connect(revOut);
  const echo = A.createDelay(8); echo.delayTime.value = (T.echoSteps || 3) * 60 / T.bpm / 4;
  const back = gainOf(A, echo, 0.38); echo.connect(filter(A, back, "lowpass", 1800)); echo.connect(gainOf(A, into, 0.8));
  const strips = T.parts.map(p => {
    // (a browser without a stereo panner plays every part in the middle)
    const pan = A.createStereoPanner ? A.createStereoPanner() : null;
    if (pan) pan.connect(into);
    const gain = gainOf(A, pan || into, p.level ?? 1), tone = filter(A, gain, "lowpass", 20000);
    const strip = { tone, gain, pan, rev: gainOf(A, rev, p.rev || 0), echo: gainOf(A, echo, p.echo || 0) };
    gain.connect(strip.rev); gain.connect(strip.echo);
    return strip;
  });
  return Object.assign(strips.map(st => st.tone), { strips, echo, back, revOut });
}
const stepSecs = T => 60 / T.bpm / 4;
// when a note starts: its step, and the swing on every other step (a
// triplet, between the steps, keeps its even spacing)
const swingOf = (T, at) => !Number.isInteger(at) ? 0 : (at % 2 ? T.swing || 0 : 0) + (at % 4 === 2 ? T.swing8 || 0 : 0);
const timeOf = (song, t0, loop, at) => t0 + (loop * song.steps + at + swingOf(song.T, at)) * stepSecs(song.T);
// (drums keep their sound when the tune is moved up or down)
const PERCUSSION = new Set(["kick", "rim", "snare", "hat", "shaker", "conga", "cowbell", "bongo", "openhat"]);
// (bend: half steps more, even for a drum: where its LFO is, see below)
function playNote(A, buses, song, ev, t, sd = stepSecs(song.T), up = 0, knobs = null, ups = null, bend = 0, wob = null) {
  const p = song.T.parts[ev.part], dur = ev.len * sd,
        move = (p.shift || 0) + bend + (PERCUSSION.has(p.inst) ? 0 : up + (ev.up || 0) + (ups ? ups[ev.part] : 0));
  for (const m of ev.notes) {
    const o = INSTRUMENTS[p.inst](A, buses[ev.part], t, m + move, dur, (ev.vel ?? 1) * (p.vel ?? 1), knobs ? knobs[ev.part] : 50);
    if (wob && o && o.detune) wobble(o, t, wob);
  }
}
// An LFO faster than a beat bends a hit all through its sound, not once at
// its start: a curve of its pitch (in cents) over the hit's 0.7 seconds,
// the wave where the tune is at each moment, 32 points a round (so at a
// 32nd of a beat, about 60 rounds a second, the wobble is a buzz).
const WOBBLE_SECS = 0.7;
function wobble(o, t, w) {
  const round = w.lfo.steps * w.sd, n = Math.min(8192, Math.max(64, Math.ceil(WOBBLE_SECS / round * 32))), c = new Float32Array(n);
  for (let i = 0; i < n; i++) c[i] = 100 * w.lfo.depth * lfoWave(w.lfo.shape, (((w.at + i / (n - 1) * WOBBLE_SECS / w.sd) / w.lfo.steps) % 1 + 1) % 1);
  o.detune.setValueCurveAtTime(c, t, WOBBLE_SECS);
}

// --- playing: one tune at a time, its notes sent a little ahead
let audio = null, master = null, heard = false, playing = null, volumeSet = null, tape = null, tapeOut = null;
function audioReady() {
  if (!audio) {
    const AC = window.AudioContext || window.webkitAudioContext;
    if (!AC) return false;
    audio = new AC();
    const limit = audio.createDynamicsCompressor(); limit.threshold.value = -10; limit.ratio.value = 8; limit.connect(audio.destination);
    tapeOut = gainOf(audio, limit, 1); tape = audio.createDelay(3); tape.delayTime.value = 0; tape.connect(tapeOut);
    master = gainOf(audio, tape, 0);
  }
  if (audio.state === "suspended" && !document.hidden) audio.resume();
  return true;
}
function startTune(id, from = 0) {
  const song = songOf(id);
  if (!song || !song.events.length) return;
  const into = gainOf(audio, master, song.T.level ?? 1);
  // its clock: step s0 at time t0, sd seconds a step (the tempo slider's);
  // the first note to send, from the step it starts at
  let i = song.events.findIndex(ev => ev.at >= from), loop = 0;
  if (i < 0) { i = 0; loop = 1; }
  playing = { id, song, into, buses: busesOf(audio, song.T, into), t0: audio.currentTime + 0.08, s0: from, sd: tunedStep(song.T), i, loop };
  const p = playing;
  applyMix(true);
  p.timer = setInterval(() => sendAhead(p), 25);
  // (ticket 133: and every frame, as frames still come when timers are held back)
  const frame = () => { if (playing === p) { sendAhead(p); requestAnimationFrame(frame); } };
  requestAnimationFrame(frame);
  sendAhead(p);
}
const tunedStep = T => stepSecs(T) * 100 / (state.music.tempo || 100);
// (ticket 133) Notes are sent to the audio clock 0.5 s ahead (0.2 s until
// 0.0.58), by a 25 ms timer, every frame, and every drawing (feedMusic): a
// drag or a busy page can hold timers back for a fifth of a second, in which
// a note came too late and was skipped, a glitch, while frames still came.
const MUSIC_AHEAD = 0.5;
function feedMusic() { if (playing && audio) sendAhead(playing); }
function sendAhead(p) {
  const until = audio.currentTime + MUSIC_AHEAD;
  for (;;) {
    const ev = p.song.events[p.i];
    const step = p.loop * p.song.steps + ev.at + swingOf(p.song.T, ev.at), t = p.t0 + (step - p.s0) * p.sd;
    if (t > until || (p.until && t > p.until)) break;
    // (a note already late, after a busy moment, is skipped, not crammed in)
    if (t > audio.currentTime - 0.02) {
      // (a drum following an LFO: the wave where the tune is, at this hit;
      // or, an LFO faster than a beat, the wave all through the hit)
      const lfo = p.lfos && p.lfos[ev.part], at = p.loop * p.song.steps + ev.at, fast = lfo && lfo.steps < 4;
      const bend = lfo && !fast ? lfo.depth * lfoWave(lfo.shape, (at / lfo.steps) % 1) : 0;
      playNote(audio, p.buses, p.song, ev, t, p.sd, (state.music.transpose || 0) + (p.song.T.shift || 0), p.knobs, p.ups, bend, fast ? { lfo, at, sd: p.sd } : null);   // (ticket 372: a tune's own shift)
      if (mixLights && mixLights.id === p.id) blink(mixLights.dots[ev.part], t - audio.currentTime);
    }
    if (++p.i >= p.song.events.length) { p.i = 0; p.loop++; }
  }
}
function stopTune() {
  const p = playing; playing = null;
  clearInterval(p.timer);
  p.into.gain.setTargetAtTime(0, audio.currentTime, 0.12);
  setTimeout(() => p.into.disconnect(), 1500);
}
// --- the mixing board: each part of a tune a channel (by its name and
// instrument), with mute, solo, a level (-10 to +10 dB), sends to the
// echo and the room, and a tone; and for the whole tune, the echo's
// repeats and time and the room's level. Kept for each tune, with the friend.
const channelKey = p => p.name + "·" + p.inst;
// (ticket 263: a tune's own mix to start from, TUNES[id].mix, till the player changes it: then theirs, from it)
const mixOf = id => (state.music.mix || {})[id] || (TUNES[id] && TUNES[id].mix) || {};
function chan(id, key) {
  const mix = state.music.mix || (state.music.mix = {}), t = mix[id] || (mix[id] = structuredClone((TUNES[id] && TUNES[id].mix) || {}));
  return t[key] || (t[key] = {});
}
// --- the LFOs: seven waves, A to G, that a drum's pitch can follow,
// and the friend's parts' see-through-ness, and the fairy house's, and
// things in the scenes. D to G are the scenes' own to start with (D the
// quick, for things that peck, flicker, bob and wag; E the slow, for things
// that sway, breathe, drift and glow; F and G under a beat, for wings: F a
// wingbeat that looks like one, G one so fast it blurs), but only to start
// with: anything can follow any of them, and they can be set like any other.
// (Seven, with the whole tune's strip first, sit evenly on the mixing board.)
// (a drum with a pitch knob: its LFO chip picks one, or none). Each keeps
// time with the tune, going once round in so many beats, from a 32nd of a
// beat to eight bars, from the tune's start; in a shape (triangle, saw or
// sine); so far up and down (its depth, in half steps). A hit takes the
// wave's value at its moment; or, for an LFO faster than a beat, follows
// the wave through its sound (see sendAhead). Set on the mixing board, kept for each tune; a tune
// may set its own to start with (T.lfos, and a part's lfo).
const LFO_NAMES = ["A", "B", "C", "D", "E", "F", "G"];
// (every whole number of beats from 2 to 32, eight bars; below a beat,
// the same turned over: 1/2 to 1/32 of a beat, so one beat is the knob's middle)
const LFO_LONG = [...Array(31)].map((_, i) => i + 2);
const LFO_BEATS = [...LFO_LONG].reverse().map(b => 1 / b).concat([1], LFO_LONG);
// a length's place on the knob: its own, or the nearest by ratio
const lfoBeatsAt = b => { let best = 0; LFO_BEATS.forEach((x, i) => { if (Math.abs(Math.log(x / b)) < Math.abs(Math.log(LFO_BEATS[best] / b))) best = i; }); return best; };
const LFO_SHAPES = ["triangle", "saw", "sine"], LFO_SHAPE_NAME = { triangle: "△ triangle", saw: "◿ saw", sine: "∿ sine" };
const LFO_DEFAULT = { A: { beats: 4, shape: "triangle", depth: 3 }, B: { beats: 2, shape: "sine", depth: 4 },
                      C: { beats: 8, shape: "saw", depth: 3 }, D: { beats: 1, shape: "sine", depth: 4 }, E: { beats: 16, shape: "sine", depth: 4 },
                      F: { beats: 1 / 4, shape: "sine", depth: 6 }, G: { beats: 1 / 16, shape: "sine", depth: 6 } };
const beatsName = b => b < 1 ? `1/${Math.round(1 / b)} beat` : b % 4 ? `${b} beats`.replace(/^1 beats/, "1 beat") : `${b / 4} bar${b === 4 ? "" : "s"}`;
const lfoOf = (id, T, L) => ({ ...LFO_DEFAULT[L], ...((T.lfos || {})[L] || {}), ...(mixOf(id)["lfo" + L] || {}) });
// the wave, from -1 to 1, a share u of the way round (a triangle and a
// sine start in the middle, going up; a saw climbs from the bottom)
const lfoWave = (shape, u) => shape === "sine" ? Math.sin(2 * Math.PI * u) : shape === "saw" ? 2 * u - 1
  : u < 0.25 ? 4 * u : u < 0.75 ? 2 - 4 * u : 4 * u - 4;
// the LFO a part follows: the mix's choice ("none" too), or the tune's
const partLfo = (id, q) => { const c = (mixOf(id)[channelKey(q)] || {}).lfo, L = c === "none" ? null : c || q.lfo || null; return LFO_NAMES.includes(L) ? L : null; };
// a part of the friend following an LFO: how long one round takes (in
// seconds), where in it we are now (from the music's own clock while it
// plays, so it keeps the beat; else at the tune's tempo), its shape, and
// how far it fades (its depth: twelve half steps' worth fades it right out)
function visLfo(g) { return lfoNow(state.visible["lfo_" + g]); }
// An LFO (A to G) as it is now: its length in seconds, how far into its
// wave it is (0 to 1), the wave's shape, and its depth as 0 to 1; in time
// with the music while it plays (else at the tune's tempo). Null for none.
function lfoNow(L) {
  if (!LFO_NAMES.includes(L)) return null;
  const id = (playing && playing.id) || tuneOf(), song = id ? songOf(id) : null;
  const T = song ? song.T : { bpm: 100, lfos: {} }, o = song ? lfoOf(id, T, L) : LFO_DEFAULT[L];
  const sd = playing ? playing.sd : tunedStep(T), steps = o.beats * 4;
  // (ticket 274) where the LFOs are (a walk's step, treading water, the scene's swaying): the picture's clock unless a
  // tune sounds, and held still, the picture's clock whatever plays
  const nowStep = stillAt === null && playing && typeof audio !== "undefined" && audio ? playing.s0 + (audio.currentTime - playing.t0) / playing.sd : picClock() / sd;
  return { dur: steps * sd, phase: ((nowStep / steps) % 1 + 1) % 1, shape: o.shape, amp: Math.min(1, o.depth / 12) };
}
// the parts that can follow one: drums tuned by their knob
const PITCHED_DRUMS = new Set(["bongo", "conga"]);
// a channel moved up or down: presses (-3 to 3) times its interval (half steps)
const chanUp = c => (c.upN || 0) * (c.upIv || 12);
const fxOf = (id, T) => ({ repeats: 38, time: T.echoSteps || 3, room: 100, ...(mixOf(id).fx || {}) });
// the tone slider: a filter from dark (150 Hz) to wide open
const toneHz = v => 150 * Math.pow(20000 / 150, v / 100);
function applyMix(instant = false) {
  const p = playing;
  if (!p) return;
  const { song, buses } = p, mix = mixOf(p.id), now = audio.currentTime;
  const to = (param, v) => instant ? param.setValueAtTime(v, now) : param.setTargetAtTime(v, now, 0.03);
  const solo = song.T.parts.some(q => (mix[channelKey(q)] || {}).solo);
  // each instrument's own knob, for its notes from now on, and its own move
  // up or down: so many times an interval
  p.knobs = song.T.parts.map(q => (mix[channelKey(q)] || {}).knob ?? 50);
  p.ups = song.T.parts.map(q => chanUp(mix[channelKey(q)] || {}));
  // and the LFO each drum follows, if any: its steps round, shape, depth
  p.lfos = song.T.parts.map(q => {
    const L = PITCHED_DRUMS.has(q.inst) && partLfo(p.id, q), o = L && lfoOf(p.id, song.T, L);
    return o ? { steps: o.beats * 4, shape: o.shape, depth: o.depth } : null;
  });
  song.T.parts.forEach((q, i) => {
    const c = mix[channelKey(q)] || {}, st = buses.strips[i], heard = !c.mute && (!solo || c.solo);
    to(st.gain.gain, heard ? (q.level ?? 1) * Math.pow(10, (c.level || 0) / 20) : 0);
    to(st.rev.gain, (c.rev ?? (q.rev || 0) * 100) / 100);
    // (a part thrown to the echo sends all of itself there while it is)
    to(st.echo.gain, throwing.has(i) || throwing.has("all") ? 1 : (c.echo ?? (q.echo || 0) * 100) / 100);
    to(st.tone.frequency, toneHz(c.tone ?? 100));
    if (st.pan) to(st.pan.pan, (c.pan || 0) / 10);
  });
  const fx = fxOf(p.id, song.T);
  to(buses.back.gain, fx.repeats / 100);
  to(buses.echo.delayTime, fx.time * p.sd);
  to(buses.revOut.gain, fx.room / 100);
}
// What in each scene moves with its quick LFO and its slow one (sceneLfo), as
// the mixing board names them; a castle room by its style
const TORCH_LFO_THINGS = { fast: "🔥 the torches' flames", slow: "🔆 the torches' glow" };
const SCENE_LFO_THINGS = {
  imagination: { fast: "🗺️ the map's dotted paths", slow: "✨ the room's glow" },
  bubbleroom: { slow: "🫧 the foam, when it moves" },
  forest: { fast: "🦋 the butterflies bobbing", wings: "🦋 the butterflies' wings", slow: "☀️ the sunbeams" },
  underwater: { fast: "🐟 the fishes' tails", slow: "🌿 the seaweed swaying, ✨ the light, 🦈 the sharks bobbing" },   // (ticket 370)
  deepsea: { fast: "🐟 the little fish darting", slow: "✨ the light shimmering, 🌿 the kelp swaying" },
  beach: { fast: "🚩 the sand castles' flags", slow: "🌴 the palm fronds swaying" },
  witch: { fast: "🔥 the witch's cauldron fire", slow: "🦇 the bats gliding" },
  cemetery: { fast: "🦋 the moths bobbing", wings: "🦋 the moths' wings", slow: "☁️ the clouds drifting" },
  lake: { fast: "☀️ the sun's rays, 🦆 the ducks bobbing", slow: "☁️ the clouds drifting" },
  house: { fast: "🦋 the butterflies bobbing", wings: "🦋 the butterflies' wings", slow: "☁️ the clouds drifting" },
  treehouse: { fast: "🦋 the butterflies bobbing", wings: "🦋 the butterflies' wings", slow: "🪢 the swing, ☁️ the clouds" },
  livingroom: { fast: "🕰️ the clock's pendulum", slow: "💡 the lamp's glow" },
  treeroom: { fast: "🍃 the leaves outside the window", slow: "🌳 the limbs outside the window swaying" },
  treetop: { fast: "🍃 the leaves outside the windows, 🐿️ the squirrel peeking", slow: "🌳 the limbs outside the windows swaying, 🏮 the lanterns' glow, ☀️ the light through the thatch" },   // (ticket 516)
  witchroom: { fast: "🔥 the cauldron fire", slow: "🐦‍⬛ the crows' cages swinging" },
  healerroom: { fast: "🔥 the fire under the pot", slow: "🌿 the healer swaying" },
  hut: { fast: "🦋 the butterflies bobbing", wings: "🦋 the butterflies' wings", slow: "💨 the smoke, ☁️ the clouds" },
  hutroom: { fast: "🎐 the wind chime", slow: "🏮 the lantern's glow" },
  hutbed: { fast: "🕯️ the candle's flame, ⭐ the stars", slow: "🪶 the dreamcatcher swaying" },
  hutpotion: { fast: "🔥 the fire, 🫧 the potion bubbling", slow: "🧪 the bottles glowing, 🌿 the herbs swaying" },
  hutcenter: { fast: "🔥 the fire", slow: "💨 the smoke, ☀️ the light from the smoke hole, 🏮 the lanterns" },
  fairyforest: { fast: "✨ the fairy lights, ☀️ the sun's rays, ⭐ the stars", slow: "🏮 the lanterns swinging, ☁️ the clouds" },
  clearing: { fast: "🦋 the butterflies bobbing", wings: "🦋 the butterflies' wings", slow: "☁️ the clouds drifting" },
  mountains: { fast: "🐦 the birds' wings", slow: "☁️ the clouds drifting" },
  waterfall: { fast: "🦜 the parrots' wings", slow: "🌿 the vines swaying" },
  campsite: { fast: "🔥 the campfire, ⭐ the stars", slow: "☁️ the clouds, 🔆 the fire's glow" },
  tent: { fast: "🔥 the campfire outside, ⭐ the stars at night", slow: "🏮 the tent's light" },
  caveroom: { fast: "💧 the drops falling", slow: "✨ the glow breathing" },   // (ticket 471)
  desert: { fast: "🌾 the tumbleweed bouncing, 🦎 the lizard's push-ups", slow: "🦅 the hawk circling, 💨 sand blowing off the dunes, ☁️ the clouds" },   // (ticket 501)
  farmyard: { fast: "🐔 the hens pecking, 🐓 the weather vane, 🌾 the straw blowing", slow: "☁️ the clouds, 🌬️ the windpump's wheel, 💨 the chimney's smoke" },   // (ticket 502)
  cave: { fast: "💎 the gems sparkling", slow: "🔮 the crystals glowing" },
  woodpath: { fast: "🦋 the butterflies bobbing", wings: "🦋 the butterflies' wings", slow: "☀️ the sunbeams" },
  pasture: { fast: "🐴 the horses' tails swishing, 💧 the trough's rings, 🦋 the butterflies bobbing", wings: "🦋 the butterflies' wings", slow: "☁️ the clouds drifting, 🌼 the grass and flowers swaying" },   // (ticket 503)
  crossing: { fast: "🦋 the butterflies bobbing", wings: "🦋 the butterflies' wings", slow: "☀️ the sunbeams" },
  castle: { fast: "🚩 the pennants, ☀️ the sun's rays", slow: "☁️ the clouds drifting", wings: "🐦 the birds' wings" },   // (wings: ticket 351)
  "castleroom:king": TORCH_LFO_THINGS, "castleroom:trap": TORCH_LFO_THINGS, "castleroom:dungeon1": TORCH_LFO_THINGS,
  "castleroom:dungeon2": TORCH_LFO_THINGS, "castleroom:hallway": TORCH_LFO_THINGS,
  "castleroom:greenhouse": { fast: "🦜 the glass parrots' wings", slow: "☁️ the clouds drifting" },
  "castleroom:catroom": { fast: "🔥 the cat room's fire", slow: "🎾 the cat toys swinging" },
  "castleroom:stable": { fast: "🐔 the hens pecking", slow: "✨ the dust in the sunbeam" },
  "castleroom:dining": { fast: "🕯️ the candle flames", slow: "🔥 the torches' and the fire's glow" },
  "castleroom:kitchen": { fast: "🔪 the cooks stirring and chopping, the fires", slow: "♨️ the steam, the oven's glow" },
  "castleroom:smithy": { fast: "🔨 the smith's hammer and the sparks", slow: "🔥 the forge's glow, the bellows" },
  "castleroom:spiral": TORCH_LFO_THINGS,
};
// (ticket 506: a stable's hens past its first two wander about with the slow LFO, named while there are some)
const sceneLfoThings = b => { if (b.kind !== "castleroom") return SCENE_LFO_THINGS[b.kind] || {};
  const [K, st] = roomHereNowOf(b), T = SCENE_LFO_THINGS["castleroom:" + (st ?? "king")] || {};
  return st === "stable" && K.hens[Math.max(1, Math.min(K.rooms.length, Math.round(b.castleRoom ?? 1))) - 1] > STABLE_HENS ? { ...T, slow: T.slow + ", 🐔 the hens wandering" } : T; };
// the cats in the room the friend is in, when it is a cat room; the horses, when it is a stable
const roomHereNowOf = B => { if (B.kind !== "castleroom") return [null, null];
  const K = castleOf(B), n = Math.max(1, Math.min(K.rooms.length, Math.round(B.castleRoom ?? 1))); return [K, K.rooms[n - 1]]; };
const roomHereNow = () => roomHereNowOf(state.background);
// the residents in the room the friend is in, of each kind that lives there: [R, its list]
const residentsHereNow = () => { const [K, st] = roomHereNow(); return Object.entries(RESIDENTS).filter(([, R]) => R.room === st).map(([kind, R]) => [R, K[kind]]); };
// Everything in the picture that follows an LFO now, as [the LFO, its name on
// the mixing board], each kind of follower once, here: the mixing board's
// strip for an LFO names its followers from this, and a change to an LFO
// draws the picture again when anything in it follows one. A new kind of
// follower is a line here (and, in a scene, a read of its LFO: sceneOpts).
const LFO_FOLLOWERS = [
  // (a part of the friend fading in and out)
  () => VISIBLE_PARTS.filter(g => LFO_NAMES.includes(state.visible["lfo_" + g])).map(g => [state.visible["lfo_" + g], "👻 " + g]),
  // (a fairy's wings, while she wears them and they flap with it)
  () => state.costume.kind === "fairy" && state.costume.wingFlutter !== false && LFO_NAMES.includes(state.costume.wingLfo) ? [[state.costume.wingLfo, "🧚 fairy wings"]] : [],
  // (ticket 499: a dragon's wings, while they flap with it)
  () => state.costume.kind === "dragon" && (state.costume.dragonWingsHow ?? "flap") === "flap" && LFO_NAMES.includes(state.costume.dragonWingLfo) ? [[state.costume.dragonWingLfo, "🐉 dragon wings"]] : [],
  () => state.background.kind === "fairyforest" && LFO_NAMES.includes(state.background.fairyHouseLfo) ? [[state.background.fairyHouseLfo, "🏡 fairy house"]] : [],
  // (the castle's residents in the room the friend is in)
  () => residentsHereNow().flatMap(([R, list]) => list.flatMap((one, i) => LFO_NAMES.includes(one.lfo) && (!R.followsIf || R.followsIf(one)) ? [[one.lfo, R.follows(i)]] : [])),   // (ticket 293: a throne's cat's tail, while it has one)
  // (the scene's quick things, its slow things and its wings)
  () => ["fast", "slow", "wings"].flatMap(pace => { const B = state.background, th = sceneLfoThings(B)[pace], L = th && sceneLfoName(B, pace); return L ? [[L, th]] : []; }),
];
const lfoFollowers = () => LFO_FOLLOWERS.flatMap(f => f());
// (ticket 361) a dup added or taken away: the mix's list and the copy's channel (its original's settings, not muted or soloed),
// then the tune, if playing, on from the step it is at, so the copy is heard at once
const mixRoot = id => { const mix = state.music.mix || (state.music.mix = {}); return mix[id] || (mix[id] = structuredClone((TUNES[id] && TUNES[id].mix) || {})); };
function dupAdd(id, p, key) {
  const t = mixRoot(id), names = new Set((songOf(id)?.T.parts || []).map(q => q.name));
  let name = p.name + " dup"; for (let n = 2; names.has(name); n++) name = `${p.name} dup ${n}`;
  t.dups = [...(t.dups || []), { of: key, name }];
  const { mute, solo, ...rest } = structuredClone(t[key] || {}); t[name + "·" + p.inst] = rest;
  dupRestart(id);
}
function dupRemove(id, p, key) {
  const t = mixRoot(id); t.dups = (t.dups || []).filter(d => d.name !== p.name); delete t[key];
  dupRestart(id);
}
function dupRestart(id) {
  if (!playing || playing.id !== id || !audio) return;
  const p = playing, at = p.s0 + (audio.currentTime - p.t0) / p.sd, song = songOf(id);
  stopTune(); startFrom = song && song.steps ? ((at % song.steps) + song.steps) % song.steps : 0; musicSync();
}
function mixChanged() {
  persist(); applyMix();
  // (whatever follows an LFO follows its changes at once)
  if (lfoFollowers().length) draw();
}
// The dub "throw": while a part is thrown (its strip's button held), all
// of it goes to the echo, then its send goes back to where it was set;
// thrown for a beat at least, so a quick tap is heard. "all": every part.
const throwing = new Set();
function throwButton(label, which) {
  const b = el("button", { class: "ms throw", "aria-pressed": "false" }, label);
  let since = 0, held = false;
  const start = e => {
    e.preventDefault(); held = true; since = performance.now();
    throwing.add(which); b.setAttribute("aria-pressed", "true"); applyMix();
  };
  const stop = () => {
    if (!held) return;
    held = false;
    const beat = playing ? 4 * playing.sd * 1000 : 0, left = Math.max(0, beat - (performance.now() - since));
    setTimeout(() => { if (held) return; throwing.delete(which); b.setAttribute("aria-pressed", "false"); applyMix(); }, left);
  };
  b.addEventListener("pointerdown", start);
  for (const kind of ["pointerup", "pointercancel", "pointerleave"]) b.addEventListener(kind, stop);
  // (a key throws for a beat)
  b.addEventListener("keydown", e => { if ((e.key === " " || e.key === "Enter") && !held) { start(e); stop(); } });
  return b;
}
// the board's lights: one per channel, lit as its part plays a note
let mixLights = null;
function blink(dot, after) {
  if (!dot) return;
  setTimeout(() => { dot.classList.add("on"); setTimeout(() => dot.classList.remove("on"), 110); }, Math.max(0, after * 1000));
}
const INST_ICON = { steelpan: "🥘", organ: "🎹", bass: "🎸", kick: "🥁", rim: "🥢", snare: "🥁", cowbell: "🔔", hat: "🎩",
                    shaker: "🧂", conga: "🪘", bubble: "🫧", pizz: "🎻", pluck: "🎸", chop: "🎸", melodica: "🎹", ghost: "👻",
                    bell: "🔔", pad: "☁️", musicbox: "🎶", flute: "🌬️", bird: "🐦", buzz: "🐝", wahbass: "🎸", harpsichord: "🎼",
                    vibes: "🔮", fiddle: "🎻", lead: "🎛️", zap: "⚡",
                    bassgtr: "🎸", bongo: "🪘", openhat: "🎩", tonewheel: "🎹", funkgtr: "🎸", twang: "🎸" };
// each instrument's own knob, by name (what it does is in INSTRUMENTS)
const INST_KNOB = { steelpan: "✨ ring", harpsichord: "✨ ring", fiddle: "✨ ring", pizz: "✨ ring", pluck: "✨ ring", musicbox: "✨ ring",
                    bell: "✨ ring", rim: "✨ length", snare: "✨ length", hat: "✨ length", shaker: "✨ length", cowbell: "✨ length",
                    chop: "✨ length", kick: "💥 boom", conga: "🎵 pitch", bass: "🎸 growl", buzz: "🐝 wah", wahbass: "🎸 wah", organ: "🎹 bright",
                    melodica: "〰️ vibrato", flute: "〰️ vibrato", ghost: "〰️ waver", vibes: "🌀 wobble", pad: "🌅 swell",
                    bubble: "🫧 bloop", bird: "🐦 chirp", lead: "✨ bright", zap: "⚡ fall",
                    bassgtr: "🎸 growl", bongo: "🎵 pitch", openhat: "✨ length", tonewheel: "🌀 spin", funkgtr: "🦆 wah", twang: "〰️ whammy" };
// "panA" reads "pan A", "bassSolo" "bass Solo"
const partName = n => n.replace(/([a-z])([A-Z])/g, "$1 $2");
// A channel's move up or down: minus, its interval (a tap goes to the
// next), plus; and how far it is moved now: none, or so many presses of
// its interval, up to three either way. Changing the interval keeps the
// presses: "+2 × 5th" becomes "+2 × octave".
function channelMove(get, put, changed) {
  const say = el("span", { class: "val" });
  const show = () => {
    const c = get(), n = c.upN || 0, iv = c.upIv || 12;
    say.textContent = n === 0 ? "none" : `${n > 0 ? "+" : "−"}${Math.abs(n)} × ${INTERVALS[iv]} (${n > 0 ? "+" : "−"}${Math.abs(n) * iv} half steps)`;
    ivBtn.textContent = "🎵 " + INTERVALS[iv];
  };
  const press = d => () => { const ch = put(); ch.upN = Math.max(-3, Math.min(3, (ch.upN || 0) + d)); changed(); show(); };
  const down = el("button", { class: "ms", title: "down by the interval", onclick: press(-1) }, "➖");
  const ivBtn = el("button", { class: "ms", title: "the interval: tap for the next", onclick: () => { const ch = put(); ch.upIv = nextInterval(ch.upIv); changed(); show(); } });
  const up = el("button", { class: "ms", title: "up by the interval", onclick: press(1) }, "➕");
  show();
  return el("div", { class: "knob move" }, [el("span", {}, "🎹 move"), el("div", { class: "row" }, [down, ivBtn, up]), say]);
}
// (ticket 362) a channel's reflection: its mode, its number (steps in key, beats for a canon), the part a counter follows
function channelReflect(id, p, parts, get, put, changed) {
  const R = () => get().reflect || { mode: "none", n: 0 };
  const set = o => { const ch = put(); ch.reflect = { ...R(), ...o }; changed(); show(); };
  const mode = el("select", { class: "ms", title: "how this part is reflected", onchange: e => set({ mode: e.target.value, n: e.target.value === "key" ? 2 : e.target.value === "canon" ? 2 : 0, follow: R().follow || channelKey(parts.find(q => q !== p && !PERCUSSION.has(q.inst)) || p) }) },
    REFLECTS.map(([v, l]) => el("option", { value: v }, l)));
  const num = el("input", { type: "number", class: "ms", min: -7, max: 8, step: 1, title: "steps up or down the scale; beats later for a canon", oninput: e => set({ n: Number(e.target.value) || 0 }) });
  const follow = el("select", { class: "ms", title: "the part a counter goes against", onchange: e => set({ follow: e.target.value }) },
    parts.filter(q => q !== p && !PERCUSSION.has(q.inst)).map(q => el("option", { value: channelKey(q) }, partName(q.name))));
  const show = () => { const r = R(); mode.value = r.mode || "none"; num.value = r.n ?? 0; num.hidden = !["key", "canon"].includes(r.mode); follow.hidden = r.mode !== "counter"; if (r.follow) follow.value = r.follow; };
  show();
  return el("div", { class: "knob move" }, [el("span", {}, "🪞 reflect"), el("div", { class: "row" }, [mode, num, follow])]);
}
// The board itself, for the tune of the moment; built again when the tune
// changes, or its mix does from outside (Load, Start over).
function mixerBox(box) {
  const body = el("div", { class: "mixer" });
  box.append(body);
  let shown, snap;
  const idNow = () => tuneOf() || (TUNES[state.music.track] ? state.music.track : null);
  const build = () => {
    const id = shown = idNow(), song = id && songOf(id);
    snap = JSON.stringify(mixOf(id));
    mixLights = null; throwing.clear();
    if (!song) { body.replaceChildren(el("p", { class: "now" }, "Pick a tune, and its instruments show up here to mix.")); return; }
    const local = () => { mixChanged(); snap = JSON.stringify(mixOf(id)); };
    const knob = (label, lo, hi, get, put, show) => {
      const val = el("span", { class: "val" }, show(get()));
      const input = el("input", { type: "range", min: lo, max: hi, step: 1,
                                  oninput: e => { put(Number(e.target.value)); val.textContent = show(Number(e.target.value)); local(); } });
      input.value = get();
      return el("label", { class: "knob" }, [el("span", {}, label), input, val]);
    };
    const pct = v => v + "%";
    const master = el("div", { class: "strip master" }, [
      el("div", { class: "strip-top" }, [el("span", { class: "strip-name" }, "🎚️ the whole tune")]),
      knob("🌊 echo repeats", 0, 90, () => fxOf(id, song.T).repeats, v => { chan(id, "fx").repeats = v; }, pct),
      knob("⏱️ echo time", 1, 8, () => fxOf(id, song.T).time, v => { chan(id, "fx").time = v; }, v => v + (v === 1 ? " step" : " steps")),
      knob("🏛️ room", 0, 200, () => fxOf(id, song.T).room, v => { chan(id, "fx").room = v; }, pct),
      el("div", { class: "strip-top" }, [throwButton("🌊 throw everything", "all"),
        el("button", { class: "ms", onclick: () => { if (state.music.mix) delete state.music.mix[id]; mixChanged(); build(); } }, "↺ reset the mix")]),
    ]);
    // the LFOs (seven, A to G: LFO_NAMES): each its speed, shape and depth, and what follows it
    // now (drums, and the friend's parts); with the whole tune's strip, a
    // square of four before the parts
    const lfoStrips = LFO_NAMES.map(L => {
      const o = () => lfoOf(id, song.T, L);
      const shapeBtn = el("button", { class: "ms", title: "the wave's shape: tap for the next",
        onclick: () => { chan(id, "lfo" + L).shape = LFO_SHAPES[(LFO_SHAPES.indexOf(o().shape) + 1) % LFO_SHAPES.length]; local(); shapeBtn.textContent = LFO_SHAPE_NAME[o().shape]; } },
        LFO_SHAPE_NAME[o().shape]);
      const who = song.T.parts.filter(q => PITCHED_DRUMS.has(q.inst) && partLfo(id, q) === L).map(q => "🪘 " + partName(q.name))
        // (and everything in the picture that follows it: LFO_FOLLOWERS)
        .concat(lfoFollowers().filter(([of]) => of === L).map(([, name]) => name));
      return el("div", { class: "strip lfo" }, [
        el("div", { class: "strip-top" }, [el("span", { class: "strip-name" }, `〰️ LFO ${L}`), shapeBtn]),
        knob("⏱️ round in", 0, LFO_BEATS.length - 1, () => lfoBeatsAt(o().beats), v => { chan(id, "lfo" + L).beats = LFO_BEATS[v]; }, v => beatsName(LFO_BEATS[v])),
        knob("↕️ depth", 0, 12, () => o().depth, v => { chan(id, "lfo" + L).depth = v; }, v => v === 0 ? "still" : "±" + v),
        el("p", { class: "now" }, who.length ? who.join(", ") : "nothing follows it yet"),
      ]);
    });
    const dots = [], rows = [];
    const strips = song.T.parts.map((p, i) => {
      const key = channelKey(p), c = () => mixOf(id)[key] || {};
      const dot = el("span", { class: "dot" });
      const flip = what => () => { const ch = chan(id, key); ch[what] = !ch[what]; local(); syncButtons(); };
      const mute = el("button", { class: "ms", onclick: flip("mute") }, "🔇 mute"), solo = el("button", { class: "ms", onclick: flip("solo") }, "🎧 solo");
      // (ticket 361) ⧉ dup: a copy of this channel, "X dup", starting from its settings; a dup's own ✖ takes it away
      const dup = el("button", { class: "ms", title: "a copy of this part, its own channel, to move up or down an octave", onclick: () => { dupAdd(id, p, key); local(); build(); } }, "⧉ dup");
      const undup = p.dupOf ? [el("button", { class: "ms", title: "take this copy away", onclick: () => { dupRemove(id, p, key); local(); build(); } }, "✖")] : [];
      const strip = el("div", { class: "strip" }, [
        el("div", { class: "strip-top" }, [dot, el("span", { class: "strip-name" }, [(INST_ICON[p.inst] || "🎵") + " " + partName(p.name) + " ",
          // (the instrument too, unless the name says it: "bass", "organ")
          ...(partName(p.name).toLowerCase().startsWith(p.inst) ? [] : [el("small", {}, p.inst)])]), mute, solo, throwButton("🌊 throw", i), dup, ...undup]),
        knob("🔊 level", -10, 10, () => c().level || 0, v => { chan(id, key).level = v; }, v => (v > 0 ? "+" : "") + v + " dB"),
        knob("🌊 echo", 0, 100, () => c().echo ?? Math.round((p.echo || 0) * 100), v => { chan(id, key).echo = v; }, pct),
        knob("🏛️ room", 0, 100, () => c().rev ?? Math.round((p.rev || 0) * 100), v => { chan(id, key).rev = v; }, pct),
        knob("🎛️ tone", 0, 100, () => c().tone ?? 100, v => { chan(id, key).tone = v; }, v => v === 100 ? "open" : v + "%"),
        knob("↔️ pan", -10, 10, () => c().pan || 0, v => { chan(id, key).pan = v; }, v => v === 0 ? "middle" : v < 0 ? "◀ " + -v : v + " ▶"),
        ...(INST_KNOB[p.inst] ? [knob(INST_KNOB[p.inst], 0, 100, () => c().knob ?? 50, v => { chan(id, key).knob = v; },
          (p.inst === "conga" || p.inst === "bongo") ? v => { const h = Math.round((v - 50) * 12 / 50); return h === 0 ? "usual" : (h > 0 ? "+" : "") + h; } : v => v === 50 ? "usual" : String(v))] : []),
        // a drum with a pitch: which LFO it follows (none, A to G: a tap for the next)
        ...(PITCHED_DRUMS.has(p.inst) ? [(() => {
          const now = () => partLfo(id, p) || "none";
          const b = el("button", { class: "ms", title: "the LFO this drum's pitch follows: tap for the next",
            onclick: () => { const all = ["none", ...LFO_NAMES]; chan(id, key).lfo = all[(all.indexOf(now()) + 1) % all.length]; local(); build(); } },
            now() === "none" ? "none" : "LFO " + now());
          return el("div", { class: "knob move" }, [el("span", {}, "〰️ LFO"), el("div", { class: "row" }, [b])]);
        })()] : []),
        ...(PERCUSSION.has(p.inst) ? [] : [channelMove(() => c(), () => chan(id, key), local)]),
        ...(PERCUSSION.has(p.inst) ? [] : [channelReflect(id, p, song.T.parts, () => c(), () => chan(id, key), () => { local(); dupRestart(id); })]),   // (ticket 362)
      ]);
      dots.push(dot); rows.push({ strip, mute, solo, c });
      return strip;
    });
    // a muted channel, or one left out by another's solo, is dimmed
    function syncButtons() {
      const anySolo = rows.some(r => r.c().solo);
      for (const r of rows) {
        r.mute.setAttribute("aria-pressed", !!r.c().mute); r.solo.setAttribute("aria-pressed", !!r.c().solo);
        r.strip.classList.toggle("dim", !!r.c().mute || (anySolo && !r.c().solo));
      }
    }
    syncButtons();
    body.replaceChildren(el("div", { class: "quad" }, [master, ...lfoStrips]), ...strips);
    mixLights = { id, dots };
  };
  build();
  syncs.push(() => { if (idNow() !== shown || JSON.stringify(mixOf(shown)) !== snap) build(); });
}
// --- the transport: where the tune is, and pause, play, rewind and seek
let paused = null, startFrom = 0;
// the step playing now, within the tune (it loops)
function stepNow() {
  if (playing) return Math.max(playing.s0, playing.s0 + (audio.currentTime - playing.t0) / playing.sd) % playing.song.steps;
  return paused ? paused.at : 0;
}
function pauseOrPlay(id) {
  if (paused) { startFrom = paused.at; paused = null; }
  else if (playing) { paused = { id: playing.id, at: stepNow() }; stopTune(); }
  else return;
  musicSync();
  // (the "playing" line says "paused" now, or "playing" again)
  for (const f of syncs) f();
}
function seek(id, at) {
  if (paused) { paused.at = at; return; }
  if (!playing) return;
  stopTune(); startFrom = at; musicSync();
}
// A tape stop, as when a tape machine loses its power: everything goes
// through a delay whose time grows faster and faster (half of D squared,
// over D), so the music slows and slides down in pitch together, to a
// halt; heard meanwhile: half of D of the tune. Then it pauses there, and
// the delay goes back to nothing once all is quiet.
let tapeStopping = false;
function tapeStop() {
  const p = playing;
  if (!p || tapeStopping || !tape) return;
  tapeStopping = true;
  const now = audio.currentTime, D = 1.6, n = 64, curve = new Float32Array(n);
  for (let k = 0; k < n; k++) curve[k] = (D / 2) * Math.pow(k / (n - 1), 2);
  tape.delayTime.cancelScheduledValues(now); tape.delayTime.setValueAtTime(0, now); tape.delayTime.setValueCurveAtTime(curve, now, D);
  tapeOut.gain.cancelScheduledValues(now); tapeOut.gain.setValueAtTime(1, now + D * 0.75); tapeOut.gain.linearRampToValueAtTime(0, now + D);
  const at = (stepNow() + (D / 2) / p.sd) % p.song.steps;
  p.until = now + D / 2;
  setTimeout(() => {
    if (playing === p) { paused = { id: p.id, at }; stopTune(); for (const f of syncs) f(); }
    setTimeout(() => {
      const t = audio.currentTime;
      tape.delayTime.cancelScheduledValues(t); tape.delayTime.setValueAtTime(0, t);
      tapeOut.gain.cancelScheduledValues(t); tapeOut.gain.setValueAtTime(1, t);
      tapeStopping = false;
    }, 400);
  }, D * 1000 + 50);
}
const clock = secs => Math.floor(secs / 60) + ":" + String(Math.floor(secs % 60)).padStart(2, "0");
function transportBox(box) {
  const say = el("span", { class: "where" }), when = el("span", { class: "when" });
  const playBtn = el("button", { class: "ms", onclick: () => pauseOrPlay() }), back = el("button", { class: "ms", onclick: () => seek(null, 0) }, "⏮ rewind");
  const tapeBtn = el("button", { class: "ms", onclick: () => tapeStop() }, "📼 tape stop");
  const slider = el("input", { type: "range", min: 0, max: 1, step: 1, value: 0 }), marks = el("div", { class: "marks" });
  let dragging = false, marksFor = null;
  slider.addEventListener("pointerdown", () => { dragging = true; });
  slider.addEventListener("input", () => { dragging = true; });
  slider.addEventListener("change", () => { dragging = false; seek(null, Number(slider.value)); });
  box.append(el("div", { class: "strip-top" }, [back, playBtn, tapeBtn]), el("div", { class: "pos" }, [say, when]), slider, marks);
  const song = () => { const id = playing ? playing.id : paused ? paused.id : tuneOf(); return id ? songOf(id) : null; };
  function frame() {
    if (!box.isConnected) return;
    const sg = song();
    box.classList.toggle("idle", !sg);
    if (sg) {
      const T = sg.T, bar = T.bar || 16, at = dragging ? Number(slider.value) : Math.floor(stepNow()), sd = playing ? playing.sd : tunedStep(T);
      slider.max = sg.steps;
      if (!dragging) slider.value = at;
      // the sections, as marks under the slider, the one playing lit
      if (marksFor !== sg) {
        marksFor = sg;
        marks.replaceChildren(...(T.form || [{ bars: sg.steps / bar }]).map((sec, n) => el("span", { style: `flex:${sec.bars}` }, String(n + 1))));
      }
      let start = 0, here = 0;
      (T.form || []).forEach((sec, n) => { if (at >= start) here = n; start += sec.bars * bar; });
      [...marks.children].forEach((m, n) => m.classList.toggle("on", n === here));
      say.textContent = `bar ${Math.floor(at / bar) + 1} · beat ${Math.floor((at % bar) / 4) + 1} of ${bar / 4}` +
                        (T.form ? ` · section ${here + 1} of ${T.form.length}` : "");
      when.textContent = `${clock(at * sd)} / ${clock(sg.steps * sd)}`;
    } else { say.textContent = "no tune playing"; when.textContent = ""; }
    playBtn.textContent = paused ? "▶ play" : "⏸ pause";
    playBtn.disabled = !(playing || paused);
    tapeBtn.disabled = !playing || tapeStopping;
    requestAnimationFrame(frame);
  }
  requestAnimationFrame(frame);
}
// the tune for now: the one chosen, or the background's own (none without one)
const tuneOf = () => { const m = state.music, id = m.track === "scene" ? SCENE_TUNE[state.background.kind] : m.track; return TUNES[id] ? id : null; };
function nowPlaying() {
  const id = tuneOf(), m = state.music;
  if (!id) return "No background, so no music. Pick a tune to play one anyway.";
  const song = songOf(id), bpm = song ? Math.round(song.T.bpm * (m.tempo || 100) / 100) : 0;
  const moved = m.transpose ? `, ${m.transpose > 0 ? "up" : "down"} ${Math.abs(m.transpose)} half step${Math.abs(m.transpose) === 1 ? "" : "s"}` : "";
  const key = keyMoved(tuneKey(id), m.transpose);   // (ticket 363)
  return (m.mute ? "🔇 muted: " : paused && paused.id === id ? "⏸ paused: " : "▶ playing: ") + TUNES[id].name + ` (${key ? "in " + key + ", " : ""}${bpm} beats a minute${moved})` +
         (heard || m.mute ? "" : " (tap anywhere to start the sound)");
}
// after every change: the right tune playing (or none), at the volume
function musicSync() {
  const id = tuneOf(), m = state.music, btn = document.getElementById("mute");
  btn.hidden = !id;
  btn.textContent = m.mute ? "🔇" : "🔊";
  btn.setAttribute("aria-pressed", !!m.mute);
  if (!heard) return;
  // (a paused tune stays paused until play, unless another tune is chosen)
  if (paused && id !== paused.id) paused = null;
  const want = m.mute || paused ? null : id;
  if (!want) { if (playing) stopTune(); return; }
  if (!audioReady()) return;
  const v = Math.pow(m.volume / 100, 2);
  if (v !== volumeSet) { master.gain.setTargetAtTime(v, audio.currentTime, 0.05); volumeSet = v; }
  if (playing && playing.id !== want) stopTune();
  if (!playing) { startTune(want, startFrom); startFrom = 0; }
  // a new tempo: on from the step playing now, at the new pace (the echo too)
  const sd = tunedStep(playing.song.T);
  if (playing.sd !== sd) {
    const p = playing, now = audio.currentTime;
    p.s0 += (now - p.t0) / p.sd; p.t0 = now; p.sd = sd;
    applyMix();
  }
}
// the first tap or key lets the sound start (inside it, as browsers ask)
for (const kind of ["pointerdown", "touchend", "keydown", "click"])
  document.addEventListener(kind, () => {
    if (heard && (!audio || audio.state === "running")) return;
    heard = true;
    musicSync();
    for (const f of syncs) f();
  }, true);
document.addEventListener("visibilitychange", () => {
  if (!audio) return;
  if (document.hidden) audio.suspend(); else if (playing) audio.resume();
});
document.getElementById("mute").addEventListener("click", () => set("music", "mute", !state.music.mute));
