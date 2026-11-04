#!/usr/bin/env python3
"""Fungi: a strange border for the right-hand margin, bright on black.

After Cyrus Omar's flora (github.com/cyrus-/webpage, site/flora.ml): a pure function of a seed draws an SVG for a
340 x 1000 margin, and every "random" choice is a hash of the path of keys that leads to it, so the same seed always
draws the same picture. Our own code and our own subject (Matthew, 2026-11-04: "a different (dark, high contrast)
color theme ... plants that are more ... extremely strange? Esp mushrooms over plants"): mycelium threads, lamp
mushrooms with gills, bracket fungi stacked up a stalk, coral, a stinkhorn's arms, morels, puffballs with eyes,
caps hanging from the top edge, and spores drifting up.

  python3 tools/fungi.py OUTDIR [seed ...]      writes OUTDIR/fungi-<palette>-<seed>.svg for each palette and seed
"""
import hashlib, math, os, sys

W, H = 340.0, 1000.0

# ---------- randomness: a value is a hash of its path of keys ----------

class R:
    """A point in the tree of choices. Each draw from the same point is a hash of its path and its draw number, so the
    choices are fixed by the seed and the order of the code, and the parts of one choice are independent."""
    def __init__(self, path): self.path, self.n = str(path), 0
    def __truediv__(self, k): return R(f"{self.path}/{k}")
    def unit(self):
        self.n += 1
        h = hashlib.blake2b(f"{self.path}#{self.n}".encode(), digest_size=8).digest()
        return int.from_bytes(h, "big") / 2.0 ** 64
    def range(self, lo, hi): return lo + (hi - lo) * self.unit()
    def int(self, lo, hi): return lo + int(self.unit() * (hi - lo + 1)) % (hi - lo + 1)
    def chance(self, p): return self.unit() < p
    def pick(self, xs): return xs[self.int(0, len(xs) - 1)]

# ---------- geometry ----------

def add(a, b): return (a[0] + b[0], a[1] + b[1])
def sub(a, b): return (a[0] - b[0], a[1] - b[1])
def mul(k, a): return (k * a[0], k * a[1])
def dirv(t): return (math.cos(t), math.sin(t))
def length(a): return math.hypot(a[0], a[1])
UP = -math.pi / 2

def grow(p, heading, steps, step, steer):
    """A path grown from p: steer(i, p, heading) gives each step's heading."""
    pts = [p]
    for i in range(steps):
        heading = steer(i, p, heading)
        p = add(p, mul(step, dirv(heading)))
        pts.append(p)
    return pts

def spline(pts, per=6):
    """Catmull-Rom through pts, per samples a segment."""
    out = []
    n = len(pts)
    g = lambda i: pts[max(0, min(n - 1, i))]
    for i in range(n - 1):
        p0, p1, p2, p3 = g(i - 1), g(i), g(i + 1), g(i + 2)
        for s in range(per):
            t = s / per; t2, t3 = t * t, t * t * t
            out.append(tuple(0.5 * ((2 * p1[k]) + (-p0[k] + p2[k]) * t + (2 * p0[k] - 5 * p1[k] + 4 * p2[k] - p3[k]) * t2
                                    + (-p0[k] + 3 * p1[k] - 3 * p2[k] + p3[k]) * t3) for k in (0, 1)))
    out.append(pts[-1])
    return out

def normals(pts):
    ns = []
    for i in range(len(pts)):
        a, b = pts[max(0, i - 1)], pts[min(len(pts) - 1, i + 1)]
        d = sub(b, a); l = length(d) or 1.0
        ns.append((-d[1] / l, d[0] / l))
    return ns

def ribbon(pts, width):
    """The outline of a pen whose width (a function of 0..1 along the path) varies."""
    ns = normals(pts); n = len(pts)
    t = lambda i: i / max(1, n - 1)
    left = [add(pts[i], mul(width(t(i)) / 2, ns[i])) for i in range(n)]
    right = [add(pts[i], mul(-width(t(i)) / 2, ns[i])) for i in range(n)]
    return left + right[::-1]

# ---------- SVG ----------

def num(x): s = f"{x:.1f}"; return s[:-2] if s.endswith(".0") else s
def pt(p): return f"{num(p[0])},{num(p[1])}"
def line_d(pts): return "M" + "L".join(pt(p) for p in pts)
def closed_d(pts): return line_d(pts) + "Z"
def attrs(**kv): return "".join(f' {k.replace("_", "-")}="{v}"' for k, v in kv.items() if v is not None)
def path(d, **kv): return f"<path d=\"{d}\"{attrs(**kv)}/>"
def circle(c, r, **kv): return f"<circle cx=\"{num(c[0])}\" cy=\"{num(c[1])}\" r=\"{num(r)}\"{attrs(**kv)}/>"
def ellipse(c, rx, ry, rot, **kv):
    return f"<ellipse cx=\"{num(c[0])}\" cy=\"{num(c[1])}\" rx=\"{num(rx)}\" ry=\"{num(ry)}\" transform=\"rotate({num(rot)} {pt(c)})\"{attrs(**kv)}/>"
def g(children, **kv): return f"<g{attrs(**kv)}>" + "".join(children) + "</g>"

# ---------- palettes: bright inks on black ----------

PALETTES = {
    # cyan threads, magenta and acid caps, gold gills: the reports' code colours
    "neon": dict(thread="#5fd7ff", stem="#e8f0ff", caps=["#ff4fd8", "#d7ff3f", "#b18cff", "#ff9f43", "#5fd7ff"],
                 gill="#ffd75f", spot="#ffffff", eye="#00e5a0", glow="#7fdfff", spore="#ffd75f"),
    # bioluminescent greens and teals with one violet
    "phosphor": dict(thread="#39ff9f", stem="#d9ffe9", caps=["#39ff9f", "#00d1c1", "#c6ff00", "#8a7dff", "#00ffd0"],
                     gill="#eaffd0", spot="#ffffff", eye="#ff4fd8", glow="#39ff9f", spore="#c6ff00"),
    # embers: reds, oranges and a cold violet
    "ember": dict(thread="#ff7a3d", stem="#ffe6cc", caps=["#ff3b3b", "#ff9f1c", "#ffd23f", "#c86bff", "#ff5fa2"],
                  gill="#ffe08a", spot="#fff5e0", eye="#5fd7ff", glow="#ff7a3d", spore="#ffd23f"),
}

def defs(pal):
    return ("<defs>"
            '<filter id="glow" x="-50%" y="-50%" width="200%" height="200%"><feGaussianBlur stdDeviation="3.2"/></filter>'
            '<filter id="haze" x="-50%" y="-50%" width="200%" height="200%"><feGaussianBlur stdDeviation="9"/></filter>'
            '<linearGradient id="fadeg" x1="0" y1="0" x2="0" y2="1">'
            '<stop offset="0" stop-color="#fff"/><stop offset=".86" stop-color="#fff"/><stop offset="1" stop-color="#000"/></linearGradient>'
            f'<mask id="fade"><rect width="{num(W)}" height="{num(H)}" fill="url(#fadeg)"/></mask>'
            "</defs>")

# ---------- parts ----------

def mycelium(r, pal, start, heading, length_, depth=0, width=1.4):
    """A glowing thread that wanders, then forks; returns a list of path elements."""
    steps = max(4, int(length_ / 6))
    wob = r.range(0.08, 0.22)
    pts = grow(start, heading, steps, 6, lambda i, p, h: h + wob * math.sin(i * 0.9 + r.range(0, 6)) + (r / i).range(-0.25, 0.25))
    out = [path(line_d(pts), fill="none", stroke=pal["thread"], stroke_width=num(width), stroke_linecap="round", opacity="0.85")]
    if depth < 3:
        for k in range(r.int(1, 2)):
            rk = r / f"fork{k}"
            j = rk.int(len(pts) // 3, len(pts) - 1)
            out += mycelium(rk, pal, pts[j], heading + rk.pick([-1, 1]) * rk.range(0.4, 1.1), length_ * rk.range(0.4, 0.7), depth + 1, width * 0.7)
    return out

def lamp(r, pal, base, height, lean, cap_r, color, hanging=False):
    """A lamp mushroom: a thin wavering stem, a domed cap with gills beneath, spots, and a glow."""
    sgn = -1 if hanging else 1
    bend = r.range(-0.5, 0.5)
    ctrl = [base] + [add(base, (lean * height * t + bend * 18 * math.sin(t * math.pi), -sgn * height * t)) for t in (0.33, 0.66, 1.0)]
    stem = spline(ctrl)
    top = stem[-1]
    head = math.atan2(stem[-1][1] - stem[-3][1], stem[-1][0] - stem[-3][0])
    sw = max(1.8, cap_r * 0.16)
    out = [path(closed_d(ribbon(stem, lambda t: sw * (1.25 - 0.45 * t))), fill=pal["stem"], opacity="0.92")]
    # the cap: a dome over the top, facing along the stem's heading
    rot = math.degrees(head) + 90
    dome = []
    squash = r.range(0.45, 0.85)
    for i in range(25):
        a = math.pi + math.pi * i / 24
        dome.append((cap_r * math.cos(a), cap_r * squash * math.sin(a) * 1.2))
    rim_droop = r.range(0.05, 0.3) * cap_r
    dome = [(x, y + (rim_droop if abs(x) > cap_r * 0.8 else 0)) for x, y in dome]
    tr = lambda p: add(top, (p[0] * math.cos(math.radians(rot)) - p[1] * math.sin(math.radians(rot)),
                             p[0] * math.sin(math.radians(rot)) + p[1] * math.cos(math.radians(rot))))
    cap_pts = [tr(p) for p in dome]
    out.append(path(closed_d(cap_pts), fill=color, filter="url(#glow)", opacity="0.55"))
    out.append(path(closed_d(cap_pts), fill=color))
    # gills: fine lines fanning from under the cap's centre to its rim
    gills = []
    for i in range(1, 12):
        x = -cap_r + 2 * cap_r * i / 12
        gills.append(path(line_d([tr((x * 0.25, 2)), tr((x, rim_droop * 0.6 + 2))]), stroke=pal["gill"], stroke_width="0.7", opacity="0.8", fill="none"))
    out += gills
    # spots
    for i in range(r.int(0, 6)):
        ri = r / f"spot{i}"
        a = ri.range(math.pi * 1.1, math.pi * 1.9)
        d = ri.range(0.3, 0.85)
        out.append(circle(tr((cap_r * d * math.cos(a), cap_r * squash * d * math.sin(a))), ri.range(0.8, 2.6), fill=pal["spot"], opacity="0.9"))
    return out

def bracket(r, pal, at, side, size, color):
    """A shelf fungus stuck to the stalk: a half disc, banded."""
    out = []
    bands = r.int(3, 6)
    for b in range(bands, 0, -1):
        rr = size * b / bands
        pts = [add(at, (side * rr * math.cos(t), -rr * 0.42 * math.sin(t) + rr * 0.08)) for t in [math.pi / 2 * k / 16 for k in range(-16, 17)]]
        out.append(path(closed_d(pts), fill=color if b % 2 else "#000", stroke=color, stroke_width="0.8", opacity="0.95" if b % 2 else "0.9"))
    out.insert(0, path(closed_d([add(at, (side * size * math.cos(t), -size * 0.42 * math.sin(t))) for t in [math.pi / 2 * k / 16 for k in range(-16, 17)]]),
                       fill=color, filter="url(#glow)", opacity="0.45"))
    return out

def coral(r, pal, base, heading, length_, color, depth=0):
    """Coral fungus: forks and forks again, each tip a bead."""
    end = add(base, mul(length_, dirv(heading)))
    mid = add(add(base, mul(length_ / 2, dirv(heading))), mul(r.range(-5, 5), dirv(heading + math.pi / 2)))
    out = [path(line_d(spline([base, mid, end], 5)), fill="none", stroke=color, stroke_width=num(max(0.8, 3.2 - depth * 0.8)), stroke_linecap="round")]
    if depth < 3:
        for k in (-1, 1):
            rk = r / f"b{k}"
            out += coral(rk, pal, end, heading + k * rk.range(0.25, 0.6), length_ * rk.range(0.55, 0.8), color, depth + 1)
    else:
        out.append(circle(end, 2.4, fill=pal["spot"]))
        out.append(circle(end, 4.5, fill=color, filter="url(#glow)", opacity="0.7"))
    return out

def eye_puff(r, pal, at, radius, color, look):
    """A puffball that looks back: a lit ball, an iris, a pupil, a lid."""
    out = [circle(at, radius * 1.25, fill=color, filter="url(#haze)", opacity="0.5"),
           circle(at, radius, fill=color),
           circle(add(at, (-radius * 0.3, -radius * 0.35)), radius * 0.28, fill="#ffffff", opacity="0.35")]
    iris_c = add(at, mul(radius * 0.32, dirv(look)))
    out += [circle(iris_c, radius * 0.48, fill="#000"),
            circle(iris_c, radius * 0.4, fill=pal["eye"]),
            circle(add(iris_c, mul(radius * 0.08, dirv(look))), radius * 0.18, fill="#000"),
            circle(add(iris_c, (-radius * 0.12, -radius * 0.14)), radius * 0.07, fill="#ffffff")]
    lid = [add(at, (radius * math.cos(t), radius * 0.55 * math.sin(t) - radius * 0.2)) for t in [math.pi + math.pi * k / 20 for k in range(21)]]
    out.append(path(line_d(lid), fill="none", stroke="#000", stroke_width=num(radius * 0.12), stroke_linecap="round", opacity="0.85"))
    return out

def stinkhorn(r, pal, at, size, color):
    """An octopus stinkhorn: arms curling out from a cup, dotted with dark spore slime."""
    out = [ellipse(add(at, (0, 4)), size * 0.5, size * 0.28, 0, fill=pal["stem"], opacity="0.9")]
    arms = r.int(4, 6)
    for k in range(arms):
        rk = r / f"arm{k}"
        h0 = UP + (k - (arms - 1) / 2) * rk.range(0.35, 0.5)
        curl = rk.pick([-1, 1]) * rk.range(0.06, 0.14)
        pts = grow(at, h0, 16, size / 9, lambda i, p, h: h + curl * (1 + i / 6))
        sp = spline(pts, 3)
        out.append(path(closed_d(ribbon(sp, lambda t: size * 0.2 * (1 - t) + 1)), fill=color))
        for j in range(3, len(sp) - 4, 5):
            out.append(circle(sp[j], size * 0.035 + 0.6, fill="#000", opacity="0.8"))
    out.insert(0, circle(add(at, (0, -size * 0.6)), size * 1.1, fill=color, filter="url(#haze)", opacity="0.35"))
    return out

def morel(r, pal, at, height, color):
    """A morel: a pale stem under a cone of pits."""
    w = height * 0.36
    out = [path(closed_d([add(at, (-w * 0.35, 0)), add(at, (-w * 0.3, -height * 0.4)), add(at, (w * 0.3, -height * 0.4)), add(at, (w * 0.35, 0))]), fill=pal["stem"])]
    top = add(at, (0, -height))
    base_y = at[1] - height * 0.35
    shoulder = base_y - (base_y - top[1]) * 0.6
    cone = spline([(at[0] - w / 2, base_y), (at[0] - w * 0.3, shoulder), top, (at[0] + w * 0.3, shoulder), (at[0] + w / 2, base_y)], 6)
    out.append(path(closed_d(cone), fill=color, filter="url(#glow)", opacity="0.5"))
    out.append(path(closed_d(cone), fill=color))
    for row in range(6):
        y = base_y - (height * 0.62) * (row + 0.5) / 6
        span = w * 0.42 * (1 - (row + 0.5) / 6.5)
        cells = max(1, 4 - row // 2)
        for c in range(cells):
            x = at[0] - span + 2 * span * (c + 0.5) / cells + (r / f"{row}.{c}").range(-1.2, 1.2)
            out.append(ellipse((x, y), max(1.0, span / cells * 0.7), 2.6, (r / f"rot{row}.{c}").range(-25, 25), fill="#000", opacity="0.85"))
    return out

def drifter(r, pal, at, size, color):
    """A cap adrift, like a jellyfish: a bell trailing long wavy tendrils, all of it glowing."""
    out = []
    bell = [add(at, (size * math.cos(t), -size * 0.75 * math.sin(t))) for t in [math.pi * k / 24 for k in range(25)]]
    hem = [add(at, (size * (1 - 2 * k / 12), size * 0.12 * math.sin(k * 1.7))) for k in range(13)]
    shape = closed_d(bell + hem)
    out.append(path(shape, fill=color, filter="url(#haze)", opacity="0.45"))
    for k in range(r.int(5, 8)):
        rk = r / f"t{k}"
        x0 = at[0] + size * (-0.8 + 1.6 * k / 7)
        amp, freq, ln = rk.range(3, 9), rk.range(0.05, 0.12), rk.range(size * 2.5, size * 6)
        pts = [(x0 + amp * math.sin(y * freq + k), at[1] + y) for y in range(0, int(ln), 4)]
        out.append(path(line_d(pts), fill="none", stroke=color, stroke_width=num(rk.range(0.6, 1.4)), stroke_linecap="round",
                        opacity=num(rk.range(0.5, 0.9))))
    out.append(path(shape, fill=color, opacity="0.9"))
    for i in range(r.int(3, 7)):
        ri = r / f"dot{i}"
        out.append(circle(add(at, (ri.range(-size * 0.6, size * 0.6), -ri.range(size * 0.1, size * 0.55))), ri.range(0.8, 2.2), fill=pal["spot"]))
    return out

def spores(r, pal, n, box):
    x0, y0, x1, y1 = box
    out = []
    for i in range(n):
        ri = r / i
        out.append(circle((ri.range(x0, x1), ri.range(y0, y1)), ri.range(0.5, 1.6), fill=pal["spore"], opacity=num(ri.range(0.35, 0.95))))
    return out

# ---------- composition ----------

def svg(seed, pal_name, background=True):
    pal = PALETTES[pal_name]
    r = R(f"fungi/{seed}")
    caps = pal["caps"]
    parts = []
    # the stalk: a dark trunk of fused hyphae, rising from the floor to about a third of the way down
    trunk_top = (W * 0.52 + r.range(-20, 20), r.range(300, 380))
    trunk = spline([(W * 0.5, H + 20), (W * 0.48 + r.range(-25, 25), H * 0.72), (W * 0.55 + r.range(-25, 25), H * 0.5), trunk_top])
    parts.append(path(closed_d(ribbon(trunk, lambda t: 30 * (1 - t) + 7)), fill="#101418", stroke=pal["thread"], stroke_width="0.9", opacity="0.95"))
    # threads wound round the trunk
    for k in range(3):
        rk = r / f"wind{k}"
        ns = normals(trunk)
        wound = [add(trunk[i], mul((14 * (1 - i / len(trunk)) + 4) * math.sin(i * rk.range(0.25, 0.4) + k * 2), ns[i])) for i in range(len(trunk))]
        parts.append(path(line_d(wound), fill="none", stroke=pal["thread"], stroke_width="0.8", opacity="0.6"))
    # bracket fungi stacked up the trunk, alternating sides
    for j in range(r.int(5, 7)):
        rj = r / f"bracket{j}"
        i = int(len(trunk) * (0.18 + 0.62 * j / 6.5))
        side = 1 if j % 2 else -1
        wdt = 30 * (1 - i / len(trunk)) + 7
        at = add(trunk[i], (side * wdt / 2, 0))
        parts += bracket(rj, pal, at, side, rj.range(22, 40), caps[j % len(caps)])
    # a crown of lamp mushrooms where the trunk ends
    for k in range(r.int(3, 5)):
        rk = r / f"crown{k}"
        parts += lamp(rk, pal, trunk_top, rk.range(50, 120), rk.range(-0.7, 0.7), rk.range(12, 26), rk.pick(caps))
    # caps hanging from the top edge on threads
    for k in range(r.int(3, 5)):
        rk = r / f"hang{k}"
        x = rk.range(30, W - 30)
        parts += mycelium(rk / "thread", pal, (x, -4), math.pi / 2 + rk.range(-0.3, 0.3), rk.range(40, 90))
        parts += lamp(rk, pal, (x, -2), rk.range(70, 190), rk.range(-0.3, 0.3), rk.range(9, 18), rk.pick(caps), hanging=True)
    # the floor: a mat of mycelium
    for k in range(6):
        rk = r / f"mat{k}"
        parts += mycelium(rk, pal, (rk.range(0, W), H - rk.range(10, 60)), rk.pick([0.0, math.pi]) + rk.range(-0.4, 0.4), rk.range(60, 140))
    # on the floor: lamps, coral, a stinkhorn, morels, and eyes
    floor_y = H - 70
    for k in range(r.int(4, 6)):
        rk = r / f"lamp{k}"
        x = rk.range(30, W - 30)
        hgt = rk.range(40, 170)
        lean = rk.range(-0.35, 0.35)
        lean = max(-(x - 25) / hgt, min((W - 25 - x) / hgt, lean))   # the cap stays inside the margin
        parts += lamp(rk, pal, (x, floor_y + rk.range(-10, 25)), hgt, lean, rk.range(8, 22), rk.pick(caps))
    parts += coral(r / "coral", pal, (r.range(40, 110), floor_y + 15), UP + r.range(-0.2, 0.2), r.range(30, 46), r.pick(caps))
    parts += stinkhorn(r / "stink", pal, (r.range(220, 300), floor_y + 20), r.range(30, 46), caps[0])
    for k in range(2):
        rk = r / f"morel{k}"
        parts += morel(rk, pal, (rk.range(110, 230), floor_y + 34 + k * 8), rk.range(62, 92), rk.pick(caps[1:]))
    # eyes: on the floor, on the trunk, and one in the hanging caps
    looks = [r / f"look{k}" for k in range(4)]
    parts += eye_puff(r / "eye0", pal, (r.range(250, 310), floor_y - r.range(80, 140)), r.range(13, 19), caps[2], looks[0].range(-3, 0))
    i = int(len(trunk) * 0.4)
    parts += eye_puff(r / "eye1", pal, add(trunk[i], (0, 0)), 11, caps[3], looks[1].range(-3, 0))
    parts += eye_puff(r / "eye2", pal, (r.range(40, 120), r.range(180, 260)), r.range(9, 13), caps[4], looks[2].range(0, 3))
    # drifters in the open middle, on the side away from the trunk
    for k in range(r.int(2, 3)):
        rk = r / f"drift{k}"
        i = int(len(trunk) * rk.range(0.35, 0.75))
        side = -1 if trunk[i][0] > W / 2 else 1
        x = trunk[i][0] + side * rk.range(70, 120)
        x = max(30, min(W - 30, x))
        parts += drifter(rk, pal, (x, trunk[i][1] + rk.range(-40, 40)), rk.range(14, 24), rk.pick(caps))
    # spores drifting up
    parts += spores(r / "spores", pal, 90, (0, 0, W, H))
    bg = f'<rect width="{num(W)}" height="{num(H)}" fill="#000"/>' if background else ""
    return (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {num(W)} {num(H)}" width="{num(W)}" height="{num(H)}">'
            + defs(pal) + bg + g(parts, mask="url(#fade)") + "</svg>\n")

if __name__ == "__main__":
    out = sys.argv[1] if len(sys.argv) > 1 else "."
    seeds = [int(s) for s in sys.argv[2:]] or [1]
    os.makedirs(out, exist_ok=True)
    for name in PALETTES:
        for seed in seeds:
            p = os.path.join(out, f"fungi-{name}-{seed}.svg")
            open(p, "w").write(svg(seed, name))
            print(p, os.path.getsize(p))
