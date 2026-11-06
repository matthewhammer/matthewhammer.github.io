#!/usr/bin/env python3
"""The pictures on the Hazel page (hazel/index.html), drawn as inline SVG in the page's own colours (its CSS variables), so
they follow its light and dark themes. (Matthew, 2026-11-06: "let's add graphics to https://matthewhammer.org/hazel/",
then "SVG, preferably".) Each sits between <!-- fig:NAME begin --> and <!-- fig:NAME end --> in the page; run this again
after changing one:

  python3 tools/hazel_page_figs.py

The first run puts the markers in, after the paragraph each picture belongs to. The pictures' styles are the page's
`figure.hz` rules, between /* figs: style begin */ and /* figs: style end */ (CSS comments: inside <style>, an HTML
comment's <!-- is read as part of the next rule's selector, and that rule is dropped)."""
import os, re

SITE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PAGE = os.path.join(SITE, "hazel", "index.html")

STYLE = """    /* the pictures (tools/hazel_page_figs.py): inline SVG in the page's colours, light or dark */
    figure.hz { margin: 1.5rem 0 0.4rem; }
    figure.hz svg { display: block; width: 100%; height: auto; max-width: 34rem; }
    figure.hz .pair { display: flex; flex-wrap: wrap; gap: 1rem 1.5rem; }
    figure.hz .pair > svg { flex: 1 1 15rem; min-width: 0; max-width: 21rem; }
    figure.hz figcaption { color: var(--dim); font-size: 0.88em; margin-top: 0.5rem; max-width: 34rem; }
    figure.hz figcaption a { border-bottom-color: var(--rule); }
    .hz text { font-family: var(--ui); fill: var(--fg); }
    .hz .mono { font-family: var(--mono); }
    .hz .kw { fill: var(--blue); font-weight: 700; }
    .hz .hole { fill: var(--sym); font-weight: 700; }
    .hz .lbl { fill: var(--dim); font-size: 12px; }
    .hz .ttl { fill: var(--ink); font-weight: 600; font-size: 14px; }
    .hz .box { fill: var(--panel); stroke: var(--rule); stroke-width: 1.5; }
    .hz .page { fill: none; stroke: var(--rule); stroke-width: 1.5; stroke-dasharray: 5 4; }
    .hz .card { fill: var(--bg); stroke: var(--rule); stroke-width: 1.5; }
    .hz .ln { fill: none; stroke: var(--dim); stroke-width: 1.6; stroke-linecap: round; stroke-linejoin: round; }
    .hz .ink { fill: none; stroke: var(--ink); stroke-width: 1.6; stroke-linecap: round; stroke-linejoin: round; }
    .hz .dot { fill: var(--ink); }
    .hz .acc { fill: var(--accent); }
    .hz .accs { fill: none; stroke: var(--accent); stroke-width: 1.8; }
    .hz .blu { fill: var(--blue); }
    .hz .blus { fill: none; stroke: var(--blue); stroke-width: 1.6; }
    .hz .sym { fill: var(--sym); }
    .hz .faint { fill: var(--rule); }
    .hz .soft { opacity: 0.22; }
"""


def svg(w, h, label, body, extra=""):
    return (f'<svg class="hz" viewBox="0 0 {w} {h}" role="img" aria-label="{label}"{extra}>'
            f'<title>{label}</title>{body}</svg>')


def arrow(x1, y1, x2, y2, cls="ln"):
    """A straight arrow, its head at (x2, y2)."""
    import math
    a = math.atan2(y2 - y1, x2 - x1)
    h = 7
    p1 = (x2 - h * math.cos(a - 0.5), y2 - h * math.sin(a - 0.5))
    p2 = (x2 - h * math.cos(a + 0.5), y2 - h * math.sin(a + 0.5))
    return (f'<path class="{cls}" d="M{x1} {y1} L{x2} {y2}"/>'
            f'<path class="{cls}" d="M{p1[0]:.1f} {p1[1]:.1f} L{x2} {y2} L{p2[0]:.1f} {p2[1]:.1f}"/>')


# --- Hazel: an unfinished program still has a type and a result ---

def fig_holes():
    body = (
        '<rect class="box" x="6" y="8" width="272" height="104" rx="9"/>'
        '<text class="mono" x="22" y="40" font-size="15"><tspan class="kw">let</tspan> r = 2. <tspan class="kw">in</tspan></text>'
        '<text class="mono" x="22" y="68" font-size="15"><tspan class="kw">let</tspan> area = 3.14 *. r *. <tspan class="hole">?</tspan></text>'
        '<text class="mono" x="22" y="96" font-size="15"><tspan class="kw">in</tspan> area</text>'
        + arrow(286, 60, 308, 60)
        + '<text class="lbl" x="318" y="30">its type</text>'
        '<text class="mono" x="318" y="52" font-size="15" style="fill: var(--ink)">Float</text>'
        '<text class="lbl" x="318" y="80">its result, so far</text>'
        '<text class="mono" x="318" y="102" font-size="15" style="fill: var(--ink)">6.28 *. <tspan class="hole">?</tspan></text>'
    )
    return ('<figure class="hz">' + svg(460, 120, "An unfinished Hazel program: let r = 2. in let area = 3.14 *. r *. ? in area, "
                                        "with a hole, ?, where code is still to come. It still has a type, Float, and a result, 6.28 *. ?, "
                                        "computed around the hole.", body)
            + '<figcaption>A program with a hole (<code>?</code>) where code is still to come. Hazel still gives it a type, and '
              'runs it as far as it can: the result is computed around the hole.</figcaption></figure>')


# --- What I'm working on: a GUI inside Hazel, and Hazel inside a GUI ---

def fig_directions():
    # a livelit: a slider in the code, and what the code draws with its value
    left = (
        '<text class="ttl" x="8" y="20">A livelit: a GUI inside Hazel</text>'
        '<rect class="box" x="6" y="34" width="288" height="86" rx="9"/>'
        '<text class="mono" x="20" y="64" font-size="14"><tspan class="kw">let</tspan> size =</text>'
        '<path class="ln" d="M132 59 L232 59"/>'
        '<path d="M132 59 L196 59" fill="none" stroke="var(--accent)" stroke-width="3" stroke-linecap="round"/>'
        '<circle cx="196" cy="59" r="7.5" class="acc"/>'
        '<text class="mono" x="246" y="64" font-size="14" style="fill: var(--ink)">42</text>'
        '<text class="mono" x="20" y="100" font-size="14"><tspan class="kw">in</tspan> draw(size)</text>'
        + arrow(150, 126, 150, 146)
        + '<circle cx="150" cy="182" r="26" class="acc soft"/>'
        '<circle cx="150" cy="182" r="26" class="accs"/>'
        '<text class="lbl" x="190" y="186">draw(42)</text>'
    )
    # a Hazlet: the page keeps its state (a Petri net here) and asks a Hazel rule what each event does
    places = [(46, 82, True), (46, 156, False), (104, 119, True)]
    net = "".join(f'<circle cx="{x}" cy="{y}" r="11" style="fill: var(--bg)" class="ink"/>'
                  + (f'<circle cx="{x}" cy="{y}" r="3.5" class="dot"/>' if tok else "") for x, y, tok in places)
    net += ('<rect x="72" y="92" width="7" height="22" rx="1.5" class="acc"/>'
            '<rect x="72" y="125" width="7" height="22" rx="1.5" class="acc"/>'
            '<path class="ln" d="M55 88 L71 100"/><path class="ln" d="M80 103 L94 113"/>'
            '<path class="ln" d="M94 125 L80 134"/><path class="ln" d="M71 140 L55 150"/>')
    right = (
        '<text class="ttl" x="8" y="20">A Hazlet: Hazel inside a GUI</text>'
        '<rect class="page" x="6" y="34" width="288" height="168" rx="9"/>'
        '<text class="lbl" x="18" y="54">the page, in JavaScript</text>'
        + net
        + '<rect class="box" x="186" y="84" width="96" height="62" rx="8" style="stroke: var(--blue)"/>'
        '<text class="mono" x="198" y="111" font-size="13.5" style="fill: var(--ink)">on_event</text>'
        '<text class="lbl" x="198" y="131">in Hazel</text>'
        + arrow(128, 100, 180, 100)
        + '<text class="lbl" x="134" y="92">event</text>'
        + arrow(180, 130, 128, 130)
        + '<text class="lbl" x="124" y="148">commands</text>'
        '<text class="lbl" x="18" y="188">the net, kept and drawn by the page</text>'
    )
    return ('<figure class="hz"><div class="pair">'
            + svg(300, 212, "A livelit: a Hazel program, let size = a slider set to 42, in draw(size), and the circle it draws.", left)
            + svg(300, 212, "A Hazlet: a JavaScript page keeps a Petri net and draws it; for each event it calls on_event, a "
                            "rule in Hazel, and acts out the commands that come back.", right)
            + '</div><figcaption>Two directions. A livelit puts a GUI in a Hazel program. A Hazlet puts Hazel in a JavaScript '
              'program: the page keeps its state, and a Hazel rule decides what each event does. The second picture is '
              'the <a href="/hazeljs-petri-example/">Petri net example</a>, whose rules you can edit while its net runs.'
              '</figcaption></figure>')


# --- Hazel reports: one question each, answered with measurements ---

def fig_reports():
    def card(x, y, inner, title_w):
        return (f'<rect class="card" x="{x}" y="{y}" width="132" height="104" rx="7"/>'
                f'<rect x="{x + 12}" y="{y + 12}" width="{title_w}" height="7" rx="3" class="dot" style="opacity: 0.75"/>'
                f'<rect x="{x + 12}" y="{y + 25}" width="96" height="4" rx="2" class="faint"/>'
                f'<rect x="{x + 12}" y="{y + 33}" width="78" height="4" rx="2" class="faint"/>' + inner)
    bars = "".join(f'<rect x="{30 + 18 * i}" y="{132 - h}" width="11" height="{h}" rx="2" class="{c}"/>'
                   for i, (h, c) in enumerate([(52, "acc"), (34, "acc"), (22, "blu"), (12, "blu"), (8, "blu")]))
    line = ('<path class="blus" style="stroke-width: 2.2" d="M182 128 L200 120 L216 124 L232 104 L248 98 L264 82 L280 84"/>'
            '<path class="ln" d="M180 132 L284 132"/>')
    boxes = ('<rect x="320" y="74" width="34" height="20" rx="4" class="box"/>'
             '<rect x="384" y="74" width="34" height="20" rx="4" class="box"/>'
             '<rect x="352" y="112" width="34" height="20" rx="4" class="box" style="stroke: var(--sym)"/>'
             + arrow(356, 84, 382, 84) + arrow(337, 96, 360, 110) + arrow(401, 96, 380, 110))
    body = ('<g transform="translate(0 -14)">' + card(18, 30, bars, 70) + card(164, 22, line, 84) + card(306, 30, boxes, 60) + '</g>')
    return ('<figure class="hz">' + svg(456, 124, "Three report pages: one with a bar chart of times, one with a line chart, "
                                        "one with a diagram of how parts connect.", body)
            + '<figcaption>Each report answers one question, with its measurements, its pictures and the way to check them.'
              '</figcaption></figure>')


# --- Games with my kids: a paper doll with every part on a slider ---

def fig_doll():
    doll = (
        '<path d="M88 52 L110 14 L132 52 Z" class="acc"/>'                                   # hat
        '<circle cx="110" cy="70" r="19" style="fill: var(--panel)" class="ink"/>'          # head
        '<path class="ink" style="stroke: var(--sym); stroke-width: 5" d="M91 64 Q110 44 129 64"/>'  # hair
        '<circle cx="103" cy="70" r="2.2" class="dot"/><circle cx="117" cy="70" r="2.2" class="dot"/>'
        '<path class="ink" d="M103 78 Q110 84 117 78"/>'
        '<path class="ink" style="stroke-width: 4" d="M96 100 L66 128"/>'                   # arms: one down,
        '<path class="ink" style="stroke-width: 4" d="M124 100 L158 74"/>'                  # one up
        '<path d="M96 92 L124 92 L150 172 L70 172 Z" class="blu" style="opacity: 0.8"/>'   # dress
        '<path class="ink" style="stroke-width: 4" d="M98 172 L94 206"/><path class="ink" style="stroke-width: 4" d="M122 172 L126 206"/>'
    )
    rows = [("hat", 46, 0.30, (132, 40)), ("arm", 106, 0.78, (158, 74)), ("dress", 166, 0.55, (150, 150))]
    sliders = ""
    for name, y, f, (px, py) in rows:
        x0, x1 = 240, 438
        k = x0 + f * (x1 - x0)
        sliders += (f'<text class="lbl" x="{x0}" y="{y - 12}" style="font-size: 13px">{name}</text>'
                    f'<path class="ln" d="M{x0} {y} L{x1} {y}"/>'
                    f'<path d="M{x0} {y} L{k:.0f} {y}" fill="none" stroke="var(--accent)" stroke-width="3" stroke-linecap="round"/>'
                    f'<circle cx="{k:.0f}" cy="{y}" r="7.5" class="acc"/>'
                    f'<path d="M{x0 - 8} {y} L{px + 8} {py}" fill="none" stroke="var(--dim)" stroke-width="1.2" stroke-dasharray="3 4"/>')
    return ('<figure class="hz">' + svg(456, 214, "A paper doll, with sliders for its hat, its arm and its dress, each joined to "
                                        "the part it moves.", doll + sliders)
            + '<figcaption>Messed Up Dress Up: a paper doll with every part on a slider. My kids ask for a feature while they '
              'play, and it goes live for the next round.</figcaption></figure>')


# where each goes: after the paragraph it illustrates (the first run puts the markers in)
FIGS = [
    ("holes", "since this fall I have been working on it again.\n      </p>", fig_holes),
    ("directions", "on how both apps are built).\n      </p>", fig_directions),
    ("reports", "question and answers it, with measurements where there are any.\n      </p>", fig_reports),
    ("games", "next round of play tests it.\n      </p>", fig_doll),
]


def main():
    s = open(PAGE, encoding="utf-8").read()
    s = s.replace("<!-- figs: style begin -->", "    /* figs: style begin */").replace("<!-- figs: style end -->", "    /* figs: style end */")
    if "/* figs: style begin */" not in s:
        s = s.replace("  </style>\n</head>", "    /* figs: style begin */\n    /* figs: style end */\n  </style>\n</head>", 1)
    s = re.sub(r"/\* figs: style begin \*/.*?/\* figs: style end \*/",
               lambda m: "/* figs: style begin */\n" + STYLE + "    /* figs: style end */", s, flags=re.S)
    for name, after, make in FIGS:
        begin, end = f"<!-- fig:{name} begin -->", f"<!-- fig:{name} end -->"
        if begin not in s:
            assert s.count(after) == 1, f"no single place for {name}"
            s = s.replace(after, after + f"\n      {begin}\n      {end}", 1)
        s = re.sub(re.escape(begin) + r".*?" + re.escape(end), lambda m: begin + make() + end, s, flags=re.S)
    open(PAGE, "w", encoding="utf-8").write(s)
    print("hazel/index.html: " + ", ".join(n for n, _, _ in FIGS))


if __name__ == "__main__":
    main()
