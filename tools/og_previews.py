#!/usr/bin/env python3
"""Link previews for the report series: a picture beside the text, when a report's link is pasted into Slack (or anywhere
that reads Open Graph tags).

(2026-10-25, Matthew: "For each reports link, we've got a preview text (for a Slack paste) but the thumbnail isn't there.
Can we do the thumbnail too?") For every report on a series index, this takes the thumbnail its card shows (an SVG or
WebP file, or an SVG drawn in the card itself), draws it as og.png beside the report (1200 x 630 on white: Slack shows
neither SVG nor, everywhere, WebP), and writes the report's Open Graph and Twitter tags between <!-- og: begin --> and
<!-- og: end --> in its <head>: title, description, the picture, the address. Run again after every publish: a report
copied over again (rsync --delete) loses both, and this puts them back. Idempotent: a report whose og.png is newer
than its thumbnail is not drawn again.

  python3 tools/og_previews.py hazel        the Hazel reports (this site, hazel/reports/)
  python3 tools/og_previews.py dress-up     the Dress Up reports (~/kids-choice-games/dress-up/reports/)
  python3 tools/og_previews.py all
Needs ffmpeg built with librsvg."""
import html, os, re, subprocess, sys, tempfile

SITE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SERIES = {
    "hazel": (os.path.join(SITE, "hazel", "reports"), "https://matthewhammer.org/hazel/reports/"),
    "dress-up": (os.path.expanduser("~/kids-choice-games/dress-up/reports"), "https://matthewhammer.org/kids-choice-games/dress-up/reports/"),
}
W, H = 1200, 630

def cards(index):
    """(number, thumbnail: ('file', path) or ('svg', text) or None) for each report card on a series index."""
    out = []
    for m in re.finditer(r'<a class="(?:thumb|card)" href="(\d{3})/">(.*?)</a>', index, re.S):
        n, inner = m.group(1), m.group(2)
        img = re.search(r'<img[^>]*\bsrc="(\d{3}/[^"]+)"', inner)
        svg = re.search(r'<svg\b.*?</svg>', inner, re.S)
        if any(c[0] == n for c in out):
            continue
        out.append((n, ("file", img.group(1)) if img else ("svg", svg.group(0)) if svg else None))
    return out

def light_vars(index):
    """The index's light-theme custom properties, so a card's inline SVG can be drawn without the page's CSS."""
    m = re.search(r':root\s*\{([^}]*)\}', index)
    return dict(re.findall(r'(--[\w-]+)\s*:\s*([^;]+);', m.group(1))) if m else {}

def inline_svg(svg, index):
    """A card's own SVG, standalone: its classes' rules from the index, the theme's variables made colours."""
    vs = light_vars(index)
    classes = set(re.findall(r'class="([^"]+)"', svg))
    names = {c for cs in classes for c in cs.split()}
    rules = [r for r in re.findall(r'(\.thumb[^{}]*\{[^}]*\})', index) if any("." + n in r for n in names)]
    css = re.sub(r'var\((--[\w-]+)\)', lambda m: vs.get(m.group(1), "#888").strip(), " ".join(rules))
    css = css.replace(".thumb ", "")
    if 'xmlns=' not in svg:
        svg = svg.replace("<svg", '<svg xmlns="http://www.w3.org/2000/svg"', 1)
    svg = re.sub(r'var\((--[\w-]+)\)', lambda m: vs.get(m.group(1), "#888").strip(), svg)
    return re.sub(r'(<svg\b[^>]*>)', lambda m: m.group(1) + f"<style>{css}</style>", svg, count=1)

def sized(svg):
    """The SVG with a width and height to draw at: from its viewBox, as large as fits the picture (one with only a
    viewBox is otherwise drawn at its own small size, in a corner)."""
    m = re.search(r'<svg\b[^>]*\bviewBox="\s*[-\d.]+[\s,]+[-\d.]+[\s,]+([\d.]+)[\s,]+([\d.]+)', svg)
    if not m:
        return svg
    vw, vh = float(m.group(1)), float(m.group(2))
    k = min((W - 100) / vw, (H - 70) / vh)
    root = re.search(r'<svg\b[^>]*>', svg).group(0)
    bare = re.sub(r'\s(width|height)="[^"]*"', "", root)
    return svg.replace(root, bare.replace("<svg", f'<svg width="{round(vw * k)}" height="{round(vh * k)}"', 1), 1)

def draw(src, out, is_svg):
    """src drawn into a W x H white picture, centred, as large as fits with a margin."""
    tmp = None
    if is_svg:
        with tempfile.NamedTemporaryFile("w", suffix=".svg", delete=False) as f:
            f.write(sized(open(src).read())); tmp = f.name
        src = tmp
    pre = []
    fil = (f"color=white:s={W}x{H}[bg];[0:v]scale={W - 100}:{H - 70}:force_original_aspect_ratio=decrease[fg];"
           f"[bg][fg]overlay=(W-w)/2:(H-h)/2:format=auto,format=rgb24")
    r = subprocess.run(["ffmpeg", "-loglevel", "error", "-y", *pre, "-i", src, "-filter_complex", fil, "-frames:v", "1", out],
                       capture_output=True, text=True)
    if tmp:
        os.unlink(tmp)
    return r.returncode == 0 and os.path.exists(out), r.stderr.strip()[:200]

def blurb(index, n):
    """The report's line on its series card (a Hazel card's p.blurb, a Dress Up card's div.q), as plain text."""
    i = index.find(f'href="{n}/"')
    m = re.search(r'<(?:p class="blurb"|div class="q")>(.*?)</(?:p|div)>', index[i:i + 6000], re.S) if i >= 0 else None
    return re.sub(r"\s+", " ", html.unescape(re.sub(r"<[^>]+>", "", m.group(1)))).strip() if m else ""

def tags(page, url, img, card=""):
    title = re.search(r'<title>(.*?)</title>', page, re.S)
    desc = re.search(r'<meta name="description" content="([^"]*)"', page)
    t = html.unescape(title.group(1).strip()) if title else ""
    d = html.unescape(desc.group(1)) if desc else card   # (no description of its own: its card's line)
    if len(d) > 300:
        d = d[:297].rsplit(" ", 1)[0] + "…"
    a = lambda s: html.escape(s, quote=True)
    return ("<!-- og: begin (tools/og_previews.py in matthewhammer.github.io: the link preview's text and picture) -->\n"
            f'<meta property="og:type" content="article">\n<meta property="og:title" content="{a(t)}">\n'
            + (f'<meta property="og:description" content="{a(d)}">\n' if d else "")
            + f'<meta property="og:url" content="{a(url)}">\n<meta property="og:image" content="{a(img)}">\n'
            f'<meta property="og:image:width" content="{W}">\n<meta property="og:image:height" content="{H}">\n'
            f'<meta name="twitter:card" content="summary_large_image">\n<meta name="twitter:image" content="{a(img)}">\n'
            "<!-- og: end -->\n")

def run(series):
    root, base = SERIES[series]
    index = open(os.path.join(root, "index.html")).read()
    done, skipped = 0, []
    for n, th in cards(index):
        page_path = os.path.join(root, n, "index.html")
        if not os.path.exists(page_path):
            skipped.append(f"{n}: no page"); continue
        if not th:
            skipped.append(f"{n}: no thumbnail on its card"); continue
        out = os.path.join(root, n, "og.png")
        if th[0] == "file":
            src = os.path.join(root, th[1])
            if not os.path.exists(src):
                skipped.append(f"{n}: {th[1]} missing"); continue
            fresh = os.path.exists(out) and os.path.getmtime(out) >= os.path.getmtime(src)
            ok, err = (True, "") if fresh else draw(src, out, src.endswith(".svg"))
        else:
            with tempfile.NamedTemporaryFile("w", suffix=".svg", delete=False) as f:
                f.write(inline_svg(th[1], index)); tmp = f.name
            ok, err = draw(tmp, out, True); os.unlink(tmp)
        if not ok:
            skipped.append(f"{n}: not drawn ({err})"); continue
        page = open(page_path).read()
        block = tags(page, f"{base}{n}/", f"{base}{n}/og.png", blurb(index, n))
        page = re.sub(r"<!-- og: begin.*?<!-- og: end -->\n", "", page, flags=re.S)
        page = re.sub(r"(</title>\n?)", lambda m: m.group(1) + ("" if m.group(1).endswith("\n") else "\n") + block, page, count=1)
        open(page_path, "w").write(page)
        done += 1
    print(f"{series}: {done} reports with a preview picture" + ("; skipped: " + "; ".join(skipped) if skipped else ""))

if __name__ == "__main__":
    which = sys.argv[1] if len(sys.argv) > 1 else "all"
    for s in (SERIES if which == "all" else [which]):
        run(s)
