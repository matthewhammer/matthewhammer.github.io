"""Check the nav bar (ticket 296): crawl the three sites from their front pages, following links within them, and list
every reachable page that does not load nav.js, with how each page answers the theme. The dashboard's snapshots are
left out (records of the page as it was). Reads the live sites: run it after the pushes have deployed.

    python3 nav/check_nav.py              the summary, and the pages without the bar
    python3 nav/check_nav.py --all        every page"""
import re, sys, urllib.request, urllib.parse, concurrent.futures as cf, html as H, time
HOSTS = {"matthewhammer.org", "www.matthewhammer.org", "fumola.org", "www.fumola.org", "adapton.org", "www.adapton.org"}
ROOTS = ["https://matthewhammer.org/", "https://fumola.org/", "https://adapton.org/"]
SKIP = re.compile(r"/dashboard-snapshots/|\.(pdf|png|jpe?g|gif|svg|webp|mp3|ogg|wav|json|zip|gz|txt|md|ps|bib|tar|tgz|js|css|wasm|ico|xml|csv|ppt|pptx|key|mov|mp4|hz|py)$", re.I)
def norm(base, href):
    href = H.unescape(href.strip())
    if href.startswith(("mailto:", "javascript:", "#", "data:")) or "${" in href: return None
    s = urllib.parse.urlsplit(urllib.parse.urljoin(base, href))
    if s.scheme not in ("http", "https") or s.hostname not in HOSTS: return None
    path = s.path or "/"
    if path.endswith("/index.html"): path = path[:-10]
    return None if SKIP.search(path) else f"https://{s.hostname.replace('www.', '')}{path}"
def fetch(u):
    try:
        r = urllib.request.urlopen(urllib.request.Request(u + "?nav-check=%d" % time.time(), headers={"User-Agent": "nav-check"}), timeout=20)
        if "html" not in (r.headers.get("Content-Type") or ""): return u, None, None
        return u, r.read().decode("utf-8", "replace"), r.geturl().split("?")[0]
    except Exception as e: return u, None, str(e)[:50]
seen, frontier, rows = set(ROOTS), list(ROOTS), []
while frontier and len(seen) < 1500:
    batch, frontier = frontier, []
    with cf.ThreadPoolExecutor(4) as ex:
        for u, b, final in ex.map(fetch, batch):
            if b is None:
                if final: rows.append((u, "unreachable: " + final, ""))
                continue
            nav = "nav/nav.js" in b
            m = re.search(r'<meta name="theme-support" content="([^"]+)"', b)
            theme = ("own: " + m.group(1)) if m else "fallback dark"
            rows.append((u, "bar" if nav else "NO BAR", theme))
            for a in re.finditer(r'<a\b[^>]*\bhref\s*=\s*"([^"]*)"', b, re.I):
                v = norm(final or u, a.group(1))
                if v and v not in seen: seen.add(v); frontier.append(v)
pages = [r for r in rows if not r[1].startswith("unreachable")]
print(f"{len(pages)} reachable pages: {sum(r[1] == 'bar' for r in pages)} with the bar, {sum(r[1] == 'NO BAR' for r in pages)} without; "
      f"{sum(r[2].startswith('own') for r in pages)} answer the theme themselves, {sum(r[2] == 'fallback dark' for r in pages)} take the fallback dark")
for u, nav, theme in sorted(rows):
    if "--all" in sys.argv or nav == "NO BAR": print(f"  {nav:8} {theme:22} {u}")
broken = [u for u, nav, _ in rows if nav.startswith("unreachable")]
if broken: print(f"links to {len(broken)} missing pages (see --all)" if "--all" not in sys.argv else "")
