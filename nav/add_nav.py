"""The constellation's nav bar on a page (ticket 296): nav.js's one-line include, between markers, right after <head>,
with the page's own answers to it. Idempotent: run it again whenever, on any page.

    python3 nav/add_nav.py page.html ...           a page
    python3 nav/add_nav.py --app page.html ...     a full-window app (the game, a Hazel build): the bar starts hidden there
    python3 nav/add_nav.py --own page.html ...     a page that keeps its own colours, whatever theme is chosen

On the way it makes a page's dark style answer the choice too: a page whose only dark style is
`@media (prefers-color-scheme: dark) { :root { ... } }` (the work notes, the crash log) gets the same tokens under
:root[data-theme="dark"], and its media rule only while light is not chosen. A page whose CSS answers data-theme says so
(<meta name="theme-support" content="data-theme">); any other gets nav.js's plain dark style. A script that writes pages
can call stamp() on what it writes (import it from here, or copy NAV's lines: they are the whole contract).
How it fits together: nav/agent-instructions.md."""
import re, sys
B, E = "<!-- nav: begin -->", "<!-- nav: end -->"
SRC = "https://matthewhammer.org/nav/nav.js"
PREVIEW = re.compile(r'<script src="/nav/nav\.js"></script><!-- \(ticket 296: the nav bar, a local preview; not committed\) -->\n?')
AUTO_ONLY = re.compile(r'@media\s*\(\s*prefers-color-scheme\s*:\s*dark\s*\)\s*\{\s*:root\s*\{([^{}]*)\}\s*\}')
def answers_theme(s):
    return bool(re.search(r'data-theme\s*=\s*"?dark"?\s*\]', s)) or "<!-- theme: head begin -->" in s
def stamp(s, app=False, own=False):
    s = PREVIEW.sub("", s)
    if not answers_theme(s) and not own:
        m = AUTO_ONLY.search(s)
        if m:   # (the system's dark, kept for auto; and the same, chosen)
            s = s[:m.start()] + f'@media (prefers-color-scheme: dark) {{ :root:not([data-theme="light"]) {{{m.group(1)}}} }}\n  :root[data-theme="dark"] {{{m.group(1)}}}' + s[m.end():]
    meta = ('<meta name="theme-support" content="own">' if own else '<meta name="theme-support" content="data-theme">' if answers_theme(s) else "") + \
           ('<meta name="nav" content="hidden">' if app else "")
    block = f'{B}{meta}<script src="{SRC}"></script>{E}'
    if B in s: return s[:s.index(B)] + block + s[s.index(E) + len(E):]
    # (after the charset, which comes first, where the page has one; else right after <head>)
    m = re.search(r"<meta\s+charset=[^>]*>", s[:4096], re.I) or re.search(r"<head\b[^>]*>", s, re.I)
    if not m: raise ValueError("no <head>")
    return s[:m.end()] + "\n" + block + s[m.end():]
if __name__ == "__main__":
    args = sys.argv[1:]; app = "--app" in args; own = "--own" in args
    for path in [a for a in args if not a.startswith("--")]:
        s = open(path, encoding="utf-8", errors="surrogateescape").read()
        t = stamp(s, app, own)
        if t != s: open(path, "w", encoding="utf-8", errors="surrogateescape").write(t)
        print(("stamped: " if t != s else "as it was: ") + path)
