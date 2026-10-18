# The nav bar: agent instructions

Every page reachable from Matthew's three sites, [matthewhammer.org](https://matthewhammer.org/), [fumola.org](https://fumola.org/) and [adapton.org](https://adapton.org/), has the same nav bar on its left, and lets its reader choose ◐ auto, ☀ light or ☾ dark. Matthew's words (ticket 296, 2026-10-18): "each page we can reach should have a nav bar on the LHS, with drag controls, and a meaningful connection in terms of that LHS to related (linked) pages. ideally, the nav bar would be a single outline, with different parts projected out for each page we can reach in this constellation". He settled this design on 2026-10-18. Read this before adding a page, moving one, or changing the bar.

## Where it is

| What | Where |
|---|---|
| The bar | [`nav/nav.js`](nav.js), served at https://matthewhammer.org/nav/nav.js. One file: the outline (`OUTLINE`), its projection, the drag controls, the theme, and its own CSS. Every page loads this one file, so a change here reaches every page (within about ten minutes: Pages serves it with `max-age=600`). |
| Putting it on a page | [`nav/add_nav.py`](add_nav.py): the include, between `<!-- nav: begin -->` and `<!-- nav: end -->`, after the page's charset. Idempotent: run it on any page, any time. `--app` for a full-window app, `--own` for a page that keeps its own colours. |
| These instructions | `nav/agent-instructions.md`, here. Each repository that writes pages links here from its own instructions. |

## The contract on a page

All of it is in the page's `<head>`, written by `add_nav.py`:

```html
<!-- nav: begin --><meta name="theme-support" content="data-theme"><script src="https://matthewhammer.org/nav/nav.js"></script><!-- nav: end -->
```

- **The script**, not deferred, so the theme is set before the page paints. It builds the bar when the page has loaded.
- **`<meta name="theme-support" content="data-theme">`**: the page's CSS answers the choice. Its colours are tokens on `:root`, the same tokens again under `:root[data-theme="dark"]`, and its `@media (prefers-color-scheme: dark)` rule applies only to `:root:not([data-theme="light"])`. `add_nav.py` adds the meta when the page already does this, and converts a page whose only dark style is a single `@media (prefers-color-scheme: dark) { :root { … } }` rule (the work notes and the crash log were).
- **No theme-support meta**: the page has no dark style of its own (the old research pages, `pubs.html`, the CEAL manual, the archives' pages). nav.js gives it a plain dark one (`FALLBACK_DARK`), when dark is chosen or the system's is dark on auto. Better is to give the page its own tokens and the meta.
- **`<meta name="theme-support" content="own">`**: the page keeps its own colours, whatever is chosen. Only for the game.
- **`<meta name="nav" content="hidden">`**: a full-window app, the game or a Hazel build. There the bar starts hidden each visit, its » at the bottom left, and the page keeps the whole window. Matthew's rule for the game: the picture as large as possible at all times.

## The outline, and how a page sees it

- **One outline:** `OUTLINE` in nav.js, `[title, url, children]`, as the sites are: Matthew Hammer (the publications and the project pages they link, the CV), Hazel (the performance analysis and its reports, the work dashboard, the work notes, the crash log, Messed Up Dress Up with its game, reports, pictures and music), Fumola, Adapton.
- **The projection:** each page shows the four sites, and its own branch opened out, its parents, its siblings and its own children. The page itself is marked, with its own headings under it ("on this page"). A page that has an outline of its own (the dashboard, fumola.org, adapton.org: `nav#outline`) has it taken into the bar there, so the page's own script still keeps it current. A page not in the outline (a dated work note) is shown under the entry whose address starts its own.
- **The connection to related pages:** a dot (•) on each entry this page links to.
- **Long lists:** past nine entries a branch shows the first nine and "N more…", unless this page is among them.
- **Matching addresses:** by origin and path, without `index.html` or the trailing slash, ignoring case. While trying it locally, `LOCAL` maps a local server to its site (8136 and 8137 for matthewhammer.org, 8765 for adapton.org, 8766 to 8769 for fumola.org).

## The controls

- **The theme:** three buttons at the top, ◐ auto (the system's), ☀ light, ☾ dark. Kept per site in `localStorage["theme"]`: absent for auto, `light` or `dark`. It is the same key and the same `data-theme` attribute as the earlier switch (`~/hazel-perf-report/tools/add_theme.py`), whose ◐ pill the bar hides where both are on a page.
- **Wider or narrower:** drag the bar's right edge (`--cnav-w`, kept as `localStorage["cnav-w"]`); a double-click puts it back to 15rem.
- **Hide and show:** « at its top right hides it, » brings it back (`localStorage["cnav-hidden"]`). On an app's page the choice lasts the visit only (`sessionStorage["cnav-app-shown"]`).
- **A narrow screen** (under 48rem): the bar is behind ☰, and opens over the page, full width; a link chosen in it closes it.
- **Print:** the bar is left out.

## Adding or moving a page

1. Put it in `OUTLINE`, under the page that leads to it, with its title as its page names itself.
2. Run `python3 nav/add_nav.py <page>` on it, in its own repository (`--app`, `--own` as above).
3. Push the homepage repository for nav.js, and the page's own repository for the page. Pull first: the dashboard's publishing commits to the homepage's `master` by itself, so the checkout is often behind.
4. Check it: `python3 nav/check_nav.py` crawls the three sites from their front pages and lists every reachable page without the bar or without the choice.

## Pages written by scripts

A script that writes a page must write the include too, or the page loses the bar the next time it runs. Each of these writes it, or runs `add_nav.py`, on what it writes. Keep the list current:

- the work dashboard and its closed tickets: `~/hazel-work-notes/dashboard/update.py` (published by `site_publish.py`);
- the work notes' gallery, `/hazel/work-notes/`: `~/hazel-work-notes/dashboard/notes_gallery.py`;
- the Hazel performance analysis: `~/hazel-perf-report/tools/add_theme.py`, which every report runs through, puts the nav block in with the theme;
- the Dress Up reports: each report's `drivers/` script in `~/kids-choice-games/dress-up/reports/NNN/`.

## Left out on purpose

- The dashboard's dated snapshots (`hazel-work-notes/dashboard-snapshots/`): records of the page as it was. Matthew: "especially not the snapshots".
- Pages no active page links to, such as the old course pages under `courses/`: "the ones that are not reachable from those active pages need not be updated".
