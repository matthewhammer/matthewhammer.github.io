/* The constellation's nav bar (ticket 296, Matthew, 2026-10-18: "each page we can reach should have a nav bar on the
   LHS, with drag controls, and a meaningful connection in terms of that LHS to related (linked) pages. ideally, the nav
   bar would be a single outline, with different parts projected out for each page we can reach in this constellation").

   One outline of every page reachable from matthewhammer.org, fumola.org and adapton.org (OUTLINE, below), and each
   page shows its own part of it: the four sites always; the branch the page is in opened out (its parents, its
   siblings, its own children); the page itself marked, with its own headings under it ("on this page"); and a dot on
   each entry this page links to. Beside the page: drag its right edge to make it wider or narrower (double-click: as
   it was), remembered on this site; « to hide it and » to bring it back, for the visit (ticket 314: a « once on any page
   of a site, remembered for the site, left every page of it starting hidden: "on matthewhammer.org by default, the LHS nav
   bar is hidden. on all other pages, it shows up by default"); on a narrow screen it is behind ☰.
   At its top: the theme, ◐ auto (the system's), ☀ light or ☾ dark, remembered on this site as localStorage "theme"
   (the same key and data-theme attribute as the earlier switch, hazel-perf-report/tools/add_theme.py).

   A page includes it with one line in its <head>, before its own styles paint:
     <script src="https://matthewhammer.org/nav/nav.js"></script>
   A page whose CSS answers :root[data-theme="light"|"dark"] says so with <meta name="theme-support" content="data-theme">;
   a page that keeps its own colours whatever is chosen (the game) says <meta name="theme-support" content="own">; any other
   page gets a plain dark style from here when dark is chosen (FALLBACK_DARK). A full-window app (the game, a Hazel build)
   says <meta name="nav" content="hidden">: there the bar starts hidden, behind », each visit, and shown only once asked.
   How to add it to a page, and keep the outline current: nav/agent-instructions.md. */
(() => {
  "use strict";
  const H = "https://matthewhammer.org", F = "https://fumola.org", A = "https://adapton.org", K = H + "/kids-choice-games/dress-up/reports/";
  // [title, url, children]
  const OUTLINE = [
    ["Matthew Hammer", H + "/", [
      ["Publications and drafts", H + "/pubs.html", [
        ["Adapton (PLDI 2014)", H + "/adapton"],
        ["Implicit self-adjusting computation (JFP 2014)", H + "/jfp2014/"],
        ["Knowledge inference for secure computation (PLAS 2013)", H + "/plas2013"],
        ["Self-adjusting stack machines (OOPSLA 2011)", H + "/oopsla11"],
        ["Implicit self-adjusting computation (ICFP 2011)", H + "/icfp11"],
        ["Self-adjusting machines (thesis)", H + "/selfadjmachines"],
        ["CEAL (PLDI 2009)", H + "/pldi09", [
          ["CEAL manual", H + "/pldi09/manual/", [
            ["Example: add", H + "/pldi09/manual/ex_add.html"],
            ["Example: arithmetic tree", H + "/pldi09/manual/ex_arith_tree.html"],
            ["Example: the whole tree", H + "/pldi09/manual/ex_arith_tree_full.html"],
            ["Example: modifiable lists", H + "/pldi09/manual/ex_modlist.html"],
            ["The language's core", H + "/pldi09/manual/group__langcore.html"],
            ["The meta language", H + "/pldi09/manual/group__langmeta.html"]]]]],
        ["Memory management for self-adjusting computation (ISMM 2008)", H + "/ismm08"]]],
      ["CV (PDF)", H + "/hammer-cv.pdf"]]],
    ["Hazel", H + "/hazel/", [
      ["Hazel reports", H + "/hazel/reports/", [
        "Slider Responsiveness", "Dress Up in Hazel", "Requests from M and N", "The Witch in Hazel", "Worker and Page", "Performance in CI",
        "Five Minutes to a Picture", "One Slider, Three Ways", "Names and Reuse", "What a Slider Touches", "Naming What a Slider Leaves Alone",
        "Moving a Group, Not Its Points", "Telling the Request What Changed", "What the Page Keeps", "Dress Up and Hazel Today", "Two Fixes for a Drag",
        "Merging the Performance PRs", "Why #2635 Fails CI", "Tiles Across the Hazel UI", "Agenda for October 21",
        "Correctness of the Performance PRs", "Week in Hazel's Credits", "#2635 Is Green", "Architecture Around #2651", "What a Call into Hazel Costs",
        "Does #2681 Need #2661?", "The World Rule in Hazel and in Fumola", "Hazel and Fumola, Side by Side", "Where the Gap Comes From",
        "Game Scripts in Hazel, Two Days In", "Reviews Still Open", null, "Game Scripts in Hazel, a Case Study", "An Official Hazel JS Library?"
      ].map((t, i) => [i + 1, t]).filter(([, t]) => t)   // (null: a number whose report is not published yet)
        .map(([n, t]) => [`${n}. ${t}`, `${H}/hazel/reports/${String(n).padStart(3, "0")}/`])],
      ["Hazel Work Dashboard", H + "/hazel/dashboard/", [["Closed tickets", H + "/hazel/dashboard/closed.html"]]],
      ["Work notes", H + "/hazel/work-notes/"],
      ["Crash log", H + "/hazel-work-notes/crash-log.html"],
      ["Messed Up Dress Up", H + "/hazel/dress-up/", [
        ["▶ Play Messed Up Dress Up", H + "/kids-choice-games/dress-up/"],
        ["Dress Up reports", H + "/hazel/dress-up/reports/", [
          "60 Frames a Second", "The Kids' Computers", "Slow Until Reloaded", "One Folder, Every Run", "Always Be Knolling",
          "Then and Now", "Stories and Stress", "Piece by Piece", "One Size, Every View", "Pose by Pose",
          "A Cast of Friends", "The Foam, Not the Makers", "Dress Up's Hardest Parts for Hazel", "Posing the Body", "Seen Before It Was Built",
          "The Space Between the Boxes", "Game Scripts: a Player's Guide", "Story Scripts: a Director's Guide", "Ready to Re-mix", "A Way In", "Dungeons, From Above", "How Dress Up Is Made"
        ].map((t, i) => [i + 1, t]).filter(([, t]) => t)   // (null: a number whose report is not published yet)
          .map(([n, t]) => [`${n}. ${t}`, `${K}${String(n).padStart(3, "0")}/`]).concat([["The series' own index", K]])],
        ["Dress Up pictures", H + "/hazel/dress-up/pictures.html", [
          ["Every picture, run by run", H + "/dress-up-pictures/"], ["Every picture, by day (to 2026-10-18)", H + "/dress-up-pictures-2026-10-18/"]]],
        ["Dress Up music", H + "/hazel/dress-up/music.html", [["Every tune, run by run", H + "/dress-up-music/"]]]]]]],
    ["Fumola", F + "/", [
      ["Press", F + "/press/"], ["Alignment", F + "/alignment/"], ["Web play", F + "/web-play/"],
      ["Fumola-Hazel integration", F + "/hazel-integration/"], ["Hazel, with Fumola (a build)", F + "/hazel/experimental-lang-integration/"],
      ["mergeSort, drawn as a graph", H + "/replayground/mergeSort44.html"]]],
    ["Adapton", A + "/", [["Adapton Lab's results", A + "/lab-results/"]]],
  ];
  // (for trying it out locally: a local server stands for its site)
  const LOCAL = { "localhost:8000": H + "/hazel/dashboard", "localhost:8136": H, "127.0.0.1:8136": H, "localhost:8137": H, "localhost:8765": A, "localhost:8766": F, "localhost:8767": F, "localhost:8768": F, "localhost:8769": F };
  const key = u => { try { const x = new URL(u, location.href), site = LOCAL[x.host] || x.origin.replace("://www.", "://");
    return (site + x.pathname).replace(/\/index\.html$/, "/").replace(/\/$/, "").toLowerCase(); } catch (_) { return ""; } };

  // ---- the theme: set before the page paints ----
  const root = document.documentElement, store = { get: k => { try { return localStorage.getItem(k); } catch (_) { return null; } },
    set: (k, v) => { try { v == null ? localStorage.removeItem(k) : localStorage.setItem(k, v); } catch (_) {} } };
  const mode = () => ["light", "dark"].includes(store.get("theme")) ? store.get("theme") : "auto";
  const applyTheme = () => { const m = mode(); m === "auto" ? root.removeAttribute("data-theme") : root.setAttribute("data-theme", m);
    root.style.colorScheme = m === "auto" ? "light dark" : m; };
  applyTheme();
  addEventListener("storage", e => { if (e.key === "theme") { applyTheme(); themeButtons(); } });

  const CSS = `
  :root { --cnav-w: 15rem; }
  #cnav { position: fixed; top: 0; left: 0; bottom: 0; width: var(--cnav-w); box-sizing: border-box; overflow-y: auto; overscroll-behavior: contain;
    padding: 0.9rem 0.6rem 3rem 0.85rem; background: Canvas; color: CanvasText; border-right: 1px solid color-mix(in srgb, CanvasText 18%, transparent);
    font: 13px/1.4 system-ui, -apple-system, "Segoe UI", sans-serif; z-index: 2147483000; scrollbar-width: thin; text-align: left; }
  #cnav * { box-sizing: border-box; }
  #cnav .cn-top { display: flex; align-items: center; gap: 4px; margin: 0 1.8rem 0.7rem 0; flex-wrap: wrap; }
  #cnav .cn-theme button { font: inherit; font-size: 12px; padding: 1px 7px; border-radius: 999px; border: 1px solid color-mix(in srgb, CanvasText 25%, transparent);
    background: Canvas; color: GrayText; cursor: pointer; }
  #cnav .cn-theme button[aria-pressed="true"] { color: CanvasText; border-color: CanvasText; font-weight: 600; }
  #cnav ul { list-style: none; margin: 0; padding: 0; }
  #cnav ul ul { margin-left: 0.55rem; padding-left: 0.5rem; border-left: 1px solid color-mix(in srgb, CanvasText 15%, transparent); }
  #cnav li { margin: 0; }
  #cnav .cn-row { display: flex; align-items: baseline; gap: 2px; }
  #cnav .cn-tw { flex: none; width: 1.1em; padding: 0; border: 0; background: none; color: GrayText; cursor: pointer; font: inherit; line-height: 1.4; }
  #cnav .cn-tw:hover { color: CanvasText; }
  #cnav .cn-tw.cn-leaf { visibility: hidden; cursor: default; }
  #cnav a { display: block; flex: 1; min-width: 0; padding: 1px 4px; border-radius: 4px; color: inherit; text-decoration: none; overflow-wrap: anywhere; }
  #cnav a:hover { background: color-mix(in srgb, CanvasText 8%, transparent); }
  #cnav li.cn-site > .cn-row > a { font-weight: 700; }
  #cnav a[aria-current="page"] { background: color-mix(in srgb, #2563eb 16%, Canvas); font-weight: 600; }
  #cnav a.cn-linked::after { content: " •"; color: #2563eb; }
  #cnav .cn-on { margin: 2px 0 6px 1.4rem; padding-left: 0.5rem; border-left: 2px solid #2563eb; }
  #cnav .cn-on a { color: GrayText; font-size: 12px; } #cnav .cn-on a:hover { color: CanvasText; }
  #cnav .cn-more { color: GrayText; font-style: italic; }
  #cnav .cn-label { color: GrayText; font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em; margin: 0.9rem 0 0.25rem; }
  #cnav-grip { position: fixed; top: 0; bottom: 0; left: calc(var(--cnav-w) - 3px); width: 7px; cursor: col-resize; z-index: 2147483001; touch-action: none; }
  #cnav-grip:hover, #cnav-grip.cn-drag { background: color-mix(in srgb, CanvasText 20%, transparent); }
  .cn-btn { position: fixed; top: 0.55rem; z-index: 2147483002; width: 1.9rem; height: 1.9rem; display: flex; align-items: center; justify-content: center;
    padding: 0; border-radius: 5px; border: 1px solid color-mix(in srgb, CanvasText 22%, transparent); background: Canvas; color: GrayText; cursor: pointer; font: 15px/1 system-ui, sans-serif; }
  .cn-btn:hover { color: CanvasText; }
  #cnav-hide { left: calc(var(--cnav-w) - 2.45rem); }
  #cnav-show, #cnav-open { left: 0.55rem; display: none; }
  html.cnav-on body { padding-left: var(--cnav-w) !important; }
  html.cnav-on body > #outline, html.cnav-on .outline-toggle, html.cnav-on .outline-grip, html.cnav-on .outline-hide, html.cnav-on .outline-show { display: none !important; }
  #cnav #outline { position: static !important; display: block !important; width: auto !important; height: auto !important; padding: 0 !important; margin: 2px 0 6px 1.4rem !important;
    border: 0 !important; border-left: 2px solid #2563eb !important; background: none !important; overflow: visible !important; font: inherit !important; z-index: auto !important; }
  #cnav #outline > ul { border-left: 0 !important; margin: 0 0 0.4rem 0 !important; padding-left: 0.5rem !important; }
  #cnav #outline .outline-pages, #cnav #outline .outline-label { display: none !important; }
  #cnav #outline a { font-size: 12px !important; padding: 1px 4px !important; }
  /* (2026-10-25, Matthew on ^report(33)'s outline here: "indentation on LHS is too extreme --- but let's keep some more minor
     version") a page's own numbered outline: a small step in per level, not the browser's 40px, and its links inline, so a
     line's subtitle and its "·" run on after the link rather than each on a line of its own */
  #cnav #outline ol { margin: 0 !important; padding-left: 1.3em !important; }
  #cnav #outline > ol { padding-left: 1.6em !important; }
  #cnav #outline ol li { margin: 1px 0 !important; font-size: 12px !important; line-height: 1.35 !important; }
  #cnav #outline ol li > a { display: inline !important; padding: 0 2px !important; }
  #cnav #outline > p { margin: 0 0 2px 0.5rem !important; font-size: 13px !important; }
  html.cnav-on main, html.cnav-off main { margin-left: auto !important; }   /* (ticket 348: folded too, the page's own outline margin is not kept for a bar that is gone) */
  html.cnav-off.cnav-widen main { max-width: 54rem !important; }   /* (ticket 348, Matthew: "use that margin": a narrow column widens into the room the folded bar leaves) */
  html.cnav-on .theme-toggle, html.cnav-off .theme-toggle { display: none !important; }   /* (add_theme.py's pill: the bar's choice replaces it) */
  html.cnav-off #cnav, html.cnav-off #cnav-grip, html.cnav-off #cnav-hide { display: none; }
  html.cnav-off #cnav-show { display: flex; }
  html.cnav-off body { padding-left: 2.8rem !important; }
  html.cnav-app.cnav-off body { padding-left: 0 !important; }   /* (an app keeps the whole window; its » waits at the bottom left) */
  html.cnav-app #cnav-show { top: auto; bottom: 0.55rem; }
  body.cn-dragging { cursor: col-resize; user-select: none; }
  @media (max-width: 47.99rem) {
    html.cnav-on body, html.cnav-off body { padding-left: 0 !important; padding-top: 2.6rem; }
    #cnav, #cnav-grip, #cnav-hide, #cnav-show { display: none !important; }
    #cnav-open { display: flex; }
    html.cnav-open #cnav { display: block !important; width: auto; right: 0; font-size: 15px; padding-top: 3.2rem; }
  }
  @media print { #cnav, #cnav-grip, .cn-btn { display: none !important; } html.cnav-on body { padding-left: 0 !important; } }`;
  // (a page whose CSS has no answer to data-theme: a plain dark style, only when dark is chosen or the system's is dark on auto)
  // (the nav's own id inside each :not(), so these outweigh a page's own colours, and never touch the nav)
  const DARK = [["body", "background: #16181d !important; color: #e3e1de !important"],
    ["body :where(*):not(svg, svg *, img, video, canvas, iframe, #cnav, #cnav *, .cn-btn, #cnav-grip)", "color: inherit !important; background-color: transparent !important; border-color: #3a404a !important; text-shadow: none !important"],
    ["body a:not(#cnav a, .cn-btn)", "color: #8ab4f8 !important"], ["body a:visited:not(#cnav a, .cn-btn)", "color: #c58af9 !important"],
    ["body :is(pre, code, tt, kbd, samp, .fragment, .memitem, .memproto, .memdoc):not(#cnav *)", "background-color: #22262d !important"]];
  const darkCss = p => DARK.map(([sel, d]) => `${p} ${sel} { ${d}; }`).join("\n");
  const FALLBACK_DARK = `\n:root[data-theme="dark"] { color-scheme: dark; }\n${darkCss(':root[data-theme="dark"]')}\n@media (prefers-color-scheme: dark) {\n${darkCss(":root:not([data-theme])")}\n}`;
  let themeButtons = () => {};
  const build = () => {
    if (document.getElementById("cnav")) return;
    const own = document.querySelector('meta[name="theme-support"]');
    const st = document.createElement("style"); st.id = "cnav-style"; st.textContent = CSS + (own ? "" : FALLBACK_DARK); document.head.append(st);
    const here = key(location.href);
    // (what this page links to, as outline keys: those entries get a dot)
    const linked = new Set([...document.querySelectorAll("a[href]")].map(a => key(a.href)));
    // the outline's entries, with their parents; this page's entry, or else the one whose address is the longest start of this page's
    const nodes = [], walk = (list, parent, depth) => list.forEach(([t, u, kids]) => { const n = { t, u, k: key(u), parent, depth, kids: [] };
      nodes.push(n); if (parent) parent.kids.push(n); if (kids) walk(kids, n, depth + 1); });
    walk(OUTLINE, null, 0);
    let cur = nodes.find(n => n.k === here) || null, extra = false;
    if (!cur) { cur = nodes.filter(n => here.startsWith(n.k + "/")).sort((a, b) => b.k.length - a.k.length)[0] || null; extra = !!cur; }
    const open = new Set(); for (let n = cur; n; n = n.parent) open.add(n);
    // (a page's own outline, as the dashboard's and fumola.org's: taken in as its "on this page", its own script still keeping it)
    const ownOutline = document.querySelector("nav#outline, #outline.outline");
    const headings = ownOutline ? [] : [...document.querySelectorAll("main h2, main h3, article h2, article h3, body > h2, body > h3, .contents h2")].filter(h => !h.closest("#cnav"));
    const el = (tag, attrs = {}, kids = []) => { const e = document.createElement(tag); for (const [a, v] of Object.entries(attrs)) e.setAttribute(a, v); e.append(...[].concat(kids)); return e; };
    const onPage = () => { if (!headings.length) return null; const ul = el("ul", { class: "cn-on" });
      for (const h of headings.slice(0, 40)) { if (!h.id) h.id = "cn-" + (h.textContent || "").trim().toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "").slice(0, 40);
        ul.append(el("li", {}, el("a", { href: "#" + h.id }, (h.textContent || "").replace(/#\s*$/, "").trim()))); }
      return ul; };
    const MANY = 9;
    const list = (ns) => { const ul = el("ul"); let shown = ns;
      const curIn = ns.findIndex(n => open.has(n));
      if (ns.length > MANY + 2) shown = ns.filter((n, i) => i < MANY || i === curIn);
      for (const n of shown) ul.append(item(n));
      if (shown.length < ns.length) { const more = el("li", {}, el("a", { href: "#", class: "cn-more" }, `${ns.length - shown.length} more…`));
        more.firstChild.addEventListener("click", e => { e.preventDefault(); ul.replaceWith(listAll(ns)); }); ul.append(more); }
      return ul; };
    const listAll = ns => { const ul = el("ul"); ns.forEach(n => ul.append(item(n))); return ul; };
    const item = n => {
      const li = el("li", n.depth === 0 ? { class: "cn-site" } : {}), a = el("a", { href: n.u }, n.t), tw = el("button", { type: "button", class: "cn-tw" + (n.kids.length ? "" : " cn-leaf"), "aria-label": "open" }, "▸");
      if (n === cur && !extra) a.setAttribute("aria-current", "page");
      if (linked.has(n.k) && n !== cur) { a.classList.add("cn-linked"); a.title = "linked from this page"; }
      li.append(el("div", { class: "cn-row" }, [tw, a]));
      let sub = null;
      const show = on => { tw.textContent = on ? "▾" : "▸"; tw.setAttribute("aria-expanded", String(on)); tw.setAttribute("aria-label", on ? "close" : "open");
        if (on && !sub) { sub = list(n.kids); li.append(sub); } if (sub) sub.hidden = !on; };
      if (n.kids.length) { show(open.has(n)); tw.addEventListener("click", () => show(tw.getAttribute("aria-expanded") !== "true")); }
      if (n === cur) { if (extra) { const t = el("ul"); t.append(el("li", {}, el("div", { class: "cn-row" }, [el("span", { class: "cn-tw cn-leaf" }, "▸"),
          el("a", { href: location.href, "aria-current": "page" }, document.title || "this page")]))); li.append(t); }
        const on = ownOutline || onPage(); if (on) li.querySelector(".cn-row").after(on); }   // (right under this page's entry, before its children)
      return li;
    };
    const nav = el("nav", { id: "cnav", "aria-label": "Matthew Hammer's pages" });
    const theme = el("div", { class: "cn-theme", role: "group", "aria-label": "Theme" }, [["auto", "◐ auto"], ["light", "☀ light"], ["dark", "☾ dark"]].map(([m, t]) =>
      { const b = el("button", { type: "button", "data-mode": m, title: { auto: "the system's choice", light: "light", dark: "dark" }[m] }, t);
        b.addEventListener("click", () => { store.set("theme", m === "auto" ? null : m); applyTheme(); themeButtons(); }); return b; }));
    themeButtons = () => theme.querySelectorAll("button").forEach(b => b.setAttribute("aria-pressed", String(b.dataset.mode === mode())));
    themeButtons();
    nav.append(el("div", { class: "cn-top" }, theme), (() => { const ul = el("ul"); OUTLINE.forEach((_, i) => ul.append(item(nodes.filter(n => n.depth === 0)[i]))); return ul; })());
    const grip = el("div", { id: "cnav-grip", role: "separator", "aria-orientation": "vertical", title: "Drag to make it wider or narrower (double-click: as it was)" });
    const hide = el("button", { type: "button", id: "cnav-hide", class: "cn-btn", title: "Hide the pages", "aria-label": "Hide the pages" }, "«");
    const showB = el("button", { type: "button", id: "cnav-show", class: "cn-btn", title: "Show the pages", "aria-label": "Show the pages" }, "»");
    const openB = el("button", { type: "button", id: "cnav-open", class: "cn-btn", title: "Pages", "aria-label": "Pages", "aria-expanded": "false" }, "☰");
    document.body.prepend(nav, grip, hide, showB, openB);
    // (ticket 348) a page whose column is narrower than 54rem widens to it while the bar is folded; a wider one keeps its own
    { const mainEl = document.querySelector("main"), rem = parseFloat(getComputedStyle(root).fontSize) || 16, mw = mainEl && parseFloat(getComputedStyle(mainEl).maxWidth);
      if (mw && mw < 54 * rem) root.classList.add("cnav-widen"); }
    // ---- the drag controls, remembered on this site ----
    const W = store.get("cnav-w"); if (W) root.style.setProperty("--cnav-w", W);
    // (an app's page starts hidden, and its choice lasts the visit (sessionStorage), never the site's)
    const app = (document.querySelector('meta[name="nav"]') || {}).content === "hidden";
    root.classList.toggle("cnav-app", app);
    const visit = { get: () => { try { return sessionStorage.getItem("cnav-app-shown"); } catch (_) { return null; } },
      set: v => { try { v ? sessionStorage.setItem("cnav-app-shown", "1") : sessionStorage.removeItem("cnav-app-shown"); } catch (_) {} } };
    // (ticket 314) any other page's « lasts the visit too (sessionStorage), so every visit starts with the bar shown; the site-wide
    // choice it once was is forgotten (it hid every page of matthewhammer.org after one «)
    const hid = { get: () => { try { return sessionStorage.getItem("cnav-hidden") === "1"; } catch (_) { return false; } },
      set: v => { try { v ? sessionStorage.setItem("cnav-hidden", "1") : sessionStorage.removeItem("cnav-hidden"); } catch (_) {} } };
    store.set("cnav-hidden", null);
    const setOff = off => { root.classList.toggle("cnav-off", off); root.classList.toggle("cnav-on", !off);
      if (app) visit.set(!off); else hid.set(off); };
    setOff(app ? visit.get() !== "1" : hid.get());
    hide.addEventListener("click", () => setOff(true)); showB.addEventListener("click", () => setOff(false));
    openB.addEventListener("click", () => { const o = !root.classList.contains("cnav-open"); root.classList.toggle("cnav-open", o); openB.setAttribute("aria-expanded", String(o)); openB.textContent = o ? "✕" : "☰"; });
    nav.addEventListener("click", e => { if (e.target.closest("a") && root.classList.contains("cnav-open")) openB.click(); });
    grip.addEventListener("pointerdown", e => { e.preventDefault(); grip.setPointerCapture(e.pointerId); grip.classList.add("cn-drag"); document.body.classList.add("cn-dragging"); });
    grip.addEventListener("pointermove", e => { if (!grip.hasPointerCapture(e.pointerId)) return;
      const px = Math.max(160, Math.min(innerWidth * 0.5, e.clientX)); root.style.setProperty("--cnav-w", px + "px"); });
    const done = e => { if (!grip.hasPointerCapture(e.pointerId)) return; grip.releasePointerCapture(e.pointerId); grip.classList.remove("cn-drag");
      document.body.classList.remove("cn-dragging"); store.set("cnav-w", root.style.getPropertyValue("--cnav-w")); };
    grip.addEventListener("pointerup", done); grip.addEventListener("pointercancel", done);
    grip.addEventListener("dblclick", () => { root.style.removeProperty("--cnav-w"); store.set("cnav-w", null); });
    const a = nav.querySelector('a[aria-current="page"]'); if (a) nav.scrollTop = Math.max(0, a.getBoundingClientRect().top - nav.getBoundingClientRect().top - nav.clientHeight / 3);
  };
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", build); else build();
})();
