// outline.js - fumola.org's page outline and heading anchors, as one file
// any page on this site can load (defer).

// Heading anchors. They are real links, so they work with JavaScript off;
// this only adds the clipboard copy on top.
document.addEventListener("click", (e) => {
  const a = e.target.closest(".anchor");
  if (!a) return;
  const url = location.origin + location.pathname + new URL(a.href).hash;
  const done = () => {
    a.setAttribute("data-copied", "");
    setTimeout(() => a.removeAttribute("data-copied"), 1400);
  };
  const fallback = () => {
    const t = document.createElement("textarea");
    t.value = url;
    t.setAttribute("readonly", "");
    t.style.cssText = "position:fixed;top:-9999px;opacity:0";
    document.body.appendChild(t);
    t.select();
    let okay = false;
    try { okay = document.execCommand("copy"); } catch (_) {}
    t.remove();
    if (okay) done();
  };
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(url).then(done, fallback);
  } else {
    fallback();
  }
});

// ---- The floating outline -------------------------------------------
// Built from the document rather than written out, so the outline and
// the page cannot disagree: a section that exists is in it, a section
// that is renamed is renamed here too, and adding one is nothing but
// adding it. The only thing hand-kept is the list of sibling pages in
// the markup, which is the one fact this page does not contain.
(function () {
  const nav = document.getElementById("outline");
  if (!nav) return;
  const list = nav.querySelector("#outline-list");
  if (!list) return;

  // The heading text without the trailing "#" that copies its link.
  const label = (h) => {
    const c = h.cloneNode(true);
    c.querySelectorAll(".anchor").forEach((a) => a.remove());
    return c.textContent.trim();
  };

  // h2 for a section, h3 for the named subsections inside it. Headings
  // without an id are skipped rather than given one: an outline entry
  // that cannot be linked to is a dead row.
  const entries = [];
  document.querySelectorAll("main section[id]").forEach((section) => {
    const h2 = section.querySelector("h2");
    if (h2) entries.push({ id: section.id, text: label(h2), sub: false });
    section.querySelectorAll("h3[id]").forEach((h3) => {
      entries.push({ id: h3.id, text: label(h3), sub: true });
    });
  });
  if (!entries.length) {
    nav.remove();
    return;
  }

  const links = entries.map((e) => {
    const li = document.createElement("li");
    if (e.sub) li.className = "sub";
    const a = document.createElement("a");
    a.href = "#" + e.id;
    a.textContent = e.text;
    li.appendChild(a);
    list.appendChild(li);
    return { a, el: document.getElementById(e.id) };
  });

  // Which entry the reader is in: the last heading to have passed a
  // line a third of the way down. A band rather than the very top, so
  // the active row changes at about the moment the eye arrives at a new
  // section rather than when it touches the top edge.
  let current = null;
  const mark = () => {
    const line = window.innerHeight * 0.33;
    let found = links[0];
    for (const l of links) {
      if (!l.el) continue;
      if (l.el.getBoundingClientRect().top <= line) found = l;
      else break;
    }
    // At the very bottom the last section may never cross the line.
    if (window.scrollY + window.innerHeight >= document.body.scrollHeight - 4) {
      found = links[links.length - 1];
    }
    if (found === current) return;
    if (current) current.a.removeAttribute("aria-current");
    if (found) found.a.setAttribute("aria-current", "true");
    current = found;
    // Keep the active row visible when the outline is longer than the
    // viewport, without scrolling the page itself.
    if (found && nav.scrollHeight > nav.clientHeight) {
      const r = found.a.getBoundingClientRect();
      const n = nav.getBoundingClientRect();
      if (r.top < n.top + 8 || r.bottom > n.bottom - 8) {
        found.a.scrollIntoView({ block: "nearest" });
      }
    }
  };

  let ticking = false;
  const onScroll = () => {
    if (ticking) return;
    ticking = true;
    requestAnimationFrame(() => {
      ticking = false;
      mark();
    });
  };
  window.addEventListener("scroll", onScroll, { passive: true });
  window.addEventListener("resize", onScroll, { passive: true });
  mark();

  // ---- Opening it on a narrow screen --------------------------------
  // Below the width where the outline sits beside the page it is the
  // page instead: the button at the top left, and the whole viewport
  // when it is pressed. Escape closes it, so does choosing a section,
  // and so does widening the window past the breakpoint -- the open
  // class describes a full-screen overlay and would be wrong applied to
  // a sidebar.
  const toggle = document.getElementById("outline-toggle");
  if (toggle) {
    const SIDEBAR = window.matchMedia("(min-width: 48rem)");
    const setOpen = (open) => {
      nav.classList.toggle("is-open", open);
      document.body.classList.toggle("outline-open", open);
      toggle.setAttribute("aria-expanded", open ? "true" : "false");
      toggle.setAttribute("aria-label", open ? "Close page outline" : "Open page outline");
      if (open) nav.scrollTop = 0;
    };
    const close = () => {
      if (!nav.classList.contains("is-open")) return;
      setOpen(false);
      toggle.focus();
    };
    toggle.addEventListener("click", () => {
      setOpen(!nav.classList.contains("is-open"));
    });
    // A link is a jump within this page, so the overlay has done its job.
    nav.addEventListener("click", (e) => {
      if (e.target.closest("a") && nav.classList.contains("is-open")) setOpen(false);
    });
    document.addEventListener("keydown", (e) => {
      if (e.key === "Escape") close();
    });
    const onBreakpoint = () => { if (SIDEBAR.matches) setOpen(false); };
    if (SIDEBAR.addEventListener) SIDEBAR.addEventListener("change", onBreakpoint);
    else SIDEBAR.addListener(onBreakpoint);
  }
})();

