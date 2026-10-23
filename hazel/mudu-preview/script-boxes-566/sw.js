// Messed Up Dress Up. Copyright 2026 Matthew Hammer and family (M and N). Reuse welcome, with credit: see LICENSE and NOTICE.
// SPDX-License-Identifier: Apache-2.0
// (ticket 486, Matthew: "hardening MUDU against lack of internet on a tablet, desktop, or mobile phone"; "On a mobile phone or
// tablet, I'd want a way to download the JS and have it run from the local storage on the device") The game keeps a copy of
// itself on the device. One visit with the Internet saves every file it needs; after that it opens with none, in airplane mode.
//   the game's page        the network if it answers within a few seconds, else the copy kept here
//   its parts (?v= names)  the copy kept here, else the network (each version names new parts, so a copy is never stale)
//   the rest of this site  the network, else the copy kept here (the nav bar, pictures, version.json)
// Nothing here changes with a version: a new version's page names new parts, they are fetched and kept, and the old ones go.
const CACHE = "dress-up";
const SCOPE = new URL("./", self.location).href;
const PAGE = SCOPE;   // (the page is kept under one name, however it was asked for: ./, index.html, #inspect)
const ALSO = ["version.json", "manifest.webmanifest", "icons/icon-192.png", "icons/icon-512.png", "icons/apple-touch-icon.png"].map(p => new URL(p, SCOPE).href);
const WAIT = 4000;   // (a page that takes longer than this, on a weak connection, comes from the copy instead)

const isPage = url => url.origin === location.origin && (url.href.split(/[?#]/)[0] === SCOPE || url.href.split(/[?#]/)[0] === SCOPE + "index.html");

// (the files the page names: its parts, its stylesheet, and the nav bar from this same site; not data: pictures, not other sites)
function named(html) {
  const out = new Set(ALSO);
  for (const m of html.matchAll(/\b(?:src|href)="([^"#]+)"/g)) {
    if (m[1].startsWith("data:")) continue;
    const u = new URL(m[1], SCOPE);
    if (u.origin === location.origin) out.add(u.href);
  }
  return out;
}

// Keep every file this page names, and drop the parts of other versions.
async function keep(html) {
  const c = await caches.open(CACHE), want = named(html), have = await c.keys();
  const got = new Set(have.map(r => r.url));
  await Promise.all([...want].filter(u => !got.has(u)).map(async u => {
    try { const r = await fetch(u, { cache: "no-cache" }); if (r.ok) await c.put(u, r); } catch (_) { /* the next visit tries again */ }
  }));
  await Promise.all(have.filter(r => new URL(r.url).searchParams.has("v") && !want.has(r.url)).map(r => c.delete(r)));
}

self.addEventListener("install", e => e.waitUntil((async () => {
  try {
    const r = await fetch(PAGE, { cache: "no-cache" });
    if (r.ok) { const c = await caches.open(CACHE); await c.put(PAGE, r.clone()); await keep(await r.text()); }
  } catch (_) { /* installed while offline: kept as the page is visited */ }
  await self.skipWaiting();
})()));

self.addEventListener("activate", e => e.waitUntil(self.clients.claim()));

const late = ms => new Promise(res => setTimeout(() => res(null), ms));

async function page(e) {
  const c = await caches.open(CACHE);
  const net = fetch(e.request).then(async r => {
    if (r.ok) { await c.put(PAGE, r.clone()); e.waitUntil(r.clone().text().then(keep)); }
    return r;
  }).catch(() => null);
  const first = await Promise.race([net, late(WAIT)]);
  if (first && first.ok) return first;
  return (await c.match(PAGE)) || (await net) || Response.error();
}

async function part(e) {
  const c = await caches.open(CACHE), kept = await c.match(e.request);
  if (kept) return kept;
  const r = await fetch(e.request);
  if (r.ok) e.waitUntil(c.put(e.request, r.clone()));
  return r;
}

async function rest(e) {
  const c = await caches.open(CACHE);
  try {
    const r = await Promise.race([fetch(e.request), late(WAIT).then(() => { throw new Error("slow"); })]);
    if (r.ok) e.waitUntil(c.put(e.request, r.clone()));
    return r;
  } catch (_) {
    return (await c.match(e.request, { ignoreSearch: true })) || Response.error();
  }
}

self.addEventListener("fetch", e => {
  const url = new URL(e.request.url);
  if (e.request.method !== "GET" || url.origin !== location.origin) return;   // (other sites: the browser's own, as without this)
  if (url.searchParams.has("t")) return;   // (the update check asks the network itself, and says nothing when offline)
  if (e.request.mode === "navigate") { if (isPage(url)) e.respondWith(page(e)); return; }   // (a report page: the network's)
  e.respondWith(url.searchParams.has("v") ? part(e) : rest(e));
});
