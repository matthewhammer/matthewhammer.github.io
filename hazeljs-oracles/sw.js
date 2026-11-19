// (2026-11-19) The oracles moved from /hazeljs-oracles/ to /hazeljs-examples/. This worker takes the place of the one that
// kept them for use with no connection here: it clears what that one saved (its caches, named oracles-*), removes itself,
// and sends each page it had open to the same page at the new address.
self.addEventListener("install", () => self.skipWaiting());
self.addEventListener("activate", e => e.waitUntil((async () => {
  for (const k of await caches.keys()) if (k.startsWith("oracles-")) await caches.delete(k);
  await self.registration.unregister();
  for (const c of await self.clients.matchAll({ type: "window" })) c.navigate(c.url.replace("/hazeljs-oracles/", "/hazeljs-examples/"));
})()));
