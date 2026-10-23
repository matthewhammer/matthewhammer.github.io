# Fumola's wasm build, pinned: the semantics fixes

`fumola_wasm.js` and `fumola_wasm_bg.wasm` as GitHub built them (Adapton/fumola, run 39962087561, its `fumola-wasm` artifact) from
branch `bench/571-semantics` at `b91650fb33`: the code of branch `semantics-571` at `877b155851` (^ticket(571)) and a measuring-only
workflow. Not on Adapton/fumola's `main` yet: its pull request waits on Matthew's word. sha256: `fumola_wasm.js` 1aba590410185e96…,
`fumola_wasm_bg.wasm` bc2e9eeb55c9f3d6….

What it fixes, from ^report(28)'s traps: text `!=` (was inverted), literal patterns of every kind (text, Bool, Int, Float: only
`()`, `null` and Nats matched), whole numbers kept one way (0 and up a Nat, below 0 an Int) with every operation and comparison
defined between them, `<` `<=` `>` `>=` on Floats and on text, and escapes in text literals. Loaded by `dress-up/js/25-fumola.js`
(`FUMOLA_BUILD`). The build before it, `vendor/fumola/2ec634b5df/`, stays for comparison.
