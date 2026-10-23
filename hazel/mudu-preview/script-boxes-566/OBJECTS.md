# Dress Up: what stands in each place

The formal list of every place's objects (ticket 99), as of 0.0.392, at the default settings, each with its picture (ticket 180). A slider that adds things (more toys, more ducks, more chairs) adds objects.

- **Depth** is the y where a thing stands on the ground, in the stage's units: the ground line is at 320, and the friend's feet can be anywhere from about 11 (far) to 406 (near).
- The friend is drawn behind every object deeper than their feet, and in front of the rest. **wall** and **floor** (ticket 284) are things hanging on a wall or lying on the floor: objects too, listed, hidden and placed as any other, but always behind the friend and in nobody's way. **front** means in front of everything: the friend is always under it (darkness, light, steam, the sign toward us).
- Flat things on the ground and the backdrop (sky, far hills, back walls and what hangs on them) are not objects. What is drawn into a place but is not an object, the audit of ticket 284, is [BAKED-IN.md](BAKED-IN.md); since 0.0.208 every object here can be placed in any place (📦 from the places).
- In the game each place shows this list in its background tab, with an 👁️ chip per object to hide or show it.
- **The pictures:** each place as it starts, the friend hidden and its animations held still; each object clipped from that picture by its box on the screen, so what stands near it shows too. Drawn by GitHub's headless Chrome.

Made by `tools/checks/objects180.mjs` (on GitHub's computers: push a branch carrying it) and `tools/objects_md.py`. Make it again after changing what stands in a place.

## 🔲 empty room

(0.0.77, ticket 153.) Nothing stands here: the friend is in front of everything. Its back wall is solid, as the imagination's is.

<img src="objects/emptyroom/place.webp" alt="🔲 empty room, as it starts" width="480">

## 🫧 bubble space

<img src="objects/bubblespace/place.webp" alt="🫧 bubble space, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| front | bubbles | <img src="objects/bubblespace/bubbles.webp" alt="bubbles" height="72"> |

## 🫧 the bubble room

<img src="objects/bubbleroom/place.webp" alt="🫧 the bubble room, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | foam, back wall | <img src="objects/bubbleroom/foam-back-wall.webp" alt="foam, back wall" height="72"> |
| wall | foam, left wall | <img src="objects/bubbleroom/foam-left-wall.webp" alt="foam, left wall" height="72"> |
| wall | foam, right wall | <img src="objects/bubbleroom/foam-right-wall.webp" alt="foam, right wall" height="72"> |
| floor | foam, floor | <img src="objects/bubbleroom/foam-floor.webp" alt="foam, floor" height="72"> |
| wall | foam, ceiling | <img src="objects/bubbleroom/foam-ceiling.webp" alt="foam, ceiling" height="72"> |
| wall | bubbles stuck out, back wall | <img src="objects/bubbleroom/bubbles-stuck-out-back-wall.webp" alt="bubbles stuck out, back wall" height="72"> |
| 255.6 | bubbles stuck out 1 | <img src="objects/bubbleroom/bubbles-stuck-out-1.webp" alt="bubbles stuck out 1" height="72"> |
| 285.5 | bubbles stuck out 2 | <img src="objects/bubbleroom/bubbles-stuck-out-2.webp" alt="bubbles stuck out 2" height="72"> |
| 315.7 | bubbles stuck out 3 | <img src="objects/bubbleroom/bubbles-stuck-out-3.webp" alt="bubbles stuck out 3" height="72"> |
| 344.5 | bubbles stuck out 4 | <img src="objects/bubbleroom/bubbles-stuck-out-4.webp" alt="bubbles stuck out 4" height="72"> |
| 374.3 | bubbles stuck out 5 | <img src="objects/bubbleroom/bubbles-stuck-out-5.webp" alt="bubbles stuck out 5" height="72"> |
| 819.7 | bubbles stuck out 6 | <img src="objects/bubbleroom/bubbles-stuck-out-6.webp" alt="bubbles stuck out 6" height="72"> |

## 💭 our imagination

(0.0.75, ticket 145: its ladders, one at each column's right, at the wall's foot. Its back wall is solid: the friend's feet stay at 271.5 or nearer.)

<img src="objects/imagination/place.webp" alt="💭 our imagination, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | glow | <img src="objects/imagination/glow.webp" alt="glow" height="72"> |
| wall | map board | <img src="objects/imagination/map-board.webp" alt="map board" height="72"> |
| 268.5 | ladder 1 | <img src="objects/imagination/ladder-1.webp" alt="ladder 1" height="72"> |
| 268.5 | ladder 2 | <img src="objects/imagination/ladder-2.webp" alt="ladder 2" height="72"> |
| 268.5 | ladder 3 | <img src="objects/imagination/ladder-3.webp" alt="ladder 3" height="72"> |
| 268.5 | ladder 4 | <img src="objects/imagination/ladder-4.webp" alt="ladder 4" height="72"> |
| 268.5 | ladder 5 | <img src="objects/imagination/ladder-5.webp" alt="ladder 5" height="72"> |
| 268.5 | ladder 6 | <img src="objects/imagination/ladder-6.webp" alt="ladder 6" height="72"> |
| 268.5 | ladder 7 | <img src="objects/imagination/ladder-7.webp" alt="ladder 7" height="72"> |
| 268.5 | ladder 8 | <img src="objects/imagination/ladder-8.webp" alt="ladder 8" height="72"> |
| 268.5 | ladder 9 | <img src="objects/imagination/ladder-9.webp" alt="ladder 9" height="72"> |

## 🌲 forest

<img src="objects/forest/place.webp" alt="🌲 forest, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | sunbeams | <img src="objects/forest/sunbeams.webp" alt="sunbeams" height="72"> |
| 234.1 | pines, far row | <img src="objects/forest/pines-far-row.webp" alt="pines, far row" height="72"> |
| 277.1 | pines, near row | <img src="objects/forest/pines-near-row.webp" alt="pines, near row" height="72"> |
| 285.6 | butterflies | <img src="objects/forest/butterflies.webp" alt="butterflies" height="72"> |
| 304 | flowers 1 | <img src="objects/forest/flowers-1.webp" alt="flowers 1" height="72"> |
| 314.3 | flowers 2 | <img src="objects/forest/flowers-2.webp" alt="flowers 2" height="72"> |
| 324.6 | flowers 3 | <img src="objects/forest/flowers-3.webp" alt="flowers 3" height="72"> |
| 335 | flowers 4 | <img src="objects/forest/flowers-4.webp" alt="flowers 4" height="72"> |
| 345.3 | flowers 5 | <img src="objects/forest/flowers-5.webp" alt="flowers 5" height="72"> |
| 345.8 | big tree, left, inner | <img src="objects/forest/big-tree-left-inner.webp" alt="big tree, left, inner" height="72"> |
| 345.8 | big tree, right, inner | <img src="objects/forest/big-tree-right-inner.webp" alt="big tree, right, inner" height="72"> |
| 355.6 | flowers 6 | <img src="objects/forest/flowers-6.webp" alt="flowers 6" height="72"> |
| 365.9 | flowers 7 | <img src="objects/forest/flowers-7.webp" alt="flowers 7" height="72"> |
| 371.5 | big tree, left, outer | <img src="objects/forest/big-tree-left-outer.webp" alt="big tree, left, outer" height="72"> |
| 371.5 | big tree, right, outer | <img src="objects/forest/big-tree-right-outer.webp" alt="big tree, right, outer" height="72"> |
| 376.2 | flowers 8 | <img src="objects/forest/flowers-8.webp" alt="flowers 8" height="72"> |
| 386.5 | flowers 9 | <img src="objects/forest/flowers-9.webp" alt="flowers 9" height="72"> |
| 396.8 | flowers 10 | <img src="objects/forest/flowers-10.webp" alt="flowers 10" height="72"> |
| front | way N | <img src="objects/forest/way-n.webp" alt="way N" height="72"> |
| front | way NE | <img src="objects/forest/way-ne.webp" alt="way NE" height="72"> |
| front | way E | <img src="objects/forest/way-e.webp" alt="way E" height="72"> |
| front | way SE | <img src="objects/forest/way-se.webp" alt="way SE" height="72"> |
| front | way S | <img src="objects/forest/way-s.webp" alt="way S" height="72"> |

## 🐠 underwater

<img src="objects/underwater/place.webp" alt="🐠 underwater, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | light rays | <img src="objects/underwater/light-rays.webp" alt="light rays" height="72"> |
| wall | far kelp | <img src="objects/underwater/far-kelp.webp" alt="far kelp" height="72"> |
| floor | sand specks | <img src="objects/underwater/sand-specks.webp" alt="sand specks" height="72"> |
| floor | clam | <img src="objects/underwater/clam.webp" alt="clam" height="72"> |
| 242.7 | school of fish | <img src="objects/underwater/school-of-fish.webp" alt="school of fish" height="72"> |
| 259.9 | orange fish | <img src="objects/underwater/orange-fish.webp" alt="orange fish" height="72"> |
| 259.9 | yellow fish | <img src="objects/underwater/yellow-fish.webp" alt="yellow fish" height="72"> |
| 259.9 | pink fish | <img src="objects/underwater/pink-fish.webp" alt="pink fish" height="72"> |
| 259.9 | blue fish | <img src="objects/underwater/blue-fish.webp" alt="blue fish" height="72"> |
| 337.2 | seaweed, left | <img src="objects/underwater/seaweed-left.webp" alt="seaweed, left" height="72"> |
| 337.2 | seaweed, right | <img src="objects/underwater/seaweed-right.webp" alt="seaweed, right" height="72"> |
| 340.6 | rock, left 3 | <img src="objects/underwater/rock-left-3.webp" alt="rock, left 3" height="72"> |
| 344.7 | rock, left 2 | <img src="objects/underwater/rock-left-2.webp" alt="rock, left 2" height="72"> |
| 345.8 | shipwreck | <img src="objects/underwater/shipwreck.webp" alt="shipwreck" height="72"> |
| 345.8 | picnic bench | <img src="objects/underwater/picnic-bench.webp" alt="picnic bench" height="72"> |
| 345.8 | shark, left | <img src="objects/underwater/shark-left.webp" alt="shark, left" height="72"> |
| 345.8 | shark, right | <img src="objects/underwater/shark-right.webp" alt="shark, right" height="72"> |
| 345.8 | picnic table | <img src="objects/underwater/picnic-table.webp" alt="picnic table" height="72"> |
| 347.2 | rock, right 2 | <img src="objects/underwater/rock-right-2.webp" alt="rock, right 2" height="72"> |
| 358.9 | rock, right 1 | <img src="objects/underwater/rock-right-1.webp" alt="rock, right 1" height="72"> |
| 362.3 | rock, left 1 | <img src="objects/underwater/rock-left-1.webp" alt="rock, left 1" height="72"> |
| front | bubbles | <img src="objects/underwater/bubbles.webp" alt="bubbles" height="72"> |

## 🪸 deep sea

<img src="objects/deepsea/place.webp" alt="🪸 deep sea, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | light rays | <img src="objects/deepsea/light-rays.webp" alt="light rays" height="72"> |
| floor | ripples in the sand | <img src="objects/deepsea/ripples-in-the-sand.webp" alt="ripples in the sand" height="72"> |
| 238.4 | school of fish | <img src="objects/deepsea/school-of-fish.webp" alt="school of fish" height="72"> |
| 340.6 | kelp, left | <img src="objects/deepsea/kelp-left.webp" alt="kelp, left" height="72"> |
| 340.6 | kelp, right | <img src="objects/deepsea/kelp-right.webp" alt="kelp, right" height="72"> |
| 342 | rock, left 2 | <img src="objects/deepsea/rock-left-2.webp" alt="rock, left 2" height="72"> |
| 349.8 | rock, left 1 | <img src="objects/deepsea/rock-left-1.webp" alt="rock, left 1" height="72"> |
| 354.4 | vent | <img src="objects/deepsea/vent.webp" alt="vent" height="72"> |
| 354.5 | rock, right 2 | <img src="objects/deepsea/rock-right-2.webp" alt="rock, right 2" height="72"> |
| 365.4 | rock, right 1 | <img src="objects/deepsea/rock-right-1.webp" alt="rock, right 1" height="72"> |
| front | way E | <img src="objects/deepsea/way-e.webp" alt="way E" height="72"> |
| front | way S | <img src="objects/deepsea/way-s.webp" alt="way S" height="72"> |

## 🏖️ beach

<img src="objects/beach/place.webp" alt="🏖️ beach, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | beach blanket, left | <img src="objects/beach/beach-blanket-left.webp" alt="beach blanket, left" height="72"> |
| floor | beach blanket, right | <img src="objects/beach/beach-blanket-right.webp" alt="beach blanket, right" height="72"> |
| 328.6 | beach chair, left | <img src="objects/beach/beach-chair-left.webp" alt="beach chair, left" height="72"> |
| 328.6 | beach chair, right | <img src="objects/beach/beach-chair-right.webp" alt="beach chair, right" height="72"> |
| 337.2 | umbrella, left | <img src="objects/beach/umbrella-left.webp" alt="umbrella, left" height="72"> |
| 337.2 | umbrella, right | <img src="objects/beach/umbrella-right.webp" alt="umbrella, right" height="72"> |
| 354.4 | palm tree, left | <img src="objects/beach/palm-tree-left.webp" alt="palm tree, left" height="72"> |
| 354.4 | palm tree, right | <img src="objects/beach/palm-tree-right.webp" alt="palm tree, right" height="72"> |
| 358.6 | sand castle, left | <img src="objects/beach/sand-castle-left.webp" alt="sand castle, left" height="72"> |
| 358.6 | sand castle, right | <img src="objects/beach/sand-castle-right.webp" alt="sand castle, right" height="72"> |
| front | way N | <img src="objects/beach/way-n.webp" alt="way N" height="72"> |
| front | way NE | <img src="objects/beach/way-ne.webp" alt="way NE" height="72"> |
| front | way E | <img src="objects/beach/way-e.webp" alt="way E" height="72"> |
| front | way SE | <img src="objects/beach/way-se.webp" alt="way SE" height="72"> |
| front | way S | <img src="objects/beach/way-s.webp" alt="way S" height="72"> |
| front | way SW | <img src="objects/beach/way-sw.webp" alt="way SW" height="72"> |
| front | way W | <img src="objects/beach/way-w.webp" alt="way W" height="72"> |
| front | way NW | <img src="objects/beach/way-nw.webp" alt="way NW" height="72"> |

## 🌊 open water

<img src="objects/sea/place.webp" alt="🌊 open water, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | sun glitter | <img src="objects/sea/sun-glitter.webp" alt="sun glitter" height="72"> |
| front | way E | <img src="objects/sea/way-e.webp" alt="way E" height="72"> |
| front | way SE | <img src="objects/sea/way-se.webp" alt="way SE" height="72"> |
| front | way SW | <img src="objects/sea/way-sw.webp" alt="way SW" height="72"> |
| front | way W | <img src="objects/sea/way-w.webp" alt="way W" height="72"> |

## 🧙 witch's woods

<img src="objects/witch/place.webp" alt="🧙 witch's woods, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | bats | <img src="objects/witch/bats.webp" alt="bats" height="72"> |
| 212.6 | dead tree, far 1 | <img src="objects/witch/dead-tree-far-1.webp" alt="dead tree, far 1" height="72"> |
| 212.6 | dead tree, far 2 | <img src="objects/witch/dead-tree-far-2.webp" alt="dead tree, far 2" height="72"> |
| 212.6 | dead tree, far 3 | <img src="objects/witch/dead-tree-far-3.webp" alt="dead tree, far 3" height="72"> |
| 212.6 | dead tree, far 4 | <img src="objects/witch/dead-tree-far-4.webp" alt="dead tree, far 4" height="72"> |
| 212.6 | dead tree, far 5 | <img src="objects/witch/dead-tree-far-5.webp" alt="dead tree, far 5" height="72"> |
| 212.6 | dead tree, far 6 | <img src="objects/witch/dead-tree-far-6.webp" alt="dead tree, far 6" height="72"> |
| 212.6 | dead tree, far 7 | <img src="objects/witch/dead-tree-far-7.webp" alt="dead tree, far 7" height="72"> |
| 212.6 | dead tree, far 8 | <img src="objects/witch/dead-tree-far-8.webp" alt="dead tree, far 8" height="72"> |
| 212.6 | dead tree, far 9 | <img src="objects/witch/dead-tree-far-9.webp" alt="dead tree, far 9" height="72"> |
| 245.3 | dead tree, near 1 | <img src="objects/witch/dead-tree-near-1.webp" alt="dead tree, near 1" height="72"> |
| 245.3 | dead tree, near 2 | <img src="objects/witch/dead-tree-near-2.webp" alt="dead tree, near 2" height="72"> |
| 245.3 | dead tree, near 3 | <img src="objects/witch/dead-tree-near-3.webp" alt="dead tree, near 3" height="72"> |
| 245.3 | dead tree, near 4 | <img src="objects/witch/dead-tree-near-4.webp" alt="dead tree, near 4" height="72"> |
| 245.3 | dead tree, near 5 | <img src="objects/witch/dead-tree-near-5.webp" alt="dead tree, near 5" height="72"> |
| 245.3 | dead tree, near 6 | <img src="objects/witch/dead-tree-near-6.webp" alt="dead tree, near 6" height="72"> |
| 245.3 | dead tree, near 7 | <img src="objects/witch/dead-tree-near-7.webp" alt="dead tree, near 7" height="72"> |
| 245.3 | dead tree, near 8 | <img src="objects/witch/dead-tree-near-8.webp" alt="dead tree, near 8" height="72"> |
| 278.8 | hut | <img src="objects/witch/hut.webp" alt="hut" height="72"> |
| 293.7 | grass 1 | <img src="objects/witch/grass-1.webp" alt="grass 1" height="72"> |
| 297.7 | fence | <img src="objects/witch/fence.webp" alt="fence" height="72"> |
| 297.7 | skull 1 | <img src="objects/witch/skull-1.webp" alt="skull 1" height="72"> |
| 297.7 | skull 2 | <img src="objects/witch/skull-2.webp" alt="skull 2" height="72"> |
| 297.7 | skull 3 | <img src="objects/witch/skull-3.webp" alt="skull 3" height="72"> |
| 297.7 | skull 4 | <img src="objects/witch/skull-4.webp" alt="skull 4" height="72"> |
| 297.7 | skull 5 | <img src="objects/witch/skull-5.webp" alt="skull 5" height="72"> |
| 297.7 | skull 6 | <img src="objects/witch/skull-6.webp" alt="skull 6" height="72"> |
| 297.7 | skull 7 | <img src="objects/witch/skull-7.webp" alt="skull 7" height="72"> |
| 297.7 | skull 8 | <img src="objects/witch/skull-8.webp" alt="skull 8" height="72"> |
| 297.7 | skull 9 | <img src="objects/witch/skull-9.webp" alt="skull 9" height="72"> |
| 297.7 | skull 10 | <img src="objects/witch/skull-10.webp" alt="skull 10" height="72"> |
| 297.7 | skull 11 | <img src="objects/witch/skull-11.webp" alt="skull 11" height="72"> |
| 304 | grass 2 | <img src="objects/witch/grass-2.webp" alt="grass 2" height="72"> |
| 314.3 | grass 3 | <img src="objects/witch/grass-3.webp" alt="grass 3" height="72"> |
| 335 | grass 4 | <img src="objects/witch/grass-4.webp" alt="grass 4" height="72"> |
| 345.3 | grass 5 | <img src="objects/witch/grass-5.webp" alt="grass 5" height="72"> |
| 354.4 | giant tree | <img src="objects/witch/giant-tree.webp" alt="giant tree" height="72"> |
| 354.4 | tree, right | <img src="objects/witch/tree-right.webp" alt="tree, right" height="72"> |
| 355.6 | grass 6 | <img src="objects/witch/grass-6.webp" alt="grass 6" height="72"> |
| 364.7 | skeleton, left | <img src="objects/witch/skeleton-left.webp" alt="skeleton, left" height="72"> |
| 365.9 | grass 7 | <img src="objects/witch/grass-7.webp" alt="grass 7" height="72"> |
| 367.2 | cauldron | <img src="objects/witch/cauldron.webp" alt="cauldron" height="72"> |
| 376.2 | grass 8 | <img src="objects/witch/grass-8.webp" alt="grass 8" height="72"> |
| 376.7 | skeleton, right | <img src="objects/witch/skeleton-right.webp" alt="skeleton, right" height="72"> |
| 380.1 | skeleton, right edge | <img src="objects/witch/skeleton-right-edge.webp" alt="skeleton, right edge" height="72"> |
| 383.6 | skeleton, left edge | <img src="objects/witch/skeleton-left-edge.webp" alt="skeleton, left edge" height="72"> |
| 396.8 | grass 9 | <img src="objects/witch/grass-9.webp" alt="grass 9" height="72"> |
| front | way E | <img src="objects/witch/way-e.webp" alt="way E" height="72"> |
| front | way SE | <img src="objects/witch/way-se.webp" alt="way SE" height="72"> |
| front | way S | <img src="objects/witch/way-s.webp" alt="way S" height="72"> |

## 🌿 cemetery

<img src="objects/cemetery/place.webp" alt="🌿 cemetery, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | stones by the brook | <img src="objects/cemetery/stones-by-the-brook.webp" alt="stones by the brook" height="72"> |
| 242.7 | far trees | <img src="objects/cemetery/far-trees.webp" alt="far trees" height="72"> |
| 259.9 | butterflies | <img src="objects/cemetery/butterflies.webp" alt="butterflies" height="72"> |
| 275.3 | far stones | <img src="objects/cemetery/far-stones.webp" alt="far stones" height="72"> |
| 293.7 | flowers 1 | <img src="objects/cemetery/flowers-1.webp" alt="flowers 1" height="72"> |
| 294.2 | yellow bush | <img src="objects/cemetery/yellow-bush.webp" alt="yellow bush" height="72"> |
| 301.5 | willow | <img src="objects/cemetery/willow.webp" alt="willow" height="72"> |
| 304 | flowers 2 | <img src="objects/cemetery/flowers-2.webp" alt="flowers 2" height="72"> |
| 314.3 | flowers 3 | <img src="objects/cemetery/flowers-3.webp" alt="flowers 3" height="72"> |
| 318.3 | green bush | <img src="objects/cemetery/green-bush.webp" alt="green bush" height="72"> |
| 323.4 | headstone, left | <img src="objects/cemetery/headstone-left.webp" alt="headstone, left" height="72"> |
| 324.6 | flowers 4 | <img src="objects/cemetery/flowers-4.webp" alt="flowers 4" height="72"> |
| 325.2 | cross | <img src="objects/cemetery/cross.webp" alt="cross" height="72"> |
| 335 | flowers 5 | <img src="objects/cemetery/flowers-5.webp" alt="flowers 5" height="72"> |
| 342.3 | headstone, right | <img src="objects/cemetery/headstone-right.webp" alt="headstone, right" height="72"> |
| 345.3 | flowers 6 | <img src="objects/cemetery/flowers-6.webp" alt="flowers 6" height="72"> |
| 345.8 | stag stone | <img src="objects/cemetery/stag-stone.webp" alt="stag stone" height="72"> |
| 345.8 | pink bush | <img src="objects/cemetery/pink-bush.webp" alt="pink bush" height="72"> |
| 349.2 | white bush | <img src="objects/cemetery/white-bush.webp" alt="white bush" height="72"> |
| 355.6 | flowers 7 | <img src="objects/cemetery/flowers-7.webp" alt="flowers 7" height="72"> |
| 376.2 | flowers 8 | <img src="objects/cemetery/flowers-8.webp" alt="flowers 8" height="72"> |
| 386.5 | flowers 9 | <img src="objects/cemetery/flowers-9.webp" alt="flowers 9" height="72"> |
| 396.8 | flowers 10 | <img src="objects/cemetery/flowers-10.webp" alt="flowers 10" height="72"> |
| front | way N | <img src="objects/cemetery/way-n.webp" alt="way N" height="72"> |
| front | way NE | <img src="objects/cemetery/way-ne.webp" alt="way NE" height="72"> |
| front | way E | <img src="objects/cemetery/way-e.webp" alt="way E" height="72"> |
| front | way W | <img src="objects/cemetery/way-w.webp" alt="way W" height="72"> |
| front | way NW | <img src="objects/cemetery/way-nw.webp" alt="way NW" height="72"> |

## 🦆 lake

<img src="objects/lake/place.webp" alt="🦆 lake, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | sun on the water | <img src="objects/lake/sun-on-the-water.webp" alt="sun on the water" height="72"> |
| floor | lily pads | <img src="objects/lake/lily-pads.webp" alt="lily pads" height="72"> |
| floor | pebbles | <img src="objects/lake/pebbles.webp" alt="pebbles" height="72"> |
| 105.3 | far wood | <img src="objects/lake/far-wood.webp" alt="far wood" height="72"> |
| 125.8 | duck 1 | <img src="objects/lake/duck-1.webp" alt="duck 1" height="72"> |
| 142.9 | duck 5 | <img src="objects/lake/duck-5.webp" alt="duck 5" height="72"> |
| 167.2 | duck 3 | <img src="objects/lake/duck-3.webp" alt="duck 3" height="72"> |
| 192.6 | duck 2 | <img src="objects/lake/duck-2.webp" alt="duck 2" height="72"> |
| 198.8 | duck 4 | <img src="objects/lake/duck-4.webp" alt="duck 4" height="72"> |
| 264.2 | cattails, left | <img src="objects/lake/cattails-left.webp" alt="cattails, left" height="72"> |
| 264.2 | cattails, right | <img src="objects/lake/cattails-right.webp" alt="cattails, right" height="72"> |
| 273.1 | grass 1 | <img src="objects/lake/grass-1.webp" alt="grass 1" height="72"> |
| 293.7 | grass 2 | <img src="objects/lake/grass-2.webp" alt="grass 2" height="72"> |
| 304 | grass 3 | <img src="objects/lake/grass-3.webp" alt="grass 3" height="72"> |
| 314.3 | grass 4 | <img src="objects/lake/grass-4.webp" alt="grass 4" height="72"> |
| 324.6 | grass 5 | <img src="objects/lake/grass-5.webp" alt="grass 5" height="72"> |
| 335 | grass 6 | <img src="objects/lake/grass-6.webp" alt="grass 6" height="72"> |
| 345.3 | grass 7 | <img src="objects/lake/grass-7.webp" alt="grass 7" height="72"> |
| 355.6 | grass 8 | <img src="objects/lake/grass-8.webp" alt="grass 8" height="72"> |
| 365.9 | grass 9 | <img src="objects/lake/grass-9.webp" alt="grass 9" height="72"> |
| 376.2 | grass 10 | <img src="objects/lake/grass-10.webp" alt="grass 10" height="72"> |
| 386.5 | grass 11 | <img src="objects/lake/grass-11.webp" alt="grass 11" height="72"> |
| 396.8 | grass 12 | <img src="objects/lake/grass-12.webp" alt="grass 12" height="72"> |
| 407.1 | grass 13 | <img src="objects/lake/grass-13.webp" alt="grass 13" height="72"> |
| front | way N | <img src="objects/lake/way-n.webp" alt="way N" height="72"> |
| front | way E | <img src="objects/lake/way-e.webp" alt="way E" height="72"> |
| front | way SE | <img src="objects/lake/way-se.webp" alt="way SE" height="72"> |
| front | way S | <img src="objects/lake/way-s.webp" alt="way S" height="72"> |
| front | way SW | <img src="objects/lake/way-sw.webp" alt="way SW" height="72"> |
| front | way W | <img src="objects/lake/way-w.webp" alt="way W" height="72"> |
| front | way NW | <img src="objects/lake/way-nw.webp" alt="way NW" height="72"> |

## 🏠 house and yard

<img src="objects/house/place.webp" alt="🏠 house and yard, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | frisbee | <img src="objects/house/frisbee.webp" alt="frisbee" height="72"> |
| floor | rope | <img src="objects/house/rope.webp" alt="rope" height="72"> |
| 191.2 | house | <img src="objects/house/house.webp" alt="house" height="72"> |
| 208.3 | tree, left | <img src="objects/house/tree-left.webp" alt="tree, left" height="72"> |
| 216.9 | tree, right | <img src="objects/house/tree-right.webp" alt="tree, right" height="72"> |
| 237.7 | teddy | <img src="objects/house/teddy.webp" alt="teddy" height="72"> |
| 239.6 | truck 2 | <img src="objects/house/truck-2.webp" alt="truck 2" height="72"> |
| 250.3 | wagon | <img src="objects/house/wagon.webp" alt="wagon" height="72"> |
| 251.1 | ball | <img src="objects/house/ball.webp" alt="ball" height="72"> |
| 268.4 | blocks | <img src="objects/house/blocks.webp" alt="blocks" height="72"> |
| 268.5 | butterflies | <img src="objects/house/butterflies.webp" alt="butterflies" height="72"> |
| 280.8 | truck | <img src="objects/house/truck.webp" alt="truck" height="72"> |
| 357.3 | ball 2 | <img src="objects/house/ball-2.webp" alt="ball 2" height="72"> |
| 366.2 | bucket | <img src="objects/house/bucket.webp" alt="bucket" height="72"> |
| front | way N | <img src="objects/house/way-n.webp" alt="way N" height="72"> |
| front | way NE | <img src="objects/house/way-ne.webp" alt="way NE" height="72"> |
| front | way E | <img src="objects/house/way-e.webp" alt="way E" height="72"> |
| front | way SE | <img src="objects/house/way-se.webp" alt="way SE" height="72"> |
| front | way S | <img src="objects/house/way-s.webp" alt="way S" height="72"> |
| front | way SW | <img src="objects/house/way-sw.webp" alt="way SW" height="72"> |
| front | way W | <img src="objects/house/way-w.webp" alt="way W" height="72"> |
| front | way NW | <img src="objects/house/way-nw.webp" alt="way NW" height="72"> |

## 🚜 farm yard

<img src="objects/farmyard/place.webp" alt="🚜 farm yard, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | far mountains | <img src="objects/farmyard/far-mountains.webp" alt="far mountains" height="72"> |
| wall | far forest | <img src="objects/farmyard/far-forest.webp" alt="far forest" height="72"> |
| wall | fields | <img src="objects/farmyard/fields.webp" alt="fields" height="72"> |
| floor | grass by the fence | <img src="objects/farmyard/grass-by-the-fence.webp" alt="grass by the fence" height="72"> |
| floor | wheel ruts | <img src="objects/farmyard/wheel-ruts.webp" alt="wheel ruts" height="72"> |
| floor | pebbles | <img src="objects/farmyard/pebbles.webp" alt="pebbles" height="72"> |
| floor | straw | <img src="objects/farmyard/straw.webp" alt="straw" height="72"> |
| 213.5 | red barn | <img src="objects/farmyard/red-barn.webp" alt="red barn" height="72"> |
| 215.2 | farmhouse | <img src="objects/farmyard/farmhouse.webp" alt="farmhouse" height="72"> |
| 239.3 | fence, back | <img src="objects/farmyard/fence-back.webp" alt="fence, back" height="72"> |
| 259.9 | windpump | <img src="objects/farmyard/windpump.webp" alt="windpump" height="72"> |
| 265 | tractor | <img src="objects/farmyard/tractor.webp" alt="tractor" height="72"> |
| 278.8 | water trough | <img src="objects/farmyard/water-trough.webp" alt="water trough" height="72"> |
| 289.1 | hen, black | <img src="objects/farmyard/hen-black.webp" alt="hen, black" height="72"> |
| 342.3 | hen, white | <img src="objects/farmyard/hen-white.webp" alt="hen, white" height="72"> |
| 345.8 | blowing straw | <img src="objects/farmyard/blowing-straw.webp" alt="blowing straw" height="72"> |
| 362.9 | haystack | <img src="objects/farmyard/haystack.webp" alt="haystack" height="72"> |
| 373.2 | hen, brown | <img src="objects/farmyard/hen-brown.webp" alt="hen, brown" height="72"> |

## 🌳 tree house

<img src="objects/treehouse/place.webp" alt="🌳 tree house, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| 214.4 | trees, far row | <img src="objects/treehouse/trees-far-row.webp" alt="trees, far row" height="72"> |
| 268.5 | butterflies | <img src="objects/treehouse/butterflies.webp" alt="butterflies" height="72"> |
| 287.4 | trees, left | <img src="objects/treehouse/trees-left.webp" alt="trees, left" height="72"> |
| 287.4 | trees, right | <img src="objects/treehouse/trees-right.webp" alt="trees, right" height="72"> |
| 304 | flowers 1 | <img src="objects/treehouse/flowers-1.webp" alt="flowers 1" height="72"> |
| 307.9 | bush, left 5 | <img src="objects/treehouse/bush-left-5.webp" alt="bush, left 5" height="72"> |
| 309.4 | bush, left 1 | <img src="objects/treehouse/bush-left-1.webp" alt="bush, left 1" height="72"> |
| 314.2 | bush, left 4 | <img src="objects/treehouse/bush-left-4.webp" alt="bush, left 4" height="72"> |
| 314.3 | flowers 2 | <img src="objects/treehouse/flowers-2.webp" alt="flowers 2" height="72"> |
| 315.5 | bush, right 3 | <img src="objects/treehouse/bush-right-3.webp" alt="bush, right 3" height="72"> |
| 320 | tree house | <img src="objects/treehouse/tree-house.webp" alt="tree house" height="72"> |
| 320 | tree house 4 | <img src="objects/treehouse/tree-house-4.webp" alt="tree house 4" height="72"> |
| 320 | tree house 3 | <img src="objects/treehouse/tree-house-3.webp" alt="tree house 3" height="72"> |
| 320 | tree house 2 | <img src="objects/treehouse/tree-house-2.webp" alt="tree house 2" height="72"> |
| 320 | tree house 1 | <img src="objects/treehouse/tree-house-1.webp" alt="tree house 1" height="72"> |
| 320.7 | bush, left 8 | <img src="objects/treehouse/bush-left-8.webp" alt="bush, left 8" height="72"> |
| 324.6 | flowers 3 | <img src="objects/treehouse/flowers-3.webp" alt="flowers 3" height="72"> |
| 334.9 | bush, right 1 | <img src="objects/treehouse/bush-right-1.webp" alt="bush, right 1" height="72"> |
| 335 | flowers 4 | <img src="objects/treehouse/flowers-4.webp" alt="flowers 4" height="72"> |
| 337.3 | bush, left 6 | <img src="objects/treehouse/bush-left-6.webp" alt="bush, left 6" height="72"> |
| 337.7 | bush, right 2 | <img src="objects/treehouse/bush-right-2.webp" alt="bush, right 2" height="72"> |
| 341.5 | bush, left 3 | <img src="objects/treehouse/bush-left-3.webp" alt="bush, left 3" height="72"> |
| 342.6 | bush, left 2 | <img src="objects/treehouse/bush-left-2.webp" alt="bush, left 2" height="72"> |
| 345.3 | flowers 5 | <img src="objects/treehouse/flowers-5.webp" alt="flowers 5" height="72"> |
| 345.5 | bush, right 8 | <img src="objects/treehouse/bush-right-8.webp" alt="bush, right 8" height="72"> |
| 346.8 | bush, left 7 | <img src="objects/treehouse/bush-left-7.webp" alt="bush, left 7" height="72"> |
| 355.6 | flowers 6 | <img src="objects/treehouse/flowers-6.webp" alt="flowers 6" height="72"> |
| 365.9 | flowers 7 | <img src="objects/treehouse/flowers-7.webp" alt="flowers 7" height="72"> |
| 376.2 | flowers 8 | <img src="objects/treehouse/flowers-8.webp" alt="flowers 8" height="72"> |
| 386.5 | flowers 9 | <img src="objects/treehouse/flowers-9.webp" alt="flowers 9" height="72"> |
| 396.8 | flowers 10 | <img src="objects/treehouse/flowers-10.webp" alt="flowers 10" height="72"> |
| front | way S | <img src="objects/treehouse/way-s.webp" alt="way S" height="72"> |
| front | way SW | <img src="objects/treehouse/way-sw.webp" alt="way SW" height="72"> |
| front | way W | <img src="objects/treehouse/way-w.webp" alt="way W" height="72"> |

## 🛋️ living room

<img src="objects/livingroom/place.webp" alt="🛋️ living room, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | rug | <img src="objects/livingroom/rug.webp" alt="rug" height="72"> |
| wall | wall clock | <img src="objects/livingroom/wall-clock.webp" alt="wall clock" height="72"> |
| wall | window and curtains | <img src="objects/livingroom/window-and-curtains.webp" alt="window and curtains" height="72"> |
| 242.7 | lamp | <img src="objects/livingroom/lamp.webp" alt="lamp" height="72"> |
| 259.9 | plant | <img src="objects/livingroom/plant.webp" alt="plant" height="72"> |
| 285.6 | couch | <img src="objects/livingroom/couch.webp" alt="couch" height="72"> |
| front | way S | <img src="objects/livingroom/way-s.webp" alt="way S" height="72"> |

## 🧙 inside the witch's hut

<img src="objects/witchroom/place.webp" alt="🧙 inside the witch's hut, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | round window | <img src="objects/witchroom/round-window.webp" alt="round window" height="72"> |
| wall | window, left | <img src="objects/witchroom/window-left.webp" alt="window, left" height="72"> |
| wall | window, right | <img src="objects/witchroom/window-right.webp" alt="window, right" height="72"> |
| wall | drying herbs 1 | <img src="objects/witchroom/drying-herbs-1.webp" alt="drying herbs 1" height="72"> |
| wall | drying herbs 2 | <img src="objects/witchroom/drying-herbs-2.webp" alt="drying herbs 2" height="72"> |
| wall | drying herbs 3 | <img src="objects/witchroom/drying-herbs-3.webp" alt="drying herbs 3" height="72"> |
| wall | drying herbs 4 | <img src="objects/witchroom/drying-herbs-4.webp" alt="drying herbs 4" height="72"> |
| wall | drying herbs 5 | <img src="objects/witchroom/drying-herbs-5.webp" alt="drying herbs 5" height="72"> |
| wall | drying herbs 6 | <img src="objects/witchroom/drying-herbs-6.webp" alt="drying herbs 6" height="72"> |
| wall | drying herbs 7 | <img src="objects/witchroom/drying-herbs-7.webp" alt="drying herbs 7" height="72"> |
| wall | drying herbs 8 | <img src="objects/witchroom/drying-herbs-8.webp" alt="drying herbs 8" height="72"> |
| wall | cobweb, left | <img src="objects/witchroom/cobweb-left.webp" alt="cobweb, left" height="72"> |
| wall | cobweb, right | <img src="objects/witchroom/cobweb-right.webp" alt="cobweb, right" height="72"> |
| wall | shelf of potions | <img src="objects/witchroom/shelf-of-potions.webp" alt="shelf of potions" height="72"> |
| wall | frogs in jars | <img src="objects/witchroom/frogs-in-jars.webp" alt="frogs in jars" height="72"> |
| wall | crow in a cage 1 | <img src="objects/witchroom/crow-in-a-cage-1.webp" alt="crow in a cage 1" height="72"> |
| wall | crow in a cage 2 | <img src="objects/witchroom/crow-in-a-cage-2.webp" alt="crow in a cage 2" height="72"> |
| wall | crow in a cage 3 | <img src="objects/witchroom/crow-in-a-cage-3.webp" alt="crow in a cage 3" height="72"> |
| 253 | snake tank | <img src="objects/witchroom/snake-tank.webp" alt="snake tank" height="72"> |
| 277.1 | cauldron | <img src="objects/witchroom/cauldron.webp" alt="cauldron" height="72"> |
| 320 | witch | <img src="objects/witchroom/witch.webp" alt="witch" height="72"> |
| front | dark | <img src="objects/witchroom/dark.webp" alt="dark" height="72"> |

## 🌲 forest clearing

<img src="objects/clearing/place.webp" alt="🌲 forest clearing, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| 136.2 | far woods | <img src="objects/clearing/far-woods.webp" alt="far woods" height="72"> |
| 204.9 | castle | <img src="objects/clearing/castle.webp" alt="castle" height="72"> |
| 205.3 | tree house | <img src="objects/clearing/tree-house.webp" alt="tree house" height="72"> |
| 226.6 | house | <img src="objects/clearing/house.webp" alt="house" height="72"> |
| 252.3 | witch hut | <img src="objects/clearing/witch-hut.webp" alt="witch hut" height="72"> |
| 271.2 | healer hut | <img src="objects/clearing/healer-hut.webp" alt="healer hut" height="72"> |
| 304 | flowers 1 | <img src="objects/clearing/flowers-1.webp" alt="flowers 1" height="72"> |
| 314.3 | flowers 2 | <img src="objects/clearing/flowers-2.webp" alt="flowers 2" height="72"> |
| 324.6 | flowers 3 | <img src="objects/clearing/flowers-3.webp" alt="flowers 3" height="72"> |
| 335 | flowers 4 | <img src="objects/clearing/flowers-4.webp" alt="flowers 4" height="72"> |
| 345.3 | flowers 5 | <img src="objects/clearing/flowers-5.webp" alt="flowers 5" height="72"> |
| 345.8 | butterflies | <img src="objects/clearing/butterflies.webp" alt="butterflies" height="72"> |
| 354.4 | tree, left, outer | <img src="objects/clearing/tree-left-outer.webp" alt="tree, left, outer" height="72"> |
| 354.4 | tree, right, outer | <img src="objects/clearing/tree-right-outer.webp" alt="tree, right, outer" height="72"> |
| 355.6 | flowers 6 | <img src="objects/clearing/flowers-6.webp" alt="flowers 6" height="72"> |
| 365.9 | flowers 7 | <img src="objects/clearing/flowers-7.webp" alt="flowers 7" height="72"> |
| 376.2 | flowers 8 | <img src="objects/clearing/flowers-8.webp" alt="flowers 8" height="72"> |
| 386.5 | flowers 9 | <img src="objects/clearing/flowers-9.webp" alt="flowers 9" height="72"> |
| 388.7 | tree, left, inner | <img src="objects/clearing/tree-left-inner.webp" alt="tree, left, inner" height="72"> |
| 388.7 | tree, right, inner | <img src="objects/clearing/tree-right-inner.webp" alt="tree, right, inner" height="72"> |
| 396.8 | flowers 10 | <img src="objects/clearing/flowers-10.webp" alt="flowers 10" height="72"> |
| 407.1 | flowers 11 | <img src="objects/clearing/flowers-11.webp" alt="flowers 11" height="72"> |
| front | way N | <img src="objects/clearing/way-n.webp" alt="way N" height="72"> |
| front | way NE | <img src="objects/clearing/way-ne.webp" alt="way NE" height="72"> |
| front | way E | <img src="objects/clearing/way-e.webp" alt="way E" height="72"> |
| front | way SE | <img src="objects/clearing/way-se.webp" alt="way SE" height="72"> |
| front | way S | <img src="objects/clearing/way-s.webp" alt="way S" height="72"> |
| front | way SW | <img src="objects/clearing/way-sw.webp" alt="way SW" height="72"> |
| front | way W | <img src="objects/clearing/way-w.webp" alt="way W" height="72"> |
| front | way NW | <img src="objects/clearing/way-nw.webp" alt="way NW" height="72"> |

## 🌿 inside the healer's hut

<img src="objects/healerroom/place.webp" alt="🌿 inside the healer's hut, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | rug | <img src="objects/healerroom/rug.webp" alt="rug" height="72"> |
| wall | round window | <img src="objects/healerroom/round-window.webp" alt="round window" height="72"> |
| wall | drying herbs 1 | <img src="objects/healerroom/drying-herbs-1.webp" alt="drying herbs 1" height="72"> |
| wall | drying herbs 2 | <img src="objects/healerroom/drying-herbs-2.webp" alt="drying herbs 2" height="72"> |
| wall | drying herbs 3 | <img src="objects/healerroom/drying-herbs-3.webp" alt="drying herbs 3" height="72"> |
| wall | drying herbs 4 | <img src="objects/healerroom/drying-herbs-4.webp" alt="drying herbs 4" height="72"> |
| wall | drying herbs 5 | <img src="objects/healerroom/drying-herbs-5.webp" alt="drying herbs 5" height="72"> |
| wall | drying herbs 6 | <img src="objects/healerroom/drying-herbs-6.webp" alt="drying herbs 6" height="72"> |
| wall | drying herbs 7 | <img src="objects/healerroom/drying-herbs-7.webp" alt="drying herbs 7" height="72"> |
| wall | drying herbs 8 | <img src="objects/healerroom/drying-herbs-8.webp" alt="drying herbs 8" height="72"> |
| wall | drying herbs 9 | <img src="objects/healerroom/drying-herbs-9.webp" alt="drying herbs 9" height="72"> |
| wall | drying herbs 10 | <img src="objects/healerroom/drying-herbs-10.webp" alt="drying herbs 10" height="72"> |
| wall | fireplace | <img src="objects/healerroom/fireplace.webp" alt="fireplace" height="72"> |
| wall | things on the mantel | <img src="objects/healerroom/things-on-the-mantel.webp" alt="things on the mantel" height="72"> |
| 234.1 | potion shelves | <img src="objects/healerroom/potion-shelves.webp" alt="potion shelves" height="72"> |
| 234.1 | cat | <img src="objects/healerroom/cat.webp" alt="cat" height="72"> |
| 320 | soup pot | <img src="objects/healerroom/soup-pot.webp" alt="soup pot" height="72"> |
| 320 | healer | <img src="objects/healerroom/healer.webp" alt="healer" height="72"> |
| 385.3 | tea table | <img src="objects/healerroom/tea-table.webp" alt="tea table" height="72"> |
| 387 | chair, left | <img src="objects/healerroom/chair-left.webp" alt="chair, left" height="72"> |
| 387 | chair, right | <img src="objects/healerroom/chair-right.webp" alt="chair, right" height="72"> |
| 392.1 | worktable | <img src="objects/healerroom/worktable.webp" alt="worktable" height="72"> |
| front | warm light | <img src="objects/healerroom/warm-light.webp" alt="warm light" height="72"> |

## 🧚 fairy forest

<img src="objects/fairyforest/place.webp" alt="🧚 fairy forest, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| 196.3 | trees, far row | <img src="objects/fairyforest/trees-far-row.webp" alt="trees, far row" height="72"> |
| 230.7 | trees, near row | <img src="objects/fairyforest/trees-near-row.webp" alt="trees, near row" height="72"> |
| 316.8 | toadstool 11 | <img src="objects/fairyforest/toadstool-11.webp" alt="toadstool 11" height="72"> |
| 317.5 | toadstool 12 | <img src="objects/fairyforest/toadstool-12.webp" alt="toadstool 12" height="72"> |
| 324 | toadstool 10 | <img src="objects/fairyforest/toadstool-10.webp" alt="toadstool 10" height="72"> |
| 325.2 | bush | <img src="objects/fairyforest/bush.webp" alt="bush" height="72"> |
| 325.4 | toadstool 13 | <img src="objects/fairyforest/toadstool-13.webp" alt="toadstool 13" height="72"> |
| 333.6 | toadstool 9 | <img src="objects/fairyforest/toadstool-9.webp" alt="toadstool 9" height="72"> |
| 335 | flowers and grass 1 | <img src="objects/fairyforest/flowers-and-grass-1.webp" alt="flowers and grass 1" height="72"> |
| 337.2 | great tree | <img src="objects/fairyforest/great-tree.webp" alt="great tree" height="72"> |
| 337.2 | fairy house | <img src="objects/fairyforest/fairy-house.webp" alt="fairy house" height="72"> |
| 341.5 | toadstool 14 | <img src="objects/fairyforest/toadstool-14.webp" alt="toadstool 14" height="72"> |
| 345.3 | flowers and grass 2 | <img src="objects/fairyforest/flowers-and-grass-2.webp" alt="flowers and grass 2" height="72"> |
| 349.4 | toadstool 8 | <img src="objects/fairyforest/toadstool-8.webp" alt="toadstool 8" height="72"> |
| 355.6 | flowers and grass 3 | <img src="objects/fairyforest/flowers-and-grass-3.webp" alt="flowers and grass 3" height="72"> |
| 357.2 | toadstool 1 | <img src="objects/fairyforest/toadstool-1.webp" alt="toadstool 1" height="72"> |
| 365.9 | flowers and grass 4 | <img src="objects/fairyforest/flowers-and-grass-4.webp" alt="flowers and grass 4" height="72"> |
| 370.6 | toadstool 7 | <img src="objects/fairyforest/toadstool-7.webp" alt="toadstool 7" height="72"> |
| 371.5 | stone, left | <img src="objects/fairyforest/stone-left.webp" alt="stone, left" height="72"> |
| 371.7 | toadstool 2 | <img src="objects/fairyforest/toadstool-2.webp" alt="toadstool 2" height="72"> |
| 376.2 | flowers and grass 5 | <img src="objects/fairyforest/flowers-and-grass-5.webp" alt="flowers and grass 5" height="72"> |
| 382.7 | toadstool 6 | <img src="objects/fairyforest/toadstool-6.webp" alt="toadstool 6" height="72"> |
| 386.5 | flowers and grass 6 | <img src="objects/fairyforest/flowers-and-grass-6.webp" alt="flowers and grass 6" height="72"> |
| 386.7 | toadstool 3 | <img src="objects/fairyforest/toadstool-3.webp" alt="toadstool 3" height="72"> |
| 390.7 | toadstool 5 | <img src="objects/fairyforest/toadstool-5.webp" alt="toadstool 5" height="72"> |
| 391.7 | toadstool 4 | <img src="objects/fairyforest/toadstool-4.webp" alt="toadstool 4" height="72"> |
| 396.8 | flowers and grass 7 | <img src="objects/fairyforest/flowers-and-grass-7.webp" alt="flowers and grass 7" height="72"> |
| 397.3 | stone, right | <img src="objects/fairyforest/stone-right.webp" alt="stone, right" height="72"> |
| 407.1 | flowers and grass 8 | <img src="objects/fairyforest/flowers-and-grass-8.webp" alt="flowers and grass 8" height="72"> |
| 417.4 | flowers and grass 9 | <img src="objects/fairyforest/flowers-and-grass-9.webp" alt="flowers and grass 9" height="72"> |
| front | sparkles | <img src="objects/fairyforest/sparkles.webp" alt="sparkles" height="72"> |
| front | way N | <img src="objects/fairyforest/way-n.webp" alt="way N" height="72"> |
| front | way SW | <img src="objects/fairyforest/way-sw.webp" alt="way SW" height="72"> |
| front | way W | <img src="objects/fairyforest/way-w.webp" alt="way W" height="72"> |
| front | way NW | <img src="objects/fairyforest/way-nw.webp" alt="way NW" height="72"> |

## ⛰️ mountain trail

<img src="objects/mountains/place.webp" alt="⛰️ mountain trail, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | far lake and village | <img src="objects/mountains/far-lake-and-village.webp" alt="far lake and village" height="72"> |
| 191.2 | birds | <img src="objects/mountains/birds.webp" alt="birds" height="72"> |
| 322.9 | pine, right 1 | <img src="objects/mountains/pine-right-1.webp" alt="pine, right 1" height="72"> |
| 326.6 | pine, right 4 | <img src="objects/mountains/pine-right-4.webp" alt="pine, right 4" height="72"> |
| 327.6 | pine, right 2 | <img src="objects/mountains/pine-right-2.webp" alt="pine, right 2" height="72"> |
| 329.6 | rock, left 5 | <img src="objects/mountains/rock-left-5.webp" alt="rock, left 5" height="72"> |
| 330.2 | pine, left 2 | <img src="objects/mountains/pine-left-2.webp" alt="pine, left 2" height="72"> |
| 334 | pine, left 3 | <img src="objects/mountains/pine-left-3.webp" alt="pine, left 3" height="72"> |
| 338.2 | rock, left 2 | <img src="objects/mountains/rock-left-2.webp" alt="rock, left 2" height="72"> |
| 338.9 | pine, right 3 | <img src="objects/mountains/pine-right-3.webp" alt="pine, right 3" height="72"> |
| 339.1 | pine, left 1 | <img src="objects/mountains/pine-left-1.webp" alt="pine, left 1" height="72"> |
| 344.1 | pine, left 4 | <img src="objects/mountains/pine-left-4.webp" alt="pine, left 4" height="72"> |
| 349.1 | rock, right 4 | <img src="objects/mountains/rock-right-4.webp" alt="rock, right 4" height="72"> |
| 351 | rock, left 4 | <img src="objects/mountains/rock-left-4.webp" alt="rock, left 4" height="72"> |
| 351.7 | rock, left 1 | <img src="objects/mountains/rock-left-1.webp" alt="rock, left 1" height="72"> |
| 355.9 | rock, left 3 | <img src="objects/mountains/rock-left-3.webp" alt="rock, left 3" height="72"> |
| 364.1 | rock, right 2 | <img src="objects/mountains/rock-right-2.webp" alt="rock, right 2" height="72"> |
| 364.7 | rock, right 3 | <img src="objects/mountains/rock-right-3.webp" alt="rock, right 3" height="72"> |
| 369.1 | rock, right 1 | <img src="objects/mountains/rock-right-1.webp" alt="rock, right 1" height="72"> |
| front | way E | <img src="objects/mountains/way-e.webp" alt="way E" height="72"> |
| front | way SE | <img src="objects/mountains/way-se.webp" alt="way SE" height="72"> |
| front | way S | <img src="objects/mountains/way-s.webp" alt="way S" height="72"> |
| front | way SW | <img src="objects/mountains/way-sw.webp" alt="way SW" height="72"> |
| front | way W | <img src="objects/mountains/way-w.webp" alt="way W" height="72"> |

## 💦 jungle waterfall

<img src="objects/waterfall/place.webp" alt="💦 jungle waterfall, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| 259.9 | mist | <img src="objects/waterfall/mist.webp" alt="mist" height="72"> |
| 259.9 | vines | <img src="objects/waterfall/vines.webp" alt="vines" height="72"> |
| 345.3 | flowers, left 1 | <img src="objects/waterfall/flowers-left-1.webp" alt="flowers, left 1" height="72"> |
| 345.3 | flowers, right 1 | <img src="objects/waterfall/flowers-right-1.webp" alt="flowers, right 1" height="72"> |
| 355.6 | flowers, left 2 | <img src="objects/waterfall/flowers-left-2.webp" alt="flowers, left 2" height="72"> |
| 355.6 | flowers, right 2 | <img src="objects/waterfall/flowers-right-2.webp" alt="flowers, right 2" height="72"> |
| 365.9 | flowers, left 3 | <img src="objects/waterfall/flowers-left-3.webp" alt="flowers, left 3" height="72"> |
| 365.9 | flowers, right 3 | <img src="objects/waterfall/flowers-right-3.webp" alt="flowers, right 3" height="72"> |
| 371.5 | jungle tree, left | <img src="objects/waterfall/jungle-tree-left.webp" alt="jungle tree, left" height="72"> |
| 371.5 | jungle tree, right | <img src="objects/waterfall/jungle-tree-right.webp" alt="jungle tree, right" height="72"> |
| 371.5 | parrot 1 | <img src="objects/waterfall/parrot-1.webp" alt="parrot 1" height="72"> |
| 371.5 | parrot 2 | <img src="objects/waterfall/parrot-2.webp" alt="parrot 2" height="72"> |
| 376.2 | flowers, left 4 | <img src="objects/waterfall/flowers-left-4.webp" alt="flowers, left 4" height="72"> |
| 376.2 | flowers, right 4 | <img src="objects/waterfall/flowers-right-4.webp" alt="flowers, right 4" height="72"> |
| front | way NE | <img src="objects/waterfall/way-ne.webp" alt="way NE" height="72"> |
| front | way E | <img src="objects/waterfall/way-e.webp" alt="way E" height="72"> |
| front | way SE | <img src="objects/waterfall/way-se.webp" alt="way SE" height="72"> |
| front | way S | <img src="objects/waterfall/way-s.webp" alt="way S" height="72"> |

## 🏕️ camp site

<img src="objects/campsite/place.webp" alt="🏕️ camp site, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| 238.4 | woods | <img src="objects/campsite/woods.webp" alt="woods" height="72"> |
| 285.6 | orange tent | <img src="objects/campsite/orange-tent.webp" alt="orange tent" height="72"> |
| 290.8 | blue tent | <img src="objects/campsite/blue-tent.webp" alt="blue tent" height="72"> |
| 354.4 | campfire | <img src="objects/campsite/campfire.webp" alt="campfire" height="72"> |
| 392.1 | log, left | <img src="objects/campsite/log-left.webp" alt="log, left" height="72"> |
| 392.1 | log, right | <img src="objects/campsite/log-right.webp" alt="log, right" height="72"> |
| 452.3 | log, near |  |
| front | way N | <img src="objects/campsite/way-n.webp" alt="way N" height="72"> |
| front | way NE | <img src="objects/campsite/way-ne.webp" alt="way NE" height="72"> |
| front | way E | <img src="objects/campsite/way-e.webp" alt="way E" height="72"> |
| front | way SE | <img src="objects/campsite/way-se.webp" alt="way SE" height="72"> |
| front | way S | <img src="objects/campsite/way-s.webp" alt="way S" height="72"> |
| front | way SW | <img src="objects/campsite/way-sw.webp" alt="way SW" height="72"> |
| front | way W | <img src="objects/campsite/way-w.webp" alt="way W" height="72"> |

## ⛺ inside a tent

<img src="objects/tent/place.webp" alt="⛺ inside a tent, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | window, left | <img src="objects/tent/window-left.webp" alt="window, left" height="72"> |
| wall | window, right | <img src="objects/tent/window-right.webp" alt="window, right" height="72"> |
| floor | sleeping bag, red | <img src="objects/tent/sleeping-bag-red.webp" alt="sleeping bag, red" height="72"> |
| floor | sleeping bag, purple | <img src="objects/tent/sleeping-bag-purple.webp" alt="sleeping bag, purple" height="72"> |
| floor | flashlight | <img src="objects/tent/flashlight.webp" alt="flashlight" height="72"> |
| 302.8 | flashlight beam | <img src="objects/tent/flashlight-beam.webp" alt="flashlight beam" height="72"> |

## 🪨 cave behind the falls

<img src="objects/cave/place.webp" alt="🪨 cave behind the falls, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | gems | <img src="objects/cave/gems.webp" alt="gems" height="72"> |
| wall | crystals 1 | <img src="objects/cave/crystals-1.webp" alt="crystals 1" height="72"> |
| wall | crystals 2 | <img src="objects/cave/crystals-2.webp" alt="crystals 2" height="72"> |
| wall | crystals 3 | <img src="objects/cave/crystals-3.webp" alt="crystals 3" height="72"> |
| wall | archway to the cave room | <img src="objects/cave/archway-to-the-cave-room.webp" alt="archway to the cave room" height="72"> |
| floor | the floor's ledges | <img src="objects/cave/the-floor-s-ledges.webp" alt="the floor's ledges" height="72"> |
| floor | wet floor | <img src="objects/cave/wet-floor.webp" alt="wet floor" height="72"> |
| wall | light from the falls | <img src="objects/cave/light-from-the-falls.webp" alt="light from the falls" height="72"> |
| floor | pebbles | <img src="objects/cave/pebbles.webp" alt="pebbles" height="72"> |
| 302.8 | big stalagmite, left | <img src="objects/cave/big-stalagmite-left.webp" alt="big stalagmite, left" height="72"> |
| 302.8 | small stalagmite, left | <img src="objects/cave/small-stalagmite-left.webp" alt="small stalagmite, left" height="72"> |
| 302.8 | big stalagmite, right | <img src="objects/cave/big-stalagmite-right.webp" alt="big stalagmite, right" height="72"> |
| 302.8 | small stalagmite, right | <img src="objects/cave/small-stalagmite-right.webp" alt="small stalagmite, right" height="72"> |
| 302.8 | stalactites | <img src="objects/cave/stalactites.webp" alt="stalactites" height="72"> |
| 302.8 | bats | <img src="objects/cave/bats.webp" alt="bats" height="72"> |
| 320 | chest | <img src="objects/cave/chest.webp" alt="chest" height="72"> |

## 🦇 cave room

<img src="objects/caveroom/place.webp" alt="🦇 cave room, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | glowing patches | <img src="objects/caveroom/glowing-patches.webp" alt="glowing patches" height="72"> |
| wall | archway to the cave | <img src="objects/caveroom/archway-to-the-cave.webp" alt="archway to the cave" height="72"> |
| floor | the floor's ledges | <img src="objects/caveroom/the-floor-s-ledges.webp" alt="the floor's ledges" height="72"> |
| floor | pebbles | <img src="objects/caveroom/pebbles.webp" alt="pebbles" height="72"> |
| floor | puddle | <img src="objects/caveroom/puddle.webp" alt="puddle" height="72"> |
| wall | the ceiling's stalactites | <img src="objects/caveroom/the-ceiling-s-stalactites.webp" alt="the ceiling's stalactites" height="72"> |
| 222.6 | stalagmite 3 | <img src="objects/caveroom/stalagmite-3.webp" alt="stalagmite 3" height="72"> |
| 227.3 | stalagmite 2 | <img src="objects/caveroom/stalagmite-2.webp" alt="stalagmite 2" height="72"> |
| 230.5 | stalagmite 4 | <img src="objects/caveroom/stalagmite-4.webp" alt="stalagmite 4" height="72"> |
| 236.9 | stalagmite 1 | <img src="objects/caveroom/stalagmite-1.webp" alt="stalagmite 1" height="72"> |
| 284.7 | stalagmite 5 | <img src="objects/caveroom/stalagmite-5.webp" alt="stalagmite 5" height="72"> |
| 297.4 | stalagmite 6 | <img src="objects/caveroom/stalagmite-6.webp" alt="stalagmite 6" height="72"> |
| 345.2 | stalagmite 8 | <img src="objects/caveroom/stalagmite-8.webp" alt="stalagmite 8" height="72"> |
| 345.8 | drops | <img src="objects/caveroom/drops.webp" alt="drops" height="72"> |
| 354.7 | stalagmite 7 | <img src="objects/caveroom/stalagmite-7.webp" alt="stalagmite 7" height="72"> |
| front | the dark | <img src="objects/caveroom/the-dark.webp" alt="the dark" height="72"> |

## 🥾 path through the woods

<img src="objects/woodpath/place.webp" alt="🥾 path through the woods, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | pebbles on the path | <img src="objects/woodpath/pebbles-on-the-path.webp" alt="pebbles on the path" height="72"> |
| 168.8 | far trees | <img src="objects/woodpath/far-trees.webp" alt="far trees" height="72"> |
| 168.8 | sunbeams | <img src="objects/woodpath/sunbeams.webp" alt="sunbeams" height="72"> |
| 277.1 | butterflies | <img src="objects/woodpath/butterflies.webp" alt="butterflies" height="72"> |
| 337.2 | tree, left inner | <img src="objects/woodpath/tree-left-inner.webp" alt="tree, left inner" height="72"> |
| 345.8 | tree, right inner | <img src="objects/woodpath/tree-right-inner.webp" alt="tree, right inner" height="72"> |
| 350.9 | mushroom, right inner | <img src="objects/woodpath/mushroom-right-inner.webp" alt="mushroom, right inner" height="72"> |
| 354.4 | mushroom, left inner | <img src="objects/woodpath/mushroom-left-inner.webp" alt="mushroom, left inner" height="72"> |
| 371.5 | mushroom, right outer | <img src="objects/woodpath/mushroom-right-outer.webp" alt="mushroom, right outer" height="72"> |
| 380.1 | tree, left outer | <img src="objects/woodpath/tree-left-outer.webp" alt="tree, left outer" height="72"> |
| 380.1 | mushroom, left outer | <img src="objects/woodpath/mushroom-left-outer.webp" alt="mushroom, left outer" height="72"> |
| 388.7 | tree, right outer | <img src="objects/woodpath/tree-right-outer.webp" alt="tree, right outer" height="72"> |
| front | way N | <img src="objects/woodpath/way-n.webp" alt="way N" height="72"> |
| front | way NE | <img src="objects/woodpath/way-ne.webp" alt="way NE" height="72"> |
| front | way E | <img src="objects/woodpath/way-e.webp" alt="way E" height="72"> |
| front | way S | <img src="objects/woodpath/way-s.webp" alt="way S" height="72"> |
| front | way W | <img src="objects/woodpath/way-w.webp" alt="way W" height="72"> |
| front | way NW | <img src="objects/woodpath/way-nw.webp" alt="way NW" height="72"> |

## 🌾 pasture

<img src="objects/pasture/place.webp" alt="🌾 pasture, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | far mountains | <img src="objects/pasture/far-mountains.webp" alt="far mountains" height="72"> |
| wall | far forest | <img src="objects/pasture/far-forest.webp" alt="far forest" height="72"> |
| 180.9 | fence and gate | <img src="objects/pasture/fence-and-gate.webp" alt="fence and gate" height="72"> |
| 201 | grass and flowers 1 | <img src="objects/pasture/grass-and-flowers-1.webp" alt="grass and flowers 1" height="72"> |
| 224.7 | grazing horse, chestnut | <img src="objects/pasture/grazing-horse-chestnut.webp" alt="grazing horse, chestnut" height="72"> |
| 231.9 | grass and flowers 2 | <img src="objects/pasture/grass-and-flowers-2.webp" alt="grass and flowers 2" height="72"> |
| 234.1 | grazing horse, bay | <img src="objects/pasture/grazing-horse-bay.webp" alt="grazing horse, bay" height="72"> |
| 238.4 | water trough | <img src="objects/pasture/water-trough.webp" alt="water trough" height="72"> |
| 247.9 | grazing horse, grey | <img src="objects/pasture/grazing-horse-grey.webp" alt="grazing horse, grey" height="72"> |
| 262.8 | grass and flowers 3 | <img src="objects/pasture/grass-and-flowers-3.webp" alt="grass and flowers 3" height="72"> |
| 277.1 | big tree | <img src="objects/pasture/big-tree.webp" alt="big tree" height="72"> |
| 293.7 | grass and flowers 4 | <img src="objects/pasture/grass-and-flowers-4.webp" alt="grass and flowers 4" height="72"> |
| 311.4 | butterflies | <img src="objects/pasture/butterflies.webp" alt="butterflies" height="72"> |
| 324.6 | grass and flowers 5 | <img src="objects/pasture/grass-and-flowers-5.webp" alt="grass and flowers 5" height="72"> |
| 355.6 | grass and flowers 6 | <img src="objects/pasture/grass-and-flowers-6.webp" alt="grass and flowers 6" height="72"> |
| 386.5 | grass and flowers 7 | <img src="objects/pasture/grass-and-flowers-7.webp" alt="grass and flowers 7" height="72"> |

## 🛤️ forest crossing

<img src="objects/crossing/place.webp" alt="🛤️ forest crossing, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| 57.2 | far trees | <img src="objects/crossing/far-trees.webp" alt="far trees" height="72"> |
| 57.2 | sunbeams | <img src="objects/crossing/sunbeams.webp" alt="sunbeams" height="72"> |
| 277.1 | butterflies | <img src="objects/crossing/butterflies.webp" alt="butterflies" height="72"> |
| front | way N | <img src="objects/crossing/way-n.webp" alt="way N" height="72"> |
| front | way E | <img src="objects/crossing/way-e.webp" alt="way E" height="72"> |
| front | way S | <img src="objects/crossing/way-s.webp" alt="way S" height="72"> |
| front | way W | <img src="objects/crossing/way-w.webp" alt="way W" height="72"> |

## 🏰 castle

<img src="objects/castle/place.webp" alt="🏰 castle, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | birds | <img src="objects/castle/birds.webp" alt="birds" height="72"> |
| 204.9 | trees, far row | <img src="objects/castle/trees-far-row.webp" alt="trees, far row" height="72"> |
| 239.3 | trees, near row | <img src="objects/castle/trees-near-row.webp" alt="trees, near row" height="72"> |
| 290.8 | castle | <img src="objects/castle/castle.webp" alt="castle" height="72"> |
| 290.8 | gate | <img src="objects/castle/gate.webp" alt="gate" height="72"> |
| 335 | flowers and grass 1 | <img src="objects/castle/flowers-and-grass-1.webp" alt="flowers and grass 1" height="72"> |
| 337.5 | well | <img src="objects/castle/well.webp" alt="well" height="72"> |
| 345.3 | flowers and grass 2 | <img src="objects/castle/flowers-and-grass-2.webp" alt="flowers and grass 2" height="72"> |
| 355.6 | flowers and grass 3 | <img src="objects/castle/flowers-and-grass-3.webp" alt="flowers and grass 3" height="72"> |
| 358.3 | hay cart | <img src="objects/castle/hay-cart.webp" alt="hay cart" height="72"> |
| 365.9 | flowers and grass 4 | <img src="objects/castle/flowers-and-grass-4.webp" alt="flowers and grass 4" height="72"> |
| 370.2 | pennant, left | <img src="objects/castle/pennant-left.webp" alt="pennant, left" height="72"> |
| 370.2 | pennant, right | <img src="objects/castle/pennant-right.webp" alt="pennant, right" height="72"> |
| 371.5 | big tree, left, outer | <img src="objects/castle/big-tree-left-outer.webp" alt="big tree, left, outer" height="72"> |
| 371.5 | big tree, right, outer | <img src="objects/castle/big-tree-right-outer.webp" alt="big tree, right, outer" height="72"> |
| 372.3 | rock, left, inner | <img src="objects/castle/rock-left-inner.webp" alt="rock, left, inner" height="72"> |
| 376.2 | flowers and grass 5 | <img src="objects/castle/flowers-and-grass-5.webp" alt="flowers and grass 5" height="72"> |
| 376.7 | barrel, far | <img src="objects/castle/barrel-far.webp" alt="barrel, far" height="72"> |
| 379 | rock, right, outer | <img src="objects/castle/rock-right-outer.webp" alt="rock, right, outer" height="72"> |
| 383.6 | barrel, middle | <img src="objects/castle/barrel-middle.webp" alt="barrel, middle" height="72"> |
| 386.5 | flowers and grass 6 | <img src="objects/castle/flowers-and-grass-6.webp" alt="flowers and grass 6" height="72"> |
| 390.4 | rock, right, inner | <img src="objects/castle/rock-right-inner.webp" alt="rock, right, inner" height="72"> |
| 390.6 | rock, left, outer | <img src="objects/castle/rock-left-outer.webp" alt="rock, left, outer" height="72"> |
| 392.1 | barrel, near | <img src="objects/castle/barrel-near.webp" alt="barrel, near" height="72"> |
| 392.1 | crate | <img src="objects/castle/crate.webp" alt="crate" height="72"> |
| 396.8 | flowers and grass 7 | <img src="objects/castle/flowers-and-grass-7.webp" alt="flowers and grass 7" height="72"> |
| 407.1 | flowers and grass 8 | <img src="objects/castle/flowers-and-grass-8.webp" alt="flowers and grass 8" height="72"> |
| 414.5 | big tree, left, inner | <img src="objects/castle/big-tree-left-inner.webp" alt="big tree, left, inner" height="72"> |
| 414.5 | big tree, right, inner | <img src="objects/castle/big-tree-right-inner.webp" alt="big tree, right, inner" height="72"> |
| 417.4 | flowers and grass 9 | <img src="objects/castle/flowers-and-grass-9.webp" alt="flowers and grass 9" height="72"> |
| front | way S | <img src="objects/castle/way-s.webp" alt="way S" height="72"> |
| front | way SW | <img src="objects/castle/way-sw.webp" alt="way SW" height="72"> |
| front | way W | <img src="objects/castle/way-w.webp" alt="way W" height="72"> |

## 🛖 grass hut

<img src="objects/hut/place.webp" alt="🛖 grass hut, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| 148.2 | far woods | <img src="objects/hut/far-woods.webp" alt="far woods" height="72"> |
| 273.1 | flowers 1 | <img src="objects/hut/flowers-1.webp" alt="flowers 1" height="72"> |
| 283.4 | flowers 2 | <img src="objects/hut/flowers-2.webp" alt="flowers 2" height="72"> |
| 285.6 | butterflies | <img src="objects/hut/butterflies.webp" alt="butterflies" height="72"> |
| 293.7 | flowers 3 | <img src="objects/hut/flowers-3.webp" alt="flowers 3" height="72"> |
| 304 | flowers 4 | <img src="objects/hut/flowers-4.webp" alt="flowers 4" height="72"> |
| 314.3 | flowers 5 | <img src="objects/hut/flowers-5.webp" alt="flowers 5" height="72"> |
| 324.6 | flowers 6 | <img src="objects/hut/flowers-6.webp" alt="flowers 6" height="72"> |
| 335 | flowers 7 | <img src="objects/hut/flowers-7.webp" alt="flowers 7" height="72"> |
| 340.6 | woodpile | <img src="objects/hut/woodpile.webp" alt="woodpile" height="72"> |
| 345.3 | flowers 8 | <img src="objects/hut/flowers-8.webp" alt="flowers 8" height="72"> |
| 355.6 | flowers 9 | <img src="objects/hut/flowers-9.webp" alt="flowers 9" height="72"> |
| 365.9 | flowers 10 | <img src="objects/hut/flowers-10.webp" alt="flowers 10" height="72"> |
| 376.2 | flowers 11 | <img src="objects/hut/flowers-11.webp" alt="flowers 11" height="72"> |
| 382.7 | hut | <img src="objects/hut/hut.webp" alt="hut" height="72"> |
| 386.5 | flowers 12 | <img src="objects/hut/flowers-12.webp" alt="flowers 12" height="72"> |
| 396.8 | flowers 13 | <img src="objects/hut/flowers-13.webp" alt="flowers 13" height="72"> |
| 407.1 | flowers 14 | <img src="objects/hut/flowers-14.webp" alt="flowers 14" height="72"> |

## 🛋️ inside the grass hut

<img src="objects/hutroom/place.webp" alt="🛋️ inside the grass hut, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | rug | <img src="objects/hutroom/rug.webp" alt="rug" height="72"> |
| wall | window | <img src="objects/hutroom/window.webp" alt="window" height="72"> |
| wall | wind chime | <img src="objects/hutroom/wind-chime.webp" alt="wind chime" height="72"> |
| wall | lantern | <img src="objects/hutroom/lantern.webp" alt="lantern" height="72"> |
| 285.6 | couch | <img src="objects/hutroom/couch.webp" alt="couch" height="72"> |
| front | way S | <img src="objects/hutroom/way-s.webp" alt="way S" height="72"> |

## 🛏️ the grass hut's bedroom

<img src="objects/hutbed/place.webp" alt="🛏️ the grass hut's bedroom, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | rug | <img src="objects/hutbed/rug.webp" alt="rug" height="72"> |
| wall | window | <img src="objects/hutbed/window.webp" alt="window" height="72"> |
| wall | dreamcatcher | <img src="objects/hutbed/dreamcatcher.webp" alt="dreamcatcher" height="72"> |
| 337.2 | stump | <img src="objects/hutbed/stump.webp" alt="stump" height="72"> |
| 349.2 | bed | <img src="objects/hutbed/bed.webp" alt="bed" height="72"> |

## 🧪 the grass hut's potion room

<img src="objects/hutpotion/place.webp" alt="🧪 the grass hut's potion room, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | shelf of potions 1 | <img src="objects/hutpotion/shelf-of-potions-1.webp" alt="shelf of potions 1" height="72"> |
| wall | shelf of potions 2 | <img src="objects/hutpotion/shelf-of-potions-2.webp" alt="shelf of potions 2" height="72"> |
| wall | shelf of potions 3 | <img src="objects/hutpotion/shelf-of-potions-3.webp" alt="shelf of potions 3" height="72"> |
| wall | beam | <img src="objects/hutpotion/beam.webp" alt="beam" height="72"> |
| wall | drying herbs 1 |  |
| wall | drying herbs 2 |  |
| wall | drying herbs 3 | <img src="objects/hutpotion/drying-herbs-3.webp" alt="drying herbs 3" height="72"> |
| wall | drying herbs 4 | <img src="objects/hutpotion/drying-herbs-4.webp" alt="drying herbs 4" height="72"> |
| wall | drying herbs 5 | <img src="objects/hutpotion/drying-herbs-5.webp" alt="drying herbs 5" height="72"> |
| wall | drying herbs 6 | <img src="objects/hutpotion/drying-herbs-6.webp" alt="drying herbs 6" height="72"> |
| 320 | cauldron | <img src="objects/hutpotion/cauldron.webp" alt="cauldron" height="72"> |
| 328.6 | table | <img src="objects/hutpotion/table.webp" alt="table" height="72"> |

## 🔥 the grass hut's center

<img src="objects/hutcenter/place.webp" alt="🔥 the grass hut's center, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | daylight | <img src="objects/hutcenter/daylight.webp" alt="daylight" height="72"> |
| wall | lantern, left | <img src="objects/hutcenter/lantern-left.webp" alt="lantern, left" height="72"> |
| wall | lantern, right | <img src="objects/hutcenter/lantern-right.webp" alt="lantern, right" height="72"> |
| wall | smoke | <img src="objects/hutcenter/smoke.webp" alt="smoke" height="72"> |
| 357.8 | fire | <img src="objects/hutcenter/fire.webp" alt="fire" height="72"> |
| 383.6 | log, left | <img src="objects/hutcenter/log-left.webp" alt="log, left" height="72"> |
| 383.6 | log, right | <img src="objects/hutcenter/log-right.webp" alt="log, right" height="72"> |
| front | way S | <img src="objects/hutcenter/way-s.webp" alt="way S" height="72"> |

## 🏜️ desert

<img src="objects/desert/place.webp" alt="🏜️ desert, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | far mountains | <img src="objects/desert/far-mountains.webp" alt="far mountains" height="72"> |
| wall | far dunes | <img src="objects/desert/far-dunes.webp" alt="far dunes" height="72"> |
| wall | near dunes | <img src="objects/desert/near-dunes.webp" alt="near dunes" height="72"> |
| wall | sand blowing off the dunes | <img src="objects/desert/sand-blowing-off-the-dunes.webp" alt="sand blowing off the dunes" height="72"> |
| floor | sand ripples | <img src="objects/desert/sand-ripples.webp" alt="sand ripples" height="72"> |
| floor | tracks in the sand | <img src="objects/desert/tracks-in-the-sand.webp" alt="tracks in the sand" height="72"> |
| 120.7 | hawk | <img src="objects/desert/hawk.webp" alt="hawk" height="72"> |
| 230.7 | rock, back | <img src="objects/desert/rock-back.webp" alt="rock, back" height="72"> |
| 247.9 | saguaro, far | <img src="objects/desert/saguaro-far.webp" alt="saguaro, far" height="72"> |
| 258.2 | joshua tree | <img src="objects/desert/joshua-tree.webp" alt="joshua tree" height="72"> |
| 268.5 | rock, right | <img src="objects/desert/rock-right.webp" alt="rock, right" height="72"> |
| 282.2 | agave | <img src="objects/desert/agave.webp" alt="agave" height="72"> |
| 294.2 | ocotillo | <img src="objects/desert/ocotillo.webp" alt="ocotillo" height="72"> |
| 366.4 | prickly pear | <img src="objects/desert/prickly-pear.webp" alt="prickly pear" height="72"> |
| 371.5 | saguaro, near | <img src="objects/desert/saguaro-near.webp" alt="saguaro, near" height="72"> |
| 375 | tumbleweed | <img src="objects/desert/tumbleweed.webp" alt="tumbleweed" height="72"> |
| 393.9 | lizard | <img src="objects/desert/lizard.webp" alt="lizard" height="72"> |
| 397.3 | barrel cactus | <img src="objects/desert/barrel-cactus.webp" alt="barrel cactus" height="72"> |

## 🪜 tree house room 1

<img src="objects/treeroom-1/place.webp" alt="🪜 tree house room 1, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | limb through the wall 1 | <img src="objects/treeroom-1/limb-through-the-wall-1.webp" alt="limb through the wall 1" height="72"> |
| wall | limb through the wall 2 | <img src="objects/treeroom-1/limb-through-the-wall-2.webp" alt="limb through the wall 2" height="72"> |
| wall | hatch lever | <img src="objects/treeroom-1/hatch-lever.webp" alt="hatch lever" height="72"> |
| 225.5 | side doors | <img src="objects/treeroom-1/side-doors.webp" alt="side doors" height="72"> |
| 231.7 | bunk beds | <img src="objects/treeroom-1/bunk-beds.webp" alt="bunk beds" height="72"> |
| 237.5 | ladder up | <img src="objects/treeroom-1/ladder-up.webp" alt="ladder up" height="72"> |
| 237.9 | toy chest | <img src="objects/treeroom-1/toy-chest.webp" alt="toy chest" height="72"> |
| 250.3 | tree trunk | <img src="objects/treeroom-1/tree-trunk.webp" alt="tree trunk" height="72"> |
| 271.9 | hatch | <img src="objects/treeroom-1/hatch.webp" alt="hatch" height="72"> |
| front | house sign | <img src="objects/treeroom-1/house-sign.webp" alt="house sign" height="72"> |
| front | dark | <img src="objects/treeroom-1/dark.webp" alt="dark" height="72"> |

## 🪜 tree house room 2

<img src="objects/treeroom-2/place.webp" alt="🪜 tree house room 2, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | limb through the wall 1 | <img src="objects/treeroom-2/limb-through-the-wall-1.webp" alt="limb through the wall 1" height="72"> |
| wall | shelves of jars | <img src="objects/treeroom-2/shelves-of-jars.webp" alt="shelves of jars" height="72"> |
| wall | hatch lever | <img src="objects/treeroom-2/hatch-lever.webp" alt="hatch lever" height="72"> |
| 225.5 | side doors | <img src="objects/treeroom-2/side-doors.webp" alt="side doors" height="72"> |
| 231.7 | counter | <img src="objects/treeroom-2/counter.webp" alt="counter" height="72"> |
| 231.7 | stove | <img src="objects/treeroom-2/stove.webp" alt="stove" height="72"> |
| 250.3 | tree trunk | <img src="objects/treeroom-2/tree-trunk.webp" alt="tree trunk" height="72"> |
| 271.9 | hatch | <img src="objects/treeroom-2/hatch.webp" alt="hatch" height="72"> |
| 277.5 | table | <img src="objects/treeroom-2/table.webp" alt="table" height="72"> |
| front | house sign | <img src="objects/treeroom-2/house-sign.webp" alt="house sign" height="72"> |
| front | dark | <img src="objects/treeroom-2/dark.webp" alt="dark" height="72"> |

## 🪜 tree house room 3

<img src="objects/treeroom-3/place.webp" alt="🪜 tree house room 3, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | lantern | <img src="objects/treeroom-3/lantern.webp" alt="lantern" height="72"> |
| wall | hatch lever | <img src="objects/treeroom-3/hatch-lever.webp" alt="hatch lever" height="72"> |
| 225.5 | side doors | <img src="objects/treeroom-3/side-doors.webp" alt="side doors" height="72"> |
| 237.9 | bookcase | <img src="objects/treeroom-3/bookcase.webp" alt="bookcase" height="72"> |
| 250.3 | tree trunk | <img src="objects/treeroom-3/tree-trunk.webp" alt="tree trunk" height="72"> |
| 262.7 | beanbag | <img src="objects/treeroom-3/beanbag.webp" alt="beanbag" height="72"> |
| 271.9 | hatch | <img src="objects/treeroom-3/hatch.webp" alt="hatch" height="72"> |
| front | house sign | <img src="objects/treeroom-3/house-sign.webp" alt="house sign" height="72"> |
| front | dark | <img src="objects/treeroom-3/dark.webp" alt="dark" height="72"> |

## 🪜 tree house room 4

<img src="objects/treeroom-4/place.webp" alt="🪜 tree house room 4, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | limb through the wall 1 | <img src="objects/treeroom-4/limb-through-the-wall-1.webp" alt="limb through the wall 1" height="72"> |
| wall | treasure map | <img src="objects/treeroom-4/treasure-map.webp" alt="treasure map" height="72"> |
| wall | pennants | <img src="objects/treeroom-4/pennants.webp" alt="pennants" height="72"> |
| 225.5 | side doors | <img src="objects/treeroom-4/side-doors.webp" alt="side doors" height="72"> |
| 237.9 | telescope | <img src="objects/treeroom-4/telescope.webp" alt="telescope" height="72"> |
| 250.3 | tree trunk | <img src="objects/treeroom-4/tree-trunk.webp" alt="tree trunk" height="72"> |
| front | house sign | <img src="objects/treeroom-4/house-sign.webp" alt="house sign" height="72"> |
| front | dark | <img src="objects/treeroom-4/dark.webp" alt="dark" height="72"> |

## 🪜 tree house room 5

<img src="objects/treeroom-5/place.webp" alt="🪜 tree house room 5, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | limb through the wall 1 | <img src="objects/treeroom-5/limb-through-the-wall-1.webp" alt="limb through the wall 1" height="72"> |
| wall | limb through the wall 2 | <img src="objects/treeroom-5/limb-through-the-wall-2.webp" alt="limb through the wall 2" height="72"> |
| floor | paint spills | <img src="objects/treeroom-5/paint-spills.webp" alt="paint spills" height="72"> |
| 237.5 | ladder up | <img src="objects/treeroom-5/ladder-up.webp" alt="ladder up" height="72"> |
| 239.1 | paint table | <img src="objects/treeroom-5/paint-table.webp" alt="paint table" height="72"> |
| 244.1 | easel | <img src="objects/treeroom-5/easel.webp" alt="easel" height="72"> |
| 250.3 | tree trunk | <img src="objects/treeroom-5/tree-trunk.webp" alt="tree trunk" height="72"> |
| front | house sign | <img src="objects/treeroom-5/house-sign.webp" alt="house sign" height="72"> |
| front | dark | <img src="objects/treeroom-5/dark.webp" alt="dark" height="72"> |

## 🪜 tree house room 6

<img src="objects/treeroom-6/place.webp" alt="🪜 tree house room 6, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | limb through the wall 1 | <img src="objects/treeroom-6/limb-through-the-wall-1.webp" alt="limb through the wall 1" height="72"> |
| wall | limb through the wall 2 | <img src="objects/treeroom-6/limb-through-the-wall-2.webp" alt="limb through the wall 2" height="72"> |
| wall | records | <img src="objects/treeroom-6/records.webp" alt="records" height="72"> |
| 234.1 | drum kit | <img src="objects/treeroom-6/drum-kit.webp" alt="drum kit" height="72"> |
| 237.5 | ladder up | <img src="objects/treeroom-6/ladder-up.webp" alt="ladder up" height="72"> |
| 237.9 | keyboard | <img src="objects/treeroom-6/keyboard.webp" alt="keyboard" height="72"> |
| 250.3 | tree trunk | <img src="objects/treeroom-6/tree-trunk.webp" alt="tree trunk" height="72"> |
| front | house sign | <img src="objects/treeroom-6/house-sign.webp" alt="house sign" height="72"> |
| front | dark | <img src="objects/treeroom-6/dark.webp" alt="dark" height="72"> |

## 👑 king's room

<img src="objects/castleroom-king/place.webp" alt="👑 king's room, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | cobweb, left | <img src="objects/castleroom-king/cobweb-left.webp" alt="cobweb, left" height="72"> |
| wall | cobweb, right | <img src="objects/castleroom-king/cobweb-right.webp" alt="cobweb, right" height="72"> |
| wall | stained-glass window | <img src="objects/castleroom-king/stained-glass-window.webp" alt="stained-glass window" height="72"> |
| wall | banner, left | <img src="objects/castleroom-king/banner-left.webp" alt="banner, left" height="72"> |
| wall | banner, right | <img src="objects/castleroom-king/banner-right.webp" alt="banner, right" height="72"> |
| wall | crossed swords, left | <img src="objects/castleroom-king/crossed-swords-left.webp" alt="crossed swords, left" height="72"> |
| wall | crossed swords, right | <img src="objects/castleroom-king/crossed-swords-right.webp" alt="crossed swords, right" height="72"> |
| wall | torch, left | <img src="objects/castleroom-king/torch-left.webp" alt="torch, left" height="72"> |
| wall | torch, right | <img src="objects/castleroom-king/torch-right.webp" alt="torch, right" height="72"> |
| floor | carpet | <img src="objects/castleroom-king/carpet.webp" alt="carpet" height="72"> |
| floor | dais | <img src="objects/castleroom-king/dais.webp" alt="dais" height="72"> |
| 225.5 | chandelier | <img src="objects/castleroom-king/chandelier.webp" alt="chandelier" height="72"> |
| 225.5 | armor, left | <img src="objects/castleroom-king/armor-left.webp" alt="armor, left" height="72"> |
| 225.5 | armor, right | <img src="objects/castleroom-king/armor-right.webp" alt="armor, right" height="72"> |
| 225.5 | throne 1 | <img src="objects/castleroom-king/throne-1.webp" alt="throne 1" height="72"> |
| 225.5 | throne 2 | <img src="objects/castleroom-king/throne-2.webp" alt="throne 2" height="72"> |
| 225.5 | door 2 | <img src="objects/castleroom-king/door-2.webp" alt="door 2" height="72"> |
| 225.5 | door 3 | <img src="objects/castleroom-king/door-3.webp" alt="door 3" height="72"> |
| 268.5 | chest of gold | <img src="objects/castleroom-king/chest-of-gold.webp" alt="chest of gold" height="72"> |
| front | way S | <img src="objects/castleroom-king/way-s.webp" alt="way S" height="72"> |

## 🌿 greenhouse

<img src="objects/castleroom-greenhouse/place.webp" alt="🌿 greenhouse, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | moss | <img src="objects/castleroom-greenhouse/moss.webp" alt="moss" height="72"> |
| floor | fallen petals | <img src="objects/castleroom-greenhouse/fallen-petals.webp" alt="fallen petals" height="72"> |
| wall | vine 1 | <img src="objects/castleroom-greenhouse/vine-1.webp" alt="vine 1" height="72"> |
| wall | vine 2 | <img src="objects/castleroom-greenhouse/vine-2.webp" alt="vine 2" height="72"> |
| wall | vine 3 | <img src="objects/castleroom-greenhouse/vine-3.webp" alt="vine 3" height="72"> |
| wall | vine 4 | <img src="objects/castleroom-greenhouse/vine-4.webp" alt="vine 4" height="72"> |
| wall | vine 5 | <img src="objects/castleroom-greenhouse/vine-5.webp" alt="vine 5" height="72"> |
| wall | vine 6 | <img src="objects/castleroom-greenhouse/vine-6.webp" alt="vine 6" height="72"> |
| wall | vine 7 | <img src="objects/castleroom-greenhouse/vine-7.webp" alt="vine 7" height="72"> |
| wall | vine 8 | <img src="objects/castleroom-greenhouse/vine-8.webp" alt="vine 8" height="72"> |
| wall | vine 9 | <img src="objects/castleroom-greenhouse/vine-9.webp" alt="vine 9" height="72"> |
| wall | hanging vine 1 | <img src="objects/castleroom-greenhouse/hanging-vine-1.webp" alt="hanging vine 1" height="72"> |
| wall | hanging vine 2 | <img src="objects/castleroom-greenhouse/hanging-vine-2.webp" alt="hanging vine 2" height="72"> |
| wall | hanging vine 3 | <img src="objects/castleroom-greenhouse/hanging-vine-3.webp" alt="hanging vine 3" height="72"> |
| wall | hanging vine 4 | <img src="objects/castleroom-greenhouse/hanging-vine-4.webp" alt="hanging vine 4" height="72"> |
| wall | hanging vine 5 | <img src="objects/castleroom-greenhouse/hanging-vine-5.webp" alt="hanging vine 5" height="72"> |
| wall | hanging vine 6 | <img src="objects/castleroom-greenhouse/hanging-vine-6.webp" alt="hanging vine 6" height="72"> |
| wall | hanging vine 7 | <img src="objects/castleroom-greenhouse/hanging-vine-7.webp" alt="hanging vine 7" height="72"> |
| wall | hanging vine 8 | <img src="objects/castleroom-greenhouse/hanging-vine-8.webp" alt="hanging vine 8" height="72"> |
| wall | hanging vine 9 | <img src="objects/castleroom-greenhouse/hanging-vine-9.webp" alt="hanging vine 9" height="72"> |
| 225.5 | parrots | <img src="objects/castleroom-greenhouse/parrots.webp" alt="parrots" height="72"> |
| 225.5 | arch to room 1 | <img src="objects/castleroom-greenhouse/arch-to-room-1.webp" alt="arch to room 1" height="72"> |
| 225.5 | arch to room 2 | <img src="objects/castleroom-greenhouse/arch-to-room-2.webp" alt="arch to room 2" height="72"> |
| 225.5 | arch to room 3 | <img src="objects/castleroom-greenhouse/arch-to-room-3.webp" alt="arch to room 3" height="72"> |
| 230.7 | ferns | <img src="objects/castleroom-greenhouse/ferns.webp" alt="ferns" height="72"> |
| 232.4 | bird of paradise, right | <img src="objects/castleroom-greenhouse/bird-of-paradise-right.webp" alt="bird of paradise, right" height="72"> |
| 234.1 | bird of paradise, left | <img src="objects/castleroom-greenhouse/bird-of-paradise-left.webp" alt="bird of paradise, left" height="72"> |
| 237.5 | monstera, left | <img src="objects/castleroom-greenhouse/monstera-left.webp" alt="monstera, left" height="72"> |
| 239.3 | monstera, right | <img src="objects/castleroom-greenhouse/monstera-right.webp" alt="monstera, right" height="72"> |
| 242.7 | palm, left | <img src="objects/castleroom-greenhouse/palm-left.webp" alt="palm, left" height="72"> |
| 251.3 | palm, right | <img src="objects/castleroom-greenhouse/palm-right.webp" alt="palm, right" height="72"> |
| 285.6 | hibiscus, left | <img src="objects/castleroom-greenhouse/hibiscus-left.webp" alt="hibiscus, left" height="72"> |
| 289.1 | hibiscus, right | <img src="objects/castleroom-greenhouse/hibiscus-right.webp" alt="hibiscus, right" height="72"> |
| 320 | fountain | <img src="objects/castleroom-greenhouse/fountain.webp" alt="fountain" height="72"> |
| 354.4 | bromeliad, left | <img src="objects/castleroom-greenhouse/bromeliad-left.webp" alt="bromeliad, left" height="72"> |
| 354.4 | bromeliad, right | <img src="objects/castleroom-greenhouse/bromeliad-right.webp" alt="bromeliad, right" height="72"> |
| 388.7 | watering can | <img src="objects/castleroom-greenhouse/watering-can.webp" alt="watering can" height="72"> |

## 🕳️ trap room

<img src="objects/castleroom-trap/place.webp" alt="🕳️ trap room, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | moss | <img src="objects/castleroom-trap/moss.webp" alt="moss" height="72"> |
| wall | cobweb, left | <img src="objects/castleroom-trap/cobweb-left.webp" alt="cobweb, left" height="72"> |
| wall | cobweb, right | <img src="objects/castleroom-trap/cobweb-right.webp" alt="cobweb, right" height="72"> |
| wall | torch, left | <img src="objects/castleroom-trap/torch-left.webp" alt="torch, left" height="72"> |
| wall | torch, right | <img src="objects/castleroom-trap/torch-right.webp" alt="torch, right" height="72"> |
| wall | lever, left | <img src="objects/castleroom-trap/lever-left.webp" alt="lever, left" height="72"> |
| wall | lever, right | <img src="objects/castleroom-trap/lever-right.webp" alt="lever, right" height="72"> |
| wall | boulder lever, left | <img src="objects/castleroom-trap/boulder-lever-left.webp" alt="boulder lever, left" height="72"> |
| wall | boulder lever, right | <img src="objects/castleroom-trap/boulder-lever-right.webp" alt="boulder lever, right" height="72"> |
| wall | warning sign | <img src="objects/castleroom-trap/warning-sign.webp" alt="warning sign" height="72"> |
| floor | sword | <img src="objects/castleroom-trap/sword.webp" alt="sword" height="72"> |
| 225.5 | way out | <img src="objects/castleroom-trap/way-out.webp" alt="way out" height="72"> |
| 285.6 | helmet | <img src="objects/castleroom-trap/helmet.webp" alt="helmet" height="72"> |

## 🗝️ dungeon

<img src="objects/castleroom-dungeon1/place.webp" alt="🗝️ dungeon, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | moss | <img src="objects/castleroom-dungeon1/moss.webp" alt="moss" height="72"> |
| wall | cobweb, left | <img src="objects/castleroom-dungeon1/cobweb-left.webp" alt="cobweb, left" height="72"> |
| wall | cobweb, right | <img src="objects/castleroom-dungeon1/cobweb-right.webp" alt="cobweb, right" height="72"> |
| wall | window | <img src="objects/castleroom-dungeon1/window.webp" alt="window" height="72"> |
| wall | torch | <img src="objects/castleroom-dungeon1/torch.webp" alt="torch" height="72"> |
| wall | chains 1 | <img src="objects/castleroom-dungeon1/chains-1.webp" alt="chains 1" height="72"> |
| wall | chains 2 | <img src="objects/castleroom-dungeon1/chains-2.webp" alt="chains 2" height="72"> |
| wall | shackles 1 | <img src="objects/castleroom-dungeon1/shackles-1.webp" alt="shackles 1" height="72"> |
| wall | shackles 2 | <img src="objects/castleroom-dungeon1/shackles-2.webp" alt="shackles 2" height="72"> |
| floor | puddle and drip | <img src="objects/castleroom-dungeon1/puddle-and-drip.webp" alt="puddle and drip" height="72"> |
| floor | straw | <img src="objects/castleroom-dungeon1/straw.webp" alt="straw" height="72"> |
| 234.1 | ladder | <img src="objects/castleroom-dungeon1/ladder.webp" alt="ladder" height="72"> |
| 320 | bucket | <img src="objects/castleroom-dungeon1/bucket.webp" alt="bucket" height="72"> |
| 345.8 | rat | <img src="objects/castleroom-dungeon1/rat.webp" alt="rat" height="72"> |

## ⛓️ deep dungeon

<img src="objects/castleroom-dungeon2/place.webp" alt="⛓️ deep dungeon, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | moss | <img src="objects/castleroom-dungeon2/moss.webp" alt="moss" height="72"> |
| wall | cobweb, left | <img src="objects/castleroom-dungeon2/cobweb-left.webp" alt="cobweb, left" height="72"> |
| wall | cobweb, right | <img src="objects/castleroom-dungeon2/cobweb-right.webp" alt="cobweb, right" height="72"> |
| wall | window | <img src="objects/castleroom-dungeon2/window.webp" alt="window" height="72"> |
| wall | torch | <img src="objects/castleroom-dungeon2/torch.webp" alt="torch" height="72"> |
| wall | chains | <img src="objects/castleroom-dungeon2/chains.webp" alt="chains" height="72"> |
| wall | shackles 1 | <img src="objects/castleroom-dungeon2/shackles-1.webp" alt="shackles 1" height="72"> |
| wall | shackles 2 | <img src="objects/castleroom-dungeon2/shackles-2.webp" alt="shackles 2" height="72"> |
| floor | puddle and drip | <img src="objects/castleroom-dungeon2/puddle-and-drip.webp" alt="puddle and drip" height="72"> |
| 225.5 | cage | <img src="objects/castleroom-dungeon2/cage.webp" alt="cage" height="72"> |
| 234.1 | ladder | <img src="objects/castleroom-dungeon2/ladder.webp" alt="ladder" height="72"> |
| 235 | mushrooms | <img src="objects/castleroom-dungeon2/mushrooms.webp" alt="mushrooms" height="72"> |
| 297 | candle | <img src="objects/castleroom-dungeon2/candle.webp" alt="candle" height="72"> |
| 354.4 | cell | <img src="objects/castleroom-dungeon2/cell.webp" alt="cell" height="72"> |

## 🕯️ hallway

<img src="objects/castleroom-hallway/place.webp" alt="🕯️ hallway, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | cobweb, left | <img src="objects/castleroom-hallway/cobweb-left.webp" alt="cobweb, left" height="72"> |
| wall | cobweb, right | <img src="objects/castleroom-hallway/cobweb-right.webp" alt="cobweb, right" height="72"> |
| floor | carpet | <img src="objects/castleroom-hallway/carpet.webp" alt="carpet" height="72"> |
| wall | banner 1 | <img src="objects/castleroom-hallway/banner-1.webp" alt="banner 1" height="72"> |
| wall | banner 2 | <img src="objects/castleroom-hallway/banner-2.webp" alt="banner 2" height="72"> |
| wall | banner 3 | <img src="objects/castleroom-hallway/banner-3.webp" alt="banner 3" height="72"> |
| wall | torch 1 | <img src="objects/castleroom-hallway/torch-1.webp" alt="torch 1" height="72"> |
| wall | torch 2 | <img src="objects/castleroom-hallway/torch-2.webp" alt="torch 2" height="72"> |
| wall | torch 3 | <img src="objects/castleroom-hallway/torch-3.webp" alt="torch 3" height="72"> |
| wall | torch, left | <img src="objects/castleroom-hallway/torch-left.webp" alt="torch, left" height="72"> |
| wall | torch, right | <img src="objects/castleroom-hallway/torch-right.webp" alt="torch, right" height="72"> |
| 225.5 | doors | <img src="objects/castleroom-hallway/doors.webp" alt="doors" height="72"> |

## 🐈 cat room

<img src="objects/castleroom-catroom/place.webp" alt="🐈 cat room, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| floor | rug | <img src="objects/castleroom-catroom/rug.webp" alt="rug" height="72"> |
| wall | fireplace | <img src="objects/castleroom-catroom/fireplace.webp" alt="fireplace" height="72"> |
| wall | mantel | <img src="objects/castleroom-catroom/mantel.webp" alt="mantel" height="72"> |
| wall | portrait | <img src="objects/castleroom-catroom/portrait.webp" alt="portrait" height="72"> |
| wall | window | <img src="objects/castleroom-catroom/window.webp" alt="window" height="72"> |
| wall | feather toy | <img src="objects/castleroom-catroom/feather-toy.webp" alt="feather toy" height="72"> |
| wall | banner | <img src="objects/castleroom-catroom/banner.webp" alt="banner" height="72"> |
| wall | shelf, left 1 | <img src="objects/castleroom-catroom/shelf-left-1.webp" alt="shelf, left 1" height="72"> |
| wall | shelf, left 2 | <img src="objects/castleroom-catroom/shelf-left-2.webp" alt="shelf, left 2" height="72"> |
| wall | shelf, left 3 | <img src="objects/castleroom-catroom/shelf-left-3.webp" alt="shelf, left 3" height="72"> |
| wall | shelf, right 1 | <img src="objects/castleroom-catroom/shelf-right-1.webp" alt="shelf, right 1" height="72"> |
| wall | shelf, right 2 | <img src="objects/castleroom-catroom/shelf-right-2.webp" alt="shelf, right 2" height="72"> |
| wall | shelf, right 3 | <img src="objects/castleroom-catroom/shelf-right-3.webp" alt="shelf, right 3" height="72"> |
| floor | mat for the bowls | <img src="objects/castleroom-catroom/mat-for-the-bowls.webp" alt="mat for the bowls" height="72"> |
| 225.5 | door 2 | <img src="objects/castleroom-catroom/door-2.webp" alt="door 2" height="72"> |
| 225.5 | door 3 | <img src="objects/castleroom-catroom/door-3.webp" alt="door 3" height="72"> |
| 228.8 | cat tree, left | <img src="objects/castleroom-catroom/cat-tree-left.webp" alt="cat tree, left" height="72"> |
| 228.8 | cat tree, right | <img src="objects/castleroom-catroom/cat-tree-right.webp" alt="cat tree, right" height="72"> |
| 228.8 | cat 2 | <img src="objects/castleroom-catroom/cat-2.webp" alt="cat 2" height="72"> |
| 228.8 | cat 5 | <img src="objects/castleroom-catroom/cat-5.webp" alt="cat 5" height="72"> |
| 231.4 | cat 3 | <img src="objects/castleroom-catroom/cat-3.webp" alt="cat 3" height="72"> |
| 241.6 | firewood | <img src="objects/castleroom-catroom/firewood.webp" alt="firewood" height="72"> |
| 241.6 | window seat | <img src="objects/castleroom-catroom/window-seat.webp" alt="window seat" height="72"> |
| 241.6 | cat 4 | <img src="objects/castleroom-catroom/cat-4.webp" alt="cat 4" height="72"> |
| 254.1 | bowls | <img src="objects/castleroom-catroom/bowls.webp" alt="bowls" height="72"> |
| 258.7 | cat 1 | <img src="objects/castleroom-catroom/cat-1.webp" alt="cat 1" height="72"> |
| 282.5 | lantern | <img src="objects/castleroom-catroom/lantern.webp" alt="lantern" height="72"> |
| 282.5 | yarn, yellow | <img src="objects/castleroom-catroom/yarn-yellow.webp" alt="yarn, yellow" height="72"> |
| 295 | basket | <img src="objects/castleroom-catroom/basket.webp" alt="basket" height="72"> |
| 295 | cat 6 | <img src="objects/castleroom-catroom/cat-6.webp" alt="cat 6" height="72"> |
| 316.8 | toy mouse | <img src="objects/castleroom-catroom/toy-mouse.webp" alt="toy mouse" height="72"> |
| 343 | yarn, red | <img src="objects/castleroom-catroom/yarn-red.webp" alt="yarn, red" height="72"> |
| 343 | rope bridge | <img src="objects/castleroom-catroom/rope-bridge.webp" alt="rope bridge" height="72"> |
| 358.2 | box | <img src="objects/castleroom-catroom/box.webp" alt="box" height="72"> |
| 387.7 | yarn, green | <img src="objects/castleroom-catroom/yarn-green.webp" alt="yarn, green" height="72"> |
| front | way S | <img src="objects/castleroom-catroom/way-s.webp" alt="way S" height="72"> |

## 🐴 stable

<img src="objects/castleroom-stable/place.webp" alt="🐴 stable, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | cobweb, left | <img src="objects/castleroom-stable/cobweb-left.webp" alt="cobweb, left" height="72"> |
| wall | cobweb, right | <img src="objects/castleroom-stable/cobweb-right.webp" alt="cobweb, right" height="72"> |
| wall | round window | <img src="objects/castleroom-stable/round-window.webp" alt="round window" height="72"> |
| wall | stall 1 | <img src="objects/castleroom-stable/stall-1.webp" alt="stall 1" height="72"> |
| wall | stall 3 | <img src="objects/castleroom-stable/stall-3.webp" alt="stall 3" height="72"> |
| wall | stall 4 | <img src="objects/castleroom-stable/stall-4.webp" alt="stall 4" height="72"> |
| wall | stall 2 | <img src="objects/castleroom-stable/stall-2.webp" alt="stall 2" height="72"> |
| wall | hay loft | <img src="objects/castleroom-stable/hay-loft.webp" alt="hay loft" height="72"> |
| wall | lantern | <img src="objects/castleroom-stable/lantern.webp" alt="lantern" height="72"> |
| wall | horseshoes | <img src="objects/castleroom-stable/horseshoes.webp" alt="horseshoes" height="72"> |
| wall | saddles | <img src="objects/castleroom-stable/saddles.webp" alt="saddles" height="72"> |
| wall | bridle | <img src="objects/castleroom-stable/bridle.webp" alt="bridle" height="72"> |
| floor | straw | <img src="objects/castleroom-stable/straw.webp" alt="straw" height="72"> |
| 225.5 | swallows | <img src="objects/castleroom-stable/swallows.webp" alt="swallows" height="72"> |
| 225.5 | door 2 | <img src="objects/castleroom-stable/door-2.webp" alt="door 2" height="72"> |
| 225.5 | door 3 | <img src="objects/castleroom-stable/door-3.webp" alt="door 3" height="72"> |
| 227.2 | horse 1 | <img src="objects/castleroom-stable/horse-1.webp" alt="horse 1" height="72"> |
| 227.2 | horse 2 | <img src="objects/castleroom-stable/horse-2.webp" alt="horse 2" height="72"> |
| 227.2 | horse 3 | <img src="objects/castleroom-stable/horse-3.webp" alt="horse 3" height="72"> |
| 251.9 | ladder | <img src="objects/castleroom-stable/ladder.webp" alt="ladder" height="72"> |
| 266.1 | hen, brown | <img src="objects/castleroom-stable/hen-brown.webp" alt="hen, brown" height="72"> |
| 279.6 | sunbeam | <img src="objects/castleroom-stable/sunbeam.webp" alt="sunbeam" height="72"> |
| 295 | water trough | <img src="objects/castleroom-stable/water-trough.webp" alt="water trough" height="72"> |
| 295 | buckets | <img src="objects/castleroom-stable/buckets.webp" alt="buckets" height="72"> |
| 295 | hen, white | <img src="objects/castleroom-stable/hen-white.webp" alt="hen, white" height="72"> |
| 333.6 | hay heap | <img src="objects/castleroom-stable/hay-heap.webp" alt="hay heap" height="72"> |
| front | way S | <img src="objects/castleroom-stable/way-s.webp" alt="way S" height="72"> |

## 🍽️ dining room

<img src="objects/castleroom-dining/place.webp" alt="🍽️ dining room, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | cobweb, left | <img src="objects/castleroom-dining/cobweb-left.webp" alt="cobweb, left" height="72"> |
| wall | cobweb, right | <img src="objects/castleroom-dining/cobweb-right.webp" alt="cobweb, right" height="72"> |
| wall | fireplace | <img src="objects/castleroom-dining/fireplace.webp" alt="fireplace" height="72"> |
| 74.6 | chair, far end | <img src="objects/castleroom-dining/chair-far-end.webp" alt="chair, far end" height="72"> |
| 88.3 | chair, left 4 | <img src="objects/castleroom-dining/chair-left-4.webp" alt="chair, left 4" height="72"> |
| 88.3 | chair, right 4 | <img src="objects/castleroom-dining/chair-right-4.webp" alt="chair, right 4" height="72"> |
| 95.9 | candelabra 2 | <img src="objects/castleroom-dining/candelabra-2.webp" alt="candelabra 2" height="72"> |
| 104.6 | chair, left 3 | <img src="objects/castleroom-dining/chair-left-3.webp" alt="chair, left 3" height="72"> |
| 104.6 | chair, right 3 | <img src="objects/castleroom-dining/chair-right-3.webp" alt="chair, right 3" height="72"> |
| 126.3 | chair, left 2 | <img src="objects/castleroom-dining/chair-left-2.webp" alt="chair, left 2" height="72"> |
| 126.3 | chair, right 2 | <img src="objects/castleroom-dining/chair-right-2.webp" alt="chair, right 2" height="72"> |
| 140.1 | candelabra 1 | <img src="objects/castleroom-dining/candelabra-1.webp" alt="candelabra 1" height="72"> |
| 156.6 | chair, left 1 | <img src="objects/castleroom-dining/chair-left-1.webp" alt="chair, left 1" height="72"> |
| 156.6 | chair, right 1 | <img src="objects/castleroom-dining/chair-right-1.webp" alt="chair, right 1" height="72"> |
| 176.7 | table | <img src="objects/castleroom-dining/table.webp" alt="table" height="72"> |
| 206.8 | chair, near end | <img src="objects/castleroom-dining/chair-near-end.webp" alt="chair, near end" height="72"> |
| 225.5 | doors | <img src="objects/castleroom-dining/doors.webp" alt="doors" height="72"> |
| front | dark | <img src="objects/castleroom-dining/dark.webp" alt="dark" height="72"> |
| front | fire light | <img src="objects/castleroom-dining/fire-light.webp" alt="fire light" height="72"> |
| front | torches | <img src="objects/castleroom-dining/torches.webp" alt="torches" height="72"> |
| front | way S | <img src="objects/castleroom-dining/way-s.webp" alt="way S" height="72"> |

## 🍲 kitchen

<img src="objects/castleroom-kitchen/place.webp" alt="🍲 kitchen, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | cobweb, left | <img src="objects/castleroom-kitchen/cobweb-left.webp" alt="cobweb, left" height="72"> |
| wall | cobweb, right | <img src="objects/castleroom-kitchen/cobweb-right.webp" alt="cobweb, right" height="72"> |
| wall | hearth | <img src="objects/castleroom-kitchen/hearth.webp" alt="hearth" height="72"> |
| wall | onions, garlic and herbs | <img src="objects/castleroom-kitchen/onions-garlic-and-herbs.webp" alt="onions, garlic and herbs" height="72"> |
| wall | shelf of plates | <img src="objects/castleroom-kitchen/shelf-of-plates.webp" alt="shelf of plates" height="72"> |
| wall | shelf of crocks | <img src="objects/castleroom-kitchen/shelf-of-crocks.webp" alt="shelf of crocks" height="72"> |
| 225.5 | oven | <img src="objects/castleroom-kitchen/oven.webp" alt="oven" height="72"> |
| 225.5 | door 2 | <img src="objects/castleroom-kitchen/door-2.webp" alt="door 2" height="72"> |
| 225.5 | door 3 | <img src="objects/castleroom-kitchen/door-3.webp" alt="door 3" height="72"> |
| 228.8 | barrels | <img src="objects/castleroom-kitchen/barrels.webp" alt="barrels" height="72"> |
| 228.8 | flour sack | <img src="objects/castleroom-kitchen/flour-sack.webp" alt="flour sack" height="72"> |
| 243.5 | cook, chopping | <img src="objects/castleroom-kitchen/cook-chopping.webp" alt="cook, chopping" height="72"> |
| 256.4 | cauldron | <img src="objects/castleroom-kitchen/cauldron.webp" alt="cauldron" height="72"> |
| 256.4 | cook, stirring | <img src="objects/castleroom-kitchen/cook-stirring.webp" alt="cook, stirring" height="72"> |
| 266.1 | pots and pans | <img src="objects/castleroom-kitchen/pots-and-pans.webp" alt="pots and pans" height="72"> |
| 333.6 | worktable | <img src="objects/castleroom-kitchen/worktable.webp" alt="worktable" height="72"> |
| front | fire light | <img src="objects/castleroom-kitchen/fire-light.webp" alt="fire light" height="72"> |
| front | way S | <img src="objects/castleroom-kitchen/way-s.webp" alt="way S" height="72"> |

## ⚒️ smithy

<img src="objects/castleroom-smithy/place.webp" alt="⚒️ smithy, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | cobweb, left | <img src="objects/castleroom-smithy/cobweb-left.webp" alt="cobweb, left" height="72"> |
| wall | cobweb, right | <img src="objects/castleroom-smithy/cobweb-right.webp" alt="cobweb, right" height="72"> |
| wall | tools on pegs | <img src="objects/castleroom-smithy/tools-on-pegs.webp" alt="tools on pegs" height="72"> |
| wall | rack of swords | <img src="objects/castleroom-smithy/rack-of-swords.webp" alt="rack of swords" height="72"> |
| wall | shield | <img src="objects/castleroom-smithy/shield.webp" alt="shield" height="72"> |
| 225.5 | forge | <img src="objects/castleroom-smithy/forge.webp" alt="forge" height="72"> |
| 225.5 | door 2 | <img src="objects/castleroom-smithy/door-2.webp" alt="door 2" height="72"> |
| 225.5 | door 3 | <img src="objects/castleroom-smithy/door-3.webp" alt="door 3" height="72"> |
| 234.1 | coal heap | <img src="objects/castleroom-smithy/coal-heap.webp" alt="coal heap" height="72"> |
| 266.1 | anvil | <img src="objects/castleroom-smithy/anvil.webp" alt="anvil" height="72"> |
| 266.1 | smith | <img src="objects/castleroom-smithy/smith.webp" alt="smith" height="72"> |
| 316.8 | water barrel | <img src="objects/castleroom-smithy/water-barrel.webp" alt="water barrel" height="72"> |
| front | dark | <img src="objects/castleroom-smithy/dark.webp" alt="dark" height="72"> |
| front | way S | <img src="objects/castleroom-smithy/way-s.webp" alt="way S" height="72"> |

## 🌀 spiral staircase

<img src="objects/castleroom-spiral/place.webp" alt="🌀 spiral staircase, as it starts" width="480">

| depth | object | picture |
|---:|---|---|
| wall | arrow slit 1 | <img src="objects/castleroom-spiral/arrow-slit-1.webp" alt="arrow slit 1" height="72"> |
| wall | arrow slit 2 | <img src="objects/castleroom-spiral/arrow-slit-2.webp" alt="arrow slit 2" height="72"> |
| wall | arrow slit 3 | <img src="objects/castleroom-spiral/arrow-slit-3.webp" alt="arrow slit 3" height="72"> |
| 56.7 | door 3 | <img src="objects/castleroom-spiral/door-3.webp" alt="door 3" height="72"> |
| 148.8 | door 2 | <img src="objects/castleroom-spiral/door-2.webp" alt="door 2" height="72"> |
| 263.3 | column | <img src="objects/castleroom-spiral/column.webp" alt="column" height="72"> |
| 281.6 | steps | <img src="objects/castleroom-spiral/steps.webp" alt="steps" height="72"> |
| front | steps, in front | <img src="objects/castleroom-spiral/steps-in-front.webp" alt="steps, in front" height="72"> |
| front | dark | <img src="objects/castleroom-spiral/dark.webp" alt="dark" height="72"> |
| front | way S | <img src="objects/castleroom-spiral/way-s.webp" alt="way S" height="72"> |
