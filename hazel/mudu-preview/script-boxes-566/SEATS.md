# Dress Up: what one can sit on

The audit of ticket 413 (Matthew, 2026-10-19: "audit of all objects to determine: do they have the approximate size of a seat, sometimes, at any scale? (rocks often do); do they have a reasonable top surface for sitting (rocks do, but cauldrons do not, since they would burn us...)"; "don't sit on bushes -- they would poke us too much"; "For each object we don't offer click to sit, let's consider it -- and lean towards doing it, unless there's a reason to discuss").

**Since 0.0.337**, a thing with a top to sit on is a seat wherever it stands, a place's own or put there, when its top is a seat's height for the friend at their size. That makes 44 more objects seats in 10 places, beyond the places' own seats, and every placed rock, toadstool, log, bench, barrel and treasure chest a seat at some of its sizes. Click it with 👆 on (a press let go where it is: a longer press-and-move drags it, as before), drag the friend onto it, or use its 🪑 chip in 🏞️ Background.

- **How a seat is found** (`thingSeats`, js/17-moving-and-climbing.js): from the thing's box as the friend meets it (`solidsHere`, measured from its drawing), so the seat is where the thing is drawn. Its height is where on the thing the seat is (its top for a rock or a barrel, half way up for a bench or a chair, under their backs), and it counts as a seat when that height is 0.05 to 0.42 of the friend's height at the thing's depth, and it is at least 0.07 of it across.
- **Why those numbers:** the places' own seats, measured the same way at the default friend. A front step is 0.04 of the friend's height; mountain rocks 0.06 to 0.09; chairs, logs and mushrooms 0.11 to 0.13; toadstools, the wagon and barrels 0.2 to 0.23; couches 0.34 to 0.38; the swing 0.46 and a throne on its dais 0.52. The narrowest, a mushroom, is 0.08 across. The friend's head is big and their hips low, so a chair's seat is about an eighth of their height.
- **A bigger or smaller friend** changes which things are seats: the test is against the friend as they are now.
- **Not a seat when** 🧱 "placed things are in your way" is off: the things put in a place then have no box, and so no seat.
- Measured on 0.0.336 with the friend at its default size, in the app's own browser.

## Things one can put anywhere

🔎 size goes from 25% to 300%; each was measured at 50%, 100%, 150%, 200% and 300% (the barrel at 25% too), in the empty room, its top as a share of the friend's height there.

| thing | a seat? | at sizes | why |
|---|---|---|---|
| 🪨 rock | yes | 50% to 200% (at 300% its top is 0.65) | a top to sit on (Matthew's example) |
| 🍄 toadstool | yes | 50% to 100% | its cap; the fairy ring's are seats already |
| 🪵 log | yes | 50% to 200% | the camp's are seats already |
| 🪑 bench | yes | 50% to 150% | its seat is half way up, under its back; faces front only |
| 🛢️ barrel | yes | 25% (at 50% its top is 0.43: just over) | a flat top; placed barrels are big beside the friend |
| 💰 treasure chest | yes | 50% (at 100% its lid is 0.56) | a flat lid |
| 🌿 bush | no | | prickly (Matthew: "they would poke us too much") |
| 🔥 campfire | no | | hot |
| 🫕 cauldron | no | | hot (Matthew's example) |
| 🌳 tree, 🌲 pine, 🌳 dead tree | no | | too tall at every size; no top |
| 💀 skull on a stake, 🪵 fence of stakes | no | | sharp |
| 💀 skeleton | no | | not a seat, and spooky |
| 🪧 sign, 🧭 sign with directions | no | | a post with boards |
| ⚓ anchor | no | | hooks and a chain |
| 🎚️ lever, 🫧 bubble maker, 🕳️ trap door | no | | for pulling, for bubbles, a hole |
| 🪸 coral | no | | sharp, and alive |
| ⭐ starfish, 🐟 fish | no | | alive |
| 🐄 cow, 🐖 pig, 🐔 chicken | no | | alive (ticket 504: not in `SIT_TOPS`; a ride on a cow was not asked for) |
| 🐚 shell, 🌿 kelp, 🌾 sea grass, 🪭 sea fan, 🥬 sea lettuce | no | | small, soft or alive; they have only a small box at their foot |
| 🌱 moss, 🪨 lichen, 🧪 potion | no | | flat on the ground or on a rock, or too small; nothing stands up |
| 🏚️ barn, 🪵 fence | no | | (ticket 502, not measured) a building; a fence's rails or a wall's top, to be thought about with building fences in play |
| 📦 from a place | as the thing it is | | a lifted thing keeps its name, so the rules below apply to it |

## The places' own objects

Each kind as it stands in the places: all 38 at their default settings, and every room of the default castle. Kinds that stand only in rooms the default castle does not have (the kitchen's stove and oven, the smithy's forge, anvil and coal heap, and the like) are judged by their kind. **Own seat** is a seat the place's own drawing gives (`SEATS`), unchanged; **new** is a seat since 0.0.337.

| object | where | a seat? | why |
|---|---|---|---|
| rock | underwater, deep sea, mountain trail, jungle waterfall, castle | **new** where its top is a seat's height (21 rocks); the mountain trail's three nearest were seats already | Matthew's example. Too big for a seat: one rock in the deep sea, one on the mountain trail, two at the waterfall |
| stone | fairy forest | **new** (2) | low, flat-topped |
| toadstool | fairy forest | **new** (11 more); three were seats already | caps to sit on, as the three biggest were |
| mushroom | path through the woods | **new** (the two inner ones); the outer two were seats already | |
| log | camp site, the grass hut's center | **new** (the camp's near log); four were seats already | |
| barrel | castle | **new** (the far one); two were seats already | |
| treasure chest | deep sea, cave behind the falls | **new** (5) | a flat lid |
| toy chest | inside the tree house | **new** | a flat lid |
| stump | the grass hut's bedroom | no: too high | its top is 0.49 of the friend's height there, a bedside table; it would be a seat if smaller |
| hay cart | castle | no: too high | its hay is 0.66 of the friend's height |
| crate | castle | own seat | |
| chair, couch, throne, picnic bench, beach chair, wagon, front step, swing, big root | their places | own seats | |
| bed, bunk beds, beach blanket | their places | own seats, to lie on (ticket 299) | |
| bush, green bush, pink bush, white bush, yellow bush | many places | no | prickly |
| cauldron, soup pot, campfire, fire, fireplace, stove, oven, forge | huts, castle rooms, camp | no | hot |
| big stalagmite, small stalagmite, crystals | cave behind the falls | no | sharp |
| haystack, tractor, windpump | farm yard | no | (ticket 502, not measured) a haystack taller than a seat, a machine, a tower; the farm yard's water trough is the stable's |
| headstone, cross, stag stone | cemetery | no | graves, and a standing stone |
| skeleton, skull | witch's woods | no | not seats, and spooky |
| bucket, buckets, basket, watering can | yard, castle rooms | no | one would sit in it, or crush it |
| truck, blocks, teddy, ball, frisbee, helmet | house and yard, trap room | no | toys and things that would break or roll |
| sand castle | beach | no | it would be crushed |
| snake tank, cage, crow in a cage, frogs in jars | witch's hut, dungeon | no | glass and animals |
| shipwreck, vent | underwater, deep sea | no | too big; a vent is hot water |
| trees, palms, pines, willow, umbrella, lamp, plants in pots | many | no | too tall; no top |
| armor, bookcase, potion shelves, cat tree, column, telescope, easel, drum kit, keyboard, ladder | castle rooms, tree rooms | no | too tall, or not for sitting |
| well, fountain | castle, greenhouse | **for Matthew** | see below |
| table, tea table, worktable, picnic table | huts, underwater | **for Matthew** | see below |
| woodpile | grass hut | **for Matthew** | see below |
| chest of gold | the king's room | **for Matthew** | see below |
| tree trunk, hatch lever, lantern, squirrel | the round room at the top of the tree | no | (ticket 516) a living trunk with no top, a lever on its post, lanterns that hang, a creature; the room has no seats until its furnishing (ticket 517) |
| everything else listed in [OBJECTS.md](OBJECTS.md) | | no | flat on the floor (rugs, straw, moss, pebbles), on a wall, in the air, light or weather, a way out, or a creature: nothing to sit on |

## For Matthew

Each of these could be a seat; each has a reason to ask first.

1. **Tables** (the healer's tea table and worktable, the potion room's table, the picnic table under the sea): flat and strong, and the tea table's top is a seat's height (0.27). Do we sit on tables?
2. **The well and the fountain:** one sits on a rim, not on the top, and the box's top is the roof or the spout (1.0 and 0.62), so each needs a rim seat of its own. A well is somewhere to fall into: a seat on its rim, or none?
3. **The woodpile** (the grass hut, 0.28): logs, but stacked and uneven. A seat, as the logs by the fire are?
4. **The king's chest of gold** (0.38): a chest, but open and full of coins. A seat, or only for looking at?
5. **The stump by the bed** (0.49) and **the hay cart** (0.66): just too high at their size in their places. Raise the highest seat from 0.42, or leave them as a bedside table and a cart?
6. **The anvil and the coal heap** (the smithy): an anvil is sturdy, but in a smithy beside a forge; a coal heap is dirty. Neither is a seat yet, and neither was measured (the default castle has no smithy).
7. **Sitting on the floor:** rugs, straw, moss and blankets are flat. The beach blankets are already for lying on (ticket 299). Should the others offer a place to sit, cross-legged, on the floor?
