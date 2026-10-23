# Dress Up: what is drawn into each place

The audit of ticket 284 (Matthew, 2026-10-18: "audit each scene for objects that we can offer as new placed objects"; "produce a list of things 'baked into' the background draw that aren't made into objects under the current inspection system, and work towards making each object show up that way, and be placeable").

- [OBJECTS.md](OBJECTS.md) lists what **stands** in each place: its objects. Since 0.0.208, every one of them can be placed in any place: 🏞️ Background, the things' group **📦 from the places**. Ways out, doors and ladders are not offered.
- **Since 0.0.217, wall and floor things are objects of their own kind** (`wallObj`, `floorObj`): the castle rooms' cobwebs and moss, and everything listed below for 🕳️ the trap room, 🗝️ the dungeon and ⛓️ the deep dungeon, are objects now.
- **Since 0.0.224, the living room, the witch's and the healer's huts, and part of the tree house rooms** (their rows below). Since 0.0.225 the rest of the tree rooms too: room 2's jars, room 4's map and pennants and room 6's records moved clear of the trunk first (Matthew's call), then made objects.
- **Since 0.0.221, seven more castle rooms, and since 0.0.223 the cat room and the stable: every room of the castle** (the hallway, the dining room, the kitchen, the smithy, the spiral staircase, the king's room, the greenhouse, the cat room, the stable): their rows below now say what is still drawn in, and what became an object. A thing drawn by statements of its own is wrapped where it is drawn (`wallSince`, `floorSince`).
- **Since 0.0.293, the last outdoor batch** (ticket 284's long run, 2026-10-18 night): under the sea and in the deep sea the light rays (wall objects) and the sand's specks and ripples (floor objects); the cemetery's stones by the brook (a floor object) and its flowers in rows by depth; the lake's sun on the water and pebbles (floor objects) and its grass in rows by depth; the mountain trail's far lake and village (a wall object); the waterfall's flowers by the trunks in rows by depth; the witch's woods' grass in rows by depth, and, at Matthew's word that its trees and skulls were missing from the list, each of its 17 dead trees and 11 skulls an object of its own (with 0.0.287: their drawings shared and placeable). Ground itself (paths, roads, brooks, streams, pools, waves) stays terrain.
- **This page lists the rest:** things drawn into a place's background that are not objects, so they cannot be hidden with an 👁️ chip, seen by 🔬 Inspection, or placed. Each is a step toward the ticket's end: every object in each scene is something that can also be placed.
- As of 0.0.208, from a reading of each place's drawing code (nothing was run). **Where** is wall, hanging, flat (on the floor or the ground) or sky. **In the code** is the part of `js/` and the line, at that version: check it before relying on it. The sun, moon, stars and clouds of every outdoor place are left out.

## Next to make into objects, and placeable

1. **The trap room's lever** (13:192): a switch to place, that moves when pressed, and that the world designer wires to what it does: trap doors opening and closing, for a start (Matthew's example). **Since 0.0.209 a thing to place**, the 🎚️ lever, drawn by the trap room's own `leverSvg`; the trap room's own lever is still drawn into its wall.
2. **A wall torch** (trap room, dungeons, hallway, king's room: one shared drawing): a light that flickers with the LFO.
3. **A pit, or a trap door in the floor** (the trap room's pits are already doorways with a shape setting): placed anywhere, a way down into another room, and what the lever opens and closes. **Since 0.0.209 a thing to place**, the 🕳️ trap door, the trap room's `pitSvg` when open; it leads nowhere yet (a way down is the next step).
4. **A hanging lantern** (tree house room 3, the hut's rooms, the stable's post).
5. **A rug** (the living room's, the healer's, the cat room's, the hut's: one shared drawing with colour themes): a flat thing.
6. **A framed picture** (living room, tree rooms, the hut): already a doorway into its place; placing one places a doorway, so it needs a way back.
7. **A banner** in its castle's colours (king's room, hallway, cat room).
8. **A wall clock**, its pendulum swinging (living room, the cat room's mantel).
9. **A fireplace or hearth** (healer, kitchen, dining room, cat room): a fire on a wall, as the campfire is not.
10. **A shelf of potions or jars** (the witch's, the healer's, the hut's potion room, the tree house kitchen).
11. **A hanging bundle of herbs** (the witch's, the healer's, the hut's potion room, the kitchen).
12. **A crow in a hanging cage**, swinging (the witch's hut).
13. **Shackles and chains**, with a hanging skeleton or without (the dungeons): the placed skeleton has no hanging pose.
14. **Bats**, flying (the witch's woods).
15. **A beach blanket**, striped (the beach).

Runners-up: the dreamcatcher, the wind chime, the tent's teddy, books, backpack and flashlight, the crossed swords and shield.

## Placeable, with a caveat

- **The stable's horse** is only its head over its stall door: the stall is drawn into the wall. A whole horse wants a kind of its own.
- **Lights and fliers** (a sunbeam, swallows, butterflies, parrots) can be placed, and float where they are put.
- **Drawn the same, but not alike everywhere:** the forest's sunbeams, the hut's butterflies, the tent's flashlight (only its beam is an object) and the hallway's torches are drawn into their places, while the same kinds of thing are objects elsewhere.

## By place

Outdoors

| place | drawn in, not an object | where | moves | in the code |
|---|---|---|---|---|
| 🌲 forest | (objects since 0.0.252: the sunbeams and the meadow flowers, the flowers in rows by depth since ticket 297) | | | |
| 🐠 underwater, 🪸 deep sea | (since 0.0.293, objects: the light rays; the sand's specks and the deep sea's ripples) | | | |
| 🏖️ beach | waves and foam: the water's edge, terrain (since 0.0.250, objects: the two striped blankets) | flat | | 15:632 |
| 🌊 open water | nothing (since 0.0.250, objects: the sun glitter; the island and the sailboat taken away in 0.0.324, ticket 212) | | | |
| 🧙 witch's woods | nothing (since 0.0.250, the bats; since 0.0.293, the grass in rows by depth, and each dead tree and skull its own object) | | | |
| 🌿 cemetery | the brook with its ripples: terrain (since 0.0.293, objects: the stones by the brook; the flowers in rows by depth) | flat | the brook | 15:338 |
| 🦆 lake | the little beach: terrain (since 0.0.250, the lily pads; since 0.0.293, the sun on the water, the pebbles, the grass in rows by depth) | flat | | 09:241 |
| 🏠 house and yard | the driveway and stone walk: terrain (since 0.0.250, objects: the frisbee and the rope) | flat | | 09:419 |
| 🌳 tree house | the stream with its ripples: terrain (objects since 0.0.252: the flowers, in rows by depth since ticket 297) | flat | the stream | 11:26 |
| 🌲 forest clearing | the paths to each far thing, the front path: terrain (objects since 0.0.252: the flowers, in rows by depth since ticket 297) | flat | | 12:22 |
| 🧚 fairy forest | the grass inside the ring: terrain (objects since 0.0.252: the flowers and grass, in rows by depth since ticket 297) | flat | | 11:1424 |
| ⛰️ mountain trail | the trail: terrain (since 0.0.293, an object: the far lake and village) | flat | | 14:39 |
| 💦 jungle waterfall | the falls (the way into the cave) and the mossy cliff, a doorway and the backdrop; the pool with its ripples, terrain (since 0.0.293, the flowers by the trunks in rows by depth) | backdrop, flat | yes | 14:106 |
| 🏕️ camp site | the dirt clearing round the fire: terrain | flat | | 14:322 |
| 🥾 path through the woods | the path: terrain (since 0.0.293, an object: its pebbles) | flat | | 14:716 |
| 🛤️ forest crossing | the paths, the meeting place, leaf litter: terrain | flat | | 14:1340 |
| 🏰 castle | the road with its ruts and edge stones: terrain (objects since 0.0.252: the flowers and grass, in rows by depth since ticket 297, and the birds, flying since 0.0.288) | flat | | 12:78 |
| 🛖 grass hut | the path: terrain (objects since 0.0.252: the meadow flowers, in rows by depth since ticket 297, and the butterflies) | flat | | 11:1798 |

Inside

| place | drawn in, not an object | where | moves | in the code |
|---|---|---|---|---|
| 🪜 every tree house room | the window and the swaying limb outside it (a doorway); pictures (doorways) (since 0.0.224, objects: the limbs coming out of the walls) | wall | the limb | 11:563-606 |
| 🪜 tree house room 2, the kitchen | nothing (since 0.0.225, an object: the shelves of jars, moved clear of the trunk) | | | |
| 🪜 tree house room 3, the books | nothing (since 0.0.224, an object: the lantern on its cord) | | | |
| 🪜 tree house room 4, the lookout | nothing (since 0.0.225, objects: the treasure map, moved clear of the trunk, and the pennants, their cord tied to it) | | | |
| 🪜 tree house room 5, the art room | nothing (since 0.0.224, objects: the paint spills; the pinned drawings, with more than 10 things) | | | |
| 🪜 tree house room 6, the music room | nothing (since 0.0.225, an object: the records, moved clear of the trunk) | | | |
| ⭕ the round room at the top of the tree | the thatch's underside, its rings and bright gaps, the rafters, the eaves' fringe, the plank wall, the wall plate, the skirting, the floor's rings (ticket 516; objects from the start: the two windows (doorways), the limbs in the thatch, the daylight, the sunlight on the floor, two lanterns, the tree trunk with its limbs, the squirrel, the hatch (a doorway), its lever, the sign, the dark) | wall, flat | | 11:1001 (topShellInSvg) |
| 🛋️ living room | the window's view (a doorway); pictures (doorways) (since 0.0.224, objects: the rug, the wall clock, the window's frame with its curtains, rod and sill) | wall | | 11:240 |
| 🧙 inside the witch's hut | the red door with its "🌲 out" sign (a doorway) (since 0.0.224, objects: the round window, two windows, the herbs on the beam (since 0.0.248 each bundle its own, "drying herbs 1" to 8), two cobwebs, the shelf of potions, the frogs in jars, the crows in their cages) | wall | | 11:900 |
| 🌿 inside the healer's hut | the arched door with its "🌲 out" sign (a doorway) (since 0.0.224, objects: the rug, the round window and its sunbeam, the herbs (since 0.0.248 each bundle its own, "drying herbs 1" to 10), the fireplace, the things on its mantel) | wall | | 11:1140 |
| ⛺ inside a tent | the open door onto the camp and its rolled flaps (a doorway) (since 0.0.249, objects: the two mesh windows; tent 1's two sleeping bags and flashlight; tent 2's blanket, teddy bear and books; tent 3's map, binoculars and backpack) | wall | | 14:387-560 |
| 🪨 cave behind the falls | the falls from behind and the mist (a doorway) (since 0.0.249, objects: the gems, three clusters of crystals, the light from the falls, the wet floor, the pebbles) | wall | yes | 14:572-629 |
| 💭 our imagination | the framed pictures (doorways) (since 0.0.249, objects: the glow, the map board with its columns' names and dotted lines) | wall | | 14:909-917 |
| 🛖 hut rooms, all four | the stick wall, the twine, the fringe of the eaves, the straw floor | wall, flat | | 11:1675-1681 |
| 🛖 inside the hut | two doors with signs; two pictures (doorways); the window's view (a doorway) (since 0.0.248, objects: the rug, the window, the wind chime, the lantern) | wall | | 11:1841-1859 |
| 🔥 the hut center | three doors with signs (doorways) (since 0.0.248, objects: the daylight, two lanterns, the smoke) | wall | | 11:1887-1893 |
| 🛏️ the hut's bedroom | the door (a doorway) (since 0.0.248, objects: the rug, the window, the dreamcatcher; the bed and the stump collide) | wall | | 11:1910-1923 |
| 🧪 the hut's potion room | the door (a doorway) (since 0.0.248, objects: three shelves of potions, the beam, six drying herbs) | wall | | 11:1965-1987 |
| 🔲 empty room, 🫧 bubble space | nothing | | | |

The castle's rooms

| room | drawn in, not an object | where | moves | in the code |
|---|---|---|---|---|
| every room but the greenhouse, the cat room and the spiral staircase | cobwebs; moss on the walls (the trap room and the dungeons) | wall | | 13:113-118 |
| 🕳️ trap room | two torches; **the lever**; the "⚠️ MIND THE FLOOR ⚠️" plaque; two spiked pits (doorways) with their 🕳️ signs; a dropped sword | wall, flat | the torches | 13:191-217 |
| 🗝️ dungeon | a barred window and its moonbeam; a torch; shackles and chains, with two hanging skeletons (with 🦴 at 1 and 2 or more); empty chains; a drip and its puddle; straw; scattered bones | wall, flat | the torch, the drip | 13:236-299 |
| ⛓️ deep dungeon | the window, the torch and the drip, as in the dungeon; a hanging skeleton (🦴 at 4 or more) and its chains | wall | yes | 13:236-251 |
| 🕯️ hallway | the stairwell at its far end, or the dead-end wall (since 0.0.221, objects: the carpet, three banners, five torches) | wall | | 13:380-450 |
| 🍽️ dining room | nothing (since 0.0.221, an object: the great fireplace and its fire) | | | |
| 🍲 kitchen | the beams (since 0.0.221, objects: the hearth with its fire and mantel, the strings of onions, garlic and herbs, the shelf of plates, the shelf of crocks) | wall | | 13:710 |
| ⚒️ smithy | soot and timbers (since 0.0.221, objects: the tools on pegs, the rack of swords, the shield) | wall | | 13:830 |
| 🌀 spiral staircase | the steps behind the column; the round floor (since 0.0.221, objects: three arrow slits and their beams) | wall, flat | | 13:950-980 |
| 👑 king's room | nothing (since 0.0.221, objects: the stained-glass window, two banners, two crossed swords and shields, two torches, the carpet) | | | |
| 🌿 greenhouse | the sky and the trees beyond the glass (since 0.0.221, objects: the moss, the fallen petals, nine vines up the posts, nine hanging from the roof) | backdrop | the trees | 13:1090 |
| 🐈 cat room | the high walkway and the cat flap with its "🐾 out" sign (since 0.0.223, objects: the rug, the fireplace, the mantel, the portrait, the window and curtains, the feather toy, the banner, six shelves, the mat for the bowls) | wall | | 13:1520 |
| 🐴 stable | ceiling beams, posts (since 0.0.223, objects: the round window, four stalls, the hay loft, the lantern, the horseshoes, the saddles, the bridle, the straw) | wall | | 13:1740 |

Not read closely: what is drawn inside a single object (the tree house's tree, the castle, the clearing's far things), and the long rooms' standing things (the greenhouse's plants, the cat trees, the dining table, the spiral's steps, the stable's animals), which are objects already. Some things are drawn only at some settings: the hanging skeletons (🦴), the pinned drawings (more than 10 toys), the frisbee and the rope (7 toys or more), the hallway's dead end. The greenhouse's code promises a bench (13:1218) that is never drawn.
