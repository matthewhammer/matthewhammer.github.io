# Messed Up Dress Up: versions

Each version lists the game's features as of that version. The newest version is first.

## Unreleased

- **📜 Scripting** (ticket 482, a pull request): the game's logic written in Hazel. A new tab has an editor for 📚 Common and one for the castle's trap room; switched on, the trap room's red levers do what its script says: flip a lever, flip both, or say something over the picture. Off to start, and with a mistake in the script the levers work as they always have.
- **📜 Hazel's tiles** (ticket 520): under the text boxes, the whole script as Hazel draws it, in its tiles, with Hazel's own red marks where something is wrong. They are for looking, for now: you still write in the boxes above, and the tiles follow a moment after you stop typing. A script that is not finished yet (a hole to fill, or a bracket left open) leaves the levers working as they always have, and the panel says what is missing and in which room.
- **📜 More of the game in Hazel** (ticket 531, an experiment): the scripts now run more than the trap room's red levers. There are three rooms' scripts: the trap room (the red levers, and now the blue boulder levers too), 🌳 the tree houses' trap doors and the round room's at the top, and 🎚️ every placed lever and its trap doors, in any place. Each ships doing just what the game always did, so switching 📜 on plays the same game, only with Hazel deciding what each lever does. This is to measure how fast Hazel is at it, next to the game's own code. Then two more scripts: 🗺️ World, which decides which square a way out leads to as you walk through it, and 🏰 Castle, which decides which dungeon a trap room's pit drops you into, and where a dungeon's ladder up goes. They too ship doing just what the game did. If a script you edit stops with a mistake while you walk, the way goes where the game always sends it, so you are never stuck.
- **📜 JS, Hazel or Fumola, room by room** (ticket 566): in 📜 Scripting each room's box has three chips. JS shows the game's own code for that room, to read; Hazel and Fumola each show a rule you can change, and the chip you pick is what runs the room. A chip at the top switches every room at once. The game starts on JS, as it always has.
- **📜 The same rules in Fumola** (ticket 542, an experiment): the rules 📜 Scripting runs in Hazel, written again in Fumola, another language, and run by it. Open the game with `?logic=fumola` at the end of its address, and every red, blue and placed lever, every tree house trap door, every pit and dungeon ladder, and every way out of a square in the world is decided by the Fumola rules. `?logic=fumola-list` does the same, but hands Fumola the whole world map at every step, where `?logic=fumola` lets it keep its own copy. Both play exactly the game `?logic=js` and `?logic=hazel` play: this is to compare how fast each one is. Under either, the 📜 tab shows the Fumola rules at its bottom. Fumola itself is pinned: the game carries its own copy (the build fumola.org served on 2026-10-22, from Fumola's main at 2ec634b5df), so a new Fumola cannot change the game's rules under it.

## 0.0.411 (2026-10-22)

Everything in 0.0.410, plus the ninja's gem cut as a gemstone, its white cuts glimmering, and M's design for the ninja's bands: skin showing between them.

- **💎 The ninja's gem is a cut gemstone** (Matthew: "we also want the default (ruby) to have apparent cut marks, as a gemstone ... Please do the same for the others"): every gem on the "👑 golden head band" now has a flat table in the middle and bevelled facets round it out to the rim, the ones toward the light lit in the gem's own light, the others deep, and sharp, bright white lines along the cuts that stay sharp at any size (Matthew: "we need to add sharp, bright white lines to indicate the 'cuts', perhaps with an animation that makes them 'glimmer'"). The cuts glimmer, a flash running round them clockwise ("✨ the gem's cuts glimmer", on at first), and two more lights can be turned on in 🐯 Costume's 🥷 the ninja box: "⭐ stars twinkle on the gem" and "🌟 a shine sweeps the gem". Each kind in its own colours (ruby, emerald, sapphire, amethyst, topaz, diamond), and each shape cut its own way: round, oval, diamond shape, teardrop, a facet down each half of every point of the star, and the heart's facets following both its lobes. From the side, the gem shows its flat front and two sloping facets (ticket 560).
- **🥷 The ninja's bands show skin between them** (M's design, as Matthew asked): the bands up the shins, and now bands on the forearms too, from the gloves up to below the elbows, are wound over bare skin in the friend's own skin colour, which shows between them, braided or "🧣 bands wrapped round" alike. "🦵 between the bands" switches to "🌑 full darkness", black cloth under the bands as before. The trousers and sleeves above the bands stay black (ticket 561).
- **Checks:** the held-things check (hold518) stopped on its own bookkeeping in 0.0.409's speed run, not on the game; fixed.

## 0.0.410 (2026-10-22)

Everything in 0.0.409, plus three new costumes: a 🥷 ninja (for M), a 🦋 moth in three styles, and a 🦒 giraffe in three styles (for Nick).

- **🥷 Ninja costume** (M): a new costume in 🐯 Costume, all in black: the shirt, the trousers, black gloves, and a black mask fitted close round the head that hides the whole face, with only a narrow dark slit across the eyes ("👀 eyes in the slit" shows the eyes in it, and is off at first). Soft, quiet ninja boots with a split toe and bands from the ankle up the shin, in "🥾 the boots' bands": "🪢 braided bands" (two colours crossing over and under each other, the first choice) or "🧣 bands wrapped round" (band above band). And "👑 golden head band", on at first: a gold band all the way round the head, above the eyebrows, and a second gold band from the middle of the forehead over the top of the head to the back, joined to the first in front and behind, like an upside-down T on the forehead, with a gem where they meet: "💎 the gem" (ruby, emerald, sapphire, amethyst, topaz or diamond; a ruby at first) and "💎 the gem's shape" (round, oval, diamond shape, teardrop, star or heart). Behind, a round gold clasp joins the bands. It works facing toward us, away, left and right, turned partway, and walking, running, crouching, riding, sitting, swimming, climbing, jumping and crawling (ticket 557).
- **🦋 moth costume** (Matthew: "moth costume, 3 styles", with moth wings and antennae): a fuzzy suit in 🐯 Costume with a moth's broad, soft wings on its arms, worn the way the bird suits wear theirs: they fold along the sides when the arms are down and spread wide when the arms go up. Feathery antennae stand up from its hood, which has big round shiny eyes, a fluffy tuft and a curled-up tongue; a fluffy collar sits round its neck and fuzzy bands go across its belly and back. Three styles, each with its own wing colours in "🦋 its wings": "🌙 luna moth" (pale green, a purple edge, small eyespots and long tails on its back wings), "🦚 emperor moth" (warm orange and brown with big bold eyespots on all four wings) and "🌫️ night moth" (cozy dusty grey with zigzag bands, and orange back wings peeking out underneath). "👁️ eyespots" turns the eyespots off and on. Choosing a style dresses the suit in that style's colours, and the animal colour rows still change them. It works in every pose and from every side.
- **🦒 giraffe costume** (for Nick, Matthew's friend: "giraffe as a costume in dress up"): a giraffe suit in 🐯 Costume. Its long neck rises up over the friend's head, with the giraffe's own head on top: two little knobbed horns, ears, big eyes with long lashes and a soft muzzle. Brown patches on cream cover the whole suit, a short mane runs down the back of the neck, a thin tail ends in a dark tuft, and the hands and feet are hooves. It has three styles, each with its own colours for its patches: "🦒 reticulated" (the classic net of patches), "🌳 Masai" (dark, jagged patches and the longest neck) and "🍼 baby" (pale, with a shorter neck, big eyes and fluffy tufts on its horns). "🦒 neck's length" makes the neck shorter or much longer. It works in every pose and from every side: lying down, crawling or swimming, the neck bends up so the giraffe's head stays upright, and at a door too low for its neck the giraffe ducks and crawls through.

## 0.0.409 (2026-10-22)

Everything in 0.0.408, plus a dress, a skirt or a robe that stays over the legs in every pose.

- **Fixed:** in a dress, a long skirt or a robe (the wizard's, the monk's or dryad's, the king's and the queen's too), the legs stuck out through its sides, as if it had holes in it (M): riding the horse ("🏇 ridden") facing left, right, toward us and away, "🧎 crouching down", "🏃 running", and also walking, sitting side on and with "🦵 knees bend". Now the cloth drapes over the knees and the stride, and a leg shows only below the hem. Standing, nothing changes (ticket 546).

## 0.0.408 (2026-10-21)

Everything in 0.0.407, plus the checks after today's releases, the lasso put away swimming, and the round room's lever post.

- **Fixed:** the cowgirl's and cowboy's ➿ lasso is put away while swimming anywhere, not only in deep water (ticket 522).
- **Fixed:** in the ⭕ round room at the top of the tree, the hatch lever's post stands a little further from the hole, so walking to the lever never steps into it (ticket 516).

## 0.0.407 (2026-10-21)

Everything in 0.0.406, plus a 🦶 Sasquatch costume in four styles.

- **🦶 Sasquatch costume** (Matthew: "Sasquatch costume"): a big, shaggy, friendly suit in 🐯 Costume, with four styles, each with its own fur colours: "🦶 Bigfoot" (brown, with the biggest feet), "❄️ yeti" (snow white with frosty blue tips), "🌿 mossy" (moss and leaves stuck in its fur), and "🍭 fluffy" (candy colours, extra fluffy, with a bow). Shaggy fur along its arms, legs and sides, a big muzzle with "🙂 big grin" (two square teeth), small round ears and a stubby tail. Stand still and "👣 footprints" shows the trail it walked in on. It works in every pose and from every side.

## 0.0.406 (2026-10-21)

Everything in 0.0.405, plus each hand keeps its own look from behind, and held things lie on the floor while crawling.

- **Held things while crawling and swimming** (ticket 529): crawling toward us, each thing in a hand (broom, bow, scepter, sword, shield, staff) lies flat on the floor under the hand, out to the side, its own drawing. Swimming, held things are put away, as the lasso already was: both arms sweep over the head and under the body each stroke, so a thing in the hand would sweep through the friend.
- **Fixed:** from behind, with different hands, the right hand's own look (claws, its style) was on the left hand, as if the friend still faced us, and a sword's fist in the left hand closed over it. Each hand's look now stays on its own hand from behind, like what it holds (ticket 528).

## 0.0.405 (2026-10-21)

Everything in 0.0.404, plus five ways to furnish the round room at the top of the tree.

- **⭕ 🪑 Furniture for the round room at the top of the tree** (its new setting "⭕ 🪑 how it is furnished"; it starts empty, "🚫 none"): five ways to furnish it, each made from things already in the game, set round the curved wall with their backs to it and round the trunk, and always leaving the trap door, its lever and the way round the trunk clear. "🛋️ hut parlour": the grass hut's couch and pillows, a rug round the trunk, the living room's lamp and plant, the wind chime hanging from a branch. "🛏️ sleeping loft": the tree house's bunk beds and toy chest, the hut's bed of logs with a candle on a stump beside it, the dreamcatcher on a branch. "📚 reading ring": shelves of books going round the trunk itself, the beanbag, the lookout's telescope, a lantern on a branch. "🧪 potion nook": the healer's potion shelves with her cat on top, the hut's curved shelves of bottles, herbs drying from the branches, the bubbling cauldron, the healer's worktable. "🍵 tea and kitchen": the healer's tea table and chairs, the tree house kitchen's counter and stove, and a bench of logs curving round the trunk. Sit on the couch, the beds, the beanbag, the chairs and the logs.

## 0.0.404 (2026-10-21)

Everything in 0.0.403, plus cave doors: a cave passage with a nav arrow, linked to a square in the world and back.

- **🕳️ Cave doors** (Matthew: "a way to place cave doors --- as objects with nav arrows associated with them", in the world design panel). A 🕳️ cave passage put in a place now has a nav arrow over its opening, like the arrows at a square's edges: walk near its foot and the arrow shows where it leads; tap the arrow (or the opening, or press X) to go through. On the passage's card (in "🗺️ World design" or "🏞️ Background"), "🧭 a cave door to a square, and back" shows the worlds and each world's grid, small: tap a square, or one of the places off the grid (the cave room behind the falls, a tent, a living room...), and the passage leads there. A passage just like it appears there, leading back, so there is always a way home. ✖ on either one takes both away; ✖ next to where it leads takes the link off (the passage stays and leads to its 🧭 place again). A passage that is not linked works as before, and it gets an arrow too. When a world is taken away, the doors into it are unlinked, and 🗺️ World design warns about a door whose square is gone or whose way back is missing. Moving a square, or putting a place from off the grid onto one, takes its cave doors with it; a door whose square was taken away leads to its 🧭 place until the square is back.

## 0.0.403 (2026-10-21)

Everything in 0.0.402, plus a thing to hold in each hand, chosen in 🧍 Body.

- **🫴 Things to hold, in either hand** (in "🧍 Body", "🫴 holding"): "🫱 right hand holds" and "🫲 left hand holds" each show what that hand holds; a tap opens a box of chips: "🎭 the costume's own" (what the costume always held, the usual), "✋ nothing", "🧹 broom", "🏹 bow", "👑 scepter", "🗡️ sword", "🛡️ shield", "🪄 staff", "➿ lasso", and, with a horse, "🐴 the horse's rein". Anyone can hold anything: a fairy with a sword, a knight with a broom, two swords at once. A shield is the knight's arms on a knight, a plain wooden one on anyone else.
- **Each held thing has its own drawings from the side and from behind**: a broom, staff, scepter and sword leaning the way the friend faces, the sword's blade edge on, a shield seen by its rim, a scepter's plain back and a bow's string side from behind. In profile, a thing in the far hand is behind the body.
- **The lasso's hand is chosen in "🫴 holding" now**, not in the costume's "➿ held in" row: pick "➿ lasso" for "🫱 right hand holds" to move it. A friend saved with the lasso in the right hand still has it there.
- **The horse's rein**: picking "🐴 the horse's rein" for a hand puts the led horse's rein in that hand. Riding, a hand holding something else lets go of its rein.
- **Fixed:** from behind, the costumes' things (the broom, staff, scepter, sword, bow and shield) were in the wrong hand, as if the friend were still facing us. They now stay in their own hand. In profile, a thing in the far hand was drawn over the body.

## 0.0.402 (2026-10-21)

Everything in 0.0.401, plus a round room at the top of the tree, round the trunk, with its limbs holding up the roof.

- **⭕ A round room at the top of the tree** (a new place in "🏠 home", "⭕ the round room at the top of the tree", after "🪜 inside the tree house"): a round room built round the great tree's trunk where its branches begin, with a thatched roof like the 🛖 grass hut's. The trunk rises through the middle of a floor laid in rings like a tree's, and the tree's own branches reach out to the walls and up through the thatch, where daylight falls in. Two round windows look out at the leaves blowing in the wind, two lanterns hang from the branches, and a squirrel peeks out of a knot hole in the trunk. Climb up to it by the ladder in the highest tree house ("🐰 4" with four houses or more), and down again through its trap door; or go out of a window to the tree house. Outside, the room sits in the top of the tree with the branches coming out through its roof: make "🌳 tree size" small to see it, and look for it on the tree in the 🌲 forest clearing. Its own settings: "⭕ 🕳️ its trapdoor's opening", "⭕ 🎚️ its trap door shut (its lever pulled)" (or pull the lever on its post beside the trap door) and "⭕ ⬇️ the way toward us leads to". The map on every tree house room's sign shows the new ladder, and the round room's sign shows the top of the tree.

## 0.0.401 (2026-10-21)

Everything in 0.0.400, plus a cowgirl and a cowboy in three styles each, six hats and a lasso.

- **👢 A cowgirl and 🤠 a cowboy** (in "🐯 Costume"), asked for by M and Matthew, with no guns. The cowgirl has three styles in "👢 style": "🌟 rodeo star" (a bright shirt with white fringe across the chest, down the sleeves and under a short white skirt, a gold buckle, and white boots with a star), "🌻 prairie" (a long gingham dress with a scalloped hem, a white petticoat ruffle peeking out and a little bow at the collar) and "🐮 cow-print" (white chaps with black cow spots over blue jeans, a polka-dot bandana, a silver buckle and boots with star spurs). The cowboy has three in "🤠 style": "⭐ sheriff" (a leather vest with a big toy star badge and a string tie), "🏅 rodeo rider" (a pointed white yoke, a big trophy buckle with a star, and a number 7 card on his back) and "🧥 trail boss" (a long duster coat with a shoulder cape and a red neckerchief). Each style has its own colour row. Both wear cowboy boots, and the star on a boot or a spur is a toy star.
- **🎩 Six cowboy hats** for either one, in "🎩 hat": "🐂 cattleman", "🪣 ten-gallon" (the tallest), "🌾 straw" (the widest, with red ribbon tails down the back), "🎀 bow" (a big bow at the front, a small one at the back), "🌠 star hat" (a gold star on the front and on each side, a small one at the back) and "🪶 feather" (a flat top with a long red feather), or "🚫 no hat". Each hat has its own colour. Each style has a usual hat it puts on when you pick it, but never over a hat you chose yourself. The hat is drawn from the front, the side and the back, and the hair tucks under it, even big hair seen from behind.
- **➿ A lasso** to hold, a coil of rope in "➿ held in" "🫲 left hand" (so the right hand is free for a horse's rein) or "🫱 right hand". Running, the cowgirl or cowboy twirls its loop. It is put away while crawling, swimming, climbing or lying down. Switch it off with "➿ lasso".

## 0.0.400 (2026-10-21)

Everything in 0.0.399, plus every room in 🗺️ World design, draggable onto the map, with its own settings.

- **🗺️ Every room can go in the world** (in "🗺️ World design"): every place is in "🎨 places to put in a square" now, the rooms inside other places too, such as "🦇 cave room" under "🏕️ camping". Drag one onto a square and it is a square like any other, with its own ways out. Its own doors still lead by kind: the cave's archway goes to the nearest cave room square, and back through the cave to the nearest waterfall, so the friend may come out somewhere far away. A tree house room, a tent or a castle room in a square has "which one" chips on its card, and the square remembers which.
- **⚙️ A place's settings in the world designer:** "⚙️ show this place's settings" opens the place's own settings from "🏞️ Background" in "🗺️ World design", such as the cave room's "🪨 on the cave room's floor" and "🦇 on the cave room's ceiling". With "✏️ changes here go to" set to "📍 only this square", a change stays in that square. It starts closed, because open it costs as much as the Background tab.
- **📍 Inside a room, the world designer says where you are:** inside the cave room behind a waterfall (or any room inside a place on the grid), "🗺️ World design" shows that place's square, named with the room ("📍 A5: 💦 jungle waterfall → 🦇 cave room (inside)"), and a "↩️ to A5" button. It used to say only "Tap a square to go there and change it."
- **🚪 The rooms off the grid are buttons:** in "🚪 off the grid", tap a room to go there, or drag it onto a square to put it there. A room dragged onto a square brings its things with it, and its "📍 only this square" settings. A campsite's tent, a tree house's room or a castle room tapped there is one that is not in a square yet. Walking through a castle room's door to another room no longer turns the square into that room.

## 0.0.399 (2026-10-21)

Everything in 0.0.398, plus a 🔥 button and the six fires' chips in the dragon costume.

- **🔥 Try the dragon's fire while dressing up** (in "🐯 Costume", in "🐉 the dragon", while the dragon costume is on): a "🔥 hold to breathe fire" button breathes fire for as long as you hold it, with the mouse, a finger or the Space key, and stops when you let go or slide off it. Under it are the six fires: "🔥 roaring flame", "☄️ fireballs", "🌀 twisting fire", "🔵 blue-hot jet", "🧱 brick fire" and "🫧 fire bubbles". Tap one and the dragon breathes that fire for two seconds, so you can try each in turn without switching to the game controls. It is the same choice as "🔥 its fire" in "🎮 Controls": pick a fire in either place and the other shows it too. 🔥 on the controller and F on the keyboard still breathe fire as before.

## 0.0.398 (2026-10-21)

Everything in 0.0.397, plus a 🤖 robot costume in eight styles.

- **🤖 A robot costume** (in "🐯 Costume"), in eight styles to pick from in "🤖 style": "🔑 wind-up tin" (a tin toy with a big key in its back that turns as it walks), "📦 box bot" (a cardboard box robot, your own face showing through the hole and your own trousers and shoes underneath), "🤗 helper bot" (a round white robot with a glowing screen face), "🚀 space bot" (a glass bubble helmet and a rocket pack that sparks flames when you jump), "🦾 big mech" (a digger-yellow mech with a cockpit window, a beacon and huge shoulder blocks), "⚡ hero bot" (a helmet with a gold V crest and a visor), "📺 TV bot" (your face on an old television's screen, with rabbit-ear antennas and a curly power cord) and "🕷️ spider bot" (four jointed spider legs out of its back). Each style keeps its own colour in "🤖 its colour", and "💡 its lights" colours the lights on all of them ("auto" is each style's own). The tin's and the helper's faces are their own, and they smile or frown with yours. Facing us, from behind and from the side, standing, sitting, lying, crawling and swimming.

## 0.0.397 (2026-10-21)

Everything in 0.0.396, plus lighter cauldron fumes.

- **🧪 The simple cauldron fumes cost less:** the glowing band that slides up each strand is drawn with fewer steps. It looks the same, and a room full of cauldrons walks a little smoother on slower computers.

## 0.0.396 (2026-10-21)

Everything in 0.0.395, plus a smoother walk in the fairy forest and the castle on slower computers.

- **🧚 The fairy forest walks smoothly:** on a computer the game finds slow, it holds a place's scenery still, as a picture, while the friend walks. In places full of things at different depths, like the fairy forest, that picture had to be made again and again as the friend walked past them, and the walk got choppier, not smoother. Now, where that happens and the place moves well enough on its own, the scenery just keeps moving there.

## 0.0.395 (2026-10-21)

Everything in 0.0.394, plus a smoother walk in our imagination.

- **💭 Our imagination walks smoothly:** its map's pictures of every place are now drawn once as ordinary pictures, so the glow, the dashes and the bobbing ladders no longer make the computer draw all 36 of them again while you walk. On computers without a strong graphics card it walked at about 48 frames a second; now 60. They look the same.

## 0.0.394 (2026-10-20)

Everything in 0.0.393, plus the date's moon in the dungeons.

- **🌙 The dungeons' moonlight follows the moon:** the moon in the barred window of the dungeon and the deep dungeon (it was always full) is the moon of the date in 🌍 the sky: where and when, in its phase, and its beam of light across the floor is as bright as the moon is lit: as before at full moon, barely there at new moon.

## 0.0.393 (2026-10-20)

Everything in 0.0.392, plus the moon of the game's date in four more places.

- **🌙 The moon in its phase, in the witch's woods and three windows:** the witch's woods (its moon was always full), and the round windows of the witch's hut and the hut's bedroom and the cat room's window (theirs were always a crescent) show the moon of the date in 🌍 the sky: where and when, in its phase, waxing or waning the right way round for the place on Earth.

## 0.0.392 (2026-10-20)

Everything in 0.0.391, plus a dragon, fire, armour for the horse and a stable full of hens: every choice in the game, for the kids to pick.

- **🐉 A dragon costume** (🐯 Costume, after the deer): a hood with two horns, spines down its back, teeth along its edge and slit eyes; scales, a plated belly, claws; bat wings that flap (with their own beat or an LFO), stay open or fold; and a long tail with its spikes standing up and a spade at its tip. One colour each for its scales, its belly, its wings and spines, and its horns, teeth and claws. Facing us, from behind and from the side, standing, sitting, lying, crawling and swimming.
- **🔥 Breathe fire** (held, like 🫧 blowing bubbles): 🔥 on the game controller (the right trigger), on the screen's controller, or F on the keyboard. Anyone can, whatever they wear. Six fires to pick in 🎮 Controls: 🔥 roaring flame, ☄️ fireballs, 🌀 twisting fire, 🔵 blue-hot jet, 🧱 brick fire and 🫧 fire bubbles; from the 👄 mouth, the 🫲 left hand or the 🫱 right hand; its size; and whether its heat shows. The heat reaches past the flames and pops every bubble it touches: in bubble space, the bubble room, from bubble makers and the ones the friend blew. The best tool against bubbles.
- **🛡️ Armour for the horse** (🧍 Body's 🐴 the horse, "🛡️ its armour"): 🛡️ full plate, 🦁 a half caparison, 🎪 tournament, 🐉 a dragon helm, 🧱 a toy helmet or 🎀 parade, seen from the side, the front and behind, led and ridden. A knight's horse wears the knight's own colours and emblem.
- **🐔 Hens in the castle's stable** (the stable's own settings): from none to 30; two, as before, to start with.

## 0.0.391 (2026-10-20)

Everything in 0.0.390, plus M's three new places and her farm animals.

- **🏜️ The desert:** sand dunes rolling into the distance, mesas far off (or other mountains: "far off" in 🏞️ Background), cacti and desert plants standing in it, a tumbleweed rolling past and a lizard doing push-ups (quick), a hawk circling and sand blowing off the dunes (slow).
- **🌵 Desert plants to put anywhere** (🪨 things, 🌵 desert): a saguaro (its arms, its flowers), a prickly pear (flowers or fruit), a barrel cactus, an agave (with its tall flower stalk), a Joshua tree and an ocotillo.
- **🚜 The farm yard:** a red barn, a farmhouse, a windpump pouring into a trough, a tractor, a haystack with a mouse, and hens pecking, behind a fence with fields beyond.
- **🚜 Farm things to put anywhere** (🪨 things, 🚜 farm): a 🏚️ barn of six kinds (a red barn with a hay loft, a round barn, a stone barn, an open pole barn full of hay, a chicken coop, a barn with a silo), and a 🪵 fence piece of three kinds (wooden rails, a white picket, a stone wall). Fence pieces snap to tiles on the floor, and a piece joins its rails to the one beside it; tap "+ 🪵 fence" again and again to build a run. The first step to building fences as you play.
- **🌾 The pasture:** a wide green field with horses grazing in it (up to three), a big shade tree, a trough, wildflowers and a fence with a gate.
- **🐄 Farm animals to put anywhere** (🪨 things, 🐄 farm animals): a cow (standing, grazing, lying down, mooing; black and white, brown, red and white, or a shaggy Highland), a pig (standing, rooting, lying on its side, sitting; pink, spotted, black with a band, or ginger), and a chicken (standing, pecking, sitting on a nest, flapping; white, brown, black or speckled), each moving with the music.

## 0.0.390 (2026-10-20)

Everything in 0.0.389, plus six ways to do the horse's mane, for M.

- **💇 The horse's mane** (in 🧍 Body's 🐴 the horse): 🐴 loose (the usual, as before), 🌊 long and flowing (with a long forelock over its forehead), 🔘 button braids (little knots along its neck), 🪮 short and standing up (a brush, like a zebra's), ➰ curly, and 🎀 braids with bows, whose bows have their own colour ("🎀 its bows"). Seen from the side, from the front and from behind, led and ridden.

## 0.0.389 (2026-10-20)

Everything in 0.0.388, plus who made it and on what terms.

- **📜 Reuse is welcome, with credit:** the game and everything with it is © 2026 Matthew Hammer and family (M and N). The code is under the Apache License 2.0 and the pictures, music and words under Creative Commons Attribution 4.0. ⚙️ Advanced says so under the code's link, and each of the game's files carries a two-line notice. The sun's and moon's formulas (🕰️ by the clock) follow SunCalc 1.9.0, by Vladimir (now Volodymyr) Agafonkin, and stay under its own BSD licence: NOTICE says so.

## 0.0.388 (2026-10-20)

Everything in 0.0.387, plus which side you lead the horse from.

- **🐴 Lead your horse from its left or its right** (in 🧍 Body's 🐴 the horse, while you lead it): "🚶 lead it from its" left side (the usual) or right side. You stay on that side whichever way you walk; on its far side you walk at its head, so the horse does not hide you.
- **Fixed:** "🕰️ by the clock" at Quito, the picture still faced the wrong way on some days near the equinoxes.
- **Fixed:** in the 🦇 cave room with columns, the columns poked up through the ceiling's rock.

## 0.0.387 (2026-10-20)

Everything in 0.0.386, plus more colours for the horse, for M.

- **🐴 More colours for the horse** (in 🧍 Body's 🐴 the horse, one colour chooser each): "🧲 its horseshoes" (none, or steel, silver, gold, red, blue, pink or green, or any colour), "⭐ the colour of the mark on its face" (the star, stripe or blaze, seen from the side and from the front), and, while you ride, "🏇 its saddle" and "🟦 its saddle blanket".

## 0.0.386 (2026-10-20)

Everything in 0.0.385, with today's new things fixed where a careful look found them wrong.

- **Fixed:** a 🕳️ cave passage you put in a place could not be tapped or clicked to go through it (it moved instead). Now a tap or a click goes through it, as any doorway, and dragging it still moves it; it glows when you point at it.
- **Fixed:** a cave passage could be set to lead to 🌊 open water or 🪸 the deep sea, and then went nowhere. It offers only places you can go through a doorway to.
- **Fixed:** in the 🦇 cave room, the ceiling's rock could cover the friend when they stood just behind the nearest formation.
- **Fixed:** the sparkles, drops, running water and glows of the cave things fell out of time with the music after it started or its LFOs changed.
- **Fixed:** "🕰️ by the clock" at Quito, on the equator, showed no sun from October to March: the picture now faces where the noon sun is.
- **Fixed:** inside a tent "by the clock", its windows kept the colour of the time of day it first had.
- **Fixed:** at night "by the clock", the ⭐ stars' settings were hidden although the stars were shown.
- **Fixed:** the sky in words could say a day was "9 h 60 min" long.
- **Fixed:** in 🗺️ World design beside the picture, the 📺 note (a place not from a square) lost its amber colour.
- **Fixed:** in the 🦇 cave room, the dark at the picture's edges is lighter work for the game.
- **Fixed:** a person kept in 👥 People from before 0.0.380 lost their horse; now they keep it, as the friend does.

## 0.0.385 (2026-10-20)

Everything in 0.0.384, with the waterfall's moon by the clock too.

- **The 🌊 waterfall and the 🪨 cave behind it, "🕰️ by the clock":** the moon is the real one there too, in its phase. At the waterfall it hides behind the cliff and the jungle unless it is high enough or far enough to the side to be seen; from inside the cave you see it through the mouth whenever it is up.

## 0.0.384 (2026-10-20)

Everything in 0.0.383, plus the sky in words.

- **🌍 where and when says what the sky is doing:** under the date and hour, a line tells you where the sun is (how high, which way) and how long the day is there, from sunrise to sunset, and the moon's phase, how much of it is lit and where it is. Far north in winter it says the sun does not rise that day.

## 0.0.383 (2026-10-20)

Everything in 0.0.382, with 🎮 Controls tidied into boxes.

- **🎮 Controls in three boxes:** "🎮 the game controller" (its buttons), "🫧 bubbles" (blowing them, and the three bubble themes) and "🏃 moving" (how your friend moves, and what a click does while they walk), instead of one long column.

## 0.0.382 (2026-10-20)

Everything in 0.0.381, with 🗺️ World design tidied into boxes beside the picture.

- **🗺️ World design, beside the picture (not full screen):** each part is a box with its name at the top ("🌍 worlds", "🎨 places to put in a square", the world's grid, "📐 the world's size", "📍" the square you are in, "🪨 things here", "🚪 off the grid"), and on a wide screen the boxes sit side by side, as they do in full screen, instead of one long column.

## 0.0.381 (2026-10-20)

Everything in 0.0.380, plus the sun and the moon as they really are, where and when you say.

- **🕰️ By the clock:** the 🌗 time of day at the waterfall (and its cave), the camp site (and its tents) and the fairy forest has a new choice, "🕰️ by the clock". Then "🌍 the sky: where and when" says where on Earth you are ("📍 where on Earth": Denver, New York, London, Tokyo, Sydney, Quito on the equator, Reykjavík, Tromsø far north, Ushuaia far south, or "📍 my own place" with its latitude and longitude) and the game's own date and hour. The game works out where the sun and the moon are then, and the moon's phase, by itself, without the Internet: the time of day follows the sun, the sun rises in the east and sets in the west, high in summer and low in winter, and the moon is drawn lit as it really is that night. Far north in winter the sun never rises; in summer it never sets.

## 0.0.380 (2026-10-20)

Everything in 0.0.379, plus the horse as part of the body, meeting things as it is drawn.

- **🐴 The horse is in 🧍 Body now**, in its own box "🐴 the horse", not in 🐯 Costume: a horse goes with any costume. A friend saved with a horse keeps it.
- **Riding or leading, the horse meets things where it is:** each of its four hooves bumps into things, and its body does too, from its belly up to its back, so something low can pass under it between its legs. Riding, you are up on the horse, not on the floor: your head is up there too, so a low doorway or bridge meets it. Leading, you and the horse each meet things. 🔬 Inspection's collision boxes show the horse's hooves and body, and the rider on it.

## 0.0.379 (2026-10-20)

Everything in 0.0.378, plus cave mushrooms, a better treasure chest and a worn floor in the cave behind the falls.

- **🍄 Cave mushrooms, in the "🦇 cave" things:** six kinds for a dark cave ("💡 glowing caps", "🔔 bells", "🍄 a crowd", "📚 shelves on a stump", "🕯️ tall and thin", "⚪ puffballs"), in a glowing colour, their spores drifting up; turn "✨ glowing" off for plain ones. You can sit on them.
- **💰 The treasure chest, better:** wooden planks with gold bands, and a lock when it is shut. Open, choose what is in it ("💰 in it"): "🪙 gold coins", "💎 gems", or "👑 gold, gems and a crown", sparkling.
- **The 🪨 cave behind the falls** has the cave room's worn, rounded floor now, and its archway to the 🦇 cave room is on the left, where the old chest no longer stands in front of it.

## 0.0.378 (2026-10-20)

Everything in 0.0.377, plus cave passages to put anywhere.

- **🕳️ Cave passages, in the "🦇 cave" things:** a dark way through, put anywhere, as "🌑 a natural archway" in a lump of rock, "🕳️ a hole through a wall of rock" or "⚫ a hole in the floor". Each leads where its "🧭 leads to" says, any place but a castle's or a tree house's rooms, and you go through it as through any doorway (walk to it, ✨, or 🚪 on the controller), and come back with ⬅️.

## 0.0.377 (2026-10-20)

Everything in 0.0.376, plus cave things to put in any place.

- **New things to put in a place, in the "🦇 cave" group:** a 💎 gem (six kinds: cut, a cluster, a geode, a single point, rough, a heart; any colour; "✨ glowing" or not), a 💧 puddle (with "💧 a drip from above" falling into it, or still), and a 💦 little waterfall: water coming out of a lump of rock and running down it into a pool at its foot (six kinds: a trickle, a sheet over a ledge, down steps, a spout from a hole, mossy, glowing). You walk around the waterfall's rock, over gems, and stand in puddles. The sparkles, drops and running water move in time with the music.

## 0.0.376 (2026-10-20)

Everything in 0.0.375, plus a new place, the 🦇 cave room, and a fix for narrow windows.

- **A new place, the 🦇 cave room** (in 🏕️ camping), deeper in behind the waterfall: go through the dark archway at the back of the 🪨 cave behind the falls, and come back the same way. Its floor is worn, rounded rock, ledge after ledge, and you can walk all over it. Choose what stands on the floor, "🪨 on the cave room's floor": 🗼 stalagmites, 🪨 boulders, 🏛️ columns from floor to ceiling, 💧 rimstone pools, 💎 crystal clusters or 🍄 glowing mushrooms. You walk around them, and you can sit on a boulder or a big mushroom. Choose what hangs from the ceiling, "🦇 on the cave room's ceiling": 🦷 stalactites, 🥢 soda straws, 🎐 curtains of rock, ☀️ a crack of daylight, 💎 crystals or 🐛 glowworms. Drops fall into a puddle in time with the music, glowing patches on the walls breathe slowly, and two bats are asleep up in the dark.
- **Fixed:** in one column (a phone, or a window about half a screen wide), the band over the controls that names where the pointer is (the breadcrumbs) slid across the top of the picture as the controls scrolled. It now stays just under the picture and its buttons.

## 0.0.375 (2026-10-20)

Everything in 0.0.374, with two things moved up where you can find them.

- **⚙️ Advanced, near the top:** "✈️ without the Internet", the steps for adding the game to your device, now has a box of its own right after 🏠 this game, instead of at the very bottom. And **See also**, the paper *Languages of Play* (2017), is in 🏠 this game, after the links to the game's page and its code, near the top as it was meant to be (in 0.0.373 and 0.0.374 it had slipped to the bottom box, ⚙️ more).

## 0.0.374 (2026-10-20)

Everything in 0.0.373, and it works without the Internet.

- **✈️ Play in airplane mode** (ticket 486): the game now keeps a copy of itself on your iPad, phone or computer, the first time it opens with the Internet. After that it opens with no Internet at all, in a car, on a plane, anywhere. In ⚙️ Advanced, "✈️ play without the Internet" folds open to show how to add the game to your device, so it gets its own icon on the home screen: on an iPad or iPhone, Safari's Share button and then "Add to Home Screen"; on Android, Chrome's ⋮ menu; on a computer, the install button in Chrome or Edge. It also says whether this version is kept on the device yet (✅). Your friends and settings stay saved on the device, as always, and when the Internet is back a newer version shows as 🆕, as before.

## 0.0.373 (2026-10-20)

Everything in 0.0.372, with a paper to read.

- **See also** (ticket 474): near the top of ⚙️ Advanced, a link to *Languages of Play: Towards Semantic Foundations For Game Interfaces* (2017), by Chris Martens and Matthew A. Hammer: a paper about what players mean to do in a game, and the languages that say it. Its web page version is right after it.

## 0.0.372 (2026-10-19)

Everything in 0.0.371, with the band going all the way down.

- **The band, deeper** (ticket 461): it now names the row you are pointing at and the part of it too: "background / the place / kind of place", "background / the place / place choices", "body / the jumps / short jump: how high". The place picker's places are now headed "place choices".

## 0.0.371 (2026-10-19)

Everything in 0.0.370, with the band naming only what you can see.

- **The band** (ticket 456): it no longer names a group whose name the controls hide (over the jumps it says "body / the jumps"); and the 👥 People box and 🗺️ World design's worlds box have plain borders like every other box.

## 0.0.370 (2026-10-19)

Everything in 0.0.369, with a band that says where you are in the controls.

- **Where you are in the controls** (ticket 456, M's idea): a band stays at the top of the controls however far you scroll. It always names the tab you are in ("background"), and as you point at the boxes in it, the box too ("background / default background objects").
- **Default background objects** (ticket 456): 🏞️ Background's 👁️ what stands here box is now called 👁️ default background objects.

## 0.0.369 (2026-10-19)

Everything in 0.0.368, with no more heads cut off lying down.

- **The picture holds the whole friend** (ticket 454): with no place behind them, lying down, crawling sideways or swimming no longer cuts your friend's head off at the picture's side, and jumping up into a hug no longer cuts off the top of their head.

## 0.0.368 (2026-10-19)

Everything in 0.0.367, with new people from a costume.

- **➕ From a costume** (ticket 459): in 👥 People, ➕ from a costume opens a box over the panels with every costume as a chip. Pick one and a new person appears in that costume, ready to dress ("knight 1"), or ✖ cancel at the bottom. It is an experiment beside the template list, to see which works better.

## 0.0.367 (2026-10-19)

Everything in 0.0.366, with many people, and templates to make them from.

- **People** (ticket 439): a new 👥 People tab. Your friend is the first person; make anyone ⭐ a template, then ➕ make new people from it ("knight 1", "knight 2") and dress each one their own way. Tap a person to dress them: they come into the picture, and the one you were dressing stays standing where they were. The others stand in their places, drawn just like your friend; 📍 brings one beside you, ✖ takes one out of the places, 🗑️ out of the game. They are all saved with your game.
- **The ridden horse's head** (ticket 458): facing us, the horse's head is in the middle again, in front of its rider.

## 0.0.366 (2026-10-19)

Everything in 0.0.365, with a horse as big as the stable's, and your friend's own size.

- **The horse's size** (ticket 457): the horse is now as big as the horses in the castle's stable, and 🐴 the horse has its own 📏 size. It stays the same however big or small your friend is, so a little gnome can ride a full-size horse. Facing us, a ridden horse turns its head a little aside so you can see who is riding.
- **Your friend's own size** (ticket 457): 🧍 Body has 📏 their size, which follows your friend into every place. Make a gnome small once and they stay small wherever they go, and their horse stays the right size.

## 0.0.365 (2026-10-19)

Everything in 0.0.364, and now you can ride the horse.

- **Riding the horse** (ticket 438): in 🐯 Costume, 🐴 the horse: 🏇 ridden puts your friend in the saddle, whatever they are wearing. From the side they sit astride with their feet down its side and the reins in their hands; facing us or from behind their legs hang down either side of it. When they walk or run, the horse walks or canters and carries them, bobbing with its stride. The saddle cloth is the colour of its halter and reins.

## 0.0.364 (2026-10-19)

Everything in 0.0.363, with a horse to lead.

- **A horse** (ticket 438): in 🐯 Costume, 🐴 the horse: lead a horse by its rein, whatever your friend is wearing. It stands beside them facing us, from behind, or from the side, and walks or runs (a canter) with them, its legs stepping in time with theirs. Choose its coat, its mane and tail, a star, stripe or blaze on its face (the same as the castle stable's horses), the colour of its halter and rein, and white socks.

## 0.0.363 (2026-10-19)

Everything in 0.0.362, with a druid's face in the shadow of the hood.

- **The face in the hood** (ticket 453): for the monk or dryad with the hood up, 👤 see the face, and 🌑 how deep the hood's shadow is over it, from clear to black. Or hide the face: the hood is dark inside, and 👀 eyes in the dark lets the eyes show there: plain black eyes vanish (only their glints show), but coloured eyes, star eyes and heart eyes glow.

## 0.0.362 (2026-10-19)

Everything in 0.0.361, with the druid's own feet, or bare ones.

- **The monk's and the dryad's feet** (ticket 452): no more boots: under the cloak your friend wears their own shoes (as set in 👟 Feet), or turn on 🦶 bare feet and their shoes are hidden away in the cloak.

## 0.0.361 (2026-10-19)

Everything in 0.0.360, with the hood round on top.

- **The hood's crown is round** (ticket 451): facing you, the monk's and the dryad's hood is rounded over the head, with no peak sticking up.

## 0.0.360 (2026-10-19)

Everything in 0.0.359, with hoods that fall onto the shoulders and robes that hang straight.

- **The hood falls onto the shoulders** (ticket 449): the monk's and the dryad's hood hangs down onto the shoulders like long hair, heavy cloth against the body, rounded under the chin in front of the neck, with a soft fold along its edge; no more bell.
- **Robes hang straight** (ticket 450): the monk's and the dryad's cloak, the wizard's robe and the king's fall straight to the ankles with a straight hem, as robes do; the queen's gown keeps its flare.

## 0.0.359 (2026-10-19)

Everything in 0.0.358, with the hood's peak hanging behind from the side.

- **The monk's and the dryad's hood from the side** (ticket 448): like the view from behind, the hood is round over the head and its peak hangs down behind it, sticking out nowhere.

## 0.0.358 (2026-10-19)

Everything in 0.0.357, with a queen and a king, and the monk's hood from the side and behind.

- **👸 The queen and 🤴 the king** (tickets 436, 437): two new costumes in 🐯 Costume: a gold crown (the queen's dainty with three points, the king's tall with five) with jewels and a velvet cap, a robe down to the floor with a gold girdle, a white ermine collar, an ermine-trimmed mantle behind (walk them away to see it over their backs), and a golden scepter. The king puts on a full white beard (change it in 💇 Hair). Robes in five or six colours, jewels in five; the mantle and the scepter can come off.
- **The monk's and the dryad's hood** (ticket 447): from the side, its peak sticks out behind the head, away from the face; from behind, the peak comes toward you, hanging down the back of the head; facing you, as before.

## 0.0.357 (2026-10-19)

Everything in 0.0.356, with a knight, and helmets to place.

- **🛡️ The knight** (ticket 435): a new costume in 🐯 Costume: chain mail under a tabard in his colours (two of them, quartered, in halves, or plain) with an emblem on his chest (a cross, a chevron, a falcon or a crown), a shield in the same colours, a sword raised in his hand, and on his head the great helm from the castle's trap room, its plume in any of five colours. Open the helm to see his face. Walk him away from you: the helm's back, the tabard's, and the shield's wooden back.
- **🪖 Helmets to place** (ticket 435): in a new 🏰 castle group of things, the trap room's helmet, standing on its rim and facing toward you, away, left or right, each its own drawing.

## 0.0.356 (2026-10-19)

Everything in 0.0.355, with Robin Hood's quiver on his back from the side.

- **Robin Hood's quiver from the side** (ticket 445): facing left or right, his quiver is on his back, behind him, with the arrows sticking out of its top (it was drawn where it shows facing you, on the front shoulder).

## 0.0.355 (2026-10-19)

Everything in 0.0.354, with Robin Hood.

- **🏹 Robin Hood** (ticket 434): a new costume in 🐯 Costume, a forester: a green tunic with zigzag edges on its cape and hem, a strap across the chest, a belt with a pouch, brown hose and boots, a cap with a long feather, a bow in his hand and a quiver of arrows on his back (walk him away from you to see it). The tunic, cap and feather each in five colours; the bow and the quiver can come off.

## 0.0.354 (2026-10-19)

Everything in 0.0.353, with no buckles on the back.

- **No buckle from behind** (ticket 444): walking away, the gnome's and the witch's belts show no buckle, the witch's hat no buckle on its band, the wizard's cord no knot or tassels, and the monk's rope no hanging ends; facing you and from the side, all as before. (The witch's boots keep their buckles: those are on the outside of each ankle.)

## 0.0.353 (2026-10-19)

Everything in 0.0.352, with a braided beard and every beard's length.

- **🪢 A braided beard** (ticket 443): a full beard gathered below the chin into one long braid, ending in a tie and a tuft. 🪢 braid width goes from thin strands with gaps between them to a fat braid with none.
- **📏 Beard length** (ticket 443): every beard style can be shorter or longer, from half its length to three times it.

## 0.0.352 (2026-10-19)

Everything in 0.0.351, with a monk or a dryad.

- **🧘 The monk or dryad** (ticket 433): a new costume in 🐯 Costume: a hooded cloak down to the feet with a rope belt, and a staff (or none). The monk's cloak is brown and his staff has a knob; the dryad's is moss green, covered in little leaves, and her staff sprouts leaves. The hood frames the face and hides the hair, from the side too, and closes over the head from behind; put it down and the hair shows, the hood folded on the shoulders. Six cloak colours, and any beard from 💇 Hair.

## 0.0.351 (2026-10-19)

Everything in 0.0.350, with beards.

- **🧔 Beards** (ticket 442): in 💇 Hair, a beard for anyone: ✂️ short, 🧔 full, 🧙 long (down the chest), or a 🐐 goatee; its colour ("auto" matches the hair, or white, grey, brown, ginger, blonde or black), and a moustache with it or without. Facing you and in profile.
- **The wizard's and the gnome's beards are these beards** (ticket 442): putting on the wizard gives your friend a long white beard, and the gnome man a full one, if they have none; change it in 💇 Hair. Taking the costume off takes that beard off again, unless you changed it.

## 0.0.350 (2026-10-19)

Everything in 0.0.349, with a wizard costume.

- **🧙‍♂️ The wizard** (ticket 432): a new costume in 🐯 Costume, a cousin of the witch but his own: a robe down to his feet covered in stars, a gold cord round his waist, a long beard, a tall hat with stars and a crescent moon whose tip flops forward (the witch's bends back), and a staff with a glowing orb. Choose the robe's colour (six), the stars' (four), the beard's (or none), and the orb's (or no staff).

## 0.0.349 (2026-10-19)

Everything in 0.0.348, with a gnome costume.

- **🍄 The gnome** (ticket 431): a new costume in 🐯 Costume, a gnome man or a gnome woman (inspired by the gnomes of the 1977 book, drawn in the game's own simple way). Both wear a tall pointed hat (red for the man and purple for the woman, or any of six colours) and felt boots. The man has a white beard and moustache (or none, or grey, brown, ginger or black), a belted tunic with its skirt below the belt, and trousers; the woman a white blouse, a laced bodice, an apron, and a skirt of upright stripes. Facing you, in profile either way, from behind, sitting, dancing, crawling and swimming.

## 0.0.348 (2026-10-19)

Everything in 0.0.347, with the greenhouse fountain's collision oval on the floor.

- **The greenhouse fountain's collision oval is round its pool** (ticket 441): with 🔬 🧱 collision boxes on, the castle greenhouse's fountain showed its oval floating above the pool, round the middle of its column; now it lies on the floor where the basin's wall meets it, as wide as the basin, and your friend bumps into the pool there.

## 0.0.347 (2026-10-19)

Everything in 0.0.346, with each panel saying its name once.

- **Each panel says its name once** (ticket 392): a panel with one set of controls shows only the panel's name (✋ the dominant hand, 🐸 the jumps, 👁️ what stands here, 🪨 things to put here), and a panel with several drops a name that only says the panel's again (no more "🤸 poses" under 🤸 the pose, or "↔️ facing" under ↔️ facing and moving). Sliders keep their names: the brows' slider is now "slant (up ↔ down)", and the wreath's flowers are just "🌸 flowers". In 🎵 Music and 🗺️ World design, the "⛶ full screen" and "🗺️ world design" names over their buttons are gone too.
- **The elbows' and knees' new sliders** (from ticket 414) are now in 💪 the arms and 🦵 the legs, instead of a panel of their own.

## 0.0.346 (2026-10-19)

Everything in 0.0.345, with many worlds, linked to each other.

- **Many worlds** (ticket 406): the world you start in is now called **start**, and you can make more. In 🗺️ World design, the new 🌍 worlds box has a chip for each world (tap one to go there), a box for this world's name (a world with no name is "world 2", "world 3"...), ➕ a new world from a template (🗺️ the first world, or 🔲 one square: the empty room), 📐 keep this world as a template (its squares and the things in them), and 🗑️ take this world away (never the last one, and every way linked into it goes too).
- **Ways to other worlds** (ticket 406): under a square's ways out, "🌍 ways to other worlds" (closed to start) links a way out of this square to a square of any world: choose the way, the world, then the square on that world's grid drawn small. Walking out that way, your friend arrives in that world, at that square, and its way back leads here. The arrow says where it goes (🌍 world 2: 🔲 empty room), the back chip brings you back to the world you left, a linked way has a 🌍 on its square in the grid, and ✖ takes a link off (its way back too). Everything is kept in Save, Load and the autosave.

## 0.0.345 (2026-10-19)

Everything in 0.0.344, with your friend's own hair when lying on a sleeping bag.

- **Lying on a sleeping bag, your friend keeps their own hair** (ticket 425): short hair close round their head, long hair fanned out on the pillow (the longer, the farther), curls in a ring, spikes, a big afro, or two pigtails tied and spread on the pillow; and none if they have none. Their bangs show as before. The 👁️ see-through sliders work there too.

## 0.0.344 (2026-10-19)

Everything in 0.0.343, with the far arrows quick to show again.

- **Fixed: with ♿ "🧭 all arrows" on, the far arrows came in slowly** (ticket 424): since 0.0.335 they took half a second to show, held almost hidden most of that time, as an arrow takes to fade away. Now they show as quickly as a near one, and still fade slowly when you switch them off.

## 0.0.343 (2026-10-19)

Everything in 0.0.342, with elbows and knees that bend toward you and away.

- **🏋️ Elbows curl, 🫖 elbows swing, 🦶 knees curl and ↔️ knees swing** (ticket 414), four new sliders in 🦒 Neck, arms & legs. The curl bends a forearm forward, as lifting a weight: facing you, the hand comes toward you, so the forearm looks short. The swing turns it side to side, as swinging a kettle across a table: + out, − in. The knees do the same backward: curl them and the feet go back behind, so facing you your friend kneels, their shoes hidden behind their legs; swing them and the feet go out or in. Both arms and both legs move alike. They work with every animation and pose.
- **Crouching, facing you, your friend's arms reach forward for balance** (ticket 414): before, their elbows bent out to the sides.

## 0.0.342 (2026-10-19)

Everything in 0.0.341, with the hair right when lying on a sleeping bag.

- **Lying on a sleeping bag, your friend's hair is on the back of their head** (ticket 412): with their head back on the pillow, looking up, the hair is round the far side of it and on the pillow, not over the top of their face.

## 0.0.341 (2026-10-19)

Everything in 0.0.340, with your friend looking up when lying on a sleeping bag.

- **Lying on a sleeping bag, your friend's face looks up at the ceiling** (ticket 412), not at you: you see it from their feet, so it is on the top of their head, their eyes looking up past their forehead, the hair at the top, their ears at the sides and the bottom of their chin in shadow.

## 0.0.340 (2026-10-19)

Everything in 0.0.339, with your friend's face and hair right when lying on a sleeping bag.

- **Fixed: lying on a sleeping bag, your friend's eyes were drawn over their hair** (ticket 412): the hair over the top of their head came down past their eyes. Now their face looks out from the middle of their head, the hair over the top with its bangs above the eyes, and spread on the pillow behind.

## 0.0.339 (2026-10-19)

Everything in 0.0.338, with a better fit on the sleeping bags.

- **Fixed: lying on a sleeping bag, your friend's head stood up past the pillow** (ticket 412): it was so big that it reached over the far end of the bag. Now it is a little smaller and rests on the pillow.

## 0.0.338 (2026-10-19)

Everything in 0.0.337, with lying down on the tent's sleeping bags.

- **🛏️ Lie down on the sleeping bags in the orange tent** (ticket 412): click a sleeping bag with 👆 walk where you click on, and your friend walks there and lies down on it, face up, head on the pillow by the far wall and feet toward you, pointing up, along the bag as it runs into the tent. It is a new way of drawing your friend, just for lying along a side wall: their head round on the pillow with their face looking up and toward you, their body and arms along the bag, the arm by the wall behind the body. The same on the red bag by the left wall and the purple one by the right. For now it shows your friend's own colours (skin, hair, shirt, pants and shoes), not a costume.

## 0.0.337 (2026-10-19)

Everything in 0.0.336, with many more things to sit on.

- **🪑 Sit on rocks, toadstools, logs, barrels, chests and benches, wherever they are** (ticket 413): anything with a top to sit on is a seat when it is the right height for your friend, whether the place has it or you put it there. That makes 44 more seats in 10 places: the rest of the fairy ring's toadstools and its two stones, the rocks on the mountain trail, under the sea, in the deep sea, at the jungle waterfall and outside the castle, the castle's far barrel, the camp's near log, the path's inner mushrooms, the deep sea's and the cave's treasure chests, and the tree house's toy chest. A rock, toadstool, log, bench, barrel or treasure chest you put somewhere is a seat too, at sizes where it is not too big or too small. Click it with 👆 walk where you click on and your friend walks there and sits (a press that moves still drags it), drag your friend onto it, or use its 🪑 chip in 🏞️ Background. Hot, prickly, sharp and spooky things are not seats: cauldrons, campfires, bushes, stalagmites, skeletons. [SEATS.md](SEATS.md) lists every object and why.

## 0.0.336 (2026-10-19)

Everything in 0.0.335, with a falling boulder turning you into rainbow bubbles part by part.

- **Hit by a falling boulder, you go to rainbow bubbles one part at a time** (ticket 415, for N): as the boulder comes down on you, your head turns into bubbles first, then your body and arms, then your legs, then your feet, each part the moment the boulder reaches it. Before, all of you turned into bubbles at once when it landed. The bubbles are still every colour of the rainbow, red at your head down to violet at your feet; they spill out, float and pop, and then you are back where you stood.

## 0.0.335 (2026-10-19)

Everything in 0.0.334, with the nav arrows' words where the arrows are, and staying longer.

- **➡️ A nav arrow's words show when the pointer is on the arrow** (ticket 404, Matthew: "the 'hover over' feature of nav buttons sometimes seem misaligned?"): the pointer was judged against where the way meets the floor, but the arrow is drawn spaced apart from the others and kept inside the picture, so in the fairy forest the arrow to bubble space showed its words while the pointer was on the flower below it. Now the pointer is judged against the arrow as it is drawn.
- **♿ "⏳ arrow words stay"** (ticket 404, Matthew: "they disappear too quickly -- let's slow it down by a control in accessibility, default 2x slower than now, with room on each side in slider, but more room up than down"): in ♿ Accessibility's 🔤 words box, how long an arrow's words take to go once the pointer leaves: 500 ms by default, twice the old 250, from 250 up to 1500. They stay near full until the end, then fade; they still come quickly.

## 0.0.334 (2026-10-19)

Everything in 0.0.333, with the bubble room's foam moving again, and cheaply.

- **🫧 The bubble room's foam moves from the start again**: its bubbles breathe and now and then pop and grow back, on the walls, the floor and the ceiling, without slowing the game down. The moving bubbles are drawn as layers of their own that the browser moves by itself, under the picture of the room, instead of animations inside it (in our tests: from about 22 to about 58 frames a second on a slow computer). If you had made it still, it stays still; if it was still only because that was the start, it moves again. The bubbles stuck out into the room hold still for now.

## 0.0.333 (2026-10-19)

Everything in 0.0.332, with the bubble room's friend and things back in reach.

- **🫧 In the bubble room you can pick up your friend again, and the bubble makers**: the bubbles stuck out of the foam in front of your friend were one see-through picture as big as the room, and it caught every press, so nothing behind it could be dragged or tapped. Now it lets presses through.
- **🧪 The bubble room is in every release's speed checks**: four bubble makers, walked and dragged at full speed, and measured on a slow computer's speed and in Firefox.

## 0.0.332 (2026-10-19)

Everything in 0.0.331, with a slow computer noticed in the first second of a walk.

- **🐢 On a slow computer the first walk no longer stutters for seconds**: the game holds the scenery still on a computer that can't keep up, but it used to watch about 120 frames of walking first (at 10 frames a second, 12 seconds of stuttering, every visit). Now, when a walk's first frames are nearly all late, it decides at once (about a second and a half at 10 frames a second); a fast computer with the odd slow frame is not caught by it.

## 0.0.331 (2026-10-19)

Everything in 0.0.330, with adding a thing to a full place quick again.

- **🫧 Adding the 257th bubble maker no longer stalls**: the game worked out where every thing in the place stands, one by one through the screen, each time one was added. Now it remembers each thing's spot and only works out the new one: with 256 makers, adding one takes about a tenth of the time.

## 0.0.330 (2026-10-19)

Everything in 0.0.329, with a place full of things drawn with less work.

- **🫧 Many things in a place cost less to draw**: the things you put in a place (bubble makers, rocks, signs...) are drawn again only when one of them changes, not looked over one by one on every frame. With 256 bubble makers, about a quarter of the work it was.

## 0.0.329 (2026-10-19)

Everything in 0.0.328, with the controls touching the page only where something changed.

- **🏃 Smoother again with 🏞️ Background open**: each time the game brought the controls up to date, thousands of buttons and rows were told again what they already showed, and the browser redid the panel's look for each; now only what changed is touched (about 4,000 changes a refresh down to 0 when nothing moved).

## 0.0.328 (2026-10-19)

Everything in 0.0.327, with 🏞️ Background open and walking smoother still.

- **🏃 No hitch every quarter second with 🏞️ Background open**: while your friend moves, the game brings its controls up to date a few times a second; with the Background tab's thousands of settings that took a frame to itself each time. Now it is spread over a few frames while your friend moves, and done all at once when they stop.

## 0.0.327 (2026-10-19)

Everything in 0.0.326, with the east and west arrows showing at the edges again.

- **➡️ Walk to a place's east or west edge and its arrow shows again**: since 0.0.321 moved those arrows up off the corner arrows, they only showed when your friend was near the back. Now the arrows stay where 0.0.321 drew them, but whether your friend is near a way is judged where the way is on the ground, as before.

## 0.0.326 (2026-10-19)

Everything in 0.0.325, with a small friend easy to pick up.

- **🧍 A small friend can be picked up and moved, not just posed**: far off, or up on the tree house's rope bridges, your friend is small on the screen, and nearly every press on them took a hand or a foot to pose instead of moving them, so you could not drag them down off a bridge. Now a hand's or foot's grip is never more than an eighth of your friend's height; at ordinary sizes nothing changes.

## 0.0.325 (2026-10-19)

Everything in 0.0.324, with a distant shore for the open water.

- **🏝️ A distant shore on the open water**: in 🏞️ Background, at the open water, choose ⛰️ mountains far off (🏔️ snowy peaks, ⛰️ a blue range or 🌄 rolling hills), 🌲 a forest along the shore (pines, round trees, or both mixed) and 🏖️ a beach between the water and the trees, in any mix: they stand one in front of the other on the horizon. All off to begin with.
- **📏 The shore's size**: bigger looks nearer. With ✏️ changes going to 📍 only this square, each open water square keeps its own size, so a column of them can bring the shore closer square by square as you swim north.
- **🌊 Swim as far as the horizon, never onto the shore or into the sky**, whatever shore you choose; with a way north, swimming on into the horizon takes it.

## 0.0.324 (2026-10-19)

Everything in 0.0.323, with open water that promises nothing it doesn't have.

- **🌊 No island and no sail boat far off on the open water**: they looked like places to go and a boat to take, but neither could be reached. The sea and the sky are all there is out there now (a distant shore you can choose is coming next).

## 0.0.323 (2026-10-19)

Everything in 0.0.322, with the 🏞️ Background tab much lighter while you play.

- **🏃 Smoother walking with 🏞️ Background open**: the castles' little theme flags were each working out a whole castle several times a second while your friend moved, which made walks and drags stutter whenever the Background tab was open (side panel or full screen). Now each castle is worked out once, and the castle designer only redraws when something in it changes. The tab's refresh takes about a fifth of the time it did.

## 0.0.322 (2026-10-19)

Everything in 0.0.321, with the world growing at its left and top too.

- **↖️ Add or take away at the left and top of the world**: in 🗺️ World design, turn on ↖️ left and top edges, and ＋/－ column works at the left edge and ＋/－ row at the top, every square moving over with its places, its ways, the start and your friend. As at the right and bottom, a column or row is taken away only while it is empty.

## 0.0.321 (2026-10-19)

Everything in 0.0.320, with the nav arrows apart in every place.

- **➡️ The nav arrows never sit on each other**: on every background, the arrow on a side (east, west) now stands clear of the arrow in the corner below it (south-east, south-west), with room between them, so each is its own thing to tap.

## 0.0.320 (2026-10-19)

Everything in 0.0.319, with the castle designer full screen.

- **🏰 The castle designer, full screen**: in a castle, 🏞️ Background's ⛶ the castle designer, full screen opens it beside the game: the castle's map big across the top, then every room in its own box, in order, each with its picture and named by what it is (2 · 🌿 greenhouse), the room you are in marked; tap a room on the map and its box lights up and comes into view. Then the castle's look, its colors and who lives there. ✕ or Escape goes back (to the background designer, if you came from there).

## 0.0.319 (2026-10-19)

Everything in 0.0.318, with hidden settings staying hidden and arrivals that never stand in a thing.

- **🫧 Settings that are off stay out of sight**: a slider or switch that only matters when another one is on (the bursting bubbles' own settings, and others like it) could still show while hidden; now it shows only when it applies.
- **🧭 Coming into a place, your friend never ends up standing inside something**: stepping off the arrows could put them on the very edge of a thing (the house's bucket), inside it; now the spot they step to is checked as it is.

## 0.0.318 (2026-10-19)

Everything in 0.0.317, with the boxes lighting what they change.

- **✨ Point at a box, and its part of your friend lights up in the game**: 👄 the mouth lights the head, 🦵 the legs the legs and feet, 👕 the shirt the shirt, and so on, so you see what you are about to change.

## 0.0.317 (2026-10-19)

Everything in 0.0.316, with no click on a doorway that leaves the doorways dead.

- **🚪 A doorway with no way to walk to it still takes you through**: when no path could be found to a doorway you clicked, the walk stopped before it began and from then on no doorway would take a click; now your friend goes through anyway.

## 0.0.316 (2026-10-19)

Everything in 0.0.315, with levers that leave the passages working.

- **🎚️ Clicking a lever no longer sends you out of the room**: a click on a lever (or on your friend) also counted as a click on the nearest doorway, so your friend could set off through it while walking to the lever, and afterwards no doorway, door or trap door would take a click. Now a click on a lever only pulls the lever.
- **🎚️ After pulling a lever, your friend steps aside**, so the lever is not hidden behind them for the next click (the tree house's trap door: shut it, then open it again).

## 0.0.315 (2026-10-19)

Everything in 0.0.314, with every tab able to open full screen beside the game.

- **⛶ Every tab full screen, beside the game**: the friend's tabs, Controls, Inspection, Accessibility and Advanced each open full screen with ⛶, as Music, World design and Background do: their boxes side by side on the left, the game itself on the right to watch as you change things (from a window 900 pixels wide), the bar between them dragged to share the room; ✕ or Escape back.

## 0.0.314 (2026-10-19)

Everything in 0.0.313, with the Inspection and Accessibility switches side by side.

- **🔬 Inspection's and ♿ Accessibility's switches side by side**, as chips, instead of one to a line; a switch turned on opens to the panel's width with its own settings.

## 0.0.313 (2026-10-19)

Everything in 0.0.312, with the castle's color themes shown as flags.

- **🚩 A castle's two color themes as flags**: each theme shows its flag and carpet by its name, as you set them; and in each room, the 🎨 theme A and theme B chips show their flags, so you see the colors you are choosing.

## 0.0.312 (2026-10-19)

Everything in 0.0.311, with each thing you put in a place as a card.

- **🪨 Things you put in a place as cards**: each with its picture, its name and ✖ at the top, its buttons together, and a line for each setting; the cards side by side when there is room, instead of one long row each.

## 0.0.311 (2026-10-19)

Everything in 0.0.310, with the Background, Controls and Advanced tabs in boxes too.

- **🏞️ The Background tab in boxes, beside the game or in its own panel alike**: 🗺️ the place, the place's own (🦆 lake, 🌳 tree house...), 🧍 the friend here, 👁️ what stands here, 🪨 things to put here, 🎵 the music moves it, 🏰 the castles.
- **🫧 Each bubble theme shows its bubble**, drawn as you set it, by its name; the three themes side by side when there is room, and no 🅰️ in front of every row of theme A.
- **⚙️ Advanced in boxes**: the look, this game, playing, tests and timing, the effects kept off till they are fast, what's new; each test's long account folded under ℹ️ how it works.

## 0.0.310 (2026-10-19)

Everything in 0.0.309, with the friend's tabs laid out in boxes.

- **🧰 The friend's tabs in boxes**: Body, Face, Eyes, Hair, Neck, arms & legs, Shirt, Hands, Pants, Swim & undies, Feet, Visibility and Costume each show their settings in boxes, one for each thing you can see (👄 the mouth, 👃 the nose, ✨ the shine, 🐈‍⬛ her cat...), named once at the top; a box shows only when it applies (the witch's boxes with the witch on).
- **📏 A slider on one line**: its name, the slider, its number; what a name used to explain ("100 = a leg's length") shows when you point at it.
- **🔘 Switches side by side**, as chips, instead of one to a line; no dashed boxes anywhere (the controller's, the bubble themes', the castle's groups are boxes now).
- **🎮 The controller's buttons fit the panel** (their 🎯 set ran off its edge).

## 0.0.309 (2026-10-19)

Everything in 0.0.308, with planned walks that find their way more often, and bubbles that stick in one plane.

- **🧭 A tap to walk finds a way out of tight spots**: from between the tree house's bushes, the ladder at one end and the trunk at the other, a tap behind the tree now walks round behind it (before, nothing happened); and round flat things like bushes the planned way keeps closer, so lanes between them stay open.
- **➡️ Coming into a sea square, the arrows stay clear of you while you tread water** (your arms swing out with each stroke).
- **🫧 In bubble space with 🔭 depth at 0, bubbles that touch stick together** (they passed through each other, though drawn side by side).

## 0.0.308 (2026-10-19)

Everything in 0.0.307, with four new turtle steppers dubs to try in place of D.

- **🐢 Turtle steppers dub D-1 to D-4**, in 🌴 reggae right after D, each a candidate to replace it: D-1, steppers, four on the floor (D minor, fast); D-2, rub-a-dub, lilting (D major); D-3, the drum break, made plain (E minor); D-4, heartbeat, hand drums (A minor, slow). D stays until one is picked.

## 0.0.307 (2026-10-18)

Everything in 0.0.306, with bubble makers that cost less.

- **🫧 Many bubble makers together stay smoother**: all the makers in a place now share twelve bubbles a second between them (before, each blew its own, and with many makers most were made only to vanish at once), and their bubbles float in a layer of their own over the picture, so moving them no longer redraws the whole place. On a slow computer, sixteen makers walked at 53 frames a second instead of 37.

## 0.0.306 (2026-10-18)

Everything in 0.0.305, with the bubble room's foam still at the start.

- **🫧 The bubble room's foam starts still**: its moving foam was what made the room slow (in Firefox about 19 frames a second, against 50 or more with it still), not the bubble makers put in it. It still moves if you want it to: 🏞️ Background, 〰️ how much the foam moves, which now says what moving costs. A saved friend whose foam moved by default is made still once; an amount you chose is kept.
- **🐢 When the bubble room gets too slow for more things, it says the foam costs the most**, and how to still it, before it asks for a thing to be taken away.

## 0.0.305 (2026-10-18)

Everything in 0.0.304, with walking into things stopping in front of them.

- **🪨 Walking into a stone, a bush or a table, you stop in front of it**: before, holding the way into a small thing let your feet creep a few steps into it as they swung.

## 0.0.304 (2026-10-18)

Everything in 0.0.303, with a new remix and tidier walks.

- **🦇 A new track: the bat-wing boogie remix**, in 🕺 groove, made from Matthew's own mix: faster (173 beats a minute), lower (in B flat minor), with doubled fiddles, bass and drums, each set just so.
- **🚶 Walking round things, your feet keep off them**: going round the witch's cauldron or the big root outside her hut, your stride no longer brushes through the edge.

## 0.0.303 (2026-10-18)

Everything in 0.0.302, with the diving gear where it belongs.

- **🛢️ Crawling, the air tank stays on your back**, not on your belly: crawling toward us it rides on your back behind your head, crawling away it lies flat along your back, and crawling sideways it is on top.
- **⚓ The old diving suit's air hose always rises up to the surface** from your helmet, even lying down, crawling or swimming flat, instead of pointing the way your head does.
- **🧚 Changing out of a diving suit takes its gear off too**: the snorkel, mask and tank. You can still put any of them on with any costume.

## 0.0.302 (2026-10-18)

Everything in 0.0.301, with you the same size inside the tree house as outside it.

- **🌳 Outside the tree house you are as big as inside**, against its doors: before, you were small out on the porches and bridges, and had to crawl through the rooms' doors but walked through the same doors from outside. Now you walk through both, and a big friend (try the size slider at 150) crawls through both. Long legs or short, your shape counts outside too.
- **🚪 The tree rooms' doors to the bridges, the witch's door out, and the greenhouse's arches are taller**, so you walk through them, as through every other door made for you.

## 0.0.301 (2026-10-18)

Everything in 0.0.300, with 🐠 under the sea come alive.

- **🐟 The fish swim**: the orange, yellow, pink and blue fish each swim round their own loop, tails beating, and a little school of seven swims together high on the left, like the deep sea's.
- **🫧 The bubbles rise**, wobbling and fading: a stream from the shipwreck, one from a clam in the sand, and more on their own.
- **🦈 The picnic sharks move a little**: they bob, sway their tails, blink, and now and then lift their forks.
- **🚢 The shipwreck in more detail**: planks and seams, a broken rail, a cabin at the stern, barnacles, weed swaying on the hull, portholes that glint, a sail in tatters with a torn flap, a splintered mast, a rudder, an anchor on its chain, and a treasure chest half sunk in the sand.
- **🌊 The water behind in layers**, like the deep sea: hazy far rocks, far kelp, nearer ridges, and the light shimmering.
- On a slow computer everything holds still, as everywhere; the deep sea's school now holds still where it swims, too.

## 0.0.300 (2026-10-18)

Everything in 0.0.299, with you arriving beside the arrows, not on them.

- **🧭 Walking from one place into the next, you arrive just beside the nav arrows**, not standing on them, so the arrow to tap next is not under you: to the right of the ⬆️ and ⬇️ when you come in at the top or bottom, a step inward when you come in at a side. Before, almost every way in landed on one.

## 0.0.299 (2026-10-18)

Everything in 0.0.298, with you drawn in front of and behind the right things.

- **🌳 Up in the tree house, the houses below you are in front of you**: on the high bridge, the owl's house covers you where they meet, as a nearer house should. On the low bridge and on the ground, nothing changes.
- **🛻 Sitting in the wagon by the house, you sit in it**, not behind it, and you bump into the wagon where it stands, not just behind it. The front step's seat is where the step is drawn too.
- 🙈 Hiding the tree house hides its houses with it.
- Under the hood: a new automatic check makes sure everything you can bump into is drawn in front of you or behind you as you stand.

## 0.0.298 (2026-10-18)

Everything in 0.0.297, with three more quiet breakages of the same kind mended.

- **🪧 The plain sign is back among the 🪧 signs** to put in a place (it had gone missing in 0.0.287).
- **🌳 A tree, 🌲 a pine and 🌿 a bush you put in a place start with their settings again** (their height, leaves, tiers and flowers).
- **🏰 A castle room draws again when its flags, carpet or horses change.**
- Under the hood: before every release the game's code is now checked for a note that hides code after it, which is how all six of these went missing.

## 0.0.297 (2026-10-18)

Everything in 0.0.296, with four things that were quietly broken mended.

- **🕳️ The cave's crystals glow where they grow again**, and the light falling in through the waterfall is drawn whole.
- **🏰 The castle's 🌳 trees nearby and 🌊 moat sliders show their numbers again** (they said NaN, and sat in the middle), starting at what the castle really has.
- **🔬 Inspection's 🧩 what it's made of boxes and names the 🌳 leafy trees and 🌲 pines again**, each with its own chip.
- Under the hood: the game's automatic checks catch broken drawing code like the cave's, and a dozen of them were brought up to date with what the game does now.

## 0.0.296 (2026-10-18)

Everything in 0.0.295, with the spiral staircase's walks going up the stairs.

- **🌀 In the spiral staircase, a click behind the bottom steps walks you up the stairs**, the short way, instead of all the way round the column. Walks across the floor are as before.

## 0.0.295 (2026-10-18)

Everything in 0.0.294, with the tree rooms' trunk solid to its foot.

- **🌳 You no longer walk onto the tree trunk's foot in a tree house room.** The trunk's dark foot in the floor reached further forward than its box, so you could stand on it, looking as if you walked into the trunk. Its box now covers its foot, in every tree room.

## 0.0.294 (2026-10-18)

Everything in 0.0.293, with the path through the woods' pebbles an object.

- **The pebbles on the path through the woods** are an object of the floor now, to see, hide and place. With them, what every place draws is an object, except the ground itself (paths, roads, brooks, streams, pools, waves), the backdrops, and the doorways.

## 0.0.293 (2026-10-18)

Everything in 0.0.292, with the last of the outdoor places' things made objects.

- **👁️ More of every outdoor place is an object you can see, hide and place**: the light rays under the sea and in the deep sea, the sand's specks and ripples, the stones by the cemetery's brook and its flowers, the lake's sun on the water, pebbles and grass, the far lake and village below the mountain trail, the flowers by the waterfall's trees, and the witch's grass.
- **🧙 In the witch's woods each dead tree and each skull on the fence is its own object** now, in the 👁️ list one by one (17 trees, 11 skulls), instead of a row each.

## 0.0.292 (2026-10-18)

Everything in 0.0.291, with reflections on the mixing board.

- **🪞 A reflection for every channel** (with a ⧉ dup, a part and its reflection together): 🎼 in key (some steps up or down the tune's scale, so it harmonizes and never clashes), 🪞 mirror (the tune turned upside down, in key), ⏪ backward (each bar played backward), 🔁 canon (the same part some beats later, a round), and 🎶 counter (a new line against another part you pick: thirds and sixths, moving the other way where it can). The octave presses still move a part up or down.

## 0.0.291 (2026-10-18)

Everything in 0.0.290, with every tune's key.

- **🎼 Every tune knows its key**, and the playing line says it: "▶ playing: 🐠 bubble reggae (in A minor, 72 beats a minute)". Move a tune with 🎵 transpose and the key moves with it (up 2: B minor). The turtle deep-dub, 13 half steps down, is in E-flat minor.

## 0.0.290 (2026-10-18)

Everything in 0.0.289, with copies of a part on the mixing board.

- **⧉ dup on every channel of the mixing board**: a copy of that part, "bass A dup", playing the same notes on the same instrument, with its own strip starting from the original's settings, so you can move it an octave up or down and make the part fuller. A second copy is "dup 2". Each copy has a ✖ to take it away, and copies are saved with the tune's mix.

## 0.0.289 (2026-10-18)

Everything in 0.0.288, with more things in your way, as they should be.

- **🐠 Under the sea, the seaweed, kelp, coral and bigger shells each have a small box at their foot**, so you weave among them, and the rocks and the shipwreck are in your way too.
- **Things that had no box now have one**: the forest's and the castle's big trees, the witch's giant tree, the cemetery's headstones, cross and stag stone, the cave's stalagmites, the hut's woodpile, the trap room's helmet, the greenhouse's plants and watering can, and the toys on the house's lawn.
- **🪜 Every ladder has a box at its foot**, so a walk goes round a ladder it isn't climbing; climbing it works as before.

## 0.0.288 (2026-10-18)

Everything in 0.0.287, with the castle's birds flying.

- **🐦 The birds over the castle flap their wings and glide** slowly back and forth across the sky, their wingbeats in time with the scene's wings LFO, as the butterflies' are (on the mixing board, the castle's "🐦 the birds' wings").

## 0.0.287 (2026-10-18)

Everything in 0.0.286, with the witch's woods' trees, fence and skulls found and placeable.

- **🧩 What it's made of finds the witch's woods' trees, fence and skulls**: each of its 19 dead trees (as 🌳 trees), each of the 11 skulls on its fence (💀 skulls, a new kind), and the fence (🪵 fences, a new kind).
- **Three new things to place**, in 💀 spooky, drawn just as the woods draws them: a 🌳 dead tree (a stump, tall or giant, weeping or not, its colour), a 💀 skull on a stake (its eyes' size), and a 🪵 fence of stakes (3 to 16, with skulls on it or not). Each is in your way, as the woods' are.

## 0.0.286 (2026-10-18)

Everything in 0.0.285, with balancing on bubbles.

- **🫧 Standing on the bubble room's floor, you balance**: feet apart, knees bent, arms out to the sides, swaying, more the more the bubbles move. **Walking there is careful**: shorter steps on bent knees, arms spread and rocking, tipping a little each step. A new slider, "〰️ how much the bubbles move under you" (0 is a still floor, as anywhere). Running and crawling are as before.

## 0.0.285 (2026-10-18)

Everything in 0.0.284, with placed things glowing.

- **✨ Every thing you place glows under the pointer**, warm, as the doors and levers do, so you can see it is something you can pick up and move. The place's own drawings don't.

## 0.0.284 (2026-10-18)

Everything in 0.0.283, with bubbles that stick.

- **🫧 A bubble maker's bubbles stick** to the bubbles standing out of the bubble room's walls and floor, so the room grows clusters, and in bubble space to its floating bubbles, following them as they drift. How often is a slider in each place (0 to 100, 30 to start). A stuck bubble swells a little and fades after a while, pops when you walk into it, and floats free if the bubble it holds pops.

## 0.0.283 (2026-10-18)

Everything in 0.0.282, with walking off a seat fixed.

- **Fixed: clicking to walk did nothing while sitting** (on a throne, a couch, a bed). Since 0.0.278 a walk with no way round goes nowhere, and the seat you were getting off counted as in the way. Now you get up and walk where you clicked, or to another seat and sit there.

## 0.0.282 (2026-10-18)

Everything in 0.0.281, with fallen boulders you can move.

- **🪨 A boulder that falls into a trap room pit becomes a rock you can drag**: "fallen 1", "fallen 2" and so on. While it lies in its pit, the pit is covered; drag it off and the pit is open again. The blue lever's next pull lifts away a rock still in its pit, or drops a new one if you moved the last.

## 0.0.281 (2026-10-18)

Everything in 0.0.280, with levers pulled from where you stand.

- **🎚️ Pull levers from where you stand**, a new switch in ⚙️ Advanced (off to start): every lever goes over at once when you press it, and the friend stays put, so you can stand where the boulder falls and pull its lever (for N).

## 0.0.280 (2026-10-18)

Everything in 0.0.279, with collision shapes that follow the things.

- **⭕ Round things are round to bump into**: barrels, buckets, cauldrons, pots, stumps, rocks, stones, toadstools, mushrooms, bushes, the campfire and the well stop you at their oval, not a box around it, so you can stand closer at their sides, and walks go round them in a curve. 🔬 draws them as ovals.
- **🪑 Tables are their top and legs**: the potion hut's table is a top on four legs, the healer's tea table a round top on one pedestal; bunk beds, beds and benches are their posts, frames and seats. Your legs now bump a table's top too, so a low table still stops you and only a high one could be walked under.

## 0.0.279 (2026-10-18)

Everything in 0.0.278, with the tree houses' trap doors on levers.

- **🎚️ Each tree house's trap door has a lever on the wall.** It starts open; pull the lever and the trap door swings shut: planks over the hole, the ladder gone, and you can walk across it. Pull it again to open it. Shut, it is no way down, and the house below's ladder up into it goes nowhere either. Its opening is still the house's own shape (■ square, ● circle, ▲ triangle), and there is a "trap door shut" switch in 🏞️ Background too.
- **🔱 The castle's trap room pits have no spikes now, unless you want them**: a spikes switch for each trap room, off to start.

## 0.0.278 (2026-10-18)

Everything in 0.0.277, with planned walks kept out of everything.

- **🚶 No more walking through the tree room's corner trunk.** A thing standing nearly against the back wall now reaches it, so there is no thin lane behind it to get stuck in (the tree trunk, the living room's lamp, a waterfall tree, the castle room's chest, the hut's couch, the house's front step).
- **A planned walk never goes straight through something** when it finds no way round: it walks as near as a way goes, and then doesn't sit or go in. From a gap beside a thing it steps out sideways first.

## 0.0.277 (2026-10-18)

Everything in 0.0.276, with bubble space's pops bursting.

- **🫧 Bubble space has depth**: bubbles are smaller and slower farther back, the far ones drawn behind (🔭 how much, in 🏞️ Background; 0 is all one size, as before). Only bubbles at about the same depth stick together.
- **💥 A pop can burst into smaller bubbles** of the same style, flung out from it: how likely (0 to 100) and how many, about (1 to 8). They drift on the room's wind and fade after a few seconds, and pop when the friend walks into them again.
- **✨ Lines shoot out from a pop**, all around it, like the sun's rays, growing and gone.
- **🔊 A pop has a sound**, with settings of its own: how loud (0 for none; silent while the music is muted), its instrument (🫧 bloop, 🎻 pluck, 🎶 music box, 🔔 bell, ⚡ zap), its pitch (smaller bubbles higher), and the instrument's knob.

## 0.0.276 (2026-10-18)

Everything in 0.0.275, with the new tracks starting from Matthew's mix.

- **🛋️ Couch-cushion rave A starts from Matthew's own mixing board**: the hooks three octaves down, darker and roomier, the stabs, the sparkle and the ghost two down, the first bass one, the hats and the shaker in the room. **Rave B starts with his two basses**: louder, the wah wide open, toned down, in the room and panned apart.
- **The other new tracks start low and groovy too**, as he likes it: raves B to E with the hooks three octaves down and the stabs, sparkle and ghost two, and both basses loud with the wah open; 🐢 turtle steppers dubs A to D with a wah bass, loud and wet, the melodicas two octaves down and the chop one. A player's own mixing board still wins once they change a knob.

## 0.0.275 (2026-10-18)

Everything in 0.0.274, with a way to the game's page.

- **⚙️ Advanced links to the game's page**, matthewhammer.org/hazel/dress-up/, with its music, its pictures and its reports, beside the link to its code.

## 0.0.274 (2026-10-18)

Everything in 0.0.273, with smoother planned walks.

- **Fixed: a walk round things stopped for a moment at each turn**: walking to a click by a path of several legs (round pits, furniture), your friend stood still for a moment at each bend, and with 🔬's 👣 planned path shown, the path flashed off and on. Now they walk straight on from leg to leg, and the path stays up.

## 0.0.273 (2026-10-18)

Everything in 0.0.272, with the trap room's things in detail.

- **A knight's helmet**: the helmet by the trap room's left pit is now a knight's great helm, its eye slit, breathing holes, a band of rivets, a shine on its dome and a red plume.
- **A sword in detail**: its blade with a groove down it and a bright edge, a gold crossguard, a leather-bound grip and a round pommel.
- **Levers in detail, and shining**: each lever's panel is bevelled iron with a rivet at each corner, the slot its handle swings in and a hub at the pivot; the handle has a bright edge, and the knob catches the room's light, flickering as the torches do. Three new settings in 🏞️ Background, wherever there are levers (the trap room's or placed ones): ✨ the shine's size (0 for none), its colour, and 🔥 how much it flickers.

## 0.0.272 (2026-10-18)

Everything in 0.0.271, with levers pulled by hand.

- **Pull a lever yourself**: with 👆 walk where you click on, pressing a lever (the trap room's on its wall, or one you placed) walks your friend over to it; they turn to face it, reach up and pull it down, and it goes over as they pull, its trap door swinging after. Already standing at it, they pull at once.
- **✋ dominant hand** (🧍 Body): right, left, or both (a coin flip each time): the hand that pulls.

## 0.0.271 (2026-10-18)

Everything in 0.0.270, with walking behind the house and the castle.

- **Walk round behind the house and the castle**: the back line (🔬's blue line) is now the horizon, the far edge of the lawn or the meadow, not the building's front. Each building has a collision box of its own at its foot, only as deep as it looks (the castle's back to its moat's far side), with room behind it for your feet. The house stands a little nearer to make that room.
- In the healer's hut, your feet fit round behind the soup pot.

## 0.0.270 (2026-10-18)

Everything in 0.0.269, with a truer walk.

- **Walking: the knee leads, the foot follows**: seen from the side, each step now swings the knee through first, bent, the foot trailing behind it; then the foot swings on ahead to land, the leg straightening; and the leg stays straight while its foot is down. Before, the knee and the foot moved together.

## 0.0.269 (2026-10-18)

Everything in 0.0.268, with walking up and down stairs.

- **🔼 walking up or down stairs**, a new animation (🧍 Body, "🎬 animations"), with a choice for the way: **up**, climbing with the feet, each knee lifted high as onto the next step and a rise each step; **down**, a controlled fall, quicker, each foot reaching on and down while the other knee bends under the weight, the arms out for balance. No stairs are drawn: it is the move itself.

## 0.0.268 (2026-10-18)

Everything in 0.0.267, with collision boxes that end where things end.

- **A thing's collision box is what stands, not what lies flat beside it**: the king's chest of gold no longer stops you at the coins spilled to its left, and rocks, stones, the fountain and the hay cart no longer stop you at their shadows (32 boxes across the places got narrower, 🔬 shows them).

## 0.0.267 (2026-10-18)

Everything in 0.0.266, with nine new upbeat tracks, for M.

- **🛋️ couch-cushion rave A to E** (🎵 Music, 🏠 house), each beside the first, about as long, in a major key (or funk's 9ths) at 120 to 128 beats a minute, with drums of its own: **A** a triplet shuffle, **B** three against four (a cowbell in threes over the beat), **C** a drum solo (snare and bongos in fives, sixes and threes, a wah bass), **D** a breakbeat, **E** half-time with triplet fills. Each has its own chords, and its hook made of their notes.
- **🐢 turtle steppers dub A to D** (🌴 reggae): **A** a fast triplet shuffle (at the super-dub's pace), **B** the one-drop, **C** rockers with three against four, **D** with a drum break; each with chords, drums and tunes of its own.
- Fixed: the castle rooms drew a sliver of a stone course with a negative height at the top of the back wall, an error in the page's console on every drawing (nothing you could see).

## 0.0.266 (2026-10-18)

Everything in 0.0.265, with pointing to sit and pointing to lie down.

- **🛌 Lying down**, a new pose: on the back, face up (the usual), or on the side, facing you, with the head toward your left or your right (🧍 Body, "🤸 poses").
- **Beds and blankets to lie on**: the grass hut's bed, the bottom bunk in tree house room 1, and the two blankets on the beach. Put your friend down on one, or click it, and they lie down on it, their head on the pillow.
- **Point to sit, point to lie down**: with 👆 walk where you click on, pointing at a seat or a bed makes it glow and says what a click does ("🪑 sit on the beanbag", "🛏️ lie down on the bed"); click, and your friend walks there and sits or lies down.
- **🔬 seats and beds**: the collision boxes have a chip of their own for them, each labelled "sit" or "lie down"; a blanket is flat, so it is in nobody's way.

## 0.0.265 (2026-10-18)

Everything in 0.0.264, with more of the tree house's meadow to walk in.

- **Walk round behind the tree house's trunk**: at the tree house, your friend can now walk back as far as the meadow goes, to just in front of the far row of trees, and 🔬's blue back line is there. Before, it stopped them just behind the trunk's foot, with no room to get round behind it; a walk you click on the far side now goes round behind the trunk when that is shorter.

## 0.0.264 (2026-10-18)

Everything in 0.0.263, with flowers you can stand among.

- **Flowers and grass in front of you or behind you, as they should be**: in the tree house's meadow, the clearing, at the castle, in the fairy forest, at the grass hut and in the forest, a flower or a tuft nearer than your feet is drawn in front of you, and one farther back behind you. Before, they were all flat on the ground, behind you. They are in rows by depth ("flowers 1", "flowers 2", ... in 🔬 Inspection, each with its 👁️ chip), a dozen or so a place.

## 0.0.263 (2026-10-18)

Everything in 0.0.262, with a choice for a click while walking.

- **👆 a click while walking to one**: with 👆 walk where you click on, a new click on the ground while your friend is walking now **walks there instead** by default: they stop where they are and set off for the new spot. Or choose **walks there next, after**, the queue of clicks from before, good for planning a long walk. The choice is in ♿ Accessibility, beside 👆, and in 🎮 Controls too.

## 0.0.262 (2026-10-18)

Everything in 0.0.261, with up to six thrones in each king's room.

- **👑 Thrones**: each castle's king's room now has a king's throne and a queen's to start with, one in the castle's theme A and one in its theme B (the velvet in that theme's carpet color, a crest in its flag color), and up to six across the dais (🏞️ Background, in the castle's settings: "👑 how many thrones"). Each throne has its own size and theme, and each is a seat you can sit on.
- **🐈 A cat on a throne**: any throne can have a cat, sitting, curled up asleep or pacing along the seat, with all the cat room's own settings (fur, tuxedo white, eyes, size, and its tail wag, with an LFO or its own).

## 0.0.261 (2026-10-18)

Everything in 0.0.260, with leaky diving suits and a riveted helmet.

- **🫧 leaks**, a new slider for the diving suits and gear (0, the default, to 100): bubbles stream out of where the suit leaks, more and bigger the higher it goes, rising and wobbling like the bubble machine's: the old helmet's air fitting and its side valve, and its collar; a scuba suit's shoulders and knees, its tank's valve and mouthpiece, a snorkel's tip.
- **The old diving helmet in more detail**: a riveted seam over its crown, a raised neck ring studded with rivets, rivets round its side ports and bolts round its glass; on its collar, a seam set in from the edge, rivets all along it, and a wing nut on each shoulder.

## 0.0.260 (2026-10-18)

Everything in 0.0.259, with costumes for the ocean and the deep sea (🐯 Costume).

- **🤿 scuba suit**: a sleek suit in its color with a bright yoke over the shoulders, a stripe down each leg and fins on the feet in the stripes' color; a zip down the back.
- **⚓ old diving suit**: a round brass helmet (or copper, or steel) over the whole head, your face showing through its bolted glass front, small side ports, a brass collar on the shoulders, a canvas suit and heavy boots, and an air hose from the top of the helmet up to the top of the place, swaying slowly.
- **Diving gear, with any costume**: 🤿 a snorkel, 🥽 a face mask and 🛢️ an air tank on your back (with straps, and a hose to a mouthpiece when there is no snorkel), each with its own color. Not under the old helmet.

## 0.0.259 (2026-10-18)

Everything in 0.0.258, with swimming toward you and away from you.

- **Swimming away from you**: seen from behind and a little above, as crawling away is: the back at the surface, the legs stretched toward you under the water, kicking, the soles showing, and each arm in turn coming up out of the water over the shoulder and reaching on ahead.
- **Swimming toward you**: the head at the surface with the water at the chin, each arm in turn coming up over the water and reaching toward you, then pulling under it.

## 0.0.258 (2026-10-18)

Everything in 0.0.257, with real swimming, from the side.

- **Swimming for real**: moving through deep water (and faster when you run), the friend swims flat along the surface instead of treading upright: head up and looking ahead, the arms windmilling in turn (up out of the water, over, into the water ahead of the chin, and back), the legs stretched out behind, kicking. Holding still in the water, they tread, upright, as before. Toward you and away from you are next.

## 0.0.257 (2026-10-18)

Everything in 0.0.256, with crawling hands on the floor, knees, and collision boxes that crawl too.

- **Hands on the floor, not on the hips**: crawling away from you, each arm goes straight down beside the body to a hand flat on the floor, low beside the legs; toward you, the arms reach down beside the face to hands flat on the floor.
- **Crawling toward you, less body and knees instead of legs**: the face, the shoulders and the arms, a little of the back behind the head, and the knees on the floor between the arms, just above the hands (in pants, a dress or fur; bare in shorts). The raised seat and the legs are gone.
- **Your collision boxes crawl with you** (🔬 Inspection, 🧍 you): from the side, a box under the hands, one under the knees and feet, and the body's box lying along the floor between them, as low as you crawl; toward or away, the body's box runs back into the room behind your hands or feet. Doors still decide whether you crawl by your height standing.

## 0.0.256 (2026-10-18)

Everything in 0.0.255, with crawling toward you and away from you drawn on the floor.

- **Crawling away from you**: seen from behind and a little above, like a baby crawling off across a carpet: the back seen short, the seat and the soles of the feet nearest you, the hands on the floor beside the body, and the head beyond. The back is in front of long hair now, and the friend crawls on the floor, not floating over their shadow.
- **Crawling toward you**: the face low and nearest, the arms straight down to the hands on the floor, and the back rising behind the head to a raised seat (a costume's tail goes up from it). Before, both looked like crawling up or down a wall.

## 0.0.255 (2026-10-18)

Everything in 0.0.254, with 🔬 Inspection's collision boxes in kinds, and placed things in your way.

- **🧱 collision boxes, a chip for each kind**: 🪨 things in the way, 🧱 a room's side walls, ↔️ the back line, 🕳️ pits (you fall in), 🌉 bridges (the way along) and 🧍 you; switch off what you don't want to see.
- **🧩 objects say what their boxes are for**: sorting the place's things front to back, separate from the collision boxes.
- **Placed things are in your way**: a tree you put down stops you at its trunk (slim, in bands up it, as the places' own trees), and so do placed benches, barrels, skeletons, anchors, bubble makers, levers and coral now, as rocks and bushes always did. "🧱 placed things are in your way" in 🏞️ Background turns it off.

## 0.0.254 (2026-10-18)

Everything in 0.0.253, with the water shown in 🔬 Inspection.

- **💧 water (where you wade or swim)**, a new toggle in 🔬 Inspection: the water in a place drawn over it, with what it means: wading to the knees in the tree house's stream and the cemetery's brook, in to the waist (or swimming where it is deep) in the moat, the lake, the sea and the falls' pool. It is not a collision box, it is a rule of its own, so it has a toggle of its own.
- Also out since 0.0.252, and not noted then: more outdoor things you can see, hide and place: the forest's sunbeams and flowers; the flowers in the clearing, at the tree house and at the grass hut; the castle's birds and its flowers and grass; the fairy forest's flowers and grass; the grass hut's butterflies.

## 0.0.253 (2026-10-18)

Everything in 0.0.252, with the trap room's levers working by touch.

- **Fixed: the trap room's red and blue levers did nothing to a touch** (on an iPad, for M). A tap on one now pulls it, as a click always has.

## 0.0.252 (2026-10-18)

Everything in 0.0.251, with the trap room more magical and less glitchy.

- **Lifting a boulder is magic**: when a blue lever lifts its boulder, stars shoot out of the boulder as it rises and out of the lever's knob, spinning and fading (drawn like the night sky's stars, with the same ⭐ settings, shown in the trap room now too). Placed boulder levers sparkle the same way.
- **Open a trap door under yourself and you fall in**: pulling a red lever while you stand on its pit drops you in, once its lid has swung open, just as walking onto an open pit does. So does lifting a boulder out of an open pit you stand on.
- **Fixed: a boulder or a trap door's lid showing for a moment in its old place after it moved.** On a computer the game has found slow, a picture of the room taken before or during the move could come back for a frame afterwards.

## 0.0.251 (2026-10-18)

Everything in 0.0.250, plus N's rock-falling surprise.

- **Hit by a falling boulder, you burst into rainbow bubbles**: stand on a pit in the trap room (or on a placed trap door) when its boulder falls, and you turn into a stack of bubbles in every colour of the rainbow, red at your head down to violet at your feet. They spill out every way, float, and pop one by one. Then you are back where you stood, fading in, and you carry on. No harm done, just bubbles.

## 0.0.250 (2026-10-18)

Everything in 0.0.249, with some of the outdoors' things as things of their own.

- **Outdoors, more things to see, hide and place**: the open water's island, sailboat and sun glitter; the beach's two striped blankets; the witch's woods' bats; the yard's frisbee and rope; the lake's lily pads.

## 0.0.249 (2026-10-18)

Everything in 0.0.248, with three more places made of things of their own.

- **Inside a tent**: its two mesh windows, and each tent's own things (the orange tent's two sleeping bags and flashlight, the blue tent's blanket, teddy bear and pile of books, the green tent's map, binoculars and backpack).
- **The cave behind the falls**: its gems, each of its three clusters of glowing crystals, the light coming in from the falls, the wet floor and the pebbles.
- **Our imagination**: its glow and the map board, with its columns' names and dotted lines (its pictures stay ways to each place).
- As everywhere now: 🔬 Inspection names them, 👁️ hides them, and 📦 can place them in any other place.

## 0.0.248 (2026-10-18)

Everything in 0.0.247, with the grass hut's rooms and the herb bundles as things of their own.

- **The grass hut's rooms are made of things you can see, hide and place**: the living room's rug, window, wind chime and lantern; the center's daylight, two lanterns and smoke; the bedroom's rug, window and dreamcatcher; and the potion room's three shelves of potions, the beam and its herbs. 🔬 Inspection names them, 👁️ hides them, and 📦 can place them anywhere.
- **The bedroom's bed and stump are in your way now**, as furniture is everywhere else.
- **The herbs drying from the beam are each a thing of their own, "drying herbs 1", 2, 3...**: in the witch's hut, the healer's hut and the grass hut's potion room (they were one long thing, named only at its far end).

## 0.0.247 (2026-10-18)

Everything in 0.0.246, with each square of the world a place of its own.

- **Each square of the world can have settings of its own**: in 🏞️ Background, in a square of the world, "✏️ changes here go to" chooses "🧩 the template", every square of that place (as it has always been, and where it starts), or "📍 only this square". Set a lake's ducks with 📍 and only that lake changes; the other lakes keep the template's. A castle's rooms are its own castle's.
- **↺ this square back to the template** takes a square's own settings away, so it follows the template again. Changing the template from a square that has a setting of its own makes that square follow the template for it again.
- A square taken away and put back keeps its own settings too.

## 0.0.246 (2026-10-18)

Everything in 0.0.245, with the world designer and the places more alike.

- **Every place can go into the world**, the abstract ones too: our imagination, the empty room, bubble space and the bubble room are now in 🗺️ World design's places. The places there are sorted by kind, as 🏞️ Background sorts them, each kind on a line of its own with its name.
- **A square taken away is not lost**: dropped on 🗑️, it waits in a new list, "🫥 unreachable in this world design", just as it was: its place, its name, its ways out, the things put in it, a forest crossing's paths. Drag it back onto any square (whatever is there takes its turn in the list), or tap it to put it back where it was (or in the first empty square). Drop one from the list on 🗑️ to forget it for good.

## 0.0.245 (2026-10-18)

Everything in 0.0.244, with more bubble makers.

- **🫧 Bubble makers in three shapes** ("🫧 its shape"): ⭕ round, as before; 📦 a box with its wheel on the front, on short feet; and 🗼 a tall tower with two wheels, one over the other, blowing bubbles from each in turn.

## 0.0.244 (2026-10-18)

Everything in 0.0.243, plus a new tune.

- **🌊 turtle deep-dub**, among the 🌴 reggae tunes: turtle steppers dub as Matthew set it, at 112 beats a minute and 13 half steps down, the bass three octaves back up, the sonar's ping and the chop one, and both melodicas down low, with his mixing board as its own mix to start from.

## 0.0.243 (2026-10-18)

Everything in 0.0.242, with the tree house's trunk boxed closer at its curvy base.

- **Walk closer round the foot of the tree house's trunk**: its curvy base now has five collision boxes of its own (nine up the whole trunk, was five), each as wide as the trunk is at that height. Standing a little behind its foot, you are measured against the trunk where it looks level with your feet, not against its roots spreading out at the front, so the space beside it is yours to walk.

## 0.0.242 (2026-10-18)

Everything in 0.0.241, with the bubble room in the speed test.

- **"🧪 this computer's speed" (in ⚙️ Advanced) now tries the bubble room too**: six more passes, twenty in all, about a minute. The bubble room standing still, walking through it (popping the bubbles stuck out into it), with its foam still, and with 1, 4 and 16 bubble makers. Each pass reports frames a second, the longest pause and the share of late frames, in the line to copy, so different computers can be compared.

## 0.0.241 (2026-10-18)

Everything in 0.0.240, with the bubble room's bubbles popping.

- **Walk into the bubbles stuck out into the bubble room and they pop**: the ones your body is in, at your depth, swell and vanish, and grow back nine to fifteen seconds later.
- **🫧 bubbles that pop (circles)**, in 🔬 Inspection: each of those bubbles drawn as a circle, in a colour of your choosing, so you can see what you will pop.

## 0.0.240 (2026-10-18)

Everything in 0.0.239, with the bubble room much lighter to draw.

- **The bubble room is far less work for the computer**: the foam that stays still is drawn once as a picture for each wall, the floor and the ceiling (and for each row of stuck-out bubbles), and only the bubbles that move are drawn again as they move. The room went from about 7,600 shapes on the page to about 900, and looks the same.
- **Bubble makers no longer make the browser measure the whole picture every frame** to see whether their bubbles touch you: where you are comes from where you are drawn.

## 0.0.239 (2026-10-18)

Everything in 0.0.238, with the bubble room alive and its own music.

- **The bubble room's foam is alive**: some of its big bubbles breathe in and out, each in its own time (with the scene's slow LFO), a few pop and grow back now and then, and the stuck-out bubbles bob and swell. Only those bubbles are drawn again as they move, not the whole room, so it stays quick.
- **New bubble room settings to start from**, Matthew's own: foam high up the walls and far out on the floor and the ceiling, lots of bubbles stuck out into the room, and the foam moving a good deal.
- **🫧 bubble super-dub step, the bubble room's own tune**, among the 🌴 reggae tunes: the 🔥 turtle super-dub step's groove, slower (116 beats a minute instead of 153), with new chords and new tunes.

## 0.0.238 (2026-10-18)

Everything in 0.0.237, with the bubble room deeper and livelier, and bubble makers.

- **Bubbles stuck out into the bubble room**: round bubbles stuck to the foam, sitting on the floor (with a soft shadow), standing out from the walls and hanging from the ceiling, now and then a smaller one stuck on a bigger one. The nearer ones pass in front of you and the farther ones behind. "🔵 bubbles stuck out into the room" sets how many come out of each wall, the floor and the ceiling.
- **The foam moves from the start**: its bubbles drift and the stuck-out ones bob, a little. "〰️ how much the foam moves" still sets how much, down to still.
- **🫧 Bubble makers to place**, in a new "🫧 bubbles" group of things to put in a place: a round toy bubble machine with a wheel of bubble rings turning on its front, a handle on top and a base. It blows bubbles out of its wheel wherever you put it, in any place: "🫧 bubbles a second", "🫧 its bubbles" (your bubble themes A, B or C), and its colour. The bubbles drift and float up, and pop when they touch you.

## 0.0.237 (2026-10-18)

Everything in 0.0.236, plus a new kind of place, for N: 🫧 bubbles.

- **🫧 bubbles, a kind of place of its own**, in 🏞️ Background: 🫧 bubble space has moved there from 💭 imagination, and beside it is a new place, the bubble room.
- **🫧 The bubble room, made of bubbles**: its back wall, its side walls, its floor and its ceiling are covered in bubble foam, a few big bubbles and lots of smaller and smaller ones packed in between them, as real foam is, all drawn in the room's perspective.
- **Its own controls, in "🫧 the bubble room"**: foam on or off on each wall, the floor and the ceiling, and how high it goes up each wall (or how far out on the floor and the ceiling); the foam's colour; a rainbow sheen; how much the foam moves (still to start with, since moving foam is a lot to draw); and 🎲 another foam.
- Each surface's foam is an object of its own, so 🔬 Inspection names it and 👁️ can hide it.

## 0.0.236 (2026-10-18)

Everything in 0.0.235, with the tree houses' doors opening as you go in.

- **A tree house's door swings open as you come to it**, outside, from the top of its ladder or the end of a bridge, the way the castle's doors swing open: then you go in through the open doorway.

## 0.0.235 (2026-10-18)

Everything in 0.0.234, with going out through a tree room's door to a bridge seen in the doorway.

- **Out through a tree room's door to a bridge, you shrink as you crawl through**, down to the size of the little bridge and house seen through the doorway, so you match the picture out there. Then you stand up, turn away from us, and walk a little way along that bridge, smaller as you go, seen only inside the doorway. Then you are outside, on the bridge itself, to walk the rest of the way.

## 0.0.234 (2026-10-18)

Everything in 0.0.233, with the tree house's doors to its bridges walked through properly.

- **Crawling out through a small door to a bridge, you are cut off at the doorway** from the moment you start to crawl, as the trap doors and ladders cut you off, instead of being drawn whole over the wall beside it.
- **At the end of a bridge, outside, you walk in through the house's door**: off the bridge onto the house's porch, along it to the door, and back through the door, seen only inside the doorway, as at the top of a ladder. Then you are in its room, beside the door to the bridge you came along.
- **On a bridge you grow or shrink toward the size of the house at its other end**: the higher houses are drawn smaller, so on a bridge up to one you get smaller as you go, and you fit its door.

## 0.0.233 (2026-10-18)

Everything in 0.0.232, with going into the tree house fixed.

- **Fixed: going into a tree house (or anywhere that leaves a way back) could stop halfway**, the friend stuck climbing in. Since 0.0.213 the "⬅️ back" button looked up the place's name in the wrong row of 🏞️ Background, failed, and the trip stopped. It reads the places' own names now.

## 0.0.232 (2026-10-18)

Everything in 0.0.231, with the levers, boulders and trap doors easier to see moving, and a speed for each.

- **Slower, so you can see them**: a lever's handle now takes about a second and a half to swing over and rock still, a boulder about a second and a third to fall from the ceiling and bounce into its pit, and a trap door's lid most of a second to swing.
- **A speed for each, in the trap room's box**: "🎚️ how fast the levers move", "🪨 how fast the boulders fall" and "🕳️ how fast the trap doors swing", from a quarter of the speed to three times as fast.
- **Fixed: pulling the right boulder lever changed the left pit's rock.** Each pit's boulder now keeps its own rock until it falls again.

## 0.0.231 (2026-10-18)

Everything in 0.0.230, plus boulder levers, for N.

- **🪨 Boulder levers in the trap room**: a second pair of levers under the first, with blue knobs. Pull one and a boulder falls from the ceiling into the pit it works (faster as it falls, with a little bounce), and the hole is covered: no way down, and the boulder stands in your way. Pull it again and the boulder rises away. They work the pits the same way the red levers do ("🎚️ the levers work"). A trap room's Background box says whether it has them ("🪨 boulder levers") and which rock its boulders are ("🪨 the boulders' rock": one of the rock colours, or 🎲 a surprise, a new rock each time one falls).
- **Every 🎚️ lever you place chooses what it does** ("🎚️ it"): opens and shuts its trap doors, or drops a boulder into them; and its boulder's rock, and the colour of its knob ("🎨 its knob": red for trap doors and blue for boulders unless you choose another).

## 0.0.230 (2026-10-18)

Everything in 0.0.229, with the trap doors' swing and the levers' rock that carry on through a redrawing.

- **Fixed: no swing at all, in Firefox with music playing.** A lid swinging or a handle rocking was drawn on the room as it was when the lever was pulled; when the room was drawn again in the middle of it (in Firefox the music's clock can stand still while the picture's runs, and the game measures the gap between them again), the new drawing showed the end at once. Each swing now finds its lid and handle again in every drawing, and carries on.

## 0.0.229 (2026-10-18)

Everything in 0.0.228, plus levers that glow when you point at them.

- **A lever glows under the pointer**, warm, as a torch's light: the trap room's levers and every 🎚️ lever you place. Point at one to see it is something to pull.

## 0.0.228 (2026-10-18)

Everything in 0.0.227, with the trap doors' swing made sure to play.

- **Fixed: a trap door's lid could change at once instead of swinging**, and a lever's handle could snap instead of rocking: each started its clock when the lever was pulled, before the room was drawn again, so in a room slow to draw (the trap room, since its moss) the swing could be over before it was ever seen. Each now starts when it is first drawn.

## 0.0.227 (2026-10-18)

Everything in 0.0.226, plus lichen, for M, to decorate rocks, castle walls and logs.

- **🪨 Lichen to place**, in 🏞️ Background's things, 🌲 nature: five kinds after the forms real lichens take, all drawn shapes: 🟠 crust (a cracked orange crust of little tiles, with darker dots), 🍃 leafy rosette (lobed grey-green leaves round its middle, paler at their edges), 🏆 pixie cups (little trumpets on stalks), 💂 soldiers (grey-green stalks with bright red caps, some forked), 🧔 beard (pale strands hanging and branching, from where it is put: on a log, a rock's edge, a wall).
- **"📍 on"**, as the moss's: the back wall (a flat lichen face on, as on a rock or a castle wall), the floor (lying flat), a left or right wall (flat lichen seen along the wall; standing lichen turned to grow out from it). A beard always hangs.
- **"🎨"**: its kind's own colour (∅), or orange, yellow, grey green, pale green, bone, blue grey or red.

## 0.0.226 (2026-10-18)

Everything in 0.0.225, with moss: pads with wavy fingers, in five kinds, on the walls and to place.

- **🌱 Moss to place**, in 🏞️ Background's things, 🌲 nature: five kinds after real mosses, each its own shape, size and colours: 🟢 cushion (round pale domes, short and close), ⭐ haircap (tall spiky stems), 🟫 carpet (a low flat golden mat), 🪶 feather (fronds with a leaflet either side), 🌬️ windswept (tufts all leaning one way). Its pads are drawn shapes with ragged edges, a darker base and a lighter top; its fingers rise from them, swaying as the witch's cauldron's simple fumes do, lighter at their tips.
- **"📍 on"**: the floor, the back wall, a left wall or a right wall. On a side wall its pads hug the wall and its fingers reach out into the room; on the back wall each pad is a whole patch stuck to the wall.
- **"🟠 lobes at the tips"**: some of its fingers carry a little capsule at the top, as real mosses carry their spores (each kind a different share and size).
- **"🎨"**: its kind's own colour (∅), or one of seven greens.
- **The castle's trap room and dungeons have this moss** instead of the dark green smears that looked like slime: clumps along the foot of the back wall, a few on the back wall, and on each side wall near its foot.

## 0.0.225 (2026-10-18)

Everything in 0.0.224, with three tree house rooms' wall things moved clear of the tree's trunk, and made objects.

- **Clear of the trunk, laid out neatly**, and in 🔬 Inspection's list of objects with a 👁️ chip each, placeable from 🏞️ Background's **📦 from the places**; the friend is always in front of them:
  - 🍳 The kitchen's jars: on shelves on the wall between the stove and the trunk, five to a shelf, one shelf under another (more shelves when there are more jars). They were behind the trunk, drawn over it.
  - 🔭 The lookout's treasure map: left of the trunk, and above the floor (its bottom edge dipped below it). Its pennants: their cord is tied to the trunk now, instead of running behind it.
  - 🎵 The music room's records: four to a row, between the ladder and the trunk.

## 0.0.224 (2026-10-18)

Everything in 0.0.223, plus wall and floor things as objects in the living room, the witch's and the healer's huts, and the tree house rooms.

- **What hangs on the walls and lies on the floor is in 🔬 Inspection's list of objects** in more places, each with its 👁️ chip to hide it and placeable anywhere from 🏞️ Background's **📦 from the places**; the friend is always in front of them.
  - 🛋️ The living room: the rug, the wall clock, and the window's frame with its curtains (its view is still the way through it).
  - 🧙 Inside the witch's hut: the round window, the window either side of the door, the herbs on the beam, two cobwebs, the shelf of potions, the frogs in jars, and each crow in its cage.
  - 🌿 Inside the healer's hut: the rug, the round window and its sunbeam, the herbs on the beam, the fireplace, and the candles, jars and heart on its mantel.
  - 🪜 The tree house rooms: the tree's limbs coming out through the walls, the books room's lantern, and the art room's paint spills (and its pinned drawings, when there are more than ten things).
- In earlier notes, "♿'s list of objects" meant 🔬 Inspection's: it is written so now.

## 0.0.223 (2026-10-18)

Everything in 0.0.222, plus the last two castle rooms whose wall and floor things are objects of their own: now every room of the castle lists them.

- **What hangs on the walls and lies on the floor is in 🔬 Inspection's list of objects** in two more of the castle's rooms, each with its 👁️ chip to hide it and placeable anywhere from 🏞️ Background's **📦 from the places**; the friend is always in front of them.
  - 🐈 The cat room: its rug, its fireplace, the mantel with its candles and clock, the royal cat's portrait, the window with its curtains, the feather toy, the banner, the six shelves up the side walls and the mat for the bowls.
  - 🐴 The stable: its round window, each of its four stalls, the hay loft with its bales, the lantern on the middle post, the horseshoes, the saddles, the bridle and the straw on the floor.

## 0.0.222 (2026-10-18)

Everything in 0.0.221, plus pits and trap doors that swing open and shut, and levers that rock.

- **A pit or a trap door opens and shuts as you watch.** Pull its lever and its lid of planks swings down into the pit on its far edge, faster as it falls, darker as it goes down; pull it again and the lid swings back up across the opening. The trap room's pits and the 🕳️ trap doors you place both do it.
- **The trap room's pits start shut.** Both levers start pulled down, so each pit has its lid across it until you pull its lever up. A lever you had already set stays as it was.
- **A lever rocks when it is pulled**: its handle swings across, past where it stops, and rocks back and forth until it settles.
- **Fixed: the controller on the screen in the full-screen designers.** In the full-screen 🏞️ Background designer and 🗺️ World design, where the game is shown beside the boxes, the controller on the screen was hidden, so its arrows could not move your friend. It shows there now (but not in the full-screen 🎵 music, where there is no friend to move).

## 0.0.221 (2026-10-18)

Everything in 0.0.220, plus seven more castle rooms whose wall and floor things are objects of their own.

- **What hangs on the walls and lies on the floor, in seven more of the castle's rooms, is in 🔬 Inspection's list of objects,** each with its 👁️ chip to hide it, and each can be placed anywhere from 🏞️ Background's **📦 from the places**. The friend is always in front of them.
  - 🕯️ The hallway: its carpet, its three banners and its five torches.
  - 🍽️ The dining room: its great fireplace.
  - 🍲 The kitchen: its hearth, the strings of onions, garlic and herbs, the shelf of plates and the shelf of crocks.
  - ⚒️ The smithy: the tools on their pegs, the rack of swords and the shield. In the smithy, the forge's glow now falls on the tools beside it.
  - 🌀 The spiral staircase: its three arrow slits, each with its beam of light.
  - 👑 The king's room: the stained-glass window, two banners, two crossed swords with their shields, two torches and the carpet.
  - 🌿 The greenhouse: the moss, the fallen petals, the nine vines up its posts and the nine hanging from its roof.

## 0.0.220 (2026-10-18)

Everything in 0.0.219, and the game can tell you about it again.

- **The game offers the newest version again.** 0.0.219 went out with an empty version file, so a game left open on an older version never said a new one was there. This version puts it back.

## 0.0.219 (2026-10-18)

Everything in 0.0.218, with the orange tent's flashlight shining the right way.

- **The flashlight's beam comes out of its lens**, the yellow end, along its shaft, up and to the right. (It came out of the bottom, toward the door.)
- **Where the beam meets the tent's wall, an oval of light** on the canvas, narrow across and tall as the wall goes away from us, brightest in the middle; brighter at night.

## 0.0.218 (2026-10-18)

Everything in 0.0.217, with the trap room's two pits and two levers set up in the background settings.

- **🕳️ the left pit and 🕳️ the right pit**, each on or off in the trap room's settings: a room with one pit, or none.
- **Two levers, left and right**, on the back wall: pull one and the pit it works shuts, a wooden hatch over it, and you walk over it; pull it back and the pit opens again.
- **"🎚️ the levers work"**: ↕️ normal, each lever its own side's pit; 🔀 reversed, each lever the other side's; or 🎲 random, new each time you come into the room, so one lever may work both pits and the other nothing.

## 0.0.217 (2026-10-18)

Everything in 0.0.216, with what hangs on the walls and lies on the floor as objects too, beginning in the castle's trap room and dungeons.

- **Wall and floor things are objects**, a kind of their own: listed in "👁️ what stands in the scene" (and 🔬 Inspection's), hidden and shown with a tap, and put in other places with 📦, but always behind the friend and in nobody's way.
- **🕳️ the trap room:** the two torches, the lever, the "⚠️ MIND THE FLOOR ⚠️" sign, the cobwebs and the moss on the walls, the sword on the floor (it had 2 objects; now 10).
- **🗝️ the dungeon and ⛓️ the deep dungeon:** the barred window with its moonbeam, the torch, the chains hanging (with a skeleton in them when "💀 skeletons" asks for one), the shackles along the walls, the puddle and its drip, the straw and the bones on the floor, the cobwebs and the moss (the dungeon had 3 objects; now 14).

## 0.0.216 (2026-10-18)

Everything in 0.0.215, with levers and trap doors that work together as soon as they are put in a place, for M.

- **A lever and a trap door join each other** when they are put in the same place, whichever comes first: a new lever opens and shuts every trap door already there, and a new trap door joins every lever there.
- **The lever's chips say whether each trap door is joined:** "🔗 🕳️ 4" when it opens and shuts with this lever, "➕ 🕳️ 4" when it does not; a tap joins it or unjoins it. (A trap door not joined looked much the same as one joined.)

## 0.0.215 (2026-10-18)

Everything in 0.0.214, with a pop-up when a new version is out, for M, for those who want it.

- **"🔔 a pop-up when a new version is out"**, in ⚙️ Advanced, off at first: switched on, the moment a newer version comes out a pop-up asks "A new version is available, would you like to switch?". "🔄 switch" changes to it, keeping your friend, the place and the settings, as the 🆕 chip after the game's name always has; "not now" leaves the chip, and the pop-up does not ask again about that version. Each browser keeps its own choice, as it keeps light or dark.

## 0.0.214 (2026-10-18)

Everything in 0.0.213, with the background designer's two sides sized by dragging, as the world designer's are.

- **The bar between the boxes and the game**, in ⛶ the background designer, full screen: drag it for more room for the boxes or for the game. This browser keeps where it was left, apart from the world designer's.

## 0.0.213 (2026-10-18)

Everything in 0.0.212, with 🏞️ Background as a designer, full screen, as 🗺️ World design is.

- **⛶ the background designer, full screen**, the first row of 🏞️ Background: the place's settings as boxes in a grid on the left, the game itself as the first card on the right, live, held in place while the boxes scroll (its 👁️ folds it away, as the world designer's does), and ✕, Escape or leaving the browser's full screen goes back.
- **The places as pictures:** in the full screen, each place to choose shows what it looks like, from the picture archive.
- **What stands in the place, as pictures:** each thing in "👁️ what stands in the scene" shows its picture, and lights up in the game while you point at it; a tap still hides it or shows it again.
- Outside the full screen, 🏞️ Background is as it was.

## 0.0.212 (2026-10-18)

Everything in 0.0.211, looking the same: the 🎵 Music and 🗺️ World design full screens are now built from pieces every tab will share, a box for each thing, a row on one line, and the full screen itself, the first step of laying out every panel as those two are (^du_report(5)).

## 0.0.211 (2026-10-18)

Everything in 0.0.210, with the three things the Dress Up report on the designers (^du_report(5)) found on the way.

- **Fixed: the house's "🧸 toys on the lawn" never showed.** It shared its setting with the tree house rooms' "🧸 things in the room", and only the rooms' rule counted: at the house there was no slider, and inside the tree house there were two. Now each shows in its own place.
- **Fixed: the bubble themes' on and off looked the same.** "🅱️ theme B" and "©️ theme C" (and the three themes of "🫧 blowing bubbles") light up while they are on, as the ♿ chips do.
- The bubbles' colors say "color", as the rest of the game does.

## 0.0.210 (2026-10-18)

Everything in 0.0.209, with the nav bar that Matthew's pages share.

- **» at the bottom left** opens the pages' nav bar: one outline of matthewhammer.org, fumola.org and adapton.org, with this game in it under Messed Up Dress Up, and at its top the choice of ◐ auto, ☀ light or ☾ dark for the pages around the game. Here it starts hidden each visit, so the picture keeps the whole window, and the game keeps its own colours; « hides it again. The Dress Up reports have the bar too, shown.

## 0.0.209 (2026-10-18)

Everything in 0.0.208, with levers to place, and trap doors they open and shut.

- **🎚️ switches and traps**, a new group of things in 🏞️ Background: a **🎚️ lever** on a little stone post (the trap room's own lever on its front), and a **🕳️ trap door** in the floor. Tap a lever in the picture and it is pulled: its handle swings across, and every trap door it is wired to opens, or shuts again. Wire it in the lever's row of the things list, "🔗 it opens and shuts": a chip for each trap door in the place, by its number. Drag a lever to move it, as before; only a tap pulls it.
- **🕳️ a trap door**: shut, planks over its opening in a ring of broken stone, and the friend walks across it; open, the trap room's pit, and the friend's feet keep out of it. Its own "🕳️ open", its opening's shape (■ square, ● circle, ▲ triangle, as the castle's pits), and "🔱 spikes inside".

## 0.0.208 (2026-10-18)

Everything in 0.0.207, with any object of any place to put in any place.

- **📦 from the places**, a new group of things in 🏞️ Background: choose a place from the list, and every object standing there has a button of its own: the kitchen's oven, the smithy's anvil, the king's throne, the beach's umbrellas, the drum kit in the tree house's music room, and so on, from 49 places. Tap one, or drag it into the picture: it stands at its new spot, sized for how far back it is, with "🔎 size" and "↔️ mirror" like every other thing, and it stops the friend's feet as it does at home. Ways out, doors and ladders are not offered, and a doorway drawn on a thing leads nowhere. A few things only look right where they are drawn: the stable's horse is just its head over its stall door.

## 0.0.207 (2026-10-18)

Everything in 0.0.206, with the grass hut as a cluster of huts, and a hut center, for M.

- **🛖 grass hut**: "🛖 huts in the cluster", 1 to 6: two stand side by side; three make a triad round the middle; four to six, a ring, each the same distance from the middle and from its neighbors. "🔥 the hut center" puts a bigger hut in the middle; its door shows between the front two when the number is even.
- **🔥 the grass hut's center** (a new place): with the hut center on, every door of the cluster leads into it. It is a round room with a door to the bedroom, the living room and the potion room, and a fire in a ring of stones whose smoke rises to the hole at the top, where daylight falls in. Logs either side of the fire to sit on; the way toward us leads out. From the living room, the way toward us leads into the center.

## 0.0.206 (2026-10-18)

Everything in 0.0.205, with the signpost's boards taking you where they point.

- **🧭 sign with directions**: tap a board and the friend goes that way, as with the arrow at the picture's edge. Dragging the signpost by a board still moves it.

## 0.0.205 (2026-10-18)

Everything in 0.0.204, with the grass hut round, outside and in, for M.

- **🛖 grass hut**: a round wall of sticks under a dome of grass thatch that hangs out over it all round, the thatch in rings of fringe up to the tuft tied at the top.
- **Inside the grass hut** (the living room, the bedroom, the potion room): round rooms. The wall of sticks curves round the floor under the dome's rafters; the twine, the potion room's shelves and its herbs' beam follow the curve, and the doors stand in it.

## 0.0.204 (2026-10-18)

Everything in 0.0.203, with a grass hut for M: four new places.

- **🛖 grass hut**: a round hut of upright sticks under a grass roof, its tuft tied off at the top, smoke rising, sticks piled for the fire, butterflies over the meadow. Its stick door goes inside.
- **🛋️ inside the grass hut**: a living room with the living room's couch and rug, in the hut's stick walls under its grass ceiling, a wind chime and a lantern, a window with its view. Its doors lead to the bedroom and the potion room; the way toward us goes back out.
- **🛏️ the grass hut's bedroom**: a bed of logs ("🛏️ blanket color"), a candle on a stump, a dreamcatcher, a round window onto the night sky.
- **🧪 the grass hut's potion room**: a cauldron bubbling over its fire ("🧪 the potion's color"), shelves of glowing bottles, herbs drying, a table with a mortar, a pestle and a book of recipes.
- **"🛋️ couch color" and "🟠 rug colors"** for both living rooms, the house's and the hut's, each its own.

## 0.0.203 (2026-10-18)

Everything in 0.0.202, with a saved friend's file saying when it was saved, apart from what draws the picture.

- **💾 Save's .json** now keeps when it was saved and the game's version under "extra": things for whoever saved it, which never change the picture. Loading a friend reads only the settings, as before, and files saved by earlier versions still load.

## 0.0.202 (2026-10-18)

Everything in 0.0.201, with every picture decided by its settings.

- **The same settings draw the same picture, every time** (for the picture archive, and for anything that draws a saved friend again): the animations' timing, the friend treading water, and where bubble space's bubbles start no longer depend on when the picture was drawn, or on how fast the computer is.
- **"🎲 other places for the bubbles"** in 🫧 bubble space: the bubbles start somewhere new, and a saved friend keeps where they were.

## 0.0.201 (2026-10-17)

Everything in 0.0.200, with every 🔬 overlay on in GitHub's tests, and what that found.

- **index.html#inspect**: the game opened so starts with every 🔬 overlay on (all the arrows, the planned path, the collision boxes, what each place is made of). GitHub's tests now open it so, every check, picture and frame-rate run: the overlays always tested, the pictures showing them. With them on, the frame-rate gate still passes everywhere; drawing takes about a quarter longer (a median of 28% more, under a millisecond a frame).
- **Fixed**: with 🧭 every arrow shown, pointing at an arrow lights it as the one to take; it stayed dim.
- **Fixed**: with 🧩 what it's made of shown, the guard against too many moving things stays shut until measured again, as it should; the overlay's redraw was reopening it.
- **A planned walk keeps a little more room** round what stands in the way: a foot could graze a corner on a slow computer.

## 0.0.200 (2026-10-17)

Everything in 0.0.199, with no arriving inside things.

- **Fixed**: arriving in a place, the friend no longer stands inside something: the healer's hut put them inside the tea table, and the tree house picked from the menu inside its trunk, where walking toward its middle could not get them out. Arriving inside anything, they are moved beside it, the nearest clear spot.

## 0.0.199 (2026-10-17)

Everything in 0.0.198, with the music no longer slowing the pictures where its sound cannot start.

- **Fixed**: where the music cannot start its sound (no sound device; Firefox on GitHub's computers), a tune counted as playing held its clock still, and the picture, timing its animations by it, was drawn again from scratch four times a second: Firefox lost about 10 frames a second after the first click or tap. The pictures now keep the page's own time while the sound is stopped.

## 0.0.198 (2026-10-17)

Everything in 0.0.197, drawing less of what cannot be seen.

- **Fewer hidden drawings**: in each place, a tree, pine, leaf pad, bush's pad or flower more than 75% hidden behind what is drawn in front of it is left out: the far rows of trees in the forest, the castle and the fairy forest, the waterfall's far jungle, the tree house's leaves, its flowers behind the trunk (at the tree's size), the lake's far wood, each bush's and leafy tree's own pads. 675 fewer drawn across the places at their defaults (the waterfall 171, the busiest tree house 112, the clearing 103), each place's picture changing by under 2% of its pixels.

## 0.0.197 (2026-10-17)

Everything in 0.0.196, with a new tune.

- **🔥 turtle super-dub step**: a new tune in 🌴 reggae, after 🐢 turtle steppers dub: the same tune at 153 beats a minute, an octave down, with the bass, the sonar's bell and the first melodica back up where Matthew set them, and his mixing board (the echoes, the room, the melodica and the chop) to start from. Changing one thing on its board keeps the rest of his.

## 0.0.196 (2026-10-17)

Everything in 0.0.195, with each trap room's pits its own.

- **Each trap room's pits**: the shape of a trap room's pits (square, round or triangular) is that room's, set in its own 🚪 room box in 🏞️ Background, so two trap rooms in a castle can differ; a chip changes the room at once. A friend saved with the castle-wide setting keeps it in each room.
- **Fixed**: with 🔬 collision boxes on, opening the game no longer throws an error before it has finished loading.

## 0.0.195 (2026-10-17)

Everything in 0.0.194, with the way into a tree house from its ladder.

- **Up the ladder and in at the door**: climbing a tree house's ladder from the ground, at the top your friend steps onto the house's platform (the tree's size, as on the bridges), walks along it to the door, and goes in through it, out of sight past its frame, into the room.

## 0.0.194 (2026-10-17)

Everything in 0.0.193, with your friend their size up the spiral staircase.

- **Up the stairs, not far away**: climbing the spiral staircase, your friend keeps the size of where they stand on the steps (a step at the back smaller, one in front bigger), instead of shrinking the higher they go.

## 0.0.193 (2026-10-17)

Everything in 0.0.192, with the spiral staircase whole.

- **The spiral staircase, drawn all the way round**: the front of each turn above, the steps coming toward you, is there now, not cut away; a step in front of your friend turns see-through while it would hide them.
- **Up the middle of the steps**: walking to a door up the stairs, your friend goes round along the middle of the steps, not along the wall's edge.

## 0.0.192 (2026-10-17)

Everything in 0.0.191, with room to click round the trap room's pits.

- **The pits' glow**: a pit in the trap room glows in its own shape (its round, square or triangular opening), and its sign glows apart; pointing or clicking only on the hole or the sign takes it, so the floor round them is yours to click and walk about.

## 0.0.191 (2026-10-17)

Everything in 0.0.190, with clicks to walk queued up.

- **👆 A queue of clicks**: while your friend walks to where you clicked, the ground still glows, and each click there is another walk, taken one after another, as many as you like. With 👣 planned path on, the walks are drawn by turns in its colour and a second, alt colour (🔬 Inspection). Moving with the controller ends the queue.
- **Fixed**: in 🗺️ World design, the forest crossing's "🌄 off in the distance" chips now show the one you chose.

## 0.0.190 (2026-10-17)

Everything in 0.0.189, with doors you walk through, not fade through.

- **Through a door, not a fade**: a door in a side wall (the tree house rooms' doors to the bridges, the castle's side doors) opens as it glows and stays open while your friend goes through it, and they go out of sight past the doorway's edge, as down a trapdoor, instead of fading.
- **On a bridge, the house you click**: clicking the house across the bridge takes your friend there, even standing near the house they came from.
- **Down a ladder to the ground**: out of a tree house down its ladder, your friend comes out at the foot of that ladder, facing you.
- **The world**: squares A4 and B5 are no longer joined, as Matthew meant.
- **One big eyebrow** is off at first.

## 0.0.189 (2026-10-17)

Everything in 0.0.188, with the world's grid easier to read.

- **🗺️ World design's letters and numbers**: in the tab too, not only full screen, the columns' letters and the rows' numbers are round the grid, big enough to read, and an empty square's own name (F3) is as big.

## 0.0.188 (2026-10-17)

Everything in 0.0.187, with the spiral staircase's doors in reach, and walking where you click on from the start.

- **The spiral staircase**: the edges of where your friend can walk follow the stairs out to each door on its landings, so every door can be reached.
- **👆 Walk where you click is on at first**: new friends have it on, and a friend saved while it was off by default gets it on once; turn it off in ♿ Accessibility if you like, and it stays off.

## 0.0.187 (2026-10-17)

Everything in 0.0.186, with walking where you click.

- **👆 Walk where you click** (in ♿ Accessibility, with its own 🎨 colour): while it is on, the ground under the pointer glows where your friend can go, and a click there walks them to it, round anything in the way (turn on 👣 planned path in 🔬 Inspection to see the way they take). Clicking a door, a picture, your friend or a thing does what it always did.

## 0.0.186 (2026-10-17)

Everything in 0.0.185, with the 🎵 Music tab laid out as cards.

- **Music as cards**: in its usual view too, the Music tab's controls are cards side by side (two across in the side column), as in full screen; the tune picker and the mixing board go across.
- **The first row**: 🔊 sound and ▶ what's playing one over the other, ⏯️ where the tune is beside them, filling the rest of the row, in both views.

## 0.0.185 (2026-10-17)

Everything in 0.0.184, with 🔭 sizing what moves, the forest crossing's settings where you design the world, and two tabs for looking closer.

- **🔭 smaller going north**: moving it now resizes your friend and the things you put in a place, as you drag it; the place's own drawing stays as it is.
- **The forest crossing**: its "🌄 off in the distance" (tall trees, sky and clouds, or mountains) can be chosen in 🗺️ World design too, and in 🏞️ Background its paths are a compass, like the world designer's ways out, with nothing in the middle.
- **🔬 Inspection** (♿ Accessibility until now): all arrows, the planned path, collision boxes, and 🧩 what it's made of, which now has its own 🎨 colour, ✏️ line and 🏷️ names size like the others.
- **♿ Accessibility**: a tab of its own with the size of the words in the game, and all arrows (the same setting as in Inspection).

## 0.0.184 (2026-10-17)

Everything in 0.0.183, with a look at what each place is made of, and a new world.

- **♿ 🧩 What it's made of**: in ♿ Accessibility, turn it on to see every object in the place boxed and named, and its plants boxed and named by the drawing that made them (leafyTree, pineTree, bushSvg, fern, palm...), a tree's leaf pads inside the tree. Each kind has its own chip to show or hide it, with how many are in the place now; leaf pads and grass start hidden, as there are so many. A legend under the picture counts them.
- **A new world**: a new friend starts in Matthew's world: six squares across, seven down, the clearing in the middle, deep sea down the west side and open water along the top, with the things he put in its squares. A saved friend still in the old world, as it was, moves to the new one.

## 0.0.183 (2026-10-17)

Everything in 0.0.182, with new palm trees in the castle's greenhouse.

- **Greenhouse palms**: their trunks are now stacked segments, leaning and narrowing as they go up, each one of four shapes (a flared collar, a barrel, a notch where a leaf grew, a dome), with a root flare at the ground, instead of a pile of ovals.
- **Bumping into them**: each palm's trunk is eight boxes stacked up it, leaning as it leans (see them with ♿'s collision boxes), so your friend stops where the trunk really is.

## 0.0.182 (2026-10-17)

Everything in 0.0.181, with doors that open as you come to them.

- **Doors open when they glow**: in the tree house rooms and the castle, a door in a side wall swings open when your friend comes near it (when it glows), and swings shut again when they move away.
- **What's behind**: through a tree house room's open door, the house across the bridge; through a castle door, the light of the room beyond, dark for a dungeon, bright for the greenhouse, and in between.
- **Walk on through**: walking into the wall at an open door takes your friend through it.

## 0.0.181 (2026-10-17)

Everything in 0.0.180, with fewer ways to end up inside things.

- **No more stuck in a mushroom**: a high jump that something tall stops in the air no longer leaves your friend inside the small thing they came down on; they step out, back the way they came.
- **Walls of things stay walls**: with one foot in a toy chest, walking on no longer takes your friend into the tree trunk beyond it, and when a walk stops with a foot just inside something, they step back out of it.
- **Walks go round the whole trunk**: a walk to a door goes round the tree house's trunk as wide as it is above its foot, not only at the ground.

## 0.0.180 (2026-10-17)

Everything in 0.0.179, with a view of where each bridge door leads.

- **Through the bridge doors**: in a tree house room, as a door to a rope bridge swings open, you see the house it leads to, its animal and its colors as it looks outside, at the far end of the bridge, the bridge's planks and ropes running out to it. Change that house's animal and the view changes too.

## 0.0.179 (2026-10-17)

Everything in 0.0.178, with the tree house rooms' bridge doors leading out onto the bridges.

- **Out the door, across the bridge**: in a tree house room, the door to a rope bridge now takes your friend outside, onto that bridge, at their house's end. Walk them along it with ◀ and ▶ (at a small friend's pace); at the other end, keep pushing and they go into that house, just inside its door back to the bridge.
- On a bridge, ✨ X, Enter or the 🚪 button walks them to the end they face and into the house there, and a tap on either end's house does the same; the house they would go into glows.

## 0.0.178 (2026-10-17)

Everything in 0.0.177, with the tree house's rope bridges to walk on.

- **Walk the rope bridges**: at the tree house, drag your friend up until their feet are on one of the rope bridges, and they stand on it, small enough to fit the houses' doors, in front of the tree. With the controller, ◀ and ▶ walk them along the bridge, down its sag and up again, as far as its ends; ⬆ and ⬇ only turn them, away from you or toward you. Drag them down to the ground to get off.
- **♿ collision boxes**: each bridge's way along it is shown as a green path.

## 0.0.177 (2026-10-17)

Everything in 0.0.176, with every house's number on the sign's map in full view.

- **Each number on the map shows**: a lower house on the map no longer covers part of the number of the house above it (house 4's, under house 2).

## 0.0.176 (2026-10-17)

Everything in 0.0.175, with a map on each tree house room's sign.

- **Where am I in the tree?**: the sign in each tree house room, under its animal and number, now shows a small map of the whole tree house: the trunk, the houses, the rope bridges and the ladders, all in a few dark colors, with each house's number on it and a bright white circle round the house you are in. It follows the tree outside: the number of houses, bridges and ladders, the houses' sizes and animals.
- The pictures on the room's walls keep out from behind the sign.

## 0.0.175 (2026-10-17)

Everything in 0.0.174, with trapdoors and pits in three shapes.

- **Square, round or triangular**: each tree house's trapdoor can now have a "■ square", "● circle" or "▲ triangle" opening ("🕳️ its trapdoor's opening", in 🏞️ Background, in its room and the room below it), and so can each of the castle's trap room pits ("🕳️ the trap room's first pit: its opening", and the second). The hole, its lid, and the hole in the ceiling of the room below all take the same shape, and you fall into a pit by its real shape.

## 0.0.174 (2026-10-17)

Everything in 0.0.173, with climbs up through the ceiling you can see.

- **Climbing up, through the hole**: going up a ladder through the ceiling (in the tree house's rooms, and out of the castle's dungeons) your friend used to fade away near the top. Now they climb on up into the hole, hidden above its edge as they go, as they already do climbing down through a trapdoor.

## 0.0.173 (2026-10-17)

Everything in 0.0.172, with a tighter fit round the tree inside the tree house.

- **Fixed: the tree inside the tree house blocked too much**: in the tree house's rooms, the tree's trunk stopped your friend in a big box covering its whole corner, branches and all. Now its boxes follow the trunk up from the floor, so you can walk right up beside it; "🧱 collision boxes" shows them.

## 0.0.172 (2026-10-17)

Everything in 0.0.171, with pictures that hang clear of the ladders.

- **Pictures out of the way**: in the tree house's rooms, a picture could hang right behind a ladder, the trapdoor or the furniture, so a tap meant for the ladder went into the picture, or the other way round. Now each picture hangs where nothing stands in front of it, well away from the ladders and the trapdoor; if a room has no clear place left, that picture isn't hung.

## 0.0.171 (2026-10-17)

Everything in 0.0.170, with smooth music while you drag things.

- **Fixed: the music stuttered while dragging things**: dragging something you'd put in a place (a rock, a tree, a potion) redrew the whole picture at every move of the mouse, which on a slower computer made the music pause and glitch. Now the thing simply moves under your finger, and the picture catches up a few times a second and when you let go.

## 0.0.170 (2026-10-17)

Everything in 0.0.169, with a tree house trunk you bump into.

- **Fixed: walking through the tree house's trunk**: outside, at the tree house, your friend could walk right through the big braided trunk. Now it stops them, a walk goes round it, and "🧱 collision boxes" shows it, its boxes following the trunk up from the ground. The ladders to the houses are still there to climb.

## 0.0.169 (2026-10-17)

Everything in 0.0.168, with collision boxes that swing with the swing.

- **Fixed: the swing's box stayed still**: at the tree house, with "🧱 collision boxes" on, the swing's box stood still while the swing went back and forth, and so did your friend's boxes while they sat on it. Now they swing with it. And walking into the swing meets it where its seat is, not where it hangs at rest.

## 0.0.168 (2026-10-17)

Everything in 0.0.167, with crawling through doors that are too small.

- **Crawling through small doors**: when your friend is too tall for a door, like the tree house's little doors to its rope bridges, they walk up to it and crawl the last bit and through, instead of walking through as if it fit. A door they fit through, they still walk through. Make your friend very big, and they'll crawl through the castle's doors too.

## 0.0.167 (2026-10-17)

Everything in 0.0.166, with a climb down a trapdoor you can see.

- **Fixed: climbing down a trapdoor, your friend went behind it**: in the tree house's rooms, going down the ladder through the trapdoor drew your friend behind the ladder and the open lid, so you couldn't see them go down. Now they're in front, and you see them go down into the hole, hidden below its near edge as they go. Standing behind the hole, they're still behind its lid.

## 0.0.166 (2026-10-17)

Everything in 0.0.165, with tree house trapdoors you walk round.

- **Fixed: walking over the trapdoors**: in the tree house's rooms, your friend walked right across the open trapdoor in the floor, and a walk to somewhere else went straight over it. Now the hole stops your friend at its edge, a walk goes round it, and "🧱 collision boxes" shows it. Its ladder down is taken from the hole's edge, as before, and a jump still clears it.

## 0.0.165 (2026-10-17)

Everything in 0.0.164, with a school of fish back in the deep sea, swimming.

- **A school of fish that swims**: the deep sea has its little school back, over the far ridges. Each fish now swims its own way round, a little behind the one ahead, turning to face where it's going and beating its tail, instead of all of them sliding about together as one block.

## 0.0.164 (2026-10-17)

Everything in 0.0.163, with no ways off the edge of the world.

- **Fixed: ways out could lead off the edge of the map**: a place put in a square at the edge of the world designer's grid kept all its usual ways, even ones pointing off the grid (a fairy forest in the bottom right corner had a way down and to the left, into nothing), and "🔗 all passageways" left them there. Now a place put on the grid, a square moved to an edge, or a grid made smaller keeps only the ways that stay on the map, and "🔗 all passageways" takes away any left from before, saying how many.

## 0.0.163 (2026-10-17)

Everything in 0.0.162, with big arrows that keep out of each other's way.

- **Fixed: big arrows could cover each other**: with ♿'s "🧭 all arrows" on at a large size, an arrow on a side of the picture could end up over a corner's arrow (the left one over the bottom-left one, or the top one over the right one in a corner square). Now an arrow that still overlaps another moves to the nearest free place along its edge.

## 0.0.162 (2026-10-17)

Everything in 0.0.161, with arrows instead of signs for the ways out.

- **The old signs are gone**: the signs that used to link places together (in the forest clearing "🥾 path ⬇️" and "🧚 fairies ↗️", the castle's "🌲 clearing", the fairy forest's "🌲 clearing ➡️", and the signposts on the mountain trail, at the jungle waterfall, at the camp site and on the path through the woods) are gone. The ways between places are the world designer's now: walk near the edge of a place and its arrow shows where that way leads. For a sign that's always right, put a "🧭 sign with directions" in the place.
- **Rooms show their way toward you with an arrow**: in the castle's rooms, the living room and the tree house's rooms, the way out toward you (at the bottom of the picture) was a small triangle, or a sign on a post in the king's room. Now it's an arrow like the world's, shown when your friend is near the bottom. Tap it, press X, or just keep walking down to go through.
- **Each room's way toward you can lead nowhere**: in the castle designer and in each room's settings, a room's ⬇️ way toward you can lead to another room, out of the castle, or "🧱 nowhere". Leading nowhere, there's no arrow, and no way out there. The king's room, the dining room and the spiral stairs can lead nowhere now too.

## 0.0.161 (2026-10-17)

Everything in 0.0.160, without the deep sea's school of fish.

- **No school of fish in the deep sea, for now**: the little school that slid round in a loop over the far ridges didn't look like fish swimming, so it's gone until it can be made to swim well. A "🐟 fish" you put in from the "🐚 sea" things still swims.

## 0.0.160 (2026-10-17)

Everything in 0.0.159, with the game in view while you design its world.

- **See the game while you design it**: in the world designer's ⛶ full screen, the game's picture is now the first card on the right, as wide as that side. It stays in place while the cards under it scroll, so you can walk your friend, follow the arrows to the next square, and drag things from "🪨 things here" straight into the picture. Drag the bar between the grid and the right side to make the picture bigger or smaller. Tap its 👁️ to fold it away for more room, and tap it again to bring it back; the browser remembers which. It shows when the window is at least 900 pixels wide.

## 0.0.159 (2026-10-17)

Everything in 0.0.158, with far-off passages back within reach.

- **Far passages work again**: in the forest clearing, standing as close as you can get to the castle's sign or the tree house's doorway, ✨ takes you through again. Since 0.0.153 made faraway things so much smaller, they had counted as out of reach.
- **Big arrows keep apart**: with ♿'s arrows at twice their size, the far arrows now drop their words whenever that's what it takes to keep from covering each other.

## 0.0.158 (2026-10-17)

Everything in 0.0.157, with tree house rooms you can see into like the castle's.

- **Square tree house rooms**: each room in the tree house now has a back wall, side walls going off toward you, a ceiling of rafters and a floor of boards, like the castle's rooms. The doors to the rope bridges are on the side walls, so the bunk beds, the kitchen counter, the bookcase and the keyboard no longer stand in front of them, and the side walls stop your friend as the castle's walls do.

## 0.0.157 (2026-10-17)

Everything in 0.0.156, with pits that count as things you meet.

- **Pits are part of bumping into things**: in the trap room, a walk to somewhere else now goes round the pits instead of across them. Walk onto one yourself and you still fall in, now when both feet are over the hole, and 🧱 collision boxes shows each pit (and lights it up when you fall).
- **Doors stay on the wall**: in the castle's throne room, a side door is no longer drawn over your friend standing beside it.
- **Tidier ♿ rows**: a helper's colour and sliders now wrap one at a time next to its switch, so more of them fit on one line.

## 0.0.156 (2026-10-17)

Everything in 0.0.155, with walks that go round things they start or end beside.

- **Round, not through**: walking to a doorway that starts or ends right beside something (the healer's soup pot, the bunk beds), your friend used to walk straight through it; now the walk goes round it.
- **The compass shows which ways are on**: in the world designer, a square's ways out that are on are lit again in the new 3×3 compass.

## 0.0.155 (2026-10-17)

Everything in 0.0.154, with a roomier world designer in full screen.

- **The grid stays put**: in ⛶ full screen the world's grid stays on the left, as big as the window allows, while the controls on the right scroll past it; drag the bar between them to give either side more room (the browser remembers).
- **Letters and numbers**: full screen, the columns are labelled A, B, C … and the rows 1, 2, 3 …, sized with the squares.
- **A compass for the ways out**: a square's ways out are now a little 3×3 compass, with the square's own picture in the middle.
- **Easier to read**: the name on signs box has room around it and bigger letters in full screen; with warnings shown, only this square's are listed, in a dotted box under its compass (the other squares are counted, and red on the grid); and the controls on the right are cards side by side, each thing in the square its own card.

## 0.0.154 (2026-10-17)

Everything in 0.0.153, with a tidier ♿ Accessibility tab.

- **One row for each helper**: 🧭 all arrows, 👣 planned path and 🧱 collision boxes (a brick wall now) each have a row: switch it on, and its colour and its sizes appear right beside it, in a box, with short sliders. The path's line and the boxes' lines can be thicker or thinner, and the boxes' names bigger or smaller. The size of the words in the game has a box of its own.

## 0.0.153 (2026-10-17)

Everything in 0.0.152, with places that look deeper, and palm trees you bump into all the way up.

- **🔭 smaller going north**: walking north, into the picture, your friend now gets much smaller in the outdoor places (on the mountain trail, about half the size at the far edge; in the forest clearing, about a fifth), and every place has its own setting in 🏞️ Background for how much smaller, for each tenth of the picture: 0 keeps them the same size anywhere.
- **Palm trees bump all the way up**: a tree's trunk now has five collision boxes stacked up it, each as wide as the trunk at that height, so a palm's leaning trunk is solid where it leans. Your friend's feet meet the low parts of things and their body the high parts, so you can step over a low rock between your feet.

## 0.0.152 (2026-10-17)

Everything in 0.0.151, with things solid again while you walk.

- **No more walking through things**: since 0.0.150 your friend could walk right through a cauldron, a rock, a bush or the couch, because a foot swinging forward into it counted as already being inside it, and you may always walk out of something you are in. Now walking out has to take you away from it.

## 0.0.151 (2026-10-17)

Everything in 0.0.150, with corners that take you round the corner.

- **Walk into a corner to go that way**: walking on into the far edge or the bottom near a corner (within a quarter of the picture's width of it), or into a side near the far edge or the bottom, now takes the corner's way out, NW, NE, SW or SE, if there is one, else the way straight ahead. In the middle of an edge it is the way straight ahead.
- **The far arrows back where they were**: the ⬆️ arrows for the far ways (N, NE, NW) are back at the far line; down at the far edge, in a place with a shallow floor like the witch's woods, they crowded the side arrows.
- **Truer boxes**: the witch's cauldron's box is the pot, not its fumes up to the sky; the living room's plant's box is its pot, not its leaves; and the small mushrooms on the path through the woods are solid too.

## 0.0.150 (2026-10-17)

Everything in 0.0.149, with your friend meeting things with their feet and body.

- **A box under each foot**: your friend now bumps into things with the soles of their shoes, a little box under each foot, the size of the shoe, and one more under the middle of their body, as wide as the gap between their feet, so a thin pole can't pass between their legs. 🟥 collision boxes shows all three; the body's box stands from the hips up through the head.
- **Through an edge facing it**: walking on into an edge, your friend now goes straight through it, still facing that way, instead of turning back to walk to the arrow's spot first. Tapping an arrow, they walk to it, then step through it the way it points.

## 0.0.149 (2026-10-17)

Everything in 0.0.148, with every place's far edge where its ground ends.

- **No more walking up into the sky**: in every outdoor place your friend now stops at the far edge of the ground (the meadow's edge, the shore, the lake's far side, the fence, the moat, the house), not high up among the trees and clouds. 🟥 collision boxes shows it as the dashed line.
- **Walk into an edge to go that way**: in the world, walk on into the far edge, the picture's side or bottom, or a corner, and your friend goes through the way out that way, if there is one. The ⬆️ arrows now stand at that far edge, where you can reach them.

## 0.0.148 (2026-10-17)

Everything in 0.0.147, with collisions you can see.

- **🟥 Your friend's own collision box**, at their feet, with the others.
- **A bump glows**: when your friend walks into something, their box and the thing's box (or the wall) light up, half see-through, and fade back. How quickly they fade: ♿ Accessibility, ✨ glow fade.

## 0.0.147 (2026-10-17)

Everything in 0.0.146, with trees and posts that stop you at their trunks.

- **A tree stops your friend at its trunk, and a signpost at its post**, wherever the trunk or post is in the drawing. Before, the place you bumped into was the middle of the whole tree (its leaves spread to one side) or the middle of the sign's boards, so the 🟥 collision boxes stood beside the trunks.

## 0.0.146 (2026-10-17)

Everything in 0.0.145, with ♿ Accessibility tidier.

- **♿ Accessibility's three switches at the top**: 🧭 all arrows, 👣 planned path, 🟥 collision boxes.
- **A colour for the planned path and for the collision boxes**, each its own picker (yellow and red to start), and one picker for the far arrows' colour too.

## 0.0.145 (2026-10-17)

Everything in 0.0.144, with the arrows in their places in full screen.

- **In full screen, the nav arrows (and the collision boxes and the planned path) move to their places** as the picture grows or shrinks. Before, going full screen left them where the smaller picture had them, bunched in its top left.

## 0.0.144 (2026-10-17)

Everything in 0.0.143, with bubbles to blow.

- **🫧 Blow bubbles**, anywhere: hold 🫧 on the controller (beside select on the screen's controller, or the left trigger) or B on the keyboard, and bubbles stream out and float up.
- **From the mouth, or either hand**, and in up to three bubble themes at once, each with its own colour, size, shine and sparkle: 🎮 Controls, "🫧 blowing bubbles". The themes are the same ones bubble space uses.

## 0.0.143 (2026-10-17)

Everything in 0.0.142, with bubble space.

- **🫧 Bubble space**, a new place (with the empty room, in 💭 imagination): a room full of bubbles drifting in a gentle wind. Bubbles that touch stick together and float on as clumps, and any that touch your friend pop! New ones drift in, so the room stays full.
- **Its settings** (🏞️ Background, in bubble space): how many bubbles, how windy, and up to three bubble themes, each with its own colour, size, shine and sparkle (a star or a dot, in a colour of its own).

## 0.0.142 (2026-10-17)

Everything in 0.0.141, with big arrows that keep out of each other's way.

- **🧭 all arrows, made big, in a small picture**: when there isn't room for every arrow's words, a far arrow shows just its triangle, so none lies over another; its words come back as your friend comes near.

## 0.0.141 (2026-10-17)

Everything in 0.0.140, working again.

- **The game starts again**: 0.0.140 stopped while loading (a list of the sea things' colours was read before it was made). Everything in 0.0.140, the 🪸 deep sea and the 🐚 sea things, works now.

## 0.0.140 (2026-10-17)

Everything in 0.0.139, with the deep sea.

- **🪸 The deep sea**, a new place: deep blue water with light coming down in shimmering shafts, a sandy bottom, kelp swaying, rocks, a little school of fish darting about, and bubbles rising from a crack in the sand. Put it in the world from 🗺️ World design, next to open water or more deep sea.
- **🐚 Sea things to put anywhere**: coral, shells (a scallop, a conch, a clam), a ⭐ starfish that crawls, a 🐟 fish that swims round and round and blows bubbles, a 💰 treasure chest, an ⚓ anchor with a broken chain, and sea plants: kelp and sea grass that sway, a sea fan and sea lettuce. Each in the colour and size you like, facing left or right.

## 0.0.139 (2026-10-17)

Everything in 0.0.138, with ♿ Accessibility put right.

- **The nav arrows stay in their places** with the collision boxes or the planned path shown. Before, showing the boxes moved every arrow onto another's place.
- **The collision boxes stay shown** until you turn them off, from square to square. They used to vanish when you went to the next square.
- **Each collision box shows its height** too, so it stands over the thing it is, not just on the floor in front of it.
- **Short chips**: 🧭 all arrows, 👣 planned path, 🟥 collision boxes, each lit while it's on; and short slider names (arrow size, text size).

## 0.0.138 (2026-10-17)

Everything in 0.0.137, with the tune chooser neater on a phone.

- **🎶 genre/track selection on a phone**: each row's name sits above its chips, so the chips have the whole width.

## 0.0.137 (2026-10-17)

Everything in 0.0.136, with a neater tune chooser.

- **🎶 genre/track selection**: the music tab's tune chooser has a shorter name, and is smaller and neater, its two rows (the genre, then its tracks) each named at its start, without the dashed boxes.

## 0.0.136 (2026-10-17)

Everything in 0.0.135, with the forest crossings' corner trails lined up.

- **A forest crossing's ↙️ and ↘️ trails lead to their arrows**: each runs to its bottom corner, under the arrow that takes you that way, instead of off the side of the picture above it.

## 0.0.135 (2026-10-17)

Everything in 0.0.134, with a ♿ Accessibility tab.

- **♿ Accessibility**, a new tab, for seeing and reading the game more easily:
  - **🧭 every nav arrow shown**, not only the near ones: the far ones dimmer, in a colour of your own (how bright, and which colour);
  - **↕️ the nav arrows' size**, bigger or smaller;
  - **🔤 the words in the game**, on signs and the arrows, bigger or smaller;
  - **👣 the planned path**, drawn while your friend walks to a door or a picture;
  - **🟥 the collision boxes**: what your friend can't walk into, shown as red boxes on the floor.

## 0.0.134 (2026-10-17)

Everything in 0.0.133, with walks that go round things.

- **Walking around what's in the way**: tap a door, a sign, a picture or any way out, and your friend plans a way there around what stands in the way (a wagon, a cauldron, a soup pot, toadstools, a couch...), instead of walking straight through it. Swimming too, in deep water.

## 0.0.133 (2026-10-17)

Everything in 0.0.132, with the tree house's trapdoor the right way round, and doors on its rooms.

- **⬇ takes the trapdoor down**: standing at a tree house room's trapdoor, press ⬇ to climb down its ladder, out of sight below the hatch's edge, into the room below. ⬆ there used to climb up the ladder's top instead.
- **Doors in the tree house rooms**: each doorway to a rope bridge has a plank door now. Go through it and it swings open, as the castle's doors do, and you walk through to the next house.

## 0.0.132 (2026-10-17)

Everything in 0.0.131, with a world designer that fills the screen.

- **🗺️ World design, full screen**: a button at the top of the tab opens it over the whole window, as the music's does. On a wide screen the grid takes the left side, as tall as the window, and the square's controls sit beside it. ✕ or Escape go back to the game.
- **The grid fills its room**: the squares grow to fit the space, in the usual two columns too, and their emojis and names grow with them.
- **Each kind of square has its own colour**: water blue, the green places green, the witch's woods purple, the castle its stone grey, and so on, faint in light and dark. The places to drag above the grid have the same colours.
- **A square with a warning, and the square you are in, have thick outlines**, red and blue, easy to see.
- **When the picture shows a place that is not your square**, the tab says so at the top: that it is not in the world yet, or which square has it.
- **The music's full screen is smaller and neater**: a slim bar at the top with ✕, and its controls side by side in cards instead of rows across the whole screen.

## 0.0.131 (2026-10-17)

Everything in 0.0.130, with the way arrows kept apart.

- **Way arrows no longer cover each other in the corners** (Matthew, in the witch's woods): when two arrows show at once, like ➡️ to the beach and ↘️ to a forest crossing near the bottom right, one slides along its edge, away from the other, so both arrows and their words can be read. At any window size, and full screen too.

## 0.0.130 (2026-10-17)

Everything in 0.0.129, with every cauldron simple until you upgrade it.

- **Every cauldron starts simple** (Matthew, after trying the witch's places on the Ubuntu laptop): the cauldrons you put in a place, and the witch's own cauldron in her woods and in her hut. Upgrade any one of them to ✨ fancy, one at a time: yours in its row in 🗺️ World design (💨 fumes), hers in 🏞️ Background (💨 her cauldron's fumes).
- **The witch's own cauldron gets the red frame too**: when the place is too busy and her cauldron is fancy, it is marked like the things you put there, and the 🐢 note says where to make it simple.
- **The simple fumes look more like the fancy ones**: they sway as smoothly, and each curl is bright where it comes round to the front and faint at the back, so the two plumes twist around each other.

## 0.0.129 (2026-10-17)

Everything in 0.0.128, with cauldrons that cost less and busy places that stay shut.

- **Simpler cauldron fumes** (Matthew): with two or more cauldrons in a place, or one on a slow computer, each cauldron's fumes are two smooth curls that sway and fade as they rise, instead of the twisting spiral. Fifteen cauldrons now move almost as smoothly as five did before (on GitHub's computers: 45 frames a second, against 48).
- **💨 Fumes: auto, ✨ fancy or simple**, for each cauldron you put in a place (Matthew): auto picks for you, fancy always has the spiral, simple always has the curls.
- **🐢 A place with a red frame takes nothing more** (Matthew, after N kept adding cauldrons): while anything in a place has the dashed red frame, every button for putting things there is greyed, not only the moving ones. Take away a marked thing, or make a cauldron's fumes simple, and once the place moves smoothly again the buttons come back.

## 0.0.128 (2026-10-17)

Everything in 0.0.127, plus what's new, right in the game.

- **📰 What's new, version by version** (for M), at the bottom of ⚙️ Advanced: every version of the game, newest first, with what changed in it. The one you are playing is marked "this one". It has a scroll bar of its own.

## 0.0.127 (2026-10-17)

Everything in 0.0.126, with busy places kept smooth.

- **🐢 A place can't be filled with more moving things than the computer can show smoothly** (Matthew, after N's fifteen cauldrons stopped the Ubuntu laptop): a cauldron or a campfire put in a place is followed by a quick check of how smoothly the picture still moves. When it gets too slow, or there are already many moving parts, the buttons for more moving things are greyed and a note says why.
- **The busiest things are marked** with a dashed red frame, in their rows and in the picture: take one of those away and the place moves smoothly again.

## 0.0.126 (2026-10-17)

Everything in 0.0.125, with the pointer picking where to go, swimming there, and every ladder climbed.

- **Point at a way out to pick it** (Matthew): move the pointer over a door, picture or sign, or near where an arrow to the next square would be, and it glows as the one to take, even when another is nearer. Click it, or press ✨ X or Enter, and your friend goes there and through. A picture far away is walked to first.
- **Swimming there**: on the way to a way out through deep water, your friend swims instead of walking.
- **The tree house's trapdoors are climbed down** (Matthew: they didn't animate): your friend climbs down the ladder into the hole, out of sight below its edge, to the room below, at the same good pace as climbing up.

## 0.0.125 (2026-10-17)

Everything in 0.0.124, with the 🏞️ Background tab lighter while your friend moves.

- **The Background tab costs a quarter of what it did while your friend moves** (^du_report(3)'s next step): measured on GitHub's computers, its refresh went from about 26 ms to about 6 (Body's is under 1). It no longer builds a whole castle to read one room's doors, and it changes a button only when something about it changed.

## 0.0.124 (2026-10-17)

Everything in 0.0.123, plus open water.

- **🌊 Open water**, a new place (Matthew): the sea all round, an island and a sail far off, your friend swimming wherever they go.
- **The first world has a row of open water along its top**, beyond the beach (Matthew): the beach's way north goes out to sea, and the water squares join each other along the row. Everything else is a row further down. If your first world is untouched, it gets the new row too; one you have changed stays as you made it.
- In 🔗 all passageways, open water joins only open water and the beach's sea side.

## 0.0.123 (2026-10-17)

Everything in 0.0.122, plus M's all-passageways button.

- **🔗 All passageways** (M's idea), in 🗺️ World design: joins every pair of squares side by side (or corner to corner) where both places have room for a way that way. A forest crossing takes all eight; never into the beach's sea, through the back of the castle or the house, or off the wood path's sides. It says how many ways it added.
- **The waterfall's north is the waterfall** (M): going north there is going into the falls, so it is no longer a way to the square above. In the first world, the waterfall now joins the forest crossing to its north-east instead.
- **Fixed: 🌍 the first world again, pressed at a forest crossing**, gave the first world's crossing your crossing's paths, with a warning at once.

## 0.0.122 (2026-10-17)

Everything in 0.0.121, plus M's light and dark switch.

- **🌗 Light or dark** (M's idea), at the top of ⚙️ Advanced: auto (as your computer is set), ☀️ light or 🌙 dark. This browser remembers it; saves don't carry it.

## 0.0.121 (2026-10-17)

Everything in 0.0.120, with the spin into pictures fixed, and two new sliders for moving.

- **Fixed: the spin into a picture and out of it** (Matthew): going in, your friend froze in place and you waited; coming out, some arms or legs were already standing there, not spinning. All of your friend spins now, in and out.
- **🏃 Top running speed** (Matthew), in 🎮 Controls: from half as fast to two and a half times; 100 is as it was.
- **🚀 Speeding up and slowing down** (Matthew), in 🎮 Controls: at 0, as before, your friend starts and stops at once; turned up, they take a moment to get going, and when you let go they slow down and stop, legs still going.

## 0.0.120 (2026-10-17)

Everything in 0.0.119, with the tree house rooms' doors in their place.

- **Fixed: in a tree house room, your friend standing in front of a side door is in front of it** (Matthew: the door was drawn over them, for a while now). The doors now stand at the back wall, as the hatch and the ladder up do.

## 0.0.119 (2026-10-17)

Everything in 0.0.118, with World design's warnings pointing at the fix.

- **The red squares are the ones to change** (Matthew): with the warnings shown, a square is red when one of its ways out could put a warning right, not the square next to it.
- **A way to take away is marked too** (Matthew): on its arrow, dotted with ✕, as when a way leads into an empty square. An arrow to turn on is dotted with ★, as before.

## 0.0.118 (2026-10-17)

Everything in 0.0.117, with one way through everything that leads somewhere.

- **Every way somewhere works the same** (Matthew: no more pictures vs passages): doors, pictures, signs, the ▼ at the bottom, holes in the floor, ladders and the arrows to the next square. Walk up to one and it glows; that is the one ✨ X, Enter or the 🚪 button takes you through. Others in reach glow too, dimmer. It works without a game controller as well, and a tap still goes straight through.
- **Many more ways can be reached from the controller**: signs and doorways that only the pointer could use before (of 180 in all the places, 166 now; 112 before).
- **A tap on a door or sign behind a fence or a bush goes through it**, as on the witch's hut behind her fence.
- **Ladders climb at a natural pace** (Matthew: they went up in slow motion): a tree house's ladder and the dungeon's trapdoor ladder, as fast as climbing with ⬆.

## 0.0.117 (2026-10-17)

Everything in 0.0.116, with M's idea for the ways out in World design.

- **A way out that would lead off the edge of the map can't be turned on** (M's idea): its arrow is greyed, so that mistake can't be made. One left on from before can still be turned off.
- **With the warnings shown, an arrow that is off but would put a warning right is starred** ★ (M): this one is off, and maybe should be on.

## 0.0.116 (2026-10-17)

Everything in 0.0.115, with the still pictures working again in Chrome.

- **Fixed: a slow computer holds the scenery still again while your friend moves, in Chrome** (Matthew: find why they never switched on). Since 0.0.95 the game waited for a signal Chrome never sends before it used them, so a slow computer running Chrome drew every place live, the slow way. Measured on GitHub's computers: the duck pond with them on in 99 to 100 drawings of every 100 (it was none).
- The speed check after each release tries a place that comes in under 55 frames a second once more before calling it slow, and says why a place failed.

## 0.0.115 (2026-10-17)

Everything in 0.0.114, plus a sign with directions, and names for the squares.

- **🧭 A sign with directions** (Matthew, for M, who likes signs), in the 🪧 signs group: a post with a board for each way out of the square, each saying where it goes. Choose its boards with the 🧭 chips in its row, from none to all eight. The boards can't turn, so where they are nailed says which way: the ways away from you high up, the ways across in the middle, the ways toward you low down, each on its own side of the post; straight ahead on top, straight back at the foot.
- **✏️ Every square has a name**: its place's (with where it is, like A5, if another square has the same place), or one you give it in World design. The signs and the arrows say it. Two squares with the same name are a warning.

## 0.0.114 (2026-10-17)

Everything in 0.0.113, with three fixes.

- **Fixed: things stay where you put them** (Matthew: the potion was on every square). Going through a place's own door, sign or picture, or back with the back chip, used to bring the things along. Now every place keeps its own, squares on the grid and the places inside them (a hut, a tent, a castle room) alike.
- **The living room has a ▼ at the bottom, back out to the house and yard** (Matthew). Where it leads is still yours to choose under "⬇️ behind you"; a friend saved with it leading somewhere keeps that.
- **Fixed: the witch's door works again** (Matthew: it no longer glowed or opened). The friend came into her hut standing right in front of it, so a click landed on them; they now come in beside it. The door also glows for the controller's X, as other doors do.

## 0.0.113 (2026-10-17)

Everything in 0.0.112, with more things in the library, taken from the places themselves.

- **🏕️ A camp group of things**: a 🪵 log to sit on and a 🔥 campfire from the camp site (burning, or put out), a 🪑 bench from the picnic under the sea, and a 🛢️ barrel from the castle kitchen.
- **🫕 The witch's cauldron**, bubbling green with its fumes twisting up, in the 🧪 magic group.
- Each is the very drawing the place already had, lifted out so it can stand anywhere; those places look exactly as they did (checked picture by picture against 0.0.112 before release).

## 0.0.112 (2026-10-17)

Everything in 0.0.111, with every thing's size yours to change.

- **🔎 A size slider for every thing you put in a place** (Matthew), from a quarter of its size to three times it, in its row under 🗺️ World design. Each kind starts at a good size of its own.
- **Toadstools start smaller**, about half as tall as before (Matthew: too big). The other things keep the sizes they had.

## 0.0.111 (2026-10-17)

Everything in 0.0.110, with green pines.

- **Fixed: a 🌲 pine put in a place is green everywhere** (Matthew: his were white, while M's tablet showed them ordinary, on the same version). A pine's needles take whatever colour they are given; the places give theirs a green, and a pine you put there got none, so it took the colour of the page's writing: white with the page dark, nearly black with it light. It now has the lake's green.

## 0.0.110 (2026-10-17)

Everything in 0.0.109, plus the start of a library of things to put in a place.

- **🧰 Things in groups**: in 🗺️ World design, the things to put here open a group at a time: 🌲 nature, 🧪 magic, 💀 spooky and 🪧 signs.
- **New things, from drawings the places already have**: a 🍄 toadstool (its cap's colour, and glowing or not), a 🧪 potion (a flask, a tall bottle, a jar or a vial, in any of the witch's colours), a 💀 skeleton sitting, and a 🪧 sign on a post **with your own words** (type them in its row, up to 12 letters). Each stands where you put it and grows nearer and smaller farther off, as the rocks and trees do.
- **Fixed: a saved world loads back exactly as it was saved** (0.0.109 gave squares with no things an empty list).

## 0.0.109 (2026-10-17)

Everything in 0.0.108, with the tree rooms' toys back, things in every place's world square again, and the world's checks passing.

- **Fixed: the tree rooms have their toys again.** Since 0.0.100 the 🧸 things-in-the-room slider and the things put in a place shared one setting: a tree room drew one toy instead of ten, and moving that slider could stop the picture drawing. The toys are their own setting now; a save from before keeps its number of toys.
- **Fixed: the 🗺️ World design tab offers things to put in every place again**, not only in a tree room (the same mix-up hid them).
- **The world designer's checks are up to date** with the world as it is now laid out, and say what they saw when one fails.

## 0.0.108 (2026-10-17)

Everything in 0.0.107, with the warnings in two lists.

- **Shown, the world designer's warnings come in two lists** (Matthew: a lot of them can overwhelm): first 📍 the square your friend is in, its own and any way into it from beside it, then 🗺️ the other squares. Both stay hidden until you tap **show warnings**.

## 0.0.107 (2026-10-17)

Everything in 0.0.106, with the world designer's warnings out of the way.

- **The world designer's warnings are hidden unless you ask for them** (M found them distracting). Near the top of 🗺️ World design, a line says how many there are; the **show warnings** chip lists them and marks their squares red, and **hide warnings** puts them away again. This browser remembers which; saves don't carry it.

## 0.0.106 (2026-10-17)

Everything in 0.0.105, with the ways out found by your friend's feet.

- **A way out is a spot on the ground, and which one is near goes by your friend's feet** (Matthew: by the arrow at the bottom of the wood path, X took the one at the top). Near and far are measured on the floor, with its depth: a step toward the back of a place covers more ground than one at the front, and walking past a way, toward its edge, still counts as at it. The front edge's way shows only once you walk down toward it, not from the middle.
- **The way X would take glows on its own arrow**, over the picture, instead of a frame drawn in the picture under it.

## 0.0.105 (2026-10-17)

Everything in 0.0.104, with the ways out showing only when you are near them.

- **A way out's arrow shows only when your friend is near it** (Matthew), fading in as they come close and out as they leave; while it shows it is on top of everything, the back chip and the picture's buttons too, and a tap on it walks through. Hidden, it is in nobody's way.
- **Its words on two short lines**, where it goes and the place's name, on the side toward the middle of the picture, so they stay on the screen (Matthew: they went off it).

## 0.0.104 (2026-10-17)

Everything in 0.0.103, with tidier forest crossings.

- **A forest crossing in the world shows only the paths that lead somewhere** (Matthew: one missing a passageway should have no trail, and no arrow, that way): a path switched on, with nothing beside it to meet, is not drawn, has no arrow, and is no longer listed as a mistake. Drop a crossing between places and it joins those that have a way toward it.
- **The crossing's paths have no outlines** (Matthew: they looked sloppy where they met).

## 0.0.103 (2026-10-17)

Everything in 0.0.102, plus a forest crossing that looks farther, and the world laid out again.

- **The forest crossing's paths meet nearer the middle of the picture** (Matthew: not so close to the bottom), its forest floor reaching farther back.
- **🌄 Off in the distance**, three to choose from for each crossing: 🌲 tall trees and shafts of light (as before, but kept to the top of the picture), ☁️ sky, sun and clouds over the trees far off (as at the forest clearing), or ⛰️ mountains far off, snow on the far ones, and far pines.
- **The world laid out again** (Matthew): the 🦆 pond in the middle, the land across the water a forest crossing you can walk to, not only see; the 🏖️ beach on the top edge, looking out on the water, the witch's woods and the mountain trail either side; the clearing east of the pond, the castle beyond it. A world you have not changed is laid out this way by itself; 🌍 the first world again (in 🗺️ World design) lays any world out this way.

## 0.0.102 (2026-10-17)

Everything in 0.0.101, plus designing the world by hand.

- **🗺️ World design, by hand** (Matthew): drag a place from the row of places onto a square of the grid (or tap it: the first empty square gets it); drag a square onto another to move it (two places swap), or onto 🗑️ to take it away; make the grid a column or row bigger or smaller, up to 12 by 12.
- **The square you are in:** its ways out, each an arrow to turn on or off (a forest crossing's are its paths); 🏁 start here; and its things: drag a rock, bush, tree or pine straight into the picture, where you let go (or tap to add one), then drag it about.

## 0.0.101 (2026-10-17)

Everything in 0.0.100, with two fixes.

- **Fixed:** opening the game, its first picture could fail to draw until you touched something (0.0.99 and 0.0.100): the world's part of the game loaded after the part that draws the first picture, which needs it.
- **Fixed:** adding a thing to a place, turning it around, changing its leaves or dragging it sometimes changed nothing in the picture until something else changed (since 0.0.98): the place's picture is kept between drawings, and a thing changed in place did not count as a change. Things are drawn apart from it now, each kept on its own.

## 0.0.100 (2026-10-17)

Everything in 0.0.99, plus walking from square to square, and things in every place.

- **Ways out, in the picture** (Matthew: "how do I access those passageways in the game?"): in a place on the world's grid, a small bobbing triangle at each of its ways out, pointing out: ahead at the far end of the ground, toward us at the bottom, left and right at the sides, the ways between at the corners. Point at one to see where it goes; click it, or press X near it, and your friend walks through into the square beside it, coming in from that side. The back chip brings them back. A way that leads nowhere yet is grey, with 🚧.
- **Things in every place**, not only the forest crossing: rocks, bushes, trees and pines to put anywhere and drag about, each place in the world keeping its own.
- Choosing a place in 🏞️ Background takes your friend to its square in the world, with its things.

## 0.0.99 (2026-10-17)

Everything in 0.0.98, plus the world, on its grid.

- **🗺️ World design shows the world** (Matthew): today's places laid out on a grid of squares, the forest clearing in the middle, joined to the squares beside them through the ways out on their shared sides (the little brown marks on each square's edges), with forest crossings between. Tap a square to go there; a forest crossing changed there (its paths, its floor, its things) stays changed in its square. Under the grid, anything to put right (a way out to an empty square, or to a place with no way back, or a place you can't get to), and the places inside others (the living room, the castle's rooms, the witch's hut...), which are off the grid.

## 0.0.98 (2026-10-17)

Everything in 0.0.97, plus a forest crossing to fill with things, a quieter way out toward us, and the start of a world to design.

- **🛤️ A forest crossing** (Matthew: a blank place for the world designer, in 🌲 woods): a forest floor (olive green, or another colour) where up to eight brown paths meet: ahead, toward us, left, right, and the four ways between. Each path on or off.
- **Things to put there:** 🪨 rocks, 🌿 bushes, 🌳 trees and 🌲 pines, each sized for how far back it stands, as your friend is; drag one to move it. Each is its own: ↔️ turn it around, 🎲 grow it another shape, and a rock's colour, a tree's height and leaves (summer, spring, autumn, 🌸 blossom), a pine's tiers, a bush's flowers.
- **A way out toward us is a small ▼ at the bottom of the picture** now, gently bobbing, not a sign on a post (Matthew: signs everywhere looked unnatural): the castle rooms' ways behind you, the living room's and the tree rooms' door behind you. It glows when you point at it or when your friend is near, says where it goes when you point at it, and X goes through it, as through a door.
- **🗺️ World design**, a new tab after the music: coming soon, a world to design on a grid of squares.
- Behind the scenes: the game's code is in files of its own now, by section (it was one file of 15,850 lines); nothing changes in how it plays. Save and Load are checked on every change, a crossing and its things included.

## 0.0.97 (2026-10-17)

Everything in 0.0.96, with the still pictures working again on a slow computer.

- **Fixed:** on a slow computer, the places were drawn live again while the friend moved, not as still pictures (since 0.0.95). The fix for the white flashes waited for each picture to be fully ready in a way Chrome never finished, so no picture was ever used: the duck pond as still pictures in 0% of drawings on our test computer, where it was 99% before. Now that wait lasts at most a quarter of a second; the flash fix stays. Found by the speed check after 0.0.96 (🚧🐢 on that version).

## 0.0.96 (2026-10-17)

Everything in 0.0.95, plus a sign when a version runs slow.

- **🚧🐢 after the version number when it runs slow** (Matthew: deploy as soon as a version is made, so M and N can try it at once, and check its speed apart, so nobody has to): each new version goes out first, and then our test computer checks how fast it draws. If it came out slow, 🚧🐢 shows after its number (🚧 if something else needs fixing), and the next version fixes it. Nothing shows while it is being checked, or when it is fast enough.

## 0.0.95 (2026-10-17)

Everything in 0.0.94, plus the 🏞️ Background tab fast again, no white flashes while moving, furniture that stands in the way, postcards, walking toward us on a slant, and a plain "please wait" while ⚙️ Advanced measures.

- **Fixed:** with the 🏞️ Background tab open, moving the friend was slow everywhere, worst in the busy places (the duck pond at 2 to 15 frames a second instead of 60 on a test computer), and stayed slow until the page was reloaded, which opens the 🧍 Body tab again. The castle's room menus in that tab worked out the whole castle for every choice's name on every refresh; they now look up just the room they name.
- **Fixed:** white flashes while walking or dragging (Matthew and the kids, since the still pictures became bitmaps in 0.0.93). On a computer the game finds slow, the place is drawn as still pictures while the friend moves, and a picture could be shown a frame before the browser had its pixels ready, so that frame was blank. A picture is now used only once it is ready to draw.
- **While ⚙️ Advanced measures, it says so plainly** (Matthew): during 🧪 this computer's speed (about 40 seconds) and 📏 measure here, a notice over the game says to wait and touch nothing, with the seconds gone of about how long it takes and a bar filling up; 📋 copy, 📝 paste, 🧪, ⏱️, 📏 and the feature switches rest until it ends, so a half-done line can't be copied or a second measure started.
- **What stands in a place is solid** (Matthew: "the objects in a room that one can't reasonably walk through"): tables, beds, wells, stoves, barrels, bookcases, rocks, bushes and the rest of what stands, and trees by their trunks, stop the friend, as the seats have since 0.0.89. The low ones (a rock, a log, a bush, a toadstool) can be jumped over, a short jump clearing the lowest. Houses, huts, doors and ladders stay open, and so do the animals and the people.
- **The flat rooms have back walls:** in the 🛋️ living room, the 🪜 tree house's rooms, the 🧙 witch's hut, the 🌿 healer's hut and ⛺ a tent, the friend stops at the foot of the back wall, as in 💭 our imagination, instead of walking up it.
- **Postcards** (💾 Save ▾ → "💌 Save a postcard"): your friend in their place, in a card's cream border, with something written under it, saved as one picture ready to send. The words start as the picture's own line ("feeling happy outside of N's castle"); type anything instead. A long message goes on two lines.
- **Walking toward us on a slant** (Matthew: the controller's diagonals): holding down and left, or down and right, the friend faces us turned partway that way, body a little narrower, the far arm behind, the face looking that way, instead of turning all the way to the side. Straight sideways is still a side view, and walking away on a slant is as before.
- **Ready, not on yet:** 📮 "send it to Matthew" beside the 🧪 and ⏱️ lines, which posts the line to a log of our own (ticket 166); it stays hidden until that log is running, and 📝 the shared pad works as before.

## 0.0.94 (2026-10-16)

Everything in 0.0.93, plus climbing that looks like climbing, and ladders you can click.

- **Hand over hand** (Matthew: the arms "should be taking turns grabbing upward, and pulling the body, but they just wave around"): climbing a ladder, one hand holds a rung and pulls while the body rises past it, the hand staying on its rung, then lets go and reaches up for the next while the other hand holds; both hold for a moment between. The legs step on the same rhythm. It keeps pace with the climb, by hand with the controller or on its own.
- **Click a ladder and the friend climbs it** (Matthew: the walk to it was good, but the climb should be animated too): they walk to its foot and climb it, and through at the top where it leads somewhere (a tree's house, a dungeon's way up); a ladder that leads nowhere they climb and hold on at the top. Clicking a tree's house walks them to its ladder and up.
- **Ladders that lead somewhere glow** when the friend comes near, as pictures and doors do, and X climbs them: the tree house's ladders outside, too.

## 0.0.93 (2026-10-16)

Everything in 0.0.92, plus the still pictures as bitmaps, for the iPad.

- **On a slow computer the places' still pictures are bitmaps now** (the kids' computers, 🧪 on 0.0.92): the duck pond swam at 22 frames a second on the iPad and 27 on the black laptop as it is; with the place as still pictures drawn as svg, 24 on the iPad (Safari draws them anew every frame) and 59 on the black laptop; as bitmaps, drawn once into the screen's own pixels, 60 on the iPad and 58 on the black laptop. So the pictures are bitmaps everywhere, at the picture's size on the screen (made again if it changes size).
- **Found choppy sooner**: the frames are judged 60 at a time, not 120, so the iPad and the black laptop hold the places still after about 3 to 5 seconds of moving in a busy place, not 7 to 11.
- 🧪's pass 9 is the pictures as svg, pass 10 as bitmaps, as the game now draws them.

## 0.0.92 (2026-10-16)

Everything in 0.0.91, plus a fast computer no longer stays "choppy".

- **"Choppy" is measured afresh on every visit** (Matthew's laptop, 🧪 on 0.0.91: 57 to 61 frames a second in every pass, yet marked choppy): the mark was kept in the browser, so one bad moment held every place still on every visit after, and made the still pictures of 0.0.87 build on every arrival. Now a computer is choppy only for the visit it was measured in, and only when its frames come late (more than a quarter of them) for two stretches of 120 frames in a row while the friend moves; a trip through a doorway does not count (its arrival is a long frame by nature). The black laptop is found choppy again within seconds of moving in a busy place.
- **🧪's held-still passes really hold still now**: passes 5 (held still), 9 (a still picture) and 10 (a still bitmap) marked the computer slow for their run, but on a computer that draws fast (the black laptop, Matthew's) the game took the mark back after one drawing, so those passes measured the place moving (the black laptop's pond on 0.0.86: 30 moving, 31 "held still"). They hold it still with a switch of their own now, and pass 9 and 10 say how many drawings had the pictures. And no 4-second wait in each pass.

## 0.0.91 (2026-10-16)

Everything in 0.0.90, plus ⏱️ timing a trip, to find where it pauses.

- **⏱️ time the next trip** (⚙️ Advanced, under 🧪; Matthew: pauses going through a castle door and into a pit, on his laptop, where our own tests show none): press it, then go through a doorway (click a door or a pit, press X or 🚪, or walk onto a pit). Every frame is timed, from the start of the trip until the friend has arrived and stood a moment; the line names the long frames, the part of the trip each fell in (walking, stepping in, teetering, falling, arriving, fading in, dropping in, landing), and how much of each was the game's own work (drawing, building the place) and so how much the browser's. 📋 copy, and 📝 paste it into the shared page.

## 0.0.90 (2026-10-16)

Everything in 0.0.89, plus falling into the castle's pits (N's).

- **Falling into a pit** (N, via Matthew): in a trap room, walking onto a pit's hole (not jumping over it) drops the friend in: arms flung up, a moment's teeter, then down into the hole, ever faster, hidden below its near rim. X, a click or 🚪 the nearest doorway walks them to the pit's middle first. Landing a jump on a pit falls in too.
- **Landing in the dungeon below**: each pit drops into the dungeon its sign names; the friend drops in from above the picture and lands on bent knees (the high jump's landing), then stands. The dungeon's ladder still climbs back up.

## 0.0.89 (2026-10-16)

Everything in 0.0.88, plus walls and things the friend can't walk through, and jumping over the small ones.

- **The castle rooms' walls are solid** (Matthew): the friend stops at the back wall's foot and at the side walls, which close in as the room goes back. The spiral stair's room has no back wall to stop at: walking up it is going up the stairs, to its doors.
- **What one sits on is solid**: barrels, benches, chairs, logs, couches, the throne. The friend stops at them, walking or dragged, and slides along them; stopping at one sits down on it, as before.
- **Jumping over the small ones**: while jumping, a short jump clears what is lower than half the friend's legs, a high jump what is lower than their legs, if it is narrow enough to clear in one jump. Landing on one, they step off by its nearer side.

## 0.0.88 (2026-10-16)

Everything in 0.0.87, plus doors that open, and walking through them.

- **Doors glow, and X goes through** (Matthew): the castle's doors, its archways, its gate from outside, and the house's front door glow when the friend comes near, as pictures have since 0.0.74, and whichever door or picture is closest gets the glow; X (or a click, or 🚪 the nearest doorway) goes through.
- **Through a door on foot**: the friend walks up to it, a wooden door swings open by itself (by magic: no reaching for the handle yet) on a dark doorway, and they step in and are gone, then come in, fading in, in the place beyond, facing us. The signs at the picture's foot (a door behind you) walk them out toward us.
- **The spin is for pictures and windows only** (Matthew: "it's the most magical looking").

## 0.0.87 (2026-10-16)

Everything in 0.0.86, plus the places drawn as still pictures while the friend moves, on a slow computer.

- **On a slow computer, each place is a still picture while the friend moves** (for the black laptop: its 🧪 on 0.0.86 swam the duck pond at 30 frames a second, and at 59 with the pond's scenery as one still picture). While the friend walks, swims or is dragged, the place is drawn as pictures cut where the friend stands in it, so they still go behind the trees in front of them and in front of the ones behind. A moment after they stop, the place is itself again, and doors, pictures and ladders work as ever; the ✨ glow on a picture to go into shows the whole time. A fast computer draws everything as before; ⚙️ Advanced's 🌀 moving scenery on a slow computer turns it off.
- **🧪 for the iPad** (where the still picture of 0.0.86 helped nothing): two more passes, the pond's still picture as a bitmap drawn once in pixels, and the friend swimming without both the rings and the part under water. Fourteen passes, about 40 seconds; pass 9 is now the still pictures as the game draws them.

## 0.0.86 (2026-10-16)

Everything in 0.0.85, plus 🧪 tests that find what costs the duck pond its frames.

- **🧪 this computer's speed takes the duck pond apart** (Matthew, for the black laptop, where the pond is choppy and no other computer we have shows it): after swimming across the pond moving and held still, the friend swims across it four more times, each with one part taken away: without its scenery, without the friend's faint part under the water, without the rings, and with its scenery as one still picture the browser draws once. Then the pond with its music, as before.
- **The empty room after the pond** (Matthew: after the duck pond, the empty room was as slow as the pond until the page was loaded again): 🧪 ends by walking across the empty room once more, then with its music, so one line shows whether the slowness stays behind.
- Twelve passes, about 40 seconds; the line and its `DATA {...}` block carry each pass's numbers. Your friend and everything else go back as they were after.

## 0.0.85 (2026-10-16)

Everything in 0.0.84, plus the music's full screen from the game's, and a mixing board in colours.

- **🎚️ the music, full screen, from the game's full screen** (Matthew): next to the 🔇 mute in the corner of the full-screen picture, it opens the 🎵 Music tab over the whole screen. ✕ goes back to the game's full screen, with the pinned controls as they were, and the screen stays full the whole way. From the Music tab, ✕ goes back to the game with its controls, as before.
- **The LFOs are as wide as an instrument:** in the music's full screen, the whole tune's strip and the seven LFOs were half the screen wide each; now they take two of the board's columns, two rows tall, with the instruments beside them.
- **Each part of the music tab has its own colour:** the tune, where it is, the volume and the other main controls in blue, the whole tune's strip in peach, the LFOs in lavender, and the instruments in mint. Pastels in light mode, dim colours in dark mode.

## 0.0.84 (2026-10-16)

Everything in 0.0.83, plus the duck pond held still where it would be choppy.

- **The places hold still when the friend's moves come out choppy** (Matthew: the duck pond was still choppy on the black laptop, while the controller was held, and the music paused with it). The black laptop draws the friend as fast as Matthew's laptop, so 0.0.83's test of drawing time never found it slow. Now the game also watches the frames while the friend moves: where more than one in ten comes late, the places hold still from then on (their animations, the rings in the water, the faint part under it), and the game remembers that on that computer. "🌀 moving scenery and water on a slow computer" in ⚙️ Advanced still keeps everything moving, and says why the computer was found slow.
- **🧪 this computer's speed measures the duck pond too:** after the empty room, the friend swims across the pond with it moving, held still, and with its music, and the line ends with a block of numbers (the screen, the browser, each pass's frames, pauses and drawing times) to compare two computers. Still with the friend as they start, and everything put back after.

## 0.0.83 (2026-10-16)

Everything in 0.0.82, plus places that hold still on a slow computer.

- **On a slow computer, the places hold still** (Matthew: "moving scenery off on a slow computer"): the game times how long it takes to draw the friend, and where that is over 6 ms on average (the black laptop, not Matthew's laptop, where it is about 1), each place's own animations stop (the ducks, the sun's rays, the fumes, the fairies' wings), the rings round the friend in water hold still, and the part of the friend under the water is hidden instead of faint. The friend still moves and animates as always. On a computer six times slower than Matthew's, swimming in the 🦆 duck pond went from 17 frames a second to 37, and dragging to 52.
- 🧪 this computer's speed measures with the friend as they start and the music off (Matthew: the same on every computer), then puts your friend back. The 📋 chip says "copy" again for each new measurement, and "📝 then paste it here" opens a shared page to paste the line into, so it can be read from another computer (Matthew: the black laptop has no easy way to send it).
- In ⚙️ Advanced, "🌀 moving scenery and water on a slow computer" (off to start) keeps everything moving anyway, and its row says whether this computer was found slow and how long a drawing takes. The game remembers it on that computer and keeps checking.

## 0.0.82 (2026-10-16)

Everything in 0.0.81, plus a speed check for comparing computers.

- **🧪 this computer's speed**, in the ⚙️ Advanced tab (Matthew): about 5 seconds in the empty room with your friend as they are, then everything back as it was. It draws the friend 60 times and says how long one drawing takes, then walks them across the room and in place, and says how many frames a second each managed and the longest pause. Run it on two computers (Matthew's laptop and the black laptop) and compare the lines: the ratio of the drawing times is how much slower one is, which sets how the game is tested for a slow computer. 📋 copy puts the line on the clipboard.

## 0.0.81 (2026-10-16)

Everything in 0.0.80, plus walking on a slow computer.

- **Fixed:** on a slow computer (the black laptop), walking with a controller, the one on the screen, or dragging the friend was slow in every place, while animations with no background looked fine. Each step did much more than an animation does: it refreshed every control beside the picture and saved the friend, 60 times a second. Now a step only draws the friend, and the rest happens a few times a second. On a computer too slow to draw the friend 60 times a second, they are drawn 30 times a second while they move, as animations with no background always were, and the picture stays smooth. Tried on Matthew's laptop slowed to a sixth of its speed: 🔲 the empty room walks at 58 to 60 frames a second.
- The duck pond is still slow on a slow computer: its drawing is about a thousand shapes, and the browser goes over all of them every time anything moves. That needs a bigger change, which is next.

## 0.0.80 (2026-10-16)

Everything in 0.0.79, plus a see-through friend smooth to walk with.

- **Fixed:** a friend made see-through (👻 Visibility) made walking choppy everywhere, nearly frozen on Matthew's laptop at the 🏖️ beach and in the 🔲 empty room (about 35 frames a second here). Their see-through layers were drawn with masks the browser drew again on every step. Now each layer is see-through on its own, at 60: where two layers of one part overlap, the one behind shows a little (the hair is a little darker where it goes behind and in front of the head, and the legs show through see-through pants). The exact look is "👻 exact see-through" in the ⚙️ Advanced tab, off to start.

## 0.0.79 (2026-10-16)

Everything in 0.0.78, plus the living room smooth to walk in, and the music full screen.

- **Fixed:** walking in the 🛋️ living room was choppy (about 50 frames a second, worse on a slower computer): its framed pictures were each a whole place drawn live, every animation in them playing. Now the ⚙️ Advanced tab's "🎞️ moving pictures" covers every framed picture, not only the imagination's map, and is off to start: each picture is its place held still. The window above the couch still shows the lake moving outside.
- **⛶ the music, full screen** (Matthew), first in the 🎵 Music tab: the tune, where it is, the kinds of music and the mixing board take the whole window, and the whole screen where the browser allows. "✕ back to the game", or Escape, brings the friend back.
- **Fixed:** after ↺ Start over or 📂 Load, the controller on the screen had the usual game controller's buttons (✨ on L, crawling on R, run on X) instead of its own. Now it gets its own back.

## 0.0.78 (2026-10-16)

Everything in 0.0.77, plus a ⚙️ Advanced tab, and 💭 our imagination smooth to walk in.

- **⚙️ Advanced** (Matthew), a new tab in place of the GitHub chip: the game's code on GitHub; the version, with the 🆕 chip to change to a newer one when there is one (on a phone, where the title is hidden, this is the only place to see them); how many frames a second the picture is drawing now, and how long a drawing takes; and the things a background can draw that slow it down, each kept off until it can be made fast. Matthew: the priority is 60 frames a second at all times, for everything the controller does.
  - **🌫️ soft blurs**, off: the soft edges in pictures, the waterfall's mist and the leaves seen outside the cave. Off, pictures are sharp, and the mist is a soft fade instead. Switched on, it says how many blurs the place has.
  - **🎞️ moving pictures in the imagination's map**, off: the 22 pictures on the imagination's wall, each its own place still, instead of all of them moving at once.
  - **📏 measure here**, by each: the friend walks right and back for 2 seconds with it off and 2 with it on, and it says how many frames a second each managed, on this computer. Handy on the black laptop.
- **Fixed:** walking or dragging the friend in 💭 our imagination was very slow, 18 frames a second on Matthew's laptop and worse on the black laptop. Now it is 60.

## 0.0.77 (2026-10-16)

Everything in 0.0.76, plus an empty room, and dragging smooth everywhere.

- **🔲 empty room** (Matthew): a new place, next to 💭 our imagination. It is the imagination's room with nothing in it: a white wall, a pale floor and the line where they meet, and nothing moving but the friend. It is for trying the controls and the moves on an old, slow computer (the black laptop), where the busier places are too slow. Its back wall is solid.
- **Fixed:** dragging the friend was choppy in busy places on a big screen: about 47 frames a second round the 🏰 castle, 55 in the 🧙 witch's woods. Now both stay at 60. The friend looks the same as before; their arms are drawn over the hair and face in a way the computer can redraw much faster.
- A hooded costume seen from the side is drawn the same faster way.

## 0.0.76 (2026-10-16)

Everything in 0.0.75, plus smooth walking in the castle's moat and swimming in the duck pond.

- **Fixed:** walking or swimming in water was choppy on a big screen. In the 🏰 castle's moat the picture dropped to about 49 frames a second, and in the 🦆 duck pond to about 52 (Matthew found it walking round the castle with the controller). Now both stay at 60. The friend in the water looks the same as before: whole above the waterline, faint below it.
- **Fixed:** the whole picture was painted again every time the friend moved, even where nothing had changed, which slowed walking in busy places like the castle.
- Dragging the friend round the castle is a little smoother than before (about 49 frames a second, from 46), but not yet as smooth as the other places. That is next.

## 0.0.75 (2026-10-16)

Everything in 0.0.74, plus climbing up to the imagination's pictures and going into them with X.

- **🪜 ladders in 💭 our imagination** (Matthew): a ladder floats beside each column of pictures, bobbing gently and slowly. Walk, run or crawl the friend to one, and press ⬆️ at its foot to climb it. ⬆️ and ⬇️ climb up and down (hold 🏃 run to climb faster), and ⬇️ at the bottom, or an arrow to the side there, steps off. The 🧗 climbing chip climbs them too.
- **The imagination's back wall is solid:** the friend can't walk into it, so the ladders are the way up.
- **✨ into the picture that glows** (Matthew): with a controller (a real one or the one on the screen), the picture nearest the friend glows when they are close enough to reach it, on a ladder or standing in front of it, in any place with pictures or windows. Press X to go into it, with the spin, as clicking it does. A controller taught before this version gets ✨ on its left shoulder (L) when that button is free; teach it again (🎓) to put ✨ on X. On the screen's controller it is X.

## 0.0.74 (2026-10-16)

Everything in 0.0.73, plus a controller on the screen, for playing with no game controller plugged in.

- **🎮 a controller on the screen** (Matthew: the iPad has no USB port for our controller, and one day, traveling, we may not have it with us). While no game controller is connected, a 🎮 button at the picture's bottom right shows one: the arrows, L and select in the screen's bottom left corner, and X, Y, A, B, R and start in the bottom right, laid out as on a SNES controller, to touch, or to click with a mouse. Each button shows what it does. It does everything a real controller does: the arrows walk the friend (diagonals too, sliding from one arrow to the next), A is the short jump, B the high one, Y crouches, L turns crawling on or off, select goes back and start goes through the nearest doorway. Hold R with an arrow to run, one thumb on each. Tap 🎮 again to hide it. The game remembers whether you showed it, on that device. The 🎮 Controls tab has the same button, and 🎯 set changes its buttons as it does a real controller's, kept apart from the real one's. When a real controller connects, the one on the screen goes away, and it comes back when the real one leaves.

## 0.0.73 (2026-10-16)

Everything in 0.0.72, plus teaching the game your controller.

- **🎓 teach me your controller** (Matthew), in the 🎮 Controls tab: the game asks you to press B, A, X, Y, L, R, select and start, one at a time (skip any your controller doesn't have). Then they're set up like a SNES game: A a short jump, B a high jump, Y crouch, R crawling on or off, select back, start through the nearest doorway, and X kept free for something new. Run stays where it was. A line shows your controller's button numbers, with a 📋 copy button. The game remembers each controller's buttons by its name, and puts them back whenever you plug it in.
- **🐛 crawling on or off** with one press (R): then the arrows crawl, and standing still they crouch. Press it again to stand up.

## 0.0.72 (2026-10-16)

Everything in 0.0.71, plus the witch's places smooth again.

- **Fixed: walking around the witch's hut was slow** (Matthew): the cauldron's fumes held the hut to about 20 to 30 frames a second (and the witch's woods to about 13), standing or walking. They look the same, and both places now run at 60 frames a second, in Chrome and in Firefox.

## 0.0.71 (2026-10-16)

Everything in 0.0.70, plus walking toward us and away that looks like walking.

- **Steps toward us and away** (Matthew): walking or running toward us or away from us, the friend no longer swings their legs out to the sides. Each leg in turn comes up as it steps, looking shorter as it swings toward us (or away).
- **Feet seen from the front, or from behind**, while they walk toward us or away: sneakers show their toes, laces and white rubber toe, or their heels; bare feet show their toes pointing at us, or their heels. As a foot lifts you see its sole: a sneaker's white treads, or a bare foot's paler sole and toe pads. For now this is the friend's own sneakers and bare feet; other shoes and costumes come later.

## 0.0.70 (2026-10-16)

Everything in 0.0.69, plus jumping in water.

- **🐸 The short jump, in deep water** (Matthew): like jumping for a ball in a pool: a quick dip, then up fast until their waist is out of the water, arms reaching up, and back in with a splash.
- **🦘 The high jump, in deep water** (Matthew): turned to the side, a dolphin's dive: they sink a little, put their arms out in front of their head, leap out of the water in an arc head first, and dive back in a little way ahead, then come back up. Facing us or away, they shoot straight up out of the water, arms overhead, and drop back in.
- Both make a splash where they leave the water and where they come back in, and both work with the game controller's jump buttons.

## 0.0.69 (2026-10-16)

Everything in 0.0.68, plus a game controller that plays like a video game.

- **The arrows walk** (Matthew): press an arrow and the friend walks that way, from standing or sitting (and gets up off a seat). The diagonals work too. Let go and they stop. They face the way they go: up is away from us, down is toward us, and on a diagonal they keep facing whichever of its two ways they faced already.
- **Four buttons** (on a SNES-style controller: A, B, X, Y): A a short, fast jump; B a high, slow jump; X runs, with an arrow (on its own it does nothing); Y crouches down, or crawls with an arrow. A jump goes on top of walking or running, and they land still going. Select goes back the way they came, and start goes through the nearest doorway.
- **In water**, walking is swimming, and running is swimming fast.
- **🎮 Controls** now lists these actions, each with its button, lit while you hold it. To change one, click "🎯 set" and press the button you want. Some controllers number their buttons their own way, and the tab says so.
- **🧎 crouching down**, a new animation in the 🧍 Body tab too.
- **Fixed:** in a crouch, and while getting ready to jump and landing, the friend floated above the ground with their legs folded up. Now they bend down with their feet on the ground.

## 0.0.68 (2026-10-16)

Everything in 0.0.67, plus two jumps.

- **🐸 short, fast jump** and **🦘 high, slow jump** (Matthew) in the 🧍 Body tab's animations, in place of the one jump. Each has its own sliders in "🐸🦘 the two jumps": how high (100 = a leg's length) and how fast. The short one starts a little under a third of a leg high and quick; the high one almost a leg's length high and slower.
- "🙌 held up" is the hug's slider's new emoji (the kangaroo is the high jump's now).

## 0.0.67 (2026-10-16)

Everything in 0.0.66, plus playing with a game controller.

- **🎮 Controls** (Matthew), a new tab, and a game controller to play with: plug one in (USB) or pair it, and press one of its buttons. Its arrows (or its left stick) move the friend around the place, as dragging them does, and turn them the way they go: ⬆️ up is away from us, into the place, and ⬇️ down is toward us. Its other buttons start the animations, and the friend moves at the animation's pace: walking is slow, running is fast. Press the same button again to stop. A jump goes on top of whatever they are doing. Two buttons are for going places: back the way they came, and through the nearest doorway.
- **The usual buttons** have every animation on them: Ⓐ jumping, Ⓑ running, Ⓧ walking, Ⓨ climbing, the shoulders crawling and swimming, the triggers climbing (with ladder) and still, select back, start through the nearest doorway. The 🎮 Controls tab lists each button with a menu to change what it does (the row lights up while you hold its button), and "↺ the usual buttons" puts them back.

## 0.0.66 (2026-10-16)

Everything in 0.0.65, plus jumping.

- **🐸 jumping (once)** (Matthew), a new animation in the 🧍 Body tab, the first that doesn't go round and round: the friend crouches with their arms back, jumps up with their arms up, comes down, and lands on bent knees. Then they stand still again. Their shadow stays on the ground while they're in the air.

## 0.0.65 (2026-10-16)

Everything in 0.0.64, plus the friend seen from behind.

- **⬆️ away from us** (Matthew), a fourth way to face in the 🧍 Body tab: the friend turns their back to us. We see the back of their head, covered by their hair, or by long hair down their back, curls, or an afro all round. A costume's hood is closed at the back, with its ears, mane or antlers still on it. Wings and tails are on their back, in front of them now. A ghost is a plain sheet from behind. Walking and running away from us, they step in place.
- **🧗 climbing** now faces the ladder, away from us.

## 0.0.64 (2026-10-16)

Everything in 0.0.63, plus climbing the ladders that are in a place.

- **🧗 climbing** (Matthew), a new animation in the 🧍 Body tab: the friend goes to the nearest ladder in the place and climbs it. Up a dungeon's ladder, they climb out into the trap room. Up a ladder in a tree house room, they climb into the room above (or out to the tree). Up the tree's ladder from the ground, they climb into its house. Up the stable's ladder, they climb to the hay loft and hold on there. Put them down by another ladder and they climb that one. Where there is no ladder, the caption under the picture says so.
- **🪜 climbing (with ladder)**: the old climbing, with a ladder of their own, has its new name.

## 0.0.63 (2026-10-16)

Everything in 0.0.62, plus the ladder seen from the side.

- **🪜 climbing a ladder, from the side** (Matthew): with the friend turned to the left or to the right, the ladder they climb now stands in front of them, seen side on: its two rails nearly one behind the other, and each rung seen end on. Their hands reach forward and up for it, and each knee comes up to the next rung. (Before, it stood behind them as if they faced us.)

## 0.0.62 (2026-10-16)

Everything in 0.0.61, plus legs that hug, and jumping up into a hug.

- **Legs in a hug** (Matthew): in a place, with the friend turned to the left or to the right, their far leg now goes behind whoever they stand against, as their far arm already did, and their near leg stays in front. So a friend hugging the witch can put a leg round her.
- **🤗 jumping up into a hug** (Matthew), a new pose in the 🧍 Body tab: the friend jumps up and is held, their legs wrapped round the one they hug (the near leg in front, the far one behind) and their arms round their neck. Facing us, they turn to the right first. "🦘 held up" says how high (100 = a leg's length), and their shadow stays on the floor. Drag them against someone to hug them. Surprise me never picks it, since a hug needs someone to hug.

## 0.0.61 (2026-10-16)

Everything in 0.0.60, plus the Body tab: poses, the way the friend faces, and animations.

- **🧍 Body** (Matthew), the first tab, three rows of chips:
  - **🤸 poses**: standing; sitting (legs down, eyes open); cross-legged, meditating; dancing, a new dance each time.
  - **↔️ facing**: toward us, to the left, to the right (moved here from the arms and legs tab, with sitting).
  - **🎬 animations**: still; 🚶 walking; 🏃 running; 🏊 swimming (treading water, anywhere); 🐛 crawling on hands and knees; 🪜 climbing a ladder. They move with the music. Walking, running and crawling turn the friend sideways; climbing turns them toward us, with a ladder behind them.

## 0.0.60 (2026-10-16)

Everything in 0.0.59, plus dragging the friend by the body again, and glows that show what a press will take.

- **Fixed: dragging the friend by the body grabbed an arm** (Matthew): in a place, where the friend is drawn small, a press anywhere near a hand or an elbow took the arm. Now a hand, a foot, an elbow or a knee is taken only when you press right on it, and anywhere else on the friend moves them.
- **✨ Glows under the mouse** (Matthew): point at the friend's body and it glows: a press will move them. Point at a hand or a foot (or an elbow or a knee) and its orb glows: a press will pose it.

## 0.0.59 (2026-10-16)

Everything in 0.0.58, plus the version in the title, and new versions offered.

- **🆕 New versions** (Matthew): the game's name now has its version and the day it came out after it. Every minute the game checks whether a newer version is out, and if there is one, a chip after the version says so ("🆕 0.0.60"). Nothing pops up: click the chip when you're ready, and the game changes to the new version, keeping your friend, the place and all your settings.

## 0.0.58 (2026-10-16)

Everything in 0.0.57, plus smoother dragging and posing, and music that keeps playing.

- **Faster dragging and posing** (Matthew: posing was very slow, dragging into water most of all): each drawing of the friend takes about half as long as before, in every place, and the picture is drawn once a frame however fast the mouse or pencil moves. Measured on the duck pond: a drag into the water 3.6 to 1.8 thousandths of a second a drawing in Chrome (4.8 to 2.4 in Firefox), posing a hand 2.9 to 1.7, treading water 4.0 to 2.3.
- **The pond keeps moving while you drag** (Matthew: the duck pond stopped animating): the ducks, the water and everything else in a place go on moving during a drag, instead of standing still until you let go.
- **Fixed: the music glitched during a drag**: notes are sent further ahead and more often, so a busy moment no longer skips them. The first press that starts the music no longer rebuilds the whole place in the middle of the drag.
- **Fixed on touch screens**: a drag the browser takes over (a scroll or a gesture) lets go of the friend properly.

## 0.0.57 (2026-10-15)

Everything in 0.0.56, plus poses, flowing water, and a hug.

- **🤸 Pose** (Matthew), a new tab, first: "🧍 standing, facing us" (the friend as they start); "🧘 cross-legged, meditating", sitting on the ground with their legs crossed, their feet resting up on the other leg, their hands together in their lap and their eyes closed; and "💃 dance", a different dance each time (disco, ballet, hooray, robot, a big kick).
- **😌 Eyes closed** (Matthew, for meditating): in the eyes tab, the eyes shut, calm lids curving down.
- **👣 Feet turn** (Matthew): the feet turn at the ankle, toes down or up, as hands turn ("👣 feet turn" in the neck, arms & legs tab, and a pose turns each foot its own way). A foot resting up on the other leg is drawn over the pants.
- **Shorts and swimsuits fit posed legs** (Matthew): swim trunks, briefs, and the skin under any swimsuit or underwear now follow each leg when it is posed or the knees bend, facing us and from the side, as long pants already did.
- **A hug** (Matthew): turned sideways in a place, the friend's far arm is a body's depth behind them, so standing just in front of someone, one arm goes round in front and the other behind them.
- **The stream and the brook flow** (Matthew): the ripples on the tree house's stream and the cemetery's brook drift down toward you, all the time.
- **The swing swings its sitter** (Matthew): sitting on the tree house's swing, the friend swings with it.
- **Fixed: the sick puddle from the side** (Matthew): turned sideways and very sick, the puddle was cut in half and pushed to one side. Now it is whole, under where it falls, a side view of it, narrower than from the front.

## 0.0.56 (2026-10-15)

Everything in 0.0.55, plus treading water with the arms in the water.

- **Treading water, deeper** (Matthew): treading in deep water, the friend is in up to their chest, and their arms reach down into the water and sweep out and in under the surface, instead of waving above it. Friends with very long arms still tread with the water at their waist.

## 0.0.55 (2026-10-15)

Everything in 0.0.54, plus smooth witch's places.

- **Fixed: the witch's woods and the witch's hut were jerky, even with nothing moving them.** The green and blue fumes rising from the cauldrons were made of hundreds of little pieces, each one changing its thickness and see-through-ness on its own, and some browsers spent every frame working all of them out again. Now those changes play a cheaper way, and look the same. Measured with nothing being dragged, in Chrome: the witch's hut went from 13 frames a second to a steady 60, and the witch's woods from 17 to 43; dragging the friend got quicker too (the hut 1.7 s to 1.2 s for the same drag, the woods 2.6 s to 2.4 s).

## 0.0.54 (2026-10-15)

Everything in 0.0.53, plus places to sit.

- **🪑 Sit on things** (Matthew, for the sitting pose): drag the friend onto a seat and let go, and they sit on it; drag them off, and they stand up. Or use the "🪑 sit on" chips in the background tab, one for each seat in the place, and "🧍 stand up". Seated, the friend sits on the seat's top, among the place's things at the seat's depth, at their usual size there, facing a way that seat allows (toward the fire on the camp's logs, toward the table in the healer's hut and at the castle's dining table).
- **Where:** the living room's couch; the healer's two chairs; the castle's dining chairs, the king's throne, the cat room's window seat, the spiral staircase's steps and the stable's hay heap; the tree house's beanbag; the logs by the camp fire; the beach chairs; the picnic bench under the sea; the biggest toadstools in the fairy ring; the mushrooms on the path through the woods; rocks on the mountain trail; the crate and barrels by the castle; the house's front step and the toy wagon; the tree house's swing; and the witch's big root.

## 0.0.53 (2026-10-15)

Everything in 0.0.52, plus ears, and wings seen from the side.

- **👂 Ears** (M asked for them): the friend has ears now, facing us and from the side, in their skin's color. Choose them in the face tab, "👂 ears": "lobe hanging free", "lobe attached", "🧝 elf or fairy, pointy", or "none"; "👂 ear size" makes them bigger or smaller. Long hair covers ears, but elf ears poke out through any hair. A costume's hood covers them.
- **Wings seen from the side** (Matthew): turned sideways, a fairy's wings, and a falcon's or eagle's, are seen in perspective, spreading out behind the friend, the far wing just behind the near one. A bird suit's wings, along the arms, are seen nearly edge on.

## 0.0.52 (2026-10-15)

Everything in 0.0.51, plus the friend turning sideways and sitting down.

- **↔️ Facing:** a new choice in the "Neck, arms & legs" tab: "🧍 toward us", "⬅️ to the left" or "➡️ to the right" (Matthew). Turned to the side, the friend is seen in profile: one eye and brow at the front of the face, the mouth at the front edge (a smile turns up toward the cheek), a nose sticking out if they have one, and an ear. Their hair is drawn from the side too, every style: long hair down their back, curls, the afro's puff, spikes, a pigtail. Their arms and legs point the way they face, the near arm in front of them and the far one behind, and their shoes point forward. Animal costumes' hoods are open at the front with the animal's face on the brim, tails swing out behind, and a witch's hat bends back.
- **🪑 Sitting:** a new switch in the same tab (Matthew). Facing us, the friend's knees come toward you and their shins hang down; turned to the side, their thighs go forward and their shins hang down, as on a chair. Posing by dragging still works, sideways and sitting.
- Facing us and standing, the friend looks exactly as before. Places to sit (the couch, chairs, logs, benches), and other people sitting, come next.

## 0.0.51 (2026-10-15)

Everything in 0.0.50, plus the shore in front of the friend in the water.

- **Fixed: legs under the water showed through the sand** (Matthew). Since 0.0.50 the friend sinks into the water where they stand, and near the shore their legs, under the water, reached down over the sand, the bank or the grass in front of them. Now what is under the water shows only where the water is: the sand at the lake and the beach, the bank of the jungle pool, the grass by a castle's moat, and the banks of the stream and the brook are in front of it. The ripples stay on the water too.

## 0.0.50 (2026-10-15)

Everything in 0.0.49, plus the friend standing in the water rather than on it.

- **Fixed: the ripples floated above the water** (Matthew). In water, the friend's feet stayed where they stood and the water came up to their waist, so the ripples round their waist hung half a body above the water they were standing in, and over the trees when they stood far back. Now the friend sinks into the water where they stand: the water's surface meets their waist (their knees in the stream and the brook), the ripples spread on the water, and what is under the water is see-through below it. Their shadow on the ground is gone while they are in the water.

## 0.0.49 (2026-10-15)

Everything in 0.0.48, plus treading water.

- **🏊 The friend treads water where it's deep** (Matthew): in the lake, the sea at the beach, the pool under the jungle waterfall and a castle's moat, the friend's arms sweep out and in, their legs kick one after the other under the water, and they bob gently, in time with the music. In the shallow stream by the tree house and the brook in the cemetery they just stand, as before.
- A new slider in the background tab, shown in places with deep water: "🏊 treading water where it's deep" (100 to start; 0 to stand still, up to 200 for big strokes). A pose you give the friend stays, with the strokes on top; while you drag a hand or a foot, the strokes wait.

## 0.0.48 (2026-10-15)

Everything in 0.0.47, plus wading: the friend standing in water is in it.

- **🌊 The friend in the water** (Matthew): put or drag the friend into water, and they stand in it. Below the water they are see-through, so you see the water through them, and a wake spreads round them: rings of ripples moving out and fading, like those at the foot of the jungle waterfall, and foam going round where the water meets them. The near half of each ripple is in front of the friend and the far half behind. Step back onto the shore, and they look just as before.
- **Where there is water to stand in:** the lake (not its little beach), the sea at the beach, the pool under the jungle waterfall, a castle's moat (on our side of the castle, and not on the drawbridge), the stream by the tree house, and the brook in the cemetery. The lake, the sea, the pool and the moat come up to the friend's waist; the stream and the brook are shallow, and come up to their knees. In a narrow moat or stream the ripples stay on the water.
- **Two new sliders in the background tab,** shown only in places with water: "🌊 see-through under the water" (70 to start; 100 hides what is under the water, 0 shows it all) and "💧 a wake round the friend in the water" (100 to start; 0 for none, up to 250 for a big one).
- Treading water, for deep water, will come later.

## 0.0.47 (2026-10-15)

Everything in 0.0.46, plus posing the friend by dragging their hands, feet, elbows and knees.

- **🤸 Pose the friend by dragging** (Matthew, for M, who poses characters this way in a game on her iPad): press on one of the friend's hands, feet, elbows or knees, with a finger or the mouse, and drag it. A hand or a foot follows your finger, and its arm or leg bends at the elbow or knee to reach it. An elbow or a knee swings the whole arm or leg round from the shoulder or hip. Each arm and leg is posed on its own, so the friend can wave with one hand, stand on one leg, kick, or kneel. With a leg lifted, the friend stands on the other foot; with a knee lowest, they kneel on it. Pants follow each leg as it bends, and shoes, sleeves, a witch's stockings, a falcon's wings and a ghost's sheet follow too.
- Press anywhere else on the friend to move them around the place, as before, or on their face to change their smile and brows.
- The "💪 elbows bend", "🦵 knees bend" and arms sliders still move both arms or both legs together, on top of the pose. The body tab says how to pose, and when the friend is posed, a "🧍 stand straight again" button there puts every arm and leg back. A friend who was never posed looks exactly as before.

## 0.0.46 (2026-10-15)

Everything in 0.0.45, plus depth: the friend steps behind whatever stands nearer and is smaller farther back, each place lists what stands in it, and the friend has elbows and knees.

- **🧍 The friend steps behind things** (Matthew): every place has depth now. Wherever you put or drag the friend, everything standing nearer than their feet is in front of them, and everything farther off is behind them. Walk them behind a tree in the forest, the couch in the living room, the trunk of the tree house, the bars of the deep dungeon's cell, the cooks in the kitchen or the castle's barrels. Things lying flat (rugs, paths, flowers) and what is far away (the sky, the hills, a room's back wall) stay behind the friend.
- **The friend is smaller farther back** (Matthew): as the friend goes back into a place they get smaller, and coming toward you they get bigger, at the rate the place itself is drawn. They are the place's usual size where they arrive, and the "🧍 size in the scene" slider still sets their size on top of that.
- **👁️ What stands here:** each place's background tab lists everything standing in it, from the back to the front, as small chips, with 🧍 where the friend is among them. Tap a chip to hide that thing (🙈), and tap it again to bring it back (👁️). The game remembers what you hid, place by place (each castle room style, and each room of the tree house, on its own). The darkness of the night or of a dark room, and the light of a fire or candles, are on the list too, so you can turn the dark off. The whole list, for every place, is in [OBJECTS.md](OBJECTS.md).
- **The friend is lit like the place:** at night, and in the dark smithy, dining room and spiral staircase, the dark is over the friend too, wherever they stand (before, it was only when they stood behind something). At the camp at night, the fire makes a pool of light round itself, and the friend standing near the fire is lit by it.
- **A picture of a place shows it the same way:** a picture on a wall, or the view out of a window, draws the place's things in the same order as the place itself.
- **💪 Elbows and 🦵 knees:** two new sliders in the body tab. "💪 elbows bend" bends both arms at the elbows, down or up, and "🦵 knees bend" bends both knees out, so the friend crouches. Sleeves, pants, stockings and skirts follow the bend. At 0 the friend looks exactly as before. These are a start on posing the friend, and on moves like walking, climbing and dancing.
- **Some places look a little different,** where a nearer thing used to be painted under a farther one: the yard's two trees stand in front of the house's corners, the tree house's near bushes in front of its roots, the greenhouse's palm trunks in front of the ferns, and the beach umbrellas' poles in front of the chairs. The butterflies fly behind the nearer trees.
- Measured with a real drag of 60 moves in every place, with the music playing: no place builds its scene again during a drag, as in 0.0.45, and every place drags as fast as in 0.0.45 or faster (our imagination 2.7 seconds, was 3.4; the witch's woods 1.5, was 2.3; the witch's hut 1.4, was 1.8).

## 0.0.45 (2026-10-15)

Everything in 0.0.44, plus dragging the friend smoothly again, while the scene moves with the music.

- **Fixed: dragging the friend is smooth again, and the music keeps playing** (Matthew). Since the scenes began moving with the music (0.0.35), a drag in a scene went slower and slower, and could stall the music. The picture's clock (its animations') and the music's clock run at the same speed, but the picture's moves on only once a frame. Read afresh at every redraw, the gap between them wobbled, so the scene's animations came out timed a little differently each time, and the whole scene was built again on every move of the drag. Each slow frame made the next one worse; holding the scene's animations still during a drag (0.0.24) made it worse still, since the picture's clock stopped while the music's ran on. Now the gap is measured once and kept, and measured again only when it truly moves (the music starting, stopping or jumping to another place in the tune). Measured with a real drag of 60 moves in every place, at the scene's defaults with the music playing:
  - no place builds its scene again, during a drag or after it (in 0.0.44, every move did);
  - the drag took: in our imagination, 20.6 seconds, now 3.4; in the witch's woods, 5.0, now 2.3; in the living room, 6.7, now 1.6; in the witch's hut, 7.8, now 1.8; inside the tree house, 3.8, now 1.1;
  - every other place at a steady 60 frames a second.
- **The scene's animations, held still during a drag, go on from where the music is when you let go,** instead of from where they stopped, so whatever moves with the music is in time with it again at once. (A fairy's wings still beat while you drag her, as before.)

## 0.0.44 (2026-10-15)

Everything in 0.0.43, plus doors you can see in the hallway and the dining room, and a picture as big as your screen allows.

- **🚪 The hallway's and the dining room's doors stand closer** (Matthew and M): their doors were spread down the whole corridor, so the far ones were tiny and their signs too small to read. Now the doors stand close together near you, a pair after a pair, the torches between them, and every sign is big enough to read even when the game isn't full screen (the far ones about twice the size they were). Where a farther door's sign would cover a nearer one's, it goes up above it. The stairs' sign at the hallway's far end, and the dining room's far door's sign, are bigger too.
- **🖼️ The picture as big as it can be:** the game uses the whole window now, with no wide margins either side. The picture is as large as its side of the window allows, the caption and buttons in one row right under it, and the title moves to the top of the controls.
- **↔️ A divider to drag** between the picture and the controls: drag it right for a bigger picture, left for more room for the controls. The game remembers where you left it; double-click it to go back to the best fit.
- **💾 Save is a menu:** the kinds of file to save, the Save button, and the links to what you saved, in a box that opens over the picture, so they no longer take room under it. (The tip about dragging the mouth and eyebrows shows when you point at the caption.)

## 0.0.43 (2026-10-15)

Everything in 0.0.42, plus more doors in the hallway and the dining room.

- **🚪 More doors, M's idea:** a hallway can have up to four doors in each side wall now (eight, one pair to each stretch of the corridor), and the dining room up to three a side (six). With the far wall and the near wall, that's ten ways out of a hallway (its stairs at the far end, and the door behind you) and eight out of the dining room. Rooms still start with three doors, and the slider stops at each room's most.
- **🚪 A door in the dining room's far wall,** to the left of the great fireplace: a new setting, "🚪 the door in its far wall leads to", out of the castle or to any room; it leads nowhere until you set it. The castle designer can use it for a way back.

## 0.0.42 (2026-10-15)

Everything in 0.0.41, plus a door behind you.

- **⬇️ A door behind you:** the bottom edge of a room's picture is a door we can't see, behind the friend, and now it can lead somewhere. In a castle hallway, the trap room and the greenhouse, a new setting, "⬇️ behind you (a door we can't see) leads to", sends it out of the castle or to any room; it leads nowhere until you set it. When it leads somewhere, a sign at the bottom of the picture points down to it, and a click on the sign goes there. The living room and each tree house room have the same setting, leading to any place in the game. (Rooms with a sign at the bottom already, like the king's room, have that sign as their door behind you; a dungeon has none, since a pit has no way in but from above.)
- **The castle designer uses it:** it can put a way back in a room's door behind you when the room has no other free exit. In N's castle, the greenhouse's archway into the trap room was one way with no fix; now "🔧 add a door back" fixes it, and the castle has no problems left.

## 0.0.41 (2026-10-15)

Everything in 0.0.40, plus the spiral staircase, where a hallway's stairs lead.

- **🌀 A spiral staircase, a new castle room:** a round tower room, cut away at the front like a doll's house, its stone wall curving round behind. In the middle a stone column rises, and the steps wind up round it, turn after turn, out of sight. Its doors come out onto the steps at different heights, one higher than the last ("🚪 doors in its walls", up to four, each leading where you say). Arrow slits let in beams of light, torches on the column light the way up, and its way out is a sign toward you. Now "🌀 the first spiral stairs", the hallway's stairs, lead somewhere: up to the castle's first spiral staircase, when it has one.

## 0.0.40 (2026-10-15)

Everything in 0.0.39, plus the castle's kitchen and smithy, and their people.

- **🍲 A kitchen, a new castle room:** a big warm room of stone under oak beams. A great hearth burns in the back wall, with onions, garlic and herbs hung along its mantel. In front of it a cauldron of soup hangs on its tripod over its own little fire, steaming, and a cook in a frilled white cap and a blue dress stirs it. There's a domed bread oven with its mouth aglow and loaves on its sill, shelves of blue-rimmed plates and crocks, and copper pots and pans hanging from a rack on the beam. Sacks of flour and barrels stand by the back wall. In front is a long worktable of bread, cheese, a cabbage and a bowl of apples, where a cook in a tall white hat chops carrots on a board, once a beat. The cooks keep time with the quick LFO, and the steam and the oven's glow with the slow one.
- **⚒️ A smithy, a new castle room,** dark and very rustic: rough stone black with soot under old timbers, lit only by the forge. The forge's coals glow and breathe, with a hood over it up to the chimney and its bellows working. The smith, bearded, in a leather apron, beats a glowing bar on the anvil on its stump, holding it with his tongs, and sparks fly as his hammer lands. There's a quench barrel, steaming, tongs, hammers and horseshoes on the wall, a rack of swords, a shield in the castle's colors, and a heap of coal. His hammer and the sparks keep time with the quick LFO, and the forge's glow and the bellows with the slow one.
- Both rooms have doors in their side walls and a way out toward you, as the king's room does. The cooks and the smith will join the cast of friends to come, with settings of their own.

## 0.0.39 (2026-10-15)

Everything in 0.0.38, plus room in the greenhouse for every way out of it.

- **🌿 The greenhouse has stone walls up its sides, and room for every way out.** With many rooms, its archways crowded together along its back wall, one over another. Now the back wall holds six archways, three either side of its middle. Any more are doors in its side walls, two to a side, set in perspective (as the king's room's are), and those walls are stone now, up to above the doors, with the glass above them. Any more still are signs at the bottom, pointing toward you ("👑 13 ⬇️"). Matthew's idea.

## 0.0.38 (2026-10-15)

Everything in 0.0.37, plus M's dining room.

- **🍽️ A dining room, a new castle room (M's idea, the first of "M's rooms"):** a long hall, seen down its length as the hallway is, its floor of wooden boards. Down its middle runs a long table, set for as many as "🍽️ places at table" says: 4 to 18, one at each end and the rest down its two sides (eight to a side at most). The table grows longer with more places. It has a white cloth with a runner in the room's carpet color, gold at its edges. At each place there's a chair with a cushion, a plate, a gold cup, and a knife and fork. Candles burn down the table in brass sticks, and candelabras of candles hang over it on chains from the vault. Torches line the walls, and at the far end a great fireplace burns. The candle flames and the fire flicker with the quick LFO, and the torches and the fire glow with the slow one. Its doors run down its walls as the hallway's do, and its way out is a sign toward you.

## 0.0.37 (2026-10-15)

Everything in 0.0.36, plus the castle designer, each room's own way out, and castles of up to 13 rooms.

- **🗺️ The castle designer:** in a castle's rooms box, "🗺️ open the castle designer" shows the castle as a map: outside and each room as a box (with its emoji and number), and every way between them as an arrow (🚪 a door, 🪧 a sign, 🏛️ an archway, 🪜 a ladder, 🌀 stairs, 🕳️ a pit, dashed for a pit or a ladder). The room you are in is ringed in purple. Tap a room (on the map, or in the row of rooms under it) to see where its ways lead, or to go there; tap a way to choose where it leads. Doors are two-way to start with ("↔️ two-way doors"): when you choose where a way leads, the room it leads to gets a door back, if it has space for one. Under the map, a list of what looks wrong: a room that can't be reached from outside, a room with no way back out, a way to a room the castle doesn't have, a way with no way back. Each has a button saying how it would put it right (add a door back, lead it somewhere that has a way back, close the door), and "🔧 fix everything" does them all. Matthew's idea. (The map is drawn with Cytoscape.js, loaded the first time the designer opens.)
- **↩️ Each room's own way out:** the king's room's sign toward you, the trap room's archway, a dungeon's ladder and the greenhouse's archway out now lead where you say, room by room ("↩️ its way out leads to": "✨ the usual way", out of the castle, or any room). The usual way: out of the castle only from room 1, up to the trap room from a dungeon, and back to room 1 from any other room. Before, every one of these led out of the castle. A hallway's first door leads out only in room 1 too; elsewhere it leads to room 1.
- **🚪 Castles of up to 13 rooms** ("🚪 how many rooms", 1 to 13).

## 0.0.36 (2026-10-15)

Everything in 0.0.35, plus a hallway whose stairs lead nowhere ends in a wall.

- **🧱 A hallway whose stairs lead nowhere ends in a wall.** When "🌀 the stairs at its end lead to" is "🧱 nowhere", there are no stairs: the corridor ends in a stone wall, its courses meeting the side walls' at the corners, with a torch on it lighting it warmly. Before, the lit archway and its stairs were still there, leading nowhere. Matthew's idea.

## 0.0.35 (2026-10-15)

Everything in 0.0.34, plus the spooky waltz funked five ways; seven LFOs, with every place moving to the music; three new castle rooms (a cat room, a stable and a hallway), with the castle's cats and horses; our imagination, a new place; gems in the cave; D's castle; two color themes per castle; and many drawings fixed, inside the tree house most of all.

- **Fixed: inside the tree house, the wind is outside.** The tree's limbs inside each room had twigs and leaves fluttering in a breeze, and the house sign swung on its ropes, though no wind blows indoors and no leaves grow without sun. Now the limbs that pass through a room are bare and still, and the sign hangs still. Out of each window you see one of the tree's limbs, with branches forking off it and the tree house's own leaf pads on their tips: the limb sways in the wind with E, and its leaves flutter with D. Matthew caught this one.
- **Fixed: the hallway's banners hang from poles.** They hung face-on just off the walls, from nothing; now each hangs from a pole jutting out of the wall, braced back into it, so it faces down the corridor as it is drawn.
- **Fixed: the stable's horseshoes, saddles and bridle lie on their walls.** They were drawn face-on, as if stuck to the picture; now each is laid flat on its side wall in perspective, narrower and slanted as the wall runs toward us.
- **Fixed: the castle's gate, well and hay cart.** The portcullis's bars started below the top of its arch and had nothing across them; now they fill the arch, with bars across too. The well's stones and the hay cart's straws stay on the well and on the heap.
- **Fixed: the deep dungeon's hanging cage, and the greenhouse's monstera leaves.** The cage's bars stuck up past its dome or stopped short of it; now each starts on the dome. The monstera's slits started in the air beside the leaf; now each cuts in from the leaf's own edge.
- **Fixed: the cat room's window seat has three panels side by side,** no longer overlapping.
- **Fixed: the castle road's stones line its edges,** instead of wandering off into the meadow beside it.
- **Fixed: the healer's chimney is stone all the way up.** Its stonework stopped partway, leaving the chimney breast bare above the mantel, and a stone in every other row stuck out past its edge; now the stones run from the ceiling to the mantel, and stay on the breast.
- **Fixed: the castle's stonework stays behind its gate.** The wall's blocks were drawn over everything on it, crossing the gate, the portcullis, the windows, the banners and the torches, and one poked onto a tower. Now they are drawn first, and kept on the wall.
- **Fixed: the house's driveway joints run edge to edge,** instead of out onto the lawn on one side and short of the edge on the other.
- **Fixed: the healer's hut's roof, seen from the clearing.** Its thatch stuck up above the roof and its smoke rose from the air above it; the roof now peaks where the thatch meets, each line of thatch runs from the eaves to the peak, and the smoke comes out of its top.
- **Fixed: the castle rooms' cobwebs hang together.** In each top corner, a web's threads fanned out over the side wall while its rings hung on the back wall; now threads and rings make one web, in the corner.
- **Fixed: the fairy's wings' veins end at the wing's edge.** On the butterfly, dragonfly and leaf wings, veins ran off the wing into the air, or stopped well short of its edge, and two of the butterfly's spots floated beside it. Now every vein reaches the edge and stops there, and the spots sit on the wing.
- **Fixed: the tree house's reading lantern hangs from the roof.** Its cord stopped in mid-air, well below the sloping roof (the same kind of slip M spotted in the fairy forest). Now it runs up to the roof's edge, on a hook.
- **Fixed: the tents' mesh windows lie on their walls.** Each was a flat box with a square grid, as if stuck on the picture. Now its frame and mesh are drawn on the sloping canvas in perspective: threads along the wall run toward the far end as the seams do, threads across it slant as the far wall's edge does, and they close up toward the back. Matthew caught this one.
- **Fixed: the cat room window's leads stay inside its arch.** The window's lead lines ran up past the curve at its top onto the wall; now everything in the window is kept inside its arch.
- **Fixed: the castle rooms' floors stay on the floor.** In the stone rooms (the king's room, the trap room, the dungeons, the hallway, the stable) and the greenhouse, the lines across the floor ran on up the side walls. Now each ends where the floor meets the wall, and the rows close up toward the back, as paving does. The floor's and ceiling's far corners follow the walls' own lines, so no dark wedge shows at the bottom corners. Matthew caught this one.
- **🧙 The spooky waltz, funked, five ways (A to E, all five kept):** the spooky waltz, all of it (the theremin ghosts, the oom-pah-pah, the bell, the music box), quicker (205 beats a minute) and four half steps down, with a funk beat in 4. The beat has kick, snare, hats, a shaker, a cowbell and congas, plus bongos as the bongo breakdown's, with their own solo halfway. Under it all, a pair of wah-wah basses, buzzy and plucky: one low, popping its octaves, and one higher, answering it. Both keep to the waltz's chords, and the bass lines are louder than the waltz's own. The five differ in how the funk's big beats land on the waltz's:
  - A, four to a bar: a whole funk bar in each waltz bar;
  - B, half-time over two bars: the kick on one waltz bar's downbeat, the snare on the next's;
  - C, the waltz in 4: the waltz itself re-set in four;
  - D, the kick with the waltz: the snare and hats in 4 over it;
  - E, a shuffle over two bars: in the waltz's own triplets.
- **〰️ Seven LFOs, and every scene moves with the music.** The mixing board has four more LFOs: D, a quick one (once a beat to start with); E, a slow one (once in four bars); and F and G, for wings (F a wingbeat that looks like one, a quarter of a beat; G so fast it blurs, a sixteenth). Out of the box, they are the scenes' own. In every scene, its quick things move with D and its slow things with E; butterflies' wings beat with F:

  | Place | Quick things (D) | Slow things (E) |
  |---|---|---|
  | lake | the sun's rays, the ducks bobbing | the clouds |
  | forest, house, tree house, clearing, path through the woods | the butterflies bobbing (their wings with F) | sunbeams, clouds, the tree house's swing |
  | cemetery | the moths | the clouds |
  | under the sea | the fishes' tails | the seaweed swaying |
  | beach | the sand castles' flags | the palm fronds |
  | witch's woods | her cauldron fire | the bats gliding |
  | witch's hut | the cauldron fire | the crows' cages swinging |
  | healer's hut | the fire under the pot | the healer swaying |
  | living room | a new wall clock's pendulum | the lamp's glow |
  | inside the tree house | the leaves outside the window | the limbs outside the window swaying |
  | fairy forest | the fairy lights, the sun's rays, the stars | the lanterns swinging, the clouds |
  | mountain trail | the birds' wings | the clouds |
  | jungle waterfall | the parrots' wings | the vines swaying |
  | camp site | the campfire, the stars | the clouds, the fire's glow |
  | inside a tent | the campfire outside, the stars | the tent's light |
  | cave behind the falls | the gems sparkling | the crystals glowing |
  | castle | the pennants, the sun's rays | the clouds |
  | the castle's rooms with torches | the torches' flames | the torches' glow |
  | greenhouse | the glass parrots' wings | the clouds |
  | cat room | the fire | the cat toys swinging |
  | stable | the hens pecking | the dust in the sunbeam |
  | our imagination | the map's dotted paths | the room's glow |

  Things side by side each go their own way into the wave, never in step. That is only how they start:
  - D to G can be set like any LFO, and anything can follow any LFO.
  - Three new choices on the scene's panel say which LFO its quick things, its slow things and its wings follow: "〰️ quick things in the scene move with", "〰️ slow things in the scene move with" and "〰️ wings in the scene beat with". Or "their own pace", as before.
  - Each LFO's strip on the mixing board names the scene's things that follow it.
- **🕰️ A wall clock in the living room,** high on the right, its pendulum swinging.
- **🐈 A cat room, a new castle room:** a big, cozy wooden room. It has honey plank walls, beams overhead and a lantern, and a stone fireplace with a crackling fire, sparks rising from it. Over the mantel, with its candles and its little clock, hangs a portrait of a royal cat in a crown. There is a window seat under the evening sky (a crescent moon, twinkling stars), its curtains tied back. Two tall cat trees have carpeted platforms, sisal posts, a pompom swinging, and a cubby at the foot with eyes peeking out of it. Shelves step up the side walls to a high walkway across the back, with a cat flap at its end and a feather toy dangling from it, and a rope bridge crosses the room. On the floor are a big braided rug, bowls of fish and water, a basket, a box, balls of yarn, a toy mouse, and a mouse hole whose mouse peeks out now and then. Its doors are in its side walls, as the king's room's are, and its way out is a sign at the bottom. The rug, the curtains, the window seat and the cat trees take its color theme.
- **🐈 The castle's cats:** up to eight ("🐈 how many cats"), six to start with. Each has its own fur, tuxedo white, eyes and size, and it is one of:
  - 🐈 sitting, its tail hanging over a shelf's edge, or curled round its paws on the floor;
  - 😴 curled up asleep, breathing slowly, a "z" rising;
  - 🐾 walking to and fro.

  Where it is: by the fire, on either cat tree, on the high walkway, in the window seat, in the basket, on a shelf, on the mantel, on the rug, in the box, or on the rope bridge.
- **〰️ Each cat wags in its own time, never together.** A cat wags with an LFO, in time with the music ("〰️ its tail wags with"; the quick LFO D to start with), or at its own pace. Each starts its own way into the wag ("↪️ how far into the wag it starts"), spread out to begin with. A tail on an LFO too fast to see blurs, as the fairy's wings do. The mixing board's LFO strip lists the cats' tails that follow it.
- **🐴 A stable, a new castle room:** the castle's stone under timber.
  - Along the back wall, four stalls, each with a half door, its top door swung open, a brass name plate, a bucket, and a hay net inside.
  - Over the stalls, a hay loft stacked with bales, straw hanging off its edge, and a ladder up.
  - High in the wall, a round window. Sunbeams slant down from it through drifting dust, and swallows loop round the beams.
  - Horseshoes are nailed up for luck, and saddles and a bridle hang on the walls. A lantern hangs on the middle post.
  - On the floor: straw everywhere, a stone water trough that ripples, buckets, a heap of hay with a pitchfork in it, and two hens pecking about.
  - Its doors are in its side walls, as the king's room's are.
- **🐴 The castle's horses:** up to four ("🐴 how many horses"), three to start with, each looking out over its stall's door. Each has its own coat, mane and face marking (⭐ a star, ❕ a stripe, ⬜ a blaze, or none), and a halter in the castle's colors. A horse nods, looks round now and then, flicks an ear, blinks, and munches a wisp of hay. Each nods in its own time: with an LFO ("〰️ it nods with"; the slow LFO E to start with) or at its own pace, starting its own way into the nod. An empty stall has a blanket in the castle's colors over its door.
- **Fixed: the fairy forest's lanterns hang from branches.** M noticed that the two lanterns by the great tree hung from nothing. Now the tree has two branches reaching out from its trunk, each ending in a tuft of leaves, and each lantern's string is tied round one of them.
- **Fixed: the fairy house follows its LFO at once.** Before, changing its LFO's speed, shape or depth on the mixing board left the house fading as before, until something else in the fairy forest changed.
- **🕯️ A hallway, a new castle room:** a long corridor going off into the dark, lit only by its torches (the fairy forest's night over it, and no fireflies: they live outside), arches across its vault, and along its walls, by turns, torches and banners in the castle's flag colors with its owner's letter. A carpet runs from you all the way down it, in a color of the castle's own ("🟥 the hallway's carpet"). Its doors are set into its side walls in perspective, left and right, as a hand-painted adventure game draws them, each with its sign. At the far end, an archway onto the foot of a spiral staircase, lit from above; where it leads is the hallway's own setting ("🌀 the stairs at its end lead to": the first spiral stairs, any room, or nowhere), even the hallway itself: the friend flies up the stairs and lands back in it, as through a picture of the room you are in. The hallway is seen from a little higher than the other rooms, so its far end shows above the friend's head.
- **🚪 A castle is rooms by number, each with its own exits:** a hallway has up to four doors in its walls ("🚪 doors in its walls", 0 to 4), each leading where you say, out of the castle or to a room by its number (its own too), and its stairs at the far end lead where you say, so two hallways can join each other and still have their own doors to their own rooms. The king's room has its own doors too, set the same way: in its side walls in perspective, two to a side, clear of the throne, the banners and the suits of armor (now against the back wall), and its way out is a sign at the bottom, toward you, as the forest clearing's path (a door set to lead out is that sign). The other rooms still lead to all the rest. A door's choices list only the rooms the castle has, each with its own room's emoji (👑 room 1, 🕯️ room 2, 🕳️ room 3), as the go-to chips show them. On the castle's tab, "🚶 go straight to" chips take you outside or into any room by its number, and its settings are in dashed boxes: the castle's look, its rooms, and each room's own (a hallway's doors and stairs, a dungeon's skeletons, only where they apply).
- **Where you are, after how you feel:** the caption under the picture says it, with each place's own word: feeling happy in the 🌲 forest, at the 🏕️ camp site, on the 🥾 path through the woods, near the 🌳 tree house, outside of N's 🏰 castle, 🐠 under the sea, in the 🗝️ dungeon (room 4 of N's castle).
- **Inside a castle room, only that room's settings show**, marked as the room you are in; at the castle itself, every room's show.
- **💭 Our imagination, a new place,** first after none: a room of endless white, and on its far wall a map of every place there is, each a little framed picture of it that you can go through, a column to each kind of place (woods, water, camping, home, the witch's, castles), the places down it joined by a dotted path. Go there, and you can go anywhere.
- **Fixed: a picture takes you to its own place.** A picture showed its place's own signposts and doors, and a click on one of them took you where that one led (the camp site's picture to the mountain trail); now the whole picture is the way in (the living room's pictures too).
- **💎 Gems in the cave behind the falls:** gems all over its walls ("💎 gems on the walls", up to 40), each with a sparkle, and sparkles on its crystals, set like the night sky's stars: their shape, size (up to huge), brightness and twinkle.
- **💦 The jungle waterfall's speed and time of day:** "how fast it falls", and "🌗 time of day" (☀️ day, 🌇 sunset, 🌙 night, the moon out at night), the waterfall's own settings, so the falls and the view out of the cave behind them always agree. Its width shows in the cave too.
- **🏰 D's castle, as D chose it:** to start with, five levels, green-grey stone, 16 trees round it, a narrower moat, purple and green flags in stripes, and purple roofs.
- **🎨 Two color themes for each castle, A and B:** each its two colors, how they go (in turn, stripes, checkers) and a carpet color. A is the castle's own (its flags outside too); B starts as A with its two colors swapped and its carpet in A's second color. Each hallway and king's room takes one ("🎨 its colors"), rooms taking A and B by turns to start with, so two hallways or two king's rooms look different. The king's room's carpet is its theme's too, no longer always red.
- **Fixed: torch flames stay on their torches.** A torch's flame flickered by stretching about the corner of the picture, so the farther a torch was from the middle, the further its flame drifted off it (the dungeons' too).

## 0.0.34 (2026-10-15)

Everything in 0.0.33, plus LFOs faster than a beat, down to a 32nd of a beat, with fairy wings that blur when they flap faster than the screen can show.

- **⏱️ LFOs faster than a beat,** down to a 32nd of a beat. The "⏱️ round in" knob on the mixing board mirrors itself around 1 beat, now its middle: 1/32, 1/31 … 1/3, 1/2 beat below it, and 2, 3, 4, 5 … beats to 8 bars above it, every whole number of beats now (it used to skip 5, 7, 9 and others). A bongo or conga following a fast LFO bends all through each hit, in time with the tune, not just at its start: at a few to the beat it warbles, and near 1/32 beat (about 60 times a second) it buzzes. Slower LFOs sound as they did.
- **🧚 Wings too fast to see:** wings flapping with an LFO faster than the screen can draw (a cycle in under about four frames, around 1/8 beat or faster) show as fast wings look: a see-through blur across the whole sweep of the flap, thickest where the wave lingers, that keeps moving (eight see-through copies flap at the LFO's own speed, each an eighth of a cycle behind the last, so every frame catches them spread round the cycle, in a new place each time). A part fading with an LFO that fast (and the fairy house) holds at its average see-through-ness instead of flickering.

## 0.0.33 (2026-10-14)

Everything in 0.0.32, plus a ghost costume, the fairy forest, and the fairy in a shirt and pants.

- **👻 A ghost costume,** like the 👻 emoji but the friend's full size: a sheet over all of them, domed over the head, its sleeves along the arms as they are held (up, out or down) with round ends, a wavy hem, soft folds, and an "o" of a mouth. **How ragged** it is: from smooth round waves to torn points and slits. **Eyes:** none, one, two, or three (the third in the middle of the forehead, like the three-eyed smiley on the poster for the movie Evolution), drawn in the friend's own eye style, or, with **🕳️ hide eyes**, plain black holes like the emoji's. **The sheet's color,** off-white at first. And its **see-through-ness** right there on its panel: the Visibility tab's every-part chips and full-body slider, with the slider's LFO chip, so the whole ghost can fade in and out with the music.
- **🧚 The fairy forest,** a new background, reached by a "🧚 fairies" sign in the forest clearing: a glade in the woods at the time of day you pick (☀️ day, 🌅 sunrise, 🌇 sunset, 🌙 night), a ring of red-and-white toadstools in the grass (as many as you like, glowing a little at night), and a great old tree with a fairy house in it: a round green door between its roots, lit round windows with flower boxes, shelf-fungus steps winding up to a little balcony, lanterns, a chimney with smoke. The house shows as much as its slider says (0 hides it), and it can fade in and out with LFO A, B or C, in time with the music; the mixing board lists it. Fireflies at sunset and at night, sparkles by day; at night, the stars as the camp site's star settings say. A sign goes back to the clearing.
- **🧚 The fairy in a shirt and pants:** two chips, 👗 dress or 👕 shirt + pants. With pants, her tunic is a top, in the same five styles and colors, its hem a short frill at her hips (petals, leaves, bells, a sparkly zigzag, or a toadstool's, with its white frill), over pants a shade deeper than the tunic.

## 0.0.32 (2026-10-14)

Everything in 0.0.31, plus inside the healer's hut, three peaceful waltzes, a wreath of flowers for the fairy, an afro of its own shape, and bangs that sit under the witch's brim.

- **Fixed: the afro no longer becomes a beard.** It was one big circle, whose lower part reached down beside the cheeks to the jaw. Now it is an afro's own shape: a big round puff over the head, curly at its edge, ending at the ears, at every length.
- **Fixed: bangs under the witch's hat.** With big eyes (which make the head taller, and lift the hat with it), a band of bare forehead opened between the hat's brim and the bangs, and only bits of hair over the eyes showed. Now the bangs come from right under the brim, whatever the eyes' size.
- **🌸 A wreath of flowers for the fairy,** worn round her head on her forehead, above her brows, over her hair: a vine with leaves and six flowers, smaller toward the sides as the ring turns away. Its flowers in every color, or all of one kind: 🌸 pink roses, 🤍 daisies, 🌼 buttercups, 💙 forget-me-nots or 💜 violets. On by default; a toggle takes it off. When her head explodes, it goes off with the lid.
- **💃 Three peaceful waltzes,** in the waltz group: a 🏰 castle waltz (a flute over harpsichord oom-pah-pah, strings and bells), the castle's own tune now; a 👑 ballroom waltz (fiddles, harpsichord broken chords, a flute answering), for inside the castle; and a slow, warm 🌿 healer's waltz (vibes over soft chords, plucked oom-pah-pah, a music box), for inside the healer's hut. The witch's spooky waltz stays with the witch.
- **🌿 Inside the healer's hut,** a new background, and the healer's hut in the forest clearing is a doorway into it. It is warm: round cream walls under a thatched roof, herbs drying from the beam, a round window full of sun, a braided rug. A stone hearth with a fire, and on it a big pot of golden soup with carrots and herbs, steaming. Shelves and shelves of potions, neatly kept, each shelf its own kind of bottle in rainbow order. The healer's cat sits up on the top shelf, its tail hanging down and slowly swinging (its fur, tuxedo and eyes have their own colors). The healer herself is friendly: long auburn hair with flowers by her ears, a green dress and an apron with a heart, rosy cheeks, a smile; she sways gently from side to side and stirs the soup with a long wooden spoon. Drag her anywhere; her size has a slider. A worktable with a mortar and pestle, bowls, herbs, scissors, a knife and a little balance; a small table with two chairs, a teapot and cups, to sit and talk. The door goes back out to the clearing.

## 0.0.31 (2026-10-14)

Everything in 0.0.30, plus fairy wings that flap with the music's LFOs.

- **〰️ Wings that flap with the music:** the fairy's wing controls have an LFO chip (no LFO, A, B or C). With an LFO, the wings flap once per LFO cycle, in the shape of its wave (a triangle, a saw or a sine), in time with the music, and farther the deeper the LFO (from a gentle flap to a big one, more than the usual flutter). Changing the LFO on the mixing board changes the flapping at once, and the board lists 🧚 fairy wings among what that LFO drives; the wings keep flapping while you drag the fairy.

## 0.0.30 (2026-10-14)

Everything in 0.0.29, plus stars of many shapes and sizes that twinkle at the camp site, and a fairy who flies when you drag her.

- **🧚 Fly as a fairy:** a fairy's wings keep flapping while you drag her about, so she can fly (and the sparkle wings' stars keep twinkling). The rest of the picture's animations still hold still during a drag, as before, so dragging stays smooth: measured, the flapping costs nothing noticeable.
- **⭐ Stars at the camp site:** at night, new controls for the stars: their **shape** (• dots as before, ✦ four points, ★ five, ✶ six, ✴ eight, or 🎲 a mix, the new usual: the small ones dots, the bigger ones stars of every kind), their **size** on average (many small and a few big, from tiny up to huge), their **brightness** on average (the brightest with a soft glow), and how much they **twinkle**: the big bright ones turn a little, grow and shrink, and glimmer, each at its own pace, and the small ones blink. The night seen through the tent's door has the same stars. The moon is drawn over the stars now, so none shows through its dark side.

## 0.0.29 (2026-10-14)

Everything in 0.0.28, plus no skeletons in the castles unless you ask for them, castle colors, bangs in front of the face, a fairy, and parts that fade with the music's LFOs.

- **〰️ Three LFOs, shared:** the mixing board's LFOs are now three, A, B and C (D is gone; no tune used it), and with the whole tune's strip they make a square of four before the instruments. The same three are used everywhere: by the drums' pitch, and now by the friend's parts.
- **👻 Fading with the beat:** on the Visibility tab, each part has an LFO chip beside its name (no LFO, A, B, C): that part fades in and out with the LFO's wave, never past its slider, as far as the LFO's depth says (full depth fades it right out), in time with the music while it plays (else at the tune's tempo). The full-body slider has one too, which sets every part to the same LFO ("mixed LFOs" when they differ). Changing an LFO on the mixing board changes the fading at once.
- **🧚 A fairy costume:** wings in five styles (🦋 butterfly, 🪰 dragonfly, 🍃 leaf, 🌸 petal, ✨ sparkle, its stars twinkling), each with its own colors, fluttering (unless turned off); a tunic in five (🌸 petals, 🍃 leaves, 🔔 bluebell, ✨ sparkle with gold stars, 🍄 toadstool with white spots and a frill), each its own hem and colors; and shoes of the fairy's own: curly slippers, petal shoes, leaf wraps, sparkle boots, or bare feet with flower anklets. Her own hair, face and hands.
- **📌 Pin the full-body slider** to the full-screen controls, like any other slider.
- **🗝️ No skeletons unless you want them:** the castles' dungeons now have no skeletons or skulls at first; each dungeon room has its own slider for them, from none up to as many as its drawing has (two in the dungeon, four in the deep dungeon), the least scary first (in the deep one: the skull under the candle, then the one in the cell, the one in the cage, the one hanging). With none, the chains hang empty and the candle stands on a stone. The dungeon's sign is a key now, not a skull, and the trap room has a dropped sword by its pit instead of a bone.
- **🚩 Castle colors:** each castle's banners and flags are in two colors, its owner's choice, used in one of three ways: in turn (each flag one color, the next the other), stripes, or checkers; in the clearing's far view, at the castle, on the road's pennants, and in the king's room (its banners and shields). The owner's letter is always readable: dark on a light banner, white on a dark one, outlined in the other, and on a round badge of its own on stripes and checkers (M's letter now shows on her gold). **🔺 Roofs** in a color of each castle's own.
- **✂️ Bangs in front:** a fix for a bug since the first version: the whole hairdo was drawn behind the head, so its front, the fringe over the forehead, was hidden by the face (a see-through head showed it). Now the front of the hair comes over the face, the eyes and the brows, in locks of its own kind (pointed for straight hair, round for curly hair and an afro, a zigzag for spiky hair), parted for long hair. A new **bangs** slider on the Hair tab sets how far down it comes: 0 for none (the old look), 100 (the usual) for down to the brows, and past that over the brows and the eyes. When the head explodes, the bangs split along the crack, the top going off with the lid.

## 0.0.28 (2026-10-14)

Everything in 0.0.27, with the ducks swimming forward.

- **🦆 Ducks swim forward:** in 0.0.27 a duck paddled out and then came back the same way facing, as if swimming backwards. Now it swims out, pauses to turn round, and swims back facing where it goes, then turns again.

## 0.0.27 (2026-10-14)

Everything in 0.0.26, plus organic trees, bushes, grass and rocks in every scene, snow that sits on its mountains, and skies that move.

- **🌳 Trees, bushes, grass and rocks, grown as the tree house's are:** every broadleaf tree now has a trunk flaring at its roots, limbs forking up, and leaf pads at their tips (the tree house's own pad shapes), darker behind and lighter in front, each tree from its own seed; pines are tiers of drooping boughs; bushes are low mounds of leaf pads, some in flower; grass grows in tufts of curved blades; rocks are irregular stones with a lit face and a crack. No more lollipops or ice-cream scoops, in the woods, the clearing, the tree house's woods, the house's yard, the lake's far wood, the castle, the healer's grove, the cemetery, the waterfall's jungle, the path through the woods, the mountain trail, the witch's woods and under the sea. One set of drawings for every scene, for consistency.
- **🏔️ Snow that sits on the mountains:** each snow cap is cut from its own peak's slopes, with a ragged snow line, on the mountain trail and behind the castle (whose mountains are now a proper range).
- **☁️ Every cloud drifts,** slowly, each at its own pace; **🦆 the lake's ducks** paddle about, bobbing and rocking; **☀️ the sun's rays** turn slowly and breathe, at the lake and the castle.
- **🌤️ Sunbeams in the woods** (and on the path through them) come from one far, still sun: they run parallel, each its own width, and each dims and brightens as clouds go by, now and then nearly gone.
- **Fixed:** bushes no longer grow in the tree house's stream; castle banners hang from the battlements as long as the wall is tall, so a one-level castle's no longer dip into the moat.

## 0.0.26 (2026-10-14)

Everything in 0.0.25, plus backgrounds by kind of place, and three castles with their rooms.

- **🏞️ Backgrounds by kind of place,** as the music is by kind: the Background tab's long row of places is now two rows, each in a dashed box with its name: first the kind of place (⬜ no background, 🌲 woods, 💧 water, 🏕️ camping, 🏠 home, 🧙 the witch's), then that kind's places. Picking a kind goes to its first place, unless the place now is already of that kind; going through a doorway shows the new place's kind.
- **🏰 Three castles, N's, M's and D's:** each with its own settings: how many levels (1 to 5), its stone, how many trees grow round it, and how wide its moat and drawbridge are (none to very wide), its flags and banners in its owner's color with the owner's letter. The forest clearing shows the one picked ("🏰 whose castle"), far off and now a doorway; a new place, 🏰 castle, shows it up close: far snowy mountains and forest, the sun and birds, stonework, ivy, lit windows and torches, reeds and lily pads in the moat, a road with pennants, a hay cart, a well, barrels, flowers, framing oaks.
- **🚪 Castle rooms:** each castle has up to six rooms, each in a style, chosen in the Background tab; the gate leads into the first, and doors lead between them. N's room styles, for any castle: 👑 the king's room (a throne on a dais, a red carpet, banners, a stained-glass window, a chandelier, suits of armor, a chest of gold, and an open door onto the hallway outside); 🌿 the greenhouse (glass walls and roof, vines on every wall, glass parrots flying by the vines, palms, monstera, ferns, birds of paradise, hibiscus, bromeliads, and a fountain in the middle, its water running); 🕳️ the trap room (one doorway, and two spiked pits in the floor that drop into the dungeons); 💀 the dungeon and ⛓️ the deep dungeon (skeletons hanging on the walls, chains, a cage, a cell, glowing mushrooms, dripping water, and a ladder back up to the trap room).

## 0.0.25 (2026-10-14)

Everything in 0.0.24, with the full-body slider working as it should.

- **🧍 The full-body slider sets every part to its value:** moving it puts every part's visibility exactly where it is, wherever the parts were (in 0.0.24 it moved them by the same amount, keeping their differences). It keeps its own value; when a part is moved on its own, the parts no longer all match it, so it dims and says "mixed" until it is moved again. The "every part at once" chips set it too.
- **The chips in order:** 🙂 visible, 👻 ghost, 🫥 invisible.

## 0.0.24 (2026-10-14)

Everything in 0.0.23, plus a full-body visibility slider, a smooth drag past the cauldron, tree house size, and a tab icon.

- **🧍 A full-body slider:** in the 👻 Visibility tab, under the "every part at once" chips and above the parts, one slider moves every part's slider by the same amount, so the friend fades or comes back as a whole while each part keeps its own difference (each stops at invisible or fully there). It shows the most visible part. A drag moves the parts from where they were when it began, so dragging back puts them back as they were.
- **🧙 Dragging the friend past the cauldron is smooth again:** in the witch's woods and inside her hut, a drag of the friend crawled (60 moves took 8 seconds in the woods, with frames up to a quarter second apart). Now the scenery stays put while the friend is redrawn, instead of being taken out and put back on every move, and the scene's animations (the fumes, the bubbles) hold still while a drag is held, going on from where they stopped when you let go: the same drag takes 2 seconds, at a steady 60 frames a second, as in a scene with no fumes.
- **🏡 Tree house size:** in the 🌳 tree house, a slider grows its houses apart from the tree (50 to 250, as big as 2½ times), each about where it sits, with the rope bridges and ladders still meeting their platforms.
- **🐈‍⬛ A tab icon, drawn by the game itself:** the tree house with two cat houses, a black cat and an amber cat, at tree house size 250, rendered from the game's own picture and scaled down to a browser tab's sizes (16, 32 and 48 pixels; PNGs in the page itself, so the game stays one file).

## 0.0.23 (2026-10-14)

Everything in 0.0.22, plus a quiet link to the game's code.

- **The game's code, one chip away:** a quiet "GitHub ↗" chip after 🎵 Music, last in the row of tabs, opens the game's repository (matthewhammer/kids-choice-games) in a new tab, so the game keeps its place.

## 0.0.22 (2026-10-14)

Everything in 0.0.21, plus LFOs on the drums' pitch, and bongos that sing.

- **〰️ LFOs on the drums' pitch:** the mixing board has four LFOs, A to D, in a row before the instruments: each is a slow wave that keeps time with the tune, going once round in one beat up to eight bars, in a shape (△ triangle, ◿ saw or ∿ sine), so far up and down (its depth, up to an octave either way). Every drum with a 🎵 pitch knob (the bongos, the congas) has an LFO chip: tap it for none, LFO A, B, C or D, and its pitch follows that wave, a hit at a time. Kept for each tune with the friend, as the rest of the mix.
- **🪘 Bongos that sing:** the classic beats set their own LFOs: 🥁 bongo breakdown's high bongo rises and falls once a bar, and in the solo the high one talks, up and down every two beats, while the low one climbs over eight bars; 🏜️ canyon cruise's low bongo, a hit every dotted quarter, plays a little tune of its own that comes round every bar and a half; 🛹 skatepark strut's high bongo goes round every three beats, against the bar's four. And 🏖️ beach calypso's second conga sways.

## 0.0.21 (2026-10-14)

Everything in 0.0.20, plus inside the tents, a cave behind the falls, a path through the woods, a usual size for every place, a visibility panel, and the classic beats in threes.

- **⛺ Inside the tents:** at the camp site every tent is a doorway into its own inside, the same color as outside: the 🟧 orange one has sleeping bags and a flashlight, the 🟦 blue one a teddy bear, books and a lantern, the 🟩 green one an explorer's map, binoculars, a backpack and a jar of fireflies. Its door stands open onto the camp at the camp's own time of day, its fire lit or out, and is the way back out. (Also a background of its own, with a choice of which tent.)
- **🪨 The cave behind the falls:** the waterfall itself is a doorway into a cave you can't see from outside: stalactites dripping, crystals glowing, glowworms, bats asleep and an old chest. From inside you see the falls from behind, as wide and as misty as the waterfall's sliders make them, and they are the way back out.
- **🥾 The path through the woods:** the forest clearing's path toward you (its sign says "🥾 path ⬇️") leads to a new path through the woods, tall trunks, ferns and mushrooms, whose signpost goes back to the clearing or on to the mountain trail; the trail's signpost has a third arrow, "🌲 woods", back.
- **🧍 Landing on the ground:** through a doorway or by picking a background, a friend lands where they can stand: beside the campfire, not in it; on the bank of the waterfall's pool, not in the water; beside a tent's door or the cave's mouth, not in front of it.
- **🧍 A usual size for every place:** a friend in a background is as big as the place's things make sense beside them: small by the waterfall or the tree house, bigger in the living room or a tent. 100 on the size slider is that usual size in every place (it goes up to 1000); a friend saved before keeps the size they had.
- **👻 Visibility:** a new panel with a slider for how much of each part of the body shows (100 all of it, 0 none): 🙂 head, 👀 eyes, 💇 hair, 👕 shirt, 👖 pants, 👟 feet and 🖐 hands. Above them, three chips set every part at once: 🫥 invisible, 🙂 visible and 👻 ghost (every part half there). An invisible shirt is an invisible body (the neck and arms too), and invisible pants are invisible hips and legs: parts turn ghostly or vanish, the clothes never come off. The eyes go on their own, so an invisible head can keep its eyes, floating; things held or worn on the side (a witch's cat and broom) stay. A ghost is evenly see-through, even where a part is drawn twice (the arms go behind the hair and again over the face), and its shadow fades with it. 🎲 Surprise leaves what shows alone.
- **🎶 The music's chips in boxes:** the kinds of music and the kind's tunes each sit in a dashed box with its name ("kind of music", "tunes: 🥁 classic beats"), so the two rows read apart.
- **🥁 Classic beats in threes:** the three classic beats play triplets and cross-rhythms against their beat: the high bongo ends every other bar in triplets and the low one plays three over two beats (🥁 bongo breakdown, whose bongo solo goes from three to the beat to three over two beats to six to the beat); the bass runs up in triplets, the theme falls in a lazy triplet, and the low bongo hits every dotted quarter, three for every two of the band's (🏜️ canyon cruise); the hats roll in triplets and the organ plays three chords over two beats (🛹 skatepark strut); and every snare fill into a new section goes faster and faster in triplets. (Tunes can now be written in triplets: a note's length may be a fraction, and drum hits in parentheses share a beat.)

## 0.0.20 (2026-10-14)

Everything in 0.0.19, plus music by kind, three classic beats, and moving music by an interval.

- **🎶 Music by kind:** the Music tab picks a kind of music, then one of its tunes: 🥁 classic beats, 🌴 reggae, 🏖️ calypso, 🏠 house, 🕺 groove, 💃 waltz, 🏃 chase and 📖 storybook (or 🏞️ the scene's own tune). Picking a kind plays its first tune, unless the tune playing is already of that kind.
- **🥁 Three classic beats:** our own funk tunes in the style of the instrumentals hip-hop grew up on, about three minutes each: 🥁 bongo breakdown, 🏜️ canyon cruise and 🛹 skatepark strut. Each gives every player a turn in front: the bongos open it, then the bass guitar, the bongos again, the drum kit, the organ, the rhythm guitar and the lead guitar each take a solo (watch the sections under the tune's slider, and solo or mute them on the mixing board). New instruments for them: a bass guitar, bongos, an open hi-hat, a tonewheel organ with a spinning speaker, a funk guitar with a wah, and a twangy lead guitar with a whammy bar.
- **🎹 Move the tune by an interval:** under the transpose slider, ➖ | 🎵 interval | ➕. A tap on the interval goes to the next one, a half step, a whole step, a minor 3rd and so on up to the octave (where it starts); ➖ and ➕ move the slider by exactly that much.
- **🎚️ Move each instrument too:** on the mixing board, every instrument that plays notes has its own ➖ | 🎵 interval | ➕, and says how far it is moved: none, or up to three presses either way ("+2 × 5th"). Drums keep their sound.
- Part names on the mixing board read with spaces ("bass Solo", "break Kick").

## 0.0.19 (2026-10-13)

Everything in 0.0.18, plus the far views match the places they show.

- **🌲 The forest clearing shows the places as you made them:** the tree house is that very tree, small and far off: its houses, their animals and colors, its bridges, ladders, leaves, swing and size (and its crown, seen from afar), each house a doorway into its room. The family house is as big as its slider says. The witch's hut stands on chicken legs only when it has them, as long as they are, with skulls on its fence as big, as wide-eyed and as glowing as her woods' skulls.
- (Pictures on the walls were already drawn from the same settings as their scenes.)

## 0.0.18 (2026-10-13)

Everything in 0.0.17, plus three new places: a mountain trail, a jungle waterfall and a camp site.

- **⛰️ A mountain trail:** snowy peaks in ranges, a lake and a village far below, clouds drifting, birds circling, and the trail winding off uphill. Its signpost points to the falls and the camp (each arrow a doorway). Sliders: how high the peaks are, clouds, birds, and the lake and village (on or off).
- **💦 A jungle waterfall:** the water streaming down a mossy cliff into a pool, ripples spreading, mist rising at its foot, big-leaved trees, vines swaying, parrots and flowers; a sign back to the trail. Sliders: waterfall width, vines, mist, parrots.
- **🏕️ A camp site:** tents round a campfire, logs to sit on, a sign back to the trail. Controls:
  - the time of day: ☀️ day, 🌇 sunset or 🌙 night (stars twinkling and a moon);
  - the campfire lit or out (out, it smokes), and its flames' size, sparks flying up;
  - what is on the fire: nothing, 🌭 veggie sausages on sticks, or 🍲 a pot of soup hanging over it, steaming;
  - how many tents.
- **🌲 The forest clearing:** the paths run on the ground to each place's door or gate; the castle has a moat, its drawbridge down, and a low fence outside it; the witch's hut has a crooked fence; the healer's hut stands among trees.

## 0.0.17 (2026-10-13)

Everything in 0.0.16, plus a room inside every tree house, ways between them, inside the witch's hut, and a forest clearing.

- **🪜 A room inside each tree house, 1 to 6,** numbered as the houses are outside, a sign with its number and its animal hanging from the rafters. Each house's kind of room is its own, with two more kinds: 🎨 an **art studio** (an easel, paint jars, drawings pinned up) and 🥁 a **music room** (drums, a keyboard, records on the wall). Each room's two pictures are its own to choose.
- **Ways between the rooms,** where the houses are joined outside: a door in the side wall where a rope bridge goes out, a hatch in the floor down a ladder (to the house below, or 🌳 out to the ground), a ladder up through the ceiling where the house above comes down to this one. Each has a sign saying where it goes, and is a doorway.
- **The ladders join the houses:** house 1's goes down to house 5, house 2's to house 6, house 3's to house 1, house 4's to house 2 (to the ground when there is no house below).
- **Inside always matches outside,** whatever the numbers of houses, bridges and ladders (checked for all 252 ways): a bridge to a smaller house now reaches its platform, and you are always inside a house the tree has (going into house 6 grows the tree to six; fewer houses take you to the last one).
- **🧙 Inside the witch's hut:** click her hut in the witch's woods to go in. A cauldron bubbles over its fire, crows sit in hanging cages, frogs in jars, snakes in a glass tank, and the witch herself stands there: drag her anywhere. It matches her hut outside: the red door in the middle, a window either side, and a round window above, now in the hut's gable too. Both cauldrons, inside and out, bubble over a flickering fire, and their fumes rise as a double helix, one green and one blue, turning as they go up and out past the top of the picture. Sliders: the witch's size, the cauldron's size, crows, frogs, snakes. Her door goes back out to the woods.
- **🌲 A forest clearing,** with paths out to what you choose to see far off, each with its own switch: 🏠 the family house, 🌳 the tree house (with its houses as you chose them), 🧙 the witch's hut, 🏰 a castle on its hill, and 🌿 a healer's hut. The house, the tree house and the witch's hut are doorways into their scenes (the castle's and the healer's insides are still to come).
- Fixed: the pictures in the tree house rooms and the living room's window could not be clicked through (a dim or a pane of glass lay over them).

## 0.0.16 (2026-10-13)

Everything in 0.0.15, plus as many talons as you like.

- **🦅 Talons, 1 to 100,** like the toes, each foot its own: one talon back and the rest forward, fanned wider and thinner as there are more, each with its hooked claw (4 looks as it always did).

## 0.0.15 (2026-10-13)

Everything in 0.0.14, plus doorways between the scenes, feet as free as the hands, and tree houses in colors of your own.

- **🚪 Doorways into other scenes:** a picture on a wall, a window, a tree house and the house's front door glow under the pointer. Click one (or tap it), and the friend flies to it, shrinking to fit it and spinning, slowly and then fast, into its scene. They land where a friend starts, at the size they had.
  - the living room's pictures and its window go to the scenes they show;
  - inside the tree house, the pictures go to theirs, and the window back outside;
  - each tree house goes inside it, each to a room of its own (bunk room, kitchen, reading nook, lookout);
  - the house's front door goes into the living room.
  - after a trip, a **⬅️ back** chip on the picture goes back through the doorway, the friend spinning into it, to where they stood; after several trips, back one at a time. Choosing a scene from the menu starts afresh.

- **👟 Feet, like hands:**
  - a **"feet are different"** switch gives each foot a column of its own (the friend's right foot on our left, as with the hands), each with its own style and toes; the color and the size are both feet's;
  - **🦶 toes** and **👽 alien toes** each have a slider for how many, **1 to 100** (5 toes and 3 or 4 alien toes look as they always did; more fan round the foot, to a fringe at 100);
  - the picture's frame grows to hold toes that fan out below the feet;
  - saved friends with the old "5 toes", "4 toes" and "3 toes" feet come back the same.
- **🌳 Tree houses in colors of your own,** each house its own:
  - a 🐱 **cat** house starts as a **tuxedo cat** (black, a white muzzle and a white bib over the door); its fur, its white (or none) and its eyes are each house's;
  - a 🎀 **fancy house**'s walls, roof and trim, and a 🔨 **craftsman**'s walls, roof, trim and gable, start as they were and can change.

## 0.0.14 (2026-10-13)

Everything in 0.0.13, plus the great tree's crown.

- **🌳 The tree house's great tree has a crown,** high above the picture: its trunk goes on up, branches spread wide, and big bonsai pads of leaves sit on them. At the usual size it is out of sight; shrink the tree to see it. The leaves slider fills it too (0: bare branches).

## 0.0.13 (2026-10-13)

Everything in 0.0.12, plus a forest, a stream and a swing round the tree house, and three more kinds of tree house.

- **🌳 The tree house stands in a forest:** pines and round trees on the hills and at the meadow's edge, bushes, and flowers in the grass.
- **A stream** winds down past the tree, and **a rope swing** sways over it from a long branch.
- **Three more kinds of house** in each house's menu, beside the animals:
  - 🪵 **cabin:** a plain red gable and a chimney;
  - 🎀 **fancy house:** a steep scalloped roof, lacy trim, a gold finial, a round window and flower boxes;
  - 🔨 **craftsman:** a low wide gable with rafter tails and braces, and tapered porch posts on stone.

## 0.0.12 (2026-10-13)

Everything in 0.0.11, plus five new backgrounds with sliders of their own, and four chase tunes.

- **Five new backgrounds**, each with its own sliders (a size, a count, and for pictures, their detail):
  - 🦆 **Lake:** ducks, cattails at the water's edge, a little beach, thin grass on the dirt shore, sun and puffy clouds. Sliders: ducks, duck size, cattails, clouds, cloud size.
  - 🏠 **House and yard:** a brown house of two floors with the garage on its right, a driveway, a stone walk, a tree each side, bushes, flowers, and toys left on the lawn. Sliders: house size, tree size, toys, flowers.
  - 🌳 **Tree house:** one great tree, its trunk five trunks braided into one, with up to six houses, rope bridges across it and ladders down. Its leaves grow in bonsai pads on many branches. Each house is an animal (🦊 fox, 🦉 owl, 🐸 frog, 🐰 bunny, 🐻 bear, 🐱 cat): its roof is the animal's head, its face above the door, like a costume's hood; the lower houses stand in front. Sliders: houses, tree size, rope bridges, ladders, leaves, and each house's animal.
  - 🪜 **Inside the tree house:** a bunk room, a kitchen, a reading nook or a lookout. Each room has its own wood, floor and window. The tree's trunk comes up through the floor, its strands braided, and bare branches poke through the walls. It is dim inside. Sliders: room, window size, things in the room, pictures on the wall and their detail, and each picture's scene.
  - 🛋️ **Living room:** a couch in the middle, pillows on it, a lamp and a plant. Above the couch, a window with curtains looks out on a background of your choice, at its own detail. Up to six pictures hang on the wall, and each one shows a background of your choice. A picture of the living room holds its own pictures. Sliders: what is outside the window and its detail, couch size, pillows, pictures, picture detail (0 is a blur, 100 is all of it), picture size, and each picture's scene.
- **Four chase tunes,** layered synths with drums dropping out and crashing back in, made for the tempo slider (try 250%). Each is about 2 minutes:
  - 🌳 **Tree-house chase** (the tree house's own, inside and out);
  - 🦆 **Duck-pond dash** (the lake's);
  - 🛋️ **Couch-cushion rave** (the living room's);
  - 🏠 **Round-the-house chase** (the house's).
- **Two new instruments** on the mixing board: 🎛️ a dance lead (its knob: how bright) and ⚡ a laser zap (its knob: how far it falls).
- **Scenes are kept between moves:**
  - moving one of the figure's sliders no longer draws the scenery again;
  - a scene's own slider builds again only the parts it changes, and the page keeps the rest as they are: a size slider (the tree's, the house's, the couch's) changes one group's transform, and a count slider (ducks, toys, pillows) one part.
  - a picture of the living room on its own wall changes with the room (its pillows, its couch), as it should.

## 0.0.11 (2026-10-12)

Everything in 0.0.10, plus longer beach tunes, with bridges, breaks and new parts.

- **The beach tunes are longer, and change as they go:**
  - 🏖️ **Beach calypso** (about 2½ minutes, 10 sections):
    - a third steel-pan tune over a bridge of its own (A minor, F, C, G), with its own strum and bass;
    - a second pan breaking the chords under the tune;
    - a higher conga answering the first, and a carnival whistle;
    - a break of pans and hand drums only, and the last stretch a whole step higher.
  - 🥁 **Beach soca jump-up** (about 2½ minutes, 12 sections):
    - a bridge tune (B minor, G, D, A) with its own chop and bass, as a breather;
    - buzzy synth stabs for the jumps, and the whistle;
    - two snare-roll build-ups, and the last jump a whole step higher.
  - 🌴 **Sunny reggae** (about 3½ minutes, 9 sections), still all rhythm:
    - a bridge (C, D, G, E minor) with its own bass line, chop, organ bubble and vamp;
    - two dub sections, where the chop and a rim shot drop deep into the echo;
    - a stretch with sixteenth-note hi-hats.

## 0.0.10 (2026-10-12)

Everything in 0.0.9, plus longer forest and garden tunes, rhythm vamps in sunny reggae, and the music's mute at the top of its tab.

- **🔊 The music's mute chip** is now at the top of the 🎵 Music tab, above the rest, not at the bottom under the mixing board.
- **🌲 The forest walk is twice as long,** about 3 minutes:
  - a third flute tune that wanders into E minor, over chords of its own;
  - plucked violins answering the flute;
  - a cuckoo calling far off, dewdrops, and a woodpecker tapping into the echo;
  - a section a fourth higher, and breaks where the bass drops out.
- **🌿 The peaceful garden is longer too,** about 4 minutes:
  - a third flute tune in D minor, over its own pad and music box;
  - wind chimes, soft vibraphone chords and a gentle bass;
  - a section a whole step higher, and a quiet break of chimes and birds.
- **🌴 Sunny reggae has rhythm where its tunes were.** The melodica's two lead melodies are gone. In their place are two vamps that follow the bass line:
  - vamp A stabs the chord on the bass line's own off-beats;
  - vamp B doubles the end of each bar's bass figure an octave up, short, as a reggae guitar picks along with the bass.

  The last section has both.

## 0.0.9 (2026-10-12)

Everything in 0.0.8, plus a knob for each instrument, a tape stop, and a steady line for where the tune is.

- **A knob for each instrument** on its mixing-board strip, at 50 as the tune was written:
  - ✨ ring: steel pan, harpsichord, plucked violin and strings, guitar strum, music box, bell;
  - ✨ length: rim, snare, hat, shaker, cowbell, chop;
  - 💥 boom: kick;
  - 🎵 pitch: conga, in half steps;
  - 🎸 growl: bass;
  - 🐝 wah: the buzzy bass;
  - 🎹 bright: organ;
  - 〰️ vibrato: flute and melodica;
  - 〰️ waver: the ghost;
  - 🌀 wobble: the vibraphone (0 stops it);
  - 🌅 swell: the pad;
  - 🫧 bloop: bubbles;
  - 🐦 chirp: birds.

  A knob changes the notes that part plays from then on.
- **📼 Tape stop** in the transport: as when a tape machine loses its power, the music, its echoes and its room slow down and slide down in pitch together to a halt, then pause there. ▶ play carries on from where the tape stopped.
- Fixed: where the tune is ("bar 14 · beat 2 of 4 · section 2 of 6") and its time ("0:16 / 0:59") sometimes broke onto two lines as the numbers changed. They now sit on a line of their own under the buttons, which never wraps.

## 0.0.8 (2026-10-12)

Everything in 0.0.7, plus stereo panning and dub echo throws on the mixing board, and a transport for the music.

- **↔️ Pan** on every strip of the mixing board: from 10 to the left to 10 to the right. The dry sound moves, and the echo and the room stay in the middle.
- **🌊 Throw,** the dub engineer's trick, on every strip: hold it, and everything that part plays goes into the echo; let go, and it goes back to its own echo setting. A quick tap throws for a whole beat. On the whole tune's strip, **🌊 throw everything** throws every part at once.
- **⏯️ Where the tune is,** at the top of the 🎵 Music tab:
  - a slider across the whole tune, with its sections marked underneath and the one playing lit;
  - where it is in bars and beats ("bar 21 · beat 2 of 4 · section 3 of 6"), and in minutes and seconds at the tempo set ("0:53 / 2:05");
  - ⏸ pause and ▶ play, which carries on from the same place, and ⏮ rewind;
  - drag the slider and let go to jump there, while playing or paused.

  Choosing another tune plays it from the start.

## 0.0.7 (2026-10-12)

Everything in 0.0.6, plus longer tunes that change as they go, four more spooky tunes for the witch's woods, and a mixing board.

- **Every tune is longer and changes as it goes:** each now plays in sections, from about 1¼ to 2½ minutes before it starts again, with parts coming in and dropping out. There are quiet intros, breakdowns with only the drums and bass, a second melody, and some key changes: the calypso moves up a whole step for its last choruses, and the forest walk wanders up for a while.
- **Four more spooky tunes** for the witch's woods, beside the theremin waltz:
  - 🦇 bat-wing boogie: a low, buzzy bass line going round and round, plucked violins creeping over it, a ghostly voice in the break;
  - 🕯️ haunted harpsichord: broken chords over a bass that creeps down a step at a time, a ticking clock and a tolling bell;
  - 🔮 crystal-ball vibes: a spooky swing on vibraphone, with a walking bass, finger snaps, brushes and a theremin answering;
  - 🕷️ spider's tiptoe: plucked violins on tiptoe over plucked cellos, with drips and a ghost in the break.

  New instruments: a buzzy bass, a harpsichord, a vibraphone (with its wobble) and a plucked violin. Tunes can swing their eighth notes too.
- **🎚️ The mixing board,** in the 🎵 Music tab, for the tune playing:
  - each part has its own strip, with a light that blinks as it plays, so you can see the parts come and go;
  - each strip has 🔇 mute, 🎧 solo, 🔊 level (−10 to +10 dB), 🌊 echo and 🏛️ room (how much of it goes to the echo and the reverb), and 🎛️ tone (a filter from dark to bright, for dub sweeps);
  - for the whole tune: 🌊 echo repeats, ⏱️ echo time (in steps, kept in time with the tempo) and 🏛️ room, and ↺ reset the mix;
  - the mix is kept for each tune and saved with the friend; 🎲 Surprise me leaves it alone, and ↺ Start over clears it.

## 0.0.6 (2026-10-12)

Everything in 0.0.5, plus four more Caribbean and reggae tunes.

- **Four more Caribbean and reggae tunes** in the 🎵 Music tab, six in all for the beach and under the sea:
  - 🥁 beach soca jump-up: fast, a kick on every beat, a cowbell, and a bouncing steel-pan riff;
  - 🌴 sunny reggae: a major-key one-drop, with a guitar's chop, the organ's quick "bubble" between the beats, and a melodica tune;
  - 🐢 turtle steppers dub: a kick on every beat under the sea, a deep rolling bass, the chop and the melodica deep in echo, and a far-off ping like sonar;
  - 🎐 steel-pan lullaby: slow and gentle, one steel pan singing over another's broken chords, with a soft bass and bubbles.

  Each background keeps its own tune as before (beach calypso, bubble reggae), and the new tunes sit in the list beside their scene's own. New instruments: a snare, a cowbell, a reggae guitar's chop and a melodica.

## 0.0.5 (2026-10-12)

Everything in 0.0.4, plus music, and the skulls' blinking in or out of sync. Tested at home before release.

- **💀 skull eyes blink in sync / out of sync:** a chip in the witch's woods. In sync, every skull's eyes blink together, as before. Out of sync, each skull (on the fence and the buried skeletons) blinks at its own pace, from half to one and a half times the blink rate, starting from its own place in the blink. Both eyes of one skull stay together, and a redraw keeps each skull where it was. The chip shows while the eyes blink and glow.
- **🎵 Music, a first cut:** each background plays a tune of its own, made by the game's own little synthesizers from notes written as MIDI numbers (no sound files):
  - 🌲 forest: a walk in the woods (flute, plucked strings and birds);
  - 🏖️ beach: calypso (steel pan, strummed guitar and congas);
  - 🐠 underwater: bubble reggae (one-drop drums, organ chops in a dub echo, bubbles);
  - 🧙 witch's woods: a spooky waltz (a ghostly wavering voice and a tolling bell);
  - 🌿 cemetery: a peaceful garden (a music box over a soft pad, a gentle flute).

  The 🎵 Music tab plays the scene's own tune, or any tune over any background. It has a volume slider and a mute chip, and a 🔊/🔇 button sits in the picture's top-left corner. There is no music without a background, unless a tune is picked. The sound starts with the first tap, as browsers require, and rests while the page is hidden. 🎲 Surprise me leaves the music alone, and Load and ↺ Start over keep the volume and mute.
- **🏃 Tempo and 🎹 transpose** in the 🎵 Music tab:
  - Tempo runs from 25% to 300% of the tune's own speed. A change carries the tune on from where it is, at the new pace, with the echo kept in time.
  - Transpose moves the tune from 24 half steps down to 24 up. The drums keep their sound.
  - The "playing" line says the beats a minute and how far the tune is moved.
  - Both go with the friend, like the tune choice: Load brings them back, and Start over resets them.

## 0.0.4 (2026-10-12)

Everything in 0.0.3, with the skulls on the witch's fence refined.

- **Each skull has its own size,** from the usual size (the smallest) up to three times bigger.
- **💀 skulls: biggest size** sets how big the biggest skull may be: from 100% (all the usual size) to 3000%, ten times the new usual biggest, for silly-huge skulls that tower over the woods.
- **💀 skull eye size** (50% to 500%) makes the skulls' eyes bigger or smaller. As on a face, bigger eyes grow the skull to hold them: its sides and top move out, and its nose and jaw move down. The red glow stays its share of the eye.
- Each skull stays on its stake, growing up and out from the tip, and is drawn in front of the whole fence, so a big skull covers the stakes beside it.
- The old "skull eyes: size" slider is now **💀 skull eyes: red glow size**, which is what it always set.

## 0.0.3 (2026-10-12)

Everything in 0.0.2, with each hand in its own column.

- **With the hands the same,** the right/left columns are gone. In 0.0.2 they showed in that mode too, under the one style menu. Below the color come the sliders for the style the hands wear.
- **With the hands different,** each column holds everything about its own hand: its style, and the sliders for that style, such as the number of fingers, the claws, the star's points, and the hand's angle. Each hand keeps its own values, so one hand can have 9 fingers and the other 5. Only the color and the size, which both hands share, stay below the columns.
- Different hands start as a copy of each other, with their style and every setting, then change on their own.
- A pinned slider from one hand's column says whose it is in the full-screen controls, such as "⬅️ right hand: ✋ number of fingers".
- 🎲 Surprise me picks each hand's settings on their own when the hands are different.
- Friends saved with 0.0.1 or 0.0.2 load as they were: their right hand takes its own style and keeps the settings it was drawn with.

## 0.0.2 (2026-10-12)

Everything in 0.0.1, with the hands' styles chosen in a new way. A player asked for it while still learning that a friend who faces you has their left hand on your right.

- **One chip says whether the hands are the same or different,** and a tap flips it: 🖐️🖐️ *hands are the same*, or 🖐️✊ *hands are different*. It replaces the "hand on the left" menu and its "same" choice.
- **Same hands have one style menu,** for both hands.
- **Different hands have a menu each,** side by side and mirrored about a dashed line, placed as the hands are in the picture. The friend faces us, so **⬅️ right hand** is on the left and **left hand ➡️** is on the right, each arrow pointing at its hand.
- Different hands start alike, and then each changes on its own.
- 🎲 Surprise me makes the hands different about one time in four, as before.
- Friends saved with 0.0.1 load as they were: their "hand on the left" was the friend's right hand, and it still is.

## 0.0.1 (2026-10-12)

The game as it was when it got its first version number: everything built from 2026-10-09 to 2026-10-11, with M and N choosing. Those are the 64 commits before this file.

### The face

- Skin and head color, with skin swatches that say what they mean (smiley, mad, sick, alien, witch).
- Head size up to 750, face width and height up to 600, and a face shape from round to alien. The head grows to hold big eyes, a big nose and a big mouth.
- Eyes:
  - size, gap (up to 600), shape (round to narrow) and angle (flat to slanted);
  - pupil, iris and eye-white colors and sizes;
  - a shine with its own light direction, color, brightness and size;
  - star eyes, heart eyes and x eyes.
- Eyebrows that rise above big eyes, or one big eyebrow.
- Noses:
  - side and front views in several shapes (L, <, soft, hook, bulbous), with height and width;
  - a witch nose that points left, out or right, at any length, with warts that grow with it.
- A mouth from a frown to a big smile, with its size and how open it is. Normal or spiky teeth, and a tongue that sticks out, with its length, width and color.
- Faces of feeling: side lines, an explosion (hair and hoods fly off), candy, head rays and sickness.

### The body

- Hair: none, short, long or curly, with a length.
- Neck length and width, arms up or down, arm length (up to 600) and width, and leg length and width. Raised arms go in front of the face.
- Hands, each style with its own angle all the way round:
  - human, with 1 to 100 fingers;
  - alien, with 1 to 99 fingers;
  - stars, with up to 100 points;
  - mittens;
  - claws: up to 100 of them, up to 900% long.

  The left hand can have its own style, and a hand closes round a broom.
- Feet: sneakers, boots, clown shoes, or bare feet with five toes, an alien's four or three, or talons.

### Clothes and costumes

- Tops: t-shirt, long sleeves or tank top, with a collar, color and pattern (pattern size and color).
- Bottoms: long pants, shorts, a skirt (with its length) or a dress.
- Swimwear and underwear (swim trunks, briefs, bikini), with or without a top.
- Animal costumes: tiger, black cat, lion with a mane, bear, peregrine falcon, raccoon, bobcat, leopard, bald eagle, cheetah, a falcon suit with arm wings, black jaguar, wolf and deer.
  - The cats have claws.
  - The hoods hug the head at any height.
  - Tiger stripes wrap round each part, as Daniel Tiger's do.
- A witch:
  - her hat and dress in one color, with orange trim;
  - a web pattern all over the dress, or a few webs;
  - buckled boots and a buckled belt;
  - a tuxedo cat on her shoulder in any colors, and a broom, each sized from 10% to 1000%.

### Scenes

- Five backgrounds:
  - a forest;
  - under the sea, with sharks at a picnic table, forks in their fins;
  - a beach;
  - a peaceful cemetery;
  - the witch's woods: a hut on chicken legs that can grow and come nearer, a skull palisade whose red eyes blink together, skeletons, a cauldron, dead trees and big roots.
- In a scene, the figure stands on a fixed stage, where it can be dragged and sized.

### Playing and keeping

- **🎲 Surprise me:** a random friend, sometimes with irises, slanted eyes or a new head shape.
- **💾 Save:** a picture (.png), a drawing that prints at any size (.svg) and the settings (.json), named by the date and time.
- **📂 Load** a saved friend, and **↺ Start over**.
- Full screen, with sliders pinned to a heads-up display.
- Pinch and pan on tablets, and one file per tap on phones.
