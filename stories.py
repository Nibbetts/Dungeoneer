# The game is supplied with a few storylines, one of which is chosen at random at the beginning of a game,
#   or TODO: for a particular dungeon seed. They are:
# 0. New castle owner
# 1. Spelunker remembering
# 2. Adventurer in village
# 3. Outcast from parliament
# 4. Wizard's apprentice
# 5. Alchemist visiting Necromancer
# 6. Dwarf miner's trial

# Intro storyline, setting the stage
INTRO = [
    "You are the proud new owner of a very old castle, which it's previous owner seems strangely relieved to be rid of... You've just spent the day exploring the upper floors, going through some broken relics that were left behind, when you come across a thick, rusty, solid iron door that appears to lead down underground. Perhaps this castle has a dungeon, then, too! You quickly rummage around for the key until you find it. With some effort you manage to turn it in the rusty lock, then pull open the door. It screeches and grinds as it reluctantly opens into a black space. You fetch a lamp, then begin descending the stairs. As you do, the door screeches shut behind you with a great bang. Somehow, it appears to once again be locked! You call for help, but no one comes. You are trapped, without gear. But then again, nothing could possibly have prepared you for what you will find down below anyway.",
    "You're an old-school spelunker, who spends most of his time exploring caves, and can't remember doing much else. You explore them wherever you can find them, almost as if you've been looking for something, something that nags at the back of your memory, but you can't quite seem to remember what it is... One day, while exploring a deep cave system you've explored dozens of times before, and which you have mapped out most of, you come across an off-shoot cave that you're almost sure wasn't there before. Excitedly, you wriggle into the narrow hole. It widens a bit, and you find a set of worn rock steps under your feet. Something tickles at the back of your mind, and you can almost remember something, but it's still just out of reach. You keep going. As a cave-in rumbles behind you, shutting you in, you don't even look back. This is what you've been looking for, ever since you can remember.",
    "You are a brave adventurer. You've heard of the great dungeons of the village on the knoll, and how the dark denizens of the deep have recently begun to terrorize the village and nearby towns sometimes at night, so you've come to put a stop to this pit of evil before anything worse happens. Nevermind that this has been tried before, nevermind that all the brave adventurers who have entered there have not returned, nevermind that the villagers fearfully whisper of dark magic and creatures with glowing eyes from the deep - you are equal to the task! As you saunter through the town, through the whispered glances of the townsfolk, bearing your shiny armor and big sword, you put on an air of utter confidence. You come to the edge of the pit, and without hesitating you begin to clamber down the rocks and broken steps spiraling down. You slip, and slide down with a shower of stones and pebbles into the dark hole at the bottom, out of sight of the world above. You stand up, looking at the circle of light high above you, and at the broken steps. Suddenly, your armor and sword begin to glow white hot, steam, and dissolves before your eyes into smoke. Your smile faulters for a moment, and you hear the distant echo of something roaring, as if through many layers of rock. This is going to be harder than you thought.",
    "Bereft of both honor and power, cast out by the governor, you have been condemned to a fate worse than death. Your once kinsmen in the city parliament, who have been dragging you down, down the levels of the city, below the basements, below the sewers, now pick you bodily up and throw you through the enormous iron gate they have opened, sending you tumbling roughly down the chipped obsidian stairs behind. You pick yourself up and look back as they slam the great gate shut behind you. They turn the great mechanism which locks it in place with huge iron beams, laughing nervously at you through the bars. You see fear in their eyes as they glance at the utter darkness behind you. They light a second torch - a last tribute to the dead - and throw it to you before their quick retreat darkens the way you came in. I'll show them! You think venemously to yourself. They'll see! You turn, and, picking up the torch, you stalk determinedly into the darkness.",
    "The wizard you're apprenticed to mumbles and turns in his sleep. You wished he wouldn't do that - it scares you, and you have trouble sleeping. Sometimes he mumbles arcane words, and accidentally sets off unpredictable spells. He mumbles some more, then shouts and reaches for the glowing orange crystal on chain around his neck. He is still clearly asleep, but great misty tendrils of light stream out of the stone and begin to swirl in the air between you. You gulp and shut your eyes. This is going to be bad, but you know from experience that trying to wake him would be even worse. What was it that happend to the previous apprentice? He never did say... He tosses and turns again, violently, the agitation of his movements matched by the great misty tendrils swirling around, as well as his unintelligible mumbling. He shouts again in his sleep, his hand opening out towards you. Instantly, the glowing clouds explode in shards of light, and you find yourself in darkness, dumped onto hard cold stone, with a glowing arcane portal above you which quickly pops and collapses to nothingness. You're on a stone platform at the top of a set of stairs leading down into a cavernous, dark space below. You gulp. Maybe you should have woken the wizard after all.",
    "You're an alchemist. You have asked the Necromancer from the tower down the road if he mightn't let you take a gander at his hoard of minions and collection of artifacts, you know, for the purpose of gathering bits and pieces for your potions and experiements. To your surprise, he seems quite pleased to let you do so. He has invited you cordially in, and shows you to a jet black door that glows in cracks and fissures like a hot coal. He hands you a lantern, opens the door with a wave of his hand and bows, showing you a grin of pointed teeth, and gestures for you to step through, onto the stone staircase beyound. You thank him kindly. Then, as he closes the door after you he tells you lightly, but with a slightly disturbing grin, that he can't spare the time, but that you're welcome to show yourself around and touch whatever you please. The door shuts and sizzles, glowing at the edges, before turning into a solid stone wall. Now you're a little more nervous, but you shrug, turn, and head down, figuring you might as well do what you came for.",
    "You're a dwarf miner, who is riding alone in a mine cart back from a long night's work digging for black lightning gems in the deep caverns off of the dwarven enclave. While griding along a stretch of track crossing through a deep diagonal chasm, some giant cave bats surge through, startling you. One collides with a clang into your cart and you jerk away. The cart tilts dangerously, and you frantically jump the other way to settle it, but you overcompensate and both cart and dwarf go hurtling down, deep into the dark chasm below, where even the dwarves don't go. You cling to the inside of the cart desperately as it bounces and skitters down the rough diagonal slope into a massive open space. At the dark bottom it comes to a halt in a squishy bed of fungus, hard enough to shatter and splinter. The crash nearly extinguishes your sturdy dwarf lantern, and lands you, already bruised, with a squelch nearby. You pick yourself up and look around. The walls are slippery with fungus, and no light can be seen from the tracks far, far above. With a frightened sqeak, the stunned bat that fell nearby picks itself up and flutters drunkenly back upwards. You see are creepy glowing eyes further along the giant crack in one direction, so you quickly head in the other. Eventually you come to some stone steps leading down. You shudder at the implications, as this can't have been made by dwarves, but it's the only way to go. You were mining a long way from the main caverns; you'd have starved to death by the time someone found you if you stayed here. They say a dwarf's trial comes sooner or later - I guess yours begins now.",
]

# TODO: fill in rest of stories
# A tidbit of story to be revealed somewhere near the middle of the dungeon
MIDDLE = [
    "", # New castle owner
    "", # Spelunker remembering
    "", # Adventurer in village
    "", # Outcast from parliament
    "", # Wizard's apprentice
    "", # Alchemist visiting Necromancer
    "", # Dwarf miner's trial
]

# Ending of the story when you lose
END_LOSE = [
    "", # New castle owner
    "", # Spelunker remembering
    "", # Adventurer in village
    "", # Outcast from parliament
    "", # Wizard's apprentice
    "", # Alchemist visiting Necromancer
    "", # Dwarf miner's trial
]

# Ending of the story when you win
END_WIN = [
    "", # New castle owner
    "", # Spelunker remembering
    "", # Adventurer in village
    "", # Outcast from parliament
    "", # Wizard's apprentice
    "", # Alchemist visiting Necromancer
    "", # Dwarf miner's trial
]

COUNT = len(INTRO)

assert len(INTRO) == len(MIDDLE) == len(END_LOSE) == len(END_WIN)