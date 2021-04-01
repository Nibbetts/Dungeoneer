import numpy as np
from random import randint, choice, random, uniform, sample, choices
from random import seed as seedrandom
from time import time
from abc import ABC, abstractstaticmethod
from sys import argv
# NOTE: randint is inclusive on both ends

from stories import COUNT as STORY_COUNT



# Enumerations
LR, UD, PIT = 0, 1, 2 # Hallway orientations
PARAM_ROOM_RATIO, PARAM_LOCKED_RATIO, PARAM_DOWN_STAIR_RATIO, PARAM_UP_STAIR_RATIO, PARAM_LOOP_RATIO, PARAM_BOSS_ROOM, PARAM_ORDERLINESS = 0, 1, 2, 3, 4, 5, 6 # Layer parameter indices
ORDER_FADE, ORDER_SKEW, ORDER_THIRDS, ORDER_JITTER, ORDER_RANDOM, ORDER_CONSTANT = "fade", "skew", "thirds", "jitter", "random", "constant"

# Size Defaults and Param Constants
TERM_W, TERM_H = 80, 24 # Standard terminal width and height
HALL_BREADTH = 1        # Can't change this without bugs
FULL_DENSITY = .8
# From observation, it seems most layers that look full fall inexplicably very close to 83% density,
#   and we should not expect any more than this as the remainder will often be spaces too small to use.

# Unicode Characters
SOLID      = 0x2588
PART3      = 0x2593
PART2      = 0x2592
PART1      = 0x2591
SPACE      = ord(' ')
DOOR       = ord('%')
LOCK       = ord('#')
KEY        = ord('⚷')
STAIR_UP   = 0x21D7
STAIR_DOWN = 0x21D8
STAIR_BOTH = 0x2928
LOOP       = DOOR # ord('L')

CHECKER    = "▀▄"
GRID       = "─┼"



"""
# TODO: Game stuff:
- Dungeoneer? Zeldungeon?
- play snake in boss room? Turns when you're in line?
- Zelda-like motion and sword/shield?
- enemies that do nothing but bounce back and forth off walls?
- traps?
- T for trees above ground?
- Flashlight is directional, lantern fills room or hall
# TODO: Lock doors leading from a boss room, and place key in boss room. Lock door into boss room?
# TODO: hazards, for d&d
# TODO: stuff, to search through "*"
- separate types of level/multilevel maps:
    - stairs
    - treasures (includes keys?)
    - monsters & bosses (not live)
    - rooms
    - rooms & halls
    - stairs, rooms, halls
    - all of the above
    - magic mirror (item; live, see around corners)
    - magic pool (live, one level)
    - crystal ball (live, everywhere)


NOTES:
- Several different parameters often affect each other. For example,
    - end stair counts are affected by:
        - down_stair_ratio
        - up_stair_ratio
        - loop_ratio (since many loops are extra stairs)
    - end density is affected by:
        - density
        - orderliness (controls truncation & dead ends if not specified, as well as complex structure like hall_headstart and hall_end_tries)
        - truncate_halls (some halls shortened)
        - allow_dead_ends (some halls deleted in post-processing)
        - hall_locks?
        - max_consec_fails, max_fails
        - max_backtrack
        - up_stair_ratio, down_stair_ratio in extreme parameter sets like one_path
        - several other things perhaps a little
    - end room / hall ratio is affected by:
        - room_ratio
        - various boss room & layer parameters
        - extreme max_hall & min_hall parameters
        - orderliness, hall_headstart, allow_dead_ends
        - hall_locks?
    - generation end is affected by:
        - max_consec_fails
        - max_fails
        - density
        - max_backtrack, override_backtrack
    - boss, room, & hall branch weights are weighted relative to each other
    - hall_branch_weight and hall_end_tries may affect each other
    - override_backtrack affects max_backtrack, especially in high density requirements
    - Probably other confusing inter-dependencies I haven't spotted at the moment
"""


class Place(ABC):
    """ This is an abstract base class for Rooms and Hallways,
        describing their similarities."""
    def __init__(self, pid, x, y, z, w, h, door_in, parent, children=None, loops=None, contents=None, boss=False, room=False):
        self.pid = pid
        self.x = x
        self.y = y
        self.z = z
        self.w = w
        self.h = h
        self.door_in = door_in # Coordinate tuple; may be of a stair. Of the form: (z, y, x)
        self.parent = parent # May be None if root
        self.children = children if children else [] # List of descendent Places (those with doors leading off of this one)
        self.loops = loops if loops else [] # List of tuples ((z, y, x), Place)
        self.area = (w-2) * (h-2) # Internal area
        self.perimeter_area = 2*w + 2*h - 4
        self.boss = boss # Whether or not I am a boss area
        self.room = room # Whether or not I am a room
        self.contents = contents if contents else [] # List of Items or Entities contained in the room (not doors or stairs),
        #   each entry of the form ((z, y, x), id). Type of object is to be retrieved from the map.

    @abstractstaticmethod
    def place(dungeon, pid, x, y, z, parent, stair=False, allow_shrink=True):
        raise NotImplementedError("Place class not to be instantiated")


class Room(Place):
    def __init__(self, pid, x, y, z, w, h, door_in, parent, children=None, loops=None, contents=None, boss=False):
        super().__init__(pid, x, y, z, w, h, door_in, parent, children=children, contents=contents, boss=boss, room=True)

    @staticmethod
    def place(dungeon, pid, x, y, z, parent, w=None, h=None, stair=False, boss=False, allow_shrink=True):
        # w and h can be used to limit the size, but not increase it.
        # Currently, we're not doing this. We could make use of limiting w and h
        # to line up rooms neatly to adjacent ones, but instead we're allowing
        # rooms and halls to more frequently fill their spaces around them, when
        # affected by the orderliness parameter.

        MAX = dungeon.BOSS_MAX_ROOM if boss else dungeon.MAX_ROOM
        MIN = dungeon.BOSS_MIN_ROOM if boss else dungeon.MIN_ROOM if dungeon.MIN_ROOM else 5

        # Compute available space
        result = dungeon.check_place(x, y, z, w if w else MAX, h if h else MAX)
        if result is None: return None
        L, R, U, D, met_bound = result
        w = L + R + 1
        h = U + D + 1

        # Make sure it works here, and catch literal edge cases
        if dungeon.MIN_ROOM is None and not boss:
            if (w < 4) or (h < 4): return None # allowing 4's for 2x3 rooms (4x5 with borders)
            if (w < 5) and (h < 5): return None
        else:
            if (w < MIN) or (h < MIN): return None
        if L == 0 or R == 0:
            if U < 1 or D < 1 or stair: return None # Stairs disallowed on borders
        if U == 0 or D == 0:
            if L < 1 or R < 1 or stair: return None

        # Randomly decrease the size
        if allow_shrink or met_bound != 2: # Slightly lessens the largeness of rooms resulting from orderliness parameter being high # TODO: revisit this. separate lr and ud?
            area = (w-2) * (h-2) # initial inner area
            w_hat = min(w, randint(MIN, MAX))
            h_hat = min(h, randint(MIN, MAX))
            while w > w_hat and ((area >= dungeon.BOSS_MIN_AREA + h-2) or not boss):
                if L > 1 and choice([True, False]):
                    L -= 1
                    w -= 1
                    area -= h-2
                elif R > 1:
                    R -= 1
                    w -= 1
                    area -= h-2
            while h > h_hat and ((area >= dungeon.BOSS_MIN_AREA + w-2) or not boss):
                if U > 1 and choice([True, False]):
                    U -= 1
                    h -= 1
                    area -= w-2
                elif D > 1:
                    D -= 1
                    h -= 1
                    area -= w-2

        # Make the Room
        corner_x = x - L
        corner_y = y - U
        room = Room(pid, corner_x, corner_y, z, w, h, door_in=(z, y, x), parent=parent, boss=boss)
        if parent: parent.children.append(room)
        else: dungeon.root = room
        return room


class Hallway(Place):
    def __init__(self, pid, x, y, z, orientation, length, breadth, door_in, parent, children=None, loops=None, contents=None):
        super().__init__(pid, x, y, z,
            length if orientation == LR else breadth,
            length if orientation == UD else breadth,
            door_in, parent, children=children, contents=contents, boss=False, room=False)
        self.orientation = orientation
        self.length = length

        # Another way of understanding area and perimeter_area:
        # self.area = length-2
        # self.perimeter_area = 2*length + 2*(breadth) - 4

    @staticmethod
    def place(dungeon, pid, x, y, z, parent, orientation=None, length=None, stair=False, allow_shrink=True):
        # Compute available space
        orientation = orientation if orientation is not None else choice([LR, UD])
        w, h = (dungeon.MAX_HALL, 3) if orientation == LR else (3, dungeon.MAX_HALL) if orientation == UD else (3, 3)
        result = dungeon.check_place(x, y, z, w, h)
        if result is None: return None
        L, R, U, D, met_bound = result
        w = L + R + 1
        h = U + D + 1
        length, width = (w, h) if orientation == LR else (h, w) if orientation == UD else (dungeon.HALL_BREADTH, dungeon.HALL_BREADTH)

        # Make sure it works here, and catch literal edge cases
        if (length < dungeon.MIN_HALL) or (width < dungeon.HALL_BREADTH): return None
        if L == 0 or R == 0:
            if U < 1 or D < 1 or stair: return None # Stairs disallowed on borders
        if U == 0 or D == 0:
            if L < 1 or R < 1 or stair: return None

        # Randomly decrease the size
        if allow_shrink or met_bound == 0: # Slightly lessens the lengthiness of halls resulting from orderliness being high
            if orientation != PIT:
                length_hat = min(length, randint(dungeon.MIN_HALL, dungeon.MAX_HALL))
                S1, S2 = (L, R) if orientation == LR else (U, D)
                while length > length_hat:
                    if S1 > 1 and choice([True, False]):
                        S1 -= 1
                        length -= 1
                    elif S2 > 1:
                        S2 -= 1
                        length -= 1
                if orientation == LR: L, R = S1, S2
                else: U, D = S1, S2
            else: raise NotImplementedError("Vertical Shafts not implemented")

        # Make the Room
        corner_x = x - L
        corner_y = y - U
        hall = Hallway(pid, corner_x, corner_y, z, orientation, length, dungeon.HALL_BREADTH, door_in=(z, y, x), parent=parent)
        if parent: parent.children.append(hall)
        else: dungeon.root = hall
        return hall


class Dungeon:
    # TODO: Move Documentation here

    def __init__(self,
            w=TERM_W//2,  h=TERM_H,  d=10, # W/2 since using 2 chars per square
            min_room=None,        max_room=8,
            min_hall=3,           max_hall=18,          hall_breadth=1,
            boss_min_room=8,      boss_max_room=20,     boss_min_area=170, # TODO: switch boss room area to a None and default
            boss_bottom=True,     boss_after_layer=3,   boss_layer_rooms=None,
            boss_layer_ratio=.2,  boss_room_delay=0,    boss_layer_count=None,
            boss_allow_stair=False,
            max_backtrack=20,     max_key_backtrack=10,
            density=.75,          override_backtrack=True,
            locked_ratio=.25,     down_stair_ratio=.03, up_stair_ratio=.1,
            room_ratio=.5,        loop_ratio=.1,        vary_room_ratio=True,
            boss_branch_weight=6, room_branch_weight=1, hall_branch_weight=2,
            hall_doors=False,     hall_locks=False,
            orderliness=.5,       orderliness_scheme=None,
            allow_dead_ends=None, truncate_halls=None,
            hall_headstart=None,  hall_end_tries=None,
            max_consec_fails=200, max_fails=None,
            highlight_borders=False,
            report=True,          seed=None ): # Same seed will only rebuild the same dungeon if given the same parameters!

        # Check some input validity
        if w < 5 or h < 5 or d < 1 \
                or (min_room and min_room < 1) or max_room < 1 or min_hall < 1 or max_hall < 1 or hall_breadth < 1 \
                or boss_max_room <= max_room or boss_min_room < 1 or boss_min_area < 1 or boss_min_area > boss_max_room**2 \
                or boss_after_layer < 0 or (boss_layer_rooms and boss_layer_rooms < 0) or boss_layer_ratio < 0 or boss_layer_ratio > 1 \
                or boss_room_delay < 0 or (boss_layer_count and (boss_layer_count < 0 or boss_layer_count > d-boss_after_layer)) \
                or (max_backtrack and max_backtrack < 0) or (max_key_backtrack and max_key_backtrack < 0) \
                or density > 1 or density < 0 \
                or locked_ratio < 0 or locked_ratio > 1 or down_stair_ratio < 0 or down_stair_ratio > 1 or up_stair_ratio < 0 or up_stair_ratio > 1 \
                or room_ratio < 0 or room_ratio > 1 or loop_ratio < 0 or loop_ratio > 1 \
                or (hall_headstart and hall_headstart < 0) or (hall_end_tries and hall_end_tries < 0) \
                or (orderliness and (orderliness < 0 or orderliness > 1)) \
                or orderliness_scheme not in [None, ORDER_FADE, ORDER_SKEW, ORDER_THIRDS, ORDER_JITTER, ORDER_RANDOM, ORDER_CONSTANT] \
                or boss_branch_weight < 0 or room_branch_weight < 0 or hall_branch_weight < 0 \
                or max_consec_fails < 1 or (max_fails and max_fails < 1):
            raise(ValueError("Invalid value given in Dungeon parameters"))
        if hall_breadth != HALL_BREADTH: raise(NotImplementedError("Wider halls not currently implemented"))
        if density > FULL_DENSITY: print(f"WARNING: density > {FULL_DENSITY} is likely to generate large failure counts and be difficult to reach without regenerating several dungeons")

        # Global Dungeon Proportion Characteristics
        self.w = w # Dungeon Width
        self.h = h # Dungeon Height
        self.d = d # Dungeon Depth

        self.MIN_ROOM           = (min_room      + 2) if min_room else min_room # min_room defaults to None for the special size 2x3, neither 2 nor 3
        self.MAX_ROOM           = max_room       + 2 # Max dimension for a normal room - user inputs the inner size
        self.MIN_HALL           = min_hall       + 2 # Min hall length - user inputs the inner size
        self.MAX_HALL           = max_hall       + 2 # Max hall length - user inputs the inner size
        self.HALL_BREADTH       = hall_breadth   + 2 # Hall Breadth - user inputs the inner size
        
        # Boss Layer/Room parameters
        self.BOSS_MIN_ROOM      = boss_min_room  + 2 # Min dimension for a boss room
        self.BOSS_MAX_ROOM      = boss_max_room  + 2 # Max dimension for a boss room
        self.BOSS_MIN_AREA      = boss_min_area      # Minimum inner area of acceptable boss rooms
        self.BOSS_BOTTOM        = boss_bottom        # Force bottom layer to be a boss layer.
        self.BOSS_AFTER_LAYER   = boss_after_layer   # Won't generate boss rooms till after this many layers, ie, maybe as soon the layer with this index.
        self.BOSS_LAYER_ROOMS   = boss_layer_rooms   # Limit boss room count per layer.
        self.BOSS_LAYER_RATIO   = boss_layer_ratio   # How often a layer is a boss room layer. Not used if boss_layer_count is used. Only applied where not contrary to boss_bottom and boss_after_layer. Superceded by BOSS_BOTTOM.
        self.BOSS_ROOM_DELAY    = boss_room_delay    # How many rooms/halls into a boss layer we'll start trying to make a boss room. Useful for large one-layer maps.
        self.BOSS_LAYER_COUNT   = boss_layer_count   # Attempt to force a specific number of boss layers (won't be more than this). Superceded by BOSS_BOTTOM.
        self.BOSS_ALLOW_STAIR   = boss_allow_stair   # Allow stairs to be the first entrance to a boss room (Loops of course will mean you may be able to enter another way).

        # Complexity and other Global Characteristics
        self.MAX_BACKTRACK      = max_backtrack      # How many rooms/halls back we might branch anew. This is not a direct control, but largely influences branching and the likelihood of long independent branches. May be overridden to meet density if override_density is True.
        self.MAX_KEY_BACKTRACK  = max_key_backtrack  # How many rooms (only) back we might place a key from it's door
        self.DENSITY            = density            # May not be met, especially with low fail limits or high down stair ratio or low backtracking unless override_backtrack True, but does stop after this density. NOTE: Density may be reduced again by removing spare halls.
        self.OVERRIDE_BACKTRACK = override_backtrack # Whether or not to go back and add more on after to layers that aren't dense enough yet, breaking max_backtrack.
        self.VARY_ROOM_RATIO    = vary_room_ratio    # Use a varied trimodal distribution for room/hall ratios (Turn off to get all layers to match ratios)
        self.BOSS_BRANCH_WEIGHT = boss_branch_weight # How boss rooms are comparatively weighted when deciding which recent place to randomly branch off of; higher numbers compared to other branching weights make it more likely.
        self.ROOM_BRANCH_WEIGHT = room_branch_weight # ''  other rooms ''
        self.HALL_BRANCH_WEIGHT = hall_branch_weight # ''  hallways    ''
        self.HALL_DOORS         = hall_doors         # Whether to draw the doors in hallways as doors or as spaces (will still draw them if locked. See hall_locks).
        self.HALL_LOCKS         = hall_locks         # Whether or not to lock some hall doors as we would rooms.

        # Global Characteristics that may be locally modified
        self.LOCKED_RATIO       = locked_ratio       # Likelihood to attempt to lock a door
        self.DOWN_STAIR_RATIO   = down_stair_ratio   # Likelihood to go down when we don't have to, or of having more down stair branching
        self.UP_STAIR_RATIO     = up_stair_ratio     # Likelihood of going up instead of down, creating an inverted stair (Adds complexity, more maze-like)
        self.ROOM_RATIO         = room_ratio         # Probability ratio of rooms to halls (not including boss rooms)
        self.LOOP_RATIO         = loop_ratio         # Likelihood of creating a loop when the option is found

        # Orderliness
        self.ORDERLINESS        = orderliness        # Generally how organized rooms and hallways are. Used for centering the ranges or if orderliness_scheme is "constant"
        self.ORDERLINESS_SCHEME = orderliness_scheme if orderliness_scheme else choice([ORDER_FADE, ORDER_SKEW, ORDER_JITTER, ORDER_THIRDS]) # Scheme to use for orderliness. "fade" for a smooth gradient, "skew" for a sigmoid-based gradient, "random" for random, "thirds" for three striations of the dungeon, "jitter" for a fade, but with random local variation, or "constant" to use the previous parameter exactly on every layer. None will pick a cool one at random.
        self.ALLOW_DEAD_ENDS    = allow_dead_ends    # If True, none are removed; if False, all are removed; if None, follows orderliness based probability.
        self.TRUNCATE_HALLS     = truncate_halls     # If True, all are shortened; if False, none are; if None, follows orderliness based probability. NOTE: Truncating halls can result in halls shorter than min_hall.
        self.HALL_HEADSTART     = hall_headstart if hall_headstart else round(2*w*h / self.MAX_HALL**2) # How many halls we'll try to generate, when being orderly, before other things on a level. Use None for an automatically estimated useful number.
        self.HALL_END_TRIES     = hall_end_tries if hall_end_tries else 3*self.HALL_HEADSTART # How many tries we get to generate things at the ends of halls, when being orderly, before off of other parts of halls on a level. Use None for an automatically estimated useful number.
        # TODO: make the above two things not dependent on orderliness if not None? Or just reduce the other things...
        # TODO: maybe combine dead ends and truncation params into one? 
        # TODO: separate out the long hall/room fill param?

        # Procedural Generation Boundaries           # NOTE: fail count limits affect the attainability of the desired density.
        self.MAX_CONSEC_FAILS   = max_consec_fails   # Hard limit on consecutive fails per layer before stopping the whole generator. Estimations ~200 for common parameter sets, so no smart calculator, but odd configurations may require more.
        self.MAX_FAILS          = max_fails if max_fails else round(w*h*d*density) # Hard limit on total fails before stopping the generator. Use None for a smart estimation based on dungeon size.
        
        # Aesthetics
        self.HIGHLIGHT_BORDERS  = highlight_borders  # Whether or not to draw borders around the rooms

        self.generate_map(report=report, seed=seed)

        # # OTHER MEMBER VARIABLES, set upon map generation:
        # self.map               # Array of ints indicating block types on the map,
                                 #   corresponding to char ords in many cases
        # self.story             # The index of the storyline used for this seed
        # self.root              # The starting Room or Hall
        # self.halls             # List of Hallways,             in creation order
        # self.rooms             # List of Rooms,                in creation order
        # self.places            # Rooms and Halls together,     in creation order
        # self.layer_places      # Rooms + Halls in each layer,  in creation order
        # self.layer_areas       # Approximate used 2D area of each layer
        # self.layer_densities   # Density metric for each layer
        # self.layer_params      # List of layer parameter lists, in depth order
        # self.layer_bosses      # List of lists of boss rooms in each layer
        # self.doors             # List of Doors,                in creation order
        # self.stairs_up         # List of upward stairways,     in creation order
        # self.stairs_down       # List of downward stairways,   in creation order
        # self.stairs_inverted   # List of places higher than their parent, in creation order
        # self.locks             # Dict mapping locked door coords to key ID's
        # self.keys              # Dict mapping key coords to key ID's
        # self.loops             # List of loops
        # self.dead_ends         # Count of dead-end hallways not removed
        # self.dead_ends_removed # Count of dead-end hallways removed
        # self.halls_truncated   # Count of hallways shortened
        # self.seed              # The seed used to generate this dungeon given these parameters
        # self.time              # How long it took to generate, in seconds
        # self.density           # Density metric for the whole dungeon
        # self.density_preclean  # Density metric before cleaning up hallways

        # self.fails             # Total count of failed attempts to generate a Room, Hall, or Loop somewhere
        # self.layer_fails       # Array of per-layer fail counts
        # self.layer_consec_fails# Array of per-layer Current count of consecutive fails
        # self.layer_max_consec  # Array of per-layer max seen consecutive fails
        # self.layer_complete    # Array of per-layer boolean indicating whether the density requirement was met
        # self.stop_condition    # String describing under what condition place generation eventually ceased

    def __str__(self):
        s = "\n"
        for l in range(self.d):
            s += f"Level {l}, Density: {self.layer_densities[l]:.2f}\n"
            s += self.level_to_string(l)
        return s

    def level_to_string(self, level):
        # Helper function to format Key/Lock numbers
        def format_int(n, hex=False, fill="??"):
            s = f"{n:0>2x}" if hex else f"{n:0>2.0f}"
            if len(s) > 2: s = fill
            return s

        if self.d <= level or level < 0:
            print(f"Cannot print Level {level}, does not exist in this dungeon!")
            return None
        else:
            s = ""
            for i in range(self.h):
                for j in range(self.w):
                    c = self.map[level, i, j]
                    if c == LOCK:
                        s += format_int(self.locks[level, i, j], fill=chr(LOCK) * 2)
                    elif c == KEY:
                        s += format_int(self.keys[level, i, j], fill=chr(KEY) + " ")
                    elif c == STAIR_UP or c == STAIR_DOWN or c == STAIR_BOTH:
                        s += chr(c) + " "
                    else:
                        s += chr(c) * 2
                s += "\n"
        return s

    def check_place(self, x, y, z, maxw=None, maxh=None):
        """ Checks around a specific location to see how far it is solid
            in every direction so we can place new open spaces.

            Returns: None if the location is already open, else (L, R, U, D),
                where L is the number of solid spaces to the Left, etc, so that
                to measure width of the solid space you would do L + R + 1,
                with the extra 1 being for the location being checked."""

        maxw = maxw if maxw else self.MAX_HALL
        maxw = maxw if maxw else self.MAX_HALL

        # Check this spot itself
        if self.map[z, y, x] != SOLID:
            return None

        # Check in expanding box for solid spaces around
        corner_x, corner_y = x, y
        w, h = 1, 1
        L, R, U, D = 0, 0, 0, 0
        met_bound = 0
        tryL, tryU, tryR, tryD = True, True, True, True
        for _ in range(100):
            if tryL or tryU or tryR or tryD:

                # Try to expand left
                if tryL and choice([True, False]): # Shuffle order of check with choice
                    if corner_x <= 0: tryL = False
                    else:
                        for i in range(h):
                            if self.map[z, corner_y+i, corner_x-1] != SOLID or corner_x-1 < 0:
                                tryL = False
                                met_bound += 1
                                break
                    if tryL:
                        w += 1
                        corner_x -= 1
                        L += 1
                        if w == maxw:
                            tryL, tryR = False, False
                
                # Try to expand up
                if tryU and choice([True, False]):
                    if corner_y <= 0: tryU = False
                    else:
                        for i in range(w):
                            if self.map[z, corner_y-1, corner_x+i] != SOLID or corner_y-1 < 0:
                                tryU = False
                                met_bound += 1
                                break
                    if tryU:
                        h += 1
                        corner_y -= 1
                        U += 1
                        if h == maxh:
                            tryU, tryD = False, False
                
                # Try to expand right
                if tryR and choice([True, False]):
                    if corner_x+w >= self.w: tryR = False
                    else:
                        for i in range(h):
                            if self.map[z, corner_y+i, corner_x+w] != SOLID or corner_x+w >= self.w:
                                tryR = False
                                met_bound += 1
                                break
                    if tryR:
                        w += 1
                        R += 1
                        if w == maxw:
                            tryL, tryR = False, False
                
                # Try to expand down
                if tryD and choice([True, False]):
                    if corner_y+h >= self.h: tryD = False
                    else:
                        for i in range(w):
                            if self.map[z, corner_y+h, corner_x+i] != SOLID or corner_y+h >= self.h:
                                tryD = False
                                met_bound += 1
                                break
                    if tryD:
                        h += 1
                        D += 1
                        if h == maxh:
                            tryU, tryD = False, False
            
            else:
                break

        return L, R, U, D, met_bound

    def find_at(self, x, y, z, ignore=[], allow_corners=True, check_borders=False, first_only=False):
        """ Place finder. If check_borders is True, will only check borders,
            while None checks both borders and inside.
            If not allow_corners, will check only the sides of the borders when
            borders is None or True. If first_only, will return only the first
            place found instead of a list."""
        found = []
        for p in self.layer_places[z]:
            if p in ignore: continue
            if check_borders != False and ((p.x <= x) and (p.y <= y) and (p.x+p.w-1 >= x) and (p.y+p.h-1 >= y)): # True or None
                if allow_corners and ((p.x == x) or (p.y == y) or (p.x+p.w-1 == x) or (p.y+p.h-1 == y)) \
                        or ((p.x == x) ^ (p.y == y) ^ (p.x+p.w-1 == x) ^ (p.y+p.h-1 == y)):
                    if first_only: return p
                    else: found.append(p)
            elif check_borders != True and ((p.x < x) and (p.y < y) and (p.x+p.w-1 > x) and (p.y+p.h-1 > y)): # False or None
                if first_only: return p
                else: found.append(p)
        return None if first_only else found
        # def find_at(self, x, y, z, ignore):
        #     for p in self.layer_places[z]:
        #         if p is ignore: continue
        #         if ((p.x <= x) and (p.y <= y) and (p.x+p.w-1 >= x) and (p.y+p.h-1 >= y)) and ((p.x == x) ^ (p.y == y) ^ (p.x+p.w-1 == x) ^ (p.y+p.h-1 == y)):
        #             return p

    def write(self, place, write_space=False, write_door_in=False, write_loops=False, erase=False, only_overwrite_symbol=None):
        if write_space:
            # Write room spaces inside its border
            for x in range(place.x + 1, place.x + place.w - 1):
                for y in range(place.y + 1, place.y + place.h - 1):
                    if only_overwrite_symbol is None or self.map[place.z, y, x] == only_overwrite_symbol:
                        self.map[place.z, y, x] = SOLID if erase else SPACE

        if write_door_in:
            # Write entryway not occurring on this level
            if place.parent and place.parent.z != place.z:
                coords = place.parent.z, place.door_in[1], place.door_in[2]
                if only_overwrite_symbol is None or self.map[coords] == only_overwrite_symbol:
                    symbol = SPACE if erase \
                        else STAIR_UP if place.parent.z > place.z \
                        else STAIR_DOWN
                    self.map[coords] = symbol
            # Write entryway occurring on this level
            if only_overwrite_symbol is None or self.map[place.door_in] == only_overwrite_symbol:
                symbol = SOLID if erase \
                    else STAIR_UP if (place.parent is None) or (place.parent.z < place.z) \
                    else STAIR_DOWN if place.parent.z > place.z \
                    else LOCK if place.door_in in self.locks \
                    else SPACE if (not place.room and not (place.parent and place.parent.room) and not self.HALL_DOORS) \
                    else DOOR
                # NOTE on above: Only conditionally draw doors between halls.
                self.map[place.door_in] = symbol
        
        if write_loops:
            # Write loops (currently all are on this level)
            for loop in place.loops:
                if only_overwrite_symbol is None or self.map[loop[0]] == only_overwrite_symbol:
                    symbol = SOLID if erase \
                        else LOCK if loop[0] in self.locks \
                        else SPACE if (not place.room and not loop[1].room and not self.HALL_DOORS) \
                        else LOOP
                    # NOTE: Loops essentially act like doors
                    self.map[loop[0]] = symbol

        # if write_doors_out: # NOTE: INCOMPLETE! If use, must add stair writing on both levels!
        #     for child in place.children:
        #         symbol = SOLID if erase else DOOR if place.z == child.z else STAIR_DOWN if place.z > child.z else STAIR_UP
        #         if only_overwrite_symbol is None or self.map[child.door_in] == only_overwrite_symbol:
        #             self.map[child.door_in] = symbol
            
    def borders(self, place, borders=None):
        # NOTE: uses (x, y) tuples instead of (z, y, x)
        if not borders:
            borders = []

            # Add sides and top but not corners,
            #   nor those too close to the edge of the map
            if place.y > 1:
                for c in range(place.x + 1, place.x + place.w - 2):
                    borders.append((c, place.y))
            if place.y + place.h < self.h - 2:
                for c in range(place.x + 1, place.x + place.w - 2):
                    borders.append((c, place.y + place.h - 1))
            if place.x > 1:
                for c in range(place.y + 1, place.y + place.h - 2):
                    borders.append((place.x, c))
            if place.x + place.w < self.w - 2:
                for c in range(place.y + 1, place.y + place.h - 2):
                    borders.append((place.x + place.w - 1, c))
        
        # Remove doors and spaces next to them
        for d in [c.door_in for c in place.children] + [place.door_in]:
            _, y, x = d
            L, R, U, D = (x-1, y), (x+1, y), (x, y-1), (x, y+1)
            for loc in [d, L, R, U, D]:
                if loc in borders: borders.remove(loc)

        return borders

    def report(self): # TODO: add/change stuff
        return  "-------------D U N G E O N-------------\n" \
            +  f"              Size: {self.w} x {self.h} x {self.d}\n" \
            +  f"           Density: {self.density}\n" \
            +  f" Pre-Clean Density: {self.density_preclean}\n" \
            +  f"             Rooms: {len(self.rooms)}\n" \
            +  f"             Halls: {len(self.halls)}\n" \
            +  f"             Doors: {len(self.doors)}\n" \
            +  f"              Keys: {len(self.keys)}\n" \
            +  f"             Locks: {len(self.locks)}\n" \
            +  f"            Stairs: {len(self.stairs_up)}\n" \
            +  f"   Inverted Stairs: {len(self.stairs_inverted)}\n" \
            +  f"             Loops: {len(self.loops)}\n" \
            +  f"       Boss Layers: {sum([s[PARAM_BOSS_ROOM] for s in self.layer_params])}\n" \
            +  f"        Boss Rooms: {len([r for r in self.rooms if r.boss])}\n" \
            +   "\n" \
            +  f"    Dead Ends Kept: {self.dead_ends}\n" \
            +  f" Dead Ends Removed: {self.dead_ends_removed}\n" \
            +  f"   Halls Truncated: {self.halls_truncated}\n" \
            +   "\n" \
            +  f"       Total Fails: {self.fails}\n" \
            +  f"  Layer Min Consec: {min(self.layer_max_consec):.0f}\n" \
            +  f"  Layer Max Consec: {max(self.layer_max_consec):.0f}\n" \
            +  f"    Stop Condition: {self.stop_condition}\n" \
            +   "\n" \
            +  f"       Story Index: {self.story}\n" \
            +  f"    Time (Seconds): {self.time}\n" \
            +  f"              Seed: {self.seed}\n" \
            +   "---------------------------------------"

    @staticmethod
    def area_of(place):
        """ Helper function to calculate the approximate unique area of a Room or Hall."""
        return place.area + place.perimeter_area/2-1

    def generate_map(self, report=True, seed=None):
        start_time = time()

        # RESET VARIABLES
        self.seed               = int(round(time() * 1e7)) if seed is None else seed
        seedrandom(self.seed)   # Seed the random number generator
        self.story              = choice(range(STORY_COUNT))
        self.map                = (np.ones((self.d, self.h, self.w), dtype='<u2') * SOLID)
        self.halls              = [] # All Hallways, in creation order
        self.rooms              = [] # All Rooms, in creation order
        self.places             = [] # Rooms and Halls together, in creation order
        self.layer_places       = [[] for _ in range(self.d)]
        self.layer_areas        = np.zeros(self.d)
        self.layer_densities    = np.zeros(self.d)
        self.layer_params       = [[0, 0, 0, 0, 0, False, 0] for _ in range(self.d)]
        self.layer_bosses       = [[] for _ in range(self.d)]
        self.doors              = []
        self.stairs_up          = []
        self.stairs_down        = []
        self.stairs_inverted    = []
        self.locks              = {}
        self.keys               = {}
        self.loops              = []
        self.root               = None
        self.dead_ends          = 0
        self.dead_ends_removed  = 0
        self.halls_truncated    = 0

        # Failsafe setup
        self.fails              = 0
        self.layer_fails        = np.zeros(self.d)
        self.layer_consec_fails = np.zeros(self.d)
        self.layer_max_consec   = np.zeros(self.d)
        self.layer_complete     = np.zeros(self.d, dtype=np.bool) # Whether or not each layer has met the target density, even if removing halls after reduced that density.
        self.stop_condition     = "Not Yet Generated"

        # Helper functions for fail tracking
        def increment_fails(layer):
            self.fails += 1
            self.layer_fails[layer] += 1
            self.layer_consec_fails[layer] += 1
            self.layer_max_consec[layer] = max(self.layer_max_consec[layer], self.layer_consec_fails[layer])

        def reset_consec_fails(layer):
            self.layer_consec_fails[layer] = 0

        # Non-member variables
        branching_weights       = [] # Branching weight for each place in places. Correct up until we start removing hallways after completing the main while loop

        # INDIVIDUAL LAYER PARAMETER CALCULATIONS
        # Do some precalculations
        locked_radius = min(self.LOCKED_RATIO, 1-self.LOCKED_RATIO) if self.d > 1 else 0
        up_radius     = min(self.UP_STAIR_RATIO, 1-self.UP_STAIR_RATIO) if self.d > 1 else 0
        down_radius   = min(self.DOWN_STAIR_RATIO, 1-self.DOWN_STAIR_RATIO) if self.d > 1 else 0
        loop_radius   = min(self.LOOP_RATIO, 1-self.LOOP_RATIO) if self.d > 1 else 0
        room_radius   = min(self.ROOM_RATIO, 1-self.ROOM_RATIO) if self.d > 1 else 0
        order_radius  = min(self.ORDERLINESS, 1-self.ORDERLINESS)

        # Orderliness
        order_scheme  = np.ones(self.d)*self.ORDERLINESS if self.ORDERLINESS_SCHEME == ORDER_CONSTANT \
            else np.random.uniform(0, 1, size=self.d) if self.ORDERLINESS_SCHEME == ORDER_RANDOM \
            else np.linspace(self.ORDERLINESS+order_radius, self.ORDERLINESS-order_radius, self.d) if self.d > 1 else np.array([self.ORDERLINESS])
        if self.ORDERLINESS_SCHEME == ORDER_THIRDS and self.d > 1:
            divU, divD = self.ORDERLINESS*4/3, self.ORDERLINESS*2/3
            order_scheme[np.logical_and(divD <= order_scheme, order_scheme <= divU)] = self.ORDERLINESS
            order_scheme[order_scheme > divU] = 1
            order_scheme[order_scheme < divD] = 0
        if self.ORDERLINESS_SCHEME == ORDER_SKEW:
            skew_fade = lambda x: 1 / (1 + np.exp((.5-x)*15))
            order_scheme = skew_fade(order_scheme)
        if self.ORDERLINESS_SCHEME == ORDER_JITTER:
            for l in range(self.d):
                r = min(order_scheme[l], 1-order_scheme[l])
                order_scheme[l] = uniform(order_scheme[l]-r, order_scheme[l]+r)

        def layer_room_ratio(layer, boss):
            """ Helper function to find room ratio for a layer."""
            if boss or self.d==1 or not self.VARY_ROOM_RATIO:
                return self.ROOM_RATIO
            else:
                return uniform(self.ROOM_RATIO-room_radius, self.ROOM_RATIO+room_radius)
            # ratio = uniform(self.ROOM_RATIO-room_radius, self.ROOM_RATIO+room_radius)
            # if random() > self.layer_params[layer][PARAM_ORDERLINESS]:
            #     return choices([.97, random(), ratio, .03], [self.ROOM_RATIO, 2, 2, 1-self.ROOM_RATIO])[0]
            # return ratio # TODO: ?

        # BUG: d = Dungeon.new(room_ratio=0, truncate_halls=True, allow_dead_ends=True, down_stair_ratio=.1, up_stair_ratio=.1, boss_layer_count=0, boss_bottom=0, seed=15928713638279554); print(d) # NO UPSTAIR AT TOP!

        # Make Boss Layer List
        if self.BOSS_LAYER_COUNT is not None:
            boss_layers = sample(list(range(self.BOSS_AFTER_LAYER, self.d-self.BOSS_BOTTOM)), self.BOSS_LAYER_COUNT-self.BOSS_BOTTOM) if self.BOSS_LAYER_COUNT else []
            if self.BOSS_BOTTOM: boss_layers.append(self.d-1)
            for l in boss_layers:
                self.layer_params[l][PARAM_BOSS_ROOM    ] = True

        # Iterate over layers to set individual layer parameters
        for l in range(self.d):
            # Boss Layer Stuff
            if self.BOSS_LAYER_COUNT is None:
                boss = True if ((l == self.d-1) and self.BOSS_BOTTOM) \
                        else False if l < self.BOSS_AFTER_LAYER \
                        else (random() < self.BOSS_LAYER_RATIO)
                self.layer_params[l][PARAM_BOSS_ROOM    ] = boss
            else: boss = self.layer_params[l][PARAM_BOSS_ROOM]
            # Everything else
            self.layer_params[l][PARAM_ORDERLINESS      ] = order_scheme[l] 
            self.layer_params[l][PARAM_ROOM_RATIO       ] = layer_room_ratio(l, boss)
            self.layer_params[l][PARAM_LOCKED_RATIO     ] = uniform(self.LOCKED_RATIO-locked_radius, self.LOCKED_RATIO+locked_radius)
            self.layer_params[l][PARAM_DOWN_STAIR_RATIO ] = uniform(self.DOWN_STAIR_RATIO-down_radius, self.DOWN_STAIR_RATIO+down_radius)
            self.layer_params[l][PARAM_UP_STAIR_RATIO   ] = uniform(self.UP_STAIR_RATIO-up_radius, self.UP_STAIR_RATIO+up_radius)
            self.layer_params[l][PARAM_LOOP_RATIO       ] = uniform(self.LOOP_RATIO-loop_radius, self.LOOP_RATIO+loop_radius)

        # SETUP PLACE GENERATION
        def generate_place_from(place, adjacent=False):
            "Attempt to generate a new place off from the place given."
            # Decide whether to build down or adjacent.
            if not place:
                # Make the first room or hall of the dungeon - it always begins with a stair
                x, y, z = randint(1, self.w-2), randint(1, self.h-2), 0
                stair = 1
            elif not adjacent and (random() < self.layer_params[place.z][PARAM_UP_STAIR_RATIO]) and place.z != 0:
                # Up
                z = place.z - 1
                x, y = randint(place.x+1, place.x+place.w-2), randint(place.y+1, place.y+place.h-2)
                stair = -1 # Also counts as true
            elif place.z != self.d-1 and (((self.layer_max_consec[place.z] >= self.MAX_CONSEC_FAILS//2 or self.layer_densities[place.z] >= self.DENSITY) and not self.layer_places[place.z+1]) or (not adjacent and random() < self.layer_params[place.z][PARAM_DOWN_STAIR_RATIO])):
                # Down
                z = place.z + 1
                x, y = randint(place.x+1, place.x+place.w-2), randint(place.y+1, place.y+place.h-2)
                stair = 1
            else:
                # Adjacent
                z = place.z
                try_hall_end = self.layer_fails[z] <= self.HALL_END_TRIES
                if not place.room and random() < self.layer_params[z][PARAM_ORDERLINESS] and try_hall_end: # Orderliness Modifier, tries to generate things at the ends of halls before other places, initially.
                    borders = self.borders(place, borders=[(place.x, place.y+1), (place.x+1, place.y),
                        (place.x+place.w-2, place.y), (place.x+place.w-1, place.y+1),
                        (place.x, place.y+place.h-2), (place.x+1, place.y+place.h-1),
                        (place.x+place.w-2, place.y+place.h-1), (place.x+place.w-1, place.y+place.h-2)]) # Priority borders on halls are at the ends
                else: borders = self.borders(place)
                if not borders: # Empty borders list should be very rare, but possible
                    increment_fails(z)
                    return
                x, y = choice(borders)
                stair = 0
            if self.layer_complete[z] or self.layer_max_consec[z] >= self.MAX_CONSEC_FAILS:
                increment_fails(z)
                return
            orderliness = self.layer_params[z][PARAM_ORDERLINESS]

            # Boss Room stuff
            if self.layer_params[z][PARAM_BOSS_ROOM] and len(self.layer_places[z]) >= self.BOSS_ROOM_DELAY and (not stair or self.BOSS_ALLOW_STAIR):
                boss = True if self.BOSS_LAYER_ROOMS is None else (len(self.layer_bosses[z]) < self.BOSS_LAYER_ROOMS)
                # Make a boss room more likely on a boss layer - otherwise it may not generate soon enough to fit
                need = boss and (len(self.layer_places[z]) >= min(4, self.BOSS_ROOM_DELAY)) and (not len(self.layer_bosses[z]))
                boss = boss and (choice([True, False]) or need) # Otherwise let others generate first sometimes, sort of like an antechamber
            else: boss = False

            # Attempt to build in the place found and decide on Room or Hallway
            pid = len(self.places)
            allow_shrink = random() >= orderliness # Orderliness Modifier # TODO: revisit this for rooms?
            if boss or (not place and bool(self.ROOM_RATIO)) or (self.ROOM_RATIO == 1) or (
                    (random()**((not stair and not try_hall_end) + 1) < self.layer_params[z][PARAM_ROOM_RATIO]) and (
                    len(self.layer_places[z]) > self.HALL_HEADSTART or random() >= orderliness)): # Orderliness Modifier; halls generate more during the headstart, and rooms more after
                # Room generation
                new_place = Room.place(self, pid, x, y, z, place, stair=stair, boss=boss, allow_shrink=allow_shrink)
                room = True
            else:
                # Hallway generation
                if place and not place.room and not stair and place.orientation != PIT and \
                        random() < orderliness and try_hall_end: # Orderliness Modifier # TODO: revisit this
                    new_place = Hallway.place(self, pid, x, y, z, place, orientation=not place.orientation, stair=stair, allow_shrink=allow_shrink)
                else:
                    new_place = Hallway.place(self, pid, x, y, z, place, stair=stair, allow_shrink=allow_shrink)
                room = False

            if new_place:
                # We succeeded in building the room or Hall, so
                # Make the necessary additions to our lists
                reset_consec_fails(z)
                self.write(new_place, write_space=True)
                self.places.append(new_place)
                if room:
                    self.rooms.append(new_place)
                    if boss: branching_weights.append(self.BOSS_BRANCH_WEIGHT)
                    else:    branching_weights.append(self.ROOM_BRANCH_WEIGHT)
                else:
                    self.halls.append(new_place)
                    branching_weights.append(self.HALL_BRANCH_WEIGHT)
                self.layer_places[z].append(new_place)
                self.layer_areas[z] += Dungeon.area_of(new_place)
                self.layer_densities[z] = self.layer_areas[z] / ((self.h-1)*(self.w-1))
                if self.layer_densities[z] >= self.DENSITY: self.layer_complete[z] = True
                if new_place.boss: self.layer_bosses[z].append(new_place)
                if stair == -1: self.stairs_inverted.append(new_place)
            elif place and not stair and random() < self.layer_params[z][PARAM_LOOP_RATIO]: # TODO: adjust how ratio is counted, because it is highly dependent on fails.
                # We failed to build the place, so try making a loop
                other_place = self.find_at(x, y, z, ignore=[place], allow_corners=False, check_borders=True, first_only=True) if place else None
                if other_place and (other_place not in (place.children + [place.parent] + [l[1] for l in place.loops])):
                    reset_consec_fails(z)
                    place.loops.append(((z, y, x), other_place))
                    other_place.loops.append(((z, y, x), place))
                    self.loops.append(((z, y, x), place, other_place))
                else: increment_fails(z) # Failed to build place or loop to place
            else: increment_fails(z)

        # DUNGEON PLACE GENERATION LOOPS
        # Iterate down the dungeon
        finite_backtrack = self.MAX_BACKTRACK is not None
        if finite_backtrack:
            backtrack = min(len(self.places), self.MAX_BACKTRACK+1)
            places_z = [p.z for p in self.places[-backtrack:]]
            while ((not places_z) or (not self.layer_complete.all() and not np.all(self.layer_max_consec[places_z] >= self.MAX_CONSEC_FAILS))) and self.fails < self.MAX_FAILS:
                # Find a Room or Hall to focus on and generate from it
                parent = choices(self.places[-backtrack:], weights=branching_weights[-backtrack:])[0] if self.places else None
                generate_place_from(parent)
                backtrack = min(len(self.places), self.MAX_BACKTRACK+1)
                places_z = [p.z for p in self.places[-backtrack:]]
        # TODO: Need to make it so locked doors placed after have keys closer than the bottom of the dungeon...
        if not finite_backtrack or self.OVERRIDE_BACKTRACK: # Continue here, in case there are layers we want to override backtrack to get to
            while not self.layer_complete.all() and not np.all(self.layer_max_consec >= self.MAX_CONSEC_FAILS) and self.fails < self.MAX_FAILS:
                # Find a Room or Hall to focus on and generate from it
                parent = choices(self.places, weights=branching_weights)[0] if self.places else None # *[.1+self.DENSITY-self.layer_densities[p.z] for p in self.places]
                generate_place_from(parent)#, adjacent=finite_backtrack) # Seems to slow it down, actually, without seeming to decrease the number of stairs.
        self.stop_condition = "Layers Density Met" if np.all(self.layer_complete) \
            else "Total Fails" if self.fails >= self.MAX_FAILS \
            else "All Layers Consec Fails" if np.all(self.layer_max_consec >= self.MAX_CONSEC_FAILS) \
            else "Backtrackable Layers Consec Fails"
        self.density_preclean = np.mean(self.layer_densities) # NOTE: for Density, we use an average of averages. In this case, since each layer is the same size, this is not incorrect.

        # CLEAN UP
        # Clean up boss layer misses
        for l in range(self.d):
            self.layer_params[l][PARAM_BOSS_ROOM] = bool(self.layer_bosses[l])

        # Move backward through the halls to clean up loose ends
        for i, h in enumerate(self.halls[::-1]):
            remove = False
            dead = not h.children and not h.loops
            cull = random() < self.layer_params[h.z][PARAM_ORDERLINESS] # Orderliness Modifier
            if dead and not h.contents:
                if self.ALLOW_DEAD_ENDS == False or (self.ALLOW_DEAD_ENDS == None and cull):
                    # Dead end hall we're going to remove
                    remove = True
                    self.dead_ends_removed += 1
                    self.write(h, write_space=True, erase=True)
                    self.halls[len(self.halls)-i-1] = None
                    self.places[h.pid] = None
                    self.layer_places[h.z].remove(h)
                    self.layer_areas[h.z] -= Dungeon.area_of(h)
                    self.layer_densities[h.z] = self.layer_areas[h.z] / ((self.h-1)*(self.w-1))
                    h.parent.children.remove(h)
                    if h is self.root: self.root = None # Edge case
                    del(h)
            elif self.TRUNCATE_HALLS == True or (self.TRUNCATE_HALLS == None and cull):
                # Hall with too-long ends we're going to shorten
                changed = False
                if h.orientation == UD: # Vertical Hall
                    U, D = h.y+1, h.y+h.h-2
                    y_coords = np.clip([h.door_in[1]] + [c.door_in[1] for c in h.children] + [l[0][1] for l in h.loops] + [k[0][1] for k in h.contents], U, D)
                    u, d = min(y_coords), max(y_coords)
                    if u > U or d < D:
                        self.write(h, write_space=True, erase=True, only_overwrite_symbol=SPACE) # Would work without only_overwrite_symbol because stairs, doors, loops, and contents not drawn yet!
                        area = Dungeon.area_of(h)
                        h.length = min(h.length, d-u+3)
                        h.h = h.length
                        h.y = max(h.y, u-1)
                        changed = True
                elif h.orientation == LR: # Horizontal Hall
                    L, R = h.x+1, h.x+h.w-2
                    x_coords = np.clip([h.door_in[2]] + [c.door_in[2] for c in h.children] + [l[0][2] for l in h.loops] + [k[0][2] for k in h.contents], L, R)
                    l, r = min(x_coords), max(x_coords)
                    if l > L or r < R:
                        self.write(h, write_space=True, erase=True, only_overwrite_symbol=SPACE) # ''
                        area = Dungeon.area_of(h)
                        h.length = min(h.length, r-l+3)
                        h.w = h.length
                        h.x = max(h.x, l-1)
                        changed = True
                if changed:
                    h.area = h.length-2
                    h.perimeter_area = 2*h.length + 2*(self.HALL_BREADTH) - 4
                    self.layer_areas[h.z] -= area-Dungeon.area_of(h)
                    self.layer_densities[h.z] = self.layer_areas[h.z] / ((self.h-1)*(self.w-1))
                    self.write(h, write_space=True, only_overwrite_symbol=SOLID) # ''
                    self.halls_truncated += 1
            if dead and not remove:
                self.dead_ends += 1
        # Filter the lists to update the changes
        f = (lambda x: x is not None)
        self.halls  = list(filter(f, self.halls ))
        self.places = list(filter(f, self.places))
        for i, p in enumerate(self.places): p.pid = i # Update the PID's
        self.density = np.mean(self.layer_densities) # NOTE: See self.density_preclean code for note on average of averages.

        # DRAW DOORS, GENERATE LOCKS
        room_index = -1
        for p in self.places:
            if p.room: room_index += 1
            stair = 1 if (not p.parent) or (p.parent.z < p.z) else 0 if p.z == p.parent.z else -1
            z, y, x = p.door_in
            if stair == 1:
                # Stair down
                self.stairs_down.append((z-1, y, x))
                self.stairs_up.append((z, y, x))
                # Places don't draw stairs by themselves
                if p.parent: self.map[z-1, y, x] = STAIR_DOWN if self.map[z-1, y, x] != STAIR_UP else STAIR_BOTH
                self.map[z, y, x] = STAIR_UP if self.map[z, y, x] != STAIR_DOWN else STAIR_BOTH
            elif stair == -1:
                # Stair up
                self.stairs_up.append((z+1, y, x))
                self.stairs_down.append((z, y, x))
                # Places don't draw stairs by themselves
                self.map[z, y, x] = STAIR_DOWN if self.map[z, y, x] != STAIR_UP else STAIR_BOTH
                self.map[z+1, y, x] = STAIR_UP if self.map[z+1, y, x] != STAIR_DOWN else STAIR_BOTH
            else:
                # Adjacent
                self.doors.append((z, y, x))
                # Decide whether to lock this one
                key_id = None
                if (self.HALL_LOCKS or p.room) and random() < self.layer_params[z][PARAM_LOCKED_RATIO] and (room_index+1 > p.room):
                    if self.MAX_KEY_BACKTRACK is None:
                        key_room = choice(self.rooms[:-1]) if p.room else choice(self.rooms)
                    else:
                        key_room = choice(self.rooms[max(0, room_index-self.MAX_KEY_BACKTRACK-p.room) : room_index-p.room+1])
                    key_coords = (key_room.z, randint(key_room.y+1, key_room.y+key_room.h-2), randint(key_room.x+1, key_room.x+key_room.w-2))
                    if self.map[key_coords] == SPACE:
                        # If many very small rooms and/or many stairs, ratio of locked doors will go down.
                        key_id = len(self.keys)
                        key_room.contents.append(((z, y, x), key_id))
                        self.locks[(z,y,x)] = key_id
                        for loop in p.loops:
                            if p.pid > loop[1].pid: self.locks[loop[0]] = key_id
                        self.keys[key_coords] = key_id
                        self.map[key_coords] = KEY
            self.write(p, write_space=False, write_door_in=True, write_loops=True) # Need to draw doors, even if what we draw is SPACE

        # GENERATE MONSTERS, BOSSES, TREASURES, MAPS
        # TODO:

        # Change room borders
        if self.HIGHLIGHT_BORDERS:
            for p in self.places:
                for x in range(p.x, p.x + p.w):
                    y2 = p.y + p.h - 1
                    if self.map[p.z, p.y, x] == SOLID: self.map[p.z, p.y, x] = PART3
                    if self.map[p.z, y2,  x] == SOLID: self.map[p.z, y2,  x] = PART3
                for y in range(p.y+1, p.y+p.h-1):
                    x2 = p.x + p.w - 1
                    if self.map[p.z, y, p.x] == SOLID: self.map[p.z, y, p.x] = PART3
                    if self.map[p.z, y, x2 ] == SOLID: self.map[p.z, y, x2 ] = PART3

        # Stop the timer
        self.time = time() - start_time

        # Print some stats of the resulting dungeon
        if report: print(self.report() + "   ", end="\r")

    @staticmethod
    def new(recipe="medium", *args, **kwargs): # TODO: Combine with init function
        # NOTE: Common kwargs to add: report, seed, highlight_borders, w, h, d.
        #   But, you can also replace recipe params.
        # ALSO make it so it can take multiple recipes?

        # Random recipe generator
        if recipe == "random":
            recipe = choice(list(Dungeon.RECIPES.keys()))
            print(f"Using recipe: {recipe}")
            # TODO: Maybe add random generator that acts in place of this half of the time.

        # In case no recipe is given
        if '=' in recipe:
            use_recipe = Dungeon.RECIPES["medium"].copy()
            args = (recipe, *args)
        else:
            use_recipe = Dungeon.RECIPES[recipe].copy()

        # In case the user wants to overwrite recipe params, not just add to them, this lets them do it:
        for key in kwargs:
            use_recipe[key] = kwargs[key]
        # In case the user wants to overwrite recipe params when executing as script:
        for arg in args:
            if '=' not in arg: raise ValueError(f"Unable to determine intended argument for value {arg}")
            key, val = arg.split('=')
            val = val.lower()
            if '.' in val: val = float(val)
            elif val == "none": val = None
            elif val == "true": val = True
            elif val == "false": val = False
            else:
                try:
                    val = int(val)
                except:
                    pass
            use_recipe[key] = val

        return Dungeon(**use_recipe)

    RECIPES = {

        "easy" : { # Default Easy Settings
            "d"                 : 5,
            "down_stair_ratio"  : 0,
            "up_stair_ratio"    : 0,
            "max_backtrack"     : 1,
            "max_key_backtrack" : 2,
            "boss_layer_ratio"  : 0,
            "boss_layer_rooms"  : 1,
            "loop_ratio"        : 0,
        },

        "medium" : {}, # Default Settings

        "hard" : { # Default Hard Settings
            "d"                 : 15,
            "down_stair_ratio"  : .05,
            "up_stair_ratio"    : .2,
            "hall_locks"        : True,
            "boss_after_layer"  : 2,
            "locked_ratio"      : .3,
            "boss_allow_stair"  : True,
        },

        "huge" : { # An Enormous Dungeon
            "w"                 : TERM_W,
            "h"                 : TERM_H*2,
            "d"                 : 30,
            "max_hall"          : 28,
            "boss_layer_rooms"  : 3,
            "boss_room_delay"   : 40,
            "max_backtrack"     : 40,
            "max_key_backtrack" : 20,
            "loop_ratio"        : .01,
        },

        "tiny" : {
            "w"                 : 20, 
            "h"                 : 12, 
            "d"                 : 5, 
            "min_room"          : 2, 
            "max_room"          : 4, 
            "min_hall"          : 2, 
            "max_hall"          : 9, 
            "boss_min_room"     : 4, 
            "boss_max_room"     : 10, 
            "boss_min_area"     : 50, 
            "boss_layer_count"  : 1, 
            "max_backtrack"     : 15, 
            "loop_ratio"        : .01,
            "locked_ratio"      : .5, 
            "orderliness"       : 1, 
            "room_ratio"        : .9, 
            "density"           : .9, 
            "hall_headstart"    : 2,
        },

        "square" : {
            "h"                 : TERM_W // 2,
        },

        "tiny_square" : {
            "w"                 : 20, 
            "h"                 : 20, 
            "d"                 : 5, 
            "min_room"          : 2, 
            "max_room"          : 4, 
            "min_hall"          : 2, 
            "max_hall"          : 9, 
            "boss_min_room"     : 4, 
            "boss_max_room"     : 10, 
            "boss_min_area"     : 50, 
            "boss_layer_count"  : 1, 
            "max_backtrack"     : 15, 
            "loop_ratio"        : .01,
            "locked_ratio"      : .3,
            "orderliness"       : 1,
            "room_ratio"        : .75,
        },

        "no_halls" : { # Extreme Test
            "room_ratio"        : 1, # TODO orderliness or something is causing halls to generate
        },

        "no_rooms" : { # Extreme Test
            "room_ratio"        : 0,
            "boss_layer_count"  : 0,
            "boss_bottom"       : False,
            "allow_dead_ends"   : True,
        },

        "one_layer" : { # Large, one-layer dungeon
            "w"                 : TERM_W,
            "h"                 : TERM_H*2,
            "d"                 : 1,
            # "max_hall"          : 28,
            "boss_layer_ratio"  : 0,
            "boss_layer_rooms"  : 1,
            "boss_room_delay"   : 40,
            "max_backtrack"     : None,
            "max_key_backtrack" : None,
            "loop_ratio"        : .01,
            "truncate_halls"    : True,
            "allow_dead_ends"   : False,
            "density"           : FULL_DENSITY,
            "room_ratio"        : .4,
        },

        "base" : { # Large, organized bunker
            "w"                 : TERM_W,
            "h"                 : TERM_H*2,
            "d"                 : 1,
            "max_hall"          : 28,
            "boss_layer_ratio"  : 0,
            "boss_layer_rooms"  : 1,
            "boss_room_delay"   : 40,
            "max_backtrack"     : None,
            "max_key_backtrack" : None,
            "density"           : FULL_DENSITY,
            "room_ratio"        : .4,
            "orderliness"       : 1,
            "hall_headstart"    : 20,
        },

        # "strip" : { # Very long, one-layer dungeon # TODO: need to make it so I can specify the start location...
        #     "w"                 : TERM_H,
        #     "h"                 : 300,
        #     "d"                 : 1, # TODO: BUG!
        #     "boss_layer_ratio"  : 0,
        #     "boss_layer_rooms"  : 3,
        #     "boss_room_delay"   : 40, # TODO: make it so it waits this long between each?
        #     "density"           : .9, # TODO: increase consec fails?
        # },

        "convoluted" : { # High branching and more up-stairs
            "max_backtrack"     : None,
            "max_key_backtrack" : None,
            "up_stair_ratio"    : .1,
            "down_stair_ratio"  : .05,
            "locked_ratio"      : .35,
            "loop_ratio"        : .3,
            "orderliness"       : 0,
            "boss_allow_stair"  : True,
        },

        "orderly" : { # High orderliness
            "orderliness"       : 1,
            "loop_ratio"        : 0,
        },

        "maze" : { # Mostly passages, more up-stairs
            "up_stair_ratio"    : .1,
            "room_ratio"        : .25,
            "max_backtrack"     : None,
            "max_key_backtrack" : 30,
            "loop_ratio"        : 0, # Change a little for harder mazes
            "orderliness"       : 0,
            "boss_layer_count"  : 0, # Bottom is still one
            "boss_layer_rooms"  : 1,
        },

        "one_path" : { # Direct route
            "down_stair_ratio"  : 0,
            "up_stair_ratio"    : 0, # Necessary because of potential entrapment
            "max_backtrack"     : 0,
            "locked_ratio"      : 0,
            "loop_ratio"        : 0,
            "override_backtrack": False,
            "orderliness"       : 0,
            "max_consec_fails"  : 300,
            # "truncate_halls"    : True,
        },

        "webbed" : { # All possible loops
            "loop_ratio"        : 1,
            "boss_allow_stair"  : True,
        },

        "web" : { # All possible loops, no rooms
            "loop_ratio"        : 1,
            "room_ratio"        : 0,
            "boss_layer_count"  : 0,
            "boss_bottom"       : False,
            "allow_dead_ends"   : True,
            "hall_headstart"    : 4,
        },

        "sparse" : { # Low density, longer passages
            "min_hall"          : 5,
            "max_hall"          : 24,
            "hall_headstart"    : 5,
            "density"           : .4,
            "boss_layer_rooms"  : 2,
        },

        "dense" : { # High density, shorter passages, smaller rooms
            "min_hall"          : 2,
            "max_hall"          : 14,
            "hall_headstart"    : 0,
            "density"           : .95,
            "max_consec_fails"  : 400,
            "max_backtrack"     : 40,
            "max_fails"         : 20000,
        },

        "dnd" : { # Example better suited to Dungeons and Dragons
            "d"                 : 4,
            "boss_layer_ratio"  : 0, # Still one on the bottom though
            "density"           : .42,
            "max_backtrack"     : 4,
            "truncate_halls"    : True,
            "boss_layer_rooms"  : 1,
        },

        "goblin" : { # An unorderly Goblin Warren
            "w"                 : TERM_W,
            "h"                 : TERM_H*2,
            "max_hall"          : 12,
            "density"           : .4,
            "room_ratio"        : .35,
            "loop_ratio"        : .8,
            "allow_dead_ends"   : True,
            "truncate_halls"    : False,
            "hall_headstart"    : 0,
            "boss_layer_rooms"  : 1,
            "orderliness_scheme": ORDER_RANDOM,
            "max_backtrack"     : None,
            "up_stair_ratio"    : .2,
            "down_stair_ratio"  : .2,
            "room_branch_weight": 3,
            "hall_branch_weight": 1,
            "boss_branch_weight": 8,
            "boss_allow_stair"  : True,
        },

        "dwarven" : { # A fairly orderly Dwarven Outpost. This generator isn't designed to do full-on patternized orderliness.
            "w"                 : 32,
            "h"                 : 32,
            "orderliness"       : 1,
            "density"           : .85,
            "loop_ratio"        : .5,
            "max_backtrack"     : 7,
            "vary_room_ratio"   : False,
            "up_stair_ratio"    : 0,
            "down_stair_ratio"  : 0,
            "boss_branch_weight": 4,
        },

        "random" : {}, # Randomize the parameters themselves. This is post-processed, since it can't be stored without de-randomizing it.
    }



# Script behavior - instant dungeon on screen
if __name__ == "__main__":
    dungeon = Dungeon.new(*argv[1:])
    print('\n', dungeon, sep='')