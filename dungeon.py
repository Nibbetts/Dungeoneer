import numpy as np
from random import randint, choice, random, uniform, sample, choices
from random import seed as seedrandom
from time import time
from abc import ABC, abstractstaticmethod
# NOTE: randint is inclusive on both ends

from stories import COUNT as STORY_COUNT



# Enumerations
LR, UD, PIT = 0, 1, 2 # Hallway orientations
PARAM_ROOM_RATIO, PARAM_LOCKED_RATIO, PARAM_DOWN_STAIR_RATIO, PARAM_UP_STAIR_RATIO, PARAM_LOOP_RATIO, PARAM_BOSS_ROOM, PARAM_ORDERLINESS = 0, 1, 2, 3, 4, 5, 6 # Layer parameter indices

# Size Defaults and Param Constants
TERM_W, TERM_H = 80, 24 # Standard terminal width and height
HALL_BREADTH = 1        # Can't change this without bugs
FULL_DENSITY = .83
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
        if allow_shrink or met_bound != 2: # Slightly lessens the largeness of rooms resulting from orderliness parameter being high
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
            min_room=None,       max_room=8,
            min_hall=3,          max_hall=18,          hall_breadth=1,
            boss_min_room=8,     boss_max_room=20,     boss_min_area=170,
            boss_bottom=True,    boss_after_layer=3,   boss_layer_rooms=None,
            boss_layer_ratio=.2, boss_room_delay=0,    boss_layer_count=None,
            boss_allow_stair=False,
            max_backtrack=20,    max_key_backtrack=10,
            max_density=1,       min_density=0,        # Non-restrictive defaults
            locked_ratio=.25,    down_stair_ratio=.08, up_stair_ratio=.02,
            room_ratio=.5,       loop_ratio=.1,        vary_room_ratio=True,
            boss_branch_weight=8,room_branch_weight=1, hall_branch_weight=2,
            hall_doors=False,    hall_locks=False,     #loop_locks=True,
            top_orderliness=1,   bottom_orderliness=0, fade_orderliness=True,
            allow_dead_ends=None,truncate_halls=None,
            consec_fails=20,     max_consec_fails=100, max_fails=10000,
            highlight_borders=False,
            report=True, seed=None ): # Same seed will only rebuild the same dungeon if given the same parameters!

        # Check some input validity
        if w < 5 or h < 5 or d < 1 \
                or (min_room and min_room < 1) or max_room < 1 or min_hall < 1 or max_hall < 1 or hall_breadth < 1 \
                or boss_max_room <= max_room or boss_min_room < 1 or boss_min_area < 1 or boss_min_area > boss_max_room**2 \
                or boss_after_layer < 0 or (boss_layer_rooms and boss_layer_rooms < 0) or boss_layer_ratio < 0 or boss_layer_ratio > 1 \
                or boss_room_delay < 0 or (boss_layer_count and (boss_layer_count < 0 or boss_layer_count > d-boss_after_layer)) \
                or (max_backtrack and max_backtrack < 0) or (max_key_backtrack and max_key_backtrack < 0) \
                or max_density > 1 or max_density <= 0 or min_density > max_density or min_density > 1 or min_density < 0 \
                or locked_ratio < 0 or locked_ratio > 1 or down_stair_ratio < 0 or down_stair_ratio > 1 or up_stair_ratio < 0 or up_stair_ratio > 1 \
                or room_ratio < 0 or room_ratio > 1 or loop_ratio < 0 or loop_ratio > 1 \
                or top_orderliness > 1 or top_orderliness < 0 or bottom_orderliness > 1 or bottom_orderliness < 0 \
                or boss_branch_weight < 0 or room_branch_weight < 0 or hall_branch_weight < 0 \
                or consec_fails < 1 or max_consec_fails < 1 or max_fails < 1:
            raise(ValueError("Invalid value given in Dungeon parameters"))
        if hall_breadth != HALL_BREADTH: raise(NotImplementedError("Wider halls not currently implemented"))
        if min_density > FULL_DENSITY: print(f"WARNING: min_density > {FULL_DENSITY} is likely to generate large failure counts and be impossible to reach")

        # Global Dungeon Proportion Characteristics
        self.w = w # Dungeon Width
        self.h = h # Dungeon Height
        self.d = d # Dungeon Depth

        self.MIN_ROOM           = (min_room      + 2) if min_room else min_room # min_room defaults to None for the special size 2x3, neither 2 nor 3
        self.MAX_ROOM           = max_room       + 2 # Max dimension for a normal room
        self.MIN_HALL           = min_hall       + 2 # Min hall length
        self.MAX_HALL           = max_hall       + 2 # Max hall length
        self.HALL_BREADTH       = hall_breadth   + 2 # Hall Breadth
        
        # Boss Layer/Room parameters
        self.BOSS_MIN_ROOM      = boss_min_room  + 2 # Min dimension for a boss room
        self.BOSS_MAX_ROOM      = boss_max_room  + 2 # Max dimension for a boss room
        self.BOSS_MIN_AREA      = boss_min_area      # Minimum inner area of acceptable boss rooms
        self.BOSS_BOTTOM        = boss_bottom        # Force bottom layer to be a boss layer.
        self.BOSS_AFTER_LAYER   = boss_after_layer   # Won't generate boss rooms till after this many layers, ie, maybe as soon as this layer
        self.BOSS_LAYER_ROOMS   = boss_layer_rooms   # Limit boss room count per layer.
        self.BOSS_LAYER_RATIO   = boss_layer_ratio   # How often a layer is a boss room layer. Not used if boss_layer_count is used. Only applied where not contrary to boss_bottom and boss_after_layer. Superceded by BOSS_BOTTOM.
        self.BOSS_ROOM_DELAY    = boss_room_delay    # How many rooms/halls into a boss layer we'll start trying to make a boss room. Useful for large one-layer maps.
        self.BOSS_LAYER_COUNT   = boss_layer_count   # Attempt to force a specific number of boss layers (won't be more than this). Superceded by BOSS_BOTTOM.
        self.BOSS_ALLOW_STAIR   = boss_allow_stair   # Allow stairs to be the first entrance to a boss room (Loops of course will mean you may be able to enter another way).

        # Complexity and other Global Characteristics
        self.MAX_BACKTRACK      = max_backtrack      # How many rooms/halls back we might branch anew. This is not a direct control, but largely influences branching and the likelihood of long independent branches.
        self.MAX_KEY_BACKTRACK  = max_key_backtrack  # How many rooms (only) back we might place a key from it's door
        self.MAX_DENSITY        = max_density        # May not be met, but stops after this density. # TODO: use this and min as well!
        self.MIN_DENSITY        = min_density        # May not be met, especially with low backtracking or low fail limits or high down stair ratio
        self.VARY_ROOM_RATIO    = vary_room_ratio    # Use a varied trimodal distribution for room/hall ratios (Turn off to get all layers to match ratios)
        self.BOSS_BRANCH_WEIGHT = boss_branch_weight # How boss rooms are comparatively weighted when deciding which recent place to randomly branch off of; higher numbers compared to other branching weights make it more likely.
        self.ROOM_BRANCH_WEIGHT = room_branch_weight # ''  other rooms ''
        self.HALL_BRANCH_WEIGHT = hall_branch_weight # ''  hallways    ''
        self.HALL_DOORS         = hall_doors         # Whether to draw the doors in hallways as doors or as spaces (will still draw them if locked. See hall_locks).
        self.HALL_LOCKS         = hall_locks         # Whether or not to lock some hall doors as we would rooms.

        # Global Characteristics that may be locally modified
        self.LOCKED_RATIO       = locked_ratio       # Likelihood to attempt to lock a door
        self.DOWN_STAIR_RATIO   = down_stair_ratio   # Likelihood to go down when we don't have to, or of having more down stair branching
        self.UP_STAIR_RATIO     = up_stair_ratio     # Likelihood of going up (Adds complexity, more maze-like)
        self.ROOM_RATIO         = room_ratio         # Probability ratio of rooms to halls (not including boss rooms)
        self.LOOP_RATIO         = loop_ratio         # Likelihood of creating a loop when the option is found

        # Orderliness
        self.TOP_ORDERLINESS    = top_orderliness    # Generally how organized rooms and hallways are at most, or at the top of the dungeon if FADE_ORDERLINESS.
        self.BOTTOM_ORDERLINESS = bottom_orderliness # Minimum orderliness, or orderliness at the bottom of the dungeon if FADE_ORDERLINESS.
        self.FADE_ORDERLINESS   = fade_orderliness   # If True, order will change in a gradient from top to bottom of dungeon. If false, will pick a random value between top and bottom for each layer.
        self.ALLOW_DEAD_ENDS    = allow_dead_ends    # If True, none are removed; if False, all are removed; if None, follows orderliness based probability.
        self.TRUNCATE_HALLS     = truncate_halls     # If True, all are shortened; if False, none are; if None, follows orderliness based probability. NOTE: Truncating halls can result in halls shorter than min_hall

        # Procedural Generation Boundaries           # NOTE: fail count limits tend to affect how filled in a dungeon is when generation finishes.
        self.CONSEC_FAILS       = consec_fails       # Typical limit on consecutive location placement fails for some aspects of dungeon generation
        self.MAX_CONSEC_FAILS   = max_consec_fails   # Hard limit on consecutive fails before stopping the generator
        self.MAX_FAILS          = max_fails          # Hard limit on total fails before stopping the generator
        # TODO: replace fails limits with NONE default and a calculation based on dungeon volume?
        
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
        # self.layer_params      # List of layer parameter lists, in depth order
        # self.layer_bosses      # List of lists of boss rooms in each layer
        # self.layer_fails       # Array of per-layer fail counts
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

    def __str__(self):
        s = "\n"
        for l in range(self.d):
            s += f"Level {l}, Density: {self.layer_areas[l] / ((self.h-1)*(self.w-1)):.2f}\n"
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
            +  f"             Rooms: {len(self.rooms)}\n" \
            +  f"             Halls: {len(self.halls)}\n" \
            +  f"             Doors: {len(self.doors)}\n" \
            +  f"              Keys: {len(self.keys)}\n" \
            +  f"             Locks: {len(self.locks)}\n" \
            +  f"            Stairs: {len(self.stairs_up)}\n" \
            +  f"   Inverted Stairs: {len(self.stairs_inverted)}\n" \
            +  f"             Loops: {len(self.loops)}\n" \
            +   "\n" \
            +  f"         Dead Ends: {self.dead_ends}\n" \
            +  f" Dead Ends Removed: {self.dead_ends_removed}\n" \
            +  f"   Halls Truncated: {self.halls_truncated}\n" \
            +   "\n" \
            +  f"             Fails: {self.fails}\n" \
            +  f" Consecutive Fails: {self.max_consec}\n" \
            +   "\n" \
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
        self.consec_fails       = 0
        self.max_consec         = 0
        self.layer_fails        = np.zeros(self.d)
        self.layer_consec_fails = np.zeros(self.d)
        self.layer_max_consec   = np.zeros(self.d)
        
        # Helper functions for fail tracking
        def increment_fails(layer):
            self.fails += 1
            self.consec_fails += 1
            self.max_consec = max(self.max_consec, self.consec_fails)

            self.layer_fails[layer] += 1
            self.layer_consec_fails[layer] += 1
            self.layer_max_consec[layer] = max(self.layer_max_consec[layer], self.layer_consec_fails[layer])

        def reset_consec_fails(layer):
            self.consec_fails = 0
            self.layer_consec_fails[layer] = 0

        # Non-member variables
        orderly_hall_start      = round(2 * self.w * self.h / self.MAX_HALL**2)
        branching_weights       = [] # Branching weight for each place in places. Correct up until we start removing hallways after completing the main while loop
        layer_complete          = np.zeros(self.d)

        # INDIVIDUAL LAYER PARAMETER CALCULATIONS
        # Do some precalculations
        locked_radius = min(self.LOCKED_RATIO, 1-self.LOCKED_RATIO) if self.d > 1 else 0
        up_radius     = min(self.UP_STAIR_RATIO, 1-self.UP_STAIR_RATIO) if self.d > 1 else 0
        down_radius   = min(self.DOWN_STAIR_RATIO, 1-self.DOWN_STAIR_RATIO) if self.d > 1 else 0
        loop_radius   = min(self.LOOP_RATIO, 1-self.LOOP_RATIO) if self.d > 1 else 0
        orderliness   = np.linspace(self.TOP_ORDERLINESS, self.BOTTOM_ORDERLINESS, self.d) if self.FADE_ORDERLINESS and self.d > 1 else None
        # Make Boss Layer List
        if self.BOSS_LAYER_COUNT is not None:
            boss_layers = sample(list(range(self.BOSS_AFTER_LAYER, self.d-self.BOSS_BOTTOM)), self.BOSS_LAYER_COUNT-self.BOSS_BOTTOM)
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
            self.layer_params[l][PARAM_ROOM_RATIO       ] = self.ROOM_RATIO if (boss or self.d==1 or not self.VARY_ROOM_RATIO) \
                                                            else random() # TODO: revisit this.
                                                            # else choice([.97, self.ROOM_RATIO, self.ROOM_RATIO, .03]) if (random() < .0) \
            self.layer_params[l][PARAM_LOCKED_RATIO     ] = uniform(self.LOCKED_RATIO-locked_radius, self.LOCKED_RATIO+locked_radius)
            self.layer_params[l][PARAM_DOWN_STAIR_RATIO ] = uniform(self.DOWN_STAIR_RATIO-down_radius, self.DOWN_STAIR_RATIO+down_radius)
            self.layer_params[l][PARAM_UP_STAIR_RATIO   ] = uniform(self.UP_STAIR_RATIO-up_radius, self.UP_STAIR_RATIO+up_radius)
            self.layer_params[l][PARAM_LOOP_RATIO       ] = uniform(self.LOOP_RATIO-loop_radius, self.LOOP_RATIO+loop_radius)
            self.layer_params[l][PARAM_ORDERLINESS      ] = uniform(self.TOP_ORDERLINESS, self.BOTTOM_ORDERLINESS) if orderliness is None else orderliness[l] 


        # DUNGEON PLACES GENERATION LOOP
        # Iterate down the dungeon
        # TODO: fix end conditions to work with density instead, keep track of local density, and local fails, and layers/places that haven't failed out?
        # Stop only once the bottom layers fill up enough
        # while len(self.layer_places[self.d-1]) < 5 and self.consec_fails < 15:
        # while not np.all(layer_complete) or 
        while ((not self.places or self.places[-1].z != self.d-1) and self.consec_fails < self.CONSEC_FAILS) \
                or self.consec_fails < self.CONSEC_FAILS or len(self.layer_places[self.d-1]) < 5 \
                and self.consec_fails < self.MAX_CONSEC_FAILS and self.fails < self.MAX_FAILS:
            # Find a Room or Hall to focus on
            if self.MAX_BACKTRACK is None:
                place = choices(self.places, weights=branching_weights)[0] if self.places else None
            else:
                backtrack = min(len(self.places), self.MAX_BACKTRACK+1)
                place = choices(self.places[-backtrack:], weights=branching_weights[-backtrack:])[0] if self.places else None

            # Decide whether to build down or adjacent.
            if not place:
                # Make the first room or hall of the dungeon - it always begins with a stair
                x, y, z = randint(1, self.w-2), randint(1, self.h-2), 0
                stair = 1
            elif (random() < self.layer_params[place.z][PARAM_UP_STAIR_RATIO]) and place.z != 0:
                # Up
                z = place.z - 1
                x, y = randint(place.x+1, place.x+place.w-2), randint(place.y+1, place.y+place.h-2)
                stair = -1 # Also counts as true
            elif (self.consec_fails > self.CONSEC_FAILS/2 or random() < self.layer_params[place.z][PARAM_DOWN_STAIR_RATIO]) and place.z != self.d-1: # TODO: check if shouldn't be based on consec_fails
                # Down
                z = place.z + 1
                x, y = randint(place.x+1, place.x+place.w-2), randint(place.y+1, place.y+place.h-2)
                stair = 1
            else:
                # Adjacent
                z = place.z
                layer_fails_half = self.layer_fails[z] >= self.CONSEC_FAILS//2 # For orderliness, halls generate more before this is true, and rooms more after
                if not place.room and random() < self.layer_params[z][PARAM_ORDERLINESS] and not layer_fails_half: # Orderliness Modifier
                    borders = self.borders(place, borders=[(place.x, place.y+1), (place.x+1, place.y),
                        (place.x+place.w-2, place.y), (place.x+place.w-1, place.y+1),
                        (place.x, place.y+place.h-2), (place.x+1, place.y+place.h-1),
                        (place.x+place.w-2, place.y+place.h-1), (place.x+place.w-1, place.y+place.h-2)]) # Priority borders on halls are at the ends
                else: borders = self.borders(place)
                if not borders: # Empty borders list should be very rare, but possible
                    increment_fails(z)
                    continue
                x, y = choice(borders)
                stair = 0
            orderly = self.layer_params[z][PARAM_ORDERLINESS]

            # Boss Room stuff
            if self.layer_params[z][PARAM_BOSS_ROOM] and len(self.layer_places[z]) >= self.BOSS_ROOM_DELAY and (not stair or self.BOSS_ALLOW_STAIR):
                boss = True if self.BOSS_LAYER_ROOMS is None else (len(self.layer_bosses[z]) < self.BOSS_LAYER_ROOMS)
                # Make a boss room more likely on a boss layer - otherwise it may not generate soon enough to fit
                need = boss and (len(self.layer_places[z]) >= min(4, self.BOSS_ROOM_DELAY)) and (not len(self.layer_bosses[z]))
                boss = boss and (choice([True, False]) or need) # Otherwise let others generate first sometimes, sort of like an antechamber
            else: boss = False

            # Attempt to build in the place found and decide on Room or Hallway
            pid = len(self.places)
            allow_shrink = random() >= orderly # Orderliness Modifier # TODO: revisit this for rooms?
            if boss or (not place and bool(self.ROOM_RATIO)) or ((random()**((not stair and layer_fails_half) + 1) < self.layer_params[z][PARAM_ROOM_RATIO]) and (
                    len(self.layer_places[z]) > orderly_hall_start or random() >= orderly)): # Orderliness Modifier
                # Room generation
                new_place = Room.place(self, pid, x, y, z, place, stair=stair, boss=boss, allow_shrink=allow_shrink)
                room = True
            else:
                # Hallway generation
                if place and not place.room and not stair and place.orientation != PIT and \
                        random() < orderly and not layer_fails_half: # Orderliness Modifier
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
                if new_place.boss: self.layer_bosses[z].append(new_place)
                if stair == -1: self.stairs_inverted.append(new_place)
            elif place and not stair and random() < self.layer_params[z][PARAM_LOOP_RATIO]: # TODO: adjust how ratio is counted, because it is highly dependent on fails.
                # We failed to build the place, so try making a loop
                other_place = self.find_at(x, y, z, ignore=[place], allow_corners=False, check_borders=True, first_only=True) if place else None
                if other_place and (other_place not in (place.children + [place.parent] + [l[1] for l in place.loops])):
                    place.loops.append(((z, y, x), other_place))
                    other_place.loops.append(((z, y, x), place))
                    self.loops.append(((z, y, x), place, other_place))
                else: increment_fails(z) # Failed to build place or loop to place
            else: increment_fails(z)

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
                    self.write(h, write_space=True, only_overwrite_symbol=SOLID) # ''
                    self.halls_truncated += 1
            if dead and not remove:
                self.dead_ends += 1
        # Filter the lists to update the changes
        f = (lambda x: x is not None)
        self.halls  = list(filter(f, self.halls ))
        self.places = list(filter(f, self.places))
        for i, p in enumerate(self.places): p.pid = i # Update the PID's

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
    def new(recipe="default", **kwargs):
        # NOTE: Common kwargs to add: report, seed, highlight_borders, w, h, d.
        #   But, you can also replace recipe params.

        # In case the user wants to overwrite recipe params, not just add to them, this lets them do it:
        use_recipe = Dungeon.RECIPES[recipe]
        for key in kwargs:
            use_recipe[key] = kwargs[key]

        return Dungeon(**use_recipe)

    RECIPES = {
        "default" : {}, # TODO: Somewhere between medium and hard?

        "easy" : { # Default Easy Settings
            "down_stair_ratio"  : 0,
            "up_stair_ratio"    : 0,
            "max_backtrack"     : 2,
            "max_key_backtrack" : 3,
            "boss_layer_ratio"  : 0,
            "boss_layer_rooms"  : 1,
            "loop_ratio"        : 0,
            # TODO: "consec_fails"      : 100,
        },

        "medium" : { # Default Medium Settings

        },

        "hard" : { # Default Hard Settings

        },

        "no_halls" : { # Extreme Test
            "room_ratio"        : 1,
        },

        "no_rooms" : { # Extreme Test
            "room_ratio"        : 0,
        },

        "one_layer" : { # Large, one-layer dungeon
            "w"                 : TERM_W,
            "h"                 : TERM_H*2,
            "d"                 : 1,
            "boss_layer_ratio"  : 0,
            "boss_layer_rooms"  : 1,
            "boss_room_delay"   : 40,
            "max_backtrack"     : None,
            "max_key_backtrack" : None,
            # TODO: Modify fails if not auto calculated
        },

        "convoluted" : { # High branching and more up-stairs
            "max_backtrack"     : None,
            "max_key_backtrack" : None,
            "up_stair_ratio"    : .1,
            "down_stair_ratio"  : .1,
            "locked_ratio"      : .35,
            "loop_ratio"        : .1, # ????
            "top_orderliness"   : 0,
        },

        "orderly" : { # High orderliness
            "bottom_orderliness": 1,
            "loop_ratio"        : 0,
        },

        "maze" : { # Mostly passages, more up-stairs
            "max_backtrack"     : None,
        },

        "one_path" : { # Direct route
            "down_stair_ratio"  : 0,
            "up_stair_ratio"    : 0,
            "max_backtrack"     : 0,
            "locked_ratio"      : 0,
            "loop_ratio"        : 0,
            # TODO: "consec_fails"      : 100,
        },

        "web" : { # All possible loops
            "loop_ratio"        : 1,
        },

        "sparse" : { # Low density, longer passages

        },

        "dense" : { # High density, shorter passages, smaller rooms

        },

        "random" : { # Randomize the parameters themselves
        
        },
    }



# Script behavior - instant dungeon on screen
if __name__ == "__main__":
    dungeon = Dungeon()
    print('\n', dungeon, sep='')