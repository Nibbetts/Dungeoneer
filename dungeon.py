import numpy as np
from random import randint, choice, random, uniform, sample, choices
from random import seed as seedrandom
from time import time
# NOTE: randint is inclusive on both ends



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
LOOP       = PART2

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
- start as a spelunker, with a cavein? Flashlight is directional, lantern fills room or hall
"""

class Room:
    def __init__(self, pid, x, y, z, w, h, door_in=None, doors_out=[], boss=False):
        self.pid = pid
        self.x = x
        self.y = y
        self.z = z
        self.w = w
        self.h = h
        self.door_in = door_in # Coordinate tuple; may be of a stair, not a door
        self.doors_out = doors_out # List of coordinate tuples; Should include stairs, not just doors
        self.area = (w-2) * (h-2) # Internal area
        self.perimeter_area = 2*w + 2*h - 4
        self.boss = boss
        self.room = True # Whether or not I am a room

    @staticmethod
    def place(dungeon, pid, x, y, z, w=None, h=None, stair=False, boss=False):
        # w and h can be used to limit the size currently, but not increase it.
        # TODO: make use of limiting w and h to line up rooms neatly to adjacent ones?

        MAX = dungeon.BOSS_MAX_ROOM if boss else dungeon.MAX_ROOM
        MIN = dungeon.BOSS_MIN_ROOM if boss else dungeon.MIN_ROOM if dungeon.MIN_ROOM else 5

        # Compute available space
        result = dungeon.check_place(x, y, z, w if w else MAX, h if h else MAX)
        if result is None: return None
        L, R, U, D = result
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
        room = Room(pid, corner_x, corner_y, z, w, h, door_in=(x, y), boss=boss)
        dungeon.write(room)
        return room


class Hallway:
    def __init__(self, pid, x, y, z, orientation, length, breadth, door_in=None, doors_out=[]):
        self.pid = pid
        self.x = x
        self.y = y
        self.z = z
        self.w = length if orientation == LR else breadth
        self.h = length if orientation == UD else breadth
        self.orientation = orientation
        self.length = length
        self.door_in = door_in # Coordinate tuple; may be of a stair, not a door
        self.doors_out = doors_out # List of coordinate tuples; Should include stairs, not just doors
        self.area = length-2 # Internal area
        self.perimeter_area = 2*length + 2*(breadth) - 4
        self.boss = False
        self.room = False # Whether or not I am a room

    @staticmethod
    def place(dungeon, pid, x, y, z, orientation=None, length=None, stair=False):
        # Compute available space
        orientation = orientation if orientation is not None else choice([LR, UD])
        w, h = (dungeon.MAX_HALL, 3) if orientation == LR else (3, dungeon.MAX_HALL) if orientation == UD else (3, 3)
        result = dungeon.check_place(x, y, z, w, h)
        if result is None: return None
        L, R, U, D = result
        w = L + R + 1
        h = U + D + 1
        length, width = (w, h) if orientation == LR else (h, w)

        # Make sure it works here, and catch literal edge cases
        if (length < dungeon.MIN_HALL) or (width < dungeon.HALL_BREADTH): return None
        if L == 0 or R == 0:
            if U < 1 or D < 1 or stair: return None # Stairs disallowed on borders
        if U == 0 or D == 0:
            if L < 1 or R < 1 or stair: return None

        # Randomly decrease the size
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

        # Make the Room
        corner_x = x - L
        corner_y = y - U
        hall = Hallway(pid, corner_x, corner_y, z, orientation, length, dungeon.HALL_BREADTH, door_in=(x, y))
        dungeon.write(hall)
        return hall


class Dungeon:
    # TODO: Move Documentation here

    def __init__(self,
            w=TERM_W//2,  h=TERM_H,  d=10, # W/2 since using 2 chars per square
            min_room=None,       max_room=8,
            min_hall=3,          max_hall=18,        hall_breadth=1,
            boss_min_room=8,     boss_max_room=20,   boss_min_area=170,
            boss_bottom=True,    boss_after_layer=3, boss_layer_rooms=None,
            boss_layer_ratio=.2, boss_room_delay=0,  boss_layer_count=None,
            boss_allow_stair=False,
            max_backtrack=20,  max_key_backtrack=10,
            max_density=1,     min_density=0,        # Non-restrictive defaults
            locked_ratio=.25,  down_stair_ratio=.08, up_stair_ratio=.02,
            room_ratio=.5,     loop_ratio=.02,
            vary_room_ratio=True,
            top_orderliness=1, bottom_orderliness=0, fade_orderliness=True,
            consec_fails=20,   max_consec_fails=100, max_fails=10000,
            highlight_borders=False, report=True, seed=None ): # Same seed will only rebuild the same dungeon if given the same parameters!

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
        self.BOSS_ALLOW_STAIR   = boss_allow_stair   # Allow stairs to be the first entrance to a boss room (Loops of course will mean you can enter another way).

        # Complexity and other Global Characteristics
        self.MAX_BACKTRACK      = max_backtrack      # How many rooms/halls back we might branch anew. This is not a direct control, but largely influences branching and the likelihood of long independent branches.
        self.MAX_KEY_BACKTRACK  = max_key_backtrack  # How many rooms (only) back we might place a key from it's door
        self.MAX_DENSITY        = max_density        # May not be met, but stops after this density. # TODO: use this and min as well!
        self.MIN_DENSITY        = min_density        # May not be met, especially with low backtracking.
        self.VARY_ROOM_RATIO    = vary_room_ratio    # Use a varied trimodal distribution for room/hall ratios (Turn off to get all layers to match ratios)

        # Global Characteristics that may be locally modified
        self.LOCKED_RATIO       = locked_ratio       # Likelihood to attempt to lock a door
        self.DOWN_STAIR_RATIO   = down_stair_ratio   # Likelihood to go down when we don't have to, or of having more down stair branching
        self.UP_STAIR_RATIO     = up_stair_ratio     # Likelihood of going up (Adds complexity, more maze-like)
        self.ROOM_RATIO         = room_ratio         # Probability ratio of rooms to halls (not including boss rooms)
        self.LOOP_RATIO         = loop_ratio         # Likelihood of creating a loop when the option is found

        # Orderliness
        self.TOP_ORDERLINESS    = top_orderliness    # Generally how organized rooms and hallways are at most, or at the top of the dungeon if FADE_ORDERLINESS.
        self.BOTTOM_ORDERLINESS = bottom_orderliness # Minimum orderliness, or orderliness at the bottom of the dungeon if FADE_ORDERLINESS.
        self.FADE_ORDERLINESS   = fade_orderliness   # If true, order will change in a gradient from top to bottom of dungeon. If false, will pick a random value between top and bottom for each layer.

        # Procedural Generation Boundaries          # NOTE: fail count limits tend to affect how filled in a dungeon is when generation finishes.
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
        # self.halls             # List of Hallways,             in creation order
        # self.rooms             # List of Rooms,                in creation order
        # self.places            # Rooms and Halls together,     in creation order
        # self.branching_weights # Branching Weights of places,  in creation order
        # self.layer_places      # Rooms + Halls in each layer,  in creation order
        # self.layer_areas       # Approximate used 2D area of each layer
        # self.doors =           # List of Doors,                in creation order
        # self.stairs_up         # List of upward stairways,     in creation order
        # self.stairs_down       # List of downward stairways,   in creation order
        # self.locks             # Dict mapping locked door coords to key ID's
        # self.keys              # Dict mapping key coords to key ID's
        # self.layer_params      # List of layer parameter lists, in depth order
        # self.layer_bosses      # List of lists of boss rooms in each layer
        # self.layer_fails       # Array of per-layer fail counts
        # self.seed              # The seed used to generate this dungeon given these parameters

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
                                break
                    if tryD:
                        h += 1
                        D += 1
                        if h == maxh:
                            tryU, tryD = False, False
            
            else:
                break

        return L, R, U, D

    def write(self, place):
        # Write spaces inside border
        for x in range(place.x + 1, place.x + place.w - 1):
            for y in range(place.y + 1, place.y + place.h - 1):
                self.map[place.z, y, x] = SPACE

        # Write doors (non-specific, here)
        self.map[place.z, place.door_in[1], place.door_in[0]] = DOOR
        for door in place.doors_out:
            self.map[place.z, door[1], door[0]] = DOOR

    def borders(self, place, borders=None):
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
        for d in place.doors_out + [place.door_in]:
            L, R, U, D = (d[0]-1, d[1]), (d[0]+1, d[1]), (d[0], d[1]-1), (d[0], d[1]+1)
            for loc in [d, L, R, U, D]:
                if loc in borders: borders.remove(loc)

        return borders

    def report(self): # TODO: add/change stuff
        return  "---------D U N G E O N---------\n" \
            +  f"      Size: {self.w} x {self.h} x {self.d}\n" \
            +  f"     Rooms: {len(self.rooms)}\n" \
            +  f"     Halls: {len(self.halls)}\n" \
            +  f"     Doors: {len(self.doors)}\n" \
            +  f"    Stairs: {len(self.stairs_up)}\n" \
            +  f"     Fails: {self.fails}\n" \
            +  f" MaxConsec: {self.max_consec}\n" \
            +  f"      Seed: {self.seed}\n" \
            +   "-------------------------------"

    @staticmethod
    def area_of(place):
        """ Helper function to calculate the approximate unique area of a Room or Hall."""
        return place.area + place.perimeter_area/2-1

    def generate_map(self, report=True, seed=None):

        # RESET VARIABLES
        self.seed               = int(round(time() * 1e7)) if seed is None else seed
        seedrandom(self.seed)   # Seed the random number generator
        self.map                = (np.ones((self.d, self.h, self.w), dtype='<u2') * SOLID)
        self.halls              = [] # All Hallways, in creation order
        self.rooms              = [] # All Rooms, in creation order
        self.places             = [] # Rooms and Halls together, in creation order
        self.branching_weights  = [] # Branching weight for each place in places
        self.layer_places       = [[] for _ in range(self.d)]
        self.layer_areas        = np.zeros(self.d)
        self.doors              = []
        self.stairs_up          = []
        self.stairs_down        = []
        self.locks              = {}
        self.keys               = {}
        self.layer_params       = [[0, 0, 0, 0, 0, False, 0] for _ in range(self.d)]
        self.layer_bosses       = [[] for _ in range(self.d)]

        orderly_hall_start      = round(2 * self.w * self.h / self.MAX_HALL**2)

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

        # INDIVIDUAL LAYER PARAMETER CALCULATIONS
        # Do some precalculations
        locked_radius = min(self.LOCKED_RATIO, 1-self.LOCKED_RATIO) if self.d > 1 else 0
        up_radius     = min(self.UP_STAIR_RATIO, 1-self.UP_STAIR_RATIO) if self.d > 1 else 0
        down_radius   = min(self.DOWN_STAIR_RATIO, 1-self.DOWN_STAIR_RATIO) if self.d > 1 else 0
        loop_radius   = min(self.LOOP_RATIO, 1-self.LOOP_RATIO) if self.d > 1 else 0
        orderliness   = np.linspace(self.TOP_ORDERLINESS, self.BOTTOM_ORDERLINESS, self.d) if self.FADE_ORDERLINESS else None
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
            self.layer_params[l][PARAM_LOOP_RATIO       ] = uniform(self.LOOP_RATIO-loop_radius, self.LOOP_RATIO+loop_radius) # TODO: use this!
            self.layer_params[l][PARAM_ORDERLINESS      ] = uniform(self.TOP_ORDERLINESS, self.BOTTOM_ORDERLINESS) if orderliness is None else orderliness[l] 


        # DUNGEON GENERATION LOOP
        # Iterate down the dungeon
        # TODO: fix end conditions to work with density instead, keep track of local density, and local fails, and layers/places that haven't failed out?
        # Stop only once the bottom layers fill up enough
        # while len(self.layer_places[self.d-1]) < 5 and self.consec_fails < 15:
        while ((not self.places or self.places[-1].z != self.d-1) and self.consec_fails < self.CONSEC_FAILS) \
                or self.consec_fails < self.CONSEC_FAILS or len(self.layer_places[self.d-1]) < 5 \
                and self.consec_fails < self.MAX_CONSEC_FAILS and self.fails < self.MAX_FAILS:
            # Find a Room or Hall to focus on
            # place = self.places[-randint(1, min(len(self.places), self.MAX_BACKTRACK+1))] if self.places else None
            backtrack = min(len(self.places), self.MAX_BACKTRACK+1)
            place = choices(self.places[-backtrack:], weights=self.branching_weights[-backtrack:])[0] if self.places else None

            # Decide whether to build down or adjacent.
            if not place:
                # Make the first room or hall of the dungeon - it always begins with a stair
                x, y, z = randint(1, self.w-2), randint(1, self.h-2), 0
                stair = 1
            elif (random() < self.layer_params[place.z][PARAM_UP_STAIR_RATIO]) and place.z != 0:
                # Up
                z = place.z - 1
                x, y = randint(place.x+1, place.x+place.w-2), randint(place.y+1, place.y+place.h-2)
                stair = -1
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
                if not borders: # Empty borders list should be rare, if even possible
                    increment_fails(z)
                    continue
                x, y = choice(borders)
                stair = 0

            # Boss Room stuff
            if self.layer_params[z][PARAM_BOSS_ROOM] and len(self.layer_places[z]) >= self.BOSS_ROOM_DELAY and (not stair or self.BOSS_ALLOW_STAIR):
                boss = True if self.BOSS_LAYER_ROOMS is None else (len(self.layer_bosses[z]) < self.BOSS_LAYER_ROOMS)
                # Make a boss room more likely on a boss layer - otherwise it may not generate soon enough to fit
                need = boss and (len(self.layer_places[z]) >= min(4, self.BOSS_ROOM_DELAY)) and (not len(self.layer_bosses[z]))
                boss = boss and (choice([True, False]) or need) # Otherwise let others generate first sometimes, sort of like an antechamber
            else: boss = False

            # Attempt to build in the place found and decide on Room or Hallway
            pid = len(self.places)
            if boss or (not place and bool(self.ROOM_RATIO)) or ((random()**((not stair and layer_fails_half) + 1) < self.layer_params[z][PARAM_ROOM_RATIO]) and (
                    len(self.layer_places[z]) > orderly_hall_start or random() >= self.layer_params[z][PARAM_ORDERLINESS])): # Orderliness Modifier
                # Room generation
                new_place = Room.place(self, pid, x, y, z, stair=stair, boss=boss)
                room = True
            else:
                # Hallway generation
                if place and not place.room and not stair and place.orientation != PIT and \
                        random() < self.layer_params[z][PARAM_ORDERLINESS] and not layer_fails_half: # Orderliness Modifier
                    new_place = Hallway.place(self, pid, x, y, z, orientation=not place.orientation, stair=stair)
                else:
                    new_place = Hallway.place(self, pid, x, y, z, stair=stair)
                room = False

            # Make the necessary additions to the map and to our lists
            if new_place:
                # We succeeded in building the room
                reset_consec_fails(z)
                self.places.append(new_place)
                if room: # TODO: make these numbers into parameters
                    self.rooms.append(new_place)
                    if boss: self.branching_weights.append(10)#15 if random() < .5 else 1)
                    else:    self.branching_weights.append(1)
                else:
                    self.halls.append(new_place)
                    self.branching_weights.append(1.5) # TODO: and here!
                self.layer_places[z].append(new_place)
                self.layer_areas[z] += Dungeon.area_of(new_place)
                if new_place.boss: self.layer_bosses[z].append(new_place)
                # place.doors_out.append((x,y)) # TODO: fix this bug, need this line. Also fix for when place is None
                if stair == 1:
                    # Stair down
                    self.stairs_down.append((x, y, z-1))
                    self.stairs_up.append((x, y, z))
                    # Places don't draw stairs by themselves
                    if place: self.map[z-1, y, x] = STAIR_DOWN if self.map[z-1, y, x] != STAIR_UP else STAIR_BOTH
                    self.map[z, y, x] = STAIR_UP if self.map[z, y, x] != STAIR_DOWN else STAIR_BOTH
                elif stair == -1:
                    # Stair up
                    self.stairs_up.append((x, y, z+1))
                    self.stairs_down.append((x, y, z))
                    # Places don't draw stairs by themselves
                    self.map[z, y, x] = STAIR_DOWN if self.map[z, y, x] != STAIR_UP else STAIR_BOTH
                    self.map[z+1, y, x] = STAIR_UP if self.map[z+1, y, x] != STAIR_DOWN else STAIR_BOTH
                else:
                    # Adjacent
                    self.doors.append((x, y, z))
                    # Decide whether to lock this one
                    if random() < self.layer_params[z][PARAM_LOCKED_RATIO] and (len(self.rooms) > room):
                        key_room = choice(self.rooms[-self.MAX_KEY_BACKTRACK-2:-1] if room else self.rooms[-self.MAX_KEY_BACKTRACK-1:])
                        key_coords = (key_room.z, randint(key_room.y+1, key_room.y+key_room.h-2), randint(key_room.x+1, key_room.x+key_room.w-2))
                        if self.map[key_coords] == SPACE:
                            # If many very small rooms and/or many stairs, ratio of locked doors will go down.
                            key_id = len(self.keys)
                            self.locks[(z,y,x)] = key_id
                            self.keys[key_coords] = key_id
                            # Places draw doors when placed, but not locked doors or keys
                            self.map[z, y, x] = LOCK
                            self.map[key_coords] = KEY
            
            else:
                # We failed to build the room
                increment_fails(z)

        # CLEAN UP
        # Clean up boss layer misses
        for l in range(self.d):
            self.layer_params[l][PARAM_BOSS_ROOM] = bool(self.layer_bosses[l])

        # Move backward through the halls to clean up loose ends
        for h in self.halls[::-1]:
            pass # TODO:
            # TODO: add param to allow doors in halls, then clean up depending on orderliness?

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

        # Print some stats of the resulting dungeon
        if report: print(self.report() + "   ", end="\r")

    @staticmethod
    def new(recipe="default", report=True):
        return Dungeon(**Dungeon.RECIPES[recipe], report=report)

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
            # TODO: Turn off orderly?? Or would it work here?
        },

        "no_rooms" : { # Extreme Test
            "room_ratio"        : 0,
            "locked_ratio"      : 0,
        },

        "one_layer" : { # Large, one-layer dungeon
            "w"                 : 80,
            "h"                 : 48,
            "d"                 : 1,
            "boss_layer_ratio"  : 0,
            "boss_layer_rooms"  : 1,
            "boss_room_delay"   : 40,
            "max_backtrack"     : None, # TODO: Modify these to allow None for infinite backtrack
            "max_key_backtrack" : None, # ''
            # TODO: Modify fails if not auto calculated
        },

        "convoluted" : { # High branching and more up-stairs
            "max_backtrack"     : None,
            "max_key_backtrack" : None,
            "up_stair_ratio"    : .1,
            "down_stair_ratio"  : .1,
            "locked_ratio"      : .35,
            "loop_ratio"        : .1,
            # TODO: Also turn off orderly or mix it in
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