def step(state):
    any_attacked = False
    any_moved    = False

    grid_w = len(state["map"][0])

    # Process each unit in turn
    for u in state["units"]:

        # If unit has already been killed, skip
        if u["hp"] < 0:
            continue

        # Identify alive targets for this unit
        targets = [x for x in state["units"] if x["hp"] > 0 and x["t"] == ("E" if u["t"] == "G" else "G")]

        # If there are no targets the game has ended
        if len(targets) == 0:
            cleanup_state(state)
            return False

        # Identify all open cells in range of each target
        in_range = list()
        for target in targets:
            in_range.append({"u": target, "in_range": get_in_range(state, target)})

        # Get list of targets in range (for attack). Select first target
        # (hp and reading order) and attack if possible.
        targets_in_range = [t["u"] for t in in_range if any([cell for cell in t["in_range"] if cell[0] == u["x"] and cell[1] == u["y"]])]
        target_in_range = None if len(targets_in_range) == 0 else min(targets_in_range, key=lambda x: (x["hp"], (x["y"] * grid_w) + x["x"]))

        # If no targets are in range then move, then check again
        if target_in_range is None:

            # Get distances from unit position to all cells
            distances = get_cell_distances(state, u["x"], u["y"])

            # Find cell in range of any target that is in the distances
            # list and has the lowest distance

            # Get list of all targets together with their nearest in-range
            # cell
            target_nearest = [
                {
                    # Target unit
                    "u": tar["u"],

                    # Unit's closest in-range cell
                    "nearest": min(
                        [
                            (c, distances.get(c, (99999, None))[0])
                            for c in tar["in_range"]
                        ],

                        # Select (distance, reading order) for min()
                        key=lambda x: (x[1], (x[0][1] * grid_w) + x[0][0])
                    ),
                }
                for tar in in_range
            ]

            # Get target with nearest in-range cell
            nearest = min(
                target_nearest,

                # Select (distance, reading order) for min()
                key=lambda x: (x["nearest"][1], (x["nearest"][0][1] * grid_w) + x["nearest"][0][0]))

            # Move to nearest if possible
            if nearest is not None:
                target = (nearest["nearest"][0][0], nearest["nearest"][0][1])

                # Get path to target
                path = list()
                current = target
                while True:
                    c = distances.get(current)
                    if c is None or c[1] is None or c[1] == (u["x"], u["y"]):
                        break
                    current = c[1]

                if abs(u["x"] - current[0]) <= 1 and abs(u["y"] - current[1]) <= 1:
                    #print("MOVE", u, current)
                    u["x"] = current[0]
                    u["y"] = current[1]
                    any_moved = True

        # Get targets in range again after move
        targets_in_range = [t["u"] for t in in_range if any([cell for cell in t["in_range"] if cell[0] == u["x"] and cell[1] == u["y"]])]
        target_in_range = None if len(targets_in_range) == 0 else min(targets_in_range, key=lambda x: (x["hp"], (x["y"] * grid_w) + x["x"]))

        if target_in_range is not None:
            #print("ATTK", u, target_in_range)
            target_in_range["hp"] -= u["ap"]
            any_attacked = True

    cleanup_state(state)
    return any_attacked or any_moved

def cleanup_state(state):
    # Remove all dead units
    state["units"] = [x for x in state["units"] if x["hp"] > 0]

    sort_units(state)

def sort_units(state):
    # Sort units into reading order
    grid_w = len(state["map"][0])
    state["units"].sort(key=lambda x: (x["y"] * grid_w) + x["x"])

def cell_unit(state, x, y):
    for unit in state["units"]:
        if unit["hp"] > 0 and unit["x"] == x and unit["y"] == y:
            return unit
    return None

def get_dist(x1, y1, x2, y2):
    return abs(x2 - x1) + abs(y2 - y1)

def get_adjacent_cell(state, x, y, dirx, diry):
    grid_w = len(state["map"][0])
    if x + dirx < 0:
        return "#"
    elif x + dirx > grid_w - 1:
        return "#"
    elif y + diry < 0:
        return "#"
    elif y + diry > grid_w - 1:
        return "#"
    unit = cell_unit(state, x + dirx, y + diry)
    if unit is not None:
        return unit["t"]
    return state["map"][y + diry][x + dirx]

def get_in_range(state, target):
    tx = target["x"]
    ty = target["y"]
    cells = list()
    if get_adjacent_cell(state, tx, ty, 0, -1) != "#":
        cells.append((tx, ty - 1))
    if get_adjacent_cell(state, tx, ty, -1, 0) != "#":
        cells.append((tx - 1, ty))
    if get_adjacent_cell(state, tx, ty, 1, 0) != "#":
        cells.append((tx + 1, ty))
    if get_adjacent_cell(state, tx, ty, 0, 1) != "#":
        cells.append((tx, ty + 1))
    return cells

def get_cell_distances(state, sx, sy):
    """Gets the distance to all reachable cells from a source cell"""

    grid_w  = len(state["map"][0])
    current = (sx, sy)

    # Visited list
    visited = dict()

    # Create set of unvisited nodes
    unvisited = dict()
    for (y, row) in enumerate(state["map"]):
        for (x, col) in enumerate(row):
            if col == "." and cell_unit(state, x, y) is None:
                unvisited[(x, y)] = (99999, None)
    unvisited[current] = (0, None)

    while True:
        # Get neighbouring unvisited cells
        neighbours = [
            x for x in [
                (current[0] + 0, current[1] + 1),
                (current[0] + 1, current[1] + 0),
                (current[0] - 1, current[1] + 0),
                (current[0] + 0, current[1] - 1),
            ]
            if x in unvisited
        ]

        # Update distances in unvisited list for all neighbours
        for n in neighbours:
            dist = unvisited[current][0] + 1
            coord = (n[0], n[1])
            if coord in unvisited:
                if (
                    unvisited[coord][0] > dist or
                    (
                        unvisited[coord][0] == dist and (
                            ((current[1] * grid_w) + current[0]) <
                            ((unvisited[coord][1][1] * grid_w) + unvisited[coord][1][0])
                        )
                    )
                ):
                    unvisited[coord] = (dist, current)

        # Add current cell to visited and remove from unvisited
        visited[current] = unvisited[current]
        del unvisited[current]

        if len(unvisited.values()) == 0 or all([x[1] is None for x in unvisited.values()]):
            break

        # Set new current node: unvisited node with smallest distance and
        # lowest reading order
        current = min(unvisited.items(), key=lambda x: (x[1][0], (x[0][1] * grid_w) + x[0][0]))[0]

    return visited
