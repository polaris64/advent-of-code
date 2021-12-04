import re

def get_input():
    with open("input.txt") as f:
        return [x.strip() for x in f.readlines() if len(x.strip()) > 0]

def get_example_input():
    return [
        "x=495, y=2..7",
        "y=7, x=495..501",
        "x=501, y=3..7",
        "x=498, y=2..4",
        "x=506, y=1..2",
        "x=498, y=10..13",
        "x=504, y=10..13",
        "y=13, x=498..504",
    ]

def parse_input(inp):
    def parse_item(item):
        val = None
        m = re.match(r"^[xy]=(\d+)$", item)
        if m is not None and len(m.groups()) == 1:
            val = int(m.groups()[0])
        else:
            m = re.match(r"^[xy]=(\d+)..(\d+)$", item)
            if m is not None and len(m.groups()) == 2:
                val = range(int(m.groups()[0]), int(m.groups()[1]) + 1)
        return (item[0], val)

    def parse_line(line):
        items = [x.strip() for x in line.split(",")]
        if len(items) != 2:
            return None
        return [parse_item(x) for x in items]

    items = [parse_line(x) for x in inp]
    return [{x[0]:x[1] for x in item} for item in items]

def build_grid(inp):
    # Get grid bounds
    min_x = min(inp, key=lambda x: x["x"] if type(x["x"]) is int else x["x"].start)["x"]
    min_x = (min_x if type(min_x) is int else min_x.start) - 1
    min_y = 0
    max_x = max(inp, key=lambda x: x["x"] if type(x["x"]) is int else x["x"].stop - 1)["x"]
    max_y = max(inp, key=lambda x: x["y"] if type(x["y"]) is int else x["y"].stop - 1)["y"]
    max_x = (max_x if type(max_x) is int else max_x.stop) + 1
    max_y = (max_y if type(max_y) is int else max_y.stop)

    grid = list()
    for y in range(min_y, max_y + 1):
        grid.append(list())
        for x in range(min_x, max_x + 1):
            span = None
            for item in inp:
                if (
                    (
                        type(item["x"]) is int and item["x"] == x or
                        type(item["x"]) is range and item["x"].count(x) > 0
                    ) and
                    (
                        type(item["y"]) is int and item["y"] == y or
                        type(item["y"]) is range and item["y"].count(y) > 0
                    )
                ):
                    span = item
                    break
            if span is None:
                grid[y].append(".")
            else:
                grid[y].append("#")

    return {
        "min":  (min_x, min_y),
        "max":  (max_x, max_y),
        "grid": grid,
        "water": None,
    }

def render_world(world):
    res = ""
    for y in range(0, world["max"][1] - world["min"][1] + 1):
        yw = y + world["min"][1]
        for x in range(0, world["max"][0] - world["min"][0] + 1):
            xw = x + world["min"][0]
            if xw == 500 and yw == 0:
                res += "+"
            elif world["water"] is not None and world["water"][0] == xw and world["water"][1] == yw:
                res += "~"
            else:
                res += world["grid"][y][x]
        res += "\n"
    return res

def step(world):
    # Add new water from spring
    if world["water"] is None:
        world["water"] = [500, world["min"][1] + 1]

    # Move existing water
    process_water(world)

def process_water(world):
    cont = True
    water = world["water"]
    while cont:
        cell_below = get_cell(world, water[0], water[1] + 1)
        if cell_below == "#":
            world["grid"][water[1] - world["min"][1]][water[0] - world["min"][0]] = "~"
            cont = False
        elif cell_below == "." or cell_below == "|":
            world["grid"][water[1] - world["min"][1]][water[0] - world["min"][0]] = "|"
            water[1] += 1
        elif cell_below == "~":
            settle(water, world)
            cell_below = get_cell(world, water[0], water[1] + 1)
            if cell_below == "#" or cell_below == "~":
                world["grid"][water[1] - world["min"][1]][water[0] - world["min"][0]] = "~"
                cont = False
        elif water[1] > world["max"][1]:
            cont = False
        else:
            cont = False
    world["water"] = None

def get_cell(world, x, y):
    xw = x - world["min"][0]
    yw = y - world["min"][1]
    if x < world["min"][0] or x > world["max"][0]:
        return "."
    elif y < world["min"][1] or y > world["max"][1]:
        return "."
    elif world["water"][0] == xw and world["water"][1] == yw:
        return "~"
    else:
        return world["grid"][yw][xw]

def settle(water, world):
    search = True
    start = (water[0], water[1] + 1)
    off = 1
    while search:
        cell = get_cell(world, start[0] - off, start[1])
        if cell == "#":
            search = False
        elif cell == "." or cell == "|":
            water[0] = start[0] - off
            water[1] = start[1]
            return
        off += 1
    off = 1
    search = True
    while search:
        cell = get_cell(world, start[0] + off, start[1])
        if cell == "#":
            search = False
        elif cell == "." or cell == "|":
            water[0] = start[0] + off
            water[1] = start[1]
            return
        off += 1


def run(inp):
    world = build_grid(inp)
    for i in range(20):
        step(world)
        print(render_world(world))
        #print(world["water"])

def run_examples():
    return run(parse_input(get_example_input()))

def run_full():
    return run(parse_input(get_input()))

if __name__ == "__main__":
    run_examples()
    #run_full()
