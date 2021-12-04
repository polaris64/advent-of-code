import game

def get_input():
    with open("input.txt") as f:
        return [list(x.strip()) for x in f.read().split("\n") if len(x.strip()) > 0]

def parse_map(inp):
    # Extract Elves and Goblins
    units = []
    for (y, row) in enumerate(inp):
        for (x, ch) in enumerate(row):
            if ch == "E":
                units.append({"t": "E", "x": x, "y": y, "ap": 3, "hp": 200})
                inp[y][x] = "."
            elif ch == "G":
                units.append({"t": "G", "x": x, "y": y, "ap": 3, "hp": 200})
                inp[y][x] = "."

    return {
        "map":   inp,
        "units": units,
    }

def render_map(state):
    output = ""
    for (y, row) in enumerate(state["map"]):

        # Render grid cells
        for (x, cell) in enumerate(row):
            units = [u for u in state["units"] if u["x"] == x and u["y"] == y]
            ch = cell
            if len(units) > 0:
                ch = units[0]["t"]
            output += ch

        # Render unit statuses for this row
        units = [u for u in state["units"] if u["y"] == y]
        output += " " + ", ".join(["{}({})".format(x["t"], x["hp"]) for x in units])

        output += "\n"
    return output


def run(state):
    run = True
    ticks = 0

    print("Initial state")
    print(render_map(state))

    while run:
        run = game.step(state)
        if run:
            ticks += 1
            print("After round {}".format(ticks))
            print(render_map(state))

    print("Final state")
    print(render_map(state))

    hitpoint_sum = sum([x["hp"] for x in state["units"]])
    print("Sum of unit hitpoints after {} turns: {}".format(ticks, hitpoint_sum))
    print("Total value: {}".format(ticks * hitpoint_sum))

    return (ticks, state, hitpoint_sum)

def run_examples():
    inputs = [

        # Site examples (known outcomes)
        [
            "#######",
            "#.G...#",
            "#...EG#",
            "#.#.#G#",
            "#..G#E#",
            "#.....#",
            "#######",
        ],
        [
            "#######",
            "#G..#E#",
            "#E#E.E#",
            "#G.##.#",
            "#...#E#",
            "#...E.#",
            "#######",
        ],
        [
            "#######",
            "#E..EG#",
            "#.#G.E#",
            "#E.##E#",
            "#G..#.#",
            "#..E#.#",
            "#######",
        ],
        [
            "#######",
            "#E.G#.#",
            "#.#G..#",
            "#G.#.G#",
            "#G..#.#",
            "#...E.#",
            "#######",
        ],
        [
            "#######",
            "#.E...#",
            "#.#..G#",
            "#.###.#",
            "#E#G#G#",
            "#...#G#",
            "#######",
        ],
        [
            "#########",
            "#G......#",
            "#.E.#...#",
            "#..##..G#",
            "#...##..#",
            "#...#...#",
            "#.G...G.#",
            "#.....G.#",
            "#########",
        ],

        # Movement example
        #[
        #    "#########",
        #    "#G..G..G#",
        #    "#.......#",
        #    "#.......#",
        #    "#G..E..G#",
        #    "#.......#",
        #    "#.......#",
        #    "#G..G..G#",
        #    "#########",
        #],

        # Unreachable target example
        #[
        #    "#####",
        #    "#G#E#",
        #    "#.#.#",
        #    "#.#.#",
        #    "#####",
        #],
    ]
    return [run(parse_map([list(x) for x in inp])) for inp in inputs]

def run_full():
    return run(parse_map(get_input()))

def test_examples():
    res = run_examples()
    assert res[0][0] * res[0][2] == 27730
    assert res[1][0] * res[1][2] == 36334
    assert res[2][0] * res[2][2] == 39514
    assert res[3][0] * res[3][2] == 27755
    assert res[4][0] * res[4][2] == 28944
    assert res[5][0] * res[5][2] == 18740


if __name__ == "__main__":
    run_examples()
    run_full()
