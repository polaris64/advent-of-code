from collections import Counter
import re


def read_input(filename):
    with open(filename, "rt") as fhandle:
        return [parse_instr(x.strip()) for x in fhandle.readlines()]


def parse_instr(line):
    parts = re.search(r"^([a-z ]+)([0-9]+,[0-9]+) through ([0-9]+,[0-9]+)", line).groups()
    coords = [
        tuple([int(x) for x in parts[1].split(",")]),
        tuple([int(x) for x in parts[2].split(",")]),
    ]
    return (parts[0].strip(), coords)


# Set version (unused)
# def follow_instrs(instrs):
#     grid = set()
#     for instr in instrs:
#         minx, miny = instr[1][0]
#         maxx, maxy = instr[1][1]

#         new_set = set((x, y) for x in range(minx, maxx + 1) for y in range(miny, maxy + 1))

#         # Union new_set and grid
#         if instr[0] == "turn on":
#             grid = grid | new_set

#         # Difference new_set and grid
#         elif instr[0] == "turn off":
#             grid = grid.difference(new_set)

#         # Intersect new_set and grid
#         elif instr[0] == "toggle":
#             grid = grid ^ new_set

#     return grid


def follow_instrs_v1(instrs):
    grid = dict()
    for instr in instrs:
        minx, miny = instr[1][0]
        maxx, maxy = instr[1][1]

        new_cells = [(x, y) for x in range(minx, maxx + 1) for y in range(miny, maxy + 1)]

        # Union new_set and grid
        if instr[0] == "turn on":
            for cell in new_cells:
                grid[cell] = 1

        # Difference new_set and grid
        elif instr[0] == "turn off":
            for cell in new_cells:
                grid[cell] = 0

        # Intersect new_set and grid
        elif instr[0] == "toggle":
            for cell in new_cells:
                grid[cell] = 0 if cell in grid and grid[cell] == 1 else 1

    return grid


def follow_instrs_v2(instrs):
    grid = Counter()
    for instr in instrs:
        minx, miny = instr[1][0]
        maxx, maxy = instr[1][1]

        new_cells = [(x, y) for x in range(minx, maxx + 1) for y in range(miny, maxy + 1)]

        # Increase brightness by 1
        if instr[0] == "turn on":
            for cell in new_cells:
                grid[cell] += 1

        # Decrease brightness by 1 (minimum is 0)
        elif instr[0] == "turn off":
            for cell in new_cells:
                grid[cell] = max(0, grid[cell] - 1)

        # Increase brightness by 2
        elif instr[0] == "toggle":
            for cell in new_cells:
                grid[cell] += 2

    return grid


def render_grid(grid):
    output = ""
    minx = min(grid, key=lambda x: x[0])[0]
    maxx = max(grid, key=lambda x: x[0])[0]
    miny = min(grid, key=lambda x: x[1])[1]
    maxy = max(grid, key=lambda x: x[1])[1]
    for y in range(miny, maxy + 1):
        for x in range(minx, maxx + 1):
            output += str(grid[(x, y)])
        output += "\n"
    return output


def solve_p1(inp):
    return sum(follow_instrs_v1(inp).values())


def solve_p2(inp):
    return sum(follow_instrs_v2(inp).values())


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 543903
    assert solve_p2(inp) == 14687245


def test_ex():
    pass


if __name__ == "__main__":
    main()
