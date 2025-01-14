from itertools import chain


def read_input(filename):
    with open(filename, "r") as fh:
        return [parse_line(line.strip()) for line in fh.readlines()]


def parse_line(line):
    parts = line.split(" -> ")
    return [tuple([int(v) for v in p.split(",")]) for p in parts]


def build_paths(inp):
    def _build_path(path):
        prev = path[0]
        path_set = set([prev])
        for segment in path[1:]:
            if prev[0] == segment[0]:
                v1 = prev[1]
                v2 = segment[1]
                for v in range(min(v1, v2), max(v1, v2) + 1):
                    path_set.add((prev[0], v))
            else:
                v1 = prev[0]
                v2 = segment[0]
                for v in range(min(v1, v2), max(v1, v2) + 1):
                    path_set.add((v, prev[1]))
            prev = segment
        return path_set

    return [_build_path(v) for v in inp]


def get_context(inp):
    ctx = {
        "walls": set(chain(*build_paths(inp))),
        "sand": set(),
    }
    return ctx


def simulate_sand_p1(ctx, max_y):
    sand = (500, 0)
    while True:
        np = (sand[0], sand[1] + 1)
        if np in ctx["walls"] or np in ctx["sand"]:
            np = (sand[0] - 1, sand[1] + 1)
            if np in ctx["walls"] or np in ctx["sand"]:
                np = (sand[0] + 1, sand[1] + 1)
                if np in ctx["walls"] or np in ctx["sand"]:
                    ctx["sand"].add(sand)
                    return True
                else:
                    sand = np
            else:
                sand = np
        else:
            sand = np
        if sand[1] >= max_y:
            break

    return False


def simulate_sand_p2(ctx, floor_y):
    sand = (500, 0)
    while True:
        np = (sand[0], sand[1] + 1)
        if np in ctx["walls"] or np in ctx["sand"] or np[1] == floor_y:
            np = (sand[0] - 1, sand[1] + 1)
            if np in ctx["walls"] or np in ctx["sand"] or np[1] == floor_y:
                np = (sand[0] + 1, sand[1] + 1)
                if np in ctx["walls"] or np in ctx["sand"] or np[1] == floor_y:
                    ctx["sand"].add(sand)
                    return not ((500, 0) in ctx["sand"])
                else:
                    sand = np
            else:
                sand = np
        else:
            sand = np

    return False


def simulate(inp, part=1):
    ctx = get_context(inp)
    cont = True
    max_y = max([y for _, y in ctx["walls"]])
    while cont:
        if part == 1:
            cont = simulate_sand_p1(ctx, max_y)
        else:
            cont = simulate_sand_p2(ctx, max_y + 2)
    return ctx


def render_grid(ctx, floor=False):
    res = ""
    max_y = max([y for _, y in ctx["walls"]])
    if floor:
        max_y += 2
    for y in range(
            min([y for _, y in ctx["walls"]]) - 1,
            max_y + 2
    ):
        for x in range(
                min([x for x, _ in ctx["walls"]]) - 1,
                max([x for x, _ in ctx["walls"]]) + 2
        ):
            if (x, y) in ctx["walls"] or (floor and y == max_y):
                res += "#"
            elif (x, y) in ctx["sand"]:
                res += "o"
            else:
                res += "."
        res += "\n"
    return res


def solve_p1(inp):
    return len(simulate(inp)["sand"])


def solve_p2(inp):
    return len(simulate(inp, 2)["sand"])


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 24
    assert solve_p2(inp) == 93


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 888
    assert solve_p2(inp) == 26461


if __name__ == '__main__':
    main()
