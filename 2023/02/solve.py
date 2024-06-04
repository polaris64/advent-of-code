from functools import reduce
import re


def read_input(fn):
    with open(fn, "rt") as fh:
        return [parse_line(l.strip()) for l in fh.readlines()]


def parse_line(line):
    match = re.search(r"^Game ([0-9]+): (.+)", line)
    if match is not None:
        groups = match.groups()
        cube_sets = [
            [(int(cs.split()[0]), cs.split()[1]) for cs in cs.split(", ")]
            for cs
            in [cs for cs in groups[1].split("; ")]
        ]
        return {
            "game_id": int(groups[0]),
            "cube_sets": cube_sets,
        }


def is_valid_game(game):
    for set in game["cube_sets"]:
        for x in set:
            if x[1] == "red" and x[0] > 12:
                return False
            if x[1] == "green" and x[0] > 13:
                return False
            if x[1] == "blue" and x[0] > 14:
                return False
    return True


def get_fewest_cubes(game):
    def _get_max(col):
        return max(
            [s for s in [[s for s in set if s[1] == col] for set in game["cube_sets"]] if len(s)], key=lambda x: x[0]
        )[0][0]

    return (
        _get_max("red"),
        _get_max("green"),
        _get_max("blue"),
    )


def solve_p1(inp):
    return sum([x["game_id"] for x in inp if is_valid_game(x)])


def solve_p2(inp):
    return sum([reduce(lambda a, b: a * b, get_fewest_cubes(game)) for game in inp])


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 2204
    assert solve_p2(inp) == 71036


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 8
    assert solve_p2(inp) == 2286


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
