import math
import re


def read_input(fname):
    with open(fname, "r") as handle:
        return [parse_rule(x.strip()) for x in handle.readlines()]


def parse_rule(line):
    res = re.match(r"^(N|S|E|W|F|B|L|R)([0-9]+)", line).groups()
    return (res[0], int(res[1]))


def rotate(xpos, ypos, ang, direction):
    if direction == "L":
        ang = -ang
    ang = math.radians(ang)
    return [
        int(round(xpos * math.cos(ang) - ypos * math.sin(ang))),
        int(round(xpos * math.sin(ang) + ypos * math.cos(ang)))
    ]


def run_commands(inp, start):
    state = {
        "bearing": 90,
        "pos": start,
    }
    for rule in inp:
        if rule[0] == "L":
            state["bearing"] = (state["bearing"] - rule[1]) % 360
        elif rule[0] == "R":
            state["bearing"] = (state["bearing"] + rule[1]) % 360
        elif rule[0] == "N":
            state["pos"][1] -= rule[1]
        elif rule[0] == "S":
            state["pos"][1] += rule[1]
        elif rule[0] == "W":
            state["pos"][0] -= rule[1]
        elif rule[0] == "E":
            state["pos"][0] += rule[1]
        elif rule[0] == "F":
            if state["bearing"] == 0:
                state["pos"][1] -= rule[1]
            elif state["bearing"] == 90:
                state["pos"][0] += rule[1]
            elif state["bearing"] == 180:
                state["pos"][1] += rule[1]
            elif state["bearing"] == 270:
                state["pos"][0] -= rule[1]
    return state


def run_waypoint_commands(inp, ship_start, waypoint_start):
    state = {
        "bearing": 90,
        "spos": ship_start,
        "wpos": waypoint_start,
    }
    for rule in inp:
        if rule[0] == "L" or rule[0] == "R":
            state["wpos"] = rotate(
                state["wpos"][0],
                state["wpos"][1],
                rule[1],
                rule[0]
            )
        elif rule[0] == "N":
            state["wpos"][1] -= rule[1]
        elif rule[0] == "S":
            state["wpos"][1] += rule[1]
        elif rule[0] == "W":
            state["wpos"][0] -= rule[1]
        elif rule[0] == "E":
            state["wpos"][0] += rule[1]
        elif rule[0] == "F":
            state["spos"][0] += state["wpos"][0] * rule[1]
            state["spos"][1] += state["wpos"][1] * rule[1]
    return state


def solve_p1(inp):
    ship = run_commands(inp, [0, 0])
    return abs(sum(ship["pos"]))


def solve_p2(inp):
    ship = run_waypoint_commands(inp, [0, 0], [10, -1])
    return abs(sum(ship["spos"]))


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 1152
    assert solve_p2(inp) == 58637


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 25
    assert solve_p2(inp) == 286


if __name__ == "__main__":
    main()
