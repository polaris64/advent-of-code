from functools import cache
import re


def read_input(filename):
    with open(filename, "rt") as fhandle:
        res = [parse_line(x.strip()) for x in fhandle.readlines()]
        return {x[0]: tuple(x[1:]) for x in res}


def parse_line(line):
    res = None
    parts = line.split("->")
    match = re.search(r"([a-z0-9]+) ([A-Z]+) ([a-z0-9]+)", parts[0])
    if match:
        groups = match.groups()
        res = [parts[1].strip(), groups[1], [groups[0], groups[2]]]
    if not res:
        match = re.search(r"([A-Z]+) ([a-z0-9]+)", parts[0])
        if match:
            groups = match.groups()
            res = [parts[1].strip(), groups[0], [groups[1]]]
    if not res:
        res = [parts[1].strip(), "EQ", [parts[0].strip()]]
    res[2] = [int(x) if x.isdecimal() else x for x in res[2]]
    return res


def calc_output_for(id, inp):
    @cache
    def _rec(id):
        if isinstance(id, int):
            return id
        arg1 = _rec(inp[id][1][0])
        if inp[id][0] == "EQ":
            return arg1
        if inp[id][0] == "NOT":
            return ~arg1 & 0xFFFF
        arg2 = _rec(inp[id][1][1])
        if inp[id][0] == "AND":
            return arg1 & arg2
        if inp[id][0] == "OR":
            return arg1 | arg2
        if inp[id][0] == "LSHIFT":
            return arg1 << arg2
        if inp[id][0] == "RSHIFT":
            return arg1 >> arg2
    return _rec(id)


def solve_p1(inp):
    return calc_output_for("a", inp)


def solve_p2(inp):
    p1 = solve_p1(inp)
    inp["b"] = ("EQ", [p1])
    return solve_p1(inp)


def test_ex():
    tests = {
        "d": 72,
        "e": 507,
        "f": 492,
        "g": 114,
        "h": 65412,
        "i": 65079,
        "x": 123,
        "y": 456,
    }
    inp = read_input("input_ex.txt")
    for id, res in tests.items():
        assert calc_output_for(id, inp) == res


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 956
    assert solve_p2(inp) == 40149


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


if __name__ == "__main__":
    main()
