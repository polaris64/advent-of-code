from functools import cmp_to_key
import json
from math import prod


def read_input(filename):
    with open(filename, "r") as fh:
        return parse_input([line.strip() for line in fh.readlines()])


def parse_input(lines):
    res = []
    pairs = []
    for line in lines:
        if len(line) == 0:
            res.append(tuple(pairs))
            pairs = []
            continue
        pairs.append(json.loads(line))
    res.append(tuple(pairs))
    return res


def compare(p1, p2):
    res = 0
    if len(p1) == 0 and len(p2) > 0:
        return 1
    if len(p1) > 0 and len(p2) == 0:
        return -1
    for v1, v2 in zip(p1, p2):
        if isinstance(v1, list) or isinstance(v2, list):
            if isinstance(v1, list) and isinstance(v2, list):
                res = compare(v1, v2)
            elif isinstance(v1, int) and isinstance(v2, list):
                res = compare([v1], v2)
            elif isinstance(v1, list) and isinstance(v2, int):
                res = compare(v1, [v2])
        elif v1 == v2:
            res = 0
        elif v1 < v2:
            res = 1
        elif v1 > v2:
            res = -1
        if res == 1 or res == -1:
            break
    if res == 0 and len(p1) < len(p2):
        res = 1
    if res == 0 and len(p1) > len(p2):
        res = -1
    return res


def check_packets(inp):
    return [compare(p1, p2) for p1, p2 in inp]


def sort_packets(inp):
    packets1 = [p1 for p1, _ in inp]
    packets2 = [p2 for _, p2 in inp]
    packets1.extend(packets2)
    packets1.extend([[[2]], [[6]]])
    return sorted(packets1, key=cmp_to_key(compare))[::-1]


def solve_p1(inp):
    return sum([idx + 1 for idx, x in enumerate(check_packets(inp)) if x == 1])


def solve_p2(inp):
    sp = sort_packets(inp)
    return prod([
        idx + 1
        for idx, p
        in enumerate(sp)
        if p in [[[2]], [[6]]]
    ])


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 13
    assert solve_p2(inp) == 140


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 6484
    assert solve_p2(inp) == 19305


if __name__ == '__main__':
    main()
