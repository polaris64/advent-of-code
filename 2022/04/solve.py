import re


def read_input(filename):
    with open(filename, "r") as fh:
        return [parse_line(x.strip()) for x in fh.readlines()]


def parse_line(line):
    matches = re.search(
        r"([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)",
        line
    )
    groups = matches.groups()
    assert len(groups) == 4
    return (
        range(int(groups[0]), int(groups[1]) + 1),
        range(int(groups[2]), int(groups[3]) + 1)
    )


def is_fully_contained(r1, r2):
    s1 = set(r1)
    s2 = set(r2)
    return s1.issubset(s2) or s2.issubset(s1)


def is_overlapping(r1, r2):
    s1 = set(r1)
    s2 = set(r2)
    return len(s1.intersection(s2)) > 0


def solve_p1(inp):
    return len([(r1, r2) for r1, r2 in inp if is_fully_contained(r1, r2)])


def solve_p2(inp):
    return len([(r1, r2) for r1, r2 in inp if is_overlapping(r1, r2)])


def main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    print(f"The solution to part 1 is: {res}")
    res = solve_p2(inp)
    print(f"The solution to part 2 is: {res}")


def test_ex():
    inp = read_input("input_ex.txt")
    res = solve_p1(inp)
    assert res == 2
    res = solve_p2(inp)
    assert res == 4


def test_main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    assert res == 547
    res = solve_p2(inp)
    assert res == 843


if __name__ == '__main__':
    main()
