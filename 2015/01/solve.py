def read_input(filename):
    with open(filename, "rt") as fhandle:
        return fhandle.read().strip()


def solve_p1(inp):
    opening_count = len([x for x in inp if x == "("])
    closing_count = len([x for x in inp if x == ")"])
    return opening_count - closing_count


def solve_p2(inp):
    floor = 1
    for idx, ch in enumerate(inp):
        if ch == "(":
            floor += 1
        elif ch == ")":
            floor -= 1
        if floor == -1:
            return idx
    return None


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 280
    assert solve_p2(inp) == 1797


if __name__ == "__main__":
    main()
