def read_input(fname):
    with open(fname, "r") as handle:
        return [x.strip() for x in handle.readlines()]


def get_result(inp):
    row = int("".join(["0" if x == "F" else "1" for x in list(inp)[:7]]), 2)
    col = int("".join(["0" if x == "L" else "1" for x in list(inp)[7:10]]), 2)
    return (row, col, row * 8 + col)


def solve_p1(inp):
    return max(
        [get_result(x)[2] for x in inp]
    )


def solve_p2(inp):
    ids = [get_result(x)[2] for x in inp]
    missing_ids = [x for x in range(solve_p1(inp)) if x not in ids]
    return [x for x in missing_ids if x - 1 in ids and x + 1 in ids][0]


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 947
    assert solve_p2(inp) == 636


def test_ex():
    inputs = [
        ("FBFBBFFRLR", 44, 5, 357),
        ("BFFFBBFRRR", 70, 7, 567),
        ("FFFBBBFRRR", 14, 7, 119),
        ("BBFFBBFRLL", 102, 4, 820),
    ]
    for inp in inputs:
        assert get_result(inp[0]) == inp[1:]


if __name__ == "__main__":
    main()
