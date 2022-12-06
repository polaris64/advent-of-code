TEST_INPUTS = [
    ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19),
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23),
    ("nppdvjthqldpwncqszvftbrmjlhg", 6, 23),
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29),
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26),
]


def read_input(filename):
    with open(filename, "r") as fh:
        return fh.readline().strip()


def find_marker(inp, length):
    for idx, ch in enumerate(inp):
        if idx < length:
            continue
        slice = inp[idx - length:idx]
        if len(set(slice)) != len(slice):
            continue
        return (idx, slice)


def solve_p1(inp):
    return find_marker(inp, 4)[0]


def solve_p2(inp):
    return find_marker(inp, 14)[0]


def main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    print(f"The solution to part 1 is: {res}")
    res = solve_p2(inp)
    print(f"The solution to part 2 is: {res}")


def test_ex():
    for inp, p1, p2 in TEST_INPUTS:
        res = solve_p1(inp)
        assert res == p1
        res = solve_p2(inp)
        assert res == p2


def test_main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    assert res == 1892
    res = solve_p2(inp)
    assert res == 2313


if __name__ == '__main__':
    main()
