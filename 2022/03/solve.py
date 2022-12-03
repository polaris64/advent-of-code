def read_input(filename):
    with open(filename, "r") as fh:
        return [x.strip() for x in fh.readlines()]


def split_compartments(inp):
    return [(x[:len(x) // 2], x[len(x) // 2:]) for x in inp]


def score_item(item):
    return ord(item) - 96 if not item.isupper() else ord(item) - 38


def group_lines(inp, size):
    return [
        inp[i * size:(i * size) + size]
        for i in range(0, len(inp) // size)
    ]


def solve_p1(inp):
    rucksacks = split_compartments(inp)
    return sum(
        [
            score_item(list(set(c1).intersection(set(c2)))[0])
            for c1, c2 in rucksacks
        ]
    )


def get_badge_from_group(group):
    sets = [set(bag) for bag in group]
    final_set = set(sets[0])
    for s in sets[1:]:
        final_set = final_set.intersection(s)
    return list(final_set)[0]


def solve_p2(inp):
    return sum(
        [
            score_item(get_badge_from_group(g))
            for g in group_lines(inp, 3)
        ]
    )


def main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    print(f"The solution to part 1 is: {res}")
    res = solve_p2(inp)
    print(f"The solution to part 2 is: {res}")


def test_ex():
    inp = read_input("input_ex.txt")
    res = solve_p1(inp)
    assert res == 157
    res = solve_p2(inp)
    assert res == 70


def test_main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    assert res == 7581
    res = solve_p2(inp)
    assert res == 2525


if __name__ == '__main__':
    main()
