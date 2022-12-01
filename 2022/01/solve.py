def read_input(filename):
    with open(filename, "r") as fh:
        return parse_input([x.strip() for x in fh.readlines()])


def parse_input(inp):
    groups = []
    group = []
    for line in inp:
        if len(line) > 0:
            group.append(int(line))
        else:
            groups.append(group)
            group = []
    groups.append(group)
    return groups


def sum_elf_groups(inp):
    return [sum(lst) for lst in inp]


def solve_p1(inp):
    return max(sum_elf_groups(inp))


def solve_p2(inp):
    return sum(sorted(sum_elf_groups(inp))[-3:])


def main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    print(f"The solution to part 1 is: {res}")
    res = solve_p2(inp)
    print(f"The solution to part 2 is: {res}")


def test_ex():
    inp = read_input("input_ex.txt")
    res = solve_p1(inp)
    assert res == 24000
    res = solve_p2(inp)
    assert res == 45000


def test_main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    assert res == 69693
    res = solve_p2(inp)
    assert res == 200945


if __name__ == '__main__':
    main()
