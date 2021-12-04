def read_input(filename):
    with open(filename, "rt") as fhandle:
        return [int(x.strip()) for x in fhandle.readlines()]


def get_combinations(inp, target, curr):
    if sum(curr) > target:
        return []
    if len(inp) == 1:
        if sum(curr) + inp[0] == target:
            return [curr + inp]
        else:
            return []
    combs = []
    for idx, x in enumerate(inp):
        if sum(curr) + x == target:
            combs += [curr + [x]]
        else:
            combs += get_combinations(inp[idx + 1:], target, curr + [x])
    return combs
    

def solve_p1(inp, target):
    return len(get_combinations(inp, target, []))


def solve_p2(inp, target):
    combs = get_combinations(inp, target, [])
    min_len = len(min(combs, key=lambda x: len(x)))
    return len([x for x in combs if len(x) == min_len])


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp, 150)))
    print("The solution to part 2 is: {}".format(solve_p2(inp, 150)))


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp, 150) == 654
    assert solve_p2(inp, 150) == 57


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp, 25) == 4
    assert solve_p2(inp, 25) == 3


if __name__ == "__main__":
    main()
