from functools import lru_cache


def read_input(fname):
    with open(fname, "r") as handle:
        return [int(x.strip()) for x in handle.readlines()]


def traverse_list(inp, start_joltage):
    curr = start_joltage
    output = []
    while len(output) < len(inp):
        possible = [x for x in inp if x not in output and 1 <= x - curr <= 3]
        if len(possible) > 0:
            possible.sort()
            output.append((possible[0], possible[0] - curr))
            curr = possible[0]
        else:
            break
    output.append((output[-1][0] + 3, 3))
    return output


def return_counts(output):
    return {k:len([x for x in output if x[1] == k]) for k in range(1, 4)}


def combs(items, start):
    @lru_cache(maxsize=None)
    def _rec(idx, last):
        if 1 < items[idx] - last < 3:
            return 0
        if idx == len(items) - 1:
            return 1
        possible = [(idx, x) for (idx, x) in enumerate(items[idx:idx + 4]) if 1 <= x - last <= 3]
        return sum([_rec(idx + poss[0], poss[1]) for poss in possible])
    return _rec(0, start)


def solve_p1(inp):
    counts = return_counts(traverse_list(inp, 0))
    return counts[1] * counts[3]


def solve_p2(inp):
    inp.sort()
    return combs(inp, 0)


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 1700
    assert solve_p2(inp) == 12401793332096


def test_ex():
    inp = read_input("input_ex1.txt")
    assert solve_p1(inp) == 35
    assert solve_p2(inp) == 8
    inp = read_input("input_ex2.txt")
    assert solve_p1(inp) == 220
    assert solve_p2(inp) == 19208


if __name__ == "__main__":
    main()
