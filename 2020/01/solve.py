from operator import mul
from functools import reduce
from itertools import combinations


def read_input(fn):
    with open(fn, "r") as f:
        return [int(l.strip()) for l in f.readlines()]


# General-purpose solution using itertools.combinations
def solve(inp, size):
    for comb in combinations(inp, size):
        if sum(comb) == 2020:
            return reduce(lambda x, y: x * y, comb)
    return None

# Simple one-line solutions using generator expressions, the
# operator.mul function and functools.reduce.
def solve_p1(inp):
    return mul(
        *next(
            (x, y) for x in inp for y in inp if y != x and x + y == 2020
        )
    )

def solve_p2(inp):
    return reduce(
        mul,
        next(
            (x, y, z) for x in inp for y in inp for z in inp if x != y != z and x + y + z == 2020
        )
    )


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve(inp, 2) == 805731
    assert solve(inp, 3) == 192684960


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve(inp, 2) == 514579
    assert solve(inp, 3) == 241861950


if __name__ == "__main__":
    main()
