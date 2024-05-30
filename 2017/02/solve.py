import itertools


def read_input(fn):
    with open(fn, "rt") as fh:
        return [[int(num) for num in l.split()] for l in [l.strip() for l in fh.readlines()]]


def solve_p1(inp):
    return sum([max(row) - min(row) for row in inp])


def check_pair(x, y):
    if x / y == x // y:
        return x // y
    elif y / x == y // x:
        return y // x
    else:
        return None
    
def solve_p2(inp):
    return sum(
        [
            [
                x for x
                in [check_pair(x, y) for x, y in itertools.combinations(row, r=2)]
                if x is not None][0]
            for row in inp
        ]
    )


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 36766
    assert solve_p2(inp) == 261


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 18
    inp = read_input("input_ex2.txt")
    assert solve_p2(inp) == 9


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
