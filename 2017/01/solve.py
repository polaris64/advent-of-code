def read_input(fn):
    with open(fn, "rt") as fh:
        return [int(ch) for ch in [l.strip() for l in fh.readlines()][0]]


def solve(inp, offset):
    return sum(
        [
            x for x, y 
            in list(zip(
                inp + inp,
                (inp + inp)[offset:]
            ))[:len(inp)]
            if x == y
        ]
    )


def solve_p1(inp):
    return solve(inp, 1)


def solve_p2(inp):
    return solve(inp, len(inp) // 2)


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 1097
    assert solve_p2(inp) == 1188


def test_ex():
    assert solve_p1([1,1,2,2]) == 3
    assert solve_p1([1,1,1,1]) == 4
    assert solve_p1([1,2,3,4]) == 0
    assert solve_p1([9,1,2,1,2,1,2,9]) == 9

    assert solve_p2([1,2,1,2]) == 6
    assert solve_p2([1,2,2,1]) == 0
    assert solve_p2([1,2,3,4,2,5]) == 4
    assert solve_p2([1,2,3,1,2,3]) == 12
    assert solve_p2([1,2,1,3,1,4,1,5]) == 4


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
