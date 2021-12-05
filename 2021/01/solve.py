def read_input(f):
    with open(f, "rt") as fh:
        return [int(x.strip()) for x in fh.readlines()]

def solve_p1(inp):
    return sum([1 if y > x else 0 for x, y in zip(inp, inp[1:])])

def solve_p2(inp):
    windows = [sum(x) for x in zip(inp, inp[1:], inp[2:])]
    return solve_p1(windows)

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 7
    assert solve_p2(inp) == 5

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 1502
    assert solve_p2(inp) == 1538

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

if __name__ == '__main__':
    main()
