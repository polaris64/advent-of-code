def read_input(fn):
    with open(fn, "rt") as fh:
        return [int(x) for x in [l.strip() for l in fh.readlines()]]


def process(jumps, offset_mod):
    steps = 0
    idx = 0
    while True:
        try:
            jump = jumps[idx]
        except IndexError:
            return steps
        offset_mod(jumps, idx)
        idx += jump
        steps += 1


def solve_p1(inp):
    def _offset_mod(jumps, idx):
        jumps[idx] += 1

    return process(inp, _offset_mod)


def solve_p2(inp):
    def _offset_mod(jumps, idx):
        if jumps[idx] >= 3:
            jumps[idx] -= 1
        else:
            jumps[idx] += 1

    return process(inp, _offset_mod)


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 355965
    inp = read_input("input.txt")
    assert solve_p2(inp) == 26948068


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 5
    inp = read_input("input_ex.txt")
    assert solve_p2(inp) == 10


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    inp = read_input("input.txt")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
