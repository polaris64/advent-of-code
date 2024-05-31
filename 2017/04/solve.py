def read_input(fn):
    with open(fn, "rt") as fh:
        return [l.strip().split() for l in fh.readlines()]


def is_valid_p1(passphrase):
    return len(passphrase) == len(set(passphrase))


def is_valid_p2(passphrase):
    return is_valid_p1(["".join(sorted(list(x))) for x in passphrase])


def solve_p1(inp):
    return len([x for x in inp if is_valid_p1(x)])


def solve_p2(inp):
    return len([x for x in inp if is_valid_p2(x)])


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 337
    assert solve_p2(inp) == 231


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 2


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
