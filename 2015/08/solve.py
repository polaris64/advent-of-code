def read_input(filename):
    with open(filename, "rt") as fhandle:
        return [x.strip() for x in fhandle.readlines()]


def unescape_str(s):
    return eval(s)


def escape_str(s):
    table = str.maketrans({
        "\\": "\\\\",
        "\"": "\\\"",
    })
    return "\"" + s.translate(table) + "\""


def solve_p1(inp):
    return (
        sum(len(x) for x in inp) -
        sum(len(unescape_str(x)) for x in inp)
    )


def solve_p2(inp):
    return (
        sum(len(escape_str(x)) for x in inp) -
        sum(len(x) for x in inp)
    )


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 12
    assert solve_p2(inp) == 19


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 1342
    assert solve_p2(inp) == 2074


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


if __name__ == "__main__":
    main()
