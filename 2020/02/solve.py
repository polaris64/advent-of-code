import re


def read_input(fn):
    with open(fn, "r") as f:
        return [parse_line(l.strip()) for l in f.readlines()]

def parse_line(l):
    # Parse to a list. Indexes are as follows: -
    # 0: lower bound, 1: upper bound, 2: char, 3: password
    return re.match(r"([0-9]+)-([0-9]+) ([a-z]): (.*)", l).groups()

def is_password_valid_p1(pwr):
    """Check that a password contains the given character a certain number of times."""
    ch_count = pwr[3].count(pwr[2])
    return int(pwr[0]) <= ch_count <= int(pwr[1])

def is_password_valid_p2(pwr):
    """Check that a password contains the given character at either of the indices but not both."""
    ch = pwr[2]
    idx1 = int(pwr[0]) - 1
    idx2 = int(pwr[1]) - 1
    return (pwr[3][idx1] == ch) ^ (pwr[3][idx2] == ch)

def solve_p1(inp):
    return len([x for x in inp if is_password_valid_p1(x)])

def solve_p2(inp):
    return len([x for x in inp if is_password_valid_p2(x)])


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 398
    assert solve_p2(inp) == 562


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 2
    assert solve_p2(inp) == 1


if __name__ == "__main__":
    main()
