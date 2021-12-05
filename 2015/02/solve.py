from functools import reduce
import operator


def read_input(filename):
    with open(filename, "rt") as fhandle:
        return [parse_package(x.strip()) for x in fhandle.readlines()]


def parse_package(line):
    l, w, h = line.split("x")
    return (int(l), int(w), int(h))


def get_paper_area(package):
    l, w, h = package
    dims = [l * w, w * h, h * l]
    lowest = min(dims)
    return sum([2 * x for x in dims]) + lowest


def get_ribbon_length(package):
    len_wrap = sum([x * 2 for x in sorted(package)[:2]])
    len_ribbon = reduce(operator.mul, package)
    return len_wrap + len_ribbon


def solve_p1(inp):
    return sum(get_paper_area(x) for x in inp)


def solve_p2(inp):
    return sum(get_ribbon_length(x) for x in inp)


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 1588178
    assert solve_p2(inp) == 3783758


def test_ex():
    assert solve_p1([(2, 3, 4)]) == 58
    assert solve_p1([(1, 1, 10)]) == 43
    assert solve_p2([(2, 3, 4)]) == 34
    assert solve_p2([(1, 1, 10)]) == 14


if __name__ == "__main__":
    main()
