from functools import reduce
from operator import mul


SLOPES_P1 = [(3, 1)]
SLOPES_P2 = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]


def read_input(fname):
    with open(fname, "r") as f:
        return [l.strip() for l in f.readlines()]


def traverse_grid(grid, xoff, yoff):
    if yoff >= len(grid):
        return None
    return grid[yoff][xoff % len(grid[0])]


def run_traversal(grid, xoff, yoff):
    trees = 0
    xpos = 0
    ypos = 0
    while ypos < len(grid):
        if traverse_grid(grid, xpos + xoff, ypos + yoff) == '#':
            trees += 1
        xpos += xoff
        ypos += yoff
    return trees


def solve(grid, slopes):
    return reduce(
        mul,
        [run_traversal(grid, xoff, yoff) for (xoff, yoff) in slopes],
        1
    )


def main():
    grid = read_input("input.txt")
    sln1 = solve(grid, SLOPES_P1)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve(grid, SLOPES_P2)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    grid = read_input("input.txt")
    assert solve(grid, SLOPES_P1) == 254
    assert solve(grid, SLOPES_P2) == 1666768320


def test_ex():
    grid = read_input("input_ex.txt")
    assert solve(grid, SLOPES_P1) == 7
    assert solve(grid, SLOPES_P2) == 336


if __name__ == "__main__":
    main()
