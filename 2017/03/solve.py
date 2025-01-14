import functools
import math


INP = 277678


def get_level(n):
    return math.ceil(math.sqrt(n)) // 2


def get_level_dim(n):
    return n * 2 + 1


def get_dist(n):
    # See https://oeis.org/A214526
    if n == 1:
        return 0
    level = get_level(n)
    return level + abs(((n - 1) % (2 * level)) - level)


@functools.cache
def get_x_coord(n):
    if n == 1:
        return 0
    return get_x_coord(n - 1) + round(math.sin(math.floor(math.sqrt(4 * n - 7)) * math.pi / 2))


@functools.cache
def get_y_coord(n):
    if n == 1:
        return 0
    return get_y_coord(n - 1) + round(math.cos(math.floor(math.sqrt(4 * n - 7)) * math.pi / 2))
    

def get_neighbours(grid, coord):
    for x in [-1, 0, 1]:
        for y in [-1, 0, 1]:
            c = (coord[0] + x, coord[1] + y)
            if x == 0 and y == 0:
                continue
            if c in grid:
                yield grid[c]


def gen_digits():
    n = 1
    grid = dict()
    grid[(0, 0)] = 1
    while True:
        if n == 1:
            yield grid[(0, 0)]
        else:
            coord = (get_x_coord(n), get_y_coord(n))
            grid[coord] = sum(get_neighbours(grid, coord))
            yield grid[coord]
        n += 1


def solve_p1(inp):
    return get_dist(inp)


def solve_p2(inp):
    g = gen_digits()
    while True:
        x = next(g)
        if x > INP:
            return x


def test_live():
    assert solve_p1(INP) == 475
    assert solve_p2(INP) == 279138


def test_ex():
    assert solve_p1(1) == 0
    assert solve_p1(12) == 3
    assert solve_p1(23) == 2
    assert solve_p1(1024) == 31


def main():
    print(f"The solution to part 1 is: {solve_p1(INP)}")
    print(f"The solution to part 2 is: {solve_p2(INP)}")


if __name__ == "__main__":
    main()
