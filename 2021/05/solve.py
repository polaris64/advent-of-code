from typing import DefaultDict


def read_input(filename):
    with open(filename, "rt") as fh:
        return [parse_line(x.strip()) for x in fh.readlines()]

def parse_line(line):
    parts = line.split(" ")
    return (parse_coord(parts[0]), parse_coord(parts[2]))

def parse_coord(coord):
    return tuple([int(x) for x in coord.split(",")])

def filter_hv(lines):
    return [l for l in lines if l[0][0] == l[1][0] or l[0][1] == l[1][1]]

def gen_grid(lines):
    grid = DefaultDict(lambda: 0)
    for l in lines:
        xr = (l[0][0], l[1][0])
        yr = (l[0][1], l[1][1])
        x = xr[0]
        y = yr[0]
        if l[0][0] == l[1][0]:
            x_inc = 0
            y_inc = 1 if yr[1] > yr[0] else -1
        elif l[0][1] == l[1][1]:
            x_inc = 1 if xr[1] > xr[0] else -1
            y_inc = 0
        else:
            x_inc = 1 if xr[1] > xr[0] else -1
            y_inc = 1 if yr[1] > yr[0] else -1
        while True:
            grid[(x, y)] += 1
            x += x_inc
            y += y_inc
            if x == xr[1] and y == yr[1]:
                grid[(x, y)] += 1
                break
    return grid

def print_grid(grid):
    min_x = min(*[k[0] for k in grid.keys()])
    max_x = max(*[k[0] for k in grid.keys()])
    min_y = min(*[k[1] for k in grid.keys()])
    max_y = max(*[k[1] for k in grid.keys()])
    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            n = grid[(x, y)]
            print(n if n > 0 else ".", end="")
        print()


def solve_p1(inp):
    return len([x for x in gen_grid(filter_hv(inp)).values() if x >= 2])

def solve_p2(inp):
    return len([x for x in gen_grid(inp).values() if x >= 2])

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 6007
    assert solve_p2(inp) == 19349

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 5
    assert solve_p2(inp) == 12

if __name__ == '__main__':
    main()
