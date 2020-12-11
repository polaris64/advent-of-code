from copy import deepcopy


def read_input(fname):
    with open(fname, "r") as handle:
        return [list(x.strip()) for x in handle.readlines()]


def get_nearest_neighbours(grid, xpos, ypos):
    occupied = 0
    for y in range(-1, 2):
        for x in range(-1, 2):
            if x == 0 and y == 0:
                continue
            y_idx = y + ypos
            if y_idx < 0 or y_idx >= len(grid):
                continue
            x_idx = x + xpos
            if x_idx < 0 or x_idx >= len(grid[0]):
                continue
            if grid[y_idx][x_idx] == "#":
                occupied += 1
    return occupied


def get_visible_neighbours(grid, xpos, ypos):
    occupied = 0
    for y in range(-1, 2):
        for x in range(-1, 2):
            if x == 0 and y == 0:
                continue
            occupied += raytrace(grid, xpos, ypos, x, y)
    return occupied


def raytrace(grid, xpos, ypos, xdir, ydir):
    x = xpos + xdir
    y = ypos + ydir
    while 0 <= x < len(grid[0]) and 0 <= y < len(grid):
        if grid[y][x] == "#":
            return 1
        if grid[y][x] == "L":
            return 0
        x += xdir
        y += ydir
    return 0


def run_simulation(grid, max_occupied, neighbour_check_fn):
    prev_grid = grid
    new_grid = deepcopy(grid)
    while True:
        for y, row in enumerate(prev_grid):
            for x, col in enumerate(row):
                occ_n = neighbour_check_fn(prev_grid, x, y)
                if col == "#" and occ_n >= max_occupied:
                    new_grid[y][x] = "L"
                elif col == "L" and occ_n == 0:
                    new_grid[y][x] = "#"
        if new_grid == prev_grid:
            return new_grid
        prev_grid = deepcopy(new_grid)
    return None


def count_occupied(grid):
    return sum([sum([1 if cell == "#" else 0 for cell in row]) for row in grid])


def solve_p1(inp):
    grid = run_simulation(inp, 4, get_nearest_neighbours)
    return count_occupied(grid)


def solve_p2(inp):
    grid = run_simulation(inp, 5, get_visible_neighbours)
    return count_occupied(grid)


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 2263
    assert solve_p2(inp) == 2002


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 37
    assert solve_p2(inp) == 26


if __name__ == "__main__":
    main()
