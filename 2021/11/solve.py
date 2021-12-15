def read_input(filename):
    with open(filename, "rt") as fh:
        return [[[int(x, 10), False] for x in line.strip()] for line in fh.readlines()]

def get_neighbours(x, y, gw, gh):
    offsets = [
        (-1, -1), (0, -1), (1, -1),
        (-1,  0),          (1,  0),
        (-1,  1), (0,  1), (1,  1),
    ]
    ns = []
    for ox, oy in offsets:
        if x + ox >= 0 and x + ox < gw and y + oy >= 0 and y + oy < gh:
            ns.append((x + ox, y + oy))
    return ns

def process_flash(grid, x, y):
    grid[y][x][1] = True
    ns = get_neighbours(x, y, len(grid[0]), len(grid))
    for nx, ny in ns:
        grid[ny][nx][0] += 1
        if grid[ny][nx][0] > 9 and not grid[ny][nx][1]:
            process_flash(grid, nx, ny)

def run_step(grid):
    # Reset flashed flags
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            grid[y][x][1] = False

    # Increase energy of each octopus
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            grid[y][x][0] += 1

    # Process flashes (octopi with energy levels greater than 9)
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x][0] > 9 and not grid[y][x][1]:
                process_flash(grid, x, y)

    flash_count = sum([len([x for x in row if x[1]]) for row in grid])

    # Return level of all flashed octopi to 0
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x][1]:
                grid[y][x][0] = 0

    return flash_count

def simulate(inp):
    flashes = 0
    step = 0
    while True:
        step += 1
        flashes += run_step(inp)
        yield (inp, flashes, step)

def render_grid(grid):
    res = ""
    for row in grid:
        for x, flashed in row:
            if flashed:
                res += ">" + str(x) + "<"
            else:
                res += " " + str(x) + " "
        res += "\n"
    return res

def solve_p1(inp):
    flashes = None
    for _, f, s in simulate(inp):
        flashes = f
        if s == 100:
            break
    return flashes

def solve_p2(inp):
    for grid, _, step in simulate(inp):
        all_flash = all(all([f for _, f in row]) for row in grid)
        if all_flash:
            return step

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    inp = read_input("input.txt")
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 1679
    inp = read_input("input.txt")
    assert solve_p2(inp) == 519

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 1656
    inp = read_input("input_ex.txt")
    assert solve_p2(inp) == 195

if __name__ == '__main__':
    main()
