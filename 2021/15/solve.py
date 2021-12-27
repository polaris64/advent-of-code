def read_input(filename):
    with open(filename, "rt") as fh:
        return [[int(x) for x in line.strip()] for line in fh.readlines()]

def gen_grid(inp, mul = (1, 1)):
    for y in range(len(inp) * mul[1]):
        row = []
        y_norm = y % len(inp)
        for x in range(len(inp[0]) * mul[0]):
            if x < len(inp[0]) and y < len(inp):
                num = inp[y][x]
            else:
                src_num = inp[y_norm][x % len(inp[0])] - 1
                num = src_num
                num += (y // len(inp))
                num += (x // len(inp[0]))
                num = num % 9
                num += 1
            row.append(num)
        yield row

def get_neighbours(cell, inp, mul = (1, 1)):
    neighbours = []
    offsets = ((-1, 0), (0, -1), (1, 0), (0, 1))
    for ox, oy in offsets:
        nx = cell[0] + ox
        ny = cell[1] + oy
        if nx >= 0 and ny >= 0 and nx < len(inp[0]) * mul[0] and ny < len(inp) * mul[1]:
            neighbours.append((nx, ny))
    return neighbours

def reconstruct_path(came_from, current):
    total_path = [current]
    while current in came_from.keys():
        current = came_from[current]
        total_path.append(current)
    total_path.reverse()
    return total_path

def astar(grid, start_pos, goal_pos):
    open_set = set([start_pos])
    came_from = dict()

    def h(cell):
        return abs(goal_pos[0] - cell[0]) + abs(goal_pos[1] - cell[1])

    g_score = dict()
    for y, row in enumerate(grid):
        for x, _ in enumerate(row):
            g_score[(x, y)] = 9999999
    g_score[start_pos] = 0

    while len(open_set) > 0:
        current = min([(g_score[c] + h(c), c) for c in open_set])[1]
        if current == goal_pos:
            return reconstruct_path(came_from, current)
        open_set.remove(current)
        for n in get_neighbours(current, grid):
            tent_g_score = g_score[current] + grid[n[1]][n[0]]
            if tent_g_score < g_score[n]:
                came_from[n] = current
                g_score[n] = tent_g_score
                if n not in open_set:
                    open_set.add(n)

def render_grid(inp, path):
    res = ""
    for y, row in enumerate(inp):
        for x, cell in enumerate(row):
            res += str(cell) if (x, y) in path else "-"
        res += "\n"
    return res

def get_least_risky_path(inp, size_mul):
    grid = list(gen_grid(inp, size_mul))
    path = astar(grid, (0, 0), (len(grid[0]) - 1, len(grid) - 1))
    return sum([grid[y][x] for x, y in path if not (x == 0 and y == 0)])

def solve_p1(inp):
    return get_least_risky_path(inp, (1, 1))

def solve_p2(inp):
    return get_least_risky_path(inp, (5, 5))

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 609
    assert solve_p2(inp) == 2925

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 40
    assert solve_p2(inp) == 315

if __name__ == '__main__':
    main()
