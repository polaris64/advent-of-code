from queue import PriorityQueue

def read_input(filename):
    with open(filename, "rt") as fh:
        return [[int(x) for x in line.strip()] for line in fh.readlines()]

def gen_grid(inp, mul = (1, 1)):
    len_x = len(inp[0])
    len_y = len(inp)
    for y in range(len(inp) * mul[1]):
        row = []
        y_norm = y % len_y
        for x in range(len_x * mul[0]):
            if x < len(inp[0]) and y < len_y:
                num = inp[y][x]
            else:
                src_num = inp[y_norm][x % len_x] - 1
                num = src_num
                num += (y // len_y)
                num += (x // len_x)
                num = num % 9
                num += 1
            row.append(num)
        yield row

def get_neighbours(cell, inp, mul = (1, 1)):
    len_x = len(inp[0]) * mul[0]
    len_y = len(inp) * mul[1]
    offsets = ((-1, 0), (0, -1), (1, 0), (0, 1))
    neighbours = [(cell[0] + ox, cell[1] + oy) for ox, oy in offsets]
    return [n for n in neighbours if n[0] >= 0 and n[1] >= 0 and n[0] < len_x and n[1] < len_y]

def reconstruct_path(came_from, current):
    total_path = [current]
    while current in came_from.keys():
        current = came_from[current]
        total_path.append(current)
    total_path.reverse()
    return total_path

def astar(grid, start_pos, goal_pos):
    open_queue = PriorityQueue()
    open_queue.put((0, start_pos))
    came_from = dict()

    f_score = dict()
    g_score = dict()
    for y, row in enumerate(grid):
        for x, _ in enumerate(row):
            g_score[(x, y)] = 9999999
            f_score[(x, y)] = abs(goal_pos[0] - x) + abs(goal_pos[1] - y)
    g_score[start_pos] = 0

    while open_queue.not_empty:
        current = open_queue.get()[1]
        if current == goal_pos:
            return reconstruct_path(came_from, current)
        for n in get_neighbours(current, grid):
            tent_g_score = g_score[current] + grid[n[1]][n[0]]
            if tent_g_score < g_score[n]:
                came_from[n] = current
                g_score[n] = tent_g_score
                open_queue.put((g_score[n] + f_score[n], n))

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
