def read_input(filename):
    with open(filename, "r") as fh:
        return [list(line.strip()) for line in fh.readlines()]


def get_pathfind_ctx(inp):
    ctx = {
        "start": None,
        "goal": None,
        "grid": [row.copy() for row in inp],
    }
    for y, row in enumerate(inp):
        for x, cell in enumerate(row):
            if cell == "S":
                ctx["start"] = (x, y)
                ctx["grid"][y][x] = "a"
            elif cell == "E":
                ctx["goal"] = (x, y)
                ctx["grid"][y][x] = "z"
    return ctx


def get_neighbours(grid, node):
    for x in range(-1, 2, 1):
        gx = node[0] + x
        if gx >= 0 and gx < len(grid[node[1]]):
            if ord(grid[node[1]][gx]) <= ord(grid[node[1]][node[0]]) + 1:
                yield (node[0] + x, node[1])
    for y in range(-1, 2, 1):
        gy = node[1] + y
        if gy >= 0 and gy < len(grid):
            if ord(grid[gy][node[0]]) <= ord(grid[node[1]][node[0]]) + 1:
                yield (node[0], node[1] + y)


def find_path(ctx):
    came_from = {}
    open_set = set([ctx["start"]])

    def _h(pos):
        return abs(pos[0] - ctx["goal"][0]) + abs(pos[1] - ctx["goal"][1])

    g_score = {ctx["start"]: 0}
    f_score = {ctx["start"]: _h(ctx["start"])}

    def _compare_g(score, pos):
        pos_score = g_score.get(pos)
        return -1 if pos_score is None or score < pos_score else 1

    def _reconstruct(node):
        final_path = [node]
        current = node
        while True:
            current = came_from[current]
            final_path.insert(0, current)
            if current == ctx["start"]:
                break
        return final_path

    while len(open_set) > 0:
        node = sorted(
            [(f_score[x], x)
             for x in open_set],
            key=lambda x: x[0])[0][1]
        if node == ctx["goal"]:
            return _reconstruct(node)

        open_set.remove(node)
        for neighbour in get_neighbours(ctx["grid"], node):
            tent_g_score = g_score[node] + 1
            if _compare_g(tent_g_score, neighbour) == -1:
                came_from[neighbour] = node
                g_score[neighbour] = tent_g_score
                f_score[neighbour] = tent_g_score + _h(neighbour)
                open_set.add(neighbour)


def solve_p1(inp):
    ctx = get_pathfind_ctx(inp)
    return len(find_path(ctx)) - 1


def solve_p2(inp):
    ctx = get_pathfind_ctx(inp)
    shortest = None
    for start_y, row in enumerate(ctx["grid"]):
        for start_x, cell in enumerate(row):
            if cell != "a":
                continue
            ctx["start"] = (start_x, start_y)
            path = find_path(ctx)
            if path is not None:
                pathlen = len(path) - 1
                if shortest is None or pathlen < shortest:
                    shortest = pathlen
    return shortest


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: -\n{solve_p2(inp)}")


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 31
    assert solve_p2(inp) == 29


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 440
    assert solve_p2(inp) == 439


if __name__ == '__main__':
    main()
