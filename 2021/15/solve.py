def read_input(filename):
    with open(filename, "rt") as fh:
        return [[int(x) for x in line.strip()] for line in fh.readlines()]

def get_neighbours(cell, inp):
    neighbours = []
    offsets = ((-1, 0), (0, -1), (1, 0), (0, 1))
    for ox, oy in offsets:
        nx = cell[0] + ox
        ny = cell[1] + oy
        if nx >= 0 and ny >= 0 and nx < len(inp[0]) and ny < len(inp):
            neighbours.append((nx, ny))
    return neighbours

def dijkstra(inp, start, goal):
    nodes = dict()
    for y, row in enumerate(inp):
        for x, _ in enumerate(row):
            nodes[(x, y)] = 9999999
    nodes[start] = 0
    curr = start

    visited = dict()

    while len(nodes) > 0:
        if curr == goal:
            visited[curr] = nodes.pop(curr)
            break
        for n in get_neighbours(curr, inp):
            if n not in visited:
                tentative_distance = nodes[curr] + inp[n[1]][n[0]]
                if nodes[n] > tentative_distance:
                    nodes[n] = tentative_distance
        visited[curr] = nodes.pop(curr)
        curr = min(nodes.items(), key=lambda x: x[1])[0]

    return visited

def render_grid(inp, path):
    res = ""
    for y, row in enumerate(inp):
        for x, cell in enumerate(row):
            res += str(cell) if (x, y) in path else "-"
        res += "\n"
    return res

def solve_p1(inp):
    goal = (len(inp[0]) - 1, len(inp) - 1)
    return dijkstra(inp, (0, 0), goal)[goal]

def solve_p2(inp):
    return None

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 609
    assert solve_p2(inp) == None

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 40
    assert solve_p2(inp) == None

if __name__ == '__main__':
    main()
