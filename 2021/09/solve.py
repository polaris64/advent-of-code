from math import prod

def read_input(filename):
    with open(filename, "rt") as fh:
        return [list([int(x, 10) for x in line.strip()]) for line in fh.readlines()]

def get_neighbours(inp, x, y):
    offsets = [(0, -1), (1, 0), (0, 1), (-1, 0)]
    neighbours = []
    for o in offsets:
        nx = o[0] + x
        ny = o[1] + y
        if nx >= 0 and nx < len(inp[0]) and ny >= 0 and ny < len(inp):
            neighbours.append((nx, ny, inp[ny][nx]))
    return neighbours

def find_low_points(inp):
    for y, row in enumerate(inp):
        for x, height in enumerate(row):
            ns = get_neighbours(inp, x, y)
            if height < min([n[2] for n in ns]):
                yield (x, y, height)

def get_basin_neighbours(inp, x, y, pts):
    pts.add((x, y))
    ns = set([n for n in get_neighbours(inp, x, y) if (n[0], n[1]) not in pts and n[2] != 9])
    for nx, ny, _ in ns:
        pts.add((nx, ny))
        others = get_basin_neighbours(inp, nx, ny, pts)
        ns = ns.union(others)
    return ns

def solve_p1(inp):
    return sum([x[2] + 1 for x in find_low_points(inp)])

def solve_p2(inp):
    lps = list(find_low_points(inp))
    basins = [[(lpx, lpy, inp[lpy][lpx])] + list(get_basin_neighbours(inp, lpx, lpy, set())) for lpx, lpy, _ in lps]
    large_basins = []
    for _ in range(3):
        max_len = max([(idx, len(x)) for idx, x in enumerate(basins)], key=lambda x: x[1])
        large_basins.append(max_len)
        basins = [x for idx, x in enumerate(basins) if idx != max_len[0]]
    return prod([x[1] for x in large_basins])

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 600
    assert solve_p2(inp) == 987840

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 15
    assert solve_p2(inp) == 1134

if __name__ == '__main__':
    main()
