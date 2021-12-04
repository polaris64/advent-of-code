def get_coords():
    with open("input") as f:
        return [tuple([int(x.strip()) for x in x.split(",")]) for x in f.read().split("\n") if len(x) > 0]

def manhattan_dist(sx, sy, ex, ey):
    return abs(sx - ex) + abs(sy - ey)

def fill_grid(coords):
    max_x = max(coords, key=lambda x: x[0])[0]
    max_y = max(coords, key=lambda x: x[1])[1]

    grid = [[dict() for x in range(max_x)] for y in range(max_y)]

    for (idx, (x, y)) in enumerate(coords):
        for gy in range(len(grid)):
            for gx in range(len(grid[gy])):
                grid[gy][gx][idx] = manhattan_dist(x, y, gx, gy)

    return grid

def get_grid_ids(grid):
    res = grid
    for gy in range(len(grid)):
        for gx in range(len(grid[gy])):
            min_dist = min(grid[gy][gx].values())
            if list(grid[gy][gx].values()).count(min_dist) > 1:
                res[gy][gx] = None
            else:
                res[gy][gx] = [idx for (idx, x) in grid[gy][gx].items() if x == min_dist][0]
    return res

def get_infinites(grid):
    borders = set()
    for x in grid[0]:
        if x is not None:
            borders.add(x)
    for x in grid[-1]:
        if x is not None:
            borders.add(x)
    for y in grid:
        if y[0] is not None:
            borders.add(y[0])
        if y[-1] is not None:
            borders.add(y[-1])

    return borders

def count_idx(grid, idx):
    count = 0
    for gy in range(len(grid)):
        for gx in range(len(grid[gy])):
            if grid[gy][gx] == idx:
                count += 1
    return count

def get_biggest_region_idx(coords, grid, infinites):
    res = list()
    for (idx, (x, y)) in enumerate(coords):
        if idx in infinites:
            continue
        res.append((idx, count_idx(grid, idx)))
    return max(res, key=lambda x: x[1])

def get_grid_max_dist_region_size(coords, max_dist):
    max_x = max(coords, key=lambda x: x[0])[0]
    max_y = max(coords, key=lambda x: x[1])[1]
    grid = [[0 for x in range(max_x)] for y in range(max_y)]

    for gy in range(len(grid)):
        for gx in range(len(grid[gy])):
            for (x, y) in coords:
                grid[gy][gx] += manhattan_dist(x, y, gx, gy)

    count = 0
    for gy in range(len(grid)):
        for gx in range(len(grid[gy])):
            if grid[gy][gx] < max_dist:
                count += 1
    return count

def run():
    coords = get_coords()

    # Get a grid with each item marked with the co-ordinate ID and the
    # Manhattan distance to each cell within the grid
    grid = fill_grid(coords)

    # Conver each item in the grid to the ID of the co-ordinate that is closest
    grid_ids = get_grid_ids(grid)

    # Get a set of all co-ordinate IDs for co-ordinate regions that are
    # infinitely large (i.e. are against the border)
    infinite_items = get_infinites(grid_ids)

    print(
        "The size of the largest area that is not infinite is: {}".format(
            get_biggest_region_idx(coords, grid_ids, infinite_items)[1]
        )
    )

    print(
        "The size of the region which is at least 10,000 units close to any co-ordinate is: {}".format(
            get_grid_max_dist_region_size(coords, 10000)
        )
    )

def run_test():
    coords = [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
    grid = fill_grid(coords)
    grid_ids = get_grid_ids(grid)
    print(grid_ids)
    infinite_items = get_infinites(grid_ids)
    print(
        "The size of the largest area that is not infinite is: {}".format(
            get_biggest_region_idx(coords, grid_ids, infinite_items)
        )
    )
    print(
        "The size of the region which is at least 32 units close to any co-ordinate is: {}".format(
            get_grid_max_dist_region_size(coords, 32)
        )
    )

run()
#run_test()
