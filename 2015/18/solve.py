def read_input(filename):
    world = set()
    with open(filename, "rt") as fhandle:
        for y, row in enumerate(fhandle.readlines()):
            for x, cell in enumerate(row):
                if cell == "#":
                    world.add((x, y))
    return world


def get_min_max_coords(world):
    return (
        (min(x for x, _ in world), min(y for _, y in world)),
        (max(x for x, _ in world), max(y for _, y in world))
    )

    
def count_neighbours(world, x, y, limits=None):
    count = 0
    for xoff in range(-1, 2):
        for yoff in range(-1, 2):
            if xoff == 0 and yoff == 0:
                continue
            if limits is not None:
                if (
                        x + xoff < limits[0][0] or x + xoff > limits[1][0] or
                        y + yoff < limits[0][1] or y + yoff > limits[1][1]
                    ):
                    continue
            if (x + xoff, y + yoff) in world:
                count += 1
    return count


def set_corners(world, limits):
    world.add((limits[0][0], limits[0][1]))
    world.add((limits[1][0], limits[0][1]))
    world.add((limits[1][0], limits[1][1]))
    world.add((limits[0][0], limits[1][1]))


def run_simulation(world, steps=1, limits=None, limits_stuck=False):
    # TODO: Optimise: only process cells in world and immediate neighbours
    min_xy, max_xy = get_min_max_coords(world)
    prev_world = world.copy()
    if limits is not None and limits_stuck:
        set_corners(prev_world, limits)
    new_world = world.copy()
    for _ in range(steps):
        if limits is not None and limits_stuck:
            set_corners(new_world, limits)
        for y in range(min_xy[1] - 1, max_xy[1] + 2):
            for x in range(min_xy[0] - 1, max_xy[0] + 2):
                if limits is not None:
                    if (
                            x < limits[0][0] or x > limits[1][0] or
                            y < limits[0][1] or y > limits[1][1]
                        ):
                        continue
                c = count_neighbours(prev_world, x, y, limits)
                if (x, y) in prev_world and not (c == 2 or c == 3):
                    new_world.remove((x, y))
                elif (x, y) not in prev_world and c == 3:
                    new_world.add((x, y))
        if limits is not None and limits_stuck:
            set_corners(new_world, limits)
        prev_world = new_world.copy()
    return new_world


def render_world(world):
    output = ""
    min_xy, max_xy = get_min_max_coords(world)
    for y in range(min_xy[1], max_xy[1] + 1):
        for x in range(min_xy[0], max_xy[0] + 1):
            output += "#" if (x, y) in world else "."
        output += "\n"
    return output


def solve_p1(world, steps, limits):
    return len(run_simulation(world, steps, limits))


def solve_p2(world, steps, limits):
    return len(run_simulation(world, steps, limits, True))


def main():
    world = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(world, 100, ((0, 0), (99, 99)))))
    print("The solution to part 2 is: {}".format(solve_p2(world, 100, ((0, 0), (99, 99)))))


def test_ex():
    world = read_input("input_ex.txt")
    assert solve_p1(world, 4, ((0, 0), (5, 5))) == 4
    assert solve_p2(world, 5, ((0, 0), (5, 5))) == 17


def test_live():
    world = read_input("input.txt")
    assert solve_p1(world, 100, ((0, 0), (99, 99))) == 814
    assert solve_p2(world, 100, ((0, 0), (99, 99))) == 924


if __name__ == "__main__":
    main()
