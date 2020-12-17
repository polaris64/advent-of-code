import itertools


def read_input(fname):
    with open(fname, "r") as handle:
        return [x.strip() for x in handle.readlines()]


def build_world(inp, dims):
    world = set()
    for ypos, _ in enumerate(inp):
        for xpos, _ in enumerate(inp[ypos]):
            if inp[ypos][xpos] == "#":
                world.add(tuple([xpos, ypos] + ([0] * (dims - 2))))
    return world


def get_neighbours(cell):
    for coord in itertools.product(*[[axis - 1, axis, axis + 1] for axis in cell]):
        if coord != cell:
            yield coord


def run_step(world):
    new = set()
    for cell in world:
        # Inactive cells become active if 3 neighbours are active
        new.update([
            neighbour for neighbour in
            [x for x in get_neighbours(cell) if x not in world]
            if len(list(
                itertools.islice([x for x in get_neighbours(neighbour) if x in world], 4)
            )) == 3
        ])

        # Active cells remain active if they have 2 or 3 active
        # neighbours
        num_active = len(list(itertools.islice([x for x in get_neighbours(cell) if x in world], 4)))
        if 2 <= num_active <= 3:
            new.add(cell)

    return new


def run_simulation(inp, dims):
    world = build_world(inp, dims)
    yield world
    while True:
        world = run_step(world)
        yield world
    return world


def render_world(world, z_slice):
    coords = {x for x in world if x[2] == z_slice}
    min_x = min(coords, key=lambda x: x[0])[0]
    max_x = max(coords, key=lambda x: x[0])[0]
    min_y = min(coords, key=lambda x: x[1])[1]
    max_y = max(coords, key=lambda x: x[1])[1]
    print()
    print("z={}".format(z_slice))
    for ypos in range(min_y, max_y + 1):
        for xpos in range(min_x, max_x + 1):
            if (xpos, ypos, z_slice) in coords:
                print("#", end="")
            else:
                print(".", end="")
        print()
    print("-" * (max_x - min_x + 1))


def solve_p1(inp):
    return len(list(
        itertools.islice(run_simulation(inp, 3), 6, 7)
    )[0])


def solve_p2(inp):
    return len(list(
        itertools.islice(run_simulation(inp, 4), 6, 7)
    )[0])


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 207
    assert solve_p2(inp) == 2308


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 112
    assert solve_p2(inp) == 848


if __name__ == "__main__":
    main()
