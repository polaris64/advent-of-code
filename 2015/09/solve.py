from itertools import permutations


def read_input(filename):
    with open(filename, "rt") as fhandle:
        return {k: v for k, v in [parse(x) for x in fhandle.readlines()]}


def parse(line):
    parts = line.split(" = ")
    return (tuple(parts[0].split(" to ")), int(parts[1]))


def get_destinations(inp):
    return set(x for rec in inp.keys() for x in rec)


def get_routes(destinations):
    # Only yield routes that have not already been yielded in reverse
    # order, as the distances of these will be identical. This cuts
    # the number of routes to check in half at the expense of
    # additional memory usage and processing time in this generator.
    # i.e. dist(a->b->c) == dist(c->b->a)
    generated = dict()
    for route in permutations(destinations):
        if route[::-1] not in generated:
            yield route
            generated[route] = 1


def get_distance(inp, step):
    if step in inp:
        return inp[step]
    return inp[step[::-1]]


def get_route_distance(inp, route):
    return sum([get_distance(inp, step) for step in zip(route, route[1:])])


def solve_p1(inp):
    routes = get_routes(get_destinations(inp))
    return min([get_route_distance(inp, x) for x in routes])


def solve_p2(inp):
    routes = get_routes(get_destinations(inp))
    return max([get_route_distance(inp, x) for x in routes])


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 117
    assert solve_p2(inp) == 909


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 605
    assert solve_p2(inp) == 982


if __name__ == "__main__":
    main()
