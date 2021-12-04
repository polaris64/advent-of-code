def read_input(filename):
    with open(filename, "rt") as fhandle:
        return list(fhandle.read())


def traverse_grid(rules, actor_count=1):
    grid = set([(0, 0)])
    actors = [(0, 0) for x in range(actor_count)]
    curr_actor = 0
    for rule in rules:
        xpos, ypos = actors[curr_actor]
        if rule == "^":
            actors[curr_actor] = (xpos, ypos - 1)
        elif rule == "v":
            actors[curr_actor] = (xpos, ypos + 1)
        elif rule == "<":
            actors[curr_actor] = (xpos - 1, ypos)
        elif rule == ">":
            actors[curr_actor] = (xpos + 1, ypos)
        grid.add(actors[curr_actor])
        curr_actor = (curr_actor + 1) % actor_count
    return grid


def solve_p1(inp):
    grid = traverse_grid(inp)
    return len(grid)


def solve_p2(inp):
    grid = traverse_grid(inp, 2)
    return len(grid)


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 2081
    assert solve_p2(inp) == 2341


if __name__ == "__main__":
    main()
