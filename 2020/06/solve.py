from functools import reduce


def read_input(fname):
    with open(fname, "r") as handle:
        return [x.strip().split("\n") for x in handle.read().split("\n\n")]


def get_group_set(grp):
    # Get list of answers for each person in group
    people = [list(x) for x in grp]

    # Flatten the list of everyone's answers and create a set
    return set([answer for person in people for answer in person])


def get_group_intersection(grp):
    # Create a set for each person's answers
    people = [set(x) for x in grp]

    # Fold each person's answer set with the previous to produce a set
    # of answers given by each person
    return reduce(
        lambda s, x: s.intersection(x),
        people[1:],
        set(people[0])
    )


def solve_p1(inp):
    # Return the sum of the number of unique answers for each group
    res = [get_group_set(x) for x in inp]
    return sum([len(x) for x in res])


def solve_p2(inp):
    # Return the sum of the number of answers given by each person of
    # a group
    return sum([len(get_group_intersection(x)) for x in inp])


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 6291
    assert solve_p2(inp) == 3052


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 11
    assert solve_p2(inp) == 6


if __name__ == "__main__":
    main()
