from itertools import permutations


def read_input(filename):
    with open(filename, "rt") as fhandle:
        return [parse(x.strip()) for x in fhandle.readlines()]


def parse(line):
    parts = line.split(" ")
    person = parts[0]
    amount = int(parts[3])
    if parts[2] == "lose":
        amount = -amount
    target = parts[-1].replace(".", "")
    return ((person, target), amount)


def get_people(inp):
    return set(x[0][0] for x in inp)


def get_happiness_maps(inp):
    return {x[0]: x[1] for x in inp}


def get_max_happiness(inp):
    mapping = get_happiness_maps(inp)
    cache = dict()
    def get_happiness_change(arr):
        if arr in cache:
            return cache[arr]
        arr_rev = arr[::-1]
        if arr_rev in cache:
            return cache[arr_rev]
        res = 0
        for couple in zip(arr, arr[1:]):
            res += mapping[couple]
            res += mapping[couple[::-1]]
        res += mapping[(arr[-1], arr[0])]
        res += mapping[(arr[0], arr[-1])]
        cache[arr] = res
        return res

    people = get_people(inp)
    res = [(arr, get_happiness_change(arr)) for arr in permutations(people)]
    return max(res, key=lambda x: x[1])


def solve_p1(inp):
    return get_max_happiness(inp)[1]


def solve_p2(inp):
    people = get_people(inp)
    for person in people:
        inp.append((("You", person), 0))
        inp.append(((person, "You"), 0))
    return get_max_happiness(inp)[1]


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 330


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 733
    assert solve_p2(inp) == 725


if __name__ == "__main__":
    main()
