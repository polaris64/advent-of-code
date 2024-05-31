def read_input(fn):
    with open(fn, "rt") as fh:
        return [[int(x) for x in l.strip().split()] for l in fh.readlines()][0]


def solve_p1(inp):
    cache = set()
    counter = 0
    while True:
        max_val = max(inp)
        max_idx = inp.index(max_val)
        inp[max_idx] = 0
        idx = max_idx + 1
        for i in range(max_val):
            inp[idx % len(inp)] += 1
            idx += 1
        counter += 1
        if tuple(inp) in cache:
            break
        cache.add(tuple(inp))
    return counter


def solve_p2(inp):
    cache = set()
    counter = 0
    target = None
    target_counter = 0
    while True:
        max_val = max(inp)
        max_idx = inp.index(max_val)
        inp[max_idx] = 0
        idx = max_idx + 1
        for i in range(max_val):
            inp[idx % len(inp)] += 1
            idx += 1
        counter += 1
        if target is not None:
            target_counter += 1
        if tuple(inp) == target:
            break
        if target is None and tuple(inp) in cache:
            target = tuple(inp)
        cache.add(tuple(inp))
    return target_counter


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 14029
    assert solve_p2(inp) == 2765


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 5
    assert solve_p2(inp) == 4


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
