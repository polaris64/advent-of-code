from functools import reduce


NUMBERS = [str(x) for x in list(range(10))]


def read_input(fn):
    with open(fn, "rt") as fh:
        return [list(l.strip()) for l in fh.readlines()]


def get_full_number(inp, xp, yp):
    xstart = xp
    xend = xp
    for x in range(xp, -1, -1):
        if inp[yp][x] not in NUMBERS:
            break
        xstart = x
    for x in range(xp, len(inp[yp]), 1):
        if inp[yp][x] not in NUMBERS:
            break
        xend = x
    return int("".join(inp[yp][xstart:xend + 1]))


def get_adjacent_numbers(inp, xp, yp):
    for y in [-1, 0, 1]:
        for x in [-1, 0, 1]:
            if x == 0 and y == 0:
                continue
            if xp + x >= 0 and xp + x < len(inp[0]) and yp + y >= 0 and yp + y < len(inp):
                if inp[yp + y][xp + x] in NUMBERS:
                    yield get_full_number(inp, xp + x, yp + y)
                
    
def get_engine_part_numbers(inp):
    for y, row in enumerate(inp):
        for x, ch in enumerate(row):
            if ch not in NUMBERS and ch != ".":
                yield ch, set(get_adjacent_numbers(inp, x, y))


def solve_p1(inp):
    return sum([sum(x[1]) for x in get_engine_part_numbers(inp)])


def solve_p2(inp):
    return sum(
        [
            reduce(lambda a, b: a * b, x[1])
            for x in get_engine_part_numbers(inp)
            if x[0] == "*" and len(x[1]) == 2
        ]
    )


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 539590
    assert solve_p2(inp) == 80703636


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 4361
    assert solve_p2(inp) == 467835


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
