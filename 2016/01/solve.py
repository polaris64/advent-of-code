def read_input(fn):
    with open(fn, "rt") as fh:
        return fh.read().strip()

def parse(inp):
    return [
        (x.strip()[0], int(x.strip()[1:]))
        for x in inp.split(",")
    ]

def change_offset(offset, direction, dist):
    if direction == 0:
        return (offset[0], offset[1] + dist)
    elif direction == 1:
        return (offset[0] + dist, offset[1])
    elif direction == 2:
        return (offset[0], offset[1] - dist)
    else:
        return (offset[0] - dist, offset[1])
    
def walk(steps, jump):
    direction = 0
    offset = (0, 0)
    for step in steps:
        direction = (direction + (1 if step[0] == "R" else -1)) % 4
        if jump:
            offset = change_offset(offset, direction, step[1])
            yield offset
        else:
            for _ in range(step[1]):
                offset = change_offset(offset, direction, 1)
                yield offset

def solve_p1(steps):
    offset = None
    for v in walk(steps, True):
        offset = v
    return sum([abs(v) for v in offset])

def solve_p2(steps):
    cells = set()
    offset = None
    for v in walk(steps, False):
        if v in cells:
            offset = v
            break
        cells.add(v)
    return sum([abs(v) for v in offset])

def main():
    steps = parse(read_input("input.txt"))
    print("The solution to part 1 is: {}".format(solve_p1(steps)))
    print("The solution to part 1 is: {}".format(solve_p2(steps)))

def test_ex():
    assert parse("R1, L23") == [("R", 1), ("L", 23)]
    assert solve_p1(parse("R2, L3")) == 5
    assert solve_p1(parse("R2, R2, R2")) == 2
    assert solve_p1(parse("R5, L5, R5, R3")) == 12
    assert solve_p2(parse("R8, R4, R4, R8")) == 4

def test_live():
    steps = parse(read_input("input.txt"))
    assert solve_p1(steps) == 291
    assert solve_p2(steps) == 159
    
if __name__ == '__main__':
    main()
