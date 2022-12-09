def read_input(filename):
    with open(filename, "r") as fh:
        return [parse_line(x.strip()) for x in fh.readlines()]


def parse_line(line):
    parts = line.split(" ")
    return (parts[0], int(parts[1]))


def move_element(ctx, idx):
    min_dist = None
    move = (0, 0)
    for y in range(-1, 2, 1):
        for x in range(-1, 2, 1):
            dist = sum([
                abs(v1 - v2)
                for v1, v2
                in zip(
                    (ctx["knots"][idx][0] + x, ctx["knots"][idx][1] + y),
                    ctx["knots"][idx - 1]
                )
            ])
            if min_dist is None or dist < min_dist:
                min_dist = dist
                move = (x, y)
    ctx["knots"][idx] = tuple(
        [v1 + v2 for v1, v2 in zip(ctx["knots"][idx], move)]
    )


def move_head(ctx, dx, dy):
    ctx["t_pos"].add(ctx["knots"][-1])
    for idx, h in enumerate(ctx["knots"]):
        if idx == 0:
            ctx["knots"][idx] = (
                ctx["knots"][idx][0] + dx,
                ctx["knots"][idx][1] + dy
            )
        else:
            dist = [
                abs(v1 - v2)
                for v1, v2
                in zip(ctx["knots"][idx], ctx["knots"][idx - 1])
            ]
            if dist[0] > 1 or dist[1] > 1:
                move_element(ctx, idx)


def simulate(inp, elements):
    ctx = {
        "knots": [(0, 0) for _ in range(elements)],
        "t_pos": set(),
    }
    for direct, places in inp:
        if direct == "U":
            for i in range(places):
                move_head(ctx, 0, -1)
        elif direct == "D":
            for i in range(places):
                move_head(ctx, 0, 1)
        elif direct == "L":
            for i in range(places):
                move_head(ctx, -1, 0)
        elif direct == "R":
            for i in range(places):
                move_head(ctx, 1, 0)
    ctx["t_pos"].add(ctx["knots"][-1])
    return ctx


def solve_p1(inp):
    return len(simulate(inp, 2)["t_pos"])


def solve_p2(inp):
    return len(simulate(inp, 10)["t_pos"])


def main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    print(f"The solution to part 1 is: {res}")
    res = solve_p2(inp)
    print(f"The solution to part 2 is: {res}")


def test_ex():
    inp = read_input("input_ex.txt")
    res = solve_p1(inp)
    assert res == 13
    res = solve_p2(inp)
    assert res == 1
    inp = read_input("input_ex2.txt")
    res = solve_p2(inp)
    assert res == 36


def test_main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    assert res == 6236
    res = solve_p2(inp)
    assert res == 2449


if __name__ == '__main__':
    main()
