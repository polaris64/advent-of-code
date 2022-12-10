def read_input(filename):
    with open(filename, "r") as fh:
        return [parse_line(l.strip()) for l in fh.readlines()]


def parse_line(line):
    parts = line.split(" ")
    if len(parts) > 1:
        parts[1] = int(parts[1])
    return tuple(parts)


def simulate(inp):
    ctx = {
        "x": 1,
        "cycle": 1,
    }
    yield ctx
    for instr in inp:
        if instr[0] == "addx":
            ctx["cycle"] += 1
            yield ctx
            ctx["x"] += instr[1]
            ctx["cycle"] += 1
            yield ctx
        elif instr[0] == "noop":
            ctx["cycle"] += 1
            yield ctx
    return ctx


def simulate_crt(inp):
    fb = [[" " for x in range(40)] for y in range(6)]

    for ctx in simulate(inp):
        cycle = ctx["cycle"] - 1
        if cycle >= 40 * 6:
            break
        xp = cycle % 40
        yp = cycle // 40
        if xp in range(ctx["x"] - 1, ctx["x"] + 2):
            fb[yp][xp] = "#"
        else:
            fb[yp][xp] = "."

    return fb


def render_fb(fb):
    return "\n".join("".join(row) for row in fb)


def solve_p1(inp):
    res = 0
    for ctx in simulate(inp):
        cycle = ctx["cycle"]
        if cycle == 20 or (cycle >= 60 and (cycle - 60) % 40 == 0):
            res += cycle * ctx["x"]
            if cycle >= 220:
                break
    return res


def solve_p2(inp):
    return render_fb(simulate_crt(inp))


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: -\n{solve_p2(inp)}")


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 13140
    expected = ""
    expected += "##..##..##..##..##..##..##..##..##..##..\n"
    expected += "###...###...###...###...###...###...###.\n"
    expected += "####....####....####....####....####....\n"
    expected += "#####.....#####.....#####.....#####.....\n"
    expected += "######......######......######......####\n"
    expected += "#######.......#######.......#######....."
    assert solve_p2(inp) == expected


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 17940
    expected = ""
    expected += "####..##..###...##....##.####...##.####.\n"
    expected += "...#.#..#.#..#.#..#....#.#.......#....#.\n"
    expected += "..#..#....###..#..#....#.###.....#...#..\n"
    expected += ".#...#....#..#.####....#.#.......#..#...\n"
    expected += "#....#..#.#..#.#..#.#..#.#....#..#.#....\n"
    expected += "####..##..###..#..#..##..#.....##..####."
    assert solve_p2(inp) == expected


if __name__ == '__main__':
    main()
