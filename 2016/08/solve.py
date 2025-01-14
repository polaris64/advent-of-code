import re


def read_input(filename):
    with open(filename, "r") as fh:
        return [parse(x.strip()) for x in fh.readlines()]


def parse(line):
    m = re.search(r"rect ([0-9]+)x([0-9]+)", line)
    if m:
        return {
            "cmd": "rect",
            "w": int(m.groups()[0]),
            "h": int(m.groups()[1]),
        }
    m = re.search(r"rotate (row|column) (x|y)=([0-9]+) by ([0-9]+)", line)
    if m:
        return {
            "cmd": "rot",
            "axis": m.groups()[0],
            "coord": int(m.groups()[2]),
            "amt": int(m.groups()[3]),
        }


def process(inp, w, h):
    grid = [[0 for x in range(w)] for y in range(h)]
    for op in inp:
        if op["cmd"] == "rect":
            for y in range(op["h"]):
                for x in range(op["w"]):
                    grid[y][x] = 1
        elif op["cmd"] == "rot":
            if op["axis"] == "row":
                grid[op["coord"]] = grid[op["coord"]][-op["amt"]:] + grid[op["coord"]][:len(grid[op["coord"]]) - op["amt"]]
            elif op["axis"] == "column":
                new_col = [grid[y][op["coord"]] for y in range(len(grid))]
                for y2 in range(len(grid)):
                    new_col[(y2 + op["amt"]) % len(grid)] = grid[y2][op["coord"]]
                for idx, v in enumerate(new_col):
                    grid[idx][op["coord"]] = v
                    
        # yield grid
    return grid


def draw_grid(grid, w, h):
    res = ""

    for y in range(h):
        for x in range(w):
            if grid[y][x] == 1:
                res += "#"
            else:
                res += "."
        res += "\n"

    return res


def solve_p1(inp, w, h):
    grid = process(inp, w, h)
    return len([v for v in [grid[v // w][v % w] for v in range(w * h)] if v == 1])


def solve_p2(inp, w, h):
    return draw_grid(process(inp, w, h), w, h)


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp, 50, 6)))
    print("The solution to part 2 is: -\n{}".format(solve_p2(inp, 50, 6)))


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp, 7, 3) == 6


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp, 50, 6) == 106

    res = """.##..####.#....####.#.....##..#...#####..##...###.
#..#.#....#....#....#....#..#.#...##....#..#.#....
#....###..#....###..#....#..#..#.#.###..#....#....
#....#....#....#....#....#..#...#..#....#.....##..
#..#.#....#....#....#....#..#...#..#....#..#....#.
.##..#....####.####.####..##....#..#.....##..###..
"""
    assert solve_p2(inp, 50, 6) == res


if __name__ == '__main__':
    main()
