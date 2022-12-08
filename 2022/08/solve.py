from math import prod


def read_input(filename):
    with open(filename, "r") as fh:
        return [[int(y) for y in list(x.strip())] for x in fh.readlines()]


def check_visibility(inp, x, y):
    vis = [1, 1, 1, 1]
    for xp in range(x - 1, -1, -1):
        if inp[y][xp] >= inp[y][x]:
            vis[0] = 0
            break
    for xp in range(x + 1, len(inp[y])):
        if inp[y][xp] >= inp[y][x]:
            vis[1] = 0
            break
    for yp in range(y - 1, -1, -1):
        if inp[yp][x] >= inp[y][x]:
            vis[2] = 0
            break
    for yp in range(y + 1, len(inp)):
        if inp[yp][x] >= inp[y][x]:
            vis[3] = 0
            break
    return vis


def find_visible(inp):
    vis = [[0 for x in row] for row in inp]
    for y, row in enumerate(inp):
        for x, tree in enumerate(row):
            if y == 0 or y == len(inp) - 1:
                vis[y][x] = 1
            elif x == 0 or x == len(row) - 1:
                vis[y][x] = 1
            else:
                if any([v == 1 for v in check_visibility(inp, x, y)]):
                    vis[y][x] = 1
    return vis


def get_scenic_score(inp, x, y):
    scores = [0, 0, 0, 0]
    for xp in range(x - 1, -1, -1):
        scores[0] += 1
        if inp[y][xp] >= inp[y][x]:
            break
    for xp in range(x + 1, len(inp[y])):
        scores[1] += 1
        if inp[y][xp] >= inp[y][x]:
            break
    for yp in range(y - 1, -1, -1):
        scores[2] += 1
        if inp[yp][x] >= inp[y][x]:
            break
    for yp in range(y + 1, len(inp)):
        scores[3] += 1
        if inp[yp][x] >= inp[y][x]:
            break
    return prod(scores)


def get_scenic_scores(inp):
    scores = [[0 for x in row] for row in inp]
    for y, row in enumerate(inp):
        for x, tree in enumerate(row):
            scores[y][x] = get_scenic_score(inp, x, y)
    return scores


def solve_p1(inp):
    return sum([sum(row) for row in find_visible(inp)])


def solve_p2(inp):
    return max([max(row) for row in get_scenic_scores(inp)])


def main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    print(f"The solution to part 1 is: {res}")
    res = solve_p2(inp)
    print(f"The solution to part 2 is: {res}")


def test_ex():
    inp = read_input("input_ex.txt")
    res = solve_p1(inp)
    assert res == 21
    res = solve_p2(inp)
    assert res == 8


def test_main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    assert res == 1854
    res = solve_p2(inp)
    assert res == 527340


if __name__ == '__main__':
    main()
