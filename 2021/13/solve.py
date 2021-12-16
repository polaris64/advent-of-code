def read_input(filename):
    with open(filename, "rt") as fh:
        mode = "DOTS"
        dots = set()
        folds = []
        for line in fh.readlines():
            if len(line.strip()) == 0:
                mode = "FOLDS"
                continue

            if mode == "DOTS":
                dots.add(tuple([int(x) for x in line.strip().split(",")]))
            elif mode == "FOLDS":
                instr = line.strip().replace("fold along ", "")
                axis, val = instr.split("=")
                folds.append((axis, int(val)))

        return (dots, folds)

def process_folds(inp):
    dots = inp[0].copy()
    for axis, val in inp[1]:
        idx = 0 if axis == "x" else 1
        r_dots = [d for d in dots if d[idx] > val]
        for d in r_dots:
            dots.remove(d)
            nd = (
                d[0] if idx == 1 else val - (d[0] - val),
                d[1] if idx == 0 else val - (d[1] - val),
            )
            dots.add(nd)
        yield dots

def render_grid(dots):
    res = ""
    minx = min([x for x, _ in dots])
    maxx = max([x for x, _ in dots])
    miny = min([y for _, y in dots])
    maxy = max([y for _, y in dots])
    for y in range(miny, maxy + 1):
        for x in range(minx, maxx + 1):
            res += "#" if (x, y) in dots else " "
        res += "\n"
    return res

def solve_p1(inp):
    return len(next(process_folds(inp)))

def solve_p2(inp):
    return render_grid(list(process_folds(inp))[-1])

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: -\n{}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 850
    assert solve_p2(inp) == ' ##  #  #  ##   ##  ###   ##   ##  #  #\n#  # #  # #  # #  # #  # #  # #  # #  #\n#  # #### #    #    #  # #    #  # #  #\n#### #  # # ## #    ###  # ## #### #  #\n#  # #  # #  # #  # #    #  # #  # #  #\n#  # #  #  ###  ##  #     ### #  #  ## \n'

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 17
    assert solve_p2(inp) == '#####\n#   #\n#   #\n#   #\n#####\n'

if __name__ == '__main__':
    main()
