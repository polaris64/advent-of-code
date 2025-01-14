from functools import reduce
import math


def read_input(fn):
    with open(fn, "rt") as fh:
        lines = [l.strip() for l in fh.readlines()]
        return {
            "t": [int(x) for x in lines[0].replace("Time:", "").split()],
            "d": [int(x) for x in lines[1].replace("Distance:", "").split()],
        }



def get_results(t, d):
    for tv in range(t):
        dv = tv * (t - tv)
        yield d, dv

    
def get_all_results(inp):
    for t, d in zip(inp["t"], inp["d"]):
        yield list(get_results(t, d))


def solve_p1(inp):
    return reduce(lambda a, b: a * b, [len([x for x in run if x[1] > x[0]]) for run in list(get_all_results(inp))])


def solve_p2(inp):
    t = int("".join([str(x) for x in inp["t"]]))
    d = int("".join([str(x) for x in inp["d"]]))

    # Rather than trying all time values, we need to solve x * (t - x)
    # > d for values of x. Rearranging gives the quadratic formula:
    # -x² + xt -d > 0 where t is a constant.

    # e.g.
    #       x * (71530 - x) > 940200
    #          -x² + 71530x > 940200
    # -x² + 71530x - 940200 > 0
    # Using the quadratic formula: x = (-b ± sqrt(b² - 4ac)) / 2a...
    # x = (-71530 ± sqrt(71530² - (4*1*-940200))) / (2*-1)
    # x = (-71530 ± sqrt(5116540900 - 3760800)) / -2
    # either x = (-71530 + 71503.7068969) / -2 =    13.147
    # or     x = (-71530 - 71503.7068969) / -2 = 71516.853

    res = (
        (t + math.sqrt((t * t) - (-4 * -d))) / -2,
        (t - math.sqrt((t * t) - (-4 * -d))) / -2,
    )
    return math.floor(res[1]) - math.ceil(res[0]) + 1


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 840336
    assert solve_p2(inp) == 41382569


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 288
    assert solve_p2(inp) == 71503


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
