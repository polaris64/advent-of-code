from functools import cache

def read_input(filename):
    with open(filename, "rt") as fh:
        return [int(x) for x in fh.readline().strip().split(",")]

def calc_fuel(inp, pos):
    return sum([abs(x - pos) for x in inp])

def calc_fuel_p2(inp, pos):
    return sum([get_cost_p2(abs(x - pos)) for x in inp])

@cache
def get_cost_p2(dist):
    # TODO: Implement better algorithm than brute-force
    return sum(range(1, dist + 1))

def find_cheapest_move(inp, fn):
    costs = dict()
    for x in range(min(inp), max(inp) + 1):
        costs[x] = fn(inp, x)
    return min(costs.items(), key=lambda x: x[1])

def solve_p1(inp):
    return find_cheapest_move(inp, calc_fuel)[1]

def solve_p2(inp):
    return find_cheapest_move(inp, calc_fuel_p2)[1]

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 340052
    assert solve_p2(inp) == 92948968

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 37
    assert solve_p2(inp) == 168

if __name__ == '__main__':
    main()
