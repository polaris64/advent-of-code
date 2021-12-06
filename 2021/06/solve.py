from typing import Counter

def read_input(filename):
    with open(filename, "rt") as fh:
        return [int(x) for x in fh.readline().strip().split(",")]

def run_sim(inp, days):

    # Create Counter to track number of fish with each counter value
    # ([0..8])
    c = Counter()
    for fish in inp:
        c[fish] += 1

    for _ in range(days):
        c0 = c[0]

        # Move counts down by one place
        for fc in range(1, 9):
            c[fc - 1] = c[fc]

        # Spawn fish with counter 8 for all counter 0 fish
        c[8] = c0

        # Move previous c[0] fish to c[6]
        c[6] += c0

    # Return count of all fish
    return sum(c.values())

def solve_p1(inp):
    return run_sim(inp, 80)

def solve_p2(inp):
    return run_sim(inp, 256)

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 358214
    assert solve_p2(inp) == 1622533344325

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 5934
    assert solve_p2(inp) == 26984457539

if __name__ == '__main__':
    main()
