from functools import reduce

def read_input(f):
    with open(f, "rt") as fh:
        return [parse_instr(x.strip()) for x in fh.readlines()]

def parse_instr(line):
    parts = line.split(" ")
    return (parts[0], int(parts[1]))

def run_instructions(inp):
    pos = [0, 0]
    for instr in inp:
        if instr[0] == "forward":
            pos[0] += instr[1]
        elif instr[0] == "up":
            pos[1] -= instr[1]
        elif instr[0] == "down":
            pos[1] += instr[1]
    return pos

def run_instructions_p2(inp):
    pos = [0, 0]
    aim = 0
    for instr in inp:
        if instr[0] == "forward":
            pos[0] += instr[1]
            pos[1] += aim * instr[1]
        elif instr[0] == "up":
            aim -= instr[1]
        elif instr[0] == "down":
            aim += instr[1]
    return pos

def solve_p1(inp):
    return reduce(lambda x, y: x * y, run_instructions(inp))

def solve_p2(inp):
    return reduce(lambda x, y: x * y, run_instructions_p2(inp))

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 150
    assert solve_p2(inp) == 900

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 1636725
    assert solve_p2(inp) == 1872757425

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

if __name__ == '__main__':
    main()
