from collections import defaultdict
import re


def read_input(fn):
    with open(fn, "rt") as fh:
        return [parse_line(l.strip()) for l in fh.readlines()]


def parse_line(line):
    match = re.search(r"^([^ ]+) (inc|dec) ([^ ]+) if ([^ ]+) ([^ ]+) (.+)$", line)
    if not match:
        return None
    groups = match.groups()
    return {
        "reg": groups[0],
        "op": groups[1],
        "operand": int(groups[2]),
        "cmp_reg": groups[3],
        "cmp_op": groups[4],
        "cmp_operand": int(groups[5]),
    }


def run_program(inp):
    registers = defaultdict(int)
    max_val = 0
    for instr in inp:
        if instr["reg"] not in registers:
            registers[instr["reg"]] = 0

        def _update():
            nonlocal max_val
            if instr["op"] == "inc":
                registers[instr["reg"]] += instr["operand"]
            else:
                registers[instr["reg"]] -= instr["operand"]
            if registers[instr["reg"]] > max_val:
                max_val = registers[instr["reg"]]

        cmp_reg_val = registers[instr["cmp_reg"]]

        if instr["cmp_op"] == ">":
            if cmp_reg_val > instr["cmp_operand"]:
                _update()
        elif instr["cmp_op"] == "<":
            if cmp_reg_val < instr["cmp_operand"]:
                _update()
        elif instr["cmp_op"] == ">=":
            if cmp_reg_val >= instr["cmp_operand"]:
                _update()
        elif instr["cmp_op"] == "<=":
            if cmp_reg_val <= instr["cmp_operand"]:
                _update()
        elif instr["cmp_op"] == "==":
            if cmp_reg_val == instr["cmp_operand"]:
                _update()
        elif instr["cmp_op"] == "!=":
            if cmp_reg_val != instr["cmp_operand"]:
                _update()

    return registers, max_val

    
def solve_p1(inp):
    return max(run_program(inp)[0].values())


def solve_p2(inp):
    return run_program(inp)[1]


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 6611
    assert solve_p2(inp) == 6619


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 1
    assert solve_p2(inp) == 10


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
