import re


def read_input(fname):
    with open(fname, "r") as handle:
        return [parse_instr(x.strip()) for x in handle.readlines()]


def parse_instr(line):
    match = re.match(r"^([a-z]+) ([+-][0-9]+)$", line)
    if not match:
        raise "ParseException"
    groups = match.groups()
    if len(groups) != 2:
        raise "ParseException"
    return {"opcode": groups[0], "operand": int(groups[1]), "hits": 0}


def create_context(prog):
    return {"acc": 0, "pc": 0, "mem": [x.copy() for x in prog]}


def run_program(ctx):
    while True:
        if ctx["pc"] < 0 or ctx["pc" ]>= len(ctx["mem"]):
            return
        instr = ctx["mem"][ctx["pc"]]
        ctx["mem"][ctx["pc"]]["hits"] += 1
        if instr["opcode"] == "acc":
            ctx["acc"] += instr["operand"]
            ctx["pc"] += 1
        elif instr["opcode"] == "jmp":
            ctx["pc"] += instr["operand"]
        else:
            ctx["pc"] += 1
        yield ctx


def flip_opcode(prog, opcode):
    prog[opcode]["opcode"] = "jmp" if prog[opcode] == "nop" else "nop"


def mutate_prog(prog, target_opcode):
    while target_opcode < len(prog):
        opcode = prog[target_opcode]["opcode"]
        if opcode == "nop" or opcode == "jmp":
            flip_opcode(prog, target_opcode)
            return target_opcode + 1
        target_opcode += 1
    return None


def mutate_and_test(initial_prog):
    target_opcode = 0
    while True:
        prog = [x.copy() for x in initial_prog]
        target_opcode = mutate_prog(prog, target_opcode)
        if target_opcode is None:
            return None
        for ctx in run_program(create_context(prog)):
            if ctx["pc"] >= len(ctx["mem"]):
                return ctx["acc"]
            if any([x["hits"] > 1 for x in ctx["mem"]]):
                break


def solve_p1(inp):
    acc = 0
    for ctx in run_program(create_context(inp)):
        if any([x["hits"] > 1 for x in ctx["mem"]]):
            return acc
        acc = ctx["acc"]


def solve_p2(inp):
    return mutate_and_test(inp)


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    pass
    inp = read_input("input.txt")
    assert solve_p1(inp) == 1262
    assert solve_p2(inp) == 1643


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 5
    assert solve_p2(inp) == 8


if __name__ == "__main__":
    main()
