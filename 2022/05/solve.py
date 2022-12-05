import re


def read_input(filename):
    with open(filename, "r") as fh:
        return parse_input([x.strip("\n") for x in fh.readlines()])


def parse_input(inp):
    stacks = {}

    # Find list of stacks
    for line in inp:
        if line.replace(" ", "").isnumeric():
            stacks = {int(x): [] for x in line.split(" ") if len(x) > 0}
            break

    # Add crates to stacks
    for line in inp:
        if line.replace(" ", "").isnumeric():
            break
        for sid, stack in enumerate(stacks):
            crate = line[4 * sid:(4 * sid) + 4].replace("[", "").replace("]", "").strip()
            if len(crate) > 0:
                stacks[sid + 1].insert(0, crate)

    # Read instructions
    instructions = []
    start = False
    for line in inp:
        if len(line.strip()) == 0:
            start = True
            continue
        if start:
            matches = re.search(r"^move ([0-9]+) from ([0-9]+) to ([0-9]+)$", line.strip())
            assert matches is not None
            groups = matches.groups()
            assert len(groups) == 3
            instructions.append(tuple([int(x) for x in groups]))

    return (stacks, instructions)


def run_instructions(inp, multiple=False):
    for num, from_stack, to_stack in inp[1]:
        if multiple:
            crates = inp[0][from_stack][-num:]
            inp[0][from_stack] = inp[0][from_stack][:-num]
            inp[0][to_stack].extend(crates)
        else:
            for crate_num in range(num):
                crate = inp[0][from_stack].pop()
                inp[0][to_stack].append(crate)


def solve_p1(inp):
    run_instructions(inp, False)
    return "".join([s[-1] for s in inp[0].values()])


def solve_p2(inp):
    run_instructions(inp, True)
    return "".join([s[-1] for s in inp[0].values()])


def main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    print(f"The solution to part 1 is: {res}")
    inp = read_input("input.txt")
    res = solve_p2(inp)
    print(f"The solution to part 2 is: {res}")


def test_ex():
    inp = read_input("input_ex.txt")
    res = solve_p1(inp)
    assert res == "CMZ"
    inp = read_input("input_ex.txt")
    res = solve_p2(inp)
    assert res == "MCD"


def test_main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    assert res == "MQSHJMWNH"
    inp = read_input("input.txt")
    res = solve_p2(inp)
    assert res == "LLWJRBHVZ"


if __name__ == '__main__':
    main()
