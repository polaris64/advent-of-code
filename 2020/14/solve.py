import re


def read_input(fname):
    with open(fname, "r") as handle:
        return [parse_line(x.strip()) for x in handle.readlines()]


def parse_line(line):
    if line[:4] == "mask":
        return {
            "cmd": "mask",
            "arg": re.match(r"mask = (.*)", line).groups()[0]
        }
    if line[:3] == "mem":
        groups = re.match(r"mem\[([0-9]+)\] = ([0-9]+)", line).groups()
        return {
            "cmd": "mem",
            "addr": int(groups[0]),
            "arg": int(groups[1]),
        }
    return None


def calc_val(mask, val):
    mask_and = int("".join(["0" if x == "0" else "1" for x in list(mask)]), 2)
    mask_or = int("".join(["1" if x == "1" else "0" for x in list(mask)]), 2)
    return val & mask_and | mask_or


def get_addresses(base_addr):
    float_pos = [x[0] for x in enumerate(base_addr) if x[1] == "X"]
    combs = int("1" * len(float_pos), 2) + 1
    for i in range(combs):
        base_addr_l = list(base_addr)
        for idx, pos in enumerate(float_pos):
            base_addr_l[pos] = str(((1 << (idx)) & i) >> (idx))
        yield "".join(base_addr_l)


def write_memory(ctx, addr, arg):
    mask = list(ctx["mask"])
    addr2 = addr | int("".join(["1" if x == "1" else "0" for x in mask]), 2)

    addr2_bin = bin(addr2)[2:]
    if len(addr2_bin) < 36:
        addr2_bin = "0" * (36 - len(addr2_bin)) + addr2_bin

    addr2_bin = "".join(["X" if m == "X" else a for (m, a) in zip(mask, addr2_bin)])

    for a_bin in get_addresses(addr2_bin):
        addr2 = int(a_bin, 2)
        ctx["mem"][addr2] = arg


def run_program(prog, version=1):
    ctx = {
        "ip": 0,
        "mask": "X" * 36,
        "mem": dict(),
    }
    for instr in prog:
        if instr["cmd"] == "mask":
            ctx["mask"] = instr["arg"]
        elif instr["cmd"] == "mem":
            if version == 1:
                ctx["mem"][instr["addr"]] = calc_val(ctx["mask"], instr["arg"])
            else:
                write_memory(ctx, instr["addr"], instr["arg"])

    return ctx


def solve_p1(inp):
    ctx = run_program(inp)
    return sum(ctx["mem"].values())


def solve_p2(inp):
    ctx = run_program(inp, version=2)
    return sum(ctx["mem"].values())


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 8566770985168
    assert solve_p2(inp) == 4832039794082


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 165
    inp = read_input("input_ex2.txt")
    assert solve_p2(inp) == 208


if __name__ == "__main__":
    main()
