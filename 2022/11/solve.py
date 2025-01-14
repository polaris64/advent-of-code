import numpy as np


def read_input(filename):
    with open(filename, "r") as fh:
        return parse_input([line.strip() for line in fh.readlines()])


def parse_input(lines):
    ctx = {
        "monkey": None,
        "res": {},
    }
    for line in lines:
        if line[:6] == "Monkey":
            parts = line.split(" ")
            mid = int(parts[1].replace(":", ""))
            ctx["monkey"] = mid
            ctx["res"][mid] = {"insp": 0, "act": {True: None, False: None}}
        elif len(line) == 0:
            ctx["monkey"] = None
        elif ctx["monkey"] is not None:
            monkey = ctx["res"][ctx["monkey"]]
            if line[:8] == "Starting":
                # e.g. "Starting items: 54, 65, 75, 74"
                line = line.replace("Starting items: ", "")
                monkey["items"] = [int(x) for x in line.split(", ")]
            elif line[:9] == "Operation":
                # e.g "Operation: new = old + 6"
                line = line.replace("Operation: ", "")
                parts = line.split(" ")
                monkey["op"] = (parts[2], parts[3], parts[4])
            elif line[:4] == "Test":
                # e.g "Test: divisible by 19"
                line = line.replace("Test: ", "")
                parts = line.split(" ")
                monkey["test"] = (parts[0] + parts[1], int(parts[2]))
            elif line[:2] == "If":
                # e.g "If true: throw to monkey 2"
                # e.g "If false: throw to monkey 0"
                line = line.replace("If ", "")
                parts = line.split(" ")
                if parts[0] == "true:":
                    monkey["act"][True] = int(parts[4])
                elif parts[0] == "false:":
                    monkey["act"][False] = int(parts[4])
    return ctx["res"]


def run_round(inp, do_divide=True, lcm=None):
    for mid, monkey in inp.items():
        items_copy = monkey["items"].copy()
        for item_id, item in enumerate(items_copy):
            op = monkey["op"]
            a = item if op[0] == "old" else int(op[0])
            b = item if op[2] == "old" else int(op[2])
            if op[1] == "+":
                items_copy[item_id] = a + b
            elif op[1] == "*":
                items_copy[item_id] = a * b

            if do_divide:
                items_copy[item_id] //= 3
            elif lcm is not None:
                if items_copy[item_id] > lcm:
                    items_copy[item_id] %= lcm

            monkey["insp"] += 1

            test = monkey["test"]
            act = monkey["act"]
            if test[0] == "divisibleby":
                if items_copy[item_id] % test[1] == 0:
                    inp[act[True]]["items"].append(items_copy[item_id])
                else:
                    inp[act[False]]["items"].append(items_copy[item_id])
                monkey["items"] = monkey["items"][1:]
    return inp


def play(inp, rounds, do_divide=True, lcm=None):
    for r in range(rounds):
        run_round(inp, do_divide, lcm)
    return inp


def find_lcm(inp):
    return np.lcm.reduce([x["test"][1] for x in inp.values()])


def solve_p1(inp):
    res = play(inp, 20, True)
    monkeys = sorted(res.values(), key=lambda m: m["insp"])[-2:]
    return monkeys[0]["insp"] * monkeys[1]["insp"]


def solve_p2(inp):
    lcm = find_lcm(inp)
    res = play(inp, 10000, False, lcm)
    monkeys = sorted(res.values(), key=lambda m: m["insp"])[-2:]
    return monkeys[0]["insp"] * monkeys[1]["insp"]


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: -\n{solve_p2(inp)}")


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 10605
    inp = read_input("input_ex.txt")
    assert solve_p2(inp) == 2713310158


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 117624
    inp = read_input("input.txt")
    assert solve_p2(inp) == 16792940265


if __name__ == '__main__':
    main()
