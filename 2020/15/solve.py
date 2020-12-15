def read_input(fname):
    with open(fname, "r") as handle:
        return [int(x.strip()) for x in handle.readline().split(",")]


def run_game(inp, rounds):
    state = {
        "last_idx": dict(zip(inp[:-1], range(len(inp) - 1))),
        "last_num": inp[-1],
        "len": len(inp),
    }

    counter = len(inp)
    while counter < rounds:
        last_num = state["last_num"]
        if last_num in state["last_idx"]:
            prev_idx = state["last_idx"][last_num]
            new_num = state["len"] - prev_idx - 1
        else:
            new_num = 0
        state["last_idx"][last_num] = state["len"] - 1
        state["len"] += 1
        state["last_num"] = new_num
        counter += 1

    return state


def solve_p1(inp):
    return run_game(inp, 2020)["last_num"]


def solve_p2(inp):
    return run_game(inp, 30000000)["last_num"]


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 639
    assert solve_p2(inp) == 266


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 436


if __name__ == "__main__":
    main()
