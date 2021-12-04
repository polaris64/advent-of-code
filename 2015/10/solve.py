INPUT = "1321131112"


def process_step(inp: str) -> str:
    res = ""
    if len(inp) == 0:
        return res
    if len(inp) == 1:
        return "1" + inp
    
    idx = 0
    while idx < len(inp):
        counter = 0
        ch = inp[idx]
        while idx < len(inp) and inp[idx] == ch:
            counter += 1
            idx += 1
        res += str(counter) + ch

    return res


def run_steps(inp: str, n: int) -> str:
    last = inp
    for _ in range(n):
        last = process_step(last)
    return last
    

def solve_p1(inp: str) -> str:
    return run_steps(inp, 40)


def solve_p2(sln1: str) -> str:
    return run_steps(sln1, 10)


def main() -> None:
    sln1 = solve_p1(INPUT)
    print("The solution to part 1 is: {}".format(len(sln1)))
    sln2 = solve_p2(sln1)
    print("The solution to part 2 is: {}".format(len(sln2)))


def test_main() -> None:
    sln1 = solve_p1(INPUT)
    assert len(sln1) == 492982
    sln2 = solve_p2(sln1)
    assert len(sln2) == 6989950


if __name__ == "__main__":
    main()
