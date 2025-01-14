TESTS = [
    ("<>", 0, 0),
    ("<random characters>", 0, 17),
    ("<<<<>", 0, 3),
    ("<{!>}>", 0, 2),
    ("<!!>", 0, 0),
    ("<!!!>>", 0, 0),
    ("<{o\"i!a,<{i<a>", 0, 10),

    ("{}", 1, 0),
    ("{{{}}}", 6, 0),
    ("{{},{}}", 5, 0),
    ("{{{},{},{{}}}}", 16, 0),
    ("{<{},{},{{}}>}", 1, 10),
    ("{<a>,<a>,<a>,<a>}", 1, 4),
    ("{{<a>},{<a>},{<a>},{<a>}}", 9, 4),
    ("{{<!>},{<!>},{<!>},{<a>}}", 3, 13),
]


def read_input(fn):
    with open(fn, "rt") as fh:
        return [l.strip() for l in fh.readlines()][0]


def filter_stream(s):
    state = {
        "garbage": False,
        "escape": False,
        "garbage_chars": 0,
    }
    res = []
    for ch in s:
        if not state["escape"] and ch == "!":
            state["escape"] = True
            continue
        if state["escape"]:
            state["escape"] = False
            continue
        if not state["escape"] and ch == "<":
            if state["garbage"]:
                state["garbage_chars"] += 1
            state["garbage"] = True
            continue
        if state["garbage"] and not state["escape"] and ch == ">":
            state["garbage"] = False
            continue
        if state["garbage"]:
            state["garbage_chars"] += 1
        if not state["garbage"] and ch != ",":
            res.append(ch)

    return ("".join(res), state)


def generate_groups(s):
   depth = 0
   for ch in s:
        if ch == "{":
            depth += 1
            yield depth
        elif ch == "}":
            depth -= 1


def solve_p1(inp):
    return sum(generate_groups(filter_stream(inp)[0]))


def solve_p2(inp):
    return filter_stream(inp)[1]["garbage_chars"]


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 16689
    assert solve_p2(inp) == 7982


def test_ex():
    for (stream, p1, p2) in TESTS:
        assert solve_p1(stream) == p1
        assert solve_p2(stream) == p2


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
