def read_input(filename):
    with open(filename, "rt") as fh:
        return [x.strip() for x in fh.readlines()]

def get_matching(ch):
    openers = list("[({<")
    closers = list("])}>")
    return openers[closers.index(ch)] if ch in closers else closers[openers.index(ch)]

def parse_line(line):
    stack = []
    for ch in line:
        if ch in "[({<":
            stack.append(ch)
        else:
            if stack[-1] == get_matching(ch):
                stack = stack[:-1]
            else:
                return ("COR", ch)
    if len(stack) == 0:
        return ("OK", [])
    else:
        return ("INC", [get_matching(x) for x in stack[::-1]])

def get_autocomplete_score(stack):
    mapping = {")": 1, "]": 2, "}": 3, ">": 4}
    score = 0
    for ch in stack[1]:
        score *= 5
        score += mapping[ch]
    return score

def solve_p1(inp):
    parses = [parse_line(x) for x in inp]
    corr = [x for x in parses if x[0] == "COR"]
    mapping = {
        ")": 3,
        "]": 57,
        "}": 1197,
        ">": 25137,
    }
    return sum([mapping[x[1]] for x in corr])

def solve_p2(inp):
    parses = [parse_line(x) for x in inp]
    inc = [x for x in parses if x[0] == "INC"]
    scores = sorted([get_autocomplete_score(x) for x in inc])
    return scores[len(scores) // 2]

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 436497
    assert solve_p2(inp) == 2377613374

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 26397
    assert solve_p2(inp) == 288957

if __name__ == '__main__':
    main()
