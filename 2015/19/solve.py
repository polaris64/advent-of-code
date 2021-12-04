import re

def read_input(fn):
    with open(fn, "rt") as fh:
        lines = [x.strip() for x in fh.readlines()]
        return parse(lines)

def parse(lines):
    start = ""
    rules = []
    for line in lines:
        rule_matches = re.findall(r"([^ ]+) => (.*)", line)
        if len(rule_matches) == 1:
            rules.append(rule_matches[0])
        else:
            if re.search(r"^[^ ]+$", line):
                start = line
    return {
        "rules": rules,
        "start": start,
    }

# def get_replacements(rules, ch):
#     # TODO: Wrong: inputs are not single chars in live input
#     res = [rule[1] for rule in rules if rule[0] == ch]
#     if len(res) == 0:
#         res = [ch]
#     return res

# def get_permutations(start, arr):
#     mols = set()
#     # TODO: Wrong: inputs are not single chars in live input
#     for idx, ch in enumerate(start):
#         for v in arr[idx]:
#             n = list(start)
#             n[idx] = v
#             mols.add(str("".join(n)))
#     return mols

# def solve_p1(inp, start):
#     reps = [get_replacements(inp, ch) for ch in start]
#     print(reps)
#     perms = get_permutations(start, reps)
#     return len(perms)

def solve_p1(inp):
    mols = set()
    start = inp["start"]
    for idx in range(len(start)):
        for rule in inp["rules"]:
            ri = rule[0]
            if start[idx:idx + len(ri)] == ri:
                n = list(start)
                n = n[:idx] + list(rule[1]) + n[idx + len(ri):]
                mols.add("".join(n))
    return len(mols)

def main():
    inp = read_input("input.txt")
    print(inp)
    print("The solution to part 1 is: {}".format(solve_p1(inp)))

def test_p1_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 535

def test_p1_example():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 4
    inp = read_input("input_ex2.txt")
    assert solve_p1(inp) == 7

if __name__ == "__main__":
    main()
