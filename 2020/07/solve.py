from functools import lru_cache
import re


def read_input(fname):
    with open(fname, "r") as handle:
        return [parse_rule(x.strip()) for x in handle.readlines()]


def parse_rule(line):
    parts = line.split("bags contain")
    contents = [
        x.replace(".", "").replace("bags", "").replace("bag", "").strip()
        for x in parts[1].strip().split(", ")
    ]
    contents = [extract_qty_type(x) for x in contents]
    return (parts[0].strip(), contents)


def extract_qty_type(rec):
    match = re.match(r"([0-9]+) (.*)", rec)
    if match:
        return tuple(match.groups())
    else:
        return None


def does_outer_contain_inner(rules, outer, inner):
    @lru_cache(maxsize=None)
    def _rec(outer, inner):
        outer_rule = [x for x in rules if x[0] == outer]
        if len(outer_rule) == 0:
            return 0
        outer_rule = outer_rule[0]

        # Check if outer_rule lists inner directly
        if any([True for x in outer_rule[1] if x is not None and x[1] == inner]):
            return 1

        if len(outer_rule[1]) > 0:
            return any([_rec(x[1], inner) for x in outer_rule[1] if x is not None])
        else:
            return 0

    return _rec(outer, inner)


def count_total_bags(bag_type, rules):
    # Find rule for bag_type
    rule = [x for x in rules if x[0] == bag_type]
    if len(rule) == 0:
        return 1
    rule = rule[0]

    # Recursively count inner bags
    count = 1
    for other in rule[1]:
        if other is not None:
            count += int(other[0]) * count_total_bags(other[1], rules)

    return count


def solve_p1(inp):
    return sum([does_outer_contain_inner(inp, rule[0], "shiny gold") for rule in inp])


def solve_p2(inp):
    return count_total_bags("shiny gold", inp) - 1


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 131
    assert solve_p2(inp) == 11261


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 4
    inp = read_input("input_ex2.txt")
    assert solve_p2(inp) == 126


if __name__ == "__main__":
    main()
