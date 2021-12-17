from typing import Counter

def read_input(filename):
    with open(filename, "rt") as fh:
        template = fh.readline().strip()
        fh.readline()
        rules = [tuple(line.strip().split(" -> ")) for line in fh.readlines()]
        return (list(template), rules)

def find_rule(els, rules):
    for rule in rules:
        if els == rule[0]:
            return rule
    return None

def expand_template(template, rules):
    """Expand an element template to a new element template."""
    nt = []
    for el1, el2 in zip(template, template[1:]):
        rule = find_rule(el1 + el2, rules)
        if rule:
            nt.extend([el1, rule[1]])
        else:
            nt.extend(el1)
    nt.append(template[-1])
    return nt

def expand_group_counter(groups, rules):
    """Expand an element group counter to a new element group
    counter."""
    new_groups = groups.copy()

    # All existing element groups should be set to zero as they will
    # disappear once split by the following rules. However keep the
    # final element with a zero-length 2nd element as this can never
    # be split.
    for e1, e2 in groups.keys():
        if len(e2) > 0:
            new_groups[(e1, e2)] = 0

    # Increase the count of groups formed by splitting each existing
    # group based on the rules.
    for e1, e2 in groups.keys():
        rule = find_rule(e1 + e2, rules)
        if not rule:
            continue
        new_groups[(e1, rule[1])] += groups[(e1, e2)]
        new_groups[(rule[1], e2)] += groups[(e1, e2)]

    return new_groups

def calculate_template_score(template):
    """Calculate the score of an element template.

    The score is the count of the most frequent element minus the
    count of the least frequent.
    """
    c = Counter(template)
    max_el = max(c, key=lambda x: c[x])
    min_el = min(c, key=lambda x: c[x])
    return c[max_el] - c[min_el]

def calculate_group_score(groups):
    """Calculate the score from an element group counter.

    The score is the count of the most frequent element minus the
    count of the least frequent.
    """
    c = Counter()

    # Count the number of times the first element from a two-element
    # group appears in the group counter. e.g. {"ab": 1, "ac": 2}
    # would count "a" as 1 + 2 = 3.
    for k, v in groups.items():
        c[list(k)[0]] += v

    max_el = max(c, key=lambda x: c[x])
    min_el = min(c, key=lambda x: c[x])
    return c[max_el] - c[min_el]

def run_template_expansions(inp, n):
    """Starting from the element template in `inp`, expand it `n`
    times and return it."""
    expansion = inp[0]
    for _ in range(n):
        expansion = expand_template(expansion, inp[1])
    return expansion

def run_group_expansions(inp, n):
    """Starting from the element template in `inp`, create an element
    group counter and expand groups `n` times, then return them."""

    # Create a group counter for each two-element group in the initial
    # template. Add an empty element to the last group so that the
    # final element is counted. i.e. ["A","B","C" ]-> {["A", "B"]: 1,
    # ["B", "C"]: 1, ["C", ""]: 1}
    groups = Counter([x for x in zip(inp[0], (inp[0] + [""])[1:])])

    for _ in range(n):
        groups = expand_group_counter(groups, inp[1])
    return groups

def solve_p1(inp):
    # NOTE: the p2 solution is more optimised, so p2 can be used to
    # solve p1 as well. However it has been kept as this was my
    # initial solution before seeing p2.
    return calculate_template_score(run_template_expansions(inp, 10))

def solve_p2(inp):
    return calculate_group_score(run_group_expansions(inp, 40))

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 3306
    assert solve_p2(inp) == 3760312702877

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 1588
    assert solve_p2(inp) == 2188189693529

if __name__ == '__main__':
    main()
