import re
from copy import deepcopy


def read_input(fname):
    with open(fname, "r") as handle:
        lines = [x.strip() for x in handle.readlines()]
        return process_input(lines)


def process_input(lines):
    return {
        "rules": dict(
            [parse_rule(x) for x in lines if re.match(r"^[0-9]+:", x)]
        ),
        "patterns": [x for x in lines if re.match(r"^[a-z]", x)],
    }


def parse_rule(inp):
    groups = re.match(r"^([0-9]+): (.*)", inp).groups()
    rnum = int(groups[0])
    or_parts = groups[1].split("|")
    if len(or_parts) == 1 and re.match(r"\"[a-z]\"", or_parts[0]):
        rparts = or_parts[0].replace("\"", "")
    else:
        rparts = [
            [int(x.strip()) for x in or_part.strip().split(" ")]
            for or_part in or_parts
        ]
    return (rnum, rparts)


def match_rule(rules, text, rnum):
    if len(text) == 0:
        return (False, text)
    rule = rules[rnum]

    # Rule matches a single character
    if type(rule) == str:
        match = text[0] == rule
        return (match, [text[1:] if match else text])

    # Rule is a list of "OR" parts which are each a sequence of rules
    else:
        # Check each "OR" rule part against the pattern
        part_results = [
            match_rule_part(rules, rpart, text)
            for rpart in rule
        ]

        # Get list of results which are a match
        matches = [x for x in part_results if x[0]]

        # Get list of unmatched pattern suffixes for each matching
        # rule
        part_suffixes = [x[1] for x in matches]

        if len(matches) > 0:
            # Return success (match) and a flattened list of suffixes
            return (True, [x for sublist in part_suffixes for x in sublist])
        else:
            # Return failure (no match) and the a single suffix of the
            # entire input text
            return (False, [text])


def match_rule_part(rules, rpart, text):
    suffixes = [text]
    matches = True

    # Loop for each rule in the sequence
    for rpart_num in rpart:

        # Build a new list of suffixes from matching this rule to each
        # suffix in turn
        new_suffixes = []
        for suffix in suffixes:
            match, part_suffixes = match_rule(rules, suffix, rpart_num)
            if match:
                new_suffixes.extend(part_suffixes)
        suffixes = new_suffixes

        # Suffixes are only added on a match, so if there are none
        # then the rule did not match anything
        if len(suffixes) == 0:
            matches = False
            break
    return (matches, suffixes if matches else [text])


def patch_rules(inp):
    res = deepcopy(inp)
    res["rules"][8] = [[42], [42, 8]]
    res["rules"][11] = [[42, 31], [42, 11, 31]]
    return res


def count_matching_patterns(inp):
    return len([
        1 for pat_match, pat_suffixes in
        [match_rule(inp["rules"], pat, 0) for pat in inp["patterns"]]

        # Pattern must match (pat_match) and list of unmatched strings
        # should contain at least one item that's empty, meaning that
        # at least one path consumed all of the pattern.
        if pat_match and any([len(suffix) == 0 for suffix in pat_suffixes])
    ])


def solve_p1(inp):
    return count_matching_patterns(inp)


def solve_p2(inp):
    inp = patch_rules(inp)
    return count_matching_patterns(inp)


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 190
    assert solve_p2(inp) == 311


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 2
    inp = read_input("input_ex2.txt")
    assert solve_p2(inp) == 12


if __name__ == "__main__":
    main()
