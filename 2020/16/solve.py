import operator
import re
from functools import reduce


def read_input(fname):
    with open(fname, "r") as handle:
        lines = [x.strip() for x in handle.readlines()]
        return parse_input(lines)


def parse_input(lines):
    res = {
        "rules": dict(),
        "yours": None,
        "nearby": [],
    }
    stage = None
    for line in lines:
        if len(line) == 0:
            stage = None
            continue
        if re.match(r"^[^:]+: [0-9]", line):
            groups = re.match(r"^([^:]+): (.*)", line).groups()
            res["rules"][groups[0]] = [
                tuple(
                    [int(y) for y in x.strip().split("-")]
                )
                for x in groups[1].split("or")
            ]
        elif line == "your ticket:":
            stage = "Y"
        elif line == "nearby tickets:":
            stage = "N"
        elif re.match(r"^([0-9]+,)+", line):
            values = [int(x) for x in line.split(",")]
            if stage == "Y":
                res["yours"] = values
            else:
                res["nearby"].append(values)
    return res


def is_range_valid(val, rng):
    return rng[0] <= val <= rng[1]


def is_rule_valid(val, rule):
    return any([is_range_valid(val, range) for range in rule])


def is_field_valid(val, rules):
    return any([is_rule_valid(val, rule) for rule in rules])


def get_invalid_fields(notes, ticket):
    return [
        field for field
        in ticket
        if not is_field_valid(field, notes["rules"].values())
    ]


def is_ticket_valid(notes, ticket):
    return len(get_invalid_fields(notes, ticket)) == 0


def get_all_invalid_fields(notes):
    return reduce(
        operator.add,
        [get_invalid_fields(notes, ticket) for ticket in notes["nearby"]],
    )


def derive_fields(notes, tickets):
    ordered_rules = []

    # Look at each position in turn (0..len(tickets[0]))
    for pos in range(0, len(tickets[0])):

        # Create set of possible fields for position
        possible_fields = {fname for fname, ranges in notes["rules"].items()}

        # Remove all fields which are not valid
        for ticket in tickets:
            for fname, ranges in notes["rules"].items():
                if not is_rule_valid(ticket[pos], ranges):
                    if fname in possible_fields:
                        possible_fields.remove(fname)

        # Add remaining possibilities to the list
        ordered_rules.append(possible_fields)

    # Repeatedly go through all rules and remove any fields that exist
    # singularly from others that also contain that field
    while not all([len(x) == 1 for x in ordered_rules]):
        for rule in ordered_rules:
            if len(rule) == 1:
                for rule_mod in ordered_rules:
                    field_name = list(rule)[0]
                    if rule_mod != rule and field_name in rule_mod:
                        rule_mod.remove(field_name)

    return ordered_rules


def solve_p1(inp):
    return sum(get_all_invalid_fields(inp))


def solve_p2(inp):
    valid_tickets = [x for x in inp["nearby"] if is_ticket_valid(inp, x)]
    fields = derive_fields(inp, valid_tickets)
    your_ticket = dict(zip([list(x)[0] for x in fields], inp["yours"]))
    return reduce(
        operator.mul,
        [v for k, v in your_ticket.items() if k[:9] == "departure"]
    )


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 24110
    assert solve_p2(inp) == 6766503490793


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 71


if __name__ == "__main__":
    main()
