import re


def read_input(fname):
    with open(fname, "r") as handle:
        records = [x.replace("\n", " ") for x in handle.read().split("\n\n")]
        return [parse_passport(rec) for rec in records]


def parse_passport(passport):
    fields = passport.replace("\n", " ").split(" ")
    return dict([tuple(x.split(":")) for x in fields if len(x) > 0])


def has_required_fields(passport):
    for r_field in ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]:
        if r_field not in passport.keys():
            return False
    return True


def validate_field(key, val):
    def _hgt_check(val):
        return (
            ("cm" in val and 150 <= int(val.replace("cm", "")) <= 193) or
            ("in" in val and 59 <= int(val.replace("in", "")) <= 76)
        )

    rules = {
        "byr": lambda x: 1920 <= int(x) <= 2002,
        "cid": lambda x: True,
        "ecl": lambda x: x in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
        "eyr": lambda x: 2020 <= int(x) <= 2030,
        "hcl": lambda x: bool(re.match(r"^#[a-fA-F0-9]{6}$", x)),
        "hgt": _hgt_check,
        "iyr": lambda x: 2010 <= int(x) <= 2020,
        "pid": lambda x: bool(re.match(r"^[0-9]{9}$", x)),
    }
    return key in rules.keys() and rules[key](val)


def is_valid_p2(passport):
    return (
        has_required_fields(passport) and
        all([validate_field(k, v) for (k, v) in passport.items()])
    )


def solve_p1(inp):
    return len([x for x in inp if has_required_fields(x)])


def solve_p2(inp):
    return len([x for x in inp if is_valid_p2(x)])


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 245
    assert solve_p2(inp) == 133


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 2
    inp = read_input("input_ex2.txt")
    assert solve_p2(inp) == 0
    inp = read_input("input_ex3.txt")
    assert solve_p2(inp) == 4


if __name__ == "__main__":
    main()
