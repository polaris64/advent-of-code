SAMPLE = {
    "children": 3,
    "cats": 7,
    "samoyeds": 2,
    "pomeranians": 3,
    "akitas": 0,
    "vizslas": 0,
    "goldfish": 5,
    "trees": 3,
    "cars": 2,
    "perfumes": 1,
}

FIELDS = SAMPLE.keys()


def read_input(filename):
    with open(filename, "rt") as fhandle:
        return [parse(x.strip()) for x in fhandle.readlines()]


def parse(line):
    idx = line.index(":") + 1
    props = line[idx:].split(", ")
    return {prop.split(":")[0].strip():int(prop.split(":")[1].strip()) for prop in props}


def is_possible(rec):
    for field in FIELDS:
        if field in rec and rec[field] != SAMPLE[field]:
            return False
    return True


def is_possible_p2(rec, field):
    if field not in rec:
        return True
    if field in ["cats", "trees"] and rec[field] > SAMPLE[field]:
        return True
    if field in ["goldfish", "pomeranians"] and rec[field] < SAMPLE[field]:
        return True
    if rec[field] == SAMPLE[field]:
        return True
    return False


def solve_p1(inp):
    res = [idx + 1 for idx, rec in enumerate(inp) if is_possible(rec)]
    return res[0] if len(res) > 0 else None


def solve_p2(inp):
    res = [(idx + 1, rec) for idx, rec in enumerate(inp)]
    for field in FIELDS:
        res = [x for x in res if is_possible_p2(x[1], field)]
    return res[0][0] if len(res) > 0 else None


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 373
    assert solve_p2(inp) == 260


if __name__ == "__main__":
    main()
