import re


def read_input(fn):
    with open(fn, "rt") as fh:
        lines = [l.strip() for l in fh.readlines()]
        return parse_lines(lines)


def parse_lines(lines):
    res = {
        "seeds": None,
        "maps": dict(),
    }
    current_map_type = None

    for line in lines:
        if line == "":
            current_map_type = None
            continue
        if line[0:6] == "seeds:":
            match = re.search(r"^seeds: (.+)", line)
            if match:
                res["seeds"] = [int(x) for x in match.groups()[0].split()]
            continue
        if line[-1] == ":":
            match = re.search(r"^([a-z]+)-to-([a-z]+) map:", line)
            if match:
                current_map_type = tuple(match.groups())
                res["maps"][current_map_type] = list()
            continue
        if current_map_type is not None:
            res["maps"][current_map_type].append(tuple([int(x) for x in line.split()]))

    return res


def map_range(inp, src_id, types):
    maps = inp["maps"][types]
    for m in maps:
        sr = range(m[1], m[1] + m[2] + 1)
        dr = range(m[0], m[0] + m[2] + 1)
        if src_id in sr:
            return dr.start + (src_id - sr.start)
    return src_id
        

def map_seeds(inp):
    categories = list(get_categories(inp))

    def _map_seed(seed):
        mapped_id = seed
        for cat in categories:
            mapped_id = map_range(inp, mapped_id, cat)
        return mapped_id
    
    for seed in inp["seeds"]:
        yield _map_seed(seed)


def get_seed_ranges(inp):
    l = inp["seeds"]
    return [range(l[x], l[x] + l[x + 1] + 1) for x in range(0, len(l), 2)]


def get_categories(inp):
    current_source_type = "seed"
    while True:
        current_map_type = [map for map in inp["maps"].keys() if map[0] == current_source_type]
        if not len(current_map_type):
            break
        current_map_type = current_map_type[0]
        yield current_map_type
        current_source_type = current_map_type[1]


def find_lowest_final_id(inp):
    cats = list(get_categories(inp))
    final_cat = cats[-1][1]


def solve_p1(inp):
    return min(map_seeds(inp))

def solve_p2(inp):
    return None


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 226172555
    assert solve_p2(inp) == None


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 35
    assert solve_p2(inp) == 46


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
