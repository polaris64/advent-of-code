def read_input(filename):
    with open(filename, "rt") as fh:
        return [parse_line(x) for x in fh.readlines()]

def parse_line(line):
    patterns, output = [x.strip() for x in line.split("|")]
    return (
        patterns.split(" "),
        output.split(" "),
    )

def create_mapping():
    base_str = "abcdefg"
    return {
        "a": set(base_str),
        "b": set(base_str),
        "c": set(base_str),
        "d": set(base_str),
        "e": set(base_str),
        "f": set(base_str),
        "g": set(base_str),
    }

def update_mapping(mappings, segments, wires):
    for mseg, _ in mappings.items():
        if mseg in segments:
            mappings[mseg] = mappings[mseg].intersection(wires)
        else:
            mappings[mseg] = mappings[mseg].difference(wires)

def get_definite_segments(sample_len):
    if sample_len == 2: # 1
        return list("cf")
    elif sample_len == 3: # 7
        return list("acf")
    elif sample_len == 4: # 4
        return list("bcdf")
    elif sample_len == 7: # 8
        return list("abcdefg")
    return None

def generate_mapping(sample):
    m = create_mapping()

    # Mark segments for known numbers (1, 4, 7, 8)
    for s in sample[0]:
        segments = get_definite_segments(len(s))
        if segments:
            update_mapping(m, segments, s)

    # Narrow down ambiguous (length-5 and length-6) numbers
    #
    # Each digit of a certain length always has a set of active
    # segments (e.g. all digits with 5 segments always have segments
    # "adg" active), so this information can be used to narrow down
    # the possibilities further.
    for g_len, g_set in [(5, "adg"), (6, "abfg")]:
        g_mapping = set("abcdefg")
        items = [set(x) for x in sample[0] if len(x) == g_len]
        for item in items:
            g_mapping = g_mapping.intersection(item)
        if len(g_mapping) == len(g_set):
            update_mapping(m, list(g_set), g_mapping)

    return m

def output_digit(segments):
    output = [list(' ' * 6) for _ in [None] * 6]
    char = "#"
    if "a" in segments:
        output[0][1] = char
        output[0][2] = char
        output[0][3] = char
        output[0][4] = char
    if "b" in segments:
        output[1][0] = char
        output[2][0] = char
    if "c" in segments:
        output[1][5] = char
        output[2][5] = char
    if "d" in segments:
        output[3][1] = char
        output[3][2] = char
        output[3][3] = char
        output[3][4] = char
    if "e" in segments:
        output[4][0] = char
        output[5][0] = char
    if "f" in segments:
        output[4][5] = char
        output[5][5] = char
    if "g" in segments:
        output[6][1] = char
        output[6][2] = char
        output[6][3] = char
        output[6][4] = char
    return "\n".join(["".join(line) for line in output])

def decode_digit(mappings, signals):
    active = set()
    for s in signals:
        mapping = [(k, len(v)) for k, v in mappings.items() if s in v]
        for m in mapping:
            active.add(m[0])
    return list(active)

def convert_digit(segments):
    mapping = {2: 1, 3: 7, 4: 4, 7: 8}
    if len(segments) in mapping:
        return mapping[len(segments)]
    if sorted(segments) == list("acdeg"):
        return 2
    if sorted(segments) == list("acdfg"):
        return 3
    if sorted(segments) == list("abdfg"):
        return 5
    if sorted(segments) == list("abdefg"):
        return 6
    if sorted(segments) == list("abcdfg"):
        return 9
    if sorted(segments) == list("abcefg"):
        return 0

def solve_p1(inp):
    counter = 0
    outputs = [op for _, op in inp]
    for output_set in outputs:
        for x in output_set:
            if len(x) == 2 or len(x) == 4 or len(x) == 3 or len(x) == 7:
                counter += 1
    return counter

def solve_p2(inp):
    res = 0
    for sample in inp:
        m = generate_mapping(sample)
        res += int("".join([str(convert_digit(decode_digit(m, digit))) for digit in sample[1]]), 10)
    return res

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 421
    assert solve_p2(inp) == 986163

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 26
    assert solve_p2(inp) == 61229

if __name__ == '__main__':
    main()
