from collections import Counter
import re

def read_input(fn):
    with open(fn, "rt") as fh:
        return [parse(line.strip()) for line in fh.readlines()]

def parse(line):
    match = re.search(r"^(([a-z]+-)+)([0-9]+)\[([a-z]+)\]$", line)
    if match:
        groups = match.groups()
        return (groups[0][:-1], int(groups[2]), groups[3])
    return line

def gen_checksum(name):
    freq = list(Counter(name.replace("-", "")).items())
    freq.sort(key = lambda x: (-x[1], x[0]))
    return "".join([x[0] for x in freq][:5])

def shift_char(ch, n):
    if ch == "-":
        return " "
    return chr((((ord(ch) - ord('a')) + n) % 26) + ord('a'))

def decrypt_name(room):
    return "".join([shift_char(x, room[1]) for x in room[0]])

def solve_p1(inp):
    return sum([x[1] for x in inp if gen_checksum(x[0]) == x[2]])

def solve_p2(inp):
    names = [(decrypt_name(x), x[1]) for x in inp if gen_checksum(x[0]) == x[2]]
    for name, sid in names:
        if name == "northpole object storage":
            return sid
    return None

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 409147
    assert solve_p2(inp) == 991

def test_ex():
    assert parse("aaaaa-bbb-z-y-x-123[abxyz]") == ("aaaaa-bbb-z-y-x", 123, "abxyz")
    assert gen_checksum("aaaaa-bbb-z-y-x") == "abxyz"
    assert gen_checksum("a-b-c-d-e-f-g-h") == "abcde"
    assert gen_checksum("not-a-real-room") == "oarel"
    assert gen_checksum("totally-real-room") != "decoy"
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 1514
    assert decrypt_name(("qzmt-zixmtkozy-ivhz", 343)) == "very encrypted name"
    assert decrypt_name(("sleep", 711)) == "bunny"

if __name__ == '__main__':
    main()
