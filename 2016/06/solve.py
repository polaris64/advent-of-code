from collections import Counter


def read_input(filename):
    with open(filename, "r") as fh:
        return [x.strip() for x in fh.readlines()]


def char_freq(l):
    return Counter(l)


def solve_p1(inp):
    res_len = len(inp[0])
    res = []
    for x in range(res_len):
        char_freqs = char_freq([s[x] for s in inp])
        res.append(char_freqs.most_common(1)[0][0])
    return "".join(res)


def solve_p2(inp):
    res_len = len(inp[0])
    res = []
    for x in range(res_len):
        char_freqs = char_freq([s[x] for s in inp])
        res.append(char_freqs.most_common()[-1][0])
    return "".join(res)


def main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    print("The solution to part 1 is: {}".format(res))


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == "easter"
    assert solve_p2(inp) == "advent"


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == "agmwzecr"
    assert solve_p2(inp) == "owlaxqvq"


if __name__ == '__main__':
    main()
