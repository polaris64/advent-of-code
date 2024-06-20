def read_input(filename):
    with open(filename, "r") as fh:
        return [parse(x.strip()) for x in fh.readlines()]


def parse(line):
    res = {
        "ss": list(),
        "hs": list(),
    }
    cur = list()
    for ch in line:
        if ch == "[":
            if len(cur):
                res["ss"].append(cur)
                cur = list()
            continue
        if ch == "]":
            res["hs"].append(cur)
            cur = list()
            continue
        cur.append(ch)
    if len(cur):
        res["ss"].append(cur)
    return res


def is_abba(seq):
    for idx, (a, b) in enumerate(zip(seq, seq[1:])):
        if a == b:
            continue
        if idx + 3 >= len(seq):
            continue
        x, y = (seq[idx + 2], seq[idx + 3])
        if (b, a) == (x, y):
            return True
    return False


def get_abas(seq):
    for a, b, c in zip(seq, seq[1:], seq[2:]):
        if a == c and a != b:
            yield a, b, c


def is_bab(seq, aba):
    for a, b, c in zip(seq, seq[1:], seq[2:]):
        if a == aba[1] and b == aba[0] and c == aba[1]:
            return True
    return False


def check_aba_bab(addr):
    for ss in addr["ss"]:
        for aba in list(get_abas(ss)):
            for hs in addr["hs"]:
                if is_bab(hs, aba):
                    return True
    return False

        
def solve_p1(inp):
    addr_tls = [any([is_abba(x) for x in addr["ss"]]) and not any([is_abba(x) for x in addr["hs"]]) for addr in inp]
    return len([x for x in addr_tls if x])


def solve_p2(inp):
    return len([x for x in [check_aba_bab(addr) for addr in inp] if x])


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 2
    assert solve_p2(inp) == None


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 105
    assert solve_p2(inp) == None


if __name__ == '__main__':
    main()
