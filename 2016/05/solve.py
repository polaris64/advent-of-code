from hashlib import md5

LIVE_INP = "ugkcyxxp"

def valid_hash(hash, l):
    return hash[:l] == "0" * l

def gen_chars(start):
    n = 0
    while True:
        h = md5()
        h.update(bytes(start + str(n), "utf-8"))
        d = h.hexdigest()
        if valid_hash(d, 5):
            yield d
        n += 1

def solve_p1(inp):
    res = ""
    for h in gen_chars(inp):
        res += h[5]
        print("\rSolving... {}".format(res), end="")
        if len(res) == 8:
            print()
            return res

def solve_p2(inp):
    res = list("________")
    for h in gen_chars(inp):
        pos = int(h[5], 16)
        if pos > 7 or res[pos] != "_":
            continue
        res[pos] = h[6]
        print("\rSolving... {}".format("".join(res)), end="")
        if len([x for x in res if x != "_"]) == 8:
            print()
            return "".join(res)

def main():
    print("The solution to part 1 is: {}".format(solve_p1(LIVE_INP)))
    print("The solution to part 2 is: {}".format(solve_p2(LIVE_INP)))

def test_ex():
    inp = "abc"
    assert valid_hash("1234abcd", 5) == False
    assert valid_hash("0000abcd", 5) == False
    assert valid_hash("00000bcd", 5) == True
    assert solve_p1(inp) == "18f47a30"
    assert solve_p2(inp) == "05ace8e3"

def test_main():
    assert solve_p1(LIVE_INP) == "d4cd2ee1"
    assert solve_p2(LIVE_INP) == "f2c730e5"


if __name__ == '__main__':
    main()
