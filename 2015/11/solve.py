import re


INPUT = "hepxcrrq"


def inc_at_index(inp, idx):
    if idx < 0 or idx >= len(inp):
        return
    ch = (ord(inp[idx]) - 97) + 1

    # Skip characters: i, l, o
    if ch == 8 or ch == 11 or ch == 14:
        ch += 1

    if ch >= 26:
        ch -= 26
        inc_at_index(inp, idx - 1)

    inp[idx] = chr(ch + 97)

    
def gen_passwords(inp):
    regexp = re.compile(r"([a-z])\1.*([a-z])\2")
    last_idx = len(inp) - 1
    last = "z" * len(inp)
    inp = list(inp)

    # Pre-process input: remove all i, l, o characters and reset
    # characters following them to "a"
    for idx, ch in enumerate(inp):
        if ch == "i" or ch == "l" or ch == "m":
            inp[idx] = chr(ord(ch) + 1)
            for idx2 in range(idx + 1, len(inp)):
                inp[idx2] = "a"
            
    while inp != last:
        inc_at_index(inp, last_idx)
        if is_valid(inp, regexp):
            yield "".join(inp)


def is_valid(pw, regexp):
    if not regexp.search("".join(pw)):
        return False
    for grp in zip(pw, pw[1:], pw[2:]):
        if ord(grp[0]) == ord(grp[1]) - 1 == ord(grp[2]) - 2 and ord(grp[2]) <= 122:
            return True
    return False


def main():
    generator = gen_passwords(INPUT)
    sln1 = next(generator)
    print("The next valid password after \"{}\" is: \"{}\"".format(INPUT, sln1))
    sln2 = next(generator)
    print("The next valid password after \"{}\" is: \"{}\"".format(sln1, sln2))


def test_main():
    generator = gen_passwords(INPUT)
    sln1 = next(generator)
    assert sln1 == "hepxxyzz"
    sln2 = next(generator)
    assert sln2 == "heqaabcc"


def test_ex():
    assert next(gen_passwords("abcdefgh")) == "abcdffaa"
    assert next(gen_passwords("ghijklmn")) == "ghjaabcc"


if __name__ == "__main__":
    main()
