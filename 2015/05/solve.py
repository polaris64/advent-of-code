import re


def read_input(filename):
    with open(filename, "rt") as fhandle:
        return [x.strip() for x in fhandle.readlines()]


def is_nice_word_v1(word):
    # It contains at least three vowels (aeiou only), like aei,
    # xazegov, or aeiouaeiouaeiou.
    vowels = len([x for x in word if x in list("aeiou")])
    if vowels < 3:
        return False

    # It contains at least one letter that appears twice in a row,
    # like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
    if not re.search(r"(\w)\1+", word):
        return False

    # It does not contain the strings ab, cd, pq, or xy, even if they
    # are part of one of the other requirements.
    if re.search(r"(ab|cd|pq|xy)", word):
        return False

    return True


def is_nice_word_v2(word):
    # It contains a pair of any two letters that appears at least
    # twice in the string without overlapping, like xyxy (xy) or
    # aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
    if not re.search(r"(\w)(\w).*\1\2", word):
        return False

    # It contains at least one letter which repeats with exactly one
    # letter between them, like xyx, abcdefeghi (efe), or even aaa.
    if not re.search(r"(\w)\w\1", word):
        return False

    return True


def solve_p1(inp):
    return sum([1 if is_nice_word_v1(x) else 0 for x in inp])


def solve_p2(inp):
    return sum([1 if is_nice_word_v2(x) else 0 for x in inp])


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 258
    assert solve_p2(inp) == 53

def test_ex():
    assert is_nice_word_v1("ugknbfddgicrmopn") == True
    assert is_nice_word_v1("aaa") == True
    assert is_nice_word_v1("jchzalrnumimnmhp") == False
    assert is_nice_word_v1("haegwjzuvuyypxyu") == False
    assert is_nice_word_v1("dvszwmarrgswjxmb") == False
    assert is_nice_word_v2("qjhvhtzxzqqjkmpb ") == True
    assert is_nice_word_v2("xxyxx ") == True
    assert is_nice_word_v2("uurcxstgmygtbstg ") == False
    assert is_nice_word_v2("ieodomkazucvgmuy ") == False


if __name__ == "__main__":
    main()
