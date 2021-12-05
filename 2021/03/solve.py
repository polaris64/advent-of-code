from math import prod

def read_input(f):
    with open(f, "rt") as fh:
        return [list(x.strip()) for x in fh.readlines()]

def calculate(inp):
    word_len = len(inp[0])
    output = ["0"] * word_len
    for bit in range(word_len):
        count = [0, 0]
        for word in inp:
            if word[bit] == "0":
                count[0] += 1
            elif word[bit] == "1":
                count[1] += 1
        output[bit] = "0" if count[0] > count[1] else "1"
    return (
        output,
        ["1" if x == "0" else "0" for x in output],
    )

def calculate_p2(inp):
    res = [None] * 2
    for rating_type in range(2):
        bit = 0
        next_list = inp.copy()
        while len(next_list) > 1 or bit == len(inp[0]) - 1:
            stats = calculate(next_list)
            bit_val = stats[rating_type][bit]
            next_list = [x for x in next_list if x[bit] == bit_val]
            bit += 1
        res[rating_type] = next_list[0]
    return res

def solve_p1(inp):
    return prod([int("".join(x), 2) for x in calculate(inp)])

def solve_p2(inp):
    return prod([int("".join(x), 2) for x in calculate_p2(inp)])

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 198
    assert solve_p2(inp) == 230

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 2035764
    assert solve_p2(inp) == 2817661

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

if __name__ == '__main__':
    main()
