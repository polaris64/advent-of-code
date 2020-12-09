from itertools import combinations


def read_input(fname):
    with open(fname, "r") as handle:
        return [int(x.strip()) for x in handle.readlines()]


def scan_list(inp, preamble_len):
    idx_start = 0
    for (idx, item) in enumerate(inp):
        if idx < preamble_len:
            continue
        pairs = find_pairs(inp, item, idx_start, preamble_len)
        yield (idx, item, pairs)
        idx_start += 1


def find_pairs(inp, item, idx_start, preamble_len):
    source = inp[idx_start:idx_start + preamble_len]
    for pair in combinations(source, 2):
        if sum(pair) == item:
            return pair
    return None


def solve_p1(inp, preamble_len):
    for item in scan_list(inp, preamble_len):
        if item[2] is None:
            return item[1]
    return None


def solve_p2(inp, target_num):
    for i in range(1, len(inp)):
        for j in range(i + 1, len(inp)):
            range_sum = sum(inp[i:j])
            if range_sum > target_num:
                break
            if range_sum == target_num:
                nums = inp[i:j]
                nums.sort()
                return nums[0] + nums[-1]
    return None


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp, 25)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp, sln1)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    p1_res = solve_p1(inp, 25)
    assert p1_res == 29221323
    assert solve_p2(inp, p1_res) == 4389369


def test_ex():
    inp = read_input("input_ex.txt")
    p1_res = solve_p1(inp, 5)
    assert p1_res == 127
    assert solve_p2(inp, p1_res) == 62


if __name__ == "__main__":
    main()
