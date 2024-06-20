import re


def read_input(filename):
    with open(filename, "r") as fh:
        return fh.readlines()[0].strip()


def decompress(line):
    in_group = False
    group = list()
    res = list()
    skip = 0
    for idx, ch in enumerate(line):
        if skip > 0:
            skip -= 1
            continue
        if in_group:
            if ch == ")":
                in_group = False
                group_str = "".join(group)
                group = list()
                m = re.search(r"([0-9]+)x([0-9]+)", group_str)
                if m:
                    group_len = int(m.groups()[0])
                    group_count = int(m.groups()[1])
                    skip = group_len
                    res.extend(list(line)[idx + 1:idx + group_len + 1] * group_count)
                continue
            group.append(ch)
            continue
        if ch == "(":
            in_group = True
            continue
        res.append(ch)
    return "".join(res)


def get_decomp_len(line, mult=1):
    in_group = False
    group = list()
    skip = 0
    count = 0
    idx = 0
    while True:
        if idx >= len(line):
            break
        ch = line[idx]
        if in_group:
            if ch == ")":
                in_group = False
                group_str = "".join(group)
                group = list()
                m = re.search(r"([0-9]+)x([0-9]+)", group_str)
                if m:
                    group_len = int(m.groups()[0])
                    group_count = int(m.groups()[1])
                    count += get_decomp_len(line[idx + 1:idx + 1 + group_len], group_count)
                    idx += group_len
            group.append(ch)
        elif ch == "(":
            in_group = True
        else:
            count += 1

        idx += 1

    return count * mult


def solve_p1(inp):
    return len(decompress(inp))


def solve_p2(inp):
    return get_decomp_len(inp)


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 74532
    assert solve_p2(inp) == 11558231665


if __name__ == '__main__':
    main()
