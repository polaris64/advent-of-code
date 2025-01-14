from collections import Counter
import re


def read_input(fn):
    with open(fn, "rt") as fh:
        return [parse_line(l.strip()) for l in fh.readlines()]


def parse_line(line):
    match = re.search(r"^([^ ]+) \(([^)]+)\)( -> (.+))?", line)
    if match is not None:
        groups = match.groups()
        links = None
        if groups[2] is not None:
            links = set(groups[2].replace(" -> ", "").split(", "))
        return (groups[0], int(groups[1]), links)


def get_weight(inp, head):
    node = [x for x in inp if x[0] == head][0]
    if node[2] is None:
        return node[1]
    return node[1] + sum([get_weight(inp, x) for x in node[2]])


def check_weight(inp, head):
    node = [x for x in inp if x[0] == head][0]
    if node[2] is None:
        return True, head
    subtree = [(x, get_weight(inp, x)) for x in node[2]]

    # Check if subtree nodes have different weights
    if len(set([x[1] for x in subtree])) != 1:

        # Find the node with the different weight
        most_common_weight = Counter([x[1] for x in subtree]).most_common()
        least_common_weight = most_common_weight[-1]
        bad_node = [x[0] for x in subtree if x[1] == least_common_weight[0]][0]
        subtree_res = check_weight(inp, bad_node)
        if subtree_res[0]:
            bad_node = [x for x in inp if x[0] == bad_node][0]

            # Calculate the difference between the two subtree weight
            # values and calculate the difference needed for this node
            # in order to make the subtree balance.
            return False, bad_node, bad_node[1] - (most_common_weight[-1][0] - most_common_weight[0][0])
        else:
            return subtree_res
    return True, head


def solve_p1(inp):
    linked = set()
    for el in inp:
        if el[2] is not None:
            linked.update(el[2])
    for el in inp:
        if el[0] not in linked:
            return el[0]


def solve_p2(inp):
    return check_weight(inp, solve_p1(inp))[2]


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == "wiapj"
    assert solve_p2(inp) == 1072


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == "tknk"
    assert solve_p2(inp) == 60


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
