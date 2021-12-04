import json


def read_input(filename):
    with open(filename, "rt") as fhandle:
        return json.load(fhandle)


def get_sum(node, ignore_val = None):
    if isinstance(node, list):
        return sum([get_sum(x, ignore_val) for x in node])
    if isinstance(node, dict):
        if ignore_val and ignore_val in node.values():
            return 0
        return sum([get_sum(x, ignore_val) for x in node.values()])
    if isinstance(node, int):
        return node
    return 0


def solve_p1(inp):
    return get_sum(inp)


def solve_p2(inp):
    return get_sum(inp, "red")


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


def test_ex():
    assert get_sum([1,2,3]) == 6
    assert get_sum({"a":2,"b":4}) == 6
    assert get_sum([[[3]]]) == 3
    assert get_sum({"a":{"b":4},"c":-1}) == 3
    assert get_sum({"a":[-1,1]}) == 0
    assert get_sum([-1,{"a":1}]) == 0
    assert get_sum([]) == 0
    assert get_sum({}) == 0
    assert get_sum([1,2,3], "red") == 6
    assert get_sum([1,{"c":"red","b":2},3], "red") == 4
    assert get_sum({"d":"red","e":[1,2,3,4],"f":5}, "red") == 0
    assert get_sum([1,"red",5], "red") == 6


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 111754
    assert solve_p2(inp) == 65402


if __name__ == "__main__":
    main()
