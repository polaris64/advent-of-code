from typing import Counter

def read_input(filename):
    with open(filename, "rt") as fh:
        return [tuple(line.strip().split("-")) for line in fh.readlines()]

def get_connected_caves(connections, cave):
    return [b for a, b in connections if a == cave] + [a for a, b in connections if b == cave]

def get_paths_from_node(connections, node, small_cave_max):
    def can_visit_cave(cave, visits):
        if cave == "start":
            return False
        if small_cave_max == 1:
            return visits[cave] <= 0
        cv = [k for k, v in visits.items() if k.islower() and v >= small_cave_max]
        if len(cv) > 0:
            if cave in cv:
                return False
            return visits[cave] <= 0
        return visits[cave] <= small_cave_max

    def _inner(connections, node, visited):
        visited[node] += 1
        paths = []
        for n in get_connected_caves(connections, node):
            if n.islower() and not can_visit_cave(n, visited):
                continue
            if n == "end":
                paths.append([node, n])
                continue
            sub_paths = [[node] + path for path in _inner(connections, n, visited.copy())]
            if len(sub_paths) > 0:
                paths.extend(sub_paths)
        return paths

    return _inner(connections, node, Counter())

def solve_p1(inp):
    return len(get_paths_from_node(inp, "start", 1))

def solve_p2(inp):
    return len(get_paths_from_node(inp, "start", 2))

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 5920
    assert solve_p2(inp) == 155477

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 10
    assert solve_p2(inp) == 36
    inp = read_input("input_ex2.txt")
    assert solve_p1(inp) == 19
    assert solve_p2(inp) == 103
    inp = read_input("input_ex3.txt")
    assert solve_p1(inp) == 226
    assert solve_p2(inp) == 3509

if __name__ == '__main__':
    main()
