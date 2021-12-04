def read_input_p1(fn):
    with open(fn, "rt") as fh:
        return [build_tri(ex_numbers(line.strip())) for line in fh.readlines()]

def read_input_p2(fn):
    triangles = []
    with open(fn, "rt") as fh:
        temp = [[], [], []]
        for line in fh.readlines():
            numbers = [int(x.strip()) for x in line.split(" ") if len(x) > 0]
            for i in range(3):
                temp[i].append(numbers[i])
            if len(temp[0]) == 3:
                for i in range(3):
                    triangles.append(temp[i])
                temp = [[], [], []]
    return [build_tri(x) for x in triangles]

def ex_numbers(line):
    return [int(x.strip()) for x in line.split(" ") if len(x) > 0]

def build_tri(numbers):
    m = max(enumerate(numbers), key = lambda x: x[1])
    return (m[1], tuple(x[1] for x in enumerate(numbers) if x[0] != m[0]))

def is_valid(t):
    return (t[1][0] + t[1][1]) > t[0]

def solve(inp):
    return len([t for t in inp if is_valid(t)])

def main():
    inp = read_input_p1("input.txt")
    print("The solution to part 1 is: {}".format(solve(inp)))
    inp = read_input_p2("input.txt")
    print("The solution to part 2 is: {}".format(solve(inp)))

def test_ex():
    assert build_tri(ex_numbers("12  56 34")) == (56, (12, 34))
    assert is_valid(build_tri(ex_numbers("5 10 25"))) == False
    inp = read_input_p2("input_ex.txt")
    assert inp[0] == (103, (101, 102))

def test_live():
    inp = read_input_p1("input.txt")
    assert solve(inp) == 862
    inp = read_input_p2("input.txt")
    assert solve(inp) == 1577

if __name__ == '__main__':
    main()
