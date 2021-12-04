class Pad:
    PAD = [[]]
    pos = (0, 0)

    def move(self, ch):
        if ch == "U":
            self.pos = (self.pos[0], max(0, self.pos[1] - 1))
        elif ch == "D":
            self.pos = (self.pos[0], min(len(self.PAD) - 1, self.pos[1] + 1))
        elif ch == "L":
            self.pos = (max(0, self.pos[0] - 1), self.pos[1])
        elif ch == "R":
            self.pos = (min(len(self.PAD[0]) - 1, self.pos[0] + 1), self.pos[1])

    def get_char(self):
        return self.PAD[self.pos[1]][self.pos[0]]

    def run_line(self, line):
        for ch in line:
            self.move(ch)
        return self.get_char()
    
class Pad1(Pad):
    PAD = [
        ["1", "2", "3"],
        ["4", "5", "6"],
        ["7", "8", "9"],
    ]
    pos = (1, 1)

class Pad2(Pad):
    PAD = [
        [" ", " ", "1", " ", " "],
        [" ", "2", "3", "4", " "],
        ["5", "6", "7", "8", "9"],
        [" ", "A", "B", "C", " "],
        [" ", " ", "D", " ", " "],
    ]
    pos = (0, 2)

    def move(self, ch):
        old_pos = self.pos
        super().move(ch)
        if self.get_char() == " ":
            self.pos = old_pos

def read_input(fn):
    with open(fn, "rt") as fh:
        return [list(x.strip()) for x in fh.readlines()]

def solve(inp, pad):
    return "".join([pad.run_line(line) for line in inp])

def solve_p1(inp):
    return solve(inp, Pad1())

def solve_p2(inp):
    return solve(inp, Pad2())

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == "1985"
    assert solve_p2(inp) == "5DB3"

def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == "38961"
    assert solve_p2(inp) == "46C92"

if __name__ == '__main__':
    main()
