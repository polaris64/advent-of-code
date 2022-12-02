# Score for each player move
# X: rock, Y: paper, Z: scissors
P1_SCORES = {
    "X": 1,
    "Y": 2,
    "Z": 3,
}

# Scores for each round result
WIN = 6
LOSE = 0
DRAW = 3

# Main game rules (2nd item in tuple is the our move)
RESULTS = {
    ("A", "X"): DRAW,
    ("A", "Y"): WIN,
    ("A", "Z"): LOSE,
    ("B", "X"): LOSE,
    ("B", "Y"): DRAW,
    ("B", "Z"): WIN,
    ("C", "X"): WIN,
    ("C", "Y"): LOSE,
    ("C", "Z"): DRAW,
}

# Score and definition of part 2 codes
P2_SCORES = {
    "X": LOSE,
    "Y": DRAW,
    "Z": WIN,
}

# Generate part 2 rules
RULES = {
    rk: {
        x[0]: x[1]
        for x in RESULTS.keys()
        if RESULTS[x] == P2_SCORES[rk]
    }
    for rk in ["X", "Y", "Z"]
}


def read_input(filename):
    with open(filename, "r") as fh:
        return parse_input([x.strip() for x in fh.readlines()])


def parse_input(inp):
    return [(x[0], x[2]) for x in inp]


def score_round_p1(r):
    return P1_SCORES[r[1]] + RESULTS[r]


def score_round_p2(r):
    player_move = RULES[r[1]][r[0]]
    return P1_SCORES[player_move] + P2_SCORES[r[1]]


def solve_p1(inp):
    score = 0
    for r in inp:
        score += score_round_p1(r)
    return score


def solve_p2(inp):
    score = 0
    for r in inp:
        score += score_round_p2(r)
    return score


def main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    print(f"The solution to part 1 is: {res}")
    res = solve_p2(inp)
    print(f"The solution to part 2 is: {res}")


def test_ex():
    inp = read_input("input_ex.txt")
    res = solve_p1(inp)
    assert res == 15
    res = solve_p2(inp)
    assert res == 12


def test_main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    assert res == 11063
    res = solve_p2(inp)
    assert res == 10349


if __name__ == '__main__':
    main()
