import re


def read_input(fn):
    with open(fn, "rt") as fh:
        return [parse_line(l.strip()) for l in fh.readlines()]


def parse_line(line):
    match = re.search(r"^Card ([^:]+): ([^|]+)\| (.+)", line)
    if match:
        groups = match.groups()
        return (
            int(groups[0]),
            [int(x) for x in groups[1].split()],
            [int(x) for x in groups[2].split()],
        )


def get_winning_numbers(card):
    for n in card[2]:
        if n in card[1]:
            yield n

    
def get_card_score(card):
    score = 0
    for n in get_winning_numbers(card):
        if score == 0:
            score = 1
        else:
            score *= 2
    return score


def generate_cards(inp):
    # NOTE: This could be optimised to be analytical
    cards = {x[0]: (x[1], x[2]) for x in inp}
    res = inp.copy()
    for card in res:
        num_matches = len(list(get_winning_numbers(card)))
        for new in range(card[0] + 1, card[0] + num_matches + 1):
            c = cards[new]
            res.append((new, c[0], c[1]))
    return res



def solve_p1(inp):
    return sum([get_card_score(c) for c in inp])


def solve_p2(inp):
    return len(generate_cards(inp))


def test_live():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 20667
    assert solve_p2(inp) == 5833065



def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 13
    assert solve_p2(inp) == 30


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp)}")
    print(f"The solution to part 2 is: {solve_p2(inp)}")


if __name__ == "__main__":
    main()
