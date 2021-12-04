from functools import reduce
from itertools import product


def read_input(filename):
    with open(filename, "rt") as fhandle:
        return dict([parse(x.strip()) for x in fhandle.readlines()])


def parse(line):
    parts = line.split(":")
    ingredient = parts[0]
    properties = [parse_property(x.strip()) for x in parts[1].split(",")]
    return (ingredient, properties)


def parse_property(v):
    parts = v.split(" ")
    return (parts[0], int(parts[1]))


def calc_score(ings, coeffs, calories = None):
    # res = 1

    # Return early score of 0 if this combination does not result in
    # the required number of calories
    if calories is not None:
        c = sum([coeffs[idx] * ing[4][1] for idx, ing in enumerate(ings)])
        if c != calories:
            return 0

    # Return the product of all terms
    return reduce(

        # Multiply each term unless it is negative, in which case
        # multiply with 0
        lambda a, x: 0 if x < 0 else a * x,

        # Multiply each ingredient property (e.g. "texture" = 3) with
        # the corresponding coefficient and take the sum
        [
            sum([coeff * ings[idx][prop][1] for idx,coeff in enumerate(coeffs)])
            for prop in range(4)
        ],

        1
    )


def get_coefficients(num_items, max_num):
    return product(
        *(range(1, max_num + 1) for _ in range(num_items - 1))
    )


def solve_p1(inp):
    coefficients = get_coefficients(len(inp), 100)
    ing_list = list(inp.values())
    return max(
        [calc_score(ing_list, c + (100 - sum(c),)) for c in coefficients]
    )


def solve_p2(inp):
    coefficients = get_coefficients(len(inp), 100)
    ing_list = list(inp.values())
    return max(
        [calc_score(ing_list, c + (100 - sum(c),), 500) for c in coefficients]
    )


def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 62842880
    assert solve_p2(inp) == 57600000


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 18965440
    assert solve_p2(inp) == 15862900


if __name__ == "__main__":
    main()
