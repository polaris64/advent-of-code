def read_input(fname):
    with open(fname, "r") as handle:
        return [x.strip() for x in handle.readlines()]


def single_char(inp, matches):
    for idx, ch in enumerate(inp):
        if ch == " ":
            continue
        if ch in matches:
            return (ch, inp[idx + 1:])
        else:
            return (None, inp)
    return (None, inp)


def number(inp):
    res = ""
    last_idx = 0
    for idx, ch in enumerate(inp):
        if ch == " ":
            continue
        if ch.isnumeric():
            res += ch
        else:
            break
        last_idx = idx
    if res.isnumeric():
        return (int(res), inp[last_idx + 1:])
    else:
        return (None, inp)


def operator(inp):
    return single_char(inp, ["+", "-", "*", "/"])


def eval_expr(inp):
    acc = 0
    rest = inp

    oparen, rest = single_char(rest, ["("])
    if oparen is not None:
        acc, rest = eval_expr(rest)
        _, rest = single_char(rest, [")"])
    else:
        num, rest = number(rest)
        if num is None:
            return (None, inp)
        acc = num

    while True:
        op, rest = operator(rest)
        if op is None:
            break
        oparen, rest = single_char(rest, ["("])
        if oparen is not None:
            num, rest = eval_expr(rest)
            _, rest = single_char(rest, [")"])
        else:
            num, rest = number(rest)
            if num is None:
                break
        acc = eval_op(acc, op, num)

    return (acc, rest)


def eval_op(n1, op, n2):
    if op == "+":
        return n1 + n2
    if op == "-":
        return n1 - n2
    if op == "*":
        return n1 * n2
    if op == "/":
        return n1 / n2
    return None


def ast_expr(inp):
    rest = inp
    res, rest = ast_prod(rest)
    if res is not None:
        return (res, rest)
    res, rest = ast_sum(rest)
    if res is not None:
        return (res, rest)
    return (None, inp)


def ast_prod(inp):
    rest = inp
    res_n1, rest = ast_sum(rest)
    if res_n1 is None:
        res_n1, rest = ast_term(rest)
        if res_n1 is None:
            return (None, inp)
    res_op, rest = single_char(rest, ["*", "/"])
    if res_op is None:
        return (None, inp)
    res_n2, rest = ast_prod(rest)
    if res_n2 is None:
        res_n2, rest = ast_sum(rest)
        if res_n2 is None:
            res_n2, rest = ast_term(rest)
            if res_n2 is None:
                return (None, inp)
    return (("binop", res_n1, res_op, res_n2), rest)


def ast_sum(inp):
    rest = inp
    res_n1, rest = ast_term(rest)
    if res_n1 is None:
        return (None, inp)
    res_op, rest = single_char(rest, ["+", "-"])
    if res_op is None:
        return (None, inp)
    res_n2, rest = ast_sum(rest)
    if res_n2 is None:
        res_n2, rest = ast_term(rest)
        if res_n2 is None:
            return (None, inp)
    return (("binop", res_n1, res_op, res_n2), rest)


def ast_subexpr(inp):
    rest = inp
    res, rest = single_char(rest, ["("])
    if res is None:
        return (None, inp)
    res_expr, rest = ast_expr(rest)
    if res_expr is None:
        return (None, inp)
    res, rest = single_char(rest, [")"])
    if res is None:
        return (None, inp)
    return (res_expr, rest)


def ast_term(inp):
    rest = inp
    res, rest = ast_subexpr(inp)
    if res is not None:
        return (res, rest)
    res, rest = number(rest)
    if res is not None:
        return (res, rest)
    return (None, inp)


def eval_ast(ast):
    if type(ast) == int:
        return ast
    if ast[0] == "binop":
        return eval_op(eval_ast(ast[1]), ast[2], eval_ast(ast[3]))
    return None


def parse_and_eval(expr):
    ast = ast_expr(expr)
    if ast[1] != "":
        return None
    return eval_ast(ast[0])


def solve_p1(inp):
    return sum(eval_expr(x)[0] for x in inp)


def solve_p2(inp):
    return sum(parse_and_eval(x) for x in inp)


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 30753705453324
    assert solve_p2(inp) == 244817530095503


def test_ex():
    inputs = [
        ("1 + 2 * 3 + 4 * 5 + 6", 71),
        ("1 + (2 * 3) + (4 * (5 + 6))", 51),
        ("2 * 3 + (4 * 5)", 26),
        ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437),
        ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240),
        ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632),
    ]
    assert solve_p1(
        [expr for expr, _ in inputs]
    ) == sum([res for _, res in inputs])
    inputs = [
        ("1 + 2 * 3 + 4 * 5 + 6", 231),
        ("1 + (2 * 3) + (4 * (5 + 6))", 51),
        ("2 * 3 + (4 * 5)", 46),
        ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445),
        ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060),
        ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340),
    ]
    assert solve_p2(
        [expr for expr, _ in inputs]
    ) == sum([res for _, res in inputs])


if __name__ == "__main__":
    main()
