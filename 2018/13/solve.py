def get_input():
    with open("input.txt") as f:
        lines = [list(x) for x in f.read().split("\n") if len(x) > 0]
        return {
            "track": lines,
            "carts": extract_carts(lines),
            "ticks": 0,
        }

def get_example_input():
    inp = ""
    inp += r"/->-\        "  + "\n"
    inp +=  "|   |  /----\\" + "\n"
    inp += r"| /-+--+-\  |"  + "\n"
    inp += r"| | |  | v  |"  + "\n"
    inp += r"\-+-/  \-+--/"  + "\n"
    inp += r"  \------/   "  + "\n"
    lines = [list(x) for x in inp.split("\n") if len(x) > 0]
    return {
        "track": lines,
        "carts": extract_carts(lines),
        "ticks": 0,
    }

def get_example_input2():
    inp = ""
    inp += r"/>-<\  "  + "\n"
    inp += r"|   |  "  + "\n"
    inp +=  "| /<+-\\" + "\n"
    inp += r"| | | v"  + "\n"
    inp += r"\>+</ |"  + "\n"
    inp += r"  |   ^"  + "\n"
    inp += r"  \<->/"  + "\n"
    lines = [list(x) for x in inp.split("\n") if len(x) > 0]
    return {
        "track": lines,
        "carts": extract_carts(lines),
        "ticks": 0,
    }

def extract_carts(track):
    carts = list()
    for (y, row) in enumerate(track):
        for (x, col) in enumerate(row):
            if col == "^" or col == "v" or col == "<" or col == ">":
                carts.append({"x": x, "y": y, "direction": col, "turn": "LT"})
                if col == "^" or col == "v":
                    track[y][x] = "|"
                elif col == "<" or col == ">":
                    track[y][x] = "-"
    return carts

def intersection(cart):
    if cart["direction"] == "^":
        lt = "<"
        rt = ">"
    elif cart["direction"] == "v":
        lt = ">"
        rt = "<"
    elif cart["direction"] == "<":
        lt = "v"
        rt = "^"
    elif cart["direction"] == ">":
        lt = "^"
        rt = "v"
    if cart["turn"] == "LT":
        cart["direction"] = lt
        cart["turn"] = "NO"
    elif cart["turn"] == "NO":
        cart["turn"] = "RT"
    elif cart["turn"] == "RT":
        cart["direction"] = rt
        cart["turn"] = "LT"

def move_cart(state, cart):
    if cart["direction"] == "^":
        ch = state["track"][cart["y"] - 1][cart["x"]]
        if ch == "|":
            cart["y"] -= 1
        elif ch == "\\":
            cart["y"] -= 1
            cart["direction"] = "<"
        elif ch == "/":
            cart["y"] -= 1
            cart["direction"] = ">"
        elif ch == "+":
            cart["y"] -= 1
            intersection(cart)
    elif cart["direction"] == "v":
        ch = state["track"][cart["y"] + 1][cart["x"]]
        if ch == "|":
            cart["y"] += 1
        elif ch == "\\":
            cart["y"] += 1
            cart["direction"] = ">"
        elif ch == "/":
            cart["y"] += 1
            cart["direction"] = "<"
        elif ch == "+":
            cart["y"] += 1
            intersection(cart)
    elif cart["direction"] == "<":
        ch = state["track"][cart["y"]][cart["x"] - 1]
        if ch == "-":
            cart["x"] -= 1
        elif ch == "\\":
            cart["x"] -= 1
            cart["direction"] = "^"
        elif ch == "/":
            cart["x"] -= 1
            cart["direction"] = "v"
        elif ch == "+":
            cart["x"] -= 1
            intersection(cart)
    elif cart["direction"] == ">":
        ch = state["track"][cart["y"]][cart["x"] + 1]
        if ch == "-":
            cart["x"] += 1
        elif ch == "\\":
            cart["x"] += 1
            cart["direction"] = "v"
        elif ch == "/":
            cart["x"] += 1
            cart["direction"] = "^"
        elif ch == "+":
            cart["x"] += 1
            intersection(cart)

def sort_carts(state):
    ylen = len(state["track"][0])
    state["carts"].sort(key=lambda a: a["y"] * ylen + a["x"])

def tick(state, remove_crashes):
    crashes = list()

    sort_carts(state)

    # Tick all carts
    for cart in state["carts"]:
        move_cart(state, cart)

        # Check for crashes
        for c1 in state["carts"]:
            for c2 in state["carts"]:
                if c1 == c2:
                    continue
                if c1["x"] == c2["x"] and c1["y"] == c2["y"]:
                    crashes.append((c1, c2))

    if remove_crashes:
        state["carts"] = [x for x in state["carts"] if len([y for y in crashes if y[0] == x or y[1] == x]) == 0]

    state["ticks"] += 1

    return crashes

def output_track(state):
    for (y, row) in enumerate(state["track"]):
        for (x, col) in enumerate(row):
            cell_carts = list()
            for cart in state["carts"]:
                if cart["x"] == x and cart["y"] == y:
                    cell_carts.append(cart)
            ch = " "
            if len(cell_carts) > 1:
                ch = "X"
            elif len(cell_carts) == 1:
                ch = cell_carts[0]["direction"]
            else:
                ch = col

            print(ch, end="")
        print()

def run(inp1, inp2):
    res = list()

    # Find first crash
    state = inp1
    crashes = list()
    while len(crashes) == 0:
        crashes = tick(state, False)
    print("The first crash occurred at ({}, {}) after {} ticks".format(crashes[0][0]["x"], crashes[0][0]["y"], state["ticks"]))
    res.append(crashes[0])

    # Find position of last un-crashed cart
    state = inp2
    while len(state["carts"]) > 1:
        crashes = tick(state, True)
    if len(state["carts"]) == 0:
        print("No carts remain after all crashes")
    else:
        print("After all crashes, the last remaining cart is at ({}, {}) after {} ticks".format(state["carts"][0]["x"], state["carts"][0]["y"], state["ticks"]))
    res.append(state["carts"])

    return res


def run_examples():
    return run(get_example_input(), get_example_input2())

def run_full():
    return run(get_input(), get_input())


def test_examples():
    res = run_examples()

    # First crash: (7, 3)
    assert res[0][0]["x"] == 7
    assert res[0][0]["y"] == 3

    # Last remaining cart after all crashes: (6, 4)
    assert res[1][0]["x"] == 6
    assert res[1][0]["y"] == 4

def test_full():
    res = run_full()

    # First crash: (65, 73)
    assert res[0][0]["x"] == 65
    assert res[0][0]["y"] == 73

    # Last remaining cart after all crashes: (54, 66)
    assert res[1][0]["x"] == 54
    assert res[1][0]["y"] == 66


if __name__ == "__main__":
    run_examples()
    run_full()
