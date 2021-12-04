def read_input(filename):
    with open(filename, "rt") as fhandle:
        return [parse(x.strip()) for x in fhandle.readlines()]


def parse(line):
    parts = line.split(" ")
    name = parts[0]
    fly_speed = int(parts[3])
    fly_duration = int(parts[6])
    rest_duration = int(parts[13])
    return {
        "counter": fly_duration,
        "distance": 0,
        "fly_duration": fly_duration,
        "fly_speed": fly_speed,
        "name": name,
        "points": 0,
        "rest_duration": rest_duration,
        "state": "FLY",
    }


def simulate(inp, seconds):
    inp = [x.copy() for x in inp]
    for _ in range(seconds):
        for v in inp:
            if v["state"] == "FLY":
                v["distance"] += v["fly_speed"]
            v["counter"] -= 1
            if v["counter"] <= 0:
                if v["state"] == "FLY":
                    v["state"] = "REST"
                    v["counter"] = v["rest_duration"]
                elif v["state"] == "REST":
                    v["state"] = "FLY"
                    v["counter"] = v["fly_duration"]

        # Assign points to reindeer in he lead
        furthest = max(inp, key=lambda x: x["distance"])
        for v in inp:
            if v["distance"] == furthest["distance"]:
                v["points"] += 1

    return inp


def solve_p1(inp, seconds):
    res = simulate(inp, seconds)
    return max(res, key=lambda x: x["distance"])["distance"]


def solve_p2(inp, seconds):
    res = simulate(inp, seconds)
    return max(res, key=lambda x: x["points"])["points"]


def main():
    inp = read_input("input.txt")
    time_limit = 2503
    print("The solution to part 1 is: {}".format(solve_p1(inp, time_limit)))
    print("The solution to part 2 is: {}".format(solve_p2(inp, time_limit)))


def test_ex():
    inp = read_input("input_ex.txt")
    time_limit = 1000
    assert solve_p1(inp, time_limit) == 1120
    assert solve_p2(inp, time_limit) == 689


def test_main():
    inp = read_input("input.txt")
    time_limit = 2503
    assert solve_p1(inp, time_limit) == 2696
    assert solve_p2(inp, time_limit) == 1084


if __name__ == "__main__":
    main()
