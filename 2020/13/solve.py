def read_input(fname):
    with open(fname, "r") as handle:
        lines = [x.strip() for x in handle.readlines()]
        return {
            "earliest": int(lines[0]),
            "bus_ids": [int(x) for x in lines[1].split(",") if x != "x"],
            "all_bus_ids": lines[1].split(","),
        }


def get_next_bus_times(notes):
    bus = None
    curr_time = notes["earliest"]
    while bus is None:
        buses = [x for x in notes["bus_ids"] if curr_time % x == 0]
        if len(buses) > 0:
            bus = buses[0]
        else:
            curr_time += 1
    return (curr_time, bus)


def get_earliest_timestamp(bus_ids):
    ids = [(start, int(id)) for (start, id) in enumerate(bus_ids) if id != 'x']
    timestamp = 0
    mult = 1
    for (offset, bid) in ids:
        while (timestamp + offset) % bid != 0:
            timestamp += mult
        mult *= bid
    return timestamp


def solve_p1(inp):
    res = get_next_bus_times(inp)
    return (res[0] - inp["earliest"]) * res[1]


def solve_p2(inp):
    return get_earliest_timestamp(inp["all_bus_ids"])


def main():
    inp = read_input("input.txt")
    sln1 = solve_p1(inp)
    print("The solution to part 1 is: {}".format(sln1))
    sln2 = solve_p2(inp)
    print("The solution to part 2 is: {}".format(sln2))


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 174
    assert solve_p2(inp) == 780601154795940


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 295
    assert solve_p2(inp) == 1068781


if __name__ == "__main__":
    main()
