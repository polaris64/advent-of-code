# NOTE: Run with pypy for decent speed

import math
import re


ROW_EXAMPLE = 10
ROW_MAIN = 2000000
MAX_EXAMPLE = 20
MAX_MAIN = 4000000
X_MUL = 4000000


def read_input(filename):
    with open(filename, "r") as fh:
        return [parse_line(line.strip()) for line in fh.readlines()]


def parse_line(line):
    matches = re.search(
        r"^Sensor at x=(-?[0-9]+), y=(-?[0-9]+): "
        r"closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)$",
        line
    )
    assert matches is not None
    groups = matches.groups()
    assert len(groups) == 4
    return (
        (int(groups[0]), int(groups[1])),
        (int(groups[2]), int(groups[3])),
    )


def dist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])


def generate_grid(inp):
    ctx = {
        "empty": set(),
        "inp": inp,
        "sensors": sorted([a for a, _ in inp]),
        "beacons": sorted([b for _, b in inp]),
    }
    return ctx


def get_empty_cells_on_row(inp, row):
    ctx = generate_grid(inp)

    min_x = min([x for x, _ in ctx["sensors"]])
    min_x_beacons = min([x for x, _ in ctx["beacons"]])
    if min_x_beacons < min_x:
        min_x = min_x_beacons
    max_x = max([x for x, _ in ctx["sensors"]])
    max_x_beacons = max([x for x, _ in ctx["beacons"]])
    if max_x_beacons > max_x:
        max_x = max_x_beacons

    max_x += max(dist(s, b) for s, b in ctx["inp"])

    count = 0
    x = min_x
    while x <= max_x:
        xdiff = 1
        for sensor, beacon in ctx["inp"]:
            if x == sensor[0] and sensor[1] == row:
                continue
            if x == beacon[0] and beacon[1] == row:
                continue
            if dist((x, row), sensor) <= dist(sensor, beacon):

                # Count all of the cells within the sensor's span on
                # this row as they must be empty. Skip over the count
                # and continue from the next cell onwards.
                span_width = dist(sensor, beacon) * 2 + 1
                offset = abs(sensor[1] - row)
                span_width_row = span_width - (offset * 2)
                span_start = math.ceil(sensor[0] - span_width_row / 2)
                xdiff = span_width_row - (x - span_start)
                count += xdiff

                break

        x += xdiff

    return count - 1


def get_possible_cell_in_row(ctx, row, max_pos):
    x = 0
    while x < max_pos:
        empty = False
        xdiff = 1

        for sensor, beacon in ctx["inp"]:
            if x == sensor[0] and sensor[1] == row:
                empty = True
                break
            if x == beacon[0] and beacon[1] == row:
                empty = True
                break
            if dist((x, row), sensor) <= dist(sensor, beacon):
                empty = True

                # Skip over this sensor's span width on this row as
                # the entire span must be empty
                span_width = dist(sensor, beacon) * 2 + 1
                offset = abs(sensor[1] - row)
                span_width_row = span_width - (offset * 2)
                span_start = math.ceil(sensor[0] - span_width_row / 2)
                xdiff = span_width_row - (x - span_start)

                break

        if not empty:
            return (x, row)
        x += xdiff

    return None


def solve_p1(inp, row):
    return get_empty_cells_on_row(inp, row)


def solve_p2(inp, max_pos):
    ctx = generate_grid(inp)
    for row in range(max_pos + 1):
        pt = get_possible_cell_in_row(ctx, row, max_pos)
        if pt is not None:
            return pt[0] * X_MUL + pt[1]


def main():
    inp = read_input("input.txt")
    print(f"The solution to part 1 is: {solve_p1(inp, ROW_MAIN)}")
    print(f"The solution to part 2 is: {solve_p2(inp, MAX_MAIN)}")


def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp, ROW_EXAMPLE) == 26
    assert solve_p2(inp, MAX_EXAMPLE) == 56000011


def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp, ROW_MAIN) == 4724228
    assert solve_p2(inp, MAX_MAIN) == 13622251246513


if __name__ == '__main__':
    main()
