import numpy as np

def get_power_level(x, y, serial):
    rack_id = x + 10
    power = rack_id * y
    power += serial
    power *= rack_id
    return (int(power / 100) % 10) - 5

def build_grid(serial):
    grid = np.zeros(shape=(300, 300), dtype=np.int32)
    for y in range(300):
        for x in range(300):
            grid[x,y] = get_power_level(x + 1, y + 1, serial)
    return grid

def sum_power(x, y, size, grid):
    return np.sum(grid[x:x+size, y:y+size])

def get_max_for_window(grid, window_size):
    max_power = None
    for y in range(300 - window_size + 1):
        for x in range(300 - window_size + 1):
            s = sum_power(x, y, window_size, grid)
            if max_power is None or s > max_power[0]:
                max_power = (s, x + 1, y + 1)
    return max_power

def run(serial, size=None):
    grid = build_grid(serial)
    if size is None:
        max_all = None
        for i in range(1, 300):
            max_power = get_max_for_window(grid, i)
            if max_all is None or max_power[0] > max_all[0]:
                max_all = max_power
                print(
                    "The current maximum power ({}) is seen at ({}, {}) for window size {}".format(
                        max_all[0],
                        max_all[1],
                        max_all[2],
                        i,
                    ),
                    flush=True,
                )
        return max_all
    else:
        return get_max_for_window(grid, size)

def run_examples():
    return [
        get_power_level(3, 5, 8),
        get_power_level(122, 79, 57),
        get_power_level(217, 196, 39),
        get_power_level(101, 153, 71),
        run(18, 3),
        run(42, 3),
    ]

def run_full():
    serial = 7511
    run(serial)
    return run(serial, 3)
    # Max (size 13): (135, 236, 287) 

def test_examples():
    res = run_examples()
    assert res[0] == 4
    assert res[1] == -5
    assert res[2] == 0
    assert res[3] == 4
    assert res[4] == (29, 33, 45)
    assert res[5] == (30, 21, 61)

def test_full():
    res = run_full()
    assert res == (34, 21, 22)


if __name__ == "__main__":
    #run_examples()
    run_full()
