from collections import deque
from functools import reduce

def get_input():
    return {
        "initial_state": "####....#...######.###.#...##....#.###.#.###.......###.##..##........##..#.#.#..##.##...####.#..##.#",
        "rules": [
            ("..#..", "."),
            ("#.#.#", "#"),
            ("#.###", "#"),
            (".##..", "."),
            ("#.#..", "#"),
            (".#.#.", "#"),
            (".###.", "#"),
            (".####", "#"),
            ("##...", "#"),
            ("#.##.", "#"),
            ("#..##", "#"),
            ("....#", "."),
            ("###.#", "."),
            ("#####", "#"),
            (".....", "."),
            ("..#.#", "."),
            (".#...", "#"),
            ("##.#.", "."),
            (".#.##", "#"),
            ("..##.", "."),
            ("#...#", "."),
            ("##.##", "#"),
            ("...#.", "."),
            ("#..#.", "."),
            ("..###", "."),
            (".##.#", "."),
            ("#....", "."),
            (".#..#", "#"),
            ("####.", "."),
            ("...##", "#"),
            ("##..#", "."),
            ("###..", "."),
        ],
    }

def get_example_input():
    return {
        "initial_state": "#..#.#..##......###...###",
        "rules": [
            ("...##", "#"),
            ("..#..", "#"),
            (".#...", "#"),
            (".#.#.", "#"),
            (".#.##", "#"),
            (".##..", "#"),
            (".####", "#"),
            ("#.#.#", "#"),
            ("#.###", "#"),
            ("##.#.", "#"),
            ("##.##", "#"),
            ("###..", "#"),
            ("###.#", "#"),
            ("####.", "#"),
        ],
    }

def get_slice(string, index):
    return (".." + string + "..")[index:index+5]

def rule_outcome(state_slice, rules):
    res = None
    for rule in rules:
        if state_slice == rule[0]:
            res = rule[1]
    if res is None:
        res = "."
    return res

def run_generation(initial_state, rules, zero_offset):
    new_state = ""
    zero_offset += 5
    for (i, ch) in enumerate(list("...." + initial_state + ".....")):
        new_state += rule_outcome(get_slice("....." + initial_state + ".....", i), rules)

    chop = True
    while chop:
        if new_state[0] == ".":
            new_state = new_state[1:]
            zero_offset -= 1
        #if new_state[0:5] == ".....":
        #    new_state = new_state[5:]
        #    zero_offset -= 5
        else:
            chop = False

    chop = True
    while chop:
        if new_state[-5:] == ".....":
            new_state = new_state[0:-5]
        else:
            chop = False

    return (new_state, zero_offset)

def get_pot_sum(state, zero_offset):
    return reduce(lambda a, x: a + (x[0] - zero_offset if x[1] == "#" else 0), enumerate(list(state)), 0)

def run(inp, generations):
    state = inp["initial_state"]
    zero_offset = 0

    # Create a ring buffer to look for consistent changes in sums across
    # generations
    ring_buffer = deque(maxlen=10)

    # Store (diff, generation, sum) for most recent change in sum
    last_diff_change = (0, None, None)

    # Keep track of generations run
    gens_run = 0

    for i in range(generations):
        new_gen = run_generation(state, inp["rules"], zero_offset)

        # Get difference in sums between previous and current generation
        new_diff = get_pot_sum(new_gen[0], new_gen[1]) - get_pot_sum(state, zero_offset)

        # If diff has changed
        if new_diff != last_diff_change[0]:
            # print(
            #     "On generation {} the difference changed from {} to {} (current generation's sum: {})".format(
            #         i,
            #         last_diff_change[0],
            #         new_diff,
            #         get_pot_sum(new_gen[0], new_gen[1])
            #     ),
            #     flush=True,
            # )
            last_diff_change = (new_diff, i, get_pot_sum(new_gen[0], new_gen[1]))

        # Check if ring buffer is full of the same diff: if so then we can say
        # the diff is consistent and extrapolate the final result for all
        # generations
        if len(ring_buffer) == 10 and len(set(ring_buffer)) == 1:
            print("The last 10 diffs were all {}, breaking...".format(ring_buffer[0]))
            break

        ring_buffer.append(new_diff)

        # Update current state
        state = new_gen[0]
        zero_offset = new_gen[1]

        gens_run += 1

    final_sum = None

    # If all generations were run
    if gens_run == generations:
        final_sum = get_pot_sum(state, zero_offset)
        print(
            "The final state after {} generations is \"{}\" and the sum of all pots containing a plant is: {}".format(
                gens_run,
                state,
                final_sum,
            ),
            flush=True
        )
    else:
        # Calculate the final recorded diff, plus the diff per generation
        # multiplied by the number of remaining generations
        final_sum = last_diff_change[2] + ((generations - last_diff_change[1] - 1) * last_diff_change[0])
        print(
            "Because of a consistent change in sums, the extrapolated final sum for {} generations would be: {}".format(
                generations,
                final_sum,
            )
        )

    return final_sum


def run_examples():
    return run(get_example_input(), 20)

def run_full():
    inp = get_input()
    return [
        run(inp, 20),
        run(inp, 50000000000),
    ]


def test_examples():
    res = run_examples()
    assert res == 325

def test_full():
    res = run_full()
    assert res[0] == 2063
    assert res[1] == 1600000000328


if __name__ == "__main__":
    run_examples()
    run_full()
