import re

import processor

def get_input_p1():
    with open("input_p1.txt") as f:
        items = list()
        state = {"before": None, "after": None, "instr": None}
        for line in f.readlines():
            line = line.strip()
            if len(line) == 0:
                items.append((state["before"], state["instr"], state["after"]))
                state["before"] = None
                state["after"]  = None
                state["instr"]  = None
            elif line[:6] == "Before":
                match = re.match(r"^Before:\s*\[((\d+,?\s*){4})\]$", line)
                if match is not None and len(match.groups()) > 0:
                    state["before"] = tuple([int(x.strip()) for x in match.groups()[0].split(",")])
            elif line[:5] == "After":
                match = re.match(r"^After:\s*\[((\d+,?\s*){4})\]$", line)
                if match is not None and len(match.groups()) > 0:
                    state["after"] = tuple([int(x.strip()) for x in match.groups()[0].split(",")])
            else:
                state["instr"] = tuple([int(x) for x in line.split(" ")])
        if state["before"] is not None:
            items.append((state["before"], state["instr"], state["after"]))
        return items

def get_input_p2():
    with open("input_p2.txt") as f:
        return [tuple([int(y) for y in x.strip().split(" ")]) for x in f.readlines() if len(x.strip()) > 0]

def process_sample(samp, full=True):
    matches = list() if full else set()

    # Try each processor opcode in turn
    for (name, ex) in processor.opcodes.items():

        state = list(samp[0])

        # Execute the opcode and update state
        state[samp[1][3]] = ex(state, samp[1][1], samp[1][2])

        if state == list(samp[2]):
            if full:
                matches.append(name)
            else:
                matches.add(name)

    return (samp[1][0], matches)

def extract_opcodes(samples):
    # Build a dict of opcode number together with all names which could match
    opcodes = dict()
    for samp in samples:
        res = process_sample(samp, full=False)
        if res[0] in opcodes.keys():
            opcodes[res[0]] = opcodes[res[0]].intersection(res[1])
        else:
            opcodes[res[0]] = res[1]

    cont = True
    while cont:
        cont = False
        definites = dict()

        # Get each number with only one matching opcode name (definite match)
        for (number, names) in opcodes.items():
            if len(names) == 1:
                definites[number] = list(names)[0]

        # Remove all definite matches from other opcodes
        if len(definites.keys()) > 0 and len(definites.keys()) < len(opcodes.keys()):
            cont = True
            for (number, names) in opcodes.items():
                opcodes[number] = names.difference(set([x[1] for x in definites.items() if x[0] != number]))

    return {x[0]:list(x[1])[0] for x in opcodes.items() if len(x[1]) > 0}

def execute_program(program, opcodes):
    state = [0, 0, 0, 0]
    for instr in program:
        opcode = opcodes.get(instr[0])
        if opcode is not None:
            ex = processor.opcodes[opcode]
            if ex is not None:
                state[instr[3]] = ex(state, instr[1], instr[2])
    return state


def run(samples, program):
    more_than_3_matches = list()
    for samp in samples:
        res = process_sample(samp, full=True)
        if len(res[1]) >= 3:
            more_than_3_matches.append(res[0])

    print("Number of samples which match 3 or more opcodes: {}".format(len(more_than_3_matches)))

    opcodes = extract_opcodes(samples)
    print("The following opcode mapping was found: {}".format(repr(opcodes)))

    # Execute program from part 2 with discovered opcodes
    state = execute_program(program, opcodes)

    print("The state after execution of the program is: {}".format(repr(state)))

    return (len(more_than_3_matches), state)

def run_examples():
    samples = [
        ((3, 2, 1, 1), (9, 2, 1, 2), (3, 2, 2, 1)),
        ((3, 2, 1, 1), (9, 1, 0, 2), (3, 2, 6, 1)),
    ]
    return run(samples, [(9, 1, 0, 0)])

def run_full():
    return run(get_input_p1(), get_input_p2())

def test_examples():
    res = run_examples()
    assert res[0] == 1
    assert res[1] == [0, 0, 0, 0]

def test_full():
    res = run_full()
    assert res[0] == 529
    assert res[1] == [573, 573, 1, 1]


if __name__ == "__main__":
    run_examples()
    run_full()
