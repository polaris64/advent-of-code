def get_polymer():
    with open("input") as f:
        return list(f.read().strip())

def reduce(polymer):
    reduced_start = None
    reduced_end   = polymer
    skip = False
    while reduced_start != reduced_end:
        reduced_start = reduced_end
        removals = [False for x in list(range(len(reduced_start)))]
        for (i, ch) in enumerate(reduced_start):
            if skip:
                skip = False
                continue
            chn = reduced_start[i + 1] if i + 1 < len(reduced_start) else None
            if chn is not None:
                if ch.upper() == chn.upper() and ch != chn:
                    removals[i] = True
                    removals[i + 1] = True
                    skip = True
        reduced_end = [x for (i, x) in enumerate(reduced_start) if not removals[i]]

    return "".join(reduced_end)

def get_units(polymer):
    units = set()
    for ch in polymer:
        units.add(ch.upper())
    return units

# Test
#print(reduce(list("aA")))
#print(reduce(list("abBA")))
#print(reduce(list("abAB")))
#print(reduce(list("aabAAB")))
#print(reduce(list("dabAcCaCBAcCcaDA")))

polymer = get_polymer()
print("The length of the original polymer is: {}".format(len(polymer)))

reduced_polymer = reduce(polymer)
print("The length of the reduced polymer is: {}".format(len(reduced_polymer)))

units = get_units(polymer)
unit_lengths = dict()
for unit in units:
    p = [x for x in list(reduced_polymer) if x.upper() != unit]
    r = reduce(p)
    unit_lengths[unit] = len(r)

smallest_unit = min(unit_lengths.items(), key=lambda x: x[1])
print(
    "The smallest polymer length possible after removing a single unit type is {} (removing unit \"{}\")".format(
        smallest_unit[1],
        smallest_unit[0]
    )
)
