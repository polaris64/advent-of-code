from functools import reduce

def reducer(acc, val):
    op  = val[0]
    num = int(val[1:])
    if op == "+":
        return acc + num
    else:
        return acc - num

with open("input.txt", "r") as f:
    input_data = [x.strip() for x in f.readlines()]


    # Find resulting frequency
    final_freq = reduce(reducer, input_data, 0)
    print("Resulting frequency: {}".format(final_freq))


    # Find first frequency that is reached twice by repeatedly reducing
    # the list
    found = False
    found_set = set()
    curr_freq = 0

    # Keep loping over list until a frequency is found
    while not found:

        for freq in input_data:

            # Add current frequency to set
            found_set.add(curr_freq)

            # Get next frequency
            curr_freq = reducer(curr_freq, freq)

            # If curr_freq is already in the set, it has been found twice
            # and is the result
            if curr_freq in found_set:
                found = True
                break


    print("The first frequency to occur twice is: {}".format(curr_freq))
