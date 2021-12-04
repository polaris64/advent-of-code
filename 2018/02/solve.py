def count_letters(string):
    """Given a string, returns a list of all distinct letters together with the
    number of times they repeat within the string

    Args:
      - string

    Return: dict of (letter, count)
    """
    letters = dict()
    for x in string:
        if x in letters.keys():
            letters[x] += 1
        else:
            letters[x] = 1
    return letters

def count_num_repeats(strings, num):
    """Given a list of strings, counts the number of string that have letters
    repeating num times

    Args:
      - strings: list of strings
      - num:     number of times a letter must repeat in a string

    Return: number of strings that match
    """
    count = 0
    for string in input_data:
        letters = count_letters(string)
        match_num = [k for (k,v) in letters.items() if v == num]
        if len(match_num) > 0:
            count += 1
    return count

def get_difference(s1, s2):
    """Given two strings of the same length, finds the number of characters in
    the same place that differ and returns a tuple of that number and the
    string with only matching characters

    Args:
      - s1: First string
      - s2: Second string

    Return: tuple (<size_diff>, <matching_chars_str>)
    """

    # Zip both string character lists and keep only items where both match
    matching = [x for x in zip(list(s1), list(s2)) if x[0] == x[1]]

    return (
        # Difference in size between strings
        len(s1) - len(matching),

        # All matching characters as a str
        "".join([x[0] for x in matching])
    )


with open("input.txt", "rt") as f:
    input_data = [x.strip() for x in f.readlines()]

    # Count strings which have any letter exactly twice
    two_count   = count_num_repeats(input_data, 2)
    three_count = count_num_repeats(input_data, 3)

    print("The checksum is {} * {} = {}".format(two_count, three_count, two_count * three_count))


    # Find the first two strings that differ by only one character at the same
    # position in both strings.  When found, keep only the same characters in
    # both.
    final_code = None

    # Loop through input strings (index, str)
    for (i, s1) in enumerate(input_data):

        # Loop through slice of all input strings after current
        for s2 in input_data[i + 1:]:

            # Get (num_diffs, same_chars) for these strings
            diff = get_difference(s1, s2)

            # Must have only one difference
            if diff[0] == 1:
                final_code = diff[1]
                break

    print(
        "The common letters between the box IDs with only one differing character at the same position are: {}".format(
            final_code,
        )
    )
