from functools import reduce

def step(rlist, chefs):
    current = str(reduce(lambda a, x: a + x, [rlist[x] for x in chefs], 0))
    for ch in current:
        rlist.append(int(ch))
    for (i, idx) in enumerate(chefs):
        chefs[i] = (idx + rlist[idx] + 1) % len(rlist)

def find_sublist(lhaystack, lneedle):
    for h in range(len(lhaystack)):
        if lhaystack[h] == lneedle[0] and lhaystack[h:h+len(lneedle)] == lneedle:
            return h
    return -1


def run_step1(recipes):
    rlist = [3, 7]
    chefs = [0, 1]
    start_idx = None
    while len(rlist) < recipes + 10:
        step(rlist, chefs)
    scores = "".join([str(x) for x in rlist[recipes:recipes+10]])
    print("The scores of the next 10 recipes after the first {} is: {}".format(recipes, scores))
    return scores

def run_step2(search_str, chunk_size):
    search_str = [int(x) for x in list(search_str)]
    rlist = [3, 7]
    chefs = [0, 1]
    idx = -1
    last_len = 2
    while idx == -1:
        for i in range(chunk_size):
            step(rlist, chefs)
        if len(rlist) >= len(search_str):
            idx = find_sublist(rlist[last_len - 1:], search_str)
            if idx > -1:
                idx += last_len - 1
            last_len = len(rlist) - len(search_str)
        #print(last_len)
    print(
        "The number of recipes before recipes with scores {} is: {}".format(
            "".join([str(x) for x in search_str]),
            idx,
        )
    )
    return idx


def run_examples():
    return [
        [run_step1(9), run_step1(5), run_step1(18), run_step1(2018)],
        [
            run_step2("51589", 10),
            run_step2("01245", 10),
            run_step2("92510", 10),
            run_step2("59414", 10),
        ],
    ]

def run_full():
    return [run_step1(165061), run_step2("165061", 100)]
    # "16": 14
    # "165": 313
    # "1650": 47670
    # "16506": 2805000
    # "165061": 20181148


def test_examples():
    res = run_examples()
    assert res[0][0] == "5158916779"
    assert res[0][1] == "0124515891"
    assert res[0][2] == "9251071085"
    assert res[0][3] == "5941429882"
    assert res[1][0] == 9
    assert res[1][1] == 5
    assert res[1][2] == 18
    assert res[1][3] == 2018

def test_full():
    res = run_full()
    assert res[0] == "5992684592"
    assert res[1] == 20181148


if __name__ == "__main__":
    run_examples()
    run_full()
