from hashlib import md5

SECRET = "iwrupvqb"


def find_suffix(secret, prefix_len):
    curr_num = 0
    prefix = "0" * prefix_len
    secret_b = bytes(secret, "ascii")
    while True:
        digest = md5(secret_b + str(curr_num).encode("ascii")).hexdigest()
        if digest.startswith(prefix):
            return (curr_num, digest)
        curr_num += 1
    return None


def solve_p1(secret):
    return find_suffix(secret, 5)[0]


def solve_p2(secret):
    return find_suffix(secret, 6)[0]


def main():
    print("The solution to part 1 is: {}".format(solve_p1(SECRET)))
    print("The solution to part 2 is: {}".format(solve_p2(SECRET)))


def test_main():
    assert solve_p1(SECRET) == 346386
    assert solve_p2(SECRET) == 9958218


def test_ex():
    assert solve_p1("abcdef") == 609043
    assert solve_p1("pqrstuv") == 1048970


if __name__ == "__main__":
    main()
