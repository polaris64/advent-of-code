def read_input(filename):
    with open(filename, "r") as fh:
        return [x.strip() for x in fh.readlines()]


def build_structure(inp):
    ctx = {
        "cmd": None,
        "pwd": [],
        "dirs": {
            "/": {
                "dirs": {},
                "files": [],
            }
        },
    }
    for line in inp:
        if line[0] == "$":
            ctx["cmd"] = None
            if line[2:4] == "cd":
                dirname = line[5:]
                if dirname == "/":
                    ctx["pwd"] = []
                elif dirname == "..":
                    ctx["pwd"] = ctx["pwd"][0:-1]
                else:
                    new_dir = add_directory(ctx, dirname)
                    ctx["pwd"] = new_dir
            elif line[2:4] == "ls":
                ctx["cmd"] = "ls"
        else:
            if ctx["cmd"] == "ls":
                add_entry(ctx, line)

    return ctx


def get_pwd(ctx):
    subdir = ctx["dirs"]["/"]
    for d in ctx["pwd"]:
        subdir = subdir["dirs"][d]
    return subdir

    
def add_directory(ctx, dirname):
    subdir = get_pwd(ctx)
    subdir["dirs"][dirname] = {"dirs": {}, "files": []}
    return ctx["pwd"] + [dirname]


def add_entry(ctx, line):
    parts = line.split(" ")
    if parts[0] == "dir":
        add_directory(ctx, parts[1])
    else:
        subdir = get_pwd(ctx)
        subdir["files"].append((parts[1], int(parts[0])))


def get_directories(dirs):
    for k, v in dirs.items():
        yield from get_directories(v["dirs"])
        yield v


def get_size(directory):
    res = 0
    if "files" in directory and len(directory["files"]) > 0:
        res += sum([f[1] for f in directory["files"]])
    return res + sum([get_size(directory["dirs"][d]) for d in directory["dirs"]])


def solve_p1(inp):
    structure = build_structure(inp)
    dirs = [get_size(d) for d in get_directories(structure["dirs"])]
    return sum([ds for ds in dirs if ds <= 100000])


def solve_p2(inp):
    structure = build_structure(inp)
    free_space = 70000000 - get_size(structure["dirs"]["/"])
    dirs = [get_size(d) for d in get_directories(structure["dirs"])]
    return min([ds for ds in dirs if free_space + ds >= 30000000])


def main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    print(f"The solution to part 1 is: {res}")
    res = solve_p2(inp)
    print(f"The solution to part 2 is: {res}")


def test_ex():
    inp = read_input("input_ex.txt")
    res = solve_p1(inp)
    assert res == 95437
    res = solve_p2(inp)
    assert res == 24933642


def test_main():
    inp = read_input("input.txt")
    res = solve_p1(inp)
    assert res == 1084134
    res = solve_p2(inp)
    assert res == 6183184


if __name__ == '__main__':
    main()
