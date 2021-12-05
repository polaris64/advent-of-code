import time
from collections import defaultdict

import intcode

def run_row(prog, row, xs, xe):
        print("run_row()", row, xs, xe)
        context = {
                "output": [0 for x in range(xs, xe + 1)],
                "curr_pos": [xs, row],
                "input_axis": 0,
        }

        def handle_input():
                axis = context["input_axis"]
                res = context["curr_pos"][axis]
                #print("Axis {}: {}".format(axis, res))
                context["input_axis"] = (context["input_axis"] + 1) % 2
                return res

        def handle_output(x):
                coord = context["curr_pos"]
                context["output"][coord[0] - xs] = x

                context["curr_pos"][0] += 1

        while context["curr_pos"][0] < xe:
                vm_prog = intcode.load_program(prog.copy())
                intcode.run_until_halt(vm_prog, handle_input, handle_output)

        if 1 in context["output"]:
                first_one = xs + context["output"].index(1)
                last_one = xs + len(context["output"]) - context["output"][::-1].index(1)
        else:
                first_one = 0
                last_one = 0

        return (context["output"], first_one, last_one)

def run_program_step1(prog):
    grid = []
    last_s = 0
    last_e = 10
    for y in range(50):
        res = run_row(prog, y, last_s, last_e)
        grid.append(
                [
                    *[0 for x in range(last_s)],
                    *res[0],
                    *[0 for x in range(last_s + len(res[0]), 50)]
                ]
        )
        last_s = res[1]
        last_e = res[2] + 5
        last_e = min(50, last_e)
    return grid


def run_program_step2(prog):
    grid = []
    #row = 650
    row = 1070
    #run_row() 1082 619 785
    # 675 * 10000 + 1082
    # = 6761082
    # = 6751082 #HIGH?
    # = 6741082 #LOW
    #n_row() 1089 623 790
    #158
    #run_row() 1189 680 681
    #680 *10000 + 1189 = 6801089
    last_s = int(row / 3)
    last_e = last_s + 1000
    while True:
        print(row)
        res = run_row(prog, row, last_s, last_e + 10)
        last_s = res[1]
        last_e = res[2]
        l = count_ones_row(res[0])
        print(l)
        if l >= 100:
            res = run_row(prog, row + 100, last_e - 99, last_e - 98)
            l = count_ones_row(res[0])
            print(len(res[0]), l)
            if l >= 1:
                break
        row +=1
    return grid

        # TODO: implement algorithm
        # 1. Check output from step1 and estimate Y coordinate where width of beam is just less than 100
        # 2. Start running program from Y coordinate from step1
        # 3. Store first Y that has a width of 100
        # 4. Extrapolate or continue running program from Y in 3.

def render_grid(grid):
        output = ""
        for row in grid:
                for cell in row:
                        if cell == 1:
                                output += "#"
                        else:
                                output += "."
                output += "\n"
        return output

def count_ones_row(row):
        return len([x for x in row if x == 1])

def count_ones(grid):
        return sum([len([cell for cell in row if cell == 1]) for row in grid])


def get_input():
        with open("input.txt", "rt") as f:
                return [int(x.strip()) for x in f.read().split(",") if len(x)]

def step1():
        prog = get_input()
        grid = run_program_step1(prog)
        res = count_ones(grid)
        return res

def step2():
        prog = get_input()
        #grid = run_program_step1(prog)
        #print(len([x for x in grid[9] if x == 1]))
        #print(len([x for x in grid[-1] if x == 1]))
        # 1 on row 9, 7 on row 49
        # 6 / 40 = 0.15
        # 6.666667
        return run_program_step2(prog)
        # 782 and 882

def main():
        #print("Number of positions showing as in range of tractor beam over 50x50 grid: {}".format(step1()))
        print("Step 2: {}".format(step2()))


if __name__ == "__main__":
        main()
