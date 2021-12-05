import time
from collections import defaultdict
from multiprocessing import Array, Pool

import intcode

PROG = None
GRID = None

def pool_init(prog):
	global PROG
	PROG = prog.copy()

def run_row(row):
	global GRID, PROG

	context = {
		"curr_pos": [0, row],
		"input_axis": 0,
	}

	def handle_input():
		axis = context["input_axis"]
		res = context["curr_pos"][axis]
		context["input_axis"] = (context["input_axis"] + 1) % 2
		return res

	def handle_output(x):
		coord = context["curr_pos"]
		GRID[(coord[1] * 50) + coord[0]] = x

		context["curr_pos"][0] += 1
		if context["curr_pos"][0] >= 50:
			context["curr_pos"][1] += 1
			context["curr_pos"][0] = 0

	while context["curr_pos"][1] == row:
		vm_prog = intcode.load_program(PROG)
		intcode.run_until_halt(vm_prog, handle_input, handle_output)

def array_to_grid(arr, w):
	idx = 0
	while idx < len(arr):
		yield arr[idx:idx + w]
		idx += w

def run_program_step1(prog):
	global PROG, GRID
	GRID = Array('i', 50 * 50)
	p = Pool(6, initializer=pool_init, initargs=(prog,))
	res = p.map(run_row, range(50))
	return list(GRID)

def run_program_step2(prog):
	first_grid = run_program_step1(prog)
	first_grid = list(array_to_grid(first_grid, 50))
	ones_per_row = dict(enumerate([len([x for x in row if x == 1]) for row in first_grid]))
	print(ones_per_row)
	last = 0
	for row, count in ones_per_row.items():
		print("Row {}: diff {}".format(row, count - last))
		last = count
	# TODO: implement algorithm
	# 1. Check output from step1 and estimate Y coordinate where width of beam is just less than 100
	# 2. Start running program from Y coordinate from step1
	# 3. Store first Y that has a width of 100
	# 4. Extrapolate or continue running program from Y in 3.

def render_grid(grid, w, h):
	output = ""
	for idx, cell in enumerate(grid):
		if idx % w == 0:
			output += "\n"
		if cell == 1:
			output += "#"
		else:
			output += "."
	return output

def count_ones(grid):
	return len([cell for cell in grid if cell == 1])


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
	grid = run_program_step2(prog)
	return None

def main():
	#print("Number of positions showing as in range of tractor beam over 50x50 grid: {}".format(step1()))
	print("Step 2: {}".format(step2()))


if __name__ == "__main__":
	main()
