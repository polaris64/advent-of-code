import time
from collections import defaultdict

import intcode

def run_program_step1(prog):
	context = {
		"grid": [[0 for x in range(50)] for y in range(50)],
		"curr_pos": [0, 0],
		"input_axis": 0,
	}

	def handle_input():
		axis = context["input_axis"]
		res = context["curr_pos"][axis]
		#time.sleep(0.1)
		#print("Axis {}: {}".format(axis, res))
		context["input_axis"] = (context["input_axis"] + 1) % 2
		return res

	def handle_output(x):
		coord = context["curr_pos"]
		context["grid"][coord[1]][coord[0]] = x

		context["curr_pos"][0] += 1
		if context["curr_pos"][0] >= 50:
			context["curr_pos"][1] += 1
			context["curr_pos"][0] = 0

	while context["curr_pos"][1] < 50:
		vm_prog = intcode.load_program(prog.copy())
		intcode.run_until_halt(vm_prog, handle_input, handle_output)

	return context

def run_program_step2():
	pass
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

def count_ones(grid):
	return sum([len([cell for cell in row if cell == 1]) for row in grid])


def get_input():
	with open("input.txt", "rt") as f:
		return [int(x.strip()) for x in f.read().split(",") if len(x)]

def step1():
	prog = get_input()
	ctx = run_program_step1(prog)
	res = count_ones(ctx["grid"])
	return res

def step2():
	return None

def main():
	print("Number of positions showing as in range of tractor beam over 50x50 grid: {}".format(step1()))
	print("Step 2: {}".format(step2()))


if __name__ == "__main__":
	main()
