import time
from collections import defaultdict

import intcode

def run_program_step1(prog):
	context = {
		"output": "",
	}

	def handle_input():
		return 0

	def handle_output(x):
		if x == 10:
			context["output"] += "\n"
		else:
			context["output"] += chr(x)

	prog = intcode.load_program(prog)
	intcode.run_until_halt(prog, handle_input, handle_output)

	return context

def run_program_step2(prog, view_feed):

	# TODO: walk scaffolding to build movement sequence
	# TODO: parse movement sequence and find longest repeating patterns for function definitions
	# TODO: convert 3 longest repeated sequences to function definitions and update main_routine to be a list of function calls
	main_routine = "A,A,B,C,B,C,B,C,B,A"
	functions = [
		"R,10,L,12,R,6",
		"R,6,R,10,R,12,R,6",
		"R,10,L,12,L,12",
	]

	context = {
		"output": "",
		"input_idx": 0,
	}

	# Concatenate the main_routine, functions and live feed answer to a
	# single string of characters to sent as input to the robot
	command_str = main_routine + "\n" + "\n".join(functions) + "\n" + ("y" if view_feed else "n") + "\n"

	def handle_input():
		res = 0
		if context["input_idx"] < len(command_str):
			res = ord(command_str[context["input_idx"]])
			context["input_idx"] += 1
		return res

	def handle_output(x):
		if view_feed:
			if x == 10:
				context["output"] += "\n"
				print(context["output"])
			else:
				context["output"] += chr(x)
		else:
			context["output"] = x

	# Wake up robot
	prog[0] = 2

	prog = intcode.load_program(prog)
	intcode.run_until_halt(prog, handle_input, handle_output)

	return context

def get_intersections(output):
	intersections = []
	grid = [list(row) for row in output.split("\n") if len(row) > 0]
	for y, row in enumerate(grid):
		for x, cell in enumerate(row):
			if cell == "#" and x > 0 and x < len(row) - 1 and y > 0 and y < len(grid) - 1:
				if (
					grid[y - 1][x] == "#" and
					grid[y + 1][x] == "#" and
					grid[y][x - 1] == "#" and
					grid[y][x + 1] == "#"
				):
					intersections.append((x, y))
	return intersections

def get_alignment_params_sum(intersections):
	return sum([x * y for x, y in intersections])


def get_input():
	with open("input.txt", "rt") as f:
		return [int(x.strip()) for x in f.read().split(",") if len(x)]

def step1():
	prog = get_input()
	res = run_program_step1(prog)
	intersections = get_intersections(res["output"])
	return get_alignment_params_sum(intersections)

def step2():
	prog = get_input()
	res = run_program_step2(prog, False)
	return res["output"]

def main():
	print("Sum of alignment parameters: {}".format(step1()))
	print("Amount of dust collected by vacuuming robot: {}".format(step2()))


if __name__ == "__main__":
	main()
