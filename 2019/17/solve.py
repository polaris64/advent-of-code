import time
from collections import defaultdict

import intcode

def get_droid(grid):
	matching_rows = [[(x, y, cell) for x, cell in enumerate(row) if cell != "#" and cell != "."] for y, row in enumerate(grid)]
	matching_rows = [x for x in matching_rows if len(x) > 0]
	if len(matching_rows) == 1:
		return matching_rows[0][0]
	else:
		return None

def can_walk(grid, c_from, direction):
	if direction == "<":
		return c_from[0] > 0 and grid[c_from[1]][c_from[0] - 1] == "#"
	elif direction == ">":
		return c_from[0] < len(grid[c_from[1]]) - 1 and grid[c_from[1]][c_from[0] + 1] == "#"
	elif direction == "^":
		return c_from[1] > 0 and grid[c_from[1] - 1][c_from[0]] == "#"
	elif direction == "v":
		return c_from[1] < len(grid) - 1 and grid[c_from[1] + 1][c_from[0]] == "#"
	else:
		return False

def update_pos(curr_pos, direction):
	if direction == "<":
		return (curr_pos[0] - 1, curr_pos[1])
	elif direction == ">":
		return (curr_pos[0] + 1, curr_pos[1])
	elif direction == "^":
		return (curr_pos[0], curr_pos[1] - 1)
	elif direction == "v":
		return (curr_pos[0], curr_pos[1] + 1)
	else:
		return curr_pos

def rotate(curr, rot_dir):
	if curr == "<":
		return "^" if rot_dir == 1 else "v"
	elif curr == ">":
		return "v" if rot_dir == 1 else "^"
	elif curr == "^":
		return ">" if rot_dir == 1 else "<"
	elif curr == "v":
		return "<" if rot_dir == 1 else ">"
	else:
		return direction

def walk_grid(grid):
	seq = []
	droid = list(get_droid(grid))
	if droid is None:
		return None
	curr_steps = 0
	while True:
		if can_walk(grid, (droid[0], droid[1]), droid[2]):
			new_pos = update_pos((droid[0], droid[1]), droid[2])
			droid[0] = new_pos[0]
			droid[1] = new_pos[1]
			curr_steps += 1
			last_rotate = False
		else:
			# Rotate
			droid[2] = rotate(droid[2], 1)
			rot_seq = "R"
			if not can_walk(grid, (droid[0], droid[1]), droid[2]):
				droid[2] = rotate(droid[2], -1)
				droid[2] = rotate(droid[2], -1)
				rot_seq = "L"
				if not can_walk(grid, (droid[0], droid[1]), droid[2]):
					if curr_steps > 0:
						seq.append(str(curr_steps))
					break

			if curr_steps > 0:
				seq.append(str(curr_steps))

			seq.append(rot_seq)

			curr_steps = 0

	return ",".join(seq)

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

def run_program_step2(prog, view_feed, seq):

	# TODO: parse movement sequence and find longest repeating patterns for function definitions
	# TODO: convert 3 longest repeated sequences to function definitions and update main_routine to be a list of function calls
	print("TODO: compress sequence: {}".format(seq))

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

def string_to_grid(output):
	return [list(row) for row in output.split("\n") if len(row) > 0]

def get_intersections(output):
	intersections = []
	grid = string_to_grid(output)
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
	res = run_program_step1(prog.copy())
	seq = walk_grid(string_to_grid(res["output"]))
	res = run_program_step2(prog, False, seq)
	return res["output"]

def main():
	print("Sum of alignment parameters: {}".format(step1()))
	print("Amount of dust collected by vacuuming robot: {}".format(step2()))


if __name__ == "__main__":
	main()
