from collections import defaultdict

import intcode

def get_element_pos(cells, ch):
	"""Returns the position of the first cell containing ch, or None"""
	for coord in cells.keys():
		if cells[coord] == ch:
			return coord
	return None

def run_program(prog, quarters):
	context = {
		"cells":       defaultdict(lambda: " "),
		"draw_pos":    [0, 0],
		"output_mode": 0,
		"score":       0,
	}

	# "Insert" quarters if necessary
	if quarters is not None:
		prog[0] = quarters

	def handle_input():
		# Obtain position of the ball ("O") and bat ("=") and return
		# the appropriate joystick input (left: -1, right: 1, centered:
		# 0) to try to keep the bat below the ball
		ball_pos = get_element_pos(context["cells"], "O")
		bat_pos = get_element_pos(context["cells"], "=")
		if ball_pos is None or bat_pos is None:
			return 0
		if ball_pos[0] < bat_pos[0]:
			return -1
		elif ball_pos[0] > bat_pos[0]:
			return 1
		else:
			return 0

	def handle_output(x):

		# Set X coordinate
		if context["output_mode"] == 0:
			context["draw_pos"][0] = x
			context["output_mode"] += 1

		# Set Y coordinate
		elif context["output_mode"] == 1:
			context["draw_pos"][1] = x
			context["output_mode"] += 1

		# Set tile ID
		elif context["output_mode"] == 2:
			pos = tuple(context["draw_pos"])
			if pos == (-1, 0):
				context["score"] = x
			else:
				context["tile_id"] = x

				# Update grid
				if x == 1:
					ch = "#"
				elif x == 2:
					ch = "x"
				elif x == 3:
					ch = "="
				elif x == 4:
					ch = "O"
				else:
					ch = " "
				context["cells"][pos] = ch

			# Reset output_mode back to 0 (set X)
			context["output_mode"] = 0


	# Run the game until it halts
	intcode.run_until_halt(prog, handle_input, handle_output)

	return context

def render_output(cells):
	"""Outputs all cells as output by the running program as a string"""
	# Get a bounding box
	min_x = min(cells.keys(), key=lambda x: x[0])[0]
	max_x = max(cells.keys(), key=lambda x: x[0])[0]
	min_y = min(cells.keys(), key=lambda x: x[1])[1]
	max_y = max(cells.keys(), key=lambda x: x[1])[1]

	# Render the grid to a string
	output = ""
	for y in range(min_y, max_y + 1):
		for x in range(min_x, max_x + 1):
			v = cells[(x, y)]
			output += v
		output += "\n"

	return output


def get_input():
	with open("input.txt", "rt") as f:
		return [int(x.strip()) for x in f.read().split(",") if len(x)]

def step1():
	prog = get_input()
	res = run_program(prog, None)
	output = render_output(res["cells"])
	print(output)
	return len([ch for ch in output if ch == "x"])

def step2():
	prog = get_input()
	res = run_program(prog, 2)
	return res["score"]

def main():
	print("Number of blocks at the beginning of the game: {}".format(step1()))
	print("Score after destroying all blocks: {}".format(step2()))


def test_main():
	assert step1() == 247
	assert step2() == 12954


if __name__ == "__main__":
	main()
