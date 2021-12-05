from collections import defaultdict

import intcode

def run_program(prog, start_panel_colour):
	context = {
		"cells":       defaultdict(lambda: 0),
		"curr_pos":    [0, 0],
		"direction":   0,
		"output_mode": 0,
	}

	context["cells"][(0, 0)] = start_panel_colour

	def handle_input():
		# Robot's camera: return the value of the current cell
		return context["cells"][tuple(context["curr_pos"])]

	def handle_output(x):
		if context["output_mode"] == 0:

			# Paint the current cell
			context["cells"][tuple(context["curr_pos"])] = x

			# Change output_mode ready for next direction change
			context["output_mode"] = 1

		else:
			# Rotate 90° left (0) or 90° right (1)
			if x == 0:
				context["direction"] = (context["direction"] - 1) % 4
			elif x == 1:
				context["direction"] = (context["direction"] + 1) % 4

			# Move forward 1 cell based on new direction
			if context["direction"] == 0: # UP
				context["curr_pos"][1] -= 1
			elif context["direction"] == 1: # RIGHT
				context["curr_pos"][0] += 1
			elif context["direction"] == 2: # DOWN
				context["curr_pos"][1] += 1
			elif context["direction"] == 3: # LEFT
				context["curr_pos"][0] -= 1

			# Reset output_mode ready for next paint
			context["output_mode"] = 0

	# Run the painting robot's program until it halts, handling input and
	# output using the above functions
	intcode.run_until_halt(prog, handle_input, handle_output)

	return context

def render_output(cells):
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
			output += "#" if v == 1 else " "
		output += "\n"

	return output


def get_input():
	with open("input.txt", "rt") as f:
		return [int(x.strip()) for x in f.read().split(",") if len(x)]

def step1():
	prog = get_input()
	res = run_program(prog, 0)
	cells_painted = len(res["cells"])
	return cells_painted

def step2():
	prog = get_input()
	res = run_program(prog, 1)
	output = render_output(res["cells"])
	return output

def main():
	print("Number of panels painted at least once: {}".format(step1()))
	print("Panels after robot finishes painting:\n{}".format(step2()))


def test_main():
	assert step1() == 2021

	expected  = " #    ###    ## #  # #### #  # #    #  #   \n"
	expected += " #    #  #    # #  # #    # #  #    #  #   \n"
	expected += " #    ###     # #### ###  ##   #    ####   \n"
	expected += " #    #  #    # #  # #    # #  #    #  #   \n"
	expected += " #    #  # #  # #  # #    # #  #    #  #   \n"
	expected += " #### ###   ##  #  # #### #  # #### #  #   \n"

	assert step2() == expected


if __name__ == "__main__":
	main()
