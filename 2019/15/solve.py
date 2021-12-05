from collections import defaultdict

import intcode

def get_new_coord(coord, direction):
	"""
	Returns a new coordinate based on movement by direction from the
	current coord
	"""
	if direction == 1:
		return (coord[0], coord[1] - 1)
	elif direction == 2:
		return (coord[0], coord[1] + 1)
	elif direction == 3:
		return (coord[0] - 1, coord[1])
	elif direction == 4:
		return (coord[0] + 1, coord[1])
	else:
		return coord

def get_dir_to(c_from, c_to):
	"""
	Given a coordinate from and to, returns an appropriate direction.
	Directions are: -
		1: north
		2: south
		3: west
		4: east
	"""
	if c_from["pos"][0] < c_to["pos"][0]:
		return 4
	elif c_from["pos"][0] > c_to["pos"][0]:
		return 3
	elif c_from["pos"][1] < c_to["pos"][1]:
		return 2
	elif c_from["pos"][1] > c_to["pos"][1]:
		return 1

def is_in_list(coord, context):
	"""
	Returns True if a coordinate is in either the open or closed list in
	the DFS context.
	"""
	for cell in context["open_cells"]:
		if cell["pos"] == coord:
			return True
	for cell in context["closed_cells"]:
		if cell["pos"] == coord:
			return True
	return False

def back_track(context):
	"""
	Back-tracks a single step in the DFS algorithm.
	"""
	context["curr_cell"] -= 1
	assert context["curr_cell"] >= 0
	if context["curr_cell"] < 0:
		return
	cell = context["open_cells"][context["curr_cell"]]
	cell["dir"] += 1
	if cell["dir"] == 5:
		cell["dir"] = None

def move_backwards(context):
	"""
	Moves the current cell from the open to closed list and returns a
	direction for the droid to travel to the new tip of the open list.
	"""
	dir_to = get_dir_to(
		context["open_cells"][context["curr_cell"]],
		context["open_cells"][context["curr_cell"] - 1]
	)
	context["closed_cells"].append(context["open_cells"].pop(-1))
	context["curr_cell"] -= 1
	return dir_to

def run_program(prog, break_on_oxy_unit):
	"""
	Runs the repair droid IntCode program and handles I/O.

	The I/O routines effectively perform a depth-first search (DFS) through
	the maze contained within the program.
	"""

	context = {
		# Current repair droid position (only for rendering)
		"droid_pos": [0, 0],

		# Index of the current cell in the open list
		"curr_cell": 0,

		# Closed list: cells fully explored (all neighbours visited or
		# a wall)
		"closed_cells": [],

		# Open list: cells whose surroundings have not been fully
		# explored
		"open_cells": [
			{"pos": (0, 0), "dir": 1, "typ": 1},
		],

		# Location of the oxygen generator (once found)
		"oxy_unit": None,
	}

	def handle_input():
		if context["curr_cell"] < 0 or context["curr_cell"] >= len(context["open_cells"]):
			return 0

		cell = context["open_cells"][context["curr_cell"]]
		d = cell["dir"]

		# If the current cell's direction is None then all directions
		# have been explored, so move it to the closed list and move
		# the droid back to the previous cell in the open list
		if d is None:
			return move_backwards(context)

		# Check all directions from the current cell that haven't
		# already been explored
		while True:
			new_pos = get_new_coord(cell["pos"], d)
			if not is_in_list(new_pos, context):
				break
			d += 1
			if d > 4:
				break

		# If an unexplored direction exists, follow it and add the new
		# cell to the open list
		if d <= 4:
			cell["dir"] = d
			context["open_cells"].append({
				"pos": get_new_coord(cell["pos"], d),
				"dir": 1,
				"typ": None,
			})
			context["curr_cell"] += 1
			return d

		# Otherwise, this cell has been fully explored so move it to
		# the closed list and move the droid back to the previous cell
		# in the open list
		else:
			return move_backwards(context)

	def handle_output(x):
		cell = context["open_cells"][context["curr_cell"]]

		# Droid hit a wall and remained at its previous location
		if x == 0:
			cell["typ"] = 0
			context["closed_cells"].append(context["open_cells"].pop(-1))
			back_track(context)

		# Droid moved to the new cell
		elif x == 1:
			cell["typ"] = 1
			context["droid_pos"][0] = cell["pos"][0]
			context["droid_pos"][1] = cell["pos"][1]

		# Droid moved to the new cell and the oxygen unit was
		# discovered there
		elif x == 2:
			cell["typ"] = 2
			context["droid_pos"][0] = cell["pos"][0]
			context["droid_pos"][1] = cell["pos"][1]
			context["oxy_unit"] = cell

	prog = intcode.load_program(prog)

	# Single-step the program and break as soon as the oxygen unit is found
	# (if necessary)
	for vm_ctx in intcode.run_single_steps(prog, handle_input, handle_output):
		if break_on_oxy_unit and context["oxy_unit"] is not None:
			break

	return context

def render_output(context):
	"""
	Takes the completed DFS context and converts the closed list to a grid
	of cells.  Converts this grid to a string for printing.
	"""

	cells = context["closed_cells"]
	if len(cells) == 0:
		return ""

	# Get a bounding box (including the droid)
	min_x = min(cells, key=lambda x: x["pos"][0])["pos"][0]
	max_x = max(cells, key=lambda x: x["pos"][0])["pos"][0]
	min_y = min(cells, key=lambda x: x["pos"][1])["pos"][1]
	max_y = max(cells, key=lambda x: x["pos"][1])["pos"][1]
	if context["droid_pos"][0] > max_x:
		max_x = context["droid_pos"][0]
	if context["droid_pos"][0] < min_x:
		min_x = context["droid_pos"][0]
	if context["droid_pos"][1] > max_y:
		max_y = context["droid_pos"][1]
	if context["droid_pos"][1] < min_y:
		min_y = context["droid_pos"][1]

	# Copy cell types into the grid
	grid = [[1 for x in range(max_x - min_x + 1)] for x in range(max_y - min_y + 1)]
	for cell in cells:
		grid_y = abs(min_y) + cell["pos"][1]
		grid_x = abs(min_x) + cell["pos"][0]
		grid[grid_y][grid_x] = cell["typ"]

	# Update the cell type for the droid
	droid_x = abs(min_x) + context["droid_pos"][0]
	droid_y = abs(min_y) + context["droid_pos"][1]
	grid[droid_y][droid_x] = 3

	# Render the grid as a string
	output = ""
	for row in grid:
		for cell in row:
			if cell == 0:
				output += "#"
			elif cell == 2:
				output += "O"
			elif cell == 3:
				output += "D"
			else:
				output += "."
		output += "\n"

	return output

def find_number_of_steps_for_fill(grid):
	"""
	Given a grid of walls ("#"), an oxygen unit ("O") and empty space
	(" "), fills as much of the space as possible with oxygen and returns
	the number of steps required.
	"""

	context = {
		"open": [],
		"prev_count": 0,
		"steps": 0,
	}

	# Find the oxygen generator and add it to the open list
	for y, row in enumerate(grid):
		for x, cell in enumerate(row):
			if cell == "O":
				context["open"].append((x, y))
				break

	while True:

		# Create a new open list with all newly-filled cells
		new_open = []
		for cell in context["open"]:
			if cell[0] > 0 and grid[cell[1]][cell[0] - 1] not in ["#", "O"]:
				new_open.append((cell[0] - 1, cell[1]))
			if cell[0] < len(grid[0]) - 1 and grid[cell[1]][cell[0] + 1] not in ["#", "O"]:
				new_open.append((cell[0] + 1, cell[1]))
			if cell[1] > 0 and grid[cell[1] - 1][cell[0]] not in ["#", "O"]:
				new_open.append((cell[0], cell[1] - 1))
			if cell[1] < len(grid) - 1 and grid[cell[1] + 1][cell[0]] not in ["#", "O"]:
				new_open.append((cell[0], cell[1] + 1))
		for cell in new_open:
			grid[cell[1]][cell[0]] = "O"
		context["open"] = new_open

		# Break if no more oxygen was produced
		count = sum([len([x for x in row if x == "O"]) for row in grid])
		if count == context["prev_count"]:
			break
		context["prev_count"] = count

		context["steps"] += 1

	return context["steps"]


def get_input():
	with open("input.txt", "rt") as f:
		return [int(x.strip()) for x in f.read().split(",") if len(x)]

def step1():
	prog = get_input()
	res = run_program(prog, True)
	return len(res["open_cells"]) - 1

def step2():
	prog = get_input()
	res = run_program(prog, False)
	grid_str = render_output(res)
	grid = [list(row) for row in grid_str.split("\n") if len(row) > 0]
	return find_number_of_steps_for_fill(grid)

def main():
	#print("Number of steps from start to oxygen unit: {}".format(step1()))
	print("Minutes taken for the entire area to be filled with oxygen: {}".format(step2()))


def test_main():
	assert step1() == 210
	assert step2() == 290


if __name__ == "__main__":
	main()
