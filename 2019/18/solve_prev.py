import math

from astar import AStar

class AOCAstar(AStar):
	def __init__(self, grid, ignore_doors=False):
		self.grid = grid
		self.ignore_doors = ignore_doors

	def distance_between(self, n1, n2):
		return 1
	def heuristic_cost_estimate(self, n1, n2):
		"""computes the 'direct' distance between two (x,y) tuples"""
		(x1, y1) = n1
		(x2, y2) = n2
		return math.hypot(x2 - x1, y2 - y1)

	def neighbors(self, node):
		x, y = node
		return [
			(nx, ny)
			for nx, ny
			in [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
			if (
				0 <= nx < len(self.grid[0]) and
				0 <= ny < len(self.grid) and
				self.grid[ny][nx] != '#'
				and (self.ignore_doors or (not 0x41 <= ord(self.grid[ny][nx]) <= 0x5a))
			)
		]

def get_closest_key(tunnels):
	# A-star from current pos to any key (shortest path)
	paths = dict()
	for key in tunnels["items"]["keys"]:
		start = tuple(tunnels["items"]["curr_pos"][0:2])
		end = tuple(key[0:2])
		path = AOCAstar(tunnels["grid"], ignore_doors=False).astar(start, end)
		if path is not None:
			paths[key[2]] = (key[0], key[1], key[2], len(list(path)) - 1)

	if len(paths.keys()) == 0:
		print("PATH IS EMPTY")
		return None
	elif len(paths.keys()) == 1:
		return list(paths.values())[0]
	else:
		# Return shortest path

		# Check combinations: e.g. path to e then path to d, then ignoring those doors how far is it to the next key, then the other way around, pick the smallest total


		# 1. Get path to key behind most doors
		start = tuple(tunnels["items"]["curr_pos"][:2])
		end = tuple(tunnels["items"]["last_key"][:2])
		path = AOCAstar(tunnels["grid"], ignore_doors=True).astar(start, end)
		if path is None:
			print("PATH TO LAST KEY IS NONE")
			return paths[0]

		path = list(path)

		# 2. List all doors in path from 1.
		doors_in_path = [door for door in tunnels["items"]["doors"] if any([True for c in path if tuple(c[:2]) == tuple(door[:2])])]

		# 3. Remove doors from 2. that also have keys in 2.
		keys_in_path = [key for key in tunnels["items"]["keys"] if any([True for c in path if tuple(c[:2]) == tuple(key[:2])])]
		doors_in_path = [door for door in doors_in_path if not any([True for key in keys_in_path if key[2].upper() == door[2]])]

		print("PATHS:", paths)
		print("KEYS IN PATH:", keys_in_path)
		print("DOORS IN PATH:", doors_in_path)
		if len(doors_in_path) == 0:
			shortest = None
			for k, path in paths.items():
				if shortest is None or path[3] < shortest[3]:
					shortest = path
			print("NO DOORS IN PATH, SHORTEST:", shortest)
			return shortest

		# 4. Choose closest key for any door from 3.
		new_paths = dict()
		for door in doors_in_path:
			door_keys = [key for key in tunnels["items"]["keys"] if key[2].upper() == door[2]]
			print("DOOR KEYS:", door_keys, len(door_keys))
			if len(door_keys) > 0:
				door_key = door_keys[0]
				start = tuple(tunnels["items"]["curr_pos"][:2])
				end = tuple(door_key[:2])
				path = AOCAstar(tunnels["grid"], ignore_doors=False).astar(start, end)
				if path is not None:
					path = list(path)
					new_paths[door_key[2]] = (door_key[0], door_key[1], door_key[2], len(path) - 1)

		if len(new_paths) == 0:
			new_paths = paths

		print("NEW PATHS:", new_paths)
		shortest = None
		for k, path in new_paths.items():
			if shortest is None or path[3] < shortest[3]:
				shortest = path
		print("SHORTEST:", shortest)
		return shortest

def remove_key_and_door(tunnels, key_id):
	tunnels["items"]["keys"] = [x for x in tunnels["items"]["keys"] if x[2] != key_id]
	tunnels["items"]["doors"] = [x for x in tunnels["items"]["doors"] if x[2] != key_id.upper()]
	for y, row in enumerate(tunnels["grid"]):
		for x, cell in enumerate(row):
			if cell == key_id or cell == key_id.upper():
				tunnels["grid"][y][x] = "."

def process(tunnels):
	steps = 0
	while True:
		target_key = get_closest_key(tunnels)
		print("TARGET KEY:", target_key)
		if target_key is None:
			break

		# Move to key
		tunnels["items"]["curr_pos"][0] = target_key[0]
		tunnels["items"]["curr_pos"][1] = target_key[1]
		steps += target_key[3]

		# Remove the key and equivalent door
		remove_key_and_door(tunnels, target_key[2])

	return (steps, tunnels)

def extract_items(grid):
	res = {
		"curr_pos": None,
		"doors": [],
		"keys": [],
		"last_key": None,
	}
	for y, row in enumerate(grid):
		for x, cell in enumerate(row):
			if cell != "#" and cell != ".":
				if cell == "@":
					res["curr_pos"] = [x, y]
					grid[y][x] = "."
				elif ord(cell) >= 0x41 and ord(cell) <= 0x5a:
					res["doors"].append((x, y, cell))
				elif ord(cell) >= 0x61 and ord(cell) <= 0x7a:
					res["keys"].append((x, y, cell))
	res["doors"].sort(key=lambda x: x[2])
	res["keys"].sort(key=lambda x: x[2])

	# last_key is the key behind most doors
	solver = AOCAstar(grid, ignore_doors=True)
	start = tuple(res["curr_pos"][:2])
	key_doors = dict()
	for key in res["keys"]:
		end = tuple(key[:2])
		path = solver.astar(start, end)
		if path is None:
			key_doors[key[2]] = 0
			continue
		count = 0
		for el in path:
			cell = grid[el[1]][el[0]]
			if ord(cell) >= 0x41 and ord(cell) <= 0x5a:
				count += 1
		key_doors[key[2]] = count
	furthest_key = max(key_doors.items(), key=lambda x: x[1])
	print("FURTHEST KEY:", furthest_key)
	for key in res["keys"]:
		if key[2] == furthest_key[0]:
			res["last_key"] = key[:]
	return res


def get_input(filename):
	with open(filename, "rt") as f:
		grid = [list(row.strip()) for row in f.readlines()]
		items = extract_items(grid)
		return {
			"grid": grid,
			"items": items,
		}


def step1():
	#tunnels = get_input("ex1.txt")
	#tunnels = get_input("ex2.txt")
	#tunnels = get_input("ex3.txt")
	tunnels = get_input("ex4.txt")
	tunnels = get_input("ex5.txt")
	#tunnels = get_input("input.txt")
	res = process(tunnels)
	return res[0]

def step2():
	return None

def main():
	print("Step 1: {}".format(step1())) # 6208 best guess (too high)
	print("Step 2: {}".format(step2()))


if __name__ == "__main__":
	main()
