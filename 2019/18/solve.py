import math

from astar import AStar

# Build a tree starting with start pos
# For each layer, take each starting node and add children for all directly accessible keys together with step count
# For each new child, do the same (remembering to ignore doors for all keys in the branch so far)
# Repeat for all keys
# For each leaf node, traverse the tree back to the root, summing the step counts
# Select the branch with the lowest sum
# The branch from root to leaf is the optimal path

class AOCAstar(AStar):
	def __init__(self, grid, ignore=[]):
		self.grid = grid
		self.ignore = ignore

	def distance_between(self, n1, n2):
		return 1
	def heuristic_cost_estimate(self, n1, n2):
		"""computes the 'direct' distance between two (x,y) tuples"""
		(x1, y1) = n1
		(x2, y2) = n2
		return abs(x2 - x1) + abs(y2 - y1)

	def neighbors(self, node):
		x, y = node
		return [
			(nx, ny)
			for nx, ny
			in [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
			if (
				0 <= nx < len(self.grid[0]) and
				0 <= ny < len(self.grid) and
				(self.grid[ny][nx] == "." or self.grid[ny][nx] in self.ignore)
			)
		]

class Node():
	def __init__(self, key, pos, steps=0, parent=None):
		self.children = []
		self.key      = key
		self.parent   = parent
		self.pos      = pos
		self.steps    = steps

	def __repr__(self):
		child_num = len(self.children)
		return "Node: {}, {}, {} children".format(
			self.key if self.key is not None else "no key",
			self.pos,
			child_num if child_num > 0 else "no"
		)

	def add_child(self, child):
		self.children.append(child)

	def get_key_list(self):
		res = [self.key] if self.key is not None else []
		if self.parent is not None:
			res = [*self.parent.get_key_list(), *res]
		return res

	def get_path_and_steps(self):
		path = [self.key]
		if self.parent is not None:
			parent_res = self.parent.get_path_and_steps()
		else:
			parent_res = ([], 0)
		return ([*path, *parent_res[0]], self.steps + parent_res[1])

def get_accessible_keys(state, pos, ignore=[]):
	# A-star from current pos to any key
	paths = dict()
	for key in state["items"]["keys"]:
		if key[2] in ignore:
			continue
		start = tuple(pos)
		end = tuple(key[:2])
		path = AOCAstar(state["grid"], ignore=[*ignore, key[2]]).astar(start, end)
		if path is not None:
			paths[key[2]] = (key[0], key[1], len(list(path)) - 1)
	return paths

def build_branch_from_node(state, node, leaves):
	#print("build_branch_from_node({})".format(node))
	ignore = node.get_key_list()
	ignore = [*ignore, *[x.upper() for x in ignore]]
	print(ignore)
	added = 0
	for keyname, key_data in get_accessible_keys(state, node.pos, ignore).items():
		child_node = Node(key=keyname, pos=key_data[:2], steps=key_data[2], parent=node)
		node.add_child(child_node)
		added += 1
		build_branch_from_node(state, child_node, leaves)
	if added == 0:
		leaves.append(node)
	return added

def build_tree(state):
	leaves = []
	root = Node(key=None, pos=tuple(state["items"]["start_pos"]), steps=0, parent=None)
	build_branch_from_node(state, root, leaves)
	return (root, leaves)
	
def extract_items(grid):
	res = {
		"start_pos": None,
		"doors": [],
		"keys": [],
	}
	for y, row in enumerate(grid):
		for x, cell in enumerate(row):
			if cell != "#" and cell != ".":
				if cell == "@":
					res["start_pos"] = [x, y]
					grid[y][x] = "."
				elif ord(cell) >= 0x41 and ord(cell) <= 0x5a:
					res["doors"].append((x, y, cell))
				elif ord(cell) >= 0x61 and ord(cell) <= 0x7a:
					res["keys"].append((x, y, cell))
	res["doors"].sort(key=lambda x: x[2])
	res["keys"].sort(key=lambda x: x[2])

	return res

def get_smallest_path(leaves):
	smallest = None
	for leaf in leaves:
		steps = leaf.get_path_and_steps()[1]
		if smallest is None or steps < smallest:
			smallest = steps
	return smallest

def get_input(filename):
	with open(filename, "rt") as f:
		grid = [list(row.strip()) for row in f.readlines()]
		items = extract_items(grid)
		return {
			"grid": grid,
			"items": items,
		}


def step1():
	#state = get_input("ex1.txt")
	#state = get_input("ex2.txt")
	#state = get_input("ex3.txt")
	state = get_input("ex4.txt")
	#state = get_input("ex5.txt")
	#state = get_input("input.txt")
	root, leaves = build_tree(state)
	return get_smallest_path(leaves);

def step2():
	return None

def main():
	print("Smallest number of steps to collect all keys: {}".format(step1())) # 6208 best guess (too high)
	print("Step 2: {}".format(step2()))


if __name__ == "__main__":
	main()
