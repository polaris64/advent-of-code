def get_input(filename):
	with open(filename, "rt") as f:
		lines = [x.strip() for x in f.read().split("\n") if len(x)]
		return [tuple(x.split(")")) for x in lines]

def get_input_main():
	return get_input("input.txt")

def get_input_ex1():
	return get_input("input_ex1.txt")

def get_input_ex2():
	return get_input("input_ex2.txt")

def count_orbits_for(inp, planet, cache):
	direct = [x for x in inp if x[1] == planet]
	indirect = 0
	for d_orbit in direct:
		if d_orbit[0] in cache:
			indirect = cache[d_orbit[0]]
		else:
			indirect = count_orbits_for(inp, d_orbit[0], cache)
			cache[d_orbit[0]] = indirect
	return len(direct) + indirect

def count_orbits(inp):
	count = 0
	cache = {}
	for orbit in inp:
		planet = orbit[1]
		res = count_orbits_for(inp, planet, cache)
		count += res
	return count

def get_ancestors(inp, id_from):
	res = []
	id_curr = id_from
	while True:
		node = [x for x in inp if x[1] == id_curr]
		if len(node) == 0:
			break
		res.append(node[0][0])
		if node[0][0] == "COM":
			break
		id_curr = node[0][0]
	return res

def count_transfers(inp, id_from, id_to):

	# Find common ancestor
	from_ancestors = get_ancestors(inp, id_from)
	to_ancestors = get_ancestors(inp, id_to)

	common_ancestor = None
	for x in from_ancestors:
		if x in to_ancestors:
			common_ancestor = x
			break

	assert common_ancestor != None

	# Count transfers from id_from to common
	count_from = 0
	for x in from_ancestors:
		if x == common_ancestor:
			break
		count_from += 1

	# Count transfers from common to id_to
	count_to = 0
	for x in to_ancestors:
		if x == common_ancestor:
			break
		count_to += 1

	return count_from + count_to


def step1():
	inp = get_input_main()
	return count_orbits(inp)

def step2():
	inp = get_input_main()
	return count_transfers(inp, "YOU", "SAN")


def main():
	print("Total number of direct and indirect orbits: {}".format(step1()))
	print("Number of orbital hops between YOU and SAN: {}".format(step2()))


def test_examples():
	inp = get_input_ex1()
	res = count_orbits(inp)
	assert res == 42

	inp = get_input_ex2()
	res = count_transfers(inp, "YOU", "SAN")
	assert res == 4

def test_main():
	assert step1() == 621125
	assert step2() == 550


if __name__ == "__main__":
	main()
