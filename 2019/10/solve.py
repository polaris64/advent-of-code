import math

def trace_beam(inp, a_from, a_to):
	visible = False
	hit = None
	x = a_from[0]
	y = a_from[1]

	# Calculate the gradient if possible
	if a_to[1] - a_from[1] == 0:
		grad = None
	else:
		grad = abs(a_to[0] - a_from[0]) / abs(a_to[1] - a_from[1])

	# Trace along the X-axis if the Y-axis values are the same
	if grad is None:
		dx = 1 if a_to[0] > a_from[0] else -1
		for x in range(a_from[0] + dx, a_to[0] + dx, dx):
			if x == a_to[0] and inp[y][x] == "#":
				visible = True
				hit = (x, y)
				break
			elif inp[y][x] == "#":
				hit = (x, y)
				break

	# Trace along the Y-axis updating X by grad otherwise
	else:
		dy = 1 if a_to[1] > a_from[1] else -1
		y += dy
		while y >= 0 and y < len(inp):
			dx = abs(a_from[1] - y) * grad
			if a_to[0] < a_from[0]:
				dx *= -1
			x = a_from[0] + dx
			if x < 0 or x >= len(inp[y]):
				break

			# TODO: test if precision is acceptable
			if x - int(x) < 0.001:
				if int(x) == a_to[0] and y == a_to[1] and inp[y][int(x)] == "#":
					visible = True
					hit = (int(x), y)
					break
				elif inp[y][int(x)] == "#":
					hit = (int(x), y)
					break
			y += dy

	return (visible, hit)

def get_visible_asteroids(inp, coord):
	count = 0
	for y, row in enumerate(inp):
		for x, cell in enumerate(row):
			if cell == "#" and not (x == coord[0] and y == coord[1]):
				if trace_beam(inp, coord, (x, y))[0]:
					count += 1
	return count
				

def get_best_location(inp):
	res = []
	for y, row in enumerate(inp):
		for x, cell in enumerate(row):
			if cell == "#":
				res.append(((x, y), get_visible_asteroids(inp, (x, y))))
	return max(res, key=lambda x: x[1])

def map_has_asteroids(inp):
	return any([row for row in inp if any([cell for cell in row if cell == "#"])])

def perform_laser_annihilation(inp, station):

	# Create a list of all cells containing an asteroid together with the
	# angle from the station's up vector
	cell_angles = []
	for y, row in enumerate(inp):
		for x, cell in enumerate(row):
			if x == station[0] and y == station[1]:
				continue
			if cell == "#":
				cell_angles.append((
					(x, y),
					(math.atan2(y - station[1], x - station[0]) + (math.pi / 2)) % (math.pi * 2)
				))

	# Sort by angle to produce an ordered list of cells to visit during
	# sweep
	cell_angles.sort(key=lambda x: x[1])

	destroy_list = []

	# Remove the station from the map
	inp[station[1]][station[0]] = "."

	while map_has_asteroids(inp):
		last_angle = None
		for cell in cell_angles:

			# If the cell no longer contains an asteroid, skip it
			if inp[cell[0][1]][cell[0][0]] != "#":
				continue

			# Only process the cell if the angle is different from
			# the previous during this rotation as the laser can
			# only destroy one asteroid for each angle
			if cell[1] != last_angle:
				nearest = trace_beam(inp, station, cell[0])
				if nearest[1] != None:

					# Destroy asteroid and append to list
					inp[nearest[1][1]][nearest[1][0]] = "."
					destroy_list.append(nearest[1])
					last_angle = cell[1]

	return destroy_list


def get_input(filename):
	with open(filename, "rt") as f:
		return [list(line.strip()) for line in f.read().split("\n") if len(line)]

def step1():
	inp = get_input("input.txt")
	res = get_best_location(inp)
	return res

def step2(station):
	inp = get_input("input.txt")
	res = perform_laser_annihilation(inp, station)[199]
	return (res, (res[0] * 100) + res[1])

def main():
	res = step1()
	print("Asteroids visible from the best location {}: {}".format(res[0], res[1]))
	res = step2(res[0])
	print("200th asteroid destroyed from base: {} (checksum {})".format(res[0], res[1]))


def test_examples_step1():
	assert get_best_location(get_input("ex1.txt")) == ((3,   4),   8)
	assert get_best_location(get_input("ex2.txt")) == ((5,   8),  33)
	assert get_best_location(get_input("ex3.txt")) == ((1,   2),  35)
	assert get_best_location(get_input("ex4.txt")) == ((6,   3),  41)
	assert get_best_location(get_input("ex5.txt")) == ((11, 13), 210)

def test_examples_step2():
	inp = get_input("ex5.txt")
	res = perform_laser_annihilation(inp, (11, 13))
	assert res[0]   == (11, 12)
	assert res[1]   == (12,  1)
	assert res[2]   == (12,  2)
	assert res[9]   == (12,  8)
	assert res[19]  == (16,  0)
	assert res[49]  == (16,  9)
	assert res[99]  == (10, 16)
	assert res[198] == (9,   6)
	assert res[199] == (8,   2)
	assert res[200] == (10,  9)
	assert res[298] == (11,  1)

def test_main():
	res = step1()
	assert res == ((23, 19),  278)
	assert step2(res[0]) == ((14, 17), 1417)


if __name__ == "__main__":
	main()
