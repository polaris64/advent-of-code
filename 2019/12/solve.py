import numpy

def apply_gravity(moons, a, b):
	ma = moons[a]
	mb = moons[b]
	for axis in range(3):
		if ma[0][axis] < mb[0][axis]:
			ma[1][axis] += 1
			mb[1][axis] -= 1
		elif ma[0][axis] > mb[0][axis]:
			ma[1][axis] -= 1
			mb[1][axis] += 1

def apply_velocity(moon):
	moon[0][0] += moon[1][0]
	moon[0][1] += moon[1][1]
	moon[0][2] += moon[1][2]

def simulate(moons, max_steps):
	step = 0
	while step < max_steps:

		for moon_a in range(4):
			for moon_b in range(moon_a + 1, 4):
				apply_gravity(moons, moon_a, moon_b)

		for moon in moons:
			apply_velocity(moon)

		step += 1

def calculate_energy(moon):
	pot = sum(abs(x) for x in moon[0])
	kin = sum(abs(x) for x in moon[1])
	return pot * kin

def calculate_total_energy(moons):
	return sum([calculate_energy(moon) for moon in moons])

def get_period(moons, axis):
	"""Finds the period of steps before the system repeats (position and
	velocity) in a single axis only"""
	period = 0

	init_state = (
		moons[0][0][axis], moons[0][1][axis],
		moons[1][0][axis], moons[1][1][axis],
		moons[2][0][axis], moons[2][1][axis],
		moons[3][0][axis], moons[3][1][axis],
	)

	while True:
		simulate(moons, 1)

		# Unrolling the comprehension and creating a 1-dimensional
		# tuple here increases speed
		#state = tuple([(moon[0][axis], moon[1][axis]) for moon in moons])
		state = (
			moons[0][0][axis], moons[0][1][axis],
			moons[1][0][axis], moons[1][1][axis],
			moons[2][0][axis], moons[2][1][axis],
			moons[3][0][axis], moons[3][1][axis],
		)

		period += 1
		if state == init_state:
			break

	return period

def get_full_period(moons):
	"""Finds the period for each axis in turn, then finds the lowest common
	multiple which is the period of the system under all 3 axes"""
	p1 = get_period(moons.copy(), 0)
	p2 = get_period(moons.copy(), 1)
	p3 = get_period(moons.copy(), 2)
	lcm = numpy.lcm(p1, p2)
	lcm = numpy.lcm(lcm, p3)
	return lcm


def get_input_live():
	return [
		([-6, -5, -8],   [0, 0, 0]),
		([0, -3, -13],   [0, 0, 0]),
		([-15, 10, -11], [0, 0, 0]),
		([-3, -8, 3],    [0, 0, 0]),
	]

def get_input_example_1():
	return [
		([-1, 0, 2],   [0, 0, 0]),
		([2, -10, -7], [0, 0, 0]),
		([4, -8, 8],   [0, 0, 0]),
		([3, 5, -1],   [0, 0, 0]),
	]

def get_input_example_2():
	return [
		([-8, -10, 0], [0, 0, 0]),
		([5, 5, 10],   [0, 0, 0]),
		([2, -7, 3],   [0, 0, 0]),
		([9, -8, -3],  [0, 0, 0]),
	]

def step1():
	moons = get_input_live()
	simulate(moons, 1000)
	return calculate_total_energy(moons)

def step2():
	moons = get_input_live()
	return get_full_period(moons)

def main():
	print("Total energy in the system after 1000 steps: {}".format(step1()))
	print("Number of steps before system returns to a previous state: {}".format(step2()))


def test_main():
	assert step1() == 5937
	assert step2() == 376203951569712

def test_example_step1():
	moons = get_input_example_1()
	simulate(moons, 10)
	assert moons[0] == ([2, 1, -3], [-3, -2, 1])
	assert moons[1] == ([1, -8, 0], [-1, 1, 3])
	assert moons[2] == ([3, -6, 1], [3, 2, -3])
	assert moons[3] == ([2, 0, 4], [1, -1, -1])
	assert calculate_total_energy(moons) == 179

def test_example_step2():
	moons = get_input_example_1()
	assert get_full_period(moons) == 2772
	moons = get_input_example_2()
	assert get_full_period(moons) == 4686774924


if __name__ == "__main__":
	main()
