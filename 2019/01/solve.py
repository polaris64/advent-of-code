def get_input():
	with open("input.txt", "rt") as f:
		return [int(x.strip()) for x in f.read().split("\n") if len(x)]

def calc_module_fuel(mass):
	return int(mass / 3) - 2

def calc_module_fuel_full(mass):
	cont = True
	t_mass = mass
	t_fuel = 0
	while cont:
		fuel = calc_module_fuel(t_mass)
		if fuel <= 0:
			break
		t_fuel += fuel
		t_mass = fuel
	return t_fuel

def step1():
	masses = get_input()
	return sum([calc_module_fuel(x) for x in masses])

def step2():
	masses = get_input()
	return sum([calc_module_fuel_full(x) for x in masses])


def main():
	print("Fuel for modules only: {}".format(step1()))
	print("Fuel for modules and additional fuel: {}".format(step2()))


def test_live():
	assert step1() == 3325347
	assert step2() == 4985145

if __name__ == "__main__":
	main()
