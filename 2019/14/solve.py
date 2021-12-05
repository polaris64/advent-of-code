import math
import re

def get_rule_for(chem, recipes):
	"""Returns the rule from recipes that is used to produce chem, or
	None"""
	for x in recipes:
		if chem in x["out"]:
			return x
	return None

def generate_chem(quantities, recipes, needed_type, needed_amt, reduce_ore=False):
	"""Recursive function to generate a required amount of a chemical.
	Updates quantities with the results.

	Arguments:
	  - quantities: dict of current chamical quantities
	  - recipes: the recipe (list of rules)
	  - needed_type: the chamical that is needed
	  - needed_amt: the number of units of needed_type that is required
	  - reduce_ore: if set, chemicals will only be produced if enough ORE
	    exists and the function will return False on failure.  If unset,
	    the amount of ORE is increased accordinly in order to show how much
	    would be required to produce the chemicals.
	"""

	rule = get_rule_for(needed_type, recipes)
	amount_to_add = needed_amt

	# Process each ingredient for this output chemical in turn
	for ch_type, ch_qty in rule["in"].items():

		# Calculate the total: recipe rules can only produce multiples
		# of their specified output chemical
		needed_total = math.ceil(needed_amt / rule["out"].get(needed_type, 1)) * ch_qty

		if ch_type == "ORE":

			# Reduce or increase ORE accoringly
			if reduce_ore:

				# If reducing ORE and we cannot obtain more,
				# return False to signify failure for this rule
				if quantities.get("ORE", 0) < needed_total:
					return False

				quantities["ORE"] = quantities.get("ORE", 0) - needed_total

			else:
				quantities["ORE"] = quantities.get("ORE", 0) + needed_total

		else:
			# If we don't have enough of the chemical
			if quantities.get(ch_type, 0) < needed_total:

				# Generate just enough of the new chemical and bail out on failure
				if generate_chem(quantities, recipes, ch_type, needed_total - quantities.get(ch_type, 0), reduce_ore) == False:
					return False

			# Update the quantity of this chemical as it has just been produces
			quantities[ch_type] = quantities.get(ch_type, 0) - needed_total

		# Calculate the amount of the main chemical that should be
		# added based on this rule's multiplier
		amount_to_add = rule["out"].get(needed_type, 1) * (needed_total / ch_qty)

	# Increase the stock of the main chemical
	quantities[needed_type] = quantities.get(needed_type, 0) + amount_to_add

	return True

def get_amount_of_ore_for_one_fuel(recipes):
	"""Returns the amount of ORE required to produce a single unit of
	FUEL"""
	quantities = dict()
	generate_chem(quantities, recipes, "FUEL", 1)
	return quantities.get("ORE")

def get_possible_fuel_from_ore(recipes, ore_units):
	"""Returns the maximum amount of FUEL units that can be created from
	ore_units"""

	# Initial quantities dict (updated during loop)
	quantities = {"ORE": ore_units}

	# Current FUEL units generated
	fuel = 0

	# Step size: try to generate this many FUEL units during each
	# iteration.  Will be halved upon failure.
	step = ore_units

	# Continue while ORE exists and we are trying to generate at least 1
	# FUEL unit
	while step > 0:

		# Work on a copy of the quantities in case the generation fails
		q_copy = quantities.copy()

		if generate_chem(q_copy, recipes, "FUEL", step, reduce_ore=True) == False:
			# If "step" FUEL units could not be created, halve
			# "step" (integer division) and try again
			step //= 2
		else:
			# FUEL units were created, so increase the total number
			# and commit the quantities dict
			fuel += step
			quantities = q_copy

	return fuel


def get_input(filename):
	def parse_line(line):
		parts = line.split("=>")
		assert len(parts) == 2
		inputs = re.findall(r"([0-9]+) ([A-Z]+),? ?", parts[0].strip())
		output = re.findall(r"([0-9]+) ([A-Z]+)$", parts[1].strip())
		inputs_dict = dict()
		for inp in inputs:
			inputs_dict[inp[1]] = int(inp[0])
		return {
			"in": inputs_dict,
			"out": {output[0][1]: int(output[0][0])},
		}
	with open(filename, "rt") as f:
		return [parse_line(line) for line in f.read().split("\n") if len(line)]

def step1():
	return get_amount_of_ore_for_one_fuel(get_input("input.txt"))

def step2():
	return get_possible_fuel_from_ore(get_input("input.txt"), 1000000000000)

def main():
	print("Amount of ORE required to produce 1 unit of FUEL: {}".format(step1()))
	print("Amount of FUEL that can be created from 1,000,000,000,000 units of ORE: {}".format(step2()))


def test_main():
	assert step1() ==  431448
	assert step2() == 3279311

def test_examples_step1():
	examples = [
		("ex1.txt",      31),
		("ex2.txt",     165),
		("ex3.txt",   13312),
		("ex4.txt",  180697),
		("ex5.txt", 2210736),
	]
	for ex in examples:
		res = get_amount_of_ore_for_one_fuel(get_input(ex[0]))
		assert res == ex[1]

def test_example_step2():
	examples = [
		("ex3.txt", 82892753),
		("ex4.txt",  5586022),
		("ex5.txt",   460664),
	]
	for ex in examples:
		res = get_possible_fuel_from_ore(get_input(ex[0]), 1000000000000)
		assert res == ex[1]


if __name__ == "__main__":
	main()
