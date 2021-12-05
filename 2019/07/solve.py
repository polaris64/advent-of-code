from itertools import permutations

import intcode

def get_input():
	with open("input.txt", "rt") as f:
		return [int(x.strip()) for x in f.read().split(",") if len(x)]

def run_amplifiers(program, phases):
	context = {
		"inputs": [],
		"outputs": [0],
	}

	# Removes and returns a value from the context's input list
	def get_input():
		return context["inputs"].pop(0)

	# Stores output in context
	def handle_output(val):
		context["outputs"].append(val)

	# Run each amp in turn
	for amp in range(len(phases)):

		# Append inputs to amp: [phase value for amp, output from last amp (0 for first)]
		context["inputs"].append(phases[amp])
		context["inputs"].append(context["outputs"][-1])

		# Run the amp until it halts
		intcode.run_until_halt(program.copy(), get_input, handle_output)

	# Return the output from the last amp
	return context["outputs"][-1]

def run_amplifiers_feedback(program, phases):
	assert len(phases) == 5
	context = {
		"last_output": 0,
	}

	# List of amps in the chain: active flag, co-routine, input queue,
	# input callback, output callback
	amps = [
		[True, None, [phases[0], 0], lambda: get_input(0), lambda x: handle_output(0, x)],
		[True, None, [phases[1]],    lambda: get_input(1), lambda x: handle_output(1, x)],
		[True, None, [phases[2]],    lambda: get_input(2), lambda x: handle_output(2, x)],
		[True, None, [phases[3]],    lambda: get_input(3), lambda x: handle_output(3, x)],
		[True, None, [phases[4]],    lambda: get_input(4), lambda x: handle_output(4, x)],
	]

	# Returns the first item of the amp's input queue
	def get_input(amp_id):
		assert amp_id >= 0 and amp_id <= 5
		if len(amps[amp_id][2]):
			return amps[amp_id][2].pop(0)
		else:
			return None

	# As amps are in a chain, appends the output to the input of the next
	# amp. The last amp is connected to the first, so outputs from amp 4 go
	# to amp 0.
	def handle_output(amp_id, val):
		assert amp_id >= 0 and amp_id <= 5
		amps[(amp_id + 1) % 5][2].append(val)
		if amp_id == 4:
			context["last_output"] = val
		
	while True:
		for amp in amps:

			# If the amp hasn't finished
			if amp[0]:

				# If this amp is not yet executing as a
				# co-routine, start it
				if amp[1] == None:
					amp[1] = intcode.run_as_coroutine(program.copy(), amp[3], amp[4])

				# Continue execution of this amp's co-routine
				# until it yields
				try:
					next(amp[1])
				except StopIteration:
					# If the co-routine has halted, mark
					# this amp as finished
					amp[0] = False
			
		# Break out of main loop once all amps have halted
		if all([not x[0] for x in amps]):
			break

	return context["last_output"]

def run_amplifier_permutations(program):
	return max([run_amplifiers(program, x) for x in permutations([0,1,2,3,4])])

def run_amplifier_feedback_permutations(program):
	return max([run_amplifiers_feedback(program, x) for x in permutations([5,6,7,8,9])])


def step1():
	program = get_input()
	return run_amplifier_permutations(program)

def step2():
	program = get_input()
	return run_amplifier_feedback_permutations(program)


def main():
	print("Highest signal that can be sent to the thrusters: {}".format(step1()))
	print("Highest signal with feedback: {}".format(step2()))


def test_examples():
	tests = [
		(
			[3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0],
			[4,3,2,1,0],
			43210,
		),
		(
			[3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0],
			[0,1,2,3,4],
			54321,
		),
		(
			[3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0],
			[1,0,4,3,2],
			65210,
		),
	]
	for test in tests:
		res = run_amplifiers(test[0], test[1])
		assert res == test[2]

	tests = [
		(
			[3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5],
			[9,8,7,6,5],
			139629729,
		),
		(
			[3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10],
			[9,7,8,5,6],
			18216,
		),
	]
	for test in tests:
		res = run_amplifiers_feedback(test[0], test[1])
		assert res == test[2]


def test_main():
	assert step1() == 38500
	assert step2() == 33660560


if __name__ == "__main__":
	main()
