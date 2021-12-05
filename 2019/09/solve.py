from itertools import permutations

import intcode

def get_input():
	with open("input.txt", "rt") as f:
		return [int(x.strip()) for x in f.read().split(",") if len(x)]

def run_BOOST(mode):
	program = get_input()
	output = []
	intcode.run_until_halt(program.copy(), lambda: mode, lambda x: output.append(x))
	return output

def step1():
	return run_BOOST(1)[0]

def step2():
	return run_BOOST(2)[0]


def main():
	print("BOOST keycode: {}".format(step1()))
	print("Coordinates of the distress signal: {}".format(step2()))


def test_examples():

	# Quine (should output a copy of itself)
	prog = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
	output = []
	intcode.run_until_halt(prog.copy(), lambda: 0, lambda x: output.append(x))
	assert output == prog

	# Should output a 16-digit number
	prog = [1102,34915192,34915192,7,4,7,99,0]
	output = []
	intcode.run_until_halt(prog, lambda: 0, lambda x: output.append(x))
	assert len(output) == 1 and len(str(output[0])) == 16

	# Should output the second large value
	prog = [104,1125899906842624,99]
	output = []
	intcode.run_until_halt(prog, lambda: 0, lambda x: output.append(x))
	assert len(output) == 1 and output[0] == prog[1]


def test_main():
	assert step1() == 2941952859
	assert step2() == 66113


if __name__ == "__main__":
	main()
