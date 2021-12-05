from intcode import load_program, run_until_halt

def test_simple():
	programs = [
		([1,0,0,0,99],          [2,0,0,0,99]),
		([2,3,0,3,99],          [2,3,0,6,99]),
		([2,4,4,5,99,0],        [2,4,4,5,99,9801]),
		([1,1,1,4,99,5,6,0,99], [30,1,1,4,2,5,6,0,99]),
		([1,1,1,4,99,5,6,0,99], [30,1,1,4,2,5,6,0,99]),
		([1002,4,3,4,33],       [1002,4,3,4,99]),
	]
	for prog in programs:
		res = run_until_halt(load_program(prog[0]), lambda: 0, lambda x: 0)
		assert list(res.values()) == prog[1]

def test_less_than_equals():
	programs = [
		([3,9,8,9,10,9,4,9,99,-1,8], [(8, 1), (7, 0), (9, 0)]),
		([3,9,7,9,10,9,4,9,99,-1,8], [(8, 0), (7, 1), (9, 0)]),
		([3,3,1108,-1,8,3,4,3,99],   [(8, 1), (7, 0), (9, 0)]),
		([3,3,1107,-1,8,3,4,3,99],   [(8, 0), (7, 1), (9, 0)]),
	]
	for prog in programs:
		for test in prog[1]:
			output = []
			res = run_until_halt(load_program(prog[0]), lambda: test[0], lambda x: output.append(x))
			assert output[0] == test[1]

def test_jumps():
	programs = [
		([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], [(-1, 1), (0, 0), (1, 1)]),
		([3,3,1105,-1,9,1101,0,0,12,4,12,99,1],      [(-1, 1), (0, 0), (1, 1)]),
	]
	for prog in programs:
		for test in prog[1]:
			output = []
			res = run_until_halt(load_program(prog[0]), lambda: test[0], lambda x: output.append(x))
			assert output[0] == test[1]

def test_complex():
	prog = load_program([
		3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,
		1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
	])
	for test in [(7, 999), (8, 1000), (9, 1001)]:
		output = []
		res = run_until_halt(prog.copy(), lambda: test[0], lambda x: output.append(x))
		assert output[0] == test[1]

def test_quine():
	prog = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
	output = []
	res = run_until_halt(load_program(prog), output_cb=lambda x: output.append(x))
	assert output == prog

def test_large_ints():

	# Should output a 16-digit number
	prog = [1102,34915192,34915192,7,4,7,99,0]
	output = []
	res = run_until_halt(load_program(prog), output_cb=lambda x: output.append(x))
	assert len(output) == 1 and len(str(output[0])) == 16

	# Should output the number in the middle
	prog = [104,1125899906842624,99]
	output = []
	res = run_until_halt(load_program(prog), output_cb=lambda x: output.append(x))
	assert len(output) == 1 and output[0] == 1125899906842624
