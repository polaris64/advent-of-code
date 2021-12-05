from collections import defaultdict

def get_input():
	with open("input.txt", "rt") as f:
		return [int(x.strip()) for x in f.read().split(",") if len(x)]

def get_opcode(prog, pc):
	opcode = str(prog[pc])
	opcode_instr = int(str(prog[pc])[-2:])

	# Decode all parameter modes: digits from right to left after opcode (2
	# digits), e.g. for 12345, 45 is the opcode and 3, 2, 1 are the
	# parameter modes in order
	modes_dict = defaultdict(lambda: 0)
	param_modes = [int(x) for x in list(opcode[-len(opcode):-2])]
	param_modes.reverse()
	for k, v in enumerate(param_modes):
		modes_dict[k] = v

	return (opcode_instr, modes_dict, param_modes)

def p_output(outputs, val):
	outputs.append(val)

def p_load(prog, val, mode):
	if mode == 0: # Position
		return prog[val]
	elif mode == 1: # Immediate
		return val
	else:
		return val

def p_store(prog, val, addr, mode):
	assert mode == 0
	if mode == 0: # Position
		prog[addr] = val
	if mode == 1: # Immediate
		pass

def run_program(prog, inputs):
	program_outputs = []
	input_idx = 0
	pc = 0
	opcode = get_opcode(prog, pc)
	while opcode[0] != 99:
		if opcode[0] == 1: # ADD
			arg1 = prog[pc + 1]
			arg2 = prog[pc + 2]
			arg3 = prog[pc + 3]
			p_store(
				prog,
				p_load(prog, arg1, opcode[1][0]) + p_load(prog, arg2, opcode[1][1]),
				arg3,
				opcode[1][2],
			)
			pc += 4
		elif opcode[0] == 2: # MUL
			arg1 = prog[pc + 1]
			arg2 = prog[pc + 2]
			arg3 = prog[pc + 3]
			p_store(
				prog,
				p_load(prog, arg1, opcode[1][0]) * p_load(prog, arg2, opcode[1][1]),
				arg3,
				opcode[1][2],
			)
			pc += 4
		elif opcode[0] == 3: # INPUT
			prog[prog[pc + 1]] = inputs[input_idx]
			input_idx += 1
			pc += 2
		elif opcode[0] == 4: # OUTPUT
			p_output(
				program_outputs,
				p_load(prog, prog[pc + 1], opcode[1][0])
			)
			pc += 2
		elif opcode[0] == 5: # JMP IF TRUE
			arg1 = prog[pc + 1]
			arg2 = prog[pc + 2]
			if p_load(prog, arg1, opcode[1][0]) != 0:
				pc = p_load(prog, arg2, opcode[1][1])
			else:
				pc += 3
		elif opcode[0] == 6: # JMP IF FALSE
			arg1 = prog[pc + 1]
			arg2 = prog[pc + 2]
			if p_load(prog, arg1, opcode[1][0]) == 0:
				pc = p_load(prog, arg2, opcode[1][1])
			else:
				pc += 3
		elif opcode[0] == 7: # LESS THAN
			arg1 = prog[pc + 1]
			arg2 = prog[pc + 2]
			arg3 = prog[pc + 3]
			p_store(
				prog,
				1 if p_load(prog, arg1, opcode[1][0]) < p_load(prog, arg2, opcode[1][1]) else 0,
				arg3,
				opcode[1][2],
			)
			pc += 4
		elif opcode[0] == 8: # EQUALS
			arg1 = prog[pc + 1]
			arg2 = prog[pc + 2]
			arg3 = prog[pc + 3]
			p_store(
				prog,
				1 if p_load(prog, arg1, opcode[1][0]) == p_load(prog, arg2, opcode[1][1]) else 0,
				arg3,
				opcode[1][2],
			)
			pc += 4
		else:
			pc += 1
		opcode = get_opcode(prog, pc)
	return (prog, program_outputs)

def init_and_run(input_program, inputs):
	prog = input_program.copy()
	return run_program(prog, inputs)

def step1():
	program = get_input()
	return init_and_run(program, [1])

def step2():
	program = get_input()
	return init_and_run(program, [5])


def main():
	print("Program diagnostic code (air conditioning unit): {}".format(step1()[1][-1]))
	print("Program diagnostic code (thermal radiator controller): {}".format(step2()[1][-1]))


def test_examples():
	programs = [
		([1,0,0,0,99],          [2,0,0,0,99]),
		([2,3,0,3,99],          [2,3,0,6,99]),
		([2,4,4,5,99,0],        [2,4,4,5,99,9801]),
		([1,1,1,4,99,5,6,0,99], [30,1,1,4,2,5,6,0,99]),
		([1,1,1,4,99,5,6,0,99], [30,1,1,4,2,5,6,0,99]),
		([1002,4,3,4,33],       [1002,4,3,4,99]),
	]
	for prog in programs:
		res = run_program(prog[0], [])
		assert res[0] == prog[1]

	programs = [
		# LESS THAN and EQUALS tests
		([3,9,8,9,10,9,4,9,99,-1,8], [(8, 1), (7, 0), (9, 0)]),
		([3,9,7,9,10,9,4,9,99,-1,8], [(8, 0), (7, 1), (9, 0)]),
		([3,3,1108,-1,8,3,4,3,99],   [(8, 1), (7, 0), (9, 0)]),
		([3,3,1107,-1,8,3,4,3,99],   [(8, 0), (7, 1), (9, 0)]),

		# JUMP tests
		([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], [(-1, 1), (0, 0), (1, 1)]),
		([3,3,1105,-1,9,1101,0,0,12,4,12,99,1],      [(-1, 1), (0, 0), (1, 1)]),
	]
	for prog in programs:
		for test in prog[1]:
			prog_copy = prog[0].copy()
			res = run_program(prog_copy, [test[0]])
			assert res[1][0] == test[1]

	prog = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
	for test in [(7, 999), (8, 1000), (9, 1001)]:
		prog_copy = prog.copy()
		res = run_program(prog_copy, [test[0]])
		print(res)
		assert res[1][0] == test[1]

def test_main():
	assert step1()[1][-1] == 7157989
	assert step2()[1][-1] == 7873292


if __name__ == "__main__":
	main()
