def get_input():
	with open("input.txt", "rt") as f:
		return [int(x.strip()) for x in f.read().split(",") if len(x)]

def run_program(prog):
	pc = 0
	opcode = prog[pc]
	while opcode != 99:
		if opcode == 1:
			arg1 = prog[pc + 1]
			arg2 = prog[pc + 2]
			arg3 = prog[pc + 3]
			prog[arg3] = prog[arg1] + prog[arg2]
			pc += 4
		elif opcode == 2:
			arg1 = prog[pc + 1]
			arg2 = prog[pc + 2]
			arg3 = prog[pc + 3]
			prog[arg3] = prog[arg1] * prog[arg2]
			pc += 4
		else:
			pc += 1
		opcode = prog[pc]
	return prog

def init_and_run(input_program, arg1, arg2):
	prog = input_program.copy()
	prog[1] = arg1
	prog[2] = arg2
	return run_program(prog)

def step1():
	program = get_input()
	return init_and_run(program, 12, 2)[0]

def step2():
	program = get_input()
	arg1 = 0
	arg2 = 0
	while True:
		res = init_and_run(program, arg1, arg2)
		if res[0] == 19690720:
			break
		if arg1 == 99:
			arg2 += 1
			arg1 = 0
		else:
			arg1 += 1
		if arg1 == 99 and arg2 == 99:
			break
	return 100 * arg1 + arg2


def main():
	print("Output of program: {}".format(step1()))
	print("100 * noun + verb = {}".format(step2()))


def test_examples():
	programs = [
		([1,0,0,0,99],          [2,0,0,0,99]),
		([2,3,0,3,99],          [2,3,0,6,99]),
		([2,4,4,5,99,0],        [2,4,4,5,99,9801]),
		([1,1,1,4,99,5,6,0,99], [30,1,1,4,2,5,6,0,99]),
	]
	for prog in programs:
		res = run_program(prog[0])
		assert res == prog[1]

def test_main():
	assert step1() == 5098658
	assert step2() == 5064
	

if __name__ == "__main__":
	main()
