import intcode

def run_and_disassemble(prog):
	print("Disassembling...")
	prog = intcode.load_program(prog)
	for ctx in intcode.run_single_steps(prog):
		print(intcode.disassemble(ctx, prog))
	print("")

def main():
	progs = [
		[1,0,0,0,99],
		[2,3,0,3,99],
		[2,4,4,5,99,0],
		[1,1,1,4,99,5,6,0,99],
		[1,1,1,4,99,5,6,0,99],
		[1002,4,3,4,33],

		# LESS THAN and EQUALS tests
		[3,9,8,9,10,9,4,9,99,-1,8],
		[3,9,7,9,10,9,4,9,99,-1,8],
		[3,3,1108,-1,8,3,4,3,99],
		[3,3,1107,-1,8,3,4,3,99],

		# JUMP tests
		[3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9],
		[3,3,1105,-1,9,1101,0,0,12,4,12,99,1],

		[
			3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
			1106,0,36,98,0,0, 1002,21,125,20,4,20,1105,1,46,104,
			999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
		],

		# Quine
		[109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
	]
	for prog in progs:
		run_and_disassemble(prog)

	with open("../09/input.txt", "rt") as f:
		prog = [int(x) for x in f.read().strip().split(",") if len(x) > 0]
		print("Disassembling BOOST (test mode)...")
		prog_c = intcode.load_program(prog)
		for ctx in intcode.run_single_steps(prog_c, input_cb=lambda: 1):
			print(intcode.disassemble(ctx, prog_c))
		print("")

		print("Running BOOST (full)...")
		intcode.run_until_halt(intcode.load_program(prog), input_cb=lambda: 2)
		print("DONE")

if __name__ == "__main__":
	main()
