from collections import defaultdict

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

def extend_program(prog, offset):
	# Extend program memory if offset is beyond current upper limit
	if offset >= len(prog):
		prog.extend([0] * (offset - len(prog) + 1))

def p_load(context, prog, val, mode):
	if mode == 1: # Immediate
		return val

	offset = 0
	if mode == 0: # Position
		offset = val
	elif mode == 2: # Relative
		offset = context["rel_base"] + val

	extend_program(prog, offset)
	return prog[offset]

def p_store(context, prog, val, addr, mode):
	assert mode != 1
	if mode == 0: # Position
		offset = addr
	elif mode == 2: # Relative
		offset = context["rel_base"] + addr

	extend_program(prog, offset)
	prog[offset] = val

def handle_opcode(prog, context):
	if context["opcode"][0] == 1: # ADD
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		arg3 = prog[context["pc"] + 3]
		p_store(
			context,
			prog,
			p_load(context, prog, arg1, context["opcode"][1][0]) + p_load(context, prog, arg2, context["opcode"][1][1]),
			arg3,
			context["opcode"][1][2],
		)
		context["pc"] += 4
	elif context["opcode"][0] == 2: # MUL
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		arg3 = prog[context["pc"] + 3]
		p_store(
			context,
			prog,
			p_load(context, prog, arg1, context["opcode"][1][0]) * p_load(context, prog, arg2, context["opcode"][1][1]),
			arg3,
			context["opcode"][1][2],
		)
		context["pc"] += 4
	elif context["opcode"][0] == 3: # INPUT
		inp = context["input"]()
		if inp == None:
			return False
		arg1 = prog[context["pc"] + 1]
		p_store(
			context,
			prog,
			inp,
			arg1,
			context["opcode"][1][0]
		)
		context["input_idx"] += 1
		context["pc"] += 2
	elif context["opcode"][0] == 4: # OUTPUT
		context["output"](
			p_load(context, prog, prog[context["pc"] + 1], context["opcode"][1][0])
		)
		context["pc"] += 2
	elif context["opcode"][0] == 5: # JMP IF TRUE
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		if p_load(context, prog, arg1, context["opcode"][1][0]) != 0:
			context["pc"] = p_load(context, prog, arg2, context["opcode"][1][1])
		else:
			context["pc"] += 3
	elif context["opcode"][0] == 6: # JMP IF FALSE
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		if p_load(context, prog, arg1, context["opcode"][1][0]) == 0:
			context["pc"] = p_load(context, prog, arg2, context["opcode"][1][1])
		else:
			context["pc"] += 3
	elif context["opcode"][0] == 7: # LESS THAN
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		arg3 = prog[context["pc"] + 3]
		p_store(
			context,
			prog,
			1 if p_load(context, prog, arg1, context["opcode"][1][0]) < p_load(context, prog, arg2, context["opcode"][1][1]) else 0,
			arg3,
			context["opcode"][1][2],
		)
		context["pc"] += 4
	elif context["opcode"][0] == 8: # EQUALS
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		arg3 = prog[context["pc"] + 3]
		p_store(
			context,
			prog,
			1 if p_load(context, prog, arg1, context["opcode"][1][0]) == p_load(context, prog, arg2, context["opcode"][1][1]) else 0,
			arg3,
			context["opcode"][1][2],
		)
		context["pc"] += 4
	elif context["opcode"][0] == 9: # REL_BASE OFFSET
		arg1 = prog[context["pc"] + 1]
		context["rel_base"] += p_load(context, prog, arg1, context["opcode"][1][0])
		context["pc"] += 2
	else:
		context["pc"] += 1

def create_context():
	return {
		"input_idx": 0, 
		"input": None,
		"opcode": None,
		"output": None,
		"pc": 0,
		"rel_base": 0,
	}

def run_until_halt(prog, input_cb, output_cb):
	
	# Create execution context
	context = create_context()
	context["input"]  = input_cb
	context["opcode"] = get_opcode(prog, 0)
	context["output"] = output_cb

	while context["opcode"][0] != 99:
		handle_opcode(prog, context)
		context["opcode"] = get_opcode(prog, context["pc"])

	return prog

def run_as_coroutine(prog, input_cb, output_cb):

	# Create execution context
	context = create_context()
	context["input"]  = input_cb
	context["opcode"] = get_opcode(prog, 0)
	context["output"] = output_cb

	while context["opcode"][0] != 99:
		
		# If handle_opcode() returns False then the program is not
		# complete but cannot continue, so yield
		if handle_opcode(prog, context) == False:
			yield

		context["opcode"] = get_opcode(prog, context["pc"])

	return prog


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
		res = run_until_halt(prog[0], lambda: 0, lambda x: 0)
		assert res == prog[1]

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
			output = []
			prog_copy = prog[0].copy()
			res = run_until_halt(prog_copy, lambda: test[0], lambda x: output.append(x))
			assert output[0] == test[1]

	prog = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
	for test in [(7, 999), (8, 1000), (9, 1001)]:
		output = []
		prog_copy = prog.copy()
		res = run_until_halt(prog_copy, lambda: test[0], lambda x: output.append(x))
		assert output[0] == test[1]
