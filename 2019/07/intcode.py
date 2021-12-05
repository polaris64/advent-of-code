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

def handle_opcode(prog, context):
	if context["opcode"][0] == 1: # ADD
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		arg3 = prog[context["pc"] + 3]
		p_store(
			prog,
			p_load(prog, arg1, context["opcode"][1][0]) + p_load(prog, arg2, context["opcode"][1][1]),
			arg3,
			context["opcode"][1][2],
		)
		context["pc"] += 4
	elif context["opcode"][0] == 2: # MUL
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		arg3 = prog[context["pc"] + 3]
		p_store(
			prog,
			p_load(prog, arg1, context["opcode"][1][0]) * p_load(prog, arg2, context["opcode"][1][1]),
			arg3,
			context["opcode"][1][2],
		)
		context["pc"] += 4
	elif context["opcode"][0] == 3: # INPUT
		inp = context["input"]()
		if inp == None:
			return False
		prog[prog[context["pc"] + 1]] = inp
		context["input_idx"] += 1
		context["pc"] += 2
	elif context["opcode"][0] == 4: # OUTPUT
		context["output"](
			p_load(prog, prog[context["pc"] + 1], context["opcode"][1][0])
		)
		context["pc"] += 2
	elif context["opcode"][0] == 5: # JMP IF TRUE
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		if p_load(prog, arg1, context["opcode"][1][0]) != 0:
			context["pc"] = p_load(prog, arg2, context["opcode"][1][1])
		else:
			context["pc"] += 3
	elif context["opcode"][0] == 6: # JMP IF FALSE
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		if p_load(prog, arg1, context["opcode"][1][0]) == 0:
			context["pc"] = p_load(prog, arg2, context["opcode"][1][1])
		else:
			context["pc"] += 3
	elif context["opcode"][0] == 7: # LESS THAN
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		arg3 = prog[context["pc"] + 3]
		p_store(
			prog,
			1 if p_load(prog, arg1, context["opcode"][1][0]) < p_load(prog, arg2, context["opcode"][1][1]) else 0,
			arg3,
			context["opcode"][1][2],
		)
		context["pc"] += 4
	elif context["opcode"][0] == 8: # EQUALS
		arg1 = prog[context["pc"] + 1]
		arg2 = prog[context["pc"] + 2]
		arg3 = prog[context["pc"] + 3]
		p_store(
			prog,
			1 if p_load(prog, arg1, context["opcode"][1][0]) == p_load(prog, arg2, context["opcode"][1][1]) else 0,
			arg3,
			context["opcode"][1][2],
		)
		context["pc"] += 4
	else:
		context["pc"] += 1

def create_context():
	return {
		"input_idx": 0, 
		"input": None,
		"opcode": None,
		"output": None,
		"pc": 0,
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
