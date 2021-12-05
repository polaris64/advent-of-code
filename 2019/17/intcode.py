from collections import defaultdict

def load_program(prog):
	return defaultdict(int, enumerate(prog))

def get_opcode(prog, pc):
	opcode = str(prog[pc])
	opcode_instr = int(str(prog[pc])[-2:])

	# Decode all parameter modes: digits from right to left after opcode (2
	# digits), e.g. for 12345, 45 is the opcode and 3, 2, 1 are the
	# parameter modes in order
	param_modes = [int(x) for x in list(opcode[-len(opcode):-2])]
	param_modes.reverse()

	# Create a defaultdict for the param modes (0 if missing)
	modes_dict = defaultdict(int)
	for k, v in enumerate(param_modes):
		modes_dict[k] = v

	return (opcode_instr, modes_dict)

def p_load(context, prog, val, mode):
	if mode == 1: # Immediate
		return val

	offset = 0
	if mode == 0: # Position
		offset = val
	elif mode == 2: # Relative
		offset = context["rb"] + val

	return prog[offset]

def p_store(context, prog, val, addr, mode):
	assert mode != 1
	if mode == 0: # Position
		offset = addr
	elif mode == 2: # Relative
		offset = context["rb"] + addr
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
		if context["input_fn"] is not None:
			inp = context["input_fn"]()
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
		context["pc"] += 2
	elif context["opcode"][0] == 4: # OUTPUT
		if context["output_fn"] is not None:
			context["output_fn"](
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
		context["rb"] += p_load(context, prog, arg1, context["opcode"][1][0])
		context["pc"] += 2
	else:
		context["pc"] += 1

def create_context():
	return {
		"input_fn":  None,
		"opcode":    None,
		"output_fn": None,
		"pc":        0,
		"rb":        0,
	}

def disassemble(ctx, mem):
	opcode = ctx["opcode"]
	pc = ctx["pc"]
	args = (mem[pc + 1], mem[pc + 2], mem[pc + 3])

	def opc(n):
		if opcode[1][n] == 0:
			return "[{} ({})]".format(args[n], p_load(ctx, mem, args[n], ctx["opcode"][1][n]))
		elif opcode[1][n] == 1:
			return str(args[n])
		elif opcode[1][n] == 2:
			return "[{}+{}={} ({})]".format(
				ctx["rb"],
				args[n],
				ctx["rb"] + args[n],
				p_load(ctx, mem, args[n], ctx["opcode"][1][n])
			)
		else:
			return None

	res = "{:02X}: ".format(pc)
	if opcode[0] == 1:
		res += "ADD {} {} {}".format(opc(0), opc(1), opc(2))
	elif opcode[0] == 2:
		res += "MUL {} {} {}".format(opc(0), opc(1), opc(2))
	elif opcode[0] == 3:
		res += "INPUT {}".format(opc(0))
	elif opcode[0] == 4:
		res += "OUTPUT {}".format(opc(0))
	elif opcode[0] == 5:
		res += "JP-T {} {}".format(opc(0), opc(1))
	elif opcode[0] == 6:
		res += "JP-F {} {}".format(opc(0), opc(1))
	elif opcode[0] == 7:
		res += "LT {} {} {}".format(opc(0), opc(1), opc(2))
	elif opcode[0] == 8:
		res += "EQ {} {} {}".format(opc(0), opc(1), opc(2))
	elif opcode[0] == 9:
		res += "IRB {}".format(opc(0))
	elif opcode[0] == 99:
		res += "HALT"
	else:
		res += "--UNKNOWN--"
	return res

def run_until_halt(prog, input_cb=lambda: 0, output_cb=lambda x: print("Output: {}".format(x))):
	
	# Create execution context
	context = create_context()
	context["input_fn"]  = input_cb
	context["opcode"]    = None
	context["output_fn"] = output_cb

	while context["opcode"] is None or context["opcode"][0] != 99:
		context["opcode"] = get_opcode(prog, context["pc"])
		handle_opcode(prog, context)

	return prog

def run_single_steps(prog, input_cb=lambda: 0, output_cb=lambda x: print("Output: {}".format(x))):

	# Create execution context
	context = create_context()
	context["input_fn"]  = input_cb
	context["opcode"]    = None
	context["output_fn"] = output_cb

	while context["opcode"] is None or context["opcode"][0] != 99:
		context["opcode"] = get_opcode(prog, context["pc"])
		yield context
		handle_opcode(prog, context)

	return prog
