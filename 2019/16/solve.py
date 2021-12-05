import math

import numpy as np

def run_FFT_basic(signal, phases):
	"""
	Basic implementation of the step 1 logic without Numpy (slow)
	"""

	# Copy input signal
	prev_signal = signal.copy()
	new_signal = None

	for phase in range(phases):

		# Copy signal for this phase
		new_signal = prev_signal.copy()

		for i, el in enumerate(new_signal):
			pattern = generate_pattern(i + 1)
			pattern = pattern * math.ceil(len(prev_signal) / len(pattern))

			digit = sum([x * y for x, y in zip(prev_signal, pattern[1:])])
			new_signal[i] = abs(digit) % 10

		# Update input signal for next phase
		prev_signal = new_signal

	return "".join([str(x) for x in new_signal])

def run_FFT_numpy(signal, phases):
	"""
	Step 1 logic implemented using Numpy with matrix multiplication

	Each number in the signal is the sum of all numbers multiplied by the
	corresponding pattern value.  For example, [1,2,3,4,5,6] with a base
	pattern [0, 1, 0, -1] would become: -
	[
		1*1 + 2*0 + 3*-1 + 4*0 + 5*1 + 6* 0,
		1*0 + 2*1 + 3* 1 + 4*0 + 5*0 + 6*-1,
		1*0 + 2*0 + 3* 1 + 4*1 + 5*1 + 6* 0,
		1*0 + 2*0 + 3* 0 + 4*1 + 5*1 + 6* 1,
		1*0 + 2*0 + 3* 0 + 4*0 + 5*1 + 6* 1,
		1*0 + 2*0 + 3* 0 + 4*0 + 5*0 + 6* 1,
	]

	Therefore, the signal after a single phase can be calculated by matrix
	multiplication between the signal and a matrix consisting of the
	pattern for each element: -

	+--              --+   +- -+
	| 1  0 -1  0  1  0 |   | 1 |
	| 0  1  1  0  0 -1 |   | 2 |
	| 0  0  1  1  1  0 | . | 3 |
	| 0  0  0  1  1  1 |   | 4 |
	| 0  0  0  0  1  1 |   | 5 |
	| 0  0  0  0  0  1 |   | 6 |
	+--              --+   +- -+

	This calculation can be done very rapidly with Numpy of course.
	"""

	signal = np.array(signal)
	signal_len = len(signal)

	# The pattern that will be repeated and tiled for the input signal
	pat = np.array([0, 1, 0, -1])

	# The matrix which will be used to modify the signal per phase
	patmat = np.zeros((signal_len, signal_len), dtype=int)

	for i in range(signal_len):

		# Repeat each element in pat by i (e.g. [0, 1, 0, -1] repeated
		# by 2 is [0, 0, 1, 1, 0, 0, -1, -1]), then roll left by 1
		# place
		basepat = np.roll(np.repeat(pat, i + 1), -1)

		# Repeat the base pattern enough times to cover the length of
		# the input signal, then take the length of the input signal
		patrow = np.tile(basepat, signal_len // len(basepat) + 1)[:signal_len]

		# Append the row to the matrix
		patmat[i] = patrow

	# Calculate signal phases in sequence
	for i in range(phases):

		# New signal for this phase is the dot product (matrix
		# multiplication) of the signal # and the pattern matrix.  Each
		# resulting element should be the # unit only (%10) and
		# ignoring the sign (abs).
		signal = np.abs(np.dot(patmat, signal)) % 10

	# Return the final sequence as a string
	return "".join([str(int(x)) for x in signal])

def generate_pattern(position):
	pattern = []
	pattern.extend([ 0 for x in range(position)])
	pattern.extend([ 1 for x in range(position)])
	pattern.extend([ 0 for x in range(position)])
	pattern.extend([-1 for x in range(position)])
	return pattern


def get_input(filename):
	with open(filename, "rt") as f:
		return [int(x) for x in list(f.read()) if x != "\n"]

def step1():
	return run_FFT_numpy(get_input("input.txt"), 100)[0:8]

def step2():
	"""
	Calculates and returns the code required for step 2

	The problem asks for the input signal to be repeated 10,000 times and
	for the step 1 algorithm to be applied 100 times before the code can be
	obtained from the correct offset (first 7 digits from the initial
	signal).  However this will take too long to process.

	However, as the offset is large we can ignore all values up to the
	offset as the pattern for all of these will be 0.  Also, the offset and
	pattern length combination means that all pattern digits after the
	offset will be 1 or 0.  The signal will be 16,500,000 digits (10,000 *
	1650) in length and offset 5,976,521 onwards will be 1 followed by 0.
	For digits corresponding to 0 in the pattern, no addition is made (n *
	0 = 0).  All meaningful digits therefore correspond to 1, so the result
	of a phase is a simple sum of those digits.

	This would be enough to solve it, however with Numpy we can also use
	another trick.  Instead of calculating each digit in turn to produce a
	new list for the next phase, we can run numpy.cumsum() on the reversed
	list, then reverse it again to get the same result.  For example:
	[1,2,3,4] would be [1+2+3+4, 2+3+4, 3+4, 4] = abs([10, 9, 7, 4]) % 10 =
	[0, 9, 7, 4].  np.cumsum([1,2,3,4][::-1]) = np.cumsum([4,3,2,1]) =
	[4, 7, 9, 10][::-1] = abs([10, 9, 7, 4]) % 10 = [0, 9, 7, 4], which is
	the same.  This is of course faster than the alternative.
	"""

	inp = get_input("input.txt")

	# The problem description states that we are interested in the 8 digits
	# of the signal starting from the offset given by the first 7 digits of
	# the signal
	offset = int("".join([str(x) for x in inp[:7]]))

	# Construct the entire signal (inp repeated 10,000 times) but only take
	# the chunk after the offset
	partial_signal = np.array([int(x) for x in inp] * 10000)[offset:]

	# Perform 100 FFT transforms on the signal
	for i in range(100):

		# Perform the np.cumsum() trick as detailed above
		summed = np.cumsum(partial_signal[::-1]) % 10
		partial_signal = summed[::-1]

	# Return the first 8 digits as a string af with step 1
	return "".join([str(x) for x in partial_signal[0:8]])

def main():
	print("First 8 digits of signal after 100 FFT phases: {}".format(step1()))
	print("8-digit message from full signal after 100 FFT phases: {}".format(step2()))


def test_main():
	assert step1() == "70856418"
	assert step2() == "87766336"

def test_examples_step1():
	examples = [
		("ex1.txt",   1, "48226158"),
		("ex1.txt",   2, "34040438"),
		("ex1.txt",   3, "03415518"),
		("ex1.txt",   4, "01029498"),
		("ex2.txt", 100, "24176176"),
		("ex3.txt", 100, "73745418"),
		("ex4.txt", 100, "52432133"),
	]
	for ex in examples:
		signal = get_input(ex[0])
		res = run_FFT_numpy(signal, ex[1])
		assert res[0:8] == ex[2]


if __name__ == "__main__":
	main()
