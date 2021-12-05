live_range = (246515, 739105)

def get_repeating_groups(chars):
	return [(x, y) for (x, y) in zip(chars, chars[1:]) if x == y]

def has_same_adj(chars):
	"""
	Returns True only if the sequence has two repeating characters in
	series (e.g. "123334" but not "123456")
	"""
	return len(get_repeating_groups(chars))

def has_same_adj_only(chars):
	"""
	Returns True only if the sequence has any two (but only 2) repeating
	characters in series (e.g. "111223" but not "111222")
	"""
	groups = get_repeating_groups(chars)
	for group in groups:
		# Number of times specific group (e.g. ("1","1")) appears in
		# list
		c = len([g for g in groups if g == group])

		# If the group only appears once, sequence passes test
		if c == 1:
			return True

	# No groups passed
	return False

def only_increases(chars):
	"""
	Returns True only of the sequence of characters increses numerically
	(e.g. "123456" but not "123546")
	"""
	last = int(chars[0])
	for ch in chars:
		if int(ch) < last:
			return False
		last = int(ch)
	return True

def get_permutations(start, end, adjacency_check):
	"""
	Returns a generator for all numbers in the range matching the criteria
	"""
	for i in range(start, end + 1):
		chars = str(i)
		if len(chars) != 6:
			continue
		if not only_increases(chars):
			continue
		if not adjacency_check(chars):
			continue
		yield i


def step1(r):
	return len(list(get_permutations(r[0], r[1], has_same_adj)))

def step2(r):
	return len(list(get_permutations(r[0], r[1], has_same_adj_only)))


def main():
	print("Possible solutions between {} and {} (step 1): {}".format(live_range[0], live_range[1], step1(live_range)))
	print("Possible solutions between {} and {} (step 2): {}".format(live_range[0], live_range[1], step2(live_range)))


def test_live():
	assert step1(live_range) == 1048
	assert step2(live_range) == 677

def test_examples():
	assert 111111     in list(get_permutations(111110, 111112, has_same_adj))
	assert 223450 not in list(get_permutations(223449, 223451, has_same_adj))
	assert 123789 not in list(get_permutations(123788, 123790, has_same_adj))

	assert 112233     in list(get_permutations(112232, 112234, has_same_adj_only))
	assert 123444 not in list(get_permutations(123443, 123445, has_same_adj_only))
	assert 111122     in list(get_permutations(111121, 111123, has_same_adj_only))


if __name__ == "__main__":
	main()
