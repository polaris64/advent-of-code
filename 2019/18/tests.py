import solve

def test_node_get_key_list():
	parent = solve.Node(key=None, parent=None, pos=None)
	c_1 = solve.Node(key="a", parent=parent, pos=None)
	c_2 = solve.Node(key="b", parent=parent, pos=None)
	c_1_1 = solve.Node(key="c", parent=c_1, pos=None)
	c_1_2 = solve.Node(key="d", parent=c_1, pos=None)
	assert parent.get_key_list() == []
	assert c_1.get_key_list() == ["a"]
	assert c_2.get_key_list() == ["b"]
	assert c_1_1.get_key_list() == ["a", "c"]
	assert c_1_2.get_key_list() == ["a", "d"]

def test_get_accessible_keys():
	state = solve.get_input("ex1.txt")
	assert solve.get_accessible_keys(state, state["items"]["start_pos"]) == {"a": (7, 1, 2)}
	assert solve.get_accessible_keys(
		state,
		(5, 1)
	) == {"a": (7, 1, 2)}
	assert solve.get_accessible_keys(
		state,
		(2, 1)
	) == {"b": (1, 1, 1)}
	assert solve.get_accessible_keys(
		state,
		(5, 1),
		ignore=["a", "A"]
	) == {"b": (1, 1, 4)}

	state = solve.get_input("ex2.txt")
	assert solve.get_accessible_keys(state, state["items"]["start_pos"]) == {"a": (17, 1, 2)}

	state = solve.get_input("ex3.txt")
	assert solve.get_accessible_keys(state, state["items"]["start_pos"]) == {"a": ( 8, 3, 2), "b": (16, 1, 22)}

	expected = {
		"a": ( 6, 5, 3),
		"b": ( 6, 3, 3),
		"c": ( 6, 1, 5),
		"d": ( 6, 7, 5),
		"e": (10, 1, 5),
		"f": (10, 3, 3),
		"g": (10, 5, 3),
		"h": (10, 7, 5),
	}
	state = solve.get_input("ex4.txt")
	actual = solve.get_accessible_keys(state, state["items"]["start_pos"])
	assert actual == expected

	expected = {
		"a": (16, 1, 15),
		"d": ( 3, 2, 3),
		"e": ( 5, 2, 5),
		"f": ( 7, 2, 7),
	}
	state = solve.get_input("ex5.txt")
	actual = solve.get_accessible_keys(state, state["items"]["start_pos"])
	assert actual == expected

def test_build_tree():
	tree, leaves = solve.build_tree(solve.get_input("ex1.txt"))

	assert len(leaves) == 1
	assert leaves[0].key == "b"
	assert leaves[0].steps == 6

	assert tree.key == None
	assert tree.parent == None
	assert tree.steps == 0
	assert len(tree.children) == 1

	assert tree.children[0].key == "a"
	assert tree.children[0].parent == tree
	assert tree.children[0].steps == 2
	assert len(tree.children[0].children) == 1

	assert tree.children[0].children[0].key == "b"
	assert tree.children[0].children[0].parent == tree.children[0]
	assert tree.children[0].children[0].steps == 6
	assert len(tree.children[0].children[0].children) == 0

def test_step1_examples():
	state = solve.get_input("ex1.txt")
	root, leaves = solve.build_tree(state)
	assert solve.get_smallest_path(leaves) == 8

	state = solve.get_input("ex2.txt")
	root, leaves = solve.build_tree(state)
	assert solve.get_smallest_path(leaves) == 86

	state = solve.get_input("ex3.txt")
	root, leaves = solve.build_tree(state)
	assert solve.get_smallest_path(leaves) == 132

	#state = solve.get_input("ex4.txt")
	#root, leaves = solve.build_tree(state)
	#assert solve.get_smallest_path() == 136

	state = solve.get_input("ex5.txt")
	root, leaves = solve.build_tree(state)
	assert solve.get_smallest_path(leaves) == 81
