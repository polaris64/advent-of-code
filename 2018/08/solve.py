def get_input():
    with open("input") as f:
        return [int(x) for x in [x.strip() for x in f.read().split(" ")] if len(x) > 0]

def extract_nodes(tree, start):

    # Read node header
    header = tree[start:start + 2]
    num_children = header[0]
    num_meta = header[1]

    next_idx = start + 2

    # Process children of this node if necessary
    if num_children > 0:
        children = []
        for child in range(num_children):
            child_data = extract_nodes(tree, next_idx)
            next_idx = child_data[0]
            children.append(child_data[1])
    else:
        children = None

    # Process metadata for this node
    metadata = tree[next_idx:next_idx + num_meta]
    next_idx += num_meta

    return (next_idx, {
        "metadata": metadata,
        "children": children,
    })

def parse_tree(tree):
    return extract_nodes(tree, 0)

def sum_metadata(tree):
    res = sum(tree["metadata"])
    if tree["children"] is not None:
        res += sum([sum_metadata(x) for x in tree["children"]])
    return res

def get_node_value(tree):
    if tree["children"] is None:
        res = sum(tree["metadata"])
        return res
    else:
        res = 0
        for md_idx in tree["metadata"]:
            if md_idx - 1 < len(tree["children"]):
                res += get_node_value(tree["children"][md_idx - 1])
        return res

def run(tree):
    parsed = parse_tree(tree)
    metadata_sum = sum_metadata(parsed[1])
    print("The sum of all metadata items in the tree is: {}".format(metadata_sum))
    tree_sum = get_node_value(parsed[1])
    print("The sum of the root node is: {}".format(tree_sum))
    return (parsed[1], metadata_sum, tree_sum)

def run_example():
    return run([2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2])

def run_full():
    return run(get_input())


def test_full():
    res = run_full()
    assert res[1] == 47112
    assert res[2] == 28237

def test_example():
    res = run_example()
    assert res[0] == {'metadata': [1, 1, 2], 'children': [{'metadata': [10, 11, 12], 'children': None}, {'metadata': [2], 'children': [{'metadata': [99], 'children': None}]}]}
    assert res[1] == 138
    assert res[2] == 66


if __name__ == "__main__":
    #run_example()
    run_full()
