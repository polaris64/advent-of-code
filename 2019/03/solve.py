def get_input():
	with open("input.txt", "rt") as f:
		return tuple(
			line.strip().split(",") for line in f.read().split("\n") if len(line)
		)

def trace_path(path):
	points = list()
	pt = [0, 0, 0]
	for x in path:
		d = x[0]
		amt = int(x[1:])
		for i in range(amt):
			if d == "R":
				pt[0] += 1
			if d == "L":
				pt[0] -= 1
			if d == "U":
				pt[1] -= 1
			if d == "D":
				pt[1] += 1
			pt[2] += 1
			points.append(tuple([tuple(pt[0:2]), pt[2]]))
	return points

def get_intersections(p1, p2):
	points1 = trace_path(p1)
	points2 = trace_path(p2)
	pts1xy = set([x[0] for x in points1])
	pts2xy = set([x[0] for x in points2])
	intersections = pts1xy.intersection(pts2xy)

	intersections_with_dists = []
	for inter in intersections:
		pts1 = [x for x in points1 if x[0][0] == inter[0] and x[0][1] == inter[1]]
		pts2 = [x for x in points2 if x[0][0] == inter[0] and x[0][1] == inter[1]]
		min1 = min(pts1, key=lambda x: x[1])
		min2 = min(pts2, key=lambda x: x[1])
		intersections_with_dists.append(((inter[0], inter[1]), min1[1] + min2[1]))

	return intersections_with_dists
		

def get_nearest_crossing(intersections):
	dists = [abs(x[0][0]) + abs(x[0][1]) for x in intersections]
	return min(dists)

def get_crossing_with_lowest_steps(intersections):
	return min(intersections, key=lambda x: x[1])[1]

def step1(intersections):
	return get_nearest_crossing(intersections)

def step2(intersections):
	return get_crossing_with_lowest_steps(intersections)


def main():
	traces = get_input()
	intersections = get_intersections(traces[0], traces[1])
	print("Closest intersection: {}".format(step1(intersections)))
	print("Intersection with lowest combined steps: {}".format(step2(intersections)))


def test_examples():
	examples = [
		(
			(
				["R75","D30","R83","U83","L12","D49","R71","U7","L72"],
				["U62","R66","U55","R34","D71","R55","D58","R83"]
			),
			(159, 610)
		),
		(
			(
				["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"],
				["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]
			),
			(135, 410)
		),
	]
	for ex in examples:
		intersections = get_intersections(ex[0][0], ex[0][1])
		dist1 = get_nearest_crossing(intersections)
		dist2 = get_crossing_with_lowest_steps(intersections)
		assert dist1 == ex[1][0]
		assert dist2 == ex[1][1]

def test_main():
	traces = get_input()
	intersections = get_intersections(traces[0], traces[1])
	assert step1(intersections) == 399
	assert step2(intersections) == 15678
	

if __name__ == "__main__":
	main()
