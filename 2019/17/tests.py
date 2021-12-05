import solve

TEST_GRIDS = [
	"..#..........\n" +
	"..#..........\n" +
	"#######...###\n" +
	"#.#...#...#.#\n" +
	"#############\n" +
	"..#...#...#..\n" +
	"..#####...<..\n",

	"#######...#####\n" +
	"#.....#...#...#\n" +
	"#.....#...#...#\n" +
	"......#...#...#\n" +
	"......#...###.#\n" +
	"......#.....#.#\n" +
	"^########...#.#\n" +
	"......#.#...#.#\n" +
	"......#########\n" +
	"........#...#..\n" +
	"....#########..\n" +
	"....#...#......\n" +
	"....#...#......\n" +
	"....#...#......\n" +
	"....#####......\n",
]


def test_get_intersections():
	ints = solve.get_intersections(TEST_GRIDS[0])
	assert len(ints) == 4

def test_get_alignment_params_sum():
	ints = solve.get_intersections(TEST_GRIDS[0])
	params = solve.get_alignment_params_sum(ints)
	assert params == 76

def test_get_current_dir():
	assert solve.get_droid(solve.string_to_grid(TEST_GRIDS[0])) == (10, 6, "<")
	assert solve.get_droid(solve.string_to_grid(TEST_GRIDS[1])) == ( 0, 6, "^")

def test_can_walk():
	grid = solve.string_to_grid(TEST_GRIDS[0])
	assert solve.can_walk(grid, (6, 2), "^") == False
	assert solve.can_walk(grid, (6, 2), ">") == False
	assert solve.can_walk(grid, (6, 2), "v") == True
	assert solve.can_walk(grid, (6, 2), "<") == True

def test_walk_grid():
	seq = solve.walk_grid(solve.string_to_grid(TEST_GRIDS[0]))
	print(seq)
	assert seq == "R,4,R,2,R,2,R,12,R,2,R,6,R,4,R,4,R,6"

	seq = solve.walk_grid(solve.string_to_grid(TEST_GRIDS[1]))
	assert seq == "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2"


def test_step1():
	assert solve.step1() == 7816

def test_step2():
	assert solve.step2() == 952010
