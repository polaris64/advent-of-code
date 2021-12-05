def read_input(filename):
    with open(filename, "rt") as f:
        draw_numbers = [int(x) for x in f.readline().split(",")]
        f.readline() # Ignore separator
        return {
            "draw_numbers": draw_numbers,
            "boards": load_boards(f.readlines()),
        }

def load_boards(lines):
    boards = []
    board = []
    for line in lines + [""]:
        if len(line.strip()) == 0:
            if len(board) > 0:
                boards.append(board)
            board = []
        else:
            board.append([int(x) for x in line.split(" ") if x != " " and len(x) > 0])
    return boards

def run_draw(inp):
    for draw_idx, num in enumerate(inp["draw_numbers"]):
        for board in inp["boards"]:
            for row in board:
                for idx, row_num in enumerate(row):
                    if row_num == num:
                        row[idx] = "X"
        yield (draw_idx, num, inp)

def check_board(board):
    # Check rows
    for row in board:
        if all([x == "X" for x in row]):
            return True
    for col_id in range(len(board[0])):
        col = [row[col_id] for row in board]
        if all([x == "X" for x in col]):
            return True
    return False

def calc_winning_board(board, last_num):
    return sum([x for row in board for x in row if x != "X"]) * last_num

def print_boards(inp):
    for idx, board in enumerate(inp["boards"]):
        print("Board {}".format(idx + 1))
        for row in board:
            print(" ".join([str(x) for x in row]))
    print("")

def solve_p1(inp):
    for _, num, inp in run_draw(inp):
        for board in inp["boards"]:
            if check_board(board):
                return calc_winning_board(board, num)
    return None

def solve_p2(inp):
    board_results = []
    last_to_win = None
    for _, num, inp in run_draw(inp):
        board_results.append([check_board(b) for b in inp["boards"]])

        if last_to_win is not None:
            if board_results[-1][last_to_win]:
                return calc_winning_board(inp["boards"][last_to_win], num)
        else:
            # Check if all but one board have won
            if len([x for x in board_results[-1] if x]) == len(inp["boards"]) - 1:
                last_to_win = [idx for idx, board in enumerate(board_results[-1]) if not board][0]
    return None

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    inp = read_input("input.txt")
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_ex():
    inp = read_input("input_ex.txt")
    assert solve_p1(inp) == 4512
    inp = read_input("input_ex.txt")
    assert solve_p2(inp) == 1924

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 67716
    inp = read_input("input.txt")
    assert solve_p2(inp) == 1830

if __name__ == '__main__':
    main()
