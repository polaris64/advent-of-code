from collections import deque

def take_turn_deque(board, marble_num):
    points = 0

    # Handle special marbles (multiples of 23)
    if marble_num > 0 and marble_num % 23 == 0:

        # Do not add marble (player adds it to their score)
        points += marble_num

        # Remove marble 7 places counter-clockwise from current (player adds it
        # to their score)

        # Rotate 7 places to the right (CC) then pop the last item and add it
        # to the points
        board.rotate(7)
        points += board.pop()

        # Rotate left (clockwise) so that current marble is the marble after
        # the one just removed
        board.rotate(-1)

    # Handle regular marbles
    else:
        # Rotate left (clockwise) and append a new marble (adds new marble
        # after previous current)
        board.rotate(-1)
        board.append(marble_num)

    return points

def take_turn(board, current, marble_num):
    new_curr = current
    points = 0

    # Handle special marbles (multiples of 23)
    if marble_num > 0 and marble_num % 23 == 0:
        # Do not add marble (player adds it to their score)
        points += marble_num

        # Remove marble 7 places counter-clockwise from current (player adds it
        # to their score)
        curr_idx = board.index(current)
        idx_to_remove = (curr_idx - 7) % len(board)
        points += board[idx_to_remove]
        board.remove(board[idx_to_remove])

        # Current marble is the one immediately clockwise from the removed
        # marble
        new_curr = board[idx_to_remove % len(board)]

    # Handle regular marbles
    else:
        if len(board) == 0:
            board.append(marble_num)
        else:
            curr_idx = board.index(current)
            new_idx = (curr_idx + 1) % len(board)
            if new_idx > len(board) - 1:
                board.append(marble_num)
            else:
                board.insert(new_idx + 1, marble_num)
        new_curr = marble_num

    return (new_curr, points)


def run(player_num, last_marble):

    board = deque()
    # board = []
    current = 0
    marble_num = 0
    players = [0 for x in range(player_num)]

    for i in range(last_marble):
        # player = i % len(players)
        # marble_num = i
        # res = take_turn(board, current, marble_num)
        # current = res[0]
        # players[player] += res[1]

        player = i % len(players)
        marble_num = i
        points = take_turn_deque(board, marble_num)
        players[player] += points

    highest = max(enumerate(players), key=lambda x: x[1])
    print(
        "The maximum score with {} players after the last marble {} is: {} (player {})".format(
            player_num,
            last_marble,
            highest[1],
            highest[0] + 1,
        )
    )
    return highest

def run_full():
    return [
        # 405 players; last marble is worth 71700 points
        run(405, 71700),

        # 100 times more marbles
        run(405, 71700 * 100),
    ]

def run_examples():
    return [
        run(10, 1618)[1],
        run(13, 7999)[1],
        run(17, 1104)[1],
        run(21, 6111)[1],
        run(30, 5807)[1],
    ]


def test_full():
    res = run_full()
    assert res[0][1] == 428690
    assert res[1][1] == 3628143500

def test_examples():
    res = run_examples()
    assert res[0] == 8317
    assert res[1] == 146373
    assert res[2] == 2720 # Site says 2764?
    assert res[3] == 54718
    assert res[4] == 37305


if __name__ == "__main__":
    run_examples()
    run_full()
