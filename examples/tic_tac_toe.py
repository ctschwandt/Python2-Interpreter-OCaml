# Interactive N x N Tic-Tac-Toe for the OCaml Python-subset interpreter.
# Adapted from Cole Schwandt's original game: X is human, O is OCaml.
# OCaml tries to win, then block X, then plays the first available square.

EMPTY = " "
HUMAN = "X"
CPU = "O"
HOLE = "@"


def in_bounds(r, c, n):
    return r >= 0 and r < n and c >= 0 and c < n


def is_playable(board, r, c, n):
    if not in_bounds(r, c, n):
        return False
    return board[r][c] == EMPTY


def make_board(n):
    board = []
    for r in range(n):
        row = []
        for c in range(n):
            row = row + [EMPTY]
        board = board + [row]
    return board


def print_board(board, n):
    bar = "-"
    for c in range(n - 1):
        bar += "+-"
    for r in range(n):
        line = board[r][0]
        for c in range(1, n):
            line += "|" + board[r][c]
        print(line)
        if r < n - 1:
            print(bar)


def count_dir(board, player, r, c, dr, dc, n):
    r += dr
    c += dc
    count = 0
    while in_bounds(r, c, n):
        if board[r][c] != player:
            return count
        count += 1
        r += dr
        c += dc
    return count


def check_dir(board, player, r, c, dr, dc, n):
    ahead = count_dir(board, player, r, c, dr, dc, n)
    behind = count_dir(board, player, r, c, -dr, -dc, n)
    return 1 + ahead + behind >= n


def would_win(board, player, r, c, n):
    if not is_playable(board, r, c, n):
        return False
    if check_dir(board, player, r, c, 1, 0, n):
        return True
    if check_dir(board, player, r, c, 0, 1, n):
        return True
    if check_dir(board, player, r, c, 1, 1, n):
        return True
    if check_dir(board, player, r, c, 1, -1, n):
        return True
    return False


def find_winning_move(board, player, n):
    for r in range(n):
        for c in range(n):
            if would_win(board, player, r, c, n):
                return [r, c]
    return [-1, -1]


def cpu_move(board, n):
    move = find_winning_move(board, CPU, n)
    if move[0] != -1:
        return move
    move = find_winning_move(board, HUMAN, n)
    if move[0] != -1:
        return move
    for r in range(n):
        for c in range(n):
            if is_playable(board, r, c, n):
                return [r, c]
    return [-1, -1]


print("Tic-Tac-Toe: You are X; OCaml is O.")
n = int(input("Board size (3 recommended): "))
if n < 1:
    print("Board size must be positive.")
else:
    board = make_board(n)
    moves_left = n * n
    holes_left = int(input("Number of holes (0 for none): "))
    if holes_left < 0 or holes_left >= moves_left:
        print("Invalid hole count; using zero holes.")
        holes_left = 0
    while holes_left > 0:
        row = int(input("Hole row (0-based): "))
        col = int(input("Hole col (0-based): "))
        if is_playable(board, row, col, n):
            board[row][col] = HOLE
            moves_left -= 1
            holes_left -= 1
        else:
            print("Invalid hole location; try again.")

    unused = print_board(board, n)
    state = 0
    while state == 0:
        row = int(input("Your row (0-based): "))
        col = int(input("Your col (0-based): "))
        if is_playable(board, row, col, n):
            human_wins = would_win(board, HUMAN, row, col, n)
            board[row][col] = HUMAN
            moves_left -= 1
            print("You played:")
            unused = print_board(board, n)
            if human_wins:
                state = 1
            elif moves_left == 0:
                state = 3
            else:
                move = cpu_move(board, n)
                if move[0] == -1:
                    state = 3
                else:
                    cr = move[0]
                    cc = move[1]
                    cpu_wins = would_win(board, CPU, cr, cc, n)
                    board[cr][cc] = CPU
                    moves_left -= 1
                    print("OCaml played:")
                    unused = print_board(board, n)
                    if cpu_wins:
                        state = 2
                    elif moves_left == 0:
                        state = 3
        else:
            print("That square is unavailable. Try again.")

    if state == 1:
        print("You win!")
    elif state == 2:
        print("OCaml wins!")
    else:
        print("It's a draw!")
