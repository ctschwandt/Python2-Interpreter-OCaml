# File: main.py
# Name: Cole Schwandt

n = 3
size = n * n
moves_left = n

EMPTY = ' '
HUMAN = 'X'
CPU = 'O'
HOLE = '@'

PLAYING = 0
DRAW = 1
HUMAN_WON = 2
CPU_WON = 3
state = PLAYING

def get_hori_bar():
    ret = "-"
    lim = n - 1
    for i in range(lim):
        ret += "+-"
    return ret
    
def print_board(board):
    hori_bar = get_hori_bar()
    for r in range(n):
        vert_delim = ""
        for c in range(n):
            print(f"{vert_delim}{board[r][c]}", end='')
            vert_delim = '|'
        print()
        if r != n-1:
            print(hori_bar)

def println_board(board):
    print_board(board)
    print()

def out_of_bounds(r, c):
    return r < 0 or r >= n or c < 0 or c >= n

def in_bounds(r, c):
    return not out_of_bounds(r, c)

def get(board, r, c):
    return board[r][c]

def check_dir_(board, player, r, c, drc):
    # count matching pieces in this direction (not including (r,c))
    r += drc[0]
    c += drc[1]
    cnt = 0
    while (in_bounds(r, c) and get(board, r, c) == player):
        cnt += 1
        r += drc[0]
        c += drc[1]
    return cnt
    
def check_dir(board, player, r, c, drc):
    # win if we can get n in a row through (r,c) in this direction
    a = check_dir_(board, player, r, c, drc)
    b = check_dir_(board, player, r, c, [-drc[0], -drc[1]])
    return (1 + a + b) == n

def is_empty(board, r, c):
    return get(board, r, c) == EMPTY

def not_empty(board, r, c):
    return not is_empty(board, r, c)

def is_hole(board, r, c):
    return get(board, r, c) == HOLE

def is_playable(board, r, c):
    return in_bounds(r, c) and is_empty(board, r, c)

def would_win(board, player, r, c):
    if not_empty(board, r, c):
        return False
    
    # NW|N|NE = [-1, -1], [-1, 0], [-1, 1]
    # W | | E = [ 0, -1], [ 0, 1]
    # SW|S|SE = [ 1, -1], [ 1, 0], [ 1, 1]
    dirs = [[1, 0], [0, 1], [1, 1], [1, -1]]
    for d in dirs:
        if check_dir(board, player, r, c, d):
            return True
    return False

def cpu_move(board):
    # win if possible
    for r in range(n):
        for c in range(n):
            if is_empty(board, r, c) and not is_hole(board, r, c) and would_win(board, CPU, r, c):
                return [r, c]

    # prevent player win
    for r in range(n):
        for c in range(n):
            if is_empty(board, r, c) and not is_hole(board, r, c) and would_win(board, HUMAN, r, c):
                return [r, c]

    # otherwise, take first empty spot
    for r in range(n):
        for c in range(n):
            if is_empty(board, r, c) and not is_hole(board, r, c):
                return [r, c]

    return None

def update_state_after_move(player, won):
    global state
    if won:
        state = HUMAN_WON if player == HUMAN else CPU_WON
        return
    if moves_left == 0:
        state = DRAW
        return
    state = PLAYING

def place_holes(board):
    global moves_left
    print("Enter hole positions.")
    print("Type the row, then the col.")
    print("Type -1 for the row when you are done placing holes.")
    r = int(input("hole row (-1 to stop): "))
    while r != -1:
        c = int(input("hole col: "))
        if in_bounds(r, c) and is_empty(board, r, c):
            board[r][c] = HOLE
            moves_left -= 1
        else:
            print("invalid hole position")
        r = int(input("hole row (-1 to stop): "))

def human_turn(board):
    global moves_left
    while True:
        r = int(input("row: "))
        c = int(input("col: "))
        if not is_playable(board, r, c):
            print("invalid move")
            continue

        # check for a win BEFORE placing
        won = would_win(board, HUMAN, r, c)
        
        board[r][c] = HUMAN
        moves_left -= 1
        update_state_after_move(HUMAN, won)
        return [r, c]

def cpu_turn(board):
    global moves_left
    mv = cpu_move(board)
    if mv == None:
        return None
    r = mv[0]
    c = mv[1]

    # check for a win BEFORE placing
    won = would_win(board, CPU, r, c)
    
    board[r][c] = CPU
    moves_left -= 1
    update_state_after_move(CPU, won)
    print(f"PYTHON chooses row {r}, col {c}")
    return [r, c]

def play_game(board):
    global state
    while state == PLAYING:
        # human move
        human_turn(board)
        println_board(board)

        if state != PLAYING:
            break

        # cpu move
        mv = cpu_turn(board)
        if mv == None:
            state = DRAW
            break

        println_board(board)

    if state == HUMAN_WON:
        print("you win")
    elif state == CPU_WON:
        print("PYTHON wins")
    else:
        print("it's a draw")

def main():
    global n, size, moves_left, state

    print("Welcome to tic-tac-toe with holes.")
    n = int(input("Enter board size n: "))
    size = n * n
    moves_left = size
    state = PLAYING
    board = [ [EMPTY for c in range(n)] for r in range(n)]
    
    place_holes(board)
    print("Starting board:")
    print_board(board)
    play_game(board)

main()
