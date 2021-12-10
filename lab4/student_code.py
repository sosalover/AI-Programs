import common

def minmax_tictactoe(board, turn):
        y = prune(board,turn, float('-inf'), float('inf'), False)
        if y== -1: return 2
        else: return y

def abprun_tictactoe(board, turn):
	#put your code here:
	#it must return common.constants.X(1), common.constants.O(2) or common.constants.NONE(0) for tie.
	#use the function common.game_status(board), to evaluate a board
	#it returns common.constants.X(1) if X wins, common.constants.O(2) if O wins or common.constants.NONE(0) if tie or game is not finished
	#the program will keep track of the number of boards evaluated
	#result = common.game_status(board);
        
        y  = prune(board, turn, float('-inf'), float('inf'), True)
        if y == -1: return 2
        else: return y
def prune(board, turn, a, b, p):
        board_status = common.game_status(board)
        board_full = True
        n = len(board)
        if (board_status==1):
                return board_status
        elif (board_status==2):
                return -1
        else:
                for i in range(n):
                        if board[i] == 0:
                                board_full = False
                if board_full :
                        return board_status
        #common.print_board(board)
        if turn == common.constants.X:
                value = float('-inf')
                for i in range(n):
                        if board[i] == common.constants.NONE:
                                board[i] = common.constants.X
                                #common.print_board(board)
                                temp_val = prune(board, common.constants.O, a, b, p)
                                if temp_val > value:
                                        value = temp_val
                                board[i] = common.constants.NONE
                                if value >= b and p:
                                        return value
                                if value > a:
                                        a = value
                return value
        elif turn == common.constants.O:
                value = float('inf')
                for i in range(n):
                        if board[i] == common.constants.NONE:
                                board[i] = common.constants.O
                                #common.print_board(board)
                                temp_val = prune(board, common.constants.X, a, b, p)
                                if temp_val <value:
                                        value = temp_val
                                board[i] = common.constants.NONE
                                if value <= a and p:
                                        return value
                                if value < b:
                                        b = value
                return value
                
                                
