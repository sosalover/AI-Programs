import common

#helpful, but not needed
class variables:
        counter=0


def sudoku_backtracking(sudoku):
        variables.counter = 0
        backtrack(sudoku)
        return variables.counter
def backtrack(sudoku):
        variables.counter = variables.counter + 1
        #check if board is done
        board_full = True
        for y in range(9):
                for x in range(9):
                        if sudoku[y][x] == 0:
                                board_full = False
        if board_full:
               #print("HELLO")
                return True
        for y in range(9):
                for x in range(9):
                        if sudoku[y][x] == 0:
                                for z in range(1,10):
                                        #print("x = " + str(x) + ", y = " + str (y) + ", z = " + str(z))
                                        if common.can_yx_be_z(sudoku, y, x, z):
                                        #print("CONDITION PASSED")
                                                sudoku[y][x] = z
                                                if backtrack(sudoku):
                                                        #print("BACKTRACK SUCCESSFUL")
                                                        return True
                                                else:
                                                        sudoku[y][x] = 0
                                return False
                                
def sudoku_forwardchecking(sudoku):
        variables.counter = 0
        domain = [[[True for x in range(9)] for x in range(9)] for x in range(9)]
        for y in range(9):
                for x in range(9):
                        if sudoku[y][x] != 0:
                                update_domain(domain, y, x, sudoku[y][x])
        #print(sudoku)
        #print(domain)
        #print(str(all_values_in_domain_not_empty(domain)))
        forwardcheck(sudoku, domain)
        return variables.counter

def update_domain(domain, y, x, z):
        for i in range(9):
                if (domain[y][i][z-1]):
                        domain[y][i][z-1] = False
                if (domain[i][x][z-1]):
                        domain[i][x][z-1]  = False
                if (domain[int(y/3)*3+int(i/3)][int(x/3)*3+i%3][z-1]):
                        domain[int(y/3)*3 + int(i/3)][int(x/3)*3 + i%3][z-1] = False
        for i in range(9):
                domain[y][x][i] = False
        domain[y][x][z-1] = True
        return None

def all_values_in_domain_not_empty(domain):
        #print("check prune")
        for y in range(9):
                for x in range(9):
                        not_empty = False
                        for d in range(9):
                                if domain[y][x][d]:
                                        not_empty = True
                        if not_empty == False:
                               # print("PRUNED")
                                return False
                                
        return True
def domain_copy(domain1, domain2):
        for y in range(9):
                for x in range(9):
                        for z in range(9):
                                domain1[y][x][z] = domain2[y][x][z]
                                
def forwardcheck (sudoku, domain):
        variables.counter = variables.counter + 1
        board_full = True
        for y in range(9):
                for x in range(9):
                               if sudoku[y][x] == 0:
                                       board_full = False
        if board_full:
               # print("SOLVED!")
                return True
        for y in range(9):
                for x in range(9):
                        if sudoku[y][x] == 0:
                                for z in range(1, 10):
                                       if common.can_yx_be_z(sudoku, y, x, z):
                                               sudoku[y][x] = z
                                               temp_domain = [[[False for x in range(9)] for x in range(9)] for x in range(9)]
                                               domain_copy(temp_domain, domain)
                                               update_domain(domain, y, x, z)
                                               if all_values_in_domain_not_empty(domain):
                                                       if forwardcheck(sudoku, domain):
                                                              # print("SOLVED!")
                                                               return True
                                               sudoku[y][x] = 0
                                               domain_copy(domain, temp_domain)
                                return False
                                                       
                               
                               
                               
                                                
