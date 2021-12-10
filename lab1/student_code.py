
import common
def df_search(map):
        found = False
        visited = [[0 for i in range(common.constants.MAP_WIDTH)] for j in range(common.constants.MAP_HEIGHT)]
        parent = [[0 for i in range(common.constants.MAP_WIDTH)] for j in range(common.constants.MAP_HEIGHT)]
        frontier = []
        width = common.constants.MAP_WIDTH
        height = common.constants.MAP_HEIGHT

        for i in range(width):
                for j in range(height):
                        if map[j][i] == 2:
                                spos = [j,i]
        frontier.insert(0,spos)

        while (len(frontier) != 0):
              node = frontier.pop()
              y = node[0]
              x = node[1]
              if map[y][x] == 3:
                      found = True
                      break
              visited[y][x] = 1
              map[y][x] = 4
              for a in range(4):

                      if (a == 0):
                              x_inc = 0
                              y_inc = -1
                      elif (a == 1):
                              x_inc = -1
                              y_inc = 0
                      elif (a == 2):
                              x_inc = 0
                              y_inc = 1
                      elif (a == 3):
                              x_inc = 1
                              y_inc = 0
                      xchild = x + x_inc
                      ychild = y + y_inc

                      if(ychild>=0 and xchild >=0 and ychild < height and xchild < width and map[ychild][xchild] != 1 and visited[ychild][xchild] == 0):
                              frontier.append([ychild,xchild])
                              parent[ychild][xchild] = [y,x]
        if found:
                while(parent[y][x] != 0):
                        map[y][x] = 5
                        temp = (parent[y][x])[0]
                        x = (parent[y][x])[1]
                        y = temp
                map [y][x] = 5
        return found

def bf_search(map):
        found = False
        visited = [[0 for i in range(common.constants.MAP_WIDTH)] for j in range(common.constants.MAP_HEIGHT)]
        parent = [[0 for i in range(common.constants.MAP_WIDTH)] for j in range(common.constants.MAP_HEIGHT)]
        frontier = []
        width = common.constants.MAP_WIDTH
        height = common.constants.MAP_HEIGHT
        ##FIND STARTING POINT
        for i in range(common.constants.MAP_WIDTH):
                for j in range(common.constants.MAP_HEIGHT):
                        if map[j][i] == 2:
                                # y, x
                                spos = [j, i]
        frontier.insert(0, spos)
        
        while (len(frontier) != 0):
                node = frontier.pop(0)
                y = node[0]
                x = node[1]
                if map[y][x] == 3:
                        found = True
                        break
                visited[y][x] = 1
                map[y][x] = 4
                for a in range(4):
                        #SWITCH TABLE TO ENSURE EXPLORATION ORDER
                        if (a == 0): 
                                x_inc = 1
                                y_inc = 0
                        elif (a == 1):
                                y_inc = 1
                                x_inc = 0
                        elif (a == 2):
                                x_inc = -1
                                y_inc = 0
                        elif (a == 3):
                                y_inc = -1
                                x_inc = 0
                        xchild = x + x_inc
                        ychild = y + y_inc

                        if (ychild>= 0 and xchild >=0 and ychild < height and xchild < width and map[ychild][xchild] != 1 and visited[ychild][xchild] == 0):
                                frontier.append([ychild, xchild])
                                parent[ychild][xchild] = [y,x]
        #MARKING PATH W/ 5s
        if found:
                while (parent[y][x] != 0):
                        map[y][x] = 5
                        temp = (parent[y][x])[0]
                        x = (parent[y][x])[1]
                        y = temp
                map[y][x] = 5
        
	# PUT YOUR CODE HERE
	# access the map using "map[y][x]"
	# y between 0 and common.constants.MAP_HEIGHT-1
	# x between 0 and common.constants.MAP_WIDTH-1
        return found

