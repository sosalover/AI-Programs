import common
def astar_search(map):
        found = False
        visited = [[0 for i in range(common.constants.MAP_WIDTH)] for j in range(common.constants.MAP_HEIGHT)]
        parent = [[0 for i in range(common.constants.MAP_WIDTH)] for j in range(common.constants.MAP_HEIGHT)]
        frontier = []
        g = [[0 for i in range(common.constants.MAP_WIDTH)] for j in range(common.constants.MAP_HEIGHT)]
        width = common.constants.MAP_WIDTH
        height = common.constants.MAP_HEIGHT
        for i in range(common.constants.MAP_WIDTH):
                for j in range(common.constants.MAP_HEIGHT):
                        if map[j][i] == 2:
                                spos = [j,i]
                        if map[j][i] == 3:
                                dpos = [j,i]
        g[spos[0]][spos[1]] = 0
        frontier.insert(0,[spos,0,h(spos[0], spos[1], dpos)])
        while (len(frontier)!=0): 
               node = pick_lowest_node(frontier)
               y = node[0][0]
               x = node[0][1]
               this_round_g = g[y][x] + 1 
               if map [y][x] == 3:
                       found = True
                       break
               visited[y][x] = 1
               map[y][x] = 4
               xchild1 = x - 1
               ychild1 = y
               xchild2 = x
               ychild2 = y - 1
               xchild3 = x + 1
               ychild3 = y
               xchild4 = x
               ychild4 = y + 1
               if valid_child(xchild1, ychild1,map,visited):
                       frontier.append([[ychild1, xchild1],this_round_g,h(xchild1,ychild1,dpos)])
                       parent[ychild1][xchild1] =[y,x]
                       g[ychild1][xchild1] = this_round_g

               if valid_child(xchild2, ychild2,map,visited):
                       frontier.append([[ychild2, xchild2],this_round_g,h(xchild2,ychild2,dpos)])
                       parent[ychild2][xchild2] =[y,x]
                       g[ychild2][xchild2] = this_round_g 

               if valid_child(xchild3, ychild3,map,visited):
                       frontier.append([[ychild3, xchild3],this_round_g,h(xchild3,ychild3,dpos)])
                       parent[ychild3][xchild3] =[y,x]
                       g[ychild3][xchild3] = this_round_g 
               if valid_child(xchild4, ychild4,map,visited):
                       frontier.append([[ychild4, xchild4],this_round_g,h(xchild4,ychild4,dpos)])
                       parent[ychild4][xchild4] = [y,x]
                       g[ychild4][xchild4] = this_round_g 

        if found:
                while(parent[y][x]!=0):
                        map[y][x] = 5
                        temp = (parent[y][x])[0]
                        x = (parent[y][x])[1]
                        y = temp
                map[y][x] = 5
        return found
        
def pick_lowest_node(frontier):
        minimum = frontier[0][1] + frontier[0][2]
        min_index = 0
        for i in range(len(frontier)):
                f = frontier[i][1] + frontier[i][2]
                if f == minimum:
                        if frontier[i][0][0]<frontier[min_index][0][0]:
                                min_index = i
                        elif frontier[i][0][1] < frontier[min_index][0][1]:
                                min_index = i
                elif f < minimum:
                        minimum = frontier[i][1] + frontier[i][2]
                        min_index = i
                
        return frontier.pop(min_index)
def h(xchild, ychild, dst):
        ydif = abs(dst[0] - ychild)
        xdif = abs(dst[1] - xchild)
        return xdif + ydif
def valid_child(xchild, ychild, m, v):
        height = common.constants.MAP_HEIGHT
        width = common.constants.MAP_WIDTH
        return ychild>=0 and xchild>=0 and ychild< height and xchild < width and m[ychild][xchild]!= 1 and v[ychild][xchild] ==0
