import common

def drone_flight_planner (map,policies, values, delivery_fee, battery_drop_cost, dronerepair_cost, discount):
    # PUT YOUR CODE HERE
    # access the map using "map[y][x]"
    # access the policies using "policies[y][x]"
    # access the values using "values[y][x]"
    # y between 0 and 5
    # x between 0 and 5
    # function must return the value of the cell corresponding to the starting position of the drone
    #
    ns = [[0 for x in range (2)] for x in range(4)]
    values1 = [[0 for x in range(6)] for x in range(6)]
    while (True):
        for i in range (6):
            for j in range(6):
                values[i][j] = values1[i][j]

        for y in range(6):
            for x in range(6):
                max_q = float('-inf')
                a = 0
                if map[y][x] == 2:
                    if delivery_fee > max_q:
                        max_q = delivery_fee
                        policies[y][x] = a
                elif map[y][x] == 3:
                    if -dronerepair_cost > max_q:
                        max_q = -dronerepair_cost
                        policies[y][x] = a

                for a in range(1,9):
                    if map[y][x] != 3:
                        value = 0
                        ns[0][0] = y + 1
                        ns[0][1] = x
                        ns[1][0] = y
                        ns[1][1] = x - 1
                        ns[2][0] = y - 1
                        ns[2][1] = x
                        ns[3][0] = y
                        ns[3][1] = x + 1
                        for nsy in range(4):
                            p = T(y, x, a, ns, nsy, map)
                            r = R(y,x, a, battery_drop_cost)
                            value += p*(r + discount * values[ns[nsy][0]][ns[nsy][1]])
                        if value > max_q:
                            max_q = value
                            policies[y][x] = a
                values1 [y][x] = max_q
        converged = True
        for i in range(6):
            for j in range(6):
                if abs(values[i][j] - values1 [i][j]) > 0.000001:
                    converged = False
        if converged:
            break

    for i in range(6):
        for j in range(6):
            if map[i][j] == 1:
                starty = i
                startx = j
    return values1[starty][startx]

def T(y, x, a, ns, nsy, map):
    south = False
    north = False
    east = False
    west = False
    if ns[nsy][0] > y: south = True
    elif ns[nsy][0] < y: north = True
    elif ns[nsy][1] > x: east = True
    elif ns[nsy][1] < x: west = True
    if a == 1 and south:
        if ns[nsy][0] > 5:
            ns[nsy][0] = 5
        return 0.7
    if a == 1 and east:
        if ns[nsy][1] > 5:
            ns[nsy][1] = 5
        return 0.15
    if a == 1 and west:
        if ns[nsy][1] < 0:
            ns[nsy][1] = 0
        return 0.15
    if a == 1 and north:
        if ns[nsy][0] < 0:
            ns[nsy][0] = 0
        return 0
    if a == 2 and south:
        if ns[nsy][0] > 5:
            ns[nsy][0] = 5
        return 0.15
    if a == 2 and east:
        if ns[nsy][1] > 5:
            ns[nsy][1] = 5
        return 0
    if a == 2 and west:
        if ns[nsy][1] < 0:
            ns[nsy][1] = 0
        return 0.7
    if a == 2 and north:
        if ns[nsy][0] < 0:
            ns[nsy][0] = 0
        return 0.15
    if a == 3 and south:
        if ns[nsy][0] > 5:
            ns[nsy][0] = 5
        return 0
    if a == 3 and east:
        if ns[nsy][1] > 5:
            ns[nsy][1] = 5
        return 0.15
    if a == 3 and west:
        if ns[nsy][1] < 0:
            ns[nsy][1] = 0
        return .15
    if a == 3 and north:
        if ns[nsy][0] < 0:
            ns[nsy][0] = 0
        return 0.7
    if a == 4 and south:
        if ns[nsy][0] > 5:
            ns[nsy][0] = 5
        return 0.15
    if a == 4 and east:
        if ns[nsy][1] > 5:
            ns[nsy][1] = 5
        return 0.7
    if a == 4 and west:
        if ns[nsy][1] < 0:
            ns[nsy][1] = 0
        return 0
    if a == 4 and north:
        if ns[nsy][0] < 0:
            ns[nsy][0] = 0
        return 0.15
    if a == 5 and south:
        if ns[nsy][0] > 5:
            ns[nsy][0] = 5
        return 0.8
    if a == 5 and east:
        if ns[nsy][1] > 5:
            ns[nsy][1] = 5
        return 0.1
    if a == 5 and west:
        if ns[nsy][1] < 0:
            ns[nsy][1] = 0
        return 0.1
    if a == 5 and north:
        if ns[nsy][0] < 0:
            ns[nsy][0] = 0
        return 0
    if a == 6 and south:
        if ns[nsy][0] > 5:
            ns[nsy][0] = 5
        return 0.1
    if a == 6 and east:
        if ns[nsy][1] > 5:
            ns[nsy][1] = 5
        return 0
    if a == 6 and west:
        if ns[nsy][1] < 0:
            ns[nsy][1] = 0
        return 0.8
    if a == 6 and north:
        if ns[nsy][0] < 0:
            ns[nsy][0] = 0
        return 0.1
    if a == 7 and south:
        if ns[nsy][0] > 5:
            ns[nsy][0] = 5
        return 0
    if a == 7 and east:
        if ns[nsy][1] > 5:
            ns[nsy][1] = 5
        return 0.1
    if a == 7 and west:
        if ns[nsy][1] < 0:
            ns[nsy][1] = 0
        return .1
    if a == 7 and north:
        if ns[nsy][0] < 0:
            ns[nsy][0] = 0
        return 0.8
    if a == 8 and south:
        if ns[nsy][0] > 5:
            ns[nsy][0] = 5
        return 0.1
    if a == 8 and east:
        if ns[nsy][1] > 5:
            ns[nsy][1] = 5
        return 0.8
    if a == 8 and west:
        if ns[nsy][1] < 0:
            ns[nsy][1] = 0
        return 0
    if a == 8 and north:
        if ns[nsy][0] < 0:
            ns[nsy][0] = 0
        return 0.1
def R(y, x, a, battery_drop_cost):
    if a <= 4:
        return -battery_drop_cost
    elif a > 4:
        return -battery_drop_cost*2

