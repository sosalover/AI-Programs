def order(data):
        for i in range(len(data)):
                for j in range(len(data)-1):
                        if data[j]>data[j+1]:
                                temp = data[j]
                                data[j] = data[j+1]
                                data[j+1] = temp
        return 0

