import common


def part_one_classifier(data_train, data_test):
	# PUT YOUR CODE HERE
	# Access the training data using "data_train[i][j]"
	# Training data contains 3 cols per row: X in 
	# index 0, Y in index 1 and Class in index 2
	# Access the test data using "data_test[i][j]"
	# Test data contains 2 cols per row: X in 
	# index 0 and Y in index 1, and a blank space in index 2 
	# to be filled with class
	# The class value could be a 0 or a 1
	w = [0,0,0]
	classified_correctly = False
	while not classified_correctly:
		classified_correctly = True
		for t in range(common.constants.TRAINING_SIZE):
			t = data_train[t]
			c = classify(t, w)
			if c != t[2]:
				classified_correctly = False
				if c == 1:
					w[0] -= t[0]
					w[1] -= t[1]
					w[2] -= 1
				if c == 0:
					w[0] += t[0]
					w[1] += t[1]
					w[2] += 1
	print("w = " + str(w))
	for t in range(common.constants.TEST_SIZE):
		t = data_test[t]
		t[2] = classify(t, w)
	return

def classify(t,w):
	dp = t[0] * w[0] + t[1] * w[1] + w[2]
	if dp >= 0:
		return 1
	else:
		return 0

def part_two_classifier(data_train, data_test):
	# PUT YOUR CODE HERE
	# Access the training data using "data_train[i][j]"
	# Training data contains 3 cols per row: X in 
	# index 0, Y in index 1 and Class in index 2
	# Access the test data using "data_test[i][j]"
	# Test data contains 2 cols per row: X in 
	# index 0 and Y in index 1, and a blank space in index 2 
	# to be filled with class
	# The class value could be a 0 or a 8
	w = [[0,0,0] for x in range(10)]
	classified_correctly = False
	while not classified_correctly:
		classified_correctly = True
		for t in range(common.constants.TRAINING_SIZE):
			t = data_train[t]
			c = classifyy(t,w)
			if c != t[2]:
				w[c][0] -= t[0]
				w[c][1] -= t[1]
				w[c][2] -= 1
				w[int(t[2])][0] += t[0]
				w[int(t[2])][1] += t[1]
				w[int(t[2])][2] += 1
				classified_correctly = False
	for t in range(common.constants.TEST_SIZE):
		t = data_test[t]
		t[2] = classifyy(t, w)
	return

def classifyy(t,w):
	max = float('-inf')
	index = -1
	for i in range(10):
		dp = t[0] * w[i][0] + t[1] * w[i][1] + w[i][2]
		if dp > max:
			max = dp
			index = i
	return index

