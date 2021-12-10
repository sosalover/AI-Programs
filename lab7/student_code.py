import common
import math #note, for this lab only, your are allowed to import math

def detect_slope_intercept(image):
	# PUT YOUR CODE HERE
	# access the image using "image[y][x]"
	# where 0 <= y < common.constants.WIDTH and 0 <= x < common.constants.HEIGHT 
	# set line.m and line.b
	# to create an auxiliar bidimentional structure 
	# you can use "space=common.init_space(heigh, width)"
	vote_space = [[0 for x in range(2000)]for x in range(2000)]
	for y in range(common.constants.WIDTH):
		for x in range(common.constants.HEIGHT):
			if image[y][x] == 0:
				for m in range(2000):
					actual_m = (m-1000)*.01
					b = compute_b(y,x,actual_m)
					if b < 1000 and b >= -1000:
						vote_space[m][int(b) + 1000] += 1
	max = -1
	line = common.Line()
	line.m = 0
	line.b = 0
	for m in range(2000):
		for b in range(2000):
			if vote_space[m][b] > max:
				max = vote_space[m][b]
				line.m = (m-1000)*.01
				line.b = b - 1000



	return line
def compute_b(y,x,m):
	return y-m*x
def detect_circles(image):
	# PUT YOUR CODE HERE
	# access the image using "image[y][x]"
	# where 0 <= y < common.constants.WIDTH and 0 <= x < common.constants.HEIGHT 
	# to create an auxiliar bidimentional structure 
	# you can use "space=common.init_space(heigh, width)"
	vote_space = [[0 for x in range(200)] for x in range(200)]
	max_votes = 0
	for y in range(common.constants.WIDTH):
		for x in range(common.constants.HEIGHT):
			if image[y][x] == 0:
				for theta in range(200):
					theta = 2*3.1415926535898/200 * theta
					a = x + 30*math.cos(theta)
					b = y + 30*math.sin(theta)
					if b < 200 and b >= 0 and a <200 and a>=0:
						vote_space[int(a)][int(b)] += 1
						if vote_space[int(a)][int(b)] > max_votes:
							max_votes = vote_space[int(a)][int(b)]

	circles = 0
	threshold = 183
	for a in range(200):
		for b in range(200):
			if vote_space[a][b] > threshold:
				circles += 1
	return circles



