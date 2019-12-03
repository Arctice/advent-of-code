#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#   3   00:17:01    378      0   00:19:28    234      0

w = open("3.input").read().strip().split('\n')

A = w[0].split(',')
B = w[1].split(',')

centre = (0,0)

def walk(path):
    direction = {
        'U': (0, -1),
        'R': (1, 0),
        'D': (0, 1),
        'L': (-1, 0),        
    }[path[0]]
    steps = int(path[1:])
    position = (0,0)
    for offset in range(1, steps+1):
        x = (direction[0], direction[1])
        yield x

def fill_grid(wire_path):
    grid = {}
    position = (0,0)
    d = 0
    for instruction in wire_path:
        for step in walk(instruction):
            x = position[0] + step[0]
            y = position[1] + step[1]
            position = x, y
            d = d + 1
            grid[position] = d
    return grid

A = fill_grid(A)
B = fill_grid(B)

def intersections(A, B):
    return set(A.keys()).intersection(set(B.keys()))

points = (intersections(A, B))

dists = [abs(x)+abs(y) for x,y in points]
walk_dists = [A[x,y] + B[x,y] for x,y in points]

print(min(dists))
print(min(walk_dists))
