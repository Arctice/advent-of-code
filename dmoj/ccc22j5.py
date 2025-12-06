import sys

source = sys.stdin

lines = [tuple(int(x) for x in line.strip().split()) for line in source]
size = lines[0][0]
trees = [(x, y) for (y, x) in lines[2:]]

xtrees = sorted(trees, key = lambda t: t[0])
ytrees = sorted(trees, key = lambda t: t[1])
xtrees = [(0, 0)] + xtrees + [(size + 1, size + 1)]

max_size = 0
for first in range(len(xtrees)):
    west = xtrees[first][0]
    for second in range(first + 1, len(xtrees)):
        east = xtrees[second][0]
        width = east - west
        if width < max_size:
            continue
        inbetween = [(x, y) for x, y in ytrees if west < x < east]
        inbetween =  [(0, 0)] + inbetween + [(size + 1, size + 1)]
        for space in range(len(inbetween) - 1):
            north = inbetween[space][1]
            south = inbetween[space + 1][1]
            height = south - north
            square = min(width, height) - 1
            max_size = max(max_size, square)
print(max_size)
