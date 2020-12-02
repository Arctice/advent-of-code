#!/bin/python3

xs = [int(x) for x in open('1.input').read().splitlines()]
ys = [False] * 2020

for x in xs:
    if ys[2020 - x]:
        y = 2020 - x
        print(x * y)
        break
    ys[x] = True

for x in xs:
    for y in xs:
        z = 2020 - x - y
        if z < 0 or not ys[z]:
            continue
        print(x, y, z, x * y * z)
