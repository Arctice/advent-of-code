# --- Day 19: Tractor Beam ---

import intcode
import time
from functools import lru_cache

data = open('19.input', 'r').read().strip().split(',')
tape = list(map(int, data))


@lru_cache(None)
def check(x, y):
    bot = intcode.intcode(tape.copy())
    bot.input += [x, y]
    bot = intcode.advance(bot)
    return bot.output.pop(0)


def part1():
    count = 0
    for y in range(50):
        for x in range(50):
            r = check(x, y)
            c = '#' if r else '.'
            if r:
                count += 1
            # print(c, end='')
        # print()
    return count


def find_bounds():
    grid = {}
    min_above = 100, 1
    max_below = 1, 100
    for x in range(1, 50):
        for y in range(1, 50):
            if check(x + 1, y) and not check(x, y):
                if (y / x) < (max_below[1] / max_below[0]):
                    max_below = x, y
            if check(x, y + 1) and not check(x, y):
                if (y / x) > (min_above[1] / min_above[0]):
                    min_above = x, y
    return max_below, min_above


def find_square(low, high, size):
    lower_a = low[1] / low[0]
    upper_a = high[1] / high[0]
    x2 = 0
    while True:
        x2 += 1
        estimated_upper_bound = upper_a * x2
        y1 = int(estimated_upper_bound)
        while not (check(x2, y1) and not (check(x2, y1 - 1))):
            y1 += 1
        x1 = x2 - size + 1
        y2 = y1 + size - 1
        if check(x1, y2):
            return (x1, y1)


def part2():
    square = 100
    square_pos = find_square(*find_bounds(), square)
    return square_pos[0] * 10000 + square_pos[1]


print(part1())
print(part2())
