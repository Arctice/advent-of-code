import intcode
import collections
from enum import Enum, auto

tape = list(map(int, open('11.input', 'r').read().strip().split(',')))


class directions(Enum):
    N = 0
    E = 1
    S = 2
    W = 3


def vector_add(A, B):
    return tuple(map(sum, zip(A, B)))


def move(position, current_facing, rotation):
    facings = [directions.N, directions.E, directions.S, directions.W]
    direction_i = current_facing.value
    direction_i += 1 if rotation else -1
    new_facing = facings[direction_i % 4]
    sides = [(0, -1), (1, 0), (0, 1), (-1, 0)]
    step = sides[new_facing.value]
    position = vector_add(position, step)
    return position, new_facing


def paint(starting_panel):
    state = intcode.intcode(tape.copy())
    grid = {}
    position = (0, 0)
    facing = directions.N
    grid[position] = starting_panel

    while state.pos is not None:
        color = grid.get(position, 0)
        state.input.append(color)
        state = intcode.advance(state)
        if state.pos is None: break

        paint = state.output.pop()
        grid[position] = paint

        state = intcode.advance(state)
        rotation = state.output.pop()
        position, facing = move(position, facing, rotation)

    return grid


def part1():
    painted = paint(0)
    return len(painted.keys())


def part2():
    painted = paint(1)

    ys = [p[1] for p in painted.keys()]
    xs = [p[0] for p in painted.keys()]
    start = (min(xs), min(ys))
    end = (max(xs), max(ys))

    for y in range(start[1], end[1] + 1):
        for x in range(start[0], end[0] + 1):
            c = painted.get((x, y), 0)
            print('#' if c else ' ', end='')
        print()


print(part1())
part2()
