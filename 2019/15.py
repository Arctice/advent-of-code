# --- Day 15: Oxygen System ---
import intcode
import time

data = open('15.input', 'r').read().strip().split(',')
tape = list(map(int, data))

sides = {
    1: (0, -1),
    2: (0, 1),
    3: (-1, 0),
    4: (1, 0),
}


def vec_add(A, B):
    return tuple(map(sum, zip(A, B)))


class repair_droid():
    def __init__(self, tape):
        self.state = intcode.intcode(tape.copy())
        self.pos = 0, 0
        self.grid = {self.pos: 1}

    def draw(self):
        grid = self.grid
        ys = [p[1] for p in grid.keys()]
        xs = [p[0] for p in grid.keys()]
        start = (min(xs), min(ys))
        end = (max(xs), max(ys))
        tiles = {0: '#', 1: '.', 2: 'x', 3: '@', 4: '?', 5: 'O'}
        for y in range(start[1], end[1] + 1):
            for x in range(start[0], end[0] + 1):
                c = grid.get((x, y), 4)
                if (x, y) == self.pos:
                    c = 3
                if (x, y) in self.oxygen:
                    c = 5
                print(tiles[c], end='')
            print()
        print()

    def step(self):
        if not self.state.input:
            return True
        last_direction = self.state.input[0]
        next_pos = vec_add(self.pos, sides[last_direction])

        self.state = intcode.advance(self.state)
        if self.state.pos is None:
            return False
        if not self.state.output:
            return True
        output = self.state.output.pop()
        self.grid[next_pos] = output
        if output == 1:
            self.pos = next_pos
        elif output == 2:
            self.pos = next_pos
            self.oxygen = next_pos

    def input(self, input):
        # north (1), south (2), west (3), and east (4)
        self.state.input.append(input)

    def interact(self, input):
        self.input(input)
        v = self.step()
        self.draw()
        return v


def directions(pos):
    for move, vec in sides.items():
        next_pos = vec_add(pos, vec)
        yield next_pos, move


def explore_path(grid, start, goal):
    queue = [(start, [])]
    seen = set()
    while queue:
        node, path = queue.pop(0)
        for neighbour, move in directions(node):
            if neighbour in seen:
                continue
            tile = grid.get(neighbour, '?')
            new_path = path + [move]
            if tile == goal:
                return new_path
            elif tile == 1:
                queue.append((neighbour, new_path))
        seen.add(node)


def explore(bot):
    while True:
        path = explore_path(bot.grid, bot.pos, '?')
        if not path:
            return
        for step in path:
            bot.input(step)
            bot.step()


def part1():
    bot = repair_droid(tape)
    explore(bot)
    return len(explore_path(bot.grid, (0, 0), 2))


def all_neighbours(grid, tiles):
    neighbours = set()
    for node in tiles:
        for adjacent, _ in directions(node):
            if grid.get(adjacent, None) == 1:
                neighbours.add(adjacent)
    return neighbours.difference(tiles)


def oxygen_fill(bot, start):
    filled = {
        start,
    }
    minutes = 0
    while True:
        spread = all_neighbours(bot.grid, filled)
        if not spread:
            return minutes
        filled = filled.union(spread)
        minutes += 1

        # bot.oxygen = filled
        # time.sleep(0.03)
        # bot.draw()


def part2():
    bot = repair_droid(tape)
    explore(bot)
    to_o2 = explore_path(bot.grid, bot.pos, 2)
    for step in to_o2:
        bot.input(step), bot.step()
    oxygen_start = bot.pos
    return oxygen_fill(bot, bot.pos)


print(part1())
print(part2())
