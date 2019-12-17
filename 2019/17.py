# --- Day 17: Set and Forget ---

import intcode
import time

data = open('17.input', 'r').read().strip().split(',')
tape = list(map(int, data))


def draw(grid):
    ys = [p[1] for p in grid.keys()]
    xs = [p[0] for p in grid.keys()]
    start = (min(xs), min(ys))
    end = (max(xs), max(ys))
    for y in range(start[1], end[1] + 1):
        for x in range(start[0], end[0] + 1):
            c = grid.get((x, y), ' ')
            print(c, end='')
        print()
    print()


class camera():
    def __init__(self, tape):
        self.state = intcode.intcode(tape.copy())
        self.grid = {}

    def run(self):
        view = []
        while self.state.pos is not None:
            self.state = intcode.advance(self.state)
            if not self.state.output:
                break
            view.append(self.state.output.pop())
        return ''.join(map(chr, view))


def build_grid():
    view = (camera(tape).run())
    x = 0
    y = 0
    grid = {}
    for c in view:
        if c == '\n':
            y += 1
            x = 0
            continue
        grid[(x, y)] = c
        x += 1
    return grid


sides = {
    0: (0, -1),
    1: (1, 0),
    2: (0, 1),
    3: (-1, 0),
}


def vec_add(A, B):
    return tuple(map(sum, zip(A, B)))


def directions(pos):
    for vec in sides.values():
        yield vec_add(pos, vec)


def intersections():
    grid = build_grid()
    for pos, c in grid.items():
        if not c == '#':
            continue
        adjacent = [grid.get(v, '.') for v in directions(pos)]
        if not all(tile == '#' for tile in adjacent):
            continue
        yield pos


def part1():
    sigma = 0
    for x, y in intersections():
        sigma += x * y
    return sigma


def add_direction(vec, d):
    return vec_add(vec, sides[d])


def forward_walk(grid, pos, facing):
    while grid.get(pos, '.') == '#':
        yield pos
        pos = add_direction(pos, facing)


def available_paths(grid, node):
    state, path, visited = node
    visited = visited.copy()
    pos, facing = state
    steps = 0
    for pos in forward_walk(grid, pos, facing):
        visited.add(pos)
        left = (facing - 1) % 4
        tile_left = add_direction(pos, (facing - 1) % 4)
        right = (facing + 1) % 4
        tile_right = add_direction(pos, (facing + 1) % 4)
        if grid.get(tile_left, '.') == '#':
            yield (pos, left), path + [steps, 'L'], visited.copy()
        if grid.get(tile_right, '.') == '#':
            yield (pos, right), path + [steps, 'R'], visited.copy()
        steps += 1
    back = (facing + 2) % 4
    yield (pos, back), path + [steps - 1, 'L', 'L'], visited.copy()


def best_path_astar(grid, start, required):
    queue = [(start, [], set([start[0]]))]
    while queue:
        node = queue.pop(0)
        for position, new_path, visited in reversed(
                list(available_paths(grid, node))):
            if len(visited) >= required:
                return new_path
            queue.append((position, new_path, visited))
        queue.sort(key=lambda node: len(node[1]) / len(node[2]))


def best_path():
    grid = build_grid()
    pos = [pos for pos in grid if grid[pos] in '^>v<'][0]
    facing = {'^': 0, '>': 1, 'v': 2, '<': 3}[grid[pos]]
    grid[pos] = '#'
    tile_count = list(grid.values()).count('#')
    return best_path_astar(grid, (pos, facing), tile_count)


def simulate_simple(path):
    grid = build_grid()
    pos = [pos for pos in grid if grid[pos] in '^>v<'][0]
    facing = {'^': 0, '>': 1, 'v': 2, '<': 3}[grid[pos]]

    for action in path:
        grid[pos] = 'o'
        if action == 'L':
            facing = (facing - 1) % 4
            continue
        if action == 'R':
            facing = (facing + 1) % 4
            continue
        for step in range(action):
            pos = add_direction(pos, facing)
            grid[pos] = 'o'
    draw(grid)


def transform_path(path):
    pathstr = ''.join(map(str, path))
    # encode 10s as Zs
    pathstr = pathstr.replace('10', 'Z').replace('0', '')
    while pathstr[-1] in 'LR':
        pathstr = pathstr[:-1]
    return pathstr


def untransform_path(pathstr):
    path = []
    for c in pathstr:
        if c == 'Z':
            path.append(10)
        elif c.isdigit():
            path.append(int(c))
        else:
            path.append(c)
    return path


def prefixes(program):
    for l in range(2, 1 + min(20, len(program))):
        yield program[:l]


def factor_occurences(program, subseq):
    program = program.copy()
    out = []
    for part in program:
        out += part.replace(subseq, '\0').split('\0')
    return out


def separate_program(program, splits):
    subname = 'CBA' [splits]
    first_substr = program.replace('A', '_').replace('B', '_').split('_')
    first_substr = [s for s in first_substr if s][0]

    for prefix in prefixes(first_substr):
        substituted = program.replace(prefix, subname)
        if splits == 0:
            candidate = substituted
            if set(candidate) == set('ABC'):
                return (prefix, candidate)
        else:
            candidate = separate_program(substituted, splits - 1)
            if not candidate:
                continue
            return (prefix, ) + candidate


class vacuum():
    def __init__(self, tape, program):
        self.grid = {}
        self.state = intcode.intcode(tape.copy())
        self.state.tape[0] = 2
        for section in program:
            section = map(str, section)
            section = ','.join(section) + '\n'
            self.state.input += map(ord, section)

    def run(self):
        self.state.input.append(ord('n'))
        self.state.input.append(ord('\n'))
        out = []
        while self.state.pos is not None:
            self.state = intcode.advance(self.state)
            while self.state.output:
                out.append(self.state.output.pop(0))
        return out[-1]

    def run_with_view(self):
        self.state.input.append(ord('y'))
        self.state.input.append(ord('\n'))
        suffix = '  '
        while self.state.pos is not None:
            self.state = intcode.advance(self.state)
            while self.state.output:
                v = chr(self.state.output.pop(0))
                print(v, end='')
                suffix = suffix[1] + v
                if suffix == '\n\n':
                    time.sleep(0.2)
        return ord(suffix[1])


def part2():
    path = transform_path(best_path())
    program = separate_program(path, 2)
    program = tuple(map(untransform_path, program))
    program = (program[-1], ) + program[:3]
    bot = vacuum(tape, program)
    return bot.run()


path = transform_path(best_path())
program = separate_program(path, 2)
program = tuple(map(untransform_path, program))
program = (program[-1], ) + program[:3]

print(part1())
print(part2())
