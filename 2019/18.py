#--- Day 18: Many-Worlds Interpretation ---
from dataclasses import dataclass, field
from collections import namedtuple, Counter
import bisect


@dataclass
class dungeon:
    grid: dict
    items: dict
    start_pos: tuple


def draw(grid):
    ys = [p[1] for p in grid.keys()]
    xs = [p[0] for p in grid.keys()]
    start = (min(xs), min(ys))
    end = (max(xs), max(ys))
    for y in range(start[1], end[1] + 1):
        for x in range(start[0], end[0] + 1):
            c = grid.get((x, y), '#')
            print(c, end='')
        print()
    print()


def vec_add(A, B):
    return tuple(map(sum, zip(A, B)))


def nearby(grid, pos):
    for side in [(0, -1), (1, 0), (0, 1), (-1, 0)]:
        at = vec_add(pos, side)
        tile = grid.get(at, '#')
        if tile != '#':
            yield at


def all_edges(map, pos):
    graph = {}
    visited = dict()
    queue = [(pos, 1)]
    while queue:
        node, depth = queue.pop()
        for next in nearby(map.grid, node):
            if next in visited and visited[node] < depth + 1:
                continue
            else:
                visited[next] = depth + 1
            if next in map.items:
                item = map.items[next]
                graph[item] = depth
                continue
            queue.append((next, depth + 1))
    return graph


def simplify_graph(map):
    graph = {}
    for pos in map.items:
        obj = map.grid[pos]
        graph[obj] = all_edges(map, pos)
    return graph


def parse(s):
    s = s.strip().split('\n')
    out = dungeon({}, {}, ())
    for y in range(len(s)):
        for x in range(len(s[y])):
            c = s[y][x]
            if c == '@':
                out.start_pos = x, y
            if c not in '.#':
                out.items[x, y] = c
            out.grid[x, y] = s[y][x]
    out.grid = simplify_graph(out)
    return out


def available_choices(map, removed, pos):
    locations = map.grid[pos]
    seen = set()
    choices = {}
    gone = removed.union('@')

    queue = list(locations.items())
    index = 0
    while index < len(queue):
        item, source_depth = queue[index]
        index += 1

        seen.add(item)

        if item not in gone:
            if not (item.islower() or item.lower() in gone):
                continue
            if choices.get(item, source_depth) >= source_depth:
                choices[item] = source_depth
            continue
        for edge, depth in map.grid[item].items():
            if edge in seen:
                continue
            queue.append((edge, depth + source_depth))
    return choices


state = namedtuple('state', ['steps', 'position', 'removed'])


def solve(map):
    init = state(0, '@', frozenset())
    queue = [init]
    seen = set()
    index = 0
    keys = frozenset(i for i in map.grid.keys() if i.islower())
    while True:
        node = queue[index]
        index += 1

        if node[1:] in seen:
            continue
        seen.add(node[1:])

        if not keys.difference(node.removed):
            return node

        choices = available_choices(map, node.removed,
                                    node.position)
        for object, distance in choices.items():
            removed = node.removed.union(object)
            if (object, removed) in seen: continue
            step = state(node.steps + distance, object, removed)

            bisect.insort(queue, step, index)


t1 = """
########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################"""
t1 = parse(t1)

t2 = """
########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################"""
t2 = parse(t2)

t3 = """
########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################"""
t3 = parse(t3)

t4 = """
#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################"""
t4 = parse(t4)

p = open('18.input').read()
p = parse(p)

# part1 = solve(p)
# print(part1.steps)
