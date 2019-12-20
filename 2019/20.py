# --- Day 20: Donut Maze ---
from collections import deque


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


def load():
    data = open('20.input', 'r').read()
    x, y = 0, 0
    grid = {}
    for c in data:
        if c == '\n':
            y += 1
            x = 0
            continue
        grid[x, y] = c
        x += 1

    portals = {}
    skip = set()
    for pos, v1 in grid.items():
        if not v1.isalpha():
            continue
        if pos in skip:
            continue
        x, y = pos
        v2 = grid.get((x + 1, y), ' ')
        if v2.isalpha():
            tile = grid.get((x + 2, y))
            skip.add((x + 1, y))
            if tile == '.':
                pos = x + 2, y
            else:
                pos = x - 1, y
        else:
            skip.add((x, y + 1))
            v2 = grid.get((x, y + 1))
            if not v2:
                continue
            tile = grid.get((x, y + 2))
            if tile == '.':
                pos = x, y + 2
            else:
                pos = x, y - 1
        id = v1 + v2
        portals.setdefault(id, []).append(pos)

    return grid, portals


sides = {
    0: (0, -1),
    1: (1, 0),
    2: (0, 1),
    3: (-1, 0),
}


def vec_add(A, B):
    return tuple(map(sum, zip(A, B)))


def bfs():
    grid, portals = load()
    ports = {}
    for id, ps in portals.items():
        if len(ps) != 2: continue
        l, r = ps
        ports[l] = r
        ports[r] = l

    start = portals['AA'][0]
    end = portals['ZZ'][0]
    queue = deque([(start, 0)])
    visited = {}

    while queue:
        node, depth = queue.popleft()

        if node == end:
            return depth

        if node in visited:
            if visited[node] <= depth:
                continue
        visited[node] = depth

        for dir in sides.values():
            adj = vec_add(node, dir)
            tile = grid.get(adj, ' ')
            if tile == '.':
                queue.append((adj, depth + 1))

        if node in ports:
            queue.append((ports[node], depth + 1))


def bfs2():
    grid, portals = load()
    ports = {}
    desc = {}
    for id, ps in portals.items():
        if len(ps) != 2: continue
        l, r = ps
        ports[l] = r
        ports[r] = l
        desc[l] = id
        desc[r] = id

    start = portals['AA'][0]
    end = portals['ZZ'][0]
    queue = deque([(start, 0, 0)])
    visited = {}

    while queue:
        node, level, depth = queue.popleft()

        if node == end and level == 0:
            return depth

        if (node, level) in visited:
            if visited[(node, level)] <= depth:
                continue
        visited[(node, level)] = depth

        for dir in sides.values():
            adj = vec_add(node, dir)
            tile = grid.get(adj, ' ')
            if tile == '.':
                queue.append((adj, level, depth + 1))

        if node in ports:
            port_pos = ports[node]
            external = node[1] < 10 or node[1] > 130 or node[0] < 5 or node[
                0] > 125
            if not external:
                queue.append((ports[node], level + 1, depth + 1))
            elif level > 0:
                queue.append((ports[node], level - 1, depth + 1))


print(bfs())
print(bfs2())
