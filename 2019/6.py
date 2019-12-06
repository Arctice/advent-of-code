inputs = open('6.input', 'r').read().strip().split('\n')


def make_tree():
    edges = [l.split(')') for l in inputs]
    graph = {}
    for a, b in edges:
        graph.setdefault(a, []).append(b)
    return graph


tree = make_tree()


def part1(node=None, n=0):
    if node is None:
        node = 'COM'
    return n + sum([part1(child, n + 1) for child in tree.get(node, [])])


def make_graph():
    edges = [l.split(')') for l in inputs]
    graph = {}
    for a, b in edges:
        graph.setdefault(a, []).append(b)
        graph.setdefault(b, []).append(a)
    return graph


def bfs(graph, start, end):
    seen = set()
    queue = [(start, 0)]
    while queue:
        node, path = queue.pop(0)
        if node in seen:
            continue
        seen.add(node)
        if node == end:
            return path
        queue += [(child, path + 1) for child in graph.get(node, [])]


def part2():
    return bfs(make_graph(), 'YOU', 'SAN') - 2


print(part1(), part2())
