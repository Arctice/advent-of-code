from collections import Counter

lines = open('11.input').readlines()
lines = [l.strip().split(': ') for l in lines]
cables = {a: b.split() for a, b in lines}

paths = set()
queue = [('you',)]
while queue:
    p = queue.pop()
    if 'out' in p: paths.add(p)
    near = [(n,) + p for n in cables.get(p[0], [])]
    queue = queue + near
print(f'part 1: {len(paths)}')


inverse = {}
for a in cables:
    for b in cables[a]:
        inverse.setdefault(b, []).append(a)

toposort = []
queue = ['svr',]
while queue:
    n = queue.pop()
    toposort += [n]
    near = cables.get(n, ())
    for m in near:
        inverse[m].remove(n)
    queue += [m for m in near if not inverse[m]]

def count_paths(a, b, init_count):
    paths = Counter({a: init_count})
    for n in toposort[:toposort.index(b)]:
        count = paths[n]
        for m in cables.get(n, ()):
            paths[m] += count
    return paths[toposort[toposort.index(b)]]

first = min(toposort.index('dac'), toposort.index('fft'))
second = max(toposort.index('dac'), toposort.index('fft'))

a = count_paths('svr', toposort[first], 1)
b = count_paths(toposort[first], toposort[second], a)
c = count_paths(toposort[second], 'out', b)
print(f'part 2: {c}')
