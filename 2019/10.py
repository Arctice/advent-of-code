import math

def locations(map):
    for y, row in enumerate(map):
        for x, val in enumerate(row):
            if val == '#':
                yield (x, y)


def vec_sub(A, B):
    return (A[0] - B[0], A[1] - B[1])


def simplify(vec):
    x, y = vec
    gcd = math.gcd(x, y)
    return (x // gcd, y // gcd)


def count_visible(from_, all_):
    vision = set()
    for to in all_:
        if from_ == to:
            continue
        line = vec_sub(to, from_)
        angle = simplify(line)
        vision.add(angle)
    return len(vision)


def best_observer(map_):
    best = max(map_, key=lambda v: count_visible(v, map_))
    return best


def spin_sort(v, map_):
    points = []
    for u in map_:
        d = simplify(vec_sub(u, v))
        d_ = d[0], -d[-1]
        r = math.atan2(*d_)
        if r < 0:
            r += math.pi * 2
        points.append((u, r))
    points.sort(key=lambda t: t[1])
    return [u for u, r in points]


def group_salvos(v, map_):
    targets = {}
    obscured = []
    for u in map_:
        if u == v:
            continue
        a = simplify(vec_sub(u, v))
        if a not in targets:
            targets[a] = u
            continue
        u_ = targets[a]
        d1 = vec_sub(u, v)
        d2 = vec_sub(u_, v)
        dist1 = d1[0]**2 + d1[1]**2
        dist2 = d2[0]**2 + d2[1]**2
        if dist1 < dist2:
            closer, farther = u, u_
        else:
            closer, farther = u_, u
        targets[a] = closer
        obscured.append(farther)
    salvo = list(targets.values())
    salvos = [salvo]
    if obscured:
        salvos += group_salvos(v, obscured)
    return salvos


def cannon_sort(observer, asteroids):
    spin_sort((8, 3), t3)
    salvos = group_salvos(observer, asteroids)
    ordered_groups = [spin_sort(observer, group) for group in salvos]
    total_order = sum(ordered_groups, [])
    return total_order


def part1(map_):
    best = best_observer(map_)
    return best, count_visible(best, map_)


def part2(map_):
    cannon = best_observer(map_)
    return cannon_sort(cannon, map_)


t1 = """
.#..#
.....
#####
....#
...##""".strip().split('\n')
t1 = list(locations(t1))

t2 = """
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####""".strip().split('\n')
t2 = list(locations(t2))

t3 = """
.#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....X...###..
..#.#.....#....##""".strip().split('\n')
t3 = list(locations(t3))

t4 = """
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##""".strip().split('\n')
t4 = list(locations(t4))


data = open('10.input', 'r').read().strip().split('\n')
data = list(locations(data))

print(part1(data))
print(part2(data)[199])
