from collections import namedtuple
from itertools import product
from math import gcd

vec3 = namedtuple('vec3', ['x', 'y', 'z'])


def vec_add(A, B):
    return vec3(A.x + B.x, A.y + B.y, A.z + B.z)


moons = [
    vec3(x=-5, y=6, z=-11),
    vec3(x=-8, y=-4, z=-2),
    vec3(x=1, y=16, z=4),
    vec3(x=11, y=11, z=-4)
]

test1 = [
    vec3(x=-1, y=0, z=2),
    vec3(x=2, y=-10, z=-7),
    vec3(x=4, y=-8, z=8),
    vec3(x=3, y=5, z=-1)
]

test2 = [
    vec3(x=-8, y=-10, z=0),
    vec3(x=5, y=5, z=10),
    vec3(x=2, y=-7, z=3),
    vec3(x=9, y=-8, z=-3)
]


def initial_velocities(n):
    return [vec3(0, 0, 0)] * n


def compare(a, b):
    if a > b:
        return 1
    elif b > a:
        return -1
    return 0


def gravity_pull(A, B):
    Avel, Bvel = [], []
    for axis in range(3):
        diff = B[axis] - A[axis]
        change = compare(diff, 0)
        Avel.append(change)
        Bvel.append(-change)
    return vec3(*Avel), vec3(*Bvel)


def gravity(positions):
    velocities = [vec3(0, 0, 0)] * len(positions)
    for A in range(len(positions)):
        for B in range(A, len(positions)):
            Avec = positions[A]
            Bvec = positions[B]
            Avel, Bvel = gravity_pull(Avec, Bvec)
            velocities[A] = vec_add(velocities[A], Avel)
            velocities[B] = vec_add(velocities[B], Bvel)
    return velocities


def advance(positions, velocities):
    acceleration = gravity(positions)
    for n in range(len(positions)):
        velocities[n] = vec_add(velocities[n], acceleration[n])
    for n in range(len(positions)):
        positions[n] = vec_add(positions[n], velocities[n])
    return positions, velocities


def vec_energy(vec):
    return sum(map(abs, vec))


def system_energy(positions, velocities):
    potential = map(vec_energy, positions)
    kinetic = map(vec_energy, velocities)
    energies = [a * b for a, b in zip(potential, kinetic)]
    return sum(energies)


def nbody(positions, iterations):
    positions = positions.copy()
    velocities = initial_velocities(len(positions))
    for _ in range(iterations):
        positions, velocities = advance(positions, velocities)
    return system_energy(positions, velocities)


def part1():
    return nbody(moons, 1000)


### Part 2


def axis_gravity_pull(A, B):
    change = compare(B - A, 0)
    return change, -change


def axis_gravity(positions):
    velocities = [0] * len(positions)
    for A in range(len(positions)):
        for B in range(A, len(positions)):
            Av = positions[A]
            Bv = positions[B]
            change = compare(Bv - Av, 0)
            velocities[A] += change
            velocities[B] -= change
    return velocities


def axis_advance(positions, velocities):
    acceleration = axis_gravity(positions)
    size = len(positions)
    for n in range(size):
        velocities[n] = velocities[n] + acceleration[n]
        positions[n] = positions[n] + velocities[n]
    return positions, velocities


def axis_nbody(axis, positions, iterations):
    positions = [v[axis] for v in positions]
    velocities = [0] * len(positions)
    for _ in range(iterations):
        positions, velocities = axis_advance(positions, velocities)
    return positions, velocities


def axis_cycle(axis, positions):
    positions = [v[axis] for v in positions]
    velocities = [0] * len(positions)

    cycle = 0
    initial = (positions.copy(), velocities.copy())

    system = positions, velocities
    while True:
        system = axis_advance(*system)
        cycle += 1
        if system == initial:
            break
    return cycle


def lcm(a, b):
    return (a * b) // gcd(a, b)


def part2(bodies):
    cycles = []
    for axis in range(3):
        cycles.append(axis_cycle(axis, bodies))
    return lcm(lcm(cycles[0], cycles[1]), cycles[2])

assert (part2(test2)) == 4686774924

print(part1())
print(part2(moons))
