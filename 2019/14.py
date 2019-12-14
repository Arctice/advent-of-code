# from collections import namedtuple


def parse_reaction(line):
    left, right = line.split('=>')
    lhs = []
    for pair in left.split(','):
        amount, chem = pair.split()
        lhs.append((int(amount), chem))
    amount, chem = right.split()
    rhs = int(amount), chem
    return lhs, rhs


def parse_input(s):
    lines = s.strip().split('\n')
    reactions = [parse_reaction(l) for l in lines]
    return reactions


def input():
    return parse_input(open('14.input', 'r').read())


def productions_for(reactions, chem):
    options = []
    for lhs, rhs in reactions:
        if rhs[1] != chem:
            continue
        options.append((lhs, rhs))
    return options


def ingredients(reactions, chem, amount):
    sources, result = productions_for(reactions, chem)[0]
    received = result[0]
    multiplier = math.ceil(amount / received)
    total = []
    for required, chem in sources:
        total.append((chem, required * multiplier))
    made = received * multiplier
    leftover = max(0, made - amount)
    return total, leftover


def consume_leftovers(target, required, leftovers):
    have = leftovers.get(target, 0)
    used = min(have, required)
    leftovers[target] = have - used
    required = required - used
    return required, leftovers


def produce(reactions, target, amount, leftovers=None):
    if leftovers is None:
        leftovers = {}
    if target == 'ORE':
        return amount
    total_ore = 0
    amount, leftovers = consume_leftovers(target, amount,
                                          leftovers)
    sources, superfluous = ingredients(reactions, target, amount)
    leftovers[target] = leftovers.get(target, 0) + superfluous
    for chem, necessary in sources:
        total_ore += produce(reactions, chem, necessary,
                             leftovers)
    return total_ore


t1 = """
10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL"""
t1 = parse_input(t1)

t2 = """
171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX"""
t2 = parse_input(t2)

r = input()


def part2(r):
    ore = 1000000000000
    upper_bound = 1
    while produce(r, 'FUEL', upper_bound) < ore:
        upper_bound *= 2
    lower_bound = upper_bound // 2
    while upper_bound - lower_bound > 1:
        middle = lower_bound + (upper_bound - lower_bound) // 2
        result = produce(r, 'FUEL', middle)
        if result < ore:
            lower_bound = middle
        elif result > ore:
            upper_bound = middle
        else:
            lower_bound = middle
            break
    return lower_bound


#part1
print(produce(r, 'FUEL', 1))
print(part2(r))
