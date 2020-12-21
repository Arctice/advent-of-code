#!/bin/python3
from pprint import pprint


def parse(line):
    ingredients, allergens = line.split(" (contains ")
    return allergens.strip()[:-1].split(", "), ingredients.split()


foods = [parse(l) for l in open('21.input').readlines()]

constraints = {}
for allergens, ingredients in foods:
    for a in allergens:
        carriers = set(ingredients)
        constraints.setdefault(a, carriers).intersection_update(carriers)

allergens = {}
while constraints:
    fixed = [a for a in constraints if len(constraints[a]) == 1]
    for a in fixed:
        allergens[a] = constraints.pop(a).pop()
        for b in constraints:
            constraints[b].difference_update({allergens[a]})

# part 1
good_count = 0
for _, ingredients in foods:
    good_count += len(set(ingredients).difference(allergens.values()))
print(good_count)

# part 2
print(','.join([b for a, b in sorted(allergens.items())]))
