#!/bin/python3

# crappy quick python at 6 am

g = open('6.input').read().split('\n\n')

zs = [z.replace('\n', '') for z in g]

zs = [set(z) for z in zs]

sigma = 0
for z in zs:
    sigma += len(z)

print(sigma)


zs = [z.splitlines() for z in g]


sigma = 0

for z in zs:
    all_ = set(z[0])
    for l in z:
        all_ = all_.intersection(set(l))
    sigma += len(all_)


print(sigma)
