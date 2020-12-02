#!/bin/python3


def parse(ss):
    policy, psswd = ss.split(': ')
    range_, ch = policy.split(' ')
    min_, max_ = range_.split('-')
    min_ = int(min_)
    max_ = int(max_)
    return min_, max_, ch, psswd


def valid(x):
    min_, max_, ch, psswd = x
    count = psswd.count(ch)
    return min_ <= count <= max_


# part 1
lines = open('2.input').read().splitlines()
xs = [parse(line) for line in lines]
print(sum([valid(x) for x in xs]))


def valid2(x):
    a, b, ch, psswd = x
    return (psswd[a - 1] == ch) ^ (psswd[b - 1] == ch)


# part 2
print(sum([valid2(x) for x in xs]))
