#!/bin/python3


def parse(rule):
    i, vs = rule.split(': ')
    i = int(i)
    if vs == '"a"' or vs == '"b"':
        return i, vs[1]
    vs = [v.strip().split(' ') for v in vs.split('|')]
    vs = tuple([list(int(n) for n in v) for v in vs])
    return i, vs


rules, msgs = open('19.input').read().split('\n\n')
rules = dict([parse(r) for r in rules.splitlines()])


def count_determined(rule):
    if type(rule) == int:
        return 0.1

    if type(rule) == str:
        return 1

    if type(rule) == list or type(rule) == tuple:
        z = 0
        q = 1 / (len(rule) * len(rule))
        for variant in rule:
            z += q * count_determined(variant)
        return z


def determined(i):
    rule = rules[i]
    return count_determined(rule)


def propagate_next(i, r):
    if type(r) == str:
        return
    if type(r) == tuple:
        for n in r:
            propagate_next(i, n)
    if type(r) == list:
        for n in range(len(r)):
            if r[n] == i:
                r[n] = rules[i]


def propagate(i):
    for k, r in rules.items():
        propagate_next(i, r)


def simplified(r):
    if type(r) == str:
        return r
    if type(r) == tuple and len(r) == 1:
        return simplified(r[0])
    if type(r) == list and all([type(c) == str for c in r]):
        return ''.join(r)
    if type(r) == tuple:
        return tuple([simplified(n) for n in r])
    if type(r) == list:
        return list([simplified(n) for n in r])
    return r


def simplify():
    for k, r in rules.items():
        rules[k] = simplified(r)


pq = list(rules.keys())

for i in range(60):
    pq = sorted(pq, key=determined, reverse=True)
    node, *pq = pq
    propagate(node)
    simplify()

for i in sorted(rules.keys(), key=determined):
    print(f'{i:3} {rules[i]} ({determined(i)})')

# 0: 1 2
# 1: "a"
# 2: 1 3 | 3 1
# 3: "b"

# Some rules, like 3: "b", simply match a single character (in this case, b).

# The remaining rules list the sub-rules that must be followed; for example, the rule 0: 1 2 means that to match rule 0, the text being checked must match rule 1, and the text after the part that matched rule 1 must then match rule 2.

# Some of the rules have multiple lists of sub-rules separated by a pipe (|). This means that at least one list of sub-rules must match. (The ones that match might be different each time the rule is encountered.) For example, the rule 2: 1 3 | 3 1 means that to match rule 2, the text being checked must match rule 1 followed by rule 3 or it must match rule 3 followed by rule 1.

# Fortunately, there are no loops in the rules, so the list of possible matches will be finite. Since rule 1 matches a and rule 3 matches b, rule 2 matches either ab or ba. Therefore, rule 0 matches aab or aba.

# Here's a more interesting example:

# 0: 4 1 5
# 1: 2 3 | 3 2
# 2: 4 4 | 5 5
# 3: 4 5 | 5 4
# 4: "a"
# 5: "b"

# Here, because rule 4 matches a and rule 5 matches b, rule 2 matches two letters that are the same (aa or bb), and rule 3 matches two letters that are different (ab or ba).

# Since rule 1 matches rules 2 and 3 once each in either order, it must match two pairs of letters, one pair with matching letters and one pair with different letters. This leaves eight possibilities: aaab, aaba, bbab, bbba, abaa, abbb, baaa, or babb.

# Rule 0, therefore, matches a (rule 4), then any of the eight options from rule 1, then b (rule 5): aaaabb, aaabab, abbabb, abbbab, aabaab, aabbbb, abaaab, or ababbb.

# The received messages (the bottom part of your puzzle input) need to be checked against the rules so you can determine which are valid and which are corrupted. Including the rules and the messages together, this might look like:

# 0: 4 1 5
# 1: 2 3 | 3 2
# 2: 4 4 | 5 5
# 3: 4 5 | 5 4
# 4: "a"
# 5: "b"

# ababbb
# bababa
# abbbab
# aaabbb
# aaaabbb

# Your goal is to determine the number of messages that completely match rule 0. In the above example, ababbb and abbbab match, but bababa, aaabbb, and aaaabbb do not, producing the answer 2. The whole message must match all of rule 0; there can't be extra unmatched characters in the message. (For example, aaaabbb might appear to match rule 0 above, but it has an extra unmatched b on the end.)

# How many messages completely match rule 0?
