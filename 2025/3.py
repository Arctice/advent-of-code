from functools import cache

@cache
def solve(xs, count):
    if not count: return 0
    left = count - 1
    return max(xs[i]*10**left + solve(xs[i + 1:], left) for i in range(len(xs) - left))

banks = [[int(x) for x in l.strip()] for l in open('3.input').readlines()]
part1 = sum(solve(tuple(bank), 2) for bank in banks)
part2 = sum(solve(tuple(bank), 12) for bank in banks)
print(part1, part2)
