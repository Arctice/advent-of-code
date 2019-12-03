series = [int(n) for n in open("1.input").read().split()]
part1 = sum(series)

def part2(series):
    seen = set()
    i = 0
    value = 0
    while True:
        value += series[i]
        if value in seen:
            return value
        seen.add(value)
        i = (i + 1)%len(series)

print("part1:", part1)
print("part2:", part2(series))
