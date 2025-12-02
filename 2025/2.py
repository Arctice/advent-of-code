ls = [x for x in open('2.input').read().splitlines()][0]
rs = ls.split(',')
rs = [(int(a), int(b)) for a, b in [r.split('-') for r in rs]]
sum = 0
for a, b in rs:
    for x in range(a, b+1):
        s = str(x)
        for rl in range(1, len(s)):
            patt = s[:rl]
            rest = s
            count = 0
            while len(rest) >= rl:
                if rest[:rl] == patt:
                    rest = rest[rl:]
                    count += 1
                    continue
                count = 0
                break
            if not rest == '':
                continue
            if count > 1:
                sum += x
                break
print(sum)
