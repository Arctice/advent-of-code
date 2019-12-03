from collections import Counter

words = open("2.input").read().strip().split()

def counts(word):
    return Counter(word)

def check_n(word, n):
    for k,v in counts(word).items():
        if v == n:
            return True
    return False

def checksum(words):
    x = len([w for w in words if check_n(w,2)])
    y = len([w for w in words if check_n(w,3)])
    return x*y

print(checksum(words))

def shared(a, b):
    same = []
    for i in range(min(len(a), len(b))):
        if a[i] == b[i]:
            same += [a[i]]
    return "".join(same)


for a in words:
    for b in words:
        same = shared(a,b)
        diff = len(a) - len(same)
        if diff == 1:
            print(same)

