from collections import deque
import sys

def make(l, r):
    return deque(l), deque(r)

def addl(tri, x):
    l, r = tri
    l.appendleft(x)
    if len(l) > len(r):
        r.appendleft(l.pop())

def addm(tri, x):
    l, r = tri
    if len(l) == len(r):
        r.appendleft(x)
    else:
        l.append(r.popleft())
        r.appendleft(x)

def addr(tri, x):
    l, r = tri
    if len(l) < len(r):
        l.append(r.popleft())
    r.append(x)

def popl(tri):
    l, r = tri
    if len(l) < len(r):
        l.append(r.popleft())
    return l.popleft()

def popm(tri):
    l, r = tri
    x = r.popleft()
    if len(r) < len(l):
        r.appendleft(l.pop())
    return x

def popr(tri):
    l, r = tri
    x = r.pop()
    if len(r) < len(l):
        r.appendleft(l.pop())
    return x


# source = sys.stdin.read().strip().split('\n')

source = """5
2 1 5 3 4
9
pop left
pop middle
append right 6
append right 7
pop middle
append left 8
append middle 9
pop right
pop middle""".split('\n')

# n = int(source[0])
# op_count = source[2]
xs = [x for x in source[1].split()]
tri = make(xs[:len(xs)//2], xs[len(xs)//2:])

for line in source[3:]:
    op = line.split()
    # print(tri)
    # print(op)
    if len(op) == 3:
        x = op[-1]
        if op[1] == 'left':
            addl(tri, x)
        elif op[1] == 'middle':
            addm(tri, x)
        elif op[1] == 'right':
            addr(tri, x)
    elif len(op) == 2:
        if op[1] == 'left':
            print(popl(tri))
        elif op[1] == 'middle':
            print(popm(tri))
        elif op[1] == 'right':
            print(popr(tri))
            
    
    



