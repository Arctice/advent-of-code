
HALT = 99
ADD = 1
MUL = 2

def run(tape):
    pos = 0
    tape = tape.copy()
    while True:
        op = tape[pos]
        if op == HALT:
            break
        src1 = tape[pos+1]
        src2 = tape[pos+2]
        dst = tape[pos+3]
        A = tape[src1]
        B = tape[src2]
        V = A+B if op == ADD else A*B
        tape[dst] = V
        pos = pos+4
    return tape

def part1(tape):
    tape = tape.copy()
    tape[1] = 12
    tape[2] = 2
    return run(tape)[0]

def part2(tape):
    goal = 19690720
    for n in range(0, 100):
        for v in range(0, 100):
            adjusted = tape.copy()
            adjusted[1] = n
            adjusted[2] = v
            out = run(adjusted)[0]
            if out == goal:
                return n,v

t1 = [1,9,10,3,2,3,11,0,99,30,40,50]
tape = [int(n) for n in open("2.input").read().split(",")]

print(part1(tape))

print(part2(tape))
