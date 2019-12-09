from dataclasses import dataclass


@dataclass
class intcode:
    tape: list
    pos: int
    input: list
    output: list
    base: int


def decode_op(instruction):
    op = instruction % 100
    instruction //= 100
    modes = [0] * 3
    argument_index = 0
    while instruction > 0:
        mode = instruction % 10
        modes[argument_index] = mode
        instruction //= 10
        argument_index += 1
    return op, modes


def tape_read(state, src, mode):
    if mode == 1:
        src = src
    if mode == 0:
        src = state.tape[src]
    if mode == 2:
        src = state.tape[src] + state.base

    while len(state.tape) <= src:
        state.tape.append(0)
    return state.tape[src]


def tape_write(state, dest, val, mode):
    dest = state.tape[dest]
    if mode == 2:
        dest += state.base
    while len(state.tape) <= dest:
        state.tape.append(0)
    state.tape[dest] = val


def op_arithmetic(op):
    def interpret(state, modes):
        A = tape_read(state, state.pos + 1, modes[0])
        B = tape_read(state, state.pos + 2, modes[1])
        val = op(A, B)
        tape_write(state, state.pos + 3, val, modes[2])
        state.pos += 4
        return state

    return interpret


def op_read(state, modes):
    value = state.input.pop(0)
    tape_write(state, state.pos + 1, value, modes[0])
    state.pos += 2
    return state


def op_write(state, modes):
    value = tape_read(state, state.pos + 1, modes[0])
    state.output.append(value)
    state.pos += 2
    return state


def op_jnz(state, modes):
    pred = tape_read(state, state.pos + 1, modes[0])
    if pred != 0:
        dest = tape_read(state, state.pos + 2, modes[1])
        state.pos = dest
    else:
        state.pos += 3
    return state


def op_jz(state, modes):
    pred = tape_read(state, state.pos + 1, modes[0])
    if pred == 0:
        dest = tape_read(state, state.pos + 2, modes[1])
        state.pos = dest
    else:
        state.pos += 3
    return state


def op_relative_offset(state, modes):
    state.base += tape_read(state, state.pos + 1, modes[0])
    state.pos += 2
    return state


def op_halt(state, modes):
    state.pos = None
    return state


instructions = {
    1: op_arithmetic(lambda a, b: a + b),
    2: op_arithmetic(lambda a, b: a * b),
    3: op_read,
    4: op_write,
    5: op_jnz,
    6: op_jz,
    7: op_arithmetic(lambda a, b: 1 if a < b else 0),
    8: op_arithmetic(lambda a, b: 1 if a == b else 0),
    9: op_relative_offset,
    99: op_halt,
}


def advance(state):
    while state.pos is not None and not state.output:
        op, modes = decode_op(state.tape[state.pos])
        state = instructions[op](state, modes)
    return state


def run(tape, input):
    state = intcode(tape.copy(), 0, input, [], 0)
    out = []
    while state.pos is not None:
        state = advance(state)
        out += state.output
        state.output = []
    return state, out


t0 = [3, 0, 4, 0, 99]
t1 = [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]
t2 = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
t3 = [3, 3, 1108, -1, 8, 3, 4, 3, 99]
t4 = [3, 3, 1107, -1, 8, 3, 4, 3, 99]
t5 = [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9]
t6 = [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]
t7 = [
    3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0,
    36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46,
    1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99
]
t8 = [int(n) for n in open("5.input").read().split(",")]

quine = [
    109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99
]
big = [1102, 34915192, 34915192, 7, 4, 7, 99, 0]

tests = [
    (t0, [123], [123]),
    (t1, [0], [0]),
    (t1, [-1], [1]),
    (t2, [8], [1]),
    (t2, [4], [0]),
    (t3, [-8], [0]),
    (t3, [8], [1]),
    (t4, [7], [1]),
    (t4, [8], [0]),
    (t4, [9], [0]),
    (t5, [0], [0]),
    (t5, [5], [1]),
    (t6, [0], [0]),
    (t6, [1], [1]),
    (t7, [7], [999]),
    (t7, [8], [1000]),
    (t7, [9], [1001]),
    (big, [], [1219070632396864]),
]


def run_tests():
    test_i = 0
    for tape, input, expected in tests:
        result = run(tape.copy(), input.copy())
        if result[1] != expected:
            print(test_i, 'failed')
            print('got', result[1])
            print('instead of', expected)
        test_i += 1


run_tests()

boost = [int(n) for n in open("9.input").read().split(",")]


def part1():
    return run(boost.copy(), [1])[1]


def part2():
    return run(boost.copy(), [2])[1]


print(part1())
print(part2())
