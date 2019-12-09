from dataclasses import dataclass


@dataclass
class intcode:
    tape: list
    pos: int
    input: list
    output: list


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


def tape_read(tape, source, mode):
    source = tape[source]
    if mode == 0:
        source = tape[source]
    return source


def op_arithmetic(op):
    def interpret(state, modes):
        A = tape_read(state.tape, state.pos + 1, modes[0])
        B = tape_read(state.tape, state.pos + 2, modes[1])
        dest = state.tape[state.pos + 3]
        val = op(A, B)
        state.tape[dest] = val
        state.pos += 4
        return state

    return interpret


def op_read(state, modes):
    value = state.input[0]
    state.input.pop(0)
    dest = state.tape[state.pos + 1]
    state.tape[dest] = value
    state.pos += 2
    return state


def op_write(state, modes):
    value = tape_read(state.tape, state.pos + 1, modes[0])
    state.output.append(value)
    state.pos += 2
    return state


def op_jnz(state, modes):
    pred = tape_read(state.tape, state.pos + 1, modes[0])
    if pred != 0:
        dest = tape_read(state.tape, state.pos + 2, modes[1])
        state.pos = dest
    else:
        state.pos += 3
    return state


def op_jz(state, modes):
    pred = tape_read(state.tape, state.pos + 1, modes[0])
    if pred == 0:
        dest = tape_read(state.tape, state.pos + 2, modes[1])
        state.pos = dest
    else:
        state.pos += 3
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
    99: op_halt,
}


def run(tape, input):
    state = intcode(tape.copy(), 0, input, [])

    while state.pos is not None:
        op, modes = decode_op(state.tape[state.pos])
        state = instructions[op](state, modes)
    return state


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

tape = [int(n) for n in open("5.input").read().split(",")]


def part1(tape):
    tape = tape.copy()
    return run(tape, [1]).output


def part2(tape):
    tape = tape.copy()
    return run(tape, [5]).output
