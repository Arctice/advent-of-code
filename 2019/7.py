from dataclasses import dataclass
from itertools import permutations


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
    value = state.input.pop(0)
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


def advance(state):
    while state.pos is not None and not state.output:
        op, modes = decode_op(state.tape[state.pos])
        state = instructions[op](state, modes)
    return state


def run(tape, input):
    state = intcode(tape.copy(), 0, input, [])
    while state.pos is not None:
        state = advance(state)
    return state


def amplifiers(tape):
    amps = []
    for n in range(5):
        state = intcode(tape.copy(), 0, [], [])
        amps.append(state)
    return amps


def amp_sequence(tape, phase):
    circuit = amplifiers(tape)
    current = 0
    cycle = len(circuit)

    for i, p in enumerate(phase):
        circuit[i].input.append(p)
    circuit[0].input.append(0)

    while circuit[current].pos is not None:
        circuit[current] = advance(circuit[current])

        signal = circuit[current].output
        next = (current + 1) % cycle
        circuit[next].input += signal
        circuit[current].output = []
        current = next
    remaining = sum([c.input for c in circuit], [])
    return max(remaining)


t1 = [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]
t2 = [
    3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33,
    7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0
]

tape = [int(n) for n in open("7.input").read().split(",")]


def part1():
    results = []
    for phases in permutations([0, 1, 2, 3, 4]):
        amp = amp_sequence(tape, phases)
        results.append((amp, phases))
    return max(results)


t1a = [
    3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001,
    28, -1, 28, 1005, 28, 6, 99, 0, 0, 5
]
t2a = [
    3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55,
    26, 1001, 54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55,
    1, 55, 2, 53, 55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0,
    10
]


def part2():
    results = []
    for phases in permutations([5, 6, 7, 8, 9]):
        amp = amp_sequence(tape, phases)
        results.append((amp, phases))
    return max(results)

print(part1())
print(part2())
