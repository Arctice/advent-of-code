from dataclasses import dataclass, field


@dataclass
class intcode:
    tape: list
    pos: int = 0
    input: list = field(default_factory=list)
    output: list = field(default_factory=list)
    base: int = 0


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
        if op == 3 and not state.input:
            return state
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
