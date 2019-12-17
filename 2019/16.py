# --- Day 16: Flawed Frequency Transmission ---

data = open('16.input', 'r').read().strip()
signal = list(map(int, data))

base = [0, 1, 0, -1]


def pattern_read(position, offset):
    offset = (offset + 1) // (position + 1)
    return offset


def fft_step(signal):
    transformed = []
    position = 0
    for pos in range(len(signal)):
        sigma = 0
        for offset in range(len(signal)):
            coefficient = pattern_read(pos, offset) % 4
            coefficient = base[coefficient]
            sigma += coefficient * signal[offset]
        transformed.append(abs(sigma) % 10)
    return transformed

def fft(signal, phases):
    for _ in range(phases):
        signal = fft_step(signal)
    return signal


# part1
print(''.join(map(str, fft(signal, 100)[:8])))
