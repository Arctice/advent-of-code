import itertools
import collections

d = open('8.input').read().strip()


def split_layers(data):
    w = 25
    h = 6
    layer_size = w * h
    for slice in range(0, len(data), layer_size):
        yield data[slice:slice + layer_size]


def part1():
    Ls = list(split_layers(d))
    zeroes = []
    for L in Ls:
        zeroes.append(len([n for n in L if n == '0']))
    safe, _ = min(enumerate(zeroes), key=lambda n: n[1])
    safe = Ls[safe]
    ones = [n for n in safe if n == '1']
    twos = [n for n in safe if n == '2']
    return len(ones) * len(twos)


part1()


def draw(layer):
    for y in range(6):
        for x in range(25):
            px = ' '
            if layer[y*25+x] == 1:
                px = '#'
            if layer[y*25+x] == 2:
                px = ' '
            print(px, end='')
        print()


def part2():
    layers = list(split_layers(d))
    final_image = []
    for y in range(6):
        for x in range(25):
            pixel = None
            for layer in layers:
                layer_pixel = layer[y * 25 + x]
                if layer_pixel == '2':
                    continue
                pixel = int(layer_pixel)
                break
            final_image.append(pixel)
    draw(final_image)


part2()
