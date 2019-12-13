# --- Day 13: Care Package ---
import intcode
import time

tape = list(map(int, open('13.input', 'r').read().strip().split(',')))


def draw_screen(grid):
    ys = [p[1] for p in grid.keys()]
    xs = [p[0] for p in grid.keys()]
    start = (min(xs), min(ys))
    end = (max(xs), max(ys))
    tiles = {0: ' ', 1: '#', 2: '.', 3: '~', 4: 'o'}
    for y in range(start[1], end[1] + 1):
        for x in range(start[0], end[0] + 1):
            c = grid.get((x, y), 0)
            print(tiles[c], end='')
        print()


def part1(game):
    state = intcode.intcode(game.copy())
    grid = {}
    while True:
        outputs = []
        state = intcode.advance(state)
        if state.pos is None:
            break
        outputs.append(state.output.pop())
        state = intcode.advance(state)
        outputs.append(state.output.pop())
        state = intcode.advance(state)
        outputs.append(state.output.pop())
        x, y, tile = outputs
        grid[(x, y)] = tile

    return len([v for v in grid.values() if v == 2])


class game():
    def __init__(self, tape):
        self.state = intcode.intcode(tape.copy())
        self.state.tape[0] = 2
        self.screen = {}
        self.step()
        self.score = 0

    def draw(self):
        print(self.score)
        draw_screen(self.screen)

    def step(self):
        while True:
            outputs = []
            for _ in range(3):
                self.state = intcode.advance(self.state)
                if self.state.pos is None:
                    return False
                if not self.state.output:
                    return True
                outputs.append(self.state.output.pop())
            x, y, value = outputs
            if x == -1 and y == 0:
                self.score = value
                continue
            self.screen[(x, y)] = value
            if value == 3:
                self.paddle = x
            if value == 4:
                self.ball = x

    def input(self, input):
        # If the joystick is in the neutral position, provide 0.
        # If the joystick is tilted to the left, provide -1.
        # If the joystick is tilted to the right, provide 1.
        self.state.input.append(input)


def win():
    arcade = game(tape)
    over = False
    while not over:
        paddle = arcade.paddle
        ball = arcade.ball
        input = 0
        if paddle > ball:
            input = -1
        if paddle < ball:
            input = 1
        arcade.input(input)
        over = not arcade.step()
        arcade.draw()
        time.sleep(0.01)
    return arcade.score


print(win())
print(part1(tape))
