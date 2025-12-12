#include <iostream>
#include <fstream>
#include <array>
#include <vector>
#include <ranges>
#include <sstream>
#include <iomanip>
#include <bitset>
#include <thread>
#include <atomic>


struct constraint {
    int width;
    int height;
    std::array<int, 6> counts;
};

constexpr auto N = 2500;
using grid = std::bitset<N>;

grid shape_bitset(int width, std::string shape) {
    grid bits;
    for (int y{0}; y < 3; ++y) {
        int offset = y * width;
        for (int x{0}; x < 3; ++x) bits[x + offset] = (shape[y * 3 + x] == '#');
    }
    return bits;
}

bool search(const constraint& puzzle, const std::vector<grid>& shapes,
            grid& state, std::array<int, 6>& counts, int leftmost) {
    // each shape that still needs placing
    bool shapes_left = false;
    for (int i{0}; i < 6; ++i) {
        int remaining = puzzle.counts[i] - counts[i];
        if (not remaining)
            continue;
        shapes_left = true;
        // find first valid position
        auto shape = shapes[i];
        int first_valid = -1;
        for (int y{leftmost / puzzle.width}; y < puzzle.height - 2; ++y) {
            for (int x{(y * puzzle.width > leftmost)
                           ? 0
                           : (leftmost % puzzle.width)};
                 x < puzzle.width - 2; ++x) {
                int pos = y * puzzle.width + x;
                auto shifted = shape << pos;
                if ((state & shifted).any())
                    continue;
                first_valid = pos;
                break;
            }

            if (first_valid != -1)
                break;
        }
        if (first_valid == -1)
            continue;

        // place, adjust counts
        counts[i]++;
        state |= (shape << first_valid);
        auto found = search(puzzle, shapes, state, counts, first_valid);
        state ^= (shape << first_valid);
        counts[i]--;
        if (found)
            return true;
    }
    // if counts null, stop
    return not shapes_left;
}

bool solve(constraint puzzle, std::vector<std::string> shape_defs) {
    std::vector<grid> shapes;
    for (auto& shape : shape_defs)
        shapes.push_back(shape_bitset(puzzle.width, shape));

    grid initial_grid;
    std::array<int, 6> counts{};
    return search(puzzle, shapes, initial_grid, counts, 0);
}

std::vector<std::string> parse_shapes(std::ifstream& in) {
    std::vector<std::string> shapes;

    std::string next;
    for (int i{}; i < 6; ++i) {
        std::string shape{};
        int characters = 0;
        in >> next;
        for (int i{}; i < 3; ++i) {
            in >> next;
            shape += next;
        }
        shapes.push_back(shape);
    }

    return shapes;
}

std::vector<constraint> parse_constraints(std::ifstream& in) {
    std::vector<constraint> out;
    std::stringstream buffer;
    buffer << in.rdbuf();
    std::string input{buffer.str()};

    for (const auto line : input | std::views::split('\n')) {
        auto l = std::string{std::string_view(line)};
        auto colon = l.find(":");
        if (colon == std::string::npos)
            continue;

        constraint next;
        std::string sizes = l.substr(0, colon);
        std::string shapecounts = l.substr(colon + 2, l.size());
        next.width = std::stoi(sizes.substr(0, sizes.find("x")));
        next.height = std::stoi(sizes.substr(sizes.find("x")+1, sizes.size()));
        int i = 0;
        for (const auto shape : shapecounts | std::views::split(' ')) {
            std::string ss = std::string(std::string_view(shape));
            next.counts[i++] = std::stoi(ss);
        }
        out.push_back(next);
    }

    return out;
}

std::atomic<int> good;

int main() {
    std::ifstream in{"12.input"};
    auto shapes = parse_shapes(in);
    auto constraints = parse_constraints(in);
    for (auto& c : constraints) {
        auto task = std::thread([&] {
            good += solve(c, shapes);
            std::cout << good << std::endl;
        });
        task.detach();
    }
    std::this_thread::sleep_for(std::chrono::seconds(10000));
}


