#import <iostream>
#import <vector>
#import <algorithm>

struct candidate {
    int i;
    int min;
    int max;
};

struct empodia {
    int start, end;
};

int main() {
    std::cin.sync_with_stdio(0);
    std::cin.tie(0);

    std::vector<candidate> candidates;
    std::vector<empodia> empodia;

    int n;
    std::cin >> n;

    for (int i = 1; i < n + 1; ++i) {
        int x;
        std::cin >> x;

        candidates.push_back({i, x, x});
        candidates.erase(std::remove_if(candidates.begin(), candidates.end(),
                                        [x](auto& c) { return x < c.min; }),
                         candidates.end());

        for (auto& c : candidates) {
            if (x < c.max)
                continue;
            c.max = x;
            int di = i - c.i;
            int dx = x - c.min;
            if (di == 0)
                continue;
            bool valid = dx == di;
            if (valid) {
                empodia.push_back({c.i, i});
                candidates.erase(
                    std::remove_if(candidates.begin(), candidates.end(),
                                   [&c](auto& cx) { return c.i >= cx.i; }),
                    candidates.end());
                break;
            }
        }
    }

    std::cout << empodia.size() << "\n";
    for (auto& empodia : empodia) {
        std::cout << empodia.start << " " << empodia.end << "\n";
    }
    std::cout << std::endl;
}
