#include <cppcoro/generator.hpp>
#include <iostream>

cppcoro::generator<int> counter(int n) {
    for (int i = 0; i < n; i++) {
        co_yield i;
    }
}

int main() {
    for (int v : counter(5)) {
        std::cout << v << " ";
    }
    std::cout << "\n";
}
