#include "rxcpp/rx.hpp"

int main(int argc, char *argv[])
{
    auto values = rxcpp::observable<>::from(1, 2, 2, 3, 3, 3, 4, 5, 5, 6)
                        .distinct()
                        //.distinct_until_changed();
                        .filter([](int v) { return v % 2; })
                        .max();

    values.subscribe([](int v) { printf("%d ", v); }, []{});
    
    return 0;
}
