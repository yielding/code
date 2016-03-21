#include <iostream>
#include <string>
#include <vector>

using namespace std;

auto fromto = [](auto start, auto finish) 
{
    return [=]() mutable {
        if (start < finish)
            return start++;
        else
            throw::runtime_error("complete");
    };
};

int main(int argc, char *argv[])
{
    auto range = fromto(0, 10);
    cout << range() << endl;
    cout << range() << endl;
    cout << range() << endl;
    
    return 0;
}

