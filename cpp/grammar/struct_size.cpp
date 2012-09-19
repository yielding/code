#include <iostream>
#include <stdint.h>

using namespace std;

struct st
{
    uint32_t a;
    uint8_t  b;
    uint32_t c;
};

int main(int argc, const char *argv[])
{
    st s;
    auto s1 = reinterpret_cast<uint32_t>(&s);
    auto s2 = reinterpret_cast<uint32_t>(&s.c);

    cout << s1 << endl;
    cout << s2 << endl;

    cout << s2 - s1 << endl;
        
    return 0;
}
