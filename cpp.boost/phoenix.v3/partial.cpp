#include <iostream>
#include <fstream>
#include <boost/phoenix.hpp>

namespace phx = boost::phoenix;

using namespace std;
using namespace phx::arg_names;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void consumer(int i, ofstream& f)
{
    char buf[50] = { 0 };
    sprintf(buf, "line %d\n", i);

    f    << buf;
    cout << buf;
}

template <typename F> 
void producer(F waster)
{
    for (int i=0; i<10; i++)
        waster(i);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char const* argv[])
{
    ofstream f;
    f.open("a.txt", ios_base::binary);

    // The point is phx::ref
    producer(phx::bind(&consumer, arg1, phx::ref(f)));

    return 0;
}
