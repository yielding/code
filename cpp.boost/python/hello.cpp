#include <iostream>
#include <Python/Python.h>
#include <boost/python.hpp>

using namespace std;
using namespace boost::python;

int main(int argc, char const* argv[])
{
    Py_Initialize();

    Py_Finalize();
    
    return 0;
}
