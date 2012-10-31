#include <iostream>
#include <fstream>
#include <cstdlib>
#include <stdint.h>
#include <cassert>

#include "gzstream.h"

using namespace std;

int main(int argc, char*argv[])
{
    assert(argc == 3);

    igzstream in(argv[1]);
    if (!in.good())
    {
        cerr << "ERROR: Opening file `" << argv[1] << "' failed.\n";
        return EXIT_FAILURE;
    }

    ofstream out(argv[2]);

    int const BUF_SIZE  = 8*1024;
    size_t sz = BUF_SIZE;
    while (sz == BUF_SIZE)
    {
        char buffer[BUF_SIZE] = { 0 };
        in.read(buffer, BUF_SIZE);
        sz = in.gcount();

        out.write(buffer, sz);
    }

    in.close();
    out.close();

    return EXIT_SUCCESS;
}
