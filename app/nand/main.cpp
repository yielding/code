#include "stdafx.h"
#include "PTreeParser.h"

#include <string>
#include <iostream>
#include <cstdlib>

using namespace std;
using namespace utility::parser;

int main(int argc, const char *argv[])
{
    string path = "/Volumes/Data.Disk/iphone.nand/4.nand/d0686b9ba2.plist";
    PTreeParser ptree;

    if (!ptree.init_with_path(path))
        exit(EXIT_FAILURE);

    cout << "ok\n";

    return 0;
}
