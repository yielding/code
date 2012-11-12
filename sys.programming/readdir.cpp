#include <iostream>
#include <dirent.h>

using namespace std;

int main(int argc, const char *argv[])
{
    DIR* src = opendir("/dev");
    dirent* pent;

    while ((pent = readdir(src)) != NULL)
        cout << pent->d_name << endl;
    
    return 0;
}
