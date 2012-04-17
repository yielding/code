#include "SSHSession.h"

#include <iostream>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char** argv)
{
    utility::comm::ssh::SSHSession ssh("127.0.0.1", 22);

    if (!ssh.connect("yielding", "alsrudk!"))
    {
        cout << "ssh.connect error\n";
        return EXIT_FAILURE;
    }

    cout << "ssh.connect ok\n";

    return EXIT_SUCCESS;
}
