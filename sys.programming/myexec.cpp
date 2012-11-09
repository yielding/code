#include <iostream>

#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>

using namespace std;

/*
int main(int argc, const char *argv[])
{
    cout << "org pid: " << getpid() << endl;
    sleep(1);

    //execl("/opt/local/bin/gls -la", "gls", NULL);
    system("/opt/local/bin/gls -la");
    cout << "I'm back\n";
    cout << "org pid: " << getpid() << endl;
    exit(0);
    return 0;
}
*/

int main(int argc, const char *argv[])
{
    int pid = fork();

    if (pid == 0)
    {
        execl("/bin/ls", "ls", "-l", nullptr);
        exit(0);
    }
    else
    {
        sleep(1);
        for (int i=0; i<10; i++)
            cout << "I'm parent." << getpid() << endl;

        int status;
        waitpid(-1, &status, WNOHANG);
    }

    return 0;
}

/*
int main(int argc, const char *argv[])
{
    execl("/bin/ls", "ls", "-l", nullptr);

    // will not be executed.
    for (int i=0; i<10; i++)
        cout << "leech" << endl;

    return 0;
}
*/
