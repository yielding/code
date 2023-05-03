#include <stdio.h>
#include <unistd.h>
#include <sys/sdt.h>
#include "probes.h"

int main(int argc, char const* argv[])
{
    int i;

    if (WORLD_LOOP_ENABLED())
    {
        for (i=0; i<10; i++) {
            WORLD_LOOP(i);
            printf("Hello World!\n");
            sleep(2);
        }
    }
    
    return 0;
}
