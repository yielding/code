#include <sys/time.h>
#include <stdio.h>
#define BAILOUT 16
#define MAX_ITERATIONS 1000

int mandelbrot(float x, float y)
{
    float cr = y - 0.5;
    float ci = x;
    float zi = 0.0;
    float zr = 0.0;
    int i = 0;

    while (1) 
    {
        i++;
        float temp = zr * zi;
        float zr2 = zr * zr;
        float zi2 = zi * zi;
        zr = zr2 - zi2 + cr;
        zi = temp + temp + ci;
        if (zi2 + zr2 > BAILOUT)
            return i;
        if (i > MAX_ITERATIONS)
            return 0;
    }
}

int main (int argc, const char * argv[]) 
{
    struct timeval aTv;
    gettimeofday(&aTv, NULL);
    long init_time = aTv.tv_sec;
    long init_usec = aTv.tv_usec;

    for (int y=-39; y<39; y++)
    {
        printf("\n");
        for (int x = -39; x < 39; x++)
        {
            int i = mandelbrot(x/40.0, y/40.0);
            char const* ch = (i == 0) ? "*" : " ";
            printf(ch);
        } 
    }

    printf("\n");

    gettimeofday(&aTv,NULL);
    double query_time = (aTv.tv_sec - init_time) + (double)(aTv.tv_usec - init_usec)/1000000.0; 
    printf ("C++ Elapsed: %0.2f\n", query_time);
    return 0;
}
