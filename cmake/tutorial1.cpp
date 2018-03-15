#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "tutorial_config.h"

#ifdef USE_MYMATH
#include "my_math.h"
#endif

int main(int argc, char *argv[])
{
  if (argc < 2)
  {
    fprintf(stdout,"%s Version %d.%d\n",
        argv[0],
        TUTORIAL_VERSION_MAJOR,
        TUTORIAL_VERSION_MINOR);
    fprintf(stdout,"Usage: %s number\n",argv[0]);
    return 1;
  }

  auto inputValue  = atof(argv[1]);

#ifdef USE_MYMATH
  double outputValue = mysqrt(inputValue);
#else
  double outputValue = sqrt(inputValue);
#endif

  fprintf(stdout,"The square root of %g is %g\n", inputValue, outputValue);

  return 0;
}
