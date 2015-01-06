#include "library.h"
#include <stdio.h>

int main()
{
  for (int i=0; i<10; i++)
    printf("%d ", libFunc());

  MyStack s;
  for (int i=0; i<10; i++)
    printf("%d ", s.top());

  return 0;
}
