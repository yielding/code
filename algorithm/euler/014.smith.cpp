#include <cstdio>

int main(int argc, char **argv)
{
  int longest = 0;
  int terms = 0;
  unsigned long j;

  for (int i=1; i <= 1000000; i++)
  {
    j = i;
    int this_terms = 1;

    while (j != 1)
    {
      this_terms++;

      if (this_terms > terms)
      {
        terms = this_terms;
        longest = i;
      }

      j = (j % 2 == 0) ? j / 2 : 3 * j + 1;
    }
  }

  printf("longest: %d (%d)\n", longest, terms);

  return 0;
}
