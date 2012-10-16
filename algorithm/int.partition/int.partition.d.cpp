#include <stdio.h>

int table[100][100] = { 0 };

void pp()
{
  for (int i=0; i<20; i++)
  {
    for (int j=0; j<20; j++) printf("%2d", table[i][j]);
    printf("\n");
  }
  printf("\n");
}

int partition(int sum, int largestNumber)
{
  if (largestNumber == 0)
    return 0;

  if (sum == 0)
    return 1;

  if (sum < 0)
    return 0;

  printf("%d, %d, %d\n", sum, largestNumber, table[sum][largestNumber]);

  if (table[sum][largestNumber] != 0)
  {
    printf("xx %d, %d\n", sum, largestNumber);

    return table[sum][largestNumber];
  }

  table[sum][largestNumber] 
    = partition(sum, largestNumber - 1)
    + partition(sum - largestNumber, largestNumber);

  pp();

  return table[sum][largestNumber];
}

int main()
{
  int sum = 5;
  int largestNumber = 5;

  printf("%d\n", partition(sum, largestNumber));

  return 0;
}
