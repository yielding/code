#include <stdio.h>

int table[100][100] = { 0 };

int partition(int sum, int largestNumber)
{
  if (largestNumber == 0)
    return 0;

  if (sum == 0)
    return 1;

  if (sum < 0)
    return 0;

  if (table[sum][largestNumber]!=0)
    return table[sum][largestNumber];

  table[sum][largestNumber] 
    = partition(sum, largestNumber - 1)
    + partition(sum - largestNumber, largestNumber);

  return table[sum][largestNumber];
}

int main()
{
  int sum = 5;
  int largestNumber = 5;

  printf("%d\n", partition(sum, largestNumber));

  return 0;
}
