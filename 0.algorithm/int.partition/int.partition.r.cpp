#include <cstdio>

int partition(int sum, int largestNumber)
{
  if (largestNumber == 0 || sum < 0)
    return 0;

  if (sum == 0)
    return 1;

  return partition(sum, largestNumber - 1) + 
    partition(sum - largestNumber, largestNumber);
}

int main()
{
  int sum = 5;
  int largestNumber = 5;

  printf("%d\n",partition(sum, largestNumber));

  return 0;
}
