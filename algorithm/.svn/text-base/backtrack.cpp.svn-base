#include <stdio.h>

#define MAX           1000
#define MAXCANDIDATES 1000

bool finished = false;

bool is_solution(int a[], int k, int n)
{
  return k == n;
}

void construct_candidates(int a[], int k, int n, int c[], int *ncandidates)
{
  c[0] = true;
  c[1] = false;
  *ncandidates = 2;
}

void process_solution(int a[], int k)
{
  printf("{ ");
  for (int i=1; i<=k; i++)
    if (a[i] == true)
      printf(" %d", i);
  printf(" }\n");
}

void backtrack(int a[], int k, int input)
{
  int c[MAXCANDIDATES];
  int ncandidates;

  if (is_solution(a, k, input))
    process_solution(a, k);
  else
  {
    k++;
    construct_candidates(a, k, input, c, &ncandidates);
    for (int i=0; i<ncandidates; i++)
    {
      a[k] = c[i];
      backtrack(a, k, input);
      if (finished)
        return;
    }
  }
}

void generate_subsets(int n)
{
  int a[MAX];

  backtrack(a, 0, n);
}

int main()
{
  generate_subsets(3);
}
