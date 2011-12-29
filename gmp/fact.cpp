#include <cstdio>
#include <gmp.h>
#include <time.h>
#include <stdint.h>

void factorial(mpz_t* f, uint32_t n)
{
  for (auto i=1; i<=n; ++i)
    mpz_mul_si(*f, *f, i);
}

int main(int argc, char const* argv[])
{
  mpz_t facN;
  unsigned int n = 200000;
  clock_t start, end;
  double runtime;

  mpz_init(facN);
  mpz_set_ui(facN, 1L);

  start = clock();
  factorial(&facN, n);
  end = clock();

  //printf("fac %u is ", n);
  //mpz_out_str(stdout, 10, facN);
  //printf("\n");

  runtime = (double)(end-start) / CLOCKS_PER_SEC;
  printf ("runtime is %f\n", runtime);
  mpz_clear(facN);

  return 0;
}
