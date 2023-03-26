#include <limits>
#include <complex>
#include <chrono>
#include <iostream>
#include <boost/range/irange.hpp>
#include <tbb/tbb.h>
#include <tiffio.h>

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"

template <typename T, typename U>
inline complex<T> operator*(const complex<T>& lhs, const U& rhs)
{
  return lhs * T(rhs);
}

template <typename T, typename U>
inline complex<T> operator*(const U& lhs, const complex<T>& rhs)
{
  return T(lhs) * rhs;
}

template<typename Iterate, typename IterationMap, typename T>
int boundedorbit(Iterate f, complex<T> seed, int bound, int bailout=100, 
                 IterationMap itmap = [](int n, complex<T> z, int bailout) { return n; })
{
  auto z = f(seed);
  for (auto k : boost::irange(1, bailout)) {
    if (abs(z) > bound) 
      return itmap(k, z, bailout);
    z = f(z);
  }

  return numeric_limits<int>::min();
}

template<typename T>
float normalized_iterations(int n, complex<T> zn, int bailout)
{
  return n + (log(log(bailout))-log(log(abs(zn))))/log(2);
}

int main(int, char**)
{
  // allow complex literal
  using namespace literals;
  
  // initialize TBB
  tbb::task_scheduler_init init;

  // allocate storage
  // TODO don't leak memory
  auto iteration_counts = new unsigned char[3500*2500];

  auto start = chrono::high_resolution_clock::now();

  const int GRAIN = 100;
  
  tbb::parallel_for(tbb::blocked_range2d<int>(0, 2500, GRAIN, 0, 3500, GRAIN), [&](auto r) {
      for (auto j = r.rows().begin(); j != r.rows().end(); j++) {
          for (auto k = r.cols().begin(); k != r.cols().end(); k++) {
              auto c = (-2.5 + k*3.5/3500.0) + (-1.25i + j*2.5i/2500.0);
              iteration_counts[3500*j + k] = boundedorbit([&c](auto z) { return z*z + c; }, 0.0i, 2, 200, &normalized_iterations<double>);
          }
      }
  });

  auto end = chrono::high_resolution_clock::now();
  chrono::duration<double> elapsed_seconds = end - start;
  
  cout << "computation took " << elapsed_seconds.count() << "s" << endl;

  stbi_write_png("mandelbrot_tbb.png", 3500, 2500, 1, iteration_counts, 3500);

  return 0;
}
