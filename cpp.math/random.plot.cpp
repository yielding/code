#include <boost/random.hpp>
#include <vector>
#include <cmath>
#include <string>
#include <iostream> 
#include <iomanip>

using namespace std;

void plot_histogram(vector<int> const& slots, int samples, double from, double to)
{
  auto m = *max_element(slots.begin(), slots.end());
  auto nRows = 20;

  cout.setf(ios::fixed | ios::left);
  cout.precision(5);

  for (int r=0; r<nRows; r++) 
  {
    double y = ((nRows - r) * double(m))/(nRows * samples);
    cout << setw(10) << y << "  ";
    for (unsigned int col=0; col<slots.size(); col++) 
    {
      char out = ' ';
      if (slots[col]/double(samples) >= y)
        out = 'x';

      cout << out;
    }

    cout << endl;
  }

  cout << setw(12) << " " << setw(10) << from;
  cout.setf(ios::right, ios::adjustfield);
  cout << setw(slots.size()-10) << to << endl;
}

// I am not sure whether these two should be in the library as well
// maintain sum of NumberGenerator results
//
template<class NumberGenerator, 
         class Sum = typename NumberGenerator::result_type>
class sum_result 
{
public:
  typedef NumberGenerator base_type;
  typedef typename base_type::result_type result_type;
  explicit sum_result(const base_type & g) : gen(g), _sum(0) { }

  result_type operator()() { result_type r = gen(); _sum += r; return r; }
  base_type & base()       { return gen;  }
  Sum sum() const          { return _sum; }
  void reset()             { _sum = 0;    }

private:
  base_type gen;
  Sum _sum;
};

// maintain square sum of NumberGenerator results
template <class NumberGenerator, 
          class Sum = typename NumberGenerator::result_type>
class squaresum_result
{
public:
  typedef NumberGenerator base_type;
  typedef typename base_type::result_type result_type;
  explicit squaresum_result(const base_type & g) : gen(g), _sum(0) { }
  result_type operator()() { result_type r = gen(); _sum += r*r; return r; }
  base_type & base() { return gen; }
  Sum squaresum() const { return _sum; }
  void reset() { _sum = 0; }

private:
  base_type gen;
  Sum _sum;
};

template<class RNG>
void histogram(RNG base, int samples, double from, double to, const string & name)
{
  typedef squaresum_result< sum_result<RNG, double>, double > SRNG;
  SRNG gen((sum_result<RNG, double>(base)));
  const int nSlots = 60;
  vector<int> slots(nSlots, 0);
  for (int i=0; i<samples; i++) 
  {
    double val = gen();
    // early check avoids overflow
    if (val < from || val >= to) continue;
    int slot = int((val-from)/(to-from) * nSlots);
    if (slot < 0 || slot > (int)slots.size()) continue;
    slots[slot]++;
  }

  cout << name << endl;
  plot_histogram(slots, samples, from, to);
  double mean = gen.base().sum() / samples;
  cout << "mean: " << mean
       << " sigma: "  << sqrt(gen.squaresum()/samples-mean*mean)
       << "\n"        << endl;
}


template<class PRNG, class Dist>
inline boost::variate_generator<PRNG&, Dist> 
make_gen(PRNG & rng, Dist d)
{
  return boost::variate_generator<PRNG&, Dist>(rng, d);
}

template<class PRNG>
void histograms()
{
  PRNG rng;
  using namespace boost;

  histogram(make_gen(rng, uniform_smallint<>(0, 5)),          100000,   -1,   6, "uniform_smallint(0,5)");
  histogram(make_gen(rng, uniform_int<>(0, 5)),               100000,   -1,   6, "uniform_int(0,5)"     );
  histogram(make_gen(rng, uniform_real<>(0,1)),               100000, -0.5, 1.5, "uniform_real(0,1)"    );
  histogram(make_gen(rng, bernoulli_distribution<>(0.2)),     100000, -0.5, 1.5, "bernoulli(0.2)"       );
  histogram(make_gen(rng, binomial_distribution<>(4, 0.2)),   100000,   -1,   5, "binomial(4, 0.2)"     );
  histogram(make_gen(rng, triangle_distribution<>(1, 2, 8)),  100000,    0,  10, "triangle(1,2,8)"      );
  histogram(make_gen(rng, geometric_distribution<>(5.0/6.0)), 100000,    0,  10, "geometric(5/6)"       );
  histogram(make_gen(rng, exponential_distribution<>(0.3)),   100000,    0,  10, "exponential(0.3)"     );
  histogram(make_gen(rng, cauchy_distribution<>()),           100000,   -5,   5, "cauchy"               );
  histogram(make_gen(rng, lognormal_distribution<>(3, 2)),    100000,    0,  10, "lognormal"            );
  histogram(make_gen(rng, normal_distribution<>()),           100000,   -3,   3, "normal"               );
  histogram(make_gen(rng, normal_distribution<>(0.5, 0.5)),   100000,   -3,   3, "normal(0.5, 0.5)"     );
  histogram(make_gen(rng, poisson_distribution<>(1.5)),       100000,    0,   5, "poisson(1.5)"         );
  histogram(make_gen(rng, poisson_distribution<>(10)),        100000,    0,  20, "poisson(10)"          );
  histogram(make_gen(rng, gamma_distribution<>(0.5)),         100000,    0, 0.5, "gamma(0.5)"           );
  histogram(make_gen(rng, gamma_distribution<>(1)),           100000,    0,   3, "gamma(1)"             );
  histogram(make_gen(rng, gamma_distribution<>(2)),           100000,    0,   6, "gamma(2)"             );
}

int main(int argc, char* argv[])
{
  histograms<boost::mt19937>();
  return 0;
}
