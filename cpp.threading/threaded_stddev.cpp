#include <vector>
#include <numeric>
#include <chrono>
#include <thread>
#include <iostream>
#include <cmath>
#include <cstdlib>
#include <iterator>

using namespace std::chrono;
using namespace std;

constexpr int NUM_THREADS = 8;

class StdDev
{
public:
  StdDev(vector<double>& v)
  {
    m_v.swap(v);
  }

  void calc(const vector<double>& v, int start, int end, double &sum, double& squared_sum)
  {
    double th_sum = 0;
    double th_squared_sum = 0;

    for (int i= start; i<end; i++)
    {
      auto it = v[i];
      th_sum += it;
      th_squared_sum += it*it;
    }

    sum += th_sum;
    squared_sum += th_squared_sum;
  }

  double std_dev()
  {
    auto m1_1 = steady_clock::now();

    // If you use a pointer, you can improve performance.
    auto& v = m_v;

    auto m1_2 = duration_cast<milliseconds>(steady_clock::now() - m1_1);
    cout << "[C++] std::vector loading: " << m1_2.count() << " ms" << endl;

    vector<thread> ths;

    double sum = 0;
    double squaredSum = 0;

    cout << "[C++] NUM_THREADS: " << NUM_THREADS << endl;

    auto m2_1 = steady_clock::now();

    // Start threads.
    for (int i = 0; i < NUM_THREADS; ++i) {
      int start = i * round(v.size() / NUM_THREADS);
      int end   = (i == NUM_THREADS - 1) 
        ? v.size() 
        : (i + 1) * round(v.size() / NUM_THREADS);

      ths.push_back(thread(&StdDev::calc, this, ref(v), start, end, ref(sum), ref(squaredSum)));
    }

    // Waiting for threads to finish.
    for (auto &t : ths) t.join();

    auto m2_2 = chrono::duration_cast<milliseconds>(steady_clock::now() - m2_1);
    cout << "[C++] thread execution elapsed: " << m2_2.count() << " ms" << endl;

    double mean = sum / v.size();
    return sqrt(squaredSum / v.size() - mean * mean);
  }

private:
    vector<double> m_v;
};

int main(int argc, char *argv[])
{
  const int item_size = 200000000;
  vector<double> data(item_size);
  data.push_back(rand() % item_size);

  StdDev stddev(data);
  stddev.std_dev();

  return 0;
}
