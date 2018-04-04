#include <boost/compute/function.hpp>
#include <boost/compute/system.hpp>
#include <boost/compute/algorithm/count_if.hpp>
#include <boost/compute/container/vector.hpp>
#include <boost/compute/iterator/buffer_iterator.hpp>
#include <boost/compute/random/default_random_engine.hpp>
#include <boost/compute/types/fundamental.hpp>

#include <chrono>
#include <iostream>

using namespace std;
namespace compute = boost::compute;

int main() 
{ 
  auto gpus = compute::system::devices();
  for (auto const gpu: gpus) 
  {
    cout << gpu.name() << endl;

    compute::context context(gpu);
    compute::command_queue queue(context, gpu);

    using compute::uint_; using compute::uint2_; // ten million random points
    size_t n = 10000000;

    auto s = chrono::high_resolution_clock::now();

    // generate random numbers
    compute::default_random_engine rng(queue);
    compute::vector<uint_> v(n * 2, context);
    rng.generate(v.begin(), v.end(), queue);

    // function returing true if the point is within the unit circle
    BOOST_COMPUTE_FUNCTION(bool, is_in_unit_circle, (const uint2_ point), {
      const float x = point.x / (float) UINT_MAX - 1;
      const float y = point.y / (float) UINT_MAX - 1;

      return (x*x + y*y) < 1.0f;
    });

    // iterate over vector<uint> as vector<uint2>
    auto start = compute::make_buffer_iterator<uint2_>(v.get_buffer(), 0);
    auto end   = compute::make_buffer_iterator<uint2_>(v.get_buffer(), v.size() / 2);

    // count number of random points within the unit circle
    size_t count = compute::count_if(start, end, is_in_unit_circle, queue);

    auto e = chrono::high_resolution_clock::now();

    auto duration = chrono::duration_cast<chrono::milliseconds>(e - s);

    // print out values
    float count_f = static_cast<float>(count);
    cout << "count: " << count << " / " << n << endl;
    cout << "ratio: " << count_f / float(n) << endl;
    cout << "pi = "   << (count_f / float(n)) * 4.0f << endl;
    cout << "time : " << duration.count() << endl;
    cout << endl;
  }

  return 0;
}
