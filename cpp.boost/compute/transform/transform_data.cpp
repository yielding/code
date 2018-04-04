#include <iostream>
#include <string>
#include <vector>
#include <cstdlib>
#include <chrono>

#include <boost/compute/event.hpp>
#include <boost/compute/system.hpp>
#include <boost/compute/algorithm/copy.hpp>
#include <boost/compute/algorithm/transform.hpp>
#include <boost/compute/async/future.hpp>
#include <boost/compute/container/vector.hpp>

using namespace std;
      namespace compute = boost::compute;

int main(int argc, char *argv[])
{
  // auto device = compute::system::default_device();
  for (auto device: compute::system::devices())
  {
    cout << "=== device : " << device.name() << " ===" << endl;

    compute::context context(device);
    compute::command_queue queue(context, device, compute::command_queue::enable_profiling);

    vector<float> host_vector(160000000);
    generate(host_vector.begin(), host_vector.end(), rand);

    compute::vector<float> device_vector(host_vector.size(), context);

    // compute::future<void> future = compute::copy_async(
    auto future = compute::copy_async(
        host_vector.begin(), host_vector.end(), device_vector.begin(), 
        queue);

    future.wait();

    auto duration = 
      future.get_event().duration<chrono::milliseconds>();

    cout << "time: " << duration.count() << "ms" << endl;
  }

  return 0;
}
