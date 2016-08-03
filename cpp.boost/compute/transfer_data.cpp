#include <vector>

#include <boost/compute/algorithm/copy.hpp>
#include <boost/compute/container/vector.hpp>

namespace compute = boost::compute;

using namespace std;

int main(int argc, char *argv[])
{
  auto device = compute::system::default_device();
  compute::context context(device);
  compute::command_queue queue(context, device);

  int host_data[] = { 1, 3, 5, 7, 9 };
  compute::vector<int> device_vector(5, context);
  compute::copy(
    host_data, host_data+5, device_vector.begin(), queue);

  vector<int> host_vector(5);

  compute::copy(
    device_vector.begin(), device_vector.end(), host_vector.begin(), queue);

  return 0;
}
