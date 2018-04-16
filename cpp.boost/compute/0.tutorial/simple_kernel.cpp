#include <iostream>

#include <boost/compute/core.hpp>
#include <boost/compute/utility/source.hpp>

using namespace std;
      namespace compute = boost::compute;

int main(int argc, char *argv[])
{
  auto device = compute::system::default_device();

  compute::context context(device);

  float a[] = { 1, 2, 3, 4 };
  float b[] = { 5, 6, 7, 8 };

  float c[] = { 0, 0, 0, 0 };

  compute::buffer ba(context, 4 * sizeof(float));
  compute::buffer bb(context, 4 * sizeof(float));
  compute::buffer bc(context, 4 * sizeof(float));

  const char source[] = BOOST_COMPUTE_STRINGIZE_SOURCE(
      __kernel void add(__global const float* a,
                        __global const float* b,
                        __global float* c) 
      {
        const uint i = get_global_id(0);
        c[i]         = a[i] + b[i];
      }
  );

  auto program = compute::program::create_with_source(source, context);
  program.build();

  compute::kernel kernel(program, "add");
  kernel.set_arg(0, ba);
  kernel.set_arg(1, bb);
  kernel.set_arg(2, bc);

  compute::command_queue queue(context, device);
  queue.enqueue_write_buffer(ba, 0, 4 * sizeof(float), a);
  queue.enqueue_write_buffer(bb, 0, 4 * sizeof(float), b);

  queue.enqueue_1d_range_kernel(kernel, 0, 4, 0);
  queue.enqueue_read_buffer(bc, 0, 4 * sizeof(float), c);

  cout << "c: [ " << c[0] << ", "
                  << c[1] << ", "
                  << c[2] << ", "
                  << c[3] << "] " << endl;

  return 0;
}
