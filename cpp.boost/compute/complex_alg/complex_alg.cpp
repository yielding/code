#include <iostream>
#include <vector>
#include <algorithm>
#include <boost/foreach.hpp>
#include <boost/compute/core.hpp>
#include <boost/compute/platform.hpp>
#include <boost/compute/algorithm/transform_reduce.hpp>
#include <boost/compute/algorithm/accumulate.hpp>
#include <boost/compute/container/vector.hpp>
#include <boost/compute/functional/math.hpp>
#include <boost/compute/types/builtin.hpp>
#include <boost/compute/function.hpp>
#include <boost/chrono/include.hpp>

using namespace std;
			namespace compute = boost::compute;

int main()
{
	vector<float> host_vector(160000000);
	generate(host_vector.begin(), host_vector.end(), rand);

	for (auto platform: compute::system::platforms())
	{
		std::cout << "====================" << platform.name() << "====================\n";

		for (auto device: platform.devices())
		{
			cout << "device: " << device.name() << endl;

			compute::context context(device);
			compute::command_queue queue(context, device);
			compute::vector<float> device_vector(host_vector.size(), context);

			compute::copy(
					host_vector.begin(), host_vector.end(), device_vector.begin(), queue
					);

			auto start = chrono::high_resolution_clock::now();
			// compute::transform(device_vector.begin(),
			// 		device_vector.end(),
			// 		device_vector.begin(),
			// 		compute::sqrt<float>(), queue);
			// auto ans = compute::accumulate(device_vector.begin(), device_vector.end(), 0, queue);

			float ans = 0;
			compute::transform_reduce(
					device_vector.begin(),
					device_vector.end(),
					&ans,
					compute::sqrt<float>(),
					compute::plus<float>(),
					queue);

      auto end = chrono::high_resolution_clock::now();
			auto duration = chrono::duration_cast<chrono::milliseconds>(end - start);
			cout << "ans: "  << ans << endl;
			cout << "time: " << duration.count() << " ms" << endl;
			cout << "-------------------\n";
		}
	}

	cout << "====================plain====================\n";
	auto start = chrono::high_resolution_clock::now();
	transform(host_vector.begin(),
			host_vector.end(),
			host_vector.begin(),
			[](float v){ return std::sqrt(v); });

	auto ans = accumulate(host_vector.begin(), host_vector.end(), 0.0);
	auto duration = chrono::duration_cast<chrono::milliseconds>(chrono::high_resolution_clock::now() 
                  - start);
	cout << "ans: " << ans << endl;
	cout << "time: " << duration.count() << " ms" << endl;

	return 0;
}
