
#include <iostream>
#include <fstream>
#include <chrono>

#define __CL_ENABLE_EXCEPTIONS
#include "cl.hpp"

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"

int main(int argc, char** argv)
{
    try {
        std::vector<cl::Platform> platforms;
        cl::Platform::get(&platforms);

        std::vector<cl::Device> platformDevices;
        platforms[0].getDevices(CL_DEVICE_TYPE_GPU, &platformDevices);

        cl::Context context(platformDevices);
        auto contextDevices = context.getInfo<CL_CONTEXT_DEVICES>();
        for (auto dev : contextDevices) {
            std::cout << "Using " << dev.getInfo<CL_DEVICE_NAME>() << std::endl;
        }

        std::ifstream programFile("mandelbrot.cl");
        std::string programString(std::istreambuf_iterator<char>(programFile), (std::istreambuf_iterator<char>()));

        cl::Program::Sources source(1, std::make_pair(programString.c_str(), programString.length()+1));
        cl::Program program(context, source);
        try {
            program.build(contextDevices);
        } catch (cl::Error e) {
            // FIXME may not be the device that failed
            std::cout << program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(contextDevices[0]) << std::endl;
        }
        
        cl::Kernel mandelbrot(program, "mandelbrot");

        // command queues
        std::vector<cl::CommandQueue> queues;
        for (auto device : contextDevices) {
            cl::CommandQueue queue(context, device, CL_QUEUE_PROFILING_ENABLE);
            queues.push_back(queue);
        }

unsigned char* iteration_counts = new unsigned char[3500*2500];

        auto start = std::chrono::high_resolution_clock::now();

        // partition the "y" dimension
        int i = 0;
        int workItemsPerQueue = 2500/queues.size(); // FIXME requires work size to be evenly divisible by number of queues
        std::vector<cl::Buffer> outputs;
        
        for (auto queue : queues) {
            cl::NDRange offset(0, 0); //i*workItemsPerQueue);
            cl::NDRange global_size(3500, workItemsPerQueue);

            // storage for results per device        
            cl_int err = CL_SUCCESS;
            cl::Buffer output(context, CL_MEM_WRITE_ONLY, (size_t)3500*workItemsPerQueue, (void*)NULL, &err);
            mandelbrot.setArg(0, output);
            mandelbrot.setArg(1, i*workItemsPerQueue);
            outputs.push_back(output);
            
            queue.enqueueNDRangeKernel(mandelbrot, offset, global_size);
            queue.enqueueBarrierWithWaitList();

            std::cout << "enqueued range " << i*workItemsPerQueue << " of length " << workItemsPerQueue << std::endl;

            i++;
        }

        // read results
        unsigned char* results = new unsigned char[3500*2500];
        std::vector<cl::Event> readWaitList;

        i = 0;
        for (auto queue : queues) {
            size_t offset = i*3500*workItemsPerQueue;
            cl::Event readDoneEvent;
            queue.enqueueReadBuffer(outputs[i], CL_FALSE, 0, 3500*workItemsPerQueue, &(results[offset]), NULL, &readDoneEvent);

            // NOTE: can't push onto vector until the event is valid, since it will be copied
            readWaitList.push_back(readDoneEvent);

            i++;
        }
            
        cl::Event::waitForEvents(readWaitList);

        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> elapsed_seconds = end - start;
        
        std::cout << "computation took " << elapsed_seconds.count() << "s" << std::endl;

        stbi_write_png("mandelbrot_cl.png", 3500, 2500, 1, results, 3500);

    } catch (cl::Error e) {
        std::cout << e.what() << ": Error code " << e.err() << std::endl;
    }

    return 0;
}
