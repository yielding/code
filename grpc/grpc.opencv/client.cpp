#include "image_processor.grpc.pb.h"

#include <grpcpp/grpcpp.h>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace image_processor
{
  using namespace std;

  class ImageProcessorClient
  {
  public:
    ImageProcessorClient(shared_ptr<grpc::Channel> channel)
      : _stub(ImageProcessor::NewStub(channel))
    {
    }

  public:
    auto apply_gaussian_blur(const string& input_path,
      const string& output_path,
      const int kernel_size = 9,
      const double sigma = 2.0) -> bool
    {
      // Read input image
      auto image_data = read_file(input_path);
      if (image_data.empty())
      {
        cerr << "Failed to read input file: " << input_path << endl;
        return false;
      }

      cout << "Sending image: " << image_data.size() << " bytes" << endl;

      // Build request
      ImageRequest request;
      request.set_image_data(image_data.data(), image_data.size());
      request.set_kernel_size(kernel_size);
      request.set_sigma(sigma);

      // Call RPC
      ImageResponse response;
      grpc::ClientContext context;

      auto status = _stub->ApplyGaussianBlur(&context, request, &response);

      if (!status.ok())
      {
        cerr << "RPC failed: " << status.error_message() << endl;
        return false;
      }

      if (!response.success())
      {
        cerr << "Processing failed: " << response.error_message() << endl;
        return false;
      }

      // Write output image
      auto const& result_data = response.image_data();
      if (!write_file(output_path, result_data))
      {
        cerr << "Failed to write output file: " << output_path << endl;
        return false;
      }

      cout << "Received image: " << result_data.size() << " bytes" << endl;
      cout << "Saved to: " << output_path << endl;

      return true;
    }

  private:
    auto read_file(const string& path) -> vector<char>
    {
      ifstream file(path, ios::binary | ios::ate);
      if (!file)
        return {};

      auto const size = file.tellg();
      file.seekg(0, ios::beg);

      vector<char> buffer(size);
      file.read(buffer.data(), size);

      return buffer;
    }

    auto write_file(const string& path, const string& data) -> bool
    {
      ofstream file(path, ios::binary);
      if (!file)
        return false;

      file.write(data.data(), data.size());
      return true;
    }

  private:
    unique_ptr<ImageProcessor::Stub> _stub;
  };

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
auto print_usage(const char* program) -> void
{
  using namespace std;

  cerr << "Usage: " << program << " <input_image> <output_image> [server_address]" << endl;
  cerr << "  input_image:    Path to input image file" << endl;
  cerr << "  output_image:   Path to save processed image" << endl;
  cerr << "  server_address: gRPC server address (default: localhost:50051)" << endl;
}

int main(const int argc, char* argv[])
{
  using namespace std;
  using namespace image_processor;

  if (argc < 3)
  {
    print_usage(argv[0]);
    return 1;
  }

  auto const input_path = string(argv[1]);
  auto const output_path = string(argv[2]);
  auto const server_address = (argc > 3) ? string(argv[3]) : "localhost:50051"s;

  cout << "Connecting to " << server_address << "..." << endl;

  auto channel = grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials());
  ImageProcessorClient client(channel);

  if (!client.apply_gaussian_blur(input_path, output_path))
  {
    cerr << "Image processing failed" << endl;
    return 1;
  }

  cout << "Image processing completed successfully" << endl;

  return 0;
}
