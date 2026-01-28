#include "image_processor.grpc.pb.h"

#include <grpcpp/grpcpp.h>
#include <opencv2/imgproc.hpp>
#include <opencv2/imgcodecs.hpp>
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

  class ImageProcessorServiceImpl final : public ImageProcessor::Service
  {
  public:
    auto ApplyGaussianBlur(
      grpc::ServerContext* context,
      const ImageRequest* request,
      ImageResponse* response) -> grpc::Status override
    {
      try
      {
        // Decode image from request
        auto const& image_data = request->image_data();
        vector<uchar> buffer(image_data.begin(), image_data.end());
        auto mat = cv::imdecode(cv::Mat(buffer), cv::IMREAD_UNCHANGED);

        if (mat.empty())
        {
          response->set_success(false);
          response->set_error_message("Failed to decode image");
          return grpc::Status::OK;
        }

        // Get parameters with defaults
        auto const kernel_size = request->kernel_size() > 0 ? request->kernel_size() : 9;
        auto const sigma = request->sigma() > 0.0 ? request->sigma() : 2.0;

        // Apply Gaussian blur
        cv::Mat blurred;
        cv::GaussianBlur(mat, blurred, cv::Size(kernel_size, kernel_size), sigma);

        // Encode result
        vector<uchar> result;
        cv::imencode(".jpg", blurred, result, vector{cv::IMWRITE_JPEG_QUALITY, 80});

        response->set_image_data(result.data(), result.size());
        response->set_success(true);

        cout << "Processed image: " << image_data.size() << " bytes -> "
             << result.size() << " bytes" << endl;

        return grpc::Status::OK;
      }
      catch (const exception& e)
      {
        response->set_success(false);
        response->set_error_message(e.what());
        return grpc::Status::OK;
      }
    }
  };

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
auto run_server(const std::string& address) -> void
{
  using namespace std;
  using namespace image_processor;

  ImageProcessorServiceImpl service;

  grpc::ServerBuilder builder;
  builder.AddListeningPort(address, grpc::InsecureServerCredentials());
  builder.RegisterService(&service);

  auto server = builder.BuildAndStart();
  cout << "Server listening on " << address << endl;

  server->Wait();
}

int main(const int argc, char* argv[])
{
  using namespace std;

  auto const address = (argc > 1) ? string(argv[1]) : "0.0.0.0:50051"s;

  run_server(address);

  return 0;
}
