#include "upload_client.hpp"

#include <print>
#include <vector>
#include <string>
#include <nlohmann/json.hpp>

using namespace std;
using json = nlohmann::json;

////////////////////////////////////////////////////////////////////////////////
//
// Main function
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[])
{
  // Check command line arguments
  if (argc < 3) 
  {
    println(stderr, "Usage: {} <host> <port> [image1] [image2] ...", argv[0]);
    println(stderr, "Example: {} 172.16.253.34 8000 frame_1.jpg frame_2.jpg frame_3.jpg", argv[0]);
    return 1;
  }

  auto host = argv[1];
  auto port = argv[2];

  // Collect image paths
  vector<string> image_paths;
  for (int i = 3; i < argc; ++i)
    image_paths.push_back(argv[i]);

  // Default test images if none provided
  if (image_paths.empty()) 
  {
    println("No images provided. Using example: frame_1.jpg, frame_2.jpg");
    image_paths = {"frame_1.jpg", "frame_2.jpg"};
  }

  println("Uploading to {}:{}", host, port);
  println("Images to upload:");
  for (const auto& path : image_paths)
    println("  - {}", path);
  
  println("");

  // Create client and upload
  net::MultipartUploadClient client{host, port};
  
  // Progress callback for tracking upload
  auto progress_callback = [](size_t bytes_sent, size_t total_bytes) {
    auto percentage = (bytes_sent * 100) / total_bytes;
    println("Upload progress: {}% ({}/{})", percentage, bytes_sent, total_bytes);
  };
  
  auto response = client.upload_with_json_and_images(
    "NpRec",       // analyzerType
    "Yolov5Att",   // detect model
    "Korea",       // OCR model
    image_paths,   // image files
    "/infer",      // endpoint
    progress_callback  // progress tracking
  );

  println("\nResponse Status Code: {}", response.status_code);
  
  // Pretty print JSON response if valid
  try 
  {
    auto json_response = json::parse(response.body);
    println("Response Body (formatted):\n{}", json_response.dump(2));
  } 
  catch (const json::parse_error&) 
  {
    println("Response Body (raw): {}", response.body);
  }
  
  if (!response.success)
  {
    println(stderr, "\nUpload failed.");
    return 1;
  }

  println("\nUpload completed successfully!");

  return 0;
}