#include "message_framing.hpp"

#include <iostream>
#include <print>
#include <format>
#include <ranges>
#include <algorithm>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace app
{
  using namespace std;
  using namespace xplat::io;
  using namespace xplat::framing;

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Image data generator for testing
  //
  ////////////////////////////////////////////////////////////////////////////////
  class TestImageGenerator
  {
  public:
    struct ImageConfig
    {
      uint32_t width = 640;
      uint32_t height = 480;
      uint32_t channels = 3;
      string pixel_format = "BGR8";
      string mime_type = "application/octet-stream";
    };

  public:
    auto generate_test_image(const ImageConfig& config) -> vector<uint8_t>
    {
      const auto total_bytes = config.width * config.height * config.channels;
      vector<uint8_t> data(total_bytes, 0x7F);  // Fill during construction
      return data;
    }

    auto generate_header_json(const ImageConfig& config) -> string
    {
      return format(
        R"({{"op":"process","mime":"{}","width":{},"height":{},"channels":{},"pixelFormat":"{}"}})",
        config.mime_type, config.width, config.height,
        config.channels,  config.pixel_format
      );
    }
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Client application
  //
  ////////////////////////////////////////////////////////////////////////////////
  class StdioClient
  {
  public:
    StdioClient()
      : _input {FileDescriptor::stdin_fd()}
      , _output{FileDescriptor::stdout_fd()}
      , _reader{_input}
      , _writer{_output}
    {}

  public:
    auto run() -> expected<void, error_code>
    {
      TestImageGenerator generator;
      const auto config = TestImageGenerator::ImageConfig{};
      
      // Generate test data
      const auto header  = generator.generate_header_json(config);
      const auto payload = generator.generate_test_image(config);
      
      // Send message
      const Message request{header, payload};
      if (auto result = _writer.write_message(request); !result)
        return unexpected(result.error());
      
      // Flush output to ensure message is sent
      fflush(stdout);
      
      // Read response  
      auto response_result = _reader.read_message();
      if (!response_result)
        return unexpected(response_result.error());
      
      if (!response_result.value())
      {
        println(stderr, "Unexpected EOF from server\n");
        return unexpected(make_error_code(IoError::eof));
      }
      
      const auto& response = response_result.value().value();
      
      // Display response info
      println(stderr, "Response header: {}\n", response.header);
      println(stderr, "Response payload bytes: {}\n", response.payload.size());
      
      // Process will exit here, automatically closing pipes and sending EOF to server
      return {};
    }

  private:
    FileDescriptor _input;
    FileDescriptor _output;
    MessageReader _reader;
    MessageWriter _writer;
  };

  auto main() -> int
  {
    try
    {
      StdioBinaryMode binary_mode;
      
      StdioClient client;
      if (auto result = client.run(); !result)
      {
        println(stderr, "Client error: {}\n", result.error().message());
        return 1;
      }
      
      return 0;
    }
    catch (const exception& e)
    {
      println(stderr, "Exception: {}\n", e.what());
      return 1;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
// /////////////////////////////////////////////////////////////////////////////
int main()
{
  return app::main();
}
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////