#include "message_framing.hpp"

#include <iostream>
#include <print>
#include <format>
#include <ranges>
#include <algorithm>
#include <functional>

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
  // Message generator interface
  //
  ////////////////////////////////////////////////////////////////////////////////
  class MessageGenerator
  {
  public:
    virtual ~MessageGenerator() = default;
    virtual auto generate_message() -> Message = 0;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Response handler interface
  //
  ////////////////////////////////////////////////////////////////////////////////
  class ResponseHandler
  {
  public:
    virtual ~ResponseHandler() = default;
    virtual auto handle_response(const Message& response) -> void = 0;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Default response handler - logs to stderr
  //
  ////////////////////////////////////////////////////////////////////////////////
  class LoggingResponseHandler : public ResponseHandler
  {
  public:
    auto handle_response(const Message& response) -> void override
    {
      cerr << format("Response header: {}\n", response.header);
      cerr << format("Response payload bytes: {}\n", response.payload.size());
    }
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Image data generator for testing
  //
  ////////////////////////////////////////////////////////////////////////////////
  class TestImageGenerator : public MessageGenerator
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
    TestImageGenerator(const ImageConfig config = {})
      : _config{config}
    {}

    auto generate_message() -> Message override
    {
      const auto header = generate_header_json();
      const auto payload = generate_test_image();
      return Message{header, payload};
    }

  private:
    auto generate_test_image() -> vector<uint8_t>
    {
      const auto total_bytes = _config.width * _config.height * _config.channels;
      vector<uint8_t> data(total_bytes);
      ranges::fill(data, 0x7F);
      return data;
    }

    auto generate_header_json() -> string
    {
      return format(
        R"({{"op":"process","mime":"{}","width":{},"height":{},"channels":{},"pixelFormat":"{}"}})",
        _config.mime_type, _config.width, _config.height,
        _config.channels,  _config.pixel_format
      );
    }

  private:
    ImageConfig _config;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // JSON message generator for custom operations
  //
  ////////////////////////////////////////////////////////////////////////////////
  class JsonMessageGenerator : public MessageGenerator
  {
  public:
    JsonMessageGenerator(const string operation, const string data)
      : _operation{operation}
      , _data{data}
    {}

    auto generate_message() -> Message override
    {
      const auto header = format(R"({{"op":"{}","data":"{}"}})", _operation, _data);
      vector<uint8_t> payload(_data.begin(), _data.end());
      return Message{header, payload};
    }

  private:
    string _operation;
    string _data;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Improved client application with dependency injection
  //
  ////////////////////////////////////////////////////////////////////////////////
  class StdioClient
  {
  public:
    StdioClient(unique_ptr<MessageGenerator> generator, 
                unique_ptr<ResponseHandler> handler = nullptr)
      : _input {FileDescriptor::stdin_fd()}
      , _output{FileDescriptor::stdout_fd()}
      , _reader{_input}
      , _writer{_output}
      , _generator{std::move(generator)}
      , _handler{handler ? std::move(handler) : make_unique<LoggingResponseHandler>()}
    {

  public:
    auto run() -> Result<void>
    {
      try
      {
        // Generate and send message
        const auto request = _generator->generate_message();
        if (auto result = _writer.write_message(request); !result)
          return unexpected(result.error());

        // Read response
        auto response_result = _reader.read_message();
        if (!response_result)
          return unexpected(response_result.error());

        if (!response_result.value())
        {
          cerr << "Unexpected EOF from server\n";
          return unexpected(make_error_code(IoError::eof));
        }

        // NOTICE!!
        // Handle response 이 함수가 추가됨으로서 stdio client는 라이브러리가 될 수 있다.
        //
        _handler->handle_response(response_result.value().value());
      }
      catch (const exception& e)
      {
      }

      return {};
    }

    // Alternative: send multiple messages
    auto run_batch(const size_t count) -> Result<void>
    {
      for (size_t i = 0; i < count; ++i)
      {
        if (auto result = run(); !result)
          return result;
      }

      return {};
    }

  private:
    FileDescriptor _input;
    FileDescriptor _output;
    MessageReader _reader;
    MessageWriter _writer;
    unique_ptr<MessageGenerator> _generator;
    unique_ptr<ResponseHandler> _handler;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Client builder for fluent configuration
  //
  ////////////////////////////////////////////////////////////////////////////////
  class StdioClientBuilder
  {
  public:
    auto with_generator(unique_ptr<MessageGenerator> generator) -> StdioClientBuilder&
    {
      _generator = std::move(generator);
      return *this;
    }

    auto with_response_handler(unique_ptr<ResponseHandler> handler) -> StdioClientBuilder&
    {
      _handler = std::move(handler);
      return *this;
    }

    auto build() -> StdioClient
    {
      if (!_generator)
        throw runtime_error("Message generator is required");
        
      return StdioClient{std::move(_generator), std::move(_handler)};
    }

  private:
    unique_ptr<MessageGenerator> _generator;
    unique_ptr<ResponseHandler> _handler;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Usage examples
  //
  ////////////////////////////////////////////////////////////////////////////////
  auto main() -> int
  {
    try
    {
      StdioBinaryMode binary_mode;
      
      // Example 1: Use with test image generator (default)
      {
        auto client = StdioClientBuilder()
          .with_generator(make_unique<TestImageGenerator>())
          .build();
          
        if (auto result = client.run(); !result)
        {
          cerr << format("Client error: {}\n", result.error().message());
          return 1;
        }
      }
      
      // Example 2: Use with custom JSON message
      // {
      //   auto client = StdioClientBuilder()
      //     .with_generator(make_unique<JsonMessageGenerator>("echo", "Hello, Server!"))
      //     .build();
      //     
      //   if (auto result = client.run(); !result)
      //   {
      //     cerr << format("Client error: {}\n", result.error().message());
      //     return 1;
      //   }
      // }
      
      // Example 3: Use with custom response handler
      // {
      //   class CustomResponseHandler : public ResponseHandler
      //   {
      //   public:
      //     auto handle_response(const Message& response) -> void override
      //     {
      //       // Custom processing logic
      //       cout << "Got response with " << response.payload.size() << " bytes\n";
      //     }
      //   };
      //   
      //   auto client = StdioClientBuilder()
      //     .with_generator(make_unique<TestImageGenerator>())
      //     .with_response_handler(make_unique<CustomResponseHandler>())
      //     .build();
      //     
      //   if (auto result = client.run(); !result)
      //   {
      //     cerr << format("Client error: {}\n", result.error().message());
      //     return 1;
      //   }
      // }
      
      return 0;
    }
    catch (const exception& e)
    {
      cerr << format("Exception: {}\n", e.what());
      return 1;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main()
{
  return app::main();
}