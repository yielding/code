#include "message_framing.hpp"

#include <iostream>
#include <format>
#include <print>

////////////////////////////////////////////////////////////////////////////////
//
// Monadic composition examples
//
////////////////////////////////////////////////////////////////////////////////
namespace monadic_examples
{
  using namespace std;
  using namespace xplat::io;
  using namespace xplat::framing;

  //////////////////////////////////////////////////////////////////////////////
  //
  // Helper function for monadic chaining with logging
  //
  //////////////////////////////////////////////////////////////////////////////
  template<typename T>
  auto log_step(const string& step_name, Result<T> result) -> Result<T>
  {
    if (result)
      println(stderr, "[OK] {}\n", step_name);
    else
      println(stderr, "[ERROR] {}: {}\n", step_name, result.error().message());
    
    return result;
  }

  //////////////////////////////////////////////////////////////////////////////
  //
  // Extended MessageWriter with monadic operations
  //
  //////////////////////////////////////////////////////////////////////////////
  class ExtendedMessageWriter : public MessageWriter
  {
  public:
    using MessageWriter::MessageWriter;

    // Write multiple messages in sequence
    auto write_messages(const vector<Message>& messages) -> Result<void>
    {
      Result<void> result;
      
      for (const auto& msg : messages)
      {
        result = write_message(msg);
        if (!result)
          return result;
      }
      
      return {};
    }

    // Write message with transformation
    template<typename Transform>
    auto write_transformed(const Message& msg, Transform transform) -> Result<void>
    {
      return transform(msg)
        .and_then([this](const auto& transformed) {
          return write_message(transformed);
        });
    }

    // Write message with validation
    auto write_validated(const Message& msg) -> Result<void>
    {
      return validate_message(msg)
        .and_then([this, &msg]() { return write_message(msg); });
    }

  private:
    auto validate_message(const Message& msg) -> Result<void>
    {
      if (msg.header.empty())
        return unexpected(make_error_code(errc::invalid_argument));
      
      // Max size check (example: 10MB)
      constexpr size_t MAX_PAYLOAD_SIZE = 10 * 1024 * 1024;
      if (msg.payload.size() > MAX_PAYLOAD_SIZE)
        return unexpected(make_error_code(errc::value_too_large));
      
      return {};
    }
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Pipeline builder for message processing
  //
  ////////////////////////////////////////////////////////////////////////////////
  class MessagePipeline
  {
  public:
    using Transform = function<Result<Message>(const Message&)>;

    MessagePipeline& add_transform(Transform transform)
    {
      _transforms.push_back(std::move(transform));
      return *this;
    }

    auto process(const Message& input) -> Result<Message>
    {
      auto result = Result<Message>{input};
      
      for (const auto& transform : _transforms)
      {
        result = result.and_then(transform);
      }
      
      return result;
    }

  private:
    vector<Transform> _transforms;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Example transformations
  //
  ////////////////////////////////////////////////////////////////////////////////
  auto compress_payload(const Message& msg) -> Result<Message>
  {
    auto compressed = msg;
    compressed.header = format(R"({{"compressed":true,"original":{}}})", msg.header);
    return compressed;
  }

  auto encrypt_payload(const Message& msg) -> Result<Message>
  {
    auto encrypted = msg;
    for (auto& byte : encrypted.payload)
      byte ^= 0x42;
    
    encrypted.header = format(R"({{"encrypted":true,"original":{}}})", msg.header);
    return encrypted;
  }

  auto add_timestamp(const Message& msg) -> Result<Message>
  {
    auto timestamped = msg;
    const auto now = chrono::system_clock::now().time_since_epoch().count();
    timestamped.header = format(R"({{"timestamp":{},"original":{}}})", now, msg.header);
    return timestamped;
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Usage examples
  //
  ////////////////////////////////////////////////////////////////////////////////
  auto demonstrate_monadic_chaining() -> void
  {
    StdioBinaryMode binary_mode;
    auto fd = FileDescriptor::stdout_fd();
    ExtendedMessageWriter writer{fd};

    // Example 1: Simple monadic chain
    {
      Message msg{"test", {1, 2, 3}};
      
      auto result = writer.write_validated(msg)
        .and_then([]() { 
          println(stderr, "Message sent successfully\n");
          return Result<void>{};
        })
        .or_else([](const error_code& ec) {
          println(stderr, "Failed to send: {}\n", ec.message());
          return Result<void>{};
        });
    }

    // Example 2: Pipeline processing
    {
      MessagePipeline pipeline;
      pipeline
        .add_transform(compress_payload)
        .add_transform(encrypt_payload)
        .add_transform(add_timestamp);

      Message original{"data", {1, 2, 3, 4, 5}};
      
      auto processed = pipeline.process(original)
        .and_then([&writer](const Message& msg) {
          return writer.write_message(msg).and_then([&msg]() {
            return Result<Message>{msg};
          });
        });

      if (processed)
        println(stderr, "Pipeline succeeded, header: {}\n", processed->header);
    }

    // Example 3: Batch processing with early termination
    {
      vector<Message> messages = {
        {"msg1", {1}},
        {"msg2", {2}},
        {"msg3", {3}}
      };

      auto result = expected<void, error_code>{}
        .and_then([&]() { return writer.write_messages(messages); })
        .and_then([]() { 
          println(stderr, "All messages sent\n");
          return Result<void>{};
        });
    }

    // Example 4: Functional composition
    {
      auto send_with_retry = [&writer](const Message& msg, const int max_retries) -> Result<void>
      {
        for (int i = 0; i < max_retries; ++i)
        {
          if (auto result = writer.write_message(msg); result)
            return result;
          else if (i < max_retries - 1)
            println(stderr, "Retry {} after error: {}\n", i + 1, result.error().message());
          else
            return result;
        }
        return unexpected(make_error_code(errc::io_error));
      };

      Message msg{"retry_test", {42}};
      auto result = send_with_retry(msg, 3);
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
  monadic_examples::demonstrate_monadic_chaining();
  return 0;
}