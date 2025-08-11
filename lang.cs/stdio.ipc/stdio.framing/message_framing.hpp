#pragma once

#include "xplat_io.hpp"

#include <memory>
#include <optional>
#include <string_view>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace xplat::framing
{
  using namespace std;
  using namespace xplat::io;

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Message structure with header and payload
  //
  ////////////////////////////////////////////////////////////////////////////////
  struct Message
  {
    string header;
    vector<uint8_t> payload;

    Message() = default;
    Message(string h, vector<uint8_t> p)
      : header{std::move(h)}, payload{std::move(p)}
    {}
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Message reader with protocol handling
  //
  ////////////////////////////////////////////////////////////////////////////////
  class MessageReader
  {
  public:
    explicit MessageReader(FileDescriptor& fd)
      : _fd{fd}
    {}

  public:
    auto read_message() -> expected<optional<Message>, error_code>
    {
      // Read header length (4 bytes, BE)
      uint32_t header_len_be;
      auto result = _fd.read_exact(span{reinterpret_cast<uint8_t*>(&header_len_be), sizeof(header_len_be)});
      if (!result)
      {
        if (result.error() == make_error_code(IoError::eof))
          return optional<Message>{};

        return unexpected(result.error());
      }

      // Read payload length (8 bytes, BE)
      uint64_t payload_len_be;
      if (auto res = _fd.read_exact(span{reinterpret_cast<uint8_t*>(&payload_len_be), sizeof(payload_len_be)}); !res)
        return unexpected(res.error());

      const auto header_len  = from_network_order(header_len_be);
      const auto payload_len = from_network_order(payload_len_be);

      // Read header
      string header(header_len, '\0');
      if (auto res = _fd.read_exact(span{reinterpret_cast<uint8_t*>(header.data()), header.size()}); !res)
        return unexpected(res.error());

      // Read payload if present
      vector<uint8_t> payload;
      if (payload_len > 0)
      {
        payload.resize(static_cast<size_t>(payload_len));
        if (auto res = _fd.read_exact(payload); !res)
          return unexpected(res.error());
      }

      return Message{std::move(header), std::move(payload)};
    }

  private:
    FileDescriptor& _fd;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Message writer with protocol handling
  //
  ////////////////////////////////////////////////////////////////////////////////
  class MessageWriter
  {
  public:
    explicit MessageWriter(FileDescriptor& fd)
      : _fd{fd}
    {}

  public:
    auto write_message(const Message& msg) -> expected<void, error_code>
    {
      const auto header_len  = to_network_order(static_cast<uint32_t>(msg.header.size()));
      const auto payload_len = to_network_order(static_cast<uint64_t>(msg.payload.size()));

      // Write header length
      if (auto res = _fd.write_all(span{reinterpret_cast<const uint8_t*>(&header_len), sizeof(header_len)}); !res)
        return res;

      // Write payload length
      if (auto res = _fd.write_all(span{reinterpret_cast<const uint8_t*>(&payload_len), sizeof(payload_len)}); !res)
        return res;

      // Write header
      if (auto res = _fd.write_all(span{reinterpret_cast<const uint8_t*>(msg.header.data()), msg.header.size()}); !res)
        return res;

      // Write payload if present
      if (!msg.payload.empty())
      {
        if (auto res = _fd.write_all(msg.payload); !res)
          return res;
      }

      return {};
    }

  private:
    FileDescriptor& _fd;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Message processor interface
  //
  ////////////////////////////////////////////////////////////////////////////////
  class MessageProcessor
  {
  public:
    virtual ~MessageProcessor() = default;
    virtual auto process(const Message& input) -> Message = 0;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Echo processor - returns input as output
  //
  ////////////////////////////////////////////////////////////////////////////////
  class EchoProcessor : public MessageProcessor
  {
  public:
    auto process(const Message& input) -> Message override
    {
      return input;
    }
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Server loop with message processing
  //
  ////////////////////////////////////////////////////////////////////////////////
  class MessageServer
  {
  public:
    MessageServer(FileDescriptor& input, FileDescriptor& output, unique_ptr<MessageProcessor> processor)
      : _reader{input}
      , _writer{output}
      , _processor{std::move(processor)}
    {}

  public:
    auto run() -> expected<void, error_code>
    {
      while (true)
      {
        auto msg_result = _reader.read_message();
        if (!msg_result)
          return unexpected(msg_result.error());

        if (!msg_result.value())
          break; // EOF

        const auto response = _processor->process(msg_result.value().value());
        
        if (auto write_result = _writer.write_message(response); !write_result)
          return unexpected(write_result.error());
      }

      return {};
    }

  private:
    MessageReader _reader;
    MessageWriter _writer;
    unique_ptr<MessageProcessor> _processor;
  };
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////