#pragma once

#include "xplat_io.hpp"

#include <memory>
#include <optional>
#include <string_view>
#include <atomic>
#include <iostream>
#include <print>
#include <format>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace xplat::framing
{
  using namespace std;
  using namespace xplat::io;

  //////////////////////////////////////////////////////////////////////////////
  //
  // Helper functions for byte span conversion
  //
  //////////////////////////////////////////////////////////////////////////////
  template<typename T>
  auto as_bytes(T& value) -> span<uint8_t>
  {
    return span{reinterpret_cast<uint8_t*>(&value), sizeof(value)};
  }

  template<typename T>
  auto as_const_bytes(const T& value) -> span<const uint8_t>
  {
    return span{reinterpret_cast<const uint8_t*>(&value), sizeof(value)};
  }

  inline auto as_bytes(string& str) -> span<uint8_t>
  {
    return span{reinterpret_cast<uint8_t*>(str.data()), str.size()};
  }

  inline auto as_const_bytes(const string& str) -> span<const uint8_t>
  {
    return span{reinterpret_cast<const uint8_t*>(str.data()), str.size()};
  }

  //////////////////////////////////////////////////////////////////////////////
  //
  // Message structure with header and payload
  //
  //////////////////////////////////////////////////////////////////////////////
  struct Message
  {
    Message() = default;
    Message(string h, vector<uint8_t> p)
      : header{std::move(h)}, payload{std::move(p)}
    {}

    auto header_size() const -> uint32_t 
    { 
      return static_cast<uint32_t>(header.size());
    }

    auto payload_size() const -> uint64_t 
    { 
      return static_cast<uint64_t>(payload.size());
    }

    string header;
    vector<uint8_t> payload;
  };

  //////////////////////////////////////////////////////////////////////////////
  //
  // Message reader with protocol handling
  //
  //////////////////////////////////////////////////////////////////////////////
  class MessageReader
  {
  public:
    explicit MessageReader(FileDescriptor& fd)
      : _fd{fd}
    {}

  public:
    auto read_message() -> Result<optional<Message>>
    {
      // Read header length (4 bytes, BE)
      uint32_t header_len_be;
      auto result = _fd.read_exact(as_bytes(header_len_be));
      if (!result)
      {
        if (result.error() == make_error_code(IoError::eof))
          return optional<Message>{};

        return unexpected(result.error());
      }

      // Read payload length (8 bytes, BE)
      uint64_t payload_len_be;
      if (auto res = _fd.read_exact(as_bytes(payload_len_be)); !res)
        return unexpected(res.error());

      const auto header_len  = from_network_order(header_len_be);
      const auto payload_len = from_network_order(payload_len_be);

      // Read header
      string header(header_len, '\0');
      if (auto res = _fd.read_exact(as_bytes(header)); !res)
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
    auto write_message(const Message& msg) -> Result<void>
    {
      const auto header_len  = to_network_order((msg.header_size()));
      const auto payload_len = to_network_order((msg.payload_size()));

      return write_header_length(header_len)
        .and_then([this, payload_len]() { return write_payload_length(payload_len); })
        .and_then([this, &msg]() { return write_header(msg.header); })
        .and_then([this, &msg]() { return write_payload(msg.payload); });
    }

  private:
    auto write_header_length(const uint32_t header_len_be) -> Result<void>
    {
      return _fd.write_all(as_const_bytes(header_len_be));
    }

    auto write_payload_length(const uint64_t payload_len_be) -> Result<void>
    {
      return _fd.write_all(as_const_bytes(payload_len_be));
    }

    auto write_header(const string& header) -> Result<void>
    {
      return _fd.write_all(as_const_bytes(header));
    }

    auto write_payload(const vector<uint8_t>& payload) -> Result<void>
    {
      if (payload.empty())
        return {};

      return _fd.write_all(payload);
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
  // Error handler for server
  //
  ////////////////////////////////////////////////////////////////////////////////
  class ErrorHandler
  {
  public:
    virtual ~ErrorHandler() = default;
    virtual auto handle_read_error(const error_code& ec) -> bool = 0;  // return true to continue
    virtual auto handle_write_error(const error_code& ec) -> bool = 0; // return true to continue
    virtual auto handle_process_error(const exception& e) -> bool = 0; // return true to continue
    virtual auto on_client_disconnect() -> void = 0;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Default error handler - logs errors to stderr
  //
  ////////////////////////////////////////////////////////////////////////////////
  class DefaultErrorHandler : public ErrorHandler
  {
  public:
    auto handle_read_error(const error_code& ec) -> bool override
    {
      if (ec == make_error_code(IoError::eof))
        return false; // Normal EOF, stop server
      
      if (ec == make_error_code(errc::connection_reset) ||
          ec == make_error_code(errc::broken_pipe))
      {
        println(stderr, "[INFO] Client disconnected\n");
        return false;
      }
      
      println(stderr, "[ERROR] Read failed: {} ({})\n", ec.message(), ec.value());
      return false; // Stop on any read error
    }

    auto handle_write_error(const error_code& ec) -> bool override
    {
      if (ec == make_error_code(errc::broken_pipe))
      {
        println(stderr, "[INFO] Client disconnected during write\n");
        return false;
      }
      
      println(stderr, "[ERROR] Write failed: {} ({})\n", ec.message(), ec.value());
      return false; // Stop on any write error
    }

    auto handle_process_error(const exception& e) -> bool override
    {
      println(stderr, "[ERROR] Message processing failed: {}\n", e.what());
      return true; // Continue processing other messages
    }

    auto on_client_disconnect() -> void override
    {
      println(stderr, "[INFO] Server shutting down gracefully\n");
    }
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Server loop with message processing and error handling
  //
  ////////////////////////////////////////////////////////////////////////////////
  class MessageServer
  {
  public:
    MessageServer(FileDescriptor& input, FileDescriptor& output, 
                  unique_ptr<MessageProcessor> processor,
                  unique_ptr<ErrorHandler> error_handler = nullptr)
      : _reader{input}
      , _writer{output}
      , _processor{std::move(processor)}
      , _error_handler{error_handler ? std::move(error_handler) : make_unique<DefaultErrorHandler>()}
    {}

  public:
    auto run() -> Result<void>
    {
      while (_running)
      {
        auto msg_result = _reader.read_message();
        if (!msg_result)
        {
          if (!_error_handler->handle_read_error(msg_result.error()))
          {
            _error_handler->on_client_disconnect();
            break;
          }

          continue; // try the next message if handler says continue
        }

        if (!msg_result.value()) // Check for EOF
        {
          _error_handler->on_client_disconnect();
          break;
        }

        Message response;
        try
        {
          response = _processor->process(msg_result.value().value());
        }
        catch (const exception& e)
        {
          if (!_error_handler->handle_process_error(e)) break;
          continue; // skip writing response if processing failed
        }
        
        // write response with error handling
        if (auto write_result = _writer.write_message(response); !write_result)
        {
          if (!_error_handler->handle_write_error(write_result.error()))
          {
            _error_handler->on_client_disconnect();
            break;
          }
        }
        
        // For stdio IPC, process one message and exit
        // This allows the client-server pipe to complete cleanly
        break;
      }

      return {};
    }

    auto stop() -> void
    {
      _running = false;
    }

  private:
    MessageReader _reader;
    MessageWriter _writer;
    unique_ptr<MessageProcessor> _processor;
    unique_ptr<ErrorHandler> _error_handler;
    atomic<bool> _running{true};
  };
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////