#pragma once

#include <cstdint>
#include <cstring>
#include <span>
#include <vector>
#include <string>
#include <expected>
#include <system_error>
#include <format>
#include <bit>

#if defined(_WIN32)
  #include <io.h>
  #include <fcntl.h>
#else
  #include <unistd.h>
#endif

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace xplat::io
{
  using namespace std;

  template<typename T>
  using Result = expected<T, error_code>;

  enum class IoError
  {
    eof,
    interrupted,
    system_error
  };

  inline auto make_error_code(const IoError e) -> error_code
  {
    static const struct : error_category
    {
      auto name() const noexcept -> const char* override { return "io_error"; }

      auto message(const int ev) const -> string override
      {
        switch (static_cast<IoError>(ev))
        {
          case IoError::eof: return "end of file";
          case IoError::interrupted: return "operation interrupted";
          case IoError::system_error: return "system error";
          default: return "unknown error";
        }
      }
    } category;
    
    return {static_cast<int>(e), category};
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Platform-specific stdio configuration
  //
  ////////////////////////////////////////////////////////////////////////////////
  class StdioBinaryMode
  {
  public:
    StdioBinaryMode()
    {
#if defined(_WIN32)
      _setmode(_fileno(stdin), _O_BINARY);
      _setmode(_fileno(stdout), _O_BINARY);
#endif
    }
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // File descriptor wrapper with RAII
  //
  ////////////////////////////////////////////////////////////////////////////////
  class FileDescriptor
  {
  public:
    explicit FileDescriptor(const int fd) : _fd(fd) {}

    static auto stdin_fd() -> FileDescriptor
    {
#if defined(_WIN32)
      return FileDescriptor{_fileno(stdin)};
#else
      return FileDescriptor{STDIN_FILENO};
#endif
    }

    static auto stdout_fd() -> FileDescriptor
    {
#if defined(_WIN32)
      return FileDescriptor{_fileno(stdout)};
#else
      return FileDescriptor{STDOUT_FILENO};
#endif
    }

  public:
    auto read_exact(span<uint8_t> buffer) -> Result<void>
    {
      size_t offset = 0;
      while (offset < buffer.size())
      {
        const auto remaining = buffer.size() - offset;
#if defined(_WIN32)
        const auto result = ::_read(_fd, buffer.data() + offset, static_cast<unsigned>(remaining));
#else
        const auto result = ::read(_fd, buffer.data() + offset, remaining);
#endif
        if (result == 0)
          return unexpected(make_error_code(IoError::eof));
        
        if (result < 0)
        {
          if (errno == EINTR) continue;
          return unexpected(make_error_code(errc{errno}));
        }
        
        offset += static_cast<size_t>(result);
      }
      
      return {};
    }

    auto write_all(span<const uint8_t> buffer) -> Result<void>
    {
      size_t offset = 0;
      while (offset < buffer.size())
      {
        const auto remaining = buffer.size() - offset;
#if defined(_WIN32)
        const auto result = ::_write(_fd, buffer.data() + offset, static_cast<unsigned>(remaining));
#else
        const auto result = ::write(_fd, buffer.data() + offset, remaining);
#endif
        if (result < 0)
        {
          if (errno == EINTR) continue;
          return unexpected(make_error_code(errc{errno}));
        }
        
        offset += static_cast<size_t>(result);
      }
      
      return {};
    }

  private:
    int _fd;
  };

  //////////////////////////////////////////////////////////////////////////////
  //
  // Network byte order conversions using std::bit_cast and std::byteswap
  //
  //////////////////////////////////////////////////////////////////////////////
  template<typename T>
  requires(sizeof(T) == 4 || sizeof(T) == 8)
  auto to_network_order(const T value) -> T
  {
    if constexpr (endian::native == endian::big)
      return value;

    return byteswap(value);
  }

  template<typename T>
  requires(sizeof(T) == 4 || sizeof(T) == 8)
  auto from_network_order(const T value) -> T
  {
    return to_network_order(value); // Same operation for both directions
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////