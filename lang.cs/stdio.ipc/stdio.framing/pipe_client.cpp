#include "message_framing.hpp"

#include <iostream>
#include <print>
#include <format>
#include <ranges>
#include <algorithm>
#include <memory>
#include <string>

#if defined(_WIN32)
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#else
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#endif

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
  // Cross-platform subprocess management
  //
  ////////////////////////////////////////////////////////////////////////////////
  class SubProcess
  {
  public:
    SubProcess() = default;
    ~SubProcess() { cleanup(); }

    // Non-copyable but moveable
    SubProcess(const SubProcess&) = delete;
    auto operator=(const SubProcess&) -> SubProcess& = delete;
    SubProcess(SubProcess&&) = default;
    auto operator=(SubProcess&&) -> SubProcess& = default;

  public:
    auto start(const string& command) -> Result<void>
    {
      #if defined(_WIN32)
      return start_windows(command);
      #else
      return start_posix(command);
      #endif
    }

    auto get_stdin_fd() const -> int { return _stdin_fd; }
    auto get_stdout_fd() const -> int { return _stdout_fd; }

    auto wait() -> Result<int>
    {
      #if defined(_WIN32)
      return wait_windows();
      #else
      return wait_posix();
      #endif
    }

    auto terminate() -> void
    {
      #if defined(_WIN32)
      if (_process_handle != INVALID_HANDLE_VALUE)
        TerminateProcess(_process_handle, 1);
      #else
      if (_pid > 0)
        kill(_pid, SIGTERM);
      #endif
    }

  private:
    #if defined(_WIN32)
    HANDLE _process_handle = INVALID_HANDLE_VALUE;
    HANDLE _thread_handle = INVALID_HANDLE_VALUE;
    int _stdin_fd = -1;
    int _stdout_fd = -1;

    auto start_windows(const string& command) -> Result<void>
    {
      HANDLE child_stdin_read, child_stdin_write;
      HANDLE child_stdout_read, child_stdout_write;
      
      SECURITY_ATTRIBUTES sa_attr = {};
      sa_attr.nLength = sizeof(SECURITY_ATTRIBUTES);
      sa_attr.bInheritHandle = TRUE;
      sa_attr.lpSecurityDescriptor = nullptr;

      // Create pipes
      if (!CreatePipe(&child_stdout_read, &child_stdout_write, &sa_attr, 0) ||
          !CreatePipe(&child_stdin_read, &child_stdin_write, &sa_attr, 0))
        return unexpected(make_error_code(errc{static_cast<int>(GetLastError())}));

      // Ensure read/write handles are not inherited
      if (!SetHandleInformation(child_stdout_read, HANDLE_FLAG_INHERIT, 0) ||
          !SetHandleInformation(child_stdin_write, HANDLE_FLAG_INHERIT, 0))
        return unexpected(make_error_code(errc{static_cast<int>(GetLastError())}));

      // Convert handles to file descriptors
      _stdout_fd = _open_osfhandle(reinterpret_cast<intptr_t>(child_stdout_read), _O_RDONLY | _O_BINARY);
      _stdin_fd = _open_osfhandle(reinterpret_cast<intptr_t>(child_stdin_write), _O_WRONLY | _O_BINARY);

      // Create process
      PROCESS_INFORMATION pi = {};
      STARTUPINFOA si = {};
      si.cb = sizeof(STARTUPINFOA);
      si.hStdError = child_stdout_write;
      si.hStdOutput = child_stdout_write;
      si.hStdInput = child_stdin_read;
      si.dwFlags |= STARTF_USESTDHANDLES;

      auto cmd_line = const_cast<char*>(command.c_str());
      if (!CreateProcessA(nullptr, cmd_line, nullptr, nullptr, TRUE, 0, nullptr, nullptr, &si, &pi))
        return unexpected(make_error_code(errc{static_cast<int>(GetLastError())}));

      _process_handle = pi.hProcess;
      _thread_handle = pi.hThread;

      // Close child pipe ends
      CloseHandle(child_stdin_read);
      CloseHandle(child_stdout_write);

      return {};
    }

    auto wait_windows() -> Result<int>
    {
      if (_process_handle == INVALID_HANDLE_VALUE)
        return unexpected(make_error_code(errc::invalid_argument));

      WaitForSingleObject(_process_handle, INFINITE);
      DWORD exit_code;
      if (!GetExitCodeProcess(_process_handle, &exit_code))
        return unexpected(make_error_code(errc{static_cast<int>(GetLastError())}));

      return static_cast<int>(exit_code);
    }

    #else
    pid_t _pid = -1;
    int _stdin_fd = -1;
    int _stdout_fd = -1;

    auto start_posix(const string& command) -> Result<void>
    {
      int stdin_pipe[2], stdout_pipe[2];

      if (pipe(stdin_pipe) == -1 || pipe(stdout_pipe) == -1)
        return unexpected(make_error_code(errc{errno}));

      _pid = fork();
      if (_pid == -1)
        return unexpected(make_error_code(errc{errno}));

      if (_pid == 0)
      {
        // Child process
        close(stdin_pipe[1]);   // Close write end of stdin pipe
        close(stdout_pipe[0]);  // Close read end of stdout pipe

        dup2(stdin_pipe[0], STDIN_FILENO);
        dup2(stdout_pipe[1], STDOUT_FILENO);
        dup2(stdout_pipe[1], STDERR_FILENO);

        close(stdin_pipe[0]);
        close(stdout_pipe[1]);

        execl("/bin/sh", "sh", "-c", command.c_str(), nullptr);
        _exit(127);
      }
      else
      {
        // Parent process
        close(stdin_pipe[0]);   // Close read end of stdin pipe
        close(stdout_pipe[1]);  // Close write end of stdout pipe

        _stdin_fd = stdin_pipe[1];
        _stdout_fd = stdout_pipe[0];
      }

      return {};
    }

    auto wait_posix() -> Result<int>
    {
      if (_pid <= 0)
        return unexpected(make_error_code(errc::invalid_argument));

      int status;
      if (waitpid(_pid, &status, 0) == -1)
        return unexpected(make_error_code(errc{errno}));

      if (WIFEXITED(status))
        return WEXITSTATUS(status);
      else if (WIFSIGNALED(status))
        return -(WTERMSIG(status));
      else
        return -1;
    }
    #endif

    auto cleanup() -> void
    {
      #if defined(_WIN32)
      if (_stdin_fd != -1) { _close(_stdin_fd); _stdin_fd = -1; }
      if (_stdout_fd != -1) { _close(_stdout_fd); _stdout_fd = -1; }
      if (_process_handle != INVALID_HANDLE_VALUE) { CloseHandle(_process_handle); _process_handle = INVALID_HANDLE_VALUE; }
      if (_thread_handle != INVALID_HANDLE_VALUE) { CloseHandle(_thread_handle); _thread_handle = INVALID_HANDLE_VALUE; }
      #else
      if (_stdin_fd != -1) { close(_stdin_fd); _stdin_fd = -1; }
      if (_stdout_fd != -1) { close(_stdout_fd); _stdout_fd = -1; }
      if (_pid > 0) { _pid = -1; }
      #endif
    }
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Pipe-based client that starts server automatically
  //
  ////////////////////////////////////////////////////////////////////////////////
  class PipeClient
  {
  public:
    PipeClient(const string& server_command)
      : _server_command{server_command}
    {}

    ~PipeClient()
    {
      if (_server_process)
      {
        _server_process->terminate();
        _server_process->wait();
      }
    }

  public:
    auto run() -> Result<void>
    {
      // Start server process
      _server_process = make_unique<SubProcess>();
      if (auto result = _server_process->start(_server_command); !result)
        return unexpected(result.error());

      // Create file descriptors for communication
      auto server_input = FileDescriptor{_server_process->get_stdin_fd()};
      auto server_output = FileDescriptor{_server_process->get_stdout_fd()};

      MessageWriter writer{server_input};
      MessageReader reader{server_output};

      // Generate test message (same as original client)
      const auto header = generate_header_json();
      const auto payload = generate_test_image();
      const Message request{header, payload};

      // Send message to server
      if (auto result = writer.write_message(request); !result)
        return unexpected(result.error());

      // Read response from server
      auto response_result = reader.read_message();
      if (!response_result)
        return unexpected(response_result.error());

      if (!response_result.value())
      {
        println(stderr, "Unexpected EOF from server\n");
        return unexpected(make_error_code(IoError::eof));
      }

      const auto& response = response_result.value().value();

      // Display response
      println(stderr, "Response header: {}\n", response.header);
      println(stderr, "Response payload bytes: {}\n", response.payload.size());

      // Wait for server to finish
      if (auto exit_code = _server_process->wait(); !exit_code || exit_code.value() != 0)
      {
        println(stderr, "Server exited with code: {}\n", exit_code.value_or(-1));
      }

      return {};
    }

  private:
    auto generate_test_image() -> vector<uint8_t>
    {
      const uint32_t width = 640;
      const uint32_t height = 480;
      const uint32_t channels = 3;
      const auto total_bytes = width * height * channels;
      vector<uint8_t> data(total_bytes, 0x7F);
      return data;
    }

    auto generate_header_json() -> string
    {
      return format(
        R"({{"op":"process","mime":"application/octet-stream","width":640,"height":480,"channels":3,"pixelFormat":"BGR8"}})"
      );
    }

  private:
    string _server_command;
    unique_ptr<SubProcess> _server_process;
  };

  auto main(const int argc, const char* argv[]) -> int
  {
    if (argc < 2)
    {
      println(stderr, "Usage: {} <server_executable>\n", argv[0]);
      println(stderr, "Example: {} ./server\n", argv[0]);
      return 1;
    }

    try
    {
      StdioBinaryMode binary_mode;

      const string server_command = argv[1];
      PipeClient client{server_command};

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
//
////////////////////////////////////////////////////////////////////////////////
int main(const int argc, const char* argv[])
{
  return app::main(argc, argv);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////