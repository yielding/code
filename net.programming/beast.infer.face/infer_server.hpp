#pragma once

#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <nlohmann/json.hpp>

#include <string>
#include <string_view>
#include <vector>
#include <chrono>
#include <optional>
#include <expected>
#include <functional>

////////////////////////////////////////////////////////////////////////////////
//
// Triton Inference Server client using Boost.Beast
//
////////////////////////////////////////////////////////////////////////////////
namespace net
{
  using namespace std;
  namespace beast = boost::beast;
  namespace http = beast::http;
  namespace asio = boost::asio;
  using tcp = asio::ip::tcp;
  using json = nlohmann::json;

  struct Response
  {
    unsigned status_code;
    string body;
    bool success;
  };

  using ProgressHandler = function<void(size_t bytes_sent, size_t total_bytes)>;

  class TritonClient
  {
  public:
    static constexpr size_t MAX_FILE_SIZE = 100 * 1024 * 1024; // 100MB
    static constexpr auto DEFAULT_TIMEOUT = std::chrono::seconds(30);

  public:
    TritonClient(string host, string port);
    ~TritonClient();

  public:
    // 방법 2: 일반화된 infer 메서드
    auto infer(
      string_view analyzer_type,
      const json& model_types,
      const vector<string>& image_paths,
      string_view endpoint = "/infer",
      const ProgressHandler& progress_handler = nullptr) -> expected<Response, string>;

    // 단일 파일 (메모리에서)
    auto infer(
      string_view analyzer_type,
      const json& model_types,
      string_view filename,
      const vector<unsigned char>& content,
      string_view endpoint = "/infer",
      const ProgressHandler& progress_handler = nullptr) -> expected<Response, string>;

    auto is_connected() const -> bool;
    auto reconnect_if_needed() -> void;
    auto disconnect() -> void;

  private:
    auto upload_multipart_request(
      string_view analyzer_type,
      const json& model_types,
      string_view endpoint,
      const ProgressHandler& progress_handler,
      const function<void(string&, string_view)>& add_files_callback) -> Response;

    auto generate_boundary() -> string;
    auto get_mime_type(string_view filename) -> string;
    auto read_file(string_view file_path) -> optional<vector<unsigned char>>;
    auto validate_endpoint(string_view endpoint) -> bool;
    auto add_form_field(string& body, string_view boundary, string_view name, string_view value) -> void;
    auto add_form_field_json(string& body, string_view boundary, string_view name, string_view json_value) -> void;
    auto add_form_file(string& body, string_view boundary, string_view name, string_view filename, vector<unsigned char>&& data) -> void;
    auto close() -> void;

  private:
    string _host;
    string _port;
    asio::io_context _ioc;
    tcp::resolver _resolver;
    beast::tcp_stream _stream;
    bool _connected = false;
    tcp::resolver::results_type _endpoints;
  };
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
