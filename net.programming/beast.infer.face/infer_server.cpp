#include "triton_client.hpp"

#include <boost/beast/version.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include <fstream>
#include <filesystem>
#include <algorithm>
#include <stdexcept>
#include <regex>
#include <print>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace net
{
  namespace fs = std::filesystem;

  TritonClient::TritonClient(string host, string port)
    : _host{std::move(host)}
    , _port{std::move(port)}
    , _resolver{_ioc}
    , _stream{_ioc}
    , _connected{false}
  {}

  TritonClient::~TritonClient()
  {
    close();
  }

  auto TritonClient::infer(
      string_view analyzer_type,
      const json& model_types,
      const vector<string>& image_paths,
      string_view endpoint,
      const ProgressHandler& progress_handler) -> expected<Response, string>
  {
    try
    {
      auto response = upload_multipart_request(
        analyzer_type, model_types, endpoint, progress_handler,
        [&](string& body, string_view boundary)
        {
          for (const auto& image_path : image_paths)
          {
            auto image_data_opt = read_file(image_path);
            if (!image_data_opt.has_value())
            {
              println(stderr, "Failed to read image: {}", image_path);
              continue;
            }

            auto filename = fs::path(image_path).filename().string();
            add_form_file(body, boundary, "sources", filename,
                          std::move(image_data_opt.value()));
          }
        });

      if (response.status_code < 200 || response.status_code >= 300)
        return unexpected{format("HTTP error {}: {}", response.status_code, response.body)};

      if (response.body.empty())
        return unexpected{"Empty response from server"};

      if (response.body[0] != '{' && response.body[0] != '[')
        return unexpected{format("Invalid JSON response: {}", response.body.substr(0, 100))};

      return response;
    }
    catch (const exception& e)
    {
      return unexpected{format("Network error during inference: {}", e.what())};
    }
  }

  auto TritonClient::infer(
      string_view analyzer_type,
      const json& model_types,
      string_view filename,
      const vector<unsigned char>& content,
      string_view endpoint,
      const ProgressHandler& progress_handler) -> expected<Response, string>
  {
    try
    {
      auto response = upload_multipart_request(
        analyzer_type, model_types, endpoint, progress_handler,
        [&](string& body, string_view boundary)
        {
          auto content_copy = content;
          add_form_file(body, boundary, "sources", filename, std::move(content_copy));
        });

      if (response.status_code < 200 || response.status_code >= 300)
        return unexpected{format("HTTP error {}: {}", response.status_code, response.body)};

      if (response.body.empty())
        return unexpected{"Empty response from server"};

      if (response.body[0] != '{' && response.body[0] != '[')
        return unexpected{format("Invalid JSON response: {}", response.body.substr(0, 100))};

      return response;
    }
    catch (const exception& e)
    {
      return unexpected{format("Network error during inference: {}", e.what())};
    }
  }

  auto TritonClient::upload_multipart_request(
      string_view analyzer_type,
      const json& model_types,
      string_view endpoint,
      const ProgressHandler& progress_handler,
      const function<void(string&, string_view)>& add_files_callback) -> Response
  {
    Response response{0, "", false};

    try
    {
      if (!validate_endpoint(endpoint))
        throw invalid_argument("Invalid endpoint format. Must start with '/'.");

      reconnect_if_needed();

      _stream.expires_after(DEFAULT_TIMEOUT);

      auto boundary = generate_boundary();

      string body;

      add_form_field(body, boundary, "analyzerType", analyzer_type);
      add_form_field_json(body, boundary, "modelTypes", model_types.dump());

      add_files_callback(body, boundary);

      body += "--" + boundary + "--\r\n";

      http::request<http::string_body> req{http::verb::post, endpoint, 11};
      req.set(http::field::host, _host);
      req.set(http::field::user_agent, BOOST_BEAST_VERSION_STRING);
      req.set(http::field::content_type, "multipart/form-data; boundary=" + boundary);
      req.body() = body;
      req.prepare_payload();

      if (progress_handler)
      {
        auto total_bytes = req.body().size();
        http::write(_stream, req);
        progress_handler(total_bytes, total_bytes);
      }
      else
      {
        http::write(_stream, req);
      }

      beast::flat_buffer buffer;
      http::response<http::dynamic_body> res;
      http::read(_stream, buffer, res);

      response.status_code = res.result_int();
      response.body = beast::buffers_to_string(res.body().data());
      response.success = res.result() == http::status::ok ||
                         res.result() == http::status::created;

      return response;
    }
    catch (const beast::system_error& e)
    {
      println(stderr, "Network error: {}", e.what());
      response.body = string("Network error: ") + e.what();
      return response;
    }
    catch (const invalid_argument& e)
    {
      println(stderr, "Invalid argument: {}", e.what());
      response.body = string("Invalid argument: ") + e.what();
      return response;
    }
    catch (const exception& e)
    {
      println(stderr, "Error: {}", e.what());
      response.body = e.what();
      return response;
    }
  }

  auto TritonClient::is_connected() const -> bool
  {
    return _connected && _stream.socket().is_open();
  }

  auto TritonClient::reconnect_if_needed() -> void
  {
    if (!is_connected())
    {
      if (_endpoints.empty())
        _endpoints = _resolver.resolve(_host, _port);

      _stream.connect(_endpoints);
      _connected = true;
    }
  }

  auto TritonClient::disconnect() -> void
  {
    if (_connected)
    {
      beast::error_code ec;
      _stream.socket().shutdown(tcp::socket::shutdown_both, ec);
      if (ec && ec != beast::errc::not_connected)
        println(stderr, "Shutdown error: {}", ec.message());

      _connected = false;
    }
  }

  auto TritonClient::generate_boundary() -> string
  {
    auto uuid = boost::uuids::random_generator{}();
    return "----WebKitFormBoundary" + boost::uuids::to_string(uuid).substr(0, 16);
  }

  auto TritonClient::get_mime_type(string_view filename) -> string
  {
    auto ext = fs::path(filename).extension().string();
    ranges::transform(ext, ext.begin(), ::tolower);

    if (ext == ".jpg" || ext == ".jpeg") return "image/jpeg";
    if (ext == ".png")  return "image/png";
    if (ext == ".gif")  return "image/gif";
    if (ext == ".bmp")  return "image/bmp";
    if (ext == ".webp") return "image/webp";

    return "application/octet-stream";
  }

  auto TritonClient::read_file(string_view file_path) -> optional<vector<unsigned char>>
  {
    fs::path path{file_path};

    if (!fs::exists(path))
    {
      println(stderr, "File not found: {}", file_path);
      return nullopt;
    }

    auto file_size = fs::file_size(path);
    if (file_size > MAX_FILE_SIZE)
    {
      println(stderr, "File too large: {} bytes (max: {} bytes)", file_size, MAX_FILE_SIZE);
      return nullopt;
    }

    ifstream file{path, ios::binary};
    if (!file)
    {
      println(stderr, "Cannot open file: {}", file_path);
      return nullopt;
    }

    vector<unsigned char> buffer(file_size);
    file.read(reinterpret_cast<char*>(buffer.data()), file_size);

    if (!file)
    {
      println(stderr, "Failed to read complete file: {}", file_path);
      return nullopt;
    }

    return buffer;
  }

  auto TritonClient::validate_endpoint(string_view endpoint) -> bool
  {
    static const regex endpoint_regex("^/[a-zA-Z0-9/_.-]*$");
    return regex_match(endpoint.begin(), endpoint.end(), endpoint_regex);
  }

  auto TritonClient::add_form_field(string& body, string_view boundary,
                                    string_view name, string_view value) -> void
  {
    body += "--";
    body += boundary;
    body += "\r\n";
    body += "Content-Disposition: form-data; name=\"";
    body += name;
    body += "\"\r\n\r\n";
    body += value;
    body += "\r\n";
  }

  auto TritonClient::add_form_field_json(string& body, string_view boundary,
                                         string_view name, string_view json_value) -> void
  {
    body += "--";
    body += boundary;
    body += "\r\n";
    body += "Content-Disposition: form-data; name=\"";
    body += name;
    body += "\"\r\n";
    body += "Content-Type: application/json\r\n\r\n";
    body += json_value;
    body += "\r\n";
  }

  auto TritonClient::add_form_file(string& body, string_view boundary,
                                   string_view name, string_view filename,
                                   vector<unsigned char>&& data) -> void
  {
    body += "--";
    body += boundary;
    body += "\r\n";
    body += "Content-Disposition: form-data; name=\"";
    body += name;
    body += "\"; filename=\"";
    body += filename;
    body += "\"\r\n";
    body += "Content-Type: ";
    body += get_mime_type(filename);
    body += "\r\n\r\n";
    body.append(make_move_iterator(data.begin()), make_move_iterator(data.end()));
    body += "\r\n";
  }

  auto TritonClient::close() -> void
  {
    disconnect();
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
