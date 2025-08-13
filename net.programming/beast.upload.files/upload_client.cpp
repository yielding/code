#include "upload_client.hpp"

#include <boost/beast/version.hpp>
#include <boost/asio/connect.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <nlohmann/json.hpp>

#include <print>
#include <fstream>
#include <filesystem>
#include <sstream>
#include <algorithm>
#include <stdexcept>
#include <regex>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace net
{
  using json = nlohmann::json;

  MultipartUploadClient::MultipartUploadClient(const string host, const string port)
    : _host{host}
    , _port{port}
    , _resolver{_ioc}
    , _stream{_ioc}
    , _connected{false}
  {}

  MultipartUploadClient::~MultipartUploadClient()
  {
    close();
  }

  auto MultipartUploadClient::upload_with_json_and_images(
    const string analyzer_type,
    const string detect_model,
    const string ocr_model,
    const vector<string>& image_paths,
    const string endpoint,
    ProgressCallback progress_callback) -> Response
  {
    Response response{0, "", false};
    
    try 
    {
      // Validate endpoint format
      if (!validate_endpoint(endpoint))
        throw invalid_argument("Invalid endpoint format. Must start with '/'.");

      // Connect or reuse existing connection
      reconnect_if_needed();
      
      // Set timeout for this operation
      _stream.expires_after(DEFAULT_TIMEOUT);

      auto boundary = generate_boundary();
      
      auto model_types_json = json{
        {{"detect", detect_model}, {"ocr", ocr_model}}
      }.dump();
      
      // Build multipart body
      string body;
      
      // Add form fields
      add_form_field(body, boundary, "analyzerType", analyzer_type);
      add_form_field_json(body, boundary, "modelTypes", model_types_json);

      // Add image files
      for (const auto& image_path : image_paths) 
      {
        auto image_data_opt = read_file(image_path);
        if (!image_data_opt.has_value()) 
        {
          println(stderr, "Failed to read image: {}", image_path);
          continue;
        }

        auto filename = filesystem::path(image_path).filename().string();
        add_form_file(body, boundary, "sources", filename, std::move(image_data_opt.value()));
      }

      body += "--" + boundary + "--\r\n";

      http::request<http::string_body> req{http::verb::post, endpoint, 11};
      req.set(http::field::host, _host);
      req.set(http::field::user_agent, BOOST_BEAST_VERSION_STRING);
      req.set(http::field::content_type, "multipart/form-data; boundary=" + boundary);
      req.body() = body;
      req.prepare_payload();

      // Send request with progress tracking
      if (progress_callback)
      {
        auto total_bytes = req.body().size();
        
        // Write request with progress updates
        http::write(_stream, req);
        progress_callback(total_bytes, total_bytes); // Final 100% callback
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
      response.success = (res.result() == http::status::ok || 
                         res.result() == http::status::created);
      
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

  auto MultipartUploadClient::generate_boundary() -> string
  {
    auto uuid = boost::uuids::random_generator{}();
    return "----WebKitFormBoundary" + boost::uuids::to_string(uuid).substr(0, 16);
  }

  auto MultipartUploadClient::get_mime_type(const string filename) -> string
  {
    auto ext = filesystem::path(filename).extension().string();
    transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
    
    if (ext == ".jpg" || ext == ".jpeg") return "image/jpeg";
    if (ext == ".png") return "image/png";
    if (ext == ".gif") return "image/gif";
    if (ext == ".bmp") return "image/bmp";
    if (ext == ".webp") return "image/webp";
    
    return "application/octet-stream";
  }

  auto MultipartUploadClient::read_file(const string file_path) -> optional<vector<unsigned char>>
  {
    namespace fs = filesystem;
    
    if (!fs::exists(file_path)) 
    {
      println(stderr, "File not found: {}", file_path);
      return nullopt;
    }

    // Check file size
    auto file_size = fs::file_size(file_path);
    if (file_size > MAX_FILE_SIZE)
    {
      println(stderr, "File too large: {} bytes (max: {} bytes)", file_size, MAX_FILE_SIZE);
      return nullopt;
    }

    ifstream file{file_path, ios::binary};
    if (!file) 
    {
      println(stderr, "Cannot open file: {}", file_path);
      return nullopt;
    }

    // Read file
    vector<unsigned char> buffer(file_size);
    file.read(reinterpret_cast<char*>(buffer.data()), file_size);
    
    if (!file)
    {
      println(stderr, "Failed to read complete file: {}", file_path);
      return nullopt;
    }
    
    return buffer;
  }

  auto MultipartUploadClient::add_form_field(string& body, const string& boundary, const string& name, const string& value) -> void
  {
    body += "--" + boundary + "\r\n";
    body += "Content-Disposition: form-data; name=\"" + name + "\"\r\n\r\n";
    body += value + "\r\n";
  }

  auto MultipartUploadClient::add_form_field_json(string& body, const string& boundary, const string& name, const string& json_value) -> void
  {
    body += "--" + boundary + "\r\n";
    body += "Content-Disposition: form-data; name=\"" + name + "\"\r\n";
    body += "Content-Type: application/json\r\n\r\n";
    body += json_value + "\r\n";
  }

  auto MultipartUploadClient::add_form_file(string& body, const string& boundary, const string& name, const string& filename, vector<unsigned char>&& data) -> void
  {
    body += "--" + boundary + "\r\n";
    body += "Content-Disposition: form-data; name=\"" + name + "\"; filename=\"" + filename + "\"\r\n";
    body += "Content-Type: " + get_mime_type(filename) + "\r\n\r\n";
    body.append(std::make_move_iterator(data.begin()), std::make_move_iterator(data.end()));
    body += "\r\n";
  }

  auto MultipartUploadClient::close() -> void
  {
    disconnect();
  }

  auto MultipartUploadClient::is_connected() const -> bool
  {
    return _connected && _stream.socket().is_open();
  }

  auto MultipartUploadClient::reconnect_if_needed() -> void
  {
    if (!is_connected())
    {
      // Resolve if we haven't already
      if (_endpoints.empty())
        _endpoints = _resolver.resolve(_host, _port);
      
      // Connect to the resolved endpoints
      _stream.connect(_endpoints);
      _connected = true;
    }
  }

  auto MultipartUploadClient::disconnect() -> void
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

  auto MultipartUploadClient::validate_endpoint(const string endpoint) -> bool
  {
    // Endpoint must start with '/' and contain only valid URI characters
    static const regex endpoint_regex("^/[a-zA-Z0-9/_.-]*$");
    return regex_match(endpoint, endpoint_regex);
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////