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

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace net
{
  using json = nlohmann::json;

  MultipartUploadClient::MultipartUploadClient(const string host, const string port)
    : _host(host) , _port(port) , _resolver(_ioc) , _stream(_ioc)
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
    const string endpoint) -> bool
  {
    try 
    {
      auto const results = _resolver.resolve(_host, _port);
      _stream.connect(results);

      auto boundary = generate_boundary();
      
      auto model_types_json = json::array({
        {{"detect", detect_model}, 
        {"ocr", ocr_model}}
      }).dump();
      
      // Build multipart body
      string body;
      
      // Add form fields
      add_form_field(body, boundary, "analyzerType", analyzer_type);
      add_form_field_json(body, boundary, "modelTypes", model_types_json);

      // Add image files
      for (const auto& image_path : image_paths) 
      {
        auto image_data = read_file(image_path);
        if (image_data.empty()) 
        {
          println(stderr, "Failed to read image: {}", image_path);
          continue;
        }

        auto filename = filesystem::path(image_path).filename().string();
        add_form_file(body, boundary, "sources", filename, image_data);
      }

      body += "--" + boundary + "--\r\n";

      http::request<http::string_body> req{http::verb::post, endpoint, 11};
      req.set(http::field::host, _host);
      req.set(http::field::user_agent, BOOST_BEAST_VERSION_STRING);
      req.set(http::field::content_type, "multipart/form-data; boundary=" + boundary);
      req.body() = body;
      req.prepare_payload();

      http::write(_stream, req);

      beast::flat_buffer buffer;
      http::response<http::dynamic_body> res;
      http::read(_stream, buffer, res);

      println("Response Status: {}", res.result_int());
      println("Response Body: {}", beast::buffers_to_string(res.body().data()));

      return (res.result() == http::status::ok || 
              res.result() == http::status::created);
    }
    catch (exception const& e) 
    {
      println(stderr, "Error: {}", e.what());
      return false;
    }
  }

  auto MultipartUploadClient::generate_boundary() -> string
  {
    auto uuid = boost::uuids::random_generator()();
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

  auto MultipartUploadClient::read_file(const string file_path) -> vector<unsigned char>
  {
    namespace fs = filesystem;
    
    if (!fs::exists(file_path)) 
    {
      println(stderr, "File not found: {}", file_path);
      return {};
    }

    ifstream file(file_path, ios::binary);
    if (!file) 
    {
      println(stderr, "Cannot open file: {}", file_path);
      return {};
    }

    // 내가 만든 함수 사용하기 
    // Get file size
    file.seekg(0, ios::end);
    auto file_size = file.tellg();
    file.seekg(0, ios::beg);

    // Read file
    vector<unsigned char> buffer(file_size);
    file.read(reinterpret_cast<char*>(buffer.data()), file_size);
    
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

  auto MultipartUploadClient::add_form_file(string& body, const string& boundary, const string& name, const string& filename, const vector<unsigned char>& data) -> void
  {
    body += "--" + boundary + "\r\n";
    body += "Content-Disposition: form-data; name=\"" + name + "\"; filename=\"" + filename + "\"\r\n";
    body += "Content-Type: " + get_mime_type(filename) + "\r\n\r\n";
    body.append(data.begin(), data.end());
    body += "\r\n";
  }

  auto MultipartUploadClient::close() -> void
  {
    beast::error_code ec;
    _stream.socket().shutdown(tcp::socket::shutdown_both, ec);
    if (ec && ec != beast::errc::not_connected)
      println(stderr, "Shutdown error: {}", ec.message());
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////