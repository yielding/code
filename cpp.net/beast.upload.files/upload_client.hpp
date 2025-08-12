#pragma once

#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/asio/ip/tcp.hpp>

#include <string>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
//
// Multipart form data upload client using Boost.Beast
//
////////////////////////////////////////////////////////////////////////////////
namespace net
{
  using namespace std;
  namespace beast = boost::beast;
  namespace http = beast::http;
  namespace asio = boost::asio;
  using tcp = asio::ip::tcp;

  class MultipartUploadClient
  {
  public:
    MultipartUploadClient(const string host, const string port);
    ~MultipartUploadClient();

  public:
    auto upload_with_json_and_images(
      const string analyzer_type,
      const string detect_model,
      const string ocr_model,
      const vector<string>& image_paths,
      const string endpoint = "/infer") -> bool;

  private:
    auto generate_boundary() -> string;
    auto get_mime_type(const string filename) -> string;
    auto read_file(const string file_path) -> vector<unsigned char>;
    auto add_form_field(string& body, const string& boundary, const string& name, const string& value) -> void;
    auto add_form_field_json(string& body, const string& boundary, const string& name, const string& json_value) -> void;
    auto add_form_file(string& body, const string& boundary, const string& name, const string& filename, const vector<unsigned char>& data) -> void;
    auto close() -> void;

  private:
    string _host;
    string _port;
    asio::io_context _ioc;
    tcp::resolver _resolver;
    beast::tcp_stream _stream;
  };
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////