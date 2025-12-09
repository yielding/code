#include "triton_client.hpp"

#include <nlohmann/json.hpp>

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <print>
#include <string>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using namespace std;
using json = nlohmann::json;
namespace fs = std::filesystem;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
constexpr char BASE64_CHARS[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

auto base64_decode(const string& encoded) -> vector<uint8_t>
{
  vector<int> lookup(256, -1);
  for (int i = 0; i < 64; ++i)
    lookup[BASE64_CHARS[i]] = i;

  vector<uint8_t> result;
  result.reserve(encoded.size() * 3 / 4);

  int val = 0;
  int bits = -8;

  for (auto c : encoded)
  {
    if (lookup[c] == -1)
      continue;

    val = (val << 6) + lookup[c];
    bits += 6;

    if (bits >= 0)
    {
      result.push_back((val >> bits) & 0xFF);
      bits -= 8;
    }
  }

  return result;
}

auto save_binary(const string& path, const vector<uint8_t>& data) -> void
{
  ofstream file(path, ios::binary);
  if (!file)
    throw runtime_error("Cannot create file: " + path);

  file.write(reinterpret_cast<const char*>(data.data()), data.size());
}

auto get_basename(const string& path) -> string
{
  return fs::path(path).stem().string();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
auto save_aligned_faces(const string& json_response,
                        const vector<string>& image_paths) -> void
{
  auto root = json::parse(json_response);

  for (size_t i = 0; i < root.size() && i < image_paths.size(); ++i)
  {
    auto& item = root[i];

    if (item["status"] != "success")
      continue;

    auto& result = item["result"];
    auto& aligned_faces = result["aligned_faces"];
    auto basename = get_basename(image_paths[i]);

    for (size_t j = 0; j < aligned_faces.size(); ++j)
    {
      auto aligned_b64 = aligned_faces[j].get<string>();
      auto decoded = base64_decode(aligned_b64);

      auto output_name = format("aligned_{}_{}.png", basename, j);
      save_binary(output_name, decoded);
      println("Saved: {}", output_name);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(const int argc, char* argv[])
{
  if (argc < 2)
  {
    println("Usage: {} <image1> [image2] ...", argv[0]);
    println("Example: {} face1.png face2.jpg", argv[0]);
    return 1;
  }

  try
  {
    vector<string> images;
    for (int i = 1; i < argc; ++i)
      images.push_back(argv[i]);

    net::TritonClient client("172.16.253.34", "8004");

    auto face_models = json {
      {"detect", "Retina640"},
      {"landmark", "CoordReg"},
      {"feature", "ArcFace"}
    };

    auto result = client.infer("Face", face_models, images);
    if (!result)
    {
      println(stderr, "Error: {}", result.error());
      return 1;
    }

    println("Response:\n{}", result->body);

    save_aligned_faces(result->body, images);
  }
  catch (const exception& e)
  {
    println(stderr, "Error: {}", e.what());
    return 1;
  }

  return 0;
}
