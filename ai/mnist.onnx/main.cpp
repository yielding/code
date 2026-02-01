#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#include <onnxruntime_cxx_api.h>

#include <algorithm>
#include <expected>
#include <print>
#include <random>
#include <span>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using namespace std;

auto load_image(const char* path) -> expected<vector<float>, string>
{
  vector<float> result(28 * 28);

  int w, h, ch;
  auto* img = stbi_load(path, &w, &h, &ch, 1);  // force grayscale
  if (!img)
    return unexpected{format("Failed to load image: {}", path)};

  if (w != 28 || h != 28)
  {
    stbi_image_free(img);
    return unexpected{format("Image size {}x{}, expected 28x28", w, h)};
  }

  for (int i = 0; i < 28 * 28; i++)
    result[i] = static_cast<float>(img[i]) / 255.0f;

  stbi_image_free(img);
  return result;;
}

auto load_or_generate(string const& fname) -> expected<vector<float>, string>
{
  if (fname.empty())
  {
    mt19937 gen(0);
    uniform_real_distribution<float> dist(0.f, 1.f);
    vector<float> result(28 * 28);
    for (auto& v : result) v = dist(gen);

    return result;
  }

  return load_image(fname.c_str());
}

auto main(const int argc, const char* argv[]) -> int
{
  using ranges::distance, ranges::max_element;

  Ort::Env env(ORT_LOGGING_LEVEL_WARNING, "mnist");
  Ort::SessionOptions session_options;
  session_options.SetIntraOpNumThreads(1);

  Ort::Session session(env, "mnist-8.onnx", session_options);

  Ort::AllocatorWithDefaultOptions allocator;
  auto in_name  = session.GetInputNameAllocated(0, allocator);
  auto out_name = session.GetOutputNameAllocated(0, allocator);
  const char* in_names[]  = { in_name.get() };
  const char* out_names[] = { out_name.get() };

  vector<int64_t> in_shape = { 1, 1, 28, 28};

  auto in_tensor_vals = *load_or_generate(argc > 1 ? argv[1] : ""s);

  auto mem_info   = Ort::MemoryInfo::CreateCpu(OrtArenaAllocator, OrtMemTypeDefault);
  auto in_tensor  = Ort::Value::CreateTensor<float>(mem_info, in_tensor_vals.data(), in_tensor_vals.size(), in_shape.data(), in_shape.size());
  auto out_tensor = session.Run(Ort::RunOptions{nullptr}, in_names, &in_tensor, 1, out_names, 1);

  auto output = out_tensor[0].GetTensorMutableData<float>();
  auto scores = span(output, 10);
  auto predicted = distance(scores.begin(), max_element(scores));

  println("Predicted digit: {}", predicted);

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
