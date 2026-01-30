#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#include <onnxruntime_cxx_api.h>

#include <print>
#include <random>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using namespace std;

auto load_image(const char* path, vector<float>& data) -> bool
{
  int w, h, ch;
  auto* img = stbi_load(path, &w, &h, &ch, 1); // force grayscale
  if (!img)
  {
    println("Failed to load image: {}", path);
    return false;
  }

  if (w != 28 || h != 28)
  {
    println("Image size {}x{}, expected 28x28", w, h);
    stbi_image_free(img);
    return false;
  }

  data.resize(28 * 28);
  for (int i = 0; i < 28 * 28; i++)
    data[i] = static_cast<float>(img[i]) / 255.0f;

  stbi_image_free(img);
  return true;
}

auto main(const int argc, const char* argv[]) -> int
{
  Ort::Env env(ORT_LOGGING_LEVEL_WARNING, "mnist");
  Ort::SessionOptions session_options;
  session_options.SetIntraOpNumThreads(1);

  Ort::Session session(env, "mnist-8.onnx", session_options);

  Ort::AllocatorWithDefaultOptions allocator;

  auto input_name  = session.GetInputNameAllocated(0, allocator);
  auto output_name = session.GetOutputNameAllocated(0, allocator);
  const char* input_names[]  = { input_name.get() };
  const char* output_names[] = { output_name.get() };

  vector<int64_t> input_shape = {1, 1, 28, 28};
  vector<float> input_tensor_values(28 * 28);

  if (argc > 1)
  {
    if (!load_image(argv[1], input_tensor_values))
      return 1;

    println("Loaded image: {}", argv[1]);
  }
  else
  {
    mt19937 gen(0);
    uniform_real_distribution<float> dist(0.f, 1.f);
    for (auto& v : input_tensor_values)
      v = dist(gen);

    println("Using random input data");
  }

  auto memory_info = Ort::MemoryInfo::CreateCpu(OrtArenaAllocator, OrtMemTypeDefault);

  auto input_tensor = Ort::Value::CreateTensor<float>(memory_info, input_tensor_values.data(),
                                                            input_tensor_values.size(),
                                                            input_shape.data(), input_shape.size());
  auto output_tensors =
      session.Run(Ort::RunOptions{nullptr}, input_names, &input_tensor, 1, output_names, 1);

  auto output = output_tensors[0].GetTensorMutableData<float>();

  auto predicted = 0;
  auto max_prob = output[0];
  for (int i = 1; i < 10; i++)
  {
    if (output[i] > max_prob)
    {
      max_prob = output[i];
      predicted = i;
    }
  }

  println("Predicted digit: {}", predicted);

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
