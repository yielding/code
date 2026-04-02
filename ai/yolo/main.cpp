#include "yolo_detector.hpp"
#include "video_processor.hpp"

#include <print>
#include <string>
#include <sstream>
#include <unordered_set>
#include <filesystem>

using namespace std;
namespace fs = std::filesystem;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
auto is_video(string const& path) -> bool
{
  static unordered_set<string> const exts = {
    ".mp4", ".avi", ".mov", ".mkv", ".wmv", ".flv", ".webm", ".m4v", ".ts"
  };

  auto ext = fs::path(path).extension().string();
  for (auto& c : ext) c = tolower(c);
  return exts.contains(ext);
}

auto print_usage(char const* prog) -> void
{
  println("Usage: {} <image|video> <model.onnx> [options]", prog);
  println("");
  println("Options:");
  println("  --classes <c1,c2,...>  Filter by class names (e.g., person,car)");
  println("  --conf <0.0-1.0>      Confidence threshold (default: 0.5)");
  println("  --output <path>       Output directory or image path (default: output/)");
  println("  --json <path>         Save metadata as JSON (image mode only)");
  println("");
  println("Examples:");
  println("  {} photo.jpg yolov8n.onnx --classes person,car --conf 0.3", prog);
  println("  {} video.mp4 yolov8n.onnx --output results/", prog);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
auto main(int argc, char* argv[]) -> int
{
  if (argc < 3)
  {
    print_usage(argv[0]);
    return 1;
  }

  string input_path  = argv[1];
  string model_path  = argv[2];
  string output_path;
  string json_path;
  float  conf_threshold = 0.5f;
  unordered_set<string> target_classes;

  for (int i = 3; i < argc; i++)
  {
    string arg = argv[i];

    if (arg == "--classes" && i + 1 < argc)
    {
      auto classes_str = string(argv[++i]);
      istringstream ss(classes_str);
      string token;
      while (getline(ss, token, ','))
        target_classes.insert(token);
    }
    else if (arg == "--conf" && i + 1 < argc)
    {
      conf_threshold = stof(argv[++i]);
    }
    else if (arg == "--output" && i + 1 < argc)
    {
      output_path = argv[++i];
    }
    else if (arg == "--json" && i + 1 < argc)
    {
      json_path = argv[++i];
    }
  }

  println("Loading model: {}", model_path);
  yolo::YOLODetector detector(model_path);

  if (!target_classes.empty())
  {
    print("Filtering classes:");
    for (auto const& c : target_classes) print(" [{}]", c);
    println("");
  }

  if (is_video(input_path))
  {
    // Video mode
    auto output_dir = output_path.empty() ? "output" : output_path;
    println("Mode: video");

    yolo::VideoProcessor vp(input_path, output_dir);
    if (!vp.process(detector, target_classes, conf_threshold))
      return 1;
  }
  else
  {
    // Image mode
    auto image = cv::imread(input_path);
    if (image.empty())
    {
      println(stderr, "Error: Cannot load image '{}'", input_path);
      return 1;
    }

    println("Mode: image ({}x{})", image.cols, image.rows);
    println("Running inference (conf >= {:.0f}%)...", conf_threshold * 100);

    auto detections = detector.detect(image, target_classes, conf_threshold);
    yolo::print_metadata(detections);

    // Determine output path
    if (output_path.empty())
    {
      auto dir = fs::path("output");
      fs::create_directories(dir);
      auto stem = fs::path(input_path).stem().string();
      auto ext  = fs::path(input_path).extension().string();
      output_path = (dir / (stem + "_detected" + ext)).string();
    }

    yolo::draw_detections(image, detections);
    cv::imwrite(output_path, image);
    println("Output image saved to: {}", output_path);

    if (!json_path.empty())
      yolo::save_metadata_json(json_path, detections);
  }

  return 0;
}
