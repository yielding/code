#pragma once

#include <opencv2/opencv.hpp>

#include <print>
#include <format>
#include <string>
#include <vector>
#include <fstream>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace yolo
{
  using namespace std;

  auto coco_class_names() -> vector<string> const&
  {
    static vector<string> const names = {
      "person", "bicycle", "car", "motorcycle", "airplane", "bus", "train",
      "truck", "boat", "traffic light", "fire hydrant", "stop sign",
      "parking meter", "bench", "bird", "cat", "dog", "horse", "sheep",
      "cow", "elephant", "bear", "zebra", "giraffe", "backpack", "umbrella",
      "handbag", "tie", "suitcase", "frisbee", "skis", "snowboard",
      "sports ball", "kite", "baseball bat", "baseball glove", "skateboard",
      "surfboard", "tennis racket", "bottle", "wine glass", "cup", "fork",
      "knife", "spoon", "bowl", "banana", "apple", "sandwich", "orange",
      "broccoli", "carrot", "hot dog", "pizza", "donut", "cake", "chair",
      "couch", "potted plant", "bed", "dining table", "toilet", "tv",
      "laptop", "mouse", "remote", "keyboard", "cell phone", "microwave",
      "oven", "toaster", "sink", "refrigerator", "book", "clock", "vase",
      "scissors", "teddy bear", "hair drier", "toothbrush"
    };

    return names;
  }

  struct Detection
  {
    int class_id;
    string class_name;
    float confidence;
    cv::Rect box;
  };

  auto get_color(const int class_id) -> cv::Scalar
  {
    static vector<cv::Scalar> const colors = {
      {255,  56,  56}, { 56, 255,  56}, { 56,  56, 255}, {255, 157, 151},
      {255, 112, 112}, {255, 178, 102}, {230, 230,  50}, {128, 255, 128},
      {102, 255, 255}, {102, 178, 255}, {153, 102, 255}, {255, 102, 178},
      {255, 102, 102}, {102, 255, 178}, {255, 230,  77}, { 77, 200, 255},
    };

    return colors[class_id % colors.size()];
  }

  auto draw_detections(cv::Mat& image, vector<Detection> const& detections) -> void
  {
    for (auto const& det : detections)
    {
      auto const color = get_color(det.class_id);

      cv::rectangle(image, det.box, color, 2);

      auto label = std::format("{} {:.1f}%", det.class_name, det.confidence * 100);

      int baseline = 0;
      auto const label_size = cv::getTextSize(label, cv::FONT_HERSHEY_SIMPLEX, 0.6, 1, &baseline);

      auto const top = max(det.box.y, label_size.height);
      cv::rectangle(image,
                    cv::Point(det.box.x, top - label_size.height - 6),
                    cv::Point(det.box.x + label_size.width, top),
                    color, cv::FILLED);

      cv::putText(image, label,
                  cv::Point(det.box.x, top - 3),
                  cv::FONT_HERSHEY_SIMPLEX, 0.6, cv::Scalar(255, 255, 255), 1);
    }
  }

  auto print_metadata(vector<Detection> const& detections) -> void
  {
    println("+---------------------------------------------------------+");
    println("|  Detection Results                                      |");
    println("+------+--------------+--------+-------------------------+");
    println("|  #   | Class        | Conf.  | Bounding Box (x,y,w,h)  |");
    println("+------+--------------+--------+-------------------------+");

    for (int i = 0; auto const& det : detections)
    {
      println("| {:>4} | {:<12} | {:5.1f}% | ({:>4},{:>4},{:>4},{:>4})   |",
              ++i, det.class_name, det.confidence * 100,
              det.box.x, det.box.y, det.box.width, det.box.height);
    }

    println("+------+--------------+--------+-------------------------+");
    println("Total: {} object(s) detected", detections.size());
  }

  auto save_metadata_json(string const& path, vector<Detection> const& detections) -> void
  {
    ofstream ofs(path);
    ofs << "{\n  \"detections\": [\n";

    for (size_t i = 0; i < detections.size(); i++)
    {
      auto const& d = detections[i];
      ofs << "    {\n"
          << "      \"class_id\": "     << d.class_id     << ",\n"
          << "      \"class_name\": \"" << d.class_name   << "\",\n"
          << "      \"confidence\": "   << d.confidence   << ",\n"
          << "      \"box\": {"
          << " \"x\": "      << d.box.x
          << ", \"y\": "     << d.box.y
          << ", \"width\": " << d.box.width
          << ", \"height\": "<< d.box.height
          << " }\n    }";

      if (i + 1 < detections.size()) ofs << ",";
      ofs << "\n";
    }

    ofs << "  ]\n}\n";
    println("Metadata saved to: {}", path);
  }
}
