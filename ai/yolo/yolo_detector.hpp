#pragma once

#include "detection.hpp"

#include <opencv2/dnn.hpp>

#include <string>
#include <vector>
#include <unordered_set>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace yolo
{
  using namespace std;

  class YOLODetector
  {
  public:
    YOLODetector(string const& model_path)
      : _net(cv::dnn::readNetFromONNX(model_path))
    {
      _net.setPreferableBackend(cv::dnn::DNN_BACKEND_OPENCV);
      _net.setPreferableTarget(cv::dnn::DNN_TARGET_CPU);
    }

  public:
    auto detect(cv::Mat const& image,
                unordered_set<string> const& target_classes,
                const float conf_threshold = 0.5f,
                const float nms_threshold  = 0.45f) -> vector<Detection>
    {
      auto const input_size = 640;
      cv::Mat blob;
      cv::dnn::blobFromImage(image, blob, 1.0/255.0,
                             cv::Size(input_size, input_size),
                             cv::Scalar(), true, false);

      _net.setInput(blob);

      vector<cv::Mat> outputs;
      _net.forward(outputs, _net.getUnconnectedOutLayersNames());

      return postprocess(image, outputs, target_classes,
                         conf_threshold, nms_threshold, input_size);
    }

  private:
    auto postprocess(cv::Mat const& image,
                     vector<cv::Mat> const& outputs,
                     unordered_set<string> const& target_classes,
                     const float conf_threshold,
                     const float nms_threshold,
                     const int input_size) -> vector<Detection>
    {
      auto const& class_names = coco_class_names();
      auto const num_classes = (int)class_names.size();

      // YOLOv8 output shape: [1, 84, 8400] -> transpose to [8400, 84]
      cv::Mat output = outputs[0];
      auto const rows = output.size[2];
      auto const dims = output.size[1];

      output = output.reshape(1, dims);
      cv::transpose(output, output);

      auto const x_scale = (float)image.cols / input_size;
      auto const y_scale = (float)image.rows / input_size;

      vector<int>       class_ids;
      vector<float>     confidences;
      vector<cv::Rect>  boxes;

      for (int i = 0; i < rows; i++)
      {
        auto const* row = output.row(i).ptr<float>();

        cv::Mat scores(1, num_classes, CV_32F, (void*)(row + 4));
        cv::Point max_loc;
        double max_score;
        cv::minMaxLoc(scores, nullptr, &max_score, nullptr, &max_loc);

        if (max_score < conf_threshold)
          continue;

        auto const class_id = max_loc.x;
        auto const& name = class_names[class_id];

        if (!target_classes.empty() && !target_classes.contains(name))
          continue;

        auto const cx = row[0] * x_scale;
        auto const cy = row[1] * y_scale;
        auto const w  = row[2] * x_scale;
        auto const h  = row[3] * y_scale;

        auto const x = (int)(cx - w / 2);
        auto const y = (int)(cy - h / 2);

        boxes.emplace_back(x, y, (int)w, (int)h);
        confidences.push_back((float)max_score);
        class_ids.push_back(class_id);
      }

      vector<int> indices;
      cv::dnn::NMSBoxes(boxes, confidences, conf_threshold, nms_threshold, indices);

      vector<Detection> detections;
      for (auto const idx : indices)
      {
        detections.push_back({
          .class_id   = class_ids[idx],
          .class_name = class_names[class_ids[idx]],
          .confidence = confidences[idx],
          .box        = boxes[idx]
        });
      }

      return detections;
    }

  private:
    cv::dnn::Net _net;
  };
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
