#pragma once

#include "ffmpeg.hpp"

#include <string>
#include <vector>

class FrameExporter 
{
public:
  FrameExporter(int width, int height, AVPixelFormat pix_fmt);
  ~FrameExporter();

  bool encode_to_file(AVFrame* frame, const std::string& filename);
  std::vector<uint8_t> encode_to_memory(AVFrame* frame);

private:
  AVCodecContext* _ctx{nullptr};
};