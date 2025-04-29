#pragma once

extern "C" {
#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
}

#include "AVPacketManager.hpp"
#include "FramePool.hpp"
#include <functional>

class DecoderPipeline 
{
public:
  DecoderPipeline(const char* filename);
  ~DecoderPipeline();

  void decode_all(const std::function<void(AVFrame*)>& callback);

private:
  AVFormatContext* _fmt_ctx{nullptr};
  AVCodecContext* _dec_ctx{nullptr};
  int _video_stream_index{-1};
  AVPacketManager _packet;
  FramePool _frame_pool;
};
