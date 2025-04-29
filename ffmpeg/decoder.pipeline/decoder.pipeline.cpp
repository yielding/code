#include "DecoderPipeline.hpp"
#include <stdexcept>

DecoderPipeline::DecoderPipeline(const char* filename) 
  : _frame_pool(8) 
{
  if (avformat_open_input(&_fmt_ctx, filename, nullptr, nullptr) < 0)
    throw std::runtime_error("Failed to open input file");

  if (avformat_find_stream_info(_fmt_ctx, nullptr) < 0)
    throw std::runtime_error("Failed to find stream info");

  const AVCodec* decoder;
  video_stream_index_ = av_find_best_stream(_fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, &decoder, 0);
  if (_video_stream_index < 0)
    throw std::runtime_error("No video stream found");

  _dec_ctx = avcodec_alloc_context3(decoder);
  avcodec_parameters_to_context(_dec_ctx, _fmt_ctx->streams[_video_stream_index]->codecpar);
  if (avcodec_open2(_dec_ctx, decoder, nullptr) < 0)
    throw std::runtime_error("Failed to open codec");
}

DecoderPipeline::~DecoderPipeline() 
{
  avcodec_free_context(&_dec_ctx);
  avformat_close_input(&_fmt_ctx);
}

void DecoderPipeline::decode_all(const std::function<void(AVFrame*)>& callback) 
{
  while (av_read_frame(_fmt_ctx, _packet.get()) >= 0)
  {
    if (_packet.get()->stream_index != video_stream_index_)
    {
      _packet.unref();
      continue;
    }

    if (avcodec_send_packet(_dec_ctx, _packet.get()) < 0) 
        break;

    auto frame = _frame_pool.acquire();
    while (avcodec_receive_frame(_dec_ctx, frame) >= 0)
    {
      callback(frame);
      _frame_pool.release(frame);
    }

    _packet.unref();
  }
}

