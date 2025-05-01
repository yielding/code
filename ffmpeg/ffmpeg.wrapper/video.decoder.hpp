#pragma once

#include "ffmpeg.hpp"
#include "uniq.format.context.hpp"
#include "decoder.hpp"
#include "av.resource.hpp"

////////////////////////////////////////////////////////////////////////////////
///
/// 비디오 decoder가 frame pumping을 하기 위해서는 포맷 컨텍스트를 가지고 있어야 한다.
///
////////////////////////////////////////////////////////////////////////////////
namespace av {

  using namespace std;

  class VideoDecoder 
  {
  public:
    VideoDecoder()
      : _stream_index{-1}, _codec_id{AV_CODEC_ID_NONE}, _fmt_ctx(nullptr)
    {
    }

    auto open_with(const UniqueFormatContext& container) -> expected<void, string>
    {
      if (container.get() == nullptr)
        return unexpected("container is not ready"s);

      const AVCodec* codec = nullptr;
      _fmt_ctx = container.get();
      const int index = av_find_best_stream(_fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, &codec, 0);
      if (index < 0)
        return unexpected("Failed to find a video stream in the input"s);

      _stream_index = index;
      _decoder = make_unique<Decoder>(codec, _fmt_ctx->streams[_stream_index]->codecpar);
      if (!_decoder->ok())
        return unexpected("Failed to create decoder"s);

      return _decoder->open(codec);
    }

    auto open(const AVCodec* codec, AVDictionary** options = nullptr) const -> expected<void, string>
    {
      return _decoder->open(codec, options);
    }
  
    auto decode_next(const UniquePacket& packet, UniqueFrame& frame) const -> expected<DecodeResult, string>
    {
      return _decoder->decode_next_frame(packet.get(), frame.get());
    }
  
    auto decode_next(const PacketRef& packet, UniqueFrame& frame) const -> expected<DecodeResult, string>
    {
      return _decoder->decode_next_frame(packet.get(), frame.get());
    }
  
    auto flush_next(AVFrame* frame) const -> expected<DecodeResult, string>
    {
      return _decoder->decode_next_frame(nullptr, frame);
    }
  
    [[nodiscard]]
    auto eof() const noexcept { return _decoder->eof(); }

    void handle_decoded_frame(AVFrame* frame)
    {
      std::cout << "Decoded frame: pts = " << frame->pts << std::endl;
    }

    void decode_loop()
    {
      UniquePacket packet;
      UniqueFrame frame;

      while (av_read_frame(_fmt_ctx, packet.get()) >= 0)
      {
        if (packet.get()->stream_index != _stream_index)
        {
          packet.reset();
          continue;
        }

        auto res = decode_next(packet, frame);
        if (res && res.value() == DecodeResult::FrameAvailable)
          handle_decoded_frame(frame.get());

        packet.reset();
        frame.reset();
      }

      while (!_decoder->eof())
      {
        auto res = flush_next(frame.get());
        if (res && res.value() == DecodeResult::FrameAvailable)
          handle_decoded_frame(frame.get());

        frame.reset();
      }
    }

  private:
    int _stream_index;
    AVCodecID _codec_id;
    AVFormatContext* _fmt_ctx;
    unique_ptr<Decoder> _decoder;
  };

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
