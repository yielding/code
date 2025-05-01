#pragma once

#include "ffmpeg.hpp"
#include "uniq.format.context.hpp"
#include "frame.queue.hpp"
#include "decoder.hpp"
#include "av.resource.hpp"

////////////////////////////////////////////////////////////////////////////////
///
/// 비디오 decoder가 frame pumping을 하기 위해서는 포맷 컨텍스트를 
/// 가지고 있어야 한다.
///
////////////////////////////////////////////////////////////////////////////////
namespace av {

  using namespace std;

  class VideoDecoder 
  {
  public:
    VideoDecoder(): _stream_index{-1}, _codec_id{AV_CODEC_ID_NONE}, _fmt_ctx{nullptr}
    {}

    auto open_with(AVFormatContext* fmt_ctx, int index) -> expected<void, string>
    {
      if (fmt_ctx == nullptr || index < 0 || index >= fmt_ctx->nb_streams)
        return unexpected("container is not ready"s);

      _fmt_ctx = fmt_ctx;
      _stream_index = index;

      // NOTE : codec, codec_param -> codec_context(in Decoder) -> Decoder
      const auto p = fmt_ctx->streams[index]->codecpar;
      const auto codec = avcodec_find_decoder(p->codec_id);
      _decoder = make_unique<Decoder>(codec, p);
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

    void decode_loop(const function<void(AVFrame*)>& callback) const
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

        if (auto res = decode_next(packet, frame); res && res.value() == DecodeResult::FrameAvailable)
          callback(frame.get());

        packet.reset();
        frame.reset();
      }

      while (!_decoder->eof())
      {
        if (auto res = flush_next(frame.get()); res && res.value() == DecodeResult::FrameAvailable)
          callback(frame.get());

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
