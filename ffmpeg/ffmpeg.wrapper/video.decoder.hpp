#pragma once

#include "ffmpeg.hpp"
#include "uniq.format.context.hpp"
#include "decoder.hpp"
#include "av.resource.hpp"

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av {

  using namespace std;

  class VideoDecoder 
  {
  public:
    // REMARK
    // 아래 함수는 큰 의미가 없다. 사용전에 원하는 코덱이 설치되어 있는지를 확인하는 용도.
    // static이어도 되지
    // Video Decoder인 만큼, open_with가 더 중요하다.
    auto load(const AVCodecID id) -> expected<bool, string>
    {
      if (const auto codec = avcodec_find_decoder(id); codec != nullptr)
      {
        _codec_id = id;
        _codec = codec;
        return true;
      }

      return unexpected("codec not found"s);
    }

    auto open_with(const UniqueFormatContext& container) -> expected<void, string>
    {
      if (container.get() == nullptr)
        return unexpected("container is not ready"s);

      const auto fmt_ctx = container.get();
      const int index = av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, &_codec, 0);
      if (index < 0)
        return unexpected("Failed to find a video stream in the input"s);

      // TODO check index again
      // fmt_ctx->nb_streams = 1;

      _stream_index = index;
      _decoder = make_unique<Decoder>(_codec, fmt_ctx->streams[_stream_index]->codecpar);
      if (!_decoder->ok())
        return unexpected("Failed to create decoder"s);

      return _decoder->open(_codec);
    }

    auto open(const AVCodec* codec, AVDictionary** options = nullptr) const -> expected<void, string>
    {
      return _decoder->open(codec, options);
    }
  
    // 준비된 패킷(UniquePacket or PacketRef)으로 디코딩
    auto decode_next(const UniquePacket& packet, AVFrame* frame) const -> expected<DecodeResult, string>
    {
      return _decoder->decode_next_frame(packet.get(), frame);
    }
  
    auto decode_next(const PacketRef& packet, AVFrame* frame) const -> expected<DecodeResult, string>
    {
      return _decoder->decode_next_frame(packet.get(), frame);
    }
  
    auto flush_next(AVFrame* frame) const -> expected<DecodeResult, string>
    {
      return _decoder->decode_next_frame(nullptr, frame);
    }
  
    [[nodiscard]]
    auto eof() const noexcept { return _decoder->eof(); }
  
  private:
    AVCodecID _codec_id{AV_CODEC_ID_NONE};
    const AVCodec* _codec{nullptr};
    unique_ptr<Decoder> _decoder;
    int _stream_index{-1};
  };

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
