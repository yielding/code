#pragma once

#include "ffmpeg.hpp"
#include "uniq.codec.context.hpp"

////////////////////////////////////////////////////////////////////////////////
///
/// 미디어의 종류와 관계없이 정확하게 decoding 하는 객체
/// 사용자가 선택한 코덱의 종류에 따라서 container 내의 정확한 stream을 찾아 codec param
/// 을 얻어 codec context를 갱신하는 요구사항을 잘 반영해야 한다.
///
/// 즉, 1) codec id -> codec -> codec context
///     2) stream index query from container -> stream -> codec param from stream
///       -> update codec context
///
////////////////////////////////////////////////////////////////////////////////
namespace av {

  enum class DecoderState
  {
    Ready,          // packet이 있으면 decode, 없으면 flush 시작
    Draining,       // 남은 프레임을 처리하는 단계
    Finished        // 더 이상 디코딩이 없는 상태(eof)
  };
  
  class Decoder 
  {
  public:
    explicit Decoder(const AVCodec* codec, const AVCodecParameters* params=nullptr)
      : _codec_ctx{codec, params}, _state{DecoderState::Ready}
    {}

    [[nodiscard]]
    auto ok() const { return _codec_ctx.ok(); }

    auto open(const AVCodec* codec, AVDictionary** options = nullptr) const -> expected<void, string>
    {
      return _codec_ctx.open(codec, options);
    }

    // REMARK:
    //  - 이 함수는 두 개의 상황에서 사용됨
    //  - pkt이 nullptr인 경우는 flush mode를 의미함
    //  - pkt이 nullptr이 아닌 경우는 패킷을 디코딩하는 것
    auto decode_next_frame(const AVPacket* pkt, AVFrame* frame) -> expected<DecodeResult, string>
    {
      if (_state == DecoderState::Finished)
        return DecodeResult::EndOfStream;
  
      if (pkt != nullptr) // decoding case
      {
        if (auto res = _codec_ctx.send_packet(pkt); !res)
          return unexpected(res.error());
      } 
      else if (_state == DecoderState::Ready) // pkt이 nullptr인 경우는 flush mode를 의미함
      {
        // 마지막 패킷 보냈음: flush 시작
        if (auto res = _codec_ctx.flush(); !res)
          return unexpected(res.error());
  
        _state = DecoderState::Draining;
      }
  
      auto res = _codec_ctx.receive_frame(frame);
      if (!res)
        return unexpected(res.error());
  
      switch (res.value()) 
      {
      case DecodeResult::FrameAvailable: 
          return DecodeResult::FrameAvailable;
      case DecodeResult::NeedMoreData:
        if (_state == DecoderState::Draining)
        {
          _state = DecoderState::Finished;
          return DecodeResult::EndOfStream;
        }
        return DecodeResult::NeedMoreData;
      case DecodeResult::EndOfStream:
        _state = DecoderState::Finished;
        return DecodeResult::EndOfStream;
      }

      return unexpected("Unknown decode result"s);
    }
  
    [[nodiscard]]
    auto eof() const noexcept { return _state == DecoderState::Finished; }
  
  private:
    UniqueCodecContext _codec_ctx;
    DecoderState _state;
  };

}
////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////