#pragma once

#include "ffmpeg.hpp"

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av::detail {

  struct AVCodecContextDeleter
  {
    void operator()(AVCodecContext* ctx) const noexcept 
    {
      if (ctx)
        avcodec_free_context(&ctx);
    }
  };

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av {

  using namespace std;

  enum class DecodeResult 
  {
    FrameAvailable, // 프레임 디코딩 성공
    NeedMoreData,   // EAGAIN
    EndOfStream     // EOF
  };

  class UniqueCodecContext 
  {
  public:
    explicit UniqueCodecContext(const AVCodec* codec) 
      : _ctx(avcodec_alloc_context3(codec)) 
    {
      if (!_ctx)
        throw std::runtime_error("Failed to allocate AVCodecContext");
    }
  
    ~UniqueCodecContext() = default;
  
    UniqueCodecContext(const UniqueCodecContext&) = delete;
    UniqueCodecContext& operator=(const UniqueCodecContext&) = delete;
  
    UniqueCodecContext(UniqueCodecContext&&) noexcept = default;
    UniqueCodecContext& operator=(UniqueCodecContext&&) noexcept = default;
  
    auto get() const noexcept { return _ctx.get(); }
  
    auto open(const AVCodec* codec, AVDictionary** options=nullptr)
      -> expected<void, string>
    {
      int ret = avcodec_open2(_ctx.get(), codec, options);
      if (ret < 0)
        return unexpected<string>(detail::ffmpeg_error_string(ret));

      return {};
    }

    // REMARK!
    // 복사 필요한가?
    // 아래는 좀 더 의문이 있긴 하다
    auto copy_from(const AVCodecContext* src) -> expected<void, string>
    {
      if (!src)
        return unexpected<string>("Source context is null"s);

      _ctx->bit_rate = src->bit_rate;
      _ctx->width  = src->width;
      _ctx->height = src->height;
      _ctx->sample_aspect_ratio = src->sample_aspect_ratio;
      _ctx->pix_fmt = src->pix_fmt;
      _ctx->sample_rate = src->sample_rate;
      _ctx->codec_type = src->codec_type;
      _ctx->codec_id = src->codec_id;

      return {};
    }
      
    auto send_packet(const AVPacket* pkt) -> expected<void, string> 
    {
      int ret = avcodec_send_packet(_ctx.get(), pkt);
      if (ret < 0)
          return unexpected<string>(detail::ffmpeg_error_string(ret));

      return {};
    }

    // 프레임 받기
    auto receive_frame(AVFrame* frame) -> expected<bool, string> 
    {
      int ret = avcodec_receive_frame(_ctx.get(), frame);
      if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF)
          return false; // 프레임 없음, 정상 상황
      
      if (ret < 0)
          return unexpected<string>(detail::ffmpeg_error_string(ret));
      
      return true;
    }

    // flush (디코딩 종료 후 남은 프레임들 출력)
    // 반복적으로 호출해서 AVERROR_EOF 나올 때까지 frame 받기
    auto flush(AVFrame* frame) -> expected<void, string>
    {
      int ret = avcodec_send_packet(_ctx.get(), nullptr);
      if (ret < 0)
        return unexpected<string>(detail::ffmpeg_error_string(ret));

      return {};
    }

    // 디코딩: Packet → Frame
    // 호출 측에서 frame을 준비해 넘기고, 리턴값은 AVERROR(EAGAIN), AVERROR_EOF, 0
    auto decode(const AVPacket* pkt, AVFrame* frame) -> expected<DecodeResult, string>
    {
      int ret = avcodec_send_packet(_ctx.get(), pkt);
      if (ret < 0)
        return unexpected<string>(detail::ffmpeg_error_string(ret));

      ret = avcodec_receive_frame(_ctx.get(), frame);
      if (ret == 0) 
        return DecodeResult::FrameAvailable;

      if (ret == AVERROR(EAGAIN)) 
        return DecodeResult::NeedMoreData;

      if (ret == AVERROR_EOF) 
        return DecodeResult::EndOfStream;

      return unexpected<string>(detail::ffmpeg_error_string(ret));
    }
  
  private:
    unique_ptr<AVCodecContext, detail::AVCodecContextDeleter> _ctx;
  };

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////