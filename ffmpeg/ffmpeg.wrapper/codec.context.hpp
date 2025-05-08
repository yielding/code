#pragma once

#include <memory.hpp>
#include "ffmpeg.hpp"

#include <utility>

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

  class CodecContext : public core::memory::move_only<CodecContext>
  {
  public:
    explicit CodecContext(const AVCodec* codec, const AVCodecParameters* codec_params=nullptr)
    {
      const auto tmp_codec_ctx = avcodec_alloc_context3(codec);
      if (!tmp_codec_ctx)
        return;

      if (codec_params)
      {
        if (avcodec_parameters_to_context(tmp_codec_ctx, codec_params) < 0)
          return;
      }

      _ctx.reset(tmp_codec_ctx);
    }
  
    [[nodiscard]]
    auto ok() const noexcept { return _ctx ? true : false; }

    [[nodiscard]]
    auto get() const noexcept { return _ctx.get(); }

    auto open(const AVCodec* codec, AVDictionary** options=nullptr) const -> expected<void, string>
    {
      if (const int ret = avcodec_open2(_ctx.get(), codec, options); ret < 0)
        return unexpected(detail::ffmpeg_error_string(ret));

      return {};
    }

    // REMARK!
    // 복사 필요한가?
    // 아래는 좀 더 의문이 있긴 하다
    auto copy_from(const AVCodecContext* src) const -> expected<void, string>
    {
      // MD
      if (!src)
        return unexpected("Source context is null"s);

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

    auto send_packet(const AVPacket* pkt) const -> expected<void, string>
    {
      if (const int ret = avcodec_send_packet(_ctx.get(), pkt); ret < 0)
          return unexpected(detail::ffmpeg_error_string(ret));

      return {};
    }

    auto receive_frame(AVFrame* frame) const -> expected<DecodeResult, string>
    {
      const int ret = avcodec_receive_frame(_ctx.get(), frame);
      if (ret == 0)
        return expected<DecodeResult, string>(DecodeResult::FrameAvailable);

      if (ret == AVERROR(EAGAIN))
        return expected<DecodeResult, string>(DecodeResult::NeedMoreData);
      
      if (ret == AVERROR_EOF)
        return expected<DecodeResult, string>(DecodeResult::EndOfStream);
      
      return unexpected(detail::ffmpeg_error_string(ret));
    }

    // flush (디코딩 종료 후 남은 프레임들 출력)
    // 반복적으로 호출해서 AVERROR_EOF 나올 때까지 frame 받기
    [[nodiscard]]
    auto flush() const -> expected<void, string>
    {
      if (const int ret = avcodec_send_packet(_ctx.get(), nullptr); ret < 0)
        return unexpected(detail::ffmpeg_error_string(ret));

      return {};
    }

    // 디코딩: Packet → Frame
    // 호출 측에서 frame을 준비해 넘기고, 리턴값은 AVERROR(EAGAIN), AVERROR_EOF, 0
    auto decode(const AVPacket* pkt, AVFrame* frame) const -> expected<DecodeResult, string>
    {
      int ret = avcodec_send_packet(_ctx.get(), pkt);
      if (ret < 0)
        return unexpected(detail::ffmpeg_error_string(ret));

      ret = avcodec_receive_frame(_ctx.get(), frame);
      if (ret == 0) 
        return DecodeResult::FrameAvailable;

      if (ret == AVERROR(EAGAIN)) 
        return DecodeResult::NeedMoreData;

      if (ret == AVERROR_EOF) 
        return DecodeResult::EndOfStream;

      return {};
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