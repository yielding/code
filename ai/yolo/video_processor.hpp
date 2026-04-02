#pragma once

#include "detection.hpp"
#include "yolo_detector.hpp"

#include <opencv2/opencv.hpp>

#include <print>
#include <string>
#include <filesystem>
#include <fstream>

extern "C" {
#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libswscale/swscale.h>
#include <libavutil/imgutils.h>
#include <libavutil/opt.h>
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace yolo
{
  using namespace std;
  namespace fs = std::filesystem;

  class VideoProcessor
  {
  public:
    VideoProcessor(string const& input_path, string const& output_dir)
      : _input_path(input_path)
      , _output_dir(output_dir)
    {
      fs::create_directories(_output_dir);
    }

    ~VideoProcessor()
    {
      cleanup();
    }

  public:
    auto process(YOLODetector& detector,
                 unordered_set<string> const& target_classes,
                 const float conf_threshold) -> bool
    {
      if (!open_input())
        return false;

      if (!open_output())
        return false;

      println("Video: {}x{}, {:.2f} fps, {} frames",
              _width, _height, _fps, _total_frames);

      auto frame    = av_frame_alloc();
      auto bgr_frame = av_frame_alloc();
      auto out_frame = av_frame_alloc();
      auto packet   = av_packet_alloc();

      av_image_alloc(bgr_frame->data, bgr_frame->linesize,
                     _width, _height, AV_PIX_FMT_BGR24, 1);

      av_image_alloc(out_frame->data, out_frame->linesize,
                     _width, _height, AV_PIX_FMT_YUV420P, 1);
      out_frame->format = AV_PIX_FMT_YUV420P;
      out_frame->width  = _width;
      out_frame->height = _height;

      auto sws_to_bgr = sws_getContext(
        _width, _height, _dec_ctx->pix_fmt,
        _width, _height, AV_PIX_FMT_BGR24,
        SWS_BILINEAR, nullptr, nullptr, nullptr);

      auto sws_to_yuv = sws_getContext(
        _width, _height, AV_PIX_FMT_BGR24,
        _width, _height, AV_PIX_FMT_YUV420P,
        SWS_BILINEAR, nullptr, nullptr, nullptr);

      int frame_no = 0;
      vector<vector<Detection>> all_detections;

      while (av_read_frame(_fmt_ctx, packet) >= 0)
      {
        if (packet->stream_index != _video_stream_idx)
        {
          av_packet_unref(packet);
          continue;
        }

        avcodec_send_packet(_dec_ctx, packet);

        while (avcodec_receive_frame(_dec_ctx, frame) == 0)
        {
          // Decode -> BGR
          sws_scale(sws_to_bgr, frame->data, frame->linesize,
                    0, _height, bgr_frame->data, bgr_frame->linesize);

          cv::Mat mat(_height, _width, CV_8UC3, bgr_frame->data[0],
                      bgr_frame->linesize[0]);

          auto detections = detector.detect(mat, target_classes, conf_threshold);
          draw_detections(mat, detections);
          all_detections.push_back(std::move(detections));

          // BGR -> YUV420P
          sws_scale(sws_to_yuv,
                    bgr_frame->data, bgr_frame->linesize,
                    0, _height,
                    out_frame->data, out_frame->linesize);

          out_frame->pts = frame_no;
          encode_frame(out_frame);

          frame_no++;
          if (frame_no % 30 == 0 || frame_no == _total_frames)
            print("\rProcessing: {}/{} frames ({:.0f}%)",
                  frame_no, _total_frames,
                  (float)frame_no / max(_total_frames, 1) * 100);
        }

        av_packet_unref(packet);
      }

      // Flush encoder
      encode_frame(nullptr);
      av_write_trailer(_out_fmt_ctx);

      println("\rProcessing: {}/{} frames (100%)", frame_no, frame_no);

      auto output_video = fs::path(_output_dir) / output_filename();
      println("Output video saved to: {}", output_video.string());

      save_video_metadata(all_detections);

      sws_freeContext(sws_to_bgr);
      sws_freeContext(sws_to_yuv);
      av_freep(&bgr_frame->data[0]);
      av_freep(&out_frame->data[0]);
      av_frame_free(&frame);
      av_frame_free(&bgr_frame);
      av_frame_free(&out_frame);
      av_packet_free(&packet);

      return true;
    }

  private:
    auto open_input() -> bool
    {
      if (avformat_open_input(&_fmt_ctx, _input_path.c_str(), nullptr, nullptr) < 0)
      {
        println(stderr, "Error: Cannot open video '{}'", _input_path);
        return false;
      }

      avformat_find_stream_info(_fmt_ctx, nullptr);

      for (unsigned i = 0; i < _fmt_ctx->nb_streams; i++)
      {
        if (_fmt_ctx->streams[i]->codecpar->codec_type == AVMEDIA_TYPE_VIDEO)
        {
          _video_stream_idx = i;
          break;
        }
      }

      if (_video_stream_idx < 0)
      {
        println(stderr, "Error: No video stream found");
        return false;
      }

      auto* par = _fmt_ctx->streams[_video_stream_idx]->codecpar;
      auto* codec = avcodec_find_decoder(par->codec_id);
      _dec_ctx = avcodec_alloc_context3(codec);
      avcodec_parameters_to_context(_dec_ctx, par);
      avcodec_open2(_dec_ctx, codec, nullptr);

      _width  = par->width;
      _height = par->height;

      auto const& tb = _fmt_ctx->streams[_video_stream_idx]->avg_frame_rate;
      _fps = (tb.den > 0) ? (double)tb.num / tb.den : 30.0;
      _total_frames = (int)_fmt_ctx->streams[_video_stream_idx]->nb_frames;
      if (_total_frames <= 0)
        _total_frames = (int)(_fmt_ctx->duration / AV_TIME_BASE * _fps);

      return true;
    }

    auto open_output() -> bool
    {
      auto output_path = (fs::path(_output_dir) / output_filename()).string();

      avformat_alloc_output_context2(&_out_fmt_ctx, nullptr, nullptr,
                                     output_path.c_str());

      auto* codec = avcodec_find_encoder(AV_CODEC_ID_H264);
      if (!codec)
      {
        println(stderr, "Error: H264 encoder not found");
        return false;
      }

      auto* stream = avformat_new_stream(_out_fmt_ctx, codec);
      _enc_ctx = avcodec_alloc_context3(codec);

      _enc_ctx->width     = _width;
      _enc_ctx->height    = _height;
      _enc_ctx->pix_fmt   = AV_PIX_FMT_YUV420P;
      _enc_ctx->time_base = {1, (int)_fps};
      _enc_ctx->framerate = {(int)_fps, 1};
      _enc_ctx->bit_rate  = 4000000;
      _enc_ctx->gop_size  = 12;

      if (_out_fmt_ctx->oformat->flags & AVFMT_GLOBALHEADER)
        _enc_ctx->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;

      av_opt_set(_enc_ctx->priv_data, "preset", "medium", 0);
      avcodec_open2(_enc_ctx, codec, nullptr);
      avcodec_parameters_from_context(stream->codecpar, _enc_ctx);
      stream->time_base = _enc_ctx->time_base;

      if (!(_out_fmt_ctx->oformat->flags & AVFMT_NOFILE))
        avio_open(&_out_fmt_ctx->pb, output_path.c_str(), AVIO_FLAG_WRITE);

      (void)avformat_write_header(_out_fmt_ctx, nullptr);

      return true;
    }

    auto encode_frame(AVFrame* frame) -> void
    {
      avcodec_send_frame(_enc_ctx, frame);

      auto* pkt = av_packet_alloc();
      while (avcodec_receive_packet(_enc_ctx, pkt) == 0)
      {
        av_packet_rescale_ts(pkt, _enc_ctx->time_base,
                             _out_fmt_ctx->streams[0]->time_base);
        pkt->stream_index = 0;
        av_interleaved_write_frame(_out_fmt_ctx, pkt);
        av_packet_unref(pkt);
      }
      av_packet_free(&pkt);
    }

    auto output_filename() const -> string
    {
      auto stem = fs::path(_input_path).stem().string();
      return stem + "_detected.mp4";
    }

    auto save_video_metadata(vector<vector<Detection>> const& all_detections) -> void
    {
      auto stem = fs::path(_input_path).stem().string();
      auto json_path = (fs::path(_output_dir) / (stem + "_detected.json")).string();

      ofstream ofs(json_path);
      ofs << "{\n"
          << "  \"source\": \"" << _input_path << "\",\n"
          << "  \"width\": "    << _width      << ",\n"
          << "  \"height\": "   << _height     << ",\n"
          << "  \"fps\": "      << _fps        << ",\n"
          << "  \"total_frames\": " << all_detections.size() << ",\n"
          << "  \"frames\": [\n";

      for (size_t f = 0; f < all_detections.size(); f++)
      {
        auto const& dets = all_detections[f];
        ofs << "    { \"frame\": " << f << ", \"detections\": [";

        for (size_t d = 0; d < dets.size(); d++)
        {
          auto const& det = dets[d];
          ofs << "{ \"class\": \"" << det.class_name << "\""
              << ", \"confidence\": " << det.confidence
              << ", \"box\": [" << det.box.x << "," << det.box.y
              << "," << det.box.width << "," << det.box.height << "] }";
          if (d + 1 < dets.size()) ofs << ", ";
        }

        ofs << "] }";
        if (f + 1 < all_detections.size()) ofs << ",";
        ofs << "\n";
      }

      ofs << "  ]\n}\n";
      println("Metadata saved to: {}", json_path);
    }

    auto cleanup() -> void
    {
      if (_dec_ctx)      { avcodec_free_context(&_dec_ctx);      _dec_ctx = nullptr; }
      if (_enc_ctx)      { avcodec_free_context(&_enc_ctx);      _enc_ctx = nullptr; }
      if (_fmt_ctx)      { avformat_close_input(&_fmt_ctx);      _fmt_ctx = nullptr; }
      if (_out_fmt_ctx)
      {
        if (_out_fmt_ctx->pb && !(_out_fmt_ctx->oformat->flags & AVFMT_NOFILE))
          avio_closep(&_out_fmt_ctx->pb);
        avformat_free_context(_out_fmt_ctx);
        _out_fmt_ctx = nullptr;
      }
    }

  private:
    string _input_path;
    string _output_dir;

    AVFormatContext*  _fmt_ctx     = nullptr;
    AVCodecContext*   _dec_ctx     = nullptr;
    int               _video_stream_idx = -1;

    AVFormatContext*  _out_fmt_ctx = nullptr;
    AVCodecContext*   _enc_ctx     = nullptr;

    int    _width        = 0;
    int    _height       = 0;
    double _fps          = 30.0;
    int    _total_frames = 0;
  };
}
