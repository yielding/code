#include "video.scaler.hpp"
#include <iostream>

using namespace av;

int main(int argc, char** argv) 
{
  if (argc < 2) 
  {
    std::cerr << "Usage: " << argv[0] << " input.mp4" << std::endl;
    return 1;
  }

  const char* input_filename = argv[1];

  // 1. 입력 파일 열기
  AVFormatContext* fmt_ctx = nullptr;
  if (avformat_open_input(&fmt_ctx, input_filename, nullptr, nullptr) < 0)
  {
    std::cerr << "Error: cannot open input file." << std::endl;
    return 1;
  }

  if (avformat_find_stream_info(fmt_ctx, nullptr) < 0)
  {
    std::cerr << "Error: cannot find stream info." << std::endl;
    avformat_close_input(&fmt_ctx);
    return 1;
  }

  // 2. 비디오 스트림 찾기
  int video_stream_index = -1;
  const AVCodec* decoder = nullptr;
  video_stream_index = av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, &decoder, 0);
  if (video_stream_index < 0) 
  {
    std::cerr << "Error: cannot find a video stream." << std::endl;
    avformat_close_input(&fmt_ctx);
    return 1;
  }

  // 3. 디코더 설정
  auto dec_ctx = avcodec_alloc_context3(decoder);
  avcodec_parameters_to_context(dec_ctx, fmt_ctx->streams[video_stream_index]->codecpar);
  if (avcodec_open2(dec_ctx, decoder, nullptr) < 0) 
  {
    std::cerr << "Error: cannot open codec." << std::endl;
    avcodec_free_context(&dec_ctx);
    avformat_close_input(&fmt_ctx);
    return 1;
  }

  // 4. 패킷 & 프레임 할당
  AVPacket* packet = av_packet_alloc();
  AVFrame* frame = av_frame_alloc();

  // 5. 첫 번째 비디오 프레임 찾기
  while (av_read_frame(fmt_ctx, packet) >= 0) 
  {
    if (packet->stream_index != video_stream_index)
    {
      av_packet_unref(packet);
      continue;
    }

    if (avcodec_send_packet(dec_ctx, packet) >= 0) 
    {
      if (avcodec_receive_frame(dec_ctx, frame) >= 0) 
      {
        // 6. 프레임을 스케일링 후 JPEG로 저장
        VideoScaler scaler(
            dec_ctx->width, dec_ctx->height, dec_ctx->pix_fmt,
            320, 180, AV_PIX_FMT_YUVJ420P
        );

        if (scaler.scale_to_file(frame, "output.jpg")) {
          std::cout << "Saved resized frame to output.jpg" << std::endl;
        } else {
          std::cerr << "Failed to save JPEG." << std::endl;
        }

        av_frame_unref(frame);
        break; // 첫 프레임만 처리
      }
    }

    av_packet_unref(packet);
  }

  // 7. 리소스 해제
  av_frame_free(&frame);
  av_packet_free(&packet);
  avcodec_free_context(&dec_ctx);
  avformat_close_input(&fmt_ctx);

  return 0;
}
