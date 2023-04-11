/*
 * Copyright (c) 2014 Stefano Sabatini
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/**
 * @file
 * libavformat AVIOContext API example.
 *
 * Make libavformat demuxer access media content through a custom
 * AVIOContext read callback.
 * @example avio_reading.c
 */

extern "C" {
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavformat/avio.h>
#include <libavutil/file.h>
}

// REMARK
// buffer_data->ptr can be anything
// istream(ifstream), FILE, char*, network_stream
//
struct buffer_data 
{
  uint8_t *ptr;
  size_t size; ///< size left in the buffer
};

static int read_packet(void *opaque, uint8_t *buf, int buf_size)
{
  auto bd = (buffer_data *)opaque;
  buf_size = FFMIN(buf_size, bd->size);

  if (!buf_size)
    return AVERROR_EOF;

  printf("ptr:%p size:%zu\n", bd->ptr, bd->size);

  /* copy internal buffer data to buf */
  memcpy(buf, bd->ptr, buf_size);
  bd->ptr  += buf_size;
  bd->size -= buf_size;

  return buf_size;
}

int main(int argc, char *argv[])
{
  size_t buffer_size,
         avio_ctx_buffer_size = 4096;

  if (argc != 2) {
    fprintf(stderr, "usage: %s input_file\n"
        "API example program to show how to read from a custom buffer "
        "accessed through AVIOContext.\n", argv[0]);
    return 1;
  }

  auto input_filename = argv[1];

  // NOTICE: 간단하게 파일로 예제를 보여주는 것임.
  // slurp file content into buffer
  uint8_t *buffer = nullptr;
  int ret = av_file_map(input_filename, &buffer, &buffer_size, 0, NULL);
  if (ret < 0)
    return 1;

  /* fill opaque structure used by the AVIOContext read callback */
  buffer_data bd = { .ptr = buffer, .size = buffer_size };

  AVFormatContext *fmt_ctx = nullptr;
  if (!(fmt_ctx = avformat_alloc_context())) 
  {
    ret = AVERROR(ENOMEM);
    return 1;
  }

  auto avio_ctx_buffer = (uint8_t*) av_malloc(avio_ctx_buffer_size);
  if (!avio_ctx_buffer) 
  {
    ret = AVERROR(ENOMEM);
    return 1;
  }

  AVIOContext *avio_ctx = avio_alloc_context(avio_ctx_buffer, avio_ctx_buffer_size,
      0, &bd, &read_packet, NULL, NULL);
  if (!avio_ctx) 
  {
    ret = AVERROR(ENOMEM);
    return 1;
  }

  fmt_ctx->pb = avio_ctx;

  ret = avformat_open_input(&fmt_ctx, NULL, NULL, NULL);
  if (ret < 0) 
  {
    fprintf(stderr, "Could not open input\n");
    return 1;
  }

  ret = avformat_find_stream_info(fmt_ctx, NULL);
  if (ret < 0) 
  {
    fprintf(stderr, "Could not find stream information\n");
    return 1;
  }

  av_dump_format(fmt_ctx, 0, input_filename, 0);

  avformat_close_input(&fmt_ctx);

  /* note: the internal buffer could have changed, and be != avio_ctx_buffer */
  if (avio_ctx)
    av_freep(&avio_ctx->buffer);

  avio_context_free(&avio_ctx);

  av_file_unmap(buffer, buffer_size);

  if (ret < 0) 
  {
    //fprintf(stderr, "Error occurred: %s\n", av_err2str(ret));
    fprintf(stderr, "Error occurred:");
    return 1;
  }

  return 0;
}
