#pragma once

#include "ffmpeg.hpp"

class AVPacketManager 
{
public:
  AVPacketManager();
  ~AVPacketManager();

  AVPacket* get();
  void unref();
  void clone_from(const AVPacket* other);
  void move_from(AVPacketManager& other);

private:
  AVPacket* pkt_;
};
