#include "AVPacketManager.hpp"
#include <stdexcept>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
AVPacketManager::AVPacketManager() 
{
  pkt_ = av_packet_alloc();
  if (!pkt_) 
    throw std::runtime_error("Failed to allocate AVPacket");
}

AVPacketManager::~AVPacketManager() 
{
  av_packet_free(&pkt_);
}

auto AVPacketManager::get() -> AVPacket* 
{
  return pkt_;
}

void AVPacketManager::unref() 
{
  av_packet_unref(pkt_);
}

void AVPacketManager::clone_from(const AVPacket* other) 
{
  av_packet_unref(pkt_);
  if (av_packet_ref(pkt_, other) < 0)
    throw std::runtime_error("av_packet_ref failed");
}

void AVPacketManager::move_from(AVPacketManager& other) 
{
  av_packet_unref(pkt_);
  av_packet_move_ref(pkt_, other.pkt_);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
