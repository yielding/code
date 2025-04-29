#pragma once

#include <memory>
#include <stdexcept>

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
namespace av::detail {

  template<typename T, void (*FreeFunc)(T**)>
  struct AVDeleter 
  {
    void operator()(T* ptr) const noexcept 
    {
      if (ptr)
        FreeFunc(&ptr);
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

  template<typename T, T* (*AllocFunc)(), void (*UnrefFunc)(T*), void (*FreeFunc)(T**)>
  class UniqueAVResource
  {
  public:
    UniqueAVResource() : _resource(AllocFunc()) 
    {
      if (!_resource)
        throw std::runtime_error("Failed to allocate resource");
    }
  
    // move만 허용
    UniqueAVResource(UniqueAVResource&&) noexcept = default;
    UniqueAVResource& operator=(UniqueAVResource&&) noexcept = default;
  
    UniqueAVResource(const UniqueAVResource&) = delete;
    UniqueAVResource& operator=(const UniqueAVResource&) = delete;

    void reset() noexcept 
    {
      if (_resource)
        UnrefFunc(&_resource);
    }
  
    T* get() const noexcept { return _resource.get(); }
    T* release() noexcept { return _resource.release(); }
  
  private:
    unique_ptr<T, detail::AVDeleter<T, FreeFunc>> _resource;
  };
  
  template<typename T, T* (*AllocFunc)(), void (*FreeFunc)(T**), int (*RefFunc)(T* dst, const T* src), void (*UnrefFunc)(T*)>
  class RefAVResource 
  {
  public:
    RefAVResource() : _resource(AllocFunc()) 
    {
      if (!_resource)
        throw std::runtime_error("Failed to allocate resource");
    }
  
    // 복사: ref
    RefAVResource(const RefAVResource& other) : _resource(AllocFunc()) 
    {
      if (!_resource)
        throw std::runtime_error("Failed to allocate resource");

      if (RefFunc(_resource.get(), other._resource.get()) < 0)
        throw std::runtime_error("Failed to ref resource");
    }
  
    auto operator=(const RefAVResource& other) -> RefAVResource& 
    {
      if (this != &other)
        if (RefFunc(_resource.get(), other._resource.get()) < 0)
          throw std::runtime_error("Failed to ref resource");

      return *this;
    }
  
    // move 지원
    RefAVResource(RefAVResource&&) noexcept = default;
    RefAVResource& operator=(RefAVResource&&) noexcept = default;

    void reset() noexcept 
    {
      if (_resource)
        UnrefFunc(_resource.get());
    }
  
    T* get() const noexcept { return _resource.get(); }
  
  private:
    unique_ptr<T, detail::AVDeleter<T, FreeFunc>> _resource;
  };

  using UniquePacket = UniqueAVResource<AVPacket, av_packet_alloc, av_packet_unref, av_packet_free>;
  using UniqueFrame = UniqueAVResource<AVFrame, av_frame_alloc, av_frame_unref, av_frame_free>;

  using PacketRef = RefAVResource<AVPacket, av_packet_alloc, av_packet_free, av_packet_ref, av_packet_unref>;
  using FrameRef = RefAVResource<AVFrame, av_frame_alloc, av_frame_free, av_frame_ref, av_frame_unref>;

}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
