#pragma once

#include <utility>

namespace handle::utility {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename Traits>
class unique_handle
{
  using pointer = typename Traits::pointer;

public:
  unique_handle(unique_handle const&) = delete;
  auto operator=(unique_handle const&) -> unique_handle& = delete; 

  explicit unique_handle(pointer value = Traits::invalid()) noexcept
    : m_value{value}
  {
  }

  unique_handle(unique_handle && other) noexcept 
    : m_value{other.release()}
  {
  }

  auto operator = (unique_handle && other) noexcept -> unique_handle&
  {
    if (this != &other)
      reset(other.release());

    return *this;
  }

  ~unique_handle() noexcept
  {
    close();
  }
  
  explicit operator bool() const noexcept
  {
    return m_value != Traits::invalid();
  }

  auto get() const noexcept -> pointer
  {
    return m_value;
  }

  auto get_address_of() noexcept -> pointer*
  {
    return &m_value;
  }

  auto release() noexcept -> pointer
  {
    auto value = m_value;
    m_value = Traits::invalid();

    return value;
  }

  auto reset(pointer value = Traits::invalid()) noexcept -> bool
  {
    if (m_value != value)
    {
      close();
      m_value = value;
    }

    return static_cast<bool>(*this);
  }

  auto swap(unique_handle<Traits>& other) noexcept -> void
  {
    std::swap(m_value, other.m_value);
  }

private:
  pointer m_value;

  auto close() noexcept -> void
  {
    if (*this)
      Traits::close(m_value);
  }
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename Traits>
auto swap(unique_handle<Traits>& lhs,
          unique_handle<Traits>& rhs) noexcept -> void
{
  lhs.swap(rhs);
}

template <typename Traits>
auto operator==(unique_handle<Traits> const& lhs,
                unique_handle<Traits> const& rhs) noexcept -> bool
{
  return lhs.get() == rhs.get();
}

template <typename Traits>
auto operator!=(unique_handle<Traits> const& lhs,
                unique_handle<Traits> const& rhs) noexcept -> bool
{
  return lhs.get() != rhs.get();
}

template <typename Traits>
auto operator<(unique_handle<Traits> const& lhs,
               unique_handle<Traits> const& rhs) noexcept -> bool
{
  return lhs.get() < rhs.get();
}

template <typename Traits>
auto operator>=(unique_handle<Traits> const& lhs,
                unique_handle<Traits> const& rhs) noexcept -> bool
{
  return lhs.get() >= rhs.get();
}

template <typename Traits>
auto operator>(unique_handle<Traits> const& lhs,
               unique_handle<Traits> const& rhs) noexcept -> bool
{
  return lhs.get() > rhs.get();
}

template <typename Traits>
auto operator<=(unique_handle<Traits> const & lhs,
                unique_handle<Traits> const & rhs) noexcept -> bool
{
  return lhs.get() <= rhs.get();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct NullHandleTraits
{
  using pointer = int;

  static auto invalid() throw() -> pointer
  {
    return 0;
  }

  static auto close(pointer value) throw() -> void
  {
    // VERIFY(CloseHandle(value));
  }
};

struct InvalidHandleTraits
{
  using pointer = int;

  static auto invalid() throw() -> pointer
  {
    return -1;
  }

  static auto close(pointer value) throw() -> void
  {
    // VERIFY(CloseHandle(value));
  }
};

using null_handle = unique_handle<NullHandleTraits>;
using invalid_handle = unique_handle<InvalidHandleTraits>;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
