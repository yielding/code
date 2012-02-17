// #include "Stdafx.h"

#include "HexUtil.h"
#include "detail/HexUtilDetail.h"

#include <boost/shared_array.hpp>
#include <boost/algorithm/string.hpp>

#include <sstream>
#include <iomanip>
#include <iterator>
#include <iostream>

namespace utility {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int64_t Hex::to_int64(uint8_t const* buf)
{
    return to_int64le(buf);
}

uint32_t Hex::to_uint32(uint8_t const* buf)
{
    return to_uint32le(buf);
}

uint16_t Hex::to_uint16(uint8_t const* buf)
{
    return to_uint16le(buf);
}

#ifdef _MFC_VER
CString Hex::to_cstr(uint8_t const* buf, size_t size)
{
    CString s;
    if (!buf || !size || 0xFF == buf[0])
        return s;

    int len = (int)strnlen((char*)buf, size);
    return CString((char*)buf, len);
}

CString Hex::to_cstr(uint8_t const* buf, size_t* size)
{
    if (buf == NULL || size == NULL)
        return L"";

    CString s; s.Empty(); 
    *size = 0;

    s = (char*)buf;

    *size = strlen((char const*)buf) + 1;

    return s;
}

CString Hex::to_cstr(uint8_t const* buf)
{
    if (!buf || 0xFF == buf[0])
        return L"";

    return CString((char*)buf);
}


std::wstring Hex::to_wstr(uint8_t const* buf, int size, uint32_t codepage)
{
    if (!buf || !size || 0xFF == buf[0])
        return L"";

    int sizewb = MultiByteToWideChar(codepage, 0, (char*)buf, size, 0, 0);
    if (sizewb == 0)
        return L"";

    boost::shared_array<wchar_t> str(new wchar_t[sizewb + 1]);
    ZeroMemory(str.get(), sizeof(wchar_t) * (sizewb + 1));
    MultiByteToWideChar(codepage, 0, (char*)buf, size, str.get(), sizewb);

    return str.get();
}

#endif

std::string Hex::to_str(uint8_t const* buf, int size)
{
    return std::string((char*)buf, size);
}

Hex::bytes Hex::bytes_from_hexcode(std::string const& str, bool is_be)
{
    using namespace boost;

    Hex::bytes result;

    if (str.length() % 2 == 0 && all(str, is_xdigit()))
    {
        int size = int(str.length() / 2);
        for (int i=0; i<size; ++i)
        {
            std::string in = is_be 
                ? str.substr((size - (i + 1)) * 2, 2)
                : str.substr(i * 2, 2);

            uint32_t tmp;
            std::istringstream strm(in);
            strm >> std::hex >> tmp;
            result.push_back((uint8_t)tmp);
        }
    }

    return result;
}

Hex::bytes Hex::bytes_from_hexcode(std::wstring const& str, bool is_be)
{
    using namespace boost;

    Hex::bytes result;

    if (str.length() % 2 == 0 && all(str, is_xdigit()))
    {
        int size = int(str.length() / 2);
        for (int i=0; i<size; ++i)
        {
            std::wstring in = is_be 
                ? str.substr((size - (i + 1)) * 2, 2)
                : str.substr(i * 2, 2);

            uint32_t tmp;
            std::wistringstream strm(in);
            strm >> std::hex >> tmp;
            result.push_back((uint8_t)tmp);
        }
    }

    return result;
}

int64_t Hex::from_int64le_hexcode(std::wstring str)
{
    if (str.size() > 16)
        str.resize(16);

    // append additional '0' in case the length is odd
    if (str.size() % 2 == 1)
        str.push_back('0');

    std::wstring tmp;
    for (int i = (int)(str.size() - 2); i >= 0; i -= 2)
        tmp.append(str.substr(i, 2));

    int64_t hexValue = 0;
    std::wistringstream strm(tmp);
    strm >> std::hex >> hexValue;

    return hexValue;
}

int64_t Hex::from_int64_hexcode(std::wstring str)
{
    return from_int64le_hexcode(str);
}

int64_t Hex::from_int64(std::wstring str)
{
    return from_int64le_hexcode(str);
}

int64_t Hex::from_int64le(std::wstring str)
{
    return from_int64le_hexcode(str);
}

int64_t Hex::from_int64be_hexcode(std::wstring str)
{
    if (16 < str.size())
        str.resize(16);

    if (str.size() % 2 == 1)
        str.insert(0, L"0");

    int64_t hexValue = 0;
    std::wistringstream strm(str);
    strm >> std::hex >> hexValue;

    return hexValue;
}

int64_t Hex::from_int64be(std::wstring str)
{
    return from_int64be_hexcode(str);
}

#ifdef _MFC_VER

// 1. prev : vector<u8> Str2Hex(wstring, codepage)
Hex::bytes Hex::bytes_from_wstr(std::wstring const& str, uint32_t codepage)
{
    Hex::bytes buf;

    if (str.empty())
        return buf;

    int size = ::WideCharToMultiByte(codepage, 0, str.c_str(), -1, 0, 0, NULL, NULL);
    if (size == 0)
        return buf;

    // remove 'NULL' size
    --size;

    buf.resize(size, 0);

    ::WideCharToMultiByte(codepage, 0, str.c_str(), 
            (int)str.length(), (char*) & (*(buf.begin())), size, NULL, NULL);

    return buf;
}
  
size_t Hex::str_length_by(uint8_t const* buf, int max_, std::wstring const& sentinal)
{
  for (int i=0; i<max_; i++)
  {
    std::wstring tmp = to_wstr(buf + i, sentinal.length());
    if (tmp == sentinal)
      return i;
  }
  
  return 0;
}
  
#endif
  
// 3. unicode
std::wstring Hex::to_unicode(uint8_t const* buf, int size)
{
    return to_utf16le(buf, size);
}

std::wstring Hex::to_unicode_jpn(uint8_t const* buf, int size)
{
    return to_utf16(buf, size, false, 0x7f);
}

std::wstring Hex::to_utf16le(uint8_t const* buf, int size)
{
    return to_utf16(buf, size, false, 0xff);
}

std::wstring Hex::to_utf16be(uint8_t const* buf, int size)
{
    return to_utf16(buf, size, true, 0xff);
}

// 4. length
size_t Hex::c_str_size(uint8_t const* buf)
{
    return strlen((char*)buf) + 1;
}

size_t Hex::c_str_length(uint8_t const* buf)
{
    return strlen((char*)buf) + 1;
}

Hex::bytes Hex::bytes_from_hexcode_be(std::wstring const& str)
{
    return bytes_from_hexcode(str, true);
}

Hex::bytes Hex::bytes_from_hexcode_le(std::wstring const& str)
{
    return bytes_from_hexcode(str, false);
}

std::wstring Hex::to_hexcode_be(uint8_t const* buf, int size)
{
    return to_hexcode(buf, size, true);
}

std::wstring Hex::to_hexcode_le(uint8_t const* buf, int size)
{
    return to_hexcode(buf, size, false);
}

std::wstring Hex::to_hexcode(uint8_t const* buf, int size, bool is_be)
{
    assert(size > 0);

    if (buf == nullptr)
        return L"";

    Hex::bytes codes;

    is_be ? std::reverse_copy(buf, buf+size, back_inserter(codes))
          : std::copy(buf, buf+size, back_inserter(codes));

    std::wostringstream strm;
    for (auto it=codes.begin(); it!=codes.end(); ++it)
    {
        strm << std::setw(2) << std::setfill(L'0');
        strm << std::uppercase << std::hex << *it;
    }

    return strm.str();
}

// standard interface with size
uint32_t Hex::to_uint32(uint8_t const* buf, int size)
{
    if (buf == nullptr || size <= 0)
        return 0;

    if (size == 1) return buf[0];
    if (size == 2) return to_uint16le(buf);

    return (size == 3) ? to_uint24le(buf) : to_uint32le(buf);
}

int64_t Hex::to_int64(uint8_t const* buf, int size)
{
    if (buf == nullptr || size <= 0)
        return 0;

    if (size == 1) return int64_t(buf[0]);
    if (size == 2) return int64_t(to_uint16le(buf));
    if (size == 4) return int64_t(to_uint32le(buf));
    if (size == 8) return int64_t(to_int64le(buf));

    int64_t result = 0;
    for (int i=0; i<size; i++)
        result += int64_t(buf[i]) << (8 *i);

    return result;
}

// standard interface without size
uint16_t Hex::to_uint16le(uint8_t const* buf)
{
    return *((uint16_t*)buf);
}

uint16_t Hex::to_uint16be(uint8_t const* buf)
{
    return (*buf << 8) + (*(buf + 1));
}

int16_t  Hex::to_int16le (uint8_t const* buf)
{
    return *((int16_t*)buf);
}

int32_t  Hex::to_int32be (uint8_t const* buf)
{
    return int32_t(to_uint32be(buf));
}

int32_t  Hex::to_int32le (uint8_t const* buf)
{
    return *((int32_t *)buf);
}

int16_t Hex::to_int16be(uint8_t const* buf)
{
    return int16_t(to_uint16be(buf));
}

uint32_t Hex::to_uint24le(uint8_t const* buf)
{
    return *((uint32_t *)buf) & 0x00FFFFFF;
}

uint32_t Hex::to_uint24be(uint8_t const* buf)
{
    return detail::to_intbe<uint32_t>(buf, 3);
}

uint32_t Hex::to_uint32le(uint8_t const* buf)
{
    return *((uint32_t*)buf);
}

uint32_t Hex::to_uint32be(uint8_t const* buf)
{
    return detail::to_intbe<uint32_t>(buf, 4);
}

uint64_t Hex::to_uint64be(uint8_t const* buf)
{
    return detail::to_intbe<uint64_t>(buf, 8);
}

uint64_t Hex::to_uint64le(uint8_t const* buf)
{
    return *((uint64_t*)buf);
}

int64_t Hex::to_int40le(uint8_t const* buf)
{
    return *((int64_t*)buf) & 0x000000FFFFFFFFFF;
}

int64_t Hex::to_int40be(uint8_t const* buf)
{
    return detail::to_intbe<int64_t>(buf, 5);
}

int64_t Hex::to_int48le(uint8_t const* buf)
{
    return *((int64_t*)buf) & 0x0000FFFFFFFFFFFF;
}

int64_t Hex::to_int48be(uint8_t const* buf)
{
    return detail::to_intbe<int64_t>(buf, 6);
}

int64_t Hex::to_int56le(uint8_t const* buf)
{
    return *((int64_t*)buf) & 0x00FFFFFFFFFFFFFF;
}

int64_t Hex::to_int56be(uint8_t const* buf)
{
    return detail::to_intbe<int64_t>(buf, 7);
}

int64_t Hex::to_int64le(uint8_t const* buf)
{
    return *((int64_t*)buf);
}

int64_t Hex::to_int64be(uint8_t const* buf)
{
    return detail::to_intbe<int64_t>(buf, 8);
}

std::wstring Hex::to_utf16(uint8_t const * buf, int& size, bool flip, int tag)
{
    if (!buf || 0 == size)
        return L"";

    std::wstring str;
    std::wstring temp;

    int lhd = 0, rhd = 1;
    if (flip) std::swap(lhd, rhd);

    int i = 0;
    for (; ; i += 2)
    {
        if (size < 0 && 0x00 == buf[i] && 0x00 == buf[i + 1])
            break;

        if (0 < size && size <= i)
            break;

        uint8_t temp_wchar[2] = {
            static_cast<uint8_t>((buf[(uint8_t)i + (uint8_t)lhd] & (uint8_t)tag)), 
            static_cast<uint8_t>((buf[(uint8_t)i + (uint8_t)rhd] & (uint8_t)tag))
        };

        temp.assign((wchar_t*)temp_wchar, 1);

        str += temp;
    }

    if (size > 0)
        str = str.substr(0, size / 2);
    else
        size = i;

    return str;
}

// variable integer used in SQLite
int64_t Hex::to_var_int(uint8_t const* buf, int const size_max, int& size)
{
    if (buf == nullptr || size_max <= 0)
    {
        assert(0);
        size = 0;
        return 0;
    }

    int64_t val = (*buf) & 0x7F;

    int i = 1;
    bool hasNext = (0 != ((*buf) & 0x80));
    while (hasNext && (i < 8) && (i < size_max))
    {
        val <<= 7;
        val |= *(buf + i) & 0x7F;
        hasNext = (0 != ((*(buf + i)) & 0x80));
        i++;
    }

    if (hasNext && (i < size_max))
    {
        val <<= 8;
        val |= *(buf + i);
        i++;
    }

    size = i;
    assert(size <= size_max);

    return val;
}

uint8_t Hex::to_bcd(uint8_t byte_)
{
    return ((byte_ & 0xF0) >> 4) * 10 + (byte_ & 0x0F);
}

uint8_t Hex::from_bcd(uint8_t bcd)
{
    if (99 < bcd)
    {
        assert(0);
        bcd %= 100;
    }

    return (bcd / 10) * 16 + (bcd % 10);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
