#pragma once

#ifdef HEXUTIL_EXPORTS
#define HEXUTIL_API __declspec(dllexport)
#else
#define HEXUTIL_API __declspec(dllimport)
#endif

#include <string>
#include <vector>
#include <stdint.h>

namespace utility {
////////////////////////////////////////////////////////////////////////////////
//
//  NOTICE: Terminologies
//  ======================
//  Hex          : uint8_t* stream from disk
//  c,w/string   : string repr. like "abc"
//  hexcode      : string repr. like "61 62 63.."
//  bytes        : vector of uint8_t [0x61, 0x62, 0x63] or [97, 98, 99]
//  be/le        : big/little endian
//  unicode(win) : utf16le
//
////////////////////////////////////////////////////////////////////////////////
class HEXUTIL_API Hex 
{
public:
    typedef std::vector<uint8_t> bytes;

public:
    // 1. standard interface without size
    static uint16_t to_uint16le(uint8_t const* buf);
    static uint16_t to_uint16be(uint8_t const* buf);
    static uint32_t to_uint24le(uint8_t const* buf);
    static uint32_t to_uint24be(uint8_t const* buf);
    static uint32_t to_uint32le(uint8_t const* buf);
    static uint32_t to_uint32be(uint8_t const* buf);
    static uint64_t to_uint64be(uint8_t const* buf);
    static uint64_t to_uint64le(uint8_t const* buf);
    static int16_t  to_int16be (uint8_t const* buf);
    static int16_t  to_int16le (uint8_t const* buf);
    static int32_t  to_int32be (uint8_t const* buf);
    static int32_t  to_int32le (uint8_t const* buf);
    static int64_t  to_int40le (uint8_t const* buf);
    static int64_t  to_int40be (uint8_t const* buf);
    static int64_t  to_int48le (uint8_t const* buf);
    static int64_t  to_int48be (uint8_t const* buf);
    static int64_t  to_int56le (uint8_t const* buf);
    static int64_t  to_int56be (uint8_t const* buf);
    static int64_t  to_int64le (uint8_t const* buf);
    static int64_t  to_int64be (uint8_t const* buf);

    //     convenient names (omitted 'le')
    static int64_t  to_int64 (uint8_t const* buf);
    static uint32_t to_uint32(uint8_t const* buf);
    static uint16_t to_uint16(uint8_t const* buf);

    // 2. standard interface with size
    static uint32_t to_uint32(uint8_t const* buf, int size);
    static int64_t  to_int64(uint8_t const* buf, int size);

#ifdef _MFC_VER
    // 3. string
    static CString  to_cstr(uint8_t const* buf, size_t size);
    static CString  to_cstr(uint8_t const* buf, size_t* size);
    static CString  to_cstr(uint8_t const* buf);
#endif

    static std::wstring to_wstr(uint8_t const* buf, int size, uint32_t cp=0); // CP_ACP == 0
    static std::string  to_str (uint8_t const* buf, int size);


    static int64_t  from_int64_hexcode(std::wstring str);
    static int64_t  from_int64le_hexcode(std::wstring str);
    static int64_t  from_int64be_hexcode(std::wstring str);

    //     string convenient names (can omit le, hexcode) 
    static int64_t  from_int64(std::wstring str);
    static int64_t  from_int64le(std::wstring str);
    static int64_t  from_int64be(std::wstring str);

    // 4. unicode
    static std::wstring to_unicode(uint8_t const* buf, int size);
    static std::wstring to_unicode_jpn(uint8_t const* buf, int size);
    static std::wstring to_utf16le(uint8_t const* buf, int size);
    static std::wstring to_utf16be(uint8_t const* buf, int size);
    static std::wstring to_utf16(uint8_t const * buf, int& size, bool flip, int tag);

    // 5. length
    static size_t c_str_size   (uint8_t const* buf);
    static size_t c_str_length (uint8_t const* buf);
    static size_t str_length_by(uint8_t const* buf, int max_, std::wstring const& sentinal);

    // 6. hex code
    static bytes  bytes_from_hexcode(std::string const& str, bool is_be=false);
    static bytes  bytes_from_hexcode(std::wstring const& str, bool is_be=false);
    static bytes  bytes_from_hexcode_be(std::wstring const& str);
    static bytes  bytes_from_hexcode_le(std::wstring const& str);
    static bytes  bytes_from_wstr(std::wstring const& str, uint32_t cp=0);

    static std::wstring to_hexcode(uint8_t const* buf, int size, bool is_be=false);
    static std::wstring to_hexcode_be(uint8_t const* buf, int size);
    static std::wstring to_hexcode_le(uint8_t const* buf, int size);

    // 7. special functions
    // to_var_int: variable integer used in SQLite
    static int64_t to_var_int(uint8_t const* buf, int const size_max, int& size);
    static uint8_t to_bcd(uint8_t byte_);
    static uint8_t from_bcd(uint8_t bcd);
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} // end of utility
