#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <iostream>
#include <string>

namespace boost { namespace property_tree { namespace json_parser {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template<> 
std::wstring create_escapes(std::wstring const& s) 
{   
  std::wstring result;
  std::wstring::const_iterator b = s.begin();
  std::wstring::const_iterator e = s.end();
  while (b != e)
  {   
    wchar_t v = *b;

    std::cout << std::hex << v << std::endl;
    if (v == 0x20 || v == 0x21 || (v >= 0x23 && v <= 0x2E) ||
        (v >= 0x30 && v <= 0x5B) || (v >= 0x5D && v <= 0xFF) || 
        (v >= 0xAC00 && v <= 0xD7A3))  // is_korean
      result += v; 
    else if (v == wchar_t(L'\b')) result += wchar_t(L'\\'), result += wchar_t(L'b');
    else if (v == wchar_t(L'\f')) result += wchar_t(L'\\'), result += wchar_t(L'f');
    else if (v == wchar_t(L'\n')) result += wchar_t(L'\\'), result += wchar_t(L'n');
    else if (v == wchar_t(L'\r')) result += wchar_t(L'\\'), result += wchar_t(L'r');
    else if (v == wchar_t(L'/')) result += wchar_t(L'\\'), result += wchar_t(L'/');
    else if (v == wchar_t(L'"'))  result += wchar_t(L'\\'), result += wchar_t(L'"');
    else if (v == wchar_t(L'\\')) result += wchar_t(L'\\'), result += wchar_t(L'\\');
    else
    {   
      const wchar_t *hexdigits = L"0123456789ABCDEF";
      typedef make_unsigned<wchar_t>::type UCh;
      unsigned long u = (std::min)(static_cast<unsigned long>(static_cast<UCh>(v)), 0xFFFFul);
      int d1 = u / 4096; u -= d1 * 4096;
      int d2 = u / 256; u -= d2 * 256;
      int d3 = u / 16; u -= d3 * 16; 
      int d4 = u;
      result += wchar_t(L'\\'); result += wchar_t(L'u');
      result += wchar_t(hexdigits[d1]); result += wchar_t(hexdigits[d2]);
      result += wchar_t(hexdigits[d3]); result += wchar_t(hexdigits[d4]);
    }   
    ++b;
  }

  return result;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} // end of json_parser
} // end of property_tree
} // end of boost
