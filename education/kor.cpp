#include <string>
#include <vector>
#include <iostream>
#include <fstream>

#include <boost/algorithm/string.hpp>

using namespace std;
using namespace boost;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class Korean
{
public:
    typedef std::pair<wchar_t, wchar_t> Range;

public:
    Korean()
    {
        m_first  = std::make_pair(0x1100, 0x1112);
        m_center = std::make_pair(0x1161, 0x1175);
        m_last   = std::make_pair(0x11A8, 0x11C2);
        m_compat = std::make_pair(0x3130, 0x318E);
    }

    bool has_korean_component(std::wstring const& str)
    {
        for (auto it=str.begin(); it != str.end(); ++it)
        {
            if (inside(*it, m_first)) return true;
            if (inside(*it, m_compat)) return true;
        }

        return false;
    }

    std::wstring transcode(std::wstring const& str)
    {
        std::wstring res;
        for (int i=0; i<str.length(); )
        {
            wchar_t ch_;
            if (inside(str[i], m_first))
            {
                auto use_count = 2;
                wchar_t first  = str[i ]  - m_first.first;              
                wchar_t second = str[i+1] - m_center.first; 
                wchar_t third  = 0;
                if (i+2 < str.length() && inside(str[i+2], m_last))
                {
                    third = str[i+2] - m_last.first + 1;
                    use_count++;
                }

                ch_ = (first * 21 + second) * 28 + third + 0xAC00;
                i += use_count;
            }
            else
            {
                ch_ = str[i];
                i++;
            }
            res += ch_;
        }

        return res;
    }

private:
    bool inside(wchar_t ch, Range& r)
    {
        return (r.first <= ch) && (ch <= r.second);
    }

private:
    Range m_first, m_center, m_last, m_compat;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char const* argv[])
{
    Korean kor;

    wifstream ifs;
    ifs.open("./ko_KO-dynamic-text.dat");
    if (!ifs.is_open())
        return 1;

    wstring sss = L"ㄱㅡㅂㅎㅏㄱㅔㅆㄷㅏ";
    wcout << kor.transcode(sss) << flush;

    wstring line;
    while (getline(ifs, line))
    {
        vector<wstring> arr;
        split(arr, line, is_any_of(L"\x01"));

        for (auto it=arr.begin(); it != arr.end(); ++it)
        {
            // if (kor.has_korean_component(*it))
                wcout << hex << (int)(*it)[0] << endl;
        }
    }
    
    return 0;
}
