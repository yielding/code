#include <iostream>
#include <locale>
#include <cwchar>
#include <cstring>
 
#include "unicode/unistr.h"
#include "unicode/normlzr.h"
#include "unicode/unorm.h"

using namespace std;
using namespace icu_63;
 
int decompose(const char* text, wchar_t* wcs_buf, uint buf_size)
{
    // UTF-8 to UCS4
    auto str = UnicodeString::fromUTF8(StringPiece(text));
 
    // UCS4 to NFD
    auto status = U_ZERO_ERROR;
    UnicodeString result;
    Normalizer::normalize(str, UNORM_NFD, 0, result, status);
    if (U_FAILURE(status)) 
    {
        cerr << "can't decompose a UTF8 string, "
             << status << ": " << u_errorName(status) << endl;

        return -1;
    }
 
    result.toUTF32((UChar32*) wcs_buf, buf_size, status);
 
    return 0;
}
 
int compose(wchar_t* wcs, uint wcs_len, char* buf, uint buf_size)
{
    // UCS4 to NFC
    auto str    = UnicodeString::fromUTF32((UChar32*) wcs, wcs_len);
    auto status = U_ZERO_ERROR;
    UnicodeString result;
    Normalizer::normalize(str, UNORM_NFC, 0, result, status);
    if (U_FAILURE(status)) 
    {
        cerr << "can't compose a UTF8 string, "
             << status << ": " << u_errorName(status) << endl;

        return -1;
    }
 
    string temp;
    StringByteSink<string> sink(&temp);
    result.toUTF8(sink);
    strcpy(buf, temp.c_str());
 
    return 0;
}
 
int main()
{
    //const char* text = "한글";
    const char* text = "위";

    wchar_t wcs_buf[1024];
    char    new_text[1024];

    decompose(text, wcs_buf, sizeof (wcs_buf));
    for (uint i = 0; wcs_buf[i] != 0; ++i)
        cout << "wcs_buf[" << i << "]=0x" << hex << (int) wcs_buf[i] << endl;
    
    cout << endl;
 
    int wcs_len = wcslen(wcs_buf);
    compose(wcs_buf, wcs_len, new_text, sizeof (new_text));
    cout << new_text << endl;
 
    return 0;
}
