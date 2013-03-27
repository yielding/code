#include "stdafx.h"

#include "BPlist.h"

#include <boost/filesystem.hpp>

#include <fstream>
#include <algorithm>
#include <cassert>
#include <cstring>

#if defined(WIN32) && defined(_DEBUG)
#define new DEBUG_NEW
#endif

using namespace std;
namespace fs = boost::filesystem;

namespace utility { namespace parser {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
PropertyList::PropertyList(string const& fname)
{
    m_is_open = open(fname);
}

PropertyList::PropertyList(ByteBuffer const& buffer)
{
    close();
    m_buffer  = buffer;
    m_is_open = init();
}

PropertyList::PropertyList(CFType t)
{
    m_value = t;
}

PropertyList::~PropertyList()
{
    close();
}

void PropertyList::close()
{
    m_is_open = false;
    m_buffer.reset();
}

bool PropertyList::open(string const& fname)
{
    close();

    if (!read_file(fname))
        return false;

    m_is_open = init();

    return m_is_open;
}

bool PropertyList::load(char* buffer, size_t size)
{
    ByteBuffer b((uint8_t*)buffer, size);
    m_buffer = b;
    m_is_open = init();

    return m_is_open;
}

void PropertyList::get_root_object()
{
    m_value = read_object_at(m_offset_table[(size_t)m_top_object]);
}

CFType PropertyList::read_object_at(int64_t pos)
{
    m_buffer.offset(pos);
    return read_object();
}

CFType PropertyList::read_object_at_ref(int64_t ref)
{
    return read_object_at(m_offset_table[size_t(ref)]);
}

CFType PropertyList::read_object()
{
    // 1. read marker
    uint8_t marker = m_buffer.get_uint1();

    // 2.1 special case for constants : NULL
    if (marker == CF_NULL_)
        return CFNull();

    // 2.2 Boolean
    if (marker == CF_TRUE || marker == CF_FALSE)
        return CFBoolean(marker == CF_TRUE);

    // 3. variable size data decoding
    uint8_t hi_nibble = marker & 0xf0;
    uint8_t lo_nibble = marker & 0x0f;

    if (hi_nibble == CF_INTEGER)
    {
        int64_t value = m_buffer.get_int<int64_t>(1<<lo_nibble);
        return CFInteger(value);
    }

    if (hi_nibble == CF_REAL)
    {
        int size = 1 << lo_nibble;
        if (size == 8)
        {
            int64_t value = m_buffer.get_int<int64_t>(size);
            return CFReal(*(double *)&value);
        }

        if (size == 4)
        {
            uint32_t value = m_buffer.get_int<uint32_t>(size);
            return CFReal(*(float *)&value);
        }
    }

    if (hi_nibble == CF_DATE)
    {
        int64_t value = m_buffer.get_int<int64_t>(1 << lo_nibble);
        double v= *(double*)&value;
        return CFDate(v);
    }

    // 4. determine length of object for variable sized object
    size_t length = size_t(get_length(lo_nibble));

    if (hi_nibble == CF_DATA)
    {
        string res = m_buffer.get_string(length);
        return CFData(res);
    }

    if (hi_nibble == CF_STRING)
    {
        string str = m_buffer.get_string(length);
        return CFString(str);
    }

    if (hi_nibble == CF_UNICODE)
    {
        wstring utf16;
        for (size_t i=0; i<length; i++)
            utf16.append(1, wchar_t(m_buffer.get_uint2_be()));

        return CFUnicodeString(utf16);
    }

    if (hi_nibble == CF_ARRAY)
    {
        CFArray arr;

        vector<int64_t> values;
        for (size_t i=0; i<length; i++)
        {
            int64_t obj_ref = m_buffer.get_int<int64_t>(m_offset_ref_size);
            values.push_back(obj_ref);
        }

        for (size_t i=0; i<length; i++)
            arr.add(read_object_at_ref(values[i]));

        return arr;
    }

    if (hi_nibble == CF_DICTIONARY)
    {
        CFDictionary dict;

        vector<int64_t> keys;
        for (size_t i=0; i<length; i++)
        {
            int64_t obj_ref = m_buffer.get_int<int64_t>(m_offset_ref_size);
            keys.push_back(obj_ref);
        }

        vector<int64_t> vals;
        for (size_t i=0; i<length; i++)
        {
            int64_t obj_ref = m_buffer.get_int<int64_t>(m_offset_ref_size);
            vals.push_back(obj_ref);
        }

        for (size_t i=0; i<length; i++)
        {
            CFType key   = read_object_at_ref(keys[i]);
            CFType value = read_object_at_ref(vals[i]);
            dict.add(key, value);
        }

        return dict;
    }

    return CFNull();
}

PropertyList& PropertyList::set(CFType v)
{
    m_value = v;

    return *this;
}

int PropertyList::type(CFType t)
{
    return boost::apply_visitor(type_visitor(), t);
}

int PropertyList::type()
{
    return boost::apply_visitor(type_visitor(), m_value);
}

string PropertyList::to_xml() const
{
    xml_builder::visitor_type v;
    boost::apply_visitor(v, m_value);
    string header = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \""
        "http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
        "<plist version=\"1.0\">\n";
    string footer = "</plist>\n";

    return header + v.result() + footer;
}

ByteBuffer PropertyList::to_bin()
{
    assert(0);
    return ByteBuffer();
}

CFType PropertyList::handle()
{
    return m_value;
}

CFArray PropertyList::as_array()
{
    CFArray* arr = boost::get<CFArray>(&m_value);
    return arr ? *arr : CFArray();
}

CFDictionary PropertyList::as_dictionary()
{
    CFDictionary* dict = boost::get<CFDictionary>(&m_value);

    return dict ? *dict : CFDictionary();
}

bool PropertyList::init()
{
    if (memcmp("bplist00", (void const*)m_buffer, 8) != 0)
        return false;

    if (m_buffer.size() < 22 + 8)
        return false;

    m_buffer.offset(m_buffer.size() - TRAILER_SIZE);
    m_offset_size     = m_buffer.get_uint1();
    m_offset_ref_size = m_buffer.get_uint1();
    m_object_count    = m_buffer.get_int8_be();
    m_top_object      = m_buffer.get_int8_be();
    m_table_offset    = m_buffer.get_int8_be();
    m_offset_table.resize((size_t)m_object_count);
    m_buffer.offset(m_table_offset);

    for (size_t i=0; i<(size_t)m_object_count; i++)
    {
        switch(m_offset_size)
        {
            case 1: m_offset_table[i] = m_buffer.get_uint1();    break;
            case 2: m_offset_table[i] = m_buffer.get_uint2_be(); break;
            case 3: m_offset_table[i] = m_buffer.get_uint3_be(); break;
            case 4: m_offset_table[i] = m_buffer.get_uint4_be(); break;
            case 8: m_offset_table[i] = m_buffer.get_int8_be();  break;
            default: return false;
        }
    }

    get_root_object();

    return true;
}

// NOTICE: We can rewrite this code using recursive call
int64_t PropertyList::get_length(uint8_t nibble)
{
    if (nibble != 0x0F)
        return nibble;

    uint8_t m2 = m_buffer.get_uint1();

    uint8_t size = m2 & 0x0f;
    if (size == 0) return m_buffer.get_uint1();
    if (size == 1) return m_buffer.get_uint2_be();
    if (size == 2) return m_buffer.get_uint4_be();
    if (size == 3) return m_buffer.get_int8_be();

    assert(0);

    return -1;
}

bool PropertyList::read_file(string const& path)
{
    if (!fs::exists(path))
        return false;

    ifstream in(path.c_str(), ios_base::binary);
    if (!in.is_open())
        return false;

    size_t file_size = (size_t)fs::file_size(path);
    ByteBuffer::buffer_t& b = m_buffer.get_buffer();

    // NOTICE !! 
    // Here, we MUST NOT use capacity or reserve instead of resize !!! 
    b.resize(file_size);
    in.read((char*)b.data(), b.size());

    return b.size() == file_size;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#if 0
int main(int argc, char const* argv[])
{
    apple::PropertyList plist;

    /**/
    {
        if (!plist.open("/Users/yielding/develop/app/iphone/data/Manifest.plist"))
        {
            cout << "open the file error";
            return -1;
        }

        cout << plist.to_xml();
    }
    /**/

    /**/
    {
        CFDictionary dict;
        dict.add(CFString("TargetIdentifier"), CFString("3c85f0509771a5e5257742531c9f938abf0ebffa"));
        dict.add(CFString("MessageName"),  CFString("Backup"));
        CFArray arr;
        arr.add(CFString("DLMessageProcessMessage"));
        arr.add(dict);
        cout << arr.to_xml();
    }
    /**/

    /**/
    {
        CFArray sub_arr;
        sub_arr.add(CFReal(2.0))
            .add(CFReal(2.1));
        CFDictionary dict;
        dict.add(CFString("SupportedProtocolVersions"), sub_arr);
        dict.add(CFString("MessageName"), CFString("Hello"));

        CFArray arr;
        arr.add(CFString("DLMessageProcessMessage"))
            .add(dict);

        cout << plist.set(arr).to_xml();
        cout << plist.set(arr).type();
    }
    /**/

    /**/
    {
        CFArray arr;
        arr.add(CFString("aaa"))
            .add(CFString("bbb"))
            .add(CFString("ccc"));

        CFArray::iterator it = arr.begin();
        for (; it != arr.end(); ++it)
        {
            CFString* value = boost::get<CFString>(&*it);
            cout << value->value() << endl;
        }

        cout << apple::PropertyList::type(arr) << endl;
    }
    /**/

    /**/
    {
        CFArray brr;
        brr.add(CFString("11")).add(CFString("22")).add(CFString("33"));
        CFArray irr;
        irr.add(CFInteger(111)).add(CFInteger(222)).add(CFInteger(333));

        CFArray arr;
        arr.add(CFString("aaa")).add(CFString("bbb")).add(CFString("ccc"))
            .add(brr)
            .add(irr);

        plist.set(arr);
        cout << plist.to_xml();

        CFArray a = plist.as_array();
        vector<string> s = CFArray::as_strings (a[3]);
        vector<int>    r = CFArray::as_integers(a[4]);
        for (size_t i=0; i<s.size(); i++) cout << s[i] << endl;
        for (size_t i=0; i<r.size(); i++) cout << r[i] << endl;

    }
    /**/

    /**/
    CFDictionary dict;
    dict.add(CFString("TargetIdentifier"), CFString("3c85f0509771a5e5257742531c9f938abf0ebffa"));
    dict.add(CFString("MessageName"),  CFString("Backup"));
    CFArray arr;
    arr.add(CFString("DLMessageProcessMessage"));
    arr.add(dict);
    cout << arr.to_xml();

    CFDictionary dict2;
    dict2.add(CFString("TargetIdentifier"), CFInteger(1));
    dict2.add(CFString("MessageName"),  CFInteger(2));

    map<string, string> m = CFDictionary::as_strings(dict);
    for (map<string, string>::iterator it = m.begin(); it != m.end(); ++it)
        cout << "key :" << it->first << " value: " << it->second << endl;

    cout << "\n";

    map<string, int> m2 = CFDictionary::as_integers(dict2);
    for (map<string, int>::iterator it = m2.begin(); it != m2.end(); ++it)
        cout << "key :" << it->first << " value: " << it->second << endl;

    cout << "\n";

    return 0;
    /**/
}

#endif

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
