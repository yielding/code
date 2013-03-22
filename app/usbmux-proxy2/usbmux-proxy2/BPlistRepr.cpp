#include "stdafx.h"

#include "BPlistRepr.h"
#include "Base64.h"

#include <boost/algorithm/string.hpp>
#include <boost/format.hpp>
#include <iostream>

#if defined(WIN32) && defined(_DEBUG)
#define new DEBUG_NEW
#endif

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
    string tabs(size_t indent)
    {
        string res; 
        for (size_t i=0; i<indent; i++) res += "\t";

        return res;
    }
}

namespace utility { namespace parser {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
CFBoolean::CFBoolean(): m_value(false) 
{
}

CFBoolean::CFBoolean(bool v): m_value(v) 
{
}

string CFBoolean::to_xml(uint8_t indent)
{ 
    string value = m_value ? "<true/>\n" : "<false/>\n";

    return tabs(indent) + value;
}

void CFBoolean::to_bin(vector<uint8_t>& /*blist*/)
{
}

uint32_t CFBoolean::count_objects()
{ 
    return 1; 
}

bool CFBoolean::value()
{
    return m_value;
}

////////////////////////////////////////////////////////////////////////////////
//
// NOTICE: need template for various integer type
//
////////////////////////////////////////////////////////////////////////////////
CFInteger::CFInteger(): m_value(0)
{
}

CFInteger::CFInteger(int64_t v): m_value(v)
{
}

string CFInteger::to_xml(uint8_t indent)
{ 
    using namespace boost;

    return tabs(indent) + boost::str(format("<integer>%s</integer>\n") % m_value);
}

void CFInteger::to_bin(vector<uint8_t>& /*blist*/)
{
}

uint32_t CFInteger::count_objects()
{ 
    return 1; 
}

int64_t CFInteger::value()
{
    return m_value;
}

int64_t CFInteger::as_integer(CFType t)
{
    CFInteger* res = boost::get<CFInteger>(&t);

    return res ? res->value() : 0;
}

////////////////////////////////////////////////////////////////////////////////
//
// 
//
////////////////////////////////////////////////////////////////////////////////
CFReal::CFReal(): m_value(0.0)
{
}

CFReal::CFReal(double v): m_value(v)
{
}

string CFReal::to_xml(uint8_t indent)
{ 
    using namespace boost;

    return boost::str(format("%s<real>%f</real>\n") % tabs(indent) % m_value);
}

string CFReal::to_s()
{
    using namespace boost;

    return boost::str(format("%f") % m_value);
}

void CFReal::to_bin(vector<uint8_t>& /*blist*/)
{
}

uint32_t CFReal::count_objects()
{ 
    return 1; 
}

double CFReal::value() const
{
    return m_value;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
CFUnicodeString::CFUnicodeString() : m_value(L"")
{
}

CFUnicodeString::CFUnicodeString(wstring const& str) : m_value(str)
{
}

uint32_t CFUnicodeString::count_objects()
{ 
    return 1; 
}

string CFUnicodeString::to_xml(uint8_t /*indent*/)
{
    return "";
}

void CFUnicodeString::to_bin(vector<uint8_t>& /*blist*/)
{
}

uint32_t CFUnicodeString::size()
{
    return uint32_t(m_value.size());
}

wstring CFUnicodeString::value()
{
    return m_value;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
CFString::CFString() : m_value("")
{
}

CFString::CFString(char const* str) : m_value(str)
{
}

CFString::CFString(string const& str) : m_value(str)
{
}

string CFString::to_xml(uint8_t indent)
{
    string str = boost::replace_all_copy(m_value, "\"", "&quot");
    boost::replace_all(str, "<", "&lt");
    boost::replace_all(str, ">", "&gt");
    boost::replace_all(str, "&", "&amp");
    return tabs(indent) + string("<string>") + str + string("</string>\n");
}

void CFString::to_bin(vector<uint8_t>& /*blist*/)
{
}

uint32_t CFString::count_objects()
{ 
    return 1; 
}

uint32_t CFString::size()
{
    return uint32_t(m_value.size());
}

string CFString::value()
{
    return m_value;
}

string CFString::as_string(CFType t)
{
    CFString *s = boost::get<CFString>(&t);
    if (s)
        return s->value();

    return string();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
CFDate::CFDate()
{
    m_value = 0;
}

CFDate::CFDate(double v)
{
    m_value = v;
}

string CFDate::to_xml(uint8_t indent)
{
    using namespace boost;

    return tabs(indent) + boost::str(format("<date>%1%</date>\n") % to_s());
}

string CFDate::to_s()
{
    char buf[32] = { 0 };

    // REMARK
    // t가 아이폰의 epoch에 기준을 두고 있다는 가정의 코드

    // mac의 epoch는 2001/1/1을 기준으로 한다.
    // unix timestamp는 1970/1/1을 기준으로 하므로 1970/1/1 ~2001/1/1 만큼의 초를 더한다.
    // (31*365 + 8:leap) * 24 * 60 * 60 = 978307200
    // 반면 FILETIME은 1601/1/1을 기준으로 100-nanosecond단위의 시간

    time_t t = (time_t)m_value; t += 978307200;
    if (t <= 0)
        return "";

    std::tm* g = gmtime(&t);
    if (g == nullptr)
        return "";

    strftime(buf, 32, "%Y-%m-%dT%H:%M:%SZ", g);

    return string(buf);
}

void CFDate::to_bin(vector<uint8_t>& /*blist*/)
{
}

uint32_t CFDate::count_objects()
{ 
    return 1; 
}

uint32_t CFDate::size()
{
    return 4;
}

// REFACTOR
string CFDate::value()
{
    return to_s();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
CFData::CFData() : m_value("")
{
}

CFData::CFData(string const& str) : m_value(str)
{
}

string CFData::to_xml(uint8_t indent)
{
	using namespace utility::codec;

    string space = tabs(indent);

    return space + string("<data>\n") + 
        base64::encode(m_value) + string("\n") + 
        space + string("</data>\n");
}

void CFData::to_bin(vector<uint8_t>& /*bplist*/)
{
}

uint32_t CFData::count_objects()
{ 
    return 1; 
}

uint32_t CFData::size()
{
    return uint32_t(m_value.size());
}


string CFData::value()
{
    return m_value;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
CFArray::CFArray()
{
}

string CFArray::to_xml(uint8_t indent)
{
    string space = tabs(indent);
    string res   = space + "<array>\n";

    for (size_t i=0; i<m_value.size(); i++)
    {
        xml_builder::visitor_type visitor;
        visitor.set_indent(indent+1);
        apply_visitor(visitor, m_value[i]);
        res += visitor.result();
    }

    res += space + "</array>\n";

    return res;
}

void CFArray::to_bin(vector<uint8_t>& /*blist*/)
{
}

uint32_t CFArray::count_objects()
{ 
    return 1; 
}

CFArray& CFArray::add(CFType const& value)
{
    m_value.push_back(value);
    return *this;
}

CFArray& CFArray::add(int value)
{
    m_value.push_back(CFInteger(value));
    return *this;
}

CFArray& CFArray::add(int64_t value)
{
    m_value.push_back(CFInteger(value));
    return *this;
}

CFArray& CFArray::add(char const* value)
{
    m_value.push_back(CFString(string(value)));
    return *this;
}

CFArray& CFArray::add(string const& value)
{
    m_value.push_back(CFString(value));
    return *this;
}

CFArray& CFArray::add_date(double value)
{
    m_value.push_back(CFDate(value));
    return *this;
}

void CFArray::push_back(CFType const& value)
{
    m_value.push_back(value);
}

size_t CFArray::size()
{
    return m_value.size();
}

CFArray& CFArray::reset()
{
    m_value.clear();
    return *this;
}

CFType& CFArray::operator[] (uint32_t index)
{
    return m_value[index];
}

vector<string> CFArray::as_strings(CFType t)
{
    vector_enumerator<string>::visitor_type v;
    boost::apply_visitor(v, t);

    return v.result();
}

vector<int64_t> CFArray::as_integers(CFType t)
{
    vector_enumerator<int64_t>::visitor_type v;
    boost::apply_visitor(v, t);

    return v.result();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
CFDictionary::CFDictionary()
{}

string CFDictionary::to_xml(uint8_t indent)
{
    string space = tabs(indent);
    string res = space + "<dict>\n";

    for (size_t i=0; i<m_value.size(); i++)
    {
        res += tabs(indent+1) + string("<key>") + m_value[i].first + "</key>\n";
        xml_builder::visitor_type visitor;
        visitor.set_indent(indent+1);
        apply_visitor(visitor, m_value[i].second);
        res += visitor.result();
    }

    res += space + "</dict>\n";

    return res;
}

void CFDictionary::to_bin(vector<uint8_t>& /*blist*/)
{
}

CFDictionary& CFDictionary::add(CFType key, CFType value)
{
    CFString* key_str = boost::get<CFString>(&key);
    if (key_str != NULL)
        m_value.push_back(make_pair(key_str->value(), value));

    return *this;
}

CFDictionary& CFDictionary::add(char const* key, CFType b)
{
    m_value.push_back(make_pair(string(key), b));
    return *this;
}

CFDictionary& CFDictionary::add(char const* key, char const* value)
{
    this->add(string(key), string(value));
    return *this;
}

CFDictionary& CFDictionary::add(string const& key, string const& value)
{
    m_value.push_back(make_pair(key, CFString(value)));

    return *this;
}

uint32_t CFDictionary::count_objects()
{ 
    return 1; 
}

CFDictionary CFDictionary::as_dictionary(CFType t)
{
    CFDictionary* res = boost::get<CFDictionary>(&t);

    return res ? *res : CFDictionary();
}

map<string, string> CFDictionary::as_strings(CFType t)
{
    map_enumerator<string, string>::visitor_type v;
    boost::apply_visitor(v, t);

    return v.result();
}

map<string, int64_t> CFDictionary::as_integers(CFType t)
{
    map_enumerator<string, int64_t>::visitor_type v;
    boost::apply_visitor(v, t);

    return v.result();
}

CFDictionary::iterator CFDictionary::find(std::string const& key)
{
    iterator it = m_value.begin();
    while (it != m_value.end())
    {
        if (it->first == key) break;
        ++it;
    }

    return it;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} 
}
#if 0
#include <iostream>

using namespace std;

struct value_adder
{
    typedef boost::mpl::vector<CFInteger, CFString, CFArray> filter;
    typedef generic_visitor<value_adder, filter> visitor_type;

    value_adder() : m_total(0)
    {}

    void operator() (CFString& s) const
    {
        // cout << s.value();
    }

    void operator() (CFInteger& i) const
    {
        // cout << i.value() << " ";
        m_total += i.value();
    }

    void operator() (CFArray& v) const
    {
        cout << "( ";
        visitor_type visitor;

        for (size_t i=0; i<v.size(); i++) apply_visitor(visitor, v.m_value[i]);
        m_total += visitor.result();

        cout << ") ";
    }

    int result()
    {
        return m_total;
    }

private:
    mutable int m_total;
};

CFType return_int()
{
    CFInteger i(1);

    return i;
}

int main(int argc, char * const argv[])
{
    CFType root;

    CFArray b;
    for (int i=0; i<10; i++) 
        b.add(CFInteger(i));

    b.add(b);
    CFArray c;
    c.add(CFInteger(100));
    c.add(CFInteger(200));
    c.add(CFString("leech"));
    c.add(c);
    c.add(CFInteger(50));
    c.add(c);
    root = c;

    value_adder::visitor_type v;
    boost::apply_visitor(v, root);

    cout << endl << v.result() << endl;

    return_int();

    return 0;
}

#endif

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
