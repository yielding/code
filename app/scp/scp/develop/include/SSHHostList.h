#ifndef SSHHOSTLIST_H_3LS84YTK
#define SSHHOSTLIST_H_3LS84YTK

#include <string>
#include <istream>

#include <boost/tuple/tuple.hpp>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using std::string;

class SSHHostListImpl;

class SSHHostList
{
public:
    SSHHostList();
    ~SSHHostList();

    bool init_with_file(string const& file);
    bool init_with_string(string const& str);
    bool init_with_stream(std::istream& stream);

    auto count() const  -> uint32_t;
    auto nth(int index) -> boost::tuple<string, int, string, string, string>;

private:
    SSHHostListImpl* _pimpl;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
