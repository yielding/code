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
class SSHHostListImpl;

class SSHHostList
{
public:
    SSHHostList();
    ~SSHHostList();

    bool init_with_file(std::string const& file);
    bool init_with_string(std::string const& str);
    bool init_with_stream(std::istream& stream);

    auto count() const  -> uint32_t;
    auto nth(int index) -> boost::tuple<std::string, std::string, std::string>;

private:
    SSHHostListImpl* _pimpl;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
