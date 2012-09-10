#include "stdafx.h"
#include "SCPChannel.h"
#include "SSHSession.h"
#include "ScopeGuard.h"

#include <string>
#include <fstream>
#include <stdexcept>

#include <boost/format.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/phoenix.hpp>
#include <boost/filesystem.hpp>

using namespace std;
using namespace boost;
using boost::phoenix::arg_names::arg1;

namespace phx = boost::phoenix;
namespace  fs = boost::filesystem;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {
string assemble(vector<string>& compo)
{
    string path;
    for_each(compo.begin(), compo.end(), phx::ref(path) += ("/" + arg1));
    
    return path;
}

string make_path(string const& prefix, string const& file_name)
{
    string path = ends_with(prefix, "/\\")
    ? prefix.substr(0, path.length()-1)
    : prefix;
    
    //
    
    return path;
}

string normalize(string file_name)
{
#ifdef WIN32
    if (contains(file_name, ":"))
    {
        replace_all(file_name, ":", "_");
        file_name[1] = ':';
    }
    
    replace_all(file_name, "/", "\\");
#endif
    return file_name;
}

void create_path(string path)
{
    path = normalize(path);
    if (!fs::exists(path))
        fs::create_directory(path);
}
}

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SCPChannel::SCPChannel(SSHSession* session)
    : _ssh(session)
{
}

SCPChannel::~SCPChannel()
{
}

bool SCPChannel::save_file(string fullpath)
{
    fullpath = normalize(fullpath);
    ofstream ofs; ofs.open(fullpath.c_str(), ios_base::binary);
    if (!ofs.is_open())
    {
        ::ssh_scp_deny_request(_scp, "impossible");
        return false;
    }
    
    size_t total_size = ::ssh_scp_request_get_size(_scp);
    size_t total_read = 0;
    do
    {
        char buffer[8*1024] = { 0 };
        auto rc = ::ssh_scp_read(_scp, (void*)buffer, sizeof(buffer));
        if (rc == SSH_ERROR)
        {
            ::ssh_scp_deny_request(_scp,
                                    "can't read remote file (maybe due to apple's special file flag");
            ofs.close();
            fs::remove(fullpath);
            return false;
        }
        
        if (rc > 0)
        {
            ofs.write(buffer, rc);
            total_read += rc;
        }
    }
    while (total_read < total_size);
    
    return true;
}

bool SCPChannel::download(string const& from, string const& to)
{
    _scp = ::ssh_scp_new(_ssh->session(),
                         SSH_SCP_READ | SSH_SCP_RECURSIVE, from.c_str());
    if (_scp == NULL)
        throw std::runtime_error("SCP channel creation error");
    
    ON_BLOCK_EXIT(phx::bind(&ssh_scp_free, phx::ref(_scp)));
    
    if (::ssh_scp_init(_scp) != SSH_OK)
    {
        _ssh->error("scp.download: ");
        return false;
    }
    
    vector<string> dir_components;
    _discarded_files.clear();
    
    do
    {
        int rc = ::ssh_scp_pull_request(_scp);
        if (rc == SSH_ERROR)
        {
            ::ssh_scp_close(_scp);
            return dir_components.empty();
        }
        
        if (rc == SSH_SCP_REQUEST_NEWFILE)
        {
            auto fname = string(::ssh_scp_request_get_filename(_scp));  // file name
            auto rpath = assemble(dir_components) + "/" + fname;        // relative path
            auto fpath = to + rpath;                                    // full path
            
            // now prepared to get the input data
            ::ssh_scp_accept_request(_scp);
            
            if (!save_file(fpath))
                _discarded_files.push_back(rpath);
        }
        else if (rc == SSH_SCP_REQUEST_NEWDIR)
        {
            dir_components.push_back(string(::ssh_scp_request_get_filename(_scp)));
            auto path = to + assemble(dir_components);
            create_path(path);
            
            ::ssh_scp_accept_request(_scp);
        }
        else if (rc == SSH_SCP_REQUEST_ENDDIR)
        {
            dir_components.pop_back();
        }
        else if (rc == SSH_SCP_REQUEST_WARNING)
        {
#if defined(DEBUG)
            cout << str(format("SSH_SCP_REQUEST_WARNING: %s\n")
                        % ::ssh_scp_request_get_warning(_scp));
#endif
        }
        else if (rc == SSH_SCP_REQUEST_EOF)
        {
#if defined(DEBUG)
            cout << "end of file\n";
#endif
            break;
        }
    }
    while(true);
    
    return true;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} // namespace ssh
} // namespace comm
} // namespace utility
