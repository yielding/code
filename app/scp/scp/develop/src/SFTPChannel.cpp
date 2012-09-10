#include "stdafx.h"
#include "SSHSession.h"
#include "SFTPChannel.h"

#include <stdexcept>
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/format.hpp>

using namespace std;
using namespace boost;
      namespace fs = boost::filesystem;

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SFTPChannel::SFTPChannel(SSHSession* session)
    : _ssh(session)
{
}

SFTPChannel::~SFTPChannel()
{
}

bool SFTPChannel::save_file_to(string const& fullpath)
{
    return false;
}
    
bool SFTPChannel::scan_dir(string const& from, Block block)
{
    auto sftp = ::sftp_new(_ssh->session());
    if (sftp == NULL)
        throw std::runtime_error("SFTP channel creation error");

    if (::sftp_init(sftp) != SSH_OK)
    {
        _ssh->error("sftp.init error: ");
        return false;
    }

    vector<string> dirs;
    dirs.push_back(from);
    
    do
    {
        auto path = dirs.back(); dirs.pop_back();
        auto dir_handle = ::sftp_opendir(sftp, path.c_str());
        if (!dir_handle)
        {
            _ssh->error("sftp.opendir error: ");
            return false;
        }

        sftp_attributes attr;
        while ((attr = sftp_readdir(sftp, dir_handle)))
        {
            auto name = string(attr->name);
            if (name == "." || name == "..")
                continue;
            
            auto is_dir = attr->type == 2;
            auto path_  = path + ("/" + string(attr->name));
            // string, u64, bool, u64, u32, u32 
            block(path_, attr->size, is_dir, attr->createtime, attr->mtime, attr->atime);
            if (is_dir)
                dirs.push_back(path_);
        }

        sftp_attributes_free(attr);

        if (!sftp_dir_eof(dir_handle))
        {
            _ssh->error("sftp.not.eof error: ");
            return false;
        }

        ::sftp_closedir(dir_handle);
    } 
    while (!dirs.empty());

    ::sftp_free(sftp);
    
    return true;
}

bool SFTPChannel::download_to(string const& from, MDFWriter mdf)
{
    auto sftp = ::sftp_new(_ssh->session());
    if (sftp == NULL)
        throw std::runtime_error("SFTP channel creation error");
    
    if (::sftp_init(sftp) != SSH_OK)
    {
        _ssh->error("sftp.init error: ");
        return false;
    }

    // O_RDONLY: 0x0000
    auto file = ::sftp_open(sftp, from.c_str(), 0x0000, 0);
    if (!file)
    {
        _ssh->error("sftp.open.file error: ");
        return false;
    }

    int len = 0;
    int const BUF_SIZE = 16 * 1024; 
    char buffer[BUF_SIZE] = { 0 };
    while ((len = (int)::sftp_read(file, buffer, BUF_SIZE)) > 0)
        mdf(buffer, len);

    if (len < 0)
    {
        _ssh->error("sftp.read error: ");
        return false;
    }

    ::sftp_close(file);
    ::sftp_free(sftp);
    
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
