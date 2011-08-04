#include "http_client.h"

#include <boost/foreach.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
// #include <boost/property_tree/exceptions.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/format.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/filesystem.hpp>

#include <vector>
#include <fstream>
#include <cstdlib>
#include <cassert>

using namespace std;
using namespace boost;
      namespace fs = boost::filesystem;

using boost::property_tree::ptree;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct FileInfo
{
  FileInfo(string const& p, uint32_t l) : path(p), length(l) {}

  string path;
  uint32_t length;
};

typedef vector<FileInfo> FileInfoList;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct Picture
{
  static string query(uint32_t index)
  {
    return str(format("/personalportal/pictureview?c=0&blk=%d") % index);
  }
};

struct MobileInbox
{
  static string query(uint32_t index)
  {
    return str(format("/personalportal/mobileinbox?c=0&d=4&blk=%d") % index);
  }
};

struct CallLog
{
  static string query(int index)
  {
    return str(format("/personalportal/calllog?c=0&d=4&blk=%d") % index);
  }
};

class SMS
{
public:
  static string query(int index)
  {
    return str(format("/personalportal/sms?c=0&d=4&blk=%d") % index);
  }
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class Motoroi
{
public:
  Motoroi(char const* host, char const* port="8080", int time=10, string const& bp=".")
    : m_http(host, port, time), m_base_path(bp)
  {
  }

  FileInfoList prepare_mobile_inbox()
  {
    FileInfoList fl;

    for (auto total_blocks=1, curr=1; curr<=total_blocks; curr++)
    {
      string result; tie(result, total_blocks) 
        = query_data(MobileInbox::query(curr), "LoadMobileInboxResp.TotalBlocks");

      string path = str(format("%s/mobile_inbox/minbox%04d.json") % m_base_path % curr);
      save_to(path, result);
      fl.push_back(FileInfo(path, result.size()));
    }

    return fl;
  }

  FileInfoList prepare_calllog()
  {
    FileInfoList fl;

    for (auto total_blocks=1, curr=1; curr<=total_blocks; curr++)
    {
      string result; tie(result, total_blocks) 
        = query_data(CallLog::query(curr), "LoadCallLogResp.TotalBlocks");

      string path = str(format("%s/calllog/calllog_%04d.json") % m_base_path % curr);
      save_to(path, result);
      fl.push_back(FileInfo(path, result.size()));
    }

    return fl;
  }

  FileInfoList prepare_sms()
  {
    FileInfoList fl;

    for (auto total_blocks=1, curr=1; curr<=total_blocks; curr++)
    {
      string result; tie(result, total_blocks) 
        = query_data(SMS::query(curr), "LoadSMSListResp.TotalBlocks");

      string path = str(format("%s/sms/sms_%04d.json") % m_base_path % curr);
      if (!save_to(path, result))
        cout << "error : " << path << endl;

      fl.push_back(FileInfo(path, result.size()));
    }

    return fl;
  }

  FileInfoList prepare_pictures()
  {
    FileInfoList fl;

    for (auto total_blocks=1, curr=1; curr<=total_blocks; curr++)
    {
      string result; tie(result, total_blocks) 
        = query_data(Picture::query(curr), "TotalBlocks");

      string path0 = str(format("%s/picture/picture_%04d.json") % m_base_path % curr);
      save_to(path0, result);
      fl.push_back(FileInfo(path0, result.size()));

      stringstream ss(result);
      ptree pt; read_json(ss, pt);
      BOOST_FOREACH(auto &v, pt.get_child("RESPONSE"))
      {
        string location = v.second.get<string>("LOC");
        string picture_name = v.second.get<string>("PicName");

        string path1   = str(format("%s/picture/%s") % m_base_path % picture_name);
        string picture = m_http.get(location.c_str());

        save_to(path1, picture);
        fl.push_back(FileInfo(path1, picture.size()));
      }
    }

    return fl;
  }

private:

  bool save_to(string const& path_, string const& contents)
  {
    fs::path p(path_);
    if (!p.is_absolute())
      return false;

    string parent = p.parent_path().native();
    if (!fs::exists(parent))
      if (!fs::create_directories(parent))
        return false;
  
    ofstream out;
    out.open(path_.c_str(), ios_base::binary);
    if (!out.is_open())
      return false;
  
    out.write(contents.data(), contents.length());
  
    return true;
  }

  pair<string, uint32_t> query_data(string const& query, string const& count) 
  {
    string result = m_http.get(query.c_str()); 

    if (result.empty())
      throw std::runtime_error("Http.get error");

    stringstream ss(result);
    ptree pt; read_json(ss, pt);
    uint32_t total_blocks = boost::lexical_cast<int>(pt.get<string>(count));

    return make_pair(result, total_blocks);
  }

private:
  HTTPClient m_http;
  string m_base_path;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char const* argv[])
{
  Motoroi m("192.168.1.3", "8080", 10, "/Users/yielding/code/cpp.boost/asio/http");

  try
  {
//    auto sms = m.prepare_sms();
//
//    for (auto it=sms.begin(); it!=sms.end(); ++it)
//    {
//      cout << it->path << endl;
//      assert(it->length == fs::file_size(it->path));
//    }

//    auto pictures = m.prepare_pictures();
//    for (auto it=pictures.begin(); it!=pictures.end(); ++it)
//    {
//      cout << it->path << endl;
//      assert(it->length == fs::file_size(it->path));
//    }

//    auto calls = m.prepare_calllog();
//    for (auto it=calls.begin(); it!=calls.end(); ++it)
//    {
//      cout << it->path << endl;
//      assert(it->length == fs::file_size(it->path));
//    }

//    auto mi = m.prepare_mobile_inbox();
//    for (auto it=mi.begin(); it!=mi.end(); ++it)
//    {
//      cout << it->path << endl;
//      assert(it->length == fs::file_size(it->path));
//    }
  }
  catch(std::runtime_error& e)
  {
    cout << "got an exception: " << e.what() << endl;
  }

  return 0;
}
