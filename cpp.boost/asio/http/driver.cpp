#include "http_client.h"

#include <boost/foreach.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/exceptions.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/format.hpp>
#include <boost/shared_ptr.hpp>

#include <vector>
#include <cstdlib>

using namespace std;
using namespace boost;

using boost::property_tree::ptree;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class Picture
{
public:
  Picture(ptree& tree)
  {
    m_id = tree.get<string>("id");
    m_name = tree.get<string>("PicName");
    m_loc = tree.get<string>("LOC");
    m_tloc = tree.get<string>("TLOC");
    m_size = tree.get<string>("Size");
    m_last_modified = tree.get<string>("LastModified");
    m_album_name = tree.get<string>("AlbumName");
    m_width  = tree.get<string>("Width");
    m_height = tree.get<string>("Height");
  }

  void assign(string& contents)
  {
    m_contents.swap(contents);
  }

  void save_to(string const& path)
  {
    ofstream out;
    string filename = str(format("%s/%s") % path % m_name);
    cout << "saving: " << filename << endl;
    out.open(filename.c_str(), ios_base::binary);
    out.write(m_contents.data(), m_contents.length());
  }

  string location()
  {
    return m_loc;
  }

  static string query(uint32_t index)
  {
    return str(format("/personalportal/pictureview?c=0&blk=%d") % index);
  }

private:
  string m_id, m_name, m_loc, m_tloc, m_size, m_last_modified;
  string m_album_name, m_width, m_height;
  string m_contents;
};

typedef vector<boost::shared_ptr<Picture> > Pictures;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class MobileInbox
{
public:
  MobileInbox(ptree& tree)
  {
    m_type = tree.get<string>("MsgType");
    m_msg_id = tree.get<string>("MsgId");
    m_name = tree.get<string>("Name");
    m_address = tree.get<string>("Address");
    m_phone_type = tree.get<string>("PhoneType");
    m_dir_id = tree.get<string>("DirId");
    m_timestamp = tree.get<string>("TimeStamp");
    if (m_type == "0")
      m_call_duration = tree.get<string>("CallDuration");
    else
      m_message_body = tree.get<string>("MsgBody");
    m_read_state = tree.get<string>("MsgReadState");
  }

  string to_s()
  {
    return m_type == "0"
      ? str(format("%s: %s") % m_name % m_call_duration)
      : str(format("%s: %s") % m_name % m_message_body);
  }

  static string query(uint32_t index)
  {
    return str(format("/personalportal/mobileinbox?c=0&d=4&blk=%d") % index);
  }

private:
  string m_type, m_msg_id, m_name, m_address, m_phone_type, m_dir_id;
  string m_timestamp, m_message_body, m_call_duration, m_read_state;
};

typedef vector<boost::shared_ptr<MobileInbox> > MobileInboxes;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class CallLog
{
public:
  CallLog(ptree& tree)
  {
    m_type = tree.get<string>("MsgType");
    m_msg_id = tree.get<string>("MsgId");
    m_name = tree.get<string>("Name");
    m_address = tree.get<string>("Address");
    m_phone_type = tree.get<string>("PhoneType");
    m_dir_id = tree.get<string>("DirId");
    m_timestamp = tree.get<string>("TimeStamp");
    m_call_duration = tree.get<string>("CallDuration");
    m_read_state = tree.get<string>("MsgReadState");
  }

  string to_s()
  {
    return str(format("%s: %s") % m_name % m_call_duration);
  }
  
  static string query(int index)
  {
      return str(format("/personalportal/calllog?c=0&d=4&blk=%d") % index);
  }

private:
  string m_type, m_msg_id, m_name, m_address, m_phone_type, m_dir_id;
  string m_timestamp, m_call_duration, m_read_state;
};

typedef vector<boost::shared_ptr<CallLog> > CallLogs;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SMS
{
public:
  SMS(ptree const& tree)
  {
    m_type = tree.get<string>("MsgType");
    m_msg_id = tree.get<string>("MsgId");
    m_name = tree.get<string>("Name");
    m_address = tree.get<string>("Address");
    m_phone_type = tree.get<string>("PhoneType");
    m_dir_id = tree.get<string>("DirId");
    m_timestamp = tree.get<string>("TimeStamp");
    m_message_body = tree.get<string>("MsgBody");
    m_read_state = tree.get<string>("MsgReadState");
  }

  string to_s()
  {
    return str(format("%s: %s") % m_name % m_message_body);
  }

  static string query(int index)
  {
    return str(format("/personalportal/sms?c=0&d=4&blk=%d") % index);
  }

private:
  string m_type, m_msg_id, m_name, m_address, m_phone_type, m_dir_id;
  string m_timestamp, m_message_body, m_read_state;
};

typedef vector<boost::shared_ptr<SMS> > SMSs;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class Motoroi
{
public:
  Motoroi(char const* host, char const* port="8080", int time=10)
    : m_http(host, port, time)
  {}

  ~Motoroi()
  {}

  MobileInboxes prepare_mobile_inbox()
  {
    MobileInboxes inbox;

    for (auto total_blocks=1, curr=1; curr<=total_blocks; curr++)
      total_blocks = query_data(MobileInbox::query(curr),
          "LoadMobileInboxResp.MsgDesc", "LoadMobileInboxResp.TotalBlocks", inbox);

    return inbox;
  }

  CallLogs prepare_calllog()
  {
    CallLogs calllogs;

    for (auto total_blocks=1, curr=1; curr<=total_blocks; curr++)
      total_blocks = query_data(CallLog::query(curr),
          "LoadCallLogResp.MsgDesc", "LoadCallLogResp.TotalBlocks", calllogs);

    return calllogs;
  }

  SMSs prepare_sms()
  {
    SMSs sms;

    for (auto total_blocks=1, curr=1; curr<=total_blocks; curr++)
      total_blocks = query_data(SMS::query(curr),
          "LoadSMSListResp.MsgDesc", "LoadSMSListResp.TotalBlocks", sms);

    return sms;
  }

  Pictures prepare_pictures()
  {
    Pictures pictures;

    for (auto total_blocks=1, curr=1; curr<=total_blocks; curr++)
      total_blocks = query_data(Picture::query(curr), "RESPONSE", "TotalBlocks", pictures);

    for (auto i=0; i<pictures.size(); i++)
    {
      string result = m_http.get(pictures[i]->location().c_str());
      assert(!result.empty());
      pictures[i]->assign(result);
    }

    return pictures;
  }

private:
  template <typename Item> 
  uint32_t query_data(string const& query, string const& root, string const& count, 
      vector<boost::shared_ptr<Item> >& c)
  {
    string result = m_http.get(query.c_str()); 
    if (result.empty())
      throw std::runtime_error("Http.get error");

    stringstream ss(result);
    ptree pt; read_json(ss, pt);
    BOOST_FOREACH(auto &v, pt.get_child(root))
      c.push_back(boost::shared_ptr<Item>(new Item(v.second)));

    return boost::lexical_cast<int>(pt.get<string>(count));
  }

private:
  HTTPClient m_http;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char const* argv[])
{
<<<<<<< HEAD
  Motoroi m("192.168.1.7");

  auto pictures = m.prepare_pictures();
  cout << "final count: " << pictures.size() << endl;
  for (auto i=0; i<pictures.size(); ++i)
  {
    pictures[i]->save_to(".");
  }

//  auto sms = m.prepare_sms();
//  cout << "final count: " << sms.size() << endl;
//  for (auto i=0; i<sms.size(); ++i)
//    cout << sms[i]->to_s() << endl;

//  auto calllog = m.prepare_calllog();
//  cout << "final count: " << calllog.size() << endl;
//  for (auto i=0; i<calllog.size(); ++i)
//    cout << calllog[i]->to_s() << endl;

//  auto inbox = m.prepare_mobile_inbox();
//  cout << "final count: " << inbox.size() << endl;
//  for (auto i=0; i<inbox.size(); ++i)
//    cout << inbox[i]->to_s() << endl;
=======
  Motoroi m("192.168.1.7", "8080", 10);

  try
  {
    auto pictures = m.prepare_pictures();
    cout << "final count: " << pictures.size() << endl;
    for (auto i=0; i<pictures.size(); ++i) pictures[i]->save_to(".");

    auto sms = m.prepare_sms();
    cout << "final count: " << sms.size() << endl;
    for (auto i=0; i<sms.size(); ++i) cout << sms[i]->to_s() << endl;

    auto calllog = m.prepare_calllog();
    cout << "final count: " << calllog.size() << endl;
    for (auto i=0; i<calllog.size(); ++i) cout << calllog[i]->to_s() << endl;

    auto inbox = m.prepare_mobile_inbox();
    cout << "final count: " << inbox.size() << endl;
    for (auto i=0; i<inbox.size(); ++i) cout << inbox[i]->to_s() << endl;
  }
  catch(std::runtime_error& e)
  {
    cout << "got an exception: " << e.what() << endl;
  }
>>>>>>> 69c8cf112b566ba48ec8acb9c1acf9ebbaeab18c

  return 0;
}
