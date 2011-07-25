#include "http_client.h"

#include <boost/foreach.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/exceptions.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/format.hpp>

#include <vector>
#include <cstdlib>

using namespace std;
using namespace boost;

using boost::property_tree::ptree;

class Picture
{
public:
  Picture(ptree& tree)
  {
    m_id     = tree.get<string>("id");
    m_name   = tree.get<string>("PicName");
    m_loc    = tree.get<string>("LOC");
    m_tloc   = tree.get<string>("TLOC");
    m_size   = tree.get<string>("Size");
    m_last_modified = tree.get<string>("LastModified");
    m_album_name    = tree.get<string>("AlbumName");
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

private:
  string m_id, m_name, m_loc, m_tloc, m_size, m_last_modified;
  string m_album_name, m_width, m_height;
  string m_contents;
};

class Motoroi
{
public:
  Motoroi(char const* host, char const* port="8080")
    : m_http(host, port, 600)
  {
  }

  ~Motoroi()
  {
    for (int i=0; i<m_pictures.size(); ++i)
      delete m_pictures[i];
  }

  vector<Picture*> prepare_pictures()
  {
    auto r = m_http.get("/personalportal/pictureview?c=0&blk=1");
    assert(r.first);

    stringstream ss(r.second);
    ptree pt; read_json(ss, pt);
    BOOST_FOREACH(auto &v, pt.get_child("RESPONSE"))
      m_pictures.push_back(new Picture(v.second));

    int total_blocks = boost::lexical_cast<int>(pt.get<string>("TotalBlocks"));
    for (size_t i=2; i<=total_blocks; i++)
    {
      string q = str(format("/personalportal/pictureview?c=0&blk=%d") % i);
      auto r1 = m_http.get(q.c_str());
      assert(r1.first);

      stringstream ss1(r1.second);
      ptree pt1; read_json(ss1, pt1);
      BOOST_FOREACH(auto &v, pt1.get_child("RESPONSE"))
        m_pictures.push_back(new Picture(v.second));
    }

    for (size_t i=0; i<m_pictures.size(); i++)
    {
      auto r2 = m_http.get(m_pictures[i]->location().c_str());
      assert(r2.first);
      m_pictures[i]->assign(r2.second);
    }

    return m_pictures;
  }

private:
  HTTPClient m_http;
  vector<Picture*> m_pictures;
};

int main(int argc, char const* argv[])
{
  Motoroi m("192.168.1.7");
  auto pictures = m.prepare_pictures();

  cout << "final count: " << pictures.size() << endl;

  for (auto i=0; i<pictures.size(); ++i)
    pictures[i]->save_to(".");

  return 0;
}
