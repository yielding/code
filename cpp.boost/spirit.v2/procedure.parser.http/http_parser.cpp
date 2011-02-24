#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/fusion/include/adapt_struct.hpp>

#include <iostream>
#include <string>
#include <complex>
#include <vector>
#include <map>

namespace qi    = boost::spirit::qi;
namespace ascii = boost::spirit::qi::ascii;

struct printer
{
  template <typename T>
  void operator() (T const& data, qi::unused_type, qi::unused_type) const
  {
    std::cout << "[in printer] ";
    std::cout << data << std::endl;
  }
};

namespace http
{
  using namespace std;
  
  struct header
  {
    string name, value;
  };

  ostream& operator << (ostream& out, header const& h)
  {
    out << "name    : " << h.name  << " ";
    out << "value   : " << h.value << "\n";
    
    return out;
  }
  
  struct request
  {
    string method;
    string abs_path;
    string http_version;
    string query;
    string host;
    vector<header> headers;
    string post_data;
  };
  
  ostream& operator << (ostream& out, request const& req)
  {
    out << "\n";
    out << "method  : " << req.method << "\n";
    out << "uri     : " << req.abs_path << "\n";
    out << "version : " << req.http_version << "\n";
    out << "host    : " << req.host << "\n";
    out << "query   : " << req.query << "\n";
    out << "post_data : " << req.post_data << "\n";
    
    for (size_t i=0; i<req.headers.size(); ++i)
      out << req.headers[i];

    return out;
  }
}

// http::header를 vector에 넣기 위해서 즉, 
// push_back(ref(headers) = _1) 표현을 위해서 만들어준다.
BOOST_FUSION_ADAPT_STRUCT(http::header,
  (std::string, name)
  (std::string, value)
)

bool parse_http_request(std::string& in, http::request& req)
{
  using namespace qi;
  using boost::phoenix::ref;
  using boost::phoenix::push_back;
  
  typedef std::string::const_iterator Iterator;
  
  rule<Iterator>
    crlf, http_request;
  
  rule<Iterator, http::header()>
    http_header;
    
  rule<Iterator, locals<bool>, std::string()> // remark: important trick
    toplabel, domainlabel;
  
  rule<Iterator, std::string()>
    query, method, http_version,
    abs_path, path_segments, segment, pchar, param,
    mark, escaped, uric, unreserved, reserved,
    net_path, uric_no_slash, scheme,
    request_line, request_uri, absolute_uri, hier_part, opaque_part,
    hostname,userinfo, ipv4address, host, port, hostport,
    server, reg_name, authority,
    header_name, header_value;
  
  crlf
    = lit("\r\n");
  
  method
    %= string("OPTIONS") | "GET"| "HEAD" | "POST" | "PUT" | "DELETE" | "TRACE" | "CONNECT";
  
  mark
    %= string("-") | "_" | "." | "!" | "~" | "*" | "\'" | "(" | ")";
  
  unreserved
    %= alnum | mark;
  
  reserved
    %= string(";") | "/" | "?" | "|" |"@" | "&" | "=" | "+" | "$" | ",";
  
  escaped
    %= string("%") >> xdigit >> xdigit;
  
  uric_no_slash
    %= raw[ unreserved | escaped | ';' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',' ];

  uric
    %= reserved | unreserved | escaped;
  
  userinfo
    %= raw[ *(unreserved | escaped | ';' | ':' | '&' | '=' | '+' | '$' | ',') ];
  
  toplabel
    %= raw[ alpha[_a=true] >> *(alnum[_a=true] | char_('-')[_a=false]) >> eps(_a) ];
  
  domainlabel
    %= raw[ alnum[_a=true] >> *(alnum[_a=true] | char_('-')[_a=false]) >> eps(_a) ];
  
  hostname
    %= raw[ *(domainlabel >> '.') >> toplabel >> -char_('.') ];
  
  uint_parser<unsigned, 10, 1, 3> decimal_byte;
  
  ipv4address
    = decimal_byte >> '.' >> decimal_byte >> '.' >> decimal_byte >> '.' >> decimal_byte;
  
  host
    %= hostname | ipv4address;
  
  port
    %= raw[ uint_ ];
  
  hostport
    %= raw[ host >> -(':' >> port) ];
  
  server
    %= raw[ -( -(userinfo >> '@') >> hostport) ];
  
  reg_name
    %= raw[ +(unreserved | escaped | '$' | ',' | ';' | ':' | '@' | '&' | '=' | '+') ] ;
  
  authority
    %= server | reg_name ;
  
  net_path
    %= raw[ "//" >> authority >> -(abs_path) ];
  
  hier_part
    %= raw[ (net_path | abs_path) >> -('?' >> query) ];
  
  scheme
    %= raw[ alpha >> *(alpha | digit | '+' | '-' | '.') ];
  
  opaque_part
    %= raw[ uric_no_slash >> *uric ];
  
  absolute_uri
    %= raw[ scheme >> ':' >> (hier_part | opaque_part) ];
  
  // REMARK (여기에서 모든 문제가 풀렸다)
  // string of string을 vector<string>으로 생각하지 않고 string += string
  query
    = *(uric [ref(req.query) += _1]);
  
  pchar
    %= raw[ unreserved | escaped | ':' | '@' | '&' | '=' | '+' | '$' | ',' ];
  
  param
    %= raw[ *pchar ];
  
  segment
    %= raw[ *(pchar) >> *(';' >> param) ];
  
  path_segments
    %= raw[ segment >> *('/' >> segment) ];
  
  abs_path
    =  lit("/")
    >> path_segments [ref(req.abs_path) = _1];
  
  request_uri
    = char_('*')
    | absolute_uri
    | abs_path >> -(char_('?') >> query)
    | authority;
  
  http_version
    %= raw[ "HTTP/" >> digit >> "." >> digit ];
  
  request_line
    =  method                 [ref(req.method) = _1]
    >> ' ' >> request_uri
    >> ' ' >> http_version    [ref(req.http_version) = _1]
    >> crlf;
  
  header_name
    %= raw[ +(alnum | '-') ];
  
  header_value
    %= raw[ *(print| ' ' | '\t') ];
  
  http_header
    %= header_name >> ':' >> *(lit(' ')) >> header_value;
  
  http_request
    =  request_line
    >> *(http_header[push_back(ref(req.headers), _1)] >> crlf)
    >> crlf;
  
  std::string::const_iterator beg = in.begin(), end = in.end();
  bool res = parse(beg, end, http_request);
  if (!res) 
    return false;
    
  if (req.method == "POST") for (size_t i=0; i<req.headers.size(); ++i)
  {
    if (req.headers[i].name == "Content-Length")
    {
      size_t post_data_length = atoi(req.headers[i].value.c_str());
      if (post_data_length > 0)
        req.post_data = in.substr(in.length() - post_data_length, post_data_length);
    }
  }
  
  return res;
}

int main(int argc, char const *argv[])
{
  http::request request;
  std::string req_str;

  req_str = "GET http://192.168.10.12:8080/index.html?userid=joe&password=guessme HTTP/1.1\r\n"
            "Host: www.mysite.com\r\n"
            "User-Agent: Mozilla/4.0\r\n"
            "\r\n"
            ;

  req_str = "POST /login.jsp HTTP/1.1\r\n"
            "Host: www.mysite.com\r\n"
            "User-Agent: Mozilla/4.0\r\n"
            "Content-Length: 27\r\n"
            "Content-Type: application/x-www-form-urlencoded\r\n"
            "\r\n"
            "userid=joe&password=guessme"
            ;

  bool r = parse_http_request(req_str, request);
  if (r)
    std::cout << request;
  else
    std::cout << "error on parsing!";

  return 0;
}
