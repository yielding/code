#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/fusion/include/std_pair.hpp>

#include <iostream>
#include <string>
#include <map>

namespace qi    = boost::spirit::qi;
namespace ascii = boost::spirit::qi::ascii;

namespace http
{      
  typedef std::map <std::string, std::string> header_map;
  
  struct request
  {
    int content_length()
    { 
      header_map::const_iterator i = m_headers.find("Content-Length");
      return (i == m_headers.end()) 
        ? 0
        : atoi(i->second.c_str());
    } 
    
    std::string query()
    {  
      std::size_t found = m_abs_path.find("?");
      return (found == std::string::npos)
        ? ""
        : m_abs_path.substr(found+1);
    }
    
    std::string post_data(std::string const& in)
    {            
      return (m_method == "POST")
        ? in.substr(in.length() - content_length(), content_length())
        : "";
    }
    
    std::string m_method;
    std::string m_abs_path;
    std::string m_http_version;
    std::string m_query;
    std::string m_host;
    std::string m_post_data;
    header_map  m_headers;
  };
}

BOOST_FUSION_ADAPT_STRUCT(http::request,
  (std::string,      m_method)
  (std::string,      m_abs_path)
  (std::string,      m_http_version)
  (http::header_map, m_headers)
);

template <typename Iterator>
struct http_request_parser: public qi::grammar<Iterator, http::request()>
{
public:
  http_request_parser() : http_request_parser::base_type(http_request)
  {
    using namespace qi;

    using boost::phoenix::ref; 
    using boost::phoenix::at_c;
    using boost::phoenix::insert; 

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
      %= raw[ reserved | unreserved | escaped ];

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
      %= raw[ +(unreserved | escaped | '$' | ',' | ';' | ':' | '@' | '&' | '=' | '+') ];

    authority
      %= server | reg_name;

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

    query
      %= raw[ *uric ];

    pchar
      %= raw[ unreserved | escaped | ':' | '@' | '&' | '=' | '+' | '$' | ',' ];

    param
      %= raw[ *pchar ];

    segment
      %= raw[ *(pchar) >> *(';' >> param) ];

    path_segments
      %= raw[ segment >> *('/' >> segment) ];

    abs_path
      %= string("/")
      >> path_segments;

    request_uri
      %= string("*")
      | absolute_uri
      | abs_path >> -(char('?') >> query)
      | authority
      ;

    http_version
      %= raw[ "HTTP/" >> digit >> "." >> digit ];
    
    header_name
      %= raw[ +(alnum | '-') ];

    header_value
      %= raw[ *(print| ' ' | '\t') ];

    http_header
      %= header_name >> ':' >> *(lit(' ')) >> header_value >> crlf;
                  
    http_headers
      %= *(http_header[insert(_r1, _1)]);
      
    http_request
      %= method       >> ' '
      >> request_uri  >> ' '
      >> http_version >> crlf
      >> http_headers(at_c<3>(_val))
      >> crlf
      ;
  }

private:
  qi::rule<Iterator, http::request()>
    http_request;                                          
    
  qi::rule<Iterator, void(http::header_map&)>
    http_headers;
    
  qi::rule<Iterator, std::pair<std::string, std::string>()>
    http_header;

  qi::rule<Iterator, qi::locals<bool>, std::string()> // remark: important trick
    toplabel, domainlabel;

  qi::rule<Iterator, std::string()>
    query, method, http_version,
    abs_path, path_segments, segment, pchar, param,
    mark, escaped, uric, unreserved, reserved,
    net_path, uric_no_slash, scheme,
    request_uri, absolute_uri, hier_part, opaque_part,
    hostname,userinfo, ipv4address, host, port, hostport,
    server, reg_name, authority,
    header_name, header_value;

  qi::rule<Iterator>
    crlf;
};

int main(int argc, char const *argv[])
{
  std::string req_str;
  
  req_str = "POST /login.jsp HTTP/1.1\r\n"
            "Host: www.mysite.com\r\n"
            "User-Agent: Mozilla/4.0\r\n"
            "Content-Length: 27\r\n"
            "Content-Type: application/x-www-form-urlencoded\r\n"
            "\r\n"
            "userid=joe&password=guessme"
            ;   
                          
//  req_str = "GET http://192.168.10.12:8080/index.html?userid=joe&password=guessme HTTP/1.1\r\n"
//            "Host: www.mysite.com\r\n"
//            "User-Agent: Mozilla/4.0\r\n"
//            "\r\n"
//            ;

  typedef std::string::const_iterator Iterator;
  typedef http_request_parser<Iterator> http_request_parser;
  http_request_parser g;
  
  http::request request;

  Iterator beg = req_str.begin(),
           end = req_str.end();

  // 이렇게 하면 procedure를 만들때 처럼 자유롭게 쓸 수 있겠다.
  // 클래스의 장점을 살리면서 (즉, 매 procedure 호출때마다 엄청나게 많은 룰들을 만드는걸 안하고)

  bool r = qi::parse(beg, end, g, request);
  if (r && beg == end)
  {                     
    std::cout << "ok\n";
    std::cout << request.content_length() << "\n";
    std::cout << request.query() << "\n";
  }
  else
    std::cout << "error on parsing!";

  return 0;
}
