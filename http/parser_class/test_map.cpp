#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
//#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/fusion/include/std_pair.hpp>

#include <iostream>
#include <string>
#include <map>

namespace http
{
  typedef std::map<std::string, std::string> header_map;

  struct request
  {
    std::string method;
    header_map headers;
  };
}

BOOST_FUSION_ADAPT_STRUCT(http::request,
  (std::string, method)
  (http::header_map, headers)
);

namespace qi    = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;

int main(int argc, char const *argv[])
{
  using namespace qi;
  using namespace ascii;
  using boost::phoenix::ref;
  using boost::phoenix::at_c;

  std::string req_str;
  
  req_str = "POST /login.jsp HTTP/1.1\r\n"
            "Host: www.mysite.com\r\n"
            "User-Agent: Mozilla/4.0\r\n"
            "Content-Length: 27\r\n"
            "Content-Type: application/x-www-form-urlencoded\r\n"
            "\r\n"
            "userid=joe&password=guessme"
            ;   
                                      
  req_str = "POST\r\n"
            "Host: www.mysite.com\r\n"
            "User-Agent: Mozilla/4.0\r\n"
            ;

  std::string::const_iterator beg = req_str.begin();
  std::string::const_iterator end = req_str.end();
  http::request request;

  std::string name, value;

  typedef std::string::const_iterator Iterator;
                
  rule<Iterator, http::request()>
    start;
  
  rule<Iterator, void(http::header_map&)>
    http_headers;
    
  rule<Iterator, std::pair<std::string, std::string>()>
    http_header;

  rule<Iterator, std::string()>
    header_name, header_value, method;

  rule<Iterator>
    crlf;
           
  crlf
    = lit("\r\n");
    
  method
    %= string("GET") | "POST";
    
  header_name
    %= raw[ +(alnum | '-') ];

  header_value
    %= raw[ *(print | ' ' | '\t') ];

  http_header 
    %= header_name
    >> ':' >> *(lit(' ')) 
    >> header_value >> crlf
    ;

  http_headers
    %= *(http_header[ insert(_r1, _1) ])
    ;
    
  start
    %= method >> crlf
    >> http_headers(at_c<1>(_val)) // 이 문법이 전부 예제에 있는거다. mini_xml3.cpp:168 참고
    ;

  bool r = qi::parse(beg, end, start, request);

  if (r && beg == end)
  {
    std::cout << "ok\n";
    std::cout << "host : " << request.headers["Host"] << "\n";
    std::cout << "Agent: " << request.headers["User-Agent"] << "\n";
  }
  else
  {
    std::cout << "error on parsing!";
  }

  return 0;
}