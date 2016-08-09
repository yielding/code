#include <boost/spirit.hpp> 
#include <sstream>
#include <fstream>

using namespace std;

struct json_grammar 
  : public boost::spirit::grammar<json_grammar> 
{ 
  template <typename Scanner> 
  struct definition 
  { 
    boost::spirit::rule<Scanner> object, member, string, value, number, array; 

    definition(const json_grammar &self) 
    { 
      using namespace boost::spirit; 
      object = "{" >> member >> *("," >> member) >> "}"; 
      member = string >> ":" >> value; 
      string = "\"" >> *~ch_p("\"") >> "\""; 
      value = string | number | object | array | "true" | "false" | "null"; 
      number = real_p; 
      array = "[" >> value >> *("," >> value) >> "]"; 
    } 

    const boost::spirit::rule<Scanner> &start() 
    { 
      return object; 
    } 
  }; 
}; 

int main(int argc, char *argv[]) 
{ 
  ifstream fs(argv[1]); 
  ostringstream ss; ss << fs.rdbuf(); 
  auto data = ss.str(); 

  json_grammar g; 
  auto pi = boost::spirit::parse(data.c_str(), g, boost::spirit::space_p); 
  if (pi.hit) 
  { 
    if (pi.full) 
      cout << "parsing all data successfully" << endl; 
    else 
      cout << "parsing data partially" << endl; 

    cout << pi.length << " characters parsed" << endl; 
  } 
  else 
  {
    cout << "parsing failed; stopped at '" << pi.stop << "'" << endl; 
  }
} 


