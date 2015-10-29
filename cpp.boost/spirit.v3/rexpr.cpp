#include <boost/config/warning_disable.hpp>
#include <boost/spirit/home/x3.hpp>
#include <boost/spirit/home/x3/support/ast/variant.hpp>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/fusion/include/io.hpp>

#include <iostream>
#include <fstream>
#include <string>
#include <map>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace client { namespace ast
{
  namespace fusion = boost::fusion;
  namespace x3     = boost::spirit::x3;

  struct rexpr;

  struct rexpr_value : x3::variant<string, x3::forward_ast<rexpr>>
  {
    using base_type::base_type;
    using base_type::operator=;
  };
  
  typedef map<string, rexpr_value> rexpr_map;
  typedef pair<string, rexpr_value> rexpr_key_value;

  struct rexpr
  {
    rexpr_map entries;
  };
}
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
BOOST_FUSION_ADAPT_STRUCT(client::ast::rexpr,
    (client::ast::rexpr_map, entries)
)

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace client { namespace ast
{
  int const tabsize = 4;

  struct rexpr_printer
  {
    typedef void result_type;

    rexpr_printer(int indent = 0) : indent(indent) {}

    void operator()(rexpr const& ast) const
    {
      cout << '{' << endl;
      for (auto const& entry : ast.entries)
      {
        tab(indent+tabsize);
        cout << '"' << entry.first << "\" = ";
        boost::apply_visitor(rexpr_printer(indent+tabsize), entry.second);
      }

      tab(indent);
      cout << '}' << endl;
    }

    void operator()(string const& text) const
    {
      cout << '"' << text << '"' << endl;
    }

    void tab(int spaces) const
    {
      for (int i = 0; i < spaces; ++i) cout << ' ';
    }

    int indent;
  };
}
}

namespace client { namespace parser
{
  namespace x3 = boost::spirit::x3;
  namespace ascii = boost::spirit::x3::ascii;
  
  using x3::lit;
  using x3::lexeme;
  
  using ascii::char_;
  using ascii::string;

  x3::rule<class rexpr_value, ast::rexpr_value>
    rexpr_value = "rexpr_value";

  x3::rule<class rexpr, ast::rexpr>
    rexpr = "rexpr";

  x3::rule<class rexpr_key_value, ast::rexpr_key_value>
    rexpr_key_value = "rexpr_key_value";

  auto const quoted_string = 
    lexeme['"' >> *(char_ - '"') >> '"'];

  auto const rexpr_value_def = 
    quoted_string | rexpr;

  auto const rexpr_key_value_def = 
    quoted_string >> '=' >> rexpr_value;

  auto const rexpr_def  = 
    '{' >> *rexpr_key_value >> '}';

  BOOST_SPIRIT_DEFINE(rexpr_value, rexpr, rexpr_key_value);
}
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char **argv)
{
  char const* filename;
  if (argc > 1)
  {
    filename = argv[1];
  }
  else
  {
    cerr << "Error: No input file provided." << endl;
    return 1;
  }

  ifstream in(filename);

  if (!in)
  {
    cerr << "Error: Could not open input file: " << filename << endl;
    return 1;
  }

  string storage;         // We will read the contents here.
  in.unsetf(ios::skipws); // No white space skipping!
  copy(istream_iterator<char>(in), istream_iterator<char>(), back_inserter(storage));

  using client::parser::rexpr; // our grammar
  client::ast::rexpr ast;      // our tree

  using boost::spirit::x3::ascii::space;

  auto const iter = storage.begin();
  auto const end  = storage.end();
  bool r = phrase_parse(iter, end, rexpr, space, ast);

  if (r && iter == end)
  {
    cout << "-------------------------\n";
    cout << "Parsing succeeded\n";
    cout << "-------------------------\n";
    client::ast::rexpr_printer printer;
    printer(ast);
    return 0;
  }
  else
  {
    string::const_iterator some = iter+30;
    string context(iter, (some>end)?end:some);
    cout << "-------------------------\n";
    cout << "Parsing failed\n";
    cout << "stopped at: \": " << context << "...\"\n";
    cout << "-------------------------\n";
    return 1;
  }
}
