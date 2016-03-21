#include <boost/config/warning_disable.hpp>
#include <boost/spirit/home/x3.hpp>

#include <iostream>
#include <string>

namespace client
{
  namespace x3    = boost::spirit::x3;
  namespace ascii = boost::spirit::x3::ascii;

  struct hundreds_ : x3::symbols<unsigned>
  {
    hundreds_()
    {
      add("C",    100)
         ("CC",   200)
         ("CCC",  300)
         ("CD",   400)
         ("D",    500)
         ("DC",   600)
         ("DCC",  700)
         ("DCCC", 800)
         ("CM",   900)
         ;
    }
  } hundreds;

  struct tens_ : x3::symbols<unsigned>
  {
    tens_()
    {
      add("X",    10)
         ("XX",   20)
         ("XXX",  30)
         ("XL",   40)
         ("L",    50)
         ("LX",   60)
         ("LXX",  70)
         ("LXXX", 80)
         ("XC",   90)
         ;
    }
  } tens;

	struct ones_ : x3::symbols<unsigned>
	{
		ones_()
		{
			add
				("I"    , 1)
				("II"   , 2)
				("III"  , 3)
				("IV"   , 4)
				("V"    , 5)
				("VI"   , 6)
				("VII"  , 7)
				("VIII" , 8)
				("IX"   , 9)
				;
		}

	} ones;
     

  // Note
  //   roman_def와 BOOST_SPIRIT_DEFINE(roman)이 연결되어 있다.
  namespace parser
  {
    using x3::eps;
    using x3::lit;
    using x3::_val;
    using x3::_attr;
    using ascii::char_;

    auto set_zero = [](auto& ctx) { _val(ctx) = 0; };
    auto add1000  = [](auto& ctx) { _val(ctx) += 1000; };
    auto add      = [](auto& ctx) { _val(ctx) += _attr(ctx); };

    x3::rule<class roman, unsigned> const roman = "roman";
    auto const roman_def =
      eps [set_zero]
      >>
      (
         -(+lit('M')   [add1000])
         >>  -hundreds [add]
         >>  -tens     [add]
         >>  -ones     [add]
      )
      ;

    BOOST_SPIRIT_DEFINE(roman);
  }
}

int main(int argc, char *argv[])
{
	std::cout << "/////////////////////////////////////////////////////////\n\n";
	std::cout << "\t\tRoman Numerals Parser\n\n";
	std::cout << "/////////////////////////////////////////////////////////\n\n";
	std::cout << "Type a Roman Numeral ...or [q or Q] to quit\n\n";

	typedef std::string::const_iterator iterator_type;
	using client::parser::roman; // Our parser

	std::string str;
	unsigned result;
	while (std::getline(std::cin, str))
	{
		if (str.empty() || str[0] == 'q' || str[0] == 'Q')
			break;

		std::string::const_iterator iter = str.begin();
		std::string::const_iterator end = str.end();
		bool r = parse(iter, end, roman, result);

		if (r && iter == end)
		{
			std::cout << "-------------------------\n";
			std::cout << "Parsing succeeded\n";
			std::cout << "result = " << result << std::endl;
			std::cout << "-------------------------\n";
		}
		else
		{
			std::string rest(iter, end);
			std::cout << "-------------------------\n";
			std::cout << "Parsing failed\n";
			std::cout << "stopped at: \": " << rest << "\"\n";
			std::cout << "-------------------------\n";
		}
	}

	std::cout << "Bye... :-) \n\n";
	return 0;
}
  
