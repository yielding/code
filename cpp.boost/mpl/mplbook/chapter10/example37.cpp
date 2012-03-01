/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It was built successfully with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\..\spirit_1_6_2 -I..\..\boost_1_32_0  /Fe%TEMP%\metaprogram-chapter10-example37.exe example37.cpp

*/

#include <boost/spirit/core.hpp>
#include <boost/spirit/attribute.hpp>
#include <iostream>
#include <string>

using namespace boost::spirit;
using namespace phoenix;

// provides one named variable of type int...
struct vars 
  : boost::spirit::closure<vars, int> // CRTP
{
    member1 value; // ...called "value" in lazy expressions
};

// calculator is a grammar with attached int called "value"
struct calculator
  : public grammar<calculator, vars::context_t> // CRTP
{
    template <class Tokenizer>
    struct definition
    {
        // all our rules have an attached int called "value," too...
        rule<Tokenizer, vars::context_t> 
          expression, term, factor, group, integer;

        // ...except the top rule
        rule<Tokenizer> top;    

        // build the grammar
        definition(calculator const& self)
        {
            top = expression[self.value = arg1];

            group = '(' >> expression[group.value = arg1] >> ')';

            factor = integer[factor.value = arg1]
                | group[factor.value = arg1]
                ;

            term = factor[term.value = arg1]
                   >> *(   ('*' >> factor[term.value *= arg1])
                         | ('/' >> factor[term.value /= arg1])
                       )
                ;

            expression = term[expression.value = arg1]
                 >> *(   ('+' >> term[expression.value += arg1])
                       | ('-' >> term[expression.value -= arg1])
                     )
                ;

            integer = int_p[integer.value = arg1];
        }

        // tell Spirit to start parsing with "top"
        rule<Tokenizer> const& start() const { return top; }
    };
};

int main()
{
    calculator calc;    //  our grammar

    std::string str;
    while (std::getline(std::cin, str))
    {
        int n = 0;
        parse(str.c_str(), calc[var(n) = arg1], space_p);
        std::cout << "result = " << n << std::endl;
    }
}
