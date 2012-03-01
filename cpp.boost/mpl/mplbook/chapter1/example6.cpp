/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004
    Copyright João Abecasis 2004
    
    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It was built successfully with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\..\spirit_1_6_2 -I..\..\boost_1_32_0  /Fe%TEMP%\metaprogram-chapter1-example6.exe example6.cpp

*/

#include <boost/spirit/core.hpp>
#include <iostream>
#include <string>

// Spirit 1.8 doesn't support MSVC 6 and 7, so you have to use Spirit
// 1.6.2, which doesn't support closures.  Thus this inclusion and
// the corresponding changes to the body of the calculator.
#include "closure_workaround.hpp"

using namespace std;
using namespace boost::spirit;

typedef workaround::context<double> context;

struct calculator : public grammar<calculator>
{
    template <class Tokenizer>
    struct definition
    {
        // All our rules have an attached int called "val", too...
        rule<Tokenizer>
          expr, term, factor, group, integer;

        // ...except the top rule
        rule<Tokenizer> top;

        definition(calculator const&)
        {
            top = CREATE_CONTEXT(
                expr[ context::assign() ]
                );

            expr = CREATE_CONTEXT(
                  ( term[ context::assign() ] >> '+' >> expr[ context::add() ] )
                | ( term[ context::assign() ] >> '-' >> expr[ context::subtract() ] )
                | term[ context::assign() ]
                );

            term = CREATE_CONTEXT(
                  ( factor[ context::assign() ] >> '*' >> term[ context::multiply() ] )
                | ( factor[ context::assign() ] >> '/' >> term[ context::divide() ] )
                | factor[ context::assign() ]
                );

            factor = CREATE_CONTEXT(
                  integer[ context::assign() ]
                | ( '(' >> expr[ context::assign() ] >> ')' )
                );

            integer = CREATE_CONTEXT(
                int_p[ context::assign() ]
                );
        }

        // Tell Spirit to start parsing with "top"
        rule<Tokenizer> const& start() const { return top; }
    };
};

////////////////////////////////////////////////////////////////////////////
//
//  Main program
//
////////////////////////////////////////////////////////////////////////////
int
main()
{
    cout << "/////////////////////////////////////////////////////////\n\n";
    cout << "\t\tThe Calculator...\n\n";
    cout << "/////////////////////////////////////////////////////////\n\n";
    cout << "Type an expression...or [q or Q] to quit\n\n";

    calculator calc; //  Our Grammar

    string str;
    while (getline(cin, str))
    {
        if (str[0] == 'q' || str[0] == 'Q')
            break;

        parse_info<> info = parse(str.c_str(), calc, space_p);

        //  calc[var(n) = arg1] invokes the calculator and extracts
        //  the result of the computation. See calculator grammar
        //  note above.

        if (info.full)
        {
            cout << "-------------------------\n";
            cout << "Parsing succeeded\n";
            cout << "result = " << context::return_value << endl;
            cout << "-------------------------\n";
        }
        else
        {
            cout << "-------------------------\n";
            cout << "Parsing failed\n";
            cout << "stopped at: \": " << info.stop << "\"\n";
            cout << "-------------------------\n";
        }
    }

    cout << "Bye... :-) \n\n";
    return 0;
}

