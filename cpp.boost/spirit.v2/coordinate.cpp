#include <boost/spirit/include/qi.hpp>

#include <string>
#include <vector>
#include <iostream>

using namespace std;
namespace qi = boost::spirit::qi;

int main(int argc, char const* argv[])
{
    string in("(1.0, 2.0)");
    vector<double> v;
    double d1 = 0.;
    double d2 = 0.;
    qi::parse(in.begin(), in.end(),
            '(' >> qi::double_ >> ", " >> qi::double_ >> ')',
            v);

    cout << v[0] << ", " << v[1] << endl;
    
    return 0;
}
