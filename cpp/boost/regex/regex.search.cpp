#include <string>
#include <map>
#include <iostream>
#include <fstream>
#include <boost/regex.hpp>

using namespace std;
using namespace boost;

// purpose:
// takes the contents of a file in the form of a string
// and searches for all the C++ class definitions, storing
// their locations in a map of strings/int's

class T1 {
};

class T2 {
};

typedef map< string, string::difference_type, less<std::string> > map_type;

const char* re = 
        // possibly leading whitespace:   
        "^[[:space:]]*" 
        // possible template declaration:
        "(template[[:space:]]*<[^;:{]+>[[:space:]]*)?"
        // class or struct:
        "(class|struct)[[:space:]]*" 
        // leading declspec macros etc:
        "("
        "\\<\\w+\\>"
        "("
        "[[:blank:]]*\\([^)]*\\)"
        ")?"
        "[[:space:]]*"
        ")*" 
        // the class name
        "(\\<\\w*\\>)[[:space:]]*" 
        // template specialisation parameters
        "(<[^;:{]+>)?[[:space:]]*"
        // terminate in { or :
        "(\\{|:[^;\\{()]*\\{)"
        ;

regex expression(re);

void 
IndexClasses(map_type& m, const std::string& file)
{
    string::const_iterator start, end;
    start = file.begin();
    end   = file.end();   
    match_results<string::const_iterator> what;
    match_flag_type flags = boost::match_default;
    while (boost::regex_search(start, end, what, expression, flags)) {
        // what[0] contains the whole string
        // what[5] contains the class name.
        // what[6] contains the template specialisation if any.
        // add class name and position to map:

        string index = string(what[5].first, what[5].second) + string(what[6].first, what[6].second);
        string::difference_type value = what[5].first - file.begin();      
        m[index] = value;
        // update search position:
        start = what[0].second;      
        // update flags:
        flags |= boost::match_prev_avail;
        flags |= boost::match_not_bob;
    }
}

void 
load_file(string& s, istream& is)
{
    s.erase();
    if (is.bad()) return;
    s.reserve(is.rdbuf()->in_avail());
    char c;
    while (is.get(c)) {
        if (s.capacity() == s.size())
            s.reserve(s.capacity() * 3);
        s.append(1, c);
    }
}

int main(int argc, const char** argv) 
{
    string text;

    for (int i=1; i<argc; ++i) {
        cout << "Processing file " << argv[i] << endl;
        map_type m;
        std::ifstream fs(argv[i]);
        load_file(text, fs);
        fs.close();
        IndexClasses(m, text);
        cout << m.size() << " matches found" << endl;
        map_type::iterator c = m.begin();
        map_type::iterator d = m.end();
        while (c != d) {
            cout << "class \"" << (*c).first << "\" found at index: " << (*c).second << endl;
            ++c;
        }
    }
    return 0;
}
