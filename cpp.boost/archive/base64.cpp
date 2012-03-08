#include <boost/archive/iterators/base64_from_binary.hpp>
#include <boost/archive/iterators/binary_from_base64.hpp>
#include <boost/archive/iterators/insert_linebreaks.hpp>
#include <boost/archive/iterators/transform_width.hpp>
#include <boost/archive/iterators/ostream_iterator.hpp>
#include <sstream>
#include <string>

int main()
{
    using namespace boost::archive::iterators;
    using namespace std;

    string test = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ornare ullamcorper ipsum ac gravida.";

    string encoded =
        "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxp"
        "dC4gRnVzY2Ugb3JuYXJlIHVsbGFtY29ycGVyIGlwc3VtIGFjIGdyYXZpZGEu";

    stringstream os;

    typedef insert_linebreaks<  // insert line breaks every 72 characters
        base64_from_binary<     // convert binary values ot base64 characters
            transform_width<    // retrieve 6 bit integers from a sequence of 8 bit bytes
                const char *,
                6,
                8
            >
        > 
        ,72
    > 
    base64_text; // compose all the above operations in to a new iterator

    copy(base64_text(test.c_str()),
         base64_text(test.c_str() + test.size()),
         std::ostream_iterator<char>(os));

    cout << os.str() << endl;

    typedef transform_width<
        binary_from_base64<string::const_iterator>
        , 8
        , 6
        > binary_text;

    string binary(binary_text(encoded.begin()),
                  binary_text(encoded.end()));

    cout << binary;
}
