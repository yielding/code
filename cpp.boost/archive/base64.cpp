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
//        "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxp"
//        "dC4gRnVzY2Ugb3JuYXJlIHVsbGFtY29ycGVyIGlwc3VtIGFjIGdyYXZpZGEu";

			"REFUQQAABORWRVJTAAAABAAAAANUWVBFAAAABAAAAABVVUlEAAAA"
			"EKthNJkMg0bzknnn9XQq2tJITUNLAAAAKEOQnk7LbBJd5dPWSkrI"
			"SkI8fdbM/e0ksB3vY1CVnZc2AxXUcAaasvFXUkFQAAAABAAAAAFT"
			"QUxUAAAAFJhg4pjyCGodKU/XsvoXleEC8BZFSVRFUgAAAAQAAMNQ"
			"VVVJRAAAABDvoDSML5xGMYDrDuacQLbTQ0xBUwAAAAQAAAALV1JB"
			"UAAAAAQAAAABS1RZUAAAAAQAAAAAV1BLWQAAACBmc9f3BkjZ+4Xd"
			"N2YAO4eoqFwVXkTHMOWKWbJZT7/IFlVVSUQAAAAQuGoKNDiZSN+r"
			"MDXWWSreIUNMQVMAAAAEAAAACldSQVAAAAAEAAAAA0tUWVAAAAAE"
			"AAAAAFdQS1kAAAAopdQCBtT16H0KquGq1shqmrHlYbFRoAt8oqwY"
			"KNTq9mTzN726dR5qRFVVSUQAAAAQA6ARrwhSSySLyzZ1ZcFKX0NM"
			"QVMAAAAEAAAACVdSQVAAAAAEAAAAA0tUWVAAAAAEAAAAAFdQS1kA"
			"AAAotdiBFxLfpzWBAFI2etmglyGiMhp1mUyDgNtEAg6blL23Dklm"
			"h1uF4FVVSUQAAAAQWAROO/AyR4G6t8QBbr/UIUNMQVMAAAAEAAAA"
			"CFdSQVAAAAAEAAAAAUtUWVAAAAAEAAAAAFdQS1kAAAAgoZhfSrN1"
			"wREFA8koe/4Rey9ziMU8tDn2XAgRWy6dhbhVVUlEAAAAEC+mOBAO"
			"skj4jJiUy5DQMsZDTEFTAAAABAAAAAdXUkFQAAAABAAAAANLVFlQ"
			"AAAABAAAAABXUEtZAAAAKGKwbYaYeaDMlHgrRR8tr27OA39mc3KK"
			"5HAPpGEzGvWrMLz484x0BKtVVUlEAAAAEIhcEBUipkBKr3aMtHNd"
			"t9lDTEFTAAAABAAAAAZXUkFQAAAABAAAAANLVFlQAAAABAAAAABX"
			"UEtZAAAAKGXBqpWwN+Z2FWmA3AwfgXbeWbBKpJ14QqNYywFSqoTG"
			"aXIQtT7P7fFVVUlEAAAAEA8HdVtC2km8tClhFxSOemVDTEFTAAAA"
			"BAAAAAVXUkFQAAAABAAAAANLVFlQAAAABAAAAABXUEtZAAAAKM5O"
			"GljkjBUwZXd6i0dkQoxSOc7S6e1mqavQmh2JblDiIVUWFFqGWnRV"
			"VUlEAAAAEGe30ZXJOEZ3sRuwVZU3aFVDTEFTAAAABAAAAANXUkFQ"
			"AAAABAAAAANLVFlQAAAABAAAAABXUEtZAAAAKBARP5iu3bcvKDdi"
			"pEBgDbmocN8pWbskuP5U1jgqRRXpWOIqzTP1edlVVUlEAAAAEBEf"
			"o9kCm0mVmxb71ODxkgVDTEFTAAAABAAAAAJXUkFQAAAABAAAAANL"
			"VFlQAAAABAAAAAFXUEtZAAAAKNDJETPoCNuIEG9EDs4QreBqVUZe"
			"sSZxMUSECNwAp7jgiqWbrxlmqXtQQktZAAAAIFisefTsm+mc0H5m"
			"6xWJvcpe9/BJQKpSXM6MfJJRT+xzVVVJRAAAABAGga/uDCJNPaeq"
			"szSVFm46Q0xBUwAAAAQAAAABV1JBUAAAAAQAAAADS1RZUAAAAAQA"
			"AAAAV1BLWQAAACj7uLlyRmWi8133d8BQ3QcN5hMS8zPwVzuUbbrD"
			"fdIXZgrp1SSYmiSFU0lHTgAAABQpkGMJYKBEFfL5nUDsELUoiXV2"
			"hg";

//    stringstream os;

//    typedef insert_linebreaks<  // insert line breaks every 72 characters
//        base64_from_binary<     // convert binary values ot base64 characters
//            transform_width<    // retrieve 6 bit integers from a sequence of 8 bit bytes
//                const char *,
//                6,
//                8
//            >
//        > 
//        ,72
//    > 
//    base64_text; // compose all the above operations in to a new iterator

//    copy(base64_text(test.c_str()),
//         base64_text(test.c_str() + test.size()),
//         std::ostream_iterator<char>(os));

//    cout << os.str() << endl;

    typedef transform_width<
        binary_from_base64<string::const_iterator>
        , 8
        , 6
        > binary_text;

    string binary(binary_text(encoded.c_str()),
                  binary_text(encoded.c_str() + encoded.size()));

    cout << binary;
}
