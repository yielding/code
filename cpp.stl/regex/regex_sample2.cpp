#include <iostream>
#include <iterator>
#include <string>
#include <regex>
 
using namespace std;

int main()
{
    string s = "Some people, when confronted with a problem, think "
        "\"I know, I'll use regular expressions.\" "
        "Now they have two problems.";
 
    regex self_regex("REGULAR EXPRESSIONS",
            regex_constants::ECMAScript | regex_constants::icase);
    if (regex_search(s, self_regex))
        cout << "Text contains the phrase 'regular expressions'\n";
 
    regex word_regex("(\\S+)");
    auto words_begin = sregex_iterator(s.begin(), s.end(), word_regex);
    auto words_end   = sregex_iterator();
 
    cout << "Found "
         << distance(words_begin, words_end)
         << " words\n";

    const int N = 6;
    cout << "Words longer than " << N << " characters:\n";
    for (auto i=words_begin; i != words_end; ++i) 
    {
        smatch match = *i;
        string match_str = match.str();
        if (match_str.size() > N)
            cout << "  " << match_str << '\n';
    }
 
    regex long_word_regex("(\\w{7,})");
    string new_s = regex_replace(s, long_word_regex, "[$&]");
    cout << new_s << '\n';
}
