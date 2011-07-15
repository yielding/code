#include <string>
#include <iostream>
#include <vector>
#include "boost/algorithm/string.hpp"

using namespace std;      

vector<string> Tokenize(const string& str, const string& delim)
{
    size_t off = 0, next = 0, pos = 0;
    size_t delimlen = delim.size();
    vector<string> tokens;
    while (1)
    {
        next = str.find(delim, off);
        if (next == string::npos)
        {
            string val = str.substr(pos, str.size() - pos);
            if (!val.empty())
            {
                boost::replace_all(val, delim + delim, delim);
                tokens.push_back(val);
            }
            break;
        }

        bool added = false;
        if (str.substr(next + delimlen, delimlen) == delim)
        {
            next += delimlen;
        }
        else
        {
            string val = str.substr(pos, next - pos);
            boost::replace_all(val, delim + delim, delim);
            tokens.push_back(val);

            added = true;
        }

        off = next + delimlen;
        if (added)
        {
            pos = off;
        }
    }

    return tokens;
}

template <typename T> 
std::vector<T> 
split(T const& s, T const& delim)
{
    std::vector<T> r;
    boost::split(r, s, boost::is_any_of(delim));
    return r;
}

int main()
{
    string a = "leech:kamin";
    vector<string>v0;
    boost::split(v0, a, boost::is_any_of(":"));
    cout << v0[0] << " : " << v0[1] << endl;


    v0 = Tokenize(a, ":");
    cout << v0[0] << " : " << v0[1] << endl;

    vector<wstring>v1;
    wstring b = L"leech:kamin";
    boost::split(v1, b, boost::is_any_of(L":"));
    wcout << v1[0] << " : " << v1[1] << endl;
}
