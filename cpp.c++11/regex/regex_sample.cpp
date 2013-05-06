#include <iostream>
#include <regex>

using namespace std;

int main(int argc, const char *argv[])
{
  regex rgx(R"([ ,.\t\n;:])");
  cmatch match;

  if (regex_search("Unseen University - Ankh-Morpork", match, rgx)) 
  {
    for (size_t a=0; a<match.size(); a++)
    {
      string str(match[a].first, match[a].second);
      cout << "[ " << str << " ]\n";
    }
  }

  return 0;
}
