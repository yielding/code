#include <iostream>
#include <string>
#include <vector>

using namespace std;

string to_hex(int64_t position)
{
  char buf[20] = { 0 };
  sprintf(buf, "0x%010X", position);

  return string(buf);
}

struct run
{
  run()
  {
    reset();
  }

  void reset()                    { pos = -1; s = ""; }

  bool can_start_with(uint8_t ch) { return _is_alpha(ch) && (pos == -1) && s.empty(); }
  void push(uint8_t ch)           { s.push_back(char(ch));                  }
  bool can_include(uint8_t ch)    { return (pos != -1) && _is_alpha(ch);    }
  bool ends_with(uint8_t ch)      { return (pos != -1) && !_is_alpha(ch);   }
  bool has_run()                  { return (pos != -1) && (s.length() > 0); }

  void position(int64_t p)        { pos = p;            }
  auto position() -> int64_t      { return pos;         }

  bool _is_alpha(uint8_t ch)      { return isalpha(ch); }

  auto to_s() -> string
  {
    auto res = to_hex(pos);
    res += " " + s;

    return res;
  }

  int64_t pos;
  string  s;
};

typedef vector<run> runs;

runs find_runs(string contents)
{
  runs result;
  run  r;

  for (auto i=0; i<contents.length(); ++i)
  {
    auto ch = uint8_t(contents[i]);

    if (r.can_start_with(ch))
    {
      r.position(i);
      r.push(ch);
    }
    else if (r.can_include(ch))
    {
      r.push(ch);
    }
    else if (r.ends_with(ch))
    {
      r.push(ch);
      result.push_back(r);
      r.reset();
    }
  }

  if (r.has_run())
    result.push_back(r);

  return result;
}

int main(int argc, const char *argv[])
{
  string s = "aaa bbb";
  auto res = find_runs(s);
  for (int i=0; i<res.size(); i++)
    cout << res[i].to_s() << endl;

  return 0;
}
