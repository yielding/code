#include "common.h"

TEST(Set, Difference)
{
  using Strs = vector<string>;

  Strs s1 = {"leech1", "leech2", "leech3" },
       s2 = {"leech1" },
       s3;

  set_difference(s1.begin(), s1.end(), s2.begin(), s2.end(),
      back_inserter(s3));

  ASSERT_THAT(s3, Eq(vector<string>{"leech2", "leech3"}));
  ASSERT_THAT(s3, ElementsAre("leech2", "leech3"));
}

TEST(Set, RemoveDuplicate)
{
  struct data 
  {
    string name;
    int age;

    bool operator<(data const& rhs) const { return name < rhs.name; }
  };

  set<data> s;

  data a0{"leech", 39}, a1{"kamin", 37}, a2{"kamin", 39};

  s.insert(a0);
  s.insert(a1);
  s.insert(a2);

  vector<string> v;
  for (auto e:s) v.push_back(e.name);

  ASSERT_THAT(v, ElementsAre("kamin", "leech"));
}
