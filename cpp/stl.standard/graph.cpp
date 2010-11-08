#include <map>
#include <vector>
#include <queue>
#include <iostream>
#include <string>

using namespace std;

typedef map<string, int>          stringVector;
typedef map<string, stringVector> graph;

struct DistancePair
{
  unsigned first;
  string   second;
  DistancePair() : first(0) {}
  DistancePair(unsigned f, const string& s) : first(f), second(s) {}
};

bool operator<(const DistancePair& lhs, const DistancePair& rhs)
{
  return lhs.first < rhs.first;
}

bool operator> (const DistancePair& lhs, const DistancePair& rhs)
{
  return lhs.first > rhs.first;
}

string pendleton("Pendleton");
string pensacola("Pensacola");
string peoria("Peoria");
string phoenix("Phoenix");
string pierre("Pierre");
string pittsburgh("Pittsburgh");
string princeton("Princeton");
string pueblo("Pueblo");

graph cityMap;

void shortestDistance (graph& cityMap, string& start, stringVector& distances)
{
  //
  // Process a priority queue of distances to nodes.
  //
  priority_queue<DistancePair, vector<DistancePair, allocator<DistancePair> >,
    greater<DistancePair> > que;
  que.push(DistancePair(0, start));

  while (! que.empty())
  {
    //
    // Pull nearest city from queue.
    //
    int distance = que.top().first;
    string city = que.top().second;
    que.pop();
    //
    // If we haven't seen it already, process it.
    //
    if (0 == distances.count(city))
    {
      //
      // Then add it to shortest distance map.
      //
      distances[city] = distance;
      //
      // And put values into queue.
      //
      const stringVector& cities = cityMap[city];
      stringVector::const_iterator start = cities.begin();
      stringVector::const_iterator stop  = cities.end();
      for (; start != stop; ++start)
        que.push(DistancePair(distance + (*start).second,
              (*start).first));
    }
  }
}

int main ()
{
  cout << "Graph example program, from Chapter 7" << endl;

  cityMap[pendleton][phoenix]    = 4;
  cityMap[pendleton][pueblo]     = 8;
  cityMap[pensacola][phoenix]    = 5;
  cityMap[peoria][pittsburgh]    = 5;
  cityMap[peoria][pueblo]        = 3;
  cityMap[phoenix][peoria]       = 4;
  cityMap[phoenix][pittsburgh]   = 10;
  cityMap[phoenix][pueblo]       = 3;
  cityMap[pierre][pendleton]     = 2;
  cityMap[pittsburgh][pensacola] = 4;
  cityMap[princeton][pittsburgh] = 2;
  cityMap[pueblo][pierre]        = 3;

  stringVector distances;

  shortestDistance(cityMap, pierre, distances);
  stringVector::iterator where;
  for (where = distances.begin(); where != distances.end(); ++where)
    cout << "Distance to: " << (*where).first << ":"
      <<  (*where).second << endl;

  cout << "End of graph example program" << endl;

  return 0;
}
