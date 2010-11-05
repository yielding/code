#include <vector>
#include <functional>
#include <algorithm>

#include <iostream>
using namespace std;

//
// Very large city class
//
class MegaPolis
{
public:

  MegaPolis(char*  s = 0 , float n = 0):cityName(s),population(n) {;}
  // The following function cannot return void due to limitations in 
  // some current C++ implementations. 
  virtual size_t byPopulation()
  {
    cout<<cityName<<"(MegaPolis)"<<"\t\t\t"<<population<<endl;
    return 0;
  }
  float population;
protected:
  char* cityName;
};

//
// City and surrounding area class
//
class MetroPolis : public MegaPolis
{
public:
  MetroPolis(char*  s = 0 , float n = 0):MegaPolis(s,n){;}
  virtual size_t byPopulation()
  {
    cout<<cityName<<"(MetroPolis)"<<"\t\t\t"<<population<<endl;
    return 0;
  }
};

//
// Functor compares the size of two MeagPolis classes
//
struct GreaterPopulation
{
  bool operator()(MegaPolis* m1, MegaPolis* m2)
  {
    return m1->population<m2->population;
  }
} greater_pop;


int main()
{
  // 
  // Create a vector of very lareg cities
  //
  vector<MegaPolis*> cityList;

  cityList.push_back(new MegaPolis ("Calcutta",35));
  cityList.push_back(new MegaPolis ("Tokyo",20));
  cityList.push_back(new MegaPolis ("Delhi",10)); 
  cityList.push_back(new MegaPolis ("Bombay",15));

  cityList.push_back(new MetroPolis ("Cairo",5));
  cityList.push_back(new MetroPolis ("New York",2.5));
  cityList.push_back(new MetroPolis ("Los Angeles",3)); 
  cityList.push_back(new MetroPolis ("Jakarta",1.5));

  // 
  // Now use mem_fun to pass byPopulation member function
  // of MegaPolis to the for_each function.
  //
  cout<<"City                    "<<" Population (in millions)    "<<endl;
  cout<<"-----                   "<<" -----------------------"<<endl;  
  for_each(cityList.begin(),cityList.end(),mem_fun(&MegaPolis::byPopulation));
  cout<<"..............After sorting..........................."<<endl;
  stable_sort(cityList.begin(),cityList.end(),greater_pop);
  for_each(cityList.begin(),cityList.end(),mem_fun(&MegaPolis::byPopulation));

  return 0;
}
