#include <iostream>
#include <memory>
#include <string>
#include <map>

using namespace std;

class HotDrink
{
public:
  virtual ~HotDrink() {}
  
  virtual void prepare(int volume) = 0;
};

class Tea: public HotDrink
{
  void prepare(int volume) override
  {
    cout << "Take tea bag, boil water, pour " << volume << endl;
  }
};

class Coffee: public HotDrink
{
  void prepare(int volume) override
  {
    cout << "Take tea bag, boil water, pour " << volume << endl;
  }
};

class HotDrinkFactory
{
public:
  virtual ~HotDrinkFactory() {}
  
  virtual auto make() const -> unique_ptr<HotDrink> = 0;
};

struct CoffeeFactory : HotDrinkFactory
{
  auto make() const -> unique_ptr<HotDrink> override
  {
    return make_unique<Coffee>();
  }
};

struct TeaFactory : HotDrinkFactory
{
  auto make() const -> unique_ptr<HotDrink> override
  {
    return make_unique<Tea>();
  }
};

class DrinkFactory
{
public:
  DrinkFactory()
  {
    hot_factories["coffee"] = make_unique<CoffeeFactory>();
    hot_factories["tea"]    = make_unique<TeaFactory>();
  }
  
  virtual ~DrinkFactory() {}
  
  auto make_drink(string const& name) -> unique_ptr<HotDrink>
  {
    auto drink = hot_factories[name]->make();
    drink->prepare(100);
    
    return drink;
  }
  
private:
  map<string, unique_ptr<HotDrinkFactory>> hot_factories;
};

int main(int argc, char *argv[])
{
  auto f = new DrinkFactory();
  
  auto c = f->make_drink("coffee");
  
  delete f;
  
  return 0;
}

