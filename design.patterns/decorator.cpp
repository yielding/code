#include <iostream>
#include <string>

using namespace std;

class ICoffee
{
public:
  virtual ~ICoffee() = default;  

  virtual double cost() = 0;
  virtual string ingredient() = 0; 
};

class SimpleCoffee: public ICoffee
{
public:
  SimpleCoffee()
  {
    m_cost = 1;
    m_ingredient = "coffee";
  }

  virtual ~SimpleCoffee()
  {}

  virtual double cost()
  {
    return m_cost;
  }

  virtual string ingredient()
  {
    return m_ingredient;
  }

private:
  double m_cost;
  string m_ingredient;
};

class CoffeeDecorator: public ICoffee
{
public:
  CoffeeDecorator(ICoffee* coffee): m_coffee(coffee)
  {}

  double cost()       { return m_coffee->cost(); }
  string ingredient() { return m_coffee->ingredient(); }

protected:
  ICoffee* m_coffee;
};

class Milk: public CoffeeDecorator
{
public:
  Milk(ICoffee* coffee): CoffeeDecorator(coffee)
  {
    m_cost = 9.5;
    m_ingredient = "Milk";
  }

  double cost()       { return m_coffee->cost() + m_cost; }
  string ingredient() { return m_coffee->ingredient() + " " + m_ingredient; }

private:
  double m_cost;
  string m_ingredient;
};

class Whip: public CoffeeDecorator
{
public:
  Whip(ICoffee* coffee)
    : CoffeeDecorator(coffee)
  {
    m_cost = 0.7;
    m_ingredient = "WHip";
  }

  double cost()       { return m_coffee->cost() + m_cost; }
  string ingredient() { return m_coffee->ingredient() + " " + m_ingredient; }

private:
  double m_cost;
  string m_ingredient;
};

class Sprinkles: public CoffeeDecorator
{
public:
  Sprinkles(ICoffee* coffee)
    : CoffeeDecorator(coffee)
  {
    m_cost = 0.2;
    m_ingredient = "Sprinkles";
  }

  double cost()       { return m_coffee->cost() + m_cost; }
  string ingredient() { return m_coffee->ingredient() + " " + m_ingredient; }

private:
  double m_cost;
  string m_ingredient;
};

int main(int argc, char const* argv[])
{
  auto coffee = new Sprinkles(new Milk(new SimpleCoffee()));
  cout << coffee->cost() << endl;
  cout << coffee->ingredient() << endl;

  delete coffee;

  return 0;
}
