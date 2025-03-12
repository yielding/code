#include <iostream>
#include <memory>
#include <functional>

using namespace std;

class MyBadClass
{
public:
  void registerCallback(function<void(shared_ptr<MyBadClass>)> callback)
  {
    // shared_ptr<MyBadClass> sp_this(this); // Bad: shared_ptr to this
    // callback(sp_this);
    callback(shared_ptr<MyBadClass>(this));
  }
};

class MyClass : public enable_shared_from_this<MyClass>
{
public:
  void registerCallback(function<void(shared_ptr<MyClass>)> callback)
  {
    callback(shared_from_this());
  }
};

void callbackBadFunction(shared_ptr<MyBadClass> obj)
{
  cout << "Callback received object, use count " << obj.use_count() << endl;
}

void callbackGoodFunction(shared_ptr<MyClass> obj)
{
  cout << "Callback received object, use count " << obj.use_count() << endl;
}

int main(int argc, char* argv[])
{
  {
    auto o1 = make_shared<MyClass>();
    o1->registerCallback(callbackGoodFunction);

    auto o2 = make_shared<MyBadClass>();
    o2->registerCallback(callbackBadFunction);
  }

  cout << "end" << endl;

  return 0;
}
