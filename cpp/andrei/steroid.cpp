//
// 아래처럼 코딩을 해놓고 보니까 이해가 되는군.
// 
//  andrei의 traits on steroid를 읽을 것...
//
#include <iostream>

using namespace std;

template <typename T>
class HierarchyRoot 
{
public:
  struct HierarchyID {};
};

template <typename T>
class RefCountingTraits 
{
public:
  static void Refer  (T* p) {}
  static void UnRefer(T* p) {}
};

// ----------------------------------------------
class Shape: private HierarchyRoot<Shape> 
{
public:
  using HierarchyRoot<Shape>::HierarchyId;
};

class Rectangle: public Shape 
{
 public:
};

class Ellipse: public Shape 
{
public:
};

class Bitmap: public Shape 
{
public:
};
// ----------------------------------------------

class Widget: private HierarchyRoot<Widget> 
{
public:
  using HierarchyRoot<Widget>::HierarchyId;
};

class Button: public Widget 
{
public:
};

class Window: public Widget 
{
public:
};

class CheckBox: public Widget 
{
public:
};

// ----------------------------------------------
// 각 hierarchy 전체를 대상으로 하는 fully specialized template 
//

// specialized for Shape hierarchy
template<>
class RefCountingTraits<Shape::HierarchyID> 
{
public:
};

// specialized for Widget hierarchy
template<>
class RefCountingTraits<Widget::HierarchyID> 
{
public:
};

// ----------------------------------------------
// 기본적으로는 hierarchy 전체를 대상으로 하지만 specialized 
// 된 클래스에 대해서는 그 클래스만 처리하는 tempalte
// 

template <typename HierarchyID>
class HierarchyTraits 
{
public:
};

template<>
class HierarchyTraits<X::HierarchyID> 
{
public:
};

template <typename T>
class Traits: public HierarchyTraits<T::HeirarchyID> 
{
};

// ----------------------------------------------
// subhierarchy traits
// ----------------------------------------------

class Bitmap: public Shape, public HierarchyRoot<Bitmap> 
{
public:
  using HierarchyRoot<Bitmap>::HierarchyID;
};

// ----------------------------------------------
template <typename T, 
          typename RCTraits=RefCountingTraits<T::HierarchyID> 
>
class SmartPointer 
{
public:
  SmartPointer() { }
  ~SmartPointer() { }
};

// ----------------------------------------------

int main()
{
    return 0;
}
