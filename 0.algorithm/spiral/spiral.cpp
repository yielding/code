#include <iostream>

using namespace std;

struct spiral_array;

struct cell
{
  cell(spiral_array* parent): m_parent(parent)
  {}

  int x, y;
  int value;
  
  bool is_wall() {
    return false;
  }

private:
  spiral_array* m_parent;
};

struct spiral_array
{
  spiral_array(int size): m_size(size) 
  {
  }

  cell next_cell()
  {
    cell c;

    return c;
  }

  void go();

private:
  int m_size;
};

int main(int argc, char *argv[])
{
  int col = 3;
  int row = 3;


  return 0;
}
