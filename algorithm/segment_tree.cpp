#include <iostream>

using namespace std;

struct node
{
  node() { value = 0; lazy = 0; }
  
  long value;
  long lazy;
};

template<int SZ>
class SegmentTree
{
public:
  SegmentTree(long* arr)
  {
    for (int i=0; i<SZ*3; i++)
    {
      tree[i].value = (long)0;
      tree[i].lazy  = 0;
    }
    
    init(arr, 1, 1, SZ);
  }
  
  void update(int pos, long value)
  {
    _update(1, 1, SZ, pos, value);
  }
  
  long sum(int i, int j)
  {
    return _sum(1, 1, SZ, i, j);
  }
  
private:
  long _sum(int node, int start, int end, int i, int j)
  {
    if (tree[node].lazy != 0)
    {
      tree[node].value += (end - start + 1) * tree[node].lazy;
      if (start != end)
      {
        tree[node * 2 + 0].lazy += tree[node].lazy;
        tree[node * 2 + 1].lazy += tree[node].lazy;
      }

      tree[node].lazy = 0;
    }

    if (i > end || j < start)
      return 0;
    
    if (i <= start && end <= j)
      return tree[node].value;
    
    return _sum(node*2, start, (start + end)/2, i, j)
           +
           _sum(node*2 + 1, (start + end)/2 + 1, end, i, j);
  }
  
  void _update(int node, int start, int end, int pos, long diff)
  {
    if (pos < start || pos > end)
      return;
    
    tree[node].value = tree[node].value + diff;
    
    if (start != end)
    {
      _update(node * 2, start, (start + end) / 2, pos, diff);
      _update(node * 2 + 1, (start + end) / 2 + 1, end, pos, diff);
    }
  }

  void _update_range(int node, int start, int end, int i, int j, long diff)
  {
    if (tree[node].lazy != 0)
    {
      tree[node].value += (end - start + 1) * tree[node].lazy;
      if (start != end)
      {
        tree[node*2+0].lazy += tree[node].lazy;
        tree[node*2+1].lazy += tree[node].lazy;
      }

      tree[node].lazy = 0;
    }

    if (j < start || i > end)
      return;

    if (i <= start && end <= j)
    {
      tree[node].value += (end - start + 1) * diff;
      if (start != end)
      {
        tree[node * 2 + 0].lazy += diff;
        tree[node * 2 + 1].lazy += diff;
      }

      return;
    }

    _update_range(node*2  , start, (start+end)/2, i, j, diff);
    _update_range(node*2+1, (start+end)/2+1, end, i, j, diff);

    tree[node].value = tree[node*2].value + tree[node*2 + 1].value;
  }
  
  long init(long* arr, int node, int start, int end)
  {
    if (start == end)
      return tree[node].value = arr[start];
    
    return tree[node].value =
      init(arr, node * 2, start, (start + end) / 2)
      +
      init(arr, node * 2 + 1, (start + end) / 2 + 1, end);
  }
  
private:
  node tree[SZ*3];
};

int main(int argc, char *argv[])
{
  long arr[] = { 0, 1, 10, 3, 6, 5, 6, 4, 0 };
  
  SegmentTree<8> sg(arr);
  
  sg.update(7, 2);
  cout << sg.sum(1, 7);
  cout.flush();
  
  return 0;
}
