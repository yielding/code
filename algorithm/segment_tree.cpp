#include <iostream>
#include <vector>

using namespace std;

class SegmentTree
{
public:
  SegmentTree(vector<int>& nums)
  {
    m_n = nums.size();
    m_tree.resize(4*m_n, 0);
    m_lazy.resize(4*m_n, 0);

    build_tree(nums, 0, 0, m_n - 1);
  }
  
  void update(int pos, int val)
  {
    update_tree(0, 0, m_n - 1, pos, val);
  }

  void update_range(int i, int j, int val)
  {
    update_lazy_tree(0, 0, m_n - 1, i, j, val);
  }
  
  int query(int i, int j)
  {
    return query_tree(0, 0, m_n - 1, i, j);
  }

  int query_lazy(int i, int j)
  {
    return query_lazy_tree(0, 0, m_n - 1, i, j);
  }

  auto traverse() -> vector<int>
  {
    vector<int> res;
    traverse_tree(res, 0, 0, m_n - 1);

    return res;
  }
  
private:
  void traverse_tree(vector<int>& res, int ti, int lo, int hi)
  {
    if (lo == hi) {
      res.push_back(m_tree[ti]);
      return;
    }

    auto mid = lo + (hi - lo) / 2;
    traverse_tree(res, 2*ti + 1, lo, mid);
    traverse_tree(res, 2*ti + 2, mid + 1, hi);
  }

  int query_tree(int ti, int lo, int hi, int i, int j)
  {
    if (i > hi or j < lo)
      return 0;
    
    if (i == lo and j == hi)
      return m_tree[ti];

    auto mid = lo + (hi - lo) / 2;

    if (i > mid)
      return query_tree(2*ti + 2, mid + 1, hi, i, j);

    if (j <= mid)
      return query_tree(2*ti + 1, lo, mid, i, j);

    auto lq = query_tree(ti*2 + 1, lo, mid, i, j);
    auto rq = query_tree(ti*2 + 2, mid + 1, hi, i, j);

    return lq + rq;
  }

  int query_lazy_tree(int ti, int lo, int hi, int i, int j)
  {
    if (i > hi or j < lo)
      return 0;

    if (m_lazy[ti] != 0)
    {
      m_tree[ti] += (hi - lo + 1) * m_lazy[ti];  // normalize current node by removing laziness
      if (lo != hi)
      {
        m_lazy[2*ti + 1] += m_lazy[ti];          // update lazy[] for children nodes
        m_lazy[2*ti + 2] += m_lazy[ti];
      }

      m_lazy[ti] = 0;
    }

    if (i <= lo and j >= hi)
      return m_tree[ti];

    auto mid = lo + (hi - lo) / 2;
    if (i > mid)
      return query_lazy_tree(2*ti + 2, mid + 1, hi, i, j);
    else if (j <= mid)
      return query_lazy_tree(2*ti + 1, lo, mid, i, j);

    auto lq = query_lazy_tree(2*ti + 1, lo, mid + i, i, mid);
    auto rq = query_lazy_tree(2*ti + 2, mid + 1, hi, mid + 1, j);

    return lq + rq;
  }
  
  void update_tree(int ti, int lo, int hi, int ai, int val)
  {
    if (lo == hi)
    {
      m_tree[ti] = val;
      return;
    }

    auto mid = lo + (hi - lo) / 2;

    if (ai > mid)
      update_tree(2*ti + 2, mid + 1, hi, ai, val);
    else
      update_tree(2*ti + 1, lo, mid, ai, val);

    m_tree[ti] = m_tree[2*ti + 1] + m_tree[2*ti + 2];
  }

  void update_lazy_tree(int ti, int lo, int hi, int i, int j, int val)
  {
    if (m_lazy[ti] != 0)
    {
      m_tree[ti] += (hi - lo + 1) * m_lazy[ti];  // normalize current node by removing laziness
      if (lo != hi)
      {
        m_lazy[2*ti + 1] += m_lazy[ti];          // update lazy[] for children nodes
        m_lazy[2*ti + 2] += m_lazy[ti];
      }

      m_lazy[ti] = 0;
    }

    if (lo > hi or lo > j or hi < i)
      return;

    if (i <= lo and hi <= j)
    {
      m_tree[ti] += (hi - lo + 1) * val;
      if (lo != hi)
      {
        m_lazy[2*ti + 1] += val;
        m_lazy[2*ti + 2] += val;
      }

      return;
    }

    auto mid = lo + (hi - lo) / 2;

    update_lazy_tree(2*ti + 1, lo, mid, i, j, val);
    update_lazy_tree(2*ti + 2, mid+1, hi, i, j, val);

    // merge updates
    m_tree[ti] = m_tree[2*ti + 1] + m_tree[2*ti + 2];
  }

  void build_tree(vector<int>& arr, int ti, int lo, int hi)
  {
    if (lo == hi)
    {
      m_tree[ti] = arr[lo];
      return;
    }

    auto mid = lo + (hi - lo) / 2;
    build_tree(arr, 2*ti + 1, lo, mid);
    build_tree(arr, 2*ti + 2, mid+1, hi);

    m_tree[ti] = m_tree[2*ti + 1] + m_tree[2*ti + 2];
  }
  
private:
  vector<int> m_tree, m_lazy;
  int m_n;
};

void p(vector<int>&& nums)
{
  for (auto i: nums) cout << i << " ";
  cout << endl;
  cout.flush();
}

int main(int argc, char *argv[])
{
  vector<int> arr = { 1, 10, 3, 6, 5, 6, 4 };
  
  SegmentTree sg(arr);
  cout << sg.query(0, 6) << " : ";
  p(sg.traverse());
  
  sg.update(6, 2);
  cout << sg.query(0, 6) << " : ";
  p(sg.traverse());

  sg.update_range(1, 3, 2);
  cout << sg.query_lazy(0, 6) << " : ";  
  p(sg.traverse());

  cout << sg.query_lazy(2, 2) << endl;
  cout << sg.query_lazy(3, 3) << endl;
  p(sg.traverse());

  return 0;
}
