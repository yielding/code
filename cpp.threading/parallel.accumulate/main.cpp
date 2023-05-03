#include <iostream>
#include <string>
#include <vector>
#include <numeric>
#include <thread> 
#include <algorithm>

using namespace std;

template <typename It, typename T>
struct accumulate_block
{
  void operator()(It first, It last, T& result)
  {
    result = accumulate(first, last, result);
  }
};

template <typename It, typename T>
T parallel_accumulate(It first, It last, T init)
{
  auto length = distance(first, last);

  auto min_per_thread = 25;
  auto max_threads = (length + min_per_thread - 1) / min_per_thread;
  auto hw_threads  = thread::hardware_concurrency();
  auto num_threads = min<int>(hw_threads!=0?hw_threads:2, max_threads);
  auto block_size  = length / num_threads;
  vector<T> results(num_threads);
  vector<thread> threads(num_threads-1);
  It block_start = first;
  for (int i=0; i<(num_threads-1); ++i)
  {
    It block_end = block_start;
    advance(block_end, block_size);
    threads[i] = thread(accumulate_block<It, T>(),
        block_start, 
        block_end,
        ref(results[i]));
  }

  accumulate_block<It, T>() (
    block_start, last, results[num_threads-1]);

  for_each(threads.begin(),threads.end(), 
      mem_fn(&thread::join));

  return accumulate(results.begin(), results.end(),init);
}

int main(int argc, char *argv[])
{
  vector<int> v{1, 2, 3, 4, 5};
  accumulate_block<vector<int>::iterator, int> blk;

  int result = 0;
  blk(v.begin(), v.end(), result);
  cout << result << endl;

  result = 0;
  parallel_accumulate(v.begin(), v.end(), result);

  cout << result;
  
  return 0;
}
