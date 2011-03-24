template <typename T> 
T repeatable_reduce(T const* first, T const* last, T identity)
{
  if (last - first <= 1000)
    return std::accumulate(first, last, identity);

  T const* mid = first + (last - first) / 2;
  tbb::parallel_invoke(
    [&] { left  = repeatable_reduce(first,  mid, identity); },
    [&] { right = repeatable_reduce(mid,   last, identity); }
  );

  return left + right;
}
