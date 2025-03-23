// https://www.cppstories.com/2025/cpp23_mdspan_adj/
#include <print>
#include <limits>
#include <vector>
#include <mdspan>

using namespace std;

template <typename T> requires is_arithmetic_v<T>
class Graph
{
public:
  static constexpr T inf = numeric_limits<T>::max();

public:
  Graph(size_t _vertices) 
    : _vertices(_vertices)
    , _adjacency_matrix(_vertices*_vertices, inf)
    , _matrix_view(_adjacency_matrix.data(), _vertices, _vertices)
  {
    for (size_t i = 0; i < _vertices; ++i)
      _matrix_view[i, i] = 0;
  }

  auto add_edge(size_t from, size_t to, T weight) -> void
  {
    if (from >= _vertices || to >= _vertices || from == to)
      return;

    _matrix_view[from, to] = weight;
    _matrix_view[to, from] = weight;
  }

  [[nodiscard]]
  auto is_connected(size_t from, size_t to) const -> bool
  {
    return from < _vertices && to < _vertices && 
      _matrix_view[from, to] != inf;
  }

  [[nodiscard]]
  auto get_weight(size_t from, size_t to) const -> T
  {
    return _matrix_view[from, to];
  }

  [[nodiscard]]
  auto& adjacent_matrix() const 
  {
    return _matrix_view;
  }

  [[nodiscard]]
  auto vertices() const 
  { 
    return _vertices; 
  }

private:
  size_t _vertices;
  vector<T> _adjacency_matrix;
  mdspan<T, dextents<int, 2>> _matrix_view;
};

template <typename T>
void print_graph(const Graph<T>& g) 
{
  auto n = g.vertices();
  auto matrix = g.adjacent_matrix();
  print("Adjacency Matrix:\n");

  for (auto i=0; i<n; i++)
  {
    for (auto j=0; j<n; j++)
    {
      auto weight = g.get_weight(i, j);
      if (weight == Graph<T>::inf)
        print("   ∞ ");
      else
        print(" {:3} ", weight);
    }
    println();
  }
}

int main(int argc, char* argv[])
{
  Graph<int> g(5);

  g.add_edge(0, 1, 4);
  g.add_edge(0, 2, 8);
  g.add_edge(1, 2, 2);
  g.add_edge(1, 3, 6);
  g.add_edge(2, 3, 3);
  g.add_edge(3, 4, 5);
  g.add_edge(4, 0, 7);

  print_graph(g);

  print("\nIs node 1 connected to node 3? {}\n", g.is_connected(1, 3));
  print("Is node 0 connected to node 4? {}\n", g.is_connected(0, 4));

  return 0;
}
